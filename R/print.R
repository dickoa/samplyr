#' Print methods for samplyr objects
#'
#' @name print.samplyr
#' @family helpers
#' @param x Object to print
#' @param ... Must be empty.
#' @return Invisibly returns the input object
NULL

#' @rdname print.samplyr
#' @export
print.sampling_design <- function(x, ...) {
  rlang::check_dots_empty()
  if (!is_null(x$title)) {
    cli::cat_rule(paste0("Sampling Design: ", x$title))
  } else {
    cli::cat_rule("Sampling Design")
  }

  n_stages <- length(x$stages)
  stage_label <- if (n_stages == 1) "stage" else "stages"
  cat("\n")
  cli::cat_bullet(
    paste(n_stages, stage_label),
    bullet = "info"
  )

  for (i in seq_along(x$stages)) {
    print_stage(x$stages[[i]], i)
  }
  cat("\n")
  invisible(x)
}

#' @noRd
print_stage <- function(stage, num) {
  cat("\n")
  if (!is_null(stage$label)) {
    cli::cat_rule(left = paste0("Stage ", num, ": ", stage$label))
  } else {
    cli::cat_rule(left = paste("Stage", num))
  }

  if (!is_null(stage$strata)) {
    strata <- stage$strata
    vars_str <- paste(strata$vars, collapse = ", ")
    alloc_str <- if (!is_null(strata$alloc)) {
      paste0(" (", strata$alloc, ")")
    } else {
      ""
    }
    cli::cat_bullet(
      paste0("Strata: ", vars_str, alloc_str),
      bullet = "bullet"
    )
  }

  if (!is_null(stage$clusters)) {
    vars_str <- paste(stage$clusters$vars, collapse = ", ")
    cli::cat_bullet(paste0("Cluster: ", vars_str), bullet = "bullet")
  }

  if (!is_null(stage$draw_spec)) {
    has_strata <- !is_null(stage$strata)
    alloc <- stage$strata$alloc
    draw_desc <- format_draw_spec(
      stage$draw_spec,
      has_strata = has_strata,
      alloc = alloc
    )
    cli::cat_bullet(paste0("Draw: ", draw_desc), bullet = "bullet")
  } else {
    cli::cat_bullet(
      "Incomplete: no draw specification",
      bullet = "warning"
    )
  }
}

#' Qualify a scalar n/frac with its scope when the stage is stratified
#'
#' Without stratification, `n = 300` is unambiguous.
#' With stratification and no `alloc`, it is per stratum.
#' With stratification and `alloc`, it is the total.
#' Named vectors and data frames are already explicit per-stratum.
#' @noRd
format_draw_spec <- function(draw, has_strata = FALSE, alloc = NULL) {
  parts <- c()

  scope_tag <- function(value, has_strata, alloc) {
    if (!has_strata) return("")
    if (length(value) > 1 && !is_null(names(value))) {
      return(" (per stratum)")
    }
    if (length(value) == 1) {
      if (is_null(alloc)) " (per stratum)" else " (total)"
    } else {
      ""
    }
  }

  format_scalar <- function(value_name, value, has_strata, alloc) {
    if (length(value) > 1 && !is_null(names(value))) {
      return(paste0(
        value_name, " = <", length(value), " values, per stratum>"
      ))
    }
    paste0(value_name, " = ", value, scope_tag(value, has_strata, alloc))
  }

  if (!is_null(draw$n)) {
    if (is.data.frame(draw$n)) {
      parts <- c(parts, "n = <custom data frame, per stratum>")
    } else {
      parts <- c(parts, format_scalar("n", draw$n, has_strata, alloc))
    }
  }
  if (!is_null(draw$frac)) {
    if (is.data.frame(draw$frac)) {
      parts <- c(parts, "frac = <custom data frame, per stratum>")
    } else {
      parts <- c(parts, format_scalar("frac", draw$frac, has_strata, alloc))
    }
  }

  parts <- c(parts, paste0("method = ", draw$method))

  if (!is_null(draw$mos)) {
    parts <- c(parts, paste0("mos = ", draw$mos))
  }

  if (!is_null(draw$prn)) {
    parts <- c(parts, paste0("prn = ", draw$prn))
  }

  if (!is_null(draw$aux)) {
    parts <- c(parts, paste0("aux = ", paste(draw$aux, collapse = ", ")))
  }

  if (!is_null(draw$bounds)) {
    bound_text <- paste0("bound(", draw$bounds, ")", collapse = ", ")
    parts <- c(parts, paste0("count bounds = ", bound_text))
  }

  if (!is_null(draw$spread)) {
    parts <- c(parts, paste0("spread = ", paste(draw$spread, collapse = ", ")))
  }

  if (!is_null(draw$control)) {
    control_str <- format_control_quos(draw$control)
    parts <- c(parts, paste0("control = ", control_str))
  }

  paste(parts, collapse = ", ")
}

#' @noRd
format_control_quos <- function(control_quos) {
  if (is_null(control_quos) || length(control_quos) == 0) {
    return(NULL)
  }

  labels <- vapply(
    control_quos,
    function(q) {
      rlang::as_label(q)
    },
    character(1)
  )

  if (length(labels) == 1) {
    labels
  } else {
    paste0("c(", paste(labels, collapse = ", "), ")")
  }
}

#' Whether an executed digest path contains with-replacement chances
#'
#' `expected_hits` cannot use a population-unit count as a sample coverage
#' denominator: draw occurrences may exceed both distinct selected units and
#' the frame universe. Restrict the check to the stages being reported so a
#' partial execution is described from its realized path only.
#' @noRd
digest_path_has_expected_hits <- function(digest, stage_ids) {
  if (is_null(digest) || length(digest$stages) == 0L) {
    return(FALSE)
  }
  digest_stage_ids <- vapply(
    digest$stages, function(st) st$stage_id, integer(1)
  )
  stages <- digest$stages[digest_stage_ids %in% stage_ids]
  any(vapply(
    stages,
    function(st) identical(st$chance_kind, "expected_hits"),
    logical(1)
  ))
}

#' Digest-backed coverage line for the tbl_sample header
#'
#' At most one line: the stage count and the ultimate-unit coverage,
#' "3 stages \u00b7 360/19,344 units". It appears only when the digest
#' supports a complete universe denominator (a universe-scope first
#' stage with complete descendant counts, or a single element stage
#' over the universe) and the realized count is not replicate-varying.
#' The per-stage detail lives in summary() and frame_summary().
#' @noRd
digest_coverage_line <- function(x) {
  digest <- get_frame_digest(x)
  if (is_null(digest) || identical(digest$status, "invalidated")) {
    return(character(0))
  }
  fmt <- function(v) format(v, big.mark = ",", trim = TRUE)

  stages <- digest$stages
  k <- length(stages)
  stages_executed <- get_stages_executed(x)
  # A digest describing fewer stages than were executed (a partial
  # replicated manifest) cannot state the sample's coverage.
  if (k < length(stages_executed)) {
    return(character(0))
  }
  last <- stages[[k]]

  # Ultimate units are counted at the element level of the last stage.
  if (anyNA(last$pools$n_realized)) {
    return(character(0))
  }
  realized <- sum(last$pools$n_realized)

  if (digest_path_has_expected_hits(digest, stages_executed)) {
    noun <- if (identical(last$chance_kind, "expected_hits")) {
      " draws"
    } else {
      " units"
    }
    return(c("Sampling" = paste0(
      k, if (k == 1L) " stage" else " stages",
      " | ", fmt(realized), noun
    )))
  }

  total <- digest_universe_units(digest)
  if (is.na(total)) {
    return(character(0))
  }

  c("Sampling" = paste0(
    k, if (k == 1L) " stage" else " stages",
    " | ", fmt(realized), "/", fmt(total), " units"
  ))
}

#' Complete ultimate-unit universe denominator of a digest, or NA
#'
#' A number only when the digest supports it: an element-level last
#' stage, and either a universe-scope single element stage or a
#' universe-scope cluster first stage with complete descendant counts.
#' @noRd
digest_universe_units <- function(digest) {
  stages <- digest$stages
  k <- length(stages)
  first <- stages[[1]]
  if (!identical(stages[[k]]$unit_level, "element")) {
    return(NA_real_)
  }
  if (k == 1L) {
    if (identical(first$scope, "universe") && !anyNA(first$pools$N)) {
      return(sum(first$pools$N))
    }
    return(NA_real_)
  }
  if (
    identical(first$unit_level, "cluster") &&
      identical(first$scope, "universe") &&
      !is_null(first$units) &&
      "n_descendants" %in% names(first$units) &&
      !anyNA(first$units$n_descendants)
  ) {
    return(sum(first$units$n_descendants))
  }
  NA_real_
}

#' @rdname print.samplyr
#' @export
tbl_sum.tbl_sample <- function(x, ...) {
  design <- get_design(x)
  share <- attr(x, "metadata")$weight_share
  dims <- paste(nrow(x), "\u00d7", ncol(x))
  if (!is_null(design$title)) {
    first <- c("A tbl_sample" = paste0(dims, " | ", design$title))
  } else {
    first <- c("A tbl_sample" = dims)
  }

  result <- first

  stages_exec <- get_stages_executed(x)
  n_total_stages <- length(design$stages)
  if (length(stages_exec) < n_total_stages) {
    result <- c(
      result,
      "Stages" = paste0(
        paste(stages_exec, collapse = ", "),
        "/",
        n_total_stages
      )
    )
  }

  result <- c(result, digest_coverage_line(x))

  if (has_multiple_replicates(x) && !anyNA(x$.replicate)) {
    n_reps <- length(unique(x$.replicate))
    result <- c(result, "Replicates" = as.character(n_reps))
  }

  if (inherits(x, "grouped_df")) {
    result <- c(
      result,
      "Groups" = paste(dplyr::group_vars(x), collapse = ", ")
    )
  }

  mods <- sample_modifications(x)
  if (length(mods) > 0) {
    result <- c(result, "Modified" = paste(mods, collapse = ", "))
  }

  if (".weight" %in% names(x) && nrow(x) > 0) {
    w <- x$.weight
    # A transformed sample's rows are target units and its weights are
    # estimation weights, so the header says which kind these are rather than
    # leaving them to read as design weights.
    kind <- if (is_null(share)) "" else "shared, "
    result <- c(
      result,
      "Weights" = paste0(
        kind,
        round(mean(w), 2),
        " [",
        round(min(w), 2),
        ", ",
        round(max(w), 2),
        "]"
      )
    )
  }

  # The recorded design describes selection from the source population, not
  # these rows. Naming the source is what keeps the stage line above from
  # reading as a description of the target sample.
  if (!is_null(share)) {
    result <- c(
      result,
      "Shared from" = paste0(
        nrow(share$source_sample), " sampled row",
        if (nrow(share$source_sample) == 1L) "" else "s"
      )
    )
  }

  result
}

#' @rdname print.samplyr
#' @export
print.rotation_program <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cat_rule("Rotation Program")

  n_cohorts <- length(x$cohorts)
  n_waves <- length(x$waves)
  cat("\n")
  cli::cat_bullet(
    paste0(
      n_cohorts, plural_suffix(n_cohorts, " cohort"),
      " over ", n_waves, plural_suffix(n_waves, " wave")
    ),
    bullet = "info"
  )

  schedule <- x$schedule
  for (nm in names(x$cohorts)) {
    rows <- schedule[schedule$cohort == nm, , drop = FALSE]
    live <- sort(unique(rows$wave[rows$active]))
    n_panels <- x$panels[[nm]]
    cli::cat_bullet(
      paste0(
        nm, ": ", nrow(x$cohorts[[nm]]), " rows, ",
        n_panels, plural_suffix(n_panels, " panel"),
        ", enters at wave ", x$entry_wave[[nm]],
        ", live at ", plural_suffix(length(live), "wave"), " ",
        paste(live, collapse = ", ")
      ),
      bullet = "bullet"
    )
  }
  cat("\n")
  invisible(x)
}

#' @rdname print.samplyr
#' @export
print.rotation_wave <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cat_rule(paste("Rotation Wave", attr(x, "wave")))
  cat("\n")

  if (length(x) == 0) {
    cli::cat_bullet("No cohort is live at this wave.", bullet = "info")
    cat("\n")
    return(invisible(x))
  }

  for (nm in names(x)) {
    panels <- attr(x[[nm]], "metadata")$wave$active_panels
    cli::cat_bullet(
      paste0(
        nm, ": ", nrow(x[[nm]]), " rows, ",
        plural_suffix(length(panels), "panel"), " ",
        paste(panels, collapse = ", ")
      ),
      bullet = "bullet"
    )
  }
  # Load-bearing rather than decorative: the components are separately
  # weighted and row-binding them does not produce a combined sample.
  cli::cat_bullet(
    "Weights are valid within a cohort and are not combined.",
    bullet = "info"
  )
  cat("\n")
  invisible(x)
}

#' @rdname print.samplyr
#' @export
print.frame_stack <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cat_rule("Frame Stack")
  cat("\n")

  membership <- attr(x, "membership")
  cli::cat_bullet(
    cli::format_inline(paste0(
      "{length(x)} frames over key {.field {attr(x, 'key')}}"
    )),
    bullet = "info"
  )

  for (nm in names(x)) {
    seed <- attr(x[[nm]], "seed")
    cli::cat_bullet(
      cli::format_inline(paste0(
        nm, ": {nrow(x[[nm]])} rows, {.field {membership[[nm]]}}",
        if (is_null(seed)) ", no seed" else ", seed {seed}"
      )),
      bullet = "bullet"
    )
  }
  overlaps <- attr(x, "overlaps")
  if (!is_null(overlaps)) {
    cli::cat_bullet(
      cli::format_inline(if (is_null(overlaps$cols)) {
        "Overlap {overlaps$scale} resolved from the registers"
      } else {
        "Overlaps declared as {overlaps$scale}: {.field {overlaps$cols}}"
      }),
      bullet = "info"
    )
  }
  # Load-bearing rather than decorative: a unit listed in two frames is on two
  # rows carrying two different design weights, and which compositing factor
  # reconciles them is an estimation-time choice.
  cli::cat_bullet(
    "Weights are each frame's own and are not composited here.",
    bullet = "info"
  )
  cat("\n")
  invisible(x)
}

#' @rdname print.samplyr
#' @export
print.shared_sample_design <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cat_rule("Shared Weight Design")
  cat("\n")

  spec <- attr(x, "transformation")
  # One source line per message. cli::format_inline() keeps a string's own
  # newlines and indentation, so a wrapped literal prints wrapped.
  cli::cat_bullet(
    cli::format_inline(paste0(
      "Source keyed by {.field {names(spec$by)}}, ",
      "targets keyed by {.field {names(spec$to)}}"
    )),
    bullet = "info"
  )
  cli::cat_bullet(
    cli::format_inline(switch(
      spec$within$mode,
      cluster = "Links grouped within {.field {spec$within$col}}",
      extended = "Links extended across {.field {spec$within$col}}",
      singleton = "Each target unit its own cluster"
    )),
    bullet = "bullet"
  )
  cli::cat_bullet(
    cli::format_inline(switch(
      spec$multiplicity$mode,
      complete_links = "Denominator counted from the supplied links",
      weighted_links = paste0(
        "Denominator {.field {spec$multiplicity$col}} ",
        "over {.field {spec$multiplicity$total_col}}"
      ),
      complete_weighted_links = paste0(
        "Denominator {.field {spec$multiplicity$col}}, ",
        "summed over the supplied links"
      )
    )),
    bullet = "bullet"
  )
  # The reason this object is not the sample: what it needs and does not have.
  cli::cat_bullet(
    paste0(
      "Replay with the source register, the links and the targets ",
      "to rebuild the sample."
    ),
    bullet = "info"
  )
  cat("\n")
  invisible(x)
}

#' @rdname print.samplyr
#' @export
print.frame_stack_design <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cat_rule("Frame Stack Design")
  cat("\n")

  membership <- attr(x, "membership")
  cli::cat_bullet(
    cli::format_inline(paste0(
      "{length(x)} frames over key {.field {attr(x, 'key')}}"
    )),
    bullet = "info"
  )

  for (nm in names(x)) {
    receipt <- attr(x[[nm]], "execution")
    seed <- receipt$seed
    cli::cat_bullet(
      cli::format_inline(paste0(
        nm, ": {.field {membership[[nm]]}}",
        if (is_null(seed)) ", no seed" else ", seed {seed}"
      )),
      bullet = "bullet"
    )
  }
  overlaps <- attr(x, "overlaps")
  if (!is_null(overlaps)) {
    cli::cat_bullet(
      cli::format_inline(
        "Overlaps declared as {overlaps$scale}: {.field {overlaps$cols}}"
      ),
      bullet = "info"
    )
  }
  # The counterpart of the frame_stack note. There are no rows here at all,
  # so the thing to say is what it takes to get them.
  cli::cat_bullet(
    "Replay each component against its register to rebuild the collection.",
    bullet = "info"
  )
  cat("\n")
  invisible(x)
}

#' @rdname print.samplyr
#' @export
print.samplyr_overlap_spec <- function(x, ...) {
  rlang::check_dots_empty()
  cli::cat_bullet(
    cli::format_inline(
      "Overlap {x$scale}, by frame: {.field {x$cols}}"
    ),
    bullet = "info"
  )
  invisible(x)
}

#' @rdname print.samplyr
#' @export
print.samplyr_exante_overlap_spec <- function(x, ...) {
  rlang::check_dots_empty()
  # format_inline() keeps the whitespace it is given, so this stays on one
  # line however long it is.
  cli::cat_bullet(
    cli::format_inline("Overlap {x$scale} to resolve from {length(x$frames)} register{?s}, keyed by {.field {unname(x$by)}}"),
    bullet = "info"
  )
  invisible(x)
}

#' Append a plural "s" to a word, optionally after a count
#' @noRd
plural_suffix <- function(n, word) {
  paste0(word, if (n == 1) "" else "s")
}
