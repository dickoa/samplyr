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
    print_stage(x$stages[[i]], i, x)
  }
  cat("\n")
  invisible(x)
}

#' @noRd
print_stage <- function(stage, num, design = NULL) {
  cat("\n")
  if (!is_null(stage$label)) {
    cli::cat_rule(left = paste0("Stage ", num, ": ", stage$label))
  } else {
    cli::cat_rule(left = paste("Stage", num))
  }

  if (!is_null(stage$strata)) {
    strata <- stage$strata
    vars_str <- paste(strata$vars, collapse = ", ")
    alloc_str <- if (identical(strata$alloc, "proportional") &&
                     !is_null(strata$importance)) {
      " (proportional to importance)"
    } else if (!is_null(strata$alloc)) {
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
    cli::cat_bullet(
      paste0("Cluster: ", format_cluster_vars(stage$clusters)),
      bullet = "bullet"
    )
  }

  if (!is_null(stage$draw_spec)) {
    has_strata <- !is_null(stage$strata)
    alloc <- stage$strata$alloc
    draw_desc <- format_draw_spec(
      stage$draw_spec,
      has_strata = has_strata,
      alloc = alloc,
      parent = nearest_parent_units(design, num)
    )
    cli::cat_bullet(paste0("Draw: ", draw_desc), bullet = "bullet")
  } else {
    cli::cat_bullet(
      "Incomplete: no draw specification",
      bullet = "warning"
    )
  }
}

#' The sizes a certainty-aware svyplan plan gives its stage
#'
#' Stage 1 selects by the plan's classification, so it shows the certainty
#' and drawn PSU counts. The stage below takes each PSU's own take, named by
#' the PSU id column.
#' @noRd
format_certainty_plan <- function(plan) {
  register <- plan$register
  if (identical(plan$role, "take")) {
    takes <- range(register$n_take)
    size <- if (takes[1] == takes[2]) {
      takes[1]
    } else {
      paste(takes[1], "to", takes[2])
    }
    return(paste0("<svyplan plan, ", size, " per ", plan$id_var, ">"))
  }
  zone <- if (!is_null(plan$n_psu_per_zone)) {
    paste(plan$n_psu_per_zone, "per zone")
  }
  paste0("<", paste(c(
    "svyplan plan",
    paste(sum(register$certainty), "certainty"),
    paste(sum(plan$n_psu_draw), "drawn"),
    zone,
    "per stratum"
  ), collapse = ", "), ">")
}

#' Qualify a scalar n/frac with its scope when the stage is stratified
#'
#' Without stratification, `n = 300` is unambiguous.
#' With stratification and no `alloc`, it is per stratum.
#' With stratification and `alloc`, it is the total.
#' Named vectors and data frames are already explicit per-stratum.
#' @noRd
format_draw_spec <- function(draw, has_strata = FALSE, alloc = NULL,
                             parent = NULL) {
  parts <- c()
  # A later stage runs once in every unit its parent stage selected.
  per_parent <- if (length(parent) > 0L) {
    paste0("per ", paste(parent, collapse = "/"))
  }

  scope_tag <- function(value, has_strata, alloc) {
    stratum <- if (!has_strata) {
      NULL
    } else if (length(value) > 1 && !is_null(names(value))) {
      "per stratum"
    } else if (length(value) == 1) {
      if (is_null(alloc)) "per stratum" else "total"
    }
    tags <- c(stratum, per_parent)
    if (length(tags) == 0L) "" else paste0(" (", paste(tags, collapse = ", "), ")")
  }

  format_scalar <- function(value_name, value, has_strata, alloc) {
    if (length(value) > 1 && !is_null(names(value))) {
      return(paste0(
        value_name, " = <", length(value), " values, ",
        paste(c("per stratum", per_parent), collapse = ", "), ">"
      ))
    }
    paste0(value_name, " = ", value, scope_tag(value, has_strata, alloc))
  }

  table_form <- paste0(
    "<custom data frame, ",
    paste(c("per stratum", per_parent), collapse = ", "), ">"
  )
  if (!is_null(draw$certainty_plan)) {
    parts <- c(parts, paste0("n = ", format_certainty_plan(draw$certainty_plan)))
  } else if (!is_null(draw$n)) {
    if (is.data.frame(draw$n)) {
      parts <- c(parts, paste0("n = ", table_form))
    } else {
      parts <- c(parts, format_scalar("n", draw$n, has_strata, alloc))
    }
  }
  if (!is_null(draw$frac)) {
    if (is.data.frame(draw$frac)) {
      parts <- c(parts, paste0("frac = ", table_form))
    } else {
      parts <- c(parts, format_scalar("frac", draw$frac, has_strata, alloc))
    }
  }
  if (!is_null(draw$min_n)) {
    parts <- c(parts, paste0("min_n = ", draw$min_n))
  }
  if (!is_null(draw$max_n)) {
    parts <- c(parts, paste0("max_n = ", draw$max_n))
  }

  parts <- c(parts, paste0("method = ", draw$method))

  if (!is_null(draw$mos)) {
    parts <- c(parts, paste0("mos = ", draw$mos))
  }

  # Settings that change the draw print when they differ from the default.
  for (arg in c("certainty_size", "certainty_prop")) {
    value <- draw[[arg]]
    if (is.data.frame(value)) {
      parts <- c(parts, paste0(arg, " = ", table_form))
    } else if (!is_null(value)) {
      parts <- c(parts, paste0(arg, " = ", value))
    }
  }
  non_default <- c(certainty_overflow = "error", on_empty = "error",
                   round = "up")
  for (arg in names(non_default)) {
    value <- draw[[arg]]
    if (!is_null(value) && !identical(value, non_default[[arg]])) {
      parts <- c(parts, paste0(arg, " = ", value))
    }
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

  if (identical(draw$on_capped, "error")) {
    parts <- c(parts, "on_capped = error")
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
  # Partial replicated digests cannot state sample coverage.
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
    # No denominator the digest can vouch for: the count stands alone.
    return(c("Sampling" = paste0(
      k, if (k == 1L) " stage" else " stages",
      " | ", fmt(realized), " units"
    )))
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
#' universe-scope cluster first stage with complete descendant counts, read
#' from the same frame as the last stage. With one register per stage, or a
#' listing supplied to a continuation, the first frame's descendants are not
#' the units the last stage drew from, and the ratio read "15/13 units".
#' @noRd
digest_universe_units <- function(digest) {
  stages <- digest$stages
  k <- length(stages)
  first <- stages[[1]]
  if (!identical(stages[[k]]$unit_level, "element")) {
    return(NA_real_)
  }
  if (!identical(first$frame_ref, stages[[k]]$frame_ref)) {
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

  result <- c(result, panel_header_line(x))

  if (has_multiple_replicates(x) && !anyNA(x$.replicate)) {
    n_reps <- n_replicates(x)
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
    # Label transformed weights as estimation weights.
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

  # Name the source population described by the recorded design.
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

#' The header line naming a sample's panels, periods or wave
#'
#' A periods sample names its periods and the column that holds them. A
#' rotation sample names its groups and whether a schedule declares their
#' waves. A materialized wave or period names which one it is.
#' @noRd
panel_header_line <- function(x) {
  metadata <- attr(x, "metadata")
  record <- metadata$panel_assignment
  if (is_null(record)) {
    return(NULL)
  }
  k <- as.integer(record$panels)
  periods <- identical(record$certainty, "assigned")
  wave <- metadata[["wave"]]
  if (!is_null(wave)) {
    several <- length(wave$wave) > 1L
    if (periods) {
      label <- if (several) "Periods" else "Period"
      return(stats::setNames(paste0(
        wave_span(wave$wave), " of ", k,
        if (isTRUE(wave$provisional)) ", provisional until every period is drawn"
      ), label))
    }
    n_waves <- max(as.integer(unlist(record$schedule$wave)))
    label <- if (several) "Waves" else "Wave"
    return(stats::setNames(
      paste0(wave_span(wave$wave), " of ", n_waves), label
    ))
  }
  if (periods) {
    return(c(
      "Periods" = paste0(k, " (in .panel)"),
      "Drawn" = wave_draws_progress(x)
    ))
  }
  schedule <- record$schedule
  if (is_null(schedule) || length(schedule) == 0L) {
    return(c("Panels" = paste0(k, " rotation groups, no schedule")))
  }
  n_waves <- max(as.integer(unlist(schedule$wave)))
  c(
    "Panels" = paste0(
      k, " rotation groups over ", n_waves, plural_suffix(n_waves, " wave"),
      if (schedule_has_tenures(schedule)) ", with tenures"
    ),
    "Drawn" = wave_draws_progress(x)
  )
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
  # Row-bound components are not yet a combined sample.
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
      cli::format_inline(if (is_resolved_overlaps(overlaps)) {
        if (any(overlaps$probability_quality == "approximate")) {
          "Overlap targets resolved from the registers (approximate probabilities accepted)"
        } else "Overlap {overlaps$scale} resolved from the registers"
      } else {
        "Overlaps declared as {overlaps$scale}: {.field {overlaps$cols}}"
      }),
      bullet = "info"
    )
  }
  # Duplicate-frame units need an estimation-time compositing choice.
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
  # Keep each source message on one line.
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
  # State what this object still needs to become a sample.
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
  # State how to materialize a stack with no rows.
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
  # Keep the formatted note on one line.
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

#' The sampling units of the nearest earlier clustered stage
#'
#' A later stage's `n` and `frac` apply within each unit that stage selected,
#' so the print says "per ea_id". NULL at stage 1, or with no clustered
#' ancestor.
#' @noRd
nearest_parent_units <- function(design, stage_idx) {
  if (is_null(design) || stage_idx <= 1L) {
    return(NULL)
  }
  for (k in rev(seq_len(stage_idx - 1L))) {
    vars <- declared_cluster_vars(design$stages[[k]]$clusters)
    if (length(vars) > 0L) {
      return(vars)
    }
  }
  NULL
}

#' A stage's cluster variables as declared, with the strata frame
#' resolution nested them in and the opt-out
#' @noRd
format_cluster_vars <- function(spec) {
  out <- paste(declared_cluster_vars(spec), collapse = ", ")
  if (length(spec$within) > 0L) {
    out <- paste0(out, " (within ", paste(spec$within, collapse = ", "), ")")
  }
  if (isFALSE(spec$nest)) {
    out <- paste0(out, ", nest = FALSE")
  }
  out
}

#' @export
print.samplyr_design_comparison <- function(x, ...) {
  cli::cat_rule("Design comparison")
  approximate <- identical(x$probability_quality, "approximate")
  what <- if (identical(x$level, "allocation")) {
    "Allocations"
  } else if (approximate) {
    "First-order inclusion probability targets"
  } else {
    "First-order inclusion probabilities"
  }
  cli::cat_line(switch(
    x$verdict,
    agree = paste(what, "agree on this frame."),
    differ = paste(what, "differ on this frame."),
    unresolved = paste(what, "could not be compared in full on this frame.")
  ))
  if (approximate) {
    cli::cat_line(
      "Both designs use methods that meet these targets only approximately."
    )
  }
  cli::cat_line()
  checks <- x$checks
  for (i in seq_len(nrow(checks))) {
    where <- if (is.na(checks$stage[[i]])) {
      check <- checks$check[[i]]
      paste0(toupper(substr(check, 1, 1)), substring(check, 2))
    } else {
      paste0("Stage ", checks$stage[[i]], " ", checks$check[[i]])
    }
    cli::cat_bullet(
      paste0(where, ": ", checks$status[[i]], ". ", checks$detail[[i]]),
      bullet = switch(
        checks$status[[i]],
        agree = "tick",
        differ = "cross",
        "info"
      )
    )
  }
  print_specification(x$specification)
  invisible(x)
}

#' The side-by-side methods and measures of size, outside the verdict
#' @noRd
print_specification <- function(spec) {
  # A stage without a measure of size in either design has nothing to show.
  shown <- !(spec$aspect == "mos" & is.na(spec$x) & is.na(spec$y))
  spec <- spec[shown, , drop = FALSE]
  if (nrow(spec) == 0L) {
    return(invisible(NULL))
  }
  cli::cat_line()
  cli::cat_line("Methods and measures of size, outside the verdict:")
  for (i in seq_len(nrow(spec))) {
    what <- if (spec$aspect[[i]] == "method") "method" else "measure of size"
    value <- function(v) if (is.na(v)) "none" else v
    text <- switch(
      spec$status[[i]],
      same = paste0(value(spec$x[[i]]), " in both"),
      proportional = paste0(
        spec$x[[i]], " in x, ", spec$y[[i]],
        " in y, proportional on this frame"
      ),
      paste0(value(spec$x[[i]]), " in x, ", value(spec$y[[i]]), " in y")
    )
    cli::cat_bullet(
      paste0("Stage ", spec$stage[[i]], " ", what, ": ", text, "."),
      bullet = if (spec$status[[i]] == "differ") "info" else "tick"
    )
  }
  invisible(NULL)
}
