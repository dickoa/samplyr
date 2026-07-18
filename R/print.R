#' Print Methods for samplyr Objects
#'
#' @name print.samplyr
#' @param x Object to print
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the input object
NULL

#' @rdname print.samplyr
#' @export
print.sampling_design <- function(x, ...) {
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
  # A digest describing fewer stages than were executed (a partial
  # replicated manifest) cannot state the sample's coverage.
  if (k < length(get_stages_executed(x))) {
    return(character(0))
  }
  first <- stages[[1]]
  last <- stages[[k]]

  # Ultimate units are counted at the element level of the last stage.
  if (!identical(last$unit_level, "element") ||
        anyNA(last$pools$n_realized)) {
    return(character(0))
  }
  realized <- sum(last$pools$n_realized)

  total <- if (k == 1L && identical(first$scope, "universe") &&
                 !anyNA(first$pools$N)) {
    sum(first$pools$N)
  } else if (
    k > 1L &&
      identical(first$unit_level, "cluster") &&
      identical(first$scope, "universe") &&
      !is_null(first$units) &&
      "n_descendants" %in% names(first$units) &&
      !anyNA(first$units$n_descendants)
  ) {
    sum(first$units$n_descendants)
  } else {
    return(character(0))
  }

  c("Sampling" = paste0(
    k, if (k == 1L) " stage" else " stages",
    " | ", fmt(realized), "/", fmt(total), " units"
  ))
}

#' @rdname print.samplyr
#' @export
tbl_sum.tbl_sample <- function(x, ...) {
  design <- get_design(x)
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
    result <- c(
      result,
      "Weights" = paste0(
        round(mean(w), 2),
        " [",
        round(min(w), 2),
        ", ",
        round(max(w), 2),
        "]"
      )
    )
  }

  result
}
