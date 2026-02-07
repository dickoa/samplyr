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
  cat("== Sampling Design ==\n")

  if (!is_null(x$title)) {
    cat("Title:", x$title, "\n")
  }

  n_stages <- length(x$stages)
  cat("Number of stages:", n_stages, "\n\n")

  for (i in seq_along(x$stages)) {
    print_stage(x$stages[[i]], i)
  }
  invisible(x)
}

#' @noRd
print_stage <- function(stage, num) {
  if (!is_null(stage$label)) {
    cat("-- Stage", num, ":", stage$label, "--\n")
  } else {
    cat("-- Stage", num, "--\n")
  }

  if (!is_null(stage$strata)) {
    strata <- stage$strata
    vars_str <- paste(strata$vars, collapse = ", ")
    cat("* Stratify by:", vars_str, "\n")
    if (!is_null(strata$alloc)) {
      cat("  Allocation:", strata$alloc, "\n")
    }
  }

  if (!is_null(stage$clusters)) {
    vars_str <- paste(stage$clusters$vars, collapse = ", ")
    cat("* Cluster by:", vars_str, "\n")
  }

  if (!is_null(stage$draw_spec)) {
    draw <- stage$draw_spec
    draw_desc <- format_draw_spec(draw)
    cat("* Draw:", draw_desc, "\n")
  } else {
    cat("! (incomplete - no draw specification)\n")
  }
  cat("\n")
}

#' @noRd
format_draw_spec <- function(draw) {
  parts <- c()

  if (!is_null(draw$n)) {
    if (is.data.frame(draw$n)) {
      parts <- c(parts, "n = <custom data frame>")
    } else {
      parts <- c(parts, glue("n = {draw$n}"))
    }
  }
  if (!is_null(draw$frac)) {
    if (is.data.frame(draw$frac)) {
      parts <- c(parts, "frac = <custom data frame>")
    } else {
      parts <- c(parts, glue("frac = {draw$frac}"))
    }
  }

  parts <- c(parts, glue("method = {draw$method}"))

  if (!is_null(draw$mos)) {
    parts <- c(parts, glue("mos = {draw$mos}"))
  }

  if (!is_null(draw$control)) {
    control_str <- format_control_quos(draw$control)
    parts <- c(parts, glue("control = {control_str}"))
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

#' @rdname print.samplyr
#' @export
print.tbl_sample <- function(x, ...) {
  design <- get_design(x)
  if (!is_null(design$title)) {
    cat("== tbl_sample:", design$title, "==\n")
  } else {
    cat("== tbl_sample ==\n")
  }

  stages_exec <- get_stages_executed(x)
  n_total_stages <- length(design$stages)
  if (length(stages_exec) < n_total_stages) {
    cat(
      "Stages executed:",
      paste(stages_exec, collapse = ", "),
      "of",
      n_total_stages,
      "\n"
    )
  }

  if (".weight" %in% names(x)) {
    w <- x$.weight
    cat(
      "Weights:",
      round(min(w), 2),
      "-",
      round(max(w), 2),
      "(mean:",
      round(mean(w), 2),
      ")\n"
    )
  }

  cat("\n")
  NextMethod()
  invisible(x)
}
