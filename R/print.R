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
    draw_desc <- format_draw_spec(stage$draw_spec)
    cli::cat_bullet(paste0("Draw: ", draw_desc), bullet = "bullet")
  } else {
    cli::cat_bullet(
      paste0("(incomplete ", "\u2014", " no draw specification)"),
      bullet = "warning"
    )
  }
}

#' @noRd
format_draw_spec <- function(draw) {
  parts <- c()

  if (!is_null(draw$n)) {
    if (is.data.frame(draw$n)) {
      parts <- c(parts, "n = <custom data frame>")
    } else {
      parts <- c(parts, paste0("n = ", draw$n))
    }
  }
  if (!is_null(draw$frac)) {
    if (is.data.frame(draw$frac)) {
      parts <- c(parts, "frac = <custom data frame>")
    } else {
      parts <- c(parts, paste0("frac = ", draw$frac))
    }
  }

  parts <- c(parts, paste0("method = ", draw$method))

  if (!is_null(draw$mos)) {
    parts <- c(parts, paste0("mos = ", draw$mos))
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

  if (".weight" %in% names(x)) {
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
