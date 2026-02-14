#' Summarise a tbl_sample
#'
#' Produces a detailed summary of a sample including design specification,
#' execution details, per-stage stratum allocation tables, and weight
#' diagnostics.
#'
#' @param object A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `object`. Called for its side effect of
#'   printing a summary.
#'
#' @details
#' The summary has four sections:
#'
#' **Design** -- the sampling specification (method, strata, clusters,
#' allocation) for each stage.
#'
#' **Execution** -- seed, stages executed, timestamp, and total sample
#' size.
#'
#' **Allocation** -- per-stage stratum tables showing population size
#' (N_h), sample size (n_h), and sampling fraction (f_h). Requires
#' `.fpc_k` columns to be present (produced by samplyr >= 0.2.0).
#'
#' **Weights** -- range, mean, coefficient of variation, Kish design
#' effect, and effective sample size.
#'
#' @examples
#' sample <- sampling_design() |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(kenya_health, seed = 42)
#'
#' summary(sample)
#'
#' @seealso [tbl_sum.tbl_sample()] for the compact print method,
#'   [as_svydesign()] for exporting to the survey package
#'
#' @export
summary.tbl_sample <- function(object, ...) {
  design <- get_design(object)
  stages_executed <- get_stages_executed(object)
  metadata <- attr(object, "metadata")
  seed <- attr(object, "seed")

  if (!is_null(design$title)) {
    cli::cat_rule(paste0("Sample Summary: ", design$title))
  } else {
    cli::cat_rule("Sample Summary")
  }

  cat("\n")
  n_total_stages <- length(design$stages)
  info_parts <- paste0("n = ", format(nrow(object), big.mark = ","))
  info_parts <- c(
    info_parts,
    paste0("stages = ", length(stages_executed), "/", n_total_stages)
  )
  if (!is_null(seed)) {
    info_parts <- c(info_parts, paste0("seed = ", seed))
  }
  cli::cat_bullet(
    paste(info_parts, collapse = " | "),
    bullet = "info"
  )

  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    label <- stage_spec$label %||% paste("Stage", stage_idx)

    cat("\n")
    cli::cat_rule(left = paste0("Design: ", label))

    if (!is_null(stage_spec$strata)) {
      vars_str <- paste(stage_spec$strata$vars, collapse = ", ")
      alloc_str <- if (!is_null(stage_spec$strata$alloc)) {
        paste0(" (", stage_spec$strata$alloc, ")")
      } else {
        ""
      }
      cli::cat_bullet(
        paste0("Strata: ", vars_str, alloc_str),
        bullet = "bullet"
      )
    }

    if (!is_null(stage_spec$clusters)) {
      vars_str <- paste(stage_spec$clusters$vars, collapse = ", ")
      cli::cat_bullet(paste0("Cluster: ", vars_str), bullet = "bullet")
    }

    method <- stage_spec$draw_spec$method
    wr_label <- if (method %in% wr_methods) " (with replacement)" else ""
    cli::cat_bullet(
      paste0("Method: ", method, wr_label),
      bullet = "bullet"
    )

    if (!is_null(stage_spec$draw_spec$mos)) {
      cli::cat_bullet(
        paste0("Size: ", stage_spec$draw_spec$mos),
        bullet = "bullet"
      )
    }
  }

  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    fpc_col <- paste0(".fpc_", stage_idx)
    has_fpc <- fpc_col %in% names(object)
    label <- stage_spec$label %||% paste("Stage", stage_idx)

    cat("\n")
    cli::cat_rule(left = paste0("Allocation: ", label))

    if (!is_null(stage_spec$strata) && has_fpc) {
      strata_vars <- stage_spec$strata$vars

      alloc_tbl <- object |>
        group_by(across(all_of(strata_vars))) |>
        summarise(
          N_h = .data[[fpc_col]][1],
          n_h = n(),
          f_h = n() / .data[[fpc_col]][1],
          .groups = "drop"
        )

      print_allocation_table(alloc_tbl, strata_vars)
    } else if (has_fpc) {
      N <- object[[fpc_col]][1]
      n_sel <- nrow(object)
      f <- format(round(n_sel / N, 4), nsmall = 4)
      cli::cat_bullet(
        paste0("N = ", N, ", n = ", n_sel, ", f = ", f),
        bullet = "bullet"
      )
    } else {
      cli::cat_bullet(
        "(no FPC information available)",
        bullet = "warning"
      )
    }
  }

  if (".weight" %in% names(object)) {
    cat("\n")
    cli::cat_rule(left = "Weights")
    w <- object$.weight
    cv_w <- if (length(w) > 1) stats::sd(w) / mean(w) else 0
    deff <- design_effect(w)
    eff <- effective_n(w)

    w_min <- round(min(w), 2)
    w_max <- round(max(w), 2)
    w_mean <- round(mean(w), 2)
    cv_fmt <- round(cv_w, 2)
    deff_fmt <- round(deff, 2)
    eff_fmt <- round(eff, 0)
    cli::cat_bullet(
      paste0("Range: [", w_min, ", ", w_max, "]"),
      bullet = "bullet"
    )
    cli::cat_bullet(
      paste0("Mean:  ", w_mean, " \u00b7 CV: ", cv_fmt),
      bullet = "bullet"
    )
    cli::cat_bullet(
      paste0("DEFF:  ", deff_fmt, " \u00b7 n_eff: ", eff_fmt),
      bullet = "bullet"
    )
  }

  cat("\n")
  invisible(object)
}

#' @noRd
print_allocation_table <- function(alloc_tbl, strata_vars) {
  display <- alloc_tbl
  display$f_h <- format(round(display$f_h, 4), nsmall = 4)

  total_N <- sum(as.integer(alloc_tbl$N_h))
  total_n <- sum(as.integer(alloc_tbl$n_h))
  total_f <- format(round(total_n / total_N, 4), nsmall = 4)

  display$N_h <- as.character(display$N_h)
  display$n_h <- as.character(display$n_h)

  col_names <- c(strata_vars, "N_h", "n_h", "f_h")
  col_widths <- vapply(
    col_names,
    function(col) {
      vals <- as.character(display[[col]])
      if (col == "N_h") {
        vals <- c(vals, as.character(total_N))
      }
      if (col == "n_h") {
        vals <- c(vals, as.character(total_n))
      }
      if (col == "f_h") {
        vals <- c(vals, total_f)
      }
      max(nchar(col), max(nchar(vals)))
    },
    integer(1)
  )

  header <- paste(
    mapply(
      function(nm, w) formatC(nm, width = w, flag = "-"),
      col_names,
      col_widths
    ),
    collapse = "  "
  )
  cat("  ", header, "\n", sep = "")

  for (i in seq_len(nrow(display))) {
    row <- paste(
      mapply(
        function(col, w) {
          formatC(as.character(display[[col]][i]), width = w, flag = "-")
        },
        col_names,
        col_widths
      ),
      collapse = "  "
    )
    cat("  ", row, "\n", sep = "")
  }

  strata_width <- sum(col_widths[seq_along(strata_vars)]) +
    2 * (length(strata_vars) - 1)
  sep_parts <- c(
    strrep(" ", strata_width),
    strrep("\u2500", col_widths["N_h"]),
    strrep("\u2500", col_widths["n_h"]),
    strrep("\u2500", col_widths["f_h"])
  )
  cat("  ", paste(sep_parts, collapse = "  "), "\n", sep = "")

  total_label <- formatC("Total", width = strata_width, flag = "-")
  total_row <- paste0(
    total_label,
    "  ",
    formatC(as.character(total_N), width = col_widths["N_h"], flag = "-"),
    "  ",
    formatC(as.character(total_n), width = col_widths["n_h"], flag = "-"),
    "  ",
    formatC(total_f, width = col_widths["f_h"], flag = "-")
  )
  cat("  ", total_row, "\n", sep = "")
}
