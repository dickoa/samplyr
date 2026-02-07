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
#' **Design** — the sampling specification (method, strata, clusters,
#' allocation) for each stage.
#'
#' **Execution** — seed, stages executed, timestamp, and total sample
#' size.
#'
#' **Allocation** — per-stage stratum tables showing population size
#' (N_h), sample size (n_h), and sampling fraction (f_h). Requires
#' `.fpc_k` columns to be present (produced by samplyr >= 0.2.0).
#'
#' **Weights** — range, mean, coefficient of variation, Kish design
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
#' @seealso [print.tbl_sample()] for the compact print method,
#'   [as_survey_design()] for exporting to the survey package
#'
#' @export
summary.tbl_sample <- function(object, ...) {
  design <- get_design(object)
  stages_executed <- get_stages_executed(object)
  metadata <- attr(object, "metadata")
  seed <- attr(object, "seed")

  cat("== Sample Summary ==\n\n")

  if (!is_null(design$title)) {
    cat("Title:", design$title, "\n")
  }

  cat("--- Design ---\n")
  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    label <- stage_spec$label %||% paste("Stage", stage_idx)
    cat("\n  ", label, "\n", sep = "")

    if (!is_null(stage_spec$strata)) {
      vars_str <- paste(stage_spec$strata$vars, collapse = ", ")
      alloc_str <- if (!is_null(stage_spec$strata$alloc)) {
        paste0(" (", stage_spec$strata$alloc, ")")
      } else {
        ""
      }
      cat("    Stratify by: ", vars_str, alloc_str, "\n", sep = "")
    }

    if (!is_null(stage_spec$clusters)) {
      vars_str <- paste(stage_spec$clusters$vars, collapse = ", ")
      cat("    Cluster by: ", vars_str, "\n", sep = "")
    }

    method <- stage_spec$draw_spec$method
    wr_label <- if (method %in% wr_methods) " (with replacement)" else ""
    cat("    Method: ", method, wr_label, "\n", sep = "")

    if (!is_null(stage_spec$draw_spec$mos)) {
      cat("    Size measure: ", stage_spec$draw_spec$mos, "\n", sep = "")
    }
  }

  cat("\n--- Execution ---\n")
  n_total_stages <- length(design$stages)
  cat(
    "  Stages: ",
    length(stages_executed),
    " of ",
    n_total_stages,
    "\n",
    sep = ""
  )
  cat("  Total sample size: ", nrow(object), "\n", sep = "")
  if (!is_null(seed)) {
    cat("  Seed: ", seed, "\n", sep = "")
  }
  if (!is_null(metadata$executed_at)) {
    cat("  Executed at: ", format(metadata$executed_at), "\n", sep = "")
  }

  cat("\n--- Allocation ---\n")

  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    fpc_col <- paste0(".fpc_", stage_idx)
    has_fpc <- fpc_col %in% names(object)

    label <- stage_spec$label %||% paste("Stage", stage_idx)

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

      cat("\n  ", label, ":\n", sep = "")
      print_allocation_table(alloc_tbl, strata_vars)
    } else if (has_fpc) {
      # Unstratified
      N <- object[[fpc_col]][1]
      n_sel <- nrow(object)
      cat(
        "\n  ",
        label,
        ": N = ",
        N,
        ", n = ",
        n_sel,
        ", f = ",
        format(round(n_sel / N, 4), nsmall = 4),
        "\n",
        sep = ""
      )
    } else {
      cat("\n  ", label, ": (no FPC information available)\n", sep = "")
    }
  }

  # --- Weights section ---
  if (".weight" %in% names(object)) {
    cat("\n--- Weights ---\n")
    w <- object$.weight
    cv_w <- if (length(w) > 1) stats::sd(w) / mean(w) else 0
    deff <- design_effect(w)
    eff_n <- effective_n(w)

    cat("  Range: ", round(min(w), 4), " - ", round(max(w), 4), "\n", sep = "")
    cat("  Mean: ", round(mean(w), 4), "\n", sep = "")
    cat("  CV: ", round(cv_w, 4), "\n", sep = "")
    cat("  Kish DEFF: ", round(deff, 4), "\n", sep = "")
    cat("  Effective n: ", round(eff_n, 1), "\n", sep = "")
  }

  cat("\n")
  invisible(object)
}

#' @noRd
print_allocation_table <- function(alloc_tbl, strata_vars) {
  display <- alloc_tbl
  display$f_h <- format(round(display$f_h, 4), nsmall = 4)
  display$N_h <- as.character(display$N_h)
  display$n_h <- as.character(display$n_h)

  col_names <- c(strata_vars, "N_h", "n_h", "f_h")
  col_widths <- vapply(
    col_names,
    function(col) {
      max(nchar(col), max(nchar(as.character(display[[col]]))))
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
  cat("    ", header, "\n", sep = "")

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
    cat("    ", row, "\n", sep = "")
  }

  total_N <- sum(as.integer(alloc_tbl$N_h))
  total_n <- sum(as.integer(alloc_tbl$n_h))
  total_f <- total_n / total_N

  strata_width <- sum(col_widths[seq_along(strata_vars)]) +
    2 * (length(strata_vars) - 1)
  total_label <- formatC("Total", width = strata_width, flag = "-")
  total_row <- paste0(
    total_label,
    "  ",
    formatC(as.character(total_N), width = col_widths["N_h"], flag = "-"),
    "  ",
    formatC(as.character(total_n), width = col_widths["n_h"], flag = "-"),
    "  ",
    formatC(
      format(round(total_f, 4), nsmall = 4),
      width = col_widths["f_h"],
      flag = "-"
    )
  )
  cat("    ", total_row, "\n", sep = "")
}
