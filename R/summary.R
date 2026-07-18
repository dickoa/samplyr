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
#' The summary has three sections preceded by a header line showing
#' the total sample size, stages executed, and seed:
#'
#' **Design** -- the sampling specification (method, strata, clusters,
#' allocation) for each stage.
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
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 42)
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

  is_replicated <- has_multiple_replicates(object)
  if (is_replicated && anyNA(object$.replicate)) {
    cli::cat_bullet(
      "{.field .replicate} contains missing values. Replicate reporting disabled.",
      bullet = "warning"
    )
    is_replicated <- FALSE
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
  if (is_replicated) {
    n_reps <- length(unique(object$.replicate))
    n_per_rep <- nrow(object) %/% n_reps
    info_parts <- c(
      info_parts,
      paste0("reps = ", n_reps),
      paste0("n/rep ~ ", format(n_per_rep, big.mark = ","))
    )
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
    is_wr <- method %in% wr_methods ||
      identical(stage_spec$draw_spec$method_type, "wr")
    wr_label <- if (is_wr) " (with replacement)" else ""
    approx_label <- if (
      identical(stage_spec$draw_spec$method_probabilities, "approximate")
    ) {
      " (approximate probabilities)"
    } else {
      ""
    }
    cli::cat_bullet(
      paste0("Method: ", method, wr_label, approx_label),
      bullet = "bullet"
    )

    if (!is_null(stage_spec$draw_spec$mos)) {
      cli::cat_bullet(
        paste0("MOS: ", stage_spec$draw_spec$mos),
        bullet = "bullet"
      )
    }
    if (!is_null(stage_spec$draw_spec$aux)) {
      cli::cat_bullet(
        paste0("HT totals: ", paste(stage_spec$draw_spec$aux, collapse = ", ")),
        bullet = "bullet"
      )
    }
    if (!is_null(stage_spec$draw_spec$bounds)) {
      cli::cat_bullet(
        paste0("Bounded counts: ", paste(stage_spec$draw_spec$bounds, collapse = ", ")),
        bullet = "bullet"
      )
    }
    if (!is_null(stage_spec$draw_spec$spread)) {
      cli::cat_bullet(
        paste0("Spatial spread: ", paste(stage_spec$draw_spec$spread, collapse = ", ")),
        bullet = "bullet"
      )
    }
  }

  object_for_alloc <- if (is_replicated) {
    object[object$.replicate == min(object$.replicate), ]
  } else {
    object
  }

  digest <- get_frame_digest(object)
  digest_stage_ids <- if (
    !is_null(digest) && !identical(digest$status, "invalidated")
  ) {
    vapply(digest$stages, function(s) s$stage_id, integer(1))
  } else {
    integer(0)
  }

  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    fpc_col <- paste0(".fpc_", stage_idx)
    has_fpc <- fpc_col %in% names(object_for_alloc)
    label <- stage_spec$label %||% paste("Stage", stage_idx)

    dpos <- match(stage_idx, digest_stage_ids)
    if (!is.na(dpos)) {
      cat("\n")
      cli::cat_rule(left = paste0("Allocation: ", label))
      summary_stage_realization(
        digest$stages[[dpos]], stage_spec, design, digest, dpos,
        is_replicated = is_replicated
      )
      next
    }

    # f_h = n_h / N_h is the realised sample fraction. It is not a
    # unit-level inclusion probability for unequal-probability designs.
    # WR/PMR stages have no sampling fraction because their FPC is Inf.
    # A declared variance_family = "srs" asserts equal probability.
    stage_method <- stage_spec$draw_spec$method
    stage_method_type <- stage_spec$draw_spec$method_type
    is_wr_stage <- stage_method %in% c(wr_methods, pmr_methods) ||
      identical(stage_method_type, "wr")
    has_unequal_prob <- (
      stage_method %in% pps_wor_methods ||
        (
          is_balanced_method(stage_spec$draw_spec) &&
            !is_null(stage_spec$draw_spec$mos)
        ) ||
        (
          identical(stage_method_type, "wor") &&
            !is_null(stage_spec$draw_spec$mos)
        )
    ) &&
      !identical(stage_spec$draw_spec$method_variance, "srs")

    cat("\n")
    cli::cat_rule(left = paste0("Allocation: ", label))
    if (is_replicated) {
      cli::cat_bullet("Allocation for replicate 1.", bullet = "info")
    }

    # Build the full identity key for this stage's units once
    if (!is_null(stage_spec$clusters)) {
      ancestor_vars <- collect_ancestor_cluster_vars(design, stage_idx)
      stage_unit_vars <- unique(c(ancestor_vars, stage_spec$clusters$vars))
      stage_unit_vars <- intersect(stage_unit_vars, names(object_for_alloc))
    } else {
      stage_unit_vars <- NULL
    }

    if (is_wr_stage && has_fpc) {
      # WR stages have .fpc = Inf; a sampling fraction is not defined.
      n_drawn <- if (!is_null(stage_unit_vars)) {
        nrow(dplyr::distinct(
          object_for_alloc, across(all_of(stage_unit_vars))
        ))
      } else {
        nrow(object_for_alloc)
      }
      cli::cat_bullet(
        paste0("n = ", n_drawn, " (with replacement, no FPC)"),
        bullet = "bullet"
      )
    } else if (!is_null(stage_spec$strata) && has_fpc) {
      strata_vars <- stage_spec$strata$vars

      count_data <- object_for_alloc
      if (!is_null(stage_unit_vars)) {
        dedup_vars <- unique(c(strata_vars, stage_unit_vars))
        count_data <- count_data |>
          distinct(across(all_of(dedup_vars)), .keep_all = TRUE)
      }

      alloc_tbl <- count_data |>
        group_by(across(all_of(strata_vars))) |>
        summarise(
          N_h = .data[[fpc_col]][1],
          n_h = n(),
          f_h = n() / .data[[fpc_col]][1],
          .groups = "drop"
        )

      print_allocation_table(alloc_tbl, strata_vars)
      if (has_unequal_prob) {
        cli::cat_bullet(
          "Inclusion probabilities are unit-specific.",
          bullet = "info"
        )
      }
    } else if (has_fpc) {
      N <- object_for_alloc[[fpc_col]][1]
      if (!is_null(stage_unit_vars)) {
        n_sel <- nrow(dplyr::distinct(
          object_for_alloc, across(all_of(stage_unit_vars))
        ))
      } else {
        n_sel <- nrow(object_for_alloc)
      }
      f <- format(round(n_sel / N, 4), nsmall = 4)
      cli::cat_bullet(
        paste0("N = ", N, ", n = ", n_sel, ", f = ", f),
        bullet = "bullet"
      )
      if (has_unequal_prob) {
        cli::cat_bullet(
          "Inclusion probabilities are unit-specific.",
          bullet = "info"
        )
      }
    } else {
      cli::cat_bullet(
        "FPC information unavailable.",
        bullet = "warning"
      )
    }
  }

  if (".weight" %in% names(object) && nrow(object) == 0) {
    cat("\n")
    cli::cat_rule(left = "Weights")
    cli::cat_bullet(
      "No weight diagnostics for an empty sample.",
      bullet = "info"
    )
  } else if (".weight" %in% names(object)) {
    cat("\n")
    cli::cat_rule(left = "Weights")
    if (is_replicated) {
      cli::cat_bullet(
        "Weight diagnostics omitted for stacked replicated sample.",
        bullet = "info"
      )
      cli::cat_bullet(
        "Filter to one replicate: x |> filter(.replicate == 1)",
        bullet = "info"
      )
    } else {
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
  }

  cat("\n")
  invisible(object)
}

#' Digest-backed realization report for one stage
#'
#' Reports pools at full parent x strata resolution: pools sharing a
#' stratum label under different parents are never merged. Shows the
#' allocation quadruple where its parts differ (target and expected
#' columns appear only when informative), replicate variation as
#' ranges, certainty counts, and method diagnostics.
#' @noRd
summary_stage_realization <- function(st, stage_spec, design, digest,
                                      dpos, is_replicated) {
  pools <- st$pools
  fmt_f <- function(f) format(round(f, 4), nsmall = 4)

  # The realization report covers the executed pools; design-resolved
  # pools are universe context, summarized in one line.
  resolved <- pools$chance_status == "design_resolved"
  if (any(resolved)) {
    cli::cat_bullet(
      paste0(
        "Universe: ", nrow(pools), " pools, ",
        format(sum(pools$N), big.mark = ","),
        " units (", sum(resolved), " pools resolved from the design, ",
        "not reached by this realization)."
      ),
      bullet = "info"
    )
    pools <- pools[!resolved, , drop = FALSE]
    st$pools <- pools
  } else if (identical(st$scope, "eligible")) {
    cli::cat_bullet(
      "Eligible units under the realized parents.",
      bullet = "info"
    )
  } else if (!identical(st$scope, "universe")) {
    cli::cat_bullet(
      paste0("Pool scope: ", st$scope, "."),
      bullet = "info"
    )
  }

  is_wr <- identical(st$chance_kind, "expected_hits")
  random_size <- all(is.na(pools$n_target))

  # Replicate-varying realized sizes are shown as ranges per pool.
  realized_chr <- NULL
  varies <- FALSE
  if (anyNA(pools$n_realized)) {
    sel <- st$selected
    if (!is_null(sel) && "replicate" %in% names(sel)) {
      reps <- sort(unique(sel$replicate))
      counts <- table(
        factor(sel$pool_id, levels = pools$pool_id),
        factor(sel$replicate, levels = reps)
      )
      lo <- apply(counts, 1L, min)
      hi <- apply(counts, 1L, max)
      realized_chr <- ifelse(
        lo == hi, as.character(lo), paste0(lo, "-", hi)
      )
      varies <- TRUE
    } else {
      realized_chr <- rep("?", nrow(pools))
    }
  }
  if (varies) {
    cli::cat_bullet(
      "Realized sizes vary by replicate. Ranges shown.",
      bullet = "info"
    )
  } else if (is_replicated) {
    cli::cat_bullet("Allocation per replicate.", bullet = "info")
  }

  single <- nrow(pools) == 1L &&
    is_null(st$strata) &&
    all(is.na(pools$parent_unit))

  if (is_wr && single) {
    n_txt <- if (is_null(realized_chr)) {
      as.character(pools$n_realized)
    } else {
      realized_chr
    }
    cli::cat_bullet(
      paste0("n = ", n_txt, " (with replacement, no FPC)"),
      bullet = "bullet"
    )
  } else if (single) {
    if (varies) {
      cli::cat_bullet(
        paste0(
          "N = ", pools$N, ", n = ", realized_chr, " across replicates"
        ),
        bullet = "bullet"
      )
    } else {
      n <- pools$n_realized
      cli::cat_bullet(
        paste0(
          "N = ", pools$N, ", n = ", n, ", f = ", fmt_f(n / pools$N)
        ),
        bullet = "bullet"
      )
    }
  } else if (nrow(pools) > 15L) {
    rng <- function(v) {
      v <- v[!is.na(v)]
      if (length(v) == 0) {
        return("?")
      }
      if (min(v) == max(v)) {
        format(min(v))
      } else {
        paste0(format(min(v)), "-", format(max(v)))
      }
    }
    n_part <- if (varies) {
      "n_h varies by replicate"
    } else {
      paste0("n_h ", rng(pools$n_realized))
    }
    cli::cat_bullet(
      paste0(nrow(pools), " pools: N_h ", rng(pools$N), ", ", n_part),
      bullet = "bullet"
    )
    if (!is_wr && !varies && all(pools$N > 0)) {
      cli::cat_bullet(
        paste0("f_h ", rng(round(pools$n_realized / pools$N, 4))),
        bullet = "bullet"
      )
    }
  } else {
    tbl <- pool_alloc_table(
      st, design, digest, dpos,
      realized_chr = realized_chr, is_wr = is_wr,
      random_size = random_size, varies = varies
    )
    print_pool_table(tbl$display, tbl$total)
    if (is_wr) {
      cli::cat_bullet(
        "With replacement: n_h counts draws, no FPC.",
        bullet = "info"
      )
    }
  }

  if (single && random_size && !anyNA(pools$expected_n)) {
    cli::cat_bullet(
      paste0(
        "Random size: expected n = ", round(sum(pools$expected_n), 1),
        "."
      ),
      bullet = "info"
    )
  }

  if (!is_wr && !identical(st$storage, "constant")) {
    cli::cat_bullet(
      "Inclusion probabilities are unit-specific.",
      bullet = "info"
    )
  }

  if (
    identical(st$storage, "units") &&
      identical(st$chance_kind, "inclusion_probability") &&
      !is_null(st$selected)
  ) {
    sel_units <- unique(st$selected$unit_id)
    n_cert <- sum(
      st$units$is_certainty[match(sel_units, st$units$unit_id)],
      na.rm = TRUE
    )
    if (n_cert > 0) {
      cli::cat_bullet(
        paste0("Certainty selections (chance 1): ", n_cert),
        bullet = "bullet"
      )
    }
  }

  diags <- st$diagnostics
  if (!is_null(diags$balance)) {
    agg <- stats::aggregate(
      cbind(target, realized) ~ term, data = diags$balance, FUN = sum
    )
    for (i in seq_len(nrow(agg))) {
      resid <- agg$realized[i] - agg$target[i]
      cli::cat_bullet(
        paste0(
          "Balance on ", agg$term[i], ": target ",
          signif(agg$target[i], 6), ", HT estimate ",
          signif(agg$realized[i], 6),
          " (residual ", sprintf("%+.4g", resid), ")"
        ),
        bullet = "bullet"
      )
    }
  }
  if (!is_null(diags$bounds)) {
    ok <- sum(diags$bounds$satisfied)
    total <- nrow(diags$bounds)
    cli::cat_bullet(
      paste0("Count bounds satisfied: ", ok, "/", total, " levels."),
      bullet = if (ok == total) "bullet" else "warning"
    )
  }
  if (!is_null(diags$spatial)) {
    sp <- diags$spatial
    dup_note <- if (sp$n_duplicate_coordinates > 0) {
      paste0(
        " (", sp$n_duplicate_coordinates, " duplicate coordinates)"
      )
    } else {
      ""
    }
    cli::cat_bullet(
      paste0(
        "Spatial spread on ", paste(sp$variables, collapse = ", "),
        dup_note, "."
      ),
      bullet = "bullet"
    )
  }

  invisible(NULL)
}

#' Build the pool table (and totals) for the realization report
#' @noRd
pool_alloc_table <- function(st, design, digest, dpos, realized_chr,
                             is_wr, random_size, varies) {
  pools <- st$pools
  cols <- list()

  if (dpos > 1L && !all(is.na(pools$parent_unit))) {
    parent_col <- as.character(pools$parent_unit)
    prev <- digest$stages[[dpos - 1L]]
    if (!is_null(prev$selected) && "key" %in% names(prev$selected)) {
      map <- prev$selected[
        !duplicated(prev$selected$unit_id), , drop = FALSE
      ]
      labels <- gsub("\x1f", "/", map$key, fixed = TRUE)
      hit <- match(pools$parent_unit, map$unit_id)
      parent_col <- ifelse(is.na(hit), parent_col, labels[hit])
    }
    prev_spec <- design$stages[[prev$stage_id]]
    parent_name <- paste(
      prev_spec$clusters$vars %||% "parent", collapse = "/"
    )
    cols[[parent_name]] <- parent_col
  }
  for (v in st$strata %||% character(0)) {
    cols[[v]] <- as.character(pools[[v]])
  }

  cols[["N_h"]] <- as.character(pools$N)

  show_target <- !random_size &&
    any(
      !is.na(pools$n_target) &
        abs(pools$n_target - pools$n_realized) > 1e-9
    )
  if (show_target) {
    cols[["target"]] <- ifelse(
      is.na(pools$n_target), "-", format(pools$n_target, trim = TRUE)
    )
  }
  show_expected <- random_size ||
    any(
      abs(pools$expected_n - (pools$n_target %||% pools$expected_n)) >
        0.005,
      na.rm = TRUE
    )
  if (show_expected && !anyNA(pools$expected_n)) {
    cols[["expected"]] <- format(round(pools$expected_n, 2), trim = TRUE)
  }

  cols[["n_h"]] <- if (is_null(realized_chr)) {
    as.character(pools$n_realized)
  } else {
    realized_chr
  }

  show_f <- !is_wr && !varies && !anyNA(pools$n_realized) &&
    all(pools$N > 0)
  if (show_f) {
    cols[["f_h"]] <- format(
      round(pools$n_realized / pools$N, 4), nsmall = 4
    )
  }

  display <- as.data.frame(cols, check.names = FALSE)

  total <- NULL
  if (!varies) {
    total <- list()
    total[["N_h"]] <- as.character(sum(pools$N))
    if (show_target) {
      total[["target"]] <- if (anyNA(pools$n_target)) {
        "-"
      } else {
        format(sum(pools$n_target), trim = TRUE)
      }
    }
    if (show_expected && !anyNA(pools$expected_n)) {
      total[["expected"]] <- format(
        round(sum(pools$expected_n), 2), trim = TRUE
      )
    }
    total[["n_h"]] <- as.character(sum(pools$n_realized))
    if (show_f && sum(pools$N) > 0) {
      total[["f_h"]] <- format(
        round(sum(pools$n_realized) / sum(pools$N), 4), nsmall = 4
      )
    }
  }

  list(display = display, total = total)
}

#' Aligned pool table printer with an optional Total row
#' @noRd
print_pool_table <- function(display, total = NULL) {
  cols <- names(display)
  widths <- vapply(
    cols,
    function(nm) {
      vals <- c(
        display[[nm]],
        if (!is_null(total)) total[[nm]] %||% character(0),
        if (!is_null(total) && nm == cols[1]) "Total"
      )
      max(nchar(nm), nchar(vals), 0L)
    },
    integer(1)
  )
  line <- function(vals) {
    paste(
      mapply(
        function(v, w) formatC(v, width = w, flag = "-"),
        vals, widths
      ),
      collapse = "  "
    )
  }
  cat("  ", line(cols), "\n", sep = "")
  for (i in seq_len(nrow(display))) {
    cat(
      "  ",
      line(vapply(cols, function(nm) display[[nm]][i], character(1))),
      "\n",
      sep = ""
    )
  }
  if (!is_null(total)) {
    sep_parts <- mapply(
      function(nm, w) {
        if (!is_null(total[[nm]])) strrep("\u2500", w) else strrep(" ", w)
      },
      cols, widths
    )
    cat("  ", paste(sep_parts, collapse = "  "), "\n", sep = "")
    total_vals <- vapply(
      cols, function(nm) total[[nm]] %||% "", character(1)
    )
    total_vals[1] <- "Total"
    cat("  ", line(total_vals), "\n", sep = "")
  }
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
