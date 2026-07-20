#' Summarize a tbl_sample
#'
#' Produces a compact summary of a sample: one section per executed
#' stage with a design line and a realization line, followed by weight
#' diagnostics.
#'
#' @param object A `tbl_sample` object produced by [execute()].
#' @param ... Must be empty.
#'
#' @return Invisibly returns `object`. Called for its side effect of
#'   printing a summary.
#'
#' @details
#' The header line shows the total sample size (with the universe size
#' when the frame digest records a complete denominator and the executed
#' path has no with-replacement stage), the stages executed, and the seed.
#'
#' Each stage section has two lines:
#'
#' - the design: method (with with-replacement and
#'   approximate-probabilities qualifiers), measure of size, cluster
#'   and stratification variables, and balancing declarations;
#' - the realization: population and sample sizes with the sampling
#'   fraction. Stages with several selection pools report ranges
#'   (`N_h`, `n_h`, `f_h`) and, for later stages, how many of the
#'   universe pools the realization reached. `f_h` is a realized
#'   sampling fraction, not a unit-level inclusion probability for
#'   unequal-probability designs.
#'
#' Per-pool detail (one row per parent-by-stratum pool) lives in
#' [frame_summary()] with `detail = "pool"`.
#'
#' **Weights** reports the mean and range, coefficient of variation,
#' Kish design effect, and effective sample size on one line.
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
#'   [frame_summary()] for per-pool allocation tables,
#'   [as_svydesign()] for exporting to the survey package
#'
#' @export
summary.tbl_sample <- function(object, ...) {
  rlang::check_dots_empty()
  design <- get_design(object)
  stages_executed <- get_stages_executed(object)
  seed <- attr(object, "seed")

  if (!is_null(design$title)) {
    cli::cat_rule(paste0("Sample Summary: ", design$title))
  } else {
    cli::cat_rule("Sample Summary")
  }

  is_replicated <- has_multiple_replicates(object)
  if (is_replicated && anyNA(object$.replicate)) {
    cli::cat_bullet(
      cli::format_inline(paste0(
        "{.field .replicate} contains missing values. ",
        "Replicate reporting disabled."
      )),
      bullet = "warning"
    )
    is_replicated <- FALSE
  }

  digest <- get_frame_digest(object)
  if (!is_null(digest) && identical(digest$status, "invalidated")) {
    digest <- NULL
  }

  cat("\n")
  n_total_stages <- length(design$stages)
  universe <- NA_real_
  if (
    !is_null(digest) &&
      length(digest$stages) >= length(stages_executed) &&
      !digest_path_has_expected_hits(digest, stages_executed)
  ) {
    universe <- digest_universe_units(digest)
  }
  n_txt <- paste0("n = ", format(nrow(object), big.mark = ","))
  if (!is.na(universe)) {
    n_txt <- paste0(n_txt, " of ", format(universe, big.mark = ","))
  }
  info_parts <- c(
    n_txt,
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

  object_for_alloc <- if (is_replicated) {
    object[object$.replicate == min(object$.replicate), ]
  } else {
    object
  }

  digest_stage_ids <- if (!is_null(digest)) {
    vapply(digest$stages, function(s) s$stage_id, integer(1))
  } else {
    integer(0)
  }

  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    label <- stage_spec$label
    rule_txt <- if (
      is_null(label) || identical(label, paste("Stage", stage_idx))
    ) {
      paste0("Stage ", stage_idx)
    } else {
      paste0("Stage ", stage_idx, ": ", label)
    }

    cat("\n")
    cli::cat_rule(left = rule_txt)
    cli::cat_bullet(summary_design_line(stage_spec), bullet = "bullet")

    dpos <- match(stage_idx, digest_stage_ids)
    if (!is.na(dpos)) {
      summary_stage_realization(
        digest$stages[[dpos]],
        is_replicated = is_replicated
      )
    } else {
      summary_stage_fallback(
        object_for_alloc, design, stage_spec, stage_idx,
        is_replicated = is_replicated
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
        paste0(
          "Weight diagnostics omitted for stacked replicates. ",
          "Filter to one first: x |> filter(.replicate == 1)"
        ),
        bullet = "info"
      )
    } else {
      w <- object$.weight
      cv_w <- if (length(w) > 1) stats::sd(w) / mean(w) else 0
      cli::cat_bullet(
        paste(
          c(
            paste0(
              "Mean ", round(mean(w), 2),
              " [", round(min(w), 2), ", ", round(max(w), 2), "]"
            ),
            paste0("CV ", round(cv_w, 2)),
            paste0("DEFF ", round(design_effect(w), 2)),
            paste0(
              "n_eff ",
              format(round(effective_n(w)), big.mark = ",")
            )
          ),
          collapse = " | "
        ),
        bullet = "bullet"
      )
    }
  }

  cat("\n")
  invisible(object)
}

#' One-line design description of a stage
#' @noRd
summary_design_line <- function(stage_spec) {
  draw <- stage_spec$draw_spec

  method <- draw$method
  replacement_txt <- if (method %in% pmr_methods) {
    " (minimum replacement)"
  } else if (is_multi_hit_method(draw)) {
    " (with replacement)"
  } else {
    ""
  }
  method_txt <- paste0(
    method,
    replacement_txt,
    if (identical(draw$method_probabilities, "approximate")) {
      " (approximate probabilities)"
    }
  )

  parts <- method_txt
  if (!is_null(draw$mos)) {
    parts <- c(parts, paste0("MOS ", draw$mos))
  }
  if (!is_null(stage_spec$clusters)) {
    parts <- c(
      parts,
      paste0(
        "cluster ", paste(stage_spec$clusters$vars, collapse = "/")
      )
    )
  }
  if (!is_null(stage_spec$strata)) {
    alloc_str <- if (!is_null(stage_spec$strata$alloc)) {
      paste0(" (", stage_spec$strata$alloc, ")")
    } else {
      ""
    }
    parts <- c(
      parts,
      paste0(
        "by ", paste(stage_spec$strata$vars, collapse = ", "), alloc_str
      )
    )
  }
  if (!is_null(draw$aux)) {
    parts <- c(
      parts,
      paste0("HT totals ", paste(draw$aux, collapse = ", "))
    )
  }
  if (!is_null(draw$bounds)) {
    parts <- c(
      parts,
      paste0("bounded counts ", paste(draw$bounds, collapse = ", "))
    )
  }
  if (!is_null(draw$spread)) {
    parts <- c(
      parts,
      paste0("spread ", paste(draw$spread, collapse = ", "))
    )
  }

  paste(parts, collapse = ", ")
}

#' Range formatter for realization lines: "4" or "2-12"
#' @noRd
summary_range <- function(v, fmt = function(x) format(x, trim = TRUE)) {
  v <- v[!is.na(v)]
  if (length(v) == 0) {
    return("?")
  }
  if (min(v) == max(v)) {
    fmt(min(v))
  } else {
    paste0(fmt(min(v)), "-", fmt(max(v)))
  }
}

#' Digest-backed realization line for one stage
#'
#' One bullet: pool counts (reached/universe for later stages), size
#' and fraction ranges, and short qualifiers (certainty count,
#' replicate scope). Pools sharing a stratum label under different
#' parents are never merged; ranges span the full parent x strata
#' resolution, and the per-pool rows live in frame_summary(). Method
#' diagnostics (balance, bounds, spatial) follow as their own bullets.
#' @noRd
summary_stage_realization <- function(st, is_replicated) {
  pools <- st$pools
  fmt_f <- function(f) sprintf("%.4f", f)
  fmt_n <- function(v) format(v, big.mark = ",", trim = TRUE)

  # Design-resolved pools are universe context the realization never
  # reached; the count contrast (reached/universe) carries that.
  resolved <- pools$chance_status == "design_resolved"
  n_universe <- nrow(pools)
  any_resolved <- any(resolved)
  pools <- pools[!resolved, , drop = FALSE]
  st$pools <- pools

  is_wr <- identical(st$chance_kind, "expected_hits")
  random_size <- all(is.na(pools$n_target))

  # Replicate-varying realized sizes collapse to a range.
  varies <- FALSE
  n_realized <- pools$n_realized
  if (anyNA(n_realized)) {
    sel <- st$selected
    if (!is_null(sel) && "replicate" %in% names(sel)) {
      reps <- sort(unique(sel$replicate))
      counts <- table(
        factor(sel$pool_id, levels = pools$pool_id),
        factor(sel$replicate, levels = reps)
      )
      n_realized <- as.integer(counts)
      varies <- TRUE
    }
  }
  n_txt <- summary_range(n_realized, fmt_n)
  n_scoped_txt <- paste0(
    n_txt, if (varies) " across replicates" else ""
  )

  single <- nrow(pools) == 1L &&
    is_null(st$strata) &&
    all(is.na(pools$parent_unit))

  main <- if (single) {
    if (is_wr) {
      paste0(
        "n = ", n_txt, " draws",
        if (varies) " across replicates" else "",
        " (no FPC)"
      )
    } else {
      base <- paste0("N = ", fmt_n(pools$N), ", n = ", n_scoped_txt)
      if (
        !varies && !anyNA(n_realized) && !is.na(pools$N) && pools$N > 0
      ) {
        base <- paste0(base, ", f = ", fmt_f(n_realized / pools$N))
      }
      if (random_size && !anyNA(pools$n_expected)) {
        base <- paste0(
          base,
          " (random size, n_expected ", round(sum(pools$n_expected), 1), ")"
        )
      }
      base
    }
  } else {
    # First-stage pools are the strata themselves; later-stage pools
    # split parents by strata, so "pools" is the accurate noun.
    noun <- if (!is_null(st$strata) && all(is.na(pools$parent_unit))) {
      "strata"
    } else {
      "pools"
    }
    count_txt <- if (any_resolved) {
      paste0(fmt_n(nrow(pools)), "/", fmt_n(n_universe), " ", noun)
    } else {
      paste0(fmt_n(nrow(pools)), " ", noun)
    }
    parts <- c(
      paste0("N_h ", summary_range(pools$N, fmt_n)),
      paste0("n_h ", n_scoped_txt)
    )
    if (
      !is_wr && !varies && !anyNA(n_realized) &&
        !anyNA(pools$N) && all(pools$N > 0)
    ) {
      parts <- c(
        parts,
        paste0("f_h ", summary_range(n_realized / pools$N, fmt_f))
      )
    }
    if (random_size && !anyNA(pools$n_expected)) {
      parts <- c(
        parts,
        paste0("n_expected ", round(sum(pools$n_expected), 1))
      )
    }
    base <- paste0(count_txt, ": ", paste(parts, collapse = ", "))
    if (is_wr) base <- paste0(base, " (draws, no FPC)")
    base
  }

  suffix <- character(0)
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
      suffix <- c(
        suffix,
        paste0(
          n_cert, " certainty selection", if (n_cert > 1) "s"
        )
      )
    }
  }
  if (is_replicated && !varies) {
    suffix <- c(suffix, "per replicate")
  }
  # Without design-resolved universe context the denominators cover
  # only what this realization reached; say so instead of implying
  # universe coverage.
  if (!any_resolved && identical(st$scope, "eligible")) {
    suffix <- c(suffix, "eligible units under realized parents")
  } else if (
    !any_resolved && !identical(st$scope, "universe") && !single
  ) {
    suffix <- c(suffix, paste0("scope ", st$scope))
  }

  cli::cat_bullet(
    paste(c(main, suffix), collapse = " | "),
    bullet = "bullet"
  )

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

#' Realization line from .fpc columns when no digest is recorded
#'
#' f_h = n_h / N_h is the realized sample fraction, not a unit-level
#' inclusion probability for unequal-probability designs. WR/PMR
#' stages have no sampling fraction because their FPC is Inf.
#' @noRd
summary_stage_fallback <- function(object_for_alloc, design, stage_spec,
                                   stage_idx, is_replicated) {
  fpc_col <- paste0(".fpc_", stage_idx)
  if (!fpc_col %in% names(object_for_alloc)) {
    cli::cat_bullet("FPC information unavailable.", bullet = "warning")
    return(invisible(NULL))
  }

  stage_method <- stage_spec$draw_spec$method
  is_wr_stage <- stage_method %in% c(wr_methods, pmr_methods) ||
    identical(stage_spec$draw_spec$method_type, "wr")

  suffix <- if (is_replicated) " | replicate 1" else ""

  # Full identity key for this stage's units
  ancestor_vars <- intersect(
    collect_ancestor_cluster_vars(design, stage_idx),
    names(object_for_alloc)
  )
  stage_strata_vars <- if (!is_null(stage_spec$strata)) {
    intersect(stage_spec$strata$vars, names(object_for_alloc))
  } else {
    character(0)
  }
  if (!is_null(stage_spec$clusters)) {
    stage_unit_vars <- unique(c(ancestor_vars, stage_spec$clusters$vars))
    stage_unit_vars <- intersect(stage_unit_vars, names(object_for_alloc))
  } else {
    stage_unit_vars <- NULL
  }

  if (is_wr_stage) {
    draw_col <- paste0(".draw_", stage_idx)
    if (draw_col %in% names(object_for_alloc)) {
      occurrence_vars <- unique(c(
        ancestor_vars, stage_strata_vars, draw_col
      ))
      n_selected <- nrow(dplyr::distinct(
        object_for_alloc, across(all_of(occurrence_vars))
      ))
      count_label <- "draws"
    } else if (!is_null(stage_unit_vars)) {
      n_selected <- nrow(dplyr::distinct(
        object_for_alloc, across(all_of(stage_unit_vars))
      ))
      count_label <- "selected clusters"
    } else {
      n_selected <- nrow(object_for_alloc)
      count_label <- "selected units"
    }
    cli::cat_bullet(
      paste0(
        "n = ", n_selected, " ", count_label, " (no FPC)", suffix
      ),
      bullet = "bullet"
    )
  } else if (!is_null(stage_spec$strata)) {
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
        .groups = "drop"
      )

    cli::cat_bullet(
      paste0(
        nrow(alloc_tbl), " strata: ",
        "N_h ", summary_range(alloc_tbl$N_h), ", ",
        "n_h ", summary_range(alloc_tbl$n_h), ", ",
        "f_h ", summary_range(round(alloc_tbl$n_h / alloc_tbl$N_h, 4)),
        suffix
      ),
      bullet = "bullet"
    )
  } else {
    N <- object_for_alloc[[fpc_col]][1]
    n_sel <- if (!is_null(stage_unit_vars)) {
      nrow(dplyr::distinct(
        object_for_alloc, across(all_of(stage_unit_vars))
      ))
    } else {
      nrow(object_for_alloc)
    }
    cli::cat_bullet(
      paste0(
        "N = ", N, ", n = ", n_sel,
        ", f = ", format(round(n_sel / N, 4), nsmall = 4),
        suffix
      ),
      bullet = "bullet"
    )
  }

  invisible(NULL)
}
