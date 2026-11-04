#' Summarize a tbl_sample
#'
#' Produces a compact summary of a sample: one section per executed
#' stage with a design line and a realization line, followed by weight
#' diagnostics.
#'
#' @param object A `tbl_sample` object produced by [execute()].
#' @param ... Must be empty.
#'
#' @return A `summary_tbl_sample` object, which prints the summary below.
#'   Its fields hold the figures the print shows: `n`, `stages`, `seed`,
#'   `weights` (mean, minimum, maximum and coefficient of variation of
#'   `.weight`), `design_effect` and `effective_n` (as [design_effect()] and
#'   [effective_n()] give them), `certainty`, the units taken with
#'   certainty at each executed stage, and `single_unit_pools`, the pools of
#'   one unit each stage took whole. Such a unit is certain too, as
#'   `.certainty_k` records, but it is counted only in `single_unit_pools`,
#'   so a stage of unsegmented EAs does not read as large units taken for
#'   their size. `periods` is `NULL` unless the sample is split into
#'   [periods()]. It then holds `k` and a `pools` table with each pool's
#'   fewest and most units per period (`min`, `max`). Weight figures are
#'   `NULL` for an
#'   empty sample and for stacked replicates, which the print also leaves
#'   out. `sample` is the summarized sample itself.
#'
#' @details
#' The header line shows the total sample size (with the universe size
#' when the frame digest records a complete denominator and the executed
#' path has no with-replacement stage), the stages executed, and the seed.
#' Under `frame_digest = "none"` the header shows the sample size alone,
#' since nothing records the size of the frame. When the later stages were
#' drawn one wave at a time, the header shows the seeds of the draws the
#' rows come from, such as `seeds = 101 to 112` for the completed master.
#' A wave shows the seed of the draw that took its units. The header shows
#' no seed when one of those draws ran without one.
#'
#' A sample split into [periods()] gets one more line, which says whether
#' each stratum has the same number of units in every period and names the
#' strata that do not, for example "Periods: 12, even in 10 of 13 strata."
#' followed by "Uneven: Cascades (2 to 3 per period), Centre-Nord (3 to
#' 4)." When every stratum is even, the line gives the range of units per
#' period instead. This is the table fieldwork is planned from, and an
#' uneven stratum shows an allocation that is not a multiple of the number
#' of periods. While the later stages are drawn one period at a time, a line
#' such as "Drawn: stage 2 in 3 of 12 periods (1, 2, 5)." says how far they
#' are.
#'
#' A materialized wave, period or window describes itself. A line such as
#' "Period: 2 of 4. It holds 26 of the master's 104 units." names it and
#' counts the units its panels were assigned to, each once however many of
#' the waves activate it. Its stage sections count the wave's own units,
#' with or without a frame digest, since the digest records the master's
#' draw. A stage drawn for the wave alone is read from the wave's part of
#' the digest. The master's own summary describes its draw.
#'
#' Each stage section has two lines:
#'
#' - the design: method (with with-replacement and
#'   approximate-probabilities qualifiers), measure of size, cluster
#'   and stratification variables, and balancing declarations.
#' - the realization: population and sample sizes with the sampling
#'   fraction. Stages with several selection pools report ranges
#'   (`N_h`, `n_h`, `f_h`) and, for later stages, how many of the
#'   universe pools the realization reached. `f_h` is a realized
#'   sampling fraction, not a unit-level inclusion probability for
#'   unequal-probability designs.
#'
#' Per-pool detail (one row per parent-by-stratum pool and realization)
#' lives in [frame_summary()] with `detail = "pool"`. The realization line uses
#' compact sampling notation: `N_h`, `n_h`, and `f_h` correspond to
#' that table's `N`, `n_realized`, and `take_rate` columns,
#' respectively, and `take_rate` is `n_realized / N`. Here `h` indexes
#' the displayed selection pools: strata at a first stratified stage
#' and parent-by-stratum pools at later stages. For a replicated
#' execution, the pool table has one row per `pool_id` and `replicate`.
#'
#' For a sample returned by [share_weights()], the rows are target units,
#' while the stage sections and the `certainty` and `single_unit_pools`
#' fields describe the source sample the weights were shared from. The
#' header line and the weight figures describe the target rows.
#'
#' **Weights** reports the mean and range, coefficient of variation,
#' Kish design effect, and effective sample size on one line. The
#' `Kish DEFF` token is the weighting loss alone, as computed by
#' [design_effect()] on the `.weight` column. Clustering and
#' stratification also move precision and are invisible in the weights,
#' so a design effect that reflects them needs [as_svydesign()] and
#' `survey::svymean(deff = TRUE)`.
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
#' @family diagnostics
#' @export
summary.tbl_sample <- function(object, ...) {
  rlang::check_dots_empty()
  design <- get_design(object)
  stages <- get_stages_executed(object)
  stage_sample <- summary_stage_sample(object)
  w <- object[[".weight"]]
  weighted <- !is_null(w) && length(w) > 0L &&
    !has_multiple_replicates(object)
  structure(
    list(
      sample = object,
      n = nrow(object),
      stages = stages,
      seed = summary_seeds(object),
      weights = if (weighted) {
        list(
          mean = mean(w),
          min = min(w),
          max = max(w),
          cv = if (length(w) > 1L) stats::sd(w) / mean(w) else 0
        )
      },
      design_effect = if (weighted) design_effect(weights = w),
      effective_n = if (weighted) effective_n(weights = w),
      certainty = stats::setNames(
        vapply(stages, function(k) {
          summary_certainty_count(stage_sample, design, k)[["certainty"]]
        }, integer(1)),
        paste0("stage_", stages)
      ),
      single_unit_pools = stats::setNames(
        vapply(stages, function(k) {
          summary_certainty_count(stage_sample, design, k)[["single_unit"]]
        }, integer(1)),
        paste0("stage_", stages)
      ),
      periods = summary_periods(object)
    ),
    class = "summary_tbl_sample"
  )
}

#' @rdname summary.tbl_sample
#' @param x A `summary_tbl_sample` object.
#' @export
print.summary_tbl_sample <- function(x, ...) {
  render_sample_summary(x$sample)
  invisible(x)
}

#' The seeds a summary shows
#'
#' Rows drawn one wave at a time show the seeds of the draws they come from,
#' and none when one of those draws had none. Other samples show their
#' seed.
#' @noRd
summary_seeds <- function(object) {
  attr(object, "metadata")$draw_seeds %||% attr(object, "seed")
}

#' Seeds as the header prints them, with a run of three or more as a range
#' @noRd
format_seeds <- function(seeds) {
  if (length(seeds) == 1L) {
    return(paste0("seed = ", seeds))
  }
  runs <- split(seeds, cumsum(c(1L, diff(seeds) != 1L)))
  parts <- vapply(runs, function(run) {
    if (length(run) >= 3L) {
      paste(run[[1L]], "to", run[[length(run)]])
    } else {
      paste(run, collapse = ", ")
    }
  }, character(1))
  paste0("seeds = ", paste(parts, collapse = ", "))
}

#' The printed summary, from the sample
#' @noRd
render_sample_summary <- function(object) {
  design <- get_design(object)
  stages_executed <- get_stages_executed(object)
  seed <- summary_seeds(object)

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
  if (length(seed) > 0L) {
    info_parts <- c(info_parts, format_seeds(seed))
  }
  if (is_replicated) {
    n_reps <- n_replicates(object)
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

  summary_periods_lines(summary_periods(object))
  summary_wave_line(object)
  progress <- wave_draws_progress(object)
  if (!is_null(progress)) {
    cli::cat_bullet(paste0("Drawn: ", progress, "."), bullet = "info")
  }

  # The recorded design describes source selection, not transformed rows.
  share <- attr(object, "metadata")$weight_share
  if (!is_null(share)) {
    summary_weight_share_note(object, share)
  }

  stage_sample <- summary_stage_sample(object)
  if (!is_null(share)) {
    digest <- get_frame_digest(stage_sample)
    if (!is_null(digest) && identical(digest$status, "invalidated")) {
      digest <- NULL
    }
  }
  # A wave's stages are counted on its rows where its digest describes the
  # master, which is every stage the master drew for all its panels.
  wave <- attr(stage_sample, "metadata")[["wave"]]
  is_wave <- !is_null(wave)
  if (is_wave && !is_null(digest)) {
    master_stages <- wave$master_stages %||% stages_executed
    digest$stages <- Filter(
      function(s) !s$stage_id %in% master_stages,
      digest$stages
    )
  }

  # The first replicate with rows. A stack of empty replicates has none.
  object_for_alloc <- if (is_replicated && nrow(stage_sample) > 0L) {
    stage_sample[stage_sample$.replicate == min(stage_sample$.replicate), ]
  } else {
    stage_sample
  }
  # A window repeats a unit once per wave it is active.
  if (is_wave && ".sample_id" %in% names(object_for_alloc)) {
    object_for_alloc <- object_for_alloc[
      !duplicated(object_for_alloc$.sample_id), ,
      drop = FALSE
    ]
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

    n_cert <- summary_certainty_count(object_for_alloc, design, stage_idx)
    dpos <- match(stage_idx, digest_stage_ids)
    if (!is.na(dpos)) {
      summary_stage_realization(
        digest$stages[[dpos]],
        is_replicated = is_replicated,
        random_size = is_random_size_method(stage_spec$draw_spec),
        n_cert = n_cert
      )
    } else {
      summary_stage_fallback(
        object_for_alloc, design, stage_spec, stage_idx,
        is_replicated = is_replicated,
        n_cert = n_cert
      )
    }
    summary_empty_parents(
      attr(stage_sample, "metadata")$empty_parents, stage_idx
    )
    summary_reserve_settlement(
      attr(object, "metadata")$reserve_settlement, stage_idx
    )
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
          "Filter to one first: x |> filter(.replicate == ",
          min(object$.replicate), ")"
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
            paste0("Kish DEFF ", round(design_effect(weights = w), 2)),
            paste0(
              "n_eff ",
              format(round(effective_n(weights = w)), big.mark = ",")
            )
          ),
          collapse = " | "
        ),
        bullet = "bullet"
      )
    }
  }

  cat("\n")
  invisible(NULL)
}

#' The settlement of a reserve stage, as ranges over its pools
#' @noRd
summary_reserve_settlement <- function(settlement, stage_idx) {
  if (is_null(settlement) || !identical(settlement$stage, stage_idx)) {
    return(invisible(NULL))
  }
  pools <- settlement$pools
  fmt_n <- function(v) format(v, big.mark = ",", trim = TRUE)
  counts <- c("n_listed", "n_attempted", "n_eligible", "n_unknown",
              "n_respondents")
  flags <- c("ran_out", "under_released", "over_released", "no_respondent")
  cli::cat_bullet(
    paste0(
      "Settled: ",
      paste(
        paste(counts, vapply(counts, function(v) {
          summary_range(pools[[v]], fmt_n)
        }, character(1))),
        collapse = ", "
      )
    ),
    bullet = "bullet"
  )
  cli::cat_bullet(
    paste0(
      paste(paste(flags, vapply(flags, function(v) {
        fmt_n(sum(pools[[v]]))
      }, character(1))), collapse = ", "),
      " of ", fmt_n(nrow(pools)), " pools, dropped ", fmt_n(sum(pools$dropped))
    ),
    bullet = "bullet"
  )
  # A pool that resolved no unit has an assumed eligible count.
  rule <- pools$eligible_rule
  if (!is_null(rule) && any(rule != "resolved")) {
    cli::cat_bullet(
      paste0(
        "eligible_rule strata ", fmt_n(sum(rule == "strata")),
        ", all ", fmt_n(sum(rule == "all")), " of ", fmt_n(nrow(pools)),
        " pools, an assumed eligible count"
      ),
      bullet = "bullet"
    )
  }
  rates <- reserve_realized_rates(pools)
  fmt_rate <- function(x) formatC(x, format = "f", digits = 3)
  cli::cat_bullet(
    paste0(
      "Realized eligibility_rate ", fmt_rate(rates[["eligibility_rate"]]),
      ", resp_rate ", fmt_rate(rates[["resp_rate"]]),
      ", n_respondents / n_attempted ", fmt_rate(rates[["completes"]]),
      ", sd between pools ", fmt_rate(rates[["completes_sd"]])
    ),
    bullet = "bullet"
  )
  invisible(NULL)
}

#' The realized rates of a settlement, as svyplan names them
#'
#' Pooled ratios of totals over the pools, which the stop rule leaves
#' consistent. Unknowns are allocated by the share of resolved units found
#' eligible (AAPOR's e), or counted as eligible when none resolved, as the
#' weights do. `resp_rate` is then RR4, and the two rates multiply to the
#' respondents per attempted unit.
#'
#' `completes_sd` is the spread of that share between pools, net of each
#' pool's sampling noise. A pool's share is (r - s) / (k - s), unbiased
#' under the stop rule, with variance estimated by p (1 - p) / (k - s - 1).
#' The between-pool variance is their spread minus the mean of those
#' variances, floored at 0. Pools with fewer than 2 units beyond the stop
#' are left out.
#' @noRd
reserve_realized_rates <- function(pools) {
  attempted <- sum(pools$n_attempted)
  resolved <- sum(pools$n_eligible) + sum(pools$n_ineligible)
  e <- if (resolved > 0) sum(pools$n_eligible) / resolved else 1
  eligible <- e * attempted
  respondents <- sum(pools$n_respondents)
  worked <- pools$n_attempted - pools$stopped
  ok <- worked >= 2L
  share <- (pools$n_respondents - pools$stopped)[ok] / worked[ok]
  noise <- share * (1 - share) / (worked[ok] - 1)
  completes_sd <- if (sum(ok) >= 2L) {
    sqrt(max(0, stats::var(share) - mean(noise)))
  } else {
    NA_real_
  }
  c(
    eligibility_rate = e,
    resp_rate = if (eligible > 0) respondents / eligible else NA_real_,
    completes = respondents / attempted,
    completes_sd = completes_sd
  )
}

#' Units per period in each pool of a periods sample
#'
#' `NULL` unless the sample is split into `periods()`, and for a period
#' materialized by `wave`, which holds one period only. A pool is a stratum
#' of the assignment stage, within its parent below the first stage.
#' @noRd
summary_periods <- function(object) {
  metadata <- attr(object, "metadata")
  record <- metadata$panel_assignment
  if (is_null(record) || !identical(record$certainty, "assigned") ||
      !is_null(metadata$wave) || !".panel" %in% names(object)) {
    return(NULL)
  }
  k <- as.integer(record$panels)
  key_vars <- as.character(record$key_vars)
  pool_vars <- as.character(record$pool_vars %||% character(0))
  units <- as.data.frame(object)
  units <- units[!duplicated(units[key_vars]), , drop = FALSE]
  pool <- if (length(pool_vars) == 0L) {
    rep("", nrow(units))
  } else {
    make_group_key(units, pool_vars)
  }
  first <- !duplicated(pool)
  label <- if (length(pool_vars) == 0L) {
    ""
  } else {
    apply(units[first, pool_vars, drop = FALSE], 1L, function(v) {
      paste(trimws(as.character(v)), collapse = " > ")
    })
  }
  counts <- lapply(split(units$.panel, factor(pool, unique(pool))),
                   function(p) tabulate(p, k))
  list(
    k = k,
    pools = data.frame(
      pool = unname(label),
      min = vapply(counts, min, integer(1)),
      max = vapply(counts, max, integer(1)),
      row.names = NULL
    ),
    unit = if (identical(as.integer(record$assignment_stage), 1L)) {
      "strata"
    } else {
      "pools"
    }
  )
}

#' The wave a materialized sample holds, and its share of the master
#'
#' The units are those the panels were assigned to, counted once however
#' many of the waves activate them.
#' @noRd
summary_wave_line <- function(object) {
  metadata <- attr(object, "metadata")
  head <- panel_header_line(object)
  if (is_null(metadata[["wave"]]) || is_null(head)) {
    return(invisible(NULL))
  }
  line <- paste0(names(head)[1], ": ", head[[1]], ".")
  record <- metadata$panel_assignment
  key_vars <- as.character(record$key_vars)
  if (length(key_vars) > 0L && all(key_vars %in% names(object))) {
    total <- sum(vapply(record$pools, function(p) length(p$keys), integer(1)))
    active <- nrow(vctrs::vec_unique(as.data.frame(object)[key_vars]))
    fmt_n <- function(v) format(v, big.mark = ",", trim = TRUE)
    line <- paste0(
      line, " It holds ", fmt_n(active), " of the master's ", fmt_n(total),
      " units."
    )
  }
  cli::cat_bullet(line, bullet = "info")
  invisible(NULL)
}

#' The summary lines of a periods sample
#'
#' Evenness first, since strata can hold different numbers of units per
#' period. The uneven pools are named, the first few of them.
#' @noRd
summary_periods_lines <- function(periods, shown = 5L) {
  if (is_null(periods)) {
    return(invisible(NULL))
  }
  pools <- periods$pools
  per_period <- function(lo, hi) {
    if (lo == hi) paste(lo) else paste(lo, "to", hi)
  }
  units_word <- function(lo, hi) if (hi == 1L) "unit" else "units"
  even <- pools$min == pools$max
  if (nrow(pools) == 1L) {
    cli::cat_bullet(
      paste0(
        "Periods: ", periods$k, ", ", per_period(pools$min, pools$max), " ",
        units_word(pools$min, pools$max), " per period."
      ),
      bullet = "info"
    )
    return(invisible(NULL))
  }
  if (all(even)) {
    lo <- min(pools$min)
    hi <- max(pools$max)
    cli::cat_bullet(
      paste0(
        "Periods: ", periods$k, ", even in all ", nrow(pools), " ",
        periods$unit, " (", per_period(lo, hi), " ", units_word(lo, hi),
        " per period)."
      ),
      bullet = "info"
    )
    return(invisible(NULL))
  }
  uneven <- pools[!even, , drop = FALSE]
  ranges <- paste0(
    uneven$pool, " (", uneven$min, " to ", uneven$max,
    c(" per period", rep("", nrow(uneven) - 1L)), ")"
  )
  if (length(ranges) > shown) {
    ranges <- c(ranges[seq_len(shown)],
                paste(length(ranges) - shown, "more"))
  }
  cli::cat_bullet(
    paste0(
      "Periods: ", periods$k, ", even in ", sum(even), " of ", nrow(pools),
      " ", periods$unit, "."
    ),
    bullet = "info"
  )
  cat("  Uneven: ", paste(ranges, collapse = ", "), ".\n", sep = "")
  invisible(NULL)
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
        "cluster ", paste(declared_cluster_vars(stage_spec$clusters), collapse = "/")
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
#' parents are never merged. Ranges span the full parent x strata
#' resolution, and the per-pool rows live in frame_summary(). Method
#' diagnostics (balance, bounds, spatial) follow as their own bullets.
#' @noRd
summary_stage_realization <- function(st, is_replicated, random_size = NULL,
                                      n_cert = no_certainty()) {
  pools <- st$pools
  fmt_f <- function(f) sprintf("%.4f", f)
  fmt_n <- function(v) format(v, big.mark = ",", trim = TRUE)

  # Count resolved universe pools separately from reached pools.
  resolved <- pools$chance_status == "design_resolved"
  n_universe <- nrow(pools)
  any_resolved <- any(resolved)
  pools <- pools[!resolved, , drop = FALSE]
  st$pools <- pools

  is_wr <- identical(st$chance_kind, "expected_hits")
  # Fall back only for older digests without design semantics.
  if (is_null(random_size)) {
    random_size <- all(is.na(pools$n_target))
  }

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
  # A reserve stage selects a list, its take followed by its reserve.
  n_reserve <- pools$n_reserve
  reserve_txt <- if (!is_null(n_reserve) && !anyNA(n_reserve)) {
    paste0("n_reserve ", summary_range(n_reserve, fmt_n))
  }
  n_txt <- summary_range(
    if (is_null(reserve_txt)) n_realized else n_realized - n_reserve,
    fmt_n
  )
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
      if (!is_null(reserve_txt)) {
        base <- paste0(base, ", ", reserve_txt)
      }
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
    # Later stages can split each parent into several pools.
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
    # A later tenure draws from the units its exclusion left, and its
    # weights count the whole listing.
    excluded <- pools$n_excluded
    listed <- if (!is_null(excluded) && !anyNA(excluded) &&
                    any(excluded > 0)) {
      pools$N + excluded
    }
    parts <- c(
      paste0(
        "N_h ", summary_range(pools$N, fmt_n),
        if (!is_null(listed)) {
          paste0(" of ", summary_range(listed, fmt_n), " listed")
        }
      ),
      paste0("n_h ", n_scoped_txt),
      reserve_txt
    )
    if (
      !is_wr && !varies && !anyNA(n_realized) &&
        !anyNA(pools$N) && all(pools$N > 0)
    ) {
      parts <- c(
        parts,
        paste0(
          "f_h ", summary_range(n_realized / pools$N, fmt_f),
          if (!is_null(listed)) {
            paste0(
              " of eligible, ", summary_range(n_realized / listed, fmt_f),
              " of listed"
            )
          }
        )
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

  suffix <- summary_certainty_text(n_cert)
  if (is_replicated && !varies) {
    suffix <- c(suffix, "per replicate")
  }
  # Without universe context, denominators cover reached pools only.
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

#' The sample the stage sections describe
#'
#' The rows of a shared-weight sample are targets, and its stages describe
#' the source selection, so their figures come from the source sample that
#' the weight-share record keeps.
#' @noRd
summary_stage_sample <- function(object) {
  attr(object, "metadata")$weight_share$source_sample %||% object
}

#' Realization line from .fpc columns when no digest is recorded
#'
#' f_h = n_h / N_h is the realized sample fraction, not a unit-level
#' inclusion probability for unequal-probability designs. WR/PMR
#' stages have no sampling fraction because their FPC is Inf.
#' @noRd
summary_stage_fallback <- function(object_for_alloc, design, stage_spec,
                                   stage_idx, is_replicated,
                                   n_cert = no_certainty()) {
  fpc_col <- paste0(".fpc_", stage_idx)
  if (!fpc_col %in% names(object_for_alloc)) {
    cli::cat_bullet("FPC information unavailable.", bullet = "warning")
    return(invisible(NULL))
  }

  stage_method <- stage_spec$draw_spec$method
  is_wr_stage <- stage_method %in% c(wr_methods, pmr_methods) ||
    identical(stage_spec$draw_spec$method_type, "wr")

  suffix <- paste(
    c("", summary_certainty_text(n_cert), if (is_replicated) "replicate 1"),
    collapse = " | "
  )

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
  } else {
    # A later stage draws within each unit its parent stages selected, once
    # per hit of a with-replacement parent, so a pool is a parent by stratum.
    prior <- seq_len(stage_idx - 1L)
    multi_hit <- vapply(prior, function(i) {
      is_multi_hit_method(design$stages[[i]]$draw_spec)
    }, logical(1))
    pool_vars <- unique(c(
      ancestor_vars,
      intersect(paste0(".draw_", prior[multi_hit]), names(object_for_alloc)),
      stage_strata_vars
    ))
    count_data <- object_for_alloc
    if (!is_null(stage_unit_vars)) {
      count_data <- count_data |>
        distinct(across(all_of(unique(c(pool_vars, stage_unit_vars)))),
                 .keep_all = TRUE)
    }
    fmt_f <- function(f) sprintf("%.4f", f)
    fmt_n <- function(v) format(v, big.mark = ",", trim = TRUE)
    # A reserve stage lists its take followed by its reserve.
    reserve_col <- paste0(".reserve_", stage_idx)
    in_reserve <- if (reserve_col %in% names(count_data)) {
      count_data[[reserve_col]] > 0L
    } else {
      rep(FALSE, nrow(count_data))
    }
    has_reserve <- reserve_col %in% names(count_data)

    if (length(pool_vars) == 0L) {
      N <- count_data[[fpc_col]][1]
      main <- paste0(
        "N = ", fmt_n(N), ", n = ", fmt_n(sum(!in_reserve)),
        if (has_reserve) paste0(", n_reserve ", fmt_n(sum(in_reserve))),
        ", f = ", fmt_f(nrow(count_data) / N)
      )
    } else {
      count_data$.in_reserve <- in_reserve
      alloc_tbl <- count_data |>
        group_by(across(all_of(pool_vars))) |>
        summarise(
          N_h = .data[[fpc_col]][1],
          n_list = n(),
          n_reserve = sum(.data$.in_reserve),
          .groups = "drop"
        )
      noun <- if (length(ancestor_vars) == 0L) "strata" else "pools"
      main <- paste0(
        fmt_n(nrow(alloc_tbl)), " ", noun, ": ",
        "N_h ", summary_range(alloc_tbl$N_h, fmt_n), ", ",
        "n_h ", summary_range(alloc_tbl$n_list - alloc_tbl$n_reserve, fmt_n),
        ", ",
        if (has_reserve) {
          paste0("n_reserve ", summary_range(alloc_tbl$n_reserve, fmt_n), ", ")
        },
        "f_h ", summary_range(alloc_tbl$n_list / alloc_tbl$N_h, fmt_f)
      )
    }
    cli::cat_bullet(paste0(main, suffix), bullet = "bullet")
  }

  invisible(NULL)
}

#' What a transformed sample has to say before its design is described
#'
#' The coverage counts are stated as facts rather than as a warning. Whether
#' an unreachable target cluster is a defect depends on whether this object
#' will stand alone or become one component of a multiple-frame design, so the
#' warning belongs at the analysis boundary and the summary reports.
#' @noRd
summary_weight_share_note <- function(object, share) {
  cov <- share$coverage
  mode_txt <- switch(
    share$within_mode,
    cluster = paste0("clustered on ", share$target_cluster),
    singleton = "one unit per cluster",
    extended = paste0("clusters eliminated over ", share$target_cluster)
  )
  # Keep `format_inline()` templates on one line.
  cli::cat_bullet(
    cli::format_inline(
      "Weights were shared from {nrow(share$source_sample)} sampled row{?s}; the stages below describe that selection, not these rows."
    ),
    bullet = "info"
  )
  cli::cat_bullet(
    cli::format_inline(
      "Links: {mode_txt} | {cov$n_reached_clusters} of {cov$n_target_clusters} target cluster{?s} reached"
    ),
    bullet = "info"
  )
  if (cov$n_unlinked_units > 0) {
    cli::cat_bullet(
      cli::format_inline(
        "{cov$n_unlinked_units} returned unit{?s} ha{?s/ve} no link of {?its/their} own and carr{?ies/y} the cluster's weight."
      ),
      bullet = "info"
    )
  }
  if (identical(cov$target_scope, "population")) {
    n_orphan <- length(cov$orphan_clusters)
    cli::cat_bullet(
      cli::format_inline(
        "{n_orphan} target cluster{?s} cannot be reached from this frame."
      ),
      bullet = if (n_orphan > 0) "warning" else "info"
    )
  } else {
    cli::cat_bullet(
      "Coverage of the target population is not established: `targets` is a roster of reached clusters.",
      bullet = "info"
    )
  }
  cat("\n")
}

#' Summarize a frame stack
#'
#' Reports the components, their membership columns and seeds, and how the
#' sampled rows fall across the domains: the frame sets units belong to. A
#' domain holding no rows is not shown, because absence from a sample is not
#' evidence that the domain is empty in the population.
#'
#' The counts are rows, not distinct units. A unit listed in two frames and
#' selected from both appears twice, once per frame that selected it, which is
#' exactly the double count a composite weight has to resolve.
#'
#' @param object A `frame_stack` produced by [stack_frames()].
#' @param ... Must be empty.
#'
#' @return A `summary_frame_stack` object, which prints the summary below.
#'   Its fields hold the figures the print shows: `n_frames`, `n_rows`, `key`
#'   (the key columns), `frames` (a tibble with one row per component, its
#'   `frame` name, `rows`, `membership` column and `seed`, `NA` when the
#'   component has none), `domains` (a tibble with the `rows` of each
#'   `domain` that holds some, in the printed order) and `coverage`. It is
#'   `NULL` unless a component shares weights, and otherwise holds the
#'   coverage `status` (`"known"`, `"incompatible"` or `"unknown"`) and the
#'   `orphan_clusters` that no frame reaches when it is known. `stack` is the
#'   summarized stack itself.
#'
#' @examples
#' population <- data.frame(
#'   person_id = 1:60,
#'   in_landline = rep(c(TRUE, FALSE), times = c(40, 20)),
#'   in_cell = rep(c(FALSE, TRUE), times = c(10, 50))
#' )
#'
#' frames <- stack_frames(
#'   landline = sampling_design() |>
#'     draw(n = 10) |>
#'     execute(population[population$in_landline, ], seed = 1),
#'   cell = sampling_design() |>
#'     draw(n = 12) |>
#'     execute(population[population$in_cell, ], seed = 2),
#'   membership = c(landline = "in_landline", cell = "in_cell"),
#'   key = person_id
#' )
#'
#' summary(frames)
#'
#' @family multiple frames
#' @export
summary.frame_stack <- function(object, ...) {
  rlang::check_dots_empty()
  membership <- attr(object, "membership")
  nms <- names(object)
  frames <- tibble::tibble(
    frame = nms,
    rows = vapply(object, nrow, integer(1), USE.NAMES = FALSE),
    membership = unname(as.character(membership[nms])),
    seed = vapply(object, function(component) {
      seed <- attr(component, "seed")
      if (is_null(seed)) NA_real_ else as.numeric(seed)
    }, numeric(1), USE.NAMES = FALSE)
  )
  counts <- frame_domain_counts(object)
  coverage <- stack_share_coverage(object)
  structure(
    list(
      stack = object,
      n_frames = length(object),
      n_rows = sum(frames$rows),
      key = attr(object, "key"),
      frames = frames,
      domains = tibble::tibble(
        domain = names(counts),
        rows = as.integer(counts)
      ),
      coverage = if (!identical(coverage$status, "not_applicable")) {
        list(status = coverage$status, orphan_clusters = coverage$clusters)
      }
    ),
    class = "summary_frame_stack"
  )
}

#' @rdname summary.frame_stack
#' @param x A `summary_frame_stack` object.
#' @export
print.summary_frame_stack <- function(x, ...) {
  cli::cat_rule("Frame Stack Summary")
  cat("\n")
  cli::cat_bullet(
    cli::format_inline(
      "{x$n_frames} frames | {x$n_rows} rows | key {.field {x$key}}"
    ),
    bullet = "info"
  )

  cat("\n")
  cli::cat_line(cli::style_bold("Frames"))
  frames <- x$frames
  for (i in seq_len(nrow(frames))) {
    seed <- frames$seed[[i]]
    cli::cat_bullet(
      cli::format_inline(paste0(
        frames$frame[[i]], ": {frames$rows[[i]]} rows | ",
        "{.field {frames$membership[[i]]}}",
        if (is.na(seed)) " | no seed" else " | seed {seed}"
      )),
      bullet = "bullet"
    )
  }

  cat("\n")
  cli::cat_line(cli::style_bold("Domains"))
  domains <- x$domains
  for (i in seq_len(nrow(domains))) {
    cli::cat_bullet(
      cli::format_inline("{domains$domain[[i]]}: {domains$rows[[i]]} rows"),
      bullet = "bullet"
    )
  }

  coverage <- x$coverage
  if (!is_null(coverage)) {
    cat("\n")
    cli::cat_line(cli::style_bold("Coverage"))
    n_orphan <- length(coverage$orphan_clusters)
    cli::cat_bullet(
      # Keep each formatted line unwrapped.
      cli::format_inline(switch(
        coverage$status,
        known = coverage_known_line,
        incompatible = coverage_incompatible_line,
        coverage_unknown_line
      )),
      bullet = if (identical(coverage$status, "known") && n_orphan > 0) {
        "warning"
      } else {
        "info"
      }
    )
  }

  cat("\n")
  cli::cat_bullet(
    "A unit listed in two frames is counted once per frame that selected it.",
    bullet = "info"
  )
  cat("\n")
  invisible(x)
}

#' @noRd
coverage_known_line <- "{n_orphan} target cluster{?s} cannot be reached from any frame."

#' @noRd
coverage_incompatible_line <- "The frames describe different target populations, so coverage over their union is not established."

#' @noRd
coverage_unknown_line <- "Coverage over the union of the frames is not established."

#' Name the selected parents a stage found empty
#'
#' Read from the metadata record rather than the digest, so it holds under
#' `frame_digest = "none"`. Pools are counted from what was sampled, which
#' an empty parent never is, so without this line the stage would show fewer
#' parents than the stage above selected. Replicated counts are per
#' replicate.
#' @noRd
summary_empty_parents <- function(records, stage_idx) {
  records <- Filter(function(r) identical(r$stage, stage_idx), records)
  if (length(records) == 0L) {
    return(invisible(NULL))
  }
  counts <- vapply(records, function(r) r$n, integer(1))
  replicated <- any(!is.na(vapply(records, function(r) r$replicate, integer(1))))
  text <- if (replicated) {
    paste0(
      summary_range(counts),
      " selected parents with nothing to sample, in each replicate that had any"
    )
  } else {
    paste0(
      sum(counts), " selected parent", if (sum(counts) == 1L) "" else "s",
      " with nothing to sample (on_empty)"
    )
  }
  cli::cat_bullet(text, bullet = "info")
  invisible(NULL)
}

#' Units a stage took with certainty, counted from the sample
#'
#' Read from `.certainty_k`, which every storage mode of the digest and no
#' digest at all leave on the sample. Counting from the digest's unit table
#' saw certainty only under `frame_digest = "full"`, so the default summary
#' hid 194 certainty selections. A clustered stage counts its units, keyed
#' by their ancestry. A stage with replacement has no certainty column.
#'
#' A certain unit whose pool holds only itself (`.fpc_k` of 1) is counted
#' apart, as `single_unit`, because a reader takes "certainty" to mean a
#' unit large enough to be taken for its size.
#' @noRd
summary_certainty_count <- function(sample, design, stage_idx) {
  col <- paste0(".certainty_", stage_idx)
  if (!col %in% names(sample) || nrow(sample) == 0L) {
    return(no_certainty())
  }
  certain <- sample[[col]] %in% TRUE
  fpc <- sample[[paste0(".fpc_", stage_idx)]]
  single <- if (is_null(fpc)) rep(FALSE, length(certain)) else fpc %in% 1
  spec <- design$stages[[stage_idx]]
  count <- if (is_null(spec$clusters)) {
    function(rows) sum(rows)
  } else {
    keys <- intersect(
      unique(c(
        collect_ancestor_cluster_vars(design, stage_idx), spec$clusters$vars
      )),
      names(sample)
    )
    df <- as.data.frame(sample)
    function(rows) nrow(vctrs::vec_unique(df[rows, keys, drop = FALSE]))
  }
  c(
    certainty = as.integer(count(certain & !single)),
    single_unit = as.integer(count(certain & single))
  )
}

#' @noRd
no_certainty <- function() {
  c(certainty = 0L, single_unit = 0L)
}

#' @noRd
summary_certainty_text <- function(n_cert) {
  n <- n_cert[["certainty"]]
  n_single <- n_cert[["single_unit"]]
  c(
    if (n > 0L) paste0(n, " certainty selection", if (n > 1L) "s"),
    if (n_single > 0L) {
      paste0(
        n_single, " pool", if (n_single > 1L) "s", " of one unit taken whole"
      )
    }
  )
}
