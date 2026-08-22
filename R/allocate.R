#' @noRd
round_sample_size <- function(x, round_method = "up") {
  result <- switch(
    round_method,
    up = ceiling(x),
    down = floor(x),
    nearest = round(x)
  )
  pmax(as.integer(result), 1L)
}

#' Bounded largest-remainder rounding (Hare-Niemeyer with bounds)
#'
#' Rounds real-valued allocations to integers that sum to n while respecting
#' per-stratum bounds. Uses priority scoring: strata furthest below their
#' target get +1 first, minimizing total deviation from the ideal allocation.
#' Multiple passes handle cases where bounds clamping creates a shortfall
#' exceeding the number of eligible strata in a single round.
#'
#' @noRd
round_preserve_total_bounded <- function(x, n, min_vals, max_vals) {
  lo <- as.integer(ceiling(min_vals))
  hi <- as.integer(floor(max_vals))

  # Start from bounded floors.
  a <- pmax(as.integer(floor(x)), lo)
  a <- pmin(a, hi)
  shortfall <- n - sum(a)

  # Increment strata furthest below target.
  while (shortfall > 0L) {
    eligible <- which(a < hi)
    if (length(eligible) == 0L) break
    scores <- (x - a)[eligible]
    k <- min(shortfall, length(eligible))
    top <- eligible[order(scores, a[eligible], decreasing = TRUE, method = "radix")[seq_len(k)]]
    a[top] <- a[top] + 1L
    shortfall <- n - sum(a)
  }

  while (shortfall < 0L) {
    eligible <- which(a > lo)
    if (length(eligible) == 0L) break
    scores <- (a - x)[eligible]
    k <- min(-shortfall, length(eligible))
    top <- eligible[order(scores, a[eligible], decreasing = TRUE, method = "radix")[seq_len(k)]]
    a[top] <- a[top] - 1L
    shortfall <- n - sum(a)
  }

  a
}

#' Bounded allocation that preserves the method's own factors
#'
#' Solves `n_h = clamp(lambda * factor_h, lower_h, upper_h)` by bisection, then
#' integerizes while preserving the total.
#'
#' @noRd
allocate_bounded <- function(factors, total, lower, upper) {
  f <- factors
  f[!is.finite(f) | f < 0] <- 0

  # Scale factors to avoid overflow while preserving ratios.
  f_max <- max(f)
  if (f_max > 0) {
    f <- f / f_max
  }

  at_scale <- function(lambda, f, lo, hi) pmin(pmax(lambda * f, lo), hi)

  # Monotone bisection finds the scale reaching the total.
  solve_scale <- function(f, total, lo, hi) {
    if (sum(at_scale(0, f, lo, hi)) >= total) {
      return(0)
    }
    positive <- f > 0
    if (!any(positive)) {
      return(NA_real_)
    }
    # Bracket with saturation scales or repeated doubling.
    reach <- hi[positive] / f[positive]
    reach <- reach[is.finite(reach)]
    top <- if (length(reach) > 0) {
      max(reach)
    } else {
      grow <- total / sum(f[positive])
      if (!is.finite(grow) || grow <= 0) {
        grow <- 1
      }
      while (is.finite(grow * 2) && sum(at_scale(grow, f, lo, hi)) < total) {
        grow <- grow * 2
      }
      grow
    }
    if (sum(at_scale(top, f, lo, hi)) <= total) {
      return(top)
    }
    span <- c(0, top)
    for (step in seq_len(200L)) {
      mid <- sum(span) / 2
      if (sum(at_scale(mid, f, lo, hi)) < total) {
        span[1] <- mid
      } else {
        span[2] <- mid
      }
    }
    sum(span) / 2
  }

  lambda <- solve_scale(f, total, lower, upper)
  alloc <- if (is.na(lambda)) lower else at_scale(lambda, f, lower, upper)

  # Level remaining strata when positive factors saturate.
  if (total - sum(alloc) > sqrt(.Machine$double.eps)) {
    has_room <- upper > alloc
    if (any(has_room)) {
      equal <- as.numeric(has_room)
      level <- solve_scale(equal, total, alloc, upper)
      if (!is.na(level)) {
        alloc <- at_scale(level, equal, alloc, upper)
      }
    }
  }

  n_h <- round_preserve_total_bounded(alloc, total, lower, upper)

  lo_int <- as.integer(ceiling(lower))
  hi_int <- as.integer(floor(upper))
  if (
    abs(sum(n_h) - total) > 0.5 ||
      any(n_h < lo_int) ||
      any(n_h > hi_int)
  ) {
    cli_abort(
      c(
        "Internal error: bounded allocation did not satisfy its contract.",
        "i" = "Requested {total}, allocated {sum(n_h)}.",
        "i" = "Bounds [{min(lo_int)}, {max(hi_int)}], got
               [{min(n_h)}, {max(n_h)}]."
      ),
      call = NULL
    )
  }
  n_h
}

## Every class join_aux_to_strata() can assemble from class_prefix, spelled
## out so the taxonomy scan in test-error-taxonomy.R sees them. The scan
## collects character vectors from the namespace; a paste0() with a variable
## part is invisible to it in both directions.
join_aux_to_strata_classes <- c(
  "samplyr_error_alloc_missing_columns",
  "samplyr_error_alloc_missing_value_column",
  "samplyr_error_alloc_missing_key_values",
  "samplyr_error_alloc_duplicate_keys",
  "samplyr_error_alloc_ambiguous_matches",
  "samplyr_error_alloc_missing_coverage",
  "samplyr_error_aux_missing_columns",
  "samplyr_error_aux_missing_value_column",
  "samplyr_error_aux_missing_key_values",
  "samplyr_error_aux_duplicate_keys",
  "samplyr_error_aux_ambiguous_matches",
  "samplyr_error_aux_missing_coverage"
)

#' @noRd
join_aux_to_strata <- function(
  stratum_info,
  aux_df,
  key_vars,
  arg_name,
  value_col,
  coverage_msg = NULL,
  missing_label = value_col,
  call = rlang::caller_env()
) {
  class_prefix <- if (arg_name %in% c("n", "frac")) "alloc" else "aux"

  # Validate auxiliary tables restored from design files.
  if (!is.data.frame(aux_df)) {
    abort_samplyr(
      "{.arg {arg_name}} must be a data frame or a named numeric vector",
      class = "samplyr_error_aux_invalid_input_type",
      call = call
    )
  }

  missing_vars <- setdiff(key_vars, names(aux_df))
  if (length(missing_vars) > 0) {
    abort_samplyr(
      c(
        "{.arg {arg_name}} is missing stratification variable{?s}:",
        "x" = "{.val {missing_vars}}"
      ),
      class = paste0("samplyr_error_", class_prefix, "_missing_columns"),
      call = call
    )
  }

  if (!value_col %in% names(aux_df)) {
    abort_samplyr(
      "{.arg {arg_name}} must contain a {.val {value_col}} column",
      class = paste0("samplyr_error_", class_prefix, "_missing_value_column"),
      call = call
    )
  }

  key_df <- aux_df[, key_vars, drop = FALSE]
  if (anyNA(key_df)) {
    missing_key_cols <- key_vars[vapply(key_df, anyNA, logical(1))]
    abort_samplyr(
      c(
        "Data frame for {.arg {arg_name}} has missing values in stratification keys.",
        "x" = "Columns with missing values: {.val {missing_key_cols}}"
      ),
      class = paste0("samplyr_error_", class_prefix, "_missing_key_values"),
      call = call
    )
  }

  dup_keys <- find_duplicate_key_rows(aux_df, key_vars)
  if (nrow(dup_keys) > 0) {
    dup_labels <- format_key_labels(dup_keys, key_vars)
    dup_msg <- if (arg_name %in% c("n", "frac")) {
      "Data frame for {.arg {arg_name}} has duplicate rows for the same stratum."
    } else {
      "{.arg {arg_name}} has duplicate rows for the same stratum."
    }
    abort_samplyr(
      c(
        dup_msg,
        "x" = "Duplicate keys: {.val {dup_labels}}"
      ),
      class = paste0("samplyr_error_", class_prefix, "_duplicate_keys"),
      call = call
    )
  }

  stratum_keys <- make_group_key(stratum_info, key_vars)
  if (anyDuplicated(stratum_keys) > 0) {
    abort_samplyr(
      "{.arg {arg_name}} produced ambiguous many-to-many matches on strata keys.",
      class = paste0("samplyr_error_", class_prefix, "_ambiguous_matches"),
      call = call
    )
  }

  aux_keys <- make_group_key(aux_df, key_vars)
  match_idx <- match(stratum_keys, aux_keys)

  missing_mask <- is.na(match_idx)
  if (any(missing_mask)) {
    missing_keys <- unique(stratum_info[missing_mask, key_vars, drop = FALSE])
    missing_labels <- format_key_labels(missing_keys, key_vars)
    if (is_null(coverage_msg)) {
      coverage_msg <- paste0(
        "{.arg ",
        arg_name,
        "} does not cover all strata in the frame."
      )
    }
    abort_samplyr(
      c(
        coverage_msg,
        "x" = "Missing {missing_label} for: {.val {missing_labels}}"
      ),
      class = paste0("samplyr_error_", class_prefix, "_missing_coverage"),
      call = call
    )
  }

  joined <- stratum_info
  joined[[value_col]] <- aux_df[[value_col]][match_idx]

  joined
}

#' @noRd
calculate_stratum_sizes <- function(
  stratum_info,
  strata_spec,
  draw_spec,
  signal = FALSE
) {
  alloc <- strata_spec$alloc
  n_total <- draw_spec$n
  frac <- draw_spec$frac
  min_n <- draw_spec$min_n
  max_n <- draw_spec$max_n
  round_method <- draw_spec$round %||% "up"

  N <- sum(stratum_info$.N_h)
  H <- nrow(stratum_info)

  validate_target <- function(target, alloc_name, hint = NULL) {
    if (length(target) != H || any(!is.finite(target))) {
      abort_samplyr(
        c(
          "Could not compute finite stratum allocation targets for {.val {alloc_name}} allocation.",
          "x" = "Computed targets contain NA/NaN/Inf.",
          if (!is_null(hint)) c("i" = hint)
        ),
        class = "samplyr_error_alloc_target_non_finite"
      )
    }
  }

  # Retain unscaled factors for bounded redistribution.
  finalize_allocation <- function(factors, n_total, N_h, alloc_name) {
    validate_target(n_total * factors / sum(factors), alloc_name)

    # Bound WOR by population and WR by the integer target.
    wr <- is_multi_hit_method(draw_spec)
    # Random-size bounds cap the target, not its realization.
    random_size <- is_random_size_method(draw_spec)
    structural_max <- if (wr) rep(n_total, H) else N_h
    upper <- if (is_null(max_n)) {
      structural_max
    } else {
      pmin(rep(max_n, H), structural_max)
    }
    lower <- if (is_null(min_n)) rep(0, H) else pmin(rep(min_n, H), upper)

    if (sum(lower) > n_total) {
      abort_samplyr(
        c(
          "Cannot satisfy minimum sample size constraint",
          "x" = "Minimum allocation requires n >= {sum(lower)}",
          "i" = "Total sample size n = {n_total}"
        ),
        class = "samplyr_error_alloc_min_infeasible",
        call = NULL
      )
    }

    # `max_n` conflicts only when narrower than the population cap.
    max_narrows <- !is_null(max_n) && any(upper < structural_max)

    # This branch caps targets and does not itself declare a census.
    population_limited <- FALSE
    if (sum(upper) < n_total) {
      if (!wr && !max_narrows) {
        population_limited <- TRUE
        requested <- n_total
        available <- sum(upper)
        # Report all strata because each reaches its bound here.
        if (signal) {
          keys <- format_key_labels(
            stratum_info,
            strata_spec$vars,
            max_n = Inf
          )
          if (random_size) {
            signal_nominal_cap(
              pool_keys = keys,
              n_capped = H,
              n_pools = H,
              n_requested = requested,
              n_available = available
            )
          } else {
            signal_population_cap(
              pool_keys = keys,
              n_capped = H,
              n_pools = H,
              n_requested = requested,
              n_actual = available,
              n_available = sum(structural_max)
            )
          }
        }
        n_total <- sum(upper)
      } else {
        abort_samplyr(
          c(
            "Cannot satisfy maximum sample size constraint",
            "x" = "Maximum allocation allows at most n = {sum(upper)}",
            "i" = "Total sample size n = {n_total}"
          ),
          class = "samplyr_error_alloc_max_infeasible",
          call = NULL
        )
      }
    }

    n_h <- allocate_bounded(factors, n_total, lower, upper)

    # Report redistribution once when population bounds change the named rule.
    if (signal && !wr && !population_limited) {
      # Re-solve without population bounds to assess redistribution.
      relaxed_upper <- if (is_null(max_n)) {
        rep(n_total, H)
      } else {
        pmin(rep(max_n, H), n_total)
      }
      unbounded <- allocate_bounded(factors, n_total, lower, relaxed_upper)
      capped <- unbounded > n_h
      if (any(capped)) {
        moved <- sum(unbounded[capped] - n_h[capped])
        labels <- format_key_labels(
          stratum_info[capped, , drop = FALSE],
          strata_spec$vars,
          max_n = Inf
        )
        # Let `execute()` report redistribution once per stage.
        signal_selection_event(
          "allocation_cap",
          pool_keys = labels,
          n_capped = sum(capped),
          n_pools = H,
          n_moved = moved
        )
      }
    }

    n_h
  }

  strata_ids <- if (length(strata_spec$vars) == 1) {
    stratum_info[[strata_spec$vars]]
  } else {
    NULL
  }

  n_is_df <- is.data.frame(n_total)
  frac_is_df <- is.data.frame(frac)

  stratum_info$.n_h <- if (n_is_df) {
    stratum_info <- join_aux_to_strata(
      stratum_info = stratum_info,
      aux_df = n_total,
      key_vars = strata_spec$vars,
      arg_name = "n",
      value_col = "n",
      coverage_msg = "Custom {.arg n} data frame does not cover all strata in the frame.",
      missing_label = "allocation"
    )

    n_values <- stratum_info$n
    if (!is_finite_numeric(n_values)) {
      abort_samplyr(
        "{.arg n} values must be finite numbers (no NA/NaN/Inf)",
        class = "samplyr_error_alloc_n_non_finite"
      )
    }
    if (any(n_values <= 0)) {
      abort_samplyr(
        "{.arg n} values must be positive",
        class = "samplyr_error_alloc_n_bounds"
      )
    }
    if (!is_integerish_numeric(n_values)) {
      abort_samplyr(
        "{.arg n} values must be integer-valued",
        class = "samplyr_error_alloc_n_integer"
      )
    }

    as.integer(n_values)
  } else if (frac_is_df) {
    stratum_info <- join_aux_to_strata(
      stratum_info = stratum_info,
      aux_df = frac,
      key_vars = strata_spec$vars,
      arg_name = "frac",
      value_col = "frac",
      coverage_msg = "Custom {.arg frac} data frame does not cover all strata in the frame.",
      missing_label = "allocation"
    )

    frac_values <- stratum_info$frac
    if (!is_finite_numeric(frac_values)) {
      abort_samplyr(
        "{.arg frac} values must be finite numbers (no NA/NaN/Inf)",
        class = "samplyr_error_alloc_frac_non_finite"
      )
    }
    if (any(frac_values <= 0)) {
      abort_samplyr(
        "{.arg frac} values must be positive",
        class = "samplyr_error_alloc_frac_bounds"
      )
    }
    if (is_wor_method(draw_spec) && any(frac_values > 1)) {
      abort_samplyr(
        "{.arg frac} cannot exceed 1 for without-replacement methods",
        class = "samplyr_error_alloc_frac_wor_bounds"
      )
    }

    round_sample_size(stratum_info$.N_h * frac_values, round_method)
  } else if (is_null(alloc)) {
    if (!is_null(n_total)) {
      if (!is_null(names(n_total))) {
        if (is_null(strata_ids)) {
          cli_abort(c(
            "Named {.arg n} vectors are only supported for single stratification variables.",
            "i" = "Use a data frame for multi-variable stratification: {.val {strata_spec$vars}}"
          ), call = NULL)
        }
        matched <- n_total[as.character(strata_ids)]
        if (anyNA(matched)) {
          missing <- strata_ids[is.na(matched)]
          cli_abort(c(
            "Named {.arg n} does not cover all strata in the frame.",
            "x" = "Missing allocation for: {.val {missing}}"
          ), call = NULL)
        }
        as.integer(matched)
      } else {
        rep(n_total, H)
      }
    } else if (!is_null(frac)) {
      if (!is_null(names(frac))) {
        if (is_null(strata_ids)) {
          cli_abort(c(
            "Named {.arg frac} vectors are only supported for single stratification variables.",
            "i" = "Use a data frame for multi-variable stratification: {.val {strata_spec$vars}}"
          ), call = NULL)
        }
        frac_matched <- frac[as.character(strata_ids)]
        if (anyNA(frac_matched)) {
          missing <- strata_ids[is.na(frac_matched)]
          cli_abort(c(
            "Named {.arg frac} does not cover all strata in the frame.",
            "x" = "Missing allocation for: {.val {missing}}"
          ), call = NULL)
        }
        round_sample_size(stratum_info$.N_h * frac_matched, round_method)
      } else {
        round_sample_size(stratum_info$.N_h * frac, round_method)
      }
    } else {
      cli_abort("Cannot determine stratum sample sizes", call = NULL)
    }
  } else {
    switch(alloc,
      equal = {
        finalize_allocation(rep(1, H), n_total, stratum_info$.N_h, alloc)
      },
      proportional = {
        finalize_allocation(
          stratum_info$.N_h, n_total, stratum_info$.N_h, alloc
        )
      },
      power = {
        cv_df <- strata_spec$cv
        importance_df <- strata_spec$importance
        q <- strata_spec$power %||% 0.5

        if (!is.numeric(q) || length(q) != 1 || !is.finite(q) || q < 0 || q > 1) {
          abort_samplyr(
            "{.arg power} must be a single finite number between 0 and 1",
            class = "samplyr_error_alloc_power_bounds"
          )
        }

        stratum_info <- join_aux_to_strata(
          stratum_info = stratum_info,
          aux_df = cv_df,
          key_vars = strata_spec$vars,
          arg_name = "cv",
          value_col = "cv"
        )
        stratum_info <- join_aux_to_strata(
          stratum_info = stratum_info,
          aux_df = importance_df,
          key_vars = strata_spec$vars,
          arg_name = "importance",
          value_col = "importance"
        )

        if (any(stratum_info$cv <= 0)) {
          abort_samplyr(
            "{.arg cv} values must be positive",
            class = "samplyr_error_aux_cv_bounds"
          )
        }
        if (any(stratum_info$importance <= 0)) {
          abort_samplyr(
            "{.arg importance} values must be positive",
            class = "samplyr_error_aux_importance_bounds"
          )
        }

        stratum_info$.factor <- stratum_info$cv * (stratum_info$importance ^ q)
        total_factor <- sum(stratum_info$.factor)
        if (!is.finite(total_factor) || total_factor <= 0) {
          abort_samplyr(
            c(
              "Could not compute {.val power} allocation targets.",
              "x" = "sum(cv * importance^power) must be greater than 0.",
              "i" = "Ensure {.arg cv} and {.arg importance} are positive for at least one stratum."
            ),
            class = "samplyr_error_alloc_target_non_finite"
          )
        }
        finalize_allocation(
          stratum_info$.factor, n_total, stratum_info$.N_h, alloc
        )
      },
      neyman = {
        var_df <- strata_spec$variance
        stratum_info <- join_aux_to_strata(
          stratum_info = stratum_info,
          aux_df = var_df,
          key_vars = strata_spec$vars,
          arg_name = "variance",
          value_col = "var"
        )
        if (any(stratum_info$var < 0)) {
          abort_samplyr(
            "{.arg variance} values must be non-negative",
            class = "samplyr_error_aux_variance_bounds"
          )
        }

        stratum_info$.factor <- stratum_info$.N_h * sqrt(stratum_info$var)
        total_factor <- sum(stratum_info$.factor)
        if (!is.finite(total_factor) || total_factor <= 0) {
          abort_samplyr(
            c(
              "Could not compute {.val neyman} allocation targets.",
              "x" = "sum(N_h * sqrt(variance)) must be greater than 0.",
              "i" = "Ensure at least one stratum has positive {.arg variance}."
            ),
            class = "samplyr_error_alloc_target_non_finite"
          )
        }
        finalize_allocation(
          stratum_info$.factor, n_total, stratum_info$.N_h, alloc
        )
      },
      optimal = {
        var_df <- strata_spec$variance
        cost_df <- strata_spec$cost
        stratum_info <- join_aux_to_strata(
          stratum_info = stratum_info,
          aux_df = var_df,
          key_vars = strata_spec$vars,
          arg_name = "variance",
          value_col = "var"
        )
        stratum_info <- join_aux_to_strata(
          stratum_info = stratum_info,
          aux_df = cost_df,
          key_vars = strata_spec$vars,
          arg_name = "cost",
          value_col = "cost"
        )
        if (any(stratum_info$var < 0)) {
          abort_samplyr(
            "{.arg variance} values must be non-negative",
            class = "samplyr_error_aux_variance_bounds"
          )
        }
        if (any(stratum_info$cost <= 0)) {
          abort_samplyr(
            "{.arg cost} values must be positive",
            class = "samplyr_error_aux_cost_bounds"
          )
        }

        stratum_info$.factor <- stratum_info$.N_h *
          sqrt(stratum_info$var) /
          sqrt(stratum_info$cost)
        total_factor <- sum(stratum_info$.factor)
        if (!is.finite(total_factor) || total_factor <= 0) {
          abort_samplyr(
            c(
              "Could not compute {.val optimal} allocation targets.",
              "x" = "sum(N_h * sqrt(variance) / sqrt(cost)) must be greater than 0.",
              "i" = "Ensure at least one stratum has positive {.arg variance} and positive {.arg cost}."
            ),
            class = "samplyr_error_alloc_target_non_finite"
          )
        }
        finalize_allocation(
          stratum_info$.factor, n_total, stratum_info$.N_h, alloc
        )
      }
    )
  }
  stratum_info
}
