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

#' @noRd
round_preserve_total <- function(x, n) {
  floored <- floor(x)
  remainders <- x - floored
  shortfall <- n - sum(floored)

  if (shortfall > 0) {
    add_indices <- order(remainders, decreasing = TRUE)[seq_len(shortfall)]
    floored[add_indices] <- floored[add_indices] + 1
  }
  as.integer(floored)
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

  # Initialize at floor, clamped to [lo, hi]
  a <- pmax(as.integer(floor(x)), lo)
  a <- pmin(a, hi)
  shortfall <- n - sum(a)

  # Distribute shortfall by priority: strata furthest below their ideal
  # target (largest x_h - a_h) receive +1 first. When the shortfall exceeds
  # the number of eligible strata (possible after aggressive clamping),
  # iterate until resolved. Feasibility (sum(lo) <= n <= sum(hi)) guarantees
  # termination.
  while (shortfall > 0L) {
    eligible <- which(a < hi)
    if (length(eligible) == 0L) break
    scores <- (x - a)[eligible]
    k <- min(shortfall, length(eligible))
    top <- eligible[order(scores, decreasing = TRUE)[seq_len(k)]]
    a[top] <- a[top] + 1L
    shortfall <- n - sum(a)
  }

  while (shortfall < 0L) {
    eligible <- which(a > lo)
    if (length(eligible) == 0L) break
    scores <- (a - x)[eligible]
    k <- min(-shortfall, length(eligible))
    top <- eligible[order(scores, decreasing = TRUE)[seq_len(k)]]
    a[top] <- a[top] - 1L
    shortfall <- n - sum(a)
  }

  a
}

#' @noRd
apply_bounds <- function(target, n_total, min_n, max_n, N_h) {
  H <- length(target)
  adjusted <- target

  effective_min <- if (!is_null(min_n)) pmin(rep(min_n, H), N_h) else rep(0, H)
  effective_max <- if (!is_null(max_n)) pmin(rep(max_n, H), N_h) else N_h

  if (sum(effective_min) > n_total) {
    cli_abort(c(
      "Cannot satisfy minimum sample size constraint",
      "x" = "Minimum allocation requires n >= {sum(effective_min)}",
      "i" = "Total sample size n = {n_total}"
    ), call = NULL)
  }

  if (sum(effective_max) < n_total) {
    cli_abort(c(
      "Cannot satisfy maximum sample size constraint",
      "x" = "Maximum allocation allows at most n = {sum(effective_max)}",
      "i" = "Total sample size n = {n_total}"
    ), call = NULL)
  }

  # Each iteration fixes at least one stratum at its bound or converges,
  # so H iterations suffices. Previous hardcoded cap of 50 could fail for
  # designs with many strata.
  max_iter <- H
  converged <- FALSE

  for (iter in seq_len(max_iter)) {
    changed <- FALSE

    below <- adjusted < effective_min
    if (any(below)) {
      borrowed <- sum(effective_min[below] - adjusted[below])
      adjusted[below] <- effective_min[below]

      can_give <- adjusted > effective_min & !below
      if (any(can_give) && borrowed > 0) {
        excess_above <- adjusted[can_give] - effective_min[can_give]
        total_excess <- sum(excess_above)
        if (total_excess > 0) {
          reduction <- pmin(
            borrowed * (excess_above / total_excess),
            excess_above
          )
          adjusted[can_give] <- adjusted[can_give] - reduction
        }
      }
      changed <- TRUE
    }

    above <- adjusted > effective_max
    if (any(above)) {
      excess <- sum(adjusted[above] - effective_max[above])
      adjusted[above] <- effective_max[above]

      can_receive <- adjusted < effective_max & !above
      if (any(can_receive) && excess > 0) {
        room <- effective_max[can_receive] - adjusted[can_receive]
        total_room <- sum(room)
        if (total_room > 0) {
          addition <- pmin(excess * (room / total_room), room)
          adjusted[can_receive] <- adjusted[can_receive] + addition
        }
      }
      changed <- TRUE
    }

    if (!changed) {
      converged <- TRUE
      break
    }
  }

  if (!converged) {
    final_sum <- sum(round(adjusted))
    cli_warn(c(
      "Bounds adjustment did not converge in {max_iter} iterations",
      "!" = "Stratum allocations may not sum exactly to {n_total}",
      "i" = "Current rounded sum: {final_sum}",
      "i" = "Consider relaxing {.arg min_n} or {.arg max_n} constraints"
    ))
  }

  adjusted <- pmin(adjusted, N_h)
  round_preserve_total_bounded(adjusted, n_total, effective_min, effective_max)
}

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
calculate_stratum_sizes <- function(stratum_info, strata_spec, draw_spec) {
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

  finalize_allocation <- function(target, n_total, N_h) {
    if (!is_null(min_n) || !is_null(max_n)) {
      apply_bounds(target, n_total, min_n, max_n, N_h)
    } else {
      round_preserve_total(target, n_total)
    }
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
    wor_methods <- c(
      "srswor",
      "systematic",
      "bernoulli",
      "pps_systematic",
      "pps_brewer",
      "pps_cps",
      "pps_poisson",
      "pps_sps",
      "pps_pareto"
    )
    if (draw_spec$method %in% wor_methods && any(frac_values > 1)) {
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
        target <- rep(n_total / H, H)
        validate_target(target, alloc)
        finalize_allocation(target, n_total, stratum_info$.N_h)
      },
      proportional = {
        target <- n_total * stratum_info$.N_h / N
        validate_target(target, alloc)
        finalize_allocation(target, n_total, stratum_info$.N_h)
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
        target <- n_total * stratum_info$.factor / total_factor
        validate_target(target, alloc)
        finalize_allocation(target, n_total, stratum_info$.N_h)
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
        target <- n_total * stratum_info$.factor / total_factor
        validate_target(target, alloc)
        finalize_allocation(target, n_total, stratum_info$.N_h)
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
        target <- n_total * stratum_info$.factor / total_factor
        validate_target(target, alloc)
        finalize_allocation(target, n_total, stratum_info$.N_h)
      }
    )
  }
  stratum_info
}
