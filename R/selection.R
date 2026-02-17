#' @noRd
sample_clusters <- function(frame, strata_spec, cluster_spec, draw_spec) {
  cluster_vars <- cluster_spec$vars
  groups <- split_row_indices(frame, cluster_vars)
  cluster_indices <- groups$indices

  invariant_vars <- character(0)
  if (!is_null(draw_spec$mos)) {
    invariant_vars <- c(invariant_vars, draw_spec$mos)
  }
  if (!is_null(strata_spec)) {
    invariant_vars <- c(invariant_vars, strata_spec$vars)
  }
  invariant_vars <- intersect(invariant_vars, names(frame))

  if (length(invariant_vars) > 0) {
    check_vars <- c(cluster_vars, invariant_vars)
    n_clusters <- length(cluster_indices)
    n_combos <- frame |>
      distinct(across(all_of(check_vars))) |>
      nrow()

    if (n_combos != n_clusters) {
      varying <- invariant_vars[vapply(invariant_vars, function(v) {
        nrow(distinct(frame, across(all_of(c(cluster_vars, v))))) != n_clusters
      }, logical(1))]
      cli_abort(c(
        "{.val {varying}} must be constant within each cluster defined by {.val {cluster_vars}}.",
        "i" = "Found clusters where {.val {varying}} varies across rows.",
        "i" = "Ensure the frame has one consistent value per cluster for these columns."
      ))
    }
  }

  first_rows <- first_row_indices_by_group(cluster_indices)
  cluster_frame <- frame[first_rows, , drop = FALSE]
  sample_units(cluster_frame, strata_spec, draw_spec)
}


#' @noRd
sample_within_clusters <- function(
  frame,
  strata_spec,
  draw_spec,
  cluster_vars
) {
  indices_list <- split_row_indices(frame, cluster_vars)$indices

  if (
    is_null(strata_spec) &&
      is_simple_srswor_draw(draw_spec) &&
      !is_null(draw_spec$n) &&
      length(draw_spec$n) == 1 &&
      is_integerish_numeric(draw_spec$n)
  ) {
    draw_n <- as.integer(draw_spec$n)
    n_per_group <- rep.int(draw_n, length(indices_list))
    result <- sample_srswor_by_group_indices(frame, indices_list, n_per_group)
    if (nrow(result) > 0) {
      result$.sample_id <- seq_len(nrow(result))
    }
    return(result)
  }

  results_list <- lapply(indices_list, function(idxs) {
    data <- frame[idxs, , drop = FALSE]
    sample_units(data, strata_spec, draw_spec)
  })

  result <- bind_rows(results_list)
  if (nrow(result) > 0) {
    result$.sample_id <- seq_len(nrow(result))
  }
  result
}

#' @noRd
sample_units <- function(frame, strata_spec, draw_spec) {
  if (!is_null(strata_spec)) {
    sample_stratified(frame, strata_spec, draw_spec)
  } else {
    sample_unstratified(frame, draw_spec)
  }
}

#' @noRd
is_simple_srswor_draw <- function(draw_spec) {
  identical(draw_spec$method, "srswor") &&
    is_null(draw_spec$mos) &&
    is_null(draw_spec$control) &&
    is_null(draw_spec$certainty_size) &&
    is_null(draw_spec$certainty_prop)
}

#' @noRd
sample_srswor_by_group_indices <- function(frame, indices_list, n_per_group) {
  selected_rows <- vector("list", length(indices_list))
  pik_list <- vector("list", length(indices_list))
  fpc_list <- vector("list", length(indices_list))

  for (i in seq_along(indices_list)) {
    idxs <- indices_list[[i]]
    N_i <- length(idxs)
    n_i <- min(as.integer(n_per_group[[i]]), N_i)

    if (is.na(n_i) || n_i <= 0L || N_i == 0L) {
      next
    }

    local_idx <- sondage::equal_prob_wor(N_i, n_i)$sample
    selected_rows[[i]] <- idxs[local_idx]
    pik_list[[i]] <- rep.int(n_i / N_i, n_i)
    fpc_list[[i]] <- rep.int(N_i, n_i)
  }

  selected_rows <- unlist(selected_rows, use.names = FALSE)
  if (length(selected_rows) == 0L) {
    empty <- frame[0, , drop = FALSE]
    empty$.weight <- numeric(0)
    empty$.fpc <- numeric(0)
    return(empty)
  }

  result <- frame[selected_rows, , drop = FALSE]
  pik <- unlist(pik_list, use.names = FALSE)
  result$.weight <- 1 / pik
  result$.fpc <- unlist(fpc_list, use.names = FALSE)
  result
}

#' @noRd
sample_stratified <- function(frame, strata_spec, draw_spec) {
  strata_vars <- strata_spec$vars
  groups <- split_row_indices(frame, strata_vars)
  stratum_info <- stratum_info_from_groups(frame, strata_vars, groups$indices)

  stratum_info <- calculate_stratum_sizes(stratum_info, strata_spec, draw_spec)
  stratum_keys <- make_group_key(stratum_info, strata_vars)
  n_lookup <- setNames(stratum_info$.n_h, stratum_keys)
  draw_lookup <- prepare_stratum_draw_lookup(draw_spec, strata_vars)

  if (!draw_spec$method %in% multi_hit_methods) {
    capped <- stratum_info$.n_h > stratum_info$.N_h
    if (any(capped)) {
      n_requested <- sum(stratum_info$.n_h)
      n_actual <- sum(pmin(stratum_info$.n_h, stratum_info$.N_h))
      capped_keys <- stratum_keys[capped]
      cli_warn(c(
        "Sample size capped to population in {length(capped_keys)} stratum/strata: {.val {capped_keys}}.",
        "i" = "Requested total: {n_requested}. Actual total: {n_actual}."
      ))
    }
  }

  if (is_simple_srswor_draw(draw_spec)) {
    n_per_group <- vapply(
      groups$keys,
      function(stratum_key) {
        n_h <- n_lookup[[stratum_key]]
        if (is_null(n_h) || length(n_h) == 0 || is.na(n_h)) {
          cli_abort("Could not determine sample size for stratum {.val {stratum_key}}")
        }
        as.integer(n_h)
      },
      integer(1)
    )

    result <- sample_srswor_by_group_indices(frame, groups$indices, n_per_group)
    result$.sample_id <- seq_len(nrow(result))
    return(result)
  }

  results_list <- lapply(seq_along(groups$indices), function(i) {
    idxs <- groups$indices[[i]]
    stratum_key <- groups$keys[[i]]
    data <- frame[idxs, , drop = FALSE]

    n_h <- n_lookup[[stratum_key]]
    if (is_null(n_h) || length(n_h) == 0 || is.na(n_h)) {
      cli_abort("Could not determine sample size for stratum {.val {stratum_key}}")
    }

    N_h <- nrow(data)
    keys <- data[1, strata_vars, drop = FALSE]
    stratum_draw_spec <- resolve_stratum_draw_spec(
      draw_spec = draw_spec,
      keys = keys,
      strata_vars = strata_vars,
      stratum_key = stratum_key,
      lookup = draw_lookup
    )

    selected <- withCallingHandlers(
      draw_sample(data, n_h, stratum_draw_spec),
      error = function(e) {
        cli_abort(
          c(conditionMessage(e), "i" = "In stratum {.val {stratum_key}}"),
          call = NULL
        )
      }
    )
    selected$.weight <- 1 / selected$.pik
    selected$.pik <- NULL
    selected$.fpc <- rep.int(N_h, nrow(selected))
    selected
  })

  result <- bind_rows(results_list)

  result$.sample_id <- seq_len(nrow(result))
  result
}

#' @noRd
stratum_info_from_groups <- function(frame, strata_vars, group_indices) {
  if (length(group_indices) == 0L) {
    out <- frame[0, strata_vars, drop = FALSE]
    out$.N_h <- integer(0)
    return(out)
  }

  first_rows <- first_row_indices_by_group(group_indices)
  out <- frame[first_rows, strata_vars, drop = FALSE]
  out$.N_h <- lengths(group_indices)
  out
}

#' @noRd
prepare_stratum_draw_lookup <- function(draw_spec, strata_vars) {
  lookup <- list(
    frac = NULL,
    certainty_size = NULL,
    certainty_prop = NULL
  )

  if (is.data.frame(draw_spec$frac)) {
    frac_key <- make_group_key(draw_spec$frac, strata_vars)
    lookup$frac <- setNames(draw_spec$frac$frac, frac_key)
  }

  if (is.data.frame(draw_spec$certainty_size)) {
    cert_size_key <- make_group_key(draw_spec$certainty_size, strata_vars)
    lookup$certainty_size <- setNames(
      draw_spec$certainty_size$certainty_size,
      cert_size_key
    )
  }

  if (is.data.frame(draw_spec$certainty_prop)) {
    cert_prop_key <- make_group_key(draw_spec$certainty_prop, strata_vars)
    lookup$certainty_prop <- setNames(
      draw_spec$certainty_prop$certainty_prop,
      cert_prop_key
    )
  }

  lookup
}

#' @noRd
lookup_by_key <- function(lookup_vec, key) {
  if (is_null(lookup_vec) || is_null(key) || length(key) == 0) {
    return(NULL)
  }
  val <- unname(lookup_vec[key])
  if (length(val) == 0 || is.na(val[1])) {
    return(NULL)
  }
  val[1]
}

#' @noRd
resolve_stratum_draw_spec <- function(
  draw_spec,
  keys,
  strata_vars,
  stratum_key = NULL,
  lookup = NULL
) {
  stratum_draw_spec <- draw_spec

  if (is.data.frame(draw_spec$frac)) {
    frac_lookup <- if (is_null(lookup)) NULL else lookup$frac
    frac_val <- lookup_by_key(frac_lookup, stratum_key)
    if (!is_null(frac_val)) {
      stratum_draw_spec$frac <- unname(frac_val)
    }
  } else if (!is_null(draw_spec$frac) && length(draw_spec$frac) > 1) {
    stratum_id <- as.character(keys[[1]][1])
    if (stratum_id %in% names(draw_spec$frac)) {
      stratum_draw_spec$frac <- draw_spec$frac[[stratum_id]]
    }
  }

  if (is.data.frame(draw_spec$certainty_size)) {
    cert_size_lookup <- if (is_null(lookup)) NULL else lookup$certainty_size
    cert_size_val <- lookup_by_key(cert_size_lookup, stratum_key)
    if (!is_null(cert_size_val)) {
      stratum_draw_spec$certainty_size <- unname(cert_size_val)
    } else {
      stratum_draw_spec$certainty_size <- NULL
    }
  }

  if (is.data.frame(draw_spec$certainty_prop)) {
    cert_prop_lookup <- if (is_null(lookup)) NULL else lookup$certainty_prop
    cert_prop_val <- lookup_by_key(cert_prop_lookup, stratum_key)
    if (!is_null(cert_prop_val)) {
      stratum_draw_spec$certainty_prop <- unname(cert_prop_val)
    } else {
      stratum_draw_spec$certainty_prop <- NULL
    }
  }

  stratum_draw_spec
}

#' @noRd
sample_unstratified <- function(frame, draw_spec) {
  N <- nrow(frame)
  round_method <- draw_spec$round %||% "up"

  n <- if (!is_null(draw_spec$n)) {
    draw_spec$n
  } else if (!is_null(draw_spec$frac)) {
    round_sample_size(N * draw_spec$frac, round_method)
  } else {
    cli_abort("Cannot determine sample size")
  }

  if (!draw_spec$method %in% multi_hit_methods && n > N) {
    cli_warn(c(
      "Requested sample size ({n}) exceeds population size ({N}).",
      "i" = "Capped at {N} (census)."
    ))
  }

  result <- draw_sample(frame, n, draw_spec)
  result$.weight <- 1 / result$.pik
  result$.pik <- NULL
  result$.fpc <- N
  result$.sample_id <- seq_len(nrow(result))
  result
}

#' Handle zero-selection fallback for random-size methods
#'
#' When a random-size method produces zero selections, either abort or
#' fall back to SRS of 1 unit.  The fallback weight is 1/(1/N) = N,
#' reflecting the actual SRS(1, N) selection mechanism — **not** the
#' intended Bernoulli/Poisson inclusion probability.
#' @noRd
handle_empty_selection <- function(method_label, on_empty, N) {
  header <- "{method_label} sampling produced zero selections."
  suggestions <- c(
    "i" = "Increase {.arg frac}, use a fixed-size method, or set {.code on_empty = \"warn\"} to fall back to SRS of 1 unit."
  )
  switch(on_empty,
    error = cli_abort(c(header, suggestions)),
    warn = cli_warn(c(
      header,
      "!" = "Falling back to SRS of 1 unit (weight = {N}).",
      "i" = "The fallback weight reflects SRS, not the intended {method_label} design.",
      "i" = "Set {.code on_empty = \"error\"} to catch this, or {.code on_empty = \"silent\"} to suppress."
    )),
    silent = NULL
  )
  list(
    idx = sondage::equal_prob_wor(N, 1L)$sample,
    pik = rep(1 / N, N)
  )
}

#' @noRd
draw_sample <- function(data, n, draw_spec) {
  method <- draw_spec$method
  mos <- draw_spec$mos
  N <- nrow(data)

  if (!is_null(draw_spec$control)) {
    data <- arrange(data, !!!draw_spec$control)
  }

  if (!method %in% multi_hit_methods) {
    n <- min(n, N)
  }

  has_certainty <- !is_null(draw_spec$certainty_size) ||
    !is_null(draw_spec$certainty_prop)

  if (method %in% pps_methods) {
    mos_check <- data[[mos]]
    if (sum(mos_check) <= 0) {
      cli_abort(c(
        "Cannot use PPS sampling: sum of MOS variable {.var {mos}} is zero.",
        "i" = "At least one unit must have a positive measure of size."
      ))
    }
  }

  if (method %in% pps_methods && has_certainty) {
    return(draw_sample_pps_certainty(data, n, draw_spec))
  }

  pik <- NULL

  switch(method,
    srswor = {
      idx <- sondage::equal_prob_wor(N, n)$sample
      pik <- rep(n / N, N)
    },
    srswr = {
      idx <- sondage::equal_prob_wr(N, n)$sample
      pik <- rep(n / N, N)
    },
    systematic = {
      idx <- sondage::equal_prob_wor(N, n, method = "systematic")$sample
      pik <- rep(n / N, N)
    },
    bernoulli = {
      frac <- draw_spec$frac
      if (is_null(frac) || length(frac) == 0) {
        cli_abort("Bernoulli sampling requires {.arg frac} but none was provided")
      }
      pik <- rep(frac, N)
      prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
      idx <- sondage::equal_prob_wor(N, frac * N, method = "bernoulli", prn = prn_vals)$sample
      if (length(idx) == 0) {
        on_empty <- draw_spec$on_empty %||% "error"
        fallback <- handle_empty_selection("Bernoulli", on_empty, N)
        idx <- fallback$idx
        pik <- fallback$pik
      }
    },
    pps_poisson = {
      mos_vals <- data[[mos]]
      frac <- draw_spec$frac %||% (n / N)
      pik <- frac * mos_vals / sum(mos_vals) * N
      pik <- pmin(pik, 1)
      prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
      idx <- sondage::unequal_prob_wor(pik, method = "poisson", prn = prn_vals)$sample
      if (length(idx) == 0) {
        on_empty <- draw_spec$on_empty %||% "error"
        fallback <- handle_empty_selection("PPS Poisson", on_empty, N)
        idx <- fallback$idx
        pik <- fallback$pik
      }
    },
    pps_brewer = ,
    pps_systematic = ,
    pps_cps = ,
    pps_sps = ,
    pps_pareto = {
      mos_vals <- data[[mos]]
      pik <- sondage::inclusion_prob(mos_vals, n)
      prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
      idx <- sondage::unequal_prob_wor(pik, method = sondage_method_name(method), prn = prn_vals)$sample
    },
    pps_multinomial = ,
    pps_chromy = {
      mos_vals <- data[[mos]]
      pik <- sondage::expected_hits(mos_vals, n)
      idx <- sondage::unequal_prob_wr(pik, method = sondage_method_name(method))$sample
    },
    cli_abort("Unknown sampling method: {.val {method}}")
  )

  if (method %in% multi_hit_methods) {
    result <- data[idx, , drop = FALSE]
    result$.pik <- pik[idx]
    result$.draw <- seq_along(idx)
  } else {
    result <- data[idx, , drop = FALSE]
    result$.pik <- pik[idx]
  }

  if (method %in% pps_methods) {
    result$.certainty <- FALSE
  }

  result
}

#' @noRd
draw_sample_pps_certainty <- function(data, n, draw_spec) {
  method <- draw_spec$method
  mos <- draw_spec$mos
  mos_vals <- data[[mos]]
  N <- nrow(data)

  cert <- identify_certainty(
    mos_vals = mos_vals,
    n = n,
    certainty_size = draw_spec$certainty_size,
    certainty_prop = draw_spec$certainty_prop
  )

  if (cert$n_remaining < 0 && draw_spec$certainty_overflow == "error") {
    threshold_msg <- if (!is_null(draw_spec$certainty_prop)) {
      c(
        "i" = "With {.arg certainty_prop}, units are selected iteratively until",
        " " = "no remaining unit's proportion exceeds the threshold.",
        "i" = "This can cascade when removing large units pushes others above threshold."
      )
    } else {
      NULL
    }

    cli_abort(
      c(
        "Certainty selection exceeds target sample size.",
        "x" = "Found {cert$n_certain} certainty unit{?s}, but {.arg n} = {n}.",
        threshold_msg,
        "i" = "Options: increase {.arg n}, raise the threshold, or use {.arg certainty_overflow = \"allow\"} to keep all certainty units."
      )
    )
  }

  certainty_result <- NULL
  if (cert$n_certain > 0) {
    certainty_result <- data[cert$certainty_idx, , drop = FALSE]
    certainty_result$.pik <- rep(1, cert$n_certain)
    certainty_result$.certainty <- TRUE
  }

  prob_result <- NULL
  if (cert$n_remaining > 0 && length(cert$remaining_idx) > 0) {
    remaining_data <- data[cert$remaining_idx, , drop = FALSE]
    remaining_mos <- mos_vals[cert$remaining_idx]
    n_prob <- min(cert$n_remaining, length(cert$remaining_idx))

    prob_result <- draw_pps_method(
      data = remaining_data,
      n = n_prob,
      method = method,
      mos_vals = remaining_mos,
      draw_spec = draw_spec
    )
    prob_result$.certainty <- FALSE
  }

  if (is_null(certainty_result) && is_null(prob_result)) {
    result <- data[integer(0), , drop = FALSE]
    result$.pik <- numeric(0)
    result$.certainty <- logical(0)
    return(result)
  }

  result <- dplyr::bind_rows(certainty_result, prob_result)
  result
}

#' @noRd
draw_pps_method <- function(data, n, method, mos_vals, draw_spec = NULL) {
  N <- nrow(data)

  if (sum(mos_vals) <= 0) {
    cli_abort(c(
      "Cannot use PPS sampling: sum of MOS values is zero.",
      "i" = "At least one remaining unit must have a positive measure of size.",
      "i" = "This can happen when certainty selection removes all units with positive MOS."
    ))
  }

  switch(method,
    pps_poisson = {
      frac <- draw_spec$frac %||% (n / N)
      pik <- frac * mos_vals / sum(mos_vals) * N
      pik <- pmin(pik, 1)
      prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
      idx <- sondage::unequal_prob_wor(pik, method = "poisson", prn = prn_vals)$sample
      if (length(idx) == 0) {
        on_empty <- draw_spec$on_empty %||% "error"
        fallback <- handle_empty_selection("PPS Poisson", on_empty, N)
        idx <- fallback$idx
        pik <- fallback$pik
      }
    },
    pps_brewer = ,
    pps_systematic = ,
    pps_cps = ,
    pps_sps = ,
    pps_pareto = {
      pik <- sondage::inclusion_prob(mos_vals, n)
      prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
      idx <- sondage::unequal_prob_wor(pik, method = sondage_method_name(method), prn = prn_vals)$sample
    },
    pps_multinomial = ,
    pps_chromy = {
      pik <- sondage::expected_hits(mos_vals, n)
      idx <- sondage::unequal_prob_wr(pik, method = sondage_method_name(method))$sample
    }
  )

  if (method %in% multi_hit_methods) {
    result <- data[idx, , drop = FALSE]
    result$.pik <- pik[idx]
    result$.draw <- seq_along(idx)
  } else {
    result <- data[idx, , drop = FALSE]
    result$.pik <- pik[idx]
  }
  result
}

#' @noRd
identify_certainty <- function(
  mos_vals,
  n,
  certainty_size = NULL,
  certainty_prop = NULL
) {
  N <- length(mos_vals)
  certainty_idx <- integer(0)

  if (!is_null(certainty_size)) {
    certainty_idx <- which(mos_vals >= certainty_size)
  } else if (!is_null(certainty_prop)) {
    remaining <- seq_len(N)
    repeat {
      if (length(remaining) == 0) {
        break
      }
      mos_remaining <- mos_vals[remaining]
      total_mos <- sum(mos_remaining)
      if (total_mos <= 0) {
        break
      }

      props <- mos_remaining / total_mos
      above_threshold <- props >= certainty_prop
      if (!any(above_threshold)) {
        break
      }

      new_certain <- remaining[above_threshold]
      certainty_idx <- c(certainty_idx, new_certain)
      remaining <- setdiff(remaining, new_certain)
    }
  }

  n_certain <- length(certainty_idx)
  n_remaining <- n - n_certain
  remaining_idx <- setdiff(seq_len(N), certainty_idx)

  list(
    certainty_idx = certainty_idx,
    remaining_idx = remaining_idx,
    n_certain = n_certain,
    n_remaining = n_remaining
  )
}
