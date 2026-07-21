#' Selection trace nodes
#'
#' Every selection wrapper returns list(sample, trace). The trace tree
#' records, for each selection pool, the resolved chance vector and
#' the selected positions in executed order, captured before the
#' engine discards them. A pool leaf holds the vectors; split and
#' clusters nodes record how the frame handed to a wrapper was
#' partitioned, so pool membership maps back to frame rows by
#' composing `rows` indices down the tree. Capture is observational:
#' it never touches the random stream or the selected sample.
#' @noRd
trace_pool <- function(
  N,
  n_target,
  chance,
  chance_kind,
  order_kind,
  selected,
  perm = NULL,
  cert_rule = integer(0),
  trace_mode = "full",
  compact = TRUE
) {
  if (identical(trace_mode, "none")) {
    return(NULL)
  }
  if (
    identical(trace_mode, "summary") && compact &&
      length(chance) > 1L && !anyNA(chance) &&
      all(chance == chance[[1L]])
  ) {
    chance <- chance[[1L]]
  }
  list(
    type = "pool",
    N = N,
    n_target = n_target,
    chance = chance,
    chance_kind = chance_kind,
    order_kind = order_kind,
    perm = perm,
    selected = selected,
    cert_rule = cert_rule
  )
}

#' One partition of a frame: `groups[[i]]` describes subset i and the
#' trace node produced on it. `rows` are positions into the frame the
#' partitioning wrapper received.
#' @noRd
trace_split <- function(by, groups) {
  list(type = "split", by = by, groups = groups)
}

#' @noRd
trace_group <- function(key, keys, rows, node) {
  list(key = key, keys = keys, rows = rows, node = node)
}

#' A cluster stage: the child node was produced on the one-row-per-
#' cluster frame; `keys`, `first_rows` and `sizes` are aligned with its
#' rows and map clusters back to the full frame.
#' @noRd
trace_clusters <- function(by, keys, first_rows, sizes, node) {
  list(
    type = "clusters",
    by = by,
    keys = keys,
    first_rows = first_rows,
    sizes = sizes,
    node = node
  )
}

#' Frame columns the cluster contract requires to be constant within
#' each cluster: the size measure, stratum labels, and balancing or
#' spreading variables.
#' @noRd
cluster_invariant_vars <- function(frame, draw_spec, strata_vars) {
  intersect(
    c(
      draw_spec$mos,
      strata_vars,
      draw_spec$bounds %||% character(0),
      draw_spec$spread %||% character(0)
    ),
    names(frame)
  )
}

#' @noRd
abort_cluster_invariants <- function(varying, cluster_vars) {
  cli_abort(
    c(
      "{.val {varying}} must be constant within each cluster defined by {.val {cluster_vars}}.",
      "i" = "Found clusters where {.val {varying}} varies across rows.",
      "i" = "Ensure the frame has one consistent value per cluster for these columns."
    ),
    call = NULL
  )
}

#' @noRd
sample_clusters <- function(
  frame,
  strata_spec,
  cluster_spec,
  draw_spec,
  trace_mode = "full"
) {
  cluster_vars <- cluster_spec$vars
  groups <- split_row_indices(frame, cluster_vars)
  cluster_indices <- groups$indices

  invariant_vars <- cluster_invariant_vars(
    frame, draw_spec, strata_spec$vars
  )

  if (length(invariant_vars) > 0) {
    check_vars <- c(cluster_vars, invariant_vars)
    n_clusters <- length(cluster_indices)
    n_combos <- frame |>
      distinct(across(all_of(check_vars))) |>
      nrow()

    if (n_combos != n_clusters) {
      varying <- invariant_vars[vapply(
        invariant_vars,
        function(v) {
          nrow(distinct(frame, across(all_of(c(cluster_vars, v))))) !=
            n_clusters
        },
        logical(1)
      )]
      abort_cluster_invariants(varying, cluster_vars)
    }
  }

  first_rows <- first_row_indices_by_group(cluster_indices)
  cluster_frame <- frame[first_rows, , drop = FALSE]

  if (is_balanced_method(draw_spec) && !is_null(draw_spec$aux)) {
    for (v in draw_spec$aux) {
      cluster_frame[[v]] <- vapply(
        cluster_indices,
        function(idx) {
          sum(frame[[v]][idx])
        },
        numeric(1)
      )
    }
  }

  inner_trace_mode <- if (identical(trace_mode, "summary")) {
    "full"
  } else {
    trace_mode
  }
  res <- sample_units(
    cluster_frame,
    strata_spec,
    draw_spec,
    trace_mode = inner_trace_mode
  )
  list(
    sample = res$sample,
    trace = if (identical(trace_mode, "none")) {
      NULL
    } else {
      trace_clusters(
        by = cluster_vars,
        keys = groups$keys,
        first_rows = first_rows,
        sizes = lengths(cluster_indices),
        node = res$trace
      )
    }
  )
}


#' @noRd
sample_within_clusters <- function(
  frame,
  strata_spec,
  draw_spec,
  cluster_vars,
  trace_mode = "full"
) {
  groups <- split_row_indices(frame, cluster_vars)
  indices_list <- groups$indices

  if (
    is_null(strata_spec) &&
      is_simple_srswor_draw(draw_spec) &&
      !is_null(draw_spec$n) &&
      length(draw_spec$n) == 1 &&
      is_integerish_numeric(draw_spec$n)
  ) {
    draw_n <- as.integer(draw_spec$n)
    n_per_group <- rep.int(draw_n, length(indices_list))
    res <- sample_srswor_by_group_indices(
      frame,
      indices_list,
      n_per_group,
      trace_mode = trace_mode
    )
    result <- res$sample
    if (nrow(result) > 0) {
      result$.sample_id <- seq_len(nrow(result))
    }
    trace_groups <- if (identical(trace_mode, "none")) {
      NULL
    } else {
      lapply(seq_along(indices_list), function(i) {
        trace_group(
          key = groups$keys[[i]],
          keys = NULL,
          rows = indices_list[[i]],
          node = res$leaves[[i]]
        )
      })
    }
    return(list(
      sample = result,
      trace = if (identical(trace_mode, "none")) {
        NULL
      } else {
        trace_split(by = cluster_vars, groups = trace_groups)
      }
    ))
  }

  results_list <- lapply(indices_list, function(idxs) {
    data <- frame[idxs, , drop = FALSE]
    sample_units(data, strata_spec, draw_spec, trace_mode = trace_mode)
  })

  result <- bind_rows(lapply(results_list, function(r) r$sample))
  if (nrow(result) > 0) {
    result$.sample_id <- seq_len(nrow(result))
  }
  trace_groups <- if (identical(trace_mode, "none")) {
    NULL
  } else {
    lapply(seq_along(indices_list), function(i) {
      trace_group(
        key = groups$keys[[i]],
        keys = NULL,
        rows = indices_list[[i]],
        node = results_list[[i]]$trace
      )
    })
  }
  list(
    sample = result,
    trace = if (identical(trace_mode, "none")) {
      NULL
    } else {
      trace_split(by = cluster_vars, groups = trace_groups)
    }
  )
}

#' @noRd
sample_units <- function(frame, strata_spec, draw_spec, trace_mode = "full") {
  if (!is_null(strata_spec)) {
    sample_stratified(frame, strata_spec, draw_spec, trace_mode = trace_mode)
  } else {
    sample_unstratified(frame, draw_spec, trace_mode = trace_mode)
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
sample_srswor_by_group_indices <- function(
  frame,
  indices_list,
  n_per_group,
  trace_mode = "full"
) {
  group_sizes <- lengths(indices_list)
  n_actual <- pmin(as.integer(n_per_group), group_sizes)
  n_actual[is.na(n_actual) | n_actual <= 0L] <- 0L
  ends <- cumsum(n_actual)
  starts <- ends - n_actual + 1L
  selected_rows <- integer(sum(n_actual))
  weights <- numeric(sum(n_actual))
  fpc <- numeric(sum(n_actual))
  leaves <- if (identical(trace_mode, "none")) {
    NULL
  } else {
    vector("list", length(indices_list))
  }

  for (i in seq_along(indices_list)) {
    idxs <- indices_list[[i]]
    N_i <- group_sizes[[i]]
    n_target <- n_per_group[[i]]
    n_i <- n_actual[[i]]

    if (n_i == 0L || N_i == 0L) {
      if (!identical(trace_mode, "none")) {
        leaves[[i]] <- trace_pool(
          N = N_i,
          n_target = n_target,
          chance = if (identical(trace_mode, "summary")) 0 else numeric(N_i),
          chance_kind = "inclusion_probability",
          order_kind = "input",
          selected = integer(0),
          trace_mode = trace_mode
        )
      }
      next
    }

    local_idx <- sample.int(N_i, n_i)
    out <- seq.int(starts[[i]], ends[[i]])
    selected_rows[out] <- idxs[local_idx]
    weights[out] <- N_i / n_i
    fpc[out] <- N_i
    if (!identical(trace_mode, "none")) {
      leaves[[i]] <- trace_pool(
        N = N_i,
        n_target = n_target,
        chance = if (identical(trace_mode, "summary")) {
          n_i / N_i
        } else {
          rep.int(n_i / N_i, N_i)
        },
        chance_kind = "inclusion_probability",
        order_kind = "input",
        selected = local_idx,
        trace_mode = trace_mode
      )
    }
  }

  if (length(selected_rows) == 0L) {
    empty <- frame[0, , drop = FALSE]
    empty$.weight <- numeric(0)
    empty$.fpc <- numeric(0)
    return(list(sample = empty, leaves = leaves))
  }

  result <- frame[selected_rows, , drop = FALSE]
  result$.weight <- weights
  result$.fpc <- fpc
  list(sample = result, leaves = leaves)
}

#' @noRd
sample_stratified <- function(
  frame,
  strata_spec,
  draw_spec,
  trace_mode = "full"
) {
  strata_vars <- strata_spec$vars
  groups <- split_row_indices(frame, strata_vars)
  stratum_info <- stratum_info_from_groups(frame, strata_vars, groups$indices)

  stratum_info <- calculate_stratum_sizes(stratum_info, strata_spec, draw_spec)

  if (identical(draw_spec$method, "cube")) {
    return(draw_balanced_stratified(
      frame,
      groups,
      stratum_info,
      strata_vars,
      draw_spec,
      trace_mode = trace_mode
    ))
  }

  if (!is_multi_hit_method(draw_spec)) {
    capped <- stratum_info$.n_h > stratum_info$.N_h
    if (any(capped)) {
      n_requested <- sum(stratum_info$.n_h)
      n_actual <- sum(pmin(stratum_info$.n_h, stratum_info$.N_h))
      capped_keys <- format_key_labels(
        stratum_info[capped, , drop = FALSE],
        strata_vars
      )
      cli_warn(c(
        "Sample size capped to population in {length(capped_keys)} stratum/strata: {.val {capped_keys}}.",
        "i" = "Requested total: {n_requested}. Actual total: {n_actual}."
      ))
    }
  }

  if (is_simple_srswor_draw(draw_spec)) {
    n_per_group <- as.integer(stratum_info$.n_h)

    res <- sample_srswor_by_group_indices(
      frame,
      groups$indices,
      n_per_group,
      trace_mode = trace_mode
    )
    result <- res$sample
    result$.sample_id <- seq_len(nrow(result))
    trace_groups <- if (identical(trace_mode, "none")) {
      NULL
    } else {
      lapply(seq_along(groups$indices), function(i) {
        idxs <- groups$indices[[i]]
        trace_group(
          key = groups$keys[[i]],
          keys = frame[idxs[1], strata_vars, drop = FALSE],
          rows = idxs,
          node = res$leaves[[i]]
        )
      })
    }
    return(list(
      sample = result,
      trace = if (identical(trace_mode, "none")) {
        NULL
      } else {
        trace_split(by = strata_vars, groups = trace_groups)
      }
    ))
  }

  draw_lookup <- prepare_stratum_draw_lookup(draw_spec, strata_vars)
  results_list <- lapply(seq_along(groups$indices), function(i) {
    idxs <- groups$indices[[i]]
    stratum_key <- groups$keys[[i]]
    data <- frame[idxs, , drop = FALSE]

    n_h <- stratum_info$.n_h[[i]]
    if (is_null(n_h) || length(n_h) == 0 || is.na(n_h)) {
      cli_abort(
        "Could not determine sample size for stratum {.val {stratum_key}}"
      )
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

    res <- withCallingHandlers(
      draw_sample(data, n_h, stratum_draw_spec, trace_mode = trace_mode),
      error = function(e) {
        cli_abort(
          c(conditionMessage(e), "i" = "In stratum {.val {stratum_key}}"),
          call = NULL
        )
      }
    )
    selected <- res$sample
    selected$.weight <- 1 / selected$.pik
    selected$.pik <- NULL
    selected$.fpc <- if (is_multi_hit_method(draw_spec)) {
      rep.int(Inf, nrow(selected))
    } else {
      rep.int(N_h, nrow(selected))
    }
    list(
      sample = selected,
      group = if (identical(trace_mode, "none")) NULL else trace_group(
        key = stratum_key,
        keys = keys,
        rows = idxs,
        node = res$trace
      )
    )
  })

  result <- bind_rows(lapply(results_list, function(r) r$sample))

  result$.sample_id <- seq_len(nrow(result))
  list(
    sample = result,
    trace = if (identical(trace_mode, "none")) NULL else trace_split(
      by = strata_vars,
      groups = lapply(results_list, function(r) r$group)
    )
  )
}

#' Balanced sampling with stratified cube (single call to sondage::balanced_wor)
#' @noRd
draw_balanced_stratified <- function(
  frame,
  groups,
  stratum_info,
  strata_vars,
  draw_spec,
  trace_mode = "full"
) {
  N <- nrow(frame)
  mos <- draw_spec$mos
  aux_vars <- draw_spec$aux

  pik <- numeric(N)
  fpc_vec <- numeric(N)
  strata_int <- integer(N)

  for (i in seq_along(groups$indices)) {
    idxs <- groups$indices[[i]]
    n_h <- stratum_info$.n_h[[i]]
    N_h <- stratum_info$.N_h[[i]]
    n_h <- min(n_h, N_h)

    if (!is_null(mos)) {
      pik[idxs] <- sondage::inclusion_prob(frame[[mos]][idxs], n_h)
    } else {
      pik[idxs] <- rep(n_h / N_h, N_h)
    }
    fpc_vec[idxs] <- N_h
    strata_int[idxs] <- i
  }

  aux_mat <- if (!is_null(aux_vars)) {
    as.matrix(frame[, aux_vars, drop = FALSE])
  } else {
    NULL
  }

  row_order <- order(strata_int)
  pik_sorted <- pik[row_order]
  strata_sorted <- strata_int[row_order]
  aux_sorted <- if (!is_null(aux_mat)) {
    aux_mat[row_order, , drop = FALSE]
  } else {
    NULL
  }

  if (!is_null(draw_spec$bounds)) {
    bounds <- compile_count_bounds(
      frame[row_order, , drop = FALSE],
      pik_sorted,
      draw_spec$bounds,
      strata = strata_sorted
    )
    res <- draw_cube_with_bounds(pik_sorted, aux_sorted, bounds)
  } else {
    res <- sondage::balanced_wor(
      pik_sorted,
      aux = aux_sorted,
      strata = strata_sorted
    )
  }
  selected_sorted <- res$sample

  original_idx <- row_order[selected_sorted]
  result <- frame[original_idx, , drop = FALSE]
  result$.weight <- 1 / pik[original_idx]
  result$.fpc <- fpc_vec[original_idx]
  result$.sample_id <- seq_len(nrow(result))

  # One pool per stratum: the cube draw is joint, but the strata
  # constraint fixes each stratum's size, and within-stratum input
  # order is preserved by the stable sort.
  if (identical(trace_mode, "none")) {
    return(list(sample = result, trace = NULL))
  }

  sel_flag <- logical(N)
  sel_flag[original_idx] <- TRUE
  trace_groups <- lapply(seq_along(groups$indices), function(i) {
    idxs <- groups$indices[[i]]
    stratum_key <- groups$keys[[i]]
    trace_group(
      key = stratum_key,
      keys = frame[idxs[1], strata_vars, drop = FALSE],
      rows = idxs,
      node = trace_pool(
        N = length(idxs),
        n_target = stratum_info$.n_h[[i]],
        chance = pik[idxs],
        chance_kind = "inclusion_probability",
        order_kind = "input",
        selected = which(sel_flag[idxs]),
        trace_mode = trace_mode,
        compact = FALSE
      )
    )
  })
  list(
    sample = result,
    trace = trace_split(by = strata_vars, groups = trace_groups)
  )
}

#' Compile bound() markers into sondage linear count constraints
#' @noRd
compile_count_bounds <- function(data, pik, bound_vars, strata = NULL) {
  blocks <- list()
  labels <- character(0)

  for (var in bound_vars) {
    values <- data[[var]]
    levels_seen <- unique(values)
    block <- vapply(
      levels_seen,
      function(level) as.numeric(values == level),
      numeric(length(values))
    )
    if (is_null(dim(block))) {
      block <- matrix(block, ncol = 1L)
    }
    blocks[[length(blocks) + 1L]] <- block
    labels <- c(labels, paste0(var, "=", as.character(levels_seen)))
  }

  B <- do.call(cbind, blocks)
  expected <- as.numeric(crossprod(B, pik))
  tol <- 1e-10 * (1 + abs(expected))
  lower <- floor(expected + tol)
  upper <- ceiling(expected - tol)

  if (!is_null(strata)) {
    strata_levels <- unique(strata)
    strata_B <- vapply(
      strata_levels,
      function(level) as.numeric(strata == level),
      numeric(length(strata))
    )
    if (is_null(dim(strata_B))) {
      strata_B <- matrix(strata_B, ncol = 1L)
    }
    strata_n <- round(as.numeric(crossprod(strata_B, pik)))
    B <- cbind(B, strata_B)
    lower <- c(lower, strata_n)
    upper <- c(upper, strata_n)
    labels <- c(labels, paste0(".stratum=", strata_levels))
  }

  colnames(B) <- labels
  list(B = B, lower = lower, upper = upper)
}

#' Draw a cube sample while treating bound() constraints as hard requirements
#' @noRd
draw_cube_with_bounds <- function(pik, aux, bounds) {
  res <- withCallingHandlers(
    sondage::balanced_wor(pik, aux = aux, bounds = bounds, method = "cube"),
    warning = function(w) {
      if (
        grepl(
          "bound.*relax|relax.*bound",
          conditionMessage(w),
          ignore.case = TRUE
        )
      ) {
        abort_samplyr(
          c(
            "Controlled count bounds could not all be satisfied.",
            "i" = conditionMessage(w),
            "i" = "Reduce or widen the {.fn bound} constraints."
          ),
          class = "samplyr_error_relaxed_bounds",
          call = NULL
        )
      }
    }
  )
  realized <- colSums(bounds$B[res$sample, , drop = FALSE])
  violated <- realized < bounds$lower | realized > bounds$upper
  if (any(violated)) {
    abort_samplyr(
      "Controlled count bounds were violated during cube landing.",
      class = "samplyr_error_relaxed_bounds",
      call = NULL
    )
  }
  res
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
sample_unstratified <- function(frame, draw_spec, trace_mode = "full") {
  N <- nrow(frame)
  round_method <- draw_spec$round %||% "up"

  n <- if (!is_null(draw_spec$n)) {
    draw_spec$n
  } else if (!is_null(draw_spec$frac)) {
    round_sample_size(N * draw_spec$frac, round_method)
  } else {
    cli_abort("Cannot determine sample size", call = NULL)
  }

  if (!is_multi_hit_method(draw_spec) && n > N) {
    cli_warn(c(
      "Requested sample size ({n}) exceeds population size ({N}).",
      "i" = "Capped at {N} (census)."
    ))
  }

  res <- draw_sample(frame, n, draw_spec, trace_mode = trace_mode)
  result <- res$sample
  result$.weight <- 1 / result$.pik
  result$.pik <- NULL
  result$.fpc <- if (is_multi_hit_method(draw_spec)) {
    rep.int(Inf, nrow(result))
  } else {
    rep.int(N, nrow(result))
  }
  result$.sample_id <- seq_len(nrow(result))
  list(sample = result, trace = res$trace)
}

#' Handle zero-selection outcomes for random-size methods
#'
#' When a random-size method produces zero selections, either abort
#' (the default) or accept the empty realization. An empty sample is a
#' valid outcome of a Bernoulli/Poisson design: it contributes zero to
#' Horvitz-Thompson totals, which is what keeps the estimator unbiased
#' over repeated realizations. (An earlier fallback drew one unit by
#' SRS with weight N; those weights were conditional on the branch
#' reached, not inverse inclusion probabilities of the combined design,
#' and biased HT totals upward by N * (1 - p)^N.)
#' @noRd
handle_empty_selection <- function(method_label, on_empty) {
  header <- "{method_label} sampling produced zero selections."
  switch(
    on_empty,
    error = cli_abort(
      c(
        header,
        "i" = "Increase {.arg frac} (or {.arg n}), or use a fixed-size method.",
        "i" = "Set {.code on_empty = \"warn\"} or {.code \"silent\"} to accept an empty sample (a valid realization of a random-size design; estimates from repeated executions remain unbiased)."
      ),
      call = NULL
    ),
    warn = cli_warn(c(
      header,
      "!" = "Returning an empty sample.",
      "i" = "This is a valid realization of a random-size design; it contributes zero to Horvitz-Thompson totals.",
      "i" = "Set {.code on_empty = \"error\"} to catch this, or {.code on_empty = \"silent\"} to suppress."
    )),
    silent = NULL
  )
  invisible(NULL)
}

#' @noRd
draw_sample <- function(data, n, draw_spec, trace_mode = "full") {
  method <- draw_spec$method
  mos <- draw_spec$mos
  N <- nrow(data)
  random_size <- method %in% rs_poisson_methods ||
    isFALSE(draw_spec$method_fixed)
  # For random-size designs, `frac` targets an expected count rather
  # than a fixed cardinality. Preserve that nominal (possibly
  # fractional) request before probability capping; `n_expected`
  # records the expectation after the chances have been resolved.
  n_target <- if (
    random_size &&
      is.numeric(draw_spec$frac) && length(draw_spec$frac) == 1L
  ) {
    as.double(N * draw_spec$frac)
  } else {
    as.double(n)
  }

  perm <- NULL
  order_kind <- "input"
  if (!is_null(draw_spec$control)) {
    data$.__trace_row <- seq_len(N)
    data <- arrange(data, !!!draw_spec$control)
    perm <- data$.__trace_row
    data$.__trace_row <- NULL
    order_kind <- "control"
  }

  if (!is_multi_hit_method(draw_spec)) {
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

  is_custom <- !is_null(draw_spec$method_type)

  if ((method %in% pps_methods || is_custom) && has_certainty) {
    return(draw_sample_pps_certainty(
      data,
      n,
      draw_spec,
      n_target = n_target,
      order_kind = order_kind,
      perm = perm,
      trace_mode = trace_mode
    ))
  }

  pik <- NULL

  if (is_custom) {
    sondage_name <- sondage_method_name(method)
    prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
    if (draw_spec$method_type == "wr") {
      mos_vals <- data[[mos]]
      pik <- sondage::expected_hits(mos_vals, n)
      idx <- sondage::unequal_prob_wr(
        pik,
        method = sondage_name,
        prn = prn_vals
      )$sample
    } else if (draw_spec$method_type == "balanced") {
      pik <- if (!is_null(mos)) {
        sondage::inclusion_prob(data[[mos]], n)
      } else {
        rep(n / N, N)
      }
      aux_mat <- if (!is_null(draw_spec$aux)) {
        as.matrix(data[, draw_spec$aux, drop = FALSE])
      } else {
        NULL
      }
      spread_mat <- if (!is_null(draw_spec$spread)) {
        as.matrix(data[, draw_spec$spread, drop = FALSE])
      } else {
        NULL
      }
      idx <- sondage::balanced_wor(
        pik,
        aux = aux_mat,
        spread = spread_mat,
        method = sondage_name
      )$sample
    } else {
      mos_vals <- data[[mos]]
      pik <- sondage::inclusion_prob(mos_vals, n)
      idx <- sondage::unequal_prob_wor(
        pik,
        method = sondage_name,
        prn = prn_vals
      )$sample
      if (length(idx) == 0) {
        handle_empty_selection(method, draw_spec$on_empty %||% "error")
      }
    }
  } else {
    switch(
      method,
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
        frac <- draw_spec$frac %||% (n / N)
        pik <- rep(frac, N)
        prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
        idx <- sondage::equal_prob_wor(
          N,
          frac * N,
          method = "bernoulli",
          prn = prn_vals
        )$sample
        if (length(idx) == 0) {
          handle_empty_selection("Bernoulli", draw_spec$on_empty %||% "error")
        }
      },
      pps_poisson = {
        mos_vals <- data[[mos]]
        frac <- draw_spec$frac %||% (n / N)
        pik <- frac * mos_vals / sum(mos_vals) * N
        pik <- pmin(pik, 1)
        prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
        idx <- sondage::unequal_prob_wor(
          pik,
          method = "poisson",
          prn = prn_vals
        )$sample
        if (length(idx) == 0) {
          handle_empty_selection("PPS Poisson", draw_spec$on_empty %||% "error")
        }
      },
      pps_brewer = ,
      pps_systematic = ,
      pps_cps = ,
      pps_sampford = ,
      pps_sps = ,
      pps_pareto = {
        mos_vals <- data[[mos]]
        pik <- sondage::inclusion_prob(mos_vals, n)
        prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
        idx <- sondage::unequal_prob_wor(
          pik,
          method = sondage_method_name(method),
          prn = prn_vals
        )$sample
      },
      pps_multinomial = ,
      pps_chromy = {
        # Built-in WR methods do not support PRN coordination; `prn_methods`
        # in R/utils.R rejects it at validation time, so no prn arg is
        # threaded here. Custom WR methods that declare supports_prn = TRUE
        # are handled in the `is_custom` branch above.
        mos_vals <- data[[mos]]
        pik <- sondage::expected_hits(mos_vals, n)
        idx <- sondage::unequal_prob_wr(
          pik,
          method = sondage_method_name(method)
        )$sample
      },
      lpm2 = ,
      scps = {
        pik <- if (!is_null(mos)) {
          sondage::inclusion_prob(data[[mos]], n)
        } else {
          rep(n / N, N)
        }
        spread_mat <- as.matrix(data[, draw_spec$spread, drop = FALSE])
        idx <- sondage::balanced_wor(
          pik,
          spread = spread_mat,
          method = method
        )$sample
      },
      cube = {
        pik <- if (!is_null(mos)) {
          sondage::inclusion_prob(data[[mos]], n)
        } else {
          rep(n / N, N)
        }
        aux_mat <- if (!is_null(draw_spec$aux)) {
          as.matrix(data[, draw_spec$aux, drop = FALSE])
        } else {
          NULL
        }
        if (!is_null(draw_spec$bounds)) {
          bounds <- compile_count_bounds(data, pik, draw_spec$bounds)
          idx <- draw_cube_with_bounds(pik, aux_mat, bounds)$sample
        } else {
          idx <- sondage::balanced_wor(pik, aux = aux_mat)$sample
        }
      },
      cli_abort("Unknown sampling method: {.val {method}}")
    )
  }

  if (is_multi_hit_method(draw_spec)) {
    result <- data[idx, , drop = FALSE]
    result$.pik <- pik[idx]
    result$.draw <- seq_along(idx)
  } else {
    result <- data[idx, , drop = FALSE]
    result$.pik <- pik[idx]
  }

  if (method %in% pps_methods || is_custom) {
    result$.certainty <- rep.int(FALSE, nrow(result))
  }

  list(
    sample = result,
    trace = trace_pool(
      N = N,
      n_target = n_target,
      chance = pik,
      chance_kind = if (is_multi_hit_method(draw_spec)) {
        "expected_hits"
      } else {
        "inclusion_probability"
      },
      order_kind = order_kind,
      selected = idx,
      perm = perm,
      trace_mode = trace_mode,
      compact = !is_balanced_method(draw_spec)
    )
  )
}

#' @noRd
draw_sample_pps_certainty <- function(
  data,
  n,
  draw_spec,
  n_target = n,
  order_kind = "input",
  perm = NULL,
  trace_mode = "full"
) {
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

  # The pool chance vector: certainty units are taken with chance one;
  # units left unselectable (certainty filled or exceeded the target)
  # have chance zero.
  chance <- numeric(N)
  chance[cert$certainty_idx] <- 1
  selected <- cert$certainty_idx

  prob_result <- NULL
  if (cert$n_remaining > 0 && length(cert$remaining_idx) > 0) {
    remaining_data <- data[cert$remaining_idx, , drop = FALSE]
    remaining_mos <- mos_vals[cert$remaining_idx]
    n_prob <- min(cert$n_remaining, length(cert$remaining_idx))

    prob_res <- draw_pps_method(
      data = remaining_data,
      n = n_prob,
      method = method,
      mos_vals = remaining_mos,
      draw_spec = draw_spec
    )
    prob_result <- prob_res$sample
    prob_result$.certainty <- rep.int(FALSE, nrow(prob_result))
    chance[cert$remaining_idx] <- prob_res$chance
    selected <- c(selected, cert$remaining_idx[prob_res$selected])
  }

  trace <- if (identical(trace_mode, "none")) {
    NULL
  } else {
    trace_pool(
      N = N,
      n_target = n_target,
      chance = chance,
      chance_kind = if (is_multi_hit_method(draw_spec)) {
        "expected_hits"
      } else {
        "inclusion_probability"
      },
      order_kind = order_kind,
      selected = selected,
      perm = perm,
      cert_rule = cert$certainty_idx,
      trace_mode = trace_mode,
      compact = !is_balanced_method(draw_spec)
    )
  }

  if (is_null(certainty_result) && is_null(prob_result)) {
    result <- data[integer(0), , drop = FALSE]
    result$.pik <- numeric(0)
    result$.certainty <- logical(0)
    return(list(sample = result, trace = trace))
  }

  result <- dplyr::bind_rows(certainty_result, prob_result)
  list(sample = result, trace = trace)
}

#' @noRd
draw_pps_method <- function(data, n, method, mos_vals, draw_spec = NULL) {
  N <- nrow(data)

  if (sum(mos_vals) <= 0) {
    cli_abort(
      c(
        "Cannot use PPS sampling: sum of MOS values is zero.",
        "i" = "At least one remaining unit must have a positive measure of size.",
        "i" = "This can happen when certainty selection removes all units with positive MOS."
      ),
      call = NULL
    )
  }

  is_custom <- !is_null(draw_spec$method_type)

  if (is_custom) {
    sondage_name <- sondage_method_name(method)
    prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
    if (draw_spec$method_type == "wr") {
      pik <- sondage::expected_hits(mos_vals, n)
      idx <- sondage::unequal_prob_wr(
        pik,
        method = sondage_name,
        prn = prn_vals
      )$sample
    } else {
      pik <- sondage::inclusion_prob(mos_vals, n)
      idx <- sondage::unequal_prob_wor(
        pik,
        method = sondage_name,
        prn = prn_vals
      )$sample
      if (length(idx) == 0) {
        handle_empty_selection(method, draw_spec$on_empty %||% "error")
      }
    }
  } else {
    switch(
      method,
      pps_poisson = {
        frac <- draw_spec$frac %||% (n / N)
        pik <- frac * mos_vals / sum(mos_vals) * N
        pik <- pmin(pik, 1)
        prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
        idx <- sondage::unequal_prob_wor(
          pik,
          method = "poisson",
          prn = prn_vals
        )$sample
        if (length(idx) == 0) {
          handle_empty_selection("PPS Poisson", draw_spec$on_empty %||% "error")
        }
      },
      pps_brewer = ,
      pps_systematic = ,
      pps_cps = ,
      pps_sampford = ,
      pps_sps = ,
      pps_pareto = {
        pik <- sondage::inclusion_prob(mos_vals, n)
        prn_vals <- if (!is_null(draw_spec$prn)) data[[draw_spec$prn]] else NULL
        idx <- sondage::unequal_prob_wor(
          pik,
          method = sondage_method_name(method),
          prn = prn_vals
        )$sample
      },
      pps_multinomial = ,
      pps_chromy = {
        # Built-in WR methods do not accept PRN; see the note in draw_sample().
        pik <- sondage::expected_hits(mos_vals, n)
        idx <- sondage::unequal_prob_wr(
          pik,
          method = sondage_method_name(method)
        )$sample
      }
    )
  }

  if (is_multi_hit_method(draw_spec)) {
    result <- data[idx, , drop = FALSE]
    result$.pik <- pik[idx]
    result$.draw <- seq_along(idx)
  } else {
    result <- data[idx, , drop = FALSE]
    result$.pik <- pik[idx]
  }
  list(sample = result, chance = pik, selected = idx)
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
