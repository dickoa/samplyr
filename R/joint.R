#' Compute pairwise joint expectations from a sample and its frame
#'
#' Reconstructs the second-order design quantities for PPS stages by
#' replaying the design specification against the original sampling
#' frame. For without-replacement (WOR) stages, this produces the joint
#' inclusion probabilities \eqn{\pi_{kl}}{pi_kl}. For with-replacement
#' (WR) and PMR stages, this produces the joint expected hits
#' \eqn{E(n_k \cdot n_l)}{E(n_k * n_l)}.
#'
#' @param x A `tbl_sample` object produced by [execute()].
#' @param frame The data frame originally passed to [execute()]. Must
#'   contain the same columns used during sampling (strata variables,
#'   cluster variables, measure of size).
#' @param stage An integer vector of stage numbers to compute, or
#'   `NULL` (default) to compute all PPS stages.
#'   Non-PPS stages produce `NULL` entries in the returned list.
#'
#' @return A named list of length equal to the number of executed
#'   stages. Each element is either:
#'   - For PPS WOR stages: a square matrix of joint inclusion
#'     probabilities \eqn{\pi_{kl}}{pi_kl}, usable with
#'     [survey::ppsmat()] for exact variance estimation.
#'   - For PPS WR/PMR stages (`pps_multinomial`, `pps_chromy`): a
#'     square matrix of joint expected hits
#'     \eqn{E(n_k \cdot n_l)}{E(n_k * n_l)}.
#'   - `NULL` for non-PPS stages (SRS, systematic) or stages not
#'     requested via the `stage` argument.
#'   Dimensions match the number of unique sampled units (or sampled
#'   clusters for clustered stages). Rows and columns are ordered to
#'   match the sample.
#'
#' @details
#' For each PPS stage, the function:
#' 1. Reconstructs the full-population first-order quantities from
#'    the frame using the stage's method and measure of size
#' 2. Dispatches to the appropriate sondage joint probability or
#'    joint expected hits function
#' 3. Extracts the submatrix corresponding to sampled units
#'
#' For stratified stages, the target sample size per stratum (n_h) is
#' reconstructed by replaying the same allocation logic used during
#' [execute()] (proportional, Neyman, optimal, etc.) against the
#' frame. This ensures first-order quantities match what was computed
#' at sampling time, regardless of allocation method.
#'
#' For stratified or conditional (within-cluster) stages, joint
#' quantities are computed independently within each group and
#' assembled into a block-diagonal matrix.
#'
#' ## Exact vs. approximate computation
#'
#' The accuracy of the returned matrix depends on the sampling method.
#' Some algorithms yield closed-form joint probabilities; others
#' require approximation or simulation.
#'
#' ### WOR methods (\eqn{\pi_{kl}}{pi_kl})
#'
#' | samplyr method     | sondage function              | Quality                            |
#' |--------------------|-------------------------------|------------------------------------|
#' | `pps_cps`          | `joint_inclusion_prob()`      | **Exact** (Aires' formula via C)   |
#' | `pps_systematic`   | `joint_inclusion_prob()`      | **Exact** (combinatorial enumeration) |
#' | `pps_poisson`      | `joint_inclusion_prob()`      | **Exact** (\eqn{\pi_{kl} = \pi_k \pi_l}{pi_kl = pi_k * pi_l}, independent draws) |
#' | `pps_brewer`       | `joint_inclusion_prob()`      | **Approximate**\eqn{^*} (high-entropy / Hajek-Brewer-Donadio) |
#' | `pps_sps`          | `joint_inclusion_prob()`      | **Approximate** (high-entropy / Hajek-Brewer-Donadio) |
#' | `pps_pareto`       | `joint_inclusion_prob()`      | **Approximate** (high-entropy / Hajek-Brewer-Donadio) |
#' | `balanced`         | `joint_inclusion_prob()`      | **Approximate** (high-entropy / Hajek-Brewer-Donadio) |
#'
#' \eqn{^*} Exact recursive formulas for Brewer's joint inclusion
#' probabilities exist (Brewer 2002, ch. 9) but are
#' \eqn{O(N^3)}{O(N^3)}, making them impractical for frames of more
#' than a few hundred units. The high-entropy approximation is
#' \eqn{O(N^2)}{O(N^2)} and sufficiently accurate for variance
#' estimation in practice. The same trade-off applies to SPS and
#' Pareto, whose exact joint probabilities would require enumerating
#' the combinatorial sample space.
#'
#' The high-entropy approximation assumes the design is close to the
#' maximum-entropy design with the same marginal \eqn{\pi_i}{pi_i}
#' (Hajek 1964; Brewer and Donadio 2003). This is a good approximation
#' for most PPS designs and is the same quantity that underlies the
#' Berger (2004) variance estimator used by `survey::svydesign(pps =
#' "brewer")`. For CPS (conditional Poisson / maximum entropy), the
#' joint probabilities are exact by definition.
#'
#' ### WR/PMR methods (\eqn{E(n_k \cdot n_l)}{E(n_k * n_l)})
#'
#' | samplyr method     | sondage function              | Quality                            |
#' |--------------------|-------------------------------|------------------------------------|
#' | `pps_multinomial`  | `joint_expected_hits()`       | **Exact** (analytic: \eqn{n(n-1) p_k p_l + n p_k \mathbf{1}_{k=l}}{n(n-1) p_k p_l + n p_k 1(k=l)}) |
#' | `pps_chromy`       | `joint_expected_hits()`       | **Approximate** (Monte Carlo simulation, 10 000 replicates) |
#'
#' For `pps_chromy`, the sequential dependence structure does not admit
#' a closed-form expression for \eqn{E(n_k \cdot n_l)}{E(n_k * n_l)}.
#' sondage uses Monte Carlo simulation (default 10 000 replicates) to
#' estimate the pairwise expectations. Increasing `nsim` (passed
#' through `...`) reduces Monte Carlo error at the cost of computation
#' time.
#'
#' ## Limitations
#'
#' - Requires the original sampling frame. The frame must be unchanged
#'   from what was passed to [execute()].
#' - Units in the frame must be uniquely identifiable within each
#'   stratum/cluster group by their column values.
#' - For WOR designs with certainty selections (\eqn{\pi_i = 1}{pi_i = 1}),
#'   the joint matrix is decomposed: certainty units are separated
#'   from the stochastic part, the joint probabilities for
#'   non-certainty units are computed from the reduced \eqn{\pi}{pi}
#'   vector, and the full matrix is reassembled with
#'   \eqn{\pi_{ij} = 1}{pi_ij = 1} for certainty pairs and
#'   \eqn{\pi_{ij} = \pi_j}{pi_ij = pi_j} for certainty x
#'   non-certainty pairs.
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
#' sample <- sampling_design() |>
#'   add_stage() |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = households) |>
#'   add_stage() |>
#'     draw(n = 12) |>
#'   execute(bfa_eas, seed = 2025)
#'
#' # Compute joint probabilities for stage 1
#' jip <- joint_expectation(sample, bfa_eas, stage = 1)
#'
#' # Use with survey package for exact variance (WOR stages)
#' svy <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
#'
#' # Compute all PPS stages at once
#' jip_all <- joint_expectation(sample, bfa_eas)
#'
#' @seealso [as_svydesign()] for the default export using Brewer's
#'   approximation, [survey::ppsmat()] for wrapping joint matrices
#'
#' @export
joint_expectation <- function(x, frame, stage = NULL) {
  if (!inherits(x, "tbl_sample")) {
    cli_abort("{.arg x} must be a {.cls tbl_sample} object.")
  }
  check_single_replicate(x, "joint_expectation")

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)
  n_stages <- length(stages_executed)

  if (is_null(stage)) {
    stages_requested <- stages_executed
  } else {
    if (!is_integerish_numeric(stage)) {
      cli_abort("{.arg stage} must be integer stage number(s).")
    }
    stage <- as.integer(stage)
    invalid <- setdiff(stage, stages_executed)
    if (length(invalid) > 0) {
      invalid_str <- paste(invalid, collapse = ", ")
      executed_str <- paste(stages_executed, collapse = ", ")
      cli_abort(
        "Stage {invalid_str} not executed. Executed stages: {executed_str}."
      )
    }
    stages_requested <- stage
  }

  result <- vector("list", max(stages_executed))
  names(result) <- paste0("stage_", seq_along(result))

  for (stage_idx in stages_requested) {
    stage_spec <- design$stages[[stage_idx]]
    method <- stage_spec$draw_spec$method

    if (!(method %in% jip_methods) && is_null(stage_spec$draw_spec$method_type)) {
      next
    }

    result[[stage_idx]] <- compute_stage_jip(
      x,
      frame,
      design,
      stage_idx,
      stages_executed
    )
  }

  result
}

#' Compute joint inclusion probabilities for a single stage
#' @noRd
compute_stage_jip <- function(
  x,
  frame,
  design,
  stage_idx,
  stages_executed
) {
  stage_spec <- design$stages[[stage_idx]]
  draw_spec <- stage_spec$draw_spec

  effective_frame <- prepare_stage_frame(
    x,
    frame,
    design,
    stage_idx,
    stages_executed
  )
  strata_spec <- stage_spec$strata
  cluster_spec <- stage_spec$clusters
  strata_vars <- if (!is_null(strata_spec)) strata_spec$vars else character()
  sample_df <- as.data.frame(x)

  ancestor_vars <- collect_ancestor_cluster_vars(design, stage_idx)

  if (!is_null(cluster_spec)) {
    cluster_vars <- cluster_spec$vars
    dedup_vars <- unique(c(ancestor_vars, cluster_vars))
    frame_keep <- unique(c(dedup_vars, strata_vars, draw_spec$mos))
    effective_frame <- effective_frame |>
      select(all_of(frame_keep)) |>
      distinct(across(all_of(dedup_vars)), .keep_all = TRUE)
    sample_keep <- unique(c(dedup_vars, strata_vars))
    sample_df <- sample_df |>
      select(all_of(sample_keep)) |>
      distinct(across(all_of(dedup_vars)), .keep_all = TRUE)
  }

  if (!is_null(strata_spec)) {
    compute_stratified_jip(
      effective_frame,
      sample_df,
      strata_spec,
      draw_spec,
      cluster_spec,
      ancestor_cluster_vars = ancestor_vars
    )
  } else {
    n_target <- resolve_unstratified_n(effective_frame, draw_spec)
    compute_group_jip(
      effective_frame,
      sample_df,
      draw_spec,
      n_target,
      strata_vars = NULL,
      cluster_spec = cluster_spec,
      ancestor_cluster_vars = ancestor_vars
    )
  }
}

#' Compute joint probabilities for a stratified stage
#'
#' Replays `calculate_stratum_sizes()` against the frame to recover
#' the exact target n_h per stratum, then computes joint matrices per
#' stratum and assembles a block-diagonal.
#' @noRd
compute_stratified_jip <- function(
  effective_frame,
  sample_df,
  strata_spec,
  draw_spec,
  cluster_spec,
  ancestor_cluster_vars = character(0)
) {
  strata_vars <- strata_spec$vars

  stratum_info <- effective_frame |>
    group_by(across(all_of(strata_vars))) |>
    summarise(.N_h = n(), .groups = "drop")

  stratum_info <- calculate_stratum_sizes(stratum_info, strata_spec, draw_spec)
  n_h_lookup <- setNames(
    stratum_info$.n_h,
    make_strata_group_ids(stratum_info, strata_vars)
  )
  draw_lookup <- prepare_stratum_draw_lookup(draw_spec, strata_vars)

  frame_group_ids <- make_strata_group_ids(effective_frame, strata_vars)
  frame_group_rows <- split(seq_len(nrow(effective_frame)), frame_group_ids)

  sample_group_rows <- split(
    seq_len(nrow(sample_df)),
    make_strata_group_ids(sample_df, strata_vars)
  )
  empty_sample <- sample_df[0, , drop = FALSE]

  block_matrices <- lapply(names(frame_group_rows), function(stratum_id) {
    frame_rows <- frame_group_rows[[stratum_id]]
    group_frame <- effective_frame[frame_rows, , drop = FALSE]
    sample_rows <- sample_group_rows[[stratum_id]]
    group_sample <- if (is_null(sample_rows)) {
      empty_sample
    } else {
      sample_df[sample_rows, , drop = FALSE]
    }

    n_h <- n_h_lookup[[stratum_id]]
    if (is_null(n_h) || is.na(n_h)) {
      cli_abort(
        "Could not resolve target stratum sample size while computing joint probabilities.",
        call = NULL
      )
    }

    keys <- group_frame[1, strata_vars, drop = FALSE]
    stratum_draw_spec <- resolve_stratum_draw_spec(
      draw_spec,
      keys,
      strata_vars,
      stratum_key = stratum_id,
      lookup = draw_lookup
    )

    compute_group_jip(
      group_frame,
      group_sample,
      stratum_draw_spec,
      n_h,
      strata_vars = NULL,
      cluster_spec,
      ancestor_cluster_vars = ancestor_cluster_vars
    )
  })

  assemble_block_diagonal(block_matrices)
}

#' Build stable stratum IDs for grouped joins/splits
#' @noRd
make_strata_group_ids <- function(data, strata_vars) {
  if (nrow(data) == 0L) {
    return(character())
  }
  make_group_key(data, strata_vars)
}

#' Resolve target sample size for an unstratified stage
#' @noRd
resolve_unstratified_n <- function(frame, draw_spec) {
  N <- nrow(frame)
  round_method <- draw_spec$round %||% "up"

  if (!is_null(draw_spec$n)) {
    return(min(as.integer(draw_spec$n), N))
  }

  if (!is_null(draw_spec$frac)) {
    frac <- draw_spec$frac
    if (is.numeric(frac) && length(frac) == 1) {
      return(round_sample_size(N * frac, round_method))
    }
  }

  cli_abort("Cannot determine target sample size for unstratified stage.",
            call = NULL)
}

#' Prepare the effective frame for a given stage
#'
#' Stage 1: full frame
#' Stage k: frame subsetted to selected clusters from stage k-1
#' @noRd
prepare_stage_frame <- function(
  x,
  frame,
  design,
  stage_idx,
  stages_executed
) {
  pos <- match(stage_idx, stages_executed)
  if (pos == 1L) {
    return(frame)
  }

  prev_stage_idx <- stages_executed[pos - 1L]
  prev_stage_spec <- design$stages[[prev_stage_idx]]

  if (!is_null(prev_stage_spec$clusters)) {
    ancestor_vars <- collect_ancestor_cluster_vars(design, stage_idx)
    join_vars <- unique(c(ancestor_vars, prev_stage_spec$clusters$vars))
    sample_df <- as.data.frame(x)
    join_vars <- intersect(
      join_vars, intersect(names(frame), names(sample_df))
    )

    selected_clusters <- sample_df |>
      distinct(across(all_of(join_vars)))

    frame |>
      semi_join(selected_clusters, by = join_vars)
  } else {
    frame
  }
}

#' Compute joint inclusion probabilities within a single group
#' (one stratum, or one parent cluster, or the whole frame)
#' @noRd
compute_group_jip <- function(
  group_frame,
  sample_df,
  draw_spec,
  n_target,
  strata_vars,
  cluster_spec,
  ancestor_cluster_vars = character(0)
) {
  sampled_idx <- match_sampled_units(
    group_frame,
    sample_df,
    strata_vars,
    cluster_spec,
    ancestor_cluster_vars = ancestor_cluster_vars
  )

  if (length(sampled_idx) == 0) {
    return(NULL)
  }

  compute_joint_matrix(
    frame = group_frame,
    n = n_target,
    draw_spec = draw_spec,
    sampled_idx = sampled_idx
  )
}

#' Compute the sampled joint matrix for one group
#'
#' Reconstructs first-order quantities on the full group, then requests
#' sampled-only second-order expectations from `sondage`.
#' @noRd
compute_joint_matrix <- function(frame, n, draw_spec, sampled_idx) {
  method <- draw_spec$method
  mos_var <- draw_spec$mos
  N <- nrow(frame)

  if (!is_null(mos_var)) {
    mos_vals <- frame[[mos_var]]
    if (sum(mos_vals) <= 0) {
      cli_abort(c(
        "Cannot compute joint expectations: sum of MOS variable {.var {mos_var}} is zero.",
        "i" = "At least one unit must have a positive measure of size."
      ), call = NULL)
    }
  } else {
    mos_vals <- NULL
  }

  has_explicit_certainty <- !is_null(draw_spec$certainty_size) ||
    !is_null(draw_spec$certainty_prop)

  if (has_explicit_certainty) {
    return(compute_joint_matrix_with_certainty(
      method = method,
      mos_vals = mos_vals,
      n = n,
      draw_spec = draw_spec,
      sampled_idx = sampled_idx
    ))
  }

  # WR/PMR: joint expected hits, no certainty decomposition needed
  is_wr <- method %in% pps_wr_methods ||
    identical(draw_spec$method_type, "wr")
  if (is_wr) {
    pik <- sondage::expected_hits(mos_vals, n)
    return(compute_jeh_by_method(pik, n, method, sampled_idx))
  }

  pik <- compute_stage_pik(method, mos_vals, n, draw_spec, N = N)
  compute_jip_from_pik(pik, method, sampled_idx, draw_spec = draw_spec)
}

#' Compute first-order inclusion/hit expectations for one stage
#' @noRd
compute_stage_pik <- function(method, mos_vals, n, draw_spec, N = length(mos_vals)) {
  # Custom registered methods
  if (!is_null(draw_spec$method_type)) {
    if (draw_spec$method_type == "wr") {
      return(sondage::expected_hits(mos_vals, n))
    }
    return(sondage::inclusion_prob(mos_vals, n))
  }

  switch(
    method,
    pps_multinomial = ,
    pps_chromy = {
      sondage::expected_hits(mos_vals, n)
    },
    pps_brewer = ,
    pps_systematic = ,
    pps_cps = ,
    pps_sps = ,
    pps_pareto = {
      sondage::inclusion_prob(mos_vals, n)
    },
    pps_poisson = {
      frac <- draw_spec$frac %||% (n / N)
      pik_raw <- frac * mos_vals / sum(mos_vals) * N
      pmin(pik_raw, 1)
    },
    balanced = {
      if (!is_null(mos_vals)) {
        sondage::inclusion_prob(mos_vals, n)
      } else {
        rep(n / N, N)
      }
    },
    cli_abort(
      "No joint probability function for method {.val {method}}",
      call = NULL
    )
  )
}

#' Compute sampled joint matrix from first-order probabilities/hits
#' @noRd
compute_jip_from_pik <- function(pik, method, sampled_idx, n = NULL, draw_spec = NULL) {
  sampled_idx <- as.integer(sampled_idx)

  is_wr <- method %in% pps_wr_methods ||
    identical(draw_spec$method_type, "wr")
  if (is_wr) {
    if (is_null(n)) {
      cli_abort(
        "Internal error: {.arg n} must be provided for WR/PMR methods.",
        call = NULL
      )
    }
    return(compute_jeh_by_method(pik, n, method, sampled_idx))
  }

  cert_tol <- 1 - sqrt(.Machine$double.eps)
  cert_idx <- which(pik >= cert_tol)

  if (length(cert_idx) == 0) {
    return(compute_jip_by_method(pik, method, sampled_idx, draw_spec = draw_spec))
  }

  assemble_jip_with_certainty(pik, cert_idx, method, sampled_idx, draw_spec = draw_spec)
}

#' Compute sampled joint matrix when certainty thresholds are explicit
#' @noRd
compute_joint_matrix_with_certainty <- function(
  method,
  mos_vals,
  n,
  draw_spec,
  sampled_idx
) {
  cert <- identify_certainty(
    mos_vals = mos_vals,
    n = n,
    certainty_size = draw_spec$certainty_size,
    certainty_prop = draw_spec$certainty_prop
  )

  sampled_idx <- as.integer(sampled_idx)
  n_sampled <- length(sampled_idx)
  result <- matrix(0, nrow = n_sampled, ncol = n_sampled)

  cert_idx <- cert$certainty_idx
  cert_pos <- which(sampled_idx %in% cert_idx)
  prob_pos <- which(!(sampled_idx %in% cert_idx))

  if (length(cert_pos) > 0) {
    result[cert_pos, cert_pos] <- 1
  }

  if (length(prob_pos) == 0) {
    return(result)
  }

  if (cert$n_remaining <= 0 || length(cert$remaining_idx) == 0) {
    cli_abort(
      c(
        "Could not reconstruct probabilistic remainder for certainty design.",
        "i" = "Sample contains non-certainty units but certainty selection left no remainder."
      ),
      call = NULL
    )
  }

  remaining_idx <- cert$remaining_idx
  n_prob <- min(cert$n_remaining, length(remaining_idx))
  remaining_mos <- mos_vals[remaining_idx]
  sampled_prob_idx <- sampled_idx[prob_pos]
  sampled_prob_reduced <- match(sampled_prob_idx, remaining_idx)

  reduced_draw_spec <- draw_spec
  reduced_draw_spec$certainty_size <- NULL
  reduced_draw_spec$certainty_prop <- NULL
  pik_prob <- compute_stage_pik(
    method = method,
    mos_vals = remaining_mos,
    n = n_prob,
    draw_spec = reduced_draw_spec,
    N = length(remaining_mos)
  )

  prob_block <- compute_jip_from_pik(
    pik = pik_prob,
    method = method,
    sampled_idx = sampled_prob_reduced,
    n = n_prob,
    draw_spec = reduced_draw_spec
  )
  result[prob_pos, prob_pos] <- prob_block

  if (length(cert_pos) > 0) {
    prob_diag <- diag(prob_block)
    result[cert_pos, prob_pos] <- rep(prob_diag, each = length(cert_pos))
    result[prob_pos, cert_pos] <- rep(prob_diag, times = length(cert_pos))
  }

  result
}

#' Dispatch to sondage::joint_inclusion_prob for WOR methods
#' @noRd
compute_jip_by_method <- function(pik, method, sampled_idx, draw_spec = NULL) {
  sondage_name <- sondage_method_name(method)
  fixed <- if (!is_null(draw_spec$method_fixed)) {
    draw_spec$method_fixed
  } else {
    !(sondage_name %in% c("poisson", "bernoulli"))
  }
  design <- structure(
    list(
      sample = as.integer(sampled_idx),
      pik = pik,
      n = as.integer(round(sum(pik))),
      N = length(pik),
      method = sondage_name,
      fixed_size = fixed
    ),
    class = c("unequal_prob", "wor", "sondage_sample")
  )
  sondage::joint_inclusion_prob(design, sampled_only = TRUE)
}

#' Dispatch to sondage::joint_expected_hits for WR/PMR methods
#' @noRd
compute_jeh_by_method <- function(pik, n, method, sampled_idx) {
  sondage_name <- sondage_method_name(method)

  sampled_idx <- as.integer(sampled_idx)
  sampled_sorted <- sort(unique(sampled_idx))
  hits <- integer(length(pik))
  hits[sampled_sorted] <- 1L

  design <- structure(
    list(
      sample = sampled_sorted,
      prob = pik / n,
      hits = hits,
      n = as.integer(n),
      N = length(pik),
      method = sondage_name,
      fixed_size = TRUE
    ),
    class = c("unequal_prob", "wr", "sondage_sample")
  )
  jeh_sorted <- sondage::joint_expected_hits(design, sampled_only = TRUE)

  reorder_idx <- match(sampled_idx, sampled_sorted)
  jeh_sorted[reorder_idx, reorder_idx, drop = FALSE]
}

#' Assemble joint matrix separating certainty from stochastic units
#'
#' Certainty units (pi_i = 1) are always in the sample, so their
#' joint probabilities are known without approximation:
#'   pi_ij = 1        if both i and j are certainty
#'   pi_ij = pi_j     if only i is certainty
#' The stochastic part is computed from the reduced pi vector.
#' @noRd
assemble_jip_with_certainty <- function(pik, cert_idx, method, sampled_idx, draw_spec = NULL) {
  N <- length(pik)
  non_cert_idx <- setdiff(seq_len(N), cert_idx)
  sampled_idx <- as.integer(sampled_idx)
  n_sampled <- length(sampled_idx)

  if (n_sampled == 0L) {
    return(NULL)
  }

  is_cert_sample <- sampled_idx %in% cert_idx
  result <- matrix(0, nrow = n_sampled, ncol = n_sampled)

  cert_pos <- which(is_cert_sample)
  non_cert_pos <- which(!is_cert_sample)
  sampled_non_cert_idx <- sampled_idx[non_cert_pos]

  if (length(cert_pos) > 0) {
    result[cert_pos, cert_pos] <- 1
  }

  if (length(cert_pos) > 0 && length(non_cert_pos) > 0) {
    non_cert_pik <- pik[sampled_non_cert_idx]
    result[cert_pos, non_cert_pos] <- rep(
      non_cert_pik,
      each = length(cert_pos)
    )
    result[non_cert_pos, cert_pos] <- rep(
      non_cert_pik,
      times = length(cert_pos)
    )
  }

  if (length(non_cert_pos) > 1) {
    sampled_non_cert_reduced <- match(sampled_non_cert_idx, non_cert_idx)
    jip_reduced <- compute_jip_by_method(
      pik = pik[non_cert_idx],
      method = method,
      sampled_idx = sampled_non_cert_reduced,
      draw_spec = draw_spec
    )
    result[non_cert_pos, non_cert_pos] <- jip_reduced
  } else if (length(non_cert_pos) == 1) {
    result[non_cert_pos, non_cert_pos] <- pik[sampled_non_cert_idx]
  }

  result
}

#' Match sampled units to their positions in the frame
#' @noRd
match_sampled_units <- function(
  group_frame,
  sample_df,
  strata_vars,
  cluster_spec,
  ancestor_cluster_vars = character(0)
) {
  if (!is_null(cluster_spec)) {
    match_vars <- unique(c(ancestor_cluster_vars, cluster_spec$vars))
  } else {
    frame_vars <- setdiff(
      names(group_frame),
      grep("^\\.", names(group_frame), value = TRUE)
    )
    sample_vars <- setdiff(
      names(sample_df),
      grep("^\\.", names(sample_df), value = TRUE)
    )
    match_vars <- intersect(frame_vars, sample_vars)
  }

  if (length(match_vars) == 0) {
    cli_abort("No shared columns to match sampled units to frame.",
              call = NULL)
  }

  group_sample <- sample_df
  if (!is_null(strata_vars)) {
    strata_keys <- group_frame |>
      distinct(across(all_of(strata_vars)))
    group_sample <- group_sample |>
      semi_join(strata_keys, by = strata_vars)
  }

  if (!is_null(cluster_spec) && length(match_vars) == 1L) {
    key_var <- match_vars[[1]]
    frame_key <- group_frame[[key_var]]
    if (is.factor(frame_key)) {
      frame_key <- as.character(frame_key)
    }
    if (anyDuplicated(frame_key) > 0L) {
      n_unique <- dplyr::n_distinct(frame_key)
      cli_abort(c(
        "Frame rows are not uniquely identified by columns {.val {match_vars}}.",
        "i" = "Found {nrow(group_frame)} rows but only {n_unique} unique key combinations.",
        "i" = "Ensure the frame has a column (or combination) that uniquely identifies each unit."
      ), call = NULL)
    }

    sample_key <- group_sample[[key_var]]
    if (is.factor(sample_key)) {
      sample_key <- as.character(sample_key)
    }
    sample_key <- unique(sample_key)
    sampled_idx <- match(sample_key, frame_key)
    return(sampled_idx[!is.na(sampled_idx)])
  }

  n_unique <- nrow(distinct(group_frame, across(all_of(match_vars))))
  if (n_unique != nrow(group_frame)) {
    cli_abort(c(
      "Frame rows are not uniquely identified by columns {.val {match_vars}}.",
      "i" = "Found {nrow(group_frame)} rows but only {n_unique} unique key combinations.",
      "i" = "Ensure the frame has a column (or combination) that uniquely identifies each unit."
    ), call = NULL)
  }

  frame_keys <- group_frame |>
    select(all_of(match_vars))
  sample_keys <- group_sample |>
    select(all_of(match_vars)) |>
    distinct()

  matched <- inner_join(
    sample_keys |> mutate(.sample_row = row_number()),
    frame_keys |> mutate(.frame_row = row_number()),
    by = match_vars
  )

  matched$.frame_row[order(matched$.sample_row)]
}

#' Assemble joint probability matrix from per-group matrices
#'
#' Initializes the full matrix with `outer(pi, pi)` (the product of all
#' marginal inclusion probabilities), then overwrites each diagonal block
#' with the actual within-group joint probabilities. Cross-group entries
#' remain at `pi_i * pi_j`, which is correct because sampling in
#' independent strata implies `pi_kl = pi_k * pi_l` for units in
#' different groups.
#' @noRd
assemble_block_diagonal <- function(matrices) {
  matrices <- Filter(Negate(is.null), matrices)
  if (length(matrices) == 0) {
    return(NULL)
  }
  if (length(matrices) == 1) {
    return(matrices[[1]])
  }

  pi_vec <- unlist(lapply(matrices, diag))
  result <- outer(pi_vec, pi_vec)

  offset <- 0L
  for (mat in matrices) {
    n_block <- nrow(mat)
    idx <- seq(offset + 1L, offset + n_block)
    result[idx, idx] <- mat
    offset <- offset + n_block
  }

  result
}
