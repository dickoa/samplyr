#' Compute joint inclusion probabilities from a sample and its frame
#'
#' Reconstructs the joint inclusion probabilities \eqn{\pi_{kl}}{pi_kl} for PPS
#' without replacement stages by replaying the design specification
#' against the original sampling frame. This allows exact variance
#' estimation via [survey::ppsmat()] instead of the default Brewer
#' approximation used by [as_survey_design()].
#'
#' @param x A `tbl_sample` object produced by [execute()].
#' @param frame The data frame originally passed to [execute()]. Must
#'   contain the same columns used during sampling (strata variables,
#'   cluster variables, measure of size).
#' @param stage An integer vector of stage numbers to compute, or
#'   `NULL` (default) to compute all PPS without replacement stages.
#'   Non-PPS stages produce `NULL` entries in the returned list.
#'
#' @return A named list of length equal to the number of executed
#'   stages. Each element is either:
#'   - A square matrix of joint inclusion probabilities for PPS WOR
#'     stages, with dimensions matching the number of sampled units
#'     at that stage (or sampled clusters for clustered stages).
#'     Rows and columns are ordered to match the sample.
#'   - `NULL` for non-PPS stages (SRS, systematic, WR methods, PMR
#'     methods) or stages not requested via the `stage` argument.
#'
#' @details
#' For each PPS without replacement stage, the function:
#' 1. Reconstructs the full-population inclusion probabilities \eqn{\pi_i}{pi_i}
#'    from the frame using the stage's method and measure of size
#' 2. Dispatches to the appropriate sondage joint probability function
#' 3. Extracts the submatrix corresponding to sampled units
#'
#' For stratified stages, the target sample size per stratum (n_h) is
#' reconstructed by replaying the same allocation logic used during
#' [execute()] (proportional, Neyman, optimal, etc.) against the
#' frame. This ensures \eqn{\pi_i}{pi_i} values match what was computed at sampling
#' time, regardless of allocation method.
#'
#' For stratified or conditional (within-cluster) stages, joint
#' probabilities are computed independently within each group and
#' assembled into a block-diagonal matrix.
#'
#' ## Method dispatch
#'
#' | samplyr method     | sondage function              |
#' |--------------------|-------------------------------|
#' | `pps_brewer`       | `up_brewer_jip(pik)`        |
#' | `pps_systematic`   | `up_systematic_jip(pik)`    |
#' | `pps_maxent`       | `up_maxent_jip(pik)`        |
#' | `pps_poisson`      | `up_poisson_jip(pik)`       |
#'
#' ## Chromy's sequential PPS method (PMR)
#'
#' `pps_chromy` is excluded from joint probability computation.
#' Chromy's method is a *Probability Minimum Replacement* (PMR)
#' method, not a standard WOR method. Units can receive multiple
#' hits, so the relevant pairwise quantities are expected sample
#' size products \eqn{E(n_i \cdot n_j)}{E(n_i * n_j)}, not inclusion probabilities \eqn{\pi_{ij}}{pi_ij}.
#'
#' While sondage provides `up_chromy_pairexp()` to estimate \eqn{E(n_i \cdot n_j)}{E(n_i * n_j)}
#' via Monte Carlo, the result cannot be passed to `survey::ppsmat()`
#' because the survey package assumes the diagonal contains \eqn{\pi_i}{pi_i}, but
#' for PMR it contains \eqn{E(n_i^2) \neq E(n_i)}{E(n_i^2) != E(n_i)} when units receive multiple
#' hits. Chromy (2009) recommends the Hansen-Hurwitz approximation
#' for variance estimation, which is the default used by
#' [as_survey_design()].
#'
#' ## Limitations
#'
#' - Requires the original sampling frame. The frame must be unchanged
#'   from what was passed to [execute()].
#' - Units in the frame must be uniquely identifiable within each
#'   stratum/cluster group by their column values.
#' - For designs with certainty selections (\eqn{\pi_i = 1}{pi_i = 1}), the joint
#'   matrix is decomposed: certainty units are separated from the
#'   stochastic part, the joint probabilities for non-certainty units
#'   are computed from the reduced \eqn{\pi}{pi} vector, and the full matrix is
#'   reassembled with \eqn{\pi_{ij} = 1}{pi_ij = 1} for certainty pairs and
#'   \eqn{\pi_{ij} = \pi_j}{pi_ij = pi_j} for certainty x non-certainty pairs.
#'
#' @examples
#' \dontrun{
#' sample <- sampling_design() |>
#'   add_stage() |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = hh_count) |>
#'   add_stage() |>
#'     draw(n = 12) |>
#'   execute(niger_eas, seed = 2025)
#'
#' # Compute joint probabilities for stage 1
#' jip <- joint_inclusion_prob(sample, niger_eas, stage = 1)
#'
#' # Use with survey package for exact variance
#' svy <- as_survey_design(sample, pps = survey::ppsmat(jip[[1]]))
#'
#' # Compute all PPS stages at once
#' jip_all <- joint_inclusion_prob(sample, niger_eas)
#' }
#'
#' @seealso [as_survey_design()] for the default export using Brewer's
#'   approximation, [survey::ppsmat()] for wrapping joint matrices
#'
#' @export
joint_inclusion_prob <- function(x, frame, stage = NULL) {
  if (!inherits(x, "tbl_sample")) {
    cli_abort("{.arg x} must be a {.cls tbl_sample} object.")
  }

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)
  n_stages <- length(stages_executed)

  if (is_null(stage)) {
    stages_requested <- stages_executed
  } else {
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

    if (!(method %in% pps_wor_methods)) {
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
compute_stage_jip <- function(x, frame, design, stage_idx, stages_executed) {
  stage_spec <- design$stages[[stage_idx]]
  draw_spec <- stage_spec$draw_spec
  strata_spec <- stage_spec$strata
  cluster_spec <- stage_spec$clusters

  effective_frame <- prepare_stage_frame(
    x,
    frame,
    design,
    stage_idx,
    stages_executed
  )

  if (!is_null(cluster_spec)) {
    cluster_vars <- cluster_spec$vars
    effective_frame <- effective_frame |>
      distinct(across(all_of(cluster_vars)), .keep_all = TRUE)
  }

  sample_df <- as.data.frame(x)
  if (!is_null(cluster_spec)) {
    sample_df <- sample_df |>
      distinct(across(all_of(cluster_spec$vars)), .keep_all = TRUE)
  }

  if (!is_null(strata_spec)) {
    compute_stratified_jip(
      effective_frame,
      sample_df,
      strata_spec,
      draw_spec,
      cluster_spec
    )
  } else {
    n_target <- resolve_unstratified_n(effective_frame, draw_spec)
    compute_group_jip(
      effective_frame,
      sample_df,
      draw_spec,
      n_target,
      strata_vars = NULL,
      cluster_spec = cluster_spec
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
  cluster_spec
) {
  strata_vars <- strata_spec$vars

  stratum_info <- effective_frame |>
    group_by(across(all_of(strata_vars))) |>
    summarise(.N_h = n(), .groups = "drop")

  stratum_info <- calculate_stratum_sizes(stratum_info, strata_spec, draw_spec)

  groups <- effective_frame |>
    group_by(across(all_of(strata_vars))) |>
    group_split(.keep = TRUE)

  block_matrices <- lapply(groups, function(group_frame) {
    keys <- group_frame |>
      distinct(across(all_of(strata_vars)))
    matched <- inner_join(keys, stratum_info, by = strata_vars)
    n_h <- matched$.n_h[1]

    stratum_draw_spec <- resolve_jip_frac(draw_spec, keys, strata_vars)

    compute_group_jip(
      group_frame,
      sample_df,
      stratum_draw_spec,
      n_h,
      strata_vars,
      cluster_spec
    )
  })

  assemble_block_diagonal(block_matrices)
}

#' Resolve per-stratum frac for joint probability computation
#' @noRd
resolve_jip_frac <- function(draw_spec, keys, strata_vars) {
  if (is.data.frame(draw_spec$frac)) {
    matched <- inner_join(keys, draw_spec$frac, by = strata_vars)
    if (nrow(matched) > 0) {
      draw_spec$frac <- matched$frac[1]
    }
  } else if (
    !is.null(draw_spec$frac) &&
      length(draw_spec$frac) > 1 &&
      !is.null(names(draw_spec$frac))
  ) {
    strata_id <- as.character(keys[[strata_vars[1]]])
    if (strata_id %in% names(draw_spec$frac)) {
      draw_spec$frac <- draw_spec$frac[[strata_id]]
    }
  }
  draw_spec
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

  cli_abort("Cannot determine target sample size for unstratified stage.")
}

#' Prepare the effective frame for a given stage
#'
#' Stage 1: full frame
#' Stage k: frame subsetted to selected clusters from stage k-1
#' @noRd
prepare_stage_frame <- function(x, frame, design, stage_idx, stages_executed) {
  pos <- match(stage_idx, stages_executed)
  if (pos == 1L) {
    return(frame)
  }

  prev_stage_idx <- stages_executed[pos - 1L]
  prev_stage_spec <- design$stages[[prev_stage_idx]]

  if (!is_null(prev_stage_spec$clusters)) {
    cluster_vars <- prev_stage_spec$clusters$vars
    sample_df <- as.data.frame(x)

    selected_clusters <- sample_df |>
      distinct(across(all_of(cluster_vars)))

    frame |>
      semi_join(selected_clusters, by = cluster_vars)
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
  cluster_spec
) {
  method <- draw_spec$method

  sampled_idx <- match_sampled_units(
    group_frame,
    sample_df,
    strata_vars,
    cluster_spec
  )

  if (length(sampled_idx) == 0) {
    return(NULL)
  }

  jip_full <- compute_joint_matrix(group_frame, n_target, draw_spec)
  jip_full[sampled_idx, sampled_idx, drop = FALSE]
}

#' Compute the full population joint inclusion probability matrix
#'
#' For fixed-size methods (brewer, systematic, maxent), computes
#' pi_i via `sondage::inclusion_prob(mos, n)`.
#' For Poisson, reconstructs pi_i from frac and mos to match
#' the formula used in `draw_sample()`.
#' @noRd
compute_joint_matrix <- function(frame, n, draw_spec) {
  method <- draw_spec$method
  mos_var <- draw_spec$mos
  mos_vals <- frame[[mos_var]]
  N <- nrow(frame)

  if (sum(mos_vals) <= 0) {
    cli_abort(c(
      "Cannot compute joint inclusion probabilities: sum of MOS variable {.var {mos_var}} is zero.",
      "i" = "At least one unit must have a positive measure of size."
    ))
  }

  pik <- switch(
    method,
    pps_brewer = ,
    pps_systematic = ,
    pps_maxent = {
      sondage::inclusion_prob(mos_vals, n)
    },
    pps_poisson = {
      frac <- draw_spec$frac %||% (n / N)
      pik_raw <- frac * mos_vals / sum(mos_vals) * N
      pmin(pik_raw, 1)
    },
    cli_abort("No joint probability function for method {.val {method}}")
  )

  # NOTE: pik = 1 causes division by zero in _jip functions; decompose
  cert_tol <- 1 - sqrt(.Machine$double.eps)
  cert_idx <- which(pik >= cert_tol)

  if (length(cert_idx) == 0) {
    return(compute_jip_by_method(pik, method))
  }

  assemble_jip_with_certainty(pik, cert_idx, method)
}

#' Dispatch to the appropriate sondage joint probability function
#' @noRd
compute_jip_by_method <- function(pik, method) {
  switch(
    method,
    pps_brewer = sondage::up_brewer_jip(pik),
    pps_systematic = sondage::up_systematic_jip(pik),
    pps_maxent = sondage::up_maxent_jip(pik),
    pps_poisson = sondage::up_poisson_jip(pik),
    cli_abort("No joint probability function for method {.val {method}}")
  )
}

#' Assemble joint matrix separating certainty from stochastic units
#'
#' Certainty units (pi_i = 1) are always in the sample, so their
#' joint probabilities are known without approximation:
#'   pi_ij = 1        if both i and j are certainty
#'   pi_ij = pi_j     if only i is certainty
#' The stochastic part is computed from the reduced pi vector.
#' @noRd
assemble_jip_with_certainty <- function(pik, cert_idx, method) {
  N <- length(pik)
  non_cert_idx <- setdiff(seq_len(N), cert_idx)

  result <- matrix(0, nrow = N, ncol = N)

  result[cert_idx, cert_idx] <- 1

  if (length(non_cert_idx) > 0) {
    result[cert_idx, non_cert_idx] <- rep(
      pik[non_cert_idx],
      each = length(cert_idx)
    )
    result[non_cert_idx, cert_idx] <- rep(
      pik[non_cert_idx],
      times = length(cert_idx)
    )
  }

  if (length(non_cert_idx) > 1) {
    jip_reduced <- compute_jip_by_method(pik[non_cert_idx], method)
    result[non_cert_idx, non_cert_idx] <- jip_reduced
  } else if (length(non_cert_idx) == 1) {
    result[non_cert_idx, non_cert_idx] <- pik[non_cert_idx]
  }

  result
}

#' Match sampled units to their positions in the frame
#' @noRd
match_sampled_units <- function(
  group_frame,
  sample_df,
  strata_vars,
  cluster_spec
) {
  if (!is_null(cluster_spec)) {
    match_vars <- cluster_spec$vars
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
    cli_abort("No shared columns to match sampled units to frame.")
  }

  n_unique <- nrow(distinct(group_frame, across(all_of(match_vars))))
  if (n_unique != nrow(group_frame)) {
    cli_abort(c(
      "Frame rows are not uniquely identified by columns {.val {match_vars}}.",
      "i" = "Found {nrow(group_frame)} rows but only {n_unique} unique key combinations.",
      "i" = "Ensure the frame has a column (or combination) that uniquely identifies each unit."
    ))
  }

  group_sample <- sample_df
  if (!is_null(strata_vars)) {
    strata_keys <- group_frame |>
      distinct(across(all_of(strata_vars)))
    group_sample <- group_sample |>
      semi_join(strata_keys, by = strata_vars)
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
