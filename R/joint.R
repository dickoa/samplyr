#' Compute pairwise joint expectations from a sample
#'
#' Reconstructs the second-order design quantities for PPS stages. For
#' without-replacement (WOR) stages, this produces the joint
#' inclusion probabilities \eqn{\pi_{kl}}{pi_kl}. For with-replacement
#' (WR) and PMR stages, this produces the joint expected hits
#' \eqn{E(n_k \cdot n_l)}{E(n_k * n_l)}.
#'
#' Without `frame`, the computation uses the frame digest recorded at
#' execution, so a sample that traveled without its (possibly
#' confidential) frame still yields joint expectations at the method's
#' stated quality. This needs each pool's exact chances, which the
#' digest always holds for cluster stages and constant-chance element
#' stages, but for element stages with varying chances only under
#' `execute(frame_digest = "full")`. A summarized digest refuses rather
#' than approximates, so pass the frame instead. An exact record of
#' approximate targets does not make their probabilities exact. A
#' supplied frame must keep the pools the sample was drawn from, as `frame`
#' describes, and its units must be uniquely identifiable within each
#' stratum/cluster group by their column values.
#'
#' A sample drawn from separately supplied stage registers needs either
#' its intact digest or all of those registers, in the order they were
#' supplied. One frame is refused with `samplyr_error_frame_count`,
#' because a lower-stage register holds no rows for the population an
#' upper stage selected from.
#'
#' @inheritParams as_svydesign
#' @param frame The frame originally passed to [execute()]. A data
#'   frame is the one shared frame. An ordered list of data frames is
#'   the stage registers, one per executed stage, in the order they
#'   were supplied to [execute()]. Either way each frame must contain
#'   the columns its stage sampled on (strata variables, cluster
#'   variables, measure of size). When `NULL` (the default), the
#'   computation uses the frame digest recorded on the sample instead.
#'
#'   The frame is checked against the digest recorded at execution.
#'   Other columns, another row order or other column types are accepted
#'   when every pool keeps the size and selection chances the sample was
#'   drawn with. Rows added or removed, a changed design value or a
#'   missing design column are refused with
#'   `samplyr_error_joint_frame_mismatch`, and so is any difference at all
#'   for a stage whose method depends on row order (`systematic`,
#'   `pps_systematic`, `pps_chromy`, registered methods), whose matrix is
#'   that of the order. Under `frame_digest = "none"` there is nothing to
#'   compare against, and a warning says so.
#' @param ... These dots are for future extensions and must be empty.
#'   `stages` and the arguments after it must be named exactly: the
#'   singular `stage` is reported rather than prefix-matched.
#' @param stages An integer vector of stage numbers to compute, or
#'   `NULL` (default) to compute all PPS stages.
#'   Non-PPS stages produce `NULL` entries in the returned list.
#' @param waves Two wave numbers of a scheduled master, which selects
#'   activation mode: the result describes how those two occasions of the
#'   rotation overlap rather than how the master was selected. Give the same
#'   wave twice for the joint expectation within one wave. `NULL` (default)
#'   keeps the stage behavior. Mutually exclusive with `stages`, `frame`,
#'   `nsim` and `seed`, none of which activation mode uses.
#' @param nsim Positive integer number of simulations used for Chromy's
#'   pairwise expected hits (default 10000), also forwarded to registered
#'   WR `joint_fn`s that explicitly declare an `nsim` formal. Analytic
#'   methods ignore it. Raising it narrows the simulation error, at a cost
#'   linear in `nsim`.
#' @param seed Single integer seeding the simulated methods (default 1),
#'   which analytic methods ignore. The session's random stream is restored
#'   afterwards, so the result is an exact function of the sample, `nsim`
#'   and `seed`, and calling this never moves a later [execute()]. At the
#'   default `nsim` a chromy matrix carries simulation error of a few
#'   percent, so two seeds give slightly different answers, neither more
#'   correct than the other.
#'
#' @return With `waves`, a tibble with one row per block of the frozen
#'   assignment, described in the "Activation mode" section. Otherwise a
#'   named list of length equal to the number of executed stages. Each
#'   element is either:
#'   - For PPS WOR stages: a square matrix of joint inclusion
#'     probabilities \eqn{\pi_{kl}}{pi_kl}, usable with
#'     [survey::ppsmat()].
#'   - For PPS WR/PMR stages (`pps_multinomial`, `pps_chromy`): a
#'     square matrix of joint expected hits
#'     \eqn{E(n_k \cdot n_l)}{E(n_k * n_l)}.
#'   - `NULL` for non-PPS stages (SRS, systematic) or stages not
#'     requested via the `stages` argument.
#'
#'   Rows and columns represent stage-specific sampled units in first
#'   appearance order. At a WR stage, repeated hits of the same
#'   population unit appear once, so dimensions match the number of
#'   distinct sampled units (or clusters). At a later stage below a WR
#'   parent, each parent draw occurrence defines a separate conditional
#'   block, so the same child population identity can appear in more
#'   than one block.
#'
#'   Each matrix is dense and covers every sampled unit of the stage,
#'   across pools as well as within them, so `m` sampled units take
#'   \eqn{8 m^2}{8 m^2} bytes: about 80 MB at 3,200 units, 0.8 GB at 10,000
#'   and 12.8 GB at 40,000. For a large stage, request only the stages
#'   needed with `stages`.
#'
#' @details
#' Each PPS stage's full-population first-order quantities are rebuilt
#' from its method and measure of size, with per-stratum targets (n_h)
#' replayed from the allocation [execute()] used (proportional, Neyman,
#' optimal, etc.), so they match those at sampling time. The matching
#' sondage joint function then gives the submatrix of sampled units.
#'
#' For stratified or conditional (within-cluster) stages, joint
#' quantities are computed independently within each group. Blocks follow
#' their first appearance in the sample, as do units within a block, and
#' cross-block entries are products of the marginal chances. Below a WR
#' parent, pair the matrix with stage-specific identities in this order,
#' not blindly with every sample row when descendants duplicate a
#' selected unit.
#'
#' With certainty selections (\eqn{\pi_i = 1}{pi_i = 1}) in a WOR design,
#' the joint probabilities of the non-certainty units are computed from
#' the reduced \eqn{\pi}{pi} vector, and the full matrix is reassembled
#' with \eqn{\pi_{ij} = 1}{pi_ij = 1} for certainty pairs and
#' \eqn{\pi_{ij} = \pi_j}{pi_ij = pi_j} for certainty x non-certainty
#' pairs.
#'
#' ## Exact vs. approximate computation
#'
#' The accuracy of the returned matrix depends on the sampling method, as
#' the two tables below show. [variance-estimation] covers how a matrix
#' enters a variance estimate.
#'
#' ## WOR methods (\eqn{\pi_{kl}}{pi_kl})
#'
#' | samplyr method     | sondage function              | Quality                            |
#' |--------------------|-------------------------------|------------------------------------|
#' | `pps_cps`          | `joint_inclusion_prob()`      | **Exact** (Aires' formula via C)   |
#' | `pps_sampford`     | `joint_inclusion_prob()`      | **Exact** (Sampford design)        |
#' | `pps_systematic`   | `joint_inclusion_prob()`      | **Exact** (circular-interval overlap) |
#' | `pps_poisson`      | `joint_inclusion_prob()`      | **Exact** (\eqn{\pi_{kl} = \pi_k \pi_l}{pi_kl = pi_k * pi_l}, independent draws) |
#' | `pps_brewer`       | `joint_inclusion_prob()`      | **Approximate**\eqn{^*} (high-entropy) |
#' | `pps_sps`          | `joint_inclusion_prob()`      | **Approximate** (high-entropy) |
#' | `pps_pareto`       | `joint_inclusion_prob()`      | **Approximate** (high-entropy) |
#' | `cube`             | `joint_inclusion_prob()`      | **Approximate** when unconstrained (high-entropy) |
#' | `lpm2`             | unavailable                   | Spatial spreading is not represented |
#' | `scps`             | unavailable                   | Spatial spreading is not represented |
#'
#' Systematic PPS commonly has zero pair probabilities. Its exact matrix
#' describes the design, but a zero pair probability rules out a
#' design-unbiased variance estimator, and near-zero pairs make it unstable.
#'
#' \eqn{^*} Exact recursive formulas for Brewer's joint inclusion
#' probabilities (Brewer 2002, ch. 9) are \eqn{O(N^3)}{O(N^3)},
#' impractical beyond a few hundred units, whereas the high-entropy
#' approximation is \eqn{O(N^2)}{O(N^2)}. It assumes the design is close
#' to the maximum-entropy design with the same marginal \eqn{\pi_i}{pi_i}
#' (Hajek 1964; Brewer and Donadio 2003), which strong ordering,
#' balancing or extreme probabilities can break, so validate variance and
#' interval coverage for the intended design and population. SPS and
#' Pareto also enter it with approximate first-order targets.
#'
#' Bounded cube, LPM2, and SCPS designs are refused, because count
#' constraints and spatial spreading alter pairwise selection beyond the
#' approximation. [variance-estimation] gives their replicate variance.
#'
#' ## WR/PMR methods (\eqn{E(n_k \cdot n_l)}{E(n_k * n_l)})
#'
#' | samplyr method     | sondage function              | Quality                            |
#' |--------------------|-------------------------------|------------------------------------|
#' | `pps_multinomial`  | `joint_expected_hits()`       | **Exact** (analytic: \eqn{n(n-1) p_k p_l + n p_k \mathbf{1}_{k=l}}{n(n-1) p_k p_l + n p_k 1(k=l)}) |
#' | `pps_chromy`       | `joint_expected_hits()`       | **Approximate** (Monte Carlo simulation, 10 000 replicates) |
#'
#' The sequential dependence of `pps_chromy` admits no closed form for
#' \eqn{E(n_k \cdot n_l)}{E(n_k * n_l)}, so sondage estimates it by Monte
#' Carlo simulation under `nsim` and `seed`.
#'
#' ## Activation mode
#'
#' A scheduled master assigned every unit to a panel at its draw and froze
#' the block-by-panel quotas, so how two occasions of the rotation overlap
#' is already determined. `joint_expectation(master, waves = c(t, s))`
#' reads it from the record, with neither the frame nor a simulation.
#'
#' Conditional on the frozen quotas, the panels inside a block of `m`
#' assignment units are an arrangement of that block's labels. Writing
#' \eqn{a_t}{a_t} and \eqn{a_s}{a_s} for the units the panels active at each
#' wave take from the block, and \eqn{a_\cap}{a_and} for those active at both:
#'
#' \deqn{P(i \in W_t) = a_t / m}{P(i in W_t) = a_t / m}
#' \deqn{P(i \in W_t, i \in W_s) = a_\cap / m}{P(i in W_t and W_s) = a_and / m}
#' \deqn{P(i \in W_t, j \in W_s) = \frac{a_t a_s - a_\cap}{m (m - 1)}}{P(i in W_t, j in W_s) = (a_t a_s - a_and) / (m (m - 1))}
#'
#' for units \eqn{i \neq j}{i != j} of one block, and the product of the
#' marginals for units of different blocks, whose arrangements are drawn
#' independently. The within-wave case is the third expression at `t == s`.
#'
#' The result has one row per block and stays small at any sample size,
#' yet every entry of the full matrix is recoverable from it, since within
#' a block the joints take only two values. The conditional covariance
#' kernel is block-diagonal, not the joint-probability matrix: across
#' blocks the joint is \eqn{p_i p_j}{p_i p_j}, generally not zero, while
#' the covariance is zero.
#'
#' `pool`, `stratum`, `class` and `block` identify the block. `units` is
#' \eqn{m}{m}. `take_1`, `take_2` and `take_both` are the three takes.
#' `prob_1` and `prob_2` are the marginals. `joint_same` and `joint_distinct`
#' are the two joint expectations. `has_pair` is `FALSE` for a block of one
#' unit, where no distinct pair exists and `joint_distinct` is `NA` rather
#' than zero.
#'
#' Quotas are frequently unequal, because a pool that is not a multiple of
#' the block size gives one block an extra unit, so they are read from the
#' record rather than derived from the panel count. A certainty block is
#' permanent and takes every unit at every wave, which makes
#' `joint_distinct` exactly one.
#'
#' These are joint expectations of the activation indicators, conditional
#' on the phase-1 units and the frozen quotas, not the unconditional joint
#' inclusion probabilities of the two-phase design, which also carry the
#' master's own pairwise term. How the two combine for a variance of change
#' is not settled here. [as_svydesign()] carries the activation as a second
#' phase for ordinary totals.
#'
#' Activation mode takes a master, not a materialized wave, so neither of
#' the two waves needs to have been materialized.
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
#' # A single-stage stratified Sampford sample, whose joint matrix is exact
#' sample <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 5, method = "pps_sampford", mos = households) |>
#'   execute(bfa_eas, seed = 2025)
#'
#' jip <- joint_expectation(sample, bfa_eas)
#'
#' # survey reads the matrix by row, one row per sampled unit
#' svy <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
#' survey::svytotal(~population, svy)
#'
#' # A sample drawn from one register per stage passes them as a list
#' regions <- dplyr::distinct(bfa_eas, region, .keep_all = TRUE)
#' registers <- sampling_design() |>
#'   add_stage() |>
#'     cluster_by(region) |>
#'     draw(n = 3, method = "pps_brewer", mos = households) |>
#'   add_stage() |>
#'     draw(n = 12) |>
#'   execute(regions, bfa_eas, seed = 2025)
#'
#' jip_registers <- joint_expectation(registers, list(regions, bfa_eas))
#'
#' @examples
#' # Activation mode: how far two occasions of a rotation overlap. Four
#' # panels rotate two at a time, so each is live for two consecutive waves.
#' rotation <- data.frame(
#'   panel = rep(1:4, times = 4),
#'   wave = rep(1:4, each = 4),
#'   active = c(
#'     TRUE, TRUE, FALSE, FALSE,
#'     FALSE, TRUE, TRUE, FALSE,
#'     FALSE, FALSE, TRUE, TRUE,
#'     TRUE, FALSE, FALSE, TRUE
#'   )
#' )
#'
#' master <- sampling_design() |>
#'   draw(n = 40) |>
#'   execute(bfa_eas, seed = 2025, panels = rotation)
#'
#' # Waves 1 and 2 share one panel of the two each activates.
#' joint_expectation(master, waves = c(1, 2))
#'
#' # The same wave twice gives the joint expectation within one wave.
#' joint_expectation(master, waves = c(2, 2))
#'
#' @references
#' High-entropy approximation:
#' \enc{Hájek}{Hajek}, J. (1964). Asymptotic theory of rejective sampling with
#' varying probabilities from a finite population.
#' \emph{Annals of Mathematical Statistics}, 35(4), 1491-1523.
#'
#' Brewer, K.R.W. and Donadio, M.E. (2003). The high entropy variance of the
#' Horvitz-Thompson estimator. \emph{Survey Methodology}, 29(2), 189-196.
#'
#' Exact conditional Poisson (CPS) joint probabilities:
#' Aires, N. (1999). Algorithms to find exact inclusion probabilities for
#' conditional Poisson sampling and Pareto \eqn{\pi}{pi}ps sampling designs.
#' \emph{Methodology and Computing in Applied Probability}, 1(4), 457-469.
#' \doi{10.1023/A:1010091628740}
#'
#' Exact Brewer joint probabilities:
#' Brewer, K.R.W. (2002). \emph{Combined Survey Sampling Inference: Weighing
#' Basu's Elephants}. Arnold, ch. 9.
#'
#' The variance estimator these underlie:
#' Berger, Y.G. (2004). A simple variance estimator for unequal probability
#' sampling without replacement. \emph{Journal of Applied Statistics},
#' 31(3), 305-315.
#'
#' @seealso [as_svydesign()] for the default export using Brewer's
#'   approximation, [survey::ppsmat()] for wrapping joint matrices
#'
#' @family diagnostics
#' @export
joint_expectation <- function(x, frame = NULL, ..., stages = NULL,
                              waves = NULL, nsim = 10000L, seed = 1L) {
  check_keyword_args(enquos(...), c("stages", "waves", "nsim", "seed"))
  if (!inherits(x, "tbl_sample")) {
    cli_abort(
      "{.arg x} must be a {.cls tbl_sample} object.",
      class = "samplyr_error_sample_expected"
    )
  }
  check_single_replicate(x, "joint_expectation")
  check_sample_unmodified(x, "joint_expectation")
  check_weight_contract_joint(x, "joint_expectation")
  check_no_materialized_wave(x, "joint_expectation")

  # Activation mode uses only the frozen assignment record.
  if (!is_null(waves)) {
    check_activation_mode_arguments(
      frame = frame,
      stages = stages,
      nsim_supplied = !missing(nsim),
      seed_supplied = !missing(seed)
    )
    return(activation_joint_expectation(x, waves))
  }

  if (
    length(nsim) != 1L ||
      !is_integerish_numeric(nsim) ||
      nsim < 1 ||
      nsim > .Machine$integer.max
  ) {
    cli_abort(
      "{.arg nsim} must be a single positive integer.",
      class = "samplyr_error_joint_argument"
    )
  }
  nsim <- as.integer(nsim)

  if (
    length(seed) != 1L ||
      !is_integerish_numeric(seed) ||
      is.na(seed) ||
      abs(seed) > .Machine$integer.max
  ) {
    cli_abort(
      "{.arg seed} must be a single integer.",
      class = "samplyr_error_joint_argument"
    )
  }
  seed <- as.integer(seed)

  digest <- NULL
  if (is_null(frame)) {
    digest <- get_frame_digest(x)
    if (is_null(digest)) {
      abort_samplyr(
        c(
          "No {.arg frame} was supplied and this sample carries no
           frame digest to compute from.",
          "i" = "Pass the original frame, or re-execute with
                 {.code frame_digest = \"summary\"} (the default) to
                 record one."
        ),
        class = "samplyr_error_no_digest"
      )
    }
    if (identical(digest$status, "invalidated")) {
      abort_samplyr(
        c(
          "The frame digest on this sample is invalidated, so the
           frame-free computation would describe a sample that no
           longer exists.",
          "i" = "Pass the original frame instead."
        ),
        class = "samplyr_error_digest_invalidated"
      )
    }
  }

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)
  n_stages <- length(stages_executed)

  frames <- NULL
  if (!is_null(frame)) {
    frames <- normalize_joint_frames(x, frame, stages_executed)
  }

  stages_requested <- if (is_null(stages)) {
    stages_executed
  } else {
    normalize_stage_selector(stages, stages_executed, what = "executed stages")
  }

  # Refuse a frame other than the executed one before any matrix.
  if (!is_null(frames)) {
    check_joint_frame(x, frames, design, stages_executed, stages_requested)
  }

  result <- vector("list", max(stages_executed))
  names(result) <- paste0("stage_", seq_along(result))

  # Chromy and WR methods simulate, so the whole loop is seeded.
  result <- withr::with_seed(seed, {
    for (stage_idx in stages_requested) {
      stage_spec <- design$stages[[stage_idx]]
      draw_spec <- stage_spec$draw_spec
      method <- draw_spec$method

      if (!is_null(draw_spec$bounds) || !is_null(draw_spec$spread)) {
        cli_abort(
          c(
            "Joint inclusion probabilities are unavailable for method {.val {method}} with its declared constraints.",
            "i" = "Controlled count bounds and spatial spreading alter pairwise selection behavior beyond the available approximation."
          ),
          class = "samplyr_error_joint_method_unsupported"
        )
      }

      if (
        !(method %in% jip_methods) &&
          is_null(stage_spec$draw_spec$method_type)
      ) {
        next
      }

      result[[stage_idx]] <- if (is_null(frame)) {
        compute_stage_jip_digest(digest, design, stage_idx, x, nsim)
      } else {
        compute_stage_jip(
          x,
          frames[[match(stage_idx, stages_executed)]],
          design,
          stage_idx,
          stages_executed,
          nsim
        )
      }
    }
    result
  })

  result
}

#' Methods whose joint probabilities depend on the order of the pool
#'
#' Registered custom methods are counted among them, since samplyr cannot
#' know how they use the order.
#' @noRd
joint_order_dependent <- function(draw_spec) {
  draw_spec$method %in% c("systematic", "pps_systematic", "pps_chromy") ||
    !is_null(draw_spec$method_type)
}

#' Refuse a frame that is not the one the sample was drawn from
#'
#' The frame path recomputes every pool from the frame it is given, so a
#' different frame gives the joint probabilities of a selection that did not
#' happen. The digest recorded at execution is the reference.
#'
#' Its fingerprint is taken over the design's columns in row order, so it
#' also changes when rows are reordered or a column changes type, as after a
#' round trip through a file or a database. When it differs, the frame is
#' accepted if every pool keeps its size and its selection chances, which is
#' what an order-free method's matrix depends on. An order-dependent method
#' is refused on any difference: its matrix is that of the order, which
#' neither record can confirm. Without a digest there is nothing to compare
#' against, which is said and not refused.
#' @noRd
check_joint_frame <- function(x, frames, design, stages_executed,
                              stages_requested, call = caller_env()) {
  digest <- get_frame_digest(x)
  if (is_null(digest)) {
    cli_warn(
      c(
        "The frame could not be checked against the one the sample was
         drawn from.",
        "i" = "The sample carries no frame digest
               ({.code frame_digest = \"none\"}). The joint probabilities
               are those of the frame as supplied."
      ),
      class = "samplyr_warning_joint_frame_unverified",
      call = call
    )
    return(invisible(NULL))
  }

  stage_ids <- vapply(digest$stages, function(st) st$stage_id, integer(1))
  refs <- vapply(digest$stages, function(st) st$frame_ref, integer(1))
  for (ref in unique(refs[stage_ids %in% stages_requested])) {
    stages <- intersect(stage_ids[refs == ref], stages_requested)
    frame <- frames[[match(stages[1], stages_executed)]]
    rec <- digest$frames[[ref]]
    role_cols <- unique(rec$roles$column)
    missing <- setdiff(role_cols, names(frame))
    problem <- NULL
    if (length(missing) > 0) {
      problem <- cli::format_inline(
        "Design column{?s} {.val {missing}} {?is/are} missing."
      )
    } else if (!is_null(rec$n_rows) && rec$n_rows != nrow(frame)) {
      problem <- cli::format_inline(
        "It has {nrow(frame)} row{?s} where the sample was drawn from
         {rec$n_rows}."
      )
    } else if (
      !is_null(rec$fingerprint_roles) &&
        !identical(
          rec$fingerprint_roles,
          frame_content_hash(frame, columns = role_cols)
        )
    ) {
      problem <- joint_frame_difference(
        digest, design, frame, ref, stages
      )
    }
    if (!is_null(problem)) {
      abort_samplyr(
        c(
          "This frame is not the one the sample was drawn from.",
          "x" = problem,
          "i" = "Pass the frame {.fn execute} used, in its original row
                 order, or omit {.arg frame} to use the digest. To preview
                 chances on another frame, use
                 {.fn exante_probabilities} or {.fn frame_summary}."
        ),
        class = "samplyr_error_joint_frame_mismatch",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' Why a frame whose fingerprint differs cannot be used, or NULL when it can
#' @noRd
joint_frame_difference <- function(digest, design, frame, ref, stages) {
  if (!identical(ref, 1L)) {
    return(paste(
      "Its design columns differ from the recorded register, and pools of a",
      "separately supplied register cannot be compared."
    ))
  }
  parts <- digest_frame_drift(digest, design, frame, parts = TRUE)
  if (length(parts$pool_diffs) > 0) {
    return("Its pools differ in size from the ones the sample was drawn from.")
  }
  chance <- parts$chance
  if (is_null(chance) || chance$n_compared == 0L) {
    return("Its design columns differ and its pools could not be compared.")
  }
  if (length(chance$diffs) > 0) {
    return(paste(
      "Its selection chances differ from the ones the sample was drawn",
      "with, so a design value changed."
    ))
  }
  ordered <- vapply(
    stages,
    function(i) joint_order_dependent(design$stages[[i]]$draw_spec),
    logical(1)
  )
  if (any(ordered)) {
    return(cli::format_inline(
      "Its design columns differ, and stage{?s} {stages[ordered]} use{?s/}
       an order-dependent method whose joint probabilities are those of the
       row order."
    ))
  }
  NULL
}

#' Resolve the frame each executed stage was drawn from
#'
#' A data frame is the shared frame every stage used. A list is the ordered
#' registers, matched to stages the way `execute()` matched them. The frame
#' mode the sample recorded is what makes a wrong shape reportable: a single
#' lower register offered for a multi-register sample holds no rows for the
#' population an upper stage selected from, and would otherwise produce a
#' plausible matrix computed from the wrong pool.
#' @noRd
normalize_joint_frames <- function(x, frame, stages_executed,
                                   call = caller_env()) {
  record <- get_frame_schedule(x)
  n_stages <- length(stages_executed)

  # Normalize frame shape with the shared grammar.
  supplied <- normalize_frame_input(frame, call = call)
  frame <- if (supplied$n_supplied == 1L) supplied$frames[[1]] else frame

  if (is.data.frame(frame)) {
    if (identical(record$mode, "separate_frames")) {
      abort_samplyr(
        c(
          "This sample was drawn from {record$n_supplied} separately supplied
           frames, so one frame cannot describe it.",
          "x" = "A register for one stage holds no rows for the population an
                 earlier stage selected from.",
          "i" = "Pass the ordered list of the original frames, or omit
                 {.arg frame} to use the recorded frame digest."
        ),
        class = "samplyr_error_frame_count",
        call = call
      )
    }
    return(rep(list(frame), n_stages))
  }

  if (length(frame) != n_stages) {
    abort_samplyr(
      c(
        "This sample executed {n_stages} stage{?s} but {length(frame)}
         frame{?s} {?was/were} supplied.",
        "i" = "Supply one frame covering every stage, or the frames originally
               given to {.fn execute}, in the same order."
      ),
      class = "samplyr_error_frame_count",
      call = call
    )
  }
  frame
}

#' Compute one stage's sampled joint matrix from the frame digest
#'
#' The digest records, per selection pool, the exact resolved chance
#' vector and the selected positions, which is everything the joint
#' computation needs: no frame access, no allocation replay. Pools are
#' independent selections, so cross-pool entries of the stage matrix are
#' products of the marginals. The covariance is block-diagonal over pools.
#' The joint matrix itself is not.
#'
#' Row order matches first appearance in the sample. The sample rows
#' themselves say where each selection appears (the verified
#' sample_row locator for element stages, the selected ancestry keys
#' matched against the sample columns for cluster stages), so blocks
#' and selections within blocks both follow their minimum sample rank.
#'
#' Refuses summarized chance representations rather than turning them
#' into apparently exact joint probabilities.
#' @noRd
compute_stage_jip_digest <- function(
  digest,
  design,
  stage_idx,
  x,
  nsim = 10000L
) {
  rlang::local_error_call(caller_env())
  stage_ids <- vapply(digest$stages, function(s) s$stage_id, integer(1))
  pos <- match(stage_idx, stage_ids)
  if (is.na(pos)) {
    abort_samplyr(
      c(
        "The frame digest does not cover stage {stage_idx}.",
        "i" = "Pass the original {.arg frame} to compute this stage."
      ),
      class = "samplyr_error_digest_no_stage"
    )
  }
  st <- digest$stages[[pos]]
  draw_spec <- design$stages[[stage_idx]]$draw_spec

  if (!st$storage %in% c("units", "constant")) {
    abort_samplyr(
      c(
        "Stage {stage_idx}'s chances were stored as a summarized
         distribution, which cannot yield exact joint
         expectations.",
        "i" = "Re-execute with {.code frame_digest = \"full\"} to keep
               exact per-unit chances, or pass the original
               {.arg frame}."
      ),
      class = "samplyr_error_digest_summarized"
    )
  }

  sel <- st$selected
  if (is_null(sel) || nrow(sel) == 0) {
    return(NULL)
  }
  pools <- st$pools

  # Rank occurrences by sample row, ancestry key, or trace order.
  sel$.rank <- seq_len(nrow(sel))
  if (
    identical(st$unit_level, "element") && "sample_row" %in% names(sel)
  ) {
    sel$.rank <- sel$sample_row
  } else if ("key" %in% names(sel)) {
    sample_df <- as.data.frame(x)
    ancestor_vars <- collect_ancestor_cluster_vars(design, stage_idx)
    key_vars <- unique(c(
      ancestor_vars,
      design$stages[[stage_idx]]$clusters$vars
    ))
    if (all(key_vars %in% names(x))) {
      sample_keys <- digest_path_keys(
        sample_df, seq_len(nrow(sample_df)), key_vars
      )
      prior_stage_ids <- if (stage_idx > 1L) {
        seq_len(stage_idx - 1L)
      } else {
        integer(0)
      }
      wr_ancestor_ids <- prior_stage_ids[vapply(
        prior_stage_ids,
        function(i) is_multi_hit_method(design$stages[[i]]$draw_spec),
        logical(1)
      )]
      prior_draw_cols <- intersect(
        paste0(".draw_", wr_ancestor_ids), names(sample_df)
      )
      block_vars <- unique(c(ancestor_vars, prior_draw_cols, st$strata))
      sample_groups <- if (length(block_vars) > 0) {
        split_row_indices(sample_df, block_vars)$indices
      } else {
        list(seq_len(nrow(sample_df)))
      }
      trace_pools <- unique(sel$pool_id)
      group_first_rows <- first_row_indices_by_group(sample_groups)
      available_groups <- rep(TRUE, length(sample_groups))
      matched_pool <- FALSE

      for (i in seq_along(trace_pools)) {
        pid <- trace_pools[[i]]
        sel_rows <- which(sel$pool_id == pid)
        pool_keys <- unique(sel$key[sel_rows])
        candidates <- which(vapply(
          sample_groups,
          function(rows) all(pool_keys %in% sample_keys[rows]),
          logical(1)
        ))
        pool_pos <- match(pid, pools$pool_id)
        if (
          length(candidates) > 0 &&
            "parent_occurrence" %in% names(pools) &&
            length(prior_draw_cols) > 0 &&
            !is.na(pools$parent_occurrence[[pool_pos]])
        ) {
          draw_col <- prior_draw_cols[[length(prior_draw_cols)]]
          candidates <- candidates[
            sample_df[[draw_col]][group_first_rows[candidates]] ==
              pools$parent_occurrence[[pool_pos]]
          ]
        }
        for (stratum in st$strata %||% character(0)) {
          candidates <- candidates[
            as.character(sample_df[[stratum]][
              group_first_rows[candidates]
            ]) == as.character(pools[[stratum]][[pool_pos]])
          ]
        }
        unused <- candidates[available_groups[candidates]]
        group_pos <- if (length(unused) > 0) {
          unused[[1]]
        } else if (length(candidates) > 0) {
          candidates[[1]]
        } else {
          NA_integer_
        }
        if (!is.na(group_pos)) {
          available_groups[[group_pos]] <- FALSE
          sample_rows <- sample_groups[[group_pos]]
          rank <- match(sel$key[sel_rows], sample_keys[sample_rows])
          if (!anyNA(rank)) {
            sel$.rank[sel_rows] <- sample_rows[rank]
            matched_pool <- TRUE
          }
        }
      }
      if (!matched_pool && anyDuplicated(sel$key) == 0L) {
        rank <- match(sel$key, sample_keys)
        if (!anyNA(rank)) {
          sel$.rank <- rank
        }
      }
    }
  }

  sel_pools <- unique(sel$pool_id)
  pool_pos <- match(sel_pools, pools$pool_id)
  unavailable <- pools$chance_status[pool_pos] == "unavailable"
  if (any(unavailable)) {
    abort_samplyr(
      c(
        "Stage {stage_idx} has pools whose chances are recorded as
         unavailable.",
        "i" = "Pass the original {.arg frame} to compute this stage."
      ),
      class = "samplyr_error_digest_unavailable"
    )
  }

  pool_min_rank <- vapply(sel_pools, function(pid) {
    min(sel$.rank[sel$pool_id == pid])
  }, numeric(1))
  block_order <- order(pool_min_rank)

  blocks <- lapply(sel_pools[block_order], function(pid) {
    p <- pools[pools$pool_id == pid, , drop = FALSE]
    in_pool <- sel[sel$pool_id == pid, , drop = FALSE]
    in_pool <- in_pool[order(in_pool$.rank), , drop = FALSE]
    if (identical(st$storage, "units")) {
      u <- st$units[st$units$pool_id == pid, , drop = FALSE]
      u <- u[order(u$unit_order), , drop = FALSE]
      pik <- u$chance
      sampled_idx <- match(in_pool$unit_id, u$unit_id)
    } else {
      # Constant element storage uses pool position as unit ID.
      pik <- rep(p$chance, p$N)
      sampled_idx <- in_pool$unit_id
    }
    compute_jip_from_pik(
      pik = pik,
      method = draw_spec$method,
      sampled_idx = sampled_idx,
      n = if (is.na(p$n_target)) NULL else as.integer(p$n_target),
      draw_spec = draw_spec,
      nsim = nsim
    )
  })

  assemble_block_diagonal(blocks)
}

#' Compute joint inclusion probabilities for a single stage
#' @noRd
compute_stage_jip <- function(
  x,
  frame,
  design,
  stage_idx,
  stages_executed,
  nsim = 10000L
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
  stage_pos <- match(stage_idx, stages_executed)
  prior_stage_ids <- if (stage_pos > 1L) {
    stages_executed[seq_len(stage_pos - 1L)]
  } else {
    integer(0)
  }
  wr_ancestor_ids <- prior_stage_ids[vapply(
    prior_stage_ids,
    function(i) is_multi_hit_method(design$stages[[i]]$draw_spec),
    logical(1)
  )]
  prior_draw_cols <- intersect(
    paste0(".draw_", wr_ancestor_ids),
    names(sample_df)
  )

  if (!is_null(cluster_spec)) {
    cluster_vars <- cluster_spec$vars
    frame_dedup_vars <- unique(c(ancestor_vars, cluster_vars))
    frame_keep <- unique(c(
      frame_dedup_vars, strata_vars, draw_spec$mos,
      extract_control_vars(draw_spec$control)
    ))
    effective_frame <- effective_frame |>
      select(all_of(frame_keep)) |>
      distinct(across(all_of(frame_dedup_vars)), .keep_all = TRUE)
    sample_dedup_vars <- unique(c(prior_draw_cols, frame_dedup_vars))
    sample_keep <- unique(c(sample_dedup_vars, strata_vars))
    sample_df <- sample_df |>
      select(all_of(sample_keep)) |>
      distinct(across(all_of(sample_dedup_vars)), .keep_all = TRUE)
  }

  # Later stages condition independently on each parent occurrence.
  ancestor_split <- intersect(
    ancestor_vars, intersect(names(effective_frame), names(sample_df))
  )
  occurrence_split <- unique(c(ancestor_split, prior_draw_cols))
  if (length(occurrence_split) > 0) {
    occurrences <- split_row_indices(sample_df, occurrence_split)
    first_rows <- first_row_indices_by_group(occurrences$indices)

    frame_groups <- NULL
    frame_group_pos <- NULL
    if (length(ancestor_split) > 0) {
      frame_groups <- split_row_indices(effective_frame, ancestor_split)
      occurrence_parent_keys <- make_group_key(
        sample_df[first_rows, , drop = FALSE], ancestor_split
      )
      frame_group_pos <- match(occurrence_parent_keys, frame_groups$keys)
      if (anyNA(frame_group_pos)) {
        cli_abort(
          "Could not match a sampled parent occurrence to the frame.",
          call = NULL,
          class = "samplyr_error_internal"
        )
      }
    }

    blocks <- lapply(seq_along(occurrences$indices), function(i) {
      occurrence_frame <- if (length(ancestor_split) > 0) {
        effective_frame[
          frame_groups$indices[[frame_group_pos[[i]]]], , drop = FALSE
        ]
      } else {
        effective_frame
      }
      compute_stage_jip_pool(
        occurrence_frame,
        sample_df[occurrences$indices[[i]], , drop = FALSE],
        strata_spec,
        draw_spec,
        cluster_spec,
        ancestor_vars,
        nsim
      )
    })
    return(assemble_block_diagonal(blocks))
  }

  compute_stage_jip_pool(
    effective_frame,
    sample_df,
    strata_spec,
    draw_spec,
    cluster_spec,
    ancestor_vars,
    nsim
  )
}

#' Joint matrix of one parent's pool (or the whole stage-1 frame)
#' @noRd
compute_stage_jip_pool <- function(
  effective_frame,
  sample_df,
  strata_spec,
  draw_spec,
  cluster_spec,
  ancestor_vars,
  nsim = 10000L
) {
  if (!is_null(strata_spec)) {
    compute_stratified_jip(
      effective_frame,
      sample_df,
      strata_spec,
      draw_spec,
      cluster_spec,
      ancestor_cluster_vars = ancestor_vars,
      nsim = nsim
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
      ancestor_cluster_vars = ancestor_vars,
      nsim = nsim
    )
  }
}

#' Compute joint probabilities for a stratified stage
#'
#' Replays `calculate_stratum_sizes()` against the frame to recover
#' the exact target n_h per stratum, then computes joint matrices per
#' stratum and assembles them, with cross-stratum entries at the product
#' of the marginals.
#' @noRd
compute_stratified_jip <- function(
  effective_frame,
  sample_df,
  strata_spec,
  draw_spec,
  cluster_spec,
  ancestor_cluster_vars = character(0),
  nsim = 10000L
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

  frame_groups <- split_row_indices(effective_frame, strata_vars)
  sample_groups <- split_row_indices(sample_df, strata_vars)
  frame_group_pos <- match(sample_groups$keys, frame_groups$keys)
  if (anyNA(frame_group_pos)) {
    cli_abort(
      "Could not match a sampled stratum to the frame.",
      call = NULL,
      class = "samplyr_error_internal"
    )
  }

  block_matrices <- lapply(seq_along(sample_groups$indices), function(i) {
    stratum_id <- sample_groups$keys[[i]]
    frame_rows <- frame_groups$indices[[frame_group_pos[[i]]]]
    group_frame <- effective_frame[frame_rows, , drop = FALSE]
    group_sample <- sample_df[sample_groups$indices[[i]], , drop = FALSE]

    n_h <- n_h_lookup[[stratum_id]]
    if (is_null(n_h) || is.na(n_h)) {
      cli_abort(
        "Could not resolve target stratum sample size while computing joint probabilities.",
        call = NULL,
        class = "samplyr_error_internal"
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
      ancestor_cluster_vars = ancestor_cluster_vars,
      nsim = nsim
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
    n_val <- as.integer(draw_spec$n)
    is_wr <- draw_spec$method %in% pps_wr_methods ||
      identical(draw_spec$method_type, "wr")
    return(if (is_wr) n_val else min(n_val, N))
  }

  if (!is_null(draw_spec$frac)) {
    frac <- draw_spec$frac
    if (is.numeric(frac) && length(frac) == 1) {
      return(round_sample_size(N * frac, round_method))
    }
  }

  cli_abort("Cannot determine target sample size for unstratified stage.",
            call = NULL,
    class = "samplyr_error_internal"
  )
}

#' Prepare the effective frame for a given stage
#'
#' Stage 1: the frame as supplied.
#' Stage k: that stage's frame restricted to the units its parent selected,
#' through the same transition `execute()` used. A read-only reconstruction:
#' the linkage is recomputed from the realized sample, nothing is drawn. Going
#' through `link_stage_frame()` is what lets a normalized register work here,
#' since it also carries forward the upper-stage strata the register omits.
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

  # Legacy designs without parent units require one shared frame.
  if (is_null(design$stages[[stages_executed[pos - 1L]]]$clusters)) {
    return(frame)
  }

  link_stage_frame(
    frame, as.data.frame(x), design, stage_idx,
    frame_index = pos, call = NULL
  )$frame
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
  ancestor_cluster_vars = character(0),
  nsim = 10000L
) {
  # Order-dependent methods ran on the pool sorted by `control`.
  perm <- selection_order(group_frame, draw_spec)
  if (!is_null(perm)) {
    group_frame <- group_frame[perm, , drop = FALSE]
  }
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
    sampled_idx = sampled_idx,
    nsim = nsim
  )
}

#' Compute the sampled joint matrix for one group
#'
#' Reconstructs first-order quantities on the full group, then requests
#' sampled-only second-order expectations from `sondage`.
#' @noRd
compute_joint_matrix <- function(
  frame,
  n,
  draw_spec,
  sampled_idx,
  nsim = 10000L
) {
  method <- draw_spec$method
  mos_var <- draw_spec$mos
  N <- nrow(frame)

  if (!is_null(mos_var)) {
    mos_vals <- frame[[mos_var]]
    if (sum(mos_vals) <= 0) {
      cli_abort(c(
        "Cannot compute joint expectations: sum of MOS variable {.var {mos_var}} is zero.",
        "i" = "At least one unit must have a positive measure of size."
      ), call = NULL,
        class = "samplyr_error_mos_zero_sum"
      )
    }
  } else {
    mos_vals <- NULL
  }

  # WR and PMR need no certainty decomposition.
  is_wr <- method %in% pps_wr_methods ||
    identical(draw_spec$method_type, "wr")
  if (is_wr) {
    pik <- sondage::expected_hits(mos_vals, n)
    return(compute_jeh_by_method(pik, n, method, sampled_idx, nsim))
  }

  # The digest's own computation, so the frame path matches selection.
  forced_idx <- NULL
  if (!is_null(draw_spec$certainty_ids)) {
    id_var <- draw_spec$certainty_plan$id_var
    forced_idx <- which(frame[[id_var]] %in% draw_spec$certainty_ids)
  }
  pool_spec <- draw_spec
  pool_spec$n <- as.double(n)
  pik <- tryCatch(
    resolve_pool_chance(pool_spec, mos_vals, N, forced_idx)$chance,
    # Both refusals fire before a sample can exist.
    error = function(e) {
      abort_samplyr(
        c(
          "Internal error: the stage's chances could not be resolved.",
          "x" = conditionMessage(e)
        ),
        class = "samplyr_error_internal",
        call = NULL
      )
    }
  )
  compute_jip_from_pik(
    pik, method, sampled_idx, draw_spec = draw_spec, nsim = nsim
  )
}

#' Compute sampled joint matrix from first-order probabilities/hits
#' @noRd
compute_jip_from_pik <- function(
  pik,
  method,
  sampled_idx,
  n = NULL,
  draw_spec = NULL,
  nsim = 10000L
) {
  sampled_idx <- as.integer(sampled_idx)

  is_wr <- method %in% pps_wr_methods ||
    identical(draw_spec$method_type, "wr")
  if (is_wr) {
    if (is_null(n)) {
      cli_abort(
        "Internal error: {.arg n} must be provided for WR/PMR methods.",
        call = NULL,
        class = "samplyr_error_internal"
      )
    }
    return(compute_jeh_by_method(pik, n, method, sampled_idx, nsim))
  }

  cert_idx <- which(is_certainty_probability(pik))

  if (length(cert_idx) == 0) {
    return(compute_jip_by_method(pik, method, sampled_idx, draw_spec = draw_spec))
  }

  assemble_jip_with_certainty(pik, cert_idx, method, sampled_idx, draw_spec = draw_spec)
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
  unname(sondage::joint_inclusion_prob(design, sampled_only = TRUE))
}

#' Dispatch to sondage::joint_expected_hits for WR/PMR methods
#' @noRd
compute_jeh_by_method <- function(
  pik,
  n,
  method,
  sampled_idx,
  nsim = 10000L
) {
  sondage_name <- sondage_method_name(method)

  sampled_idx <- as.integer(sampled_idx)
  sampled_distinct <- unique(sampled_idx)
  hits <- integer(length(pik))
  hits[sampled_distinct] <- 1L

  design <- structure(
    list(
      sample = sampled_distinct,
      prob = pik / n,
      hits = hits,
      n = as.integer(n),
      N = length(pik),
      method = sondage_name,
      fixed_size = TRUE
    ),
    class = c("unequal_prob", "wr", "sondage_sample")
  )
  jeh_population_order <- unname(
    sondage::joint_expected_hits(
      design, sampled_only = TRUE, nsim = nsim
    )
  )
  population_order <- sort(sampled_distinct)
  first_appearance_order <- match(sampled_distinct, population_order)
  jeh_population_order[
    first_appearance_order,
    first_appearance_order,
    drop = FALSE
  ]
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
              call = NULL,
      class = "samplyr_error_joint_frame_key"
    )
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
      ), call = NULL,
        class = "samplyr_error_joint_frame_key"
      )
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
    ), call = NULL,
      class = "samplyr_error_joint_frame_key"
    )
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
