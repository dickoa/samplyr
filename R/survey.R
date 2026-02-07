#' Convert a tbl_sample to a survey design object
#'
#' Creates a [survey::svydesign()] object from a `tbl_sample`, using
#' the sampling design metadata (strata, clusters, weights, and
#' finite population corrections) captured during [execute()].
#'
#' @param x A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to [survey::svydesign()].
#'   In particular, you can pass `pps = survey::ppsmat(joint_matrix)`
#'   to supply exact joint inclusion probabilities instead of the
#'   default Brewer approximation (see Details).
#' @param nest If `TRUE`, relabel cluster ids to enforce nesting within
#'   strata. Passed to [survey::svydesign()]. Default is `TRUE`, which
#'   is appropriate for most complex survey designs.
#'
#' @return A `survey.design2` object from the survey package.
#'
#' @details
#' The conversion maps samplyr's design specification to the arguments
#' expected by [survey::svydesign()]:
#'
#' - **Cluster ids**: extracted from `cluster_by()` variables at each stage
#' - **Strata**: extracted from `stratify_by()` variables at the first stage
#' - **Weights**: the `.weight` column (compound weight across all stages)
#' - **FPC**: for equal-probability stages, the population size `.fpc_k`;
#'   for PPS without replacement stages, the per-unit inclusion
#'   probability (1/`.weight_k`)
#'
#' ## Variance estimation for PPS designs
#'
#' For stages using PPS without replacement methods (`pps_brewer`,
#' `pps_systematic`, `pps_maxent`, `pps_poisson`), variance is
#' estimated by default using Brewer's approximation (`pps = "brewer"`
#' in survey's terminology), which approximates the joint inclusion
#' probabilities from the marginal inclusion probabilities. This is
#' the approximation described by Berger (2004) and works well for
#' most PPS designs regardless of the sampling algorithm used.
#'
#' For exact variance estimation, you can compute joint inclusion
#' probabilities using [joint_inclusion_prob()] and pass them via
#' `pps = survey::ppsmat(joint_matrix)`.
#'
#' ## Chromy's sequential PPS method (PMR)
#'
#' `pps_chromy` is classified as a *Probability Minimum Replacement*
#' (PMR) method — neither with-replacement nor without-replacement.
#' Each unit receives exactly \eqn{\lfloor E(n_i) \rfloor} or
#' \eqn{\lfloor E(n_i) \rfloor + 1} hits, where
#' \eqn{E(n_i) = n \cdot \textrm{mos}_i / \sum \textrm{mos}}.
#' When all expected hit counts are below 1, this reduces to WOR;
#' otherwise large units receive multiple hits.
#'
#' For variance estimation, Chromy (2009) recommends the
#' Hansen-Hurwitz (with-replacement) approximation rather than
#' exact pairwise expectations, which he found "quite variable."
#' Chauvet (2019) confirmed this in simulation. Accordingly,
#' `as_survey_design()` treats `pps_chromy` stages like
#' with-replacement stages (no FPC, no pps argument).
#'
#' Note that `survey::ppsmat()` is **not** valid for the general
#' PMR case. The survey package reads \eqn{\pi_i} from the diagonal
#' of the joint matrix, but for PMR the diagonal contains
#' \eqn{E(n_i^2)}, which differs from \eqn{E(n_i)} when units
#' receive multiple hits. The generalized Sen-Yates-Grundy variance
#' requires \eqn{E(n_i) E(n_j) - E(n_i n_j)} as the pairwise
#' weight (Chromy 2009, eq. 5), not \eqn{E(n_i^2) E(n_j^2) - E(n_i n_j)}.
#'
#' For stages using with-replacement methods (`srswr`,
#' `pps_multinomial`), the finite population correction is omitted
#' and the `.draw_k` column (sequential draw index) is used as the
#' sampling unit identifier for Hansen-Hurwitz variance estimation.
#'
#' The `survey` package is required but not imported — it must be
#' installed to use this function.
#'
#' @references
#' Berger, Y.G. (2004). A Simple Variance Estimator for Unequal
#' Probability Sampling Without Replacement. *Journal of Applied
#' Statistics*, 31, 305-315.
#'
#' Brewer, K.R.W. (2002). *Combined Survey Sampling Inference
#' (Weighing Basu's Elephants)*. Chapter 9.
#'
#' Chauvet, G. (2019). Properties of Chromy's sampling procedure.
#' *arXiv:1912.10896*.
#'
#' Chromy, J.R. (2009). Some Generalizations of the Horvitz-Thompson
#' Estimator. *JSM Proceedings, Survey Research Methods Section*.
#'
#' @examples
#' \dontrun{
#' # Stratified sample -> survey design
#' sample <- sampling_design() |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(kenya_health, seed = 42)
#'
#' svy <- as_survey_design(sample)
#' survey::svymean(~score, svy)
#'
#' # Two-stage cluster sample with PPS first stage
#' sample <- sampling_design() |>
#'   stage() |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = hh_count) |>
#'   stage() |>
#'     draw(n = 12) |>
#'   execute(niger_eas, seed = 2025)
#'
#' # Default: Brewer variance approximation
#' svy <- as_survey_design(sample)
#'
#' # Exact: compute joint probabilities from frame (requires sondage)
#' # pik <- inclusion_probabilities(frame, n = 5, mos = hh_count)
#' # joint <- sondage::up_brewer_jip(pik)
#' # svy <- as_survey_design(sample, pps = survey::ppsmat(joint))
#' }
#'
#' @seealso [execute()] for producing tbl_sample objects,
#'   [survey::svydesign()] for the underlying function
#'
#' @export
as_survey_design <- function(x, ...) {
  UseMethod("as_survey_design")
}

#' @rdname as_survey_design
#' @export
as_survey_design.tbl_sample <- function(x, ..., nest = TRUE) {
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a survey design object."
  )

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)

  # Work on a plain data frame (avoids tbl_sample method dispatch issues)
  df <- as.data.frame(x)

  # --- Cluster ids ---
  # For WR/PMR stages, use the draw index (.draw_k) as the sampling unit
  # identifier instead of cluster variables. Each draw is an independent
  # "PSU" for Hansen-Hurwitz variance estimation.
  id_vars <- character(0)
  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    method <- stage_spec$draw_spec$method
    draw_col <- paste0(".draw_", stage_idx)

    if (method %in% multi_hit_methods && draw_col %in% names(df)) {
      id_vars <- c(id_vars, draw_col)
    } else if (!is_null(stage_spec$clusters)) {
      id_vars <- c(id_vars, stage_spec$clusters$vars)
    }
  }

  ids_formula <- if (length(id_vars) == 0) {
    stats::as.formula("~1")
  } else {
    stats::as.formula(paste("~", paste(id_vars, collapse = " + ")))
  }

  # --- Strata (first stage only) ---
  first_stage <- design$stages[[stages_executed[1]]]
  strata_formula <- if (!is_null(first_stage$strata)) {
    stats::as.formula(
      paste("~", paste(first_stage$strata$vars, collapse = " + "))
    )
  } else {
    NULL
  }

  # --- FPC and PPS ---
  # survey::svydesign requires the number of FPC columns to match
  # the number of id levels. Only stages that contribute a cluster
  # variable to `ids` produce an FPC column. If no stage has
  # clustering (ids = ~1), the first stage provides element-level FPC.
  #
  # FPC values depend on stage method:
  # - Equal-probability WOR: population size (N_h) from .fpc_k
  # - PPS WOR: inclusion probability (pi_i = 1/.weight_k)
  # - WR/PMR methods: Inf (infinite population = no FPC correction)
  #
  # survey::svydesign requires one FPC column per id level. WR/PMR
  # stages use Inf as population size, which gives a correction factor
  # of (1 - n/Inf) = 1, effectively disabling FPC for those stages
  # while preserving real FPC for WOR stages.
  has_pps_wor <- FALSE
  fpc_vars <- character(0)

  # Determine which stages should contribute FPC columns
  # (stages that contribute an id variable: clusters or WR draws)
  id_stage_indices <- integer(0)
  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    method <- stage_spec$draw_spec$method
    draw_col <- paste0(".draw_", stage_idx)

    if (method %in% multi_hit_methods && draw_col %in% names(df)) {
      id_stage_indices <- c(id_stage_indices, stage_idx)
    } else if (!is_null(stage_spec$clusters)) {
      id_stage_indices <- c(id_stage_indices, stage_idx)
    }
  }

  # If no clustering at any stage, the first stage provides FPC
  fpc_stage_indices <- if (length(id_stage_indices) == 0) {
    stages_executed[1]
  } else {
    id_stage_indices
  }

  for (stage_idx in fpc_stage_indices) {
    stage_spec <- design$stages[[stage_idx]]
    method <- stage_spec$draw_spec$method
    weight_col <- paste0(".weight_", stage_idx)
    fpc_col <- paste0(".fpc_", stage_idx)

    if (method %in% c(wr_methods, pmr_methods)) {
      # WR and PMR methods: Inf population size = no finite population
      # correction, while keeping the FPC column count aligned with
      # the number of id levels (required by survey::svydesign).
      inf_col <- paste0(".fpc_inf_", stage_idx)
      df[[inf_col]] <- Inf
      fpc_vars <- c(fpc_vars, inf_col)
      next
    }

    if (!(fpc_col %in% names(df))) {
      next
    }

    if (method %in% pps_wor_methods) {
      # PPS WOR: survey requires sampling fractions, not population sizes
      has_pps_wor <- TRUE
      fpc_pi_col <- paste0(".fpc_pi_", stage_idx)
      df[[fpc_pi_col]] <- 1 / df[[weight_col]]
      fpc_vars <- c(fpc_vars, fpc_pi_col)
    } else {
      # Equal-probability WOR (srs, systematic, bernoulli): population size
      fpc_vars <- c(fpc_vars, fpc_col)
    }
  }

  fpc_formula <- if (length(fpc_vars) == 0) {
    NULL
  } else {
    stats::as.formula(paste("~", paste(fpc_vars, collapse = " + ")))
  }

  # Use Brewer's variance approximation for PPS WOR designs,
  # unless the user explicitly supplies their own pps argument
  # (e.g. pps = survey::ppsmat(joint_matrix) for exact variance)
  dots <- list(...)
  pps_arg <- if (!is.null(dots$pps)) {
    dots$pps
  } else if (has_pps_wor) {
    "brewer"
  } else {
    FALSE
  }

  # Remove pps from dots if present (we pass it explicitly)
  dots$pps <- NULL

  do.call(
    survey::svydesign,
    c(
      list(
        ids = ids_formula,
        strata = strata_formula,
        weights = stats::as.formula("~.weight"),
        fpc = fpc_formula,
        data = df,
        nest = nest,
        pps = pps_arg
      ),
      dots
    )
  )
}
