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
#' @param method For two-phase samples, the variance method passed to
#'   [survey::twophase()]. One of `"full"`, `"approx"`, or `"simple"`.
#'   This argument is ignored for single-phase samples.
#'
#' @return A `survey.design2` object from the survey package.
#'
#' @details
#' The conversion maps samplyr's design specification to the arguments
#' expected by [survey::svydesign()]:
#'
#' - **Cluster ids** (`ids`): extracted from `cluster_by()` variables at each
#'   stage, assembled into a multi-level formula (e.g., `~ ea_id + hh_id`).
#'   For WR/PMR stages, the `.draw_k` column is used as the sampling unit
#'   identifier instead (each draw is treated as an independent unit for
#'   Hansen--Hurwitz variance estimation).
#' - **Strata** (`strata`): extracted from `stratify_by()` variables at the
#'   **first executed stage only** (see Multi-stage designs below).
#' - **Weights** (`weights`): the `.weight` column -- the compound weight
#'   across all stages (i.e., the product of per-stage weights
#'   \eqn{w = \prod w_k = \prod 1/\pi_k}{w = prod(1/pi_k)}).
#'   This is the inverse of the overall inclusion probability and is the
#'   correct weight for design-based point estimation
#'   (\eqn{\hat{Y} = \sum w_i y_i}{Y-hat = sum(w_i * y_i)}).
#' - **FPC** (`fpc`): a per-stage formula assembled from `.fpc_k` columns.
#'   The encoding depends on the method:
#'   - **Equal-probability WOR**: `.fpc_k` (the stratum population count
#'     \eqn{N_h}) is passed directly. The survey package derives the sampling
#'     fraction as \eqn{f_h = n_h / N_h}.
#'   - **PPS WOR**: `.fpc_k` is \eqn{N_h}, but this is **not** passed
#'     directly. Instead, a derived column \eqn{1 / w_k = \pi_i}{1/w_k = pi_i}
#'     is created and passed, because `survey::svydesign()` interprets FPC
#'     values in \eqn{(0, 1)}{(0,1)} as inclusion probabilities.
#'   - **WR / PMR**: a synthetic column filled with `Inf` is passed. The
#'     survey package interprets this as no finite population correction,
#'     giving Hansen--Hurwitz variance.
#'
#' ## Multi-stage designs
#'
#' For multi-stage designs, `as_svydesign()` maps each stage's
#' cluster variable to a level of the `ids` formula and provides
#' per-stage finite population corrections. Strata are exported from
#' the first stage only, which is consistent with the standard
#' "with-replacement at stage 1" variance approximation used by
#' [survey::svydesign()] (Cochran 1977, ch. 11). Under this
#' approximation, second-stage and deeper stratification affects
#' weights (which are correctly compounded) but does not need to
#' appear in the design object for variance estimation -- the
#' contribution of later stages is captured through the variability
#' of first-stage unit totals.
#'
#' Concretely, for a two-stage stratified-cluster design, the exported
#' call is equivalent to:
#' \preformatted{
#' survey::svydesign(
#'   ids     = ~ ea_id,         # stage-1 clusters
#'   strata  = ~ region,        # stage-1 strata only
#'   weights = ~ .weight,       # product of stage-1 and stage-2 weights
#'   fpc     = ~ .fpc_pi_1 + .fpc_2,  # pi_i for PPS stage, N_h for SRS stage
#'   data    = sample,
#'   nest    = TRUE
#' )
#' }
#'
#' ## Variance estimation for PPS designs
#'
#' For stages using PPS without replacement methods (`pps_brewer`,
#' `pps_systematic`, `pps_cps`, `pps_poisson`, `pps_sps`, `pps_pareto`), variance is
#' estimated by default using Brewer's approximation (`pps = "brewer"`
#' in survey's terminology), which approximates the joint inclusion
#' probabilities from the marginal inclusion probabilities. This is
#' the approximation described by Berger (2004) and works well for
#' most PPS designs regardless of the sampling algorithm used.
#'
#' For exact variance estimation, you can compute joint inclusion
#' probabilities using [joint_expectation()] and pass them via
#' `pps = survey::ppsmat(joint_matrix)`.
#'
#' ## Chromy's sequential PPS method (PMR)
#'
#' `pps_chromy` is classified as a *Probability Minimum Replacement*
#' (PMR) method -- neither with-replacement nor without-replacement.
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
#' `as_svydesign()` treats `pps_chromy` stages like
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
#' ## Certainty stratum (take-all units)
#'
#' For PPS without-replacement stages that use certainty selection
#' (`certainty_size` or `certainty_prop`), units with inclusion
#' probability \eqn{\pi_i = 1}{pi_i = 1} are placed in a separate
#' take-all stratum. This follows the standard practice from
#' Cochran (1977, ch. 11) and Sarndal et al. (1992, ch. 3.5):
#' the take-all stratum contributes zero variance (it is a census)
#' and does not inflate the degrees of freedom for the probability
#' stratum.
#'
#' For stages using with-replacement methods (`srswr`,
#' `pps_multinomial`), the finite population correction is omitted
#' and the `.draw_k` column (sequential draw index) is used as the
#' sampling unit identifier for Hansen-Hurwitz variance estimation.
#'
#' The `survey` package is required but not imported -- it must be
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
#' Cochran, W.G. (1977). *Sampling Techniques*. 3rd edition. Wiley.
#'
#' Sarndal, C.-E., Swensson, B. and Wretman, J. (1992). *Model
#' Assisted Survey Sampling*. Springer.
#'
#' @examples
#' \dontrun{
#' # Stratified sample -> survey design
#' sample <- sampling_design() |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(kenya_health, seed = 42)
#'
#' svy <- as_svydesign(sample)
#' survey::svymean(~beds, svy)
#'
#' # Two-stage cluster sample with PPS first stage
#' sample <- sampling_design() |>
#'   add_stage() |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = hh_count) |>
#'   add_stage() |>
#'     draw(n = 12) |>
#'   execute(niger_eas, seed = 2025)
#'
#' # Default: Brewer variance approximation
#' svy <- as_svydesign(sample)
#'
#' # Exact: compute joint probabilities from frame
#' jip <- joint_expectation(sample, niger_eas, stage = 1)
#' svy_exact <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
#' }
#'
#' @seealso [execute()] for producing tbl_sample objects,
#'   [survey::svydesign()] for the underlying function,
#'   [as_survey_design.tbl_sample] for converting directly to a srvyr `tbl_svy`,
#'   `as_svrepdesign()` for replicate-weight export
#'
#' @export
as_svydesign <- function(x, ...) {
  UseMethod("as_svydesign")
}

#' @noRd
survey_phase_info <- function(sample) {
  metadata <- attr(sample, "metadata")
  prev_phase <- metadata$prev_phase
  has_prev_sample <- is.list(prev_phase) && is_tbl_sample(prev_phase$sample)
  prev_prev <- if (has_prev_sample) attr(prev_phase$sample, "metadata") else NULL
  has_three_phase <- has_prev_sample &&
    is.list(prev_prev) &&
    !is_null(prev_prev$prev_phase)
  is_twophase <- has_prev_sample && !has_three_phase

  list(
    prev_phase = prev_phase,
    has_prev_sample = has_prev_sample,
    has_three_phase = has_three_phase,
    is_twophase = is_twophase
  )
}

#' @noRd
survey_validate_phase_support <- function(
  sample,
  allow_twophase = TRUE,
  fn_name = "as_svydesign",
  call = rlang::caller_env()
) {
  phase_info <- survey_phase_info(sample)

  if (phase_info$has_three_phase) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} only supports up to two-phase samples.",
        "i" = "This sample has more than two phases.",
        "i" = "Convert phases separately or collapse phases before exporting."
      ),
      class = "samplyr_error_survey_multiphase_unsupported",
      call = call
    )
  }

  if (!allow_twophase && phase_info$is_twophase) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} does not support two-phase samples.",
        "i" = "Use {.fn as_svydesign} for two-phase linearization export."
      ),
      class = "samplyr_error_svrep_twophase_unsupported",
      call = call
    )
  }

  phase_info
}

#' @noRd
survey_stage_methods <- function(sample) {
  design <- get_design(sample)
  stages_executed <- get_stages_executed(sample)
  vapply(
    stages_executed,
    function(stage_idx) {
      design$stages[[stage_idx]]$draw_spec$method
    },
    character(1)
  )
}

#' @noRd
survey_id_vars <- function(design, stages_executed, df) {
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
  id_vars
}

#' @noRd
survey_ids_formula <- function(id_vars) {
  if (length(id_vars) == 0) {
    stats::as.formula("~1")
  } else {
    stats::as.formula(paste("~", paste(id_vars, collapse = " + ")))
  }
}

#' @noRd
survey_strata_info <- function(df, design, stages_executed) {
  cert_stratum_col <- NULL
  first_stage_idx <- stages_executed[1]
  first_method <- design$stages[[first_stage_idx]]$draw_spec$method
  cert_col <- paste0(".certainty_", first_stage_idx)

  if (
    first_method %in%
      pps_wor_methods &&
      cert_col %in% names(df) &&
      any(df[[cert_col]])
  ) {
    cert_stratum_col <- ".cert_stratum"
    df[[cert_stratum_col]] <- ifelse(
      df[[cert_col]],
      "certainty",
      "probability"
    )
  }

  first_stage <- design$stages[[stages_executed[1]]]
  user_strata_vars <- if (!is_null(first_stage$strata)) {
    first_stage$strata$vars
  } else {
    character(0)
  }

  strata_vars <- c(user_strata_vars, cert_stratum_col)
  strata_formula <- if (length(strata_vars) == 0) {
    NULL
  } else {
    stats::as.formula(paste("~", paste(strata_vars, collapse = " + ")))
  }

  list(
    df = df,
    formula = strata_formula,
    vars = strata_vars
  )
}

#' @noRd
survey_id_stage_indices <- function(design, stages_executed, df) {
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
  id_stage_indices
}

#' @noRd
survey_fpc_info <- function(df, design, stages_executed, id_stage_indices) {
  has_pps_wor <- FALSE
  fpc_vars <- character(0)

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
      inf_col <- paste0(".fpc_inf_", stage_idx)
      df[[inf_col]] <- Inf
      fpc_vars <- c(fpc_vars, inf_col)
      next
    }

    if (!(fpc_col %in% names(df))) {
      next
    }

    if (method %in% pps_wor_methods) {
      has_pps_wor <- TRUE
      fpc_pi_col <- paste0(".fpc_pi_", stage_idx)
      df[[fpc_pi_col]] <- 1 / df[[weight_col]]
      fpc_vars <- c(fpc_vars, fpc_pi_col)
    } else {
      fpc_vars <- c(fpc_vars, fpc_col)
    }
  }

  fpc_formula <- if (length(fpc_vars) == 0) {
    NULL
  } else {
    stats::as.formula(paste("~", paste(fpc_vars, collapse = " + ")))
  }

  list(
    df = df,
    formula = fpc_formula,
    fpc_vars = fpc_vars,
    has_pps_wor = has_pps_wor
  )
}

#' @rdname as_svydesign
#' @export
as_svydesign.tbl_sample <- function(x, ..., nest = TRUE, method = NULL) {
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a survey design object."
  )

  phase_info <- survey_validate_phase_support(
    x,
    allow_twophase = TRUE,
    fn_name = "as_svydesign"
  )
  prev_phase <- phase_info$prev_phase
  is_twophase <- phase_info$is_twophase

  if (is_twophase) {
    method <- if (is_null(method)) {
      NULL
    } else {
      match.arg(method, c("full", "approx", "simple"))
    }
  } else if (!is_null(method)) {
    cli_abort(
      "{.arg method} is only valid when converting a two-phase sample."
    )
  }

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)

  df <- as.data.frame(x)

  if (is_twophase) {
    phase1 <- prev_phase$sample
    design1 <- prev_phase$design %||% get_design(phase1)
    stages1 <- prev_phase$stages %||% get_stages_executed(phase1)
    df1 <- as.data.frame(phase1)
    df2 <- df
    design2 <- design

    id_vars1 <- survey_id_vars(design1, stages1, df1)
    id_vars2 <- survey_id_vars(design2, stages_executed, df2)
    key_vars <- intersect(id_vars1, id_vars2)

    if (length(key_vars) == 0) {
      cli_abort(
        c(
          "Two-phase conversion requires shared phase identifiers.",
          "i" = "Define a unique identifier available in both phases (e.g. via {.fn cluster_by})."
        )
      )
    }

    key_df <- df1[, key_vars, drop = FALSE]
    if (anyDuplicated(key_df) > 0) {
      cli_abort(
        c(
          "Phase 1 identifiers are not unique.",
          "i" = "Use a unique unit identifier for two-phase conversion."
        )
      )
    }

    ids_formula1 <- survey_ids_formula(id_vars1)
    ids_formula2 <- survey_ids_formula(id_vars2)

    strata1 <- survey_strata_info(df1, design1, stages1)
    df1 <- strata1$df
    strata2 <- survey_strata_info(df2, design2, stages_executed)
    df2 <- strata2$df

    id_stage1 <- survey_id_stage_indices(design1, stages1, df1)
    id_stage2 <- survey_id_stage_indices(design2, stages_executed, df2)

    fpc1 <- survey_fpc_info(df1, design1, stages1, id_stage1)
    df1 <- fpc1$df
    fpc2 <- survey_fpc_info(df2, design2, stages_executed, id_stage2)
    df2 <- fpc2$df

    strata2_extra <- setdiff(strata2$vars, names(df1))
    id_vars2_extra <- setdiff(id_vars2, names(df1))

    fpc2_vars <- fpc2$fpc_vars
    fpc2_vars_renamed <- if (length(fpc2_vars) > 0) {
      sub("^\\.fpc_", ".fpc_phase2_", fpc2_vars)
    } else {
      character(0)
    }
    fpc2_rename_map <- setNames(fpc2_vars_renamed, fpc2_vars)

    phase2_cols_needed <- unique(
      c(
        key_vars,
        id_vars2_extra,
        strata2_extra,
        fpc2_vars,
        setdiff(names(df2), names(df1)),
        ".weight"
      )
    )
    phase2_cols_needed <- intersect(phase2_cols_needed, names(df2))

    df2_join <- df2[, phase2_cols_needed, drop = FALSE]
    if (".weight" %in% names(df2_join)) {
      names(df2_join)[names(df2_join) == ".weight"] <- ".weight_phase2"
    }
    if (length(fpc2_rename_map) > 0) {
      idx <- match(names(fpc2_rename_map), names(df2_join))
      names(df2_join)[idx] <- fpc2_rename_map
    }

    df_combined <- df1 |>
      left_join(df2_join, by = key_vars)

    df_combined$.phase2 <- !is.na(df_combined$.weight_phase2)
    if (!any(df_combined$.phase2)) {
      cli_abort(
        c(
          "Phase 2 rows could not be matched to phase 1 identifiers.",
          "i" = "Ensure a shared unique identifier is present in both phases."
        )
      )
    }
    df_combined$.weight_phase2_cond <- ifelse(
      df_combined$.phase2,
      df_combined$.weight_phase2 / df_combined$.weight,
      NA_real_
    )
    if (any(!is.finite(df_combined$.weight_phase2_cond[df_combined$.phase2]))) {
      cli_abort(
        "Invalid phase 2 conditional weights detected after matching phases."
      )
    }
    df_combined$.prob_1 <- 1 / df_combined$.weight
    df_combined$.prob_2 <- ifelse(
      df_combined$.phase2,
      1 / df_combined$.weight_phase2_cond,
      NA_real_
    )

    dots <- list(...)
    pps_arg <- if (!is_null(dots$pps)) dots$pps else NULL
    dots$pps <- NULL

    fpc2_formula <- if (length(fpc2_vars_renamed) == 0) {
      NULL
    } else {
      stats::as.formula(paste("~", paste(fpc2_vars_renamed, collapse = " + ")))
    }

    use_weights <- !is_null(method) && method %in% c("approx", "simple")
    probs_arg <- if (use_weights) {
      NULL
    } else {
      list(
        stats::as.formula("~.prob_1"),
        stats::as.formula("~.prob_2")
      )
    }
    weights_arg <- if (use_weights) {
      list(
        stats::as.formula("~.weight"),
        stats::as.formula("~.weight_phase2_cond")
      )
    } else {
      NULL
    }

    result <- do.call(
      survey::twophase,
      c(
        list(
          id = list(ids_formula1, ids_formula2),
          strata = list(strata1$formula, strata2$formula),
          probs = probs_arg,
          weights = weights_arg,
          fpc = list(fpc1$formula, fpc2_formula),
          subset = stats::as.formula("~.phase2"),
          data = df_combined,
          method = method,
          pps = pps_arg
        ),
        dots
      )
    )

    result
  } else {
    id_vars <- survey_id_vars(design, stages_executed, df)
    ids_formula <- survey_ids_formula(id_vars)

    strata <- survey_strata_info(df, design, stages_executed)
    df <- strata$df

    id_stage_indices <- survey_id_stage_indices(design, stages_executed, df)
    fpc <- survey_fpc_info(df, design, stages_executed, id_stage_indices)
    df <- fpc$df

    dots <- list(...)
    pps_arg <- if (!is_null(dots$pps)) {
      dots$pps
    } else if (fpc$has_pps_wor) {
      "brewer"
    } else {
      FALSE
    }

    dots$pps <- NULL

    result <- do.call(
      survey::svydesign,
      c(
        list(
          ids = ids_formula,
          strata = strata$formula,
          weights = stats::as.formula("~.weight"),
          fpc = fpc$formula,
          data = df,
          nest = nest,
          pps = pps_arg
        ),
        dots
      )
    )

    # Replace the stored call to avoid inlining the entire data frame,
    # which causes massive output when printing the survey.design object
    result$call <- call(
      "svydesign",
      ids = ids_formula,
      strata = strata$formula,
      weights = stats::as.formula("~.weight"),
      fpc = fpc$formula,
      data = quote(data),
      nest = nest
    )

    result
  }
}

#' Convert a tbl_sample to a replicate-weight survey design
#'
#' Creates a `svyrep.design` object from a `tbl_sample` by first
#' converting to a [survey::svydesign()] object via [as_svydesign()],
#' then converting with [survey::as.svrepdesign()].
#'
#' @param x A `tbl_sample` object produced by [execute()].
#' @param type Replicate method passed to [survey::as.svrepdesign()].
#'   One of `"auto"`, `"JK1"`, `"JKn"`, `"BRR"`, `"bootstrap"`,
#'   `"subbootstrap"`, `"mrbbootstrap"`, or `"Fay"`.
#' @param ... Additional arguments passed to [survey::as.svrepdesign()].
#'
#' @return A `svyrep.design` object from the survey package.
#'
#' @details
#' Replicate conversion currently supports single-phase, non-PPS designs.
#' Two-phase and PPS designs should be exported with [as_svydesign()] and
#' analyzed with linearization-based variance.
#'
#' @examples
#' \dontrun{
#' sample <- sampling_design() |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(kenya_health, seed = 42)
#'
#' rep_svy <- as_svrepdesign(sample, type = "auto")
#' survey::svymean(~beds, rep_svy)
#' }
#'
#' @seealso [as_svydesign()] for linearization export,
#'   [survey::as.svrepdesign()] for the underlying conversion
#'
#' @export
as_svrepdesign <- function(x, ...) {
  UseMethod("as_svrepdesign")
}

#' @rdname as_svrepdesign
#' @export
as_svrepdesign.tbl_sample <- function(
  x,
  ...,
  type = c(
    "auto",
    "JK1",
    "JKn",
    "BRR",
    "bootstrap",
    "subbootstrap",
    "mrbbootstrap",
    "Fay"
  )
) {
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a replicate-weight survey design."
  )

  survey_validate_phase_support(
    x,
    allow_twophase = FALSE,
    fn_name = "as_svrepdesign"
  )

  methods_used <- survey_stage_methods(x)
  pps_used <- unique(methods_used[methods_used %in% pps_methods])
  if (length(pps_used) > 0) {
    abort_samplyr(
      c(
        "{.fn as_svrepdesign} does not currently support PPS designs.",
        "i" = "Found method{?s}: {.val {pps_used}}.",
        "i" = "Use {.fn as_svydesign} for linearization-based variance, or provide external replicate weights to {.fn survey::svrepdesign}."
      ),
      class = "samplyr_error_svrep_pps_unsupported"
    )
  }

  type <- match.arg(type)
  svydesign_obj <- as_svydesign(x)

  tryCatch(
    survey::as.svrepdesign(design = svydesign_obj, type = type, ...),
    error = function(e) {
      abort_samplyr(
        c(
          "{.fn as_svrepdesign} failed to convert this design to replicate weights.",
          "x" = "{conditionMessage(e)}"
        ),
        class = "samplyr_error_svrep_conversion_failed"
      )
    }
  )
}


#' Convert a tbl_sample to a srvyr tbl_svy object
#'
#' Creates a [srvyr::tbl_svy] object from a `tbl_sample` by first
#' converting to a [survey::svydesign()] object via [as_svydesign()],
#' then wrapping with [srvyr::as_survey_design()].
#'
#' This method is registered on the [srvyr::as_survey_design()] generic,
#' so it is available when srvyr is loaded.
#'
#' @param .data A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to [as_svydesign()].
#'
#' @return A `tbl_svy` object from the srvyr package.
#'
#' @examples
#' \dontrun{
#' library(srvyr)
#'
#' sample <- sampling_design() |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(kenya_health, seed = 42)
#'
#' # Returns a tbl_svy for use with srvyr verbs
#' svy <- as_survey_design(sample)
#' svy |>
#'   group_by(facility_type) |>
#'   summarise(mean_beds = survey_mean(beds))
#' }
#'
#' @seealso [as_svydesign()] for converting to a survey.design2 object
#'
#' @exportS3Method srvyr::as_survey_design
as_survey_design.tbl_sample <- function(.data, ...) {
  rlang::check_installed(
    "srvyr",
    reason = "to convert a tbl_sample to a srvyr tbl_svy object."
  )

  survey_validate_phase_support(
    .data,
    allow_twophase = TRUE,
    fn_name = "as_survey_design"
  )

  svydesign_obj <- as_svydesign(.data, ...)
  if (inherits(svydesign_obj, c("twophase", "twophase2"))) {
    srvyr::as_survey_twophase(svydesign_obj)
  } else {
    srvyr::as_survey_design(svydesign_obj)
  }
}

#' Convert a tbl_sample to a srvyr replicate-weight tbl_svy object
#'
#' Creates a [srvyr::tbl_svy] replicate design from a `tbl_sample` by first
#' converting to a `svyrep.design` object via `as_svrepdesign()`,
#' then wrapping with [srvyr::as_survey_rep()].
#'
#' @param .data A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to `as_svrepdesign()`.
#'
#' @return A replicate-weight `tbl_svy` object from the srvyr package.
#'
#' @examples
#' \dontrun{
#' library(srvyr)
#'
#' sample <- sampling_design() |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(kenya_health, seed = 42)
#'
#' rep_tbl <- as_survey_rep(sample, type = "auto")
#' rep_tbl |>
#'   summarise(mean_beds = survey_mean(beds, vartype = "se"))
#' }
#'
#' @seealso `as_svrepdesign()` for survey replicate-weight export
#'
#' @exportS3Method srvyr::as_survey_rep
as_survey_rep.tbl_sample <- function(.data, ...) {
  rlang::check_installed(
    "srvyr",
    reason = "to convert a tbl_sample to a srvyr replicate-weight tbl_svy object."
  )

  rep_design <- as_svrepdesign(.data, ...)
  srvyr::as_survey_rep(rep_design)
}
