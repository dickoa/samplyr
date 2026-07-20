#' Design Effect and Effective Sample Size
#'
#' These are the \pkg{svyplan} generics re-exported by samplyr. Samplyr adds
#' `tbl_sample` methods rather than defining competing generics. Compute the
#' design effect (DEFF) or effective sample size from sampling weights. Five
#' methods are available for two use cases:
#'
#' **After data collection (diagnostic):** assess how much precision was
#' lost due to the complex design.
#' - `"kish"` (default): weights only. Quick, outcome-independent summary.
#' - `"henry"`: weights + outcome + calibration covariate. Accounts for
#'   calibration weighting.
#' - `"spencer"`: weights + outcome + selection probabilities. Accounts
#'   for correlation between weights and the outcome.
#' - `"cr"`: weights + outcome + strata/cluster IDs. Full Chen-Rust
#'   decomposition for multistage stratified designs.
#'
#' **Before data collection (planning):** estimate an expected DEFF to
#' inflate a simple-random-sample size calculation.
#' - `"cluster"`: uses homogeneity (`delta`) and mean cluster size
#'   (`psu_size`) to compute DEFF = 1 + (psu_size - 1) * delta. Pass
#'   the result to [svyplan::n_prop()], [svyplan::n_mean()], or other
#'   sizing functions.
#'
#' The `tbl_sample` methods extract what they can from the sample
#' metadata. The user only needs to supply column names for variables
#' that are not part of the sampling metadata.
#' - Weights from `.weight`
#' - Selection probabilities from `1 / .weight` (for Spencer). Spencer
#'   (2000) derives his DEFF at the unit level, so the required
#'   probability is the overall inclusion probability
#'   \eqn{\pi_i = 1/w_i}{pi_i = 1/w_i}, i.e. the product of all per-stage
#'   inclusion probabilities for multi-stage designs. This matches the
#'   convention used by `PracTools::deffS()`.
#' - Stratification and clustering variables from the stored design (for CR)
#'
#' @param x A numeric weight vector, a `tbl_sample`, or `NULL` (for
#'   the `"cluster"` planning method).
#' @param ... Passed to the svyplan method. For `method = "cluster"`,
#'   pass `delta` (measure of homogeneity, scalar or `svyplan_varcomp`)
#'   and `psu_size` (mean cluster size). See [svyplan::design_effect()].
#' @param y <[`data-masking`][dplyr::dplyr_data_masking]> Outcome variable
#'   (column name). Required for Henry, Spencer, and CR methods.
#' @param x_cal <[`data-masking`][dplyr::dplyr_data_masking]> Calibration
#'   covariate (column name). Required for the Henry method.
#' @param method Design effect method. For diagnostic use (with weights):
#'   one of `"kish"` (default), `"henry"`, `"spencer"`, or `"cr"`. For
#'   planning (no weights): `"cluster"`.
#'
#' @return `design_effect()` returns a numeric `svyplan_design_effect` object.
#'   Use [as.double()] for the overall value and [as.data.frame()] for the
#'   Chen-Rust decomposition. `effective_n()` returns a numeric scalar.
#'
#' @examples
#' # Kish design effect (default)
#' set.seed(1207)
#' frame <- data.frame(
#'   id = 1:200,
#'   stratum = rep(c("A", "B"), each = 100),
#'   income = c(rnorm(100, 50, 10), rnorm(100, 80, 15)),
#'   x_cal = runif(200, 0.5, 2)
#' )
#' samp <- sampling_design() |>
#'   stratify_by(stratum) |>
#'   draw(n = c(A = 10, B = 40)) |>
#'   execute(frame, seed = 1213)
#'
#' design_effect(samp)
#' effective_n(samp)
#'
#' # Henry (calibration covariate)
#' design_effect(samp, y = income, x_cal = x_cal, method = "henry")
#'
#' # Spencer (selection probabilities extracted automatically)
#' design_effect(samp, y = income, method = "spencer")
#'
#' # Chen-Rust (strata and clusters extracted from design)
#' design_effect(samp, y = income, method = "cr")
#'
#' # Cluster planning (no sample needed)
#' design_effect(delta = 0.05, psu_size = 25, method = "cluster")
#'
#' @seealso [svyplan::design_effect()], [svyplan::effective_n()],
#'   [svyplan::varcomp()], [svyplan::n_cluster()],
#'   [svyplan::prec_prop()], [svyplan::prec_mean()]
#'
#' @name design_effect
#' @importFrom svyplan design_effect effective_n
#' @export
svyplan::design_effect

#' @rdname design_effect
#' @export
svyplan::effective_n

#' @rdname design_effect
#' @export
design_effect.tbl_sample <- function(
  x,
  ...,
  y = NULL,
  x_cal = NULL,
  method = "kish"
) {
  check_single_replicate(x, "design_effect")
  check_sample_unmodified(x, "design_effect")
  w <- x[[".weight"]]
  if (is.null(w)) {
    cli_abort("tbl_sample has no {.field .weight} column.")
  }
  args <- resolve_deff_args(
    x,
    y = enquo(y),
    x_cal = enquo(x_cal),
    method = method
  )
  design_effect(
    w,
    y = args$y,
    x_cal = args$x_cal,
    prob = args$prob,
    strata_id = args$strata_id,
    cluster_id = args$cluster_id,
    stages = args$stages,
    method = method,
    ...
  )
}

#' @rdname design_effect
#' @export
effective_n.tbl_sample <- function(
  x,
  ...,
  y = NULL,
  x_cal = NULL,
  method = "kish"
) {
  check_single_replicate(x, "effective_n")
  check_sample_unmodified(x, "effective_n")
  w <- x[[".weight"]]
  if (is.null(w)) {
    cli_abort("tbl_sample has no {.field .weight} column.")
  }
  args <- resolve_deff_args(
    x,
    y = enquo(y),
    x_cal = enquo(x_cal),
    method = method
  )
  effective_n(
    w,
    y = args$y,
    x_cal = args$x_cal,
    prob = args$prob,
    strata_id = args$strata_id,
    cluster_id = args$cluster_id,
    stages = args$stages,
    method = method,
    ...
  )
}

#' Resolve design_effect arguments from tbl_sample metadata
#' @noRd
resolve_deff_args <- function(x, y, x_cal, method, call = caller_env()) {
  y_val <- if (!quo_is_null(y)) rlang::eval_tidy(y, data = x) else NULL
  x_cal_val <- if (!quo_is_null(x_cal)) {
    rlang::eval_tidy(x_cal, data = x)
  } else {
    NULL
  }

  prob_val <- NULL
  strata_id_val <- NULL
  cluster_id_val <- NULL
  stages_val <- NULL

  if (method == "spencer") {
    # Spencer (2000) is a unit-level approximation: the probability input
    # is the overall inclusion probability pi_i = 1/w_i, not a stage-1
    # probability. This matches PracTools::deffS().
    w_overall <- x[[".weight"]]
    if (!is.null(w_overall)) {
      prob_val <- 1 / w_overall
    }
  }

  if (method == "cr") {
    design <- get_design(x)
    if (!is_null(design)) {
      stage1 <- design$stages[[1L]]
      strata_vars <- stage1$strata$vars
      cluster_vars <- if (!is_null(stage1$clusters)) {
        stage1$clusters$vars
      } else {
        NULL
      }

      if (!is_null(strata_vars)) {
        if (length(strata_vars) == 1L) {
          strata_id_val <- x[[strata_vars[[1L]]]]
        } else {
          strata_id_val <- make_group_key(x, strata_vars)
        }
        n_strata <- length(unique(strata_id_val))
        stages_val <- rep(if (!is_null(cluster_vars)) 2L else 1L, n_strata)
      }

      if (!is_null(cluster_vars)) {
        if (length(cluster_vars) == 1L) {
          cluster_id_val <- x[[cluster_vars[[1L]]]]
        } else {
          cluster_id_val <- make_group_key(x, cluster_vars)
        }
      }
    }

    if (is.null(strata_id_val) && is.null(cluster_id_val)) {
      cli_abort(
        c(
          "The CR method requires stratification or clustering in the design.",
          i = "Use {.fn design_effect} with {.arg method = \"kish\"} for unstratified, unclustered designs."
        ),
        call = call
      )
    }
  }

  list(
    y = y_val,
    x_cal = x_cal_val,
    prob = prob_val,
    strata_id = strata_id_val,
    cluster_id = cluster_id_val,
    stages = stages_val
  )
}

#' Coerce svyplan objects for draw()
#'
#' Uses svyplan's `as.data.frame()` contract for tabular plans and the
#' design context for multistage ones. A stage is "stage-aware" when it
#' is clustered or is not the first stage. There, cluster plans hand over
#' the value for that stage (PSU count, then per-cluster take) instead
#' of a grand total.
#'
#' - `n_alloc()` results: named per-stratum vector. For stratified
#'   two-stage plans (cluster mode), stage 1 gets `n_psu_int` and
#'   stage 2 gets `psu_size_int`, both named by stratum (the jointly
#'   integerized field design, svyplan >= 0.8.8).
#' - `n_multi()` results with domains: data frame keyed on the domain
#'   columns (requires a matching `stratify_by()`).
#' - `n_cluster()` results: `as.integer()` returns the integerized
#'   field design as a stage vector. Stage-aware contexts take the
#'   value for their stage, a flat single-stage design takes the
#'   product (operational element total). Per-domain plans expose only
#'   continuous stages, which are ceiled.
#' - Other svyplan objects: scalar total via `as.integer()`.
#' @noRd
coerce_svyplan_n <- function(n, stage_index = 1L, clustered = FALSE) {
  stage_aware <- clustered || stage_index > 1L

  if (inherits(n, "svyplan_n") && identical(n$type, "alloc")) {
    detail <- as.data.frame(n)
    if ("n_psu_int" %in% names(detail)) {
      if (stage_index == 1L && !clustered) {
        abort_samplyr(
          c(
            "This svyplan allocation plans PSUs, then elements within them.",
            "i" = "Declare the cluster structure with {.fn cluster_by} at stage 1 (on an EA-level frame, cluster by the EA id)."
          ),
          class = "samplyr_error_svyplan_clustered_plan"
        )
      }
      if (stage_index == 1L) {
        return(stats::setNames(detail$n_psu_int, detail$stratum))
      }
      if (stage_index == 2L) {
        return(stats::setNames(
          as.integer(detail$psu_size_int),
          detail$stratum
        ))
      }
      abort_samplyr(
        c(
          "This svyplan allocation covers 2 stages. The design is at stage {stage_index}.",
          "i" = "Pass an explicit {.arg n} for stages beyond the plan."
        ),
        class = "samplyr_error_svyplan_stage"
      )
    }
    return(stats::setNames(detail$n_int, detail$stratum))
  }

  if (inherits(n, "svyplan_n") && identical(n$type, "multi") &&
      !is_null(n$domains)) {
    tab <- as.data.frame(n)
    out <- tab[, setdiff(names(tab), grep("^\\.", names(tab), value = TRUE)),
               drop = FALSE]
    out$n <- as.integer(ceiling(tab$.n))
    return(out)
  }

  if (inherits(n, "svyplan_cluster")) {
    stage_cols <- c("n_psu", "psu_size", "ssu_size")
    if (!is_null(n$domains)) {
      dom <- as.data.frame(n)
      if (!stage_aware) {
        abort_samplyr(
          c(
            "This svyplan plan allocates per domain and per stage.",
            "i" = "Use it in a design with {.fn stratify_by} on the domain variable{?s} and {.fn cluster_by} at stage 1."
          ),
          class = "samplyr_error_svyplan_domains"
        )
      }
      if (stage_index > n$stages) {
        abort_samplyr(
          "This svyplan plan covers {n$stages} stages. The design is at stage {stage_index}.",
          class = "samplyr_error_svyplan_stage"
        )
      }
      keep <- setdiff(
        names(dom),
        c(stage_cols, grep("^\\.", names(dom), value = TRUE))
      )
      out <- dom[, keep, drop = FALSE]
      out$n <- as.integer(ceiling(dom[[stage_cols[stage_index]]]))
      return(out)
    }
    stages <- as.integer(n)
    if (stage_aware) {
      if (stage_index > length(stages)) {
        abort_samplyr(
          "This svyplan plan covers {length(stages)} stages. The design is at stage {stage_index}.",
          class = "samplyr_error_svyplan_stage"
        )
      }
      return(stages[[stage_index]])
    }
    return(as.integer(prod(stages)))
  }

  if (inherits(n, c("svyplan_n", "svyplan_power"))) {
    return(as.integer(n))
  }
  n
}

#' Variance Components from an Executed Sample
#'
#' Estimate design-based variance components (B, W, delta, k) from a
#' `tbl_sample`, for planning the next round with
#' [svyplan::n_cluster()]. The method extracts everything the
#' estimation needs from the sample's design columns, applying two
#' conventions that are easy to get wrong by hand:
#'
#' - **Within-PSU weights.** The components are weighted by the
#'   product of the per-stage weights below stage 1 (`.weight_2`, or
#'   `.weight_2 * .weight_3`), never the compound `.weight`, whose
#'   stage-1 factor would overstate every cluster size.
#' - **Stage-1 selection shares.** For an unequal-probability first
#'   stage, per-PSU shares are derived from the stage-1 weights and
#'   normalized to sum to 1 over the sampled PSUs (per stratum when
#'   the first stage is stratified). For an equal-probability first
#'   stage the SRS path applies. See the Details of
#'   [svyplan::varcomp()].
#'
#' The decomposition covers the clustered stages plus the sample rows
#' as elements: one clustered stage gives 2-stage components, two give
#' 3-stage. Deeper designs are refused. Estimate the top stages and
#' fold the rest into a design effect. With-replacement (WR/PMR)
#' stages treat each draw as an independent unit, keyed by the
#' `.draw_k` column as in [as_svydesign()]: a cluster hit twice enters
#' the decomposition twice, with its share counted per draw. Two-phase samples are refused:
#' the nested decomposition does not model phase sampling. Certainty
#' PSUs are refused: self-representing PSUs contribute no between-PSU
#' variance and belong in their own stratum, so estimate components on
#' the probability part of the design.
#'
#' Variance-component estimation should use this method on the
#' `tbl_sample` directly, not [as_svydesign()]: the exported design
#' carries the compound weight and encodes stage probabilities in
#' fpc conventions that `varcomp` cannot see.
#'
#' @param x A `tbl_sample` with at least one executed clustered stage.
#' @param ... The outcome as a one-sided formula, e.g.
#'   `varcomp(x, ~y)`, mirroring the survey.design method.
#' @param strata Optional one-sided formula naming a column to
#'   estimate per-stratum components by, when the first stage was not
#'   stratified. Stage-1 design strata are picked up automatically.
#'   Combining them with `strata` is an error, because per-stratum
#'   components crossed with design strata are ambiguous.
#'
#' @return A `svyplan_varcomp` object. Pass it as `delta` to
#'   [svyplan::n_cluster()]. Use [as.data.frame()] to export either the
#'   one-row unstratified components or the per-stratum component table.
#'
#' @examples
#' # Two-stage sample: 8 of 24 clusters by PPS, 3 persons per cluster
#' set.seed(7)
#' frame <- data.frame(
#'   cl = rep(sprintf("c%02d", 1:24), each = 5),
#'   size = rep(rep(c(80, 120, 160, 200), 6), each = 5),
#'   y = rnorm(120) + rep(rnorm(24, sd = 0.4), each = 5)
#' )
#' sam <- sampling_design() |>
#'   add_stage() |> cluster_by(cl) |>
#'   draw(n = 8, method = "pps_brewer", mos = size) |>
#'   add_stage() |> draw(n = 3) |>
#'   execute(frame, seed = 11)
#'
#' vc <- varcomp(sam, ~y)
#' vc
#' as.data.frame(vc)
#'
#' # Feed the components into next-round cluster planning
#' svyplan::n_cluster(stage_cost = c(500, 50), delta = vc,
#'                    budget = 100000)
#'
#' @seealso [svyplan::varcomp()] for the estimator and its
#'   conventions, [svyplan::n_cluster()] for planning with the result
#'
#' @name varcomp.tbl_sample
#' @aliases varcomp
#' @importFrom svyplan varcomp
#' @export
svyplan::varcomp

#' @rdname varcomp.tbl_sample
#' @export
varcomp.tbl_sample <- function(x, ..., strata = NULL) {
  check_single_replicate(x, "varcomp")
  check_sample_unmodified(x, "varcomp")

  metadata <- attr(x, "metadata") %||% list()
  if (!is_null(metadata$prev_phase)) {
    abort_samplyr(
      c(
        "{.fn varcomp} does not support two-phase samples.",
        "i" = "The decomposition assumes nested stages. Phase sampling
               is not a stage. Estimate components on each phase
               separately."
      ),
      class = "samplyr_error_varcomp_two_phase"
    )
  }

  dots <- list(...)
  fml <- if (length(dots) >= 1) dots[[1]] else NULL
  if (!inherits(fml, "formula") || length(fml) != 2L) {
    abort_samplyr(
      "Pass the outcome as a one-sided formula: {.code varcomp(x, ~y)}."
    )
  }
  y_name <- all.vars(fml)
  if (length(y_name) != 1L) {
    abort_samplyr(
      "The outcome formula must reference exactly one variable."
    )
  }
  if (!y_name %in% names(x)) {
    abort_samplyr("Variable {.var {y_name}} not found in the sample.")
  }
  y <- x[[y_name]]

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)
  k1 <- stages_executed[1]

  # Decomposition levels: every executed clustered stage contributes
  # its (ancestor-qualified) cluster key. The sample rows are the
  # elements below them.
  clustered <- stages_executed[vapply(
    stages_executed,
    function(k) !is_null(design$stages[[k]]$clusters),
    logical(1)
  )]
  if (length(clustered) == 0) {
    abort_samplyr(
      c(
        "{.fn varcomp} needs a clustered sample.",
        "i" = "The decomposition estimates between- and within-cluster
               components. This sample has no {.fn cluster_by} stage."
      ),
      class = "samplyr_error_varcomp_unclustered"
    )
  }
  if (length(clustered) > 2) {
    abort_samplyr(
      c(
        "{.fn varcomp} decomposes at most 3 stages (2 clustered stages
         above the elements). This sample has {length(clustered)}.",
        "i" = "Estimate components for the top stages and fold deeper
               stages into a design effect."
      ),
      class = "samplyr_error_varcomp_stages"
    )
  }
  stage_key <- function(k) {
    # Multi-hit (WR/PMR) stages: each draw is an independent unit for
    # Hansen-Hurwitz treatment, so the draw index is the key, exactly
    # as in as_svydesign(). Keying by the cluster variable would merge
    # repeated hits of one cluster: its estimated size doubles while
    # its selection share counts once. The draw index restarts per
    # pool, so qualify it by the stage strata and ancestors.
    draw_col <- paste0(".draw_", k)
    if (is_multi_hit_method(design$stages[[k]]$draw_spec) &&
        draw_col %in% names(x)) {
      vars <- c(
        intersect(collect_ancestor_cluster_vars(design, k), names(x)),
        intersect(design$stages[[k]]$strata$vars, names(x)),
        draw_col
      )
      if (length(vars) == 1L) {
        return(x[[draw_col]])
      }
      return(make_group_key(x, vars))
    }
    vars <- unique(c(
      intersect(collect_ancestor_cluster_vars(design, k), names(x)),
      design$stages[[k]]$clusters$vars
    ))
    if (length(vars) == 1L) x[[vars]] else make_group_key(x, vars)
  }
  stage_ids <- lapply(clustered, stage_key)
  psu_key <- stage_ids[[1]]
  if (!any(duplicated(psu_key))) {
    abort_samplyr(
      c(
        "{.fn varcomp} needs elements below the first-stage clusters.",
        "i" = "Every row is its own cluster here, so within-cluster
               components are undefined."
      ),
      class = "samplyr_error_varcomp_unclustered"
    )
  }

  spec1 <- design$stages[[k1]]$draw_spec
  equal_prob <- spec1$method %in% equal_prob_methods ||
    identical(spec1$method_variance, "srs") ||
    (is_balanced_method(spec1) && is_null(spec1$mos))

  # Certainty PSUs have inclusion chance 1: no between-PSU variance,
  # and no place in the share normalization below. The explicit flag
  # records threshold-based certainty; a realized stage weight near 1
  # also catches chances capped by the PPS probability calculation.
  # Restrict the weight check to unequal-probability WOR stages: a
  # whole-take equal-probability design and a WR expected hit of one
  # are not certainty PSUs for this decomposition.
  cert_col <- paste0(".certainty_", k1)
  explicit_certainty <- cert_col %in% names(x) && any(x[[cert_col]])
  stage1_weight <- x[[paste0(".weight_", k1)]]
  cert_tol <- sqrt(.Machine$double.eps)
  weight_certainty <- !equal_prob && is_wor_method(spec1) && any(
    is.finite(stage1_weight) & abs(stage1_weight - 1) <= cert_tol
  )
  if (explicit_certainty || weight_certainty) {
    abort_samplyr(
      c(
        "The first stage holds certainty selections. {.fn varcomp}
         covers probability PSUs only.",
        "i" = "Self-representing PSUs contribute no between-PSU
               variance and act as their own strata. Estimate the
               components on a design without certainty selections,
               and handle the certainty stratum separately."
      ),
      class = "samplyr_error_varcomp_certainty"
    )
  }

  # Within-PSU weights: the product of the per-stage weights below
  # stage 1. Whole-take rows below a single executed clustered stage
  # are self-weighting within their cluster.
  later <- stages_executed[-1]
  w_within <- if (length(later) == 0) {
    rep(1, nrow(x))
  } else {
    Reduce(`*`, lapply(later, function(k) x[[paste0(".weight_", k)]]))
  }

  # Stage-1 strata (design) or the user's stratification, not both.
  strata_vec <- NULL
  design_strata <- design$stages[[k1]]$strata$vars
  if (!is_null(strata) && !is_null(design_strata)) {
    abort_samplyr(
      c(
        "The first stage is stratified by
         {.var {design_strata}}. A {.arg strata} argument would cross
         two stratifications.",
        "i" = "Design strata are picked up automatically. For other
               splits, derive per-domain samples and estimate each."
      ),
      class = "samplyr_error_varcomp_strata"
    )
  }
  if (!is_null(design_strata)) {
    strata_vec <- if (length(design_strata) == 1L) {
      x[[design_strata]]
    } else {
      make_group_key(x, design_strata)
    }
  } else if (!is_null(strata)) {
    if (!inherits(strata, "formula") || length(all.vars(strata)) != 1L) {
      abort_samplyr(
        "{.arg strata} must be a one-sided formula naming one column."
      )
    }
    s_name <- all.vars(strata)
    if (!s_name %in% names(x)) {
      abort_samplyr("Variable {.var {s_name}} not found in the sample.")
    }
    strata_vec <- x[[s_name]]
  }

  # Stage-1 selection shares. Equal-probability first stages take the
  # SRS path. Unequal-probability first stages derive per-PSU shares
  # from the stage-1 weights (pi proportional to the one-draw
  # probabilities for fixed-size PPS, Hansen-Hurwitz WR, and Poisson
  # designs, so normalized shares are exact without the frame).
  prob <- NULL
  if (!equal_prob) {
    pi1 <- 1 / x[[paste0(".weight_", k1)]]
    spread_in_psu <- tapply(pi1, psu_key, function(v) diff(range(v)))
    if (any(spread_in_psu > 1e-9 * max(pi1))) {
      abort_samplyr(
        "Stage-1 weights vary within a first-stage cluster. The design
         columns are inconsistent."
      )
    }
    first_of_psu <- !duplicated(psu_key)
    psu_pi <- pi1[first_of_psu]
    psu_stratum <- if (is_null(strata_vec)) {
      rep("all", sum(first_of_psu))
    } else {
      as.character(strata_vec)[first_of_psu]
    }
    share <- psu_pi / stats::ave(psu_pi, psu_stratum, FUN = sum)
    prob <- share[match(psu_key, psu_key[first_of_psu])]
  }

  svyplan::varcomp(
    y,
    stage_id = stage_ids,
    prob = prob,
    weights = w_within,
    strata = strata_vec
  )
}
