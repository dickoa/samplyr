#' Design Effect and Effective Sample Size
#'
#' Re-exported from \pkg{svyplan}. Compute the design effect (DEFF) or
#' effective sample size from sampling weights. Five methods are available
#' for two use cases:
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
#' @return For `"kish"`, `"henry"`, `"spencer"`, and `"cluster"`: a
#'   numeric scalar. For `"cr"`: a list with `$strata` (data frame of
#'   per-stratum DEFF values) and `$overall` (numeric scalar).
#'
#' @examples
#' # Kish design effect (default)
#' set.seed(1)
#' frame <- data.frame(
#'   id = 1:200,
#'   stratum = rep(c("A", "B"), each = 100),
#'   income = c(rnorm(100, 50, 10), rnorm(100, 80, 15)),
#'   x_cal = runif(200, 0.5, 2)
#' )
#' samp <- sampling_design() |>
#'   stratify_by(stratum) |>
#'   draw(n = c(A = 10, B = 40)) |>
#'   execute(frame, seed = 1)
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
#' is clustered or is not the first stage; there, cluster plans hand over
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
#'   field design as a stage vector; stage-aware contexts take the
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
          "This svyplan allocation covers 2 stages; the design is at stage {stage_index}.",
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
          "This svyplan plan covers {n$stages} stages; the design is at stage {stage_index}.",
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
          "This svyplan plan covers {length(stages)} stages; the design is at stage {stage_index}.",
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
