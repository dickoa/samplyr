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
#' - Selection probabilities from `.weight_1` (for Spencer)
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
    w1 <- x[[".weight_1"]]
    if (!is.null(w1)) {
      prob_val <- 1 / w1
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
#' For `n_alloc()` results (type = "alloc"), extracts per-stratum allocations
#' as a named integer vector. For other svyplan objects (`n_prop`, `n_mean`,
#' `n_multi`, `power_*`, `n_cluster`), extracts the scalar total via
#' `as.integer()`.
#' @noRd
coerce_svyplan_n <- function(n) {
  if (inherits(n, "svyplan_n") && identical(n$type, "alloc")) {
    detail <- n$detail
    return(stats::setNames(detail$n_int, detail$stratum))
  }
  if (inherits(n, c("svyplan_n", "svyplan_power", "svyplan_cluster"))) {
    return(as.integer(n))
  }
  n
}
