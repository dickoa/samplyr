#' Design Effect and Effective Sample Size
#'
#' Re-exported from \pkg{svyplan}. Compute the design effect or effective
#' sample size from sampling weights. Five methods are available: Kish
#' (default), Henry, Spencer, Chen-Rust, and cluster planning.
#'
#' The `tbl_sample` methods extract what they can from the sample:
#' - Weights from `.weight`
#' - Selection probabilities from `.weight_1` (for Spencer)
#' - Stratification and clustering variables from the stored design (for CR)
#'
#' The user only needs to supply column names for variables that are not
#' part of the sampling metadata: `y` (outcome) for all non-Kish methods,
#' and `x_cal` (calibration covariate) for Henry.
#'
#' @param x A numeric weight vector or a `tbl_sample`.
#' @param ... Passed to the svyplan method.
#' @param y <[`data-masking`][dplyr::dplyr_data_masking]> Outcome variable
#'   (column name). Required for Henry, Spencer, and CR methods.
#' @param x_cal <[`data-masking`][dplyr::dplyr_data_masking]> Calibration
#'   covariate (column name). Required for the Henry method.
#' @param method Design effect method. One of `"kish"` (default), `"henry"`,
#'   `"spencer"`, or `"cr"`. See [svyplan::design_effect()] for details.
#'
#' @examples
#' # Kish design effect (default)
#' frame <- data.frame(
#'   id = 1:200,
#'   stratum = rep(c("A", "B"), each = 100),
#'   income = c(rnorm(100, 50, 10), rnorm(100, 80, 15))
#' )
#' samp <- sampling_design() |>
#'   stratify_by(stratum) |>
#'   draw(n = c(A = 10, B = 40)) |>
#'   execute(frame, seed = 1)
#'
#' design_effect(samp)
#' effective_n(samp)
#'
#' # Spencer (selection probabilities extracted automatically)
#' design_effect(samp, y = income, method = "spencer")
#'
#' @seealso [svyplan::design_effect()], [svyplan::effective_n()]
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
design_effect.tbl_sample <- function(x, ..., y = NULL, x_cal = NULL,
                                     method = "kish") {
  w <- x[[".weight"]]
  if (is.null(w)) {
    cli_abort("tbl_sample has no {.field .weight} column.")
  }
  args <- resolve_deff_args(x, y = enquo(y), x_cal = enquo(x_cal),
                            method = method)
  design_effect(w, y = args$y, x_cal = args$x_cal, p = args$p,
                strvar = args$strvar, clvar = args$clvar,
                stages = args$stages, method = method, ...)
}

#' @rdname design_effect
#' @export
effective_n.tbl_sample <- function(x, ..., y = NULL, x_cal = NULL,
                                   method = "kish") {
  w <- x[[".weight"]]
  if (is.null(w)) {
    cli_abort("tbl_sample has no {.field .weight} column.")
  }
  args <- resolve_deff_args(x, y = enquo(y), x_cal = enquo(x_cal),
                            method = method)
  effective_n(w, y = args$y, x_cal = args$x_cal, p = args$p,
              strvar = args$strvar, clvar = args$clvar,
              stages = args$stages, method = method, ...)
}

#' Resolve design_effect arguments from tbl_sample metadata
#' @noRd
resolve_deff_args <- function(x, y, x_cal, method) {
  y_val <- if (!quo_is_null(y)) rlang::eval_tidy(y, data = x) else NULL
  x_cal_val <- if (!quo_is_null(x_cal)) rlang::eval_tidy(x_cal, data = x) else NULL

  p_val <- NULL
  strvar_val <- NULL
  clvar_val <- NULL
  stages_val <- NULL

  if (method == "spencer") {
    w1 <- x[[".weight_1"]]
    if (!is.null(w1)) {
      p_val <- 1 / w1
    }
  }

  if (method == "cr") {
    design <- get_design(x)
    if (!is_null(design)) {
      stage1 <- design$stages[[1L]]
      strata_vars <- stage1$strata$vars
      cluster_vars <- if (!is_null(stage1$clusters)) stage1$clusters$vars else NULL

      if (!is_null(strata_vars)) {
        if (length(strata_vars) == 1L) {
          strvar_val <- x[[strata_vars]]
        } else {
          strvar_val <- interaction(x[strata_vars], drop = TRUE)
        }
        n_strata <- length(unique(strvar_val))
        stages_val <- rep(if (!is_null(cluster_vars)) 2L else 1L, n_strata)
      }

      if (!is_null(cluster_vars)) {
        clvar_val <- x[[cluster_vars[1L]]]
      }
    }
  }

  list(y = y_val, x_cal = x_cal_val, p = p_val,
       strvar = strvar_val, clvar = clvar_val, stages = stages_val)
}

#' Coerce svyplan objects for draw()
#' @noRd
coerce_svyplan_n <- function(n) {
  if (inherits(n, c("svyplan_n", "svyplan_power"))) {
    return(as.integer(n))
  }
  n
}
