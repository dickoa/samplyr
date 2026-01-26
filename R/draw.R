#' Specify Selection Parameters
#'
#' `draw()` specifies how units are selected: sample size, sampling fraction,
#' selection method, and measure of size for PPS sampling. Every stage in a
#' sampling design must end with `draw()`.
#'
#' @param .data A `sampling_design` object (piped from [sampling_design()],
#'   [stratify_by()], or [cluster_by()]).
#' @param n Sample size. Can be:
#'   - A scalar: applies per stratum (if no `alloc`) or as total (if `alloc` specified)
#'   - A named vector: stratum-specific sizes (for single stratification variable)
#'   - A data frame: stratum-specific sizes with stratification columns + `n` column
#' @param frac Sampling fraction. Can be:
#'   - A scalar: same fraction for all strata
#'   - A named vector: stratum-specific fractions
#'   - A data frame: stratum-specific fractions with stratification columns + `frac` column
#'   Only one of `n` or `frac` should be specified.
#' @param min_n Minimum sample size per stratum. When an allocation method
#'   (e.g., Neyman, proportional) would assign fewer than `min_n` units to a
#'   stratum, that stratum receives `min_n` units instead. The excess is
#'   redistributed proportionally among strata that were above `min_n`.
#'   Commonly set to 2 (minimum for variance estimation) or higher for
#'   reliable subgroup estimates. Only applies when stratification with an
#'   allocation method is used. Default is `NULL` (no minimum).
#' @param max_n Maximum sample size per stratum. When an allocation method
#'   would assign more than `max_n` units to a stratum, that stratum is
#'   capped at `max_n` units. The surplus is redistributed proportionally
#'   among strata that were below `max_n`. Useful for capping dominant strata
#'   or managing operational constraints. Only applies when stratification
#'   with an allocation method is used. Default is `NULL` (no maximum).
#' @param method Character string specifying the selection method. One of:
#'
#'   **Equal probability methods:**
#'   - `"srswor"` (default): Simple random sampling without replacement
#'   - `"srswr"`: Simple random sampling with replacement
#'   - `"systematic"`: Systematic (fixed interval) sampling
#'   - `"bernoulli"`: Independent Bernoulli trials (random sample size)
#'
#'
#'   **PPS methods (require `mos`):**
#'   - `"pps_systematic"`: PPS systematic sampling
#'   - `"pps_brewer"`: Generalized Brewer (Tillé) method
#'   - `"pps_maxent"`: Maximum entropy / conditional Poisson
#'   - `"pps_poisson"`: PPS Poisson sampling (random sample size)
#'   - `"pps_multinomial"`: PPS multinomial (with replacement)
#'
#' @param mos <[`data-masking`][dplyr::dplyr_data_masking]> Measure of size
#'   variable for PPS methods. Required for all `pps_*` methods.
#' @param round Rounding method when converting `frac` to sample sizes.
#'   One of:
#'   - `"up"` (default): Round up (ceiling). Matches SAS SURVEYSELECT default.
#'   - `"down"`: Round down (floor).
#'   - `"nearest"`: Round to nearest integer (standard rounding).
#'
#'   This parameter only affects designs using `frac` to specify the sampling
#'   rate. When `n` is specified directly, no rounding occurs.
#'
#' @return A modified `sampling_design` object with selection parameters specified.
#'
#' @details
#' ## Selection Methods
#'
#' ### Equal Probability Methods
#'
#' | Method | Replacement | Sample Size | Notes |
#' |--------|-------------|-------------|-------|
#' | `srswor` | Without | Fixed | Standard SRS |
#' | `srswr` | With | Fixed | Allows duplicates |
#' | `systematic` | Without | Fixed | Periodic selection |
#' | `bernoulli` | Without | Random | Each unit selected independently |
#'
#' ### PPS Methods
#'
#' | Method | Replacement | Sample Size | Notes |
#' |--------|-------------|-------------|-------|
#' | `pps_systematic` | Without | Fixed | Simple, some bias |
#' | `pps_brewer` | Without | Fixed | Fast, π_ij > 0 |
#' | `pps_maxent` | Without | Fixed | Highest entropy, π_ij available |
#' | `pps_poisson` | Without | Random | PPS analog of Bernoulli |
#' | `pps_multinomial` | With | Fixed | Allows duplicates |
#'
#' ## Parameter Requirements
#'
#' | Method | `n` | `frac` | `mos` |
#' |--------|-----|--------|-------|
#' | `srswor` | ✓ | or ✓ | — |
#' | `srswr` | ✓ | or ✓ | — |
#' | `systematic` | ✓ | or ✓ | — |
#' | `bernoulli` | — | ✓ | — |
#' | `pps_systematic` | ✓ | or ✓ | ✓ |
#' | `pps_brewer` | ✓ | or ✓ | ✓ |
#' | `pps_maxent` | ✓ | — | ✓ |
#' | `pps_poisson` | — | ✓ | ✓ |
#' | `pps_multinomial` | ✓ | or ✓ | ✓ |
#'
#' ## Fixed vs Random Sample Size Methods
#'
#' Methods with **fixed sample size** (`srswor`, `srswr`, `systematic`, `pps_systematic`,
#' `pps_brewer`, `pps_maxent`, `pps_multinomial`) accept either `n` or `frac`. When `frac`
#' is provided, the sample size is computed based on the `round` parameter (default: ceiling).
#'
#' Methods with **random sample size** (`bernoulli`, `pps_poisson`) require `frac` only.
#' These methods perform independent selection trials for each unit, so the final sample
#' size is a random variable—not a fixed count. Specifying `n` would be misleading since
#' the method cannot guarantee exactly `n` selections.
#'
#' ## Custom Allocation with Data Frames
#'
#' For stratum-specific sample sizes or rates, pass a data frame to `n` or `frac`.
#' The data frame must contain:
#' - All stratification variable columns (matching those in `stratify_by()`)
#' - An `n` column (for sizes) or `frac` column (for rates)
#'
#' @examples
#' \dontrun{
#' # Simple random sample
#' sampling_design() |>
#'   draw(n = 100) |>
#'   execute(frame, seed = 42)
#'
#' # Systematic sample of 10%
#' sampling_design() |>
#'   draw(frac = 0.10, method = "systematic") |>
#'   execute(frame, seed = 42)
#'
#' # PPS sample
#' sampling_design() |>
#'   cluster_by(school_id) |>
#'   draw(n = 50, method = "pps_brewer", mos = enrollment) |>
#'   execute(school_frame, seed = 42)
#'
#' # Bernoulli sampling (random sample size)
#' sampling_design() |>
#'   draw(frac = 0.05, method = "bernoulli") |>
#'   execute(frame, seed = 42)
#'
#' # Stratified with different sizes per stratum (data frame)
#' sizes_df <- data.frame(
#'   region = c("North", "South", "East", "West"),
#'   n = c(100, 200, 150, 100)
#' )
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = sizes_df) |>
#'   execute(frame, seed = 42)
#'
#' # Stratified with different rates per stratum (named vector)
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(frac = c(North = 0.1, South = 0.2, East = 0.15, West = 0.1)) |>
#'   execute(frame, seed = 42)
#'
#' # Neyman allocation with minimum 2 per stratum (for variance estimation)
#' sampling_design() |>
#'   stratify_by(region, alloc = "neyman", variance = var_df) |>
#'   draw(n = 500, min_n = 2) |>
#'   execute(frame, seed = 42)
#'
#' # Proportional allocation with min and max bounds
#' sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'
#'   draw(n = 1000, min_n = 20, max_n = 300) |>
#'   execute(frame, seed = 42)
#' }
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [stratify_by()] for stratification,
#' [cluster_by()] for clustering,
#' [execute()] for running designs
#'
#' @export
draw <- function(.data, n = NULL, frac = NULL, min_n = NULL, max_n = NULL,
                 method = "srswor", mos = NULL, round = "up") {
  if (!is_sampling_design(.data)) {
    cli_abort("{.arg .data} must be a {.cls sampling_design} object")
  }

  mos_quo <- enquo(mos)
  mos_name <- if (quo_is_null(mos_quo)) NULL else as_label(mos_quo)

  valid_methods <- c(
    "srswor", "srswr", "systematic", "bernoulli",
    "pps_systematic", "pps_brewer", "pps_maxent", "pps_poisson", "pps_multinomial"
  )

  if (!is_character(method) || length(method) != 1) {
    cli_abort("{.arg method} must be a single character string")
  }

  method <- match.arg(method, valid_methods)

  valid_round <- c("up", "down", "nearest")
  if (!is_character(round) || length(round) != 1) {
    cli_abort("{.arg round} must be a single character string")
  }
  round <- match.arg(round, valid_round)

  current <- .data$current_stage
  if (current < 1 || current > length(.data$stages)) {
    cli_abort("Invalid design state: no current stage")
  }

  current_stage <- .data$stages[[current]]
  has_alloc <- !is_null(current_stage$strata) && !is_null(current_stage$strata$alloc)

  n_is_df <- is.data.frame(n)
  frac_is_df <- is.data.frame(frac)

  if (n_is_df || frac_is_df) {
    strata_vars <- current_stage$strata$vars
    if (is_null(strata_vars)) {
      cli_abort("Data frame for {.arg n} or {.arg frac} requires stratification. Use {.fn stratify_by} first.")
    }
    if (n_is_df) validate_draw_df(n, strata_vars, "n")
    if (frac_is_df) validate_draw_df(frac, strata_vars, "frac")
  }

  validate_draw_args(n, frac, method, mos_name, has_alloc, n_is_df, frac_is_df)
  validate_bounds(min_n, max_n, has_alloc)

  if (!is_null(current_stage$draw_spec)) {
    cli_abort("{.fn draw} already called for this stage. Use {.fn stage} to start a new stage.")
  }

  draw_spec <- new_draw_spec(
    n = n,
    frac = frac,
    method = method,
    mos = mos_name,
    min_n = min_n,
    max_n = max_n,
    round = round
  )

  .data$stages[[current]]$draw_spec <- draw_spec
  .data$validated <- FALSE

  .data
}

#' @noRd
quo_is_null <- function(quo) {
  is_null(rlang::quo_get_expr(quo))
}

#' @noRd
validate_draw_df <- function(df, strata_vars, value_col,
                             call = rlang::caller_env()) {
  if (!is.data.frame(df)) {
    cli_abort(
      "{.arg {value_col}} must be a data frame when providing stratum-specific values",
      call = call
    )
  }

  missing_vars <- setdiff(strata_vars, names(df))
  if (length(missing_vars) > 0) {
    cli_abort(
      c("Data frame for {.arg {value_col}} is missing stratification variable{?s}:",
        "x" = "{.val {missing_vars}}"),
      call = call
    )
  }

  if (!value_col %in% names(df)) {
    cli_abort(
      "Data frame for {.arg {value_col}} must contain a {.val {value_col}} column",
      call = call
    )
  }

  invisible(NULL)
}

#' @noRd
validate_draw_args <- function(n, frac, method, mos, has_alloc, n_is_df, frac_is_df,
                               call = rlang::caller_env()) {
  pps_methods <- c(
    "pps_systematic", "pps_brewer", "pps_maxent", "pps_poisson", "pps_multinomial"
  )
  is_pps <- method %in% pps_methods

  if (is_pps && is_null(mos)) {
    cli_abort("PPS methods require {.arg mos} (measure of size)", call = call)
  }

  if (!is_pps && !is_null(mos)) {
    cli_warn("{.arg mos} is ignored for non-PPS methods")
  }

  if (method == "bernoulli") {
    if (!is_null(n)) {
      cli_abort("{.val bernoulli} sampling requires {.arg frac}, not {.arg n}", call = call)
    }
    if (is_null(frac)) {
      cli_abort("{.val bernoulli} sampling requires {.arg frac}", call = call)
    }
  }

  if (method == "pps_poisson") {
    if (!is_null(n)) {
      cli_abort("{.val pps_poisson} sampling requires {.arg frac}, not {.arg n}", call = call)
    }
    if (is_null(frac)) {
      cli_abort("{.val pps_poisson} sampling requires {.arg frac}", call = call)
    }
  }

  if (method == "pps_maxent") {
    if (!is_null(frac)) {
      cli_abort("{.val pps_maxent} sampling requires {.arg n}, not {.arg frac}", call = call)
    }
    if (is_null(n)) {
      cli_abort("{.val pps_maxent} sampling requires {.arg n}", call = call)
    }
  }

  random_size_methods <- c("bernoulli", "pps_poisson")
  if (!method %in% random_size_methods) {
    if (is_null(n) && is_null(frac)) {
      cli_abort("Specify either {.arg n} or {.arg frac}", call = call)
    }
    if (!is_null(n) && !is_null(frac)) {
      cli_abort("Specify either {.arg n} or {.arg frac}, not both", call = call)
    }
  }

  if (!is_null(n) && !n_is_df) {
    if (!is.numeric(n)) {
      cli_abort("{.arg n} must be numeric or a data frame", call = call)
    }
    if (any(n <= 0)) {
      cli_abort("{.arg n} must be positive", call = call)
    }
    if (any(n != round(n))) {
      cli_abort("{.arg n} must be integer-valued", call = call)
    }
  }

  if (!is_null(frac) && !frac_is_df) {
    if (!is.numeric(frac)) {
      cli_abort("{.arg frac} must be numeric or a data frame", call = call)
    }
    if (any(frac <= 0)) {
      cli_abort("{.arg frac} must be positive", call = call)
    }
    wor_methods <- c(
      "srswor", "systematic", "bernoulli",
      "pps_systematic", "pps_brewer", "pps_maxent", "pps_poisson"
    )
    if (method %in% wor_methods && any(frac > 1)) {
      cli_abort("{.arg frac} cannot exceed 1 for without-replacement methods", call = call)
    }
  }

  invisible(NULL)
}

#' @noRd
validate_bounds <- function(min_n, max_n, has_alloc,
                            call = rlang::caller_env()) {
  if (!is_null(min_n)) {
    if (!is.numeric(min_n) || length(min_n) != 1) {
      cli_abort("{.arg min_n} must be a single positive integer", call = call)
    }
    if (min_n < 1 || min_n != round(min_n)) {
      cli_abort("{.arg min_n} must be a positive integer", call = call)
    }
    if (!has_alloc) {
      cli_warn("{.arg min_n} only applies when an allocation method is specified in {.fn stratify_by}")
    }
  }

  if (!is_null(max_n)) {
    if (!is.numeric(max_n) || length(max_n) != 1) {
      cli_abort("{.arg max_n} must be a single positive integer", call = call)
    }
    if (max_n < 1 || max_n != round(max_n)) {
      cli_abort("{.arg max_n} must be a positive integer", call = call)
    }
    if (!has_alloc) {
      cli_warn("{.arg max_n} only applies when an allocation method is specified in {.fn stratify_by}")
    }
  }

  if (!is_null(min_n) && !is_null(max_n) && min_n > max_n) {
    cli_abort("{.arg min_n} ({min_n}) cannot be greater than {.arg max_n} ({max_n})", call = call)
  }

  invisible(NULL)
}
