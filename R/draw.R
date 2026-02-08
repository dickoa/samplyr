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
#'   - `"pps_brewer"`: Generalized Brewer (\enc{Tillé}{Tille}) method
#'   - `"pps_maxent"`: Maximum entropy / conditional Poisson
#'   - `"pps_poisson"`: PPS Poisson sampling (random sample size)
#'   - `"pps_multinomial"`: PPS multinomial (with replacement, any hit count)
#'   - `"pps_chromy"`: Chromy's sequential PPS (minimum replacement)
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
#' @param control <[`data-masking`][dplyr::dplyr_data_masking]> Variables for
#'   sorting the frame before selection. Control sorting provides implicit
#'   stratification, which is particularly effective with systematic and
#'   sequential sampling methods. Can be:
#'   - A single variable: `control = region`
#'   - Multiple variables: `control = c(region, district)`
#'   - With [serp()] for serpentine sorting: `control = serp(region, district)`
#'   - With [dplyr::desc()] for descending: `control = c(region, desc(population))`
#'   - Mixed: `control = c(region, serp(district, commune), desc(size))`
#'
#'   When stratification is also specified, control sorting is applied within
#'   each stratum. See the section "Control Sorting" below for details.
#'
#' @param certainty_size For PPS without-replacement methods, units with MOS >= this value
#'   are selected with certainty (probability = 1). Can be:
#'   - A scalar: same threshold for all strata
#'   - A data frame: stratum-specific thresholds with stratification columns
#'     + `certainty_size` column
#'
#'   Certainty units are removed from the frame before probability sampling,
#'   and the remaining sample size is reduced accordingly.
#'   Mutually exclusive with `certainty_prop`.
#'   Equivalent to SAS SURVEYSELECT `CERTSIZE=` option.
#' @param certainty_prop For PPS without-replacement methods, units whose MOS proportion
#'   (MOS_i / sum(MOS)) >= this value are selected with certainty. Can be:
#'   - A scalar between 0 and 1 (exclusive): same threshold for all strata
#'   - A data frame: stratum-specific thresholds with stratification columns
#'     + `certainty_prop` column
#'
#'   Uses iterative selection: after removing certainty units, proportions are
#'   recomputed and the check is repeated until no new units qualify.
#'   Mutually exclusive with `certainty_size`.
#'   Equivalent to SAS SURVEYSELECT `CERTSIZE=P=` option.
#'
#' @param on_empty Behaviour when a random-size method (`bernoulli`,
#'   `pps_poisson`) selects zero units. One of:
#'   - `"warn"` (default): Issue a warning and fall back to SRS of 1 unit.
#'   - `"error"`: Stop with an error.
#'   - `"silent"`: Fall back to SRS of 1 unit without a message.
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
#' | `pps_brewer` | Without | Fixed | Fast, joint prob > 0 |
#' | `pps_maxent` | Without | Fixed | Highest entropy, joint prob available |
#' | `pps_poisson` | Without | Random | PPS analog of Bernoulli |
#' | `pps_multinomial` | With | Fixed | Any hit count, Hansen-Hurwitz |
#' | `pps_chromy` | Min. repl. | Fixed | SAS default PPS_SEQ |
#'
#' ## Parameter Requirements
#'
#' | Method | `n` | `frac` | `mos` |
#' |--------|-----|--------|-------|
#' | `srswor` | Yes | or Yes | -- |
#' | `srswr` | Yes | or Yes | -- |
#' | `systematic` | Yes | or Yes | -- |
#' | `bernoulli` | -- | Yes | -- |
#' | `pps_systematic` | Yes | or Yes | Yes |
#' | `pps_brewer` | Yes | or Yes | Yes |
#' | `pps_maxent` | Yes | -- | Yes |
#' | `pps_poisson` | -- | Yes | Yes |
#' | `pps_multinomial` | Yes | or Yes | Yes |
#' | `pps_chromy` | Yes | or Yes | Yes |
#'
#' ## Fixed vs Random Sample Size Methods
#'
#' Methods with **fixed sample size** (`srswor`, `srswr`, `systematic`, `pps_systematic`,
#' `pps_brewer`, `pps_maxent`, `pps_multinomial`) accept either `n` or `frac`. When `frac`
#' is provided, the sample size is computed based on the `round` parameter (default: ceiling).
#'
#' Methods with **random sample size** (`bernoulli`, `pps_poisson`) require `frac` only.
#' These methods perform independent selection trials for each unit, so the final sample
#' size is a random variable, not a fixed count. Specifying `n` would be misleading since
#' the method cannot guarantee exactly `n` selections.
#'
#' ## Custom Allocation with Data Frames
#'
#' For stratum-specific sample sizes or rates, pass a data frame to `n` or `frac`.
#' The data frame must contain:
#' - All stratification variable columns (matching those in `stratify_by()`)
#' - An `n` column (for sizes) or `frac` column (for rates)
#'
#' ## Certainty Selection
#'
#' In PPS without-replacement sampling, very large units can have theoretical
#' inclusion probabilities exceeding 1. Certainty selection handles this by
#' selecting such units with probability 1 before sampling the remainder.
#' The output includes a `.certainty_k` column (where `k` is the stage number)
#' indicating which units were certainty selections.
#'
#' Certainty selection is only available for WOR PPS methods (`pps_systematic`,
#' `pps_brewer`, `pps_maxent`, `pps_poisson`). With-replacement methods
#' (`pps_multinomial`) and PMR methods (`pps_chromy`) handle large units
#' natively through their hit mechanism.
#'
#' For stratum-specific thresholds, pass a data frame containing:
#' - All stratification variable columns
#' - A `certainty_size` or `certainty_prop` column
#'
#' ## Control Sorting
#'
#' Control sorting orders the sampling frame before selection, providing implicit
#' stratification. This is particularly effective with systematic and sequential
#' methods (`systematic`, `pps_systematic`, `pps_chromy`), where it ensures the
#' sample spreads evenly across the sorted variables.
#'
#' **Serpentine vs Nested Sorting:**
#' - **Nested** (default): Standard ascending sort by each variable in order.
#'   Use `control = c(var1, var2, var3)`.
#' - **Serpentine**: Alternating direction that minimizes "jumps" between
#'   adjacent units. Use `control = serp(var1, var2, var3)`.
#'
#' Serpentine sorting makes nearby observations more similar by reversing
#' direction at each hierarchy level. For geographic hierarchies, this means
#' the last district of region 1 is adjacent to the last district of region 2.
#'
#' **Combining with Explicit Stratification:**
#' When both `stratify_by()` and `control` are used, sorting is applied within
#' each stratum. This allows explicit stratification for variance control
#' combined with implicit stratification for sample spread.
#'
#' @examples
#' # Simple random sample of 100 facilities
#' sampling_design() |>
#'   draw(n = 100) |>
#'   execute(kenya_health, seed = 1)
#'
#' # Systematic sample of 10%
#' sampling_design() |>
#'   draw(frac = 0.10, method = "systematic") |>
#'   execute(kenya_health, seed = 123)
#'
#' # PPS sample of schools using enrollment
#' sampling_design() |>
#'   cluster_by(school_id) |>
#'   draw(n = 50, method = "pps_brewer", mos = enrollment) |>
#'   execute(tanzania_schools, seed = 42)
#'
#' # Bernoulli sampling (random sample size, expected ~5%)
#' sampling_design() |>
#'   draw(frac = 0.05, method = "bernoulli") |>
#'   execute(nigeria_business, seed = 1234)
#'
#' # Stratified with different sizes per stratum (data frame)
#' facility_sizes <- data.frame(
#'   facility_type = c("Clinic", "Dispensary", "Health Centre",
#'                     "Sub-County Hospital", "County Hospital",
#'                     "Referral Hospital", "Maternity Home"),
#'   n = c(30, 40, 35, 25, 20, 10, 15)
#' )
#' sampling_design() |>
#'   stratify_by(facility_type) |>
#'   draw(n = facility_sizes) |>
#'   execute(kenya_health, seed = 123)
#'
#' # Stratified with different rates per stratum (named vector)
#' sampling_design() |>
#'   stratify_by(size_class) |>
#'   draw(frac = c(Micro = 0.01, Small = 0.05, Medium = 0.20, Large = 0.50)) |>
#'   execute(nigeria_business, seed = 42)
#'
#' # Neyman allocation with minimum 2 per stratum (for variance estimation)
#' sampling_design() |>
#'   stratify_by(region, alloc = "neyman", variance = niger_eas_variance) |>
#'   draw(n = 150, min_n = 2) |>
#'   execute(niger_eas, seed = 2026)
#'
#' # Proportional allocation with min and max bounds
#' sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 200, min_n = 10, max_n = 50) |>
#'   execute(niger_eas, seed = 42)
#'
#' # Control sorting with serpentine ordering (implicit stratification)
#' sampling_design() |>
#'   draw(n = 100, method = "systematic",
#'        control = serp(region, department)) |>
#'   execute(niger_eas, seed = 42)
#'
#' # Control sorting with nested (standard) ordering
#' sampling_design() |>
#'   draw(n = 100, method = "systematic",
#'        control = c(region, department)) |>
#'   execute(niger_eas, seed = 42)
#'
#' # Combined explicit stratification with control sorting within strata
#' sampling_design() |>
#'   stratify_by(strata) |>
#'   draw(n = 50, method = "systematic",
#'        control = serp(region, department)) |>
#'   execute(niger_eas, seed = 42)
#'
#' # PPS with certainty selection (absolute threshold)
#' # Large EAs selected with certainty, rest sampled with PPS
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 100, method = "pps_brewer", mos = hh_count,
#'        certainty_size = 500) |>
#'   execute(niger_eas, seed = 42)
#'
#' # PPS with certainty selection (proportional threshold)
#' # EAs with >= 10% of stratum total selected with certainty
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 100, method = "pps_systematic", mos = hh_count,
#'        certainty_prop = 0.10) |>
#'   execute(niger_eas, seed = 42)
#'
#' # Stratum-specific certainty thresholds (data frame)
#' cert_thresholds <- data.frame(
#'   region = c("Agadez", "Diffa", "Dosso", "Maradi",
#'              "Niamey", "Tahoua", "Tillaberi", "Zinder"),
#'   certainty_size = c(1000, 500, 600, 700, 300, 800, 650, 750)
#' )
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 100, method = "pps_brewer", mos = hh_count,
#'        certainty_size = cert_thresholds) |>
#'   execute(niger_eas, seed = 42)
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [stratify_by()] for stratification,
#' [cluster_by()] for clustering,
#' [execute()] for running designs,
#' [serp()] for serpentine sorting
#'
#' @export
draw <- function(
  .data,
  n = NULL,
  frac = NULL,
  min_n = NULL,
  max_n = NULL,
  method = "srswor",
  mos = NULL,
  round = "up",
  control = NULL,
  certainty_size = NULL,
  certainty_prop = NULL,
  on_empty = "warn"
) {
  if (!is_sampling_design(.data)) {
    cli_abort("{.arg .data} must be a {.cls sampling_design} object")
  }

  mos_quo <- enquo(mos)
  mos_name <- if (quo_is_null(mos_quo)) NULL else as_label(mos_quo)

  control_quo <- enquo(control)
  control_quos <- if (quo_is_null(control_quo)) {
    NULL
  } else {
    control_expr <- quo_get_expr(control_quo)
    control_env <- quo_get_env(control_quo)

    if (is_call(control_expr, "c")) {
      lapply(as.list(control_expr)[-1], function(expr) {
        new_quosure(expr, control_env)
      })
    } else {
      list(control_quo)
    }
  }

  valid_methods <- c(
    "srswor",
    "srswr",
    "systematic",
    "bernoulli",
    "pps_systematic",
    "pps_brewer",
    "pps_maxent",
    "pps_poisson",
    "pps_multinomial",
    "pps_chromy"
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
  has_alloc <- !is_null(current_stage$strata) &&
    !is_null(current_stage$strata$alloc)

  strata_vars <- current_stage$strata$vars

  n_is_df <- is.data.frame(n)
  frac_is_df <- is.data.frame(frac)
  certainty_size_is_df <- is.data.frame(certainty_size)
  certainty_prop_is_df <- is.data.frame(certainty_prop)

  if (n_is_df || frac_is_df) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Data frame for {.arg n} or {.arg frac} requires stratification. Use {.fn stratify_by} first."
      )
    }
    if (n_is_df) {
      validate_draw_df(n, strata_vars, "n")
    }
    if (frac_is_df) validate_draw_df(frac, strata_vars, "frac")
  }

  valid_on_empty <- c("warn", "error", "silent")
  if (!is_character(on_empty) || length(on_empty) != 1 ||
      !on_empty %in% valid_on_empty) {
    cli_abort(
      "{.arg on_empty} must be one of {.val {valid_on_empty}}"
    )
  }

  validate_draw_args(n, frac, method, mos_name, has_alloc, n_is_df, frac_is_df,
                     strata_vars = strata_vars)
  validate_bounds(min_n, max_n, has_alloc)
  validate_certainty(
    certainty_size,
    certainty_prop,
    mos_name,
    method,
    strata_vars,
    certainty_size_is_df,
    certainty_prop_is_df
  )

  if (!is_null(current_stage$draw_spec)) {
    cli_abort(
      "{.fn draw} already called for this stage. Use {.fn stage} to start a new stage."
    )
  }

  draw_spec <- new_draw_spec(
    n = n,
    frac = frac,
    method = method,
    mos = mos_name,
    min_n = min_n,
    max_n = max_n,
    round = round,
    control = control_quos,
    certainty_size = certainty_size,
    certainty_prop = certainty_prop,
    on_empty = on_empty
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
validate_draw_df <- function(
  df,
  strata_vars,
  value_col,
  call = rlang::caller_env()
) {
  if (!is.data.frame(df)) {
    cli_abort(
      "{.arg {value_col}} must be a data frame when providing stratum-specific values",
      call = call
    )
  }

  missing_vars <- setdiff(strata_vars, names(df))
  if (length(missing_vars) > 0) {
    cli_abort(
      c(
        "Data frame for {.arg {value_col}} is missing stratification variable{?s}:",
        "x" = "{.val {missing_vars}}"
      ),
      call = call
    )
  }

  if (!value_col %in% names(df)) {
    cli_abort(
      "Data frame for {.arg {value_col}} must contain a {.val {value_col}} column",
      call = call
    )
  }

  key_df <- df[, strata_vars, drop = FALSE]
  if (anyDuplicated(key_df) > 0) {
    cli_abort(
      "Data frame for {.arg {value_col}} has duplicate rows for the same stratum",
      call = call
    )
  }
  invisible(NULL)
}

#' @noRd
validate_draw_args <- function(
  n,
  frac,
  method,
  mos,
  has_alloc,
  n_is_df,
  frac_is_df,
  strata_vars = NULL,
  call = rlang::caller_env()
) {
  is_pps <- method %in% pps_methods

  if (is_pps && is_null(mos)) {
    cli_abort("PPS methods require {.arg mos} (measure of size)", call = call)
  }

  if (!is_pps && !is_null(mos)) {
    cli_warn("{.arg mos} is ignored for non-PPS methods")
  }

  if (method == "bernoulli") {
    if (!is_null(n)) {
      cli_abort(
        "{.val bernoulli} sampling requires {.arg frac}, not {.arg n}",
        call = call
      )
    }
    if (is_null(frac)) {
      cli_abort("{.val bernoulli} sampling requires {.arg frac}", call = call)
    }
  }

  if (method == "pps_poisson") {
    if (!is_null(n)) {
      cli_abort(
        "{.val pps_poisson} sampling requires {.arg frac}, not {.arg n}",
        call = call
      )
    }
    if (is_null(frac)) {
      cli_abort("{.val pps_poisson} sampling requires {.arg frac}", call = call)
    }
  }

  if (method == "pps_maxent") {
    if (!is_null(frac)) {
      cli_abort(
        "{.val pps_maxent} sampling requires {.arg n}, not {.arg frac}",
        call = call
      )
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
    if (length(n) > 1 && (is_null(names(n)) || is_null(strata_vars))) {
      cli_abort(
        "{.arg n} must be a scalar, a named vector, or a data frame",
        call = call
      )
    }
    if (any(n <= 0)) {
      cli_abort("{.arg n} must be positive", call = call)
    }
    if (any(abs(n - round(n)) > sqrt(.Machine$double.eps))) {
      cli_abort("{.arg n} must be integer-valued", call = call)
    }
  }

  if (!is_null(frac) && !frac_is_df) {
    if (!is.numeric(frac)) {
      cli_abort("{.arg frac} must be numeric or a data frame", call = call)
    }
    if (length(frac) > 1 && (is_null(names(frac)) || is_null(strata_vars))) {
      cli_abort(
        "{.arg frac} must be a scalar, a named vector, or a data frame",
        call = call
      )
    }
    if (any(frac <= 0)) {
      cli_abort("{.arg frac} must be positive", call = call)
    }
    wor_methods <- c(
      "srswor",
      "systematic",
      "bernoulli",
      "pps_systematic",
      "pps_brewer",
      "pps_maxent",
      "pps_poisson"
    )
    if (method %in% wor_methods && any(frac > 1)) {
      cli_abort(
        "{.arg frac} cannot exceed 1 for without-replacement methods",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' @noRd
validate_bounds <- function(
  min_n,
  max_n,
  has_alloc,
  call = rlang::caller_env()
) {
  if (!is_null(min_n)) {
    if (!is.numeric(min_n) || length(min_n) != 1) {
      cli_abort("{.arg min_n} must be a single positive integer", call = call)
    }
    if (min_n < 1 || abs(min_n - round(min_n)) > sqrt(.Machine$double.eps)) {
      cli_abort("{.arg min_n} must be a positive integer", call = call)
    }
    if (!has_alloc) {
      cli_warn(
        "{.arg min_n} only applies when an allocation method is specified in {.fn stratify_by}"
      )
    }
  }

  if (!is_null(max_n)) {
    if (!is.numeric(max_n) || length(max_n) != 1) {
      cli_abort("{.arg max_n} must be a single positive integer", call = call)
    }
    if (max_n < 1 || abs(max_n - round(max_n)) > sqrt(.Machine$double.eps)) {
      cli_abort("{.arg max_n} must be a positive integer", call = call)
    }
    if (!has_alloc) {
      cli_warn(
        "{.arg max_n} only applies when an allocation method is specified in {.fn stratify_by}"
      )
    }
  }

  if (!is_null(min_n) && !is_null(max_n) && min_n > max_n) {
    cli_abort(
      "{.arg min_n} ({min_n}) cannot be greater than {.arg max_n} ({max_n})",
      call = call
    )
  }
  invisible(NULL)
}

#' @noRd
validate_certainty <- function(
  certainty_size,
  certainty_prop,
  mos,
  method,
  strata_vars,
  certainty_size_is_df,
  certainty_prop_is_df,
  call = rlang::caller_env()
) {
  if (!is_null(certainty_size) && !is_null(certainty_prop)) {
    cli_abort(
      "Specify only one of {.arg certainty_size} or {.arg certainty_prop}, not both.",
      call = call
    )
  }

  has_certainty <- !is_null(certainty_size) || !is_null(certainty_prop)
  if (!has_certainty) {
    return(invisible(NULL))
  }

  if (is_null(mos)) {
    cli_abort(
      "Certainty selection requires {.arg mos} to be specified.",
      call = call
    )
  }

  pps_wor_methods <- c(
    "pps_systematic", "pps_brewer", "pps_maxent", "pps_poisson"
  )
  if (!method %in% pps_wor_methods) {
    cli_abort(
      c(
        "Certainty selection is only available for PPS without-replacement methods.",
        "i" = "Valid methods: {.val {pps_wor_methods}}",
        "i" = "WR ({.val pps_multinomial}) and PMR ({.val pps_chromy}) methods handle large units natively.",
        "x" = "Current method: {.val {method}}"
      ),
      call = call
    )
  }

  if (certainty_size_is_df) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Data frame for {.arg certainty_size} requires stratification. Use {.fn stratify_by} first.",
        call = call
      )
    }
    validate_draw_df(certainty_size, strata_vars, "certainty_size", call = call)
    vals <- certainty_size$certainty_size
    if (!is.numeric(vals) || any(is.na(vals)) || any(vals <= 0)) {
      cli_abort(
        "{.arg certainty_size} values must be positive numbers.",
        call = call
      )
    }
  } else if (!is_null(certainty_size)) {
    if (
      !is.numeric(certainty_size) ||
        length(certainty_size) != 1 ||
        is.na(certainty_size) ||
        certainty_size <= 0
    ) {
      cli_abort(
        "{.arg certainty_size} must be a single positive number or a data frame.",
        call = call
      )
    }
  }

  if (certainty_prop_is_df) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Data frame for {.arg certainty_prop} requires stratification. Use {.fn stratify_by} first.",
        call = call
      )
    }
    validate_draw_df(certainty_prop, strata_vars, "certainty_prop", call = call)
    vals <- certainty_prop$certainty_prop
    if (
      !is.numeric(vals) || any(is.na(vals)) || any(vals <= 0) || any(vals >= 1)
    ) {
      cli_abort(
        "{.arg certainty_prop} values must be between 0 and 1 (exclusive).",
        call = call
      )
    }
  } else if (!is_null(certainty_prop)) {
    if (
      !is.numeric(certainty_prop) ||
        length(certainty_prop) != 1 ||
        is.na(certainty_prop) ||
        certainty_prop <= 0 ||
        certainty_prop >= 1
    ) {
      cli_abort(
        "{.arg certainty_prop} must be a single number between 0 and 1 (exclusive) or a data frame.",
        call = call
      )
    }
  }
  invisible(NULL)
}
