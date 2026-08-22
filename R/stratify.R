#' Define stratification
#'
#' `stratify_by()` specifies stratification variables and optional allocation
#' methods for a sampling design. Stratification ensures representation from
#' all subgroups defined by the stratification variables.
#'
#' @param .data A `sampling_design` object (piped from [sampling_design()] or
#'   [add_stage()]).
#' @param ... Stratification variables specified as bare column names. Any
#'   name given here is a label and does not rename the variable. A label
#'   resembling one of the arguments below (`allocc`, `varianc`) is refused,
#'   because those arguments follow `...` and are matched exactly.
#' @param alloc Character string specifying the allocation method. One of:
#'   - `NULL` (default): No allocation. `n` in [draw()] is per stratum
#'   - `"equal"`: Equal allocation across strata
#'   - `"proportional"`: Proportional to stratum size
#'   - `"neyman"`: Neyman optimal allocation (requires `variance`)
#'   - `"optimal"`: Cost-variance optimal allocation (requires `variance` and `cost`)
#'   - `"power"`: Power allocation (requires `cv` and `importance`)
#'
#'   Every method is capped at the stratum population: a stratum is never
#'   allocated more units than it holds, and the surplus is redistributed
#'   across the strata with room left, in proportion to the same factors the
#'   method uses. A named rule can therefore be departed from when a stratum
#'   is too small to absorb its share, so `"equal"` on populations
#'   \eqn{(10, 490, 500)} with `n = 300` gives 10/145/145 rather than
#'   100/100/100. `execute()` reports this once per stage with a message of
#'   class `samplyr_message_allocation_capped`, however many parent pools or
#'   replicates the stage runs over.
#' @param variance Stratum variances for Neyman or optimal allocation.
#'   Either a data frame with columns for all stratification variables plus
#'   a `var` column, or a named numeric vector (when using a single
#'   stratification variable) where names correspond to stratum levels.
#'   For named vectors, names must match the values in the stratification
#'   column (for example `c(A = 1.2, B = 0.8)`).
#' @param cost Stratum costs for optimal allocation.
#'   Either a data frame with columns for all stratification variables plus
#'   a `cost` column, or a named numeric vector (when using a single
#'   stratification variable) where names correspond to stratum levels.
#'   For named vectors, names must match the values in the stratification
#'   column.
#' @param cv Stratum coefficients of variation (\eqn{C_h}) for power allocation.
#'   Either a data frame with stratification columns plus a `cv` column, or
#'   a named numeric vector for a single stratification variable (names are
#'   stratum levels).
#' @param importance Stratum importance measure (\eqn{X_h}) for power allocation.
#'   Either a data frame with stratification columns plus an `importance`
#'   column, or a named numeric vector for a single stratification variable
#'   (names are stratum levels).
#' @param power Power exponent \eqn{q} for power allocation.
#'   Must satisfy \eqn{0 \le q \le 1}. Defaults to `0.5`.
#'
#' @return A modified `sampling_design` object with stratification specified.
#'
#' @details
#' ## Allocation methods
#'
#' When no `alloc` is specified, the `n` parameter in [draw()] is interpreted
#' as the sample size *per stratum*. When an `alloc` method is specified,
#' `n` becomes the *total* sample size to be distributed according to the
#' allocation method.
#'
#' ## Population bounds and redistribution
#'
#' For an allocation method the realized sizes satisfy
#' \eqn{\sum_h n_h = n}{sum(n_h) = n} and
#' \eqn{0 \le n_h \le N_h}{0 <= n_h <= N_h} whenever the request is feasible.
#' The population size is an upper bound in its own right, so `min_n` and
#' `max_n` add user bounds on top of a cap that is always present rather than
#' introducing one.
#'
#' Saturated strata are fixed at their bound and removed from the problem,
#' then the remaining total is reallocated over the remaining strata using the
#' method's own factors (\eqn{N_h} for proportional,
#' \eqn{N_h \sqrt{S_h^2}}{N_h * sqrt(var_h)} for Neyman, and so on). This
#' keeps the stated criterion intact after saturation. When the free strata
#' all carry zero factors the criterion is indifferent between them, and the
#' remainder is split equally in stratum order.
#'
#' Requesting more than the frame holds allocates every unit rather than
#' failing. `execute()` reports it through the same per-stage diagnostic as
#' every other population cap, so a stage that ends up taking everything it
#' could reach warns with class `samplyr_warning_census`, and a random-size
#' method in the same position warns with `samplyr_warning_nominal_cap`.
#' Bounds that make the request impossible are errors rather than silent
#' adjustments, with classes `samplyr_error_alloc_min_infeasible` and
#' `samplyr_error_alloc_max_infeasible`.
#'
#' With-replacement and Poisson-multinomial designs draw units repeatedly, so
#' the number of distinct units is not an upper bound on the number of draws.
#' For those methods only `min_n` and `max_n` bind.
#'
#' Per-stratum sizes given directly, through a scalar or named `n`, a `frac`,
#' or a data frame, are instructions rather than a total to distribute. They
#' are never redistributed. Selection caps an impossible one and reports it.
#'
#' ## Equal allocation
#' Each stratum receives n/H units, where H is the number of strata.
#'
#' ## Proportional allocation
#' Each stratum receives \eqn{n \times N_h/N}{n * N_h/N} units, where \eqn{N_h} is the stratum
#' population size and N is the total population size.
#'
#' ## Neyman allocation
#' Minimizes variance for fixed sample size. Each stratum receives:
#' \eqn{n \times (N_h \times S_h) / \sum(N_h \times S_h)}{n * (N_h * S_h) / sum(N_h * S_h)}
#' where S_h is the stratum standard deviation.
#'
#' ## Optimal allocation
#' Minimizes variance for fixed cost (or cost for fixed variance).
#' Each stratum receives:
#' \eqn{n \times (N_h \times S_h / \sqrt{C_h}) / \sum(N_h \times S_h / \sqrt{C_h})}{n * (N_h * S_h / sqrt(C_h)) / sum(N_h * S_h / sqrt(C_h))}
#' where C_h is the per-unit cost in stratum h.
#'
#' ## Power allocation
#' Power allocation (Bankier, 1988) is a compromise allocation:
#' \eqn{n_h \propto C_h \times X_h^q}, where \eqn{C_h} is stratum CV, \eqn{X_h}
#' is a stratum importance measure, and \eqn{q \in [0, 1]}.
#'
#' ## Custom allocation
#' For custom stratum-specific sample sizes or rates, pass a data frame
#' directly to the `n` or `frac` argument in [draw()]. The data frame must
#' contain columns for all stratification variables plus an `n` or `frac` column.
#'
#' ## Auxiliary input formats (`variance`, `cost`, `cv`, `importance`)
#' - With **one** stratification variable, you may use a named vector
#'   (e.g., `variance = c(A = 1.2, B = 0.8)`).
#' - With **multiple** stratification variables, you must use a data frame
#'   containing all stratification columns plus the value column.
#'
#' @section Data frame requirements:
#' Auxiliary data frames (`variance`, `cost`) must contain:
#' - All stratification variable columns (used as join keys)
#' - The appropriate value column (`var` or `cost`)
#'
#' @examples
#' # Simple stratification: 20 EAs per region
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 20) |>
#'   execute(bfa_eas, seed = 1234)
#'
#' # Proportional allocation across regions
#' sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 123)
#'
#' # Neyman allocation using pre-computed variances
#' sampling_design() |>
#'   stratify_by(region, alloc = "neyman", variance = bfa_eas_variance) |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 12)
#'
#' # Optimal allocation considering both variance and cost
#' sampling_design() |>
#'   stratify_by(region, alloc = "optimal",
#'               variance = bfa_eas_variance,
#'               cost = bfa_eas_cost) |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 1)
#'
#' # Power allocation (Bankier, 1988)
#' sampling_design() |>
#'   stratify_by(
#'     region,
#'     alloc = "power",
#'     cv = data.frame(
#'       region = levels(bfa_eas$region),
#'       cv = c(0.40, 0.35, 0.12, 0.20, 0.30, 0.18,
#'              0.15, 0.38, 0.22, 0.32, 0.17, 0.45, 0.25)
#'     ),
#'     importance = data.frame(
#'       region = levels(bfa_eas$region),
#'       importance = c(60, 40, 120, 70, 80, 65,
#'                      50, 55, 90, 75, 45, 35, 30)
#'     ),
#'     power = 0.5
#'   ) |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 7)
#'
#' # Custom sample sizes per stratum using a data frame
#' custom_sizes <- data.frame(
#'   region = levels(bfa_eas$region),
#'   n = c(20, 12, 25, 18, 22, 16, 14, 15, 20, 18, 12, 10, 8)
#' )
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = custom_sizes) |>
#'   execute(bfa_eas, seed = 2026)
#'
#' # Multiple stratification variables
#' sampling_design() |>
#'   stratify_by(region, urban_rural, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 2025)
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [draw()] for specifying sample sizes,
#' [cluster_by()] for cluster sampling
#'
#' @family design specification
#' @export
stratify_by <- function(
  .data,
  ...,
  alloc = NULL,
  variance = NULL,
  cost = NULL,
  cv = NULL,
  importance = NULL,
  power = NULL
) {
  if (!is_sampling_design(.data)) {
    cli_abort("{.arg .data} must be a {.cls sampling_design} object")
  }

  vars_quo <- enquos(...)
  if (length(vars_quo) == 0) {
    cli_abort("At least one stratification variable must be specified")
  }

  check_stratify_dots(vars_quo)

  is_bare_name <- vapply(
    vars_quo,
    function(q) is.symbol(quo_get_expr(q)),
    logical(1)
  )
  if (any(!is_bare_name)) {
    # Suggest prefix matches only after the name is known to be invalid.
    offending <- (names(vars_quo) %||% rep("", length(vars_quo)))[!is_bare_name]
    meant <- NULL
    for (nm in offending) {
      meant <- suggest_reserved_arg(
        nm,
        c("alloc", "variance", "cost", "cv", "importance", "power"),
        prefix = TRUE
      )
      if (!is_null(meant)) {
        break
      }
    }
    cli_abort(c(
      "{.fn stratify_by} variables must be bare column names.",
      "x" = "Tidy-select helpers and expressions are not supported.",
      if (!is_null(meant)) {
        c("i" = cli::format_inline("Did you mean {.arg {meant}}?"))
      },
      "i" = "Example: {.code stratify_by(region, strata)}"
    ))
  }

  vars <- unname(vapply(vars_quo, as_label, character(1)))

  valid_alloc <- c("equal", "proportional", "neyman", "optimal", "power")
  if (!is_null(alloc)) {
    if (!is_character(alloc) || length(alloc) != 1) {
      cli_abort("{.arg alloc} must be a single character string")
    }
    alloc <- match.arg(alloc, valid_alloc)
  }

  if (!is_null(variance)) {
    variance <- coerce_aux_input(variance, vars, "var", "variance")
  }
  if (!is_null(cost)) {
    cost <- coerce_aux_input(cost, vars, "cost", "cost")
  }
  if (!is_null(cv)) {
    cv <- coerce_aux_input(cv, vars, "cv", "cv")
  }
  if (!is_null(importance)) {
    importance <- coerce_aux_input(importance, vars, "importance", "importance")
  }
  if (identical(alloc, "power") && is_null(power)) {
    power <- 0.5
  }

  validate_stratify_args(
    alloc = alloc,
    variance = variance,
    cost = cost,
    cv = cv,
    importance = importance,
    power = power,
    vars = vars
  )

  if (!is_null(variance)) {
    variance <- variance[, c(vars, "var"), drop = FALSE]
  }
  if (!is_null(cost)) {
    cost <- cost[, c(vars, "cost"), drop = FALSE]
  }
  if (!is_null(cv)) {
    cv <- cv[, c(vars, "cv"), drop = FALSE]
  }
  if (!is_null(importance)) {
    importance <- importance[, c(vars, "importance"), drop = FALSE]
  }

  strata_spec <- new_stratum_spec(
    vars = vars,
    alloc = alloc,
    variance = variance,
    cost = cost,
    cv = cv,
    importance = importance,
    power = power
  )

  current <- .data$current_stage
  if (current < 1 || current > length(.data$stages)) {
    cli_abort("Invalid design state: no current stage")
  }

  if (!is_null(.data$stages[[current]]$strata)) {
    cli_abort(
      "Stratification already defined for this stage. Use {.fn add_stage} to start a new stage."
    )
  }

  .data$stages[[current]]$strata <- strata_spec
  .data$validated <- FALSE
  .data
}

#' Catch a misspelled reserved argument before it is read as a variable
#'
#' `alloc` and the auxiliary inputs all sit after `...`, so a near miss such
#' as `allocc` is captured as a stratification variable and then reported as
#' a bad variable expression. Naming the argument is the useful diagnosis.
#' Names that resemble no reserved argument are left alone: they are ignored
#' labels, as in `stratify_by(reg = region)`.
#' @noRd
check_stratify_dots <- function(vars_quo, call = rlang::caller_env()) {
  reserved <- c(
    "alloc", "variance", "cost", "cv", "importance", "power"
  )
  nms <- names(vars_quo) %||% rep("", length(vars_quo))

  for (i in seq_along(vars_quo)) {
    name <- nms[[i]]
    if (!nzchar(name) || is.null(suggest_reserved_arg(name, reserved))) {
      next
    }
    abort_samplyr(
      c(
        "{.fn stratify_by} received an unexpected argument.",
        stray_arg_bullets(name, reserved),
        "i" = "Stratification variables are passed as bare column names."
      ),
      class = "samplyr_error_unknown_argument",
      call = call
    )
  }

  invisible(vars_quo)
}

#' @noRd
validate_stratify_args <- function(
  alloc,
  variance,
  cost,
  cv,
  importance,
  power,
  vars,
  call = rlang::caller_env()
) {
  if (!is_null(alloc)) {
    switch(alloc,
      neyman = {
        if (is_null(variance)) {
          cli_abort(
            "Neyman allocation requires {.arg variance} data frame",
            call = call
          )
        }
      },
      optimal = {
        if (is_null(variance)) {
          cli_abort(
            "Optimal allocation requires {.arg variance} data frame",
            call = call
          )
        }
        if (is_null(cost)) {
          cli_abort(
            "Optimal allocation requires {.arg cost} data frame",
            call = call
          )
        }
      },
      power = {
        if (is_null(cv)) {
          cli_abort(
            "Power allocation requires {.arg cv} data frame or named vector",
            call = call
          )
        }
        if (is_null(importance)) {
          cli_abort(
            "Power allocation requires {.arg importance} data frame or named vector",
            call = call
          )
        }
        if (!is.numeric(power) || length(power) != 1 || !is_finite_numeric(power)) {
          abort_samplyr(
            "{.arg power} must be a single finite number in [0, 1]",
            class = "samplyr_error_alloc_power_bounds",
            call = call
          )
        }
        if (power < 0 || power > 1) {
          abort_samplyr(
            "{.arg power} must be between 0 and 1",
            class = "samplyr_error_alloc_power_bounds",
            call = call
          )
        }
      }
    )
  }

  if (!is_null(variance)) {
    validate_aux_df(variance, vars, "var", "variance", call = call)
  }

  if (!is_null(cost)) {
    validate_aux_df(cost, vars, "cost", "cost", call = call)
  }
  if (!is_null(cv)) {
    validate_aux_df(cv, vars, "cv", "cv", call = call)
  }
  if (!is_null(importance)) {
    validate_aux_df(importance, vars, "importance", "importance", call = call)
  }
  invisible(NULL)
}

#' @noRd
validate_aux_df <- function(
  df,
  vars,
  value_col,
  arg_name,
  call = rlang::caller_env()
) {
  # Callers validate auxiliary input types before this helper.
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    abort_samplyr(
      c(
        "{.arg {arg_name}} is missing stratification variable{?s}:",
        "x" = "{.val {missing_vars}}"
      ),
      class = "samplyr_error_aux_missing_columns",
      call = call
    )
  }

  if (!value_col %in% names(df)) {
    abort_samplyr(
      "{.arg {arg_name}} must contain a {.val {value_col}} column",
      class = "samplyr_error_aux_missing_value_column",
      call = call
    )
  }

  key_df <- df[, vars, drop = FALSE]
  if (anyNA(key_df)) {
    missing_key_cols <- vars[vapply(key_df, anyNA, logical(1))]
    abort_samplyr(
      c(
        "{.arg {arg_name}} has missing values in stratification keys.",
        "x" = "Columns with missing values: {.val {missing_key_cols}}"
      ),
      class = "samplyr_error_aux_missing_key_values",
      call = call
    )
  }

  dup_keys <- find_duplicate_key_rows(df, vars)
  if (nrow(dup_keys) > 0) {
    dup_labels <- format_key_labels(dup_keys, vars)
    abort_samplyr(
      c(
        "{.arg {arg_name}} has duplicate rows for the same stratum.",
        "x" = "Duplicate keys: {.val {dup_labels}}"
      ),
      class = "samplyr_error_aux_duplicate_keys",
      call = call
    )
  }

  values <- df[[value_col]]
  if (!is_finite_numeric(values)) {
    abort_samplyr(
      "{.arg {arg_name}} column {.val {value_col}} must be finite numeric values (no NA/NaN/Inf).",
      class = "samplyr_error_aux_non_finite_values",
      call = call
    )
  }

  switch(arg_name,
    variance = {
      if (any(values < 0)) {
        abort_samplyr(
          "{.arg variance} values must be non-negative",
          class = "samplyr_error_aux_variance_bounds",
          call = call
        )
      }
    },
    cost = {
      if (any(values <= 0)) {
        abort_samplyr(
          "{.arg cost} values must be positive",
          class = "samplyr_error_aux_cost_bounds",
          call = call
        )
      }
    },
    cv = {
      if (any(values <= 0)) {
        abort_samplyr(
          "{.arg cv} values must be positive",
          class = "samplyr_error_aux_cv_bounds",
          call = call
        )
      }
    },
    importance = {
      if (any(values <= 0)) {
        abort_samplyr(
          "{.arg importance} values must be positive",
          class = "samplyr_error_aux_importance_bounds",
          call = call
        )
      }
    }
  )
  invisible(NULL)
}

#' Coerce auxiliary input to a data frame
#'
#' Accepts either a data frame (returned as-is) or a named numeric vector
#' which is converted to a two-column data frame. Named vectors are only
#' supported when there is a single stratification variable.
#' @noRd
coerce_aux_input <- function(
  x,
  vars,
  value_col,
  arg_name,
  call = rlang::caller_env()
) {
  if (is.data.frame(x)) {
    return(x)
  }

  if (is.numeric(x) && !is_null(names(x))) {
    if (length(vars) != 1) {
      cli_abort(
        c(
          "{.arg {arg_name}} as a named vector is only supported with a single stratification variable.",
          "i" = "Current stratification variables: {.val {vars}}.",
          "i" = "Use a data frame with columns {.val {c(vars, value_col)}}."
        ),
        call = call
      )
    }
    df <- data.frame(names(x), unname(x), stringsAsFactors = FALSE)
    names(df) <- c(vars, value_col)
    return(df)
  }

  if (is.numeric(x) && is_null(names(x))) {
    abort_samplyr(
      c(
        "{.arg {arg_name}} must be a data frame or a named numeric vector.",
        "i" = "For one stratification variable, use names as stratum levels (e.g., {.code c(A = 1, B = 2)}).",
        "i" = "For multiple stratification variables, use a data frame."
      ),
      class = "samplyr_error_aux_invalid_input_type",
      call = call
    )
  }

  abort_samplyr(
    c(
      "{.arg {arg_name}} must be a data frame or a named numeric vector.",
      "i" = "Named vectors are only supported with one stratification variable."
    ),
    class = "samplyr_error_aux_invalid_input_type",
    call = call
  )
}
