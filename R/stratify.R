#' Define Stratification
#'
#' `stratify_by()` specifies stratification variables and optional allocation
#' methods for a sampling design. Stratification ensures representation from
#' all subgroups defined by the stratification variables.
#'
#' @param .data A `sampling_design` object (piped from [sampling_design()] or
#'   [stage()]).
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> Stratification
#'   variables. These should be categorical variables that define the strata.
#' @param alloc Character string specifying the allocation method. One of:
#'   - `NULL` (default): No allocation; `n` in [draw()] is per stratum
#'   - `"equal"`: Equal allocation across strata
#'   - `"proportional"`: Proportional to stratum size
#'   - `"neyman"`: Neyman optimal allocation (requires `variance`)
#'   - `"optimal"`: Cost-variance optimal allocation (requires `variance` and `cost`)
#' @param variance A data frame with stratum variances for Neyman or optimal
#'   allocation. Must contain columns for all stratification variables plus
#'   a `var` column with variance estimates.
#' @param cost A data frame with stratum costs for optimal allocation.
#'   Must contain columns for all stratification variables plus a `cost`
#'   column with per-unit costs.
#'
#' @return A modified `sampling_design` object with stratification specified.
#'
#' @details
#' ## Allocation Methods
#'
#' When no `alloc` is specified, the `n` parameter in [draw()] is interpreted
#' as the sample size *per stratum*. When an `alloc` method is specified,
#' `n` becomes the *total* sample size to be distributed according to the
#' allocation method.
#'
#' ### Equal Allocation
#' Each stratum receives n/H units, where H is the number of strata.
#'
#' ### Proportional Allocation
#' Each stratum receives n × (N_h/N) units, where N_h is the stratum
#' population size and N is the total population size.
#'
#' ### Neyman Allocation
#' Minimizes variance for fixed sample size. Each stratum receives:
#' n × (N_h × S_h) / Σ(N_h × S_h)
#' where S_h is the stratum standard deviation.
#'
#' ### Optimal Allocation
#' Minimizes variance for fixed cost (or cost for fixed variance).
#' Each stratum receives:
#' n × (N_h × S_h / √C_h) / Σ(N_h × S_h / √C_h)
#' where C_h is the per-unit cost in stratum h.
#'
#' ### Custom Allocation
#' For custom stratum-specific sample sizes or rates, pass a data frame
#' directly to the `n` or `frac` argument in [draw()]. The data frame must
#' contain columns for all stratification variables plus an `n` or `frac` column.
#'
#' @section Data Frame Requirements:
#' Auxiliary data frames (`variance`, `cost`) must contain:
#' - All stratification variable columns (used as join keys)
#' - The appropriate value column (`var` or `cost`)
#'
#' @examples
#' # Simple stratification: 20 EAs per region
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 20) |>
#'   execute(niger_eas, seed = 1234)
#'
#' # Proportional allocation across regions
#' sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 200) |>
#'   execute(niger_eas, seed = 123)
#'
#' # Neyman allocation using pre-computed variances
#' sampling_design() |>
#'   stratify_by(region, alloc = "neyman", variance = niger_eas_variance) |>
#'   draw(n = 200) |>
#'   execute(niger_eas, seed = 12)
#'
#' # Optimal allocation considering both variance and cost
#' sampling_design() |>
#'   stratify_by(region, alloc = "optimal",
#'               variance = niger_eas_variance,
#'               cost = niger_eas_cost) |>
#'   draw(n = 200) |>
#'   execute(niger_eas, seed = 1)
#'
#' # Custom sample sizes per stratum using a data frame
#' custom_sizes <- data.frame(
#'   region = c("Agadez", "Diffa", "Dosso", "Maradi",
#'              "Niamey", "Tahoua", "Tillabéri", "Zinder"),
#'   n = c(15, 20, 30, 35, 25, 30, 25, 20)
#' )
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = custom_sizes) |>
#'   execute(niger_eas, seed = 2026)
#'
#' # Multiple stratification variables
#' sampling_design() |>
#'   stratify_by(region, strata, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(niger_eas, seed = 2025)
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [draw()] for specifying sample sizes,
#' [cluster_by()] for cluster sampling
#'
#' @export
stratify_by <- function(.data, ...,
                        alloc = NULL,
                        variance = NULL,
                        cost = NULL) {
  if (!is_sampling_design(.data)) {
    cli_abort("{.arg .data} must be a {.cls sampling_design} object")
  }

  vars_quo <- enquos(...)
  if (length(vars_quo) == 0) {
    cli_abort("At least one stratification variable must be specified")
  }

  vars <- unname(vapply(vars_quo, as_label, character(1)))

  valid_alloc <- c("equal", "proportional", "neyman", "optimal")
  if (!is_null(alloc)) {
    if (!is_character(alloc) || length(alloc) != 1) {
      cli_abort("{.arg alloc} must be a single character string")
    }
    alloc <- match.arg(alloc, valid_alloc)
  }

  validate_stratify_args(alloc, variance, cost, vars)

  strata_spec <- new_stratum_spec(
    vars = vars,
    alloc = alloc,
    variance = variance,
    cost = cost
  )

  current <- .data$current_stage
  if (current < 1 || current > length(.data$stages)) {
    cli_abort("Invalid design state: no current stage")
  }

  if (!is_null(.data$stages[[current]]$strata)) {
    cli_abort("Stratification already defined for this stage. Use {.fn stage} to start a new stage.")
  }

  .data$stages[[current]]$strata <- strata_spec
  .data$validated <- FALSE
  .data
}

#' @noRd
validate_stratify_args <- function(alloc, variance, cost, vars,
                                   call = rlang::caller_env()) {
  if (identical(alloc, "neyman") && is_null(variance)) {
    cli_abort("Neyman allocation requires {.arg variance} data frame", call = call)
  }

  if (identical(alloc, "optimal")) {
    if (is_null(variance)) {
      cli_abort("Optimal allocation requires {.arg variance} data frame", call = call)
    }
    if (is_null(cost)) {
      cli_abort("Optimal allocation requires {.arg cost} data frame", call = call)
    }
  }

  if (!is_null(variance)) {
    validate_aux_df(variance, vars, "var", "variance", call = call)
  }

  if (!is_null(cost)) {
    validate_aux_df(cost, vars, "cost", "cost", call = call)
  }
  invisible(NULL)
}

#' @noRd
validate_aux_df <- function(df, vars, value_col, arg_name,
                            call = rlang::caller_env()) {
  if (!is.data.frame(df)) {
    cli_abort("{.arg {arg_name}} must be a data frame", call = call)
  }

  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    cli_abort(
      c("{.arg {arg_name}} is missing stratification variable{?s}:",
        "x" = "{.val {missing_vars}}"),
      call = call
    )
  }

  if (!value_col %in% names(df)) {
    cli_abort("{.arg {arg_name}} must contain a {.val {value_col}} column", call = call)
  }
  invisible(NULL)
}
