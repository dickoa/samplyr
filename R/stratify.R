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
#' @param variance Stratum variances for Neyman or optimal allocation.
#'   Either a data frame with columns for all stratification variables plus
#'   a `var` column, or a named numeric vector (when using a single
#'   stratification variable) where names correspond to stratum levels.
#' @param cost Stratum costs for optimal allocation.
#'   Either a data frame with columns for all stratification variables plus
#'   a `cost` column, or a named numeric vector (when using a single
#'   stratification variable) where names correspond to stratum levels.
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
#' Each stratum receives \eqn{n \times N_h/N}{n * N_h/N} units, where \eqn{N_h} is the stratum
#' population size and N is the total population size.
#'
#' ### Neyman Allocation
#' Minimizes variance for fixed sample size. Each stratum receives:
#' \eqn{n \times (N_h \times S_h) / \sum(N_h \times S_h)}{n * (N_h * S_h) / sum(N_h * S_h)}
#' where S_h is the stratum standard deviation.
#'
#' ### Optimal Allocation
#' Minimizes variance for fixed cost (or cost for fixed variance).
#' Each stratum receives:
#' \eqn{n \times (N_h \times S_h / \sqrt{C_h}) / \sum(N_h \times S_h / \sqrt{C_h})}{n * (N_h * S_h / sqrt(C_h)) / sum(N_h * S_h / sqrt(C_h))}
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
#'              "Niamey", "Tahoua", "Tillab\u00e9ri", "Zinder"),
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

  if (!is_null(variance)) {
    variance <- coerce_aux_input(variance, vars, "var", "variance")
  }
  if (!is_null(cost)) {
    cost <- coerce_aux_input(cost, vars, "cost", "cost")
  }

  validate_stratify_args(alloc, variance, cost, vars)

  if (!is_null(variance)) {
    variance <- variance[, c(vars, "var"), drop = FALSE]
  }
  if (!is_null(cost)) {
    cost <- cost[, c(vars, "cost"), drop = FALSE]
  }

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
    cli_abort("{.arg {arg_name}} must be a data frame or a named numeric vector",
              call = call)
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

  key_df <- df[, vars, drop = FALSE]
  if (anyDuplicated(key_df) > 0) {
    cli_abort(
      "{.arg {arg_name}} has duplicate rows for the same stratum",
      call = call
    )
  }
  invisible(NULL)
}

#' Coerce auxiliary input to a data frame
#'
#' Accepts either a data frame (returned as-is) or a named numeric vector
#' which is converted to a two-column data frame. Named vectors are only
#' supported when there is a single stratification variable.
#' @noRd
coerce_aux_input <- function(x, vars, value_col, arg_name,
                             call = rlang::caller_env()) {
  if (is.data.frame(x)) {
    return(x)
  }

  if (is.numeric(x) && !is_null(names(x))) {
    if (length(vars) != 1) {
      cli_abort(
        c("{.arg {arg_name}} as a named vector is only supported with a single stratification variable.",
          "i" = "Use a data frame instead."),
        call = call
      )
    }
    df <- data.frame(names(x), unname(x), stringsAsFactors = FALSE)
    names(df) <- c(vars, value_col)
    return(df)
  }

  cli_abort(
    "{.arg {arg_name}} must be a data frame or a named numeric vector",
    call = call
  )
}
