#' Internal Utility Functions
#'
#' @name utils
#' @keywords internal
NULL

#' Null-coalescing operator
#'
#' @param x Value to check
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @noRd
`%||%` <- function(x, y) {
  if (is_null(x)) y else x
}

#' Check if all elements are unique
#'
#' @param x Vector to check
#' @return Logical
#' @noRd
all_unique <- function(x) {
  length(x) == length(unique(x))
}

#' Safely convert to integer
#'
#' @param x Value to convert
#' @return Integer or error
#' @noRd
safe_as_integer <- function(x) {
  if (is_null(x)) {
    return(NULL)
  }
  if (!is.numeric(x)) {
    cli_abort("Expected numeric value")
  }
  as.integer(x)
}

#' Create a named list from arguments
#'
#' @param ... Named arguments
#' @return Named list
#' @noRd
named_list <- function(...) {
  args <- list(...)
  if (is_null(names(args)) || any(names(args) == "")) {
    cli_abort("All arguments must be named")
  }
  args
}

#' Check if value is a positive integer
#'
#' @param x Value to check
#' @param arg Argument name for error messages
#' @return TRUE if valid, otherwise errors
#' @noRd
check_positive_int <- function(x, arg = "x") {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x <= 0 || x != round(x)) {
    cli_abort("{.arg {arg}} must be a positive integer")
  }
  invisible(TRUE)
}

#' Check if value is a proportion (0, 1]
#'
#' @param x Value to check
#' @param arg Argument name for error messages
#' @return TRUE if valid, otherwise errors
#' @noRd
check_proportion <- function(x, arg = "x") {
  if (!is.numeric(x) || any(is.na(x)) || any(x <= 0) || any(x > 1)) {
    cli_abort("{.arg {arg}} must be a proportion in (0, 1]")
  }
  invisible(TRUE)
}

#' Get variable names from quosures
#'
#' @param quos List of quosures
#' @return Character vector of variable names
#' @noRd
get_var_names <- function(quos) {
  unname(vapply(quos, as_label, character(1)))
}

#' Create a stratum key
#'
#' Create a unique identifier for each stratum combination
#'
#' @param data Data frame
#' @param vars Stratification variables
#' @return Character vector of stratum keys
#' @noRd
create_stratum_key <- function(data, vars) {
  if (length(vars) == 0) {
    return(rep("__all__", nrow(data)))
  }
  do.call(paste, c(data[vars], sep = "__"))
}

#' Validate nesting
#'
#' Check that cluster variable is properly nested within strata
#'
#' @param data Data frame
#' @param strata_vars Stratification variables
#' @param cluster_vars Cluster variables
#' @return TRUE if valid, otherwise errors
#' @noRd
validate_nesting <- function(data, strata_vars, cluster_vars) {
  if (length(strata_vars) == 0 || length(cluster_vars) == 0) {
    return(invisible(TRUE))
  }

  check_df <- data |>
    select(all_of(c(strata_vars, cluster_vars))) |>
    distinct()

  cluster_counts <- check_df |>
    group_by(across(all_of(cluster_vars))) |>
    summarise(
      ".n_strata" = n_distinct(across(all_of(strata_vars))),
      .groups = "drop"
    )

  non_nested <- cluster_counts |>
    filter(.data$`.n_strata` > 1)

  if (nrow(non_nested) > 0) {
    example <- non_nested |> slice(1)
    cluster_id <- example[[cluster_vars[1]]]
    cli_abort(c(
      "Clusters are not properly nested within strata",
      "x" = "Cluster {.val {cluster_id}} appears in multiple strata"
    ))
  }
  invisible(TRUE)
}

#' Compute effective sample size
#'
#' Compute effective sample size accounting for unequal weights
#'
#' @param weights Vector of sampling weights
#' @return Effective sample size
#' @export
effective_n <- function(weights) {
  sum(weights)^2 / sum(weights^2)
}

#' Compute design effect
#'
#' Estimate design effect from sample weights
#'
#' @param weights Vector of sampling weights
#' @return Estimated design effect
#' @export
design_effect <- function(weights) {
  n <- length(weights)
  n / effective_n(weights)
}
