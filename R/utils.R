#' Internal Utility Functions
#'
#' @name utils
#' @keywords internal
NULL

#' @noRd
wr_methods <- c("srswr", "pps_multinomial")
pmr_methods <- c("pps_chromy")
multi_hit_methods <- c(wr_methods, pmr_methods)
pps_wor_methods <- c(
  "pps_brewer",
  "pps_systematic",
  "pps_maxent",
  "pps_poisson"
)
pps_methods <- c(
  pps_wor_methods,
  "pps_multinomial",
  "pps_chromy"
)

#' Compute effective sample size
#'
#' Compute effective sample size accounting for unequal weights
#'
#' @param weights Vector of sampling weights
#' @return Effective sample size
#' @export
effective_n <- function(weights) {
  if (!is.numeric(weights) || length(weights) == 0) {
    cli_abort("{.arg weights} must be a non-empty numeric vector")
  }
  if (anyNA(weights)) {
    cli_abort("{.arg weights} must not contain NA values")
  }
  if (any(weights <= 0)) {
    cli_abort("{.arg weights} must be positive")
  }
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
