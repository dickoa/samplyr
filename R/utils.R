# Methods that sample with replacement (units can appear multiple times)
wr_methods <- c("srswr", "pps_multinomial")

# Probability Minimum Replacement (PMR) methods.
# Chromy's sequential PPS method is neither WR nor WOR but a third
# category: each unit receives exactly floor(E(n_i)) or
# floor(E(n_i)) + 1 hits, where E(n_i) = n * mos_i / sum(mos).
# When all E(n_i) < 1 the method reduces to WOR (hits in {0, 1}),
# but in general units can receive multiple hits.
#
# For variance estimation, Chromy (2009) recommends the Hansen-Hurwitz
# (WR) approximation with finite population correction rather than
# exact pairwise expectations, which he found "quite variable."
# Chauvet (2019) confirmed this via simulation.  We therefore treat
# PMR stages like WR for FPC and variance purposes.
#
# References:
#   Chromy, J.R. (2009). "Some Generalizations of the Horvitz-Thompson
#     Estimator." JSM Proceedings, Survey Research Methods Section.
#   Chauvet, G. (2019). "Properties of Chromy's sampling procedure."
#     arXiv:1912.10896.
pmr_methods <- c("pps_chromy")

# Combined: methods that can produce multiple hits per unit
multi_hit_methods <- c(wr_methods, pmr_methods)

# PPS without replacement methods (need joint probability support)
pps_wor_methods <- c(
  "pps_brewer",
  "pps_systematic",
  "pps_maxent",
  "pps_poisson"
)

#' Internal Utility Functions
#'
#' @name utils
#' @keywords internal
NULL

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
