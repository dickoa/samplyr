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
  "pps_cps",
  "pps_poisson",
  "pps_sps",
  "pps_pareto"
)
pps_wr_methods <- c("pps_multinomial", "pps_chromy")
pps_methods <- c(pps_wor_methods, pps_wr_methods)
prn_methods <- c("bernoulli", "pps_poisson", "pps_sps", "pps_pareto")

#' Map samplyr PPS method name to sondage method name
#' @noRd
sondage_method_name <- function(method) {
  sub("^pps_", "", method)
}

#' @noRd
is_finite_numeric <- function(x) {
  is.numeric(x) &&
    length(x) > 0 &&
    !anyNA(x) &&
    all(is.finite(x))
}

#' @noRd
is_integerish_numeric <- function(x, tol = sqrt(.Machine$double.eps)) {
  is_finite_numeric(x) &&
    all(abs(x - round(x)) <= tol)
}

#' @noRd
abort_samplyr <- function(
  message,
  class = NULL,
  call = rlang::caller_env(),
  envir = parent.frame()
) {
  cli_abort(
    message,
    class = c(class, "samplyr_error"),
    call = call,
    .envir = envir
  )
}

#' @noRd
find_duplicate_key_rows <- function(df, vars) {
  key_df <- df[, vars, drop = FALSE]
  dup <- duplicated(key_df) | duplicated(key_df, fromLast = TRUE)
  unique(key_df[dup, , drop = FALSE])
}

#' @noRd
format_key_labels <- function(df, vars, max_n = 8L) {
  if (nrow(df) == 0) {
    return(character(0))
  }

  labels <- do.call(
    paste,
    c(df[, vars, drop = FALSE], list(sep = "/"))
  )
  labels <- unique(labels)

  if (length(labels) <= max_n) {
    return(labels)
  }

  c(
    labels[seq_len(max_n)],
    paste0("... and ", length(labels) - max_n, " more")
  )
}

