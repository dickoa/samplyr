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
jip_methods <- c(pps_methods, "balanced")

#' Map samplyr PPS method name to sondage method name
#' @noRd
sondage_method_name <- function(method) {
  if (identical(method, "balanced")) return("cube")
  sub("^pps_", "", method)
}

#' Check if a method is a custom registered method in sondage
#' @noRd
is_custom_method <- function(method) {
  sondage_name <- sondage_method_name(method)
  sondage::is_registered_method(sondage_name)
}

#' Query metadata for a custom registered method
#' @return A list with type, fixed_size, supports_prn, or NULL.
#' @noRd
custom_method_spec <- function(method) {
  sondage::method_spec(sondage_method_name(method))
}

#' Check if method is WOR (built-in or registered)
#' @noRd
is_wor_method <- function(draw_spec) {
  method <- draw_spec$method
  if (!is_null(draw_spec$method_type)) return(draw_spec$method_type == "wor")
  !(method %in% c(wr_methods, pmr_methods))
}

#' Check if method is a multi-hit method (built-in or registered)
#' @noRd
is_multi_hit_method <- function(draw_spec) {
  method <- draw_spec$method
  if (!is_null(draw_spec$method_type)) return(draw_spec$method_type == "wr")
  method %in% multi_hit_methods
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

#' Collect cluster variables from all stages before the given stage
#' @noRd
collect_ancestor_cluster_vars <- function(design, stage_idx) {
  if (stage_idx <= 1L) return(character(0))
  vars <- character(0)
  for (i in seq_len(stage_idx - 1L)) {
    spec <- design$stages[[i]]
    if (!is_null(spec$clusters)) vars <- c(vars, spec$clusters$vars)
  }
  unique(vars)
}

#' @noRd
find_duplicate_key_rows <- function(df, vars) {
  key_df <- df[, vars, drop = FALSE]
  dup <- duplicated(key_df) | duplicated(key_df, fromLast = TRUE)
  unique(key_df[dup, , drop = FALSE])
}

#' Test whether a tbl_sample contains multiple replicates
#'
#' Note: if `.replicate` contains NA, unique() includes it, so this
#' returns TRUE for c(1, NA). This is intentionally conservative.
#' In guarded contexts (survey export, svyplan), check_single_replicate()
#' catches NA before calling this. In display contexts (print, summary),
#' treating corrupted data as multi-replicate is the safe default.
#' @noRd
has_multiple_replicates <- function(x) {
  ".replicate" %in% names(x) && length(unique(x$.replicate)) > 1L
}

#' Check that a tbl_sample has at most one replicate
#' @noRd
check_single_replicate <- function(x, fn_name, call = caller_env()) {
  if (!".replicate" %in% names(x)) {
    return(invisible(NULL))
  }
  if (anyNA(x$.replicate)) {
    cli_abort(
      "{.field .replicate} column contains {.val NA} values.",
      call = call
    )
  }
  if (has_multiple_replicates(x)) {
    n_reps <- length(unique(x$.replicate))
    abort_samplyr(
      c(
        "{.fn {fn_name}} requires a single replicate.",
        "i" = "This sample has {n_reps} replicates.",
        "i" = "Filter first: {.code x |> filter(.replicate == 1)}"
      ),
      class = "samplyr_error_replicated_sample_unsupported",
      call = call
    )
  }
  invisible(NULL)
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

