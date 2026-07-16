#' Internal Utility Functions
#'
#' @name utils
#' @keywords internal
NULL

#' @noRd
equal_prob_methods <- c("srswor", "srswr", "systematic", "bernoulli")
wr_methods <- c("srswr", "pps_multinomial")
pmr_methods <- c("pps_chromy")
multi_hit_methods <- c(wr_methods, pmr_methods)
pps_wor_methods <- c(
  "pps_brewer",
  "pps_systematic",
  "pps_cps",
  "pps_sampford",
  "pps_poisson",
  "pps_sps",
  "pps_pareto"
)
pps_wr_methods <- c("pps_multinomial", "pps_chromy")
pps_methods <- c(pps_wor_methods, pps_wr_methods)
prn_methods <- c("bernoulli", "pps_poisson", "pps_sps", "pps_pareto")
spatial_balanced_methods <- c("lpm2", "scps")
balanced_methods <- c("cube", spatial_balanced_methods)
builtin_methods <- c(equal_prob_methods, pps_methods, balanced_methods)
builtin_method_aliases <- c(balanced = "cube")
valid_builtin_methods <- c(builtin_methods, names(builtin_method_aliases))
jip_methods <- c(pps_methods, balanced_methods)

# Random-size Poisson methods: independent unit selection with random
# realised sample size. Variance estimation requires the Horvitz-Thompson
# Poisson formula via survey::poisson_sampling(), not the SRSWOR or
# Brewer estimators used for fixed-size designs.
rs_poisson_methods <- c("bernoulli", "pps_poisson")

#' Return the public family prefix for a registered method
#' @noRd
custom_method_prefix <- function(method) {
  if (grepl("^pps_.+", method)) return("pps")
  if (grepl("^balanced_.+", method)) return("balanced")
  NULL
}

#' Normalize compatibility names to their canonical public form
#' @noRd
canonical_method_name <- function(method, method_type = NULL) {
  if (identical(method, "balanced")) return("cube")
  if (
    identical(method_type, "balanced") &&
      grepl("^pps_.+", method)
  ) {
    return(sub("^pps_", "balanced_", method))
  }
  method
}

#' Map a samplyr method name to its sondage registry name
#' @noRd
sondage_method_name <- function(method) {
  if (method %in% c("balanced", "cube")) return("cube")
  sub("^(pps|balanced)_", "", method)
}

#' Check if a method is a custom registered method in sondage
#' @noRd
is_custom_method <- function(method) {
  if (is_null(custom_method_prefix(method))) return(FALSE)
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
#'
#' Balanced (cube) selection is without replacement; custom balanced
#' methods carry method_type "balanced" and must count as WOR just
#' like the built-in `cube` method does through the name test.
#' @noRd
is_wor_method <- function(draw_spec) {
  method <- draw_spec$method
  if (!is_null(draw_spec$method_type)) {
    return(draw_spec$method_type %in% c("wor", "balanced"))
  }
  !(method %in% c(wr_methods, pmr_methods))
}

#' Check if method is a multi-hit method (built-in or registered)
#' @noRd
is_multi_hit_method <- function(draw_spec) {
  method <- draw_spec$method
  if (!is_null(draw_spec$method_type)) return(draw_spec$method_type == "wr")
  method %in% multi_hit_methods
}

#' Check if a method belongs to the balanced family
#' @noRd
is_balanced_method <- function(draw_spec) {
  draw_spec$method %in% balanced_methods ||
    identical(draw_spec$method_type, "balanced")
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

#' Internal tbl_sample column pattern
#'
#' Columns written by execute() that carry design metadata. Stripped
#' when a tbl_sample is reused as a frame (samplyr_internal_cols) and
#' protected against overwrites by dplyr_col_modify.tbl_sample.
#' @noRd
samplyr_internal_col_pattern <-
  "^\\.(weight|fpc|sample_id|stage|draw|certainty|replicate|panel)"

#' Detect a tbl_sample whose class was stripped
#'
#' Some tidyr and base operations preserve samplyr's attributes and generated
#' columns while dropping only the tbl_sample class. Such an object must not be
#' accepted as an ordinary population frame for a fresh design execution: that
#' would silently rerun stage 1 and treat inherited weights as frame variables.
#'
#' Attributes are definitive evidence. The column fallback deliberately
#' requires the full core/stage signature so an unrelated frame with a single
#' conventional `.weight` column is not rejected.
#' @noRd
looks_like_stripped_tbl_sample <- function(x) {
  if (is_tbl_sample(x) || !is.data.frame(x)) {
    return(FALSE)
  }

  has_provenance <-
    is_sampling_design(attr(x, "design")) &&
      !is_null(attr(x, "stages_executed")) &&
      is.list(attr(x, "metadata"))

  nms <- names(x)
  has_internal_signature <-
    all(c(".weight", ".sample_id", ".stage") %in% nms) &&
      any(grepl("^\\.weight_[0-9]+$", nms)) &&
      any(grepl("^\\.fpc_[0-9]+$", nms))

  has_provenance || has_internal_signature
}

#' Columns whose values the stored design depends on
#'
#' Internal metadata columns plus the stratification and clustering
#' variables of the executed stages. These are the columns covered by
#' the integrity record: dropping, renaming, or changing their values
#' breaks the link between the data and the stored design.
#' @noRd
protected_sample_cols <- function(data, design, stages_executed) {
  internal <- grep(samplyr_internal_col_pattern, names(data), value = TRUE)
  design_vars <- character(0)
  for (i in stages_executed) {
    stage_spec <- design$stages[[i]]
    if (!is_null(stage_spec$strata)) {
      design_vars <- c(design_vars, stage_spec$strata$vars)
    }
    if (!is_null(stage_spec$clusters)) {
      design_vars <- c(design_vars, stage_spec$clusters$vars)
    }
  }
  unique(c(internal, intersect(unique(design_vars), names(data))))
}

#' Order-invariant hash of the protected columns
#'
#' Rows are put in a canonical order before hashing, so harmless
#' reordering (arrange, sorted joins) leaves the hash unchanged. Ties
#' in the ordering are rows with identical protected values, which are
#' interchangeable, so the hash is well defined. (.sample_id alone is
#' not a usable key: it is duplicated across expanded rows of
#' cluster-final stages.)
#' @noRd
protected_values_hash <- function(data, cols) {
  vals <- lapply(cols, function(col) data[[col]])
  if (nrow(data) > 1L) {
    ord <- do.call(order, c(vals, list(method = "radix")))
    vals <- lapply(vals, function(v) v[ord])
  }
  names(vals) <- cols
  rlang::hash(vals)
}

#' Integrity record for an executed sample
#'
#' Stored in metadata$integrity by every execute path. This is the
#' authoritative description of the realization: per-operation
#' modification marks give immediate feedback and good messages, but
#' too many table operations can bypass an S3 hook (base assignment,
#' rbind(), vctrs operations, third-party verbs), so
#' check_sample_unmodified() recomputes and compares this record at
#' the analysis boundary.
#' @noRd
sample_integrity_record <- function(data, design, stages_executed) {
  cols <- protected_sample_cols(data, design, stages_executed)
  list(
    n_rows = nrow(data),
    cols = cols,
    hash = protected_values_hash(data, cols)
  )
}

#' Per-replicate hashes for the complete-replicate exemption
#' @noRd
replicate_integrity_hashes <- function(data, cols, rep_ids) {
  hashes <- lapply(rep_ids, function(r) {
    protected_values_hash(
      data[data$.replicate == r, , drop = FALSE],
      cols
    )
  })
  setNames(hashes, as.character(rep_ids))
}

#' Compare a sample against its stored integrity record
#'
#' @return "ok", or the failure kind: "columns" (protected columns
#'   missing), "rows" (row count changed), or "values" (protected
#'   values changed).
#' @noRd
verify_sample_integrity <- function(x, integrity) {
  if (!all(integrity$cols %in% names(x))) {
    return("columns")
  }
  if (nrow(x) != integrity$n_rows) {
    return("rows")
  }
  if (!identical(protected_values_hash(x, integrity$cols), integrity$hash)) {
    return("values")
  }
  "ok"
}

#' Apply integrity-derived modification marks to a tbl_sample
#'
#' Used by as_tbl_sample() so a stripped-and-restored object cannot
#' launder away its modification state.
#' @noRd
apply_integrity_marks <- function(x) {
  integrity <- attr(x, "metadata")$integrity
  if (is_null(integrity)) {
    return(x)
  }
  status <- verify_sample_integrity(x, integrity)
  if (!identical(status, "ok") && !is_complete_replicate(x)) {
    x <- mark_sample_modified(x, status)
  }
  x
}

#' Record a post-execution modification on a tbl_sample
#'
#' `what` is "rows" (row set changed: removed, added, or duplicated),
#' "columns" (an internal design column was overwritten or dropped),
#' or "values" (protected values changed through an untracked route,
#' detected by integrity verification). The marks accumulate in
#' `metadata$modified` and give immediate feedback;
#' check_sample_unmodified() treats the integrity record as
#' authoritative at the analysis boundary.
#' @noRd
mark_sample_modified <- function(x, what) {
  meta <- attr(x, "metadata") %||% list()
  meta$modified <- union(meta$modified, what)
  attr(x, "metadata") <- meta
  x
}

#' Modifications recorded on a tbl_sample
#' @return Character vector, subset of c("rows", "columns").
#' @noRd
sample_modifications <- function(x) {
  attr(x, "metadata")$modified %||% character(0)
}

#' Test whether a row-modified sample is exactly one complete replicate
#'
#' Extracting a single replicate from a replicated execution (for
#' example `filter(.replicate == 1)`) is the documented way to analyze
#' one realization, so it is exempt from the modified-rows check. The
#' extraction is verified against the contiguous `.sample_id` block
#' recorded at execution time (`metadata$replicate_rows`), so a
#' replicate that was further filtered, duplicated, or had its
#' identifier columns rewritten does not pass.
#' @noRd
is_complete_replicate <- function(x) {
  meta <- attr(x, "metadata")
  counts <- meta$replicate_rows
  if (
    is_null(counts) ||
      is_null(names(counts)) ||
      !all(c(".replicate", ".sample_id") %in% names(x))
  ) {
    return(FALSE)
  }
  r <- unique(x$.replicate)
  if (length(r) != 1L || is.na(r)) {
    return(FALSE)
  }
  pos <- match(as.character(r), names(counts))
  if (is.na(pos)) {
    return(FALSE)
  }
  ids <- x$.sample_id
  if (length(ids) == 0L) {
    return(counts[pos] == 0L)
  }
  end <- sum(counts[seq_len(pos)])
  start <- end - counts[pos] + 1L
  structural_ok <- !anyNA(ids) &&
    length(ids) == counts[pos] &&
    anyDuplicated(ids) == 0L &&
    min(ids) == start &&
    max(ids) == end
  if (!structural_ok) {
    return(FALSE)
  }

  # When the execution stored per-replicate hashes, verify the
  # extracted values match the recorded realization exactly.
  integrity <- meta$integrity
  rep_hash <- integrity$replicate_hashes[[as.character(r)]]
  if (!is_null(rep_hash)) {
    if (!all(integrity$cols %in% names(x))) {
      return(FALSE)
    }
    return(identical(protected_values_hash(x, integrity$cols), rep_hash))
  }
  TRUE
}

#' Integrity-aware realization status of a tbl_sample
#'
#' The integrity record is authoritative: when it verifies, the sample
#' IS the executed realization and any per-operation marks were false
#' alarms (e.g. an overwrite with identical values). When it fails, the
#' sample is not the realization even if no operation marked it, which
#' catches routes the S3 hooks cannot see (base assignment, rbind(),
#' vctrs operations, third-party verbs). A sample reduced to exactly
#' one complete replicate (hash-verified when available) counts as
#' intact. Samples without an integrity record (built by older
#' versions or by hand) fall back to the marks.
#' @return list(ok = logical, mods = character): mods combines the
#'   per-operation marks with the integrity failure kind.
#' @noRd
sample_realization_status <- function(x) {
  mods <- sample_modifications(x)
  integrity <- attr(x, "metadata")$integrity

  if (!is_null(integrity)) {
    status <- verify_sample_integrity(x, integrity)
    if (identical(status, "ok") || is_complete_replicate(x)) {
      return(list(ok = TRUE, mods = character(0)))
    }
    return(list(ok = FALSE, mods = union(mods, status)))
  }

  if (
    length(mods) == 0 ||
      (identical(mods, "rows") && is_complete_replicate(x))
  ) {
    return(list(ok = TRUE, mods = character(0)))
  }
  list(ok = FALSE, mods = mods)
}

#' Check that a tbl_sample still matches its executed realization
#' @noRd
check_sample_unmodified <- function(x, fn_name, call = caller_env()) {
  status <- sample_realization_status(x)
  if (status$ok) {
    return(invisible(NULL))
  }
  mods <- status$mods

  bullets <- character(0)
  if ("rows" %in% mods) {
    bullets <- c(
      bullets,
      "x" = "Rows were removed, added, or duplicated after {.fn execute}."
    )
  }
  if ("columns" %in% mods) {
    bullets <- c(
      bullets,
      "x" = "Internal design columns (e.g. {.field .weight}, {.field .fpc_*}) or design-referenced strata/cluster columns were dropped or renamed after {.fn execute}."
    )
  }
  if ("values" %in% mods) {
    bullets <- c(
      bullets,
      "x" = "Protected values (weights, design metadata, or strata/cluster identifiers) no longer match the executed realization."
    )
  }
  if (length(sample_modifications(x)) == 0) {
    bullets <- c(
      bullets,
      "i" = "The change came through a route samplyr does not track per operation (e.g. base assignment, {.fn rbind}, or a vctrs operation); integrity verification caught it at this boundary."
    )
  }

  abort_samplyr(
    c(
      "{.fn {fn_name}} requires a sample that still matches its executed design.",
      bullets,
      "i" = "For domain (subpopulation) analysis, convert the full sample first, then subset the design: {.code subset(as_svydesign(full_sample), condition)}, or with srvyr: {.code as_survey_design(full_sample) |> filter(condition)}.",
      "i" = "To subsample an executed sample, run a second phase: {.code sampling_design() |> draw(...) |> execute(full_sample)}."
    ),
    class = "samplyr_error_modified_sample",
    call = call
  )
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
