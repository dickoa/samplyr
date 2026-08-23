## Recorded weight-sharing operator

# The linear operator serves point estimates and every replicate column.

## The triplet operator

#' A sparse linear map from source rows to target rows
#'
#' Stored in triplet form rather than as a dense matrix. The dense form is
#' `n_target * n_source` doubles for a structure that is almost always sparse,
#' and the triplet form is also the shape the record serializes to and the
#' shape a reader can check by eye against a link table.
#'
#' Triplets are canonicalized on construction, ordered by target and then by
#' source. Two calls that build the same map in different orders therefore
#' produce identical records, which is what makes the record comparable and
#' hashable rather than merely equivalent.
#'
#' @param target_row,source_row Positive integer row indices.
#' @param share The coefficient multiplying that source row's weight.
#' @param n_target,n_source Dimensions of the map. Passed rather than inferred
#'   from the maxima, because a map whose last target rows receive nothing is
#'   legitimate and inferring would silently shrink it.
#' @noRd
new_share_operator <- function(
  target_row,
  source_row,
  share,
  n_target,
  n_source,
  call = caller_env()
) {
  op <- list(
    target_row = vec_cast_index(target_row, "target_row", call = call),
    source_row = vec_cast_index(source_row, "source_row", call = call),
    share = share,
    n_target = vec_cast_count(n_target, "n_target", call = call),
    n_source = vec_cast_count(n_source, "n_source", call = call)
  )
  validate_share_operator(op, call = call)
  canonicalize_share_operator(op)
}

#' @noRd
vec_cast_index <- function(x, arg, call = caller_env()) {
  if (!is.numeric(x) || anyNA(x) || any(x != trunc(x))) {
    abort_samplyr(
      c(
        "{.arg {arg}} must be whole numbers with no missing values.",
        "i" = "It indexes rows of a table, so a fractional or missing value
               names no row."
      ),
      class = "samplyr_error_share_operator_malformed",
      call = call
    )
  }
  as.integer(x)
}

#' @noRd
vec_cast_count <- function(x, arg, call = caller_env()) {
  if (
    !is.numeric(x) || length(x) != 1L || is.na(x) ||
      x != trunc(x) || x < 0
  ) {
    abort_samplyr(
      c("{.arg {arg}} must be a single non-negative whole number."),
      class = "samplyr_error_share_operator_malformed",
      call = call
    )
  }
  as.integer(x)
}

#' Refuse an operator that cannot be a weight-sharing map
#'
#' The duplicate-pair check is the one with statistical content. A repeated
#' (target, source) pair adds a second coefficient for a link that exists once,
#' which inflates that target's weight without any diagnostic saying so. It is
#' the same defect that makes duplicate link rows a refusal at the call site,
#' arriving one layer down where it can no longer be attributed to user data.
#' @noRd
validate_share_operator <- function(op, call = caller_env()) {
  n <- length(op$target_row)
  if (length(op$source_row) != n || length(op$share) != n) {
    abort_samplyr(
      c(
        "{.field target_row}, {.field source_row} and {.field share} must be
         the same length.",
        "x" = "They are {length(op$target_row)}, {length(op$source_row)} and
               {length(op$share)}."
      ),
      class = "samplyr_error_share_operator_malformed",
      call = call
    )
  }

  if (!is.numeric(op$share) || anyNA(op$share) || any(!is.finite(op$share))) {
    abort_samplyr(
      c(
        "{.field share} must be finite numbers with no missing values.",
        "i" = "A missing coefficient would propagate into the target weight
               rather than being reported."
      ),
      class = "samplyr_error_share_operator_malformed",
      call = call
    )
  }
  if (any(op$share < 0)) {
    abort_samplyr(
      c(
        "{.field share} must be non-negative.",
        "i" = "Links carry a non-negative importance. A negative coefficient
               would subtract one unit's representation from another's."
      ),
      class = "samplyr_error_share_operator_malformed",
      call = call
    )
  }

  out_target <- op$target_row < 1L | op$target_row > op$n_target
  out_source <- op$source_row < 1L | op$source_row > op$n_source
  if (any(out_target) || any(out_source)) {
    abort_samplyr(
      c(
        "Every operator entry must index an existing row.",
        "x" = if (any(out_target)) {
          "{sum(out_target)} entr{?y/ies} name a target row outside
           {.field 1:{op$n_target}}."
        },
        "x" = if (any(out_source)) {
          "{sum(out_source)} entr{?y/ies} name a source row outside
           {.field 1:{op$n_source}}."
        }
      ),
      class = "samplyr_error_share_operator_malformed",
      call = call
    )
  }

  if (n > 0L) {
    dup <- duplicated(data.frame(
      t = op$target_row,
      s = op$source_row
    ))
    if (any(dup)) {
      abort_samplyr(
        c(
          "The operator carries {sum(dup)} repeated
           {cli::qty(sum(dup))}{?pair/pairs} of target and source row.",
          "x" = "A pair appearing twice contributes its source weight twice.",
          "i" = "Combine repeated links into one entry whose {.field share}
                 is their total, so the coefficient is visible in the record."
        ),
        class = "samplyr_error_share_operator_duplicate",
        call = call
      )
    }
  }

  invisible(op)
}

#' @noRd
canonicalize_share_operator <- function(op) {
  if (length(op$target_row) > 1L) {
    ord <- order(op$target_row, op$source_row, method = "radix")
    op$target_row <- op$target_row[ord]
    op$source_row <- op$source_row[ord]
    op$share <- as.numeric(op$share)[ord]
  } else {
    op$share <- as.numeric(op$share)
  }
  op
}

#' Apply the operator to source weights
#'
#' `w_B = T w_A`. Accepts a vector of source weights or a matrix whose columns
#' are replicate weight systems, and returns the same shape over target rows.
#' Applying the one recorded operator to every replicate column is what keeps
#' the sharing inside the replication, which is the ordering the variance
#' depends on.
#'
#' A target row no entry names receives zero rather than a missing value: it
#' contributed nothing, which is a number, not an unknown.
#'
#' @param w Numeric vector of length `n_source`, or a numeric matrix with
#'   `n_source` rows.
#' @return A vector or matrix over the `n_target` target rows, matching the
#'   shape of `w`.
#' @noRd
apply_share_operator <- function(op, w, call = caller_env()) {
  vector_in <- is.null(dim(w))
  wm <- if (vector_in) matrix(w, ncol = 1L) else as.matrix(w)

  if (nrow(wm) != op$n_source) {
    abort_samplyr(
      c(
        "The weights do not line up with the recorded transformation.",
        "x" = "The operator was built over {op$n_source} source row{?s} and
               was given {nrow(wm)}.",
        "i" = "The source sample must be the one the transformation was
               recorded on, in its recorded row order."
      ),
      class = "samplyr_error_share_operator_misaligned",
      call = call
    )
  }
  if (!is.numeric(wm)) {
    abort_samplyr(
      "Weights must be numeric.",
      class = "samplyr_error_share_operator_misaligned",
      call = call
    )
  }

  out <- matrix(0, nrow = op$n_target, ncol = ncol(wm))
  if (length(op$target_row) > 0L) {
    contrib <- op$share * wm[op$source_row, , drop = FALSE]
    agg <- rowsum(contrib, group = op$target_row, reorder = TRUE)
    out[as.integer(rownames(agg)), ] <- agg
  }

  if (vector_in) {
    return(as.numeric(out[, 1L]))
  }
  dimnames(out) <- list(NULL, colnames(wm))
  out
}

#' Target rows the operator never names
#'
#' Returned rather than refused. Whether an unreached target row is a defect
#' depends on the mode that built the operator, and this file does not know
#' the mode.
#' @noRd
share_operator_unreached <- function(op) {
  setdiff(seq_len(op$n_target), unique(op$target_row))
}

## The transformation record

#' The law this package writes weight-share records under
#'
#' Stamped for the same reason the panel assignment stamps one: everything
#' computed from the record afterwards is specific to the algorithm and the
#' version. A later algorithm writing these field names must not inherit this
#' one's meaning by default.
#' @noRd
weight_share_record_algorithm <- "generalized_weight_share"

#' @noRd
supported_weight_share_record_versions <- 1L

#' @noRd
weight_share_within_modes <- c("cluster", "singleton", "extended")

#' @noRd
weight_share_denominator_modes <- c(
  "supplied", "complete_links", "weighted_links", "complete_weighted_links"
)

#' @noRd
weight_share_denominator_scales <- c("binary", "quantitative")

#' @noRd
weight_share_target_scopes <- c("reached", "population")

#' The columns each denominator scale generates on the result
#'
#' Only the pair belonging to the mode in use is emitted. Two always-missing
#' columns would be a shape this package does not otherwise produce.
#' @noRd
weight_share_generated_cols <- list(
  binary = c(".unit_links", ".cluster_links"),
  quantitative = c(".link_weight", ".cluster_link_weight")
)

#' Build the record a transformed sample carries
#'
#' The record stores the normalized operator and normalized identities, never
#' the user's expressions. Re-evaluating a quosure or re-reading a link table
#' at export time would let an external register change the meaning of a
#' transformation that has already happened, which is the discipline
#' `replay_design()` already enforces for designs.
#'
#' `result_integrity` is filled in by `attach_weight_share_record()`, because
#' it cannot be computed until the result it describes exists.
#'
#' @param source_sample The intact source sample, retained whole. The replicate
#'   route needs to rebuild its replicate weights, and a payload sufficient for
#'   that is the sample itself.
#' @param source_key_cols,target_key_cols Column names identifying a row on
#'   each side. The per-row key values are derived from them and stored
#'   separately: the operator addresses rows by position, so validating
#'   alignment needs the keys themselves and not only where to find them.
#' @param denominator A list carrying `mode`, `scale` and, for the asserted
#'   modes, what was asserted.
#' @param coverage The Constraint 2.1 diagnostics, from
#'   `new_weight_share_coverage()`.
#' @noRd
new_weight_share_record <- function(
  operator,
  source_sample,
  source_integrity,
  source_key_cols,
  target_key_cols,
  target_cluster,
  within_mode,
  denominator,
  coverage,
  generated_cols,
  call_info,
  call = caller_env()
) {
  record <- list(
    algorithm = weight_share_record_algorithm,
    version = 1L,
    operator = operator,
    source_sample = source_sample,
    source_integrity = source_integrity,
    source_key_cols = source_key_cols,
    target_key_cols = target_key_cols,
    # Attach target keys after the result exists.
    source_row_keys = share_row_keys(source_sample, source_key_cols),
    target_row_keys = NULL,
    target_cluster = target_cluster,
    within_mode = within_mode,
    denominator = denominator,
    coverage = coverage,
    generated_cols = generated_cols,
    result_integrity = NULL,
    call = call_info
  )
  check_weight_share_record_fields(
    record, "This transformation",
    attached = FALSE, call = call
  )
  record
}

#' Per-row key values, in the table's own row order
#'
#' `make_group_key()` rather than pasting with a separator: it length-prefixes
#' each component, so a key value containing the separator cannot be confused
#' with a boundary between two of them.
#' @noRd
share_row_keys <- function(data, key_cols) {
  make_group_key(as.data.frame(data, stringsAsFactors = FALSE), key_cols)
}

#' @noRd
new_weight_share_coverage <- function(
  target_scope,
  n_target_units,
  n_target_clusters,
  n_reached_clusters,
  n_unlinked_units,
  orphan_clusters = NULL,
  cluster_digest = NULL
) {
  list(
    target_scope = target_scope,
    n_target_units = as.integer(n_target_units),
    n_target_clusters = as.integer(n_target_clusters),
    n_reached_clusters = as.integer(n_reached_clusters),
    n_unlinked_units = as.integer(n_unlinked_units),
    # NULL means orphan coverage was not assessed.
    orphan_clusters = orphan_clusters,
    # NULL means the target cluster set was not fingerprinted.
    cluster_digest = cluster_digest
  )
}

#' Read a weight-share record under the law it names
#'
#' Ordered the same way `prepare_panel_record()` is, and for the same reason:
#' which law the record was written under decides what its fields mean, so the
#' algorithm and version are established before any field is read.
#'
#' `NULL` in is `NULL` out. An ordinary sample carries no transformation.
#' @noRd
prepare_weight_share_record <- function(record, what, call = caller_env()) {
  check_weight_share_record_supported(record, what, call = call)
  if (is_null(record)) {
    return(NULL)
  }
  check_weight_share_record_fields(record, what, call = call)
  record
}

#' @noRd
check_weight_share_record_supported <- function(
  record,
  what,
  call = caller_env()
) {
  if (is_null(record)) {
    return(invisible(NULL))
  }

  if (!is.list(record)) {
    abort_samplyr(
      c(
        "{what} is computed from the weight-share transformation, and this
         sample's transformation record is not a record.",
        "x" = "The sample carries {describe_record_value(record)} where the
               transformation belongs.",
        "i" = "A transformation record is a set of named fields. A bare value
               states neither the algorithm it was made under nor the version
               that fixes what its fields mean."
      ),
      class = "samplyr_error_weight_share_record_malformed",
      call = call
    )
  }

  algorithm <- record$algorithm
  if (!is_scalar_string(algorithm) || !identical(algorithm, weight_share_record_algorithm)) {
    abort_samplyr(
      c(
        "{what} is computed under the {.val {weight_share_record_algorithm}}
         weight-sharing law, which this record does not name.",
        "x" = "The record names {.val {algorithm %||% NA_character_}}.",
        "i" = "The weights a transformation produces mean what they mean under
               the algorithm that produced them."
      ),
      class = "samplyr_error_weight_share_record_malformed",
      call = call
    )
  }

  version <- record$version
  ok_version <- is.numeric(version) && length(version) == 1L &&
    !is.na(version) && version %in% supported_weight_share_record_versions
  if (!ok_version) {
    abort_samplyr(
      c(
        "{what} needs a transformation record this build knows how to read.",
        "x" = "The record is at version {version %||% NA}.",
        "i" = "This build reads
               version{?s} {supported_weight_share_record_versions}.",
        "i" = "A version outside that set describes fields whose meaning this
               build has not been taught."
      ),
      class = "samplyr_error_weight_share_record_unsupported",
      call = call
    )
  }

  invisible(NULL)
}

#' @noRd
weight_share_record_fields <- c(
  "algorithm", "version", "operator", "source_sample", "source_integrity",
  "source_key_cols", "target_key_cols", "source_row_keys", "target_row_keys",
  "target_cluster", "within_mode", "denominator", "coverage",
  "generated_cols", "result_integrity", "call"
)

#' The fields that cannot be filled until the result exists
#' @noRd
weight_share_attached_fields <- c("target_row_keys", "result_integrity")

#' @param attached Whether the record has been attached to its result. Two
#'   fields describe the result and are necessarily empty before there is one,
#'   so requiring them at construction would make a correct record fail and
#'   never requiring them would let an unattached one reach an exporter.
#' @noRd
check_weight_share_record_fields <- function(
  record,
  what,
  attached = TRUE,
  call = caller_env()
) {
  missing <- setdiff(weight_share_record_fields, names(record))
  # Test singleton cluster fields by name rather than value.
  if (length(missing) > 0) {
    abort_samplyr(
      c(
        "{what} needs a complete transformation record.",
        "x" = "The record is missing {.field {missing}}."
      ),
      class = "samplyr_error_weight_share_record_malformed",
      call = call
    )
  }

  if (attached) {
    empty <- weight_share_attached_fields[
      vapply(
        weight_share_attached_fields,
        function(f) is_null(record[[f]]),
        logical(1)
      )
    ]
    if (length(empty) > 0) {
      abort_samplyr(
        c(
          "{what} needs a transformation record that was attached to its
           result.",
          "x" = "{.field {empty}} {?is/are} empty.",
          "i" = "These describe the transformed sample and are filled when the
                 transformation is attached to it."
        ),
        class = "samplyr_error_weight_share_record_malformed",
        call = call
      )
    }
  }

  if (!is_valid_share_operator(record$operator)) {
    abort_samplyr(
      c(
        "{what} needs the recorded operator, and this record does not carry
         one.",
        "i" = "The operator is the transformation. Without it the target
               weights cannot be reproduced or applied to a replicate."
      ),
      class = "samplyr_error_weight_share_record_malformed",
      call = call
    )
  }

  check_record_enum(
    record$within_mode, "within_mode", weight_share_within_modes,
    what, call = call
  )
  check_record_enum(
    record$denominator$mode, "denominator$mode", weight_share_denominator_modes,
    what, call = call
  )
  check_record_enum(
    record$denominator$scale, "denominator$scale",
    weight_share_denominator_scales, what, call = call
  )
  check_record_enum(
    record$coverage$target_scope, "coverage$target_scope",
    weight_share_target_scopes, what, call = call
  )

  invisible(NULL)
}

#' @noRd
check_record_enum <- function(value, field, allowed, what, call = caller_env()) {
  if (is_scalar_string(value) && value %in% allowed) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{what} needs a transformation record whose {.field {field}} names a
       mode this build reads.",
      "x" = "The record carries {describe_record_value(value)}.",
      "i" = "Known: {.val {allowed}}."
    ),
    class = "samplyr_error_weight_share_record_malformed",
    call = call
  )
}

#' @noRd
is_valid_share_operator <- function(op) {
  if (!is.list(op)) {
    return(FALSE)
  }
  fields <- c("target_row", "source_row", "share", "n_target", "n_source")
  if (!all(fields %in% names(op))) {
    return(FALSE)
  }
  tryCatch(
    {
      validate_share_operator(op)
      TRUE
    },
    samplyr_error = function(e) FALSE
  )
}

## Integrity of a transformed sample

#' Columns whose values a transformed sample's integrity record covers
#'
#' The internal design columns, the target keys, and the link columns the
#' transformation generated. Target keys are included because the operator
#' addresses target rows positionally: a reordered or rewritten key column
#' means the recorded map no longer describes this table.
#'
#' The generated link columns are named explicitly rather than folded into
#' `samplyr_internal_col_pattern`. Widening that pattern would change which
#' columns count as internal for every sample in the package, including the
#' stripped-class detection, which is a blast radius this file has no reason
#' to take on.
#' @noRd
weight_share_protected_cols <- function(data, key_cols, generated_cols) {
  internal <- grep(samplyr_internal_col_pattern, names(data), value = TRUE)
  unique(intersect(
    c(internal, key_cols, generated_cols),
    names(data)
  ))
}

#' @noRd
weight_share_integrity_record <- function(data, key_cols, generated_cols) {
  cols <- weight_share_protected_cols(data, key_cols, generated_cols)
  list(
    n_rows = nrow(data),
    cols = cols,
    hash = protected_values_hash(data, cols)
  )
}

#' Attach a completed transformation to its result
#'
#' Mints a fresh integrity record for the transformed sample and stores a copy
#' inside the transformation record. A shared-weight sample is a legitimately
#' modified sample, so it must not inherit the source's integrity record and it
#' must not be reachable through the tampering gate: `check_sample_unmodified()`
#' would tell a user their data was corrupted when it was transformed on
#' purpose.
#'
#' Any modification marks carried over from the source are cleared. They
#' describe the source realization and the result is not it.
#' @noRd
attach_weight_share_record <- function(result, record, call = caller_env()) {
  row_keys <- share_row_keys(result, record$target_key_cols)
  # Duplicate target keys make realignment ambiguous.
  dup <- unique(row_keys[duplicated(row_keys)])
  if (length(dup) > 0) {
    abort_samplyr(
      c(
        "Target rows must be uniquely identified by
         {.field {record$target_key_cols}}.",
        "x" = "{length(dup)} key value{?s} name{?s/} more than one row.",
        "i" = "The transformation addresses target rows by position and
               resolves them by key, so a key naming two rows cannot say which
               weight belongs to which."
      ),
      class = "samplyr_error_weight_share_duplicate_key",
      call = call
    )
  }

  integrity <- weight_share_integrity_record(
    result, record$target_key_cols, record$generated_cols
  )
  record$result_integrity <- integrity
  record$target_row_keys <- row_keys

  meta <- attr(result, "metadata") %||% list()
  meta$integrity <- integrity
  meta$modified <- NULL
  meta$weight_share <- record
  attr(result, "metadata") <- meta
  result
}

#' Put a transformed sample back into the order the operator was recorded in
#'
#' The integrity hash is order-invariant on purpose: for an ordinary sample
#' nothing is positional, so `arrange()` or a sorted join is harmless and must
#' not read as tampering. A share operator *is* positional, so for a
#' transformed sample the order is load-bearing and has to be recovered rather
#' than assumed.
#'
#' Recovered, not refused: reordering a transformed sample is an ordinary thing
#' to do to a table, and the keys say exactly which row is which.
#'
#' @return An integer vector `pos` with `x[pos, ]` in recorded order.
#' @noRd
align_share_rows <- function(x, record, fn_name, call = caller_env()) {
  current <- share_row_keys(x, record$target_key_cols)
  pos <- match(record$target_row_keys, current)

  if (anyNA(pos) || anyDuplicated(current) > 0L) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} cannot line this sample up with its recorded
         transformation.",
        "x" = if (anyDuplicated(current) > 0L) {
          "{.field {record$target_key_cols}} no longer identifies target rows
           uniquely."
        } else {
          "{sum(is.na(pos))} target row{?s} the transformation produced
           {?is/are} no longer present."
        },
        "i" = "The recorded weights belong to specific target units. Rows may
               be reordered, but a row set that differs from the recorded one
               is a different table.",
        "i" = "Share weights again from the source sample rather than
               repairing the result."
      ),
      class = "samplyr_error_weight_share_misaligned",
      call = call
    )
  }
  pos
}

#' Verify a transformed sample against everything the record claims
#'
#' Four separate things can have gone wrong and they need separate findings:
#' the result was altered, the retained source sample was altered, the operator
#' no longer spans the tables it is being applied to, or the target rows were
#' merely reordered. The last is recoverable and the others are not, so
#' collapsing them would either refuse a harmless `arrange()` or accept a
#' rewritten table.
#'
#' @return "ok", "reordered", or the failure kind: "result", "source", or
#'   "dimensions".
#' @noRd
verify_weight_share_alignment <- function(x, record) {
  op <- record$operator
  source_sample <- record$source_sample

  if (
    nrow(x) != op$n_target ||
      nrow(source_sample) != op$n_source ||
      op$n_target != record$result_integrity$n_rows
  ) {
    return("dimensions")
  }
  if (!identical(verify_sample_integrity(x, record$result_integrity), "ok")) {
    return("result")
  }
  if (
    !identical(
      verify_sample_integrity(source_sample, record$source_integrity),
      "ok"
    )
  ) {
    return("source")
  }
  # Distinguish row permutation from value changes.
  if (!identical(share_row_keys(x, record$target_key_cols), record$target_row_keys)) {
    return("reordered")
  }
  "ok"
}

#' @noRd
check_weight_share_alignment <- function(x, fn_name, call = caller_env()) {
  record <- attr(x, "metadata")$weight_share
  if (is_null(record)) {
    return(invisible(NULL))
  }
  status <- verify_weight_share_alignment(x, record)
  # `align_share_rows()` recovers reordering.
  if (status %in% c("ok", "reordered")) {
    return(invisible(NULL))
  }

  bullets <- switch(
    status,
    result = c(
      "x" = "Rows or protected values of the transformed sample changed after
             the weights were shared."
    ),
    source = c(
      "x" = "The source sample retained by the transformation no longer matches
             the realization the weights were shared from."
    ),
    dimensions = c(
      "x" = "The recorded operator does not span these tables: it maps
             {record$operator$n_source} source row{?s} to
             {record$operator$n_target} target row{?s}."
    )
  )

  abort_samplyr(
    c(
      "{.fn {fn_name}} requires a transformed sample that still matches its
       recorded transformation.",
      bullets,
      "i" = "The operator addresses rows by position, so the target weights and
             every replicate built from them refer to rows that are no longer
             there.",
      "i" = "Share weights again from the source sample rather than repairing
             the result."
    ),
    class = "samplyr_error_weight_share_misaligned",
    call = call
  )
}

## The weight contract

#' What kind of weight a sample's `.weight` column holds
#'
#' The one place this question is answered. Every statistical consumer decides
#' explicitly what to do with a transformed sample, and it decides through this
#' rather than by testing for a metadata field, so adding a second kind of
#' transformation later is one change here and not eighteen elsewhere.
#'
#' @return `"design"` for Horvitz-Thompson or Hansen-Hurwitz design weights,
#'   `"shared"` for estimation weights produced by a recorded transformation.
#' @noRd
sample_weight_contract <- function(x) {
  if (is_null(attr(x, "metadata")$weight_share)) {
    "design"
  } else {
    "shared"
  }
}

#' @noRd
weight_contract_label <- c(
  design = "design weights",
  shared = "shared estimation weights"
)

#' Refuse a sample whose weights this operation is not defined for
#'
#' Distinct from `check_sample_unmodified()` on purpose. That gate says the
#' data no longer matches the design, which is a defect. This one says the
#' weights are a different quantity than the operation needs, which is a
#' property of a deliberately produced object, and telling a user the first
#' when the second is true sends them looking for corruption they will not
#' find.
#'
#' @param allowed The contracts this operation is defined for.
#' @param class The operation's own condition class. Callers pass one so a
#'   refusal can be caught for that operation specifically. The shared class
#'   is always appended, so a caller can also catch the whole family.
#' @param advice Bullets naming what to do instead. Left to the caller because
#'   the alternative differs by operation and generic advice would be worse
#'   than none.
#' @noRd
check_weight_contract <- function(
  x,
  fn_name,
  allowed = "design",
  class = NULL,
  advice = NULL,
  call = caller_env()
) {
  contract <- sample_weight_contract(x)
  if (contract %in% allowed) {
    return(invisible(NULL))
  }

  record <- attr(x, "metadata")$weight_share
  bullets <- if (identical(contract, "shared")) {
    c(
      "x" = "This sample's {.field .weight} holds
             {weight_contract_label[['shared']]} produced by
             {.fn share_weights}, not inverse inclusion probabilities.",
      "i" = "Its rows are target units reached through links from the sampled
             units, and its recorded design still describes the selection those
             links start from."
    )
  } else {
    c(
      "x" = "{.fn {fn_name}} needs
             {weight_contract_label[allowed]}, and this sample carries
             {weight_contract_label[[contract]]}."
    )
  }

  abort_samplyr(
    c(
      "{.fn {fn_name}} is not defined for this sample's weights.",
      bullets,
      advice
    ),
    class = c(class, "samplyr_error_weight_contract"),
    call = call
  )
}

## Consumer-specific refusals

# Each operation has a catchable class and specific recovery advice.

#' @noRd
check_weight_contract_joint <- function(x, fn_name, call = caller_env()) {
  check_weight_contract(
    x, fn_name,
    class = "samplyr_error_joint_weight_contract",
    advice = c(
      "i" = "Joint probabilities are a property of the selection, so ask the
             source sample the transformation was built from.",
      "i" = "The probability that two target units are both reached is a
             different quantity: it runs over every source unit linked to
             either of them."
    ),
    call = call
  )
}

#' @noRd
check_weight_contract_varcomp <- function(x, fn_name, call = caller_env()) {
  check_weight_contract(
    x, fn_name,
    class = "samplyr_error_varcomp_weight_contract",
    advice = c(
      "i" = "The decomposition attributes variance to the design's stages, and
             the rows of a transformed sample are not units those stages
             selected.",
      "i" = "Decompose the source sample instead."
    ),
    call = call
  )
}

#' Refuse a transformed sample as the starting point of more selection
#'
#' Covers both routes into `execute()`: continuing the stored design, and
#' supplying the sample as a frame for a further phase. One condition, because
#' the defect is the same one and the user's next step is the same either way.
#' @noRd
check_weight_contract_execute <- function(x, fn_name, call = caller_env()) {
  check_weight_contract(
    x, fn_name,
    class = "samplyr_error_execute_weight_contract",
    advice = c(
      "i" = "Selecting from these rows would treat a shared estimation weight
             as a first-phase inclusion probability, and the product would be
             neither.",
      "i" = "Run the further selection on the source sample, then share
             weights from its result."
    ),
    call = call
  )
}

#' @noRd
check_weight_contract_panel <- function(x, fn_name, call = caller_env()) {
  check_weight_contract(
    x, fn_name,
    class = "samplyr_error_panel_weight_contract",
    advice = c(
      "i" = "Panels and waves are properties of the units that were selected,
             and the rows here are target units reached through links.",
      # Do not advise an operation that also refuses waves.
      "i" = "Weight sharing and wave activation do not compose in either
             direction. Share weights from the master and analyse the result
             as one sample, or rotate panels and analyse each wave without
             sharing."
    ),
    call = call
  )
}

#' Refuse a transformed sample as the subject of a receipt
#'
#' The JSON format carries a design and an execution receipt, and nothing
#' else. Writing a transformed sample would produce a file describing the
#' source selection alone, which reads back as an ordinary sample and replays
#' to one: the links, the target data and the shared weights would be absent
#' with nothing marking their absence. Refused rather than warned about,
#' because the file would be indistinguishable from a correct one.
#' @noRd
check_weight_contract_serialize <- function(x, fn_name, call = caller_env()) {
  check_weight_contract(
    x, fn_name,
    class = "samplyr_error_serialize_weight_contract",
    advice = c(
      "i" = "The receipt reproduces the source selection only. Serialize the
             source sample the transformation was built from, and re-apply
             {.fn share_weights} to what {.fn replay_design} returns.",
      "i" = "{.fn saveRDS} preserves this object whole, links and target rows
             included."
    ),
    call = call
  )
}

#' The source design and the transformation that rebuild a shared sample
#'
#' What [read_design()] returns for a shared-weight sample file. It carries
#' the source selection's design and receipt, and the arguments
#' [share_weights()] was given, but neither the links nor the target register:
#' those are supplied to [replay_design()], the way a frame is.
#'
#' A `sampling_design` with an attribute rather than a list, because there is
#' exactly one design here and every design accessor should keep working on
#' it.
#' @noRd
new_shared_sample_design <- function(source, transformation) {
  structure(
    source,
    transformation = transformation,
    class = c("shared_sample_design", class(source))
  )
}

#' @noRd
is_shared_sample_design <- function(x) {
  inherits(x, "shared_sample_design")
}

## Generated columns

#' Refuse generated names the target data already uses
#'
#' Silently overwriting is the alternative, and it would replace a user's own
#' column with a quantity that happens to share its name. The refusal names
#' every colliding column at once rather than the first, because a user
#' renaming them wants the whole list.
#'
#' @param owner How to describe the table in the message.
#' @noRd
check_generated_cols <- function(
  data,
  cols,
  fn_name,
  owner = "targets",
  call = caller_env()
) {
  clash <- intersect(cols, names(data))
  if (length(clash) == 0) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn {fn_name}} generates {cli::qty(length(clash))}{?a column/columns}
       {.arg {owner}} already carries.",
      "x" = "Already present: {.field {clash}}.",
      "i" = "Rename {cli::qty(length(clash))}{?it/them} in {.arg {owner}}.
             Overwriting would replace your values with the transformation's
             own."
    ),
    class = "samplyr_error_generated_column_collision",
    call = call
  )
}

## Coverage at the analysis boundary

# Unreachable target clusters bias totals downward. Warn at the analysis
# boundary so multiframe coverage is assessed over the union. NULL means the
# question was not assessed while an empty set means none were found.

#' The clusters no component of a collection can reach
#'
#' A cluster is unreachable from the union only when every component names it,
#' and one component's silence counts as coverage only when it was describing
#' the same target clusters. That is what the digest establishes. Without it,
#' or with a component that cannot answer at all, the union is unknown rather
#' than assumed.
#'
#' A `NULL` record answers `NULL` to both questions, so a component with no
#' link structure at all lands in the same "unknown" as one whose register
#' never claimed to be the population.
#' @noRd
union_share_coverage <- function(records) {
  coverages <- lapply(records, function(record) record$coverage)
  orphans <- lapply(coverages, function(coverage) coverage$orphan_clusters)
  digests <- vapply(coverages, function(coverage) {
    coverage$cluster_digest %||% NA_character_
  }, character(1))

  if (any(vapply(orphans, is_null, logical(1))) || anyNA(digests)) {
    return(list(status = "unknown", clusters = NULL))
  }
  if (length(unique(digests)) > 1L) {
    return(list(status = "incompatible", clusters = NULL))
  }
  list(status = "known", clusters = Reduce(intersect, orphans))
}

#' Warn once about what the estimate cannot reach, and record it
#'
#' `where` names what the finding is about, so the stack's message says the
#' union rather than repeating a per-component one. It is a formal rather than
#' a field of `coverage`: neither `union_share_coverage()` nor
#' `stack_share_coverage()` produces it, every call site appended it by hand,
#' and a call site that forgot got an empty interpolation and a message with
#' no locus.
#' @noRd
report_share_coverage <- function(result, coverage, where, call = caller_env()) {
  # Force this early to report the missing argument clearly.
  force(where)
  attr(result, "samplyr_weight_share_coverage") <- coverage
  if (!identical(coverage$status, "known") || length(coverage$clusters) == 0) {
    return(result)
  }

  n <- length(coverage$clusters)
  shown <- utils::head(coverage$clusters, 5)
  cli_warn(
    c(
      "{n} target cluster{?s} cannot be reached{where}.",
      "x" = "{.val {shown}}{if (n > length(shown)) ' and more' else ''}.",
      "i" = "Constraint 2.1: a cluster with no link to any source population
             is never surveyed, so totals are understated by its share.",
      "i" = "This is a property of the links, not of the sample. Estimates
             describe the reachable part of the target population."
    ),
    class = "samplyr_warning_unlinked_cluster",
    call = call
  )
  result
}
