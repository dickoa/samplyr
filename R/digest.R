#' Frame Digest Internals
#'
#' The frame digest is a compact, versioned execution manifest
#' attached to executed samples under `metadata$frame_digest`: frames,
#' selection pools, resolved chances, allocation, and the
#' selected-unit trace. It is a plain list of base-typed records and
#' data frames whose size scales with selection pools, retained
#' cluster units, and quantile bins rather than with frame rows.
#' Cluster stages keep one unit row per cluster for downstream
#' linkage, so a stage whose clusters are single frame rows grows
#' with the frame; `execute(frame_digest = "none")` opts out.
#'
#' @name digest
#' @keywords internal
NULL

#' Current frame digest schema version
#' @noRd
frame_digest_version <- 2L

#' @noRd
digest_status_values <- c("complete", "partial", "invalidated")

#' @noRd
digest_privacy_modes <- c("summary", "full")

#' @noRd
digest_scope_values <- c(
  "eligible",
  "universe",
  "conditional",
  "partial",
  "unknown"
)

#' @noRd
digest_chance_kind_values <- c("inclusion_probability", "expected_hits")

#' Per-stage probabilities tier: do the recorded chances equal the
#' design's true first-order chances, or a target the method honors to
#' a documented approximation (SPS, Pareto, registered "approximate")?
#' @noRd
digest_probabilities_values <- c("exact", "approximate")

#' @noRd
digest_order_kind_values <- c("input", "control", "method", "conventional")

#' @noRd
digest_storage_values <- c("constant", "units", "quantiles")

#' @noRd
digest_chance_status_values <- c(
  "executed",
  "design_resolved",
  "summarized",
  "unavailable"
)

#' Required columns of the per-stage tables
#' @noRd
digest_pool_cols <- c(
  "pool_id",
  "parent_unit",
  "N",
  "n_target",
  "n_expected",
  "n_realized",
  "scope",
  "chance_status"
)

#' @noRd
digest_unit_cols <- c(
  "unit_id",
  "pool_id",
  "unit_order",
  "chance",
  "is_certainty"
)

#' @noRd
digest_distribution_cols <- c("pool_id", "quantile", "chance")

#' @noRd
digest_selected_cols <- c("pool_id", "unit_id", "occurrence")

#' Pool-table column names that stratum columns must not collide with
#' @noRd
digest_reserved_pool_cols <- c(
  digest_pool_cols,
  "chance",
  "n_descendants",
  "parent_occurrence",
  "stage",
  "take_rate",
  "unit_level",
  "chance_kind",
  "probabilities",
  "storage",
  "n_pools"
)

#' Build one frame registry record
#'
#' The digest constructors are light-weight builders for the execution
#' hot path. They shape and coerce; the full invariant matrix lives in
#' validate_frame_digest(), which runs once on the assembled digest.
#'
#' @param frame_id Integer identifier, unique within the digest.
#' @param fingerprint_exact Exact-replay content hash of the frame
#'   (frame_content_hash), or NULL when not retained.
#' @param fingerprint_roles Role-scoped content hash covering only the
#'   design-relevant columns, or NULL.
#' @param n_rows Number of rows the frame had at execution.
#' @param roles Data frame with columns `column` and `role` naming the
#'   design-relevant columns, or NULL.
#' @param scope Population scope of the frame as supplied.
#' @noRd
new_digest_frame <- function(
  frame_id,
  fingerprint_exact = NULL,
  fingerprint_roles = NULL,
  n_rows,
  roles = NULL,
  scope = "universe"
) {
  list(
    frame_id = as.integer(frame_id),
    fingerprint_exact = fingerprint_exact,
    fingerprint_roles = fingerprint_roles,
    n_rows = as.integer(n_rows),
    roles = roles,
    scope = scope
  )
}

#' Build one stage manifest record
#'
#' @param stage_id Integer stage identifier, unique within the digest.
#' @param frame_ref frame_id of the frame this stage selected from.
#' @param unit_level What one selection unit is at this stage, for
#'   example "cluster" or "element".
#' @param scope Completeness of the recorded population representation.
#' @param chance_kind "inclusion_probability" (without replacement) or
#'   "expected_hits" (with replacement / multiplicity based).
#' @param probabilities "exact" when the recorded chances equal the
#'   design's true first-order chances, "approximate" when the method
#'   honors them as a target to a documented approximation, or NULL
#'   when the tier is not recorded (digests written before the field).
#' @param order_kind How unit order was determined: "input", "control",
#'   "method", or "conventional".
#' @param storage Resolution of the population representation:
#'   "constant", "units", or "quantiles".
#' @param pools Data frame, one row per selection pool (full parent
#'   ancestry x stage strata). Required columns: pool_id, parent_unit,
#'   N, n_target, n_expected, n_realized, scope, chance_status.
#'   Optional: chance (constant storage only), n_descendants, plus the
#'   stratum label columns named by `strata`.
#' @param units Data frame, one anonymous row per population unit.
#'   Present iff storage is "units". Columns: unit_id, pool_id,
#'   unit_order, chance, is_certainty; optional n_descendants.
#' @param chance_distribution Data frame of compressed chance
#'   quantiles. Present iff storage is "quantiles". Columns: pool_id,
#'   quantile, chance; optional n_units (units per bin, summing to the
#'   pool size, which makes the weighted mean reproduce n_expected
#'   exactly).
#' @param selected Selected-unit trace: one row per selected
#'   occurrence. Required columns: pool_id, unit_id, occurrence;
#'   optional replicate, unit_order, sample_row, and key (full
#'   ancestry key of a selected cluster, derivable from the sample
#'   rows; continuations use it to link later pools to this stage's
#'   units). NULL when nothing was selected at this stage.
#' @param strata Character vector naming the stratum label columns in
#'   `pools`, or NULL when the stage is unstratified.
#' @param diagnostics Optional named list of method diagnostics
#'   (balance, bounds, spatial).
#' @noRd
new_digest_stage <- function(
  stage_id,
  frame_ref,
  unit_level,
  scope,
  chance_kind,
  order_kind,
  storage,
  pools,
  units = NULL,
  chance_distribution = NULL,
  selected = NULL,
  strata = NULL,
  diagnostics = NULL,
  probabilities = NULL
) {
  list(
    stage_id = as.integer(stage_id),
    frame_ref = as.integer(frame_ref),
    unit_level = unit_level,
    scope = scope,
    chance_kind = chance_kind,
    probabilities = probabilities,
    order_kind = order_kind,
    storage = storage,
    strata = strata,
    pools = pools,
    units = units,
    chance_distribution = chance_distribution,
    selected = selected,
    diagnostics = diagnostics
  )
}

#' Privacy record of a digest
#' @noRd
digest_privacy <- function(
  mode = "summary",
  stable_keys = FALSE,
  labels_retained = FALSE
) {
  list(
    mode = mode,
    stable_keys = stable_keys,
    labels_retained = labels_retained
  )
}

#' Assemble a frame digest
#'
#' @param frames List of records built by new_digest_frame().
#' @param stages List of records built by new_digest_stage(), in
#'   execution order.
#' @param privacy Record built by digest_privacy().
#' @param status "complete", "partial", or "invalidated".
#' @noRd
new_frame_digest <- function(
  frames,
  stages,
  privacy = digest_privacy(),
  status = "complete"
) {
  list(
    version = frame_digest_version,
    status = status,
    privacy = privacy,
    frames = frames,
    stages = stages
  )
}

#' Abort with a digest validation error
#'
#' Deep call chain below validate_frame_digest(), so call = NULL.
#' @noRd
abort_digest <- function(message, what, envir = parent.frame()) {
  abort_samplyr(
    message,
    class = c(
      paste0("samplyr_error_digest_", what),
      "samplyr_error_digest_invalid"
    ),
    call = NULL,
    envir = envir
  )
}

#' @noRd
is_scalar_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

#' @noRd
is_scalar_flag <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' @noRd
is_scalar_count <- function(x) {
  is.numeric(x) &&
    length(x) == 1L &&
    !is.na(x) &&
    is.finite(x) &&
    x >= 0 &&
    abs(x - round(x)) < sqrt(.Machine$double.eps)
}

#' Require the exact frame digest schema supported by this release
#' @noRd
check_frame_digest_version <- function(version) {
  if (!is_scalar_count(version)) {
    abort_digest(
      "The frame digest has no valid {.field version} field.",
      "malformed"
    )
  }
  if (version == frame_digest_version) {
    return(invisible(NULL))
  }
  if (version < frame_digest_version) {
    abort_digest(
      c(
        "Frame digest schema version {version} is unsupported by this
         version of samplyr.",
        "i" = "Use the samplyr version that wrote this artifact to
               read it.",
        "i" = "To use the current samplyr, re-execute the original
               design and save it again."
      ),
      "version"
    )
  }
  abort_digest(
    c(
      "Frame digest schema version {version} is newer than the
       supported version {frame_digest_version}.",
      "i" = "Update samplyr to a newer version to read this artifact."
    ),
    "version"
  )
}

#' Finite integerish vector with no NA
#' @noRd
is_id_vector <- function(x) {
  is.numeric(x) &&
    all(is.finite(x)) &&
    all(abs(x - round(x)) < sqrt(.Machine$double.eps))
}

#' Validate every invariant of a frame digest
#'
#' Checks the digest against the schema contract: identifier
#' uniqueness, parent links, chance ranges by chance_kind, storage and
#' scope declarations, n_expected consistency with the retained chance
#' representation, selected-trace linkage, and certainty declarations.
#'
#' @param x A frame digest list.
#' @param tol Absolute-relative tolerance for exact representations
#'   (constant and units storage) and for certainty declarations.
#' @param quantile_tol Looser relative tolerance for n_expected checks
#'   against compressed quantile representations.
#' @return `x`, invisibly.
#' @noRd
validate_frame_digest <- function(x, tol = 1e-6, quantile_tol = 0.05) {
  if (!is.list(x)) {
    abort_digest("A frame digest must be a list.", "malformed")
  }
  check_frame_digest_version(x$version)
  if (!is_scalar_string(x$status) || !x$status %in% digest_status_values) {
    abort_digest(
      "{.field status} must be one of {.val {digest_status_values}}.",
      "field"
    )
  }
  validate_digest_privacy(x$privacy)
  frame_ids <- validate_digest_frames(x$frames)

  stages <- x$stages
  if (!is.list(stages) || length(stages) == 0) {
    abort_digest(
      "A frame digest needs at least one stage record.",
      "malformed"
    )
  }
  stage_ids <- vapply(
    stages,
    function(s) {
      id <- s$stage_id
      if (!is_scalar_count(id)) {
        abort_digest(
          "Every stage record needs an integer {.field stage_id}.",
          "field"
        )
      }
      as.integer(id)
    },
    integer(1)
  )
  if (anyDuplicated(stage_ids)) {
    abort_digest("Stage ids must be unique.", "duplicate_id")
  }

  prev <- NULL
  for (pos in seq_along(stages)) {
    validate_digest_stage(
      stages[[pos]],
      pos,
      frame_ids,
      prev,
      tol = tol,
      quantile_tol = quantile_tol
    )
    prev <- stages[[pos]]
  }
  invisible(x)
}

#' @noRd
validate_digest_privacy <- function(privacy) {
  if (
    !is.list(privacy) ||
      !is_scalar_string(privacy$mode) ||
      !privacy$mode %in% digest_privacy_modes ||
      !is_scalar_flag(privacy$stable_keys) ||
      !is_scalar_flag(privacy$labels_retained)
  ) {
    abort_digest(
      c(
        "{.field privacy} must be a list with {.field mode} in
         {.val {digest_privacy_modes}} and logical scalars
         {.field stable_keys} and {.field labels_retained}.",
        "i" = "Use {.fn digest_privacy} to build it."
      ),
      "field"
    )
  }
  invisible(privacy)
}

#' @return Integer vector of validated frame ids.
#' @noRd
validate_digest_frames <- function(frames) {
  if (!is.list(frames) || length(frames) == 0) {
    abort_digest(
      "A frame digest needs at least one frame record.",
      "malformed"
    )
  }
  ids <- vapply(
    frames,
    function(f) {
      if (!is.list(f) || !is_scalar_count(f$frame_id)) {
        abort_digest(
          "Every frame record needs an integer {.field frame_id}.",
          "field"
        )
      }
      if (!is_scalar_count(f$n_rows)) {
        abort_digest(
          "Frame record {f$frame_id} needs an integer {.field n_rows}.",
          "field"
        )
      }
      if (!is_scalar_string(f$scope) || !f$scope %in% digest_scope_values) {
        abort_digest(
          "Frame record {f$frame_id}: {.field scope} must be one of
           {.val {digest_scope_values}}.",
          "field"
        )
      }
      for (fp in c("fingerprint_exact", "fingerprint_roles")) {
        if (!is_null(f[[fp]]) && !is_scalar_string(f[[fp]])) {
          abort_digest(
            "Frame record {f$frame_id}: {.field {fp}} must be a string
             or NULL.",
            "field"
          )
        }
      }
      if (
        !is_null(f$roles) &&
          !(is.data.frame(f$roles) &&
            all(c("column", "role") %in% names(f$roles)))
      ) {
        abort_digest(
          "Frame record {f$frame_id}: {.field roles} must be NULL or a
           data frame with columns {.field column} and {.field role}.",
          "field"
        )
      }
      as.integer(f$frame_id)
    },
    integer(1)
  )
  if (anyDuplicated(ids)) {
    abort_digest("Frame ids must be unique.", "duplicate_id")
  }
  ids
}

#' @noRd
validate_digest_stage <- function(
  stage,
  pos,
  frame_ids,
  prev,
  tol,
  quantile_tol
) {
  id <- stage$stage_id

  if (!is_scalar_count(stage$frame_ref) || !stage$frame_ref %in% frame_ids) {
    abort_digest(
      "Stage {id}: {.field frame_ref} does not match any recorded
       frame.",
      "broken_link"
    )
  }
  if (!is_scalar_string(stage$unit_level) || !nzchar(stage$unit_level)) {
    abort_digest(
      "Stage {id}: {.field unit_level} must be a non-empty string.",
      "field"
    )
  }
  enums <- list(
    scope = digest_scope_values,
    chance_kind = digest_chance_kind_values,
    order_kind = digest_order_kind_values,
    storage = digest_storage_values
  )
  for (field in names(enums)) {
    value <- stage[[field]]
    allowed <- enums[[field]]
    if (!is_scalar_string(value) || !value %in% allowed) {
      abort_digest(
        "Stage {id}: {.field {field}} must be one of {.val {allowed}}.",
        "field"
      )
    }
  }
  # Optional: digests written before the field carry no tier.
  if (
    !is_null(stage$probabilities) &&
      (!is_scalar_string(stage$probabilities) ||
         !stage$probabilities %in% digest_probabilities_values)
  ) {
    abort_digest(
      "Stage {id}: {.field probabilities} must be NULL or one of
       {.val {digest_probabilities_values}}.",
      "field"
    )
  }

  pools <- validate_digest_pools(stage, pos, prev, tol)
  units <- validate_digest_units(stage, pools, tol)
  validate_digest_distribution(stage, pools, tol)
  validate_digest_n_expected(stage, pools, units, tol, quantile_tol)
  validate_digest_selected(stage, pools, units)

  if (!is_null(stage$diagnostics) && !is.list(stage$diagnostics)) {
    abort_digest(
      "Stage {id}: {.field diagnostics} must be NULL or a list.",
      "field"
    )
  }
  invisible(stage)
}

#' @return The validated pools data frame.
#' @noRd
validate_digest_pools <- function(stage, pos, prev, tol) {
  id <- stage$stage_id
  pools <- stage$pools
  if (!is.data.frame(pools) || nrow(pools) == 0) {
    abort_digest(
      "Stage {id}: {.field pools} must be a data frame with at least
       one row.",
      "malformed"
    )
  }
  missing_cols <- setdiff(digest_pool_cols, names(pools))
  if (length(missing_cols) > 0) {
    abort_digest(
      "Stage {id}: {.field pools} is missing column{?s}
       {.field {missing_cols}}.",
      "malformed"
    )
  }
  if (!is_id_vector(pools$pool_id)) {
    abort_digest(
      "Stage {id}: {.field pool_id} must be integers without NA.",
      "field"
    )
  }
  if (anyDuplicated(pools$pool_id)) {
    abort_digest(
      "Stage {id}: pool ids must be unique within the stage.",
      "duplicate_id"
    )
  }

  strata <- stage$strata
  if (!is_null(strata)) {
    if (!is.character(strata) || anyNA(strata)) {
      abort_digest(
        "Stage {id}: {.field strata} must be a character vector of
         pool column names.",
        "field"
      )
    }
    missing_strata <- setdiff(strata, names(pools))
    if (length(missing_strata) > 0) {
      abort_digest(
        "Stage {id}: stratum column{?s} {.field {missing_strata}} not
         found in {.field pools}.",
        "broken_link"
      )
    }
    reserved <- intersect(strata, digest_reserved_pool_cols)
    if (length(reserved) > 0) {
      abort_digest(
        "Stage {id}: stratum column{?s} {.field {reserved}} collide
         with reserved digest column names.",
        "field"
      )
    }
  }

  for (col in c("scope", "chance_status")) {
    allowed <- if (col == "scope") {
      digest_scope_values
    } else {
      digest_chance_status_values
    }
    values <- pools[[col]]
    if (!is.character(values) || anyNA(values) || !all(values %in% allowed)) {
      abort_digest(
        "Stage {id}: pool {.field {col}} values must be one of
         {.val {allowed}}.",
        "field"
      )
    }
  }

  check_nonneg <- function(col, integerish = FALSE) {
    values <- pools[[col]]
    if (!is.numeric(values)) {
      abort_digest(
        "Stage {id}: pool column {.field {col}} must be numeric.",
        "field"
      )
    }
    present <- values[!is.na(values)]
    bad <- any(!is.finite(present)) ||
      any(present < 0) ||
      (integerish && !is_id_vector(present))
    if (length(present) > 0 && bad) {
      abort_digest(
        "Stage {id}: pool column {.field {col}} must contain
         non-negative {if (integerish) 'integers' else 'numbers'}.",
        "field"
      )
    }
  }
  check_nonneg("N", integerish = TRUE)
  check_nonneg("n_target")
  check_nonneg("n_expected")
  check_nonneg("n_realized", integerish = TRUE)

  if ("chance" %in% names(pools)) {
    check_chance_range(
      pools$chance[!is.na(pools$chance)],
      stage$chance_kind,
      "Stage {id}: pool {.field chance}",
      tol
    )
    if (stage$storage != "constant" && !all(is.na(pools$chance))) {
      abort_digest(
        "Stage {id}: pool {.field chance} may only carry values under
         {.val constant} storage; unit chances belong in the units or
         distribution table.",
        "storage"
      )
    }
  }
  if (stage$storage == "constant") {
    resolvable <- pools$chance_status != "unavailable"
    chance <- if ("chance" %in% names(pools)) pools$chance else NULL
    if (is_null(chance) || anyNA(chance[resolvable])) {
      abort_digest(
        "Stage {id}: {.val constant} storage requires a non-missing
         pool {.field chance} for every pool whose chance is not
         {.val unavailable}.",
        "storage"
      )
    }
  }

  if (stage$chance_kind == "inclusion_probability") {
    over <- !is.na(pools$n_realized) &
      !is.na(pools$N) &
      pools$n_realized > pools$N
    if (any(over)) {
      abort_digest(
        "Stage {id}: {.field n_realized} exceeds pool size {.field N}
         for pool{?s} {.val {pools$pool_id[over]}} in a
         without-replacement stage.",
        "allocation"
      )
    }
  }

  if ("n_descendants" %in% names(pools)) {
    check_nonneg("n_descendants", integerish = TRUE)
  }
  if ("parent_occurrence" %in% names(pools)) {
    occ <- pools$parent_occurrence
    if (!is_id_vector(occ) || any(occ < 1)) {
      abort_digest(
        "Stage {id}: {.field parent_occurrence} must be positive
         integers.",
        "field"
      )
    }
  }

  parent <- pools$parent_unit
  if (!is.numeric(parent) && !all(is.na(parent))) {
    abort_digest(
      "Stage {id}: {.field parent_unit} must be integer or NA.",
      "field"
    )
  }
  if (pos == 1L) {
    if (!all(is.na(parent))) {
      abort_digest(
        "Stage {id}: first-stage pools must not declare a
         {.field parent_unit}.",
        "broken_link"
      )
    }
  } else if (
    identical(prev$storage, "units") &&
      identical(prev$unit_level, "cluster")
  ) {
    if (anyNA(parent) || !is_id_vector(parent)) {
      abort_digest(
        "Stage {id}: later-stage pools must reference a
         {.field parent_unit} in the previous stage.",
        "broken_link"
      )
    }
    if (!all(parent %in% prev$units$unit_id)) {
      abort_digest(
        "Stage {id}: {.field parent_unit} references units that do not
         exist in stage {prev$stage_id}.",
        "broken_link"
      )
    }
  } else if (!all(is.na(parent))) {
    # After an element-level stage there is no cluster registry to
    # reference: parents must be absent, not invented.
    abort_digest(
      "Stage {id}: pools cannot reference a {.field parent_unit}
       because stage {prev$stage_id} retained no cluster registry.",
      "broken_link"
    )
  }
  pools
}

#' Range check for a chance vector under a chance kind
#' @noRd
check_chance_range <- function(
  values,
  chance_kind,
  where,
  tol,
  envir = parent.frame()
) {
  if (length(values) == 0) {
    return(invisible(NULL))
  }
  if (!is.numeric(values) || any(!is.finite(values))) {
    abort_digest(
      paste0(where, " values must be finite numbers."),
      "chance",
      envir = envir
    )
  }
  if (chance_kind == "inclusion_probability") {
    if (any(values < -tol) || any(values > 1 + tol)) {
      abort_digest(
        paste0(
          where,
          " values must lie in [0, 1] for inclusion probabilities."
        ),
        "chance",
        envir = envir
      )
    }
  } else if (any(values < -tol)) {
    abort_digest(
      paste0(where, " values must be non-negative for expected hits."),
      "chance",
      envir = envir
    )
  }
  invisible(NULL)
}

#' @return The validated units data frame, or NULL.
#' @noRd
validate_digest_units <- function(stage, pools, tol) {
  id <- stage$stage_id
  units <- stage$units
  if (stage$storage == "units" && is_null(units)) {
    abort_digest(
      "Stage {id}: {.val units} storage requires a {.field units}
       table.",
      "storage"
    )
  }
  if (stage$storage != "units" && !is_null(units)) {
    abort_digest(
      "Stage {id}: a {.field units} table is only allowed under
       {.val units} storage (declared: {.val {stage$storage}}).",
      "storage"
    )
  }
  if (is_null(units)) {
    return(NULL)
  }
  if (!is.data.frame(units) || nrow(units) == 0) {
    abort_digest(
      "Stage {id}: {.field units} must be a data frame with at least
       one row.",
      "malformed"
    )
  }
  missing_cols <- setdiff(digest_unit_cols, names(units))
  if (length(missing_cols) > 0) {
    abort_digest(
      "Stage {id}: {.field units} is missing column{?s}
       {.field {missing_cols}}.",
      "malformed"
    )
  }
  if (!is_id_vector(units$unit_id)) {
    abort_digest(
      "Stage {id}: {.field unit_id} must be integers without NA.",
      "field"
    )
  }
  if (anyDuplicated(units$unit_id)) {
    abort_digest(
      "Stage {id}: unit ids must be unique within the stage.",
      "duplicate_id"
    )
  }
  if (!all(units$pool_id %in% pools$pool_id)) {
    abort_digest(
      "Stage {id}: {.field units} references pools that are not in the
       pool registry.",
      "broken_link"
    )
  }
  if (!is_id_vector(units$unit_order)) {
    abort_digest(
      "Stage {id}: {.field unit_order} must be integers without NA.",
      "field"
    )
  }
  order_dup <- vapply(
    split(units$unit_order, units$pool_id),
    anyDuplicated,
    integer(1)
  )
  if (any(order_dup > 0)) {
    abort_digest(
      "Stage {id}: {.field unit_order} must be unique within each
       pool.",
      "field"
    )
  }

  status_by_pool <- pools$chance_status[
    match(units$pool_id, pools$pool_id)
  ]
  chance_na <- is.na(units$chance)
  if (any(chance_na & status_by_pool != "unavailable")) {
    abort_digest(
      "Stage {id}: unit {.field chance} may be NA only in pools whose
       chance is {.val unavailable}.",
      "chance"
    )
  }
  check_chance_range(
    units$chance[!chance_na],
    stage$chance_kind,
    "Stage {id}: unit {.field chance}",
    tol
  )

  if (!is.logical(units$is_certainty)) {
    abort_digest(
      "Stage {id}: {.field is_certainty} must be logical.",
      "field"
    )
  }
  if (stage$chance_kind == "inclusion_probability") {
    known <- !chance_na
    declared <- units$is_certainty[known]
    implied <- units$chance[known] >= 1 - tol
    if (anyNA(declared) || any(declared != implied)) {
      abort_digest(
        "Stage {id}: {.field is_certainty} must be TRUE exactly for
         units with inclusion probability 1.",
        "certainty"
      )
    }
  }

  if ("n_descendants" %in% names(units)) {
    desc <- units$n_descendants[!is.na(units$n_descendants)]
    if (length(desc) > 0 && (!is_id_vector(desc) || any(desc < 0))) {
      abort_digest(
        "Stage {id}: {.field n_descendants} must contain non-negative
         integers.",
        "field"
      )
    }
  }
  units
}

#' @noRd
validate_digest_distribution <- function(stage, pools, tol) {
  id <- stage$stage_id
  dist <- stage$chance_distribution
  if (stage$storage == "quantiles" && is_null(dist)) {
    abort_digest(
      "Stage {id}: {.val quantiles} storage requires a
       {.field chance_distribution} table.",
      "storage"
    )
  }
  if (stage$storage != "quantiles" && !is_null(dist)) {
    abort_digest(
      "Stage {id}: a {.field chance_distribution} table is only
       allowed under {.val quantiles} storage (declared:
       {.val {stage$storage}}).",
      "storage"
    )
  }
  if (is_null(dist)) {
    return(invisible(NULL))
  }
  if (!is.data.frame(dist) || nrow(dist) == 0) {
    abort_digest(
      "Stage {id}: {.field chance_distribution} must be a data frame
       with at least one row.",
      "malformed"
    )
  }
  missing_cols <- setdiff(digest_distribution_cols, names(dist))
  if (length(missing_cols) > 0) {
    abort_digest(
      "Stage {id}: {.field chance_distribution} is missing column{?s}
       {.field {missing_cols}}.",
      "malformed"
    )
  }
  if (!all(dist$pool_id %in% pools$pool_id)) {
    abort_digest(
      "Stage {id}: {.field chance_distribution} references pools that
       are not in the pool registry.",
      "broken_link"
    )
  }
  q <- dist$quantile
  if (!is.numeric(q) || anyNA(q) || any(q < 0) || any(q > 1)) {
    abort_digest(
      "Stage {id}: {.field quantile} values must lie in [0, 1].",
      "field"
    )
  }
  if (anyNA(dist$chance)) {
    abort_digest(
      "Stage {id}: {.field chance_distribution} chances must not be
       NA.",
      "chance"
    )
  }
  check_chance_range(
    dist$chance,
    stage$chance_kind,
    "Stage {id}: distribution {.field chance}",
    tol
  )
  if ("n_units" %in% names(dist)) {
    if (!is_id_vector(dist$n_units) || any(dist$n_units < 1)) {
      abort_digest(
        "Stage {id}: {.field n_units} must be positive integers.",
        "field"
      )
    }
    sums <- tapply(dist$n_units, dist$pool_id, sum)
    pool_N <- pools$N[match(as.numeric(names(sums)), pools$pool_id)]
    bad <- !is.na(pool_N) & as.numeric(sums) != pool_N
    if (any(bad)) {
      abort_digest(
        "Stage {id}: {.field n_units} must sum to the pool size
         {.field N} within each pool.",
        "field"
      )
    }
  }
  for (pool in split(dist, dist$pool_id)) {
    if (anyDuplicated(pool$quantile)) {
      abort_digest(
        "Stage {id}: duplicated {.field quantile} values within a
         pool.",
        "field"
      )
    }
    ord <- order(pool$quantile)
    if (any(diff(pool$chance[ord]) < -tol)) {
      abort_digest(
        "Stage {id}: {.field chance} must be non-decreasing in
         {.field quantile}; the table is a quantile function of the
         resolved chance.",
        "chance"
      )
    }
  }
  invisible(NULL)
}

#' n_expected must match the retained chance representation
#' @noRd
validate_digest_n_expected <- function(stage, pools, units, tol, quantile_tol) {
  id <- stage$stage_id
  checkable <- !is.na(pools$n_expected) &
    pools$chance_status != "unavailable"

  represented <- switch(
    stage$storage,
    constant = ifelse(is.na(pools$chance), NA_real_, pools$N * pools$chance),
    units = {
      sums <- vapply(
        split(units$chance, units$pool_id),
        function(v) sum(v, na.rm = TRUE),
        numeric(1)
      )
      out <- sums[match(as.character(pools$pool_id), names(sums))]
      unname(out)
    },
    quantiles = {
      dist <- stage$chance_distribution
      has_n <- "n_units" %in% names(dist)
      by_pool <- split(dist, dist$pool_id)
      vals <- vapply(
        by_pool,
        function(p) {
          if (has_n) sum(p$chance * p$n_units) else mean(p$chance)
        },
        numeric(1)
      )
      out <- unname(vals[match(as.character(pools$pool_id), names(by_pool))])
      if (has_n) out else out * pools$N
    }
  )
  use_tol <- if (stage$storage == "quantiles") quantile_tol else tol
  bad <- checkable &
    !is.na(represented) &
    abs(pools$n_expected - represented) > use_tol * pmax(1, pools$n_expected)
  if (any(bad)) {
    abort_digest(
      "Stage {id}: {.field n_expected} does not match the sum of
       retained chances for pool{?s} {.val {pools$pool_id[bad]}}.",
      "allocation"
    )
  }
  invisible(NULL)
}

#' @noRd
validate_digest_selected <- function(stage, pools, units) {
  id <- stage$stage_id
  selected <- stage$selected

  if (is_null(selected)) {
    realized <- pools$n_realized
    if (any(!is.na(realized) & realized > 0)) {
      abort_digest(
        "Stage {id}: pools declare selected units but the stage has no
         {.field selected} trace.",
        "trace"
      )
    }
    return(invisible(NULL))
  }
  if (!is.data.frame(selected)) {
    abort_digest(
      "Stage {id}: {.field selected} must be a data frame or NULL.",
      "malformed"
    )
  }
  missing_cols <- setdiff(digest_selected_cols, names(selected))
  if (length(missing_cols) > 0) {
    abort_digest(
      "Stage {id}: {.field selected} is missing column{?s}
       {.field {missing_cols}}.",
      "malformed"
    )
  }
  if (!all(selected$pool_id %in% pools$pool_id)) {
    abort_digest(
      "Stage {id}: {.field selected} references pools that are not in
       the pool registry.",
      "broken_link"
    )
  }
  if (!is_id_vector(selected$unit_id)) {
    abort_digest(
      "Stage {id}: selected {.field unit_id} must be integers without
       NA.",
      "field"
    )
  }
  if (!is_null(units)) {
    pos <- match(selected$unit_id, units$unit_id)
    if (anyNA(pos)) {
      abort_digest(
        "Stage {id}: {.field selected} references units that are not
         in the units table.",
        "broken_link"
      )
    }
    if (!all(units$pool_id[pos] == selected$pool_id)) {
      abort_digest(
        "Stage {id}: {.field selected} places units in a different
         pool than the units table.",
        "broken_link"
      )
    }
  }
  if (!is_id_vector(selected$occurrence) || any(selected$occurrence < 1)) {
    abort_digest(
      "Stage {id}: {.field occurrence} must be positive integers.",
      "trace"
    )
  }

  replicate <- selected[["replicate"]]
  if (is_null(replicate)) {
    replicate <- rep(1L, nrow(selected))
  } else if (!is_id_vector(replicate) || any(replicate < 1)) {
    abort_digest(
      "Stage {id}: {.field replicate} must be positive integers.",
      "trace"
    )
  }
  key <- paste(
    replicate,
    selected$pool_id,
    selected$unit_id,
    selected$occurrence
  )
  if (anyDuplicated(key)) {
    abort_digest(
      "Stage {id}: duplicated selected occurrences (same replicate,
       pool, unit, occurrence).",
      "duplicate_id"
    )
  }
  if (
    stage$chance_kind == "inclusion_probability" &&
      any(selected$occurrence != 1L)
  ) {
    abort_digest(
      "Stage {id}: a without-replacement stage cannot select the same
       unit more than once ({.field occurrence} > 1).",
      "trace"
    )
  }
  if ("sample_row" %in% names(selected)) {
    row <- selected$sample_row
    if (!is_id_vector(row) || any(row < 1)) {
      abort_digest(
        "Stage {id}: {.field sample_row} must be positive integers.",
        "trace"
      )
    }
  }
  if ("key" %in% names(selected)) {
    key <- selected[["key"]]
    if (!is.character(key) || anyNA(key)) {
      abort_digest(
        "Stage {id}: selected {.field key} must be character without
         NA.",
        "trace"
      )
    }
  }

  # Realized allocation must match the trace. Each trace row is one
  # selected occurrence, so per-pool row counts equal n_realized for
  # both chance kinds. With several replicates the per-pool count is
  # replicate-specific and n_realized must be left NA.
  n_reps <- length(unique(replicate))
  if (n_reps <= 1L) {
    counts <- table(selected$pool_id)
    traced <- as.integer(counts[
      match(as.character(pools$pool_id), names(counts))
    ])
    traced[is.na(traced)] <- 0L
    realized <- pools$n_realized
    bad <- !is.na(realized) & realized != traced
    if (any(bad)) {
      abort_digest(
        "Stage {id}: {.field n_realized} does not match the selected
         trace for pool{?s} {.val {pools$pool_id[bad]}}.",
        "allocation"
      )
    }
  }
  invisible(NULL)
}

#' Read the frame digest of an executed sample
#'
#' Low-level accessor to the frame digest that `execute()` records.
#' Returns `NULL` when the sample carries no digest. When a digest is
#' present, its effective status is computed lazily: the stored digest
#' is immutable, and a sample that no longer matches its executed
#' realization (per the sealed integrity record) reports status
#' `"invalidated"` without the digest itself being rewritten. Adding
#' analysis columns does not touch protected columns and therefore
#' does not invalidate.
#'
#' This is an extension API for packages that build on the digest
#' (such as samplens, which draws it as a sampling card). The version
#' field of the stored digest is validated before returning. The list
#' structure of the returned digest is internal and versioned: it may
#' change between samplyr releases, so extension packages should pin a
#' minimum samplyr version. [frame_summary()] is the stable tabular
#' interface for everyone else.
#'
#' @param x A `tbl_sample` produced by [execute()].
#' @return The frame digest list, or `NULL` when none is recorded.
#' @seealso [frame_summary()]
#' @keywords internal
#' @export
get_frame_digest <- function(x) {
  if (!is_tbl_sample(x)) {
    return(NULL)
  }
  digest <- attr(x, "metadata")$frame_digest
  if (is_null(digest)) {
    return(NULL)
  }
  check_frame_digest_version(digest$version)
  if (
    !identical(digest$status, "invalidated") &&
      !sample_realization_status(x)$ok
  ) {
    digest$status <- "invalidated"
  }
  digest
}

#' Attach a frame digest to a tbl_sample
#' @noRd
set_frame_digest <- function(x, digest, validate = TRUE) {
  if (validate) {
    validate_frame_digest(digest)
  }
  meta <- attr(x, "metadata") %||% list()
  meta$frame_digest <- digest
  attr(x, "metadata") <- meta
  x
}

#' Summarize the population structure recorded with a sample
#'
#' An executed sample can carry a frame digest: a compact record of the
#' frames, selection pools, resolved selection chances, and allocation
#' that each stage encountered at execution time. `frame_summary()`
#' turns that record into documented tibbles, so the structure of the
#' selection remains intelligible after the original frame has gone
#' away.
#'
#' @param x A `tbl_sample` produced by [execute()] that carries a
#'   frame digest, or a `sampling_design` restored by [read_design()]
#'   whose file was written from an executed sample: the execution
#'   receipt carries the digest, so a shipped design file supports
#'   next-wave planning without the frame or the sample.
#' @param stage An integer vector of stage numbers to report, or
#'   `NULL` (default) for all recorded stages.
#' @param scope Denominator basis for population sizes and take rates.
#'   With `"eligible"` (default), denominators cover the units that
#'   were eligible for the recorded execution. With `"universe"`,
#'   denominators are reported only where the digest covers the full
#'   population hierarchy. Denominators that the recorded scope cannot
#'   support are `NA`, never invented.
#' @param detail Resolution of the report: `"stage"` (one row per
#'   stage), `"pool"` (one row per selection pool), or `"unit"` (one
#'   row per population unit, only for stages that retained a
#'   unit-level representation).
#'
#' @return A tibble. The `scope` column always states how complete the
#'   underlying representation is (`"eligible"`, `"universe"`,
#'   `"conditional"`, `"partial"`, or `"unknown"`).
#'
#'   For `detail = "stage"`, one row per stage with `stage`,
#'   `unit_level`, `scope`, `chance_kind`, `probabilities`, `storage`,
#'   `n_pools`, `N`, `n_target`, `n_expected`, `n_realized`, and
#'   `take_rate`.
#'
#'   For `detail = "pool"`, one row per selection pool with `stage`,
#'   `pool_id`, `parent_unit`, any stratum label columns, `N`,
#'   `n_target`, `n_expected`, `n_realized`, `scope`, `chance_status`,
#'   `chance` (the constant per-unit chance where one applies, `NA`
#'   otherwise), and `take_rate`. Stratum columns of stages that do not
#'   use them are `NA`.
#'
#'   For `detail = "unit"`, one row per population unit of each stage
#'   that stored units, with `stage`, `pool_id`, `unit_id`,
#'   `unit_order`, `chance`, `is_certainty`, `n_descendants`,
#'   `is_selected`, and `n_hits`. Stages that stored only a constant or a
#'   chance distribution contribute no rows; requesting such a stage
#'   explicitly is an error rather than a silently empty result.
#'
#' @details
#' `take_rate` is `n_realized / N`. It is a take rate, not an
#' inclusion probability: for unequal-probability designs the two
#' differ by design. `chance_kind` states what the recorded chances
#' mean: first-order inclusion probabilities for without-replacement
#' stages, expected hits for with-replacement stages. `probabilities`
#' states how well the stage's method honors them: `"exact"` when the
#' recorded chances equal the design's true first-order chances,
#' `"approximate"` when the method treats them as a target achieved to
#' a documented approximation (`"pps_sps"`, `"pps_pareto"`, and
#' registered methods declared `probabilities = "approximate"`), and
#' `NA` for digests recorded before the field existed.
#'
#' Allocation is reported as three quantities that only coincide for
#' fixed-size designs: `n_target` (requested), `n_expected` (sum of
#' resolved chances), and `n_realized` (selected units or
#' occurrences). For replicated executions, allocation quantities are
#' per replicate: when the realized allocation is identical across
#' replicates the common value is reported, and when it varies
#' (random-size designs) `n_realized` and `take_rate` are `NA` rather
#' than a guess. Unit detail spans the stacked replicates:
#' `is_selected` marks units selected in at least one replicate and
#' `n_hits` counts occurrences across all replicates, so a
#' without-replacement unit can show `n_hits > 1`. The per-replicate
#' trace is the `replicate` column of the digest's selected units.
#' A replicated multi-stage execution records only the stage prefix
#' shared by every replicate: later-stage pools hang off each
#' replicate's own selected parents, so those stages are not part of
#' the manifest and `frame_summary()` says so.
#'
#' A digest whose sample was modified after execution (rows, weights,
#' or design columns changed) is reported as invalidated and refused:
#' a stale digest is worse than no digest.
#'
#' @seealso [execute()], [validate_frame()]
#' @export
frame_summary <- function(
  x,
  stage = NULL,
  scope = c("eligible", "universe"),
  detail = c("stage", "pool", "unit")
) {
  scope <- match.arg(scope)
  detail <- match.arg(detail)
  if (is_tbl_sample(x)) {
    digest <- get_frame_digest(x)
    if (is_null(digest)) {
      abort_samplyr(
        "No frame digest is recorded on this sample.",
        class = "samplyr_error_no_digest"
      )
    }
  } else if (is_sampling_design(x)) {
    # A design restored by read_design() carries the execution
    # receipt's digest: next-wave planning can read population counts
    # and realized allocations from the design file alone.
    digest <- attr(x, "execution")$frame_digest
    if (is_null(digest)) {
      abort_samplyr(
        c(
          "This design carries no execution receipt with a frame
           digest.",
          "i" = "Only designs restored by {.fn read_design} from a
                 file written from an executed sample carry one."
        ),
        class = "samplyr_error_no_digest"
      )
    }
  } else {
    abort_samplyr(
      "{.arg x} must be a {.cls tbl_sample} produced by
       {.fn execute}, or a {.cls sampling_design} restored by
       {.fn read_design} with an execution receipt."
    )
  }
  if (identical(digest$status, "invalidated")) {
    abort_samplyr(
      c(
        "The frame digest on this sample is invalidated.",
        "x" = "The sample no longer matches its executed realization,
               so the digest no longer describes it.",
        "i" = "Re-run {.fn execute} to obtain a sample with a valid
               digest."
      ),
      class = "samplyr_error_digest_invalidated"
    )
  }

  stages <- digest$stages
  stage_ids <- vapply(stages, function(s) s$stage_id, integer(1))

  # A replicated multi-stage execution records only the stage prefix
  # shared by every replicate: later-stage pools hang off each
  # replicate's own selected parents. Say so instead of silently
  # reporting fewer stages than were executed.
  dropped <- if (is_tbl_sample(x)) {
    setdiff(get_stages_executed(x), stage_ids)
  } else {
    integer(0)
  }
  replicated <- is_tbl_sample(x) && has_multiple_replicates(x)
  if (length(dropped) > 0 && is_null(stage)) {
    cli::cli_inform(c(
      "The digest records {cli::qty(length(stage_ids))} stage{?s}
       {.val {stage_ids}} of the {length(get_stages_executed(x))}
       executed.",
      "i" = if (replicated) {
        "Later stages are replicate-specific, execute with
         {.code reps = 1} for a full-depth digest."
      } else {
        "The remaining stages are not part of this manifest."
      }
    ))
  }

  if (!is_null(stage)) {
    if (!is_id_vector(stage) || length(stage) == 0) {
      abort_samplyr("{.arg stage} must be a vector of stage numbers.")
    }
    stage <- as.integer(stage)
    unknown <- setdiff(stage, stage_ids)
    if (length(unknown) > 0) {
      abort_samplyr(
        c(
          "Stage {.val {unknown}} not recorded in the frame digest.",
          "i" = "Recorded stages: {.val {stage_ids}}.",
          "i" = if (replicated && any(unknown %in% dropped)) {
            "Later stages are replicate-specific, execute with
             {.code reps = 1} for a full-depth digest."
          }
        )
      )
    }
    stages <- stages[stage_ids %in% stage]
  }

  switch(
    detail,
    stage = frame_summary_stage(stages, scope),
    pool = frame_summary_pool(stages, scope),
    unit = frame_summary_unit(stages, explicit = !is_null(stage))
  )
}

#' Sum that refuses to invent a total from incomplete parts
#' @noRd
sum_or_na <- function(values) {
  if (anyNA(values)) {
    return(NA_real_)
  }
  sum(as.double(values))
}

#' Does the recorded scope support a denominator under this basis?
#' @noRd
digest_scope_supports <- function(recorded, basis) {
  if (basis == "universe") {
    recorded == "universe"
  } else {
    recorded %in% c("eligible", "universe")
  }
}

#' @noRd
frame_summary_stage <- function(stages, scope) {
  rows <- lapply(stages, function(s) {
    pools <- s$pools
    # Design-resolved pools cover parents the execution never reached:
    # part of the universe, but not eligible for this realization.
    # Allocation quantities are conditional on the parent, so they sum
    # only over the executed pools under either basis.
    executed <- pools$chance_status != "design_resolved"
    N <- if (identical(scope, "universe")) {
      if (identical(s$scope, "universe")) sum_or_na(pools$N) else NA_real_
    } else if (digest_scope_supports(s$scope, scope)) {
      sum_or_na(pools$N[executed])
    } else {
      NA_real_
    }
    n_realized <- sum_or_na(pools$n_realized[executed])
    take_rate <- if (!is.na(N) && N > 0 && !is.na(n_realized)) {
      n_realized / N
    } else {
      NA_real_
    }
    data.frame(
      stage = s$stage_id,
      unit_level = s$unit_level,
      scope = s$scope,
      chance_kind = s$chance_kind,
      probabilities = s$probabilities %||% NA_character_,
      storage = s$storage,
      n_pools = nrow(pools),
      N = N,
      n_target = sum_or_na(pools$n_target[executed]),
      n_expected = sum_or_na(pools$n_expected[executed]),
      n_realized = n_realized,
      take_rate = take_rate
    )
  })
  as_tibble(do.call(rbind, rows))
}

#' @noRd
frame_summary_pool <- function(stages, scope) {
  rows <- lapply(stages, function(s) {
    pools <- s$pools
    supported <- digest_scope_supports(pools$scope, scope)
    if (identical(scope, "eligible")) {
      # A design-resolved pool was never eligible for this
      # realization; its 0/N is not an eligible take rate.
      supported <- supported & pools$chance_status != "design_resolved"
    }
    take_rate <- ifelse(
      supported & !is.na(pools$n_realized) & !is.na(pools$N) & pools$N > 0,
      pools$n_realized / pools$N,
      NA_real_
    )
    chance <- if (
      identical(s$storage, "constant") && "chance" %in% names(pools)
    ) {
      as.double(pools$chance)
    } else {
      rep(NA_real_, nrow(pools))
    }
    out <- data.frame(
      stage = rep(s$stage_id, nrow(pools)),
      pool_id = as.integer(pools$pool_id),
      parent_unit = as.integer(pools$parent_unit),
      N = as.double(pools$N),
      n_target = as.double(pools$n_target),
      n_expected = as.double(pools$n_expected),
      n_realized = as.double(pools$n_realized),
      scope = pools$scope,
      chance_status = pools$chance_status,
      chance = chance,
      take_rate = take_rate
    )
    strata <- s$strata %||% character(0)
    if (length(strata) > 0) {
      out <- cbind(out, pools[strata])
    }
    out
  })
  as_tibble(bind_rows(rows))
}

#' @noRd
frame_summary_unit <- function(stages, explicit) {
  has_units <- vapply(
    stages,
    function(s) identical(s$storage, "units"),
    logical(1)
  )
  if (explicit && any(!has_units)) {
    no_units <- vapply(
      stages[!has_units],
      function(s) s$stage_id,
      integer(1)
    )
    abort_samplyr(
      c(
        "Stage{?s} {.val {no_units}} did not retain a unit-level
         representation.",
        "i" = "Their storage is a constant or a chance distribution;
               unit detail is unavailable, not inferred."
      ),
      class = "samplyr_error_digest_no_units"
    )
  }
  stages <- stages[has_units]
  rows <- lapply(stages, function(s) {
    units <- s$units
    hits <- integer(nrow(units))
    selected <- s$selected
    if (!is_null(selected) && nrow(selected) > 0) {
      counts <- table(selected$unit_id)
      pos <- match(as.character(units$unit_id), names(counts))
      hits <- ifelse(is.na(pos), 0L, as.integer(counts[pos]))
    }
    n_descendants <- if ("n_descendants" %in% names(units)) {
      as.integer(units$n_descendants)
    } else {
      rep(NA_integer_, nrow(units))
    }
    data.frame(
      stage = rep(s$stage_id, nrow(units)),
      pool_id = as.integer(units$pool_id),
      unit_id = as.integer(units$unit_id),
      unit_order = as.integer(units$unit_order),
      chance = as.double(units$chance),
      is_certainty = units$is_certainty,
      n_descendants = n_descendants,
      is_selected = hits > 0L,
      n_hits = hits
    )
  })
  if (length(rows) == 0) {
    return(tibble::tibble(
      stage = integer(0),
      pool_id = integer(0),
      unit_id = integer(0),
      unit_order = integer(0),
      chance = double(0),
      is_certainty = logical(0),
      n_descendants = integer(0),
      is_selected = logical(0),
      n_hits = integer(0)
    ))
  }
  as_tibble(do.call(rbind, rows))
}
