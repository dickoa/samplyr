#' Create a new sampling_design object
#'
#' Low-level constructor for sampling_design objects. Users should use
#' [sampling_design()] instead.
#'
#' @param title Optional title for the design
#' @param stages List of sampling_stage objects
#' @param current_stage Index of the current stage being built
#' @param validated Logical indicating if design has been validated
#'
#' @return A sampling_design object
#' @noRd
new_sampling_design <- function(
  title = NULL,
  stages = list(),
  current_stage = 0L,
  validated = FALSE
) {
  structure(
    list(
      title = title,
      stages = stages,
      current_stage = current_stage,
      validated = validated
    ),
    class = "sampling_design"
  )
}

#' Validate a sampling_design object
#' @noRd
validate_sampling_design <- function(x, call = caller_env()) {
  if (!inherits(x, "sampling_design")) {
    cli_abort("Object must be a {.cls sampling_design}", call = call)
  }

  if (!is_null(x$title) && !is_character(x$title)) {
    cli_abort("{.arg title} must be a character string", call = call)
  }

  if (!is.list(x$stages)) {
    cli_abort("{.arg stages} must be a list", call = call)
  }

  x
}

#' Test if object is a sampling_design
#'
#' @param x Object to test
#' @return Logical
#' @export
is_sampling_design <- function(x) {
  inherits(x, "sampling_design")
}

#' Create a new sampling_stage object
#'
#' Low-level constructor for sampling_stage objects.
#'
#' @param label Optional label for the stage
#' @param strata Stratification specification
#' @param clusters Cluster specification
#' @param draw_spec Draw specification
#'
#' @return A sampling_stage object
#' @noRd
new_sampling_stage <- function(
  label = NULL,
  strata = NULL,
  clusters = NULL,
  draw_spec = NULL
) {
  structure(
    list(
      label = label,
      strata = strata,
      clusters = clusters,
      draw_spec = draw_spec
    ),
    class = "sampling_stage"
  )
}

#' Test if object is a sampling_stage
#'
#' @param x Object to test
#' @return Logical
#' @noRd
is_sampling_stage <- function(x) {
  inherits(x, "sampling_stage")
}

#' Create a stratum specification
#'
#' @param vars Character vector of stratification variable names
#' @param alloc Allocation method
#' @param variance Variance data frame for Neyman/optimal allocation
#' @param cost Cost data frame for optimal allocation
#' @param cv Coefficient-of-variation data frame for power allocation
#' @param importance Importance/size data frame for power allocation
#' @param power Power exponent for power allocation
#'
#' @return A stratum_spec object
#' @noRd
new_stratum_spec <- function(
  vars,
  alloc = NULL,
  variance = NULL,
  cost = NULL,
  cv = NULL,
  importance = NULL,
  power = NULL
) {
  structure(
    list(
      vars = vars,
      alloc = alloc,
      variance = variance,
      cost = cost,
      cv = cv,
      importance = importance,
      power = power
    ),
    class = "stratum_spec"
  )
}

#' Create a cluster specification
#'
#' @param vars Character vector of clustering variable names
#'
#' @return A cluster_spec object
#' @noRd
new_cluster_spec <- function(vars) {
  structure(
    list(vars = vars),
    class = "cluster_spec"
  )
}

#' Create a draw specification
#'
#' @param n Sample size
#' @param frac Sampling fraction
#' @param method Selection method
#' @param mos Measure of size variable name
#' @param prn Permanent random number variable name
#' @param min_n Minimum sample size per stratum
#' @param max_n Maximum sample size per stratum
#' @param round Rounding method
#' @param control List of quosures for control sorting
#' @param certainty_size Absolute MOS threshold for certainty selection
#' @param certainty_prop Proportional MOS threshold for certainty selection
#'
#' @return A draw_spec object
#' @noRd
new_draw_spec <- function(
  n = NULL,
  frac = NULL,
  method = "srswor",
  mos = NULL,
  prn = NULL,
  aux = NULL,
  bounds = NULL,
  spread = NULL,
  min_n = NULL,
  max_n = NULL,
  round = "up",
  control = NULL,
  certainty_size = NULL,
  certainty_prop = NULL,
  certainty_overflow = "error",
  on_empty = "error",
  method_type = NULL,
  method_fixed = NULL,
  method_variance = NULL
) {
  method <- canonical_method_name(method, method_type)
  structure(
    list(
      n = n,
      frac = frac,
      method = method,
      mos = mos,
      prn = prn,
      aux = aux,
      bounds = bounds,
      spread = spread,
      min_n = min_n,
      max_n = max_n,
      round = round,
      control = control,
      certainty_size = certainty_size,
      certainty_prop = certainty_prop,
      certainty_overflow = certainty_overflow,
      on_empty = on_empty,
      method_type = method_type,
      method_fixed = method_fixed,
      method_variance = method_variance
    ),
    class = "draw_spec"
  )
}

#' Create a new tbl_sample object
#'
#' Low-level constructor for tbl_sample objects. This is the result
#' of executing a sampling design.
#'
#' @param data The sampled data frame
#' @param design The design that was executed
#' @param stages_executed Integer vector of executed stages
#' @param seed Random seed used
#' @param metadata Additional metadata
#'
#' @return A tbl_sample object
#' @noRd
new_tbl_sample <- function(
  data,
  design,
  stages_executed,
  seed = NULL,
  metadata = list()
) {
  if (!inherits(data, "tbl_df")) {
    data <- tibble::as_tibble(data)
  }
  structure(
    data,
    design = design,
    stages_executed = stages_executed,
    seed = seed,
    metadata = metadata,
    class = unique(c("tbl_sample", class(data)))
  )
}

#' Test if object is a tbl_sample
#'
#' @param x Object to test
#' @return Logical
#' @export
is_tbl_sample <- function(x) {
  inherits(x, "tbl_sample")
}

#' Coerce to a tbl_sample
#'
#' Restores the `tbl_sample` class on a data frame that carries sampling
#' attributes (`design`, `stages_executed`) but has lost its class, for
#' example after a tidyr operation such as [tidyr::uncount()].
#'
#' @section Class preservation:
#'
#' All dplyr verbs (`mutate`, `filter`, `select`, `*_join`, etc.)
#' preserve the `tbl_sample` class automatically.
#'
#' Preserving the class does not mean the sample remains analyzable:
#' operations that remove, add, or duplicate rows, or overwrite
#' internal design columns (`.weight`, `.fpc_k`, ...), mark the sample
#' as modified. Design-based computations ([as_svydesign()],
#' [joint_expectation()], [design_effect()]) reject modified samples;
#' see the Domain analysis section of [as_svydesign()]. Restoring the
#' class with `as_tbl_sample()` does not clear the mark: the data is
#' re-verified against the integrity record stored at execution, so a
#' stripped, altered, and restored object is detected.
#'
#' Some operations strip the class but keep the sampling attributes.
#' Use `as_tbl_sample()` to restore it:
#' - `tidyr::uncount()`
#' - `tibble::as_tibble()`
#' - `as.data.frame()`
#'
#' For an operational multistage listing, keep the unmodified partial sample
#' as the first argument to [execute()] and pass the expanded plain object only
#' as its frame; restoring the listing itself is not required. A plain object
#' that still carries sample provenance is rejected as the frame of a fresh
#' design execution, because that would rerun stage 1.
#'
#' Other operations strip both class and attributes and are
#' not recoverable. Use dplyr alternatives instead:
#' - `base::merge()` -- use `dplyr::left_join()` etc.
#' - `base::cbind()` -- use `dplyr::bind_cols()`
#' - `tidyr::pivot_longer()` / `tidyr::pivot_wider()`
#'
#' @param x A data frame with sampling attributes.
#' @param ... Not used.
#'
#' @return A `tbl_sample`, or an error if `x` does not carry the
#'   required attributes.
#'
#' @examples
#' design <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 20)
#'
#' sample <- execute(design, bfa_eas, seed = 42)
#'
#' # as_tibble() strips the class but keeps attributes
#' plain <- tibble::as_tibble(sample)
#' is_tbl_sample(plain)
#'
#' # as_tbl_sample() restores it
#' restored <- as_tbl_sample(plain)
#' is_tbl_sample(restored)
#' @export
as_tbl_sample <- function(x, ...) {
  UseMethod("as_tbl_sample")
}

#' @rdname as_tbl_sample
#' @export
as_tbl_sample.tbl_sample <- function(x, ...) {
  x
}

#' @rdname as_tbl_sample
#' @export
as_tbl_sample.data.frame <- function(x, ...) {
  design <- attr(x, "design")
  stages <- attr(x, "stages_executed")
  if (is.null(design) || is.null(stages)) {
    cli_abort(c(
      "Cannot coerce to {.cls tbl_sample}.",
      "i" = "The object does not carry the required sampling attributes."
    ))
  }
  out <- new_tbl_sample(
    data = x,
    design = design,
    stages_executed = stages,
    seed = attr(x, "seed"),
    metadata = attr(x, "metadata")
  )
  # Restoring the class does not launder modifications: verify the
  # data against the stored integrity record and mark discrepancies.
  apply_integrity_marks(out)
}

#' Get the design specification stored on a sample
#'
#' Returns the `sampling_design` metadata attached to a `tbl_sample`. This is
#' the original design object used to create the sample, including all stage
#' specifications, labels, stratification, clustering, and draw settings.
#'
#' This is especially useful for:
#' - inspecting the design after execution
#' - programmatic workflows that need to branch on design metadata
#' - continuation workflows, where later calls to [execute()] resume from a
#'   partially executed sample
#'
#' @param x A `tbl_sample` object.
#'
#' @return The `sampling_design` object stored in `x`.
#'
#' @examples
#' design <- sampling_design(title = "Example") |>
#'   stratify_by(region) |>
#'   draw(n = 20)
#'
#' sample <- execute(design, bfa_eas, seed = 1)
#'
#' get_design(sample)
#'
#' # The returned object is the original sampling_design
#' is_sampling_design(get_design(sample))
#' @export
get_design <- function(x) {
  if (!is_tbl_sample(x)) {
    cli_abort("{.arg x} must be a {.cls tbl_sample}")
  }
  attr(x, "design")
}

#' Get the executed stage numbers stored on a sample
#'
#' Returns the stage indices that have been executed for a `tbl_sample`.
#' For a fully executed single-stage design, this is typically `1L`. For
#' partial execution or continuation workflows, this indicates how far the
#' design has progressed.
#'
#' This is mainly useful for:
#' - checking whether a multi-stage design was executed completely
#' - writing continuation workflows that resume from the next stage
#' - inspecting samples created with `stages =` in [execute()]
#'
#' @param x A `tbl_sample` object.
#'
#' @return An integer vector of executed stage numbers.
#'
#' @examples
#' design <- sampling_design() |>
#'   add_stage("EAs") |>
#'     cluster_by(ea_id) |>
#'     draw(n = 10) |>
#'   add_stage("Households") |>
#'     draw(n = 5)
#'
#' stage1 <- execute(design, bfa_eas, stages = 1, seed = 1)
#' get_stages_executed(stage1)
#'
#' full_sample <- execute(stage1, bfa_eas, seed = 2)
#' get_stages_executed(full_sample)
#' @export
get_stages_executed <- function(x) {
  if (!is_tbl_sample(x)) {
    cli_abort("{.arg x} must be a {.cls tbl_sample}")
  }
  attr(x, "stages_executed")
}

#' Reconstruct a tbl_sample after dplyr operations
#'
#' This method ensures that the tbl_sample class is only preserved
#' when the essential column (.weight) is still present.
#' Operations like summarise() or count() that remove this column
#' will return a regular tibble instead.
#'
#' Operations that change the number of rows (filtering, joins that
#' drop or duplicate rows) keep the class but mark the sample as
#' modified: the stored design then no longer describes the data, so
#' design-based computations ([as_svydesign()], [joint_expectation()],
#' [design_effect()]) refuse the sample and point to survey-side
#' domain analysis instead. Dropping an internal design column
#' (for example `select()` without `.fpc_1`) is marked the same way,
#' because the export would silently change (a missing FPC column
#' falls back to no finite population correction). Reordering rows,
#' reordering columns, and adding ordinary data columns are harmless
#' and are not marked.
#'
#' @param data The modified data
#' @param template The original tbl_sample object
#' @return A tbl_sample if essential columns present, otherwise a tibble
#' @export
#' @keywords internal
dplyr_reconstruct.tbl_sample <- function(data, template) {
  if (inherits(template, "grouped_df")) {
    # Let the grouped_df method rebuild the grouping structure, then
    # re-attach the sample provenance with the usual marks.
    out <- NextMethod()
    if (!".weight" %in% names(out)) {
      return(out)
    }
    return(restore_tbl_sample(out, template))
  }
  restore_tbl_sample(data, template)
}

#' Attach sample provenance to a grouped data frame
#'
#' `tbl_sample` is placed ahead of `grouped_df` so samplyr's methods
#' (ungroup, the dplyr hooks, `[`) dispatch first; grouped verbs still
#' reach the `grouped_df` methods through NextMethod().
#' @noRd
as_grouped_sample <- function(out, template) {
  attr(out, "design") <- attr(template, "design")
  attr(out, "stages_executed") <- attr(template, "stages_executed")
  attr(out, "seed") <- attr(template, "seed")
  attr(out, "metadata") <- attr(template, "metadata")
  cls <- class(out)
  if (!"tbl_sample" %in% cls) {
    class(out) <- c("tbl_sample", cls)
  }
  out
}

#' Group a tbl_sample without losing sample provenance
#'
#' Grouping does not change rows or design columns, so a grouped
#' sample keeps its clean/modified state. `group_by()` and `ungroup()`
#' do not use the dplyr extension generics, so explicit methods are
#' required (see the dplyr extending documentation).
#'
#' @param .data A tbl_sample object
#' @param ... Grouping variables, passed to [dplyr::group_by()]
#' @param .add,.drop Passed to [dplyr::group_by()]
#' @return A grouped tbl_sample
#' @export
#' @keywords internal
group_by.tbl_sample <- function(
  .data,
  ...,
  .add = FALSE,
  .drop = dplyr::group_by_drop_default(.data)
) {
  out <- NextMethod()
  if (dplyr::is_grouped_df(out)) {
    as_grouped_sample(out, .data)
  } else {
    # group_by() with no variables returns an ungrouped frame
    restore_tbl_sample(out, .data)
  }
}

#' @rdname group_by.tbl_sample
#' @param x A grouped tbl_sample object
#' @export
#' @keywords internal
ungroup.tbl_sample <- function(x, ...) {
  out <- NextMethod()
  if (dplyr::is_grouped_df(out)) {
    # Partial ungrouping (ungroup(x, some_var)) keeps a grouped frame
    as_grouped_sample(out, x)
  } else {
    restore_tbl_sample(out, x)
  }
}

#' Centralized tbl_sample restoration
#'
#' Every method that rebuilds a tbl_sample from new data and a template
#' (dplyr_reconstruct, `[`, vec_restore, grouping) funnels through this
#' helper, so validity has a single definition:
#'
#' - `.weight` gone: not a sample anymore, demote to a plain tibble
#'   with the sampling attributes stripped.
#' - Row count changed: mark "rows".
#' - Internal design column missing: mark "columns".
#'
#' Marks give immediate feedback; the integrity record in
#' metadata$integrity remains the authoritative check at the analysis
#' boundary (sample_realization_status()).
#' @noRd
restore_tbl_sample <- function(data, template) {
  if (!".weight" %in% names(data)) {
    return(demote_to_tibble(data))
  }
  if (inherits(data, "grouped_df")) {
    # Grouped results keep their grouping structure; provenance is
    # attached on top (tbl_sample ahead of grouped_df in the class).
    out <- as_grouped_sample(data, template)
  } else {
    out <- new_tbl_sample(
      data = data,
      design = attr(template, "design"),
      stages_executed = attr(template, "stages_executed"),
      seed = attr(template, "seed"),
      metadata = attr(template, "metadata")
    )
  }
  if (nrow(data) != nrow(template)) {
    out <- mark_sample_modified(out, "rows")
  }
  internal_cols <- grep(
    samplyr_internal_col_pattern,
    names(template),
    value = TRUE
  )
  if (!all(internal_cols %in% names(data))) {
    out <- mark_sample_modified(out, "columns")
  }
  out
}

#' @noRd
demote_to_tibble <- function(data) {
  data <- tibble::as_tibble(data)
  class(data) <- setdiff(class(data), "tbl_sample")
  attr(data, "design") <- NULL
  attr(data, "stages_executed") <- NULL
  attr(data, "seed") <- NULL
  attr(data, "metadata") <- NULL
  data
}

#' Restore a tbl_sample after vctrs operations
#'
#' vctrs restores attributes from a prototype after operations like
#' [vctrs::vec_rbind()]; the default restoration copies them blindly,
#' which previously produced a "clean" doubled realization from
#' `vec_rbind(sample, sample)`. Routing through the shared restore
#' helper applies the same demotion and marking rules as the dplyr
#' hooks. (The integrity record catches base `rbind()` and any other
#' route at the analysis boundary regardless.)
#'
#' @param x The data to restore
#' @param to The tbl_sample prototype
#' @param ... Unused
#' @return A tbl_sample or plain tibble, per the restoration rules
#' @export
#' @keywords internal
vec_restore.tbl_sample <- function(x, to, ...) {
  restore_tbl_sample(x, to)
}

#' Track row-set changes from dplyr verbs
#'
#' filter(), slice(), arrange(), and friends funnel through
#' dplyr_row_slice(). Row removal or duplication is recorded on the
#' result (see [dplyr_reconstruct.tbl_sample()]); a pure reordering is
#' not. The location check catches same-length changes (for example
#' `slice(c(1, 1, 3:n))`) that a row-count comparison would miss.
#'
#' @param data A tbl_sample object
#' @param i Row locations, as passed by dplyr
#' @param ... Additional arguments passed to the default method
#' @return A tbl_sample, marked as modified if the row set changed
#' @export
#' @keywords internal
dplyr_row_slice.tbl_sample <- function(data, i, ...) {
  out <- NextMethod()
  # The grouped_df method rebuilds the result without consulting
  # dplyr_reconstruct() on our template, so re-attach provenance here.
  if (
    is.data.frame(out) &&
      !is_tbl_sample(out) &&
      ".weight" %in% names(out)
  ) {
    out <- restore_tbl_sample(out, data)
  }
  if (is_tbl_sample(out)) {
    loc <- vctrs::vec_as_location(i, n = nrow(data))
    if (length(loc) != nrow(data) || anyDuplicated(loc) > 0L) {
      out <- mark_sample_modified(out, "rows")
    }
  }
  out
}

#' Track renames of internal design columns
#'
#' [dplyr::rename()] assigns names directly instead of reconstructing,
#' so renamed internal columns are caught here. Renaming `.weight_1`
#' away breaks the export exactly like dropping it.
#'
#' @param x A tbl_sample object
#' @param value New column names
#' @return A tbl_sample, marked as modified if an internal design
#'   column was renamed
#' @export
#' @keywords internal
`names<-.tbl_sample` <- function(x, value) {
  internal_cols <- grep(samplyr_internal_col_pattern, names(x), value = TRUE)
  out <- NextMethod()
  if (is_tbl_sample(out) && !all(internal_cols %in% names(out))) {
    out <- mark_sample_modified(out, "columns")
  }
  out
}

#' Track overwrites of internal design columns
#'
#' mutate() and friends funnel through dplyr_col_modify(). Overwriting
#' or dropping an internal design column (`.weight`, `.weight_k`,
#' `.fpc_k`, `.draw_k`, `.certainty_k`, `.replicate`, `.sample_id`,
#' `.panel`) breaks the link between the data and the stored design,
#' so the result is marked as modified. Adding or changing ordinary
#' data columns is unaffected.
#'
#' @param data A tbl_sample object
#' @param cols Named list of modified columns, as passed by dplyr
#' @return A tbl_sample, marked as modified if a design column changed
#' @export
#' @keywords internal
dplyr_col_modify.tbl_sample <- function(data, cols) {
  out <- NextMethod()
  # See dplyr_row_slice.tbl_sample(): the grouped_df method bypasses
  # our reconstruct, so re-attach provenance.
  if (
    is.data.frame(out) &&
      !is_tbl_sample(out) &&
      ".weight" %in% names(out)
  ) {
    out <- restore_tbl_sample(out, data)
  }
  if (is_tbl_sample(out)) {
    touched <- intersect(names(cols), names(data))
    if (any(grepl(samplyr_internal_col_pattern, touched))) {
      out <- mark_sample_modified(out, "columns")
    }
  }
  out
}

#' Subset a tbl_sample preserving class
#'
#' Subsetting a tbl_sample with `[` preserves the tbl_sample class
#' and its sampling metadata when the essential column (.weight)
#' remains. Row subsetting that changes the number of rows, and column
#' subsetting that drops internal design columns (`.fpc_k`,
#' `.weight_k`, ...), mark the sample as modified, consistent with
#' [dplyr::filter()] and [dplyr::select()] (see
#' [dplyr_reconstruct.tbl_sample()]).
#'
#' @param x A tbl_sample object
#' @param i Row index
#' @param j Column index
#' @param ... Additional arguments passed to the default method
#' @param drop Coerce to lowest possible dimension
#' @return A tbl_sample if essential columns remain, otherwise a data.frame
#' @method [ tbl_sample
#' @export
`[.tbl_sample` <- function(x, i, j, ..., drop = FALSE) {
  # x[i, ] and x[i, j] pass three or more index arguments; x[i] (column
  # subsetting) passes two. Captured before NextMethod() consumes them.
  matrix_style <- (nargs() - !missing(drop)) >= 3L
  result <- NextMethod()
  if (!is.data.frame(result)) {
    return(result)
  }
  out <- restore_tbl_sample(result, x)
  if (
    is_tbl_sample(out) &&
      matrix_style &&
      !missing(i) &&
      nrow(result) == nrow(x)
  ) {
    # Same-length row subsets can still duplicate and drop rows
    # (x[c(1, 1, 3:n), ]); the row-count check in the restore helper
    # cannot see that, so inspect the locations directly.
    loc <- tryCatch(
      vctrs::vec_as_location(i, n = nrow(x)),
      error = function(cnd) NULL
    )
    if (
      !is_null(loc) &&
        (length(loc) != nrow(x) || anyDuplicated(loc) > 0L)
    ) {
      out <- mark_sample_modified(out, "rows")
    }
  }
  out
}
