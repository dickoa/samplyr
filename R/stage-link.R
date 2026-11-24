# Stage transitions use declared ancestry rather than shared columns.

#' Render compound keys as the user's values for a message
#'
#' Users see their own column values, never an internal key encoding. Every
#' key is returned, and a message bounds the list with `format_pool_sample()`.
#' @noRd
format_key_preview <- function(keys) {
  # apply() would pad numbers to a common width.
  do.call(paste, c(lapply(unname(as.list(keys)), value_text), sep = "/"))
}

#' Require the full declared ancestry on both sides of a transition
#' @noRd
check_parent_vars_present <- function(frame, previous_sample, parent_vars,
                                      design, stage_idx, frame_index,
                                      frame_label, call = caller_env()) {
  missing_frame <- setdiff(parent_vars, names(frame))
  if (length(missing_frame) > 0) {
    abort_samplyr(
      c(
        "{frame_token(frame_index, frame_label)}, used for
         {stage_token(design, stage_idx)}, is missing
         {.field {missing_frame}}.",
        "x" = "Linking a stage to the units its parent selected needs the
               complete ancestry: {.field {parent_vars}}.",
        "i" = "Add the missing ancestry to the frame, or supply a frame that
               already carries the parent identifiers."
      ),
      class = "samplyr_error_frame_missing_ancestry",
      call = call
    )
  }

  missing_sample <- setdiff(parent_vars, names(previous_sample))
  if (length(missing_sample) > 0) {
    abort_samplyr(
      c(
        "The sample being continued is missing {.field {missing_sample}}.",
        "x" = "{stage_token(design, stage_idx)} links through
               {.field {parent_vars}}, which the previous stages should have
               carried.",
        "i" = "This usually means the sample was modified after execution."
      ),
      class = "samplyr_error_frame_missing_ancestry",
      call = call
    )
  }
  invisible(NULL)
}

#' Refuse ancestry that cannot be joined without a lossy conversion
#' @noRd
check_parent_key_types <- function(frame, previous_sample, parent_vars,
                                   design, stage_idx, frame_index,
                                   frame_label, call = caller_env()) {
  for (var in parent_vars) {
    compatible <- tryCatch(
      {
        vctrs::vec_ptype2(frame[[var]], previous_sample[[var]])
        TRUE
      },
      error = function(e) FALSE
    )
    if (compatible) {
      next
    }
    frame_type <- vctrs::vec_ptype_full(frame[[var]])
    sample_type <- vctrs::vec_ptype_full(previous_sample[[var]])
    abort_samplyr(
      c(
        "{.field {var}} cannot link {frame_token(frame_index, frame_label)}
         to the selected units.",
        "x" = "The frame holds {.cls {frame_type}} and the sample holds
               {.cls {sample_type}}.",
        "i" = "Give the parent identifier the same type in both tables."
      ),
      class = "samplyr_error_frame_key_type",
      call = call
    )
  }
  invisible(NULL)
}

#' Refuse ancestry two registers cannot be joined on
#'
#' The preflight counterpart of `check_parent_key_types()`. No sample exists
#' yet, so each ancestry column is compared against the nearest earlier
#' register that supplies it, which is the register the linkage will match
#' against once the earlier stage has drawn.
#' @noRd
check_register_key_types <- function(design, schedule, call = caller_env()) {
  entries <- schedule$entries
  if (length(entries) < 2L) {
    return(invisible(NULL))
  }

  for (i in seq_along(entries)[-1]) {
    child <- entries[[i]]
    parent_vars <- collect_ancestor_cluster_vars(design, child$stage)
    for (var in parent_vars) {
      donor <- NULL
      for (j in rev(seq_len(i - 1L))) {
        if (var %in% names(entries[[j]]$frame)) {
          donor <- entries[[j]]
          break
        }
      }
      if (is_null(donor) ||
            identical(donor$frame_index, child$frame_index) ||
            !var %in% names(child$frame)) {
        next
      }
      compatible <- tryCatch(
        {
          vctrs::vec_ptype2(child$frame[[var]], donor$frame[[var]])
          TRUE
        },
        error = function(e) FALSE
      )
      if (compatible) {
        next
      }
      child_type <- vctrs::vec_ptype_full(child$frame[[var]])
      donor_type <- vctrs::vec_ptype_full(donor$frame[[var]])
      abort_samplyr(
        c(
          "{.field {var}} cannot link
           {frame_token(child$frame_index, child$frame_label)} to
           {frame_token(donor$frame_index, donor$frame_label)}.",
          "x" = "One holds {.cls {child_type}} and the other holds
                 {.cls {donor_type}}.",
          "i" = "Give the parent identifier the same type in both tables."
        ),
        class = "samplyr_error_frame_key_type",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' Report ancestry that names no parent
#'
#' Not an execution rule. A row with a missing parent key cannot match a
#' complete selected key, so execution filters it out like any other unrelated
#' row. If it was the only row representing a selected parent, the coverage
#' check reports that instead. Supplying several frames says nothing about the
#' granularity of their rows, so it cannot be grounds for rejecting them.
#'
#' This is the primitive behind the strict `validate_frame()` preflight, where
#' the user has asked for the frames to be checked rather than sampled.
#' @noRd
check_parent_key_na <- function(frame, parent_vars, design, stage_idx,
                                frame_index, frame_label,
                                call = caller_env()) {
  for (var in parent_vars) {
    n_missing <- sum(is.na(frame[[var]]))
    if (n_missing == 0) {
      next
    }
    abort_samplyr(
      c(
        "{frame_token(frame_index, frame_label)}, used for
         {stage_token(design, stage_idx)}, has {n_missing} row{?s} with a
         missing {.field {var}}.",
        "x" = "A row with no parent identifier belongs to no selected unit.",
        "i" = "Drop those rows or fill in the parent identifier."
      ),
      class = "samplyr_error_frame_ancestry_na",
      call = call
    )
  }
  invisible(NULL)
}

#' Rows whose key columns are all present under their class's `is.na()`
#'
#' `vctrs::vec_match()`, `stats::complete.cases()` and dplyr joins read the
#' stored value, so they miss a code a class declares missing, such as one of
#' haven's `labelled_spss`. Returns `NULL` when every row is complete, so the
#' common case allocates nothing.
#' @noRd
complete_key_rows <- function(frame, vars) {
  keep <- NULL
  for (v in vars) {
    col <- frame[[v]]
    if (anyNA(col)) {
      ok <- !is.na(col)
      keep <- if (is_null(keep)) ok else keep & ok
    }
  }
  keep
}

#' The rows of `frame` with complete keys, or `frame` itself
#' @noRd
slice_complete_keys <- function(frame, vars) {
  keep <- complete_key_rows(frame, vars)
  if (is_null(keep)) frame else vctrs::vec_slice(frame, keep)
}

#' For each row of `frame`, the first row of `previous_sample` with its key
#'
#' Only complete keys on both sides take part, so a row whose parent id is
#' missing matches no parent, whatever value it stores.
#' @noRd
match_complete_parents <- function(frame, previous_sample, parent_vars) {
  frame_ok <- complete_key_rows(frame, parent_vars)
  prev_ok <- complete_key_rows(previous_sample, parent_vars)
  prev_keys <- previous_sample[, parent_vars, drop = FALSE]
  if (!is_null(prev_ok)) {
    prev_keys <- vctrs::vec_slice(prev_keys, prev_ok)
  }
  parent_row <- vctrs::vec_match(frame[, parent_vars, drop = FALSE], prev_keys)
  if (!is_null(prev_ok)) {
    parent_row <- which(prev_ok)[parent_row]
  }
  if (!is_null(frame_ok)) {
    parent_row[!frame_ok] <- NA_integer_
  }
  parent_row
}

#' @noRd
distinct_parent_units <- function(frame, parent_vars) {
  units <- vctrs::vec_slice(
    frame[, parent_vars, drop = FALSE],
    unique_key_loc(frame, parent_vars)
  )
  # A class declares missing by value, so filtering the distinct keys is exact.
  slice_complete_keys(units, parent_vars)
}

#' Require every selected parent to have an eligible population
#' @noRd
check_realized_parent_coverage <- function(frame, previous_sample, parent_vars,
                                           design, stage_idx, frame_index,
                                           frame_label, call = caller_env()) {
  # Collapse WR multiplicity before checking population coverage.
  selected <- unique(previous_sample[, parent_vars, drop = FALSE])
  # The frame is the same in every replicate. Its units are not.
  available <- memo_call(
    paste0("parent_units_", stage_idx),
    distinct_parent_units,
    frame,
    parent_vars
  )
  missing <- dplyr::anti_join(selected, available, by = parent_vars)

  if (nrow(missing) == 0) {
    return(invisible(missing))
  }

  # An empty pool contributes zero to every total.
  on_empty <- design$stages[[stage_idx]]$draw_spec$on_empty %||% "error"
  if (identical(on_empty, "warn")) {
    signal_selection_event(
      "empty_parent",
      pool_keys = unique(path_labels(
        missing, parent_vars, ancestor_cluster_levels(design, stage_idx)
      )),
      n_empty = nrow(missing),
      n_pools = nrow(selected),
      stage = stage_idx
    )
  }
  if (!identical(on_empty, "error")) {
    return(invisible(missing))
  }

  preview <- format_key_preview(missing)
  abort_samplyr(
    c(
      "{nrow(missing)} unit{?s} selected at the previous stage
       {?has/have} no rows in {frame_token(frame_index, frame_label)}.",
      "x" = "{stage_token(design, stage_idx)} cannot sample within
             {format_pool_sample(preview)}.",
      "i" = "Keyed on {.field {parent_vars}}.",
      "i" = "Every selected unit needs an eligible population in the next
             frame. Extra rows for unselected units are allowed and are
             filtered out.",
      "i" = "If a selected unit can have no eligible members, such as a
             household with no eligible person, set {.code on_empty =
             \"warn\"} or {.code \"silent\"} in this stage's {.fn draw}.
             The unit then contributes zero."
    ),
    class = "samplyr_error_frame_missing_parent",
    call = call
  )
}

#' Design variables completed stages need to keep
#'
#' Cluster ancestry is already required on the frame. Stratification is not:
#' a school register can hold `school_type` while the class register has no
#' reason to repeat it, and survey export still needs it on every row.
#' @noRd
prior_design_carry_vars <- function(design, stage_idx) {
  if (stage_idx <= 1L) {
    return(character(0))
  }
  vars <- character(0)
  for (i in seq_len(stage_idx - 1L)) {
    spec <- design$stages[[i]]
    if (!is_null(spec$strata)) {
      vars <- c(vars, spec$strata$vars)
    }
  }
  unique(vars)
}

#' Measures of size of completed stages, kept on a continued sample
#'
#' A one-frame execution keeps every frame column, so its sample carries what
#' each stage's inclusion probabilities were computed from. A continuation
#' carries these too. Unlike strata, they are not exempt from the frame
#' preflight, so an earlier stage's measure never stands in for a variable
#' this stage selects on.
#' @noRd
prior_stage_mos_vars <- function(design, stage_idx) {
  if (stage_idx <= 1L) {
    return(character(0))
  }
  vars <- unlist(lapply(
    design$stages[seq_len(stage_idx - 1L)],
    function(spec) spec$draw_spec$mos
  ))
  setdiff(unique(vars), prior_design_carry_vars(design, stage_idx))
}

#' A temporary column name no frame is using
#' @noRd
free_column_name <- function(frame, base) {
  free_name(names(frame), base)
}

#' `base`, or `base` followed by the first integer that avoids `taken`
#' @noRd
free_name <- function(taken, base) {
  name <- base
  i <- 1L
  while (name %in% taken) {
    name <- paste0(base, i)
    i <- i + 1L
  }
  name
}

#' Compare two columns without tripping over type or NA
#' @noRd
values_agree <- function(x, y) {
  tryCatch(
    {
      common <- vctrs::vec_cast_common(x, y)
      eq <- vctrs::vec_equal(common[[1]], common[[2]], na_equal = TRUE)
      # The cast can drop a class's declared missing codes.
      eq[xor(is.na(x), is.na(y))] <- FALSE
      eq
    },
    error = function(e) rep(FALSE, max(length(x), length(y)))
  )
}

#' Carry variables forward onto a linked frame by the resolved parent key
#'
#' The shared primitive behind two different carries: design variables of
#' completed stages, and identifiers of a previous phase. Both must be
#' functionally determined by the parent unit, both are joined by the exact
#' parent key rather than by whatever the two tables share, and in both a copy
#' already present in the lower frame is kept if it agrees and refused if it
#' does not, so no `.x`/`.y` pair can hide a disagreement.
#'
#' @param carry_vars Names to carry. What they mean is the caller's business.
#' @param keep_present `TRUE` leaves a name the lower frame already holds to
#'   that frame, unchecked, for a variable that may mean something else there.
#' @param same_rows `TRUE` when `frame` holds the rows of `previous_sample`.
#'   Every variable is then present and agrees with its parent's once it is
#'   constant within the parent, so only that is checked.
#' @param parent_row For each row of `frame`, the first row of
#'   `previous_sample` with its parent key, when the caller has matched them.
#' @noRd
carry_vars_by_parent <- function(frame, previous_sample, carry_vars, design,
                                 stage_idx, parent_vars, frame_index = 1L,
                                 frame_label = NULL, keep_present = FALSE,
                                 same_rows = FALSE, parent_row = NULL,
                                 call = caller_env()) {
  carry_vars <- setdiff(carry_vars, parent_vars)
  carry_vars <- intersect(carry_vars, names(previous_sample))
  if (keep_present) {
    carry_vars <- setdiff(carry_vars, names(frame))
  }
  if (length(carry_vars) == 0 || nrow(frame) == 0) {
    return(frame)
  }

  # vec_unique() keeps first occurrences in order.
  loc <- unique_key_loc(previous_sample, c(parent_vars, carry_vars))
  lookup <- vctrs::vec_slice(
    previous_sample[, c(parent_vars, carry_vars), drop = FALSE],
    loc
  )

  # Parent variables must be invariant within parent.
  duplicated_parents <- duplicated(lookup[, parent_vars, drop = FALSE])
  if (any(duplicated_parents)) {
    offenders <- unique(lookup[duplicated_parents, parent_vars, drop = FALSE])
    abort_samplyr(
      c(
        "{.field {carry_vars}} does not have one value per selected unit.",
        "x" = "{stage_token(design, stage_idx)} would carry it forward by
               {.field {parent_vars}}, which needs one value per unit.",
        "i" = "Units with more than one value: {format_pool_sample(format_key_preview(offenders))}."
      ),
      class = "samplyr_error_frame_parent_conflict",
      call = call
    )
  }
  if (same_rows) {
    return(frame)
  }

  present <- intersect(carry_vars, names(frame))
  # A parent's first row is the lookup row of its key, which is unique.
  parent_row <- if (is_null(parent_row)) {
    vctrs::vec_match(
      frame[, parent_vars, drop = FALSE],
      lookup[, parent_vars, drop = FALSE]
    )
  } else {
    match(parent_row, loc)
  }
  for (var in present) {
    parent_value <- lookup[[var]][parent_row]
    # Remaining unmatched rows indicate linkage failure.
    disagree <- !values_agree(frame[[var]], parent_value)
    if (any(disagree)) {
      offenders <- unique(frame[disagree, parent_vars, drop = FALSE])
      abort_samplyr(
        c(
          "{.field {var}} in {frame_token(frame_index, frame_label)}
           disagrees with the value carried from the selected unit.",
          "x" = "One variable cannot hold two values for the same unit.",
          "i" = "Units that disagree: {format_pool_sample(format_key_preview(offenders))}.",
          "i" = "Drop the column from this frame to use the value the earlier
                 stage selected on."
        ),
        class = "samplyr_error_frame_parent_conflict",
        call = call
      )
    }
  }

  to_join <- setdiff(carry_vars, present)
  if (length(to_join) == 0) {
    return(frame)
  }

  # Keys take their common type, as a join would give them.
  for (var in parent_vars) {
    frame[[var]] <- vctrs::vec_cast(
      frame[[var]], vctrs::vec_ptype2(frame[[var]], lookup[[var]])
    )
  }
  for (var in to_join) {
    frame[[var]] <- vctrs::vec_slice(lookup[[var]], parent_row)
  }
  rownames(frame) <- NULL
  frame
}

#' Identifiers of the previous phase that later stages must keep
#'
#' Derived once from the phase this execution descends from. These are the
#' units phase 1 declared, which is what a two-phase export needs to match
#' phase-2 rows back into the phase-1 table. They belong to no stage of the
#' current design, so they are carried separately from its own variables.
#' @noRd
phase_link_vars_of <- function(prev_phase) {
  if (is_null(prev_phase) || is_null(prev_phase$design)) {
    return(character(0))
  }
  stages <- prev_phase$stages %||% seq_along(prev_phase$design$stages)
  vars <- character(0)
  for (stage_idx in stages) {
    spec <- prev_phase$design$stages[[stage_idx]]
    if (!is_null(spec$clusters)) {
      vars <- c(vars, spec$clusters$vars)
    }
  }
  unique(vars)
}

#' Require a previous phase's identifier to be constant within each unit
#'
#' Carrying the phase key by parent unit only makes sense if each unit belongs
#' to one phase-1 unit. This cannot be left to the carry itself: a clustered
#' stage that is not the last of its execution keeps one representative row per
#' cluster, so a key varying inside a cluster is already gone by the time a
#' later transition looks at the sample, and would be silently resolved to
#' whichever descendant row came first.
#' @noRd
check_phase_key_invariance <- function(schedule, design, phase_link_vars,
                                       prev_phase = NULL,
                                       call = caller_env()) {
  if (length(phase_link_vars) == 0) {
    return(invisible(NULL))
  }

  # Refuse missing declared phase keys at the transition.
  if (!is_null(prev_phase) && !is_null(prev_phase$sample)) {
    absent <- setdiff(phase_link_vars, names(prev_phase$sample))
    if (length(absent) > 0) {
      abort_samplyr(
        c(
          "The previous phase no longer carries {.field {absent}}.",
          "x" = "Its design declared {.field {phase_link_vars}} as the units
                 it sampled, so the phases cannot be linked without it.",
          "i" = "This usually means the phase-1 sample was modified after
                 execution."
        ),
        class = "samplyr_error_phase_link_missing",
        call = call
      )
    }
  }

  for (entry in schedule$entries) {
    spec <- design$stages[[entry$stage]]
    # A previous phase's sample, as the new phase reads it.
    frame <- with_phase_carried_columns(entry$frame)
    if (is_null(spec$clusters) || !is.data.frame(frame)) {
      next
    }
    # Qualify local cluster IDs by full ancestry.
    unit_vars <- unique(c(
      collect_ancestor_cluster_vars(design, entry$stage), spec$clusters$vars
    ))
    if (!all(unit_vars %in% names(frame))) {
      next
    }
    keys <- setdiff(intersect(phase_link_vars, names(frame)), unit_vars)
    if (length(keys) == 0) {
      next
    }

    n_units <- nrow(unique(frame[, unit_vars, drop = FALSE]))
    for (var in keys) {
      n_combos <- nrow(unique(frame[, c(unit_vars, var), drop = FALSE]))
      if (n_combos == n_units) {
        next
      }
      if (stage_reads_phase1(frame, prev_phase)) {
        # The rows carry their own keys and positions, so a unit holding
        # several phase-1 units links. It must lie within one primary unit.
        check_phase2_unit_within_psu(frame, unit_vars, prev_phase, design,
                                     entry$stage, call = call)
        break
      }
      abort_samplyr(
        c(
          "{.field {var}} is not constant within
           {.field {unit_vars}} at {stage_token(design, entry$stage)}.",
          "x" = "It identifies the previous phase, so a unit that spans more
                 than one of its values cannot be linked back to one
                 phase-1 unit.",
          "i" = "This usually means the frame's hierarchy is broken, or that
                 the previous phase sampled units that cut across this
                 stage's."
        ),
        class = "samplyr_error_phase_key_ambiguous",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' Does a stage read the previous phase's sample itself?
#'
#' Its rows are then the previous phase's rows, which carry their own
#' identifiers and the row positions that link the phases. A register of the
#' stage's own does not. The frame is the sample as supplied, or as
#' prepared for the new phase with its row positions.
#' @noRd
stage_reads_phase1 <- function(frame, prev_phase) {
  sample <- prev_phase$sample
  if (!is_tbl_sample(sample) || !is.data.frame(frame)) {
    return(FALSE)
  }
  if (phase_row_column(sample) %in% names(frame)) {
    return(TRUE)
  }
  # Rows and values, not the execution record, which a replay rebuilds.
  is_tbl_sample(frame) && nrow(frame) == nrow(sample) &&
    identical(lapply(as.data.frame(frame), identity),
              lapply(as.data.frame(sample), identity))
}

#' Refuse a phase-2 unit that spans primary units of the previous phase
#'
#' Phase 2 subsamples the previous phase's primary units, so a phase-2 unit
#' holds rows of one of them. Under a stage that can repeat a unit, each hit
#' is a primary unit of its own.
#' @noRd
check_phase2_unit_within_psu <- function(frame, unit_vars, prev_phase,
                                         design, stage_idx,
                                         call = caller_env()) {
  design1 <- prev_phase$design
  first1 <- (prev_phase$stages %||% seq_along(design1$stages))[1]
  own_hit <- paste0(".draw_", first1)
  carried_hit <- phase_draw_columns(prev_phase$sample)[own_hit]
  units1 <- intersect(design1$stages[[first1]]$clusters$vars, names(frame))
  # The prepared frame carries the hit number, the supplied sample its own.
  hit <- intersect(stats::na.omit(c(carried_hit, own_hit)), names(frame))[1]
  psu_vars <- c(units1, stats::na.omit(hit))
  if (length(psu_vars) == 0L) {
    return(invisible(NULL))
  }
  n_units <- nrow(unique(frame[, unit_vars, drop = FALSE]))
  n_combos <- nrow(unique(frame[, unique(c(unit_vars, psu_vars)),
                                drop = FALSE]))
  if (n_combos == n_units) {
    return(invisible(NULL))
  }
  advice <- c(units1, if (!is.na(hit)) unname(carried_hit))
  abort_samplyr(
    c(
      "A unit of {stage_token(design, stage_idx)} spans more than one
       primary unit of the previous phase.",
      "x" = "{.field {unit_vars}} holds rows of several of the previous
             phase's {.field {psu_vars}} units, while a phase subsamples the
             primary units the previous phase selected.",
      "i" = "Stratify by {.field {advice}} to draw within each of them."
    ),
    class = "samplyr_error_phase_cluster_spans_psu",
    call = call
  )
}

#' Link a stage's frame to the units its parent stage selected
#'
#' Replaces the earlier `subset_frame_to_sample()`, which intersected the
#' required ancestry with whatever columns the two tables shared and could
#' reach a join with an empty key.
#'
#' @return The frame restricted to selected parents, plus the resolved
#'   ancestry.
#' @noRd
link_stage_frame <- function(frame, previous_sample, design, stage_idx,
                             frame_index = 1L, frame_label = NULL,
                             phase_link_vars = character(0),
                             check_coverage = TRUE,
                             call = caller_env()) {
  parent_vars <- collect_ancestor_cluster_vars(design, stage_idx)
  # Rows of the previous phase's sample carry their own phase keys, with the
  # row positions that link the phases, so nothing is carried by parent.
  if (any(grepl("^\\.phase[0-9]+_row$", names(frame)))) {
    phase_link_vars <- character(0)
  }

  if (length(parent_vars) == 0) {
    abort_samplyr(
      c(
        "{stage_token(design, stage_idx)} has no parent identifier to link to.",
        "x" = "No earlier stage declared its sampling units with
               {.fn cluster_by}.",
        "i" = "Multi-stage designs link through the units an earlier stage
               selected."
      ),
      class = "samplyr_error_frame_missing_ancestry",
      call = call
    )
  }

  check_parent_vars_present(
    frame, previous_sample, parent_vars, design, stage_idx,
    frame_index, frame_label, call = call
  )
  check_parent_key_types(
    frame, previous_sample, parent_vars, design, stage_idx,
    frame_index, frame_label, call = call
  )
  empty_parents <- if (check_coverage) {
    check_realized_parent_coverage(
      frame, previous_sample, parent_vars, design, stage_idx,
      frame_index, frame_label, call = call
    )
  }

  # A frame linked to its own rows keeps every row, as a stage of one
  # hierarchy does before anything is drawn. The slice would copy the frame
  # to return the same rows.
  same_rows <- rlang::is_reference(frame, previous_sample) &&
    slice_keeps_frame(frame)
  parent_row <- NULL
  linked <- if (same_rows) {
    sliced_row_names(frame)
  } else {
    parent_row <- match_complete_parents(frame, previous_sample, parent_vars)
    rows <- which(!is.na(parent_row))
    parent_row <- parent_row[rows]
    vctrs::vec_slice(frame, rows)
  }

  # Design variables of completed stages of this design.
  linked <- carry_vars_by_parent(
    linked, previous_sample,
    prior_design_carry_vars(design, stage_idx),
    design, stage_idx, parent_vars, frame_index, frame_label,
    same_rows = same_rows, parent_row = parent_row, call = call
  )

  linked <- carry_vars_by_parent(
    linked, previous_sample, prior_stage_mos_vars(design, stage_idx),
    design, stage_idx, parent_vars, frame_index, frame_label,
    keep_present = TRUE, same_rows = same_rows, parent_row = parent_row,
    call = call
  )

  # Preserve prior-phase identifiers across normalized registers.
  linked <- carry_vars_by_parent(
    linked, previous_sample, phase_link_vars,
    design, stage_idx, parent_vars, frame_index, frame_label,
    same_rows = same_rows, parent_row = parent_row, call = call
  )

  # The selected rows of the empty parents, which an export stands in for.
  empty_rows <- if (!is_null(empty_parents) && nrow(empty_parents) > 0L) {
    at <- vctrs::vec_in(
      previous_sample[parent_vars],
      empty_parents[parent_vars]
    )
    as.data.frame(vctrs::vec_slice(previous_sample, at))
  }

  list(
    frame = linked,
    parent_vars = parent_vars,
    empty_parents = empty_parents,
    empty_rows = empty_rows
  )
}

#' Whether slicing every row gives back the frame
#'
#' True for columns `vctrs::vec_slice()` returns unchanged when it takes
#' every row: atomic vectors, factors, dates and date-times. Anything else
#' goes through the slice.
#' @noRd
slice_keeps_frame <- function(frame) {
  all(vapply(frame, function(col) {
    is.atomic(col) && !is.array(col) &&
      (!is.object(col) || inherits(col, c("factor", "Date", "POSIXct")))
  }, logical(1)))
}

#' A frame with the row names `vctrs::vec_slice()` would give it
#'
#' A slice keeps character row names and replaces integer ones with the
#' automatic form.
#' @noRd
sliced_row_names <- function(frame) {
  rn <- attr(frame, "row.names")
  if (is.integer(rn)) {
    attr(frame, "row.names") <- .set_row_names(length(rn))
  }
  frame
}

#' Selected units a later stage found empty, per stage and replicate
#'
#' Kept in the sample's metadata rather than its digest, which
#' `frame_digest = "none"` omits, because export has to say when a stage
#' ran with empty parents. `rows` holds their rows in the sample of the
#' stage above, one per draw, which an export turns into zero rows.
#' @noRd
add_empty_parents <- function(records, stage, keys, rows = NULL,
                              replicate = NA_integer_) {
  if (is_null(keys) || nrow(keys) == 0L) {
    return(records)
  }
  c(records, list(list(
    stage = as.integer(stage),
    replicate = as.integer(replicate),
    n = nrow(keys),
    keys = as.data.frame(keys),
    rows = rows
  )))
}

#' The pools of a random-size stage in which the draw selected nothing
#'
#' A fixed-size stage takes at least one unit in every pool, since execution
#' refuses a take of zero, but a random-size draw can take none from a pool
#' that has units. That pool then leaves no row, and when it is the only
#' pool of its parent, the parent leaves none either. A pool is a stratum of
#' the stage within one parent (its ancestry and hit), read off the stage's
#' linked frame, and `N` counts its units: rows, or clusters at a clustered
#' stage. Whatever the frame digest, this is kept.
#' @return `NULL` when the stage is of fixed size or no pool is empty, else
#'   a list of `keys` (one row per empty pool), `N` and `parent_vars`.
#' @noRd
stage_empty_pools <- function(frame, result, draw_spec, split_vars,
                              strata_vars, cluster_vars) {
  if (!is_random_size_method(draw_spec) || nrow(frame) == 0L) {
    return(NULL)
  }
  key_vars <- unique(c(split_vars, strata_vars))
  pools <- if (length(key_vars) == 0L) {
    vctrs::new_data_frame(list(), n = 1L)
  } else {
    vctrs::vec_unique(frame[key_vars])
  }
  empty <- if (nrow(result) == 0L) {
    rep(TRUE, nrow(pools))
  } else if (length(key_vars) == 0L) {
    FALSE
  } else {
    !vctrs::vec_in(pools, result[key_vars])
  }
  if (!any(empty)) {
    return(NULL)
  }
  units <- if (length(cluster_vars) > 0L) {
    vctrs::vec_unique(frame[unique(c(key_vars, cluster_vars))])
  } else {
    frame
  }
  pool_of <- if (length(key_vars) == 0L) {
    rep(1L, nrow(units))
  } else {
    vctrs::vec_match(units[key_vars], pools)
  }
  N <- tabulate(pool_of, nbins = nrow(pools))
  list(
    keys = as.data.frame(vctrs::vec_slice(pools, empty)),
    N = N[empty],
    parent_vars = split_vars
  )
}

#' Record the pools a random-size stage left empty
#'
#' Kept with the sample's metadata in the shape of `add_empty_parents()`,
#' with `N` per pool and, below stage 1, the rows of each pool's parent in
#' the sample of the stage above, one per hit.
#' @param found From `stage_empty_pools()`.
#' @param previous_sample The sample of the stage above, `NULL` at stage 1.
#' @noRd
add_empty_pools <- function(records, stage, found, previous_sample) {
  if (is_null(found)) {
    return(records)
  }
  parent <- intersect(found$parent_vars, names(previous_sample))
  rows <- if (!is_null(previous_sample) && length(parent) > 0L) {
    as.data.frame(vctrs::vec_slice(
      previous_sample,
      vctrs::vec_in(previous_sample[parent], found$keys[parent])
    ))
  }
  c(records, list(list(
    stage = as.integer(stage),
    replicate = NA_integer_,
    n = nrow(found$keys),
    keys = found$keys,
    N = found$N,
    rows = rows
  )))
}

#' Add a replicate's new empty pools to the carried record
#'
#' A record that is unknown on either side, `NULL`, stays unknown.
#' @param inherited The records the replicate's run started from.
#' @noRd
carry_empty_pools <- function(records, result_records, inherited,
                              replicate) {
  if (is_null(records) || is_null(result_records)) {
    return(NULL)
  }
  added <- result_records[setdiff(seq_along(result_records),
                                  seq_along(inherited))]
  c(records, tag_empty_parents(added, replicate))
}

#' @noRd
tag_empty_parents <- function(records, replicate) {
  lapply(records, function(r) {
    r$replicate <- as.integer(replicate)
    r
  })
}

#' Find candidate parents no later register can serve
#'
#' Only an all-at-once view can see the candidate population of every stage at
#' the same time, so only it can report a gap before sampling. The traversal is
#' shared by the two callers that disagree about severity: `execute()` warns
#' because a realization may still be complete, `validate_frame()` aborts
#' because the caller asked for the registers to be judged rather than sampled.
#'
#' @param previous_sample The realized sample a continuation extends. Its
#'   selected units, not the whole first register, bound what is reachable:
#'   a continuation can never descend into a unit an earlier call did not
#'   select, so reporting those as gaps is noise about work that will not
#'   happen.
#' @return A list of gap records, empty when the registers cover every
#'   candidate. `NULL` means the traversal declined to judge, leaving the
#'   message to the transition's own error.
#' @noRd
scan_incomplete_registers <- function(schedule, design,
                                      previous_sample = NULL) {
  if (!identical(schedule$frame_mode, "separate_frames")) {
    return(list())
  }

  gaps <- list()
  entries <- schedule$entries
  # Candidate units must be reachable through every earlier register.
  reachable <- entries[[1]]$frame
  if (!is_null(previous_sample)) {
    seed_vars <- collect_ancestor_cluster_vars(design, entries[[1]]$stage)
    if (length(seed_vars) > 0 &&
          all(seed_vars %in% names(reachable)) &&
          all(seed_vars %in% names(previous_sample))) {
      reachable <- tryCatch(
        dplyr::semi_join(
          reachable,
          unique(previous_sample[, seed_vars, drop = FALSE]),
          by = seed_vars
        ),
        error = function(e) reachable
      )
    }
  }

  for (i in seq_along(entries)[-1]) {
    stage_idx <- entries[[i]]$stage
    parent_vars <- collect_ancestor_cluster_vars(design, stage_idx)
    child <- entries[[i]]$frame

    # Let transition errors report missing columns.
    if (length(parent_vars) == 0 ||
          !all(parent_vars %in% names(child)) ||
          !all(parent_vars %in% names(reachable))) {
      return(NULL)
    }

    candidates <- unique(reachable[, parent_vars, drop = FALSE])
    candidates <- slice_complete_keys(candidates, parent_vars)
    # A row whose parent id is missing serves no parent.
    child <- slice_complete_keys(child, parent_vars)
    # A stage that accepts empty parents has no gap to report.
    accepts_empty <- !identical(
      design$stages[[stage_idx]]$draw_spec$on_empty %||% "error", "error"
    )
    if (accepts_empty) {
      reachable <- tryCatch(
        dplyr::semi_join(child, candidates, by = parent_vars),
        error = function(e) child
      )
      next
    }
    available <- unique(child[, parent_vars, drop = FALSE])
    # Diagnostics must not pre-empt transition errors.
    missing <- tryCatch(
      dplyr::anti_join(candidates, available, by = parent_vars),
      error = function(e) NULL
    )
    if (!is_null(missing) && nrow(missing) > 0) {
      gaps[[length(gaps) + 1L]] <- list(
        stage = stage_idx,
        frame_index = entries[[i]]$frame_index,
        frame_label = entries[[i]]$frame_label,
        n = nrow(missing),
        preview = format_key_preview(missing),
        parent_vars = parent_vars
      )
    }

    reachable <- tryCatch(
      dplyr::semi_join(child, candidates, by = parent_vars),
      error = function(e) child
    )
  }

  gaps
}

#' One bullet per uncovered register, safe to hand to cli
#' @noRd
format_register_gaps <- function(design, gaps) {
  detail <- vapply(gaps, function(gap) {
    paste0(
      frame_token(gap$frame_index, gap$frame_label),
      " has no rows for ", gap$n,
      if (gap$n == 1L) " candidate unit of " else " candidate units of ",
      stage_token(design, gap$stage), ": ",
      paste(c(utils::head(gap$preview, 5L), if (gap$n > 5L) paste("and", gap$n - 5L, "more")), collapse = ", "), "."
    )
  }, character(1))
  # Escape key values before cli interpolation.
  detail <- gsub("}", "}}", gsub("{", "{{", detail, fixed = TRUE), fixed = TRUE)
  names(detail) <- rep("*", length(detail))
  detail
}

#' Warn once about candidate parents no later register can serve
#'
#' The report is diagnostic: whether a given realization is accepted is decided
#' by `link_stage_frame()`, identically in both execution forms.
#' @noRd
warn_incomplete_registers <- function(schedule, design, previous_sample = NULL,
                                     call = caller_env()) {
  gaps <- scan_incomplete_registers(schedule, design, previous_sample)
  if (is_null(gaps) || length(gaps) == 0) {
    return(invisible(NULL))
  }

  cli_warn(
    c(
      "The supplied registers do not cover every candidate unit.",
      format_register_gaps(design, gaps),
      "i" = "This realization may still be complete. Execution fails only if a
             unit that is actually selected has no rows in the next register.",
      "i" = "Use {.fn validate_frame} to reject incomplete registers outright."
    ),
    class = "samplyr_warning_frame_incomplete_register",
    call = call
  )
  invisible(NULL)
}

#' Refuse candidate parents no later register can serve
#'
#' The strict counterpart of `warn_incomplete_registers()`. Explicit validation
#' judges the registers themselves, so a gap is a defect even when the seed
#' that would be used never realizes it.
#' @noRd
abort_incomplete_registers <- function(schedule, design,
                                      previous_sample = NULL,
                                      call = caller_env()) {
  gaps <- scan_incomplete_registers(schedule, design, previous_sample)
  if (is_null(gaps) || length(gaps) == 0) {
    return(invisible(NULL))
  }

  abort_samplyr(
    c(
      "The supplied registers do not cover every candidate unit.",
      format_register_gaps(design, gaps),
      "i" = "A unit reachable at one stage must have rows in the register the
             next stage samples from.",
      "i" = "{.fn execute} warns instead and fails only on a unit it actually
             selects."
    ),
    class = "samplyr_error_frame_incomplete_register",
    call = call
  )
}
