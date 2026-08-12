# Stage transitions.
#
# Linking a stage to the units its parent stage selected is the one operation
# every execution form performs at every stage boundary. It runs here, once,
# with the complete ancestry the design declares. Nothing infers a key from
# whichever columns two tables happen to share.

#' Render a bounded preview of compound keys for a message
#'
#' Users see their own column values, never an internal key encoding.
#' @noRd
format_key_preview <- function(keys, max_shown = 5L) {
  shown <- utils::head(keys, max_shown)
  rendered <- apply(shown, 1, function(row) paste(row, collapse = "/"))
  rendered <- unname(rendered)
  extra <- nrow(keys) - length(rendered)
  if (extra > 0) {
    c(rendered, paste0("... and ", extra, " more"))
  } else {
    rendered
  }
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

#' Require every selected parent to have an eligible population
#' @noRd
check_realized_parent_coverage <- function(frame, previous_sample, parent_vars,
                                           design, stage_idx, frame_index,
                                           frame_label, call = caller_env()) {
  # Draw occurrences repeat a with-replacement parent. Coverage is a property
  # of the population key, so multiplicity is collapsed first.
  selected <- unique(previous_sample[, parent_vars, drop = FALSE])
  available <- unique(frame[, parent_vars, drop = FALSE])
  missing <- dplyr::anti_join(selected, available, by = parent_vars)

  if (nrow(missing) == 0) {
    return(invisible(NULL))
  }

  preview <- format_key_preview(missing)
  abort_samplyr(
    c(
      "{nrow(missing)} unit{?s} selected at the previous stage
       {?has/have} no rows in {frame_token(frame_index, frame_label)}.",
      "x" = "{stage_token(design, stage_idx)} cannot sample within
             {.val {preview}}.",
      "i" = "Keyed on {.field {parent_vars}}.",
      "i" = "Every selected unit needs an eligible population in the next
             frame. Extra rows for unselected units are allowed and are
             filtered out."
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

#' A temporary column name no frame is using
#' @noRd
free_column_name <- function(frame, base) {
  name <- base
  i <- 1L
  while (name %in% names(frame)) {
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
      vctrs::vec_equal(common[[1]], common[[2]], na_equal = TRUE)
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
#' @noRd
carry_vars_by_parent <- function(frame, previous_sample, carry_vars, design,
                                 stage_idx, parent_vars, frame_index = 1L,
                                 frame_label = NULL, call = caller_env()) {
  carry_vars <- setdiff(carry_vars, parent_vars)
  carry_vars <- intersect(carry_vars, names(previous_sample))
  if (length(carry_vars) == 0 || nrow(frame) == 0) {
    return(frame)
  }

  lookup <- unique(previous_sample[, c(parent_vars, carry_vars), drop = FALSE])

  # One value per parent, or the variable does not describe the parent unit
  # and cannot be carried by it.
  duplicated_parents <- duplicated(lookup[, parent_vars, drop = FALSE])
  if (any(duplicated_parents)) {
    offenders <- unique(lookup[duplicated_parents, parent_vars, drop = FALSE])
    abort_samplyr(
      c(
        "{.field {carry_vars}} does not have one value per selected unit.",
        "x" = "{stage_token(design, stage_idx)} would carry it forward by
               {.field {parent_vars}}, which needs one value per unit.",
        "i" = "Units with more than one value: {.val {format_key_preview(offenders)}}."
      ),
      class = "samplyr_error_frame_parent_conflict",
      call = call
    )
  }

  present <- intersect(carry_vars, names(frame))
  # Match whole key rows. Pasting a compound key into one string would make
  # ("a/b", "c") and ("a", "b/c") the same key, which both invents conflicts
  # and hides real ones.
  parent_row <- vctrs::vec_match(
    frame[, parent_vars, drop = FALSE],
    lookup[, parent_vars, drop = FALSE]
  )
  for (var in present) {
    parent_value <- lookup[[var]][parent_row]
    # Rows outside the selected parents were already filtered out, so an
    # unmatched row would be a linkage failure rather than a disagreement.
    disagree <- !values_agree(frame[[var]], parent_value)
    if (any(disagree)) {
      offenders <- unique(frame[disagree, parent_vars, drop = FALSE])
      abort_samplyr(
        c(
          "{.field {var}} in {frame_token(frame_index, frame_label)}
           disagrees with the value carried from the selected unit.",
          "x" = "One variable cannot hold two values for the same unit.",
          "i" = "Units that disagree: {.val {format_key_preview(offenders)}}.",
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

  # Frame order decides which rows a seeded selection draws, and a join is
  # free to reorder. Restore the input order explicitly rather than trusting
  # the join to preserve it.
  pos <- free_column_name(frame, ".samplyr_row_pos")
  n_before <- nrow(frame)
  frame[[pos]] <- seq_len(n_before)
  frame <- dplyr::left_join(
    frame, lookup[, c(parent_vars, to_join), drop = FALSE], by = parent_vars
  )
  if (nrow(frame) != n_before) {
    abort_samplyr(
      "Carrying {.field {to_join}} changed the frame from {n_before} to
       {nrow(frame)} rows.",
      class = "samplyr_error_frame_parent_conflict",
      call = call
    )
  }
  frame <- frame[order(frame[[pos]]), , drop = FALSE]
  frame[[pos]] <- NULL
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

  # The keys are derived from what phase 1 declared. If the sample no longer
  # carries one, nothing can carry it forward and the omission would surface
  # much later as a bare "object not found" from survey export.
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
    frame <- entry$frame
    if (is_null(spec$clusters) || !is.data.frame(frame)) {
      next
    }
    # A unit is its full ancestry plus this stage's cluster, because lower
    # identifiers are routinely local: person 1 exists in every site.
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
                             call = caller_env()) {
  parent_vars <- collect_ancestor_cluster_vars(design, stage_idx)

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
  check_realized_parent_coverage(
    frame, previous_sample, parent_vars, design, stage_idx,
    frame_index, frame_label, call = call
  )

  linked <- dplyr::semi_join(
    frame,
    unique(previous_sample[, parent_vars, drop = FALSE]),
    by = parent_vars
  )

  # Design variables of completed stages of this design.
  linked <- carry_vars_by_parent(
    linked, previous_sample,
    prior_design_carry_vars(design, stage_idx),
    design, stage_idx, parent_vars, frame_index, frame_label, call = call
  )

  # Identifiers of the previous phase. Phase linkage is orthogonal to the
  # stages of this design: without it a normalized later register drops the
  # phase-1 identifier and the two-phase export loses its bridge.
  linked <- carry_vars_by_parent(
    linked, previous_sample, phase_link_vars,
    design, stage_idx, parent_vars, frame_index, frame_label, call = call
  )

  list(frame = linked, parent_vars = parent_vars)
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
  # Candidacy is chained: a unit is a candidate at stage i only if it is itself
  # reachable from every earlier register. Rows that no earlier register can
  # reach, including any with a missing parent key, are not candidates and are
  # not reported here.
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

    # A missing column is a hard error at the transition. Skip it here so the
    # warning never pre-empts the better message.
    if (length(parent_vars) == 0 ||
          !all(parent_vars %in% names(child)) ||
          !all(parent_vars %in% names(reachable))) {
      return(NULL)
    }

    candidates <- unique(reachable[, parent_vars, drop = FALSE])
    candidates <- candidates[stats::complete.cases(candidates), , drop = FALSE]
    available <- unique(child[, parent_vars, drop = FALSE])
    # A diagnostic must never be the thing that fails, and must never pre-empt
    # the transition's own message. Incompatible key types, for one, are
    # reported there with both type names.
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
      paste(gap$preview, collapse = ", "), "."
    )
  }, character(1))
  # Frame values are data, not templates: a key containing a brace must not be
  # read as cli interpolation.
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
