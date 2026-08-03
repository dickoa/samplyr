# Stage-to-frame scheduling.
#
# Every execution form resolves to one schedule: an ordered list with one entry
# per stage executed in this call, each carrying the frame that stage samples
# from. Building it is the only place stage indices are resolved and frames are
# matched to stages, so the one-call and continuation spellings cannot drift
# apart. The schedule is built before any random number is consumed.

#' Diagnostic name for a stage
#'
#' Labels are optional presentation metadata, so a stage always has a
#' positional name and gains its label only when one was given.
#' @noRd
stage_token <- function(design, stage_idx) {
  label <- design$stages[[stage_idx]]$label
  if (is_null(label)) {
    paste0("stage ", stage_idx)
  } else {
    paste0("stage ", stage_idx, " \"", label, "\"")
  }
}

#' Diagnostic name for a supplied frame
#' @noRd
frame_token <- function(frame_index, frame_label = NULL) {
  if (is_null(frame_label) || !nzchar(frame_label)) {
    paste0("frame ", frame_index)
  } else {
    paste0("frame ", frame_index, " \"", frame_label, "\"")
  }
}

#' The base contract every `stages` argument shares
#'
#' `stages` means the same thing in `execute()`, `validate_frame()`,
#' `joint_expectation()` and `frame_summary()`, so it is validated the same
#' way in all four: a non-empty vector of distinct whole stage numbers drawn
#' from what the caller allows. Each verb then layers its own rules on the
#' canonical result. Execution adds a start stage and contiguity;
#' `joint_expectation()` and `frame_summary()` restrict `allowed` to what they
#' can answer for.
#'
#' Duplicates are refused rather than quietly collapsed. `stages = c(1, 1)`
#' asks for one stage twice, which no verb can honour, so silently returning
#' one stage hides a mistake in whatever computed the vector.
#'
#' @param allowed The stage numbers this caller accepts.
#' @param what A plural noun phrase naming `allowed`, such as
#'   `"executed stages"`. It appears verbatim, so no message here depends on
#'   cli pluralization agreeing with a second interpolated vector.
#' @return The distinct stage numbers, sorted, as integers.
#' @noRd
normalize_stage_selector <- function(stages, allowed,
                                     what = "available stages",
                                     arg = "stages",
                                     call = caller_env()) {
  refuse <- function(...) {
    abort_samplyr(
      c(...),
      class = "samplyr_error_stage_selector",
      call = call
    )
  }

  if (!is.numeric(stages)) {
    refuse(
      "{.arg {arg}} must be a vector of stage numbers.",
      "x" = "Got {.cls {class(stages)[[1]]}}."
    )
  }
  if (length(stages) == 0L) {
    refuse(
      "{.arg {arg}} must name at least one stage.",
      "i" = "Omit {.arg {arg}} for all {what}."
    )
  }
  if (anyNA(stages)) {
    refuse("{.arg {arg}} must not contain missing values.")
  }
  if (!is_integerish_numeric(stages)) {
    # Infinite as well as fractional: trunc(Inf) is Inf, so a fractional
    # test alone finds nothing to report.
    bad <- stages[!is.finite(stages) | stages != trunc(stages)]
    refuse(
      "{.arg {arg}} must be whole, finite stage numbers.",
      "x" = "Got {.val {if (length(bad)) bad else stages}}."
    )
  }
  stages <- as.integer(stages)

  duplicated_stages <- unique(stages[duplicated(stages)])
  if (length(duplicated_stages) > 0) {
    refuse(
      "{.arg {arg}} must name each stage once.",
      "x" = "Repeated: {.val {duplicated_stages}}."
    )
  }

  outside <- setdiff(stages, allowed)
  if (length(outside) > 0) {
    refuse(
      "{.arg {arg}} must name only {what}.",
      "x" = "Not {what}: {.val {outside}}.",
      "i" = "Available: {.val {allowed}}."
    )
  }

  sort(stages)
}

#' Resolve the stages a call executes
#'
#' Moved out of the executors so that a design start and a continuation share
#' one definition of contiguity and one set of messages.
#' @noRd
resolve_execute_stages <- function(design, stages, executed = NULL,
                                   call = caller_env()) {
  n_stages <- length(design$stages)
  continuation <- !is_null(executed)

  if (is_null(stages)) {
    if (!continuation) {
      return(seq_len(n_stages))
    }
    remaining <- setdiff(seq_len(n_stages), executed)
    if (length(remaining) == 0) {
      cli_abort("All stages have been executed", call = call)
    }
    return(remaining)
  }

  stages <- normalize_stage_selector(
    stages, seq_len(n_stages), what = "stages of this design", call = call
  )

  if (continuation) {
    already_done <- intersect(stages, executed)
    if (length(already_done) > 0) {
      cli_abort("Stage{?s} {already_done} already executed", call = call)
    }
    next_expected <- max(executed) + 1L
    if (stages[1] != next_expected) {
      executed_str <- paste(executed, collapse = ", ")
      cli_abort(c(
        "{.arg stages} must continue from stage {next_expected}.",
        "i" = "Stage(s) {executed_str} already executed; next stage must be {next_expected}."
      ), call = call)
    }
  } else if (stages[1] != 1L) {
    cli_abort(c(
      "{.arg stages} must start at stage 1 when executing from a design.",
      "i" = "To continue from a previous sample, pass the {.cls tbl_sample} instead of the design."
    ), call = call)
  }

  expected <- seq.int(stages[1], stages[length(stages)])
  if (!identical(stages, expected)) {
    cli_abort("{.arg stages} must be contiguous (no gaps)", call = call)
  }
  stages
}

#' Refuse a continuation that cannot say which stage a single frame belongs to
#'
#' With two or more stages left, one frame is either a stage-specific register
#' for the next stage or a shared hierarchy for all of them. Guessing produced
#' samples drawn from the wrong register, so the caller states which.
#' @noRd
check_continuation_ambiguity <- function(design, remaining, n_frames,
                                         call = caller_env()) {
  if (length(remaining) < 2L || n_frames != 1L) {
    return(invisible(NULL))
  }
  next_stage <- remaining[1]
  span <- paste0(remaining[1], ":", remaining[length(remaining)])
  abort_samplyr(
    c(
      "{.arg stages} is required: {length(remaining)} stages remain and one
       frame was supplied.",
      "x" = "One frame cannot say whether it is a register for
             {stage_token(design, next_stage)} or a hierarchy for all of them.",
      "i" = "Use {.code stages = {next_stage}} for a register covering only
             {stage_token(design, next_stage)}.",
      "i" = "Use {.code stages = {span}} for one frame that already contains
             every remaining stage."
    ),
    class = "samplyr_error_ambiguous_continuation",
    call = call
  )
}

#' Require a parent identity on any stage another stage samples within
#'
#' Strata are selection pools, not parent unit identities. Without
#' `cluster_by()` the next stage has no key naming the units this stage
#' selected, and the frame it samples from silently widens to the whole
#' stratum.
#' @noRd
check_stage_parent_identity <- function(design, stages, executed = NULL,
                                        call = caller_env()) {
  parents <- stages[-length(stages)]

  # A continuation samples within the stage the previous call ended on, so that
  # stage carries the same requirement.
  if (!is_null(executed) && length(executed) > 0) {
    parents <- c(max(executed), parents)
  }

  for (stage_idx in parents) {
    if (!is_null(design$stages[[stage_idx]]$clusters)) {
      next
    }
    child <- min(stages[stages > stage_idx])
    abort_samplyr(
      c(
        "{stage_token(design, stage_idx)} must declare its sampling units with
         {.fn cluster_by} before {stage_token(design, child)} can sample
         within them.",
        "x" = "It selects elements, so there is no unit identifier to link
               {stage_token(design, child)} to the units it selected.",
        "i" = "Add {.code cluster_by(<unit id>)} to
               {stage_token(design, stage_idx)}.",
        "i" = "To subsample the selected elements instead, execute
               {stage_token(design, stage_idx)} on its own and start a new
               design on the result, which records a second phase."
      ),
      class = "samplyr_error_stage_parent_id",
      call = call
    )
  }
  invisible(NULL)
}

#' Keep phase linkage in the first frame position
#'
#' A `tbl_sample` supplied to a new design is the previous phase, and a phase
#' is a property of the whole execution rather than of one stage. Later
#' positions are stage registers, so a sample there is a category error: today
#' it is read as a second previous phase and fails downstream with an opaque
#' empty-frame message.
#' @noRd
check_phase_frame_position <- function(frames, labels, call = caller_env()) {
  if (length(frames) < 2L) {
    return(invisible(NULL))
  }
  for (i in seq_along(frames)[-1]) {
    if (!is_tbl_sample(frames[[i]])) {
      next
    }
    abort_samplyr(
      c(
        "{frame_token(i, labels[i])} is a {.cls tbl_sample}, but only the
         first frame may be one.",
        "x" = "A {.cls tbl_sample} frame is the previous phase of the whole
               execution, not the register for one stage.",
        "i" = "For a later stage, pass an ordinary data frame listing that
               stage's units.",
        "i" = "For a new phase, pass the previous sample as the first frame."
      ),
      class = "samplyr_error_phase_frame_position",
      call = call
    )
  }
  invisible(NULL)
}

#' Match supplied frames to the stages of one call
#'
#' @param design The design being executed.
#' @param frames The captured `...`, in order.
#' @param stages The stage indices this call executes, already resolved.
#' @param executed Stages already executed, or `NULL` for a design start.
#' @return One entry per executed stage.
#' @noRd
build_frame_schedule <- function(design, frames, stages, executed = NULL,
                                 call = caller_env()) {
  n_frames <- length(frames)
  n_stages <- length(stages)
  labels <- names(frames) %||% rep("", n_frames)

  if (n_frames != 1L && n_frames != n_stages) {
    stage_names <- paste(vapply(
      stages, function(i) stage_token(design, i), character(1)
    ), collapse = ", ")
    abort_samplyr(
      c(
        "This call executes {n_stages} stage{?s} but received
         {n_frames} frame{?s}.",
        "i" = "Supply one frame covering every stage, or one frame per stage.",
        "i" = "Stages being executed: {stage_names}."
      ),
      class = "samplyr_error_frame_count",
      call = call
    )
  }

  # Scheduling and provenance only. No name implies anything about the
  # granularity of the rows: a frame may hold one row per unit of its stage or
  # any finer level, and cluster_by() is what identifies the sampling unit.
  # A single frame for a single stage is reused by nothing, so it is neither
  # shared nor separate.
  mode <- if (n_frames > 1L) {
    "separate_frames"
  } else if (n_stages > 1L) {
    "shared_frame"
  } else {
    "single_frame"
  }

  entries <- vector("list", n_stages)
  for (i in seq_len(n_stages)) {
    frame_index <- if (n_frames == 1L) 1L else i
    label <- labels[frame_index]
    entries[[i]] <- list(
      stage = stages[i],
      stage_label = design$stages[[stages[i]]]$label,
      frame_index = frame_index,
      frame_label = if (nzchar(label)) label else NULL,
      frame = frames[[frame_index]],
      frame_mode = mode
    )
  }

  structure(
    list(stages = stages, entries = entries, frame_mode = mode,
         n_supplied = n_frames),
    class = "samplyr_frame_schedule"
  )
}

#' Every frame column a stage selects on
#'
#' One definition for all eight families a stage can name, so a preflight and
#' the stage's own validation cannot disagree about what a frame must carry.
#' Anything omitted here is a column whose absence is only discovered mid
#' execution, after earlier stages have already drawn.
#' @noRd
stage_required_vars <- function(stage_spec) {
  draw_spec <- stage_spec$draw_spec
  role <- function(vars, label) {
    if (length(vars) == 0) {
      return(character(0))
    }
    stats::setNames(vars, rep(label, length(vars)))
  }
  # Names carry the role each column plays, so a diagnostic can say that the
  # missing column is the PRN rather than only that it is missing. Callers
  # comparing values are unaffected: setdiff() and %in% ignore names.
  vars <- c(
    role(stage_spec$strata$vars, "stratification"),
    role(stage_spec$clusters$vars, "cluster"),
    role(draw_spec$mos, "MOS"),
    role(draw_spec$prn, "PRN"),
    role(draw_spec$aux, "auxiliary"),
    role(draw_spec$bounds, "bound"),
    role(draw_spec$spread, "spread"),
    role(extract_control_vars(draw_spec$control), "control")
  )
  vars[!duplicated(vars)]
}

#' Describe columns by the role their stage gives them
#' @noRd
role_bullets <- function(vars) {
  roles <- names(vars) %||% rep("", length(vars))
  vapply(seq_along(vars), function(i) {
    if (nzchar(roles[[i]])) {
      cli::format_inline("{.field {vars[[i]]}} is the {roles[[i]]} variable.")
    } else {
      cli::format_inline("{.field {vars[[i]]}} is required.")
    }
  }, character(1))
}

#' Check every frame for the columns its stage needs, before sampling
#'
#' Presence of a column does not depend on any draw, so finding out at the
#' transition means stage 1 has already consumed the RNG stream. Values are
#' still validated at the stage itself, where the linked frame is known.
#'
#' Prior-stage strata are exempt: they may legitimately be absent from a later
#' frame and arrive by carry-forward. A `tbl_sample` frame is not exempt; it is
#' checked against the schema that survives once its generated columns are
#' stripped, which is what the stage will actually see.
#' @noRd
check_scheduled_frame_vars <- function(design, entries, call = caller_env()) {
  for (entry in entries) {
    frame <- entry$frame
    if (!is.data.frame(frame)) {
      next
    }
    stage_idx <- entry$stage
    stage_spec <- design$stages[[stage_idx]]
    # A previous-phase frame loses its generated columns before any stage
    # sees it, so the preflight judges the schema that will remain.
    available <- setdiff(names(frame), samplyr_internal_cols(frame))

    ancestry <- collect_ancestor_cluster_vars(design, stage_idx)
    missing_ancestry <- setdiff(ancestry, available)
    if (length(missing_ancestry) > 0) {
      abort_samplyr(
        c(
          "{frame_token(entry$frame_index, entry$frame_label)}, used for
           {stage_token(design, stage_idx)}, is missing
           {.field {missing_ancestry}}.",
          "x" = "Linking a stage to the units its parent selected needs the
                 complete ancestry: {.field {ancestry}}.",
          "i" = "Add the missing ancestry to the frame, or supply a frame that
                 already carries the parent identifiers."
        ),
        class = "samplyr_error_frame_missing_ancestry",
        call = call
      )
    }

    required <- stage_required_vars(stage_spec)
    # Strata of completed stages can arrive by carry-forward, so their absence
    # here is not yet a failure.
    carried <- prior_design_carry_vars(design, stage_idx)
    required <- required[!required %in% carried]
    missing <- required[!required %in% available]
    if (length(missing) > 0) {
      abort_samplyr(
        c(
          "{frame_token(entry$frame_index, entry$frame_label)}, used for
           {stage_token(design, stage_idx)}, is missing
           {.field {missing}}.",
          stats::setNames(role_bullets(missing), rep("x", length(missing))),
          "i" = "{stage_token(design, stage_idx)} selects on
                 {.field {unname(required)}}."
        ),
        class = "samplyr_error_frame_missing_vars",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' Build and validate the schedule for one execution
#'
#' The single entry point. Everything here is static: it runs before any random
#' number is consumed, so a misuse cannot leave a partially drawn sample or
#' advance the RNG stream.
#' @noRd
stage_frame_schedule <- function(design, frames, stages, executed = NULL,
                                 call = caller_env()) {
  stages_supplied <- !is_null(stages)
  stages <- resolve_execute_stages(design, stages, executed, call = call)

  if (is_null(executed)) {
    check_phase_frame_position(
      frames, names(frames) %||% rep("", length(frames)), call = call
    )
  } else if (!stages_supplied) {
    check_continuation_ambiguity(design, stages, length(frames), call = call)
  }
  check_stage_parent_identity(design, stages, executed, call = call)

  schedule <- build_frame_schedule(design, frames, stages, executed,
                                   call = call)
  check_scheduled_frame_vars(design, schedule$entries, call = call)
  schedule
}

#' What a schedule recorded about the frames it was given
#'
#' Receipts and frame-backed consumers need to tell three spellings apart: one
#' shared frame, one register for a partial execution, and one register per
#' stage. The record carries the mapping, never the data, so it can be
#' serialized and compared against what a later call supplies.
#'
#' It describes the call that produced it. A chained sample records the final
#' call only, which is why `chained` and not this record decides whether a
#' receipt can be replayed.
#' @noRd
schedule_record <- function(schedule) {
  labels <- rep(NA_character_, schedule$n_supplied)
  for (entry in schedule$entries) {
    if (!is_null(entry$frame_label)) {
      labels[entry$frame_index] <- entry$frame_label
    }
  }
  list(
    mode = schedule$frame_mode,
    n_supplied = as.integer(schedule$n_supplied),
    labels = if (all(is.na(labels))) NULL else labels,
    stages = as.integer(schedule$stages),
    stage_frame_index = vapply(
      schedule$entries, function(e) as.integer(e$frame_index), integer(1)
    )
  )
}

#' The frame record a sample carries, or the historical one-frame default
#' @noRd
get_frame_schedule <- function(x) {
  if (!is_tbl_sample(x)) {
    return(NULL)
  }
  frame_record_or_default(
    attr(x, "metadata")$frame_schedule, get_stages_executed(x)
  )
}

#' The frame record of an executed sample, or the historical one-frame default
#'
#' Samples executed before the record existed, and the deserialized receipts of
#' those samples, carry no mapping. They came from one frame by construction,
#' which is what the default states.
#' @noRd
frame_record_or_default <- function(record, stages) {
  if (!is_null(record)) {
    return(record)
  }
  stages <- as.integer(stages)
  list(
    mode = if (length(stages) > 1L) "shared_frame" else "single_frame",
    n_supplied = 1L,
    labels = NULL,
    stages = stages,
    stage_frame_index = rep(1L, length(stages))
  )
}

#' Frames of a schedule, one per executed stage
#' @noRd
schedule_frames <- function(schedule) {
  lapply(schedule$entries, function(entry) entry$frame)
}

#' Replace the frames of a validated schedule, keeping its stage mapping
#'
#' Replicated multi-phase execution rebuilds its frames per replicate. The stage
#' mapping was validated once and does not change with the replicate.
#' @noRd
schedule_swap_frames <- function(schedule, frames) {
  for (i in seq_along(schedule$entries)) {
    schedule$entries[[i]]$frame <- frames[[schedule$entries[[i]]$frame_index]]
  }
  schedule
}
