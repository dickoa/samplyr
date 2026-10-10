## Chained receipts

#' The execute() calls behind a chained sample
#'
#' A continuation, a new phase and a materialized wave each start from the
#' result of an earlier call. The final call is described by the receipt
#' itself. This returns the earlier calls, oldest first, each as replay needs
#' it, and how the final call used the one before it.
#'
#' The walk stops early at an earlier phase that was modified before the next
#' phase was drawn from it. Replaying that phase cannot rebuild the rows the
#' next one saw, so the modified sample becomes the first call's frame. The
#' chain cannot be replayed at all, and the result is `NULL`, when a
#' continuation or a wave started from a modified sample, or when a
#' continuation predates the record of its parent's seed.
#'
#' @return `NULL`, or a list with `transition` (the final call's) and `calls`.
#' @noRd
execution_chain <- function(sample) {
  view <- chain_view_of_sample(sample)
  calls <- list()
  repeat {
    link <- chain_predecessor(view)
    view$transition <- link$transition
    calls <- c(list(view), calls)
    if (identical(link$transition, "start")) {
      break
    }
    if (is_null(link$view)) {
      return(NULL)
    }
    if (isTRUE(link$view$modified)) {
      if (identical(link$transition, "phase")) {
        break
      }
      return(NULL)
    }
    view <- link$view
  }
  n <- length(calls)
  list(
    transition = calls[[n]]$transition,
    calls = lapply(calls[-n], chain_call_of_view)
  )
}

#' @noRd
chain_view_of_sample <- function(sample, modified = FALSE) {
  list(
    seed = attr(sample, "seed"),
    stages_executed = get_stages_executed(sample),
    meta = attr(sample, "metadata") %||% list(),
    design = get_design(sample),
    modified = modified
  )
}

#' How a call used the one before it, and that earlier call
#'
#' A continuation is checked first, because it also carries its parent's
#' earlier-phase link forward.
#' @noRd
chain_predecessor <- function(view) {
  parent <- view$meta$continued_from
  if (!is_null(parent)) {
    if (!"stages_executed" %in% names(parent)) {
      return(list(transition = "continuation", view = NULL))
    }
    return(list(transition = "continuation", view = list(
      seed = parent$seed,
      stages_executed = parent$stages_executed,
      meta = parent,
      design = view$design,
      modified = isTRUE(parent$realization_modified)
    )))
  }
  prev <- view$meta$prev_phase
  if (is_null(prev)) {
    return(list(transition = "start"))
  }
  transition <- if (identical(prev$transition, "panel_activation")) {
    "wave"
  } else {
    "phase"
  }
  if (!is_tbl_sample(prev$sample)) {
    return(list(transition = transition, view = NULL))
  }
  list(transition = transition, view = chain_view_of_sample(
    prev$sample,
    modified = !sample_realization_status(prev$sample)$ok
  ))
}

#' One call in the form replay reads, from memory or from a file
#' @noRd
chain_call_of_view <- function(view) {
  meta <- view$meta
  list(
    transition = view$transition,
    seed = view$seed,
    stages = frame_record_or_default(
      meta$frame_schedule, view$stages_executed
    )$stages,
    n_selected = meta$n_selected,
    reps = meta$reps,
    panels = meta$panels,
    panel_assignment = if (!is_null(meta$panel_assignment)) {
      encode_panel_assignment(meta$panel_assignment)
    },
    frames = frame_record_or_default(
      meta$frame_schedule, view$stages_executed
    ),
    wave = if (!is_null(meta$wave)) encode_wave(meta$wave),
    rng = meta$execution_environment$rng,
    design = view$design
  )
}

#' Write the earlier calls of a chain into a receipt
#'
#' A call's design, with the method records that go with it, is written only
#' when the next call starts a new phase. Otherwise it is the next call's
#' design, which the reader recovers by walking back from the final call.
#' @noRd
encode_execution_chain <- function(chain) {
  calls <- chain$calls
  successors <- c(
    vapply(calls[-1], function(cl) cl$transition, character(1)),
    chain$transition
  )
  lapply(seq_along(calls), function(k) {
    cl <- calls[[k]]
    out <- list(
      transition = cl$transition,
      stages = I(as.integer(cl$stages))
    )
    if (!is_null(cl$seed)) out$seed <- as.integer(cl$seed)
    if (!is_null(cl$n_selected)) out$n_selected <- as.integer(cl$n_selected)
    if (!is_null(cl$reps)) out$reps <- as.integer(cl$reps)
    if (!is_null(cl$panels)) out$panels <- as.integer(cl$panels)
    out$panel_assignment <- cl$panel_assignment
    out$frames <- encode_frame_schedule(cl$frames)
    out$wave <- cl$wave
    if (!is_null(cl$rng)) {
      out$rng <- lapply(
        cl$rng[c("kind", "normal_kind", "sample_kind")],
        as.character
      )
    }
    if (identical(successors[[k]], "phase")) {
      out$design <- encode_design(cl$design)
      out$methods <- lapply(cl$design$stages, encode_samplyr_stage_metadata)
    }
    out
  })
}

#' Read the earlier calls of a chain back from a receipt
#'
#' Designs left out by the writer are the next call's, so they are filled in
#' walking back from the final call's design.
#' @noRd
decode_execution_chain <- function(entries, final_design, call = caller_env()) {
  calls <- lapply(entries, function(entry) {
    list(
      transition = decode_chr(entry$transition),
      seed = if (length(entry$seed) > 0) as.integer(entry$seed),
      stages = as.integer(unlist(entry$stages)),
      n_selected = entry$n_selected,
      reps = if (length(entry$reps) > 0) as.integer(entry$reps),
      panels = if (length(entry$panels) > 0) entry$panels,
      panel_assignment = entry$panel_assignment,
      frames = decode_frame_schedule(entry$frames),
      wave = entry$wave,
      rng = entry$rng,
      design = if (!is_null(entry$design)) {
        decode_design_body(entry$design, entry$methods, call = call)
      }
    )
  })
  fill_chain_designs(calls, final_design)
}

#' @noRd
fill_chain_designs <- function(calls, final_design) {
  design <- final_design
  for (k in rev(seq_along(calls))) {
    calls[[k]]$design <- calls[[k]]$design %||% design
    design <- calls[[k]]$design
  }
  calls
}

#' How many frames each call of a chain took from outside
#'
#' The first call reads its frames from the caller, and so does every
#' continuation. A new phase reads the phase before it, and a wave its master.
#' @noRd
chain_frame_counts <- function(calls) {
  takes <- vapply(seq_along(calls), function(k) {
    k == 1L || identical(calls[[k]]$transition, "continuation")
  }, logical(1))
  vapply(calls[takes], function(cl) {
    as.integer(cl$frames$n_supplied %||% 1L)
  }, integer(1))
}

#' The frames of a chain, one element per call that took frames
#'
#' An element is that call's frame, or its list of registers. A single data
#' frame stands for every call when each took one frame. The result is flat,
#' in call order, which is the order the fingerprints are written in.
#' @noRd
flatten_chain_frames <- function(frame, counts, labels = NULL,
                                 class = "samplyr_error_replay_frame_count",
                                 call = caller_env()) {
  n_calls <- length(counts)
  if (is.data.frame(frame) && all(counts == 1L)) {
    return(list(
      frames = rep(list(frame), n_calls),
      labels = rep(labels[1] %||% NA_character_, n_calls)
    ))
  }
  fits <- is.list(frame) && !is.data.frame(frame) &&
    length(frame) == n_calls
  if (fits) {
    for (j in seq_len(n_calls)) {
      element <- frame[[j]]
      fits <- fits && if (counts[[j]] == 1L) {
        is.data.frame(element) ||
          (is.list(element) && length(element) == 1L &&
             is.data.frame(element[[1]]))
      } else {
        is.list(element) && !is.data.frame(element) &&
          length(element) == counts[[j]] &&
          all(vapply(element, is.data.frame, logical(1)))
      }
    }
  }
  if (!fits) {
    described <- vapply(seq_len(n_calls), function(j) {
      paste0("call ", j, ": ", counts[[j]], " frame",
             if (counts[[j]] > 1L) "s" else "")
    }, character(1))
    abort_samplyr(
      c(
        "This sample was built by more than one {.fn execute} call, and
         {.arg frame} must hold the frames of each call that read one.",
        "i" = "The receipt records {n_calls} such call{?s}:
               {paste(described, collapse = ', ')}.",
        "i" = "Supply a list with one element per call, in call order. An
               element is that call's frame, or a list of its registers. One
               data frame is enough when every call read the same single
               frame."
      ),
      class = class,
      call = call
    )
  }
  frames <- list()
  flat_labels <- character(0)
  for (j in seq_len(n_calls)) {
    element <- frame[[j]]
    parts <- if (is.data.frame(element)) list(element) else element
    part_labels <- names(parts) %||% rep("", length(parts))
    part_labels[!nzchar(part_labels)] <- labels[j] %||% NA_character_
    frames <- c(frames, unname(parts))
    flat_labels <- c(flat_labels, part_labels)
  }
  list(frames = frames, labels = flat_labels)
}

#' Replay every call of a chained receipt in order
#'
#' Each call is re-run with its own seed, stages and RNG kind, on what it
#' originally read: the caller's frames for the first call and for each
#' continuation, the replayed earlier phase for a new phase, the replayed
#' master for a wave. Arguments a call inherited rather than declared are not
#' passed again, because execute() refuses them: panels on a sample that
#' already carries an assignment, and replicates on a replicated input.
#' @noRd
replay_execution_chain <- function(calls, frames, frame_digest = "summary",
                                   call = caller_env()) {
  counts <- chain_frame_counts(calls)
  offsets <- cumsum(c(0L, counts))
  slot <- 0L
  result <- NULL
  n <- length(calls)
  for (k in seq_len(n)) {
    cl <- calls[[k]]
    own_frames <- NULL
    if (k == 1L || identical(cl$transition, "continuation")) {
      slot <- slot + 1L
      own_frames <- frames[offsets[slot] + seq_len(counts[[slot]])]
    }
    if (identical(cl$transition, "wave")) {
      result <- execute(result, wave = as.integer(cl$wave$wave))
    } else {
      input <- switch(
        cl$transition,
        start = NULL,
        continuation = result,
        phase = if (k == 1L) own_frames[[1]] else result
      )
      if (identical(cl$transition, "phase") && !is_tbl_sample(input)) {
        abort_samplyr(
          c(
            "The first recorded call drew a new phase from an earlier
             sample, which was modified before that phase was drawn.",
            "i" = "Supply that modified sample, as the {.cls tbl_sample} the
                   phase was drawn from, as its frame."
          ),
          class = "samplyr_error_replay_phase_frame",
          call = call
        )
      }
      record <- prepare_panel_record(cl$panel_assignment, "A replay")
      panels <- decode_panel_argument(cl, record)
      small_pool <- decode_small_pool_argument(record, panels)
      panel_stage <- decode_panel_stage_argument(record)
      inherited_panels <- identical(cl$transition, "continuation") &&
        !is_null(attr(input, "metadata")$panel_assignment)
      if (inherited_panels) {
        panels <- small_pool <- panel_stage <- NULL
      }
      reps <- cl$reps
      if (is_tbl_sample(input) && has_multiple_replicates(input)) {
        reps <- NULL
      }
      run <- function(.data, frame) {
        execute(
          .data,
          frame,
          stages = cl$stages,
          seed = cl$seed,
          panels = panels,
          panel_stage = panel_stage,
          small_pool = small_pool,
          reps = reps,
          frame_digest = frame_digest
        )
      }
      result <- with_replay_rng(cl$rng, switch(
        cl$transition,
        start = run(cl$design, own_frames),
        continuation = run(input, own_frames),
        phase = run(cl$design, input)
      ), call = call)
    }
    n_recorded <- cl$n_selected
    if (!is_null(n_recorded) && nrow(result) != as.integer(n_recorded)) {
      cli_warn(c(
        "Call {k} of {n} replayed {nrow(result)} row{?s}; the receipt
         recorded {n_recorded}.",
        "i" = "The frame likely differs from the one used originally."
      ), class = "samplyr_warning_replay_rows")
    }
  }
  result
}

#' Every call of a chain but a wave draws, so each needs its seed
#' @noRd
check_chain_seeds <- function(calls, call = caller_env()) {
  n <- length(calls)
  for (k in seq_len(n)) {
    cl <- calls[[k]]
    if (!identical(cl$transition, "wave") && length(cl$seed) == 0L) {
      abort_samplyr(
        c(
          "Call {k} of the {n} {.fn execute} calls behind this sample ran
           without a seed, so the sample cannot be reproduced.",
          "i" = "Re-run that call and the ones after it with {.arg seed}
                 before saving."
        ),
        class = "samplyr_error_receipt_no_seed",
        call = call
      )
    }
  }
  invisible(calls)
}

#' The calls of an encoded receipt, far enough decoded to count their frames
#' @noRd
encoded_chain_calls <- function(execution) {
  entries <- c(
    execution[["earlier_calls"]],
    list(list(transition = execution$transition, frames = execution$frames))
  )
  lapply(entries, function(entry) {
    list(
      transition = decode_chr(entry$transition),
      frames = list(n_supplied = as.integer(entry$frames$count %||% 1L))
    )
  })
}
