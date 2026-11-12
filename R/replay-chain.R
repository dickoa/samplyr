## Chained receipts

#' The execute() calls behind a chained sample
#'
#' A continuation, a new phase and a materialized occasion each start from the
#' result of an earlier call. The final call is described by the receipt
#' itself. This returns the earlier calls, oldest first, each as replay needs
#' it, and how the final call used the one before it.
#'
#' The walk stops early at an earlier phase that was modified before the next
#' phase was drawn from it. Replaying that phase cannot rebuild the rows the
#' next one saw, so the modified sample becomes the first call's frame. The
#' chain cannot be replayed at all, and the result is `NULL`, when a
#' continuation or an occasion started from a modified sample, or when a
#' continuation predates the record of its parent's seed.
#'
#' @return `NULL`, or a list with `transition` (the final call's) and `calls`.
#' @noRd
execution_chain <- function(sample) {
  entries <- chain_entries(chain_view_of_sample(sample))
  if (is_null(entries)) {
    return(NULL)
  }
  n <- length(entries)
  list(
    transition = entries[[n]]$view$transition,
    calls = lapply(entries[-n], function(e) e$call %||% chain_call_of_view(e$view))
  )
}

#' The calls behind a view, oldest first and ending with the view itself
#'
#' Each entry holds a `view`, which carries the transition from the call
#' before it, or a ready `call`. A master whose later stages were drawn one
#' occasion at a time stands for its own calls followed by one call per occasion, in
#' the order they were drawn, and the view is the last of those. `NULL` when
#' the chain cannot be replayed.
#' @noRd
chain_entries <- function(view) {
  entries <- list()
  repeat {
    draws <- view$meta$per_occasion_draws
    if (length(draws) > 0L) {
      view$transition <- "occasion_draw"
      master <- occasion_draw_master_view(view)
      if (isTRUE(master$modified)) {
        return(NULL)
      }
      head <- chain_entries(master)
      if (is_null(head)) {
        return(NULL)
      }
      n_master <- master$meta$n_selected
      earlier <- lapply(draws[-length(draws)], function(d) {
        list(call = occasion_draw_call(d, n_selected = n_master,
                                   design = view$design))
      })
      return(c(head, earlier, list(list(view = view)), entries))
    }
    link <- chain_predecessor(view)
    view$transition <- link$transition
    entries <- c(list(list(view = view)), entries)
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
  entries
}

#' The master an occasion-by-occasion draw started from, as a view
#'
#' Before the last occasion the sample is the master itself, holding the draws.
#' After it, the master is the record the completed sample continues.
#' @noRd
occasion_draw_master_view <- function(view) {
  remaining <- setdiff(seq_along(view$design$stages), view$stages_executed)
  if (length(remaining) > 0L) {
    meta <- view$meta
    meta$per_occasion_draws <- NULL
    return(list(
      seed = view$seed,
      stages_executed = view$stages_executed,
      meta = meta,
      design = view$design,
      modified = isTRUE(view$modified)
    ))
  }
  parent <- view$meta$continued_from
  list(
    seed = parent$seed,
    stages_executed = parent$stages_executed,
    meta = parent,
    design = view$design,
    modified = isTRUE(parent$realization_modified)
  )
}

#' One occasion's call in the form replay reads
#' @noRd
occasion_draw_call <- function(draw, n_selected, design) {
  list(
    transition = "occasion_draw",
    seed = draw$seed,
    stages = as.integer(draw$stages),
    n_selected = n_selected,
    frames = frame_record_or_default(draw$frame_schedule, draw$stages),
    occasion = encode_occasion_draw(draw),
    rng = draw$execution_environment$rng,
    design = design
  )
}

#' @noRd
encode_occasion_draw <- function(draw) {
  list(
    occasion = as.integer(draw$occasion),
    active_panels = I(as.integer(draw$active_panels))
  )
}

#' What the last occasion's call contributes to the receipt of its master
#'
#' `NULL` for a sample with no occasion drawn this way.
#' @noRd
occasion_draw_final_call <- function(sample) {
  draws <- occasion_draws(sample)
  if (length(draws) == 0L) {
    return(NULL)
  }
  draws[[length(draws)]]
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
    multi_hit <- any(vapply(
      parent$stages_executed,
      function(k) is_multi_hit_method(view$design$stages[[k]]$draw_spec),
      logical(1)
    ))
    return(list(transition = "continuation", view = list(
      seed = parent$seed,
      stages_executed = parent$stages_executed,
      meta = parent,
      design = view$design,
      modified = isTRUE(parent$realization_modified) ||
        (multi_hit && isTRUE(parent$reordered))
    )))
  }
  prev <- view$meta$prev_phase
  if (is_null(prev)) {
    return(list(transition = "start"))
  }
  transition <- if (identical(prev$transition, "panel_activation")) {
    "occasion"
  } else {
    "phase"
  }
  if (!is_tbl_sample(prev$sample)) {
    return(list(transition = transition, view = NULL))
  }
  list(transition = transition, view = chain_view_of_sample(
    prev$sample,
    modified = !sample_realization_status(prev$sample)$ok ||
      (identical(transition, "phase") && !sample_order_intact(prev$sample))
  ))
}

#' One call in the form replay reads, from memory or from a file
#' @noRd
chain_call_of_view <- function(view) {
  meta <- view$meta
  # The last occasion's draw is the call such a view stands for.
  if (identical(view$transition, "occasion_draw")) {
    draws <- meta$per_occasion_draws
    return(occasion_draw_call(draws[[length(draws)]],
                          n_selected = meta$n_selected, design = view$design))
  }
  list(
    transition = view$transition,
    seed = view$seed,
    stages = frame_record_or_default(
      meta$frame_schedule, view$stages_executed
    )$stages,
    n_selected = meta$n_selected,
    reps = meta$extracted_from$reps %||% meta$reps,
    replicate = meta$extracted_from$replicate,
    panels = meta$panels,
    panel_assignment = if (!is_null(meta$panel_assignment)) {
      encode_panel_assignment(meta$panel_assignment)
    },
    frames = frame_record_or_default(
      meta$frame_schedule, view$stages_executed
    ),
    occasion = if (!is_null(meta$occasion)) encode_occasion(meta$occasion),
    settlement = if (settled_here(meta$reserve_settlement)) {
      encode_reserve_settlement(meta$reserve_settlement)
    },
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
    if (!is_null(cl[["replicate"]])) {
      out$replicate <- as.integer(cl[["replicate"]])
    }
    if (!is_null(cl$panels)) out$panels <- as.integer(cl$panels)
    out$panel_assignment <- cl$panel_assignment
    out$frames <- encode_frame_schedule(cl$frames)
    out$occasion <- cl$occasion
    out$settlement <- cl$settlement
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
      replicate = if (length(entry[["replicate"]]) > 0) {
        as.integer(entry[["replicate"]])
      },
      panels = if (length(entry$panels) > 0) entry$panels,
      panel_assignment = entry$panel_assignment,
      frames = decode_frame_schedule(entry$frames),
      occasion = entry$occasion,
      settlement = entry$settlement,
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
#' continuation and every occasion's draw of later stages. A new phase reads the
#' phase before it, and an occasion its master.
#' @noRd
chain_frame_counts <- function(calls) {
  takes <- vapply(seq_along(calls), function(k) {
    k == 1L || calls[[k]]$transition %in% c("continuation", "occasion_draw")
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
#' originally read: the caller's frames for the first call, for each
#' continuation and for each occasion's draw of later stages, the replayed
#' earlier phase for a new phase, the replayed master for an occasion. Arguments a call inherited rather than declared are not
#' passed again, because execute() refuses them: panels on a sample that
#' already carries an assignment, and replicates on a replicated input.
#' @noRd
replay_execution_chain <- function(calls, frames, frame_digest = "summary",
                                   fingerprint = "error",
                                   call = caller_env()) {
  counts <- chain_frame_counts(calls)
  offsets <- cumsum(c(0L, counts))
  slot <- 0L
  result <- NULL
  n <- length(calls)
  for (k in seq_len(n)) {
    cl <- calls[[k]]
    own_frames <- NULL
    if (k == 1L || cl$transition %in% c("continuation", "occasion_draw")) {
      slot <- slot + 1L
      own_frames <- frames[offsets[slot] + seq_len(counts[[slot]])]
    }
    if (identical(cl$transition, "occasion")) {
      at <- recorded_occasions(result, cl$occasion$occasion)
      result <- execute(result, period = at$period, subround = at$subround)
    } else if (identical(cl$transition, "occasion_draw")) {
      at <- recorded_occasions(result, cl$occasion$occasion)
      result <- with_replay_rng(cl$rng, execute(
        result,
        own_frames,
        period = at$period,
        subround = at$subround,
        stages = cl$stages,
        seed = cl$seed
      ), call = call)
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
             sample, which was modified or reordered before that phase was
             drawn.",
            "i" = "Supply that earlier sample, as the {.cls tbl_sample} the
                   phase was drawn from, as its frame."
          ),
          class = "samplyr_error_replay_phase_frame",
          call = call
        )
      }
      if (
        k == 1L &&
          identical(cl$transition, "phase") &&
          !is_null(cl$input_hash) &&
          !identical(fingerprint, "ignore") &&
          !identical(frame_content_hash(input), cl$input_hash)
      ) {
        msg <- c(
          "The supplied {.arg frame} differs from the earlier-phase sample
           recorded with the design.",
          "*" = "Row values or row order differ from the sample this phase
                 was drawn from.",
          "i" = "The replay yields a valid sample from this frame, but
                 not the recorded one."
        )
        if (identical(fingerprint, "error")) {
          abort_samplyr(
            msg,
            class = "samplyr_error_replay_frame_mismatch",
            call = call
          )
        } else if (identical(fingerprint, "warn")) {
          cli_warn(msg, class = "samplyr_warning_replay_frame_mismatch")
        } else {
          cli::cli_inform(msg, class = "samplyr_message_replay_frame_mismatch")
        }
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
    result <- replay_extract_replicate(result, cl[["replicate"]], cl$reps,
                                       call = call)
    result <- replay_reserve_settlement(result, cl$settlement, call = call)
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

#' Every call of a chain but an occasion draws, so each needs its seed
#' @noRd
check_chain_seeds <- function(calls, call = caller_env()) {
  n <- length(calls)
  for (k in seq_len(n)) {
    cl <- calls[[k]]
    if (!identical(cl$transition, "occasion") && length(cl$seed) == 0L) {
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

#' The period and subround of occasions a receipt numbers
#' @param sample The replayed master, whose record holds the schedule.
#' @noRd
recorded_occasions <- function(sample, occasions) {
  record <- prepare_panel_record(
    attr(sample, "metadata")$panel_assignment, "A replay"
  )
  pairs <- grid_pairs(record$schedule)
  occasions <- as.integer(occasions)
  list(period = pairs$period[occasions], subround = pairs$subround[occasions])
}
