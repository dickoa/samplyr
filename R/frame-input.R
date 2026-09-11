#' Supply sampling frames and stage registers
#'
#' A frame is a data frame or a non-empty ordered list of data frames.
#' A data frame and a one-element list containing it have the same meaning.
#' [execute()] also accepts separate data-frame arguments after `frame`.
#'
#' ## One hierarchy or separate registers
#'
#' A single hierarchy contains the rows needed for the requested stages,
#' including parent identifiers. Variables describing a selected cluster,
#' such as its MOS and stratum, must be constant within that cluster.
#' Separate registers describe one sampling level each and map positionally
#' to the requested stages. List names label registers in messages. They do
#' not reorder stages or define joins. Child registers carry the parent keys
#' needed to identify eligible descendants.
#'
#' ## Continuing fieldwork
#'
#' Use `execute(partial_sample, next_register)` to resume the same design
#' after listing. The earlier selection and weights are retained. If one
#' supplied frame could cover either the next stage or several remaining
#' stages, specify `stages` to state which stages to execute.
#'
#' Use `execute(new_design, previous_sample)` to select a new phase from an
#' earlier sample. This records an additional probability-sampling operation.
#' It is distinct from continuing the unfinished stages of a design.
#'
#' Frame checks apply before selection: column names must be unambiguous,
#' design variables must be present, and keys and cluster-level variables
#' must satisfy the design's identity rules. A frame fingerprint in a saved
#' design records the supplied data. It does not certify executability.
#'
#' @seealso [execute()], [validate_frame()], [frame_summary()],
#'   [write_design()], `vignette("three-stage-sampling")`
#' @name frame-input-grammar
NULL

#' Canonicalize any public frame input into one ordered collection
#'
#' @param frame A data frame, or a list of data frames.
#' @param arg The caller's argument name, for the message.
#' @return A list with `frames` (unnamed, ordered), `labels` (character, `""`
#'   where absent), `n_supplied`, and `input_was_list`. The last is a
#'   diagnostic only: it must never change acceptance or persisted schema,
#'   which is what made a singleton list write a different file.
#' @noRd
normalize_frame_input <- function(frame, arg = "frame",
                                  call = rlang::caller_env()) {
  # Data frames are lists, so this test comes first.
  if (is.data.frame(frame)) {
    return(list(
      frames = list(frame),
      labels = "",
      n_supplied = 1L,
      input_was_list = FALSE
    ))
  }

  if (!is.list(frame)) {
    abort_samplyr(
      c(
        "{.arg {arg}} must be a data frame or a list of data frames.",
        "x" = "Got {.cls {class(frame)[[1]]}}."
      ),
      class = "samplyr_error_frame_not_data_frame",
      call = call
    )
  }

  if (length(frame) == 0L) {
    abort_samplyr(
      c(
        "{.arg {arg}} must contain at least one data frame.",
        "x" = "An empty list supplies no frame at all.",
        "i" = "Pass the frames the design samples from, one per stage, or a
               single shared hierarchy."
      ),
      class = "samplyr_error_frame_count",
      call = call
    )
  }

  labels <- names(frame) %||% rep("", length(frame))
  labels[is.na(labels)] <- ""
  for (i in seq_along(frame)) {
    if (!is.data.frame(frame[[i]])) {
      abort_samplyr(
        c(
          "{frame_token(i, labels[[i]])} must be a data frame.",
          "x" = "Got {.cls {class(frame[[i]])[[1]]}}.",
          "i" = "Frames are supplied positionally, in stage order."
        ),
        class = "samplyr_error_frame_not_data_frame",
        call = call
      )
    }
  }

  # Preserve collection names as diagnostic frame labels.
  list(
    frames = frame,
    labels = labels,
    n_supplied = length(frame),
    input_was_list = TRUE
  )
}

#' Apply the rules execute() enforces before it samples
#'
#' A preflight that approves a frame execution refuses is worse than no
#' preflight, so these are called on the same primitives from both.
#'
#' @param allow_generated Passed through: a continuation frame legitimately
#'   carries the generated columns of the stages already run.
#' @param allow_stripped `TRUE` where the caller is not starting a fresh
#'   execution, so a sample whose class was dropped cannot silently rerun
#'   stage 1.
#' @noRd
check_frames_executable <- function(frames,
                                    labels = NULL,
                                    allow_generated = FALSE,
                                    allow_stripped = TRUE,
                                    require_rows = TRUE,
                                    call = rlang::caller_env()) {
  labels <- labels %||% rep("", length(frames))

  for (i in seq_along(frames)) {
    if (require_rows && nrow(frames[[i]]) == 0L) {
      abort_samplyr(
        "{sentence_frame_token(i, labels[[i]])} has 0 rows.",
        class = "samplyr_error_frame_empty",
        call = call
      )
    }
    if (!allow_stripped) {
      check_stripped_sample_frame(frames[[i]], i, labels[[i]], call = call)
    }
    validate_execute_frame_names(
      frames[[i]],
      index = i,
      label = labels[[i]],
      allow_generated = allow_generated ||
        is_tbl_sample(frames[[i]]),
      call = call
    )
  }

  invisible(NULL)
}

#' `frame_token()` at the start of a sentence
#' @noRd
sentence_frame_token <- function(frame_index, frame_label = NULL) {
  token <- frame_token(frame_index, frame_label)
  substr(token, 1L, 1L) <- "F"
  token
}

#' Refuse a sample whose class was dropped as the frame of a fresh execution
#'
#' A class-dropping operation such as `tidyr::uncount()` can leave both the
#' sample attributes and every generated design column on an apparently plain
#' frame. Starting a design from it would rerun stage 1 silently.
#' @noRd
check_stripped_sample_frame <- function(frame, index, label = "",
                                        call = rlang::caller_env()) {
  if (!looks_like_stripped_tbl_sample(frame)) {
    return(invisible(NULL))
  }
  has_provenance <- is_sampling_design(attr(frame, "design")) &&
    !is_null(attr(frame, "stages_executed"))

  abort_samplyr(
    c(
      "{sentence_frame_token(index, label)} looks like a {.cls tbl_sample}
       whose class was dropped.",
      "x" = "Executing it from a {.cls sampling_design} would start a fresh
             execution at stage 1 and could silently produce incorrect
             weights.",
      "i" = "For operational multistage sampling, continue from the
             unmodified partial sample and pass this object only as the
             listing frame:
             {.code partial_sample |> execute(listing_frame)}.",
      if (has_provenance) {
        c(
          "i" = "For a genuinely new sampling phase, restore the previous
                 sample explicitly with {.fn as_tbl_sample} before using it
                 as the new design's frame."
        )
      },
      "i" = "Generated columns such as {.field .weight}, {.field .weight_k},
             {.field .fpc_k}, {.field .sample_id}, and {.field .stage} are
             evidence of an earlier execution."
    ),
    class = "samplyr_error_stripped_sample_frame",
    call = call
  )
}

#' How many frames a design or an executed sample can be serialized with
#'
#' Fingerprints record which frames a design was written against. A count the
#' design could not have been executed with, or one contradicting the receipt
#' of a sample that already was, describes a call that never happened. Refusing
#' it at the write side means the contradictory artifact is never created,
#' rather than being diagnosed on read.
#' @noRd
check_serialization_frame_count <- function(x, design, n_supplied,
                                            call = rlang::caller_env()) {
  if (is_tbl_sample(x)) {
    record <- attr(x, "metadata")$frame_schedule
    recorded <- record$n_supplied
    if (is_null(recorded) || identical(as.integer(recorded), n_supplied)) {
      return(invisible(NULL))
    }
    abort_samplyr(
      c(
        "{.arg frame} must be the {recorded} frame{?s} this sample was
         executed with, not {n_supplied}.",
        "i" = "The fingerprints written here are what {.fn replay_design}
               verifies the replay frames against, so they have to describe
               the same call."
      ),
      class = c(
        "samplyr_error_serialization_frame_count",
        "samplyr_error_frame_count"
      ),
      call = call
    )
  }

  n_stages <- length(design$stages)
  if (n_supplied == 1L || n_supplied == n_stages) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "This design has {n_stages} stage{?s} and cannot be executed with
       {n_supplied} frames.",
      "i" = "Supply one shared hierarchy, or one frame per stage
             ({n_stages})."
    ),
    class = c(
      "samplyr_error_serialization_frame_count",
      "samplyr_error_frame_count"
    ),
    call = call
  )
}
