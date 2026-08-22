#' Convert a sampling design to a list
#'
#' Converts a sampling design object to a plain list representation,
#' useful for inspection, serialization, or export.
#'
#' @param x A `sampling_design` object
#' @param ... Must be empty.
#'
#' @return A list representation of the design containing:
#'   - `title`: The design title (if any)
#'   - `stages`: A list of stage specifications
#'
#' @examples
#' design <- sampling_design(title = "Household Survey") |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 500)
#'
#' as.list(design)
#'
#' @family serialization
#' @export
as.list.sampling_design <- function(x, ...) {
  rlang::check_dots_empty()
  result <- list(
    title = x$title
  )

  result$stages <- lapply(x$stages, function(stage) {
    stage_list <- list()

    if (!is_null(stage$label)) {
      stage_list$label <- stage$label
    }

    if (!is_null(stage$strata)) {
      stage_list$strata <- list(
        vars = stage$strata$vars,
        alloc = stage$strata$alloc
      )
      if (!is_null(stage$strata$variance)) {
        stage_list$strata$variance_columns <- names(stage$strata$variance)
      }
      if (!is_null(stage$strata$cost)) {
        stage_list$strata$cost_columns <- names(stage$strata$cost)
      }
    }

    if (!is_null(stage$clusters)) {
      stage_list$clusters <- list(
        vars = stage$clusters$vars
      )
    }

    if (!is_null(stage$draw_spec)) {
      stage_list$draw <- list()
      if (!is_null(stage$draw_spec$n)) {
        if (is.data.frame(stage$draw_spec$n)) {
          stage_list$draw$n <- "custom (data frame)"
          stage_list$draw$n_columns <- names(stage$draw_spec$n)
        } else {
          stage_list$draw$n <- stage$draw_spec$n
        }
      }
      if (!is_null(stage$draw_spec$frac)) {
        if (is.data.frame(stage$draw_spec$frac)) {
          stage_list$draw$frac <- "custom (data frame)"
          stage_list$draw$frac_columns <- names(stage$draw_spec$frac)
        } else {
          stage_list$draw$frac <- stage$draw_spec$frac
        }
      }
      stage_list$draw$method <- stage$draw_spec$method
      if (!is_null(stage$draw_spec$mos)) {
        stage_list$draw$mos <- stage$draw_spec$mos
      }
      if (!is_null(stage$draw_spec$aux)) {
        stage_list$draw$aux <- stage$draw_spec$aux
      }
      if (!is_null(stage$draw_spec$bounds)) {
        stage_list$draw$bounds <- stage$draw_spec$bounds
      }
      if (!is_null(stage$draw_spec$spread)) {
        stage_list$draw$spread <- stage$draw_spec$spread
      }
      if (!is_null(stage$draw_spec$control)) {
        stage_list$draw$control <- vapply(
          stage$draw_spec$control,
          rlang::as_label,
          character(1)
        )
      }
      if (!is_null(stage$draw_spec$min_n)) {
        stage_list$draw$min_n <- stage$draw_spec$min_n
      }
      if (!is_null(stage$draw_spec$max_n)) {
        stage_list$draw$max_n <- stage$draw_spec$max_n
      }
      if (!is_null(stage$draw_spec$certainty_size)) {
        stage_list$draw$certainty_size <- stage$draw_spec$certainty_size
      }
      if (!is_null(stage$draw_spec$certainty_prop)) {
        stage_list$draw$certainty_prop <- stage$draw_spec$certainty_prop
      }
      if (!is_null(stage$draw_spec$round)) {
        stage_list$draw$round <- stage$draw_spec$round
      }
      if (
        !is_null(stage$draw_spec$on_empty) &&
          stage$draw_spec$on_empty != "error"
      ) {
        stage_list$draw$on_empty <- stage$draw_spec$on_empty
      }
    }
    stage_list
  })
  result
}

## Design files

# The disk format is a versioned JSON envelope.
# Scalar, named vector, and data frame values become JSON values, objects,
# and row arrays. Control terms use declarative JSON and never contain R code.
# One frame uses `frame.fingerprint`. Multiple frames use ordered
# `frame.fingerprints` plus `execution.frames` for stage mapping.

design_format_id <- "samplyr/design"
# Write the lowest version that preserves the file's meaning.
design_format_version <- 3L
method_vocabulary_id <- "samplyr/common-sampling-method"
method_vocabulary_version <- 1L

# Collections use a distinct format to prevent single-design replay.
# Each component adds its name and membership column to a full design document.
frame_stack_format_id <- "samplyr/frame-stack"
frame_stack_format_version <- 1L

# Shared-weight files store the source and declarative transformation only.
shared_sample_format_id <- "samplyr/shared-sample"
shared_sample_format_version <- 1L

#' Write a sampling design to a file
#'
#' @description
#' `write_design()` saves a sampling design (or the design carried by an
#' executed sample) to a human-readable, samplyr-native JSON file.
#' `read_design()` reads it back into a `sampling_design` that executes
#' identically to the original.
#'
#' The file format is versioned JSON and diffable in version control. It
#' stores the complete design specification (stages, stratification,
#' clustering, draw settings, including per-stratum vectors and data frames),
#' never the frame data itself.
#'
#' A file written from an executed sample can also contain its frame digest.
#' That digest retains selected-unit identifiers and may contain per-unit chance
#' metadata. Treat an execution receipt as potentially confidential even
#' though it does not contain the ordinary frame columns.
#'
#' @details
#' ## Lifecycle
#'
#' The serialization interface and its samplyr-native file format are
#' experimental. They support samplyr persistence and replay. They are not a
#' finalized cross-tool survey-sampling interchange standard. The structure
#' may change while that separate specification is developed.
#'
#' ## Frame information
#'
#' Designs are frame-independent, and so are design files. Two derived
#' blocks describe the frame without embedding it:
#'
#' - *Requirements* (always written): the columns each stage needs
#'   (stratification, clustering, `mos`, `prn`, `aux`, and control
#'   variables), so any candidate frame can be checked before execution
#'   with [validate_frame()].
#' - *Fingerprint* (written when `frame` is supplied): portable dimensions
#'   and column types in `frame`, plus the R source label, native classes, and
#'   content hash in `tools.samplyr`. Together these can verify that a frame is
#'   the exact one the design was built against without putting R details in
#'   the common metadata.
#'
#' ## Execution receipts
#'
#' When `x` is a `tbl_sample`, the file additionally records an execution
#' receipt: every argument of the [execute()] call that affects the
#' result (`seed`, executed stages, `panels`, `reps`, and the per
#' replicate seeds), the execution-time RNG configuration and package
#' versions, plus the number of selected units and the execution timestamp.
#' Together with the frame fingerprint this makes a single-call sample
#' reproducible when the same frame, compatible package implementations, and
#' any recorded custom methods are available. Running
#' `replay_design(read_design(path), frame)` then obtains the same
#' `tbl_sample` (the same rows in the same order, including `.panel` and
#' `.replicate` assignments) with only the execution timestamp differing.
#' The sampled rows themselves are not stored. Use a data format (CSV,
#' parquet) for those.
#'
#' Receipts describe one [execute()] call. A sample built by several
#' calls (a stage continuation or a multi-phase pipeline) is flagged as
#' `chained` in the receipt and `write_design()` warns: replaying the
#' final call alone cannot reproduce it, so save and replay each phase
#' or stage batch separately. A sample whose rows or design columns were
#' modified after execution is likewise flagged (`modified`). Its
#' receipt describes the original execution, not the modified object.
#'
#' The receipt also records how frames were mapped to stages: the frame
#' mode, how many frames were supplied, their optional labels, and the
#' frame position each executed stage drew from. This is what a
#' `chained` receipt describes too, for its final call only, so the
#' mapping never implies that a chained sample can be replayed. A
#' receipt written before these fields existed is read as the one-frame
#' call it can only have been.
#'
#' ## Control expressions
#'
#' `draw(control = ...)` expressions are stored as declarative JSON terms,
#' not R code. Each term records an ordering type (`"ascending"`,
#' `"descending"`, or `"serpentine"`) and its variables. Only bare column
#' names, `dplyr::desc()`, and [serp()] can be represented.
#' `write_design()` errors on anything else.
#'
#' ## Declarative and implementation metadata
#'
#' The `design`, `frame`, and `execution` blocks use declarative JSON rather
#' than R expressions. Selection methods carry samplyr's internal semantic
#' descriptor. The `tools.samplyr` block records exact method names, R classes,
#' the R-derived frame hash, and execution environment needed to rebuild and
#' replay the native object. These descriptors are not a finalized external
#' method vocabulary.
#'
#' ## Frame collections
#'
#' A `frame_stack` from [stack_frames()] is written as `samplyr/frame-stack`,
#' its own format. Each component entry is a complete `samplyr/design`
#' document plus the two fields that make it a component: its name, and the
#' column saying which frames its units belong to. The collection's key and
#' any overlaps declared with [declared_overlaps()] are recorded
#' alongside. Give `frame` as a list keyed by
#' component name, since the components are separate selections with separate
#' registers.
#'
#' [read_design()] returns the components' designs and receipts rather than
#' the collection, which needs the registers; [replay_design()] executes each
#' against its register and stacks the results.
#'
#' ## Shared-weight samples
#'
#' A sample carrying shared weights from [share_weights()] is written as
#' `samplyr/shared-sample`, its own format. It records the source selection
#' and the transformation's arguments, and nothing else: the links and the
#' target register are supplied again to [replay_design()], the way a frame
#' is, so no unit-level data and no linkage is written.
#'
#' Two integrity records travel with it, and replay is checked against both.
#' A source that does not reproduce means `frame` is not the register selected
#' from; a result that does not means `links` or `targets` is not the table
#' the transformation was built from.
#'
#' ## What the format does not carry
#'
#' A design and one execution receipt per component, and nothing beyond them.
#' Frame data, target data and link tables are never written. So what cannot
#' be described that way is refused rather than written in part:
#'
#' * a collection whose overlaps come from [exante_overlaps()]. Those resolve
#'   to one chance per selected unit when the collection is formed, which is
#'   unit-level data, and the request that produced them is not kept.
#' * a shared-weight sample used as a component of a collection. A component
#'   entry is a design document, and a collection replays from one register
#'   per component with nowhere to put a link table.
#'
#' Serialize the sample on its own, or the collection without them, and
#' rebuild afterwards. `saveRDS()` preserves any of these objects whole.
#'
#' @param x A `sampling_design`, a `tbl_sample` (the stored design is saved
#'   along with an execution receipt), a `frame_stack`, or a sample carrying
#'   shared weights.
#' @param path File path to write to. Conventionally with a `.json`
#'   extension.
#' @param frame Optional sampling frame. A data frame is the one frame
#'   the design was built against. An ordered list of data frames is the
#'   stage registers, in the order [execute()] received them, and each
#'   is fingerprinted separately. One frame written as a one-element list
#'   is still one frame and is recorded identically. The number of frames
#'   must be one the design could be executed with, and for an executed
#'   sample must be the number its receipt records, so a file cannot say
#'   it was drawn from one frame and carry fingerprints for three. When
#'   supplied, a fingerprint (name,
#'   dimensions, column types, content hash) is stored so the frame can
#'   be verified later. The ordinary frame columns are never written. An
#'   executed sample's receipt can still contain selected-unit identifiers
#'   in its frame digest. The content
#'   hash covers column names, column values, and row order. It does not
#'   depend on the class of the data frame (tibble or data frame) or on
#'   the order of its columns. For a `frame_stack`, a list **named** by
#'   component, holding what each component was drawn from. Matched by name
#'   rather than position, since a list in the wrong order would fingerprint
#'   each component against another's register. For a shared-weight sample,
#'   the register the source selection was drawn from.
#' @param ... These dots are for future extensions and must be empty.
#'   `pretty` follows `...`, so it is matched exactly and must be named.
#' @param pretty Whether to pretty-print the JSON. Defaults to `TRUE` for
#'   files and `FALSE` for [design_json()].
#'
#' @return `write_design()` returns `x` invisibly. `read_design()` returns
#'   a `sampling_design`, a `frame_stack_design` for a frame collection file,
#'   or a `shared_sample_design` for a shared-weight sample file. Any frame
#'   information and execution receipt in the file are attached as the
#'   `"frame_info"` and `"execution"` attributes.
#'
#' @examples
#' design <- sampling_design(title = "Household Survey") |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 200, method = "systematic", control = c(province, ea_id))
#'
#' path <- tempfile(fileext = ".json")
#' write_design(design, path, frame = bfa_eas)
#'
#' restored <- read_design(path)
#' restored
#'
#' # The restored design executes identically
#' s1 <- execute(design, bfa_eas, seed = 42)
#' s2 <- execute(restored, bfa_eas, seed = 42)
#' identical(s1$ea_id, s2$ea_id)
#'
#' # Saving an executed sample records a reproducibility receipt
#' write_design(s1, path, frame = bfa_eas)
#' attr(read_design(path), "execution")$seed
#'
#' # Replay the receipt to reproduce the sample exactly
#' s3 <- replay_design(read_design(path), bfa_eas)
#' identical(s3$ea_id, s1$ea_id)
#'
#' unlink(path)
#' @seealso [replay_design()] for reproducing a sample from its receipt,
#'   [design_json()] for in-memory JSON, [validate_frame()] for
#'   checking a frame against a design, [get_design()] for extracting the
#'   design from a sample.
#' @family serialization
#' @export
write_design <- function(x, path, frame = NULL, ..., pretty = TRUE) {
  check_keyword_args(enquos(...), "pretty")
  if (!is_character(path) || length(path) != 1) {
    cli_abort("{.arg path} must be a single file path")
  }
  frame_label <- frame_label_for(enquo(frame), frame)
  json <- build_design_json(
    x,
    frame = frame,
    frame_label = frame_label,
    pretty = pretty,
    fn_name = "write_design"
  )
  writeLines(json, path, useBytes = TRUE)
  invisible(x)
}

#' Convert a sampling design to JSON
#'
#' Renders a design (or the design carried by a `tbl_sample`) as a JSON
#' string in the same format written by [write_design()]. Useful for
#' storing designs in databases or sending them over APIs. [read_design()]
#' accepts the resulting string as well as file paths.
#'
#' @inheritParams write_design
#'
#' @return A JSON string (class `json`).
#'
#' @examples
#' design <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 100)
#'
#' json <- design_json(design, pretty = TRUE)
#' json
#'
#' # read_design() accepts the JSON string directly
#' restored <- read_design(json)
#' identical(
#'   execute(restored, bfa_eas, seed = 7)$ea_id,
#'   execute(design, bfa_eas, seed = 7)$ea_id
#' )
#' @seealso [write_design()], [read_design()]
#' @family serialization
#' @export
design_json <- function(x, frame = NULL, ..., pretty = FALSE) {
  check_keyword_args(enquos(...), "pretty")
  frame_label <- frame_label_for(enquo(frame), frame)
  build_design_json(
    x,
    frame = frame,
    frame_label = frame_label,
    pretty = pretty,
    fn_name = "design_json"
  )
}

#' @param file A path to a local file, or a JSON string produced by
#'   [design_json()]. URLs are refused: `read_design()` never fetches
#'   remote files. Download the file first and read the local copy.
#' @rdname write_design
#' @export
read_design <- function(file) {
  if (!is_character(file) || length(file) != 1) {
    cli_abort("{.arg file} must be a single file path or JSON string")
  }
  # Never let design reading fetch URL-shaped input.
  if (!grepl("^[[:space:]]*[{[]", file)) {
    if (grepl("^[A-Za-z][A-Za-z0-9+.-]+://", file)) {
      cli_abort(c(
        "{.arg file} must be a local file path or a JSON string, not a URL.",
        "i" = "Download the file first and read the local copy."
      ))
    }
    if (!file.exists(file)) {
      cli_abort(
        "{.arg file} is not valid JSON or a path to an existing file."
      )
    }
  }
  payload <- tryCatch(
    jsonlite::fromJSON(file, simplifyVector = FALSE),
    error = function(cnd) {
      cli_abort(
        "{.arg file} is not valid JSON or a readable file.",
        parent = cnd
      )
    }
  )
  if (identical(payload$format, frame_stack_format_id)) {
    decode_frame_stack_payload(payload)
  } else if (identical(payload$format, shared_sample_format_id)) {
    decode_shared_sample_payload(payload)
  } else {
    decode_design_payload(payload)
  }
}

#' Replay an execution receipt
#'
#' Re-executes a design exactly as recorded in its execution receipt,
#' passing the stored `seed`, `stages`, `panels`, and `reps` back to
#' [execute()]. This avoids reconstructing those arguments by hand from
#' the receipt fields.
#'
#' @details
#' Given the same frame and compatible recorded implementations, the replayed
#' sample is identical to the original: the same rows in the same order, the
#' same weights and design columns, and the same `.panel` and `.replicate`
#' assignments. Only the execution timestamp differs. Replay restores the
#' execution-time RNG configuration and then restores the caller's RNG state.
#' It warns when recorded R or package versions differ.
#'
#' Receipts record a single [execute()] call. A sample produced by
#' several calls (a stage continuation or a multi-phase pipeline)
#' carries a `chained` flag in its receipt and cannot be replayed.
#' Save and replay each phase or stage batch separately.
#'
#' A sample carrying shared weights replays in two steps: the source selection
#' is re-executed against `frame`, then the recorded transformation is
#' re-applied to the `links` and `targets` given here. Those two are not in
#' the file, by design, so they cannot be checked before use; the file instead
#' records what the source and the result hashed to, and replay is checked
#' against both. A source that does not reproduce means `frame` is wrong, and
#' a result that does not means `links` or `targets` is. Both a file and a
#' live shared sample are accepted.
#'
#' A frame collection replays component by component and is stacked again
#' afterwards, so `frame` is a list named by component. Both a collection read
#' back from a file and a live `frame_stack` are accepted. Every component is
#' checked before any of them runs, since replaying re-executes each
#' selection. The collection is rebuilt through [stack_frames()] rather than
#' by restoring its attributes, so a register that has stopped carrying the
#' membership column, or whose key is no longer unique, is reported as that.
#'
#' For a design using a registered custom method, the receipt records a
#' fingerprint of the implementation (the formals and body of the
#' registered `sample_fn` and `joint_fn`). Replay refuses when the
#' currently registered function differs from the recorded one, since
#' identical registry metadata does not imply the same selections.
#' The fingerprint normalizes formatting and comments and does not
#' cover the function's enclosing environment: a registered function
#' that reads from its environment can change behavior without
#' changing its fingerprint.
#'
#' A panelized receipt carries a panel assignment record, and it is read
#' before any panel argument is decoded from it. A record naming an
#' assignment algorithm or a schema version this samplyr does not know is
#' `samplyr_error_panel_record_unsupported` rather than a replay under the
#' current law. A record that does not carry what the version it states
#' requires, or that is not a set of named fields at all, is
#' `samplyr_error_panel_record_malformed` rather than a repaired one: the
#' assignment stage decides what the assignment units are, so filling in a
#' missing one would replay a different assignment of the same sample.
#'
#' When the design was saved with a frame fingerprint, `frame` is
#' compared against it before replaying. A differing frame still yields
#' a valid sample, but not the recorded one, so the default is to error.
#' Several frames are compared one by one and reported by position and
#' recorded label, so a mismatch names the register that moved. After
#' replaying, the row count is checked against the receipt's
#' `n_selected` as a final consistency check.
#'
#' A sample drawn from one register per stage is replayed by passing
#' those registers back as a list, in the same order. The receipt
#' records how many frames the call was given, so supplying the wrong
#' number is `samplyr_error_replay_frame_count` rather than a sample
#' drawn from the wrong pools.
#'
#' @param x A `sampling_design` carrying an execution receipt, as
#'   returned by [read_design()] for a file written from a
#'   `tbl_sample`. A `tbl_sample` is also accepted and is replayed from
#'   its own metadata, which is useful for verifying reproducibility
#'   without a file round trip.
#' @param frame The sampling frame the receipt refers to: a data frame
#'   for a one-frame call, or the ordered list of stage frames for a
#'   call that supplied one register per stage.
#' @param ... Must be empty. Arguments after it are matched by exact name.
#' @param fingerprint How to respond when `frame` differs from the
#'   fingerprint stored in the design file: `"error"` (default), `"warn"`,
#'   `"inform"`, or `"ignore"`.
#' @param links,targets The link table and the target register, for a
#'   shared-weight sample only. Both are required there and refused
#'   elsewhere, since accepting them where nothing uses them would return an
#'   untransformed sample to someone who believes a transformation was
#'   re-applied. Supply the tables the transformation was built from; the
#'   result is checked against the integrity the file records.
#'
#' @return The replayed `tbl_sample`, or the rebuilt `frame_stack` for a
#'   frame collection.
#'
#' @examples
#' sample <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 100) |>
#'   execute(bfa_eas, seed = 11, panels = 4)
#'
#' path <- tempfile(fileext = ".json")
#' write_design(sample, path, frame = bfa_eas)
#'
#' replayed <- replay_design(read_design(path), bfa_eas)
#' identical(replayed$ea_id, sample$ea_id)
#' identical(replayed$.panel, sample$.panel)
#'
#' # One register per stage replays from the same ordered list
#' regions <- dplyr::distinct(bfa_eas, region)
#' two_stage <- sampling_design() |>
#'   add_stage(label = "Regions") |>
#'     cluster_by(region) |>
#'     draw(n = 3) |>
#'   add_stage(label = "EAs") |>
#'     draw(n = 5)
#'
#' registers <- execute(two_stage, regions, bfa_eas, seed = 4)
#' write_design(registers, path, frame = list(regions, bfa_eas))
#' identical(
#'   replay_design(read_design(path), list(regions, bfa_eas))$ea_id,
#'   registers$ea_id
#' )
#'
#' unlink(path)
#' @seealso [write_design()] and [read_design()] for the receipt
#'   round trip, [validate_frame()] for checking a frame against a
#'   design before executing.
#' @family serialization
#' @export
replay_design <- function(
  x,
  frame,
  ...,
  fingerprint = c("error", "warn", "inform", "ignore"),
  links = NULL,
  targets = NULL
) {
  check_keyword_args(enquos(...), c("fingerprint", "links", "targets"))
  fingerprint <- match.arg(fingerprint)

  if (is_frame_stack(x) || is_frame_stack_design(x)) {
    check_replay_link_args(links, targets, "a frame collection")
    return(replay_frame_stack(x, frame, fingerprint))
  }
  if (is_shared_sample_design(x) || is_shared_weight_sample(x)) {
    return(replay_shared_sample(x, frame, fingerprint, links, targets))
  }
  check_replay_link_args(links, targets, "an ordinary sample or design")
  if (is_tbl_sample(x)) {
    design <- get_design(x)
    receipt <- encode_execution(x)
    frame_record <- get_frame_schedule(x)
    frame_info <- NULL
    execution_environment <- attr(x, "metadata")$execution_environment
  } else if (is_sampling_design(x)) {
    design <- x
    receipt <- attr(x, "execution")
    frame_record <- frame_record_or_default(
      receipt$frames, as.integer(unlist(receipt$stages_executed))
    )
    frame_info <- attr(x, "frame_info")
    execution_environment <- attr(
      x,
      "design_tools"
    )$samplyr$execution$environment
  } else {
    cli_abort(
      "{.arg x} must be a {.cls sampling_design} or a {.cls tbl_sample}"
    )
  }

  if (is_null(receipt)) {
    abort_samplyr(
      c(
        "{.arg x} carries no execution receipt.",
        "i" = "Receipts are written by {.fn write_design} when given an
               executed {.cls tbl_sample}, and restored by
               {.fn read_design}."
      ),
      class = "samplyr_error_no_receipt"
    )
  }

  seed <- receipt$seed
  if (is_null(seed)) {
    abort_samplyr(
      c(
        "The execution receipt has no seed, so the sample cannot be
         reproduced.",
        "i" = "Re-run {.fn execute} with {.arg seed} before saving."
      ),
      class = "samplyr_error_receipt_no_seed"
    )
  }

  if (isTRUE(receipt$chained)) {
    abort_samplyr(
      c(
        "This sample was produced by more than one {.fn execute} call
         (stage continuation or multi-phase).",
        "i" = "The receipt records only the final call and cannot
               reproduce the sample.",
        "i" = "Save and replay each phase or stage batch separately."
      ),
      class = "samplyr_error_receipt_chained"
    )
  }

  check_replay_custom_methods(design)
  check_replay_environment(execution_environment)

  frames <- normalize_replay_frames(frame_record, frame)

  if (!identical(fingerprint, "ignore")) {
    diffs <- fingerprint_diffs(frame_info, frames)
    if (length(diffs) > 0) {
      msg <- c(
        "{.arg frame} differs from the
         {cli::qty(length(frames))}frame{?s} recorded with the design:",
        setNames(diffs, rep("*", length(diffs))),
        "i" = "The replay yields a valid sample from this frame, but
               not the recorded one."
      )
      if (identical(fingerprint, "error")) {
        abort_samplyr(
          msg,
          class = "samplyr_error_replay_frame_mismatch"
        )
      } else if (identical(fingerprint, "warn")) {
        cli_warn(msg)
      } else {
        cli::cli_inform(msg)
      }
    }
  }

  stages <- as.integer(unlist(receipt$stages_executed))
  if (length(stages) == 0) {
    stages <- NULL
  }
  reps <- if (!is_null(receipt$reps)) as.integer(receipt$reps) else NULL
  # Validate the assignment law before decoding replay arguments.
  record <- prepare_panel_record(receipt$panel_assignment, "A replay")
  # Replay scheduled masters from their full schedule.
  panels <- decode_panel_argument(receipt, record)
  # Preserve the scheduled small-pool policy.
  small_pool <- decode_small_pool_argument(record, panels)
  # The assignment stage identifies panel units.
  panel_stage <- decode_panel_stage_argument(record)

  result <- with_replay_rng(
    execution_environment$rng,
    execute(
      design,
      frames,
      stages = stages,
      seed = as.integer(seed),
      panels = panels,
      panel_stage = panel_stage,
      small_pool = small_pool,
      reps = reps
    )
  )

  n_recorded <- receipt$n_selected
  if (!is_null(n_recorded) && nrow(result) != as.integer(n_recorded)) {
    cli_warn(c(
      "The replayed sample has {nrow(result)} row{?s}; the receipt
       recorded {n_recorded}.",
      "i" = "The frame likely differs from the one used originally."
    ))
  }

  result
}

#' Require the replay to supply the frames the recorded call was given
#'
#' A receipt from separately supplied registers cannot be replayed against one
#' frame: the stages would all draw from it and select different units. The
#' count is checked before any fingerprint so the caller learns the shape is
#' wrong rather than that three frames' worth of content disagrees.
#' @noRd
normalize_replay_frames <- function(record, frame, call = caller_env()) {
  frames <- as_frame_list(frame, call = call)
  if (length(frames) == record$n_supplied) {
    return(frames)
  }
  abort_samplyr(
    c(
      "The receipt records {record$n_supplied} supplied frame{?s};
       {length(frames)} {?was/were} given.",
      "x" = "Frames map to stages by position, so replay needs the same
             frames in the same order.",
      if (identical(record$mode, "separate_frames")) c(
        "i" = "This sample was drawn from one register per stage:
               {.code replay_design(x, list(<frame 1>, ...))}."
      ) else c(
        "i" = "This sample was drawn from one frame covering every stage."
      )
    ),
    class = "samplyr_error_replay_frame_count",
    call = call
  )
}

#' Verify that registered methods required by a restored design are present
#' and still advertise the metadata recorded in the design file.
#' @noRd
check_replay_custom_methods <- function(design, call = caller_env()) {
  specs <- lapply(design$stages, function(stage) stage$draw_spec)
  specs <- Filter(function(spec) !is_null(spec$method_type), specs)
  if (length(specs) == 0) {
    return(invisible(design))
  }

  for (spec in specs) {
    native_name <- sondage_method_name(spec$method)
    if (!sondage::is_registered_method(native_name)) {
      abort_samplyr(
        c(
          "Cannot replay the unregistered custom method {.val {spec$method}}.",
          "i" = "Register the same implementation with
                 {.fn sondage::register_method} before replaying."
        ),
        class = "samplyr_error_replay_method_unregistered",
        call = call
      )
    }

    current <- sondage::method_spec(native_name)
    # Fingerprint registered formals and bodies for replay safety.
    current$implementation <- method_implementation_hash(current)
    recorded <- list(
      type = spec$method_type,
      fixed_size = spec$method_fixed,
      variance_family = spec$method_variance,
      probabilities = spec$method_probabilities,
      implementation = spec$method_implementation
    )
    fields <- names(recorded)[!vapply(recorded, is_null, logical(1))]
    agrees <- vapply(
      fields,
      function(field) identical(recorded[[field]], current[[field]]),
      logical(1)
    )
    if (!all(agrees)) {
      differing <- fields[!agrees]
      abort_samplyr(
        c(
          "Registered method {.val {spec$method}} differs from the
           method recorded in the design file
           ({.field {differing}} disagree{?s/}).",
          "i" = "Re-register the original implementation before replaying."
        ),
        class = "samplyr_error_replay_method_mismatch",
        call = call
      )
    }
  }
  invisible(design)
}

#' Warn when implementation versions differ from the execution receipt.
#' @noRd
check_replay_environment <- function(recorded, call = caller_env()) {
  if (is_null(recorded)) {
    return(invisible(NULL))
  }
  current <- capture_execution_environment()
  pairs <- list(
    R = c(recorded$language$version, current$language$version),
    samplyr = c(recorded$packages$samplyr, current$packages$samplyr),
    sondage = c(recorded$packages$sondage, current$packages$sondage),
    svyplan = c(recorded$packages$svyplan, current$packages$svyplan)
  )
  differences <- character()
  for (label in names(pairs)) {
    pair <- decode_chr(pairs[[label]])
    if (length(pair) == 2 && !identical(pair[[1]], pair[[2]])) {
      differences <- c(
        differences,
        sprintf(
          "%s: recorded %s, current %s",
          label,
          pair[[1]],
          pair[[2]]
        )
      )
    }
  }
  if (length(differences) > 0) {
    cli_warn(
      c(
        "The replay environment differs from the recorded execution:",
        setNames(differences, rep("*", length(differences))),
        "i" = "Replay will be attempted, but an identical realization is
               not guaranteed."
      ),
      call = call
    )
  }
  invisible(recorded)
}

#' Evaluate replay under its recorded RNG configuration, restoring the
#' caller's RNG kind and state afterwards.
#' @noRd
with_replay_rng <- function(rng, code, call = caller_env()) {
  if (is_null(rng)) {
    return(force(code))
  }

  kind <- decode_chr(rng$kind)
  normal_kind <- decode_chr(rng$normal_kind)
  sample_kind <- decode_chr(rng$sample_kind)
  if (
    length(kind) != 1 || length(normal_kind) != 1 ||
      length(sample_kind) != 1
  ) {
    abort_samplyr(
      "The execution receipt contains an invalid RNG configuration.",
      class = "samplyr_error_replay_rng",
      call = call
    )
  }

  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }
  on.exit({
    do.call(
      RNGkind,
      list(
        kind = old_kind[[1]],
        normal.kind = old_kind[[2]],
        sample.kind = old_kind[[3]]
      )
    )
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  tryCatch(
    do.call(
      RNGkind,
      list(
        kind = kind,
        normal.kind = normal_kind,
        sample.kind = sample_kind
      )
    ),
    error = function(cnd) {
      cli_abort(
        "Cannot restore the RNG configuration recorded for this execution.",
        parent = cnd,
        class = "samplyr_error_replay_rng",
        call = call
      )
    }
  )
  force(code)
}

## Encoding

#' @noRd
build_design_json <- function(
  x,
  frame,
  frame_label,
  pretty,
  fn_name,
  call = caller_env()
) {
  payload <- if (is_shared_weight_sample(x)) {
    shared_sample_payload(
      x,
      frame = frame,
      frame_label = frame_label,
      fn_name = fn_name,
      call = call
    )
  } else if (is_frame_stack(x)) {
    frame_stack_payload(
      x,
      frame = frame,
      frame_label = frame_label,
      fn_name = fn_name,
      call = call
    )
  } else {
    design_payload(
      x,
      frame = frame,
      frame_label = frame_label,
      fn_name = fn_name,
      call = call
    )
  }
  jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    dataframe = "rows",
    digits = NA,
    na = "null",
    null = "null",
    pretty = pretty
  )
}

#' Refuse link arguments where they describe nothing
#'
#' Accepting and ignoring them would let a user replay an ordinary sample
#' while believing a transformation was re-applied, and get an untransformed
#' result that looks like the one they asked for.
#' @noRd
check_replay_link_args <- function(links, targets, subject,
                                   call = caller_env()) {
  given <- c("links", "targets")[c(!is_null(links), !is_null(targets))]
  if (length(given) == 0) {
    return(invisible(NULL))
  }
  # Each bullet needs its own `qty()` context.
  abort_samplyr(
    c(
      "{cli::qty(length(given))}{.arg {given}} {?is/are} not used when
       replaying {subject}.",
      "i" = "{cli::qty(length(given))}{?It describes/They describe} a
             {.fn share_weights} transformation, and this object carries
             none."
    ),
    class = "samplyr_error_replay_argument",
    call = call
  )
}

#' Replay a shared-weight sample from its source and its recorded call
#'
#' Two steps and two checkpoints. The source selection is re-executed against
#' `frame` and checked against the integrity the transformation recorded for
#' it, so a register that has changed is reported as that. Then the
#' transformation is re-applied to the supplied links and targets and the
#' result is checked against its own recorded integrity, which is what catches
#' a link table or a target register that is not the one used originally.
#'
#' It goes back through `share_weights()` rather than replaying the stored
#' operator. The operator addresses rows by position, so replaying it would
#' assume the supplied tables match a layout nothing has checked; re-running
#' the verb holds the inputs to every rule the first call was held to.
#' @noRd
replay_shared_sample <- function(x, frame, fingerprint, links, targets,
                                 call = caller_env()) {
  spec <- if (is_shared_sample_design(x)) {
    attr(x, "transformation")
  } else {
    record <- attr(x, "metadata")$weight_share
    check_weight_share_record_writable(record, "replay_design", call = call)
    c(record$call[c("by", "to", "within", "multiplicity", "target_scope")],
      record[c("source_integrity", "result_integrity")])
  }
  if (is_null(links) || is_null(targets)) {
    abort_samplyr(
      c(
        "{.arg links} and {.arg targets} must both be given to replay a
         shared-weight sample.",
        "i" = "The file records the selection and the transformation's
               arguments. The link table and the target register are supplied
               here, the way {.arg frame} is, so that neither is written into
               it.",
        "i" = "Supply the same tables the transformation was built from. The
               result is checked against the integrity the file records."
      ),
      class = "samplyr_error_replay_argument",
      call = call
    )
  }

  source_design <- if (is_shared_sample_design(x)) {
    # Strip the collection class before ordinary replay.
    structure(x, class = setdiff(class(x), "shared_sample_design"))
  } else {
    attr(x, "metadata")$weight_share$source_sample
  }
  source_sample <- replay_design(source_design, frame, fingerprint = fingerprint)
  check_replayed_integrity(
    verify_sample_integrity(source_sample, spec$source_integrity),
    spec$source_integrity, "source", call = call
  )

  result <- rlang::inject(share_weights(
    source_sample,
    targets = targets,
    links = links,
    by = spec$by,
    to = spec$to,
    within = !!decode_within_marker(spec$within),
    multiplicity = !!decode_multiplicity_marker(spec$multiplicity),
    target_scope = spec$target_scope
  ))
  check_replayed_integrity(
    verify_sample_integrity(result, spec$result_integrity),
    spec$result_integrity, "result", call = call
  )
  result
}

#' The `within` and `multiplicity` markers, rebuilt as expressions
#'
#' `share_weights()` takes these as a bare column and as a marker call, so
#' they go back as the expressions they were written as rather than as the
#' strings they are stored as.
#' @noRd
decode_within_marker <- function(within) {
  switch(
    within$mode,
    singleton = NULL,
    cluster = rlang::sym(within$col),
    extended = rlang::call2("extend_links", rlang::sym(within$col)),
    cli_abort("Unknown {.arg within} mode {.val {within$mode}}")
  )
}

#' @noRd
decode_multiplicity_marker <- function(multiplicity) {
  switch(
    multiplicity$mode,
    complete_links = rlang::call2("complete_links"),
    weighted_links = rlang::call2(
      "weighted_links",
      rlang::sym(multiplicity$col),
      total = rlang::sym(multiplicity$total_col)
    ),
    # Decode the legacy stored mode to `complete_links()`.
    complete_weighted_links = rlang::call2(
      "weighted_links",
      rlang::sym(multiplicity$col),
      total = rlang::call2("complete_links")
    ),
    cli_abort("Unknown {.arg multiplicity} mode {.val {multiplicity$mode}}")
  )
}

#' Report a replay that did not reproduce what the file recorded
#'
#' Two call sites with different causes, so the message names which stage
#' disagreed. A source mismatch means the register is not the one selected
#' from; a result mismatch means the links or the targets are not the ones
#' the transformation was built from.
#' @noRd
check_replayed_integrity <- function(verdict, recorded, stage,
                                     call = caller_env()) {
  if (is_null(recorded) || identical(verdict, "ok")) {
    return(invisible(NULL))
  }
  bullets <- if (identical(stage, "source")) {
    c(
      "x" = "The replayed selection is not the one the transformation was
             built from.",
      "i" = "{.arg frame} is not the register the source sample was drawn
             from, or it has changed since."
    )
  } else {
    c(
      "x" = "The transformation replayed, but not to the sample the file
             records.",
      "i" = "{.arg links} or {.arg targets} is not the table the
             transformation was built from. The selection itself matched."
    )
  }
  abort_samplyr(
    c("Replaying this shared-weight sample did not reproduce it.", bullets),
    class = "samplyr_error_replay_weight_share_mismatch",
    call = call
  )
}

#' @noRd
is_shared_weight_sample <- function(x) {
  is_tbl_sample(x) && identical(sample_weight_contract(x), "shared")
}

#' Encode a shared-weight sample as its source and its transformation
#'
#' The file records the selection the links start from, and the arguments
#' `share_weights()` was given. It does not record the links, the target
#' register, or the transformed rows: those are supplied again at replay, the
#' way a frame is, so nothing unit-level and no linkage is written.
#'
#' Two integrity records travel with it and are what replay is checked
#' against. They cost three fields each and need no data the record does not
#' already hold, which is why the file needs no fingerprint of the link table:
#' a wrong `links` or `targets` shows up as a result that does not match.
#' @noRd
shared_sample_payload <- function(
  x,
  frame = NULL,
  frame_label = NULL,
  fn_name,
  call = caller_env()
) {
  record <- attr(x, "metadata")$weight_share
  check_weight_share_record_writable(record, fn_name, call = call)

  list(
    format = shared_sample_format_id,
    format_version = shared_sample_format_version,
    transformation = encode_weight_share_call(record),
    # Encode the source as an ordinary sample.
    source = design_payload(
      record$source_sample,
      frame = frame,
      frame_label = frame_label,
      fn_name = fn_name,
      call = call
    )
  )
}

#' Refuse a transformation this build cannot write back out
#'
#' Same discipline as the panel assignment record. A record naming an
#' algorithm or a schema version this samplyr does not know is refused rather
#' than written under the current one, because the file would then claim a
#' transformation nobody performed.
#' @noRd
check_weight_share_record_writable <- function(record, fn_name,
                                               call = caller_env()) {
  if (
    !identical(record$algorithm, weight_share_record_algorithm) ||
      !identical(record$version, 1L)
  ) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} cannot write this transformation record.",
        "x" = "It states algorithm {.val {record$algorithm}} at version
               {.val {record$version}}, and this samplyr writes
               {.val {weight_share_record_algorithm}} at version {.val {1L}}.",
        "i" = "{.fn saveRDS} preserves the object whole."
      ),
      class = "samplyr_error_serialize_unsupported",
      call = call
    )
  }
  invisible(NULL)
}

#' @noRd
encode_weight_share_call <- function(record) {
  spec <- record$call
  list(
    algorithm = record$algorithm,
    version = record$version,
    # JSON objects preserve named join pairs.
    by = as.list(spec$by),
    to = as.list(spec$to),
    within = list(mode = spec$within$mode, col = spec$within$col),
    multiplicity = list(
      mode = spec$multiplicity$mode,
      scale = spec$multiplicity$scale,
      col = spec$multiplicity$col,
      total_col = spec$multiplicity$total_col
    ),
    target_scope = spec$target_scope,
    source_integrity = record$source_integrity,
    result_integrity = record$result_integrity
  )
}

#' Replay every component of a collection and stack the results
#'
#' Takes both kinds: a collection read back from a file, and a live one, for
#' the same reason `replay_design()` takes both a restored design and an
#' executed sample.
#'
#' The collection is rebuilt through `stack_frames()` rather than by restoring
#' the attributes directly, so a replay is held to every structural rule a
#' first call was: a register that no longer carries the membership column, or
#' whose key has stopped being unique, is reported as that rather than
#' returned as a collection nothing else will accept.
#' @noRd
replay_frame_stack <- function(x, frame, fingerprint, call = caller_env()) {
  # Validate collection inputs before replaying components.
  check_overlap_spec_portable(attr(x, "overlaps"), "replay_design", call = call)
  names_x <- names(x)
  frames <- frame_stack_component_frames(
    frame, names_x, "replay_design", required = TRUE, call = call
  )
  # Gate every component before any selection runs.
  if (is_frame_stack(x)) {
    for (nm in names_x) {
      check_weight_contract_serialize(x[[nm]], "replay_design", call = call)
    }
  }
  components <- lapply(names_x, function(nm) {
    replay_design(x[[nm]], frames[[nm]], fingerprint = fingerprint)
  })
  names(components) <- names_x

  # Restore a stored bare key as a symbol.
  rlang::inject(stack_frames(
    !!!components,
    membership = attr(x, "membership"),
    key = !!rlang::sym(attr(x, "key")),
    overlaps = attr(x, "overlaps")
  ))
}

#' Encode a frame collection as one document
#'
#' The envelope carries what makes the components a collection: the shared
#' `key`, and per component its name and membership column. Everything else is
#' the component's own design document, built by the same encoder a lone
#' sample goes through, so the two never drift.
#'
#' `frame` is a list keyed by component name. Per component rather than one
#' frame for the collection, because the components are separate selections
#' and generally have separate registers. Frames are fingerprinted, never
#' written, exactly as for a lone design.
#' @noRd
frame_stack_payload <- function(
  x,
  frame = NULL,
  frame_label = NULL,
  fn_name,
  call = caller_env()
) {
  # Gate the collection before encoding components.
  check_overlap_spec_portable(attr(x, "overlaps"), fn_name, call = call)
  names_x <- names(x)
  membership <- attr(x, "membership")
  frames <- frame_stack_component_frames(frame, names_x, fn_name, call = call)

  components <- lapply(seq_along(x), function(i) {
    nm <- names_x[[i]]
    # `design_payload()` applies the component weight gate.
    # Use the component name as its frame label.
    component <- design_payload(
      x[[i]],
      frame = frames[[nm]],
      frame_label = if (is_null(frames[[nm]])) NULL else nm,
      fn_name = fn_name,
      call = call
    )
    # Put component identity before its design payload.
    c(list(name = nm, membership = unname(membership[[nm]])), component)
  })

  payload <- list(
    format = frame_stack_format_id,
    format_version = frame_stack_format_version,
    key = attr(x, "key"),
    components = components
  )
  payload$overlaps <- encode_overlap_spec(attr(x, "overlaps"))
  payload
}

#' Encode a declared overlap specification, and refuse a resolved one
#'
#' A declared specification names one column per frame and states the scale it
#' is on. That is the whole thing, and it is what the estimator reads.
#'
#' A specification built from `exante_overlaps()` is resolved when the
#' collection is formed, and what it leaves on the collection is one matrix
#' per frame holding a chance per selected unit. That is microdata, which this
#' format never writes.
#'
#' **The test is `cols`, not the class.** A resolved record is a bare list
#' carrying no class at all, so `is_exante_overlap_spec()` is FALSE for it and
#' a class test falls through to the declared branch and writes `cols: null`,
#' losing the matrices without a word. `print.frame_stack()` already
#' discriminates the two the same way. Dropping the specification is what the
#' refusal prevents: the collection would read back looking complete and
#' export under a different estimator, which is a different total rather than
#' an error.
#' @noRd
encode_overlap_spec <- function(overlaps) {
  if (is_null(overlaps)) {
    return(NULL)
  }
  list(
    scale = overlaps$scale,
    # Names preserve frame mapping without object order.
    cols = as.list(overlaps$cols)
  )
}

#' @noRd
check_overlap_spec_portable <- function(overlaps, fn_name,
                                        call = caller_env()) {
  if (is_null(overlaps)) {
    return(invisible(NULL))
  }
  if (is_resolved_overlaps(overlaps)) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} is not defined for a collection whose overlaps come
         from {.fn exante_overlaps}.",
        "x" = "They are resolved when the collection is formed, into one
               chance per selected unit. That is unit-level data, which this
               format never writes, and the request that produced it is not
               kept, so it cannot be re-run either.",
        "i" = "Rebuild the collection without them, and pass
               {.fn exante_overlaps} to {.fn stack_frames} again afterwards.
               The registers it resolves against are the ones the components
               are replayed from.",
        "i" = "Overlaps from {.fn declared_overlaps} name columns of the
               components, and
               travel with the collection."
      ),
      class = "samplyr_error_serialize_unsupported",
      call = call
    )
  }
  invisible(NULL)
}

#' The frame supplied for each component, checked against the collection
#'
#' A collection's components are separate selections, so one data frame for
#' all of them describes a call that never happened. Names rather than
#' position: a list in the wrong order would fingerprint each component
#' against another's register and report a mismatch naming the wrong frame.
#' @noRd
frame_stack_component_frames <- function(frame, names_x, fn_name,
                                         required = FALSE,
                                         call = caller_env()) {
  if (is_null(frame)) {
    # Frames are optional for saving and required for replay.
    if (!required) {
      return(setNames(vector("list", length(names_x)), names_x))
    }
  }
  if (!is.list(frame) || is.data.frame(frame)) {
    abort_samplyr(
      c(
        "{.arg frame} must be a named list with one entry per component.",
        "x" = "A {.cls frame_stack} has one register per component, so a
               single frame cannot describe it.",
        "i" = "Write it as {.code frame = list({paste(names_x,
               collapse = ' = , ')} = )}."
      ),
      class = "samplyr_error_serialize_frame_stack_frame",
      call = call
    )
  }
  supplied <- names(frame) %||% rep("", length(frame))
  missing <- setdiff(names_x, supplied)
  extra <- setdiff(supplied, names_x)
  if (length(missing) > 0 || length(extra) > 0) {
    abort_samplyr(
      c(
        "{.arg frame} must name every component of the collection, and only
         those.",
        if (length(missing) > 0) {
          c("x" = "No frame for {.val {missing}}.")
        },
        if (length(extra) > 0) {
          c("x" = "{.val {extra}} {?is/are} not {?a component/components} of
                   this collection.")
        }
      ),
      class = "samplyr_error_serialize_frame_stack_frame",
      call = call
    )
  }
  frame[names_x]
}

#' @param fn_name The public verb this was reached through. Required rather
#'   than defaulted, so a new entry point has to say what it is: the refusals
#'   below name it, and a message naming a verb the user did not call is
#'   worse than the missing-argument error.
#' @noRd
design_payload <- function(
  x,
  frame = NULL,
  frame_label = NULL,
  fn_name,
  call = caller_env()
) {
  execution <- NULL
  execution_environment <- NULL
  if (is_tbl_sample(x)) {
    # Refuse transformed samples before receipt encoding.
    check_weight_contract_serialize(x, fn_name, call = call)
    execution <- encode_execution(x, call = call)
    execution_environment <- attr(x, "metadata")$execution_environment
    if (is_null(execution$seed)) {
      cli_warn(c(
        "{.arg x} was executed without a seed.",
        "i" = "The execution receipt cannot reproduce the sample. Re-run
               {.fn execute} with {.arg seed} for a reproducible receipt."
      ))
    }
    if (isTRUE(execution$chained)) {
      cli_warn(c(
        "{.arg x} was produced by more than one {.fn execute} call
         (stage continuation or multi-phase).",
        "i" = "The receipt records only the final call, so
               {.fn replay_design} cannot reproduce this sample.
               Save and replay each phase or stage batch separately."
      ))
    }
    if (isTRUE(execution$modified)) {
      cli_warn(c(
        "{.arg x} was modified after execution (rows or design columns
         changed).",
        "i" = "The receipt describes the original execution;
               {.fn replay_design} reproduces the full original sample,
               not this object."
      ))
    }
    design <- get_design(x)
    if (is_null(design)) {
      cli_abort("{.arg x} does not carry a stored design", call = call)
    }
  } else if (is_sampling_design(x)) {
    design <- x
    execution <- attr(x, "execution")
    execution_environment <- attr(
      x,
      "design_tools"
    )$samplyr$execution$environment
  } else {
    cli_abort(
      "{.arg x} must be a {.cls sampling_design} or a {.cls tbl_sample}",
      call = call
    )
  }
  validate_sampling_design(design, call = call)
  check_controls_serializable(design, call = call)

  # Refuse frame counts inconsistent with the design or receipt.
  if (!is_null(frame)) {
    supplied <- normalize_frame_input(frame, call = call)
    check_serialization_frame_count(
      x, design, supplied$n_supplied, call = call
    )
  }
  if (
    is_tbl_sample(x) && is_null(frame) &&
      is_null(recorded_fingerprints(attr(design, "portable_frame_info")))
  ) {
    cli_warn(c(
      "{.arg x} is being saved without a frame fingerprint.",
      "i" = "The receipt can be replayed, but {.fn replay_design} cannot
             verify that the supplied frame is the one originally sampled.",
      "i" = "Supply {.arg frame} to {.fn write_design} for verifiable
             replay."
    ))
  }

  payload <- list(
    format = design_format_id,
    format_version = design_format_version,
    schema = list(
      method_vocabulary = list(
        id = method_vocabulary_id,
        version = method_vocabulary_version
      )
    ),
    design = encode_design(design)
  )
  payload$frame <- encode_frame_info(design, frame, frame_label)
  payload$execution <- execution
  payload$format_version <- required_format_version(payload)
  payload$tools <- attr(design, "design_tools") %||% list()
  payload$tools$samplyr <- encode_samplyr_metadata(
    design,
    frame,
    frame_label,
    execution_environment
  )
  payload
}

#' The lowest format version that cannot be misread
#'
#' Additive fields alone do not justify a bump: an older reader ignores them
#' and loses only detail. These do, because an older reader would misread the
#' file rather than lose detail. A multi-register file (version 2) would be
#' taken for a one-frame file and replayed against a single frame. A
#' certainty-plan file (version 3) would drop the plan's classification and
#' field the stored per-stratum totals as an ordinary PPS stage, capping by
#' threshold instead of forcing the plan's certainty set, and refuse the take
#' stage's absent size - a different design, silently at stage 1.
#' @noRd
required_format_version <- function(payload) {
  bridge <- any(vapply(
    payload$design$stages %||% list(),
    function(stage) !is_null(stage$draw$certainty_plan),
    logical(1)
  ))
  if (bridge) {
    3L
  } else if (
    !is_null(payload$frame[["fingerprints"]]) ||
      identical(payload$execution$frames$mode, "separate_frames")
  ) {
    2L
  } else {
    1L
  }
}

## Sampling method vocabulary

# Common identifiers retain distinctions missing from the broader DDI terms.

#' @noRd
sampling_method_dictionary <- function() {
  probability <- list(
    code = "Probability",
    uri = paste0(
      "http://rdf-vocabulary.ddialliance.org/cv/",
      "SamplingProcedure/1.1.4/0d2765b"
    )
  )
  simple_random <- list(
    code = "Probability.SimpleRandom",
    uri = paste0(
      "http://rdf-vocabulary.ddialliance.org/cv/",
      "SamplingProcedure/1.1.4/38e8e88"
    )
  )
  systematic_random <- list(
    code = "Probability.SystematicRandom",
    uri = paste0(
      "http://rdf-vocabulary.ddialliance.org/cv/",
      "SamplingProcedure/1.1.4/f189f62"
    )
  )
  entry <- function(
    id,
    family,
    algorithm,
    replacement,
    sample_size,
    probabilities,
    ddi = probability
  ) {
    list(
      id = id,
      family = family,
      algorithm = algorithm,
      replacement = replacement,
      sample_size = sample_size,
      probabilities = probabilities,
      ddi = ddi
    )
  }

  list(
    srswor = entry(
      "simple_random_without_replacement", "equal_probability", "simple_random",
      "without_replacement", "fixed", "equal", simple_random
    ),
    srswr = entry(
      "simple_random_with_replacement", "equal_probability", "simple_random",
      "with_replacement", "fixed", "equal"
    ),
    systematic = entry(
      "systematic_equal_probability", "equal_probability", "systematic",
      "without_replacement", "fixed", "equal", systematic_random
    ),
    bernoulli = entry(
      "bernoulli", "equal_probability", "bernoulli",
      "without_replacement", "random", "equal"
    ),
    pps_systematic = entry(
      "systematic_probability_proportional_to_size",
      "probability_proportional_to_size", "systematic",
      "without_replacement", "fixed", "unequal"
    ),
    pps_brewer = entry(
      "generalized_brewer_probability_proportional_to_size",
      "probability_proportional_to_size", "generalized_brewer",
      "without_replacement", "fixed", "unequal"
    ),
    pps_cps = entry(
      "conditional_poisson", "probability_proportional_to_size",
      "conditional_poisson",
      "without_replacement", "fixed", "unequal"
    ),
    pps_sampford = entry(
      "sampford", "probability_proportional_to_size", "sampford",
      "without_replacement", "fixed", "unequal"
    ),
    pps_poisson = entry(
      "poisson_probability_proportional_to_size",
      "probability_proportional_to_size", "poisson",
      "without_replacement", "random", "unequal"
    ),
    pps_sps = entry(
      "sequential_poisson", "probability_proportional_to_size",
      "sequential_poisson",
      "without_replacement", "fixed", "unequal"
    ),
    pps_pareto = entry(
      "pareto", "probability_proportional_to_size", "pareto",
      "without_replacement", "fixed", "unequal"
    ),
    pps_multinomial = entry(
      "multinomial_probability_proportional_to_size",
      "probability_proportional_to_size", "multinomial",
      "with_replacement", "fixed", "unequal"
    ),
    pps_chromy = entry(
      "chromy_minimum_replacement", "probability_proportional_to_size",
      "chromy",
      "minimum_replacement", "fixed", "unequal"
    ),
    cube = entry(
      "cube_balanced", "balanced", "cube",
      "without_replacement", "fixed", "equal_or_unequal"
    ),
    lpm2 = entry(
      "local_pivotal", "spatially_balanced", "local_pivotal",
      "without_replacement", "fixed", "equal_or_unequal"
    ),
    scps = entry(
      "spatially_correlated_poisson", "spatially_balanced",
      "spatially_correlated_poisson",
      "without_replacement", "fixed", "equal_or_unequal"
    )
  )
}

#' @noRd
encode_method <- function(spec) {
  entry <- sampling_method_dictionary()[[spec$method]]
  if (is_null(entry)) {
    entry <- list(
      id = "tool_specific",
      family = switch(
        spec$method_type %||% "",
        balanced = "balanced",
        wr = "probability_proportional_to_size",
        wor = "probability_proportional_to_size",
        "tool_specific"
      ),
      algorithm = "tool_specific",
      replacement = switch(
        spec$method_type %||% "",
        wr = "with_replacement",
        balanced = "without_replacement",
        wor = "without_replacement",
        "tool_specific"
      ),
      sample_size = if (is_null(spec$method_fixed)) {
        "tool_specific"
      } else if (isTRUE(spec$method_fixed)) {
        "fixed"
      } else {
        "random"
      },
      probabilities = "tool_specific",
      ddi = list(
        code = "Probability",
        uri = paste0(
          "http://rdf-vocabulary.ddialliance.org/cv/",
          "SamplingProcedure/1.1.4/0d2765b"
        )
      )
    )
  }
  list(
    id = entry$id,
    family = entry$family,
    algorithm = entry$algorithm,
    replacement = entry$replacement,
    sample_size = entry$sample_size,
    probabilities = entry$probabilities,
    standards = list(list(
      vocabulary = "DDI SamplingProcedure",
      version = "1.1.4",
      code = entry$ddi$code,
      uri = entry$ddi$uri
    ))
  )
}

#' @noRd
encode_design <- function(design) {
  out <- list()
  if (!is_null(design$title)) {
    out$title <- design$title
  }
  out$stages <- lapply(design$stages, encode_stage)
  out
}

#' @noRd
encode_stage <- function(stage) {
  out <- list()
  if (!is_null(stage$label)) {
    out$label <- stage$label
  }

  if (!is_null(stage$strata)) {
    strata <- list(vars = I(stage$strata$vars))
    strata$alloc <- stage$strata$alloc
    strata$variance <- encode_value(stage$strata$variance)
    strata$cost <- encode_value(stage$strata$cost)
    strata$cv <- encode_value(stage$strata$cv)
    strata$importance <- encode_value(stage$strata$importance)
    strata$power <- stage$strata$power
    out$strata <- strata
  }

  if (!is_null(stage$clusters)) {
    out$clusters <- list(vars = I(stage$clusters$vars))
  }

  if (!is_null(stage$draw_spec)) {
    spec <- stage$draw_spec
    draw <- list()
    draw$n <- encode_value(spec$n)
    draw$frac <- encode_value(spec$frac)
    draw$method <- encode_method(spec)
    draw$mos <- spec$mos
    draw$prn <- spec$prn
    if (!is_null(spec$aux)) {
      draw$aux <- I(spec$aux)
    }
    if (!is_null(spec$bounds)) {
      draw$bounds <- I(spec$bounds)
    }
    if (!is_null(spec$spread)) {
      draw$spread <- I(spec$spread)
    }
    draw$min_n <- encode_value(spec$min_n)
    draw$max_n <- encode_value(spec$max_n)
    draw$round <- spec$round
    if (!is_null(spec$control)) {
      draw$control <- encode_control(spec$control)
    }
    draw$certainty_size <- encode_value(spec$certainty_size)
    draw$certainty_prop <- encode_value(spec$certainty_prop)
    draw$certainty_overflow <- spec$certainty_overflow
    if (!is_null(spec$certainty_plan)) {
      draw$certainty_plan <- encode_certainty_plan(spec$certainty_plan)
    }
    draw$on_empty <- spec$on_empty
    out$draw <- draw
  }

  out
}

#' Encode a certainty-plan bridge spec (design format 3)
#'
#' Plain data throughout: the register as row objects, the per-stratum
#' vectors as JSON objects. `n_per_psu` may hold NA (a plan whose recorded
#' frame lacked the take), which the writer turns into null and the decoder
#' restores.
#' @noRd
encode_certainty_plan <- function(spec) {
  list(
    role = spec$role,
    register = spec$register,
    n_psu_draw = as.list(spec$n_psu_draw),
    n_per_psu = as.list(spec$n_per_psu),
    strata_var = spec$strata_var,
    id_var = spec$id_var,
    svyplan_version = spec$svyplan_version
  )
}

#' Encode a per-stratum value (scalar, named vector, or data frame)
#' @noRd
encode_value <- function(x) {
  if (is_null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    return(x)
  }
  if (!is.atomic(x)) {
    cli_abort("Cannot serialize a value of class {.cls {class(x)}}")
  }
  nms <- names(x)
  if (!is_null(nms)) {
    return(as.list(x))
  }
  if (length(x) == 1) {
    return(x)
  }
  I(x)
}

## Control expressions

#' @noRd
check_controls_serializable <- function(design, call = caller_env()) {
  for (stage in design$stages) {
    for (quo in stage$draw_spec$control %||% list()) {
      if (!control_expr_serializable(quo_get_expr(quo))) {
        cli_abort(
          c(
            "Cannot serialize the control expression
             {.code {as_label(quo)}}.",
            "i" = "Only bare column names, {.fn desc}, and {.fn serp} calls
                   on bare column names can be written to a design file.",
            "i" = "Namespace prefixes are not supported: write
                   {.code desc(pop)}, not {.code dplyr::desc(pop)}."
          ),
          call = call
        )
      }
    }
  }
  invisible(design)
}

#' @noRd
encode_control <- function(control_quos) {
  lapply(control_quos, function(quo) {
    expr <- quo_get_expr(quo)
    if (is.symbol(expr)) {
      return(list(
        type = "ascending",
        variables = I(as.character(expr))
      ))
    }

    operation <- as.character(expr[[1]])
    type <- switch(
      operation,
      desc = "descending",
      serp = "serpentine"
    )
    variables <- vapply(as.list(expr)[-1], as.character, character(1))
    list(type = type, variables = I(variables))
  })
}

#' @noRd
decode_control <- function(control) {
  if (is_null(control)) {
    return(NULL)
  }

  env <- control_eval_env()
  lapply(control, function(term) {
    expr <- decode_control_term(term)
    new_quosure(expr, env)
  })
}

#' Decode the declarative control grammar
#' @noRd
decode_control_term <- function(term, call = caller_env()) {
  if (!is.list(term) || is_null(names(term))) {
    abort_samplyr(
      "Design file contains a malformed control term.",
      class = "samplyr_error_design_file_malformed",
      call = call
    )
  }

  type <- term$type
  variables <- term$variables
  valid_variables <- is.list(variables) &&
    length(variables) > 0 &&
    all(vapply(
      variables,
      function(x) is.character(x) && length(x) == 1 && nzchar(x),
      logical(1)
    ))
  if (
    !is.character(type) || length(type) != 1 ||
      !type %in% c("ascending", "descending", "serpentine") ||
      !valid_variables
  ) {
    abort_samplyr(
      c(
        "Design file contains an invalid control term.",
        "i" = "Each term needs a supported {.field type} and a non-empty
               {.field variables} array."
      ),
      class = "samplyr_error_design_file_malformed",
      call = call
    )
  }
  if (type != "serpentine" && length(variables) != 1) {
    cli_abort(
      "Control type {.val {type}} requires exactly one variable.",
      call = call
    )
  }

  variables <- unlist(variables, use.names = FALSE)
  symbols <- lapply(variables, rlang::sym)
  switch(
    type,
    ascending = symbols[[1]],
    descending = rlang::call2("desc", symbols[[1]]),
    serpentine = rlang::call2("serp", !!!symbols)
  )
}

#' Allowlist for control expressions in design files
#'
#' A control expression can be serialized (and deserialized) only if it is
#' a bare column name or a desc()/serp() call on bare column names. This
#' keeps design files declarative: reading one can never run arbitrary
#' code. Namespaced calls are rejected too (ns = ""): is_call() would
#' otherwise match pkg::desc(), and evaluating that from a design file
#' loads an arbitrary installed package.
#' @noRd
control_expr_serializable <- function(expr) {
  if (is.symbol(expr)) {
    return(TRUE)
  }
  if (is_call(expr, c("desc", "serp"), ns = "")) {
    args <- as.list(expr)[-1]
    operation <- as.character(expr[[1]])
    valid_arity <- if (identical(operation, "desc")) {
      length(args) == 1
    } else {
      length(args) > 0
    }
    return(valid_arity && all(vapply(args, is.symbol, logical(1))))
  }
  FALSE
}

#' @noRd
control_eval_env <- function() {
  rlang::new_environment(
    list(desc = dplyr::desc, serp = serp),
    parent = baseenv()
  )
}

## Frame information

#' The label, or labels, of whatever shape `frame` was given in
#'
#' The expression is read before `frame` is looked at: forcing the argument
#' first leaves `enquo()` with the value rather than the call that produced it,
#' and every label would be lost.
#' @noRd
frame_label_for <- function(frame_quo, frame) {
  expr <- quo_get_expr(frame_quo)
  if (is_null(frame) || is.data.frame(frame)) {
    if (is_null(expr)) {
      return(NULL)
    }
    return(as_label(expr))
  }
  frame_arg_labels(expr, frame)
}

#' One diagnostic label per supplied frame
#'
#' Labels are presentation only, so they are taken wherever they read best:
#' the list's own names first, then the expression each element was written as,
#' which is usually the register's variable name.
#' @noRd
frame_arg_labels <- function(expr, frames) {
  n <- length(frames)
  labels <- names(frames) %||% rep("", n)
  if (
    is.call(expr) && is.symbol(expr[[1]]) &&
      identical(as.character(expr[[1]]), "list") && length(expr) == n + 1L
  ) {
    args <- as.list(expr)[-1]
    arg_names <- names(args) %||% rep("", n)
    for (i in seq_len(n)) {
      if (!nzchar(labels[i])) {
        labels[i] <- if (nzchar(arg_names[i])) {
          arg_names[i]
        } else {
          as_label(args[[i]])
        }
      }
    }
  }
  labels[!nzchar(labels)] <- NA_character_
  labels
}

#' A data frame is one frame. A list is the ordered stage frames
#' @noRd
as_frame_list <- function(frame, call = caller_env()) {
  normalize_frame_input(frame, call = call)$frames
}

#' @noRd
encode_frame_info <- function(design, frame, frame_label) {
  stored <- attr(design, "portable_frame_info")
  if (is_null(frame) && !is_null(stored)) {
    return(stored)
  }
  info <- list(required_variables = design_requirements(design))
  if (!is_null(frame)) {
    frames <- as_frame_list(frame)
    # One frame remains singular even inside `list(frame)`.
    if (length(frames) == 1L) {
      info$fingerprint <- portable_frame_fingerprint(frames[[1]])
    } else {
      info$fingerprints <- lapply(frames, portable_frame_fingerprint)
    }
  }
  info
}

#' Encode metadata that belongs to the samplyr/R implementation
#' @noRd
encode_samplyr_metadata <- function(
  design,
  frame,
  frame_label,
  execution_environment = NULL
) {
  out <- list(
    version = as.character(utils::packageVersion("samplyr")),
    language = list(
      name = "R",
      version = as.character(getRversion())
    ),
    dependencies = list(
      sondage = as.character(utils::packageVersion("sondage")),
      svyplan = as.character(utils::packageVersion("svyplan"))
    ),
    design = list(
      stages = lapply(design$stages, encode_samplyr_stage_metadata)
    )
  )
  if (!is_null(frame)) {
    # One supplied frame has one representation.
    frames <- as_frame_list(frame)
    out$frame <- if (length(frames) == 1L) {
      samplyr_frame_fingerprint(frames[[1]], frame_label[1])
    } else {
      list(frames = lapply(seq_along(frames), function(i) {
        samplyr_frame_fingerprint(frames[[i]], frame_label[i])
      }))
    }
  } else {
    out$frame <- attr(design, "design_tools")$samplyr$frame
  }
  if (!is_null(execution_environment)) {
    out$execution <- list(environment = execution_environment)
  }
  out
}

#' @noRd
encode_samplyr_stage_metadata <- function(stage) {
  spec <- stage$draw_spec
  if (is_null(spec)) {
    return(list())
  }
  list(method = list(
    name = spec$method,
    registry_type = spec$method_type,
    fixed_size = spec$method_fixed,
    variance_family = spec$method_variance,
    probabilities = spec$method_probabilities,
    implementation = spec$method_implementation
  ))
}

#' Derive the frame variables a design requires, stage by stage
#' @noRd
design_requirements <- function(design) {
  reqs <- list()
  add_req <- function(vars, role, stage) {
    lapply(vars, function(var) {
      list(name = var, role = role, stage = stage)
    })
  }

  for (i in seq_along(design$stages)) {
    stage <- design$stages[[i]]
    spec <- stage$draw_spec
    reqs <- c(
      reqs,
      add_req(stage$strata$vars, "strata", i),
      add_req(stage$clusters$vars, "clusters", i),
      add_req(spec$mos, "mos", i),
      add_req(spec$prn, "prn", i),
      add_req(spec$aux, "aux", i),
      add_req(spec$bounds, "bounds", i),
      add_req(spec$spread, "spread", i),
      add_req(extract_control_vars(spec$control), "control", i)
    )
  }
  reqs
}

#' Portable structural fingerprint. Content hashes and native column classes
#' live in the tool namespace because their canonicalization is implementation
#' dependent.
#' @noRd
portable_frame_fingerprint <- function(frame) {
  fingerprint <- list()
  fingerprint$row_count <- nrow(frame)
  fingerprint$column_count <- ncol(frame)
  fingerprint$columns <- lapply(names(frame), function(col) {
    list(name = col, type = portable_column_type(frame[[col]]))
  })
  fingerprint
}

#' @noRd
portable_column_type <- function(x) {
  if (inherits(x, "POSIXt")) return("date_time")
  if (inherits(x, "Date")) return("date")
  if (inherits(x, "difftime")) return("duration")
  if (is.ordered(x)) return("ordered_categorical")
  if (is.factor(x)) return("categorical")
  if (is.logical(x)) return("boolean")
  if (is.integer(x)) return("integer")
  if (is.numeric(x)) return("number")
  if (is.character(x)) return("string")
  if (is.raw(x)) return("binary")
  if (is.list(x)) return("composite")
  "tool_specific"
}

#' @noRd
samplyr_frame_fingerprint <- function(frame, frame_label) {
  out <- list()
  if (!is_null(frame_label) && !is.na(frame_label)) {
    out$source <- list(kind = "r_expression", value = frame_label)
  }
  out$columns <- lapply(names(frame), function(col) {
    list(name = col, class = I(class(frame[[col]])))
  })
  out$hash <- list(
    algorithm = "rlang::hash",
    value = frame_content_hash(frame)
  )
  out
}

#' Content hash of a frame for fingerprinting
#'
#' Hashes a plain named list of the frame's columns, sorted by name in
#' radix (locale-independent) order. Designs execute by column name, so
#' the data-frame wrapper (tibble vs data.frame class, row names,
#' grouping) and the column order must not contribute: frames differing
#' only in those respects are the same population. Row order does
#' contribute, because replaying a seed against reordered rows selects
#' different units.
#'
#' `columns` restricts the hash to a subset of columns. The frame
#' digest uses this for the role-scoped fingerprint, which must not be
#' invalidated by analysis columns added later.
#' @noRd
frame_content_hash <- function(frame, columns = NULL) {
  nms <- names(frame)
  if (!is_null(columns)) {
    nms <- intersect(nms, columns)
  }
  ord <- nms[order(nms, method = "radix")]
  cols <- lapply(ord, function(nm) frame[[nm]])
  names(cols) <- ord
  rlang::hash(cols)
}

## Execution receipts

#' @param call The public call the receipt is being written for, so a record
#'   this build cannot write out is reported against `write_design()`,
#'   `design_json()` or `replay_design()` rather than against an internal
#'   encoder.
#' @noRd
encode_execution <- function(sample, call = caller_env()) {
  meta <- attr(sample, "metadata") %||% list()
  receipt <- list()
  receipt$seed <- attr(sample, "seed")
  receipt$stages_executed <- I(as.integer(get_stages_executed(sample)))
  receipt$n_selected <- meta$n_selected %||% nrow(sample)
  if (!is_null(meta$executed_at)) {
    receipt$executed_at <- format(
      meta$executed_at,
      "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    )
  }
  receipt$reps <- meta$reps
  if (!is_null(meta$replicate_seeds)) {
    receipt$replicate_seeds <- I(as.integer(meta$replicate_seeds))
  }
  if (!is_null(meta$panels)) {
    receipt$panels <- as.integer(meta$panels)
  }
  if (!is_null(meta$panel_assignment)) {
    receipt$panel_assignment <- encode_panel_assignment(
      meta$panel_assignment,
      call = call
    )
  }
  receipt$frames <- encode_frame_schedule(
    frame_record_or_default(
      meta$frame_schedule, get_stages_executed(sample)
    )
  )
  if (!is_null(meta$wave)) {
    receipt$wave <- encode_wave(meta$wave)
  }
  # One receipt cannot replay a chain of execution calls.
  if (!is_null(meta$continued_from) || !is_null(meta$prev_phase)) {
    receipt$chained <- TRUE
  }
  # Do not encode a receipt invalidated by sample changes.
  if (!sample_realization_status(sample)$ok) {
    receipt$modified <- TRUE
  }
  # Do not encode an invalidated frame digest.
  digest <- get_frame_digest(sample)
  if (!is_null(digest) && !identical(digest$status, "invalidated")) {
    receipt$frame_digest <- digest
  }
  receipt
}

#' The frozen panel assignment
#'
#' Replay reproduces `.panel` from the seed, so the values themselves are not
#' written. What is written is what the values cannot be recomputed from: the
#' algorithm and its version, the assignment-unit identities in pool order,
#' the block sizes, and the realized block-by-panel quotas. Those are the
#' denominators a later activation of a subset of panels is computed against.
#'
#' The record is read here under the version it states, exactly as a reader
#' reads it. A writer that filled in a field the record does not carry would
#' produce a well-formed file describing an assignment nothing recorded: a
#' stage-less record used to be written out as stage 1, and then replayed as a
#' first-stage assignment of a sample that was not assigned at the first
#' stage.
#' @noRd
encode_panel_assignment <- function(record, call = caller_env()) {
  record <- prepare_panel_record(record, "An execution receipt", call = call)
  list(
    algorithm = record$algorithm,
    version = as.integer(record$version),
    panels = as.integer(record$panels),
    # Record the validated assignment stage.
    assignment_stage = record$assignment_stage,
    block_size = as.integer(record$block_size),
    r_min = as.integer(record$r_min),
    unit = record$unit,
    key_vars = I(as.character(record$key_vars)),
    # Pool columns include assignment strata and realized ancestors.
    pool_vars = I(as.character(record$pool_vars %||% character(0))),
    control_ordered = isTRUE(record$control_ordered),
    certainty = record$certainty,
    small_pool_policy = record$small_pool_policy %||% "error",
    # Store the schedule needed to reproduce panel blocks.
    schedule = if (!is_null(record$schedule)) {
      data.frame(
        wave = as.integer(record$schedule$wave),
        panel = as.integer(record$schedule$panel),
        active = as.logical(record$schedule$active)
      )
    }
    # Store the assignment law, not reproducible realized pools.
  )
}

#' The assignment stage a receipt recorded
#'
#' `NULL` for a first-stage assignment, which is what omitting the argument
#' replays. Anything else has to be passed back explicitly: replay re-executes
#' the design, and an execution given no `panel_stage` assigns from the first
#' stage, so a lower-stage master would otherwise replay as a different
#' assignment of the same sample rather than failing.
#'
#' @param record The prepared assignment record. Its stage is a whole number of
#'   1 or more under every readable version, so there is nothing to test here
#'   but whether it is the first: a version-1 or version-2 record means stage 1
#'   whatever it carries, and a version-3 record that does not state a stage was
#'   refused before this was called.
#' @noRd
decode_panel_stage_argument <- function(record) {
  stage <- record$assignment_stage
  if (is_null(stage) || stage <= 1L) {
    return(NULL)
  }
  stage
}

#' The small-pool policy a scheduled receipt recorded
#'
#' `NULL` unless the receipt both carries a schedule and names a policy. A
#' version-1 receipt names none because it applied none, so omitting the
#' argument replays it under today's default. That is a stricter rule than it
#' was drawn under, which is the intended direction: an assignment that would
#' now be refused should not be reproduced in silence.
#' @noRd
decode_small_pool_argument <- function(record, panels) {
  if (!is.data.frame(panels)) {
    return(NULL)
  }
  policy <- record$small_pool_policy
  if (is_null(policy) || identical(policy, "error")) {
    return(NULL)
  }
  as.character(policy)[1]
}

#' Rebuild the `panels` argument a receipt recorded
#'
#' Reads back the same shape the original call was given: a schedule when the
#' master declared one, otherwise the panel count. A receipt written before
#' schedules existed carries only the count, which is what it was executed
#' with.
#' @noRd
decode_panel_argument <- function(receipt, record) {
  if (is_null(receipt$panels)) {
    return(NULL)
  }
  schedule <- record$schedule
  if (is_null(schedule) || length(schedule) == 0) {
    return(as.integer(receipt$panels))
  }
  if (!is.data.frame(schedule)) {
    schedule <- do.call(rbind, lapply(schedule, as.data.frame))
  }
  data.frame(
    wave = as.integer(schedule$wave),
    panel = as.integer(schedule$panel),
    active = as.logical(schedule$active)
  )
}

#' The wave a materialized sample realizes
#'
#' The activation probabilities are recorded per block rather than per unit:
#' a unit's probability is its block's, and the block a unit belongs to is
#' already recoverable from the assignment record.
#' @noRd
encode_wave <- function(record) {
  # Wave pools preserve activation facts that replay cannot rebuild.
  list(
    wave = as.integer(record$wave),
    active_panels = I(as.integer(record$active_panels)),
    schedule_digest = record$schedule_digest,
    pools = lapply(record$pools, function(pool) {
      out <- list()
      if (!is_null(pool$stratum)) {
        out$stratum <- lapply(pool$stratum, function(v) as.character(v)[1])
      }
      out$class <- pool$class
      # Preserve why a pool is permanent.
      out$activation <- pool$activation
      if (!is.na(pool$permanent_reason %||% NA_character_)) {
        out$permanent_reason <- pool$permanent_reason
      }
      out$blocks <- I(as.integer(pool$blocks))
      out$take <- I(as.integer(pool$take))
      out$probability <- I(as.numeric(pool$probability))
      out
    })
  )
}

#' How the recorded call mapped frames to stages
#'
#' Written for every executed sample so a reader never has to infer the mapping,
#' though a receipt without it is read as the one-frame call it can only have
#' been.
#' @noRd
encode_frame_schedule <- function(record) {
  out <- list(
    mode = record$mode,
    count = as.integer(record$n_supplied),
    stages = I(as.integer(record$stages)),
    stage_frame_index = I(as.integer(record$stage_frame_index))
  )
  if (!is_null(record$labels)) {
    out$labels <- I(as.character(record$labels))
  }
  out
}

#' @noRd
decode_frame_schedule <- function(x) {
  if (is_null(x)) {
    return(NULL)
  }
  labels <- NULL
  if (!is_null(x$labels)) {
    labels <- vapply(
      x$labels,
      function(l) if (is_null(l)) NA_character_ else as.character(l)[1],
      character(1)
    )
  }
  list(
    mode = decode_chr(x$mode),
    n_supplied = as.integer(x$count),
    labels = labels,
    stages = as.integer(unlist(x$stages)),
    stage_frame_index = as.integer(unlist(x$stage_frame_index))
  )
}

#' Decode a frame digest read back from a design file
#'
#' Rebuilds the digest tables from the row-wise JSON representation,
#' checks the digest schema version, and validates the result. Schema
#' version errors propagate from the caller. Other malformed optional
#' digest content drops with a warning.
#' @noRd
decode_frame_digest <- function(x) {
  check_frame_digest_version(x$version)

  chr1 <- function(v) {
    if (is_null(v)) NULL else as.character(v)[1]
  }
  chrs <- function(v) {
    if (is_null(v)) NULL else as.character(unlist(v))
  }

  frames <- lapply(x$frames, function(f) {
    list(
      frame_id = as.integer(f$frame_id),
      fingerprint_exact = chr1(f$fingerprint_exact),
      fingerprint_roles = chr1(f$fingerprint_roles),
      n_rows = as.integer(f$n_rows),
      roles = decode_digest_table(
        f$roles, c(column = "chr", role = "chr", stage = "int")
      ),
      scope = chr1(f$scope)
    )
  })

  stages <- lapply(x$stages, function(s) {
    strata <- chrs(s$strata)
    pool_spec <- c(
      pool_id = "int", parent_unit = "int", parent_occurrence = "int",
      N = "int",
      n_target = "dbl", n_expected = "dbl", n_realized = "int",
      scope = "chr", chance_status = "chr", chance = "dbl",
      n_descendants = "int",
      setNames(rep("chr", length(strata)), strata)
    )
    diagnostics <- NULL
    if (!is_null(s$diagnostics)) {
      diagnostics <- list()
      diagnostics$balance <- decode_digest_table(
        s$diagnostics$balance,
        c(pool_id = "int", term = "chr", target = "dbl",
          realized = "dbl", residual = "dbl")
      )
      diagnostics$bounds <- decode_digest_table(
        s$diagnostics$bounds,
        c(pool_id = "int", term = "chr", level = "chr",
          expected = "dbl", lower = "dbl", upper = "dbl",
          realized = "dbl", satisfied = "lgl")
      )
      if (!is_null(s$diagnostics$spatial)) {
        sp <- s$diagnostics$spatial
        diagnostics$spatial <- list(
          variables = chrs(sp$variables),
          dimensions = as.integer(sp$dimensions),
          ranges = decode_digest_table(
            sp$ranges, c(variable = "chr", min = "dbl", max = "dbl")
          ),
          n_duplicate_coordinates = as.integer(
            sp$n_duplicate_coordinates
          )
        )
      }
      diagnostics <- Filter(Negate(is_null), diagnostics)
      if (length(diagnostics) == 0) diagnostics <- NULL
    }
    new_digest_stage(
      stage_id = as.integer(s$stage_id),
      frame_ref = as.integer(s$frame_ref),
      unit_level = chr1(s$unit_level),
      scope = chr1(s$scope),
      chance_kind = chr1(s$chance_kind),
      probabilities = chr1(s$probabilities),
      order_kind = chr1(s$order_kind),
      storage = chr1(s$storage),
      pools = decode_digest_table(s$pools, pool_spec),
      units = decode_digest_table(
        s$units,
        c(unit_id = "int", pool_id = "int", unit_order = "int",
          chance = "dbl", is_certainty = "lgl", n_descendants = "int")
      ),
      chance_distribution = decode_digest_table(
        s$chance_distribution,
        c(pool_id = "int", quantile = "dbl", chance = "dbl",
          n_units = "int")
      ),
      selected = decode_digest_table(
        s$selected,
        c(pool_id = "int", unit_id = "int", occurrence = "int",
          replicate = "int", key = "chr", sample_row = "int")
      ),
      strata = strata,
      diagnostics = diagnostics
    )
  })

  digest <- new_frame_digest(
    frames = frames,
    stages = stages,
    privacy = digest_privacy(
      mode = chr1(x$privacy$mode) %||% "summary",
      stable_keys = isTRUE(x$privacy$stable_keys),
      labels_retained = isTRUE(x$privacy$labels_retained)
    ),
    status = chr1(x$status) %||% "complete"
  )
  validate_frame_digest(digest)
  digest
}

#' Rebuild one digest table from row-wise JSON
#'
#' `spec` names the known columns and their types ("int", "dbl",
#' "chr", "lgl"). Columns absent from every row stay absent. JSON
#' nulls become typed NA.
#' @noRd
decode_digest_table <- function(rows, spec) {
  if (is_null(rows) || length(rows) == 0) {
    return(NULL)
  }
  present <- unique(unlist(lapply(rows, names)))
  cols <- intersect(names(spec), present)
  out <- lapply(cols, function(col) {
    values <- lapply(rows, function(row) row[[col]])
    switch(
      spec[[col]],
      int = vapply(values, function(v) {
        if (is_null(v)) NA_integer_ else as.integer(v)
      }, integer(1)),
      dbl = vapply(values, function(v) {
        if (is_null(v)) NA_real_ else as.double(v)
      }, numeric(1)),
      chr = vapply(values, function(v) {
        if (is_null(v)) NA_character_ else as.character(v)
      }, character(1)),
      lgl = vapply(values, function(v) {
        if (is_null(v)) NA else as.logical(v)
      }, logical(1))
    )
  })
  names(out) <- cols
  as.data.frame(out, check.names = FALSE)
}

## Decoding

#' Rebuild a shared-weight sample's source design and transformation
#'
#' Returns the source design with its receipt, plus the transformation to
#' re-apply. `replay_design()` executes the first against the register and the
#' second against the links and targets it is given.
#' @noRd
decode_shared_sample_payload <- function(payload, call = caller_env()) {
  version <- payload$format_version
  if (
    !is.numeric(version) || length(version) != 1 || is.na(version) ||
      version < 1 || version != floor(version) ||
      version > shared_sample_format_version
  ) {
    cli_abort(
      c(
        "Shared-weight sample file format version {.val {version}} is not
         supported.",
        "i" = "This version of samplyr reads format versions up to
               {.val {shared_sample_format_version}}. Update samplyr to read
               this file."
      ),
      call = call
    )
  }
  spec <- decode_weight_share_call(payload$transformation, call = call)
  new_shared_sample_design(
    decode_design_payload(payload$source, call = call),
    transformation = spec
  )
}

#' @noRd
decode_weight_share_call <- function(transformation, call = caller_env()) {
  algorithm <- decode_chr(transformation$algorithm)
  version <- transformation$version
  # Check transformation type before reading its fields.
  if (
    !identical(algorithm, weight_share_record_algorithm) ||
      !is.numeric(version) || length(version) != 1 || is.na(version) ||
      version != 1
  ) {
    abort_samplyr(
      c(
        "This file records a transformation this samplyr cannot replay.",
        "x" = "It states algorithm {.val {algorithm}} at version
               {.val {version}}.",
        "i" = "Replaying it under the rules of a different algorithm would
               reproduce a sample nobody drew."
      ),
      class = "samplyr_error_weight_share_record_unsupported",
      call = call
    )
  }

  chr_map <- function(x) {
    if (is_null(x) || length(x) == 0) {
      return(NULL)
    }
    out <- vapply(x, function(v) decode_chr(v) %||% NA_character_, character(1))
    names(out) <- names(x)
    out
  }
  spec <- list(
    by = chr_map(transformation$by),
    to = chr_map(transformation$to),
    within = list(
      mode = decode_chr(transformation$within$mode),
      col = decode_chr(transformation$within$col)
    ),
    multiplicity = list(
      mode = decode_chr(transformation$multiplicity$mode),
      scale = decode_chr(transformation$multiplicity$scale),
      col = decode_chr(transformation$multiplicity$col),
      total_col = decode_chr(transformation$multiplicity$total_col)
    ),
    target_scope = decode_chr(transformation$target_scope),
    source_integrity = decode_integrity(transformation$source_integrity),
    result_integrity = decode_integrity(transformation$result_integrity)
  )
  if (
    is_null(spec$by) || is_null(spec$to) || anyNA(spec$by) || anyNA(spec$to) ||
      is_null(names(spec$by)) || is_null(names(spec$to)) ||
      !is_scalar_string(spec$within$mode) ||
      !is_scalar_string(spec$multiplicity$mode) ||
      !is_scalar_string(spec$target_scope)
  ) {
    cli_abort(
      c(
        "Shared-weight sample file has an unreadable transformation.",
        "i" = "It records the join maps, how links are grouped, how the
               denominator is formed, and the scope. None can be inferred
               from the others."
      ),
      call = call
    )
  }
  spec
}

#' @noRd
decode_integrity <- function(x) {
  if (is_null(x)) {
    return(NULL)
  }
  list(
    n_rows = as.integer(x$n_rows),
    cols = vapply(x$cols, as.character, character(1)),
    hash = decode_chr(x$hash)
  )
}

#' Rebuild a frame collection's designs from its document
#'
#' Returns the collection's counterpart to what a design file returns: the
#' designs and receipts that rebuild it, not the collection itself, which
#' needs the registers. `replay_design()` turns one into the other.
#' @noRd
decode_frame_stack_payload <- function(payload, call = caller_env()) {
  version <- payload$format_version
  if (
    !is.numeric(version) || length(version) != 1 || is.na(version) ||
      version < 1 || version != floor(version) ||
      version > frame_stack_format_version
  ) {
    cli_abort(
      c(
        "Frame collection file format version {.val {version}} is not
         supported.",
        "i" = "This version of samplyr reads format versions up to
               {.val {frame_stack_format_version}}. Update samplyr to read
               this file."
      ),
      call = call
    )
  }
  components <- payload$components
  if (!is.list(components) || length(components) == 0) {
    cli_abort(
      "Frame collection file has no {.field components} entry",
      call = call
    )
  }
  key <- decode_chr(payload$key)
  if (!is_scalar_string(key)) {
    cli_abort(
      "Frame collection file has no {.field key} entry",
      call = call
    )
  }

  names_x <- vapply(components, function(component) {
    decode_chr(component$name) %||% NA_character_
  }, character(1))
  membership <- vapply(components, function(component) {
    decode_chr(component$membership) %||% NA_character_
  }, character(1))
  if (anyNA(names_x) || anyNA(membership) || anyDuplicated(names_x) > 0) {
    cli_abort(
      c(
        "Frame collection file has a component without a usable
         {.field name} or {.field membership}.",
        "i" = "Each component names itself, and names the column saying which
               frames its units belong to. Neither can be inferred from the
               other components."
      ),
      call = call
    )
  }
  names(membership) <- names_x

  designs <- lapply(components, decode_design_payload, call = call)
  names(designs) <- names_x

  new_frame_stack_design(
    designs,
    membership = membership,
    key = key,
    overlaps = decode_overlap_spec(payload$overlaps, names_x, call = call)
  )
}

#' @noRd
decode_overlap_spec <- function(overlaps, frames, call = caller_env()) {
  if (is_null(overlaps)) {
    return(NULL)
  }
  cols <- vapply(overlaps$cols, function(col) {
    decode_chr(col) %||% NA_character_
  }, character(1))
  scale <- decode_chr(overlaps$scale)
  if (anyNA(cols) || !setequal(names(cols), frames) || is_null(scale)) {
    cli_abort(
      c(
        "Frame collection file has an unreadable {.field overlaps} entry.",
        "i" = "It names one column per frame and the scale those columns are
               on. A partial mapping would export under a different
               estimator."
      ),
      call = call
    )
  }
  # Validate decoded specifications through the constructor.
  new_overlap_spec(scale, as.list(cols[frames]), call = call)
}

#' @noRd
decode_design_payload <- function(payload, call = caller_env()) {
  if (!identical(payload$format, design_format_id)) {
    cli_abort(
      "This is not a samplyr design file
       (expected {.field format} = {.val {design_format_id}}).",
      call = call
    )
  }
  version <- payload$format_version
  if (
    !is.numeric(version) || length(version) != 1 || is.na(version) ||
      version < 1 || version != floor(version) ||
      version > design_format_version
  ) {
    abort_samplyr(
      c(
        "Design file format version {.val {version}} is not supported.",
        "i" = "This version of samplyr reads format versions up to
               {.val {design_format_version}}. Update samplyr to read
               this file."
      ),
      class = "samplyr_error_design_file_unsupported",
      call = call
    )
  }
  if (!is.list(payload$design) || !is.list(payload$design$stages)) {
    abort_samplyr(
      "Design file has no {.field design.stages} entry",
      class = "samplyr_error_design_file_malformed",
      call = call
    )
  }
  vocabulary <- payload$schema$method_vocabulary
  if (
    !identical(vocabulary$id, method_vocabulary_id) ||
      !is.numeric(vocabulary$version) || length(vocabulary$version) != 1 ||
      vocabulary$version > method_vocabulary_version
  ) {
    abort_samplyr(
      "Design file uses an unsupported sampling method vocabulary.",
      class = "samplyr_error_design_file_unsupported",
      call = call
    )
  }

  samplyr_tools <- payload$tools$samplyr
  tool_stages <- samplyr_tools$design$stages %||% list()
  stages <- lapply(seq_along(payload$design$stages), function(i) {
    tool_stage <- if (length(tool_stages) >= i) tool_stages[[i]] else NULL
    decode_stage(
      payload$design$stages[[i]],
      tool_stage = tool_stage
    )
  })
  design <- new_sampling_design(
    title = decode_chr(payload$design$title),
    stages = stages,
    current_stage = length(stages),
    validated = FALSE
  )
  design <- validate_sampling_design(design, call = call)

  attr(design, "frame_info") <- decode_frame_info(
    payload$frame,
    samplyr_tools$frame
  )
  attr(design, "portable_frame_info") <- payload$frame
  attr(design, "design_tools") <- payload$tools
  execution <- payload$execution
  if (!is_null(execution$frame_digest)) {
    execution$frame_digest <- tryCatch(
      decode_frame_digest(execution$frame_digest),
      error = function(e) {
        if (inherits(e, "samplyr_error_digest_version")) {
          stop(e)
        }
        cli_warn(c(
          "The frame digest stored with this design could not be read
           and was dropped.",
          "i" = conditionMessage(e)
        ))
        NULL
      }
    )
  }
  execution$frames <- decode_frame_schedule(execution$frames)
  attr(design, "execution") <- execution
  design
}

#' @noRd
decode_stage <- function(
  stage,
  tool_stage = NULL
) {
  strata <- NULL
  if (!is_null(stage$strata)) {
    strata <- new_stratum_spec(
      vars = decode_chr(stage$strata$vars),
      alloc = decode_chr(stage$strata$alloc),
      variance = decode_value(stage$strata$variance),
      cost = decode_value(stage$strata$cost),
      cv = decode_value(stage$strata$cv),
      importance = decode_value(stage$strata$importance),
      power = decode_num(stage$strata$power)
    )
  }

  clusters <- NULL
  if (!is_null(stage$clusters)) {
    clusters <- new_cluster_spec(vars = decode_chr(stage$clusters$vars))
  }

  draw_spec <- NULL
  if (!is_null(stage$draw)) {
    draw <- stage$draw
    method <- decode_method(
      draw$method,
      tool_method = tool_stage$method
    )
    draw_spec <- new_draw_spec(
      n = decode_value(draw$n),
      frac = decode_value(draw$frac),
      method = method$name,
      mos = decode_chr(draw$mos),
      prn = decode_chr(draw$prn),
      aux = decode_chr(draw$aux),
      bounds = decode_chr(draw$bounds),
      spread = decode_chr(draw$spread),
      min_n = decode_value(draw$min_n),
      max_n = decode_value(draw$max_n),
      round = decode_chr(draw$round) %||% "up",
      control = decode_control(draw$control),
      certainty_size = decode_value(draw$certainty_size),
      certainty_prop = decode_value(draw$certainty_prop),
      certainty_overflow = decode_chr(draw$certainty_overflow) %||% "error",
      certainty_plan = decode_certainty_plan(draw$certainty_plan),
      on_empty = decode_chr(draw$on_empty) %||% "error",
      method_type = method$registry_type,
      method_fixed = method$fixed_size,
      method_variance = method$variance_family,
      method_probabilities = method$probabilities,
      method_implementation = method$implementation
    )
  }

  new_sampling_stage(
    label = decode_chr(stage$label),
    strata = strata,
    clusters = clusters,
    draw_spec = draw_spec
  )
}

#' Decode a common method descriptor, preferring a matching samplyr extension
#' when one is present. A file produced by another tool can omit the extension
#' and still be read when its common identifier maps to a built-in method.
#' @noRd
decode_method <- function(
  method,
  tool_method = NULL,
  call = caller_env()
) {
  if (!is.list(method) || !is.character(method$id) || length(method$id) != 1) {
    abort_samplyr(
      "Design file contains an invalid sampling method.",
      class = "samplyr_error_design_file_malformed",
      call = call
    )
  }

  common_id <- method$id
  dictionary <- sampling_method_dictionary()
  by_common_id <- vapply(dictionary, `[[`, character(1), "id")
  common_match <- names(dictionary)[match(common_id, by_common_id)]
  if (!is.na(common_match)) {
    expected <- dictionary[[common_match]]
    fields <- c(
      "family", "algorithm", "replacement", "sample_size", "probabilities"
    )
    matches <- vapply(fields, function(field) {
      identical(decode_chr(method[[field]]), expected[[field]])
    }, logical(1))
    if (!all(matches)) {
      cli_abort(
        "Common sampling method {.val {common_id}} has contradictory
         properties.",
        call = call
      )
    }
  }

  if (!is_null(tool_method)) {
    # Validate metadata as a list before using `$`.
    if (!is.list(tool_method)) {
      abort_samplyr(
        "Design file contains invalid samplyr method metadata.",
        class = "samplyr_error_design_file_malformed",
        call = call
      )
    }
    name <- decode_chr(tool_method$name)
    if (is_null(name) || length(name) != 1) {
      abort_samplyr(
        "Design file contains invalid samplyr method metadata.",
        class = "samplyr_error_design_file_malformed",
        call = call
      )
    }
    known <- dictionary[[name]]
    if (!is_null(known) && !identical(known$id, common_id)) {
      cli_abort(
        c(
          "Common and samplyr sampling methods disagree.",
          "i" = "Common method {.val {common_id}} does not describe
                 samplyr method {.val {name}}."
        ),
        call = call
      )
    }
    if (is_null(known) && !identical(common_id, "tool_specific")) {
      cli_abort(
        "Unknown samplyr method {.val {name}} must use common method
         {.val tool_specific}.",
        call = call
      )
    }
    return(list(
      name = name,
      registry_type = decode_chr(tool_method$registry_type),
      fixed_size = decode_flag(tool_method$fixed_size),
      variance_family = decode_chr(tool_method$variance_family),
      probabilities = decode_chr(tool_method$probabilities),
      implementation = decode_chr(tool_method$implementation)
    ))
  }

  if (is.na(common_match)) {
    cli_abort(
      c(
        "Common sampling method {.val {common_id}} has no samplyr mapping.",
        "i" = "Add a {.field tools.samplyr} method extension to reproduce
               a tool-specific method."
      ),
      call = call
    )
  }
  list(
    name = common_match,
    registry_type = NULL,
    fixed_size = NULL,
    variance_family = NULL,
    probabilities = NULL,
    implementation = NULL
  )
}

#' Reconstruct samplyr's native frame metadata view from the portable and
#' namespaced representations.
#' @noRd
decode_frame_info <- function(
  frame,
  tool_frame = NULL
) {
  if (!is_null(frame[["fingerprints"]])) {
    tool_frames <- tool_frame$frames %||% list()
    return(list(
      required_variables = frame$required_variables,
      fingerprints = lapply(seq_along(frame[["fingerprints"]]), function(i) {
        decode_frame_fingerprint(
          frame[["fingerprints"]][[i]],
          if (length(tool_frames) >= i) tool_frames[[i]] else NULL
        )
      })
    ))
  }
  if (is_null(frame[["fingerprint"]])) {
    return(frame)
  }
  list(
    required_variables = frame$required_variables,
    fingerprint = decode_frame_fingerprint(frame[["fingerprint"]], tool_frame)
  )
}

#' @noRd
decode_frame_fingerprint <- function(
  portable,
  tool_frame = NULL
) {
  tool_columns <- tool_frame$columns %||% list()
  class_by_name <- setNames(
    lapply(tool_columns, function(x) decode_chr(x$class)),
    vapply(tool_columns, function(x) decode_chr(x$name), character(1))
  )
  columns <- lapply(portable$columns, function(column) {
    name <- decode_chr(column$name)
    native_class <- class_by_name[[name]]
    list(
      name = name,
      type = native_class[[1]] %||% portable_type_to_r(column$type)
    )
  })
  source <- tool_frame$source
  hash <- tool_frame$hash
  fingerprint <- list(
    nrow = decode_num(portable$row_count),
    ncol = decode_num(portable$column_count),
    columns = columns
  )
  if (!is_null(source$value)) {
    fingerprint$name <- decode_chr(source$value)
  }
  if (!is_null(hash$value)) {
    fingerprint$hash <- decode_chr(hash$value)
  }
  fingerprint
}

#' @noRd
portable_type_to_r <- function(type) {
  switch(
    decode_chr(type),
    boolean = "logical",
    integer = "integer",
    number = "numeric",
    string = "character",
    categorical = "factor",
    ordered_categorical = "ordered",
    date = "Date",
    date_time = "POSIXct",
    duration = "difftime",
    binary = "raw",
    composite = "list",
    "tool_specific"
  )
}

#' Decode a per-stratum value: scalar, named vector, or data frame
#' @noRd
decode_value <- function(x) {
  if (is_null(x) || (is.list(x) && length(x) == 0)) {
    return(NULL)
  }
  if (!is.list(x)) {
    return(x)
  }
  nms <- names(x)
  if (!is_null(nms) && all(nzchar(nms))) {
    return(unlist(x))
  }
  if (all(vapply(x, is.list, logical(1)))) {
    return(decode_rows(x))
  }
  unlist(x)
}

#' Decode a certainty-plan bridge spec (design format 3)
#'
#' Shape checks only: the execute-time gates are the validators for what the
#' spec means against a frame, and they run for deserialized designs too.
#' What the reader refuses is a block the gates could misread - missing
#' columns, wrong types, takes that are not positive whole numbers.
#' @noRd
decode_certainty_plan <- function(x, call = caller_env()) {
  if (is_null(x)) {
    return(NULL)
  }
  malformed <- function(what) {
    abort_samplyr(
      "Design file contains an invalid certainty plan: {what}.",
      class = "samplyr_error_design_file_malformed",
      call = call
    )
  }
  if (!is.list(x)) {
    malformed("not a plan block")
  }

  role <- decode_chr(x$role)
  if (is_null(role) || length(role) != 1 || !role %in% c("select", "take")) {
    malformed("unknown stage role")
  }
  strata_var <- decode_chr(x$strata_var)
  id_var <- decode_chr(x$id_var)
  if (
    is_null(strata_var) || length(strata_var) != 1 || !nzchar(strata_var) ||
      is_null(id_var) || length(id_var) != 1 || !nzchar(id_var)
  ) {
    malformed("the stratum and PSU id variables must be single names")
  }

  register <- decode_value(x$register)
  needed <- c("psu_id", "stratum", "N", "certainty", "n_take")
  if (!is.data.frame(register) || !all(needed %in% names(register)) ||
        nrow(register) == 0) {
    malformed("the register must have psu_id, stratum, N, certainty, n_take")
  }
  register <- as.data.frame(register[, needed], stringsAsFactors = FALSE)
  register$stratum <- as.character(register$stratum)
  if (anyNA(register$psu_id) || anyDuplicated(register$psu_id)) {
    malformed("register PSU ids must be present and unique")
  }
  if (!is.logical(register$certainty) || anyNA(register$certainty)) {
    malformed("the register's certainty column must be logical")
  }
  if (
    !is.numeric(register$N) || anyNA(register$N) ||
      any(!is.finite(register$N)) || any(register$N <= 0)
  ) {
    malformed("register sizes must be positive and finite")
  }
  if (
    !is.numeric(register$n_take) || anyNA(register$n_take) ||
      any(!is.finite(register$n_take)) || any(register$n_take < 1) ||
      any(register$n_take != floor(register$n_take))
  ) {
    malformed("register takes must be positive whole numbers")
  }

  decode_named_counts <- function(v, what, allow_na = FALSE) {
    if (!is.list(v) || is_null(names(v)) || !all(nzchar(names(v)))) {
      malformed(paste(what, "must be named"))
    }
    out <- vapply(
      v,
      function(e) if (is_null(e)) NA_real_ else as.numeric(e),
      numeric(1)
    )
    bad <- if (allow_na) {
      !is.na(out) & (out < 1 | out != floor(out))
    } else {
      is.na(out) | out < 0 | out != floor(out)
    }
    if (any(bad)) {
      malformed(paste(what, "must hold whole numbers"))
    }
    out
  }
  n_psu_draw <- decode_named_counts(x$n_psu_draw, "n_psu_draw")
  n_per_psu <- decode_named_counts(x$n_per_psu, "n_per_psu", allow_na = TRUE)

  list(
    role = role,
    register = register,
    n_psu_draw = n_psu_draw,
    n_per_psu = n_per_psu,
    strata_var = strata_var,
    id_var = id_var,
    svyplan_version = decode_chr(x$svyplan_version)
  )
}

#' Rebuild a data frame from JSON row objects
#' @noRd
decode_rows <- function(rows) {
  cols <- names(rows[[1]])
  consistent <- vapply(
    rows,
    function(row) identical(names(row), cols),
    logical(1)
  )
  if (is_null(cols) || !all(consistent)) {
    abort_samplyr(
      "Design file contains a malformed table entry",
      class = "samplyr_error_design_file_malformed"
    )
  }
  values <- lapply(cols, function(col) {
    unlist(lapply(rows, function(row) row[[col]] %||% NA))
  })
  names(values) <- cols
  tibble::as_tibble(values)
}

#' @noRd
decode_chr <- function(x) {
  if (is_null(x)) {
    return(NULL)
  }
  as.character(unlist(x))
}

#' @noRd
decode_num <- function(x) {
  if (is_null(x)) {
    return(NULL)
  }
  as.numeric(unlist(x))
}

#' @noRd
decode_flag <- function(x) {
  if (is_null(x)) {
    return(NULL)
  }
  as.logical(unlist(x))
}
