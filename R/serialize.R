#' Convert Sampling Design to List
#'
#' Converts a sampling design object to a plain list representation,
#' useful for inspection, serialization, or export.
#'
#' @param x A `sampling_design` object
#' @param ... Additional arguments (ignored)
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
#' @export
as.list.sampling_design <- function(x, ...) {
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

# Design files -----------------------------------------------------------

# On-disk format: a versioned JSON envelope.
#
# {
#   "format": "samplyr/design",
#   "format_version": 1,
#   "schema": { "method_vocabulary": {...} },
#   "design": { "title": ..., "stages": [...] },
#   "frame": { "required_variables": [...], "fingerprint": {...} },
#   "execution": { "seed": ..., "stages_executed": [...], ... },
#   "tools": { "samplyr": {...} }
# }
#
# Per-stratum values (n, frac, min_n, max_n, certainty_*, variance, cost,
# cv, importance) use a natural, unambiguous JSON encoding:
#   scalar        -> JSON number
#   named vector  -> JSON object
#   data frame    -> JSON array of row objects
#
# Control expressions use a declarative JSON grammar. Each term records an
# ordering type (ascending, descending, or serpentine) and its variables, so
# the file contains no R source code and reading it never parses or executes
# arbitrary code. The portable design and frame metadata are separate from
# namespaced samplyr/R metadata needed for exact reconstruction.

design_format_id <- "samplyr/design"
design_format_version <- 1L
method_vocabulary_id <- "samplyr/common-sampling-method"
method_vocabulary_version <- 1L

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
#' @details
#' ## Lifecycle
#'
#' The serialization interface and its samplyr-native file format are
#' experimental. They support samplyr persistence and replay; they are not a
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
#' `tbl_sample` -- the same rows in the same order, including `.panel` and
#' `.replicate` assignments -- with only the execution timestamp differing.
#' The sampled rows themselves are not stored; use a data format (CSV,
#' parquet) for those.
#'
#' Receipts describe one [execute()] call. A sample built by several
#' calls (a stage continuation or a multi-phase pipeline) is flagged as
#' `chained` in the receipt and `write_design()` warns: replaying the
#' final call alone cannot reproduce it, so save and replay each phase
#' or stage batch separately. A sample whose rows or design columns were
#' modified after execution is likewise flagged (`modified`); its
#' receipt describes the original execution, not the modified object.
#'
#' ## Control expressions
#'
#' `draw(control = ...)` expressions are stored as declarative JSON terms,
#' not R code. Each term records an ordering type (`"ascending"`,
#' `"descending"`, or `"serpentine"`) and its variables. Only bare column
#' names, `dplyr::desc()`, and [serp()] can be represented;
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
#' @param x A `sampling_design`, or a `tbl_sample` (the stored design is
#'   saved along with an execution receipt).
#' @param path File path to write to. Conventionally with a `.json`
#'   extension.
#' @param frame Optional sampling frame (data frame). When supplied, a
#'   fingerprint of the frame (name, dimensions, column types, content
#'   hash) is stored so the frame can be verified later. The frame data is
#'   never written. The content hash covers column names, column values,
#'   and row order; it does not depend on the class of the data frame
#'   (tibble or data frame) or on the order of its columns.
#' @param pretty Whether to pretty-print the JSON. Defaults to `TRUE` for
#'   files and `FALSE` for [design_json()].
#'
#' @return `write_design()` returns `x` invisibly. `read_design()` returns
#'   a `sampling_design`; any frame information and execution receipt in
#'   the file are attached as the `"frame_info"` and `"execution"`
#'   attributes.
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
#' @export
write_design <- function(x, path, frame = NULL, pretty = TRUE) {
  if (!is_character(path) || length(path) != 1) {
    cli_abort("{.arg path} must be a single file path")
  }
  frame_label <- frame_arg_label(enquo(frame))
  json <- build_design_json(
    x,
    frame = frame,
    frame_label = frame_label,
    pretty = pretty
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
#' @export
design_json <- function(x, frame = NULL, pretty = FALSE) {
  frame_label <- frame_arg_label(enquo(frame))
  build_design_json(
    x,
    frame = frame,
    frame_label = frame_label,
    pretty = pretty
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
  # jsonlite::fromJSON() downloads URL-shaped strings; reading a design
  # must never touch the network, so refuse them before handing off.
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
  decode_design_payload(payload)
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
#' carries a `chained` flag in its receipt and cannot be replayed;
#' save and replay each phase or stage batch separately.
#'
#' When the design was saved with a frame fingerprint, `frame` is
#' compared against it before replaying. A differing frame still yields
#' a valid sample, but not the recorded one, so the default is to error.
#' After replaying, the row count is checked against the receipt's
#' `n_selected` as a final consistency check.
#'
#' @param x A `sampling_design` carrying an execution receipt, as
#'   returned by [read_design()] for a file written from a
#'   `tbl_sample`. A `tbl_sample` is also accepted and is replayed from
#'   its own metadata, which is useful for verifying reproducibility
#'   without a file round trip.
#' @param frame The sampling frame the receipt refers to.
#' @param fingerprint How to respond when `frame` differs from the
#'   fingerprint stored in the design file: `"error"` (default), `"warn"`,
#'   `"inform"`, or `"ignore"`.
#'
#' @return The replayed `tbl_sample`.
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
#' unlink(path)
#' @seealso [write_design()] and [read_design()] for the receipt
#'   round trip, [validate_frame()] for checking a frame against a
#'   design before executing.
#' @export
replay_design <- function(
  x,
  frame,
  fingerprint = c("error", "warn", "inform", "ignore")
) {
  fingerprint <- match.arg(fingerprint)

  if (is_tbl_sample(x)) {
    design <- get_design(x)
    receipt <- encode_execution(x)
    frame_info <- NULL
    execution_environment <- attr(x, "metadata")$execution_environment
  } else if (is_sampling_design(x)) {
    design <- x
    receipt <- attr(x, "execution")
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

  fp <- frame_info$fingerprint
  if (!is_null(fp) && !identical(fingerprint, "ignore")) {
    diffs <- fingerprint_differences(fp, frame)
    if (length(diffs) > 0) {
      msg <- c(
        "{.arg frame} differs from the frame recorded with the design:",
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
  panels <- if (!is_null(receipt$panels)) {
    as.integer(receipt$panels)
  } else {
    NULL
  }

  result <- with_replay_rng(
    execution_environment$rng,
    execute(
      design,
      frame,
      stages = stages,
      seed = as.integer(seed),
      panels = panels,
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
    recorded <- list(
      type = spec$method_type,
      fixed_size = spec$method_fixed,
      variance_family = spec$method_variance
    )
    fields <- names(recorded)[!vapply(recorded, is_null, logical(1))]
    agrees <- vapply(
      fields,
      function(field) identical(recorded[[field]], current[[field]]),
      logical(1)
    )
    if (!all(agrees)) {
      abort_samplyr(
        c(
          "Registered method {.val {spec$method}} differs from the
           method recorded in the design file.",
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

# Encoding ----------------------------------------------------------------

#' @noRd
build_design_json <- function(x, frame, frame_label, pretty, call = caller_env()) {
  payload <- design_payload(
    x,
    frame = frame,
    frame_label = frame_label,
    call = call
  )
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

#' @noRd
design_payload <- function(
  x,
  frame = NULL,
  frame_label = NULL,
  call = caller_env()
) {
  execution <- NULL
  execution_environment <- NULL
  if (is_tbl_sample(x)) {
    execution <- encode_execution(x)
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
  if (
    is_tbl_sample(x) && is_null(frame) &&
      is_null(attr(design, "portable_frame_info")$fingerprint)
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
  payload$tools <- attr(design, "design_tools") %||% list()
  payload$tools$samplyr <- encode_samplyr_metadata(
    design,
    frame,
    frame_label,
    execution_environment
  )
  payload
}

# Sampling method vocabulary -----------------------------------------------

# The common identifiers are intentionally implementation-neutral. DDI's
# Sampling Procedure vocabulary supplies the broader standard classification;
# algorithm-level distinctions such as Brewer, Sampford, and cube are retained
# by the common identifier and properties because DDI does not distinguish all
# of them.

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
    draw$on_empty <- spec$on_empty
    out$draw <- draw
  }

  out
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

# Control expressions ------------------------------------------------------

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
    cli_abort("Design file contains a malformed control term.", call = call)
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
    cli_abort(
      c(
        "Design file contains an invalid control term.",
        "i" = "Each term needs a supported {.field type} and a non-empty
               {.field variables} array."
      ),
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

# Frame information --------------------------------------------------------

#' @noRd
frame_arg_label <- function(frame_quo) {
  if (is_null(quo_get_expr(frame_quo))) {
    return(NULL)
  }
  as_label(frame_quo)
}

#' @noRd
encode_frame_info <- function(design, frame, frame_label) {
  stored <- attr(design, "portable_frame_info")
  if (is_null(frame) && !is_null(stored)) {
    return(stored)
  }
  info <- list(required_variables = design_requirements(design))
  if (!is_null(frame)) {
    if (!is.data.frame(frame)) {
      cli_abort("{.arg frame} must be a data frame")
    }
    info$fingerprint <- portable_frame_fingerprint(frame)
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
    out$frame <- samplyr_frame_fingerprint(frame, frame_label)
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
    variance_family = spec$method_variance
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
  if (!is_null(frame_label)) {
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
#' @noRd
frame_content_hash <- function(frame) {
  ord <- order(names(frame), method = "radix")
  cols <- lapply(ord, function(i) frame[[i]])
  names(cols) <- names(frame)[ord]
  rlang::hash(cols)
}

# Execution receipts -------------------------------------------------------

#' @noRd
encode_execution <- function(sample) {
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
  # A sample produced by more than one execute() call (stage
  # continuation or multi-phase) cannot be reproduced by replaying the
  # final call alone; the receipt records only that call.
  if (!is_null(meta$continued_from) || !is_null(meta$prev_phase)) {
    receipt$chained <- TRUE
  }
  # Rows or design columns changed after execution: the receipt
  # describes the original execution, not the current object.
  if (!sample_realization_status(sample)$ok) {
    receipt$modified <- TRUE
  }
  receipt
}

# Decoding ----------------------------------------------------------------

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
    cli_abort(
      c(
        "Design file format version {.val {version}} is not supported.",
        "i" = "This version of samplyr reads format versions up to
               {.val {design_format_version}}. Update samplyr to read
               this file."
      ),
      call = call
    )
  }
  if (!is.list(payload$design) || !is.list(payload$design$stages)) {
    cli_abort("Design file has no {.field design.stages} entry", call = call)
  }
  vocabulary <- payload$schema$method_vocabulary
  if (
    !identical(vocabulary$id, method_vocabulary_id) ||
      !is.numeric(vocabulary$version) || length(vocabulary$version) != 1 ||
      vocabulary$version > method_vocabulary_version
  ) {
    cli_abort(
      "Design file uses an unsupported sampling method vocabulary.",
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
  attr(design, "execution") <- payload$execution
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
      on_empty = decode_chr(draw$on_empty) %||% "error",
      method_type = method$registry_type,
      method_fixed = method$fixed_size,
      method_variance = method$variance_family
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
    cli_abort("Design file contains an invalid sampling method.", call = call)
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
    name <- decode_chr(tool_method$name)
    if (is_null(name) || length(name) != 1) {
      cli_abort(
        "Design file contains invalid samplyr method metadata.",
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
      variance_family = decode_chr(tool_method$variance_family)
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
    variance_family = NULL
  )
}

#' Reconstruct samplyr's native frame metadata view from the portable and
#' namespaced representations.
#' @noRd
decode_frame_info <- function(
  frame,
  tool_frame = NULL
) {
  if (is_null(frame$fingerprint)) {
    return(frame)
  }
  portable <- frame$fingerprint
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
  list(
    required_variables = frame$required_variables,
    fingerprint = fingerprint
  )
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
    cli_abort("Design file contains a malformed table entry")
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
