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
#   "samplyr_version": "0.8.9999",
#   "design": { "title": ..., "stages": [...] },
#   "frame": { "required_variables": [...], "fingerprint": {...} },
#   "execution": { "seed": ..., "stages_executed": [...], ... }
# }
#
# Per-stratum values (n, frac, min_n, max_n, certainty_*, variance, cost,
# cv, importance) use a natural, unambiguous JSON encoding:
#   scalar        -> JSON number
#   named vector  -> JSON object
#   data frame    -> JSON array of row objects
#
# Control expressions are stored as deparsed text and reparsed on read
# against an allowlist (bare columns, desc(), serp()), so reading a design
# file never executes arbitrary code.

design_format_id <- "samplyr/design"
design_format_version <- 1L

#' Write a sampling design to a file
#'
#' @description
#' `write_design()` saves a sampling design (or the design carried by an
#' executed sample) to a portable, human-readable JSON file.
#' `read_design()` reads it back into a `sampling_design` that executes
#' identically to the original.
#'
#' The file format is versioned JSON: diffable in version control,
#' language-neutral, and stable across samplyr versions. It stores the
#' complete design specification (stages, stratification, clustering, draw
#' settings, including per-stratum vectors and data frames), never the
#' frame data itself.
#'
#' @details
#' ## Frame information
#'
#' Designs are frame-independent, and so are design files. Two derived
#' blocks describe the frame without embedding it:
#'
#' - *Requirements* (always written): the columns each stage needs
#'   (stratification, clustering, `mos`, `prn`, `aux`, and control
#'   variables), so any candidate frame can be checked before execution
#'   with [validate_frame()].
#' - *Fingerprint* (written when `frame` is supplied): the frame's name,
#'   dimensions, column types, and a content hash, so you can later verify
#'   that a frame is the exact one the design was built against.
#'
#' ## Execution receipts
#'
#' When `x` is a `tbl_sample`, the file additionally records an execution
#' receipt: every argument of the [execute()] call that affects the
#' result (`seed`, executed stages, `panels`, `reps`, and the per
#' replicate seeds), plus the number of selected units and the execution
#' timestamp. Together with the frame fingerprint this makes a
#' single-call sample exactly reproducible: anyone with the frame can
#' run `replay_design(read_design(path), frame)` and obtain the same
#' `tbl_sample` -- the same rows in the same order, including `.panel`
#' and `.replicate` assignments -- with only the execution timestamp
#' differing. The sampled rows themselves are not stored; use a data
#' format (CSV, parquet) for those.
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
#' `draw(control = ...)` expressions are stored as text and rebuilt on
#' read. Only bare column names, `dplyr::desc()`, and [serp()] can be
#' represented; `write_design()` errors on anything else, and
#' `read_design()` refuses to evaluate any other function, so design files
#' from third parties cannot execute arbitrary code.
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
#' Given the same frame, the replayed sample is identical to the
#' original: the same rows in the same order, the same weights and
#' design columns, and the same `.panel` and `.replicate` assignments.
#' Only the execution timestamp differs.
#'
#' Receipts record a single [execute()] call. A sample produced by
#' several calls (a stage continuation or a multi-phase pipeline)
#' carries a `chained` flag in its receipt and cannot be replayed;
#' save and replay each phase or stage batch separately.
#'
#' When the design was saved with a frame fingerprint, `frame` is
#' compared against it before replaying. A differing frame still yields
#' a valid sample, but not the recorded one, so the default is to warn.
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
#'   fingerprint stored in the design file: `"warn"` (default),
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
  fingerprint = c("warn", "inform", "ignore")
) {
  fingerprint <- match.arg(fingerprint)

  if (is_tbl_sample(x)) {
    design <- get_design(x)
    receipt <- encode_execution(x)
    frame_info <- NULL
  } else if (is_sampling_design(x)) {
    design <- x
    receipt <- attr(x, "execution")
    frame_info <- attr(x, "frame_info")
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
      if (identical(fingerprint, "warn")) {
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

  result <- execute(
    design,
    frame,
    stages = stages,
    seed = as.integer(seed),
    panels = panels,
    reps = reps
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
  if (is_tbl_sample(x)) {
    execution <- encode_execution(x)
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
  } else {
    cli_abort(
      "{.arg x} must be a {.cls sampling_design} or a {.cls tbl_sample}",
      call = call
    )
  }
  validate_sampling_design(design, call = call)
  check_controls_serializable(design, call = call)

  payload <- list(
    format = design_format_id,
    format_version = design_format_version,
    samplyr_version = as.character(utils::packageVersion("samplyr")),
    design = encode_design(design)
  )
  payload$frame <- encode_frame_info(design, frame, frame_label)
  payload$execution <- execution
  payload
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
    draw$method <- spec$method
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
      draw$control <- I(encode_control(spec$control))
    }
    draw$certainty_size <- encode_value(spec$certainty_size)
    draw$certainty_prop <- encode_value(spec$certainty_prop)
    draw$certainty_overflow <- spec$certainty_overflow
    draw$on_empty <- spec$on_empty
    draw$method_type <- spec$method_type
    draw$method_fixed <- spec$method_fixed
    draw$method_variance <- spec$method_variance
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
  vapply(
    control_quos,
    function(quo) paste(deparse(quo_get_expr(quo)), collapse = " "),
    character(1)
  )
}

#' @noRd
decode_control <- function(control) {
  if (is_null(control)) {
    return(NULL)
  }
  env <- control_eval_env()
  lapply(control, function(text) {
    expr <- tryCatch(
      rlang::parse_expr(text),
      error = function(cnd) {
        cli_abort(
          "Invalid control expression in design file: {.code {text}}",
          parent = cnd
        )
      }
    )
    if (!control_expr_serializable(expr)) {
      cli_abort(c(
        "Refusing to evaluate the control expression {.code {text}} from
         a design file.",
        "i" = "Only bare column names, {.fn desc}, and {.fn serp} calls
               on bare column names are allowed."
      ))
    }
    new_quosure(expr, env)
  })
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
    return(
      length(args) > 0 && all(vapply(args, is.symbol, logical(1)))
    )
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
  info <- list(required_variables = design_requirements(design))
  if (!is_null(frame)) {
    if (!is.data.frame(frame)) {
      cli_abort("{.arg frame} must be a data frame")
    }
    info$fingerprint <- frame_fingerprint(frame, frame_label)
  }
  info
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

#' @noRd
frame_fingerprint <- function(frame, frame_label) {
  fingerprint <- list()
  if (!is_null(frame_label)) {
    fingerprint$name <- frame_label
  }
  fingerprint$nrow <- nrow(frame)
  fingerprint$ncol <- ncol(frame)
  fingerprint$columns <- lapply(names(frame), function(col) {
    list(name = col, type = class(frame[[col]])[[1]])
  })
  fingerprint$hash <- frame_content_hash(frame)
  fingerprint
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
  if (!is.numeric(version) || version > design_format_version) {
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

  stages <- lapply(payload$design$stages, decode_stage)
  design <- new_sampling_design(
    title = decode_chr(payload$design$title),
    stages = stages,
    current_stage = length(stages),
    validated = FALSE
  )
  design <- validate_sampling_design(design, call = call)

  attr(design, "frame_info") <- payload$frame
  attr(design, "execution") <- payload$execution
  design
}

#' @noRd
decode_stage <- function(stage) {
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
    draw_spec <- new_draw_spec(
      n = decode_value(draw$n),
      frac = decode_value(draw$frac),
      method = decode_chr(draw$method) %||% "srswor",
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
      method_type = decode_chr(draw$method_type),
      method_fixed = decode_flag(draw$method_fixed),
      method_variance = decode_chr(draw$method_variance)
    )
  }

  new_sampling_stage(
    label = decode_chr(stage$label),
    strata = strata,
    clusters = clusters,
    draw_spec = draw_spec
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
