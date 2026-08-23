## Overlapping frames covering one target population

# These independently sampled frames overlap on one target population.

#' The separator between frame names in a domain label
#'
#' Frame names carrying it are refused, so the label stays reversible.
#' @noRd
frame_domain_separator <- "+"

#' @noRd
frame_stack_columns <- c(".frame", ".domain")

#' Stack samples selected from overlapping frames
#'
#' @description
#' Collects two or more samples, each selected from a frame that covers part
#' of one target population, into a single object that records which frames
#' every sampled unit belongs to. Frame A and frame B are two registers of the
#' same people, and a unit listed in both had two chances of being selected.
#'
#' The samples are selected independently: two designs, two frames, two seeds,
#' two calls to [execute()]. Nothing about selection is shared, so nothing is
#' combined until here.
#'
#' @details
#' ## What it does not do
#'
#' It does not composite the weights. Which compositing factor to use, and
#' whether it should follow Hartley, the multiplicity estimator or
#' pseudo-maximum-likelihood, depends on the estimand and on the design
#' effects, so it is an estimation-time choice rather than a property of the
#' stack. The components keep their own design weights, unchanged.
#'
#' It does not assert that the frames together cover the target population.
#' That is an assumption about the registers, not something this call can
#' establish, which is why the verb is not named after the union.
#'
#' ## Membership is declared, and it is logical
#'
#' `membership` maps each frame's name to the column saying whether a unit
#' belongs to that frame. Every component must carry all of the columns, not
#' only its own: a unit selected from frame A has to say whether it was also
#' listed in frame B, and that is the information compositing needs. An
#' unresolved membership is an error rather than `NA`.
#'
#' Membership columns must be `logical`. Integer `0` and `1` are refused
#' rather than read, because numeric overlap information is a different input
#' on a different scale.
#'
#' ## Independence is assumed, and one violation of it is detectable
#'
#' Two components carrying the same recorded seed warn with
#' `samplyr_warning_frame_seed_reused`. Distinct seeds are not evidence of
#' independence, and the warning detects one recorded common-random-number
#' mistake and nothing more. A component executed with `seed = NULL` records
#' no seed and never warns, so a deterministic take-all component is exempt
#' unless it was given a seed it did not use.
#'
#' @param ... Two or more named samples, one per frame, each an executed
#'   `tbl_sample`. The names are the frame names and they appear in the
#'   `.domain` labels, so none may contain `+`.
#' @param membership A named character vector mapping every frame name to the
#'   column holding that frame's membership indicator, in the same direction
#'   as `c(frame_name = "column_name")`. Every component must contain every
#'   one of the columns.
#' @param key A bare column identifying the target-population unit, present in
#'   every component. The same unit may be selected from several frames, so
#'   the key repeats across components on purpose. Within a component it must
#'   be unique, or unique per selection occurrence where the component
#'   replicates rows with replacement.
#' @param overlaps Optional. The probability, or the weight, each sampled unit
#'   *would have had* in every frame, including the frames it was not selected
#'   from. Name the columns holding them with [declared_overlaps()], which
#'   states the scale, since it is never inferred from the values. Or name the
#'   registers with [exante_overlaps()] and let samplyr resolve the chances
#'   from each component's own design, which is exact and is checked against
#'   what the execution produced. Required by `estimator = "expected"` at
#'   export and unused by the default estimator, which reads membership
#'   alone.
#'
#' @return An object of class `frame_stack`: a named list of the component
#'   samples, unchanged, carrying the membership mapping, the key, and any
#'   overlap record. It is not a `tbl_sample` and not a data frame. `[[`
#'   returns an intact component, and [as.data.frame()] gives the row-bound
#'   inspection view.
#'
#' @references
#' Lohr, S. L. (2021). Multiple-frame surveys for a multiple-data-source
#' world. *Survey Methodology*, 47(2), 229-263.
#'
#' Mecatti, F. (2007). A single frame multiplicity estimator for multiple
#' frame surveys. *Survey Methodology*, 33(2), 151-157.
#'
#' @examples
#' population <- data.frame(
#'   person_id = 1:60,
#'   in_landline = rep(c(TRUE, FALSE), times = c(40, 20)),
#'   in_cell = rep(c(FALSE, TRUE), times = c(10, 50))
#' )
#'
#' s_landline <- sampling_design() |>
#'   draw(n = 10) |>
#'   execute(population[population$in_landline, ], seed = 1)
#'
#' s_cell <- sampling_design() |>
#'   draw(n = 12) |>
#'   execute(population[population$in_cell, ], seed = 2)
#'
#' frames <- stack_frames(
#'   landline = s_landline,
#'   cell = s_cell,
#'   membership = c(landline = "in_landline", cell = "in_cell"),
#'   key = person_id
#' )
#'
#' frames
#' summary(frames)
#' head(as.data.frame(frames))
#'
#' @seealso [as.data.frame.frame_stack()] for the row-bound view,
#'   [share_weights()] for reaching a linked population through one frame
#'
#' @family multiple frames
#' @export
stack_frames <- function(..., membership, key, overlaps = NULL) {
  samples <- rlang::list2(...)
  check_frame_components(samples)

  if (missing(membership)) {
    abort_samplyr(
      c(
        "{.arg membership} must be given.",
        "i" = "Map every frame name to the column holding its membership
               indicator: {.code membership = c(frame = \"column\")}.",
        "i" = "It is what says which frames a sampled unit belongs to, and no
               column name can be inferred from a frame name."
      ),
      class = "samplyr_error_stack_frames_membership"
    )
  }
  membership <- parse_membership_map(membership, names(samples))
  key_col <- parse_frame_key(rlang::enquo(key))
  overlap_spec <- parse_overlap_spec(overlaps, names(samples))

  validate_frame_stack(samples, membership, key_col, overlap_spec)
  overlap_spec <- resolve_frame_overlaps(samples, membership, overlap_spec)
  warn_reused_frame_seeds(samples)

  new_frame_stack(
    samples,
    membership = membership,
    key = key_col,
    overlaps = overlap_spec
  )
}

#' Declare the overlap values and the scale they are on
#'
#' @description
#' A declarative marker for [stack_frames()]'s `overlaps` argument. It names,
#' for every frame, the column holding the chance a unit would have been
#' selected from that frame, including the frames it was not selected from,
#' and states which scale those columns are on.
#'
#' It is the declaring half of a pair. [exante_overlaps()] is the other:
#' rather than naming columns that already hold the chances, it names the
#' registers and lets samplyr resolve them from each frame's own design.
#'
#' @details
#' The scale is declared rather than inferred from the values.
#' `survey::multiframe()` infers it, reading a matrix as weights when no
#' non-zero entry in some frame falls below one, which is reachable whenever a
#' frame is a census or its overlapping units are certainties. samplyr
#' produces those routinely, so the value that decides the reading is a value
#' this package generates on purpose.
#'
#' `"probabilities"` and `"weights"` are reciprocal statements of the same
#' thing, and the record normalizes either to probabilities, so the two are
#' interchangeable once the scale is stated. `scale` has no default for the
#' same reason it is not inferred: a probability of one and a weight of one
#' are the same number.
#'
#' @param ... One named argument per frame, giving the column name as a
#'   string: `declared_overlaps(area = "pi_area", list = "pi_list")`.
#' @param scale Which quantity the columns hold, `"probabilities"` or
#'   `"weights"`. Required, and matched by exact name.
#'
#' @return An object of class `samplyr_overlap_spec`, carrying the declared
#'   scale and the column mapping, for `stack_frames()`'s `overlaps` argument.
#'
#' @seealso [exante_overlaps()] to resolve the chances from the registers
#'   instead of naming columns that hold them
#'
#' @examples
#' declared_overlaps(area = "pi_area", list = "pi_list",
#'                   scale = "probabilities")
#'
#' declared_overlaps(area = "w_area", list = "w_list", scale = "weights")
#'
#' @family multiple frames
#' @export
declared_overlaps <- function(..., scale) {
  if (missing(scale)) {
    abort_samplyr(
      c(
        "{.arg scale} must be given.",
        "i" = "Say whether the columns hold {.val probabilities} or
               {.val weights}.",
        "i" = "It is never inferred from the values: a probability of one and
               a weight of one are the same number."
      ),
      class = "samplyr_error_stack_frames_overlaps"
    )
  }
  if (!is_scalar_string(scale) || !scale %in% overlap_scales) {
    abort_samplyr(
      c(
        "{.arg scale} must be {.val probabilities} or {.val weights}.",
        "x" = "Got {.code {as_label(scale)}}."
      ),
      class = "samplyr_error_stack_frames_overlaps"
    )
  }
  new_overlap_spec(scale, rlang::list2(...))
}

#' @noRd
overlap_scales <- c("probabilities", "weights")

#' A declared overlap scale and the columns carrying it
#'
#' A value rather than an expression marker, unlike `complete_links()` on the
#' other feature. That marker names a column of a data frame and has to be
#' evaluated in its mask, whereas this one carries strings and a scale, so
#' making it a value keeps it composable in the same way the `membership`
#' argument beside it already is.
#' @noRd
new_overlap_spec <- function(scale, args, call = caller_env()) {
  nms <- names(args) %||% rep("", length(args))
  ok <- vapply(args, is_scalar_string, logical(1))
  if (length(args) == 0L || !all(nzchar(nms)) || !all(ok)) {
    abort_samplyr(
      c(
        "Every frame's overlap column must be named and given as a string.",
        "i" = "{.code declared_overlaps(frame = \"column\", ...,
               scale = \"{scale}\")}, one entry per frame."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }
  if (anyDuplicated(nms) > 0) {
    abort_samplyr(
      c(
        "Each frame may be named once.",
        "x" = "Repeated: {.val {unique(nms[duplicated(nms)])}}."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }

  cols <- vapply(args, identity, character(1))
  names(cols) <- nms
  if (anyDuplicated(unname(cols)) > 0) {
    abort_samplyr(
      c(
        "Each frame needs its own overlap column.",
        "x" = "{.field {unique(cols[duplicated(unname(cols))])}} is given for
               more than one frame."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }

  structure(
    list(scale = scale, cols = cols),
    class = "samplyr_overlap_spec"
  )
}

#' @noRd
is_overlap_spec <- function(x) {
  inherits(x, "samplyr_overlap_spec")
}

#' A stack of frames is a collection, not a sample
#'
#' The components are separately weighted samples of overlapping registers.
#' Row-binding them would double-count every unit listed in two frames, and
#' what to do about that is an estimation-time decision, so the object keeps
#' the components apart and keeps each one's receipt with it.
#' @noRd
new_frame_stack <- function(samples, membership, key, overlaps = NULL) {
  structure(
    samples,
    membership = membership,
    key = key,
    overlaps = overlaps,
    class = "frame_stack"
  )
}

#' @noRd
is_frame_stack <- function(x) {
  inherits(x, "frame_stack")
}

#' The designs that rebuild a collection, without its data
#'
#' What [read_design()] returns for a frame collection file, standing to a
#' `frame_stack` as a `sampling_design` stands to a `tbl_sample`: the
#' components' designs and execution receipts, the membership mapping, the
#' key, and any declared overlaps. [replay_design()] executes each component
#' against its register and stacks the results.
#'
#' A named list rather than a tibble, for the reason `frame_stack` is one:
#' the component boundaries and their separate receipts have to survive.
#' @noRd
new_frame_stack_design <- function(designs, membership, key, overlaps = NULL) {
  structure(
    designs,
    membership = membership,
    key = key,
    overlaps = overlaps,
    class = "frame_stack_design"
  )
}

#' @noRd
is_frame_stack_design <- function(x) {
  inherits(x, "frame_stack_design")
}

## Argument parsing

#' @noRd
parse_membership_map <- function(map, frames, call = caller_env()) {
  nms <- names(map)
  if (
    !is.character(map) || length(map) == 0L ||
      is_null(nms) || !all(nzchar(nms)) || anyNA(map) || anyNA(nms)
  ) {
    abort_samplyr(
      c(
        "{.arg membership} must be a named character vector.",
        "i" = "Name the frames as the components are named:
               {.code membership = c(frame = \"column\")}."
      ),
      class = "samplyr_error_stack_frames_membership",
      call = call
    )
  }

  extra <- setdiff(nms, frames)
  absent <- setdiff(frames, nms)
  if (length(extra) > 0 || length(absent) > 0 || anyDuplicated(nms) > 0) {
    abort_samplyr(
      c(
        "{.arg membership} must name every frame exactly once.",
        if (length(absent) > 0) {
          c("x" = "No entry for {.val {absent}}.")
        },
        if (length(extra) > 0) {
          c("x" = "No component named {.val {extra}}.")
        },
        if (anyDuplicated(nms) > 0) {
          c("x" = "Repeated: {.val {unique(nms[duplicated(nms)])}}.")
        }
      ),
      class = "samplyr_error_stack_frames_membership",
      call = call
    )
  }

  if (anyDuplicated(unname(map)) > 0) {
    abort_samplyr(
      c(
        "Each frame needs its own membership column.",
        "x" = "{.field {unique(map[duplicated(unname(map))])}} is given for
               more than one frame.",
        "i" = "One column shared by two frames would make every unit belong to
               both."
      ),
      class = "samplyr_error_stack_frames_membership",
      call = call
    )
  }

  map[frames]
}

#' Match a declared overlap spec to the frames of the stack
#' @noRd
parse_overlap_spec <- function(overlaps, frames, call = caller_env()) {
  if (is_null(overlaps)) {
    return(NULL)
  }
  if (is_exante_overlap_spec(overlaps)) {
    return(overlaps)
  }
  # Accept an existing frame stack record.
  if (is_resolved_overlaps(overlaps)) {
    abort_samplyr(
      c(
        "{.arg overlaps} is a record {.fn stack_frames} already resolved.",
        "x" = "It holds one chance per selected unit, which describes the
               components it was resolved against and no others.",
        "i" = "Pass the {.fn exante_overlaps} request again. It names the
               registers, and resolving it against these components is what
               makes the chances theirs."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }
  if (!is_overlap_spec(overlaps)) {
    abort_samplyr(
      c(
        "{.arg overlaps} must come from {.fn declared_overlaps} or
         {.fn exante_overlaps}.",
        "i" = "The scale is declared, never read off the values: a
               probability of one and a weight of one are the same number."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }

  nms <- names(overlaps$cols)
  if (!setequal(nms, frames)) {
    abort_samplyr(
      c(
        "{.arg overlaps} must name every frame exactly once.",
        "x" = "Frames: {.val {frames}}. Given: {.val {nms}}."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }

  overlaps$cols <- overlaps$cols[frames]
  overlaps
}

#' @noRd
parse_frame_key <- function(quo, class = "samplyr_error_stack_frames_key",
                            call = caller_env()) {
  if (rlang::quo_is_missing(quo)) {
    abort_samplyr(
      c(
        "{.arg key} must be given.",
        "i" = "It identifies the target-population unit, so that the same unit
               selected from two frames is recognizable as one unit.",
        "i" = "Use a bare column present in every component."
      ),
      class = class,
      call = call
    )
  }

  expr <- quo_get_expr(quo)
  if (!is.symbol(expr)) {
    abort_samplyr(
      c(
        "{.arg key} must be a bare column name.",
        "x" = "Got {.code {as_label(expr)}}."
      ),
      class = class,
      call = call
    )
  }
  as_label(expr)
}

## Structural checks

#' The components themselves, before anything is asked of their columns
#' @noRd
check_frame_components <- function(samples, call = caller_env()) {
  if (length(samples) < 2L) {
    abort_samplyr(
      c(
        "{.fn stack_frames} stacks two or more frames.",
        "x" = "{length(samples)} {?was/were} given.",
        "i" = "A single frame is already a sample: use it directly, or export
               it with {.fn as_svydesign}."
      ),
      class = "samplyr_error_stack_frames_input",
      call = call
    )
  }

  nms <- names(samples) %||% rep("", length(samples))
  if (!all(nzchar(nms)) || anyNA(nms)) {
    abort_samplyr(
      c(
        "Every frame must be named.",
        "x" = "Argument{cli::qty(sum(!nzchar(nms) | is.na(nms)))}{?s}
               {which(!nzchar(nms) | is.na(nms))} {?has/have} no name.",
        "i" = "The name is the frame's name and it labels the domains:
               {.code stack_frames(area = s_area, list = s_list, ...)}."
      ),
      class = "samplyr_error_stack_frames_names",
      call = call
    )
  }
  if (anyDuplicated(nms) > 0) {
    abort_samplyr(
      c(
        "Frame names must be distinct.",
        "x" = "Repeated: {.val {unique(nms[duplicated(nms)])}}.",
        "i" = "Two frames sharing a name cannot be told apart in a domain
               label."
      ),
      class = "samplyr_error_stack_frames_names",
      call = call
    )
  }
  collides <- grep(frame_domain_separator, nms, fixed = TRUE, value = TRUE)
  if (length(collides) > 0) {
    abort_samplyr(
      c(
        "Frame names must not contain {.val {frame_domain_separator}}.",
        "x" = "Offending: {.val {collides}}.",
        "i" = "It joins the frame names in a {.field .domain} label, so a name
               carrying it would make the label ambiguous."
      ),
      class = "samplyr_error_stack_frames_separator",
      call = call
    )
  }

  bad <- which(!vapply(samples, is_tbl_sample, logical(1)))
  if (length(bad) > 0) {
    first <- nms[[bad[[1]]]]
    abort_samplyr(
      c(
        "{.fn stack_frames} takes executed samples.",
        "x" = "{cli::qty(length(bad))}Frame{?s} {.val {nms[bad]}}
               {cli::qty(length(bad))}{?is/are} not one.",
        frame_component_hint(first)
      ),
      class = "samplyr_error_stack_frames_input",
      call = call
    )
  }

  for (nm in nms) {
    check_single_replicate(samples[[nm]], "stack_frames", call = call)
    check_sample_unmodified(samples[[nm]], "stack_frames", call = call)
  }
  invisible(NULL)
}

#' What to tell someone whose component is not a sample
#'
#' `membership` and `key` follow `...`, so they are matched by exact name and
#' a near miss lands among the components instead of raising R's own "unused
#' argument" error. It is diagnosed here, where the name is still visible.
#' @noRd
frame_component_hint <- function(name) {
  suggestion <- suggest_reserved_arg(
    name,
    c("membership", "key", "overlaps")
  )
  if (is_null(suggestion)) {
    c("i" = "Each comes from {.code execute(design, frame)}.")
  } else {
    c("i" = cli::format_inline("Did you mean {.arg {suggestion}}?"))
  }
}

#' Everything the components have to agree on, one question at a time
#'
#' Each check runs across all components rather than component by component,
#' so a message names every frame with the same problem instead of the first.
#' @noRd
validate_frame_stack <- function(samples, membership, key, overlaps = NULL,
                                 call = caller_env()) {
  frames <- names(samples)
  cols <- unname(membership)

  clash <- unique(unlist(lapply(samples, function(s) {
    intersect(names(s), frame_stack_columns)
  }), use.names = FALSE))
  if (length(clash) > 0) {
    abort_samplyr(
      c(
        "{.fn stack_frames} generates columns a component already carries as
         data.",
        "x" = "Conflicting: {.field {clash}}.",
        "i" = "The generated names are {.field {frame_stack_columns}}. Rename
               the data columns before stacking."
      ),
      class = "samplyr_error_stack_frames_columns",
      call = call
    )
  }

  for (col in cols) {
    without <- frames[!vapply(samples, function(s) col %in% names(s),
                              logical(1))]
    if (length(without) > 0) {
      abort_samplyr(
        c(
          "Every component must carry every membership column.",
          "x" = "{.field {col}} is missing from {.val {without}}.",
          "i" = "A unit selected from one frame has to say whether it is
                 listed in the others, which is what a composite weight needs."
        ),
        class = "samplyr_error_stack_frames_membership",
        call = call
      )
    }
  }

  check_membership_type(samples, cols, call = call)
  check_membership_complete(samples, membership, call = call)
  check_frame_key_columns(samples, key, call = call)
  check_frame_overlaps(samples, membership, overlaps, call = call)
  invisible(NULL)
}

#' Everything a declared overlap has to satisfy before it is stored
#'
#' The values say what a unit's chance of selection would have been in a frame
#' it may not have been selected from, so nothing about them can be checked
#' against the realized sample except the diagonal. That diagonal is worth a
#' great deal: samplyr knows what the design weight actually was, so a
#' supplied own-frame value that disagrees with it is caught here rather than
#' becoming a variance nobody can trace.
#' @noRd
check_frame_overlaps <- function(samples, membership, overlaps,
                                 call = caller_env()) {
  if (is_null(overlaps)) {
    return(NULL)
  }
  frames <- names(samples)

  shared <- frames[vapply(samples, function(component) {
    identical(sample_weight_contract(component), "shared")
  }, logical(1))]
  if (length(shared) > 0) {
    abort_samplyr(
      c(
        "{.arg overlaps} cannot describe a component whose weights were
         shared from another population.",
        "x" = "{cli::qty(length(shared))}Frame{?s} {.val {shared}}
               carr{?ies/y} shared weights.",
        "i" = "A shared weight is a realized quantity, not an inverse
               inclusion probability, so it cannot stand as the value a unit
               would have had in this frame.",
        "i" = "The probability that a target unit is reached at all is a
               joint-inclusion quantity over its linked source units, and it
               is not derived here."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }

  # Defer register checks to overlap resolution.
  if (is_exante_overlap_spec(overlaps)) {
    return(invisible(NULL))
  }

  cols <- overlaps$cols
  for (col in cols) {
    without <- frames[!vapply(samples, function(s) col %in% names(s),
                              logical(1))]
    if (length(without) > 0) {
      abort_samplyr(
        c(
          "Every component must carry every overlap column.",
          "x" = "{.field {col}} is missing from {.val {without}}.",
          "i" = "The expected estimator needs a unit's chance in the frames
                 it was not selected from, so every component states all of
                 them."
        ),
        class = "samplyr_error_stack_frames_overlaps",
        call = call
      )
    }
  }

  for (nm in frames) {
    component <- samples[[nm]]
    member <- frame_component_membership(component, membership)
    for (q in frames) {
      check_overlap_column(
        component[[cols[[q]]]], member[, q], overlaps$scale,
        frame = nm, of = q, col = cols[[q]], call = call
      )
    }
    check_overlap_diagonal(component, cols[[nm]], overlaps$scale, nm,
                           call = call)
  }
  invisible(NULL)
}

#' Turn a resolution request into the record the stack stores
#'
#' Separate from `validate_frame_stack()` because a validator returns
#' `invisible(NULL)` everywhere else in this package, and a resolver that
#' hides inside one is found by reading the call site rather than the name.
#' A declared spec passes through unchanged, and only `exante_overlaps()` has
#' anything to resolve.
#' @noRd
resolve_frame_overlaps <- function(samples, membership, overlaps,
                                   call = caller_env()) {
  if (!is_exante_overlap_spec(overlaps)) {
    return(overlaps)
  }
  resolve_exante_overlaps(samples, overlaps, membership, call = call)
}

#' @noRd
check_overlap_column <- function(value, member, scale, frame, of, col,
                                 call = caller_env()) {
  if (!is.numeric(value)) {
    abort_samplyr(
      c(
        "Overlap columns must be numeric.",
        "x" = "{.field {col}} in {.val {frame}} is
               {.cls {class(value)[[1]]}}."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }

  inside <- value[member]
  bad <- if (identical(scale, "probabilities")) {
    which(is.na(inside) | inside <= 0 | inside > 1)
  } else {
    which(is.na(inside) | inside < 1 | !is.finite(inside))
  }
  if (length(bad) > 0) {
    range_txt <- if (identical(scale, "probabilities")) {
      "in {.code (0, 1]}"
    } else {
      "at least {.val {1}}"
    }
    abort_samplyr(
      c(
        paste0("A unit's overlap value for a frame it belongs to must be ",
               range_txt, "."),
        "x" = "{.field {col}} in {.val {frame}} has {length(bad)} such
               value{?s} that {?is/are} not, for units in {.val {of}}.",
        "i" = "A member of a frame had some chance of being selected from it."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }

  # Use zero for a chance outside the frame.
  outside <- value[!member]
  if (any(!is.na(outside) & outside != 0)) {
    abort_samplyr(
      c(
        "A unit outside a frame has no overlap value for it.",
        "x" = "{.field {col}} in {.val {frame}} is non-zero for
               {sum(!is.na(outside) & outside != 0)} unit{?s} that {?is/are}
               not in {.val {of}}.",
        "i" = "Use {.val {0}} or {.val {NA}} there."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }
  invisible(NULL)
}

#' The one overlap value samplyr can check against what actually happened
#' Where a supplied or resolved own-frame value parts from the realized one
#'
#' The comparison itself, in one place. Both diagonal checks used the same
#' relative tolerance and the same worst-offender pick, written twice, which
#' is the part that could drift silently. The messages stay separate: one is
#' about a column a user supplied and the other about a chance samplyr
#' resolved, and they say different things.
#' @noRd
own_frame_disagreement <- function(supplied, realized) {
  off <- abs(supplied - realized) > 1e-6 * pmax(abs(realized), 1)
  if (!any(off)) {
    return(NULL)
  }
  worst <- which.max(abs(supplied - realized))
  list(n = sum(off), supplied = supplied[[worst]], realized = realized[[worst]])
}

#' @noRd
check_overlap_diagonal <- function(component, col, scale, frame,
                                   call = caller_env()) {
  supplied <- component[[col]]
  realized <- if (identical(scale, "probabilities")) {
    1 / component[[".weight"]]
  } else {
    component[[".weight"]]
  }

  gap <- own_frame_disagreement(supplied, realized)
  if (!is_null(gap)) {
    abort_samplyr(
      c(
        "A frame's own overlap value must be the selection that happened.",
        "x" = "{.field {col}} disagrees with {.val {frame}}'s design weights
               on {gap$n} row{?s}.",
        "i" = "Worst: {.val {signif(gap$supplied, 6)}} supplied against
               {.val {signif(gap$realized, 6)}} realized.",
        "i" = "The own-frame column describes this component's own selection,
               which samplyr already computed, so the two have to agree."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }
  invisible(NULL)
}

#' @noRd
check_membership_type <- function(samples, cols, call = caller_env()) {
  for (col in cols) {
    for (nm in names(samples)) {
      value <- samples[[nm]][[col]]
      if (is.logical(value)) {
        next
      }
      binary <- is.numeric(value) && all(value %in% c(0, 1, NA))
      abort_samplyr(
        c(
          "Membership columns must be {.cls logical}.",
          "x" = "{.field {col}} in {.val {nm}} is
                 {.cls {class(value)[[1]]}}.",
          if (binary) {
            c("i" = "Numeric {.val {0}} and {.val {1}} are refused rather than
                     read as membership: a number on this column is overlap
                     information on an undeclared scale.")
          },
          "i" = "Convert it with {.code as.logical()} once you have decided
                 which values mean membership."
        ),
        class = "samplyr_error_stack_frames_membership_type",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' Missing memberships, a false diagonal, and rows in no frame at all
#' @noRd
check_membership_complete <- function(samples, membership,
                                      call = caller_env()) {
  frames <- names(samples)

  for (nm in frames) {
    m <- frame_component_membership(samples[[nm]], membership)
    unresolved <- colnames(m)[apply(m, 2L, anyNA)]
    if (length(unresolved) > 0) {
      abort_samplyr(
        c(
          "Membership must be resolved for every sampled unit.",
          "x" = "{.val {nm}} has missing membership for
                 {.val {unresolved}}.",
          "i" = "An unknown membership is not a domain. Resolve it against the
                 registers before stacking."
        ),
        class = "samplyr_error_stack_frames_membership_missing",
        call = call
      )
    }

    own <- m[, nm]
    if (!all(own)) {
      abort_samplyr(
        c(
          "A component must belong to its own frame.",
          "x" = "{sum(!own)} row{?s} of {.val {nm}} {?is/are} false on
                 {.field {membership[[nm]]}}.",
          "i" = "The diagonal says the unit was reachable through the frame it
                 was selected from, so a false entry means the column and the
                 frame do not describe the same register."
        ),
        class = "samplyr_error_stack_frames_membership_diagonal",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' @noRd
check_frame_key_columns <- function(samples, key, call = caller_env()) {
  frames <- names(samples)

  without <- frames[!vapply(samples, function(s) key %in% names(s),
                            logical(1))]
  if (length(without) > 0) {
    abort_samplyr(
      c(
        "{.arg key} must be a column of every component.",
        "x" = "{.field {key}} is missing from {.val {without}}."
      ),
      class = "samplyr_error_stack_frames_key",
      call = call
    )
  }

  reference <- samples[[1]][[key]]
  for (nm in frames[-1]) {
    value <- samples[[nm]][[key]]
    if (!share_key_types_compatible(reference, value)) {
      abort_samplyr(
        c(
          "{.arg key} must hold the same kind of value in every component.",
          "x" = "{.field {key}} is {.cls {class(reference)[[1]]}} in
                 {.val {frames[[1]]}} and {.cls {class(value)[[1]]}} in
                 {.val {nm}}.",
          "i" = "Keys are compared as values, so a number and the text of that
                 number are different units."
        ),
        class = "samplyr_error_stack_frames_key",
        call = call
      )
    }
  }

  for (nm in frames) {
    value <- samples[[nm]][[key]]
    if (anyNA(value)) {
      abort_samplyr(
        c(
          "{.arg key} must be known for every sampled unit.",
          "x" = "{.val {nm}} has {sum(is.na(value))} missing
                 {.field {key}} value{?s}.",
          "i" = "A unit with no key cannot be recognized in another frame."
        ),
        class = "samplyr_error_stack_frames_key",
        call = call
      )
    }

    # WR uniqueness applies to selection occurrences.
    occurrence <- grep("^\\.draw_", names(samples[[nm]]), value = TRUE)
    columns <- c(key, occurrence)
    frame <- as.data.frame(samples[[nm]])[columns]
    if (anyDuplicated(frame) > 0) {
      abort_samplyr(
        c(
          "{.arg key} must identify a unit once within a frame.",
          "x" = "{.val {nm}} repeats {sum(duplicated(frame))}
                 {.field {key}} value{?s}.",
          if (length(occurrence) > 0) {
            c("i" = "This component replicates rows with replacement, so the
                     key was checked together with {.field {occurrence}}.")
          },
          "i" = "The same unit may appear in several frames, and does so on
                 one row per frame. Twice in one frame is a duplicated
                 register entry."
        ),
        class = "samplyr_error_stack_frames_key",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' One recorded seed used twice is a detectable independence failure
#'
#' Lohr's assumption A3 is that the samples were selected independently. It
#' cannot be verified, but the common-random-number version of breaking it
#' leaves a record, because samplyr stores the seed each execution used.
#' @noRd
warn_reused_frame_seeds <- function(samples, call = caller_env()) {
  seeds <- vapply(samples, function(s) {
    seed <- attr(s, "seed")
    if (is_null(seed)) NA_character_ else as.character(seed)
  }, character(1))

  shared <- unique(seeds[!is.na(seeds) & duplicated(seeds)])
  if (length(shared) == 0) {
    return(invisible(NULL))
  }

  frames <- vapply(shared, function(value) {
    paste(names(seeds)[!is.na(seeds) & seeds == value], collapse = " and ")
  }, character(1))

  cli_warn(
    c(
      "Frames selected from the same seed are not independent samples.",
      "x" = "Seed {.val {shared}}: {frames}.",
      "i" = "Give each frame its own seed. Distinct seeds are not evidence of
             independence; this detects one recorded mistake and nothing
             more.",
      "i" = "A component executed with {.code seed = NULL} records none and is
             not compared."
    ),
    class = "samplyr_warning_frame_seed_reused",
    call = call
  )
  invisible(NULL)
}

## Membership and domains

#' The K membership indicators of one component, as a matrix
#'
#' Built from the validated columns on demand rather than stored, so the
#' components stay the only place membership lives. Constructed explicitly
#' because `vapply()` returns a vector rather than a matrix for a one-row
#' component.
#' @noRd
frame_component_membership <- function(component, membership) {
  column_matrix(
    lapply(unname(membership), function(col) as.logical(component[[col]])),
    nrow(component), names(membership)
  )
}

#' One column per frame, as a matrix, whatever the component's height
#'
#' Constructed explicitly rather than with `vapply()`, which returns a vector
#' rather than a matrix for a one-row component. Three callers built the same
#' shape by hand.
#' @noRd
column_matrix <- function(values, n_rows, frames) {
  matrix(
    unlist(values, use.names = FALSE),
    nrow = n_rows,
    ncol = length(frames),
    dimnames = list(NULL, frames)
  )
}

#' One component's declared overlaps, always as probabilities
#'
#' Built from the columns on demand, like the membership matrix, so the
#' components stay the only place the values live. Normalizing here is what
#' lets the two markers be interchangeable: a weight form and a probability
#' form of the same design produce the same matrix. Zero marks a frame the
#' unit does not belong to, which is the absence survey's own formula reads.
#' @noRd
frame_component_overlaps <- function(x, nm) {
  overlaps <- attr(x, "overlaps")
  if (is_resolved_overlaps(overlaps)) {
    return(overlaps$resolved[[nm]])
  }
  component <- x[[nm]]
  membership <- attr(x, "membership")
  frames <- names(overlaps$cols)
  member <- frame_component_membership(component, membership)
  values <- lapply(frames, function(q) {
    value <- as.numeric(component[[overlaps$cols[[q]]]])
    value[!member[, q]] <- 0
    if (identical(overlaps$scale, "weights")) {
      value <- ifelse(member[, q], 1 / value, 0)
    }
    value
  })
  column_matrix(values, nrow(component), frames)
}

#' The canonical label of each row's domain
#'
#' The frames of a domain are joined in C-locale byte order, so the label is
#' invariant to the order the components were given in and to the session
#' locale. Distinct labels number at most `2^K - 1`, so they are resolved once
#' per domain rather than once per row.
#' @noRd
frame_domain_labels <- function(m) {
  frames <- colnames(m)
  ord <- order(frames, method = "radix")
  m <- m[, ord, drop = FALSE]
  frames <- frames[ord]

  if (nrow(m) == 0L) {
    return(character(0))
  }

  codes <- do.call(paste0, lapply(seq_len(ncol(m)), function(j) {
    as.integer(m[, j])
  }))
  present <- unique(codes)
  labels <- vapply(present, function(code) {
    paste(
      frames[strsplit(code, "", fixed = TRUE)[[1]] == "1"],
      collapse = frame_domain_separator
    )
  }, character(1))
  unname(labels[match(codes, present)])
}

## Conversion

#' Row-bind a frame stack for inspection, not for estimation
#'
#' @description
#' Returns a plain data frame with every component's rows, a `.frame` column
#' naming the component a row came from, and a `.domain` column naming the set
#' of frames the unit belongs to. It is the view to look at, join against, or
#' hand to another package.
#'
#' It is deliberately not a `tbl_sample` and not a `frame_stack`. A unit
#' listed in two frames may appear on two rows carrying two different design
#' weights, so summing the weight column estimates nothing. Being a plain data
#' frame is what stops it reaching [as_svydesign()] as though it were one
#' sample. Restoring a class attribute does not turn it back into a stack, and
#' modifying it does not touch the components.
#'
#' `.domain` is a label for reading. Statistical code uses the membership
#' columns, which are carried through unchanged, and never parses it back.
#'
#' @param x A `frame_stack`.
#' @param ... Not used.
#'
#' @return A data frame with `.frame` and `.domain` first, then the columns of
#'   the components.
#'
#' @examples
#' population <- data.frame(
#'   person_id = 1:60,
#'   in_landline = rep(c(TRUE, FALSE), times = c(40, 20)),
#'   in_cell = rep(c(FALSE, TRUE), times = c(10, 50))
#' )
#'
#' frames <- stack_frames(
#'   landline = sampling_design() |>
#'     draw(n = 10) |>
#'     execute(population[population$in_landline, ], seed = 1),
#'   cell = sampling_design() |>
#'     draw(n = 12) |>
#'     execute(population[population$in_cell, ], seed = 2),
#'   membership = c(landline = "in_landline", cell = "in_cell"),
#'   key = person_id
#' )
#'
#' view <- as.data.frame(frames)
#' table(view$.domain)
#'
#' @family multiple frames
#' @export
as.data.frame.frame_stack <- function(x, ...) {
  rlang::check_dots_empty()
  membership <- attr(x, "membership")

  parts <- lapply(names(x), function(nm) {
    part <- as.data.frame(x[[nm]])
    cbind(
      .frame = rep(nm, nrow(part)),
      .domain = frame_domain_labels(
        frame_component_membership(part, membership)
      ),
      part,
      stringsAsFactors = FALSE
    )
  })
  out <- as.data.frame(dplyr::bind_rows(parts))
  rownames(out) <- NULL
  out
}

#' How many rows each domain holds, in canonical label order
#' @noRd
frame_domain_counts <- function(x) {
  membership <- attr(x, "membership")
  labels <- unlist(lapply(x, function(component) {
    frame_domain_labels(frame_component_membership(component, membership))
  }), use.names = FALSE)
  counts <- table(labels)
  counts[order(names(counts), method = "radix")]
}
