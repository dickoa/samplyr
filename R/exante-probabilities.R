
## Exact selection probabilities, resolved without drawing

# Resolve exact cross-frame chances with execution's allocation machinery.
# Frame digests cannot serve because they may bin chances under internal keys.

#' Selection probabilities a design would give a register, without drawing
#'
#' @description
#' Returns the probability that each unit of a register would be selected,
#' computed from the design rather than observed from a sample. It draws no
#' random numbers, and it needs no execution.
#'
#' The intended use is a survey covering one population through several
#' registers, where the expected multiframe estimator needs a unit's chance in
#' the frames it was *not* selected from. See [exante_overlaps()], which
#' resolves those chances for a whole [stack_frames()] collection.
#'
#' @details
#' The probability is compounded across stages: for a two-stage design it is
#' the chance the unit's cluster is selected times the chance the unit is
#' selected within it. The stage-by-stage quantities come from the same
#' allocation and chance resolvers execution uses, so a sample's own weights
#' reproduce these numbers exactly.
#'
#' ## What it refuses
#'
#' A with-replacement or minimum-replacement stage, because the quantity there
#' is an expected number of hits rather than an inclusion probability, and a
#' method whose registered probabilities are `"unknown"`. It also refuses a
#' design given several registers, one per stage: the compounding runs along
#' the rows of one register and there is no correspondence between the rows of
#' two.
#'
#' [frame_summary()] answers a different question. It reports the pools and
#' chances a design resolves, at whatever resolution the digest retained, and
#' returns no unit rows at all where that was a quantile summary.
#'
#' @param design A `sampling_design` with a complete [draw()] on every stage.
#' @param frame The register, a data frame.
#' @param ... Must be empty. Arguments after it are matched by exact name.
#' @param key A bare column of `frame` identifying the population unit. It
#'   must be unique.
#'
#' @return A tibble with the key column and `probability`, one row per unit,
#'   in the register's own order.
#'
#' @references
#' Lohr, S. L. (2021). Multiple-frame surveys for a multiple-data-source
#' world. *Survey Methodology*, 47(2), 229-263.
#'
#' @examples
#' register <- data.frame(
#'   person_id = 1:40,
#'   size = rep(c(2, 5, 3, 8), times = 10)
#' )
#'
#' design <- sampling_design() |>
#'   draw(n = 10, method = "pps_brewer", mos = size)
#'
#' head(exante_probabilities(design, register, key = person_id))
#'
#' # The design's own sample carries the same numbers, as 1 / .weight.
#' sample <- execute(design, register, seed = 1)
#' resolved <- exante_probabilities(design, register, key = person_id)
#' all.equal(
#'   resolved$probability[match(sample$person_id, resolved$person_id)],
#'   1 / sample$.weight
#' )
#'
#' @seealso [exante_overlaps()] for resolving a whole stack,
#'   [frame_summary()] for the pool-level report
#'
#' @family multiple frames
#' @export
exante_probabilities <- function(design, frame, ..., key) {
  check_keyword_args(enquos(...), "key")
  key_col <- parse_frame_key(
    rlang::enquo(key),
    class = "samplyr_error_exante_key"
  )
  resolved <- resolve_exante_probabilities(design, frame, key_col)
  result <- tibble::tibble(
    key = resolved$keys,
    probability = resolved$probability
  )
  names(result)[[1]] <- key_col
  result
}

#' The compound probability of every row of one register
#'
#' Returns the key values and the probabilities separately, so the callers
#' that join on the key do not have to take the column name apart again.
#' @noRd
resolve_exante_probabilities <- function(design, frame, key_col,
                                         frame_label = NULL,
                                         call = caller_env()) {
  check_exante_design_complete(design, call = call)

  supplied <- normalize_frame_input(frame, call = call)
  if (supplied$n_supplied > 1L) {
    abort_samplyr(
      c(
        "{.fn exante_probabilities} resolves one register.",
        "x" = "{supplied$n_supplied} were given, one per stage.",
        "i" = "The probability is compounded along the rows of a register,
               and the rows of two registers do not correspond."
      ),
      class = "samplyr_error_exante_unsupported",
      call = call
    )
  }
  check_frames_executable(
    supplied$frames,
    labels = supplied$labels,
    allow_generated = FALSE,
    allow_stripped = FALSE,
    require_rows = FALSE,
    call = call
  )

  register <- supplied$frames[[1]]
  keys <- check_exante_key(register, key_col, frame_label, call = call)

  schedule <- stage_frame_schedule(
    design, supplied$frames, stages = NULL, executed = NULL, call = call
  )
  stage_frames <- effective_register_frames(schedule, design, call = call)
  frames_by_stage <- vector("list", length(design$stages))
  for (i in seq_along(schedule$entries)) {
    frames_by_stage[[schedule$entries[[i]]$stage]] <- stage_frames[[i]]
  }

  probability <- rep(1, nrow(register))
  registry <- NULL
  for (stage_idx in seq_along(design$stages)) {
    stage_frame <- frames_by_stage[[stage_idx]]
    resolved <- tryCatch(
      resolve_exante_pools(design, stage_idx, stage_frame, registry),
      error = function(e) {
        if (inherits(e, "samplyr_error")) {
          stop(e)
        }
        abort_samplyr(
          c(
            "Stage {stage_idx} has no resolvable inclusion probabilities.",
            "x" = "{conditionMessage(e)}"
          ),
          class = "samplyr_error_exante_unsupported",
          call = call
        )
      }
    )
    check_exante_stage_resolvable(resolved, stage_idx, call = call)
    probability <- probability * exante_stage_chances(resolved, nrow(register))
    registry <- exante_parent_registry(resolved)
  }

  list(keys = keys, probability = probability)
}

#' @noRd
check_exante_design_complete <- function(design, call = caller_env()) {
  if (!is_sampling_design(design)) {
    abort_samplyr(
      "{.arg design} must be a {.cls sampling_design}.",
      class = "samplyr_error_exante_unsupported",
      call = call
    )
  }
  incomplete <- length(design$stages) == 0 ||
    any(vapply(design$stages, function(s) is_null(s$draw_spec), logical(1)))
  if (incomplete) {
    abort_samplyr(
      c(
        "The design has no complete {.fn draw} specification.",
        "i" = "Probabilities are resolved from the design, so every stage
               needs a {.fn draw}."
      ),
      class = "samplyr_error_exante_unsupported",
      call = call
    )
  }
  invisible(NULL)
}

#' @noRd
check_exante_key <- function(register, key_col, frame_label = NULL,
                             call = caller_env()) {
  where <- frame_label %||% "the register"
  if (!key_col %in% names(register)) {
    abort_samplyr(
      c(
        "{.arg key} must be a column of {where}.",
        "x" = "{.field {key_col}} is not one."
      ),
      class = "samplyr_error_exante_key",
      call = call
    )
  }
  keys <- register[[key_col]]
  if (anyNA(keys)) {
    abort_samplyr(
      c(
        "{.arg key} must be known for every unit of the register.",
        "x" = "{.field {key_col}} has {sum(is.na(keys))} missing value{?s}."
      ),
      class = "samplyr_error_exante_key",
      call = call
    )
  }
  if (anyDuplicated(keys) > 0) {
    abort_samplyr(
      c(
        "{.arg key} must identify a unit once.",
        "x" = "{.field {key_col}} repeats {sum(duplicated(keys))} value{?s}.",
        "i" = "A register enumerates the population, so a repeated key is two
               rows for one unit and the probability would be ambiguous."
      ),
      class = "samplyr_error_exante_key",
      call = call
    )
  }
  keys
}

#' The two design properties that leave a stage with no probability to resolve
#' @noRd
check_exante_stage_resolvable <- function(resolved, stage_idx,
                                          call = caller_env()) {
  if (resolved$wr) {
    abort_samplyr(
      c(
        "Stage {stage_idx} has no inclusion probabilities to resolve.",
        "x" = "It selects with replacement, so a unit's recorded chance is
               an expected number of hits.",
        "i" = "An expected hit count is not a probability and does not
               compound with one."
      ),
      class = "samplyr_error_exante_unsupported",
      call = call
    )
  }
  # Keep a guard for designs that bypassed `draw()`.
  invisible(NULL)
}

#' One chance per register row, from the pools of one stage
#'
#' With one register every row belongs to some pool, at every stage: the
#' parent groups of a later stage partition the register. The initial `NA` is
#' therefore not a default for anything, it is what would surface if that ever
#' stopped being true, and an `NA` probability is visible where a `1` would
#' quietly claim certainty.
#' @noRd
exante_stage_chances <- function(resolved, n_rows) {
  chances <- rep(NA_real_, n_rows)
  for (pool in resolved$pools) {
    chances[pool$rows] <- pool$chance[pool$row_units]
  }
  chances
}

#' The parent identity the next stage groups by
#'
#' `resolve_exante_pools()` maps a row to its parent through the ancestry key
#' of the stage being built, so what has to be registered is one id per
#' cluster key of the stage just resolved.
#' @noRd
exante_parent_registry <- function(resolved) {
  if (!resolved$is_cluster) {
    return(NULL)
  }
  keys <- unlist(lapply(resolved$pools, function(p) p$keys), use.names = FALSE)
  setNames(seq_along(keys), keys)
}

## Resolving a whole stack

#' Resolve overlap probabilities from the designs, instead of stating them
#'
#' @description
#' A declarative marker for [stack_frames()]'s `overlaps` argument. Instead of
#' naming columns that already hold the chance each unit had in every frame,
#' it names the registers, and samplyr resolves the chances from each
#' component's own design with [exante_probabilities()].
#'
#' This is the capability the expected multiframe estimator needs and that a
#' sample alone cannot supply: the probability a unit would have had in a
#' frame it was not selected from. It is exact, and it is checked against what
#' actually happened, because a component's own resolved chance has to equal
#' the design weight the execution produced.
#'
#' @param frames A named list of registers, one per frame, with the same names
#'   as the components of the stack. Each is the population the corresponding
#'   design would draw from.
#' @param ... Must be empty. Arguments after it are matched by exact name.
#' @param by A single named string matching the stack's key to the column
#'   holding it in the registers, in the same direction as a join:
#'   `by = c(person_id = "person_id")`.
#'
#' @return An object of class `samplyr_exante_overlap_spec`, for
#'   `stack_frames()`'s `overlaps` argument.
#'
#' @examples
#' exante_overlaps(
#'   frames = list(
#'     area = data.frame(person_id = 1:10),
#'     list = data.frame(person_id = 5:14)
#'   ),
#'   by = c(person_id = "person_id")
#' )
#'
#' @seealso [exante_probabilities()] for one register,
#'   [declared_overlaps()] for naming columns that already hold them
#'
#' @family multiple frames
#' @export
exante_overlaps <- function(frames, ..., by) {
  check_keyword_args(enquos(...), "by")
  if (missing(by)) {
    abort_samplyr(
      c(
        "{.arg by} must be given.",
        "i" = "Match the stack's key to the registers' column as a join
               does: {.code by = c(key_column = \"register_column\")}.",
        "i" = "It is never inferred: two tables sharing a column name is not
               evidence that the column means the same thing in both."
      ),
      class = "samplyr_error_stack_frames_overlaps"
    )
  }
  if (
    !is.list(frames) || length(frames) == 0L ||
      is_null(names(frames)) || !all(nzchar(names(frames))) ||
      !all(vapply(frames, is.data.frame, logical(1)))
  ) {
    abort_samplyr(
      c(
        "{.arg frames} must be a named list of registers, one per frame.",
        "i" = "{.code frames = list(area = area_register, list =
               list_register)}."
      ),
      class = "samplyr_error_stack_frames_overlaps"
    )
  }
  if (
    !is.character(by) || length(by) != 1L ||
      is_null(names(by)) || !nzchar(names(by)) || is.na(by) || !nzchar(by)
  ) {
    abort_samplyr(
      c(
        "{.arg by} must be a single named string.",
        "i" = "Match the stack's key to the registers' column as a join
               does: {.code by = c(key_column = \"register_column\")}."
      ),
      class = "samplyr_error_stack_frames_overlaps"
    )
  }

  structure(
    list(scale = "probabilities", frames = frames, by = by),
    class = "samplyr_exante_overlap_spec"
  )
}

#' @noRd
is_exante_overlap_spec <- function(x) {
  inherits(x, "samplyr_exante_overlap_spec")
}

#' Turn a resolution request into the matrices the estimator reads
#'
#' Every frame's register is resolved once, against the design of the
#' component that drew from it, and then read for every component. So a unit
#' selected from frame A gets its frame B chance from B's own design, which is
#' the quantity the expected estimator wants and the one a sample of A cannot
#' contain.
#' @noRd
resolve_exante_overlaps <- function(samples, spec, membership,
                                    call = caller_env()) {
  frames <- names(samples)
  extra <- setdiff(names(spec$frames), frames)
  absent <- setdiff(frames, names(spec$frames))
  if (length(extra) > 0 || length(absent) > 0) {
    abort_samplyr(
      c(
        "{.fn exante_overlaps} must name every frame exactly once.",
        if (length(absent) > 0) c("x" = "No register for {.val {absent}}."),
        if (length(extra) > 0) c("x" = "No component named {.val {extra}}.")
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }

  key_col <- names(spec$by)
  register_col <- unname(spec$by)
  missing_key <- frames[!vapply(samples, function(component) {
    key_col %in% names(component)
  }, logical(1))]
  if (length(missing_key) > 0) {
    abort_samplyr(
      c(
        "{.arg by} names a column the components do not have.",
        "x" = "{.field {key_col}} is missing from {.val {missing_key}}."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }

  resolved <- lapply(frames, function(nm) {
    resolve_exante_probabilities(
      get_design(samples[[nm]]),
      spec$frames[[nm]],
      register_col,
      frame_label = paste0("the register for frame \"", nm, "\""),
      call = call
    )
  })
  names(resolved) <- frames

  matrices <- lapply(frames, function(nm) {
    component <- samples[[nm]]
    member <- frame_component_membership(component, membership)
    values <- lapply(frames, function(q) {
      position <- match(component[[key_col]], resolved[[q]]$keys)
      unresolved <- member[, q] & is.na(position)
      if (any(unresolved)) {
        abort_samplyr(
          c(
            "A unit of a frame is missing from that frame's register.",
            "x" = "{sum(unresolved)} unit{?s} of {.val {nm}} belong{?s/} to
                   {.val {q}} and {?is/are} not in its register.",
            "i" = "The register must enumerate the frame, or the chance in
                   it cannot be resolved."
          ),
          class = "samplyr_error_stack_frames_overlaps",
          call = call
        )
      }
      ifelse(member[, q], resolved[[q]]$probability[position], 0)
    })
    column_matrix(values, nrow(component), frames)
  })
  names(matrices) <- frames

  # Check resolved own-frame chances against executed design weights.
  for (nm in frames) {
    check_exante_diagonal(samples[[nm]], matrices[[nm]][, nm], nm, call = call)
  }

  new_resolved_overlaps(matrices)
}

#' The resolved form of an overlap declaration
#'
#' A class rather than a shape. The resolved record used to be a bare list
#' told apart from a declared one by `is_null(x$cols)`, in three places, which
#' is a test on the absence of a field rather than on what the object is.
#' `cols` stays `NULL` so nothing that reads it has to change.
#' @noRd
new_resolved_overlaps <- function(matrices) {
  structure(
    list(scale = "probabilities", cols = NULL, resolved = matrices),
    class = "samplyr_resolved_overlaps"
  )
}

#' @noRd
is_resolved_overlaps <- function(x) {
  inherits(x, "samplyr_resolved_overlaps")
}

#' @noRd
check_exante_diagonal <- function(component, resolved, frame,
                                  call = caller_env()) {
  realized <- 1 / component[[".weight"]]
  gap <- own_frame_disagreement(resolved, realized)
  if (!is_null(gap)) {
    abort_samplyr(
      c(
        "A resolved own-frame chance must be the selection that happened.",
        "x" = "{.val {frame}} disagrees with its own design weights on
               {gap$n} row{?s}.",
        "i" = "Worst: {.val {signif(gap$supplied, 6)}} resolved against
               {.val {signif(gap$realized, 6)}} realized.",
        "i" = "The register given for a frame must be the population that
               frame's design drew from."
      ),
      class = "samplyr_error_stack_frames_overlaps",
      call = call
    )
  }
  invisible(NULL)
}
