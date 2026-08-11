## Rotation programs

# A rotating panel that replenishes spans more than one execution: a start-up
# master drawn from one frame vintage, and a refreshment cohort drawn from
# each later vintage. `rotation_program()` is the registry that links them and
# says which components are live when.
#
# Three things it deliberately does not do. It does not infer a cohort's entry
# from its first active wave, because a cohort may be drawn early and held in
# reserve, and because entry is what anchors a cohort's population vintage on
# the wave axis. It does not renumber or rewrite the cohorts, whose receipts
# stay exactly as their own executions wrote them: a cohort number is a fact
# about a program, not about an execution. And it does not combine weights
# across cohorts.
#
# The last of those is a statement about knowledge, not about caution. A
# union weight needs three things a registry cannot hold: whether a unit was
# in the population at each vintage, what chance it would have had in a draw
# it was not selected into, and how the draws depend on one another. The last
# is not recoverable from inclusion probabilities at all, since two
# executions with identical marginals can be independent or identical. It is
# also not enough to keep one row per unit: two frame vintages may disagree
# about that unit's auxiliary values, and under a with-replacement design
# repeated occurrences of one unit are legitimate draw occurrences rather
# than duplicates.

#' Link the cohorts of a replenishing rotating panel
#'
#' @description
#' A rotating panel that replenishes is drawn as a series of executions: a
#' start-up master against the first frame vintage, then one refreshment
#' cohort against each later vintage. `rotation_program()` records which
#' samples make up the program, when each entered, and which components are
#' live at each occasion, so that [execute()] can materialize a wave across
#' all of them.
#'
#' @details
#' ## Cohorts
#'
#' `cohorts` is a named list of executed samples. A cohort may be
#' *partitioned*, drawn with `panels` so that it carries `.panel` and a frozen
#' assignment, or *whole*, drawn without `panels`. A whole cohort has one
#' implicit panel numbered 1 covering all its rows; activating it is not a
#' subsample, so its weights are unchanged.
#'
#' ## Entry waves
#'
#' `entry_wave` names the occasion at which each cohort's vintage enters the
#' program. It is declared, never inferred from the schedule: a cohort may
#' be drawn early and held in reserve, so its first active wave is not
#' necessarily its entry. Entry is also what anchors which population a cohort
#' represents, which is what any later cross-vintage weighting depends on.
#'
#' ## Schedule
#'
#' `schedule` is a data frame with `cohort` (a name from `cohorts`), integer
#' `panel`, integer `wave`, and an optional logical `active`; a combination
#' left out is inactive. For a one-cohort program the `cohort` column may be
#' omitted. Waves are numbered from 1 without gaps. Activity need not be
#' contiguous: a 4-8-4 rotation deliberately leaves and re-enters.
#'
#' The schedule is completed to the full grid only after the registry is
#' known, because a cohort's available panels come from its own receipt rather
#' than from the schedule.
#'
#' ## What a wave returns
#'
#' `execute(program, wave = t)` returns a collection of samples with separate
#' receipts, one per live cohort, each carrying its own exact activation
#' factor. It is not row-bound and its weights are not combined.
#'
#' ## Overlap between cohorts is unknown, not assumed absent
#'
#' A program records when its cohorts are live. It does not record how they
#' relate. Whether a unit could have been drawn into more than one cohort
#' depends on facts no receipt carries: whether it was in the population at
#' each vintage, what chance it would have had in a draw it was not selected
#' into, and whether the draws were independent of one another. Two executions
#' with identical inclusion probabilities can be independent or identical
#' depending on how they were seeded and coordinated, and nothing stored can
#' tell them apart.
#'
#' So the weights of a wave are valid **within** a cohort, and combining them
#' across cohorts is left to the analyst, who may know things the program
#' does not.
#'
#' The common case where that knowledge is easy is a refreshment cohort drawn
#' from an **entrant register**, a frame holding only units that entered the
#' population since the previous vintage. No unit can appear on both frames,
#' so the cohorts are disjoint by construction, each unit belongs to exactly
#' one, and the component weights are already the right ones for the combined
#' live set. A refresher drawn from the *whole* population at a later vintage
#' is the other case: a unit present at both times had two chances of
#' selection, and combining then needs a union probability, which is a
#' multi-frame problem rather than a scheduling one.
#'
#' @param cohorts A named list of executed `tbl_sample` objects, one per
#'   cohort. Each must be complete, unmodified and a single replicate.
#' @param entry_wave Integer vector of entry occasions, named to match
#'   `cohorts`.
#' @param schedule A data frame declaring which components are active at which
#'   wave, as described in Details.
#'
#' @return A `rotation_program`.
#'
#' @examples
#' frame_1 <- data.frame(id = 1:200, value = rnorm(200))
#' # Two groups, each live for two of the three occasions.
#' startup_schedule <- data.frame(
#'   panel = rep(1:2, times = 3),
#'   wave = rep(1:3, each = 2),
#'   active = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
#' )
#' startup <- sampling_design() |>
#'   draw(n = 40) |>
#'   execute(frame_1, seed = 1, panels = startup_schedule)
#'
#' # A refreshment cohort drawn from the entrants of a later vintage.
#' entrants <- data.frame(id = 201:260, value = rnorm(60))
#' intake_2 <- sampling_design() |>
#'   draw(n = 12) |>
#'   execute(entrants, seed = 2)
#'
#' program <- rotation_program(
#'   cohorts = list(startup = startup, intake_2 = intake_2),
#'   entry_wave = c(startup = 1, intake_2 = 2),
#'   schedule = rbind(
#'     transform(startup_schedule, cohort = "startup"),
#'     data.frame(
#'       cohort = "intake_2", panel = 1, wave = 1:3,
#'       active = c(FALSE, TRUE, TRUE)
#'     )
#'   )
#' )
#' program
#'
#' execute(program, wave = 2)
#'
#' @seealso [execute()] for drawing a master and materializing a wave,
#'   `vignette("rotating-panels")` for the design taxonomy this fits
#'   into.
#' @family execution
#' @export
rotation_program <- function(cohorts, entry_wave, schedule) {
  call <- current_env()
  cohorts <- check_cohort_registry(cohorts)
  panels <- vapply(
    cohorts, function(x) cohort_panel_count(x, call = call), integer(1)
  )
  entry_wave <- check_entry_waves(entry_wave, names(cohorts))
  schedule <- normalize_program_schedule(schedule, panels, entry_wave)
  check_program_block_sizes(schedule, cohorts)

  structure(
    list(
      cohorts = cohorts,
      entry_wave = entry_wave,
      schedule = schedule,
      panels = panels,
      waves = sort(unique(schedule$wave))
    ),
    class = "rotation_program"
  )
}

#' @noRd
is_rotation_program <- function(x) {
  inherits(x, "rotation_program")
}

#' How many panels a cohort was partitioned into
#'
#' A cohort drawn without `panels` has one implicit panel covering all of it.
#' @noRd
cohort_panel_count <- function(sample, call = caller_env()) {
  record <- cohort_assignment(sample, "A rotation program", call = call)
  if (is_null(record)) 1L else as.integer(record$panels)
}

#' A cohort's assignment record, read under the version it states
#'
#' A program is built out of numbers taken from these records: the panel count
#' the schedule is completed against, and the `r_min` and `block_size` its
#' declared takes are checked against. Those mean what the recorded algorithm
#' and schema say they mean, so a cohort whose record this build cannot read
#' is refused where the program is declared rather than at the first wave it
#' materializes.
#' @noRd
cohort_assignment <- function(sample, what, call = caller_env()) {
  prepare_panel_record(
    attr(sample, "metadata")$panel_assignment,
    what,
    call = call
  )
}

#' Every cohort must be a finished sample of its own
#' @noRd
check_cohort_registry <- function(cohorts, call = caller_env()) {
  if (!is.list(cohorts) || length(cohorts) == 0L) {
    abort_samplyr(
      "{.arg cohorts} must be a non-empty named list of executed samples.",
      class = "samplyr_error_program_cohorts",
      call = call
    )
  }
  names <- names(cohorts)
  if (is_null(names) || any(!nzchar(names)) || anyDuplicated(names) > 0) {
    abort_samplyr(
      c(
        "{.arg cohorts} must be named, with one distinct name per cohort.",
        "i" = "The names are how the schedule refers to each cohort."
      ),
      class = "samplyr_error_program_cohort_names",
      call = call
    )
  }

  for (nm in names) {
    sample <- cohorts[[nm]]
    if (!is_tbl_sample(sample)) {
      abort_samplyr(
        "Cohort {.val {nm}} is not an executed sample.",
        class = "samplyr_error_program_cohorts",
        call = call
      )
    }
    check_single_replicate(sample, "rotation_program", call = call)
    check_sample_unmodified(sample, "rotation_program", call = call)

    metadata <- attr(sample, "metadata")
    if (!is_null(metadata$wave)) {
      abort_samplyr(
        c(
          "Cohort {.val {nm}} is a materialized wave, not a cohort.",
          "i" = "Register the sample the wave was materialized from."
        ),
        class = "samplyr_error_program_cohort_is_wave",
        call = call
      )
    }
    remaining <- setdiff(
      seq_along(get_design(sample)$stages),
      get_stages_executed(sample)
    )
    if (length(remaining) > 0) {
      abort_samplyr(
        c(
          "Cohort {.val {nm}} has unexecuted stages: {remaining}.",
          "i" = "A cohort enters a program once its design is complete."
        ),
        class = "samplyr_error_program_cohort_incomplete",
        call = call
      )
    }
  }
  cohorts
}

#' @noRd
check_entry_waves <- function(entry_wave, cohort_names, call = caller_env()) {
  ok <- is.numeric(entry_wave) &&
    !anyNA(entry_wave) &&
    is_integerish_numeric(entry_wave) &&
    all(entry_wave >= 1)
  if (!ok) {
    abort_samplyr(
      "{.arg entry_wave} must hold complete integers of 1 or more.",
      class = "samplyr_error_program_entry_values",
      call = call
    )
  }
  names <- names(entry_wave)
  if (is_null(names) || !setequal(names, cohort_names)) {
    abort_samplyr(
      c(
        "{.arg entry_wave} must name every cohort, and only those.",
        "i" = "Cohorts: {cohort_names}.",
        "x" = "Named: {names %||% character(0)}."
      ),
      class = "samplyr_error_program_entry_names",
      call = call
    )
  }
  out <- as.integer(entry_wave[cohort_names])
  names(out) <- cohort_names
  out
}

#' Complete and check a program schedule against its registry
#'
#' A program schedule cannot be validated on its own: which panels a cohort
#' has comes from that cohort's receipt, not from the schedule.
#' @noRd
normalize_program_schedule <- function(
  schedule,
  panels,
  entry_wave,
  call = caller_env()
) {
  cohort_names <- names(panels)
  if (!is.data.frame(schedule)) {
    abort_samplyr(
      "{.arg schedule} must be a data frame.",
      class = "samplyr_error_schedule_columns",
      call = call
    )
  }
  missing <- setdiff(c("panel", "wave"), names(schedule))
  if (length(missing) > 0) {
    abort_samplyr(
      c(
        "A program schedule needs a {.field panel} and a {.field wave}
         column.",
        "x" = "Missing: {.field {missing}}."
      ),
      class = "samplyr_error_schedule_columns",
      call = call
    )
  }

  cohort <- if ("cohort" %in% names(schedule)) {
    as.character(schedule$cohort)
  } else if (length(cohort_names) == 1L) {
    # The one-master shorthand: a schedule without a cohort column can only
    # mean the single cohort registered.
    rep(cohort_names, nrow(schedule))
  } else {
    abort_samplyr(
      c(
        "A schedule for more than one cohort needs a {.field cohort} column.",
        "i" = "Registered cohorts: {cohort_names}."
      ),
      class = "samplyr_error_schedule_columns",
      call = call
    )
  }

  panel <- schedule$panel
  wave <- schedule$wave
  active <- if ("active" %in% names(schedule)) {
    schedule$active
  } else {
    rep(TRUE, nrow(schedule))
  }

  check_schedule_integers(panel, "panel", "schedule", call = call)
  check_schedule_integers(wave, "wave", "schedule", call = call)
  if (!is.logical(active) || anyNA(active)) {
    abort_samplyr(
      "The {.field active} column of a schedule must be logical and
       complete.",
      class = "samplyr_error_schedule_active",
      call = call
    )
  }
  panel <- as.integer(panel)
  wave <- as.integer(wave)

  unknown <- setdiff(unique(cohort), cohort_names)
  if (length(unknown) > 0) {
    abort_samplyr(
      c(
        "The schedule names cohorts that are not registered.",
        "x" = "Unknown: {unknown}.",
        "i" = "Registered: {cohort_names}."
      ),
      class = "samplyr_error_program_schedule_cohort",
      call = call
    )
  }

  if (anyDuplicated(paste(cohort, panel, wave, sep = "|")) > 0) {
    abort_samplyr(
      c(
        "A schedule may declare each panel of each cohort once per wave.",
        "x" = "It repeats at least one {.field cohort}-{.field panel}-{.field
               wave} combination."
      ),
      class = "samplyr_error_schedule_duplicates",
      call = call
    )
  }

  n_waves <- max(wave)
  check_schedule_contiguous(wave, n_waves, "wave", "schedule", call = call)

  over <- panel > panels[cohort]
  if (any(over)) {
    at <- which(over)[1L]
    abort_samplyr(
      c(
        "The schedule activates a panel a cohort does not have.",
        "x" = "Cohort {.val {cohort[at]}} was drawn with
               {panels[[cohort[at]]]} panel{?s}, and the schedule names panel
               {panel[at]}."
      ),
      class = "samplyr_error_program_schedule_panel",
      call = call
    )
  }

  outside <- entry_wave > n_waves
  if (any(outside)) {
    abort_samplyr(
      c(
        "Every cohort must enter within the declared waves.",
        "x" = "The schedule declares {n_waves} wave{?s}, and
               {.val {names(entry_wave)[outside][1]}} enters at
               {entry_wave[outside][1]}."
      ),
      class = "samplyr_error_program_entry_range",
      call = call
    )
  }

  grid <- expand.grid(
    panel = seq_len(max(panels)),
    cohort = cohort_names,
    wave = seq_len(n_waves),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[grid$panel <= panels[grid$cohort], , drop = FALSE]
  grid <- grid[order(grid$wave, match(grid$cohort, cohort_names), grid$panel), ]
  grid <- grid[c("wave", "cohort", "panel")]
  rownames(grid) <- NULL

  at <- match(
    paste(grid$cohort, grid$panel, grid$wave, sep = "|"),
    paste(cohort, panel, wave, sep = "|")
  )
  grid$active <- !is.na(at) & active[at]

  early <- grid$active & grid$wave < entry_wave[grid$cohort]
  if (any(early)) {
    at <- which(early)[1L]
    abort_samplyr(
      c(
        "A cohort cannot be active before it enters.",
        "x" = "Cohort {.val {grid$cohort[at]}} enters at wave
               {entry_wave[[grid$cohort[at]]]} and is active at wave
               {grid$wave[at]}."
      ),
      class = "samplyr_error_program_activation_before_entry",
      call = call
    )
  }

  per_wave <- vapply(split(grid$active, grid$wave), sum, integer(1))
  if (any(per_wave == 0L)) {
    idle <- as.integer(names(per_wave)[per_wave == 0L])
    abort_samplyr(
      c(
        "Every declared wave must have at least one active component.",
        "x" = "Nothing is active at wave {idle}.",
        "i" = "A single cohort may be dormant at a wave; the program may
               not."
      ),
      class = "samplyr_error_schedule_idle_wave",
      call = call
    )
  }

  never <- vapply(
    split(grid$active, grid$cohort)[cohort_names],
    function(a) !any(a),
    logical(1)
  )
  if (any(never)) {
    abort_samplyr(
      c(
        "Every registered cohort must be active at some declared wave.",
        "x" = "Never active: {cohort_names[never]}.",
        "i" = "Either the schedule missed it or it does not belong to this
               program."
      ),
      class = "samplyr_error_program_idle_cohort",
      call = call
    )
  }

  grid
}

#' The block size a cohort was drawn with bounds what its schedule may ask
#'
#' Block size was frozen at the cohort's own draw from `r_min`, the fewest
#' panels its schedule then activated. A program that activates fewer than
#' that leaves under two units per block in the take, so the activation stops
#' carrying a within-block variance estimate while its weights stay perfectly
#' computable. That is the kind of loss worth refusing where it is declared
#' rather than discovering at export.
#' @noRd
check_program_block_sizes <- function(schedule, cohorts, call = caller_env()) {
  for (nm in names(cohorts)) {
    record <- cohort_assignment(cohorts[[nm]], "A rotation program", call = call)
    if (is_null(record) || is_null(record$r_min)) {
      next
    }
    rows <- schedule[schedule$cohort == nm, , drop = FALSE]
    per_wave <- vapply(split(rows$active, rows$wave), sum, integer(1))
    live <- per_wave[per_wave > 0L]
    short <- live[live < record$r_min]
    if (length(short) > 0) {
      wave <- as.integer(names(short)[1L])
      abort_samplyr(
        c(
          "The schedule activates fewer panels of a cohort than its blocks
           were built for.",
          "x" = "Cohort {.val {nm}} activates {short[[1]]} panel{?s} at wave
                 {wave}, against the {record$r_min} its assignment assumed.",
          "i" = "Block size was frozen at that cohort's draw as
                 {record$block_size}. A smaller take leaves under two units
                 per block, so the activation carries no within-block
                 variance estimate.",
          "i" = "Redraw the cohort with a schedule whose leanest wave matches
                 this one, or activate at least {record$r_min} panels."
        ),
        class = "samplyr_error_program_block_size",
        call = call
      )
    }
  }
  invisible(NULL)
}

## Materializing a program wave

#' Materialize one wave across every live cohort
#' @noRd
materialize_program_wave <- function(
  program,
  wave,
  frames,
  stages,
  seed,
  panels,
  panel_stage = NULL,
  small_pool = NULL,
  reps,
  execution_environment,
  call = caller_env()
) {
  supplied <- c(
    if (length(frames) > 0) "a frame",
    if (!is_null(stages)) "stages",
    if (!is_null(seed)) "seed",
    if (!is_null(panels)) "panels",
    # Each cohort froze its own assignment stage and small-pool policy at its
    # draw, so a program wave cannot revisit either any more than a
    # single-master wave can.
    if (!is_null(panel_stage)) "panel_stage",
    if (!is_null(small_pool)) "small_pool",
    if (!is_null(reps)) "reps"
  )
  if (length(supplied) > 0) {
    abort_samplyr(
      c(
        "{.arg wave} selects components the program already assigned, so it
         takes no further execution input.",
        "x" = "Also given: {supplied}."
      ),
      class = "samplyr_error_wave_extra_arguments",
      call = call
    )
  }
  wave <- check_wave_declared(wave, program$schedule, call = call)

  schedule <- program$schedule
  digest <- rlang::hash(schedule)
  at_wave <- schedule[schedule$wave == wave, , drop = FALSE]

  live <- list()
  for (nm in names(program$cohorts)) {
    active <- at_wave$panel[at_wave$cohort == nm & at_wave$active]
    if (length(active) == 0L) {
      next
    }
    sample <- program$cohorts[[nm]]
    record <- cohort_assignment(sample, "An activation", call = call)
    live[[nm]] <- build_wave_sample(
      source = sample,
      record = record,
      wave = wave,
      activation = activate_cohort(sample, record, active, call = call),
      schedule_digest = digest,
      execution_environment = execution_environment,
      extra = list(
        cohort = nm,
        entry_wave = unname(program$entry_wave[[nm]])
      )
    )
  }

  new_rotation_wave(live, wave = wave, program = program)
}

#' A wave of a program is a collection, not a sample
#'
#' Each component carries its own receipt and its own exact activation
#' factor. They are deliberately not row-bound and their weights are
#' deliberately not combined: a unit eligible for two vintages had two
#' chances of selection, and the union probability needs eligibility
#' information no receipt carries.
#' @noRd
new_rotation_wave <- function(cohorts, wave, program) {
  structure(
    cohorts,
    wave = wave,
    entry_wave = program$entry_wave[names(cohorts)],
    schedule_digest = rlang::hash(program$schedule),
    class = "rotation_wave"
  )
}

#' @noRd
is_rotation_wave <- function(x) {
  inherits(x, "rotation_wave")
}

## Conversion

#' Row-bind a rotation wave for fieldwork, not for estimation
#'
#' The result is a plain data frame carrying a `.cohort` column, so it can be
#' used as the list of units to visit. It is deliberately not a `tbl_sample`:
#' the weights it carries are each cohort's own, the row set may hold a unit
#' once per cohort that drew it, and whether the cohorts overlap at all is
#' something the program does not know. Being a plain data frame is what
#' stops it reaching [as_svydesign()] as though it were one sample.
#'
#' @param x A rotation wave.
#' @param ... Not used.
#' @return A data frame.
#' @export
as.data.frame.rotation_wave <- function(x, ...) {
  rlang::check_dots_empty()
  if (length(x) == 0L) {
    return(data.frame())
  }
  parts <- lapply(names(x), function(nm) {
    part <- as.data.frame(x[[nm]])
    cbind(.cohort = rep(nm, nrow(part)), part, stringsAsFactors = FALSE)
  })
  out <- as.data.frame(dplyr::bind_rows(parts))
  rownames(out) <- NULL
  out
}

#' @export
as_svydesign.rotation_wave <- function(x, ...) {
  abort_rotation_wave_export("as_svydesign")
}

#' @export
as_svrepdesign.rotation_wave <- function(x, ...) {
  abort_rotation_wave_export("as_svrepdesign")
}

#' @noRd
abort_rotation_wave_export <- function(fn_name, call = caller_env()) {
  abort_samplyr(
    c(
      "{.fn {fn_name}} cannot take a rotation wave, which is a collection of
       samples rather than one sample.",
      "i" = "Its cohorts come from different frame vintages, so combining
             them needs the probability that a unit was selected into at
             least one, which no receipt carries.",
      "i" = "Export a component instead: {.code as_svydesign(wave[[\"<cohort
             name>\"]])}, once the activation phase is supported."
    ),
    class = "samplyr_error_rotation_wave_not_combinable",
    call = call
  )
}
