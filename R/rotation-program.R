## Rotation programs

#' Link the selections of a replenishing rotating panel
#'
#' @description
#' A rotating panel that replenishes is drawn as a series of executions: a
#' start-up master against the first frame vintage, then one refreshment
#' selection against each later vintage. `rotation_program()` records which
#' samples make up the program, when each entered, and which components are
#' live at each occasion, so that [execute()] can materialize an occasion
#' across all of them.
#'
#' @details
#' ## Selections
#'
#' `selections` is a named list of executed samples, one per selection of
#' units from a frame. A selection may be *partitioned*, drawn with `panels`
#' so that it carries `.panel` and a frozen assignment, or *whole*, drawn
#' without `panels`. A whole selection has one implicit panel numbered 1
#' covering all its rows. Activating it is not a subsample, so its weights
#' are unchanged.
#'
#' ## Entry periods
#'
#' With a data-frame schedule, `entry_period` names the period at which each
#' selection's vintage enters the program. It is declared rather than
#' inferred from first activity because a selection may be drawn early and
#' held in reserve. An `svyplan_schedule` already records every entry
#' period, so it supplies this field and redundant `entry_period` input is
#' refused.
#'
#' ## Schedule
#'
#' `schedule` is a data frame with `selection` (a name from `selections`),
#' integer `panel`, integer `period`, an optional integer `subround` and an
#' optional logical `active`, read as a `panels` schedule is (see
#' [panel_assignment]). A combination left out is inactive. For a program
#' of one selection the `selection` column may be omitted. Activity need not
#' be contiguous: a 4-8-4 rotation deliberately leaves and re-enters.
#'
#' The schedule is completed to the full grid only after the registry is
#' known, because a selection's available panels come from its own receipt
#' rather than from the schedule.
#'
#' ## Planning schedules
#'
#' An `svyplan_schedule` registers the program as of `through`. Exactly the
#' selections of `plan$selections` entering by then must be
#' supplied, under the names the plan gives them (`startup`, `period_2`,
#' ...). Later selections remain planned but unfielded. The executed
#' receipts determine realized panel and issue counts, which are checked
#' against the plan.
#'
#' The program's occasions are the plan's periods and subrounds, so a plan
#' with `horizon = 8` and `subrounds = 3` has 24. `through = 2` registers
#' through the last subround of period 2, and
#' `through = c(period = 2, subround = 1)` through its first.
#' `execute(program, period = 2, subround = 1)` materializes one occasion.
#' Each selection is drawn with the number of panels the plan gives it in
#' `plan$selections$panels`. With
#' `subrounds = 3`, a selection after the start-up has one panel per
#' subround and is drawn with `panels = 3`. Drawn whole, it is refused
#' (`samplyr_error_program_plan_panels`).
#'
#' ## What an occasion returns
#'
#' `execute(program, period = p, subround = s)` returns a collection of
#' samples with separate receipts, one per live selection, each carrying its
#' own exact activation factor, of class `rotation_occasion`. It is not
#' row-bound and its weights are not combined. A program materializes one
#' occasion at a time.
#'
#' ## Overlap between selections is unknown, not assumed absent
#'
#' A program records when its selections are live. It does not record how
#' they relate. Whether a unit could have been drawn into more than one
#' selection depends on facts no receipt carries: whether it was in the
#' population at each vintage, what chance it would have had in a draw it
#' was not selected into, and whether the draws were independent of one
#' another. Two executions with identical inclusion probabilities can be
#' independent or identical depending on how they were seeded and
#' coordinated, and nothing stored can tell them apart.
#'
#' So the weights of an occasion are valid **within** a selection, and combining
#' them across selections is left to the analyst, who may know things the
#' program does not.
#'
#' The common case where that knowledge is easy is a refreshment selection
#' drawn from an **entrant register**, a frame holding only units that
#' entered the population since the previous vintage. No unit can appear on
#' both frames, so the selections are disjoint by construction, each unit
#' belongs to exactly one, and the component weights are already the right
#' ones for the combined live set. A refresher drawn from the *whole*
#' population at a later vintage is the other case: a unit present at both
#' times had two chances of selection, and combining then needs a union
#' probability, which is a multi-frame problem rather than a scheduling one.
#'
#' @param selections A named list of executed `tbl_sample` objects, one per
#'   selection. Each must be complete, unmodified and a single replicate.
#' @param entry_period Integer vector of entry periods, named to match
#'   `selections`. Required with a data-frame `schedule` and omitted with an
#'   `svyplan_schedule`.
#' @param schedule A data frame declaring which components are active at which
#'   occasion, or an `svyplan_schedule` from [svyplan::design_schedule()].
#' @param through Last period to register from an `svyplan_schedule`, or a
#'   period and a subround, `c(period = p, subround = s)`. Required for that
#'   route and unused with a data-frame schedule.
#'
#' @return A `rotation_program`, an object whose fields are internal.
#'   [execute()] with `period` returns an occasion of it, a list of samples
#'   with one element per active selection.
#'
#' @examples
#' frame_1 <- data.frame(id = 1:200, value = rnorm(200))
#' # Two groups, each live for two of the three occasions.
#' startup_schedule <- data.frame(
#'   panel = rep(1:2, times = 3),
#'   period = rep(1:3, each = 2),
#'   active = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
#' )
#' startup <- sampling_design() |>
#'   draw(n = 40) |>
#'   execute(frame_1, seed = 1, panels = startup_schedule)
#'
#' # A refreshment selection drawn from the entrants of a later vintage.
#' entrants <- data.frame(id = 201:260, value = rnorm(60))
#' period_2 <- sampling_design() |>
#'   draw(n = 12) |>
#'   execute(entrants, seed = 2)
#'
#' program <- rotation_program(
#'   selections = list(startup = startup, period_2 = period_2),
#'   entry_period = c(startup = 1, period_2 = 2),
#'   schedule = rbind(
#'     transform(startup_schedule, selection = "startup"),
#'     data.frame(
#'       selection = "period_2", panel = 1, period = 1:3,
#'       active = c(FALSE, TRUE, TRUE)
#'     )
#'   )
#' )
#' program
#'
#' execute(program, period = 2)
#'
#' @seealso [execute()] for drawing a master and materializing an occasion,
#'   [panel_assignment] for partitioning one execution into rotation groups.
#' @family execution
#' @export
rotation_program <- function(selections, entry_period = NULL,
                             schedule = NULL, through = NULL) {
  call <- current_env()
  selections <- check_selection_registry(selections, call = call)
  panels <- vapply(
    selections, function(x) selection_panel_count(x, call = call), integer(1)
  )
  if (is_svyplan_schedule(schedule)) {
    if (!is_null(entry_period)) {
      abort_samplyr(
        c(
          "{.arg entry_period} is supplied by the {.cls svyplan_schedule}.",
          "i" = "Remove the redundant argument."
        ),
        class = "samplyr_error_program_plan_argument",
        call = call
      )
    }
    plan <- normalize_plan_program(
      schedule, selections, panels, through, call = call
    )
    entry_period <- plan$entry_period
    schedule <- plan$schedule
  } else {
    if (!is_null(through)) {
      abort_samplyr(
        "{.arg through} is used only with an {.cls svyplan_schedule}.",
        class = "samplyr_error_program_plan_argument",
        call = call
      )
    }
    entry_period <- check_entry_periods(entry_period, names(selections),
                                        call = call)
    schedule <- normalize_program_schedule(
      schedule, panels, entry_period, call = call
    )
  }
  check_program_block_sizes(schedule, selections, call = call)

  structure(
    list(
      selections = selections,
      entry_period = entry_period,
      schedule = schedule,
      panels = panels,
      occasions = program_occasions(schedule)
    ),
    class = "rotation_program"
  )
}

#' A program's occasions in order, one row each
#' @noRd
program_occasions <- function(schedule) {
  out <- unique(schedule[order(schedule$occasion), c("period", "subround")])
  rownames(out) <- NULL
  out
}

#' The last occasion a planning schedule registers, from `through`
#'
#' A period registers through its last subround, and `c(period = p,
#' subround = s)` through that subround.
#' @return The occasion number in the plan's order.
#' @noRd
plan_through_occasion <- function(plan, through, call = caller_env()) {
  horizon <- plan$horizon
  sub <- plan$subrounds
  ok <- is.numeric(through) && length(through) %in% 1:2 &&
    !anyNA(through) && is_integerish_numeric(through) && all(through >= 1) &&
    through[[1L]] <= horizon && (length(through) == 1L || through[[2L]] <= sub)
  if (!ok) {
    abort_samplyr(
      c(
        "{.arg through} must be one period from 1 to {horizon}, or a period
         and a subround, {.code c(period = p, subround = s)}.",
        if (sub > 1L) {
          c("i" = "Each period of this plan has {sub} subrounds.")
        }
      ),
      class = "samplyr_error_program_through",
      call = call
    )
  }
  period <- as.integer(through[[1L]])
  subround <- if (length(through) == 2L) as.integer(through[[2L]]) else sub
  (period - 1L) * sub + subround
}

#' Translate and reconcile a planning schedule
#' @noRd
normalize_plan_program <- function(plan, selections, panels, through,
                                   call = caller_env()) {
  check_svyplan_schedule(plan, call = call)
  through <- plan_through_occasion(plan, through, call = call)

  planned <- plan$selections
  entry <- as.integer((planned$entry_period - 1L) * plan$subrounds + 1L)
  planned <- planned[entry <= through, , drop = FALSE]
  entry <- entry[entry <= through]
  required <- planned$selection
  supplied <- names(selections)
  missing <- setdiff(required, supplied)
  extra <- setdiff(supplied, required)
  unknown <- setdiff(extra, plan$selections$selection)
  future <- setdiff(extra, unknown)
  if (length(missing) > 0L || length(extra) > 0L) {
    abort_samplyr(
      c(
        "The selection registry must match the plan through occasion
         {through}.",
        "x" = if (length(missing) > 0L) "Missing: {missing}." else NULL,
        "x" = if (length(future) > 0L) "Not due yet: {future}." else NULL,
        "x" = if (length(unknown) > 0L) "Not in the plan: {unknown}." else NULL
      ),
      class = "samplyr_error_program_plan_selections",
      call = call
    )
  }

  at <- match(supplied, planned$selection)
  planned_panels <- planned$panels[at]
  if (!identical(unname(panels), unname(planned_panels))) {
    abort_samplyr(
      c(
        "The executed selections do not have the panel counts in the plan.",
        "x" = "Planned: {setNames(planned_panels, supplied)}.",
        "x" = "Executed: {panels}."
      ),
      class = "samplyr_error_program_plan_panels",
      call = call
    )
  }

  selected <- vapply(
    selections,
    function(x) selection_issue_count(x, call = call),
    numeric(1)
  )
  planned_issue <- planned$operational_issue[at]
  mismatch <- selected != planned_issue
  if (any(mismatch)) {
    selection <- supplied[which(mismatch)[1L]]
    abort_samplyr(
      c(
        "The executed selection size does not match the operational plan.",
        "x" = "Selection {.val {selection}} planned
               {planned_issue[mismatch][1L]}
               units and selected {selected[mismatch][1L]}."
      ),
      class = "samplyr_error_program_plan_count",
      call = call
    )
  }

  entry_period <- as.integer(planned$entry_period[at])
  names(entry_period) <- supplied
  grid <- plan$schedule
  # The plan numbers panels across the schedule, an execution from 1.
  grid$panel <- stats::ave(grid$panel, grid$selection, FUN = function(p) {
    match(p, sort(unique(p)))
  })
  translated <- grid[
    svyplan_draw_occasions(plan, grid) <= through &
      grid$selection %in% supplied,
    c("selection", "panel", "period", "subround", "active")
  ]
  translated <- normalize_program_schedule(
    translated, panels, entry_period, call = call
  )
  list(entry_period = entry_period, schedule = translated)
}

#' Count the assignment units represented by a selection receipt
#'
#' The plan figure this is compared against is `operational_issue`, which
#' svyplan computes as `panels * panel_issue`: the units issued to the field,
#' not the rows they expand to. Both branches here have to count that same
#' thing.
#'
#' A selection drawn whole has no assignment record. Its unit is resolved the
#' way an assignment would resolve it, from the stage that would have carried
#' it, so a clustered selection counts clusters, not rows.
#' @noRd
selection_issue_count <- function(sample, call = caller_env()) {
  record <- selection_assignment(sample, "A rotation program", call = call)
  if (!is_null(record)) {
    return(sum(vapply(record$pools, function(pool) pool$size, numeric(1))))
  }

  # Default to the first executed assignment stage.
  executed <- get_stages_executed(sample)
  design <- get_design(sample)
  context <- panel_assignment_context(
    design, executed[[1L]], as.data.frame(sample), call = call
  )
  keys <- make_group_key(as.data.frame(sample), context$key_vars)
  as.double(length(unique(keys)))
}

#' @noRd
is_rotation_program <- function(x) {
  inherits(x, "rotation_program")
}

#' How many panels a selection was partitioned into
#'
#' A selection drawn without `panels` has one implicit panel covering all of it.
#' @noRd
selection_panel_count <- function(sample, call = caller_env()) {
  record <- selection_assignment(sample, "A rotation program", call = call)
  if (is_null(record)) 1L else as.integer(record$panels)
}

#' A selection's assignment record, read under the version it states
#'
#' A program is built out of numbers taken from these records: the panel count
#' the schedule is completed against, and the `r_min` and `block_size` its
#' declared takes are checked against. Those mean what the recorded algorithm
#' and schema say they mean, so a selection whose record this build cannot read
#' is refused where the program is declared rather than at the first occasion it
#' materializes.
#' @noRd
selection_assignment <- function(sample, what, call = caller_env()) {
  prepare_panel_record(
    attr(sample, "metadata")$panel_assignment,
    what,
    call = call
  )
}

#' Every selection must be a finished sample of its own
#' @noRd
check_selection_registry <- function(selections, call = caller_env()) {
  if (!is.list(selections) || length(selections) == 0L) {
    abort_samplyr(
      "{.arg selections} must be a non-empty named list of executed samples.",
      class = "samplyr_error_program_selections",
      call = call
    )
  }
  names <- names(selections)
  if (is_null(names) || any(!nzchar(names)) || anyDuplicated(names) > 0) {
    abort_samplyr(
      c(
        "{.arg selections} must be named, with one distinct name per
         selection.",
        "i" = "The names are how the schedule refers to each selection."
      ),
      class = "samplyr_error_program_selection_names",
      call = call
    )
  }

  for (nm in names) {
    sample <- selections[[nm]]
    if (!is_tbl_sample(sample)) {
      abort_samplyr(
        "Selection {.val {nm}} is not an executed sample.",
        class = "samplyr_error_program_selections",
        call = call
      )
    }
    check_single_replicate(sample, "rotation_program", call = call)
    check_sample_unmodified(sample, "rotation_program", call = call)
    check_weight_contract_panel(sample, "rotation_program", call = call)

    metadata <- attr(sample, "metadata")
    if (!is_null(metadata$occasion)) {
      abort_samplyr(
        c(
          "Selection {.val {nm}} is a materialized occasion, not a selection.",
          "i" = "Register the sample the occasion was materialized from."
        ),
        class = "samplyr_error_program_selection_is_occasion",
        call = call
      )
    }
    if (identical(metadata$panel_assignment$certainty, "assigned")) {
      abort_samplyr(
        c(
          "Selection {.val {nm}} is split into {.fn subrounds}, not rotation
           groups.",
          "i" = "A subround is visited once, and its certainty units are
                 assigned like the others rather than kept in every occasion.",
          "i" = "Draw the selection with {.code panels = <count>} or a schedule
                 to rotate it."
        ),
        class = "samplyr_error_program_subrounds",
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
          "Selection {.val {nm}} has unexecuted stages: {remaining}.",
          "i" = "A selection enters a program once its design is complete."
        ),
        class = "samplyr_error_program_selection_incomplete",
        call = call
      )
    }
  }
  selections
}

#' @noRd
check_entry_periods <- function(entry_period, selection_names,
                                call = caller_env()) {
  ok <- is.numeric(entry_period) &&
    !anyNA(entry_period) &&
    is_integerish_numeric(entry_period) &&
    all(entry_period >= 1)
  if (!ok) {
    abort_samplyr(
      "{.arg entry_period} must hold complete integers of 1 or more.",
      class = "samplyr_error_program_entry_values",
      call = call
    )
  }
  names <- names(entry_period)
  if (is_null(names) || !setequal(names, selection_names)) {
    abort_samplyr(
      c(
        "{.arg entry_period} must name every selection, and only those.",
        "i" = "Selections: {selection_names}.",
        "x" = "Named: {names %||% character(0)}."
      ),
      class = "samplyr_error_program_entry_names",
      call = call
    )
  }
  out <- as.integer(entry_period[selection_names])
  names(out) <- selection_names
  out
}

#' Complete and check a program schedule against its registry
#'
#' A program schedule cannot be validated on its own: which panels a selection
#' has comes from that selection's receipt, not from the schedule.
#' @noRd
normalize_program_schedule <- function(
  schedule,
  panels,
  entry_period,
  call = caller_env()
) {
  selection_names <- names(panels)
  subject <- schedule_subject("schedule")
  check_schedule_columns(schedule, subject, call = call)

  selection <- if ("selection" %in% names(schedule)) {
    as.character(schedule$selection)
  } else if (length(selection_names) == 1L) {
    # A selection-free schedule can name only one registered selection.
    rep(selection_names, nrow(schedule))
  } else {
    abort_samplyr(
      c(
        "A schedule for more than one selection needs a {.field selection}
         column.",
        "i" = "Registered selections: {selection_names}."
      ),
      class = "samplyr_error_schedule_columns",
      call = call
    )
  }

  panel <- schedule$panel
  active <- schedule_active_column(schedule, subject, call = call)

  check_schedule_integers(panel, "panel", "schedule", call = call)
  panel <- as.integer(panel)
  occasions <- schedule_occasions(schedule, subject, call = call)
  occasion <- occasions$occasion

  unknown <- setdiff(unique(selection), selection_names)
  if (length(unknown) > 0) {
    abort_samplyr(
      c(
        "The schedule names selections that are not registered.",
        "x" = "Unknown: {unknown}.",
        "i" = "Registered: {selection_names}."
      ),
      class = "samplyr_error_program_schedule_selection",
      call = call
    )
  }

  check_schedule_duplicates(
    paste(selection, panel, occasion, sep = "|"), subject,
    declares = "each panel of each selection once per period and subround",
    combination = cli::format_inline(
      "{.field selection}-{.field panel}-{.field period}-{.field subround}"
    ),
    call = call
  )

  n_occasions <- nrow(occasions$pairs)

  over <- panel > panels[selection]
  if (any(over)) {
    at <- which(over)[1L]
    abort_samplyr(
      c(
        "The schedule activates a panel a selection does not have.",
        "x" = "Selection {.val {selection[at]}} was drawn with
               {panels[[selection[at]]]} panel{?s}, and the schedule names panel
               {panel[at]}."
      ),
      class = "samplyr_error_program_schedule_panel",
      call = call
    )
  }

  n_periods <- max(occasions$pairs$period)
  outside <- entry_period > n_periods
  if (any(outside)) {
    abort_samplyr(
      c(
        "Every selection must enter within the declared periods.",
        "x" = "The schedule declares {n_periods} period{?s}, and
               {.val {names(entry_period)[outside][1]}} enters at period
               {entry_period[outside][1]}."
      ),
      class = "samplyr_error_program_entry_range",
      call = call
    )
  }

  grid <- expand.grid(
    panel = seq_len(max(panels)),
    selection = selection_names,
    occasion = seq_len(n_occasions),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[grid$panel <= panels[grid$selection], , drop = FALSE]
  grid <- grid[
    order(grid$occasion, match(grid$selection, selection_names), grid$panel),
  ]
  grid <- grid[c("occasion", "selection", "panel")]
  rownames(grid) <- NULL
  grid$period <- occasions$pairs$period[grid$occasion]
  grid$subround <- occasions$pairs$subround[grid$occasion]

  grid$active <- schedule_grid_active(
    paste(grid$selection, grid$panel, grid$occasion, sep = "|"),
    paste(selection, panel, occasion, sep = "|"),
    active
  )

  early <- grid$active & grid$period < entry_period[grid$selection]
  if (any(early)) {
    at <- which(early)[1L]
    abort_samplyr(
      c(
        "A selection cannot be active before it enters.",
        "x" = "Selection {.val {grid$selection[at]}} enters at period
               {entry_period[[grid$selection[at]]]} and is active in period
               {grid$period[at]}."
      ),
      class = "samplyr_error_program_activation_before_entry",
      call = call
    )
  }

  check_schedule_idle_occasions(
    grid$active, grid$occasion,
    headline = "Every declared occasion must have at least one active
                component.",
    detail = "Nothing is active at {idle}.",
    extra = c("i" = "A single selection may be dormant at an occasion, but
                     the program may not."),
    describe = function(idle) pair_phrase(occasions$pairs, idle),
    call = call
  )

  never <- vapply(
    split(grid$active, grid$selection)[selection_names],
    function(a) !any(a),
    logical(1)
  )
  if (any(never)) {
    abort_samplyr(
      c(
        "Every registered selection must be active at some declared
         occasion.",
        "x" = "Never active: {selection_names[never]}.",
        "i" = "Either the schedule missed it or it does not belong to this
               program."
      ),
      class = "samplyr_error_program_idle_selection",
      call = call
    )
  }

  grid
}

#' The block size a selection was drawn with bounds what its schedule may ask
#'
#' Block size was frozen at the selection's own draw from `r_min`, the fewest
#' panels its schedule then activated. A program that activates fewer than
#' that leaves under two units per block in the take, so the activation stops
#' carrying a within-block variance estimate while its weights stay perfectly
#' computable. That is the kind of loss worth refusing where it is declared
#' rather than discovering at export.
#' @noRd
check_program_block_sizes <- function(schedule, selections,
                                      call = caller_env()) {
  for (nm in names(selections)) {
    record <- selection_assignment(
      selections[[nm]], "A rotation program", call = call
    )
    if (is_null(record) || is_null(record$r_min)) {
      next
    }
    rows <- schedule[schedule$selection == nm, , drop = FALSE]
    per_occasion <- vapply(split(rows$active, rows$occasion), sum, integer(1))
    live <- per_occasion[per_occasion > 0L]
    short <- live[live < record$r_min]
    if (length(short) > 0) {
      occasion <- as.integer(names(short)[1L])
      abort_samplyr(
        c(
          "The schedule activates fewer panels of a selection than its blocks
           were built for.",
          "x" = "Selection {.val {nm}} activates {short[[1]]} panel{?s} at occasion
                 {occasion}, against the {record$r_min} its assignment assumed.",
          "i" = "Block size was frozen at that selection's draw as
                 {record$block_size}. A smaller take leaves under two units
                 per block, so the activation carries no within-block
                 variance estimate.",
          "i" = "Redraw the selection with a schedule whose leanest occasion matches
                 this one, or activate at least {record$r_min} panels."
        ),
        class = "samplyr_error_program_block_size",
        call = call
      )
    }
  }
  invisible(NULL)
}

## Materializing a program occasion

#' Materialize one occasion across every live selection
#' @noRd
materialize_program_occasion <- function(
  program,
  occasion,
  frames,
  stages,
  seed,
  panels,
  panel_stage = NULL,
  small_pool = NULL,
  reps,
  frame_digest_given = FALSE,
  execution_environment,
  call = caller_env()
) {
  # Program occasions cannot revise frozen selection assignment policies.
  check_occasion_extra_arguments(
    frames = frames, stages = stages, seed = seed, panels = panels,
    panel_stage = panel_stage, small_pool = small_pool, reps = reps,
    frame_digest_given = frame_digest_given,
    selects = "components the program already assigned",
    stored_with = "the program",
    call = call
  )
  occasion <- check_occasion_declared(occasion, program$schedule, call = call)

  schedule <- program$schedule
  digest <- rlang::hash(schedule)
  at_occasion <- schedule[schedule$occasion == occasion, , drop = FALSE]

  live <- list()
  for (nm in names(program$selections)) {
    active <- at_occasion$panel[at_occasion$selection == nm & at_occasion$active]
    if (length(active) == 0L) {
      next
    }
    sample <- program$selections[[nm]]
    record <- selection_assignment(sample, "An activation", call = call)
    live[[nm]] <- build_occasion_sample(
      source = sample,
      record = record,
      occasion = occasion,
      activation = activate_panels(sample, record, active, call = call),
      schedule_digest = digest,
      execution_environment = execution_environment,
      extra = list(
        selection = nm,
        entry_period = unname(program$entry_period[[nm]]),
        period = at_occasion$period[1L],
        subround = at_occasion$subround[1L]
      )
    )
  }

  new_rotation_occasion(live, occasion = occasion, program = program)
}

#' An occasion of a program is a collection, not a sample
#'
#' Each component carries its own receipt and its own exact activation
#' factor. They are deliberately not row-bound and their weights are
#' deliberately not combined: a unit eligible for two vintages had two
#' chances of selection, and the union probability needs eligibility
#' information no receipt carries.
#' @noRd
new_rotation_occasion <- function(selections, occasion, program) {
  at <- program$schedule[program$schedule$occasion == occasion, , drop = FALSE]
  structure(
    selections,
    period = at$period[1L],
    subround = at$subround[1L],
    entry_period = program$entry_period[names(selections)],
    schedule_digest = rlang::hash(program$schedule),
    class = "rotation_occasion"
  )
}

## Conversion

#' Row-bind a rotation occasion for fieldwork, not for estimation
#'
#' The result is a plain data frame carrying a `.selection` column, so it can be
#' used as the list of units to visit. It is deliberately not a `tbl_sample`:
#' the weights it carries are each selection's own, the row set may hold a unit
#' once per selection that drew it, and whether the selections overlap at all is
#' something the program does not know. Being a plain data frame is what
#' stops it reaching [as_svydesign()] as though it were one sample.
#'
#' @param x A rotation occasion.
#' @param ... Not used.
#' @return A plain data frame with a leading `.selection` column naming the
#'   selection of each row, followed by the columns of the selections. A unit
#'   drawn by several selections has one row for each, with that selection's
#'   weights.
#' @export
as.data.frame.rotation_occasion <- function(x, ...) {
  rlang::check_dots_empty()
  if (length(x) == 0L) {
    return(data.frame())
  }
  parts <- lapply(names(x), function(nm) {
    part <- as.data.frame(x[[nm]])
    cbind(.selection = rep(nm, nrow(part)), part, stringsAsFactors = FALSE)
  })
  out <- as.data.frame(dplyr::bind_rows(parts))
  rownames(out) <- NULL
  out
}

#' @export
as_svydesign.rotation_occasion <- function(x, ...) {
  abort_rotation_occasion_export("as_svydesign")
}

#' @export
as_svrepdesign.rotation_occasion <- function(x, ...) {
  abort_rotation_occasion_export("as_svrepdesign")
}

#' @noRd
abort_rotation_occasion_export <- function(fn_name, call = caller_env()) {
  abort_samplyr(
    c(
      "{.fn {fn_name}} cannot take a rotation occasion, which is a collection of
       samples rather than one sample.",
      "i" = "Its selections come from different frame vintages, so combining
             them needs the probability that a unit was selected into at
             least one, which no receipt carries.",
      "i" = "Export a component instead:
             {.code as_svydesign(occasion[[\"<selection name>\"]])}, which exports
             that selection's occasion on its own."
    ),
    class = "samplyr_error_rotation_occasion_not_combinable",
    call = call
  )
}
