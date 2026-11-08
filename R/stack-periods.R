## Multi-period interchange

#' Stack materialized periods or occasions into a long table
#'
#' @description
#' Verifies that two or more materialized samples come from one executed
#' master and stacks them into a plain long table, one row per active master
#' row per element. This is the structural handoff to an inference layer for
#' change between reporting periods, or between occasions. Which estimator of
#' change is appropriate, and under what conditions, is a separate question
#' that depends on the design and the overlap.
#'
#' Row-binding samples by hand does not reproduce the checks this performs.
#' Two executions of one design produce identical `.sample_id` values, so
#' samples of *different* masters stack without complaint. A sample edited
#' after execution keeps a correct provenance record while its rows no longer
#' match it. Both are refused here.
#'
#' @details
#' ## What it stacks
#'
#' The elements are all whole periods, as `execute(master, period = 2)`
#' gives, for change between periods, or all single occasions, as
#' `execute(master, period = 2, subround = 1)` gives, for change between
#' subrounds. The two are not mixed, and the result records which in its
#' `level` attribute, `"period"` or `"occasion"`. On a schedule without
#' subrounds a period is one occasion, and the level is `"period"`.
#'
#' A whole period is accepted when each panel is active at one of its
#' subrounds, so each unit has one row, at its own subround. A period in which
#' a panel is active at two of its subrounds would hold a unit twice, and the
#' stack's key would repeat.
#' It is refused, and so is any other window, whose window-averaged weights
#' a `period` label would misdescribe. [execute()] materializes such a
#' window on its own.
#'
#' ## What the columns mean
#'
#' Six columns are generated, and their names are chosen to prevent the two
#' substitutions that would quietly invalidate a downstream estimate:
#'
#' \describe{
#'   \item{`period`, `subround`}{The occasion each row was observed at. A
#'     row of a whole period keeps the subround it was observed at.}
#'   \item{`master_id`}{A **master-local** unit key, taken from `.sample_id`.
#'     It matches units across the elements of one master and means nothing
#'     outside it. It is not a population identity, and it is not generally
#'     a primary sampling unit: a clustered design's own cluster variable is
#'     carried through unchanged and is what a consumer's `PSU` argument
#'     wants.}
#'   \item{`panel`}{The rotation panel the row's **assignment unit** was
#'     given. Always present: a master drawn without panels has no schedule
#'     to materialize, so it has nothing to stack. That unit is the one the
#'     master's `panel_stage` names, so several rows can share one panel: all
#'     the members of a rotating household under a retained primary unit
#'     carry its label, not one of their own.}
#'   \item{`visit`}{The interview number of the row's unit within its
#'     cohort, from the schedule, which is what conditioning and attrition
#'     follow. Permanent units are active at every occasion outside the
#'     rotation and have `NA`.}
#'   \item{`design_weight`}{samplyr's exact design weight for that element,
#'     including the activation factor. It is **not** a final weight:
#'     nothing here is adjusted for nonresponse or calibrated, so a consumer
#'     asking for `w_final` is asking for something the analysis must
#'     supply.}
#' }
#'
#' Every other column of each element is carried through unchanged,
#' including the strata and cluster variables a consumer needs. samplyr's
#' internal columns are dropped, stage-specific quantities such as `.fpc_1`
#' among them.
#'
#' ## Masters drawn one occasion at a time
#'
#' A master whose later stages are drawn one occasion at a time stacks too,
#' as a master whose panels open several cohorts always is. A cohort keeps
#' its units' `master_id` over its visits, and the next cohort of a panel
#' takes new ones. Each element records the master's first stages and the
#' draws its rows come from. The stack is refused
#' (`samplyr_error_occasion_master_mismatch`) when two elements read
#' different draws of one cohort, as after drawing a period again with
#' another seed, and when some elements come from the master once its draws
#' were complete, since completing them numbers the units again. It is also
#' refused when the elements read occasions drawn into separate copies of
#' the master, each drawn from the same earlier master, since each copy
#' numbers its new units from the same point.
#'
#' ## The variance of a change
#'
#' The variance of a change between two periods is that of the window of
#' both, exported by [as_svydesign()], which reads the activation's quota
#' blocks and the periods each unit is active in as second-phase strata
#' ([panel_assignment]). Each period enters the window at half its weight,
#' so the change is the window's total of the outcome times 2 in the later
#' period and -2 in the earlier:
#'
#' ```r
#' both <- execute(master, period = c(1, 2))
#' both$change <- ifelse(both$.period == 2, 2, -2) * both$y
#' survey::svytotal(~change, as_svydesign(both))
#' ```
#'
#' A design built on the stack, clustered by the primary units, must at
#' least cross the design's strata with `panel`. Without it, a change
#' between periods that read different panels mixes their totals in one
#' stratum. In simulations over 300 masters, the window's standard error was
#' 0.95 to 0.99 of the spread of a change between quarters of a 2-2-2
#' rotation and 0.94 to 1.04 on a master of cohorts with 6 EAs per stratum
#' and panel. On the stack, crossing
#' the strata with `panel` gave 1.10 to 1.30 on the rotation, since it leaves
#' out the quota blocks, and leaving `panel` out gave 3.9 where the periods
#' read different panels.
#'
#' ## What it does not do
#'
#' It does not compute a covariance, a correlation, or a variance of change.
#' Those belong to the inference layer, and which estimator is appropriate
#' depends on the overlap and the design. It does not reshape outcomes:
#' attach each element's measurement under a common name before stacking.
#' It does not span the selections of a [rotation_program()], because
#' identity across frame vintages is not established.
#'
#' @param ... Two or more materialized samples of one master realization,
#'   all whole periods or all single occasions.
#'
#' @return A tibble with `period`, `subround`, `master_id`, `panel`, `visit`
#'   and `design_weight` first, then the columns of the elements
#'   themselves, with a `level` attribute. One row per active master row per
#'   element: under with-replacement selection a population unit may occupy
#'   several master rows, so a row is a selection occurrence rather than
#'   necessarily a distinct population unit. `master_id` is unique within an
#'   element either way. It is an ordinary tibble, not a `tbl_sample`: it
#'   holds several realizations and repeats the unit key on purpose.
#'
#' @examples
#' rotation <- data.frame(
#'   panel = rep(1:4, times = 4),
#'   period = rep(1:4, each = 4),
#'   active = c(
#'     TRUE, TRUE, FALSE, FALSE,
#'     FALSE, TRUE, TRUE, FALSE,
#'     FALSE, FALSE, TRUE, TRUE,
#'     TRUE, FALSE, FALSE, TRUE
#'   )
#' )
#'
#' master <- sampling_design() |>
#'   draw(n = 40) |>
#'   execute(bfa_eas, seed = 2025, panels = rotation)
#'
#' stack_periods(execute(master, period = 1), execute(master, period = 2))
#'
#' @seealso [execute()] for materializing a period or an occasion,
#'   [as_svydesign()] for the export that supplies each element's own
#'   variance, [joint_expectation()] with `period` for how far two occasions
#'   overlap
#'
#' @family survey export
#' @export
stack_periods <- function(...) {
  occasions <- list(...)
  check_stack_periods_inputs(occasions)
  call <- current_env()
  records <- lapply(occasions, stack_periods_provenance, call = call)
  level <- check_stack_periods_agreement(records)

  rows <- lapply(seq_along(occasions), function(i) {
    stack_periods_rows(occasions[[i]], records[[i]], call = call)
  })
  out <- vctrs::vec_rbind(!!!rows)
  attr(out, "level") <- level
  out
}

#' The provenance one element carries, and the guards it must pass alone
#' @noRd
stack_periods_provenance <- function(occasion, call = caller_env()) {
  metadata <- attr(occasion, "metadata")
  check_single_replicate(occasion, "stack_periods", call = call)
  check_sample_unmodified(occasion, "stack_periods", provisional_ok = TRUE,
                          call = call)
  # Shared samples cannot also be occasions.

  # Refuse occasion stacks spanning selections without cross-vintage identity.
  if (!is_null(metadata$occasion$selection)) {
    abort_samplyr(
      c(
        "{.fn stack_periods} does not span the selections of a rotation
         program.",
        "x" = "This sample is selection {.val {metadata$occasion$selection}} of a
               program.",
        "i" = "Identity across selections is not established, so units of
               different selections cannot be matched by {.field master_id}."
      ),
      class = "samplyr_error_stack_periods_input",
      call = call
    )
  }

  # Require provenance before comparing occasions with each other.
  missing <- c(
    if (!identical(metadata$prev_phase$transition, "panel_activation")) {
      "an activation link"
    },
    if (length(metadata$occasion$occasion) < 1L ||
          !is_integerish_numeric(metadata$occasion$occasion)) {
      "the occasions it holds"
    },
    if (is_null(metadata$occasion$master_digest)) "a master fingerprint",
    if (is_null(metadata$occasion$schedule_digest)) "a schedule fingerprint",
    if (is_null(metadata$panel_assignment)) "a panel assignment record"
  )
  if (length(missing) > 0) {
    abort_samplyr(
      c(
        "A sample is missing the provenance {.fn stack_periods} verifies.",
        "x" = "Missing: {missing}.",
        "i" = "Materialize it from the master with
               {.code execute(master, period = p)} rather than assembling it."
      ),
      class = "samplyr_error_stack_periods_provenance",
      call = call
    )
  }

  level <- stack_level(occasion)
  if (is_null(level)) {
    abort_samplyr(
      c(
        "{.fn stack_periods} stacks whole periods or single occasions.",
        "x" = "This sample pools {sample_occasion_phrase(metadata)}, which is
               neither a whole period read once per unit nor one occasion.",
        "i" = "Materialize such a window on its own with {.fn execute}, or
               stack its periods, {.code execute(master, period = p)}, or its
               occasions, {.code execute(master, period = p, subround = s)}."
      ),
      class = "samplyr_error_stack_periods_input",
      call = call
    )
  }

  master <- metadata$prev_phase$sample
  if (is_null(master)) {
    abort_samplyr(
      c(
        "{.fn stack_periods} needs the master each element was activated
         from.",
        "x" = "The sample of {sample_occasion_phrase(metadata)} carries no
               link to it.",
        "i" = "Materialize it again from the master:
               {.code execute(master, {occasion_code(
               metadata$panel_assignment$schedule, metadata$occasion$occasion)})}."
      ),
      class = "samplyr_error_occasion_no_master",
      call = call
    )
  }
  check_occasion_master_identity(metadata, master, call = call)
  prepare_panel_record(
    metadata$panel_assignment,
    "A stacked table",
    call = call
  )

  list(
    occasion = metadata$occasion$occasion,
    level = level,
    period = metadata$occasion$period,
    subround = metadata$occasion$subround,
    master_digest = metadata$occasion$master_digest,
    schedule_digest = metadata$occasion$schedule_digest,
    algorithm = metadata$panel_assignment$algorithm,
    version = metadata$panel_assignment$version,
    unit = metadata$panel_assignment$unit,
    provisional = isTRUE(metadata$occasion$provisional),
    realization = metadata$occasion$realization
  )
}

#' @noRd
check_stack_periods_inputs <- function(occasions, call = caller_env()) {
  if (length(occasions) < 2L) {
    abort_samplyr(
      c(
        "{.fn stack_periods} stacks two or more periods or occasions.",
        "x" = "{length(occasions)} {?was/were} given.",
        "i" = "A single one is already a sample: use it directly, or export
               it with {.fn as_svydesign}."
      ),
      class = "samplyr_error_stack_periods_input",
      call = call
    )
  }

  bad <- which(!vapply(occasions, function(w) {
    is_tbl_sample(w) && !is_null(attr(w, "metadata")$occasion)
  }, logical(1)))
  if (length(bad) > 0) {
    abort_samplyr(
      c(
        "{.fn stack_periods} takes materialized periods or occasions.",
        # Set each cli plural quantity explicitly.
        "x" = "{cli::qty(length(bad))}Argument{?s} {bad}{cli::qty(length(bad))} {?is/are} not one.",
        "i" = "One comes from {.code execute(master, period = p)}."
      ),
      class = "samplyr_error_stack_periods_input",
      call = call
    )
  }
  invisible(NULL)
}

#' Every element must describe the same realization of the same master
#' @noRd
check_stack_periods_agreement <- function(records, call = caller_env()) {
  # `toString()` safely describes malformed non-scalar legacy fields.
  disagree <- function(field) {
    values <- unique(vapply(records, function(r) {
      value <- r[[field]]
      if (is_null(value) || length(value) == 0L) NA_character_ else
        toString(as.character(value))
    }, character(1)))
    length(values) > 1L
  }
  provisional <- vapply(records, function(r) isTRUE(r$provisional), logical(1))
  if (any(provisional) && !all(provisional)) {
    abort_samplyr(
      c(
        "Every element must come from one state of the master.",
        "x" = "Some were read while the master's later stages were drawn one
               occasion at a time, and others after they were complete.",
        "i" = "Completing the draws numbers the units again, so read every
               element from the same master."
      ),
      class = "samplyr_error_occasion_master_mismatch",
      call = call
    )
  }
  # A provisional element's fingerprint describes its own rows. Its master's
  # first stages and the draws it reads say which realization it is.
  same_master <- if (all(provisional)) {
    !disagree_realization(records)
  } else {
    !disagree("master_digest")
  }
  differing <- c(
    if (!same_master) "the master they were activated from",
    if (disagree("schedule_digest")) "their rotation schedule",
    if (disagree("algorithm")) "their assignment algorithm",
    if (disagree("version")) "their assignment record version",
    if (disagree("unit")) "their assignment unit"
  )
  if (length(differing) > 0) {
    abort_samplyr(
      c(
        "Every element must come from one execution of one master.",
        "x" = "These samples differ on {differing}.",
        "i" = "Two executions of one design carry the same
               {.field .sample_id} values, so stacking them would match units
               that were never the same unit."
      ),
      class = "samplyr_error_occasion_master_mismatch",
      call = call
    )
  }

  if (all(provisional) && diverged_draws(records)) {
    abort_samplyr(
      c(
        "Every element must come from one line of draws of the master.",
        "x" = "They read occasions drawn into separate copies of the master,
               which number their units independently.",
        "i" = "Draw each occasion into the master the previous draw returned,
               and read every element from the last."
      ),
      class = "samplyr_error_occasion_master_mismatch",
      call = call
    )
  }

  levels <- unique(vapply(records, function(r) r$level, character(1)))
  if (length(levels) > 1L) {
    abort_samplyr(
      c(
        "{.fn stack_periods} stacks elements at one level.",
        "x" = "It was given whole periods and single occasions together.",
        "i" = "Stack the periods, or stack the occasions."
      ),
      class = "samplyr_error_stack_periods_input",
      call = call
    )
  }
  labels <- vapply(records, function(r) {
    if (identical(levels, "period")) {
      paste("period", unique(r$period))
    } else {
      paste0("period ", r$period, " subround ", r$subround)
    }
  }, character(1))
  if (anyDuplicated(labels) > 0) {
    repeated <- unique(labels[duplicated(labels)])
    abort_samplyr(
      c(
        "Each period or occasion may be stacked once.",
        "x" = "{repeated} {?was/were} given more than once."
      ),
      class = "samplyr_error_stack_periods_input",
      call = call
    )
  }
  levels
}

#' Do provisional elements read different realizations?
#'
#' They agree when their masters' first stages agree and every draw that
#' two of them read is the same draw.
#' @noRd
disagree_realization <- function(records) {
  masters <- unique(lapply(records, function(r) r$realization$master))
  if (length(masters) != 1L || is_null(masters[[1L]])) {
    return(TRUE)
  }
  draws <- unlist(lapply(records, function(r) r$realization$draws))
  any(vapply(split(unname(draws), names(draws)), function(d) {
    length(unique(d)) > 1L
  }, logical(1)))
}

#' Do provisional elements read draws of separate copies of a master?
#'
#' One master holds every draw they read when the elements could all have
#' been read from one copy. Otherwise two of the draws may have numbered
#' different units alike.
#' @noRd
diverged_draws <- function(records) {
  read <- unique(unname(unlist(lapply(records, function(r) {
    r$realization$draws
  }))))
  !any(vapply(records, function(r) {
    all(read %in% r$realization$history)
  }, logical(1)))
}

#' The level a materialized sample enters a stack at
#'
#' `"period"` for a whole period read once per unit, or for one occasion of
#' a period that has only one, `"occasion"` for one subround of a period
#' with several, and `NULL` for any other window.
#' @noRd
stack_level <- function(occasion) {
  metadata <- attr(occasion, "metadata")
  schedule <- metadata$panel_assignment$schedule
  occasions <- metadata$occasion$occasion
  if (is_null(schedule) || length(occasions) == 0L) {
    return(NULL)
  }
  pairs <- grid_pairs(schedule)
  period <- unique(pairs$period[occasions])
  if (length(period) != 1L) {
    return(NULL)
  }
  whole <- identical(which(pairs$period == period), sort(occasions))
  active <- schedule$panel[schedule$active & schedule$occasion %in% occasions]
  if (whole && (length(occasions) == 1L || !anyDuplicated(active))) {
    return("period")
  }
  if (length(occasions) == 1L) "occasion"
}

#' @noRd
stack_periods_columns <- c(
  "period", "subround", "master_id", "panel", "visit", "design_weight"
)

#' One element's rows, with the generated columns in front
#' @noRd
stack_periods_rows <- function(occasion, record, call = caller_env()) {
  data <- as.data.frame(occasion)
  carried <- setdiff(names(data), samplyr_internal_cols(data))

  # Refuse collisions with the generated interchange columns.
  clash <- intersect(carried, stack_periods_columns)
  if (length(clash) > 0) {
    abort_samplyr(
      c(
        "{.fn stack_periods} generates columns that a sample already carries
         as data.",
        "x" = "Conflicting: {.field {clash}}.",
        "i" = "The generated names are {.field {stack_periods_columns}}. Rename
               the data columns before stacking; {.fn execute} cannot refuse
               them, because none is a reserved samplyr name."
      ),
      class = "samplyr_error_stack_periods_columns",
      call = call
    )
  }

  observed <- function(column, value) {
    if (column %in% names(data)) as.integer(data[[column]]) else
      rep(as.integer(value), nrow(data))
  }
  generated <- tibble::tibble(
    period = observed(".period", record$period),
    subround = observed(".subround", record$subround),
    master_id = data$.sample_id,
    # Keep an explicit missing-panel guard for future cohort support.
    panel = if (is_null(data$.panel)) {
      rep(NA_integer_, nrow(data))
    } else {
      as.integer(data$.panel)
    },
    visit = if (is_null(data$.visit)) {
      rep(NA_integer_, nrow(data))
    } else {
      as.integer(data$.visit)
    },
    design_weight = data$.weight
  )
  vctrs::vec_cbind(generated, tibble::as_tibble(data[, carried, drop = FALSE]))
}
