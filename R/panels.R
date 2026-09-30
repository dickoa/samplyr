## Panel assignment

# Random fixed quotas within ordered blocks. Certainty units stay permanent.

#' Normalize the `panels` argument to a count and an optional schedule
#'
#' @return `NULL`, or a list with `k`, `r_min`, `block_size`, `schedule`,
#'   `stage`, `small_pool`, and `from_plan` for an `svyplan_schedule`
#'   (read by `resolve_small_pools()`).
#' @noRd
normalize_panel_input <- function(panels, panel_stage = NULL,
                                  small_pool = NULL,
                                  call = caller_env()) {
  if (is_null(panels)) {
    check_small_pool_applicable(small_pool, has_schedule = FALSE,
                                has_panels = FALSE, call = call)
    check_panel_stage_applicable(panel_stage, call = call)
    return(NULL)
  }
  stage <- normalize_panel_stage(panel_stage, call = call)
  if (is_svyplan_schedule(panels)) {
    return(normalize_plan_panels(
      panels,
      stage = stage,
      small_pool = small_pool,
      call = call
    ))
  }
  if (is.data.frame(panels)) {
    spec <- normalize_panel_schedule(panels, call = call)
    spec$small_pool <- normalize_small_pool(small_pool, call = call)
    spec$stage <- stage
    return(spec)
  }
  # Several counts read as issued and reserve sizes, which panels are not.
  if (is.numeric(panels) && length(panels) > 1L) {
    abort_samplyr(
      c(
        "{.arg panels} takes one count, the number of equal random panels,
         not {length(panels)} sizes.",
        "i" = "To hold part of the sample back, split it into panels and
               activate the held panels in a later wave through a rotation
               schedule, as {.help execute} describes."
      ),
      class = "samplyr_error_panel_count",
      call = call
    )
  }
  if (
    !is.numeric(panels) ||
      length(panels) != 1 ||
      !is_integerish_numeric(panels) ||
      panels < 2
  ) {
    abort_samplyr(
      c(
        "{.arg panels} must be a single integer >= 2, or a rotation schedule.",
        "i" = "One panel is no partition, so there is nothing to rotate."
      ),
      class = "samplyr_error_panel_count",
      call = call
    )
  }
  k <- as.integer(panels)
  check_small_pool_applicable(small_pool, has_schedule = FALSE,
                              has_panels = TRUE, call = call)
  list(
    k = k,
    r_min = 1L,
    block_size = panel_block_size(k, 1L),
    schedule = NULL,
    small_pool = NULL,
    stage = stage
  )
}

#' Translate the startup partition from a planning schedule
#' @noRd
normalize_plan_panels <- function(plan, stage, small_pool,
                                  call = caller_env()) {
  check_svyplan_schedule(plan, "panels", call = call)
  policy <- normalize_small_pool(small_pool, call = call)
  if (identical(policy, "permanent")) {
    abort_samplyr(
      c(
        "{.arg small_pool} cannot be {.val permanent} with an
         {.cls svyplan_schedule}.",
        "i" = "The planning schedule describes a rotating cohort life.
               Permanent activation has a different overlap contract."
      ),
      class = "samplyr_error_plan_small_pool",
      call = call
    )
  }

  expected <- plan$panel_parameters
  if (identical(expected$k, 1L)) {
    check_small_pool_applicable(
      small_pool,
      has_schedule = FALSE,
      has_panels = FALSE,
      call = call
    )
    check_panel_stage_applicable(stage, call = call)
    return(NULL)
  }

  startup <- plan$schedule[plan$schedule$cohort == "startup", ]
  tail <- plan$tail_commitments[
    plan$tail_commitments$cohort == "startup",
    ,
    drop = FALSE
  ]
  if (nrow(tail) > 0L) {
    tail$active <- TRUE
    startup <- rbind(
      startup[c("panel", "wave", "active")],
      tail[c("panel", "wave", "active")]
    )
  }
  last_active <- max(startup$wave[startup$active])
  startup <- startup[startup$wave <= last_active,
                     c("panel", "wave", "active")]
  spec <- normalize_panel_schedule(startup, min_panels = 1L, call = call)
  same <- identical(spec$k, expected$k) &&
    identical(spec$r_min, expected$r_min) &&
    identical(spec$block_size, expected$block_width)
  if (!same) {
    abort_samplyr(
      "The {.cls svyplan_schedule} panel parameters do not match its startup schedule.",
      class = "samplyr_error_svyplan_schedule",
      call = call
    )
  }
  spec$small_pool <- "error"
  spec$stage <- stage
  spec$from_plan <- TRUE
  spec
}

#' Which stage owns the panel assignment
#'
#' A stage number, since labels are optional and need not be unique. `NULL`
#' means the first executed stage.
#' @noRd
normalize_panel_stage <- function(panel_stage, call = caller_env()) {
  if (is_null(panel_stage)) {
    return(NULL)
  }
  ok <- is.numeric(panel_stage) &&
    length(panel_stage) == 1L &&
    is_integerish_numeric(panel_stage) &&
    panel_stage >= 1
  if (!ok) {
    abort_samplyr(
      c(
        "{.arg panel_stage} must be a single stage number of 1 or more.",
        "i" = "It names the stage whose selected units are assigned to
               panels, and every later stage inherits the assignment."
      ),
      class = "samplyr_error_panel_stage_value",
      call = call
    )
  }
  as.integer(panel_stage)
}

#' @noRd
check_panel_stage_applicable <- function(panel_stage, call = caller_env()) {
  if (is_null(panel_stage)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.arg panel_stage} names the stage that assigns panels.",
      "x" = "No {.arg panels} was supplied, so nothing is assigned.",
      "i" = "Supply {.arg panels} as a count or a rotation schedule, or drop
             {.arg panel_stage}."
    ),
    class = "samplyr_error_panel_stage_not_applicable",
    call = call
  )
}

#' Resolve the assignment stage against the execution about to run
#'
#' Checked against the resolved schedule, not the design, so a partial
#' execution that stops before the named stage is refused.
#' @noRd
resolve_panel_stage <- function(spec, executed, design, call = caller_env()) {
  if (is_null(spec) || is_null(spec$stage)) {
    return(spec)
  }
  stage <- spec$stage
  if (!stage %in% executed) {
    n_stages <- length(design$stages)
    abort_samplyr(
      c(
        "{.arg panel_stage} must name a stage this execution completes.",
        "x" = if (stage > n_stages) {
          "Stage {stage} was named, and the design has {n_stages} stage{?s}."
        } else {
          "Stage {stage} was named, and this execution covers
           {cli::qty(length(executed))}stage{?s} {executed}."
        },
        "i" = "Panels are assigned from the units a stage selected, so the
               stage has to have been drawn."
      ),
      class = "samplyr_error_panel_stage_unexecuted",
      call = call
    )
  }
  spec
}

#' The small-pool policy, and where it does and does not apply
#'
#' A wave activating `r` of `k` panels takes nothing from a pool of `m`
#' units exactly when `m <= k - r`. The policy governs only that case, and
#' only with a schedule, since a scalar `panels` has no `r_min`.
#' @noRd
normalize_small_pool <- function(small_pool, call = caller_env()) {
  if (is_null(small_pool)) {
    return("error")
  }
  if (
    !is.character(small_pool) ||
      length(small_pool) != 1L ||
      !small_pool %in% c("error", "permanent")
  ) {
    abort_samplyr(
      c(
        "{.arg small_pool} must be {.val error} or {.val permanent}.",
        "i" = "{.val error} refuses an assignment whose schedule would leave
               a pool with no active unit. {.val permanent} activates such a
               pool at every wave instead."
      ),
      class = "samplyr_error_small_pool_value",
      call = call
    )
  }
  small_pool
}

#' @noRd
check_small_pool_applicable <- function(small_pool, has_schedule, has_panels,
                                        call = caller_env()) {
  if (is_null(small_pool)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.arg small_pool} is a policy for a rotation schedule.",
      "x" = if (has_panels) {
        "{.arg panels} is a panel count, which declares no wave activation."
      } else {
        "No {.arg panels} was supplied."
      },
      "i" = "Supply a schedule with {.field panel}, {.field wave} and
             optionally {.field active} columns, or drop
             {.arg small_pool}."
    ),
    class = "samplyr_error_small_pool_not_applicable",
    call = call
  )
}

#' Validate and normalize a rotation schedule
#'
#' The stored form is always the complete panel-by-wave grid.
#' @noRd
normalize_panel_schedule <- function(schedule, min_panels = 2L,
                                     call = caller_env()) {
  subject <- schedule_subject("panels")
  check_schedule_columns(
    schedule, subject,
    extra = c("i" = "An {.field active} column is optional: without it every
                     row given is active, and every combination left out is
                     not."),
    call = call
  )

  panel <- schedule$panel
  wave <- schedule$wave
  active <- schedule_active_column(schedule, subject, call = call)

  check_schedule_integers(panel, "panel", call = call)
  check_schedule_integers(wave, "wave", call = call)
  panel <- as.integer(panel)
  wave <- as.integer(wave)

  check_schedule_duplicates(
    paste(panel, wave, sep = "|"), subject,
    declares = "each panel once per wave",
    combination = cli::format_inline("{.field panel}-{.field wave}"),
    call = call
  )

  k <- max(panel)
  n_waves <- max(wave)
  check_schedule_contiguous(panel, k, "panel", call = call)
  check_schedule_contiguous(wave, n_waves, "wave", call = call)
  if (k < min_panels) {
    abort_samplyr(
      "{subject$upper} must declare at least {min_panels} panels.",
      class = "samplyr_error_schedule_size",
      call = call
    )
  }

  grid <- expand.grid(
    panel = seq_len(k),
    wave = seq_len(n_waves),
    KEEP.OUT.ATTRS = FALSE
  )
  grid <- grid[order(grid$wave, grid$panel), c("wave", "panel")]
  rownames(grid) <- NULL
  grid$active <- schedule_grid_active(
    paste(grid$panel, grid$wave, sep = "|"),
    paste(panel, wave, sep = "|"),
    active
  )

  per_wave <- check_schedule_idle_waves(
    grid$active, grid$wave,
    headline = "Every declared wave must activate at least one panel.",
    detail = "No panel is active at wave {idle}.",
    call = call
  )

  r_min <- min(per_wave)
  list(
    k = k,
    r_min = r_min,
    block_size = panel_block_size(k, r_min),
    schedule = grid
  )
}

## Shared schedule validation

# Master and program schedules share the same table checks.

#' The phrase the two schedule paths differ on, in the forms a message needs
#'
#' Both cases are prebuilt because the phrase is already cli-formatted, so
#' its first character cannot be capitalized reliably.
#' @noRd
schedule_subject <- function(arg) {
  if (identical(arg, "panels")) {
    list(
      arg = "panels",
      lower = cli::format_inline("a {.arg panels} schedule"),
      upper = cli::format_inline("A {.arg panels} schedule")
    )
  } else {
    list(
      arg = "schedule",
      lower = "a program schedule",
      upper = "A program schedule"
    )
  }
}

#' The table shape both schedules need before any column is read
#' @noRd
check_schedule_columns <- function(schedule, subject, extra = NULL,
                                   call = caller_env()) {
  if (!is.data.frame(schedule)) {
    abort_samplyr(
      "{.arg {subject$arg}} must be a data frame.",
      class = "samplyr_error_schedule_columns",
      call = call
    )
  }
  missing <- setdiff(c("panel", "wave"), names(schedule))
  if (length(missing) > 0) {
    abort_samplyr(
      c(
        "{subject$upper} needs a {.field panel} and a {.field wave} column.",
        "x" = "Missing: {.field {missing}}.",
        extra
      ),
      class = "samplyr_error_schedule_columns",
      call = call
    )
  }
  invisible(NULL)
}

#' The optional `active` column, defaulted and checked
#'
#' Absent means every row given is active.
#' @noRd
schedule_active_column <- function(schedule, subject, call = caller_env()) {
  active <- if ("active" %in% names(schedule)) {
    schedule$active
  } else {
    return(rep(TRUE, nrow(schedule)))
  }
  if (!is.logical(active) || anyNA(active)) {
    abort_samplyr(
      "The {.field active} column of {subject$lower} must be logical and
       complete.",
      class = "samplyr_error_schedule_active",
      call = call
    )
  }
  active
}

#' A combination declared twice is a mistake, not a re-statement
#'
#' `combination` names the key, panel-wave or cohort-panel-wave.
#' @noRd
check_schedule_duplicates <- function(key, subject, declares, combination,
                                      call = caller_env()) {
  if (anyDuplicated(key) == 0L) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{subject$upper} may declare {declares}.",
      "x" = "It repeats at least one {combination} combination."
    ),
    class = "samplyr_error_schedule_duplicates",
    call = call
  )
}

#' Fill a completed grid's activity from the rows the caller declared
#' @noRd
schedule_grid_active <- function(grid_key, input_key, active) {
  at <- match(grid_key, input_key)
  !is.na(at) & active[at]
}

#' No declared wave may be empty
#'
#' @return The count active at each wave, which is `r_min` for a master.
#' @noRd
check_schedule_idle_waves <- function(active, wave, headline, detail,
                                      extra = NULL, call = caller_env()) {
  per_wave <- vapply(split(active, wave), sum, integer(1))
  if (any(per_wave == 0L)) {
    idle <- as.integer(names(per_wave)[per_wave == 0L])
    abort_samplyr(
      c(headline, "x" = detail, extra),
      class = "samplyr_error_schedule_idle_wave",
      call = call
    )
  }
  per_wave
}

#' Shared by the draw-time and program schedule paths, which differ only in
#' which argument carried the schedule.
#' @noRd
check_schedule_integers <- function(x, name, arg = "panels",
                                    call = caller_env()) {
  ok <- is.numeric(x) &&
    !anyNA(x) &&
    is_integerish_numeric(x) &&
    all(x >= 1)
  if (!ok) {
    abort_samplyr(
      "The {.field {name}} column of a {.arg {arg}} schedule must hold
       complete integers of 1 or more.",
      class = "samplyr_error_schedule_values",
      call = call
    )
  }
  invisible(NULL)
}

#' @noRd
check_schedule_contiguous <- function(x, top, name, arg = "panels",
                                      call = caller_env()) {
  gaps <- setdiff(seq_len(top), unique(x))
  if (length(gaps) > 0) {
    abort_samplyr(
      c(
        "A {.arg {arg}} schedule must number its {.field {name}} values
         from 1 without gaps.",
        "x" = "Declared up to {top}, but missing: {gaps}."
      ),
      class = "samplyr_error_schedule_gap",
      call = call
    )
  }
  invisible(NULL)
}

#' Block size for a pool
#'
#' Large enough that the `r_min` panels of the leanest wave take two units
#' per block, the smallest take with a within-block variance estimate.
#' @noRd
panel_block_size <- function(k, r_min) {
  as.integer(k * ceiling(2 / r_min))
}

#' Everything one stage contributes to a panel assignment
#'
#' Unit identity, pool variables, certainty column, control order, multi-hit
#' status and unit label, resolved together so they stay consistent.
#' @param stage_num The assignment stage, an index into `design$stages`.
#' @param sample The realized sample, read for the identity columns.
#' @noRd
panel_assignment_context <- function(design, stage_num, sample,
                                     call = caller_env()) {
  spec <- design$stages[[stage_num]]
  draw_col <- paste0(".draw_", stage_num)
  # With-replacement panel units need draw IDs.
  multi_hit <- is_multi_hit_method(spec$draw_spec)
  if (multi_hit && !draw_col %in% names(sample)) {
    abort_panel_missing_identity(stage_num, draw_col, occurrence = TRUE,
                                 call = call)
  }
  clustered <- !is_null(spec$clusters)
  ancestors <- collect_ancestor_occurrence_vars(
    design, stage_num, sample, call = call
  )

  context <- list(
    stage_num = stage_num,
    stage_spec = spec,
    clustered = clustered,
    multi_hit = multi_hit,
    ancestor_vars = ancestors,
    # Qualify restarted draw indices by strata.
    key_vars = if (clustered) {
      unique(c(
        ancestors, spec$strata$vars, spec$clusters$vars,
        if (multi_hit) draw_col
      ))
    } else {
      ".sample_id"
    },
    pool_vars = unique(c(ancestors, spec$strata$vars)),
    certainty_col = paste0(".certainty_", stage_num),
    control = spec$draw_spec$control,
    unit = if (multi_hit) {
      "occurrence"
    } else if (clustered) {
      "cluster"
    } else {
      "element"
    }
  )

  missing <- setdiff(
    c(context$key_vars, context$pool_vars), names(sample)
  )
  if (length(missing) > 0) {
    abort_panel_missing_identity(stage_num, missing, occurrence = FALSE,
                                 call = call)
  }
  context
}

#' Refuse an assignment whose units the sample can no longer tell apart
#'
#' A missing draw index would silently coarsen the identity.
#' @noRd
abort_panel_missing_identity <- function(stage_num, columns, occurrence,
                                         call = caller_env()) {
  abort_samplyr(
    c(
      "Panel assignment needs {cli::qty(length(columns))}{?a column/columns}
       this sample does not carry.",
      "x" = if (occurrence) {
        "Stage {stage_num} selects with replacement, so its units are draw
         occurrences, and {.field {columns}} is missing."
      } else {
        "Stage {stage_num} builds its assignment units and pools from
         {.field {columns}}."
      },
      "i" = if (occurrence) {
        "Without the draw index, two selections of one cluster read as one
         unit and would take one panel between them."
      },
      "i" = "Design columns are removed by modifying a sample after it was
             executed. Assign panels on the sample {.fn execute} returned."
    ),
    class = "samplyr_error_panel_missing_identity",
    call = call
  )
}

#' Assign panel labels by blocked random quota within frozen pools
#'
#' @return The sample with a `.panel` column, and the record for the receipt.
#' @noRd
assign_panels <- function(result, spec, context, call = caller_env()) {
  k <- spec$k
  block <- spec$block_size
  key_vars <- context$key_vars

  if (!context$clustered) {
    unit_rows <- seq_len(nrow(result))
    rows_of_unit <- NULL
  } else {
    row_keys <- make_group_key(result, key_vars)
    unit_rows <- which(!duplicated(row_keys))
    rows_of_unit <- match(row_keys, row_keys[unit_rows])
  }

  units <- result[unit_rows, , drop = FALSE]
  pools <- panel_pools(units, context)

  # Resolve every pool before consuming RNG state.
  pools <- resolve_small_pools(pools, spec, call = call)

  panel <- integer(length(unit_rows))
  pool_records <- vector("list", length(pools))
  for (p in seq_along(pools)) {
    pool <- pools[[p]]
    idx <- panel_pool_order(units, pool$indices, context$control)
    assigned <- assign_blocked_panels(length(idx), k, block)
    panel[idx] <- assigned$panel
    pool_records[[p]] <- list(
      stratum = pool$stratum,
      class = pool$class,
      activation = pool$activation,
      permanent_reason = pool$permanent_reason,
      size = length(idx),
      keys = make_group_key(units[idx, , drop = FALSE], key_vars),
      blocks = assigned$blocks,
      quotas = block_quotas(assigned$panel, assigned$blocks, k)
    )
  }

  result$.panel <- if (is_null(rows_of_unit)) panel else panel[rows_of_unit]

  record <- list(
    algorithm = "blocked_random_quota",
    version = 3L,
    panels = k,
    assignment_stage = context$stage_num,
    block_size = block,
    r_min = spec$r_min,
    unit = context$unit,
    key_vars = key_vars,
    pool_vars = context$pool_vars,
    control_ordered = length(context$control) > 0L,
    # Permanence is defined at the assignment stage.
    certainty = "permanent",
    small_pool_policy = spec$small_pool %||% "error",
    schedule = spec$schedule,
    pools = pool_records
  )

  list(sample = result, record = record)
}

#' The assignment record this package knows how to read
#'
#' Quotas belong to this algorithm, so a record naming another is refused.
#' @noRd
panel_record_algorithm <- "blocked_random_quota"

#' The record versions this build knows how to read
#'
#' A set, not a ceiling. A version below any that existed is unreadable too.
#' @noRd
supported_panel_record_versions <- c(1L, 2L, 3L)

#' Read an assignment record under the law it names
#'
#' Checks algorithm and version, then normalizes, then validates fields, and
#' the order matters. `NULL` in is `NULL` out.
#' @return The normalized record, which every later reader should use.
#' @noRd
prepare_panel_record <- function(record, what, call = caller_env()) {
  check_panel_record_supported(record, what, call = call)
  record <- normalize_panel_record(record)
  check_panel_record_fields(record, what, call = call)
  if (!is_null(record)) {
    # Coerce assignment stages only after validation.
    record$assignment_stage <- as.integer(record$assignment_stage)
  }
  record
}

#' @param record The assignment record, or `NULL` for a sample drawn whole.
#' @noRd
check_panel_record_supported <- function(record, what, call = caller_env()) {
  if (is_null(record)) {
    return(invisible(NULL))
  }

  if (!is.list(record)) {
    shown <- describe_record_value(record)
    abort_samplyr(
      c(
        "{what} is computed from the panel assignment, and this sample's
         assignment record is not a record.",
        "x" = "The receipt carries {shown} where the assignment belongs.",
        "i" = "An assignment record is a set of named fields. A bare value
               states neither the algorithm it was made under nor the version
               that fixes what its fields mean."
      ),
      class = "samplyr_error_panel_record_malformed",
      call = call
    )
  }

  algorithm <- record$algorithm
  if (!identical(algorithm, panel_record_algorithm)) {
    abort_samplyr(
      c(
        "{what} is computed from the panel assignment, and this sample's
         assignment was not made by an algorithm it can read.",
        "x" = "The record names {.val {algorithm %||% NA_character_}}, and
               {.val {panel_record_algorithm}} is what this version knows.",
        "i" = "The quotas of one algorithm are not the quotas of another, so
               reading them across would state a probability law the sample
               was not drawn under."
      ),
      class = "samplyr_error_panel_record_unsupported",
      call = call
    )
  }

  version <- record$version
  stated <- length(version) == 1L && is_integerish_numeric(version)
  if (stated && version %in% supported_panel_record_versions) {
    return(invisible(NULL))
  }

  if (stated && version > max(supported_panel_record_versions)) {
    abort_samplyr(
      c(
        "{what} is computed from the panel assignment, and this sample's
         assignment record is newer than this version of samplyr.",
        "x" = "The record is version {version}. This build reads
               {supported_panel_record_versions}.",
        "i" = "Install a samplyr new enough to have written it."
      ),
      class = "samplyr_error_panel_record_unsupported",
      call = call
    )
  }

  shown <- if (length(version) == 0L) {
    "no version"
  } else {
    describe_record_value(version)
  }
  abort_samplyr(
    c(
      "{what} is computed from the panel assignment, and this sample's
       assignment record does not state a version this build can read.",
      "x" = "The record states {shown}. This build reads
             {supported_panel_record_versions}.",
      "i" = "The version fixes which probability law a block's quotas encode,
             so an unrecognized one cannot be read as the current one."
    ),
    class = "samplyr_error_panel_record_unsupported",
    call = call
  )
}

#' What a record numbered 3 has to carry to be one
#'
#' A version-3 record missing its stage, unit or identity columns is refused,
#' not filled in, because version 3 is where they are written down. Versions
#' 1 and 2 mean stage 1 (see `normalize_panel_record()`). Of these fields only
#' `assignment_stage` can come from a design file. Pools are not checked.
#' @noRd
check_panel_record_fields <- function(record, what, call = caller_env()) {
  if (is_null(record) || !record_states_version(record, 3L)) {
    return(invisible(NULL))
  }

  stage <- record$assignment_stage
  if (
    !(length(stage) == 1L && is_integerish_numeric(stage) && stage >= 1)
  ) {
    abort_panel_record_malformed(
      "must state {.field assignment_stage} as one whole stage number of 1 or
       more",
      stage,
      what = what,
      call = call
    )
  }

  unit <- record$unit
  if (!(length(unit) == 1L && is.character(unit) && unit %in% panel_units)) {
    abort_panel_record_malformed(
      "must name {.field unit} as one of {.val {panel_units}}",
      unit,
      what = what,
      call = call
    )
  }

  if (!valid_record_columns(record$key_vars, allow_none = FALSE)) {
    abort_panel_record_malformed(
      "must list {.field key_vars} as one or more distinct column names",
      record$key_vars,
      what = what,
      call = call
    )
  }

  # Empty pool variables mean one pool while empty key variables name nothing.
  if (!valid_record_columns(record$pool_vars, allow_none = TRUE)) {
    abort_panel_record_malformed(
      "must list {.field pool_vars} as distinct column names, or none at all
       for a single pool",
      record$pool_vars,
      what = what,
      call = call
    )
  }

  invisible(NULL)
}

#' The unit vocabulary version 3 is authoritative for
#'
#' The older `psu` is not among them, since it named a stage, not a unit kind.
#' @noRd
panel_units <- c("cluster", "occurrence", "element")

#' Does the record state exactly this version?
#'
#' Compared by value, so a double from a JSON reader matches an integer.
#' @noRd
record_states_version <- function(record, version) {
  stated <- record$version
  length(stated) == 1L &&
    is_integerish_numeric(stated) &&
    as.integer(stated) == version
}

#' A list of column names, as a record may state one
#'
#' A character vector, or a list of strings as read from JSON. Every name
#' must be non-missing, non-empty and distinct.
#' @noRd
valid_record_columns <- function(x, allow_none) {
  if (is.list(x)) {
    each_a_name <- vapply(
      x,
      function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v),
      logical(1),
      USE.NAMES = FALSE
    )
    if (!all(each_a_name)) {
      return(FALSE)
    }
    x <- as.character(unlist(x, use.names = FALSE))
  }
  is.character(x) &&
    (allow_none || length(x) > 0L) &&
    !anyNA(x) &&
    all(nzchar(x)) &&
    anyDuplicated(x) == 0L
}

#' @param requirement A cli clause completing "A version-3 panel assignment".
#' @param stated The value the record carries.
#' @noRd
abort_panel_record_malformed <- function(
  requirement,
  stated,
  what,
  call = caller_env()
) {
  shown <- describe_record_value(stated)
  abort_samplyr(
    c(
      "{what} is computed from the panel assignment, and this sample's
       assignment record does not carry the version it states.",
      "x" = paste0("A version-3 panel assignment ", requirement, ". This
             record states {shown}."),
      "i" = "The version fixes which fields the record carries and what they
             mean, so a record numbered 3 is read as version 3 rather than
             repaired into it."
    ),
    class = "samplyr_error_panel_record_malformed",
    call = call
  )
}

#' A record field's value, as plain text for a diagnostic
#'
#' Plain text, since cli markup in an interpolated value prints as itself.
#' @noRd
describe_record_value <- function(value) {
  # Flatten a JSON list of strings to a character vector.
  if (
    is.list(value) &&
      all(vapply(
        value,
        function(v) is.character(v) && length(v) == 1L,
        logical(1),
        USE.NAMES = FALSE
      ))
  ) {
    value <- as.character(unlist(value, use.names = FALSE))
  }
  if (is_null(value) || length(value) == 0L) {
    return("nothing")
  }
  if (is.character(value)) {
    return(toString(ifelse(
      is.na(value), "a missing value", encodeString(value, quote = "\"")
    )))
  }
  if (length(value) > 1L) {
    return(paste0(length(value), " values of type ", typeof(value)))
  }
  if (is.atomic(value) && is.na(value)) {
    return("a missing value")
  }
  if (is.numeric(value)) {
    return(format(value))
  }
  paste0("a ", typeof(value), " value")
}

#' Resolve every pool's activation before any assignment is drawn
#'
#' A rotating pool of at most `k - r_min` units is short. A promoted pool is
#' not selection-certain, so readers branch on `activation`, not `class`.
#' @noRd
resolve_small_pools <- function(pools, spec, call = caller_env()) {
  policy <- spec$small_pool %||% "error"
  threshold <- spec$k - spec$r_min

  pools <- lapply(pools, function(pool) {
    if (identical(pool$class, "certainty")) {
      pool$activation <- "permanent"
      pool$permanent_reason <- "selection_certainty"
    } else {
      pool$activation <- "rotating"
      pool$permanent_reason <- NA_character_
    }
    pool
  })

  certain <- vapply(
    pools,
    function(pool) identical(pool$activation, "permanent"),
    logical(1)
  )
  if (isTRUE(spec$from_plan) && any(certain)) {
    abort_samplyr(
      c(
        "An {.cls svyplan_schedule} cannot assign selection-certainty units
         to its rotating panels.",
        "i" = "Execute this design with an explicit panel schedule until
               certainty-aware overlap planning is supported."
      ),
      class = "samplyr_error_plan_certainty",
      call = call
    )
  }

  # No schedule means no wave can be short.
  if (is_null(spec$schedule)) {
    return(pools)
  }

  short <- vapply(
    pools,
    function(pool) {
      identical(pool$activation, "rotating") &&
        length(pool$indices) <= threshold
    },
    logical(1)
  )
  if (!any(short)) {
    return(pools)
  }

  n_short <- sum(short)
  sizes <- vapply(pools[short], function(pool) length(pool$indices), integer(1))
  labels <- paste0(
    vapply(pools[short], format_pool_stratum, character(1)),
    " (", sizes, ")"
  )

  if (identical(policy, "error")) {
    abort_samplyr(
      c(
        "A rotation schedule must leave every pool a unit to activate.",
        "x" = "{qty(n_short)}{n_short} pool{?s} hold{?s/} at most
               {threshold} unit{?s} against {spec$k} panels and
               {.field r_min} = {spec$r_min}, so some wave would select none
               of {qty(n_short)}{?it/them}: {.val {labels}}.",
        "i" = "Use fewer panels, activate more panels per wave, take more
               units per pool, or set {.code small_pool = \"permanent\"} to
               activate {qty(n_short)}{?this pool/these pools} at every
               wave."
      ),
      class = "samplyr_error_panel_small_pool",
      call = call
    )
  }

  for (p in which(short)) {
    pools[[p]]$activation <- "permanent"
    pools[[p]]$permanent_reason <- "small_pool"
  }
  cli_warn(
    c(
      "!" = "{qty(n_short)}{n_short} pool{?s} {?is/are} too small to rotate
             and {?is/are} now active at every wave: {.val {labels}}.",
      "i" = "{qty(n_short)}{?Its/Their} units are permanent rather than
             rotating, so wave sizes, overlap and repeated interviewing all
             increase."
    ),
    class = "samplyr_warning_panel_small_pool",
    call = call
  )
  pools
}

#' A pool's stratum, as a short label
#'
#' `empty` is the label of an unstratified pool.
#' @noRd
format_pool_stratum <- function(pool, empty = "(unstratified)") {
  if (is_null(pool$stratum) || length(pool$stratum) == 0L) {
    return(empty)
  }
  paste0(
    names(pool$stratum), " = ",
    vapply(pool$stratum, function(v) format(v)[1], character(1)),
    collapse = ", "
  )
}

#' The units a wave takes from each block of a pool
#'
#' Fails with "incorrect number of dimensions" when `quotas` came back from
#' JSON as a list rather than a matrix.
#' @noRd
pool_take <- function(pool, panels) {
  as.integer(rowSums(pool$quotas[, panels, drop = FALSE]))
}

#' Fill in what an earlier record version left implicit
#'
#' Versions 1 and 2 assign from stage 1 and keep their older unit vocabulary.
#' A missing small-pool policy reads as `error`.
#' @noRd
normalize_panel_record <- function(record) {
  if (is_null(record)) {
    return(NULL)
  }
  if (is_null(record$small_pool_policy)) {
    record$small_pool_policy <- "error"
  }
  if (record_states_version(record, 1L) || record_states_version(record, 2L)) {
    record$assignment_stage <- 1L
  }
  record$pools <- lapply(record$pools, function(pool) {
    if (is_null(pool$activation)) {
      certain <- identical(pool$class, "certainty")
      pool$activation <- if (certain) "permanent" else "rotating"
      pool$permanent_reason <- if (certain) {
        "selection_certainty"
      } else {
        NA_character_
      }
    }
    pool
  })
  record
}

#' Split assignment units into frozen pools
#'
#' Strata within the realized ancestor occurrence, each split into rotating
#' and certainty parts. Pool order is deterministic for seed reproducibility.
#' @noRd
panel_pools <- function(units, context) {
  if (length(context$pool_vars) == 0L) {
    indices <- list(seq_len(nrow(units)))
    key_df <- NULL
  } else {
    groups <- split_row_indices(units, context$pool_vars)
    indices <- groups$indices
    key_df <- groups$key_df
  }

  certainty <- panel_certainty_flag(units, context$certainty_col)

  pools <- list()
  for (g in seq_along(indices)) {
    idx <- indices[[g]]
    stratum <- if (is_null(key_df)) {
      NULL
    } else {
      as.list(key_df[g, , drop = FALSE])
    }
    parts <- list(
      rotating = idx[!certainty[idx]],
      certainty = idx[certainty[idx]]
    )
    for (class in names(parts)) {
      if (length(parts[[class]]) > 0L) {
        pools[[length(pools) + 1L]] <- list(
          stratum = stratum,
          class = class,
          indices = parts[[class]]
        )
      }
    }
  }
  pools
}

#' Selection certainty at the assignment stage, and only there
#'
#' A certain ancestor says nothing about whether this stage's units rotate.
#' @noRd
panel_certainty_flag <- function(units, certainty_col) {
  if (!certainty_col %in% names(units)) {
    return(rep(FALSE, nrow(units)))
  }
  flag <- as.logical(units[[certainty_col]])
  !is.na(flag) & flag
}

#' Order the units of one pool before blocking
#'
#' Control order if declared, else realized order. The sort is stable.
#' @noRd
panel_pool_order <- function(units, idx, control_quos) {
  if (length(control_quos) == 0L || length(idx) <= 1L) {
    return(idx)
  }
  idx[control_order(units[idx, , drop = FALSE], control_quos)]
}

#' Blocked random quota labels for one pool
#'
#' @param m Pool size in assignment units.
#' @param k Number of panels.
#' @param block Block size, from `panel_block_size()`.
#' @return `panel`, the labels in pool order, and `blocks`, their sizes.
#' @noRd
assign_blocked_panels <- function(m, k, block = panel_block_size(k, 1L)) {
  if (m == 0L) {
    return(list(panel = integer(0), blocks = integer(0)))
  }

  # Permute panel labels while keeping deterministic quotas.
  labels <- sample.int(k)[rep_len(seq_len(k), m)]
  sizes <- panel_block_sizes(m, block)

  panel <- integer(m)
  start <- 0L
  for (size in sizes) {
    at <- start + seq_len(size)
    # Avoid bare `sample()` on length-one pools.
    panel[at] <- labels[at][sample.int(size)]
    start <- start + size
  }
  list(panel = panel, blocks = sizes)
}

#' Sizes of the consecutive blocks of one pool
#'
#' A pool under one block is one block. A remainder spreads over full blocks.
#' @noRd
panel_block_sizes <- function(m, block) {
  m <- as.integer(m)
  block <- as.integer(block)
  if (m < block) {
    return(m)
  }
  n_full <- m %/% block
  tail <- m %% block
  base <- tail %/% n_full
  extra <- tail %% n_full
  rep(block + base, n_full) + c(rep(1L, extra), rep(0L, n_full - extra))
}

#' Realized panel quotas of each block
#' @noRd
block_quotas <- function(panel, sizes, k) {
  quotas <- matrix(0L, nrow = length(sizes), ncol = k)
  start <- 0L
  for (b in seq_along(sizes)) {
    quotas[b, ] <- tabulate(panel[start + seq_len(sizes[b])], k)
    start <- start + sizes[b]
  }
  quotas
}
