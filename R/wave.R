## Wave materialization

# A wave is a probability subsample using the schedule and block quotas frozen
# in the master receipt. Its activation chance is a_b / m_b. Certainty units
# remain active with probability one.

#' Materialize one scheduled wave of a rotating master
#' @noRd
materialize_wave <- function(
  master,
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
  record <- check_wave_call(
    master,
    frames = frames,
    stages = stages,
    seed = seed,
    panels = panels,
    panel_stage = panel_stage,
    small_pool = small_pool,
    reps = reps,
    call = call
  )
  wave <- check_wave_declared(wave, record$schedule, call = call)

  schedule <- record$schedule
  active <- schedule$panel[schedule$wave == wave & schedule$active]

  build_wave_sample(
    source = master,
    record = record,
    wave = wave,
    activation = activate_cohort(master, record, active, call = call),
    schedule_digest = rlang::hash(record$schedule),
    execution_environment = execution_environment
  )
}

#' Build the sample one activation realizes
#'
#' Shared by the single-master route and by each cohort of a rotation
#' program, which differ only in where the active panel set comes from.
#' @noRd
build_wave_sample <- function(
  source,
  record,
  wave,
  activation,
  schedule_digest,
  execution_environment,
  extra = list()
) {
  data <- as.data.frame(source)[activation$keep, , drop = FALSE]
  data$.weight <- data$.weight * activation$factor[activation$keep]
  rownames(data) <- NULL

  design <- get_design(source)
  stages_executed <- get_stages_executed(source)

  new_tbl_sample(
    data = data,
    design = design,
    stages_executed = stages_executed,
    seed = attr(source, "seed"),
    metadata = c(
      list(
        n_selected = nrow(data),
        executed_at = Sys.time(),
        panels = record$panels,
        panel_assignment = record,
        wave = c(
          list(
            wave = wave,
            active_panels = activation$active,
            schedule_digest = schedule_digest,
            pools = activation$pools,
            # Which realization the activation was computed against. The
            # phase-1 link is what an export reads its non-active rows from,
            # and `.sample_id` is a row position rather than an identity, so
            # a substituted sample of the same shape would otherwise line up.
            master_digest = wave_source_digest(source)
          ),
          extra
        ),
        # Retain the master as phase 1. Activation is not a new design execution.
        prev_phase = list(
          transition = "panel_activation",
          sample = source,
          design = design,
          stages = stages_executed
        ),
        execution_environment = execution_environment,
        integrity = sample_integrity_record(data, design, stages_executed)
      )
    )
  )
}

#' Fingerprint of the realization a wave was activated from
#'
#' Uses metadata frozen at execution so later analysis columns or row order do
#' not change identity. Frame, execution, seed, and integrity metadata jointly
#' distinguish realizations.
#' @noRd
wave_source_digest <- function(source) {
  metadata <- attr(source, "metadata")
  rlang::hash(list(
    integrity = metadata$integrity,
    frame_digest = metadata$frame_digest,
    executed_at = metadata$executed_at,
    seed = attr(source, "seed")
  ))
}

#' Guards for the wave route
#'
#' `execute(sample, ...)` already means two-phase or continuation, so the wave
#' route has to be unambiguous rather than merely distinguishable: it accepts
#' a scheduled master and a wave number, and nothing else.
#' @noRd
check_wave_call <- function(
  master,
  frames,
  stages,
  seed,
  panels,
  panel_stage = NULL,
  small_pool = NULL,
  reps,
  call = caller_env()
) {
  if (!is_tbl_sample(master)) {
    abort_samplyr(
      c(
        "{.arg wave} materializes a stored rotation schedule, so the first
         argument must be an executed sample.",
        "i" = "Draw the master first:
               {.code execute(design, frame, panels = <schedule>)}."
      ),
      class = "samplyr_error_wave_not_a_sample",
      call = call
    )
  }

  supplied <- c(
    if (length(frames) > 0) "a frame",
    if (!is_null(stages)) "stages",
    if (!is_null(seed)) "seed",
    if (!is_null(panels)) "panels",
    # The assignment stage and the small-pool policy were both resolved when
    # the master was drawn and are recorded with the assignment, so a wave
    # cannot revisit either.
    if (!is_null(panel_stage)) "panel_stage",
    if (!is_null(small_pool)) "small_pool",
    if (!is_null(reps)) "reps"
  )
  if (length(supplied) > 0) {
    abort_samplyr(
      c(
        "{.arg wave} selects units the master already assigned, so it takes
         no further execution input.",
        "x" = "Also given: {supplied}.",
        "i" = "Every input a wave needs is stored with the master."
      ),
      class = "samplyr_error_wave_extra_arguments",
      call = call
    )
  }

  check_single_replicate(master, "execute", call = call)
  check_sample_unmodified(master, "execute", call = call)
  check_weight_contract_panel(master, "execute", call = call)

  metadata <- attr(master, "metadata")
  if (!is_null(metadata$wave)) {
    abort_samplyr(
      c(
        "This sample is already the materialization of wave
         {metadata$wave$wave}.",
        "i" = "Materialize each wave from the master:
               {.code execute(master, wave = t)}."
      ),
      class = "samplyr_error_wave_already_materialized",
      call = call
    )
  }

  design <- get_design(master)
  remaining <- setdiff(seq_along(design$stages), get_stages_executed(master))
  if (length(remaining) > 0) {
    abort_samplyr(
      c(
        "A wave can only be materialized from a completed master.",
        "x" = "The design has unexecuted stages: {remaining}.",
        "i" = "Finish the design first:
               {.code execute(master, <listing frame>)}."
      ),
      class = "samplyr_error_wave_incomplete_master",
      call = call
    )
  }

  # Whether a record declares waves is a fact about its fields, so the law
  # those fields were written under is established first. Otherwise a record
  # this build cannot read is reported as one that declares no waves, which
  # names the wrong problem and suggests redrawing with a schedule.
  record <- metadata$panel_assignment
  if (!is_null(record)) {
    record <- prepare_panel_record(record, "An activation", call = call)
  }
  if (is_null(record) || is_null(record$schedule)) {
    abort_samplyr(
      c(
        "This sample carries no rotation schedule.",
        "i" = "A schedule is declared at the master draw:
               {.code execute(design, frame, panels = <data frame with
               panel, wave and active>)}.",
        if (!is_null(record)) c(
          "i" = "{.code panels = {record$panels}} partitions the sample but
                 declares no waves."
        )
      ),
      class = "samplyr_error_wave_no_schedule",
      call = call
    )
  }

  record
}

#' @noRd
check_wave_declared <- function(wave, schedule, call = caller_env()) {
  declared <- sort(unique(schedule$wave))
  ok <- is.numeric(wave) &&
    length(wave) == 1L &&
    !anyNA(wave) &&
    is_integerish_numeric(wave)
  if (!ok || !as.integer(wave) %in% declared) {
    abort_samplyr(
      c(
        "{.arg wave} must be one of the waves the schedule declares.",
        "i" = "This master declares
               {cli::qty(length(declared))}wave{?s} {declared}."
      ),
      class = "samplyr_error_wave_undeclared",
      call = call
    )
  }
  as.integer(wave)
}

#' Which rows an activation keeps, and at what conditional probability
#'
#' @param record The cohort's assignment record, or `NULL` for a cohort drawn
#'   whole. A cohort that was never partitioned has one implicit panel
#'   comprising all its rows, and activating it is not a subsample, so its
#'   factor is one.
#' @return A list with `keep`, the row mask, `factor`, the weight multiplier
#'   for kept rows, `active`, the activated panels, and `pools`, the per-block
#'   activation record.
#' @noRd
activate_cohort <- function(sample, record, active, call = caller_env()) {
  record <- prepare_panel_record(record, "An activation", call = call)
  if (is_null(record)) {
    n <- nrow(sample)
    live <- length(active) > 0L
    return(list(
      keep = rep(live, n),
      factor = rep(1, n),
      active = if (live) 1L else integer(0),
      pools = list()
    ))
  }

  data <- as.data.frame(sample)
  keys <- make_group_key(data, record$key_vars)
  panel <- data$.panel

  keep <- rep(FALSE, nrow(data))
  factor <- rep(NA_real_, nrow(data))
  pools <- vector("list", length(record$pools))

  for (p in seq_along(record$pools)) {
    pool <- record$pools[[p]]
    at <- match(keys, pool$keys)
    rows <- which(!is.na(at))

    if (identical(pool$activation, "permanent")) {
      # Permanent pools stay outside the randomized quota denominator.
      keep[rows] <- TRUE
      factor[rows] <- 1
      take <- pool$blocks
      probability <- rep(1, length(pool$blocks))
    } else {
      take <- as.integer(rowSums(pool$quotas[, active, drop = FALSE]))
      # Backstop for older records that did not enforce positive activation.
      if (any(take == 0L)) {
        abort_samplyr(
          c(
            "This wave cannot be materialized: some units have no chance of
             being selected for it.",
            "x" = "Pool {.val {describe_pool_stratum(pool)}} has
                   {sum(take == 0L)} block{?s} with no active unit in this
                   wave.",
            "i" = "The master was drawn before this check existed. Redraw it
                   with fewer panels, more panels active per wave, a larger
                   take per pool, or {.code small_pool = \"permanent\"}."
          ),
          class = "samplyr_error_panel_small_pool",
          call = call
        )
      }
      probability <- take / pool$blocks
      block_of_unit <- rep(seq_along(pool$blocks), pool$blocks)
      block <- block_of_unit[at[rows]]
      selected <- panel[rows] %in% active
      keep[rows[selected]] <- TRUE
      factor[rows[selected]] <- 1 / probability[block[selected]]
    }

    pools[[p]] <- list(
      stratum = pool$stratum,
      class = pool$class,
      activation = pool$activation,
      permanent_reason = pool$permanent_reason,
      blocks = pool$blocks,
      take = take,
      probability = probability
    )
  }

  list(keep = keep, factor = factor, active = active, pools = pools)
}
