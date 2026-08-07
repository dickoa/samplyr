## Wave materialization

# `execute(master, wave = t)` realizes one precommitted occasion of a
# scheduled master. It selects the panels the stored schedule declares active
# at `t`, and compounds the activation factor into `.weight`.
#
# The activation is a probability subsample, not a filter. Within block `b` of
# an assignment pool the master froze quotas `q_bg`, so activating the panel
# set `A` takes `a_b = sum(q_bg for g in A)` of the block's `m_b` units, and
# does so as a simple random sample without replacement. The unit's
# conditional probability is therefore `a_b / m_b`, and the factor applied to
# its weight is the inverse. Permanent certainty units are activated at every
# wave with probability one and are not part of any block's take.
#
# The schedule is read from the receipt the master wrote, never recomputed
# against a later frame vintage: a stratifier a unit can drift out of would
# otherwise change `m_b` between the draw and the wave.

#' Materialize one scheduled wave of a rotating master
#' @noRd
materialize_wave <- function(
  master,
  wave,
  frames,
  stages,
  seed,
  panels,
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
    reps = reps,
    call = call
  )
  wave <- check_wave_declared(wave, record$schedule, call = call)

  activation <- wave_activation(master, record, wave)
  data <- as.data.frame(master)[activation$keep, , drop = FALSE]
  data$.weight <- data$.weight * activation$factor[activation$keep]
  rownames(data) <- NULL

  design <- get_design(master)
  stages_executed <- get_stages_executed(master)

  new_tbl_sample(
    data = data,
    design = design,
    stages_executed = stages_executed,
    seed = attr(master, "seed"),
    metadata = list(
      n_selected = nrow(data),
      executed_at = Sys.time(),
      panels = record$panels,
      panel_assignment = record,
      wave = list(
        wave = wave,
        active_panels = activation$active,
        schedule_digest = rlang::hash(record$schedule),
        pools = activation$pools
      ),
      # The master's own metadata, kept whole. A wave is derived from one
      # recorded execution and cannot be replayed as a call of its own.
      materialized_from = attr(master, "metadata"),
      execution_environment = execution_environment,
      integrity = sample_integrity_record(data, design, stages_executed)
    )
  )
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

  record <- metadata$panel_assignment
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

#' Which rows a wave activates, and at what conditional probability
#'
#' @return `keep`, a row mask; `factor`, the weight multiplier for the kept
#'   rows; `active`, the activated panels; and `pools`, the per-block
#'   activation record.
#' @noRd
wave_activation <- function(master, record, wave) {
  schedule <- record$schedule
  active <- schedule$panel[schedule$wave == wave & schedule$active]

  data <- as.data.frame(master)
  keys <- make_group_key(data, record$key_vars)
  panel <- data$.panel

  keep <- rep(FALSE, nrow(data))
  factor <- rep(NA_real_, nrow(data))
  pools <- vector("list", length(record$pools))

  for (p in seq_along(record$pools)) {
    pool <- record$pools[[p]]
    at <- match(keys, pool$keys)
    rows <- which(!is.na(at))

    if (identical(pool$class, "certainty")) {
      # Permanent by policy: in the sample at every wave, and outside the
      # randomized quota denominator.
      keep[rows] <- TRUE
      factor[rows] <- 1
      take <- pool$blocks
      probability <- rep(1, length(pool$blocks))
    } else {
      take <- as.integer(rowSums(pool$quotas[, active, drop = FALSE]))
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
      blocks = pool$blocks,
      take = take,
      probability = probability
    )
  }

  list(keep = keep, factor = factor, active = active, pools = pools)
}
