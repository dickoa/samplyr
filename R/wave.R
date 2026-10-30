## Wave materialization

# A wave uses frozen block quotas with activation chance a_b / m_b.
# Certainty units remain active with probability one.

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
  frame_digest_given = FALSE,
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
    frame_digest_given = frame_digest_given,
    call = call
  )
  wave <- check_wave_declared(
    wave, record$schedule, window = TRUE, call = call
  )

  schedule <- record$schedule
  remaining <- setdiff(
    seq_along(get_design(master)$stages), get_stages_executed(master)
  )
  if (length(wave) > 1L) {
    if (length(remaining) > 0 && length(wave_draws(master)) == 0L) {
      abort_samplyr(
        c(
          "A window of waves needs a master whose stages are all drawn.",
          "x" = "The design has unexecuted stages: {remaining}.",
          "i" = "Draw them, {.code execute(master, listing)}, or for every
                 wave, {.code execute(master, listing, wave = t)}, then ask
                 for {.code execute(master, wave = {wave_code(wave)})}."
        ),
        class = "samplyr_error_wave_incomplete_master",
        call = call
      )
    }
    return(build_window_sample(
      master, record, wave, execution_environment,
      remaining = remaining,
      call = call
    ))
  }
  active <- schedule$panel[schedule$wave == wave & schedule$active]

  if (length(remaining) > 0) {
    return(materialize_drawn_wave(
      master, record, wave, active, remaining, execution_environment,
      call = call
    ))
  }

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
  extra = list(),
  digest = readable_digest(wave_master_digest(source))
) {
  data <- as.data.frame(source)[activation$keep, , drop = FALSE]
  data$.weight <- data$.weight * activation$factor[activation$keep]
  if (!is_null(record)) {
    data$.visit <- wave_row_visits(
      record, data$.panel, wave, activation$permanent[activation$keep]
    )
  }
  rownames(data) <- NULL

  new_wave_sample(
    source,
    record,
    data,
    wave = c(
      list(
        wave = wave,
        active_panels = activation$active,
        schedule_digest = schedule_digest,
        pools = activation$pools,
        # Bind activation to the exact master realization.
        master_digest = wave_source_digest(source)
      ),
      extra
    ),
    execution_environment = execution_environment,
    digest = digest
  )
}

#' The sample a set of waves realizes, over one master
#'
#' Each rotating unit active once in the window is drawn by one activation
#' over the union of the window's panels, with its row at the wave it is
#' active. When some panel is active at more than one wave, every unit keeps
#' a row per wave it is active, weighted by its wave weight over the number
#' of waves. Permanent units are active at every wave and always take that
#' second form.
#' @noRd
build_window_sample <- function(
  master,
  record,
  waves,
  execution_environment,
  remaining = integer(0),
  call = caller_env()
) {
  schedule <- record$schedule
  active_at <- lapply(waves, function(t) {
    schedule$panel[schedule$wave == t & schedule$active]
  })
  repeated <- anyDuplicated(unlist(active_at)) > 0L
  n_waves <- length(waves)
  union_panels <- sort(unique(unlist(active_at)))

  # A master drawn one wave at a time gives each wave its own rows.
  sources <- lapply(waves, function(t) {
    if (length(remaining) == 0L) {
      master
    } else {
      wave_draw_source(master, record, t, remaining, call = call)
    }
  })
  union <- NULL
  parts <- lapply(seq_len(n_waves), function(j) {
    # The union reaches every unit even where one wave holds none of a pool.
    activation <- activate_cohort(sources[[j]], record, active_at[[j]],
                                  allow_empty = !repeated, call = call)
    rows <- which(activation$keep)
    factor <- if (repeated) {
      activation$factor[rows] / n_waves
    } else {
      union <<- activate_cohort(sources[[j]], record, union_panels,
                                call = call)
      ifelse(activation$permanent[rows], 1 / n_waves, union$factor[rows])
    }
    part <- as.data.frame(sources[[j]])[rows, , drop = FALSE]
    part$.weight <- part$.weight * factor
    part$.wave <- rep(waves[j], nrow(part))
    part$.visit <- wave_row_visits(
      record, part$.panel, waves[j], activation$permanent[rows]
    )
    part
  })
  data <- vctrs::vec_rbind(!!!parts)
  rownames(data) <- NULL
  digest <- readable_digest(wave_rows_digest(
    master,
    vctrs::vec_rbind(!!!lapply(sources, as.data.frame))
  ))

  new_wave_sample(
    sources[[1L]],
    record,
    data,
    wave = list(
      wave = waves,
      window = if (repeated) "average" else "union",
      active_panels = union_panels,
      schedule_digest = rlang::hash(schedule),
      pools = if (!repeated) union$pools,
      master_digest = wave_source_digest(master),
      provisional = if (length(remaining) > 0L) TRUE
    ),
    execution_environment = execution_environment,
    digest = digest
  )
}

#' A wave's digest, or none when its master's cannot be read
#'
#' A wave is materialized from the master's assignment record, and an
#' unreadable digest only leaves the wave without one.
#' @noRd
readable_digest <- function(digest) {
  tryCatch(digest, error = function(e) NULL)
}

#' Wrap a wave's rows as a sample that keeps its master as phase 1
#' @noRd
new_wave_sample <- function(
  source,
  record,
  data,
  wave,
  execution_environment,
  digest = NULL
) {
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
        wave = wave,
        frame_digest = digest,
        # Retain the master as phase 1.
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

#' The execution inputs a wave refuses, and the message both routes share
#'
#' A wave materializes a stored schedule. Every input `execute()` otherwise
#' takes was resolved when the master was drawn, so each is refused by name
#' rather than accepted and dropped.
#'
#' `frame_digest` is in the list because a wave touches no frame, so there is
#' nothing to digest. It reaches this by way of `frame_digest_given`, not by
#' being `NULL`: it has a real default, so absence cannot be read off the
#' value the way it can for the other six.
#'
#' @param selects,stored_with The two phrases the master route and the program
#'   route differ on. Everything else about the refusal is the same.
#' @noRd
check_wave_extra_arguments <- function(
  frames,
  stages,
  seed,
  panels,
  panel_stage,
  small_pool,
  reps,
  frame_digest_given = FALSE,
  selects,
  stored_with,
  call = caller_env()
) {
  supplied <- c(
    if (length(frames) > 0) "a frame",
    if (!is_null(stages)) "stages",
    if (!is_null(seed)) "seed",
    if (!is_null(panels)) "panels",
    # A wave cannot revise the frozen assignment policy.
    if (!is_null(panel_stage)) "panel_stage",
    if (!is_null(small_pool)) "small_pool",
    if (!is_null(reps)) "reps",
    if (isTRUE(frame_digest_given)) "frame_digest"
  )
  if (length(supplied) == 0L) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.arg wave} selects {selects}, so it takes no further execution
       input.",
      "x" = "Also given: {supplied}.",
      if (isTRUE(frame_digest_given)) {
        c("i" = "A wave is materialized from a stored schedule and reads no
                 frame, so there is nothing for {.arg frame_digest} to
                 describe.")
      },
      "i" = "Every input a wave needs is stored with {stored_with}."
    ),
    class = "samplyr_error_wave_extra_arguments",
    call = call
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
  panel_stage = NULL,
  small_pool = NULL,
  reps,
  frame_digest_given = FALSE,
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

  check_wave_extra_arguments(
    frames = frames, stages = stages, seed = seed, panels = panels,
    panel_stage = panel_stage, small_pool = small_pool, reps = reps,
    frame_digest_given = frame_digest_given,
    selects = "units the master already assigned",
    stored_with = "the master",
    call = call
  )

  check_single_replicate(master, "execute", call = call)
  check_sample_unmodified(master, "execute", call = call)
  check_weight_contract_panel(master, "execute", call = call)

  metadata <- attr(master, "metadata")
  if (!is_null(metadata$wave)) {
    abort_samplyr(
      c(
        "This sample is already the materialization of
         {wave_phrase(metadata$wave$wave)}.",
        "i" = "Materialize each wave from the master:
               {.code execute(master, wave = t)}."
      ),
      class = "samplyr_error_wave_already_materialized",
      call = call
    )
  }


  # Validate record version before checking whether it declares waves.
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

#' Check the waves asked for against the schedule
#'
#' @param window Whether several distinct waves may be asked for at once.
#' @noRd
check_wave_declared <- function(wave, schedule, window = FALSE,
                                call = caller_env()) {
  if (!window && is.numeric(wave) && length(wave) > 1L) {
    abort_samplyr(
      c(
        "{.arg wave} takes a single wave here.",
        "i" = "A window of several waves is materialized from a master,
               {.code execute(master, wave = c(...))}, without a listing."
      ),
      class = "samplyr_error_wave_window_unsupported",
      call = call
    )
  }
  declared <- sort(unique(schedule$wave))
  ok <- is.numeric(wave) &&
    length(wave) >= 1L &&
    !anyNA(wave) &&
    is_integerish_numeric(wave) &&
    !anyDuplicated(wave)
  if (!ok || !all(as.integer(wave) %in% declared)) {
    abort_samplyr(
      c(
        "{.arg wave} must be {if (window) \"distinct waves\" else
         \"one of the waves\"} the schedule declares.",
        "i" = "This master declares
               {cli::qty(length(declared))}wave{?s} {declared}."
      ),
      class = "samplyr_error_wave_undeclared",
      call = call
    )
  }
  sort(as.integer(wave))
}

#' Waves as a message reads them: `2`, `1 and 2`, `1 to 3`, or `1, 3 and 5`
#' @noRd
wave_span <- function(waves) {
  waves <- sort(as.integer(waves))
  n <- length(waves)
  if (n == 1L) {
    return(as.character(waves))
  }
  if (n > 2L && all(diff(waves) == 1L)) {
    return(paste(waves[1L], "to", waves[n]))
  }
  paste(paste(waves[-n], collapse = ", "), "and", waves[n])
}

#' @noRd
wave_phrase <- function(waves) {
  paste(if (length(waves) == 1L) "wave" else "waves", wave_span(waves))
}

#' Waves as code that asks for them again: `2`, `1:3`, or `c(1, 3)`
#' @noRd
wave_code <- function(waves) {
  waves <- sort(as.integer(waves))
  n <- length(waves)
  if (n == 1L) {
    return(as.character(waves))
  }
  if (all(diff(waves) == 1L)) {
    return(paste0(waves[1L], ":", waves[n]))
  }
  paste0("c(", paste(waves, collapse = ", "), ")")
}

#' Does this sample pool several waves?
#' @noRd
is_wave_window <- function(x) {
  length(attr(x, "metadata")[["wave"]]$wave) > 1L
}

#' The sample rows each pool of an assignment record holds
#'
#' One match of the keys against every pool's keys, split by pool. Keys
#' that are not all character, or a key listed twice, are matched pool by
#' pool instead, since each pool then reads its own first match.
#' @return One element per pool: `rows`, the sample rows in the pool in
#'   order, and `at`, each row's position in the pool's keys.
#' @noRd
record_pool_rows <- function(keys, pools) {
  pool_keys <- lapply(pools, function(pool) pool$keys)
  flat <- unlist(pool_keys, use.names = FALSE)
  character_keys <- vapply(
    pool_keys,
    function(k) is_null(k) || is.character(k),
    logical(1)
  )
  one_match <- is.character(keys) && all(character_keys) &&
    anyDuplicated(flat) == 0L
  if (!one_match) {
    return(lapply(pool_keys, function(k) {
      at <- match(keys, k)
      rows <- which(!is.na(at))
      list(rows = rows, at = at[rows])
    }))
  }
  sizes <- lengths(pool_keys)
  hit <- match(keys, flat)
  rows <- which(!is.na(hit))
  hit <- hit[rows]
  offset <- cumsum(c(0L, sizes))
  pool_of <- rep.int(seq_along(pool_keys), sizes)[hit]
  by_pool <- split(
    seq_along(rows),
    factor(pool_of, levels = seq_along(pool_keys))
  )
  lapply(seq_along(pool_keys), function(p) {
    i <- by_pool[[p]]
    list(rows = rows[i], at = hit[i] - offset[p])
  })
}

#' Which rows an activation keeps, and at what conditional probability
#'
#' @param record The cohort's assignment record, or `NULL` for a cohort drawn
#'   whole. A cohort that was never partitioned has one implicit panel
#'   comprising all its rows, and activating it is not a subsample, so its
#'   factor is one.
#' @param allow_empty Whether a block with no active unit is kept out
#'   rather than refused, for a wave read inside a window.
#' @return A list with `keep`, the row mask, `factor`, the weight multiplier
#'   for kept rows, `active`, the activated panels, and `pools`, the per-block
#'   activation record.
#' @noRd
activate_cohort <- function(sample, record, active, allow_empty = FALSE,
                            call = caller_env()) {
  record <- prepare_panel_record(record, "An activation", call = call)
  if (is_null(record)) {
    n <- nrow(sample)
    live <- length(active) > 0L
    return(list(
      keep = rep(live, n),
      factor = rep(1, n),
      permanent = rep(FALSE, n),
      active = if (live) 1L else integer(0),
      pools = list()
    ))
  }

  data <- as.data.frame(sample)
  keys <- make_group_key(data, record$key_vars)
  panel <- data$.panel

  keep <- rep(FALSE, nrow(data))
  factor <- rep(NA_real_, nrow(data))
  permanent <- rep(FALSE, nrow(data))
  pools <- vector("list", length(record$pools))
  located <- record_pool_rows(keys, record$pools)

  for (p in seq_along(record$pools)) {
    pool <- record$pools[[p]]
    rows <- located[[p]]$rows

    if (identical(pool$activation, "permanent")) {
      # Exclude permanent pools from randomized quota denominators.
      keep[rows] <- TRUE
      factor[rows] <- 1
      permanent[rows] <- TRUE
      take <- pool$blocks
      probability <- rep(1, length(pool$blocks))
    } else {
      take <- pool_take(pool, active)
      # Reporting periods leave a wave empty legitimately, and older records
      # did not enforce positive activation.
      if (any(take == 0L) && !allow_empty) {
        abort_samplyr(
          c(
            "This wave cannot be materialized: some units have no chance of
             being selected for it.",
            "x" = "Pool {.val {format_pool_stratum(pool)}} has
                   {sum(take == 0L)} block{?s} with no active unit in this
                   wave.",
            "i" = if (record_states_version(record, 6L)) {
              "The master leaves every pool a unit in each reporting
               period, not at each wave. Materialize the period as a window
               of its waves, {.code execute(master, wave = c(...))}."
            } else {
              "The master was drawn before this check existed. Redraw it
               with fewer panels, more panels active per wave, a larger take
               per pool, or {.code small_pool = \"permanent\"}."
            }
          ),
          class = "samplyr_error_panel_small_pool",
          call = call
        )
      }
      probability <- take / pool$blocks
      block_of_unit <- rep(seq_along(pool$blocks), pool$blocks)
      block <- block_of_unit[located[[p]]$at]
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

  list(
    keep = keep,
    factor = factor,
    permanent = permanent,
    active = active,
    pools = pools
  )
}

#' The visit each row of a wave is at
#'
#' Read from the schedule by panel. A permanent unit is active at every wave
#' outside the rotation, so it has no visit number.
#' @noRd
wave_row_visits <- function(record, panel, wave, permanent) {
  # paste() would recycle no panels to one key.
  if (length(panel) == 0L) {
    return(integer(0))
  }
  schedule <- record$schedule
  at <- match(
    paste(panel, wave, sep = "|"),
    paste(schedule$panel, schedule$wave, sep = "|")
  )
  visit <- as.integer(schedule$visit[at])
  visit[permanent] <- NA_integer_
  visit
}
