## Drawing the later stages one wave at a time

#' Draw the remaining stages under the units of one wave
#'
#' `execute(master, listing, wave = t)` runs an ordinary continuation on a
#' view of the master that holds only the units active at wave `t`, so every
#' check and message of a continuation applies. The rows it draws are kept
#' with the master, whose own rows do not change. The call that draws the
#' last wave returns the completed sample.
#' @noRd
draw_wave <- function(master, wave, frames, stages, seed, panels,
                      panel_stage, small_pool, reps,
                      frame_digest_given = FALSE, call = caller_env()) {
  record <- check_wave_draw_call(
    master, stages = stages, panels = panels, panel_stage = panel_stage,
    small_pool = small_pool, reps = reps,
    frame_digest_given = frame_digest_given, call = call
  )
  wave <- check_wave_declared(wave, record$schedule, call = call)
  draws <- wave_draws(master)
  drawn <- wave_draws_drawn(draws)
  if (wave %in% drawn) {
    abort_samplyr(
      c(
        "The later stages of wave {wave} are already drawn.",
        "i" = "Each wave is drawn once. Drawn so far:
               {cli::qty(length(drawn))}wave{?s} {sort(drawn)}."
      ),
      class = "samplyr_error_wave_already_drawn",
      call = call
    )
  }

  opening <- wave_opening_panels(record, wave)
  permanent <- panel_permanent_rows(master, record)
  # Permanent units hold one tenure, from the first wave drawn.
  with_permanent <- any(permanent) &&
    !any(vapply(draws, function(d) isTRUE(d$permanent), logical(1)))
  keep <- (master$.panel %in% opening & !permanent) |
    (with_permanent & permanent)
  if (!any(keep)) {
    abort_wave_opens_nothing(master, record, wave, opening, drawn, call = call)
  }
  view <- wave_draw_view(master, keep)
  prior <- master_executed_digest(master)
  exclusion <- tenure_exclusion(master, record, draws, frames, view,
                                call = call)
  frames <- exclusion$frames
  # The inner call holds the listing as a value, so an error raised there
  # is reported against the call the user wrote.
  piece <- withCallingHandlers(
    rlang::inject(execute(
      view, !!!frames, stages = stages, seed = seed,
      frame_digest = prior$privacy$mode %||% "none"
    )),
    rlang_error = function(cnd) {
      if (is.call(cnd$call) && identical(cnd$call[[1L]], quote(execute))) {
        cnd$call <- rlang::frame_call(call)
        rlang::cnd_signal(cnd)
      }
    }
  )

  meta <- attr(piece, "metadata")
  master_parents <- attr(master, "metadata")$empty_parents
  draw <- list(
    wave = wave,
    active_panels = opening,
    permanent = with_permanent,
    seed = attr(piece, "seed"),
    stages = setdiff(get_stages_executed(piece), get_stages_executed(master)),
    frame_schedule = meta$frame_schedule,
    execution_environment = meta$execution_environment,
    executed_at = meta$executed_at,
    rows = tenure_reweight(as.data.frame(piece), exclusion$pools),
    empty_parents = utils::tail(
      meta$empty_parents,
      length(meta$empty_parents) - length(master_parents)
    ),
    design = get_design(piece),
    frame_digest = wave_draw_digest_part(prior, get_frame_digest(piece))
  )
  if (schedule_has_tenures(record$schedule)) {
    draw$frame_digest <- record_tenure_exclusion(
      draw$frame_digest, prior, view, exclusion$pools, get_design(piece)
    )
  }
  draws <- c(draws, list(draw))

  # A panel with several tenures keeps drawing, so its master never closes.
  pending <- setdiff(wave_draws_required(master, record), c(drawn, wave))
  if (length(pending) == 0L && !schedule_has_tenures(record$schedule)) {
    return(complete_wave_draws(master, draws))
  }
  meta <- attr(master, "metadata")
  meta$per_wave_draws <- draws
  # The master's digest then records every tenure drawn so far.
  if (schedule_has_tenures(record$schedule) && !is_null(prior)) {
    meta$frame_digest <- tenure_master_digest(master, draws) %||% prior
  }
  attr(master, "metadata") <- meta
  master
}

#' Guards for drawing the later stages of one wave
#'
#' The master must be a single unmodified sample with a periods assignment.
#' `execute()` routes here only with stages left to draw. Inputs fixed at the
#' master draw are refused by name. `stages` is accepted when it names every remaining stage, which a
#' single listing for several stages needs.
#' @noRd
check_wave_draw_call <- function(master, stages, panels, panel_stage,
                                 small_pool, reps, frame_digest_given = FALSE,
                                 call = caller_env()) {
  check_single_replicate(master, "execute", call = call)
  check_sample_unmodified(master, "execute", call = call)

  metadata <- attr(master, "metadata")

  supplied <- c(
    if (!is_null(panels)) "panels",
    if (!is_null(panel_stage)) "panel_stage",
    if (!is_null(small_pool)) "small_pool",
    if (!is_null(reps)) "reps",
    if (isTRUE(frame_digest_given)) "frame_digest"
  )
  if (length(supplied) > 0L) {
    abort_samplyr(
      c(
        "Drawing a wave's later stages takes a listing, {.arg seed} and
         {.arg stages} only.",
        "x" = "Also given: {supplied}.",
        "i" = "The panel assignment was fixed when the master was drawn.",
        if (isTRUE(frame_digest_given)) {
          c("i" = "The frame digest extends the master's.")
        }
      ),
      class = "samplyr_error_wave_extra_arguments",
      call = call
    )
  }

  design <- get_design(master)
  remaining <- remaining_stages(master)
  if (!is_null(stages)) {
    stages <- normalize_stage_selector(
      stages, seq_along(design$stages), what = "stages of this design",
      call = call
    )
    if (!setequal(stages, remaining)) {
      abort_samplyr(
        c(
          "A wave's draw covers every remaining stage.",
          "x" = "{.arg stages} names {stages}, and the remaining
                 {cli::qty(length(remaining))}stage{?s} {?is/are}
                 {remaining}.",
          "i" = "Omit {.arg stages}, or give {.code stages = {remaining}}."
        ),
        class = "samplyr_error_wave_draw_stages",
        call = call
      )
    }
  }

  record <- metadata$panel_assignment
  if (!is_null(record)) {
    record <- prepare_panel_record(record, "A wave draw", call = call)
  }
  if (is_null(record) || is_null(record$schedule)) {
    abort_samplyr(
      c(
        "Later stages are drawn one wave at a time only for a master with a
         rotation schedule or {.code panels = periods(k)}.",
        "x" = if (is_null(record)) {
          "This sample carries no panel assignment."
        } else {
          "This sample's panels declare no waves."
        },
        "i" = "Draw the later stages in one call:
               {.code execute(master, listing)}."
      ),
      class = "samplyr_error_wave_draw_unsupported",
      call = call
    )
  }
  record
}

#' Refuse a wave that opens no tenure, naming where its units come from
#' @noRd
abort_wave_opens_nothing <- function(master, record, wave, opening, drawn,
                                     call = caller_env()) {
  active <- wave_active_panels(record, wave)
  present <- intersect(active, unique(master$.panel))
  sources <- sort(unique(tenure_opening(record, present, wave)))
  done <- intersect(sources, drawn)
  abort_samplyr(
    c(
      if (length(opening) == 0L) {
        "Wave {wave} opens no tenure, so it has nothing to draw."
      } else {
        "No unit of the panels opening a tenure at wave {wave} is in the
         master, so it has nothing to draw."
      },
      "i" = if (length(sources) == 0L) {
        "No unit of the master is active at wave {wave}."
      } else if (length(done) == length(sources)) {
        "Its units were drawn at {wave_phrase(sources)}, and
         {.code execute(master, wave = {wave})} gives the wave."
      } else {
        "Its units are drawn at {wave_phrase(sources)}, with
         {.code execute(master, listing, wave = t)} for each, and then
         {.code execute(master, wave = {wave})} gives the wave."
      }
    ),
    class = "samplyr_error_wave_already_drawn",
    call = call
  )
}

#' Leave out of a tenure's listing the units earlier tenures drew
#'
#' Only under a schedule with tenures, and only in a primary unit drawn
#' before. Each pool's count of units left out is kept, so the draw can be
#' weighted to the whole listing. A primary unit drawn before with none of
#' its earlier units in the listing is refused: that listing was cut
#' already, and nothing would count those units back.
#' @return The frames to draw from and the pools' counts, or `NULL` counts
#'   when nothing is left out.
#' @noRd
tenure_exclusion <- function(master, record, draws, frames, view,
                             call = caller_env()) {
  # A replay passes the call's frames as one list.
  if (length(frames) == 1L && is.list(frames[[1L]]) &&
        !is.data.frame(frames[[1L]])) {
    frames <- frames[[1L]]
  }
  none <- list(frames = frames, pools = NULL)
  if (!schedule_has_tenures(record$schedule) || length(draws) == 0L ||
        length(frames) == 0L) {
    return(none)
  }
  design <- get_design(master)
  stage <- min(remaining_stages(master))
  spec <- design$stages[[stage]]
  listing <- as.data.frame(frames[[1L]])
  # The listing keys its rows to the master's units as the draw does, by
  # the unit variables it carries, so strata may be left out of it. The
  # draw then carries them down from the master, and so does this count.
  psu_vars <- intersect(record$key_vars, names(listing))
  strata_vars <- spec$strata$vars %||% character(0)
  listing <- carry_unit_columns(listing, master, psu_vars, strata_vars)
  # Clusters lie within strata, so the strata name the same units and keep
  # apart ids numbered within each stratum.
  unit_vars <- unique(c(psu_vars, strata_vars, spec$clusters$vars))
  pool_vars <- c(psu_vars, strata_vars)
  earlier <- dplyr::bind_rows(lapply(draws, `[[`, "rows"))
  # A listing without them is refused by the draw itself.
  if (length(psu_vars) == 0L || !all(unit_vars %in% names(listing))) {
    return(none)
  }

  used <- make_group_key(earlier, unit_vars)
  listed <- make_group_key(listing, unit_vars)
  dropped <- listed %in% used
  psu_listed <- make_group_key(listing, psu_vars)
  psu_used <- make_group_key(earlier, psu_vars)
  kept_psu <- unique(psu_listed[dropped])
  psu_now <- make_group_key(as.data.frame(view), psu_vars)
  drawn_now <- intersect(unique(psu_now), unique(psu_used))
  cut <- setdiff(intersect(drawn_now, unique(psu_listed)), kept_psu)
  if (length(cut) > 0L) {
    abort_samplyr(
      c(
        "The listing for this tenure leaves out every unit earlier tenures
         drew in {length(cut)} primary unit{?s}.",
        "i" = "Pass the full listing. samplyr leaves out the units drawn
               before and weights the new tenure to the whole listing, so
               that it estimates the whole population."
      ),
      class = "samplyr_error_tenure_listing",
      call = call
    )
  }
  if (!any(dropped)) {
    return(none)
  }

  # Units, not rows, are counted: a listing may hold several rows per unit.
  units <- !duplicated(listed)
  pool <- make_group_key(listing, pool_vars)
  remaining <- c(tapply(units & !dropped, pool, sum))
  left_out <- c(tapply(units & dropped, pool, sum))
  frames[[1L]] <- frames[[1L]][!dropped, , drop = FALSE]
  excluded_key <- tenure_pool_key(psu_listed, listing[strata_vars])
  list(
    frames = frames,
    pools = list(
      vars = pool_vars,
      stage = stage,
      factor = (remaining + left_out) / remaining,
      psu_vars = psu_vars,
      strata_vars = strata_vars,
      excluded = c(tapply(units & dropped, excluded_key, sum))
    )
  )
}

#' A tenure pool's key from its primary unit's key and its strata
#'
#' Strata are read as text, so a pool of the listing and the same pool of
#' the digest, which stores them as text, get one key.
#' @noRd
tenure_pool_key <- function(psu_key, strata) {
  df <- data.frame(.psu = psu_key, stringsAsFactors = FALSE)
  for (v in names(strata)) {
    df[[v]] <- as.character(strata[[v]])
  }
  make_group_key(df, names(df))
}

#' Record in a tenure's digest the units its exclusion left out
#'
#' Each pool of the tenure stage gets `n_excluded`, the units of its listing
#' that earlier tenures drew. The recorded chances stay those of the draw,
#' over the `N` units left, and `chance * N / (N + n_excluded)` is the
#' chance the weights use. A first tenure, and every stage below the tenure
#' stage, leave none out.
#' @noRd
record_tenure_exclusion <- function(part, prior, view, pools, design) {
  if (is_null(part)) {
    return(part)
  }
  part$stages <- lapply(part$stages, function(stage) {
    n <- rep(0L, nrow(stage$pools))
    if (!is_null(pools) && identical(stage$stage_id, pools$stage)) {
      n <- tenure_pools_excluded(stage, prior, view, pools, design)
    }
    stage$pools$n_excluded <- n
    stage
  })
  part
}

#' The units left out of each pool of a tenure stage
#'
#' A pool is found by its parent, the master's unit above it, and its
#' strata. `NA` for a pool whose parent the master's digest does not name.
#' @noRd
tenure_pools_excluded <- function(stage, prior, view, pools, design) {
  ids <- vapply(prior$stages, function(s) s$stage_id, integer(1))
  parent <- prior$stages[[match(stage$stage_id - 1L, ids)]]
  view <- as.data.frame(view)
  vars <- collect_ancestor_cluster_vars(design, stage$stage_id)
  view_keys <- digest_path_keys(view, seq_len(nrow(view)), vars)
  sel <- parent$selected
  at <- match(
    sel$key[match(stage$pools$parent_unit, sel$unit_id)],
    view_keys
  )
  psu_key <- make_group_key(
    view[at, pools$psu_vars, drop = FALSE], pools$psu_vars
  )
  key <- tenure_pool_key(psu_key, stage$pools[pools$strata_vars])
  n <- as.integer(unname(pools$excluded[key]))
  n[is.na(at)] <- NA_integer_
  n
}

#' Copy columns of the master's primary units onto a listing
#'
#' Only columns the listing lacks. A listing row under no unit of the
#' master gets `NA`, and no draw reaches it.
#' @noRd
carry_unit_columns <- function(listing, master, by, vars) {
  vars <- setdiff(intersect(vars, names(master)), names(listing))
  if (length(vars) == 0L || length(by) == 0L) {
    return(listing)
  }
  units <- as.data.frame(master)[c(by, vars)]
  unit_key <- make_group_key(units, by)
  keep <- !duplicated(unit_key)
  at <- match(make_group_key(listing, by), unit_key[keep])
  for (v in vars) {
    listing[[v]] <- units[[v]][keep][at]
  }
  listing
}

#' Weight a tenure's draw to the whole listing
#'
#' An equal-probability draw of `n` from the `R` units left in a pool gives
#' each unit of the listing the chance `n / (R + U)`, with `U` the units
#' left out, so the stage's weight and population count are scaled by
#' `(R + U) / R`.
#' @noRd
tenure_reweight <- function(rows, pools) {
  if (is_null(pools)) {
    return(rows)
  }
  key <- make_group_key(rows, pools$vars)
  factor <- unname(pools$factor[key])
  if (anyNA(factor)) {
    cli_abort(
      c(
        "Internal error: a tenure drew rows in a pool without a unit count.",
        "i" = "Pools without a count: {.val {unique(key[is.na(factor)])}}."
      ),
      call = NULL,
      class = "samplyr_error_internal"
    )
  }
  for (col in c(".weight", paste0(c(".weight_", ".fpc_"), pools$stage))) {
    if (col %in% names(rows)) {
      rows[[col]] <- rows[[col]] * factor
    }
  }
  rows
}

#' The master restricted to one wave's units, as a sample in its own right
#'
#' Its integrity record is rebuilt for the rows it keeps, so the continuation
#' reads it as unmodified. It keeps the master's frame digest, which the
#' continuation extends with the wave's later stages. The draws stay with the
#' master.
#' @noRd
wave_draw_view <- function(master, keep) {
  data <- as.data.frame(master)[keep, , drop = FALSE]
  rownames(data) <- NULL
  design <- get_design(master)
  stages <- get_stages_executed(master)
  meta <- attr(master, "metadata")
  meta$per_wave_draws <- NULL
  meta$frame_digest <- master_executed_digest(master)
  # The view is how one tenure is drawn, so the tenure guard lets it through.
  meta$tenure_view <- TRUE
  meta$integrity <- sample_integrity_record(data, design, stages)
  new_tbl_sample(
    data = data,
    design = design,
    stages_executed = stages,
    seed = attr(master, "seed"),
    metadata = meta
  )
}

#' Bind every wave's rows into the completed sample
#'
#' Rows follow the master's order of their parent units. The sample is a
#' continuation of the master, and keeps each wave's call, without its rows,
#' for the receipt. Its frame digest merges the waves' digests.
#' @noRd
complete_wave_draws <- function(master, draws) {
  design <- draws[[length(draws)]]$design
  executed <- get_stages_executed(master)
  stages <- sort(unique(c(executed, draws[[1]]$stages)))
  meta <- attr(master, "metadata")

  rows <- dplyr::bind_rows(lapply(draws, `[[`, "rows"))
  key_vars <- collect_ancestor_cluster_vars(design, max(executed) + 1L)
  key_vars <- intersect(key_vars, names(rows))
  if (length(key_vars) > 0L) {
    parent <- match(
      make_group_key(rows, key_vars),
      make_group_key(as.data.frame(master), key_vars)
    )
    rows <- rows[order(parent, seq_len(nrow(rows))), , drop = FALSE]
  }
  rownames(rows) <- NULL
  rows$.sample_id <- seq_len(nrow(rows))

  base <- master
  base_meta <- meta
  base_meta$per_wave_draws <- NULL
  attr(base, "metadata") <- base_meta
  last <- draws[[length(draws)]]

  digest <- NULL
  prior <- master_executed_digest(master)
  parts <- lapply(draws, `[[`, "frame_digest")
  if (!is_null(prior) && !any(vapply(parts, is_null, logical(1)))) {
    digest <- tryCatch(
      merge_wave_draw_digests(prior, parts),
      error = function(e) {
        cli_warn(c(
          "The frame digest could not be extended for this continuation.",
          "i" = conditionMessage(e)
        ), class = "samplyr_warning_digest_unavailable")
        NULL
      }
    )
  }

  new_tbl_sample(
    data = rows,
    design = design,
    stages_executed = stages,
    seed = last$seed,
    metadata = list(
      n_selected = nrow(rows),
      executed_at = last$executed_at,
      panels = meta$panels,
      panel_assignment = meta$panel_assignment,
      frame_digest = digest,
      continued_from = continuation_parent_record(base),
      prev_phase = meta$prev_phase,
      execution_environment = last$execution_environment,
      integrity = sample_integrity_record(rows, design, stages),
      frame_schedule = last$frame_schedule,
      empty_parents = c(
        meta$empty_parents,
        unlist(lapply(draws, `[[`, "empty_parents"), recursive = FALSE)
      ),
      per_wave_draws = lapply(draws, function(d) {
        d$rows <- NULL
        d$design <- NULL
        d$frame_digest <- NULL
        d
      })
    )
  )
}

#' The master's frame digest, if it still describes the master
#' @noRd
wave_master_digest <- function(master) {
  digest <- get_frame_digest(master)
  if (is_null(digest) || identical(digest$status, "invalidated")) {
    return(NULL)
  }
  digest
}

#' What a wave's continuation added to the master's digest
#'
#' `NULL` when either digest is missing, so the completed sample has none.
#' @noRd
wave_draw_digest_part <- function(prior, digest) {
  if (is_null(prior) || is_null(digest) ||
        length(digest$stages) <= length(prior$stages)) {
    return(NULL)
  }
  prior_ids <- vapply(prior$frames, function(f) f$frame_id, integer(1))
  list(
    frames = Filter(function(f) !f$frame_id %in% prior_ids, digest$frames),
    stages = digest$stages[-seq_along(prior$stages)]
  )
}

#' @noRd
wave_draws <- function(x) {
  attr(x, "metadata")$per_wave_draws %||% list()
}

#' @noRd
wave_draws_drawn <- function(draws) {
  vapply(draws, function(d) as.integer(d$wave), integer(1))
}

#' @noRd
wave_active_panels <- function(record, wave) {
  schedule <- record$schedule
  schedule$panel[schedule$wave == wave & schedule$active]
}

#' The waves whose later stages must be drawn before the sample is complete
#'
#' A wave with no unit in the master has nothing to draw.
#' @noRd
wave_draws_required <- function(master, record) {
  waves <- sort(unique(record$schedule$wave))
  rotating <- master$.panel[!panel_permanent_rows(master, record)]
  waves[vapply(waves, function(w) {
    any(rotating %in% wave_opening_panels(record, w))
  }, logical(1))]
}

#' The panels whose tenure opens at a wave
#' @noRd
wave_opening_panels <- function(record, wave) {
  schedule <- record$schedule
  schedule$panel[
    schedule$wave == wave & schedule$active & schedule$visit %in% 1L
  ]
}

#' The wave each panel's current tenure opened at, as of `wave`
#' @noRd
tenure_opening <- function(record, panels, wave) {
  schedule <- record$schedule
  opens <- schedule[
    schedule$active & schedule$visit %in% 1L & schedule$wave <= wave,
    c("panel", "wave")
  ]
  last <- tapply(opens$wave, opens$panel, max)
  as.integer(last[as.character(panels)])
}

#' Rows of the units in a permanent pool, active at every wave
#' @noRd
panel_permanent_rows <- function(data, record) {
  permanent <- rep(FALSE, nrow(data))
  pools <- Filter(function(pool) identical(pool$activation, "permanent"),
                  record$pools)
  if (length(pools) == 0L) {
    return(permanent)
  }
  keys <- make_group_key(as.data.frame(data), record$key_vars)
  for (pool in record_pool_rows(keys, pools)) {
    permanent[pool$rows] <- TRUE
  }
  permanent
}

#' Refuse an ordinary continuation of a master whose waves are being drawn
#' @noRd
check_no_wave_draws <- function(x, call = caller_env()) {
  drawn <- wave_draws_drawn(wave_draws(x))
  record <- attr(x, "metadata")$panel_assignment
  if (length(remaining_stages(x)) > 0L && length(drawn) == 0L &&
        !isTRUE(attr(x, "metadata")$tenure_view) &&
        schedule_has_tenures(record$schedule)) {
    abort_samplyr(
      c(
        "This master's schedule opens several tenures per panel, so its later
         stages are drawn one tenure at a time.",
        "i" = "Draw each tenure where it opens:
               {.code execute(master, listing, wave = t)}."
      ),
      class = "samplyr_error_tenure_stages",
      call = call
    )
  }
  if (length(drawn) == 0L || length(remaining_stages(x)) == 0L) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "The later stages of this master are being drawn one wave at a time.",
      "x" = "Drawn so far: {cli::qty(length(drawn))}wave{?s} {sort(drawn)}.",
      "i" = "Draw each remaining wave with its listing:
             {.code execute(master, listing, wave = t)}."
    ),
    class = "samplyr_error_wave_draws_in_progress",
    call = call
  )
}

#' The sample of a wave whose later stages are drawn, before the master is
#' complete
#'
#' Its weights are exact. It is marked provisional, because the variance of a
#' wave reads the whole master as its first phase, and the analysis verbs
#' refuse it until the last wave is drawn.
#' @noRd
materialize_drawn_wave <- function(master, record, wave, active, remaining,
                                   execution_environment,
                                   call = caller_env()) {
  source <- wave_draw_source(master, record, wave, remaining, call = call)
  build_wave_sample(
    source = source,
    record = record,
    wave = wave,
    activation = activate_cohort(source, record, active, call = call),
    schedule_digest = rlang::hash(record$schedule),
    execution_environment = execution_environment,
    extra = list(provisional = TRUE),
    digest = readable_digest(wave_rows_digest(master, source))
  )
}

#' The rows a wave reads, from the draws of its units' current tenures
#'
#' Each active panel's units come from the draw where its current tenure
#' opened, and permanent units from the draw that took them. A wave whose
#' tenures are not all drawn is refused, naming the waves to draw.
#' @noRd
wave_draw_source <- function(master, record, wave, remaining,
                             call = caller_env()) {
  draws <- wave_draws(master)
  drawn <- wave_draws_drawn(draws)
  active <- wave_active_panels(record, wave)
  permanent <- panel_permanent_rows(master, record)
  present <- intersect(active, unique(master$.panel[!permanent]))
  needed <- sort(unique(tenure_opening(record, present, wave)))
  missing <- setdiff(needed, drawn)
  holds <- vapply(draws, function(d) isTRUE(d$permanent), logical(1))
  if (any(permanent) && !any(holds) && length(missing) == 0L) {
    missing <- wave
  }
  if (length(missing) > 0L) {
    abort_samplyr(
      c(
        "A wave can only be materialized once its later stages are drawn.",
        "x" = "The design has unexecuted stages: {remaining}.",
        "i" = if (length(drawn) > 0L) {
          "Drawn so far: {wave_phrase(sort(drawn))}."
        },
        "i" = "Wave {wave} reads the units drawn at {wave_phrase(needed)}.
               Draw {wave_phrase(missing)} with its listing:
               {.code execute(master, listing, wave = t)}."
      ),
      class = "samplyr_error_wave_incomplete_master",
      call = call
    )
  }

  parts <- lapply(draws, function(d) {
    rows <- d$rows
    stay <- panel_permanent_rows(rows, record)
    opened <- tenure_opening(record, rows$.panel, wave)
    take <- (!stay & rows$.panel %in% active & opened %in% d$wave) |
      (stay & isTRUE(d$permanent))
    rows[take, , drop = FALSE]
  })
  data <- dplyr::bind_rows(parts)
  rownames(data) <- NULL
  first <- draws[[1L]]
  stages <- sort(unique(c(get_stages_executed(master), first$stages)))
  meta <- attr(master, "metadata")
  new_tbl_sample(
    data = data,
    design = first$design,
    stages_executed = stages,
    seed = first$seed,
    metadata = list(
      n_selected = nrow(data),
      executed_at = first$executed_at,
      panels = meta$panels,
      panel_assignment = meta$panel_assignment,
      execution_environment = first$execution_environment,
      integrity = sample_integrity_record(data, first$design, stages)
    )
  )
}

#' Refuse a provisional wave where its first phase would be read
#' @noRd
check_wave_not_provisional <- function(x, fn_name, call = caller_env()) {
  wave <- attr(x, "metadata")[["wave"]]
  if (!isTRUE(wave$provisional)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn {fn_name}} does not take {wave_phrase(wave$wave)} of a master
       whose later stages are still being drawn.",
      "x" = "The master's later stages are drawn one wave at a time, and some
             are not drawn yet.",
      "i" = "The weights of this wave are exact, and {.fn as_svydesign} and
             {.fn as_svrepdesign} export it by ultimate clusters from its own
             rows."
    ),
    class = "samplyr_error_wave_provisional",
    call = call
  )
}

#' How far a master's later stages are drawn, for print() and summary()
#'
#' `NULL` unless some waves are drawn and the master is not complete.
#' @noRd
wave_draws_progress <- function(x) {
  drawn <- wave_draws_drawn(wave_draws(x))
  later <- remaining_stages(x)
  if (length(drawn) == 0L || length(later) == 0L) {
    return(NULL)
  }
  record <- attr(x, "metadata")$panel_assignment
  stages <- paste0(
    "stage", if (length(later) > 1L) "s", " ", paste(later, collapse = ", ")
  )
  if (!identical(record$certainty, "assigned")) {
    return(paste0(stages, " at ", wave_phrase(drawn)))
  }
  k <- as.integer(record$panels)
  paste0(
    stages, " in ", length(drawn), " of ", k, " periods (",
    paste(sort(drawn), collapse = ", "), ")"
  )
}
