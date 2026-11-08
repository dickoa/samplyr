## Drawing the later stages one occasion at a time

#' Draw the remaining stages under the units of one occasion
#'
#' `execute(master, listing, period = p, subround = s)` runs an ordinary continuation on a
#' view of the master that holds only the units active at occasion `t`, so every
#' check and message of a continuation applies. The rows it draws are kept
#' with the master, whose own rows do not change. The call that draws the
#' last occasion returns the completed sample.
#' @noRd
draw_occasion <- function(master, occasion, frames, stages, seed, panels,
                      panel_stage, small_pool, reps,
                      frame_digest_given = FALSE, call = caller_env()) {
  record <- check_occasion_draw_call(
    master, stages = stages, panels = panels, panel_stage = panel_stage,
    small_pool = small_pool, reps = reps,
    frame_digest_given = frame_digest_given, call = call
  )
  occasion <- check_occasion_declared(occasion, record$schedule, call = call)
  draws <- occasion_draws(master)
  drawn <- occasion_draws_drawn(draws)
  if (occasion %in% drawn) {
    abort_samplyr(
      c(
        "The later stages of {occasion_phrase(occasion, record$schedule)} are
         already drawn.",
        "i" = "Each occasion is drawn once. Drawn so far:
               {occasion_phrase(drawn, record$schedule)}."
      ),
      class = "samplyr_error_occasion_already_drawn",
      call = call
    )
  }

  opening <- occasion_opening_panels(record, occasion)
  permanent <- panel_permanent_rows(master, record)
  # Permanent units hold one cohort, from the first occasion drawn.
  with_permanent <- any(permanent) &&
    !any(vapply(draws, function(d) isTRUE(d$permanent), logical(1)))
  keep <- (master$.panel %in% opening & !permanent) |
    (with_permanent & permanent)
  if (!any(keep)) {
    abort_occasion_opens_nothing(master, record, occasion, opening, drawn, call = call)
  }
  view <- occasion_draw_view(master, keep)
  prior <- master_executed_digest(master)
  exclusion <- cohort_exclusion(master, record, draws, frames, view,
                                call = call)
  frames <- exclusion$frames
  # The draw sizes its takes on the whole listing.
  takes <- exclusion$takes
  if (!is_null(takes)) {
    sized <- get_design(view)
    sized$stages[[takes$stage]]$draw_spec$cohort_takes <- takes
    attr(view, "design") <- sized
  }
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
    },
    # Units of earlier cohorts were left out, so taking the rest is no census.
    samplyr_warning_census = function(cnd) {
      if (!is_null(exclusion$pools)) {
        warn_cohort_census(cnd$stage, cnd$payload, call = call)
        invokeRestart("muffleWarning")
      }
    }
  )

  # The design the draw keeps does not carry them.
  if (!is_null(takes)) {
    kept <- get_design(piece)
    kept$stages[[takes$stage]]$draw_spec$cohort_takes <- NULL
    attr(piece, "design") <- kept
  }
  meta <- attr(piece, "metadata")
  master_parents <- attr(master, "metadata")$empty_parents
  # Numbered after the earlier draws, so an occasion or window that reads
  # several cohorts names each unit once.
  rows <- cohort_reweight(as.data.frame(piece), exclusion$pools)
  used <- unlist(lapply(draws, function(d) d$rows$.sample_id))
  rows$.sample_id <- rows$.sample_id + max(c(0L, used))
  draw <- list(
    occasion = occasion,
    active_panels = opening,
    permanent = with_permanent,
    seed = attr(piece, "seed"),
    stages = setdiff(get_stages_executed(piece), get_stages_executed(master)),
    frame_schedule = meta$frame_schedule,
    execution_environment = meta$execution_environment,
    executed_at = meta$executed_at,
    rows = rows,
    empty_parents = utils::tail(
      meta$empty_parents,
      length(meta$empty_parents) - length(master_parents)
    ),
    design = get_design(piece),
    frame_digest = occasion_draw_digest_part(prior, get_frame_digest(piece))
  )
  if (schedule_has_cohorts(record$schedule)) {
    draw$frame_digest <- record_cohort_exclusion(
      draw$frame_digest, prior, view, exclusion$pools, get_design(piece)
    )
  }
  draws <- c(draws, list(draw))

  # A panel with several cohorts keeps drawing, so its master never closes.
  pending <- setdiff(occasion_draws_required(master, record), c(drawn, occasion))
  if (length(pending) == 0L && !schedule_has_cohorts(record$schedule)) {
    return(complete_occasion_draws(master, draws))
  }
  meta <- attr(master, "metadata")
  meta$per_occasion_draws <- draws
  # The master's digest then records every cohort drawn so far.
  if (schedule_has_cohorts(record$schedule) && !is_null(prior)) {
    meta$frame_digest <- cohort_master_digest(master, draws) %||% prior
  }
  attr(master, "metadata") <- meta
  master
}

#' Guards for drawing the later stages of one occasion
#'
#' The master must be a single unmodified sample with a periods assignment.
#' `execute()` routes here only with stages left to draw. Inputs fixed at the
#' master draw are refused by name. `stages` is accepted when it names every remaining stage, which a
#' single listing for several stages needs.
#' @noRd
check_occasion_draw_call <- function(master, stages, panels, panel_stage,
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
        "Drawing an occasion's later stages takes a listing, {.arg seed} and
         {.arg stages} only.",
        "x" = "Also given: {supplied}.",
        "i" = "The panel assignment was fixed when the master was drawn.",
        if (isTRUE(frame_digest_given)) {
          c("i" = "The frame digest extends the master's.")
        }
      ),
      class = "samplyr_error_occasion_extra_arguments",
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
          "An occasion's draw covers every remaining stage.",
          "x" = "{.arg stages} names {stages}, and the remaining
                 {cli::qty(length(remaining))}stage{?s} {?is/are}
                 {remaining}.",
          "i" = "Omit {.arg stages}, or give {.code stages = {remaining}}."
        ),
        class = "samplyr_error_occasion_draw_stages",
        call = call
      )
    }
  }

  record <- metadata$panel_assignment
  if (!is_null(record)) {
    record <- prepare_panel_record(record, "An occasion draw", call = call)
  }
  if (is_null(record) || is_null(record$schedule)) {
    abort_samplyr(
      c(
        "Later stages are drawn one occasion at a time only for a master with a
         rotation schedule or {.code panels = subrounds(k)}.",
        "x" = if (is_null(record)) {
          "This sample carries no panel assignment."
        } else {
          "This sample's panels declare no occasions."
        },
        "i" = "Draw the later stages in one call:
               {.code execute(master, listing)}."
      ),
      class = "samplyr_error_occasion_draw_unsupported",
      call = call
    )
  }
  record
}

#' Refuse an occasion that opens no cohort, naming where its units come from
#' @noRd
abort_occasion_opens_nothing <- function(master, record, occasion, opening, drawn,
                                     call = caller_env()) {
  active <- occasion_active_panels(record, occasion)
  present <- intersect(active, unique(master$.panel))
  sources <- sort(unique(cohort_opening(record, present, occasion)))
  done <- intersect(sources, drawn)
  where <- occasion_phrase(occasion, record$schedule)
  where_upper <- occasion_phrase(occasion, record$schedule, upper = TRUE)
  abort_samplyr(
    c(
      if (length(opening) == 0L) {
        "{where_upper} opens no cohort, so it has nothing to draw."
      } else {
        "No unit of the panels opening a cohort at {where} is in the master, so
         it has nothing to draw."
      },
      "i" = if (length(sources) == 0L) {
        "No unit of the master is active at {where}."
      } else if (length(done) == length(sources)) {
        "Its units were drawn at {occasion_phrase(sources, record$schedule)},
         and
         {.code execute(master, {occasion_code(record$schedule, occasion)})}
         gives the occasion."
      } else {
        "Its units are drawn at {occasion_phrase(sources, record$schedule)},
         with
         {.code execute(master, listing, period = p, subround = s)} for each,
         and then {.code execute(master, {occasion_code(record$schedule,
         occasion)})} gives the occasion."
      }
    ),
    class = "samplyr_error_occasion_already_drawn",
    call = call
  )
}

#' Leave out of a cohort's listing the units earlier cohorts drew
#'
#' Only under a schedule with cohorts, and only in a primary unit drawn
#' before. Each pool's count of units left out is kept, so the draw can be
#' weighted to the whole listing. A primary unit drawn before with none of
#' its earlier units in the listing is refused: that listing was cut
#' already, and nothing would count those units back. One missing some of
#' them is drawn, and they are read as gone from the population. Only the
#' units of a primary unit's latest cohort warn, so each is reported once.
#' @return The frames to draw from and the pools' counts, or `NULL` counts
#'   when nothing is left out.
#' @noRd
cohort_exclusion <- function(master, record, draws, frames, view,
                             call = caller_env()) {
  # A replay passes the call's frames as one list.
  if (length(frames) == 1L && is.list(frames[[1L]]) &&
        !is.data.frame(frames[[1L]])) {
    frames <- frames[[1L]]
  }
  none <- list(frames = frames, pools = NULL)
  if (!schedule_has_cohorts(record$schedule) || length(draws) == 0L ||
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
        "The listing for this cohort leaves out every unit earlier cohorts
         drew in {length(cut)} primary unit{?s}.",
        "i" = "Pass the full listing. samplyr leaves out the units drawn
               before and weights the new cohort to the whole listing, so
               that it estimates the whole population."
      ),
      class = "samplyr_error_cohort_listing",
      call = call
    )
  }
  # Units of a returning primary unit's latest cohort missing from the
  # listing may have left the population, or the listing may have been cut
  # of them. Older cohorts are left out, so a departure warns once. The
  # latest cohort is read from the panels each draw opened, since a cohort
  # that drew no unit leaves no row.
  position <- rep(seq_along(draws),
                  vapply(draws, function(d) nrow(d$rows), integer(1)))
  master_rows <- as.data.frame(master)
  at <- match(psu_used, make_group_key(master_rows, psu_vars))
  panel <- master_rows$.panel[at]
  permanent <- panel_permanent_rows(master, record)[at]
  last_draw <- integer(length(at))
  for (d in seq_along(draws)) {
    took <- (panel %in% draws[[d]]$active_panels & !permanent) |
      (isTRUE(draws[[d]]$permanent) & permanent)
    last_draw[took] <- d
  }
  latest <- position == last_draw
  returning <- latest & psu_used %in% drawn_now & psu_used %in% psu_listed
  missing <- unique(used[returning & !used %in% listed])
  if (length(missing) > 0L) {
    n_psu <- length(unique(psu_used[returning & used %in% missing]))
    cli_warn(
      c(
        "The listing for this cohort lacks {length(missing)} unit{?s} that
         earlier cohorts drew, in {n_psu} primary unit{?s}.",
        "i" = "samplyr reads them as gone from the population and weights
               the new cohort to the listing.",
        "i" = "If the listing was cut of them, pass the full listing."
      ),
      class = "samplyr_warning_cohort_listing",
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
  check_cohort_exhausted(
    listing[pool_vars], pool, psu_listed %in% drawn_now,
    remaining, left_out, call = call
  )
  frames[[1L]] <- frames[[1L]][!dropped, , drop = FALSE]
  excluded_key <- cohort_pool_key(psu_listed, listing[strata_vars])
  list(
    frames = frames,
    takes = cohort_listing_takes(
      listing[units, , drop = FALSE], spec, stage, psu_vars, strata_vars
    ),
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

#' Each pool's take, sized on the whole listing
#'
#' A cohort draws from the units earlier cohorts left, but `frac` and an
#' allocation size its take on every unit listed, as `n` is, so each cohort
#' takes the same share of the listing. A take above what is left takes it
#' all, which the draw reports. `NULL` when the take does not depend on the
#' pool's size.
#' @noRd
cohort_listing_takes <- function(units, spec, stage, psu_vars, strata_vars) {
  draw <- spec$draw_spec
  if (is_null(draw$frac) && is_null(spec$strata$alloc)) {
    return(NULL)
  }
  parents <- split_row_indices(units, psu_vars)
  table <- vctrs::vec_rbind(!!!lapply(parents$indices, function(rows) {
    pool <- units[rows, , drop = FALSE]
    if (length(strata_vars) == 0L) {
      out <- pool[1L, psu_vars, drop = FALSE]
      out$n <- draw$n %||% frac_pool_size(nrow(pool), draw$frac, draw)
      return(out)
    }
    groups <- split_row_indices(pool, strata_vars)$indices
    info <- calculate_stratum_sizes(
      stratum_info_from_groups(pool, strata_vars, groups), spec$strata, draw
    )
    out <- pool[vapply(groups, `[[`, integer(1), 1L), c(psu_vars, strata_vars),
                drop = FALSE]
    out$n <- info$.n_h
    out
  }))
  rownames(table) <- NULL
  list(stage = stage, psu_vars = psu_vars, strata_vars = strata_vars,
       table = table)
}

#' Report a cohort stage that took every unit earlier cohorts left
#'
#' The census warning says the stage contributes no variance. A cohort is
#' weighted to the whole listing, so where earlier cohorts drew from a pool
#' its units still vary between samples.
#' @noRd
warn_cohort_census <- function(stage, x, call = caller_env()) {
  cli_warn(
    c(
      "Stage {stage}: selected every unit that earlier cohorts did not draw,
       in the pools it executed.",
      "x" = "Requested {x$n_requested} unit{?s}, selected all
             {x$n_actual} available.",
      pool_lines(x, "Exhausted pools"),
      "i" = "Where earlier cohorts drew from a pool, the new cohort is
             weighted to the whole listing, so the stage still contributes
             sampling variance there."
    ),
    class = "samplyr_warning_census",
    stage = stage,
    operation = "population_cap",
    payload = x,
    call = call
  )
}

#' Refuse a cohort whose pools earlier cohorts drew whole
#'
#' Leaving out the earlier units leaves no row of such a pool, so the draw
#' would never see it. A stratum would drop out of the totals, and a
#' primary unit would read as empty although it is not.
#' @noRd
check_cohort_exhausted <- function(keys, pool, now, remaining, left_out,
                                   call = caller_env()) {
  gone <- names(remaining)[remaining == 0L & left_out > 0L]
  at <- now & pool %in% gone
  if (!any(at)) {
    return(invisible(NULL))
  }
  pools <- vctrs::vec_unique(vctrs::vec_slice(keys, at))
  n <- nrow(pools)
  abort_samplyr(
    c(
      "{n} pool{?s} of this cohort {?has/have} no unit that earlier cohorts
       did not draw.",
      "x" = "{cli::qty(n)}Pool{?s}: {format_pool_sample(format_key_preview(pools))}.",
      "i" = "A cohort draws only units no earlier cohort drew, so the
             listing needs new units in every pool it draws from.",
      "i" = "List the units added since, or take fewer units per cohort so
             the listing lasts the schedule."
    ),
    class = "samplyr_error_cohort_exhausted",
    call = call
  )
}

#' A cohort pool's key from its primary unit's key and its strata
#'
#' Strata are read as text, so a pool of the listing and the same pool of
#' the digest, which stores them as text, get one key.
#' @noRd
cohort_pool_key <- function(psu_key, strata) {
  df <- data.frame(.psu = psu_key, stringsAsFactors = FALSE)
  for (v in names(strata)) {
    df[[v]] <- as.character(strata[[v]])
  }
  make_group_key(df, names(df))
}

#' Record in a cohort's digest the units its exclusion left out
#'
#' Each pool of the cohort stage gets `n_excluded`, the units of its listing
#' that earlier cohorts drew. The recorded chances stay those of the draw,
#' over the `N` units left, and `chance * N / (N + n_excluded)` is the
#' chance the weights use. A first cohort, and every stage below the cohort
#' stage, leave none out.
#' @noRd
record_cohort_exclusion <- function(part, prior, view, pools, design) {
  if (is_null(part)) {
    return(part)
  }
  part$stages <- lapply(part$stages, function(stage) {
    n <- rep(0L, nrow(stage$pools))
    if (!is_null(pools) && identical(stage$stage_id, pools$stage)) {
      n <- cohort_pools_excluded(stage, prior, view, pools, design)
    }
    stage$pools$n_excluded <- n
    stage
  })
  part
}

#' The units left out of each pool of a cohort stage
#'
#' A pool is found by its parent, the master's unit above it, and its
#' strata. `NA` for a pool whose parent the master's digest does not name.
#' @noRd
cohort_pools_excluded <- function(stage, prior, view, pools, design) {
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
  key <- cohort_pool_key(psu_key, stage$pools[pools$strata_vars])
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

#' Weight a cohort's draw to the whole listing
#'
#' An equal-probability draw of `n` from the `R` units left in a pool gives
#' each unit of the listing the chance `n / (R + U)`, with `U` the units
#' left out, so the stage's weight and population count are scaled by
#' `(R + U) / R`.
#' @noRd
cohort_reweight <- function(rows, pools) {
  if (is_null(pools)) {
    return(rows)
  }
  key <- make_group_key(rows, pools$vars)
  factor <- unname(pools$factor[key])
  if (anyNA(factor)) {
    cli_abort(
      c(
        "Internal error: a cohort drew rows in a pool without a unit count.",
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

#' The master restricted to one occasion's units, as a sample in its own right
#'
#' Its integrity record is rebuilt for the rows it keeps, so the continuation
#' reads it as unmodified. It keeps the master's frame digest, which the
#' continuation extends with the occasion's later stages. The draws stay with the
#' master.
#' @noRd
occasion_draw_view <- function(master, keep) {
  data <- as.data.frame(master)[keep, , drop = FALSE]
  rownames(data) <- NULL
  design <- get_design(master)
  stages <- get_stages_executed(master)
  meta <- attr(master, "metadata")
  meta$per_occasion_draws <- NULL
  meta$frame_digest <- master_executed_digest(master)
  # The view is how one cohort is drawn, so the cohort guard lets it through.
  meta$cohort_view <- TRUE
  meta$integrity <- sample_integrity_record(data, design, stages)
  new_tbl_sample(
    data = data,
    design = design,
    stages_executed = stages,
    seed = attr(master, "seed"),
    metadata = meta
  )
}

#' Bind every occasion's rows into the completed sample
#'
#' Rows follow the master's order of their parent units. The sample is a
#' continuation of the master, and keeps each occasion's call, without its rows,
#' for the receipt. Its frame digest merges the occasions' digests.
#' @noRd
complete_occasion_draws <- function(master, draws) {
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
  base_meta$per_occasion_draws <- NULL
  attr(base, "metadata") <- base_meta
  last <- draws[[length(draws)]]

  digest <- NULL
  prior <- master_executed_digest(master)
  parts <- lapply(draws, `[[`, "frame_digest")
  if (!is_null(prior) && !any(vapply(parts, is_null, logical(1)))) {
    digest <- tryCatch(
      merge_occasion_draw_digests(prior, parts),
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
      draw_seeds = draw_seeds(draws),
      per_occasion_draws = lapply(draws, function(d) {
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
occasion_master_digest <- function(master) {
  digest <- get_frame_digest(master)
  if (is_null(digest) || identical(digest$status, "invalidated")) {
    return(NULL)
  }
  digest
}

#' What an occasion's continuation added to the master's digest
#'
#' `NULL` when either digest is missing, so the completed sample has none.
#' @noRd
occasion_draw_digest_part <- function(prior, digest) {
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
occasion_draws <- function(x) {
  attr(x, "metadata")$per_occasion_draws %||% list()
}

#' @noRd
occasion_draws_drawn <- function(draws) {
  vapply(draws, function(d) as.integer(d$occasion), integer(1))
}

#' The seeds of the occasion draws a sample's rows come from
#'
#' A unit's rows come from the draw where its cohort opened, as of the occasion
#' the row belongs to, and permanent units from the draw that took them.
#' @noRd
occasion_draw_seeds <- function(draws, record, data, occasions) {
  if (length(draws) == 0L || is_null(record) || nrow(data) == 0L) {
    return(NULL)
  }
  drawn <- occasion_draws_drawn(draws)
  at <- row_occasions(data, record$schedule) %||% rep(occasions[[1L]], nrow(data))
  permanent <- panel_permanent_rows(data, record)
  used <- unlist(lapply(unique(at), function(t) {
    rows <- at == t & !permanent
    cohort_opening(record, data$.panel[rows], t)
  }))
  if (any(permanent)) {
    holds <- vapply(draws, function(d) isTRUE(d$permanent), logical(1))
    used <- c(used, drawn[holds])
  }
  draw_seeds(draws[drawn %in% used])
}

#' The seeds of some occasion draws, in the order of their occasions
#'
#' `integer(0)` when one of the draws ran without a seed, so no seed
#' reproduces the rows, and `NULL` for no draw.
#' @noRd
draw_seeds <- function(draws) {
  if (length(draws) == 0L) {
    return(NULL)
  }
  seeds <- lapply(draws[order(occasion_draws_drawn(draws))], `[[`, "seed")
  if (any(vapply(seeds, is_null, logical(1)))) {
    return(integer(0))
  }
  unique(as.integer(unlist(seeds)))
}

#' @noRd
occasion_active_panels <- function(record, occasion) {
  schedule <- record$schedule
  schedule$panel[schedule$occasion == occasion & schedule$active]
}

#' The occasions whose later stages must be drawn before the sample is complete
#'
#' An occasion with no unit in the master has nothing to draw.
#' @noRd
occasion_draws_required <- function(master, record) {
  occasions <- sort(unique(record$schedule$occasion))
  rotating <- master$.panel[!panel_permanent_rows(master, record)]
  occasions[vapply(occasions, function(w) {
    any(rotating %in% occasion_opening_panels(record, w))
  }, logical(1))]
}

#' The panels whose cohort opens at an occasion
#' @noRd
occasion_opening_panels <- function(record, occasion) {
  schedule <- record$schedule
  schedule$panel[
    schedule$occasion == occasion & schedule$active & schedule$visit %in% 1L
  ]
}

#' The occasion each panel's current cohort opened at, as of `occasion`
#' @noRd
cohort_opening <- function(record, panels, occasion) {
  schedule <- record$schedule
  opens <- schedule[
    schedule$active & schedule$visit %in% 1L & schedule$occasion <= occasion,
    c("panel", "occasion")
  ]
  last <- tapply(opens$occasion, opens$panel, max)
  as.integer(last[as.character(panels)])
}

#' Rows of the units in a permanent pool, active at every occasion
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

#' Refuse an ordinary continuation of a master whose occasions are being drawn
#' @noRd
check_no_occasion_draws <- function(x, call = caller_env()) {
  drawn <- occasion_draws_drawn(occasion_draws(x))
  record <- attr(x, "metadata")$panel_assignment
  if (length(remaining_stages(x)) > 0L && length(drawn) == 0L &&
        !isTRUE(attr(x, "metadata")$cohort_view) &&
        schedule_has_cohorts(record$schedule)) {
    abort_samplyr(
      c(
        "This master's schedule opens several cohorts per panel, so its later
         stages are drawn one cohort at a time.",
        "i" = "Draw each cohort where it opens:
               {.code execute(master, listing, period = p, subround = s)}."
      ),
      class = "samplyr_error_cohort_stages",
      call = call
    )
  }
  if (length(drawn) == 0L || length(remaining_stages(x)) == 0L) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "The later stages of this master are being drawn one occasion at a time.",
      "x" = "Drawn so far: {occasion_phrase(drawn, record$schedule)}.",
      "i" = "Draw each remaining occasion with its listing:
             {.code execute(master, listing, period = p, subround = s)}."
    ),
    class = "samplyr_error_occasion_draws_in_progress",
    call = call
  )
}

#' The sample of an occasion whose later stages are drawn, before the master is
#' complete
#'
#' Its weights are exact. It is marked provisional, because the two-phase
#' variance of an occasion reads the whole master as its first phase, and
#' the verbs that need that phase refuse it until the last occasion is
#' drawn.
#' @noRd
materialize_drawn_occasion <- function(master, record, occasion, active, remaining,
                                   execution_environment,
                                   call = caller_env()) {
  source <- occasion_draw_source(master, record, occasion, remaining, call = call)
  build_occasion_sample(
    source = source,
    draws = occasion_draws(master),
    record = record,
    occasion = occasion,
    activation = activate_panels(source, record, active, call = call),
    schedule_digest = rlang::hash(record$schedule),
    execution_environment = execution_environment,
    # The digest describes the master at these stages, the occasion below.
    extra = list(
      provisional = TRUE,
      master_stages = get_stages_executed(master),
      realization = provisional_realization(master, source)
    ),
    digest = readable_digest(occasion_rows_digest(master, source))
  )
}

#' The realization a provisional occasion reads
#'
#' Its fingerprint describes its own rows, so two occasions of one master
#' cannot be matched by it. `master` identifies the master's first stages,
#' which later draws leave as they are, and `draws` each draw the rows come
#' from, by the occasion it opened at. `history` holds every draw of the
#' master the occasion was read from. A draw numbers its units after those of
#' the draws before it, so draws held by one master never share a number,
#' while two draws made into separate copies of a master can.
#' @param rows The occasion's rows, or its source's.
#' @noRd
provisional_realization <- function(master, rows) {
  meta <- attr(master, "metadata")
  draws <- occasion_draws(master)
  read <- vapply(draws, function(d) {
    any(d$rows$.sample_id %in% rows$.sample_id)
  }, logical(1))
  history <- vapply(draws, function(d) {
    rlang::hash(list(seed = d$seed, executed_at = d$executed_at,
                     rows = d$rows))
  }, character(1))
  list(
    master = rlang::hash(list(
      integrity = meta$integrity,
      executed_at = meta$executed_at,
      seed = attr(master, "seed")
    )),
    draws = stats::setNames(
      history[read],
      vapply(draws[read], function(d) as.character(d$occasion), character(1))
    ),
    history = history
  )
}

#' The rows an occasion reads, from the draws of its units' current cohorts
#'
#' Each active panel's units come from the draw where its current cohort
#' opened, and permanent units from the draw that took them. An occasion whose
#' cohorts are not all drawn is refused, naming the occasions to draw.
#' @noRd
occasion_draw_source <- function(master, record, occasion, remaining,
                             call = caller_env()) {
  draws <- occasion_draws(master)
  drawn <- occasion_draws_drawn(draws)
  active <- occasion_active_panels(record, occasion)
  permanent <- panel_permanent_rows(master, record)
  present <- intersect(active, unique(master$.panel[!permanent]))
  needed <- sort(unique(cohort_opening(record, present, occasion)))
  missing <- setdiff(needed, drawn)
  holds <- vapply(draws, function(d) isTRUE(d$permanent), logical(1))
  if (any(permanent) && !any(holds) && length(missing) == 0L) {
    missing <- occasion
  }
  if (length(missing) > 0L) {
    abort_samplyr(
      c(
        "An occasion can only be materialized once its later stages are drawn.",
        "x" = "The design has unexecuted stages: {remaining}.",
        "i" = if (length(drawn) > 0L) {
          "Drawn so far: {occasion_phrase(drawn, record$schedule)}."
        },
        "i" = "{occasion_phrase(occasion, record$schedule, upper = TRUE)} reads
               the units drawn at {occasion_phrase(needed, record$schedule)}.
               Draw {occasion_phrase(missing, record$schedule)} with its
               listing:
               {.code execute(master, listing, period = p, subround = s)}."
      ),
      class = "samplyr_error_occasion_incomplete_master",
      call = call
    )
  }

  parts <- lapply(draws, function(d) {
    rows <- d$rows
    stay <- panel_permanent_rows(rows, record)
    opened <- cohort_opening(record, rows$.panel, occasion)
    take <- (!stay & rows$.panel %in% active & opened %in% d$occasion) |
      (stay & isTRUE(d$permanent))
    rows[take, , drop = FALSE]
  })
  data <- dplyr::bind_rows(parts)
  rownames(data) <- NULL
  first <- draws[[1L]]
  stages <- sort(unique(c(get_stages_executed(master), first$stages)))
  meta <- attr(master, "metadata")

  # A primary unit its cohort's draw found empty has no row here, and the
  # export refuses it only if the occasion keeps the draw's record of it.
  vars <- first$design$stages[[1L]]$clusters$vars
  units <- as.data.frame(master)
  opened <- cohort_opening(record, units$.panel, occasion)
  empty_parents <- unlist(lapply(draws, function(d) {
    read <- (!permanent & units$.panel %in% active & opened %in% d$occasion) |
      (permanent & isTRUE(d$permanent))
    keep_empty_parents(d$empty_parents, vars, units[read, vars, drop = FALSE])
  }), recursive = FALSE)

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
      integrity = sample_integrity_record(data, first$design, stages),
      empty_parents = empty_parents
    )
  )
}

#' Empty-parent records restricted to some primary units
#'
#' Keeps the parents whose primary unit is one of `units`, keyed on `vars`,
#' or with `inside = FALSE` the parents whose primary unit is not. A record
#' without those keys is kept whole.
#' @noRd
keep_empty_parents <- function(records, vars, units, inside = TRUE) {
  records <- lapply(records, function(r) {
    if (length(vars) == 0L || !all(vars %in% names(r$keys))) {
      return(r)
    }
    at <- vctrs::vec_in(r$keys[vars], units) == inside
    r$keys <- vctrs::vec_slice(r$keys, at)
    r$n <- nrow(r$keys)
    if (!is_null(r$rows)) {
      r$rows <- vctrs::vec_slice(
        r$rows,
        vctrs::vec_in(r$rows[vars], units) == inside
      )
    }
    r
  })
  Filter(function(r) r$n > 0L, records)
}

#' Refuse a provisional occasion where its first phase would be read
#' @noRd
check_occasion_not_provisional <- function(x, fn_name, call = caller_env()) {
  occasion <- attr(x, "metadata")[["occasion"]]
  if (!isTRUE(occasion$provisional)) {
    return(invisible(NULL))
  }
  # A master whose panels open several cohorts keeps drawing, so it never
  # closes, even once its schedule is drawn.
  cohorts <- schedule_has_cohorts(attr(x, "metadata")$panel_assignment$schedule)
  abort_samplyr(
    c(
      "{.fn {fn_name}} does not take {sample_occasion_phrase(attr(x,
       \"metadata\"))} of a master whose later stages are drawn one occasion
       at a time.",
      "x" = if (cohorts) {
        "The master's panels open several cohorts, so its later stages are
         drawn one cohort at a time and the master is never complete."
      } else {
        "Some of the master's later stages are not drawn yet."
      },
      "i" = "The weights of this occasion are exact, and {.fn as_svydesign} and
             {.fn as_svrepdesign} export it by ultimate clusters from its own
             rows."
    ),
    class = "samplyr_error_occasion_provisional",
    call = call
  )
}

#' How far a master's later stages are drawn, for print() and summary()
#'
#' `NULL` unless some occasions are drawn and the master is not complete.
#' @noRd
occasion_draws_progress <- function(x) {
  drawn <- occasion_draws_drawn(occasion_draws(x))
  later <- remaining_stages(x)
  if (length(drawn) == 0L || length(later) == 0L) {
    return(NULL)
  }
  record <- attr(x, "metadata")$panel_assignment
  stages <- paste0(
    "stage", if (length(later) > 1L) "s", " ", paste(later, collapse = ", ")
  )
  if (!identical(record$certainty, "assigned")) {
    return(paste0(stages, " at ", occasion_phrase(drawn, record$schedule)))
  }
  k <- as.integer(record$panels)
  paste0(
    stages, " in ", length(drawn), " of ", k, " subrounds (",
    paste(sort(drawn), collapse = ", "), ")"
  )
}
