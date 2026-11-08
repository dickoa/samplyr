## Occasion materialization

# An occasion uses frozen block quotas with activation chance a_b / m_b.
# Certainty units remain active with probability one.

#' Materialize one scheduled occasion of a rotating master
#' @noRd
materialize_occasion <- function(
  master,
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
  record <- check_occasion_call(
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
  occasion <- check_occasion_declared(
    occasion, record$schedule, window = TRUE, call = call
  )

  schedule <- record$schedule
  remaining <- setdiff(
    seq_along(get_design(master)$stages), get_stages_executed(master)
  )
  if (length(occasion) > 1L) {
    if (length(remaining) > 0 && length(occasion_draws(master)) == 0L) {
      abort_samplyr(
        c(
          "A window of occasions needs a master whose stages are all drawn.",
          "x" = "The design has unexecuted stages: {remaining}.",
          "i" = "Draw them, {.code execute(master, listing)}, or for every
                 occasion, {.code execute(master, listing, period = p,
                 subround = s)}, then ask for
                 {.code execute(master, {occasion_code(schedule, occasion)})}."
        ),
        class = "samplyr_error_occasion_incomplete_master",
        call = call
      )
    }
    return(build_window_sample(
      master, record, occasion, execution_environment,
      remaining = remaining,
      call = call
    ))
  }
  active <- schedule$panel[schedule$occasion == occasion & schedule$active]

  if (length(remaining) > 0) {
    return(materialize_drawn_occasion(
      master, record, occasion, active, remaining, execution_environment,
      call = call
    ))
  }

  build_occasion_sample(
    source = master,
    record = record,
    occasion = occasion,
    activation = activate_panels(master, record, active, call = call),
    schedule_digest = rlang::hash(record$schedule),
    execution_environment = execution_environment
  )
}

#' Build the sample one activation realizes
#'
#' Shared by the single-master route and by each cohort of a rotation
#' program, which differ only in where the active panel set comes from.
#' @noRd
build_occasion_sample <- function(
  source,
  record,
  occasion,
  activation,
  schedule_digest,
  execution_environment,
  extra = list(),
  digest = readable_digest(occasion_master_digest(source)),
  draws = occasion_draws(source)
) {
  data <- as.data.frame(source)[activation$keep, , drop = FALSE]
  data$.weight <- data$.weight * activation$factor[activation$keep]
  if (!is_null(record)) {
    data$.visit <- occasion_row_visits(
      record, data$.panel, occasion, activation$permanent[activation$keep]
    )
  }
  rownames(data) <- NULL

  new_occasion_sample(
    source,
    record,
    data,
    occasion = c(
      list(
        occasion = occasion,
        active_panels = activation$active,
        schedule_digest = schedule_digest,
        pools = activation$pools,
        # Bind activation to the exact master realization.
        master_digest = occasion_source_digest(source)
      ),
      extra
    ),
    execution_environment = execution_environment,
    digest = digest,
    draws = draws
  )
}

#' The sample a set of occasions realizes, over one master
#'
#' The window estimates the average of its occasions' totals, so each occasion
#' enters as its own sample, at its exact activation weight over `M`, the
#' number of the window's occasions. A unit has a row at each occasion it is
#' active. A pool with no unit at some occasion of a period has that period's
#' occasions read together: its units take the chance of the period's panels,
#' times the period's share `m / M` of the window. That needs a period whose
#' occasions hold the same number of panels, none of them twice, so that the
#' occasion the pool misses is a matter of its labels. Otherwise the pool is
#' refused.
#' @noRd
build_window_sample <- function(
  master,
  record,
  occasions,
  execution_environment,
  remaining = integer(0),
  call = caller_env()
) {
  schedule <- record$schedule
  active_at <- lapply(occasions, function(t) {
    schedule$panel[schedule$occasion == t & schedule$active]
  })
  n_occasions <- length(occasions)
  pairs <- grid_pairs(schedule)
  union_panels <- sort(unique(unlist(active_at)))
  blocks <- window_blocks(occasions, schedule)

  # A master drawn one occasion at a time gives each occasion its own rows.
  sources <- lapply(occasions, function(t) {
    if (length(remaining) == 0L) {
      master
    } else {
      occasion_draw_source(master, record, t, remaining, call = call)
    }
  })
  collapsed <- list()
  parts <- list()
  for (block in blocks) {
    activations <- lapply(block, function(j) {
      activate_panels(sources[[j]], record, active_at[[j]],
                      allow_empty = TRUE, window = occasions,
                      at = occasions[j], call = call)
    })
    empty <- window_empty_pools(activations)
    if (any(empty)) {
      check_collapsible_block(record, active_at[block], activations, empty,
                              occasions, block, call = call)
      collapsed[[length(collapsed) + 1L]] <- list(
        occasions = occasions[block],
        pools = which(empty)
      )
    }
    block_panels <- sort(unique(unlist(active_at[block])))
    for (i in seq_along(block)) {
      j <- block[[i]]
      activation <- activations[[i]]
      rows <- which(activation$keep)
      factor <- activation$factor[rows] / n_occasions
      if (any(empty)) {
        together <- activation$pool[rows] %in% which(empty)
        union <- activate_panels(sources[[j]], record, block_panels,
                                 window = occasions, part = occasions[block],
                                 call = call)
        factor[together] <- union$factor[rows][together] *
          length(block) / n_occasions
      }
      part <- as.data.frame(sources[[j]])[rows, , drop = FALSE]
      part$.weight <- part$.weight * factor
      part$.period <- rep(pairs$period[occasions[j]], nrow(part))
      part$.subround <- rep(pairs$subround[occasions[j]], nrow(part))
      part$.visit <- occasion_row_visits(
        record, part$.panel, occasions[j], activation$permanent[rows]
      )
      parts[[length(parts) + 1L]] <- part
    }
  }
  data <- vctrs::vec_rbind(!!!parts)
  rownames(data) <- NULL
  digest <- readable_digest(occasion_rows_digest(
    master,
    vctrs::vec_rbind(!!!lapply(sources, as.data.frame))
  ))

  # The window's first phase is its first occasion. On a master drawn occasion by
  # occasion, it carries every occasion's empty primary units with no row in the
  # window, so the export refuses them.
  source <- sources[[1L]]
  if (length(remaining) > 0L) {
    vars <- get_design(source)$stages[[1L]]$clusters$vars
    attr(source, "metadata")$empty_parents <- keep_empty_parents(
      unlist(lapply(sources, function(s) {
        attr(s, "metadata")$empty_parents
      }), recursive = FALSE),
      vars,
      data[vars],
      inside = FALSE
    )
  }

  new_occasion_sample(
    source,
    record,
    data,
    occasion = list(
      occasion = occasions,
      window = if (length(collapsed) > 0L) "collapsed" else "occasions",
      # The pools each period reads together, which the export keeps whole.
      collapsed = if (length(collapsed) > 0L) collapsed,
      blocks = if (length(blocks) > 1L) {
        lapply(blocks, function(block) occasions[block])
      },
      active_panels = union_panels,
      schedule_digest = rlang::hash(schedule),
      # A window that reaches each unit once and every panel is an identity
      # activation, which keeps the master's empty primary units.
      pools = if (!anyDuplicated(unlist(active_at))) {
        activate_panels(sources[[n_occasions]], record, union_panels,
                        allow_empty = TRUE, call = call)$pools
      },
      master_digest = occasion_source_digest(master),
      provisional = if (length(remaining) > 0L) TRUE,
      master_stages = if (length(remaining) > 0L) {
        get_stages_executed(master)
      },
      realization = if (length(remaining) > 0L) {
        provisional_realization(master, data)
      },
      # Each occasion reads its own source, and the export's ratio
      # auxiliary needs every row's weight before activation.
      source_weights = if (length(remaining) > 0L) {
        source_weights(sources)
      }
    ),
    execution_environment = execution_environment,
    digest = digest,
    draws = occasion_draws(master)
  )
}

#' The weight before activation of every row the occasions' sources hold
#'
#' A unit read at several occasions comes from the same draw each time, so
#' it has one weight.
#' @return A data frame with `.sample_id` and `.weight`, one row per id.
#' @noRd
source_weights <- function(sources) {
  rows <- vctrs::vec_rbind(!!!lapply(sources, function(s) {
    as.data.frame(s)[c(".sample_id", ".weight")]
  }))
  rows <- rows[!duplicated(rows$.sample_id), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

#' The pools an occasion of a window block leaves without a unit
#'
#' @param activations The block's activations, one per occasion.
#' @return A logical vector over the record's pools.
#' @noRd
window_empty_pools <- function(activations) {
  n_pools <- length(activations[[1L]]$pools)
  empty <- rep(FALSE, n_pools)
  for (activation in activations) {
    for (p in seq_len(n_pools)) {
      pool <- activation$pools[[p]]
      if (!identical(pool$activation, "permanent") && any(pool$take == 0L)) {
        empty[[p]] <- TRUE
      }
    }
  }
  empty
}

#' Refuse a block whose occasions cannot be read together for a pool
#'
#' Reading a pool's occasions together estimates their average only when
#' which occasion the pool misses depends on its labels alone. The block has
#' several occasions, each with the same number of panels, none active twice.
#' The labels cover rotation groups first and subrounds second, so the
#' schedule must also treat the block's subrounds alike.
#' @noRd
check_collapsible_block <- function(record, active_at, activations, empty,
                                    occasions, block, call = caller_env()) {
  counts <- lengths(active_at)
  reason <- if (length(block) == 1L) {
    "alone"
  } else if (anyDuplicated(unlist(active_at)) > 0L) {
    "revisit"
  } else if (length(unique(counts)) > 1L) {
    "uneven"
  } else {
    unspread_block(record$schedule, active_at, occasions[block])
  }
  if (is_null(reason)) {
    return(invisible(NULL))
  }
  p <- which(empty)[[1L]]
  for (i in seq_along(activations)) {
    take <- activations[[i]]$pools[[p]]$take
    if (any(take == 0L)) {
      abort_inactive_pool(record$pools[[p]], take, record, occasions,
                          occasions[block], at = occasions[block[[i]]],
                          reason = reason, call = call)
    }
  }
}

#' Does the labels' spread treat the subrounds of a block alike?
#'
#' The spread counts the panels of each rotation group at their first
#' subround. Swapping two subrounds of the block, in those first subrounds
#' and in where the panels sit in the block, maps the law of the labels onto
#' itself when every group keeps the same panels. Pools then miss each
#' subround with the same chance.
#' @param active_at The panels active at each occasion of the block.
#' @param at The block's occasions.
#' @return `"unbalanced"`, or `NULL` when the subrounds are alike.
#' @noRd
unspread_block <- function(grid, active_at, at) {
  reporting <- schedule_period_groups(grid)
  if (is_null(reporting)) {
    return(NULL)
  }
  subrounds <- grid_pairs(grid)$subround[at]
  sits <- rep(NA_integer_, length(reporting$group))
  for (i in seq_along(at)) {
    sits[active_at[[i]]] <- subrounds[[i]]
  }
  pair <- function(swap) {
    sort(paste(reporting$group, swap(reporting$subround), swap(sits)))
  }
  kept <- pair(identity)
  ordered <- sort(unique(subrounds))
  for (k in seq_len(length(ordered) - 1L)) {
    a <- ordered[[k]]
    b <- ordered[[k + 1L]]
    swap <- function(s) ifelse(s %in% a, b, ifelse(s %in% b, a, s))
    if (!identical(pair(swap), kept)) {
      return("unbalanced")
    }
  }
  NULL
}

#' The parts of a window inside each reporting period
#'
#' A record without periods reads each occasion as its own period.
#' @return A list of positions in `occasions`, one element per period, in the
#'   order the window first reaches each period.
#' @noRd
window_blocks <- function(occasions, schedule) {
  period <- if ("period" %in% names(schedule)) {
    schedule$period[match(occasions, schedule$occasion)]
  } else {
    occasions
  }
  unname(split(seq_along(occasions), factor(period, levels = unique(period))))
}

#' An occasion's digest, or none when its master's cannot be read
#'
#' An occasion is materialized from the master's assignment record, and an
#' unreadable digest only leaves the occasion without one.
#' @noRd
readable_digest <- function(digest) {
  tryCatch(digest, error = function(e) NULL)
}

#' Wrap an occasion's rows as a sample that keeps its master as phase 1
#' @noRd
new_occasion_sample <- function(
  source,
  record,
  data,
  occasion,
  execution_environment,
  digest = NULL,
  draws = occasion_draws(source)
) {
  # A program occasion states its own occasion, which a selection's record does
  # not number.
  if (is_null(occasion$period)) {
    pairs <- grid_pairs(record$schedule)
    occasion$period <- pairs$period[occasion$occasion]
    occasion$subround <- pairs$subround[occasion$occasion]
  }
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
        occasion = occasion,
        frame_digest = digest,
        # Retain the master as phase 1.
        prev_phase = list(
          transition = "panel_activation",
          sample = source,
          design = design,
          stages = stages_executed
        ),
        execution_environment = execution_environment,
        integrity = sample_integrity_record(data, design, stages_executed),
        # An activation that keeps every unit exports as its first phase's
        # own design, which refuses the primary units that phase lost.
        empty_parents = if (activation_is_identity(list(occasion = occasion))) {
          attr(source, "metadata")$empty_parents
        },
        # The summary shows them, since the seed is the last call's.
        draw_seeds = occasion_draw_seeds(draws, record, data, occasion$occasion)
      )
    )
  )
}

#' Fingerprint of the realization an occasion was activated from
#'
#' Uses metadata frozen at execution so later analysis columns or row order do
#' not change identity. Frame, execution, seed, and integrity metadata jointly
#' distinguish realizations.
#' @noRd
occasion_source_digest <- function(source) {
  metadata <- attr(source, "metadata")
  rlang::hash(list(
    integrity = metadata$integrity,
    frame_digest = metadata$frame_digest,
    executed_at = metadata$executed_at,
    seed = attr(source, "seed")
  ))
}

#' The execution inputs an occasion refuses, and the message both routes share
#'
#' An occasion materializes a stored schedule. Every input `execute()` otherwise
#' takes was resolved when the master was drawn, so each is refused by name
#' rather than accepted and dropped.
#'
#' `frame_digest` is in the list because an occasion touches no frame, so there is
#' nothing to digest. It reaches this by way of `frame_digest_given`, not by
#' being `NULL`: it has a real default, so absence cannot be read off the
#' value the way it can for the other six.
#'
#' @param selects,stored_with The two phrases the master route and the program
#'   route differ on. Everything else about the refusal is the same.
#' @noRd
check_occasion_extra_arguments <- function(
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
    # An occasion cannot revise the frozen assignment policy.
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
      "{.arg period} selects {selects}, so it takes no further execution
       input.",
      "x" = "Also given: {supplied}.",
      if (isTRUE(frame_digest_given)) {
        c("i" = "An occasion is materialized from a stored schedule and reads no
                 frame, so there is nothing for {.arg frame_digest} to
                 describe.")
      },
      "i" = "Every input an occasion needs is stored with {stored_with}."
    ),
    class = "samplyr_error_occasion_extra_arguments",
    call = call
  )
}

#' Guards for the occasion route
#'
#' `execute(sample, ...)` already means two-phase or continuation, so the occasion
#' route has to be unambiguous rather than merely distinguishable: it accepts
#' a scheduled master and an occasion number, and nothing else.
#' @noRd
check_occasion_call <- function(
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
        "{.arg period} materializes a stored rotation schedule, so the first
         argument must be an executed sample.",
        "i" = "Draw the master first:
               {.code execute(design, frame, panels = <schedule>)}."
      ),
      class = "samplyr_error_occasion_not_a_sample",
      call = call
    )
  }

  check_occasion_extra_arguments(
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
  if (!is_null(metadata$occasion)) {
    abort_samplyr(
      c(
        "This sample is already the materialization of
         {sample_occasion_phrase(metadata)}.",
        "i" = "Materialize each occasion from the master:
               {.code execute(master, period = p, subround = s)}."
      ),
      class = "samplyr_error_occasion_already_materialized",
      call = call
    )
  }


  # Validate record version before checking whether it declares occasions.
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
               panel, occasion and active>)}.",
        if (!is_null(record)) c(
          "i" = "{.code panels = {record$panels}} partitions the sample but
                 declares no occasions."
        )
      ),
      class = "samplyr_error_occasion_no_schedule",
      call = call
    )
  }

  record
}

#' The occasions a call asks for, before any schedule is read
#'
#' `NULL` when neither argument is given. `subround` without `period` names
#' no occasion and is refused.
#' @noRd
occasion_request <- function(period, subround, call = caller_env()) {
  if (is_null(period) && is_null(subround)) {
    return(NULL)
  }
  if (is_null(period)) {
    abort_samplyr(
      c(
        "{.arg subround} needs a {.arg period}.",
        "i" = "A subround is a split of a period, so
               {.code subround = s} alone names no occasion."
      ),
      class = "samplyr_error_occasion_pairs",
      call = call
    )
  }
  whole <- function(x) {
    is.numeric(x) && length(x) >= 1L && !anyNA(x) &&
      is_integerish_numeric(x) && all(x >= 1)
  }
  if (!whole(period) || (!is_null(subround) && !whole(subround))) {
    abort_samplyr(
      "{.arg period} and {.arg subround} must hold whole numbers of 1 or
       more.",
      class = "samplyr_error_occasion_pairs",
      call = call
    )
  }
  if (!is_null(subround) && length(period) != length(subround) &&
      length(period) != 1L && length(subround) != 1L) {
    abort_samplyr(
      c(
        "{.arg period} and {.arg subround} are read as pairs, so they must
         have the same length, or one of them length 1.",
        "x" = "{.arg period} has length {length(period)} and
               {.arg subround} has length {length(subround)}.",
        "i" = "To cross them, build the pairs with
               {.code expand.grid(subround = s, period = p)}."
      ),
      class = "samplyr_error_occasion_pairs",
      call = call
    )
  }
  structure(
    list(period = as.integer(period),
         subround = if (!is_null(subround)) as.integer(subround)),
    class = "samplyr_occasion_request"
  )
}

#' Resolve a request to the occasion numbers of a schedule
#'
#' A period alone asks for all its subrounds. Pairs are read position by
#' position after recycling a length-1 argument. A schedule without
#' subrounds takes `subround = 1` or none.
#' @param request From `occasion_request()`.
#' @param schedule A normalized grid with `occasion`, `period` and
#'   `subround`.
#' @param window Whether several occasions may be asked for at once.
#' @return The occasion numbers, sorted.
#' @noRd
check_occasion_declared <- function(request, schedule, window = FALSE,
                                call = caller_env()) {
  pairs <- grid_pairs(schedule)
  period <- request$period
  subround <- request$subround
  if (is_null(subround)) {
    if (anyDuplicated(period) > 0L) {
      abort_occasion_duplicates(call)
    }
    unknown <- setdiff(period, pairs$period)
    if (length(unknown) > 0L) {
      abort_occasion_undeclared(
        pairs, paste(if (length(unknown) == 1L) "period" else "periods",
                     occasion_span(unknown)),
        call
      )
    }
    occasion <- which(pairs$period %in% period)
  } else {
    n <- max(length(period), length(subround))
    period <- rep_len(period, n)
    subround <- rep_len(subround, n)
    if (anyDuplicated(paste(period, subround)) > 0L) {
      abort_occasion_duplicates(call)
    }
    occasion <- match(paste(period, subround),
                  paste(pairs$period, pairs$subround))
    if (anyNA(occasion)) {
      missed <- paste0("period ", period[is.na(occasion)], " subround ",
                       subround[is.na(occasion)])
      abort_occasion_undeclared(pairs, paste(missed, collapse = ", "), call)
    }
  }
  if (!window && length(occasion) > 1L) {
    abort_samplyr(
      c(
        "This call takes a single occasion.",
        "x" = "It names {length(occasion)} occasions.",
        "i" = "A window of several occasions is materialized from a master,
               {.code execute(master, period = , subround = )}, without a
               listing."
      ),
      class = "samplyr_error_occasion_window_unsupported",
      call = call
    )
  }
  sort(as.integer(occasion))
}

#' @noRd
abort_occasion_duplicates <- function(call) {
  abort_samplyr(
    "{.arg period} and {.arg subround} must name each occasion once.",
    class = "samplyr_error_occasion_pairs",
    call = call
  )
}

#' @noRd
abort_occasion_undeclared <- function(pairs, missed, call) {
  declared <- if (all(pairs$subround == 1L)) {
    paste("periods", occasion_span(unique(pairs$period)))
  } else {
    sizes <- tapply(pairs$subround, pairs$period, max)
    paste0("periods ", occasion_span(unique(pairs$period)), ", with ",
           paste(unique(sizes), collapse = " or "), " subrounds each")
  }
  abort_samplyr(
    c(
      "The schedule does not declare {missed}.",
      "i" = "It declares {declared}."
    ),
    class = "samplyr_error_occasion_undeclared",
    call = call
  )
}

#' The occasion of each row of a window, from its `.period` and `.subround`
#'
#' `NULL` for a sample of one occasion, which carries neither column.
#' @noRd
row_occasions <- function(data, schedule) {
  if (!".period" %in% names(data)) {
    return(NULL)
  }
  pairs <- grid_pairs(schedule)
  match(paste(data$.period, data$.subround),
        paste(pairs$period, pairs$subround))
}

#' The arguments that ask for these occasions again
#'
#' `period = 2` for whole periods, else the pairs, as in
#' `period = c(1, 1, 2), subround = c(2, 3, 1)`.
#' @param schedule A normalized grid, or `NULL` when none is at hand, which
#'   writes the occasion numbers as periods.
#' @noRd
occasion_code <- function(schedule, occasions) {
  occasions <- sort(as.integer(occasions))
  if (is_null(schedule)) {
    return(paste0("period = ", values_code(occasions)))
  }
  pairs <- grid_pairs(schedule)
  period <- pairs$period[occasions]
  whole <- which(pairs$period %in% period)
  if (identical(whole, occasions)) {
    return(paste0("period = ", values_code(unique(period))))
  }
  paste0("period = ", values_code(period), ", subround = ",
         values_code(pairs$subround[occasions]))
}

#' Integers as R code, in their order: `2`, `1:3` or `c(1, 1, 2)`
#' @noRd
values_code <- function(x) {
  n <- length(x)
  if (n == 1L) {
    return(as.character(x))
  }
  if (all(diff(x) == 1L)) {
    return(paste0(x[1L], ":", x[n]))
  }
  paste0("c(", paste(x, collapse = ", "), ")")
}

#' Occasions as a message reads them: `2`, `1 and 2`, `1 to 3`, or `1, 3 and 5`
#' @noRd
occasion_span <- function(occasions) {
  occasions <- sort(as.integer(occasions))
  n <- length(occasions)
  if (n == 1L) {
    return(as.character(occasions))
  }
  if (n > 2L && all(diff(occasions) == 1L)) {
    return(paste(occasions[1L], "to", occasions[n]))
  }
  paste(paste(occasions[-n], collapse = ", "), "and", occasions[n])
}

#' Name occasions in a message by their period and subround
#'
#' @param occasions Occasion numbers of `schedule`.
#' @param schedule A normalized grid, as a record stores it.
#' @param upper Capitalize the phrase, to open a sentence.
#' @noRd
occasion_phrase <- function(occasions, schedule, upper = FALSE) {
  phrase <- pair_phrase(grid_pairs(schedule), occasions)
  if (upper) {
    substr(phrase, 1L, 1L) <- toupper(substr(phrase, 1L, 1L))
  }
  phrase
}

#' The occasions of a materialized sample, named for a message
#' @noRd
sample_occasion_phrase <- function(metadata) {
  occasion_phrase(
    metadata$occasion$occasion,
    metadata$panel_assignment$schedule
  )
}

#' Does this sample pool several occasions?
#' @noRd
is_occasion_window <- function(x) {
  length(attr(x, "metadata")[["occasion"]]$occasion) > 1L
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

#' Refuse an activation that leaves blocks of a pool without a unit
#'
#' A window is refused in its own terms, by the occasions of its part when they
#' are read together and by the occasion it reads otherwise. `reason` says why
#' that occasion could not be read with the others of its period: `"alone"`,
#' `"revisit"`, `"uneven"` or `"unbalanced"`.
#' @noRd
abort_inactive_pool <- function(pool, take, record, window, part, at,
                                reason = "revisit", call = caller_env()) {
  stratum <- format_pool_stratum(pool)
  empty <- sum(take == 0L)
  if (!is_null(window)) {
    window_phrase <- occasion_phrase(window, record$schedule)
  }
  if (is_null(window)) {
    message <- c(
      "This occasion cannot be materialized: some units have no chance of
       being selected for it.",
      "x" = "Pool {.val {stratum}} has {empty} block{?s} with no active unit
             in this occasion."
    )
    hint <- "The master leaves every pool a unit in each reporting period,
             not at each subround. Materialize the whole period,
             {.code execute(master, period = p)}."
  } else if (is_null(at) && length(part) < length(window)) {
    part_phrase <- occasion_phrase(part, record$schedule)
    message <- c(
      "This window, {window_phrase}, cannot be materialized: its part in
       {part_phrase} holds no unit of pool {.val {stratum}}.",
      "x" = "The pool has {empty} block{?s} with no active unit in
             {part_phrase}."
    )
    hint <- "Each period's part of a window is read on its own, and the
             master leaves every pool a unit in each whole reporting period
             only. Give the stratum more units, or start and end the window on
             whole periods."
  } else if (is_null(at)) {
    message <- c(
      "This window, {window_phrase}, cannot be materialized: it holds no unit
       of pool {.val {stratum}}.",
      "x" = "The pool has {empty} block{?s} with no active unit in
             {window_phrase}."
    )
    hint <- "The master leaves every pool a unit in each reporting period,
             not in every window of occasions. Give the stratum more units, or
             declare reporting periods at the level of the window."
  } else {
    at_phrase <- occasion_phrase(at, record$schedule)
    message <- c(
      "This window, {window_phrase}, cannot be materialized: {at_phrase}
       holds no unit of pool {.val {stratum}}.",
      "x" = "The pool has {empty} block{?s} with no active unit in
             {at_phrase}."
    )
    hint <- switch(reason,
      alone = "The window holds only this occasion of its period, and the
               master leaves every pool a unit in each whole reporting period
               only. Give the stratum more units, or start and end the window
               on whole periods.",
      revisit = "A panel is active at several occasions of one period of this
                 window, so each of them is read on its own, and the master
                 leaves every pool a unit in each reporting period, not at
                 each occasion. Give the stratum more units.",
      uneven = "The occasions of this period hold different numbers of
                panels, so each is read on its own, and the master leaves
                every pool a unit in each reporting period, not at each
                occasion. Give the stratum more units, or give each occasion
                of the period the same number of panels.",
      unbalanced = "The rotation groups do not hold their panels alike at
                    the subrounds of this period, so the subround a pool
                    misses depends on the schedule, not on its labels alone.
                    Each occasion is then read on its own, and the master
                    leaves every pool a unit in each reporting period, not at
                    each occasion. Give the stratum more units, or give each
                    rotation group as many panels at every subround."
    )
  }
  abort_samplyr(
    c(message, "i" = hint),
    class = "samplyr_error_panel_small_pool",
    call = call
  )
}

#' Which rows an activation keeps, and at what conditional probability
#'
#' @param record The cohort's assignment record, or `NULL` for a cohort drawn
#'   whole. A cohort that was never partitioned has one implicit panel
#'   comprising all its rows, and activating it is not a subsample, so its
#'   factor is one.
#' @param allow_empty Whether a block with no active unit is kept out
#'   rather than refused, for an occasion read inside a window.
#' @param window The occasions of the window being read, or `NULL` for one occasion.
#' @param part The occasions of the window's part this union reads, all of
#'   `window` unless the window is read period by period.
#' @param at The occasion of the window this activation reads, or `NULL` for
#'   a union.
#' @return A list with `keep`, the row mask, `factor`, the weight multiplier
#'   for kept rows, `active`, the activated panels, `pools`, the per-block
#'   activation record, and `pool`, the record pool of each row.
#' @noRd
activate_panels <- function(sample, record, active, allow_empty = FALSE,
                            window = NULL, part = window, at = NULL,
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
      pools = list(),
      pool = rep(NA_integer_, n)
    ))
  }

  data <- as.data.frame(sample)
  keys <- make_group_key(data, record$key_vars)
  panel <- data$.panel

  keep <- rep(FALSE, nrow(data))
  factor <- rep(NA_real_, nrow(data))
  permanent <- rep(FALSE, nrow(data))
  pool_of <- rep(NA_integer_, nrow(data))
  pools <- vector("list", length(record$pools))
  located <- record_pool_rows(keys, record$pools)

  for (p in seq_along(record$pools)) {
    pool <- record$pools[[p]]
    rows <- located[[p]]$rows
    pool_of[rows] <- p

    if (identical(pool$activation, "permanent")) {
      # Exclude permanent pools from randomized quota denominators.
      keep[rows] <- TRUE
      factor[rows] <- 1
      permanent[rows] <- TRUE
      take <- pool$blocks
      probability <- rep(1, length(pool$blocks))
    } else {
      take <- pool_take(pool, active)
      # Reporting periods leave an occasion empty legitimately, and older records
      # did not enforce positive activation.
      if (any(take == 0L) && !allow_empty) {
        abort_inactive_pool(pool, take, record, window, part, at,
                            call = call)
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
    pools = pools,
    pool = pool_of
  )
}

#' The visit each row of an occasion is at
#'
#' Read from the schedule by panel. A permanent unit is active at every occasion
#' outside the rotation, so it has no visit number.
#' @noRd
occasion_row_visits <- function(record, panel, occasion, permanent) {
  # paste() would recycle no panels to one key.
  if (length(panel) == 0L) {
    return(integer(0))
  }
  schedule <- record$schedule
  at <- match(
    paste(panel, occasion, sep = "|"),
    paste(schedule$panel, schedule$occasion, sep = "|")
  )
  visit <- as.integer(schedule$visit[at])
  visit[permanent] <- NA_integer_
  visit
}
