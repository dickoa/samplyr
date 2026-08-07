## Panel assignment

# `execute(panels = k)` partitions a realized sample into k operational
# groups. The partition is a randomized fixed-quota assignment inside frozen
# ordered micro-pools: within every first-stage selection stratum the
# assignment units are ordered, cut into consecutive blocks, given fixed
# per-panel quotas, and permuted within each block.
#
# The construction has two exact probability statements at different
# conditioning levels, and they answer different questions. Before the
# pool-level identity permutation is fixed, every unit has marginal
# probability 1/k of carrying any given panel. Once it is fixed, activating a
# set of panels takes a simple random sample without replacement of the
# realized quota from each block, so within block `b` a unit's conditional
# probability is `q_bg / m_b` and a pair's is
# `q_bg (q_bg - 1) / {m_b (m_b - 1)}`. The block sizes and the block-by-panel
# quotas are therefore recorded with the sample: they, not 1/k, are what a
# later activation is computed from.
#
# Certainty units are labelled from their own pools. They are permanent, so
# they consume no rotating quota.
#
# `panels` takes either a count or a rotation schedule. A count carries no
# information about how few panels a later wave will activate, so it uses the
# worst case block size 2k. A schedule states it, and the block size follows
# `r_min`, the fewest panels any declared wave activates: finer blocks retain
# more of the assignment order, and the take stays large enough for a
# within-block variance estimate.

#' Normalize the `panels` argument to a count and an optional schedule
#'
#' @return `NULL`, or a list with `k`, `r_min`, `block_size` and `schedule`.
#' @noRd
normalize_panel_input <- function(panels, call = caller_env()) {
  if (is_null(panels)) {
    return(NULL)
  }
  if (is.data.frame(panels)) {
    return(normalize_panel_schedule(panels, call = call))
  }
  if (
    !is.numeric(panels) ||
      length(panels) != 1 ||
      !is_integerish_numeric(panels) ||
      panels < 2
  ) {
    cli_abort(
      "{.arg panels} must be a single integer >= 2, or a rotation schedule",
      call = call
    )
  }
  k <- as.integer(panels)
  list(
    k = k,
    r_min = 1L,
    block_size = panel_block_size(k, 1L),
    schedule = NULL
  )
}

#' Validate and normalize a rotation schedule
#'
#' The stored form is the complete panel-by-wave grid even when the input
#' names only the active rows, so a reader never has to know which convention
#' the caller used.
#' @noRd
normalize_panel_schedule <- function(schedule, call = caller_env()) {
  missing <- setdiff(c("panel", "wave"), names(schedule))
  if (length(missing) > 0) {
    abort_samplyr(
      c(
        "A {.arg panels} schedule needs a {.field panel} and a {.field wave}
         column.",
        "x" = "Missing: {.field {missing}}.",
        "i" = "An {.field active} column is optional: without it every row
               given is active, and every combination left out is not."
      ),
      class = "samplyr_error_panel_schedule_columns",
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

  check_schedule_integers(panel, "panel", call = call)
  check_schedule_integers(wave, "wave", call = call)
  if (!is.logical(active) || anyNA(active)) {
    abort_samplyr(
      "The {.field active} column of a {.arg panels} schedule must be logical
       and complete.",
      class = "samplyr_error_panel_schedule_active",
      call = call
    )
  }
  panel <- as.integer(panel)
  wave <- as.integer(wave)

  if (anyDuplicated(paste(panel, wave, sep = "|")) > 0) {
    abort_samplyr(
      c(
        "A {.arg panels} schedule may declare each panel once per wave.",
        "x" = "It repeats at least one {.field panel}-{.field wave}
               combination."
      ),
      class = "samplyr_error_panel_schedule_duplicates",
      call = call
    )
  }

  k <- max(panel)
  n_waves <- max(wave)
  check_schedule_contiguous(panel, k, "panel", call = call)
  check_schedule_contiguous(wave, n_waves, "wave", call = call)
  if (k < 2L) {
    abort_samplyr(
      "A {.arg panels} schedule must declare at least 2 panels.",
      class = "samplyr_error_panel_schedule_size",
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
  at <- match(
    paste(grid$panel, grid$wave, sep = "|"),
    paste(panel, wave, sep = "|")
  )
  grid$active <- !is.na(at) & active[at]

  per_wave <- vapply(
    split(grid$active, grid$wave),
    sum,
    integer(1)
  )
  if (any(per_wave == 0L)) {
    idle <- as.integer(names(per_wave)[per_wave == 0L])
    abort_samplyr(
      c(
        "Every declared wave must activate at least one panel.",
        "x" = "No panel is active at wave {idle}."
      ),
      class = "samplyr_error_panel_schedule_idle_wave",
      call = call
    )
  }

  r_min <- min(per_wave)
  list(
    k = k,
    r_min = r_min,
    block_size = panel_block_size(k, r_min),
    schedule = grid
  )
}

#' @noRd
check_schedule_integers <- function(x, name, call = caller_env()) {
  ok <- is.numeric(x) &&
    !anyNA(x) &&
    is_integerish_numeric(x) &&
    all(x >= 1)
  if (!ok) {
    abort_samplyr(
      "The {.field {name}} column of a {.arg panels} schedule must hold
       complete integers of 1 or more.",
      class = "samplyr_error_panel_schedule_values",
      call = call
    )
  }
  invisible(NULL)
}

#' @noRd
check_schedule_contiguous <- function(x, top, name, call = caller_env()) {
  gaps <- setdiff(seq_len(top), unique(x))
  if (length(gaps) > 0) {
    abort_samplyr(
      c(
        "A {.arg panels} schedule must number its {.field {name}} values
         from 1 without gaps.",
        "x" = "Declared up to {top}, but missing: {gaps}."
      ),
      class = "samplyr_error_panel_schedule_gap",
      call = call
    )
  }
  invisible(NULL)
}

#' Block size for a pool
#'
#' `r_min` is the fewest panels any declared wave activates. A block must be
#' large enough that the take of `r_min` panels leaves two units per block,
#' which is the smallest take carrying a within-block variance estimate.
#' @noRd
panel_block_size <- function(k, r_min) {
  as.integer(k * ceiling(2 / r_min))
}

#' Assign panel labels by blocked random quota within frozen pools
#'
#' Returns the sample carrying a `.panel` column and the assignment record
#' [execute()] stores in the receipt.
#' @noRd
assign_panels <- function(result, spec, first_stage_spec, first_stage_num) {
  k <- spec$k
  block <- spec$block_size
  cluster_spec <- first_stage_spec$clusters
  strata_spec <- first_stage_spec$strata
  control_quos <- first_stage_spec$draw_spec$control

  key_vars <- panel_unit_key_vars(result, cluster_spec, first_stage_num)

  if (is_null(cluster_spec)) {
    unit_rows <- seq_len(nrow(result))
    rows_of_unit <- NULL
  } else {
    row_keys <- make_group_key(result, key_vars)
    unit_rows <- which(!duplicated(row_keys))
    rows_of_unit <- match(row_keys, row_keys[unit_rows])
  }

  units <- result[unit_rows, , drop = FALSE]
  pools <- panel_pools(units, strata_spec, first_stage_num)

  panel <- integer(length(unit_rows))
  pool_records <- vector("list", length(pools))
  for (p in seq_along(pools)) {
    pool <- pools[[p]]
    idx <- panel_pool_order(units, pool$indices, control_quos)
    assigned <- assign_blocked_panels(length(idx), k, block)
    panel[idx] <- assigned$panel
    pool_records[[p]] <- list(
      stratum = pool$stratum,
      class = pool$class,
      size = length(idx),
      keys = make_group_key(units[idx, , drop = FALSE], key_vars),
      blocks = assigned$blocks,
      quotas = block_quotas(assigned$panel, assigned$blocks, k)
    )
  }

  result$.panel <- if (is_null(rows_of_unit)) panel else panel[rows_of_unit]

  record <- list(
    algorithm = "blocked_random_quota",
    version = 1L,
    panels = k,
    block_size = block,
    r_min = spec$r_min,
    unit = if (is_null(cluster_spec)) "element" else "psu",
    key_vars = key_vars,
    control_ordered = length(control_quos) > 0L,
    certainty = "permanent",
    schedule = spec$schedule,
    pools = pool_records
  )

  list(sample = result, record = record)
}

#' Identity of an assignment unit
#'
#' Element-level assignment keys on `.sample_id`. A clustered first stage
#' keys on its cluster variables, plus the stage draw column when the stage
#' is with replacement: the realized draw occurrence, not the population
#' cluster, is the unit the estimator and the survey export use, so the same
#' cluster may be labelled into different panels.
#' @noRd
panel_unit_key_vars <- function(result, cluster_spec, first_stage_num) {
  if (is_null(cluster_spec)) {
    return(".sample_id")
  }
  draw_col <- paste0(".draw_", first_stage_num)
  c(cluster_spec$vars, intersect(draw_col, names(result)))
}

#' Split assignment units into frozen pools
#'
#' Pools are the first-stage selection strata, each split into its rotating
#' and its permanent-certainty part. Pool order is deterministic, which is
#' what makes the assignment reproducible from the seed.
#' @noRd
panel_pools <- function(units, strata_spec, first_stage_num) {
  if (is_null(strata_spec)) {
    indices <- list(seq_len(nrow(units)))
    key_df <- NULL
  } else {
    groups <- split_row_indices(units, strata_spec$vars)
    indices <- groups$indices
    key_df <- groups$key_df
  }

  certainty <- panel_certainty_flag(units, first_stage_num)

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

#' @noRd
panel_certainty_flag <- function(units, first_stage_num) {
  col <- paste0(".certainty_", first_stage_num)
  if (!col %in% names(units)) {
    return(rep(FALSE, nrow(units)))
  }
  flag <- as.logical(units[[col]])
  !is.na(flag) & flag
}

#' Order the units of one pool before blocking
#'
#' The explicit panel control order when the stage declares one, and the
#' realized order otherwise. `arrange()` is stable, so control ties keep the
#' realized order and the ordering is recoverable from the sample.
#' @noRd
panel_pool_order <- function(units, idx, control_quos) {
  if (length(control_quos) == 0L || length(idx) <= 1L) {
    return(idx)
  }
  sub <- units[idx, , drop = FALSE]
  position <- free_column_name(sub, ".panel_position")
  sub[[position]] <- seq_along(idx)
  idx[arrange(sub, !!!control_quos)[[position]]]
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

  # Panel identities are permuted once per pool and frozen. The multiset of
  # quota sizes stays deterministic and only the labelling is random, which
  # is what keeps every unit's marginal exactly 1/k when m is not a multiple
  # of k.
  labels <- sample.int(k)[rep_len(seq_len(k), m)]
  sizes <- panel_block_sizes(m, block)

  panel <- integer(m)
  start <- 0L
  for (size in sizes) {
    at <- start + seq_len(size)
    # Never bare sample(): on a length-one pool it permutes seq_len(x).
    panel[at] <- labels[at][sample.int(size)]
    start <- start + size
  }
  list(panel = panel, blocks = sizes)
}

#' Sizes of the consecutive blocks of one pool
#'
#' A pool smaller than one block is a single block, still assignable but with
#' no block-level order structure. Otherwise the tail is spread one unit at a
#' time over the full blocks, never left standalone.
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
