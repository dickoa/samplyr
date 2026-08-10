## Joint expectations of the activation

# `joint_expectation(master, waves = c(t, s))` states how two occasions of a
# rotation overlap, exactly, from the record the master froze at its draw.
#
# Conditional on the frozen quotas, the panels inside a block are an
# arrangement of that block's label multiset, so for a block of `m`
# assignment units with takes `a_t` and `a_s` and intersection take
# `a_both = sum(q_bg for g in A_t and A_s)`:
#
#   P(i active at t)                     = a_t / m
#   P(i active at t and at s)            = a_both / m
#   P(i active at t, j active at s)      = (a_t a_s - a_both) / {m (m - 1)}
#
# for units i != j of one block, and the product of the marginals for units of
# different blocks, whose position permutations are drawn independently. The
# same-wave case is the third expression at `t == s`, which is why one
# implementation answers both questions.
#
# Two properties are worth knowing rather than rediscovering. Quotas are
# routinely unequal, because the spread-tail rule gives one block an extra
# unit whenever a pool is not a multiple of the block size, so `q_bg` is
# always read from the record and never assumed flat. And a certainty block
# takes every unit at every wave, so `a_t = a_s = a_both = m` and the third
# expression is exactly one; permanence needs no branch in the arithmetic.
#
# What this states is conditional on the phase-1 units and on the frozen
# quotas. It is not the unconditional joint inclusion probability of the
# two-phase design, which also carries the master's own pairwise term.

#' Arguments the activation mode does not use
#'
#' Everything an activation joint needs was frozen at the master's draw, so
#' the frame, the stage selector and the simulation count have no role. They
#' are refused by name rather than accepted without effect.
#' @noRd
check_activation_mode_arguments <- function(
  frame,
  stages,
  nsim_supplied,
  call = caller_env()
) {
  supplied <- c(
    if (!is_null(frame)) "a frame",
    if (!is_null(stages)) "stages",
    if (isTRUE(nsim_supplied)) "nsim"
  )
  if (length(supplied) == 0) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.arg waves} reads the assignment the master already froze, so it
       takes no further input.",
      "x" = "Also given: {supplied}.",
      "i" = "{.arg stages} describes the selection of the master itself:
             ask for it in a separate call."
    ),
    class = "samplyr_error_joint_activation_arguments",
    call = call
  )
}

#' The pair of waves an activation joint is asked for
#' @noRd
check_waves_pair <- function(waves, schedule, call = caller_env()) {
  ok <- is.numeric(waves) &&
    length(waves) == 2L &&
    !anyNA(waves) &&
    is_integerish_numeric(waves)
  if (!ok) {
    abort_samplyr(
      c(
        "{.arg waves} must be two wave numbers.",
        "i" = "Give the pair to compare, or the same wave twice for the
               joint expectation within one wave:
               {.code waves = c(2, 2)}."
      ),
      class = "samplyr_error_waves_not_a_pair",
      call = call
    )
  }
  vapply(waves, check_wave_declared, integer(1), schedule = schedule,
         call = call)
}

#' Joint expectations of the activation indicators of two waves
#' @noRd
activation_joint_expectation <- function(master, waves, call = caller_env()) {
  # The version before any field, including the field that says whether there
  # are waves to compute a joint expectation over.
  record <- attr(master, "metadata")$panel_assignment
  if (!is_null(record)) {
    record <- prepare_panel_record(record, "A joint expectation", call = call)
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

  waves <- check_waves_pair(waves, record$schedule, call = call)
  schedule <- record$schedule
  active_at <- function(w) schedule$panel[schedule$wave == w & schedule$active]
  first <- active_at(waves[1])
  second <- active_at(waves[2])
  both <- intersect(first, second)

  rows <- lapply(
    seq_along(record$pools),
    function(p) activation_joint_pool(record$pools[[p]], p, waves,
                                      first, second, both)
  )
  vctrs::vec_rbind(!!!rows)
}

#' One pool's contribution, a row per block
#' @noRd
activation_joint_pool <- function(pool, index, waves, first, second, both) {
  units <- as.integer(pool$blocks)
  n_blocks <- length(units)

  take <- function(panels) {
    if (identical(pool$activation, "permanent")) {
      # Permanent by policy: in the sample at every wave, and outside the
      # randomized quota denominator. Selection certainty and a pool too
      # small to rotate are both permanent and share this arithmetic.
      return(units)
    }
    if (length(panels) == 0L) {
      return(rep(0L, n_blocks))
    }
    as.integer(rowSums(pool$quotas[, panels, drop = FALSE]))
  }

  take_1 <- take(first)
  take_2 <- take(second)
  take_both <- take(both)

  # A block of one unit has no distinct pair, so the pairwise expectation is
  # not zero but undefined. Sub-minimum pools of exactly one unit exist.
  pairs <- units > 1L
  joint_distinct <- rep(NA_real_, n_blocks)
  joint_distinct[pairs] <- (take_1[pairs] * take_2[pairs] - take_both[pairs]) /
    (units[pairs] * (units[pairs] - 1))

  # Every column is computed before the call: tibble() masks by column name
  # as it builds, so a column named for a local would shadow it.
  stratum <- activation_pool_stratum(pool)
  class <- pool$class
  # `class` is the master's selection status and `activation` is whether the
  # units rotate. They differ for a pool promoted because it is too small to
  # rotate, which stays "rotating" and reads probability one.
  activation <- pool$activation
  prob_1 <- take_1 / units
  prob_2 <- take_2 / units
  joint_same <- take_both / units

  tibble::tibble(
    pool = rep(as.integer(index), n_blocks),
    stratum = rep(stratum, n_blocks),
    class = rep(class, n_blocks),
    activation = rep(activation, n_blocks),
    block = seq_len(n_blocks),
    wave_1 = rep(waves[1], n_blocks),
    wave_2 = rep(waves[2], n_blocks),
    units = units,
    take_1 = take_1,
    take_2 = take_2,
    take_both = take_both,
    prob_1 = prob_1,
    prob_2 = prob_2,
    joint_same = joint_same,
    joint_distinct = joint_distinct,
    has_pair = pairs
  )
}

#' The stratum a pool belongs to, rendered for a table column
#' @noRd
activation_pool_stratum <- function(pool) {
  if (is_null(pool$stratum)) {
    return(NA_character_)
  }
  paste(
    paste0(names(pool$stratum), "=", vapply(pool$stratum, format, character(1))),
    collapse = ", "
  )
}
