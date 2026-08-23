## Joint expectations of the activation

# Conditional moments use frozen quotas for a block of m units.
#
#   P(i active at t)                     = a_t / m
#   P(i active at t and at s)            = a_both / m
#   P(i active at t, j active at s)      = (a_t a_s - a_both) / {m (m - 1)}
#
# Cross-block moments are marginal products. These are conditional activation
# moments rather than full two-phase joint chances.

#' Arguments the activation mode does not use
#'
#' Everything an activation joint needs was frozen at the master's draw, so
#' the frame, the stage selector and the simulation controls have no role.
#' They are refused by name rather than accepted without effect, and each is
#' named as itself so the message does not report a cause that was not given.
#' @noRd
check_activation_mode_arguments <- function(
  frame,
  stages,
  nsim_supplied,
  seed_supplied = FALSE,
  call = caller_env()
) {
  supplied <- c(
    if (!is_null(frame)) "a frame",
    if (!is_null(stages)) "stages",
    if (isTRUE(nsim_supplied)) "nsim",
    if (isTRUE(seed_supplied)) "seed"
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
  # Validate the version before reading fields.
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
      # Permanent units stay outside randomized quota denominators.
      return(units)
    }
    if (length(panels) == 0L) {
      return(rep(0L, n_blocks))
    }
    pool_take(pool, panels)
  }

  take_1 <- take(first)
  take_2 <- take(second)
  take_both <- take(both)

  # Distinct-pair expectation is undefined for a block of one.
  pairs <- units > 1L
  joint_distinct <- rep(NA_real_, n_blocks)
  joint_distinct[pairs] <- (take_1[pairs] * take_2[pairs] - take_both[pairs]) /
    (units[pairs] * (units[pairs] - 1))

  # Compute columns before `tibble()` can mask local names.
  stratum <- format_pool_stratum(pool, empty = NA_character_)
  class <- pool$class
  # Selection class and activation status can differ for small pools.
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

