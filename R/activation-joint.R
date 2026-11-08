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
      "{.arg period} reads the assignment the master already froze, so it
       takes no further input.",
      "x" = "Also given: {supplied}.",
      "i" = "{.arg stages} describes the selection of the master itself:
             ask for it in a separate call."
    ),
    class = "samplyr_error_joint_activation_arguments",
    call = call
  )
}

#' The two occasions an activation joint is asked for
#'
#' Pairs under the recycling rule of `execute()`, with exactly two
#' occasions after recycling, which may be the same one. A period alone
#' names an occasion only where the period has one subround.
#' @return The two occasion numbers, in the order asked.
#' @noRd
check_occasions_pair <- function(request, schedule, call = caller_env()) {
  period <- request$period
  subround <- request$subround
  whole <- function(x) {
    is.numeric(x) && length(x) %in% 1:2 && !anyNA(x) &&
      is_integerish_numeric(x) && all(x >= 1)
  }
  n <- max(length(period), length(subround))
  ok <- whole(period) && (is_null(subround) || whole(subround)) && n == 2L &&
    (length(period) == 1L || length(subround) %in% c(0L, 1L, length(period)))
  if (!ok) {
    abort_samplyr(
      c(
        "{.arg period} and {.arg subround} must name two occasions.",
        "i" = "Give the pair to compare, as {.code period = c(2, 3)} or
               {.code period = 2, subround = c(1, 2)}, or the same occasion
               twice for the joint expectation within one occasion:
               {.code period = c(2, 2)}."
      ),
      class = "samplyr_error_occasions_not_a_pair",
      call = call
    )
  }
  pairs <- grid_pairs(schedule)
  period <- rep_len(as.integer(period), 2L)
  if (is_null(subround)) {
    several <- vapply(period, function(p) sum(pairs$period == p) > 1L,
                      logical(1))
    if (any(several)) {
      abort_samplyr(
        c(
          "Period {period[several][1L]} holds several subrounds, so it names
           no single occasion.",
          "i" = "Name the subround, {.code period = c(...), subround =
                 c(...)}."
        ),
        class = "samplyr_error_occasions_not_a_pair",
        call = call
      )
    }
    subround <- rep(1L, 2L)
  }
  subround <- rep_len(as.integer(subround), 2L)
  occasion <- match(paste(period, subround), paste(pairs$period, pairs$subround))
  if (anyNA(occasion)) {
    missed <- paste0("period ", period[is.na(occasion)], " subround ",
                     subround[is.na(occasion)])
    abort_occasion_undeclared(pairs, paste(unique(missed), collapse = ", "),
                              call)
  }
  occasion
}

#' Joint expectations of the activation indicators of two occasions
#' @noRd
activation_joint_expectation <- function(master, occasions, call = caller_env()) {
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
               panel, period and active>)}.",
        if (!is_null(record)) c(
          "i" = "{.code panels = {record$panels}} partitions the sample but
                 declares no occasions."
        )
      ),
      class = "samplyr_error_occasion_no_schedule",
      call = call
    )
  }

  occasions <- check_occasions_pair(occasions, record$schedule, call = call)
  schedule <- record$schedule
  pairs <- grid_pairs(schedule)
  named <- list(period = pairs$period[occasions],
                subround = pairs$subround[occasions])
  active_at <- function(w) {
    schedule$panel[schedule$occasion == w & schedule$active]
  }
  first <- active_at(occasions[1])
  second <- active_at(occasions[2])
  both <- intersect(first, second)

  rows <- lapply(
    seq_along(record$pools),
    function(p) activation_joint_pool(record$pools[[p]], p, named,
                                      first, second, both)
  )
  vctrs::vec_rbind(!!!rows)
}

#' One pool's contribution, a row per block
#' @noRd
activation_joint_pool <- function(pool, index, occasions, first, second,
                                  both) {
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
    period_1 = rep(occasions$period[1], n_blocks),
    subround_1 = rep(occasions$subround[1], n_blocks),
    period_2 = rep(occasions$period[2], n_blocks),
    subround_2 = rep(occasions$subround[2], n_blocks),
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

