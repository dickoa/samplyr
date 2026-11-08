# Extracted from test-reporting-periods.R:292

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "samplyr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
period_frame <- function() {
  data.frame(
    id = 1:600,
    h = rep(c("a", "b", "c"), each = 200),
    y = rep(c(10, 20, 30), each = 200) + (1:600) %% 7
  )
}
rotating_months <- function(period = TRUE) {
  in_quarter <- list(c(1, 4), c(1, 2), c(2, 3), c(3, 4))
  schedule <- expand.grid(panel = 1:12, subround = 1:3, period = 1:4)
  group <- ceiling(schedule$panel / 3)
  month <- (schedule$panel - 1) %% 3 + 1
  schedule$active <- month == schedule$subround &
    mapply(function(g, q) g %in% in_quarter[[q]], group, schedule$period)
  if (!period) {
    schedule$period <- (schedule$period - 1L) * 3L + schedule$subround
    schedule$subround <- NULL
  }
  schedule
}
retained_months <- function() {
  schedule <- expand.grid(panel = 1:15, subround = 1:3, period = 1:4)
  schedule$active <- schedule$subround == (schedule$panel - 1) %% 3 + 1
  schedule
}
period_master <- function(n, schedule = rotating_months(), seed = 1) {
  sampling_design() |>
    stratify_by(h) |>
    draw(n = n) |>
    execute(period_frame(), seed = seed, panels = schedule)
}
monthly_plan <- function(subrounds = 3, retain_psus = FALSE,
                         policy = "continuing") {
  svyplan::design_schedule(
    svyplan::n_panel(
      svyplan::n_prop(p = 0.5, moe = 0.02, deff = 1.8),
      retention = rep(0.9, 4),
      resp_rate = 0.8,
      design = "rotating",
      start = "immediate"
    ),
    horizon = 4,
    horizon_policy = policy,
    refreshment = "entrant_register",
    rounding = "ceiling",
    take = 14,
    retain_psus = retain_psus,
    subrounds = subrounds
  )
}

# test -------------------------------------------------------------------------
master <- period_master(n = 3, seed = 1)
rolling <- expect_error(
    execute(master, period = c(1, 1, 2), subround = c(2, 3, 1)),
    class = "samplyr_error_panel_small_pool"
  )
expect_match(
    cli::ansi_strip(conditionMessage(rolling)),
    paste("This window, period 1 subrounds 2 and 3, and period 2 subround 1,",
          "cannot be materialized: it holds no unit of pool"),
    fixed = TRUE
  )
