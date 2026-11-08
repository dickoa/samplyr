# Extracted from test-reporting-periods.R:310

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
  schedule <- expand.grid(panel = 1:12, wave = 1:12)
  group <- ceiling(schedule$panel / 3)
  month <- (schedule$panel - 1) %% 3 + 1
  quarter <- ceiling(schedule$wave / 3)
  schedule$active <- month == (schedule$wave - 1) %% 3 + 1 &
    mapply(function(g, q) g %in% in_quarter[[q]], group, quarter)
  if (period) {
    schedule$period <- quarter
  }
  schedule
}
retained_months <- function() {
  schedule <- expand.grid(panel = 1:15, wave = 1:12)
  schedule$active <- (schedule$wave - 1) %% 3 == (schedule$panel - 1) %% 3
  schedule$period <- ceiling(schedule$wave / 3)
  schedule
}
period_master <- function(n, schedule = rotating_months(), seed = 1) {
  sampling_design() |>
    stratify_by(h) |>
    draw(n = n) |>
    execute(period_frame(), seed = seed, panels = schedule)
}
monthly_plan <- function(subrounds = 3, tenures = 1,
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
    tenures = tenures,
    subrounds = subrounds
  )
}

# test -------------------------------------------------------------------------
expect_null(samplyr:::normalize_panel_input(monthly_plan(1))$reporting)
