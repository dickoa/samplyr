# Extracted from test-longitudinal-planning.R:429

# test -------------------------------------------------------------------------
plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.1),
    retention = c(0.9, 0.9, 0.9),
    resp_rate = 0.8,
    design = "rotating",
    start = "immediate",
    rotation = "2-2-2"
  )
schedule <- svyplan::design_schedule(
    plan, 4, "continuing",
    refreshment = "entrant_register", rounding = "ceiling",
    take = 6, subrounds = 3
  )
issue <- schedule$selections$operational_issue
expect_identical(schedule$selections$panels, c(18L, 3L, 3L, 3L))
startup <- sampling_design() |>
    draw(n = issue[[1L]]) |>
    execute(data.frame(id = 1:200), seed = 81, panels = schedule)
entrants <- data.frame(id = 201:300)
period_2 <- sampling_design() |>
    draw(n = issue[[2L]]) |>
    execute(entrants, seed = 82, panels = 3)
expect_error(
    rotation_program(
      list(startup = startup, period_2 = period_2),
      schedule = schedule, through = 1
    ),
    class = "samplyr_error_program_plan_selections"
  )
program <- rotation_program(
    list(startup = startup, period_2 = period_2),
    schedule = schedule, through = c(period = 2, subround = 1)
  )
expect_identical(program$entry_period, c(startup = 1L, period_2 = 2L))
expect_identical(
    program$occasions,
    data.frame(period = c(1L, 1L, 1L, 2L), subround = c(1:3, 1L))
  )
