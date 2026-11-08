# Extracted from test-longitudinal-planning.R:432

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
      schedule = schedule, through = 3
    ),
    class = "samplyr_error_program_plan_selections"
  )
program <- rotation_program(
    list(startup = startup, period_2 = period_2),
    schedule = schedule, through = 4
  )
expect_identical(program$entry_wave, c(startup = 1L, period_2 = 4L))
expect_identical(program$waves, 1:4)
tab <- schedule$schedule
live <- tab[tab$selection == "period_2" & tab$active & tab$period == 2, ]
expect_identical(live$subround[order(live$panel)], 1:3)
wave_4 <- execute(program, period = 2, subround = 1)
