# Extracted from test-longitudinal-planning.R:340

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate"
  )
schedule <- svyplan::design_schedule(
    plan, 2, "continuing",
    refreshment = "whole_vintage", rounding = "ceiling"
  )
intake_n <- ceiling(plan$n_entrants)
startup <- sampling_design() |>
    draw(n = plan$n_cohorts * intake_n) |>
    execute(data.frame(id = 1:200), seed = 71, panels = schedule)
intake_2 <- sampling_design() |>
    draw(n = intake_n) |>
    execute(data.frame(id = 1:200), seed = 72)
program <- rotation_program(
    list(startup = startup, period_2 = intake_2),
    schedule = schedule,
    through = 2
  )
expect_false(schedule$requires_disjoint_entrants)
expect_error(
    as_svydesign(execute(program, period = 2)),
    class = "samplyr_error_rotation_occasion_not_combinable"
  )
