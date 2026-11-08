# Extracted from test-longitudinal-planning.R:314

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
