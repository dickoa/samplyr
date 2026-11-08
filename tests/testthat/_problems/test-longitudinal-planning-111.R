# Extracted from test-longitudinal-planning.R:111

# test -------------------------------------------------------------------------
plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate"
  )
schedule <- svyplan::design_schedule(
    plan, 6, "continuing",
    refreshment = "entrant_register", rounding = "ceiling"
  )
intake_n <- ceiling(plan$n_entrants)
startup_n <- plan$n_cohorts * intake_n
startup <- sampling_design() |>
    draw(n = startup_n) |>
    execute(data.frame(id = 1:200), seed = 41, panels = schedule)
