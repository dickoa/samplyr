# Extracted from test-longitudinal-planning.R:265

# test -------------------------------------------------------------------------
plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "gradual"
  )
schedule <- svyplan::design_schedule(
    plan, 4, "continuing",
    refreshment = "entrant_register", rounding = "ceiling"
  )
startup_n <- ceiling(plan$n_entrants)
startup <- sampling_design() |>
    draw(n = startup_n) |>
    execute(data.frame(id = 1:200), seed = 51, panels = schedule)
