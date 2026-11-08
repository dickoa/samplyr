# Extracted from test-longitudinal-planning.R:56

# test -------------------------------------------------------------------------
plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate"
  )
intake_n <- ceiling(plan$n_entrants)
startup_n <- plan$n_cohorts * intake_n
schedule <- svyplan::design_schedule(
    plan,
    horizon = 6,
    horizon_policy = "continuing",
    refreshment = "entrant_register",
    rounding = "ceiling"
  )
expect_gte(startup_n, plan$n_in_sample)
expect_equal(startup_n %% plan$n_cohorts, 0)
startup <- sampling_design() |>
    draw(n = startup_n) |>
    execute(
      data.frame(id = sprintf("S%03d", 1:200)),
      seed = 31,
      panels = schedule
    )
