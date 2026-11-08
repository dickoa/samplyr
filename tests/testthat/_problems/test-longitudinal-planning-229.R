# Extracted from test-longitudinal-planning.R:229

# test -------------------------------------------------------------------------
plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate"
  )
schedule <- svyplan::design_schedule(
    plan, 4, "continuing",
    refreshment = "entrant_register", rounding = "ceiling"
  )
startup_n <- plan$n_cohorts * ceiling(plan$n_entrants)
design <- sampling_design() |> draw(n = startup_n)
expect_error(
    execute(
      design, data.frame(id = 1:200), panels = schedule,
      small_pool = "permanent"
    ),
    # One class per defect kind: this is an argument conflict.
    class = "samplyr_error_plan_small_pool"
  )
expect_error(
    sampling_design() |>
      draw(
        n = startup_n,
        method = "pps_systematic",
        mos = mos,
        certainty_size = 800
      ) |>
      execute(
        data.frame(
          id = 1:40,
          mos = c(rep(10, 36), 900, 950, 1000, 1100)
        ),
        panels = schedule
      ),
    # And this is a property of the realized draw, not of the arguments.
    class = "samplyr_error_plan_certainty"
  )
unsupported <- schedule
unsupported$schedule$selection <- NULL
