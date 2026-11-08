# Extracted from test-longitudinal-planning.R:284

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
program <- rotation_program(
    list(startup = startup), schedule = schedule, through = 1
  )
expect_null(attr(startup, "metadata")$panel_assignment)
expect_identical(program$panels, c(startup = 1L))
expect_identical(names(execute(program, period = 1)), "startup")
