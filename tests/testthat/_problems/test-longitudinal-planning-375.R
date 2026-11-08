# Extracted from test-longitudinal-planning.R:375

# test -------------------------------------------------------------------------
plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.85, 0.90),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate",
    rotation = "2-2-2"
  )
schedule <- svyplan::design_schedule(
    plan, 8, "continuing",
    refreshment = "entrant_register", rounding = "ceiling"
  )
intake_n <- schedule$selections$panel_issue[[1L]]
startup_n <- schedule$selections$operational_issue[[1L]]
expect_equal(startup_n, schedule$life_length * intake_n)
selections <- list(
    startup = sampling_design() |>
      draw(n = startup_n) |>
      execute(data.frame(id = sprintf("S%03d", 1:400)), seed = 31,
              panels = schedule)
  )
for (t in 2:8) {
    selections[[paste0("period_", t)]] <- sampling_design() |>
      draw(n = intake_n) |>
      execute(data.frame(id = sprintf("I%d_%03d", t, 1:100)), seed = 30 + t)
  }
program <- rotation_program(selections, schedule = schedule, through = 8)
ids <- lapply(1:8, function(t) {
    unlist(lapply(execute(program, period = t), function(z) z$id))
  })
