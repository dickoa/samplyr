test_that("four-occasion overlap has the expected first-lag share", {
  overlap <- svyplan::design_overlap("4")

  expect_equal(overlap$overlap[[1L]], 0.75)
  expect_equal(overlap$shared, 3:1)
})

test_that("a rotating panel plan keeps launch and intake counts distinct", {
  plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate"
  )

  expect_s3_class(plan, "svyplan_panel")
  expect_identical(plan$n_cohorts, 4L)
  expect_equal(plan$n_in_sample, 4 * plan$n_entrants)
  expect_gt(ceiling(plan$n_in_sample), ceiling(plan$n_entrants))
  expect_equal(as.integer(plan), ceiling(plan$n_entrants))

  expect_error(
    sampling_design() |> draw(n = plan),
    "must be numeric or a data frame"
  )
})

test_that("panel planning counts hand off to a multi-cohort program", {
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
    svyplan::design_rotation("4"),
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

  intake_2 <- sampling_design() |>
    draw(n = intake_n) |>
    execute(data.frame(id = sprintf("A%03d", 1:100)), seed = 32)
  intake_3 <- sampling_design() |>
    draw(n = intake_n) |>
    execute(data.frame(id = sprintf("B%03d", 1:100)), seed = 33)

  program <- rotation_program(
    cohorts = list(
      startup = startup,
      intake_2 = intake_2,
      intake_3 = intake_3
    ),
    schedule = schedule,
    through = 3
  )
  wave_2 <- execute(program, wave = 2)
  wave_3 <- execute(program, wave = 3)

  expect_s3_class(program, "rotation_program")
  expect_s3_class(wave_2, "rotation_wave")
  expect_s3_class(wave_3, "rotation_wave")
  expect_identical(names(program$cohorts), c(
    "startup", "intake_2", "intake_3"
  ))
  expect_identical(program$entry_wave,
                   c(startup = 1L, intake_2 = 2L, intake_3 = 3L))
  expect_equal(nrow(intake_2), intake_n)
  expect_equal(nrow(intake_3), intake_n)
  expect_identical(names(wave_2), c("startup", "intake_2"))
  expect_identical(names(wave_3), c("startup", "intake_2", "intake_3"))
  for (cohort in wave_3) {
    expect_s3_class(cohort, "tbl_sample")
    expect_true(sample_realization_status(cohort)$ok)
  }
})

test_that("the planning route has one source for entries and one prefix", {
  plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate"
  )
  schedule <- svyplan::design_schedule(
    plan, svyplan::design_rotation("4"), 6, "continuing",
    refreshment = "entrant_register", rounding = "ceiling"
  )
  intake_n <- ceiling(plan$n_entrants)
  startup_n <- plan$n_cohorts * intake_n
  startup <- sampling_design() |>
    draw(n = startup_n) |>
    execute(data.frame(id = 1:200), seed = 41, panels = schedule)
  intake_2 <- sampling_design() |>
    draw(n = intake_n) |>
    execute(data.frame(id = 201:300), seed = 42)
  cohorts <- list(startup = startup, intake_2 = intake_2)

  expect_error(
    rotation_program(cohorts, schedule = schedule),
    class = "samplyr_error_program_through"
  )
  expect_error(
    rotation_program(
      cohorts,
      entry_wave = c(startup = 1, intake_2 = 2),
      schedule = schedule,
      through = 2
    ),
    class = "samplyr_error_program_plan_argument"
  )
  expect_error(
    rotation_program(
      cohorts,
      entry_wave = c(startup = 1, intake_2 = 2),
      schedule = data.frame(panel = 1, wave = 1),
      through = 2
    ),
    class = "samplyr_error_program_plan_argument"
  )
  expect_error(
    rotation_program(
      cohorts = list(startup = startup),
      schedule = schedule,
      through = 2
    ),
    class = "samplyr_error_program_plan_cohorts"
  )

  short_intake <- sampling_design() |>
    draw(n = intake_n - 1L) |>
    execute(data.frame(id = 301:400), seed = 43)
  expect_error(
    rotation_program(
      list(startup = startup, intake_2 = short_intake),
      schedule = schedule,
      through = 2
    ),
    class = "samplyr_error_program_plan_count"
  )

  wrong_schedule <- data.frame(
    panel = rep(1:2, 2),
    wave = rep(1:2, each = 2),
    active = c(TRUE, TRUE, TRUE, FALSE)
  )
  wrong_startup <- sampling_design() |>
    draw(n = startup_n) |>
    execute(
      data.frame(id = 401:600),
      seed = 44,
      panels = wrong_schedule
    )
  expect_error(
    rotation_program(
      list(startup = wrong_startup), schedule = schedule, through = 1
    ),
    class = "samplyr_error_program_plan_panels"
  )

  future <- list(startup = startup, intake_2 = intake_2, intake_3 = intake_2)
  expect_error(
    rotation_program(future, schedule = schedule, through = 2),
    class = "samplyr_error_program_plan_cohorts"
  )
})

test_that("the planning route refuses permanent activation", {
  plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate"
  )
  schedule <- svyplan::design_schedule(
    plan, svyplan::design_rotation("4"), 4, "continuing",
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

  unsupported <- unclass(schedule)
  unsupported$schema_version <- 2L
  class(unsupported) <- c("svyplan_schedule", "list")
  expect_error(
    execute(design, data.frame(id = 1:200), panels = unsupported),
    class = "samplyr_error_svyplan_schedule"
  )

  pooled_frame <- data.frame(
    id = 1:80,
    region = rep(LETTERS[1:8], each = 10)
  )
  expect_error(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = data.frame(region = LETTERS[1:8], n = 3L)) |>
      execute(pooled_frame, panels = schedule),
    "r_min.*1",
    class = "samplyr_error_panel_small_pool"
  )
})

test_that("a gradual plan uses the same interface with one startup panel", {
  plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "gradual"
  )
  schedule <- svyplan::design_schedule(
    plan, svyplan::design_rotation("4"), 4, "continuing",
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
  expect_identical(names(execute(program, wave = 1)), "startup")
})

test_that("a continuing plan freezes startup activity beyond a short horizon", {
  plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate"
  )
  schedule <- svyplan::design_schedule(
    plan, svyplan::design_rotation("4"), 2, "continuing",
    refreshment = "entrant_register", rounding = "ceiling"
  )
  startup_n <- plan$n_cohorts * ceiling(plan$n_entrants)
  startup <- sampling_design() |>
    draw(n = startup_n) |>
    execute(data.frame(id = 1:200), seed = 61, panels = schedule)
  record <- attr(startup, "metadata")$panel_assignment

  expect_identical(record$r_min, 1L)
  expect_identical(record$block_size, 8L)
  expect_identical(max(record$schedule$wave), 4L)
})

test_that("whole-vintage refreshment does not gain a combined-weight route", {
  plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.25),
    retention = c(0.90, 0.95, 0.95),
    resp_rate = 0.80,
    design = "rotating",
    start = "immediate"
  )
  schedule <- svyplan::design_schedule(
    plan, svyplan::design_rotation("4"), 2, "continuing",
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
    list(startup = startup, intake_2 = intake_2),
    schedule = schedule,
    through = 2
  )

  expect_false(schedule$requires_disjoint_entrants)
  expect_error(
    as_svydesign(execute(program, wave = 2)),
    class = "samplyr_error_rotation_wave_not_combinable"
  )
})
