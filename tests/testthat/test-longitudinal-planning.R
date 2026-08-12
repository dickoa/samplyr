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
  startup_n <- ceiling(plan$n_in_sample)
  intake_n <- ceiling(plan$n_entrants)

  startup_schedule <- expand.grid(panel = 1:4, wave = 1:4)
  startup_schedule$active <- with(
    startup_schedule,
    wave <= 5L - panel
  )
  startup <- sampling_design() |>
    draw(n = startup_n) |>
    execute(
      data.frame(id = sprintf("S%03d", 1:200)),
      seed = 31,
      panels = startup_schedule
    )

  intake_2 <- sampling_design() |>
    draw(n = intake_n) |>
    execute(data.frame(id = sprintf("A%03d", 1:100)), seed = 32)
  intake_3 <- sampling_design() |>
    draw(n = intake_n) |>
    execute(data.frame(id = sprintf("B%03d", 1:100)), seed = 33)

  schedule <- expand.grid(
    wave = 1:6,
    cohort = c("startup", "intake_2", "intake_3"),
    panel = 1:4,
    stringsAsFactors = FALSE
  )
  schedule <- schedule[
    schedule$cohort == "startup" | schedule$panel == 1L,
  ]
  schedule$active <- with(
    schedule,
    (cohort == "startup" & wave <= 5L - panel) |
      (cohort == "intake_2" & wave >= 2L & wave <= 5L) |
      (cohort == "intake_3" & wave >= 3L & wave <= 6L)
  )

  program <- rotation_program(
    cohorts = list(
      startup = startup,
      intake_2 = intake_2,
      intake_3 = intake_3
    ),
    entry_wave = c(startup = 1, intake_2 = 2, intake_3 = 3),
    schedule = schedule
  )
  wave_2 <- execute(program, wave = 2)
  wave_3 <- execute(program, wave = 3)

  expect_s3_class(program, "rotation_program")
  expect_s3_class(wave_2, "rotation_wave")
  expect_s3_class(wave_3, "rotation_wave")
  expect_identical(names(program$cohorts), c(
    "startup", "intake_2", "intake_3"
  ))
  expect_equal(nrow(intake_2), intake_n)
  expect_equal(nrow(intake_3), intake_n)
  expect_identical(names(wave_2), c("startup", "intake_2"))
  expect_identical(names(wave_3), c("startup", "intake_2", "intake_3"))
  for (cohort in wave_3) {
    expect_s3_class(cohort, "tbl_sample")
    expect_true(sample_realization_status(cohort)$ok)
  }
})
