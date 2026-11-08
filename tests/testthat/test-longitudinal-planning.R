test_that("four-occasion overlap has the expected first-lag share", {
  overlap <- svyplan::design_overlap("4")

  expect_equal(overlap$overlap[[1L]], 0.75)
  expect_equal(overlap$shared, 3:1)
})

test_that("a rotating panel plan keeps entrant and in-sample counts distinct", {
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

test_that("panel planning counts hand off to a program of several selections", {
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

  intake_2 <- sampling_design() |>
    draw(n = intake_n) |>
    execute(data.frame(id = sprintf("A%03d", 1:100)), seed = 32)
  intake_3 <- sampling_design() |>
    draw(n = intake_n) |>
    execute(data.frame(id = sprintf("B%03d", 1:100)), seed = 33)

  program <- rotation_program(
    selections = list(
      startup = startup,
      period_2 = intake_2,
      period_3 = intake_3
    ),
    schedule = schedule,
    through = 3
  )
  occasion_2 <- execute(program, period = 2)
  occasion_3 <- execute(program, period = 3)

  expect_s3_class(program, "rotation_program")
  expect_s3_class(occasion_2, "rotation_occasion")
  expect_s3_class(occasion_3, "rotation_occasion")
  expect_identical(names(program$selections), c(
    "startup", "period_2", "period_3"
  ))
  expect_identical(program$entry_period,
                   c(startup = 1L, period_2 = 2L, period_3 = 3L))
  expect_equal(nrow(intake_2), intake_n)
  expect_equal(nrow(intake_3), intake_n)
  expect_identical(names(occasion_2), c("startup", "period_2"))
  expect_identical(names(occasion_3), c("startup", "period_2", "period_3"))
  for (cohort in occasion_3) {
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
    plan, 6, "continuing",
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
  selections <- list(startup = startup, period_2 = intake_2)

  expect_error(
    rotation_program(selections, schedule = schedule),
    class = "samplyr_error_program_through"
  )
  expect_error(
    rotation_program(
      selections,
      entry_period = c(startup = 1, period_2 = 2),
      schedule = schedule,
      through = 2
    ),
    class = "samplyr_error_program_plan_argument"
  )
  expect_error(
    rotation_program(
      selections,
      entry_period = c(startup = 1, period_2 = 2),
      schedule = data.frame(panel = 1, period = 1),
      through = 2
    ),
    class = "samplyr_error_program_plan_argument"
  )
  expect_error(
    rotation_program(
      selections = list(startup = startup),
      schedule = schedule,
      through = 2
    ),
    class = "samplyr_error_program_plan_selections"
  )

  short_intake <- sampling_design() |>
    draw(n = intake_n - 1L) |>
    execute(data.frame(id = 301:400), seed = 43)
  expect_error(
    rotation_program(
      list(startup = startup, period_2 = short_intake),
      schedule = schedule,
      through = 2
    ),
    class = "samplyr_error_program_plan_count"
  )

  wrong_schedule <- data.frame(
    panel = rep(1:2, 2),
    period = rep(1:2, each = 2),
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

  future <- list(startup = startup, period_2 = intake_2, period_3 = intake_2)
  expect_error(
    rotation_program(future, schedule = schedule, through = 2),
    class = "samplyr_error_program_plan_selections"
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

  without <- function(x, at) {
    x <- unclass(x)
    x[[at]] <- NULL
    structure(x, class = class(schedule))
  }
  expect_error(
    execute(
      design, data.frame(id = 1:200),
      panels = without(schedule, c("schedule", "selection"))
    ),
    "has no .*selection",
    class = "samplyr_error_svyplan_schedule"
  )
  unsupported <- without(schedule, c("panel_parameters", "block_width"))
  expect_error(
    execute(design, data.frame(id = 1:200), panels = unsupported),
    "panel_parameters.*has no .*block_width",
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
    plan, 2, "continuing",
    refreshment = "entrant_register", rounding = "ceiling"
  )
  startup_n <- plan$n_cohorts * ceiling(plan$n_entrants)
  startup <- sampling_design() |>
    draw(n = startup_n) |>
    execute(data.frame(id = 1:200), seed = 61, panels = schedule)
  record <- attr(startup, "metadata")$panel_assignment

  expect_identical(record$r_min, 1L)
  expect_identical(record$block_size, 8L)
  expect_identical(max(record$schedule$occasion), 4L)
})

test_that("whole-vintage refreshment does not gain a combined-weight route", {
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
})

test_that("a plan with a gap in its rotation runs with its planned overlap", {
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
  # a gapped startup holds a panel for every stage of the life, so it draws
  # more than the cohorts in sample
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

  # the schedule's wave is the place in the rotation pattern; with no
  # subrounds the draw occasion is the period
  planned <- tapply(schedule$schedule$active, schedule$schedule$period, sum)
  expect_equal(lengths(ids), as.vector(planned) * intake_n)
  realized <- vapply(1:5, function(lag) {
    length(intersect(ids[[1L]], ids[[1L + lag]])) / length(ids[[1L]])
  }, numeric(1))
  expect_equal(realized, as.double(svyplan::design_overlap("2-2-2")))
})

test_that("a plan with subrounds registers occasions by subround", {
  plan <- svyplan::n_panel(
    svyplan::n_prop(p = 0.5, moe = 0.1),
    retention = c(0.9, 0.9, 0.9),
    resp_rate = 0.8,
    design = "rotating",
    start = "immediate",
    rotation = "2-2-2"
  )
  schedule <- svyplan::design_schedule(
    plan, 4, "continuing",
    refreshment = "entrant_register", rounding = "ceiling",
    take = 6, subrounds = 3
  )
  issue <- schedule$selections$operational_issue
  expect_identical(schedule$selections$panels, c(18L, 3L, 3L, 3L))

  startup <- sampling_design() |>
    draw(n = issue[[1L]]) |>
    execute(data.frame(id = 1:200), seed = 81, panels = schedule)
  entrants <- data.frame(id = 201:300)
  period_2 <- sampling_design() |>
    draw(n = issue[[2L]]) |>
    execute(entrants, seed = 82, panels = 3)

  # Period 2 opens at its first subround, after period 1.
  expect_error(
    rotation_program(
      list(startup = startup, period_2 = period_2),
      schedule = schedule, through = 1
    ),
    class = "samplyr_error_program_plan_selections"
  )
  program <- rotation_program(
    list(startup = startup, period_2 = period_2),
    schedule = schedule, through = c(period = 2, subround = 1)
  )
  expect_identical(program$entry_period, c(startup = 1L, period_2 = 2L))
  expect_identical(
    program$occasions,
    data.frame(period = c(1L, 1L, 1L, 2L), subround = c(1:3, 1L))
  )

  # One panel of the selection per subround, in the plan's order.
  tab <- schedule$schedule
  live <- tab[tab$selection == "period_2" & tab$active & tab$period == 2, ]
  expect_identical(live$subround[order(live$panel)], 1:3)
  occasion_4 <- execute(program, period = 2, subround = 1)
  expect_identical(
    attr(occasion_4$period_2, "metadata")$occasion$active_panels,
    1L
  )
  expect_identical(
    sort(occasion_4$period_2$id),
    sort(period_2$id[period_2$.panel == 1L])
  )

  whole <- sampling_design() |>
    draw(n = issue[[2L]]) |>
    execute(entrants, seed = 82)
  expect_error(
    rotation_program(
      list(startup = startup, period_2 = whole),
      schedule = schedule, through = c(period = 2, subround = 1)
    ),
    class = "samplyr_error_program_plan_panels"
  )
  expect_error(
    rotation_program(list(startup = startup), schedule = schedule,
                     through = 5),
    "from 1 to 4",
    class = "samplyr_error_program_through"
  )
})
