## Rotation programs: linking cohorts across frame vintages

# Two panels, each live for two of the three occasions. Every wave activates
# one panel, so r_min is 1 and blocks are 2K.
startup_schedule <- function() {
  data.frame(
    panel = rep(1:2, times = 3),
    wave = rep(1:3, each = 2),
    active = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
}

program_startup <- function(seed = 1) {
  sampling_design() |>
    draw(n = 40) |>
    execute(
      data.frame(id = 1:200, value = seq_len(200) / 200),
      seed = seed,
      panels = startup_schedule()
    )
}

# A refreshment cohort drawn whole from an entrant register: disjoint from
# the master by construction, and never partitioned.
program_intake <- function(seed = 2) {
  sampling_design() |>
    draw(n = 12) |>
    execute(
      data.frame(id = 201:260, value = seq_len(60) / 60),
      seed = seed
    )
}

program_schedule <- function() {
  rbind(
    transform(startup_schedule(), cohort = "startup"),
    data.frame(
      cohort = "intake_2",
      panel = 1L,
      wave = 1:3,
      active = c(FALSE, TRUE, TRUE)
    )
  )
}

two_cohort_program <- function() {
  rotation_program(
    cohorts = list(startup = program_startup(), intake_2 = program_intake()),
    entry_wave = c(startup = 1, intake_2 = 2),
    schedule = program_schedule()
  )
}

## Construction

test_that("a program records its cohorts, entries and completed schedule", {
  program <- two_cohort_program()

  expect_s3_class(program, "rotation_program")
  expect_identical(names(program$cohorts), c("startup", "intake_2"))
  expect_identical(program$entry_wave, c(startup = 1L, intake_2 = 2L))
  expect_identical(program$panels, c(startup = 2L, intake_2 = 1L))
  expect_identical(program$waves, 1:3)

  # Completed grid: 2 panels of startup plus 1 of intake, over 3 waves.
  expect_identical(nrow(program$schedule), 9L)
  expect_identical(
    names(program$schedule),
    c("wave", "cohort", "panel", "active")
  )
  expect_false(any(is.na(program$schedule$active)))
})

test_that("a one-cohort schedule may omit the cohort column", {
  startup <- program_startup()

  program <- rotation_program(
    cohorts = list(startup = startup),
    entry_wave = c(startup = 1),
    schedule = startup_schedule()
  )

  expect_identical(unique(program$schedule$cohort), "startup")
  expect_identical(nrow(program$schedule), 6L)
})

test_that("a schedule for several cohorts must name them", {
  expect_error(
    rotation_program(
      cohorts = list(a = program_startup(), b = program_intake()),
      entry_wave = c(a = 1, b = 2),
      schedule = startup_schedule()
    ),
    class = "samplyr_error_schedule_columns"
  )
})

test_that("non-contiguous activity is accepted", {
  # 2-out-2 in miniature: live, dormant, live again. A rotation such as the
  # 4-8-4 leaves and re-enters by design.
  schedule <- data.frame(
    panel = rep(1:2, times = 4),
    wave = rep(1:4, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  master <- sampling_design() |>
    draw(n = 40) |>
    execute(data.frame(id = 1:200, v = 1:200), seed = 3, panels = schedule)

  program <- rotation_program(
    cohorts = list(m = master),
    entry_wave = c(m = 1),
    schedule = schedule
  )

  live <- program$schedule[program$schedule$active, ]
  expect_identical(live$wave[live$panel == 1L], c(1L, 3L))
})

## Registry validation

test_that("cohorts must be complete, unmodified, named samples", {
  startup <- program_startup()

  expect_error(
    rotation_program(list(), c(a = 1), startup_schedule()),
    class = "samplyr_error_program_cohorts"
  )
  expect_error(
    rotation_program(list(startup), c(startup = 1), startup_schedule()),
    class = "samplyr_error_program_cohort_names"
  )
  expect_error(
    rotation_program(
      list(a = startup, a = startup),
      c(a = 1),
      startup_schedule()
    ),
    class = "samplyr_error_program_cohort_names"
  )
  expect_error(
    rotation_program(
      list(a = data.frame(x = 1)),
      c(a = 1),
      startup_schedule()
    ),
    class = "samplyr_error_program_cohorts"
  )

  modified <- startup
  modified$.weight <- modified$.weight * 2
  expect_error(
    rotation_program(list(a = modified), c(a = 1), startup_schedule()),
    class = "samplyr_error_modified_sample"
  )

  materialized <- execute(startup, wave = 1)
  expect_error(
    rotation_program(list(a = materialized), c(a = 1), startup_schedule()),
    class = "samplyr_error_program_cohort_is_wave"
  )

  design <- sampling_design() |>
    add_stage("Clusters") |>
      cluster_by(g) |>
      draw(n = 2) |>
    add_stage("Units") |>
      draw(n = 3)
  partial <- execute(
    design,
    data.frame(g = rep(1:10, each = 5), id = 1:50),
    stages = 1,
    seed = 1
  )
  expect_error(
    rotation_program(list(a = partial), c(a = 1), startup_schedule()),
    class = "samplyr_error_program_cohort_incomplete"
  )
})

test_that("entry waves are declared for every cohort and within range", {
  startup <- program_startup()

  expect_error(
    rotation_program(list(a = startup), c(a = 0), startup_schedule()),
    class = "samplyr_error_program_entry_values"
  )
  expect_error(
    rotation_program(list(a = startup), c(a = 1.5), startup_schedule()),
    class = "samplyr_error_program_entry_values"
  )
  expect_error(
    rotation_program(list(a = startup), c(b = 1), startup_schedule()),
    class = "samplyr_error_program_entry_names"
  )
  expect_error(
    rotation_program(list(a = startup), 1, startup_schedule()),
    class = "samplyr_error_program_entry_names"
  )
  expect_error(
    rotation_program(list(a = startup), c(a = 9), startup_schedule()),
    class = "samplyr_error_program_entry_range"
  )
})

## Schedule validation

test_that("schedule defects are refused by kind", {
  startup <- program_startup()
  one <- function(schedule, entry = c(startup = 1)) {
    rotation_program(
      cohorts = list(startup = startup),
      entry_wave = entry,
      schedule = schedule
    )
  }

  expect_error(
    one(data.frame(panel = 1:2)),
    class = "samplyr_error_schedule_columns"
  )
  expect_error(one(list(panel = 1)), class = "samplyr_error_schedule_columns")

  expect_error(
    one(data.frame(cohort = "nope", panel = 1, wave = 1)),
    class = "samplyr_error_program_schedule_cohort"
  )
  expect_error(
    one(data.frame(panel = c(1, 1), wave = c(1, 1))),
    class = "samplyr_error_schedule_duplicates"
  )
  expect_error(
    one(data.frame(panel = c(1, 1), wave = c(1, 3))),
    class = "samplyr_error_schedule_gap"
  )
  expect_error(
    one(data.frame(panel = c(1, 5), wave = c(1, 1))),
    class = "samplyr_error_program_schedule_panel"
  )
  expect_error(
    one(data.frame(panel = 1, wave = 1, active = "yes")),
    class = "samplyr_error_schedule_active"
  )
  expect_error(
    one(data.frame(panel = 1:2, wave = c(1, 1), active = c(FALSE, FALSE))),
    class = "samplyr_error_schedule_idle_wave"
  )
})

test_that("a cohort may not be active before it enters", {
  expect_error(
    rotation_program(
      cohorts = list(startup = program_startup(), intake_2 = program_intake()),
      entry_wave = c(startup = 1, intake_2 = 3),
      schedule = program_schedule()
    ),
    class = "samplyr_error_program_activation_before_entry"
  )
})

test_that("a registered cohort that is never active is a mistake", {
  schedule <- program_schedule()
  schedule$active[schedule$cohort == "intake_2"] <- FALSE

  expect_error(
    rotation_program(
      cohorts = list(startup = program_startup(), intake_2 = program_intake()),
      entry_wave = c(startup = 1, intake_2 = 2),
      schedule = schedule
    ),
    class = "samplyr_error_program_idle_cohort"
  )
})

test_that("a schedule may not activate fewer panels than the blocks allow", {
  # Drawn with two of four panels live at every wave, so r_min is 2 and the
  # block size collapsed to K. A program that then runs a single panel
  # leaves under two units per block.
  lean <- data.frame(
    panel = rep(1:4, times = 2),
    wave = rep(1:2, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  master <- sampling_design() |>
    draw(n = 80) |>
    execute(data.frame(id = 1:400, v = 1:400), seed = 4, panels = lean)

  record <- attr(master, "metadata")$panel_assignment
  expect_identical(record$r_min, 2L)
  expect_identical(record$block_size, 4L)

  thinner <- lean
  thinner$active <- c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)

  expect_error(
    rotation_program(list(m = master), c(m = 1), thinner),
    class = "samplyr_error_program_block_size"
  )

  # The schedule it was drawn with is of course acceptable.
  expect_s3_class(
    rotation_program(list(m = master), c(m = 1), lean),
    "rotation_program"
  )
})

## Materializing a wave

test_that("a wave holds only the live cohorts, each with its own receipt", {
  program <- two_cohort_program()

  wave_1 <- execute(program, wave = 1)
  expect_identical(names(wave_1), "startup")

  wave_2 <- execute(program, wave = 2)
  expect_identical(names(wave_2), c("startup", "intake_2"))
  expect_s3_class(wave_2, "rotation_wave")
  expect_identical(attr(wave_2, "wave"), 2L)

  for (nm in names(wave_2)) {
    part <- wave_2[[nm]]
    expect_s3_class(part, "tbl_sample")
    expect_true(sample_realization_status(part)$ok)

    record <- attr(part, "metadata")$wave
    expect_identical(record$wave, 2L)
    expect_identical(record$cohort, nm)
    expect_identical(record$entry_wave, unname(program$entry_wave[[nm]]))
  }
})

test_that("a partitioned cohort carries its exact activation factor", {
  program <- two_cohort_program()
  wave_2 <- execute(program, wave = 2)

  master <- program$cohorts$startup
  live <- wave_2$startup

  # One of two panels, blocks of 2K = 4 with a quota of two each: 2/4.
  factor <- live$.weight /
    master$.weight[match(live$.sample_id, master$.sample_id)]
  expect_identical(unique(factor), 2)
  expect_setequal(unique(live$.panel), 1L)
})

test_that("a whole cohort activates entire, at factor one", {
  program <- two_cohort_program()
  wave_2 <- execute(program, wave = 2)

  intake <- program$cohorts$intake_2
  live <- wave_2$intake_2

  expect_identical(nrow(live), nrow(intake))
  expect_identical(live$.weight, intake$.weight)
  expect_identical(attr(live, "metadata")$wave$active_panels, 1L)
  # Never partitioned, so it never gained a panel column.
  expect_false(".panel" %in% names(live))
})

test_that("the wave route refuses other execution input on a program", {
  program <- two_cohort_program()

  expect_error(
    execute(program),
    class = "samplyr_error_program_wave_required"
  )
  expect_error(
    execute(program, wave = 2, seed = 1),
    class = "samplyr_error_wave_extra_arguments"
  )
  expect_error(
    execute(program, data.frame(id = 1), wave = 2),
    class = "samplyr_error_wave_extra_arguments"
  )
  expect_error(
    execute(program, wave = 9),
    class = "samplyr_error_wave_undeclared"
  )
})

## The collection is not a sample

test_that("row-binding a wave yields a plain data frame it cannot export", {
  wave_2 <- execute(two_cohort_program(), wave = 2)
  flat <- as.data.frame(wave_2)

  expect_identical(class(flat), "data.frame")
  expect_false(is_tbl_sample(flat))
  expect_identical(names(flat)[1], ".cohort")
  expect_setequal(unique(flat$.cohort), c("startup", "intake_2"))
  expect_identical(nrow(flat), sum(vapply(wave_2, nrow, integer(1))))
})

test_that("survey export refuses the collection by name", {
  wave_2 <- execute(two_cohort_program(), wave = 2)

  expect_error(
    as_svydesign(wave_2),
    class = "samplyr_error_rotation_wave_not_combinable"
  )
  expect_error(
    as_svrepdesign(wave_2),
    class = "samplyr_error_rotation_wave_not_combinable"
  )
})

test_that("programs and waves print", {
  program <- two_cohort_program()

  expect_output(print(program), "Rotation Program")
  expect_output(print(program), "startup")
  expect_output(print(execute(program, wave = 2)), "Rotation Wave 2")
  expect_error(print(program, nope = 1), class = "rlib_error_dots_nonempty")
})
