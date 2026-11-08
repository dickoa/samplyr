## Rotation programs: linking selections across frame vintages

# Two panels, each live for two of the three occasions. Every occasion activates
# one panel, so r_min is 1 and blocks are 2K.
startup_schedule <- function() {
  data.frame(
    panel = rep(1:2, times = 3),
    period = rep(1:3, each = 2),
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

# A refreshment selection drawn whole from an entrant register: disjoint from
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
    transform(startup_schedule(), selection = "startup"),
    data.frame(
      selection = "intake_2",
      panel = 1L,
      period = 1:3,
      active = c(FALSE, TRUE, TRUE)
    )
  )
}

two_selection_program <- function() {
  rotation_program(
    selections = list(startup = program_startup(), intake_2 = program_intake()),
    entry_period = c(startup = 1, intake_2 = 2),
    schedule = program_schedule()
  )
}

## Construction

test_that("a program records its selections, entries and completed schedule", {
  program <- two_selection_program()

  expect_s3_class(program, "rotation_program")
  expect_identical(names(program$selections), c("startup", "intake_2"))
  expect_identical(program$entry_period, c(startup = 1L, intake_2 = 2L))
  expect_identical(program$panels, c(startup = 2L, intake_2 = 1L))
  expect_identical(
    program$occasions,
    data.frame(period = 1:3, subround = rep(1L, 3))
  )

  # Completed grid: 2 panels of startup plus 1 of period_2, over 3 periods.
  expect_identical(nrow(program$schedule), 9L)
  expect_identical(
    names(program$schedule),
    c("occasion", "selection", "panel", "period", "subround", "active")
  )
  expect_false(any(is.na(program$schedule$active)))
})

test_that("a one-selection schedule may omit the selection column", {
  startup <- program_startup()

  program <- rotation_program(
    selections = list(startup = startup),
    entry_period = c(startup = 1),
    schedule = startup_schedule()
  )

  expect_identical(unique(program$schedule$selection), "startup")
  expect_identical(nrow(program$schedule), 6L)
})

test_that("a schedule for several selections must name them", {
  expect_error(
    rotation_program(
      selections = list(a = program_startup(), b = program_intake()),
      entry_period = c(a = 1, b = 2),
      schedule = startup_schedule()
    ),
    class = "samplyr_error_schedule_columns"
  )
})

test_that("non-contiguous activity is accepted", {
  # Live, dormant, live again, as a 4-8-4 rotation does by design.
  schedule <- data.frame(
    panel = rep(1:2, times = 4),
    period = rep(1:4, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  master <- sampling_design() |>
    draw(n = 40) |>
    execute(data.frame(id = 1:200, v = 1:200), seed = 3, panels = schedule)

  program <- rotation_program(
    selections = list(m = master),
    entry_period = c(m = 1),
    schedule = schedule
  )

  live <- program$schedule[program$schedule$active, ]
  expect_identical(live$occasion[live$panel == 1L], c(1L, 3L))
})

## Registry validation

test_that("selections must be complete, unmodified, named samples", {
  startup <- program_startup()

  expect_error(
    rotation_program(list(), c(a = 1), startup_schedule()),
    class = "samplyr_error_program_selections"
  )
  expect_error(
    rotation_program(list(startup), c(startup = 1), startup_schedule()),
    class = "samplyr_error_program_selection_names"
  )
  expect_error(
    rotation_program(
      list(a = startup, a = startup),
      c(a = 1),
      startup_schedule()
    ),
    class = "samplyr_error_program_selection_names"
  )
  expect_error(
    rotation_program(
      list(a = data.frame(x = 1)),
      c(a = 1),
      startup_schedule()
    ),
    class = "samplyr_error_program_selections"
  )

  modified <- startup
  modified$.weight <- modified$.weight * 2
  expect_error(
    rotation_program(list(a = modified), c(a = 1), startup_schedule()),
    class = "samplyr_error_modified_sample"
  )

  materialized <- execute(startup, period = 1)
  expect_error(
    rotation_program(list(a = materialized), c(a = 1), startup_schedule()),
    class = "samplyr_error_program_selection_is_occasion"
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
    class = "samplyr_error_program_selection_incomplete"
  )
})

test_that("entry periods are declared for every selection and within range", {
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

test_that("rotation program schedules are refused by defect kind", {
  startup <- program_startup()
  one <- function(schedule, entry = c(startup = 1)) {
    rotation_program(
      selections = list(startup = startup),
      entry_period = entry,
      schedule = schedule
    )
  }

  expect_error(
    one(data.frame(panel = 1:2)),
    class = "samplyr_error_schedule_columns"
  )
  expect_error(one(list(panel = 1)), class = "samplyr_error_schedule_columns")

  expect_error(
    one(data.frame(selection = "nope", panel = 1, period = 1)),
    class = "samplyr_error_program_schedule_selection"
  )
  expect_error(
    one(data.frame(panel = c(1, 1), period = c(1, 1))),
    class = "samplyr_error_schedule_duplicates"
  )
  expect_error(
    one(data.frame(panel = c(1, 1), period = c(1, 3))),
    class = "samplyr_error_schedule_gap"
  )
  expect_error(
    one(data.frame(panel = c(1, 5), period = c(1, 1))),
    class = "samplyr_error_program_schedule_panel"
  )
  expect_error(
    one(data.frame(panel = 1, period = 1, active = "yes")),
    class = "samplyr_error_schedule_active"
  )
  expect_error(
    one(data.frame(panel = 1:2, period = c(1, 1), active = c(FALSE, FALSE))),
    class = "samplyr_error_schedule_idle_occasion"
  )
})

test_that("a selection may not be active before it enters", {
  expect_error(
    rotation_program(
      selections = list(
        startup = program_startup(), intake_2 = program_intake()
      ),
      entry_period = c(startup = 1, intake_2 = 3),
      schedule = program_schedule()
    ),
    class = "samplyr_error_program_activation_before_entry"
  )
})

test_that("a registered selection that is never active is a mistake", {
  schedule <- program_schedule()
  schedule$active[schedule$selection == "intake_2"] <- FALSE

  expect_error(
    rotation_program(
      selections = list(
        startup = program_startup(), intake_2 = program_intake()
      ),
      entry_period = c(startup = 1, intake_2 = 2),
      schedule = schedule
    ),
    class = "samplyr_error_program_idle_selection"
  )
})

test_that("a schedule may not activate fewer panels than the blocks allow", {
  # Two of four panels live at every occasion give r_min = 2 and blocks of K.
  lean <- data.frame(
    panel = rep(1:4, times = 2),
    period = rep(1:2, each = 4),
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

## Materializing an occasion

test_that("an occasion holds only the live selections, each with its own receipt", {
  program <- two_selection_program()

  occasion_1 <- execute(program, period = 1)
  expect_identical(names(occasion_1), "startup")

  occasion_2 <- execute(program, period = 2)
  expect_identical(names(occasion_2), c("startup", "intake_2"))
  expect_s3_class(occasion_2, "rotation_occasion")
  expect_identical(attr(occasion_2, "period"), 2L)
  expect_identical(attr(occasion_2, "subround"), 1L)

  for (nm in names(occasion_2)) {
    part <- occasion_2[[nm]]
    expect_s3_class(part, "tbl_sample")
    expect_true(sample_realization_status(part)$ok)

    record <- attr(part, "metadata")$occasion
    expect_identical(record$occasion, 2L)
    expect_identical(record$selection, nm)
    expect_identical(record$entry_period, unname(program$entry_period[[nm]]))
  }
})

test_that("a partitioned selection carries its exact activation factor", {
  program <- two_selection_program()
  occasion_2 <- execute(program, period = 2)

  master <- program$selections$startup
  live <- occasion_2$startup

  # One of two panels, blocks of 2K = 4 with a quota of two each: 2/4.
  factor <- live$.weight /
    master$.weight[match(live$.sample_id, master$.sample_id)]
  expect_identical(unique(factor), 2)
  expect_setequal(unique(live$.panel), 1L)
})

test_that("a whole selection activates entire, at factor one", {
  program <- two_selection_program()
  occasion_2 <- execute(program, period = 2)

  intake <- program$selections$intake_2
  live <- occasion_2$intake_2

  expect_identical(nrow(live), nrow(intake))
  expect_identical(live$.weight, intake$.weight)
  expect_identical(attr(live, "metadata")$occasion$active_panels, 1L)
  # Never partitioned, so it never gained a panel column.
  expect_false(".panel" %in% names(live))
})

test_that("the occasion route refuses other execution input on a program", {
  program <- two_selection_program()

  expect_error(
    execute(program),
    class = "samplyr_error_program_occasion_required"
  )
  expect_error(
    execute(program, period = 2, seed = 1),
    class = "samplyr_error_occasion_extra_arguments"
  )
  expect_error(
    execute(program, data.frame(id = 1), period = 2),
    class = "samplyr_error_occasion_extra_arguments"
  )
  expect_error(
    execute(program, period = 2, frame_digest = "full"),
    class = "samplyr_error_occasion_extra_arguments"
  )
  expect_error(
    execute(program, period = 9),
    class = "samplyr_error_occasion_undeclared"
  )

  expect_error(
    execute(program, period = 2, seed = 1),
    regexp = "Every input an occasion needs is stored with the program"
  )
  expect_error(
    execute(program, period = 2, seed = 1),
    regexp = "components the program already assigned"
  )
})

## The collection is not a sample

test_that("row-binding an occasion yields a plain data frame it cannot export", {
  occasion_2 <- execute(two_selection_program(), period = 2)
  flat <- as.data.frame(occasion_2)

  expect_identical(class(flat), "data.frame")
  expect_false(is_tbl_sample(flat))
  expect_identical(names(flat)[1], ".selection")
  expect_setequal(unique(flat$.selection), c("startup", "intake_2"))
  expect_identical(nrow(flat), sum(vapply(occasion_2, nrow, integer(1))))
})

test_that("survey export refuses the collection by name", {
  skip_if_not_installed("survey")
  occasion_2 <- execute(two_selection_program(), period = 2)

  expect_error(
    as_svydesign(occasion_2),
    class = "samplyr_error_rotation_occasion_not_combinable"
  )
  expect_error(
    as_svrepdesign(occasion_2),
    class = "samplyr_error_rotation_occasion_not_combinable"
  )
})

test_that("programs and occasions print", {
  program <- two_selection_program()

  expect_output(print(program), "Rotation Program")
  expect_output(print(program), "startup")
  expect_output(print(execute(program, period = 2)),
                "Rotation Occasion: period 2")
  expect_error(print(program, nope = 1), class = "rlib_error_dots_nonempty")
})

## Issue counts and the plan figure they are compared against

test_that("a whole selection is counted in the units the plan means", {
  # svyplan's `operational_issue` counts units issued, not the rows they span.
  population <- data.frame(
    psu = rep(sprintf("p%02d", 1:40), each = 10),
    hh = sprintf("h%04d", 1:400),
    stringsAsFactors = FALSE
  )
  whole <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 8) |>
    execute(population, seed = 5)

  expect_identical(nrow(whole), 80L)
  expect_identical(attr(whole, "metadata")$n_selected, 80L)
  expect_identical(selection_issue_count(whole), 8)

  # The same design with panels reaches the record branch, which agrees.
  panelled <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 8) |>
    execute(
      population,
      seed = 5,
      panels = data.frame(
        panel = rep(1:2, times = 2), period = rep(1:2, each = 2),
        active = c(TRUE, FALSE, FALSE, TRUE)
      )
    )
  expect_identical(
    attr(panelled, "metadata")$panel_assignment$unit, "cluster"
  )
  expect_identical(
    selection_issue_count(panelled), selection_issue_count(whole)
  )

  # Unclustered, a row is a unit.
  flat <- sampling_design() |> draw(n = 12) |> execute(data.frame(id = 1:200), seed = 2)
  expect_identical(selection_issue_count(flat), 12)
})

test_that("the rotation-occasion export names a route that works", {
  program <- two_selection_program()
  occasion <- execute(program, period = 2)

  expect_error(
    as_svydesign(occasion),
    class = "samplyr_error_rotation_occasion_not_combinable"
  )
  # The component route the message recommends exports the selection's
  # occasion.
  expect_error(as_svydesign(occasion), regexp = "selection's occasion")
  expect_no_match(
    conditionMessage(tryCatch(as_svydesign(occasion), error = identity)),
    "once the activation phase is supported",
    fixed = TRUE
  )
  skip_if_not_installed("survey")
  expect_s3_class(as_svydesign(occasion[["startup"]]), "survey.design2")
})

test_that("a selection drawn whole leaves the occasion when its panel is idle", {
  # A lone idle selection empties the occasion and is refused, so a second is
  # needed.
  frame <- data.frame(id = sprintf("U%03d", 1:120))
  master_sched <- data.frame(
    panel = rep(1:2, 3), period = rep(1:3, each = 2), active = rep(TRUE, 6)
  )
  partitioned <- execute(
    sampling_design() |> draw(n = 40), frame, seed = 21, panels = master_sched
  )
  solo <- execute(
    sampling_design() |> draw(n = 20),
    data.frame(id = sprintf("B%03d", 1:60)), seed = 22
  )
  expect_null(attr(solo, "metadata")$panel_assignment)

  schedule <- data.frame(
    selection = c(rep("main", 6), rep("solo", 3)),
    panel = c(rep(1:2, 3), rep(1L, 3)),
    period = c(rep(1:3, each = 2), 1:3),
    active = c(rep(TRUE, 6), TRUE, FALSE, TRUE)
  )
  program <- rotation_program(
    selections = list(main = partitioned, solo = solo),
    entry_period = c(main = 1L, solo = 1L),
    schedule = schedule
  )

  rows <- function(occasion, selection) {
    if (!selection %in% names(occasion)) return(0L)
    nrow(as.data.frame(occasion[[selection]]))
  }

  # Named selections, not a row count, so 40 rows cannot pass by chance.
  expect_setequal(names(execute(program, period = 1)), c("main", "solo"))
  expect_setequal(names(execute(program, period = 2)), "main")
  expect_setequal(names(execute(program, period = 3)), c("main", "solo"))

  expect_identical(rows(execute(program, period = 2), "solo"), 0L)
  expect_identical(rows(execute(program, period = 1), "solo"), 20L)

  # Activating a whole selection is not a subsample, so weights are unscaled.
  live <- as.data.frame(execute(program, period = 1)[["solo"]])
  expect_equal(live$.weight, as.data.frame(solo)$.weight[match(live$id, solo$id)])
})
