## Fixtures

period_frame <- function() {
  data.frame(
    id = 1:600,
    h = rep(c("a", "b", "c"), each = 200),
    y = rep(c(10, 20, 30), each = 200) + (1:600) %% 7
  )
}

# Four groups of three monthly panels over four quarters. Panel
# (g - 1) * 3 + j is interviewed in month j of the quarters its group is in,
# and each quarter interviews two groups.
rotating_months <- function(period = TRUE) {
  in_quarter <- list(c(1, 4), c(1, 2), c(2, 3), c(3, 4))
  schedule <- expand.grid(panel = 1:12, wave = 1:12)
  group <- ceiling(schedule$panel / 3)
  month <- (schedule$panel - 1) %% 3 + 1
  quarter <- ceiling(schedule$wave / 3)
  schedule$active <- month == (schedule$wave - 1) %% 3 + 1 &
    mapply(function(g, q) g %in% in_quarter[[q]], group, quarter)
  if (period) {
    schedule$period <- quarter
  }
  schedule
}

# Fifteen monthly panels kept in every quarter, so the schedule cannot tell
# groups apart and every quarter reaches every panel.
retained_months <- function() {
  schedule <- expand.grid(panel = 1:15, wave = 1:12)
  schedule$active <- (schedule$wave - 1) %% 3 == (schedule$panel - 1) %% 3
  schedule$period <- ceiling(schedule$wave / 3)
  schedule
}

period_master <- function(n, schedule = rotating_months(), seed = 1) {
  sampling_design() |>
    stratify_by(h) |>
    draw(n = n) |>
    execute(period_frame(), seed = seed, panels = schedule)
}

monthly_plan <- function(subperiods = 3, tenures = 1,
                         policy = "continuing") {
  svyplan::design_schedule(
    svyplan::n_panel(
      svyplan::n_prop(p = 0.5, moe = 0.02, deff = 1.8),
      retention = rep(0.9, 4),
      resp_rate = 0.8,
      design = "rotating",
      start = "immediate"
    ),
    horizon = 4,
    horizon_policy = policy,
    refreshment = "entrant_register",
    rounding = "ceiling",
    take = 14,
    tenures = tenures,
    subperiods = subperiods
  )
}

## Reading the period column

test_that("a period column gives rotation groups and sub-periods", {
  spec <- samplyr:::normalize_panel_input(rotating_months())

  expect_identical(spec$reporting$group, rep(1:4, each = 3))
  expect_identical(spec$reporting$subperiod, rep(1:3, times = 4))
  expect_identical(spec$reporting$groups, 4L)
  expect_identical(spec$reporting$threshold, 2L)
  expect_identical(spec$r_min, 2L)
  expect_identical(spec$block_size, 12L)
})

test_that("panels the schedule cannot tell apart form one group", {
  spec <- samplyr:::normalize_panel_input(retained_months())

  expect_identical(spec$reporting$group, rep(1L, 15))
  expect_identical(spec$reporting$subperiod, rep(1:3, times = 5))
  expect_identical(spec$reporting$threshold, 0L)
})

test_that("a period equal to the wave assigns as no period", {
  schedule <- rotating_months(period = FALSE)
  same <- schedule
  same$period <- same$wave

  plain <- period_master(n = 12, schedule = schedule)
  stated <- period_master(n = 12, schedule = same)

  expect_null(samplyr:::normalize_panel_input(same)$reporting)
  expect_identical(stated$.panel, plain$.panel)
  record <- attr(stated, "metadata")$panel_assignment
  expect_identical(record$version, 3L)
  expect_false("period" %in% names(record$schedule))
})

test_that("a period column that does not group waves is refused", {
  split_wave <- rotating_months()
  split_wave$period[split_wave$wave == 2 & split_wave$panel == 5] <- 2
  backwards <- rotating_months()
  backwards$period <- 5 - backwards$period
  gap <- rotating_months()
  gap$period <- ifelse(gap$period > 2, gap$period + 1, gap$period)
  fractional <- rotating_months()
  fractional$period <- fractional$wave / 3

  expect_error(
    samplyr:::normalize_panel_input(split_wave),
    class = "samplyr_error_schedule_period"
  )
  expect_error(
    samplyr:::normalize_panel_input(backwards),
    class = "samplyr_error_schedule_period"
  )
  expect_error(
    samplyr:::normalize_panel_input(gap),
    class = "samplyr_error_schedule_gap"
  )
  expect_error(
    samplyr:::normalize_panel_input(fractional),
    class = "samplyr_error_schedule_values"
  )
})

## Pools and labels

test_that("a pool needs a unit in each period, not at each wave", {
  expect_error(
    period_master(n = 3, schedule = rotating_months(period = FALSE)),
    class = "samplyr_error_panel_small_pool"
  )
  expect_error(
    period_master(n = 2),
    class = "samplyr_error_panel_small_pool"
  )

  master <- period_master(n = 3)
  record <- attr(master, "metadata")$panel_assignment
  expect_identical(record$version, 6L)
  expect_identical(
    record$schedule$period,
    as.integer(ceiling(record$schedule$wave / 3))
  )
})

test_that("labels cover the groups before the sub-periods", {
  reporting <- samplyr:::normalize_panel_input(rotating_months())$reporting

  set.seed(11)
  covered <- vapply(seq_len(300), function(i) {
    panel <- samplyr:::assign_blocked_panels(4L, 12L, 12L, reporting)$panel
    c(
      groups = length(unique(reporting$group[panel])),
      months = length(unique(reporting$subperiod[panel]))
    )
  }, integer(2))

  expect_identical(unique(covered["groups", ]), 4L)
  expect_identical(unique(covered["months", ]), 3L)
})

test_that("the group-first order is a permutation of the panels", {
  set.seed(5)
  perm <- sample.int(12)
  out <- samplyr:::spread_panel_order(
    perm, rep(1:4, each = 3), rep(1:3, times = 4)
  )

  expect_identical(sort(out), 1:12)
  expect_identical(sort(ceiling(out[1:4] / 3)), c(1, 2, 3, 4))
})

## Waves and windows

test_that("every period reaches every pool and adds up to its size", {
  master <- period_master(n = 3)
  for (q in 1:4) {
    window <- execute(master, wave = (q - 1) * 3 + 1:3)
    df <- as.data.frame(window)
    expect_identical(sort(unique(df$h)), c("a", "b", "c"))
    expect_equal(
      as.vector(tapply(df$.weight, df$h, sum)),
      c(200, 200, 200)
    )
  }
})

test_that("a month without a unit of a pool is refused for its window", {
  master <- suppressMessages(
    period_master(n = 1, schedule = retained_months())
  )
  df <- as.data.frame(master)
  month <- (df$.panel - 1) %% 3 + 1

  refused <- vapply(1:3, function(t) {
    !all(c("a", "b", "c") %in% df$h[month == t])
  }, logical(1))
  expect_gte(sum(refused), 2L)
  for (t in which(refused)) {
    expect_error(
      execute(master, wave = t),
      "Materialize the period as a window",
      class = "samplyr_error_panel_small_pool"
    )
  }

  window <- as.data.frame(execute(master, wave = 1:3))
  expect_identical(sort(window$id), sort(df$id))
  expect_equal(as.vector(tapply(window$.weight, window$h, sum)), rep(200, 3))
})

## Records and replay

test_that("a master with periods replays from its design file", {
  master <- period_master(n = 3)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(master, path, frame = period_frame())

  receipt <- jsonlite::read_json(path)$execution$panel_assignment
  expect_identical(receipt$version, 6L)
  expect_identical(
    vapply(receipt$schedule, function(row) row$period, integer(1)),
    as.integer(ceiling(rotating_months()$wave[
      order(rotating_months()$wave, rotating_months()$panel)
    ] / 3))
  )

  replayed <- replay_design(read_design(path), period_frame())
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(
    attr(replayed, "metadata")$panel_assignment$schedule$period,
    attr(master, "metadata")$panel_assignment$schedule$period
  )
})

test_that("a version-6 record without periods is refused", {
  record <- attr(period_master(n = 3), "metadata")$panel_assignment
  record$schedule$period <- NULL

  expect_error(
    samplyr:::prepare_panel_record(record, "A wave"),
    class = "samplyr_error_panel_record_malformed"
  )
})

## Planning schedules

test_that("an svyplan schedule with sub-periods checks each period", {
  plan <- monthly_plan()
  spec <- samplyr:::normalize_panel_input(plan)

  expect_identical(spec$k, 15L)
  expect_identical(spec$reporting$group, rep(1:5, each = 3))
  expect_identical(spec$reporting$threshold, 4L)
  expect_identical(spec$k - spec$r_min, 14L)

  design <- function(n) {
    sampling_design() |>
      stratify_by(h) |>
      draw(n = n)
  }
  expect_error(
    execute(design(4), period_frame(), seed = 2, panels = plan),
    class = "samplyr_error_panel_small_pool"
  )
  master <- execute(design(5), period_frame(), seed = 2, panels = plan)
  expect_identical(attr(master, "metadata")$panel_assignment$version, 6L)
})

test_that("kept primary units are told apart by their visits", {
  # Every group is active every quarter, and only tenure timing differs.
  # The first two groups differ only after the horizon, so they are one.
  plan <- monthly_plan(tenures = "open", policy = "truncate_lives")
  spec <- samplyr:::normalize_panel_input(plan)

  expect_identical(spec$reporting$group, rep(c(1L, 1L, 2L, 3L, 4L), each = 3))
  expect_identical(spec$reporting$threshold, 0L)
})

test_that("an svyplan schedule without sub-periods keeps its labels", {
  expect_null(samplyr:::normalize_panel_input(monthly_plan(1))$reporting)
})
