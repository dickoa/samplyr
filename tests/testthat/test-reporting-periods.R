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
  schedule <- expand.grid(panel = 1:12, subround = 1:3, period = 1:4)
  group <- ceiling(schedule$panel / 3)
  month <- (schedule$panel - 1) %% 3 + 1
  schedule$active <- month == schedule$subround &
    mapply(function(g, q) g %in% in_quarter[[q]], group, schedule$period)
  if (!period) {
    schedule$period <- (schedule$period - 1L) * 3L + schedule$subround
    schedule$subround <- NULL
  }
  schedule
}

# Fifteen monthly panels kept in every quarter, so the schedule cannot tell
# groups apart and every quarter reaches every panel.
retained_months <- function() {
  schedule <- expand.grid(panel = 1:15, subround = 1:3, period = 1:4)
  schedule$active <- schedule$subround == (schedule$panel - 1) %% 3 + 1
  schedule
}

period_master <- function(n, schedule = rotating_months(), seed = 1) {
  sampling_design() |>
    stratify_by(h) |>
    draw(n = n) |>
    execute(period_frame(), seed = seed, panels = schedule)
}

monthly_plan <- function(subrounds = 3, retain_psus = FALSE,
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
    retain_psus = retain_psus,
    subrounds = subrounds
  )
}

## Reading the period column

test_that("a period column gives rotation groups and subrounds", {
  spec <- samplyr:::normalize_panel_input(rotating_months())

  expect_identical(spec$reporting$group, rep(1:4, each = 3))
  expect_identical(spec$reporting$subround, rep(1:3, times = 4))
  expect_identical(spec$reporting$groups, 4L)
  expect_identical(spec$reporting$threshold, 2L)
  expect_identical(spec$r_min, 2L)
  expect_identical(spec$block_size, 12L)
})

test_that("panels the schedule cannot tell apart form one group", {
  spec <- samplyr:::normalize_panel_input(retained_months())

  expect_identical(spec$reporting$group, rep(1L, 15))
  expect_identical(spec$reporting$subround, rep(1:3, times = 5))
  expect_identical(spec$reporting$threshold, 0L)
})

test_that("a schedule whose periods have one subround assigns as none", {
  schedule <- rotating_months(period = FALSE)
  same <- schedule
  same$subround <- 1L

  plain <- period_master(n = 12, schedule = schedule)
  stated <- period_master(n = 12, schedule = same)

  expect_null(samplyr:::normalize_panel_input(same)$reporting)
  expect_identical(stated$.panel, plain$.panel)
  record <- attr(stated, "metadata")$panel_assignment
  expect_identical(record$version, 7L)
  expect_identical(unique(record$schedule$subround), 1L)
})

test_that("periods and subrounds must number occasions without gaps", {
  gap <- rotating_months()
  gap$period <- ifelse(gap$period > 2, gap$period + 1, gap$period)
  sub_gap <- rotating_months()
  sub_gap$subround[sub_gap$period == 2 & sub_gap$subround == 2] <- 4L
  fractional <- rotating_months()
  fractional$subround <- fractional$subround / 2
  waves <- rotating_months(period = FALSE)
  names(waves)[names(waves) == "period"] <- "wave"
  months <- rotating_months()
  months$wave <- (months$period - 1L) * 3L + months$subround
  months$subround <- NULL

  expect_error(
    samplyr:::normalize_panel_input(gap),
    class = "samplyr_error_schedule_gap"
  )
  expect_error(
    samplyr:::normalize_panel_input(sub_gap),
    "Period 2 declares subrounds up to 4",
    class = "samplyr_error_schedule_gap"
  )
  expect_error(
    samplyr:::normalize_panel_input(fractional),
    class = "samplyr_error_schedule_values"
  )
  expect_error(
    samplyr:::normalize_panel_input(waves),
    "place of an interview in the rotation pattern",
    class = "samplyr_error_schedule_columns"
  )
  # A wave per month under a quarterly period repeats each panel's key.
  expect_error(
    samplyr:::normalize_panel_input(months),
    "several waves per period",
    class = "samplyr_error_schedule_duplicates"
  )
})

test_that("a table with a period ignores its wave", {
  schedule <- rotating_months()
  with_wave <- schedule
  with_wave$wave <- rev(seq_len(nrow(with_wave)))

  expect_identical(
    samplyr:::normalize_panel_input(with_wave)$schedule,
    samplyr:::normalize_panel_input(schedule)$schedule
  )
})

test_that("periods may hold different numbers of subrounds", {
  # Weekly fieldwork in quarters of 13 and 14 weeks. Panel j is interviewed
  # in week j of each quarter, so panel 14 only in the second.
  weeks <- rbind(
    expand.grid(panel = 1:14, subround = 1:13, period = 1L),
    expand.grid(panel = 1:14, subround = 1:14, period = 2L)
  )
  weeks$active <- weeks$panel == weeks$subround
  master <- period_master(n = 42, schedule = weeks)
  record <- attr(master, "metadata")$panel_assignment
  pairs <- samplyr:::grid_pairs(record$schedule)
  expect_identical(as.vector(table(pairs$period)), c(13L, 14L))

  second <- execute(master, period = 2)
  meta <- attr(second, "metadata")$occasion
  expect_identical(meta$occasion, 14:27)
  expect_identical(meta$subround, 1:14)
  rows <- as.data.frame(second)
  expect_equal(as.vector(tapply(rows$.weight, rows$h, sum)), rep(200, 3))

  expect_error(
    execute(master, period = 1, subround = 14),
    class = "samplyr_error_occasion_undeclared"
  )

  # Panels 1 to 13 return in the second quarter, so the window reads one
  # part per quarter, each by its share of the 27 weeks.
  both <- execute(master, period = 1:2)
  meta <- attr(both, "metadata")$occasion
  expect_identical(meta$window, "occasions")
  expect_identical(meta$blocks, list(1:13, 14:27))
  rows <- as.data.frame(both)
  expect_equal(
    tapply(rows$.weight, list(rows$h, rows$.period), sum),
    matrix(200 * c(13, 13, 13, 14, 14, 14) / 27, 3,
           dimnames = list(c("a", "b", "c"), c("1", "2")))
  )
})

## Pools and labels

test_that("a pool needs a unit in each period, not at each subround", {
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
  expect_identical(record$version, 7L)
  expect_identical(
    record$schedule$period,
    as.integer(ceiling(record$schedule$occasion / 3))
  )
  expect_identical(
    record$schedule$subround,
    as.integer((record$schedule$occasion - 1L) %% 3L + 1L)
  )
})

test_that("labels cover the groups before the subrounds", {
  reporting <- samplyr:::normalize_panel_input(rotating_months())$reporting

  set.seed(11)
  covered <- vapply(seq_len(300), function(i) {
    panel <- samplyr:::assign_blocked_panels(4L, 12L, 12L, reporting)$panel
    c(
      groups = length(unique(reporting$group[panel])),
      months = length(unique(reporting$subround[panel]))
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

## Occasions and windows

test_that("every period reaches every pool and adds up to its size", {
  master <- period_master(n = 3)
  for (q in 1:4) {
    window <- execute(master, period = q)
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
    err <- expect_error(
      execute(master, period = 1, subround = t),
      "Materialize the whole period",
      class = "samplyr_error_panel_small_pool"
    )
    expect_identical(deparse(conditionCall(err)[[1]]), "execute")
  }

  window <- as.data.frame(execute(master, period = 1))
  expect_identical(sort(window$id), sort(df$id))
  expect_equal(as.vector(tapply(window$.weight, window$h, sum)), rep(200, 3))
})

test_that("a window without a unit of a pool is refused as a window", {
  master <- period_master(n = 3, seed = 1)

  rolling <- expect_error(
    execute(master, period = c(1, 1), subround = c(2, 3)),
    class = "samplyr_error_panel_small_pool"
  )
  expect_match(
    cli::ansi_strip(conditionMessage(rolling)),
    paste("This window, period 1 subrounds 2 and 3, cannot be materialized:",
          "it holds no unit of pool"),
    fixed = TRUE
  )
  expect_match(conditionMessage(rolling), "declare reporting periods")
  expect_no_match(conditionMessage(rolling), "Materialize the whole period")

  partial <- expect_error(
    execute(master, period = c(1, 1, 2, 2, 2), subround = c(2, 3, 1:3)),
    class = "samplyr_error_panel_small_pool"
  )
  expect_match(
    cli::ansi_strip(conditionMessage(partial)),
    paste("This window, period 1 subrounds 2 and 3, and period 2, cannot be",
          "materialized: its part in period 1 subrounds 2 and 3 holds no",
          "unit of pool"),
    fixed = TRUE
  )
  expect_match(conditionMessage(partial), "whole periods")
})

test_that("a window of whole periods needs a unit in each period only", {
  master <- period_master(n = 3, seed = 1)
  df <- as.data.frame(master)
  in_window <- function(quarters) {
    # The quarters each group of `rotating_months()` is in.
    groups <- list(c(1, 2), c(2, 3), c(3, 4), c(1, 4))
    vapply(ceiling(df$.panel / 3), function(g) {
      sum(groups[[g]] %in% quarters)
    }, numeric(1))
  }

  for (quarters in list(1:2, 2:3, 1:4)) {
    window <- execute(master, period = quarters)
    rows <- as.data.frame(window)
    meta <- attr(window, "metadata")$occasion

    # Three units of a stratum cannot cover twelve panels, so each pool's
    # months are read together within each quarter.
    expect_identical(meta$window, "collapsed")
    expect_identical(
      meta$blocks,
      lapply(quarters, function(q) as.integer((q - 1) * 3 + 1:3))
    )
    expect_equal(as.vector(tapply(rows$.weight, rows$h, sum)), rep(200, 3))
    # A unit has a row in each period of the window its group is in.
    visits <- table(factor(rows$id, levels = df$id))
    expect_identical(as.vector(visits), as.integer(in_window(quarters)))
  }
})

test_that("a pool that misses a month has its quarter read together", {
  # Five units a stratum over the quarter's six panels leave stratum c a unit
  # in every month and the other two a month without one.
  master <- period_master(n = 5, seed = 2)
  record <- attr(master, "metadata")$panel_assignment
  window <- execute(master, period = 1)
  expect_identical(attr(window, "metadata")$occasion$window, "collapsed")

  at <- match(window$.sample_id, master$.sample_id)
  month_panels <- lapply(1:3, function(s) {
    record$schedule$panel[record$schedule$active &
                            record$schedule$period == 1 &
                            record$schedule$subround == s]
  })
  quarter_panels <- sort(unlist(month_panels))
  full <- tapply(window$.subround, window$h, function(s) length(unique(s)))
  # A stratum with a unit in every month reads each month at its own chance.
  # One that misses a month reads the quarter's panels together.
  expect_true(any(full == 3L) && any(full < 3L))
  for (h in names(full)) {
    rows <- which(window$h == h)
    chance <- if (full[[h]] == 3L) {
      vapply(rows, function(r) {
        block_chance(master, month_panels[[window$.subround[r]]])[at[r]]
      }, numeric(1)) * 3
    } else {
      block_chance(master, quarter_panels)[at[rows]]
    }
    expect_equal(window$.weight[rows], master$.weight[at[rows]] / chance,
                 info = h)
  }
})

test_that("a pool that misses an uneven month is refused", {
  # Month 3 holds two panels and the others one, so which month a pool of
  # two units misses depends on the schedule, not on its labels alone.
  uneven <- data.frame(panel = 1:4, period = 1L, subround = c(1L, 2L, 3L, 3L))
  master <- period_master(n = 2, schedule = uneven)
  err <- expect_error(
    execute(master, period = 1),
    class = "samplyr_error_panel_small_pool"
  )
  expect_match(conditionMessage(err), "different numbers of\\s+panels")
})

## The labels must treat the subrounds of a collapsed period alike

# Panel 3 stays in period 2 with panels 1 and 2, and panel 4 leaves, so the
# group of panels 1 to 3 holds two panels at subround 1 and one at 2.
uneven_group <- function() {
  rbind(
    data.frame(panel = 1:4, period = 1L, subround = c(1L, 1L, 2L, 2L)),
    data.frame(panel = 1:3, period = 2L, subround = c(1L, 1L, 2L))
  )
}

# Panels 1 and 2 hold one subround each in period 1, and panel 3, alone in
# period 2, takes subround 1 in the spread's count.
later_group <- function() {
  rbind(
    data.frame(panel = 1:2, period = 1L, subround = 1:2, active = TRUE),
    data.frame(panel = 3L, period = 1L, subround = 1L, active = FALSE),
    data.frame(panel = 1:2, period = 2L, subround = 1L, active = FALSE),
    data.frame(panel = 3L, period = 2L, subround = 1L, active = TRUE)
  )
}

# The two panels swap subrounds in period 2, which keeps them alike.
swapped_months <- function() {
  rbind(
    data.frame(panel = 1:2, period = 1L, subround = 1:2),
    data.frame(panel = 1:2, period = 2L, subround = 2:1)
  )
}

# The chance that a pool of m units misses each subround of period p, over
# every order of the labels the spread starts from.
miss_chance <- function(schedule, p, m) {
  spec <- samplyr:::normalize_panel_input(schedule)
  grid <- spec$schedule
  on <- grid[grid$active & grid$period == p, ]
  subrounds <- sort(unique(on$subround))
  orders <- function(v) {
    if (length(v) <= 1L) {
      return(list(v))
    }
    do.call(c, lapply(seq_along(v), function(i) {
      lapply(orders(v[-i]), function(rest) c(v[i], rest))
    }))
  }
  missed <- vapply(orders(seq_len(spec$k)), function(perm) {
    labels <- samplyr:::spread_panel_order(
      perm, spec$reporting$group, spec$reporting$subround
    )[seq_len(m)]
    !subrounds %in% on$subround[on$panel %in% labels]
  }, logical(length(subrounds)))
  rowMeans(missed)
}

test_that("the spread misses a subround by chance only where the rule allows", {
  expect_identical(miss_chance(uneven_group(), 1, 2), c(0.25, 0))
  expect_identical(miss_chance(later_group(), 1, 2), c(2, 1) / 3)
  expect_identical(miss_chance(swapped_months(), 2, 1), c(0.5, 0.5))
})

test_that("a period whose subrounds the labels treat apart is refused", {
  for (schedule in list(uneven_group(), later_group())) {
    master <- period_master(n = 2, schedule = schedule)
    err <- expect_error(
      execute(master, period = 1),
      class = "samplyr_error_panel_small_pool"
    )
    expect_match(conditionMessage(err),
                 "rotation groups do not hold their panels alike")
  }
})

test_that("panels that swap subrounds keep their period collapsible", {
  master <- suppressMessages(
    period_master(n = 1, schedule = swapped_months())
  )
  window <- execute(master, period = 2)
  expect_identical(attr(window, "metadata")$occasion$window, "collapsed")
  expect_equal(as.vector(tapply(window$.weight, window$h, sum)), rep(200, 3))
})

test_that("each part of a window enters by its share of the occasions", {
  window <- as.data.frame(execute(
    period_master(n = 6, seed = 1),
    period = c(1, 1, 2, 2, 2), subround = c(2, 3, 1:3)
  ))
  part <- ifelse(window$.period == 1, "q1", "q2")

  expect_equal(
    tapply(window$.weight, list(window$h, part), sum),
    matrix(200 * c(2, 2, 2, 3, 3, 3) / 5, 3,
           dimnames = list(c("a", "b", "c"), c("q1", "q2")))
  )
})

test_that("a panel that returns inside a period is read occasion by occasion", {
  # Six panels over two quarters, each in two of a quarter's three months.
  twice <- expand.grid(panel = 1:6, subround = 1:3, period = 1:2)
  twice$active <- twice$subround - 1 != (twice$panel - 1) %% 3
  master <- suppressMessages(period_master(n = 1, schedule = twice))

  err <- expect_error(
    execute(master, period = 1:2),
    class = "samplyr_error_panel_small_pool"
  )
  expect_match(
    cli::ansi_strip(conditionMessage(err)),
    paste("This window, periods 1 and 2, cannot be materialized: period 1",
          "subround 2 holds no unit of pool"),
    fixed = TRUE
  )
  expect_match(conditionMessage(err), "several occasions of one period")
})

test_that("a window's lone units share one stratum, and the design's keep theirs", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # Six units a stratum over two quarters leave some units alone in their
  # cell. They share one stratum, so the export makes no lonely stratum.
  window <- execute(period_master(n = 6, seed = 1), period = 1:2)
  expect_no_warning(svy <- as_svydesign(window))
  expect_true(is.finite(as.vector(survey::SE(survey::svytotal(~y, svy)))))

  # A stratum the window itself leaves with one unit keeps the design advice.
  quarter <- execute(period_master(n = 3, seed = 1), period = 1)
  design <- expect_warning(
    as_svydesign(quarter),
    class = "samplyr_warning_lonely_psu"
  )
  expect_match(conditionMessage(design), "collapse the strata")
  expect_no_match(conditionMessage(design), "window's export")
})

test_that("units alone in their occasions share a stratum by their visits", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # Twelve units a stratum over twelve panels put each unit of a two-quarter
  # window alone in its set of occasions. They fall back to the count of
  # their occasions, which units of the stratum share.
  window <- execute(period_master(n = 12, seed = 1), period = 1:2)
  expect_no_warning(svy <- as_svydesign(window))
  expect_true(is.finite(unname(survey::SE(survey::svytotal(~y, svy)))))
})

## Naming occasions

test_that("period and subround are read as pairs", {
  # With full blocks every subround holds a unit of each pool.
  master <- period_master(n = 12, seed = 1)
  occasions <- function(...) {
    meta <- attr(execute(master, ...), "metadata")$occasion
    paste(meta$period, meta$subround)
  }

  expect_identical(occasions(period = 2), paste(2, 1:3))
  expect_identical(occasions(period = 2, subround = 1), "2 1")
  expect_identical(occasions(period = 3, subround = 1:2), paste(3, 1:2))
  expect_identical(occasions(period = 1:4, subround = 1), paste(1:4, 1))
  expect_identical(
    occasions(period = c(1, 1, 2), subround = c(2, 3, 1)),
    c("1 2", "1 3", "2 1")
  )
  # Order does not matter, and the window names its occasions.
  expect_identical(
    occasions(period = c(2, 1, 1), subround = c(1, 3, 2)),
    c("1 2", "1 3", "2 1")
  )
  window <- execute(master, period = 1:4, subround = 1)
  expect_identical(
    sort(unique(paste(window$.period, window$.subround))),
    paste(1:4, 1)
  )
  expect_match(
    paste(capture.output(print(window)), collapse = "\n"),
    "period.subround 1.1, 2.1, 3.1, 4.1"
  )
  expect_match(
    paste(capture.output(print(execute(master, period = 1:2))),
          collapse = "\n"),
    "periods 1 and 2"
  )
})

test_that("a request that names no occasion of the schedule is refused", {
  master <- period_master(n = 6, seed = 1)

  expect_error(
    execute(master, period = 1:2, subround = 1:3),
    "lengths? 2 and 3|length 2 and",
    class = "samplyr_error_occasion_pairs"
  )
  expect_error(
    execute(master, subround = 1),
    class = "samplyr_error_occasion_pairs"
  )
  expect_error(
    execute(master, period = c(1, 1), subround = 2),
    class = "samplyr_error_occasion_pairs"
  )
  expect_error(
    execute(master, period = c(1, 1)),
    class = "samplyr_error_occasion_pairs"
  )
  expect_error(
    execute(master, period = 1.5),
    class = "samplyr_error_occasion_pairs"
  )
  expect_error(
    execute(master, period = 5),
    "does not declare period 5",
    class = "samplyr_error_occasion_undeclared"
  )
  expect_error(
    execute(master, period = 1, subround = 4),
    "does not declare period 1 subround 4",
    class = "samplyr_error_occasion_undeclared"
  )
  expect_error(
    execute(master, wave = 2),
    "names an occasion by",
    class = "samplyr_error_unknown_argument"
  )
})

test_that("a schedule without subrounds takes subround 1 or none", {
  master <- period_master(n = 12, schedule = rotating_months(period = FALSE))

  one <- execute(master, period = 3, subround = 1)
  whole <- execute(master, period = 3)
  expect_identical(one$id, whole$id)
  expect_identical(one$.weight, whole$.weight)
  expect_error(
    execute(master, period = 3, subround = 2),
    class = "samplyr_error_occasion_undeclared"
  )
})

test_that("a joint expectation names its two occasions as pairs", {
  master <- period_master(n = 12, seed = 1)
  across <- joint_expectation(master, period = c(1, 2), subround = c(2, 1))

  expect_identical(unique(across$period_1), 1L)
  expect_identical(unique(across$subround_1), 2L)
  expect_identical(unique(across$period_2), 2L)
  expect_identical(unique(across$subround_2), 1L)
  # Each panel keeps its month, so no panel is active at both occasions.
  expect_identical(unique(across$take_both), 0L)

  within <- joint_expectation(master, period = 1, subround = c(2, 2))
  expect_identical(within$take_1, within$take_both)

  expect_error(
    joint_expectation(master, period = c(1, 2)),
    "holds several subrounds",
    class = "samplyr_error_occasions_not_a_pair"
  )
  expect_error(
    joint_expectation(master, period = 1:3, subround = 1),
    class = "samplyr_error_occasions_not_a_pair"
  )
  expect_error(
    joint_expectation(master, period = 1, subround = c(1, 4)),
    class = "samplyr_error_occasion_undeclared"
  )
})

## Stacking periods and occasions

test_that("whole periods stack once per unit at their own subround", {
  master <- period_master(n = 6, seed = 1)
  q1 <- execute(master, period = 1)
  q2 <- execute(master, period = 2)
  tab <- stack_periods(q1, q2)

  expect_identical(attr(tab, "level"), "period")
  expect_identical(sort(unique(tab$period)), 1:2)
  expect_setequal(unique(tab$subround), 1:3)
  for (p in 1:2) {
    part <- tab[tab$period == p, ]
    expect_false(anyDuplicated(part$master_id) > 0L)
  }
  expect_identical(tab$subround[tab$period == 1], q1$.subround)
})

test_that("single occasions stack at the occasion level", {
  master <- period_master(n = 12, seed = 1)
  tab <- stack_periods(
    execute(master, period = 1, subround = 1),
    execute(master, period = 1, subround = 2)
  )

  expect_identical(attr(tab, "level"), "occasion")
  expect_identical(unique(paste(tab$period, tab$subround)), c("1 1", "1 2"))
})

test_that("a stack is refused across levels and for other windows", {
  master <- period_master(n = 12, seed = 1)

  expect_error(
    stack_periods(execute(master, period = 1),
                  execute(master, period = 2, subround = 1)),
    "at one level",
    class = "samplyr_error_stack_periods_input"
  )
  expect_error(
    stack_periods(execute(master, period = 1:2), execute(master, period = 3)),
    "neither a whole period",
    class = "samplyr_error_stack_periods_input"
  )
  expect_error(
    stack_periods(
      execute(master, period = c(1, 1, 2), subround = c(2, 3, 1)),
      execute(master, period = 3)
    ),
    class = "samplyr_error_stack_periods_input"
  )
  expect_error(
    stack_periods(execute(master, period = 1), execute(master, period = 1)),
    "given more than once",
    class = "samplyr_error_stack_periods_input"
  )

  # A panel active at two subrounds of a period would repeat its units.
  twice <- expand.grid(panel = 1:6, subround = 1:3, period = 1:2)
  twice$active <- twice$subround - 1 != (twice$panel - 1) %% 3
  repeated <- suppressMessages(period_master(n = 12, schedule = twice))
  expect_error(
    stack_periods(execute(repeated, period = 1),
                  execute(repeated, period = 2)),
    "neither a whole period",
    class = "samplyr_error_stack_periods_input"
  )
})

## Records and replay

test_that("a master with periods replays from its design file", {
  master <- period_master(n = 3)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(master, path, frame = period_frame())

  receipt <- jsonlite::read_json(path)$execution$panel_assignment
  expect_identical(receipt$version, 7L)
  written <- data.frame(
    period = vapply(receipt$schedule, function(row) row$period, integer(1)),
    subround = vapply(receipt$schedule, function(row) row$subround,
                      integer(1))
  )
  expected <- rotating_months()
  expected <- expected[order(expected$period, expected$subround,
                             expected$panel), c("period", "subround")]
  rownames(expected) <- NULL
  expect_identical(written, expected)

  replayed <- replay_design(read_design(path), period_frame())
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(
    attr(replayed, "metadata")$panel_assignment$schedule$period,
    attr(master, "metadata")$panel_assignment$schedule$period
  )
})

test_that("a record without subrounds in its schedule is refused", {
  record <- attr(period_master(n = 3), "metadata")$panel_assignment
  record$schedule$subround <- NULL

  expect_error(
    samplyr:::prepare_panel_record(record, "A wave"),
    class = "samplyr_error_panel_record_malformed"
  )
})

## Planning schedules

test_that("an svyplan schedule with subrounds checks each period", {
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
  expect_identical(attr(master, "metadata")$panel_assignment$version, 7L)
})

test_that("kept primary units are told apart by their visits", {
  # Every group is active every quarter, and only cohort timing differs.
  # The startup panels run from the shortest first cohort to the whole
  # life, and the last two differ only after the horizon, so they are one.
  plan <- monthly_plan(retain_psus = TRUE, policy = "truncate_lives")
  spec <- samplyr:::normalize_panel_input(plan)

  expect_identical(spec$reporting$group, rep(c(1L, 2L, 3L, 4L, 4L), each = 3))
  expect_identical(spec$reporting$threshold, 0L)
})

test_that("an svyplan schedule without subrounds keeps its labels", {
  expect_null(samplyr:::normalize_panel_input(monthly_plan(1))$reporting)
})
