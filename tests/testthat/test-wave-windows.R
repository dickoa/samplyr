## Fixtures

window_frame <- function() {
  data.frame(
    id = 1:600,
    h = rep(c("a", "b", "c"), each = 200),
    y = rep(c(10, 20, 30), each = 200) + (1:600) %% 7
  )
}

# 6 panels over 3 monthly waves, each panel active in one month.
monthly_schedule <- function() {
  schedule <- expand.grid(panel = 1:6, wave = 1:3)
  schedule$active <- (schedule$wave - 1) %% 3 == (schedule$panel - 1) %% 3
  schedule
}

# 4 panels, two active at a time, each for two consecutive waves.
overlap_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    wave = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
}

window_master <- function(n = 15, schedule = monthly_schedule(), seed = 1) {
  sampling_design() |>
    stratify_by(h) |>
    draw(n = n) |>
    execute(window_frame(), seed = seed, panels = schedule)
}

# The chance a unit's block gives the union of `panels`, from the record.
union_chance <- function(master, panels) {
  record <- attr(master, "metadata")$panel_assignment
  df <- as.data.frame(master)
  keys <- do.call(paste, c(df[record$key_vars], sep = "\r"))
  chance <- rep(NA_real_, nrow(df))
  for (pool in record$pools) {
    pool_keys <- vapply(pool$keys, identity, character(1))
    at <- match(keys, pool_keys)
    rows <- which(!is.na(at))
    block <- rep(seq_along(pool$blocks), pool$blocks)[at[rows]]
    take <- rowSums(pool$quotas[, panels, drop = FALSE])
    chance[rows] <- take[block] / pool$blocks[block]
  }
  chance
}

# A window's rows with the count of its waves each unit is active.
with_visits <- function(window, unit) {
  df <- as.data.frame(window)
  visits <- tapply(df$.wave, df[[unit]], function(w) length(unique(w)))
  df$visits <- as.vector(visits[as.character(df[[unit]])])
  df
}

## A window where each unit is active once

test_that("a one-visit window is one activation over its panels", {
  # 15 per stratum leaves a partial block, so the quotas are uneven.
  master <- window_master(n = 15)
  window <- execute(master, wave = 1:2)
  union <- c(1L, 2L, 4L, 5L)

  expect_s3_class(window, "tbl_sample")
  expect_identical(attr(window, "metadata")$wave$window, "union")
  expect_identical(attr(window, "metadata")$wave$wave, 1:2)

  # Each unit once, at the wave its panel is active.
  in_union <- master$.panel %in% union
  expect_setequal(window$.sample_id, master$.sample_id[in_union])
  expect_false(anyDuplicated(window$.sample_id) > 0L)
  at <- match(window$.sample_id, master$.sample_id)
  expect_identical(window$.wave, (master$.panel[at] - 1L) %% 3L + 1L)

  chance <- union_chance(master, union)
  expect_equal(window$.weight, master$.weight[at] / chance[at])
  # The union's weights add up to each stratum's size.
  expect_equal(
    as.vector(tapply(window$.weight, window$h, sum)),
    c(200, 200, 200)
  )
})

test_that("with full blocks a one-visit window is the average of its waves", {
  # 12 per stratum fills two blocks of 6, so every quota is even.
  master <- window_master(n = 12)
  window <- execute(master, wave = 1:3)
  for (t in 1:3) {
    single <- execute(master, wave = t)
    rows <- window[window$.wave == t, ]
    at <- match(single$.sample_id, rows$.sample_id)
    expect_equal(rows$.weight[at], single$.weight / 3, info = t)
  }
})

test_that("a window that revisits units averages its waves", {
  master <- window_master(n = 20, schedule = overlap_schedule(), seed = 2)
  window <- execute(master, wave = 1:2)
  expect_identical(attr(window, "metadata")$wave$window, "average")

  first <- execute(master, wave = 1)
  second <- execute(master, wave = 2)
  expect_identical(nrow(window), nrow(first) + nrow(second))
  for (single in list(first, second)) {
    t <- attr(single, "metadata")$wave$wave
    rows <- window[window$.wave == t, ]
    at <- match(single$.sample_id, rows$.sample_id)
    expect_equal(rows$.weight[at], single$.weight / 2, info = t)
  }
  expect_equal(
    sum(window$.weight * window$y),
    (sum(first$.weight * first$y) + sum(second$.weight * second$y)) / 2
  )
})

test_that("permanent units keep a row per wave in a one-visit window", {
  frame <- window_frame()
  frame$mos <- rep(c(rep(1, 195), rep(80, 5)), 3)
  master <- suppressMessages(
    sampling_design() |>
      stratify_by(h) |>
      draw(n = 15, method = "pps_sps", mos = mos) |>
      execute(frame, seed = 3, panels = monthly_schedule())
  )
  expect_true(any(master$.certainty_1))
  window <- execute(master, wave = 1:3)
  expect_identical(attr(window, "metadata")$wave$window, "union")

  sure <- master$.sample_id[master$.certainty_1]
  rows <- window[window$.sample_id %in% sure, ]
  expect_identical(nrow(rows), 3L * length(sure))
  at <- match(rows$.sample_id, master$.sample_id)
  expect_equal(rows$.weight, master$.weight[at] / 3)

  # Every panel at chance one is not the master's design: units repeat.
  skip_if_not_installed("survey")
  svy <- as_svydesign(window)
  reference <- survey::svydesign(
    ids = ~.sample_id,
    strata = ~ interaction(h, visits),
    weights = ~.weight,
    nest = TRUE,
    data = with_visits(window, ".sample_id")
  )
  expect_identical(survey::degf(svy), survey::degf(reference))
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, reference)))
  )
})

## Export

test_that("a window exports by ultimate clusters with the unit as cluster", {
  skip_if_not_installed("survey")
  master <- window_master(n = 20, schedule = overlap_schedule(), seed = 2)
  window <- execute(master, wave = 1:2)
  svy <- as_svydesign(window)
  expect_s3_class(svy, "survey.design2")

  reference <- survey::svydesign(
    ids = ~.sample_id,
    strata = ~ interaction(h, visits),
    weights = ~.weight,
    nest = TRUE,
    data = with_visits(window, ".sample_id")
  )
  # Units seen twice are one cluster, so the degrees of freedom count units,
  # in strata crossed with the number of waves a unit is active.
  n_units <- length(unique(window$.sample_id))
  expect_identical(survey::degf(svy), n_units - 6L)
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, reference)))
  )
  expect_s3_class(as_svrepdesign(window), "svyrep.design")

  expect_error(
    as_svydesign(window, method = "full"),
    class = "samplyr_error_survey_argument"
  )
})

test_that("a clustered window keeps its primary units as clusters", {
  skip_if_not_installed("survey")
  frame <- window_frame()
  frame$psu <- (frame$id - 1) %/% 10
  master <- sampling_design() |>
    stratify_by(h) |>
    cluster_by(psu) |>
    draw(n = 8) |>
    execute(frame, seed = 4, panels = overlap_schedule())
  window <- execute(master, wave = 1:3)
  svy <- as_svydesign(window)
  reference <- survey::svydesign(
    ids = ~psu,
    strata = ~ interaction(h, visits),
    weights = ~.weight,
    nest = TRUE,
    data = with_visits(window, "psu")
  )
  expect_identical(survey::degf(svy), survey::degf(reference))
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, reference)))
  )
})

## What a window is not

test_that("a window is refused where it has no meaning yet", {
  master <- window_master(n = 20, schedule = overlap_schedule(), seed = 2)
  window <- execute(master, wave = 2:3)

  expect_error(
    execute(window, wave = 1),
    class = "samplyr_error_wave_already_materialized"
  )
  expect_error(
    stack_waves(window, execute(master, wave = 1)),
    class = "samplyr_error_stack_waves_input"
  )
  expect_error(
    write_design(window, withr::local_tempfile(fileext = ".json")),
    class = "samplyr_error_serialize_wave_window"
  )

  # A window needs every stage drawn, and a listing draws one period.
  incomplete <- sampling_design() |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(id) |>
    draw(n = 20) |>
    add_stage() |>
    draw(n = 1) |>
    execute(window_frame(), stages = 1, seed = 5, panels = periods(4))
  expect_error(
    execute(incomplete, wave = 1:2),
    class = "samplyr_error_wave_incomplete_master"
  )
  expect_error(
    execute(incomplete, window_frame(), wave = 1:2),
    class = "samplyr_error_wave_window_unsupported"
  )
})

test_that("a window prints the waves it pools", {
  master <- window_master(n = 15)
  expect_output(print(execute(master, wave = 1:3)), "Waves:\\s+1 to 3 of 3")
  expect_output(print(execute(master, wave = c(1, 3))), "Waves:\\s+1 and 3 of 3")
})
