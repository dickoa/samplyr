## periods(): a one-visit partition of the sample

poura_urban <- function() {
  bfa_eas[bfa_eas$commune == "Poura" & bfa_eas$urban_rural == "Urban", ]
}

pps_12 <- function() {
  sampling_design() |>
    draw(n = 12, method = "pps_systematic", mos = households)
}

test_that("a certainty unit takes one period like any other", {
  # Poura's largest urban EA is certain at n = 12, so a count would keep
  # it apart and leave the twelve panels unbalanced.
  certain_period <- integer(0)
  for (seed in 1:20) {
    sample <- suppressMessages(
      execute(pps_12(), poura_urban(), seed = seed, panels = periods(12))
    )
    expect_identical(sum(sample$.certainty_1), 1L)
    expect_identical(tabulate(sample$.panel, 12), rep(1L, 12))
    certain_period <- c(certain_period, sample$.panel[sample$.certainty_1])
  }
  expect_gt(length(unique(certain_period)), 1L)
})

test_that("a count keeps certainty units in a pool of their own", {
  sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = 12)
  )
  record <- attr(sample, "metadata")$panel_assignment
  expect_identical(record$version, 3L)
  expect_identical(record$certainty, "permanent")
  expect_identical(
    vapply(record$pools, `[[`, character(1), "class"),
    c("rotating", "certainty")
  )
})

test_that("a periods record is version 4 with one pool per stratum", {
  sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = periods(12))
  )
  record <- attr(sample, "metadata")$panel_assignment
  expect_identical(record$version, 4L)
  expect_identical(record$certainty, "assigned")
  expect_identical(record$panels, 12L)
  expect_identical(vapply(record$pools, `[[`, character(1), "class"),
                   "rotating")
  expect_identical(record$schedule$active,
                   as.vector(diag(12) == 1))
})

test_that("strata allocated in multiples of k get equal periods", {
  sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 24, method = "pps_systematic", mos = households,
         control = c(province, commune)) |>
    execute(bfa_eas, seed = 1, panels = periods(12))
  counts <- table(sample$region, sample$.panel)
  expect_identical(dim(counts), c(13L, 12L))
  expect_true(all(counts == 2L))
})

test_that("a stratum with fewer units than periods is refused", {
  frame <- data.frame(
    st = rep(c("a", "b"), c(30, 20)),
    id = 1:50
  )
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(n = c(a = 12, b = 5))
  err <- tryCatch(
    execute(design, frame, seed = 1, panels = periods(12)),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_panel_small_pool")
  expect_match(conditionMessage(err), "Every period needs units")
  expect_match(cli::ansi_strip(conditionMessage(err)), "\"st = b (5)\"",
               fixed = TRUE)

  expect_error(
    execute(design, frame, seed = 1, panels = periods(12),
            small_pool = "permanent"),
    class = "samplyr_error_periods_small_pool"
  )
})

test_that("periods() takes one whole number of at least 2", {
  expect_error(periods(1), class = "samplyr_error_panel_count")
  expect_error(periods(c(2, 3)), class = "samplyr_error_panel_count")
  expect_error(periods("a"), class = "samplyr_error_panel_count")
  expect_error(periods(2.5), class = "samplyr_error_panel_count")
  # A count beyond R's integers is refused, not coerced to NA.
  expect_no_warning(expect_error(periods(2^31),
                                 class = "samplyr_error_panel_count"))
  expect_no_warning(expect_error(
    execute(sampling_design() |> draw(n = 4), data.frame(id = 1:20),
            seed = 1, panels = 2^31),
    class = "samplyr_error_panel_count"
  ))
  expect_identical(periods(.Machine$integer.max)$k, .Machine$integer.max)
  expect_identical(periods(12)$k, 12L)
  expect_output(print(periods(12)), "<periods: 12>", fixed = TRUE)
})

test_that("a rotation program refuses a cohort split into periods", {
  sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = periods(12))
  )
  expect_error(
    rotation_program(
      list(a = sample),
      schedule = data.frame(panel = 1:12, wave = 1:12)
    ),
    class = "samplyr_error_program_periods"
  )
})

test_that("periods below the first stage spread a unit's children", {
  frame <- data.frame(
    psu = rep(1:6, each = 30),
    size = rep(c(500, 10, 10, 10, 10, 10), each = 30)
  )
  frame$hh <- seq_len(nrow(frame))
  design <- sampling_design() |>
    add_stage() |>
      cluster_by(psu) |>
      draw(n = 3, method = "pps_systematic", mos = size) |>
    add_stage() |>
      draw(n = 12)
  sample <- suppressMessages(
    execute(design, frame, seed = 1, panels = periods(4), panel_stage = 2)
  )
  expect_true(any(sample$.certainty_1))
  expect_false(anyNA(sample$.panel))
  # Each PSU's twelve households fill the four periods three times.
  per_psu <- table(sample$psu, sample$.panel)
  expect_true(all(per_psu == 3L))
})

## A period materialized by wave, and the header lines

test_that("each period is a wave of its own with exact weights", {
  sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 24, method = "pps_systematic", mos = households,
         control = c(province, commune)) |>
    execute(bfa_eas, seed = 1, panels = periods(12))
  seen <- integer(0)
  for (t in 1:12) {
    period <- execute(sample, wave = t)
    ids <- period$.sample_id
    expect_setequal(ids, sample$.sample_id[sample$.panel == t])
    # Each region's 24 EAs form one block of 2k units with two per period,
    # so the activation chance is 2/24 and the factor 12.
    full <- sample$.weight[match(ids, sample$.sample_id)]
    expect_equal(period$.weight, full * 12)
    seen <- c(seen, ids)
  }
  expect_false(anyDuplicated(seen) > 0L)
  expect_setequal(seen, sample$.sample_id)
})

test_that("a certainty unit is in its own period only", {
  sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = periods(12))
  )
  certain <- sample$.sample_id[sample$.certainty_1]
  found_in <- integer(0)
  for (t in 1:12) {
    period <- execute(sample, wave = t)
    if (certain %in% period$.sample_id) found_in <- c(found_in, t)
  }
  expect_identical(found_in, sample$.panel[sample$.certainty_1])
})

test_that("the header names periods, rotation groups and waves", {
  periods_sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = periods(12))
  )
  expect_identical(tbl_sum(periods_sample)[["Periods"]], "12 (in .panel)")
  expect_identical(tbl_sum(execute(periods_sample, wave = 3))[["Period"]],
                   "3 of 12")

  count_sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = 12)
  )
  expect_identical(tbl_sum(count_sample)[["Panels"]],
                   "12 rotation groups, no schedule")

  schedule <- data.frame(
    panel = rep(1:4, 3),
    wave = rep(1:3, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
               FALSE, FALSE, TRUE, TRUE)
  )
  rotating <- execute(sampling_design() |> draw(n = 40),
                      data.frame(id = 1:200), seed = 1, panels = schedule)
  expect_identical(tbl_sum(rotating)[["Panels"]],
                   "4 rotation groups over 3 waves")
  expect_identical(tbl_sum(execute(rotating, wave = 2))[["Wave"]], "2 of 3")

  plain <- execute(sampling_design() |> draw(n = 4), data.frame(id = 1:20),
                   seed = 1)
  expect_false(any(c("Periods", "Panels", "Period", "Wave") %in%
                     names(tbl_sum(plain))))
})

## Periods survive a design file and a replay

test_that("a periods sample round-trips and replays as periods", {
  sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 3, panels = periods(12))
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = poura_urban())
  read_back <- read_design(path)
  replayed <- suppressMessages(replay_design(read_back, poura_urban()))
  expect_identical(replayed$.panel, sample$.panel)
  expect_identical(replayed$ea_id, sample$ea_id)
  record <- attr(replayed, "metadata")$panel_assignment
  expect_identical(record$version, 4L)
  expect_identical(record$certainty, "assigned")
  expect_identical(tabulate(replayed$.panel, 12), rep(1L, 12))
})

## A count with certainty units says what it does with them

capture_panel_certainty <- function(expr) {
  found <- list()
  withCallingHandlers(
    force(expr),
    samplyr_message_panel_certainty = function(m) {
      found[[length(found) + 1L]] <<- m
      invokeRestart("muffleMessage")
    }
  )
  found
}

test_that("a count with certainty units points to periods()", {
  found <- capture_panel_certainty(suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = 12),
    classes = "samplyr_message_certainty_selected"
  ))
  expect_length(found, 1L)
  expect_identical(found[[1]]$stage, 1L)
  expect_identical(found[[1]]$payload$n_certainty, 1L)
  expect_identical(found[[1]]$payload$n_pools, 1L)
  expect_match(conditionMessage(found[[1]]), "panels = periods(12)",
               fixed = TRUE)
})

test_that("periods, schedules and samples without certainty stay silent", {
  quiet <- function(expr) {
    capture_panel_certainty(suppressMessages(
      expr, classes = "samplyr_message_certainty_selected"
    ))
  }
  expect_length(quiet(
    execute(pps_12(), poura_urban(), seed = 1, panels = periods(12))
  ), 0L)
  schedule <- data.frame(
    panel = rep(1:4, 3),
    wave = rep(1:3, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
               FALSE, FALSE, TRUE, TRUE)
  )
  expect_length(quiet(
    execute(pps_12(), poura_urban(), seed = 1, panels = schedule)
  ), 0L)
  expect_length(quiet(
    execute(sampling_design() |> draw(n = 12), poura_urban(), seed = 1,
            panels = 12)
  ), 0L)
})

## summary() reports how evenly each stratum fills the periods

periods_by_region <- function(n) {
  sampling_design() |>
    stratify_by(region) |>
    draw(n = n, method = "pps_systematic", mos = households) |>
    execute(bfa_eas, seed = 1, panels = periods(12))
}

test_that("an allocation in multiples of k is even everywhere", {
  out <- cli::ansi_strip(capture.output(summary(periods_by_region(24))))
  expect_true(any(out == paste0(
    "i Periods: 12, even in all 13 strata (2 units per period)."
  )))
  periods <- summary(periods_by_region(24))$periods
  expect_identical(periods$k, 12L)
  expect_identical(periods$pools$min, rep(2L, 13))
  expect_identical(periods$pools$max, rep(2L, 13))
})

test_that("uneven strata are named with their range", {
  n <- data.frame(
    region = levels(bfa_eas$region),
    n = c(24, 26, 24, 24, 38, rep(24, 7), 25)
  )
  out <- cli::ansi_strip(capture.output(summary(periods_by_region(n))))
  expect_true(any(out == "i Periods: 12, even in 10 of 13 strata."))
  expect_true(any(out == paste0(
    "  Uneven: Cascades (2 to 3 per period), Centre-Nord (3 to 4), ",
    "Sud-Ouest (2 to 3)."
  )))
})

test_that("many uneven strata are cut short", {
  out <- cli::ansi_strip(capture.output(summary(periods_by_region(25))))
  expect_true(any(out == "i Periods: 12, even in 0 of 13 strata."))
  uneven <- out[startsWith(out, "  Uneven: ")]
  expect_length(uneven, 1L)
  expect_match(uneven, ", 8 more.$")
})

test_that("a single pool and samples without periods print no strata", {
  sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = periods(12))
  )
  out <- cli::ansi_strip(capture.output(summary(sample)))
  expect_true(any(out == "i Periods: 12, 1 unit per period."))
  plain <- suppressMessages(execute(pps_12(), poura_urban(), seed = 1))
  expect_false(any(grepl("Periods", capture.output(summary(plain)))))
  expect_null(summary(plain)$periods)
  expect_null(summary(execute(sample, wave = 1))$periods)
})

test_that("periods of a first stage list their units before the next stage", {
  design <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(ea_id) |>
    draw(n = 24, method = "pps_systematic", mos = households) |>
    add_stage() |> draw(n = 12, method = "systematic")
  selected <- execute(design, bfa_eas, stages = 1, seed = 1,
                      panels = periods(12))
  month <- dplyr::filter(as.data.frame(selected), .panel == 3)
  expect_identical(nrow(month), 26L)
  listing <- function(eas) {
    eas <- as.data.frame(eas)[c("ea_id", "region")]
    eas[rep(seq_len(nrow(eas)), each = 15), ]
  }
  expect_error(execute(selected, listing(month), seed = 2),
               class = "samplyr_error_frame_missing_parent")
  expect_error(execute(selected, wave = 3),
               class = "samplyr_error_wave_incomplete_master")
  complete <- execute(selected, listing(selected), seed = 2)
  expect_setequal(execute(complete, wave = 3)$ea_id, month$ea_id)
})
