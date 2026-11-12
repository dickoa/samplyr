## Fixtures

window_frame <- function() {
  data.frame(
    id = 1:600,
    h = rep(c("a", "b", "c"), each = 200),
    y = rep(c(10, 20, 30), each = 200) + (1:600) %% 7
  )
}

# 6 panels over 3 monthly periods, each panel active in one month.
monthly_schedule <- function() {
  schedule <- expand.grid(panel = 1:6, period = 1:3)
  schedule$active <- (schedule$period - 1) %% 3 == (schedule$panel - 1) %% 3
  schedule
}

# 4 panels, two active at a time, each for two consecutive periods.
overlap_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    period = rep(1:4, each = 4),
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

# A window's rows with the occasions of the window each unit is active at.
with_occasions <- function(window, unit) {
  df <- as.data.frame(window)
  at <- paste(df$.period, df$.subround)
  occasions <- tapply(at, df[[unit]], function(w) {
    paste(sort(unique(w)), collapse = ", ")
  })
  df$occasions <- as.vector(occasions[as.character(df[[unit]])])
  df
}

## A window where each unit is active once

test_that("a one-visit window reads each occasion at its own chance", {
  # 15 per stratum leaves a partial block, so the quotas are uneven.
  master <- window_master(n = 15)
  window <- execute(master, period = 1:2)
  active <- list(c(1L, 4L), c(2L, 5L))

  expect_s3_class(window, "tbl_sample")
  expect_identical(attr(window, "metadata")$occasion$window, "occasions")
  expect_identical(attr(window, "metadata")$occasion$occasion, 1:2)

  # Each unit once, at the occasion its panel is active.
  in_window <- master$.panel %in% unlist(active)
  expect_setequal(window$.sample_id, master$.sample_id[in_window])
  expect_false(anyDuplicated(window$.sample_id) > 0L)
  at <- match(window$.sample_id, master$.sample_id)
  expect_identical(window$.period, (master$.panel[at] - 1L) %% 3L + 1L)
  expect_identical(unique(window$.subround), 1L)

  for (t in 1:2) {
    chance <- block_chance(master, active[[t]])
    rows <- window$.period == t
    expect_equal(window$.weight[rows],
                 master$.weight[at[rows]] / chance[at[rows]] / 2, info = t)
  }
  # Each occasion's weights add up to half of each stratum's size.
  expect_equal(
    tapply(window$.weight, list(window$h, window$.period), sum),
    matrix(100, 3, 2, dimnames = list(c("a", "b", "c"), c("1", "2")))
  )
})

test_that("a one-visit window is the average of its occasions", {
  for (n in c(12, 15)) {
    master <- window_master(n = n)
    window <- execute(master, period = 1:3)
    for (t in 1:3) {
      single <- execute(master, period = t)
      rows <- window[window$.period == t, ]
      at <- match(single$.sample_id, rows$.sample_id)
      expect_equal(rows$.weight[at], single$.weight / 3, info = c(n, t))
    }
  }
})

test_that("a window counts each occasion once whatever its panels", {
  # One panel in the first period and three in the second: the second period
  # holds three times the rows and still counts for half of the window.
  frame <- data.frame(id = 1:80)
  schedule <- data.frame(panel = 1:4, period = c(1L, 2L, 2L, 2L))
  for (seed in 1:3) {
    master <- sampling_design() |>
      draw(n = 8) |>
      execute(frame, seed = seed, panels = schedule)
    window <- execute(master, period = 1:2)
    expect_identical(as.vector(table(window$.period)), c(2L, 6L))
    window$y <- window$.period
    # The outcome is 1 in period 1 and 2 in period 2 for every unit.
    expect_equal(sum(window$.weight * window$y), 80 * 1.5)
    skip_if_not_installed("survey")
    expect_equal(
      unname(stats::coef(survey::svytotal(~y, as_svydesign(window)))),
      80 * 1.5
    )
  }
})

test_that("a period whose subrounds hold different panels is read by occasion", {
  frame <- window_frame()
  schedule <- data.frame(panel = 1:3, period = 1L, subround = c(1L, 2L, 2L))
  master <- sampling_design() |>
    stratify_by(h) |>
    draw(n = 9) |>
    execute(frame, seed = 4, panels = schedule)
  window <- execute(master, period = 1)
  expect_identical(attr(window, "metadata")$occasion$window, "occasions")
  expect_equal(
    tapply(window$.weight, list(window$h, window$.subround), sum),
    matrix(100, 3, 2, dimnames = list(c("a", "b", "c"), c("1", "2")))
  )
  # With an outcome equal to the subround, the window estimates 1.5 a unit.
  expect_equal(sum(window$.weight * window$.subround), 600 * 1.5)
})

test_that("a window that revisits units averages its occasions", {
  master <- window_master(n = 20, schedule = overlap_schedule(), seed = 2)
  window <- execute(master, period = 1:2)
  expect_identical(attr(window, "metadata")$occasion$window, "occasions")

  first <- execute(master, period = 1)
  second <- execute(master, period = 2)
  expect_identical(nrow(window), nrow(first) + nrow(second))
  for (single in list(first, second)) {
    t <- attr(single, "metadata")$occasion$occasion
    rows <- window[window$.period == t, ]
    at <- match(single$.sample_id, rows$.sample_id)
    expect_equal(rows$.weight[at], single$.weight / 2, info = t)
  }
  expect_equal(
    sum(window$.weight * window$y),
    (sum(first$.weight * first$y) + sum(second$.weight * second$y)) / 2
  )
})

test_that("permanent units keep a row per occasion in a one-visit window", {
  frame <- window_frame()
  frame$mos <- rep(c(rep(1, 195), rep(80, 5)), 3)
  master <- suppressMessages(
    sampling_design() |>
      stratify_by(h) |>
      draw(n = 15, method = "pps_sps", mos = mos) |>
      execute(frame, seed = 3, panels = monthly_schedule())
  )
  expect_true(any(master$.certainty_1))
  window <- execute(master, period = 1:3)
  expect_identical(attr(window, "metadata")$occasion$window, "occasions")

  sure <- master$.sample_id[master$.certainty_1]
  rows <- window[window$.sample_id %in% sure, ]
  expect_identical(nrow(rows), 3L * length(sure))
  at <- match(rows$.sample_id, master$.sample_id)
  expect_equal(rows$.weight, master$.weight[at] / 3)

  # Every panel at chance one is not the master's design: units repeat.
  # The units taken with certainty add no variance, so the rest give it.
  skip_if_not_installed("survey")
  svy <- as_svydesign(window)
  df <- with_occasions(window, ".sample_id")
  reference <- survey::svydesign(
    ids = ~.sample_id,
    strata = ~ interaction(h, occasions),
    weights = ~.weight,
    nest = TRUE,
    data = df[!df$.certainty_1, ]
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
  window <- execute(master, period = 1:2)
  svy <- as_svydesign(window)
  expect_s3_class(svy, "survey.design2")

  # 20 units over 4 panels fill 5 blocks of 4 with one unit of each panel,
  # so each set of occasions pairs blocks 1 and 2 and joins blocks 3 to 5.
  df <- with_occasions(window, ".sample_id")
  df$blocks <- ifelse(row_block(master, df) <= 2L, "1-2", "3-5")
  reference <- survey::svydesign(
    ids = ~.sample_id,
    strata = ~ interaction(h, occasions, blocks),
    weights = ~.weight,
    nest = TRUE,
    data = df
  )
  # Units seen twice are one cluster, so the degrees of freedom count units,
  # in 3 strata by 3 sets of occasions (the first, the second or both) by 2
  # groups of blocks.
  n_units <- length(unique(window$.sample_id))
  expect_identical(survey::degf(svy), n_units - 18L)
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

test_that("a window's standard error leaves out the differences between occasions", {
  skip_if_not_installed("survey")
  # 12 per stratum fill the blocks evenly, so a stratum's weights are equal
  # within each month and a shift of one month's outcome adds no spread.
  window <- execute(window_master(n = 12), period = 1:3)
  shifted <- window
  shifted$y <- window$y + 40 * (window$.period == 2L)
  se <- function(w) unname(survey::SE(survey::svytotal(~y, as_svydesign(w))))
  expect_equal(se(shifted), se(window))
  expect_equal(
    sum(shifted$.weight * shifted$y) - sum(window$.weight * window$y),
    40 * 600 / 3
  )
})

test_that("a window's standard error leaves out the differences between blocks", {
  skip_if_not_installed("survey")
  # 15 per stratum fill two blocks unevenly, so a month's weights differ
  # between blocks. Within a block and month they are equal, and a constant
  # outcome then has no spread to report.
  window <- execute(window_master(n = 15), period = 1:2)
  window$y <- 1
  svy <- as_svydesign(window)
  expect_equal(sum(window$.weight), 600)
  expect_equal(as.vector(survey::SE(survey::svytotal(~y, svy))), 0)
})

test_that("a window's merged strata take ratio residuals", {
  skip_if_not_installed("survey")
  # 18 units over 4 panels fill blocks of 5, 5, 4 and 4, so the blocks merged
  # in a stratum carry different multipliers. An outcome equal to each row's
  # auxiliary, one over its master weight, makes every unit's total its
  # multiplier, which the ratio residuals remove entirely.
  master <- window_master(n = 18, schedule = overlap_schedule(), seed = 2)
  window <- execute(master, period = 1:4)
  at <- match(window$.sample_id, master$.sample_id)
  window$y <- 1 / master$.weight[at]
  svy <- as_svydesign(window)
  expect_equal(unname(stats::weights(svy)), window$.weight)
  expect_lt(as.vector(survey::SE(survey::svytotal(~y, svy))), 1e-8)

  plain <- survey::svydesign(
    ids = ~.sample_id, strata = ~ interaction(h, .panel), weights = ~.weight,
    data = as.data.frame(window)
  )
  expect_gt(as.vector(survey::SE(survey::svytotal(~y, plain))), 1e-3)

  # The replicates are calibrated the same way.
  jkn <- as_svrepdesign(window, type = "JKn")
  expect_equal(unname(stats::weights(jkn, type = "sampling")), window$.weight)
  expect_lt(as.vector(survey::SE(survey::svytotal(~y, jkn))), 1e-8)
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
  window <- execute(master, period = 1:3)
  svy <- as_svydesign(window)
  reference <- survey::svydesign(
    ids = ~psu,
    strata = ~ interaction(h, occasions),
    weights = ~.weight,
    nest = TRUE,
    data = with_occasions(window, "psu")
  )
  expect_identical(survey::degf(svy), survey::degf(reference))
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, reference)))
  )
})

test_that("a primary unit taken with certainty represents itself", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # EA 1 is taken with certainty, so it is active at every occasion.
  frame <- expand.grid(hh = 1:4, psu = 1:9)
  frame$h <- "a"
  frame$mos <- ifelse(frame$psu == 1L, 1000, 10)
  frame$y <- (frame$hh * 3 + frame$psu) %% 7
  schedule <- data.frame(
    period = c(1, 1, 2, 2), panel = c(1, 2, 1, 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  master <- suppressMessages(
    sampling_design() |>
      add_stage() |> stratify_by(h) |> cluster_by(psu) |>
      draw(n = 5, method = "pps_brewer", mos = mos) |>
      add_stage() |> cluster_by(hh) |> draw(n = 2) |>
      execute(frame, seed = 1, panels = schedule)
  )
  expect_identical(unique(master$psu[master$.certainty_1]), 1L)

  # Its households are the clusters of a stratum of its own, in an occasion and
  # in a window, where it would otherwise stand alone among its occasions.
  for (occasions in list(1, 1:2)) {
    window <- execute(master, period = occasions)
    df <- if (length(occasions) > 1L) {
      with_occasions(window, "psu")
    } else {
      transform(as.data.frame(window), occasions = "1")
    }
    df$unit <- ifelse(df$.certainty_1, paste0("hh", df$hh), paste0("ea", df$psu))
    df$stratum <- interaction(df$h, df$occasions, df$.certainty_1)
    reference <- survey::svydesign(
      ids = ~unit, strata = ~stratum, weights = ~.weight, data = df
    )
    expect_no_warning(svy <- as_svydesign(window))
    expect_identical(survey::degf(svy), survey::degf(reference))
    expect_equal(
      unname(survey::SE(survey::svytotal(~y, svy))),
      unname(survey::SE(survey::svytotal(~y, reference)))
    )
  }

})

test_that("a unit taken with certainty and nothing below adds no variance", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # EA 1, or element 1, is taken with certainty.
  eas <- expand.grid(hh = 1:4, psu = 1:9)
  eas$mos <- ifelse(eas$psu == 1L, 1000, 10)
  eas$y <- (eas$hh * 3 + eas$psu) %% 7
  elements <- data.frame(id = 1:30, mos = c(1000, rep(10, 29)))
  elements$y <- (elements$id * 7) %% 11
  schedule <- data.frame(
    period = c(1, 1, 2, 2), panel = c(1, 2, 1, 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  masters <- suppressMessages(list(
    psu = sampling_design() |> cluster_by(psu) |>
      draw(n = 5, method = "pps_brewer", mos = mos) |>
      execute(eas, seed = 1, panels = schedule),
    .sample_id = sampling_design() |>
      draw(n = 7, method = "pps_brewer", mos = mos) |>
      execute(elements, seed = 1, panels = schedule)
  ))
  for (unit in names(masters)) {
    master <- masters[[unit]]
    expect_identical(unique(master[[unit]][master$.certainty_1]), 1L)
    for (occasions in list(1, 1:2)) {
      window <- execute(master, period = occasions)
      df <- if (length(occasions) > 1L) {
        with_occasions(window, unit)
      } else {
        transform(as.data.frame(window), occasions = "1")
      }
      # The unit is the same in every sample, so the others give the whole
      # variance. In a window it would stand alone among its occasions.
      reference <- survey::svydesign(
        ids = stats::as.formula(paste0("~", unit)),
        strata = ~occasions,
        weights = ~.weight,
        data = df[!df$.certainty_1, ]
      )
      se <- as.vector(survey::SE(survey::svytotal(~y, reference)))
      info <- paste(unit, length(occasions))
      expect_no_warning(svy <- as_svydesign(window))
      expect_equal(as.vector(survey::SE(survey::svytotal(~y, svy))), se,
                   info = info)
      jkn <- as_svrepdesign(window, type = "JKn")
      expect_equal(as.vector(survey::SE(survey::svytotal(~y, jkn))), se,
                   info = info)
      boot <- as_svrepdesign(window, type = "subbootstrap", replicates = 20)
      sure <- boot$variables$.certainty_1
      expect_equal(
        unname(weights(boot, type = "analysis")[sure, , drop = FALSE]),
        matrix(boot$variables$.weight[sure], sum(sure), 20),
        info = info
      )
    }
  }
})

## What a window is not

test_that("a window is refused where it has no meaning yet", {
  master <- window_master(n = 20, schedule = overlap_schedule(), seed = 2)
  window <- execute(master, period = 2:3)

  expect_error(
    execute(window, period = 1),
    class = "samplyr_error_occasion_already_materialized"
  )
  expect_error(
    stack_periods(window, execute(master, period = 1)),
    class = "samplyr_error_stack_periods_input"
  )
  expect_error(
    write_design(window, withr::local_tempfile(fileext = ".json")),
    class = "samplyr_error_serialize_occasion_window"
  )

  # A window needs every stage drawn, and a listing draws one period.
  incomplete <- sampling_design() |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(id) |>
    draw(n = 20) |>
    add_stage() |>
    draw(n = 1) |>
    execute(window_frame(), stages = 1, seed = 5, panels = subrounds(4))
  expect_error(
    execute(incomplete, period = 1, subround = 1:2),
    class = "samplyr_error_occasion_incomplete_master"
  )
  expect_error(
    execute(incomplete, window_frame(), period = 1, subround = 1:2),
    class = "samplyr_error_occasion_window_unsupported"
  )
})

test_that("a window prints the occasions it pools", {
  master <- window_master(n = 15)
  expect_output(print(execute(master, period = 1:3)),
                "Occasions:\\s+periods 1 to 3")
  expect_output(print(execute(master, period = c(1, 3))),
                "Occasions:\\s+periods 1 and 3")
})

test_that("summary() of an occasion describes the occasion, with or without a digest", {
  stage_lines <- function(x) {
    out <- cli::ansi_strip(capture.output(print(summary(x))))
    out[startsWith(out, "i Occasion") | grepl("strata: N_h", out, fixed = TRUE)]
  }
  draw_master <- function(frame_digest) {
    sampling_design() |>
      stratify_by(h) |>
      draw(n = 20) |>
      execute(window_frame(), seed = 2, panels = overlap_schedule(),
              frame_digest = frame_digest)
  }
  full <- draw_master("full")
  none <- draw_master("none")
  expect_false(is.null(get_frame_digest(full)))
  # Panel 2 is active at both occasions, so the window revisits its units.
  for (occasions in list(2, 1:2)) {
    occasion <- execute(full, period = occasions)
    lines <- stage_lines(occasion)
    expect_identical(lines, stage_lines(execute(none, period = occasions)))
    n_h <- unique(tapply(occasion$.sample_id, occasion$h, function(id) {
      length(unique(id))
    }))
    expect_length(n_h, 1L)
    expect_identical(lines[[2]], sprintf(
      "* 3 strata: N_h 200, n_h %d, f_h %.4f", n_h, n_h / 200
    ))
    expect_identical(lines[[1]], sprintf(
      "i %s: %s. It holds %d of the master's 60 units.",
      if (length(occasions) > 1L) "Occasions" else "Occasion",
      if (length(occasions) > 1L) "periods 1 and 2" else "period 2",
      length(unique(occasion$.sample_id))
    ))
  }
})

test_that("a primary unit taken by an SRS census represents itself", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # Every EA is taken and the outcome is constant within each, so an
  # occasion and a window have no variance, under SRS as under PPS.
  frame <- expand.grid(hh = 1:8, ea = 1:3)
  frame$mos <- 10
  frame$y <- frame$ea * 10
  schedule <- data.frame(
    period = c(1, 1, 2, 2), panel = c(1, 2, 1, 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  for (method in c("srswor", "pps_brewer")) {
    first <- sampling_design() |> add_stage() |> cluster_by(ea)
    first <- if (identical(method, "srswor")) {
      draw(first, n = 3)
    } else {
      draw(first, n = 3, method = method, mos = mos)
    }
    master <- suppressMessages(
      first |>
        add_stage() |> cluster_by(hh) |> draw(n = 4) |>
        execute(frame, seed = 1, panels = schedule, panel_stage = 2)
    )
    for (occasions in list(1, 1:2)) {
      window <- suppressMessages(execute(master, period = occasions))
      est <- survey::svytotal(~y, as_svydesign(window))
      expect_equal(as.numeric(stats::coef(est)), 480, label = method)
      expect_equal(as.numeric(survey::SE(est)), 0, label = method)
    }
  }
})

test_that("a census rotated by panels at stage 1 is no census of an occasion", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # Every EA is taken, but each occasion holds the EAs of its panel only, so
  # the EAs vary between occasions and stay the clusters.
  frame <- expand.grid(hh = 1:4, ea = 1:8)
  frame$y <- frame$ea * 10
  schedule <- data.frame(
    period = c(1, 1, 2, 2), panel = c(1, 2, 1, 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  master <- suppressMessages(
    sampling_design() |>
      add_stage() |> cluster_by(ea) |> draw(n = 8) |>
      add_stage() |> cluster_by(hh) |> draw(n = 2) |>
      execute(frame, seed = 1, panels = schedule, panel_stage = 1)
  )
  occasion <- suppressMessages(execute(master, period = 1))
  expect_identical(length(unique(occasion$ea)), 4L)
  reference <- survey::svydesign(
    ids = ~ea, weights = ~.weight, data = as.data.frame(occasion)
  )
  expect_equal(
    survey::SE(survey::svytotal(~y, as_svydesign(occasion))),
    survey::SE(survey::svytotal(~y, reference))
  )
})
