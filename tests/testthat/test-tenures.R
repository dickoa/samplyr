## Fixtures

tenure_eas <- function() {
  data.frame(
    ea = 1:60,
    h = rep(c("a", "b"), each = 30),
    size = rep(c(20, 24, 28, 32, 36, 40), times = 10)
  )
}

tenure_households <- function() {
  eas <- tenure_eas()
  hh <- data.frame(ea = rep(eas$ea, eas$size))
  hh$hh <- seq_len(nrow(hh))
  hh$y <- (hh$hh %% 11) + hh$ea / 10
  hh
}

# 3 panels over 12 monthly waves, each active in its own month of every
# quarter. A tenure lasts two quarters, so visit restarts every 6 months.
tenure_schedule <- function() {
  schedule <- expand.grid(panel = 1:3, wave = 1:12)
  schedule$active <- (schedule$wave - 1) %% 3 == (schedule$panel - 1) %% 3
  quarter <- (schedule$wave - 1) %/% 3 + 1
  schedule$visit <- ifelse(schedule$active, (quarter - 1) %% 2 + 1, NA)
  schedule
}

tenure_design <- function() {
  sampling_design() |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(ea) |>
    draw(n = 9) |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 4)
}

tenure_master <- function(schedule = tenure_schedule()) {
  execute(tenure_design(), tenure_eas(), stages = 1, seed = 1,
          panels = schedule)
}

# The full listing of the master's EAs. samplyr leaves out the households
# earlier tenures drew.
tenure_listing <- function(master) {
  hh <- tenure_households()
  hh[hh$ea %in% master$ea, ]
}

draw_tenures <- function(master, waves) {
  for (t in waves) {
    master <- execute(master, tenure_listing(master), wave = t, seed = 10 + t)
  }
  master
}

## The visit column

test_that("a schedule's visit column states where tenures open", {
  master <- tenure_master()
  record <- attr(master, "metadata")$panel_assignment
  expect_identical(record$version, 5L)

  schedule <- tenure_schedule()
  at <- match(
    paste(record$schedule$panel, record$schedule$wave),
    paste(schedule$panel, schedule$wave)
  )
  expect_identical(record$schedule$visit, as.integer(schedule$visit[at]))

  # Without the column each panel has one tenure, and the record keeps the
  # version earlier builds read.
  one <- tenure_master(schedule[c("panel", "wave", "active")])
  record <- attr(one, "metadata")$panel_assignment
  expect_identical(record$version, 3L)
  first <- record$schedule[record$schedule$panel == 1L, ]
  expect_identical(first$visit[first$active], 1:4)
})

test_that("a visit column that does not number tenures is refused", {
  schedule <- tenure_schedule()
  cases <- list(
    "starts above 1" = within(schedule, visit[active & panel == 1][1] <- 2),
    "skips" = within(schedule, visit[active & panel == 2][2] <- 3),
    "on an inactive row" = within(schedule, visit[!active][1] <- 1),
    "missing where active" = within(schedule, visit[active][1] <- NA),
    "not whole" = within(schedule, visit[active][1] <- 1.5)
  )
  for (case in names(cases)) {
    expect_error(
      tenure_master(cases[[case]]),
      class = "samplyr_error_schedule_visit",
      info = case
    )
  }
})

## Tenures are drawn where they open

test_that("tenures need a stage below the panels, drawn tenure by tenure", {
  one_stage <- sampling_design() |> stratify_by(h) |> draw(n = 9)
  expect_error(
    execute(one_stage, tenure_eas(), seed = 1, panels = tenure_schedule()),
    class = "samplyr_error_tenure_stages"
  )
  expect_error(
    execute(tenure_design(), tenure_eas(), tenure_households(), seed = 1,
            panels = tenure_schedule()),
    class = "samplyr_error_tenure_stages"
  )
  master <- tenure_master()
  expect_error(
    execute(master, tenure_households()),
    class = "samplyr_error_tenure_stages"
  )
})

test_that("a wave draws the panels whose tenure opens there", {
  master <- draw_tenures(tenure_master(), 1:3)
  draws <- attr(master, "metadata")$per_wave_draws
  expect_identical(
    lapply(draws, function(d) sort(unique(d$rows$.panel))),
    list(1L, 2L, 3L)
  )
  # A master with tenures keeps drawing, so it never closes.
  expect_identical(remaining_stages(master), 2L)

  err <- tryCatch(
    execute(master, tenure_listing(master), wave = 4, seed = 14),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_wave_already_drawn")
  expect_match(cli::ansi_strip(conditionMessage(err)), "drawn at wave 1")

  # Wave 4 is panel 1's second visit, with the households of wave 1.
  first <- execute(master, wave = 1)
  fourth <- execute(master, wave = 4)
  expect_identical(sort(fourth$hh), sort(first$hh))
  expect_identical(unique(fourth$.visit), 2L)
  expect_equal(fourth$.weight[order(fourth$hh)],
               first$.weight[order(first$hh)])
})

test_that("a wave's summary shows the seed of its tenure's draw", {
  master <- draw_tenures(tenure_master(), 1:3)
  seeds <- vapply(1:6, function(t) {
    summary(execute(master, wave = t))$seed
  }, integer(1))
  # A tenure lasts two quarters, so waves 4 to 6 read the draws of 1 to 3.
  expect_identical(seeds, c(11L, 12L, 13L, 11L, 12L, 13L))
})

test_that("a new tenure draws new units from the full listing", {
  master <- draw_tenures(tenure_master(), 1:3)
  # A listing already cut of the used households would lose them.
  used <- unlist(lapply(
    attr(master, "metadata")$per_wave_draws, function(d) d$rows$hh
  ))
  cut <- tenure_listing(master)
  expect_error(
    execute(master, cut[!cut$hh %in% used, ], wave = 7, seed = 17),
    class = "samplyr_error_tenure_listing"
  )
  # One earlier household per EA missing: read as gone, with a warning.
  first_used <- tapply(used, cut$ea[match(used, cut$hh)], min)
  trimmed <- cut[!cut$hh %in% first_used, ]
  returning <- unique(master$ea[master$.panel == 1L])
  expect_warning(
    partial <- execute(master, trimmed, wave = 7, seed = 17),
    paste(length(returning), "units that earlier tenures drew, in",
          length(returning), "primary units"),
    class = "samplyr_warning_tenure_listing"
  )
  seventh <- execute(partial, wave = 7)
  expect_false(any(seventh$hh %in% used))
  expect_no_warning(draw_tenures(master, 7))
  master <- draw_tenures(master, 7)
  seventh <- execute(master, wave = 7)
  first <- execute(master, wave = 1)
  expect_identical(unique(seventh$.visit), 1L)
  expect_identical(sort(unique(seventh$ea)), sort(unique(first$ea)))
  expect_false(any(seventh$hh %in% first$hh))
  expect_identical(sort(execute(master, wave = 10)$hh), sort(seventh$hh))

  # Wave 8 reads panel 2's second tenure, which is not drawn yet.
  err <- tryCatch(execute(master, wave = 8), error = identity)
  expect_s3_class(err, "samplyr_error_wave_incomplete_master")
  expect_match(cli::ansi_strip(conditionMessage(err)), "Draw wave 8")

  # With every tenure opening drawn the master still does not close.
  master <- draw_tenures(master, 8:9)
  expect_identical(remaining_stages(master), 2L)
  expect_identical(sort(execute(master, wave = 12)$hh),
                   sort(execute(master, wave = 9)$hh))
})

test_that("every tenure estimates the whole population", {
  # A constant outcome: every wave's total must be the population count.
  eas <- data.frame(ea = 1:12)
  hh <- data.frame(ea = rep(1:12, each = 10), hh = 1:120, y = 1)
  schedule <- expand.grid(panel = 1:2, wave = 1:4)
  schedule$active <- schedule$panel == (schedule$wave - 1) %% 2 + 1
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 8) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2)
  master <- execute(design, eas, stages = 1, panels = schedule, seed = 1)
  seen <- integer(0)
  for (t in 1:4) {
    master <- execute(master, hh, wave = t, seed = t + 5)
    wave <- execute(master, wave = t)
    expect_equal(sum(wave$.weight * wave$y), 120, info = paste("wave", t))
    # Each household's weight is 10 / 2 within its EA, as at the first.
    expect_equal(unique(wave$.weight_2), 5, info = paste("wave", t))
    expect_equal(unique(wave$.fpc_2), 10, info = paste("wave", t))
    expect_false(any(wave$hh %in% seen), info = paste("wave", t))
    seen <- c(seen, wave$hh)
  }
})

test_that("a rate or an allocation sizes each tenure on the whole listing", {
  # Two panels, each with a tenure at every one of its waves.
  schedule <- expand.grid(panel = 1:2, wave = 1:4)
  schedule$active <- schedule$panel == (schedule$wave - 1) %% 2 + 1
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  # Each EA lists 6 households of type a and 4 of type b.
  hh <- data.frame(ea = rep(1:4, each = 10), hh = 1:40)
  hh$type <- rep(rep(c("a", "b"), c(6, 4)), 4)
  takes <- function(stage2) {
    design <- sampling_design() |>
      add_stage() |> cluster_by(ea) |> draw(n = 4) |>
      add_stage() |> stage2()
    master <- execute(design, data.frame(ea = 1:4), stages = 1,
                      panels = schedule, seed = 1)
    for (t in c(1, 3)) {
      master <- suppressWarnings(execute(master, hh, wave = t, seed = t))
    }
    draws <- attr(master, "metadata")$per_wave_draws
    lapply(draws, function(d) {
      rows <- d$rows
      list(
        n = as.vector(table(factor(paste(rows$ea, rows$type)))),
        weight = sort(unique(round(rows$.weight_2, 6)))
      )
    })
  }

  # Half of each EA's 10 households at both tenures, not half of the 5
  # the first one left.
  rate <- takes(function(d) d |> cluster_by(hh) |> draw(frac = 0.5))
  fixed <- takes(function(d) d |> cluster_by(hh) |> draw(n = 5))
  expect_identical(rate, fixed)
  expect_identical(rate[[2]]$weight, 2)

  # By type: 3 of 6 and 2 of 4 at both, so the second tenure takes every
  # unit left.
  by_type <- takes(function(d) {
    d |> stratify_by(type) |> cluster_by(hh) |> draw(frac = 0.5)
  })
  expect_identical(by_type[[2]]$n, rep(c(3L, 2L), 2))
  expect_identical(by_type[[2]]$weight, 2)
  allocated <- takes(function(d) {
    d |> stratify_by(type, alloc = "proportional") |> cluster_by(hh) |>
      draw(n = 5)
  })
  expect_identical(allocated, by_type)
})

test_that("a tenure reads strata its listing leaves to the primary units", {
  # Stage 2 repeats stage 1's region, which the listing may leave out: the
  # draw carries it down from the selected EAs.
  eas <- data.frame(ea = 1:12, region = rep(c("A", "B"), each = 6))
  hh <- data.frame(ea = rep(1:12, each = 10), hh = 1:120, y = 1)
  schedule <- expand.grid(panel = 1:2, wave = 1:4)
  schedule$active <- schedule$panel == (schedule$wave - 1) %% 2 + 1
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  design <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(ea) |> draw(n = 4) |>
    add_stage() |> stratify_by(region) |> cluster_by(hh) |> draw(n = 2)
  drawn <- lapply(list(hh, merge(hh, eas, by = "ea")), function(listing) {
    master <- execute(design, eas, stages = 1, panels = schedule, seed = 1)
    for (t in 1:4) {
      master <- execute(master, listing, wave = t, seed = t + 5)
    }
    lapply(1:4, function(t) {
      wave <- as.data.frame(execute(master, wave = t))
      wave <- wave[order(wave$hh), c("hh", "region", ".weight", ".weight_2")]
      rownames(wave) <- NULL
      wave
    })
  })
  for (t in 1:4) {
    without <- drawn[[1]][[t]]
    expect_equal(sum(without$.weight), 120, info = paste("wave", t))
    expect_equal(unique(without$.weight_2), 5, info = paste("wave", t))
    expect_identical(without, drawn[[2]][[t]], info = paste("wave", t))
  }
})

test_that("a pool earlier tenures drew whole is refused", {
  schedule <- data.frame(
    wave = c(1, 1, 2, 2), panel = c(1, 2, 1, 2), active = TRUE, visit = 1L
  )
  # Stratum S1 of each EA holds two households, both drawn at wave 1.
  stratified <- rbind(
    expand.grid(hh = 1:2, s = "S1", ea = 1:2, stringsAsFactors = FALSE),
    expand.grid(hh = 3:6, s = "S2", ea = 1:2, stringsAsFactors = FALSE)
  )
  master <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 2) |>
    add_stage() |> stratify_by(s) |> cluster_by(hh) |> draw(n = 2) |>
    execute(data.frame(ea = 1:2), stages = 1, panels = schedule, seed = 1)
  master <- execute(master, stratified, wave = 1, seed = 10)
  expect_error(
    execute(master, stratified, wave = 2, seed = 11),
    "2 pools of this tenure have no unit",
    class = "samplyr_error_tenure_exhausted"
  )

  # EA 1 holds two households, both drawn at wave 1. The draw would read
  # it as empty, even where empty units are accepted.
  small <- rbind(
    data.frame(ea = 1L, hh = 1:2),
    data.frame(ea = 2L, hh = 1:6)
  )
  master <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2, on_empty = "warn") |>
    execute(data.frame(ea = 1:2), stages = 1, panels = schedule, seed = 1)
  master <- execute(master, small, wave = 1, seed = 10)
  expect_error(
    execute(master, small, wave = 2, seed = 11),
    "1 pool of this tenure has no unit",
    class = "samplyr_error_tenure_exhausted"
  )

  # A listing that adds two households to EA 1 is drawn, and weighted to
  # the four households it lists there.
  grown <- rbind(small, data.frame(ea = 1L, hh = 3:4))
  master <- execute(master, grown, wave = 2, seed = 11)
  expect_equal(sum(execute(master, wave = 2)$.weight), 10)
})

test_that("a tenure that takes what earlier tenures left is no census", {
  schedule <- data.frame(
    wave = c(1, 1, 2, 2), panel = c(1, 2, 1, 2), active = TRUE, visit = 1L
  )
  listing <- expand.grid(hh = 1:6, ea = 1:2)
  master <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    execute(data.frame(ea = 1:2), stages = 1, panels = schedule, seed = 1)
  master <- execute(master, listing, wave = 1, seed = 10)
  # Two households of six are left in each EA, so wave 2 takes both.
  cnd <- expect_warning(
    master <- execute(master, listing, wave = 2, seed = 11),
    "every unit that earlier tenures did not draw",
    class = "samplyr_warning_census"
  )
  expect_match(conditionMessage(cnd), "still contributes sampling variance",
               fixed = TRUE)
  expect_no_match(conditionMessage(cnd), "contributes no sampling variance",
                  fixed = TRUE)
  expect_identical(cnd$stage, 2L)
  expect_equal(unique(execute(master, wave = 2)$.weight_2), 3)
})

test_that("a tenure keeps apart units numbered within strata", {
  # Household numbers restart in each type of every EA, so a number drawn
  # in one type says nothing about the same number in the other.
  eas <- data.frame(ea = 1:12)
  hh <- expand.grid(ea = 1:12, type = c("a", "b"), hh = 1:10,
                    stringsAsFactors = FALSE)
  schedule <- expand.grid(panel = 1:2, wave = 1:4)
  schedule$active <- schedule$panel == (schedule$wave - 1) %% 2 + 1
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 8) |>
    add_stage() |> stratify_by(type) |> cluster_by(hh) |> draw(n = 2)
  master <- execute(design, eas, stages = 1, panels = schedule, seed = 1)
  for (t in 1:4) {
    master <- execute(master, hh, wave = t, seed = t + 5)
  }
  unit <- function(w) paste(w$ea, w$type, w$hh)
  waves <- lapply(1:4, function(t) as.data.frame(execute(master, wave = t)))
  for (t in 1:4) {
    expect_equal(sum(waves[[t]]$.weight), 240, info = paste("wave", t))
    expect_equal(unique(waves[[t]]$.weight_2), 5, info = paste("wave", t))
  }
  # A panel's second tenure draws none of the units of its first. With 8
  # of 10 households left in each type of an EA, a weight of 5 means that
  # exactly the 2 drawn there were left out.
  for (pair in list(c(1, 3), c(2, 4))) {
    expect_length(
      intersect(unit(waves[[pair[1]]]), unit(waves[[pair[2]]])), 0L
    )
    expect_equal(unique(waves[[pair[2]]]$.fpc_2), 10)
  }
})

test_that("a tenure counts the units of a listing, not its rows", {
  # One to five persons per dwelling: dwellings are the units a tenure
  # redraws, and a count of rows would weigh them by their size.
  eas <- data.frame(ea = 1:12)
  size <- rep(1:5, length.out = 120)
  persons <- data.frame(
    ea = rep(rep(1:12, each = 10), size),
    dwelling = rep(1:120, size)
  )
  persons$person <- seq_len(nrow(persons))
  schedule <- expand.grid(panel = 1:2, wave = 1:4)
  schedule$active <- schedule$panel == (schedule$wave - 1) %% 2 + 1
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 8) |>
    add_stage() |> cluster_by(dwelling) |> draw(n = 2) |>
    add_stage() |> draw(n = 1)
  master <- execute(design, eas, stages = 1, panels = schedule, seed = 1)
  for (t in 1:3) {
    master <- execute(master, persons, wave = t, seed = t + 5, stages = 2:3)
  }
  third <- execute(master, wave = 3)
  expect_equal(unique(third$.weight_2), 5)
  expect_equal(unique(third$.fpc_2), 10)
})

test_that("a tenure needs named units drawn with equal probabilities", {
  for (stage_2 in list(
    sampling_design() |> add_stage() |> draw(n = 4),
    sampling_design() |> add_stage() |> cluster_by(hh) |>
      draw(n = 4, method = "pps_brewer", mos = size)
  )) {
    design <- sampling_design() |>
      add_stage() |> stratify_by(h) |> cluster_by(ea) |> draw(n = 9)
    design$stages[[2]] <- stage_2$stages[[1]]
    expect_error(
      execute(design, tenure_eas(), stages = 1, seed = 1,
              panels = tenure_schedule()),
      class = "samplyr_error_tenure_stages"
    )
  }
})

test_that("permanent units hold one tenure from the first wave drawn", {
  eas <- tenure_eas()
  eas$size[c(1, 31)] <- 400
  hh <- data.frame(ea = rep(eas$ea, eas$size))
  hh$hh <- seq_len(nrow(hh))
  master <- suppressMessages(
    sampling_design() |>
      add_stage() |>
      stratify_by(h) |>
      cluster_by(ea) |>
      draw(n = 9, method = "pps_sps", mos = size) |>
      add_stage() |>
      cluster_by(hh) |>
      draw(n = 4) |>
      execute(eas, stages = 1, seed = 2, panels = tenure_schedule())
  )
  sure <- master$ea[master$.certainty_1]
  expect_setequal(sure, c(1L, 31L))

  master <- execute(master, hh[hh$ea %in% master$ea, ], wave = 1, seed = 3)
  drawn <- attr(master, "metadata")$per_wave_draws[[1]]$rows
  expect_true(all(sure %in% drawn$ea))
  for (t in 2:3) {
    used <- unlist(lapply(
      attr(master, "metadata")$per_wave_draws, function(d) d$rows$hh
    ))
    master <- execute(master, hh[hh$ea %in% master$ea & !hh$hh %in% used, ],
                      wave = t, seed = 2 + t)
  }

  # Each certainty EA's own panel is active at one of these waves.
  seeds <- list(3L, 3:4, c(3L, 5L))
  for (t in 1:3) {
    rows <- execute(master, wave = t)
    expect_identical(summary(rows)$seed, seeds[[t]])
    rows <- rows[rows$ea %in% sure, ]
    expect_setequal(unique(rows$ea), sure)
    expect_true(all(rows$hh %in% drawn$hh), info = t)
    expect_true(all(is.na(rows$.visit)), info = t)
  }
})

test_that("a schedule with one tenure per panel completes", {
  schedule <- tenure_schedule()[c("panel", "wave", "active")]
  master <- tenure_master(schedule)
  for (t in 1:3) {
    master <- execute(master, tenure_listing(master), wave = t, seed = 10 + t)
  }
  expect_length(remaining_stages(master), 0L)
  expect_null(attr(master, "metadata")$per_wave_draws[[1]]$rows)
  expect_identical(unique(execute(master, wave = 4)$.visit), 2L)
})

## Export of a wave whose master is still being drawn

test_that("a drawn wave exports by ultimate clusters from its own rows", {
  skip_if_not_installed("survey")
  master <- draw_tenures(tenure_master(), 1:3)
  fourth <- execute(master, wave = 4)
  expect_true(attr(fourth, "metadata")[["wave"]]$provisional)

  svy <- as_svydesign(fourth)
  reference <- survey::svydesign(
    ids = ~ea,
    strata = ~h,
    weights = ~.weight,
    nest = TRUE,
    data = as.data.frame(fourth)
  )
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, reference)))
  )
  expect_s3_class(as_svrepdesign(fourth), "svyrep.design")

  expect_error(as_svydesign(fourth, method = "full"),
               class = "samplyr_error_wave_provisional")
  expect_error(stack_waves(fourth, execute(master, wave = 1)),
               class = "samplyr_error_wave_provisional")
  expect_error(write_design(fourth, withr::local_tempfile(fileext = ".json")),
               class = "samplyr_error_serialize_wave_provisional")

  # A master with tenures is never complete, so the refusals say that rather
  # than that some stages are not drawn yet.
  # Waves 1 to 3 and 7 to 9 open every tenure of the schedule.
  master <- draw_tenures(tenure_master(), c(1:3, 7:9))
  first <- execute(master, wave = 1)
  expect_error(stack_waves(first, execute(master, wave = 4)),
               "the master is never complete",
               class = "samplyr_error_wave_provisional")
  expect_error(write_design(first, withr::local_tempfile(fileext = ".json")),
               "such a master is never complete",
               class = "samplyr_error_serialize_wave_provisional")
})

test_that("a window pools the drawn waves of an unfinished master", {
  skip_if_not_installed("survey")
  master <- draw_tenures(tenure_master(), 1:3)
  quarter <- execute(master, wave = 1:3)
  expect_identical(attr(quarter, "metadata")$wave$window, "union")
  singles <- lapply(1:3, function(t) execute(master, wave = t))
  expect_setequal(quarter$hh, unlist(lapply(singles, `[[`, "hh")))
  # Every panel is in the quarter, so each EA keeps its own weight.
  draws <- attr(master, "metadata")$per_wave_draws
  rows <- dplyr::bind_rows(lapply(draws, `[[`, "rows"))
  expect_equal(quarter$.weight, rows$.weight[match(quarter$hh, rows$hh)])
  expect_s3_class(as_svydesign(quarter), "survey.design2")
})

## The frame digest of a wave

test_that("a drawn wave's digest holds the tenures it reads", {
  master <- draw_tenures(tenure_master(), c(1:3, 7))
  fourth <- execute(master, wave = 4)
  digest <- get_frame_digest(fourth)
  expect_length(digest$stages, 2L)

  # Stage 1 is the master's, stage 2 the pools of wave 4's EAs alone.
  expect_identical(digest$stages[[1]], get_frame_digest(master)$stages[[1]])
  pools <- digest$stages[[2]]$pools
  expect_identical(nrow(pools), length(unique(fourth$ea)))
  expect_identical(sum(pools$n_realized), nrow(fourth))
  keys <- samplyr:::digest_path_keys(
    as.data.frame(fourth), seq_len(nrow(fourth)), c("ea", "hh")
  )
  expect_setequal(digest$stages[[2]]$selected$key, keys)
  expect_s3_class(frame_summary(fourth, detail = "pool"), "tbl_df")

  # A later tenure of the same EAs is a different set of pools.
  seventh <- get_frame_digest(execute(master, wave = 7))
  expect_false(identical(
    seventh$stages[[2]]$selected$key, digest$stages[[2]]$selected$key
  ))
})

test_that("a window over two tenures of a unit keys their pools apart", {
  master <- draw_tenures(tenure_master(), c(1:3, 7))
  quarter <- execute(master, wave = 1:3)
  expect_identical(
    nrow(get_frame_digest(quarter)$stages[[2]]$pools),
    length(unique(quarter$ea))
  )

  # Panel 1 is read at wave 4 from its first tenure and at 7 from its second.
  across <- get_frame_digest(execute(master, wave = 4:7))
  expect_identical(across$version, 3L)
  pools <- across$stages[[2]]$pools
  expect_setequal(unique(pools$tenure), c(1L, 2L, 3L, 7L))
  expect_identical(
    sort(pools$parent_unit[pools$tenure == 1L]),
    sort(pools$parent_unit[pools$tenure == 7L])
  )

  none <- execute(tenure_design(), tenure_eas(), stages = 1, seed = 1,
                  panels = tenure_schedule(), frame_digest = "none")
  none <- execute(none, tenure_listing(none), wave = 1, seed = 11)
  err <- tryCatch(frame_summary(execute(none, wave = 1)), error = identity)
  expect_s3_class(err, "samplyr_error_no_digest")
  expect_match(cli::ansi_strip(conditionMessage(err)), "master it was")
})

## The frame digest of a master with tenures

test_that("a master's digest records every tenure drawn so far", {
  master <- draw_tenures(tenure_master(), c(1:3, 7))
  digest <- get_frame_digest(master)
  expect_identical(digest$version, 3L)
  expect_length(digest$stages, 2L)

  draws <- attr(master, "metadata")$per_wave_draws
  pools <- digest$stages[[2]]$pools
  for (d in draws) {
    here <- pools[pools$tenure == d$wave, ]
    expect_identical(nrow(here), length(unique(d$rows$ea)), info = d$wave)
    expect_identical(sum(here$n_realized), nrow(d$rows), info = d$wave)
  }

  stage <- frame_summary(master, detail = "stage")
  expect_identical(stage$tenure, c(NA, 1L, 2L, 3L, 7L))
  expect_identical(
    stage$n_realized[-1],
    vapply(draws, function(d) as.double(nrow(d$rows)), numeric(1))
  )
  expect_true("tenure" %in% names(frame_summary(master, detail = "pool")))

  # One tenure per panel needs no key, and keeps the version earlier
  # builds read.
  one <- tenure_master(tenure_schedule()[c("panel", "wave", "active")])
  for (t in 1:3) {
    one <- execute(one, tenure_listing(one), wave = t, seed = 10 + t)
  }
  expect_identical(get_frame_digest(one)$version, 2L)
  expect_false("tenure" %in% names(frame_summary(one, detail = "stage")))
})

test_that("a later tenure's digest records the units left out", {
  # Household numbers restart in each type of an EA, and region comes down
  # from the EAs, so the pools are found by both.
  eas <- data.frame(ea = 1:12, region = rep(c("A", "B"), each = 6))
  hh <- expand.grid(ea = 1:12, type = c("a", "b"), hh = 1:10,
                    stringsAsFactors = FALSE)
  schedule <- expand.grid(panel = 1:2, wave = 1:4)
  schedule$active <- schedule$panel == (schedule$wave - 1) %% 2 + 1
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  design <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(ea) |> draw(n = 4) |>
    add_stage() |> stratify_by(type) |> cluster_by(hh) |> draw(n = 2)
  master <- execute(design, eas, stages = 1, panels = schedule, seed = 1,
                    frame_digest = "full")
  listings <- list()
  for (t in 1:4) {
    listings[[t]] <- hh[hh$ea %in% master$ea, ]
    master <- execute(master, listings[[t]], wave = t, seed = t + 5)
  }

  # Each pool of 10 households: a second tenure leaves out the 2 its
  # first drew and draws 2 of the 8 left.
  pools <- get_frame_digest(master)$stages[[2]]$pools
  first <- pools$tenure %in% 1:2
  expect_identical(pools$n_excluded[first], rep(0L, sum(first)))
  expect_identical(pools$n_excluded[!first], rep(2L, sum(!first)))
  expect_identical(pools$N + pools$n_excluded, rep(10L, nrow(pools)))
  stage <- frame_summary(master, detail = "stage")
  expect_identical(stage$n_excluded, c(NA, 0, 0, 16, 16))

  # The recorded chances are the draw's, and rescaled to the listing
  # they are the chances the weights use.
  wave <- execute(master, wave = 3)
  units <- frame_summary(wave, detail = "unit", stages = 2)
  pool_rows <- frame_summary(wave, detail = "pool", stages = 2)
  expect_identical(unique(units$chance), 0.25)
  at <- match(units$pool_id, pool_rows$pool_id)
  listed <- units$chance * pool_rows$N[at] /
    (pool_rows$N[at] + pool_rows$n_excluded[at])
  expect_equal(unique(listed), unique(1 / wave$.weight_2))
  printed <- cli::ansi_strip(capture.output(print(summary(wave))))
  expect_true(any(grepl(
    "N_h 8 of 10 listed, n_h 2, f_h 0.2500 of eligible, 0.2000 of listed",
    printed, fixed = TRUE
  )))
  # The digest's first stage is the master's draw, and the wave holds two of
  # the four EAs of each region.
  expect_true("* 2 strata: N_h 6, n_h 2, f_h 0.3333" %in% printed)
  printed <- cli::ansi_strip(capture.output(print(summary(
    execute(master, wave = 1)
  ))))
  expect_false(any(grepl("listed", printed, fixed = TRUE)))

  path <- withr::local_tempfile(fileext = ".json")
  write_design(master, path, frame = c(list(eas), listings))
  saved <- attr(read_design(path), "execution")$frame_digest
  expect_identical(saved$stages[[2]]$pools$n_excluded, pools$n_excluded)
})

test_that("the digest's version and its tenure key go together", {
  digest <- get_frame_digest(draw_tenures(tenure_master(), 1:2))
  older <- digest
  older$version <- 2L
  expect_error(
    samplyr:::validate_frame_digest(older),
    class = "samplyr_error_digest_version"
  )
  newer <- digest
  newer$version <- 4L
  expect_error(
    samplyr:::validate_frame_digest(newer),
    class = "samplyr_error_digest_version"
  )
  bad <- digest
  bad$stages[[2]]$pools$tenure[1] <- 0L
  expect_error(
    samplyr:::validate_frame_digest(bad),
    class = "samplyr_error_digest_field"
  )
})

## A design file

test_that("a master with tenures replays from its design file", {
  master <- draw_tenures(tenure_master(), 1:3)
  path <- withr::local_tempfile(fileext = ".json")
  frames <- list(tenure_eas(), tenure_households())
  record <- attr(master, "metadata")$panel_assignment

  master_out <- tenure_master()
  listings <- list()
  for (t in 1:3) {
    listings[[t]] <- tenure_listing(master_out)
    master_out <- execute(master_out, listings[[t]], wave = t, seed = 10 + t)
  }
  write_design(master_out, path, frame = c(list(tenure_eas()), listings))
  replayed <- replay_design(read_design(path), c(list(tenure_eas()), listings))

  expect_identical(
    attr(replayed, "metadata")$panel_assignment$schedule$visit,
    record$schedule$visit
  )
  expect_identical(
    sort(execute(replayed, wave = 4)$hh),
    sort(execute(master_out, wave = 4)$hh)
  )

  # The digest the file keeps holds the tenures, so a replay without frame
  # fingerprints still finds a changed tenure listing.
  expect_identical(
    attr(read_design(path), "execution")$frame_digest$version,
    3L
  )
  bare <- withr::local_tempfile(fileext = ".json")
  suppressWarnings(write_design(master_out, bare))
  changed <- listings
  drawn_at_2 <- master_out$ea[master_out$.panel == 2L]
  changed[[2]] <- changed[[2]][-match(drawn_at_2[1], changed[[2]]$ea), ]
  expect_warning(
    replay_design(read_design(bare), c(list(tenure_eas()), changed)),
    class = "samplyr_warning_replay_digest"
  )
})

## Units missing from a tenure's listing

# Two strata of six EAs, six drawn per stratum, then two households per EA.
missing_design <- function(on_empty = "error") {
  sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(ea) |>
    draw(n = 6) |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 2, on_empty = on_empty)
}

missing_master <- function(schedule) {
  eas <- data.frame(ea = 1:12, region = rep(c("A", "B"), each = 6))
  execute(missing_design(), eas, stages = 1, panels = schedule, seed = 1,
          frame_digest = "none")
}

missing_households <- function() {
  data.frame(ea = rep(1:12, each = 10), hh = 1:120)
}

# One household per EA among those a wave drew.
one_per_ea <- function(master, wave) {
  rows <- attr(master, "metadata")$per_wave_draws
  rows <- rows[[which(vapply(rows, `[[`, 1, "wave") == wave)]]$rows
  unname(tapply(rows$hh, rows$ea, min))
}

# The value of a draw and the messages of its listing warnings.
listing_warnings <- function(expr) {
  messages <- character(0)
  value <- withCallingHandlers(
    expr,
    samplyr_warning_tenure_listing = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = messages)
}

two_panels <- function(waves) {
  schedule <- expand.grid(panel = 1:2, wave = waves)
  schedule$active <- schedule$panel == (schedule$wave - 1) %% 2 + 1
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  schedule
}

test_that("a unit missing from its last tenure warns once", {
  hh <- missing_households()
  master <- missing_master(two_panels(1:7))
  master <- execute(master, hh, wave = 1, seed = 2)
  master <- execute(master, hh, wave = 2, seed = 3)
  gone <- one_per_ea(master, 1)
  listing <- hh[!hh$hh %in% gone, ]

  third <- listing_warnings(execute(master, listing, wave = 3, seed = 4))
  expect_length(third$messages, 1L)
  expect_match(third$messages, "lacks 6 units that earlier tenures drew, in 6",
               fixed = TRUE)
  master <- third$value
  master <- execute(master, listing, wave = 4, seed = 5)

  # The same units, gone at the next tenure too, warn no more.
  quiet <- listing_warnings(execute(master, listing, wave = 5, seed = 6))
  expect_length(quiet$messages, 0L)

  # Units of the third wave gone since warn afresh, and only they.
  newly <- one_per_ea(master, 3)
  fifth <- listing_warnings(
    execute(master, listing[!listing$hh %in% newly, ], wave = 5, seed = 6)
  )
  expect_length(fifth$messages, 1L)
  expect_match(fifth$messages, "lacks 6 units", fixed = TRUE)
  master <- execute(fifth$value, listing[!listing$hh %in% newly, ],
                    wave = 6, seed = 7)
  seventh <- listing_warnings(
    execute(master, listing[!listing$hh %in% newly, ], wave = 7, seed = 8)
  )
  expect_length(seventh$messages, 0L)

  # Exclusion still covers every earlier tenure.
  drawn <- execute(seventh$value, wave = 7)$hh
  expect_false(any(drawn %in% c(gone, newly, one_per_ea(master, 1))))
})

test_that("a listing cut of the last tenure warns, or stops when asked", {
  hh <- missing_households()
  master <- missing_master(two_panels(1:3))
  master <- execute(master, hh, wave = 1, seed = 2)
  master <- execute(master, hh, wave = 2, seed = 3)
  cut <- hh[!hh$hh %in% one_per_ea(master, 1), ]
  expect_warning(
    execute(master, cut, wave = 3, seed = 4),
    class = "samplyr_warning_tenure_listing"
  )
  err <- expect_error(
    withCallingHandlers(
      execute(master, cut, wave = 3, seed = 4),
      samplyr_warning_tenure_listing = function(w) {
        rlang::abort(conditionMessage(w), parent = w)
      }
    )
  )
  expect_s3_class(err$parent, "samplyr_warning_tenure_listing")
})

test_that("each primary unit is checked against its own latest tenure", {
  hh <- missing_households()
  # Out of wave order: the fifth wave is drawn before the third.
  master <- missing_master(two_panels(1:6))
  master <- execute(master, hh, wave = 1, seed = 2)
  master <- execute(master, hh, wave = 5, seed = 3)
  older <- hh[!hh$hh %in% one_per_ea(master, 1), ]
  expect_length(
    listing_warnings(execute(master, older, wave = 3, seed = 4))$messages,
    0L
  )
  latest <- hh[!hh$hh %in% one_per_ea(master, 5), ]
  expect_length(
    listing_warnings(execute(master, latest, wave = 3, seed = 4))$messages,
    1L
  )

  # Panels 1 and 3 open together at wave 3, after tenures drawn at
  # different waves. Both lose a unit of their own latest tenure.
  schedule <- expand.grid(panel = 1:3, wave = 1:3)
  schedule$active <- with(schedule, (wave == 1 & panel <= 2) |
                            (wave == 2 & panel == 3) |
                            (wave == 3 & panel != 2))
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  master <- missing_master(schedule)
  master <- execute(master, hh, wave = 1, seed = 2)
  master <- execute(master, hh, wave = 2, seed = 3)
  first <- one_per_ea(master, 1)
  first <- first[first %in% hh$hh[hh$ea %in% master$ea[master$.panel == 1L]]]
  gone <- c(first, one_per_ea(master, 2))
  out <- listing_warnings(
    execute(master, hh[!hh$hh %in% gone, ], wave = 3, seed = 4)
  )
  expect_length(out$messages, 1L)
  expect_match(out$messages,
               paste("lacks", length(gone), "units"), fixed = TRUE)
})

test_that("a tenure that drew no unit is still the latest", {
  # An EA of two households, both drawn at wave 1.
  eas <- data.frame(ea = 1:12, region = rep(c("A", "B"), each = 6))
  master <- execute(missing_design(on_empty = "silent"), eas, stages = 1, panels = two_panels(1:5),
                    seed = 1, frame_digest = "none")
  small <- master$ea[master$.panel == 1L][1]
  sizes <- ifelse(1:12 == small, 2L, 10L)
  hh <- data.frame(ea = rep(1:12, sizes), hh = seq_len(sum(sizes)))
  master <- execute(master, hh, wave = 1, seed = 2)
  master <- execute(master, hh, wave = 2, seed = 3)
  listing <- hh[hh$ea != small, ]

  # Both households dissolved, so wave 3 has none to draw. An EA whose
  # households were drawn before and are still listed is refused instead.
  third <- listing_warnings(execute(master, listing, wave = 3, seed = 4))
  master <- third$value
  expect_false(small %in% execute(master, wave = 3)$ea)
  master <- execute(master, listing, wave = 4, seed = 5)

  # The EA is listed again with one household of wave 1 and two that moved
  # in. Wave 3 is its latest tenure, so the other household of wave 1 does
  # not warn again.
  back <- rbind(
    listing,
    hh[hh$hh == min(hh$hh[hh$ea == small]), ],
    data.frame(ea = small, hh = max(hh$hh) + 1:2)
  )
  fifth <- listing_warnings(execute(master, back, wave = 5, seed = 6))
  expect_length(fifth$messages, 0L)
  wave5 <- execute(fifth$value, wave = 5)
  expect_setequal(wave5$hh[wave5$ea == small], max(hh$hh) + 1:2)
})
