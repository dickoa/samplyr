## Export equivalence
#
# Each case compares an export against an oracle built without samplyr's
# export code: a textbook formula, a hand-written survey design, or another
# route that must agree. The invariants are in helper-export-equivalence.R.

## Single-stage SRS against the textbook variance

test_that("an SRS total and variance equal the closed form on every route", {
  skip_if_not_installed("survey")
  set.seed(1)
  frame <- data.frame(id = 1:60, y = round(stats::rnorm(60, 50, 10), 1))
  sample <- sampling_design() |> draw(n = 15) |> execute(frame, seed = 3)

  n <- 15
  N <- 60
  closed_form <- list(
    total = N * mean(sample$y),
    variance = N^2 * (1 - n / N) * stats::var(sample$y) / n
  )

  expect_export_invariants(
    as_svydesign(sample), sample, "y",
    reference = closed_form, stages = 1
  )
  # JK1 with the fpc reproduces the SRSWOR variance exactly.
  expect_export_invariants(
    as_svrepdesign(sample, type = "JK1"), sample, "y",
    reference = closed_form
  )
})

test_that("the srvyr route is the survey route", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  set.seed(1)
  frame <- data.frame(id = 1:60, y = round(stats::rnorm(60, 50, 10), 1))
  sample <- sampling_design() |> draw(n = 15) |> execute(frame, seed = 3)

  expect_export_invariants(
    srvyr::as_survey_design(sample), sample, "y",
    reference = as_svydesign(sample), stages = 1
  )
})

## PSU SRS then a stratified element stage, against a hand-built design

test_that("a stratified second stage matches svydesign() written by hand", {
  skip_if_not_installed("survey")
  # Stratum means 100 apart put a pooled second-stage stratum far off.
  pop <- expand.grid(
    e = 1:10, h = c("a", "b"), psu = 1:24,
    stringsAsFactors = FALSE
  )
  pop$id <- seq_len(nrow(pop))
  pop$y <- 100 * (pop$h == "b") + pop$psu + pop$e

  sample <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 8) |>
    add_stage() |>
    stratify_by(h) |>
    draw(n = 3) |>
    execute(pop, seed = 5)

  df <- as.data.frame(sample)
  df$one <- 1
  df$N1 <- 24
  df$N2 <- 10
  by_hand <- survey::svydesign(
    ids = ~ psu + id,
    strata = ~ one + h,
    fpc = ~ N1 + N2,
    data = df
  )

  expect_export_invariants(
    as_svydesign(sample), sample, "y",
    reference = by_hand, stages = 2
  )
})

## Certainty units at a later PPS stage

# Stage 2 is Brewer within each PSU, with two certainty units per PSU.
# `balanced` puts their values near the probability units' mean, otherwise
# they are far from it.
late_certainty_design <- function(balanced) {
  set.seed(if (balanced) 5 else 4)
  sizes <- if (balanced) rep(12L, 40) else sample(8:14, 40, replace = TRUE)
  frame <- data.frame(psu = rep(1:40, sizes))
  frame$eid <- seq_len(nrow(frame))
  if (balanced) {
    frame$x <- stats::ave(frame$eid, frame$psu, FUN = function(v) {
      c(20, 20, stats::runif(length(v) - 2, 3, 5))
    })
    frame$y <- frame$x + stats::rnorm(nrow(frame), 0, 2)
  } else {
    frame$x <- stats::ave(frame$eid, frame$psu, FUN = function(v) {
      c(60, 40, rep(3, length(v) - 2))
    })
    frame$y <- stats::ave(frame$eid, frame$psu, FUN = function(v) {
      c(500, 300, rep(0, length(v) - 2))
    }) + 0.5 * frame$x + stats::rnorm(nrow(frame), 0, 6)
  }
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = if (balanced) 40 else 10) |>
    add_stage() |>
    draw(n = 4, method = "pps_brewer", mos = x)
  list(frame = frame, design = design)
}

test_that("later-stage certainty units form their own stratum", {
  skip_if_not_installed("survey")
  for (balanced in c(FALSE, TRUE)) {
    case <- late_certainty_design(balanced)
    sample <- execute(case$design, case$frame, seed = 1)
    expect_true(any(sample$.certainty_2))
    expect_false(all(sample$.certainty_2))

  # Element ids follow row order because survey pairs corrections in id order.
    df <- as.data.frame(sample)
    df$eid <- seq_len(nrow(df))
    df$one <- 1
    df$cert2 <- df$.certainty_2
    df$f1 <- 1 / df$.weight_1
    df$pi2 <- 1 / df$.weight_2
    by_hand <- survey::svydesign(
      ids = ~ psu + eid,
      strata = ~ one + cert2,
      weights = ~.weight,
      fpc = ~ f1 + pi2,
      pps = "brewer",
      data = df
    )
    expect_export_invariants(
      as_svydesign(sample), sample, "y",
      reference = by_hand, stages = 2
    )
  }
})

test_that("later-stage certainty gives a variance in the Monte Carlo band", {
  skip_on_cran()
  skip_if_not_installed("survey")
  case <- late_certainty_design(balanced = FALSE)
  # Band fixed before the run. The ratio's standard error is about 0.08.
  mc <- mc_variance_ratio(
    case$design, case$frame, "y",
    reps = 300, export = as_svydesign
  )
  expect_gt(mc$ratio, 0.75)
  expect_lt(mc$ratio, 1.25)
  expect_lt(abs(mc$bias_z), 4)
})

## Stratified PPS stage 1 with certainty PSUs, then SRS

test_that("stage-1 certainty PSUs give a variance in the Monte Carlo band", {
  skip_on_cran()
  skip_if_not_installed("survey")
  set.seed(11)
  psu <- data.frame(
    psu = 1:60,
    st = rep(c("A", "B"), each = 30),
    M = sample(5:15, 60, replace = TRUE)
  )
  psu$x <- round(exp(stats::rnorm(60, 3, 1)))
  # One dominant PSU per stratum, so each stratum carries certainty units.
  psu$x[c(1, 31)] <- c(3000, 2500)
  frame <- psu[rep(seq_len(nrow(psu)), psu$M), ]
  frame$eid <- seq_len(nrow(frame))
  frame$y <- frame$x / frame$M + stats::rnorm(nrow(frame), 0, 3)

  design <- sampling_design() |>
    add_stage() |>
    stratify_by(st) |>
    cluster_by(psu) |>
    draw(n = 6, method = "pps_brewer", mos = x) |>
    add_stage() |>
    draw(n = 3)

  expect_true(any(execute(design, frame, seed = 1)$.certainty_1))

  # Band fixed before the run. The ratio's standard error is about 0.07.
  mc <- mc_variance_ratio(design, frame, "y", reps = 400, export = as_svydesign)
  expect_gt(mc$ratio, 0.75)
  expect_lt(mc$ratio, 1.25)
  expect_lt(abs(mc$bias_z), 4)
})

## Certainty PSUs and a PPS first stage
#
# A certainty PSU is a stratum of its own whose stage-two units are
# resampled. A certainty unit with no stage below keeps its weight. JKn
# replicates are deterministic, so a hand-written design is an exact oracle.

certainty_rep_fixture <- function(stages = 2L) {
  set.seed(20260925)
  psu <- data.frame(
    str = rep(1:3, each = 15),
    psu = 1:45,
    M = sample(20:60, 45, TRUE)
  )
  frame <- psu[rep(seq_len(nrow(psu)), psu$M), ]
  frame$eid <- seq_len(nrow(frame))
  frame$y <- 20 + 0.3 * frame$M + stats::rnorm(nrow(frame), 0, 15)
  design <- if (stages == 2L) {
    sampling_design() |>
      add_stage() |> stratify_by(str) |> cluster_by(psu) |>
      draw(n = 5, method = "pps_brewer", mos = M, certainty_size = 56) |>
      add_stage() |> draw(n = 4)
  } else {
    sampling_design() |>
      stratify_by(str) |>
      draw(n = 8, method = "pps_brewer", mos = M, certainty_size = 58)
  }
  frame <- if (stages == 2L) frame else psu_frame_one_stage(psu)
  list(frame = frame, design = design)
}

psu_frame_one_stage <- function(psu) {
  set.seed(3)
  psu$y <- 5 * psu$M + stats::rnorm(nrow(psu), 0, 20)
  psu
}

# The approximation warning is asserted in test-survey-export.R.
quiet_rep <- function(expr) {
  withCallingHandlers(
    expr,
    samplyr_warning_replicate_wr_first_stage = function(w) {
      invokeRestart("muffleWarning")
    }
  )
}

jkn_se <- function(design) {
  as.numeric(survey::SE(survey::svytotal(
    ~y, survey::as.svrepdesign(design, type = "JKn")
  )))
}

test_that("certainty PSUs' stage-two units are resampled as PSUs", {
  skip_if_not_installed("survey")
  fx <- certainty_rep_fixture(2L)
  s <- execute(fx$design, fx$frame, seed = 11)
  df <- as.data.frame(s)
  expect_gt(sum(df$.certainty_1[!duplicated(df$psu)]), 1L)

  cert <- df$.certainty_1
  df$u <- ifelse(cert, paste("c", df$psu, df$eid), paste("p", df$psu))
  df$h <- ifelse(cert, paste("c", df$psu), paste("s", df$str))
  hand <- survey::svydesign(
    ids = ~u, strata = ~h, weights = ~.weight, data = df, nest = TRUE
  )
  ours <- quiet_rep(as_svrepdesign(s, type = "JKn"))
  expect_equal(
    as.numeric(survey::SE(survey::svytotal(~y, ours))),
    jkn_se(hand),
    tolerance = 1e-8
  )

  for (type in c("bootstrap", "subbootstrap")) {
    se <- as.numeric(survey::SE(survey::svytotal(
      ~y, quiet_rep(as_svrepdesign(s, type = type, replicates = 50))
    )))
    expect_true(is.finite(se) && se > 0, label = type)
  }
})

test_that("a certainty unit with no stage below keeps its weight", {
  skip_if_not_installed("survey")
  fx <- certainty_rep_fixture(1L)
  s <- execute(fx$design, fx$frame, seed = 1)
  df <- as.data.frame(s)
  cert <- df$.certainty_1
  expect_true(any(cert))
  expect_true(all(tapply(!cert, df$str, sum) >= 2L))

  hand <- survey::svydesign(
    ids = ~1, strata = ~str, weights = ~.weight, data = df[!cert, ]
  )
  ours <- quiet_rep(as_svrepdesign(s, type = "JKn"))
  expect_equal(
    as.numeric(survey::SE(survey::svytotal(~y, ours))),
    jkn_se(hand),
    tolerance = 1e-8
  )

  for (type in c("JKn", "bootstrap", "subbootstrap")) {
    rep <- quiet_rep(as_svrepdesign(s, type = type, replicates = 30))
    w <- stats::weights(rep, type = "analysis")
    expect_true(
      all(abs(w[cert, , drop = FALSE] - df$.weight[cert]) < 1e-12),
      label = type
    )
  }
})

test_that("certainty PSUs' units are resampled within their stage-two strata", {
  skip_if_not_installed("survey")
  fx <- certainty_rep_fixture(2L)
  frame <- fx$frame
  frame$g <- ifelse(frame$eid %% 2 == 0, "even", "odd")
  design <- sampling_design() |>
    add_stage() |> stratify_by(str) |> cluster_by(psu) |>
    draw(n = 5, method = "pps_brewer", mos = M, certainty_size = 56) |>
    add_stage() |> stratify_by(g) |> draw(n = 2)
  s <- execute(design, frame, seed = 11)
  df <- as.data.frame(s)
  cert <- df$.certainty_1
  expect_gt(sum(cert[!duplicated(df$psu)]), 1L)

  df$u <- ifelse(cert, paste("c", df$psu, df$eid), paste("p", df$psu))
  df$h <- ifelse(cert, paste("c", df$psu, df$g), paste("s", df$str))
  hand <- survey::svydesign(
    ids = ~u, strata = ~h, weights = ~.weight, data = df, nest = TRUE
  )
  expect_equal(
    as.numeric(survey::SE(survey::svytotal(
      ~y, quiet_rep(as_svrepdesign(s, type = "JKn"))
    ))),
    jkn_se(hand),
    tolerance = 1e-8
  )
})

test_that("a spatial first stage's certainty units keep their weight", {
  skip_if_not_installed("survey")
  # Not a PPS family, so only the certainty flag sends it to this route.
  set.seed(8)
  frame <- data.frame(
    id = 1:120,
    x = c(900, 800, stats::runif(118, 5, 40)),
    lon = stats::runif(120),
    lat = stats::runif(120),
    y = stats::rnorm(120)
  )
  s <- sampling_design() |>
    draw(n = 20, method = "lpm2", mos = x, spread = c(lon, lat)) |>
    execute(frame, seed = 1)
  df <- as.data.frame(s)
  cert <- df$.certainty_1
  expect_identical(sum(cert), 2L)

  rep <- quiet_rep(as_svrepdesign(s, type = "subbootstrap", replicates = 30))
  w <- stats::weights(rep, type = "analysis")
  expect_true(all(abs(w[cert, , drop = FALSE] - df$.weight[cert]) < 1e-12))
  expect_false(all(abs(w[!cert, , drop = FALSE] - df$.weight[!cert]) < 1e-12))
})

test_that("JKn of a PPS first stage is the with-replacement jackknife", {
  skip_if_not_installed("survey")
  fx <- certainty_rep_fixture(2L)
  design <- sampling_design() |>
    add_stage() |> stratify_by(str) |> cluster_by(psu) |>
    draw(n = 5, method = "pps_brewer", mos = M) |>
    add_stage() |> draw(n = 4)
  s <- execute(design, fx$frame, seed = 11)
  df <- as.data.frame(s)
  expect_false(any(df$.certainty_1))
  hand <- survey::svydesign(
    ids = ~psu, strata = ~str, weights = ~.weight, data = df, nest = TRUE
  )
  expect_equal(
    as.numeric(survey::SE(survey::svytotal(
      ~y, quiet_rep(as_svrepdesign(s, type = "JKn"))
    ))),
    jkn_se(hand),
    tolerance = 1e-8
  )
})

test_that("BRR of a PPS first stage is BRR of the with-replacement design", {
  skip_if_not_installed("survey")
  # survey pairs strata with Hadamard rows in the sorted order of their
  # labels, so the strata appear here in an order that is not sorted.
  set.seed(6)
  psu <- data.frame(
    st = rep(c("b", "a", "c"), each = 12),
    psu = 1:36,
    M = sample(20:40, 36, TRUE)
  )
  frame <- psu[rep(seq_len(nrow(psu)), each = 6), ]
  frame$y <- stats::rnorm(nrow(frame), frame$M)
  s <- sampling_design() |>
    add_stage() |> stratify_by(st) |> cluster_by(psu) |>
    draw(n = 2, method = "pps_brewer", mos = M) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 2)
  df <- as.data.frame(s)
  hand <- survey::svydesign(
    ids = ~psu, strata = ~st, weights = ~.weight, data = df, nest = TRUE
  )

  for (type in c("BRR", "Fay")) {
    ours <- quiet_rep(as_svrepdesign(s, type = type))
    theirs <- survey::as.svrepdesign(hand, type = type)
    expect_identical(
      unname(stats::weights(ours, type = "analysis")),
      unname(stats::weights(theirs, type = "analysis")),
      label = type
    )
  }
})

test_that("RWYB takes a final-stage singleton as certainty on request", {
  skip_if_not_installed("svrep")
  fx <- certainty_rep_fixture(2L)
  one_each <- sampling_design() |>
    add_stage() |> stratify_by(str) |> cluster_by(psu) |> draw(n = 4) |>
    add_stage() |> draw(n = 1)
  s <- suppressMessages(execute(one_each, fx$frame, seed = 1))
  expect_error(
    as_svrepdesign(s, type = "rwyb", replicates = 20),
    class = "samplyr_error_rwyb_singleton"
  )
  rep <- as_svrepdesign(s, type = "rwyb", replicates = 20,
                        lonely.psu = "certainty")
  # One unit per PSU at the final stage, so its replicate factor is the PSU's.
  w <- stats::weights(rep, type = "analysis")
  factors <- w / as.data.frame(s)$.weight
  expect_equal(
    unname(factors),
    unname(factors[match(s$psu, s$psu), , drop = FALSE])
  )
  expect_error(
    as_svrepdesign(s, type = "rwyb", replicates = 20, lonely.psu = "adjust"),
    class = "samplyr_error_rwyb_input"
  )

  # A singleton above the final stage has a stage below it and is refused.
  top <- sampling_design() |>
    add_stage() |> stratify_by(str) |> cluster_by(psu) |>
    draw(n = c(`1` = 1, `2` = 3, `3` = 3)) |>
    add_stage() |> draw(n = 4)
  s_top <- suppressMessages(execute(top, fx$frame, seed = 1))
  expect_error(
    as_svrepdesign(s_top, type = "rwyb", replicates = 20,
                   lonely.psu = "certainty"),
    class = "samplyr_error_rwyb_singleton"
  )
})

## A wave that activates every panel is the master's own design

test_that("a wave activating every panel exports as its master", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:400,
    region = rep(c("N", "S"), each = 200),
    value = (1:400) / 4
  )
  schedule <- data.frame(
    panel = rep(1:2, times = 2),
    wave = rep(1:2, each = 2),
    active = c(TRUE, TRUE, TRUE, FALSE)
  )
  master <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 60) |>
    execute(frame, seed = 42, panels = schedule)
  wave <- execute(master, wave = 1)

  expect_identical(nrow(wave), nrow(master))
  expect_export_invariants(
    as_svydesign(wave), wave, "value",
    reference = as_svydesign(master), stages = 1
  )
})

## A selected primary unit left with no row
#
# A PSU with nothing eligible under it has no row, so survey would run the
# between-PSU variance over the others. The export refuses instead.

empty_psu_areas <- function() data.frame(psu = 1:4)
empty_psu_roster <- function() data.frame(psu = 2:4, id = 2:4, y = 10)

test_that("an empty primary unit is refused where survey would drop it", {
  skip_if_not_installed("survey")
  design <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage() |>
    draw(frac = 1, on_empty = "silent")
  selected <- execute(design, empty_psu_areas(), stages = 1, seed = 1)
  expect_identical(selected$psu, c(1L, 3L, 4L))
  sample <- execute(selected, empty_psu_roster(), seed = 2)
  # PSU totals 0, 10, 10: the SRS variance is 44.44, survey's would be 0.
  totals <- c(0, 10, 10)
  expect_equal(4^2 * (1 - 3 / 4) * stats::var(totals) / 3, 400 / 9)

  cnd <- expect_error(
    as_svydesign(sample),
    class = "samplyr_error_export_empty_psu"
  )
  expect_identical(cnd$n_units, 1L)
  expect_identical(condition_header(cnd), "as_svydesign")
  for (type in c("bootstrap", "JK1", "subbootstrap")) {
    expect_error(
      as_svrepdesign(sample, type = type, replicates = 20),
      class = "samplyr_error_export_empty_psu"
    )
  }
  skip_if_not_installed("svrep")
  expect_error(
    as_svrepdesign(sample, type = "rwyb"),
    class = "samplyr_error_rwyb_missing_parents"
  )
})

test_that("a primary unit emptied further down is refused, a thinned one is not", {
  skip_if_not_installed("survey")
  psus <- data.frame(psu = 1:4)
  dwellings <- data.frame(psu = rep(1:4, each = 2), dw = 1:8)
  design <- sampling_design() |>
    cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> cluster_by(dw) |> draw(frac = 1) |>
    add_stage() |> draw(frac = 1, on_empty = "silent")

  # PSU 1 loses both dwellings, so it has no row at all.
  people <- data.frame(dw = 3:8, pid = 3:8, y = 10)
  people$psu <- dwellings$psu[people$dw]
  gone <- execute(design, list(psus, dwellings, people), seed = 1)
  expect_true(1L %in% vapply(
    attr(gone, "metadata")$empty_parents, function(r) r$keys$psu[1], 1L
  ))
  expect_false(1L %in% gone$psu)
  expect_error(as_svydesign(gone), class = "samplyr_error_export_empty_psu")

  # PSU 1 keeps one dwelling: its total is right and the export warns.
  people <- data.frame(dw = 2:8, pid = 2:8, y = 10)
  people$psu <- dwellings$psu[people$dw]
  thinned <- execute(design, list(psus, dwellings, people), seed = 1)
  expect_warning(
    as_svydesign(thinned),
    class = "samplyr_warning_export_empty_parent"
  )
})

test_that("an empty primary unit is refused within a stratum", {
  skip_if_not_installed("survey")
  areas <- data.frame(psu = 1:8, st = rep(1:2, each = 4))
  roster <- data.frame(psu = 2:8, y = c(10, 10, 10, 1, 2, 3, 4))
  roster$id <- seq_len(nrow(roster))
  roster$st <- areas$st[roster$psu]
  sample <- sampling_design() |>
    stratify_by(st) |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage() |>
    draw(frac = 1, on_empty = "silent") |>
    execute(list(areas, roster), seed = 1)
  expect_false(1L %in% sample$psu)
  cnd <- expect_error(
    as_svydesign(sample),
    class = "samplyr_error_export_empty_psu"
  )
  expect_identical(cnd$n_units, 1L)
})

test_that("an empty primary unit in phase 1 is refused at two-phase export", {
  skip_if_not_installed("survey")
  roster <- data.frame(psu = rep(2:4, each = 5), id = 1:15, y = 1:15)
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> cluster_by(id) |> draw(frac = 1, on_empty = "silent") |>
    execute(list(empty_psu_areas(), roster), seed = 1)
  expect_false(1L %in% phase1$psu)
  phase2 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 5) |>
    execute(phase1, seed = 2)
  expect_error(
    quiet_across(as_svydesign(phase2)),
    class = "samplyr_error_export_empty_psu"
  )
})

test_that("RWYB refuses an empty parent without a frame digest", {
  skip_if_not_installed("svrep")
  # The digest is optional, so the refusal reads the recorded empty parents.
  psus <- data.frame(psu = 1:4)
  dwellings <- data.frame(psu = rep(1:4, each = 2), dw = 1:8)
  design <- sampling_design() |>
    cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> cluster_by(dw) |> draw(frac = 1) |>
    add_stage() |> draw(frac = 1, on_empty = "silent")
  for (from in list(3:8, 2:8)) {
    people <- data.frame(dw = from, pid = from, y = 10)
    people$psu <- dwellings$psu[people$dw]
    sample <- execute(
      design, list(psus, dwellings, people), seed = 1, frame_digest = "none"
    )
    expect_null(get_frame_digest(sample))
    expect_error(
      as_svrepdesign(sample, type = "rwyb", replicates = 20),
      class = "samplyr_error_rwyb_missing_parents",
      info = paste("people from dwelling", from[1])
    )
  }
})

test_that("an empty primary unit in phase 2 or in a wave's master is refused", {
  skip_if_not_installed("survey")
  roster <- data.frame(psu = rep(2:4, each = 5), id = 1:15, y = 1:15)
  by_psu <- sampling_design() |>
    cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> cluster_by(id) |> draw(frac = 1, on_empty = "silent")

  whole <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 4) |>
    execute(empty_psu_areas(), seed = 1)
  phase2 <- execute(by_psu, list(whole, roster), seed = 2)
  expect_false(1L %in% phase2$psu)
  expect_error(
    quiet_across(as_svydesign(phase2)),
    class = "samplyr_error_export_empty_psu"
  )

  schedule <- data.frame(
    panel = rep(1:2, 2), wave = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  master <- execute(
    by_psu, list(empty_psu_areas(), roster), seed = 1, panels = schedule
  )
  expect_false(1L %in% master$psu)
  expect_error(
    as_svydesign(execute(master, wave = 1)),
    class = "samplyr_error_export_empty_psu"
  )
})

test_that("a replicate that kept every primary unit still exports", {
  skip_if_not_installed("survey")
  design <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage() |>
    draw(frac = 1, on_empty = "silent")
  reps <- execute(
    design, list(empty_psu_areas(), empty_psu_roster()),
    seed = 4, reps = 6
  )
  has_psu1 <- vapply(split(reps$psu, reps$.replicate), function(p) 1L %in% p, NA)
  records <- attr(reps, "metadata")$empty_parents
  emptied <- unique(vapply(records, function(r) r$replicate, 1L))
  kept <- setdiff(unique(reps$.replicate), emptied)
  expect_gt(length(emptied), 0L)
  expect_gt(length(kept), 0L)
  expect_false(any(has_psu1))

  one <- function(r) reps[reps$.replicate == r, ]
  expect_s3_class(as_svydesign(one(kept[1])), "survey.design")
  expect_error(
    as_svydesign(one(emptied[1])),
    class = "samplyr_error_export_empty_psu"
  )
})

## A frame-stack component is the component's own export

test_that("each dual-frame component equals its standalone export", {
  skip_if_not_installed("survey")
  pop <- data.frame(
    id = 1:60,
    y = as.numeric(1:60),
    in_a = rep(c(TRUE, FALSE), times = c(40, 20)),
    in_b = rep(c(FALSE, TRUE), times = c(20, 40)),
    st = rep(c("x", "y"), 30)
  )
  a <- sampling_design() |>
    stratify_by(st) |>
    draw(n = c(x = 6, y = 4)) |>
    execute(pop[pop$in_a, ], seed = 3)
  b <- sampling_design() |>
    draw(n = 20) |>
    execute(pop[pop$in_b, ], seed = 2)
  stacked <- as_svydesign(stack_frames(
    a = a, b = b,
    membership = c(a = "in_a", b = "in_b"),
    key = id
  ))

  components <- list(a, b)
  for (i in seq_along(components)) {
    expect_export_invariants(
      stacked$designs[[i]], components[[i]], "y",
      reference = as_svydesign(components[[i]]),
      stages = 1
    )
  }
})

## Two-phase and wave export keep every stage's strata
#
# Stratum means 100 apart make a pooled phase move the total, not only the
# variance. Each reference is a hand-written survey::twophase() call.

twophase_strata_population <- function(n_psu = 12) {
  pop <- expand.grid(person = 1:20, h = 1:2, psu = seq_len(n_psu))
  pop$id <- seq_len(nrow(pop))
  pop$y <- 100 * pop$h + pop$person + pop$psu
  pop$g <- rep(c("u", "v"), length.out = nrow(pop))
  pop
}

test_that("phase-1 later-stage strata reach the two-phase export", {
  skip_if_not_installed("survey")
  pop <- twophase_strata_population()
  phase1 <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 8) |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(id) |>
    draw(n = 8) |>
    execute(pop, seed = 8)
  phase2 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 80) |>
    execute(phase1, seed = 9)

  df <- as.data.frame(phase1)
  df$all <- 1
  df$in2 <- df$id %in% phase2$id
  df$N2 <- nrow(df)
  by_hand <- survey::twophase(
    id = list(~ psu + id, ~id),
    strata = list(~ all + h, NULL),
    fpc = list(~ .fpc_1 + .fpc_2, ~N2),
    subset = ~in2,
    data = df
  )

  expect_export_invariants(
    quiet_across(as_svydesign(phase2)), phase2, "y",
    reference = by_hand, stages = c(2, 1)
  )
})

test_that("a with-replacement phase 1 passes its own probabilities", {
  skip_if_not_installed("survey")
  # Its correction is infinite, so survey cannot derive the phase-1 weights
  # from it and the export has to pass them.
  pop <- twophase_strata_population()
  phase1 <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 4, method = "srswr") |>
    execute(pop, seed = 1)
  draws <- unique(as.data.frame(phase1)[c("psu", ".draw_1")])
  expect_false(anyDuplicated(draws$psu) > 0)
  phase2 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 40) |>
    execute(phase1, seed = 9)

  svy <- quiet_across(as_svydesign(phase2))
  expect_s3_class(svy, "twophase2")
  expect_export_invariants(svy, phase2, "y", stages = c(1, 1))
})

test_that("a phase 2 drawn across the phase-1 units warns at export", {
  skip_if_not_installed("survey")
  # Measured: across the units survey's variance was negative in 26% to 51%
  # of samples. Within each unit, or taking whole units, it was right.
  pop <- expand.grid(person = 1:20, psu = 1:60)
  pop$id <- seq_len(nrow(pop))
  pop$y <- stats::rnorm(nrow(pop))
  by_psu <- sampling_design() |> cluster_by(psu) |> draw(n = 12)
  two_stage <- by_psu |> add_stage() |> cluster_by(id) |> draw(n = 8)
  by_element <- sampling_design() |> cluster_by(id) |> draw(n = 240)
  across <- sampling_design() |> cluster_by(id) |> draw(n = 40)
  within <- sampling_design() |>
    stratify_by(psu) |> cluster_by(id) |> draw(n = 3)
  whole <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 6) |>
    add_stage() |> cluster_by(id) |> draw(n = 10)
  export <- function(phase1, phase2) {
    as_svydesign(execute(phase2, execute(phase1, pop, seed = 1), seed = 2))
  }

  for (phase1 in list(by_psu, two_stage)) {
    expect_warning(
      export(phase1, across),
      "stratify_by(psu)",
      fixed = TRUE,
      class = "samplyr_warning_twophase_across_units"
    )
    expect_no_warning(
      export(phase1, within),
      class = "samplyr_warning_twophase_across_units"
    )
  }
  expect_no_warning(
    export(by_psu, whole),
    class = "samplyr_warning_twophase_across_units"
  )
  expect_no_warning(
    export(by_element, across),
    class = "samplyr_warning_twophase_across_units"
  )
})

phase2_strata_reference <- function(phase1, phase2, strata) {
  df <- as.data.frame(phase1)
  df$all <- "all"
  df$hg <- paste(df$h, df$g)
  df$in2 <- df$id %in% phase2$id
  at <- match(df$id, phase2$id)
  df$f2a <- as.data.frame(phase2)$.fpc_1[at]
  df$f2b <- as.data.frame(phase2)$.fpc_2[at]
  survey::twophase(
    id = list(~psu, ~ psu + id),
    strata = list(NULL, strata),
    fpc = list(~.fpc_1, ~ f2a + f2b),
    subset = ~in2,
    data = df
  )
}

test_that("phase-2 later-stage strata reach the two-phase export", {
  skip_if_not_installed("survey")
  pop <- twophase_strata_population()
  phase1 <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 8) |>
    execute(pop, seed = 8)

  one_var <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 6) |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(id) |>
    draw(n = 5) |>
    execute(phase1, seed = 9)
  expect_export_invariants(
    as_svydesign(one_var), one_var, "y",
    reference = phase2_strata_reference(phase1, one_var, ~ all + h),
    stages = c(1, 2)
  )

  # Two stratification variables give a combined term on every phase-1 row.
  two_vars <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 6) |>
    add_stage() |>
    stratify_by(h, g) |>
    cluster_by(id) |>
    draw(n = 3) |>
    execute(phase1, seed = 9)
  expect_export_invariants(
    as_svydesign(two_vars), two_vars, "y",
    reference = phase2_strata_reference(phase1, two_vars, ~ all + hg),
    stages = c(1, 2)
  )
})

test_that("double sampling for stratification on two variables exports", {
  skip_if_not_installed("survey")
  # Phase 2 stratifies on two phase-1 variables, a combined stratum term.
  pop <- twophase_strata_population()
  phase1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 200) |>
    execute(pop, seed = 8)
  phase2 <- sampling_design() |>
    stratify_by(h, g) |>
    cluster_by(id) |>
    draw(n = 10) |>
    execute(phase1, seed = 9)

  df <- as.data.frame(phase1)
  df$hg <- paste(df$h, df$g)
  df$in2 <- df$id %in% phase2$id
  df$n1h <- stats::ave(df$id, df$hg, FUN = length)
  by_hand <- survey::twophase(
    id = list(~id, ~id),
    strata = list(NULL, ~hg),
    fpc = list(~.fpc_1, ~n1h),
    subset = ~in2,
    data = df
  )

  expect_export_invariants(
    as_svydesign(phase2), phase2, "y",
    reference = by_hand, stages = c(1, 1)
  )
})

test_that("a wave of a master with later-stage strata keeps them", {
  skip_if_not_installed("survey")
  pop <- twophase_strata_population(n_psu = 48)
  schedule <- data.frame(
    panel = rep(1:4, times = 4),
    wave = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
  master <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 16) |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(id) |>
    draw(n = 5) |>
    execute(pop, seed = 3, panels = schedule)
  wave <- execute(master, wave = 1)

  # Master stages at phase 1, activation blocks as phase-2 strata.
  record <- attr(master, "metadata")$panel_assignment
  df <- as.data.frame(master)
  keys <- make_group_key(df, record$key_vars)
  df$.block <- NA_character_
  df$.block_n <- NA_real_
  for (p in seq_along(record$pools)) {
    pool <- record$pools[[p]]
    at <- match(keys, pool$keys)
    rows <- which(!is.na(at))
    b <- rep(seq_along(pool$blocks), pool$blocks)[at[rows]]
    df$.block[rows] <- paste(p, b, sep = ".")
    df$.block_n[rows] <- pool$blocks[b]
  }
  df$.unit <- match(keys, unique(keys))
  df$all <- "all"
  df$.active <- df$.sample_id %in% wave$.sample_id
  by_hand <- survey::twophase(
    id = list(~ psu + id, ~.unit),
    strata = list(~ all + h, ~.block),
    fpc = list(~ .fpc_1 + .fpc_2, ~.block_n),
    subset = ~.active,
    data = df,
    method = "full"
  )

  expect_export_invariants(
    as_svydesign(wave), wave, "y",
    reference = by_hand, stages = c(2, 1)
  )
})

## Strata holding a single sampled unit are reported at export

test_that("a lonely probability stratum is named by stage at export", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # Two certainties per PSU leave one probability unit in every PSU.
  frame <- data.frame(psu = rep(1:10, each = 6))
  frame$eid <- seq_len(nrow(frame))
  frame$x <- rep(c(50, 50, 2, 3, 4, 5), times = 10)
  frame$y <- frame$x + rep(1:6, times = 10)
  sample <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 4) |>
    add_stage("Dwellings") |>
    draw(n = 3, method = "pps_brewer", mos = x) |>
    execute(frame, seed = 1)
  expect_identical(sum(sample$.certainty_2), 8L)

  cnd <- expect_warning(
    svy <- as_svydesign(sample),
    class = "samplyr_warning_lonely_psu"
  )
  expect_identical(cnd$stage, 2L)
  expect_identical(cnd$n_strata, 4L)
  expect_match(conditionMessage(cnd), "Dwellings")
  expect_identical(condition_header(cnd), "as_svydesign")
  expect_error(survey::svytotal(~y, svy), "only one PSU at stage 2")

  withr::local_options(survey.lonely.psu = "adjust")
  expect_no_warning(as_svydesign(sample))
})

test_that("a stratum taken whole is a census, not a lonely stratum", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  frame <- data.frame(
    id = 1:31,
    st = c("a", rep("b", 30)),
    y = as.numeric(1:31)
  )
  census <- sampling_design() |>
    stratify_by(st) |>
    draw(n = c(a = 1, b = 5)) |>
    execute(frame, seed = 1)
  expect_no_warning(svy <- as_svydesign(census))
  expect_no_error(survey::svytotal(~y, svy))

  # One unit drawn from a stratum of two is a sample, and it is lonely.
  frame$st[2] <- "a"
  lonely <- sampling_design() |>
    stratify_by(st) |>
    draw(n = c(a = 1, b = 5)) |>
    execute(frame, seed = 1)
  cnd <- expect_warning(
    as_svydesign(lonely),
    class = "samplyr_warning_lonely_psu"
  )
  expect_identical(cnd$stage, 1L)
  expect_identical(cnd$n_strata, 1L)
})

## A user column named like a generated export column survives export
#
# The export writes its own columns into a copy of the sample. Each pinned
# generated name is given to a user column in turn: its total must be its
# own, and the design's estimates must not move.

collision_frame <- function() {
  set.seed(2)
  psu <- data.frame(
    psu = 1:40,
    a = rep(c("u", "v"), 20),
    b = rep(c("p", "q"), each = 20),
    x = c(400, 350, round(stats::runif(38, 5, 40)))
  )
  frame <- psu[rep(1:40, each = 6), ]
  frame$eid <- seq_len(nrow(frame))
  frame$h <- rep(c("m", "f"), length.out = nrow(frame))
  frame$y <- frame$x / 10 + stats::rnorm(nrow(frame))
  frame$z <- pmax(frame$y, 0.1) * ifelse(frame$eid %% 6 == 1, 50, 1)
  frame
}

export_variables <- function(svy) {
  if (inherits(svy, "twophase2")) svy$phase1$full$variables else svy$variables
}

expect_collision_free <- function(sample, generated, export,
                                  sample_cols = names(sample)) {
  clean <- export(sample)
  expect_setequal(
    setdiff(names(export_variables(clean)), sample_cols),
    generated
  )
  reference <- survey::svytotal(~y, clean)
  for (nm in generated) {
    user <- sample
    user[[nm]] <- seq_len(nrow(user)) + 0.5
    svy <- export(user)
    own <- survey::svytotal(stats::reformulate(nm), svy)
    expect_equal(
      unname(coef(own)),
      sum(as.data.frame(user)$.weight * user[[nm]]),
      info = nm
    )
    total <- survey::svytotal(~y, svy)
    expect_equal(coef(total), coef(reference), info = nm)
    expect_equal(vcov(total), vcov(reference), info = nm)
  }
}

test_that("single-phase generated names never overwrite a user column", {
  skip_if_not_installed("survey")
  frame <- collision_frame()
  quiet <- function(s) suppressWarnings(as_svydesign(s))

  pps_certainty <- sampling_design() |>
    add_stage() |>
    stratify_by(a, b) |>
    cluster_by(psu) |>
    draw(n = 4, method = "pps_brewer", mos = x) |>
    add_stage() |>
    stratify_by(h) |>
    draw(n = 2) |>
    execute(frame, seed = 1)
  expect_collision_free(
    pps_certainty,
    c(
      ".id_1", ".id_2", ".cert_stratum", ".strata_1", ".fpc_pi_1",
      ".fpc_f_2"
    ),
    quiet
  )

  later_certainty <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 10) |>
    add_stage() |>
    draw(n = 3, method = "pps_brewer", mos = z) |>
    execute(frame, seed = 1)
  expect_collision_free(
    later_certainty,
    c(
      ".id_1", ".id_2", ".cert_stratum_2", ".strata_all_1", ".fpc_f_1",
      ".fpc_pi_2"
    ),
    quiet
  )

  wr_psus <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 8, method = "srswr") |>
    add_stage() |>
    draw(n = 2) |>
    execute(frame, seed = 1)
  expect_collision_free(wr_psus, c(".id_2", ".fpc_inf_1"), quiet)

  wr_later <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 8, method = "pps_brewer", mos = x) |>
    add_stage() |>
    draw(n = 2, method = "srswr") |>
    execute(frame, seed = 1)
  expect_collision_free(
    wr_later,
    c(".id_1", ".cert_stratum", ".fpc_pi_1", ".fpc_f0_2"),
    quiet
  )
})

test_that("the replicate route keeps a user column too", {
  skip_if_not_installed("survey")
  frame <- collision_frame()
  wr_psus <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 8, method = "srswr") |>
    add_stage() |>
    draw(n = 2) |>
    execute(frame, seed = 1)
  user <- wr_psus
  user$.id_2 <- seq_len(nrow(user)) + 0.5
  # survey's own conversion warnings are not what this checks.
  svy <- suppressWarnings(
    as_svrepdesign(user, type = "bootstrap", replicates = 20)
  )
  expect_equal(
    unname(coef(survey::svytotal(~.id_2, svy))),
    sum(as.data.frame(user)$.weight * user$.id_2)
  )
})

test_that("two-phase generated names never overwrite a user column", {
  skip_if_not_installed("survey")
  frame <- collision_frame()
  phase1 <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 10) |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(eid) |>
    draw(n = 2) |>
    execute(frame, seed = 1)
  phase2 <- sampling_design() |>
    stratify_by(a, b) |>
    cluster_by(eid) |>
    draw(n = 3) |>
    execute(phase1, seed = 2)

  expect_collision_free(
    phase2,
    c(
      ".p1_id_1", ".p1_id_2", ".p1_strata_all_1", ".p2_id_1", ".p2_strata_1",
      ".fpc_phase2_1", ".weight_phase2", ".phase2", ".weight_phase2_cond",
      ".prob_1", ".prob_2"
    ),
    function(s) quiet_across(as_svydesign(s)),
    sample_cols = union(names(phase1), names(phase2))
  )
})

test_that("wave generated names never overwrite a user column", {
  skip_if_not_installed("survey")
  pop <- twophase_strata_population(n_psu = 48)
  schedule <- data.frame(
    panel = rep(1:4, times = 4),
    wave = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
  master <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 16) |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(id) |>
    draw(n = 5) |>
    execute(pop, seed = 3, panels = schedule)
  wave <- execute(master, wave = 1)

  expect_collision_free(
    wave,
    c(
      ".p1_id_1", ".p1_id_2", ".p1_strata_all_1", ".activation_unit",
      ".activation_block",
      ".activation_block_N", ".active", ".activation_prob",
      ".activation_weight", ".prob_1"
    ),
    as_svydesign,
    sample_cols = union(names(master), names(wave))
  )
})

## Relabelling clusters does not change the exported variance
#
# survey pairs cluster corrections with totals in sorted id order, so the
# export numbers clusters by first appearance and a relabelled frame gives
# the same design.

test_that("permuting cluster labels leaves the variance unchanged", {
  skip_if_not_installed("survey")
  set.seed(14)
  frame <- data.frame(psu = rep(1:12, each = 4))
  frame$eid <- seq_len(nrow(frame))
  frame$x <- rep(c(2, 30, 5, 60, 8, 25, 3, 45, 12, 6, 50, 9), each = 4)
  frame$y <- frame$x / 2 + stats::rnorm(nrow(frame), 0, 3)
  relabelled <- frame
  relabelled$psu <- c(7, 2, 11, 5, 12, 1, 9, 3, 10, 4, 8, 6)[frame$psu]

  one_stage <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 6, method = "pps_brewer", mos = x)
  two_stage <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 6, method = "pps_brewer", mos = x) |>
    add_stage() |>
    draw(n = 2)

  for (design in list(one_stage, two_stage)) {
    a <- execute(design, frame, seed = 3)
    b <- execute(design, relabelled, seed = 3)
    expect_identical(a$eid, b$eid)
    ta <- survey::svytotal(~y, as_svydesign(a))
    tb <- survey::svytotal(~y, as_svydesign(b))
    expect_equal(coef(ta), coef(tb))
    expect_equal(vcov(ta), vcov(tb))
  }
})

## Replicates of a with-replacement first stage
#
# A with-replacement first stage has no fpc. For a total under WR sampling
# of the first stage, the jackknife equals the linearized ultimate-cluster
# variance, an exact oracle.

wr_first_designs <- function() {
  set.seed(2)
  frame <- data.frame(
    psu = rep(1:40, each = 6),
    st = rep(c("A", "B"), each = 120)
  )
  frame$eid <- seq_len(nrow(frame))
  frame$y <- stats::rnorm(nrow(frame), 10) + frame$psu / 5
  frame$x <- rep(round(stats::runif(40, 5, 50)), each = 6)
  list(
    frame = frame,
    designs = list(
      srswr_psus = list(
        sampling_design() |> add_stage() |> cluster_by(psu) |>
          draw(n = 8, method = "srswr") |> add_stage() |> draw(n = 2),
        "JK1"
      ),
      stratified_srswr_psus = list(
        sampling_design() |> add_stage() |> stratify_by(st) |>
          cluster_by(psu) |> draw(n = 5, method = "srswr") |>
          add_stage() |> draw(n = 2),
        "JKn"
      ),
      multinomial_psus = list(
        sampling_design() |> add_stage() |> cluster_by(psu) |>
          draw(n = 8, method = "pps_multinomial", mos = x) |>
          add_stage() |> draw(n = 2),
        "JK1"
      ),
      # A later PPS stage makes "no correction" a zero fraction.
      multinomial_then_pps = list(
        sampling_design() |> add_stage() |> cluster_by(psu) |>
          draw(n = 8, method = "pps_multinomial", mos = x) |>
          add_stage() |> draw(n = 2, method = "pps_brewer", mos = y),
        "JK1"
      ),
      single_stage_srswr = list(
        sampling_design() |> draw(n = 20, method = "srswr"),
        "JK1"
      )
    )
  )
}

test_that("the jackknife of a WR first stage equals linearization", {
  skip_if_not_installed("survey")
  cases <- wr_first_designs()
  for (nm in names(cases$designs)) {
    design <- cases$designs[[nm]][[1]]
    type <- cases$designs[[nm]][[2]]
    sample <- suppressWarnings(execute(design, cases$frame, seed = 1))
    linearized <- as.numeric(
      survey::SE(survey::svytotal(~y, as_svydesign(sample)))
    )
    jackknife <- suppressWarnings(as_svrepdesign(sample, type = type))
    expect_equal(
      as.numeric(survey::SE(survey::svytotal(~y, jackknife))),
      linearized,
      info = nm
    )
  }
})

test_that("the bootstrap of a WR first stage is not zero", {
  skip_if_not_installed("survey")
  cases <- wr_first_designs()
  for (nm in names(cases$designs)) {
    sample <- suppressWarnings(
      execute(cases$designs[[nm]][[1]], cases$frame, seed = 1)
    )
    linearized <- as.numeric(
      survey::SE(survey::svytotal(~y, as_svydesign(sample)))
    )
    set.seed(15)
    boot <- suppressWarnings(
      as_svrepdesign(sample, type = "bootstrap", replicates = 2000)
    )
    # Band fixed before the run. The bootstrap SE's relative error is ~1.6%.
    ratio <- as.numeric(survey::SE(survey::svytotal(~y, boot))) / linearized
    expect_gt(ratio, 0.85, label = nm)
    expect_lt(ratio, 1.15, label = nm)
  }
})

test_that("mrbbootstrap still receives every stage's correction", {
  skip_if_not_installed("survey")
  # mrbbootstrap reads the later stages' corrections too.
  cases <- wr_first_designs()
  sample <- suppressWarnings(
    execute(cases$designs$srswr_psus[[1]], cases$frame, seed = 1)
  )
  set.seed(15)
  ours <- suppressWarnings(
    as_svrepdesign(sample, type = "mrbbootstrap", replicates = 200)
  )
  set.seed(15)
  theirs <- suppressWarnings(survey::as.svrepdesign(
    as_svydesign(sample),
    type = "mrbbootstrap",
    replicates = 200
  ))
  expect_equal(
    survey::SE(survey::svytotal(~y, ours)),
    survey::SE(survey::svytotal(~y, theirs))
  )
})

## The pps argument

pps_fixture <- function() {
  set.seed(1)
  frame <- data.frame(psu = rep(1:30, each = 4))
  frame$eid <- seq_len(nrow(frame))
  frame$x <- rep(round(stats::runif(30, 2, 20)), each = 4)
  frame$y <- stats::rnorm(nrow(frame), 10) + frame$x
  frame
}

test_that("pps = 'brewer' is the default multi-stage export", {
  skip_if_not_installed("survey")
  sample <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 8, method = "pps_brewer", mos = x) |>
    add_stage() |>
    draw(n = 2) |>
    execute(pps_fixture(), seed = 2)
  # The stage count guards against dropping stage 2.
  expect_export_invariants(
    as_svydesign(sample, pps = "brewer"), sample, "y",
    reference = as_svydesign(sample), stages = 2
  )
})

test_that("pps = FALSE on an equal-probability design changes nothing", {
  skip_if_not_installed("survey")
  sample <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 8) |>
    add_stage() |>
    draw(n = 2) |>
    execute(pps_fixture(), seed = 2)
  expect_export_invariants(
    as_svydesign(sample, pps = FALSE), sample, "y",
    reference = as_svydesign(sample), stages = 2
  )
})

test_that("a pps value that contradicts the design is refused", {
  skip_if_not_installed("survey")
  frame <- pps_fixture()
  pps_sample <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 8, method = "pps_brewer", mos = x) |>
    execute(frame, seed = 2)
  srs_sample <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 8) |>
    execute(frame, seed = 2)

  expect_error(
    as_svydesign(srs_sample, pps = "brewer"),
    class = "samplyr_error_pps_argument"
  )
  expect_error(
    as_svydesign(pps_sample, pps = FALSE),
    class = "samplyr_error_pps_argument"
  )
  for (bad in list("other", 1, TRUE)) {
    expect_error(
      as_svydesign(pps_sample, pps = bad),
      class = "samplyr_error_pps_argument"
    )
  }
})

## PPS at phase 2

# Phase 2's joint probabilities come from the phase-1 sample, its frame. The
# phase-2 term is read in the Sen-Yates-Grundy form, so with a census at
# phase 1 the export is the single-phase design with the same matrix.
pps_phase2_fixture <- function() {
  set.seed(3)
  frame <- data.frame(
    id = 1:60,
    x = round(stats::runif(60, 5, 50)),
    h = rep(c("a", "b"), 30)
  )
  frame$x[c(3, 8)] <- 400
  frame$y <- 3 * frame$x + stats::rnorm(60, 0, 10)
  frame$dom <- frame$id %% 3 == 0
  frame[sample(60), ]
}

survey_estimates <- function(estimate) {
  c(estimate = unname(coef(estimate)), se = unname(survey::SE(estimate)))
}

test_that("a PPS phase 2 over a census is the single-phase design", {
  skip_if_not_installed("survey")
  frame <- pps_phase2_fixture()
  phase1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 60) |>
    execute(frame, seed = 1)
  for (method in c("pps_sampford", "pps_sps")) {
    phase2 <- sampling_design() |>
      stratify_by(h) |>
      draw(n = c(a = 6, b = 5), method = method, mos = x) |>
      execute(phase1, seed = 2)
    expect_identical(sum(as.data.frame(phase2)$.certainty_1), 2L)
    joint <- joint_expectation(phase2, phase1)[[1]]
    reference <- survey::svydesign(
      ids = ~1, probs = ~ I(1 / .weight), data = as.data.frame(phase2),
      pps = survey::ppsmat(joint), variance = "YG"
    )
    exported <- as_svydesign(phase2)

    expect_equal(
      survey_estimates(survey::svytotal(~y, exported)),
      survey_estimates(survey::svytotal(~y, reference))
    )
    expect_equal(
      survey_estimates(survey::svymean(~y, exported)),
      survey_estimates(survey::svymean(~y, reference))
    )
    expect_equal(
      survey_estimates(survey::svytotal(~y, subset(exported, dom))),
      survey_estimates(survey::svytotal(~y, subset(reference, dom)))
    )
  }
})

test_that("a PPS phase 2 keeps survey's phase-1 term", {
  skip_if_not_installed("survey")
  frame <- pps_phase2_fixture()
  phase1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 30) |>
    execute(frame, seed = 1)
  phase2 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 10, method = "pps_sampford", mos = x) |>
    execute(phase1, seed = 2)
  exported <- survey::svytotal(~y, as_svydesign(phase2))

  # survey's own route, in the Horvitz-Thompson form, by hand.
  df1 <- as.data.frame(phase1)
  df2 <- as.data.frame(phase2)
  at <- match(df1$id, df2$id)
  df1$in2 <- !is.na(at)
  df1$prob1 <- 1 / df1$.weight
  df1$prob2 <- df1$.weight / df2$.weight[at]
  df1$pop <- nrow(frame)
  joint <- joint_expectation(phase2, phase1)[[1]]
  rows <- at[df1$in2]
  by_hand <- survey::svytotal(~y, survey::twophase(
    id = list(~id, ~id), probs = list(~prob1, ~prob2),
    fpc = list(~pop, NULL), subset = ~in2,
    data = df1, pps = list(NULL, survey::ppsmat(joint[rows, rows])),
    method = "full"
  ))
  expect_equal(unname(coef(exported)), unname(coef(by_hand)))
  phases <- attr(vcov(exported), "phases")
  expect_equal(phases$phase1, attr(vcov(by_hand), "phases")$phase1)

  # The phase-2 term in the Sen-Yates-Grundy form.
  pi2 <- diag(joint)
  expanded <- df2$y * df2$.weight
  pairs <- (outer(pi2, pi2) - joint) / joint
  syg <- sum(pairs * outer(expanded, expanded, "-")^2) / 2
  expect_equal(c(phases$phase2), syg)
  expect_false(isTRUE(all.equal(
    c(phases$phase2), c(attr(vcov(by_hand), "phases")$phase2)
  )))
})

test_that("a PPS phase 2 reads phase 1's probabilities as the other routes do", {
  skip_if_not_installed("survey")
  set.seed(4)
  frame <- data.frame(
    village = paste0("v", 1:60),
    region = rep(c("A", "B", "C"), each = 20),
    size = round(stats::runif(60, 5, 60))
  )
  frame$y <- 2 * frame$size + stats::rnorm(60)
  phase2 <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(village) |>
    draw(n = 2, method = "pps_sampford", mos = size)
  total_of <- function(phase1, seed = 1) {
    sample <- execute(phase2, execute(phase1, frame, seed = seed), seed = 2)
    exported <- survey::svytotal(~y, as_svydesign(sample))
    expect_equal(unname(coef(exported)), sum(sample$y * sample$.weight))
  }

  # Phase 1's correction states both stages' probabilities.
  total_of(
    sampling_design() |>
      cluster_by(region) |>
      draw(n = 2) |>
      add_stage() |>
      cluster_by(village) |>
      draw(n = 12)
  )
  # A with-replacement stage has no correction, so phase 1's weight is
  # passed. Seed 3 hits two villages twice.
  total_of(
    sampling_design() |>
      cluster_by(village) |>
      draw(n = 15, method = "srswr"),
    seed = 3
  )
  # Two identifier stages and no correction to read them from.
  expect_error(
    total_of(
      sampling_design() |>
        cluster_by(region) |>
        draw(n = 2, method = "srswr") |>
        add_stage() |>
        cluster_by(village) |>
        draw(n = 12)
    ),
    class = "samplyr_error_twophase_stage_probs"
  )
})

test_that("a phase 2 without an exported joint route is refused", {
  skip_if_not_installed("survey")
  frame <- pps_fixture()
  phase1 <- sampling_design() |>
    cluster_by(eid) |>
    draw(n = 80) |>
    execute(frame, seed = 1)
  refused <- function(phase2, ...) {
    cnd <- expect_error(
      as_svydesign(phase2, ...),
      class = "samplyr_error_twophase_phase2_pps"
    )
    expect_identical(condition_header(cnd), "as_svydesign")
  }

  for (method in c("pps_systematic", "pps_multinomial")) {
    refused(
      sampling_design() |>
        cluster_by(eid) |>
        draw(n = 20, method = method, mos = x) |>
        execute(phase1, seed = 2),
      systematic_variance = "approximate"
    )
  }
  clustered <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 20) |>
    execute(frame, seed = 1)
  refused(
    sampling_design() |>
      cluster_by(psu) |>
      draw(n = 6, method = "pps_sampford", mos = x) |>
      add_stage() |>
      cluster_by(eid) |>
      draw(n = 2) |>
      execute(clustered, seed = 2)
  )
  sampford <- sampling_design() |>
    cluster_by(eid) |>
    draw(n = 20, method = "pps_sampford", mos = x) |>
    execute(phase1, seed = 2)
  expect_s3_class(as_svydesign(sampford, method = "full"), "twophase2")
  refused(sampford, method = "approx")
  refused(sampford, method = "simple")
  refused(sampford, pps = "brewer")

  srs_phase2 <- sampling_design() |>
    cluster_by(eid) |>
    draw(n = 20) |>
    execute(phase1, seed = 2)
  expect_no_error(as_svydesign(srs_phase2))
  refused(srs_phase2, pps = "brewer")
})

test_that("a two-phase object survey lays out otherwise is refused", {
  skip_if_not_installed("survey")
  square <- Matrix::Diagonal(2)
  laid_out <- function(dcheck, class = "twophase2") {
    structure(list(dcheck = dcheck), class = class)
  }
  expect_s3_class(
    twophase_phase2_syg(laid_out(list(phase2 = square, full = square)), 2L),
    "twophase2"
  )
  for (other in list(
    laid_out(list(phase2 = square, full = square), class = "twophase"),
    laid_out(list(full = square)),
    laid_out(list(phase2 = square)),
    laid_out(list(phase2 = square, full = Matrix::Diagonal(3))),
    laid_out(list(phase2 = diag(2), full = square))
  )) {
    cnd <- expect_error(
      twophase_phase2_syg(other, 2L),
      class = "samplyr_error_twophase_phase2_pps"
    )
    expect_match(conditionMessage(cnd), "Sen-Yates-Grundy")
  }
})

test_that("a wave export takes no pps", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:400, value = (1:400) / 4)
  schedule <- data.frame(
    panel = rep(1:2, times = 2),
    wave = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  master <- sampling_design() |>
    draw(n = 60) |>
    execute(frame, seed = 42, panels = schedule)
  wave <- execute(master, wave = 1)
  expect_no_error(as_svydesign(wave))
  expect_error(
    as_svydesign(wave, pps = "brewer"),
    class = "samplyr_error_twophase_phase2_pps"
  )
})
