# varcomp.tbl_sample: design-based variance components from an
# executed sample (dev/dev-notes-varcomp-tbl-sample.md).

varcomp_frame <- function(n_clusters = 24, per = 5, seed = 99) {
  withr::with_seed(seed, {
    sizes <- rep(c(80, 120, 160, 200), length.out = n_clusters)
    data.frame(
      cl = rep(sprintf("c%02d", seq_len(n_clusters)), each = per),
      size = rep(sizes, each = per),
      y = rnorm(n_clusters * per) +
        rep(rnorm(n_clusters, sd = 0.4), each = per)
    )
  })
}

two_stage_pps <- function(frame, seed = 11) {
  sampling_design() |>
    add_stage() |> cluster_by(cl) |>
    draw(n = 8, method = "pps_brewer", mos = size) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = seed)
}

test_that("varcomp matches the manual svyplan recipe on 2-stage PPS", {
  frame <- varcomp_frame()
  s <- two_stage_pps(frame)
  vc <- varcomp(s, ~y)
  expect_s3_class(vc, "svyplan_varcomp")
  expect_identical(
    names(as.data.frame(vc)),
    c("stages", "varb", "varw", "delta", "k", "rel_var")
  )

  pi1 <- 1 / s$.weight_1
  first <- !duplicated(s$cl)
  share <- pi1[first] / sum(pi1[first])
  manual <- svyplan::varcomp(
    s$y,
    stage_id = list(s$cl),
    prob = share[match(s$cl, s$cl[first])],
    weights = s$.weight_2
  )
  expect_equal(vc$varb, manual$varb, tolerance = 1e-12)
  expect_equal(vc$varw, manual$varw, tolerance = 1e-12)
  expect_equal(vc$delta, manual$delta, tolerance = 1e-12)
  expect_equal(vc$k, manual$k, tolerance = 1e-12)
})

test_that("a whole-take self-weighting sample recovers frame components", {
  frame <- varcomp_frame()
  s <- sampling_design() |>
    add_stage() |> cluster_by(cl) |> draw(frac = 1) |>
    add_stage() |> draw(frac = 1) |>
    execute(frame, seed = 5)
  expect_true(all(s$.weight == 1))
  vc <- varcomp(s, ~y)
  vc_frame <- svyplan::varcomp(frame$y, stage_id = list(frame$cl))
  expect_equal(vc$varb, vc_frame$varb, tolerance = 1e-12)
  expect_equal(vc$delta, vc_frame$delta, tolerance = 1e-12)
})

test_that("mean delta over replicates tracks the population delta", {
  frame <- varcomp_frame()
  truth <- svyplan::varcomp(frame$y, stage_id = list(frame$cl))$delta
  deltas <- vapply(seq_len(200), function(r) {
    s <- two_stage_pps(frame, seed = 1000 + r)
    varcomp(s, ~y)$delta
  }, numeric(1))
  mc_se <- stats::sd(deltas) / sqrt(length(deltas))
  expect_lt(abs(mean(deltas) - truth), 1.5 * mc_se + 0.02)
})

test_that("an SRS first stage takes the prob = NULL path", {
  frame <- varcomp_frame()
  s <- sampling_design() |>
    add_stage() |> cluster_by(cl) |> draw(n = 8) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 21)
  vc <- varcomp(s, ~y)
  manual <- svyplan::varcomp(
    s$y, stage_id = list(s$cl), weights = s$.weight_2
  )
  expect_equal(vc$varb, manual$varb, tolerance = 1e-12)
  expect_equal(vc$delta, manual$delta, tolerance = 1e-12)
})

test_that("3-stage decomposition uses both cluster keys and stage 2-3 weights", {
  frame <- withr::with_seed(31, {
    data.frame(
      dist = rep(sprintf("d%d", 1:6), each = 20),
      vil = rep(sprintf("v%02d", 1:24), each = 5),
      dsize = rep(c(300, 500, 700, 400, 600, 800), each = 20),
      y = rnorm(120)
    )
  })
  s <- sampling_design() |>
    add_stage() |> cluster_by(dist) |>
    draw(n = 4, method = "pps_brewer", mos = dsize) |>
    add_stage() |> cluster_by(vil) |> draw(n = 2) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 41)
  vc <- varcomp(s, ~y)
  expect_identical(vc$stages, 3L)

  pi1 <- 1 / s$.weight_1
  first <- !duplicated(s$dist)
  share <- pi1[first] / sum(pi1[first])
  vil_key <- paste(s$dist, s$vil, sep = "/")
  manual <- svyplan::varcomp(
    s$y,
    stage_id = list(s$dist, vil_key),
    prob = share[match(s$dist, s$dist[first])],
    weights = s$.weight_2 * s$.weight_3
  )
  expect_equal(vc$varb, manual$varb, tolerance = 1e-12)
  expect_equal(vc$delta, manual$delta, tolerance = 1e-12)
})

test_that("stage-1 design strata give per-stratum components with per-stratum shares", {
  frame <- withr::with_seed(51, {
    data.frame(
      region = rep(c("N", "S"), each = 60),
      cl = rep(sprintf("c%02d", 1:24), each = 5),
      size = rep(rep(c(90, 140, 190), 8), each = 5),
      y = rnorm(120)
    )
  })
  s <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(cl) |>
    draw(n = 4, method = "pps_brewer", mos = size) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 61)
  vc <- varcomp(s, ~y)
  expect_s3_class(vc, "svyplan_varcomp")
  tab <- as.data.frame(vc)
  expect_identical(nrow(tab), 2L)

  for (r in c("N", "S")) {
    rows <- s$region == r
    pi1 <- 1 / s$.weight_1[rows]
    first <- !duplicated(s$cl[rows])
    share <- pi1[first] / sum(pi1[first])
    manual <- svyplan::varcomp(
      s$y[rows],
      stage_id = list(s$cl[rows]),
      prob = share[match(s$cl[rows], s$cl[rows][first])],
      weights = s$.weight_2[rows]
    )
    expect_equal(
      tab$delta_psu[tab$stratum == r], manual$delta,
      tolerance = 1e-12
    )
  }
})

test_that("user strata split the components when the design has none", {
  frame <- varcomp_frame()
  frame$zone <- rep(c("a", "b"), each = 60)
  s <- sampling_design() |>
    add_stage() |> cluster_by(cl) |> draw(n = 12) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 71)
  vc <- varcomp(s, ~y, strata = ~zone)
  expect_identical(nrow(as.data.frame(vc)), 2L)
})

test_that("design strata plus a strata argument is refused", {
  frame <- withr::with_seed(81, {
    data.frame(
      region = rep(c("N", "S"), each = 60),
      cl = rep(sprintf("c%02d", 1:24), each = 5),
      zone = rep(c("a", "b"), 60),
      y = rnorm(120)
    )
  })
  s <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(cl) |> draw(n = 4) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 82)
  expect_error(
    varcomp(s, ~y, strata = ~zone),
    class = "samplyr_error_varcomp_strata"
  )
})

test_that("certainty PSUs are refused with a precise message", {
  frame <- varcomp_frame()
  s <- sampling_design() |>
    add_stage() |> cluster_by(cl) |>
    draw(n = 8, method = "pps_brewer", mos = size, certainty_size = 195) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 91)
  expect_true(any(s$.certainty_1))
  expect_error(varcomp(s, ~y), class = "samplyr_error_varcomp_certainty")
})

test_that("capped PPS certainty is refused without an explicit flag", {
  frame <- varcomp_frame(n_clusters = 12)
  frame$size <- rep(c(1000, rep(1, 11)), each = 5)
  s <- sampling_design() |>
    add_stage() |> cluster_by(cl) |>
    draw(n = 4, method = "pps_brewer", mos = size) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 92)

  dominant <- s$cl == "c01"
  expect_true(any(dominant))
  expect_false(any(s$.certainty_1[dominant]))
  expect_identical(unique(s$.weight_1[dominant]), 1)
  expect_error(
    varcomp(s, ~y),
    class = "samplyr_error_varcomp_certainty"
  )
})

test_that("a stage-1 weight within certainty tolerance is refused", {
  tol <- sqrt(.Machine$double.eps)
  target_pi <- 1 - tol / 2
  n_clusters <- 24L
  n_target <- 8L
  dominant_mos <- target_pi * (n_clusters - 1) / (n_target - target_pi)
  frame <- varcomp_frame(n_clusters = n_clusters)
  frame$size <- rep(c(dominant_mos, rep(1, n_clusters - 1)), each = 5)
  frame$u <- rep(
    c(0.5, seq(0.01, 0.99, length.out = n_clusters - 1)),
    each = 5
  )
  s <- sampling_design() |>
    add_stage() |> cluster_by(cl) |>
    draw(n = n_target, method = "pps_poisson", mos = size, prn = u) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 93, frame_digest = "none")

  dominant_weight <- unique(s$.weight_1[s$cl == "c01"])
  expect_length(dominant_weight, 1L)
  expect_lte(abs(dominant_weight - 1), tol)
  expect_gt(abs(dominant_weight - 1), 0)
  expect_error(
    varcomp(s, ~y),
    class = "samplyr_error_varcomp_certainty"
  )
})

test_that("modified, two-phase, and unclustered samples are refused", {
  frame <- varcomp_frame()
  s <- two_stage_pps(frame)

  filtered <- s[s$cl != s$cl[1], ]
  expect_error(varcomp(filtered, ~y), "varcomp")

  phase1 <- sampling_design() |> draw(n = 60) |> execute(frame, seed = 3)
  phase2 <- sampling_design() |> draw(n = 20) |> execute(phase1, seed = 4)
  expect_error(
    varcomp(phase2, ~y),
    class = "samplyr_error_varcomp_two_phase"
  )

  flat <- sampling_design() |> draw(n = 30) |> execute(frame, seed = 6)
  expect_error(
    varcomp(flat, ~y),
    class = "samplyr_error_varcomp_unclustered"
  )

  expect_error(varcomp(s, "y"), "one-sided formula")
  expect_error(varcomp(s, ~nope), "not found")
})

test_that("a WR first stage keys PSUs by draw, not by cluster", {
  frame <- varcomp_frame()
  s <- sampling_design() |>
    add_stage() |> cluster_by(cl) |>
    draw(n = 8, method = "pps_multinomial", mos = size) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 101)
  expect_lt(length(unique(s$cl)), 8L)

  vc <- varcomp(s, ~y)

  pi1 <- 1 / s$.weight_1
  first <- !duplicated(s$.draw_1)
  share <- pi1[first] / sum(pi1[first])
  manual <- svyplan::varcomp(
    s$y,
    stage_id = list(s$.draw_1),
    prob = share[match(s$.draw_1, s$.draw_1[first])],
    weights = s$.weight_2
  )
  expect_equal(vc$varb, manual$varb, tolerance = 1e-12)
  expect_equal(vc$delta, manual$delta, tolerance = 1e-12)

  merged <- svyplan::varcomp(
    s$y,
    stage_id = list(s$cl),
    prob = tapply(pi1, s$cl, unique) / sum(tapply(pi1, s$cl, unique)),
    weights = s$.weight_2
  )
  expect_false(isTRUE(all.equal(vc$delta, merged$delta)))
})


test_that("a stratified WR first stage qualifies draw keys by stratum", {
  # .draw_1 restarts in each stratum pool. The bare index would merge
  # draws across strata and trip the weight-constancy check.
  frame <- withr::with_seed(3, {
    data.frame(
      region = rep(c("N", "S"), each = 60),
      cl = rep(sprintf("c%02d", 1:24), each = 5),
      size = rep(rep(c(90, 140, 190), 8), each = 5),
      y = rnorm(120)
    )
  })
  s <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(cl) |>
    draw(n = 4, method = "pps_multinomial", mos = size) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 8)
  expect_true(any(table(s$region, s$.draw_1) > 0))

  vc <- varcomp(s, ~y)
  tab <- as.data.frame(vc)
  expect_identical(nrow(tab), 2L)

  for (r in c("N", "S")) {
    rows <- s$region == r
    pi1 <- 1 / s$.weight_1[rows]
    first <- !duplicated(s$.draw_1[rows])
    share <- pi1[first] / sum(pi1[first])
    manual <- svyplan::varcomp(
      s$y[rows],
      stage_id = list(s$.draw_1[rows]),
      prob = share[match(s$.draw_1[rows], s$.draw_1[rows][first])],
      weights = s$.weight_2[rows]
    )
    expect_equal(
      tab$delta_psu[tab$stratum == r], manual$delta,
      tolerance = 1e-12
    )
  }
})
