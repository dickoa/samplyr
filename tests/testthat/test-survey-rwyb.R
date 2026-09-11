skip_if_not_installed("svrep", "0.9.1")
skip_if_not_installed("survey")

rwyb_variance <- function(x, seed = 42, replicates = 20000) {
  r <- withr::with_seed(seed, as_svrepdesign(x, type = "rwyb", replicates = replicates))
  as.numeric(vcov(survey::svytotal(~y, r)))
}

test_that("RWYB recovers the Poisson variance lost by fixed-size replication", {
  frame <- data.frame(psu = rep(1:4, each = 40), id = 1:160, y = 1)
  sample <- sampling_design() |> cluster_by(psu) |> draw(n = 4) |> add_stage() |>
    draw(frac = .5, method = "bernoulli") |> execute(frame, seed = 41)
  expect_equal(nrow(sample), 87L)
  expect_error(as_svydesign(sample), class = "samplyr_error_multistage_poisson_later")
  for (type in c("auto", "bootstrap", "subbootstrap", "mrbbootstrap", "JK1")) {
    expect_error(as_svrepdesign(sample, type = type), class = "samplyr_error_poisson_replicates")
  }
  expect_equal(rwyb_variance(sample), 174, tolerance = .05)

  frame <- data.frame(psu = rep(1:20, each = 40), y = 1)
  sample <- sampling_design() |> cluster_by(psu) |> draw(frac = .5, method = "bernoulli") |>
    execute(frame, seed = 5)
  expect_equal(rwyb_variance(sample), 32000, tolerance = .05)
})

test_that("mixed SRS and Poisson stages reproduce the recursive variance estimator", {
  frame <- data.frame(psu = rep(1:8, each = 16), id = 1:128, y = (1:128)^.8)
  sample <- sampling_design() |> cluster_by(psu) |> draw(n = 5) |> add_stage() |>
    draw(frac = .5, method = "bernoulli") |> execute(frame, seed = 31)
  totals <- tapply(sample$y * sample$.weight_2, sample$psu, sum)
  conditional <- sum((1 - 1 / sample$.weight_2) * (sample$y * sample$.weight_2)^2)
  target <- 8^2 * (1 - 5/8) / 5 * var(totals) + 8/5 * conditional
  expect_equal(rwyb_variance(sample), target, tolerance = .05)
})

test_that("Poisson certainties have exactly factor one", {
  frame <- data.frame(id = 1:20, mos = c(20, rep(1, 19)), y = 1:20)
  sample <- suppressWarnings(sampling_design() |> draw(n = 8, method = "pps_poisson", mos = mos) |>
    execute(frame, seed = 1))
  r <- withr::with_seed(42, as_svrepdesign(sample, type = "rwyb", replicates = 20000))
  w <- stats::weights(r, type = "analysis")
  expect_true(all(w[sample$.weight == 1, ] == 1))
  target <- sum((1 - 1 / sample$.weight) * (sample$y * sample$.weight)^2)
  expect_equal(as.numeric(vcov(survey::svytotal(~y, r))), target, tolerance = .05)
  census <- sampling_design() |> draw(frac = 1, method = "bernoulli") |> execute(frame)
  expect_equal(rwyb_variance(census, replicates = 20), 0)
})

test_that("SRS and WR multistage designs retain their variance and draw occurrences", {
  frame <- data.frame(psu = rep(1:8, each = 12), id = 1:96,
    mos = rep(1:8, each = 12), y = (1:96)^.6)
  for (method in c("srswor", "srswr", "pps_multinomial")) {
    design <- sampling_design() |> cluster_by(psu)
    design <- if (method == "pps_multinomial") draw(design, n = 12, method = method, mos = mos) else draw(design, n = 5, method = method)
    sample <- design |> add_stage() |> draw(n = 6) |> execute(frame, seed = 3)
    target <- as.numeric(vcov(survey::svytotal(~y, as_svydesign(sample))))
    expect_equal(rwyb_variance(sample), target, tolerance = .05, info = method)
  }
})

test_that("RWYB respects local IDs and later-stage strata", {
  frame <- expand.grid(id = 1:8, h = 1:2, psu = 1:6)
  frame$y <- with(frame, id + h^2 + psu^2)
  sample <- sampling_design() |> cluster_by(psu) |> draw(n = 4) |> add_stage() |>
    stratify_by(h) |> cluster_by(h, id) |> draw(n = 4) |> execute(frame, seed = 3)
  target <- as.numeric(vcov(survey::svytotal(~y, as_svydesign(sample))))
  expect_equal(rwyb_variance(sample), target, tolerance = .05)
})

test_that("RWYB rejects lost parents, unsupported mechanisms and unidentified singleton variance", {
  frame <- data.frame(psu = rep(1:4, each = 4), id = 1:16, u = c(rep(.9, 4), rep(.1, 12)), y = 1)
  sample <- sampling_design() |> cluster_by(psu) |> draw(n = 3) |> add_stage() |>
    draw(frac = .5, method = "bernoulli", prn = u, on_empty = "silent") |> execute(frame, seed = 1)
  expect_error(as_svrepdesign(sample, type = "rwyb"), class = "samplyr_error_rwyb_missing_parents")
  singleton <- sampling_design() |> draw(n = 1) |> execute(frame, seed = 1)
  expect_error(as_svrepdesign(singleton, type = "rwyb"), class = "samplyr_error_rwyb_singleton")
  poisson_one <- sampling_design() |> draw(frac = .1, method = "bernoulli", prn = u,
    on_empty = "silent") |> execute(data.frame(u = c(.01, rep(.9, 15)), y = 1))
  expect_gt(rwyb_variance(poisson_one, replicates = 1000), 0)
  pareto <- sampling_design() |> draw(n = 4, method = "pps_pareto", mos = id) |> execute(frame, seed = 1)
  expect_error(as_svrepdesign(pareto, type = "rwyb"), class = "samplyr_error_rwyb_method")
  expect_error(as_svrepdesign(singleton, type = "rwyb", replicates = 1), class = "samplyr_error_rwyb_input")
  expect_error(as_svrepdesign(singleton, type = "rwyb", lonely.psu = "certainty"),
    class = "samplyr_error_unknown_argument")
})

test_that("RWYB singleton checks count stage units and allow certainties", {
  frame <- expand.grid(id = 1:4, psu = 1:4)
  first <- sampling_design() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> draw(n = 3) |> execute(frame, seed = 1)
  expect_equal(nrow(first), 3L)
  expect_error(as_svrepdesign(first, type = "rwyb"),
    class = "samplyr_error_rwyb_singleton")

  later <- sampling_design() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> draw(n = 1) |> execute(frame, seed = 1)
  expect_error(as_svrepdesign(later, type = "rwyb", replicates = 10),
    class = "samplyr_error_rwyb_singleton")

  census <- sampling_design() |> stratify_by(psu, id) |> draw(n = 1) |>
    execute(frame, seed = 1)
  expect_s3_class(as_svrepdesign(census, type = "rwyb", replicates = 10),
    "svyrep.design")
})

test_that("PPS approximation, seeds and srvyr export are explicit", {
  frame <- data.frame(id = 1:30, mos = 1:30, y = 1:30)
  sample <- sampling_design() |> draw(n = 10, method = "pps_brewer", mos = mos) |> execute(frame, seed = 3)
  expect_warning(r <- withr::with_seed(42, as_svrepdesign(sample, type = "rwyb")),
    class = "samplyr_warning_rwyb_pps_approximation")
  r2 <- suppressWarnings(withr::with_seed(42, as_svrepdesign(sample, type = "rwyb")))
  expect_equal(stats::weights(r, type = "analysis"), stats::weights(r2, type = "analysis"))
  expect_true(attr(r, "samplyr_replication")$pps_approximation)
  expect_equal(unname(stats::weights(r, type = "sampling")), sample$.weight)
  skip_if_not_installed("srvyr")
  expect_s3_class(suppressWarnings(srvyr::as_survey_rep(sample, type = "rwyb")), "tbl_svy")
})

test_that("independent frames combine RWYB variances by frame", {
  frame <- data.frame(id = 1:30, y = 1:30, in_a = TRUE, in_b = TRUE)
  a <- sampling_design() |> draw(frac = .5, method = "bernoulli") |> execute(frame, seed = 1)
  b <- sampling_design() |> draw(frac = .6, method = "bernoulli") |> execute(frame, seed = 2)
  frames <- stack_frames(a = a, b = b, membership = c(a = "in_a", b = "in_b"), key = id)
  result <- withr::with_seed(42, as_svrepdesign(frames, type = "rwyb", replicates = 200))
  separate <- withr::with_seed(42, lapply(list(a, b), function(s) {
    survey::svytotal(~y, as_svrepdesign(s, type = "rwyb", replicates = 200))
  }))
  estimate <- survey::svytotal(~y, result)
  expect_equal(as.numeric(coef(estimate)), sum(vapply(separate, function(z) as.numeric(coef(z)), 0)) / 2)
  expect_equal(as.numeric(vcov(estimate)), sum(vapply(separate, function(z) as.numeric(vcov(z)), 0)) / 4)
})

test_that("Poisson first-stage replication retains conditional SRS variance", {
  frame <- data.frame(psu = rep(1:12, each = 10), y = (1:120)^.7)
  sample <- sampling_design() |> cluster_by(psu) |> draw(frac = .5, method = "bernoulli") |>
    add_stage() |> draw(n = 5) |> execute(frame, seed = 1)
  totals <- tapply(sample$y * sample$.weight_2, sample$psu, sum)
  conditional <- sum(vapply(split(sample$y, sample$psu), function(y) 10^2 * (1 - 5/10) / 5 * var(y), 0))
  target <- sum((1 - .5) / .5^2 * totals^2) + conditional / .5
  expect_equal(rwyb_variance(sample), target, tolerance = .05)
})
