test_that("deterministic zero allocations fail in validation, preview and execution", {
  frame <- data.frame(h = c("A", "B", rep("C", 101)), y = c(1000, 1000, rep(1, 101)))
  design <- sampling_design() |> stratify_by(h, alloc = "proportional") |> draw(n = 3)
  expect_error(validate_frame(design, frame), class = "samplyr_error_zero_allocation")
  expect_error(frame_summary(design, frame), "zero inclusion probability")
  expect_error(execute(design, frame), class = "samplyr_error_zero_allocation")
  repaired <- sampling_design() |> stratify_by(h, alloc = "proportional") |> draw(n = 3, min_n = 1)
  sample <- execute(repaired, frame, seed = 1)
  expect_setequal(sample$h, c("A", "B", "C"))
  expect_equal(sum(sample$.weight * sample$y), sum(frame$y))
})

test_that("certainty cannot exhaust the draw while leaving a noncertainty population", {
  frame <- data.frame(id = 1:5, mos = c(100, 100, 1, 1, 1))
  for (overflow in c("error", "allow")) {
    design <- sampling_design() |> draw(n = 2, method = "pps_brewer", mos = mos,
      certainty_size = 50, certainty_overflow = overflow)
    expect_error(validate_frame(design, frame), class = "samplyr_error_certainty_zero_probability")
    expect_error(execute(design, frame), class = "samplyr_error_certainty_zero_probability")
    expect_error(exante_probabilities(design, frame, key = id), "zero inclusion probability")
  }
  census <- sampling_design() |> draw(n = 2, method = "pps_brewer", mos = mos,
    certainty_size = 1, certainty_overflow = "allow") |> execute(frame)
  expect_equal(census$.weight, rep(1, 5))
})

test_that("wave and phase exports use current observations including missing values", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:40, y = 1, label = "old")
  schedule <- data.frame(panel = c(1L, 2L, 1L, 2L), wave = c(1L, 1L, 2L, 2L),
    active = c(TRUE, FALSE, FALSE, TRUE))
  master <- sampling_design() |> draw(n = 20) |> execute(frame, seed = 12, panels = schedule)
  wave <- execute(master, wave = 2)
  phase1 <- sampling_design() |> cluster_by(id) |> draw(n = 20) |> execute(frame, seed = 12)
  phase2 <- sampling_design() |> draw(n = 10) |> execute(phase1, seed = 13)
  for (s in list(wave, phase2)) {
    s$y <- 100 + s$id
    s$y[2] <- NA_real_
    s$label <- factor(rep("new", nrow(s)))
    s$new_measure <- s$id^2
    s$.active <- "reported"
    # Row order must not determine which observation replaces an old value.
    s <- s[nrow(s):1, ]
    fit <- as_svydesign(s)
    out <- fit$phase1$sample$variables
    at <- match(out$id, s$id)
    expect_equal(out$y, s$y[at])
    expect_equal(out$label, s$label[at])
    expect_equal(out$new_measure, s$new_measure[at])
    expect_equal(out$.active, s$.active[at])
    expect_equal(unname(coef(suppressWarnings(survey::svytotal(~y, fit, na.rm = TRUE)))),
      sum(s$.weight * s$y, na.rm = TRUE))
  }
  expect_equal(master$y, rep(1, 20))
  expect_equal(phase1$y, rep(1, 20))
})

test_that("random empty Poisson realizations remain valid", {
  frame <- data.frame(id = 1:10, u = rep(.99, 10))
  sample <- sampling_design() |> draw(frac = .01, method = "bernoulli", prn = u,
    on_empty = "silent") |> execute(frame)
  expect_equal(nrow(sample), 0L)
})

test_that("two-phase export cannot silently use fixed-size Poisson variance", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:40, y = 1)
  phase1 <- sampling_design() |> cluster_by(id) |> draw(n = 20) |> execute(frame, seed = 12)
  phase2 <- sampling_design() |> draw(frac = .5, method = "bernoulli") |> execute(phase1, seed = 13)
  expect_error(as_svydesign(phase2), class = "samplyr_error_twophase_poisson")
})
