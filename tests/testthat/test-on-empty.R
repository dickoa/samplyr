# Tests for on_empty empty-selection semantics (review issue 3)
#
# When a random-size method selects zero units, on_empty = "warn"/
# "silent" returns an empty sample. An empty realization contributes
# zero to Horvitz-Thompson totals, which keeps the estimator unbiased
# over repeated executions. The former fallback (one SRS unit with
# weight N) biased HT totals upward by N * (1 - p)^N.

test_that("Bernoulli HT totals are unbiased under on_empty = 'silent'", {
  skip_on_cran()

  # The review's analytic case: N = 2, p = 0.1, y = c(1, 1).
  # True total = 2. The old fallback gave E[estimate] = 3.62.
  frame <- data.frame(id = 1:2, y = c(1, 1))
  estimates <- vapply(seq_len(4000), function(seed) {
    s <- sampling_design() |>
      draw(frac = 0.1, method = "bernoulli", on_empty = "silent") |>
      execute(frame, seed = seed)
    sum(s$.weight * s$y)
  }, numeric(1))

  # MC se ~ 4.02 / sqrt(4000) ~ 0.064; the old bias was +1.62.
  expect_equal(mean(estimates), 2, tolerance = 0.15)
  # Empty realizations occur at the theoretical rate (1 - p)^N = 0.81
  expect_equal(mean(estimates == 0), 0.81, tolerance = 0.05)
})

test_that("PPS Poisson HT totals are unbiased under on_empty = 'warn'", {
  skip_on_cran()

  frame <- data.frame(id = 1:3, size = c(1, 2, 3), y = c(1, 1, 1))
  estimates <- vapply(seq_len(4000), function(seed) {
    s <- suppressWarnings(
      sampling_design() |>
        draw(frac = 0.15, method = "pps_poisson", mos = size,
             on_empty = "warn") |>
        execute(frame, seed = seed)
    )
    sum(s$.weight * s$y)
  }, numeric(1))

  expect_equal(mean(estimates), 3, tolerance = 0.25)
})

test_that("an empty realization is a well-formed tbl_sample", {
  frame <- data.frame(id = 1:5)
  # frac = 0.001 on N = 5: empty with probability ~0.995
  result <- sampling_design() |>
    draw(frac = 0.001, method = "bernoulli", on_empty = "silent") |>
    execute(frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_identical(nrow(result), 0L)
  expect_true(all(
    c(".weight", ".sample_id", ".stage", ".weight_1", ".fpc_1") %in%
      names(result)
  ))
  expect_identical(attr(result, "metadata")$n_selected, 0L)

  # Display paths tolerate the empty sample
  expect_no_warning(capture.output(print(result)))
  expect_no_error(capture.output(summary(result)))
})

test_that("an empty stratum leaves other strata untouched", {
  skip_on_cran()

  frame <- data.frame(
    id = seq_len(1005),
    g = rep(c("small", "big"), c(5, 1000))
  )
  found <- FALSE
  for (seed in 1:100) {
    s <- suppressWarnings(
      sampling_design() |>
        stratify_by(g) |>
        draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
        execute(frame, seed = seed)
    )
    if (!"small" %in% s$g && nrow(s) > 0) {
      # Nonempty strata keep the design weight 1 / frac
      expect_equal(unique(s$.weight), 20)
      found <- TRUE
      break
    }
  }
  expect_true(found, info = "Expected an empty small stratum within 100 seeds")
})

test_that("an empty first stage ends a multi-stage execution", {
  frame <- data.frame(psu = rep(1:5, each = 4), id = 1:20)
  result <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(frac = 0.001, method = "bernoulli", on_empty = "silent") |>
    add_stage() |>
    draw(n = 2) |>
    execute(frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_identical(nrow(result), 0L)
})

test_that("certainty units survive an empty probabilistic part", {
  skip_on_cran()

  frame <- data.frame(id = 1:50, size = c(rep(1, 49), 100000))
  found <- FALSE
  for (seed in 1:200) {
    s <- suppressWarnings(
      sampling_design() |>
        draw(frac = 0.03, method = "pps_poisson", mos = size,
             certainty_prop = 0.3, on_empty = "silent") |>
        execute(frame, seed = seed)
    )
    if (nrow(s) == 1) {
      expect_true(s$.certainty_1)
      expect_equal(s$.weight, 1)
      found <- TRUE
      break
    }
  }
  expect_true(found, info = "Expected a certainty-only realization within 200 seeds")
})

test_that("replicated executions record empty replicates", {
  frame <- data.frame(id = 1:5)
  result <- suppressWarnings(
    sampling_design() |>
      draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
      execute(frame, seed = 42, reps = 10)
  )
  counts <- attr(result, "metadata")$replicate_rows
  expect_length(counts, 10)
  expect_identical(sum(counts), nrow(result))
  expect_true(any(counts == 0L))
})

test_that("custom random-size WOR methods honour on_empty", {
  on.exit(sondage::unregister_method("always_empty"), add = TRUE)
  sondage::register_method(
    "always_empty", "wor",
    sample_fn = function(pik, ...) integer(0),
    fixed_size = FALSE
  )
  frame <- data.frame(id = 1:5, size = rep(1, 5))

  # Default errors, like the built-in random-size methods
  expect_error(
    sampling_design() |>
      draw(n = 2, method = "pps_always_empty", mos = size) |>
      execute(frame, seed = 1),
    "zero selections"
  )

  empty <- sampling_design() |>
    draw(n = 2, method = "pps_always_empty", mos = size,
         on_empty = "silent") |>
    execute(frame, seed = 1)
  expect_identical(nrow(empty), 0L)

  # Certainty path: the certainty unit survives an empty prob part
  cert_frame <- data.frame(id = 1:5, size = c(1, 1, 1, 1, 500))
  cert_only <- sampling_design() |>
    draw(n = 2, method = "pps_always_empty", mos = size,
         certainty_size = 100, on_empty = "silent") |>
    execute(cert_frame, seed = 1)
  expect_identical(nrow(cert_only), 1L)
  expect_true(cert_only$.certainty_1)
})

test_that("empty replicates block later phases with a contextual error", {
  frame <- data.frame(id = 1:5)
  phase1 <- suppressWarnings(
    sampling_design(title = "Screening phase") |>
      draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
      execute(frame, seed = 42, reps = 10)
  )
  counts <- attr(phase1, "metadata")$replicate_rows
  expect_true(any(counts == 0L))

  err <- expect_error(
    sampling_design() |> draw(n = 1) |> execute(phase1, seed = 1),
    class = "samplyr_error_empty_phase_replicate"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "phase-1")
  expect_match(msg, "phase 2")
  expect_match(msg, "bernoulli")
  expect_match(msg, "Screening")
  expect_match(msg, "bias simulation summaries")
})

test_that("continuing a stack with empty replicates errors, not skips", {
  ms <- data.frame(psu = rep(1:5, each = 4), id = 1:20)
  st1 <- suppressWarnings(
    sampling_design() |>
      add_stage("PSUs") |>
      cluster_by(psu) |>
      draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
      add_stage("Units") |>
      draw(n = 2) |>
      execute(ms, stages = 1, seed = 7, reps = 6)
  )
  expect_true(any(attr(st1, "metadata")$replicate_rows == 0L))

  expect_error(
    execute(st1, ms, seed = 2),
    class = "samplyr_error_empty_phase_replicate"
  )
})

test_that("an extracted nonempty replicate can enter a later phase", {
  frame <- data.frame(id = 1:5)
  phase1 <- suppressWarnings(
    sampling_design() |>
      draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
      execute(frame, seed = 42, reps = 10)
  )
  counts <- attr(phase1, "metadata")$replicate_rows
  nonempty <- as.integer(names(counts)[counts > 0])
  expect_gt(length(nonempty), 0)

  extracted <- dplyr::filter(phase1, .replicate == nonempty[1])
  phase2 <- sampling_design() |> draw(n = 1) |> execute(extracted, seed = 5)
  expect_s3_class(phase2, "tbl_sample")
  expect_gt(nrow(phase2), 0)
})

test_that("replicated stacks without empty replicates flow to later phases", {
  frame <- data.frame(id = 1:5)
  phase1 <- sampling_design() |>
    draw(n = 3) |>
    execute(frame, seed = 1, reps = 4)
  phase2 <- sampling_design() |> draw(n = 1) |> execute(phase1, seed = 3)
  expect_identical(length(unique(phase2$.replicate)), 4L)
})
