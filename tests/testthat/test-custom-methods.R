# Tests for custom registered sondage methods flowing through samplyr

# Deterministic WOR: always picks the n units with largest pik
toy_wor_fn <- function(pik, n = NULL, prn = NULL, ...) {
  order(pik, decreasing = TRUE)[seq_len(n)]
}

# Deterministic WOR with PRN support
toy_prn_fn <- function(pik, n = NULL, prn = NULL, ...) {
  if (is.null(prn)) {
    prn <- runif(length(pik))
  }
  which(prn < pik)
}

# Simple joint: outer product (independent approximation)
toy_joint_fn <- function(pik, sample_idx = NULL, ...) {
  if (!is.null(sample_idx)) {
    pik <- pik[sample_idx]
  }
  J <- outer(pik, pik)
  diag(J) <- pik
  J
}

# Deterministic WR: multinomial-like, always pick top-prob units
toy_wr_fn <- function(hits, n = NULL, prn = NULL, ...) {
  sample.int(length(hits), size = n, replace = TRUE, prob = hits)
}

set.seed(20260328)
custom_frame <- data.frame(
  id = seq_len(100),
  region = factor(rep(c("North", "South"), each = 50)),
  size = c(runif(50, 10, 100), runif(50, 5, 50)),
  prn_col = runif(100)
)

test_that("draw() accepts registered custom methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn)

  design <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size)

  expect_s3_class(design, "sampling_design")
  expect_equal(design$stages[[1]]$draw_spec$method, "pps_test_wor")
  expect_equal(design$stages[[1]]$draw_spec$method_type, "wor")
  expect_true(design$stages[[1]]$draw_spec$method_fixed)
})

test_that("draw() rejects unregistered custom methods", {
  expect_error(
    sampling_design() |> draw(n = 10, method = "pps_nonexistent", mos = size),
    "Unknown sampling method"
  )
})

test_that("draw() requires mos for custom methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn)

  expect_error(
    sampling_design() |> draw(n = 10, method = "pps_test_wor"),
    "mos"
  )
})

test_that("draw() validates frac <= 1 for custom WOR methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn)

  expect_error(
    sampling_design() |> draw(frac = 1.5, method = "pps_test_wor", mos = size),
    "cannot exceed 1"
  )
})

test_that("draw() validates PRN support for custom methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method(
    "test_wor",
    "wor",
    sample_fn = toy_wor_fn,
    supports_prn = FALSE
  )

  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_test_wor", mos = size, prn = prn_col),
    "permanent random numbers"
  )
})

test_that("draw() accepts PRN for custom methods that support it", {
  on.exit(sondage::unregister_method("test_prn"), add = TRUE)
  sondage::register_method(
    "test_prn",
    "wor",
    sample_fn = toy_prn_fn,
    fixed_size = FALSE,
    supports_prn = TRUE
  )

  design <- sampling_design() |>
    draw(n = 20, method = "pps_test_prn", mos = size, prn = prn_col)
  expect_equal(design$stages[[1]]$draw_spec$prn, "prn_col")
})

test_that("execute() works with custom WOR method", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn)

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_equal(nrow(result), 10)
  expect_true(".weight_1" %in% names(result))
  expect_true(".fpc_1" %in% names(result))
  # WOR: FPC should be finite
  expect_true(all(is.finite(result$.fpc_1)))
})

test_that("execute() works with stratified custom WOR method", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn)

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  # n = 10 per stratum x 2 strata = 20
  expect_equal(nrow(result), 20)
  expect_equal(length(unique(result$region)), 2)
})

test_that("execute() works with custom WR method", {
  on.exit(sondage::unregister_method("test_wr"), add = TRUE)
  sondage::register_method("test_wr", "wr", sample_fn = toy_wr_fn)

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wr", mos = size) |>
    execute(custom_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  # WR: FPC should be Inf
  expect_true(all(is.infinite(result$.fpc_1)))
  # WR: .draw column
  expect_true(".draw_1" %in% names(result))
})

test_that("execute() works with custom random-size PRN method", {
  on.exit(sondage::unregister_method("test_prn"), add = TRUE)
  sondage::register_method(
    "test_prn",
    "wor",
    sample_fn = toy_prn_fn,
    fixed_size = FALSE,
    supports_prn = TRUE
  )

  result <- sampling_design() |>
    draw(frac = 0.3, method = "pps_test_prn", mos = size, prn = prn_col) |>
    execute(custom_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_true(nrow(result) > 0)
})

test_that("certainty selection works with custom WOR method", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn)

  # Extreme skew: one unit dominates
  cert_frame <- data.frame(
    id = 1:20,
    size = c(1000, rep(1, 19))
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_test_wor", mos = size, certainty_prop = 0.5) |>
    execute(cert_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_true(".certainty_1" %in% names(result))
  expect_true(any(result$.certainty_1))
})

test_that("joint_expectation() works with custom WOR method + joint_fn", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method(
    "test_wor",
    "wor",
    sample_fn = toy_wor_fn,
    joint_fn = toy_joint_fn
  )

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  jip <- joint_expectation(result, custom_frame)
  expect_type(jip, "list")
  stage1 <- jip$stage_1
  expect_true(is.matrix(stage1))
  expect_equal(nrow(stage1), 10)
  expect_true(isSymmetric(stage1))
})

test_that("joint_expectation() errors gracefully without joint_fn", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn)

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  expect_error(joint_expectation(result, custom_frame), "not implemented")
})

test_that("as_svydesign() works with custom WOR method", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn)

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  svyd <- as_svydesign(result)
  expect_s3_class(svyd, "survey.design2")
})

test_that("as_svydesign() works with custom WR method", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_wr"), add = TRUE)
  sondage::register_method("test_wr", "wr", sample_fn = toy_wr_fn)

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wr", mos = size) |>
    execute(custom_frame, seed = 1)

  svyd <- as_svydesign(result)
  expect_s3_class(svyd, "survey.design2")
})

test_that("summary() works with custom methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn)

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  out <- capture.output(summary(result))
  expect_true(any(grepl("pps_test_wor", out)))
})

test_that("summary() shows WR label for custom WR method", {
  on.exit(sondage::unregister_method("test_wr"), add = TRUE)
  sondage::register_method("test_wr", "wr", sample_fn = toy_wr_fn)

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wr", mos = size) |>
    execute(custom_frame, seed = 1)

  out <- capture.output(summary(result))
  expect_true(any(grepl("with replacement", out)))
})
