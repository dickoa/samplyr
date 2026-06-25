# Coverage for validate_frame() issue-detection branches and input guards.
# Existing tests (test-prn.R, test-edge-cases.R, test-balanced.R) cover PRN
# checks, NA strata/cluster, and balanced aux type, these fill the remaining
# branches: input guards, the happy path, and the missing/type/range issues.

good_frame <- data.frame(
  region = rep(c("N", "S"), each = 10),
  district = rep(letters[1:4], each = 5),
  size = runif(20, 1, 100),
  y = rnorm(20)
)

test_that("validate_frame returns invisibly TRUE for a valid frame", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4)

  expect_true(validate_frame(design, good_frame))
  expect_invisible(validate_frame(design, good_frame))
})

test_that("validate_frame guards its inputs", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4)

  expect_error(validate_frame(list(), good_frame), "sampling_design")
  expect_error(validate_frame(design, 1:5), "data frame")
  expect_error(validate_frame(design, good_frame[0, ]), "0 rows")
})

test_that("validate_frame accepts a valid stage selector", {
  design <- sampling_design() |>
    cluster_by(district) |>
    draw(n = 2, method = "pps_brewer", mos = size) |>
    add_stage() |>
    draw(n = 1)

  expect_true(validate_frame(design, good_frame, stage = 1))
})

test_that("validate_frame detects a missing stratification variable", {
  design <- sampling_design() |>
    stratify_by(zzz) |>
    draw(n = 4)

  expect_error(
    validate_frame(design, good_frame),
    "missing stratification variable"
  )
})

test_that("validate_frame detects a missing cluster variable", {
  design <- sampling_design() |>
    cluster_by(zzz) |>
    draw(n = 2, method = "pps_brewer", mos = size)

  expect_error(
    validate_frame(design, good_frame),
    "missing cluster variable"
  )
})

test_that("validate_frame detects MOS problems", {
  base <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = size)

  # missing
  missing_mos <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = zzz)
  expect_error(validate_frame(missing_mos, good_frame), "missing MOS variable")

  # non-numeric
  type_mos <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = region)
  expect_error(validate_frame(type_mos, good_frame), "must be numeric")

  # NA
  na_frame <- good_frame
  na_frame$size[1] <- NA
  expect_error(validate_frame(base, na_frame), "contains NA values")

  # negative
  neg_frame <- good_frame
  neg_frame$size <- -neg_frame$size
  expect_error(validate_frame(base, neg_frame), "contains negative values")
})

test_that("validate_frame detects auxiliary variable NA values", {
  design <- sampling_design() |>
    draw(n = 2, method = "balanced", aux = size)

  na_frame <- good_frame
  na_frame$size[1] <- NA
  expect_error(
    validate_frame(design, na_frame),
    "auxiliary variable .* contains NA values"
  )
})

test_that("validate_frame detects a missing control variable", {
  design <- sampling_design() |>
    draw(n = 2, method = "systematic", control = zzz)

  expect_error(
    validate_frame(design, good_frame),
    "missing control variable"
  )
})
