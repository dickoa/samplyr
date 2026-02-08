# Tests for sampling_design()

test_that("sampling_design() creates a valid design object", {
  d <- sampling_design()

  expect_s3_class(d, "sampling_design")
  expect_true(is_sampling_design(d))
  expect_null(d$title)
  expect_equal(length(d$stages), 1)
  expect_equal(d$current_stage, 1L)
})

test_that("sampling_design() accepts a title", {
  d <- sampling_design(title = "My Survey")

  expect_equal(d$title, "My Survey")
})

test_that("sampling_design() rejects invalid title", {
  expect_error(sampling_design(title = 123), "character")
  expect_error(sampling_design(title = c("a", "b")), "single string")
})

test_that("is_sampling_design() works correctly", {
  d <- sampling_design()

  expect_true(is_sampling_design(d))
  expect_false(is_sampling_design(list()))
  expect_false(is_sampling_design(NULL))
  expect_false(is_sampling_design(data.frame()))
})

test_that("sampling_design() can be piped", {
  # Should not error
  result <- sampling_design() |>
    stratify_by(region)

  expect_s3_class(result, "sampling_design")
})
