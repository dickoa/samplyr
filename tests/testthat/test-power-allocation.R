make_power_test_frame <- function() {
  data.frame(
    id = 1:100,
    region = c(rep("A", 20), rep("B", 30), rep("C", 50)),
    stringsAsFactors = FALSE
  )
}

region_counts <- function(sample) {
  as.integer(table(factor(sample$region, levels = c("A", "B", "C"))))
}

test_that("power allocation with q = 0 and constant cv matches equal allocation", {
  frame <- make_power_test_frame()

  power_sample <- sampling_design() |>
    stratify_by(
      region,
      alloc = "power",
      cv = c(A = 1, B = 1, C = 1),
      importance = c(A = 20, B = 30, C = 50),
      power = 0
    ) |>
    draw(n = 12) |>
    execute(frame, seed = 1)

  equal_sample <- sampling_design() |>
    stratify_by(region, alloc = "equal") |>
    draw(n = 12) |>
    execute(frame, seed = 1)

  expect_equal(region_counts(power_sample), c(4, 4, 4))
  expect_equal(region_counts(power_sample), region_counts(equal_sample))
})

test_that("power allocation with q = 1 and constant cv matches proportional allocation", {
  frame <- make_power_test_frame()

  power_sample <- sampling_design() |>
    stratify_by(
      region,
      alloc = "power",
      cv = c(A = 1, B = 1, C = 1),
      importance = c(A = 20, B = 30, C = 50),
      power = 1
    ) |>
    draw(n = 20) |>
    execute(frame, seed = 1)

  prop_sample <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 20) |>
    execute(frame, seed = 1)

  expect_equal(region_counts(power_sample), c(4, 6, 10))
  expect_equal(region_counts(power_sample), region_counts(prop_sample))
})

test_that("power allocation can match neyman allocation with equivalent factors", {
  frame <- make_power_test_frame()
  var_df <- data.frame(region = c("A", "B", "C"), var = c(1, 4, 9))

  neyman_sample <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = var_df) |>
    draw(n = 46) |>
    execute(frame, seed = 1)

  power_sample <- sampling_design() |>
    stratify_by(
      region,
      alloc = "power",
      cv = c(A = 1, B = 2, C = 3),
      importance = c(A = 20, B = 30, C = 50),
      power = 1
    ) |>
    draw(n = 46) |>
    execute(frame, seed = 1)

  expect_equal(region_counts(power_sample), c(4, 12, 30))
  expect_equal(region_counts(power_sample), region_counts(neyman_sample))
})

test_that("power allocation is invariant to common scaling of cv", {
  frame <- make_power_test_frame()

  sample_a <- sampling_design() |>
    stratify_by(
      region,
      alloc = "power",
      cv = c(A = 1, B = 2, C = 4),
      importance = c(A = 20, B = 30, C = 50),
      power = 0.5
    ) |>
    draw(n = 24) |>
    execute(frame, seed = 1)

  sample_b <- sampling_design() |>
    stratify_by(
      region,
      alloc = "power",
      cv = c(A = 10, B = 20, C = 40),
      importance = c(A = 20, B = 30, C = 50),
      power = 0.5
    ) |>
    draw(n = 24) |>
    execute(frame, seed = 1)

  expect_equal(region_counts(sample_a), region_counts(sample_b))
})

