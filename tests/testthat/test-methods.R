test_that("fixed equal-probability methods give exact weights", {
  frame <- data.frame(id = 1:100)

  for (method in c("srswor", "systematic")) {
    result <- sampling_design() |>
      draw(n = 10, method = method) |>
      execute(frame, seed = 42)
    expect_equal(nrow(result), 10L, label = paste(method, "cardinality"))
    expect_equal(result$.weight, rep(10, 10), label = paste(method, "weight"))
  }
})

test_that("SRS with frac gives correct weight", {
  frame <- data.frame(id = 1:200)

  result <- sampling_design() |>
    draw(frac = 0.25, method = "srswor") |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 50) # 200 * 0.25
  expect_true(all(result$.weight == 4))
})

test_that("Stratified SRS gives within-stratum weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), c(100, 200)),
    id = 1:300
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 10, method = "srswor") |>
    execute(frame, seed = 42)

  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]

  # Stratum A: N_A/n = 100/10 = 10
  expect_true(all(result_A$.weight == 10))

  # Stratum B: N_B/n = 200/10 = 20
  expect_true(all(result_B$.weight == 20))
})

test_that("Systematic sampling produces evenly spaced samples", {
  frame <- data.frame(id = 1:100)

  result <- sampling_design() |>
    draw(n = 10, method = "systematic") |>
    execute(frame, seed = 42)

  # IDs should be roughly evenly spaced (interval ~10)
  sorted_ids <- sort(result$id)
  gaps <- diff(sorted_ids)

  # Most gaps should be close to interval (allowing for rounding)
  expect_true(all(gaps >= 9 & gaps <= 11))
})

test_that("Stratified systematic gives within-stratum weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 50),
    id = 1:100
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "systematic") |>
    execute(frame, seed = 42)

  # Each stratum: N_h/n = 50/5 = 10
  expect_true(all(result$.weight == 10))
})

test_that("Bernoulli sampling gives correct weights", {
  frame <- data.frame(id = 1:100)

  result <- sampling_design() |>
    draw(frac = 0.3, method = "bernoulli") |>
    execute(frame, seed = 42)

  # All selected units have weight = 1/frac
  expect_true(all(abs(result$.weight - 1 / 0.3) < 1e-10))
})

test_that("Bernoulli has random realized cardinality", {
  frame <- data.frame(
    id = 1:20,
    u4 = c(rep(0.1, 4), rep(0.9, 16)),
    u6 = c(rep(0.1, 6), rep(0.9, 14))
  )
  draw_with <- function(prn) {
    sampling_design() |>
      draw(frac = 0.25, method = "bernoulli", prn = {{ prn }}) |>
      execute(frame)
  }

  expect_equal(nrow(draw_with(u4)), 4L)
  expect_equal(nrow(draw_with(u6)), 6L)
})

test_that("Stratified Bernoulli gives stratum-specific weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 50),
    id = 1:100
  )

  # Different fractions per stratum
  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = c("A" = 0.2, "B" = 0.5), method = "bernoulli") |>
    execute(frame, seed = 123)

  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]

  if (nrow(result_A) > 0) {
    expect_true(all(result_A$.weight == 5))
  }

  if (nrow(result_B) > 0) {
    expect_true(all(result_B$.weight == 2))
  }
})

test_that("Bernoulli with n gives expected sample size and correct weights", {
  frame <- data.frame(
    id = 1:1000,
    u = c(rep(0.05, 100), rep(0.5, 900))
  )

  result <- sampling_design() |>
    draw(n = 100, method = "bernoulli", prn = u) |>
    execute(frame)

  expect_equal(nrow(result), 100L)
  expect_equal(result$.weight, rep(10, 100))
})

test_that("pps_poisson with n gives correct weights", {
  frame <- data.frame(
    id = 1:100,
    size = 1,
    u = c(rep(0.1, 20), rep(0.9, 80))
  )

  result <- sampling_design() |>
    draw(n = 20, method = "pps_poisson", mos = size, prn = u) |>
    execute(frame)

  expect_equal(nrow(result), 20L)
  expect_equal(result$.weight, rep(5, 20))
})

test_that("Stratified bernoulli with scalar n uses n per stratum", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 500),
    id = 1:1000,
    u = rep(c(rep(0.05, 50), rep(0.5, 450)), 2)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 50, method = "bernoulli", prn = u) |>
    execute(frame)

  expect_equal(as.integer(table(result$stratum)), c(50L, 50L))
  expect_equal(result$.weight, rep(10, 100))
})

test_that("Stratified bernoulli with named vector n", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), c(200, 800)),
    id = 1:1000,
    u = c(
      rep(0.05, 20), rep(0.5, 180),
      rep(0.05, 80), rep(0.5, 720)
    )
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = c(A = 20, B = 80), method = "bernoulli", prn = u) |>
    execute(frame)

  expect_equal(as.integer(table(result$stratum)), c(20L, 80L))
  expect_equal(result$.weight, rep(10, 100))
})

test_that("bernoulli errors when both n and frac provided", {
  expect_error(
    sampling_design() |>
      draw(n = 50, frac = 0.1, method = "bernoulli"),
    "not both"
  )
})

test_that("pps_poisson errors when both n and frac provided", {
  expect_error(
    sampling_design() |>
      draw(n = 50, frac = 0.1, method = "pps_poisson", mos = size),
    "not both"
  )
})

test_that("bernoulli errors when neither n nor frac provided", {
  expect_error(
    sampling_design() |>
      draw(method = "bernoulli"),
    "requires.*n.*or.*frac"
  )
})

test_that("pps_poisson errors when neither n nor frac provided", {
  expect_error(
    sampling_design() |>
      draw(method = "pps_poisson", mos = size),
    "requires.*n.*or.*frac"
  )
})
