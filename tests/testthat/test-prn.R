test_that("PRN with bernoulli gives deterministic selection", {
  frame <- data.frame(
    id = 1:50,
    u = runif(50)
  )

  r1 <- sampling_design() |>
    draw(frac = 0.3, method = "bernoulli", prn = u) |>
    execute(frame, seed = 1)

  r2 <- sampling_design() |>
    draw(frac = 0.3, method = "bernoulli", prn = u) |>
    execute(frame, seed = 999)

  expect_equal(sort(r1$id), sort(r2$id))
})

test_that("PRN with pps_poisson gives deterministic selection", {
  frame <- data.frame(
    id = 1:50,
    size = sample(10:100, 50),
    u = runif(50)
  )

  r1 <- sampling_design() |>
    draw(frac = 0.3, method = "pps_poisson", mos = size, prn = u) |>
    execute(frame, seed = 1)

  r2 <- sampling_design() |>
    draw(frac = 0.3, method = "pps_poisson", mos = size, prn = u) |>
    execute(frame, seed = 999)

  expect_equal(sort(r1$id), sort(r2$id))
})

test_that("PRN with pps_sps gives deterministic fixed-size selection", {
  frame <- data.frame(
    id = 1:50,
    size = sample(10:100, 50),
    u = runif(50)
  )

  r1 <- sampling_design() |>
    draw(n = 10, method = "pps_sps", mos = size, prn = u) |>
    execute(frame, seed = 1)

  r2 <- sampling_design() |>
    draw(n = 10, method = "pps_sps", mos = size, prn = u) |>
    execute(frame, seed = 999)

  expect_equal(nrow(r1), 10)
  expect_equal(nrow(r2), 10)
  expect_equal(sort(r1$id), sort(r2$id))
})

test_that("PRN with pps_pareto gives deterministic fixed-size selection", {
  frame <- data.frame(
    id = 1:50,
    size = sample(10:100, 50),
    u = runif(50)
  )

  r1 <- sampling_design() |>
    draw(n = 10, method = "pps_pareto", mos = size, prn = u) |>
    execute(frame, seed = 1)

  r2 <- sampling_design() |>
    draw(n = 10, method = "pps_pareto", mos = size, prn = u) |>
    execute(frame, seed = 999)

  expect_equal(nrow(r1), 10)
  expect_equal(nrow(r2), 10)
  expect_equal(sort(r1$id), sort(r2$id))
})

test_that("PRN rejects incompatible methods", {
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "srswor", prn = u),
    "prn.*only supported"
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size, prn = u),
    "prn.*only supported"
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_cps", mos = size, prn = u),
    "prn.*only supported"
  )
})

test_that("PRN validates column existence at execution", {
  frame <- data.frame(id = 1:20, size = runif(20, 10, 100))

  expect_error(
    sampling_design() |>
      draw(n = 5, method = "pps_sps", mos = size, prn = missing_col) |>
      execute(frame, seed = 1),
    "not found"
  )
})

test_that("PRN validates no NAs at execution", {
  frame <- data.frame(
    id = 1:10,
    size = 10:19,
    u = c(runif(9), NA)
  )

  expect_error(
    sampling_design() |>
      draw(n = 5, method = "pps_sps", mos = size, prn = u) |>
      execute(frame, seed = 1),
    "NA"
  )
})

test_that("PRN validates range (0, 1) at execution", {
  frame_low <- data.frame(
    id = 1:10,
    size = 10:19,
    u = c(0, runif(9))
  )
  frame_high <- data.frame(
    id = 1:10,
    size = 10:19,
    u = c(runif(9), 1)
  )

  expect_error(
    sampling_design() |>
      draw(n = 5, method = "pps_sps", mos = size, prn = u) |>
      execute(frame_low, seed = 1),
    "\\(0, 1\\)"
  )
  expect_error(
    sampling_design() |>
      draw(n = 5, method = "pps_sps", mos = size, prn = u) |>
      execute(frame_high, seed = 1),
    "\\(0, 1\\)"
  )
})

test_that("PRN validates numeric type at execution", {
  frame <- data.frame(
    id = 1:10,
    size = 10:19,
    u = letters[1:10]
  )

  expect_error(
    sampling_design() |>
      draw(n = 5, method = "pps_sps", mos = size, prn = u) |>
      execute(frame, seed = 1),
    "numeric"
  )
})

test_that("pps_sps works without PRN", {
  frame <- data.frame(
    id = 1:20,
    size = sample(10:100, 20)
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_sps", mos = size) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 5)
  expect_true(all(result$.weight > 0))
  expect_true(all(is.finite(result$.weight)))
})

test_that("pps_pareto works without PRN", {
  frame <- data.frame(
    id = 1:20,
    size = sample(10:100, 20)
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_pareto", mos = size) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 5)
  expect_true(all(result$.weight > 0))
  expect_true(all(is.finite(result$.weight)))
})

test_that("PRN with pps_sps works under stratification", {
  frame <- data.frame(
    id = 1:40,
    stratum = rep(c("A", "B"), each = 20),
    size = sample(10:100, 40),
    u = runif(40)
  )

  r1 <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "pps_sps", mos = size, prn = u) |>
    execute(frame, seed = 1)

  r2 <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "pps_sps", mos = size, prn = u) |>
    execute(frame, seed = 999)

  expect_equal(nrow(r1), 10)
  expect_equal(sort(r1$id), sort(r2$id))
})

test_that("prn is stored in draw_spec", {
  d <- sampling_design() |>
    draw(n = 10, method = "pps_sps", mos = size, prn = u)

  expect_equal(d$stages[[1]]$draw_spec$prn, "u")
})

test_that("prn defaults to NULL in draw_spec", {
  d <- sampling_design() |>
    draw(n = 10, method = "pps_sps", mos = size)

  expect_null(d$stages[[1]]$draw_spec$prn)
})

test_that("validate_frame detects PRN issues", {
  design <- sampling_design() |>
    draw(n = 5, method = "pps_sps", mos = size, prn = u)

  # Missing PRN variable
  frame_no_prn <- data.frame(id = 1:10, size = 10:19)
  expect_error(validate_frame(design, frame_no_prn), "PRN")

  # Non-numeric PRN
  frame_char_prn <- data.frame(id = 1:10, size = 10:19, u = letters[1:10])
  expect_error(validate_frame(design, frame_char_prn), "numeric")

  # PRN with NAs
  frame_na_prn <- data.frame(id = 1:10, size = 10:19, u = c(runif(9), NA))
  expect_error(validate_frame(design, frame_na_prn), "NA")

  # PRN out of range
  frame_bad_prn <- data.frame(
    id = 1:10,
    size = 10:19,
    u = seq(0, 0.9, length.out = 10)
  )
  expect_error(validate_frame(design, frame_bad_prn), "\\(0, 1\\)")
})

test_that("prn is displayed in print output", {
  d <- sampling_design() |>
    draw(n = 10, method = "pps_sps", mos = size, prn = u)

  output <- capture.output(print(d))
  expect_true(any(grepl("prn = u", output)))
})
