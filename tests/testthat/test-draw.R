# Tests for draw()

test_that("draw() adds selection parameters to design", {
  d <- sampling_design() |>
    draw(n = 100)
  
  expect_s3_class(d, "sampling_design")
  expect_false(is.null(d$stages[[1]]$draw_spec))
  expect_equal(d$stages[[1]]$draw_spec$n, 100)
  expect_equal(d$stages[[1]]$draw_spec$method, "srswor")
})

test_that("draw() accepts frac instead of n", {
  d <- sampling_design() |>
    draw(frac = 0.1)
  
  expect_equal(d$stages[[1]]$draw_spec$frac, 0.1)
  expect_null(d$stages[[1]]$draw_spec$n)
})

test_that("draw() requires n or frac for fixed-size methods", {
  expect_error(
    sampling_design() |> draw(),
    "n.*frac"
  )
  
  expect_error(
    sampling_design() |> draw(method = "srswor"),
    "n.*frac"
  )
})

test_that("draw() rejects both n and frac", {
  expect_error(
    sampling_design() |> draw(n = 100, frac = 0.1),
    "not both"
  )
})

test_that("draw() validates method", {
  # Valid methods
  expect_no_error(sampling_design() |> draw(n = 100, method = "srswor"))
  expect_no_error(sampling_design() |> draw(n = 100, method = "srswr"))
  expect_no_error(sampling_design() |> draw(n = 100, method = "systematic"))
  
  # Invalid method
  expect_error(
    sampling_design() |> draw(n = 100, method = "invalid"),
    "arg"
  )
})

test_that("draw() with bernoulli requires frac", {
  expect_error(
    sampling_design() |> draw(n = 100, method = "bernoulli"),
    "frac.*not.*n"
  )
  
  expect_error(
    sampling_design() |> draw(method = "bernoulli"),
    "frac"
  )
  
  expect_no_error(
    sampling_design() |> draw(frac = 0.1, method = "bernoulli")
  )
})

test_that("draw() with PPS methods requires mos", {
  pps_methods <- c("pps_systematic", "pps_brewer", "pps_maxent", "pps_multinomial")
  
  for (method in pps_methods) {
    expect_error(
      sampling_design() |> draw(n = 100, method = method),
      "mos"
    )
  }
})

test_that("draw() with pps_poisson requires frac", {
  expect_error(
    sampling_design() |> draw(n = 100, method = "pps_poisson", mos = size),
    "frac"
  )
  
  expect_no_error(
    sampling_design() |> draw(frac = 0.1, method = "pps_poisson", mos = size)
  )
})

test_that("draw() with pps_maxent requires n", {
  expect_error(
    sampling_design() |> draw(frac = 0.1, method = "pps_maxent", mos = size),
    "n.*not.*frac"
  )
  
  expect_no_error(
    sampling_design() |> draw(n = 50, method = "pps_maxent", mos = size)
  )
})

test_that("draw() validates n is positive", {
  expect_error(
    sampling_design() |> draw(n = 0),
    "positive"
  )
  
  expect_error(
    sampling_design() |> draw(n = -10),
    "positive"
  )
})

test_that("draw() validates n is integer", {
  expect_error(
    sampling_design() |> draw(n = 10.5),
    "integer"
  )
})

test_that("draw() validates frac is positive", {
  expect_error(
    sampling_design() |> draw(frac = 0),
    "positive"
  )
  
  expect_error(
    sampling_design() |> draw(frac = -0.1),
    "positive"
  )
})

test_that("draw() validates frac <= 1 for WOR methods", {
  expect_error(
    sampling_design() |> draw(frac = 1.5, method = "srswor"),
    "cannot exceed 1"
  )
})

test_that("draw() allows frac > 1 for WR methods", {
  expect_no_error(
    sampling_design() |> draw(frac = 1.5, method = "srswr")
  )
})

test_that("draw() errors on double draw", {
  expect_error(
    sampling_design() |> 
      draw(n = 100) |>
      draw(n = 50),
    "already called"
  )
})

test_that("draw() accepts data frame for n (custom allocation)", {
  sizes_df <- data.frame(region = c("A", "B"), n = c(100, 200))
  
  d <- sampling_design() |>
    stratify_by(region) |>
    draw(n = sizes_df)
  
  expect_true(is.data.frame(d$stages[[1]]$draw_spec$n))
  expect_equal(nrow(d$stages[[1]]$draw_spec$n), 2)
})

test_that("draw() accepts data frame for frac (custom allocation)", {
  rates_df <- data.frame(region = c("A", "B"), frac = c(0.1, 0.2))
  
  d <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = rates_df)
  
  expect_true(is.data.frame(d$stages[[1]]$draw_spec$frac))
  expect_equal(nrow(d$stages[[1]]$draw_spec$frac), 2)
})

test_that("draw() with data frame requires stratification", {
  sizes_df <- data.frame(region = c("A", "B"), n = c(100, 200))
  
  expect_error(
    sampling_design() |> draw(n = sizes_df),
    "stratify"
  )
})

test_that("draw() validates data frame has required columns", {
  # Missing stratification variable
  sizes_df <- data.frame(wrong_col = c("A", "B"), n = c(100, 200))
  
  expect_error(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = sizes_df),
    "missing"
  )
  
  # Missing n column
  sizes_df <- data.frame(region = c("A", "B"), wrong_col = c(100, 200))
  
  expect_error(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = sizes_df),
    "n"
  )
})

test_that("draw() accepts round parameter", {
  d_up <- sampling_design() |>
    draw(frac = 0.1, round = "up")
  expect_equal(d_up$stages[[1]]$draw_spec$round, "up")
  
  d_down <- sampling_design() |>
    draw(frac = 0.1, round = "down")
  expect_equal(d_down$stages[[1]]$draw_spec$round, "down")
  
  d_nearest <- sampling_design() |>
    draw(frac = 0.1, round = "nearest")
  expect_equal(d_nearest$stages[[1]]$draw_spec$round, "nearest")
})

test_that("draw() defaults round to 'up'", {
  d <- sampling_design() |>
    draw(frac = 0.1)
  expect_equal(d$stages[[1]]$draw_spec$round, "up")
})

test_that("draw() validates round parameter", {
  expect_error(
    sampling_design() |> draw(frac = 0.1, round = "invalid"),
    "arg"
  )
  
  expect_error(
    sampling_design() |> draw(frac = 0.1, round = 123),
    "character"
  )
})
