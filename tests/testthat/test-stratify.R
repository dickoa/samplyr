# Tests for stratify_by()

test_that("stratify_by() adds stratification to design", {
  d <- sampling_design() |>
    stratify_by(region)
  
  expect_s3_class(d, "sampling_design")
  expect_false(is.null(d$stages[[1]]$strata))
  expect_equal(d$stages[[1]]$strata$vars, "region")
})

test_that("stratify_by() accepts multiple variables", {
  d <- sampling_design() |>
    stratify_by(region, urban_rural)
  
  expect_equal(d$stages[[1]]$strata$vars, c("region", "urban_rural"))
})

test_that("stratify_by() requires at least one variable", {
  expect_error(
    sampling_design() |> stratify_by(),
    "At least one"
  )
})

test_that("stratify_by() validates allocation method", {
  # Valid allocations
  expect_no_error(sampling_design() |> stratify_by(region, alloc = "equal"))
  expect_no_error(sampling_design() |> stratify_by(region, alloc = "proportional"))
  
  # Invalid allocation
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "invalid"),
    "arg"
  )
})

test_that("stratify_by() with neyman requires variance", {
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "neyman"),
    "variance"
  )
  
  var_df <- data.frame(region = c("A", "B"), var = c(1, 2))
  expect_no_error(
    sampling_design() |> stratify_by(region, alloc = "neyman", variance = var_df)
  )
})

test_that("stratify_by() with optimal requires variance and cost", {
  var_df <- data.frame(region = c("A", "B"), var = c(1, 2))
  cost_df <- data.frame(region = c("A", "B"), cost = c(10, 20))
  
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "optimal"),
    "variance"
  )
  
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "optimal", variance = var_df),
    "cost"
  )
  
  expect_no_error(
    sampling_design() |> stratify_by(region, alloc = "optimal", variance = var_df, cost = cost_df)
  )
})

test_that("stratify_by() validates auxiliary data frames", {
  # Missing stratification variable
  var_df <- data.frame(wrong_col = c("A", "B"), var = c(1, 2))
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "neyman", variance = var_df),
    "missing"
  )
  
  # Missing value column
  var_df <- data.frame(region = c("A", "B"), wrong_col = c(1, 2))
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "neyman", variance = var_df),
    "var"
  )
})

test_that("stratify_by() errors on double stratification", {
  expect_error(
    sampling_design() |> 
      stratify_by(region) |>
      stratify_by(urban_rural),
    "already defined"
  )
})
