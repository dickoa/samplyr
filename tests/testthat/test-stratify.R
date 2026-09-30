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
  expect_no_error(
    sampling_design() |> stratify_by(region, alloc = "proportional")
  )
  expect_no_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = c(A = 0.2, B = 0.3),
        importance = c(A = 10, B = 20),
        power = 0.5
      )
  )

  # Invalid allocation
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "invalid"),
    class = "samplyr_error_alloc_unknown_method"
  )
})

test_that("stratify_by() with neyman requires variance", {
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "neyman"),
    "variance"
  )

  var_df <- data.frame(region = c("A", "B"), var = c(1, 2))
  expect_no_error(
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = var_df)
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
    sampling_design() |>
      stratify_by(region, alloc = "optimal", variance = var_df),
    "cost"
  )

  expect_no_error(
    sampling_design() |>
      stratify_by(region, alloc = "optimal", variance = var_df, cost = cost_df)
  )
})

test_that("stratify_by() with power requires cv and importance", {
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "power"),
    "cv"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "power", cv = c(A = 0.2, B = 0.3)),
    "importance"
  )

  expect_no_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = c(A = 0.2, B = 0.3),
        importance = c(A = 10, B = 20)
      )
  )
})

test_that("stratify_by() validates power exponent", {
  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = c(A = 0.2, B = 0.3),
        importance = c(A = 10, B = 20),
        power = -0.1
      ),
    "between 0 and 1"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = c(A = 0.2, B = 0.3),
        importance = c(A = 10, B = 20),
        power = Inf
      ),
    "single finite number"
  )
})

test_that("stratify_by() validates auxiliary data frames", {
  # Missing stratification variable
  var_df <- data.frame(wrong_col = c("A", "B"), var = c(1, 2))
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = var_df),
    "missing"
  )

  # Missing value column
  var_df <- data.frame(region = c("A", "B"), wrong_col = c(1, 2))
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = var_df),
    "var"
  )
})

test_that("stratify_by() rejects missing auxiliary key values", {
  var_df <- data.frame(region = c("A", NA), var = c(1, 2))
  cost_df <- data.frame(region = c("A", NA), cost = c(10, 20))

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = var_df),
    "missing values in stratification keys"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "optimal", variance = c(A = 1, B = 2), cost = cost_df),
    "missing values in stratification keys"
  )
})

test_that("stratify_by() enforces finite numeric auxiliary values", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = data.frame(
        region = c("A", "B"),
        var = c(1, Inf)
      )),
    "finite numeric"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "optimal",
                  variance = c(A = 1, B = 2),
                  cost = data.frame(region = c("A", "B"), cost = c(5, NA))),
    "finite numeric"
  )
})

test_that("stratify_by() validates auxiliary value bounds", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = data.frame(
        region = c("A", "B"),
        var = c(1, -1)
      )),
    "non-negative"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "optimal",
                  variance = c(A = 1, B = 2),
                  cost = data.frame(region = c("A", "B"), cost = c(10, 0))),
    "positive"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = c(A = 0, B = 0.2),
        importance = c(A = 10, B = 20)
      ),
    "cv.*positive"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = c(A = 0.1, B = 0.2),
        importance = c(A = 0, B = 20)
      ),
    "importance.*positive"
  )
})

test_that("stratify_by() trims extra columns from variance and cost data frames", {
  # A combined data frame with both var and cost columns
  combined_df <- data.frame(
    region = c("A", "B"),
    var = c(1, 2),
    cost = c(10, 20)
  )

  d <- sampling_design() |>
    stratify_by(
      region,
      alloc = "optimal",
      variance = combined_df,
      cost = combined_df
    )

  # Stored variance df should only have region + var

  expect_equal(names(d$stages[[1]]$strata$variance), c("region", "var"))
  # Stored cost df should only have region + cost
  expect_equal(names(d$stages[[1]]$strata$cost), c("region", "cost"))
})

test_that("stratify_by() accepts named vectors for variance and cost", {
  d <- sampling_design() |>
    stratify_by(
      region,
      alloc = "optimal",
      variance = c(A = 1, B = 2),
      cost = c(A = 10, B = 20)
    )

  var_df <- d$stages[[1]]$strata$variance
  cost_df <- d$stages[[1]]$strata$cost

  expect_s3_class(var_df, "data.frame")
  expect_equal(names(var_df), c("region", "var"))
  expect_equal(var_df$region, c("A", "B"))
  expect_equal(var_df$var, c(1, 2))

  expect_s3_class(cost_df, "data.frame")
  expect_equal(names(cost_df), c("region", "cost"))
  expect_equal(cost_df$cost, c(10, 20))
})

test_that("stratify_by() accepts named vectors for power auxiliaries", {
  d <- sampling_design() |>
    stratify_by(
      region,
      alloc = "power",
      cv = c(A = 0.2, B = 0.4),
      importance = c(A = 10, B = 20),
      power = 0.75
    )

  cv_df <- d$stages[[1]]$strata$cv
  imp_df <- d$stages[[1]]$strata$importance

  expect_s3_class(cv_df, "data.frame")
  expect_equal(names(cv_df), c("region", "cv"))
  expect_equal(cv_df$cv, c(0.2, 0.4))

  expect_s3_class(imp_df, "data.frame")
  expect_equal(names(imp_df), c("region", "importance"))
  expect_equal(imp_df$importance, c(10, 20))
  expect_equal(d$stages[[1]]$strata$power, 0.75)
})

test_that("stratify_by() rejects named vectors with multiple strat vars", {
  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        urban_rural,
        alloc = "neyman",
        variance = c(A = 1, B = 2)
      ),
    "single stratification variable"
  )
})

test_that("stratify_by() rejects unnamed vectors", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = c(1, 2)),
    "named numeric vector"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = c(1, 2)),
    "use names as stratum levels"
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

test_that("execute() rejects duplicate auxiliary keys if design is tampered", {
  frame <- data.frame(
    id = 1:60,
    region = rep(c("A", "B"), each = 30)
  )

  design <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = data.frame(
      region = c("A", "B"),
      var = c(10, 20)
    )) |>
    draw(n = 20)

  design$stages[[1]]$strata$variance <- data.frame(
    region = c("A", "A", "B"),
    var = c(10, 12, 20)
  )

  expect_error(
    execute(design, frame, seed = 1),
    "duplicate rows for the same stratum"
  )
})

test_that("execute() rejects duplicate keys in tampered custom n data frame", {
  frame <- data.frame(
    id = 1:60,
    region = rep(c("A", "B"), each = 30)
  )

  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = data.frame(region = c("A", "B"), n = c(8, 12)))

  design$stages[[1]]$draw_spec$n <- data.frame(
    region = c("A", "A", "B"),
    n = c(8, 9, 12)
  )

  expect_error(
    execute(design, frame, seed = 1),
    "duplicate rows for the same stratum"
  )
})

test_that("execute() rejects invalid keys/values in tampered custom frac data frame", {
  frame <- data.frame(
    id = 1:60,
    region = rep(c("A", "B"), each = 30)
  )

  design <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = data.frame(region = c("A", "B"), frac = c(0.2, 0.3)))

  design_bad_keys <- design
  design_bad_keys$stages[[1]]$draw_spec$frac <- data.frame(
    region = c("A", NA),
    frac = c(0.2, 0.3)
  )

  expect_error(
    execute(design_bad_keys, frame, seed = 1),
    "missing values in stratification keys"
  )

  design_bad_vals <- design
  design_bad_vals$stages[[1]]$draw_spec$frac <- data.frame(
    region = c("A", "B"),
    frac = c(0.2, 1.2)
  )

  expect_error(
    execute(design_bad_vals, frame, seed = 1),
    "cannot exceed 1"
  )
})

## Proportional allocation to a size

test_that("proportional allocation follows importance when it is given", {
  frame <- data.frame(
    id = 1:100,
    region = rep(c("A", "B", "C"), times = c(20, 30, 50))
  )
  size <- c(A = 10, B = 30, C = 60)
  counts <- function(s) c(table(factor(s$region, levels = c("A", "B", "C"))))
  by_size <- sampling_design() |>
    stratify_by(region, alloc = "proportional", importance = size) |>
    draw(n = 12)

  # 12 * size / 100 is 1.2, 3.6, 7.2. Largest remainders give B the last unit.
  sample <- execute(by_size, frame, seed = 1)
  expect_identical(counts(sample), c(A = 1L, B = 4L, C = 7L))

  # Power allocation with every cv 1 and power 1 is the same rule.
  recipe <- sampling_design() |>
    stratify_by(region, alloc = "power", cv = c(A = 1, B = 1, C = 1),
                importance = size, power = 1) |>
    draw(n = 12)
  expect_identical(counts(execute(recipe, frame, seed = 1)), counts(sample))

  # Without importance the shares follow the 20, 30, 50 units.
  by_count <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 12)
  expect_identical(
    counts(execute(by_count, frame, seed = 1)),
    c(A = 2L, B = 4L, C = 6L)
  )

  expect_match(
    paste(utils::capture.output(print(by_size)), collapse = "\n"),
    "region (proportional to importance)",
    fixed = TRUE
  )

  path <- withr::local_tempfile(fileext = ".json")
  write_design(by_size, path)
  expect_identical(counts(execute(read_design(path), frame, seed = 1)),
                   counts(sample))

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional",
                  importance = c(A = 0, B = 30, C = 60)) |>
      draw(n = 12) |>
      execute(frame, seed = 1),
    class = "samplyr_error_aux_importance_bounds"
  )
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional",
                  importance = c(A = 10, B = 30)) |>
      draw(n = 12) |>
      execute(frame, seed = 1),
    class = "samplyr_error_aux_missing_coverage"
  )
})
