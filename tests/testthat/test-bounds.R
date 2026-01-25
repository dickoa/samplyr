# Tests for min_n and max_n parameters in draw()

# =============================================================================
# Test Data Setup
# =============================================================================

# Frame with unequal stratum sizes (for testing bounds)
make_unequal_frame <- function() {
  set.seed(42)
  data.frame(
    id = 1:1000,
    region = c(
      rep("Large", 800), # 80% of population
      rep("Medium", 150), # 15% of population
      rep("Small", 50) # 5% of population
    ),
    income = rlnorm(1000, meanlog = 10, sdlog = 0.5)
  )
}

# Variance data for Neyman allocation
make_variance_df <- function() {
  data.frame(
    region = c("Large", "Medium", "Small"),
    var = c(100, 400, 900) # Small stratum has highest variance
  )
}

# =============================================================================
# Parameter Validation Tests
# =============================================================================

test_that("min_n must be a positive integer", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = -1),
    "positive integer"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = 2.5),
    "positive integer"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = c(2, 3)),
    "single positive integer"
  )
})

test_that("max_n must be a positive integer", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, max_n = -1),
    "positive integer"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, max_n = 2.5),
    "positive integer"
  )
})

test_that("min_n cannot exceed max_n", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = 50, max_n = 20),
    "cannot be greater than"
  )
})

test_that("min_n and max_n warn when no allocation method", {
  expect_warning(
    sampling_design() |>
      stratify_by(region) |> # No alloc specified
      draw(n = 100, min_n = 2),
    "only applies when an allocation method"
  )

  expect_warning(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = 100, max_n = 50),
    "only applies when an allocation method"
  )
})

# =============================================================================
# Feasibility Tests
# =============================================================================

test_that("min_n errors when constraint is infeasible", {
  frame <- make_unequal_frame()

  # 3 strata * 50 min = 150 required, but only asking for 100
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = 50) |>
      execute(frame, seed = 42),
    "Cannot satisfy minimum"
  )
})

test_that("max_n errors when constraint is infeasible", {
  frame <- make_unequal_frame()

  # 3 strata * 10 max = 30 allowed, but asking for 100
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, max_n = 10) |>
      execute(frame, seed = 42),
    "Cannot satisfy maximum"
  )
})

# =============================================================================
# min_n Functionality Tests
# =============================================================================

test_that("min_n ensures minimum per stratum with proportional allocation", {
  frame <- make_unequal_frame()

  # Without min_n, proportional would give Small stratum very few units
  # 5% of 100 = 5 units

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 10) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # All strata should have at least min_n
  expect_true(all(counts >= 10))

  # Total should still be exactly 100
  expect_equal(sum(counts), 100)

  # Small stratum should have exactly 10 (raised from ~5)
  expect_equal(as.numeric(counts["Small"]), 10)
})

test_that("min_n ensures minimum per stratum with Neyman allocation", {
  frame <- make_unequal_frame()
  var_df <- make_variance_df()

  # Neyman allocation with high variance in Small stratum
  # But Small has low N_h, so allocation might still be low

  result <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = var_df) |>
    draw(n = 100, min_n = 5) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # All strata should have at least min_n
  expect_true(all(counts >= 5))

  # Total should be exactly 100
  expect_equal(sum(counts), 100)
})

test_that("min_n ensures minimum with equal allocation", {
  frame <- make_unequal_frame()

  # Equal allocation gives 33-34 per stratum for n=100
  # min_n = 40 should force all to at least 40
  # But that requires 120 total, which is infeasible for n=100

  # Test with feasible min_n
  result <- sampling_design() |>
    stratify_by(region, alloc = "equal") |>
    draw(n = 120, min_n = 35) |>
    execute(frame, seed = 42)

  counts <- table(result$region)
  expect_true(all(counts >= 35))
  expect_equal(sum(counts), 120)
})

# =============================================================================
# max_n Functionality Tests
# =============================================================================

test_that("max_n caps large strata with proportional allocation", {
  frame <- make_unequal_frame()

  # Proportional allocation would give Large stratum 80 units (80% of 100)
  # max_n = 50 should cap it

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, max_n = 50) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # No stratum should exceed max_n
  expect_true(all(counts <= 50))

  # Total should still be exactly 100
  expect_equal(sum(counts), 100)

  # Large stratum should be capped at 50 (down from ~80)
  expect_equal(as.numeric(counts["Large"]), 50)
})

test_that("max_n caps with Neyman allocation", {
  frame <- make_unequal_frame()
  var_df <- make_variance_df()

  result <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = var_df) |>
    draw(n = 100, max_n = 40) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # No stratum should exceed max_n
  expect_true(all(counts <= 40))

  # Total should be exactly 100
  expect_equal(sum(counts), 100)
})

# =============================================================================
# Combined min_n and max_n Tests
# =============================================================================

test_that("min_n and max_n work together", {
  frame <- make_unequal_frame()

  # Proportional would give: Large ~80, Medium ~15, Small ~5
  # With min_n=10, max_n=50: Large capped at 50, Small raised to 10

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 10, max_n = 50) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # All within bounds
  expect_true(all(counts >= 10))
  expect_true(all(counts <= 50))

  # Total correct
  expect_equal(sum(counts), 100)

  # Large capped, Small raised
  expect_equal(as.numeric(counts["Large"]), 50)
  expect_gte(as.numeric(counts["Small"]), 10)
})

test_that("tight bounds still work when feasible", {
  frame <- make_unequal_frame()

  # 3 strata, n = 99, min_n = 30, max_n = 35
  # Forces each to be in [30, 35], total must be 99
  # Feasible: 33 + 33 + 33 = 99

  result <- sampling_design() |>
    stratify_by(region, alloc = "equal") |>
    draw(n = 99, min_n = 30, max_n = 35) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  expect_true(all(counts >= 30))
  expect_true(all(counts <= 35))
  expect_equal(sum(counts), 99)
})

# =============================================================================
# Edge Cases
# =============================================================================

test_that("bounds work when stratum size < min_n", {
  # Create frame where one stratum is smaller than desired min_n
  small_frame <- data.frame(
    id = 1:100,
    region = c(rep("A", 80), rep("B", 15), rep("C", 5)) # C has only 5 units
  )

  # Asking for min_n = 10 but C only has 5 units
  # Should cap at population size (take all 5 from C)
  # Effective minimums: A=10, B=10, C=5 (capped at pop)
  # Total minimum = 25, so n=50 is feasible
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 50, min_n = 10) |>
    execute(small_frame, seed = 42)

  counts <- table(result$region)

  # A and B should have at least 10
  expect_gte(as.numeric(counts["A"]), 10)
  expect_gte(as.numeric(counts["B"]), 10)

  # C should have all 5 (capped by population)
  expect_equal(as.numeric(counts["C"]), 5)

  # Total should still be 50
  expect_equal(sum(counts), 50)
})

test_that("bounds work with optimal allocation", {
  frame <- make_unequal_frame()
  var_df <- make_variance_df()
  cost_df <- data.frame(
    region = c("Large", "Medium", "Small"),
    cost = c(1, 2, 3)
  )

  result <- sampling_design() |>
    stratify_by(region, alloc = "optimal", variance = var_df, cost = cost_df) |>
    draw(n = 100, min_n = 5, max_n = 60) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  expect_true(all(counts >= 5))
  expect_true(all(counts <= 60))
  expect_equal(sum(counts), 100)
})

# =============================================================================
# Weights Correctness with Bounds
# =============================================================================

test_that("weights are correct when bounds applied", {
  frame <- make_unequal_frame()

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 10, max_n = 50) |>
    execute(frame, seed = 42)

  # Check weights are inverse of selection probabilities
  for (r in c("Large", "Medium", "Small")) {
    stratum_data <- result[result$region == r, ]
    n_h <- nrow(stratum_data)
    N_h <- sum(frame$region == r)
    expected_weight <- N_h / n_h

    # All units in stratum should have same weight
    expect_equal(length(unique(stratum_data$.weight)), 1)
    expect_equal(stratum_data$.weight[1], expected_weight, tolerance = 0.001)
  }
})

# =============================================================================
# Design Storage Tests
# =============================================================================

test_that("min_n and max_n are stored in design", {
  design <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 5, max_n = 50)

  draw_spec <- design$stages[[1]]$draw_spec

  expect_equal(draw_spec$min_n, 5)
  expect_equal(draw_spec$max_n, 50)
})

test_that("NULL bounds are stored correctly", {
  design <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100)

  draw_spec <- design$stages[[1]]$draw_spec

  expect_null(draw_spec$min_n)
  expect_null(draw_spec$max_n)
})
