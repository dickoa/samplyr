# Tests for execute()

# Create test data
test_frame <- function() {
  set.seed(123)
  data.frame(
    id = 1:1000,
    region = rep(c("North", "South", "East", "West"), each = 250),
    urban_rural = rep(c("Urban", "Rural"), 500),
    school_id = rep(1:100, each = 10),
    enrollment = rep(sample(100:500, 100, replace = TRUE), each = 10),
    value = rnorm(1000)
  )
}

test_that("execute() produces a tbl_sample", {
  frame <- test_frame()
  
  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)
  
  expect_s3_class(result, "tbl_sample")
  expect_true(is_tbl_sample(result))
})

test_that("execute() samples correct number of units", {
  frame <- test_frame()
  
  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)
  
  expect_equal(nrow(result), 100)
})

test_that("execute() adds weight columns", {
  frame <- test_frame()
  
  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)
  
  expect_true(".weight" %in% names(result))
  expect_true(".prob" %in% names(result))
  expect_true(".sample_id" %in% names(result))
})

test_that("execute() weights sum to population", {
  frame <- test_frame()
  N <- nrow(frame)
  
  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)
  
  # Weights should sum approximately to population
  expect_equal(sum(result$.weight), N, tolerance = 0.01)
})

test_that("execute() requires at least one frame", {
  d <- sampling_design() |> draw(n = 100)
  
  expect_error(
    execute(d),
    "data frame"
  )
})

test_that("execute() validates incomplete design", {
  d <- sampling_design() |>
    stratify_by(region)
  
  frame <- test_frame()
  
  expect_error(
    execute(d, frame, seed = 42),
    "missing.*draw"
  )
})

test_that("execute() validates required variables", {
  frame <- data.frame(id = 1:100, x = rnorm(100))
  
  expect_error(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = 10) |>
      execute(frame, seed = 42),
    "not found"
  )
})

test_that("execute() with stratification samples from all strata", {
  frame <- test_frame()
  
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 25) |>
    execute(frame, seed = 42)
  
  # Should have samples from all 4 regions
  expect_equal(length(unique(result$region)), 4)
  
  # Each stratum should have ~25 samples
  counts <- table(result$region)
  expect_true(all(counts == 25))
})

test_that("execute() with proportional allocation", {
  frame <- test_frame()
  
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 200) |>
    execute(frame, seed = 42)
  
  # Each stratum is 250/1000 = 25% of population
  # Should get ~50 from each stratum (200 * 0.25 = 50)
  counts <- table(result$region)
  expect_true(all(counts >= 45 & counts <= 55))  # Allow some rounding
})

test_that("execute() with seed is reproducible", {
  frame <- test_frame()
  design <- sampling_design() |> draw(n = 100)
  
  result1 <- execute(design, frame, seed = 42)
  result2 <- execute(design, frame, seed = 42)
  
  expect_equal(result1$id, result2$id)
})

test_that("execute() without seed gives different results", {
  frame <- test_frame()
  design <- sampling_design() |> draw(n = 100)
  
  result1 <- execute(design, frame)
  result2 <- execute(design, frame)
  
  # Very unlikely to get same sample
expect_false(identical(result1$id, result2$id))
})

test_that("execute() with frac samples correct proportion", {
  frame <- test_frame()
  N <- nrow(frame)
  
  result <- sampling_design() |>
    draw(frac = 0.1) |>
    execute(frame, seed = 42)
  
  expected_n <- ceiling(N * 0.1)
  expect_equal(nrow(result), expected_n)
})

test_that("execute() with cluster_by samples clusters", {
  frame <- test_frame()
  
  result <- sampling_design() |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    execute(frame, seed = 42)
  
  # Should have exactly 20 schools
  n_schools <- length(unique(result$school_id))
  expect_equal(n_schools, 20)
  
  # Each school should have all 10 students
  school_counts <- table(result$school_id)
  expect_true(all(school_counts == 10))
})

test_that("execute() stores design in result", {
  frame <- test_frame()
  design <- sampling_design(title = "Test Survey") |>
    draw(n = 100)
  
  result <- execute(design, frame, seed = 42)
  
  stored_design <- get_design(result)
  expect_equal(stored_design$title, "Test Survey")
})

test_that("execute() with stages parameter", {
  frame <- test_frame()
  
  design <- sampling_design() |>
    stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 20) |>
    stage(label = "Students") |>
      draw(n = 5)
  
  # Execute only stage 1
  result <- execute(design, frame, stages = 1, seed = 42)
  
  stages_exec <- get_stages_executed(result)
  expect_equal(stages_exec, 1)
})

test_that("sample can continue to next stage", {
  frame <- test_frame()
  
  design <- sampling_design() |>
    stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 20) |>
    stage(label = "Students") |>
      draw(n = 5)
  
  # Execute stage 1
  stage1_result <- execute(design, frame, stages = 1, seed = 42)
  
  # Continue to stage 2
  stage2_result <- stage1_result |> execute(frame, seed = 43)
  
  stages_exec <- get_stages_executed(stage2_result)
  expect_equal(stages_exec, c(1, 2))
  
  # Should have 20 schools * 5 students = 100
  expect_equal(nrow(stage2_result), 100)
})

test_that("execute() with round='up' uses ceiling", {
  # Create a frame where 0.1 * N is not an integer
  frame <- data.frame(id = 1:105, x = rnorm(105))
  
  result <- sampling_design() |>
    draw(frac = 0.1, round = "up") |>
    execute(frame, seed = 42)
  
  # 105 * 0.1 = 10.5, ceiling = 11
  expect_equal(nrow(result), 11)
})

test_that("execute() with round='down' uses floor", {
  # Create a frame where 0.1 * N is not an integer
  frame <- data.frame(id = 1:105, x = rnorm(105))
  
  result <- sampling_design() |>
    draw(frac = 0.1, round = "down") |>
    execute(frame, seed = 42)
  
  # 105 * 0.1 = 10.5, floor = 10
  expect_equal(nrow(result), 10)
})

test_that("execute() with round='nearest' uses standard rounding", {
  # Test case where fractional part < 0.5
  frame1 <- data.frame(id = 1:102, x = rnorm(102))
  
  result1 <- sampling_design() |>
    draw(frac = 0.1, round = "nearest") |>
    execute(frame1, seed = 42)
  
  # 102 * 0.1 = 10.2, round = 10
  expect_equal(nrow(result1), 10)
  
  # Test case where fractional part > 0.5
  frame2 <- data.frame(id = 1:108, x = rnorm(108))
  
  result2 <- sampling_design() |>
    draw(frac = 0.1, round = "nearest") |>
    execute(frame2, seed = 42)
  
  # 108 * 0.1 = 10.8, round = 11
  expect_equal(nrow(result2), 11)
})

test_that("execute() with stratified sampling respects round parameter", {
  frame <- data.frame(
    id = 1:100,
    region = c(rep("A", 33), rep("B", 67))
  )
  
  # With round = "up" (ceiling)
  result_up <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = 0.1, round = "up") |>
    execute(frame, seed = 42)
  
  # A: 33 * 0.1 = 3.3, ceiling = 4
  # B: 67 * 0.1 = 6.7, ceiling = 7
  # Total = 11
  expect_equal(nrow(result_up), 11)
  
  # With round = "nearest"
  result_nearest <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = 0.1, round = "nearest") |>
    execute(frame, seed = 42)
  
  # A: 33 * 0.1 = 3.3, round = 3
  # B: 67 * 0.1 = 6.7, round = 7
  # Total = 10
  expect_equal(nrow(result_nearest), 10)
})

test_that("execute() with round='down' ensures minimum of 1 per stratum", {
  # Create frame with tiny stratum
  frame <- data.frame(
    id = 1:110,
    region = c(rep("A", 5), rep("B", 105))  # A is tiny
  )
  
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = 0.1, round = "down") |>
    execute(frame, seed = 42)
  
  counts <- table(result$region)
  
  # A: 5 * 0.1 = 0.5, floor would be 0, but minimum is 1
  expect_true(counts["A"] >= 1)
  
  # B: 105 * 0.1 = 10.5, floor = 10
  expect_equal(as.integer(counts["B"]), 10)
})
