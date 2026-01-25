# Integration Tests - Full Sampling Workflows

# Test data generators
make_population_frame <- function(n = 10000) {
  set.seed(42)
  regions <- c("North", "South", "East", "West")
  urban_rural <- c("Urban", "Rural")
  
  data.frame(
    id = seq_len(n),
    region = sample(regions, n, replace = TRUE, 
                    prob = c(0.3, 0.25, 0.25, 0.2)),
    urban_rural = sample(urban_rural, n, replace = TRUE,
                         prob = c(0.6, 0.4)),
    income = rlnorm(n, meanlog = 10, sdlog = 0.5),
    age = round(runif(n, 18, 75))
  )
}

make_school_frame <- function() {
  set.seed(42)
  n_schools <- 200
  n_students_per_school <- sample(50:500, n_schools, replace = TRUE)
  
  schools <- data.frame(
    school_id = seq_len(n_schools),
    region = sample(c("North", "South", "East", "West"), n_schools, replace = TRUE),
    enrollment = n_students_per_school,
    school_type = sample(c("Public", "Private"), n_schools, replace = TRUE,
                         prob = c(0.7, 0.3))
  )
  
  # Expand to students
  students <- do.call(rbind, lapply(seq_len(n_schools), function(i) {
    data.frame(
      student_id = paste0("S", i, "_", seq_len(n_students_per_school[i])),
      school_id = i,
      region = schools$region[i],
      enrollment = schools$enrollment[i],
      school_type = schools$school_type[i],
      grade = sample(1:12, n_students_per_school[i], replace = TRUE),
      score = rnorm(n_students_per_school[i], mean = 70, sd = 15)
    )
  }))
  
  students
}

# ============================================================================
# Test 1: Simple Random Sample
# ============================================================================
test_that("Simple random sample workflow", {
  frame <- make_population_frame()
  
  result <- sampling_design() |>
    draw(n = 500) |>
    execute(frame, seed = 42)
  
  expect_equal(nrow(result), 500)
  expect_true(".weight" %in% names(result))
  
  # Weights should average to population/sample
  expect_equal(mean(result$.weight), nrow(frame) / 500, tolerance = 0.01)
})

# ============================================================================
# Test 2: Stratified Sample with Proportional Allocation
# ============================================================================
test_that("Stratified proportional allocation workflow", {
  frame <- make_population_frame()
  
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 1000) |>
    execute(frame, seed = 42)
  
  # Sample should reflect population proportions
  pop_props <- table(frame$region) / nrow(frame)
  sample_props <- table(result$region) / nrow(result)
  
  # Should be close (within 5% absolute)
  for (r in names(pop_props)) {
    expect_equal(sample_props[r], pop_props[r], tolerance = 0.05)
  }
})

# ============================================================================
# Test 3: Stratified Sample with Neyman Allocation
# ============================================================================
test_that("Stratified Neyman allocation workflow", {
  frame <- make_population_frame()
  
  # Calculate stratum variances
  var_df <- aggregate(income ~ region, data = frame, FUN = var)
  names(var_df)[2] <- "var"
  
  result <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = var_df) |>
    draw(n = 1000) |>
    execute(frame, seed = 42)
  
  expect_true(nrow(result) >= 950)  # Allow for rounding
  expect_true(all(levels(factor(frame$region)) %in% result$region))
})

# ============================================================================
# Test 4: Cluster Sample
# ============================================================================
test_that("Cluster sample workflow", {
  frame <- make_school_frame()
  n_schools <- length(unique(frame$school_id))
  
  result <- sampling_design() |>
    cluster_by(school_id) |>
    draw(n = 30) |>
    execute(frame, seed = 42)
  
  # Should have exactly 30 schools
  expect_equal(length(unique(result$school_id)), 30)
  
  # All students from selected schools should be included
  selected_schools <- unique(result$school_id)
  for (school in selected_schools) {
    expected_n <- sum(frame$school_id == school)
    actual_n <- sum(result$school_id == school)
    expect_equal(actual_n, expected_n)
  }
})

# ============================================================================
# Test 5: Stratified Cluster Sample
# ============================================================================
test_that("Stratified cluster sample workflow", {
  frame <- make_school_frame()
  
  result <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(school_id) |>
    draw(n = 10) |>  # 10 schools per region
    execute(frame, seed = 42)
  
  # Should have 10 schools from each of 4 regions = 40 schools
  schools_per_region <- tapply(
    result$school_id,
    result$region,
    function(x) length(unique(x))
  )
  
  expect_true(all(schools_per_region == 10))
})

# ============================================================================
# Test 6: Two-Stage Cluster Sample
# ============================================================================
test_that("Two-stage cluster sample workflow", {
  frame <- make_school_frame()
  
  result <- sampling_design() |>
    stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 40) |>
    stage(label = "Students") |>
      draw(n = 25) |>
    execute(frame, seed = 42)
  
  # Should have 40 schools * 25 students = 1000
  expect_equal(nrow(result), 1000)
  expect_equal(length(unique(result$school_id)), 40)
  
  # Each school should have exactly 25 students
  students_per_school <- table(result$school_id)
  expect_true(all(students_per_school == 25))
})

# ============================================================================
# Test 7: Custom Allocation
# ============================================================================
test_that("Custom allocation workflow with data frame in draw()", {
  frame <- make_population_frame()
  
  # Custom sizes via data frame in draw()
  sizes_df <- data.frame(
    region = c("North", "South", "East", "West"),
    n = c(400, 300, 200, 100)
  )
  
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = sizes_df) |>
    execute(frame, seed = 42)
  
  counts <- table(result$region)
  expect_equal(as.numeric(counts["North"]), 400)
  expect_equal(as.numeric(counts["South"]), 300)
  expect_equal(as.numeric(counts["East"]), 200)
  expect_equal(as.numeric(counts["West"]), 100)
})

test_that("Custom allocation workflow with frac data frame", {
  frame <- make_population_frame()
  
  # Custom rates via data frame in draw()
  rates_df <- data.frame(
    region = c("North", "South", "East", "West"),
    frac = c(0.10, 0.15, 0.20, 0.25)
  )
  
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = rates_df) |>
    execute(frame, seed = 42)
  
  # Check approximate rates
  for (r in c("North", "South", "East", "West")) {
    pop_n <- sum(frame$region == r)
    sample_n <- sum(result$region == r)
    expected_frac <- rates_df$frac[rates_df$region == r]
    actual_frac <- sample_n / pop_n
    expect_equal(actual_frac, expected_frac, tolerance = 0.02)
  }
})

# ============================================================================
# Test 8: Systematic Sampling
# ============================================================================
test_that("Systematic sampling workflow", {
  frame <- make_population_frame()
  
  result <- sampling_design() |>
    draw(n = 500, method = "systematic") |>
    execute(frame, seed = 42)
  
  expect_equal(nrow(result), 500)
})

# ============================================================================
# Test 9: Different Rates per Stratum
# ============================================================================
test_that("Different sampling rates per stratum", {
  frame <- make_population_frame()
  
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = c(North = 0.2, South = 0.15, East = 0.1, West = 0.05)) |>
    execute(frame, seed = 42)
  
  # Check approximate rates
  for (r in c("North", "South", "East", "West")) {
    pop_n <- sum(frame$region == r)
    sample_n <- sum(result$region == r)
    expected_frac <- c(North = 0.2, South = 0.15, East = 0.1, West = 0.05)[[r]]
    actual_frac <- sample_n / pop_n
    expect_equal(actual_frac, expected_frac, tolerance = 0.02)
  }
})

# ============================================================================
# Test 10: Multi-Stage with Separate Execution
# ============================================================================
test_that("Multi-stage with separate execution workflow", {
  frame <- make_school_frame()
  
  # Define design
  design <- sampling_design() |>
    stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 30) |>
    stage(label = "Students") |>
      draw(n = 20)
  
  # Execute stage 1 only
  stage1 <- execute(design, frame, stages = 1, seed = 42)
  expect_equal(length(unique(stage1$school_id)), 30)
  
  # Execute stage 2 (continuation)
  final <- stage1 |> execute(frame, seed = 43)
  expect_equal(nrow(final), 30 * 20)
  expect_equal(get_stages_executed(final), c(1, 2))
})

# ============================================================================
# Test 11: Print Methods
# ============================================================================
test_that("Print methods work", {
  design <- sampling_design(title = "Test Survey") |>
    stratify_by(region, alloc = "proportional") |>
    cluster_by(school_id) |>
    draw(n = 100)
  
  # Should not error
  expect_output(print(design), "Sampling Design")
  expect_output(print(design), "Test Survey")
})

# ============================================================================
# Test 12: Weight Correctness
# ============================================================================
test_that("Weights are mathematically correct", {
  frame <- make_population_frame(n = 1000)
  N <- nrow(frame)
  n <- 100
  
  result <- sampling_design() |>
    draw(n = n) |>
    execute(frame, seed = 42)
  
  # For SRS: weight = N/n
  expected_weight <- N / n
  expect_equal(unique(result$.weight), expected_weight)
  
  # Probability = n/N
  expected_prob <- n / N
  expect_equal(unique(result$.prob), expected_prob)
  
  # Weight * prob = 1
  expect_equal(unique(result$.weight * result$.prob), 1)
})
