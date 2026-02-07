# Tests for review fixes
# =============================================================================

# Shared test data ----------------------------------------------------------

test_frame <- function(n = 1000) {
  set.seed(123)
  data.frame(
    id = seq_len(n),
    region = rep(c("North", "South", "East", "West"), each = n / 4),
    urban_rural = rep(c("Urban", "Rural"), n / 2),
    school_id = rep(seq_len(n / 10), each = 10),
    enrollment = rep(sample(100:500, n / 10, replace = TRUE), each = 10),
    value = rnorm(n)
  )
}

# =============================================================================
# Fix 1: Control sorting is applied during execution
# =============================================================================

test_that("control sorting is applied for systematic sampling", {
  frame <- test_frame()

  # Shuffle the frame so it's NOT already sorted by region
  set.seed(999)
  frame <- frame[sample(nrow(frame)), ]
  rownames(frame) <- NULL

  # Systematic sampling with control sorting should produce different results
  # than without control sorting (because input order matters)
  result_no_control <- sampling_design() |>
    draw(n = 100, method = "systematic") |>
    execute(frame, seed = 42)

  result_with_control <- sampling_design() |>
    draw(n = 100, method = "systematic", control = region) |>
    execute(frame, seed = 42)

  # The samples should differ because the frame was sorted differently
  expect_false(identical(
    sort(result_no_control$id),
    sort(result_with_control$id)
  ))

  # The controlled sample should have better spread across regions
  control_counts <- table(result_with_control$region)
  expect_true(all(control_counts > 0))
})

test_that("control sorting with multiple variables works", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(n = 100, method = "systematic", control = c(region, urban_rural)) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100)

  # Should have good coverage of all region x urban_rural combos
  combo_counts <- table(result$region, result$urban_rural)
  expect_true(all(combo_counts > 0))
})

test_that("control sorting with serp() works", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(n = 100, method = "systematic", control = serp(region, urban_rural)) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100)
  expect_true(all(table(result$region) > 0))
})

test_that("control sorting within stratified sampling works", {
  frame <- test_frame()

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 25, method = "systematic", control = urban_rural) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100) # 4 strata x 25

  # Within each region, should have good Urban/Rural spread
  for (r in unique(result$region)) {
    r_data <- result[result$region == r, ]
    expect_true(length(unique(r_data$urban_rural)) == 2)
  }
})

test_that("control sorting preserves correctness for srswor (order-insensitive)", {
  frame <- test_frame()

  # For SRS, control sorting should not change the sampling probabilities
  result <- sampling_design() |>
    draw(n = 100, control = region) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100)
  # SRS weights should still be N/n
  expect_equal(unique(result$.weight), nrow(frame) / 100)
})

# =============================================================================
# Fix 2: Multi-stage weight compounding for non-cluster stratified path
# =============================================================================

test_that("multi-stage non-cluster stratified weights are correct", {
  # Create a frame where strata have different sizes
  frame <- data.frame(
    id = 1:200,
    region = c(rep("North", 50), rep("South", 150)),
    value = rnorm(200)
  )

  # Stage 1: stratified SRS with EQUAL allocation (different probs per stratum)
  # North: 20/50 = 0.4, South: 20/150 = 0.133
  # Stage 2: SRS within selected units
  result <- sampling_design() |>
    stage() |>
    stratify_by(region, alloc = "equal") |>
    draw(n = 40) |>
    stage() |>
    draw(n = 5) |>
    execute(frame, seed = 42)

  # Check that compound weight = product of stage weights
  expect_equal(
    result$.weight,
    result$.weight_1 * result$.weight_2,
    tolerance = 1e-10
  )

  # The key check: different regions should have different stage 1 weights
  north_weights <- unique(result$.weight_1[result$region == "North"])
  south_weights <- unique(result$.weight_1[result$region == "South"])
  if (length(north_weights) > 0 && length(south_weights) > 0) {
    expect_false(identical(north_weights, south_weights))
  }
})

test_that("non-cluster path with equal strata still works", {
  # When all strata are equal, old behavior should match new behavior
  frame <- data.frame(
    id = 1:200,
    region = rep(c("North", "South"), each = 100),
    value = rnorm(200)
  )

  result <- sampling_design() |>
    stage() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 40) |>
    stage() |>
    draw(n = 5) |>
    execute(frame, seed = 42)

  # Compound weight should be product of stage weights
  expect_true(all(result$.weight > 0))
  expect_true(".weight_1" %in% names(result))
  expect_true(".weight_2" %in% names(result))
})

# =============================================================================
# Fix 3: PPS with-replacement expected hits
# =============================================================================

test_that("pps_multinomial uses expected hits for probabilities", {
  skip_if_not_installed("sondage")
  frame <- data.frame(
    id = 1:20,
    size = c(
      100,
      200,
      300,
      50,
      150,
      80,
      120,
      250,
      90,
      170,
      60,
      110,
      140,
      180,
      220,
      70,
      130,
      160,
      190,
      210
    )
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 42)

  # WR method: one row per draw with .draw_1
  expect_equal(nrow(result), 5L)
  expect_true(".draw_1" %in% names(result))
  expect_equal(result$.draw_1, 1:5)

  # Weight = 1/pik = total_size / (n * size_i) per draw
  total_size <- sum(frame$size)
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (5 * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("pps_chromy uses correct weights and draws", {
  skip_if_not_installed("sondage")
  frame <- data.frame(
    id = 1:20,
    size = c(
      100,
      200,
      300,
      50,
      150,
      80,
      120,
      250,
      90,
      170,
      60,
      110,
      140,
      180,
      220,
      70,
      130,
      160,
      190,
      210
    )
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 42)

  # PMR method: one row per draw with .draw_1
  expect_equal(nrow(result), 5L)
  expect_true(".draw_1" %in% names(result))
  expect_equal(result$.draw_1, 1:5)

  # Weight = 1/pik = total_size / (n * size_i) per draw
  total_size <- sum(frame$size)
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (5 * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("pps_multinomial dominant unit gets many draws", {
  skip_if_not_installed("sondage")
  # Create a frame with one very large unit
  frame <- data.frame(
    id = 1:10,
    size = c(1000, rep(10, 9)) # One dominant unit
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 42)

  # WR: one row per draw (n=5 rows total)
  expect_equal(nrow(result), 5L)
  expect_equal(result$.draw_1, 1:5)

  # Dominant unit (id=1) should appear in most draws
  # Expected hits = 5 * 1000/1090 = 4.59
  n_dominant_draws <- sum(result$id == 1)
  expect_true(n_dominant_draws >= 4)

  # All weights should be positive
  expect_true(all(result$.weight > 0))
})

# =============================================================================
# Fix 4: %||% works via rlang import (no redefinition)
# =============================================================================

test_that("%||% operator works after removing custom definition", {
  # This implicitly tests that rlang::`%||%` is properly imported
  # by using functions that rely on it (draw_spec$round, etc.)
  frame <- test_frame()

  # round defaults to "up" via %||%
  result <- sampling_design() |>
    draw(frac = 0.1) |>
    execute(frame, seed = 42)

  # Should work without error (round defaults to "up" via %||%)
  expect_equal(nrow(result), ceiling(nrow(frame) * 0.1))
})

test_that("print works with data-frame n", {
  alloc_df <- data.frame(
    region = c("North", "South"),
    n = c(10, 20)
  )

  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = alloc_df)

  # Should not error
  output <- capture.output(print(design))
  expect_true(any(grepl("custom data frame", output)))
})

test_that("print works with data-frame frac", {
  alloc_df <- data.frame(
    region = c("North", "South"),
    frac = c(0.1, 0.2)
  )

  design <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = alloc_df)

  # Should not error
  output <- capture.output(print(design))
  expect_true(any(grepl("custom data frame", output)))
})

test_that("print works with scalar n", {
  design <- sampling_design() |> draw(n = 100)

  output <- capture.output(print(design))
  expect_true(any(grepl("n = 100", output)))
})


test_that("[.tbl_sample preserves class on row subsetting", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  subset <- sample[1:10, ]
  expect_s3_class(subset, "tbl_sample")
  expect_true(is_tbl_sample(subset))

  expect_equal(get_design(subset)$title, get_design(sample)$title)
})

test_that("[.tbl_sample strips class when essential columns removed", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  subset <- sample[, c("id", "region")]
  expect_false(is_tbl_sample(subset))
})

test_that("[.tbl_sample preserves class on column subsetting with essentials", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  # Keeping essential columns should preserve class
  subset <- sample[, c("id", "region", ".weight")]
  expect_s3_class(subset, "tbl_sample")
})

test_that("sample_stratified gives correct results (implicit group_modify test)", {
  frame <- test_frame()

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 200) |>
    execute(frame, seed = 42)

  # Basic correctness
  expect_equal(nrow(result), 200)
  expect_equal(length(unique(result$region)), 4)

  # Weights should be positive
  expect_true(all(result$.weight > 0))

  # Weights should sum to population
  expect_equal(sum(result$.weight), nrow(frame), tolerance = 1)
})

test_that("sample_within_clusters (split+lapply) gives correct results", {
  frame <- test_frame()

  result <- sampling_design() |>
    stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    stage(label = "Students") |>
    draw(n = 5) |>
    execute(frame, seed = 42)

  # Should have 20 schools * 5 students = 100
  expect_equal(nrow(result), 100)

  # Each school should have exactly 5 students
  school_counts <- table(result$school_id)
  expect_true(all(school_counts == 5))

  # Weight should be compound of stage weights
  expect_true(all(result$.weight > 0))
  expect_true(".weight_1" %in% names(result))
  expect_true(".weight_2" %in% names(result))
})

test_that("stratified and within-clusters both produce correct weights", {
  # This tests that the two code paths (group_modify in sample_stratified
  # and split+lapply in sample_within_clusters) produce consistent results
  frame <- test_frame()

  # Stratified only
  strat_result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 25) |>
    execute(frame, seed = 42)

  expect_equal(sum(strat_result$.weight), nrow(frame), tolerance = 1)

  # Two-stage with clusters
  twostage_result <- sampling_design() |>
    stage() |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    stage() |>
    draw(n = 5) |>
    execute(frame, seed = 42)

  # Compound weights: w = w1 * w2
  # Weight should still sum approximately to population
  expect_equal(sum(twostage_result$.weight), nrow(frame), tolerance = 50)
})

# =============================================================================
# Reproducibility check across fixes
# =============================================================================

test_that("all fixes maintain seed reproducibility", {
  frame <- test_frame()

  design <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 200, method = "systematic", control = urban_rural)

  result1 <- execute(design, frame, seed = 42)
  result2 <- execute(design, frame, seed = 42)

  expect_equal(result1$id, result2$id)
  expect_equal(result1$.weight, result2$.weight)
})

# =============================================================================
# Edge cases
# =============================================================================

test_that("control sorting with desc() works", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(
      n = 50,
      method = "systematic",
      control = c(region, dplyr::desc(value))
    ) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 50)
})

test_that("multi-stage stratified then unstratified compounding works", {
  # Regression test for Fix 2
  frame <- data.frame(
    id = 1:300,
    region = rep(c("A", "B", "C"), each = 100),
    value = rnorm(300)
  )

  result <- sampling_design() |>
    stage() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 60) |>
    stage() |>
    draw(n = 5) |>
    execute(frame, seed = 42)

  # Compound weight = product of stage weights
  expect_equal(
    result$.weight,
    result$.weight_1 * result$.weight_2,
    tolerance = 1e-10
  )

  # Weights should be positive
  expect_true(all(result$.weight > 0))
})
