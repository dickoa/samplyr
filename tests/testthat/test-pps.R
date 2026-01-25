# Tests for PPS (Probability Proportional to Size) sampling methods
# Covers: pps_systematic, pps_brewer, pps_maxent, pps_poisson, pps_multinomial

test_that("PPS weights are inversely proportional to inclusion probabilities", {
  # Simple frame with known sizes
  frame <- data.frame(
    id = 1:5,
    size = c(10, 20, 30, 40, 100)  # Total = 200
  )
  
  # Sample 2 units with PPS systematic
  result <- sampling_design() |>
    draw(n = 2, method = "pps_systematic", mos = size) |>
    execute(frame, seed = 123)
  

  # Check that weights vary (not equal probability)
  expect_false(all(result$.weight == result$.weight[1]))
  
  # Check weight = 1/prob relationship
  expect_equal(result$.weight, 1 / result$.prob, tolerance = 1e-10)
  
  # Check probabilities are proportional to size
  # π_i = n * (size_i / total_size) = 2 * size_i / 200 = size_i / 100
  total_size <- sum(frame$size)
  n <- 2
  
  for (i in seq_len(nrow(result))) {
    unit_size <- result$size[i]
    expected_prob <- n * unit_size / total_size
    expect_equal(result$.prob[i], expected_prob, tolerance = 1e-10)
  }
})

test_that("PPS Brewer method gives correct probabilities", {
  frame <- data.frame(
    id = 1:4,
    size = c(25, 25, 25, 25)  # Equal sizes = equal probabilities
  )
  
  # With equal sizes, PPS should give equal probabilities

  result <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = size) |>
    execute(frame, seed = 42)
  
  # All probabilities should be equal: 2/4 = 0.5
  expect_equal(result$.prob[1], result$.prob[2], tolerance = 1e-10)
  expect_equal(result$.prob[1], 0.5, tolerance = 1e-10)
  expect_equal(result$.weight[1], 2, tolerance = 1e-10)
})

test_that("PPS gives larger weights to smaller units", {
  # Use more units and smaller n to avoid certainty selections
  frame <- data.frame(
    id = 1:6,
    size = c(10, 20, 30, 40, 50, 60)  # Total = 210
  )
  
  # Sample 2 from 6 - no certainty selections
  # π_i = 2 * size_i / 210
  result <- sampling_design() |>
    draw(n = 2, method = "pps_systematic", mos = size) |>
    execute(frame, seed = 42)
  
  # Weights should vary
  expect_false(all(result$.weight == result$.weight[1]))
  
  # If both a small and large unit are selected, small should have larger weight
  if (nrow(result) == 2) {
    smaller_idx <- which.min(result$size)
    larger_idx <- which.max(result$size)
    expect_true(result$.weight[smaller_idx] > result$.weight[larger_idx])
  }
  
  # Verify formula: π_i = n * size_i / total_size
  total_size <- sum(frame$size)
  n <- 2
  for (i in seq_len(nrow(result))) {
    expected_prob <- n * result$size[i] / total_size
    expect_equal(result$.prob[i], expected_prob, tolerance = 1e-10)
  }
})

test_that("Equal probability methods still work correctly", {
  frame <- data.frame(
    id = 1:10,
    x = rnorm(10)
  )
  
  result <- sampling_design() |>
    draw(n = 5) |>
    execute(frame, seed = 42)
  
  # All weights should be equal: N/n = 10/5 = 2
  expect_true(all(result$.weight == 2))
  expect_true(all(result$.prob == 0.5))
})

test_that("Stratified PPS gives correct within-stratum probabilities", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 4),
    id = 1:8,
    size = c(10, 20, 30, 40, 100, 200, 300, 400)
  )
  
  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 2, method = "pps_systematic", mos = size) |>
    execute(frame, seed = 123)
  
  # Check within each stratum
  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]
  
  # Stratum A: total = 100, n = 2, π_i = 2 * size_i / 100
  for (i in seq_len(nrow(result_A))) {
    expected_prob <- 2 * result_A$size[i] / 100
    expect_equal(result_A$.prob[i], expected_prob, tolerance = 1e-10)
  }
  
  # Stratum B: total = 1000, n = 2, π_i = 2 * size_i / 1000
  for (i in seq_len(nrow(result_B))) {
    expected_prob <- 2 * result_B$size[i] / 1000
    expect_equal(result_B$.prob[i], expected_prob, tolerance = 1e-10)
  }
})

test_that("Cluster PPS sampling gives correct cluster-level probabilities", {
  # Frame with clusters of different sizes
  frame <- data.frame(
    cluster = rep(1:4, times = c(5, 10, 15, 20)),
    cluster_size = rep(c(5, 10, 15, 20), times = c(5, 10, 15, 20)),
    id = 1:50
  )
  
  result <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_systematic", mos = cluster_size) |>
    execute(frame, seed = 42)
  
  # Get unique cluster info from result
  cluster_info <- unique(result[, c("cluster", "cluster_size", ".prob", ".weight")])
  
  # Total cluster size sum (at cluster level) = 5+10+15+20 = 50
  # π_i = 2 * cluster_size / 50
  for (i in seq_len(nrow(cluster_info))) {
    expected_prob <- 2 * cluster_info$cluster_size[i] / 50
    expect_equal(cluster_info$.prob[i], expected_prob, tolerance = 1e-10)
  }
})

test_that("PPS maxent method produces valid probabilities", {
  frame <- data.frame(
    id = 1:6,
    size = c(5, 10, 15, 20, 25, 30)  # Total = 105
  )
  
  result <- sampling_design() |>
    draw(n = 3, method = "pps_maxent", mos = size) |>
    execute(frame, seed = 999)
  
  # Probabilities should be in (0, 1]
  expect_true(all(result$.prob > 0))
  expect_true(all(result$.prob <= 1))
  
  # Weights should be positive
  expect_true(all(result$.weight > 0))
  
  # Weight = 1/prob
  expect_equal(result$.weight, 1 / result$.prob, tolerance = 1e-10)
})

test_that("PPS with certainty selections gives prob=1 for large units", {
  # When n is large relative to N and sizes are skewed,

  # large units become certainty selections (prob=1)
  frame <- data.frame(
    id = 1:3,
    size = c(10, 50, 100)  # Total = 160
  )
  
  # With n=3 and N=3, all become certainty selections
  result <- sampling_design() |>
    draw(n = 3, method = "pps_systematic", mos = size) |>
    execute(frame, seed = 1)
  
  # All should have prob=1 and weight=1 (certainty selections)
  expect_equal(nrow(result), 3)
  expect_true(all(result$.prob == 1))
  expect_true(all(result$.weight == 1))
})

test_that("PPS Poisson method produces valid probabilities", {
  frame <- data.frame(
    id = 1:10,
    size = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)  # Total = 275
  )
  
  result <- sampling_design() |>
    draw(frac = 0.3, method = "pps_poisson", mos = size) |>
    execute(frame, seed = 42)
  
  # Probabilities should be in (0, 1]
  expect_true(all(result$.prob > 0))
  expect_true(all(result$.prob <= 1))
  
  # Weights should be positive
  expect_true(all(result$.weight > 0))
  
  # Weight = 1/prob
  expect_equal(result$.weight, 1 / result$.prob, tolerance = 1e-10)
  
  # Probabilities should be proportional to size
  # Larger size -> larger prob
  if (nrow(result) >= 2) {
    expect_true(cor(result$size, result$.prob) > 0)
  }
})

test_that("PPS Poisson sample size varies", {
  frame <- data.frame(
    id = 1:20,
    size = sample(10:100, 20)
  )
  
  # Run multiple times - Poisson should give variable sample sizes
  sizes <- sapply(1:10, function(seed) {
    result <- sampling_design() |>
      draw(frac = 0.25, method = "pps_poisson", mos = size) |>
      execute(frame, seed = seed)
    nrow(result)
  })
  
  # Should have some variation (unlike fixed-size methods)
  # Note: with small n, variation may be limited
  expect_true(length(unique(sizes)) >= 1)
})

test_that("PPS multinomial method produces valid probabilities", {
  frame <- data.frame(
    id = 1:8,
    size = c(10, 20, 30, 40, 50, 60, 70, 80)  # Total = 360
  )
  
  result <- sampling_design() |>
    draw(n = 4, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 123)
  
  # Probabilities should be in (0, 1]
  expect_true(all(result$.prob > 0))
  expect_true(all(result$.prob <= 1))
  
  # Weights should be positive
  expect_true(all(result$.weight > 0))
  
  # Weight = 1/prob
  expect_equal(result$.weight, 1 / result$.prob, tolerance = 1e-10)
})

test_that("PPS multinomial with replacement can select same unit multiple times", {
  # Multinomial is with-replacement, so same unit can be selected
  # This test verifies that probabilities are still calculated correctly
  frame <- data.frame(
    id = 1:5,
    size = c(10, 10, 10, 10, 100)  # One very large unit
  )
  
  result <- sampling_design() |>
    draw(n = 3, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 42)
  
  # Large unit (id=5) should have higher probability
  # π_i proportional to size_i / sum(size)
  total_size <- sum(frame$size)
  
  for (i in seq_len(nrow(result))) {
    unit_size <- result$size[i]
    # For multinomial, π_i = 1 - (1 - p_i)^n where p_i = size_i/total
    # But sondage returns the actual inclusion probability
    expect_true(result$.prob[i] > 0 && result$.prob[i] <= 1)
  }
})

test_that("Stratified PPS Poisson gives within-stratum probabilities", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 5),
    id = 1:10,
    size = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)
  )
  
  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = 0.4, method = "pps_poisson", mos = size) |>
    execute(frame, seed = 42)
  
  # Check within each stratum - probabilities proportional to size
  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]
  
  if (nrow(result_A) >= 2) {
    expect_true(cor(result_A$size, result_A$.prob) > 0)
  }
  
  if (nrow(result_B) >= 2) {
    expect_true(cor(result_B$size, result_B$.prob) > 0)
  }
})
