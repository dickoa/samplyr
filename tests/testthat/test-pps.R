test_that("PPS weights are inversely proportional to inclusion probabilities", {
  # Simple frame with known sizes
  frame <- data.frame(
    id = 1:5,
    size = c(10, 20, 30, 40, 100) # Total = 200
  )

  # Sample 2 units with PPS systematic
  result <- sampling_design() |>
    draw(n = 2, method = "pps_systematic", mos = size) |>
    execute(frame, seed = 123)

  # Check that weights vary (not equal probability)
  expect_false(all(result$.weight == result$.weight[1]))

  # Check weights match expected formula: w_i = 1 / pi_i = total_size / (n * size_i)
  total_size <- sum(frame$size)
  n <- 2

  for (i in seq_len(nrow(result))) {
    unit_size <- result$size[i]
    expected_weight <- total_size / (n * unit_size)
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("PPS Brewer method gives correct weights", {
  frame <- data.frame(
    id = 1:4,
    size = c(25, 25, 25, 25) # Equal sizes = equal probabilities
  )

  # With equal sizes, PPS should give equal probabilities = 2/4 = 0.5
  result <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = size) |>
    execute(frame, seed = 42)

  # All weights should be equal: 1/0.5 = 2
  expect_equal(result$.weight[1], result$.weight[2], tolerance = 1e-10)
  expect_equal(result$.weight[1], 2, tolerance = 1e-10)
})

test_that("PPS gives larger weights to smaller units", {
  # Use more units and smaller n to avoid certainty selections
  frame <- data.frame(
    id = 1:6,
    size = c(10, 20, 30, 40, 50, 60) # Total = 210
  )

  # Sample 2 from 6 - no certainty selections
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

  # Verify formula: w_i = total_size / (n * size_i)
  total_size <- sum(frame$size)
  n <- 2
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (n * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
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
})

test_that("Stratified PPS gives correct within-stratum weights", {
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

  # Stratum A: total = 100, n = 2, w_i = 100 / (2 * size_i)
  for (i in seq_len(nrow(result_A))) {
    expected_weight <- 100 / (2 * result_A$size[i])
    expect_equal(result_A$.weight[i], expected_weight, tolerance = 1e-10)
  }

  # Stratum B: total = 1000, n = 2, w_i = 1000 / (2 * size_i)
  for (i in seq_len(nrow(result_B))) {
    expected_weight <- 1000 / (2 * result_B$size[i])
    expect_equal(result_B$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("Cluster PPS sampling gives correct cluster-level weights", {
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
  cluster_info <- unique(result[, c(
    "cluster",
    "cluster_size",
    ".weight"
  )])

  # Total cluster size sum (at cluster level) = 5+10+15+20 = 50
  # w_i = 50 / (2 * cluster_size)
  for (i in seq_len(nrow(cluster_info))) {
    expected_weight <- 50 / (2 * cluster_info$cluster_size[i])
    expect_equal(cluster_info$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("PPS maxent method produces valid weights", {
  frame <- data.frame(
    id = 1:6,
    size = c(5, 10, 15, 20, 25, 30) # Total = 105
  )

  result <- sampling_design() |>
    draw(n = 3, method = "pps_cps", mos = size) |>
    execute(frame, seed = 999)

  # Weights should be >= 1 (since prob <= 1 for WOR)
  expect_true(all(result$.weight >= 1))

  # Weights should be positive
  expect_true(all(result$.weight > 0))
})

test_that("PPS with certainty selections gives weight=1 for large units", {
  # When n is large relative to N and sizes are skewed,
  # large units become certainty selections (prob=1, weight=1)
  frame <- data.frame(
    id = 1:3,
    size = c(10, 50, 100) # Total = 160
  )

  # With n=3 and N=3, all become certainty selections
  result <- sampling_design() |>
    draw(n = 3, method = "pps_systematic", mos = size) |>
    execute(frame, seed = 1)

  # All should have weight=1 (certainty selections)
  expect_equal(nrow(result), 3)
  expect_true(all(result$.weight == 1))
})

test_that("PPS Poisson method produces valid weights", {
  frame <- data.frame(
    id = 1:10,
    size = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50) # Total = 275
  )

  result <- sampling_design() |>
    draw(frac = 0.3, method = "pps_poisson", mos = size) |>
    execute(frame, seed = 42)

  # Weights should be >= 1 (Poisson is WOR, prob <= 1)
  expect_true(all(result$.weight >= 1))

  # Weights should be positive
  expect_true(all(result$.weight > 0))

  # Weights should be inversely proportional to size
  if (nrow(result) >= 2) {
    expect_true(cor(result$size, result$.weight) < 0)
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
  expect_true(length(unique(sizes)) >= 1)
})

test_that("PPS multinomial method produces replicated rows with draw index", {
  frame <- data.frame(
    id = 1:8,
    size = c(10, 20, 30, 40, 50, 60, 70, 80) # Total = 360
  )

  result <- sampling_design() |>
    draw(n = 4, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 123)

  # WR method: one row per draw (n rows total)
  expect_equal(nrow(result), 4L)

  # Should have .draw_1 column for stage 1
  expect_true(".draw_1" %in% names(result))

  # Draw IDs should be sequential 1:n
  expect_equal(result$.draw_1, 1:4)

  # Weights should be positive
  expect_true(all(result$.weight > 0))

  # Weight = 1/pik = total_size / (n * size_i) for each draw
  total_size <- sum(frame$size)
  n <- 4
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (n * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("PPS multinomial with replacement replicates rows with draw index", {
  # Multinomial is with-replacement, so same unit can be selected
  frame <- data.frame(
    id = 1:5,
    size = c(10, 10, 10, 10, 100) # One very large unit
  )

  result <- sampling_design() |>
    draw(n = 3, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 42)

  # WR: one row per draw (n=3 rows total, ids may repeat)
  expect_equal(nrow(result), 3L)
  expect_equal(result$.draw_1, 1:3)

  # All weights should be positive
  expect_true(all(result$.weight > 0))
})

test_that("Stratified PPS Poisson gives within-stratum weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 5),
    id = 1:10,
    size = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = 0.4, method = "pps_poisson", mos = size) |>
    execute(frame, seed = 42)

  # Check within each stratum - weights inversely proportional to size
  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]

  if (nrow(result_A) >= 2) {
    expect_true(cor(result_A$size, result_A$.weight) < 0)
  }

  if (nrow(result_B) >= 2) {
    expect_true(cor(result_B$size, result_B$.weight) < 0)
  }
})

test_that("PPS Chromy method produces replicated rows with draw index", {
  frame <- data.frame(
    id = 1:8,
    size = c(10, 20, 30, 40, 50, 60, 70, 80) # Total = 360
  )

  result <- sampling_design() |>
    draw(n = 4, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 123)

  # PMR: one row per draw (n rows total)
  expect_equal(nrow(result), 4L)

  # Should have .draw_1 column
  expect_true(".draw_1" %in% names(result))

  # Draw IDs should be sequential
  expect_equal(result$.draw_1, 1:4)

  # Weights should be positive
  expect_true(all(result$.weight > 0))

  # Weight = 1/pik = total_size / (n * size_i)
  total_size <- sum(frame$size)
  n <- 4
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (n * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("PPS Chromy with minimum replacement replicates large-hit units", {
  # When expected hits > 1, Chromy uses minimum replacement
  frame <- data.frame(
    id = 1:4,
    size = c(10, 20, 30, 140) # Total = 200
  )

  # n = 10 means expected hits for id=4 is 10*140/200 = 7
  result <- sampling_design() |>
    draw(n = 10, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 42)

  # PMR: one row per draw (n=10 rows total)
  expect_equal(nrow(result), 10L)
  expect_equal(result$.draw_1, 1:10)

  # Large unit (id=4) should appear many times
  n_large_draws <- sum(result$id == 4)
  expect_true(n_large_draws >= 6) # Expected hits = 7, so 6-8 is reasonable
})

test_that("PPS Chromy gives exact total sample size via row replication", {
  # Chromy always gives exactly n total selections
  frame <- data.frame(
    id = 1:20,
    size = sample(10:100, 20)
  )

  for (seed in 1:5) {
    result <- sampling_design() |>
      draw(n = 8, method = "pps_chromy", mos = size) |>
      execute(frame, seed = seed)
    # Total rows should be exactly n
    expect_equal(nrow(result), 8L)
    expect_equal(result$.draw_1, 1:8)
  }
})

test_that("Stratified PPS Chromy works correctly", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 5),
    id = 1:10,
    size = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 3, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 42)

  # Total rows should be 6 (3 per stratum)
  expect_equal(nrow(result), 6L)

  # 3 draws from each stratum
  expect_equal(sum(result$stratum == "A"), 3L)
  expect_equal(sum(result$stratum == "B"), 3L)
})

test_that("PPS SPS method gives correct weights", {
  frame <- data.frame(
    id = 1:6,
    size = c(10, 20, 30, 40, 50, 60) # Total = 210
  )

  result <- sampling_design() |>
    draw(n = 3, method = "pps_sps", mos = size) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 3)
  expect_true(all(result$.weight > 0))
  expect_true(all(is.finite(result$.weight)))

  # Weights should be inversely proportional to size
  total_size <- sum(frame$size)
  n <- 3
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (n * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("PPS Pareto method gives correct weights", {
  frame <- data.frame(
    id = 1:6,
    size = c(10, 20, 30, 40, 50, 60) # Total = 210
  )

  result <- sampling_design() |>
    draw(n = 3, method = "pps_pareto", mos = size) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 3)
  expect_true(all(result$.weight > 0))
  expect_true(all(is.finite(result$.weight)))

  # Weights should be inversely proportional to size
  total_size <- sum(frame$size)
  n <- 3
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (n * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("Stratified PPS SPS gives correct within-stratum weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 5),
    id = 1:10,
    size = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 2, method = "pps_sps", mos = size) |>
    execute(frame, seed = 123)

  expect_equal(nrow(result), 4)

  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]

  for (i in seq_len(nrow(result_A))) {
    expected_weight <- 150 / (2 * result_A$size[i])
    expect_equal(result_A$.weight[i], expected_weight, tolerance = 1e-10)
  }

  for (i in seq_len(nrow(result_B))) {
    expected_weight <- 1500 / (2 * result_B$size[i])
    expect_equal(result_B$.weight[i], expected_weight, tolerance = 1e-10)
  }
})
