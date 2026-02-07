# Tests for equal probability sampling methods: SRS, systematic, Bernoulli

# ---- Simple Random Sampling (SRS) ----

test_that("SRS gives equal weights", {
  frame <- data.frame(id = 1:100)
  
  result <- sampling_design() |>
    draw(n = 10, method = "srswor") |>
    execute(frame, seed = 42)
  
  # All units have equal weight: N/n = 100/10 = 10
  expect_true(all(result$.weight == 10))
  expect_equal(nrow(result), 10)
})

test_that("SRS weights sum to population size", {
  frame <- data.frame(id = 1:500)
  
  result <- sampling_design() |>
    draw(n = 50, method = "srswor") |>
    execute(frame, seed = 123)
  
  expect_equal(sum(result$.weight), 500)
})

test_that("SRS with frac gives correct weight", {
  frame <- data.frame(id = 1:200)
  
  result <- sampling_design() |>
    draw(frac = 0.25, method = "srswor") |>
    execute(frame, seed = 42)
  
  expect_equal(nrow(result), 50)  # 200 * 0.25
  expect_true(all(result$.weight == 4))
})

test_that("Stratified SRS gives within-stratum weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), c(100, 200)),
    id = 1:300
  )
  
  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 10, method = "srswor") |>
    execute(frame, seed = 42)
  
  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]
  
  # Stratum A: N_A/n = 100/10 = 10
  expect_true(all(result_A$.weight == 10))
  
  # Stratum B: N_B/n = 200/10 = 20
  expect_true(all(result_B$.weight == 20))
})

# ---- Systematic Sampling ----

test_that("Systematic sampling gives equal weights", {
  frame <- data.frame(id = 1:100)
  
  result <- sampling_design() |>
    draw(n = 10, method = "systematic") |>
    execute(frame, seed = 42)
  
  # Equal weight: N/n = 10
  expect_true(all(result$.weight == 10))
  expect_equal(nrow(result), 10)
})

test_that("Systematic sampling produces evenly spaced samples", {
  frame <- data.frame(id = 1:100)
  
  result <- sampling_design() |>
    draw(n = 10, method = "systematic") |>
    execute(frame, seed = 42)
  
  # IDs should be roughly evenly spaced (interval ~10)
  sorted_ids <- sort(result$id)
  gaps <- diff(sorted_ids)
  
  # Most gaps should be close to interval (allowing for rounding)
  expect_true(all(gaps >= 9 & gaps <= 11))
})

test_that("Stratified systematic gives within-stratum weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 50),
    id = 1:100
  )
  
  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "systematic") |>
    execute(frame, seed = 42)
  
  # Each stratum: N_h/n = 50/5 = 10
  expect_true(all(result$.weight == 10))
})

# ---- Bernoulli Sampling ----

test_that("Bernoulli sampling gives correct weights", {
  frame <- data.frame(id = 1:100)
  
  result <- sampling_design() |>
    draw(frac = 0.3, method = "bernoulli") |>
    execute(frame, seed = 42)
  
  # All selected units have weight = 1/frac
  expect_true(all(abs(result$.weight - 1 / 0.3) < 1e-10))
})

test_that("Bernoulli sample size varies around expected value", {
  frame <- data.frame(id = 1:1000)
  
  # Run multiple times to check variability
  sizes <- sapply(1:10, function(seed) {
    result <- sampling_design() |>
      draw(frac = 0.1, method = "bernoulli") |>
      execute(frame, seed = seed)
    nrow(result)
  })
  
  # Expected size is 100 (0.1 * 1000)
  # Should vary (unlike SRS which is fixed)
  expect_true(sd(sizes) > 0)
  
  # Mean should be close to expected
  expect_true(abs(mean(sizes) - 100) < 30)
})

test_that("Stratified Bernoulli gives stratum-specific weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 50),
    id = 1:100
  )
  
  # Different fractions per stratum
  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = c("A" = 0.2, "B" = 0.5), method = "bernoulli") |>
    execute(frame, seed = 123)
  
  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]
  
  if (nrow(result_A) > 0) {
    expect_true(all(result_A$.weight == 5))
  }
  
  if (nrow(result_B) > 0) {
    expect_true(all(result_B$.weight == 2))
  }
})

# ---- Weight correctness across methods ----

test_that("All equal probability methods give consistent weights", {
  frame <- data.frame(id = 1:100)
  methods <- c("srswor", "systematic")
  
  for (m in methods) {
    result <- sampling_design() |>
      draw(n = 10, method = m) |>
      execute(frame, seed = 42)
    
    # Weight should be N/n = 10 for all equal probability methods
    expect_equal(
      unique(result$.weight), 
      10,
      info = paste("Method:", m)
    )
  }
  
  # Bernoulli uses frac: weight = 1/frac = 5
  result <- sampling_design() |>
    draw(frac = 0.2, method = "bernoulli") |>
    execute(frame, seed = 42)
  
  expect_equal(unique(result$.weight), 5, tolerance = 1e-10)
})
