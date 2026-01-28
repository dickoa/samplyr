# =============================================================================
# 1. SEED HANDLING TESTS
# =============================================================================
test_that("execute restores RNG state correctly", {
  design <- sampling_design() |> draw(n = 5)
  frame <- data.frame(id = 1:20)

  # --- SCENARIO 1: User has set a seed beforehand ---
  set.seed(123)
  runif(1) # User does something random
  # execute() happens here ideally without side effects
  val_control <- runif(1) # The next value in the stream

  # 2. Run the "Test" stream
  set.seed(123)
  runif(1) # User does same thing

  # Run execute with its OWN seed
  # This advances the RNG internally, but should restore 123 upon exit
  invisible(execute(design, frame, seed = 9999))

  val_test <- runif(1) # This should match val_control

  expect_equal(
    val_test,
    val_control,
    info = "RNG state was not restored to original stream after execute()"
  )

  # Clear the seed completely
  if (exists(".Random.seed", envir = globalenv())) {
    rm(".Random.seed", envir = globalenv())
  }

  # Run execute
  invisible(execute(design, frame, seed = 555))

  # Check if seed was removed
  expect_false(
    exists(".Random.seed", envir = globalenv()),
    info = "execute() left a seed behind when none existed before"
  )
})

test_that("execute() does not pollute global RNG state", {
  frame <- data.frame(id = 1:100)

  set.seed(123)
  rnorm(5) # Advance state

  state_before <- .Random.seed

  sample <- sampling_design() |>
    draw(n = 10) |>
    execute(frame, seed = 999)

  state_after <- .Random.seed

  expect_identical(state_before, state_after)
})

test_that("execute() without seed does not affect reproducibility", {
  frame <- data.frame(id = 1:100)
  design <- sampling_design() |> draw(n = 10)

  # Two executions without seed should give different results
  result1 <- execute(design, frame)
  result2 <- execute(design, frame)

  expect_false(identical(result1$id, result2$id))
})

test_that("execute() with same seed is reproducible", {
  frame <- data.frame(id = 1:100)
  design <- sampling_design() |> draw(n = 10)

  result1 <- execute(design, frame, seed = 42)
  result2 <- execute(design, frame, seed = 42)

  expect_identical(result1$id, result2$id)
})

test_that("execute() with different seeds gives different results", {
  frame <- data.frame(id = 1:100)
  design <- sampling_design() |> draw(n = 10)

  result1 <- execute(design, frame, seed = 42)
  result2 <- execute(design, frame, seed = 43)

  expect_false(identical(result1$id, result2$id))
})


# =============================================================================
# 2. EMPTY AND SINGLE-UNIT STRATA TESTS
# =============================================================================

test_that("stratified sampling handles single-unit strata", {
  frame <- data.frame(
    id = 1:10,
    region = c("A", rep("B", 9)) # Region A has only 1 unit
  )

  # With n=1 per stratum
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 1) |>
    execute(frame, seed = 42)

  expect_equal(sum(result$region == "A"), 1)
  expect_equal(sum(result$region == "B"), 1)
})

test_that("proportional allocation with tiny stratum may give zero without min_n", {
  frame <- data.frame(
    id = 1:101,
    region = c("A", rep("B", 100)) # A has ~1%, B has ~99%
  )

  # Proportional allocation of n=10: A gets 10 * (1/101) = 0.099
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 10) |>
    execute(frame, seed = 42)

  # Total should be 10
  expect_equal(nrow(result), 10)

  # Document behavior: tiny stratum may get 0
  n_from_A <- sum(result$region == "A")
  expect_true(n_from_A >= 0 && n_from_A <= 1)
})

test_that("proportional allocation respects min_n for tiny strata", {
  frame <- data.frame(
    id = 1:101,
    region = c("A", rep("B", 100))
  )

  # With min_n=1, stratum A should get at least 1
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 10, min_n = 1) |>
    execute(frame, seed = 42)

  # min_n=1 should guarantee at least 1 from each stratum
  expect_true(sum(result$region == "A") >= 1)
})

test_that("neyman allocation with zero-variance stratum and min_n", {
  frame <- data.frame(
    id = 1:101,
    region = c("A", rep("B", 100)),
    value = c(100, rnorm(100, 50, 10))
  )

  variance_df <- data.frame(
    region = c("A", "B"),
    var = c(0, 100) # A has zero variance (single unit)
  )

  # With min_n=1 to ensure A gets at least 1
  result <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = variance_df) |>
    draw(n = 10, min_n = 1) |>
    execute(frame, seed = 42)

  expect_true(sum(result$region == "A") >= 1)
  expect_equal(nrow(result), 10)
})

test_that("sample size silently capped at stratum population", {
  frame <- data.frame(
    id = 1:15,
    region = c(rep("A", 5), rep("B", 10))
  )

  # Request 10 per stratum, but A only has 5
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10) |>
    execute(frame, seed = 42)

  # A should have 5 (all available), B should have 10
  expect_equal(sum(result$region == "A"), 5)
  expect_equal(sum(result$region == "B"), 10)
})


# =============================================================================
# 3. SMALL FRAC EDGE CASES
# =============================================================================

test_that("very small frac gives at least 1 unit", {
  frame <- data.frame(id = 1:100)

  # frac = 0.001 on N=100 -> expected 0.1 -> rounds to at least 1
  result <- sampling_design() |>
    draw(frac = 0.001) |>
    execute(frame, seed = 42)

  expect_true(nrow(result) >= 1)
})

test_that("frac rounding with round='up'", {
  frame <- data.frame(id = 1:100)

  # frac = 0.025 on N=100 -> 2.5 -> ceiling = 3
  result <- sampling_design() |>
    draw(frac = 0.025, round = "up") |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 3)
})

test_that("frac rounding with round='down'", {
  frame <- data.frame(id = 1:100)

  # frac = 0.025 on N=100 -> 2.5 -> floor = 2
  result <- sampling_design() |>
    draw(frac = 0.025, round = "down") |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 2)
})

test_that("stratified frac with tiny strata", {
  frame <- data.frame(
    id = 1:110,
    region = c(rep("Tiny", 5), rep("Large", 105))
  )

  # frac = 0.1: Tiny gets 0.5, Large gets 10.5
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = 0.1, round = "up") |>
    execute(frame, seed = 42)

  expect_equal(sum(result$region == "Tiny"), 1) # ceiling(0.5)
  expect_equal(sum(result$region == "Large"), 11) # ceiling(10.5)
})


# =============================================================================
# 4. CLUSTER EDGE CASES
# =============================================================================

test_that("cluster sampling with varying cluster sizes", {
  frame <- data.frame(
    cluster_id = c(1, 2, 2, 3, 3, 3),
    id = 1:6
  )

  result <- sampling_design() |>
    cluster_by(cluster_id) |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  # Should select exactly 2 clusters
  expect_equal(length(unique(result$cluster_id)), 2)

  # Weights should be correct (3 clusters, select 2 -> weight = 3/2 = 1.5)
  expect_equal(unique(result$.weight), 1.5)
})

test_that("two-stage sampling handles small second-stage populations", {
  frame <- data.frame(
    school_id = c(1, 1, 1, 2, 3, 3),
    student_id = 1:6
  )

  result <- sampling_design() |>
    stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 2) |>
    stage(label = "Students") |>
    draw(n = 2) |>
    execute(frame, seed = 123)

  selected_schools <- unique(result$school_id)

  for (school in selected_schools) {
    n_available <- sum(frame$school_id == school)
    n_selected <- sum(result$school_id == school)
    expect_true(n_selected <= min(2, n_available))
  }
})


# =============================================================================
# 5. MULTI-STAGE EDGE CASES
# =============================================================================

test_that("multi-stage with all clusters selected in stage 1", {
  frame <- data.frame(
    cluster_id = rep(1:3, each = 5),
    unit_id = 1:15
  )

  result <- sampling_design() |>
    stage(label = "Clusters") |>
    cluster_by(cluster_id) |>
    draw(n = 3) |>
    stage(label = "Units") |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 6)
  expect_equal(length(unique(result$cluster_id)), 3)
})

test_that("partial execution returns correct stages", {
  frame <- data.frame(
    cluster_id = rep(1:10, each = 5),
    unit_id = 1:50
  )

  design <- sampling_design() |>
    stage(label = "Clusters") |>
    cluster_by(cluster_id) |>
    draw(n = 3) |>
    stage(label = "Units") |>
    draw(n = 2)

  stage1_result <- execute(design, frame, stages = 1, seed = 42)

  expect_equal(length(unique(stage1_result$cluster_id)), 3)
})


# =============================================================================
# 6. BOUNDS EDGE CASES (min_n / max_n)
# =============================================================================

test_that("all strata get exactly min_n when forced", {
  frame <- data.frame(
    id = 1:100,
    region = rep(LETTERS[1:10], each = 10)
  )

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 10) |>
    execute(frame, seed = 42)

  counts <- table(result$region)
  expect_true(all(counts == 10))
})

test_that("bounds infeasibility detected: min_n too high", {
  frame <- data.frame(
    id = 1:100,
    region = rep(LETTERS[1:10], each = 10)
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 50, min_n = 10) |>
      execute(frame, seed = 42),
    "Cannot satisfy minimum"
  )
})

test_that("bounds infeasibility detected: max_n too low", {
  frame <- data.frame(
    id = 1:100,
    region = rep(LETTERS[1:10], each = 10)
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, max_n = 5) |>
      execute(frame, seed = 42),
    "Cannot satisfy maximum"
  )
})

test_that("min_n capped at stratum population", {
  frame <- data.frame(
    id = 1:25,
    region = c(rep("A", 3), rep("B", 22))
  )

  result <- sampling_design() |>
    stratify_by(region, alloc = "equal") |>
    draw(n = 15, min_n = 5) |>
    execute(frame, seed = 42)

  expect_equal(sum(result$region == "A"), 3)
  expect_equal(sum(result$region == "B"), 12)
})


# =============================================================================
# 7. PPS EDGE CASES
# =============================================================================

test_that("PPS with all equal sizes gives equal probabilities", {
  frame <- data.frame(
    id = 1:10,
    size = rep(100, 10)
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = size) |>
    execute(frame, seed = 42)

  expect_true(all(abs(result$.prob - 0.5) < 1e-10))
  expect_true(all(abs(result$.weight - 2) < 1e-10))
})

test_that("PPS with dominant unit selects it with certainty", {
  frame <- data.frame(
    id = 1:5,
    size = c(1, 1, 1, 1, 996)
  )

  result <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = size) |>
    execute(frame, seed = 42)

  expect_true(5 %in% result$id)

  unit5_prob <- result$.prob[result$id == 5]
  expect_equal(unit5_prob, 1, tolerance = 1e-10)
})

test_that("PPS with zero size unit never selects it", {
  frame <- data.frame(
    id = 1:5,
    size = c(0, 10, 20, 30, 40)
  )

  for (seed in 1:5) {
    result <- sampling_design() |>
      draw(n = 2, method = "pps_systematic", mos = size) |>
      execute(frame, seed = seed)

    expect_false(1 %in% result$id)
  }
})


# =============================================================================
# 8. DPLYR COMPATIBILITY TESTS
# =============================================================================
library(dplyr)
test_that("tbl_sample survives filter()", {
  frame <- data.frame(id = 1:100, region = rep(c("A", "B"), 50))

  sample <- sampling_design() |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  filtered <- sample |> filter(region == "A")

  expect_s3_class(filtered, "tbl_sample")
  expect_true(".weight" %in% names(filtered))
  expect_true(".prob" %in% names(filtered))
})

test_that("tbl_sample survives mutate()", {
  frame <- data.frame(id = 1:100)

  sample <- sampling_design() |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  mutated <- sample |> mutate(new_col = id * 2)

  expect_s3_class(mutated, "tbl_sample")
  expect_true("new_col" %in% names(mutated))
})

test_that("tbl_sample survives arrange()", {
  frame <- data.frame(id = 1:100, x = rnorm(100))

  sample <- sampling_design() |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  arranged <- sample |> arrange(desc(x))

  expect_s3_class(arranged, "tbl_sample")
  expect_true(all(diff(arranged$x) <= 0))
})

test_that("tbl_sample survives slice()", {
  frame <- data.frame(id = 1:100)

  sample <- sampling_design() |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  sliced <- sample |> slice(1:5)

  expect_equal(nrow(sliced), 5)
  expect_s3_class(sliced, "tbl_sample")
})

test_that("summarise() on tbl_sample returns regular tibble", {
  frame <- data.frame(id = 1:100, region = rep(c("A", "B"), 50), x = rnorm(100))

  sample <- sampling_design() |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  summarised <- sample |>
    group_by(region) |>
    summarise(mean_x = mean(x), .groups = "drop")

  expect_false(inherits(summarised, "tbl_sample"))
})

test_that("left_join() preserves tbl_sample class", {
  frame <- data.frame(id = 1:100, region = rep(c("A", "B"), 50))
  extra_data <- data.frame(region = c("A", "B"), label = c("North", "South"))

  sample <- sampling_design() |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  joined <- sample |> left_join(extra_data, by = "region")

  expect_s3_class(joined, "tbl_sample")
  expect_true("label" %in% names(joined))
})


# =============================================================================
# 9. INPUT VALIDATION EDGE CASES
# =============================================================================

test_that("tibble input works same as data.frame", {
  set.seed(999)
  df_frame <- data.frame(id = 1:100, x = rnorm(100))
  tbl_frame <- tibble::as_tibble(df_frame)

  design <- sampling_design() |> draw(n = 10)

  result_df <- execute(design, df_frame, seed = 42)
  result_tbl <- execute(design, tbl_frame, seed = 42)

  expect_equal(result_df$id, result_tbl$id)
})

test_that("grouped data frame input works", {
  frame <- data.frame(id = 1:100, region = rep(c("A", "B"), 50))
  grouped_frame <- frame |> group_by(region)

  design <- sampling_design() |> draw(n = 10)

  result <- execute(design, grouped_frame, seed = 42)

  expect_equal(nrow(result), 10)
})

test_that("frame with .weight and .prob columns doesn't break sampling", {
  frame <- data.frame(
    id = 1:100,
    x = rnorm(100)
  )
  frame$.weight <- rnorm(100)
  frame$.prob <- runif(100)

  design <- sampling_design() |> draw(n = 10)
  result <- execute(design, frame, seed = 42)

  # Output .weight should be the sampling weight (N/n = 10)
  expect_equal(unique(result$.weight), 10)
  expect_equal(unique(result$.prob), 0.1)
})


# =============================================================================
# 10. UNICODE AND SPECIAL CHARACTERS
# =============================================================================

test_that("unicode stratum names work", {
  frame <- data.frame(
    id = 1:100,
    region = rep(c("Île-de-France", "Rhône-Alpes"), 50),
    stringsAsFactors = FALSE
  )

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 20)
  expect_true("Île-de-France" %in% result$region)
  expect_true("Rhône-Alpes" %in% result$region)
})

test_that("stratum names with spaces work", {
  frame <- data.frame(
    id = 1:100,
    region = rep(c("New York", "Los Angeles"), 50)
  )

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10) |>
    execute(frame, seed = 42)

  expect_equal(sum(result$region == "New York"), 10)
  expect_equal(sum(result$region == "Los Angeles"), 10)
})

test_that("factor strata work correctly", {
  frame <- data.frame(
    id = 1:100,
    region = factor(rep(c("A", "B"), 50))
  )

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10) |>
    execute(frame, seed = 42)

  expect_equal(sum(result$region == "A"), 10)
  expect_equal(sum(result$region == "B"), 10)
})


# =============================================================================
# 11. WEIGHT CALCULATION VERIFICATION
# =============================================================================

test_that("SRS weights are correct", {
  frame <- data.frame(id = 1:100)

  result <- sampling_design() |>
    draw(n = 10) |>
    execute(frame, seed = 42)

  expect_equal(unique(result$.weight), 10)
  expect_equal(unique(result$.prob), 0.1)
})

test_that("stratified SRS weights are correct within strata", {
  frame <- data.frame(
    id = 1:100,
    region = c(rep("A", 40), rep("B", 60))
  )

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10) |>
    execute(frame, seed = 42)

  weights_A <- unique(result$.weight[result$region == "A"])
  weights_B <- unique(result$.weight[result$region == "B"])

  expect_equal(weights_A, 4)
  expect_equal(weights_B, 6)
})

test_that("cluster sampling weights are correct", {
  frame <- data.frame(
    cluster_id = rep(1:10, each = 5),
    id = 1:50
  )

  result <- sampling_design() |>
    cluster_by(cluster_id) |>
    draw(n = 4) |>
    execute(frame, seed = 42)

  expect_equal(unique(result$.weight), 2.5)
})
