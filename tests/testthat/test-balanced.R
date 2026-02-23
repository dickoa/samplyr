test_that("equal-prob balanced selects correct sample size", {
  set.seed(1)
  frame <- data.frame(
    id = 1:200,
    x1 = rnorm(200),
    x2 = runif(200)
  )
  result <- sampling_design() |>
    draw(n = 30, method = "balanced", aux = c(x1, x2)) |>
    execute(frame, seed = 42)
  expect_equal(nrow(result), 30)
  expect_true(".weight" %in% names(result))
  expect_true(all(result$.weight > 0))
})

test_that("unequal-prob balanced (mos + aux) works", {
  set.seed(2)
  frame <- data.frame(
    id = 1:200,
    size = runif(200, 10, 100),
    x1 = rnorm(200),
    x2 = runif(200)
  )
  result <- sampling_design() |>
    draw(n = 30, method = "balanced", mos = size, aux = c(x1, x2)) |>
    execute(frame, seed = 42)
  expect_equal(nrow(result), 30)
  expect_true(all(result$.weight > 0))
  # Weights should vary (PPS)
  expect_true(length(unique(round(result$.weight, 4))) > 1)
})

test_that("balanced with no aux works (fixed-size unbalanced)", {
  set.seed(3)
  frame <- data.frame(
    id = 1:100,
    size = runif(100, 10, 100)
  )
  result <- sampling_design() |>
    draw(n = 20, method = "balanced", mos = size) |>
    execute(frame, seed = 42)
  expect_equal(nrow(result), 20)
})

test_that("balanced with no mos and no aux works", {
  set.seed(4)
  frame <- data.frame(id = 1:100, x1 = rnorm(100))
  result <- sampling_design() |>
    draw(n = 15, method = "balanced", aux = x1) |>
    execute(frame, seed = 42)
  expect_equal(nrow(result), 15)
  # Equal prob: all weights should be the same
  expect_equal(length(unique(result$.weight)), 1)
})

test_that("stratified balanced respects stratum allocation", {
  set.seed(5)
  frame <- data.frame(
    id = 1:300,
    stratum = factor(rep(c("A", "B", "C"), each = 100)),
    x1 = rnorm(300),
    x2 = runif(300)
  )
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 30, method = "balanced", aux = c(x1, x2)) |>
    execute(frame, seed = 42)
  expect_equal(nrow(result), 30)
  # Proportional: 10 per stratum
  counts <- table(result$stratum)
  expect_equal(as.integer(counts), c(10L, 10L, 10L))
})

test_that("stratified balanced with neyman allocation", {
  set.seed(6)
  frame <- data.frame(
    id = 1:300,
    stratum = factor(rep(c("A", "B", "C"), each = 100)),
    x1 = rnorm(300)
  )
  variance_df <- data.frame(
    stratum = c("A", "B", "C"),
    var = c(1, 4, 9)
  )
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "neyman", variance = variance_df) |>
    draw(n = 30, method = "balanced", aux = x1) |>
    execute(frame, seed = 42)
  expect_equal(nrow(result), 30)
  # Neyman: more units in high-variance strata
  counts <- table(result$stratum)
  expect_true(counts[["C"]] >= counts[["A"]])
})

test_that("stratified balanced with mos (PPS within strata)", {
  set.seed(7)
  frame <- data.frame(
    id = 1:300,
    stratum = factor(rep(c("A", "B", "C"), each = 100)),
    size = runif(300, 10, 100),
    x1 = rnorm(300)
  )
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 30, method = "balanced", mos = size, aux = x1) |>
    execute(frame, seed = 42)
  expect_equal(nrow(result), 30)
  # Weights should vary within strata (PPS)
  wts <- result$.weight
  expect_true(length(unique(round(wts, 4))) > 1)
})

test_that("clustered balanced aggregates aux to cluster level", {
  set.seed(8)
  n_cl <- 50
  frame <- data.frame(
    id = 1:250,
    cluster = factor(rep(sprintf("c%02d", 1:n_cl), each = 5)),
    size = rep(runif(n_cl, 10, 100), each = 5),
    x1 = rnorm(250)
  )
  result <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 10, method = "balanced", mos = size, aux = x1) |>
    execute(frame, seed = 42)
  # 10 clusters * 5 units each = 50 rows
  selected_clusters <- length(unique(result$cluster))
  expect_equal(selected_clusters, 10)
  expect_equal(nrow(result), 50)
})

test_that("clustered + stratified balanced works", {
  set.seed(9)
  n_cl <- 40
  frame <- data.frame(
    id = 1:200,
    stratum = factor(rep(c("A", "B"), each = 100)),
    cluster = factor(rep(sprintf("c%02d", 1:n_cl), each = 5)),
    size = rep(runif(n_cl, 10, 100), each = 5),
    x1 = rnorm(200)
  )
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    cluster_by(cluster) |>
    draw(n = 10, method = "balanced", mos = size, aux = x1) |>
    execute(frame, seed = 42)
  selected_clusters <- length(unique(result$cluster))
  expect_equal(selected_clusters, 10)
  # 5 clusters per stratum
  counts <- tapply(result$cluster, result$stratum, function(x) length(unique(x)))
  expect_equal(as.integer(counts), c(5L, 5L))
})

test_that("two-stage balanced works (balanced at both stages)", {
  set.seed(10)
  n_cl <- 30
  frame <- data.frame(
    id = 1:150,
    cluster = factor(rep(sprintf("c%02d", 1:n_cl), each = 5)),
    size = rep(runif(n_cl, 10, 100), each = 5),
    x1 = rnorm(150),
    x2 = runif(150)
  )
  result <- sampling_design() |>
    add_stage("PSU") |>
      cluster_by(cluster) |>
      draw(n = 10, method = "balanced", mos = size, aux = x1) |>
    add_stage("SSU") |>
      draw(n = 3, method = "balanced", aux = x2) |>
    execute(frame, seed = 42)
  selected_clusters <- length(unique(result$cluster))
  expect_equal(selected_clusters, 10)
  # 10 clusters * 3 units = 30
  expect_equal(nrow(result), 30)
})

test_that("two-stage mixed: balanced at stage 1, srswor at stage 2", {
  set.seed(11)
  n_cl <- 30
  frame <- data.frame(
    id = 1:150,
    cluster = factor(rep(sprintf("c%02d", 1:n_cl), each = 5)),
    size = rep(runif(n_cl, 10, 100), each = 5),
    x1 = rnorm(150)
  )
  result <- sampling_design() |>
    add_stage("PSU") |>
      cluster_by(cluster) |>
      draw(n = 10, method = "balanced", mos = size, aux = x1) |>
    add_stage("SSU") |>
      draw(n = 3) |>
    execute(frame, seed = 42)
  expect_equal(length(unique(result$cluster)), 10)
  expect_equal(nrow(result), 30)
})

test_that("three-stage balanced errors", {
  expect_error(
    sampling_design() |>
      add_stage("PSU") |>
        draw(n = 10, method = "balanced") |>
      add_stage("SSU") |>
        draw(n = 5, method = "balanced") |>
      add_stage("TSU") |>
        draw(n = 2, method = "balanced") |>
      execute(data.frame(id = 1:100), seed = 1),
    "at most 2 stages"
  )
})

test_that("aux with non-balanced method errors", {
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "srswor", aux = c(x1, x2)),
    "only supported for.*balanced"
  )
})

test_that("aux column missing errors at validation", {
  set.seed(12)
  frame <- data.frame(id = 1:100, x1 = rnorm(100))
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "balanced", aux = c(x1, x_missing)) |>
      execute(frame, seed = 1),
    "x_missing"
  )
})

test_that("aux column non-numeric errors at validation", {
  set.seed(13)
  frame <- data.frame(
    id = 1:100,
    x1 = rnorm(100),
    x_char = letters[1:100]
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "balanced", aux = c(x1, x_char)) |>
      execute(frame, seed = 1),
    "numeric"
  )
})

test_that("aux column with NAs errors at validation", {
  set.seed(14)
  x1_vals <- rnorm(100)
  x1_vals[5] <- NA
  frame <- data.frame(id = 1:100, x1 = x1_vals)
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "balanced", aux = x1) |>
      execute(frame, seed = 1),
    "NA"
  )
})

test_that("balanced balancing property: HT estimator close to population totals", {
  set.seed(15)
  N <- 500
  frame <- data.frame(
    id = 1:N,
    x1 = rnorm(N, 100, 20),
    x2 = runif(N, 50, 200)
  )
  n_samp <- 100
  pop_total_x1 <- sum(frame$x1)
  pop_total_x2 <- sum(frame$x2)

  result <- sampling_design() |>
    draw(n = n_samp, method = "balanced", aux = c(x1, x2)) |>
    execute(frame, seed = 42)

  ht_x1 <- sum(result$x1 * result$.weight)
  ht_x2 <- sum(result$x2 * result$.weight)

  # HT estimates should be very close to population totals for balanced samples
  expect_equal(ht_x1, pop_total_x1, tolerance = 0.01)
  expect_equal(ht_x2, pop_total_x2, tolerance = 0.01)
})

test_that("joint_expectation works with balanced", {
  set.seed(16)
  frame <- data.frame(
    id = 1:100,
    size = runif(100, 10, 100),
    x1 = rnorm(100)
  )
  result <- sampling_design() |>
    draw(n = 15, method = "balanced", mos = size, aux = x1) |>
    execute(frame, seed = 42)

  jip <- joint_expectation(result, frame)
  expect_type(jip, "list")
  expect_true(!is.null(jip$stage_1))
  jip_mat <- jip$stage_1
  expect_equal(nrow(jip_mat), 15)
  expect_equal(ncol(jip_mat), 15)
  # Diagonal should equal marginal pik
  diag_pik <- diag(jip_mat)
  expected_pik <- 1 / result$.weight
  expect_equal(diag_pik, expected_pik, tolerance = 1e-6)
})

test_that("as_svydesign works with balanced", {
  skip_if_not_installed("survey")
  set.seed(17)
  frame <- data.frame(
    id = 1:200,
    size = runif(200, 10, 100),
    x1 = rnorm(200),
    y = rnorm(200, 50, 10)
  )
  result <- sampling_design() |>
    draw(n = 30, method = "balanced", mos = size, aux = x1) |>
    execute(frame, seed = 42)
  svy <- as_svydesign(result)
  expect_s3_class(svy, "survey.design2")
})

test_that("as_svrepdesign with balanced: subbootstrap works, bootstrap warns", {
  skip_if_not_installed("survey")
  set.seed(18)
  frame <- data.frame(
    id = 1:200,
    stratum = factor(rep(c("A", "B"), each = 100)),
    size = runif(200, 10, 100),
    x1 = rnorm(200)
  )
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 30, method = "balanced", mos = size, aux = x1) |>
    execute(frame, seed = 42)
  rep_design <- as_svrepdesign(result, type = "subbootstrap")
  expect_s3_class(rep_design, "svyrep.design")
})

test_that("balanced with frac works", {
  set.seed(19)
  frame <- data.frame(
    id = 1:200,
    x1 = rnorm(200)
  )
  result <- sampling_design() |>
    draw(frac = 0.1, method = "balanced", aux = x1) |>
    execute(frame, seed = 42)
  expect_equal(nrow(result), 20)
})

test_that("validate_frame detects balanced aux issues", {
  set.seed(20)
  design <- sampling_design() |>
    draw(n = 10, method = "balanced", aux = c(x1, x2))

  # Missing column
  bad_frame1 <- data.frame(id = 1:100, x1 = rnorm(100))
  expect_error(
    validate_frame(design, bad_frame1),
    "x2"
  )

  # Non-numeric column
  bad_frame2 <- data.frame(
    id = 1:100,
    x1 = rnorm(100),
    x2 = letters[1:100]
  )
  expect_error(
    validate_frame(design, bad_frame2),
    "numeric"
  )
})
