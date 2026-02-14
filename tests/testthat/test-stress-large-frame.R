test_that("large stratified execution remains stable", {
  skip_on_cran()

  set.seed(42)
  n <- 100000L
  frame <- data.frame(
    id = seq_len(n),
    region = sample(sprintf("R%02d", 1:12), n, replace = TRUE),
    strata = sample(c("urban", "rural"), n, replace = TRUE),
    hh_count = sample(10:200, n, replace = TRUE),
    stringsAsFactors = FALSE
  )

  design <- sampling_design() |>
    stratify_by(region, strata, alloc = "proportional") |>
    draw(n = 8000)

  elapsed <- system.time({
    sample <- execute(design, frame, seed = 123)
  })[["elapsed"]]

  expect_equal(nrow(sample), 8000)
  expect_true(all(sample$.weight > 0))
  expect_true(all(sample$.fpc_1 >= 1))
  expect_lt(elapsed, 30)
})

test_that("large multi-stage execution remains stable", {
  skip_on_cran()

  set.seed(123)
  n_clusters <- 5000L
  cluster_size <- 20L

  clusters <- data.frame(
    ea_id = seq_len(n_clusters),
    region = sample(sprintf("R%02d", 1:10), n_clusters, replace = TRUE),
    hh_count = sample(20:250, n_clusters, replace = TRUE),
    stringsAsFactors = FALSE
  )

  frame <- merge(
    clusters,
    data.frame(unit_in_ea = seq_len(cluster_size)),
    by = NULL
  )
  frame$unit_id <- seq_len(nrow(frame))

  design <- sampling_design() |>
    add_stage(label = "EA") |>
      stratify_by(region, alloc = "proportional") |>
      cluster_by(ea_id) |>
      draw(n = 400, method = "pps_brewer", mos = hh_count) |>
    add_stage(label = "Household") |>
      draw(n = 5)

  elapsed <- system.time({
    sample <- execute(design, frame, seed = 11)
  })[["elapsed"]]

  expect_equal(length(unique(sample$ea_id)), 400)
  expect_equal(nrow(sample), 2000)
  expect_true(all(sample$.weight > 0))
  expect_lt(elapsed, 45)
})

test_that("high-cardinality strata with two-stage clustered srswor remains stable", {
  skip_on_cran()

  set.seed(321)
  n_strata <- 2000L
  clusters_per_stratum <- 2L
  cluster_size <- 8L

  clusters <- data.frame(
    cluster_id = seq_len(n_strata * clusters_per_stratum),
    stratum_id = rep(sprintf("S%04d", seq_len(n_strata)), each = clusters_per_stratum),
    stringsAsFactors = FALSE
  )

  frame <- merge(
    clusters,
    data.frame(unit_in_cluster = seq_len(cluster_size)),
    by = NULL
  )
  frame$unit_id <- seq_len(nrow(frame))

  design <- sampling_design() |>
    add_stage(label = "PSU") |>
      stratify_by(stratum_id, alloc = "proportional") |>
      cluster_by(cluster_id) |>
      draw(n = n_strata) |>
    add_stage(label = "SSU") |>
      draw(n = 3)

  elapsed <- system.time({
    sample <- execute(design, frame, seed = 99)
  })[["elapsed"]]

  expect_equal(length(unique(sample$stratum_id)), n_strata)
  expect_equal(length(unique(sample$cluster_id)), n_strata)
  expect_equal(nrow(sample), n_strata * 3L)
  expect_true(all(table(sample$cluster_id) == 3L))
  expect_equal(unique(sample$.weight_1), 2, tolerance = 1e-10)
  expect_equal(unique(sample$.fpc_1), clusters_per_stratum)
  expect_equal(unique(sample$.fpc_2), cluster_size)
  expect_equal(sample$.weight, sample$.weight_1 * sample$.weight_2, tolerance = 1e-10)
  expect_lt(elapsed, 60)
})
