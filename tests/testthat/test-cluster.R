# Coverage for cluster_by() input guards.

test_that("cluster_by requires a sampling_design", {
  expect_error(cluster_by(list(), ea), "sampling_design")
})

test_that("cluster_by requires at least one variable", {
  design <- sampling_design()
  expect_error(cluster_by(design), "At least one clustering variable")
})

test_that("cluster_by rejects expressions and tidy-select helpers", {
  design <- sampling_design()
  expect_error(cluster_by(design, starts_with("ea")), "bare column names")
})

test_that("cluster_by rejects a second clustering on the same stage", {
  design <- sampling_design() |>
    cluster_by(cluster)
  expect_error(
    cluster_by(design, stratum),
    "Clustering already defined"
  )
})
