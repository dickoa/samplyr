# Coverage for as.list.sampling_design(): exercises the optional-field
# branches (labels, strata variance/cost columns, clusters, data-frame n/frac,
# control, on_empty) that the round-trip serialization records.

test_that("as.list records labels, strata aux columns, and clusters", {
  strata <- c("A", "B", "C", "D")
  design <- sampling_design(title = "Multi") |>
    add_stage(label = "PSU") |>
    stratify_by(
      stratum,
      alloc = "optimal",
      variance = data.frame(stratum = strata, var = c(1.2, 0.8, 1, 1)),
      cost = data.frame(stratum = strata, cost = c(1, 2, 1, 2))
    ) |>
    cluster_by(cluster) |>
    draw(n = 6, method = "pps_brewer", mos = size)

  out <- as.list(design)
  expect_equal(out$title, "Multi")
  stage <- out$stages[[1]]
  expect_equal(stage$label, "PSU")
  expect_equal(stage$strata$alloc, "optimal")
  expect_true("var" %in% stage$strata$variance_columns)
  expect_true("cost" %in% stage$strata$cost_columns)
  expect_equal(stage$clusters$vars, "cluster")
  expect_equal(stage$draw$method, "pps_brewer")
})

test_that("as.list records a custom data-frame n", {
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = data.frame(stratum = c("A", "B"), n = c(5L, 5L)))

  out <- as.list(design)
  expect_equal(out$stages[[1]]$draw$n, "custom (data frame)")
  expect_true("n" %in% out$stages[[1]]$draw$n_columns)
})

test_that("as.list records a custom data-frame frac", {
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = data.frame(stratum = c("A", "B"), frac = c(0.2, 0.2)))

  out <- as.list(design)
  expect_equal(out$stages[[1]]$draw$frac, "custom (data frame)")
  expect_true("frac" %in% out$stages[[1]]$draw$frac_columns)
})

test_that("as.list records scalar frac, control, and non-default on_empty", {
  design <- sampling_design() |>
    draw(
      frac = 0.3,
      method = "systematic",
      control = c(cluster, y),
      on_empty = "warn"
    )

  out <- as.list(design)
  draw <- out$stages[[1]]$draw
  expect_equal(draw$frac, 0.3)
  expect_equal(draw$control, c("cluster", "y"))
  expect_equal(draw$on_empty, "warn")
})
