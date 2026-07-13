balanced_feature_frame <- data.frame(
  id = seq_len(120),
  mos = rep(seq_len(30), 4),
  income = sin(seq_len(120) / 7),
  region = factor(rep(letters[1:4], each = 30)),
  urban = rep(c(FALSE, TRUE), 60),
  lon = rep(seq_len(12), 10),
  lat = rep(seq_len(10), each = 12)
)

test_that("bound() compiles categorical margins for the cube method", {
  design <- sampling_design() |>
    draw(
      n = 24,
      method = "cube",
      aux = c(bound(region), bound(urban))
    )

  spec <- design$stages[[1]]$draw_spec
  expect_null(spec$aux)
  expect_equal(spec$bounds, c("region", "urban"))
  expect_null(spec$method_variance)
  expect_identical(samplyr:::survey_stage_kind(spec), "unsupported")

  result <- execute(design, balanced_feature_frame, seed = 42)
  expect_equal(as.integer(table(result$region)), rep(6L, 4))
  expect_equal(as.integer(table(result$urban)), c(12L, 12L))
})

test_that("ordinary cube auxiliaries and bound() constraints compose", {
  result <- sampling_design() |>
    draw(
      n = 24,
      method = "cube",
      mos = mos,
      aux = c(income, bound(region))
    ) |>
    execute(balanced_feature_frame, seed = 8)

  pik <- sondage::inclusion_prob(balanced_feature_frame$mos, 24)
  expected <- vapply(
    levels(balanced_feature_frame$region),
    function(x) sum(pik[balanced_feature_frame$region == x]),
    numeric(1)
  )
  realised <- as.integer(table(result$region))
  expect_true(all(realised >= floor(expected)))
  expect_true(all(realised <= ceiling(expected)))
})

test_that("bound() reproduces the paper's controlled matrix rounding example", {
  # Tripet & Tillé (2026), "Balanced Sampling With Inequalities: Application
  # to Category Bounding, Matrix Rounding, and Spread Sampling",
  # doi:10.1080/01621459.2025.2550667, supplementary R code: Cochran's
  # 5 x 4 controlled-rounding matrix.
  cochran <- matrix(
    c(
      15, 21, 17, 9,
      10, 8, 13, 7,
      6, 9, 5, 8,
      4, 3, 6, 6,
      3, 2, 5, 8
    ),
    nrow = 5,
    byrow = TRUE
  )
  target <- cochran / 16.5
  target[target > 1] <- target[target > 1] - 1
  expect_equal(sum(target), 8)

  frame <- data.frame(
    cell = seq_along(target),
    row = factor(rep(seq_len(nrow(target)), times = ncol(target))),
    column = factor(rep(seq_len(ncol(target)), each = nrow(target))),
    pik = c(target)
  )
  expect_equal(sondage::inclusion_prob(frame$pik, 8), frame$pik)

  result <- sampling_design() |>
    draw(
      n = 8,
      method = "cube",
      mos = pik,
      aux = c(bound(row), bound(column))
    ) |>
    execute(frame, seed = 2026)

  rounded <- matrix(0L, nrow = 5, ncol = 4)
  rounded[result$cell] <- 1L
  row_error <- rowSums(rounded) - rowSums(target)
  column_error <- colSums(rounded) - colSums(target)

  expect_lt(max(abs(row_error)), 1)
  expect_lt(max(abs(column_error)), 1)
  expect_true(all(
    rowSums(rounded) == floor(rowSums(target)) |
      rowSums(rounded) == ceiling(rowSums(target))
  ))
  expect_true(all(
    colSums(rounded) == floor(colSums(target)) |
      colSums(rounded) == ceiling(colSums(target))
  ))
})

test_that("stratified bound() preserves allocated stratum sizes", {
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 6, method = "cube", aux = bound(urban)) |>
    execute(balanced_feature_frame, seed = 17)

  expect_equal(as.integer(table(result$region)), rep(6L, 4))
  expect_equal(as.integer(table(result$urban)), c(12L, 12L))
})

test_that("bound() syntax and method compatibility are validated", {
  expect_error(bound(region), "inside.*draw")
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "cube", aux = bound(region, urban)),
    "exactly one"
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "srswor", aux = bound(region)),
    "only supported"
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "cube", spread = c(lon, lat)),
    "spatially balanced"
  )
})

test_that("balanced is a compatibility alias normalized to cube", {
  design <- sampling_design() |>
    draw(n = 12, method = "balanced", aux = income)
  expect_identical(design$stages[[1]]$draw_spec$method, "cube")
})

test_that("lpm2 and scps support equal and unequal spatial draws", {
  for (method in spatial_balanced_methods) {
    unequal <- sampling_design() |>
      draw(n = 18, method = method, mos = mos, spread = c(lon, lat)) |>
      execute(balanced_feature_frame, seed = 21)
    expect_equal(nrow(unequal), 18)
    expect_gt(length(unique(round(unequal$.weight, 8))), 1)

    equal <- sampling_design() |>
      draw(n = 18, method = method, spread = c(lon, lat)) |>
      execute(balanced_feature_frame, seed = 22)
    expect_equal(nrow(equal), 18)
    expect_equal(length(unique(equal$.weight)), 1)
  }
})

test_that("spatial methods validate their capability contract", {
  expect_error(
    sampling_design() |> draw(n = 10, method = "lpm2"),
    "requires.*spread"
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "scps", spread = c(lon, lat), aux = income),
    "cannot combine"
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "lpm2", spread = c(lon, lat), prn = income),
    "permanent random numbers"
  )
})

test_that("stratified spatial methods spread within allocated strata", {
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4, method = "lpm2", spread = c(lon, lat)) |>
    execute(balanced_feature_frame, seed = 9)

  expect_equal(as.integer(table(result$region)), rep(4L, 4))
})

test_that("clustered spatial coordinates must be cluster invariant", {
  frame <- transform(
    balanced_feature_frame,
    cluster = rep(seq_len(60), each = 2),
    cluster_lon = rep(seq_len(60), each = 2),
    cluster_lat = rep(seq_len(12), each = 10)
  )
  result <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 10, method = "scps", spread = c(cluster_lon, cluster_lat)) |>
    execute(frame, seed = 5)
  expect_equal(length(unique(result$cluster)), 10)

  frame$cluster_lon[2] <- -1
  expect_error(
    sampling_design() |>
      cluster_by(cluster) |>
      draw(n = 10, method = "scps", spread = c(cluster_lon, cluster_lat)) |>
      execute(frame, seed = 5),
    "constant within each cluster"
  )
})

test_that("balanced constraint fields serialize and print", {
  bounded <- sampling_design() |>
    draw(n = 20, method = "cube", aux = c(income, bound(region)))
  bounded_rt <- read_design(design_json(bounded))
  expect_equal(bounded_rt$stages[[1]]$draw_spec$aux, "income")
  expect_equal(bounded_rt$stages[[1]]$draw_spec$bounds, "region")

  spatial <- sampling_design() |>
    draw(n = 20, method = "lpm2", spread = c(lon, lat))
  spatial_rt <- read_design(design_json(spatial))
  expect_equal(spatial_rt$stages[[1]]$draw_spec$spread, c("lon", "lat"))
  expect_snapshot(print(spatial))
})

test_that("joint and linearization export refuse constrained balanced designs", {
  bounded <- sampling_design() |>
    draw(n = 20, method = "cube", aux = bound(region)) |>
    execute(balanced_feature_frame, seed = 4)
  spatial <- sampling_design() |>
    draw(n = 20, method = "scps", spread = c(lon, lat)) |>
    execute(balanced_feature_frame, seed = 4)

  expect_error(joint_expectation(bounded, balanced_feature_frame), "unavailable")
  expect_error(joint_expectation(spatial, balanced_feature_frame), "unavailable")

  skip_if_not_installed("survey")
  expect_error(as_svydesign(bounded), "Cannot export")
  expect_error(as_svydesign(spatial), "Cannot export")
  expect_s3_class(
    as_svrepdesign(bounded, type = "subbootstrap", replicates = 10),
    "svyrep.design"
  )
  expect_s3_class(
    as_svrepdesign(spatial, type = "subbootstrap", replicates = 10),
    "svyrep.design"
  )
})

test_that("Sampford is a built-in fixed-size PPS method", {
  design <- sampling_design() |>
    draw(n = 20, method = "pps_sampford", mos = mos)
  spec <- design$stages[[1]]$draw_spec
  expect_null(spec$method_type)
  expect_true(spec$method %in% pps_wor_methods)

  result <- execute(design, balanced_feature_frame, seed = 31, reps = 2)
  expect_equal(as.integer(table(result$.replicate)), c(20L, 20L))

  one <- dplyr::filter(result, .replicate == 1)
  jip <- joint_expectation(one, balanced_feature_frame)$stage_1
  expect_equal(dim(jip), c(20L, 20L))
  expect_true(isSymmetric(jip))
  expect_identical(samplyr:::survey_stage_kind(spec), "pps_wor")
})
