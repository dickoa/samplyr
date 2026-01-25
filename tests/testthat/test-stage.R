# Tests for stage()

test_that("stage() creates a new stage", {
  d <- sampling_design() |>
    stage(label = "First") |>
    draw(n = 10)
  
  expect_s3_class(d, "sampling_design")
  expect_equal(length(d$stages), 1)
  expect_equal(d$stages[[1]]$label, "First")
})

test_that("stage() adds subsequent stages", {
  d <- sampling_design() |>
    stage(label = "Stage 1") |>
    cluster_by(school) |>
    draw(n = 50) |>
    stage(label = "Stage 2") |>
    draw(n = 20)
  
  expect_equal(length(d$stages), 2)
  expect_equal(d$stages[[1]]$label, "Stage 1")
  expect_equal(d$stages[[2]]$label, "Stage 2")
})

test_that("stage() requires previous stage to have draw()", {
  expect_error(
    sampling_design() |>
      stage(label = "Stage 1") |>
      cluster_by(school) |>
      stage(label = "Stage 2"),
    "no.*draw"
  )
})

test_that("stage() accepts optional label", {
  d1 <- sampling_design() |>
    stage() |>
    draw(n = 10)
  
  d2 <- sampling_design() |>
    stage(label = "My Stage") |>
    draw(n = 10)
  
  expect_null(d1$stages[[1]]$label)
  expect_equal(d2$stages[[1]]$label, "My Stage")
})

test_that("stage() validates label type", {
  expect_error(
    sampling_design() |> stage(label = 123),
    "character"
  )
  
  expect_error(
    sampling_design() |> stage(label = c("a", "b")),
    "single"
  )
})

test_that("multi-stage design works correctly", {
  d <- sampling_design() |>
    stage(label = "Schools") |>
      stratify_by(region) |>
      cluster_by(school_id) |>
      draw(n = 10, method = "pps_brewer", mos = enrollment) |>
    stage(label = "Students") |>
      draw(n = 20)
  
  expect_equal(length(d$stages), 2)
  
  # Stage 1
  expect_equal(d$stages[[1]]$label, "Schools")
  expect_equal(d$stages[[1]]$strata$vars, "region")
  expect_equal(d$stages[[1]]$clusters$vars, "school_id")
  expect_equal(d$stages[[1]]$draw_spec$n, 10)
  expect_equal(d$stages[[1]]$draw_spec$method, "pps_brewer")
  expect_equal(d$stages[[1]]$draw_spec$mos, "enrollment")
  
  # Stage 2
  expect_equal(d$stages[[2]]$label, "Students")
  expect_null(d$stages[[2]]$strata)
  expect_null(d$stages[[2]]$clusters)
  expect_equal(d$stages[[2]]$draw_spec$n, 20)
})

test_that("each stage can have its own stratification", {
  d <- sampling_design() |>
    stage(label = "Districts") |>
      stratify_by(region) |>
      cluster_by(district) |>
      draw(n = 5) |>
    stage(label = "Villages") |>
      stratify_by(urban_rural) |>
      cluster_by(village) |>
      draw(n = 2) |>
    stage(label = "Households") |>
      draw(n = 10)
  
  expect_equal(length(d$stages), 3)
  expect_equal(d$stages[[1]]$strata$vars, "region")
  expect_equal(d$stages[[2]]$strata$vars, "urban_rural")
  expect_null(d$stages[[3]]$strata)
})
