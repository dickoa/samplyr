test_that("add_stage() creates a new stage", {
  d <- sampling_design() |>
    add_stage(label = "First") |>
    draw(n = 10)

  expect_s3_class(d, "sampling_design")
  expect_equal(length(d$stages), 1)
  expect_equal(d$stages[[1]]$label, "First")
})

test_that("add_stage() adds subsequent stages", {
  d <- sampling_design() |>
    add_stage(label = "Stage 1") |>
    cluster_by(school) |>
    draw(n = 50) |>
    add_stage(label = "Stage 2") |>
    draw(n = 20)

  expect_equal(length(d$stages), 2)
  expect_equal(d$stages[[1]]$label, "Stage 1")
  expect_equal(d$stages[[2]]$label, "Stage 2")
})

test_that("add_stage() requires previous stage to have draw()", {
  expect_error(
    sampling_design() |>
      add_stage(label = "Stage 1") |>
      cluster_by(school) |>
      add_stage(label = "Stage 2"),
    "no.*draw"
  )
})

test_that("add_stage() accepts optional label", {
  d1 <- sampling_design() |>
    add_stage() |>
    draw(n = 10)

  d2 <- sampling_design() |>
    add_stage(label = "My Stage") |>
    draw(n = 10)

  expect_null(d1$stages[[1]]$label)
  expect_equal(d2$stages[[1]]$label, "My Stage")
})

test_that("add_stage() validates label type", {
  expect_error(
    sampling_design() |> add_stage(label = 123),
    "character"
  )

  expect_error(
    sampling_design() |> add_stage(label = c("a", "b")),
    "single"
  )
})

test_that("multi-stage design works correctly", {
  d <- sampling_design() |>
    add_stage(label = "Schools") |>
    stratify_by(region) |>
    cluster_by(school_id) |>
    draw(n = 10, method = "pps_brewer", mos = enrollment) |>
    add_stage(label = "Students") |>
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
    add_stage(label = "Districts") |>
    stratify_by(region) |>
    cluster_by(district) |>
    draw(n = 5) |>
    add_stage(label = "Villages") |>
    stratify_by(urban_rural) |>
    cluster_by(village) |>
    draw(n = 2) |>
    add_stage(label = "Households") |>
    draw(n = 10)

  expect_equal(length(d$stages), 3)
  expect_equal(d$stages[[1]]$strata$vars, "region")
  expect_equal(d$stages[[2]]$strata$vars, "urban_rural")
  expect_null(d$stages[[3]]$strata)
})

## draw() closes a stage
#
# draw() reads the stage's strata and clusters when it is called, so nothing
# may change them afterwards. `draw(n = plan) |> stratify_by(region)` used to
# take a plan's total in every region.

test_that("a verb after draw() on the same stage is refused", {
  closed <- sampling_design() |> add_stage("EAs") |> draw(n = 10)
  expect_error(
    closed |> stratify_by(region),
    class = "samplyr_error_stage_closed"
  )
  expect_error(closed |> cluster_by(ea), class = "samplyr_error_stage_closed")
  expect_error(closed |> draw(n = 5), class = "samplyr_error_stage_closed")

  # The order is refused before the arguments are read.
  expect_error(closed |> draw(n = -5), class = "samplyr_error_stage_closed")
  expect_error(
    closed |> stratify_by(region, alloc = "nonsense"),
    class = "samplyr_error_stage_closed"
  )
})

test_that("a design from a file or a sample is closed at its last stage", {
  design <- sampling_design() |> draw(n = 5)
  path <- tempfile(fileext = ".json")
  write_design(design, path)
  expect_error(
    read_design(path) |> stratify_by(region),
    class = "samplyr_error_stage_closed"
  )

  sample <- execute(design, bfa_eas, seed = 1)
  expect_error(
    get_design(sample) |> cluster_by(ea_id),
    class = "samplyr_error_stage_closed"
  )

  # add_stage() opens the next stage, which takes every verb.
  extended <- read_design(path) |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = 2)
  expect_identical(extended$stages[[2]]$strata$vars, "region")
  expect_identical(extended$stages[[2]]$clusters$vars, "ea_id")
})

test_that("strata and clusters may be declared in either order", {
  one <- sampling_design() |> stratify_by(region) |> cluster_by(ea_id) |>
    draw(n = 2)
  other <- sampling_design() |> cluster_by(ea_id) |> stratify_by(region) |>
    draw(n = 2)
  expect_identical(one$stages, other$stages)
})
