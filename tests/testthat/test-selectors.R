test_that("stratify_by() rejects tidy-select helpers and expressions", {
  expect_error(
    sampling_design() |>
      stratify_by(starts_with("reg")),
    "bare column names"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region == "A"),
    "bare column names"
  )
})

test_that("cluster_by() rejects tidy-select helpers and expressions", {
  expect_error(
    sampling_design() |>
      cluster_by(ends_with("_id")),
    "bare column names"
  )

  expect_error(
    sampling_design() |>
      cluster_by(paste0("ea", "_id")),
    "bare column names"
  )
})
