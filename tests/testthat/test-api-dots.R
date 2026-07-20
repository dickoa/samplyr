test_that("reserved dots reject unexpected arguments", {
  design <- sampling_design() |>
    draw(n = 3)
  sample <- execute(
    design,
    data.frame(id = 1:10),
    seed = 1,
    frame_digest = "none"
  )
  plain <- tibble::as_tibble(sample)

  expect_error(
    print(design, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    as.list(design, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    summary(sample, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    as_tbl_sample(sample, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    as_tbl_sample(plain, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
})
