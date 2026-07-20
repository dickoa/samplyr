test_that("trace-disabled selection preserves the seeded sample", {
  frame <- data.frame(
    id = seq_len(120),
    stratum = rep(letters[1:4], each = 30)
  )
  design <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 24)
  spec <- design$stages[[1L]]

  full <- withr::with_seed(
    91,
    samplyr:::sample_units(
      frame,
      spec$strata,
      spec$draw_spec,
      trace_mode = "full"
    )
  )
  none <- withr::with_seed(
    91,
    samplyr:::sample_units(
      frame,
      spec$strata,
      spec$draw_spec,
      trace_mode = "none"
    )
  )

  expect_identical(none$sample, full$sample)
  expect_null(none$trace)
  expect_type(full$trace, "list")
})

test_that("frame_digest mode does not affect selection", {
  frame <- data.frame(
    id = seq_len(200),
    stratum = rep(letters[1:10], each = 20)
  )
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 4)

  none <- execute(design, frame, seed = 11, frame_digest = "none")
  summary <- execute(design, frame, seed = 11, frame_digest = "summary")

  expect_identical(names(none), names(summary))
  for (column in names(none)) {
    expect_identical(none[[column]], summary[[column]])
  }
  expect_null(get_frame_digest(none))
  expect_type(get_frame_digest(summary), "list")
})

test_that("grouped SRS fast path matches direct sample.int draws", {
  frame <- data.frame(id = seq_len(15))
  indices <- list(1:5, 6:9, 10:15)
  n_per_group <- c(2L, 4L, 3L)

  expected <- withr::with_seed(72, {
    unlist(Map(
      function(rows, n) rows[sample.int(length(rows), n)],
      indices,
      n_per_group
    ), use.names = FALSE)
  })
  actual <- withr::with_seed(
    72,
    samplyr:::sample_srswor_by_group_indices(
      frame,
      indices,
      n_per_group,
      trace_mode = "none"
    )
  )

  expect_identical(actual$sample$id, expected)
  expect_equal(actual$sample$.weight, c(rep(2.5, 2), rep(1, 4), rep(2, 3)))
  expect_equal(actual$sample$.fpc, c(rep(5, 2), rep(4, 4), rep(6, 3)))
  expect_null(actual$leaves)
})

test_that("summary traces compact constant chances", {
  frame <- data.frame(
    id = seq_len(40),
    stratum = rep(c("a", "b"), each = 20)
  )
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5)
  spec <- design$stages[[1L]]

  result <- withr::with_seed(
    3,
    samplyr:::sample_units(
      frame,
      spec$strata,
      spec$draw_spec,
      trace_mode = "summary"
    )
  )

  chances <- lapply(result$trace$groups, function(group) group$node$chance)
  expect_true(all(lengths(chances) == 1L))
  expect_equal(unlist(chances), c(0.25, 0.25))
})

test_that("summary digest expands compact chances into correct totals", {
  frame <- data.frame(
    id = seq_len(60),
    stratum = rep(letters[1:3], each = 20)
  )
  sample <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5) |>
    execute(frame, seed = 8, frame_digest = "summary")

  pools <- get_frame_digest(sample)$stages[[1L]]$pools

  expect_equal(pools$N, rep(20L, 3))
  expect_equal(pools$n_expected, rep(5, 3))
  expect_equal(pools$chance, rep(0.25, 3))
})
