test_that("panels produces .panel column with correct values", {
  frame <- data.frame(
    id = 1:100,
    value = rnorm(100)
  )

  result <- sampling_design() |>
    draw(n = 40) |>
    execute(frame, seed = 42, panels = 4)

  expect_true(".panel" %in% names(result))
  expect_equal(sort(unique(result$.panel)), 1:4)
})

test_that("panels = NULL produces no .panel column", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  result <- sampling_design() |>
    draw(n = 40) |>
    execute(frame, seed = 42)

  expect_false(".panel" %in% names(result))
})

test_that("panels splits evenly when n divisible by k", {
  frame <- data.frame(id = 1:200, value = rnorm(200))

  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42, panels = 4)

  panel_counts <- table(result$.panel)
  expect_equal(as.integer(panel_counts), rep(25L, 4))
})

test_that("panels differ by at most 1 when n not divisible by k", {
  frame <- data.frame(id = 1:200, value = rnorm(200))

  result <- sampling_design() |>
    draw(n = 101) |>
    execute(frame, seed = 42, panels = 4)

  panel_counts <- as.integer(table(result$.panel))
  expect_true(max(panel_counts) - min(panel_counts) <= 1)
})

test_that("stratified panels have equal representation per stratum", {
  frame <- data.frame(
    id = 1:200,
    region = rep(c("North", "South", "East", "West"), each = 50),
    value = rnorm(200)
  )

  expect_warning(
    result <- sampling_design() |>
      stratify_by(region) |>
      draw(n = 100) |>
      execute(frame, seed = 42, panels = 4),
    "capped to population"
  )

  # Each panel should have equal count per stratum
  panel_strata <- table(result$region, result$.panel)
  for (i in seq_len(nrow(panel_strata))) {
    counts <- as.integer(panel_strata[i, ])
    expect_true(max(counts) - min(counts) <= 1)
  }
})

test_that("multi-stage panels assigned at PSU level", {
  set.seed(123)
  frame <- data.frame(
    school_id = rep(1:40, each = 10),
    student_id = 1:400,
    enrollment = rep(sample(100:500, 40, replace = TRUE), each = 10),
    value = rnorm(400)
  )

  result <- sampling_design() |>
    add_stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 20) |>
    add_stage(label = "Students") |>
      draw(n = 5) |>
    execute(frame, seed = 42, panels = 4)

  expect_true(".panel" %in% names(result))

  # All students in a school share the same panel
  panel_by_school <- tapply(result$.panel, result$school_id, unique)
  for (panels_in_school in panel_by_school) {
    expect_length(panels_in_school, 1)
  }
})

test_that("panels = 1 errors", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = 1),
    "panels.*integer.*>= 2"
  )
})

test_that("panels = 0 errors", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = 0),
    "panels.*integer.*>= 2"
  )
})

test_that("non-integer panels errors", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = 2.5),
    "panels.*integer.*>= 2"
  )

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = "two"),
    "panels.*integer.*>= 2"
  )
})

test_that("negative panels errors", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = -3),
    "panels.*integer.*>= 2"
  )
})

test_that("panels are deterministic with same seed", {
  frame <- data.frame(id = 1:200, value = rnorm(200))

  r1 <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42, panels = 4)

  r2 <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42, panels = 4)

  expect_equal(r1$.panel, r2$.panel)
})

test_that("panels work with continuation (execute_continuation path)", {
  set.seed(123)
  frame <- data.frame(
    school_id = rep(1:40, each = 10),
    student_id = 1:400,
    enrollment = rep(sample(100:500, 40, replace = TRUE), each = 10),
    value = rnorm(400)
  )

  design <- sampling_design() |>
    add_stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 20) |>
    add_stage(label = "Students") |>
      draw(n = 5)

  s1 <- execute(design, frame, stages = 1, seed = 42)
  result <- execute(s1, frame, seed = 43, panels = 4)

  expect_true(".panel" %in% names(result))
  expect_equal(sort(unique(result$.panel)), 1:4)
})

test_that("panels does not alter weights", {
  frame <- data.frame(
    id = 1:200,
    region = rep(c("A", "B"), each = 100),
    value = rnorm(200)
  )

  without_panels <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  with_panels <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 100) |>
    execute(frame, seed = 42, panels = 4)

  expect_equal(with_panels$.weight, without_panels$.weight)
})
