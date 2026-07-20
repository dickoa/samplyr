test_that("compound strata containing key separators remain distinct", {
  frame <- data.frame(
    id = seq_len(100),
    s1 = c(rep("a\x01b", 10), rep("a", 90)),
    s2 = c(rep("c", 10), rep("b\x01c", 90)),
    stringsAsFactors = FALSE
  )

  result <- sampling_design() |>
    stratify_by(s1, s2, alloc = "proportional") |>
    draw(n = 10) |>
    execute(frame, seed = 1, frame_digest = "none")

  expect_equal(nrow(result), 10L)
  expect_equal(sum(result$s1 == "a\x01b"), 1L)
  expect_equal(sum(result$s1 == "a"), 9L)
})

test_that("canonical group keys distinguish separators and missing values", {
  keys <- samplyr:::make_group_key(
    data.frame(
      a = c("a\x01b", "a", NA_character_, "NA"),
      b = c("c", "b\x01c", "", ""),
      stringsAsFactors = FALSE
    ),
    c("a", "b")
  )

  expect_length(unique(keys), 4L)
})

test_that("survey export preserves colliding compound cluster values", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    part1 = rep(c("a.b", "a"), each = 4),
    part2 = rep(c("c", "b.c"), each = 4),
    id = seq_len(8),
    y = seq_len(8)
  )
  sample <- sampling_design() |>
    cluster_by(part1, part2) |>
    draw(n = 2) |>
    execute(frame, seed = 1)

  svy <- as_svydesign(sample)

  expect_equal(length(unique(svy$cluster[[1L]])), 2L)
})

test_that("survey export preserves colliding compound strata values", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    part1 = rep(c("a.b", "a"), each = 5),
    part2 = rep(c("c", "b.c"), each = 5),
    id = seq_len(10),
    y = seq_len(10)
  )
  sample <- sampling_design() |>
    stratify_by(part1, part2) |>
    draw(frac = 0.4) |>
    execute(frame, seed = 1)

  svy <- as_svydesign(sample)

  expect_equal(length(unique(svy$strata[[1L]])), 2L)
})

test_that("survey export supports non-syntactic design column names", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    "stratum name" = rep(c("north", "south"), each = 12),
    "psu id" = rep(paste0("psu ", seq_len(6)), each = 4),
    "unit id" = seq_len(24),
    y = seq_len(24),
    check.names = FALSE
  )
  sample <- sampling_design() |>
    stratify_by(`stratum name`) |>
    cluster_by(`psu id`) |>
    draw(n = 2) |>
    execute(frame, seed = 2)

  svy <- as_svydesign(sample)

  expect_equal(all.vars(svy$call$ids), "psu id")
  expect_equal(all.vars(svy$call$strata), "stratum name")
})

test_that("execute rejects generated column name collisions before sampling", {
  design <- sampling_design() |> draw(n = 2)
  reserved <- c(
    ".weight", ".fpc", ".pik", ".sample_id", ".stage", ".panel",
    ".replicate", ".draw", ".certainty", ".weight_1", ".fpc_1",
    ".draw_1", ".certainty_1"
  )

  for (name in reserved) {
    frame <- data.frame(id = seq_len(10))
    frame[[name]] <- seq_len(10)
    set.seed(27)
    before <- .Random.seed

    expect_error(
      execute(design, frame, seed = 1),
      class = "samplyr_error_frame_reserved_names"
    )
    expect_identical(.Random.seed, before)
  }
})

test_that("execute rejects duplicate frame names", {
  frame <- data.frame(
    first = seq_len(10),
    middle = seq_len(10),
    first = seq_len(10),
    last = seq_len(10),
    last = seq_len(10),
    check.names = FALSE
  )
  design <- sampling_design() |> draw(n = 2)

  error <- expect_error(
    execute(design, frame, seed = 1),
    class = "samplyr_error_frame_duplicate_names"
  )
  message <- conditionMessage(error)
  count_matches <- function(pattern) {
    length(regmatches(message, gregexpr(pattern, message, fixed = TRUE))[[1]])
  }
  expect_identical(count_matches("first"), 1L)
  expect_identical(count_matches("last"), 1L)
  expect_identical(count_matches("middle"), 0L)
})

test_that("execute validates seed range and replicate overflow", {
  frame <- data.frame(id = seq_len(10))
  design <- sampling_design() |> draw(n = 2)

  expect_error(
    execute(design, frame, seed = .Machine$integer.max + 1),
    class = "samplyr_error_seed_range"
  )
  expect_error(
    execute(design, frame, seed = -.Machine$integer.max - 1),
    class = "samplyr_error_seed_range"
  )
  expect_error(
    execute(
      design,
      frame,
      seed = .Machine$integer.max,
      reps = 2
    ),
    class = "samplyr_error_seed_overflow"
  )

  expect_no_error(
    execute(
      design,
      frame,
      seed = .Machine$integer.max,
      frame_digest = "none"
    )
  )
})
