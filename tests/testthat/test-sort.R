test_that("serp returns correct order for 2 variables", {
  df <- data.frame(
    region = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    district = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    id = 1:9
  )

  result <- df[order(serp(df$region, df$district)), ]

  # Odd regions take districts ascending, even regions descending.
  expected_ids <- c(1, 2, 3, 6, 5, 4, 7, 8, 9)

  expect_equal(result$id, expected_ids)
})


test_that("serp returns correct order for 3 variables", {
  df <- expand.grid(A = 1:2, B = 1:3, C = 1:2)
  df$id <- seq_len(nrow(df))

  result <- df[order(serp(df$A, df$B, df$C)), ]

  # Each level runs descending in the even-numbered cells of the level above.
  expected_ids <- c(1, 7, 9, 3, 5, 11, 12, 6, 4, 10, 8, 2)

  expect_equal(result$id, expected_ids)
})


test_that("serp works with character variables", {
  df <- data.frame(
    region = c("North", "North", "South", "South", "West", "West"),
    district = c("A", "B", "A", "B", "A", "B"),
    id = 1:6
  )

  result <- df[order(serp(df$region, df$district)), ]

  # South, the second region, takes its districts descending.
  expected_ids <- c(1, 2, 4, 3, 5, 6)

  expect_equal(result$id, expected_ids)
})


test_that("serp works with single variable (just ascending)", {
  df <- data.frame(x = c(3, 1, 4, 1, 5), id = 1:5)

  result <- df[order(serp(df$x)), ]

  expected_ids <- c(2, 4, 1, 3, 5)

  expect_equal(result$id, expected_ids)
})


test_that("serp handles NA values", {
  df <- data.frame(
    region = c(1, 1, 1, 2, 2, 2),
    district = c(1, NA, 2, 1, NA, 2),
    id = 1:6
  )

  result <- df[order(serp(df$region, df$district)), ]

  # NA sorts last in ascending region 1 and first in descending region 2.
  expected_ids <- c(1, 3, 2, 5, 6, 4)

  expect_equal(result$id, expected_ids)
})


test_that("serp returns empty numeric for empty input", {
  result <- serp(integer(0), integer(0))

  expect_equal(result, numeric(0))
})


test_that("serp returns 1 for single row", {
  result <- serp(1, 2, 3)

  expect_equal(result, 1)
})


test_that("serp errors with no variables", {
  expect_error(
    serp(),
    "At least one variable",
    class = "samplyr_error_serp_no_variables"
  )
})


test_that("serp errors with mismatched lengths", {
  expect_error(
    serp(1:3, 1:5),
    "same length",
    class = "samplyr_error_serp_incompatible_lengths"
  )
})


test_that("serp composes with other arrange arguments", {
  df <- data.frame(
    group = rep(c("A", "B"), each = 6),
    x = rep(1:2, times = 6),
    y = rep(1:3, times = 4),
    z = c(10, 20, 30, 40, 50, 60, 15, 25, 35, 45, 55, 65),
    id = 1:12
  )

  # The order arrange(df, group, serp(x, y), desc(z)) gives
  result <- df[order(df$group, serp(df$x, df$y), -df$z), ]

  # In each group x=1 takes y ascending and x=2 takes y descending.
  expected_ids <- c(1, 5, 3, 6, 2, 4, 7, 11, 9, 12, 8, 10)

  expect_equal(result$id, expected_ids)
})


test_that("serp produces correct pattern for 4 variables", {
  df <- expand.grid(A = 1:2, B = 1:2, C = 1:2, D = 1:2)
  df$id <- seq_len(nrow(df))

  result <- df[order(serp(df$A, df$B, df$C, df$D)), ]

  # At A=2 the snake carries on from where it stopped rather than restarting.
  expect_identical(
    paste0(result$A, result$B, result$C, result$D),
    c("1111", "1112", "1122", "1121", "1221", "1222", "1212", "1211",
      "2211", "2212", "2222", "2221", "2121", "2122", "2112", "2111")
  )
})

test_that("serp is a snake at every level, for even and odd cardinalities", {
  # Intermediate cardinalities decide where the direction reverses.
  layouts <- list(
    c(2, 2, 2), c(3, 2, 3), c(4, 2, 3), c(2, 4, 3), c(3, 4, 4),
    c(2, 3, 3), c(3, 3, 3), c(3, 5, 2),
    c(2, 2, 2, 2), c(3, 2, 2, 3), c(2, 4, 2, 3)
  )
  for (dims in layouts) {
    df <- do.call(expand.grid, rev(lapply(dims, seq_len)))
    df <- df[, rev(seq_along(dims)), drop = FALSE]
    names(df) <- paste0("v", seq_along(dims))
    m <- as.matrix(df[order(do.call(serp, as.list(df))), , drop = FALSE])
    expect_identical(
      unique(rowSums(abs(diff(m)))),
      1,
      info = paste(dims, collapse = "x")
    )
  }
})

test_that("serp snakes through a ragged hierarchy", {
  # Unequal numbers of children leave no fixed radix to carry.
  df <- do.call(rbind, lapply(1:4, function(a) {
    do.call(rbind, lapply(seq_len(c(2, 3, 2, 4)[a]), function(b) {
      data.frame(v1 = a, v2 = b, v3 = seq_len(c(3, 2, 4, 2, 3)[((a + b) %% 5) + 1]))
    }))
  }))
  ordered <- df[order(serp(df$v1, df$v2, df$v3)), ]
  within_cell <- split(ordered$v3, paste(ordered$v1, ordered$v2))
  for (v in within_cell) {
    expect_identical(abs(diff(v)), rep(1L, length(v) - 1L))
  }
})


test_that("serp matches SAS SURVEYSELECT SORT=SERP behavior", {
  # SAS sorts the second CONTROL variable descending in the first one's level 2.
  df <- data.frame(
    control1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    control2 = c("a", "b", "c", "a", "b", "c", "a", "b", "c"),
    id = 1:9
  )

  result <- df[order(serp(df$control1, df$control2)), ]

  expect_equal(result$control1, c(1, 1, 1, 2, 2, 2, 3, 3, 3))
  expect_equal(result$control2, c("a", "b", "c", "c", "b", "a", "a", "b", "c"))
})


test_that("serp uses byte-order (radix) for character variable ranks", {
  # Byte order ranks A, B, a, b in every locale, unlike the input positions.
  df <- data.frame(
    region   = c("a", "B", "A", "b"),
    district = c(1L, 1L, 1L, 1L),
    id       = 1:4
  )
  result <- df[order(serp(df$region, df$district)), ]
  expect_equal(result$id, c(3L, 2L, 1L, 4L))
})


test_that("serp is fast for large datasets", {
  skip_on_cran()

  set.seed(42)
  n <- 100000
  large_df <- data.frame(
    a = sample(1:10, n, replace = TRUE),
    b = sample(1:20, n, replace = TRUE),
    c = sample(1:50, n, replace = TRUE)
  )

  time <- system.time({
    key <- serp(large_df$a, large_df$b, large_df$c)
    result <- large_df[order(key), ]
  })

  expect_lt(time["elapsed"], 1)
})
