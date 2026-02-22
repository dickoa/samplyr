test_that("serp returns correct order for 2 variables", {
  df <- data.frame(
    region = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    district = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
    id = 1:9
  )

  result <- df[order(serp(df$region, df$district)), ]

  # Region 1 (odd):  district ascending  -> 1, 2, 3
  # Region 2 (even): district descending -> 3, 2, 1
  # Region 3 (odd):  district ascending  -> 1, 2, 3
  expected_ids <- c(1, 2, 3, 6, 5, 4, 7, 8, 9)

  expect_equal(result$id, expected_ids)
})


test_that("serp returns correct order for 3 variables", {
  df <- expand.grid(A = 1:2, B = 1:3, C = 1:2)
  df$id <- 1:nrow(df)

  result <- df[order(serp(df$A, df$B, df$C)), ]

  # A=1 (odd): B ascending (1, 2, 3)
  #   A=1, B=1 (group 1, odd):  C ascending  (1, 2)
  #   A=1, B=2 (group 2, even): C descending (2, 1)
  #   A=1, B=3 (group 3, odd):  C ascending  (1, 2)
  # A=2 (even): B descending (3, 2, 1)
  #   A=2, B=3 (group 4, even): C descending (2, 1)
  #   A=2, B=2 (group 5, odd):  C ascending  (1, 2)
  #   A=2, B=1 (group 6, even): C descending (2, 1)
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

  # North (1st, odd):  district ascending  (A, B)
  # South (2nd, even): district descending (B, A)
  # West  (3rd, odd):  district ascending  (A, B)
  expected_ids <- c(1, 2, 4, 3, 5, 6)

  expect_equal(result$id, expected_ids)
})


test_that("serp works with single variable (just ascending)", {
  df <- data.frame(x = c(3, 1, 4, 1, 5), id = 1:5)

  result <- df[order(serp(df$x)), ]

  # Single variable: simple ascending sort
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

  # Region 1 (odd): district ascending, NA last -> 1, 2, NA

  # Region 2 (even): district descending, NA first -> NA, 2, 1
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
  expect_error(serp(), "At least one variable")
})


test_that("serp errors with mismatched lengths", {
  expect_error(
    serp(1:3, 1:5),
    "same length"
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

  # Simulating: arrange(df, group, serp(x, y), desc(z))
  result <- df[order(df$group, serp(df$x, df$y), -df$z), ]

  # Within each group:
  #   x=1 (odd):  y ascending  (1, 2, 3)
  #   x=2 (even): y descending (3, 2, 1)
  #   Within each (x, y), z descending (but each cell has only one row here)
  #
  # Group A data:
  #   id=1: x=1, y=1, z=10
  #   id=2: x=2, y=2, z=20
  #   id=3: x=1, y=3, z=30
  #   id=4: x=2, y=1, z=40
  #   id=5: x=1, y=2, z=50
  #   id=6: x=2, y=3, z=60
  #
  # Serpentine order within A:
  #   x=1, y=1: id=1
  #   x=1, y=2: id=5
  #   x=1, y=3: id=3
  #   x=2, y=3: id=6
  #   x=2, y=2: id=2
  #   x=2, y=1: id=4
  #
  # Same pattern for Group B: ids 7, 11, 9, 12, 8, 10

  expected_ids <- c(1, 5, 3, 6, 2, 4, 7, 11, 9, 12, 8, 10)

  expect_equal(result$id, expected_ids)
})


test_that("serp produces correct pattern for 4 variables", {
  df <- expand.grid(A = 1:2, B = 1:2, C = 1:2, D = 1:2)
  df$id <- 1:nrow(df)

  result <- df[order(serp(df$A, df$B, df$C, df$D)), ]

  # Verify the snake pattern holds at each level
  # A=1 (odd): B asc
  #   A=1,B=1 (g1, odd): C asc
  #     A=1,B=1,C=1 (g1, odd): D asc
  #     A=1,B=1,C=2 (g2, even): D desc
  #   A=1,B=2 (g2, even): C desc
  #     A=1,B=2,C=2 (g3, odd): D asc
  #     A=1,B=2,C=1 (g4, even): D desc
  # A=2 (even): B desc
  #   ... and so on

  # Just verify it runs and produces valid output
  expect_equal(nrow(result), 16)
  expect_equal(sort(result$id), 1:16)

  # Check first few: A=1, B=1, C=1, D=1 then D=2
  expect_equal(result$A[1:2], c(1, 1))
  expect_equal(result$B[1:2], c(1, 1))
  expect_equal(result$C[1:2], c(1, 1))
  expect_equal(result$D[1:2], c(1, 2)) # D ascending in group 1
})


test_that("serp matches SAS SURVEYSELECT SORT=SERP behavior", {
  # This test verifies the algorithm matches the SAS documentation description:
  # "In hierarchic serpentine sorting, PROC SURVEYSELECT sorts by the first
  # CONTROL variable in ascending order. Then within the first level of the

  # first CONTROL variable, the procedure sorts by the second CONTROL variable
  # in ascending order. Within the second level of the first CONTROL variable,
  # the procedure sorts by the second CONTROL variable in descending order."

  df <- data.frame(
    control1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
    control2 = c("a", "b", "c", "a", "b", "c", "a", "b", "c"),
    id = 1:9
  )

  result <- df[order(serp(df$control1, df$control2)), ]

  # Level 1 of control1: control2 ascending (a, b, c)
  # Level 2 of control1: control2 descending (c, b, a)
  # Level 3 of control1: control2 ascending (a, b, c)

  expect_equal(result$control1, c(1, 1, 1, 2, 2, 2, 3, 3, 3))
  expect_equal(result$control2, c("a", "b", "c", "c", "b", "a", "a", "b", "c"))
})


test_that("serp uses byte-order (radix) for character variable ranks", {
  # Byte/ASCII order: uppercase before lowercase (A=65 < B=66 < a=97 < b=98).
  # The fix (method = "radix" in sort()) ensures this holds regardless of
  # system locale, which may interleave cases (e.g. A, a, B, b).
  # Input deliberately scrambles position order to distinguish from position-based sort.
  df <- data.frame(
    region   = c("a", "B", "A", "b"),
    district = c(1L, 1L, 1L, 1L),
    id       = 1:4
  )
  result <- df[order(serp(df$region, df$district)), ]
  # Radix level order: A(rank 1), B(rank 2), a(rank 3), b(rank 4)
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

  # Should complete in under 1 second

  expect_lt(time["elapsed"], 1)
})
