test_that("certainty_size selects large units with prob=1", {
  frame <- data.frame(
    id = 1:10,
    mos = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_systematic", mos = mos, certainty_size = 800) |>
    execute(frame, seed = 1)

  expect_equal(nrow(result), 5)
  expect_true(all(c(8, 9, 10) %in% result$id))

  certainty_rows <- result[result$id %in% c(8, 9, 10), ]
  expect_true(all(certainty_rows$.weight == 1))
  expect_true(all(certainty_rows$.certainty_1 == TRUE))

  prob_rows <- result[!result$id %in% c(8, 9, 10), ]
  expect_true(all(prob_rows$.certainty_1 == FALSE))
  expect_true(all(prob_rows$.weight > 1))
})

test_that("certainty_prop selects units above proportional threshold", {
  frame <- data.frame(
    id = 1:5,
    mos = c(10, 20, 30, 40, 900)
  )

  result <- sampling_design() |>
    draw(n = 3, method = "pps_systematic", mos = mos, certainty_prop = 0.50) |>
    execute(frame, seed = 2)

  expect_true(5 %in% result$id)

  unit5 <- result[result$id == 5, ]
  expect_equal(unit5$.weight, 1)
  expect_equal(unit5$.certainty_1, TRUE)
})

test_that("certainty_prop performs iterative selection", {
  frame <- data.frame(
    id = 1:5,
    mos = c(10, 20, 30, 40, 900)
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_systematic", mos = mos, certainty_prop = 0.30) |>
    execute(frame, seed = 3)

  expect_equal(nrow(result), 5)
  expect_true(all(result$.certainty_1 == TRUE))
  expect_true(all(result$.weight == 1))
})

test_that("certainty selection errors when exceeding sample size", {
  frame <- data.frame(
    id = 1:10,
    mos = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  )

  expect_error(
    sampling_design() |>
      draw(n = 2, method = "pps_systematic", mos = mos, certainty_size = 700) |>
      execute(frame, seed = 42),
    "exceeds target sample size"
  )
})

test_that("certainty selection works with stratification", {
  frame <- data.frame(
    id = 1:8,
    stratum = rep(c("A", "B"), each = 4),
    mos = c(100, 200, 300, 1000, 50, 100, 150, 800)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 4, method = "pps_systematic", mos = mos, certainty_size = 500) |>
    execute(frame, seed = 123)

  expect_true(all(c(4, 8) %in% result$id))

  certainty_rows <- result[result$id %in% c(4, 8), ]
  expect_true(all(certainty_rows$.certainty_1 == TRUE))
})

test_that("certainty selection works with different PPS WOR methods", {
  frame <- data.frame(
    id = 1:10,
    mos = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  )

  methods <- c(
    "pps_systematic",
    "pps_brewer",
    "pps_cps",
    "pps_sps",
    "pps_pareto"
  )

  for (m in methods) {
    result <- sampling_design() |>
      draw(n = 5, method = m, mos = mos, certainty_size = 900) |>
      execute(frame, seed = 2)

    expect_true(all(c(9, 10) %in% result$id), label = m)
    certainty_rows <- result[result$id %in% c(9, 10), ]
    expect_true(all(certainty_rows$.certainty_1 == TRUE), label = m)
  }
})

test_that("no certainty selection when threshold not met", {
  frame <- data.frame(
    id = 1:10,
    mos = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_systematic", mos = mos, certainty_size = 99999) |>
    execute(frame, seed = 1)

  expect_equal(nrow(result), 5)
  expect_true(all(result$.certainty_1 == FALSE))
})

test_that("draw errors when both certainty_size and certainty_prop specified", {
  expect_error(
    sampling_design() |>
      draw(
        n = 10,
        method = "pps_brewer",
        mos = size,
        certainty_size = 1000,
        certainty_prop = 0.1
      ),
    "Specify only one of"
  )
})

test_that("draw errors when certainty specified without mos", {
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_brewer", certainty_size = 1000),
    "require `mos`"
  )
})

test_that("draw errors when certainty specified with non-PPS method", {
  expect_warning(
    expect_error(
      sampling_design() |>
        draw(n = 10, method = "srswor", mos = size, certainty_size = 1000),
      "only available for PPS"
    ),
    "ignored for non-PPS"
  )

  expect_warning(
    expect_error(
      sampling_design() |>
        draw(n = 10, method = "systematic", mos = size, certainty_prop = 0.1),
      "only available for PPS"
    ),
    "ignored for non-PPS"
  )
})

test_that("draw errors for invalid certainty_size values", {
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_size = -100),
    "positive number"
  )

  expect_error(
    sampling_design() |>
      draw(
        n = 10,
        method = "pps_brewer",
        mos = size,
        certainty_size = c(100, 200)
      ),
    "single positive number"
  )
})

test_that("draw errors for invalid certainty_prop values", {
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_prop = 0),
    "between 0 and 1"
  )

  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_prop = 1),
    "between 0 and 1"
  )

  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_prop = 1.5),
    "between 0 and 1"
  )
})

test_that("certainty selection is reproducible with same seed", {
  frame <- data.frame(
    id = 1:20,
    mos = seq(100, 2000, by = 100)
  )

  result1 <- sampling_design() |>
    draw(n = 8, method = "pps_systematic", mos = mos, certainty_size = 1500) |>
    execute(frame, seed = 12345)

  result2 <- sampling_design() |>
    draw(n = 8, method = "pps_systematic", mos = mos, certainty_size = 1500) |>
    execute(frame, seed = 12345)

  expect_equal(result1$id, result2$id)
  expect_equal(result1$.certainty_1, result2$.certainty_1)
})

test_that("certainty units have weight = 1/prob = 1", {
  frame <- data.frame(
    id = 1:10,
    mos = c(rep(100, 7), 2000, 3000, 4000)
  )

  result <- sampling_design() |>
    draw(n = 6, method = "pps_brewer", mos = mos, certainty_size = 1500) |>
    execute(frame, seed = 123)

  certainty_rows <- result[result$.certainty_1 == TRUE, ]
  expect_true(all(certainty_rows$.weight == 1))
  expect_true(all(certainty_rows$.weight == 1))

  prob_rows <- result[result$.certainty_1 == FALSE, ]
  expect_true(all(prob_rows$.weight > 0))
})

test_that("all units selected with certainty when threshold is low", {
  frame <- data.frame(
    id = 1:5,
    mos = c(100, 200, 300, 400, 500)
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_systematic", mos = mos, certainty_size = 50) |>
    execute(frame, seed = 24)

  expect_equal(nrow(result), 5)
  expect_true(all(result$.certainty_1 == TRUE))
  expect_true(all(result$.weight == 1))
})

test_that("certainty_prop with uniform MOS selects no units", {
  frame <- data.frame(
    id = 1:10,
    mos = rep(100, 10)
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_systematic", mos = mos, certainty_prop = 0.20) |>
    execute(frame, seed = 40)

  expect_equal(nrow(result), 5)
  expect_true(all(result$.certainty_1 == FALSE))
})

test_that("certainty selection preserves sample size", {
  frame <- data.frame(
    id = 1:20,
    mos = seq(100, 2000, by = 100)
  )

  for (threshold in c(500, 1000, 1500)) {
    n_certain <- sum(frame$mos >= threshold)
    if (n_certain <= 10) {
      result <- sampling_design() |>
        draw(
          n = 10,
          method = "pps_systematic",
          mos = mos,
          certainty_size = threshold
        ) |>
        execute(frame, seed = 2026)

      expect_equal(nrow(result), 10, label = paste("threshold =", threshold))
    }
  }
})

# Data frame certainty threshold tests

test_that("certainty_size as data frame applies stratum-specific thresholds", {
  frame <- data.frame(
    id = 1:12,
    stratum = rep(c("A", "B", "C"), each = 4),
    mos = c(
      100,
      200,
      500,
      600, # A: 500, 600 >= 400
      100,
      200,
      300,
      400, # B: none >= 800
      100,
      800,
      900,
      1000
    ) # C: 800, 900, 1000 >= 600
  )

  cert_df <- data.frame(
    stratum = c("A", "B", "C"),
    certainty_size = c(400, 800, 600)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = cert_df) |>
    execute(frame, seed = 1)

  expect_equal(nrow(result), 9)

  result_A <- result[result$stratum == "A", ]
  expect_true(all(c(3, 4) %in% result_A$id))
  expect_true(all(result_A[result_A$id %in% c(3, 4), ]$.certainty_1 == TRUE))

  result_B <- result[result$stratum == "B", ]
  expect_true(all(result_B$.certainty_1 == FALSE))

  result_C <- result[result$stratum == "C", ]
  expect_true(all(c(10, 11, 12) %in% result_C$id))
  expect_true(all(
    result_C[result_C$id %in% c(10, 11, 12), ]$.certainty_1 == TRUE
  ))
})

test_that("certainty_prop as data frame applies stratum-specific thresholds", {
  frame <- data.frame(
    id = 1:8,
    stratum = rep(c("A", "B"), each = 4),
    mos = c(
      10,
      20,
      30,
      940, # A: total=1000, unit 4 = 94%
      100,
      100,
      100,
      100
    ) # B: all equal at 25%
  )

  cert_df <- data.frame(
    stratum = c("A", "B"),
    certainty_prop = c(0.50, 0.30)
  )

  expect_warning(
    result <- sampling_design() |>
      stratify_by(stratum) |>
      draw(
        n = 5,
        method = "pps_systematic",
        mos = mos,
        certainty_prop = cert_df
      ) |>
      execute(frame, seed = 202602),
    "capped to population"
  )

  result_A <- result[result$stratum == "A", ]
  expect_true(4 %in% result_A$id)
  expect_true(result_A[result_A$id == 4, ]$.certainty_1 == TRUE)

  result_B <- result[result$stratum == "B", ]
  expect_true(all(result_B$.certainty_1 == FALSE))
})

test_that("certainty_size data frame errors without stratification", {
  cert_df <- data.frame(
    stratum = c("A", "B"),
    certainty_size = c(500, 800)
  )

  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_size = cert_df),
    "requires stratification"
  )
})

test_that("certainty_prop data frame errors without stratification", {
  cert_df <- data.frame(
    stratum = c("A", "B"),
    certainty_prop = c(0.1, 0.2)
  )

  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_prop = cert_df),
    "requires stratification"
  )
})

test_that("certainty_size data frame errors with missing strata column", {
  cert_df <- data.frame(
    wrong_col = c("A", "B"),
    certainty_size = c(500, 800)
  )

  expect_error(
    sampling_design() |>
      stratify_by(stratum) |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_size = cert_df),
    "missing stratification"
  )
})

test_that("certainty_size data frame errors with missing certainty_size column", {
  cert_df <- data.frame(
    stratum = c("A", "B"),
    wrong_col = c(500, 800)
  )

  expect_error(
    sampling_design() |>
      stratify_by(stratum) |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_size = cert_df),
    "must contain"
  )
})

test_that("certainty_size data frame errors with invalid values", {
  cert_df <- data.frame(
    stratum = c("A", "B"),
    certainty_size = c(-100, 800)
  )

  expect_error(
    sampling_design() |>
      stratify_by(stratum) |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_size = cert_df),
    "positive numbers"
  )
})

test_that("certainty_prop data frame errors with invalid values", {
  cert_df <- data.frame(
    stratum = c("A", "B"),
    certainty_prop = c(0.1, 1.5)
  )

  expect_error(
    sampling_design() |>
      stratify_by(stratum) |>
      draw(n = 10, method = "pps_brewer", mos = size, certainty_prop = cert_df),
    "between 0 and 1"
  )
})

test_that("certainty data frame with multi-variable stratification works", {
  # fmt: skip
  frame <- data.frame(
    id = 1:16,
    region = rep(c("North", "South"), each = 8),
    urban = rep(c("Urban", "Rural"), times = 8),
    mos = c(
      100,
      200,
      800,
      900,
      100,
      200,
      300,
      400,
      100,
      200,
      700,
      750,
      100,
      200,
      300,
      400
    )
  )

  cert_df <- data.frame(
    region = c("North", "North", "South", "South"),
    urban = c("Urban", "Rural", "Urban", "Rural"),
    certainty_size = c(700, 350, 600, 350)
  )

  result <- sampling_design() |>
    stratify_by(region, urban) |>
    draw(n = 2, method = "pps_brewer", mos = mos, certainty_size = cert_df) |>
    execute(frame, seed = 20260205)

  expect_equal(nrow(result), 8)

  north <- result[result$region == "North", ]
  expect_true(all(c(3, 4) %in% north$id))
  expect_true(all(
    north[north$id %in% c(3, 4), ]$.certainty_1 == TRUE
  ))
})

test_that("certainty_overflow = 'allow' returns all certainty units when n_cert > n", {
  frame <- data.frame(
    id = 1:10,
    mos = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  )

  result <- sampling_design() |>
    draw(
      n = 2,
      method = "pps_systematic",
      mos = mos,
      certainty_size = 700,
      certainty_overflow = "allow"
    ) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 4)
  expect_true(all(c(7, 8, 9, 10) %in% result$id))
  expect_true(all(result$.weight == 1))
  expect_true(all(result$.certainty_1 == TRUE))
})

test_that("certainty_overflow = 'allow' works with certainty_prop cascade", {
  frame <- data.frame(
    id = 1:5,
    mos = c(10, 20, 30, 40, 900)
  )

  result <- sampling_design() |>
    draw(
      n = 1,
      method = "pps_systematic",
      mos = mos,
      certainty_prop = 0.30,
      certainty_overflow = "allow"
    ) |>
    execute(frame, seed = 1)

  expect_true(nrow(result) >= 1)
  expect_true(all(result$.weight == 1))
  expect_true(all(result$.certainty_1 == TRUE))
})

test_that("certainty_overflow = 'allow' with stratification", {
  frame <- data.frame(
    id = 1:8,
    stratum = rep(c("A", "B"), each = 4),
    mos = c(100, 200, 300, 1000, 50, 100, 150, 800)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(
      n = 1,
      method = "pps_systematic",
      mos = mos,
      certainty_size = 50,
      certainty_overflow = "allow"
    ) |>
    execute(frame, seed = 1)

  expect_true(nrow(result) >= 2)
  expect_true(all(result$.weight == 1))
  expect_true(all(result$.certainty_1 == TRUE))
})

test_that("certainty_overflow default is 'error' (backward compatible)", {
  frame <- data.frame(
    id = 1:10,
    mos = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  )

  expect_error(
    sampling_design() |>
      draw(n = 2, method = "pps_systematic", mos = mos, certainty_size = 700) |>
      execute(frame, seed = 42),
    "exceeds target sample size"
  )
})

test_that("certainty_overflow validates input", {
  expect_error(
    sampling_design() |>
      draw(
        n = 10,
        method = "pps_brewer",
        mos = size,
        certainty_size = 500,
        certainty_overflow = "invalid"
      ),
    "should be one of"
  )
})

test_that("certainty_overflow = 'allow' returns exact certainty count when all are certain", {
  frame <- data.frame(
    id = 1:5,
    mos = c(100, 200, 300, 400, 500)
  )

  result <- sampling_design() |>
    draw(
      n = 2,
      method = "pps_systematic",
      mos = mos,
      certainty_size = 50,
      certainty_overflow = "allow"
    ) |>
    execute(frame, seed = 1)

  expect_equal(nrow(result), 5)
  expect_true(all(result$.certainty_1 == TRUE))
  expect_true(all(result$.weight == 1))
})

test_that("certainty_overflow error message mentions allow option", {
  frame <- data.frame(
    id = 1:10,
    mos = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  )

  expect_error(
    sampling_design() |>
      draw(n = 2, method = "pps_systematic", mos = mos, certainty_size = 700) |>
      execute(frame, seed = 42),
    "certainty_overflow"
  )
})

test_that("missing stratum in certainty data frame uses no threshold", {
  frame <- data.frame(
    id = 1:12,
    stratum = rep(c("A", "B", "C"), each = 4),
    mos = c(100, 200, 500, 600, 100, 200, 500, 600, 100, 200, 500, 600)
  )

  cert_df <- data.frame(
    stratum = c("A", "B"),
    certainty_size = c(400, 400)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = cert_df) |>
    execute(frame, seed = 1)

  result_A <- result[result$stratum == "A", ]
  expect_true(any(result_A$.certainty_1 == TRUE))

  result_C <- result[result$stratum == "C", ]
  expect_true(all(result_C$.certainty_1 == FALSE))
})
