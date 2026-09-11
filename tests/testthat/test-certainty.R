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
    draw(n = c(A = 3, B = 3, C = 4), method = "pps_brewer", mos = mos, certainty_size = cert_df) |>
    execute(frame, seed = 1)

  expect_equal(nrow(result), 10)

  result_A <- result[result$stratum == "A", ]
  expect_true(all(c(3, 4) %in% result_A$id))
  expect_true(all(result_A[result_A$id %in% c(3, 4), ]$.certainty_1 == TRUE))

  # No unit in B reaches the 800 threshold, but inclusion_prob() caps the
  # two largest at probability one, which is certainty all the same.
  result_B <- result[result$stratum == "B", ]
  expect_setequal(result_B$id[result_B$.certainty_1], c(7, 8))

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
    class = "samplyr_warning_census"
  )

  result_A <- result[result$stratum == "A", ]
  expect_true(4 %in% result_A$id)
  expect_true(result_A[result_A$id == 4, ]$.certainty_1 == TRUE)

  # B is taken whole, so every unit resolves to probability one. The 30%
  # threshold never binds; the census does.
  result_B <- result[result$stratum == "B", ]
  expect_true(all(result_B$.certainty_1))
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
    draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = cert_df) |>
    execute(frame, seed = 20260205)

  expect_equal(nrow(result), 12)

  north <- result[result$region == "North", ]
  expect_true(all(c(3, 4) %in% north$id))
  expect_true(all(
    north[north$id %in% c(3, 4), ]$.certainty_1 == TRUE
  ))
})

test_that("certainty_overflow cannot exclude the remaining population", {
  frame <- data.frame(id = 1:10, mos = seq(100, 1000, 100))
  design <- sampling_design() |> draw(n = 2, method = "pps_systematic",
    mos = mos, certainty_size = 700, certainty_overflow = "allow")
  expect_error(execute(design, frame, seed = 42),
    class = "samplyr_error_certainty_zero_probability")
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

test_that("certainty_overflow defaults to refusing an above-target census", {
  frame <- data.frame(
    id = 1:10,
    mos = c(100, 200, 300, 400, 500, 600, 700, 800, 900, 1000)
  )

  expect_error(
    sampling_design() |>
      draw(n = 2, method = "pps_systematic", mos = mos, certainty_size = 50) |>
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
      draw(n = 2, method = "pps_systematic", mos = mos, certainty_size = 50) |>
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

  # C gets no threshold, but its two largest units still cap at probability
  # one, so certainty here comes from the probability calculation.
  result_C <- result[result$stratum == "C", ]
  expect_setequal(result_C$id[result_C$.certainty_1], c(11, 12))
})

# Resolved certainty: .certainty_k records an inclusion probability of one
# however it arose, not membership of an explicit certainty rule. Frames
# below are fully deterministic so the resolved probabilities are exact.

# Three dominant units cap at one under n = 10; the remaining 57 share the
# residual take at 0.1228 each.
dominant_frame <- function() {
  data.frame(id = seq_len(60), mos = c(500, 400, 300, rep(10, 57)))
}

test_that("automatic capping sets certainty for every fixed-size PPS-WOR method", {
  frame <- dominant_frame()

  for (method in c(
    "pps_brewer",
    "pps_systematic",
    "pps_cps",
    "pps_sampford",
    "pps_sps",
    "pps_pareto"
  )) {
    result <- sampling_design() |>
      draw(n = 10, method = method, mos = mos) |>
      execute(frame, seed = 2)

    expect_setequal(result$id[result$.certainty_1], c(1, 2, 3))
    expect_equal(
      result$.certainty_1,
      is_certainty_probability(1 / result$.weight),
      info = method
    )
  }
})

test_that("a certainty rule's remainder can create further certainty units", {
  # Only unit 1 clears the 900 threshold. Removing it shrinks the frame
  # total, so inclusion_prob() then caps units 2 and 3 in the remainder.
  frame <- data.frame(id = seq_len(53), mos = c(1000, 500, 400, rep(10, 50)))

  result <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos, certainty_size = 900) |>
    execute(frame, seed = 1)

  expect_setequal(result$id[result$.certainty_1], c(1, 2, 3))
  expect_true(all(result$.weight[result$.certainty_1] == 1))
})

test_that("cube certainty is resolved from the inclusion probabilities", {
  frame <- dominant_frame()
  frame$x <- rep(c(1, 2), length.out = 60)

  result <- sampling_design() |>
    draw(n = 10, method = "cube", mos = mos, aux = c(x)) |>
    execute(frame, seed = 3)

  expect_true(".certainty_1" %in% names(result))
  expect_setequal(result$id[result$.certainty_1], c(1, 2, 3))
})

test_that("custom balanced and custom WOR methods resolve certainty", {
  on.exit(
    {
      sondage::unregister_method("cert_wor")
      sondage::unregister_method("cert_bal")
    },
    add = TRUE
  )
  top_n_fn <- function(pik, n = NULL, prn = NULL, ...) {
    order(pik, decreasing = TRUE)[seq_len(n)]
  }
  sondage::register_method(
    "cert_wor", "wor", sample_fn = top_n_fn, probabilities = "exact"
  )
  sondage::register_method(
    "cert_bal", "balanced", sample_fn = top_n_fn, probabilities = "exact"
  )

  frame <- dominant_frame()

  for (method in c("pps_cert_wor", "balanced_cert_bal")) {
    result <- sampling_design() |>
      draw(n = 10, method = method, mos = mos) |>
      execute(frame, seed = 4)

    expect_setequal(result$id[result$.certainty_1], c(1, 2, 3))
  }
})

test_that("expected hits of one or more are never certainty", {
  frame <- dominant_frame()

  for (method in c("pps_multinomial", "pps_chromy")) {
    result <- sampling_design() |>
      draw(n = 10, method = method, mos = mos) |>
      execute(frame, seed = 5)

    expect_gt(max(1 / result$.weight), 1)
    expect_false(any(result$.certainty_1))
  }
})

test_that("certainty propagates per stage in stratified and clustered designs", {
  frame <- data.frame(
    id = seq_len(80),
    stratum = rep(c("A", "B"), each = 40),
    mos = rep(c(500, 400, 300, rep(10, 37)), times = 2)
  )

  strat <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 8, method = "pps_brewer", mos = mos) |>
    execute(frame, seed = 6)

  # Each stratum caps its own three dominant units.
  expect_equal(sum(strat$.certainty_1), 6L)
  expect_equal(
    as.vector(tapply(strat$.certainty_1, strat$stratum, sum)),
    c(3L, 3L)
  )

  clustered <- data.frame(
    psu = rep(seq_len(20), each = 5),
    id = seq_len(100),
    mos = rep(c(500, 400, 300, rep(10, 17)), each = 5)
  )

  multi <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 6, method = "pps_brewer", mos = mos) |>
    add_stage() |>
    draw(n = 2) |>
    execute(clustered, seed = 7)

  expect_setequal(unique(multi$psu[multi$.certainty_1]), c(1, 2, 3))
  # Stage 2 is equal-probability, so it carries no certainty column.
  expect_false(".certainty_2" %in% names(multi))
})

test_that("equal-probability stages carry no certainty column", {
  frame <- dominant_frame()

  for (design in list(
    sampling_design() |> draw(n = 10),
    sampling_design() |> draw(n = 10, method = "systematic"),
    sampling_design() |> draw(frac = 0.2, method = "bernoulli"),
    sampling_design() |> draw(n = 60)
  )) {
    result <- execute(design, frame, seed = 8)
    expect_false(".certainty_1" %in% names(result))
  }
})

test_that("sample certainty flags agree with the frame digest", {
  frame <- dominant_frame()

  result <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(frame, seed = 9, frame_digest = "full")

  units <- frame_summary(result, detail = "unit")
  selected <- units[units$is_selected, ]

  expect_identical(
    as.logical(selected$is_certainty),
    as.logical(result$.certainty_1)
  )
  expect_identical(
    as.logical(units$is_certainty),
    is_certainty_probability(units$chance)
  )
})

test_that("stratified cube resolves certainty and exports a take-all stratum", {
  skip_if_not_installed("survey")

  # The stratified cube returns through draw_balanced_stratified() rather
  # than draw_sample(), so it needs its own certainty assignment. Without
  # one the design carried no .certainty_k at all and the export could not
  # build the take-all stratum its PPS-WOR treatment requires.
  frame <- data.frame(
    id = seq_len(80),
    h = rep(c("A", "B"), each = 40),
    mos = rep(c(500, 400, 300, rep(10, 37)), times = 2),
    x = rep(c(1, 2), length.out = 80)
  )
  frame$y <- frame$mos * 2 + (frame$id %% 5)

  result <- sampling_design() |>
    stratify_by(h) |>
    draw(n = 10, method = "cube", mos = mos, aux = c(x)) |>
    execute(frame, seed = 1, frame_digest = "full")

  expect_true(".certainty_1" %in% names(result))
  # Each stratum caps its own three dominant units.
  expect_equal(sum(result$.certainty_1), 6L)
  expect_setequal(result$mos[result$.certainty_1], c(500, 400, 300))

  units <- frame_summary(result, detail = "unit")
  selected <- units[units$is_selected, ]
  expect_identical(
    as.logical(selected$is_certainty),
    as.logical(result$.certainty_1)
  )

  expect_true(grepl(
    "cert_stratum|strata_1",
    deparse(as_svydesign(result)$call$strata)
  ))
})

test_that("every unequal-probability stage carries a consistent certainty column", {
  # Guards the gap that stratified cube fell into: a selection path that
  # builds its own result must not silently omit the column. Asserted as an
  # invariant rather than relying on a downstream weight-based fallback,
  # which would mask the omission instead of surfacing it.
  frame <- dominant_frame()
  frame$x <- rep(c(1, 2), length.out = nrow(frame))

  designs <- list(
    pps_brewer = sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = mos),
    pps_poisson = sampling_design() |>
      draw(n = 10, method = "pps_poisson", mos = mos),
    cube = sampling_design() |>
      draw(n = 10, method = "cube", mos = mos, aux = c(x)),
    cube_stratified = sampling_design() |>
      stratify_by(x) |>
      draw(n = 5, method = "cube", mos = mos, aux = c(id)),
    pps_certainty = sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = mos, certainty_size = 250)
  )

  for (name in names(designs)) {
    # `dominant_frame()` saturates by construction, so the Poisson design
    # reports a shortfall. Not what this test is about.
    result <- suppressWarnings(execute(designs[[name]], frame, seed = 3))
    expect_true(".certainty_1" %in% names(result), info = name)
    expect_identical(
      result$.certainty_1,
      is_certainty_probability(1 / result$.weight_1),
      info = name
    )
  }
})

test_that("the certainty tolerance is an exactness test", {
  # Producers assign probability one rather than converging on it, so the
  # tolerance only has to absorb a few eps. It must not swallow a design
  # probability that is legitimately just below one, because dropping that
  # unit's variance contribution would understate the standard error.
  expect_true(is_certainty_probability(1))
  expect_true(is_certainty_probability(1 - 10 * .Machine$double.eps))
  expect_false(is_certainty_probability(1 - sqrt(.Machine$double.eps)))
  expect_false(is_certainty_probability(1 - 1e-8))
  expect_false(is_certainty_probability(0.999))
  expect_false(is_certainty_probability(NA_real_))
  expect_false(is_certainty_probability(NaN))
  expect_false(is_certainty_probability(Inf))
})
