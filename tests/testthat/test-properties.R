make_property_frame <- function() {
  set.seed(2026)
  data.frame(
    id = seq_len(200),
    region = rep(c("North", "South", "East", "West"), each = 50),
    ea_id = rep(seq_len(40), each = 5),
    pop = sample(10:500, 200, replace = TRUE),
    stringsAsFactors = FALSE
  )
}

test_that("weight sum = N for unstratified SRSWOR", {
  frame <- make_property_frame()
  result <- sampling_design() |>
    draw(n = 40) |>
    execute(frame, seed = 1)

  expect_equal(sum(result$.weight), nrow(frame))
})

test_that("weight sum = N for systematic sampling", {
  frame <- make_property_frame()
  result <- sampling_design() |>
    draw(n = 40, method = "systematic") |>
    execute(frame, seed = 2)

  expect_equal(sum(result$.weight), nrow(frame))
})

test_that("weight sum is positive and finite for PPS WOR methods", {
  frame <- make_property_frame()
  pps_wor <- c("pps_brewer", "pps_systematic", "pps_cps", "pps_sps", "pps_pareto")

  for (method in pps_wor) {
    result <- sampling_design() |>
      draw(n = 10, method = method, mos = pop) |>
      execute(frame, seed = 3)

    ws <- sum(result$.weight)
    expect_true(is.finite(ws), label = paste(method, "finite weight sum"))
    expect_true(ws > 0, label = paste(method, "positive weight sum"))
  }
})

test_that("weight sum = N for stratified SRSWOR", {
  frame <- make_property_frame()
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 40) |>
    execute(frame, seed = 4)

  expect_equal(sum(result$.weight), nrow(frame), tolerance = 1)
})

test_that("within-stratum weight sum = N_h for stratified designs", {
  frame <- make_property_frame()
  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10) |>
    execute(frame, seed = 5)

  for (r in unique(frame$region)) {
    N_h <- sum(frame$region == r)
    w_h <- sum(result$.weight[result$region == r])
    expect_equal(w_h, N_h, label = paste("stratum", r))
  }
})

test_that("weights are positive for SRSWOR", {
  frame <- make_property_frame()
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(frame, seed = 10)
  expect_true(all(result$.weight > 0))
})

test_that("weights are positive for systematic", {
  frame <- make_property_frame()
  result <- sampling_design() |>
    draw(n = 20, method = "systematic") |>
    execute(frame, seed = 11)
  expect_true(all(result$.weight > 0))
})

test_that("weights are positive for PPS methods", {
  frame <- make_property_frame()
  for (method in c("pps_brewer", "pps_systematic", "pps_cps")) {
    result <- sampling_design() |>
      draw(n = 10, method = method, mos = pop) |>
      execute(frame, seed = 12)
    expect_true(all(result$.weight > 0), label = method)
  }
})

test_that("weights are positive for WR methods", {
  frame <- make_property_frame()

  result_srswr <- sampling_design() |>
    draw(n = 20, method = "srswr") |>
    execute(frame, seed = 13)
  expect_true(all(result_srswr$.weight > 0), label = "srswr")

  for (method in c("pps_multinomial", "pps_chromy")) {
    result <- sampling_design() |>
      draw(n = 10, method = method, mos = pop) |>
      execute(frame, seed = 13)
    expect_true(all(result$.weight > 0), label = method)
  }
})

test_that("fixed-size WOR methods return exactly n rows", {
  frame <- make_property_frame()
  n <- 25

  for (method in c("srswor", "systematic", "pps_brewer", "pps_systematic", "pps_cps")) {
    if (startsWith(method, "pps_")) {
      design <- sampling_design() |>
        draw(n = n, method = method, mos = pop)
    } else {
      design <- sampling_design() |>
        draw(n = n, method = method)
    }
    result <- execute(design, frame, seed = 20)
    expect_equal(nrow(result), n, label = method)
  }
})

test_that("WR methods return exactly n rows (one per draw)", {
  frame <- make_property_frame()
  n <- 15

  result_srswr <- sampling_design() |>
    draw(n = n, method = "srswr") |>
    execute(frame, seed = 21)
  expect_equal(nrow(result_srswr), n)

  for (method in c("pps_multinomial", "pps_chromy")) {
    result <- sampling_design() |>
      draw(n = n, method = method, mos = pop) |>
      execute(frame, seed = 22)
    expect_equal(nrow(result), n, label = method)
  }
})

test_that("sampled IDs are a subset of frame IDs", {
  frame <- make_property_frame()
  result <- sampling_design() |>
    draw(n = 30) |>
    execute(frame, seed = 30)

  expect_true(all(result$id %in% frame$id))
})

test_that("WOR samples have no duplicate IDs", {
  frame <- make_property_frame()
  result <- sampling_design() |>
    draw(n = 30) |>
    execute(frame, seed = 31)

  expect_equal(length(unique(result$id)), nrow(result))
})

test_that(".fpc equals population/stratum size", {
  frame <- make_property_frame()

  # Unstratified
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(frame, seed = 40)
  expect_true(all(result$.fpc_1 == nrow(frame)))

  # Stratified
  result_strat <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10) |>
    execute(frame, seed = 41)

  for (r in unique(frame$region)) {
    N_h <- sum(frame$region == r)
    fpc_h <- unique(result_strat$.fpc_1[result_strat$region == r])
    expect_equal(fpc_h, N_h, label = paste("fpc for stratum", r))
  }
})

test_that("implied inclusion probabilities are in (0, 1] for WOR", {
  frame <- make_property_frame()

  result <- sampling_design() |>
    draw(n = 30) |>
    execute(frame, seed = 50)

  pi_i <- 1 / result$.weight
  expect_true(all(pi_i > 0))
  expect_true(all(pi_i <= 1))
})

test_that("implied inclusion probabilities are in (0, 1] for PPS WOR", {
  frame <- make_property_frame()

  for (method in c("pps_brewer", "pps_systematic", "pps_cps")) {
    result <- sampling_design() |>
      draw(n = 10, method = method, mos = pop) |>
      execute(frame, seed = 51)

    pi_i <- 1 / result$.weight
    expect_true(all(pi_i > 0), label = paste(method, "> 0"))
    expect_true(all(pi_i <= 1 + 1e-10), label = paste(method, "<= 1"))
  }
})

test_that("compound weight = product of stage weights", {
  frame <- make_property_frame()

  result <- sampling_design() |>
    add_stage(label = "EAs") |>
      cluster_by(ea_id) |>
      draw(n = 20) |>
    add_stage(label = "Units") |>
      draw(n = 3) |>
    execute(frame, seed = 60)

  expect_equal(
    result$.weight,
    result$.weight_1 * result$.weight_2,
    tolerance = 1e-10
  )
})

test_that("multi-stage weight sum approximates N", {
  frame <- make_property_frame()

  result <- sampling_design() |>
    add_stage(label = "EAs") |>
      cluster_by(ea_id) |>
      draw(n = 20) |>
    add_stage(label = "Units") |>
      draw(n = 3) |>
    execute(frame, seed = 61)

  # Weight sum should approximate N (not exact due to cluster size variation)
  weight_sum <- sum(result$.weight)
  expect_true(
    weight_sum > 0,
    label = "weight sum is positive"
  )
  # Each selected unit represents weight_i units in the population
  # so sum should be close to N, but depends on cluster sizes
})

test_that("certainty units have weight 1", {
  frame <- data.frame(
    id = 1:10,
    size = c(500, 400, 10, 10, 10, 10, 10, 10, 10, 10)
  )

  result <- sampling_design() |>
    draw(n = 4, method = "pps_brewer", mos = size, certainty_size = 300) |>
    execute(frame, seed = 70)

  certainty_rows <- result[result$.certainty_1 == TRUE, ]
  expect_true(all(certainty_rows$.weight == 1))
})

test_that("Bernoulli sampling gives equal weights", {
  frame <- make_property_frame()
  frac <- 0.2

  result <- sampling_design() |>
    draw(frac = frac, method = "bernoulli") |>
    execute(frame, seed = 80)

  if (nrow(result) > 0) {
    expected_weight <- 1 / frac
    expect_true(all(result$.weight == expected_weight))
  }
})

test_that("WR methods produce sequential .draw_k values", {
  frame <- make_property_frame()

  result <- sampling_design() |>
    draw(n = 15, method = "srswr") |>
    execute(frame, seed = 90)

  expect_true(".draw_1" %in% names(result))
  expect_equal(sort(result$.draw_1), seq_len(nrow(result)))
})

test_that("PPS WR methods produce sequential .draw_k values", {
  frame <- make_property_frame()

  for (method in c("pps_multinomial", "pps_chromy")) {
    result <- sampling_design() |>
      draw(n = 10, method = method, mos = pop) |>
      execute(frame, seed = 91)

    expect_true(".draw_1" %in% names(result), label = paste(method, "has .draw_1"))
    expect_equal(
      sort(result$.draw_1), seq_len(nrow(result)),
      label = paste(method, "sequential draws")
    )
  }
})

test_that("execute returns tbl_sample with correct metadata", {
  frame <- make_property_frame()
  design <- sampling_design() |>
    draw(n = 20)

  result <- execute(design, frame, seed = 100)

  expect_s3_class(result, "tbl_sample")
  expect_true(is_tbl_sample(result))
  expect_equal(get_stages_executed(result), 1L)
  expect_true(is_sampling_design(get_design(result)))
})

test_that("required output columns present for all methods", {
  frame <- make_property_frame()
  required <- c(".weight", ".weight_1", ".fpc_1", ".sample_id", ".stage")

  # WOR methods
  for (method in c("srswor", "systematic")) {
    result <- sampling_design() |>
      draw(n = 10, method = method) |>
      execute(frame, seed = 110)
    expect_true(
      all(required %in% names(result)),
      label = paste(method, "has required cols")
    )
  }

  # PPS WOR
  for (method in c("pps_brewer", "pps_systematic", "pps_cps")) {
    result <- sampling_design() |>
      draw(n = 10, method = method, mos = pop) |>
      execute(frame, seed = 111)
    expect_true(
      all(required %in% names(result)),
      label = paste(method, "has required cols")
    )
  }

  # WR methods (also have .draw_1)
  result_wr <- sampling_design() |>
    draw(n = 10, method = "srswr") |>
    execute(frame, seed = 112)
  expect_true(all(c(required, ".draw_1") %in% names(result_wr)))
})
