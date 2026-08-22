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

test_that("fixed-size WOR methods return exactly n rows", {
  frame <- make_property_frame()
  n <- 25

  methods <- c(
    "srswor", "systematic", "pps_brewer", "pps_systematic", "pps_cps",
    "pps_sampford", "pps_sps", "pps_pareto"
  )

  for (method in methods) {
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

test_that("multi-stage weight sum equals N with equal cluster sizes", {
  frame <- make_property_frame()

  result <- sampling_design() |>
    add_stage(label = "EAs") |>
      cluster_by(ea_id) |>
      draw(n = 20) |>
    add_stage(label = "Units") |>
      draw(n = 3) |>
    execute(frame, seed = 61)

  expect_equal(sum(result$.weight), nrow(frame), tolerance = 1e-10)
})

test_that("multistage WR weights use expected hits", {
  frame <- data.frame(
    psu = rep(1:4, each = 5),
    unit = rep(1:5, 4)
  )

  result <- sampling_design() |>
    add_stage("PSUs") |>
      cluster_by(psu) |>
      draw(n = 6, method = "srswr") |>
    add_stage("Units") |>
      draw(n = 2, method = "srswor") |>
    execute(frame, seed = 321)

  expect_equal(result$.weight_1, rep(4 / 6, nrow(result)))
  expect_equal(result$.weight_2, rep(5 / 2, nrow(result)))
  expect_equal(result$.weight, rep(5 / 3, nrow(result)))
  expect_equal(sum(result$.weight), nrow(frame))
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

test_that("core output columns are present across method families", {
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
