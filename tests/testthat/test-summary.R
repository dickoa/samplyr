test_that("summary distinguishes sample fractions from inclusion probabilities", {
  set.seed(42)
  frame <- data.frame(
    id = seq_len(100),
    size = runif(100, 1, 2),
    x = rnorm(100)
  )

  equal_sample <- sampling_design() |>
    draw(n = 20, method = "cube", aux = x) |>
    execute(frame, seed = 1)
  equal_txt <- paste(capture.output(summary(equal_sample)), collapse = "\n")

  expect_match(equal_txt, "N = 100, n = 20, f = 0.2000", fixed = TRUE)
  expect_false(grepl("mean f", equal_txt, fixed = TRUE))
  expect_false(grepl("unit-specific", equal_txt, fixed = TRUE))

  unequal_sample <- sampling_design() |>
    draw(n = 20, method = "cube", mos = size, aux = x) |>
    execute(frame, seed = 1)
  unequal_txt <- paste(
    capture.output(summary(unequal_sample)),
    collapse = "\n"
  )

  expect_match(unequal_txt, "N = 100, n = 20, f = 0.2000", fixed = TRUE)
  expect_match(unequal_txt, "MOS size", fixed = TRUE)
  expect_false(grepl("mean inclusion fraction", unequal_txt, fixed = TRUE))
})

test_that("summary uses concise with-replacement wording", {
  frame <- data.frame(id = seq_len(20))
  sample <- sampling_design() |>
    draw(n = 5, method = "srswr") |>
    execute(frame, seed = 1)

  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "srswr (with replacement)", fixed = TRUE)
  expect_match(txt, "n = 5 draws (no FPC)", fixed = TRUE)
  expect_false(grepl(";", txt, fixed = TRUE))
})

test_that("summary labels PMR without calling it with replacement", {
  frame <- data.frame(id = seq_len(20), size = seq_len(20))
  sample <- sampling_design() |>
    draw(n = 8, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 3)

  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "pps_chromy (minimum replacement)", fixed = TRUE)
  expect_match(txt, "n = 8 draws (no FPC)", fixed = TRUE)
  expect_false(grepl("pps_chromy (with replacement)", txt, fixed = TRUE))
})

test_that("summary omits fractions when pool N is unknown", {
  stage <- list(
    pools = data.frame(
      pool_id = 1L,
      parent_unit = NA_integer_,
      N = NA_integer_,
      n_target = 2,
      n_expected = 2,
      n_realized = 2L,
      scope = "universe",
      chance_status = "executed"
    ),
    selected = NULL,
    strata = NULL,
    chance_kind = "inclusion_probability",
    storage = "constant",
    scope = "universe",
    diagnostics = NULL
  )

  single_txt <- paste(
    capture.output(samplyr:::summary_stage_realization(stage, FALSE)),
    collapse = "\n"
  )
  expect_match(single_txt, "N = NA, n = 2", fixed = TRUE)
  expect_false(grepl(", f =", single_txt, fixed = TRUE))

  stage$pools <- rbind(
    stage$pools,
    transform(stage$pools, pool_id = 2L, N = 10L)
  )
  multi_txt <- paste(
    capture.output(samplyr:::summary_stage_realization(stage, FALSE)),
    collapse = "\n"
  )
  expect_match(multi_txt, "N_h", fixed = TRUE)
  expect_false(grepl("f_h", multi_txt, fixed = TRUE))
})

test_that("WR summary omits an invalid universe denominator", {
  frame <- data.frame(id = seq_len(44570))
  sample <- sampling_design() |>
    draw(n = 60000, method = "srswr") |>
    execute(frame, seed = 2)

  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "n = 60,000 | stages = 1/1", fixed = TRUE)
  expect_match(txt, "n = 60,000 draws (no FPC)", fixed = TRUE)
  expect_false(grepl("60,000 of 44,570", txt, fixed = TRUE))
  expect_identical(
    unname(tbl_sum(sample)["Sampling"]),
    "1 stage | 60,000 draws"
  )
})

test_that("WOR summary retains its universe denominator", {
  frame <- data.frame(id = seq_len(20))
  sample <- sampling_design() |>
    draw(n = 5) |>
    execute(frame, seed = 2)

  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "n = 5 of 20", fixed = TRUE)
  expect_identical(
    unname(tbl_sum(sample)["Sampling"]),
    "1 stage | 5/20 units"
  )
})

test_that("WR fallback counts clustered draw occurrences", {
  frame <- data.frame(
    cluster = rep(1:2, each = 4),
    unit = rep(1:4, 2)
  )
  sample <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 5, method = "srswr") |>
    execute(frame, seed = 4, frame_digest = "none")

  expect_identical(dplyr::n_distinct(sample$.draw_1), 5L)
  expect_gt(nrow(sample), 5L)
  expect_lt(dplyr::n_distinct(sample$cluster), 5L)
  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "n = 5 draws (no FPC)", fixed = TRUE)
  expect_false(grepl(
    paste0("n = ", nrow(sample), " draws"), txt, fixed = TRUE
  ))

  sample$.draw_1 <- NULL
  fallback_txt <- paste(capture.output(summary(sample)), collapse = "\n")
  n_clusters <- dplyr::n_distinct(sample$cluster)
  expect_match(
    fallback_txt,
    paste0("n = ", n_clusters, " selected clusters (no FPC)"),
    fixed = TRUE
  )
  expect_false(grepl("n = 5 draws", fallback_txt, fixed = TRUE))

  element_sample <- sampling_design() |>
    draw(n = 5, method = "srswr") |>
    execute(data.frame(id = seq_len(10)), seed = 6, frame_digest = "none")
  element_sample$.draw_1 <- NULL
  element_txt <- paste(
    capture.output(summary(element_sample)), collapse = "\n"
  )
  expect_match(
    element_txt, "n = 5 selected units (no FPC)", fixed = TRUE
  )
  expect_false(grepl("n = 5 draws", element_txt, fixed = TRUE))
})

test_that("WR fallback qualifies restarted draw indices by ancestry", {
  frame <- expand.grid(
    unit = 1:2,
    child = 1:2,
    parent = 1:2,
    KEEP.OUT.ATTRS = FALSE
  )
  sample <- sampling_design() |>
    add_stage() |> cluster_by(parent) |> draw(n = 2) |>
    add_stage() |> cluster_by(child) |> draw(n = 3, method = "srswr") |>
    execute(frame, seed = 5, frame_digest = "none")

  occurrences <- dplyr::distinct(
    as.data.frame(sample), parent, .draw_2
  )
  expect_identical(nrow(occurrences), 6L)
  expect_gt(nrow(sample), nrow(occurrences))
  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "n = 6 draws (no FPC)", fixed = TRUE)
})

test_that("replicated WR ranges place draws before the qualifier", {
  stage <- list(
    pools = data.frame(
      pool_id = 1L,
      parent_unit = NA_integer_,
      N = 10L,
      n_target = NA_real_,
      n_expected = NA_real_,
      n_realized = NA_integer_,
      chance_status = "executed"
    ),
    selected = data.frame(
      pool_id = 1L,
      replicate = rep(1:2, c(5, 7))
    ),
    strata = NULL,
    chance_kind = "expected_hits",
    storage = "constant",
    scope = "universe",
    diagnostics = NULL
  )

  txt <- paste(
    capture.output(samplyr:::summary_stage_realization(stage, TRUE)),
    collapse = "\n"
  )
  expect_match(
    txt, "n = 5-7 draws across replicates (no FPC)", fixed = TRUE
  )
  expect_false(grepl("across replicates draws", txt, fixed = TRUE))
})

test_that("realization ranges use fixed decimals for small fractions", {
  frame <- data.frame(
    id = seq_len(40000),
    stratum = rep(c("small", "large"), c(10000, 30000))
  )
  sample <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 1) |>
    execute(frame, seed = 7)

  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "N_h 10,000-30,000", fixed = TRUE)
  expect_match(txt, "f_h 0.0000-0.0001", fixed = TRUE)
  expect_false(grepl("e-", txt, fixed = TRUE))
})

test_that("large reached and universe pool counts use separators", {
  frame <- data.frame(parent = seq_len(2000), unit = 1L)
  sample <- sampling_design() |>
    add_stage() |> cluster_by(parent) |> draw(n = 1200) |>
    add_stage() |> draw(n = 1) |>
    execute(frame, seed = 8)

  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "1,200/2,000 pools", fixed = TRUE)
  expect_false(grepl("1200/2000 pools", txt, fixed = TRUE))
})

test_that("missing replicate warning renders its field markup", {
  sample <- sampling_design() |>
    draw(n = 5) |>
    execute(data.frame(id = seq_len(20)), seed = 9, reps = 2)
  sample$.replicate[[1]] <- NA_integer_

  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(
    txt,
    ".replicate contains missing values. Replicate reporting disabled.",
    fixed = TRUE
  )
  expect_false(grepl("{.field .replicate}", txt, fixed = TRUE))
})

test_that("summary flags approximate-probabilities methods", {
  set.seed(7)
  frame <- data.frame(id = seq_len(40), size = runif(40, 1, 3))

  approx <- sampling_design() |>
    draw(n = 8, method = "pps_sps", mos = size) |>
    execute(frame, seed = 3)
  txt <- paste(capture.output(summary(approx)), collapse = "\n")
  expect_match(
    txt,
    "pps_sps (approximate probabilities)",
    fixed = TRUE
  )

  exact <- sampling_design() |>
    draw(n = 8, method = "pps_brewer", mos = size) |>
    execute(frame, seed = 3)
  txt <- paste(capture.output(summary(exact)), collapse = "\n")
  expect_match(txt, "pps_brewer", fixed = TRUE)
  expect_false(grepl("approximate probabilities", txt, fixed = TRUE))
})
