# Digest-backed print and summary output.

two_stage_fixture <- function(seed = 8) {
  sampling_design() |>
    add_stage("Clusters") |> stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage("Units") |> draw(n = 3) |>
    execute(test_frame, seed = seed)
}

test_that("tbl_sum shows one coverage line", {
  s <- two_stage_fixture()
  header <- tbl_sum(s)
  expect_identical(unname(header["Sampling"]), "2 stages | 24/120 units")
  expect_snapshot(print(tbl_sum(s)))

  s1 <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1)
  expect_identical(
    unname(tbl_sum(s1)["Sampling"]),
    "1 stage | 20/120 units"
  )
})

test_that("the coverage line is absent without a digest or a universe", {
  s_none <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, frame_digest = "none")
  expect_false("Sampling" %in% names(tbl_sum(s_none)))

  s <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1)
  expect_true("Sampling" %in% names(tbl_sum(s)))
  s$.weight <- s$.weight * 2
  expect_false("Sampling" %in% names(tbl_sum(s)))

  # Replicate-varying realized sizes: no line rather than a guess.
  r <- sampling_design() |>
    draw(frac = 0.1, method = "bernoulli", on_empty = "silent") |>
    execute(test_frame, seed = 5, reps = 3)
  expect_false("Sampling" %in% names(tbl_sum(r)))
})

test_that("random-size coverage shows the realized fraction", {
  s <- sampling_design() |>
    draw(frac = 0.1, method = "bernoulli", on_empty = "silent") |>
    execute(test_frame, seed = 5)
  expect_identical(
    unname(tbl_sum(s)["Sampling"]),
    paste0("1 stage | ", nrow(s), "/120 units")
  )
})

test_that("summary prints the realization report from the digest", {
  s <- two_stage_fixture()
  expect_snapshot(summary(s))
})

test_that("summary shows replicate ranges for random-size designs", {
  r <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = 0.1, method = "bernoulli", on_empty = "silent") |>
    execute(test_frame, seed = 3, reps = 3)
  expect_snapshot(summary(r))
})

test_that("summary reports balance and bound diagnostics", {
  s <- sampling_design() |>
    draw(n = 20, method = "cube", mos = mos, aux = c(y, bound(stratum))) |>
    execute(test_frame, seed = 62)
  out <- paste(capture.output(summary(s)), collapse = "\n")
  expect_match(out, "Balance on y: target ", fixed = TRUE)
  expect_match(out, "HT estimate", fixed = TRUE)
  expect_match(out, "Count bounds satisfied: 4/4 levels.", fixed = TRUE)
})

test_that("summary reports certainty selections from the registry", {
  s <- sampling_design() |>
    draw(n = 12, method = "pps_brewer", mos = mos,
         certainty_size = 180) |>
    execute(test_frame, seed = 4, frame_digest = "full")
  out <- paste(capture.output(summary(s)), collapse = "\n")
  expect_match(out, "5 certainty selections", fixed = TRUE)
})

test_that("summary falls back to the sample-based report without a digest", {
  s <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40) |>
    execute(test_frame, seed = 2, frame_digest = "none")
  out <- paste(capture.output(summary(s)), collapse = "\n")
  expect_match(out, "4 strata: N_h 30, n_h 10, f_h 0.3333", fixed = TRUE)
})

test_that("pools under different parents keep distinct lines", {
  # The proof case from the plan: same stratum label under different
  # parents must not be merged in the realization report.
  frame <- expand.grid(
    district = c("A", "B"),
    ea = 1:3,
    hh = 1:4,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  s <- sampling_design() |>
    add_stage() |> cluster_by(district) |> draw(n = 2) |>
    add_stage() |> stratify_by(district) |> cluster_by(ea) |>
    draw(n = 1) |>
    execute(frame, seed = 2)
  out <- capture.output(summary(s))
  # Two stage-2 pools, one per parent district, each 1/3; a merged
  # display would overstate the take rate as N 6, n 2, f 0.6667.
  expect_true(any(grepl("2 pools: N_h 3, n_h 1, f_h 0.3333", out,
                        fixed = TRUE)))
  expect_false(any(grepl("0.6667", out, fixed = TRUE)))
  fs <- frame_summary(s, stage = 2, detail = "pool")
  expect_identical(nrow(fs), 2L)
  expect_equal(fs$N, c(3, 3))
  expect_equal(fs$n_realized, c(1, 1))
})
