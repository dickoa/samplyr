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

test_that("compound stored keys have a readable display path", {
  values <- data.frame(
    first = c("Malmö/7", "", NA, "12:34"),
    second = c("尾", "x", "z", NA),
    stringsAsFactors = FALSE
  )
  keys <- samplyr:::make_group_key(values, names(values))
  original <- keys
  expect_identical(
    samplyr:::display_path_key(keys),
    c("Malmö\\/7/尾", "/x", "NA/z", "12:34/NA")
  )
  expect_identical(keys, original)

  malformed <- c("2|V3:a", "2|Vx:aN", "2|V1:aNtrailing")
  displayed <- expect_no_error(samplyr:::display_path_key(malformed))
  expect_true(all(grepl("^<invalid key 0x", displayed)))
})

test_that("validate_frame displays compound parents without machine keys", {
  frame <- data.frame(
    area = rep(c("Malmö/7", "North"), each = 4),
    segment = rep(c("09:west", "East"), each = 4),
    household = seq_len(8)
  )
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(area, segment) |>
    draw(n = 2) |>
    add_stage() |>
    draw(n = 2)
  sample <- execute(design, frame, seed = 94)
  drifted <- frame[-1, ]

  before <- testthat::capture_messages(validate_frame(sample, drifted))
  stored_key <- samplyr:::get_frame_digest(sample)$stages[[1]]$selected$key
  samplyr:::display_path_key(stored_key)
  after <- testthat::capture_messages(validate_frame(sample, drifted))
  expect_identical(after, before)

  text <- paste(after, collapse = "")
  expect_match(text, "under Malmö\\/7/09:west", fixed = TRUE)
  expect_false(grepl("[0-9]+\\|V[0-9]+:", text))
  expect_false(grepl("V1:|V3:", text))
})

test_that("frame_summary reports replicated executions per replicate", {
  # Stage detail remains a compact common-per-replicate summary.
  r <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 10) |>
    execute(test_frame, seed = 3, reps = 3)
  fs <- frame_summary(r)
  expect_equal(fs$n_realized, 40)
  expect_equal(fs$n_target, 40)
  expect_equal(fs$take_rate, 40 / 120)

  # Pool detail has one scalar row per structural pool and realization,
  # even when every replicate happens to have the same allocation.
  fp <- frame_summary(r, detail = "pool")
  expect_identical(nrow(fp), 12L)
  expect_identical(fp$replicate, rep(1:3, times = 4))
  expect_equal(fp$n_realized, rep(10, 12))
  expect_equal(fp$take_rate, rep(1 / 3, 12))
  expect_identical(
    anyDuplicated(fp[c("stage", "pool_id", "replicate")]),
    0L
  )

  # Random-size stage detail has no single realized value, while pool
  # detail exposes every replicate instead of returning a list column.
  b <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = 0.1, method = "bernoulli", on_empty = "silent") |>
    execute(test_frame, seed = 5, reps = 3)
  fs <- frame_summary(b)
  expect_equal(fs$n_target, 12)
  expect_true(is.na(fs$n_realized))
  expect_true(is.na(fs$take_rate))
  expect_equal(fs$n_expected, 12)
  fp <- frame_summary(b, detail = "pool")
  expect_identical(nrow(fp), 12L)
  expect_identical(fp$replicate, rep(1:3, times = 4))
  expect_equal(fp$n_target, rep(3, 12))
  expect_equal(fp$n_expected, rep(3, 12))
  realized <- as.double(as.vector(t(table(
    factor(b$stratum, levels = unique(test_frame$stratum)),
    factor(b$.replicate, levels = 1:3)
  ))))
  expect_equal(fp$n_realized, realized)
  expect_equal(fp$take_rate, realized / 30)
})

test_that("frame_summary unit detail spans the stacked replicates", {
  r <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 9, reps = 3, frame_digest = "full")
  fu <- frame_summary(r, detail = "unit")
  expect_identical(nrow(fu), 120L)
  # n_hits counts occurrences across all replicates; is_selected marks
  # selection in at least one.
  expect_equal(sum(fu$n_hits), 30)
  expect_identical(sum(fu$is_selected), 26L)
  expect_true(all(fu$n_hits[!fu$is_selected] == 0))
  # A WOR unit drawn by more than one replicate shows n_hits > 1.
  expect_true(any(fu$n_hits > 1))

  # Cross-check against the per-replicate trace.
  sel <- samplyr:::get_frame_digest(r)$stages[[1]]$selected
  expect_identical(sort(unique(sel$replicate)), 1:3)
  expect_equal(
    sum(fu$n_hits),
    nrow(sel)
  )
  per_rep <- table(sel$replicate)
  expect_true(all(per_rep == 10))
})

test_that("replicated multi-stage digests report the shared stage prefix", {
  r <- sampling_design() |>
    add_stage("Clusters") |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage("Units") |> draw(n = 3) |>
    execute(test_frame, seed = 8, reps = 3)

  # Later stages are replicate-specific: the manifest keeps stage 1
  # and frame_summary states the truncation instead of silently
  # narrowing.
  expect_message(
    fs <- frame_summary(r),
    "replicate-specific"
  )
  expect_identical(fs$stage, 1L)
  expect_identical(
    samplyr:::get_frame_digest(r)$status, "partial"
  )

  expect_error(
    suppressMessages(frame_summary(r, stage = 2)),
    "replicate-specific"
  )

  # One replicate records the full depth, silently.
  s1 <- sampling_design() |>
    add_stage("Clusters") |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage("Units") |> draw(n = 3) |>
    execute(test_frame, seed = 8)
  expect_no_message(fs1 <- frame_summary(s1))
  expect_identical(fs1$stage, 1:2)
})
