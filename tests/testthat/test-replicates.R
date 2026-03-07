# test-replicates.R — Tests for replicated sampling (reps parameter)

# --- Basic functionality ---

test_that("reps draws R independent samples with .replicate column", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 42, reps = 3)

  expect_s3_class(result, "tbl_sample")
  expect_true(".replicate" %in% names(result))
  expect_equal(sort(unique(result$.replicate)), 1:3)
  expect_equal(nrow(result), 60)
})

test_that("replicates are independent (different rows selected)", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 42, reps = 3)

  rep1 <- result$id[result$.replicate == 1]
  rep2 <- result$id[result$.replicate == 2]
  # With n=20 from 120, there's overlap, but they shouldn't be identical

  expect_false(identical(rep1, rep2))
})

test_that("seed reproducibility across runs", {
  design <- sampling_design() |> draw(n = 20)

  r1 <- execute(design, test_frame, seed = 42, reps = 3)
  r2 <- execute(design, test_frame, seed = 42, reps = 3)

  expect_identical(r1$id, r2$id)
  expect_identical(r1$.replicate, r2$.replicate)
  expect_identical(r1$.weight, r2$.weight)
})

test_that("seed + r - 1 convention: replicate 1 matches standalone seed", {
  design <- sampling_design() |> draw(n = 20)

  replicated <- execute(design, test_frame, seed = 42, reps = 3)
  standalone <- execute(design, test_frame, seed = 42)

  rep1 <- replicated[replicated$.replicate == 1, ]
  expect_equal(sort(rep1$id), sort(standalone$id))
})

test_that("no seed still produces independent replicates", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, reps = 3)

  expect_equal(sort(unique(result$.replicate)), 1:3)
  rep1 <- result$id[result$.replicate == 1]
  rep2 <- result$id[result$.replicate == 2]
  # Should be different (extremely unlikely to be identical by chance)
  expect_false(identical(rep1, rep2))
})

test_that(".sample_id is globally unique across stacked output", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 42, reps = 3)

  expect_equal(result$.sample_id, seq_len(nrow(result)))
  expect_false(anyDuplicated(result$.sample_id) > 0)
})

test_that("row ordering: grouped by .replicate ascending", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 42, reps = 5)

  # .replicate should be non-decreasing
  expect_true(all(diff(result$.replicate) >= 0))
})

test_that("weights are correct per replicate", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 42, reps = 3)

  # SRS n=20 from 120 => weight = 120/20 = 6
  expect_true(all(result$.weight == 6))
})

test_that("reps with stratified proportional allocation", {
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40) |>
    execute(test_frame, seed = 1, reps = 3)

  expect_equal(nrow(result), 120)
  # Each replicate has 40 rows, 10 per stratum
  for (r in 1:3) {
    rep_data <- result[result$.replicate == r, ]
    expect_equal(nrow(rep_data), 40)
    expect_equal(as.integer(table(rep_data$stratum)), rep(10L, 4))
  }
})

test_that("nrow equals n_per_rep * reps for fixed-size methods", {
  result <- sampling_design() |>
    draw(n = 15) |>
    execute(test_frame, seed = 1, reps = 4)

  expect_equal(nrow(result), 60)
})

# --- Validation ---

test_that("reps must be integer >= 2", {
  design <- sampling_design() |> draw(n = 10)

  expect_error(execute(design, test_frame, reps = 1), "integer >= 2")
  expect_error(execute(design, test_frame, reps = 0), "integer >= 2")
  expect_error(execute(design, test_frame, reps = -1), "integer >= 2")
  expect_error(execute(design, test_frame, reps = 1.5), "integer >= 2")
  expect_error(execute(design, test_frame, reps = "3"), "integer >= 2")
})

test_that("reps + panels is an error", {
  design <- sampling_design() |> draw(n = 10)

  expect_error(
    execute(design, test_frame, seed = 1, reps = 3, panels = 2),
    "cannot be used together"
  )
})

test_that("reps + PRN on executed stage is an error", {
  tf <- test_frame
  tf$prn <- runif(nrow(tf))

  design <- sampling_design() |>
    draw(n = 10, method = "pps_pareto", mos = mos, prn = prn)

  expect_error(
    execute(design, tf, seed = 1, reps = 3),
    "permanent random numbers"
  )
})

test_that("reps on continuation is allowed when PRN was on a prior stage", {
  tf <- test_frame
  tf$prn <- runif(nrow(tf))

  design <- sampling_design() |>
    add_stage("Clusters") |>
      cluster_by(cluster) |>
      draw(n = 10, method = "pps_pareto", mos = mos, prn = prn) |>
    add_stage("Units") |>
      draw(n = 3)

  # Stage 1 with PRN, no reps
  stage1 <- execute(design, tf, stages = 1, seed = 1)

  # Continue stage 2 with reps — should work because PRN stage already done
  result <- execute(stage1, tf, seed = 10, reps = 3)
  expect_equal(sort(unique(result$.replicate)), 1:3)
})

# --- Multi-stage designs ---

test_that("two-stage replicated sampling", {
  result <- sampling_design() |>
    add_stage("Clusters") |>
      stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage("Units") |>
      draw(n = 3) |>
    execute(test_frame, seed = 1, reps = 3)

  expect_true(".replicate" %in% names(result))
  expect_equal(sort(unique(result$.replicate)), 1:3)
  # Each replicate: 4 strata * 2 clusters * 3 units = 24
  for (r in 1:3) {
    expect_equal(nrow(result[result$.replicate == r, ]), 24)
  }
  expect_equal(nrow(result), 72)
})

test_that("stratified PPS replicated", {
  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 8, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 42, reps = 2)

  expect_equal(sort(unique(result$.replicate)), 1:2)
  # 8 per stratum * 4 strata = 32 per rep
  expect_equal(nrow(result), 64)
})

# --- WR/PMR methods ---

test_that("reps works with srswr", {
  result <- sampling_design() |>
    draw(n = 10, method = "srswr") |>
    execute(test_frame, seed = 1, reps = 3)

  expect_equal(sort(unique(result$.replicate)), 1:3)
  expect_true(".draw_1" %in% names(result))
})

test_that("reps works with pps_multinomial", {
  result <- sampling_design() |>
    draw(n = 8, method = "pps_multinomial", mos = mos) |>
    execute(test_frame, seed = 42, reps = 2)

  expect_equal(sort(unique(result$.replicate)), 1:2)
  expect_true(".draw_1" %in% names(result))
})

test_that(".draw_k values are per-replicate for WR methods", {
  result <- sampling_design() |>
    draw(n = 10, method = "srswr") |>
    execute(test_frame, seed = 1, reps = 3)

  # Each replicate should have draw values starting from 1
  for (r in 1:3) {
    rep_draws <- result$.draw_1[result$.replicate == r]
    expect_true(1L %in% rep_draws)
  }
})

# --- Continuation ---

test_that("continuation from replicated partial sample auto-loops", {
  design <- sampling_design() |>
    add_stage("Clusters") |>
      stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 2) |>
    add_stage("Units") |>
      draw(n = 3)

  # Stage 1 with reps
  stage1 <- execute(design, test_frame, stages = 1, seed = 1, reps = 3)
  expect_equal(sort(unique(stage1$.replicate)), 1:3)

  # Continue — should auto-loop per replicate
  result <- execute(stage1, test_frame, seed = 100)
  expect_equal(sort(unique(result$.replicate)), 1:3)
  # Each replicate expanded to units
  expect_true(".weight_2" %in% names(result))
})

test_that("continuation: cannot add reps to replicated sample", {
  design <- sampling_design() |>
    add_stage("Clusters") |>
      cluster_by(cluster) |>
      draw(n = 10) |>
    add_stage("Units") |>
      draw(n = 3)

  stage1 <- execute(design, test_frame, stages = 1, seed = 1, reps = 2)

  expect_error(
    execute(stage1, test_frame, seed = 10, reps = 3),
    "Cannot add new replicates"
  )
})

test_that("continuation: add reps to non-replicated sample works", {
  design <- sampling_design() |>
    add_stage("Clusters") |>
      cluster_by(cluster) |>
      draw(n = 10) |>
    add_stage("Units") |>
      draw(n = 3)

  stage1 <- execute(design, test_frame, stages = 1, seed = 1)
  expect_false(".replicate" %in% names(stage1))

  result <- execute(stage1, test_frame, seed = 10, reps = 3)
  expect_equal(sort(unique(result$.replicate)), 1:3)
})

test_that("continuation: replicate count preserved through continuation", {
  design <- sampling_design() |>
    add_stage("Clusters") |>
      cluster_by(cluster) |>
      draw(n = 10) |>
    add_stage("Units") |>
      draw(n = 3)

  stage1 <- execute(design, test_frame, stages = 1, seed = 1, reps = 2)
  result <- execute(stage1, test_frame, seed = 100)

  expect_equal(length(unique(result$.replicate)), 2)
  expect_equal(attr(result, "metadata")$reps, 2)
})

test_that("continuation: panels rejected on replicated input", {
  design <- sampling_design() |>
    add_stage("Clusters") |>
      cluster_by(cluster) |>
      draw(n = 10) |>
    add_stage("Units") |>
      draw(n = 3)

  stage1 <- execute(design, test_frame, stages = 1, seed = 1, reps = 2)

  expect_error(
    execute(stage1, test_frame, seed = 100, panels = 2),
    "panels.*replicated"
  )
})

# --- Metadata ---

test_that("metadata records reps count and replicate_seeds", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 42, reps = 3)

  meta <- attr(result, "metadata")
  expect_equal(meta$reps, 3)
  expect_equal(meta$replicate_seeds, c(42L, 43L, 44L))
})

test_that("metadata replicate_seeds is NULL when no seed", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, reps = 3)

  meta <- attr(result, "metadata")
  expect_equal(meta$reps, 3)
  expect_null(meta$replicate_seeds)
})

test_that("stages_executed is correct for replicated design", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_equal(get_stages_executed(result), 1L)
})

test_that("stages_executed is correct for replicated continuation", {
  design <- sampling_design() |>
    add_stage("Clusters") |>
      cluster_by(cluster) |>
      draw(n = 10) |>
    add_stage("Units") |>
      draw(n = 3)

  stage1 <- execute(design, test_frame, stages = 1, seed = 1, reps = 2)
  result <- execute(stage1, test_frame, seed = 100)

  expect_equal(get_stages_executed(result), 1:2)
})

# --- Method coverage ---

test_that("reps works with balanced sampling", {
  result <- sampling_design() |>
    draw(n = 20, method = "balanced", aux = c(mos, y)) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_equal(sort(unique(result$.replicate)), 1:2)
  expect_equal(nrow(result), 40)
})

test_that("reps works with random-size bernoulli", {
  result <- sampling_design() |>
    draw(frac = 0.2, method = "bernoulli") |>
    execute(test_frame, seed = 1, reps = 3)

  expect_equal(sort(unique(result$.replicate)), 1:3)
  # Replicates may have different sizes
  rep_sizes <- table(result$.replicate)
  expect_true(all(rep_sizes > 0))
})

test_that("reps works with certainty selection", {
  tf <- test_frame
  tf$big_mos <- tf$mos
  tf$big_mos[1:3] <- 10000  # Force certainty

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "pps_brewer", mos = big_mos) |>
    execute(tf, seed = 1, reps = 2)

  expect_equal(sort(unique(result$.replicate)), 1:2)
})

# --- Survey export guards ---

test_that("as_svydesign errors on replicated sample", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_error(
    as_svydesign(result),
    class = "samplyr_error_replicated_sample_unsupported"
  )
})

test_that("as_svydesign works after filtering to one replicate", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  one_rep <- result[result$.replicate == 1, ]
  svy <- as_svydesign(one_rep)
  expect_s3_class(svy, "survey.design")
})

test_that("as_svrepdesign errors on replicated sample", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_error(
    as_svrepdesign(result),
    class = "samplyr_error_replicated_sample_unsupported"
  )
})

test_that("design_effect errors on replicated sample", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_error(
    design_effect(result),
    class = "samplyr_error_replicated_sample_unsupported"
  )
})

test_that("effective_n errors on replicated sample", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_error(
    effective_n(result),
    class = "samplyr_error_replicated_sample_unsupported"
  )
})

test_that("joint_expectation errors on replicated sample", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_error(
    joint_expectation(result, test_frame),
    class = "samplyr_error_replicated_sample_unsupported"
  )
})

test_that("check_single_replicate errors on NA in .replicate", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  result$.replicate[1] <- NA_integer_
  expect_error(as_svydesign(result), "NA")
})

# --- Print and summary ---

test_that("tbl_sum shows replicate count for multi-replicate", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 3)

  header <- tbl_sum(result)
  expect_true("Replicates" %in% names(header))
  expect_equal(header[["Replicates"]], "3")
})

test_that("tbl_sum omits replicate line for single replicate", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  # Filter to single replicate
  one_rep <- result[result$.replicate == 1, ]
  header <- tbl_sum(one_rep)
  expect_false("Replicates" %in% names(header))
})

test_that("tbl_sum omits replicate line for non-replicated sample", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1)

  header <- tbl_sum(result)
  expect_false("Replicates" %in% names(header))
})

test_that("summary reports replicate count and omits weight diagnostics", {
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40) |>
    execute(test_frame, seed = 1, reps = 3)

  output <- capture.output(summary(result))
  output_text <- paste(output, collapse = "\n")

  expect_match(output_text, "reps = 3")
  expect_match(output_text, "n/rep")
  expect_match(output_text, "replicate 1")
  expect_match(output_text, "omitted")
})

test_that("summary shows full weight diagnostics for single replicate", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1)

  output <- capture.output(summary(result))
  output_text <- paste(output, collapse = "\n")

  expect_match(output_text, "DEFF")
  expect_no_match(output_text, "omitted")
})

# --- Bug fix: replicated multi-phase ---

test_that("replicated phase-1 passed as frame executes per-replicate", {
  # Phase 1: replicated
  phase1 <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_equal(nrow(phase1), 40)

  # Phase 2: new design on replicated phase-1
  phase2 <- sampling_design() |>
    draw(n = 5) |>
    execute(phase1, seed = 100)

  expect_true(".replicate" %in% names(phase2))
  expect_equal(sort(unique(phase2$.replicate)), 1:2)
  # 5 per replicate
  expect_equal(sum(phase2$.replicate == 1), 5)
  expect_equal(sum(phase2$.replicate == 2), 5)
  expect_equal(nrow(phase2), 10)
})

test_that("replicated multi-phase: reps on replicated frame errors", {
  phase1 <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_error(
    sampling_design() |>
      draw(n = 5) |>
      execute(phase1, seed = 1, reps = 3),
    "already replicated"
  )
})

test_that("replicated multi-phase: panels on replicated frame errors", {
  phase1 <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_error(
    sampling_design() |>
      draw(n = 5) |>
      execute(phase1, seed = 1, panels = 2),
    "panels.*replicated"
  )
})

test_that("replicated multi-phase: weights compound correctly", {
  phase1 <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  phase2 <- sampling_design() |>
    draw(n = 5) |>
    execute(phase1, seed = 100)

  # Phase-1 weight: 120/20 = 6, phase-2 weight: 20/5 = 4
  # Combined: 6 * 4 = 24
  expect_equal(unique(phase2$.weight), 24)
})

test_that("replicated multi-phase: seed reproducibility", {
  phase1 <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  r1 <- sampling_design() |> draw(n = 5) |> execute(phase1, seed = 100)
  r2 <- sampling_design() |> draw(n = 5) |> execute(phase1, seed = 100)

  expect_identical(r1$id, r2$id)
  expect_identical(r1$.replicate, r2$.replicate)
})

test_that("replicated multi-phase: .replicate stripped from phase-2 internals", {
  phase1 <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  phase2 <- sampling_design() |>
    draw(n = 5) |>
    execute(phase1, seed = 100)

  # .replicate should only appear once (outer tag, not leaked from frame)
  expect_equal(sum(names(phase2) == ".replicate"), 1)
})

test_that("non-replicated phase-1 with reps on phase-2 still works", {
  phase1 <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1)

  phase2 <- sampling_design() |>
    draw(n = 5) |>
    execute(phase1, seed = 100, reps = 3)

  expect_equal(sort(unique(phase2$.replicate)), 1:3)
  expect_equal(nrow(phase2), 15)
})

# --- Bug fix: stages validation before PRN check ---

test_that("reps with out-of-range stages gives clean user error", {
  design <- sampling_design() |> draw(n = 10)

  expect_error(
    execute(design, test_frame, stages = 2, reps = 2),
    "integers between 1 and 1"
  )
})

test_that("reps with out-of-range stages on continuation gives clean error", {
  design <- sampling_design() |>
    add_stage("A") |>
      cluster_by(cluster) |>
      draw(n = 10) |>
    add_stage("B") |>
      draw(n = 3)

  stage1 <- execute(design, test_frame, stages = 1, seed = 1)

  expect_error(
    execute(stage1, test_frame, stages = 5, reps = 2),
    "integers between 1 and 2"
  )
})

# --- Bug fix: summary/print with corrupted .replicate ---

test_that("summary handles NA in .replicate gracefully", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  result$.replicate[1] <- NA_integer_

  # Should not error, should warn about NA
  output <- capture.output(summary(result))
  output_text <- paste(output, collapse = "\n")
  # Should fall back to non-replicated display (show DEFF)
  expect_match(output_text, "DEFF")
})

test_that("tbl_sum omits replicate line when .replicate has NA", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  result$.replicate[1] <- NA_integer_

  header <- tbl_sum(result)
  expect_false("Replicates" %in% names(header))
})
