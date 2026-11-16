# Tests for replicated sampling (reps parameter)

## Basic functionality

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

test_that("each replicate is the execution at its recorded seed", {
  design <- sampling_design() |> draw(n = 20)

  replicated <- execute(design, test_frame, seed = 42, reps = 3)
  seeds <- attr(replicated, "metadata")$replicate_seeds
  for (r in 1:3) {
    standalone <- execute(design, test_frame, seed = seeds[r])
    expect_identical(
      sort(replicated$id[replicated$.replicate == r]),
      sort(standalone$id),
      info = r
    )
  }
})

test_that("no seed still produces independent replicates", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, reps = 3)

  expect_equal(sort(unique(result$.replicate)), 1:3)
  rep1 <- result$id[result$.replicate == 1]
  rep2 <- result$id[result$.replicate == 2]
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

  expect_true(all(diff(result$.replicate) >= 0))
})

test_that("weights are correct per replicate", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 42, reps = 3)

  # 120/20 = 6.
  expect_true(all(result$.weight == 6))
})

test_that("reps with stratified proportional allocation", {
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40) |>
    execute(test_frame, seed = 1, reps = 3)

  expect_equal(nrow(result), 120)
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

## Validation

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
  # A clustered stage needs one PRN per cluster, not per descendant row.
  clusters <- unique(tf$cluster)
  tf$prn <- runif(length(clusters))[match(tf$cluster, clusters)]

  design <- sampling_design() |>
    draw(n = 10, method = "pps_pareto", mos = mos, prn = prn)

  expect_error(
    execute(design, tf, seed = 1, reps = 3),
    "permanent random numbers"
  )
})

test_that("reps on continuation is allowed when PRN was on a prior stage", {
  tf <- test_frame
  # A clustered stage needs one PRN per cluster, not per descendant row.
  clusters <- unique(tf$cluster)
  tf$prn <- runif(length(clusters))[match(tf$cluster, clusters)]

  design <- sampling_design() |>
    add_stage("Clusters") |>
      cluster_by(cluster) |>
      draw(n = 10, method = "pps_pareto", mos = mos, prn = prn) |>
    add_stage("Units") |>
      draw(n = 3)

  stage1 <- execute(design, tf, stages = 1, seed = 1)

  # The PRN stage is already done.
  result <- execute(stage1, tf, seed = 10, reps = 3)
  expect_equal(sort(unique(result$.replicate)), 1:3)
})

test_that("inherited replicates + PRN on an executed stage is an error", {
  tf <- data.frame(
    psu = rep(1:10, each = 10),
    id = 1:100,
    u = ((1:100 * 37) %% 100 + 0.5) / 100
  )
  prn_stage <- sampling_design() |>
    draw(frac = 0.5, method = "bernoulli", prn = u)

  # A replicated phase 1 used as the frame of a PRN phase 2.
  phase1 <- sampling_design() |> draw(n = 60) |> execute(tf, seed = 1, reps = 4)
  expect_error(
    execute(prn_stage, phase1, seed = 2),
    class = "samplyr_error_replicated_sample_unsupported"
  )
  expect_error(execute(prn_stage, phase1, seed = 2), "permanent random")

  # A replicated partial sample continued into a PRN stage.
  design <- sampling_design() |>
    add_stage() |>
      cluster_by(psu) |>
      draw(n = 5) |>
    add_stage() |>
      draw(frac = 0.5, method = "bernoulli", prn = u)
  stage1 <- execute(design, tf, stages = 1, seed = 1, reps = 4)
  expect_error(
    execute(stage1, tf, seed = 2),
    class = "samplyr_error_replicated_sample_unsupported"
  )

  # Without PRN, each replicate draws on its own.
  plain <- sampling_design() |> draw(frac = 0.5, method = "bernoulli")
  expect_equal(sort(unique(execute(plain, phase1, seed = 2)$.replicate)), 1:4)
})

## Multi-stage designs

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
  # 4 strata * 2 clusters * 3 units.
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
  # 8 per stratum * 4 strata.
  expect_equal(nrow(result), 64)
})

## WR/PMR methods

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

  for (r in 1:3) {
    rep_draws <- result$.draw_1[result$.replicate == r]
    expect_true(1L %in% rep_draws)
  }
})

## Continuation

test_that("continuation from replicated partial sample auto-loops", {
  design <- sampling_design() |>
    add_stage("Clusters") |>
      stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 2) |>
    add_stage("Units") |>
      draw(n = 3)

  stage1 <- execute(design, test_frame, stages = 1, seed = 1, reps = 3)
  expect_equal(sort(unique(stage1$.replicate)), 1:3)

  result <- execute(stage1, test_frame, seed = 100)
  expect_equal(sort(unique(result$.replicate)), 1:3)
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

## Metadata

test_that("metadata records reps count and replicate_seeds", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 42, reps = 3)

  meta <- attr(result, "metadata")
  expect_equal(meta$reps, 3)
  # Drawn from the seed, not counted up from it.
  expect_identical(
    meta$replicate_seeds,
    c(1781592037L, 1228985497L, 608797924L)
  )
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

## Method coverage

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
  rep_sizes <- table(result$.replicate)
  expect_true(all(rep_sizes > 0))
})

test_that("reps works with certainty selection", {
  tf <- test_frame
  tf$big_mos <- tf$mos
  tf$big_mos[1:3] <- 10000

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "pps_brewer", mos = big_mos) |>
    execute(tf, seed = 1, reps = 2)

  expect_equal(sort(unique(result$.replicate)), 1:2)
})

## Survey export guards

test_that("as_svydesign errors on replicated sample", {
  skip_if_not_installed("survey")
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_error(
    as_svydesign(result),
    class = "samplyr_error_replicated_sample_unsupported"
  )
})

test_that("as_svydesign works after filtering to one replicate", {
  skip_if_not_installed("survey")
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  one_rep <- result[result$.replicate == 1, ]
  svy <- as_svydesign(one_rep)
  expect_s3_class(svy, "survey.design")
})

test_that("as_svrepdesign errors on replicated sample", {
  skip_if_not_installed("survey")
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
  skip_if_not_installed("survey")
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  result$.replicate[1] <- NA_integer_
  expect_error(as_svydesign(result), "NA")
})

test_that("execute refuses a sample with a missing replicate id", {
  design <- sampling_design() |>
    add_stage() |> cluster_by(cl) |> draw(n = 4) |>
    add_stage() |> draw(n = 2)
  frame <- data.frame(cl = rep(1:20, each = 5), id = 1:100)
  stage1 <- execute(design, frame, stages = 1, seed = 1, reps = 3)
  stage1$.replicate[c(1, 5)] <- NA_integer_
  phase1 <- sampling_design() |>
    draw(n = 40) |>
    execute(frame, seed = 2, reps = 3)
  phase1$.replicate[2] <- NA_integer_
  unnamed <- dplyr::filter(phase1, .replicate %in% 1)
  unnamed$.replicate <- NA_integer_

  attempts <- list(
    continuation = function() execute(stage1, frame, seed = 3),
    phase = function() {
      execute(sampling_design() |> draw(n = 10), phase1, seed = 3)
    },
    all_missing = function() {
      execute(sampling_design() |> draw(n = 10), unnamed, seed = 3)
    }
  )
  expected <- c(
    continuation = "input sample has 2 rows",
    phase = "frame sample has 1 row",
    all_missing = "frame sample has 39 rows"
  )
  for (nm in names(attempts)) {
    err <- expect_error(
      attempts[[nm]](),
      class = "samplyr_error_replicated_sample_unsupported"
    )
    expect_match(conditionMessage(err), expected[[nm]], fixed = TRUE)
    expect_identical(condition_header(err), "execute", info = nm)
  }
})

## Print and summary

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
  expect_match(output_text, "per replicate")
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

## Replicated multi-phase execution

test_that("replicated phase-1 passed as frame executes per-replicate", {
  phase1 <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  expect_equal(nrow(phase1), 40)

  phase2 <- sampling_design() |>
    draw(n = 5) |>
    execute(phase1, seed = 100)

  expect_true(".replicate" %in% names(phase2))
  expect_equal(sort(unique(phase2$.replicate)), 1:2)
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

  # Phase 1 weight 120/20 = 6 times phase 2 weight 20/5 = 4.
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

  # The outer tag only, not one leaked from the frame.
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

## Stage validation comes before the PRN check

test_that("reps with out-of-range stages gives clean user error", {
  design <- sampling_design() |> draw(n = 10)

  expect_error(
    execute(design, test_frame, stages = 2, reps = 2),
    class = "samplyr_error_stage_selector"
  )
  expect_error(
    execute(design, test_frame, stages = 2, reps = 2),
    "Available: 1"
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
    class = "samplyr_error_stage_selector"
  )
  expect_error(
    execute(stage1, test_frame, stages = 5, reps = 2),
    "Available: 1 and 2"
  )
})

## summary() and print() with a corrupted .replicate

test_that("summary handles NA in .replicate gracefully", {
  result <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 1, reps = 2)

  result$.replicate[1] <- NA_integer_

  output <- capture.output(summary(result))
  output_text <- paste(output, collapse = "\n")
  # Falls back to the non-replicated display, which shows DEFF.
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

test_that("replicates that end at an empty first stage stack with the others", {
  frame <- data.frame(cl = rep(1:40, each = 5), id = 1:200)
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(cl) |>
    draw(frac = 0.03, method = "bernoulli", on_empty = "silent") |>
    add_stage() |>
    draw(n = 2)

  result <- execute(design, frame, seed = 2, reps = 6)
  meta <- attr(result, "metadata")
  expect_identical(
    unname(meta$replicate_rows),
    c(0L, 2L, 4L, 0L, 4L, 2L)
  )
  expect_true(all(c(".weight_1", ".weight_2") %in% names(result)))

  single <- execute(design, frame, seed = meta$replicate_seeds[[3]])
  stacked <- as.data.frame(result)[result$.replicate == 3L, names(single)]
  stacked$.sample_id <- seq_len(nrow(stacked))
  rownames(stacked) <- NULL
  expect_identical(stacked$id, single$id)
  expect_identical(stacked$.weight, single$.weight)
})

test_that("replicates keep a matrix column as the frame holds it", {
  set.seed(8)
  frame <- data.frame(
    id = 1:80,
    st = rep(c("a", "b"), each = 40),
    psu = rep(1:16, each = 5),
    m = runif(80, 1, 5)
  )
  frame$xy <- I(cbind(easting = frame$id, northing = 80 - frame$id))
  same_rows <- function(x, source = frame) {
    expect_identical(x$xy, source$xy[x$id, , drop = FALSE])
  }
  designs <- list(
    sampling_design() |> draw(n = 4),
    sampling_design() |>
      stratify_by(st) |>
      draw(n = 2, method = "pps_systematic", mos = m),
    sampling_design() |>
      add_stage() |>
      cluster_by(psu) |>
      draw(n = 3) |>
      add_stage() |>
      draw(n = 2)
  )
  for (design in designs) {
    same_rows(execute(design, frame, seed = 1))
    same_rows(execute(design, frame, seed = 1, reps = 3))
  }

  first <- execute(designs[[3]], frame, stages = 1, seed = 2, reps = 3)
  same_rows(execute(first, frame, seed = 3))
  phase_1 <- execute(sampling_design() |> draw(n = 30), frame, seed = 4, reps = 2)
  same_rows(execute(sampling_design() |> draw(n = 5), phase_1, seed = 5))

  plain <- tibble::tibble(id = 1:20, xy = cbind(easting = 1:20, northing = 20:1))
  s <- execute(sampling_design() |> draw(n = 4), plain, seed = 6, reps = 2)
  expect_identical(s$xy, plain$xy[s$id, , drop = FALSE])
})

test_that("a census exports with zero variance or a classed refusal", {
  skip_if_not_installed("survey")
  # A census total has variance zero. Under the jackknife survey gives an
  # unstratified census no replicate column, which neither sharing nor
  # calibration could read, and it now takes two columns at the full
  # weights. survey 4.5's calibrate() needs compress = FALSE on the
  # uncompressed matrix of equal weights, so it is called that way.
  frame <- data.frame(
    id = 1:16, st = rep(c("A", "B"), each = 8), psu = rep(1:8, each = 2),
    g = rep(c("a", "b"), 8), y = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7,
                                   9, 3)
  )
  designs <- list(
    plain = sampling_design() |> draw(frac = 1),
    stratified = sampling_design() |> stratify_by(st) |> draw(frac = 1),
    clustered = sampling_design() |> cluster_by(psu) |> draw(frac = 1),
    two_stage = sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
      add_stage() |> draw(frac = 1)
  )
  types <- c("auto", "JK1", "JKn", "BRR", "Fay", "bootstrap",
             "subbootstrap", "mrbbootstrap", "rwyb")
  checked <- character(0)
  for (dn in names(designs)) {
    s <- execute(designs[[dn]], frame, seed = 1)
    for (type in types) {
      warned <- character(0)
      r <- tryCatch(withCallingHandlers(
        if (type %in% c("bootstrap", "subbootstrap", "mrbbootstrap",
                        "rwyb")) {
          as_svrepdesign(s, type = type, replicates = 10)
        } else {
          as_svrepdesign(s, type = type)
        },
        warning = function(w) {
          warned <<- c(warned, grep("^samplyr_", class(w), value = TRUE))
          invokeRestart("muffleWarning")
        },
        message = function(m) invokeRestart("muffleMessage")
      ), error = identity)
      outcome <- if (inherits(r, "error")) {
        class(r)[1]
      } else {
        variance <- as.numeric(stats::vcov(survey::svytotal(~y, r)))
        calibrated <- survey::calibrate(
          r, ~g, population = c(`(Intercept)` = 16, gb = 8),
          compress = FALSE
        )
        calibrated_variance <- as.numeric(
          stats::vcov(survey::svytotal(~y, calibrated))
        )
        expect_lt(calibrated_variance, 1e-20)
        if (variance < 1e-20) "zero" else "positive"
      }
      if (type %in% c("auto", "JK1") && dn %in% c("plain", "clustered")) {
        expect_identical(ncol(r$repweights), 2L)
      }
      checked <- c(checked, paste(dn, type, outcome,
                                  paste(unique(warned), collapse = " ")))
    }
  }
  refused <- "samplyr_error_svrep_conversion_failed"
  # BRR and Fay resampled a stratified census, which their missing
  # correction made positive, and the rescaled bootstrap warned of that
  # correction although survey left the census out.
  expect_setequal(trimws(checked), c(
    "plain auto zero", "plain JK1 zero", paste("plain JKn", refused),
    paste("plain BRR", refused), paste("plain Fay", refused),
    "plain bootstrap zero", "plain subbootstrap zero",
    "plain mrbbootstrap zero", "plain rwyb zero",
    paste("stratified auto", refused), paste("stratified JK1", refused),
    paste("stratified JKn", refused), "stratified BRR zero",
    "stratified Fay zero", "stratified bootstrap zero",
    "stratified subbootstrap zero",
    "stratified mrbbootstrap zero", "stratified rwyb zero",
    "clustered auto zero", "clustered JK1 zero",
    paste("clustered JKn", refused), paste("clustered BRR", refused),
    paste("clustered Fay", refused), "clustered bootstrap zero",
    "clustered subbootstrap zero",
    "clustered mrbbootstrap zero", "clustered rwyb zero",
    "two_stage auto zero", paste("two_stage JK1", refused),
    "two_stage JKn zero", "two_stage BRR zero", "two_stage Fay zero",
    "two_stage bootstrap zero", "two_stage subbootstrap zero",
    "two_stage mrbbootstrap zero", "two_stage rwyb zero"
  ))
})

test_that("a stratum taken whole adds no replicate variance", {
  skip_if_not_installed("survey")
  # Closed form: a stratified SRS without replacement has variance
  # sum_h N_h^2 (1 - n_h / N_h) s_h^2 / n_h, to which a stratum taken whole
  # adds nothing. survey left that stratum out of its replicate estimates,
  # so under mse = TRUE they were centered on a total that held it, and the
  # jackknife gave 165 times this. BRR, Fay and the rescaled bootstrap,
  # which carry no correction, resampled it.
  frame <- data.frame(id = 1:48, st = rep(c("A", "B", "C"), each = 16))
  frame$y <- (frame$id * 37) %% 23 + 5
  frame$y_whole <- ifelse(frame$st == "A", frame$y, 0)
  take <- c(A = 16, B = 4, C = 6)
  s <- execute(sampling_design() |> stratify_by(st) |> draw(n = take),
               frame, seed = 1)
  expect_identical(sum(s$st == "A"), 16L)
  closed <- sum(vapply(c("B", "C"), function(h) {
    y <- s$y[s$st == h]
    16^2 * (1 - take[[h]] / 16) * stats::var(y) / take[[h]]
  }, numeric(1)))
  variance <- function(design, formula) {
    as.numeric(stats::vcov(survey::svytotal(formula, design)))
  }
  checked <- character(0)
  for (type in c("JKn", "BRR", "Fay", "bootstrap", "subbootstrap",
                 "mrbbootstrap", "rwyb")) {
    for (mse in c(FALSE, TRUE)) {
      design <- suppressWarnings(withr::with_seed(1, if (type %in% c(
        "bootstrap", "subbootstrap", "mrbbootstrap", "rwyb"
      )) {
        as_svrepdesign(s, type = type, replicates = 50, mse = mse)
      } else {
        as_svrepdesign(s, type = type, mse = mse)
      }))
      expect_null(design$selfrep)
      expect_lt(variance(design, ~y_whole), 1e-20)
      if (identical(type, "JKn")) {
        expect_equal(variance(design, ~y), closed, tolerance = 1e-10)
      }
      checked <- c(checked, paste(type, mse))
    }
  }
  expect_length(unique(checked), 14L)

  # A census of every stratum converts under BRR, Fay and the rescaled
  # bootstrap with no warning of a correction they would drop.
  census <- execute(sampling_design() |> stratify_by(st) |> draw(frac = 1),
                    frame, seed = 1)
  for (type in c("BRR", "Fay", "subbootstrap")) {
    expect_no_warning(design <- as_svrepdesign(census, type = type))
    expect_lt(variance(design, ~y), 1e-20)
  }
})

test_that("mrbbootstrap keeps the stage below a census first stage", {
  skip_if_not_installed("survey")
  # Closed form: 12 PSUs of 4 units all taken, 2 units drawn in each, so
  # the variance is sum_i 4^2 (1 - 2 / 4) s_i^2 / 2. survey marked every
  # row self-representing from the first stage alone and gave 0.
  frame <- data.frame(id = 1:48, psu = rep(1:12, each = 4))
  frame$y <- (frame$id * 37) %% 23 + 5
  s <- execute(sampling_design() |>
                 add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
                 add_stage() |> draw(n = 2),
               frame, seed = 1)
  closed <- sum(tapply(s$y, s$psu, function(y) 16 * (1 / 2) * stats::var(y) / 2))
  expect_gt(closed, 0)
  linearized <- suppressWarnings(as_svydesign(s))
  expect_equal(as.numeric(stats::vcov(survey::svytotal(~y, linearized))),
               closed, tolerance = 1e-10)
  # 2,000 replicates put the simulation error near 3 %, inside the band.
  for (mse in c(FALSE, TRUE)) {
    design <- withr::with_seed(3, as_svrepdesign(
      s, type = "mrbbootstrap", replicates = 2000, mse = mse
    ))
    v <- as.numeric(stats::vcov(survey::svytotal(~y, design)))
    expect_lt(abs(v / closed - 1), 0.15)
  }
})

test_that("the replicates keep stage 2's correction inside a certainty PSU", {
  skip_if_not_installed("survey")
  # Closed form over the certainty PSUs: sum_i N_i^2 (1 - n_i / N_i) s_i^2 / n_i.
  # The replicates resampled their stage-2 units as drawn with replacement
  # and gave 1 / (1 - n_i / N_i) times that, 2 at n = 2 of 4.
  closed <- function(s, frame, units) {
    sum(vapply(units, function(p) {
      big_n <- sum(frame$psu == p)
      y <- s$y[s$psu == p]
      big_n^2 * (1 - length(y) / big_n) * stats::var(y) / length(y)
    }, numeric(1)))
  }
  total_var <- function(design) {
    as.numeric(stats::vcov(survey::svytotal(~y, design)))
  }
  frame <- data.frame(psu = rep(1:12, rep(c(4, 6, 10), 4)))
  frame$id <- seq_len(nrow(frame))
  frame$y <- (frame$id * 37) %% 23 + 5
  s <- execute(sampling_design() |>
                 add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
                 add_stage() |> draw(n = 2),
               frame, seed = 1)
  truth <- closed(s, frame, 1:12)
  for (type in c("JKn", "BRR", "Fay")) {
    for (mse in c(FALSE, TRUE)) {
      expect_no_warning(design <- as_svrepdesign(
        s, type = type, mse = mse, fay.rho = if (type == "Fay") 0.3 else 0
      ))
      expect_equal(total_var(design), truth, tolerance = 1e-10,
                   info = paste(type, mse))
    }
  }
  # 2,000 replicates put the simulation error near 3 %, inside the band.
  for (type in c("bootstrap", "subbootstrap")) {
    expect_no_warning(design <- withr::with_seed(3, as_svrepdesign(
      s, type = type, replicates = 2000
    )))
    expect_lt(abs(total_var(design) / truth - 1), 0.15)
  }

  # Strata inside each PSU, halves of 3, 4 or 6 units, with 2 units drawn
  # from u and 3 from r, so one r cell of each size-6 PSU is taken whole.
  frame <- data.frame(psu = rep(1:12, rep(c(6, 8, 12), 4)))
  frame$id <- seq_len(nrow(frame))
  frame$y <- (frame$id * 37) %% 23 + 5
  frame$sub <- ave(frame$id, frame$psu, FUN = function(i) {
    ifelse(seq_along(i) <= length(i) / 2, "u", "r")
  })
  frame$cell <- paste(frame$psu, frame$sub)
  s <- execute(sampling_design() |>
                 add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
                 add_stage() |> stratify_by(sub) |>
                 draw(n = data.frame(sub = c("u", "r"), n = c(2, 3))),
               frame, seed = 1)
  expect_identical(as.vector(table(s$psu)), rep(5L, 12))
  cells <- unique(s$cell)
  truth <- sum(vapply(cells, function(k) {
    big_n <- sum(frame$cell == k)
    y <- s$y[s$cell == k]
    big_n^2 * (1 - length(y) / big_n) * stats::var(y) / length(y)
  }, numeric(1)))
  expect_no_warning(design <- as_svrepdesign(s, type = "JKn"))
  expect_equal(total_var(design), truth, tolerance = 1e-10)

  # Certainty PSUs of a PPS first stage, drawn without or with replacement.
  # y is zero outside them, so only their stage 2 adds variance.
  frame <- data.frame(psu = rep(1:20, rep(c(10, 12), c(16, 4))))
  frame$id <- seq_len(nrow(frame))
  frame$mos <- ifelse(frame$psu > 16, 400, 10 + frame$psu)
  frame$y <- ifelse(frame$psu > 16, (frame$id * 37) %% 23 + 5, 0)
  for (method in c("pps_brewer", "pps_multinomial")) {
    s <- suppressWarnings(execute(sampling_design() |>
      add_stage() |> cluster_by(psu) |>
      draw(n = 8, mos = mos, method = method, certainty_size = 300) |>
      add_stage() |> draw(n = 4), frame, seed = 1))
    expect_setequal(unique(s$psu[s$.certainty_1]), 17:20)
    design <- suppressWarnings(as_svrepdesign(s, type = "JKn"))
    expect_equal(total_var(design), closed(s, frame, 17:20),
                 tolerance = 1e-10, info = method)
  }
})

test_that("the stage-2 correction takes one factor per resampling cell", {
  # Cell 1's units carry two fractions, so no single factor exists; cell
  # 2's units share one; cell 3 holds a unit sampled at stage 3.
  sub <- list(split = rep(TRUE, 6), cell_id = c(1, 1, 2, 2, 3, 3),
              unit_id = 1:6)
  lower <- list(
    list(kind = "equal_wor", prob = c(0.5, 0.25, 0.5, 0.5, 0.5, 0.5)),
    list(kind = "equal_wor", prob = c(1, 1, 1, 1, 1, 0.5))
  )
  out <- samplyr:::certainty_lower_correction(sub, lower, 6)
  expect_equal(out$factor, c(1, 1, sqrt(0.5), sqrt(0.5), 1, 1))
  expect_identical(out$uncorrected, c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
  expect_true(out$below)
})

test_that("stage 2 keeps the with-replacement reading where no factor is exact", {
  skip_if_not_installed("survey")
  # With-replacement oracle over the certainty PSUs, from the weighted
  # totals z_j of the stage-2 units: sum_i n_i / (n_i - 1) sum_j
  # (z_j - mean_i(z))^2.
  wr_oracle <- function(s, unit) {
    z <- tapply(s$y * s$.weight, s[[unit]], sum)
    psu <- tapply(s$psu, s[[unit]], `[`, 1)
    sum(tapply(z, psu, function(v) {
      length(v) / (length(v) - 1) * sum((v - mean(v))^2)
    }))
  }
  total_var <- function(design) {
    as.numeric(stats::vcov(survey::svytotal(~y, design)))
  }
  frame <- expand.grid(el = 1:3, ssu = 1:4, psu = 1:6)
  frame$id <- seq_len(nrow(frame))
  frame$ssu_id <- paste(frame$psu, frame$ssu)
  frame$m <- frame$ssu + 1
  frame$y <- (frame$id * 37) %% 23 + 5
  census <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
    add_stage() |> cluster_by(ssu_id)
  designs <- list(
    # Stage 3 samples inside the stage-2 units.
    below = census |> draw(n = 2) |> add_stage() |> draw(n = 2),
    # A PPS stage 2 has no single factor.
    pps = census |> draw(n = 2, mos = m, method = "pps_brewer")
  )
  for (name in names(designs)) {
    s <- execute(designs[[name]], frame, seed = 1)
    expect_identical(
      variance_estimators(designs[[name]], frame)$class[4],
      "samplyr_warning_replicate_certainty_stage_wr"
    )
    for (type in c("JKn", "BRR", "Fay", "bootstrap", "subbootstrap")) {
      expect_warning(
        design <- if (type %in% c("bootstrap", "subbootstrap")) {
          as_svrepdesign(s, type = type, replicates = 20)
        } else {
          as_svrepdesign(s, type = type)
        },
        class = "samplyr_warning_replicate_certainty_stage_wr"
      )
      if (type == "JKn") {
        expect_equal(total_var(design), wr_oracle(s, "ssu_id"),
                     tolerance = 1e-10, info = name)
      }
    }
  }
  # One stratum mixing areas taken whole below with areas sampled below.
  # The children of an area are equal, so all variance is in the area
  # totals. Correcting only the whole areas made every contrast cancel, and
  # each type gave zero. The stratum keeps the with-replacement reading.
  mixed <- do.call(rbind, lapply(1:6, function(p) {
    do.call(rbind, lapply(1:4, function(a) {
      n <- if (a <= 2) 2 else 4
      data.frame(psu = p, area = paste(p, a), id = paste(p, a, seq_len(n)),
                 y = if (a <= 2) 5 else 10 * sqrt(0.5) / 4)
    }))
  }))
  s <- execute(sampling_design() |>
                 add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
                 add_stage() |> cluster_by(area) |> draw(n = 2) |>
                 add_stage() |> draw(n = 2),
               mixed, seed = 1)
  # The seed puts an area of each kind together in some PSU.
  kind <- tapply(s$.weight_3, s$area, `[`, 1) > 1
  psu_of <- tapply(s$psu, s$area, `[`, 1)
  expect_true(any(tapply(kind, psu_of, function(k) any(k) && !all(k))))
  for (type in c("JKn", "BRR", "Fay")) {
    expect_warning(
      design <- as_svrepdesign(s, type = type,
                               fay.rho = if (type == "Fay") 0.3 else 0),
      class = "samplyr_warning_replicate_certainty_stage_wr"
    )
    expect_equal(total_var(design), wr_oracle(s, "area"), tolerance = 1e-10,
                 info = type)
    expect_gt(total_var(design), 0)
  }

  # Strata are decided one by one: PSUs 1 to 3 take their areas' units
  # whole below and keep the correction, PSUs 4 to 6 sample below and keep
  # the with-replacement reading.
  sides <- do.call(rbind, lapply(1:6, function(p) {
    do.call(rbind, lapply(1:4, function(a) {
      n <- if (p <= 3) 2 else 4
      data.frame(psu = p, area = paste(p, a), id = paste(p, a, seq_len(n)))
    }))
  }))
  sides$y <- (seq_len(nrow(sides)) * 37) %% 23 + 5
  s <- execute(sampling_design() |>
                 add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
                 add_stage() |> cluster_by(area) |> draw(n = 2) |>
                 add_stage() |> draw(n = 2),
               sides, seed = 1)
  t <- tapply(s$y * s$.weight_3, s$area, sum)
  psu <- tapply(s$psu, s$area, `[`, 1)
  corrected <- sum(tapply(t[psu <= 3], psu[psu <= 3], function(v) {
    4^2 * (1 - 2 / 4) * stats::var(v) / 2
  }))
  kept <- wr_oracle(s[s$psu > 3, ], "area")
  expect_warning(design <- as_svrepdesign(s, type = "JKn"),
                 class = "samplyr_warning_replicate_certainty_stage_wr")
  expect_equal(total_var(design), corrected + kept, tolerance = 1e-10)

  # A stage below that takes every unit leaves the stage-2 totals exact, so
  # the correction applies: sum_i N_i^2 (1 - n_i / N_i) s_i^2(z) / n_i.
  s <- execute(census |> draw(n = 2) |> add_stage() |> draw(frac = 1),
               frame, seed = 1)
  z <- tapply(s$y, s$ssu_id, sum)
  psu <- tapply(s$psu, s$ssu_id, `[`, 1)
  truth <- sum(tapply(z, psu, function(v) 4^2 * (1 - 2 / 4) * stats::var(v) / 2))
  expect_no_warning(design <- as_svrepdesign(s, type = "JKn"))
  expect_equal(total_var(design), truth, tolerance = 1e-10)
})

test_that("a census of one unit converts under JK1 with zero variance", {
  skip_if_not_installed("survey")
  # survey's jackknife divides by the number of units less one and stopped
  # on a census of a single unit, which the report called supported.
  cases <- list(
    row_n = list(sampling_design() |> draw(n = 1), data.frame(id = 1, y = 3)),
    row_frac = list(sampling_design() |> draw(frac = 1),
                    data.frame(id = 1, y = 3)),
    cluster = list(sampling_design() |> cluster_by(cl) |> draw(frac = 1),
                   data.frame(cl = 1, id = 1:3, y = c(2, 5, 7)))
  )
  for (name in names(cases)) {
    design <- cases[[name]][[1]]
    frame <- cases[[name]][[2]]
    s <- execute(design, frame, seed = 1)
    report <- variance_estimators(design, frame)
    expect_identical(report$status[report$estimator == "Jackknife (JK1)"],
                     "supported", info = name)
    for (type in c("JK1", "auto")) {
      expect_no_warning(rep <- as_svrepdesign(s, type = type))
      expect_identical(rep$type, "JK1", info = paste(name, type))
      expect_identical(ncol(rep$repweights), 2L, info = paste(name, type))
      total <- survey::svytotal(~y, rep)
      expect_equal(unname(coef(total)), sum(frame$y), info = paste(name, type))
      expect_identical(as.vector(survey::SE(total)), 0, info = paste(name, type))
      # The weights are all one, so the matrix stays uncompressed.
      cal <- survey::calibrate(rep, ~1,
                               population = c(`(Intercept)` = nrow(frame)),
                               compress = FALSE)
      expect_identical(as.vector(survey::SE(survey::svytotal(~y, cal))), 0)
    }
  }
  # The resolved mse is kept, from the call or from survey's option.
  s <- execute(cases$row_n[[1]], cases$row_n[[2]], seed = 1)
  expect_true(as_svrepdesign(s, type = "JK1", mse = TRUE)$mse)
  withr::with_options(list(survey.replicates.mse = TRUE), {
    expect_true(as_svrepdesign(s, type = "JK1")$mse)
  })
  expect_false(as_svrepdesign(s, type = "JK1", mse = FALSE)$mse)
})

test_that("one unit drawn from several is refused with a class by every export", {
  skip_if_not_installed("survey")
  # survey's own "only one primary sampling unit" error reached the user
  # unclassed, and the report called JK1, the bootstrap and the multistage
  # bootstrap supported. A census of one unit exports, with zero variance.
  cases <- list(
    srs = list(sampling_design() |> draw(n = 1),
               data.frame(id = 1:2, y = c(2, 5))),
    cluster = list(sampling_design() |> cluster_by(cl) |> draw(n = 1),
                   data.frame(cl = rep(1:3, each = 2), id = 1:6, y = 1:6)),
    wr = list(sampling_design() |> draw(n = 1, method = "srswr"),
              data.frame(id = 1:3, y = 1:3)),
    pps = list(sampling_design() |> draw(n = 1, method = "pps_brewer", mos = m),
               data.frame(id = 1:4, m = 1:4, y = 1:4))
  )
  exports <- list(
    linearization = function(s) as_svydesign(s),
    JK1 = function(s) as_svrepdesign(s, type = "JK1"),
    JKn = function(s) as_svrepdesign(s, type = "JKn"),
    bootstrap = function(s) as_svrepdesign(s, type = "bootstrap",
                                           replicates = 10),
    mrbbootstrap = function(s) as_svrepdesign(s, type = "mrbbootstrap",
                                              replicates = 10)
  )
  for (name in names(cases)) {
    design <- cases[[name]][[1]]
    frame <- cases[[name]][[2]]
    s <- suppressWarnings(execute(design, frame, seed = 1))
    report <- variance_estimators(design, frame)
    for (type in names(exports)) {
      expect_error(suppressWarnings(exports[[type]](s)),
                   class = "samplyr_error_export_one_unit",
                   info = paste(name, type))
    }
    # Every row but RWYB, which refuses the stratum of one itself, and
    # random groups; the joint route applies to the PPS draw only.
    refused <- report$class %in% "samplyr_error_export_one_unit"
    expect_identical(sum(refused), if (name == "pps") 9L else 8L, info = name)
    expect_true(all(report$decided_by[refused] == "frame"), info = name)
  }
  # Without the frame, the pool size is unknown: a risk.
  report <- variance_estimators(sampling_design() |> draw(n = 1))
  expect_match(report$sample_risk[report$estimator == "Taylor linearization"],
               "samplyr_error_export_one_unit", fixed = TRUE)
  # Independent samples of the whole design give its variance.
  frame <- data.frame(id = 1:6, y = c(2, 5, 1, 8, 4, 6))
  reps <- execute(sampling_design() |> draw(n = 1), frame, seed = 1, reps = 5)
  se <- survey::SE(survey::svytotal(~y, as_svrepdesign(reps,
                                                         type = "random_groups")))
  expect_gt(as.vector(se), 0)
})

test_that("a census of every stage converts without survey's scale warning", {
  skip_if_not_installed("survey")
  # The census branch passed a scale to survey's BRR constructor, which
  # ignores it and warned "type='BRR' does not use 'scale=' argument".
  frame <- data.frame(id = 1:16, psu = rep(1:8, each = 2), y = 1:16)
  s <- execute(sampling_design() |>
                 add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
                 add_stage() |> draw(frac = 1),
               frame, seed = 1)
  types <- c("auto", "JKn", "BRR", "Fay", "bootstrap", "subbootstrap")
  for (type in types) {
    expect_no_warning(design <- as_svrepdesign(s, type = type))
    expect_identical(design$type, if (type == "auto") "JKn" else type)
    expect_identical(ncol(design$repweights), 2L)
    expect_lt(as.numeric(stats::vcov(survey::svytotal(~y, design))), 1e-20)
  }
})

test_that("a spliced BRR or Fay design converts without survey's scale warning", {
  skip_if_not_installed("survey")
  # A census first stage whose stratum u is taken whole inside each PSU
  # keeps u's rows out of the resampling. The design put back together
  # passed a scale to survey's BRR constructor, which warned "type='BRR'
  # does not use 'scale=' argument".
  frame <- data.frame(psu = rep(1:8, each = 6))
  frame$id <- seq_len(nrow(frame))
  frame$y <- (frame$id * 37) %% 23 + 5
  frame$sub <- rep(c("u", "u", "r", "r", "r", "r"), 8)
  s <- execute(sampling_design() |>
                 add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
                 add_stage() |> stratify_by(sub) |>
                 draw(n = data.frame(sub = c("u", "r"), n = c(2, 2))),
               frame, seed = 1)
  for (type in c("BRR", "Fay")) {
    expect_no_warning(design <- as_svrepdesign(
      s, type = type, fay.rho = if (type == "Fay") 0.3 else 0
    ))
    expect_identical(design$type, type)
    expect_equal(design$scale, if (type == "BRR") 1 / 12 else
      1 / (12 * 0.7^2))
  }
})

test_that("a stratified census refusal names the types that take it", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:16, st = rep(c("A", "B"), each = 8), y = 1:16,
                      m = rep(c(1, 3), 8))
  s <- execute(sampling_design() |> stratify_by(st) |> draw(frac = 1),
               frame, seed = 1)
  for (type in c("auto", "JKn")) {
    cnd <- expect_error(as_svrepdesign(s, type = type),
                        class = "samplyr_error_svrep_conversion_failed")
    expect_match(conditionMessage(cnd), "taken whole", fixed = TRUE)
    expect_match(conditionMessage(cnd), "rwyb", fixed = TRUE)
  }
  # The remedies hold: each gives this census a variance of zero.
  remedies <- list(
    bootstrap = as_svrepdesign(s, type = "bootstrap", replicates = 20),
    rwyb = as_svrepdesign(s, type = "rwyb", replicates = 20),
    linearization = as_svydesign(s)
  )
  for (nm in names(remedies)) {
    expect_lt(as.numeric(stats::vcov(survey::svytotal(~y, remedies[[nm]]))),
              1e-20, label = nm)
  }
  # A PPS census is converted by samplyr, not survey, and is not refused.
  pps <- execute(sampling_design() |> stratify_by(st) |>
                   draw(n = c(A = 8, B = 8), method = "pps_brewer", mos = m),
                 frame, seed = 1)
  expect_lt(as.numeric(stats::vcov(survey::svytotal(
    ~y, as_svrepdesign(pps, type = "JKn")
  ))), 1e-20)
  # Other conversion failures keep survey's own reason, with no remedy.
  odd <- execute(sampling_design() |> stratify_by(st) |> draw(frac = 1),
                 data.frame(id = 1:12, st = rep(c("A", "B"), c(5, 7)),
                            y = 1:12),
                 seed = 1)
  cnd <- expect_error(as_svrepdesign(odd, type = "BRR"),
                      class = "samplyr_error_svrep_conversion_failed")
  expect_no_match(conditionMessage(cnd), "rwyb", fixed = TRUE)
})
