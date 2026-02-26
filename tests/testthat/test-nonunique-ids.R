# Non-unique cluster IDs across parent clusters
#
# Common in real surveys: ea=1 in district A and ea=1 in district B.
# All operations (split, join, dedup) must use the full ancestry key.

# Shared frame: 3 districts x 4 EAs (1-4 reused) x 5 HH = 60 rows
make_nonunique_frame <- function() {
  frame <- expand.grid(
    district = c("A", "B", "C"),
    ea = 1:4,
    hh = 1:5,
    KEEP.OUT.ATTRS = FALSE
  )
  frame$hh_id <- seq_len(nrow(frame))
  frame$pop_ea <- 50L
  frame
}

test_that("two-stage execute: non-unique EA IDs across districts", {
  frame <- make_nonunique_frame()

  design <- sampling_design() |>
    add_stage() |>
      cluster_by(district) |>
      draw(n = 2) |>
    add_stage() |>
      cluster_by(ea) |>
      draw(n = 2)

  result <- execute(design, frame, seed = 42)

  # 2 districts * 2 EAs * 5 HH = 20 rows
  expect_equal(nrow(result), 20)

  # No cross-district contamination: each district has exactly 2 EAs
  selected_districts <- unique(result$district)
  expect_equal(length(selected_districts), 2)
  for (d in selected_districts) {
    sub <- result[result$district == d, ]
    expect_equal(length(unique(sub$ea)), 2)
    expect_equal(nrow(sub), 10)
  }

  # Weights sum to population
  expect_equal(sum(result$.weight), nrow(frame))
})

test_that("three-stage execute: non-unique IDs at multiple levels", {
  frame3 <- expand.grid(
    region = c("R1", "R2"),
    district = c("D1", "D2"),
    ea = 1:3,
    hh = 1:2,
    KEEP.OUT.ATTRS = FALSE
  )
  frame3$pop_d <- 10L
  frame3$pop_e <- 5L

  design <- sampling_design() |>
    add_stage() |>
      cluster_by(region) |>
      draw(n = 2) |>
    add_stage() |>
      cluster_by(district) |>
      draw(n = 1) |>
    add_stage() |>
      cluster_by(ea) |>
      draw(n = 2)

  result <- execute(design, frame3, seed = 1)

  # 2 regions * 1 district * 2 EAs * 2 HH = 8 rows
  expect_equal(nrow(result), 8)

  # Weights sum to population (24)
  expect_equal(sum(result$.weight), nrow(frame3))

  # Each region has exactly 1 district
  for (r in unique(result$region)) {
    sub <- result[result$region == r, ]
    expect_equal(length(unique(sub$district)), 1)
    # Each district has exactly 2 EAs
    expect_equal(length(unique(sub$ea)), 2)
  }
})

test_that("continuation: non-unique EA IDs across districts", {
  frame <- make_nonunique_frame()

  design <- sampling_design() |>
    add_stage() |>
      cluster_by(district) |>
      draw(n = 2) |>
    add_stage() |>
      cluster_by(ea) |>
      draw(n = 2)

  # Partial: stage 1 only
  stage1 <- execute(design, frame, stages = 1, seed = 42)
  expect_equal(length(unique(stage1$district)), 2)

  # Continue: stage 2
  result <- execute(stage1, frame, seed = 43)

  # Same structure: 2 districts * 2 EAs * 5 HH = 20
  expect_equal(nrow(result), 20)
  expect_equal(sum(result$.weight), nrow(frame))

  # No cross-district contamination
  for (d in unique(result$district)) {
    sub <- result[result$district == d, ]
    expect_equal(length(unique(sub$ea)), 2)
  }
})

test_that("joint_expectation: non-unique cluster IDs", {
  frame_jip <- expand.grid(
    district = c("A", "B"),
    ea = 1:5,
    KEEP.OUT.ATTRS = FALSE
  )
  frame_jip$pop <- rep(c(100, 200, 150, 80, 120), 2)

  design <- sampling_design() |>
    add_stage() |>
      cluster_by(district) |>
      draw(n = 2) |>
    add_stage() |>
      cluster_by(ea) |>
      draw(n = 3, method = "pps_brewer", mos = pop)

  result <- execute(design, frame_jip, seed = 42)
  jip <- joint_expectation(result, frame_jip, stage = 2)

  # Matrix dims = number of selected (district, ea) pairs
  n_selected <- nrow(dplyr::distinct(result, district, ea))
  expect_equal(nrow(jip[[2]]), n_selected)
  expect_equal(ncol(jip[[2]]), n_selected)

  # Diagonal elements are first-order inclusion probs (0 < pi < 1)
  diag_vals <- diag(jip[[2]])
  expect_true(all(diag_vals > 0))
  expect_true(all(diag_vals <= 1))
})

test_that("summary: clustered stage shows cluster counts not row counts", {
  frame <- make_nonunique_frame()

  # Two-stage: stratify+cluster at stage 1, draw at stage 2
  # Stage 1: 2 of 4 EAs per district (FPC = 4, n = 2 clusters)
  design <- sampling_design() |>
    add_stage() |>
      stratify_by(district) |>
      cluster_by(district, ea) |>
      draw(n = 2) |>
    add_stage() |>
      draw(n = 3)

  result <- execute(design, frame, seed = 42)

  # Capture summary output
  output <- capture.output(summary(result))

  # Find lines containing f_h values (sampling fractions)
  fh_lines <- grep("0\\.", output, value = TRUE)
  # Extract numeric f_h values
  fh_values <- as.numeric(
    regmatches(fh_lines, regexpr("0\\.\\d+", fh_lines))
  )

  # All stage-1 f_h should be <= 1 (was > 1 before fix because n_h counted rows)
  expect_true(all(fh_values <= 1))

  # n_h = 2 clusters per district, N_h = 4 EAs per district, f_h = 0.5
  expect_true(any(grepl("0\\.5000", output)))
})

test_that("summary: unstratified clustered stage with non-unique IDs", {
  # Exact repro: both districts select the same ea value.
  # distinct(ea) alone collapses them to 1; need (district, ea).
  frame <- expand.grid(
    district = c("A", "B"),
    ea = 1:3,
    hh = 1:4,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  design <- sampling_design() |>
    add_stage() |>
      cluster_by(district) |>
      draw(n = 2) |>
    add_stage() |>
      cluster_by(ea) |>
      draw(n = 1)

  s <- execute(design, frame, seed = 2)

  # Both districts selected the same ea — 2 distinct (district, ea) pairs
  n_pairs <- nrow(unique(s[c("district", "ea")]))
  expect_equal(n_pairs, 2)

  output <- capture.output(summary(s))

  # Stage 2 allocation: N = 3, n = 2, f = 0.6667
  expect_true(any(grepl("n = 2", output)))
  expect_true(any(grepl("0\\.6667", output)))
})

test_that("summary: stratified clustered stage with non-unique IDs", {
  # Same frame, but stage 2 adds stratify_by(district).
  # Dedup must still use (district, ea) — strata alone don't help
  # when both branches share the same stage_unit_vars.
  frame <- expand.grid(
    district = c("A", "B"),
    ea = 1:3,
    hh = 1:4,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  design <- sampling_design() |>
    add_stage() |>
      cluster_by(district) |>
      draw(n = 2) |>
    add_stage() |>
      stratify_by(district) |>
      cluster_by(ea) |>
      draw(n = 1)

  s <- execute(design, frame, seed = 2)
  output <- capture.output(summary(s))

  # Stage 2: each district has N_h = 3 EAs, n_h = 1 selected, f_h = 0.3333
  # Without ancestor key the dedup would still be correct here (strata_vars
  # already contain district), but this confirms the stratified path works.
  fh_lines <- grep("0\\.3333", output)
  expect_true(length(fh_lines) > 0)
})

test_that("WR parent stage with non-unique child IDs", {
  frame <- make_nonunique_frame()

  design <- sampling_design() |>
    add_stage() |>
      cluster_by(district) |>
      draw(n = 2, method = "srswr") |>
    add_stage() |>
      cluster_by(ea) |>
      draw(n = 2)

  result <- execute(design, frame, seed = 42)

  # With WR at stage 1, each draw is independent
  # Check no cross-district EA contamination within each draw
  for (dk in unique(result$.draw_1)) {
    sub <- result[result$.draw_1 == dk, ]
    for (d in unique(sub$district)) {
      eas_in_district <- unique(sub$ea[sub$district == d])
      expect_lte(length(eas_in_district), 2)
    }
  }

  # Weights have the Inf FPC for stage 1 (WR)
  expect_true(all(result$.fpc_1 == Inf))
})
