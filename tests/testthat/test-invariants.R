make_invariant_frame <- function() {
  set.seed(2026)
  data.frame(
    unit_id = seq_len(600),
    region = rep(c("North", "South", "East"), each = 200),
    ea_id = rep(seq_len(60), each = 10),
    hh_count = rep(sample(20:150, 60, replace = TRUE), each = 10),
    stringsAsFactors = FALSE
  )
}

test_that("default srswor is equivalent to explicit srswor", {
  frame <- make_invariant_frame()

  sample_default <- sampling_design() |>
    draw(n = 120) |>
    execute(frame, seed = 111)

  sample_explicit <- sampling_design() |>
    draw(n = 120, method = "srswor") |>
    execute(frame, seed = 111)

  expect_equal(sample_default$unit_id, sample_explicit$unit_id)
  expect_equal(sample_default$.weight, sample_explicit$.weight)
})

test_that("continuation is reproducible with the same stage seeds", {
  frame <- make_invariant_frame()

  design <- sampling_design() |>
    add_stage(label = "EA") |>
      cluster_by(ea_id) |>
      draw(n = 12) |>
    add_stage(label = "Units") |>
      draw(n = 4)

  run_once <- function() {
    s1 <- execute(design, frame, stages = 1, seed = 77)
    execute(s1, frame, seed = 88)
  }

  result1 <- run_once()
  result2 <- run_once()

  norm1 <- result1 |>
    dplyr::arrange(unit_id) |>
    dplyr::select(unit_id, ea_id, .weight, .weight_1, .weight_2)
  norm2 <- result2 |>
    dplyr::arrange(unit_id) |>
    dplyr::select(unit_id, ea_id, .weight, .weight_1, .weight_2)

  expect_equal(norm1, norm2)
})

test_that("a phase continued after its first stage keeps each row's phase-1 weight", {
  # Phase 1 weights households of group B 2.5 and group A 1 within every EA.
  frame <- expand.grid(ea = 1:10, hh = 1:10)
  frame$grp <- ifelse(frame$hh <= 5, "A", "B")
  frame$id <- seq_len(nrow(frame))
  phase1 <- sampling_design() |>
    stratify_by(grp) |>
    draw(n = c(A = 50, B = 20)) |>
    execute(frame, seed = 1)
  design <- sampling_design() |>
    add_stage("ea") |> cluster_by(ea) |> draw(n = 4) |>
    add_stage("hh") |> draw(n = 3)
  # Weights by hand: 10 / 4 EAs, then 3 of the EA's phase-1 households, times
  # the household's phase-1 weight.
  by_hand <- function(sample) {
    listed <- table(phase1$ea)[as.character(sample$ea)]
    10 / 4 * as.vector(listed) / 3 * ifelse(sample$grp == "B", 2.5, 1)
  }
  at_once <- execute(design, phase1, seed = 7)
  staged <- execute(execute(design, phase1, stages = 1, seed = 7), phase1,
                    seed = 8)
  expect_true(any(staged$grp == "B") && any(staged$grp == "A"))
  expect_equal(at_once$.weight, by_hand(at_once), tolerance = 1e-12)
  expect_equal(staged$.weight, by_hand(staged), tolerance = 1e-12)
})

test_that("a separate listing is refused when phase 1 weighted its parent's rows unequally", {
  frame <- expand.grid(ea = 1:10, hh = 1:10)
  frame$grp <- ifelse(frame$hh <= 5, "A", "B")
  phase1 <- sampling_design() |>
    stratify_by(grp) |>
    draw(n = c(A = 50, B = 20)) |>
    execute(frame, seed = 1)
  design <- sampling_design() |>
    add_stage("ea") |> cluster_by(ea) |> draw(n = 4) |>
    add_stage("hh") |> draw(n = 3)
  # The listing holds households phase 1 did not draw, and an EA's drawn
  # households weigh 1 or 2.5, so a listed household has no phase weight.
  expect_error(execute(design, phase1, frame, seed = 7),
               class = "samplyr_error_phase_key_ambiguous")
  staged <- execute(design, phase1, stages = 1, seed = 7)
  expect_error(execute(staged, frame, seed = 8),
               class = "samplyr_error_phase_key_ambiguous")
  # Phase 1 drew whole EAs, so every listed household takes its EA's
  # weight: 10 / 5, then 5 / 2, then 10 / 3.
  eas <- sampling_design() |> cluster_by(ea) |> draw(n = 5) |>
    execute(frame, seed = 1)
  design2 <- sampling_design() |>
    add_stage("ea") |> cluster_by(ea) |> draw(n = 2) |>
    add_stage("hh") |> draw(n = 3)
  for (sample in list(
    execute(design2, eas, frame, seed = 7),
    execute(execute(design2, eas, stages = 1, seed = 7), frame, seed = 8)
  )) {
    expect_identical(nrow(sample), 6L)
    expect_equal(sample$.weight, rep(10 / 5 * 5 / 2 * 10 / 3, 6),
                 tolerance = 1e-12)
  }
})

test_that("continuation matches single-run when downstream stage is deterministic", {
  frame <- make_invariant_frame()

  design <- sampling_design() |>
    add_stage(label = "EA") |>
      cluster_by(ea_id) |>
      draw(n = 10) |>
    add_stage(label = "Units") |>
      draw(n = 9999)

  # `n = 9999` is the take-everything idiom, so every stage-2 pool caps.
  full_run <- suppressWarnings(execute(design, frame, seed = 202))

  stage1 <- execute(design, frame, stages = 1, seed = 202)
  continued <- suppressWarnings(execute(stage1, frame, seed = 999))

  full_norm <- full_run |>
    dplyr::arrange(unit_id) |>
    dplyr::select(unit_id, ea_id, .weight, .weight_1, .weight_2)
  continued_norm <- continued |>
    dplyr::arrange(unit_id) |>
    dplyr::select(unit_id, ea_id, .weight, .weight_1, .weight_2)

  expect_equal(full_norm$unit_id, continued_norm$unit_id)
  expect_equal(full_norm$ea_id, continued_norm$ea_id)
  expect_equal(full_norm$.weight, continued_norm$.weight)
  expect_equal(full_norm$.weight_1, continued_norm$.weight_1)
  expect_equal(full_norm$.weight_2, continued_norm$.weight_2)
})
