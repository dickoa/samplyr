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

test_that("continuation matches single-run when downstream stage is deterministic", {
  frame <- make_invariant_frame()

  design <- sampling_design() |>
    add_stage(label = "EA") |>
      cluster_by(ea_id) |>
      draw(n = 10) |>
    add_stage(label = "Units") |>
      draw(n = 9999)

  full_run <- execute(design, frame, seed = 202)

  stage1 <- execute(design, frame, stages = 1, seed = 202)
  continued <- execute(stage1, frame, seed = 999)

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
