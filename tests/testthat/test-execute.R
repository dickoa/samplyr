test_frame <- function() {
  set.seed(123)
  data.frame(
    id = 1:1000,
    region = rep(c("North", "South", "East", "West"), each = 250),
    urban_rural = rep(c("Urban", "Rural"), 500),
    school_id = rep(1:100, each = 10),
    enrollment = rep(sample(100:500, 100, replace = TRUE), each = 10),
    value = rnorm(1000)
  )
}

test_that("execute() produces a tbl_sample", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  expect_s3_class(result, "tbl_sample")
  expect_true(is_tbl_sample(result))
})

test_that("execute() samples correct number of units", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100)
})

test_that("execute() adds weight columns", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  expect_true(".weight" %in% names(result))
  expect_true(".sample_id" %in% names(result))
})

test_that("execute() weights sum to population", {
  frame <- test_frame()
  N <- nrow(frame)

  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  # Weights should sum approximately to population
  expect_equal(sum(result$.weight), N, tolerance = 0.01)
})

test_that("execute() requires at least one frame", {
  d <- sampling_design() |> draw(n = 100)

  expect_error(
    execute(d),
    "data frame"
  )
})

test_that("execute() validates incomplete design", {
  d <- sampling_design() |>
    stratify_by(region)

  frame <- test_frame()

  expect_error(
    execute(d, frame, seed = 42),
    "missing.*draw"
  )
})

test_that("execute() validates required variables", {
  frame <- data.frame(id = 1:100, x = rnorm(100))

  expect_error(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = 10) |>
      execute(frame, seed = 42),
    "not found"
  )
})

test_that("execute() with stratification samples from all strata", {
  frame <- test_frame()

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 25) |>
    execute(frame, seed = 42)

  # Should have samples from all 4 regions
  expect_equal(length(unique(result$region)), 4)

  # Each stratum should have ~25 samples
  counts <- table(result$region)
  expect_true(all(counts == 25))
})

test_that("execute() with proportional allocation", {
  frame <- test_frame()

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 200) |>
    execute(frame, seed = 42)

  # Each stratum is 250/1000 = 25% of population
  # Should get ~50 from each stratum (200 * 0.25 = 50)
  counts <- table(result$region)
  expect_true(all(counts >= 45 & counts <= 55)) # Allow some rounding
})

test_that("execute() with seed is reproducible", {
  frame <- test_frame()
  design <- sampling_design() |> draw(n = 100)

  result1 <- execute(design, frame, seed = 42)
  result2 <- execute(design, frame, seed = 42)

  expect_equal(result1$id, result2$id)
})

test_that("execute() validates seed is a single integer", {
  frame <- test_frame()
  design <- sampling_design() |> draw(n = 100)

  expect_error(
    execute(design, frame, seed = 1.5),
    "single integer"
  )

  expect_error(
    execute(design, frame, seed = NA_real_),
    "single integer"
  )
})

test_that("execute() without seed gives different results", {
  frame <- test_frame()
  design <- sampling_design() |> draw(n = 100)

  result1 <- execute(design, frame)
  result2 <- execute(design, frame)

  # Very unlikely to get same sample
  expect_false(identical(result1$id, result2$id))
})

test_that("execute() with frac samples correct proportion", {
  frame <- test_frame()
  N <- nrow(frame)

  result <- sampling_design() |>
    draw(frac = 0.1) |>
    execute(frame, seed = 42)

  expected_n <- ceiling(N * 0.1)
  expect_equal(nrow(result), expected_n)
})

test_that("execute() with cluster_by samples clusters", {
  frame <- test_frame()

  result <- sampling_design() |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  # Should have exactly 20 schools
  n_schools <- length(unique(result$school_id))
  expect_equal(n_schools, 20)

  # Each school should have all 10 students
  school_counts <- table(result$school_id)
  expect_true(all(school_counts == 10))
})

test_that("execute() stores design in result", {
  frame <- test_frame()
  design <- sampling_design(title = "Test Survey") |>
    draw(n = 100)

  result <- execute(design, frame, seed = 42)

  stored_design <- get_design(result)
  expect_equal(stored_design$title, "Test Survey")
})

test_that("execute() with stages parameter", {
  frame <- test_frame()

  design <- sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    add_stage(label = "Students") |>
    draw(n = 5)

  # Execute only stage 1
  result <- execute(design, frame, stages = 1, seed = 42)

  stages_exec <- get_stages_executed(result)
  expect_equal(stages_exec, 1)
})

test_that("sample can continue to next stage", {
  frame <- test_frame()

  design <- sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    add_stage(label = "Students") |>
    draw(n = 5)

  # Execute stage 1
  stage1_result <- execute(design, frame, stages = 1, seed = 42)

  # Continue to stage 2
  stage2_result <- stage1_result |> execute(frame, seed = 43)

  stages_exec <- get_stages_executed(stage2_result)
  expect_equal(stages_exec, c(1, 2))

  # Should have 20 schools * 5 students = 100
  expect_equal(nrow(stage2_result), 100)
})

test_that("execute() with round='up' uses ceiling", {
  # Create a frame where 0.1 * N is not an integer
  frame <- data.frame(id = 1:105, x = rnorm(105))

  result <- sampling_design() |>
    draw(frac = 0.1, round = "up") |>
    execute(frame, seed = 42)

  # 105 * 0.1 = 10.5, ceiling = 11
  expect_equal(nrow(result), 11)
})

test_that("execute() with round='down' uses floor", {
  # Create a frame where 0.1 * N is not an integer
  frame <- data.frame(id = 1:105, x = rnorm(105))

  result <- sampling_design() |>
    draw(frac = 0.1, round = "down") |>
    execute(frame, seed = 42)

  # 105 * 0.1 = 10.5, floor = 10
  expect_equal(nrow(result), 10)
})

test_that("execute() with round='nearest' uses standard rounding", {
  # Test case where fractional part < 0.5
  frame1 <- data.frame(id = 1:102, x = rnorm(102))

  result1 <- sampling_design() |>
    draw(frac = 0.1, round = "nearest") |>
    execute(frame1, seed = 42)

  # 102 * 0.1 = 10.2, round = 10
  expect_equal(nrow(result1), 10)

  # Test case where fractional part > 0.5
  frame2 <- data.frame(id = 1:108, x = rnorm(108))

  result2 <- sampling_design() |>
    draw(frac = 0.1, round = "nearest") |>
    execute(frame2, seed = 42)

  # 108 * 0.1 = 10.8, round = 11
  expect_equal(nrow(result2), 11)
})

test_that("execute() with stratified sampling respects round parameter", {
  frame <- data.frame(
    id = 1:100,
    region = c(rep("A", 33), rep("B", 67))
  )

  # With round = "up" (ceiling)
  result_up <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = 0.1, round = "up") |>
    execute(frame, seed = 42)

  # A: 33 * 0.1 = 3.3, ceiling = 4
  # B: 67 * 0.1 = 6.7, ceiling = 7
  # Total = 11
  expect_equal(nrow(result_up), 11)

  # With round = "nearest"
  result_nearest <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = 0.1, round = "nearest") |>
    execute(frame, seed = 42)

  # A: 33 * 0.1 = 3.3, round = 3
  # B: 67 * 0.1 = 6.7, round = 7
  # Total = 10
  expect_equal(nrow(result_nearest), 10)
})

test_that("execute() with round='down' ensures minimum of 1 per stratum", {
  # Create frame with tiny stratum
  frame <- data.frame(
    id = 1:110,
    region = c(rep("A", 5), rep("B", 105)) # A is tiny
  )

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = 0.1, round = "down") |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # A: 5 * 0.1 = 0.5, floor would be 0, but minimum is 1
  expect_true(counts["A"] >= 1)

  # B: 105 * 0.1 = 10.5, floor = 10
  expect_equal(as.integer(counts["B"]), 10)
})

test_that("single-stage tracks per-stage weight", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  # Stagewise weight column should exist
  expect_true(".weight_1" %in% names(result))

  # For single stage, .weight_1 should equal .weight
  expect_equal(result$.weight_1, result$.weight)
})

test_that("multi-stage compound weight = product of stage weights", {
  frame <- test_frame()

  result <- sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    add_stage(label = "Students") |>
    draw(n = 5) |>
    execute(frame, seed = 42)

  # Stagewise weight columns should exist
  expect_true(".weight_1" %in% names(result))
  expect_true(".weight_2" %in% names(result))

  # Compound weight = product of stagewise weights
  expect_equal(result$.weight, result$.weight_1 * result$.weight_2)
})

test_that("multi-stage with separate execution maintains weight compounding", {
  frame <- test_frame()

  design <- sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    add_stage(label = "Students") |>
    draw(n = 5)

  # Execute stage 1 only
  stage1_result <- execute(design, frame, stages = 1, seed = 42)

  # Stage 1 should have .weight_1
  expect_true(".weight_1" %in% names(stage1_result))
  expect_true(all(stage1_result$.weight > 0))

  # Continue to stage 2
  final_result <- stage1_result |> execute(frame, seed = 43)

  # Should have both stagewise weight columns
  expect_true(".weight_1" %in% names(final_result))
  expect_true(".weight_2" %in% names(final_result))

  # Compound weight = product of stagewise weights
  expect_equal(
    final_result$.weight,
    final_result$.weight_1 * final_result$.weight_2
  )
})

test_that("stratified sampling tracks stage weight", {
  frame <- test_frame()

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 200) |>
    execute(frame, seed = 42)

  # All weights positive
  expect_true(all(result$.weight > 0))

  # Stagewise weight column should exist
  expect_true(".weight_1" %in% names(result))
})

test_that("cluster sampling tracks stage weight", {
  frame <- test_frame()

  result <- sampling_design() |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  # All weights positive
  expect_true(all(result$.weight > 0))

  # Stagewise weight column should exist
  expect_true(".weight_1" %in% names(result))
})

test_that("two-stage with cluster_by at both stages samples within groups", {
  frame <- test_frame()

  result <- sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 10) |>
    add_stage(label = "Students") |>
    cluster_by(id) |>
    draw(n = 3) |>
    execute(frame, seed = 42)

  # 10 schools * 3 students = 30

  expect_equal(nrow(result), 30)
  expect_equal(length(unique(result$school_id)), 10)

  # Each school should have exactly 3 students
  students_per_school <- table(result$school_id)
  expect_true(all(students_per_school == 3))
})

test_that("two-stage cluster_by at both stages matches omitting cluster_by at stage 2", {
  frame <- test_frame()

  result_with <- sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 10) |>
    add_stage(label = "Students") |>
    cluster_by(id) |>
    draw(n = 3) |>
    execute(frame, seed = 42)

  result_without <- sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 10) |>
    add_stage(label = "Students") |>
    draw(n = 3) |>
    execute(frame, seed = 42)

  # Both should produce the same number of rows
  expect_equal(nrow(result_with), nrow(result_without))

  # Both should produce the same schools and same count per school
  expect_equal(
    sort(unique(result_with$school_id)),
    sort(unique(result_without$school_id))
  )
  expect_true(all(table(result_with$school_id) == 3))
  expect_true(all(table(result_without$school_id) == 3))
})

test_that("two-stage cluster_by at both stages has correct compound weights", {
  frame <- test_frame()

  result <- sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    add_stage(label = "Students") |>
    cluster_by(id) |>
    draw(n = 5) |>
    execute(frame, seed = 42)

  expect_true(".weight_1" %in% names(result))
  expect_true(".weight_2" %in% names(result))
  expect_equal(result$.weight, result$.weight_1 * result$.weight_2)
})

test_that("three-stage with cluster_by at all stages samples correctly", {
  # Region (4) -> School (25 per region) -> Student (10 per school)
  set.seed(99)
  frame <- data.frame(
    region = rep(c("N", "S", "E", "W"), each = 250),
    school_id = rep(1:100, each = 10),
    student_id = 1:1000,
    score = rnorm(1000)
  )

  result <- sampling_design() |>
    add_stage(label = "Regions") |>
    cluster_by(region) |>
    draw(n = 2) |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 3) |>
    add_stage(label = "Students") |>
    draw(n = 4) |>
    execute(frame, seed = 42)

  # 2 regions * 3 schools * 4 students = 24
  expect_equal(nrow(result), 24)
  expect_equal(length(unique(result$region)), 2)
  expect_equal(length(unique(result$school_id)), 6)

  # Each school should have exactly 4 students
  expect_true(all(table(result$school_id) == 4))

  # Each region should have exactly 3 schools
  schools_per_region <- tapply(
    result$school_id, result$region, function(x) length(unique(x))
  )
  expect_true(all(schools_per_region == 3))

  # Compound weight = product of all stage weights
  expect_equal(
    result$.weight,
    result$.weight_1 * result$.weight_2 * result$.weight_3
  )
})

three_stage_design <- function() {
  sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 5) |>
    add_stage(label = "Classes") |>
    draw(n = 3) |>
    add_stage(label = "Students") |>
    draw(n = 2)
}

three_stage_frame <- function() {
  data.frame(
    school_id = rep(1:10, each = 20),
    class_id = rep(1:40, each = 5),
    student_id = 1:200,
    score = rnorm(200)
  )
}

test_that("execute(design) with stages = 2 errors (must start at 1)", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  expect_error(
    execute(design, frame, stages = 2, seed = 1),
    "must start at stage 1"
  )
})

test_that("execute(design) with stages = c(2, 3) errors (must start at 1)", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  expect_error(
    execute(design, frame, stages = c(2, 3), seed = 1),
    "must start at stage 1"
  )
})

test_that("execute(design) with stages = 3 errors (must start at 1)", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  expect_error(
    execute(design, frame, stages = 3, seed = 1),
    "must start at stage 1"
  )
})

test_that("execute(design) with stages = c(1, 3) errors (gap)", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  expect_error(
    execute(design, frame, stages = c(1, 3), seed = 1),
    "contiguous"
  )
})

test_that("execute(design) with stages = 1 works (partial)", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  result <- execute(design, frame, stages = 1, seed = 42)
  expect_s3_class(result, "tbl_sample")
  expect_equal(get_stages_executed(result), 1L)
})

test_that("execute(design) with stages = c(1, 2) works (partial contiguous)", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  result <- execute(design, frame, stages = c(1, 2), seed = 42)
  expect_s3_class(result, "tbl_sample")
  expect_equal(get_stages_executed(result), c(1L, 2L))
})

test_that("execute(design) with stages = NULL runs all stages", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  expect_warning(
    result <- execute(design, frame, seed = 42),
    "No design-driven columns"
  )
  expect_s3_class(result, "tbl_sample")
  expect_equal(get_stages_executed(result), 1:3)
})

test_that("execute(design) with stages = 0 errors", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  expect_error(
    execute(design, frame, stages = 0, seed = 1),
    "stages"
  )
})

test_that("execute(design) with non-integer stages errors", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  expect_error(
    execute(design, frame, stages = 1.5, seed = 1),
    "stages"
  )
})

test_that("execute(design) with stages = 4 errors (exceeds n_stages)", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  expect_error(
    execute(design, frame, stages = 4, seed = 1),
    "stages"
  )
})

test_that("continuation with stages = 3 after stage 1 errors (skips stage 2)", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  s1 <- execute(design, frame, stages = 1, seed = 42)

  expect_error(
    execute(s1, frame, stages = 3, seed = 2),
    "must continue from stage 2"
  )
})

test_that("continuation with stages = c(2, 3) after stages c(1, 2) errors (already done)", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  s12 <- execute(design, frame, stages = c(1, 2), seed = 42)

  expect_error(
    execute(s12, frame, stages = c(2, 3), seed = 2),
    "already executed"
  )
})

test_that("continuation with stages = c(2, 4) errors on out-of-bounds", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  s1 <- execute(design, frame, stages = 1, seed = 42)

  expect_error(
    execute(s1, frame, stages = c(2, 4), seed = 2),
    "stages"
  )
})

test_that("continuation with stages = 2 after stage 1 works", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  s1 <- execute(design, frame, stages = 1, seed = 42)
  s2 <- execute(s1, frame, stages = 2, seed = 2)

  expect_s3_class(s2, "tbl_sample")
  expect_equal(get_stages_executed(s2), c(1L, 2L))
})

test_that("continuation with stages = c(2, 3) after stage 1 works", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  s1 <- execute(design, frame, stages = 1, seed = 42)
  expect_warning(
    s23 <- execute(s1, frame, stages = c(2, 3), seed = 2),
    "No design-driven columns"
  )

  expect_s3_class(s23, "tbl_sample")
  expect_equal(get_stages_executed(s23), 1:3)
})

test_that("continuation with stages = NULL picks up remaining stages", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  s1 <- execute(design, frame, stages = 1, seed = 42)
  expect_warning(
    s_rest <- execute(s1, frame, seed = 2),
    "No design-driven columns"
  )

  expect_s3_class(s_rest, "tbl_sample")
  expect_equal(get_stages_executed(s_rest), 1:3)
})

test_that("chained continuation 1 -> 2 -> 3 works", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  s1 <- execute(design, frame, stages = 1, seed = 42)
  s2 <- execute(s1, frame, stages = 2, seed = 2)
  expect_warning(
    s3 <- execute(s2, frame, stages = 3, seed = 3),
    "No design-driven columns"
  )

  expect_s3_class(s3, "tbl_sample")
  expect_equal(get_stages_executed(s3), 1:3)
})

test_that("execute(design) with stages = c(2, 1) is sorted to c(1, 2) and works", {
  design <- three_stage_design()
  frame <- three_stage_frame()

  result <- execute(design, frame, stages = c(2, 1), seed = 42)
  expect_s3_class(result, "tbl_sample")
  expect_equal(get_stages_executed(result), c(1L, 2L))
})

test_that("two-phase SRS -> SRS: weights compound correctly", {
  frame <- data.frame(id = 1:1000, x = rnorm(1000))

  phase1 <- sampling_design() |>
    draw(n = 200) |>
    execute(frame, seed = 42)

  expect_equal(nrow(phase1), 200)
  expect_equal(unique(phase1$.weight), 5) # 1000/200

  phase2 <- sampling_design() |>
    draw(n = 50) |>
    execute(phase1, seed = 123)

  expect_s3_class(phase2, "tbl_sample")
  expect_equal(nrow(phase2), 50)

  # w1 * w2 = (1000/200) * (200/50) = 5 * 4 = 20
  expect_equal(unique(phase2$.weight), 20)
  expect_equal(sum(phase2$.weight), 1000)
})

test_that("stratified phase 1 -> SRS phase 2: heterogeneous weights compound", {
  frame <- data.frame(
    id = 1:200,
    region = c(rep("A", 80), rep("B", 120)),
    x = rnorm(200)
  )

  phase1 <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  w_A <- unique(phase1$.weight[phase1$region == "A"])
  w_B <- unique(phase1$.weight[phase1$region == "B"])
  expect_equal(w_A, 4)  # 80/20
  expect_equal(w_B, 6)  # 120/20

  phase2 <- sampling_design() |>
    draw(n = 10) |>
    execute(phase1, seed = 123)

  expect_equal(nrow(phase2), 10)

  # Phase 2 SRS weight = 40/10 = 4
  for (i in seq_len(nrow(phase2))) {
    expected_w <- ifelse(phase2$region[i] == "A", 4 * 4, 6 * 4)
    expect_equal(phase2$.weight[i], expected_w)
  }

  expect_equal(sum(phase2$.weight), 200, tolerance = 1)
})

test_that("phase 1 -> stratified phase 2", {
  frame <- data.frame(
    id = 1:500,
    region = rep(c("A", "B"), each = 250),
    x = rnorm(500)
  )

  phase1 <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  phase2 <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10) |>
    execute(phase1, seed = 123)

  expect_equal(nrow(phase2), 20)

  # Phase 1 weight = 500/100 = 5 for all
  expect_true(all(phase2$.weight >= 5))
  expect_equal(phase2$.weight, phase2$.weight_1 * 5)
})

test_that("phase 1 -> multi-stage phase 2: three-level compounding", {
  frame <- data.frame(
    cluster_id = rep(1:20, each = 50),
    id = 1:1000,
    x = rnorm(1000)
  )

  # Cluster phase 1 selects 10 of 20 clusters (all units within selected clusters)
  phase1 <- sampling_design() |>
    cluster_by(cluster_id) |>
    draw(n = 10) |>
    execute(frame, seed = 42)

  expect_equal(nrow(phase1), 500)
  phase1_weight <- unique(phase1$.weight)
  expect_equal(phase1_weight, 2) # 20/10

  # Phase 2: select 5 clusters, then 3 units within each
  phase2 <- sampling_design() |>
    add_stage(label = "Clusters") |>
    cluster_by(cluster_id) |>
    draw(n = 5) |>
    add_stage(label = "Units") |>
    draw(n = 3) |>
    execute(phase1, seed = 123)

  expect_s3_class(phase2, "tbl_sample")
  expect_equal(nrow(phase2), 15)

  # Phase 2 stage weights: .weight_1 = 10/5 = 2, .weight_2 = 50/3
  # Overall: .weight = phase1_weight * .weight_1 * .weight_2
  expect_equal(
    phase2$.weight,
    phase2$.weight_1 * phase2$.weight_2 * phase1_weight
  )
  expect_equal(sum(phase2$.weight), 1000, tolerance = 50)
})

test_that("clustered phase 1 -> phase 2: heterogeneous cluster weights compound", {
  frame <- data.frame(
    cluster_id = rep(1:50, each = 20),
    id = 1:1000,
    x = rnorm(1000)
  )

  phase1 <- sampling_design() |>
    cluster_by(cluster_id) |>
    draw(n = 10) |>
    execute(frame, seed = 42)

  expect_equal(unique(phase1$.weight), 5) # 50/10

  phase2 <- sampling_design() |>
    draw(n = 50) |>
    execute(phase1, seed = 123)

  expect_equal(nrow(phase2), 50)
  expect_equal(unique(phase2$.weight), 20) # 5 * (200/50)
  expect_equal(sum(phase2$.weight), 1000)
})

test_that("dplyr mutations on phase 1 preserve tbl_sample class for phase 2", {
  frame <- data.frame(id = 1:200, x = rnorm(200))

  phase1 <- sampling_design() |>
    draw(n = 50) |>
    execute(frame, seed = 42)

  phase1_updated <- phase1 |>
    dplyr::mutate(screening_score = rnorm(50))

  expect_s3_class(phase1_updated, "tbl_sample")

  phase2 <- sampling_design() |>
    draw(n = 20) |>
    execute(phase1_updated, seed = 123)

  expect_s3_class(phase2, "tbl_sample")
  expect_equal(nrow(phase2), 20)
  expect_true("screening_score" %in% names(phase2))

  # 200/50 = 4, 50/20 = 2.5, compound = 10
  expect_equal(unique(phase2$.weight), 10)
})

test_that("internal columns from phase 1 do not collide with phase 2", {
  frame <- data.frame(id = 1:200, x = rnorm(200))

  phase1 <- sampling_design() |>
    draw(n = 50) |>
    execute(frame, seed = 42)

  expect_true(".weight_1" %in% names(phase1))
  expect_true(".fpc_1" %in% names(phase1))

  phase2 <- sampling_design() |>
    draw(n = 20) |>
    execute(phase1, seed = 123)

  # No duplicate suffixed columns (.weight_1.x / .weight_1.y)
  expect_equal(sum(grepl("^\\.weight_1", names(phase2))), 1)
  expect_equal(sum(grepl("^\\.fpc_1", names(phase2))), 1)
})

test_that("as.data.frame on phase 1 disables multiphase compounding", {
  frame <- data.frame(id = 1:200, x = rnorm(200))

  phase1 <- sampling_design() |>
    draw(n = 50) |>
    execute(frame, seed = 42)

  phase1_plain <- as.data.frame(phase1)
  expect_false(is_tbl_sample(phase1_plain))

  phase2 <- sampling_design() |>
    draw(n = 20) |>
    execute(phase1_plain, seed = 123)

  # No compounding — just phase 2 weight (50/20 = 2.5)
  expect_equal(unique(phase2$.weight), 2.5)
})

test_that("phase 2 metadata records previous phase info", {
  frame <- data.frame(id = 1:200, x = rnorm(200))

  phase1 <- sampling_design() |>
    draw(n = 50) |>
    execute(frame, seed = 42)

  phase2 <- sampling_design() |>
    draw(n = 20) |>
    execute(phase1, seed = 123)

  meta <- attr(phase2, "metadata")
  expect_false(is.null(meta$prev_phase))
  expect_true(is_sampling_design(meta$prev_phase$design))
})

test_that("plain data frame as frame does not trigger multiphase", {
  frame <- data.frame(id = 1:100, x = rnorm(100))

  result <- sampling_design() |>
    draw(n = 20) |>
    execute(frame, seed = 42)

  expect_null(attr(result, "metadata")$prev_phase)
})
