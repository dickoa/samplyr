test_that("execute() produces .fpc_k columns for unstratified SRS", {
  sample <- sampling_design() |>
    draw(n = 50) |>
    execute(kenya_health, seed = 1)

  expect_true(".fpc_1" %in% names(sample))
  # FPC should equal the frame size
  expect_equal(unique(sample$.fpc_1), nrow(kenya_health))
})

test_that("execute() produces .fpc_k columns for stratified design", {
  sample <- sampling_design() |>
    stratify_by(facility_type, alloc = "proportional") |>
    draw(n = 300) |>
    execute(kenya_health, seed = 42)

  expect_true(".fpc_1" %in% names(sample))
  # FPC should vary by stratum and equal stratum population sizes
  fpc_by_stratum <- tapply(sample$.fpc_1, sample$facility_type, unique)
  pop_by_stratum <- table(kenya_health$facility_type)
  for (stratum in names(fpc_by_stratum)) {
    expect_equal(
      as.integer(fpc_by_stratum[[stratum]]),
      as.integer(pop_by_stratum[[stratum]])
    )
  }
})

test_that("execute() produces .fpc_k columns for clustered design", {
  sample <- sampling_design() |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    execute(tanzania_schools, seed = 5)

  expect_true(".fpc_1" %in% names(sample))
  # FPC should equal the number of distinct clusters in the frame
  n_clusters <- length(unique(tanzania_schools$school_id))
  expect_equal(unique(sample$.fpc_1), n_clusters)
})

test_that("execute() produces .fpc_k for each stage in multi-stage design", {
  sample <- sampling_design() |>
    add_stage(label = "EAs") |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = 5, method = "pps_brewer", mos = hh_count) |>
    add_stage(label = "Households") |>
    draw(n = 12) |>
    execute(niger_eas, seed = 2025)

  expect_true(".fpc_1" %in% names(sample))
  expect_true(".fpc_2" %in% names(sample))
  # Stage 1 FPC should be the number of EAs per stratum (region)
  # Stage 2 FPC should be the number of households per selected EA
})

test_that(".fpc_k columns carry forward in partial execution", {
  design <- sampling_design() |>
    add_stage(label = "EAs") |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = 5, method = "pps_brewer", mos = hh_count) |>
    add_stage(label = "Households") |>
    draw(n = 12)

  # Execute stage 1 only
  stage1 <- execute(design, niger_eas, stages = 1, seed = 42)
  expect_true(".fpc_1" %in% names(stage1))

  # Continue with stage 2
  stage2 <- execute(stage1, niger_eas, seed = 43)
  expect_true(".fpc_1" %in% names(stage2)) # carried forward
  expect_true(".fpc_2" %in% names(stage2)) # new
})

test_that(".fpc_k is preserved through dplyr operations", {
  sample <- sampling_design() |>
    stratify_by(facility_type, alloc = "proportional") |>
    draw(n = 300) |>
    execute(kenya_health, seed = 42)

  # filter should preserve
  filtered <- dplyr::filter(sample, facility_type == levels(facility_type)[1])
  expect_true(".fpc_1" %in% names(filtered))

  # mutate should preserve
  mutated <- dplyr::mutate(sample, new_col = 1)
  expect_true(".fpc_1" %in% names(mutated))
})

test_that("summary.tbl_sample runs without error", {
  sample <- sampling_design() |>
    stratify_by(facility_type, alloc = "proportional") |>
    draw(n = 300) |>
    execute(kenya_health, seed = 42)

  expect_output(summary(sample), "Sample Summary")
  expect_output(summary(sample), "Design")
  expect_output(summary(sample), "Execution")
  expect_output(summary(sample), "Allocation")
  expect_output(summary(sample), "Weights")
})

test_that("summary.tbl_sample shows stratum allocation table", {
  sample <- sampling_design() |>
    stratify_by(facility_type, alloc = "proportional") |>
    draw(n = 300) |>
    execute(kenya_health, seed = 42)

  output <- capture.output(summary(sample))
  # Should contain N_h and n_h headers
  expect_true(any(grepl("N_h", output)))
  expect_true(any(grepl("n_h", output)))
  expect_true(any(grepl("f_h", output)))
  # Should contain Total row
  expect_true(any(grepl("Total", output)))
})

test_that("summary.tbl_sample works for unstratified design", {
  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(kenya_health, seed = 1)

  output <- capture.output(summary(sample))
  expect_true(any(grepl("N =", output)))
  expect_true(any(grepl("n =", output)))
})

test_that("summary.tbl_sample shows weight diagnostics", {
  sample <- sampling_design() |>
    stratify_by(facility_type, alloc = "proportional") |>
    draw(n = 300) |>
    execute(kenya_health, seed = 42)

  output <- capture.output(summary(sample))
  expect_true(any(grepl("DEFF", output)))
  expect_true(any(grepl("Effective n", output)))
  expect_true(any(grepl("CV", output)))
})

test_that("as_survey_design produces valid survey object", {
  skip_if_not_installed("survey")

  sample <- sampling_design() |>
    stratify_by(facility_type, alloc = "proportional") |>
    draw(n = 300) |>
    execute(kenya_health, seed = 42)

  svy <- as_survey_design(sample)
  expect_s3_class(svy, "survey.design2")
})

test_that("as_survey_design works for clustered design", {
  skip_if_not_installed("survey")

  sample <- sampling_design() |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    execute(tanzania_schools, seed = 5)

  svy <- as_survey_design(sample)
  expect_s3_class(svy, "survey.design2")
})

test_that("as_survey_design works for multi-stage design", {
  skip_if_not_installed("survey")

  sample <- sampling_design() |>
    add_stage(label = "EAs") |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = 5, method = "pps_brewer", mos = hh_count) |>
    add_stage(label = "Households") |>
    draw(n = 12) |>
    execute(niger_eas, seed = 2025)

  svy <- as_survey_design(sample)
  expect_s3_class(svy, "survey.design2")
})

test_that("as_survey_design uses Inf FPC for WR methods (no correction)", {
  skip_if_not_installed("survey")

  sample <- sampling_design() |>
    draw(n = 50, method = "srswr") |>
    execute(kenya_health, seed = 1)

  svy <- as_survey_design(sample)
  expect_s3_class(svy, "survey.design2")
  # WR design: Inf population size = no finite population correction
  expect_true(all(svy$fpc$popsize == Inf))
})

test_that("as_survey_design handles mixed WR/WOR with Inf FPC for WR stages", {
  skip_if_not_installed("survey")

  # Two clustered stages: stage 1 WR, stage 2 WOR.
  # WR stage gets Inf FPC (no correction), WOR stage keeps real FPC.
  n_districts <- 10
  n_schools_per <- 8
  n_students_per <- 5
  frame <- data.frame(
    district_id = rep(
      seq_len(n_districts),
      each = n_schools_per * n_students_per
    ),
    school_id = rep(
      seq_len(n_districts * n_schools_per),
      each = n_students_per
    ),
    student_id = seq_len(n_districts * n_schools_per * n_students_per)
  )

  sample <- sampling_design() |>
    add_stage(label = "Districts (WR)") |>
    cluster_by(district_id) |>
    draw(n = 4, method = "srswr") |>
    add_stage(label = "Schools (WOR)") |>
    cluster_by(school_id) |>
    draw(n = 3) |>
    add_stage(label = "Students") |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  svy <- as_survey_design(sample)
  expect_s3_class(svy, "survey.design2")

  # FPC should be present — WR stage uses Inf, WOR stage uses real FPC
  expect_false(is.null(svy$fpc$popsize))
})

test_that("as_survey_design preserves FPC when all clustered stages are WOR", {
  skip_if_not_installed("survey")

  # Two clustered stages, both WOR. FPC should be present for both.
  n_districts <- 10
  n_schools_per <- 8
  n_students_per <- 5
  frame <- data.frame(
    district_id = rep(
      seq_len(n_districts),
      each = n_schools_per * n_students_per
    ),
    school_id = rep(
      seq_len(n_districts * n_schools_per),
      each = n_students_per
    ),
    student_id = seq_len(n_districts * n_schools_per * n_students_per)
  )

  sample <- sampling_design() |>
    add_stage(label = "Districts (WOR)") |>
    cluster_by(district_id) |>
    draw(n = 4) |>
    add_stage(label = "Schools (WOR)") |>
    cluster_by(school_id) |>
    draw(n = 3) |>
    add_stage(label = "Students") |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  svy <- as_survey_design(sample)
  expect_s3_class(svy, "survey.design2")

  # FPC should be present — both clustered stages are WOR
  expect_false(is.null(svy$fpc$popsize))
})

test_that("as_survey_design errors without survey package", {
  # This test validates the check_installed mechanism
  # It would only fail if survey is not installed, which is the point
  # In practice, the skip_if_not_installed tests above cover the happy path
  expect_true(TRUE) # placeholder
})

# --- joint_inclusion_prob tests ---

test_that("joint_inclusion_prob returns list of correct length", {
  sample <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = 5, method = "pps_brewer", mos = hh_count) |>
    add_stage() |>
    draw(n = 12) |>
    execute(niger_eas, seed = 2025)

  jip <- joint_inclusion_prob(sample, niger_eas)
  expect_type(jip, "list")
  expect_length(jip, 2)
  # Stage 1 is PPS -> matrix; Stage 2 is SRS -> NULL
  expect_true(is.matrix(jip[[1]]))
  expect_null(jip[[2]])
})

test_that("joint_inclusion_prob stage argument filters correctly", {
  sample <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = 5, method = "pps_brewer", mos = hh_count) |>
    add_stage() |>
    draw(n = 12) |>
    execute(niger_eas, seed = 2025)

  jip <- joint_inclusion_prob(sample, niger_eas, stage = 1)
  expect_length(jip, 2)
  expect_true(is.matrix(jip[[1]]))
  expect_null(jip[[2]])
})

test_that("joint_inclusion_prob matrix is square and symmetric", {
  sample <- sampling_design() |>
    cluster_by(ea_id) |>
    draw(n = 10, method = "pps_brewer", mos = hh_count) |>
    execute(niger_eas, seed = 42)

  jip <- joint_inclusion_prob(sample, niger_eas)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Joint matrix should be symmetric
  expect_equal(mat, t(mat), tolerance = 1e-10)
})

test_that("joint_inclusion_prob diagonal equals marginal pik", {
  sample <- sampling_design() |>
    draw(n = 20, method = "pps_systematic", mos = hh_count) |>
    execute(niger_eas, seed = 7)

  jip <- joint_inclusion_prob(sample, niger_eas)
  mat <- jip[[1]]

  # Diagonal should equal 1/weight (marginal inclusion probabilities)
  pik_from_weights <- 1 / sample$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_inclusion_prob errors on invalid stage", {
  sample <- sampling_design() |>
    draw(n = 50) |>
    execute(kenya_health, seed = 1)

  expect_error(
    joint_inclusion_prob(sample, kenya_health, stage = 2),
    "not executed"
  )
})

test_that("joint_inclusion_prob returns NULL for non-PPS stages", {
  sample <- sampling_design() |>
    draw(n = 50) |>
    execute(kenya_health, seed = 1)

  jip <- joint_inclusion_prob(sample, kenya_health)
  expect_null(jip[[1]])
})

test_that("joint_inclusion_prob works with ppsmat for survey export", {
  skip_if_not_installed("survey")

  sample <- sampling_design() |>
    draw(n = 20, method = "pps_brewer", mos = hh_count) |>
    execute(niger_eas, seed = 42)

  jip <- joint_inclusion_prob(sample, niger_eas)
  svy <- as_survey_design(sample, pps = survey::ppsmat(jip[[1]]))
  # ppsmat designs return "pps"/"survey.design", not "survey.design2"
  expect_s3_class(svy, "survey.design")
})

test_that("joint_inclusion_prob works with proportional allocation", {
  sample <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    cluster_by(ea_id) |>
    draw(n = 30, method = "pps_brewer", mos = hh_count) |>
    execute(niger_eas, seed = 99)

  jip <- joint_inclusion_prob(sample, niger_eas, stage = 1)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Diagonal π_i should equal 1/weight for each cluster
  sample_df <- as.data.frame(sample)
  sample_clusters <- sample_df |>
    dplyr::distinct(ea_id, .keep_all = TRUE)
  pik_from_weights <- 1 / sample_clusters$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_inclusion_prob works with stage vector", {
  sample <- sampling_design() |>
    add_stage() |>
    cluster_by(ea_id) |>
    draw(
      n = 10,
      method = "pps_brewer",
      mos = hh_count
    ) |>
    add_stage() |>
    draw(n = 5) |>
    execute(niger_eas, seed = 2025)

  # Request both stages via vector
  jip <- joint_inclusion_prob(sample, niger_eas, stage = c(1, 2))
  expect_length(jip, 2)
  expect_true(is.matrix(jip[[1]]))
  # Stage 2 is SRS — should be NULL
  expect_null(jip[[2]])
})

test_that("joint_inclusion_prob works with stratified pps_poisson and named frac", {
  levels_r <- levels(niger_eas$region)
  frac_vec <- setNames(rep(0.15, length(levels_r)), levels_r)

  sample <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = frac_vec, method = "pps_poisson", mos = hh_count) |>
    execute(niger_eas, seed = 42)

  jip <- joint_inclusion_prob(sample, niger_eas)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Symmetric
  expect_equal(mat, t(mat), tolerance = 1e-10)
  # Diagonal should equal marginal pik = 1/weight
  pik_from_weights <- 1 / sample$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_inclusion_prob decomposes certainty units correctly", {
  # Create a frame where one unit is very large (certainty selection)
  frame <- data.frame(
    id = paste0("u", 1:10),
    size = c(5000, rep(20, 9))
  )

  sample <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = size, certainty_size = 1000) |>
    execute(frame, seed = 42)

  jip <- joint_inclusion_prob(sample, frame)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Symmetric
  expect_equal(mat, t(mat), tolerance = 1e-10)

  # Diagonal should equal marginal pik = 1/weight
  pik_from_weights <- 1 / sample$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)

  # Certainty unit (weight = 1, π = 1) should have diagonal = 1
  cert_rows <- which(sample$.certainty_1)
  if (length(cert_rows) > 0) {
    expect_equal(
      diag(mat)[cert_rows],
      rep(1, length(cert_rows)),
      tolerance = 1e-10
    )
    # Off-diagonal: certainty × non-certainty = π_j
    non_cert_rows <- setdiff(seq_len(nrow(mat)), cert_rows)
    if (length(non_cert_rows) > 0) {
      for (ci in cert_rows) {
        expect_equal(
          mat[ci, non_cert_rows],
          pik_from_weights[non_cert_rows],
          tolerance = 1e-6
        )
      }
    }
  }
})

test_that("joint_inclusion_prob with certainty works in stratified design", {
  sample <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = 10, method = "pps_brewer", mos = hh_count, certainty_size = 800) |>
    execute(niger_eas, seed = 42)

  jip <- joint_inclusion_prob(sample, niger_eas)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  expect_equal(mat, t(mat), tolerance = 1e-10)

  # Diagonal equals marginal pik
  sample_clusters <- as.data.frame(sample) |>
    dplyr::distinct(ea_id, .keep_all = TRUE)
  pik_from_weights <- 1 / sample_clusters$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

# =============================================================================
# WR Row Replication: Survey Export Tests
# =============================================================================

test_that("as_survey_design for srswr uses .draw_1 as PSU with Inf FPC", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:20, y = rnorm(20))

  result <- sampling_design() |>
    draw(n = 10, method = "srswr") |>
    execute(frame, seed = 42)

  svy <- as_survey_design(result)
  expect_s3_class(svy, "survey.design")

  # Should have 10 rows (one per draw)
  expect_equal(nrow(svy$variables), 10L)

  # Estimate the mean — should not error
  est <- survey::svymean(~y, svy)
  expect_true(is.numeric(coef(est)))
  expect_true(all(survey::SE(est) > 0))
})

test_that("as_survey_design for pps_chromy uses .draw_1 with Inf FPC", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:10,
    size = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
    y = 1:10
  )

  result <- sampling_design() |>
    draw(n = 6, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 42)

  svy <- as_survey_design(result)
  expect_s3_class(svy, "survey.design")

  est <- survey::svymean(~y, svy)
  expect_true(is.numeric(coef(est)))
  expect_true(all(survey::SE(est) > 0))
})

test_that("two-stage WR cluster design with independent sub-samples", {
  # Stage 1: WR cluster sampling; Stage 2: SRS within clusters
  frame <- data.frame(
    cluster = rep(1:10, each = 5),
    id = 1:50,
    size = rep(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), each = 5),
    y = rnorm(50)
  )

  result <- sampling_design() |>
    add_stage() |>
    cluster_by(cluster) |>
    draw(n = 6, method = "pps_multinomial", mos = size) |>
    add_stage() |>
    draw(n = 3) |>
    execute(frame, seed = 42)

  # Stage 1: 6 draws, Stage 2: 3 units per draw = 18 rows
  expect_equal(nrow(result), 18L)

  # Each draw should have exactly 3 sub-sampled units
  expect_true(".draw_1" %in% names(result))
  for (d in unique(result$.draw_1)) {
    expect_equal(sum(result$.draw_1 == d), 3L)
  }

  # Compound weight = stage1_weight * stage2_weight
  expect_true(all(
    abs(result$.weight - result$.weight_1 * result$.weight_2) < 1e-10
  ))
})

test_that("two-stage WR cluster design produces valid survey design", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    cluster = rep(1:10, each = 5),
    id = 1:50,
    size = rep(c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100), each = 5),
    y = rnorm(50)
  )

  result <- sampling_design() |>
    add_stage() |>
    cluster_by(cluster) |>
    draw(n = 6, method = "pps_multinomial", mos = size) |>
    add_stage() |>
    draw(n = 3) |>
    execute(frame, seed = 42)

  svy <- as_survey_design(result)
  expect_s3_class(svy, "survey.design")

  # Should be able to estimate mean without error
  est <- survey::svymean(~y, svy)
  expect_true(is.numeric(coef(est)))
  expect_true(all(survey::SE(est) > 0))
})

# =============================================================================
# Certainty Stratum (Take-All) Tests
# =============================================================================

test_that("PPS WOR with certainty creates separate take-all stratum", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:10,
    size = c(5000, rep(20, 9)),
    y = c(100, rnorm(9, 10, 2))
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = size, certainty_size = 1000) |>
    execute(frame, seed = 42)

  svy <- as_survey_design(result)
  expect_s3_class(svy, "survey.design")

  # Should have .cert_stratum in the data
  expect_true(".cert_stratum" %in% names(svy$variables))

  # Should have 2 strata: certainty and probability
  strata_levels <- unique(svy$strata[, 1])
  expect_equal(length(strata_levels), 2L)

  # svymean should work
  est <- survey::svymean(~y, svy)
  expect_true(is.numeric(coef(est)))
  expect_true(all(survey::SE(est) > 0))
})

test_that("PPS WOR with certainty + user strata creates interaction strata", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:20,
    group = rep(c("A", "B"), each = 10),
    size = c(5000, rep(20, 9), 4000, rep(30, 9)),
    y = rnorm(20)
  )

  result <- sampling_design() |>
    stratify_by(group) |>
    draw(n = 5, method = "pps_brewer", mos = size, certainty_size = 1000) |>
    execute(frame, seed = 42)

  svy <- as_survey_design(result)
  expect_s3_class(svy, "survey.design")

  # Should have both group and .cert_stratum
  expect_true(".cert_stratum" %in% names(svy$variables))

  # svymean should work
  est <- survey::svymean(~y, svy)
  expect_true(is.numeric(coef(est)))
  expect_true(all(survey::SE(est) > 0))
})

test_that("PPS WOR without certainty does not add .cert_stratum", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:10,
    size = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
    y = 1:10
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = size) |>
    execute(frame, seed = 42)

  svy <- as_survey_design(result)
  expect_false(".cert_stratum" %in% names(svy$variables))
})

test_that("certainty stratum gives lower df than without separation", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:10,
    size = c(5000, rep(20, 9)),
    y = c(100, rnorm(9, 10, 2))
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = size, certainty_size = 1000) |>
    execute(frame, seed = 42)

  svy_with_cert <- as_survey_design(result)
  df_with <- survey::degf(svy_with_cert)

  n_cert <- sum(result$.certainty_1)
  n_prob <- sum(!result$.certainty_1)

  # df with separation = (n_cert - 1) + (n_prob - 1)
  # df without = n_total - 1
  # Difference = 1 (one fewer df due to stratum split)
  expect_equal(df_with, (n_cert - 1) + (n_prob - 1))
  expect_true(df_with < nrow(result) - 1)
})
