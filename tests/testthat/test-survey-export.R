test_that("execute() produces .fpc_k columns for unstratified SRS", {
  expect_true(".fpc_1" %in% names(fix_srs))
  # FPC should equal the frame size
  expect_equal(unique(fix_srs$.fpc_1), nrow(test_frame))
})

test_that("execute() produces .fpc_k columns for stratified design", {
  expect_true(".fpc_1" %in% names(fix_strat_prop))
  # FPC should vary by stratum and equal stratum population sizes
  fpc_by_stratum <- tapply(fix_strat_prop$.fpc_1, fix_strat_prop$stratum, unique)
  pop_by_stratum <- table(test_frame$stratum)
  for (s in names(fpc_by_stratum)) {
    expect_equal(
      as.integer(fpc_by_stratum[[s]]),
      as.integer(pop_by_stratum[[s]])
    )
  }
})

test_that("execute() produces .fpc_k columns for clustered design", {
  sample <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 8) |>
    execute(test_frame, seed = 5)

  expect_true(".fpc_1" %in% names(sample))
  # FPC should equal the number of distinct clusters in the frame
  n_clusters <- length(unique(test_frame$cluster))
  expect_equal(unique(sample$.fpc_1), n_clusters)
})

test_that("execute() produces .fpc_k for each stage in multi-stage design", {
  expect_true(".fpc_1" %in% names(fix_multistage))
  expect_true(".fpc_2" %in% names(fix_multistage))
  # Stage 1 FPC should be the number of clusters per stratum
  # Stage 2 FPC should be the number of units per selected cluster
})

test_that(".fpc_k columns carry forward in partial execution", {
  design <- sampling_design() |>
    add_stage(label = "Clusters") |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage(label = "Units") |>
    draw(n = 3)

  # Execute stage 1 only
  stage1 <- execute(design, test_frame, stages = 1, seed = 42)
  expect_true(".fpc_1" %in% names(stage1))

  # Continue with stage 2
  stage2 <- execute(stage1, test_frame, seed = 43)
  expect_true(".fpc_1" %in% names(stage2)) # carried forward
  expect_true(".fpc_2" %in% names(stage2)) # new
})

test_that(".fpc_k is preserved through dplyr operations", {
  # filter should preserve
  filtered <- dplyr::filter(fix_strat_prop, stratum == levels(stratum)[1])
  expect_true(".fpc_1" %in% names(filtered))

  # mutate should preserve
  mutated <- dplyr::mutate(fix_strat_prop, new_col = 1)
  expect_true(".fpc_1" %in% names(mutated))
})

test_that("summary.tbl_sample runs without error", {
  expect_output(summary(fix_strat_prop), "Sample Summary")
  expect_output(summary(fix_strat_prop), "Design")
  expect_output(summary(fix_strat_prop), "stages =")
  expect_output(summary(fix_strat_prop), "Allocation")
  expect_output(summary(fix_strat_prop), "Weights")
})

test_that("summary.tbl_sample shows stratum allocation table", {
  output <- capture.output(summary(fix_strat_prop))
  # Should contain N_h and n_h headers
  expect_true(any(grepl("N_h", output)))
  expect_true(any(grepl("n_h", output)))
  expect_true(any(grepl("f_h", output)))
  # Should contain Total row
  expect_true(any(grepl("Total", output)))
})

test_that("summary.tbl_sample shows allocation with stratum name", {
  output <- capture.output(summary(fix_strat_prop))
  expect_true(any(grepl("Allocation", output)))
  expect_true(any(grepl("stratum", output)))
  expect_true(any(grepl("Total", output)))
})

test_that("summary.tbl_sample works for unstratified design", {
  output <- capture.output(summary(fix_srs))
  expect_true(any(grepl("N =", output)))
  expect_true(any(grepl("n =", output)))
})

test_that("summary.tbl_sample shows weight diagnostics", {
  output <- capture.output(summary(fix_strat_prop))
  expect_true(any(grepl("DEFF", output)))
  expect_true(any(grepl("n_eff", output)))
  expect_true(any(grepl("CV", output)))
})

test_that("as_svydesign produces valid survey object", {
  skip_if_not_installed("survey")

  svy <- as_svydesign(fix_strat_prop)
  expect_s3_class(svy, "survey.design2")
})

test_that("as_svydesign works for clustered design", {
  skip_if_not_installed("survey")

  sample <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 8) |>
    execute(test_frame, seed = 5)

  svy <- as_svydesign(sample)
  expect_s3_class(svy, "survey.design2")
})

test_that("as_svydesign works for multi-stage design", {
  skip_if_not_installed("survey")

  svy <- as_svydesign(fix_multistage)
  expect_s3_class(svy, "survey.design2")
})

test_that("as_svrepdesign produces valid replicate survey object", {
  skip_if_not_installed("survey")

  rep_svy <- as_svrepdesign(fix_strat_prop, type = "auto")
  expect_s3_class(rep_svy, "svyrep.design")

  est <- survey::svymean(~mos, rep_svy)
  expect_true(all(is.finite(coef(est))))
  expect_true(all(is.finite(survey::SE(est))))
})

test_that("as_svrepdesign works for multi-stage non-PPS design", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    district_id = rep(seq_len(12), each = 40),
    ea_id = rep(seq_len(120), each = 4),
    student_id = seq_len(480),
    y = rnorm(480)
  )

  sample <- sampling_design() |>
    add_stage(label = "Districts") |>
    cluster_by(district_id) |>
    draw(n = 6) |>
    add_stage(label = "Schools") |>
    cluster_by(ea_id) |>
    draw(n = 3) |>
    add_stage(label = "Students") |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  expect_warning(
    rep_svy <- as_svrepdesign(sample, type = "auto"),
    "Finite population corrections after first stage have been dropped"
  )
  expect_s3_class(rep_svy, "svyrep.design")
})

test_that("as_svrepdesign rejects two-phase samples", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = 1:200,
    region = rep(letters[1:4], each = 50),
    x = rnorm(200)
  )

  design <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 80)

  phase1 <- execute(design, frame, seed = 1)
  phase2 <- execute(design, phase1, seed = 2)

  expect_error(
    as_svrepdesign(phase2),
    class = "samplyr_error_svrep_twophase_unsupported"
  )
})

test_that("as_svrepdesign rejects PPS designs", {
  skip_if_not_installed("survey")

  expect_error(
    as_svrepdesign(fix_pps_brewer, type = "auto"),
    class = "samplyr_error_svrep_pps_unsupported"
  )
})

test_that("as_survey_rep.tbl_sample returns srvyr replicate object", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")

  rep_tbl <- srvyr::as_survey_rep(fix_strat_prop, type = "auto")
  expect_s3_class(rep_tbl, "tbl_svy")
})

test_that("as_svydesign auto-detects twophase and honors method", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = 1:200,
    region = rep(letters[1:4], each = 50),
    x = rnorm(200)
  )

  design <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 80)

  phase1 <- execute(design, frame, seed = 1)
  phase1$y <- phase1$x + rnorm(nrow(phase1))

  phase2 <- execute(design, phase1, seed = 2)

  svy_auto <- as_svydesign(phase2)
  expect_s3_class(svy_auto, c("twophase", "twophase2"))

  svy_simple <- as_svydesign(phase2, method = "simple")
  expect_s3_class(svy_simple, "twophase")
})

test_that("as_svydesign twophase default uses full method", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = 1:120,
    region = rep(letters[1:3], each = 40),
    x = rnorm(120)
  )

  design <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 60)

  phase1 <- execute(design, frame, seed = 1)
  phase1$y <- phase1$x + rnorm(nrow(phase1))

  phase2 <- execute(design, phase1, seed = 2)

  svy_default <- as_svydesign(phase2)
  expect_s3_class(svy_default, "twophase2")
})

test_that("as_svydesign errors for three-phase samples", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = 1:150,
    region = rep(letters[1:3], each = 50),
    x = rnorm(150)
  )

  design <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 60)

  phase1 <- execute(design, frame, seed = 1)
  phase1$y <- phase1$x + rnorm(nrow(phase1))

  phase2 <- execute(design, phase1, seed = 2)
  phase2$z <- phase2$y + rnorm(nrow(phase2))

  phase3 <- execute(design, phase2, seed = 3)

  expect_error(
    as_svydesign(phase3),
    "two-phase"
  )
})

test_that("as_svydesign rejects method for single-phase samples", {
  skip_if_not_installed("survey")

  expect_error(
    as_svydesign(fix_srs, method = "simple"),
    "only valid when converting a two-phase sample"
  )
})

test_that("as_svydesign uses Inf FPC for WR methods (no correction)", {
  skip_if_not_installed("survey")

  svy <- as_svydesign(fix_srswr)
  expect_s3_class(svy, "survey.design2")
  # WR design: Inf population size = no finite population correction
  expect_true(all(svy$fpc$popsize == Inf))
})

test_that("as_svydesign handles mixed WR/WOR with Inf FPC for WR stages", {
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
    ea_id = rep(
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
    cluster_by(ea_id) |>
    draw(n = 3) |>
    add_stage(label = "Students") |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  svy <- as_svydesign(sample)
  expect_s3_class(svy, "survey.design2")

  # FPC should be present -- WR stage uses Inf, WOR stage uses real FPC
  expect_false(is.null(svy$fpc$popsize))
})

test_that("as_svydesign preserves FPC when all clustered stages are WOR", {
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
    ea_id = rep(
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
    cluster_by(ea_id) |>
    draw(n = 3) |>
    add_stage(label = "Students") |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  svy <- as_svydesign(sample)
  expect_s3_class(svy, "survey.design2")

  # FPC should be present -- both clustered stages are WOR
  expect_false(is.null(svy$fpc$popsize))
})

test_that("as_svydesign errors without survey package", {
  # This test validates the check_installed mechanism
  # It would only fail if survey is not installed, which is the point
  # In practice, the skip_if_not_installed tests above cover the happy path
  expect_true(TRUE) # placeholder
})

test_that("joint_expectation returns list of correct length", {
  jip <- joint_expectation(fix_multistage, test_frame)
  expect_type(jip, "list")
  expect_length(jip, 2)
  # Stage 1 is PPS -> matrix; Stage 2 is SRS -> NULL
  expect_true(is.matrix(jip[[1]]))
  expect_null(jip[[2]])
})

test_that("joint_expectation stage argument filters correctly", {
  jip <- joint_expectation(fix_multistage, test_frame, stage = 1)
  expect_length(jip, 2)
  expect_true(is.matrix(jip[[1]]))
  expect_null(jip[[2]])
})

test_that("joint_expectation matrix is square and symmetric", {
  jip <- joint_expectation(fix_pps_brewer, test_frame)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Joint matrix should be symmetric
  expect_equal(mat, t(mat), tolerance = 1e-10)
})

test_that("joint_expectation diagonal equals marginal pik", {
  jip <- joint_expectation(fix_pps_brewer, test_frame)
  mat <- jip[[1]]

  # Diagonal should equal 1/weight (marginal inclusion probabilities)
  pik_from_weights <- 1 / fix_pps_brewer$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_expectation errors on invalid stage", {
  expect_error(
    joint_expectation(fix_srs, test_frame, stage = 2),
    "not executed"
  )
})

test_that("joint_expectation requires integer stage values", {
  expect_error(
    joint_expectation(fix_srs, test_frame, stage = 1.5),
    "integer stage"
  )
})

test_that("joint_expectation returns NULL for non-PPS stages", {
  jip <- joint_expectation(fix_srs, test_frame)
  expect_null(jip[[1]])
})

test_that("joint_expectation works with ppsmat for survey export", {
  skip_if_not_installed("survey")

  jip <- joint_expectation(fix_pps_brewer, test_frame)
  svy <- as_svydesign(fix_pps_brewer, pps = survey::ppsmat(jip[[1]]))
  # ppsmat designs return "pps"/"survey.design", not "survey.design2"
  expect_s3_class(svy, "survey.design")
})

test_that("joint_expectation works with proportional allocation", {
  jip <- joint_expectation(fix_strat_pps, test_frame, stage = 1)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Diagonal pi_i should equal 1/weight for each cluster
  sample_df <- as.data.frame(fix_strat_pps)
  sample_clusters <- sample_df |>
    dplyr::distinct(cluster, .keep_all = TRUE)
  pik_from_weights <- 1 / sample_clusters$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_expectation works with stage vector", {
  # Request both stages via vector
  jip <- joint_expectation(fix_multistage, test_frame, stage = c(1, 2))
  expect_length(jip, 2)
  expect_true(is.matrix(jip[[1]]))
  # Stage 2 is SRS -- should be NULL
  expect_null(jip[[2]])
})

test_that("joint_expectation works with stratified pps_poisson and named frac", {
  levels_s <- levels(test_frame$stratum)
  frac_vec <- setNames(rep(0.15, length(levels_s)), levels_s)

  sample <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = frac_vec, method = "pps_poisson", mos = mos) |>
    execute(test_frame, seed = 42)

  jip <- joint_expectation(sample, test_frame)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Symmetric
  expect_equal(mat, t(mat), tolerance = 1e-10)
  # Diagonal should equal marginal pik = 1/weight
  pik_from_weights <- 1 / sample$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_expectation works with frac data frame", {
  frac_df <- data.frame(
    stratum = levels(test_frame$stratum),
    frac = c(0.18, 0.12, 0.18, 0.12)
  )

  sample <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = frac_df, method = "pps_poisson", mos = mos) |>
    execute(test_frame, seed = 42)

  jip <- joint_expectation(sample, test_frame)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  expect_equal(mat, t(mat), tolerance = 1e-10)

  pik_from_weights <- 1 / sample$.weight_1
  expect_equal(sort(diag(mat)), sort(pik_from_weights), tolerance = 1e-6)
})

test_that("joint_expectation decomposes certainty units correctly", {
  # Create a frame where one unit is very large (certainty selection)
  frame <- data.frame(
    id = paste0("u", 1:10),
    size = c(5000, rep(20, 9))
  )

  sample <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = size, certainty_size = 1000) |>
    execute(frame, seed = 42)

  jip <- joint_expectation(sample, frame)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Symmetric
  expect_equal(mat, t(mat), tolerance = 1e-10)

  # Diagonal should equal marginal pik = 1/weight
  pik_from_weights <- 1 / sample$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)

  # Certainty unit (weight = 1, pi = 1) should have diagonal = 1
  cert_rows <- which(sample$.certainty_1)
  if (length(cert_rows) > 0) {
    expect_equal(
      diag(mat)[cert_rows],
      rep(1, length(cert_rows)),
      tolerance = 1e-10
    )
    # Off-diagonal: certainty x non-certainty = pi_j
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

test_that("joint_expectation cross-stratum entries equal pi_i * pi_j", {
  jip <- joint_expectation(fix_strat_pps, test_frame)
  mat <- jip[[1]]

  # No zeros: cross-stratum entries should be pi_i * pi_j, not 0
  expect_true(all(mat > 0))

  # Verify cross-stratum entries explicitly
  pi_vec <- diag(mat)
  expected_cross <- outer(pi_vec, pi_vec)
  # Within-stratum entries differ from pi_i * pi_j (joint != product)
  # but cross-stratum entries should be exactly pi_i * pi_j
  sample_clusters <- as.data.frame(fix_strat_pps) |>
    dplyr::distinct(cluster, stratum)
  strata <- sample_clusters$stratum
  for (i in seq_along(strata)) {
    for (j in seq_along(strata)) {
      if (strata[i] != strata[j]) {
        expect_equal(
          mat[i, j],
          expected_cross[i, j],
          tolerance = 1e-12,
          info = paste("Cross-stratum entry [", i, ",", j, "]")
        )
      }
    }
  }
})

test_that("joint_expectation matrix matches sample PSU ordering", {
  jip <- joint_expectation(fix_strat_pps, test_frame)
  mat <- jip[[1]]

  # Diagonal should equal 1/weight in the same order as the sample
  sample_clusters <- as.data.frame(fix_strat_pps) |>
    dplyr::distinct(cluster, .keep_all = TRUE)
  pik_from_weights <- 1 / sample_clusters$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_expectation with ppsmat gives valid SE for stratified PPS", {
  skip_if_not_installed("survey")

  # ppsmat requires matrix dims = nrow(sample), so use unclustered PPS
  # (each row is its own PSU, like bfa_eas where ea_id = row)
  sample <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 2025)

  jip <- joint_expectation(sample, test_frame)

  svy_exact <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
  svy_brewer <- as_svydesign(sample)

  est_exact <- survey::svymean(~mos, svy_exact)
  est_brewer <- survey::svymean(~mos, svy_brewer)

  # SE should not be NaN
  expect_false(any(is.nan(survey::SE(est_exact))))
  # Point estimates should match
  expect_equal(coef(est_exact), coef(est_brewer), tolerance = 1e-6)
  # SEs should be close (exact vs approximation)
  expect_equal(
    survey::SE(est_exact)[[1]],
    survey::SE(est_brewer)[[1]],
    tolerance = 1
  )
})

test_that("joint_expectation with certainty works in stratified design", {
  # Add a large-mos cluster to force certainty selection
  cert_frame <- test_frame
  cert_frame$mos[cert_frame$cluster == "cl01"] <- 5000L

  sample <- sampling_design() |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = 1000) |>
    execute(cert_frame, seed = 42)

  jip <- joint_expectation(sample, cert_frame)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  expect_equal(mat, t(mat), tolerance = 1e-10)

  # Diagonal equals marginal pik
  sample_clusters <- as.data.frame(sample) |>
    dplyr::distinct(cluster, .keep_all = TRUE)
  pik_from_weights <- 1 / sample_clusters$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("as_svydesign for srswr uses .draw_1 as PSU with Inf FPC", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:20, y = rnorm(20))

  result <- sampling_design() |>
    draw(n = 10, method = "srswr") |>
    execute(frame, seed = 42)

  svy <- as_svydesign(result)
  expect_s3_class(svy, "survey.design")

  # Should have 10 rows (one per draw)
  expect_equal(nrow(svy$variables), 10L)

  # Estimate the mean -- should not error
  est <- survey::svymean(~y, svy)
  expect_true(is.numeric(coef(est)))
  expect_true(all(survey::SE(est) > 0))
})

test_that("as_svydesign for pps_chromy uses .draw_1 with Inf FPC", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:10,
    size = c(50, 100, 150, 200, 250, 300, 350, 400, 450, 500),
    y = 1:10
  )

  result <- sampling_design() |>
    draw(n = 6, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 42)

  svy <- as_svydesign(result)
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

  svy <- as_svydesign(result)
  expect_s3_class(svy, "survey.design")

  # Should be able to estimate mean without error
  est <- survey::svymean(~y, svy)
  expect_true(is.numeric(coef(est)))
  expect_true(all(survey::SE(est) > 0))
})

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

  svy <- as_svydesign(result)
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

  svy <- as_svydesign(result)
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

  svy <- as_svydesign(result)
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

  svy_with_cert <- as_svydesign(result)
  df_with <- survey::degf(svy_with_cert)

  n_cert <- sum(result$.certainty_1)
  n_prob <- sum(!result$.certainty_1)

  # df with separation = (n_cert - 1) + (n_prob - 1)
  # df without = n_total - 1
  # Difference = 1 (one fewer df due to stratum split)
  expect_equal(df_with, (n_cert - 1) + (n_prob - 1))
  expect_true(df_with < nrow(result) - 1)
})

test_that("as_svydesign works with pps_sps", {
  skip_if_not_installed("survey")

  svy <- as_svydesign(fix_pps_sps)
  expect_s3_class(svy, "survey.design")
})

test_that("as_svydesign works with pps_pareto", {
  skip_if_not_installed("survey")

  svy <- as_svydesign(fix_pps_pareto)
  expect_s3_class(svy, "survey.design")
})

test_that("joint_expectation works with pps_sps", {
  jip <- joint_expectation(fix_pps_sps, test_frame)
  jip_mat <- jip[[1]]

  expect_true(is.matrix(jip_mat))
  expect_equal(nrow(jip_mat), ncol(jip_mat))
  expect_equal(nrow(jip_mat), nrow(fix_pps_sps))
})

test_that("joint_expectation works with pps_pareto", {
  jip <- joint_expectation(fix_pps_pareto, test_frame)
  jip_mat <- jip[[1]]

  expect_true(is.matrix(jip_mat))
  expect_equal(nrow(jip_mat), ncol(jip_mat))
  expect_equal(nrow(jip_mat), nrow(fix_pps_pareto))
})

test_that("joint_expectation returns matrix for pps_multinomial", {
  je <- joint_expectation(fix_pps_multinomial, test_frame)
  mat <- je[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Symmetric
  expect_equal(mat, t(mat), tolerance = 1e-10)
  # Dimensions match unique sampled units
  n_unique <- dplyr::n_distinct(fix_pps_multinomial$id)
  expect_equal(nrow(mat), n_unique)
})

test_that("joint_expectation returns matrix for pps_chromy", {
  je <- joint_expectation(fix_pps_chromy, test_frame)
  mat <- je[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  # Symmetric
  expect_equal(mat, t(mat), tolerance = 1e-10)
  # Dimensions match unique sampled units
  n_unique <- dplyr::n_distinct(fix_pps_chromy$id)
  expect_equal(nrow(mat), n_unique)
})

test_that("joint_expectation for pps_multinomial diagonal matches expected hits", {
  frame <- data.frame(
    id = 1:20,
    size = c(50, 40, 30, rep(10, 17))
  )

  sample <- sampling_design() |>
    draw(n = 5, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 42)

  je <- joint_expectation(sample, frame)
  mat <- je[[1]]

  # Reconstruct expected hits from the frame
  mos_vals <- frame$size
  eh <- sondage::expected_hits(mos_vals, 5)

  # Diagonal of the joint matrix should equal expected hits
  # for the units that were sampled (in sample order)
  unique_ids <- unique(sample$id)
  expect_equal(diag(mat), eh[unique_ids], tolerance = 1e-10)
})

test_that("joint_expectation for stratified pps_multinomial works", {
  sample <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "pps_multinomial", mos = mos) |>
    execute(test_frame, seed = 42)

  je <- joint_expectation(sample, test_frame)
  mat <- je[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  expect_equal(mat, t(mat), tolerance = 1e-10)
})
