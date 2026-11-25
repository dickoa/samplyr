# Occasion export is tested in test-occasions.R and test-small-pool.R. It
# reaches the survey_*_info() helpers tested here, so a change to them reaches
# it too.

test_that("execute() produces .fpc_k columns for unstratified SRS", {
  expect_true(".fpc_1" %in% names(fix_srs))
  expect_equal(unique(fix_srs$.fpc_1), nrow(test_frame))
})

test_that("execute() produces .fpc_k columns for stratified design", {
  expect_true(".fpc_1" %in% names(fix_strat_prop))
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
  n_clusters <- length(unique(test_frame$cluster))
  expect_equal(unique(sample$.fpc_1), n_clusters)
})

test_that("execute() produces .fpc_k for each stage in multi-stage design", {
  expect_true(".fpc_1" %in% names(fix_multistage))
  expect_true(".fpc_2" %in% names(fix_multistage))
})

test_that(".fpc_k columns carry forward in partial execution", {
  design <- sampling_design() |>
    add_stage(label = "Clusters") |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage(label = "Units") |>
    draw(n = 3)

  stage1 <- execute(design, test_frame, stages = 1, seed = 42)
  expect_true(".fpc_1" %in% names(stage1))

  stage2 <- execute(stage1, test_frame, seed = 43)
  expect_true(".fpc_1" %in% names(stage2))
  expect_true(".fpc_2" %in% names(stage2))
})

test_that(".fpc_k is preserved through dplyr operations", {
  filtered <- dplyr::filter(fix_strat_prop, stratum == levels(stratum)[1])
  expect_true(".fpc_1" %in% names(filtered))

  mutated <- dplyr::mutate(fix_strat_prop, new_col = 1)
  expect_true(".fpc_1" %in% names(mutated))
})

test_that("summary.tbl_sample returns a summary that prints", {
  x <- summary(fix_strat_prop)
  expect_s3_class(x, "summary_tbl_sample")
  expect_output(print(x), "Sample Summary")
  expect_output(print(x), "Stage 1")
  expect_output(print(x), "stages =")
  expect_output(print(x), "Weights")
  utils::capture.output(shown <- withVisible(print(x)))
  expect_false(shown$visible)
  expect_identical(shown$value, x)
})

test_that("summary.tbl_sample shows stratum allocation ranges", {
  output <- capture.output(summary(fix_strat_prop))
  expect_true(any(grepl("N_h", output)))
  expect_true(any(grepl("n_h", output)))
  expect_true(any(grepl("f_h", output)))
  expect_true(any(grepl("4 strata: N_h 30, n_h 10, f_h 0.3333", output,
                        fixed = TRUE)))
})

test_that("summary.tbl_sample shows allocation with stratum name", {
  output <- capture.output(summary(fix_strat_prop))
  expect_true(any(grepl("by stratum", output)))
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

test_that("operational continuation exports as one multistage design", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(seq_len(6), each = 5),
    unit = seq_len(30)
  )
  design <- sampling_design() |>
    add_stage("PSUs") |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage("Units") |>
    draw(n = 2)

  psu_sample <- execute(design, frame, stages = 1, seed = 1)
  sample <- execute(psu_sample, frame, seed = 2)
  meta <- attr(sample, "metadata")

  expect_identical(get_design(sample), design)
  expect_equal(get_stages_executed(sample), c(1L, 2L))
  expect_false(is.null(meta$continued_from))
  expect_null(meta$prev_phase)
  expect_false(samplyr:::survey_phase_info(sample)$is_twophase)
  expect_equal(sample$.weight, sample$.weight_1 * sample$.weight_2)

  svy <- as_svydesign(sample)
  expect_s3_class(svy, "survey.design2")
  expect_false(inherits(svy, c("twophase", "twophase2")))
  expect_identical(all.vars(svy$call$ids), c(".id_1", ".id_2"))
  expect_identical(svy$variables$.id_1, match(sample$psu, unique(sample$psu)))
  expect_identical(all.vars(svy$call$fpc), c(".fpc_1", ".fpc_2"))
})

test_that("same-design partial frame warns but remains a valid new phase", {
  skip_if_not_installed("survey")
  skip_if_not_installed("tidyr")

  frame <- data.frame(site = seq_len(40))
  design <- sampling_design() |>
    add_stage("Sites") |>
    cluster_by(site) |>
    draw(frac = 0.5) |>
    add_stage("People") |>
    cluster_by(person) |>
    draw(n = 2)

  stage1 <- execute(design, frame, stages = 1, seed = 1)
  listing <- tidyr::expand_grid(
    site = stage1$site,
    person = seq_len(5)
  )
  expect_warning(
    phase2 <- execute(design, stage1, listing, seed = 2),
    "partial result of the same design",
    class = "samplyr_warning_same_design_frame"
  )

  meta <- attr(phase2, "metadata")
  expect_false(is.null(meta$prev_phase))
  expect_true(samplyr:::survey_phase_info(phase2)$is_twophase)
  expect_equal(unique(stage1$.weight), 2)
  expect_equal(unique(phase2$.weight_1), 2)
  expect_equal(unique(phase2$.weight_2), 2.5)
  expect_equal(unique(phase2$.weight), 10)

  svy <- as_svydesign(phase2, method = "simple")
  expect_s3_class(svy, "twophase")
})

test_that("stage continuation preserves an existing phase link", {
  skip_if_not_installed("survey")
  skip_if_not_installed("tidyr")

  frame <- data.frame(unit = seq_len(60), y = rnorm(60))
  phase1_design <- sampling_design() |>
    cluster_by(unit) |>
    draw(n = 30)
  phase1 <- execute(phase1_design, frame, seed = 1)

  phase2_design <- sampling_design() |>
    add_stage("Units") |>
    cluster_by(unit) |>
    draw(n = 20) |>
    add_stage("Subunits") |>
    cluster_by(subunit) |>
    draw(n = 2)
  phase2_stage1 <- execute(
    phase2_design, phase1, stages = 1, seed = 2
  )
  listing <- tidyr::expand_grid(
    unit = phase2_stage1$unit,
    subunit = seq_len(4)
  )
  phase2 <- execute(phase2_stage1, listing, seed = 3)
  meta <- attr(phase2, "metadata")

  expect_false(is.null(meta$continued_from))
  expect_false(is.null(meta$prev_phase))
  expect_true(samplyr:::survey_phase_info(phase2)$is_twophase)

  svy <- as_svydesign(phase2, method = "simple")
  expect_s3_class(svy, "twophase")
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
    expect_warning(
      rep_svy <- as_svrepdesign(sample, type = "auto"),
      "Finite population corrections after first stage have been dropped"
    ),
    class = "samplyr_warning_replicate_first_stage_fpc"
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

test_that("subbootstrap reads a PPS first stage as drawn with replacement", {
  skip_if_not_installed("survey")

  warning <- expect_warning(
    rep_svy <- as_svrepdesign(fix_pps_brewer, type = "subbootstrap"),
    class = "samplyr_warning_replicate_wr_first_stage"
  )
  expect_s3_class(rep_svy, "svyrep.design")
  # mrbbootstrap refuses this design, so the advice does not name it.
  expect_no_match(conditionMessage(warning), "mrbbootstrap", fixed = TRUE)
})

test_that("mrbbootstrap refuses a stage drawn with unequal probabilities", {
  skip_if_not_installed("survey")
  # survey reads one fraction per stratum from its first row, so the
  # variance would follow the row order.
  expect_error(
    as_svrepdesign(fix_pps_brewer, type = "mrbbootstrap"),
    class = "samplyr_error_svrep_conversion_failed"
  )
  rows <- variance_estimators(fix_pps_brewer)
  expect_identical(
    rows$status[rows$estimator == "Multistage rescaled bootstrap"], "refused"
  )
})

test_that("mrbbootstrap refuses a unit taken whole alone in its stratum", {
  skip_if_not_installed("survey")
  # Stratum A holds one EA, taken whole, whose households are drawn.
  # survey's multistage bootstrap resamples none of a stratum of one and
  # drops the stage below it, which gave that EA a standard error of 0.
  eas <- data.frame(ea = 1:21, st = c("A", rep("B", 20)),
                    size = c(40, rep(10, 20)))
  frame <- eas[rep(1:21, eas$size), ]
  frame$hh <- seq_len(nrow(frame))
  frame$y <- frame$hh %% 7
  design <- sampling_design() |>
    add_stage() |> stratify_by(st) |> cluster_by(ea) |>
    draw(n = c(A = 1, B = 5)) |>
    add_stage() |> draw(n = 5)
  sample <- execute(design, frame, seed = 3)
  expect_error(
    as_svrepdesign(sample, type = "mrbbootstrap", replicates = 20),
    class = "samplyr_error_svrep_conversion_failed"
  )
  rows <- variance_estimators(design, frame)
  expect_identical(
    rows$status[rows$estimator == "Multistage rescaled bootstrap"], "refused"
  )
  # Two EAs in every stratum are resampled as before.
  design2 <- sampling_design() |>
    add_stage() |> stratify_by(st) |> cluster_by(ea) |>
    draw(n = 2) |>
    add_stage() |> draw(n = 5)
  frame2 <- rbind(frame, transform(frame[frame$ea == 1, ], ea = 22,
                                   hh = hh + 1000))
  sample2 <- execute(design2, frame2, seed = 3)
  expect_no_error(
    as_svrepdesign(sample2, type = "mrbbootstrap", replicates = 20)
  )
})

test_that("as_svrepdesign says a PPS first stage is taken as with replacement", {
  skip_if_not_installed("survey")

  for (type in c("bootstrap", "JKn")) {
    expect_warning(
      rep <- as_svrepdesign(fix_pps_brewer, type = type),
      "first stage, drawn with \"pps_brewer\", as drawn with replacement",
      class = "samplyr_warning_replicate_wr_first_stage"
    )
    expect_s3_class(rep, "svyrep.design")
  }
})

test_that("unequal probabilities below stage 1 draw no first-stage warning", {
  skip_if_not_installed("survey")
  frame <- test_frame
  frame$unit_mos <- rep(c(3, 9, 5, 12, 7), 24)
  later_pps <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 3) |>
    add_stage() |> draw(n = 2, method = "pps_brewer", mos = unit_mos) |>
    execute(frame, seed = 1)
  wr_first <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 3, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 2) |>
    execute(frame, seed = 1)
  for (sample in list(later_pps, wr_first)) {
    expect_no_warning(
      suppressWarnings(
        as_svrepdesign(sample, type = "JKn"),
        classes = c("simpleWarning",
                    "samplyr_warning_replicate_first_stage_fpc")
      ),
      class = "samplyr_warning_replicate_wr_first_stage"
    )
  }
})

test_that("a first-stage correction scaling later-stage variance warns", {
  skip_if_not_installed("survey")
  # Every PSU has total 4, so all of the variance comes from stage 2.
  frame <- data.frame(
    psu = rep(1:8, each = 4),
    id = 1:32,
    y = rep(c(0, 0, 2, 2), 8)
  )
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 6) |>
    add_stage() |> draw(n = 2)
  sample <- execute(design, frame, seed = 3)
  raised <- function(type) {
    classes <- character(0)
    withCallingHandlers(
      as_svrepdesign(sample, type = type, replicates = 20),
      warning = function(w) {
        classes <<- c(classes, grep("^samplyr_", class(w), value = TRUE))
        invokeRestart("muffleWarning")
      }
    )
    classes
  }
  for (type in c("auto", "JK1", "bootstrap")) {
    expect_identical(raised(type),
                     "samplyr_warning_replicate_first_stage_fpc")
  }
  # The rescaled bootstrap drops the correction instead, reading the first
  # stage as drawn with replacement.
  expect_identical(raised("subbootstrap"),
                   "samplyr_warning_replicate_wr_first_stage")
  expect_identical(raised("mrbbootstrap"), character(0))

  # The jackknife is the uncorrected one times 1 - 6/8, stage 2 included.
  rep_jk1 <- suppressWarnings(as_svrepdesign(sample, type = "JK1"))
  uncorrected <- survey::as.svrepdesign(
    survey::svydesign(ids = ~psu, weights = ~.weight,
                      data = as.data.frame(sample)),
    type = "JK1"
  )
  v_jk1 <- as.numeric(vcov(survey::svytotal(~y, rep_jk1)))
  v_uncorrected <- as.numeric(vcov(survey::svytotal(~y, uncorrected)))
  expect_gt(v_uncorrected, 0)
  expect_equal(v_jk1, v_uncorrected * (1 - 6 / 8))

  report <- variance_estimators(design, frame)
  expect_identical(
    report$status[report$estimator %in% c("Jackknife (JK1)", "Bootstrap")],
    c("approximate", "approximate")
  )

  # A later stage that takes every unit adds no variance.
  census <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 6) |>
    add_stage() |> draw(n = 4)
  expect_no_warning(
    suppressWarnings(
      as_svrepdesign(execute(census, frame, seed = 3), type = "JK1"),
      classes = "simpleWarning"
    ),
    class = "samplyr_warning_replicate_first_stage_fpc"
  )
  expect_identical(variance_estimators(census, frame)$status[3], "supported")

  # Four draws with replacement from four units are no census.
  redrawn <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 6) |>
    add_stage() |> draw(n = 4, method = "srswr")
  expect_warning(
    suppressWarnings(
      as_svrepdesign(execute(redrawn, frame, seed = 3), type = "JK1"),
      classes = "simpleWarning"
    ),
    class = "samplyr_warning_replicate_first_stage_fpc"
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

test_that("as_svydesign errors for two-phase with PPS at phase 1", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = 1:200,
    size = runif(200, 1, 10),
    y = rnorm(200)
  )

  d1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 80, method = "pps_brewer", mos = size)

  d2 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 40)

  p1 <- execute(d1, frame, seed = 1)
  p2 <- execute(d2, p1, seed = 2)

  err <- tryCatch(as_svydesign(p2), error = identity)
  expect_s3_class(err, "samplyr_error_twophase_phase1_pps")
  expect_match(conditionMessage(err), "pps_brewer", fixed = TRUE)
  expect_false(grepl("separately", conditionMessage(err), fixed = TRUE))
})

## A subsample within the final-stage pools exports as phase 1's design

# 40 EAs in two regions, each listing 25 to 60 households. EAs are drawn by
# PPS, 12 households in each, and phase 2 keeps half of them in every EA.
subsample_frame <- function() {
  eas <- data.frame(
    ea = sprintf("e%02d", 1:40),
    region = rep(c("North", "South"), each = 20),
    size = 25 + (seq_len(40) * 7) %% 36
  )
  hh <- eas[rep(seq_len(nrow(eas)), eas$size), ]
  hh$hh <- seq_len(nrow(hh))
  hh$y <- (hh$hh %% 11) + (hh$region == "South") * 3
  rownames(hh) <- NULL
  hh
}

subsample_phase1 <- function(method = "pps_systematic", take = 12) {
  design <- sampling_design() |>
    add_stage("EAs") |>
    stratify_by(region) |>
    cluster_by(ea) |>
    draw(n = 5, method = method, mos = size) |>
    add_stage("Households") |>
    draw(n = take, method = "systematic")
  execute(design, subsample_frame(), seed = 1)
}

half_by_ea <- function(frac = 0.5) {
  sampling_design() |>
    stratify_by(ea) |>
    draw(frac = frac, method = "systematic")
}

test_that("a half sample of each EA's households exports as a multistage design", {
  skip_if_not_installed("survey")
  phase1 <- subsample_phase1()
  # The EA id repeats over households, and the pools link the phases.
  expect_no_warning(validate_frame(half_by_ea(), phase1))
  phase2 <- execute(half_by_ea(), phase1, seed = 2)

  svy <- as_svydesign(phase2, systematic_variance = "approximate")
  expect_s3_class(svy, "survey.design2")
  expect_false(inherits(svy, "twophase2"))
  expect_equal(unname(weights(svy)), phase2$.weight)

  # The household stage now takes 6 of each EA's listing.
  per_ea <- table(phase2$ea)
  expect_true(all(per_ea == 6L))
  listed <- table(subsample_frame()$ea)
  expect_equal(
    unname(svy$fpc$sampsize[, 2]),
    rep(6, nrow(phase2))
  )
  expect_equal(
    unname(svy$fpc$popsize[, 2]),
    as.numeric(listed[phase2$ea])
  )

  # Analysis columns added to phase 2 are carried.
  phase2$male <- phase2$y > 5
  expect_true("male" %in% names(as_svydesign(
    phase2, systematic_variance = "approximate"
  )$variables))
})

test_that("a subsample that keeps every household exports phase 1 itself", {
  skip_if_not_installed("survey")
  phase1 <- subsample_phase1(method = "pps_brewer")
  everything <- execute(half_by_ea(frac = 1), phase1, seed = 2)

  se <- function(x) {
    unname(survey::SE(survey::svytotal(
      ~y, as_svydesign(x, systematic_variance = "approximate")
    )))
  }
  expect_equal(se(everything), se(phase1))
  half <- execute(half_by_ea(), phase1, seed = 2)
  expect_false(isTRUE(all.equal(se(half), se(phase1))))
})

test_that("the subsample has a replicate export", {
  skip_if_not_installed("survey")
  phase2 <- execute(half_by_ea(), subsample_phase1(), seed = 2)
  # JKn treats the PPS first stage as drawn with replacement, and says so.
  expect_warning(
    rep <- as_svrepdesign(phase2, type = "JKn",
                          systematic_variance = "approximate"),
    class = "samplyr_warning_replicate_wr_first_stage"
  )
  expect_s3_class(rep, "svyrep.design")
  expect_equal(unname(weights(rep, type = "sampling")), phase2$.weight)
})

test_that("a systematic subsample keeps its approximation in every export", {
  skip_if_not_installed("survey")
  # Phase 1 draws its final stage by srswor, so only phase 2 is systematic.
  frame <- data.frame(id = 1:100, y = rep(c(1, 0, 0, 0, 0), 20))
  phase1 <- sampling_design() |> draw(n = 80) |> execute(frame, seed = 1)
  systematic <- sampling_design() |>
    draw(n = 20, method = "systematic", control = id)
  phase2 <- execute(systematic, phase1, seed = 2)

  expect_error(
    as_svydesign(phase2, systematic_variance = "error"),
    class = "samplyr_error_systematic_variance"
  )
  for (type in c("JK1", "rwyb")) {
    expect_error(
      as_svrepdesign(phase2, type = type, systematic_variance = "error"),
      class = "samplyr_error_systematic_variance"
    )
  }
  svy <- as_svydesign(phase2, systematic_variance = "approximate")
  expect_identical(
    attr(svy, "samplyr_systematic_variance")$stages,
    "stage 1"
  )
  expect_identical(
    variance_estimators(systematic, phase1)$status[1],
    "approximate"
  )

  # An srswor phase 2 has no approximation to report.
  srs <- sampling_design() |> draw(n = 20)
  expect_no_warning(as_svydesign(execute(srs, phase1, seed = 2)))
  expect_identical(variance_estimators(srs, phase1)$status[1], "supported")
})

test_that("a systematic phase 2 that takes every unit adds no approximation", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:100, y = 1:100, s = rep(1:4, 25))
  phase1 <- sampling_design() |>
    stratify_by(s) |>
    draw(n = 20) |>
    execute(frame, seed = 1)
  by_s <- function(n) {
    sampling_design() |>
      stratify_by(s) |>
      draw(n = n, method = "systematic")
  }
  # Every unit of every pool, by fraction or by a take the pool caps.
  for (design in list(
    sampling_design() |> stratify_by(s) |> draw(frac = 1, method = "systematic"),
    by_s(25)
  )) {
    phase2 <- suppressWarnings(execute(design, phase1, seed = 2))
    expect_s3_class(
      as_svydesign(phase2, systematic_variance = "error"),
      "survey.design2"
    )
    expect_s3_class(
      as_svrepdesign(phase2, type = "JKn", systematic_variance = "error"),
      "svyrep.design"
    )
    expect_identical(variance_estimators(design, phase1)$status[1],
                     "supported")
  }

  # One pool subsampled keeps the approximation.
  partial <- by_s(c("1" = 20, "2" = 20, "3" = 20, "4" = 5))
  phase2 <- execute(partial, phase1, seed = 2)
  expect_error(
    as_svydesign(phase2, systematic_variance = "error"),
    class = "samplyr_error_systematic_variance"
  )
  expect_identical(variance_estimators(partial, phase1)$status[1],
                   "approximate")
})

test_that("method is refused on a subsample that exports as multistage", {
  skip_if_not_installed("survey")
  phase2 <- execute(half_by_ea(), subsample_phase1(), seed = 2)
  err <- expect_error(
    as_svydesign(phase2, method = "full",
                 systematic_variance = "approximate"),
    class = "samplyr_error_survey_argument"
  )
  expect_match(conditionMessage(err), "multistage", fixed = TRUE)
})

test_that("other second phases keep the two-phase route", {
  skip_if_not_installed("survey")
  phase1 <- subsample_phase1()
  refused <- function(design2) {
    phase2 <- suppressWarnings(suppressMessages(
      execute(design2, phase1, seed = 2)
    ))
    expect_error(
      as_svydesign(phase2, systematic_variance = "approximate"),
      class = "samplyr_error_twophase_phase1_pps"
    )
  }
  # Coarser than the pools: one stratum holds several EAs.
  refused(sampling_design() |> stratify_by(region) |> draw(n = 20))
  # Finer than the pools: a pool split in two.
  phase1$half <- phase1$hh %% 2
  refused(sampling_design() |> stratify_by(ea, half) |> draw(n = 2))
  # As many strata as pools, cutting across them: ten teams, ten EAs.
  phase1$team <- seq_len(nrow(phase1)) %% 10
  expect_identical(length(unique(phase1$ea)), 10L)
  refused(sampling_design() |> stratify_by(team) |> draw(n = 4))
  # Unequal probabilities at phase 2.
  refused(sampling_design() |> stratify_by(ea) |>
            draw(n = 4, method = "pps_brewer", mos = hh))
  # Clustered phase 2.
  refused(sampling_design() |> stratify_by(ea) |> cluster_by(hh) |>
            draw(n = 4))

  # A PPS final stage at phase 1.
  pps_last <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(ea) |>
    draw(n = 5, method = "pps_brewer", mos = size) |>
    add_stage() |> draw(n = 12, method = "pps_brewer", mos = hh) |>
    execute(subsample_frame(), seed = 1)
  expect_error(
    as_svydesign(execute(half_by_ea(), pps_last, seed = 2),
                 systematic_variance = "approximate"),
    class = "samplyr_error_twophase_phase1_pps"
  )
})

test_that("a PPS phase 1 is named before a linkage problem behind it", {
  skip_if_not_installed("survey")
  # The EA key does not identify phase-1 rows, but phase-1 PPS has no route.
  pop <- expand.grid(k = 1:10, ea = 1:30)
  pop$mos <- (pop$ea %% 6) + 1
  p1 <- sampling_design() |>
    cluster_by(ea) |>
    draw(n = 8, method = "pps_brewer", mos = mos) |>
    execute(pop, seed = 1)
  p2 <- sampling_design() |> draw(n = 30) |> execute(p1, seed = 2)
  expect_error(as_svydesign(p2), class = "samplyr_error_twophase_phase1_pps")
})

test_that("a spatial or bounded phase 1 is refused before the phases are linked", {
  skip_if_not_installed("survey")
  # As on the occasion route: these methods have no variance at either phase.
  pop <- expand.grid(k = 1:10, ea = 1:30)
  pop$mos <- (pop$ea %% 6) + 1
  pop$lon <- (pop$ea * 7) %% 11
  pop$lat <- (pop$ea * 5) %% 13
  pop$band <- pop$ea %% 2
  second <- sampling_design() |> draw(n = 30)
  phase1s <- list(
    lpm2 = sampling_design() |>
      cluster_by(ea) |>
      draw(n = 8, method = "lpm2", mos = mos, spread = c(lon, lat)),
    scps = sampling_design() |>
      cluster_by(ea) |>
      draw(n = 8, method = "scps", spread = c(lon, lat)),
    bounded = sampling_design() |>
      cluster_by(ea) |>
      draw(n = 8, method = "cube", aux = c(bound(band)))
  )
  for (nm in names(phase1s)) {
    p2 <- execute(second, execute(phase1s[[nm]], pop, seed = 1), seed = 2)
    err <- tryCatch(as_svydesign(p2), error = identity)
    expect_s3_class(err, "samplyr_error_custom_random_wor_export")
    expect_match(conditionMessage(err), phase1s[[nm]]$stages[[1]]$draw_spec$method,
                 fixed = TRUE, info = nm)
    err <- tryCatch(as_svrepdesign(p2), error = identity)
    expect_match(conditionMessage(err), "refuses it too", fixed = TRUE, info = nm)
  }
})

test_that("replicate advice for two phases names a route that exists", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:200, size = runif(200, 1, 10), y = rnorm(200))
  second <- sampling_design() |> cluster_by(id) |> draw(n = 40)

  pps1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 80, method = "pps_brewer", mos = size) |>
    execute(frame, seed = 1)
  err <- tryCatch(
    as_svrepdesign(execute(second, pps1, seed = 2)),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_svrep_twophase_unsupported")
  expect_match(conditionMessage(err), "refuses it too", fixed = TRUE)

  srs1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 80) |>
    execute(frame, seed = 1)
  err <- tryCatch(
    as_svrepdesign(execute(second, srs1, seed = 2)),
    error = identity
  )
  expect_match(conditionMessage(err), "two-phase linearization", fixed = TRUE)
  expect_s3_class(as_svydesign(execute(second, srs1, seed = 2)), "twophase2")
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

  # Stage 1 WR gets Inf FPC, and stage 2 WOR keeps its real FPC.
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

  expect_false(is.null(svy$fpc$popsize))
})

test_that("as_svydesign preserves FPC when all clustered stages are WOR", {
  skip_if_not_installed("survey")

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

  expect_false(is.null(svy$fpc$popsize))
})

test_that("every export asks for the survey package before anything else", {
  asked <- character(0)
  local_mocked_bindings(
    check_installed = function(pkg, ...) {
      asked <<- c(asked, pkg)
      rlang::abort("survey is absent", class = "probe_not_installed")
    },
    .package = "rlang"
  )
  population <- data.frame(id = 1:20, in_a = 1:20 <= 12, in_b = 1:20 > 8)
  component <- function(col, seed) {
    sampling_design() |>
      draw(n = 4) |>
      execute(population[population[[col]], ], seed = seed)
  }
  sample <- component("in_a", 1)
  stack <- stack_frames(
    a = sample, b = component("in_b", 2),
    membership = c(a = "in_a", b = "in_b"), key = id
  )
  for (x in list(sample, stack)) {
    expect_error(as_svydesign(x), class = "probe_not_installed")
    expect_error(as_svrepdesign(x), class = "probe_not_installed")
  }
  expect_identical(asked, rep("survey", 4))
})

## Random-size Poisson methods (bernoulli, pps_poisson)

test_that("as_svydesign uses poisson_sampling for single-stage bernoulli", {
  skip_if_not_installed("survey")

  s <- sampling_design() |>
    draw(frac = 0.3, method = "bernoulli") |>
    execute(test_frame, seed = 11)

  svy <- as_svydesign(s)
  expect_s3_class(svy, "pps")
  expect_true(".fpc_pi_1" %in% names(svy$variables))
})

test_that("as_svydesign uses poisson_sampling for single-stage pps_poisson", {
  skip_if_not_installed("survey")

  s <- sampling_design() |>
    draw(frac = 0.3, method = "pps_poisson", mos = mos) |>
    execute(test_frame, seed = 12)

  svy <- as_svydesign(s)
  expect_s3_class(svy, "pps")
  expect_true(".fpc_pi_1" %in% names(svy$variables))
})

test_that("a pps specification must fit stage 1's sample size", {
  skip_if_not_installed("survey")
  set.seed(3)
  frame <- data.frame(id = 1:200, mos = runif(200, 1, 10))
  frame$y <- 3 * frame$mos + rnorm(200)
  draws <- list(
    bernoulli = draw(sampling_design(), frac = 0.2, method = "bernoulli"),
    pps_poisson = draw(sampling_design(), n = 40, mos = mos,
                       method = "pps_poisson"),
    pps_brewer = draw(sampling_design(), n = 40, mos = mos,
                      method = "pps_brewer"),
    srswor = draw(sampling_design(), n = 40)
  )
  # The rule written out. Overton and HR need a fixed-size PPS stage 1, and
  # poisson_sampling() a Poisson PPS stage 1. A joint matrix, or the user's
  # own covariance, needs a PPS stage 1 of either size regime.
  accepted <- c(
    "pps_poisson ppsmat", "pps_poisson poisson_sampling",
    "pps_poisson ppscov",
    "pps_brewer overton", "pps_brewer hr", "pps_brewer ppsmat",
    "pps_brewer ppscov"
  )
  forms <- c("overton", "hr", "ppsmat", "poisson_sampling", "ppscov")
  seen <- character(0)
  for (method in names(draws)) {
    s <- execute(draws[[method]], frame, seed = 1)
    pik <- 1 / s$.weight
    if (method == "pps_brewer") {
      joint <- joint_expectation(s, frame)[[1]]
    } else {
      joint <- outer(pik, pik)
      diag(joint) <- pik
    }
    specs <- list(
      overton = "overton", hr = survey::HR(),
      ppsmat = survey::ppsmat(joint),
      poisson_sampling = survey::poisson_sampling(pik),
      ppscov = survey::ppscov(1 - outer(pik, pik) / joint, weighted = TRUE)
    )
    for (spec in names(specs)) {
      label <- paste(method, spec)
      seen <- c(seen, label)
      if (label %in% accepted) {
        expect_no_error(as_svydesign(s, pps = specs[[spec]]))
      } else {
        expect_error(
          as_svydesign(s, pps = specs[[spec]]),
          class = "samplyr_error_pps_argument"
        )
      }
    }
    if (method == "pps_poisson") {
      # Closed-form Horvitz-Thompson variance of a Poisson total.
      exact <- sqrt(sum((1 - pik) / pik^2 * s$y^2))
      for (spec in c("ppsmat", "poisson_sampling")) {
        svy <- as_svydesign(s, pps = specs[[spec]])
        expect_equal(
          as.numeric(survey::SE(survey::svytotal(~y, svy))), exact,
          tolerance = 1e-10
        )
      }
    }
  }
  expect_setequal(seen, paste(rep(names(draws), each = 5), forms))
})

test_that("single-stage bernoulli SE matches HT-Poisson formula", {
  skip_if_not_installed("survey")

  s <- sampling_design() |>
    draw(frac = 0.3, method = "bernoulli") |>
    execute(test_frame, seed = 13)

  svy <- as_svydesign(s)
  est <- survey::svytotal(~y, svy)

  pi_vec <- 1 / s$.weight
  manual_se <- sqrt(sum((1 - pi_vec) / pi_vec^2 * s$y^2))
  expect_equal(unname(survey::SE(est)[1]), manual_se, tolerance = 1e-6)
})

test_that("single-stage pps_poisson SE matches HT-Poisson formula", {
  skip_if_not_installed("survey")

  s <- sampling_design() |>
    draw(frac = 0.3, method = "pps_poisson", mos = mos) |>
    execute(test_frame, seed = 14)

  svy <- as_svydesign(s)
  est <- survey::svytotal(~y, svy)

  pi_vec <- 1 / s$.weight
  manual_se <- sqrt(sum((1 - pi_vec) / pi_vec^2 * s$y^2))
  expect_equal(unname(survey::SE(est)[1]), manual_se, tolerance = 1e-6)
})

test_that("multi-stage with bernoulli at stage 2 refuses invalid linearization", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(seq_len(20), each = 30),
    hh  = seq_len(600),
    y   = rnorm(600)
  )

  s <- sampling_design() |>
    add_stage() |>
      cluster_by(psu) |>
      draw(n = 10) |>
    add_stage() |>
      cluster_by(hh) |>
      draw(frac = 0.5, method = "bernoulli") |>
    execute(frame, seed = 21)

  expect_error(as_svydesign(s), class = "samplyr_error_multistage_poisson_later")
})

test_that("multi-stage with bernoulli at stage 1 errors with samplyr_error_multistage_poisson_stage1", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(seq_len(50), each = 10),
    id  = seq_len(500),
    y   = rnorm(500)
  )

  s <- sampling_design() |>
    add_stage() |>
      cluster_by(psu) |>
      draw(frac = 0.4, method = "bernoulli") |>
    add_stage() |>
      draw(n = 4) |>
    execute(frame, seed = 22)

  expect_error(
    as_svydesign(s),
    class = "samplyr_error_multistage_poisson_stage1"
  )
})

test_that("clustered single-stage bernoulli with multi-row clusters errors", {
  skip_if_not_installed("survey")

  # test_frame has 5 rows per cluster, which trips the multi-row guard.
  s <- sampling_design() |>
    cluster_by(cluster) |>
    draw(frac = 0.3, method = "bernoulli") |>
    execute(test_frame, seed = 23)

  expect_error(
    as_svydesign(s),
    class = "samplyr_error_cluster_poisson_export"
  )
})

test_that("generic bootstrap methods refuse Poisson sampling", {
  skip_if_not_installed("survey")
  s <- sampling_design() |> cluster_by(cluster) |>
    draw(frac = 0.3, method = "bernoulli") |> execute(test_frame, seed = 25)
  for (type in c("bootstrap", "subbootstrap", "mrbbootstrap")) {
    expect_error(as_svrepdesign(s, type = type),
      class = "samplyr_error_poisson_replicates")
  }
})

test_that("as_survey_design returns a tbl_svy for single-stage bernoulli", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")

  s <- sampling_design() |>
    draw(frac = 0.3, method = "bernoulli") |>
    execute(test_frame, seed = 27)

  svy <- srvyr::as_survey_design(s)
  expect_s3_class(svy, "tbl_svy")
  expect_true(inherits(svy, "pps"))
})

test_that("as_survey_design returns a tbl_svy for single-stage pps_poisson", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")

  s <- sampling_design() |>
    draw(frac = 0.3, method = "pps_poisson", mos = mos) |>
    execute(test_frame, seed = 28)

  svy <- srvyr::as_survey_design(s)
  expect_s3_class(svy, "tbl_svy")
  expect_true(inherits(svy, "pps"))
})

test_that("as_survey_design pps_poisson SE matches the survey design SE", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")

  s <- sampling_design() |>
    draw(frac = 0.3, method = "pps_poisson", mos = mos) |>
    execute(test_frame, seed = 29)

  svy <- srvyr::as_survey_design(s)
  est <- srvyr::summarise(svy, t = srvyr::survey_total(y, vartype = "se"))

  ref_se <- unname(survey::SE(survey::svytotal(~y, as_svydesign(s)))[1])
  expect_equal(est$t_se, ref_se, tolerance = 1e-6)
})

test_that("as_survey_design supports grouped verbs for pps_poisson", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")

  s <- sampling_design() |>
    draw(frac = 0.3, method = "pps_poisson", mos = mos) |>
    execute(test_frame, seed = 30)

  svy <- srvyr::as_survey_design(s)
  grouped <- svy |>
    srvyr::group_by(stratum) |>
    srvyr::summarise(t = srvyr::survey_total(y, vartype = "se"))

  # svyby on the survey.design object is the known-good domain estimator
  ref <- survey::svyby(~y, ~stratum, as_svydesign(s), survey::svytotal)
  expect_equal(grouped$t, unname(ref$y), tolerance = 1e-6)
  expect_equal(grouped$t_se, unname(ref$se), tolerance = 1e-6)
})

test_that("joint_expectation returns list of correct length", {
  jip <- joint_expectation(fix_multistage, test_frame)
  expect_type(jip, "list")
  expect_length(jip, 2)
  # Stage 1 is PPS, so a matrix. Stage 2 is SRS, so NULL.
  expect_true(is.matrix(jip[[1]]))
  expect_null(jip[[2]])
})

test_that("joint_expectation stage argument filters correctly", {
  jip <- joint_expectation(fix_multistage, test_frame, stages = 1)
  expect_length(jip, 2)
  expect_true(is.matrix(jip[[1]]))
  expect_null(jip[[2]])
})

test_that("joint_expectation matrix is square and symmetric", {
  jip <- joint_expectation(fix_pps_brewer, test_frame)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  expect_equal(mat, t(mat), tolerance = 1e-10)
})

test_that("joint_expectation diagonal equals marginal pik", {
  jip <- joint_expectation(fix_pps_brewer, test_frame)
  mat <- jip[[1]]

  pik_from_weights <- 1 / fix_pps_brewer$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_expectation errors on invalid stage", {
  expect_error(
    joint_expectation(fix_srs, test_frame, stages = 2),
    class = "samplyr_error_stage_selector"
  )
  expect_error(
    joint_expectation(fix_srs, test_frame, stages = 2),
    "Not executed stages: 2"
  )
})

test_that("joint_expectation requires integer stage values", {
  expect_error(
    joint_expectation(fix_srs, test_frame, stages = 1.5),
    class = "samplyr_error_stage_selector"
  )
  expect_error(
    joint_expectation(fix_srs, test_frame, stages = 1.5),
    "whole, finite stage numbers"
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

test_that("systematic PPS ppsmat export warns about unseen pair probabilities", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = seq_len(80),
    mos = 1 + seq_len(80) %% 9
  )
  sample <- sampling_design() |>
    draw(n = 12, method = "pps_systematic", mos = mos) |>
    execute(frame, seed = 42)
  joint <- joint_expectation(sample, frame)

  expect_warning(
    as_svydesign(sample, pps = survey::ppsmat(joint[[1L]])),
    class = "samplyr_warning_systematic_ppsmat"
  )
  # A supplied matrix is not the systematic-variance warning's business.
  expect_warning(
    as_svydesign(sample, pps = "brewer"),
    class = "samplyr_warning_systematic_variance"
  )
  classes <- character()
  withCallingHandlers(
    as_svydesign(sample, pps = survey::ppsmat(joint[[1L]])),
    warning = function(w) {
      classes <<- c(classes, class(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true("samplyr_warning_systematic_ppsmat" %in% classes)
  expect_false("samplyr_warning_systematic_variance" %in% classes)
})

test_that("joint_expectation works with proportional allocation", {
  jip <- joint_expectation(fix_strat_pps, test_frame, stages = 1)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  sample_df <- as.data.frame(fix_strat_pps)
  sample_clusters <- sample_df |>
    dplyr::distinct(cluster, .keep_all = TRUE)
  pik_from_weights <- 1 / sample_clusters$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_expectation works with stage vector", {
  jip <- joint_expectation(fix_multistage, test_frame, stages = c(1, 2))
  expect_length(jip, 2)
  expect_true(is.matrix(jip[[1]]))
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
  expect_equal(mat, t(mat), tolerance = 1e-10)
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
  expect_equal(mat, t(mat), tolerance = 1e-10)

  pik_from_weights <- 1 / sample$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)

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

test_that("joint_expectation honors explicit certainty thresholds", {
  frame <- data.frame(
    id = 1:100,
    size = 1:100
  )

  sample <- sampling_design() |>
    draw(n = 20, method = "pps_brewer", mos = size, certainty_size = 95) |>
    execute(frame, seed = 1)

  expect_true(any(sample$.certainty_1))

  jip <- joint_expectation(sample, frame)
  mat <- jip[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), nrow(sample))
  expect_equal(mat, t(mat), tolerance = 1e-10)

  pik_from_weights <- 1 / sample$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-8)
  expect_equal(diag(mat)[sample$.certainty_1], rep(1, sum(sample$.certainty_1)))
})

test_that("joint_expectation cross-stratum entries equal pi_i * pi_j", {
  jip <- joint_expectation(fix_strat_pps, test_frame)
  mat <- jip[[1]]

  # Cross-stratum entries are pi_i * pi_j, not 0.
  expect_true(all(mat > 0))

  pi_vec <- diag(mat)
  expected_cross <- outer(pi_vec, pi_vec)
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

  sample_clusters <- as.data.frame(fix_strat_pps) |>
    dplyr::distinct(cluster, .keep_all = TRUE)
  pik_from_weights <- 1 / sample_clusters$.weight_1
  expect_equal(diag(mat), pik_from_weights, tolerance = 1e-6)
})

test_that("joint_expectation with ppsmat gives valid SE for stratified PPS", {
  skip_if_not_installed("survey")

  # ppsmat needs one matrix row per sample row, so the PPS is unclustered.
  sample <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 2025)

  jip <- joint_expectation(sample, test_frame)

  svy_exact <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
  svy_brewer <- as_svydesign(sample)

  est_exact <- survey::svymean(~mos, svy_exact)
  est_brewer <- survey::svymean(~mos, svy_brewer)

  expect_false(any(is.nan(survey::SE(est_exact))))
  expect_equal(coef(est_exact), coef(est_brewer), tolerance = 1e-6)
  expect_equal(
    survey::SE(est_exact)[[1]],
    survey::SE(est_brewer)[[1]],
    tolerance = 1
  )
})

test_that("joint_expectation with certainty works in stratified design", {
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

  # One row per draw.
  expect_equal(nrow(svy$variables), 10L)

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

test_that("the replicate warning for a PMR first stage names routes that exist", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:40, size = c(rep(5, 30), 40, 60, rep(8, 8)),
                      y = 1:40)
  design <- sampling_design() |>
    draw(n = 6, method = "pps_chromy", mos = size)
  result <- execute(design, frame, seed = 1)

  w <- expect_warning(
    as_svrepdesign(result, type = "bootstrap", replicates = 10),
    class = "samplyr_warning_replicate_wr_first_stage"
  )
  # RWYB refuses minimum replacement, so the warning must not offer it.
  expect_no_match(conditionMessage(w), "use every stage")
  expect_match(conditionMessage(w), "random_groups", fixed = TRUE)
  expect_error(as_svrepdesign(result, type = "rwyb"),
               class = "samplyr_error_rwyb_method")

  repeated <- execute(design, frame, seed = 1, reps = 5)
  rg <- expect_no_warning(as_svrepdesign(repeated, type = "random_groups"))
  expect_identical(ncol(rg$repweights), 5L)

  ve <- variance_estimators(result)
  note <- ve$note[ve$call == "as_svrepdesign(x, type = \"bootstrap\")"]
  expect_match(note, "dependence between hits", fixed = TRUE)
})

test_that("two-stage WR cluster design with independent sub-samples", {
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

  # 6 draws times 3 units per draw.
  expect_equal(nrow(result), 18L)

  expect_true(".draw_1" %in% names(result))
  for (d in unique(result$.draw_1)) {
    expect_equal(sum(result$.draw_1 == d), 3L)
  }

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

  expect_true(".cert_stratum" %in% names(svy$variables))

  # Two strata: certainty and probability.
  strata_levels <- unique(svy$strata[, 1])
  expect_equal(length(strata_levels), 2L)

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

  expect_true(".cert_stratum" %in% names(svy$variables))

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

  # One fewer df than n - 1, from the certainty/probability split.
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
  expect_equal(mat, t(mat), tolerance = 1e-10)
  n_unique <- dplyr::n_distinct(fix_pps_multinomial$id)
  expect_equal(nrow(mat), n_unique)
})

test_that("joint_expectation returns matrix for pps_chromy", {
  je <- joint_expectation(fix_pps_chromy, test_frame)
  mat <- je[[1]]

  expect_true(is.matrix(mat))
  expect_equal(nrow(mat), ncol(mat))
  expect_equal(mat, t(mat), tolerance = 1e-10)
  n_unique <- dplyr::n_distinct(fix_pps_chromy$id)
  expect_equal(nrow(mat), n_unique)
})

test_that("joint_expectation accepts and validates Chromy nsim", {
  expect_no_error(
    je <- joint_expectation(fix_pps_chromy, test_frame, nsim = 25L)
  )
  expect_true(is.matrix(je[[1]]))

  for (bad in list(0, -1, 2.5, NA_real_, Inf, c(1, 2), "10")) {
    expect_error(
      joint_expectation(fix_pps_chromy, test_frame, nsim = bad),
      "single positive integer"
    )
  }
})

test_that("chromy joint expectations are a function of nsim and seed alone", {
  # Simulated hits have no closed form, so the test pins the simulation.
  set.seed(1)
  before <- .Random.seed
  a <- joint_expectation(fix_pps_chromy, test_frame)
  expect_identical(.Random.seed, before)

  expect_equal(joint_expectation(fix_pps_chromy, test_frame), a)

  # A later draw must be unaffected by whether a joint was computed first.
  set.seed(99)
  with_joint <- {
    invisible(joint_expectation(fix_pps_chromy, test_frame))
    stats::runif(3)
  }
  set.seed(99)
  expect_identical(stats::runif(3), with_joint)

  # seed and nsim must each move the answer, or neither argument does a job.
  expect_false(
    isTRUE(all.equal(a, joint_expectation(fix_pps_chromy, test_frame, seed = 2L)))
  )
  expect_false(
    isTRUE(all.equal(a, joint_expectation(fix_pps_chromy, test_frame, nsim = 50L)))
  )

  # An analytic method draws nothing, so seed cannot reach it.
  expect_equal(
    joint_expectation(fix_pps_multinomial, test_frame, seed = 1L),
    joint_expectation(fix_pps_multinomial, test_frame, seed = 12345L)
  )
})

test_that("joint_expectation validates seed and refuses it in activation mode", {
  for (bad in list(2.5, NA_integer_, Inf, c(1, 2), "10", numeric(0))) {
    expect_error(
      joint_expectation(fix_pps_chromy, test_frame, seed = bad),
      "single integer"
    )
  }
  # The refusal must name seed rather than report nsim, which was not given.
  err <- tryCatch(
    joint_expectation(fix_pps_chromy, period = c(1, 2), seed = 3L),
    error = function(e) e
  )
  expect_s3_class(err, "samplyr_error_joint_activation_arguments")
  expect_match(conditionMessage(err), "seed")
  expect_false(grepl("nsim", conditionMessage(err)))
})

test_that("joint_expectation for pps_multinomial diagonal matches E[n_i^2]", {
  frame <- data.frame(
    id = 1:20,
    size = c(50, 40, 30, rep(10, 17))
  )

  sample <- sampling_design() |>
    draw(n = 5, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 42)

  je <- joint_expectation(sample, frame)
  mat <- je[[1]]

  # The diagonal is E[n_i^2] = n * (n - 1) * p_i^2 + n * p_i, not E[n_i].
  mos_vals <- frame$size
  n <- 5
  p <- mos_vals / sum(mos_vals)
  eh_sq <- n * (n - 1) * p^2 + n * p

  unique_ids <- unique(sample$id)
  expect_equal(diag(mat), eh_sq[unique_ids], tolerance = 1e-10)
})

test_that("joint_expectation for pps_multinomial with n > N is not truncated", {
  frame <- data.frame(
    id = 1:5,
    mos = c(1, 2, 3, 4, 5)
  )
  n <- 10L

  sample <- sampling_design() |>
    draw(n = n, method = "pps_multinomial", mos = mos) |>
    execute(frame, seed = 1)

  je <- joint_expectation(sample, frame)
  mat <- je[[1]]

  p <- frame$mos / sum(frame$mos)
  eh_sq <- n * (n - 1) * p^2 + n * p

  unique_ids <- unique(sample$id)
  expect_equal(diag(mat), eh_sq[unique_ids], tolerance = 1e-10)
  expect_equal(sum(diag(mat)), sum(eh_sq[unique_ids]), tolerance = 1e-10)
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

## Multistage export completeness
#
# Every executed stage appears in the exported ids and fpc, multi-variable
# keys collapse to one term per stage, and the certainty stratum combines
# with user strata.

test_that("first-stage census + unclustered SRS stage has positive SE matching reference", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(1:10, each = 20),
    id = seq_len(200),
    y = rep(1:20, 10)^2
  )

  s <- sampling_design() |>
    add_stage("PSU") |>
      cluster_by(psu) |>
      draw(n = 10) |>
    add_stage("unit") |>
      draw(n = 5) |>
    execute(frame, seed = 7)

  svy <- as_svydesign(s)
  se <- unname(survey::SE(survey::svymean(~y, svy)))
  expect_gt(se, 0)

  ref <- survey::svydesign(
    ids = ~psu + id,
    fpc = ~.fpc_1 + .fpc_2,
    weights = ~.weight,
    data = as.data.frame(s)
  )
  ref_se <- unname(survey::SE(survey::svymean(~y, ref)))
  expect_equal(se, ref_se, tolerance = 1e-10)
})

test_that("implicit final unit stage matches explicit cluster_by(id)", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(1:10, each = 20),
    id = seq_len(200),
    y = rep(1:20, 10)^2
  )

  implicit <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 6) |>
    add_stage() |> draw(n = 5) |>
    execute(frame, seed = 11)
  explicit <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 6) |>
    add_stage() |> cluster_by(id) |> draw(n = 5) |>
    execute(frame, seed = 11)

  est_i <- survey::svymean(~y, as_svydesign(implicit))
  est_e <- survey::svymean(~y, as_svydesign(explicit))
  expect_equal(unname(coef(est_i)), unname(coef(est_e)), tolerance = 1e-10)
  expect_equal(
    unname(survey::SE(est_i)),
    unname(survey::SE(est_e)),
    tolerance = 1e-10
  )
})

test_that("exported design has one id and one fpc term per executed stage", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(1:10, each = 20),
    id = seq_len(200),
    y = seq_len(200)
  )

  s <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 6) |>
    add_stage() |> draw(n = 5) |>
    execute(frame, seed = 3)

  svy <- as_svydesign(s)
  expect_length(all.vars(svy$call$ids), 2L)
  expect_length(all.vars(svy$call$fpc), 2L)
})

test_that("multi-variable cluster_by exports a single interaction id", {
  skip_if_not_installed("survey")

  frame <- expand.grid(unit = 1:10, ea = 1:5, region = c("N", "S"))
  frame$y <- seq_len(nrow(frame)) + 40 * (frame$region == "S")

  s <- sampling_design() |>
    cluster_by(region, ea) |>
    draw(n = 4) |>
    execute(frame, seed = 11)

  svy <- as_svydesign(s)
  expect_length(all.vars(svy$call$ids), 1L)

  df <- as.data.frame(s)
  df$cl <- interaction(df$region, df$ea, drop = TRUE)
  ref <- survey::svydesign(
    ids = ~cl, fpc = ~.fpc_1, weights = ~.weight, data = df
  )
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, ref))),
    tolerance = 1e-10
  )
})

test_that("multi-variable cluster_by at stage 1 of a two-stage design matches reference", {
  skip_if_not_installed("survey")

  frame <- expand.grid(unit = 1:10, ea = 1:5, region = c("N", "S"))
  frame$y <- seq_len(nrow(frame))
  frame$row_id <- seq_len(nrow(frame))

  s <- sampling_design() |>
    add_stage() |> cluster_by(region, ea) |> draw(n = 4) |>
    add_stage() |> draw(n = 5) |>
    execute(frame, seed = 12)

  svy <- as_svydesign(s)
  df <- as.data.frame(s)
  df$cl <- interaction(df$region, df$ea, drop = TRUE)
  ref <- survey::svydesign(
    ids = ~cl + row_id,
    fpc = ~.fpc_1 + .fpc_2,
    weights = ~.weight,
    data = df,
    nest = TRUE
  )
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, ref))),
    tolerance = 1e-10
  )
})

test_that("multi-variable stratify_by exports interaction strata", {
  skip_if_not_installed("survey")

  frame <- expand.grid(
    unit = 1:25,
    sector = c("A", "B"),
    size = c("small", "large")
  )
  frame$y <- as.integer(factor(frame$sector)) * 10 +
    as.integer(factor(frame$size)) * 100 + frame$unit

  s <- sampling_design() |>
    stratify_by(sector, size) |>
    draw(frac = 0.2) |>
    execute(frame, seed = 13)

  svy <- as_svydesign(s)

  df <- as.data.frame(s)
  df$stratum <- interaction(df$sector, df$size, drop = TRUE)
  ref <- survey::svydesign(
    ids = ~1, strata = ~stratum, fpc = ~.fpc_1,
    weights = ~.weight, data = df
  )
  # Must match full cross-classification, not first-variable-only.
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, ref))),
    tolerance = 1e-10
  )
})

test_that("certainty stratum combines with user strata", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    s = rep(c("A", "B"), each = 20),
    id = 1:40,
    mos = rep(c(rep(1, 19), 100), 2),
    y = 1:40
  )

  smp <- sampling_design() |>
    stratify_by(s) |>
    draw(
      n = c(A = 5, B = 5), method = "pps_brewer", mos = mos,
      certainty_size = 50
    ) |>
    execute(frame, seed = 19)
  expect_equal(sum(smp$.certainty_1), 2L)

  svy <- as_svydesign(smp)
  # One stratum term, with certainty separated within each user stratum.
  expect_length(all.vars(svy$call$strata), 1L)
  expect_equal(
    length(unique(svy$strata[[1]])),
    4L
  )
})

test_that("second-stage strata are exported and match reference", {
  skip_if_not_installed("survey")

  frame <- expand.grid(unit = 1:4, s2 = c("m", "f"), psu = 1:8)
  frame$row_id <- seq_len(nrow(frame))
  frame$y <- frame$row_id %% 7 + 10 * (frame$s2 == "f")

  s <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 4) |>
    add_stage() |> stratify_by(s2) |> draw(n = c(m = 2, f = 2)) |>
    execute(frame, seed = 21)

  svy <- as_svydesign(s)

  df <- as.data.frame(s)
  df$s1 <- "all"
  ref <- survey::svydesign(
    ids = ~psu + row_id,
    strata = ~s1 + s2,
    fpc = ~.fpc_1 + .fpc_2,
    weights = ~.weight,
    data = df,
    nest = TRUE
  )
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, ref))),
    tolerance = 1e-10
  )
})

test_that("srswr export still matches the manual with-replacement design", {
  skip_if_not_installed("survey")

  frame <- data.frame(id = 1:20, y = (1:20)^1.5)
  s <- sampling_design() |>
    draw(n = 10, method = "srswr") |>
    execute(frame, seed = 3)

  svy <- as_svydesign(s)
  ref <- survey::svydesign(
    ids = ~1, weights = ~.weight, data = as.data.frame(s)
  )
  expect_equal(
    unname(survey::SE(survey::svymean(~y, svy))),
    unname(survey::SE(survey::svymean(~y, ref))),
    tolerance = 1e-10
  )
})

test_that("PPS stage 1 with later stages exports fraction-scale fpc", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(1:10, each = 20),
    mos = rep(c(30, 50, 20, 80, 40, 60, 25, 45, 70, 35), each = 20),
    id = 1:200,
    y = (1:200) %% 13
  )

  s <- sampling_design() |>
    add_stage() |>
      cluster_by(psu) |>
      draw(n = 4, method = "pps_brewer", mos = mos) |>
    add_stage() |>
      draw(n = 8) |>
    execute(frame, seed = 31)

  svy <- as_svydesign(s)
  se <- unname(survey::SE(survey::svymean(~y, svy)))
  expect_gt(se, 0)

  df <- as.data.frame(s)
  df$pi1 <- 1 / df$.weight_1
  df$f2 <- 1 / df$.weight_2
  ref <- survey::svydesign(
    ids = ~psu + id,
    fpc = ~pi1 + f2,
    weights = ~.weight,
    data = df,
    pps = "brewer",
    nest = TRUE
  )
  expect_equal(
    se,
    unname(survey::SE(survey::svymean(~y, ref))),
    tolerance = 1e-10
  )
})

test_that("PPS stage 1 with clustered stage 2 exports without fpc scale error", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(1:10, each = 20),
    mos = rep(c(30, 50, 20, 80, 40, 60, 25, 45, 70, 35), each = 20),
    id = 1:200,
    y = (1:200) %% 13
  )

  # survey needs every fpc term on one scale, pi at stage 1 included.
  s <- sampling_design() |>
    add_stage() |>
      cluster_by(psu) |>
      draw(n = 4, method = "pps_brewer", mos = mos) |>
    add_stage() |>
      cluster_by(id) |>
      draw(n = 8) |>
    execute(frame, seed = 31)

  svy <- as_svydesign(s)
  expect_s3_class(svy, "survey.design2")
  expect_gt(unname(survey::SE(survey::svymean(~y, svy))), 0)
})

test_that("unclustered element stage before later stages is refused", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(1:10, each = 20),
    id = seq_len(200),
    y = seq_len(200)
  )

  design <- sampling_design() |>
    add_stage() |> draw(n = 100) |>
    add_stage() |> cluster_by(psu) |> draw(n = 4)

  # Execution refuses it: an element stage cannot name stage 2's parents.
  expect_error(
    execute(design, frame, seed = 5),
    class = "samplyr_error_stage_parent_id"
  )
})

test_that("WR stage 1 with unclustered stage 2 keeps the WR variance", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(1:8, each = 10),
    mos = rep(c(5, 10, 15, 20, 25, 30, 35, 40), each = 10),
    id = 1:80,
    y = (1:80 %% 7) + rep(1:8, each = 10)
  )

  s <- sampling_design() |>
    add_stage() |>
      cluster_by(psu) |>
      draw(n = 4, method = "pps_multinomial", mos = mos) |>
    add_stage() |>
      draw(n = 3) |>
    execute(frame, seed = 5)

  svy <- as_svydesign(s)

  # fpc = Inf at stage 1, so the export equals the between-draw estimator.
  df <- as.data.frame(s)
  ref <- survey::svydesign(
    ids = ~.draw_1, weights = ~.weight, data = df
  )
  expect_equal(
    unname(survey::SE(survey::svymean(~y, svy))),
    unname(survey::SE(survey::svymean(~y, ref))),
    tolerance = 1e-10
  )
})

test_that("a single-stage pps specification on a multi-stage sample exports stage 1", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(1:10, each = 20),
    mos = rep(c(30, 50, 20, 80, 40, 60, 25, 45, 70, 35), each = 20),
    id = 1:200,
    y = (1:200) %% 13
  )

  s <- sampling_design() |>
    add_stage() |>
      cluster_by(psu) |>
      draw(n = 4, method = "pps_brewer", mos = mos) |>
    add_stage() |>
      draw(n = 8) |>
    execute(frame, seed = 31)

  # Overton's approximation is single-stage in survey: stage 1 only.
  expect_warning(
    svy <- as_svydesign(s, pps = "overton"),
    "one stage"
  )
  expect_s3_class(svy, "survey.design")
  expect_identical(NCOL(svy$cluster), 1L)

  # survey indexes a joint matrix by row, so it needs one row per stage-1 unit.
  jip <- joint_expectation(s, frame, stages = 1)[[1]]
  idx <- match(s$psu, sort(unique(s$psu)))
  expect_error(
    as_svydesign(s, pps = survey::ppsmat(jip[idx, idx])),
    class = "samplyr_error_pps_rows_per_psu"
  )
})

# A unit capped at probability one by the PPS calculation is the same object
# as one named by certainty_size and exports the same way. The frame is
# deterministic: units 1-3 cap at one under n = 10.
cert_export_frame <- function() {
  frame <- data.frame(id = seq_len(60), mos = c(500, 400, 300, rep(10, 57)))
  frame$y <- frame$mos * 2 + (frame$id %% 7)
  frame
}

test_that("automatic and explicit certainty export identically", {
  skip_if_not_installed("survey")

  frame <- cert_export_frame()
  automatic <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(frame, seed = 11)
  # certainty_size = 250 names exactly the units the calculation caps.
  explicit <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos, certainty_size = 250) |>
    execute(frame, seed = 11)

  expect_identical(sort(automatic$id), sort(explicit$id))
  expect_identical(automatic$.certainty_1, explicit$.certainty_1)

  svy_auto <- as_svydesign(automatic)
  svy_expl <- as_svydesign(explicit)

  auto_total <- survey::svytotal(~y, svy_auto)
  expl_total <- survey::svytotal(~y, svy_expl)

  expect_equal(coef(auto_total), coef(expl_total))
  expect_equal(survey::SE(auto_total), survey::SE(expl_total))
  expect_equal(survey::degf(svy_auto), survey::degf(svy_expl))
  expect_identical(svy_auto$call$strata, svy_expl$call$strata)
})

test_that("PPS-WOR certainty units get a take-all stratum", {
  skip_if_not_installed("survey")

  sample <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(cert_export_frame(), seed = 11)

  svy <- as_svydesign(sample)
  expect_equal(deparse(svy$call$strata), "~.cert_stratum")
  expect_setequal(
    unique(svy$variables$.cert_stratum),
    c("certainty", "probability")
  )
})

test_that("cube certainty units get a take-all stratum", {
  skip_if_not_installed("survey")

  frame <- cert_export_frame()
  frame$x <- rep(c(1, 2), length.out = 60)

  sample <- sampling_design() |>
    draw(n = 10, method = "cube", mos = mos, aux = c(x)) |>
    execute(frame, seed = 3)

  # cube is not in pps_wor_methods but exports under the PPS-WOR treatment.
  expect_true(any(sample$.certainty_1))
  expect_equal(deparse(as_svydesign(sample)$call$strata), "~.cert_stratum")
})

test_that("random-size designs do not acquire a take-all stratum", {
  skip_if_not_installed("survey")

  # pps_poisson exports under the Poisson treatment, with no take-all stratum.
  sample <- suppressWarnings(
    sampling_design() |>
      draw(n = 10, method = "pps_poisson", mos = mos) |>
      execute(cert_export_frame(), seed = 11)
  )

  expect_true(any(sample$.certainty_1))
  expect_null(as_svydesign(sample)$call$strata)
  expect_false(".cert_stratum" %in% names(as_svydesign(sample)$variables))
})

test_that("probability-one units do not change the Poisson variance family", {
  skip_if_not_installed("survey")

  frame <- cert_export_frame()
  poisson <- suppressWarnings(
    sampling_design() |>
      draw(n = 10, method = "pps_poisson", mos = mos) |>
      execute(frame, seed = 11)
  )
  bernoulli <- sampling_design() |>
    draw(frac = 0.2, method = "bernoulli") |>
    execute(frame, seed = 11)

  expect_identical(
    samplyr:::survey_stage_kind(
      attr(poisson, "design")$stages[[1]]$draw_spec
    ),
    "rs_poisson"
  )
  expect_s3_class(as_svydesign(poisson), "survey.design")
  expect_s3_class(as_svydesign(bernoulli), "survey.design")
})

test_that("a two-phase export represents every phase-1 stage", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = seq_len(400),
    psu = rep(seq_len(40), each = 10),
    value = seq_len(400) / 400
  )

  # The unclustered element stage needs a synthesized identifier and fpc term.
  phase1 <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 16) |>
    add_stage() |>
    draw(n = 5) |>
    execute(frame, seed = 7)

  phase2 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 20) |>
    execute(phase1, seed = 8)

  svy <- quiet_across(as_svydesign(phase2))
  total <- survey::svytotal(~value, svy)

  expect_equal(
    unname(stats::weights(svy)),
    unname(as.data.frame(phase2)$.weight)
  )
  expect_equal(
    unname(coef(total)),
    sum(as.data.frame(phase2)$.weight * as.data.frame(phase2)$value)
  )

  # The weights cannot detect a dropped stage's variance, so SEs are compared.
  reference_data <- as.data.frame(phase1)
  reference_data$.elem <- seq_len(nrow(reference_data))
  reference_data$.in_phase2 <- reference_data$id %in% as.data.frame(phase2)$id
  reference_data$.p2_fpc <- nrow(reference_data)
  reference <- survey::twophase(
    id = list(~ psu + .elem, ~id),
    strata = list(NULL, NULL),
    fpc = list(~ .fpc_1 + .fpc_2, ~.p2_fpc),
    subset = ~.in_phase2,
    data = reference_data,
    method = "full"
  )
  reference_total <- survey::svytotal(~value, reference)

  expect_equal(unname(coef(total)), unname(coef(reference_total)))
  expect_equal(unname(survey::SE(total)), unname(survey::SE(reference_total)))

  # The phase-1 design carries one identifier stage per executed stage.
  expect_equal(ncol(svy$phase1$full$cluster), 2L)
})

test_that("a systematic stage says its variance is approximated", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = seq_len(400),
    region = rep(c("North", "South"), each = 200),
    mos = 1 + seq_len(400) %% 9
  )
  systematic <- sampling_design() |>
    add_stage("Households") |>
    stratify_by(region) |>
    draw(n = 50, method = "systematic") |>
    execute(frame, seed = 1)

  expect_warning(
    result <- as_svydesign(systematic),
    class = "samplyr_warning_systematic_variance"
  )
  # The stage is named, and the approximated estimator is linearization.
  svy_warning <- conditionMessage(
    tryCatch(as_svydesign(systematic), warning = function(w) w)
  )
  expect_match(svy_warning, "Households")
  expect_match(svy_warning, "simple random sampling")

  # Acknowledged, and refused.
  expect_no_warning(
    as_svydesign(systematic, systematic_variance = "approximate")
  )
  expect_error(
    as_svydesign(systematic, systematic_variance = "error"),
    class = "samplyr_error_systematic_variance"
  )

  # The decision is inspectable afterwards.
  recorded <- attr(result, "samplyr_systematic_variance")
  expect_identical(recorded$approximation, "srswor")
  expect_identical(recorded$acknowledged, "warn")
  expect_match(recorded$stages, "Households")
})

test_that("replicate weights say they do not reproduce a systematic stage", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = seq_len(400),
    region = rep(c("North", "South"), each = 200),
    mos = 1 + seq_len(400) %% 9
  )
  systematic <- sampling_design() |>
    add_stage("Households") |>
    stratify_by(region) |>
    draw(n = 50, method = "systematic") |>
    execute(frame, seed = 1)

  expect_warning(
    result <- as_svrepdesign(systematic, type = "JKn"),
    class = "samplyr_warning_systematic_variance"
  )
  # The condition describes the replicate estimator, not the linearization one.
  rep_warning <- conditionMessage(
    tryCatch(as_svrepdesign(systematic, type = "JKn"),
             warning = function(w) w)
  )
  expect_match(rep_warning, "Households")
  expect_match(rep_warning, "replicate weights")
  expect_false(grepl("simple random sampling", rep_warning, fixed = TRUE))
  expect_false(grepl("0.0006", rep_warning, fixed = TRUE))

  # Naming a type is not an acknowledgement, whichever type it is.
  for (type in c("auto", "bootstrap", "subbootstrap", "mrbbootstrap")) {
    expect_warning(
      suppressWarnings(
        as_svrepdesign(systematic, type = type),
        classes = "samplyr_warning_replicate_wr_first_stage"
      ),
      class = "samplyr_warning_systematic_variance"
    )
  }

  # Only the acknowledgement silences it, and "error" refuses.
  expect_no_warning(
    as_svrepdesign(systematic, type = "JKn",
                   systematic_variance = "approximate")
  )
  expect_error(
    as_svrepdesign(systematic, type = "JKn", systematic_variance = "error"),
    class = "samplyr_error_systematic_variance"
  )

  # The decision is inspectable afterwards, and names this export's estimator.
  recorded <- attr(result, "samplyr_systematic_variance")
  expect_identical(recorded$approximation, "generic_replicates")
  expect_identical(recorded$acknowledged, "warn")
  expect_match(recorded$stages, "Households")
})

test_that("the replicate warning is confined to what it approximates", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = seq_len(400),
    region = rep(c("North", "South"), each = 200),
    mos = 1 + seq_len(400) %% 9
  )

  # Equal probability but not systematic.
  expect_no_warning(as_svrepdesign(
    sampling_design() |> stratify_by(region) |> draw(n = 50) |>
      execute(frame, seed = 1),
    type = "JKn"
  ))

  # A census stage has nothing to misstate. Its PSUs are strata of their
  # stage-2 units, which JKn resamples.
  clustered <- data.frame(
    id = seq_len(400),
    psu = rep(seq_len(40), each = 10)
  )
  census_stage <- suppressWarnings(
    sampling_design() |>
      add_stage("PSUs") |>
      cluster_by(psu) |>
      draw(n = 40, method = "systematic") |>
      add_stage("Elements") |>
      draw(n = 5) |>
      execute(clustered, seed = 1)
  )
  # Only survey's own warnings are muffled, so the class under test surfaces.
  without_survey_warnings <- function(expr) {
    withCallingHandlers(expr, warning = function(w) {
      if (!inherits(w, "samplyr_warning_systematic_variance")) {
        invokeRestart("muffleWarning")
      }
    })
  }
  expect_no_warning(
    without_survey_warnings(as_svrepdesign(census_stage, type = "JKn")),
    class = "samplyr_warning_systematic_variance"
  )
  # The muffler is not what makes that pass: it lets the class through.
  expect_warning(
    without_survey_warnings(as_svrepdesign(
      sampling_design() |> cluster_by(psu) |>
        draw(n = 20, method = "systematic") |>
        execute(clustered, seed = 1),
      type = "JK1"
    )),
    class = "samplyr_warning_systematic_variance"
  )

  # An export with no systematic stage records no approximation.
  plain <- as_svrepdesign(
    sampling_design() |> stratify_by(region) |> draw(n = 50) |>
      execute(frame, seed = 1),
    type = "JKn"
  )
  expect_null(attr(plain, "samplyr_systematic_variance")$approximation)
})

test_that("the replicate refusal comes before the conversion", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = seq_len(400),
    region = rep(c("North", "South"), each = 200)
  )
  # survey refuses unstratified JKn too. The error shows which check ran first.
  unstratified <- sampling_design() |>
    draw(n = 50, method = "systematic") |>
    execute(frame, seed = 1)
  # The default warns on the way past, which is not what this asserts.
  cnd <- expect_error(
    suppressWarnings(as_svrepdesign(unstratified, type = "JKn")),
    class = "samplyr_error_svrep_conversion_failed"
  )
  expect_identical(condition_header(cnd), "as_svrepdesign")
  expect_error(
    as_svrepdesign(unstratified, type = "JKn", systematic_variance = "error"),
    class = "samplyr_error_systematic_variance"
  )

  # A near miss is a stray argument, not partial-matched or sent to survey.
  systematic <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 50, method = "systematic") |>
    execute(frame, seed = 1)
  expect_error(
    as_svrepdesign(systematic, type = "JKn", systematic_varianc = "error"),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    as_svrepdesign(systematic, type = "JKn", systematic_variance = "nonsense"),
    class = "samplyr_error_survey_argument"
  )
})

test_that("the systematic warning is confined to what it approximates", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = seq_len(400),
    region = rep(c("North", "South"), each = 200),
    mos = 1 + seq_len(400) %% 9
  )

  # An equal-probability design that is not systematic.
  expect_no_warning(as_svydesign(
    sampling_design() |> stratify_by(region) |> draw(n = 50) |>
      execute(frame, seed = 1)
  ))

  # pps_systematic is approximated by Brewer's formula, named in the condition.
  pps_sys <- sampling_design() |>
    draw(n = 50, method = "pps_systematic", mos = mos) |>
    execute(frame, seed = 1)
  w <- tryCatch(as_svydesign(pps_sys), warning = identity)
  expect_s3_class(w, "samplyr_warning_systematic_variance")
  expect_match(conditionMessage(w), "Brewer", fixed = TRUE)
  expect_identical(
    attr(as_svydesign(pps_sys, systematic_variance = "approximate"),
         "samplyr_systematic_variance")$approximation,
    "brewer"
  )

  # A census pps_systematic stage has nothing to approximate.
  expect_no_warning(as_svydesign(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = 200, method = "pps_systematic", mos = mos) |>
      execute(frame, seed = 1)
  ))

  # A census stage contributes no variance for the approximation to misstate.
  expect_no_warning(as_svydesign(
    sampling_design() |> stratify_by(region) |> draw(n = 200, method = "systematic") |>
      execute(frame, seed = 1)
  ))
})

test_that("a systematic phase one is named even though the sample is not", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = seq_len(400),
    psu = rep(seq_len(40), each = 10),
    region = rep(c("North", "South"), each = 200)
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 20, method = "systematic") |>
    execute(frame, seed = 1)
  phase2 <- sampling_design() |>
    stratify_by(psu) |>
    cluster_by(id) |>
    draw(n = 2) |>
    execute(phase1, seed = 2)

  # The approximation enters through a design the sample itself does not hold.
  warning_text <- conditionMessage(
    tryCatch(as_svydesign(phase2), warning = function(w) w)
  )
  expect_match(warning_text, "phase 1")
  expect_no_warning(
    as_svydesign(phase2, systematic_variance = "approximate")
  )
})

## The stored call

test_that("exported two-phase and occasion designs print without their data", {
  skip_if_not_installed("survey")
  pop <- data.frame(id = 1:2000, st = rep(1:2, 1000), y = rnorm(2000))
  p1 <- sampling_design() |>
    stratify_by(st) |> cluster_by(id) |> draw(n = 300) |>
    execute(pop, seed = 1)
  p2 <- sampling_design() |> draw(n = 50) |> execute(p1, seed = 2)
  rotation <- data.frame(
    panel = rep(1:4, times = 4),
    period = rep(1:4, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
               FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  master <- sampling_design() |> stratify_by(st) |> draw(n = 200) |>
    execute(pop, seed = 3, panels = rotation)

  for (svy in list(as_svydesign(p2), as_svydesign(execute(master, period = 1)))) {
    expect_lte(length(utils::capture.output(print(svy))), 30L)
    expect_identical(svy$call$data, quote(data))
    expect_lt(length(deparse(svy$call)), 20L)
  }
})

test_that("the stored call keeps every argument and rebuilds the design", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    psu = rep(1:40, each = 5),
    st = rep(c("a", "b"), each = 100),
    size = rep(1:40, each = 5),
    y = rnorm(200)
  )
  s <- sampling_design() |>
    stratify_by(st) |>
    draw(n = 30, method = "pps_brewer", mos = size) |>
    execute(frame, seed = 1)
  svy <- as_svydesign(s, pps = "brewer", nest = FALSE, check.strata = TRUE)
  expect_identical(svy$call$pps, "brewer")
  expect_identical(svy$call$check.strata, TRUE)
  expect_identical(svy$call$nest, FALSE)

  rebuilt <- eval(svy$call, list(data = svy$variables), asNamespace("survey"))
  expect_equal(
    survey::SE(survey::svytotal(~y, rebuilt)),
    survey::SE(survey::svytotal(~y, svy))
  )

  # An object passed as a value is named, not printed.
  one <- sampling_design() |>
    draw(n = 12, method = "pps_brewer", mos = size) |>
    execute(frame, seed = 2)
  joint <- joint_expectation(one, frame)[[1]]
  svy <- as_svydesign(one, pps = survey::ppsmat(joint))
  expect_identical(svy$call$pps, quote(pps))
})

## The stages argument

ea_frame <- function() {
  frame <- data.frame(
    h = rep(c("a", "b"), each = 120),
    ea = rep(1:24, each = 10),
    hh = 1:240
  )
  frame$y <- frame$hh %% 7 + 3 * (frame$ea %% 4)
  frame
}

test_that("stages = 1 is the ultimate-cluster design of the sample", {
  skip_if_not_installed("survey")
  sample <- execute(
    sampling_design() |>
      add_stage() |> stratify_by(h) |> cluster_by(ea) |> draw(n = 5) |>
      add_stage() |> draw(n = 4),
    ea_frame(), seed = 1
  )
  hand <- survey::svydesign(ids = ~ea, strata = ~h, weights = ~.weight,
                            data = as.data.frame(sample))
  svy <- as_svydesign(sample, stages = 1)
  expect_null(svy$fpc$popsize)
  expect_identical(
    as.vector(survey::SE(survey::svytotal(~y, svy))),
    as.vector(survey::SE(survey::svytotal(~y, hand)))
  )
  # Its replicates resample the same units, and say nothing of a correction.
  expect_no_warning(rep <- as_svrepdesign(sample, type = "JKn", stages = 1))
  expect_equal(
    as.vector(survey::SE(survey::svytotal(~y, rep))),
    as.vector(survey::SE(survey::svytotal(~y, hand)))
  )
})

test_that("stages = 1 moves units taken with probability 1 down a stage", {
  skip_if_not_installed("survey")
  frame <- ea_frame()
  # Stratum a's five EAs are all taken.
  frame <- frame[frame$h == "b" | frame$ea <= 5, ]
  sample <- execute(
    sampling_design() |>
      add_stage() |> stratify_by(h) |> cluster_by(ea) |>
      draw(n = c(a = 5, b = 4)) |>
      add_stage() |> draw(n = 3),
    frame, seed = 2
  )
  svy <- as_svydesign(sample, stages = 1)
  data <- svy$variables
  whole <- data$h == "a"
  # Each EA of stratum a is a stratum of its households, and stratum b's
  # EAs are its clusters.
  expect_identical(length(unique(svy$strata[whole, 1])), 5L)
  expect_identical(length(unique(svy$strata[!whole, 1])), 1L)
  expect_identical(length(unique(svy$cluster[whole, 1])), sum(whole))
  expect_identical(length(unique(svy$cluster[!whole, 1])), 4L)
  data$unit <- ifelse(whole, paste("hh", data$hh), paste("ea", data$ea))
  data$stratum <- ifelse(whole, paste("ea", data$ea), "b")
  expected <- survey::svydesign(ids = ~unit, strata = ~stratum,
                                weights = ~.weight, data = data)
  expect_equal(as.vector(survey::SE(survey::svytotal(~y, svy))),
               as.vector(survey::SE(survey::svytotal(~y, expected))))
  rep <- as_svrepdesign(sample, type = "JKn", stages = 1)
  expect_equal(as.vector(survey::SE(survey::svytotal(~y, rep))),
               as.vector(survey::SE(survey::svytotal(~y, expected))))
  err <- expect_error(as_svrepdesign(sample, type = "JK1", stages = 1),
                      class = "samplyr_error_svrep_conversion_failed")
  expect_match(conditionMessage(err), "taken with probability 1", fixed = TRUE)
})

test_that("a unit moved down a stage keeps its variance beside a census unit", {
  skip_if_not_installed("survey")
  # EAs 1 and 2 are taken with certainty. EA 1's three households are all
  # taken at stage 2, EA 2 draws four of its twenty.
  sizes <- c(3, 20, rep(8, 10))
  frame <- do.call(rbind, lapply(1:12, function(e) {
    data.frame(ea = e, hh = seq_len(sizes[e]),
               mos = c(400, 400, rep(20, 10))[e])
  }))
  frame$y <- (seq_len(nrow(frame)) * 7) %% 11 + frame$ea
  sample <- suppressWarnings(execute(
    sampling_design() |>
      add_stage() |> cluster_by(ea) |>
      draw(n = 5, method = "pps_brewer", mos = mos, certainty_size = 300) |>
      add_stage() |> draw(n = 4),
    frame, seed = 1
  ))
  data <- as.data.frame(sample)
  expect_true(all(data$.certainty_1[data$ea %in% 1:2]))
  expect_identical(sum(data$ea == 1), 3L)
  svy <- as_svydesign(sample, stages = 1)
  # The design written by hand: EA 1 adds nothing, EA 2 is a stratum of its
  # households and the other EAs are its clusters, all read with
  # replacement.
  data <- data[data$ea != 1, ]
  data$unit <- ifelse(data$ea == 2, paste("hh", data$hh), paste("ea", data$ea))
  data$stratum <- ifelse(data$ea == 2, "ea 2", "drawn")
  expected <- survey::svydesign(ids = ~unit, strata = ~stratum,
                                weights = ~.weight, data = data)
  expect_equal(as.vector(survey::SE(survey::svytotal(~y, svy))),
               as.vector(survey::SE(survey::svytotal(~y, expected))),
               tolerance = 1e-12)
})

test_that("stages = 1:2 reads a unit moved down from stage 2 with replacement", {
  skip_if_not_installed("survey")
  frame <- do.call(rbind, lapply(1:8, function(e) {
    do.call(rbind, lapply(1:6, function(h) {
      data.frame(ea = e, hh = h, hsize = if (h == 1) 100 else 5,
                 person = 1:6)
    }))
  }))
  frame$y <- (seq_len(nrow(frame)) * 7) %% 13
  sample <- suppressWarnings(execute(
    sampling_design() |>
      add_stage() |> cluster_by(ea) |> draw(n = 4) |>
      add_stage() |> cluster_by(hh) |>
      draw(n = 3, method = "pps_brewer", mos = hsize, certainty_size = 50) |>
      add_stage() |> draw(n = 2),
    frame, seed = 2
  ))
  data <- as.data.frame(sample)
  expect_identical(data$.certainty_2, data$hh == 1)
  svy <- suppressMessages(as_svydesign(sample, stages = 1:2))
  # By hand: 4 of 8 EAs without replacement, then in each EA household 1 as a
  # stratum of its persons and the others as clusters, with replacement.
  data$one <- 1
  data$ea_count <- 8
  data$no_correction <- Inf
  data$unit <- ifelse(data$hh == 1, paste("p", data$person),
                      paste("hh", data$hh))
  data$stratum <- ifelse(data$hh == 1, "certain", "drawn")
  expected <- survey::svydesign(ids = ~ea + unit, strata = ~one + stratum,
                                fpc = ~ea_count + no_correction,
                                weights = ~.weight,
                                data = data, nest = TRUE)
  expect_equal(as.vector(survey::SE(survey::svytotal(~y, svy))),
               as.vector(survey::SE(survey::svytotal(~y, expected))),
               tolerance = 1e-12)
})

test_that("stages = 1 keeps empty units and makes census units a stratum", {
  skip_if_not_installed("survey")
  # PSU 1 lists nobody.
  roster <- data.frame(psu = rep(2:4, each = 5), id = 1:15, y = 1:15)
  sample <- execute(
    sampling_design() |>
      cluster_by(psu) |> draw(n = 3) |>
      add_stage() |> cluster_by(id) |> draw(frac = 1, on_empty = "silent"),
    list(data.frame(psu = 1:4), roster), seed = 1
  )
  svy <- suppressWarnings(as_svydesign(sample, stages = 1))
  expect_identical(sort(unique(svy$variables$psu)), c(1L, 3L, 4L))
  expect_identical(length(unique(svy$cluster[, 1])), 3L)

  # A single stage: stratum "all" is taken whole and adds no variance.
  frame <- data.frame(h = rep(c("all", "part"), c(5, 40)), id = 1:45)
  frame$y <- frame$id %% 6
  single <- execute(
    sampling_design() |> stratify_by(h) |> draw(n = c(all = 5, part = 10)),
    frame, seed = 3
  )
  svy <- as_svydesign(single, stages = 1)
  part <- as.data.frame(single)
  part <- part[part$h == "part", ]
  drawn <- survey::svydesign(ids = ~1, weights = ~.weight, data = part)
  expect_equal(as.vector(survey::SE(survey::svytotal(~y, svy))),
               as.vector(survey::SE(survey::svytotal(~y, drawn))))
  rep <- as_svrepdesign(single, type = "JKn", stages = 1)
  expect_equal(as.vector(survey::SE(survey::svytotal(~y, rep))),
               as.vector(survey::SE(survey::svytotal(~y, drawn))))
})

test_that("stages = 1 reads expected hits with replacement as draws", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:10, y = 1:10, x = rep(1:5, each = 2))
  cases <- list(
    srswr = draw(sampling_design(), n = 10, method = "srswr"),
    multinomial = draw(sampling_design(), n = 15, method = "pps_multinomial",
                       mos = x),
    chromy = draw(sampling_design(), n = 10, method = "pps_chromy", mos = x),
    systematic_mr = draw(sampling_design(), n = 10,
                         method = "pps_systematic_mr", mos = x)
  )
  for (case in names(cases)) {
    sample <- execute(cases[[case]], frame, seed = 4)
    hits <- 1 / sample$.weight
    # Expected hits fall below, at and above 1.
    if (case != "srswr") {
      expect_true(any(hits < 1) && any(hits == 1) && any(hits > 1))
    }
    hand <- survey::svydesign(ids = ~1, weights = ~.weight,
                              data = as.data.frame(sample))
    se <- as.vector(survey::SE(survey::svytotal(~y, hand)))
    expect_gt(se, 0)
    # A systematic stage warns about its approximation, acknowledged here.
    svy <- as_svydesign(sample, stages = 1, systematic_variance = "approximate")
    expect_equal(as.vector(survey::SE(survey::svytotal(~y, svy))), se,
                 info = case)
    rep <- as_svrepdesign(sample, type = "JK1", stages = 1,
                          systematic_variance = "approximate")
    expect_equal(
      as.vector(survey::SE(survey::svytotal(~y, rep))),
      as.vector(survey::SE(survey::svytotal(
        ~y, survey::as.svrepdesign(hand, type = "JK1")
      ))),
      info = case
    )
  }
})

test_that("stages = 1:2 carries a stage of one unit per parent", {
  skip_if_not_installed("survey")
  people <- ea_frame()[rep(1:240, each = 3), ]
  people$person <- seq_len(nrow(people))
  people$y <- people$person %% 5
  sample <- execute(
    sampling_design() |>
      add_stage() |> stratify_by(h) |> cluster_by(ea) |> draw(n = 5) |>
      add_stage() |> cluster_by(hh) |> draw(n = 4) |>
      add_stage() |> draw(n = 1),
    people, seed = 1
  )
  withr::local_options(survey.lonely.psu = "fail")
  expect_warning(as_svydesign(sample), class = "samplyr_warning_lonely_psu")
  svy <- as_svydesign(sample, stages = 1:2)
  # Stage 1 keeps its correction, 5 of 12 EAs, and stage 2 has none.
  expect_identical(unique(svy$fpc$popsize[, 1]), 12)
  expect_identical(unique(svy$fpc$popsize[, 2]), Inf)
  se <- as.vector(survey::SE(survey::svytotal(~y, svy)))
  expect_true(is.finite(se) && se > 0)
})

test_that("stages = 1:2 reads a unit taken whole by what lies below it", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # EA 1 has two households, so both are taken.
  kish <- function(persons, take) {
    frame <- do.call(rbind, lapply(1:5, function(e) {
      expand.grid(person = seq_len(if (e == 1) persons else 3),
                  hh = seq_len(if (e == 1) 2 else 4), ea = e)
    }))
    frame$y <- (seq_len(nrow(frame)) * 7) %% 11 + frame$ea
    execute(
      sampling_design() |>
        add_stage() |> cluster_by(ea) |> draw(n = 4) |>
        add_stage() |> cluster_by(hh) |> draw(n = 2) |>
        add_stage() |> draw(n = take),
      frame, seed = 1
    )
  }
  se <- function(design) as.vector(survey::SE(survey::svytotal(~y, design)))
  by_hand <- function(sample, unit, stratum, census = FALSE) {
    data <- as.data.frame(sample)
    whole <- data$ea == 1
    data$one <- 1
    data$f1 <- 5
    data$f2 <- ifelse(whole & census, 1, Inf)
    data$u2 <- ifelse(whole, unit(data), data$hh)
    data$s2 <- ifelse(whole, stratum(data), "drawn")
    survey::svydesign(ids = ~ea + u2, strata = ~one + s2, fpc = ~f1 + f2,
                      weights = ~.weight, data = data, nest = TRUE)
  }

  # One person drawn in each: the households stay draws of EA 1.
  one <- kish(3, 1)
  expect_true(1 %in% one$ea)
  expect_message(svy <- as_svydesign(one, stages = 1:2),
                 class = "samplyr_message_stage_certainty_drawn")
  expected <- by_hand(one, function(d) d$hh, function(d) "drawn")
  expect_true(is.finite(se(svy)))
  expect_equal(se(svy), se(expected))
  # The export keeps the recorded weights.
  expect_equal(as.vector(stats::weights(svy)), one$.weight)

  # Two persons drawn in each: each household is a stratum of its persons.
  two <- kish(3, 2)
  expect_no_message(svy <- as_svydesign(two, stages = 1:2),
                    class = "samplyr_message_stage_certainty_drawn")
  expected <- by_hand(two, function(d) paste(d$hh, d$person),
                      function(d) paste("hh", d$hh))
  expect_equal(se(svy), se(expected))

  # A household of one person is enumerated, a census stratum.
  whole <- kish(1, 1)
  expect_no_message(svy <- as_svydesign(whole, stages = 1:2),
                    class = "samplyr_message_stage_certainty_drawn")
  expected <- by_hand(whole, function(d) d$hh,
                      function(d) paste("hh", d$hh), census = TRUE)
  expect_equal(se(svy), se(expected))
})

test_that("stages = 1 reads a unit taken whole by what lies below it", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # Stratum a's EAs are all taken, and one household is drawn in each.
  listing <- function(size_a) {
    frame <- do.call(rbind, lapply(1:8, function(e) {
      data.frame(ea = e, h = if (e <= 2) "a" else "b",
                 hh = seq_len(if (e <= 2) size_a else 4))
    }))
    frame$y <- (seq_len(nrow(frame)) * 7) %% 11 + frame$ea
    frame
  }
  design <- function(n_a) {
    sampling_design() |>
      add_stage() |> stratify_by(h) |> cluster_by(ea) |>
      draw(n = c(a = n_a, b = 3)) |>
      add_stage() |> cluster_by(hh) |> draw(n = 1)
  }
  se <- function(design) as.vector(survey::SE(survey::svytotal(~y, design)))

  sample <- execute(design(2), listing(3), seed = 1)
  data <- as.data.frame(sample)
  expected <- survey::svydesign(ids = ~ea, strata = ~h, weights = ~.weight,
                                data = data)
  expect_message(svy <- as_svydesign(sample, stages = 1),
                 class = "samplyr_message_stage_certainty_drawn")
  expect_equal(se(svy), se(expected))
  expect_message(rep <- as_svrepdesign(sample, stages = 1, type = "JKn"),
                 class = "samplyr_message_stage_certainty_drawn")
  expect_equal(se(rep), se(expected))

  # With one household each, stratum a is enumerated and adds nothing.
  sample <- execute(design(2), listing(1), seed = 1)
  data <- as.data.frame(sample)
  expected <- survey::svydesign(ids = ~ea, strata = ~h, weights = ~.weight,
                                data = data[data$h == "b", ])
  expect_equal(se(as_svydesign(sample, stages = 1)), se(expected))
  expect_equal(se(as_svrepdesign(sample, stages = 1, type = "JKn")),
               se(expected))

  # A unit taken whole and alone in its stratum still has one unit there.
  frame <- listing(3)
  sample <- execute(design(1), frame[frame$ea != 2, ], seed = 1)
  expect_warning(
    suppressMessages(as_svydesign(sample, stages = 1)),
    class = "samplyr_warning_lonely_psu"
  )
})

test_that("stages = 1 checks stage support and crosses strata below certainty PSUs", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")

  # Random-size Stage 2 inside a certainty PSU is refused as in full export.
  bern_frame <- rbind(
    data.frame(psu = 1L, ssu = 1:40, mos = 1000, y = 10),
    data.frame(psu = 2L, ssu = 1:15, mos = 50, y = 2),
    data.frame(psu = 3L, ssu = 1:15, mos = 50, y = 3),
    data.frame(psu = 4L, ssu = 1:15, mos = 50, y = 4),
    data.frame(psu = 5L, ssu = 1:15, mos = 50, y = 5)
  )
  bern_sample <- execute(
    sampling_design() |>
      cluster_by(psu) |>
      draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = 500) |>
      add_stage() |>
      draw(frac = 0.5, method = "bernoulli"),
    bern_frame, seed = 1
  )
  expect_error(
    as_svydesign(bern_sample, stages = 1),
    class = "samplyr_error_multistage_poisson_later"
  )
  expect_error(
    as_svrepdesign(bern_sample, stages = 1, type = "JKn"),
    class = "samplyr_error_poisson_replicates"
  )

  # Unsupported spatial Stage 2 promoted into .taken_whole is refused by
  # linearization and JKn, and relaxed by subbootstrap.
  bern_frame$lon <- seq_len(nrow(bern_frame))
  lpm_sample <- execute(
    sampling_design() |>
      cluster_by(psu) |>
      draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = 500) |>
      add_stage() |>
      draw(n = 4, method = "lpm2", spread = lon),
    bern_frame, seed = 2
  )
  expect_error(
    as_svydesign(lpm_sample, stages = 1),
    class = "samplyr_error_custom_random_wor_export"
  )
  expect_error(
    as_svrepdesign(lpm_sample, stages = 1, type = "JKn"),
    class = "samplyr_error_custom_random_wor_export"
  )
  expect_s3_class(
    as_svrepdesign(lpm_sample, stages = 1, type = "subbootstrap", replicates = 20),
    "svyrep.design"
  )

  # Systematic Stage 2 promoted into .taken_whole triggers systematic checks.
  sys_sample <- execute(
    sampling_design() |>
      cluster_by(psu) |>
      draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = 500) |>
      add_stage() |>
      draw(n = 4, method = "systematic"),
    bern_frame, seed = 3
  )
  expect_warning(
    as_svydesign(sys_sample, stages = 1),
    class = "samplyr_warning_systematic_variance"
  )
  expect_error(
    as_svydesign(sys_sample, stages = 1, systematic_variance = "error"),
    class = "samplyr_error_systematic_variance"
  )

  # Stratified Stage 2 inside a certainty PSU crosses PSU with Stage-2 strata.
  strat_frame <- rbind(
    data.frame(psu = 1L, s2 = rep(c("U", "R"), each = 100), ssu = 1:200,
               mos = 5000, y = rep(c(100, 1), each = 100) + (1:200) %% 5),
    do.call(rbind, lapply(2:21, function(p) {
      data.frame(psu = p, s2 = "U", ssu = 1:20, mos = 40,
                 y = 10 + p %% 4 + (1:20) %% 3)
    }))
  )
  strat_sample <- execute(
    sampling_design() |>
      cluster_by(psu) |>
      draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = 1000) |>
      add_stage() |>
      stratify_by(s2) |>
      draw(n = 5),
    strat_frame, seed = 42
  )
  svy_st1 <- as_svydesign(strat_sample, stages = 1)
  rep_st1 <- as_svrepdesign(strat_sample, stages = 1, type = "JKn")
  df_hand <- as.data.frame(strat_sample)
  df_hand$unit <- ifelse(df_hand$psu == 1L, paste("ssu", df_hand$ssu),
                         paste("psu", df_hand$psu))
  df_hand$st <- ifelse(df_hand$psu == 1L, paste("cert", df_hand$s2), "prob")
  hand <- survey::svydesign(ids = ~unit, strata = ~st, weights = ~.weight,
                            data = df_hand)
  se_st1 <- as.vector(survey::SE(survey::svytotal(~y, svy_st1)))
  se_rep1 <- as.vector(survey::SE(survey::svytotal(~y, rep_st1)))
  se_hand <- as.vector(survey::SE(survey::svytotal(~y, hand)))
  se_full <- as.vector(survey::SE(survey::svytotal(~y, as_svydesign(strat_sample))))
  expect_equal(se_st1, 414.7288270666, tolerance = 1e-8)
  expect_equal(se_st1, se_hand, tolerance = 1e-10)
  expect_equal(se_rep1, se_hand, tolerance = 1e-10)
  expect_lt(abs(se_st1 - se_full) / se_full, 0.06)

  # A singleton Stage-2 stratum inside a certainty PSU triggers lonely warning.
  lonely_s2 <- execute(
    sampling_design() |>
      cluster_by(psu) |>
      draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = 1000) |>
      add_stage() |>
      stratify_by(s2) |>
      draw(n = c(U = 5, R = 1)),
    strat_frame, seed = 42
  )
  expect_warning(
    as_svydesign(lonely_s2, stages = 1),
    class = "samplyr_warning_lonely_psu"
  )

  # Kish stages = 1:2 when a certainty 1-adult HH sits next to a certainty
  # multi-adult HH in the same EA.
  kish_mix_frame <- do.call(rbind, lapply(1:5, function(e) {
    if (e == 1L) {
      rbind(
        data.frame(ea = 1L, hh = 1L, person = 1L),
        data.frame(ea = 1L, hh = 2L, person = 1:3)
      )
    } else {
      expand.grid(person = 1:3, hh = 1:4, ea = e)[c("ea", "hh", "person")]
    }
  }))
  kish_mix_frame$y <- (seq_len(nrow(kish_mix_frame)) * 7) %% 11 + kish_mix_frame$ea
  kish_mix <- execute(
    sampling_design() |>
      add_stage() |> cluster_by(ea) |> draw(n = 4) |>
      add_stage() |> cluster_by(hh) |> draw(n = 2) |>
      add_stage() |> draw(n = 1),
    kish_mix_frame, seed = 1
  )
  msg <- expect_message(
    svy_kish <- as_svydesign(kish_mix, stages = 1:2),
    class = "samplyr_message_stage_certainty_drawn"
  )
  expect_identical(msg$n_units, 1L)
  se_kish <- as.vector(survey::SE(survey::svytotal(~y, svy_kish)))
  expect_true(is.finite(se_kish) && se_kish > 0)
})

test_that("stages is refused where the export has no such form", {
  skip_if_not_installed("survey")
  sample <- execute(
    sampling_design() |>
      add_stage() |> cluster_by(ea) |> draw(n = 5) |>
      add_stage() |> draw(n = 4),
    ea_frame(), seed = 1
  )
  for (bad in list(2, c(1, 3), 1:3, "1", 0.5)) {
    expect_error(as_svydesign(sample, stages = bad),
                 class = "samplyr_error_survey_argument")
  }
  expect_error(as_svydesign(sample, stages = 1, pps = "brewer"),
               class = "samplyr_error_survey_argument")
  for (type in c("mrbbootstrap", "rwyb")) {
    expect_error(as_svrepdesign(sample, type = type, stages = 1),
                 class = "samplyr_error_survey_argument")
  }
  expect_error(as_svrepdesign(sample, stages = 1:2),
               class = "samplyr_error_survey_argument")

  phase1 <- execute(
    sampling_design() |> cluster_by(ea) |> draw(n = 12), ea_frame(), seed = 1
  )
  phase2 <- execute(
    sampling_design() |>
      add_stage() |> cluster_by(ea) |> draw(n = 6) |>
      add_stage() |> draw(n = 2),
    phase1, ea_frame(), seed = 2
  )
  expect_true(samplyr:::survey_phase_info(phase2)$is_twophase)
  expect_error(as_svydesign(phase2, stages = 1),
               class = "samplyr_error_survey_argument")

  # An occasion is exported by ultimate clusters already.
  master <- execute(
    sampling_design() |>
      add_stage() |> stratify_by(h) |> cluster_by(ea) |> draw(n = 6) |>
      add_stage() |> draw(n = 2),
    ea_frame(), seed = 1,
    panels = data.frame(panel = rep(1:2, 2), period = rep(1:2, each = 2),
                        active = c(TRUE, FALSE, FALSE, TRUE))
  )
  occasion <- execute(master, period = 1)
  expect_equal(
    as.vector(survey::SE(survey::svytotal(~y, as_svydesign(occasion)))),
    as.vector(survey::SE(survey::svytotal(
      ~y, as_svydesign(occasion, stages = 1)
    )))
  )
  expect_error(as_svydesign(occasion, stages = 1:2),
               class = "samplyr_error_survey_argument")
  expect_error(as_svrepdesign(occasion, stages = 1:2),
               class = "samplyr_error_survey_argument")
})

## Stage-1 units taken with probability 1 in the replicate export

# A jackknife of a total gives the linearized variance of the same strata
# and units, so each expectation is the design the conversion should build,
# written for survey directly. Inside a PSU taken with certainty, stage 2
# keeps its correction, given as the stage-2 population count. Elsewhere a
# count of 1e300 leaves a factor of 1 in double precision.
replicate_se <- function(sample, type = "JKn") {
  rep <- suppressWarnings(as_svrepdesign(sample, type = type))
  as.vector(survey::SE(survey::svytotal(~y, rep)))
}
direct_se <- function(data, ids, strata, fpc = NULL) {
  svy <- survey::svydesign(ids = ids, strata = strata, weights = ~.weight,
                           fpc = fpc, data = data)
  as.vector(survey::SE(survey::svytotal(~y, svy)))
}

test_that("a first stage taken whole leaves its later stage's variance", {
  skip_if_not_installed("survey")
  frame <- data.frame(psu = rep(1:12, each = 6), id = 1:72)
  frame$y <- frame$id %% 5 + frame$psu
  sample <- execute(
    sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 12) |>
      add_stage() |> draw(n = 3),
    frame, seed = 1
  )
  # Every PSU is a stratum of its households, 3 drawn of 6.
  data <- as.data.frame(sample)
  expect_equal(replicate_se(sample), direct_se(data, ~id, ~psu, ~.fpc_2))
  expect_equal(replicate_se(sample, "auto"),
               direct_se(data, ~id, ~psu, ~.fpc_2))
  expect_gt(replicate_se(sample), 0)
  err <- expect_error(as_svrepdesign(sample, type = "JK1"),
                      class = "samplyr_error_svrep_conversion_failed")
  expect_match(conditionMessage(err), "taken with probability 1", fixed = TRUE)
})

test_that("a stratum taken whole is read as certainty units", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    h = rep(c("whole", "drawn"), times = c(4, 8) * 6),
    psu = rep(1:12, each = 6),
    id = 1:72
  )
  frame$y <- frame$id %% 7 + 2 * frame$psu
  design <- sampling_design() |>
    add_stage() |> stratify_by(h) |> cluster_by(psu) |>
    draw(n = c(whole = 4, drawn = 3)) |>
    add_stage() |> draw(n = 3)
  sample <- execute(design, frame, seed = 2)
  data <- as.data.frame(sample)
  # The whole stratum's PSUs are strata of their households, 3 drawn of 6,
  # and the drawn stratum's PSUs are drawn with replacement.
  sure <- data$h == "whole"
  data$unit <- ifelse(sure, paste("hh", data$id), paste("psu", data$psu))
  data$stratum <- ifelse(sure, paste("psu", data$psu), "drawn")
  data$count <- ifelse(sure, data$.fpc_2, 1e300)
  expected <- direct_se(data, ~unit, ~stratum, ~count)
  for (type in c("JKn", "auto")) {
    expect_equal(replicate_se(sample, type), expected, info = type)
  }
  expect_warning(as_svrepdesign(sample, type = "JKn"),
                 class = "samplyr_warning_replicate_wr_first_stage")

  # The report reads the same conversion from the frame.
  report <- variance_estimators(design, frame)
  status <- function(type) {
    report$status[report$call == sprintf('as_svrepdesign(x, type = "%s")',
                                          type)]
  }
  expect_identical(status("JK1"), "refused")
  expect_identical(status("JKn"), "approximate")
  # Each whole PSU keeps three households, which BRR cannot pair.
  expect_identical(status("BRR"), "refused")
  expect_error(suppressWarnings(as_svrepdesign(sample, type = "BRR")),
               class = "samplyr_error_svrep_conversion_failed")
})

test_that("a single stage taken whole keeps its exact correction", {
  skip_if_not_installed("survey")
  frame <- data.frame(h = rep(c("all", "part"), c(5, 40)), id = 1:45)
  frame$y <- frame$id %% 6
  sample <- execute(
    sampling_design() |> stratify_by(h) |> draw(n = c(all = 5, part = 10)),
    frame, seed = 3
  )
  expect_no_warning(rep <- as_svrepdesign(sample, type = "JKn"))
  expect_equal(
    as.vector(survey::SE(survey::svytotal(~y, rep))),
    as.vector(survey::SE(survey::svytotal(~y, as_svydesign(sample))))
  )
})

## A census at every stage

# Oracle: a census estimates the population total exactly, with no
# sampling variance, whatever the method that took it.
expect_census <- function(design, total, info = NULL) {
  est <- survey::svytotal(~y, design)
  expect_equal(unname(coef(est)), total, info = info)
  expect_equal(as.vector(survey::SE(est)), 0, info = info)
}

test_that("a census exports with zero variance whatever the method", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:4, mos = c(10, 20, 30, 40), y = c(2, 10, 3, 9))
  for (method in c("pps_multinomial", "pps_chromy", "pps_systematic_mr")) {
    sample <- sampling_design() |> cluster_by(id) |>
      draw(n = 4, method = method, mos = mos, certainty_size = 1) |>
      execute(frame, seed = 1)
    expect_true(all(sample$.certainty_1))
    expect_census(as_svydesign(sample, systematic_variance = "approximate"),
      24, info = method)
    for (type in c("bootstrap", "JKn", "subbootstrap", "mrbbootstrap")) {
      # The with-replacement reading warns; a census has nothing to read.
      rep <- suppressWarnings(withr::with_seed(1, as_svrepdesign(sample,
        type = type, replicates = 20, systematic_variance = "approximate")))
      expect_census(rep, 24, info = paste(method, type))
    }
  }
  for (method in c("pps_brewer", "pps_cps", "pps_sampford", "pps_systematic",
                   "cube")) {
    sample <- sampling_design() |> cluster_by(id) |>
      draw(n = 4, method = method, mos = mos) |>
      execute(frame, seed = 1)
    expect_census(as_svydesign(sample, systematic_variance = "approximate"),
      24, info = method)
  }
  expect_census(as_svydesign(sample, stages = 1), 24)
  brewer <- sampling_design() |> cluster_by(id) |>
    draw(n = 4, method = "pps_brewer", mos = mos) |> execute(frame, seed = 1)
  expect_census(
    as_svydesign(brewer, pps = survey::ppsmat(joint_expectation(brewer)[[1]])),
    24
  )

  # Population counts of one: each stratum holds one unit.
  strata <- transform(frame, st = id)
  sample <- sampling_design() |> stratify_by(st) |> draw(n = 1) |>
    execute(strata, seed = 1)
  expect_census(as_svydesign(sample), 24)

  frame <- expand.grid(hh = 1:3, psu = 1:3)
  frame$mos <- frame$psu
  frame$y <- frame$hh * frame$psu
  sample <- sampling_design() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_brewer", mos = mos) |> add_stage() |>
    draw(n = 3) |> execute(frame, seed = 1)
  expect_census(as_svydesign(sample), 36)
  expect_census(as_svydesign(sample, stages = 1), 36)
  expect_census(suppressWarnings(as_svrepdesign(sample, type = "JKn")), 36)
  expect_census(suppressWarnings(withr::with_seed(1,
    as_svrepdesign(sample, type = "rwyb", replicates = 20))), 36)
})

test_that("a PPS phase 2 taking all of phase 1 leaves phase 1's variance", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:20, y = 1:20, m = 1:20)
  phase1 <- sampling_design() |> draw(n = 8) |> execute(frame, seed = 2)
  # Oracle: the SRS variance of phase 1 alone.
  target <- 20^2 * (1 - 8 / 20) * stats::var(phase1$y) / 8
  for (method in c("srswor", "pps_brewer", "pps_sampford")) {
    design <- if (method == "srswor") {
      draw(sampling_design(), n = 8)
    } else {
      draw(sampling_design(), n = 8, method = method, mos = m)
    }
    phase2 <- execute(design, phase1, seed = 1)
    est <- survey::svytotal(~y, as_svydesign(phase2))
    expect_equal(unname(coef(est)), 20 / 8 * sum(phase1$y), info = method)
    expect_equal(as.vector(survey::SE(est))^2, target, info = method)
  }
})

test_that("a single primary unit taken with certainty exports", {
  skip_if_not_installed("survey")
  # One element: the population, taken with weight one.
  for (method in c("srswor", "pps_brewer")) {
    design <- if (method == "srswor") {
      sampling_design() |> cluster_by(id) |> draw(n = 1)
    } else {
      sampling_design() |> cluster_by(id) |>
        draw(n = 1, method = method, mos = mos)
    }
    one <- execute(design, data.frame(id = 1L, mos = 1, y = 7), seed = 1)
    expect_census(as_svydesign(one), 7, info = method)
    expect_census(as_svydesign(one, stages = 1), 7, info = method)
    # survey's JKn refuses an SRS census of every stratum, as it always has.
    types <- c("bootstrap", "rwyb", if (method != "srswor") "JKn")
    for (type in types) {
      rep <- suppressWarnings(withr::with_seed(1,
        as_svrepdesign(one, type = type, replicates = 5)))
      expect_census(rep, 7, info = paste(method, type))
    }
  }

  # One parent with census descendants, then with a subsample of them.
  frame <- data.frame(psu = 1L, hh = 1:3, mos = 10, y = c(1, 2, 6))
  for (method in c("srswor", "pps_brewer", "pps_multinomial")) {
    first <- sampling_design() |> cluster_by(psu)
    first <- switch(method,
      srswor = draw(first, n = 1),
      pps_brewer = draw(first, n = 1, method = method, mos = mos),
      pps_multinomial = draw(first, n = 1, method = method, mos = mos,
                             certainty_size = 1)
    )
    all <- first |> add_stage() |> draw(n = 3) |> execute(frame, seed = 1)
    expect_census(as_svydesign(all), 9, info = method)
    part <- first |> add_stage() |> draw(n = 2) |> execute(frame, seed = 1)
    # Oracle: the SRS variance of the two households within the parent.
    est <- survey::svytotal(~y, as_svydesign(part))
    expect_equal(unname(coef(est)), 3 / 2 * sum(part$y), info = method)
    expect_equal(as.vector(survey::SE(est))^2,
      3^2 * (1 - 2 / 3) * stats::var(part$y) / 2, info = method)
  }

  # Strata below the parent divide it, they do not make several parents.
  frame <- data.frame(psu = 1L, hh = 1:6, st = rep(1:3, each = 2),
                      mos = 10, y = c(3, 1, 4, 1, 5, 9))
  for (method in c("srswor", "pps_brewer")) {
    first <- sampling_design() |> cluster_by(psu)
    first <- if (method == "srswor") {
      draw(first, n = 1)
    } else {
      draw(first, n = 1, method = method, mos = mos)
    }
    all <- suppressWarnings(first |> add_stage() |> stratify_by(st) |>
      draw(n = 2) |> execute(frame, seed = 1))
    expect_census(as_svydesign(all), 23, info = method)
    one_each <- frame[!duplicated(frame$st), ]
    all <- suppressWarnings(first |> add_stage() |> stratify_by(st) |>
      draw(n = 1) |> execute(one_each, seed = 1))
    expect_census(as_svydesign(all), sum(one_each$y), info = method)
  }
  frame <- data.frame(psu = 1L, hh = 1:6, st = rep(1:2, each = 3),
                      y = c(3, 1, 4, 1, 5, 9))
  part <- sampling_design() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> stratify_by(st) |> draw(n = 2) |> execute(frame, seed = 1)
  # Oracle: the SRS variance of two of three households in each stratum.
  target <- sum(vapply(split(part$y, part$st), function(y) {
    3^2 * (1 - 2 / 3) * stats::var(y) / 2
  }, 0))
  expect_equal(as.vector(survey::SE(survey::svytotal(~y,
    as_svydesign(part))))^2, target)

  # One unit drawn from several has no variance to estimate.
  frame <- data.frame(psu = rep(1:3, each = 2), y = 1:6)
  lone <- sampling_design() |> cluster_by(psu) |> draw(n = 1) |>
    execute(frame, seed = 1)
  expect_error(suppressWarnings(as_svydesign(lone)),
    class = "samplyr_error_export_one_unit")
})

test_that("phase 2 of a one-unit population phase 1 is a sample of it", {
  skip_if_not_installed("survey")
  # The proof reads the sample alone, so the frame digest changes nothing.
  population <- data.frame(id = 1L, mos = 1, y = 7)
  first <- list(
    srswor = sampling_design() |> draw(n = 1),
    certain_wr = sampling_design() |>
      draw(n = 1, method = "pps_multinomial", mos = mos, certainty_size = 1)
  )
  for (digest in c("summary", "full", "none")) {
    for (design1 in names(first)) {
      phase1 <- execute(first[[design1]], population, seed = 1,
                        frame_digest = digest)
      for (method in c("pps_sampford", "pps_brewer", "srswor")) {
        design2 <- if (method == "srswor") {
          sampling_design() |> draw(n = 1)
        } else {
          sampling_design() |> draw(n = 1, method = method, mos = mos)
        }
        phase2 <- execute(design2, phase1, seed = 1)
        expect_census(as_svydesign(phase2), 7,
          info = paste(digest, design1, method))
      }
    }
  }

  # Phase 1 is one certain cluster holding the whole population.
  frame <- data.frame(psu = 1L, hh = 1:6, m = 1:6, y = c(3, 1, 4, 1, 5, 9))
  srs <- sampling_design() |> draw(n = 3)
  for (digest in c("summary", "none")) {
    phase1 <- sampling_design() |> cluster_by(psu) |> draw(n = 1) |>
      execute(frame, seed = 1, frame_digest = digest)
    phase2 <- execute(srs, phase1, seed = 1)
    # Oracle: the SRS variance of 3 of the 6 population units.
    est <- survey::svytotal(~y, as_svydesign(phase2))
    expect_equal(unname(coef(est)), 2 * sum(phase2$y), info = digest)
    expect_equal(as.vector(survey::SE(est))^2,
      6^2 * (1 - 3 / 6) * stats::var(phase2$y) / 3, info = digest)
  }
  # A phase 1 changed after execution is not read as fixed.
  changed <- suppressWarnings(execute(srs, phase1[-1, ], seed = 1))
  expect_false(samplyr:::phase1_is_fixed_one_unit(phase1[-1, ]))
  expect_error(as_svydesign(changed), class = "samplyr_error_modified_sample")
  sampford <- sampling_design() |> draw(n = 3, method = "pps_sampford", mos = m)
  phase2 <- execute(sampford, phase1, seed = 1)
  direct <- execute(sampford, frame, seed = 1)
  expect_identical(phase2$hh, direct$hh)
  expect_equal(
    survey::SE(survey::svytotal(~y, as_svydesign(phase2))),
    survey::SE(survey::svytotal(~y, as_svydesign(direct)))
  )
  expect_identical(variance_estimators(phase2)$status,
                   variance_estimators(sampford, frame)$status)
  expect_identical(variance_estimators(sampford, phase1)$status,
                   variance_estimators(sampford, frame)$status)
  expect_error(as_svydesign(phase2, method = "full"),
    class = "samplyr_error_survey_argument")

  # One unit sampled from five is not the population.
  phase1 <- sampling_design() |> draw(n = 1) |>
    execute(data.frame(id = 1:5, mos = 1, y = 1:5), seed = 1)
  phase2 <- execute(sampling_design() |> draw(n = 1, method = "pps_sampford",
    mos = mos), phase1, seed = 1)
  err <- expect_error(suppressWarnings(as_svydesign(phase2)),
    class = "samplyr_error_export_one_unit")
  expect_match(conditionMessage(err), "not taken with certainty", fixed = TRUE)

  # Phase 1 takes the frame's only PSU whole and subsamples it. A phase 2
  # stratified across it goes to survey's two-phase design, which refuses
  # one phase-1 unit although it was taken with certainty, so the reason
  # names the certainty rather than a draw from several.
  frame <- data.frame(psu = 1, id = 1:12, g = rep(c("a", "b"), 6), y = 1:12)
  first <- sampling_design() |> add_stage() |> cluster_by(psu) |>
    draw(frac = 1) |> add_stage() |> draw(n = 8)
  across <- sampling_design() |> stratify_by(g) |> draw(n = 2)
  phase1 <- execute(first, frame, seed = 1)
  phase2 <- execute(across, phase1, seed = 2)
  err <- expect_error(as_svydesign(phase2),
                      class = "samplyr_error_export_one_unit")
  expect_match(conditionMessage(err), "taken with certainty", fixed = TRUE)
  expect_no_match(conditionMessage(err), "not taken with certainty",
                  fixed = TRUE)
  row <- variance_estimators(phase2)[1, ]
  expect_identical(row$status, "refused")
  expect_match(row$note, "taken with certainty", fixed = TRUE)
  # The two routes the message names give a design.
  within <- execute(sampling_design() |> draw(n = 4), phase1, seed = 2)
  expect_s3_class(as_svydesign(within), "survey.design2")
  reps <- execute(across, execute(first, frame, seed = 1, reps = 4), seed = 2)
  se <- survey::SE(survey::svytotal(
    ~y, as_svrepdesign(reps, type = "random_groups")
  ))
  expect_gt(as.vector(se), 0)
  # A phase 1 of several units keeps the two-phase export.
  phase1 <- sampling_design() |> draw(n = 20) |>
    execute(data.frame(id = 1:20, m = 1:20, y = 1:20), seed = 1)
  phase2 <- execute(sampford, phase1, seed = 1)
  expect_s3_class(as_svydesign(phase2), "twophase2")
})

test_that("final weights of one do not make phase 1 the population", {
  skip_if_not_installed("survey")
  # One of two clusters, then its only household drawn twice with
  # replacement: the stage weights 2 and 0.5 cancel. Totals 20 and 40 are
  # equally likely, so the true variance is 100, not 0.
  frame <- data.frame(psu = 1:2, hh = 1:2, mos = 1, y = c(10, 20))
  design1 <- sampling_design() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> draw(n = 2, method = "srswr")
  design2 <- sampling_design() |>
    draw(n = 2, method = "pps_sampford", mos = mos)
  for (seed in c(1, 4)) {
    phase1 <- execute(design1, frame, seed = seed, frame_digest = "none")
    expect_true(all(phase1$.weight == 1))
    phase2 <- execute(design2, phase1, seed = 1)
    expect_error(as_svydesign(phase2),
      class = "samplyr_error_twophase_stage_probs")
    expect_identical(variance_estimators(phase2)$class[1],
                     "samplyr_error_twophase_stage_probs")
  }

  # A random-size phase 1 that drew only its certain unit left four out.
  frame <- data.frame(id = 1:5, mos = c(100, 1, 1, 1, 1), y = c(50, 1:4))
  for (digest in c("summary", "none")) {
    phase1 <- suppressWarnings(sampling_design() |>
      draw(n = 2, method = "pps_poisson", mos = mos) |>
      execute(frame, seed = 1, frame_digest = digest))
    expect_identical(phase1$id, 1L)
    expect_identical(phase1$.weight, 1)
    phase2 <- execute(sampling_design() |>
      draw(n = 1, method = "pps_sampford", mos = mos), phase1, seed = 1)
    expect_error(as_svydesign(phase2),
      class = "samplyr_error_twophase_poisson")
  }
  # A Bernoulli stage of fraction one gives every unit chance one.
  phase1 <- sampling_design() |> draw(frac = 1, method = "bernoulli") |>
    execute(data.frame(id = 1L, mos = 1, y = 7), seed = 1,
            frame_digest = "none")
  phase2 <- execute(sampling_design() |>
    draw(n = 1, method = "pps_sampford", mos = mos), phase1, seed = 1)
  expect_census(as_svydesign(phase2), 7)
})

test_that("a phase 1 changed after execution proves no unit certain", {
  skip_if_not_installed("survey")
  # Clusters 1 and 2 are certain and the third draw is random. Removing
  # the random cluster's rows leaves only certain ones, which proves
  # nothing about the hits the phase drew.
  frame <- data.frame(
    psu = rep(1:4, each = 6), hh = rep(1:6, 4),
    mos = rep(c(100, 100, 1, 1), each = 6), m = 1,
    y = rep(c(1, 2, 4, 8, 16, 32), 4) * rep(1:4, each = 6)
  )
  within <- sampling_design() |> stratify_by(psu) |>
    draw(n = 2, method = "pps_sampford", mos = m)
  refused <- function(phase1, info) {
    phase2 <- suppressWarnings(execute(within, phase1, seed = 1))
    expect_error(as_svydesign(phase2), class = "samplyr_error_modified_sample",
                 info = info)
    expect_identical(variance_estimators(phase2)$class[1],
                     "samplyr_error_modified_sample", info = info)
    phase2
  }
  for (method in c("pps_multinomial", "pps_chromy")) {
    two <- sampling_design() |> cluster_by(psu) |>
      draw(n = 3, method = method, mos = mos, certainty_size = 50) |>
      add_stage() |> draw(n = 4)
    phase1 <- execute(two, frame, seed = 1)
    expect_false(all(phase1$.certainty_1))
    cut <- phase1[phase1$.certainty_1, ]
    expect_false(samplyr:::sample_realization_status(cut)$ok)
    # The rows left are all certain, and still prove no stage certain.
    expect_length(samplyr:::phase_wr_stages_certain(cut, two, 1:2), 0L)
    refused(cut, method)
    refused(cut[cut$psu == cut$psu[1], ], paste(method, "one cluster"))
    one <- sampling_design() |> cluster_by(psu) |>
      draw(n = 3, method = method, mos = mos, certainty_size = 50)
    phase1 <- execute(one, frame, seed = 1)
    phase2 <- refused(phase1[phase1$.certainty_1, ], method)
    # Unmodified, the same phase 2 is refused for its random hits.
    expect_error(as_svydesign(execute(within, phase1, seed = 1)),
                 class = "samplyr_error_twophase_wr_subsample")
  }
  # The weights still serve, but no export reads the rows as phase 1,
  # with or without replacement.
  across <- sampling_design() |> draw(n = 3)
  phase1 <- execute(two, frame, seed = 1)
  phase2 <- suppressWarnings(execute(across, phase1[phase1$.certainty_1, ],
                                     seed = 1))
  expect_error(as_svydesign(phase2, method = "approx"),
               class = "samplyr_error_modified_sample")
  srs <- sampling_design() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> draw(n = 4)
  phase1 <- execute(srs, frame, seed = 1)
  phase2 <- suppressWarnings(execute(across,
    phase1[phase1$psu != phase1$psu[1], ], seed = 1))
  expect_error(as_svydesign(phase2), class = "samplyr_error_modified_sample")
})

test_that("a random-size stratum that drew nothing keeps phase 1 random", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("rs_custom"), add = TRUE)
  sondage::register_method(
    "rs_custom", "wor",
    sample_fn = function(pik, n = NULL, prn = NULL, ...) {
      which(stats::runif(length(pik)) < pik)
    },
    fixed_size = FALSE, probabilities = "exact"
  )
  # Unit 1 is taken for sure. Stratum b is random, and where it selects
  # nothing it leaves no row and no record, so phase 1 is still random.
  frame <- data.frame(st = c("a", rep("b", 4)), id = 1:5,
                      y = c(7, 10, 20, 30, 40), m = 1,
                      mos = c(100, 1, 1, 1, 1))
  cluster <- data.frame(psu = 1L)
  households <- cbind(psu = 1L, frame)
  half <- data.frame(st = c("a", "b"), frac = c(1, 0.5))
  one_each <- data.frame(st = c("a", "b"), n = c(1, 1))
  random <- list(
    bernoulli = list(sampling_design() |> stratify_by(st) |>
      draw(frac = half, method = "bernoulli", on_empty = "silent"),
      list(frame)),
    pps_poisson = list(sampling_design() |> stratify_by(st) |>
      draw(n = one_each, method = "pps_poisson", mos = mos,
           on_empty = "silent"), list(frame)),
    # Fraction one is not chance one proportional to size.
    pps_poisson_one = list(sampling_design() |>
      draw(frac = 1, method = "pps_poisson", mos = mos, on_empty = "silent"),
      list(frame)),
    custom = list(sampling_design() |> stratify_by(st) |>
      draw(n = one_each, method = "pps_rs_custom", mos = mos,
           on_empty = "silent"), list(frame)),
    later = list(sampling_design() |> cluster_by(psu) |> draw(frac = 1) |>
      add_stage() |> stratify_by(st) |>
      draw(frac = half, method = "bernoulli", on_empty = "silent"),
      list(cluster, households)),
    # One size for every stratum: b's count is not recorded once it drew
    # nothing.
    size_value = list(sampling_design() |> stratify_by(st) |>
      draw(n = 1, method = "bernoulli", on_empty = "silent"), list(frame)),
    # The size exhausts stratum a, and b, larger, drew nothing.
    size = list(sampling_design() |> stratify_by(st) |>
      draw(n = one_each, method = "bernoulli", on_empty = "silent"),
      list(frame))
  )
  design2 <- sampling_design() |>
    draw(n = 1, method = "pps_sampford", mos = m)
  # pps_poisson of fraction one warns that its expected size falls short.
  run <- function(args) suppressMessages(suppressWarnings(
    do.call(execute, args)
  ))
  reached <- character(0)
  for (name in names(random)) {
    for (seed in 1:40) {
      args <- c(list(random[[name]][[1]]), random[[name]][[2]], seed = seed)
      phase1 <- run(args)
      if (!identical(phase1$id, 1L)) next
      reached <- c(reached, name)
      for (digest in c("summary", "full", "none")) {
        args$frame_digest <- digest
        phase1 <- run(args)
        phase2 <- execute(design2, phase1, seed = 1)
        info <- paste(name, seed, digest)
        expect_error(as_svydesign(phase2),
          class = "samplyr_error_twophase_poisson", info = info)
        expect_identical(variance_estimators(phase2)$class[1],
                         "samplyr_error_twophase_poisson", info = info)
      }
      break
    }
  }
  expect_setequal(reached, names(random))

  # Rung 1: the stage is proved to take every unit of every pool, so phase
  # 2 is a sample of that population. A fraction of one, as a value or a
  # table, or a size that reaches each population count, where the table
  # names every stratum of the frame or there are none.
  ones <- list(
    value = list(sampling_design() |> draw(frac = 1, method = "bernoulli"),
                 list(frame[1, ])),
    table = list(sampling_design() |> stratify_by(st) |>
      draw(frac = data.frame(st = "a", frac = 1), method = "bernoulli"),
      list(frame[1, ])),
    size = list(sampling_design() |> stratify_by(st) |>
      draw(n = data.frame(st = "a", n = 1), method = "bernoulli"),
      list(frame[1, ])),
    size_value = list(sampling_design() |> draw(n = 1, method = "bernoulli"),
                      list(frame[1, ]))
  )
  for (name in names(ones)) {
    for (digest in c("summary", "full", "none")) {
      phase1 <- do.call(execute, c(list(ones[[name]][[1]]),
                                   ones[[name]][[2]], seed = 1,
                                   frame_digest = digest))
      phase2 <- execute(design2, phase1, seed = 1)
      expect_census(as_svydesign(phase2), 7, info = paste(name, digest))
      expect_false(identical(variance_estimators(phase2)$status[1],
                             "refused"), info = paste(name, digest))
    }
  }
  # Inside one certain cluster, execution records each pool a random-size
  # draw left empty. Where it names none, the pools are those with rows, so
  # a stage that took every unit of each with chance one is proved, by any
  # method. Without the record nothing shows that no pool drew nothing.
  census_of <- function(phase1, info) {
    phase2 <- execute(sampling_design() |>
      draw(n = 2, method = "pps_sampford", mos = m), phase1, seed = 1)
    est <- survey::svytotal(~y, as_svydesign(phase2))
    expect_equal(unname(coef(est)), 2.5 * sum(phase2$y), info = info)
    expect_equal(as.vector(vcov(est)),
                 5^2 * (1 - 2 / 5) * var(phase2$y) / 2, info = info)
    expect_false(identical(variance_estimators(phase2)$status[1],
                           "refused"), info = info)
  }
  refused <- function(phase1, info) {
    phase2 <- execute(design2, phase1, seed = 1)
    expect_error(as_svydesign(phase2),
      class = "samplyr_error_twophase_poisson", info = info)
    expect_identical(variance_estimators(phase2)$class[1],
                     "samplyr_error_twophase_poisson", info = info)
  }
  within <- function(n, method) {
    sizes <- data.frame(st = c("a", "b"), n = n)
    stage <- sampling_design() |> cluster_by(psu) |> draw(frac = 1) |>
      add_stage() |> stratify_by(st)
    if (method == "bernoulli") {
      draw(stage, n = sizes, method = method, on_empty = "silent")
    } else {
      draw(stage, n = sizes, method = method, mos = mos, on_empty = "silent")
    }
  }
  for (method in c("bernoulli", "pps_poisson", "pps_rs_custom")) {
    for (digest in c("summary", "full", "none")) {
      info <- paste(method, digest)
      # Sizes that reach every count take every unit.
      phase1 <- run(list(within(c(1, 4), method), cluster, households,
                         seed = 1, frame_digest = digest))
      expect_identical(nrow(phase1), 5L)
      census_of(phase1, info)
      unknown <- phase1
      attr(unknown, "metadata")$empty_pools <- NULL
      refused(unknown, paste(info, "without the record"))
      # A size short of a count leaves the stage random, whatever it drew.
      for (seed in 1:4) {
        refused(run(list(within(c(1, 1), method), cluster, households,
                         seed = seed, frame_digest = digest)),
                paste(info, seed))
      }
    }
  }
  # Inside one certain cluster: two of five by equal probabilities.
  design1 <- sampling_design() |> cluster_by(psu) |> draw(frac = 1) |>
    add_stage() |> stratify_by(st) |>
    draw(frac = data.frame(st = c("a", "b"), frac = 1), method = "bernoulli")
  for (digest in c("summary", "full", "none")) {
    phase1 <- execute(design1, cluster, households, seed = 1,
                      frame_digest = digest)
    expect_identical(nrow(phase1), 5L)
    phase2 <- execute(sampling_design() |>
      draw(n = 2, method = "pps_sampford", mos = m), phase1, seed = 1)
    est <- survey::svytotal(~y, as_svydesign(phase2))
    expect_equal(unname(coef(est)), 2.5 * sum(phase2$y), info = digest)
    expect_equal(as.vector(vcov(est)),
                 5^2 * (1 - 2 / 5) * var(phase2$y) / 2, info = digest)
  }
})

test_that("a unit an empty stage emptied still counts against a fixed phase 1", {
  skip_if_not_installed("survey")
  # Cluster 1 is certain, the other draw takes cluster 2 or 3, and cluster
  # 2 has no households. Seed 1 draws it, which leaves only cluster 1's
  # rows, each of weight one: totals 7 and 147 are equally likely.
  psus <- data.frame(psu = 1:3, mos = c(100, 1, 1))
  households <- data.frame(
    psu = rep(c(1L, 3L), each = 3), hh = rep(1:3, 2),
    y = c(1, 2, 4, 10, 20, 40), m = 1
  )
  census2 <- sampling_design() |>
    draw(frac = 1, method = "pps_sampford", mos = m)
  brewer <- sampling_design() |> cluster_by(psu) |>
    draw(n = 2, method = "pps_brewer", mos = mos, certainty_size = 50) |>
    add_stage() |> draw(frac = 1, on_empty = "silent")
  for (digest in c("summary", "full", "none")) {
    for (seed in c(1, 4)) {
      phase1 <- execute(brewer, psus, households, seed = seed,
                        frame_digest = digest)
      phase2 <- execute(census2, phase1, seed = 1)
      info <- paste(digest, seed)
      expect_error(as_svydesign(phase2),
        class = "samplyr_error_twophase_phase1_pps", info = info)
      expect_identical(variance_estimators(phase2)$class[1],
                       "samplyr_error_twophase_phase1_pps", info = info)
    }
  }
  phase1 <- execute(brewer, psus, households, seed = 1)
  expect_identical(unique(phase1$psu), 1L)
  expect_true(all(phase1$.weight == 1))
  # Drawn with replacement: the same seeds leave one cluster's rows.
  for (method in c("pps_multinomial", "pps_chromy", "pps_systematic_mr")) {
    design1 <- sampling_design() |> cluster_by(psu) |>
      draw(n = 2, method = method, mos = mos, certainty_size = 50) |>
      add_stage() |> draw(frac = 1, on_empty = "silent")
    for (seed in c(1, 4)) {
      phase1 <- execute(design1, psus, households, seed = seed)
      phase2 <- execute(census2, phase1, seed = 1)
      # Their correction states no probability, so "full" is refused.
      expect_error(
        suppressWarnings(as_svydesign(phase2,
          systematic_variance = "approximate")),
        class = "samplyr_error_twophase_stage_probs",
        info = paste(method, seed)
      )
      expect_identical(variance_estimators(phase2)$class[1],
                       "samplyr_error_twophase_stage_probs",
                       info = paste(method, seed))
    }
  }
  # No empty unit: the same refusal, and "approx" uses the weights.
  all_households <- data.frame(psu = rep(1:3, each = 3), hh = rep(1:3, 3),
                               y = 1:9, m = 1)
  design1 <- sampling_design() |> cluster_by(psu) |>
    draw(n = 2, method = "pps_multinomial", mos = mos, certainty_size = 50) |>
    add_stage() |> draw(frac = 1)
  phase1 <- execute(design1, psus, all_households, seed = 1)
  phase2 <- execute(sampling_design() |> draw(n = 4), phase1, seed = 1)
  expect_error(as_svydesign(phase2),
    class = "samplyr_error_twophase_stage_probs")
  expect_s3_class(suppressWarnings(as_svydesign(phase2, method = "approx")),
                  "twophase")
  # A cluster drawn at random inside a certain primary unit, then emptied.
  ssus <- data.frame(psu = 1L, ssu = 1:3)
  inner <- data.frame(psu = 1L, ssu = rep(c(1L, 3L), each = 2), hh = 1:4,
                      y = c(1, 2, 10, 20), m = 1)
  nested <- sampling_design() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> cluster_by(ssu) |> draw(n = 2) |>
    add_stage() |> draw(frac = 1, on_empty = "silent")
  phase1 <- execute(nested, data.frame(psu = 1L), ssus, inner, seed = 2)
  expect_identical(unique(phase1$ssu), 3L)
  phase2 <- execute(census2, phase1, seed = 1)
  expect_false(inherits(
    tryCatch(suppressWarnings(as_svydesign(phase2)), error = identity),
    "survey.design2"
  ))

  # Units taken for sure that are empty keep phase 1 fixed.
  both <- sampling_design() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> draw(frac = 1, on_empty = "silent")
  for (digest in c("summary", "none")) {
    phase1 <- execute(both, data.frame(psu = 1:2),
                      households[households$psu == 1L, ], seed = 1,
                      frame_digest = digest)
    expect_census(as_svydesign(execute(census2, phase1, seed = 1)), 7,
      info = digest)
  }
  every <- sampling_design() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> cluster_by(ssu) |> draw(n = 3) |>
    add_stage() |> draw(frac = 1, on_empty = "silent")
  phase1 <- execute(every, data.frame(psu = 1L), ssus, inner, seed = 1)
  expect_census(as_svydesign(execute(census2, phase1, seed = 1)), 33)
})

test_that("a stage drawn with replacement that took only certain units states its probabilities", {
  skip_if_not_installed("survey")
  # Rung 1, closed form. Both clusters are certain, so the stage is a
  # census, and phase 2 draws by equal probabilities (constant size): two of
  # four of six households is two of six, in each cluster.
  frame <- data.frame(
    psu = rep(1:2, each = 6), hh = rep(1:6, 2), m = 1,
    y = c(1, 2, 4, 8, 16, 32, 3, 6, 9, 12, 18, 36)
  )
  design2 <- sampling_design() |> stratify_by(psu) |>
    draw(n = 2, method = "pps_sampford", mos = m)
  stratified_srs <- function(y, stratum, N, n) {
    sum(vapply(split(y, stratum), function(v) {
      N^2 * (1 - n / N) * stats::var(v) / n
    }, 0))
  }
  for (method in c("pps_multinomial", "pps_chromy", "pps_systematic_mr")) {
    phase1 <- sampling_design() |> cluster_by(psu) |>
      draw(n = 2, method = method, mos = m, certainty_size = 1) |>
      add_stage() |> draw(n = 4) |> execute(frame, seed = 1)
    expect_true(all(phase1$.certainty_1))
    phase2 <- execute(design2, phase1, seed = 1)
    design <- suppressWarnings(as_svydesign(phase2,
      systematic_variance = "approximate"))
    est <- survey::svytotal(~y, design)
    expect_equal(unname(coef(est)), 3 * sum(phase2$y), info = method)
    expect_equal(as.vector(vcov(est)),
                 stratified_srs(phase2$y, phase2$psu, 6, 2), info = method)
    expect_true(all(design$phase1$full$allprob[, 1] == 1), info = method)
    linear <- variance_estimators(phase2)
    linear <- linear[linear$estimator == "Taylor linearization", ]
    expect_false(identical(linear$status, "refused"), info = method)
  }
  # Its systematic policy still applies.
  expect_warning(as_svydesign(phase2),
    class = "samplyr_warning_systematic_variance")
  expect_error(as_svydesign(phase2, systematic_variance = "error"),
    class = "samplyr_error_systematic_variance")
  expect_match(linear$class, "samplyr_warning_systematic_variance")

  # No certainty unit: the stage is random on the count scale.
  phase1 <- sampling_design() |> cluster_by(psu) |>
    draw(n = 2, method = "pps_multinomial", mos = m) |>
    add_stage() |> draw(n = 4) |> execute(frame, seed = 1)
  phase2 <- execute(design2, phase1, seed = 1)
  expect_error(as_svydesign(phase2),
    class = "samplyr_error_twophase_stage_probs")
  expect_identical(variance_estimators(phase2)$class[1],
                   "samplyr_error_twophase_stage_probs")

  # Inside each certain region, area 1 is certain and the other draw takes
  # area 2, which is empty, or area 3. Where both regions draw area 2, every
  # row left is certain, but the stage was random.
  regions <- data.frame(reg = 1:2)
  areas <- data.frame(reg = rep(1:2, each = 3), ea = rep(1:3, 2),
                      mos = c(100, 1, 1))
  households <- data.frame(
    reg = rep(1:2, each = 6), ea = rep(rep(c(1L, 3L), each = 3), 2),
    hh = rep(1:3, 4), y = c(1, 2, 4, 10, 20, 40, 3, 5, 7, 30, 50, 70), m = 1
  )
  design2 <- sampling_design() |> stratify_by(reg) |>
    draw(n = 2, method = "pps_sampford", mos = m)
  reached <- character(0)
  for (method in c("pps_multinomial", "pps_chromy", "pps_systematic_mr")) {
    design1 <- sampling_design() |> cluster_by(reg) |> draw(frac = 1) |>
      add_stage() |> cluster_by(ea) |>
      draw(n = 2, method = method, mos = mos, certainty_size = 50) |>
      add_stage() |> draw(frac = 1, on_empty = "silent")
    for (seed in 1:10) {
      phase1 <- execute(design1, regions, areas, households, seed = seed)
      if (!all(phase1$.certainty_2)) next
      reached <- c(reached, method)
      phase2 <- execute(design2, phase1, seed = 1)
      expect_error(
        suppressWarnings(as_svydesign(phase2,
          systematic_variance = "approximate")),
        class = "samplyr_error_twophase_stage_probs",
        info = paste(method, seed)
      )
      expect_identical(variance_estimators(phase2)$class[1],
                       "samplyr_error_twophase_stage_probs",
                       info = paste(method, seed))
    }
  }
  expect_setequal(reached,
                  c("pps_multinomial", "pps_chromy", "pps_systematic_mr"))
  # Every area certain, area 2 empty: phase 1 took every household.
  for (method in c("pps_multinomial", "pps_chromy", "pps_systematic_mr")) {
    design1 <- sampling_design() |> cluster_by(reg) |> draw(frac = 1) |>
      add_stage() |> cluster_by(ea) |>
      draw(n = 3, method = method, mos = mos, certainty_size = 0.5) |>
      add_stage() |> draw(frac = 1, on_empty = "silent")
    phase1 <- execute(design1, regions, areas, households, seed = 1)
    expect_identical(nrow(phase1), nrow(households))
    phase2 <- execute(design2, phase1, seed = 1)
    est <- survey::svytotal(~y, suppressWarnings(as_svydesign(phase2,
      systematic_variance = "approximate")))
    expect_equal(unname(coef(est)), 3 * sum(phase2$y), info = method)
    expect_equal(as.vector(vcov(est)),
                 stratified_srs(phase2$y, phase2$reg, 6, 2), info = method)
  }
})

test_that("a zero row survey's two-phase design cannot place is refused by class", {
  skip_if_not_installed("survey")
  # Cluster 2 has no households, so phase 1 holds it as a zero row.
  psus <- data.frame(psu = 1:3)
  households <- data.frame(
    psu = rep(c(1L, 3L), each = 3), hh = rep(1:3, 2),
    y = c(1, 2, 4, 10, 20, 40), m = 1, st = rep(c("a", "b"), 3)
  )
  census1 <- sampling_design() |> cluster_by(psu) |> draw(frac = 1) |>
    add_stage() |> draw(frac = 1, on_empty = "silent")
  phase1 <- execute(census1, psus, households, seed = 1)
  class <- "samplyr_error_twophase_empty_unit"
  expect_refused <- function(design2, phase1, methods, info) {
    # A stratum of one cluster's households can be smaller than the take.
    phase2 <- suppressWarnings(execute(design2, phase1, seed = 1))
    for (method in methods) {
      expect_error(suppressWarnings(as_svydesign(phase2,
        method = if (method != "full") method)),
        class = class, info = paste(info, method))
    }
    expect_identical(variance_estimators(phase2)$class[1], class, info = info)
    risk <- variance_estimators(design2, phase1)$sample_risk[1]
    expect_match(risk, class, fixed = TRUE, info = info)
  }
  # A stratum on a household variable: the zero row has none.
  expect_refused(sampling_design() |> stratify_by(st) |> draw(n = 2),
                 phase1, c("full", "approx", "simple"), "household strata")
  # Every unit phase 2 can draw, beside joint probabilities.
  expect_refused(sampling_design() |>
                   draw(frac = 1, method = "pps_sampford", mos = m),
                 phase1, "full", "census")
  expect_refused(sampling_design() |> stratify_by(psu) |>
                   draw(frac = 1, method = "pps_sampford", mos = m),
                 phase1, "full", "census within clusters")
  # Two clusters drawn, one of them empty: phase 2 lies in the other.
  srs1 <- sampling_design() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> draw(frac = 1, on_empty = "silent")
  reached <- 0L
  for (seed in 1:4) {
    drawn <- execute(srs1, psus, households, seed = seed)
    if (length(attr(drawn, "metadata")$empty_parents) == 0L) next
    reached <- reached + 1L
    expect_refused(sampling_design() |> stratify_by(st) |> draw(n = 2),
                   drawn, c("full", "approx", "simple"), paste("one", seed))
    expect_refused(sampling_design() |>
                     draw(n = 2, method = "pps_sampford", mos = m),
                   drawn, "full", paste("one pps", seed))
  }
  expect_gt(reached, 0L)

  # Rung 1, closed form: phase 1 took every household, and two of three in
  # each cluster by equal probabilities is SRS. The zero row stays.
  phase2 <- execute(sampling_design() |> stratify_by(psu) |>
    draw(n = 2, method = "pps_sampford", mos = m), phase1, seed = 1)
  est <- survey::svytotal(~y, as_svydesign(phase2), na.rm = TRUE)
  expect_equal(unname(coef(est)), 1.5 * sum(phase2$y))
  expect_equal(as.vector(vcov(est)), sum(vapply(
    split(phase2$y, phase2$psu), function(y) 9 * (1 - 2 / 3) * var(y) / 2, 0
  )))
  expect_false(identical(variance_estimators(phase2)$status[1], "refused"))
  expect_census(suppressWarnings(as_svydesign(execute(
    sampling_design() |> draw(frac = 1), phase1, seed = 1
  ))), 77)
  # A census of one household per stratum restates its correction and
  # drops the strata, so the zero row's stratum never reaches survey, as
  # the preflight reads it too. Beside joint probabilities it stays refused.
  one_each <- execute(sampling_design() |> stratify_by(psu, hh) |>
    draw(frac = 1), phase1, seed = 1)
  expect_census(suppressWarnings(as_svydesign(one_each)), 77)
  expect_false(identical(variance_estimators(one_each)$status[1], "refused"))
  expect_refused(sampling_design() |> stratify_by(psu, hh) |>
                   draw(frac = 1, method = "pps_sampford", mos = m),
                 phase1, "full", "census of one per stratum")
})

test_that("phase 2 inside clusters phase 1 drew with replacement is refused", {
  skip_if_not_installed("survey")
  # Both draws hit cluster 1, each hit is a unit of its own with weight one.
  frame <- data.frame(psu = rep(1:2, each = 3), hh = rep(1:3, 2),
                      y = c(1, 2, 10, 3, 4, 20))
  phase1 <- sampling_design() |> cluster_by(psu) |>
    draw(n = 2, method = "srswr") |> execute(frame, seed = 2)
  expect_identical(unique(phase1$psu), 1L)
  expect_identical(sort(unique(phase1$.draw_1)), 1:2)
  for (design2 in list(
    sampling_design() |> stratify_by(.phase1_draw_1) |> draw(n = 2),
    sampling_design() |> draw(n = 3)
  )) {
    phase2 <- execute(design2, phase1, seed = 1)
    for (method in list(NULL, "full")) {
      expect_error(as_svydesign(phase2, method = method),
        class = "samplyr_error_twophase_wr_subsample")
    }
    expect_identical(variance_estimators(phase2)$class[1],
                     "samplyr_error_twophase_wr_subsample")
    raised <- character(0)
    withCallingHandlers(
      as_svydesign(phase2, method = "approx"),
      warning = function(w) {
        raised <<- c(raised, class(w)[1])
        invokeRestart("muffleWarning")
      }
    )
    expect_true("samplyr_warning_twophase_approx" %in% raised)
  }

  # Clusters a certainty rule took once add no with-replacement variance.
  frame$mos <- 1
  certain <- sampling_design() |> cluster_by(psu) |>
    draw(n = 2, method = "pps_multinomial", mos = mos, certainty_size = 1) |>
    execute(frame, seed = 1)
  expect_true(all(certain$.certainty_1))
  phase2 <- sampling_design() |> stratify_by(psu) |> draw(n = 2) |>
    execute(certain, seed = 1)
  # Oracle: stratified SRS, two of each cluster's three households.
  est <- survey::svytotal(~y, as_svydesign(phase2))
  expect_equal(unname(coef(est)), 3 / 2 * sum(phase2$y))
  expect_equal(as.vector(survey::SE(est))^2,
    sum(vapply(split(phase2$y, phase2$psu), function(y) {
      3^2 * (1 - 2 / 3) * stats::var(y) / 2
    }, 0)))
  expect_identical(variance_estimators(phase2)$status[1], "supported")

  # One certain cluster and random hits among the others: refused where it
  # subsamples a random hit. Clusters reuse their ids in each region.
  mixed <- data.frame(
    region = rep(1:2, each = 12), psu = rep(rep(1:4, each = 3), 2),
    hh = rep(1:3, 8), mos = rep(c(100, 1, 1, 1), each = 3, times = 2),
    y = seq_len(24)
  )
  phase1 <- sampling_design() |> stratify_by(region) |> cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = mos,
         certainty_size = 50) |>
    execute(mixed, seed = 3)
  expect_true(any(phase1$.certainty_1) && !all(phase1$.certainty_1))
  phase2 <- sampling_design() |>
    stratify_by(region, psu, .phase1_draw_1) |> draw(n = 2) |>
    execute(phase1, seed = 1)
  expect_error(as_svydesign(phase2),
    class = "samplyr_error_twophase_wr_subsample")

  # A stage drawn with replacement below another also lacks the stage
  # probabilities. Both routes report the same refusal first.
  nested <- expand.grid(e = 1:3, ssu = 1:2, psu = 1:4)
  nested$y <- seq_len(nrow(nested))
  phase1 <- sampling_design() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> cluster_by(ssu) |> draw(n = 2, method = "srswr") |>
    execute(nested, seed = 1)
  phase2 <- sampling_design() |> stratify_by(ssu, .phase1_draw_2) |>
    draw(n = 2) |> execute(phase1, seed = 1)
  expect_error(as_svydesign(phase2),
    class = "samplyr_error_twophase_wr_subsample")
  expect_identical(variance_estimators(phase2)$class[1],
                   "samplyr_error_twophase_wr_subsample")

  # The advised design: households as a stage of phase 1, phase 2 within
  # each hit. Oracle: the Hansen-Hurwitz variance of the two hits' totals.
  phase1 <- sampling_design() |> cluster_by(psu) |>
    draw(n = 2, method = "srswr") |> add_stage() |> draw(frac = 1) |>
    execute(frame, seed = 2)
  phase2 <- sampling_design() |> stratify_by(psu, .phase1_draw_1) |>
    draw(n = 2) |> execute(phase1, seed = 1)
  est <- survey::svytotal(~y, as_svydesign(phase2))
  totals <- tapply(phase2$y * 3 / 2, phase2$.phase1_draw_1, sum)
  expect_equal(unname(coef(est)), mean(2 * totals))
  expect_equal(as.vector(survey::SE(est))^2, 2 * stats::var(totals))
})

## Replicate types that drop an equal-probability first-stage correction

test_that("BRR, Fay and the rescaled bootstrap disclose a dropped correction", {
  skip_if_not_installed("survey")
  warned <- function(sample, type, ...) {
    classes <- character(0)
    rep <- withCallingHandlers(
      as_svrepdesign(sample, type = type, ...),
      warning = function(w) {
        classes <<- c(classes, if (any(grepl("^samplyr_", class(w)))) {
          grep("^samplyr_", class(w), value = TRUE)
        } else {
          conditionMessage(w)
        })
        invokeRestart("muffleWarning")
      }
    )
    list(rep = rep, classes = classes)
  }

  # 60 of 80 units: dropping 1 - 60/80 multiplies the variance by four.
  frame <- data.frame(id = 1:80, y = 1:80)
  design <- sampling_design() |> draw(n = 60)
  sample <- execute(design, frame, seed = 1)
  report <- variance_estimators(design, frame)
  row <- report$estimator == "Rescaled bootstrap"
  expect_identical(report$status[row], "approximate")
  expect_identical(report$class[row],
                   "samplyr_warning_replicate_wr_first_stage")
  set.seed(5)
  sub <- warned(sample, "subbootstrap", replicates = 4000)
  expect_identical(sub$classes, "samplyr_warning_replicate_wr_first_stage")
  ratio <- as.numeric(vcov(survey::svytotal(~y, sub$rep))) /
    as.numeric(vcov(survey::svytotal(~y, as_svydesign(sample))))
  expect_gt(ratio, 3.5)
  expect_lt(ratio, 4.5)
  for (type in c("bootstrap", "JK1")) {
    expect_identical(warned(sample, type, replicates = 50)$classes,
                     character(0))
  }

  # Two per stratum: survey's own warning is muted behind the classed one.
  frame <- data.frame(st = rep(1:10, each = 3), id = 1:30, y = 1:30)
  design <- sampling_design() |> stratify_by(st) |> draw(n = 2)
  sample <- execute(design, frame, seed = 1)
  report <- variance_estimators(design, frame)
  for (estimator in c("Balanced repeated replication",
                      "Fay's balanced repeated replication")) {
    expect_identical(report$status[report$estimator == estimator],
                     "approximate")
  }
  for (type in c("BRR", "Fay")) {
    expect_identical(warned(sample, type)$classes,
                     "samplyr_warning_replicate_wr_first_stage")
  }
  expect_identical(warned(sample, "JKn")$classes, character(0))
})

test_that("every replicate design can be calibrated", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # survey's calibration fails on replicate weights held as a plain matrix.
  calibrates <- function(rep, label) {
    total <- sum(weights(rep, type = "sampling"))
    calibrated <- survey::calibrate(
      rep, ~g, c(`(Intercept)` = total, gv = total / 2)
    )
    expect_s3_class(rep$repweights, "repweights_compressed")
    expect_equal(
      unname(coef(survey::svytotal(~g, calibrated))),
      c(total / 2, total / 2),
      info = label
    )
  }
  frame <- data.frame(
    id = 1:200, st = rep(c("a", "b"), each = 100), psu = rep(1:40, each = 5),
    y = (1:200) %% 13, g = rep(c("u", "v"), 100)
  )
  sample <- sampling_design() |>
    stratify_by(st) |> cluster_by(psu) |> draw(n = 6) |>
    add_stage() |> draw(n = 2) |>
    execute(frame, seed = 1)
  calibrates(
    suppressWarnings(as_svrepdesign(sample, type = "mrbbootstrap")),
    "mrbbootstrap"
  )
  groups <- sampling_design() |>
    stratify_by(st) |> draw(n = 10) |>
    execute(frame, seed = 1, reps = 10)
  calibrates(as_svrepdesign(groups, type = "random_groups"), "random groups")

  population <- data.frame(
    id = 1:120, y = (1:120) %% 7, g = rep(c("u", "v"), 60),
    in_a = rep(c(TRUE, FALSE), c(80, 40)),
    in_b = rep(c(FALSE, TRUE), c(40, 80))
  )
  stack <- stack_frames(
    a = sampling_design() |> draw(n = 20) |>
      execute(population[population$in_a, ], seed = 1),
    b = sampling_design() |> draw(n = 20) |>
      execute(population[population$in_b, ], seed = 2),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )
  calibrates(
    suppressWarnings(as_svrepdesign(stack, type = "bootstrap",
                                    replicates = 50)),
    "frame stack"
  )

  dwellings <- data.frame(dw_id = 1:45, region = rep(c("n", "s"), c(25, 20)))
  targets <- data.frame(person_id = 1:90, hh = rep(1:45, each = 2),
                        g = rep(c("u", "v"), 45))
  shared <- share_weights(
    sampling_design() |> stratify_by(region) |> draw(n = 10) |>
      execute(dwellings, seed = 2),
    targets = targets,
    links = data.frame(dw_id = rep(1:45, each = 2), person_id = 1:90),
    by = c(dw_id = "dw_id"),
    to = c(person_id = "person_id"),
    within = hh,
    multiplicity = complete_links()
  )
  calibrates(
    suppressWarnings(as_svrepdesign(shared, type = "JKn")),
    "shared weights"
  )
})

test_that("two-phase PPS export handles one-unit phase-2 strata via survey.lonely.psu", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = 1:24,
    str = rep(c("A", "B"), c(8, 16)),
    mos = c(10, 12, 14, 16, 18, 20, 22, 24, seq(10, 40, length.out = 16)),
    y = c(12, 15, 19, 21, 26, 29, 31, 35, seq(15, 60, length.out = 16))
  )
  p1 <- sampling_design() |>
    stratify_by(str) |>
    cluster_by(id) |>
    draw(n = c(A = 4, B = 8)) |>
    execute(frame, seed = 101)
  p2 <- suppressMessages(
    sampling_design() |>
      stratify_by(str) |>
      cluster_by(id) |>
      draw(n = c(A = 1, B = 2), method = "pps_sampford", mos = mos) |>
      execute(p1, seed = 202)
  )

  withr::with_options(list(survey.lonely.psu = "fail"), {
    expect_error(as_svydesign(p2), class = "samplyr_error_lonely_psu")
  })
  withr::with_options(list(survey.lonely.psu = "average"), {
    expect_error(as_svydesign(p2), class = "samplyr_error_lonely_psu")
  })

  d_cert <- withr::with_options(
    list(survey.lonely.psu = "certainty"),
    as_svydesign(p2)
  )
  d_rem <- withr::with_options(
    list(survey.lonely.psu = "remove"),
    as_svydesign(p2)
  )
  d_adj <- withr::with_options(
    list(survey.lonely.psu = "adjust"),
    as_svydesign(p2)
  )

  tot_cert <- survey::svytotal(~y, d_cert)
  tot_rem <- survey::svytotal(~y, d_rem)
  tot_adj <- survey::svytotal(~y, d_adj)
  expect_equal(as.numeric(vcov(tot_cert)), as.numeric(vcov(tot_rem)))
  expect_equal(
    as.numeric(attr(vcov(tot_adj), "phases")$phase1),
    as.numeric(attr(vcov(tot_cert), "phases")$phase1)
  )

  sub_cert_a <- survey::svytotal(~y, suppressWarnings(subset(d_cert, str == "A")))
  sub_adj_a <- survey::svytotal(~y, suppressWarnings(subset(d_adj, str == "A")))
  expect_equal(as.numeric(attr(vcov(sub_cert_a), "phases")$phase2), 0)
  expect_gt(as.numeric(attr(vcov(sub_adj_a), "phases")$phase2), 0)
  expect_gt(as.numeric(survey::SE(sub_adj_a)), as.numeric(survey::SE(sub_cert_a)))

  # A true 1-unit census stratum at phase 2 (pi_{2|1} == 1) is exempt.
  frame_cen <- data.frame(
    id = 1:14,
    str = c("A", rep("B", 13)),
    mos = seq(15, 28, length.out = 14),
    y = seq(30, 56, length.out = 14)
  )
  p1_cen <- sampling_design() |>
    stratify_by(str) |>
    cluster_by(id) |>
    draw(n = c(A = 1, B = 8)) |>
    execute(frame_cen, seed = 11)
  p2_cen <- sampling_design() |>
    stratify_by(str) |>
    cluster_by(id) |>
    draw(n = c(A = 1, B = 4), method = "pps_sampford", mos = mos) |>
    execute(p1_cen, seed = 22)
  withr::with_options(list(survey.lonely.psu = "fail"), {
    d_cen <- expect_no_error(as_svydesign(p2_cen))
    expect_gt(as.numeric(survey::SE(survey::svytotal(~y, d_cen))), 0)
  })
})

test_that("with-replacement 1-unit stratum warns as lonely even when expected hits equal 1", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = 1:11,
    str = c("solo", rep("s2", 5), rep("s3", 5)),
    mos = c(10, 1:5, 2:6),
    y = (1:11) * 3
  )
  s_wr <- suppressMessages(
    sampling_design() |>
      stratify_by(str) |>
      draw(n = c(solo = 1, s2 = 3, s3 = 3), method = "srswr") |>
      execute(frame, seed = 7)
  )
  s_mult <- suppressMessages(
    sampling_design() |>
      stratify_by(str) |>
      draw(n = c(solo = 1, s2 = 3, s3 = 3), method = "pps_multinomial", mos = mos) |>
      execute(frame, seed = 8)
  )

  withr::with_options(list(survey.lonely.psu = "fail"), {
    expect_warning(
      d_wr <- as_svydesign(s_wr),
      class = "samplyr_warning_lonely_psu"
    )
    expect_error(survey::svytotal(~y, d_wr))
    expect_warning(
      d_mult <- as_svydesign(s_mult),
      class = "samplyr_warning_lonely_psu"
    )
    expect_error(survey::svytotal(~y, d_mult))
  })
})

test_that("ppsmat on fixed-size PPS WOR defaults to YG and respects explicit variance", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = seq_len(20),
    mos = seq(10, 200, by = 10)
  )
  s <- sampling_design() |>
    draw(n = 5, method = "pps_sampford", mos = mos) |>
    execute(frame, seed = 2025)
  jip <- joint_expectation(s, frame)[[1L]]
  s$y_prop <- 50 / s$.weight

  svy_def <- as_svydesign(s, pps = survey::ppsmat(jip))
  expect_identical(svy_def$variance, "YG")
  # With y proportional to pi the YG form is 0 up to rounding, which can
  # fall on either side of 0, so the variance is checked and not its root.
  var_total <- function(svy) {
    as.numeric(stats::vcov(survey::svytotal(~y_prop, svy)))
  }
  expect_lt(abs(var_total(svy_def)), 1e-10 * 250^2)

  svy_ht <- as_svydesign(s, pps = survey::ppsmat(jip), variance = "HT")
  expect_identical(svy_ht$variance, "HT")
  expect_gt(var_total(svy_ht), 100)

  # Random-size Poisson PPS keeps HT because sample size is not fixed.
  s_pois <- sampling_design() |>
    draw(n = 6, method = "pps_poisson", mos = mos) |>
    execute(frame, seed = 2025)
  jip_pois <- joint_expectation(s_pois, frame)[[1L]]
  svy_pois <- as_svydesign(s_pois, pps = survey::ppsmat(jip_pois))
  expect_identical(svy_pois$variance, "HT")

  # variance_estimators() prints variance = "YG" on fixed-size WOR joint row.
  ve <- variance_estimators(sampling_design() |> draw(n = 5, method = "pps_sampford", mos = mos))
  expect_match(ve$call[ve$estimator == "Linearization, joint probabilities"], 'variance = "YG"', fixed = TRUE)
})

test_that("single-stage pps export skips systematic check on dropped later stages", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    psu = rep(paste0("p", 1:10), each = 4),
    ssu = seq_len(40),
    mos = rep(seq(10, 100, by = 10), each = 4),
    y = seq_len(40) * 3
  )
  two_stage <- function(n2) {
    suppressMessages(
      sampling_design() |>
        add_stage() |>
          cluster_by(psu) |>
          draw(n = 4, method = "pps_sampford", mos = mos) |>
        add_stage() |>
          draw(n = n2, method = "systematic") |>
        execute(frame, seed = 12)
    )
  }
  # A pps matrix needs one row per PSU, so stage 2 takes one unit per PSU,
  # which is also a draw that does not depend on the frame order.
  s <- two_stage(1)
  jip <- joint_expectation(s, frame, stages = 1)[[1L]]

  classes <- character()
  svy <- withCallingHandlers(
    as_svydesign(s, pps = survey::ppsmat(jip), systematic_variance = "error"),
    warning = function(w) {
      classes <<- c(classes, class(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(svy, "survey.design")
  expect_true("samplyr_warning_pps_single_stage" %in% classes)
  expect_false("samplyr_warning_systematic_variance" %in% classes)

  # Keeping all stages with pps = "brewer" still checks a systematic stage 2
  # that draws two units per PSU.
  s <- two_stage(2)
  brewer_classes <- character()
  withCallingHandlers(
    as_svydesign(s, pps = "brewer"),
    warning = function(w) {
      brewer_classes <<- c(brewer_classes, class(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true("samplyr_warning_systematic_variance" %in% brewer_classes)
  expect_error(
    as_svydesign(s, pps = "brewer", systematic_variance = "error"),
    class = "samplyr_error_systematic_variance"
  )

  # A systematic stage 1 still warns under pps = "overton".
  s_sys1 <- sampling_design() |>
    draw(n = 4, method = "pps_systematic", mos = mos) |>
    execute(unique(frame[c("psu", "mos")]), seed = 12)
  expect_warning(
    as_svydesign(s_sys1, pps = "overton"),
    class = "samplyr_warning_systematic_variance"
  )
})

test_that("the lonely-PSU warning fires when survey stops on the stratum", {
  skip_if_not_installed("survey")
  # Below a hit of a stage drawn with replacement, survey's compiled
  # variance reads nothing, since the spread between hits carries the stages
  # below. The warning counted those strata and said survey stops, and it
  # did not. Its R variance computes every stage before scaling, so it stops.
  probe <- function(s, rpath) {
    withr::local_options(survey.use_rcpp = !rpath)
    n_strata <- 0L
    svy <- withCallingHandlers(as_svydesign(s), warning = function(w) {
      if (inherits(w, "samplyr_warning_lonely_psu")) {
        n_strata <<- n_strata + as.integer(w$n_strata)
      }
      invokeRestart("muffleWarning")
    })
    withr::local_options(survey.lonely.psu = "fail")
    stops <- tryCatch({
      survey::svytotal(~y, svy)
      FALSE
    }, error = function(e) TRUE)
    list(n_strata = n_strata, stops = stops, svy = svy)
  }
  units <- function(sizes, m) {
    out <- data.frame(psu = rep(seq_along(sizes), sizes))
    out$m <- m[out$psu]
    out$id <- seq_len(nrow(out))
    out$y <- out$id %% 7 + 1
    out
  }
  m <- c(100, 40, 30, 15, 10, 5)
  wr <- function(certainty_size = NULL) {
    sampling_design() |> add_stage() |> cluster_by(psu) |>
      draw(n = 5, method = "pps_multinomial", mos = m,
           certainty_size = certainty_size) |>
      add_stage() |> draw(n = 1)
  }
  areas <- data.frame(psu = rep(1:6, each = 9), m = rep(m, each = 9))
  areas$ea <- paste(areas$psu, rep(1:3, each = 3))
  areas$id <- seq_len(nrow(areas))
  areas$y <- areas$id %% 5 + 1
  # Strata survey reads on its compiled path: none below an ordinary hit,
  # one below the certain PSU of two units that takes one.
  cases <- list(
    hits = list(wr(), units(rep(3, 6), m), 0L),
    certain_one_unit = list(wr(60), units(c(1, rep(3, 5)), m), 0L),
    mixed = list(wr(60), units(c(2, rep(3, 5)), m), 1L),
    three_stages = list(
      sampling_design() |> add_stage() |> cluster_by(psu) |>
        draw(n = 5, method = "pps_multinomial", mos = m) |>
        add_stage() |> cluster_by(ea) |> draw(n = 2) |>
        add_stage() |> draw(n = 1),
      areas, 0L
    ),
    wr_stage_2 = list(
      sampling_design() |> add_stage() |> cluster_by(psu) |> draw(n = 3) |>
        add_stage() |> cluster_by(ea) |> draw(n = 2, method = "srswr") |>
        add_stage() |> draw(n = 1),
      areas, 0L
    ),
    without_replacement = list(
      sampling_design() |> add_stage() |> cluster_by(psu) |> draw(n = 3) |>
        add_stage() |> draw(n = 1),
      units(rep(3, 6), m), 3L
    )
  )
  checked <- character(0)
  for (case in names(cases)) {
    for (seed in 1:2) {
      s <- suppressWarnings(execute(cases[[case]][[1]], cases[[case]][[2]],
                                    seed = seed))
      for (rpath in c(FALSE, TRUE)) {
        info <- paste(case, seed, if (rpath) "R" else "compiled")
        got <- probe(s, rpath)
        expect_identical(got$n_strata > 0L, got$stops, info = info)
        if (!rpath) {
          expect_identical(got$n_strata, cases[[case]][[3]], info = info)
        }
        checked <- c(checked, info)
      }
    }
  }
  expect_length(unique(checked), 24L)
  # Read as ultimate clusters, survey reads no stage below the first.
  s <- suppressWarnings(execute(cases$without_replacement[[1]],
                                cases$without_replacement[[2]], seed = 1))
  local({
    withr::local_options(survey.ultimate.cluster = TRUE)
    got <- probe(s, rpath = FALSE)
    expect_identical(got$n_strata, 0L)
    expect_false(got$stops)
  })
  # Exempt and exact: the compiled SE is the Hansen-Hurwitz one, computed by
  # hand from the hits, and a PSU is drawn twice.
  s <- execute(wr(), units(rep(3, 6), m), seed = 1)
  expect_true(any(tapply(s$.draw_1, s$psu, function(d) length(unique(d))) > 1))
  z <- tapply(s$y * s$.weight, s$.draw_1, sum)
  k <- length(z)
  got <- probe(s, rpath = FALSE)
  expect_identical(got$n_strata, 0L)
  withr::local_options(survey.lonely.psu = "fail")
  expect_equal(as.numeric(survey::SE(survey::svytotal(~y, got$svy))),
               sqrt(k / (k - 1) * sum((z - mean(z))^2)))
})

test_that("the default replicate type exports a stage drawn with replacement", {
  skip_if_not_installed("survey")
  # A stage drawn with replacement carries an infinite correction, which
  # survey's jackknife cannot read. JK1, JKn and the bootstrap left it out,
  # but "auto", survey's own choice between them, kept it and failed.
  frame <- data.frame(id = 1:30, m = rep(1:5, 6) + 2, y = (1:30) %% 7 + 1)
  designs <- list(
    srswr = sampling_design() |> draw(n = 8, method = "srswr"),
    pps_multinomial = sampling_design() |>
      draw(n = 8, method = "pps_multinomial", mos = m),
    pps_chromy = sampling_design() |>
      draw(n = 8, method = "pps_chromy", mos = m)
  )
  checked <- character(0)
  for (method in names(designs)) {
    s <- suppressWarnings(execute(designs[[method]], frame, seed = 1))
    replicate <- suppressWarnings(as_svrepdesign(s))
    expect_identical(replicate$type, "JK1", info = method)
    se <- unname(survey::SE(survey::svytotal(~y, replicate)))
    # Closed form: the with-replacement variance of the weighted hits.
    z <- s$.weight * s$y
    n <- length(z)
    expect_equal(se, sqrt(n / (n - 1) * sum((z - mean(z))^2)), info = method)
    expect_equal(
      se,
      unname(survey::SE(survey::svytotal(
        ~y, suppressWarnings(as_svrepdesign(s, type = "JK1"))
      ))),
      info = method
    )
    checked <- c(checked, method)
  }
  expect_setequal(checked, names(designs))
})

test_that("a phase-1 stratum of one drawn unit is refused by the full export", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:20, g = rep(1:2, each = 10), y = (1:20)^2)
  for (method in c("srswr", "srswor")) {
    phase1 <- sampling_design() |> stratify_by(g) |>
      draw(n = c(`1` = 1, `2` = 4), method = method) |>
      execute(frame, seed = 2)
    phase2 <- sampling_design() |> draw(frac = 1) |> execute(phase1, seed = 3)
    expect_error(as_svydesign(phase2), class = "samplyr_error_lonely_psu",
                 info = method)
    row <- variance_estimators(phase2)[1L, ]
    expect_identical(row$status, "refused", info = method)
    expect_identical(row$class, "samplyr_error_lonely_psu", info = method)
    # The approximate export follows survey.lonely.psu, so it warns.
    expect_warning(as_svydesign(phase2, method = "approx"),
                   class = "samplyr_warning_lonely_psu")
    # With the lone stratum removed, phase 2 takes all of phase 1, and the
    # variance is stratum 2's alone.
    z <- with(as.data.frame(phase1), (.weight * y)[g == 2])
    f <- if (method == "srswr") 0 else 4 / 10
    withr::with_options(list(survey.lonely.psu = "remove"), {
      svy <- expect_no_warning(as_svydesign(phase2, method = "approx"))
      expect_equal(as.numeric(vcov(survey::svytotal(~y, svy))),
                   (1 - f) * length(z) * var(z), info = method)
    })
  }
})
