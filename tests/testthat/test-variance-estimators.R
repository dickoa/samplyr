## The report against what the exports raise

ve_frame <- function() {
  data.frame(
    id = sprintf("u%03d", 1:120),
    stratum = rep(c("A", "B", "C", "D"), each = 30),
    cluster = rep(sprintf("cl%02d", 1:24), each = 5),
    mos = rep(c(5, 40, 12, 90, 7, 33, 21, 60, 15, 8, 50, 11, 26, 70, 9, 18,
                44, 13, 6, 38, 29, 17, 55, 10), each = 5),
    x = rep(1:5, 24),
    lon = rep(1:12, 10),
    lat = rep(1:10, each = 12),
    y = (1:120) %% 7
  )
}

# One design per case, named by what it exercises.
ve_designs <- function() {
  list(
    srs = sampling_design() |> draw(n = 20),
    stratified = sampling_design() |> stratify_by(stratum) |> draw(n = 4),
    two_per_stratum = sampling_design() |>
      stratify_by(stratum) |> cluster_by(cluster) |> draw(n = 2),
    two_stage = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(n = 3) |>
      add_stage() |> draw(n = 2),
    one_per_psu = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(n = 3) |>
      add_stage() |> draw(n = 1),
    brewer = sampling_design() |> draw(n = 10, method = "pps_brewer",
                                       mos = mos),
    brewer_two_stage = sampling_design() |>
      add_stage() |> cluster_by(cluster) |>
      draw(n = 6, method = "pps_brewer", mos = mos) |>
      add_stage() |> draw(n = 2),
    sampford_clusters = sampling_design() |> cluster_by(cluster) |>
      draw(n = 4, method = "pps_sampford", mos = mos),
    pps_systematic = sampling_design() |>
      draw(n = 10, method = "pps_systematic", mos = mos),
    systematic = sampling_design() |> draw(n = 12, method = "systematic"),
    systematic_census = sampling_design() |> stratify_by(stratum) |>
      draw(n = 30, method = "systematic"),
    bernoulli = sampling_design() |> draw(frac = 0.2, method = "bernoulli"),
    pps_poisson = sampling_design() |>
      draw(n = 10, method = "pps_poisson", mos = mos),
    poisson_clusters = sampling_design() |> cluster_by(cluster) |>
      draw(frac = 0.3, method = "bernoulli"),
    later_poisson = sampling_design() |>
      add_stage() |> cluster_by(cluster) |> draw(n = 6) |>
      add_stage() |> draw(frac = 0.5, method = "bernoulli",
                          on_empty = "silent"),
    srswr = sampling_design() |> draw(n = 15, method = "srswr"),
    multinomial_two_stage = sampling_design() |>
      add_stage() |> cluster_by(cluster) |>
      draw(n = 5, method = "pps_multinomial", mos = mos) |>
      add_stage() |> draw(n = 2),
    chromy = sampling_design() |> draw(n = 10, method = "pps_chromy",
                                       mos = mos),
    cube = sampling_design() |> draw(n = 12, method = "cube", aux = c(x)),
    lpm2 = sampling_design() |>
      draw(n = 12, method = "lpm2", spread = c(lon, lat)),
    singleton_stratum = sampling_design() |> stratify_by(stratum) |>
      draw(n = c(A = 1, B = 3, C = 3, D = 3)),
    sps = sampling_design() |> draw(n = 10, method = "pps_sps", mos = mos),
    certainty = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 2, method = "pps_brewer", mos = mos, certainty_size = 80)
  )
}

# What an estimator's export raised: the first samplyr error class, and the
# samplyr warning classes.
ve_outcome <- function(key, sample, frame, twophase = FALSE) {
  pps <- if (twophase) {
    "brewer"
  } else if (identical(key, "joint")) {
    joint <- suppressWarnings(joint_expectation(sample, frame))
    survey::ppsmat(joint[[1]])
  }
  warnings <- character(0)
  error <- NA_character_
  withCallingHandlers(
    tryCatch(
      switch(
        key,
        linearization = as_svydesign(sample),
        joint = as_svydesign(sample, pps = pps),
        rwyb = as_svrepdesign(sample, type = "rwyb", replicates = 20),
        bootstrap = ,
        subbootstrap = ,
        mrbbootstrap = as_svrepdesign(sample, type = key, replicates = 20),
        as_svrepdesign(sample, type = key)
      ),
      error = function(e) {
        error <<- grep("^samplyr_error_", class(e), value = TRUE)[1] %|%
          class(e)[1]
      }
    ),
    warning = function(w) {
      warnings <<- c(warnings, grep("^samplyr_warning_", class(w),
                                    value = TRUE)[1])
      invokeRestart("muffleWarning")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  list(error = error, warnings = unique(stats::na.omit(warnings)))
}

`%|%` <- function(x, y) if (is.na(x)) y else x

# Classes a row allows: those it states and those its sample risks name.
ve_allowed <- function(row) {
  text <- paste(row$class, row$sample_risk)
  regmatches(text, gregexpr("samplyr_(error|warning)_[a-z0-9_]+", text))[[1]]
}

# A report row and an export outcome disagree when a refusal was not raised,
# a stated class was not raised, or something else was.
ve_disagreement <- function(row, outcome) {
  allowed <- ve_allowed(row)
  stated <- if (is.na(row$class)) character(0) else
    strsplit(row$class, ", ", fixed = TRUE)[[1]]
  if (identical(row$status, "refused")) {
    # A risk the sample realizes can refuse before the stated refusal.
    risks <- setdiff(allowed, stated)
    if (!identical(outcome$error, stated[1]) && !outcome$error %in% risks) {
      return(paste("refused with", stated[1], "but the export gave",
                   outcome$error))
    }
    return(NULL)
  }
  if (!is.na(outcome$error) && !outcome$error %in% allowed) {
    return(paste("the export refused with", outcome$error))
  }
  if (is.na(outcome$error) && !all(stated %in% outcome$warnings)) {
    return(paste("stated", paste(setdiff(stated, outcome$warnings),
                                 collapse = ", "), "was not raised"))
  }
  unexpected <- setdiff(outcome$warnings, allowed)
  if (length(unexpected) > 0) {
    return(paste("unreported", paste(unexpected, collapse = ", ")))
  }
  NULL
}

ve_check <- function(label, design, frame, seeds = 1:2,
                     report = variance_estimators(design, frame)) {
  keys <- vapply(samplyr:::estimator_catalogue(), `[[`, "", "key")
  problems <- character(0)
  for (seed in seeds) {
    sample <- suppressMessages(suppressWarnings(
      execute(design, frame, seed = seed)
    ))
    for (i in seq_along(keys)) {
      row <- report[i, ]
      if (row$status == "not applicable" || keys[i] == "random_groups") {
        next
      }
      outcome <- ve_outcome(keys[i], sample, frame)
      why <- ve_disagreement(row, outcome)
      if (!is_null(why)) {
        problems <- c(problems, paste0(label, " seed ", seed, " ", keys[i],
                                       ": ", why))
      }
    }
  }
  problems
}

test_that("every single-phase row agrees with what its export raises", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  frame <- ve_frame()
  designs <- ve_designs()
  problems <- unlist(Map(ve_check, names(designs), designs,
                         MoreArgs = list(frame = frame)))
  expect_identical(problems, character(0))
})

test_that("the report made without a frame agrees too", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  frame <- ve_frame()
  # Only the frame shows that a systematic stage takes every unit.
  designs <- ve_designs()
  designs$systematic_census <- NULL
  problems <- unlist(Map(function(label, design) {
    ve_check(label, design, frame, report = variance_estimators(design))
  }, names(designs), designs))
  expect_identical(problems, character(0))
})

test_that("registers with an empty candidate unit report the risk it runs", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  schools <- data.frame(school = 1:6, st = rep(c("a", "b"), each = 3))
  classes <- data.frame(school = rep(1:5, each = 2), class = 1:10)
  design <- sampling_design() |>
    add_stage() |> stratify_by(st) |> cluster_by(school) |> draw(n = 2) |>
    add_stage() |> cluster_by(class) |> draw(n = 1, on_empty = "silent")
  frames <- list(schools, classes)
  report <- variance_estimators(design, frames)
  expect_match(report$sample_risk[1], "1 candidate unit has nothing to sample")
  expect_match(report$sample_risk[1], "samplyr_error_export_empty_psu",
               fixed = TRUE)
  expect_match(report$sample_risk[10], "samplyr_error_rwyb_missing_parents",
               fixed = TRUE)
  expect_identical(ve_check("registers", design, frames, seeds = 1:6),
                   character(0))
})

ve_check_twophase <- function(label, design1, design2, frame, seeds = 1:2) {
  phase1 <- suppressMessages(suppressWarnings(
    execute(design1, frame, seed = 11)
  ))
  report <- variance_estimators(design2, phase1)
  keys <- vapply(samplyr:::estimator_catalogue(), `[[`, "", "key")
  problems <- character(0)
  for (seed in seeds) {
    sample <- suppressMessages(suppressWarnings(
      execute(design2, phase1, seed = seed)
    ))
    for (i in seq_along(keys)) {
      if (keys[i] == "random_groups") {
        next
      }
      outcome <- ve_outcome(keys[i], sample, NULL, twophase = TRUE)
      why <- ve_disagreement(report[i, ], outcome)
      if (!is_null(why)) {
        problems <- c(problems, paste0(label, " seed ", seed, " ", keys[i],
                                       ": ", why))
      }
    }
  }
  problems
}

test_that("every two-phase row agrees with what its export raises", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  frame <- ve_frame()
  clusters <- sampling_design() |> cluster_by(cluster) |> draw(n = 8)
  # Phase 1 takes whole clusters, so phase 2 declares the row-level unit.
  cases <- list(
    within = list(clusters, sampling_design() |> stratify_by(cluster) |>
                    cluster_by(id) |> draw(n = 2)),
    across = list(clusters, sampling_design() |> cluster_by(id) |>
                    draw(n = 10)),
    pps_phase1 = list(sampling_design() |> cluster_by(cluster) |>
                        draw(n = 8, method = "pps_brewer", mos = mos),
                      sampling_design() |> stratify_by(cluster) |>
                        cluster_by(id) |> draw(n = 2)),
    no_bridge = list(sampling_design() |> draw(n = 40),
                     sampling_design() |> draw(n = 10)),
    unit_bridge = list(sampling_design() |> cluster_by(id) |> draw(n = 40),
                       sampling_design() |> draw(n = 10)),
    sampford = list(clusters, sampling_design() |> stratify_by(stratum) |>
                      cluster_by(id) |>
                      draw(n = 3, method = "pps_sampford", mos = x)),
    brewer = list(clusters, sampling_design() |> cluster_by(id) |>
                    draw(n = 6, method = "pps_brewer", mos = x)),
    pps_systematic = list(clusters, sampling_design() |> cluster_by(id) |>
                            draw(n = 6, method = "pps_systematic", mos = x)),
    poisson = list(clusters, sampling_design() |> cluster_by(id) |>
                     draw(frac = 0.3, method = "bernoulli")),
    systematic_phase1 = list(sampling_design() |> cluster_by(cluster) |>
                               draw(n = 8, method = "systematic"),
                             sampling_design() |> stratify_by(cluster) |>
                               cluster_by(id) |> draw(n = 2)),
    two_stage_phase2 = list(clusters, sampling_design() |>
                              add_stage() |> cluster_by(cluster) |>
                              draw(n = 4) |>
                              add_stage() |> cluster_by(id) |> draw(n = 2)),
    wr_phase1 = list(sampling_design() |> cluster_by(cluster) |>
                       draw(n = 8, method = "srswr"),
                     sampling_design() |> cluster_by(id) |> draw(n = 10))
  )
  problems <- unlist(Map(function(label, case) {
    ve_check_twophase(label, case[[1]], case[[2]], frame)
  }, names(cases), cases))
  expect_identical(problems, character(0))
})

test_that("random groups are reported as the replicated execution allows", {
  skip_if_not_installed("survey")
  frame <- ve_frame()
  design <- sampling_design() |> stratify_by(stratum) |>
    draw(n = 3, method = "pps_brewer", mos = mos)
  report <- variance_estimators(design, frame)
  expect_identical(report$status[11], "supported")
  replicated <- execute(design, frame, seed = 1, reps = 3)
  expect_s3_class(as_svrepdesign(replicated, type = "random_groups"),
                  "svyrep.design")

  phase1 <- execute(sampling_design() |> cluster_by(cluster) |> draw(n = 8),
                    frame, seed = 2)
  phase2 <- sampling_design() |> stratify_by(cluster) |> draw(n = 2)
  report <- variance_estimators(phase2, phase1)
  expect_identical(report$class[11], "samplyr_error_random_groups_shared")
  expect_error(
    as_svrepdesign(execute(phase2, phase1, seed = 3, reps = 3),
                   type = "random_groups"),
    class = "samplyr_error_random_groups_shared"
  )
})

## The report itself

test_that("each design of the corpus gets the statuses its methods imply", {
  frame <- ve_frame()
  designs <- ve_designs()
  short <- function(status) {
    c(supported = "S", approximate = "A", refused = "R",
      `not applicable` = "-")[status]
  }
  got <- vapply(designs, function(design) {
    paste(short(variance_estimators(design, frame)$status), collapse = "")
  }, "")
  # Columns: linearization, joint, JK1, JKn, BRR, Fay, bootstrap,
  # subbootstrap, mrbbootstrap, rwyb, random groups.
  expect_identical(got, c(
    srs = "S-SRRRSSSSS",
    stratified = "S-RSSSSSSSS",
    two_per_stratum = "S-RSSSSSSSS",
    two_stage = "S-RSRRSSSSS",
    one_per_psu = "A-RSRRSSSRS",
    brewer = "AARAAAASSAS",
    brewer_two_stage = "ARRAAAASSAS",
    sampford_clusters = "ARRAAAASSAS",
    pps_systematic = "AARAAAAAAAS",
    systematic = "A-ARRRAAAAS",
    systematic_census = "S-RRSSSSSSS",
    bernoulli = "S-RRRRRRRSS",
    pps_poisson = "SSRRRRRRRSS",
    poisson_clusters = "R-RRRRRRRSS",
    later_poisson = "R-RRRRRRRSS",
    srswr = "S-SRRRSSSSS",
    multinomial_two_stage = "S-SRRRSSSSS",
    chromy = "A-ARRRASSRS",
    cube = "AARAAAASSRS",
    lpm2 = "R-RRRRRAARS",
    singleton_stratum = "A-RRRRSRSRS",
    sps = "AARAAAASSRS",
    certainty = "ARRRRRARSRS"
  ))
})

test_that("the report has one row per estimator and fixed columns", {
  report <- variance_estimators(sampling_design() |> draw(n = 5))
  expect_named(report, c("estimator", "call", "status", "decided_by",
                         "phase", "stage", "class", "note", "sample_risk"))
  expect_identical(report$call[c(1, 3, 10)], c(
    "as_svydesign(x)",
    "as_svrepdesign(x, type = \"JK1\")",
    "as_svrepdesign(x, type = \"rwyb\")"
  ))
  expect_type(report$stage, "integer")
  expect_type(report$phase, "integer")
})

test_that("a frame settles what the design alone leaves to the sample", {
  frame <- ve_frame()
  design <- ve_designs()$singleton_stratum
  alone <- variance_estimators(design)
  resolved <- variance_estimators(design, frame)
  expect_identical(alone$status[1], "supported")
  expect_match(alone$sample_risk[1], "samplyr_warning_lonely_psu",
               fixed = TRUE)
  expect_identical(resolved$status[1], "approximate")
  expect_identical(resolved$decided_by[1], "frame")
  expect_identical(resolved$class[1], "samplyr_warning_lonely_psu")
  expect_match(resolved$note[1], "1 stratum at stage 1 takes a single unit",
               fixed = TRUE)
  withr::local_options(survey.lonely.psu = "adjust")
  expect_identical(variance_estimators(design, frame)$status[1], "supported")
})

test_that("a later-phase sample is assessed with its earlier phase", {
  frame <- ve_frame()
  phase1 <- execute(sampling_design() |> cluster_by(cluster) |> draw(n = 8),
                    frame, seed = 1)
  design2 <- sampling_design() |> cluster_by(id) |> draw(n = 10)
  phase2 <- execute(design2, phase1, seed = 2)
  expect_identical(variance_estimators(phase2),
                   variance_estimators(design2, phase1))
  expect_identical(variance_estimators(phase2)$class[3],
                   "samplyr_error_svrep_twophase_unsupported")

  phase3 <- execute(sampling_design() |> draw(n = 4), phase2, seed = 3)
  report <- variance_estimators(sampling_design() |> draw(n = 2), phase3)
  expect_identical(unique(report$class),
                   "samplyr_error_survey_multiphase_unsupported")
})

test_that("variance_estimators() refuses what is not a design", {
  expect_error(variance_estimators(data.frame(x = 1)),
               class = "samplyr_error_design_expected")
  expect_error(variance_estimators(sampling_design()),
               class = "samplyr_error_stage_incomplete")
})

test_that("a systematic stage that takes every unit is exempt once the frame shows it", {
  frame <- ve_frame()
  design <- ve_designs()$systematic_census
  expect_identical(variance_estimators(design)$status[1], "approximate")
  expect_match(variance_estimators(design)$note[1], "which the frame shows",
               fixed = TRUE)
  expect_identical(variance_estimators(design, frame)$status[1], "supported")
  skip_if_not_installed("survey")
  expect_no_warning(as_svydesign(execute(design, frame, seed = 1)))
})
