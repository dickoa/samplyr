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
    systematic_mr = sampling_design() |>
      draw(n = 10, method = "pps_systematic_mr", mos = mos),
    systematic_mr_two_stage = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(n = 2, method = "pps_systematic_mr", mos = mos) |>
      add_stage() |> draw(n = 2),
    cube = sampling_design() |> draw(n = 12, method = "cube", aux = c(x)),
    lpm2 = sampling_design() |>
      draw(n = 12, method = "lpm2", spread = c(lon, lat)),
    singleton_stratum = sampling_design() |> stratify_by(stratum) |>
      draw(n = c(A = 1, B = 3, C = 3, D = 3)),
    sps = sampling_design() |> draw(n = 10, method = "pps_sps", mos = mos),
    certainty = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 2, method = "pps_brewer", mos = mos, certainty_size = 80),
    # A systematic draw of one per pool is a draw proportional to size.
    systematic_one = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |> draw(n = 1, method = "systematic"),
    pps_systematic_one = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |> draw(n = 1, method = "pps_systematic", mos = mos),
    systematic_mr_one = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 1, method = "pps_systematic_mr", mos = mos),
    systematic_one_per_psu = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(n = 3) |>
      add_stage() |> draw(n = 1, method = "systematic"),
    # Each stratum holds one certainty cluster, so one cluster is drawn.
    systematic_certain_plus_one = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 2, method = "pps_systematic", mos = mos, certainty_size = 55),
    # Stratum A's cluster of size 90 is taken once, the rest with replacement.
    multinomial_certainty = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 3, method = "pps_multinomial", mos = mos, certainty_size = 80),
    chromy_certainty_two_stage = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(n = 3, method = "pps_chromy", mos = mos, certainty_size = 80) |>
      add_stage() |> draw(n = 2),
    systematic_mr_certainty = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 3, method = "pps_systematic_mr", mos = mos,
           certainty_size = 80),
    # Censuses. survey's JKn stops on a stratified one, BRR and Fay resampled
    # it, and the design alone did not show it.
    census = sampling_design() |> draw(frac = 1),
    stratified_census = sampling_design() |> stratify_by(stratum) |>
      draw(frac = 1),
    stratified_census_clusters = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |> draw(frac = 1),
    census_two_stage = sampling_design() |>
      add_stage() |> cluster_by(cluster) |> draw(frac = 1) |>
      add_stage() |> draw(frac = 1),
    census_then_srs = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(frac = 1) |>
      add_stage() |> draw(n = 2),
    stratum_taken_whole = sampling_design() |> stratify_by(stratum) |>
      draw(n = c(A = 30, B = 4, C = 4, D = 6)),
    pps_census = sampling_design() |>
      draw(n = 120, method = "pps_brewer", mos = mos),
    # Inside a certainty PSU, stage 2 keeps its correction only when it drew
    # with equal probabilities and nothing below it samples.
    census_then_two_stages = sampling_design() |>
      add_stage() |> cluster_by(stratum) |> draw(frac = 1) |>
      add_stage() |> cluster_by(cluster) |> draw(n = 2) |>
      add_stage() |> draw(n = 2),
    census_then_pps = sampling_design() |>
      add_stage() |> cluster_by(stratum) |> draw(frac = 1) |>
      add_stage() |> cluster_by(cluster) |>
      draw(n = 2, method = "pps_brewer", mos = mos),
    certainty_then_pps = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(n = 3, method = "pps_brewer", mos = mos, certainty_size = 80) |>
      add_stage() |> draw(n = 2, method = "pps_brewer", mos = x)
  )
}

# What an estimator's export raised: the first samplyr error class, and the
# samplyr warning classes.
ve_outcome <- function(key, sample, frame, twophase = FALSE,
                       joint_source = sample) {
  warnings <- character(0)
  error <- NA_character_
  withCallingHandlers(
    tryCatch(
      {
        pps <- if (twophase) {
          "brewer"
        } else if (identical(key, "joint")) {
          joint <- suppressWarnings(joint_expectation(joint_source, frame))
          survey::ppsmat(joint[[1]])
        }
        switch(
          key,
          linearization = as_svydesign(sample),
          joint = as_svydesign(sample, pps = pps),
          rwyb = as_svrepdesign(sample, type = "rwyb", replicates = 20),
          bootstrap = ,
          subbootstrap = ,
          mrbbootstrap = as_svrepdesign(sample, type = key, replicates = 20),
          as_svrepdesign(sample, type = key)
        )
      },
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
  keys <- vapply(samplyr:::estimator_catalog(), `[[`, "", "key")
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
  # Only the frame shows that a systematic stage takes every unit, or one
  # unit per pool, or that a size takes every unit.
  designs <- ve_designs()
  designs[c("systematic_census", "systematic_one", "pps_systematic_one",
            "systematic_mr_one", "systematic_one_per_psu",
            "systematic_certain_plus_one", "pps_census")] <- NULL
  problems <- unlist(Map(function(label, design) {
    ve_check(label, design, frame, report = variance_estimators(design))
  }, names(designs), designs))
  expect_identical(problems, character(0))
})

test_that("registers with an empty candidate unit report its zero row", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  schools <- data.frame(school = 1:6, st = rep(c("a", "b"), each = 3))
  classes <- data.frame(school = rep(1:5, each = 2), class = 1:10)
  frames <- list(schools, classes)
  design_for <- function(on_empty) {
    sampling_design() |>
      add_stage() |> stratify_by(st) |> cluster_by(school) |> draw(n = 2) |>
      add_stage() |> cluster_by(class) |> draw(n = 1, on_empty = on_empty)
  }
  # A selected school with no class exports as a zero row, so no estimator
  # is refused for it. The export says so under "warn".
  loud <- variance_estimators(design_for("warn"), frames)
  expect_match(
    loud$sample_risk[1],
    paste("1 candidate unit has nothing to sample at stage 2, and a selected",
          "one is exported as a zero row, so estimates need `na.rm = TRUE`",
          "(samplyr_warning_export_empty_parent)"),
    fixed = TRUE
  )
  quiet <- variance_estimators(design_for("silent"), frames)
  expect_false(any(grepl("zero row", quiet$sample_risk, fixed = TRUE)))
  expect_identical(quiet$status, loud$status)
  for (on_empty in c("warn", "silent")) {
    expect_identical(
      ve_check(on_empty, design_for(on_empty), frames, seeds = 1:6),
      character(0)
    )
  }
  # A sample that drew the empty school is supported, and exports.
  sample <- suppressWarnings(execute(design_for("warn"), frames, seed = 3))
  expect_false(is.null(vanished_primary_units(sample)))
  report <- variance_estimators(sample)
  expect_identical(report$status[1], "supported")
  expect_s3_class(suppressWarnings(as_svydesign(sample)), "survey.design")
})

test_that("an empty unit below a primary unit is reported under silent", {
  schools <- data.frame(school = 1:6, st = rep(c("a", "b"), each = 3))
  classes <- data.frame(school = rep(1:6, each = 2), class = 1:12)
  # Class 1 has no pupil.
  pupils <- data.frame(class = rep(2:12, each = 3), pupil = 1:33)
  pupils$school <- classes$school[match(pupils$class, classes$class)]
  frames <- list(schools, classes, pupils)
  design_for <- function(on_empty) {
    sampling_design() |>
      add_stage() |> stratify_by(st) |> cluster_by(school) |> draw(n = 2) |>
      add_stage() |> cluster_by(class) |> draw(n = 2) |>
      add_stage() |> draw(n = 2, on_empty = on_empty)
  }
  note <- paste(
    "1 candidate unit has nothing to sample at stage 3, and a selected one",
    "has no row, so survey computes its parent's within-unit variance",
    "without it"
  )
  # The export warns of it under "warn" alone.
  loud <- variance_estimators(design_for("warn"), frames)
  expect_identical(
    loud$sample_risk[1],
    paste0(note, " (samplyr_warning_export_empty_parent)")
  )
  quiet <- variance_estimators(design_for("silent"), frames)
  expect_identical(quiet$sample_risk[1], note)
  expect_identical(quiet$status, loud$status)
})

test_that("registers missing a column get the error execute() gives", {
  schools <- data.frame(school = 1:6, st = rep(c("a", "b"), each = 3))
  classes <- data.frame(school = rep(1:6, each = 2), class = 1:12, sz = 1:12)
  pupils <- data.frame(class = 1:12, pupil = 1:12)
  pupils$school <- classes$school
  design <- sampling_design() |>
    add_stage() |> stratify_by(st) |> cluster_by(school) |> draw(n = 2) |>
    add_stage() |> cluster_by(class) |>
    draw(n = 2, method = "pps_brewer", mos = sz) |>
    add_stage() |> draw(n = 1)
  expect_no_error(variance_estimators(design, list(schools, classes, pupils)))

  no_school <- pupils[c("class", "pupil")]
  no_mos <- classes[c("school", "class")]
  for (frames in list(
    list(frames = list(schools, classes, no_school),
         class = "samplyr_error_frame_missing_ancestry"),
    list(frames = list(schools, no_mos, pupils),
         class = "samplyr_error_frame_missing_vars")
  )) {
    expect_error(execute(design, frames$frames, seed = 1),
                 class = frames$class)
    expect_error(variance_estimators(design, frames$frames),
                 class = frames$class)
  }

  # A single frame may cover the first stages only.
  first <- variance_estimators(design, schools)
  expect_identical(first, variance_estimators(design, list(schools)))
  expect_identical(nrow(first), nrow(variance_estimators(design)))
})

ve_check_twophase <- function(label, design1, design2, frame, seeds = 1:2) {
  phase1 <- suppressMessages(suppressWarnings(
    execute(design1, frame, seed = 11)
  ))
  report <- variance_estimators(design2, phase1)
  keys <- vapply(samplyr:::estimator_catalog(), `[[`, "", "key")
  problems <- character(0)
  for (seed in seeds) {
    sample <- suppressMessages(suppressWarnings(
      execute(design2, phase1, seed = seed)
    ))
    # A subsample within phase 1's final pools exports as phase 1's design.
    composed <- samplyr:::composed_subsample(sample)
    for (i in seq_along(keys)) {
      if (keys[i] == "random_groups" || report$status[i] == "not applicable") {
        next
      }
      # Its stage-1 matrix is the phase-1 sample's.
      outcome <- if (is.null(composed)) {
        ve_outcome(keys[i], sample, NULL, twophase = TRUE)
      } else {
        ve_outcome(keys[i], sample, frame, joint_source = phase1)
      }
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
    systematic_one_phase1 = list(sampling_design() |> stratify_by(stratum) |>
                                   cluster_by(cluster) |>
                                   draw(n = 1, method = "systematic"),
                                 sampling_design() |> stratify_by(cluster) |>
                                   cluster_by(id) |> draw(n = 2)),
    systematic_one_phase2 = list(clusters, sampling_design() |>
                                   stratify_by(cluster) |> cluster_by(id) |>
                                   draw(n = 1, method = "systematic")),
    # Phase 1 declares its rows, and phase 2 clusters above them.
    whole_units = list(sampling_design() |>
                         add_stage() |> cluster_by(cluster) |> draw(n = 8) |>
                         add_stage() |> cluster_by(id) |> draw(n = 3),
                       sampling_design() |> cluster_by(cluster) |>
                         draw(n = 4)),
    units_then_rows = list(sampling_design() |>
                             add_stage() |> cluster_by(cluster) |>
                             draw(n = 8) |>
                             add_stage() |> cluster_by(id) |> draw(n = 3),
                           sampling_design() |>
                             add_stage() |> cluster_by(cluster) |>
                             draw(n = 4) |>
                             add_stage() |> cluster_by(id) |> draw(n = 2)),
    # Exported as phase 1's design, each hit being a final pool.
    within_hits = list(sampling_design() |>
                         add_stage() |> cluster_by(cluster) |>
                         draw(n = 8, method = "pps_multinomial", mos = mos) |>
                         add_stage() |> draw(n = 3),
                       sampling_design() |>
                         stratify_by(cluster, .phase1_draw_1) |> draw(n = 2)),
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
                     sampling_design() |> cluster_by(id) |> draw(n = 10)),
    # A subsample within each final-stage pool exports as phase 1's design.
    within_pools = list(sampling_design() |>
                          add_stage() |> cluster_by(cluster) |>
                          draw(n = 8, method = "pps_brewer", mos = mos) |>
                          add_stage() |> draw(n = 4),
                        sampling_design() |> stratify_by(cluster) |>
                          draw(n = 2))
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
    srs = "S-SRRRSASSS",
    stratified = "S-RSAASASSS",
    two_per_stratum = "S-RSAASASSS",
    two_stage = "S-RARRAASSS",
    one_per_psu = "A-RARRAASRS",
    brewer = "AARAAAASSAS",
    brewer_two_stage = "ARRAAAASSAS",
    sampford_clusters = "ARRAAAASSAS",
    pps_systematic = "AARAAAAAAAS",
    systematic = "A-ARRRAAAAS",
    # A census adds no variance, and BRR, Fay and the rescaled bootstrap
    # keep its rows at their full weight.
    systematic_census = "S-RRSSSSSSS",
    bernoulli = "S-RRRRRRRSS",
    pps_poisson = "SSRRRRRRRSS",
    poisson_clusters = "R-RRRRRRRSS",
    later_poisson = "R-RRRRRRRSS",
    srswr = "S-SRRRSSSSS",
    multinomial_two_stage = "S-SRRRSSSSS",
    # Every replicate type reads minimum replacement as with replacement.
    chromy = "A-ARRRAAARS",
    # One random start: no replicate type reproduces it.
    systematic_mr = "A-ARRRAAARS",
    systematic_mr_two_stage = "A-RAAAAAARS",
    cube = "AARAAAASSRS",
    lpm2 = "R-RRRRRAARS",
    singleton_stratum = "A-RRRRSRSRS",
    sps = "AARAAAASSRS",
    certainty = "ARRRRRARSRS",
    # A draw of one per pool is refused or approximated as its non-systematic
    # counterpart is, for its lonely strata.
    systematic_one = "A-RRRRSRSRS",
    pps_systematic_one = "ARRRRRARSRS",
    systematic_mr_one = "A-RRRRARARS",
    systematic_one_per_psu = "A-RARRAASRS",
    systematic_certain_plus_one = "ARRRRRARSRS",
    # A certainty unit taken once is a take-all stratum of the stage.
    multinomial_certainty = "S-RSRRSSSSS",
    # Its certain PSU's pools are read from the frame, which shows an odd
    # first-stage take that BRR and Fay cannot pair.
    chromy_certainty_two_stage = "A-RARRAAARS",
    systematic_mr_certainty = "A-RARRAAARS",
    census = "S-SRRRSSSSS",
    # survey's JKn stops on a stratified census.
    stratified_census = "S-RRSSSSSSS",
    stratified_census_clusters = "S-RRSSSSSSS",
    # A first stage taken whole above a later stage is a set of certainty
    # units, which stratifies it for JK1.
    census_two_stage = "S-RSSSSSSSS",
    census_then_srs = "S-RSSSSSSSS",
    stratum_taken_whole = "S-RSAASASSS",
    pps_census = "AARSSSSSSAS",
    # Stage 2 inside a certainty PSU is read as drawn with replacement when
    # a stage below samples or stage 2 is PPS.
    census_then_two_stages = "S-RAAAAASSS",
    census_then_pps = "A-RAAAAASAS",
    certainty_then_pps = "ARRAAAAASAS"
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

test_that("a systematic draw of one unit per pool is not reported as systematic", {
  skip_if_not_installed("survey")
  frame <- ve_frame()
  designs <- ve_designs()
  one <- c("systematic_one", "pps_systematic_one", "systematic_mr_one",
           "systematic_one_per_psu", "systematic_certain_plus_one")
  # The same designs drawing two units per pool keep the warning.
  two <- list(
    systematic_two = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |> draw(n = 2, method = "systematic"),
    systematic_mr_two = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 2, method = "pps_systematic_mr", mos = mos),
    systematic_two_per_psu = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(n = 3) |>
      add_stage() |> draw(n = 2, method = "systematic"),
    systematic_certain_plus_two = sampling_design() |> stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 3, method = "pps_systematic", mos = mos, certainty_size = 55)
  )
  cases <- c(designs[one], two)
  checked <- character(0)
  for (label in names(cases)) {
    design <- cases[[label]]
    systematic <- label %in% names(two)
    stated <- variance_estimators(design, frame)$class
    expect_identical(
      any(grepl("samplyr_warning_systematic_variance", stated)), systematic,
      info = label
    )
    for (seed in 1:3) {
      sample <- suppressMessages(execute(design, frame, seed = seed))
      raised <- character(0)
      withCallingHandlers(
        as_svydesign(sample),
        warning = function(w) {
          raised <<- c(raised, class(w)[1])
          invokeRestart("muffleWarning")
        }
      )
      expect_identical(
        "samplyr_warning_systematic_variance" %in% raised, systematic,
        info = paste(label, seed)
      )
      if (!systematic) {
        # The refusing policy has nothing to refuse.
        expect_no_error(suppressWarnings(
          as_svydesign(sample, systematic_variance = "error")
        ))
        expect_setequal(unique(raised), "samplyr_warning_lonely_psu")
      }
    }
    checked <- c(checked, label)
  }
  expect_setequal(checked, c(one, names(two)))
})

test_that("a two-phase systematic draw of one per pool is not reported as systematic", {
  skip_if_not_installed("survey")
  frame <- ve_frame()
  clusters <- suppressMessages(execute(
    sampling_design() |> cluster_by(cluster) |> draw(n = 8), frame, seed = 11
  ))
  checked <- character(0)
  for (n in 1:2) {
    design2 <- sampling_design() |> stratify_by(cluster) |> cluster_by(id) |>
      draw(n = n, method = "systematic")
    stated <- variance_estimators(design2, clusters)$class[1]
    sample <- suppressMessages(execute(design2, clusters, seed = 1))
    raised <- character(0)
    withCallingHandlers(
      as_svydesign(sample),
      warning = function(w) {
        raised <<- c(raised, class(w)[1])
        invokeRestart("muffleWarning")
      }
    )
    systematic <- n == 2L
    expect_identical(
      grepl("samplyr_warning_systematic_variance", stated), systematic,
      info = paste("n =", n)
    )
    expect_identical(
      "samplyr_warning_systematic_variance" %in% raised, systematic,
      info = paste("n =", n)
    )
    checked <- c(checked, paste("n =", n))
  }
  expect_setequal(checked, c("n = 1", "n = 2"))
})

test_that("a frame of primary units settles the first stage only", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  primary <- ve_frame()[c("id", "stratum", "cluster", "mos", "x", "y")]
  full <- primary
  full$segment <- full$x %% 2
  full$segment_mos <- ifelse(full$segment == 1, 3, 2)
  design <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_systematic", mos = mos) |>
    add_stage() |> cluster_by(segment) |>
    draw(n = 1, method = "pps_brewer", mos = segment_mos) |>
    add_stage() |> draw(n = 2)
  report <- variance_estimators(design, primary)
  expect_identical(
    ve_check("primary frame", design, full, report = report),
    character(0)
  )
  # Stage 1 is read from the frame, the stages below from the design.
  expect_match(report$note[1],
               "stage 1 has no design-unbiased variance estimator. Its variance is approximated. ",
               fixed = TRUE)
  expect_match(variance_estimators(design)$note[1],
               "stage 1 has no design-unbiased variance estimator. Its variance is approximated, unless",
               fixed = TRUE)
  expect_identical(report$status[2], variance_estimators(design)$status[2])
  expect_match(report$sample_risk[1], "A frame that reaches every stage",
               fixed = TRUE)
  expect_error(variance_estimators(design, primary["stratum"]),
               class = "samplyr_error_frame_missing_vars")
})

test_that("a certainty PSU keeping one unit below it is refused by the stratified types", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  primary <- ve_frame()[c("id", "stratum", "cluster", "mos", "x", "y")]
  full <- primary
  full$segment <- full$x %% 2
  full$segment_mos <- ifelse(full$segment == 1, 3, 2)
  # Three of six clusters per stratum puts the largest one at certainty.
  segments <- function(take) {
    sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(n = 3, method = "pps_systematic", mos = mos) |>
      add_stage() |> cluster_by(segment) |>
      draw(n = take, method = "pps_brewer", mos = segment_mos) |>
      add_stage() |> draw(n = 2)
  }
  elements <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 3, method = "pps_systematic", mos = mos) |>
    add_stage() |> draw(n = 1)
  cases <- list(one_segment = segments(1), two_segments = segments(2),
                one_element = elements)
  for (label in names(cases)) {
    expect_identical(ve_check(label, cases[[label]], full), character(0))
  }
  expect_identical(
    ve_check("primary frame", segments(1), full,
             report = variance_estimators(segments(1), primary)),
    character(0)
  )

  stratified <- c(4, 5, 6, 8)
  report <- variance_estimators(segments(1), full)
  expect_identical(report$status[stratified], rep("refused", 4))
  expect_match(report$note[4],
               "4 stage-2 pools under a PSU taken with certainty take a single unit",
               fixed = TRUE)
  expect_identical(report$status[c(7, 9)], c("approximate", "approximate"))
  expect_false(any(variance_estimators(segments(2), full)$status[stratified] ==
                     "refused"))
  partial <- variance_estimators(segments(1), primary)
  expect_identical(partial$status[stratified], rep("approximate", 4))
  expect_match(partial$sample_risk[4],
               "A PSU taken with certainty is a stratum of its stage-2 units",
               fixed = TRUE)
})

## Samples whose export follows rules of its own

test_that("occasions and windows are refused, not reported as their master", {
  frame <- data.frame(id = 1:600, h = rep(c("a", "b", "c"), each = 200))
  schedule <- expand.grid(panel = 1:6, period = 1:3)
  schedule$active <- (schedule$period - 1) %% 3 == (schedule$panel - 1) %% 3
  master <- sampling_design() |>
    stratify_by(h) |>
    draw(n = 15) |>
    execute(frame, seed = 1, panels = schedule)

  expect_no_error(variance_estimators(master))
  for (occasion in list(1, 1:2)) {
    w <- execute(master, period = occasion)
    expect_error(variance_estimators(w),
                 class = "samplyr_error_variance_estimators_input")
    expect_error(
      variance_estimators(sampling_design() |> draw(n = 3), frame = w),
      class = "samplyr_error_variance_estimators_input"
    )
  }
})

test_that("a shared-weight sample is refused, not reported as its source", {
  source_frame <- data.frame(unit = letters[1:12], mos = 1:12)
  source <- sampling_design() |>
    draw(n = 4, method = "pps_sampford", mos = mos) |>
    execute(source_frame, seed = 2)
  shared <- share_weights(
    source,
    targets = data.frame(tid = paste0("t", 1:12),
                         hh = rep(paste0("H", 1:6), each = 2)),
    links = data.frame(unit = letters[1:12], tid = paste0("t", 1:12)),
    by = c(unit = "unit"), to = c(tid = "tid"),
    within = hh, multiplicity = complete_links()
  )

  expect_no_error(variance_estimators(source))
  expect_error(variance_estimators(shared),
               class = "samplyr_error_variance_estimators_input")
})

test_that("a design with a reserve is refused, listed or settled", {
  frame <- data.frame(region = rep(c("a", "b"), each = 20), id = 1:40)
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4, reserve = 4)
  listed <- execute(design, frame, seed = 1)
  listed$result <- ifelse(listed$.reserve_1 == 0, "interview", NA)
  settled <- execute(listed, response_status = result)
  for (x in list(design, listed, settled)) {
    expect_error(variance_estimators(x),
                 "does not assess a design with a reserve",
                 class = "samplyr_error_variance_estimators_input")
  }
  # A second phase drawn from a settled reserve is refused too.
  expect_error(
    variance_estimators(sampling_design() |> draw(n = 3), frame = settled),
    class = "samplyr_error_variance_estimators_input"
  )
  expect_no_error(variance_estimators(sampling_design() |> draw(n = 4)))
})

test_that("within-pool subsampling carries phase-2 pool facts into the composed report", {
  p1 <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 6) |>
    execute(bfa_eas, seed = 1)
  d2 <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 1)
  p2 <- suppressMessages(execute(d2, p1, seed = 2))

  single_ref <- variance_estimators(d2, bfa_eas)
  two_pre <- variance_estimators(d2, p1)
  two_post <- variance_estimators(p2)

  # Random groups of phase 2 alone share the one realized phase 1.
  other <- single_ref$estimator != "Random groups"
  expect_identical(two_pre$status[other], single_ref$status[other])
  expect_identical(two_pre$decided_by[other], single_ref$decided_by[other])
  expect_identical(two_pre$class[other], single_ref$class[other])
  expect_identical(two_pre$class[!other], "samplyr_error_random_groups_shared")
  expect_identical(two_post, two_pre)
})

test_that("joint_estimator_row refuses a stage that draws one PSU per zone", {
  skip_if_not_installed("svyplan")
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")

  plan <- certainty_pair_fixture()
  register <- certainty_pair_register()
  design <- sampling_design() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N)

  rep_alone <- variance_estimators(design)
  rep_frame <- variance_estimators(design, register)
  expect_identical(rep_alone$status[2], "refused")
  expect_identical(rep_alone$class[2], "samplyr_error_joint_method_unsupported")
  expect_identical(rep_frame$status[2], "refused")
  expect_identical(rep_frame$class[2], "samplyr_error_joint_method_unsupported")
  expect_identical(ve_check("one_per_zone", design, register), character(0))
})

test_that("the preflight refuses exactly the replicate exports that fail", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # Pools of 150 units keep their chances in quantile bins, which average a
  # unit of chance one with its neighbors. Each stratum has one such unit,
  # so a take of 2, 6 or 7 leaves 1, 5 or 6 units to resample.
  n_pool <- 150
  frame <- data.frame(
    id = seq_len(2 * n_pool),
    st = rep(c("a", "b"), each = n_pool),
    mos = rep(c(400, seq(1, 3, length.out = n_pool - 1)), 2),
    y = seq_len(2 * n_pool)
  )
  types <- c(
    JKn = "Stratified jackknife (JKn)",
    BRR = "Balanced repeated replication",
    Fay = "Fay's balanced repeated replication",
    bootstrap = "Bootstrap",
    subbootstrap = "Rescaled bootstrap",
    mrbbootstrap = "Multistage rescaled bootstrap"
  )
  outcomes <- character(0)
  for (method in c("pps_brewer", "pps_systematic", "pps_sampford", "pps_cps")) {
    for (n in c(2, 6, 7)) {
      design <- sampling_design() |>
        stratify_by(st) |>
        draw(n = n, method = method, mos = mos)
      rows <- variance_estimators(design, frame)
      s <- suppressMessages(execute(design, frame, seed = 1))
      expect_identical(sum(s$.certainty_1), 2L)
      for (type in names(types)) {
        refused <- identical(
          rows$status[rows$estimator == types[[type]]], "refused"
        )
        failed <- tryCatch(
          {
            suppressWarnings(as_svrepdesign(
              s, type = type, systematic_variance = "approximate"
            ))
            FALSE
          },
          error = function(e) TRUE
        )
        expect_identical(refused, failed, info = paste(method, n, type))
        outcomes <- c(outcomes, if (failed) "failed" else "exported")
      }
    }
  }
  expect_length(outcomes, 72)
  expect_setequal(outcomes, c("failed", "exported"))
})

test_that("the preflight reads the frame below a stage drawn with replacement", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # PSU 1 is certain under pps_multinomial. The frame facts stopped at a
  # stage drawn with replacement, so every replicate type was reported as
  # the design alone decides it.
  psus <- data.frame(psu = 1:5, mos = c(100, 10, 10, 10, 10))
  hh <- data.frame(psu = rep(1:5, each = 4), id = 1:20,
                   y = (1:20 %% 6) * 3)
  frames <- list(psus, hh)
  estimators <- c(
    linearization = "Taylor linearization", JK1 = "Jackknife (JK1)",
    JKn = "Stratified jackknife (JKn)", bootstrap = "Bootstrap",
    subbootstrap = "Rescaled bootstrap",
    mrbbootstrap = "Multistage rescaled bootstrap",
    rwyb = "Rao-Wu-Yue-Beaumont bootstrap"
  )
  exported <- function(s, type) {
    tryCatch(
      withCallingHandlers({
        if (type == "linearization") {
          survey::svytotal(~y, as_svydesign(s))
        } else {
          as_svrepdesign(s, type = type, replicates = 10)
        }
        "ok"
      }, warning = function(w) {
        if (inherits(w, "samplyr_warning_lonely_psu")) {
          rlang::abort("lonely", class = "lonely")
        }
        invokeRestart("muffleWarning")
      }),
      lonely = function(e) "lonely",
      error = function(e) class(e)[1]
    )
  }
  expected <- list(
    `1` = c(linearization = "approximate", JK1 = "refused", JKn = "refused",
            bootstrap = "supported", subbootstrap = "refused",
            mrbbootstrap = "supported", rwyb = "refused"),
    `2` = c(linearization = "supported", JK1 = "refused", JKn = "supported",
            bootstrap = "supported", subbootstrap = "supported",
            mrbbootstrap = "supported", rwyb = "supported")
  )
  for (take in 1:2) {
    design <- sampling_design() |>
      add_stage() |> cluster_by(psu) |>
      draw(n = 3, method = "pps_multinomial", mos = mos,
           certainty_size = 50) |>
      add_stage() |> draw(n = take)
    v <- variance_estimators(design, frames)
    status <- stats::setNames(v$status[match(estimators, v$estimator)],
                              names(estimators))
    expect_identical(status, expected[[as.character(take)]],
                     info = paste("take", take))
    s <- execute(design, frames, seed = 1)
    expect_true(s$.certainty_1[s$psu == 1][1])
    for (type in names(estimators)) {
      out <- exported(s, type)
      info <- paste(type, "take", take)
      if (status[[type]] == "refused") {
        expect_false(out %in% c("ok", "lonely"), info = info)
      } else if (status[[type]] == "approximate") {
        expect_true(out %in% c("ok", "lonely"), info = info)
      } else {
        expect_identical(out, "ok", info = info)
      }
    }
    if (take == 1) {
      expect_identical(v$class[v$estimator == estimators[["rwyb"]]],
                       "samplyr_error_rwyb_singleton")
      expect_identical(v$decided_by[v$estimator == estimators[["rwyb"]]],
                       "frame")
    }
  }
  # Without the frame, whether stage 1 takes a unit for sure is unknown:
  # a risk, not a refusal the design would decide.
  v <- variance_estimators(design)
  row <- v[v$estimator == estimators[["JKn"]], ]
  expect_false(identical(row$status, "refused"))
  expect_match(row$sample_risk, "depends on the frame")
  # Without a certainty rule nothing below stage 1 is a stratum of its own.
  plain <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 1)
  v <- variance_estimators(plain, frames)
  expect_identical(v$status[v$estimator == estimators[["JKn"]]], "refused")
  expect_identical(v$status[v$estimator == estimators[["JK1"]]], "supported")
  expect_identical(v$status[v$estimator == estimators[["rwyb"]]], "supported")
})

test_that("a take of one from a pool of one under a WR certainty is a census", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # `n = 1` inside a certain PSU leaves a stratum of one unit only when the
  # PSU holds more than one. The preflight inferred it from `n = 1` alone
  # and refused RWYB, the rescaled bootstrap and JKn, which export.
  estimators <- c(
    linearization = "Taylor linearization",
    JKn = "Stratified jackknife (JKn)", subbootstrap = "Rescaled bootstrap",
    rwyb = "Rao-Wu-Yue-Beaumont bootstrap"
  )
  exported <- function(s, type) {
    tryCatch(
      withCallingHandlers({
        if (type == "linearization") {
          survey::svytotal(~y, as_svydesign(s))
        } else {
          as_svrepdesign(s, type = type, replicates = 10)
        }
        "ok"
      }, warning = function(w) {
        if (inherits(w, "samplyr_warning_lonely_psu")) {
          rlang::abort("lonely", class = "lonely")
        }
        invokeRestart("muffleWarning")
      }),
      lonely = function(e) "lonely",
      error = function(e) class(e)[1]
    )
  }
  checked <- character(0)
  agrees <- function(design, frame, label, expected, types = names(expected)) {
    v <- variance_estimators(design, frame)
    status <- stats::setNames(v$status[match(estimators[types], v$estimator)],
                              types)
    expect_identical(status, expected, info = label)
    for (seed in 1:3) {
      s <- suppressWarnings(execute(design, frame, seed = seed))
      expect_true(any(s$.certainty_1), info = label)
      for (type in types) {
        out <- exported(s, type)
        info <- paste(label, type, seed)
        if (status[[type]] == "refused") {
          expect_false(out %in% c("ok", "lonely"), info = info)
        } else {
          expect_identical(out, "ok", info = info)
        }
      }
    }
    checked <<- c(checked, label)
    v
  }
  supported <- c(linearization = "supported", JKn = "supported",
                 subbootstrap = "supported", rwyb = "supported")
  m <- c(100, 40, 30, 15, 10, 5)
  wr <- sampling_design() |> cluster_by(psu) |>
    draw(n = 5, method = "pps_multinomial", mos = m,
         certainty_size = 60) |>
    add_stage() |> draw(n = 1)
  # Every PSU holds one unit, as one register and as two.
  psus <- data.frame(psu = 1:6, m = m)
  units <- data.frame(psu = 1:6, id = 1:6, y = c(9, 4, 6, 2, 3, 1))
  for (form in c("single", "separate")) {
    frame <- if (form == "single") merge(psus, units) else list(psus, units)
    agrees(wr, frame, paste("census", form), supported)
    s <- suppressWarnings(execute(wr, frame, seed = 2))
    expect_true(all(s$.weight_2 == 1))
  }
  # Only the pools under a certain PSU are read: the others take one unit
  # per hit, which the replicate types resample with their PSU.
  units <- data.frame(psu = rep(1:6, c(1, 3, 3, 3, 3, 3)))
  units$id <- seq_len(nrow(units))
  units$y <- units$id %% 7 + 1
  replicate_types <- c("JKn", "subbootstrap", "rwyb")
  agrees(wr, merge(psus, units), "census among larger",
         supported[replicate_types])
  # One certain PSU of one unit and one of four: the second keeps a single
  # unit outside certainty.
  psus <- data.frame(psu = 1:7, m = c(100, 90, 30, 15, 10, 5, 5))
  units <- data.frame(psu = rep(1:7, c(1, 4, 3, 3, 3, 3, 3)))
  units$id <- seq_len(nrow(units))
  units$y <- units$id %% 7 + 1
  for (form in c("single", "separate")) {
    frame <- if (form == "single") merge(psus, units) else list(psus, units)
    v <- agrees(wr, frame, paste("single", form),
                c(JKn = "refused", subbootstrap = "refused",
                  rwyb = "refused"))
    row <- v[v$estimator == estimators[["rwyb"]], ]
    expect_identical(row$class, "samplyr_error_rwyb_singleton")
    expect_identical(row$decided_by, "frame")
  }
  # The certain PSU holds one area, taken whole. Its replicate stratum is
  # that one area, which adds no variance unless stage 3 draws inside it.
  psus <- data.frame(psu = 1:6, m = m)
  rows <- data.frame(psu = rep(1:6, c(3, 6, 6, 6, 6, 6)))
  rows$ea <- paste(rows$psu, c(1, 1, 1, rep(1:2, each = 3, times = 5)))
  rows$id <- seq_len(nrow(rows))
  rows$y <- rows$id %% 5 + 1
  frame <- merge(psus, rows)
  three <- function(last) {
    sampling_design() |> add_stage() |> cluster_by(psu) |>
      draw(n = 5, method = "pps_multinomial", mos = m,
           certainty_size = 60) |>
      add_stage() |> cluster_by(ea) |> draw(n = 1) |> last()
  }
  agrees(three(function(d) draw(add_stage(d), frac = 1)), frame,
         "census below", supported[replicate_types])
  drawn <- three(function(d) draw(add_stage(d), n = 2))
  v <- agrees(drawn, frame, "drawn below",
              c(JKn = "refused", subbootstrap = "refused", rwyb = "supported"))
  expect_identical(v$decided_by[v$estimator == estimators[["JKn"]]], "frame")
  # A frame of stage 1 alone leaves the stage-2 pools unresolved: a risk,
  # also for the with-replacement reading of stage 2 inside the certain PSU.
  v <- variance_estimators(drawn, psus)
  row <- v[v$estimator == estimators[["JKn"]], ]
  expect_identical(row$status, "supported")
  expect_match(row$sample_risk, "taken with certainty")
  expect_match(row$sample_risk,
               "samplyr_warning_replicate_certainty_stage_wr", fixed = TRUE)
  expect_setequal(checked, c("census single", "census separate",
                             "census among larger", "single single",
                             "single separate", "census below",
                             "drawn below"))
})

test_that("a pool of one unit taken whole is no singleton in the preflight", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # The preflight counted any take of one as a stratum of one unit, while
  # the exports read a pool taken whole as a census. RWYB was refused for
  # PSUs of one unit each, and it exports.
  estimators <- c(
    linearization = "Taylor linearization",
    JKn = "Stratified jackknife (JKn)", BRR = "Balanced repeated replication",
    subbootstrap = "Rescaled bootstrap",
    rwyb = "Rao-Wu-Yue-Beaumont bootstrap"
  )
  exported <- function(s, type) {
    tryCatch(
      withCallingHandlers({
        if (type == "linearization") {
          survey::svytotal(~y, as_svydesign(s))
        } else if (type %in% c("JKn", "BRR")) {
          as_svrepdesign(s, type = type)
        } else {
          as_svrepdesign(s, type = type, replicates = 10)
        }
        "ok"
      }, warning = function(w) {
        if (inherits(w, "samplyr_warning_lonely_psu")) {
          rlang::abort("lonely", class = "lonely")
        }
        invokeRestart("muffleWarning")
      }),
      lonely = function(e) "lonely",
      error = function(e) class(e)[1]
    )
  }
  checked <- character(0)
  agrees <- function(design, frame, label, expected) {
    v <- variance_estimators(design, frame)
    types <- names(expected)
    rows <- v[match(estimators[types], v$estimator), ]
    expect_identical(stats::setNames(rows$status, types), expected,
                     info = label)
    for (seed in 1:3) {
      s <- suppressWarnings(execute(design, frame, seed = seed))
      for (type in types) {
        out <- exported(s, type)
        info <- paste(label, type, seed)
        if (expected[[type]] == "refused") {
          expect_false(out %in% c("ok", "lonely"), info = info)
        } else if (out != "ok") {
          # A failure the preflight did not prove is one it names as a risk.
          class <- if (out == "lonely") "samplyr_warning_lonely_psu" else out
          expect_match(rows$sample_risk[types == type], class, fixed = TRUE,
                       info = info)
        }
      }
    }
    checked <<- c(checked, label)
    v
  }
  units <- function(sizes, m = seq_along(sizes) + 9) {
    out <- data.frame(psu = rep(seq_along(sizes), sizes))
    out$m <- m[out$psu]
    out$id <- seq_len(nrow(out))
    out$y <- out$id %% 7 + 1
    out
  }
  srs <- sampling_design() |> add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> draw(n = 1)
  ones <- units(rep(1, 6))
  agrees(srs, ones, "srswor ones",
         c(linearization = "supported", rwyb = "supported"))
  # Exact: stage 2 adds nothing, so the variance estimator is the SRS one,
  # 6^2 (1 - 3 / 6) s^2 / 3. Band fixed before running: four Monte Carlo
  # standard errors of a variance from 2,000 replicates, 4 * sqrt(2 / 2000).
  s <- suppressWarnings(execute(srs, ones, seed = 1))
  exact <- 6^2 * (1 - 3 / 6) * stats::var(s$y) / 3
  for (seed in 1:3) {
    r <- withr::with_seed(seed, as_svrepdesign(s, type = "rwyb",
                                                replicates = 2000))
    ratio <- unname(vcov(survey::svytotal(~y, r))) / exact
    expect_lt(abs(ratio - 1), 4 * sqrt(2 / 2000), label = paste("seed", seed))
  }
  # Pools of one and of three: whether a singleton is drawn depends on the
  # PSUs selected, a risk rather than a refusal.
  v <- agrees(srs, units(rep(c(1, 3), 4)), "srswor mixed",
              c(linearization = "supported", rwyb = "supported"))
  expect_match(v$sample_risk[v$estimator == estimators[["rwyb"]]],
               "samplyr_error_rwyb_singleton", fixed = TRUE)
  agrees(srs, units(rep(3, 6)), "srswor threes",
         c(rwyb = "refused"))
  # A draw with replacement from a pool of one counts draws: no census.
  srswr <- sampling_design() |> add_stage() |> cluster_by(psu) |>
    draw(n = 3) |> add_stage() |> draw(n = 1, method = "srswr")
  agrees(srswr, ones, "srswr ones", c(rwyb = "refused"))
  # Under a PSU taken with certainty, a pool of one unit taken whole is a
  # replicate stratum of one area, which survey cannot resample when stage 3
  # draws inside it. PSU 1 is certain by its size, and the two others drawn
  # pair up for BRR.
  m <- c(100, 10, 11, 12, 13, 14, 15)
  areas <- data.frame(psu = rep(1:7, each = 3), m = rep(m, each = 3))
  areas$ea <- paste0("e", areas$psu)
  areas$id <- seq_len(nrow(areas))
  areas$y <- areas$id %% 5 + 1
  three <- function(last) {
    d <- sampling_design() |> add_stage() |> cluster_by(psu) |>
      draw(n = 3, method = "pps_brewer", mos = m) |>
      add_stage() |> cluster_by(ea) |> draw(n = 1) |> add_stage()
    if (last == "census") draw(d, frac = 1) else draw(d, n = 2)
  }
  agrees(three("census"), areas, "census below",
         c(JKn = "approximate", BRR = "approximate",
           subbootstrap = "supported", rwyb = "approximate"))
  v <- agrees(three("drawn"), areas, "drawn below",
              c(JKn = "refused", BRR = "refused", subbootstrap = "refused",
                rwyb = "approximate"))
  expect_identical(v$decided_by[v$estimator == estimators[["JKn"]]], "frame")
  s <- suppressWarnings(execute(three("drawn"), areas, seed = 1))
  expect_identical(sum(s$.certainty_1 & !duplicated(s$psu)), 1L)
  # The same rule under a stage-1 draw with replacement.
  wr <- sampling_design() |> add_stage() |> cluster_by(psu) |>
    draw(n = 5, method = "pps_multinomial", mos = m, certainty_size = 60) |>
    add_stage() |> draw(n = 1, method = "srswr")
  agrees(wr, units(rep(1, 6), c(100, 40, 30, 15, 10, 5)), "wr srswr ones",
         c(JKn = "refused", subbootstrap = "refused", rwyb = "refused"))
  expect_setequal(checked, c("srswor ones", "srswor mixed", "srswor threes",
                             "srswr ones", "census below", "drawn below",
                             "wr srswr ones"))
})

test_that("a certain PSU's stratum counts the units it selects", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # The stratum a certain PSU makes of its stage-2 units was read from the
  # take outside certainty. A take of two capped at a pool of one read two,
  # and the one area taken with certainty by pps_brewer read zero. Both are
  # a stratum of one area, which survey cannot resample when stage 3 draws
  # inside it.
  frame <- data.frame(
    psu = rep(1:7, each = 3), m = rep(c(100, rep(10, 6)), each = 3),
    ea = rep(1:7, each = 3), id = 1:21, y = (1:21) %% 7 + 1
  )
  labels <- c(JKn = "Stratified jackknife (JKn)",
              BRR = "Balanced repeated replication",
              subbootstrap = "Rescaled bootstrap",
              rwyb = "Rao-Wu-Yue-Beaumont bootstrap")
  design <- function(stage2, last) {
    d <- sampling_design() |> cluster_by(psu) |>
      draw(n = 3, method = "pps_brewer", mos = m, certainty_size = 60) |>
      add_stage() |> cluster_by(ea)
    d <- add_stage(stage2(d))
    if (last == "census") draw(d, frac = 1) else draw(d, n = 2)
  }
  stage2 <- list(
    srs = function(d) draw(d, n = 1),
    capped = function(d) draw(d, n = 2),
    certain = function(d) draw(d, n = 1, method = "pps_brewer", mos = m)
  )
  exported <- function(s, type) {
    tryCatch({
      suppressWarnings(if (type %in% c("JKn", "BRR")) {
        as_svrepdesign(s, type = type)
      } else {
        as_svrepdesign(s, type = type, replicates = 20)
      })
      "ok"
    }, error = function(e) class(e)[1])
  }
  reference <- suppressWarnings(execute(design(stage2$srs, "drawn"), frame,
                                        seed = 1))
  checked <- character(0)
  for (case in names(stage2)) {
    for (last in c("drawn", "census")) {
      d <- design(stage2[[case]], last)
      v <- variance_estimators(d, frame)
      s <- suppressWarnings(execute(d, frame, seed = 1))
      expect_true(all(s$.weight_2 == 1))
      if (case == "capped" && last == "drawn") {
        # The same sample as one area drawn by simple random sampling.
        expect_identical(s$id, reference$id, info = case)
        expect_identical(s$.weight, reference$.weight, info = case)
      }
      for (type in names(labels)) {
        info <- paste(case, last, type)
        row <- v[v$estimator == labels[[type]], ]
        out <- exported(s, type)
        if (last == "drawn" && type != "rwyb") {
          expect_identical(row$status, "refused", info = info)
          expect_identical(row$decided_by, "frame", info = info)
          expect_identical(out, "samplyr_error_svrep_conversion_failed",
                           info = info)
        } else {
          expect_false(row$status == "refused", info = info)
          expect_identical(out, "ok", info = info)
        }
        checked <- c(checked, info)
      }
    }
  }
  expect_length(unique(checked), 24L)
  # One certain area and one area drawn beside it, in a certain PSU of five:
  # a noncertainty singleton, refused whatever the stages below.
  five <- data.frame(psu = c(rep(1, 15), rep(2:7, each = 3)))
  five$m <- ifelse(five$psu == 1, 100, 10)
  five$ea <- paste(five$psu, c(rep(1:5, each = 3), rep(1, 18)))
  five$am <- ifelse(five$ea == "1 1", 100, 1)
  five$id <- seq_len(nrow(five))
  five$y <- five$id %% 7 + 1
  d <- sampling_design() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_brewer", mos = m, certainty_size = 60) |>
    add_stage() |> cluster_by(ea) |>
    draw(n = 2, method = "pps_brewer", mos = am, certainty_size = 50) |>
    add_stage() |> draw(frac = 1)
  v <- variance_estimators(d, five)
  s <- suppressWarnings(execute(d, five, seed = 1))
  expect_identical(length(unique(s$ea[s$psu == 1])), 2L)
  for (type in c("JKn", "BRR", "subbootstrap")) {
    expect_identical(v$status[v$estimator == labels[[type]]], "refused",
                     info = type)
    expect_identical(exported(s, type),
                     "samplyr_error_svrep_conversion_failed", info = type)
  }
})

test_that("any fixed take below a WR certainty PSU is read from the frame", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # The pools inside a PSU a draw with replacement takes for sure were read
  # only for a scalar n = 1. A take by stratum, n = c(A = 1, B = 2) or the
  # same as a table, was called supported, with a risk, and the replicate
  # exports refused it.
  labels <- c(JKn = "Stratified jackknife (JKn)",
              subbootstrap = "Rescaled bootstrap",
              rwyb = "Rao-Wu-Yue-Beaumont bootstrap")
  exported <- function(s, type) {
    tryCatch({
      suppressWarnings(if (type == "JKn") as_svrepdesign(s, type = type)
                       else as_svrepdesign(s, type = type, replicates = 20))
      "ok"
    }, error = function(e) class(e)[1])
  }
  m <- c(100, 40, 30, 15, 10, 5)
  registers <- function(a_size, form) {
    psus <- data.frame(psu = 1:6, m = m)
    units <- do.call(rbind, lapply(1:6, function(p) data.frame(
      psu = p, s2 = c(rep("A", if (p == 1) a_size else 3), rep("B", 3))
    )))
    units$id <- seq_len(nrow(units))
    units$y <- units$id %% 7 + 1
    if (form == "single") merge(psus, units) else list(psus, units)
  }
  design <- function(take) {
    d <- sampling_design() |> add_stage() |> cluster_by(psu) |>
      draw(n = 5, method = "pps_multinomial", mos = m, certainty_size = 60) |>
      add_stage() |> stratify_by(s2)
    switch(take,
           vector = draw(d, n = c(A = 1, B = 2)),
           table = draw(d, n = data.frame(s2 = c("A", "B"), n = c(1, 2))))
  }
  checked <- character(0)
  for (take in c("vector", "table")) for (form in c("single", "separate")) {
    for (a_size in c(1, 3)) {
      info <- paste(take, form, "A =", a_size)
      frame <- registers(a_size, form)
      v <- variance_estimators(design(take), frame)
      s <- suppressWarnings(execute(design(take), frame, seed = 1))
      expect_true(any(s$.certainty_1), info = info)
      for (type in names(labels)) {
        row <- v[v$estimator == labels[[type]], ]
        out <- exported(s, type)
        if (a_size == 3) {
          # One of three units of A in the certain PSU: a singleton.
          expect_identical(row$status, "refused", info = paste(info, type))
          expect_identical(row$decided_by, "frame", info = paste(info, type))
          expect_false(identical(out, "ok"), info = paste(info, type))
        } else {
          # A's one unit is taken whole.
          expect_identical(row$status, "supported", info = paste(info, type))
          expect_identical(out, "ok", info = paste(info, type))
        }
      }
      checked <- c(checked, info)
    }
  }
  expect_length(checked, 8L)

  # Two certain PSUs: one holds one area, whose units are all taken, and
  # one takes two areas and subsamples inside them. Only a pool of one
  # selected unit makes its PSU a stratum of one unit, so neither is one.
  areas <- function(p, n_areas, units_each) {
    out <- expand.grid(k = seq_len(units_each), a = seq_len(n_areas))
    data.frame(psu = p, ea = paste(p, out$a))
  }
  frame <- rbind(areas(1, 1, 2), areas(2, 2, 3),
                 do.call(rbind, lapply(3:7, areas, n_areas = 2,
                                       units_each = 3)))
  frame$m <- c(100, 100, 30, 15, 10, 5, 5)[frame$psu]
  frame$id <- seq_len(nrow(frame))
  frame$y <- frame$id %% 5 + 1
  mixed <- sampling_design() |> add_stage() |> cluster_by(psu) |>
    draw(n = 5, method = "pps_multinomial", mos = m, certainty_size = 60) |>
    add_stage() |> cluster_by(ea) |> draw(n = 2) |>
    add_stage() |> draw(n = 2)
  # Stage 3 samples inside PSU 2's areas, so the replicates read its stage
  # 2 as drawn with replacement, which the frame shows.
  v <- variance_estimators(mixed, frame)
  s <- suppressWarnings(execute(mixed, frame, seed = 1))
  expect_identical(sort(unique(s$psu[s$.certainty_1])), c(1, 2))
  for (type in names(labels)) {
    row <- v[v$estimator == labels[[type]], ]
    expect_identical(exported(s, type), "ok", info = type)
    if (type == "rwyb") {
      expect_identical(row$status, "supported", info = type)
    } else {
      expect_identical(row$status, "approximate", info = type)
      expect_identical(row$decided_by, "frame", info = type)
      expect_identical(row$class,
                       "samplyr_warning_replicate_certainty_stage_wr",
                       info = type)
      expect_warning(
        as_svrepdesign(s, type = type, replicates = 20),
        class = "samplyr_warning_replicate_certainty_stage_wr"
      )
    }
  }
})

test_that("a stratum of one unit taken whole is refused by the rescaled bootstrap", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # The pool is a census, so it is no singleton, but survey's rescaled
  # bootstrap still divides by n - 1 there. The report called it
  # approximate.
  frame <- data.frame(id = 1:17, h = rep(c("one", "many"), c(1, 16)),
                      y = (1:17) %% 5)
  designs <- list(
    census = sampling_design() |> stratify_by(h) |> draw(frac = 1),
    beside_sampled = sampling_design() |> stratify_by(h) |>
      draw(n = c(one = 1, many = 4))
  )
  for (nm in names(designs)) {
    report <- variance_estimators(designs[[nm]], frame)
    row <- report[report$estimator == "Rescaled bootstrap", ]
    expect_identical(row$status, "refused", info = nm)
    expect_identical(row$class, "samplyr_error_svrep_conversion_failed",
                     info = nm)
    expect_identical(ve_check(nm, designs[[nm]], frame, report = report),
                     character(0))
  }
})

test_that("an odd take inside a certainty PSU names that PSU's stratum", {
  skip_if_not_installed("survey")
  # Each certainty PSU is a half-sample stratum of its stage-2 units, so a
  # take of 3 below a census first stage cannot be paired.
  frame <- data.frame(id = 1:48, psu = rep(1:12, each = 4), y = 1:48)
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
    add_stage() |> draw(n = 3)
  v <- variance_estimators(design, frame)
  s <- execute(design, frame, seed = 1)
  for (type in c("BRR", "Fay")) {
    row <- v[v$call == sprintf("as_svrepdesign(x, type = \"%s\")", type), ]
    expect_identical(row$status, "refused")
    expect_identical(row$decided_by, "frame")
    expect_identical(row$stage, 2L)
    expect_match(row$note, "a stratum of its stage-2 units", fixed = TRUE)
    expect_error(as_svrepdesign(s, type = type),
                 class = "samplyr_error_svrep_conversion_failed")
  }
  # The design alone leaves it to the sample, as a risk.
  row <- variance_estimators(design)[5, ]
  expect_identical(row$status, "supported")
  expect_match(row$sample_risk, "Balanced half-samples", fixed = TRUE)
})

test_that("without a frame, a possible certainty PSU above a PPS stage 2 is a risk", {
  design <- ve_designs()$certainty_then_pps
  classes <- "samplyr_warning_replicate_certainty_stage_wr"
  report <- variance_estimators(design)
  for (type in c("JKn", "BRR", "Fay", "bootstrap", "subbootstrap")) {
    row <- report[report$call == sprintf('as_svrepdesign(x, type = "%s")',
                                          type), ]
    expect_false(grepl(classes, row$class %|% "", fixed = TRUE), info = type)
    expect_match(row$sample_risk, classes, fixed = TRUE, info = type)
    expect_match(row$sample_risk, "Pass `frame`", fixed = TRUE, info = type)
  }
  # The frame shows the certainty PSU, so the reading is stated.
  row <- variance_estimators(design, ve_frame())[4, ]
  expect_match(row$class, classes, fixed = TRUE)
  expect_identical(row$decided_by, "frame")
})

test_that("a certainty PSU taken whole below needs no half-sample pairing", {
  skip_if_not_installed("survey")
  # PSU 1 is certain and takes its three areas, each of two units, whole.
  # Sampling in the other PSUs' areas once cleared that exemption for
  # every certain PSU, and BRR was reported refused for an odd take of 3
  # that the export never pairs.
  areas <- function(n_cert) {
    do.call(rbind, lapply(1:7, function(p) {
      do.call(rbind, lapply(1:3, function(a) {
        n <- if (p == 1) n_cert else 4
        data.frame(psu = p, area = paste(p, a), id = paste(p, a, seq_len(n)),
                   m = if (p == 1) 100 else 10, y = seq_len(n) + p + a)
      }))
    }))
  }
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_brewer", mos = m, certainty_size = 60) |>
    add_stage() |> cluster_by(area) |> draw(n = 3) |>
    add_stage() |> draw(n = 2)
  for (type in c("BRR", "Fay")) {
    call <- sprintf('as_svrepdesign(x, type = "%s")', type)
    frame <- areas(2)
    row <- variance_estimators(design, frame)
    row <- row[row$call == call, ]
    s <- suppressWarnings(execute(design, frame, seed = 1))
    expect_identical(row$status, "approximate", info = type)
    expect_identical(row$class, "samplyr_warning_replicate_wr_first_stage")
    expect_warning(as_svrepdesign(s, type = type),
                   class = "samplyr_warning_replicate_wr_first_stage")
    # Sampled inside, the certain PSU's three areas are resampled and an odd
    # take of 3 cannot be paired.
    frame <- areas(3)
    row <- variance_estimators(design, frame)
    row <- row[row$call == call, ]
    s <- suppressWarnings(execute(design, frame, seed = 1))
    expect_identical(row$status, "refused", info = type)
    expect_identical(row$stage, 2L)
    expect_error(suppressWarnings(as_svrepdesign(s, type = type)),
                 class = "samplyr_error_svrep_conversion_failed")
  }
  # A stage 3 drawn with replacement leaves the pools below unresolved, so
  # the odd take that may need pairing is a risk the export realizes.
  wr <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_brewer", mos = m, certainty_size = 60) |>
    add_stage() |> cluster_by(area) |> draw(n = 3) |>
    add_stage() |> draw(n = 2, method = "srswr")
  frame <- areas(4)
  row <- variance_estimators(wr, frame)
  row <- row[row$call == 'as_svrepdesign(x, type = "BRR")', ]
  expect_false(identical(row$status, "refused"))
  expect_match(row$sample_risk, "Balanced half-samples", fixed = TRUE)
  s <- suppressWarnings(execute(wr, frame, seed = 1))
  expect_error(suppressWarnings(as_svrepdesign(s, type = "BRR")),
               class = "samplyr_error_svrep_conversion_failed")
})

test_that("a pool's units are read for sampling below them one by one", {
  # Stage 2 pools: A holds a unit taken whole below and one with no stage-3
  # pool, B one unit taken whole below and one sampled, C two sampled.
  view <- list(stages = list(
    NULL,
    list(units = data.frame(unit_id = 1:6,
                            pool_id = c("A", "A", "B", "B", "C", "C"))),
    list(pools = data.frame(pool_id = c("a1", "b1", "b2", "c1", "c2"),
                            parent_unit = c(1, 3, 4, 5, 6),
                            n_target = c(2, 2, 2, 2, 2),
                            N = c(2, 2, 4, 4, 4)))
  ))
  traits <- rep(list(list(kind = "equal_wor", random_size = FALSE)), 3)
  below <- function(census) {
    samplyr:::pool_samples_below(view, traits, from = 2,
                                 pool_ids = c("A", "B", "C"), census = census)
  }
  # B is settled only when every unit is selected, so its census reads TRUE.
  expect_identical(below(c(FALSE, FALSE, FALSE)), c(FALSE, NA, TRUE))
  expect_identical(below(c(TRUE, TRUE, TRUE)), c(FALSE, TRUE, TRUE))
  # A stage drawn with replacement leaves every pool with a unit above it
  # unsettled, a take of 2 from 2 included.
  traits[[3]]$kind <- "wr"
  expect_identical(below(c(TRUE, TRUE, TRUE)), c(NA, NA, NA))
})

test_that("a stage-2 stratum mixing units sampled and taken whole below is a risk", {
  # Two of four areas are drawn in each certain PSU, two of them taking
  # their units whole and two sampling them. Whether a drawn pair holds an
  # area sampled below depends on the sample.
  frame <- do.call(rbind, lapply(1:6, function(p) {
    do.call(rbind, lapply(1:4, function(a) {
      n <- if (a <= 2) 2 else 4
      data.frame(psu = p, area = paste(p, a), id = paste(p, a, seq_len(n)),
                 y = a + p)
    }))
  }))
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(frac = 1) |>
    add_stage() |> cluster_by(area) |> draw(n = 2) |>
    add_stage() |> draw(n = 2)
  row <- variance_estimators(design, frame)[4, ]
  expect_identical(row$status, "supported")
  expect_match(row$sample_risk,
               "samplyr_warning_replicate_certainty_stage_wr", fixed = TRUE)
  # With every area sampled below, the frame settles it.
  sampled <- frame[!duplicated(frame$area), ]
  sampled <- sampled[rep(seq_len(nrow(sampled)), each = 4), ]
  sampled$id <- seq_len(nrow(sampled))
  row <- variance_estimators(design, sampled)[4, ]
  expect_identical(row$status, "approximate")
  expect_identical(row$decided_by, "frame")
})

test_that("an allocated first stage drawn with replacement still reads its certain PSUs' pools", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # PSUs 1 and 5 are certain and keep one unit below them, a singleton the
  # frame shows. The census view of stage 1 kept the allocation, which a
  # fraction refuses, so the pools went unread and both rows fell back to
  # a risk decided by the design.
  psus <- data.frame(psu = 1:8, st = rep(c("A", "B"), each = 4),
                     m = c(100, 40, 30, 15, 90, 40, 30, 15))
  units <- do.call(rbind, lapply(1:8, function(p) {
    data.frame(psu = p, id = paste(p, 1:3), y = 1:3 + p)
  }))
  frame <- merge(psus, units)
  by_stratum <- function(...) data.frame(st = c("A", "B"), ...)
  strata <- list(
    none = function(d) stratify_by(d, st),
    proportional = function(d) stratify_by(d, st, alloc = "proportional"),
    equal = function(d) stratify_by(d, st, alloc = "equal"),
    neyman = function(d) {
      stratify_by(d, st, alloc = "neyman", variance = by_stratum(var = c(4, 9)))
    },
    optimal = function(d) {
      stratify_by(d, st, alloc = "optimal",
                  variance = by_stratum(var = c(4, 9)),
                  cost = by_stratum(cost = c(1, 2)))
    },
    importance = function(d) {
      stratify_by(d, st, alloc = "proportional",
                  importance = by_stratum(importance = c(3, 5)))
    }
  )
  labels <- c(JKn = "Stratified jackknife (JKn)",
              rwyb = "Rao-Wu-Yue-Beaumont bootstrap")
  for (alloc in names(strata)) {
    design <- sampling_design() |> add_stage() |> strata[[alloc]]() |>
      cluster_by(psu) |>
      draw(n = if (alloc == "none") 3 else 6, method = "pps_multinomial",
           mos = m, certainty_size = 60) |>
      add_stage() |> draw(n = 1)
    report <- variance_estimators(design, frame)
    s <- suppressMessages(suppressWarnings(execute(design, frame, seed = 1)))
    expect_true(any(s$.certainty_1), info = alloc)
    for (type in names(labels)) {
      row <- report[report$estimator == labels[[type]], ]
      expect_identical(row$status, "refused", info = paste(alloc, type))
      expect_identical(row$decided_by, "frame", info = paste(alloc, type))
      expect_error(suppressWarnings(
        if (type == "rwyb") as_svrepdesign(s, type = type, replicates = 20)
        else as_svrepdesign(s, type = type)
      ), info = paste(alloc, type))
    }
  }
})

test_that("the pools of a certain PSU drawn with replacement are read from the frame", {
  skip_if_not_installed("survey")
  # Stage 1 takes PSU 1 with certainty and draws two other hits. Its
  # stage-2 pools were left unread below the draw with replacement, so
  # what the frame fixes there was reported as a risk.
  psus <- data.frame(psu = 1:6, m = c(100, 40, 30, 15, 10, 5))
  units <- function(first) {
    do.call(rbind, lapply(1:6, function(p) {
      n <- if (p == 1) first else 6
      data.frame(psu = p, id = paste(p, seq_len(n)), x = seq_len(n),
                 y = seq_len(n) + p)
    }))
  }
  stage1 <- sampling_design() |> add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = m, certainty_size = 50) |>
    add_stage()
  row_of <- function(design, frame, type) {
    r <- variance_estimators(design, frame)
    r[r$call == sprintf('as_svrepdesign(x, type = "%s")', type), ]
  }
  outcome <- function(design, frame, type) {
    s <- suppressMessages(suppressWarnings(execute(design, frame, seed = 1)))
    expect_true(any(s$.certainty_1))
    tryCatch({
      suppressWarnings(as_svrepdesign(s, type = type))
      "ok"
    }, error = function(e) class(e)[1])
  }
  frame <- merge(psus, units(6))
  # An odd take outside certainty: BRR refuses it.
  odd <- stage1 |> draw(n = 3)
  for (type in c("BRR", "Fay")) {
    row <- row_of(odd, frame, type)
    expect_identical(row$status, "refused", info = type)
    expect_identical(row$decided_by, "frame", info = type)
    expect_identical(outcome(odd, frame, type),
                     "samplyr_error_svrep_conversion_failed")
  }
  # A PPS stage 2 that does not take its pool whole is read as drawn with
  # replacement inside the certain PSU.
  pps <- stage1 |> draw(n = 2, method = "pps_brewer", mos = x)
  for (type in c("JKn", "BRR")) {
    row <- row_of(pps, frame, type)
    expect_identical(row$status, "approximate", info = type)
    expect_identical(row$decided_by, "frame", info = type)
    expect_identical(row$class, "samplyr_warning_replicate_certainty_stage_wr")
  }
  # PSU 1 holds three units, all taken, with nothing below: no pairing.
  whole <- merge(psus, units(3))
  row <- row_of(odd, whole, "BRR")
  expect_identical(row$status, "supported")
  expect_identical(outcome(odd, whole, "BRR"), "ok")
  # A stage 3 drawn with replacement leaves the descendants unsettled.
  areas <- do.call(rbind, lapply(1:6, function(p) {
    do.call(rbind, lapply(seq_len(if (p == 1) 3 else 6), function(a) {
      data.frame(psu = p, area = paste(p, a), id = paste(p, a, 1:3),
                 y = 1:3 + p + a)
    }))
  }))
  three <- merge(psus, areas)
  wr_below <- sampling_design() |> add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = m, certainty_size = 50) |>
    add_stage() |> cluster_by(area) |> draw(n = 3) |>
    add_stage() |> draw(n = 2, method = "srswr")
  row <- row_of(wr_below, three, "BRR")
  expect_false(identical(row$status, "refused"))
  expect_match(row$sample_risk, "Balanced half-samples", fixed = TRUE)
  expect_identical(outcome(wr_below, three, "BRR"),
                   "samplyr_error_svrep_conversion_failed")
})

test_that("a composed phase 2 reads phase 1's own draw of one unit", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # Phase 2 drawn inside the one PSU phase 1 drew from four is exported as
  # one phase whose stage 1 holds that unit. The report read phase 1's rows
  # as the frame, where the PSU looked like a census, and called the
  # exports supported while they refused it.
  frame <- data.frame(psu = rep(1:4, each = 5), id = 1:20, y = 1:20)
  first <- function(n) {
    sampling_design() |> add_stage() |> cluster_by(psu) |> draw(n = n) |>
      add_stage() |> draw(n = 4)
  }
  within <- sampling_design() |> stratify_by(psu) |> draw(n = 2)
  outcome <- function(s, key) {
    tryCatch({
      suppressWarnings(switch(
        key,
        linearization = as_svydesign(s),
        rwyb = ,
        bootstrap = ,
        mrbbootstrap = as_svrepdesign(s, type = key, replicates = 10),
        as_svrepdesign(s, type = key)
      ))
      "ok"
    }, error = function(e) class(e)[1])
  }
  keys <- c(linearization = "Taylor linearization", JK1 = "Jackknife (JK1)",
            JKn = "Stratified jackknife (JKn)", bootstrap = "Bootstrap",
            mrbbootstrap = "Multistage rescaled bootstrap",
            rwyb = "Rao-Wu-Yue-Beaumont bootstrap")
  phase2 <- execute(within, execute(first(1), frame, seed = 1), seed = 2)
  report <- variance_estimators(phase2)
  for (key in names(keys)) {
    row <- report[report$estimator == keys[[key]], ]
    expected <- if (key == "rwyb") "samplyr_error_rwyb_singleton" else
      "samplyr_error_export_one_unit"
    expect_identical(row$status, "refused", info = key)
    expect_identical(row$class, expected, info = key)
    expect_identical(row$decided_by, "frame", info = key)
    expect_identical(outcome(phase2, key), expected, info = key)
  }
  # Two PSUs in phase 1, and a census of a frame's only PSU, export.
  phase2 <- execute(within, execute(first(2), frame, seed = 1), seed = 2)
  row <- variance_estimators(phase2)[1, ]
  expect_false(identical(row$status, "refused"))
  expect_identical(outcome(phase2, "linearization"), "ok")
  single <- data.frame(psu = 1, id = 1:12, y = 1:12)
  whole <- sampling_design() |> add_stage() |> cluster_by(psu) |>
    draw(frac = 1) |> add_stage() |> draw(n = 8)
  phase2 <- execute(sampling_design() |> draw(n = 4),
                    execute(whole, single, seed = 1), seed = 2)
  report <- variance_estimators(phase2)
  for (key in c("linearization", "JKn", "rwyb")) {
    expect_identical(report$status[report$estimator == keys[[key]]],
                     "supported", info = key)
    expect_identical(outcome(phase2, key), "ok", info = key)
  }
})

test_that("a two-phase phase 2 held in one primary unit is refused with a class", {
  skip_if_not_installed("survey")
  # survey builds the phase-2 design, and phase 1's design on phase 2's
  # rows. A phase 2 of one unit, or one drawn across the phase-1 PSUs that
  # fell in one of them, hit survey's unclassed "only one primary sampling
  # unit" while the report called linearization approximate.
  frame <- data.frame(psu = rep(1:4, each = 5), id = 1:20, y = 1:20)
  phase1 <- execute(sampling_design() |> add_stage() |> cluster_by(psu) |>
                      draw(n = 2) |> add_stage() |> draw(n = 4),
                    frame, seed = 1)
  linearization <- function(report) {
    report[report$estimator == "Taylor linearization", ]
  }
  refused <- function(phase2, pattern) {
    row <- linearization(variance_estimators(phase2))
    expect_identical(row$status, "refused")
    expect_identical(row$class, "samplyr_error_export_one_unit")
    err <- expect_error(suppressWarnings(as_svydesign(phase2)),
                        class = "samplyr_error_export_one_unit")
    expect_match(conditionMessage(err), pattern, fixed = TRUE)
  }
  one <- sampling_design() |> draw(n = 1)
  row <- linearization(variance_estimators(one, phase1))
  expect_identical(row$status, "refused")
  expect_identical(row$decided_by, "design")
  refused(execute(one, phase1, seed = 1), "Phase 2 holds a single primary unit")

  three <- sampling_design() |> draw(n = 3)
  row <- linearization(variance_estimators(three, phase1))
  expect_match(row$sample_risk, "samplyr_error_export_one_unit", fixed = TRUE)
  landed <- vapply(1:12, function(seed) {
    length(unique(execute(three, phase1, seed = seed)$psu))
  }, integer(1))
  expect_true(any(landed == 1L) && any(landed == 2L))
  refused(execute(three, phase1, seed = which(landed == 1L)[1]),
          "fall in a single phase-1 primary unit")
  spread <- execute(three, phase1, seed = which(landed == 2L)[1])
  expect_false(identical(linearization(variance_estimators(spread))$status,
                         "refused"))
  expect_s3_class(suppressWarnings(as_svydesign(spread)), "twophase2")
  # A draw larger than any phase-1 PSU cannot fall in one: no risk.
  row <- linearization(variance_estimators(sampling_design() |> draw(n = 6),
                                           phase1))
  expect_false(grepl("samplyr_error_export_one_unit", row$sample_risk %|% ""))
})

test_that("a single hit with replacement is no certainty for the one-unit refusal", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # Phase 1 hits a frame's only PSU once with replacement, so its stage
  # weight is 1, an expected hit, with no certainty recorded. The report read
  # that weight as a census of the PSU and called a composed phase 2
  # supported, while the export refuses its single primary unit.
  one_psu <- data.frame(psu = 1, id = 1:12, m = 5, y = 1:12)
  four_psus <- data.frame(psu = rep(1:4, each = 5), id = 1:20, m = 5,
                          y = 1:20)
  within <- sampling_design() |> draw(n = 4)
  first <- function(...) {
    sampling_design() |> add_stage() |> cluster_by(psu) |> draw(...) |>
      add_stage() |> draw(n = 4)
  }
  outcome <- function(s, type) {
    tryCatch({
      suppressWarnings(if (type == "linearization") as_svydesign(s) else
        if (type == "rwyb") as_svrepdesign(s, type = type, replicates = 10)
        else as_svrepdesign(s, type = type))
      "ok"
    }, error = function(e) class(e)[1])
  }
  keys <- c(linearization = "Taylor linearization", JK1 = "Jackknife (JK1)",
            JKn = "Stratified jackknife (JKn)",
            rwyb = "Rao-Wu-Yue-Beaumont bootstrap")
  check <- function(label, design1, frame, expected) {
    phase1 <- suppressWarnings(execute(design1, frame, seed = 1))
    phase2 <- execute(within, phase1, seed = 2)
    by_sample <- variance_estimators(phase2)
    by_design <- variance_estimators(within, phase1)
    for (type in names(expected)) {
      row <- by_sample[by_sample$estimator == keys[[type]], ]
      info <- paste(label, type)
      expect_identical(row$status, by_design$status[
        by_design$estimator == keys[[type]]], info = info)
      expect_identical(outcome(phase2, type), expected[[type]], info = info)
      if (expected[[type]] == "ok") {
        expect_false(identical(row$status, "refused"), info = info)
      } else {
        expect_identical(row$status, "refused", info = info)
        expect_identical(row$class, expected[[type]], info = info)
        if (expected[[type]] %in% refused) {
          expect_identical(row$decided_by, "frame", info = info)
        }
      }
    }
  }
  refused <- c(linearization = "samplyr_error_export_one_unit",
               rwyb = "samplyr_error_rwyb_singleton")
  # One hit, weight 1, no recorded certainty: refused as the export refuses.
  check("srswr one hit", first(n = 1, method = "srswr"), one_psu, refused)
  check("multinomial one hit",
        first(n = 1, method = "pps_multinomial", mos = m), one_psu, refused)
  # One hit with a weight above 1 keeps its refusal.
  check("srswr one of four", first(n = 1, method = "srswr"), four_psus,
        refused)
  # A census of the only PSU, and a recorded certainty, export. The census
  # makes the first stage stratified, which JK1 refuses.
  check("srs census", first(frac = 1), one_psu,
        c(linearization = "ok", JKn = "ok", rwyb = "ok",
          JK1 = "samplyr_error_svrep_conversion_failed"))
  check("recorded certainty",
        first(n = 1, method = "pps_multinomial", mos = m, certainty_size = 1),
        one_psu, c(linearization = "ok", JKn = "ok"))
  # Two hits of the one PSU are two draws, not one unit. A phase 2 that pools
  # them keeps the two-phase route and its own refusal of a phase 1 drawn
  # with replacement above a later stage.
  check("srswr two hits", first(n = 2, method = "srswr"), one_psu,
        c(linearization = "samplyr_error_twophase_stage_probs"))
  # A genuine two-phase sample whose phase 1 is a census of the only PSU
  # keeps its own refusal and its certainty wording.
  frame <- data.frame(psu = 1, id = 1:12, g = rep(c("a", "b"), 6), y = 1:12)
  phase1 <- execute(first(frac = 1), frame, seed = 1)
  phase2 <- execute(sampling_design() |> stratify_by(g) |> draw(n = 2),
                    phase1, seed = 2)
  row <- variance_estimators(phase2)[1, ]
  expect_identical(row$class, "samplyr_error_export_one_unit")
  expect_match(row$note, "taken with certainty", fixed = TRUE)
  expect_error(as_svydesign(phase2), class = "samplyr_error_export_one_unit")
})
