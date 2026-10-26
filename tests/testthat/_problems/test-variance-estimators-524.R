# Extracted from test-variance-estimators.R:524

# prequel ----------------------------------------------------------------------
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
ve_outcome <- function(key, sample, frame, twophase = FALSE,
                       joint_source = sample) {
  pps <- if (twophase) {
    "brewer"
  } else if (identical(key, "joint")) {
    joint <- suppressWarnings(joint_expectation(joint_source, frame))
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
ve_allowed <- function(row) {
  text <- paste(row$class, row$sample_risk)
  regmatches(text, gregexpr("samplyr_(error|warning)_[a-z0-9_]+", text))[[1]]
}
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

# test -------------------------------------------------------------------------
frame <- data.frame(id = 1:600, h = rep(c("a", "b", "c"), each = 200))
schedule <- expand.grid(panel = 1:6, wave = 1:3)
schedule$active <- (schedule$wave - 1) %% 3 == (schedule$panel - 1) %% 3
master <- sampling_design() |>
    stratify_by(h) |>
    draw(n = 15) |>
    execute(frame, seed = 1, panels = schedule)
expect_no_error(variance_estimators(master))
for (wave in list(1, 1:2)) {
    w <- execute(master, wave = wave)
    expect_error(variance_estimators(w),
                 class = "samplyr_error_variance_estimators_input")
    expect_error(
      variance_estimators(sampling_design() |> draw(n = 3), frame = w),
      class = "samplyr_error_variance_estimators_input"
    )
  }
