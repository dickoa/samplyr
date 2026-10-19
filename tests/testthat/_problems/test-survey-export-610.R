# Extracted from test-survey-export.R:610

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "samplyr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
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
      as_svrepdesign(phase2, type = "JK1", systematic_variance = "error"),
      "svyrep.design"
    )
    expect_identical(variance_estimators(design, phase1)$status[1],
                     "supported")
  }
