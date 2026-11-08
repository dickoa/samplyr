# Extracted from test-cohorts.R:626

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "samplyr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
cohort_eas <- function() {
  data.frame(
    ea = 1:60,
    h = rep(c("a", "b"), each = 30),
    size = rep(c(20, 24, 28, 32, 36, 40), times = 10)
  )
}
cohort_households <- function() {
  eas <- cohort_eas()
  hh <- data.frame(ea = rep(eas$ea, eas$size))
  hh$hh <- seq_len(nrow(hh))
  hh$y <- (hh$hh %% 11) + hh$ea / 10
  hh
}
cohort_schedule <- function() {
  schedule <- expand.grid(panel = 1:3, period = 1:12)
  schedule$active <- (schedule$period - 1) %% 3 == (schedule$panel - 1) %% 3
  quarter <- (schedule$period - 1) %/% 3 + 1
  schedule$visit <- ifelse(schedule$active, (quarter - 1) %% 2 + 1, NA)
  schedule
}
cohort_design <- function() {
  sampling_design() |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(ea) |>
    draw(n = 9) |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 4)
}
cohort_master <- function(schedule = cohort_schedule()) {
  execute(cohort_design(), cohort_eas(), stages = 1, seed = 1,
          panels = schedule)
}
cohort_listing <- function(master) {
  hh <- cohort_households()
  hh[hh$ea %in% master$ea, ]
}
draw_cohorts <- function(master, occasions) {
  for (t in occasions) {
    master <- execute(master, cohort_listing(master), period = t, seed = 10 + t)
  }
  master
}
with_cohort <- function(schedule) {
  schedule$cohort <- NA_integer_
  for (p in unique(schedule$panel)) {
    rows <- which(schedule$panel == p & schedule$active)
    rows <- rows[order(schedule$period[rows])]
    schedule$cohort[rows] <- 10L * p + cumsum(schedule$visit[rows] == 1)
  }
  schedule
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
master <- draw_cohorts(cohort_master(), 1:3)
quarter <- execute(master, period = 1:3)
expect_identical(attr(quarter, "metadata")$occasion$window, "union")
