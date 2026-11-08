# Extracted from test-occasion-windows.R:254

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "samplyr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
window_frame <- function() {
  data.frame(
    id = 1:600,
    h = rep(c("a", "b", "c"), each = 200),
    y = rep(c(10, 20, 30), each = 200) + (1:600) %% 7
  )
}
monthly_schedule <- function() {
  schedule <- expand.grid(panel = 1:6, period = 1:3)
  schedule$active <- (schedule$period - 1) %% 3 == (schedule$panel - 1) %% 3
  schedule
}
overlap_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    period = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
}
window_master <- function(n = 15, schedule = monthly_schedule(), seed = 1) {
  sampling_design() |>
    stratify_by(h) |>
    draw(n = n) |>
    execute(window_frame(), seed = seed, panels = schedule)
}
with_occasions <- function(window, unit) {
  df <- as.data.frame(window)
  at <- paste(df$.period, df$.subround)
  occasions <- tapply(at, df[[unit]], function(w) {
    paste(sort(unique(w)), collapse = ", ")
  })
  df$occasions <- as.vector(occasions[as.character(df[[unit]])])
  df
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
window <- execute(window_master(n = 15), period = 1:2)
window$y <- 1
svy <- as_svydesign(window)
expect_equal(sum(window$.weight), 600)
expect_equal(unname(survey::SE(survey::svytotal(~y, svy))), 0)
