# Extracted from test-occasion-windows.R:85

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
union_chance <- function(master, panels) {
  record <- attr(master, "metadata")$panel_assignment
  df <- as.data.frame(master)
  keys <- do.call(paste, c(df[record$key_vars], sep = "\r"))
  chance <- rep(NA_real_, nrow(df))
  for (pool in record$pools) {
    pool_keys <- vapply(pool$keys, identity, character(1))
    at <- match(keys, pool_keys)
    rows <- which(!is.na(at))
    block <- rep(seq_along(pool$blocks), pool$blocks)[at[rows]]
    take <- rowSums(pool$quotas[, panels, drop = FALSE])
    chance[rows] <- take[block] / pool$blocks[block]
  }
  chance
}
with_visits <- function(window, unit) {
  df <- as.data.frame(window)
  visits <- tapply(df$.period, df[[unit]], function(w) length(unique(w)))
  df$visits <- as.vector(visits[as.character(df[[unit]])])
  df
}

# test -------------------------------------------------------------------------
master <- window_master(n = 15)
window <- execute(master, period = 1:2)
union <- c(1L, 2L, 4L, 5L)
expect_s3_class(window, "tbl_sample")
expect_identical(attr(window, "metadata")$occasion$window, "union")
expect_identical(attr(window, "metadata")$occasion$occasion, 1:2)
in_union <- master$.panel %in% union
expect_setequal(window$.sample_id, master$.sample_id[in_union])
expect_false(anyDuplicated(window$.sample_id) > 0L)
at <- match(window$.sample_id, master$.sample_id)
expect_identical(window$.period, (master$.panel[at] - 1L) %% 3L + 1L)
expect_identical(unique(window$.subround), 1L)
chance <- union_chance(master, union)
expect_equal(window$.weight, master$.weight[at] / chance[at])
