# Extracted from test-cohorts.R:826

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
missing_design <- function(on_empty = "error") {
  sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(ea) |>
    draw(n = 6) |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 2, on_empty = on_empty)
}
missing_master <- function(schedule) {
  eas <- data.frame(ea = 1:12, region = rep(c("A", "B"), each = 6))
  execute(missing_design(), eas, stages = 1, panels = schedule, seed = 1,
          frame_digest = "none")
}
missing_households <- function() {
  data.frame(ea = rep(1:12, each = 10), hh = 1:120)
}
one_per_ea <- function(master, occasion) {
  rows <- attr(master, "metadata")$per_occasion_draws
  rows <- rows[[which(vapply(rows, `[[`, 1, "occasion") == occasion)]]$rows
  unname(tapply(rows$hh, rows$ea, min))
}
listing_warnings <- function(expr) {
  messages <- character(0)
  value <- withCallingHandlers(
    expr,
    samplyr_warning_cohort_listing = function(w) {
      messages <<- c(messages, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(value = value, messages = messages)
}
two_panels <- function(occasions) {
  schedule <- expand.grid(panel = 1:2, period = occasions)
  schedule$active <- schedule$panel == (schedule$period - 1) %% 2 + 1
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  schedule
}

# test -------------------------------------------------------------------------
hh <- missing_households()
master <- missing_master(two_panels(1:7))
master <- execute(master, hh, period = 1, seed = 2)
master <- execute(master, hh, period = 2, seed = 3)
gone <- one_per_ea(master, 1)
listing <- hh[!hh$hh %in% gone, ]
third <- listing_warnings(execute(master, listing, period = 3, seed = 4))
expect_length(third$messages, 1L)
expect_match(third$messages, "lacks 6 units that earlier tenures drew, in 6",
               fixed = TRUE)
