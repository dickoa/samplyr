# Extracted from test-tenures.R:897

# prequel ----------------------------------------------------------------------
tenure_eas <- function() {
  data.frame(
    ea = 1:60,
    h = rep(c("a", "b"), each = 30),
    size = rep(c(20, 24, 28, 32, 36, 40), times = 10)
  )
}
tenure_households <- function() {
  eas <- tenure_eas()
  hh <- data.frame(ea = rep(eas$ea, eas$size))
  hh$hh <- seq_len(nrow(hh))
  hh$y <- (hh$hh %% 11) + hh$ea / 10
  hh
}
tenure_schedule <- function() {
  schedule <- expand.grid(panel = 1:3, period = 1:12)
  schedule$active <- (schedule$period - 1) %% 3 == (schedule$panel - 1) %% 3
  quarter <- (schedule$period - 1) %/% 3 + 1
  schedule$visit <- ifelse(schedule$active, (quarter - 1) %% 2 + 1, NA)
  schedule
}
tenure_design <- function() {
  sampling_design() |>
    add_stage() |>
    stratify_by(h) |>
    cluster_by(ea) |>
    draw(n = 9) |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 4)
}
tenure_master <- function(schedule = tenure_schedule()) {
  execute(tenure_design(), tenure_eas(), stages = 1, seed = 1,
          panels = schedule)
}
tenure_listing <- function(master) {
  hh <- tenure_households()
  hh[hh$ea %in% master$ea, ]
}
draw_tenures <- function(master, waves) {
  for (t in waves) {
    master <- execute(master, tenure_listing(master), wave = t, seed = 10 + t)
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
one_per_ea <- function(master, wave) {
  rows <- attr(master, "metadata")$per_wave_draws
  rows <- rows[[which(vapply(rows, `[[`, 1, "wave") == wave)]]$rows
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
two_panels <- function(waves) {
  schedule <- expand.grid(panel = 1:2, period = waves)
  schedule$active <- schedule$panel == (schedule$period - 1) %% 2 + 1
  schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  schedule
}

# test -------------------------------------------------------------------------
hh <- missing_households()
master <- missing_master(two_panels(1:6))
master <- execute(master, hh, wave = 1, seed = 2)
master <- execute(master, hh, wave = 5, seed = 3)
older <- hh[!hh$hh %in% one_per_ea(master, 1), ]
expect_length(
    listing_warnings(execute(master, older, wave = 3, seed = 4))$messages,
    0L
  )
latest <- hh[!hh$hh %in% one_per_ea(master, 5), ]
expect_length(
    listing_warnings(execute(master, latest, wave = 3, seed = 4))$messages,
    1L
  )
schedule <- expand.grid(panel = 1:3, period = 1:3)
schedule$active <- with(schedule, (wave == 1 & panel <= 2) |
                            (wave == 2 & panel == 3) |
                            (wave == 3 & panel != 2))
