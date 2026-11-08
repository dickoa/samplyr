# Extracted from test-tenures.R:134

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
draw_tenures <- function(master, occasions) {
  for (t in occasions) {
    master <- execute(master, tenure_listing(master), period = t, seed = 10 + t)
  }
  master
}

# test -------------------------------------------------------------------------
master <- draw_tenures(tenure_master(), 1:3)
draws <- attr(master, "metadata")$per_occasion_draws
expect_identical(
    lapply(draws, function(d) sort(unique(d$rows$.panel))),
    list(1L, 2L, 3L)
  )
expect_identical(remaining_stages(master), 2L)
err <- tryCatch(
    execute(master, tenure_listing(master), period = 4, seed = 14),
    error = identity
  )
expect_s3_class(err, "samplyr_error_occasion_already_drawn")
expect_match(cli::ansi_strip(conditionMessage(err)), "drawn at wave 1")
