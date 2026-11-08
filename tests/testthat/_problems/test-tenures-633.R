# Extracted from test-tenures.R:633

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

# test -------------------------------------------------------------------------
master <- draw_tenures(tenure_master(), c(1:3, 7))
digest <- get_frame_digest(master)
expect_identical(digest$version, 3L)
expect_length(digest$stages, 2L)
draws <- attr(master, "metadata")$per_wave_draws
pools <- digest$stages[[2]]$pools
for (d in draws) {
    here <- pools[pools$tenure == d$wave, ]
    expect_identical(nrow(here), length(unique(d$rows$ea)), info = d$wave)
    expect_identical(sum(here$n_realized), nrow(d$rows), info = d$wave)
  }
stage <- frame_summary(master, detail = "stage")
expect_identical(stage$tenure, c(NA, 1L, 2L, 3L, 7L))
expect_identical(
    stage$n_realized[-1],
    vapply(draws, function(d) as.double(nrow(d$rows)), numeric(1))
  )
expect_true("tenure" %in% names(frame_summary(master, detail = "pool")))
one <- tenure_master(tenure_schedule()[c("panel", "wave", "active")])
