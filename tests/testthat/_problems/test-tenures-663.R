# Extracted from test-tenures.R:663

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
  schedule <- expand.grid(panel = 1:3, wave = 1:12)
  schedule$active <- (schedule$wave - 1) %% 3 == (schedule$panel - 1) %% 3
  quarter <- (schedule$wave - 1) %/% 3 + 1
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
eas <- data.frame(ea = 1:12, region = rep(c("A", "B"), each = 6))
hh <- expand.grid(ea = 1:12, type = c("a", "b"), hh = 1:10,
                    stringsAsFactors = FALSE)
schedule <- expand.grid(panel = 1:2, wave = 1:4)
schedule$active <- schedule$panel == (schedule$wave - 1) %% 2 + 1
schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
design <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(ea) |> draw(n = 4) |>
    add_stage() |> stratify_by(type) |> cluster_by(hh) |> draw(n = 2)
master <- execute(design, eas, stages = 1, panels = schedule, seed = 1,
                    frame_digest = "full")
listings <- list()
for (t in 1:4) {
    listings[[t]] <- hh[hh$ea %in% master$ea, ]
    master <- execute(master, listings[[t]], wave = t, seed = t + 5)
  }
pools <- get_frame_digest(master)$stages[[2]]$pools
first <- pools$tenure %in% 1:2
expect_identical(pools$n_excluded[first], rep(0L, sum(first)))
expect_identical(pools$n_excluded[!first], rep(2L, sum(!first)))
expect_identical(pools$N + pools$n_excluded, rep(10L, nrow(pools)))
stage <- frame_summary(master, detail = "stage")
expect_identical(stage$n_excluded, c(NA, 0, 0, 16, 16))
wave <- execute(master, wave = 3)
units <- frame_summary(wave, detail = "unit", stages = 2)
