# Extracted from test-tenures.R:736

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
    master <- execute(master, tenure_listing(master), period = t, seed = 10 + t)
  }
  master
}

# test -------------------------------------------------------------------------
master <- draw_tenures(tenure_master(), 1:3)
path <- withr::local_tempfile(fileext = ".json")
frames <- list(tenure_eas(), tenure_households())
record <- attr(master, "metadata")$panel_assignment
master_out <- tenure_master()
listings <- list()
for (t in 1:3) {
    listings[[t]] <- tenure_listing(master_out)
    master_out <- execute(master_out, listings[[t]], period = t, seed = 10 + t)
  }
write_design(master_out, path, frame = c(list(tenure_eas()), listings))
replayed <- replay_design(read_design(path), c(list(tenure_eas()), listings))
