# Extracted from test-tenures.R:188

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
used <- unlist(lapply(
    attr(master, "metadata")$per_occasion_draws, function(d) d$rows$hh
  ))
cut <- tenure_listing(master)
expect_error(
    execute(master, cut[!cut$hh %in% used, ], period = 7, seed = 17),
    class = "samplyr_error_cohort_listing"
  )
first_used <- tapply(used, cut$ea[match(used, cut$hh)], min)
trimmed <- cut[!cut$hh %in% first_used, ]
returning <- unique(master$ea[master$.panel == 1L])
expect_warning(
    partial <- execute(master, trimmed, period = 7, seed = 17),
    paste(length(returning), "units that earlier tenures drew, in",
          length(returning), "primary units"),
    class = "samplyr_warning_cohort_listing"
  )
seventh <- execute(partial, period = 7)
expect_false(any(seventh$hh %in% used))
expect_no_warning(draw_tenures(master, 7))
master <- draw_tenures(master, 7)
seventh <- execute(master, period = 7)
first <- execute(master, period = 1)
expect_identical(unique(seventh$.visit), 1L)
expect_identical(sort(unique(seventh$ea)), sort(unique(first$ea)))
expect_false(any(seventh$hh %in% first$hh))
expect_identical(sort(execute(master, period = 10)$hh), sort(seventh$hh))
err <- tryCatch(execute(master, period = 8), error = identity)
expect_s3_class(err, "samplyr_error_occasion_incomplete_master")
