# Extracted from test-tenures.R:523

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
skip_if_not_installed("survey")
master <- draw_tenures(tenure_master(), 1:3)
fourth <- execute(master, period = 4)
expect_true(attr(fourth, "metadata")[["occasion"]]$provisional)
svy <- as_svydesign(fourth)
reference <- survey::svydesign(
    ids = ~ea,
    strata = ~h,
    weights = ~.weight,
    nest = TRUE,
    data = as.data.frame(fourth)
  )
expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, reference)))
  )
expect_s3_class(as_svrepdesign(fourth), "svyrep.design")
expect_error(as_svydesign(fourth, method = "full"),
               class = "samplyr_error_occasion_provisional")
expect_error(stack_periods(fourth, execute(master, period = 1)),
               class = "samplyr_error_occasion_provisional")
