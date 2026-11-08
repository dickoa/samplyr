# Extracted from test-occasion-windows.R:192

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
skip_if_not_installed("survey")
master <- window_master(n = 20, schedule = overlap_schedule(), seed = 2)
window <- execute(master, period = 1:2)
svy <- as_svydesign(window)
expect_s3_class(svy, "survey.design2")
reference <- survey::svydesign(
    ids = ~.sample_id,
    strata = ~ interaction(h, visits),
    weights = ~.weight,
    nest = TRUE,
    data = with_visits(window, ".sample_id")
  )
n_units <- length(unique(window$.sample_id))
expect_identical(survey::degf(svy), n_units - 6L)
expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy))),
    unname(survey::SE(survey::svytotal(~y, reference)))
  )
expect_s3_class(as_svrepdesign(window), "svyrep.design")
expect_error(
    as_svydesign(window, method = "full"),
    class = "samplyr_error_survey_argument"
  )
