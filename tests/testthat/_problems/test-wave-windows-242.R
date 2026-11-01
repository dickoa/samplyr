# Extracted from test-wave-windows.R:242

# prequel ----------------------------------------------------------------------
window_frame <- function() {
  data.frame(
    id = 1:600,
    h = rep(c("a", "b", "c"), each = 200),
    y = rep(c(10, 20, 30), each = 200) + (1:600) %% 7
  )
}
monthly_schedule <- function() {
  schedule <- expand.grid(panel = 1:6, wave = 1:3)
  schedule$active <- (schedule$wave - 1) %% 3 == (schedule$panel - 1) %% 3
  schedule
}
overlap_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    wave = rep(1:4, each = 4),
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
  visits <- tapply(df$.wave, df[[unit]], function(w) length(unique(w)))
  df$visits <- as.vector(visits[as.character(df[[unit]])])
  df
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
withr::local_options(survey.lonely.psu = "fail")
frame <- expand.grid(hh = 1:4, psu = 1:9)
frame$h <- "a"
frame$mos <- ifelse(frame$psu == 1L, 1000, 10)
frame$y <- (frame$hh * 3 + frame$psu) %% 7
schedule <- data.frame(
    wave = c(1, 1, 2, 2), panel = c(1, 2, 1, 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
master <- suppressMessages(
    sampling_design() |>
      add_stage() |> stratify_by(h) |> cluster_by(psu) |>
      draw(n = 5, method = "pps_brewer", mos = mos) |>
      add_stage() |> cluster_by(hh) |> draw(n = 2) |>
      execute(frame, seed = 1, panels = schedule)
  )
expect_identical(unique(master$psu[master$.certainty_1]), 1L)
for (waves in list(1, 1:2)) {
    window <- execute(master, wave = waves)
    df <- with_visits(window, "psu")
    df$unit <- ifelse(df$.certainty_1, paste0("hh", df$hh), paste0("ea", df$psu))
    df$stratum <- interaction(df$h, df$visits, df$.certainty_1)
    reference <- survey::svydesign(
      ids = ~unit, strata = ~stratum, weights = ~.weight, data = df
    )
    expect_no_warning(svy <- as_svydesign(window))
    expect_identical(survey::degf(svy), survey::degf(reference))
    expect_equal(
      unname(survey::SE(survey::svytotal(~y, svy))),
      unname(survey::SE(survey::svytotal(~y, reference)))
    )
  }
