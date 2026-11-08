# Extracted from test-waves.R:123

# prequel ----------------------------------------------------------------------
wave_frame <- function(n = 400) {
  data.frame(
    id = seq_len(n),
    region = rep(c("North", "South"), each = n / 2),
    score = rep((seq_len(n / 2) * 7L) %% 97L, times = 2),
    value = seq_len(n) / n
  )
}
rotation_2_2 <- function() {
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
rotation_1_of_4 <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    period = rep(1:4, each = 4),
    active = as.logical(diag(4))
  )
}
master_2_2 <- function(seed = 42) {
  sampling_design() |>
    stratify_by(region) |>
    draw(n = 60) |>
    execute(wave_frame(), seed = seed, panels = rotation_2_2())
}

# test -------------------------------------------------------------------------
expect_error(
    normalize_panel_input(data.frame(panel = 1:4)),
    class = "samplyr_error_schedule_columns"
  )
expect_error(
    normalize_panel_input(data.frame(panel = c(1, 1), period = c(1, 1))),
    class = "samplyr_error_schedule_duplicates"
  )
expect_error(
    normalize_panel_input(data.frame(panel = c(1, 3), period = c(1, 1))),
    class = "samplyr_error_schedule_gap"
  )
expect_error(
    normalize_panel_input(data.frame(panel = c(1, 2), period = c(1, 3))),
    class = "samplyr_error_schedule_gap"
  )
expect_error(
    normalize_panel_input(data.frame(panel = c(1, 2), period = c(1, 1))) |>
      suppressWarnings(),
    NA
  )
expect_error(
    normalize_panel_input(data.frame(panel = 1:2, period = c(1, 1.5))),
    class = "samplyr_error_schedule_values"
  )
expect_error(
    normalize_panel_input(
      data.frame(panel = 1:2, period = c(1, 1), active = c("y", "n"))
    ),
    class = "samplyr_error_schedule_active"
  )
expect_error(
    normalize_panel_input(data.frame(panel = c(1, 1), period = c(1, 2))),
    class = "samplyr_error_schedule_size"
  )
expect_error(
    normalize_panel_input(
      data.frame(
        panel = rep(1:2, 2),
        period = rep(1:2, each = 2),
        active = c(TRUE, TRUE, FALSE, FALSE)
      )
    ),
    class = "samplyr_error_schedule_idle_occasion"
  )
