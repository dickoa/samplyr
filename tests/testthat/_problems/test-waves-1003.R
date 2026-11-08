# Extracted from test-waves.R:1003

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
skip_if_not_installed("survey")
frame <- wave_frame()
selections <- list(
    a = sampling_design() |>
      stratify_by(region) |>
      draw(n = 40) |>
      execute(frame, seed = 21, panels = rotation_2_2()),
    b = sampling_design() |>
      stratify_by(region) |>
      draw(n = 20) |>
      execute(frame, seed = 22)
  )
program <- rotation_program(
    selections,
    entry_wave = c(a = 1L, b = 2L),
    schedule = data.frame(
      selection = c("a", "a", "a", "a", "b"),
      panel = c(1L, 2L, 1L, 2L, 1L),
      period = c(1L, 1L, 2L, 2L, 2L),
      active = TRUE
    )
  )
live <- execute(program, period = 2)
