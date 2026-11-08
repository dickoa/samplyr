# Extracted from test-waves.R:55

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
spec <- normalize_panel_input(rotation_2_2())
expect_identical(spec$k, 4L)
expect_identical(spec$r_min, 2L)
expect_identical(spec$block_size, 4L)
expect_identical(nrow(spec$schedule), 16L)
expect_identical(
    names(spec$schedule),
    c("wave", "panel", "active", "visit")
  )
