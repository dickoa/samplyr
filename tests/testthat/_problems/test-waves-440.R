# Extracted from test-waves.R:440

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
frame <- wave_frame()
master <- master_2_2()
path <- withr::local_tempfile(fileext = ".json")
write_design(master, path, frame = frame)
restored <- read_design(path)
receipt <- attr(restored, "execution")
expect_equal(receipt$panels, 4L)
expect_equal(receipt$panel_assignment$r_min, 2L)
expect_equal(
    decode_panel_argument(receipt, receipt$panel_assignment),
    normalize_panel_input(rotation_2_2())$schedule[c("wave", "panel", "active")]
  )
