# Extracted from test-waves.R:1040

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
bare <- sampling_design() |> draw(n = 10) |> execute(wave_frame(), seed = 3)
expect_error(execute(bare, period = 1), class = "samplyr_error_occasion_no_schedule")
st <- data.frame(
    panel = rep(1:2, times = 3), period = rep(1:3, each = 2),
    active = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
startup <- sampling_design() |> draw(n = 40) |>
    execute(data.frame(id = 1:200), seed = 1, panels = st)
intake <- sampling_design() |> draw(n = 12) |>
    execute(data.frame(id = 201:260), seed = 2)
program <- rotation_program(
    selections = list(startup = startup, intake_2 = intake),
    entry_wave = c(startup = 1, intake_2 = 2),
    schedule = rbind(
      transform(st, selection = "startup"),
      data.frame(selection = "intake_2", panel = 1L, period = 1:3,
                 active = c(FALSE, TRUE, TRUE))
    )
  )
whole <- execute(program, period = 2)[["intake_2"]]
