# Extracted from test-rotation-program.R:392

# prequel ----------------------------------------------------------------------
startup_schedule <- function() {
  data.frame(
    panel = rep(1:2, times = 3),
    period = rep(1:3, each = 2),
    active = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
}
program_startup <- function(seed = 1) {
  sampling_design() |>
    draw(n = 40) |>
    execute(
      data.frame(id = 1:200, value = seq_len(200) / 200),
      seed = seed,
      panels = startup_schedule()
    )
}
program_intake <- function(seed = 2) {
  sampling_design() |>
    draw(n = 12) |>
    execute(
      data.frame(id = 201:260, value = seq_len(60) / 60),
      seed = seed
    )
}
program_schedule <- function() {
  rbind(
    transform(startup_schedule(), selection = "startup"),
    data.frame(
      selection = "intake_2",
      panel = 1L,
      period = 1:3,
      active = c(FALSE, TRUE, TRUE)
    )
  )
}
two_selection_program <- function() {
  rotation_program(
    selections = list(startup = program_startup(), intake_2 = program_intake()),
    entry_period = c(startup = 1, intake_2 = 2),
    schedule = program_schedule()
  )
}

# test -------------------------------------------------------------------------
program <- two_selection_program()
expect_error(
    execute(program),
    class = "samplyr_error_program_occasion_required"
  )
expect_error(
    execute(program, period = 2, seed = 1),
    class = "samplyr_error_occasion_extra_arguments"
  )
expect_error(
    execute(program, data.frame(id = 1), period = 2),
    class = "samplyr_error_occasion_extra_arguments"
  )
expect_error(
    execute(program, period = 2, frame_digest = "full"),
    class = "samplyr_error_occasion_extra_arguments"
  )
expect_error(
    execute(program, period = 9),
    class = "samplyr_error_occasion_undeclared"
  )
expect_error(
    execute(program, period = 2, seed = 1),
    regexp = "Every input a wave needs is stored with the program"
  )
