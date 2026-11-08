# Extracted from test-rotation-program.R:70

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
    entry_wave = c(startup = 1, intake_2 = 2),
    schedule = program_schedule()
  )
}

# test -------------------------------------------------------------------------
program <- two_selection_program()
expect_s3_class(program, "rotation_program")
expect_identical(names(program$selections), c("startup", "intake_2"))
expect_identical(program$entry_wave, c(startup = 1L, intake_2 = 2L))
expect_identical(program$panels, c(startup = 2L, intake_2 = 1L))
expect_identical(program$waves, 1:3)
expect_identical(nrow(program$schedule), 9L)
expect_identical(
    names(program$schedule),
    c("wave", "selection", "panel", "active")
  )
