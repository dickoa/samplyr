# Extracted from test-rotation-program.R:526

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
frame <- data.frame(id = sprintf("U%03d", 1:120))
master_sched <- data.frame(
    panel = rep(1:2, 3), period = rep(1:3, each = 2), active = rep(TRUE, 6)
  )
partitioned <- execute(
    sampling_design() |> draw(n = 40), frame, seed = 21, panels = master_sched
  )
solo <- execute(
    sampling_design() |> draw(n = 20),
    data.frame(id = sprintf("B%03d", 1:60)), seed = 22
  )
expect_null(attr(solo, "metadata")$panel_assignment)
schedule <- data.frame(
    selection = c(rep("main", 6), rep("solo", 3)),
    panel = c(rep(1:2, 3), rep(1L, 3)),
    period = c(rep(1:3, each = 2), 1:3),
    active = c(rep(TRUE, 6), TRUE, FALSE, TRUE)
  )
program <- rotation_program(
    selections = list(main = partitioned, solo = solo),
    entry_wave = c(main = 1L, solo = 1L),
    schedule = schedule
  )
rows <- function(wave, selection) {
    if (!selection %in% names(wave)) return(0L)
    nrow(as.data.frame(wave[[selection]]))
  }
expect_setequal(names(execute(program, period = 1)), c("main", "solo"))
