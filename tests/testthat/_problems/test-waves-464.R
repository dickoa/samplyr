# Extracted from test-waves.R:464

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
materialized <- execute(master_2_2(), period = 3)
path <- withr::local_tempfile(fileext = ".json")
expect_no_warning(write_design(materialized, path, frame = frame))
receipt <- attr(read_design(path), "execution")
expect_true(isTRUE(receipt$chained))
expect_identical(receipt$transition, "wave")
expect_equal(receipt$wave$wave, 3L)
expect_equal(as.integer(unlist(receipt$wave$active_panels)), c(3L, 4L))
expect_length(receipt$wave$pools, 2L)
replayed <- replay_design(read_design(path), frame)
