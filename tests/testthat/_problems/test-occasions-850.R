# Extracted from test-occasions.R:850

# prequel ----------------------------------------------------------------------
occasion_frame <- function(n = 400) {
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
    execute(occasion_frame(), seed = seed, panels = rotation_2_2())
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
materialized <- execute(master_2_2(), period = 1)
expect_error(
    joint_expectation(materialized),
    class = "samplyr_error_occasion_joint_unsupported"
  )
