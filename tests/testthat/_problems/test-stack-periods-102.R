# Extracted from test-stack-periods.R:102

# prequel ----------------------------------------------------------------------
stack_frame <- function(n = 400) {
  data.frame(
    id = seq_len(n),
    region = rep(c("North", "South"), each = n / 2),
    psu = rep(seq_len(n / 10), each = 10),
    value = seq_len(n) / n
  )
}
stack_rotation <- function() {
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
stack_master <- function(seed = 11, design = NULL) {
  design <- design %||%
    (sampling_design() |> stratify_by(region) |> draw(n = 60))
  execute(design, stack_frame(), seed = seed, panels = stack_rotation())
}

# test -------------------------------------------------------------------------
master <- stack_master()
w1 <- execute(master, period = 1)
w2 <- execute(master, period = 2)
w1$asked_at_occasion_1 <- 1
w2$asked_at_occasion_2 <- 2
tab <- stack_periods(w1, w2)
expect_true(all(c("asked_at_wave_1", "asked_at_wave_2") %in% names(tab)))
