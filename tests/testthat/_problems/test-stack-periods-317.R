# Extracted from test-stack-periods.R:317

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
damage <- list(
    `no activation link` = function(md) {
      md$prev_phase$transition <- "something_else"
      md
    },
    `no wave number` = function(md) {
      md$wave$wave <- NULL
      md
    },
    `no master fingerprint` = function(md) {
      md$wave$master_digest <- NULL
      md
    },
    `no schedule fingerprint` = function(md) {
      md$wave$schedule_digest <- NULL
      md
    },
    `no assignment record` = function(md) {
      md$panel_assignment <- NULL
      md
    }
  )
for (nm in names(damage)) {
    waves <- lapply(1:2, function(t) {
      w <- execute(master, period = t)
      attr(w, "metadata") <- damage[[nm]](attr(w, "metadata"))
      w
    })
    expect_error(
      stack_periods(waves[[1]], waves[[2]]),
      class = "samplyr_error_stack_periods_provenance",
      info = nm
    )
  }
