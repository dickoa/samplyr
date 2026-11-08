# Extracted from test-subrounds.R:295

# prequel ----------------------------------------------------------------------
poura_urban <- function() {
  bfa_eas[bfa_eas$commune == "Poura" & bfa_eas$urban_rural == "Urban", ]
}
pps_12 <- function() {
  sampling_design() |>
    draw(n = 12, method = "pps_systematic", mos = households)
}
capture_panel_certainty <- function(expr) {
  found <- list()
  withCallingHandlers(
    force(expr),
    samplyr_message_panel_certainty = function(m) {
      found[[length(found) + 1L]] <<- m
      invokeRestart("muffleMessage")
    }
  )
  found
}
periods_by_region <- function(n) {
  sampling_design() |>
    stratify_by(region) |>
    draw(n = n, method = "pps_systematic", mos = households) |>
    execute(bfa_eas, seed = 1, panels = subrounds(12))
}

# test -------------------------------------------------------------------------
out <- cli::ansi_strip(capture.output(summary(periods_by_region(24))))
expect_true(any(out == paste0(
    "i Subrounds: 12, even in all 13 strata (2 units per subround)."
  )))
periods <- summary(periods_by_region(24))$periods
expect_identical(periods$k, 12L)
