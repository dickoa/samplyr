# Extracted from test-periods.R:201

# prequel ----------------------------------------------------------------------
poura_urban <- function() {
  bfa_eas[bfa_eas$commune == "Poura" & bfa_eas$urban_rural == "Urban", ]
}
pps_12 <- function() {
  sampling_design() |>
    draw(n = 12, method = "pps_systematic", mos = households)
}

# test -------------------------------------------------------------------------
periods_sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = periods(12))
  )
expect_identical(tbl_sum(periods_sample)[["Periods"]], "12 (in .panel)")
expect_identical(tbl_sum(execute(periods_sample, period = 3))[["Period"]],
                   "3 of 12")
count_sample <- suppressMessages(
    execute(pps_12(), poura_urban(), seed = 1, panels = 12)
  )
expect_identical(tbl_sum(count_sample)[["Panels"]],
                   "12 rotation groups, no schedule")
schedule <- data.frame(
    panel = rep(1:4, 3),
    period = rep(1:3, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
               FALSE, FALSE, TRUE, TRUE)
  )
rotating <- execute(sampling_design() |> draw(n = 40),
                      data.frame(id = 1:200), seed = 1, panels = schedule)
expect_identical(tbl_sum(rotating)[["Panels"]],
                   "4 rotation groups over 3 waves")
