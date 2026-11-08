# Extracted from test-subrounds.R:78

# prequel ----------------------------------------------------------------------
poura_urban <- function() {
  bfa_eas[bfa_eas$commune == "Poura" & bfa_eas$urban_rural == "Urban", ]
}
pps_12 <- function() {
  sampling_design() |>
    draw(n = 12, method = "pps_systematic", mos = households)
}

# test -------------------------------------------------------------------------
frame <- data.frame(
    st = rep(c("a", "b"), c(30, 20)),
    id = 1:50
  )
design <- sampling_design() |>
    stratify_by(st) |>
    draw(n = c(a = 12, b = 5))
err <- tryCatch(
    execute(design, frame, seed = 1, panels = subrounds(12)),
    error = identity
  )
expect_s3_class(err, "samplyr_error_panel_small_pool")
expect_match(conditionMessage(err), "Every period needs units")
