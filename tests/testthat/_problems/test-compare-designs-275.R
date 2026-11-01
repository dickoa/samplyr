# Extracted from test-compare-designs.R:275

# prequel ----------------------------------------------------------------------
compare_frame <- function() {
  villages <- expand.grid(
    v = 1:4,
    district = sprintf("d%02d", 1:12),
    stringsAsFactors = FALSE
  )
  villages$region <- c("East", "Centre", "West")[
    (match(villages$district, sprintf("d%02d", 1:12)) - 1L) %/% 4L + 1L
  ]
  villages$village <- paste0(villages$district, "-v", villages$v)
  villages$phc <- ifelse(villages$v <= 2, "PHC", "non-PHC")
  villages$size <- 6L
  villages$size[villages$village == "d07-v3"] <- 5L
  frame <- villages[rep(seq_len(nrow(villages)), villages$size), ]
  frame$compound <- seq_len(nrow(frame))
  frame$district_pop <- 100 + 7 * match(frame$district, sprintf("d%02d", 1:12))
  frame$village_pop <- 20 + 3 * frame$v + nchar(frame$district)
  rownames(frame) <- NULL
  frame[c("region", "district", "phc", "village", "compound",
          "district_pop", "village_pop")]
}
compare_reference <- function(district_method = "pps_brewer") {
  mos <- if (district_method == "srswor") NULL else quote(district_pop)
  sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(district) |>
    draw(n = 2, method = district_method, mos = !!mos) |>
    add_stage() |>
    stratify_by(phc) |>
    cluster_by(village) |>
    draw(n = 1, method = "pps_brewer", mos = village_pop) |>
    add_stage() |>
    draw(n = 4)
}
compare_requirement <- function() {
  sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(district) |>
    draw(n = 2) |>
    add_stage() |>
    stratify_by(phc) |>
    cluster_by(village) |>
    draw(n = 1) |>
    add_stage() |>
    draw(n = 4)
}

# test -------------------------------------------------------------------------
frame <- compare_frame()
design <- compare_requirement()
sample <- execute(design, frame, seed = 1)
err <- expect_error(
    compare_designs(design, sample, frame, key = compound),
    class = "samplyr_error_compare_argument"
  )
expect_identical(condition_header(err), "compare_designs")
expect_error(
    compare_designs(design, design, frame, key = compound, atol = -1),
    class = "samplyr_error_compare_argument"
  )
expect_error(
    compare_designs(design, design, frame, key = compound, level = "units"),
    class = "samplyr_error_compare_argument"
  )
missing_key <- expect_error(
    compare_designs(design, design, frame),
    class = "samplyr_error_exante_key"
  )
