# Extracted from test-validate-frame.R:264

# prequel ----------------------------------------------------------------------
good_frame <- data.frame(
  region = rep(c("N", "S"), each = 10),
  district = rep(letters[1:4], each = 5),
  size = runif(20, 1, 100),
  y = rnorm(20)
)

# test -------------------------------------------------------------------------
on.exit(sondage::unregister_method("sized_wor"), add = TRUE)
sondage::register_method(
    "sized_wor", "wor",
    sample_fn = function(pik, ...) {
      sondage::unequal_prob_wor(pik, method = "brewer")$sample
    },
    probabilities = "exact"
  )
frame <- data.frame(
    id = 1:10,
    size = c(5, 2, rep(0, 8)),
    lon = seq(0, 1, length.out = 10),
    lat = rev(seq(0, 1, length.out = 10))
  )
designs <- list(
    sampling_design() |> draw(n = 3, method = "pps_brewer", mos = size),
    sampling_design() |> draw(n = 3, method = "cube", mos = size),
    sampling_design() |>
      draw(n = 3, method = "lpm2", mos = size, spread = c(lon, lat)),
    sampling_design() |> draw(n = 3, method = "pps_sized_wor", mos = size)
  )
for (design in designs) {
    expect_error(
      validate_frame(design, frame),
      class = "samplyr_error_mos_positive_short"
    )
    expect_error(
      suppressWarnings(execute(design, frame, seed = 1)),
      class = "samplyr_error_mos_positive_short"
    )
  }
frame$size <- 0
for (design in designs[c(2, 4)]) {
    expect_error(
      validate_frame(design, frame),
      class = "samplyr_error_mos_zero_sum"
    )
    expect_error(
      suppressWarnings(execute(design, frame, seed = 1)),
      class = "samplyr_error_mos_zero_sum"
    )
  }
frame$size <- c(5, 2, rep(0, 8))
expect_true(validate_frame(
    sampling_design() |> draw(n = 3, method = "pps_multinomial", mos = size),
    frame
  ))
expect_true(validate_frame(
    sampling_design() |> draw(frac = 0.3, method = "pps_poisson", mos = size),
    frame
  ))
expect_error(
    suppressWarnings(execute(designs[[1]], frame, seed = 1)),
    "Cannot draw 3 units by size from 2 units"
  )
