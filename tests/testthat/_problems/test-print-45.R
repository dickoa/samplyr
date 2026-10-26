# Extracted from test-print.R:45

# test -------------------------------------------------------------------------
draw_line <- function(design) {
    out <- cli::ansi_strip(capture.output(print(design)))
    out[grepl("Draw:", out, fixed = TRUE)]
  }
plain <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = households)
expect_identical(
    draw_line(plain),
    "\u2022 Draw: n = 10, method = pps_brewer, mos = households"
  )
