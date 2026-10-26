# Extracted from test-print.R:53

# test -------------------------------------------------------------------------
draw_line <- function(design) {
    out <- cli::ansi_strip(capture.output(print(design)))
    out[grepl("Draw:", out, fixed = TRUE)]
  }
plain <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = households)
expect_match(draw_line(plain),
               "Draw: n = 10, method = pps_brewer, mos = households$")
set <- sampling_design() |>
    draw(frac = 0.1, method = "pps_brewer", mos = households,
         certainty_size = 800, certainty_overflow = "allow",
         on_empty = "warn", round = "nearest")
line <- draw_line(set)
for (part in c("certainty_size = 800", "certainty_overflow = allow",
                 "on_empty = warn", "round = nearest")) {
    expect_match(line, part, fixed = TRUE)
  }
