# Extracted from test-validate-frame.R:195

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "samplyr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
good_frame <- data.frame(
  region = rep(c("N", "S"), each = 10),
  district = rep(letters[1:4], each = 5),
  size = runif(20, 1, 100),
  y = rnorm(20)
)
fp_design <- sampling_design() |>
  stratify_by(region) |>
  draw(n = 4)
fp_restored <- read_design(design_json(fp_design, frame = good_frame))

# test -------------------------------------------------------------------------
modified <- good_frame
modified$y <- NULL
modified$extra <- 1
modified$size <- as.character(modified$size)
msgs <- capture.output(
    validate_frame(fp_restored, modified),
    type = "message"
  )
text <- paste(msgs, collapse = " ")
expect_match(text, "\"y\" no longer present")
expect_match(text, "new column \"extra\"")
expect_match(text, "size \\(character instead of numeric\\)")
