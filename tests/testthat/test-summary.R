test_that("summary distinguishes sample fractions from inclusion probabilities", {
  set.seed(42)
  frame <- data.frame(
    id = seq_len(100),
    size = runif(100, 1, 2),
    x = rnorm(100)
  )

  equal_sample <- sampling_design() |>
    draw(n = 20, method = "cube", aux = x) |>
    execute(frame, seed = 1)
  equal_txt <- paste(capture.output(summary(equal_sample)), collapse = "\n")

  expect_match(equal_txt, "N = 100, n = 20, f = 0.2000", fixed = TRUE)
  expect_false(grepl("mean f", equal_txt, fixed = TRUE))
  expect_false(grepl("unit-specific", equal_txt, fixed = TRUE))

  unequal_sample <- sampling_design() |>
    draw(n = 20, method = "cube", mos = size, aux = x) |>
    execute(frame, seed = 1)
  unequal_txt <- paste(
    capture.output(summary(unequal_sample)),
    collapse = "\n"
  )

  expect_match(unequal_txt, "N = 100, n = 20, f = 0.2000", fixed = TRUE)
  expect_match(unequal_txt, "MOS: size", fixed = TRUE)
  expect_match(
    unequal_txt,
    "Inclusion probabilities are unit-specific.",
    fixed = TRUE
  )
  expect_false(grepl("mean inclusion fraction", unequal_txt, fixed = TRUE))
})

test_that("summary uses concise with-replacement wording", {
  frame <- data.frame(id = seq_len(20))
  sample <- sampling_design() |>
    draw(n = 5, method = "srswr") |>
    execute(frame, seed = 1)

  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "n = 5 (with replacement, no FPC)", fixed = TRUE)
  expect_false(grepl(";", txt, fixed = TRUE))
})
