# Extracted from test-on-empty.R:154

# test -------------------------------------------------------------------------
frame <- data.frame(id = 1:5)
design <- sampling_design() |>
    draw(frac = 0.2, method = "bernoulli", on_empty = "silent")
none <- sampling_design() |>
    draw(frac = 0.01, method = "bernoulli", on_empty = "silent") |>
    execute(frame, seed = 1, reps = 3)
expect_identical(attr(none, "metadata")$replicate_rows, c(`1` = 0L, `2` = 0L, `3` = 0L))
expect_true(has_multiple_replicates(none))
one <- NULL
for (seed in 1:200) {
    s <- execute(design, frame, seed = seed, reps = 3)
    if (nrow(s) > 0L && length(unique(s$.replicate)) == 1L) {
      one <- s
      break
    }
  }
expect_false(is.null(one), info = "Expected one visible replicate within 200 seeds")
expect_true(has_multiple_replicates(one))
expect_true(any(cli::ansi_strip(format(one)) == "# Replicates:   3"))
