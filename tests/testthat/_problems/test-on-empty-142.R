# Extracted from test-on-empty.R:142

# test -------------------------------------------------------------------------
frame <- data.frame(id = 1:5)
design <- sampling_design() |>
    draw(frac = 0.2, method = "bernoulli", on_empty = "silent")
none <- sampling_design() |>
    draw(frac = 0.01, method = "bernoulli", on_empty = "silent") |>
    execute(frame, seed = 1, reps = 3)
expect_identical(attr(none, "metadata")$replicate_rows, c(`1` = 0L, `2` = 0L, `3` = 0L))
expect_true(has_multiple_replicates(none))
