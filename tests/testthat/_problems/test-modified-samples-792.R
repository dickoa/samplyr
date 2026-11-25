# Extracted from test-modified-samples.R:792

# test -------------------------------------------------------------------------
skip_if_not_installed("bit64")
ids <- bit64::as.integer64(c(-5, 3, -100, 7, 0, -2147483648))
frame <- data.frame(id = 1:6, k = ids, ea = rep(1:3, 2))
expect_identical(control_order(frame, list(rlang::quo(k))),
                   c(6L, 3L, 1L, 5L, 2L, 4L))
frame <- data.frame(
    ea = bit64::as.integer64(rep(c(-100, -200, 300, -400, 500, -600), each = 2)),
    st = bit64::as.integer64(rep(c(-1, -2), 6)),
    hh = 1:12
  )
sample <- sampling_design() |> cluster_by(ea) |> draw(n = 4) |>
    execute(frame, seed = 1)
edited <- sample
edited$ea[edited$ea == edited$ea[1]] <- bit64::as.integer64(-999999)
expect_false(identical(verify_sample_integrity(edited), "ok"))
