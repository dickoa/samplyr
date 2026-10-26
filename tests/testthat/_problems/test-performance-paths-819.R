# Extracted from test-performance-paths.R:819

# test -------------------------------------------------------------------------
design <- sampling_design() |>
    add_stage() |> stratify_by(h) |> cluster_by(psu, nest = FALSE) |>
    draw(n = 1) |>
    add_stage() |> draw(n = 1)
frame <- data.frame(psu = rep(1:4, each = 2), h = c("a", "b", rep("b", 6)))
expect_error(
    link_stage_frame(frame, frame, design, 2L, check_coverage = FALSE),
    class = "samplyr_error_frame_parent_conflict"
  )
