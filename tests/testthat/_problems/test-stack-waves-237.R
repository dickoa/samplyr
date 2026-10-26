# Extracted from test-stack-waves.R:237

# prequel ----------------------------------------------------------------------
stack_frame <- function(n = 400) {
  data.frame(
    id = seq_len(n),
    region = rep(c("North", "South"), each = n / 2),
    psu = rep(seq_len(n / 10), each = 10),
    value = seq_len(n) / n
  )
}
stack_rotation <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    wave = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
}
stack_master <- function(seed = 11, design = NULL) {
  design <- design %||%
    (sampling_design() |> stratify_by(region) |> draw(n = 60))
  execute(design, stack_frame(), seed = seed, panels = stack_rotation())
}

# test -------------------------------------------------------------------------
design <- sampling_design() |> stratify_by(region) |> draw(n = 60)
frame_a <- stack_frame()
frame_b <- stack_frame()
frame_b$id <- frame_b$id + 100000L
a <- execute(design, frame_a, seed = 7, panels = stack_rotation())
b <- execute(design, frame_b, seed = 7, panels = stack_rotation())
expect_length(intersect(as.data.frame(a)$id, as.data.frame(b)$id), 0L)
expect_identical(
    attr(a, "metadata")$integrity,
    attr(b, "metadata")$integrity
  )
