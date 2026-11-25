# Extracted from test-edge-cases.R:2212

# prequel ----------------------------------------------------------------------
suppressPackageStartupMessages(library(dplyr))
alike_frame <- function() {
  data.frame(
    id = 1:6,
    h = rep(c(0.1 + 0.2, 0.3), each = 3),
    cl = rep(c(1, 1 + 1e-15), c(2, 4)),
    mos = 10
  )
}
date_panel <- function(days) {
  psu <- data.frame(key = as.Date("2020-01-01") + days)
  el <- data.frame(key = rep(psu$key, each = 5L), id = seq_len(5L * nrow(psu)))
  design <- sampling_design() |> add_stage("psu") |> cluster_by(key) |>
    draw(n = 5) |> add_stage("el") |> draw(n = 2)
  rotation <- data.frame(
    panel = rep(1:2, 2), period = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  execute(design, list(psu = psu, el = el), seed = 1, panels = rotation)
}

# test -------------------------------------------------------------------------
thirty <- (0.3 / 0.1) * 10
expect_lt(thirty, 30)
frame <- data.frame(id = 1:100, ea = rep(1:50, each = 2),
                      st = rep(c("a", "b"), 50), size = rep(1:10, 10))
for (method in c("srswor", "systematic", "srswr")) {
    s <- execute(sampling_design() |> draw(n = thirty, method = method),
                 frame, seed = 1)
    expect_identical(nrow(s), 30L, info = method)
  }
clusters <- execute(sampling_design() |> cluster_by(ea) |> draw(n = thirty),
                      frame, seed = 1)
expect_identical(length(unique(clusters$ea)), 30L)
allocated <- execute(
    sampling_design() |> stratify_by(st, alloc = "proportional") |>
      draw(n = 2 * thirty),
    frame, seed = 1
  )
expect_identical(nrow(allocated), 60L)
expect_identical(
    execute(sampling_design() |> draw(n = 10), frame, seed = thirty)$id,
    execute(sampling_design() |> draw(n = 10), frame, seed = 30)$id
  )
expect_identical(
    subrounds((0.3 / 0.1) * 1)$k,
    3L
  )
master <- execute(sampling_design() |> draw(n = 30), frame, seed = 1,
                    panels = (0.3 / 0.1) * 1)
expect_identical(attr(master, "metadata")$panel_assignment$panels, 3L)
two <- sampling_design() |> add_stage() |> cluster_by(ea) |> draw(n = 5) |>
    add_stage() |> draw(n = 1)
expect_error(execute(two, frame, stages = (0.2 / 0.1) * 1 - 1e-15, seed = 1),
               class = "samplyr_error_stage_argument")
