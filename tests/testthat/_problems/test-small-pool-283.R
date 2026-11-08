# Extracted from test-small-pool.R:283

# prequel ----------------------------------------------------------------------
small_pool_frame <- function() {
  data.frame(
    reg = c(rep("A", 400), rep("B", 400), rep("C", 20)),
    psu = c(
      rep(sprintf("A%02d", 1:20), each = 20),
      rep(sprintf("B%02d", 1:20), each = 20),
      rep("C01", 20)
    ),
    y = rep(c(1, 2, 50), times = c(400, 400, 20))
  )
}
small_pool_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 2),
    period = rep(1:2, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
}
small_pool_design <- function() {
  sampling_design() |>
    add_stage("psu") |>
    stratify_by(reg) |>
    cluster_by(psu) |>
    draw(n = 1) |>
    add_stage("hh") |>
    draw(n = 5)
}

# test -------------------------------------------------------------------------
suppressWarnings(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    )
  )
metadata <- attr(master, "metadata")
metadata$panel_assignment$version <- 1L
metadata$panel_assignment$small_pool_policy <- NULL
metadata$panel_assignment$pools <- lapply(
    metadata$panel_assignment$pools,
    function(pool) {
      pool$activation <- NULL
      pool$permanent_reason <- NULL
      pool
    }
  )
attr(master, "metadata") <- metadata
expect_error(
    execute(master, wave = 1),
    class = "samplyr_error_panel_small_pool"
  )
