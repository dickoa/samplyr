# Extracted from test-wave-windows.R:393

# prequel ----------------------------------------------------------------------
window_frame <- function() {
  data.frame(
    id = 1:600,
    h = rep(c("a", "b", "c"), each = 200),
    y = rep(c(10, 20, 30), each = 200) + (1:600) %% 7
  )
}
monthly_schedule <- function() {
  schedule <- expand.grid(panel = 1:6, period = 1:3)
  schedule$active <- (schedule$period - 1) %% 3 == (schedule$panel - 1) %% 3
  schedule
}
overlap_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    period = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
}
window_master <- function(n = 15, schedule = monthly_schedule(), seed = 1) {
  sampling_design() |>
    stratify_by(h) |>
    draw(n = n) |>
    execute(window_frame(), seed = seed, panels = schedule)
}
union_chance <- function(master, panels) {
  record <- attr(master, "metadata")$panel_assignment
  df <- as.data.frame(master)
  keys <- do.call(paste, c(df[record$key_vars], sep = "\r"))
  chance <- rep(NA_real_, nrow(df))
  for (pool in record$pools) {
    pool_keys <- vapply(pool$keys, identity, character(1))
    at <- match(keys, pool_keys)
    rows <- which(!is.na(at))
    block <- rep(seq_along(pool$blocks), pool$blocks)[at[rows]]
    take <- rowSums(pool$quotas[, panels, drop = FALSE])
    chance[rows] <- take[block] / pool$blocks[block]
  }
  chance
}
with_visits <- function(window, unit) {
  df <- as.data.frame(window)
  visits <- tapply(df$.period, df[[unit]], function(w) length(unique(w)))
  df$visits <- as.vector(visits[as.character(df[[unit]])])
  df
}

# test -------------------------------------------------------------------------
stage_lines <- function(x) {
    out <- cli::ansi_strip(capture.output(print(summary(x))))
    out[startsWith(out, "i Wave") | grepl("strata: N_h", out, fixed = TRUE)]
  }
draw_master <- function(frame_digest) {
    sampling_design() |>
      stratify_by(h) |>
      draw(n = 20) |>
      execute(window_frame(), seed = 2, panels = overlap_schedule(),
              frame_digest = frame_digest)
  }
full <- draw_master("full")
none <- draw_master("none")
expect_false(is.null(get_frame_digest(full)))
for (waves in list(2, 1:2)) {
    wave <- execute(full, period = waves)
    lines <- stage_lines(wave)
    expect_identical(lines, stage_lines(execute(none, period = waves)))
    n_h <- unique(tapply(wave$.sample_id, wave$h, function(id) {
      length(unique(id))
    }))
    expect_length(n_h, 1L)
    expect_identical(lines[[2]], sprintf(
      "* 3 strata: N_h 200, n_h %d, f_h %.4f", n_h, n_h / 200
    ))
    expect_identical(lines[[1]], sprintf(
      "i %s: %s of 4. It holds %d of the master's 60 units.",
      if (length(waves) > 1L) "Waves" else "Wave",
      wave_span(waves),
      length(unique(wave$.sample_id))
    ))
  }
