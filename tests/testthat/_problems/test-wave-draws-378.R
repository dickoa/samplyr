# Extracted from test-wave-draws.R:378

# prequel ----------------------------------------------------------------------
wave_draw_design <- function(n = 12) {
  sampling_design() |>
    add_stage("EAs") |> stratify_by(region) |> cluster_by(ea_id) |>
    draw(n = n, method = "pps_systematic", mos = households) |>
    add_stage("Households") |> draw(n = 5)
}
wave_draw_master <- function(n = 12) {
  execute(wave_draw_design(n), bfa_eas, stages = 1, seed = 1,
          panels = periods(12))
}
wave_draw_listing <- function(master) {
  eas <- as.data.frame(master)["ea_id"]
  out <- eas[rep(seq_len(nrow(eas)), each = 20), , drop = FALSE]
  out$hh_id <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}
wave_listing <- function(listing, master, t) {
  listing[listing$ea_id %in% master$ea_id[master$.panel == t], ]
}
draw_every_wave <- function(master, listing, order = 1:12) {
  for (t in order) {
    master <- execute(master, wave_listing(listing, master, t), period = t,
                      seed = 100 + t)
  }
  master
}

# test -------------------------------------------------------------------------
master <- wave_draw_master()
listing <- wave_draw_listing(master)
order <- c(4, 9, 1)
partial <- master
for (t in order) {
    partial <- execute(partial, wave_listing(listing, master, t), period = t,
                       seed = 100 + t)
  }
frames <- c(list(bfa_eas), lapply(order, function(t) {
    wave_listing(listing, master, t)
  }))
drawn_rows <- function(x) {
    lapply(samplyr:::wave_draws(x), function(d) d$rows$hh_id)
  }
path <- withr::local_tempfile(fileext = ".json")
expect_no_warning(write_design(partial, path, frame = frames))
receipt <- attr(read_design(path), "execution")
expect_identical(receipt$transition, "wave_draw")
expect_identical(
    vapply(receipt$earlier_calls, `[[`, "", "transition"),
    c("start", "wave_draw", "wave_draw")
  )
for (replayed in list(replay_design(partial, frames),
                        replay_design(read_design(path), frames))) {
    expect_identical(replayed$ea_id, partial$ea_id)
    expect_identical(drawn_rows(replayed), drawn_rows(partial))
  }
