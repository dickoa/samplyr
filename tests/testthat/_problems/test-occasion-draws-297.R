# Extracted from test-occasion-draws.R:297

# prequel ----------------------------------------------------------------------
occasion_draw_design <- function(n = 12) {
  sampling_design() |>
    add_stage("EAs") |> stratify_by(region) |> cluster_by(ea_id) |>
    draw(n = n, method = "pps_systematic", mos = households) |>
    add_stage("Households") |> draw(n = 5)
}
occasion_draw_master <- function(n = 12) {
  execute(occasion_draw_design(n), bfa_eas, stages = 1, seed = 1,
          panels = subrounds(12))
}
occasion_draw_listing <- function(master) {
  eas <- as.data.frame(master)["ea_id"]
  out <- eas[rep(seq_len(nrow(eas)), each = 20), , drop = FALSE]
  out$hh_id <- seq_len(nrow(out))
  rownames(out) <- NULL
  out
}
occasion_listing <- function(listing, master, t) {
  listing[listing$ea_id %in% master$ea_id[master$.panel == t], ]
}
draw_every_occasion <- function(master, listing, order = 1:12) {
  for (t in order) {
    master <- execute(master, occasion_listing(listing, master, t), period = 1, subround = t,
                      seed = 100 + t)
  }
  master
}

# test -------------------------------------------------------------------------
master <- occasion_draw_master(n = 15)
listing <- occasion_draw_listing(master)
partial <- execute(master, occasion_listing(listing, master, 3), period = 1, subround = 3,
                     seed = 103)
early <- execute(partial, period = 1, subround = 3)
complete <- draw_every_occasion(master, listing)
later <- execute(complete, period = 1, subround = 3)
expect_identical(sort(early$hh_id), sort(later$hh_id))
expect_equal(early$.weight[order(early$hh_id)],
               later$.weight[order(later$hh_id)])
expect_true(attr(early, "metadata")[["occasion"]]$provisional)
expect_null(attr(later, "metadata")[["occasion"]]$provisional)
err <- tryCatch(execute(partial, period = 1, subround = 4), error = identity)
expect_s3_class(err, "samplyr_error_occasion_incomplete_master")
expect_match(cli::ansi_strip(conditionMessage(err)),
               "Drawn so far: wave 3", fixed = TRUE)
