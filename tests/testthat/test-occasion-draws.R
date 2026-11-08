## Drawing the later stages one occasion at a time

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

# Twenty households in every selected EA.
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

test_that("a master drawn occasion by occasion completes into the sample of one call", {
  master <- occasion_draw_master()
  listing <- occasion_draw_listing(master)
  order <- c(3, 1, 12, 2, 4:11)

  partial <- master
  for (t in order[1:11]) {
    partial <- execute(partial, occasion_listing(listing, master, t), period = 1, subround = t,
                       seed = 100 + t)
    expect_identical(get_stages_executed(partial), 1L)
    expect_identical(lapply(as.data.frame(partial), identity),
                     lapply(as.data.frame(master), identity))
  }
  # A growing listing file: rows of other occasions are ignored.
  complete <- execute(partial, listing, period = 1, subround = order[12], seed = 112)
  oneshot <- execute(master, listing, seed = 2)

  expect_identical(get_stages_executed(complete), 1:2)
  expect_identical(nrow(complete), nrow(oneshot))
  expect_identical(complete$.sample_id, seq_len(nrow(complete)))
  expect_identical(unique(complete$ea_id), master$ea_id)
  expect_identical(c(table(complete$ea_id)), c(table(oneshot$ea_id)))
  expect_equal(sort(complete$.weight), sort(oneshot$.weight))
  expect_identical(complete$.panel, master$.panel[match(complete$ea_id,
                                                        master$ea_id)])

  # Each occasion's households come from that occasion's call alone.
  again <- execute(master, occasion_listing(listing, master, 3), period = 1, subround = 3,
                   seed = 103)
  first <- samplyr:::occasion_draws(again)[[1]]$rows
  expect_identical(complete$hh_id[complete$.panel == 3], first$hh_id)

  expect_equal(
    sort(execute(complete, period = 1, subround = 5)$.weight),
    sort(execute(oneshot, period = 1, subround = 5)$.weight)
  )
  skip_if_not_installed("survey")
  expect_no_error(suppressWarnings(as_svydesign(complete)))
})

test_that("an occasion's summary shows the seeds of the draws behind its rows", {
  master <- occasion_draw_master()
  listing <- occasion_draw_listing(master)
  header <- function(x) {
    grep("stages = ", capture.output(summary(x)), value = TRUE)
  }

  partial <- execute(master, occasion_listing(listing, master, 3), period = 1, subround = 3,
                     seed = 103)
  expect_identical(summary(execute(partial, period = 1, subround = 3))$seed, 103L)

  complete <- draw_every_occasion(master, listing, order = c(3, 1, 12, 2, 4:11))
  expect_identical(summary(complete)$seed, 101:112)
  expect_match(header(complete), "seeds = 101 to 112", fixed = TRUE)
  expect_identical(summary(execute(complete, period = 1, subround = 3))$seed, 103L)
  expect_match(header(execute(complete, period = 1, subround = 3)), "| seed = 103$")
  window <- execute(complete, period = 1, subround = 2:3)
  expect_identical(summary(window)$seed, 102:103)
  expect_match(header(window), "seeds = 102, 103", fixed = TRUE)

  # A master completed in one call keeps the seed of that call.
  oneshot <- execute(master, listing, seed = 2)
  expect_identical(summary(execute(oneshot, period = 1, subround = 3))$seed, 2L)

  # A draw without a seed leaves its occasion without one.
  unseeded <- execute(master, occasion_listing(listing, master, 3), period = 1, subround = 3)
  expect_identical(summary(execute(unseeded, period = 1, subround = 3))$seed, integer(0))
  expect_no_match(header(execute(unseeded, period = 1, subround = 3)), "seed")
})

test_that("an error inside an occasion's draw names the call the user wrote", {
  master <- occasion_draw_master()
  listing <- occasion_draw_listing(master)
  part <- occasion_listing(listing, master, 2)
  short <- part[part$ea_id != part$ea_id[1], ]
  err <- tryCatch(
    execute(master, short, period = 1, subround = 2, seed = 102),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_frame_missing_parent")
  expect_identical(err$call, quote(execute(master, short, period = 1, subround = 2, seed = 102)))
  # The header is the call, not the listing it carried.
  header <- cli::ansi_strip(format(err))
  expect_lt(nchar(strsplit(header, "\n")[[1]][1]), 120L)
})

test_that("drawing the same occasions with the same seeds gives the same sample", {
  master <- occasion_draw_master()
  listing <- occasion_draw_listing(master)
  a <- draw_every_occasion(master, listing)
  b <- draw_every_occasion(master, listing, order = 12:1)
  expect_identical(as.data.frame(a)[order(a$hh_id), "hh_id"],
                   as.data.frame(b)[order(b$hh_id), "hh_id"])
})

test_that("a sample completed occasion by occasion carries the digest of one call", {
  master <- occasion_draw_master()
  listing <- occasion_draw_listing(master)
  oneshot <- get_frame_digest(execute(master, listing, seed = 2))
  pools <- function(digest) {
    p <- digest$stages[[2]]$pools
    p <- p[order(p$parent_unit), c("parent_unit", "N", "n_target",
                                   "n_expected", "n_realized", "chance")]
    rownames(p) <- NULL
    p
  }

  # The same listing every time keeps its frame record.
  same <- master
  for (t in 1:12) {
    same <- execute(same, listing, period = 1, subround = t, seed = 100 + t)
  }
  expect_identical(get_frame_digest(same)$frames, oneshot$frames)
  expect_identical(pools(get_frame_digest(same)), pools(oneshot))

  complete <- draw_every_occasion(master, listing)
  digest <- get_frame_digest(complete)
  expect_identical(digest$status, "complete")
  expect_identical(digest$stages[[1]], get_frame_digest(master)$stages[[1]])
  expect_identical(pools(digest), pools(oneshot))
  record <- digest$frames[[2]]
  expect_null(record$fingerprint_exact)
  expect_identical(record$n_rows, nrow(listing))
  expect_identical(
    digest,
    get_frame_digest(draw_every_occasion(master, listing, order = 12:1))
  )

  # Each selected unit is a household of the sample, under its own EA.
  sel <- digest$stages[[2]]$selected
  stage_1 <- digest$stages[[1]]$selected
  parent <- digest$stages[[2]]$pools$parent_unit[sel$pool_id]
  key_vars <- samplyr:::collect_ancestor_cluster_vars(get_design(master), 2)
  traced <- paste(stage_1$key[match(parent, stage_1$unit_id)], sel$unit_id)
  drawn <- paste(
    samplyr:::digest_path_keys(complete, seq_len(nrow(complete)), key_vars),
    (complete$hh_id - 1L) %% 20L + 1L
  )
  expect_identical(sort(traced), sort(drawn))

  stage_2 <- frame_summary(complete)
  expect_identical(stage_2$n_realized[stage_2$stage == 2], 780)
})

test_that("a later stage drawn occasion by occasion links to its parents' units", {
  design <- sampling_design() |>
    add_stage("EAs") |> stratify_by(region) |> cluster_by(ea_id) |>
    draw(n = 12, method = "pps_systematic", mos = households) |>
    add_stage("Segments") |> cluster_by(seg_id) |> draw(n = 2) |>
    add_stage("Households") |> draw(n = 3)
  master <- execute(design, bfa_eas, stages = 1, seed = 1,
                    panels = subrounds(12))
  eas <- as.data.frame(master)["ea_id"]
  listing <- eas[rep(seq_len(nrow(eas)), each = 40), , drop = FALSE]
  listing$seg_id <- rep(rep(1:4, each = 10), nrow(eas))
  listing$hh_id <- seq_len(nrow(listing))
  complete <- master
  for (t in c(5:12, 1:4)) {
    complete <- execute(complete, occasion_listing(listing, master, t),
                        period = 1, subround = t, seed = 100 + t, stages = 2:3)
  }

  digest <- get_frame_digest(complete)
  expect_identical(vapply(digest$stages, `[[`, 1L, "frame_ref"), c(1L, 2L, 2L))
  segments <- digest$stages[[2]]$selected
  parents <- digest$stages[[3]]$pools$parent_unit
  key_vars <- samplyr:::collect_ancestor_cluster_vars(design, 3)
  expect_identical(
    sort(segments$key[match(parents, segments$unit_id)]),
    sort(unique(samplyr:::digest_path_keys(complete, seq_len(nrow(complete)),
                                           key_vars)))
  )
  expect_identical(sum(digest$stages[[3]]$pools$n_realized), nrow(complete))
})

test_that("a master without a digest is completed without one", {
  master <- execute(occasion_draw_design(), bfa_eas, stages = 1, seed = 1,
                    panels = subrounds(12), frame_digest = "none")
  listing <- occasion_draw_listing(master)
  expect_no_warning(complete <- draw_every_occasion(master, listing))
  expect_null(get_frame_digest(complete))
})

test_that("uneven subrounds keep their own factor when drawn occasion by occasion", {
  master <- occasion_draw_master(n = 15)
  listing <- occasion_draw_listing(master)
  complete <- draw_every_occasion(master, listing)
  oneshot <- execute(master, listing, seed = 2)
  factors <- function(s, t) {
    w <- execute(s, period = 1, subround = t)
    sort(unique(round(w$.weight / s$.weight[match(w$hh_id, s$hh_id)], 6)))
  }
  for (t in c(1, 7)) {
    expect_identical(factors(complete, t), factors(oneshot, t))
  }
  expect_gt(length(factors(complete, 1)), 1L)
})

test_that("an occasion is drawn once, under subrounds, with every unit listed", {
  master <- occasion_draw_master()
  listing <- occasion_draw_listing(master)
  once <- execute(master, occasion_listing(listing, master, 3), period = 1, subround = 3,
                  seed = 1)

  expect_error(execute(once, listing, period = 1, subround = 3, seed = 2),
               class = "samplyr_error_occasion_already_drawn")
  expect_error(execute(once, listing, seed = 2),
               class = "samplyr_error_occasion_draws_in_progress")
  short <- occasion_listing(listing, master, 4)
  short <- short[short$ea_id != short$ea_id[1], ]
  expect_error(execute(master, short, period = 1, subround = 4, seed = 2),
               class = "samplyr_error_frame_missing_parent")
  expect_error(execute(master, listing, period = 1, subround = 13, seed = 2),
               class = "samplyr_error_occasion_undeclared")
  expect_error(execute(master, listing, period = 1, subround = 1, stages = 1),
               class = "samplyr_error_occasion_draw_stages")
  expect_no_error(execute(master, occasion_listing(listing, master, 1), period = 1, subround = 1,
                          stages = 2, seed = 1))
  expect_error(execute(master, listing, period = 1, subround = 1, reps = 2),
               class = "samplyr_error_occasion_extra_arguments")
  expect_error(execute(master, listing, period = 1, subround = 1, frame_digest = "summary"),
               class = "samplyr_error_occasion_extra_arguments")

  rotating <- execute(occasion_draw_design(), bfa_eas, stages = 1, seed = 1,
                      panels = 4)
  expect_error(execute(rotating, listing, period = 1, subround = 1),
               class = "samplyr_error_occasion_draw_unsupported")
  plain <- execute(occasion_draw_design(), bfa_eas, stages = 1, seed = 1)
  expect_error(execute(plain, listing, period = 1, subround = 1),
               class = "samplyr_error_occasion_draw_unsupported")
  expect_warning(
    expect_error(execute(dplyr::filter(master, .panel != 2), listing,
                         period = 1, subround = 1),
                 class = "samplyr_error_modified_sample"),
    NA
  )
  complete <- execute(master, listing, seed = 2)
  expect_error(execute(complete, listing, period = 1, subround = 1),
               class = "samplyr_error_occasion_extra_arguments")
  expect_error(execute(execute(complete, period = 1, subround = 1), listing, period = 1, subround = 2),
               class = "samplyr_error_occasion_extra_arguments")
})

test_that("a drawn occasion of an incomplete master has exact weights and no export", {
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
               "Drawn so far: period 1 subround 3.", fixed = TRUE)
  expect_match(cli::ansi_strip(conditionMessage(err)),
               "Period 1 subround 4 reads the units", fixed = TRUE)
  expect_error(execute(master, period = 1, subround = 4),
               class = "samplyr_error_occasion_incomplete_master")

  skip_if_not_installed("survey")
  # Its own rows give the ultimate-cluster export, and nothing else reads it.
  withr::with_options(list(survey.lonely.psu = "adjust"), {
    expect_s3_class(
      as_svydesign(early, systematic_variance = "approximate"),
      "survey.design2"
    )
    expect_s3_class(
      as_svrepdesign(early, systematic_variance = "approximate"),
      "svyrep.design"
    )
  })
  expect_error(as_svydesign(early, method = "full"),
               class = "samplyr_error_occasion_provisional")
  # Completing the draws numbers the units again, so an occasion of the
  # completed master does not stack with one read before.
  expect_error(
    stack_periods(early, execute(complete, period = 1, subround = 4)),
    class = "samplyr_error_occasion_master_mismatch"
  )
  expect_error(write_design(early, withr::local_tempfile(fileext = ".json")),
               class = "samplyr_error_serialize_occasion_provisional")
  expect_error(
    as_svydesign(later, systematic_variance = "approximate", method = "full"),
    class = "samplyr_error_occasion_phase1_pps"
  )
  expect_warning(
    exported <- as_svydesign(later, systematic_variance = "approximate"),
    class = "samplyr_warning_lonely_psu"
  )
  expect_s3_class(exported, "survey.design2")
})

test_that("print and summary say how far the occasions are drawn", {
  master <- occasion_draw_master()
  listing <- occasion_draw_listing(master)
  partial <- master
  for (t in c(5, 2)) {
    partial <- execute(partial, occasion_listing(listing, master, t), period = 1, subround = t,
                       seed = t)
  }
  printed <- cli::ansi_strip(format(partial))
  expect_true(any(grepl("Drawn: +stage 2 in 2 of 12 subrounds \\(2, 5\\)",
                        printed)))
  expect_false(any(grepl("Drawn:", cli::ansi_strip(format(master)))))
  # The footer names the call that reads a drawn occasion.
  footer <- "Read a drawn occasion with `execute(master, period = 1, subround = s)`."
  expect_true(any(grepl(footer, printed, fixed = TRUE)))
  expect_false(any(grepl(footer, cli::ansi_strip(format(master)),
                         fixed = TRUE)))
  expect_match(
    paste(cli::ansi_strip(capture.output(summary(partial))), collapse = "\n"),
    "Drawn: stage 2 in 2 of 12 subrounds (2, 5).", fixed = TRUE
  )
  expect_true(any(grepl("Subround: +2 of 12, provisional",
                        cli::ansi_strip(format(execute(partial, period = 1, subround = 2))))))
})

test_that("receipts replay every occasion's call, before and after the last", {
  master <- occasion_draw_master()
  listing <- occasion_draw_listing(master)
  order <- c(4, 9, 1)
  partial <- master
  for (t in order) {
    partial <- execute(partial, occasion_listing(listing, master, t), period = 1, subround = t,
                       seed = 100 + t)
  }
  frames <- c(list(bfa_eas), lapply(order, function(t) {
    occasion_listing(listing, master, t)
  }))
  drawn_rows <- function(x) {
    lapply(samplyr:::occasion_draws(x), function(d) d$rows$hh_id)
  }

  path <- withr::local_tempfile(fileext = ".json")
  expect_no_warning(write_design(partial, path, frame = frames))
  receipt <- attr(read_design(path), "execution")
  expect_identical(receipt$transition, "occasion_draw")
  expect_identical(
    vapply(receipt$earlier_calls, `[[`, "", "transition"),
    c("start", "occasion_draw", "occasion_draw")
  )
  for (replayed in list(replay_design(partial, frames),
                        replay_design(read_design(path), frames))) {
    expect_identical(replayed$ea_id, partial$ea_id)
    expect_identical(drawn_rows(replayed), drawn_rows(partial))
  }

  rest <- setdiff(1:12, order)
  complete <- partial
  for (t in rest) {
    complete <- execute(complete, occasion_listing(listing, master, t), period = 1, subround = t,
                        seed = 100 + t)
  }
  frames <- c(frames, lapply(rest, function(t) {
    occasion_listing(listing, master, t)
  }))
  write_design(complete, path, frame = frames)
  for (replayed in list(replay_design(complete, frames),
                        replay_design(read_design(path), frames))) {
    expect_identical(lapply(as.data.frame(replayed), identity),
                     lapply(as.data.frame(complete), identity))
  }
  expect_error(replay_design(read_design(path), frames[-2]),
               class = "samplyr_error_replay_frame_count")

  # An occasion of the completed sample replays through every occasion's call.
  month <- execute(complete, period = 1, subround = 9)
  write_design(month, path, frame = frames)
  receipt <- attr(read_design(path), "execution")
  expect_identical(receipt$transition, "occasion")
  expect_identical(
    vapply(receipt$earlier_calls, `[[`, "", "transition"),
    c("start", rep("occasion_draw", 12))
  )
  replayed <- replay_design(read_design(path), frames)
  expect_identical(lapply(as.data.frame(replayed), identity),
                   lapply(as.data.frame(month), identity))
})
