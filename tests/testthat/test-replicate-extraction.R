## A replicate extracted from a stack, then settled, shared, continued or saved

extraction_frame <- function() data.frame(id = 1:20, y = 1:20)

listed_stack <- function() {
  sampling_design() |>
    draw(n = 3, reserve = 2) |>
    execute(extraction_frame(), seed = 1, reps = 2)
}

# The take responds, and the reserve is not released.
code_take <- function(sample, k = 1L) {
  sample$code <- ifelse(sample[[paste0(".reserve_", k)]] == 0L, 1, NA)
  sample
}

# The same list drawn alone, at the seed its replicate was drawn with.
listed_alone <- function(stack, r) {
  seed <- attr(stack, "metadata")$replicate_seeds[[r]]
  sampling_design() |>
    draw(n = 3, reserve = 2) |>
    execute(extraction_frame(), seed = seed)
}

total_se <- function(svy) {
  est <- survey::svytotal(~y, svy, na.rm = TRUE)
  c(total = unname(stats::coef(est)), se = unname(survey::SE(est)))
}

replay_both_ways <- function(sample, frame, ...) {
  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = frame)
  list(
    file = replay_design(read_design(path), frame, ...),
    memory = replay_design(sample, frame, ...)
  )
}

## One replicate after settlement or a weight share

test_that("a settled replicate is one sample for both exports", {
  skip_if_not_installed("survey")
  stack <- listed_stack()
  for (r in 1:2) {
    settled <- execute(
      code_take(dplyr::filter(stack, .replicate == r)),
      response_status = code
    )
    alone <- execute(code_take(listed_alone(stack, r)), response_status = code)
    expect_identical(settled$id, alone$id)
    expect_identical(settled$.weight, alone$.weight)
    expect_identical(unique(settled$.replicate), r)
    expect_equal(total_se(as_svydesign(settled)), total_se(as_svydesign(alone)))
    expect_equal(
      total_se(as_svrepdesign(settled, type = "JK1")),
      total_se(as_svrepdesign(alone, type = "JK1"))
    )
  }
})

test_that("a settled replicate keeps its stack ids, in any row order", {
  stack <- listed_stack()
  one <- dplyr::filter(stack, .replicate == 2)
  settled <- execute(code_take(one), response_status = code)
  # The settled rows keep the positions they held in the stack.
  expect_identical(settled$.sample_id, one$.sample_id[one$.reserve_1 == 0L])

  reordered <- dplyr::arrange(one, dplyr::desc(id))
  settled_reordered <- execute(code_take(reordered), response_status = code)
  expect_identical(sort(settled_reordered$id), sort(settled$id))
  expect_identical(
    settled_reordered$.weight[order(settled_reordered$id)],
    settled$.weight[order(settled$id)]
  )
})

test_that("the nonempty replicate of a stack with empty siblings settles", {
  skip_if_not_installed("survey")
  frame <- data.frame(ea = rep(1:20, each = 5), hh = 1:100, y = 1)
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |>
    draw(frac = 0.06, method = "bernoulli", on_empty = "silent") |>
    add_stage() |> draw(n = 2, reserve = 1)
  stack <- suppressWarnings(suppressMessages(
    execute(design, frame, seed = 5, reps = 3)
  ))
  # Replicates 2 and 3 are empty, and replicate 1 holds EAs 2 and 20.
  expect_identical(unique(stack$.replicate), 1L)
  expect_identical(sort(unique(stack$ea)), c(2L, 20L))
  # The stack itself stays a stack of three replicates.
  expect_error(
    execute(code_take(stack, 2L), response_status = code),
    class = "samplyr_error_replicated_sample_unsupported"
  )
  settled <- execute(
    code_take(dplyr::filter(stack, .replicate == 1), 2L),
    response_status = code
  )
  expect_no_error(suppressWarnings(as_svydesign(settled)))
})

test_that("a replicate shared to its targets exports like a lone sample", {
  skip_if_not_installed("survey")
  frame <- data.frame(unit = letters[1:8])
  targets <- data.frame(tid = paste0("t", 1:8), hh = paste0("H", 1:8), y = 1:8)
  links <- data.frame(unit = letters[1:8], tid = paste0("t", 1:8))
  design <- sampling_design() |> draw(n = 3)
  stack <- execute(design, frame, seed = 1, reps = 2)
  alone <- execute(
    design, frame, seed = attr(stack, "metadata")$replicate_seeds[[2]]
  )
  share <- function(x) {
    share_weights(
      x, targets = targets, links = links,
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = hh, multiplicity = complete_links()
    )
  }
  shared <- share(dplyr::filter(stack, .replicate == 2))
  reference <- share(alone)
  expect_identical(shared$tid, reference$tid)
  expect_identical(shared$.weight, reference$.weight)
  expect_equal(total_se(as_svydesign(shared)), total_se(as_svydesign(reference)))
  expect_equal(
    total_se(as_svrepdesign(shared, type = "JK1")),
    total_se(as_svrepdesign(reference, type = "JK1"))
  )
})

test_that("a replicate continued to stage 2 carries only its own digest", {
  frame <- data.frame(ea = rep(1:20, each = 5), hh = 1:100, y = (1:100) %% 7)
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 4) |>
    add_stage() |> draw(n = 2)
  stack <- execute(design, frame, stages = 1, seed = 1, reps = 2)
  one <- dplyr::filter(stack, .replicate == 2)
  continued <- execute(one, frame, seed = 2)
  digest <- get_frame_digest(continued)
  expect_identical(nrow(digest$stages[[1]]$selected), 4L)

  seed <- attr(stack, "metadata")$replicate_seeds[[2]]
  alone <- execute(design, frame, stages = 1, seed = seed)
  continued_alone <- execute(alone, frame, seed = 2)
  expect_identical(continued$hh, continued_alone$hh)
  expect_equal(frame_summary(continued), frame_summary(continued_alone))
})

test_that("extractions that are no replicate stay refused", {
  skip_if_not_installed("survey")
  stack <- listed_stack()
  one <- dplyr::filter(stack, .replicate == 2)
  # A subset of a replicate.
  expect_error(
    execute(code_take(one[-1, ]), response_status = code),
    class = "samplyr_error_modified_sample"
  )
  # A duplicated row.
  expect_error(
    execute(code_take(one[c(1, seq_len(nrow(one))), ]), response_status = code),
    class = "samplyr_error_modified_sample"
  )
  # An edited release rank.
  edited <- one
  edited$.reserve_1[1] <- 4L
  expect_error(
    execute(code_take(edited), response_status = code),
    class = "samplyr_error_modified_sample"
  )
  # An edit after settlement.
  settled <- execute(code_take(one), response_status = code)
  settled$.weight[1] <- 2 * settled$.weight[1]
  expect_error(as_svydesign(settled), class = "samplyr_error_modified_sample")
})

## Receipts of one replicate

test_that("a receipt of one replicate replays that replicate", {
  frame <- extraction_frame()
  stack <- sampling_design() |> draw(n = 3) |> execute(frame, seed = 1, reps = 2)
  expected <- list(c(20L, 12L, 5L), c(16L, 14L, 10L))
  for (r in 1:2) {
    one <- dplyr::filter(stack, .replicate == r)
    expect_identical(one$id, expected[[r]])
    replayed <- expect_no_warning(replay_both_ways(one, frame))
    for (z in replayed) {
      expect_identical(z$id, one$id)
      expect_identical(z$.replicate, one$.replicate)
      expect_identical(z$.sample_id, one$.sample_id)
      expect_identical(z$.weight, one$.weight)
    }
  }
  # The whole stack still replays as the whole stack.
  path <- withr::local_tempfile(fileext = ".json")
  write_design(stack, path, frame = frame)
  expect_identical(replay_design(read_design(path), frame)$id, stack$id)
})

test_that("a receipt of a reordered replicate replays its units", {
  frame <- extraction_frame()
  stack <- sampling_design() |> draw(n = 3) |> execute(frame, seed = 1, reps = 2)
  one <- dplyr::arrange(dplyr::filter(stack, .replicate == 2), id)
  for (z in replay_both_ways(one, frame)) {
    expect_identical(sort(z$id), sort(one$id))
    expect_identical(z$.weight[order(z$id)], one$.weight[order(one$id)])
  }
})

test_that("a receipt of a settled replicate replays the settled sample", {
  skip_if_not_installed("survey")
  frame <- extraction_frame()
  stack <- listed_stack()
  for (r in 1:2) {
    one <- code_take(dplyr::filter(stack, .replicate == r))
    # One main unit is ineligible, so the reserve is released.
    one$code[which(one$.reserve_1 == 0L)[1]] <- 4
    one$code[one$.reserve_1 == 1L] <- 1
    settled <- execute(one, response_status = code)
    for (z in replay_both_ways(settled, frame)) {
      expect_identical(z$id, settled$id)
      expect_identical(z$.weight, settled$.weight)
      expect_equal(total_se(as_svydesign(z)), total_se(as_svydesign(settled)))
    }
  }
})

test_that("a receipt of a shared replicate replays the share", {
  frame <- data.frame(unit = letters[1:8])
  targets <- data.frame(tid = paste0("t", 1:8), hh = paste0("H", 1:8))
  links <- data.frame(unit = letters[1:8], tid = paste0("t", 1:8))
  stack <- sampling_design() |> draw(n = 3) |> execute(frame, seed = 1, reps = 2)
  shared <- share_weights(
    dplyr::filter(stack, .replicate == 2),
    targets = targets, links = links,
    by = c(unit = "unit"), to = c(tid = "tid"),
    within = hh, multiplicity = complete_links()
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_design(shared, path, frame = frame)
  replayed <- replay_design(
    read_design(path), frame, links = links, targets = targets
  )
  expect_identical(replayed$tid, shared$tid)
  expect_identical(replayed$.weight, shared$.weight)
})

test_that("a receipt replays a continuation or a phase drawn from one replicate", {
  frame <- data.frame(ea = rep(1:20, each = 5), hh = 1:100, y = (1:100) %% 7)
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 4) |>
    add_stage() |> draw(n = 2)
  stack <- execute(design, frame, stages = 1, seed = 1, reps = 2)
  one <- dplyr::filter(stack, .replicate == 2)

  continued <- execute(one, frame, seed = 2)
  expect_identical(nrow(continued), 8L)
  for (z in expect_no_warning(replay_both_ways(continued, frame))) {
    expect_identical(z$hh, continued$hh)
    expect_identical(z$.weight, continued$.weight)
  }

  phase2 <- sampling_design() |> draw(n = 4) |> execute(one, seed = 3)
  expect_identical(nrow(phase2), 4L)
  for (z in expect_no_warning(replay_both_ways(phase2, frame))) {
    expect_identical(z$hh, phase2$hh)
    expect_identical(z$.weight, phase2$.weight)
  }
})

test_that("a replicate extracted after a replicated continuation replays", {
  frame <- data.frame(ea = rep(1:20, each = 5), hh = 1:100, y = (1:100) %% 7)
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 4) |>
    add_stage() |> draw(n = 2)
  stack <- execute(design, frame, stages = 1, seed = 1, reps = 2)
  continued <- execute(stack, frame, seed = 2)
  one <- dplyr::filter(continued, .replicate == 2)
  for (z in expect_no_warning(replay_both_ways(one, frame))) {
    expect_identical(z$hh, one$hh)
    expect_identical(z$.replicate, one$.replicate)
    expect_identical(z$.weight, one$.weight)
  }
})

test_that("a receipt of one replicate replays under every digest mode", {
  frame <- extraction_frame()
  for (mode in c("none", "summary", "full")) {
    stack <- sampling_design() |>
      draw(n = 3, method = "pps_brewer", mos = y) |>
      execute(frame, seed = 1, reps = 2, frame_digest = mode)
    one <- dplyr::filter(stack, .replicate == 2)
    for (z in expect_no_warning(replay_both_ways(one, frame))) {
      expect_identical(z$id, one$id, label = mode)
      expect_identical(z$.weight, one$.weight, label = mode)
    }
  }
})

test_that("a receipt of one replicate still reports a changed frame", {
  frame <- extraction_frame()
  stack <- sampling_design() |> draw(n = 3) |> execute(frame, seed = 1, reps = 2)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(dplyr::filter(stack, .replicate == 2), path, frame = frame)
  changed <- frame[-1, ]
  expect_error(
    replay_design(read_design(path), changed),
    class = "samplyr_error_replay_frame_mismatch"
  )
})

test_that("a receipt that selects a replicate it cannot hold is refused", {
  frame <- extraction_frame()
  stack <- sampling_design() |> draw(n = 3) |> execute(frame, seed = 1, reps = 2)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(dplyr::filter(stack, .replicate == 2), path, frame = frame)
  text <- readLines(path)
  # The execution's own fields, at their indentation.
  at <- which(text == '    "replicate": 2,')
  expect_length(at, 1L)

  beyond <- text
  beyond[at] <- '    "replicate": 5,'
  writeLines(beyond, path)
  expect_error(
    replay_design(read_design(path), frame),
    class = "samplyr_error_replay_replicate"
  )

  writeLines(text[text != '    "reps": 2,'], path)
  expect_error(read_design(path), class = "samplyr_error_design_file_malformed")
})

test_that("replicates continued from one replicate replay that replicate", {
  frame <- expand.grid(hh = 1:12, ea = 1:15)
  frame$id <- seq_len(nrow(frame))
  frames <- list(frame, frame)
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 6) |>
    add_stage() |> draw(n = 4)
  stack <- execute(design, frame, stages = 1, seed = 1, reps = 3)
  for (parent in c(1L, 3L)) {
    one <- dplyr::filter(stack, .replicate == parent)
    continued <- execute(one, frame, seed = 2, reps = 2)
    expect_identical(sort(unique(continued$ea)), sort(unique(one$ea)))
    for (s in list(continued, dplyr::filter(continued, .replicate == 2))) {
      for (z in expect_no_warning(replay_both_ways(s, frames))) {
        expect_identical(z$id, s$id)
        expect_identical(z$ea, s$ea)
        expect_identical(z$.replicate, s$.replicate)
        expect_identical(z$.sample_id, s$.sample_id)
        expect_identical(z$.weight, s$.weight)
      }
    }
  }
})

test_that("the digest and summaries of one replicate describe that replicate", {
  frame <- data.frame(ea = rep(1:10, each = 4), hh = 1:40, y = 1:40)
  stack <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 3) |>
    execute(frame, seed = 1, reps = 2, frame_digest = "full")
  one <- dplyr::filter(stack, .replicate == 2)
  replayed <- replay_design(one, frame)
  expect_identical(nrow(get_frame_digest(one)$stages[[1]]$selected), 3L)
  for (detail in c("stage", "pool", "unit")) {
    expect_identical(
      frame_summary(one, detail = detail),
      frame_summary(replayed, detail = detail),
      label = detail
    )
  }
  expect_identical(sum(frame_summary(one, detail = "unit")$is_selected), 3L)

  # A random-size draw realizes its own count in each replicate.
  elements <- data.frame(id = 1:20, y = 1:20)
  stack <- sampling_design() |>
    draw(frac = 0.25, method = "bernoulli") |>
    execute(elements, seed = 1, reps = 2)
  one <- dplyr::filter(stack, .replicate == 2)
  expect_identical(frame_summary(one)$n_realized, as.double(nrow(one)))
  pools <- frame_summary(one, detail = "pool")
  expect_identical(nrow(pools), 1L)
  expect_identical(pools$n_realized, as.double(nrow(one)))
  for (detail in c("stage", "pool")) {
    expect_identical(
      frame_summary(one, detail = detail),
      frame_summary(replay_design(one, elements), detail = detail)
    )
  }

  # A receipt of the replicate reads back as one replicate too.
  path <- withr::local_tempfile(fileext = ".json")
  write_design(one, path, frame = elements)
  restored <- frame_summary(read_design(path), detail = "pool")
  expect_identical(nrow(restored), 1L)
  expect_identical(restored$n_realized, as.double(nrow(one)))
})

test_that("the certainty count of one replicate counts its own rows", {
  frame <- data.frame(id = 1:20, mos = c(100, 5, rep(1, 18)), y = 1:20)
  stack <- suppressMessages(
    sampling_design() |>
      draw(n = 3, method = "pps_brewer", mos = mos) |>
      execute(frame, seed = 1, reps = 2, frame_digest = "summary")
  )
  expect_identical(frame_summary(stack)$n_certainty, 1)
  for (r in 1:2) {
    one <- dplyr::filter(stack, .replicate == r)
    expect_identical(sum(one$.certainty_1), 1L)
    expect_identical(frame_summary(one)$n_certainty, 1, label = r)
    replayed <- suppressMessages(replay_design(one, frame))
    expect_identical(frame_summary(replayed)$n_certainty, 1, label = r)
  }
})

test_that("a settled replicate with no row left keeps its empty parents", {
  skip_if_not_installed("survey")
  primary <- data.frame(ea = 1:6)
  listing <- data.frame(ea = rep(5:6, each = 5), hh = 1:10, y = 1:10)
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 2) |>
    add_stage() |> draw(n = 2, reserve = 1, on_empty = "silent")
  stack <- execute(design, list(primary, listing), seed = 2, reps = 2)
  seed <- attr(stack, "metadata")$replicate_seeds[[2]]
  alone <- execute(design, list(primary, listing), seed = seed)
  # Replicate 2 draws an EA with no households and EA 6, where nobody
  # responds.
  one <- dplyr::filter(stack, .replicate == 2)
  expect_identical(unique(one$ea), 6L)
  one$code <- 2
  alone$code <- 2
  settled <- suppressWarnings(execute(one, response_status = code))
  settled_alone <- suppressWarnings(execute(alone, response_status = code))
  expect_identical(nrow(settled), 0L)
  expect_length(sample_empty_parents(settled), 1L)
  svy <- suppressWarnings(as_svydesign(settled))
  svy_alone <- suppressWarnings(as_svydesign(settled_alone))
  expect_identical(nrow(svy$variables), nrow(svy_alone$variables))
  expect_equal(unname(weights(svy)), unname(weights(svy_alone)))
})

test_that("the header of one replicate flags only real edits", {
  frame <- extraction_frame()
  stack <- sampling_design() |> draw(n = 6) |> execute(frame, seed = 1, reps = 2)
  one <- dplyr::filter(stack, .replicate == 2)
  # The log keeps the row change the replicate counters read.
  expect_identical(sample_modifications(one), "rows")
  expect_false("Modified" %in% names(tbl_sum(one)))
  expect_identical(
    names(tbl_sum(one)),
    names(tbl_sum(replay_design(one, frame)))
  )
  expect_identical(tbl_sum(dplyr::filter(one, y > 5))[["Modified"]], "rows")
  expect_identical(
    tbl_sum(dplyr::select(one, -.weight_1))[["Modified"]],
    "rows, columns"
  )

  # After a replay or a settlement the replicate is a sample of its own,
  # and a row dropped from it is flagged as from any sample.
  replayed <- replay_design(one, frame)
  expect_false("Modified" %in% names(tbl_sum(replayed)))
  expect_identical(tbl_sum(replayed[-1, ])[["Modified"]], "rows")
  settled <- execute(
    code_take(dplyr::filter(listed_stack(), .replicate == 2)),
    response_status = code
  )
  expect_false("Modified" %in% names(tbl_sum(settled)))
  expect_identical(tbl_sum(settled[-1, ])[["Modified"]], "rows")
})

test_that("a replicate replayed empty on another frame is an empty sample", {
  design <- sampling_design() |>
    draw(frac = 0.1, method = "bernoulli", on_empty = "silent")
  frame <- data.frame(id = 1:40, y = 1:40)
  smaller <- frame[1:5, ]
  stack <- suppressWarnings(execute(design, frame, seed = 1, reps = 3))
  one <- dplyr::filter(stack, .replicate == 2)
  expect_gt(nrow(one), 0L)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(one, path, frame = frame)
  expect_error(
    replay_design(read_design(path), smaller),
    class = "samplyr_error_replay_frame_mismatch"
  )

  # On the smaller frame replicate 2 draws nothing while replicate 3 draws.
  expect_identical(
    unname(attr(
      suppressWarnings(execute(design, smaller, seed = 1, reps = 3)),
      "metadata"
    )$replicate_rows),
    c(0L, 0L, 1L)
  )
  replayed <- suppressWarnings(
    replay_design(read_design(path), smaller, fingerprint = "warn")
  )
  seed <- attr(stack, "metadata")$replicate_seeds[[2]]
  alone <- suppressWarnings(execute(design, smaller, seed = seed))
  expect_identical(nrow(replayed), 0L)
  expect_null(attr(replayed, "metadata")$reps)
  expect_false("Modified" %in% names(tbl_sum(replayed)))
  expect_identical(frame_summary(replayed), frame_summary(alone))
})
