# on_empty: an empty random-size realization contributes zero, which keeps
# Horvitz-Thompson totals unbiased over repeated executions.

test_that("Bernoulli HT totals are unbiased under on_empty = 'silent'", {
  skip_on_cran()

  # N = 2, p = 0.1, y = c(1, 1): the true total is 2.
  frame <- data.frame(id = 1:2, y = c(1, 1))
  estimates <- vapply(seq_len(4000), function(seed) {
    s <- sampling_design() |>
      draw(frac = 0.1, method = "bernoulli", on_empty = "silent") |>
      execute(frame, seed = seed)
    sum(s$.weight * s$y)
  }, numeric(1))

  # MC se ~ 4.02 / sqrt(4000) ~ 0.064; the old bias was +1.62.
  expect_equal(mean(estimates), 2, tolerance = 0.15)
  # Empty realizations occur at the theoretical rate (1 - p)^N = 0.81
  expect_equal(mean(estimates == 0), 0.81, tolerance = 0.05)
})

test_that("PPS Poisson HT totals are unbiased under on_empty = 'warn'", {
  skip_on_cran()

  frame <- data.frame(id = 1:3, size = c(1, 2, 3), y = c(1, 1, 1))
  estimates <- vapply(seq_len(4000), function(seed) {
    s <- suppressWarnings(
      sampling_design() |>
        draw(frac = 0.15, method = "pps_poisson", mos = size,
             on_empty = "warn") |>
        execute(frame, seed = seed)
    )
    sum(s$.weight * s$y)
  }, numeric(1))

  expect_equal(mean(estimates), 3, tolerance = 0.25)
})

test_that("an empty realization is a well-formed tbl_sample", {
  frame <- data.frame(id = 1:5)
  # frac = 0.001 on N = 5: empty with probability ~0.995
  result <- sampling_design() |>
    draw(frac = 0.001, method = "bernoulli", on_empty = "silent") |>
    execute(frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_identical(nrow(result), 0L)
  expect_true(all(
    c(".weight", ".sample_id", ".stage", ".weight_1", ".fpc_1") %in%
      names(result)
  ))
  expect_identical(attr(result, "metadata")$n_selected, 0L)

  # Display paths tolerate the empty sample
  expect_no_warning(capture.output(print(result)))
  expect_no_error(capture.output(summary(result)))
})

test_that("an empty stratum leaves other strata untouched", {
  skip_on_cran()

  frame <- data.frame(
    id = seq_len(1005),
    g = rep(c("small", "big"), c(5, 1000))
  )
  found <- FALSE
  for (seed in 1:100) {
    s <- suppressWarnings(
      sampling_design() |>
        stratify_by(g) |>
        draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
        execute(frame, seed = seed)
    )
    if (!"small" %in% s$g && nrow(s) > 0) {
      # Nonempty strata keep the design weight 1 / frac
      expect_equal(unique(s$.weight), 20)
      found <- TRUE
      break
    }
  }
  expect_true(found, info = "Expected an empty small stratum within 100 seeds")
})

test_that("an empty first stage ends a multi-stage execution", {
  frame <- data.frame(psu = rep(1:5, each = 4), id = 1:20)
  result <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(frac = 0.001, method = "bernoulli", on_empty = "silent") |>
    add_stage() |>
    draw(n = 2) |>
    execute(frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_identical(nrow(result), 0L)
})

test_that("certainty units survive an empty probabilistic part", {
  skip_on_cran()

  frame <- data.frame(id = 1:50, size = c(rep(1, 49), 100000))
  found <- FALSE
  for (seed in 1:200) {
    s <- suppressWarnings(
      sampling_design() |>
        draw(frac = 0.03, method = "pps_poisson", mos = size,
             certainty_prop = 0.3, on_empty = "silent") |>
        execute(frame, seed = seed)
    )
    if (nrow(s) == 1) {
      expect_true(s$.certainty_1)
      expect_equal(s$.weight, 1)
      found <- TRUE
      break
    }
  }
  expect_true(found, info = "Expected a certainty-only realization within 200 seeds")
})

test_that("replicated executions record empty replicates", {
  frame <- data.frame(id = 1:5)
  result <- suppressWarnings(
    sampling_design() |>
      draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
      execute(frame, seed = 42, reps = 10)
  )
  counts <- attr(result, "metadata")$replicate_rows
  expect_length(counts, 10)
  expect_identical(sum(counts), nrow(result))
  expect_true(any(counts == 0L))
})

test_that("a stack is replicated even when its rows show one replicate or none", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:5)
  design <- sampling_design() |>
    draw(frac = 0.2, method = "bernoulli", on_empty = "silent")

  none <- sampling_design() |>
    draw(frac = 0.01, method = "bernoulli", on_empty = "silent") |>
    execute(frame, seed = 1, reps = 3)
  expect_identical(attr(none, "metadata")$replicate_rows, c(`1` = 0L, `2` = 0L, `3` = 0L))
  expect_true(has_multiple_replicates(none))

  one <- NULL
  for (seed in 1:200) {
    s <- execute(design, frame, seed = seed, reps = 3)
    if (nrow(s) > 0L && length(unique(s$.replicate)) == 1L) {
      one <- s
      break
    }
  }
  expect_false(is.null(one), info = "Expected one visible replicate within 200 seeds")
  expect_true(has_multiple_replicates(one))
  expect_true(any(cli::ansi_strip(format(one)) == "# Replicates:   3"))
  expect_null(summary(one)$weights)
  expect_null(summary(one)$design_effect)
  printed <- cli::ansi_strip(capture.output(print(summary(one))))
  expect_false(any(grepl("Kish DEFF", printed, fixed = TRUE)))
  expect_error(design_effect(one),
               class = "samplyr_error_replicated_sample_unsupported")
  expect_error(as_svydesign(one),
               class = "samplyr_error_replicated_sample_unsupported")
  expect_error(as_svydesign(one), "3 replicates")
})

test_that("advice on a stack names a replicate that has rows", {
  frame <- data.frame(id = 1:20, y = 1:20)
  design <- sampling_design() |>
    draw(frac = 0.01, method = "bernoulli", on_empty = "silent")
  none <- execute(design, frame, seed = 1, reps = 3)
  one <- execute(design, frame, seed = 3, reps = 3)
  expect_identical(attr(none, "metadata")$replicate_rows,
                   c(`1` = 0L, `2` = 0L, `3` = 0L))
  expect_identical(attr(one, "metadata")$replicate_rows,
                   c(`1` = 0L, `2` = 0L, `3` = 1L))

  expect_no_warning(printed <- capture.output(print(summary(none))))
  expect_false(any(grepl("filter(", printed, fixed = TRUE)))
  printed <- cli::ansi_strip(capture.output(print(summary(one))))
  expect_true(any(grepl("filter(.replicate == 3)", printed, fixed = TRUE)))

  err <- tryCatch(design_effect(one), error = identity)
  expect_s3_class(err, "samplyr_error_replicated_sample_unsupported")
  expect_match(cli::ansi_strip(conditionMessage(err)),
               "filter(.replicate == 3)", fixed = TRUE)
  err <- tryCatch(design_effect(none), error = identity)
  expect_s3_class(err, "samplyr_error_replicated_sample_unsupported")
  expect_match(conditionMessage(err), "Every replicate is empty")
})

test_that("one complete replicate filtered from the stack is a single sample", {
  skip_if_not_installed("survey")
  stack <- sampling_design() |>
    draw(n = 2) |>
    execute(data.frame(id = 1:5), seed = 1, reps = 3)
  one <- dplyr::filter(stack, .replicate == 2)
  expect_false(has_multiple_replicates(one))
  expect_s3_class(as_svydesign(one), "survey.design2")
})

test_that("a filter on .replicate extracts the one replicate with rows", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:20, y = 1:20)
  stack <- sampling_design() |>
    draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
    execute(frame, seed = 3, reps = 3)
  expect_identical(attr(stack, "metadata")$replicate_rows,
                   c(`1` = 0L, `2` = 0L, `3` = 2L))
  # The stack itself stays three replicates.
  expect_error(design_effect(stack),
               class = "samplyr_error_replicated_sample_unsupported")

  for (one in list(dplyr::filter(stack, .replicate == 3),
                   dplyr::filter(stack, .data$.replicate > 1),
                   dplyr::filter(stack, .replicate == 3, .by = id))) {
    expect_identical(nrow(one), 2L)
    expect_false(has_multiple_replicates(one))
    expect_type(design_effect(one), "double")
    expect_s3_class(as_svydesign(one), "survey.design")
    phase2 <- sampling_design() |> draw(n = 1) |> execute(one, seed = 5)
    expect_identical(nrow(phase2), 1L)
    expect_true(phase2$id %in% one$id)
  }

  # The same rows reached without naming .replicate keep the stack, so
  # the empty replicates still count.
  kept <- list(
    arrange = dplyr::arrange(stack, dplyr::desc(id)),
    permuted = stack[rev(seq_len(nrow(stack))), ],
    filter_true = dplyr::filter(stack, TRUE),
    filter_id = dplyr::filter(stack, id > 0),
    # Grouping by .replicate says how to evaluate, not what to select.
    by_replicate = dplyr::filter(stack, TRUE, .by = .replicate),
    id_by_replicate = dplyr::filter(stack, id > 0, .by = .replicate),
    slice = dplyr::slice(stack, seq_len(nrow(stack))),
    bracket = stack[stack$.replicate == 3, ]
  )
  for (nm in names(kept)) {
    expect_true(has_multiple_replicates(kept[[nm]]), info = nm)
    expect_error(as_svydesign(kept[[nm]]),
                 class = "samplyr_error_replicated_sample_unsupported",
                 info = nm)
  }

  # Part of that replicate, or an empty stack, is not one replicate.
  part <- dplyr::filter(stack, .replicate == 3, id == min(id))
  expect_error(as_svydesign(part), class = "samplyr_error_modified_sample")
  none <- sampling_design() |>
    draw(frac = 0.01, method = "bernoulli", on_empty = "silent") |>
    execute(frame, seed = 1, reps = 3)
  expect_true(has_multiple_replicates(dplyr::filter(none, .replicate == 1)))
})

test_that("custom random-size WOR methods honor on_empty", {
  on.exit(sondage::unregister_method("always_empty"), add = TRUE)
  sondage::register_method(
    "always_empty", "wor",
    sample_fn = function(pik, ...) integer(0),
    fixed_size = FALSE
  ,
    probabilities = "exact"
  )
  frame <- data.frame(id = 1:5, size = rep(1, 5))

  # Default errors, like the built-in random-size methods
  expect_error(
    sampling_design() |>
      draw(n = 2, method = "pps_always_empty", mos = size) |>
      execute(frame, seed = 1),
    "zero selections"
  )

  empty <- sampling_design() |>
    draw(n = 2, method = "pps_always_empty", mos = size,
         on_empty = "silent") |>
    execute(frame, seed = 1)
  expect_identical(nrow(empty), 0L)

  # Certainty path: the certainty unit survives an empty prob part
  cert_frame <- data.frame(id = 1:5, size = c(1, 1, 1, 1, 500))
  cert_only <- sampling_design() |>
    draw(n = 2, method = "pps_always_empty", mos = size,
         certainty_size = 100, on_empty = "silent") |>
    execute(cert_frame, seed = 1)
  expect_identical(nrow(cert_only), 1L)
  expect_true(cert_only$.certainty_1)
})

test_that("custom random-size balanced methods honor on_empty", {
  on.exit(sondage::unregister_method("always_empty"), add = TRUE)
  sondage::register_method(
    "always_empty", "balanced",
    sample_fn = function(pik, aux = NULL, ...) integer(0),
    fixed_size = FALSE,
    probabilities = "exact"
  )
  frame <- data.frame(id = 1:5, size = rep(1, 5))
  design <- function(...) {
    sampling_design() |>
      draw(n = 2, method = "balanced_always_empty", mos = size, ...)
  }

  expect_error(
    execute(design(), frame, seed = 1),
    class = "samplyr_error_empty_selection"
  )
  expect_warning(
    empty <- execute(design(on_empty = "warn"), frame, seed = 1),
    class = "samplyr_warning_empty_selection"
  )
  expect_identical(nrow(empty), 0L)
  expect_no_condition(
    empty <- execute(design(on_empty = "silent"), frame, seed = 1)
  )
  expect_identical(nrow(empty), 0L)
})

test_that("empty replicates block later phases with a contextual error", {
  frame <- data.frame(id = 1:5)
  phase1 <- suppressWarnings(
    sampling_design(title = "Screening phase") |>
      draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
      execute(frame, seed = 42, reps = 10)
  )
  counts <- attr(phase1, "metadata")$replicate_rows
  expect_true(any(counts == 0L))

  err <- expect_error(
    sampling_design() |> draw(n = 1) |> execute(phase1, seed = 1),
    class = "samplyr_error_empty_phase_replicate"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "phase-1")
  expect_match(msg, "phase 2")
  expect_match(msg, "bernoulli")
  expect_match(msg, "Screening")
  expect_match(msg, "bias simulation summaries")
})

test_that("continuing a stack with empty replicates errors, not skips", {
  ms <- data.frame(psu = rep(1:5, each = 4), id = 1:20)
  st1 <- suppressWarnings(
    sampling_design() |>
      add_stage("PSUs") |>
      cluster_by(psu) |>
      draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
      add_stage("Units") |>
      draw(n = 2) |>
      execute(ms, stages = 1, seed = 7, reps = 6)
  )
  expect_true(any(attr(st1, "metadata")$replicate_rows == 0L))

  expect_error(
    execute(st1, ms, seed = 2),
    class = "samplyr_error_empty_phase_replicate"
  )
})

test_that("an extracted nonempty replicate can enter a later phase", {
  frame <- data.frame(id = 1:5)
  phase1 <- suppressWarnings(
    sampling_design() |>
      draw(frac = 0.05, method = "bernoulli", on_empty = "silent") |>
      execute(frame, seed = 1, reps = 10)
  )
  counts <- attr(phase1, "metadata")$replicate_rows
  nonempty <- as.integer(names(counts)[counts > 0])
  extracted <- dplyr::filter(phase1, .replicate == nonempty[1])
  phase2 <- sampling_design() |> draw(n = 1) |> execute(extracted, seed = 5)
  expect_s3_class(phase2, "tbl_sample")
  expect_gt(nrow(phase2), 0)
})

test_that("replicated stacks without empty replicates flow to later phases", {
  frame <- data.frame(id = 1:5)
  phase1 <- sampling_design() |>
    draw(n = 3) |>
    execute(frame, seed = 1, reps = 4)
  phase2 <- sampling_design() |> draw(n = 1) |> execute(phase1, seed = 3)
  expect_identical(length(unique(phase2$.replicate)), 4L)
})

## The pools a random-size draw left empty

# The pools of a stage that the sample reached, from the frame and the
# sample alone: those of the frame no selected row belongs to.
missed_pools <- function(frame, sample, vars) {
  pools <- unique(frame[vars])
  taken <- unique(as.data.frame(sample)[vars])
  key <- function(d) do.call(paste, c(unname(as.list(d)), sep = "\r"))
  missed <- pools[!key(pools) %in% key(taken), , drop = FALSE]
  missed <- missed[do.call(order, unname(as.list(missed))), , drop = FALSE]
  rownames(missed) <- NULL
  missed
}
record_keys <- function(r, vars) {
  keys <- r$keys[vars]
  keys <- keys[do.call(order, unname(as.list(keys))), , drop = FALSE]
  rownames(keys) <- NULL
  keys
}

test_that("a random-size draw that selected nothing in a pool is recorded", {
  frame <- data.frame(st = c("a", rep("b", 4)), id = 1:5,
                      mos = c(100, 1, 1, 1, 1))
  fr <- data.frame(st = c("a", "b"), frac = c(1, 0.1))
  bernoulli <- sampling_design() |> stratify_by(st) |>
    draw(frac = fr, method = "bernoulli", on_empty = "silent")
  seed <- Find(function(s) {
    all(execute(bernoulli, frame, seed = s)$st == "a")
  }, 1:50)
  expect_false(is.null(seed))
  records <- lapply(c("summary", "full", "none"), function(digest) {
    attr(execute(bernoulli, frame, seed = seed, frame_digest = digest),
         "metadata")$empty_pools
  })
  # The record does not depend on the digest.
  expect_identical(records[[2]], records[[1]])
  expect_identical(records[[3]], records[[1]])
  r <- records[[1]]
  expect_length(r, 1L)
  expect_identical(r[[1]]$stage, 1L)
  expect_identical(r[[1]]$keys, data.frame(st = "b"))
  expect_identical(r[[1]]$N, 4L)
  expect_null(r[[1]]$rows)

  # A fixed-size stage takes a unit in every pool, so it records none.
  srs <- execute(sampling_design() |> stratify_by(st) |> draw(n = 1),
                 frame, seed = 1)
  expect_identical(attr(srs, "metadata")$empty_pools, list())
  # Poisson proportional to size and a custom random-size method.
  on.exit(sondage::unregister_method("rs_custom"), add = TRUE)
  sondage::register_method(
    "rs_custom", "wor",
    sample_fn = function(pik, n = NULL, prn = NULL, ...) {
      which(stats::runif(length(pik)) < pik)
    },
    fixed_size = FALSE, probabilities = "exact"
  )
  sizes <- data.frame(st = c("a", "b"), n = c(1, 0.1))
  for (method in c("pps_poisson", "pps_rs_custom")) {
    design <- sampling_design() |> stratify_by(st) |>
      draw(n = sizes, method = method, mos = mos, on_empty = "silent")
    s <- execute(design, frame, seed = 1)
    expect_identical(
      lapply(attr(s, "metadata")$empty_pools, `[[`, "keys"),
      if (any(s$st == "b")) list() else list(data.frame(st = "b")),
      info = method
    )
  }
})

test_that("an empty pool below stage 1 keeps its parent's rows", {
  psu <- data.frame(psu = 1:4)
  hh <- data.frame(psu = rep(1:4, each = 4), st = rep(c("a", "b"), 8),
                   id = 1:16, y = 1)
  design <- sampling_design() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> stratify_by(st) |>
    draw(frac = 0.3, method = "bernoulli", on_empty = "silent")
  reached <- FALSE
  for (seed in 1:20) {
    s <- execute(design, psu, hh, seed = seed)
    r <- attr(s, "metadata")$empty_pools
    if (nrow(s) == 0L) next
    # The selected clusters, those emptied whole included, from stage 1.
    first <- execute(design, psu, seed = seed, stages = 1)
    want <- missed_pools(hh[hh$psu %in% first$psu, ], s, c("psu", "st"))
    if (nrow(want) == 0L) {
      expect_identical(r, list(), info = paste(seed))
      next
    }
    reached <- TRUE
    expect_length(r, 1L)
    expect_identical(r[[1]]$stage, 2L)
    expect_identical(record_keys(r[[1]], c("psu", "st")), want,
                     info = paste(seed))
    expect_identical(r[[1]]$N, rep(2L, nrow(want)), info = paste(seed))
    expect_setequal(r[[1]]$rows$psu, unique(want$psu))
  }
  expect_true(reached)

  # Below a stage drawn with replacement, a pool is one hit of its parent.
  wr <- sampling_design() |> cluster_by(psu) |> draw(n = 4, method = "srswr") |>
    add_stage() |> draw(frac = 0.2, method = "bernoulli", on_empty = "silent")
  reached <- FALSE
  for (seed in 1:30) {
    first <- execute(wr, psu, seed = seed, stages = 1)
    s <- execute(first, hh, seed = seed + 100)
    r <- attr(s, "metadata")$empty_pools
    hits <- unique(as.data.frame(first)[c("psu", ".draw_1")])
    taken <- unique(as.data.frame(s)[c("psu", ".draw_1")])
    want <- missed_pools(hits, taken, c("psu", ".draw_1"))
    if (nrow(want) == 0L || nrow(s) == 0L) next
    reached <- TRUE
    expect_identical(record_keys(r[[1]], c("psu", ".draw_1")), want,
                     info = paste(seed))
    expect_identical(nrow(r[[1]]$rows), nrow(want), info = paste(seed))
  }
  expect_true(reached)
})

test_that("the empty-pool record follows the sample wherever it goes", {
  frame <- data.frame(st = rep(c("a", "b"), each = 4), id = 1:8)
  design <- sampling_design() |> stratify_by(st) |>
    draw(frac = 0.15, method = "bernoulli", on_empty = "silent")
  # Replicates: each record names its replicate, and an extracted
  # replicate keeps its own.
  s <- execute(design, frame, seed = 3, reps = 4)
  r <- attr(s, "metadata")$empty_pools
  for (k in 1:4) {
    mine <- Filter(function(e) identical(e$replicate, k), r)
    rows <- as.data.frame(s)[s$.replicate == k, ]
    want <- missed_pools(frame, rows, "st")
    got <- if (length(mine)) {
      record_keys(mine[[1]], "st")
    } else {
      want[0, , drop = FALSE]
    }
    expect_identical(got, want, info = paste(k))
  }
  expect_true(length(r) > 0L)
  # A replicate that drew nothing has no row to name it, so it is passed.
  for (k in 1:4) {
    one <- samplyr:::normalize_extracted_replicate(
      dplyr::filter(s, .data$.replicate == k), r = k
    )
    mine <- Filter(function(e) identical(e$replicate, k), r)
    expect_identical(attr(one, "metadata")$empty_pools, mine,
                     info = paste(k))
  }

  # Continuation keeps stage 1's record and adds stage 2's.
  psu <- data.frame(psu = 1:3, st = c("a", "a", "b"))
  hh <- data.frame(psu = rep(1:3, each = 3), id = 1:9)
  two <- sampling_design() |> stratify_by(st) |> cluster_by(psu) |>
    draw(frac = 0.4, method = "bernoulli", on_empty = "silent") |>
    add_stage() |> draw(frac = 0.3, method = "bernoulli", on_empty = "silent")
  seed <- Find(function(k) {
    first <- execute(two, psu, seed = k, stages = 1)
    nrow(first) > 0L && length(attr(first, "metadata")$empty_pools) > 0L
  }, 1:60)
  expect_false(is.null(seed))
  first <- execute(two, psu, seed = seed, stages = 1)
  later <- execute(first, hh, seed = 1)
  stages <- vapply(attr(later, "metadata")$empty_pools, `[[`, 1L, "stage")
  expect_identical(attr(later, "metadata")$empty_pools[stages == 1L],
                   attr(first, "metadata")$empty_pools)
  # A sample without the record leaves it unknown.
  unknown <- first
  attr(unknown, "metadata")$empty_pools <- NULL
  expect_null(attr(execute(unknown, hh, seed = 1), "metadata")$empty_pools)

  # Saved and read back, and replayed, the record is the same.
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(later, path)
  expect_identical(attr(readRDS(path), "metadata")$empty_pools,
                   attr(later, "metadata")$empty_pools)
  replayed <- replay_design(s, frame)
  expect_identical(attr(replayed, "metadata")$empty_pools, r)
})
