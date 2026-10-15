test_that("trace-disabled selection preserves the seeded sample", {
  frame <- data.frame(
    id = seq_len(120),
    stratum = rep(letters[1:4], each = 30)
  )
  design <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 24)
  spec <- design$stages[[1L]]

  full <- withr::with_seed(
    91,
    samplyr:::sample_units(
      frame,
      spec$strata,
      spec$draw_spec,
      trace_mode = "full"
    )
  )
  none <- withr::with_seed(
    91,
    samplyr:::sample_units(
      frame,
      spec$strata,
      spec$draw_spec,
      trace_mode = "none"
    )
  )

  expect_identical(none$sample, full$sample)
  expect_null(none$trace)
  expect_type(full$trace, "list")
})

test_that("frame_digest mode does not affect selection", {
  frame <- data.frame(
    id = seq_len(200),
    stratum = rep(letters[1:10], each = 20)
  )
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 4)

  none <- execute(design, frame, seed = 11, frame_digest = "none")
  summary <- execute(design, frame, seed = 11, frame_digest = "summary")

  expect_identical(names(none), names(summary))
  for (column in names(none)) {
    expect_identical(none[[column]], summary[[column]])
  }
  expect_null(get_frame_digest(none))
  expect_type(get_frame_digest(summary), "list")
})

test_that("grouped SRS fast path matches direct sample.int draws", {
  frame <- data.frame(id = seq_len(15))
  indices <- list(1:5, 6:9, 10:15)
  n_per_group <- c(2L, 4L, 3L)

  expected <- withr::with_seed(72, {
    unlist(Map(
      function(rows, n) rows[sample.int(length(rows), n)],
      indices,
      n_per_group
    ), use.names = FALSE)
  })
  actual <- withr::with_seed(
    72,
    samplyr:::sample_srswor_by_group_indices(
      frame,
      indices,
      n_per_group,
      trace_mode = "none"
    )
  )

  expect_identical(actual$sample$id, expected)
  expect_equal(actual$sample$.weight, c(rep(2.5, 2), rep(1, 4), rep(2, 3)))
  expect_equal(actual$sample$.fpc, c(rep(5, 2), rep(4, 4), rep(6, 3)))
  expect_null(actual$leaves)
})

test_that("summary traces compact constant chances", {
  frame <- data.frame(
    id = seq_len(40),
    stratum = rep(c("a", "b"), each = 20)
  )
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5)
  spec <- design$stages[[1L]]

  result <- withr::with_seed(
    3,
    samplyr:::sample_units(
      frame,
      spec$strata,
      spec$draw_spec,
      trace_mode = "summary"
    )
  )

  chances <- lapply(result$trace$groups, function(group) group$node$chance)
  expect_true(all(lengths(chances) == 1L))
  expect_equal(unlist(chances), c(0.25, 0.25))
})

test_that("summary digest expands compact chances into correct totals", {
  frame <- data.frame(
    id = seq_len(60),
    stratum = rep(letters[1:3], each = 20)
  )
  sample <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5) |>
    execute(frame, seed = 8, frame_digest = "summary")

  pools <- get_frame_digest(sample)$stages[[1L]]$pools

  expect_equal(pools$N, rep(20L, 3))
  expect_equal(pools$n_expected, rep(5, 3))
  expect_equal(pools$chance, rep(0.25, 3))
})

test_that("first rows of row groups are the first index of each group", {
  groups <- list(c(4L, 1L), 7L, c(2L, 3L, 9L))
  expect_identical(
    samplyr:::first_row_indices_by_group(groups),
    c(4L, 7L, 2L)
  )
  names(groups) <- c("a", "b", "c")
  expect_identical(
    samplyr:::first_row_indices_by_group(groups),
    c(a = 4L, b = 7L, c = 2L)
  )
  expect_identical(samplyr:::first_row_indices_by_group(list()), integer(0))
  expect_error(
    samplyr:::first_row_indices_by_group(list(1L, integer(0))),
    class = "samplyr_error_internal"
  )
})

test_that("replicate row locations match subsetting by equality", {
  replicate <- c(2L, 1L, NA, 2L, 3L, 1L)
  ids <- 1:3
  expect_identical(
    samplyr:::replicate_row_locations(replicate, ids),
    lapply(ids, function(r) seq_along(replicate)[replicate == r])
  )
  expect_identical(
    samplyr:::replicate_row_locations(replicate, ids),
    list(c(2L, NA, 6L), c(1L, NA, 4L), c(NA, 5L))
  )
  expect_identical(
    samplyr:::replicate_row_locations(c(1L, 1L), 1:2),
    list(1:2, integer(0))
  )
})

test_that("stacking replicates matches rbind and refuses differing columns", {
  lv <- c("b", "a")
  blocks <- list(
    data.frame(x = integer(0), f = factor(character(0), levels = lv)),
    data.frame(x = 1L, f = factor("a", levels = lv)),
    data.frame(f = factor("b", levels = lv), x = 2L)
  )
  expect_identical(
    samplyr:::stack_replicates(blocks),
    do.call(rbind, blocks)
  )
  # A replicate that ended early has no rows and fewer columns.
  short <- data.frame(x = integer(0))
  expect_identical(
    samplyr:::stack_replicates(list(short, blocks[[2]], short)),
    do.call(rbind, list(short, blocks[[2]], short))
  )
  expect_identical(
    samplyr:::stack_replicates(list(blocks[[1]], short)),
    do.call(rbind, list(blocks[[1]], short))
  )
  expect_error(
    samplyr:::stack_replicates(list(blocks[[2]], data.frame(x = 3L))),
    class = "samplyr_error_internal"
  )
  expect_error(
    samplyr:::stack_replicates(list(blocks[[2]], data.frame(x = 3L, g = 1))),
    class = "samplyr_error_internal"
  )
})

test_that("the replicate memo reuses only bitwise identical calls", {
  calls <- 0L
  f <- function(x) {
    calls <<- calls + 1L
    sum(1 / x)
  }
  memo_call <- samplyr:::memo_call

  expect_identical(memo_call("f", f, 1), 1)
  expect_identical(memo_call("f", f, 1), 1)
  expect_identical(calls, 2L)

  local({
    samplyr:::local_replicate_memo()
    expect_identical(memo_call("f", f, c(1, 2)), 1.5)
    expect_identical(memo_call("f", f, c(1, 2)), 1.5)
    expect_identical(calls, 3L)
    # -0 equals 0 but is a different argument.
    expect_identical(memo_call("f", f, -0), -Inf)
    expect_identical(memo_call("f", f, 0), Inf)
    expect_identical(calls, 5L)
  })

  expect_null(samplyr:::replicate_memo$slots)
})

test_that("the replicate memo replays selection events and skips other conditions", {
  memo_call <- samplyr:::memo_call
  calls <- 0L
  noisy <- function(x) {
    calls <<- calls + 1L
    samplyr:::signal_selection_event("first", value = x)
    samplyr:::signal_selection_event("second", value = -x)
    x
  }
  warns <- function(x) {
    calls <<- calls + 1L
    warning("kept")
    x
  }
  seen <- function(expr) {
    ops <- character()
    value <- withCallingHandlers(
      expr,
      samplyr_condition_selection_event = function(cnd) {
        ops <<- c(ops, paste(cnd$operation, cnd$payload$value))
        rlang::cnd_muffle(cnd)
      }
    )
    list(value = value, ops = ops)
  }

  local({
    samplyr:::local_replicate_memo()
    first <- seen(memo_call("noisy", noisy, 2))
    again <- seen(memo_call("noisy", noisy, 2))
    expect_identical(first, list(value = 2, ops = c("first 2", "second -2")))
    expect_identical(again, first)
    expect_identical(calls, 1L)

    expect_warning(memo_call("warns", warns, 1), "kept")
    expect_warning(memo_call("warns", warns, 1), "kept")
    expect_identical(calls, 3L)

    local({
      samplyr:::local_replicate_memo_paused()
      expect_null(samplyr:::replicate_memo$slots)
      expect_identical(seen(memo_call("noisy", noisy, 2))$ops, first$ops)
      expect_identical(calls, 4L)
    })
    expect_identical(seen(memo_call("noisy", noisy, 2)), first)
    expect_identical(calls, 4L)
  })
})

test_that("a stratum plan consumes no random numbers", {
  frame <- data.frame(
    id = 1:600,
    st = rep(c("a", "b", "c"), times = c(100, 200, 300)),
    mos = rep(1:6, 100),
    x = sin(1:600)
  )
  base <- sampling_design() |> stratify_by(st, alloc = "proportional")
  designs <- list(
    srswor = base |> draw(n = 60),
    bounded = base |> draw(n = 60, min_n = 15, max_n = 25),
    pps_systematic = base |> draw(n = 60, method = "pps_systematic", mos = mos),
    srswr = base |> draw(n = 400, method = "srswr"),
    bernoulli = sampling_design() |> stratify_by(st) |>
      draw(frac = 0.5, method = "bernoulli"),
    cube = base |> draw(n = 60, method = "balanced", aux = x)
  )
  for (nm in names(designs)) {
    spec <- designs[[nm]]$stages[[1L]]
    set.seed(11)
    before <- .Random.seed
    plan <- samplyr:::stratum_plan(frame, spec$strata, spec$draw_spec)
    expect_identical(.Random.seed, before, info = nm)
    expect_identical(plan$stratum_info$.N_h, c(100L, 200L, 300L), info = nm)
  }
})

test_that("a cluster plan consumes no random numbers", {
  frame <- data.frame(
    cl = rep(1:30, each = 4),
    st = rep(c("a", "b", "c"), each = 40),
    mos = rep(1:30, each = 4),
    x = sin(1:120)
  )
  designs <- list(
    srswor = sampling_design() |> cluster_by(cl) |> draw(n = 6),
    pps = sampling_design() |> stratify_by(st) |> cluster_by(cl) |>
      draw(n = 2, method = "pps_brewer", mos = mos),
    cube = sampling_design() |> cluster_by(cl) |>
      draw(n = 6, method = "balanced", aux = x)
  )
  for (nm in names(designs)) {
    spec <- designs[[nm]]$stages[[1L]]
    set.seed(12)
    before <- .Random.seed
    plan <- samplyr:::cluster_plan(
      frame, spec$clusters$vars, spec$strata$vars, spec$draw_spec
    )
    expect_identical(.Random.seed, before, info = nm)
    expect_identical(plan$first_rows, seq(1L, 117L, by = 4L), info = nm)
  }
})

test_that("a stage-1 warning still fires in every replicate", {
  frame <- data.frame(id = 1:50, m = c(0, 1 + sin(1:49)^2))
  design <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = m)
  classes <- character()
  withCallingHandlers(
    execute(design, frame, seed = 1, reps = 3),
    warning = function(w) {
      classes <<- c(classes, class(w)[[1]])
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(classes, rep("samplyr_warning_mos_zero", 3))
})

test_that("a replicated execution validates only its merged digest", {
  frame <- data.frame(id = 1:200, st = rep(c("a", "b"), 100))
  design <- sampling_design() |> stratify_by(st) |> draw(n = 10)
  checked <- 0L
  validate <- samplyr:::validate_frame_digest
  local_mocked_bindings(
    validate_frame_digest = function(x, ...) {
      checked <<- checked + 1L
      validate(x, ...)
    }
  )
  single <- execute(design, frame, seed = 1)
  expect_identical(checked, 1L)
  replicated <- execute(design, frame, seed = 1, reps = 4)
  expect_identical(checked, 2L)
  expect_identical(get_frame_digest(replicated)$status, "complete")
  selected <- get_frame_digest(replicated)$stages[[1]]$selected
  expect_identical(tabulate(selected$replicate), rep(20L, 4))
})

test_that("replicates share the digest skeleton of stage 1", {
  frame <- data.frame(
    cl = rep(1:30, each = 4),
    st = rep(c("a", "b"), each = 60),
    mos = rep(1:30, each = 4)
  )
  design <- sampling_design() |>
    add_stage() |> stratify_by(st) |> cluster_by(cl) |>
    draw(n = 3, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 2)
  built <- integer()
  skeleton <- samplyr:::digest_stage_skeleton
  local_mocked_bindings(
    digest_stage_skeleton = function(design, stage_idx, ...) {
      built <<- c(built, stage_idx)
      skeleton(design, stage_idx, ...)
    }
  )
  replicated <- execute(design, frame, seed = 1, reps = 4)
  expect_identical(sum(built == 1L), 1L)
  first <- get_frame_digest(replicated)$stages[[1]]
  single <- get_frame_digest(execute(design, frame, seed = 1))$stages[[1]]
  expect_identical(first$units, single$units)
  expect_identical(
    tabulate(first$selected$replicate),
    rep(6L, 4)
  )
})

test_that("a digest lists repeated selections in order within each pool", {
  frame <- data.frame(id = 1:12, st = rep(c("a", "b"), each = 6))
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(n = 10, method = "srswr")
  sample <- execute(design, frame, seed = 3, frame_digest = "full")
  selected <- get_frame_digest(sample)$stages[[1]]$selected
  expect_identical(selected$pool_id, rep(1:2, each = 10))
  expected <- ave(
    seq_len(nrow(selected)),
    selected$pool_id,
    selected$unit_id,
    FUN = seq_along
  )
  expect_identical(selected$occurrence, as.integer(expected))
  expect_gt(max(selected$occurrence), 1L)
})

test_that("a cluster digest keys each selected cluster", {
  frame <- data.frame(cl = rep(1:8, each = 3), st = rep(c("a", "b"), each = 12))
  design <- sampling_design() |>
    add_stage() |> stratify_by(st) |> cluster_by(cl) |> draw(n = 2) |>
    add_stage() |> draw(n = 1)
  sample <- execute(design, frame, seed = 4, reps = 3)
  selected <- get_frame_digest(sample)$stages[[1]]$selected
  units <- get_frame_digest(sample)$stages[[1]]$units
  expect_identical(nrow(selected), 12L)
  expect_identical(tabulate(selected$replicate), rep(4L, 3))
  keys <- as.character(1:8)
  expect_identical(selected$key, keys[selected$unit_id])
  expect_identical(units$unit_id, 1:8)
})

test_that("the singleton check reads a sampled row's pool by its key", {
  # 0.1 + 0.2 and 0.3 differ as doubles but share the key "0.3".
  frame <- data.frame(x = c(rep(0.1 + 0.2, 3), rep(0.3, 3), rep(1, 4)))
  spec <- (sampling_design() |> stratify_by(x) |> draw(n = 1))$stages[[1]]
  sample <- frame[c(4, 7, 8), , drop = FALSE]
  events <- list()
  withCallingHandlers(
    samplyr:::signal_singleton_strata(
      frame, "x", sample, spec$draw_spec
    ),
    samplyr_condition_selection_event = function(cnd) {
      events[[length(events) + 1L]] <<- cnd
      rlang::cnd_muffle(cnd)
    }
  )
  expect_length(events, 1L)
  expect_identical(events[[1]]$payload$n_singleton, 1L)
})

test_that("replicate blocks are the rows each replicate holds in the stack", {
  sizes <- c(3L, 0L, 2L, 0L, 4L)
  results <- lapply(seq_along(sizes), function(r) {
    data.frame(x = seq_len(sizes[[r]]), .replicate = rep.int(r, sizes[[r]]))
  })
  stacked <- samplyr:::stack_replicates(results)
  expect_identical(
    samplyr:::replicate_blocks(results),
    samplyr:::replicate_row_locations(stacked$.replicate, seq_along(sizes))
  )
})

test_that("replicates share the parent units of a lower-stage frame", {
  frame <- data.frame(cl = rep(1:20, each = 5), id = 1:100)
  design <- sampling_design() |>
    add_stage() |> cluster_by(cl) |> draw(n = 4) |>
    add_stage() |> draw(n = 2)
  calls <- 0L
  distinct <- samplyr:::distinct_parent_units
  local_mocked_bindings(
    distinct_parent_units = function(...) {
      calls <<- calls + 1L
      distinct(...)
    }
  )
  replicated <- execute(design, frame, seed = 1, reps = 5)
  expect_identical(calls, 1L)
  expect_identical(tabulate(replicated$.replicate), rep(8L, 5))
})

test_that("only replicates keep every stratum's rows", {
  frame <- data.frame(id = 1:300, st = rep(c("a", "b", "c"), 100), m = 1:300)
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(n = 5, method = "pps_systematic", mos = m)
  calls <- 0L
  parts <- samplyr:::stratum_parts
  local_mocked_bindings(
    stratum_parts = function(...) {
      calls <<- calls + 1L
      parts(...)
    }
  )
  single <- execute(design, frame, seed = 1)
  expect_identical(calls, 0L)
  replicated <- execute(design, frame, seed = 1, reps = 3)
  expect_identical(calls, 1L)
  first <- dplyr::filter(as.data.frame(replicated), .replicate == 1)
  alone <- execute(
    design, frame, seed = attr(replicated, "metadata")$replicate_seeds[[1]]
  )
  expect_identical(first$id, alone$id)
})

test_that("unstratified SRS draws as draw_sample() does", {
  frame <- data.frame(id = 1:50, x = rnorm(50))
  spec <- list(method = "srswor")
  for (mode in c("full", "summary", "none")) {
    for (n in c(7L, 50L)) {
      set.seed(3)
      fast <- samplyr:::draw_srswor_compact(frame, n, trace_mode = mode)
      fast_rng <- .Random.seed
      set.seed(3)
      slow <- samplyr:::draw_sample(frame, n, spec, trace_mode = mode)
      expect_identical(fast, slow)
      expect_identical(fast_rng, .Random.seed)
    }
  }
  summary <- samplyr:::draw_srswor_compact(frame, 7L, trace_mode = "summary")
  expect_identical(summary$trace$chance, 7 / 50)
})

test_that("a stratum draws on every column selection reads", {
  set.seed(4)
  frame <- data.frame(
    st = rep(c("a", "b"), each = 30),
    c = runif(60),
    .row = runif(60, 1, 5),
    note = sample(letters, 60, TRUE),
    id = 1:60
  )
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(n = 4, method = "pps_systematic", mos = .row, control = c)
  spec <- design$stages[[1L]]
  expect_identical(
    samplyr:::selection_columns(frame, "st", spec$draw_spec),
    c("st", "c", ".row")
  )
  expect_identical(samplyr:::selection_row_column(c("st", ".row")), "..row")
  parts <- samplyr:::stratum_parts(
    frame, samplyr:::split_row_indices(frame, "st"), "st", spec$draw_spec
  )
  expect_identical(names(parts[[2L]]$data), c("st", "c", ".row", "..row"))
  expect_identical(parts[[2L]]$data[["..row"]], 31:60)

  s <- execute(design, frame, seed = 2)
  presorted <- execute(
    sampling_design() |>
      stratify_by(st) |>
      draw(n = 4, method = "pps_systematic", mos = .row),
    frame[order(frame$st, frame$c), ],
    seed = 2
  )
  expect_identical(s$id, presorted$id)
  expect_identical(s$note, frame$note[s$id])
  expect_identical(s$.row, frame$.row[s$id])
  totals <- vapply(split(frame$.row, frame$st), sum, numeric(1))
  expect_equal(s$.weight, unname(totals[s$st]) / (4 * s$.row))
})

test_that("a parent unit draws on every column selection reads", {
  set.seed(5)
  frame <- data.frame(
    cl = rep(1:4, each = 15),
    c = runif(60),
    .row = runif(60, 1, 5),
    note = sample(letters, 60, TRUE),
    id = 1:60
  )
  stage_1 <- sampling_design() |>
    add_stage() |>
    cluster_by(cl) |>
    draw(n = 4)
  seen <- NULL
  units <- samplyr:::sample_units
  local_mocked_bindings(
    sample_units = function(frame, ...) {
      seen <<- names(frame)
      units(frame, ...)
    }
  )
  s <- execute(
    stage_1 |>
      add_stage() |>
      draw(n = 3, method = "pps_systematic", mos = .row, control = c),
    frame,
    seed = 2
  )
  expect_identical(seen, c("c", ".row", "..row"))

  presorted <- execute(
    stage_1 |>
      add_stage() |>
      draw(n = 3, method = "pps_systematic", mos = .row),
    frame[order(frame$cl, frame$c), ],
    seed = 2
  )
  expect_identical(s$id, presorted$id)
  expect_identical(s$note, frame$note[s$id])
  expect_identical(s$.row, frame$.row[s$id])
  totals <- vapply(split(frame$.row, frame$cl), sum, numeric(1))
  expect_equal(s$.weight, unname(totals[s$cl]) / (3 * s$.row))
})

test_that("carried parent columns match a join on the selected units", {
  frame <- data.frame(psu = c(3L, 1L, 3L, 2L, NA), id = 1:5)
  previous <- data.frame(
    psu = c(1, 3, 3, NA),
    st = c("a", "b", "b", "z"),
    .weight = 2
  )
  expect_identical(
    samplyr:::carry_vars_by_parent(frame, previous, "st", NULL, 2L, "psu"),
    dplyr::left_join(frame, unique(previous[c("psu", "st")]), by = "psu")
  )

  frame$psu <- factor(frame$psu, levels = c(4, 3, 2, 1))
  previous$psu <- as.character(previous$psu)
  expect_identical(
    samplyr:::carry_vars_by_parent(frame, previous, "st", NULL, 2L, "psu"),
    dplyr::left_join(frame, unique(previous[c("psu", "st")]), by = "psu")
  )
})

test_that("a later stage keeps the selected parents' rows in frame order", {
  set.seed(6)
  psus <- data.frame(psu = 1:10, st = rep(c("a", "b"), each = 5))
  units <- data.frame(psu = sample(rep(1:10, each = 4)), id = 1:40)
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(st) |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage() |>
    draw(n = 2)
  linked <- NULL
  parents <- NULL
  carry <- samplyr:::carry_vars_by_parent
  local_mocked_bindings(
    carry_vars_by_parent = function(frame, previous_sample, ...) {
      # Preflight links the whole stage-1 frame first. The last call links
      # the selected units.
      linked <<- frame
      parents <<- previous_sample
      carry(frame, previous_sample, ...)
    }
  )
  execute(design, psus, units, seed = 1)
  expected <- dplyr::semi_join(units, parents["psu"], by = "psu")
  expect_identical(linked$id, expected$id)
  expect_identical(length(unique(linked$psu)), 4L)
})

test_that("a later stage checks its clusters once over all parents", {
  frame <- data.frame(
    psu = rep(1:4, each = 6),
    ssu = rep(1:12, each = 2),
    m = rep(1:12, each = 2) + 0.5,
    id = 1:24
  )
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage() |>
    cluster_by(ssu) |>
    draw(n = 1, method = "pps_brewer", mos = m)
  checks <- logical(0)
  clusters <- samplyr:::sample_clusters
  local_mocked_bindings(
    sample_clusters = function(..., check_invariants = TRUE) {
      checks <<- c(checks, check_invariants)
      clusters(..., check_invariants = check_invariants)
    }
  )
  s <- execute(design, frame, seed = 1, reps = 2)
  # In each replicate, stage 1 checks its frame and stage 2 checked both
  # parents at once.
  expect_identical(checks, rep(c(TRUE, FALSE, FALSE), 2))
  expect_identical(tabulate(s$.replicate), c(4L, 4L))
})

test_that("the count over all parents agrees with the check per parent", {
  frame <- data.frame(
    p = c(1, 1, 1, 2, 2, 2),
    c = c(1, 1, 2, 1, 1, 2),
    m = c(5, 5, 6, 7, 7, 8)
  )
  spec <- list(method = "pps_brewer", mos = "m")
  expect_true(
    samplyr:::cluster_invariants_hold(frame, "p", "c", NULL, spec)
  )
  # Cluster 1 holds two values across parents, one within each.
  expect_false(
    samplyr:::cluster_invariants_hold(frame, character(0), "c", NULL, spec)
  )
  frame$m[[2]] <- 4
  expect_false(
    samplyr:::cluster_invariants_hold(frame, "p", "c", NULL, spec)
  )
  expect_true(
    samplyr:::cluster_invariants_hold(frame, "p", "c", NULL, list())
  )
})

test_that("only the documented control grammar narrows a pool", {
  reads_named <- function(expr, env = globalenv()) {
    samplyr:::control_reads_named_columns(rlang::new_quosure(expr, env))
  }
  expect_true(reads_named(quote(x)))
  expect_true(reads_named(quote(desc(x))))
  expect_true(reads_named(quote(serp(a, b))))
  expect_false(reads_named(quote(get("x"))))
  expect_false(reads_named(quote(desc(get("x")))))
  expect_false(reads_named(quote(-x)))
  expect_false(reads_named(quote(serp(a, "b"))))

  # A local function of a documented name is the one evaluation calls.
  local_serp <- rlang::env(globalenv(), serp = function(...) NULL)
  expect_false(reads_named(quote(serp(a, b)), local_serp))
  # A top-level desc() is stripped, never called, so its binding is moot.
  local_desc <- rlang::env(globalenv(), desc = function(x) NULL)
  expect_true(reads_named(quote(desc(x)), local_desc))
  expect_false(reads_named(quote(serp(a, desc(x))), local_desc))
})

test_that("a local serp() that reads a column by name sees the frame", {
  frame <- data.frame(id = 1:60, st = rep(c("a", "b"), each = 30))
  frame$key <- rev(frame$id)
  serp <- function(...) get("key", envir = parent.frame())
  s <- execute(
    sampling_design() |>
      stratify_by(st) |>
      draw(n = 4, method = "systematic", control = serp(st)),
    frame,
    seed = 2
  )
  by_key <- execute(
    sampling_design() |>
      stratify_by(st) |>
      draw(n = 4, method = "systematic", control = key),
    frame,
    seed = 2
  )
  expect_identical(s$id, by_key$id)
})

test_that("a control that reads a column by name sees the frame's column", {
  set.seed(7)
  frame <- data.frame(
    st = rep(c("a", "b"), each = 30),
    id = 1:60,
    m = runif(60, 1, 5)
  )
  frame$.row <- sample(60)
  by_name <- sampling_design() |>
    stratify_by(st) |>
    draw(n = 4, method = "pps_systematic", mos = m, control = get(".row"))
  expect_identical(
    samplyr:::selection_columns(frame, "st", by_name$stages[[1L]]$draw_spec),
    names(frame)
  )
  expect_identical(samplyr:::selection_row_column(names(frame)), "..row")

  sorted <- frame[order(frame$st, frame$.row), ]
  for (reps in list(NULL, 2L)) {
    s <- execute(by_name, frame, seed = 3, reps = reps)
    presorted <- execute(
      sampling_design() |>
        stratify_by(st) |>
        draw(n = 4, method = "pps_systematic", mos = m),
      sorted,
      seed = 3,
      reps = reps
    )
    expect_identical(s$id, presorted$id)
  }

  parent <- function(...) {
    sampling_design() |>
      add_stage() |>
      cluster_by(st) |>
      draw(n = 2) |>
      add_stage() |>
      draw(n = 3, method = "pps_systematic", mos = m, ...)
  }
  for (reps in list(NULL, 2L)) {
    s <- execute(parent(control = get(".row")), frame, seed = 3, reps = reps)
    presorted <- execute(parent(), sorted, seed = 3, reps = reps)
    expect_identical(s$id, presorted$id)
  }
})
