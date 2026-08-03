## Capping is detected one pool at a time and reported once per stage.
## These tests are about the counting: how many conditions reach the user,
## and whether their totals match the pools that actually capped.

count_conditions <- function(expr, class) {
  n <- 0L
  withCallingHandlers(
    force(expr),
    condition = function(cnd) {
      if (inherits(cnd, class)) {
        n <<- n + 1L
      }
      if (inherits(cnd, "warning")) {
        invokeRestart("muffleWarning")
      }
      if (inherits(cnd, "message")) {
        invokeRestart("muffleMessage")
      }
    }
  )
  n
}

capture_capped <- function(expr) {
  events <- list()
  withCallingHandlers(
    force(expr),
    samplyr_warning_size_capped = function(w) {
      events[[length(events) + 1L]] <<- w
      invokeRestart("muffleWarning")
    }
  )
  events
}

# PSUs of size 2, 3, 4, 50, 50, 50 with a stage-2 take of 10: the three small
# PSUs cannot fill it. 60 units requested, 39 selected.
short_psu_frame <- function() {
  sizes <- c(2, 3, 4, 50, 50, 50)
  data.frame(
    psu = rep(paste0("c", seq_along(sizes)), times = sizes),
    id = seq_len(sum(sizes))
  )
}

short_psu_design <- function(take = 10) {
  sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = length(unique(short_psu_frame()$psu))) |>
    add_stage("unit") |>
    draw(n = take)
}

many_pool_frame <- function(n_pools, small, big_size = 50, small_size = 2) {
  sizes <- rep(big_size, n_pools)
  sizes[seq_len(small)] <- small_size
  data.frame(
    psu = rep(sprintf("c%03d", seq_len(n_pools)), times = sizes),
    id = seq_len(sum(sizes))
  )
}

many_pool_design <- function(n_pools, take = 10) {
  sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = n_pools) |>
    add_stage("unit") |>
    draw(n = take)
}

test_that("an unstratified stage-2 take above the pool size reports once", {
  # This case was silent before: `pmin()` capped each pool and said nothing,
  # so the design stopped being self-weighting with no notice.
  expect_warning(
    result <- execute(short_psu_design(), short_psu_frame(), seed = 1),
    class = "samplyr_warning_size_capped"
  )

  expect_equal(nrow(result), 39)
})

test_that("many capped pools still report exactly one condition", {
  # 199 of 200, not 200 of 200: leaving one pool with units to spare keeps
  # this a population cap. A stage that exhausts every pool it executed is a
  # census and reports as one, which is covered separately.
  frame <- many_pool_frame(n_pools = 200, small = 199)

  expect_equal(
    count_conditions(
      execute(many_pool_design(200), frame, seed = 1),
      "samplyr_warning_size_capped"
    ),
    1L
  )
})

test_that("the aggregate totals match the pools that capped", {
  events <- capture_capped(
    execute(short_psu_design(), short_psu_frame(), seed = 1)
  )

  expect_length(events, 1L)
  expect_equal(events[[1]]$stage, 2L)
  expect_equal(events[[1]]$payload$n_capped, 3L)
  expect_equal(events[[1]]$payload$n_pools, 6L)
  expect_equal(events[[1]]$payload$n_requested, 60L)
  expect_equal(events[[1]]$payload$n_actual, 39L)
  expect_setequal(events[[1]]$payload$pool_keys, c("c1", "c2", "c3"))
})

test_that("a long pool list is named in part and counted in full", {
  frame <- many_pool_frame(n_pools = 60, small = 40)

  expect_warning(
    execute(many_pool_design(60), frame, seed = 1),
    "and 35 more",
    class = "samplyr_warning_size_capped"
  )

  # The condition still carries every key, however many are printed.
  events <- capture_capped(execute(many_pool_design(60), frame, seed = 1))
  expect_length(events[[1]]$payload$pool_keys, 40L)
})

test_that("the full pool list survives frame_digest = \"none\"", {
  frame <- many_pool_frame(n_pools = 60, small = 40)

  events <- capture_capped(
    execute(many_pool_design(60), frame, seed = 1, frame_digest = "none")
  )

  expect_length(events, 1L)
  expect_length(events[[1]]$payload$pool_keys, 40L)
})

test_that("replicates report the shared finding once, not once each", {
  # The pools that cap are a property of the design and the frame, so ten
  # replicates are one finding, not ten.
  frame <- data.frame(id = 1:110, h = rep(c("A", "B"), times = c(10, 100)))
  design <- sampling_design() |>
    stratify_by(h) |>
    draw(n = 40)

  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1, reps = 10),
      "samplyr_warning_size_capped"
    ),
    1L
  )
})

test_that("a stratified stage inside a cluster loop reports once", {
  # sample_stratified() runs once per parent pool, so this reported once per
  # cluster before the events were aggregated.
  frame <- do.call(rbind, lapply(1:3, function(k) {
    data.frame(
      clust = paste0("c", k),
      h = rep(c("A", "B"), times = c(2, 100)),
      id = paste0(k, "-", 1:102)
    )
  }))

  design <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(clust) |>
    draw(n = 3) |>
    add_stage("unit") |>
    stratify_by(h, alloc = "equal") |>
    draw(n = 20)

  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1),
      "samplyr_message_allocation_capped"
    ),
    1L
  )
})

test_that("replicated allocation capping reports once", {
  frame <- data.frame(id = 1:110, h = rep(c("A", "B"), times = c(10, 100)))
  design <- sampling_design() |>
    stratify_by(h, alloc = "equal") |>
    draw(n = 40)

  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1, reps = 10),
      "samplyr_message_allocation_capped"
    ),
    1L
  )
})

test_that("random-size shortfall is not a population cap", {
  # Bernoulli and Poisson realize a count around their target. Falling below
  # it is the shortfall of P1 item 4, not a pool that ran out of units.
  frame <- data.frame(id = 1:500)

  for (design in list(
    sampling_design() |> draw(frac = 0.1, method = "bernoulli"),
    sampling_design() |> draw(n = 50, method = "pps_poisson", mos = id)
  )) {
    expect_equal(
      count_conditions(
        execute(design, frame, seed = 3),
        "samplyr_warning_size_capped"
      ),
      0L
    )
  }
})

test_that("a design that fits reports nothing", {
  expect_no_warning(
    execute(short_psu_design(take = 2), short_psu_frame(), seed = 1),
    class = "samplyr_warning_size_capped"
  )
})

test_that("the digest marks exactly the pools that capped", {
  sample <- suppressWarnings(
    execute(short_psu_design(), short_psu_frame(), seed = 1,
            frame_digest = "full")
  )

  pools <- frame_summary(sample, detail = "pool")
  stage2 <- pools[pools$stage == 2, ]

  expect_equal(sum(stage2$capped), 3L)
  expect_equal(stage2$N[stage2$capped], c(2, 3, 4))
  expect_true(all(stage2$n_expected[stage2$capped] < 10))
})

test_that("nothing is marked capped when nothing caps", {
  sample <- execute(
    short_psu_design(take = 2), short_psu_frame(), seed = 1,
    frame_digest = "full"
  )

  pools <- frame_summary(sample, detail = "pool")
  expect_false(any(pools$capped))
})

test_that("a random-size shortfall is not marked capped", {
  # Poisson realizes around its target, so n_expected < n_target would read as
  # a population cap on a stage that never ran out of units.
  frame <- data.frame(id = 1:500, m = seq(1, 10, length.out = 500))

  sample <- execute(
    sampling_design() |> draw(n = 50, method = "pps_poisson", mos = m),
    frame,
    seed = 3,
    frame_digest = "full"
  )

  expect_false(any(frame_summary(sample, detail = "pool")$capped))
})

test_that("a random-size target above the population caps the nominal target", {
  # Clamping every chance at 1 caps what the stage aims at, it does not select
  # that many units. Reporting a population cap here states a selected count
  # the sample does not contain, and contradicts the digest, which reads the
  # same stage as uncapped.
  frame <- data.frame(id = 1:10, m = seq(1, 5, length.out = 10))

  for (design in list(
    sampling_design() |> draw(n = 20, method = "bernoulli"),
    sampling_design() |> draw(n = 20, method = "pps_poisson", mos = m)
  )) {
    expect_equal(
      count_conditions(
        execute(design, frame, seed = 1),
        "samplyr_warning_size_capped"
      ),
      0L
    )
    expect_equal(
      count_conditions(
        execute(design, frame, seed = 1),
        "samplyr_warning_nominal_cap"
      ),
      1L
    )

    sample <- suppressWarnings(
      execute(design, frame, seed = 1, frame_digest = "full")
    )
    expect_false(any(frame_summary(sample, detail = "pool")$capped))
  }
})

test_that("the nominal cap reports the target it capped to, not a count", {
  frame <- data.frame(id = 1:10, m = seq(1, 5, length.out = 10))
  design <- sampling_design() |> draw(n = 20, method = "pps_poisson", mos = m)

  # The frame also saturates, so a shortfall is reported alongside the
  # nominal cap. Two reductions, two findings; only one is under test here.
  w <- NULL
  sample <- suppressWarnings(withCallingHandlers(
    execute(design, frame, seed = 1),
    samplyr_warning_nominal_cap = function(cnd) w <<- cnd
  ))

  expect_equal(w$payload$n_requested, 20)
  expect_equal(w$payload$n_available, 10)
  # The realized size is a draw and lands below the cap. The warning must not
  # be readable as a claim about it.
  expect_lt(nrow(sample), 10L)
})

test_that("an explicit per-stratum random-size target caps nominally", {
  # The allocation path already distinguishes the two. This is the path that
  # skips it: explicit per-stratum sizes reach selection uncapped.
  frame <- data.frame(stratum = rep(c("A", "B"), each = 5), id = 1:10)
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = c(A = 20, B = 2), method = "bernoulli")

  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1),
      "samplyr_warning_size_capped"
    ),
    0L
  )
  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1),
      "samplyr_warning_nominal_cap"
    ),
    1L
  )
})

test_that("a fixed-size target above the population is still a population cap", {
  # Asserted on the operation rather than the class: a fixed-size shortfall
  # is a population cap however the reporter reads the aggregate, and this
  # frame happens to be exhausted, so the reading is census.
  frame <- data.frame(id = 1:10)
  design <- sampling_design() |> draw(n = 20)

  ops <- character(0)
  suppressWarnings(withCallingHandlers(
    execute(design, frame, seed = 1),
    condition = function(cnd) {
      op <- cnd$operation
      if (!is.null(op)) {
        ops <<- c(ops, op)
      }
    }
  ))
  expect_identical(unique(ops), "population_cap")

  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1),
      "samplyr_warning_nominal_cap"
    ),
    0L
  )
})

## Census: one detected operation, promoted centrally

capture_one <- function(expr, class) {
  found <- NULL
  withCallingHandlers(
    force(expr),
    condition = function(cnd) {
      if (inherits(cnd, class)) {
        found <<- cnd
      }
      if (inherits(cnd, "warning")) {
        invokeRestart("muffleWarning")
      }
      if (inherits(cnd, "message")) {
        invokeRestart("muffleMessage")
      }
    }
  )
  found
}

test_that("every fixed-size path reaches the same class for the same request", {
  # Asking for 20 units from a frame of 10 is one situation. Which code path
  # computed the number is not something the user expressed, so it must not
  # decide which condition they get.
  flat <- data.frame(id = 1:10)
  strat <- data.frame(stratum = rep(c("A", "B"), each = 5), id = 1:10)

  designs <- list(
    sampling_design() |> draw(n = 20),
    sampling_design() |> stratify_by(stratum) |> draw(n = c(A = 10, B = 10)),
    sampling_design() |>
      stratify_by(stratum, alloc = "proportional") |>
      draw(n = 20)
  )
  frames <- list(flat, strat, strat)

  for (i in seq_along(designs)) {
    expect_equal(
      count_conditions(
        execute(designs[[i]], frames[[i]], seed = 1),
        "samplyr_warning_census"
      ),
      1L
    )
  }
})

test_that("a stage census is decided by exhaustion, not by saturated pools", {
  # `equal` on populations (1, 100) with n = 102: one stratum exceeds its
  # share before redistribution, both are taken whole after it. Counting
  # saturated pools would read this as a partial cap.
  frame <- data.frame(
    stratum = rep(c("A", "B"), times = c(1, 100)),
    id = 1:101
  )

  w <- capture_one(
    execute(
      sampling_design() |>
        stratify_by(stratum, alloc = "equal") |>
        draw(n = 102),
      frame,
      seed = 1
    ),
    "samplyr_warning_census"
  )

  expect_false(is.null(w))
  expect_equal(w$payload$n_requested, 102)
  expect_equal(w$payload$n_actual, 101)
  expect_equal(w$payload$n_available, 101)
})

test_that("a stage that leaves units behind is not a census", {
  w <- capture_one(
    execute(short_psu_design(), short_psu_frame(), seed = 1),
    "samplyr_warning_size_capped"
  )

  expect_false(is.null(w))
  expect_equal(w$payload$n_actual, 39)
  expect_equal(w$payload$n_available, sum(c(2, 3, 4, 50, 50, 50)))
  expect_lt(w$payload$n_actual, w$payload$n_available)
})

test_that("both readings of a population cap carry the same operation", {
  census <- capture_one(
    execute(sampling_design() |> draw(n = 20), data.frame(id = 1:10), seed = 1),
    "samplyr_warning_census"
  )
  capped <- capture_one(
    execute(short_psu_design(), short_psu_frame(), seed = 1),
    "samplyr_warning_size_capped"
  )

  expect_identical(census$operation, "population_cap")
  expect_identical(capped$operation, "population_cap")
})

test_that("replicates report one census with one replicate's totals", {
  frame <- data.frame(stratum = rep(c("A", "B"), each = 5), id = 1:10)
  design <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 20)

  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1, reps = 4),
      "samplyr_warning_census"
    ),
    1L
  )

  w <- capture_one(
    execute(design, frame, seed = 1, reps = 4),
    "samplyr_warning_census"
  )
  expect_equal(w$payload$n_requested, 20)
  expect_equal(w$payload$n_available, 10)
})

test_that("replicates reaching different parents still report once", {
  # Deduplication used to require the aggregates to match, which they do only
  # for a single-stage design. A replicated multistage design reaches
  # different parents in different replicates, so the same stage legitimately
  # names different pools each time and the report fragmented into one warning
  # per distinct pool set.
  frame <- data.frame(psu = rep(c("p1", "p2"), each = 4), id = 1:8)
  design <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = 1) |>
    add_stage("unit") |>
    draw(n = 10)

  expect_equal(
    count_conditions(
      execute(design, frame, seed = 7, reps = 10),
      "samplyr_warning_census"
    ),
    1L
  )

  w <- capture_one(
    execute(design, frame, seed = 7, reps = 10),
    "samplyr_warning_census"
  )
  # Both parents are named, and the counts stay one replicate's.
  expect_setequal(w$payload$pool_keys, c("p1", "p2"))
  expect_true(w$payload$varied)
  expect_equal(w$payload$n_replicates, 10L)
  expect_equal(w$payload$n_requested, 10)
  expect_equal(w$payload$n_available, 4)
})

test_that("replicates reaching different outcomes report each one", {
  # PSUs of 2, 3 and 50, taking 2 of them and 10 units from each. A replicate
  # drawing the two small PSUs exhausts them; one drawing the large PSU does
  # not. Merging before classifying let whichever replicate reported first
  # name the class for the rest, so the same design and replicate count gave
  # a census under one seed and a population cap under another.
  frame <- data.frame(
    psu = rep(c("p1", "p2", "p3"), times = c(2, 3, 50)),
    id = 1:55
  )
  design <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage("unit") |>
    draw(n = 10)

  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1, reps = 6),
      "samplyr_warning_census"
    ),
    1L
  )
  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1, reps = 6),
      "samplyr_warning_size_capped"
    ),
    1L
  )

  # Each condition names only the pools that produced it: p2 is exhausted
  # alongside p1 in the census replicates, and must not be listed as a capped
  # pool of a replicate that never exhausted it.
  census <- capture_one(
    execute(design, frame, seed = 1, reps = 6),
    "samplyr_warning_census"
  )
  capped <- capture_one(
    execute(design, frame, seed = 1, reps = 6),
    "samplyr_warning_size_capped"
  )

  expect_setequal(census$payload$pool_keys, c("p1", "p2"))
  expect_identical(capped$payload$pool_keys, "p1")
  expect_equal(census$payload$n_actual, census$payload$n_available)
  expect_lt(capped$payload$n_actual, capped$payload$n_available)
})

test_that("the class does not depend on which replicate reported first", {
  frame <- data.frame(
    psu = rep(c("p1", "p2", "p3"), times = c(2, 3, 50)),
    id = 1:55
  )
  design <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage("unit") |>
    draw(n = 10)

  # Seeds 1 and 2 differ only in replicate ordering; both executions contain
  # both outcomes, so both must report both.
  for (seed in c(1, 2, 5)) {
    expect_equal(
      count_conditions(
        execute(design, frame, seed = seed, reps = 6),
        "samplyr_warning_census"
      ),
      1L,
      info = paste("seed", seed)
    )
  }
})

test_that("replicates that agree do not claim to have varied", {
  frame <- data.frame(stratum = rep(c("A", "B"), each = 5), id = 1:10)
  design <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 20)

  w <- capture_one(
    execute(design, frame, seed = 1, reps = 4),
    "samplyr_warning_census"
  )
  expect_false(w$payload$varied)
  expect_equal(w$payload$n_replicates, 4L)
})

test_that("an unreplicated execution reports its own totals", {
  # The replicate grouping key has to distinguish "no replicate" from a
  # missing value, or an unreplicated run aggregates an empty group and every
  # total comes back NA.
  w <- capture_one(
    execute(short_psu_design(), short_psu_frame(), seed = 1),
    "samplyr_warning_size_capped"
  )

  expect_equal(w$payload$n_capped, 3L)
  expect_equal(w$payload$n_actual, 39)
  expect_false(w$payload$varied)
  expect_equal(w$payload$n_replicates, 1L)
})

test_that("a random-size allocation above the population is never a census", {
  frame <- data.frame(stratum = rep(c("A", "B"), each = 5), id = 1:10)
  design <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 20, method = "bernoulli")

  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1),
      "samplyr_warning_census"
    ),
    0L
  )
  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1),
      "samplyr_warning_nominal_cap"
    ),
    1L
  )
})

test_that("the allocation path no longer warns on its own", {
  frame <- data.frame(
    psu = rep(paste0("c", 1:3), each = 8),
    stratum = rep(rep(c("A", "B"), each = 4), 3),
    id = 1:24
  )
  design <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage("unit") |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 20)

  # Three parent pools used to produce three warnings from allocate.R.
  expect_equal(
    count_conditions(
      execute(design, frame, seed = 1),
      "samplyr_warning_census"
    ),
    1L
  )
})

## Hierarchical pool identities

test_that("capped pools inside a cluster loop keep their parent", {
  frame <- data.frame(
    psu = rep(paste0("c", 1:3), each = 110),
    stratum = rep(rep(c("A", "B"), times = c(10, 100)), 3),
    id = 1:330
  )
  design <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage("unit") |>
    stratify_by(stratum, alloc = "equal") |>
    draw(n = 60)

  m <- capture_one(
    execute(design, frame, seed = 1),
    "samplyr_message_allocation_capped"
  )

  expect_false(is.null(m))
  expect_equal(m$payload$n_capped, 3L)
  # Three parents each cap their local stratum A. Three pools, three names.
  expect_identical(m$payload$pool_keys, c("c1 / A", "c2 / A", "c3 / A"))
})

test_that("an allocation-originated event names every stratum it counted", {
  # The count and the key list describe the same pools, so a report of 2 of 2
  # that names none of them is not a report.
  w <- capture_one(
    execute(
      sampling_design() |>
        stratify_by(stratum, alloc = "proportional") |>
        draw(n = 20),
      data.frame(stratum = rep(c("A", "B"), each = 5), id = 1:10),
      seed = 1
    ),
    "samplyr_warning_census"
  )

  expect_equal(w$payload$n_pools, 2L)
  expect_identical(w$payload$pool_keys, c("A", "B"))
})

test_that("allocation strata inside parents are named parent by parent", {
  frame <- data.frame(
    psu = rep(paste0("c", 1:3), each = 8),
    stratum = rep(rep(c("A", "B"), each = 4), 3),
    id = 1:24
  )
  design <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage("unit") |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 20)

  w <- capture_one(execute(design, frame, seed = 1), "samplyr_warning_census")

  expect_equal(w$payload$n_pools, 6L)
  expect_identical(
    w$payload$pool_keys,
    c("c1 / A", "c1 / B", "c2 / A", "c2 / B", "c3 / A", "c3 / B")
  )
})

test_that("a cluster stage nested in a cluster stage keeps its parent", {
  # The nested branch of execute_single_stage() runs its own parent loop and
  # has to qualify what that loop raises, exactly as the within-cluster
  # sampler does.
  frame <- data.frame(
    psu = rep(c("p1", "p2"), each = 6),
    ssu = rep(paste0("s", 1:4), each = 3),
    id = 1:12
  )
  design <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage("ssu") |>
    cluster_by(ssu) |>
    draw(n = 10)

  w <- capture_one(execute(design, frame, seed = 1), "samplyr_warning_census")

  expect_false(is.null(w))
  expect_identical(w$payload$pool_keys, c("p1", "p2"))
})

test_that("a stage with no pools of its own is named by its parent alone", {
  w <- capture_one(
    execute(short_psu_design(), short_psu_frame(), seed = 1),
    "samplyr_warning_size_capped"
  )
  expect_identical(w$payload$pool_keys, c("c1", "c2", "c3"))
})

test_that("multi-variable cluster keys are reported as values, not encodings", {
  frame <- data.frame(
    region = rep(c("N", "S"), each = 6),
    district = rep(c("d1", "d2"), each = 3, times = 2),
    id = 1:12
  )
  design <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(region, district) |>
    draw(n = 4) |>
    add_stage("unit") |>
    draw(n = 10)

  w <- capture_one(execute(design, frame, seed = 1), "samplyr_warning_census")

  expect_identical(w$payload$pool_keys, c("N/d1", "N/d2", "S/d1", "S/d2"))
})

## Payload contract

test_that("a payload field the operation does not record is NA, not zero", {
  nominal <- capture_one(
    execute(
      sampling_design() |> draw(n = 20, method = "bernoulli"),
      data.frame(id = 1:10),
      seed = 1
    ),
    "samplyr_warning_nominal_cap"
  )
  # No realized count is being reported, so none is claimed.
  expect_true(is.na(nominal$payload$n_actual))
  expect_equal(nominal$payload$n_available, 10)

  alloc <- capture_one(
    execute(
      sampling_design() |>
        stratify_by(stratum, alloc = "equal") |>
        draw(n = 60),
      data.frame(
        stratum = rep(c("A", "B"), times = c(10, 100)),
        id = 1:110
      ),
      seed = 1
    ),
    "samplyr_message_allocation_capped"
  )
  expect_true(is.na(alloc$payload$n_requested))
  expect_true(is.na(alloc$payload$n_actual))
  expect_true(is.na(alloc$payload$n_available))
})

test_that("the retired allocation-specific classes are gone", {
  sources <- c(
    list.files("../../R", pattern = "[.]R$", full.names = TRUE),
    list.files("../../man", pattern = "[.]Rd$", full.names = TRUE),
    "../../NEWS.md"
  )
  sources <- sources[file.exists(sources)]
  text <- unlist(lapply(sources, readLines, warn = FALSE))

  expect_false(any(grepl("samplyr_warning_alloc_census", text, fixed = TRUE)))
  expect_false(any(grepl(
    "samplyr_warning_alloc_nominal_cap", text, fixed = TRUE
  )))
})

test_that("the aggregated messages read as intended", {
  expect_snapshot({
    invisible(execute(short_psu_design(), short_psu_frame(), seed = 1))
    invisible(execute(
      many_pool_design(60),
      many_pool_frame(n_pools = 60, small = 40),
      seed = 1
    ))
  })
})

test_that("the census and nominal-cap messages read as intended", {
  strat <- data.frame(stratum = rep(c("A", "B"), each = 5), id = 1:10)

  expect_snapshot({
    # Whole frame taken: a census of a single-stage design.
    invisible(execute(
      sampling_design() |>
        stratify_by(stratum, alloc = "proportional") |>
        draw(n = 20),
      strat,
      seed = 1
    ))

    # A census of stage 2 only. The wording must not claim the design is one.
    invisible(execute(
      sampling_design() |>
        add_stage("psu") |>
        cluster_by(psu) |>
        draw(n = 2) |>
        add_stage("unit") |>
        draw(n = 10),
      data.frame(psu = rep(c("c1", "c2", "c3"), each = 4), id = 1:12),
      seed = 1
    ))

    invisible(execute(
      sampling_design() |> draw(n = 20, method = "bernoulli"),
      data.frame(id = 1:10),
      seed = 1
    ))
  })
})
