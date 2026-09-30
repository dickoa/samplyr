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
  # Capping breaks self-weighting, so it has to be reported.
  expect_warning(
    result <- execute(short_psu_design(), short_psu_frame(), seed = 1),
    class = "samplyr_warning_size_capped"
  )

  expect_equal(nrow(result), 39)
})

test_that("many capped pools still report exactly one condition", {
  # One pool keeps units to spare, so this is a cap and not a census.
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
  # Capped pools are a property of the design and frame, not of a replicate.
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
  # sample_stratified() runs once per parent pool.
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
  # Poisson realizes around its target, so a shortfall is not a cap.
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
  # Clamping every chance at 1 caps the target, not the selected count.
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

  # The frame also saturates, so a shortfall is reported alongside.
  w <- NULL
  sample <- suppressWarnings(withCallingHandlers(
    execute(design, frame, seed = 1),
    samplyr_warning_nominal_cap = function(cnd) w <<- cnd
  ))

  expect_equal(w$payload$n_requested, 20)
  expect_equal(w$payload$n_available, 10)
  # The realized size is a draw and lands below the cap.
  expect_lt(nrow(sample), 10L)
})

test_that("an explicit per-stratum random-size target caps nominally", {
  # Explicit per-stratum sizes skip allocation and reach selection uncapped.
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
  # Asserted on the operation, since this exhausted frame reads as a census.
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
  # The code path that computed the size must not decide the condition.
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
  # equal on sizes (1, 100) with n = 102 ends with both strata taken whole.
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

# 40 EAs, two of them with 3 households against a take of 5 or 3 per pool.
# Each parent runs alone and only the two short ones signal, so their sums
# read as a stage that took everything it reached.
short_ea_frame <- function() {
  sizes <- c(rep(12, 38), 3, 3)
  frame <- data.frame(ea = rep(sprintf("e%02d", 1:40), sizes))
  frame$hh <- seq_len(nrow(frame))
  frame$sex <- rep(c("f", "m"), length.out = nrow(frame))
  frame
}

short_ea_design <- function(variant) {
  first <- sampling_design() |>
    add_stage() |>
    cluster_by(ea) |>
    draw(n = 40) |>
    add_stage()
  switch(
    variant,
    cluster = first |> cluster_by(hh) |> draw(n = 5),
    stratify = first |> stratify_by(sex) |> draw(n = 3),
    both = first |> stratify_by(sex) |> cluster_by(hh) |> draw(n = 3),
    alloc = first |> stratify_by(sex, alloc = "proportional") |> draw(n = 5),
    census = first |> cluster_by(hh) |> draw(n = 20)
  )
}

test_that("a lower stage short in a few parents is capped, not a census", {
  frame <- short_ea_frame()
  n_hh <- nrow(frame)
  n_ea_sex <- nrow(unique(frame[c("ea", "sex")]))
  take_ea <- pmin(table(frame$ea), 5)
  take_ea_sex <- pmin(table(frame$ea, frame$sex), 3)
  # Hand totals for each variant: pools, units selected, units requested.
  expected <- list(
    cluster = c(40, sum(take_ea), 40 * 5),
    stratify = c(n_ea_sex, sum(take_ea_sex), n_ea_sex * 3),
    both = c(n_ea_sex, sum(take_ea_sex), n_ea_sex * 3),
    alloc = c(n_ea_sex, sum(take_ea), 40 * 5)
  )
  expect_identical(names(expected), c("cluster", "stratify", "both", "alloc"))

  for (variant in names(expected)) {
    design <- short_ea_design(variant)
    expect_identical(
      count_conditions(
        execute(design, frame, seed = 1),
        "samplyr_warning_census"
      ),
      0L,
      label = variant
    )
    w <- capture_one(
      execute(design, frame, seed = 1),
      "samplyr_warning_size_capped"
    )
    expect_false(is.null(w), label = variant)
    got <- unlist(w$payload[c("n_pools", "n_actual", "n_requested")])
    expect_equal(unname(got), expected[[variant]], label = variant)
    expect_equal(w$payload$n_available, n_hh, label = variant)
  }
})

test_that("a lower stage that takes every household is still a census", {
  frame <- short_ea_frame()
  w <- capture_one(
    execute(short_ea_design("census"), frame, seed = 1),
    "samplyr_warning_census"
  )
  expect_false(is.null(w))
  expect_equal(w$payload$n_pools, 40)
  expect_equal(w$payload$n_actual, nrow(frame))
  expect_equal(w$payload$n_available, nrow(frame))
})

test_that("a clustered lower stage counts clusters, not their rows", {
  # Two persons per household, and the stage counts households.
  hh <- short_ea_frame()
  frame <- hh[rep(seq_len(nrow(hh)), each = 2), c("ea", "hh")]
  frame$person <- seq_len(nrow(frame))
  s <- NULL
  w <- capture_one(
    s <- execute(short_ea_design("cluster"), frame, seed = 1),
    "samplyr_warning_size_capped"
  )
  expect_identical(nrow(s), 2L * length(unique(s$hh)))
  expect_equal(w$payload$n_available, nrow(hh))
  expect_equal(w$payload$n_actual, length(unique(s$hh)))
  expect_equal(w$payload$n_actual, sum(pmin(table(hh$ea), 5)))
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
  # Replicates reach different parents but form one finding.
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
  # PSUs of 2, 3 and 50: replicates drawing the two small ones exhaust them.
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
      execute(design, frame, seed = 18, reps = 6),
      "samplyr_warning_census"
    ),
    1L
  )
  expect_equal(
    count_conditions(
      execute(design, frame, seed = 18, reps = 6),
      "samplyr_warning_size_capped"
    ),
    1L
  )

  # p2 is listed only in the census, where it is exhausted alongside p1.
  census <- capture_one(
    execute(design, frame, seed = 18, reps = 6),
    "samplyr_warning_census"
  )
  capped <- capture_one(
    execute(design, frame, seed = 18, reps = 6),
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

  # Every seed's replicates contain both outcomes, so each reports both.
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
  # The replicate key must distinguish no replicate from a missing value.
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

test_that("the allocation path does not warn on its own", {
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
  expect_identical(m$payload$pool_keys, c("c1 > A", "c2 > A", "c3 > A"))
})

test_that("an allocation-originated event names every stratum it counted", {
  # The count and the key list describe the same pools.
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
    c("c1 > A", "c1 > B", "c2 > A", "c2 > B", "c3 > A", "c3 > B")
  )
})

test_that("a cluster stage nested in a cluster stage keeps its parent", {
  # The nested branch of execute_single_stage() qualifies its parent loop.
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

## Strata that draw a single unit outside certainty
#
# A stratum needs two selections for a variance. The export warns once the
# sample exists, and execute() says it while the allocation can change. It is
# a message, because one unit per stratum is sometimes the design.

capture_singletons <- function(expr) {
  found <- list()
  withCallingHandlers(
    force(expr),
    samplyr_message_singleton_pool = function(m) {
      found[[length(found) + 1L]] <<- m
      invokeRestart("muffleMessage")
    }
  )
  found
}

test_that("strata taking one unit are named once per stage", {
  n_by_stratum <- c(A = 5, B = 20, C = 975)
  frame <- data.frame(id = 1:1000, st = rep(names(n_by_stratum), n_by_stratum))
  found <- capture_singletons(
    sampling_design() |>
      stratify_by(st, alloc = "proportional") |>
      draw(n = 50, min_n = 1) |>
      execute(frame, seed = 1)
  )
  expect_length(found, 1L)
  expect_identical(found[[1]]$payload$pool_keys, c("A", "B"))
  expect_identical(found[[1]]$payload$n_singleton, 2L)
  expect_identical(found[[1]]$stage, 1L)

  # Replicates report the shared finding once.
  found <- capture_singletons(
    sampling_design() |>
      stratify_by(st, alloc = "proportional") |>
      draw(n = 50, min_n = 1) |>
      execute(frame, seed = 1, reps = 3)
  )
  expect_length(found, 1L)
})

test_that("certainty units do not count as the stratum's selections", {
  # a: one selection besides certainty. b: nothing to estimate. c: two drawn.
  frame <- data.frame(
    id = 1:13,
    st = rep(c("a", "b", "c"), c(5, 2, 6)),
    size = c(100, 1, 1, 1, 1, 100, 1, 1, 1, 1, 1, 1, 1)
  )
  found <- capture_singletons(
    sampling_design() |>
      stratify_by(st) |>
      draw(
        n = c(a = 2, b = 2, c = 2), method = "pps_brewer", mos = size,
        certainty_size = 50
      ) |>
      execute(frame, seed = 1)
  )
  expect_length(found, 1L)
  expect_identical(found[[1]]$payload$pool_keys, "a")
})

test_that("a stratum of one unit, taken whole, is not reported", {
  # Nothing is left to estimate in it, so it is no lonely stratum.
  frame <- data.frame(id = 1:21, st = rep(c("a", "b"), c(20, 1)))
  found <- capture_singletons(
    sampling_design() |>
      stratify_by(st) |>
      draw(n = c(a = 2, b = 1)) |>
      execute(frame, seed = 1)
  )
  expect_length(found, 0L)
})

test_that("only fixed-size strata without replacement are judged", {
  frame <- data.frame(
    id = 1:40,
    st = rep(c("a", "b"), each = 20),
    size = 1:40
  )
  quiet <- list(
    random_size = sampling_design() |> stratify_by(st) |>
      draw(frac = 0.05, method = "bernoulli", on_empty = "silent"),
    with_replacement = sampling_design() |> stratify_by(st) |>
      draw(n = 1, method = "pps_multinomial", mos = size),
    unstratified = sampling_design() |> draw(n = 1),
    two_each = sampling_design() |> stratify_by(st) |> draw(n = 2)
  )
  expect_length(quiet, 4L)
  for (name in names(quiet)) {
    found <- capture_singletons(
      suppressWarnings(execute(quiet[[name]], frame, seed = 1))
    )
    expect_length(found, 0L)
  }
  expect_length(
    capture_singletons(execute(
      sampling_design() |> stratify_by(st) |> draw(n = 1), frame, seed = 1
    )),
    1L
  )
})

test_that("a lower stage counts a stratum once in every parent", {
  frame <- data.frame(
    psu = rep(1:10, each = 6),
    g = rep(c("x", "y"), 30),
    id = 1:60
  )
  found <- capture_singletons(
    sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 3) |>
      add_stage() |> stratify_by(g) |> draw(n = 1) |>
      execute(frame, seed = 1)
  )
  expect_length(found, 1L)
  expect_identical(found[[1]]$stage, 2L)
  expect_identical(found[[1]]$payload$n_singleton, 6L)
  expect_length(found[[1]]$payload$pool_keys, 6L)
})

test_that("a pool label steps down the hierarchy once per level", {
  # Levels are joined with " > ", so a "/" inside a value stays part of it,
  # and a stratum keyed by the parent's own identifier is not repeated.
  areas <- data.frame(area = c("a/b", "a/b", "c/d", "c/d"), ea = 1:4)
  homes <- expand.grid(hh = 1:5, ea = 1:4)
  homes$area <- areas$area[homes$ea]
  homes$sex <- rep(c("f", "m"), length.out = nrow(homes))
  take <- data.frame(ea = 1:4, n = c(9, 2, 2, 2))
  by_ea <- sampling_design() |>
    add_stage() |> cluster_by(area) |> draw(n = 2) |>
    add_stage() |> cluster_by(ea) |> draw(n = 2) |>
    add_stage() |> stratify_by(ea) |> draw(n = take)
  w <- capture_one(
    execute(by_ea, list(unique(areas["area"]), areas, homes), seed = 1),
    "samplyr_warning_size_capped"
  )
  expect_identical(w$payload$pool_keys, "a/b > 1")

  cells <- expand.grid(sex = c("f", "m"), ea = 1:4, stringsAsFactors = FALSE)
  cells$n <- ifelse(cells$ea == 1 & cells$sex == "f", 9, 1)
  by_cell <- sampling_design() |>
    add_stage() |> cluster_by(area) |> draw(n = 2) |>
    add_stage() |> cluster_by(ea) |> draw(n = 2) |>
    add_stage() |> stratify_by(ea, sex) |> draw(n = cells)
  w <- capture_one(
    execute(by_cell, list(unique(areas["area"]), areas, homes), seed = 1),
    "samplyr_warning_size_capped"
  )
  expect_identical(w$payload$pool_keys, "a/b > 1 > f")
})
