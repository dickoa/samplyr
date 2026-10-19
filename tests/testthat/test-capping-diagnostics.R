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

test_that("the stage summary counts the pools that capped", {
  sample <- suppressWarnings(
    execute(short_psu_design(), short_psu_frame(), seed = 1,
            frame_digest = "full")
  )
  recorded <- frame_summary(sample)
  expect_identical(recorded$n_capped, c(0L, 3L))

  preview <- frame_summary(short_psu_design(), short_psu_frame())
  expect_identical(preview$n_capped, c(0L, 3L))

  fits <- frame_summary(short_psu_design(take = 2), short_psu_frame())
  expect_identical(fits$n_capped, c(0L, 0L))

  poisson <- sampling_design() |>
    draw(n = 5, method = "pps_poisson", mos = mos, on_empty = "silent")
  skewed <- data.frame(id = 1:20, mos = c(100, rep(1, 19)))
  expect_identical(frame_summary(poisson, skewed)$n_capped, 0L)
})

test_that("a preview names its pools as execution reports them", {
  frame <- data.frame(
    district = rep(c("d1", "d2"), each = 12),
    urban = rep(rep(c("no", "yes"), each = 6), 2),
    village = rep(paste0("v", 1:8), c(3, 3, 3, 3, 3, 3, 5, 1)),
    id = 1:24
  )
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(district) |>
    draw(n = 2) |>
    add_stage() |>
    stratify_by(urban) |>
    cluster_by(village) |>
    draw(n = 2) |>
    add_stage() |>
    draw(n = 4)

  preview <- frame_summary(design, frame, detail = "pool")
  expect_identical(
    preview$pool_key[preview$stage == 2],
    c("d1 > no", "d1 > yes", "d2 > no", "d2 > yes")
  )
  expect_true(all(is.na(preview$pool_key[preview$stage == 1])))
  expect_identical(
    preview$pool_key[preview$capped],
    c("d1 > v1", "d1 > v2", "d1 > v3", "d1 > v4", "d2 > v5", "d2 > v6",
      "d2 > v8")
  )

  events <- capture_capped(
    sample <- execute(design, frame, seed = 1, frame_digest = "full")
  )
  expect_length(events, 1L)
  expect_setequal(
    events[[1]]$payload$pool_keys,
    preview$pool_key[preview$capped]
  )
  expect_true(all(is.na(frame_summary(sample, detail = "pool")$pool_key)))
})

test_that("nothing is marked capped when nothing caps", {
  sample <- execute(
    short_psu_design(take = 2), short_psu_frame(), seed = 1,
    frame_digest = "full"
  )

  pools <- frame_summary(sample, detail = "pool")
  expect_false(any(pools$capped))
})

test_that("a stratified cube stage reports the pools it caps", {
  frame <- data.frame(stratum = rep(c("A", "B"), c(10, 3)), x = c(1:10, 1:3))
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "cube", aux = x)

  events <- capture_capped(sample <- execute(design, frame, seed = 1))
  expect_length(events, 1L)
  expect_identical(events[[1]]$payload$pool_keys, "B")
  expect_identical(events[[1]]$payload$n_actual, 8L)
  expect_identical(as.vector(table(sample$stratum)), c(5L, 3L))

  preview <- frame_summary(design, frame, detail = "pool")
  expect_identical(preview$stratum[preview$capped], "B")

  exhausted <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 11, method = "cube", aux = x)
  expect_equal(
    count_conditions(
      execute(exhausted, frame, seed = 1),
      "samplyr_warning_census"
    ),
    1L
  )
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

## A stage that took everything without a request above any pool

# Six EAs of four households, three EAs then two households taken, so the
# third stage runs on 6 pools that each hold one household.
one_household_pools <- function() {
  frame <- data.frame(ea = rep(sprintf("e%d", 1:6), each = 4))
  frame$hh <- seq_len(nrow(frame))
  frame
}

subsample_households <- function(...) {
  sampling_design() |>
    add_stage("EAs") |>
    cluster_by(ea) |>
    draw(n = 3) |>
    add_stage("Households") |>
    cluster_by(hh) |>
    draw(n = 2) |>
    add_stage("Subsample") |>
    draw(...)
}

test_that("a fraction that rounds every pool of one unit up is a census", {
  s <- NULL
  w <- capture_one(
    s <- execute(
      subsample_households(frac = 0.5, method = "systematic",
                           round = "nearest"),
      one_household_pools(),
      seed = 1
    ),
    "samplyr_warning_census"
  )

  expect_identical(nrow(s), 6L)
  expect_true(all(s$.weight_3 == 1))
  expect_false(is.null(w))
  expect_identical(w$stage, 3L)
  expect_identical(w$payload$cause, "single_unit")
  expect_equal(
    unlist(w$payload[c("n_pools", "n_actual", "n_available")]),
    c(n_pools = 6, n_actual = 6, n_available = 6)
  )
  expect_match(conditionMessage(w), "single unit")
  expect_match(conditionMessage(w), "second phase")
})

test_that("a capped stage of single-unit pools reports one census with its cause", {
  run <- function() {
    execute(subsample_households(n = 2), one_household_pools(), seed = 1)
  }
  expect_identical(count_conditions(run(), "samplyr_warning_census"), 1L)
  w <- capture_one(run(), "samplyr_warning_census")
  expect_identical(w$payload$cause, "single_unit")
  expect_equal(
    unlist(w$payload[c("n_requested", "n_actual", "n_available")]),
    c(n_requested = 12, n_actual = 6, n_available = 6)
  )
})

test_that("a take of one in pools of one unit is a census", {
  expect_identical(
    count_conditions(
      execute(subsample_households(n = 1), one_household_pools(), seed = 1),
      "samplyr_warning_census"
    ),
    1L
  )
})

test_that("a fraction is a census only when it reaches every pool whole", {
  frame <- data.frame(g = rep(1:5, each = 3), id = 1:15)
  partial <- sampling_design() |>
    stratify_by(g) |>
    draw(frac = 0.2)
  raised <- sampling_design() |>
    stratify_by(g) |>
    draw(frac = 0.2, min_n = 3)

  expect_identical(
    count_conditions(
      execute(partial, frame, seed = 1),
      "samplyr_warning_census"
    ),
    0L
  )
  w <- capture_one(
    execute(raised, frame, seed = 1),
    "samplyr_warning_census"
  )
  expect_identical(w$payload$cause, "min_n")
  expect_match(conditionMessage(w), "min_n")

  # 0.6 of 2 rounds up to 2 in every pool.
  pairs <- data.frame(g = rep(1:4, each = 2), id = 1:8)
  w <- capture_one(
    execute(
      sampling_design() |> stratify_by(g) |> draw(frac = 0.6),
      pairs,
      seed = 1
    ),
    "samplyr_warning_census"
  )
  expect_identical(w$payload$cause, "frac")
  expect_no_match(conditionMessage(w), "min_n")
})

test_that("a take equal to every pool's size is not reported", {
  pairs <- data.frame(g = rep(1:3, each = 2), id = 1:6)
  singles <- data.frame(g = 1:3, id = 1:3)

  # At stage 1, strata of one unit are the frame as given, not a subsample.
  for (case in list(list(pairs, 2), list(singles, 1))) {
    design <- sampling_design() |>
      stratify_by(g) |>
      draw(n = case[[2]])
    expect_identical(
      count_conditions(
        execute(design, case[[1]], seed = 1),
        "samplyr_warning_census"
      ),
      0L
    )
  }
})

test_that("a stage taking only some pools whole is not a census", {
  frame <- data.frame(g = rep(1:2, times = c(1, 4)), id = 1:5)
  design <- sampling_design() |>
    stratify_by(g) |>
    draw(frac = 0.5)

  expect_identical(
    count_conditions(
      execute(design, frame, seed = 1),
      "samplyr_warning_census"
    ),
    0L
  )
})

test_that("a fraction of one declares the census and is not reported", {
  frame <- data.frame(g = rep(1:3, each = 2), id = 1:6)
  scalar <- sampling_design() |>
    stratify_by(g) |>
    draw(frac = 1)
  table <- sampling_design() |>
    stratify_by(g) |>
    draw(frac = data.frame(g = 1:3, frac = 1))

  for (design in list(scalar, table)) {
    expect_identical(
      count_conditions(
        execute(design, frame, seed = 1),
        "samplyr_warning_census"
      ),
      0L
    )
  }
  expect_identical(
    count_conditions(
      execute(subsample_households(frac = 1), one_household_pools(),
              seed = 1),
      "samplyr_warning_census"
    ),
    0L
  )
})

test_that("random-size and with-replacement stages are not judged", {
  for (draw_args in list(
    list(frac = 0.5, method = "bernoulli", on_empty = "silent"),
    list(n = 1, method = "srswr")
  )) {
    expect_identical(
      count_conditions(
        execute(
          do.call(subsample_households, draw_args),
          one_household_pools(),
          seed = 1
        ),
        "samplyr_warning_census"
      ),
      0L,
      label = draw_args$method
    )
  }
})

test_that("replicates report a stage taken whole once", {
  expect_identical(
    count_conditions(
      execute(
        subsample_households(frac = 0.5, method = "systematic"),
        one_household_pools(),
        seed = 1,
        reps = 3
      ),
      "samplyr_warning_census"
    ),
    1L
  )
})

test_that("the message for a stage taken whole names its cause", {
  expect_snapshot({
    invisible(execute(
      subsample_households(frac = 0.5, method = "systematic",
                           round = "nearest"),
      one_household_pools(),
      seed = 1
    ))
    invisible(execute(
      sampling_design() |>
        stratify_by(g) |>
        draw(frac = 0.2, min_n = 3),
      data.frame(g = rep(1:5, each = 3), id = 1:15),
      seed = 1
    ))
  })
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
  # The allocation set the sizes, so the message names it.
  expect_identical(found[[1]]$payload$alloc, "proportional")
  expect_match(conditionMessage(found[[1]]), "shares `n` across the strata")

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
  # The sizes were given per stratum, so no allocation is blamed.
  expect_null(found[[1]]$payload$alloc)
  expect_no_match(conditionMessage(found[[1]]), "`alloc", fixed = TRUE)
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

## Short candidate pools before drawing

test_that("validate_frame() names the candidate pools a take cannot fill", {
  found <- capture_one(
    result <- withVisible(
      validate_frame(short_psu_design(), short_psu_frame())
    ),
    "samplyr_message_capped_pools"
  )
  expect_identical(result, list(value = TRUE, visible = FALSE))
  expect_identical(found$stage, 2L)
  expect_identical(found$payload$n_capped, 3L)
  expect_identical(found$payload$n_pools, 6L)
  preview <- frame_summary(
    short_psu_design(), short_psu_frame(), detail = "pool"
  )
  expect_identical(found$payload$pool_keys, preview$pool_key[preview$capped])

  strata <- data.frame(stratum = rep(c("A", "B"), c(10, 3)), id = 1:13)
  stratified <- sampling_design() |> stratify_by(stratum) |> draw(n = 5)
  found <- capture_one(
    validate_frame(stratified, strata),
    "samplyr_message_capped_pools"
  )
  expect_identical(found$stage, 1L)
  expect_identical(found$payload$pool_keys, "B")
})

test_that("validate_frame() is silent on pools a take fits or cannot cap", {
  quiet <- function(design, frame) {
    count_conditions(
      validate_frame(design, frame),
      "samplyr_message_capped_pools"
    )
  }
  expect_identical(quiet(short_psu_design(take = 2), short_psu_frame()), 0L)

  strata <- data.frame(stratum = rep(c("A", "B"), c(10, 3)), id = 1:13)
  expect_identical(
    quiet(sampling_design() |> stratify_by(stratum) |> draw(frac = 0.5), strata),
    0L
  )
  expect_identical(
    quiet(
      sampling_design() |>
        stratify_by(stratum) |>
        draw(n = 5, method = "bernoulli", on_empty = "silent"),
      strata
    ),
    0L
  )
  expect_identical(
    quiet(
      sampling_design() |> stratify_by(stratum) |> draw(n = 5, method = "srswr"),
      strata
    ),
    0L
  )
})

## Strata an explicit n names that a parent lacks

# Four districts of six villages, PHC and non-PHC alternating, except
# district 4, which has no PHC village. Each village holds eight compounds.
absent_phc_frame <- function() {
  villages <- data.frame(
    district = rep(paste0("d", 1:4), each = 6),
    village = sprintf("v%02d", 1:24),
    phc = rep(c("PHC", "non-PHC"), 12)
  )
  villages$phc[villages$district == "d4"] <- "non-PHC"
  frame <- villages[rep(seq_len(nrow(villages)), each = 8), ]
  frame$compound <- seq_len(nrow(frame))
  rownames(frame) <- NULL
  frame
}

absent_phc_design <- function(n, districts = 4) {
  sampling_design() |>
    add_stage() |>
    cluster_by(district) |>
    draw(n = districts) |>
    add_stage() |>
    stratify_by(phc) |>
    cluster_by(village) |>
    draw(n = n)
}

test_that("a selected parent lacking a named stratum is reported", {
  tables <- list(
    data.frame(phc = c("PHC", "non-PHC"), n = c(2, 2)),
    c(PHC = 2, `non-PHC` = 2)
  )
  for (n in tables) {
    found <- capture_one(
      sample <- execute(absent_phc_design(n), absent_phc_frame(), seed = 1),
      "samplyr_message_absent_strata"
    )
    expect_identical(found$stage, 2L)
    expect_identical(found$payload$pool_keys, "d4 > PHC")
    expect_identical(found$payload$n_absent, 1L)
    villages <- unique(sample[c("district", "village")])
    expect_identical(as.vector(table(villages$district)), c(4L, 4L, 4L, 2L))
  }
})

test_that("validate_frame() reports the candidate parents lacking a stratum", {
  design <- absent_phc_design(
    data.frame(phc = c("PHC", "non-PHC"), n = c(2, 2)),
    districts = 2
  )
  found <- capture_one(
    validate_frame(design, absent_phc_frame()),
    "samplyr_message_absent_strata"
  )
  expect_identical(found$stage, 2L)
  expect_identical(found$payload$pool_keys, "d4 > PHC")
})

test_that("strata a parent need not hold are not reported", {
  count_absent <- function(design) {
    count_conditions(
      execute(design, absent_phc_frame(), seed = 1),
      "samplyr_message_absent_strata"
    ) +
      count_conditions(
        validate_frame(design, absent_phc_frame()),
        "samplyr_message_absent_strata"
      )
  }
  # A scalar n names no strata.
  expect_identical(count_absent(absent_phc_design(2)), 0L)
  # Per-parent takes keyed by the parent's own identifier.
  per_parent <- sampling_design() |>
    add_stage() |>
    cluster_by(district) |>
    draw(n = 4) |>
    add_stage() |>
    stratify_by(district) |>
    cluster_by(village) |>
    draw(n = data.frame(district = paste0("d", 1:4), n = 3))
  expect_identical(count_absent(per_parent), 0L)
})

test_that("replicates report an absent stratum once", {
  n <- data.frame(phc = c("PHC", "non-PHC"), n = c(2, 2))
  expect_identical(
    count_conditions(
      execute(absent_phc_design(n), absent_phc_frame(), seed = 1, reps = 3),
      "samplyr_message_absent_strata"
    ),
    1L
  )
})

## Units an allocation moved, before drawing

test_that("a preview counts the units an allocation moved", {
  frame <- data.frame(stratum = rep(c("A", "B", "C"), c(1, 10, 10)), id = 1:21)
  equal <- sampling_design() |>
    stratify_by(stratum, alloc = "equal") |>
    draw(n = 9)
  preview <- frame_summary(equal, frame)
  expect_identical(preview$n_moved, 2)
  pools <- frame_summary(equal, frame, detail = "pool")
  expect_identical(pools$n_target, c(1, 4, 4))
  expect_false(any(pools$capped))

  fits <- sampling_design() |>
    stratify_by(stratum, alloc = "equal") |>
    draw(n = 3)
  expect_identical(frame_summary(fits, frame)$n_moved, 0)

  per_stratum <- sampling_design() |> stratify_by(stratum) |> draw(n = 1)
  expect_identical(frame_summary(per_stratum, frame)$n_moved, NA_real_)

  sample <- suppressMessages(execute(equal, frame, seed = 1))
  expect_identical(frame_summary(sample)$n_moved, NA_real_)
})

test_that("a later stage's moved units roll up like its target", {
  # Two districts, one drawn. Each moves 2 units under equal allocation.
  frame <- data.frame(
    district = rep(c("d1", "d2"), each = 21),
    stratum = rep(rep(c("A", "B", "C"), c(1, 10, 10)), 2),
    id = 1:42
  )
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(district) |>
    draw(n = 1) |>
    add_stage() |>
    stratify_by(stratum, alloc = "equal") |>
    draw(n = 9)
  preview <- frame_summary(design, frame)
  expect_identical(preview$n_moved, c(NA_real_, 2))
  expect_identical(preview$n_target, c(1, 9))
})

## The split an allocation gives a stage

# validate_frame() states what a shared n became, since reading a total as
# a per-stratum count is the most common misreading of a stratified design.
capture_allocation <- function(expr) {
  found <- list()
  withCallingHandlers(
    force(expr),
    samplyr_message_allocation = function(m) {
      found[[length(found) + 1L]] <<- m
      invokeRestart("muffleMessage")
    }
  )
  found
}

test_that("validate_frame() states the split of a shared n", {
  frame <- data.frame(
    id = 1:11,
    region = rep(c("North", "South", "West"), c(4, 4, 3))
  )
  equal <- sampling_design() |>
    stratify_by(region, alloc = "equal") |>
    draw(n = 5)
  found <- capture_allocation(validate_frame(equal, frame))
  expect_length(found, 1L)
  x <- found[[1]]$payload
  expect_identical(found[[1]]$stage, 1L)
  expect_identical(x$alloc, "equal")
  expect_identical(x$n_parents, 1L)
  expect_identical(x$pool_keys, c("North", "South", "West"))
  expect_identical(x$sizes, c(2, 2, 1))
  # Three equal remainders for two units: label order decided.
  expect_identical(x$n_tied, 1L)
  text <- conditionMessage(found[[1]])
  expect_match(text, "`n = 5` would be taken in each stratum", fixed = TRUE)
  expect_match(text, "equal remainders", fixed = TRUE)

  # The message comes from the check, not from drawing.
  expect_length(
    suppressMessages(capture_allocation(execute(equal, frame, seed = 1))),
    0L
  )
  per_stratum <- sampling_design() |> stratify_by(region) |> draw(n = 2)
  expect_length(capture_allocation(validate_frame(per_stratum, frame)), 0L)
})

test_that("an equal remainder goes to the larger target first", {
  # 9 and 7 units share 8 as 4.5 and 3.5. The larger target gets the unit,
  # whatever the labels, and the message says a tie was broken.
  village <- function(first, second) {
    data.frame(id = 1:16, kind = rep(c(first, second), c(9, 7)))
  }
  design <- sampling_design() |>
    stratify_by(kind, alloc = "proportional") |>
    draw(n = 8)
  for (labels in list(c("a", "b"), c("b", "a"))) {
    frame <- village(labels[[1]], labels[[2]])
    x <- capture_allocation(validate_frame(design, frame))[[1]]$payload
    expect_identical(x$pool_keys, labels)
    expect_identical(x$sizes, c(5, 3))
    expect_identical(x$n_tied, 1L)
  }

  # 10 and 6 share 8 as 5 and 3 with no remainder to break.
  frame <- data.frame(id = 1:16, kind = rep(c("a", "b"), c(10, 6)))
  x <- capture_allocation(validate_frame(design, frame))[[1]]$payload
  expect_identical(x$sizes, c(5, 3))
  expect_identical(x$n_tied, 0L)
  expect_no_match(
    conditionMessage(capture_allocation(validate_frame(design, frame))[[1]]),
    "remainders"
  )
})

test_that("a later stage reports its split across the candidate parents", {
  frame <- data.frame(
    district = rep(c("d1", "d2", "d3"), each = 10),
    kind = rep(rep(c("a", "b"), c(5, 5)), 3),
    id = 1:30
  )
  frame$kind[frame$district == "d3"] <- rep(c("a", "b"), c(7, 3))
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(district) |>
    draw(n = 2) |>
    add_stage() |>
    stratify_by(kind, alloc = "proportional") |>
    draw(n = 4)
  found <- capture_allocation(validate_frame(design, frame))
  expect_length(found, 1L)
  x <- found[[1]]$payload
  expect_identical(found[[1]]$stage, 2L)
  expect_identical(x$n_parents, 3L)
  # d1 and d2 split 4 as 2 and 2, d3 as 2.8 and 1.2.
  expect_identical(x$sizes, c(2, 2, 2, 2, 3, 1))
  expect_identical(x$n_tied, 0L)
  expect_match(
    conditionMessage(found[[1]]),
    "Stratum takes range from 1 to 3.", fixed = TRUE
  )
})

test_that("rounding marks the strata a tie decided", {
  tied <- function(x, n) {
    attr(round_preserve_total_bounded(x, n, rep(0, length(x)), rep(n, length(x))), "tied")
  }
  expect_identical(tied(c(1.5, 1.5), 3), c(TRUE, TRUE))
  expect_identical(tied(c(5, 5, 5) / 3, 5), c(TRUE, TRUE, TRUE))
  expect_identical(tied(c(4.5, 3.5), 8), c(TRUE, TRUE))
  # Only the strata level with the cut were decided by the order.
  expect_identical(tied(c(1.5, 1.5, 1.2), 4), c(TRUE, TRUE, FALSE))
  expect_identical(tied(c(1.7, 1.5, 1.5, 1.3), 6), c(FALSE, TRUE, TRUE, FALSE))
  expect_identical(tied(c(1.6, 1.4), 3), c(FALSE, FALSE))
  expect_identical(tied(c(2, 2, 2), 6), c(FALSE, FALSE, FALSE))
  # Every unit placed, so nothing was left out to tie with.
  expect_identical(tied(c(1.5, 1.5), 4), c(FALSE, FALSE))
})

test_that("a preview marks the pools a tie decided", {
  frame <- data.frame(id = 1:16, kind = rep(c("a", "b", "c"), c(6, 6, 4)))
  # 6, 6 and 4 share 5 as 1.875, 1.875 and 1.25: a and b tie for one unit.
  design <- sampling_design() |>
    stratify_by(kind, alloc = "proportional") |>
    draw(n = 5)
  pools <- frame_summary(design, frame, detail = "pool")
  expect_identical(pools$n_target, c(2, 2, 1))
  expect_identical(pools$tied, c(FALSE, FALSE, FALSE))
  design4 <- sampling_design() |>
    stratify_by(kind, alloc = "proportional") |>
    draw(n = 4)
  # 1.5, 1.5 and 1: the floors take 3 and a and b tie for the fourth.
  pools <- frame_summary(design4, frame, detail = "pool")
  expect_identical(pools$n_target, c(2, 1, 1))
  expect_identical(pools$tied, c(TRUE, TRUE, FALSE))
  expect_identical(frame_summary(design4, frame)$n_tied, 2L)

  # Pools follow the frame, the allocation the labels: the marks follow
  # each pool, not the position.
  reversed <- frame[nrow(frame):1, ]
  pools <- frame_summary(design4, reversed, detail = "pool")
  expect_identical(pools$pool_key, c("c", "b", "a"))
  expect_identical(pools$n_target, c(1, 1, 2))
  expect_identical(pools$tied, c(FALSE, TRUE, TRUE))

  # Without an allocation method, and once drawn, nothing is known.
  per_stratum <- sampling_design() |> stratify_by(kind) |> draw(n = 1)
  expect_identical(frame_summary(per_stratum, frame)$n_tied, NA_integer_)
  expect_true(all(is.na(frame_summary(per_stratum, frame, detail = "pool")$tied)))
  sample <- suppressMessages(execute(design4, frame, seed = 1))
  expect_identical(frame_summary(sample)$n_tied, NA_integer_)
  expect_true(all(is.na(frame_summary(sample, detail = "pool")$tied)))
})

test_that("a later stage counts the tied pools of every candidate parent", {
  # d1 and d2 split 3 over 2 and 2 villages (a tie), d3 over 3 and 1.
  frame <- data.frame(
    district = rep(c("d1", "d2", "d3"), each = 4),
    kind = c(rep(c("a", "b"), 2, each = 2)[1:8], "a", "a", "a", "b"),
    id = 1:12
  )
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(district) |>
    draw(n = 2) |>
    add_stage() |>
    stratify_by(kind, alloc = "proportional") |>
    draw(n = 3)
  pools <- frame_summary(design, frame, detail = "pool")
  pools <- pools[pools$stage == 2L, ]
  expect_identical(pools$tied, c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))
  summary <- frame_summary(design, frame)
  expect_identical(summary$n_tied, c(NA_integer_, 4L))
  found <- capture_allocation(validate_frame(design, frame))
  expect_identical(found[[1]]$payload$n_tied, 2L)
})

## A declared shortfall policy

strict_psu_design <- function(on_capped = "error") {
  sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage("unit") |>
    draw(n = 10, on_capped = on_capped)
}

test_that("on_capped = \"error\" refuses before anything is drawn", {
  set.seed(99)
  before <- .Random.seed
  err <- expect_error(
    execute(strict_psu_design(), short_psu_frame(), seed = 1),
    class = "samplyr_error_size_capped"
  )
  expect_identical(.Random.seed, before)
  expect_identical(err$stage, 2L)
  expect_identical(err$payload$n_capped, 3L)
  expect_identical(err$payload$n_pools, 6L)
  expect_identical(err$payload$pool_keys, c("c1", "c2", "c3"))

  # Every seed is refused, whether or not it would reach a short pool.
  for (seed in 1:5) {
    expect_error(
      execute(strict_psu_design(), short_psu_frame(), seed = seed),
      class = "samplyr_error_size_capped"
    )
  }
})

test_that("on_capped = \"warn\" keeps the take-all rule", {
  strict <- strict_psu_design("warn")
  default <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage("unit") |>
    draw(n = 10)
  a <- suppressWarnings(execute(strict, short_psu_frame(), seed = 1))
  b <- suppressWarnings(execute(default, short_psu_frame(), seed = 1))
  expect_identical(a$id, b$id)
  expect_identical(a$.weight, b$.weight)
  expect_identical(
    exante_probabilities(strict, short_psu_frame(), key = id),
    exante_probabilities(default, short_psu_frame(), key = id)
  )
})

test_that("the strict policy refuses alike in validation and probabilities", {
  expect_error(
    validate_frame(strict_psu_design(), short_psu_frame()),
    class = "samplyr_error_size_capped"
  )
  expect_error(
    exante_probabilities(strict_psu_design(), short_psu_frame(), key = id),
    class = "samplyr_error_size_capped"
  )
  # The preview stays available, to find the short pools.
  preview <- frame_summary(strict_psu_design(), short_psu_frame())
  expect_identical(preview$n_capped, c(0L, 3L))
})

test_that("a staged execution checks a stage when its frame arrives", {
  frame <- short_psu_frame()
  # Stage 1 alone has no take to check, so it runs.
  first <- execute(strict_psu_design(), frame, stages = 1, seed = 1)
  expect_identical(sort(unique(first$psu)), c("c1", "c4"))
  listing <- frame[frame$psu %in% first$psu, ]
  expect_error(
    execute(first, listing, seed = 2),
    class = "samplyr_error_size_capped"
  )

  full <- data.frame(psu = rep(paste0("c", 1:6), each = 50), id = 1:300)
  fits <- execute(strict_psu_design(), full, stages = 1, seed = 1)
  expect_no_error(
    execute(fits, full[full$psu %in% fits$psu, ], seed = 2)
  )
})

test_that("an allocated total above the population counts as capped", {
  frame <- data.frame(stratum = rep(c("A", "B"), c(3, 4)), id = 1:7)
  over <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 9, on_capped = "error")
  err <- expect_error(
    execute(over, frame, seed = 1),
    class = "samplyr_error_size_capped"
  )
  expect_identical(err$payload$pool_keys, c("A", "B"))

  # Redistribution that delivers the total is the allocation rule.
  moved <- sampling_design() |>
    stratify_by(stratum, alloc = "equal") |>
    draw(n = 7, min_n = 1, on_capped = "error")
  expect_no_error(suppressMessages(execute(moved, frame, seed = 1)))
})

test_that("on_capped = \"error\" is refused where no fixed take falls short", {
  base <- sampling_design()
  for (design in list(
    function() base |> draw(n = 5, method = "bernoulli", on_capped = "error"),
    function() {
      base |>
        draw(n = 5, method = "pps_poisson", mos = m, on_capped = "error")
    },
    function() base |> draw(n = 5, method = "srswr", on_capped = "error"),
    function() base |> draw(frac = 0.1, on_capped = "error")
  )) {
    expect_error(design(), class = "samplyr_error_draw_argument")
  }
})

test_that("the design prints a strict policy and nothing by default", {
  expect_match(
    paste(capture.output(print(strict_psu_design())), collapse = "\n"),
    "on_capped = error",
    fixed = TRUE
  )
  expect_no_match(
    paste(capture.output(print(short_psu_design())), collapse = "\n"),
    "on_capped",
    fixed = TRUE
  )
})

test_that("absent strata are matched on values, not on their labels", {
  # Joined with "/", the two strata read "a/b/c" alike.
  frame <- data.frame(
    parent = rep(c("P1", "P2"), each = 4),
    a = rep(c("a/b", "a"), each = 4),
    b = rep(c("c", "b/c"), each = 4),
    id = 1:8
  )
  sizes <- data.frame(a = c("a/b", "a"), b = c("c", "b/c"), n = c(2, 2))
  design <- sampling_design() |>
    cluster_by(parent) |>
    draw(n = 2) |>
    add_stage() |>
    stratify_by(a, b) |>
    draw(n = sizes)
  found <- capture_one(
    validate_frame(design, frame),
    "samplyr_message_absent_strata"
  )
  expect_identical(found$payload$n_absent, 2L)
  expect_setequal(found$payload$pool_keys, c("P1 > a/b/c", "P2 > a/b/c"))
})

test_that("a named vector matches a numeric stratum column", {
  frame <- absent_phc_frame()
  frame$phc <- as.integer(frame$phc == "PHC")
  found <- capture_one(
    validate_frame(absent_phc_design(c(`0` = 2, `1` = 2), districts = 2), frame),
    "samplyr_message_absent_strata"
  )
  expect_identical(found$payload$pool_keys, "d4 > 1")
})
