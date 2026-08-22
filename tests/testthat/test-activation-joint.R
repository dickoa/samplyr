## Joint expectations of the activation

## The exact reference. Given the frozen quotas, a block's panels are an
## arrangement of its label multiset, so the joint law of the labels at two
## positions is obtained by enumerating every ordered pair of distinct
## positions. This assumes nothing the implementation assumes: it counts
## arrangements rather than applying the closed form under test.

enumerate_block <- function(labels, first, second) {
  m <- length(labels)
  in_first <- labels %in% first
  in_second <- labels %in% second

  same <- mean(in_first & in_second)
  distinct <- if (m < 2L) {
    NA_real_
  } else {
    pairs <- 0
    for (a in seq_len(m)) {
      for (b in seq_len(m)) {
        if (a != b && in_first[a] && in_second[b]) {
          pairs <- pairs + 1
        }
      }
    }
    pairs / (m * (m - 1))
  }
  list(marginal = mean(in_first), same = same, distinct = distinct)
}

## Reconstruct a block's label multiset from the record's quotas.
block_labels <- function(pool, block) {
  rep(seq_len(ncol(pool$quotas)), pool$quotas[block, ])
}

joint_frame <- function(n = 400) {
  data.frame(
    id = seq_len(n),
    region = rep(c("North", "South"), each = n / 2),
    mos = rep(c(rep(10, (n / 2) - 3), 900, 950, 1000), times = 2),
    value = seq_len(n) / n
  )
}

joint_rotation <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    wave = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
}

joint_master <- function(seed = 42, n = 60) {
  sampling_design() |>
    stratify_by(region) |>
    draw(n = n) |>
    execute(joint_frame(), seed = seed, panels = joint_rotation())
}

## The closed form against enumeration

test_that("every block's joint expectations match enumerated arrangements", {
  master <- joint_master()
  record <- attr(master, "metadata")$panel_assignment
  schedule <- record$schedule

  for (pair in list(c(1L, 2L), c(1L, 3L), c(2L, 2L), c(4L, 1L))) {
    result <- joint_expectation(master, waves = pair)
    first <- schedule$panel[schedule$wave == pair[1] & schedule$active]
    second <- schedule$panel[schedule$wave == pair[2] & schedule$active]

    expect_identical(unique(result$wave_1), pair[1])
    expect_identical(unique(result$wave_2), pair[2])

    for (i in seq_len(nrow(result))) {
      pool <- record$pools[[result$pool[i]]]
      reference <- enumerate_block(
        block_labels(pool, result$block[i]),
        first,
        second
      )
      expect_equal(result$prob_1[i], reference$marginal)
      expect_equal(result$joint_same[i], reference$same)
      expect_equal(result$joint_distinct[i], reference$distinct)
    }
  }
})

test_that("the within-wave case is the same expression at t equals s", {
  master <- joint_master()
  result <- joint_expectation(master, waves = c(3L, 3L))

  # Both takes are the same take, and the pairwise expectation collapses to
  # a(a-1)/{m(m-1)}.
  expect_identical(result$take_1, result$take_2)
  expect_identical(result$take_1, result$take_both)
  expect_equal(result$joint_same, result$take_1 / result$units)
  expect_equal(
    result$joint_distinct,
    result$take_1 * (result$take_1 - 1) / (result$units * (result$units - 1))
  )
})

test_that("unequal realized quotas are read, not assumed flat", {
  # A pool that is not a multiple of the block size gives one block an extra
  # unit, and its quotas are then unequal. This is the ordinary case, not an
  # edge case, so the fixture asserts the fixture.
  master <- joint_master(seed = 7, n = 82)
  record <- attr(master, "metadata")$panel_assignment
  quotas <- do.call(rbind, lapply(record$pools, function(p) p$quotas))
  uneven <- apply(quotas, 1, function(row) length(unique(row)) > 1L)
  expect_true(any(uneven))

  result <- joint_expectation(master, waves = c(1L, 2L))
  expect_true(any(result$units != min(result$units)))

  # And those blocks still match enumeration.
  for (i in which(result$units > 4L)) {
    pool <- record$pools[[result$pool[i]]]
    schedule <- record$schedule
    reference <- enumerate_block(
      block_labels(pool, result$block[i]),
      schedule$panel[schedule$wave == 1L & schedule$active],
      schedule$panel[schedule$wave == 2L & schedule$active]
    )
    expect_equal(result$joint_distinct[i], reference$distinct)
  }
})

## Published rotation patterns

test_that("published overlap figures are reproduced", {
  # Take-weighted overlap between two waves. With quotas exactly equal, which
  # is what a pool that is a multiple of the block size gives, this is the
  # published proportion of units in common.
  overlap <- function(master, t, s) {
    result <- joint_expectation(master, waves = c(t, s))
    sum(result$take_both) / sum(result$take_1)
  }

  frame <- joint_frame(800)

  # A cyclic in-for-D-out-of-C rotation as a schedule. One spell per cycle,
  # so nothing is affected by a panel returning.
  cyclic <- function(cycle, live) {
    grid <- expand.grid(panel = seq_len(cycle), wave = seq_len(2 * cycle))
    grid$active <- ((grid$wave - grid$panel) %% cycle) < live
    grid
  }

  # The US CPS 4-8-4: in 4 months, out 8, in 4 more, then out of the survey
  # for good. The two spells have to be staggered rather than cycled: a panel
  # that returns to its first spell keeps 7 of 8 groups from month to month,
  # not 6, and the published figure is 6. Panels are therefore declared with
  # a finite life, and the waves compared are interior ones where all eight
  # live groups exist.
  cps <- expand.grid(panel = 1:40, wave = 1:55)
  offset <- cps$wave - cps$panel
  cps$active <- offset %in% c(0:3, 12:15)
  cps_master <- sampling_design() |>
    draw(n = 160) |>
    execute(frame, seed = 3, panels = cps)

  expect_equal(overlap(cps_master, 20L, 21L), 0.75)
  expect_equal(overlap(cps_master, 20L, 32L), 0.50)

  # ONS in-for-15 and in-for-27, quarterly labels aside: consecutive overlap
  # is (D-1)/D and annual overlap (D-12)/D.
  ons15 <- sampling_design() |>
    draw(n = 60) |>
    execute(frame, seed = 4, panels = cyclic(30L, 15L))
  expect_equal(overlap(ons15, 40L, 41L), 14 / 15)
  expect_equal(overlap(ons15, 40L, 52L), 3 / 15)

  ons27 <- sampling_design() |>
    draw(n = 108) |>
    execute(frame, seed = 5, panels = cyclic(54L, 27L))
  expect_equal(overlap(ons27, 60L, 61L), 26 / 27)
  expect_equal(overlap(ons27, 60L, 72L), 15 / 27)
})

## Blocks the formula has to degrade on

test_that("a block of one unit has no distinct pair", {
  # Sub-minimum pools exist: a stratum of a single unit is one block of one.
  frame <- data.frame(
    id = 1:41,
    stratum = c(rep("big", 40), "alone")
  )
  # A stratum of one unit cannot rotate over four panels, so the schedule
  # has to say what happens to it. Promotion keeps the pool in the table,
  # which is what this test is about.
  expect_warning(
    master <- sampling_design() |>
      stratify_by(stratum) |>
      draw(n = c(big = 20, alone = 1)) |>
      execute(
        frame, seed = 2, panels = joint_rotation(),
        small_pool = "permanent"
      ),
    class = "samplyr_warning_panel_small_pool"
  )

  result <- joint_expectation(master, waves = c(1L, 2L))
  singles <- result[result$units == 1L, ]

  expect_gt(nrow(singles), 0L)
  expect_false(any(singles$has_pair))
  expect_true(all(is.na(singles$joint_distinct)))
  # The marginal and the same-unit joint are still exact.
  expect_true(all(singles$joint_same %in% c(0, 1)))
  expect_true(all(singles$prob_1 %in% c(0, 1)))
})

test_that("a permanent pool is certain at every wave and every pair", {
  master <- sampling_design() |>
    draw(n = 30, method = "pps_systematic", mos = mos, certainty_size = 800) |>
    execute(joint_frame(60), seed = 3, panels = joint_rotation())

  result <- joint_expectation(master, waves = c(1L, 3L))
  permanent <- result[result$class == "certainty", ]
  expect_gt(nrow(permanent), 0L)

  # Every take is the whole block, so both marginals, the same-unit joint and
  # the pairwise expectation are one. The last needs no special case: it is
  # (m^2 - m) / {m (m - 1)}.
  expect_identical(permanent$take_1, permanent$units)
  expect_identical(permanent$take_both, permanent$units)
  expect_equal(permanent$prob_1, rep(1, nrow(permanent)))
  expect_equal(permanent$joint_same, rep(1, nrow(permanent)))
  expect_equal(
    permanent$joint_distinct[permanent$has_pair],
    rep(1, sum(permanent$has_pair))
  )

  # The rotating part of the same master is not certain.
  rotating <- result[result$class == "rotating", ]
  expect_true(all(rotating$prob_1 < 1))
})

test_that("no column pre-judges what is estimable", {
  # Whether a pair of waves supports a covariance estimator is T1.2b's
  # ruling, so the table states moments and structure only. A take below two
  # is visible in the takes themselves, without a column naming it a defect:
  # a one-unit certainty block would be labelled non-estimable by such a
  # column while having exactly zero activation variance.
  master <- joint_master()
  result <- joint_expectation(master, waves = c(1L, 2L))

  expect_identical(
    names(result),
    c(
      "pool", "stratum", "class", "activation", "block", "wave_1", "wave_2",
      "units",
      "take_1", "take_2", "take_both", "prob_1", "prob_2", "joint_same",
      "joint_distinct", "has_pair"
    )
  )
  expect_false(any(is.na(result$joint_same)))
})

test_that("an assignment record from another algorithm is refused", {
  # The algorithm and version were frozen so that a later assignment cannot
  # inherit this probability law by writing the same field names.
  master <- joint_master()
  metadata <- attr(master, "metadata")
  metadata$panel_assignment$algorithm <- "some_future_algorithm"
  attr(master, "metadata") <- metadata

  expect_error(
    joint_expectation(master, waves = c(1L, 2L)),
    class = "samplyr_error_panel_record_unsupported"
  )
  # The weights path reads the same quotas and is guarded with it.
  expect_error(
    execute(master, wave = 1),
    class = "samplyr_error_panel_record_unsupported"
  )

  newer <- joint_master()
  metadata <- attr(newer, "metadata")
  metadata$panel_assignment$version <- 99L
  attr(newer, "metadata") <- metadata

  expect_error(
    joint_expectation(newer, waves = c(1L, 2L)),
    class = "samplyr_error_panel_record_unsupported"
  )
})

test_that("only a version this build reads is accepted", {
  # Support is a set, not a ceiling. A record numbered below any schema that
  # existed describes no known law, so it is as unreadable as one from the
  # future, and a version that is not a single whole number states nothing at
  # all.
  master <- joint_master()
  with_version <- function(value) {
    metadata <- attr(master, "metadata")
    metadata$panel_assignment$version <- value
    attr(master, "metadata") <- metadata
    master
  }

  for (bad in list(0L, -5L, 99L, c(1L, 1L), NULL, "1", NA_integer_, 1.5)) {
    expect_error(
      joint_expectation(with_version(bad), waves = c(1L, 2L)),
      class = "samplyr_error_panel_record_unsupported"
    )
  }

  # The supported version is accepted whether or not it is typed as integer.
  expect_s3_class(
    joint_expectation(with_version(1L), waves = c(1L, 2L)),
    "tbl_df"
  )
  expect_s3_class(
    joint_expectation(with_version(1), waves = c(1L, 2L)),
    "tbl_df"
  )

  # A record from the future says so specifically; anything else reports
  # provenance it cannot read.
  expect_match(
    conditionMessage(tryCatch(
      joint_expectation(with_version(99L), waves = c(1L, 2L)),
      error = function(e) e
    )),
    "newer than this version"
  )
  expect_match(
    conditionMessage(tryCatch(
      joint_expectation(with_version(0L), waves = c(1L, 2L)),
      error = function(e) e
    )),
    "does not state a version"
  )
})

## The mode itself

test_that("activation mode refuses what it does not use", {
  master <- joint_master()
  frame <- joint_frame()

  expect_error(
    joint_expectation(master, frame, waves = c(1L, 2L)),
    class = "samplyr_error_joint_activation_arguments"
  )
  expect_error(
    joint_expectation(master, waves = c(1L, 2L), stages = 1),
    class = "samplyr_error_joint_activation_arguments"
  )
  expect_error(
    joint_expectation(master, waves = c(1L, 2L), nsim = 500),
    class = "samplyr_error_joint_activation_arguments"
  )
})

test_that("activation mode needs a pair of declared waves", {
  master <- joint_master()

  expect_error(
    joint_expectation(master, waves = 1L),
    class = "samplyr_error_waves_not_a_pair"
  )
  expect_error(
    joint_expectation(master, waves = c(1L, 2L, 3L)),
    class = "samplyr_error_waves_not_a_pair"
  )
  expect_error(
    joint_expectation(master, waves = c(1, 2.5)),
    class = "samplyr_error_waves_not_a_pair"
  )
  expect_error(
    joint_expectation(master, waves = c(1L, 9L)),
    class = "samplyr_error_wave_undeclared"
  )
})

test_that("activation mode needs a scheduled master", {
  frame <- joint_frame()

  unscheduled <- sampling_design() |>
    draw(n = 60) |>
    execute(frame, seed = 1, panels = 4)
  expect_error(
    joint_expectation(unscheduled, waves = c(1L, 2L)),
    class = "samplyr_error_wave_no_schedule"
  )

  unpartitioned <- sampling_design() |>
    draw(n = 60) |>
    execute(frame, seed = 1)
  expect_error(
    joint_expectation(unpartitioned, waves = c(1L, 2L)),
    class = "samplyr_error_wave_no_schedule"
  )
})

test_that("a materialized wave is refused and points at its master", {
  master <- joint_master()
  materialized <- execute(master, wave = 1)

  expect_error(
    joint_expectation(materialized, waves = c(1L, 2L)),
    class = "samplyr_error_wave_joint_unsupported"
  )
  expect_error(
    joint_expectation(materialized),
    class = "samplyr_error_wave_joint_unsupported"
  )
})

test_that("the stage modes are untouched by the new argument", {
  # A gentle size measure, so no unit is clipped and the draw is silent.
  frame <- data.frame(
    id = 1:400,
    mos = 10 + (seq_len(400) %% 7),
    region = rep(c("North", "South"), each = 200)
  )

  # Poisson joint inclusion probabilities are exactly the outer product of
  # the marginals, so this asserts values rather than shape.
  sample <- sampling_design() |>
    draw(n = 40, method = "pps_poisson", mos = mos) |>
    execute(frame, seed = 8)

  result <- joint_expectation(sample, frame)
  expect_type(result, "list")
  expect_length(result, 1L)

  pi <- 1 / as.data.frame(sample)$.weight_1
  expected <- outer(pi, pi)
  diag(expected) <- pi
  expect_equal(result$stage_1, expected)

  # And a scheduled master answers the stage question the same way.
  scheduled <- sampling_design() |>
    draw(n = 40, method = "pps_poisson", mos = mos) |>
    execute(frame, seed = 8, panels = joint_rotation())
  expect_equal(joint_expectation(scheduled, frame)$stage_1, expected)
})
