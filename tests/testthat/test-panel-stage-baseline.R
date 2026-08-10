## B0. Semantic baselines for stage-aware panel assignment
#
# Panel assignment is about to become stage-aware: the unit it assigns, the
# pools it assigns within and the record it writes all stop being specific to
# stage 1. These tests pin what stage-1 assignment does now, in values rather
# than in counts, so that a refactor which preserves the behavior is
# distinguishable from one that merely still runs.
#
# What is pinned: the selected rows and their base weights under a fixed seed,
# the realized `.panel` labels, the assignment record down to each pool's
# keys, block sizes and quotas, the rows and activation factors of a
# materialized wave, the conditional joint probabilities, the serialized
# receipt, the exported survey structure, and the number of random draws the
# assignment consumes.
#
# These values are a description of the current implementation, not a
# derivation from the design. A change to any of them is a change to what a
# user's `.panel` column contains, so it belongs in a deliberate commit rather
# than in a refactor.
#
# Stage-aware assignment changed exactly four of them, in the commit that made
# a lower assignment stage reachable, and nothing else:
#
#   1. the record `version`, 2 to 3;
#   2. the fields the record carries, `assignment_stage` and `pool_vars`;
#   3. the `unit` vocabulary, `psu` and `element` to `cluster`, `occurrence`
#      and `element`, which now name the sampling law rather than the column
#      implementing the key;
#   4. the assignment stage's own strata joining `key_vars`, which changes
#      every recorded pool key of a stratified clustered stage.
#
# Nothing else moved. The selected rows, base weights, `.panel` labels, pool
# membership, block sizes, quotas, wave rows, activation factors, joint
# probabilities and the number of random draws consumed are all still the
# values first recorded here.

## B0.1 Fixtures

# Unstratified elements, scalar panels: the simplest assignment there is.
b0_element_frame <- function() {
  data.frame(id = 1:60, x = seq_len(60) / 60)
}

b0_element <- function() {
  sampling_design() |>
    draw(n = 12) |>
    execute(b0_element_frame(), seed = 101, panels = 4)
}

# Stratified clusters over elements, with a 2-of-4 rotation. Four clusters
# per stratum against four panels is exactly one block, so every panel holds
# one cluster and every wave takes two.
b0_cluster_frame <- function() {
  data.frame(
    stratum = rep(c("A", "B"), each = 30),
    cluster = rep(sprintf("c%02d", 1:12), each = 5),
    unit = rep(1:5, times = 12),
    y = seq_len(60)
  )
}

b0_rotation_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 3),
    wave = rep(1:3, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE
    )
  )
}

b0_clustered <- function() {
  sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 4) |>
    add_stage() |>
    draw(n = 2) |>
    execute(b0_cluster_frame(), seed = 202, panels = b0_rotation_schedule())
}

# A multi-hit first stage. Cluster g05 is selected three times, so the
# assignment unit is the realized draw rather than the population cluster.
b0_multihit_frame <- function() {
  data.frame(
    cluster = rep(sprintf("g%02d", 1:10), each = 6),
    unit = rep(1:6, times = 10),
    mos = rep(seq(10, 100, by = 10), each = 6),
    y = seq_len(60)
  )
}

b0_multihit <- function() {
  sampling_design() |>
    add_stage() |>
    cluster_by(cluster) |>
    draw(n = 6, method = "pps_multinomial", mos = mos) |>
    add_stage() |>
    draw(n = 2) |>
    execute(b0_multihit_frame(), seed = 300, panels = 2)
}

# A stratified multi-hit stage: the draw index restarts at 1 inside every
# stratum, so it identifies an occurrence only once the stratum is known.
b0_stratified_multihit_frame <- function() {
  data.frame(
    stratum = rep(c("A", "B"), each = 30),
    cluster = rep(sprintf("h%02d", 1:12), each = 5),
    unit = rep(1:5, times = 12),
    mos = rep(rep(seq(10, 60, by = 10), times = 2), each = 5),
    y = seq_len(60)
  )
}

b0_stratified_multihit <- function() {
  sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 4, method = "pps_multinomial", mos = mos) |>
    add_stage() |>
    draw(n = 2) |>
    execute(b0_stratified_multihit_frame(), seed = 606, panels = 2)
}

# A terminal unclustered stage drawn with replacement: unit 23 is selected
# twice and the two rows are separate assignment units.
b0_terminal_multihit <- function() {
  sampling_design() |>
    draw(n = 10, method = "srswr") |>
    execute(data.frame(id = 1:40, y = seq_len(40)), seed = 707, panels = 2)
}

# One cluster large enough to be selected with certainty, under a schedule.
b0_certainty <- function() {
  frame <- data.frame(
    cluster = sprintf("k%02d", 1:12),
    mos = c(500, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30),
    y = seq_len(12)
  )
  schedule <- data.frame(
    panel = rep(1:2, times = 2),
    wave = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 5, method = "pps_systematic", mos = mos) |>
    execute(frame, seed = 404, panels = schedule)
}

# A stratum too small to rotate, promoted rather than refused.
b0_promoted <- function() {
  frame <- data.frame(
    stratum = rep(c("S1", "S2"), times = c(4, 40)),
    id = 1:44,
    y = seq_len(44)
  )
  schedule <- data.frame(
    panel = rep(1:4, times = 2),
    wave = rep(1:2, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
  suppressWarnings(
    sampling_design() |>
      stratify_by(stratum) |>
      draw(n = c(S1 = 2, S2 = 20)) |>
      execute(frame, seed = 505, panels = schedule, small_pool = "permanent")
  )
}

b0_record <- function(x) attr(x, "metadata")$panel_assignment

## B0.2 Unstratified element assignment

test_that("element assignment selects and labels a fixed sample", {
  result <- b0_element()

  expect_identical(
    result$id,
    c(9L, 57L, 46L, 31L, 55L, 17L, 59L, 35L, 3L, 32L, 60L, 51L)
  )
  expect_identical(result$.weight, rep(5, 12))
  expect_identical(result$.panel, c(1L, 3L, 3L, 4L, 2L, 2L, 4L, 4L, 1L, 3L, 1L, 2L))
})

test_that("the element assignment record is exact", {
  record <- b0_record(b0_element())

  expect_identical(record$algorithm, "blocked_random_quota")
  expect_identical(record$version, 3L)
  expect_identical(record$panels, 4L)
  expect_identical(record$assignment_stage, 1L)
  expect_identical(record$block_size, 8L)
  expect_identical(record$r_min, 1L)
  expect_identical(record$unit, "element")
  expect_identical(record$key_vars, ".sample_id")
  # Unstratified: one pool over everything, named by no column.
  expect_identical(record$pool_vars, character(0))
  expect_false(record$control_ordered)
  expect_identical(record$certainty, "permanent")
  expect_identical(record$small_pool_policy, "error")
  expect_null(record$schedule)

  expect_length(record$pools, 1L)
  pool <- record$pools[[1]]
  expect_null(pool$stratum)
  expect_identical(pool$class, "rotating")
  expect_identical(pool$activation, "rotating")
  expect_identical(pool$permanent_reason, NA_character_)
  expect_identical(pool$size, 12L)
  expect_identical(pool$keys, as.character(1:12))
  expect_identical(pool$blocks, 12L)
  expect_identical(pool$quotas, matrix(3L, nrow = 1, ncol = 4))
})

## B0.3 Stratified clustered assignment

test_that("clustered assignment selects and labels a fixed sample", {
  result <- b0_clustered()

  expect_identical(
    result$cluster,
    rep(c("c01", "c04", "c05", "c06", "c07", "c09", "c11", "c12"), each = 2)
  )
  expect_identical(
    result$unit,
    c(3L, 4L, 1L, 5L, 3L, 5L, 5L, 4L, 3L, 4L, 1L, 2L, 5L, 4L, 1L, 5L)
  )
  expect_identical(result$.weight, rep(3.75, 16))
  # Every cluster's two elements carry one label: the assignment unit is the
  # cluster and the elements inherit it.
  expect_identical(
    result$.panel,
    rep(c(3L, 1L, 4L, 2L, 3L, 4L, 2L, 1L), each = 2)
  )
})

test_that("the clustered assignment record is exact", {
  record <- b0_record(b0_clustered())

  expect_identical(record$version, 3L)
  expect_identical(record$panels, 4L)
  expect_identical(record$assignment_stage, 1L)
  expect_identical(record$block_size, 4L)
  expect_identical(record$r_min, 2L)
  # Without replacement, so a cluster and not an occurrence.
  expect_identical(record$unit, "cluster")
  expect_identical(record$key_vars, c("stratum", "cluster"))
  expect_identical(record$pool_vars, "stratum")
  expect_identical(record$small_pool_policy, "error")
  expect_identical(nrow(record$schedule), 12L)

  expect_length(record$pools, 2L)
  expect_identical(
    lapply(record$pools, function(p) p$stratum$stratum),
    list("A", "B")
  )
  # The stratum qualifies the key, so a pool key is the compound identity
  # rather than the bare cluster label.
  expect_identical(
    lapply(record$pools, function(p) p$keys),
    list(
      samplyr:::make_group_key(
        data.frame(stratum = "A", cluster = c("c01", "c04", "c05", "c06")),
        c("stratum", "cluster")
      ),
      samplyr:::make_group_key(
        data.frame(stratum = "B", cluster = c("c07", "c09", "c11", "c12")),
        c("stratum", "cluster")
      )
    )
  )
  for (pool in record$pools) {
    expect_identical(pool$class, "rotating")
    expect_identical(pool$activation, "rotating")
    expect_identical(pool$size, 4L)
    expect_identical(pool$blocks, 4L)
    expect_identical(pool$quotas, matrix(1L, nrow = 1, ncol = 4))
  }
})

test_that("a materialized wave keeps fixed rows, factors and probabilities", {
  wave <- execute(b0_clustered(), wave = 2)

  expect_identical(
    wave$cluster,
    rep(c("c01", "c06", "c07", "c11"), each = 2)
  )
  expect_identical(wave$unit, c(3L, 4L, 5L, 4L, 3L, 4L, 5L, 4L))
  expect_identical(wave$.panel, rep(c(3L, 2L, 3L, 2L), each = 2))
  # 3.75 master weight over an activation probability of 1/2.
  expect_identical(wave$.weight, rep(7.5, 8))

  record <- attr(wave, "metadata")$wave
  expect_identical(record$active_panels, c(2L, 3L))
  expect_identical(
    lapply(record$pools, function(p) p$take),
    list(2L, 2L)
  )
  expect_identical(
    lapply(record$pools, function(p) p$probability),
    list(0.5, 0.5)
  )
})

test_that("cross-wave joint probabilities are exact", {
  joint <- as.data.frame(joint_expectation(b0_clustered(), waves = c(1, 2)))

  expect_identical(joint$block, c(1L, 1L))
  expect_identical(joint$units, c(4L, 4L))
  expect_identical(joint$take_1, c(2L, 2L))
  expect_identical(joint$take_2, c(2L, 2L))
  expect_identical(joint$take_both, c(1L, 1L))
  expect_identical(joint$prob_1, c(0.5, 0.5))
  expect_identical(joint$prob_2, c(0.5, 0.5))
  expect_identical(joint$joint_same, c(0.25, 0.25))
  expect_identical(joint$joint_distinct, c(0.25, 0.25))
  expect_identical(joint$has_pair, c(TRUE, TRUE))
})

test_that("the exported wave keeps its structure, estimate and error", {
  skip_if_not_installed("survey")
  design <- as_svydesign(execute(b0_clustered(), wave = 2))

  expect_s3_class(design, "twophase2")
  # Degrees of freedom, not the standard error, are what distinguish a
  # two-phase export from a single-phase treatment of the same rows.
  expect_identical(survey::degf(design), 2L)

  total <- survey::svytotal(~y, design)
  expect_equal(as.numeric(coef(total)), 1815, tolerance = 1e-9)
  expect_equal(as.numeric(survey::SE(total)), 409.383683114, tolerance = 1e-9)
})

## B0.4 Multi-hit ancestor

test_that("a repeated cluster is assigned once per occurrence", {
  result <- b0_multihit()

  expect_identical(
    result$cluster,
    rep(c("g03", "g05", "g05", "g05", "g06", "g10"), each = 2)
  )
  expect_identical(result$.draw_1, rep(1:6, each = 2))
  # g05 is selected three times and its occurrences take panels 2, 1 and 2:
  # the population cluster is not the assignment unit.
  expect_identical(result$.panel, rep(c(2L, 2L, 1L, 2L, 1L, 1L), each = 2))
  expect_equal(
    result$.weight,
    rep(c(9.166667, 5.5, 5.5, 5.5, 4.583333, 2.75), each = 2),
    tolerance = 1e-6
  )
})

test_that("the multi-hit record keys on the draw occurrence", {
  record <- b0_record(b0_multihit())

  expect_identical(record$key_vars, c("cluster", ".draw_1"))
  # The stage can select one cluster twice, so its units are occurrences.
  expect_identical(record$unit, "occurrence")
  expect_identical(record$block_size, 4L)

  pool <- record$pools[[1]]
  expect_identical(pool$size, 6L)
  # Three distinct keys for one cluster.
  expect_identical(
    pool$keys,
    samplyr:::make_group_key(
      data.frame(
        cluster = c("g03", "g05", "g05", "g05", "g06", "g10"),
        .draw_1 = 1:6
      ),
      c("cluster", ".draw_1")
    )
  )
  expect_length(unique(pool$keys), 6L)
  expect_identical(pool$blocks, 6L)
  expect_identical(pool$quotas, matrix(3L, nrow = 1, ncol = 2))
})

## B0.5 Stratified multi-hit

test_that("the draw index restarts inside each stratum", {
  result <- b0_stratified_multihit()

  expect_identical(
    result$stratum,
    rep(c("A", "B"), each = 8)
  )
  expect_identical(
    result$cluster,
    rep(c("h03", "h03", "h04", "h06", "h07", "h09", "h11", "h12"), each = 2)
  )
  # Both strata number their occurrences from 1, so the draw index alone
  # names an occurrence in neither of them.
  expect_identical(result$.draw_1, rep(c(2L, 4L, 1L, 3L, 2L, 4L, 3L, 1L), each = 2))
  expect_identical(sort(result$.draw_1[1:8]), sort(result$.draw_1[9:16]))
  expect_identical(result$.panel, rep(c(2L, 1L, 2L, 1L, 1L, 2L, 1L, 2L), each = 2))
})

test_that("stratified multi-hit pools separate their occurrences", {
  record <- b0_record(b0_stratified_multihit())

  expect_identical(record$key_vars, c("stratum", "cluster", ".draw_1"))
  expect_identical(record$pool_vars, "stratum")
  expect_length(record$pools, 2L)
  expect_identical(
    lapply(record$pools, function(p) p$stratum$stratum),
    list("A", "B")
  )
  # Cluster h03 is hit twice within stratum A and holds two keys.
  expect_identical(
    record$pools[[1]]$keys,
    samplyr:::make_group_key(
      data.frame(
        stratum = "A",
        cluster = c("h03", "h03", "h04", "h06"),
        .draw_1 = c(2L, 4L, 1L, 3L)
      ),
      c("stratum", "cluster", ".draw_1")
    )
  )
  # Every key is unique across pools as well as within them.
  all_keys <- unlist(lapply(record$pools, function(p) p$keys))
  expect_length(unique(all_keys), 8L)
  for (pool in record$pools) {
    expect_identical(pool$size, 4L)
    expect_identical(pool$blocks, 4L)
    expect_identical(pool$quotas, matrix(2L, nrow = 1, ncol = 2))
  }
})

## B0.6 Terminal unclustered multi-hit

test_that("a repeated element row is its own assignment unit", {
  result <- b0_terminal_multihit()

  expect_identical(
    result$id,
    c(23L, 36L, 37L, 28L, 30L, 10L, 1L, 32L, 19L, 23L)
  )
  expect_identical(result$.draw_1, 1:10)
  # Unit 23 is selected twice and its two rows take different panels.
  expect_identical(result$.panel, c(2L, 2L, 2L, 1L, 1L, 2L, 1L, 1L, 2L, 1L))
  expect_identical(result$.weight, rep(4, 10))
})

test_that("the terminal multi-hit record keys on the sample row", {
  record <- b0_record(b0_terminal_multihit())

  expect_identical(record$key_vars, ".sample_id")
  # The key is `.sample_id` because a row is the safest thing to locate, and
  # the unit is an occurrence because the law is with replacement. The
  # vocabulary describes the law, not the column.
  expect_identical(record$unit, "occurrence")

  pool <- record$pools[[1]]
  expect_identical(pool$size, 10L)
  expect_identical(pool$keys, as.character(1:10))
  expect_identical(pool$blocks, c(5L, 5L))
  expect_identical(pool$quotas, matrix(c(2L, 3L, 3L, 2L), nrow = 2))
})

## B0.7 Certainty pools

test_that("a certainty unit is a permanent pool of its own", {
  result <- b0_certainty()

  expect_identical(result$cluster, c("k01", "k05", "k08", "k10", "k12"))
  expect_identical(result$.certainty_1, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(result$.panel, c(2L, 2L, 1L, 1L, 2L))

  record <- b0_record(result)
  expect_length(record$pools, 2L)

  rotating <- record$pools[[1]]
  expect_identical(rotating$class, "rotating")
  expect_identical(rotating$activation, "rotating")
  expect_identical(rotating$permanent_reason, NA_character_)
  expect_identical(rotating$keys, c("k05", "k08", "k10", "k12"))
  expect_identical(rotating$quotas, matrix(2L, nrow = 1, ncol = 2))

  certain <- record$pools[[2]]
  expect_identical(certain$class, "certainty")
  expect_identical(certain$activation, "permanent")
  expect_identical(certain$permanent_reason, "selection_certainty")
  expect_identical(certain$keys, "k01")
  expect_identical(certain$blocks, 1L)
})

test_that("a certainty unit is in every wave at factor one", {
  wave <- execute(b0_certainty(), wave = 1)

  expect_identical(wave$cluster, c("k01", "k08", "k10"))
  # The certainty cluster keeps its master weight; the rotating ones are
  # divided by an activation probability of 1/2.
  expect_equal(wave$.weight, c(1, 5, 4.230769), tolerance = 1e-6)
})

## B0.8 Small-pool promotion

test_that("a promoted pool is permanent without being selection-certain", {
  record <- b0_record(b0_promoted())

  expect_identical(record$small_pool_policy, "permanent")
  expect_length(record$pools, 2L)

  promoted <- record$pools[[1]]
  expect_identical(promoted$stratum$stratum, "S1")
  expect_identical(promoted$class, "rotating")
  expect_identical(promoted$activation, "permanent")
  expect_identical(promoted$permanent_reason, "small_pool")
  expect_identical(promoted$size, 2L)
  expect_identical(promoted$blocks, 2L)
  expect_identical(promoted$quotas, matrix(c(1L, 0L, 1L, 0L), nrow = 1))

  rotating <- record$pools[[2]]
  expect_identical(rotating$activation, "rotating")
  expect_identical(rotating$permanent_reason, NA_character_)
  expect_identical(rotating$size, 20L)
  expect_identical(rotating$blocks, rep(4L, 5))
  expect_identical(rotating$quotas, matrix(1L, nrow = 5, ncol = 4))
})

## B0.9 Serialization and replay

test_that("the serialized assignment record carries fixed fields", {
  path <- withr::local_tempfile(fileext = ".json")
  suppressWarnings(write_design(b0_clustered(), path))
  encoded <- jsonlite::fromJSON(path, simplifyVector = FALSE)$execution$panel_assignment

  expect_identical(
    names(encoded),
    c("algorithm", "version", "panels", "assignment_stage", "block_size",
      "r_min", "unit", "key_vars", "pool_vars", "control_ordered",
      "certainty", "small_pool_policy", "schedule", "pools")
  )
  expect_identical(encoded$algorithm, "blocked_random_quota")
  expect_identical(encoded$version, 3L)
  expect_identical(encoded$panels, 4L)
  expect_identical(encoded$assignment_stage, 1L)
  expect_identical(encoded$block_size, 4L)
  expect_identical(encoded$r_min, 2L)
  expect_identical(encoded$unit, "cluster")
  expect_identical(unlist(encoded$key_vars), c("stratum", "cluster"))
  expect_identical(unlist(encoded$pool_vars), "stratum")
  expect_false(encoded$control_ordered)
  expect_identical(encoded$certainty, "permanent")
  expect_identical(encoded$small_pool_policy, "error")

  expect_length(encoded$pools, 2L)
  pool <- encoded$pools[[1]]
  expect_identical(pool$stratum$stratum, "A")
  expect_identical(pool$class, "rotating")
  expect_identical(pool$activation, "rotating")
  expect_identical(pool$size, 4L)
  expect_identical(
    unlist(pool$keys),
    samplyr:::make_group_key(
      data.frame(stratum = "A", cluster = c("c01", "c04", "c05", "c06")),
      c("stratum", "cluster")
    )
  )
  expect_identical(unlist(pool$blocks), 4L)
  expect_identical(unlist(pool$quotas), rep(1L, 4))
})

test_that("a replayed receipt reproduces the assignment exactly", {
  master <- b0_clustered()
  path <- withr::local_tempfile(fileext = ".json")
  suppressWarnings(write_design(master, path))

  replayed <- replay_design(read_design(path), b0_cluster_frame())

  expect_identical(replayed$.panel, master$.panel)
  expect_identical(replayed$.weight, master$.weight)
  expect_identical(replayed$cluster, master$cluster)
  expect_identical(
    b0_record(replayed)$pools[[1]]$quotas,
    b0_record(master)$pools[[1]]$quotas
  )
})

## B0.10 Random number consumption

test_that("assignment consumes a fixed number of random draws", {
  # The value of the draw taken after an unseeded execution changes if the
  # assignment consumes a different number of random numbers, which a
  # reordering of the pool loop or a changed permutation would do without
  # altering any single fixture's labels.
  after_element <- withr::with_seed(99, {
    sampling_design() |>
      draw(n = 12) |>
      execute(b0_element_frame(), panels = 4)
    runif(1)
  })
  expect_equal(after_element, 0.0404900275170803, tolerance = 1e-12)

  after_clustered <- withr::with_seed(99, {
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(cluster) |>
      draw(n = 4) |>
      add_stage() |>
      draw(n = 2) |>
      execute(b0_cluster_frame(), panels = b0_rotation_schedule())
    runif(1)
  })
  expect_equal(after_clustered, 0.588103166781366, tolerance = 1e-12)
})

## B0.11 Rotation programs and separate registers

test_that("a program wave keeps fixed cohorts, rows and weights", {
  startup_schedule <- data.frame(
    panel = rep(1:2, times = 3),
    wave = rep(1:3, each = 2),
    active = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  startup <- sampling_design() |>
    draw(n = 40) |>
    execute(
      data.frame(id = 1:200, value = seq_len(200) / 200),
      seed = 1,
      panels = startup_schedule
    )
  intake <- sampling_design() |>
    draw(n = 12) |>
    execute(data.frame(id = 201:260, value = seq_len(60) / 60), seed = 2)

  program <- rotation_program(
    cohorts = list(startup = startup, intake_2 = intake),
    entry_wave = c(startup = 1, intake_2 = 2),
    schedule = rbind(
      transform(startup_schedule, cohort = "startup"),
      data.frame(
        cohort = "intake_2", panel = 1L, wave = 1:3,
        active = c(FALSE, TRUE, TRUE)
      )
    )
  )
  wave <- execute(program, wave = 2)

  expect_identical(names(wave), c("startup", "intake_2"))
  expect_identical(
    wave$startup$id,
    c(167L, 129L, 187L, 51L, 106L, 182L, 73L, 79L, 110L, 165L,
      34L, 89L, 172L, 163L, 188L, 42L, 148L, 20L, 121L, 87L)
  )
  # Wave 2 activates one of two panels, so the master weight of 5 doubles.
  expect_identical(unique(wave$startup$.weight), 10)
  expect_identical(
    wave$intake_2$id,
    c(221L, 215L, 206L, 258L, 232L, 208L, 217L, 229L, 254L, 212L, 241L, 211L)
  )
  # A cohort drawn whole is not a subsample of itself.
  expect_identical(unique(wave$intake_2$.weight), 5)
})

test_that("assignment over separate registers keeps its pools and labels", {
  sample <- execute(
    mf_design(), mf_schools(), mf_classes(), mf_students(),
    seed = 7, panels = 2
  )

  expect_identical(sample$.panel, c(2L, 2L, 1L, 1L))

  record <- b0_record(sample)
  expect_identical(record$key_vars, c("school_type", "school_id"))
  expect_identical(record$unit, "cluster")
  expect_length(record$pools, 2L)
  expect_identical(
    lapply(record$pools, function(p) p$stratum$school_type),
    list("Public", "Private")
  )
  # One school per stratum: a pool below the block size is still assigned.
  expect_identical(
    lapply(record$pools, function(p) p$keys),
    list(
      samplyr:::make_group_key(
        data.frame(school_type = "Public", school_id = "S2"),
        c("school_type", "school_id")
      ),
      samplyr:::make_group_key(
        data.frame(school_type = "Private", school_id = "S3"),
        c("school_type", "school_id")
      )
    )
  )
  expect_identical(
    lapply(record$pools, function(p) p$quotas),
    list(matrix(c(0L, 1L), nrow = 1), matrix(c(1L, 0L), nrow = 1))
  )
})
