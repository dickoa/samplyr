## Descendant propagation of a stage-aware panel assignment

# One chain for every reachable design shape: a selected assignment occurrence
# gets one frozen panel label, every descendant row inherits it, and pools stay
# inside the complete parent occurrence. Each fixture varies one attribute.

## Fixtures and shared checks

# Households repeat their labels across PSUs on purpose: a household is only
# identified once the PSU it sits in is known, and the local label alone would
# merge six populations into one.
pp_frame <- function() {
  frame <- expand.grid(
    person = 1:2, hh = 1:6, psu = sprintf("P%d", 1:6),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  frame <- frame[order(frame$psu, frame$hh, frame$person), ]
  frame$hh_stratum <- ifelse(frame$hh <= 3, "small", "large")
  # One dominant unit per level makes repeated hits and certainty reachable.
  frame$psu_mos <- ifelse(frame$psu == "P1", 400, 10)
  frame$hh_mos <- ifelse(frame$hh == 1, 400, 10)
  frame$y <- seq_len(nrow(frame))
  rownames(frame) <- NULL
  frame
}

# Three PSUs, four households in each, two people in each household: every
# assignment unit has descendants, so inheritance is a claim rather than a
# restatement of the assignment.
pp_design <- function(persons = 2) {
  sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |> draw(n = persons)
}

pp_record <- function(x) attr(x, "metadata")$panel_assignment

# The key and pool variables are passed in as literals rather than read from
# the record. Reading them from the record would make the check follow a
# mutant that dropped a component from both the key and its own description.
pp_group <- function(df, vars) {
  do.call(paste, c(unname(as.data.frame(df)[vars]), sep = "\r"))
}

# The distinct panels each group carries, one element per group.
pp_panels_by <- function(sample, vars) {
  df <- as.data.frame(sample)
  unname(lapply(
    split(df$.panel, pp_group(df, vars)),
    function(p) sort(unique(p))
  ))
}

# The three propagation invariants that hold in every fixture, whatever the
# assignment stage: every occurrence carries one label, every row carries a
# label, and no occurrence spans two parent pools.
expect_propagates <- function(sample, key_vars, pool_vars) {
  df <- as.data.frame(sample)
  key <- pp_group(df, key_vars)
  pool <- pp_group(df, pool_vars)

  expect_false(anyNA(df$.panel))
  labels_per_unit <- tapply(df$.panel, key, function(p) length(unique(p)))
  expect_identical(as.vector(labels_per_unit), rep(1L, length(unique(key))))
  pools_per_unit <- tapply(pool, key, function(p) length(unique(p)))
  expect_identical(as.vector(pools_per_unit), rep(1L, length(unique(key))))
}

# The recorded pools partition the realized assignment units, each pool inside
# one parent occurrence. Asserted as sets, so a pool that lost or gained a unit
# fails even when the sizes add up.
expect_pools_partition <- function(sample, record, key_vars, pool_vars) {
  df <- as.data.frame(sample)
  units <- df[!duplicated(pp_group(df, key_vars)), , drop = FALSE]

  recorded <- unlist(lapply(record$pools, function(p) p$keys), use.names = FALSE)
  expect_identical(sort(recorded), sort(make_group_key(units, key_vars)))
  expect_identical(anyDuplicated(recorded), 0L)

  # A block that crossed a parent would not rotate within every parent.
  unit_keys <- make_group_key(units, key_vars)
  for (pool in record$pools) {
    rows <- units[match(pool$keys, unit_keys), , drop = FALSE]
    expect_identical(length(unique(pp_group(rows, pool_vars))), 1L)
  }
}

## The base case: without replacement, one stage below the first

test_that("a household's panel reaches every person in it", {
  master <- execute(pp_design(), pp_frame(), seed = 41,
                    panels = 4, panel_stage = 2)
  record <- pp_record(master)

  expect_identical(record$key_vars, c("psu", "hh"))
  expect_identical(record$pool_vars, "psu")
  expect_identical(record$unit, "cluster")
  expect_propagates(master, c("psu", "hh"), "psu")
  expect_pools_partition(master, record, c("psu", "hh"), "psu")

  # Two people per household: twelve assignment units carry twenty-four rows.
  expect_identical(nrow(master), 24L)
  expect_identical(sum(!duplicated(pp_group(master, c("psu", "hh")))), 12L)
  # Each PSU holds every panel once, which stage-1 assignment cannot do.
  expect_identical(pp_panels_by(master, "psu"), rep(list(1:4), 3))
})

test_that("a retained parent keeps every one of its selected units", {
  schedule <- data.frame(
    panel = rep(1:4, times = 3),
    wave = rep(1:3, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE
    )
  )
  master <- execute(pp_design(), pp_frame(), seed = 41,
                    panels = schedule, panel_stage = 2)
  wave <- execute(master, wave = 2)

  # The PSUs stay and each contributes its two households with active panels.
  expect_identical(sort(unique(wave$psu)), sort(unique(master$psu)))
  expect_identical(sort(unique(wave$.panel)), c(2L, 3L))
  expect_identical(pp_panels_by(wave, "psu"), rep(list(2:3), 3))
  # A wave subsets the master, and households keep their people.
  expect_identical(nrow(wave), 12L)
  expect_propagates(wave, c("psu", "hh"), "psu")
})

## A with-replacement ancestor

# Stage 1 selects with replacement and P1 is hit three times. Each hit is an
# independent conditional household population, so the hits stay separate.

pp_wr_ancestor <- function() {
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = psu_mos) |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |> draw(n = 2)
  execute(design, pp_frame(), seed = 1, panels = 2, panel_stage = 2)
}

test_that("repeated hits of one PSU are separate populations", {
  master <- pp_wr_ancestor()
  record <- pp_record(master)

  # P1 three times only: a household hangs from the occurrence, not the PSU.
  expect_identical(sort(unique(master$psu)), "P1")
  expect_identical(sort(unique(master$.draw_1)), 1:3)

  expect_identical(record$key_vars, c("psu", ".draw_1", "hh"))
  expect_identical(record$pool_vars, c("psu", ".draw_1"))
  expect_identical(record$unit, "cluster")
  expect_propagates(master, c("psu", ".draw_1", "hh"), c("psu", ".draw_1"))
  expect_pools_partition(master, record, c("psu", ".draw_1", "hh"),
                         c("psu", ".draw_1"))

  # Three pools of four, not one of twelve, and the pools name the occurrence.
  expect_identical(vapply(record$pools, function(p) p$size, integer(1)),
                   rep(4L, 3))
  expect_identical(
    sort(vapply(record$pools, function(p) p$stratum$.draw_1, integer(1))),
    1:3
  )
})

test_that("two occurrences of one household may take different panels", {
  master <- pp_wr_ancestor()
  labels <- unique(as.data.frame(master)[c(".draw_1", "hh", ".panel")])
  labels <- labels[order(labels$.draw_1, labels$hh), ]
  rownames(labels) <- NULL

  # Household 2 of P1 appears under all three hits, assigned separately in each.
  expect_identical(labels$.draw_1, rep(1:3, each = 4))
  expect_identical(labels$hh, c(1L, 2L, 3L, 6L, 2L, 3L, 4L, 6L,
                                1L, 2L, 5L, 6L))
  expect_identical(labels$.panel, c(2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L,
                                    1L, 2L, 1L, 2L))
  hh2 <- labels[labels$hh == 2L, ]
  expect_identical(hh2$.draw_1, 1:3)
  expect_identical(hh2$.panel, c(1L, 2L, 2L))
})

## A with-replacement assignment stage

# The assigning stage itself can select one household twice, so its draw index
# joins the identity and repeated hits may take different panels, as
# `?execute` documents.

pp_wr_assignment <- function() {
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 4, method = "pps_multinomial", mos = hh_mos) |>
    add_stage() |> draw(n = 2)
  execute(design, pp_frame(), seed = 1, panels = 2, panel_stage = 2)
}

test_that("a multi-hit assignment stage assigns its occurrences", {
  master <- pp_wr_assignment()
  record <- pp_record(master)

  expect_identical(record$key_vars, c("psu", "hh", ".draw_2"))
  expect_identical(record$unit, "occurrence")
  expect_identical(record$pool_vars, "psu")
  expect_propagates(master, c("psu", "hh", ".draw_2"), "psu")
  expect_pools_partition(master, record, c("psu", "hh", ".draw_2"), "psu")

  # Occurrences outnumber households: eight units over four households.
  occurrences <- pp_group(master, c("psu", "hh", ".draw_2"))
  households <- pp_group(master, c("psu", "hh"))
  expect_identical(length(unique(occurrences)), 8L)
  expect_identical(length(unique(households)), 4L)
})

test_that("one household selected twice may carry two panels", {
  master <- pp_wr_assignment()
  labels <- unique(as.data.frame(master)[c("psu", "hh", ".draw_2", ".panel")])
  labels <- labels[order(labels$psu, labels$hh, labels$.draw_2), ]
  rownames(labels) <- NULL

  expect_identical(labels$psu, rep(c("P1", "P4"), each = 4))
  expect_identical(labels$hh, c(1L, 1L, 3L, 3L, 1L, 1L, 1L, 5L))
  expect_identical(labels$.draw_2, c(1L, 3L, 2L, 4L, 2L, 3L, 4L, 1L))
  # Each occurrence's people share its panel, and occurrences need not agree.
  expect_identical(labels$.panel, c(2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L))
})

## Both occurrence dimensions at once

# An ancestor occurrence and the stage's own occurrence qualify different
# labels, so the key composes both. A key with either alone merges units the
# design kept apart.

test_that("an ancestor occurrence and an own occurrence both qualify a key", {
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = psu_mos) |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 4, method = "pps_multinomial", mos = hh_mos) |>
    add_stage() |> draw(n = 2)
  master <- execute(design, pp_frame(), seed = 1, panels = 2, panel_stage = 2)
  record <- pp_record(master)

  expect_identical(record$key_vars, c("psu", ".draw_1", "hh", ".draw_2"))
  expect_identical(record$pool_vars, c("psu", ".draw_1"))
  expect_identical(record$unit, "occurrence")
  expect_propagates(master, c("psu", ".draw_1", "hh", ".draw_2"),
                    c("psu", ".draw_1"))
  expect_pools_partition(master, record, c("psu", ".draw_1", "hh", ".draw_2"),
                         c("psu", ".draw_1"))

  # Dropping `.draw_1` would leave 6 units and dropping `.draw_2` would leave 5.
  expect_identical(
    length(unique(pp_group(master, c("psu", ".draw_1", "hh", ".draw_2")))),
    12L
  )
  expect_identical(length(unique(pp_group(master, c("psu", "hh")))), 3L)
  expect_identical(
    sort(vapply(record$pools, function(p) p$stratum$.draw_1, integer(1))),
    1:3
  )
})

test_that("the ancestor path accumulates rather than keeping the latest", {
  frame <- expand.grid(
    visit = 1:2, person = 1:3, hh = 1:4, psu = sprintf("P%d", 1:4),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  frame <- frame[order(frame$psu, frame$hh, frame$person, frame$visit), ]
  frame$psu_mos <- ifelse(frame$psu == "P1", 400, 10)
  frame$hh_mos <- ifelse(frame$hh == 1, 400, 10)
  rownames(frame) <- NULL
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 2, method = "pps_multinomial", mos = psu_mos) |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 2, method = "pps_multinomial", mos = hh_mos) |>
    add_stage() |> cluster_by(person) |> draw(n = 2) |>
    add_stage() |> draw(n = 2)
  master <- execute(design, frame, seed = 1, panels = 2, panel_stage = 3)
  record <- pp_record(master)

  # Two WR stages above the assigning one, so the path is two occurrences deep.
  expect_identical(record$key_vars,
                   c("psu", ".draw_1", "hh", ".draw_2", "person"))
  expect_identical(record$pool_vars, c("psu", ".draw_1", "hh", ".draw_2"))
  expect_propagates(master, c("psu", ".draw_1", "hh", ".draw_2", "person"),
                    c("psu", ".draw_1", "hh", ".draw_2"))
  expect_pools_partition(master, record,
                         c("psu", ".draw_1", "hh", ".draw_2", "person"),
                         c("psu", ".draw_1", "hh", ".draw_2"))

  # One pool per (PSU hit, household hit) pair, not two pools of four.
  expect_identical(length(record$pools), 4L)
  expect_identical(vapply(record$pools, function(p) p$size, integer(1)),
                   rep(2L, 4))
  pairs <- vapply(record$pools, function(p) {
    paste(p$stratum$.draw_1, p$stratum$.draw_2)
  }, character(1))
  expect_identical(sort(pairs), c("1 1", "1 2", "2 1", "2 2"))

  # Person 1 is selected in three of the four occurrences, assigned in each.
  labels <- unique(as.data.frame(master)[
    c(".draw_1", ".draw_2", "person", ".panel")
  ])
  first <- labels[labels$person == 1L, ]
  first <- first[order(first$.draw_1, first$.draw_2), ]
  expect_identical(paste(first$.draw_1, first$.draw_2), c("1 1", "2 1", "2 2"))
  expect_identical(first$.panel, c(2L, 2L, 1L))
})

## Descendants further down

# Two stages sit below the assignment stage, and inheritance is transitive.

test_that("inheritance passes through an intermediate stage", {
  frame <- expand.grid(
    person = 1:2, hh = 1:3, seg = 1:2, psu = sprintf("P%d", 1:4),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  frame <- frame[order(frame$psu, frame$seg, frame$hh, frame$person), ]
  rownames(frame) <- NULL
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> cluster_by(seg) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> draw(n = 2)
  master <- execute(design, frame, seed = 12, panels = 2, panel_stage = 2)
  record <- pp_record(master)

  expect_identical(record$assignment_stage, 2L)
  expect_identical(record$key_vars, c("psu", "seg"))
  expect_identical(record$pool_vars, "psu")
  expect_propagates(master, c("psu", "seg"), "psu")
  expect_pools_partition(master, record, c("psu", "seg"), "psu")

  # Households and people below the segments inherit, both panels per PSU.
  expect_identical(nrow(master), 16L)
  expect_identical(pp_panels_by(master, "psu"), rep(list(1:2), 2))
})

## A terminal stage

# The assigning stage is the last and selects elements, so each row is its
# own unit and the claim rests on pools inside the complete ancestry.

test_that("a terminal element stage pools inside its complete ancestry", {
  master <- execute(pp_design(), pp_frame(), seed = 41,
                    panels = 2, panel_stage = 3)
  record <- pp_record(master)

  expect_identical(record$key_vars, ".sample_id")
  expect_identical(record$unit, "element")
  expect_identical(record$pool_vars, c("psu", "hh"))
  expect_propagates(master, ".sample_id", c("psu", "hh"))
  expect_pools_partition(master, record, ".sample_id", c("psu", "hh"))

  # Two people per household take different panels, so rotation is within it.
  expect_identical(length(record$pools), 12L)
  expect_identical(vapply(record$pools, function(p) p$size, integer(1)),
                   rep(2L, 12))
  expect_identical(pp_panels_by(master, c("psu", "hh")), rep(list(1:2), 12))
})

## Strata at the assignment stage

# The assigning stage stratifies. The strata join the pool key, and the unit
# key too, because a draw index restarts inside every selection pool.

test_that("a stratified stage pools inside each parent, not across parents", {
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |>
    stratify_by(hh_stratum) |>
    cluster_by(hh) |>
    draw(n = 2) |>
    add_stage() |> draw(n = 2)
  master <- execute(design, pp_frame(), seed = 44, panels = 2, panel_stage = 2)
  record <- pp_record(master)

  expect_identical(record$key_vars, c("psu", "hh_stratum", "hh"))
  expect_identical(record$pool_vars, c("psu", "hh_stratum"))
  expect_propagates(master, c("psu", "hh_stratum", "hh"),
                    c("psu", "hh_stratum"))
  expect_pools_partition(master, record, c("psu", "hh_stratum", "hh"),
                         c("psu", "hh_stratum"))

  # Two PSUs by two household strata: four pools, and each names both.
  pools <- do.call(rbind, lapply(record$pools, function(p) {
    data.frame(psu = p$stratum$psu, hh_stratum = p$stratum$hh_stratum)
  }))
  expect_identical(
    sort(paste(pools$psu, pools$hh_stratum)),
    sort(paste(rep(unique(master$psu), each = 2), c("large", "small")))
  )
})

## Certainty is stage-local

# Only certainty at the assignment stage makes a unit permanent. Certainty
# above it is a retained parent, and certainty below it is a selection inside
# a rotating unit.

test_that("an ancestor selected with certainty leaves its units rotating", {
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3, method = "pps_systematic", mos = psu_mos) |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |> draw(n = 2)
  master <- execute(design, pp_frame(), seed = 8, panels = 2, panel_stage = 2)
  record <- pp_record(master)

  # P1 is a certainty PSU, and its households are still assigned by rotation.
  expect_true(any(master$.certainty_1))
  expect_identical(record$certainty, "permanent")
  expect_identical(
    vapply(record$pools, function(p) p$class, character(1)),
    rep("rotating", 3)
  )
  expect_propagates(master, c("psu", "hh"), "psu")

  # The certainty PSU's households carry both panels, not one permanent label.
  certain <- as.data.frame(master)[master$.certainty_1, , drop = FALSE]
  expect_identical(sort(unique(certain$.panel)), 1:2)
})

test_that("certainty at the assignment stage is permanent and takes no quota", {
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 4, method = "pps_systematic", mos = hh_mos) |>
    add_stage() |> draw(n = 2)
  master <- execute(design, pp_frame(), seed = 8, panels = 2, panel_stage = 2)
  record <- pp_record(master)

  # Household 1 of each PSU is certain: a rotating pool of 3, a permanent one.
  expect_identical(
    vapply(record$pools, function(p) p$class, character(1)),
    rep(c("rotating", "certainty"), 3)
  )
  expect_identical(vapply(record$pools, function(p) p$size, integer(1)),
                   rep(c(3L, 1L), 3))
  expect_identical(
    vapply(record$pools, function(p) p$activation, character(1)),
    rep(c("rotating", "permanent"), 3)
  )
  expect_identical(
    unique(vapply(record$pools[c(2, 4, 6)], function(p) p$permanent_reason,
                  character(1))),
    "selection_certainty"
  )
  expect_propagates(master, c("psu", "hh"), "psu")
  expect_pools_partition(master, record, c("psu", "hh"), "psu")

  # A certainty household's people inherit its assignment like any other.
  certain <- as.data.frame(master)[master$.certainty_2, , drop = FALSE]
  expect_identical(lengths(pp_panels_by(certain, c("psu", "hh"))), rep(1L, 3))
})

test_that("certainty below the assignment stage leaves the unit rotating", {
  # Person 1 carries forty times the measure, so a take of two reaches one.
  frame <- expand.grid(
    person = 1:3, hh = 1:6, psu = sprintf("P%d", 1:6),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  frame <- frame[order(frame$psu, frame$hh, frame$person), ]
  frame$person_mos <- ifelse(frame$person == 1, 400, 10)
  rownames(frame) <- NULL
  schedule <- data.frame(
    panel = rep(1:2, times = 2),
    wave = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |>
    draw(n = 2, method = "pps_systematic", mos = person_mos)
  master <- execute(design, frame, seed = 21, panels = schedule,
                    panel_stage = 2)
  record <- pp_record(master)

  # The stage-3 certainties sit inside households that still rotate.
  expect_identical(sum(master$.certainty_3), 12L)
  expect_identical(
    vapply(record$pools, function(p) p$class, character(1)),
    rep("rotating", 3)
  )
  expect_identical(
    vapply(record$pools, function(p) p$activation, character(1)),
    rep("rotating", 3)
  )
  expect_propagates(master, c("psu", "hh"), "psu")

  # A certainty person appears only in the waves where its household is active.
  wave1 <- execute(master, wave = 1)
  wave2 <- execute(master, wave = 2)
  expect_identical(sum(wave1$.certainty_3), 6L)
  expect_identical(sum(wave2$.certainty_3), 6L)

  person_of <- function(x) {
    df <- as.data.frame(x)
    pp_group(df[df$.certainty_3, , drop = FALSE], c("psu", "hh", "person"))
  }
  expect_identical(length(intersect(person_of(wave1), person_of(wave2))), 0L)
  expect_identical(
    sort(c(person_of(wave1), person_of(wave2))),
    sort(person_of(master))
  )

  # Wave weight is master weight over activation chance, certainties included.
  certain <- as.data.frame(wave1)[wave1$.certainty_3, , drop = FALSE]
  master_certain <- as.data.frame(master)[master$.certainty_3, , drop = FALSE]
  expect_identical(unique(certain$.weight),
                   unique(master_certain$.weight) * 2)
})

## A with-replacement stage below the assignment stage

# The terminal stage replicates rows, and the label reaches every row.

test_that("replicated descendant rows all inherit one label", {
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> draw(n = 3, method = "srswr")
  master <- execute(design, pp_frame(), seed = 6, panels = 2, panel_stage = 2)
  record <- pp_record(master)

  # The stage below assigns nothing: the key and the unit are the household's.
  expect_identical(record$key_vars, c("psu", "hh"))
  expect_identical(record$unit, "cluster")
  expect_true(max(master$.draw_3) > 1L)
  expect_propagates(master, c("psu", "hh"), "psu")
  expect_pools_partition(master, record, c("psu", "hh"), "psu")
})

## First-stage assignment

test_that("a first-stage assignment propagates to everything below it", {
  master <- execute(pp_design(), pp_frame(), seed = 41, panels = 2)
  record <- pp_record(master)

  expect_identical(record$assignment_stage, 1L)
  expect_identical(record$key_vars, "psu")
  expect_identical(record$pool_vars, character(0))
  expect_propagates(master, "psu", "psu")

  # A whole PSU is one unit, so its households cannot rotate inside it.
  expect_identical(lengths(pp_panels_by(master, "psu")), rep(1L, 3))
  # One global pool holding the three PSUs.
  expect_identical(length(record$pools), 1L)
  expect_identical(record$pools[[1]]$size, 3L)
})

## An incomplete ancestry key

# Unreachable: every key column is a cluster or stratification variable, and
# selection refuses a missing value in either before any assignment unit exists.

test_that("a missing ancestor cluster value is refused before assignment", {
  frame <- pp_frame()
  frame$psu[frame$psu == "P2"] <- NA

  expect_error(
    execute(pp_design(), frame, seed = 41, panels = 2, panel_stage = 2),
    "Cluster variable"
  )
})

test_that("a missing stratum value is refused before assignment", {
  frame <- pp_frame()
  frame$hh_stratum[frame$hh_stratum == "small"] <- NA
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |>
    stratify_by(hh_stratum) |>
    cluster_by(hh) |>
    draw(n = 2) |>
    add_stage() |> draw(n = 2)

  # Checked on every reachable PSU before any draw, so no seed avoids it.
  for (seed in c(44, 45, 46)) {
    expect_error(
      execute(design, frame, seed = seed, panels = 2, panel_stage = 2),
      class = "samplyr_error_frame_invalid"
    )
  }
})

test_that("a missing key component would stay distinct if one existed", {
  # Encoding gives a missing value its own token, distinct from "NA" and "".
  probe <- data.frame(a = c(NA, "NA", ""), b = rep("x", 3))
  expect_identical(anyDuplicated(make_group_key(probe, c("a", "b"))), 0L)
  expect_identical(anyDuplicated(make_group_key(probe, "a")), 0L)
})
