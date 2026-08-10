## B3. Descendant propagation of a stage-aware panel assignment
#
# One chain, proved once for every reachable shape of design:
#
#   a selected assignment occurrence
#     -> one frozen panel label
#     -> every descendant row inherits that label
#     -> pools stay inside the complete parent occurrence
#
# Each fixture differs from the base in one attribute, and each assertion
# names the attribute it is there for. A fixture that varies two things at
# once cannot say which one a failure came from, and an assertion weaker than
# the comment above it blesses whatever the code happens to do.
#
# Statistical work is workstream D and is not here: activation moments over
# enumeration, modified-master validation, wave receipt enrichment and
# stage-aware `stack_waves()`. What a wave does appear for below is the one
# propagation claim it carries, that a retained parent keeps every one of its
# selected units.

## B3.0 Fixtures and shared checks

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
  # One dominant unit at each level, which makes a repeated hit reachable
  # under a with-replacement method and a certainty under a pps one.
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

# The recorded pools describe the realized sample: their keys are exactly the
# assignment units, each unit appears in one pool, and each pool sits inside
# one parent occurrence. Asserted as sets, so a pool that lost or gained a
# unit fails whether or not the sizes still add up.
expect_pools_partition <- function(sample, record, key_vars, pool_vars) {
  df <- as.data.frame(sample)
  units <- df[!duplicated(pp_group(df, key_vars)), , drop = FALSE]

  recorded <- unlist(lapply(record$pools, function(p) p$keys), use.names = FALSE)
  expect_identical(sort(recorded), sort(make_group_key(units, key_vars)))
  expect_identical(anyDuplicated(recorded), 0L)

  # Each pool's units share one value of every pool variable: a block that
  # crossed a parent would stop guaranteeing rotation within every parent.
  unit_keys <- make_group_key(units, key_vars)
  for (pool in record$pools) {
    rows <- units[match(pool$keys, unit_keys), , drop = FALSE]
    expect_identical(length(unique(pp_group(rows, pool_vars))), 1L)
  }
}

## B3.1 The base case: without replacement, one stage below the first

test_that("a household's panel reaches every person in it", {
  master <- execute(pp_design(), pp_frame(), seed = 41,
                    panels = 4, panel_stage = 2)
  record <- pp_record(master)

  expect_identical(record$key_vars, c("psu", "hh"))
  expect_identical(record$pool_vars, "psu")
  expect_identical(record$unit, "cluster")
  expect_propagates(master, c("psu", "hh"), "psu")
  expect_pools_partition(master, record, c("psu", "hh"), "psu")

  # Two people per household, so inheritance is doing something: twelve
  # assignment units carry twenty-four rows.
  expect_identical(nrow(master), 24L)
  expect_identical(sum(!duplicated(pp_group(master, c("psu", "hh")))), 12L)
  # Four panels, four households per PSU: each PSU holds every panel once,
  # which is the address-panel design and what stage-1 assignment cannot do.
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

  # The PSUs stay and the households rotate inside them. Every PSU is still
  # there, and each contributes the two households whose panels are active.
  expect_identical(sort(unique(wave$psu)), sort(unique(master$psu)))
  expect_identical(sort(unique(wave$.panel)), c(2L, 3L))
  expect_identical(pp_panels_by(wave, "psu"), rep(list(2:3), 3))
  # A wave subsets the master: the households it keeps are households the
  # master selected, and their people come with them.
  expect_identical(nrow(wave), 12L)
  expect_propagates(wave, c("psu", "hh"), "psu")
})

## B3.2 A with-replacement ancestor
#
# One attribute differs from B3.1: stage 1 selects with replacement, and P1 is
# selected three times. Each hit is an independent conditional household
# population, and collapsing them would give one PSU three panels' worth of
# households drawn from a single pool.

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

  # P1 three times and nothing else: the occurrence, not the PSU, is what a
  # household hangs from.
  expect_identical(sort(unique(master$psu)), "P1")
  expect_identical(sort(unique(master$.draw_1)), 1:3)

  expect_identical(record$key_vars, c("psu", ".draw_1", "hh"))
  expect_identical(record$pool_vars, c("psu", ".draw_1"))
  expect_identical(record$unit, "cluster")
  expect_propagates(master, c("psu", ".draw_1", "hh"), c("psu", ".draw_1"))
  expect_pools_partition(master, record, c("psu", ".draw_1", "hh"),
                         c("psu", ".draw_1"))

  # Three pools of four rather than one pool of twelve, and the pools name
  # the occurrence.
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

  # Pinned in values. Household 2 of P1 is selected under all three hits and
  # carries panel 1, 2 and 2: the same population household, assigned
  # separately in each conditional population it belongs to. A key that
  # dropped the occurrence would have to force one label onto all three.
  expect_identical(labels$.draw_1, rep(1:3, each = 4))
  expect_identical(labels$hh, c(1L, 2L, 3L, 6L, 2L, 3L, 4L, 6L,
                                1L, 2L, 5L, 6L))
  expect_identical(labels$.panel, c(2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L,
                                    1L, 2L, 1L, 2L))
  hh2 <- labels[labels$hh == 2L, ]
  expect_identical(hh2$.draw_1, 1:3)
  expect_identical(hh2$.panel, c(1L, 2L, 2L))
})

## B3.3 A with-replacement assignment stage
#
# One attribute differs from B3.1: the assigning stage itself can select one
# household twice. Its own draw index becomes part of the identity, the unit
# vocabulary says so, and repeated hits may be assigned differently. That is
# the occurrence-level contract, and `?execute` documents it as a reason to
# assign at a without-replacement stage when fieldwork needs one label per
# household.

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

  # A household is selected more than once, so occurrences outnumber
  # households: eight assignment units over six population households.
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
  # Household 1 of P1 takes panel 2 as its first draw and panel 1 as its
  # third; household 3 of P1 takes 2 and 1. Both people of an occurrence take
  # that occurrence's panel, and the two occurrences do not have to agree.
  expect_identical(labels$.panel, c(2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L))
})

## B3.4 Both occurrence dimensions at once
#
# B3.2 and B3.3 each vary one of them. A design can vary both, and then the
# identity has to compose rather than pick: the ancestor's occurrence and the
# stage's own occurrence are different qualifiers of different labels, and a
# key carrying either one alone still merges units the design kept apart.
# Neither of these shapes is a defect in the current build; they are here so
# that a later change to how the path is built cannot pass by handling one
# dimension.

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

  # Twelve assignment occurrences over three population households, all of
  # them in P1: household 1 of P1 is selected under all three hits of P1 and
  # more than once inside two of them. The key that dropped `.draw_1` would
  # leave 6 units and the one that dropped `.draw_2` would leave 5.
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

  # Two with-replacement stages above the assigning one, so the path is two
  # occurrences deep. Keeping only the nearest would drop `.draw_1` from both
  # lists.
  expect_identical(record$key_vars,
                   c("psu", ".draw_1", "hh", ".draw_2", "person"))
  expect_identical(record$pool_vars, c("psu", ".draw_1", "hh", ".draw_2"))
  expect_propagates(master, c("psu", ".draw_1", "hh", ".draw_2", "person"),
                    c("psu", ".draw_1", "hh", ".draw_2"))
  expect_pools_partition(master, record,
                         c("psu", ".draw_1", "hh", ".draw_2", "person"),
                         c("psu", ".draw_1", "hh", ".draw_2"))

  # Household 1 of P1 under both hits of P1 and both hits of itself: four
  # pools of two, one per (PSU hit, household hit) pair. A path holding only
  # the nearest occurrence would merge them into two pools of four.
  expect_identical(length(record$pools), 4L)
  expect_identical(vapply(record$pools, function(p) p$size, integer(1)),
                   rep(2L, 4))
  pairs <- vapply(record$pools, function(p) {
    paste(p$stratum$.draw_1, p$stratum$.draw_2)
  }, character(1))
  expect_identical(sort(pairs), c("1 1", "1 2", "2 1", "2 2"))

  # Pinned in values. Person 1 of that household is selected in three of the
  # four occurrences and carries panels 2, 2 and 1: one person of one
  # population household, assigned separately in each.
  labels <- unique(as.data.frame(master)[
    c(".draw_1", ".draw_2", "person", ".panel")
  ])
  first <- labels[labels$person == 1L, ]
  first <- first[order(first$.draw_1, first$.draw_2), ]
  expect_identical(paste(first$.draw_1, first$.draw_2), c("1 1", "2 1", "2 2"))
  expect_identical(first$.panel, c(2L, 2L, 1L))
})

## B3.5 Descendants further down
#
# One attribute differs from B3.1: two stages sit below the assignment stage
# rather than one. Inheritance is transitive, and the segment's panel reaches
# people it has no column in common with beyond the ancestry.

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

  # Sixteen rows under four segments: the households and the people below
  # them all inherit, and both panels appear inside each PSU.
  expect_identical(nrow(master), 16L)
  expect_identical(pp_panels_by(master, "psu"), rep(list(1:2), 2))
})

## B3.6 A terminal stage
#
# One attribute differs from B3.1: the assigning stage is the last one, and it
# selects elements. Each selected row is its own unit, so the chain's third
# link is empty by construction, and the pools are what the claim rests on:
# they run inside the complete ancestry rather than across the sample.

test_that("a terminal element stage pools inside its complete ancestry", {
  master <- execute(pp_design(), pp_frame(), seed = 41,
                    panels = 2, panel_stage = 3)
  record <- pp_record(master)

  expect_identical(record$key_vars, ".sample_id")
  expect_identical(record$unit, "element")
  expect_identical(record$pool_vars, c("psu", "hh"))
  expect_propagates(master, ".sample_id", c("psu", "hh"))
  expect_pools_partition(master, record, ".sample_id", c("psu", "hh"))

  # One pool per selected household, holding that household's two people, and
  # the two people take different panels: the rotation is inside the
  # household rather than inside the PSU.
  expect_identical(length(record$pools), 12L)
  expect_identical(vapply(record$pools, function(p) p$size, integer(1)),
                   rep(2L, 12))
  expect_identical(pp_panels_by(master, c("psu", "hh")), rep(list(1:2), 12))
})

## B3.7 Strata at the assignment stage
#
# One attribute differs from B3.1: the assigning stage stratifies. The strata
# join the pool key, so pools are the stage's strata inside each parent, and
# they join the unit key, because a draw index restarts inside every selection
# pool.

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

## B3.8 Certainty is stage-local
#
# Only certainty at the assignment stage makes a unit permanent. Certainty
# above it is a parent that stays in the survey and certainty below it is a
# selection inside a unit that rotates; neither says anything about whether
# the assignment unit is active at a wave. All three rules are here because
# a build that read certainty from a fixed stage would satisfy one of them by
# accident.

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

  # The certainty PSU's own households carry both panels rather than one
  # permanent label.
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

  # Household 1 of each PSU is a certainty selection, so each PSU splits into
  # a rotating pool of three and a permanent pool of one. The permanent unit
  # is out of the rotation rather than holding a panel's quota in it.
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
  # Three people per household so a take of two can reach probability one:
  # person 1 carries forty times the measure of the other two.
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

  # Twelve of the twenty-four selected people are certainties of stage 3, and
  # every stage-2 pool is still rotating: the certainty is a property of the
  # selection inside the household, not of the household's activation.
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

  # A certainty person leaves the survey when their household's panel is
  # inactive. Six of the twelve are gone at each wave, and each certainty
  # person appears in exactly one of the two waves.
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

  # The wave weight is the master weight over the activation chance, for a
  # certainty of stage 3 exactly as for any other row.
  certain <- as.data.frame(wave1)[wave1$.certainty_3, , drop = FALSE]
  master_certain <- as.data.frame(master)[master$.certainty_3, , drop = FALSE]
  expect_identical(unique(certain$.weight),
                   unique(master_certain$.weight) * 2)
})

## B3.9 A with-replacement stage below the assignment stage
#
# One attribute differs from B3.1: the terminal stage replicates rows. A
# descendant is a row, however many rows one selection produced, and the label
# reaches all of them.

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

## B3.10 The first stage still behaves as it did

test_that("a first-stage assignment propagates to everything below it", {
  master <- execute(pp_design(), pp_frame(), seed = 41, panels = 2)
  record <- pp_record(master)

  expect_identical(record$assignment_stage, 1L)
  expect_identical(record$key_vars, "psu")
  expect_identical(record$pool_vars, character(0))
  expect_propagates(master, "psu", "psu")

  # A whole PSU is one unit, so its households cannot rotate inside it: the
  # difference between this and B3.1 is the design, not the machinery.
  expect_identical(lengths(pp_panels_by(master, "psu")), rep(1L, 3))
  # One global pool holding the three PSUs.
  expect_identical(length(record$pools), 1L)
  expect_identical(record$pools[[1]]$size, 3L)
})

## B3.11 An incomplete ancestry key
#
# "No descendant is matched through an incomplete ancestry key" is not
# reachable, and the assignment is not what forecloses it. Every column the
# key is built from is a cluster or a stratification variable of some stage,
# and selection refuses a missing value in either before an assignment unit
# exists. Measured rather than assumed, the way the stage-1 key gap was.

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

  # Raised at the stage that stratifies, so it is the selected parents' rows
  # that are checked rather than the whole frame.
  expect_error(
    execute(design, frame, seed = 44, panels = 2, panel_stage = 2),
    "Stratification variable"
  )
})

test_that("a missing key component would stay distinct if one existed", {
  # The qualifier the two refusals above make unreachable. Keys are compared
  # as encoded strings, and the encoding gives a missing value its own token
  # rather than an empty one, so it can neither match everything nor collide
  # with the string that prints like it.
  probe <- data.frame(a = c(NA, "NA", ""), b = rep("x", 3))
  expect_identical(anyDuplicated(make_group_key(probe, c("a", "b"))), 0L)
  expect_identical(anyDuplicated(make_group_key(probe, "a")), 0L)
})
