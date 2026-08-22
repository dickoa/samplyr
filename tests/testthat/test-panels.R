test_that("panels produces .panel column with correct values", {
  frame <- data.frame(
    id = 1:100,
    value = rnorm(100)
  )

  result <- sampling_design() |>
    draw(n = 40) |>
    execute(frame, seed = 42, panels = 4)

  expect_true(".panel" %in% names(result))
  expect_equal(sort(unique(result$.panel)), 1:4)
})

test_that("panels = NULL produces no .panel column", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  result <- sampling_design() |>
    draw(n = 40) |>
    execute(frame, seed = 42)

  expect_false(".panel" %in% names(result))
})

test_that("panels splits evenly when n divisible by k", {
  frame <- data.frame(id = 1:200, value = rnorm(200))

  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42, panels = 4)

  panel_counts <- table(result$.panel)
  expect_equal(as.integer(panel_counts), rep(25L, 4))
})

test_that("panels differ by at most 1 when n not divisible by k", {
  frame <- data.frame(id = 1:200, value = rnorm(200))

  result <- sampling_design() |>
    draw(n = 101) |>
    execute(frame, seed = 42, panels = 4)

  panel_counts <- as.integer(table(result$.panel))
  expect_true(max(panel_counts) - min(panel_counts) <= 1)
})

test_that("stratified panels have equal representation per stratum", {
  frame <- data.frame(
    id = 1:200,
    region = rep(c("North", "South", "East", "West"), each = 50),
    value = rnorm(200)
  )

  expect_warning(
    result <- sampling_design() |>
      stratify_by(region) |>
      draw(n = 100) |>
      execute(frame, seed = 42, panels = 4),
    # Every stratum is taken whole: 4 x 50 requested 100 each.
    class = "samplyr_warning_census"
  )

  # Each panel should have equal count per stratum
  panel_strata <- table(result$region, result$.panel)
  for (i in seq_len(nrow(panel_strata))) {
    counts <- as.integer(panel_strata[i, ])
    expect_true(max(counts) - min(counts) <= 1)
  }
})

test_that("multi-stage panels assigned at PSU level", {
  set.seed(123)
  frame <- data.frame(
    school_id = rep(1:40, each = 10),
    student_id = 1:400,
    enrollment = rep(sample(100:500, 40, replace = TRUE), each = 10),
    value = rnorm(400)
  )

  result <- sampling_design() |>
    add_stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 20) |>
    add_stage(label = "Students") |>
      draw(n = 5) |>
    execute(frame, seed = 42, panels = 4)

  expect_true(".panel" %in% names(result))

  # All students in a school share the same panel
  panel_by_school <- tapply(result$.panel, result$school_id, unique)
  for (panels_in_school in panel_by_school) {
    expect_length(panels_in_school, 1)
  }
})

test_that("clustered panels block in control order", {
  # 16 PSUs with K = 2 is four blocks of 2K, the point at which blocking
  # differs from one global random quota. The control variable is
  # deliberately non-monotone in the PSU identifier, so the recorded
  # assignment order can only come from `control`.
  frame <- data.frame(
    psu = rep(1:20, each = 2),
    ssu = 1:40,
    score = rep((1:20 * 7L) %% 20L, each = 2)
  )

  asc <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 16, control = score) |>
    execute(frame, seed = 1, panels = 2)

  psus <- unique(as.data.frame(asc)[c("psu", "score")])
  expected <- as.character(psus$psu[order(psus$score)])

  pool <- attr(asc, "metadata")$panel_assignment$pools[[1]]
  expect_identical(pool$keys, expected)
  expect_identical(pool$blocks, c(4L, 4L, 4L, 4L))
  expect_identical(pool$quotas, matrix(2L, nrow = 4L, ncol = 2L))

  # The recorded quotas are the realized ones: each block of four PSUs in
  # control order holds exactly two of each panel.
  panel_of <- unique(as.data.frame(asc)[c("psu", ".panel")])
  in_order <- panel_of$.panel[match(expected, as.character(panel_of$psu))]
  for (block in split(in_order, rep(1:4, each = 4))) {
    expect_identical(tabulate(block, 2L), c(2L, 2L))
  }
})

test_that("panel blocking supports mixed desc() and serp() control ordering", {
  frame <- data.frame(
    psu = rep(1:8, each = 2),
    region = rep(c("A", "A", "A", "A", "B", "B", "B", "B"), each = 2),
    district = rep(c(1, 2, 3, 4, 1, 2, 3, 4), each = 2),
    score = rep(c(10, 20, 30, 40, 50, 60, 70, 80), each = 2),
    # A clustered stage orders whole clusters, so a control variable has to
    # be constant within one.
    ssu = rep(1:8, each = 2)
  )

  desc_first <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(psu) |>
    draw(n = 4, control = c(dplyr::desc(score), serp(district, ssu))) |>
    execute(frame, seed = 1, panels = 2)

  asc_first <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(psu) |>
    draw(n = 4, control = c(score, serp(district, ssu))) |>
    execute(frame, seed = 1, panels = 2)

  # Scores are distinct, so `desc(score)` fixes the order outright and the
  # serpentine term never breaks a tie. Each stratum is one block of four
  # with a quota of two per panel.
  pool_keys <- function(x) {
    lapply(attr(x, "metadata")$panel_assignment$pools, function(p) p$keys)
  }
  region_keys <- function(region, psus) {
    samplyr:::make_group_key(
      data.frame(region = region, psu = psus), c("region", "psu")
    )
  }
  expect_identical(
    pool_keys(desc_first),
    list(region_keys("A", 4:1), region_keys("B", 8:5))
  )
  expect_identical(
    pool_keys(asc_first),
    list(region_keys("A", 1:4), region_keys("B", 5:8))
  )

  for (pool in attr(desc_first, "metadata")$panel_assignment$pools) {
    expect_identical(pool$blocks, 4L)
    expect_identical(pool$quotas, matrix(2L, nrow = 1L, ncol = 2L))
  }
})

test_that("panels = 1 errors", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = 1),
    "panels.*integer.*>= 2"
  )
})

test_that("panels = 0 errors", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = 0),
    "panels.*integer.*>= 2"
  )
})

test_that("non-integer panels errors", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = 2.5),
    "panels.*integer.*>= 2"
  )

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = "two"),
    "panels.*integer.*>= 2"
  )
})

test_that("negative panels errors", {
  frame <- data.frame(id = 1:100, value = rnorm(100))

  expect_error(
    sampling_design() |>
      draw(n = 40) |>
      execute(frame, seed = 42, panels = -3),
    "panels.*integer.*>= 2"
  )
})

test_that("panels are deterministic with same seed", {
  frame <- data.frame(id = 1:200, value = rnorm(200))

  r1 <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42, panels = 4)

  r2 <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42, panels = 4)

  expect_equal(r1$.panel, r2$.panel)
})

test_that("panels work with continuation (execute_continuation path)", {
  set.seed(123)
  frame <- data.frame(
    school_id = rep(1:40, each = 10),
    student_id = 1:400,
    enrollment = rep(sample(100:500, 40, replace = TRUE), each = 10),
    value = rnorm(400)
  )

  design <- sampling_design() |>
    add_stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 20) |>
    add_stage(label = "Students") |>
      draw(n = 5)

  s1 <- execute(design, frame, stages = 1, seed = 42)
  result <- execute(s1, frame, seed = 43, panels = 4)

  expect_true(".panel" %in% names(result))
  expect_equal(sort(unique(result$.panel)), 1:4)
})

## The assignment construction

test_that("block sizes follow the scalar rule", {
  # A pool below one block is one block; otherwise the tail is spread one
  # unit at a time over the full blocks and never left standing alone.
  expect_identical(panel_block_sizes(1L, 8L), 1L)
  expect_identical(panel_block_sizes(7L, 8L), 7L)
  expect_identical(panel_block_sizes(8L, 8L), 8L)
  expect_identical(panel_block_sizes(16L, 8L), c(8L, 8L))
  expect_identical(panel_block_sizes(41L, 8L), c(9L, 8L, 8L, 8L, 8L))
  expect_identical(
    panel_block_sizes(100L, 8L),
    c(9L, 9L, 9L, 9L, 8L, 8L, 8L, 8L, 8L, 8L, 8L, 8L)
  )

  for (m in 8:60) {
    sizes <- panel_block_sizes(m, 8L)
    expect_identical(sum(sizes), m)
    expect_gte(min(sizes), 8L)
    expect_lte(max(sizes) - min(sizes), 1L)
  }
})

test_that("every unit's unconditional panel probability is 1/k", {
  # m = 9 with k = 4 is the awkward case: the quota multiset is 3, 2, 2, 2,
  # so uniformity comes from permuting the panel identities, not from equal
  # group sizes.
  m <- 9L
  k <- 4L
  reps <- 4000L

  counts <- matrix(0L, nrow = m, ncol = k)
  withr::with_seed(20260807, {
    for (i in seq_len(reps)) {
      at <- cbind(seq_len(m), assign_blocked_panels(m, k)$panel)
      counts[at] <- counts[at] + 1L
    }
  })

  expect_identical(as.integer(rowSums(counts)), rep(reps, m))
  z <- (counts - reps / k) / sqrt(reps * (1 / k) * (1 - 1 / k))
  expect_lt(max(abs(z)), 4.5)
})

test_that("labels are uniformly permuted inside a block", {
  # A block of 2k with k = 2 holds two labels of each panel, so its 4!
  # orderings collapse to 6 equiprobable arrangements. This is the SRSWOR
  # property a later activation of one panel rests on.
  reps <- 6000L
  arrangements <- withr::with_seed(11, {
    vapply(
      seq_len(reps),
      function(i) paste(assign_blocked_panels(4L, 2L)$panel, collapse = ""),
      character(1)
    )
  })

  counts <- table(arrangements)
  expect_setequal(
    names(counts),
    c("1122", "1212", "1221", "2112", "2121", "2211")
  )
  z <- (as.integer(counts) - reps / 6) / sqrt(reps * (1 / 6) * (5 / 6))
  expect_lt(max(abs(z)), 4)
})

test_that("a pool smaller than one block is assigned, not refused", {
  # Every stratum contributes a single unit against a block size of 8. The
  # assignment is well defined; only a later variance estimate would need
  # the pools collapsed, and that is not assignment's call to make.
  frame <- data.frame(
    id = 1:30,
    region = rep(letters[1:6], each = 5),
    value = rep(1, 30)
  )

  expect_no_warning(
    result <- sampling_design() |>
      stratify_by(region) |>
      draw(n = 1) |>
      execute(frame, seed = 4, panels = 4)
  )

  pools <- attr(result, "metadata")$panel_assignment$pools
  expect_length(pools, 6L)
  for (pool in pools) {
    expect_identical(pool$size, 1L)
    expect_identical(pool$blocks, 1L)
    expect_identical(sum(pool$quotas), 1L)
  }
  expect_true(all(result$.panel %in% 1:4))
})

## Pools, keys and the recorded contract

test_that("the receipt records the frozen blocks and quotas", {
  frame <- data.frame(id = 1:200, value = rnorm(200))

  result <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42, panels = 4)

  record <- attr(result, "metadata")$panel_assignment
  expect_identical(record$algorithm, "blocked_random_quota")
  expect_identical(record$version, 3L)
  expect_identical(record$panels, 4L)
  expect_identical(record$assignment_stage, 1L)
  expect_identical(record$block_size, 8L)
  expect_identical(record$unit, "element")
  expect_identical(record$key_vars, ".sample_id")
  expect_identical(record$certainty, "permanent")

  pool <- record$pools[[1]]
  expect_identical(pool$keys, as.character(result$.sample_id))
  expect_identical(pool$blocks, panel_block_sizes(100L, 8L))
  expect_identical(as.integer(rowSums(pool$quotas)), pool$blocks)

  # The recorded quota is the realized count in that block, which is what a
  # later activation is computed against.
  in_order <- result$.panel[match(pool$keys, as.character(result$.sample_id))]
  block_of <- rep(seq_along(pool$blocks), pool$blocks)
  realized <- vapply(
    split(in_order, block_of),
    function(p) tabulate(p, 4L),
    integer(4)
  )
  expect_equal(t(realized), pool$quotas, ignore_attr = TRUE)
})

test_that("the design file carries the assignment law, not the realization", {
  frame <- data.frame(
    id = 1:120,
    region = rep(c("A", "B"), each = 60),
    value = rnorm(120)
  )

  sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 25) |>
    execute(frame, seed = 8, panels = 4)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = frame)
  written <- attr(read_design(path), "execution")$panel_assignment
  record <- attr(sample, "metadata")$panel_assignment

  # Everything replay needs and cannot derive.
  expect_identical(written$algorithm, record$algorithm)
  expect_identical(as.integer(written$version), record$version)
  expect_identical(as.integer(written$panels), 4L)
  expect_identical(as.integer(written$block_size), 8L)
  expect_identical(as.integer(written$r_min), record$r_min)
  expect_identical(written$unit, "element")
  expect_identical(written$certainty, "permanent")
  expect_identical(unlist(written$key_vars), record$key_vars)

  # And nothing of the realization. The pools are the law applied to one
  # draw, and re-executing rebuilds them, so writing them stored a second
  # copy that no reader decoded.
  expect_null(written$pools)

  # Which is also why a design file carries no assignment-unit identifier,
  # including when the caller asked for no frame content at all.
  clustered <- sampling_design() |>
    cluster_by(region) |>
    draw(n = 1) |>
    execute(frame, seed = 8, panels = 2, frame_digest = "none")
  bare <- withr::local_tempfile(fileext = ".json")
  write_design(clustered, bare, frame = frame)
  raw <- paste(readLines(bare, warn = FALSE), collapse = "")
  expect_false(grepl("\"A\"", raw, fixed = TRUE))
  expect_false(grepl("\"B\"", raw, fixed = TRUE))

  # Replay reproduces the pools the file does not carry, exactly.
  replayed <- replay_design(read_design(path), frame)
  rebuilt <- attr(replayed, "metadata")$panel_assignment
  expect_identical(length(rebuilt$pools), length(record$pools))
  for (i in seq_along(record$pools)) {
    expect_identical(rebuilt$pools[[i]]$keys, record$pools[[i]]$keys)
    expect_identical(rebuilt$pools[[i]]$blocks, record$pools[[i]]$blocks)
    expect_identical(rebuilt$pools[[i]]$quotas, record$pools[[i]]$quotas)
  }
  expect_identical(replayed$.panel, sample$.panel)
})

test_that("certainty units are labelled from their own pool", {
  frame <- data.frame(id = 1:40, mos = c(rep(10, 36), 900, 950, 1000, 1100))

  result <- sampling_design() |>
    draw(n = 20, method = "pps_systematic", mos = mos, certainty_size = 800) |>
    execute(frame, seed = 3, panels = 4)

  record <- attr(result, "metadata")$panel_assignment
  expect_identical(
    vapply(record$pools, function(p) p$class, character(1)),
    c("rotating", "certainty")
  )

  certainty <- record$pools[[2]]
  expect_setequal(
    certainty$keys,
    as.character(result$.sample_id[result$.certainty_1])
  )
  # A permanent unit consumes no rotating quota: the two pools carry their
  # own, and together they still cover every selected unit exactly once.
  expect_identical(
    sum(vapply(record$pools, function(p) p$size, integer(1))),
    nrow(result)
  )
  expect_identical(sum(record$pools[[1]]$quotas), 16L)
  expect_identical(sum(certainty$quotas), 4L)
})

test_that("PSU certainty is pooled at PSU level", {
  frame <- data.frame(
    psu = rep(1:20, each = 4),
    unit = 1:80,
    size = rep(c(rep(10, 17), 400, 450, 500), each = 4)
  )

  result <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 10, method = "pps_systematic", mos = size, certainty_size = 300) |>
    execute(frame, seed = 6, panels = 2)

  record <- attr(result, "metadata")$panel_assignment
  expect_identical(record$unit, "cluster")
  expect_identical(
    vapply(record$pools, function(p) p$class, character(1)),
    c("rotating", "certainty")
  )

  psus <- unique(as.data.frame(result)[c("psu", ".certainty_1")])
  expect_setequal(
    record$pools[[2]]$keys,
    as.character(psus$psu[psus$.certainty_1])
  )
})

test_that("a with-replacement first stage assigns by realized draw", {
  frame <- data.frame(
    psu = rep(1:20, each = 5),
    unit = 1:100,
    size = rep(seq(10, 200, length.out = 20), each = 5)
  )

  result <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 10, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 5, panels = 4)

  record <- attr(result, "metadata")$panel_assignment
  # The estimator's PSU is the realized draw, so the same population cluster
  # drawn twice is two assignment units.
  expect_identical(record$key_vars, c("psu", ".draw_1"))
  expect_identical(record$pools[[1]]$size, 10L)
  expect_identical(
    nrow(unique(as.data.frame(result)[c("psu", ".draw_1")])),
    10L
  )
})

## Assignment happens once, and after selection

test_that("panels cannot be redeclared on a sample that already has them", {
  set.seed(123)
  frame <- data.frame(
    school_id = rep(1:40, each = 10),
    student_id = 1:400,
    value = rnorm(400)
  )

  design <- sampling_design() |>
    add_stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 20) |>
    add_stage(label = "Students") |>
      draw(n = 5)

  master <- execute(design, frame, stages = 1, seed = 42, panels = 4)

  expect_error(
    execute(master, frame, seed = 43, panels = 4),
    class = "samplyr_error_panels_already_assigned"
  )
  expect_error(
    execute(master, frame, seed = 43, panels = 2),
    class = "samplyr_error_panels_already_assigned"
  )
})

test_that("a continuation keeps the assignment its master recorded", {
  set.seed(123)
  frame <- data.frame(
    school_id = rep(1:40, each = 10),
    student_id = 1:400,
    value = rnorm(400)
  )

  design <- sampling_design() |>
    add_stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 20) |>
    add_stage(label = "Students") |>
      draw(n = 5)

  master <- execute(design, frame, stages = 1, seed = 42, panels = 4)
  continued <- execute(master, frame, seed = 43)

  master_map <- unique(as.data.frame(master)[c("school_id", ".panel")])
  continued_map <- unique(as.data.frame(continued)[c("school_id", ".panel")])
  expect_identical(
    continued_map$.panel,
    master_map$.panel[match(continued_map$school_id, master_map$school_id)]
  )

  # The frozen pools travel with the sample: a later wave reads them from
  # the receipt rather than recomputing them against another frame vintage.
  expect_identical(
    attr(continued, "metadata")$panel_assignment,
    attr(master, "metadata")$panel_assignment
  )
  expect_identical(attr(continued, "metadata")$panels, 4L)
})

test_that("panels do not change which units are selected", {
  set.seed(123)
  frame <- data.frame(
    school_id = rep(1:40, each = 10),
    student_id = 1:400,
    value = rnorm(400)
  )

  design <- sampling_design() |>
    add_stage(label = "Schools") |>
      cluster_by(school_id) |>
      draw(n = 20) |>
    add_stage(label = "Students") |>
      draw(n = 5)

  without <- execute(design, frame, seed = 42)
  with <- execute(design, frame, seed = 42, panels = 4)

  expect_identical(with$student_id, without$student_id)
  expect_identical(with$.weight, without$.weight)

  # Same at a single stage: assignment draws sit after the stage loop, so
  # they cannot perturb any selection.
  flat <- sampling_design() |>
    stratify_by(school_id) |>
    draw(n = 2)

  expect_identical(
    execute(flat, frame, seed = 7, panels = 3)$student_id,
    execute(flat, frame, seed = 7)$student_id
  )
})

test_that("panels does not alter weights", {
  frame <- data.frame(
    id = 1:200,
    region = rep(c("A", "B"), each = 100),
    value = rnorm(200)
  )

  without_panels <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  with_panels <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 100) |>
    execute(frame, seed = 42, panels = 4)

  expect_equal(with_panels$.weight, without_panels$.weight)
})

## The assignment context
#
# Panel assignment is described by six things that all follow from which
# stage owns it. Stage 1 exercises only the stage-1 answers, so the context
# is tested directly at the stages a public argument cannot yet select.

ctx_frame <- function() {
  frame <- expand.grid(
    person = 1:2, hh = 1:6, psu = sprintf("P%d", 1:6),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  frame <- frame[order(frame$psu, frame$hh, frame$person), ]
  frame$psu_stratum <- ifelse(frame$psu %in% c("P1", "P2", "P3"), "urban", "rural")
  frame$hh_stratum <- ifelse(frame$hh <= 3, "small", "large")
  frame$mos <- rep(seq(10, 60, by = 10), each = 2, times = 6)
  frame$psu_mos <- rep(seq(100, 600, by = 100), each = 12)
  frame$y <- seq_len(nrow(frame))
  rownames(frame) <- NULL
  frame
}

ctx_lower_stages <- function(design) {
  design |>
    add_stage() |>
    stratify_by(hh_stratum) |>
    cluster_by(hh) |>
    draw(n = 1) |>
    add_stage() |>
    draw(n = 1)
}

ctx_design <- function() {
  ctx_lower_stages(
    sampling_design() |>
      add_stage() |>
      stratify_by(psu_stratum) |>
      cluster_by(psu) |>
      draw(n = 2)
  )
}

ctx_design_multihit <- function() {
  ctx_lower_stages(
    sampling_design() |>
      add_stage() |>
      stratify_by(psu_stratum) |>
      cluster_by(psu) |>
      draw(n = 2, method = "pps_multinomial", mos = psu_mos)
  )
}

test_that("an ancestor occurrence path is empty at the first stage", {
  design <- ctx_design()
  sample <- execute(design, ctx_frame(), seed = 11)

  expect_identical(
    samplyr:::collect_ancestor_occurrence_vars(design, 1L, sample),
    character(0)
  )
})

test_that("a without-replacement ancestor contributes its cluster only", {
  design <- ctx_design()
  sample <- execute(design, ctx_frame(), seed = 11)

  expect_identical(
    samplyr:::collect_ancestor_occurrence_vars(design, 2L, sample),
    "psu"
  )
  # Stage 3 sits below both, and stage 2 is clustered as well.
  expect_identical(
    samplyr:::collect_ancestor_occurrence_vars(design, 3L, sample),
    c("psu", "hh")
  )
})

test_that("a multi-hit ancestor contributes its strata, cluster and draw", {
  design <- ctx_design_multihit()
  sample <- execute(design, ctx_frame(), seed = 12)
  expect_true(".draw_1" %in% names(sample))

  # The draw index restarts inside every stratum, so the stratum is what
  # makes it name an occurrence.
  expect_identical(
    samplyr:::collect_ancestor_occurrence_vars(design, 2L, sample),
    c("psu_stratum", "psu", ".draw_1")
  )
})

test_that("an ancestor that has lost its draw index is refused", {
  design <- ctx_design_multihit()
  sample <- execute(design, ctx_frame(), seed = 12)
  sample$.draw_1 <- NULL

  # Reading the ancestor as without replacement instead would merge two
  # conditional populations of descendants into one.
  expect_error(
    samplyr:::collect_ancestor_occurrence_vars(design, 2L, sample),
    class = "samplyr_error_panel_missing_identity"
  )
})

test_that("an assignment stage that has lost its draw index is refused", {
  design <- ctx_design_multihit()
  sample <- execute(design, ctx_frame(), seed = 12)
  sample$.draw_1 <- NULL

  expect_error(
    samplyr:::panel_assignment_context(design, 1L, sample),
    class = "samplyr_error_panel_missing_identity"
  )
})

test_that("a public execution cannot assign over collapsed occurrences", {
  # The reachable form: a with-replacement first stage, its draw column
  # dropped, and panels declared on the continuation. This used to warn only
  # that the sample was modified and then assign one panel to occurrences
  # that were selected separately.
  frame <- data.frame(
    psu = rep(sprintf("P%d", 1:8), each = 5),
    unit = rep(1:5, times = 8),
    mos = rep(seq(10, 80, by = 10), each = 5),
    y = seq_len(40)
  )
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 5, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 2)

  stage1 <- execute(design, frame, stages = 1, seed = 4)
  expect_gt(
    nrow(unique(as.data.frame(stage1)[c("psu", ".draw_1")])),
    length(unique(stage1$psu))
  )
  stage1$.draw_1 <- NULL

  expect_error(
    suppressWarnings(execute(stage1, frame, stages = 2, seed = 5, panels = 2)),
    class = "samplyr_error_panel_missing_identity"
  )
})

test_that("a missing cluster or stratum column is the same refusal", {
  design <- ctx_design()
  sample <- execute(design, ctx_frame(), seed = 11)

  without_cluster <- sample
  without_cluster$psu <- NULL
  expect_error(
    samplyr:::panel_assignment_context(design, 2L, without_cluster),
    class = "samplyr_error_panel_missing_identity"
  )

  without_stratum <- sample
  without_stratum$hh_stratum <- NULL
  expect_error(
    samplyr:::panel_assignment_context(design, 2L, without_stratum),
    class = "samplyr_error_panel_missing_identity"
  )
})

test_that("the declared-ancestor helper keeps its own contract", {
  design <- ctx_design_multihit()

  # It takes no sample and names no draw column: validation and linkage call
  # it before an execution exists.
  expect_identical(samplyr:::collect_ancestor_cluster_vars(design, 1L), character(0))
  expect_identical(samplyr:::collect_ancestor_cluster_vars(design, 2L), "psu")
  expect_identical(samplyr:::collect_ancestor_cluster_vars(design, 3L), c("psu", "hh"))
})

test_that("the context resolves a lower stage's key, pools and certainty", {
  design <- ctx_design()
  sample <- execute(design, ctx_frame(), seed = 11)
  context <- samplyr:::panel_assignment_context(design, 2L, sample)

  expect_identical(context$stage_num, 2L)
  expect_true(context$clustered)
  expect_false(context$multi_hit)
  expect_identical(context$ancestor_vars, "psu")
  # The household is identified inside the PSU occurrence it sits in, and
  # qualified by its own selection stratum.
  expect_identical(context$key_vars, c("psu", "hh_stratum", "hh"))
  # Pools are the household strata within each realized PSU.
  expect_identical(context$pool_vars, c("psu", "hh_stratum"))
  expect_identical(context$certainty_col, ".certainty_2")
  expect_identical(context$unit, "cluster")
})

test_that("the context takes the draw index of a multi-hit assignment stage", {
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(hh_stratum) |> cluster_by(hh) |>
    draw(n = 2, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 1)
  sample <- execute(design, ctx_frame(), seed = 13)
  context <- samplyr:::panel_assignment_context(design, 2L, sample)

  expect_true(context$multi_hit)
  expect_identical(
    context$key_vars, c("psu", "hh_stratum", "hh", ".draw_2")
  )
  expect_identical(context$pool_vars, c("psu", "hh_stratum"))
})

test_that("a terminal unclustered stage is keyed on the sample row", {
  design <- ctx_design()
  sample <- execute(design, ctx_frame(), seed = 11)
  context <- samplyr:::panel_assignment_context(design, 3L, sample)

  expect_false(context$clustered)
  expect_identical(context$key_vars, ".sample_id")
  expect_identical(context$unit, "element")
  # The ancestry is still what a pool is built from, even though the key
  # does not need it.
  expect_identical(context$pool_vars, c("psu", "hh"))
})

test_that("the context and the record it produced agree at stage 1", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 30),
    cluster = rep(sprintf("c%02d", 1:12), each = 5),
    unit = rep(1:5, times = 12),
    y = seq_len(60)
  )
  design <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |> draw(n = 4) |>
    add_stage() |> draw(n = 2)
  sample <- execute(design, frame, seed = 202, panels = 4)

  context <- samplyr:::panel_assignment_context(design, 1L, sample)
  record <- attr(sample, "metadata")$panel_assignment

  expect_identical(record$key_vars, context$key_vars)
  expect_identical(record$unit, context$unit)
  expect_identical(record$control_ordered, length(context$control) > 0L)
  expect_identical(
    lapply(record$pools, function(p) names(p$stratum)),
    rep(list(context$pool_vars), length(record$pools))
  )
})


## Lower-stage construction
#
# The context computing the right variable lists at a lower stage is a
# separate question from whether assignment built from that context pools,
# orders and marks the right units. No public argument selects a lower stage
# yet, so these drive the internals directly. Wave propagation is not covered
# here.

# Two PSUs, four households in each, two people in each household. Households
# are stratified inside the PSU, so a stage-2 pool is a proper subset of a
# PSU and a stage-3 pool is a proper subset of a household.
ctx_lower_design <- function() {
  sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(hh_stratum) |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> draw(n = 2)
}

ctx_assign <- function(design, sample, stage_num, panels = 2) {
  samplyr:::assign_panels(
    as.data.frame(sample),
    samplyr:::normalize_panel_input(panels),
    samplyr:::panel_assignment_context(design, stage_num, sample)
  )
}

test_that("stage-2 pools are the parent occurrence crossed with its strata", {
  design <- ctx_lower_design()
  sample <- execute(design, ctx_frame(), seed = 11)
  pools <- ctx_assign(design, sample, 2L)$record$pools

  # Two PSUs each holding a small and a large household stratum: four pools,
  # not two and not one.
  expect_length(pools, 4L)
  expect_identical(
    lapply(pools, function(p) names(p$stratum)),
    rep(list(c("psu", "hh_stratum")), 4L)
  )
  selected_psus <- sort(unique(sample$psu))
  expect_identical(
    sort(vapply(pools, function(p) {
      paste(p$stratum$psu, p$stratum$hh_stratum)
    }, character(1))),
    sort(paste(rep(selected_psus, each = 2), c("large", "small")))
  )

  # No pool crosses a parent. Every key is the pool's own PSU paired with a
  # household of that PSU's realized selection.
  data <- as.data.frame(sample)
  for (pool in pools) {
    in_pool <- data[
      data$psu == pool$stratum$psu &
        data$hh_stratum == pool$stratum$hh_stratum, ,
      drop = FALSE
    ]
    expect_identical(
      sort(pool$keys),
      sort(samplyr:::make_group_key(
        unique(in_pool[c("psu", "hh_stratum", "hh")]),
        c("psu", "hh_stratum", "hh")
      ))
    )
  }
})

test_that("stage-2 assignment rotates households inside a retained parent", {
  design <- ctx_lower_design()
  sample <- execute(design, ctx_frame(), seed = 11)
  data <- ctx_assign(design, sample, 2L)$sample

  # The point of a lower assignment stage: one PSU carries more than one
  # panel, which stage-1 assignment can never produce.
  by_psu <- tapply(data$.panel, data$psu, function(x) length(unique(x)))
  expect_true(all(by_psu > 1L))
  # Every person of one household still carries that household's panel.
  by_hh <- tapply(
    data$.panel, paste(data$psu, data$hh), function(x) length(unique(x))
  )
  expect_true(all(by_hh == 1L))
})

test_that("certainty at the assignment stage decides permanence, not below", {
  frame <- ctx_frame()
  # One household in every PSU is large enough to be selected for sure.
  frame$hh_mos <- ifelse(frame$hh == 1L, 400, 10)

  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |>
    draw(n = 3, method = "pps_systematic", mos = hh_mos) |>
    add_stage() |> draw(n = 1)
  sample <- execute(design, frame, seed = 21)
  expect_true(any(sample$.certainty_2))

  # Stage 2 reads `.certainty_2`, so the self-representing household is a
  # permanent pool of its own.
  at_2 <- ctx_assign(design, sample, 2L)$record$pools
  expect_true(any(vapply(at_2, function(p) {
    identical(p$class, "certainty")
  }, logical(1))))
  expect_true(any(vapply(at_2, function(p) {
    identical(p$class, "rotating")
  }, logical(1))))

  # Stage 3 reads `.certainty_3`, which no stage produced. A certainty
  # household does not make the people inside it permanent.
  at_3 <- ctx_assign(design, sample, 3L)$record$pools
  expect_true(all(vapply(at_3, function(p) {
    identical(p$class, "rotating")
  }, logical(1))))
})

test_that("the assignment stage's own control order fixes the key order", {
  frame <- ctx_frame()
  # Non-monotone in the household identifier, so an order that follows it
  # can only have come from the stage-2 control.
  frame$hh_score <- (frame$hh * 7L) %% 6L

  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2, control = psu) |>
    add_stage() |> cluster_by(hh) |> draw(n = 4, control = hh_score) |>
    add_stage() |> draw(n = 1)
  sample <- execute(design, frame, seed = 31)
  pools <- ctx_assign(design, sample, 2L)$record$pools

  data <- as.data.frame(sample)
  for (pool in pools) {
    in_psu <- data[data$psu == pool$stratum$psu, , drop = FALSE]
    households <- unique(in_psu[c("hh", "hh_score")])
    expect_identical(
      pool$keys,
      samplyr:::make_group_key(
        data.frame(
          psu = pool$stratum$psu,
          hh = households$hh[order(households$hh_score, households$hh)]
        ),
        c("psu", "hh")
      )
    )
  }
})

test_that("a terminal stage pools within its complete ancestry", {
  design <- ctx_lower_design()
  sample <- execute(design, ctx_frame(), seed = 11)
  record <- ctx_assign(design, sample, 3L)$record
  data <- as.data.frame(sample)

  expect_identical(record$key_vars, ".sample_id")
  expect_identical(record$unit, "element")
  # One pool per realized household: not one per PSU, and not one overall.
  expect_length(record$pools, nrow(unique(data[c("psu", "hh")])))
  expect_identical(
    lapply(record$pools, function(p) names(p$stratum)),
    rep(list(c("psu", "hh")), length(record$pools))
  )
  expect_identical(
    sum(vapply(record$pools, function(p) p$size, integer(1))),
    nrow(data)
  )
})

test_that("a stage stratified by its own parent names that parent once", {
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> stratify_by(psu) |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> draw(n = 1)
  sample <- execute(design, ctx_frame(), seed = 41)
  context <- samplyr:::panel_assignment_context(design, 2L, sample)

  # The ancestry and the stage's own strata both name the PSU. A repeated
  # column would reach the record as an invented `psu.1` field.
  expect_identical(context$pool_vars, "psu")
  expect_identical(context$key_vars, c("psu", "hh"))

  pools <- ctx_assign(design, sample, 2L)$record$pools
  expect_identical(
    lapply(pools, function(p) names(p$stratum)),
    rep(list("psu"), length(pools))
  )
})

## One class per defect kind

test_that("the panel count refusal carries a class, like every other in the file", {
  # It was a bare cli_abort(), catchable only by its message, in a file where
  # every other refusal is classed.
  expect_error(
    execute(sampling_design() |> draw(n = 5), data.frame(id = 1:20), panels = 1),
    class = "samplyr_error_panel_count"
  )
  expect_error(
    execute(sampling_design() |> draw(n = 5), data.frame(id = 1:20), panels = 2.5),
    class = "samplyr_error_panel_count"
  )
  expect_error(
    execute(sampling_design() |> draw(n = 5), data.frame(id = 1:20), panels = "four"),
    class = "samplyr_error_panel_count"
  )
})

## The two schedule paths ask the same questions

test_that("a defect in either schedule kind carries the same class", {
  # `normalize_panel_schedule()` and `normalize_program_schedule()` used to
  # write these five checks twice, with the same condition classes and
  # divergent wording. They share one implementation now, so a defect that
  # moves on one path must move on both, and this pins the pairing rather
  # than either message.
  frame <- data.frame(id = 1:200)
  master_schedule <- data.frame(
    panel = rep(1:2, times = 3), wave = rep(1:3, each = 2),
    active = c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  cohort <- sampling_design() |>
    draw(n = 40) |>
    execute(frame, seed = 1, panels = master_schedule)

  as_panels <- function(schedule) {
    sampling_design() |> draw(n = 10) |>
      execute(data.frame(id = 1:100), seed = 1, panels = schedule)
  }
  as_program <- function(schedule) {
    rotation_program(
      list(startup = cohort), entry_wave = c(startup = 1), schedule = schedule
    )
  }

  cases <- list(
    list(
      class = "samplyr_error_schedule_columns",
      panels = data.frame(panel = 1:2),
      program = data.frame(panel = 1:2)
    ),
    list(
      class = "samplyr_error_schedule_active",
      panels = data.frame(
        panel = rep(1:2, 2), wave = rep(1:2, each = 2),
        active = c("y", "n", "y", "n")
      ),
      program = data.frame(
        panel = c(1, 2), wave = c(1, 1), active = c("y", "n")
      )
    ),
    list(
      class = "samplyr_error_schedule_values",
      panels = data.frame(panel = c(1.5, 2), wave = c(1, 1)),
      program = data.frame(panel = c(1.5, 2), wave = c(1, 1))
    ),
    list(
      class = "samplyr_error_schedule_duplicates",
      panels = data.frame(panel = c(1, 1, 2, 2), wave = c(1, 1, 2, 2)),
      program = data.frame(panel = c(1, 1), wave = c(1, 1))
    ),
    list(
      class = "samplyr_error_schedule_idle_wave",
      panels = data.frame(
        panel = rep(1:2, 2), wave = rep(1:2, each = 2),
        active = c(TRUE, TRUE, FALSE, FALSE)
      ),
      program = data.frame(
        panel = c(1, 2, 1, 2), wave = c(1, 1, 2, 2),
        active = c(TRUE, TRUE, FALSE, FALSE)
      )
    )
  )

  for (case in cases) {
    expect_error(as_panels(case$panels), class = case$class)
    expect_error(as_program(case$program), class = case$class)
  }

  # And each still names the kind of schedule it was given, which is the one
  # thing the two are allowed to differ on.
  expect_error(as_panels(data.frame(panel = 1:2)), regexp = "`panels` schedule")
  expect_error(as_program(data.frame(panel = 1:2)), regexp = "program schedule")

  # Sentence case survives the shared template: the phrase is cli-formatted,
  # so it cannot be capitalized at the point of use.
  expect_error(as_panels(data.frame(panel = 1:2)), regexp = "^A `panels` schedule")
  expect_error(as_program(data.frame(panel = 1:2)), regexp = "^A program schedule")

  # The argument-naming message names the argument, not the description.
  expect_error(as_program(list(panel = 1)), regexp = "`schedule` must be a data frame")
})

## Fragments that were written more than once

test_that("one renderer serves both stratum labels", {
  # A diagnostic sentence and a table column differ only in what an
  # unstratified pool renders as. They were two functions with two separators
  # and two fallbacks.
  stratified <- list(stratum = list(region = "A", urban = TRUE))
  unstratified <- list(stratum = NULL)

  expect_identical(
    format_pool_stratum(stratified),
    "region = A, urban = TRUE"
  )
  expect_identical(
    format_pool_stratum(stratified, empty = NA_character_),
    format_pool_stratum(stratified)
  )
  expect_identical(format_pool_stratum(unstratified), "(unstratified)")
  expect_identical(
    format_pool_stratum(unstratified, empty = NA_character_), NA_character_
  )
  # A pool whose stratum is present but empty is unstratified too.
  expect_identical(
    format_pool_stratum(list(stratum = list())), "(unstratified)"
  )
})

test_that("one take computation, which is where a malformed quotas surfaces", {
  pool <- list(quotas = matrix(c(2L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), nrow = 2,
                               byrow = TRUE),
               blocks = c(5L, 4L))

  expect_identical(pool_take(pool, c(1L, 2L)), c(3L, 2L))
  expect_identical(pool_take(pool, 1L), c(2L, 1L))
  expect_identical(pool_take(pool, integer(0)), c(0L, 0L))

  # The JSON shape a record can come back as. It fails in one place now
  # rather than in two, which is what a guard for it would need.
  listed <- list(quotas = list(c(2L, 1L, 1L, 1L), c(1L, 1L, 1L, 1L)))
  expect_error(pool_take(listed, 1L), "incorrect number of dimensions")
})

test_that("a malformed version is described the way every other field is", {
  record <- function(version) {
    list(
      algorithm = "blocked_random_quota", version = version, panels = 4L,
      assignment_stage = 1L, block_size = 8L, r_min = 1L, unit = "element",
      key_vars = ".sample_id", pool_vars = character(0), pools = list()
    )
  }
  shown <- function(version) {
    conditionMessage(tryCatch(
      prepare_panel_record(record(version), "A probe"), error = identity
    ))
  }

  # An inline ladder here said "a character value" where
  # `describe_record_value()` names the value itself, which is what the
  # reader needs in order to see what the record actually says.
  expect_match(shown("3"), '"3"', fixed = TRUE)
  expect_match(shown(c(1L, 2L)), "2 values of type integer", fixed = TRUE)
  # "nothing" reads wrong of a version, so that one case stays its own.
  expect_match(shown(integer(0)), "no version", fixed = TRUE)
  expect_no_match(shown(integer(0)), "states nothing", fixed = TRUE)
})
