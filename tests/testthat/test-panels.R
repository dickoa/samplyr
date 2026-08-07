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
  expect_identical(
    pool_keys(desc_first),
    list(c("4", "3", "2", "1"), c("8", "7", "6", "5"))
  )
  expect_identical(
    pool_keys(asc_first),
    list(c("1", "2", "3", "4"), c("5", "6", "7", "8"))
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
  expect_identical(record$version, 1L)
  expect_identical(record$panels, 4L)
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

test_that("the design file carries the blocks and quotas", {
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
  expect_identical(written$algorithm, record$algorithm)
  expect_identical(as.integer(written$version), record$version)
  expect_identical(as.integer(written$panels), 4L)
  expect_identical(as.integer(written$block_size), 8L)
  expect_identical(written$unit, "element")
  expect_identical(written$certainty, "permanent")
  expect_length(written$pools, length(record$pools))

  for (i in seq_along(record$pools)) {
    expect_identical(
      as.character(unlist(written$pools[[i]]$keys)),
      record$pools[[i]]$keys
    )
    expect_identical(
      as.integer(unlist(written$pools[[i]]$blocks)),
      record$pools[[i]]$blocks
    )
    expect_identical(
      matrix(
        as.integer(unlist(written$pools[[i]]$quotas)),
        nrow = length(record$pools[[i]]$blocks),
        byrow = TRUE
      ),
      record$pools[[i]]$quotas
    )
  }
  expect_identical(
    vapply(written$pools, function(p) p$stratum$region, character(1)),
    c("A", "B")
  )
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
  expect_identical(record$unit, "psu")
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
