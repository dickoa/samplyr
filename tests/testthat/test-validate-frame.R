# validate_frame() input guards, the happy path, and the missing, type and
# range issues. PRN, NA strata/cluster and balanced aux checks are tested in
# test-prn.R, test-edge-cases.R and test-balanced.R.

good_frame <- data.frame(
  region = rep(c("N", "S"), each = 10),
  district = rep(letters[1:4], each = 5),
  size = runif(20, 1, 100),
  y = rnorm(20)
)

test_that("validate_frame returns invisibly TRUE for a valid frame", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4)

  expect_true(validate_frame(design, good_frame))
  expect_invisible(validate_frame(design, good_frame))
})

test_that("validate_frame guards its inputs", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4)

  expect_error(validate_frame(list(), good_frame), "sampling_design")
  expect_error(validate_frame(design, 1:5), "data frame")
  expect_error(
    validate_frame(design, good_frame[0, ]),
    class = "samplyr_error_frame_empty"
  )

  # execute() refuses an empty frame from a later layer, without a class.
  expect_error(execute(design, good_frame[0, ], seed = 1), "0 rows")
})

test_that("validate_frame accepts a valid stage selector", {
  design <- sampling_design() |>
    cluster_by(district) |>
    draw(n = 2, method = "pps_brewer", mos = size) |>
    add_stage() |>
    draw(n = 1)

  # A cluster-level MOS must be constant within the cluster.
  cluster_frame <- good_frame
  cluster_frame$size <- rep(c(10, 20, 30, 40), each = 5)

  expect_true(validate_frame(design, cluster_frame, stages = 1))
  expect_error(
    validate_frame(design, good_frame, stages = 1),
    class = "samplyr_error_frame_cluster_invariant"
  )
})

test_that("validate_frame detects a missing stratification variable", {
  design <- sampling_design() |>
    stratify_by(zzz) |>
    draw(n = 4)

  expect_error(
    validate_frame(design, good_frame),
    class = "samplyr_error_frame_missing_vars"
  )
  expect_error(
    validate_frame(design, good_frame),
    "stratification variable"
  )
})

test_that("validate_frame detects a missing cluster variable", {
  design <- sampling_design() |>
    cluster_by(zzz) |>
    draw(n = 2, method = "pps_brewer", mos = size)

  expect_error(
    validate_frame(design, good_frame),
    class = "samplyr_error_frame_missing_vars"
  )
  expect_error(
    validate_frame(design, good_frame),
    "cluster variable"
  )
})

test_that("validate_frame detects MOS problems", {
  base <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = size)

  missing_mos <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = zzz)
  expect_error(
    validate_frame(missing_mos, good_frame),
    class = "samplyr_error_frame_missing_vars"
  )
  expect_error(validate_frame(missing_mos, good_frame), "MOS variable")

  type_mos <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = region)
  expect_error(validate_frame(type_mos, good_frame), "must be numeric")

  na_frame <- good_frame
  na_frame$size[1] <- NA
  expect_error(validate_frame(base, na_frame), "contains NA values")

  neg_frame <- good_frame
  neg_frame$size <- -neg_frame$size
  expect_error(validate_frame(base, neg_frame), "contains negative values")
})

test_that("validate_frame detects auxiliary variable NA values", {
  design <- sampling_design() |>
    draw(n = 2, method = "balanced", aux = size)

  na_frame <- good_frame
  na_frame$size[1] <- NA
  expect_error(
    validate_frame(design, na_frame),
    "auxiliary variable .* contains NA values"
  )
})

test_that("validate_frame detects a missing control variable", {
  design <- sampling_design() |>
    draw(n = 2, method = "systematic", control = zzz)

  expect_error(
    validate_frame(design, good_frame),
    class = "samplyr_error_frame_missing_vars"
  )
  expect_error(
    validate_frame(design, good_frame),
    "control variable"
  )
})

## Fingerprint comparison for designs restored with read_design()

fp_design <- sampling_design() |>
  stratify_by(region) |>
  draw(n = 4)

fp_restored <- read_design(design_json(fp_design, frame = good_frame))

test_that("validate_frame is silent when the frame matches the fingerprint", {
  expect_no_message(validate_frame(fp_restored, good_frame))
  expect_true(validate_frame(fp_restored, good_frame))
})

test_that("the fingerprint hash ignores the data-frame class", {
  expect_no_message(validate_frame(fp_restored, tibble::as_tibble(good_frame)))

  from_tibble <- read_design(
    design_json(fp_design, frame = tibble::as_tibble(good_frame))
  )
  expect_no_message(validate_frame(from_tibble, good_frame))
})

test_that("the fingerprint hash ignores column order but not row order", {
  reordered_cols <- good_frame[, rev(names(good_frame))]
  expect_no_message(validate_frame(fp_restored, reordered_cols))

  reordered_rows <- good_frame[rev(seq_len(nrow(good_frame))), ]
  expect_message(
    validate_frame(fp_restored, reordered_rows),
    "same structure but different content"
  )
})

test_that("validate_frame informs when rows changed and still passes", {
  expect_message(
    out <- validate_frame(fp_restored, good_frame[-1, ]),
    "Frame differs"
  )
  expect_true(out)
  expect_message(
    validate_frame(fp_restored, good_frame[-1, ]),
    "19 rows instead of the 20 recorded"
  )
})

test_that("validate_frame reports removed, added, and retyped columns", {
  modified <- good_frame
  modified$y <- NULL
  modified$extra <- 1
  modified$size <- as.character(modified$size)

  # Matched on the condition: a sink-based capture is empty under a reporter.
  expect_message(
    validate_frame(fp_restored, modified),
    "\"y\" no longer present"
  )
  expect_message(
    validate_frame(fp_restored, modified),
    "new column \"extra\""
  )
  expect_message(
    validate_frame(fp_restored, modified),
    "size \\(character instead of numeric\\)"
  )
})

test_that("validate_frame reports content-only changes", {
  modified <- good_frame
  modified$y[1] <- modified$y[1] + 1

  expect_message(
    validate_frame(fp_restored, modified),
    "same structure but different content"
  )
})

test_that("the fingerprint argument switches between warn and ignore", {
  expect_warning(
    validate_frame(fp_restored, good_frame[-1, ], fingerprint = "warn"),
    "Frame differs"
  )
  expect_no_message(
    validate_frame(fp_restored, good_frame[-1, ], fingerprint = "ignore")
  )
})

test_that("designs without a fingerprint validate as before", {
  expect_no_message(validate_frame(fp_design, good_frame[-1, ]))

  no_fp <- read_design(design_json(fp_design))
  expect_no_message(validate_frame(no_fp, good_frame[-1, ]))
})

## Two-phase linkage pre-flight
#
# With a tbl_sample frame, validate_frame() warns about linkage problems that
# would fail at as_svydesign() time, and validation still passes.

test_that("plain data frame frames trigger no phase-linkage warning", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4)

  expect_no_warning(validate_frame(design, good_frame))
})

test_that("phase-2 design with shared unique id passes silently", {
  frame <- data.frame(id = 1:100, x = rnorm(100))
  phase1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 40) |>
    execute(frame, seed = 1)

  phase2_design <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 10)

  expect_no_warning(validate_frame(phase2_design, phase1))
  expect_true(validate_frame(phase2_design, phase1))
})

test_that("phases that declare different units are linkable", {
  # Neither phase redeclares the other's unit. The bridge is their compound.
  frame <- data.frame(
    psu = rep(1:8, each = 6), hh = rep(1:24, each = 2),
    id = seq_len(48), y = 1
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 4) |>
    execute(frame, seed = 1)

  phase2_design <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 3) |>
    add_stage() |> cluster_by(id) |> draw(n = 1)

  expect_no_warning(validate_frame(phase2_design, phase1))

  skip_if_not_installed("survey")
  expect_s3_class(
    quiet_across(as_svydesign(execute(phase2_design, phase1, seed = 3))),
    "twophase2"
  )
})

test_that("a phase-2 design that declares no unit still links through phase 1", {
  # Phase 1's identifier is on every phase-2 row, which the bridge needs.
  frame <- data.frame(id = 1:100, x = rnorm(100))
  phase1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 40) |>
    execute(frame, seed = 1)

  phase2_design <- sampling_design() |>
    draw(n = 10)

  expect_no_warning(validate_frame(phase2_design, phase1))

  skip_if_not_installed("survey")
  expect_s3_class(
    as_svydesign(execute(phase2_design, phase1, seed = 2)), "twophase2"
  )
})

test_that("a design warns when neither phase declares a unit", {
  frame <- data.frame(id = 1:100, x = rnorm(100))
  phase1 <- sampling_design() |>
    draw(n = 40) |>
    execute(frame, seed = 1)

  phase2_design <- sampling_design() |>
    draw(n = 10)

  expect_warning(
    validate_frame(phase2_design, phase1),
    class = "samplyr_warning_phase_linkage"
  )
  expect_true(suppressWarnings(validate_frame(phase2_design, phase1)))

  # The export the warning predicts does fail.
  skip_if_not_installed("survey")
  expect_error(
    as_svydesign(execute(phase2_design, phase1, seed = 2)),
    class = "samplyr_error_twophase_bridge"
  )
})

test_that("phase-2 design with non-unique phase-1 keys warns", {
  # psu repeats across the retained rows, so it cannot be the join key.
  frame <- data.frame(
    psu = rep(1:10, each = 5),
    id = 1:50,
    x = rnorm(50)
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 4) |>
    execute(frame, seed = 2)

  phase2_design <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 2)

  expect_warning(
    validate_frame(phase2_design, phase1),
    class = "samplyr_warning_phase_linkage"
  )
})

## execute() judges later stages before it draws
#
# A later stage is checked on every reachable parent, not only those a seed
# selects, so a frame defect fails on every seed.

na_psu_frame <- function() {
  frame <- data.frame(
    psu = rep(1:20, each = 5),
    id = 1:100,
    st = rep(c("A", "B", "A", "B", "A"), 20),
    m = stats::runif(100, 1, 2)
  )
  frame$st[frame$psu %in% c(3, 8, 14, 19)] <- NA
  frame
}

na_psu_design <- function() {
  sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 6) |>
    add_stage() |> stratify_by(st) |> draw(n = 1)
}

test_that("a later-stage defect is refused whatever the seed", {
  frame <- na_psu_frame()
  for (seed in 1:20) {
    expect_error(
      execute(na_psu_design(), frame, seed = seed),
      class = "samplyr_error_frame_invalid",
      label = paste("seed", seed)
    )
  }
  # The same class validate_frame() raises for the same frame.
  expect_error(
    validate_frame(na_psu_design(), frame),
    class = "samplyr_error_frame_invalid"
  )
})

test_that("separate registers are judged on every reachable unit", {
  frame <- na_psu_frame()
  psus <- frame[!duplicated(frame$psu), c("psu", "m")]
  for (seed in 1:5) {
    expect_error(
      execute(na_psu_design(), list(psus, frame), seed = seed),
      class = "samplyr_error_frame_invalid"
    )
  }

  # Rows of a PSU the first register does not list are unreachable.
  reachable <- psus[!psus$psu %in% c(3, 8, 14, 19), ]
  expect_s3_class(
    suppressWarnings(execute(na_psu_design(), list(reachable, frame), seed = 1)),
    "tbl_sample"
  )
})

test_that("a defect two stages down is refused before stage 1 draws", {
  frame <- na_psu_frame()
  frame$hh <- rep(1:2, length.out = 100)
  frame$st <- "A"
  frame$m[frame$psu == 11] <- NA
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 4) |>
    add_stage() |> cluster_by(hh) |> draw(n = 1) |>
    add_stage() |> draw(n = 1, method = "pps_brewer", mos = m)
  for (seed in 1:10) {
    expect_error(
      execute(design, frame, seed = seed),
      class = "samplyr_error_frame_invalid"
    )
  }
})

test_that("gaps between registers still only warn", {
  # An empty parent is tolerated until a selected unit falls in it.
  frame <- na_psu_frame()
  frame$st <- "A"
  psus <- frame[!duplicated(frame$psu), c("psu", "m")]
  lower <- frame[frame$psu != 20, ]
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> draw(n = 1)
  expect_warning(
    s <- execute(design, list(psus, lower), seed = 1),
    class = "samplyr_warning_frame_incomplete_register"
  )
  expect_s3_class(s, "tbl_sample")
})

## Sizes for strata the stage cannot reach

strata_frame <- function() {
  data.frame(
    id = 1:60,
    st = rep(c("A", "B", "C"), each = 20),
    g = rep(c("x", "y"), 30),
    psu = rep(1:12, each = 5)
  )
}

test_that("a size for a stratum the frame lacks is refused, not dropped", {
  frame <- strata_frame()
  designs <- list(
    named_n = sampling_design() |> stratify_by(st) |>
      draw(n = c(A = 2, B = 3, C = 1, Z = 4)),
    named_frac = sampling_design() |> stratify_by(st) |>
      draw(frac = c(A = 0.1, B = 0.1, C = 0.1, Z = 0.5)),
    table_n = sampling_design() |> stratify_by(st, g) |>
      draw(n = data.frame(
        st = c("A", "A", "B", "B", "C", "C", "Z"),
        g = c("x", "y", "x", "y", "x", "y", "x"),
        n = 1
      ))
  )
  expect_identical(names(designs), c("named_n", "named_frac", "table_n"))
  for (name in names(designs)) {
    expect_error(
      execute(designs[[name]], frame, seed = 1),
      class = "samplyr_error_alloc_unknown_strata",
      label = name
    )
    expect_error(
      validate_frame(designs[[name]], frame),
      class = "samplyr_error_alloc_unknown_strata",
      label = paste(name, "validate_frame")
    )
  }

  # Tables that describe strata may be reused and keep their extra rows.
  s <- sampling_design() |>
    stratify_by(st, alloc = "neyman", variance = c(A = 1, B = 2, C = 3, Z = 9)) |>
    draw(n = 10) |>
    execute(frame, seed = 1)
  expect_identical(nrow(s), 10L)
})

test_that("a later stage is judged on every parent it could reach", {
  frame <- strata_frame()
  # z exists in PSU 12 only. Most parents lack it, and that is normal.
  frame$g[frame$psu == 12] <- c("x", "y", "z", "z", "z")
  stage_two <- function(n) {
    sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 3) |>
      add_stage() |> stratify_by(g) |> draw(n = n)
  }
  expect_error(
    execute(stage_two(c(x = 1, y = 1, z = 1, w = 1)), frame, seed = 1),
    class = "samplyr_error_alloc_unknown_strata"
  )
  # Every seed passes: a parent without z takes its x and y entries only.
  for (seed in 1:5) {
    expect_s3_class(
      execute(stage_two(c(x = 1, y = 1, z = 1)), frame, seed = seed),
      "tbl_sample"
    )
  }

  # A fraction keyed on every unit of the frame, as a take-all table is.
  frame$ea <- frame$id
  by_ea <- data.frame(ea = frame$ea, frac = 0.5)
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> stratify_by(ea) |> draw(frac = by_ea)
  expect_s3_class(execute(design, frame, seed = 1), "tbl_sample")
})

test_that("a continuation is judged on the register it is given", {
  frame <- strata_frame()
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(st) |> draw(n = c(A = 1, B = 1, C = 1, Z = 1))
  first <- execute(design, frame, stages = 1, seed = 1)
  expect_error(
    execute(first, frame, seed = 2),
    class = "samplyr_error_alloc_unknown_strata"
  )

  # Two PSUs reach at most two strata, but the third is still in the register.
  complete <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(st) |> draw(n = c(A = 1, B = 1, C = 1))
  first <- execute(complete, frame, stages = 1, seed = 1)
  expect_lt(length(unique(first$st)), 3L)
  expect_s3_class(execute(first, frame, seed = 2), "tbl_sample")
  expect_no_error(validate_frame(first, frame))
})

test_that("a take table over every parent continues on a selected listing", {
  # The table is set before stage 1. The listing covers the selected PSUs.
  areas <- data.frame(psu = 1:4)
  frame <- expand.grid(hh = 1:10, psu = 1:4)
  frame$sex <- rep(c("f", "m"), length.out = nrow(frame))
  by_psu <- sampling_design() |>
    cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(psu) |>
    draw(n = data.frame(psu = 1:4, n = c(2, 3, 4, 5)))
  first <- execute(by_psu, areas, stages = 1, seed = 1)
  expect_identical(sort(first$psu), c(1L, 3L))
  listing <- frame[frame$psu %in% first$psu, ]
  expect_no_error(validate_frame(first, listing))
  resumed <- execute(first, listing, seed = 2)
  expect_identical(c(table(resumed$psu)), c("1" = 2L, "3" = 4L))
  named <- sampling_design() |>
    cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(psu) |>
    draw(n = c("1" = 2, "2" = 3, "3" = 4, "4" = 5))
  first_named <- execute(named, areas, stages = 1, seed = 1)
  resumed <- execute(first_named, listing, seed = 2)
  expect_identical(c(table(resumed$psu)), c("1" = 2L, "3" = 4L))

  # An entry under a selected parent is still judged.
  cells <- expand.grid(sex = c("f", "m"), psu = 1:4, stringsAsFactors = FALSE)
  cells$n <- 1
  by_cell <- function(take) {
    sampling_design() |>
      cluster_by(psu) |> draw(n = 2) |>
      add_stage() |> stratify_by(psu, sex) |> draw(n = take)
  }
  first <- execute(by_cell(cells), areas, stages = 1, seed = 1)
  expect_s3_class(execute(first, listing, seed = 2), "tbl_sample")
  typo <- rbind(cells, data.frame(sex = "x", psu = 3, n = 1))
  first <- execute(by_cell(typo), areas, stages = 1, seed = 1)
  expect_error(
    execute(first, listing, seed = 2),
    class = "samplyr_error_alloc_unknown_strata"
  )
  expect_error(
    validate_frame(first, listing),
    class = "samplyr_error_alloc_unknown_strata"
  )
})

test_that("a take keyed by a selected ancestor holds through every remaining stage", {
  # Resuming stages 2 and 3 together must accept what resuming them one call
  # at a time accepts.
  areas <- data.frame(psu = 1:4)
  frame <- expand.grid(person = 1:6, hh = 1:5, psu = 1:4)
  frame$sex <- rep(c("f", "m"), length.out = nrow(frame))
  takes <- data.frame(psu = 1:4, n = c(2, 3, 4, 5))
  by_psu <- sampling_design() |>
    cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> stratify_by(psu) |> draw(n = takes)
  first <- execute(by_psu, areas, stages = 1, seed = 1)
  expect_identical(sort(first$psu), c(1L, 3L))
  listing <- frame[frame$psu %in% first$psu, ]

  expect_no_error(validate_frame(first, listing, stages = 2:3))
  together <- execute(first, listing, stages = 2:3, seed = 2)
  expect_identical(c(table(together$psu)), c("1" = 4L, "3" = 8L))

  # Keyed by the PSU and by the household this same call selects.
  per_home <- expand.grid(hh = 1:5, psu = 1:4)
  per_home$n <- 1
  by_home <- sampling_design() |>
    cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> stratify_by(psu, hh) |> draw(n = per_home)
  first_home <- execute(by_home, areas, stages = 1, seed = 1)
  expect_no_error(validate_frame(first_home, listing, stages = 2:3))
  expect_identical(
    nrow(execute(first_home, listing, stages = 2:3, seed = 2)),
    4L
  )

  # An entry under a selected PSU is still judged at the lower stage.
  cells <- expand.grid(sex = c("f", "m", "x"), psu = 1:4,
                       stringsAsFactors = FALSE)
  cells$n <- 1
  by_cell <- sampling_design() |>
    cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> stratify_by(psu, sex) |> draw(n = cells)
  first <- execute(by_cell, areas, stages = 1, seed = 1)
  expect_error(
    execute(first, listing, stages = 2:3, seed = 2),
    class = "samplyr_error_alloc_unknown_strata"
  )
  expect_error(
    validate_frame(first, listing, stages = 2:3),
    class = "samplyr_error_alloc_unknown_strata"
  )

  # Stage 1 run on the full hierarchy keeps every household column of the
  # selected PSUs, but no household has been selected yet.
  typo <- rbind(per_home, data.frame(hh = 99, psu = 1, n = 1))
  by_home_typo <- sampling_design() |>
    cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> stratify_by(psu, hh) |> draw(n = typo)
  expanded <- execute(by_home_typo, frame, stages = 1, seed = 1)
  expect_true("hh" %in% names(expanded))
  expect_error(
    execute(expanded, listing, stages = 2:3, seed = 2),
    class = "samplyr_error_alloc_unknown_strata"
  )
  expect_error(
    validate_frame(expanded, listing, stages = 2:3),
    class = "samplyr_error_alloc_unknown_strata"
  )
})

test_that("a named size that misses a stratum has the table form's class", {
  frame <- strata_frame()
  expect_error(
    sampling_design() |> stratify_by(st) |> draw(n = c(A = 2, B = 3)) |>
      execute(frame, seed = 1),
    class = "samplyr_error_alloc_missing_coverage"
  )
  expect_error(
    sampling_design() |> stratify_by(st) |> draw(frac = c(A = 0.1)) |>
      execute(frame, seed = 1),
    class = "samplyr_error_alloc_missing_coverage"
  )
})
