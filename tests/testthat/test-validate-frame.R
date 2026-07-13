# Coverage for validate_frame() issue-detection branches and input guards.
# Existing tests (test-prn.R, test-edge-cases.R, test-balanced.R) cover PRN
# checks, NA strata/cluster, and balanced aux type, these fill the remaining
# branches: input guards, the happy path, and the missing/type/range issues.

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
  expect_error(validate_frame(design, good_frame[0, ]), "0 rows")
})

test_that("validate_frame accepts a valid stage selector", {
  design <- sampling_design() |>
    cluster_by(district) |>
    draw(n = 2, method = "pps_brewer", mos = size) |>
    add_stage() |>
    draw(n = 1)

  expect_true(validate_frame(design, good_frame, stage = 1))
})

test_that("validate_frame detects a missing stratification variable", {
  design <- sampling_design() |>
    stratify_by(zzz) |>
    draw(n = 4)

  expect_error(
    validate_frame(design, good_frame),
    "missing stratification variable"
  )
})

test_that("validate_frame detects a missing cluster variable", {
  design <- sampling_design() |>
    cluster_by(zzz) |>
    draw(n = 2, method = "pps_brewer", mos = size)

  expect_error(
    validate_frame(design, good_frame),
    "missing cluster variable"
  )
})

test_that("validate_frame detects MOS problems", {
  base <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = size)

  # missing
  missing_mos <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = zzz)
  expect_error(validate_frame(missing_mos, good_frame), "missing MOS variable")

  # non-numeric
  type_mos <- sampling_design() |>
    draw(n = 2, method = "pps_brewer", mos = region)
  expect_error(validate_frame(type_mos, good_frame), "must be numeric")

  # NA
  na_frame <- good_frame
  na_frame$size[1] <- NA
  expect_error(validate_frame(base, na_frame), "contains NA values")

  # negative
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
    "missing control variable"
  )
})

# Fingerprint comparison for designs restored with read_design() ---------

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

  msgs <- capture.output(
    validate_frame(fp_restored, modified),
    type = "message"
  )
  text <- paste(msgs, collapse = " ")
  expect_match(text, "\"y\" no longer present")
  expect_match(text, "new column \"extra\"")
  expect_match(text, "size \\(character instead of numeric\\)")
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

# Two-phase linkage pre-flight -----------------------------------------------
#
# When the frame is a tbl_sample (phase-2 preparation), validate_frame()
# warns about linkage problems that would otherwise only fail at
# as_svydesign() time. Validation itself still passes.

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

test_that("phase-2 design without shared identifiers warns", {
  frame <- data.frame(id = 1:100, x = rnorm(100))
  phase1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 40) |>
    execute(frame, seed = 1)

  phase2_design <- sampling_design() |>
    draw(n = 10)

  expect_warning(
    validate_frame(phase2_design, phase1),
    class = "samplyr_warning_phase_linkage"
  )
  # validation still passes
  expect_true(suppressWarnings(validate_frame(phase2_design, phase1)))
})

test_that("phase-2 design with non-unique phase-1 keys warns", {
  # Phase 1 selects whole clusters: psu is shared but repeats across
  # the retained rows, so it cannot serve as the join key.
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
