# Normalized stage registers and stage-continuation linkage: one register per
# STAGE, which is what "multi-frame" means in this file's sense. The other
# sense, several frames covering one population, is `stack_frames()` and lives
# in test-stack-frames*.R. Every `multiframe` identifier in R/ carries that
# other sense; this file was named before it existed.
#
# Fixtures live in helper-stage-registers.R.
# for the contract these tests enforce.

## T0. Golden equivalence
#
# The one-call form is shorthand for the continuation chain. The three frame
# shapes must select the same elements and produce the same weights under one
# shared RNG stream with an explicit stages = on every intermediate call.
# This holds today and must keep holding through every phase of the linkage
# refactor: it is the regression harness for the whole change.

test_that("all three frame spellings select identically under one RNG stream", {
  design <- mf_design()

  all_at_once <- execute(
    design, mf_schools(), mf_classes(), mf_students(), seed = 7
  )

  continued <- withr::with_seed(7, {
    s1 <- execute(design, mf_schools(), stages = 1)
    s2 <- execute(s1, mf_classes(), stages = 2)
    execute(s2, mf_students(), stages = 3)
  })

  hierarchical <- execute(design, mf_hierarchy(), seed = 7)

  expect_identical(mf_keys(all_at_once), mf_keys(continued))
  expect_identical(mf_keys(all_at_once), mf_keys(hierarchical))

  for (col in c(".weight", ".weight_1", ".weight_2", ".weight_3")) {
    expect_identical(all_at_once[[col]], continued[[col]], info = col)
    expect_identical(all_at_once[[col]], hierarchical[[col]], info = col)
  }

  expect_identical(get_stages_executed(all_at_once), 1:3)
  expect_identical(get_stages_executed(continued), 1:3)
})

test_that("per-call seeds are a different RNG boundary, not an equivalence", {
  design <- mf_design()

  one_call <- execute(
    design, mf_schools(), mf_classes(), mf_students(), seed = 7
  )
  s1 <- execute(design, mf_schools(), stages = 1, seed = 101)
  s2 <- execute(s1, mf_classes(), stages = 2, seed = 102)
  per_call <- execute(s2, mf_students(), stages = 3, seed = 103)

  expect_s3_class(per_call, "tbl_sample")
  expect_false(identical(mf_keys(one_call), mf_keys(per_call)))
})

test_that("the fixture reproduces its documented compound weights", {
  sample <- execute(
    mf_design(), mf_schools(), mf_classes(), mf_students(), seed = 7
  )

  expect_identical(nrow(sample), 4L)
  expect_equal(unique(sample$.weight_1), 2)
  expect_equal(unique(sample$.weight_2), 2)
  expect_equal(unique(sample$.weight_3), 3 / 2)
  expect_equal(unique(sample$.weight), 6)
  expect_equal(sum(sample$.weight), nrow(mf_students()))
})

## P1. Frame scheduling

test_that("the frame count must be one or one per executed stage", {
  design <- mf_design()

  expect_error(
    execute(design, mf_schools(), mf_classes(), seed = 7),
    class = "samplyr_error_frame_count"
  )
  expect_error(
    execute(
      design, mf_schools(), mf_classes(), mf_students(), mf_students(),
      seed = 7
    ),
    class = "samplyr_error_frame_count"
  )
  # Two frames for the two stages this call executes is the exact count.
  expect_s3_class(
    execute(design, mf_schools(), mf_classes(), stages = 1:2, seed = 7),
    "tbl_sample"
  )
})

test_that("the frame count error names the stages being executed", {
  # Labels are optional, so the message carries the position either way.
  expect_error(
    execute(mf_design(), mf_schools(), mf_classes(), seed = 7),
    "stage 1 \"Schools\""
  )

  unlabelled <- sampling_design() |>
    cluster_by(school_id) |> draw(n = 1) |>
    add_stage() |> draw(n = 1)
  expect_error(
    execute(unlabelled, mf_schools(), mf_classes(), mf_students(), seed = 7),
    "stage 1(?! \")", perl = TRUE
  )
})

test_that("frame names are diagnostic and position controls the mapping", {
  design <- mf_design()

  positional <- execute(
    design, mf_schools(), mf_classes(), mf_students(), seed = 7
  )
  named <- execute(
    design,
    schools = mf_schools(), classes = mf_classes(), students = mf_students(),
    seed = 7
  )
  # Names that do not match the stage labels still map by position.
  misnamed <- execute(
    design,
    third = mf_schools(), first = mf_classes(), second = mf_students(),
    seed = 7
  )

  expect_identical(mf_keys(positional), mf_keys(named))
  expect_identical(mf_keys(positional), mf_keys(misnamed))
})

test_that("a static misuse fails before the RNG stream advances", {
  design <- mf_design()
  set.seed(99)
  before <- .Random.seed

  try(execute(design, mf_schools(), mf_classes(), seed = 7), silent = TRUE)
  expect_identical(.Random.seed, before)

  try(execute(design, mf_schools(), stages = 2, seed = 7), silent = TRUE)
  expect_identical(.Random.seed, before)
})

test_that("only the first frame of a new design may be a tbl_sample", {
  frame <- data.frame(
    psu = rep(1:10, each = 20), id = seq_len(200), y = seq_len(200)
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 5) |>
    execute(frame, seed = 1)

  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> draw(n = 3)

  expect_error(
    execute(design, frame, phase1, seed = 2),
    class = "samplyr_error_phase_frame_position"
  )

  # In first position it is the previous phase, which is supported.
  expect_s3_class(execute(design, phase1, seed = 2), "tbl_sample")
})

test_that("an element stage may be terminal but cannot be continued", {
  frame <- data.frame(
    psu = rep(1:10, each = 20), id = seq_len(200), y = seq_len(200)
  )
  design <- sampling_design() |>
    add_stage("Elements") |> draw(n = 40) |>
    add_stage("Within") |> cluster_by(psu) |> draw(n = 2)

  # Executing it alone is permitted: nothing samples within it.
  s1 <- execute(design, frame, stages = 1, seed = 3)
  expect_identical(get_stages_executed(s1), 1L)

  # Continuing it is not: stage 1 named no units.
  expect_error(
    execute(s1, frame, stages = 2, seed = 4),
    class = "samplyr_error_stage_parent_id"
  )
})

## P2. Strict stage transitions

test_that("linkage never reaches a join without a resolved key", {
  # A design with no parent identity cannot be executed at all, so this
  # exercises the guard directly. It is what keeps a join from being called
  # with an empty `by`, which dplyr would treat as a cross join.
  design <- sampling_design() |>
    draw(n = 5) |>
    add_stage() |>
    draw(n = 2)
  frame <- data.frame(id = 1:20)

  expect_error(
    samplyr:::link_stage_frame(frame, frame, design, stage_idx = 2L),
    class = "samplyr_error_frame_missing_ancestry"
  )
})

test_that("a row with no parent is filtered, not treated as a frame defect", {
  # Supplying several frames says nothing about their granularity, so an
  # unlinked row cannot be grounds for rejecting the frame. It matches no
  # complete selected key and drops out like any other unrelated row.
  classes <- mf_classes()
  classes$school_id[3] <- NA

  sample <- execute(mf_design(), mf_schools(), classes, mf_students(), seed = 7)

  expect_identical(nrow(sample), 4L)
  # Every selected class comes from a row that named its school. The unlinked
  # row is simply not part of any pool, so S2 draws from what remains.
  linked <- classes[!is.na(classes$school_id), , drop = FALSE]
  selected <- unique(sample[, c("school_id", "class_id")])
  expect_identical(
    nrow(dplyr::anti_join(selected, linked, by = c("school_id", "class_id"))),
    0L
  )
})

test_that("a parent whose only rows are unlinked is a coverage failure", {
  # When the missing key is the sole representation of a selected unit, the
  # coverage check reports it. There is no separate NA rule to reach.
  classes <- mf_classes()
  classes$school_id[classes$school_id == "S4"] <- NA

  expect_warning(
    expect_error(
      execute(mf_design(), mf_schools(), classes, mf_students(), seed = 1),
      class = "samplyr_error_frame_missing_parent"
    ),
    class = "samplyr_warning_frame_incomplete_register"
  )
})

test_that("ancestry NA reporting stays available for explicit preflight", {
  # Strictness belongs where the user asked for frames to be checked rather
  # than sampled. This is the primitive validate_frame() will call.
  classes <- mf_classes()
  classes$school_id[3] <- NA

  expect_error(
    samplyr:::check_parent_key_na(
      classes, "school_id", mf_design(), stage_idx = 2L,
      frame_index = 2L, frame_label = NULL
    ),
    class = "samplyr_error_frame_ancestry_na"
  )
})

test_that("ancestry types must be join-compatible", {
  classes <- mf_classes()
  classes$school_id <- seq_len(nrow(classes))

  expect_error(
    execute(mf_design(), mf_schools(), classes, mf_students(), seed = 7),
    class = "samplyr_error_frame_key_type"
  )
})

test_that("compound keys survive punctuation and braces", {
  # Frame values are data. A key containing cli markup must not be read as a
  # template, in the sample or in any diagnostic.
  schools <- mf_schools()
  schools$school_id <- c("S/1", "S{2}", "S 3", "S-4")
  classes <- mf_classes()
  classes$school_id <- rep(schools$school_id, each = 2)
  students <- mf_students()
  students$school_id <- rep(schools$school_id, each = 6)

  sample <- execute(mf_design(), schools, classes, students, seed = 7)
  expect_identical(nrow(sample), 4L)
  expect_true(all(sample$school_id %in% schools$school_id))
})

test_that("extra rows for unselected parents do not change the sample", {
  # A complete national register may be handed to a design that sampled one
  # region. Users must not have to pre-filter.
  extra_schools <- data.frame(
    school_id = paste0("X", 1:3), class_id = "C1", stringsAsFactors = FALSE
  )
  padded <- rbind(mf_classes(), extra_schools)

  with_extra <- execute(mf_design(), mf_schools(), padded, mf_students(),
                        seed = 7)
  clean <- execute(mf_design(), mf_schools(), mf_classes(), mf_students(),
                   seed = 7)

  expect_identical(mf_keys(with_extra), mf_keys(clean))
  expect_identical(with_extra$.weight, clean$.weight)
})

test_that("every selected parent missing is reported, not just the first", {
  empty_classes <- mf_classes()[0, , drop = FALSE]

  err <- tryCatch(
    suppressWarnings(
      execute(mf_design(), mf_schools(), empty_classes, mf_students(), seed = 7)
    ),
    error = function(e) e
  )
  expect_s3_class(err, "samplyr_error_frame_missing_parent")
  expect_match(conditionMessage(err), "2 units")
})

test_that("with-replacement parents are covered once per population unit", {
  # Draw occurrences repeat a parent. Coverage is a property of the population
  # key, so a parent hit twice needs one set of rows, not two.
  frame <- data.frame(
    psu = rep(1:6, each = 10),
    mos = rep(c(50, 40, 30, 20, 10, 5), each = 10),
    id = seq_len(60),
    y = seq_len(60)
  )
  sample <- sampling_design() |>
    add_stage() |>
      cluster_by(psu) |>
      draw(n = 4, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 2) |>
    execute(frame, seed = 11)

  expect_s3_class(sample, "tbl_sample")
  expect_true(".draw_1" %in% names(sample))
  # A repeated parent contributes an independent second-stage draw.
  expect_gte(max(sample$.draw_1), 1L)
})

test_that("the candidate-register warning fires once per call", {
  classes <- mf_classes_without_s4()

  # Not once per stage, and not once per replicate.
  warnings <- character(0)
  withCallingHandlers(
    execute(mf_design(), mf_schools(), classes, mf_students(), seed = 2),
    samplyr_warning_frame_incomplete_register = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_length(warnings, 1L)
  expect_match(warnings, "S4")
})

test_that("one hierarchy sampled three ways gives one sample", {
  # Frame count schedules stages; cluster_by() identifies the sampling unit.
  # The same finer-grained hierarchy therefore samples identically whether it
  # is supplied once, once per stage, or across a continuation. Repeated
  # compound keys are what a hierarchy is, and must not be penalized.
  design <- mf_design()
  hierarchy <- mf_hierarchy()

  once <- execute(design, hierarchy, seed = 7)
  per_stage <- execute(design, hierarchy, hierarchy, hierarchy, seed = 7)
  continued <- withr::with_seed(7, {
    s1 <- execute(design, hierarchy, stages = 1)
    s2 <- execute(s1, hierarchy, stages = 2)
    execute(s2, hierarchy, stages = 3)
  })

  expect_identical(mf_keys(once), mf_keys(per_stage))
  expect_identical(mf_keys(once), mf_keys(continued))

  for (col in c(".weight", ".weight_1", ".weight_2", ".weight_3")) {
    expect_identical(once[[col]], per_stage[[col]], info = col)
    expect_identical(once[[col]], continued[[col]], info = col)
  }

  # The sampling unit is the distinct compound key, not the row.
  n_schools <- function(x) length(unique(x$school_id))
  n_classes <- function(x) nrow(unique(x[, c("school_id", "class_id")]))
  expect_identical(n_schools(once), n_schools(per_stage))
  expect_identical(n_schools(once), n_schools(continued))
  expect_identical(n_classes(once), n_classes(per_stage))
  expect_identical(n_classes(once), n_classes(continued))
})

## P3. Carry-forward of prior design variables

test_that("a stratum introduced at a later stage also survives", {
  # Not just stage 1: every completed stage's strata must reach the sample,
  # wherever they were introduced.
  classes <- mf_classes()
  classes$track <- rep(c("Science", "Arts"), times = 4)

  design <- sampling_design() |>
    add_stage("Schools") |>
      stratify_by(school_type) |> cluster_by(school_id) |> draw(n = 1) |>
    add_stage("Classes") |>
      stratify_by(track) |> cluster_by(class_id) |> draw(n = 1) |>
    add_stage("Students") |> draw(n = 2)

  sample <- execute(design, mf_schools(), classes, mf_students(), seed = 7)

  expect_true(all(c("school_type", "track") %in% names(sample)))
  expect_false(any(grepl("\\.[xy]$", names(sample))))
})

test_that("an agreeing copy in the lower frame is kept, not duplicated", {
  classes <- mf_classes()
  classes$school_type <- rep(c("Public", "Public", "Private", "Private"),
                             each = 2)

  sample <- execute(mf_design(), mf_schools(), classes, mf_students(),
                    seed = 7)
  clean <- execute(mf_design(), mf_schools(), mf_classes(), mf_students(),
                   seed = 7)

  expect_identical(sum(names(sample) == "school_type"), 1L)
  expect_false(any(grepl("\\.[xy]$", names(sample))))
  expect_identical(mf_keys(sample), mf_keys(clean))
  expect_identical(sample$school_type, clean$school_type)
})

test_that("a disagreeing copy in the lower frame is an error", {
  classes <- mf_classes()
  # S1 is Public in the school register.
  classes$school_type <- "Private"

  expect_error(
    execute(mf_design(), mf_schools(), classes, mf_students(), seed = 7),
    class = "samplyr_error_frame_parent_conflict"
  )
})

test_that("carrying a variable does not reorder the frame it samples from", {
  # Systematic selection reads frame order, so a join that reorders rows
  # would silently change the sample. Compare a frame that needs the join
  # against one that already carries the value and skips it.
  design <- sampling_design() |>
    add_stage("Schools") |>
      stratify_by(school_type) |> cluster_by(school_id) |> draw(n = 2) |>
    add_stage("Students") |> draw(n = 3, method = "systematic")

  students <- mf_students()
  # Shuffle deterministically so frame order is not already sorted by parent.
  students <- students[order(students$student_no, students$class_id), ]
  rownames(students) <- NULL

  pre_joined <- dplyr::left_join(students, mf_schools()[, c("school_id",
                                                            "school_type")],
                                 by = "school_id")

  needs_join <- execute(design, mf_schools(), students, seed = 5)
  no_join <- execute(design, mf_schools(), pre_joined, seed = 5)

  expect_identical(mf_keys(needs_join), mf_keys(no_join))
  expect_identical(needs_join$.weight, no_join$.weight)
})

test_that("carry-forward handles non-syntactic strata names", {
  schools <- mf_schools()
  names(schools)[names(schools) == "school_type"] <- "school type"

  design <- sampling_design() |>
    add_stage("Schools") |>
      stratify_by(`school type`) |> cluster_by(school_id) |> draw(n = 1) |>
    add_stage("Classes") |> cluster_by(class_id) |> draw(n = 1) |>
    add_stage("Students") |> draw(n = 2)

  sample <- execute(design, schools, mf_classes(), mf_students(), seed = 7)
  expect_true("school type" %in% names(sample))
})

test_that("an unstratified stage between stratified stages carries both", {
  classes <- mf_classes()
  students <- mf_students()
  students$sex <- rep(c("F", "M", "F"), times = 8)

  design <- sampling_design() |>
    add_stage("Schools") |>
      stratify_by(school_type) |> cluster_by(school_id) |> draw(n = 1) |>
    add_stage("Classes") |> cluster_by(class_id) |> draw(n = 1) |>
    add_stage("Students") |> stratify_by(sex) |> draw(n = 1)

  sample <- execute(design, mf_schools(), classes, students, seed = 7)
  expect_true(all(c("school_type", "sex") %in% names(sample)))
})

## T1. Reproductions of the four verified defects
#
# These fail until the linkage refactor lands. They are written against the
# contract, not against current behavior. Expected to turn green in:
#   parent coverage  -> phase 2
#   ancestry         -> phase 2
#   strata carry     -> phase 3

test_that("a realized parent with no rows in the next register is an error", {
  # Before the linkage refactor this returned 2 rows for S1 alone, silently
  # dropping the Private stratum that S4 represented.
  #
  # The register gap is visible before sampling and the selection realizes it,
  # so the call warns about the incomplete register and then fails.
  expect_warning(
    expect_error(
      execute(
        mf_design(), mf_schools(), mf_classes_without_s4(), mf_students(),
        seed = 1
      ),
      class = "samplyr_error_frame_missing_parent"
    ),
    class = "samplyr_warning_frame_incomplete_register"
  )
})

test_that("an unrealized candidate gap warns once and still samples", {
  # Today: succeeds with no diagnostic at all.
  expect_warning(
    incomplete <- execute(
      mf_design(), mf_schools(), mf_classes_without_s4(), mf_students(),
      seed = 2
    ),
    class = "samplyr_warning_frame_incomplete_register"
  )

  complete <- execute(
    mf_design(), mf_schools(), mf_classes(), mf_students(), seed = 2
  )

  # The gap is diagnostic only: it must not change the realization.
  expect_identical(mf_keys(incomplete), mf_keys(complete))
  expect_identical(incomplete$.weight, complete$.weight)
})

test_that("a register missing parent ancestry fails as a samplyr error", {
  # Today: two dplyr `by = character()` cross-join deprecation warnings,
  # then base R's "undefined columns selected".
  classes_no_ancestry <- mf_classes()[, "class_id", drop = FALSE]

  expect_error(
    execute(
      mf_design(), mf_schools(), classes_no_ancestry, mf_students(), seed = 7
    ),
    class = "samplyr_error_frame_missing_ancestry"
  )

  # The failure must not arrive as base R subsetting. The companion dplyr
  # cross-join deprecation is not assertable here: dplyr signals it once per
  # session and lifecycle_verbosity does not override that, so any expectation
  # on it would pass or fail according to test order. Phase 2 covers it
  # directly instead, by asserting that the transition helper refuses an empty
  # key before it can reach a join.
  err <- tryCatch(
    execute(
      mf_design(), mf_schools(), classes_no_ancestry, mf_students(), seed = 7
    ),
    error = function(e) e
  )
  expect_false(grepl("undefined columns selected", conditionMessage(err)))
})

test_that("a stage-1 stratum held only in the school register survives", {
  # Today: school_type never reaches the sample, so as_svydesign() fails
  # with "object 'school_type' not found".
  sample <- execute(
    mf_design(), mf_schools(), mf_classes(), mf_students(), seed = 7
  )

  expect_true("school_type" %in% names(sample))
  expect_setequal(
    unique(sample[, c("school_id", "school_type")])$school_type,
    c("Public", "Private")
  )
  expect_false(any(grepl("\\.[xy]$", names(sample))))
})

test_that("normalized registers export to survey with carried strata", {
  skip_if_not_installed("survey")

  sample <- execute(
    mf_design(), mf_schools(), mf_classes(), mf_students(), seed = 7
  )

  expect_no_error(as_svydesign(sample))
})

## P4. Execution paths, metadata and digest

test_that("a carried column is not attributed to the frame it was joined onto", {
  # school_type reaches the sample from the school register. The class
  # register never contained it, and its digest record must say so.
  sample <- execute(
    mf_design(), mf_schools(), mf_classes(), mf_students(),
    seed = 7, frame_digest = "full"
  )
  digest <- get_frame_digest(sample)

  expect_true("school_type" %in% names(sample))
  roles <- lapply(digest$frames, function(f) f$roles$column)
  carried_anywhere <- vapply(
    roles[-1], function(cols) "school_type" %in% cols, logical(1)
  )
  expect_false(any(carried_anywhere))
})

test_that("separate registers work in every digest mode", {
  for (mode in c("none", "summary", "full")) {
    sample <- execute(
      mf_design(), mf_schools(), mf_classes(), mf_students(),
      seed = 7, frame_digest = mode
    )
    expect_identical(nrow(sample), 4L, info = mode)
    if (identical(mode, "none")) {
      expect_null(get_frame_digest(sample), info = mode)
    } else {
      expect_false(is.null(get_frame_digest(sample)), info = mode)
    }
  }
})

test_that("each register is recorded as its own frame", {
  sample <- execute(
    mf_design(), mf_schools(), mf_classes(), mf_students(),
    seed = 7, frame_digest = "full"
  )
  # Three distinct registers, three distinct frame records.
  expect_identical(length(get_frame_digest(sample)$frames), 3L)

  shared <- execute(mf_design(), mf_hierarchy(), seed = 7,
                    frame_digest = "full")
  # One frame recycled across stages stays one record.
  expect_identical(length(get_frame_digest(shared)$frames), 1L)
})

test_that("replicates check parent coverage independently", {
  # Coverage depends on what each replicate selected, so it cannot be
  # decided once for the call.
  expect_s3_class(
    execute(mf_design(), mf_schools(), mf_classes(), mf_students(),
            seed = 7, reps = 3),
    "tbl_sample"
  )

  expect_error(
    suppressWarnings(
      execute(mf_design(), mf_schools(), mf_classes_without_s4(),
              mf_students(), seed = 1, reps = 3)
    ),
    class = "samplyr_error_frame_missing_parent"
  )
})

test_that("the register warning is not repeated per replicate", {
  seen <- 0L
  withCallingHandlers(
    try(
      execute(mf_design(), mf_schools(), mf_classes_without_s4(),
              mf_students(), seed = 2, reps = 3),
      silent = TRUE
    ),
    samplyr_warning_frame_incomplete_register = function(w) {
      seen <<- seen + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(seen, 1L)
})

test_that("panels are assigned over separate registers", {
  sample <- execute(
    mf_design(), mf_schools(), mf_classes(), mf_students(),
    seed = 7, panels = 2
  )
  expect_true(".panel" %in% names(sample))
  expect_true(all(sample$.panel %in% 1:2))
})

test_that("a continuation keeps the digest of the stages it did not run", {
  s1 <- execute(mf_design(), mf_schools(), stages = 1, seed = 7,
                frame_digest = "full")
  s2 <- execute(s1, mf_classes(), stages = 2, seed = 8,
                frame_digest = "full")

  digest <- get_frame_digest(s2)
  expect_false(is.null(digest))
  expect_identical(length(digest$stages), 2L)
})

## Compound keys, temporary names and pre-RNG guarantees

test_that("compound ancestry keys are matched exactly, not as pasted text", {
  # ("a/b", "c") and ("a", "b/c") render identically if a compound key is
  # flattened to a string, which both invents conflicts and hides real ones.
  schools <- data.frame(
    school_id = c("a/b", "a"), school_type = c("P", "Q"),
    stringsAsFactors = FALSE
  )
  classes <- data.frame(
    school_id = c("a/b", "a"), class_id = c("c", "b/c"),
    stringsAsFactors = FALSE
  )
  students <- data.frame(
    school_id = rep(c("a/b", "a"), each = 2),
    class_id = rep(c("c", "b/c"), each = 2),
    student_no = rep(1:2, times = 2),
    stringsAsFactors = FALSE
  )
  design <- sampling_design() |>
    add_stage() |> stratify_by(school_type) |> cluster_by(school_id) |>
      draw(n = 1) |>
    add_stage() |> cluster_by(class_id) |> draw(n = 1) |>
    add_stage() |> draw(n = 1)

  # Correct values must not be read as a conflict.
  agreeing <- students
  agreeing$school_type <- rep(c("P", "Q"), each = 2)
  expect_s3_class(
    execute(design, schools, classes, agreeing, seed = 3), "tbl_sample"
  )

  # A real disagreement must not be masked by a colliding key.
  conflicting <- students
  conflicting$school_type <- rep(c("Q", "P"), each = 2)
  expect_error(
    execute(design, schools, classes, conflicting, seed = 3),
    class = "samplyr_error_frame_parent_conflict"
  )
})

test_that("a static frame failure does not consume the RNG stream", {
  # No explicit seed, so nothing restores the stream afterwards. A missing
  # column is knowable before sampling and must be reported before it.
  design <- mf_design()
  set.seed(4)
  before <- .Random.seed

  no_ancestry <- mf_classes()[, "class_id", drop = FALSE]
  try(execute(design, mf_schools(), no_ancestry, mf_students()), silent = TRUE)
  expect_identical(.Random.seed, before)

  no_cluster <- mf_classes()[, "school_id", drop = FALSE]
  try(execute(design, mf_schools(), no_cluster, mf_students()), silent = TRUE)
  expect_identical(.Random.seed, before)
})

test_that("internal temporary columns do not collide with frame columns", {
  # Both names are legitimate user columns.
  frame <- data.frame(
    psu = rep(1:6, each = 4),
    mos = rep(c(50, 40, 30, 20, 10, 5), each = 4),
    id = seq_len(24),
    .row_id = seq_len(24),
    .prev_weight = seq_len(24)
  )

  wr <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
      draw(n = 3, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 2) |>
    execute(frame, seed = 6)

  expect_s3_class(wr, "tbl_sample")
  expect_true(all(c(".row_id", ".prev_weight") %in% names(wr)))
  # The user's values survive untouched.
  expect_true(all(wr$.row_id %in% frame$.row_id))
  expect_true(all(wr$.prev_weight %in% frame$.prev_weight))
})

test_that("a previous-phase frame is fingerprinted as supplied", {
  frame <- data.frame(psu = rep(1:8, each = 5), id = seq_len(40), y = 1)
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 4) |>
    execute(frame, seed = 1)

  phase2 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 2) |>
    execute(phase1, seed = 2, frame_digest = "full")

  # Preparing the frame strips generated columns and adds an internal weight;
  # the digest must describe what the user passed, not that intermediate.
  digest <- get_frame_digest(phase2)
  expect_false(is.null(digest))
  expect_identical(
    digest$frames[[1]]$fingerprint_exact,
    samplyr:::frame_content_hash(phase1)
  )
})

test_that("replicated multiphase keeps the whole phase-1 sample", {
  frame <- data.frame(psu = rep(1:10, each = 4), id = seq_len(40), y = 1)
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 2) |>
    execute(frame, seed = 1, reps = 2)

  phase2 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 1) |>
    execute(phase1, seed = 2)

  prev <- attr(phase2, "metadata")$prev_phase
  # Not the final replicate's subset.
  expect_identical(nrow(prev$sample), nrow(phase1))
  expect_true(".replicate" %in% names(prev$sample))
  expect_identical(
    sort(unique(prev$sample$.replicate)), sort(unique(phase1$.replicate))
  )
})

test_that("a single frame for a single stage is neither shared nor separate", {
  design <- mf_design()
  schedule <- samplyr:::stage_frame_schedule(
    design, list(mf_schools()), stages = 1, executed = NULL
  )
  expect_identical(schedule$frame_mode, "single_frame")

  shared <- samplyr:::stage_frame_schedule(
    design, list(mf_hierarchy()), stages = NULL, executed = NULL
  )
  expect_identical(shared$frame_mode, "shared_frame")
})

test_that("clustered PRN and control variables must be cluster-constant", {
  # Selection reads one representative row per cluster, so a value that varies
  # within a cluster makes the result depend on descendant row order.
  frame <- data.frame(
    cluster = rep(1:6, each = 4),
    mos = rep(c(50, 40, 30, 20, 10, 5), each = 4),
    id = seq_len(24),
    varying = seq_len(24)
  )
  frame$prn <- seq_len(24) / 25

  expect_error(
    sampling_design() |>
      cluster_by(cluster) |>
      draw(n = 3, method = "pps_pareto", mos = mos, prn = prn) |>
      execute(frame, seed = 1),
    "constant within each cluster"
  )

  expect_error(
    sampling_design() |>
      cluster_by(cluster) |>
      draw(n = 3, method = "systematic", control = varying) |>
      execute(frame, seed = 1),
    "constant within each cluster"
  )

  # One value per cluster is accepted.
  frame$prn <- rep(seq_len(6) / 7, each = 4)
  frame$varying <- rep(seq_len(6), each = 4)
  expect_s3_class(
    sampling_design() |>
      cluster_by(cluster) |>
      draw(n = 3, method = "pps_pareto", mos = mos, prn = prn) |>
      execute(frame, seed = 1),
    "tbl_sample"
  )
})

## P5. Phase linkage and two-phase export

test_that("phase-1 identifiers survive a normalized later register", {
  # The element register carries hh but not psu. Without phase-key carry the
  # phase-1 identifier is lost and the two-phase export has no bridge.
  frame <- data.frame(
    psu = rep(1:10, each = 20), hh = rep(1:100, each = 2),
    id = seq_len(200), y = as.numeric(seq_len(200))
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 5) |>
    execute(frame, seed = 1)

  design2 <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |> cluster_by(id) |> draw(n = 1)

  elements <- frame[, c("hh", "id", "y")]
  phase2 <- execute(design2, phase1, elements, seed = 2)

  expect_true("psu" %in% names(phase2))
  expect_true(all(phase2$psu %in% phase1$psu))
})

test_that("an ambiguous phase key is refused before any stage draws", {
  # Carry is only valid when each unit maps to one phase-1 unit. A clustered
  # stage keeps one representative row, so this cannot be left to the carry:
  # by then the second value is already gone.
  frame <- data.frame(
    psu = rep(1:10, each = 20), hh = rep(1:100, each = 2),
    id = seq_len(200), y = 1
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 5) |>
    execute(frame, seed = 1)
  # Break the hierarchy so every household spans two PSUs, whichever the
  # stage selects.
  odd <- seq_len(nrow(phase1)) %% 2 == 1
  phase1$psu[odd] <- 999L

  design2 <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |> cluster_by(id) |> draw(n = 1)

  expect_error(
    suppressWarnings(
      execute(design2, phase1, frame[, c("hh", "id", "y")], seed = 2)
    ),
    class = "samplyr_error_phase_key_ambiguous"
  )
})

test_that("phase weights compound exactly once through a multistage phase 2", {
  frame <- data.frame(
    psu = rep(1:10, each = 20), hh = rep(1:100, each = 2),
    id = seq_len(200), y = 1
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 5) |>
    execute(frame, seed = 1)
  expect_equal(unique(phase1$.weight), 2)

  phase2 <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |> draw(n = 1) |>
    execute(phase1, seed = 2)

  # 2 (phase 1) x 12.5 (10 of 50 households wait, per selected psu) x 2
  expect_equal(unique(phase2$.weight_1), 12.5)
  expect_equal(unique(phase2$.weight_2), 2)
  expect_equal(unique(phase2$.weight), 2 * 12.5 * 2)
  expect_equal(unique(phase2$.weight), 50)

  # The internal carrier must not reach the result.
  expect_false("._prev_phase_weight" %in% names(phase2))
})

test_that("continuing a phase keeps its link and does not reapply its weight", {
  frame <- data.frame(
    psu = rep(1:10, each = 20), hh = rep(1:100, each = 2),
    id = seq_len(200), y = 1
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 5) |>
    execute(frame, seed = 1)

  design2 <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |> draw(n = 1)

  partial <- execute(design2, phase1, stages = 1, seed = 2)
  continued <- execute(partial, frame, stages = 2, seed = 3)

  expect_false(is.null(attr(partial, "metadata")$prev_phase))
  expect_false(is.null(attr(continued, "metadata")$prev_phase))
  expect_equal(unique(continued$.weight), 50)
  expect_false("._prev_phase_weight" %in% names(continued))
})

test_that("the two-phase bridge rejects ambiguous, missing and orphan keys", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    psu = rep(1:10, each = 20), hh = rep(1:100, each = 2),
    id = seq_len(200), y = as.numeric(seq_len(200))
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 5) |>
    execute(frame, seed = 1)

  # Ambiguous: the bridge (psu, hh) reaches two phase-1 element rows.
  ambiguous <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |> draw(n = 1) |>
    execute(phase1, seed = 2)
  expect_error(
    as_svydesign(ambiguous),
    class = "samplyr_error_twophase_bridge"
  )

  # No declared identifier on either side: nothing to bridge with.
  unbridged <- sampling_design() |>
    draw(n = 20) |>
    execute(phase1, seed = 2)
  expect_error(
    as_svydesign(unbridged),
    class = "samplyr_error_twophase_bridge"
  )
})

test_that("a mistyped or missing bridge key is reported as a bridge failure", {
  df1 <- data.frame(psu = 1:4, hh = 1:4, .weight = 2)
  df2 <- data.frame(psu = as.character(1:4), hh = 1:4)
  expect_error(
    samplyr:::resolve_phase_bridge("psu", "hh", df1, df2),
    class = "samplyr_error_twophase_bridge"
  )

  df2_na <- data.frame(psu = c(1L, NA, 3L, 4L), hh = 1:4)
  expect_error(
    samplyr:::resolve_phase_bridge("psu", "hh", df1, df2_na),
    class = "samplyr_error_twophase_bridge"
  )

  # A phase-2 key with no phase-1 row is an orphan, even though other keys
  # match: a left join would drop it silently.
  df2_orphan <- data.frame(psu = c(1L, 2L, 99L), hh = c(1L, 2L, 3L))
  expect_error(
    samplyr:::resolve_phase_bridge("psu", "hh", df1, df2_orphan),
    class = "samplyr_error_twophase_bridge"
  )

  # Repeated phase-2 keys are expected: one phase-1 unit, many descendants.
  df2_many <- data.frame(psu = c(1L, 1L, 2L), hh = c(1L, 1L, 2L))
  expect_identical(
    samplyr:::resolve_phase_bridge("psu", "hh", df1, df2_many),
    c("psu", "hh")
  )
})

test_that("two-phase export agrees with a hand-built survey reference", {
  skip_if_not_installed("survey")
  # 10 PSUs, 5 households each, 4 elements each: enough units per stage that
  # survey has no lonely PSU to complain about.
  frame <- data.frame(
    psu = rep(1:10, each = 20),
    hh = rep(1:50, each = 4),
    id = seq_len(200),
    y = as.numeric(seq_len(200))
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 5) |>
    execute(frame, seed = 1)

  # Phase 2 declares its own units; the bridge is (psu, hh, id).
  phase2 <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 3) |>
    add_stage() |> cluster_by(id) |> draw(n = 2) |>
    execute(phase1, seed = 2)

  exported <- as_svydesign(phase2, method = "simple")
  expect_s3_class(exported, "twophase")
  # The default method builds a per-stage covariance and must survive a
  # multistage phase 2, not only the weight-based methods.
  exported_full <- as_svydesign(phase2)
  expect_s3_class(exported_full, "twophase2")

  # The same design, built directly with survey.
  df1 <- as.data.frame(phase1)
  df2 <- as.data.frame(phase2)
  key <- function(d) paste(d$psu, d$hh, d$id)
  df1$.in2 <- key(df1) %in% key(df2)
  # Phase 2 has two clustered stages, so its id formula names both. The
  # finite-population corrections are stated from the known structure rather
  # than read back off the exported object: 10 PSUs in the population; phase 2
  # draws its 3 households from the 25 the phase-1 sample contains (5 PSUs of
  # 5), not 3 per PSU; and 2 of the 4 elements in each household.
  df1$fpc_psu <- 10
  df1$fpc_hh <- 25
  df1$fpc_id <- 4
  reference <- survey::twophase(
    id = list(~psu, ~hh + id),
    data = df1,
    subset = ~.in2,
    fpc = list(~fpc_psu, ~fpc_hh + fpc_id),
    method = "simple"
  )

  reference_full <- survey::twophase(
    id = list(~psu, ~hh + id),
    data = df1,
    subset = ~.in2,
    fpc = list(~fpc_psu, ~fpc_hh + fpc_id)
  )

  for (pair in list(
    list(got = exported, want = reference, label = "simple"),
    list(got = exported_full, want = reference_full, label = "full")
  )) {
    got <- survey::svymean(~y, pair$got)
    want <- survey::svymean(~y, pair$want)
    expect_equal(as.numeric(coef(got)), as.numeric(coef(want)),
                 tolerance = 1e-8, info = pair$label)
    expect_equal(as.numeric(survey::SE(got)), as.numeric(survey::SE(want)),
                 tolerance = 1e-8, info = pair$label)
    expect_true(is.finite(as.numeric(survey::SE(got))), info = pair$label)
  }
})

test_that("a legacy mid-stage element sample is still refused at export", {
  skip_if_not_installed("survey")
  # This shape can no longer be executed, so it is fabricated the way an
  # object saved by an older version would arrive. The export guard exists
  # for exactly those objects.
  frame <- data.frame(
    psu = rep(1:10, each = 20), id = seq_len(200), y = as.numeric(seq_len(200))
  )
  design <- sampling_design() |>
    add_stage() |> draw(n = 100) |>
    add_stage() |> cluster_by(psu) |> draw(n = 4)

  data <- frame[seq_len(40), ]
  data$.weight <- 5
  data$.weight_1 <- 2
  data$.weight_2 <- 2.5
  data$.fpc_1 <- 200
  data$.fpc_2 <- 10
  data$.sample_id <- seq_len(40)
  data$.stage <- 2L

  legacy <- samplyr:::new_tbl_sample(
    data = data,
    design = design,
    stages_executed = 1:2,
    seed = 5,
    metadata = list(n_selected = 40L)
  )

  expect_error(
    as_svydesign(legacy),
    class = "samplyr_error_survey_midstage_element"
  )
})

test_that("replicated multiphase records no digest, as documented", {
  # Not a defect to fix here: merging replicate-conditional manifests would
  # overstate provenance, since each replicate has its own population.
  frame <- data.frame(psu = rep(1:10, each = 4), id = seq_len(40), y = 1)
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 3) |>
    execute(frame, seed = 1, reps = 2)

  for (mode in c("summary", "full")) {
    phase2 <- sampling_design() |>
      cluster_by(psu) |> draw(n = 2) |>
      execute(phase1, seed = 2, frame_digest = mode)
    expect_null(get_frame_digest(phase2), info = mode)
  }
})

test_that("every stage variable family is preflighted before any draw", {
  # No seed: nothing restores the stream, so an advance is observable. A
  # column that is simply absent is knowable before sampling, whichever
  # family it belongs to.
  frame <- data.frame(
    psu = rep(1:8, each = 5), id = seq_len(40),
    x = as.numeric(seq_len(40)), lon = 1, lat = 2,
    grp = rep(c("a", "b"), 20), y = 1
  )

  designs <- list(
    aux = sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 4) |>
      add_stage() |> draw(n = 2, method = "cube", aux = absent_aux),
    bounds = sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 4) |>
      add_stage() |> draw(n = 2, method = "cube", aux = c(x, bound(absent_b))),
    spread = sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 4) |>
      add_stage() |> draw(n = 2, method = "lpm2", spread = c(lon, absent_s)),
    control = sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 4) |>
      add_stage() |> draw(n = 2, method = "systematic", control = absent_c)
  )

  for (family in names(designs)) {
    set.seed(11)
    before <- .Random.seed
    expect_error(
      execute(designs[[family]], frame),
      class = "samplyr_error_frame_missing_vars",
      info = family
    )
    expect_identical(.Random.seed, before, info = family)
  }
})

test_that("a previous-phase frame is preflighted on its stripped schema", {
  # A tbl_sample frame is not exempt from the preflight: what matters is the
  # schema that survives once its generated columns are removed.
  frame <- data.frame(
    psu = rep(1:8, each = 5), id = seq_len(40), x = as.numeric(seq_len(40))
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 4) |>
    execute(frame, seed = 1)

  design2 <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> draw(n = 2, method = "cube", aux = absent_aux)

  set.seed(12)
  before <- .Random.seed
  expect_error(
    execute(design2, phase1),
    class = "samplyr_error_frame_missing_vars"
  )
  expect_identical(.Random.seed, before)
})

test_that("a phase key the previous phase no longer carries is refused", {
  frame <- data.frame(
    psu = rep(1:8, each = 5), hh = rep(1:20, each = 2), id = seq_len(40), y = 1
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 4) |>
    execute(frame, seed = 1)
  phase1$psu <- NULL

  design2 <- sampling_design() |>
    cluster_by(hh) |> draw(n = 3)

  set.seed(13)
  before <- .Random.seed
  expect_error(
    suppressWarnings(execute(design2, phase1)),
    class = "samplyr_error_phase_link_missing"
  )
  expect_identical(.Random.seed, before)
})

test_that("an ambiguous replicate is caught before any replicate draws", {
  # Replicate 1 is fine and replicate 2 is not. Without prevalidation the
  # first would draw before the second failed. No seed, so the advance would
  # be observable.
  # Households nest inside PSUs, so replicate 1 is well formed and only the
  # corruption below makes replicate 2 ambiguous.
  frame <- data.frame(
    psu = rep(1:8, each = 6), hh = rep(1:24, each = 2), id = seq_len(48), y = 1
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 4) |>
    execute(frame, seed = 1, reps = 2)

  # Break the hierarchy in the second replicate only.
  rep2 <- phase1$.replicate == 2
  phase1$psu[rep2] <- ifelse(seq_len(sum(rep2)) %% 2 == 1, 999L,
                             phase1$psu[rep2])

  design2 <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 3) |>
    add_stage() |> draw(n = 1)

  set.seed(14)
  before <- .Random.seed
  expect_error(
    suppressWarnings(execute(design2, phase1)),
    class = "samplyr_error_phase_key_ambiguous"
  )
  expect_identical(.Random.seed, before)
})

## P6. Downstream frame consumers

test_that("validate_frame accepts an ordered list of stage registers", {
  design <- mf_design()
  expect_true(
    validate_frame(design, list(mf_schools(), mf_classes(), mf_students()))
  )

  # Names are diagnostics; position is what maps a register to a stage.
  expect_true(validate_frame(design, list(
    schools = mf_schools(), classes = mf_classes(), students = mf_students()
  )))

  expect_error(
    validate_frame(design, list(mf_schools(), mf_classes())),
    class = "samplyr_error_frame_count"
  )
})

test_that("a finer-grained register with repeated cluster keys validates", {
  # class_id is local: C1 exists under every school. Grouping on the local
  # identifier alone would compare classes that are not the same class.
  design <- mf_design()
  expect_true(
    validate_frame(design, list(mf_schools(), mf_students(), mf_students()))
  )
})

test_that("validate_frame rejects cluster-level variables that vary", {
  classes <- mf_classes()
  classes <- rbind(classes, classes)
  classes$size <- c(seq_len(8), rev(seq_len(8)))

  design <- sampling_design() |>
    add_stage("Schools") |>
      cluster_by(school_id) |>
      draw(n = 2, method = "pps_brewer", mos = enrollment) |>
    add_stage("Classes") |>
      cluster_by(class_id) |>
      draw(n = 1, method = "pps_brewer", mos = size)

  expect_error(
    validate_frame(design, list(mf_schools(), classes)),
    class = "samplyr_error_frame_cluster_invariant"
  )

  # One value per (school, class) is accepted; the same values keyed on
  # class_id alone would not be.
  classes$size <- rep(seq_len(8), times = 2)
  expect_true(validate_frame(design, list(mf_schools(), classes)))
})

test_that("validate_frame rejects ancestry it could not join on", {
  classes <- mf_classes()
  classes$school_id <- seq_len(nrow(classes))
  expect_error(
    validate_frame(mf_design(), list(mf_schools(), classes, mf_students())),
    class = "samplyr_error_frame_key_type"
  )
})

test_that("validate_frame rejects ancestry that names no parent", {
  classes <- mf_classes()
  classes$school_id[1] <- NA
  expect_error(
    validate_frame(mf_design(), list(mf_schools(), classes, mf_students())),
    class = "samplyr_error_frame_ancestry_na"
  )
})

test_that("an uncovered candidate parent is an error, not a warning", {
  # Seed 2 never selects S4, so execute() draws a complete sample and only
  # warns. Explicit validation judges the registers, not one realization.
  design <- mf_design()
  registers <- list(mf_schools(), mf_classes_without_s4(), mf_students())

  expect_error(
    validate_frame(design, registers),
    class = "samplyr_error_frame_incomplete_register"
  )
  expect_warning(
    execute(design, mf_schools(), mf_classes_without_s4(), mf_students(),
            seed = 2),
    class = "samplyr_warning_frame_incomplete_register"
  )

  # The realized-parent failure keeps its own class.
  expect_error(
    suppressWarnings(
      execute(design, mf_schools(), mf_classes_without_s4(), mf_students(),
              seed = 1)
    ),
    class = "samplyr_error_frame_missing_parent"
  )
})

test_that("a partial sample preflights the register that would continue it", {
  design <- mf_design()
  stage1 <- execute(design, mf_schools(), stages = 1, seed = 2)

  expect_true(validate_frame(stage1, mf_classes(), stages = 2))

  # `stages` is required for the same reason execute() requires it: two
  # stages remain, and one frame cannot say whether it is the stage 2
  # register or a hierarchy covering both.
  expect_error(
    validate_frame(stage1, mf_classes()),
    class = "samplyr_error_ambiguous_continuation"
  )
  expect_error(
    execute(stage1, mf_classes(), seed = 3),
    class = "samplyr_error_ambiguous_continuation"
  )

  # Scoped to stage 2, stage 1's own strata are not required of a class
  # register.
  expect_false("school_type" %in% names(mf_classes()))

  no_ancestry <- mf_classes()
  no_ancestry$school_id <- NULL
  expect_error(
    validate_frame(stage1, no_ancestry, stages = 2),
    class = "samplyr_error_frame_missing_ancestry"
  )

  mistyped <- mf_classes()
  mistyped$school_id <- seq_len(nrow(mistyped))
  expect_error(
    validate_frame(stage1, mistyped, stages = 2),
    class = "samplyr_error_frame_key_type"
  )

  na_parent <- mf_classes()
  na_parent$school_id[1] <- NA
  expect_error(
    validate_frame(stage1, na_parent, stages = 2),
    class = "samplyr_error_frame_ancestry_na"
  )

  selected <- unique(stage1$school_id)
  orphaned <- mf_classes()
  orphaned <- orphaned[orphaned$school_id != selected[1], , drop = FALSE]
  expect_error(
    validate_frame(stage1, orphaned, stages = 2),
    class = "samplyr_error_frame_missing_parent"
  )
})

test_that("a partial sample is not compared against the recorded frame", {
  # The digest describes the school register. A class register is a different
  # table by design, so reporting drift would fire on every correct call.
  stage1 <- execute(mf_design(), mf_schools(), stages = 1, seed = 2)
  expect_no_message(validate_frame(stage1, mf_classes(), stages = 2))

  # A completed sample still reports drift.
  full <- execute(mf_design(), mf_hierarchy(), seed = 2)
  expect_message(
    validate_frame(full, mf_hierarchy()[-1, , drop = FALSE]),
    class = "samplyr_message_digest_drift"
  )
})

test_that("a continuation preflights one register per remaining stage", {
  # A continuation is not limited to one stage batch, only to a contiguous
  # one, so validation preflights the same schedule execution accepts.
  stage1 <- execute(mf_design(), mf_schools(), stages = 1, seed = 2)
  expect_true(validate_frame(stage1, list(mf_classes(), mf_students())))
  expect_s3_class(
    execute(stage1, mf_classes(), mf_students(), stages = 2:3, seed = 3),
    "tbl_sample"
  )

  # Each register is judged against the stage it belongs to.
  broken <- mf_students()
  broken$class_id <- NULL
  expect_error(
    validate_frame(stage1, list(mf_classes(), broken)),
    class = "samplyr_error_frame_missing_ancestry"
  )

  # One frame with two stages left cannot say which it is, in either verb.
  expect_error(
    validate_frame(stage1, list(mf_classes())),
    class = "samplyr_error_ambiguous_continuation"
  )
})

test_that("frame-backed and digest-backed joint matrices agree", {
  schools <- mf_schools()
  classes <- mf_classes()
  classes$size <- c(10, 20, 30, 40, 50, 60, 70, 80)
  students <- mf_students()

  design <- sampling_design() |>
    add_stage("Schools") |>
      cluster_by(school_id) |>
      draw(n = 2, method = "pps_brewer", mos = enrollment) |>
    add_stage("Classes") |>
      cluster_by(class_id) |>
      draw(n = 1, method = "pps_brewer", mos = size) |>
    add_stage("Students") |>
      draw(n = 2)

  sample <- execute(design, schools, classes, students, seed = 7,
                    frame_digest = "full")

  from_digest <- joint_expectation(sample)
  from_registers <- joint_expectation(sample, list(schools, classes, students))

  # PPS at an upper and a lower stage, both reconstructed from the registers.
  expect_equal(from_registers[[1]], from_digest[[1]])
  expect_equal(from_registers[[2]], from_digest[[2]])
  expect_identical(dim(from_registers[[1]]), c(2L, 2L))
})

test_that("one register cannot stand in for several", {
  schools <- mf_schools()
  classes <- mf_classes()
  students <- mf_students()
  sample <- execute(mf_design(), schools, classes, students, seed = 7)

  expect_error(
    joint_expectation(sample, classes),
    class = "samplyr_error_frame_count"
  )
  expect_error(
    joint_expectation(sample, list(schools, classes)),
    class = "samplyr_error_frame_count"
  )
})

test_that("the receipt records the frame mode and the stage mapping", {
  schools <- mf_schools()
  classes <- mf_classes()
  students <- mf_students()

  separate <- execute(mf_design(), schools, classes, students, seed = 7)
  record <- attr(separate, "metadata")$frame_schedule
  expect_identical(record$mode, "separate_frames")
  expect_identical(record$n_supplied, 3L)
  expect_identical(record$stage_frame_index, 1:3)

  shared <- execute(mf_design(), mf_hierarchy(), seed = 7)
  shared_record <- attr(shared, "metadata")$frame_schedule
  expect_identical(shared_record$mode, "shared_frame")
  expect_identical(shared_record$n_supplied, 1L)
  expect_identical(shared_record$stage_frame_index, rep(1L, 3))
})

test_that("a chained receipt records its final call and stays unreplayable", {
  schools <- mf_schools()
  stage1 <- execute(mf_design(), schools, stages = 1, seed = 2)
  continued <- execute(stage1, mf_classes(), mf_students(), stages = 2:3,
                       seed = 3)

  record <- attr(continued, "metadata")$frame_schedule
  expect_identical(record$mode, "separate_frames")
  expect_identical(record$stages, 2:3)

  # The mapping describes the final call. Replayability is decided by
  # `chained`, which the mapping must not contradict.
  path <- withr::local_tempfile(fileext = ".json")
  expect_warning(
    write_design(continued, path, frame = list(mf_classes(), mf_students())),
    "more than one"
  )
  expect_error(
    replay_design(read_design(path), list(mf_classes(), mf_students())),
    class = "samplyr_error_receipt_chained"
  )
})

test_that("registers round trip through a design file and replay", {
  schools <- mf_schools()
  classes <- mf_classes()
  students <- mf_students()
  sample <- execute(mf_design(), schools, classes, students, seed = 7)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = list(schools, classes, students))

  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  # A reader that does not know these fields would replay against one frame.
  expect_identical(payload$format_version, 3L)
  expect_identical(payload$execution$frames$mode, "separate_frames")
  expect_identical(payload$execution$frames$count, 3L)
  expect_length(payload$frame$fingerprints, 3L)

  restored <- read_design(path)
  expect_identical(attr(restored, "execution")$frames$n_supplied, 3L)
  expect_identical(
    attr(restored, "execution")$frames$stage_frame_index, 1:3
  )

  # Every file declares the current format version, including one
  # written without fingerprints.
  bare <- withr::local_tempfile(fileext = ".json")
  expect_warning(write_design(sample, bare), "without a frame fingerprint")
  expect_identical(
    jsonlite::fromJSON(bare, simplifyVector = FALSE)$format_version, 3L
  )

  replayed <- replay_design(restored, list(schools, classes, students))
  expect_identical(mf_keys(replayed), mf_keys(sample))
  expect_identical(replayed$.weight, sample$.weight)
})

test_that("frame labels survive as diagnostics only", {
  schools <- mf_schools()
  classes <- mf_classes()
  students <- mf_students()
  sample <- execute(mf_design(), schools, classes, students, seed = 7)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = list(schools, classes, students))
  restored <- read_design(path)

  recorded <- vapply(
    attr(restored, "frame_info")$fingerprints,
    function(f) f$name, character(1)
  )
  expect_identical(recorded, c("schools", "classes", "students"))

  # A label names the position in a message and nothing else: the same frames
  # under different names replay identically.
  path2 <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path2, frame = list(a = schools, b = classes,
                                           c = students))
  expect_identical(
    mf_keys(replay_design(read_design(path2),
                          list(schools, classes, students))),
    mf_keys(sample)
  )
})

test_that("a per-frame mismatch names the frame that differs", {
  schools <- mf_schools()
  classes <- mf_classes()
  students <- mf_students()
  sample <- execute(mf_design(), schools, classes, students, seed = 7)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = list(schools, classes, students))
  restored <- read_design(path)

  moved <- classes
  moved$class_id[1] <- "CX"
  err <- expect_error(
    replay_design(restored, list(schools, moved, students)),
    class = "samplyr_error_replay_frame_mismatch"
  )
  expect_match(cli::ansi_strip(conditionMessage(err)), 'frame 2 "classes"')

  expect_error(
    replay_design(restored, schools),
    class = "samplyr_error_replay_frame_count"
  )
})

test_that("a receipt without frame-mode fields is read as one frame", {
  # A file with the frame mapping stripped out.
  design <- sampling_design() |> stratify_by(school_type) |> draw(n = 2)
  sample <- execute(design, mf_schools(), seed = 3)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = mf_schools())

  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(payload$format_version, 3L)
  payload$execution$frames <- NULL
  writeLines(jsonlite::toJSON(payload, auto_unbox = TRUE, digits = NA,
                              null = "null"), path)

  restored <- read_design(path)
  expect_null(attr(restored, "execution")$frames)
  expect_identical(
    replay_design(restored, mf_schools())$school_id, sample$school_id
  )
})

test_that("a sample carrying no frame record is read as one frame", {
  sample <- execute(mf_design(), mf_hierarchy(), seed = 7)
  metadata <- attr(sample, "metadata")
  metadata$frame_schedule <- NULL
  attr(sample, "metadata") <- metadata

  record <- get_frame_schedule(sample)
  expect_identical(record$mode, "shared_frame")
  expect_identical(record$n_supplied, 1L)
  expect_identical(record$stage_frame_index, rep(1L, 3))
  expect_identical(mf_keys(replay_design(sample, mf_hierarchy())),
                   mf_keys(sample))
})

## P6b. Validation models the transitions execution performs

test_that("a carried stratum absent from a lower register is not a defect", {
  # Stage 2 stratifies on a variable only the school register holds. Execution
  # carries it across the transition, so validation must judge stage 2 against
  # the frame it will actually see, not the raw register.
  design <- sampling_design() |>
    add_stage("Schools") |>
      stratify_by(school_type) |> cluster_by(school_id) |> draw(n = 1) |>
    add_stage("Classes") |>
      stratify_by(school_type) |> cluster_by(class_id) |> draw(n = 1) |>
    add_stage("Students") |>
      draw(n = 2)

  expect_false("school_type" %in% names(mf_classes()))
  expect_s3_class(
    execute(design, mf_schools(), mf_classes(), mf_students(), seed = 7),
    "tbl_sample"
  )
  expect_true(
    validate_frame(design, list(mf_schools(), mf_classes(), mf_students()))
  )

  # The same holds for a continuation, whose next stage reads its own register.
  stage1 <- execute(design, mf_schools(), stages = 1, seed = 7)
  expect_true(validate_frame(stage1, mf_classes(), stages = 2))
})

test_that("a lower register that contradicts a carried variable is refused", {
  classes <- mf_classes()
  classes$school_type <- "Wrong"

  expect_error(
    validate_frame(mf_design(), list(mf_schools(), classes, mf_students())),
    class = "samplyr_error_frame_parent_conflict"
  )
  expect_error(
    execute(mf_design(), mf_schools(), classes, mf_students(), seed = 7),
    class = "samplyr_error_frame_parent_conflict"
  )
})

test_that("an ambiguous phase key is refused by validation too", {
  frame <- data.frame(
    psu = rep(1:10, each = 20), hh = rep(1:100, each = 2),
    id = seq_len(200), y = 1
  )
  phase1 <- sampling_design() |>
    cluster_by(psu) |> draw(n = 5) |>
    execute(frame, seed = 1)
  phase1$psu[seq_len(nrow(phase1)) %% 2 == 1] <- 999L

  design2 <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 4) |>
    add_stage() |> cluster_by(id) |> draw(n = 1)
  lower <- frame[, c("hh", "id", "y")]

  expect_error(
    suppressWarnings(validate_frame(design2, list(phase1, lower))),
    class = "samplyr_error_phase_key_ambiguous"
  )
  # And through the single-frame spelling, where the phase is the frame.
  expect_error(
    suppressWarnings(validate_frame(design2, phase1)),
    class = "samplyr_error_phase_key_ambiguous"
  )
  expect_error(
    suppressWarnings(execute(design2, phase1, lower, seed = 2)),
    class = "samplyr_error_phase_key_ambiguous"
  )
})

test_that("a continuation reports gaps only among units it can reach", {
  # Candidacy for a continuation is bounded by what the earlier call selected.
  # A register covering exactly those units is complete, whatever the rest of
  # the population looks like.
  stage1 <- execute(mf_design(), mf_schools(), stages = 1, seed = 2)
  selected <- unique(stage1$school_id)
  students <- mf_students()
  reachable <- students[students$school_id %in% selected, , drop = FALSE]

  expect_lt(nrow(reachable), nrow(students))
  expect_no_warning(
    execute(stage1, mf_classes(), reachable, stages = 2:3, seed = 3)
  )
  expect_true(validate_frame(stage1, list(mf_classes(), reachable)))

  # A unit that is reachable but not selected is still reported: seed 3 takes
  # S1/C1 and S3/C2, so dropping S1/C2 leaves a candidate gap that this
  # realization never needs.
  short <- reachable[
    !(reachable$school_id == "S1" & reachable$class_id == "C2"), ,
    drop = FALSE
  ]
  expect_warning(
    execute(stage1, mf_classes(), short, stages = 2:3, seed = 3),
    class = "samplyr_warning_frame_incomplete_register"
  )
  expect_error(
    validate_frame(stage1, list(mf_classes(), short)),
    class = "samplyr_error_frame_incomplete_register"
  )
})

## P6b. execute() accepts the frames as one list

test_that("a single unnamed list is the same call as separate frames", {
  design <- mf_design()
  dots <- execute(design, mf_schools(), mf_classes(), mf_students(), seed = 7)
  listed <- execute(design, list(mf_schools(), mf_classes(), mf_students()),
                    seed = 7)

  expect_identical(mf_keys(dots), mf_keys(listed))
  expect_identical(dots$.weight, listed$.weight)

  # The recorded mapping cannot tell the spellings apart either.
  expect_identical(
    attr(dots, "metadata")$frame_schedule,
    attr(listed, "metadata")$frame_schedule
  )

  # One hierarchy in a one-element list is still one frame.
  expect_identical(
    mf_keys(execute(design, list(mf_hierarchy()), seed = 7)),
    mf_keys(execute(design, mf_hierarchy(), seed = 7))
  )
})

test_that("the list form works for a continuation", {
  design <- mf_design()
  stage1 <- execute(design, mf_schools(), stages = 1, seed = 2)
  dots <- execute(stage1, mf_classes(), mf_students(), stages = 2:3, seed = 3)
  listed <- execute(stage1, list(mf_classes(), mf_students()), stages = 2:3,
                    seed = 3)
  expect_identical(mf_keys(dots), mf_keys(listed))
})

test_that("inner names label frames and outer names stay arguments", {
  design <- mf_design()
  labelled <- execute(
    design,
    list(schools = mf_schools(), classes = mf_classes(),
         students = mf_students()),
    seed = 7
  )
  expect_identical(
    unlist(attr(labelled, "metadata")$frame_schedule$labels),
    c("schools", "classes", "students")
  )

  # A misspelled reserved argument is still an argument, not a frame label.
  expect_error(
    execute(design, mf_hierarchy(), seedd = 1),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    execute(design, mf_hierarchy(), stage = 1),
    class = "samplyr_error_unknown_argument"
  )
})

test_that("mixing a list with separate frames is refused", {
  design <- mf_design()
  expect_error(
    execute(design, list(mf_schools(), mf_classes()), mf_students(), seed = 7),
    class = "samplyr_error_frame_count"
  )
  expect_error(
    execute(design, mf_schools(), list(mf_classes(), mf_students()), seed = 7),
    class = "samplyr_error_frame_count"
  )
  expect_error(execute(design, list(), seed = 7), "At least one data frame")
})

## P6b. The list form reaches every preflight the dots form does

test_that("the two-phase preflight runs for the list form too", {
  # Returning early from the list branch skipped it entirely, so a design
  # that cannot be linked validated silently when its phase arrived in a list.
  frame <- data.frame(psu = rep(1:10, each = 5), id = 1:50, x = 1)

  non_unique <- sampling_design() |>
    cluster_by(psu) |> draw(n = 4) |>
    execute(frame, seed = 2)
  design <- sampling_design() |> cluster_by(psu) |> draw(n = 2)
  expect_warning(
    validate_frame(design, non_unique),
    class = "samplyr_warning_phase_linkage"
  )
  expect_warning(
    validate_frame(design, list(non_unique)),
    class = "samplyr_warning_phase_linkage"
  )

  elements <- data.frame(id = 1:100, x = 1)
  no_bridge <- sampling_design() |> draw(n = 40) |> execute(elements, seed = 1)
  unlinkable <- sampling_design() |> draw(n = 10)
  expect_warning(
    validate_frame(unlinkable, no_bridge),
    class = "samplyr_warning_phase_linkage"
  )
  expect_warning(
    validate_frame(unlinkable, list(no_bridge)),
    class = "samplyr_warning_phase_linkage"
  )

  # And the linkable design stays quiet in both spellings.
  linkable_phase <- sampling_design() |>
    cluster_by(id) |> draw(n = 40) |>
    execute(elements, seed = 1)
  linkable <- sampling_design() |> cluster_by(id) |> draw(n = 10)
  expect_no_warning(validate_frame(linkable, linkable_phase))
  expect_no_warning(validate_frame(linkable, list(linkable_phase)))
})

test_that("a name inside the frame list is a label, not an argument", {
  design <- sampling_design() |> draw(n = 1)

  err <- expect_error(
    execute(design, list(ok = data.frame(x = 1), bad = 42)),
    class = "samplyr_error_frame_not_data_frame"
  )
  expect_match(cli::ansi_strip(conditionMessage(err)), 'frame 2 "bad"')

  expect_error(
    execute(design, list(data.frame(x = 1), 42)),
    class = "samplyr_error_frame_not_data_frame"
  )

  # On the call's own arguments a name can still be a misspelled argument.
  expect_error(
    execute(design, data.frame(x = 1), bad = 42),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    execute(design, data.frame(x = 1), seedd = 42),
    class = "samplyr_error_unknown_argument"
  )
})

## P6b. Stage arguments are named exactly, never prefix-matched

test_that("the singular stage is refused and names the plural", {
  sample <- execute(mf_design(), mf_hierarchy(), seed = 7,
                    frame_digest = "full")
  design <- mf_design()
  frame <- mf_hierarchy()

  for (call in list(
    function() validate_frame(design, frame, stage = 1),
    function() joint_expectation(sample, stage = 1),
    function() frame_summary(sample, stage = 1),
    function() execute(design, frame, stage = 1)
  )) {
    err <- expect_error(call(), class = "samplyr_error_unknown_argument")
    expect_match(cli::ansi_strip(conditionMessage(err)), "Did you mean .stages")
  }

  # Partial matching would have accepted any prefix, teaching nothing.
  expect_error(
    frame_summary(sample, st = 1),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    joint_expectation(sample, stag = 1),
    class = "samplyr_error_unknown_argument"
  )
})

test_that("arguments after the dots must be named", {
  sample <- execute(mf_design(), mf_hierarchy(), seed = 7,
                    frame_digest = "full")
  design <- mf_design()
  frame <- mf_hierarchy()

  expect_error(
    validate_frame(design, frame, 1),
    class = "samplyr_error_unnamed_argument"
  )
  expect_error(
    joint_expectation(sample, frame, 1),
    class = "samplyr_error_unnamed_argument"
  )
  expect_error(
    frame_summary(sample, frame, 1),
    class = "samplyr_error_unnamed_argument"
  )
  # `frame_summary()` takes `frame` second, like the other two, so a bare
  # positional value is read as a frame and reported as one.
  expect_error(
    frame_summary(sample, 1),
    class = "samplyr_error_frame_not_data_frame"
  )

  # Named, they are the same calls as before.
  expect_true(validate_frame(design, frame, stages = 1))
  expect_identical(nrow(frame_summary(sample, stages = 1)), 1L)
})

test_that("a stray argument is diagnosed without being evaluated", {
  # Forcing the dots to read their names lets a stray argument's expression
  # fail first, replacing the diagnosis with whatever that expression did.
  sample <- execute(mf_design(), mf_hierarchy(), seed = 7,
                    frame_digest = "full")
  design <- mf_design()
  frame <- mf_hierarchy()

  for (call in list(
    function() validate_frame(design, frame, stage = stop("forced")),
    function() joint_expectation(sample, stage = stop("forced")),
    function() frame_summary(sample, stage = stop("forced")),
    function() execute(design, frame, stage = stop("forced"))
  )) {
    err <- expect_error(call(), class = "samplyr_error_unknown_argument")
    expect_match(cli::ansi_strip(conditionMessage(err)), "Did you mean .stages")
  }

  # The helper's own formals are not reserved names a caller can collide with.
  expect_error(
    validate_frame(design, frame, candidates = 5),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    frame_summary(sample, call = 5),
    class = "samplyr_error_unknown_argument"
  )
})

## P0. Fingerprint extraction is exact, and a shape mismatch is a difference

test_that("a multi-frame file does not answer a request for one fingerprint", {
  # `fingerprint` is a prefix of `fingerprints`, so `$` on a file that
  # recorded several returns all of them. The comparison then read the list
  # as one fingerprint and reported every column of the frame as new.
  schools <- mf_schools()
  classes <- mf_classes()
  students <- mf_students()
  sample <- execute(mf_design(), schools, classes, students, seed = 7)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = list(schools, classes, students))
  restored <- read_design(path)

  info <- attr(restored, "frame_info")
  expect_null(info[["fingerprint"]])
  expect_length(info[["fingerprints"]], 3L)

  # The registers as recorded compare clean.
  expect_no_message(
    validate_frame(restored, list(schools, classes, students))
  )

  # One register against a three-register file is a shape difference, not a
  # frame full of new columns.
  msg <- cli::ansi_strip(paste(
    testthat::capture_messages(validate_frame(restored, schools, stages = 1)),
    collapse = ""
  ))
  expect_match(msg, "1 frame supplied; 3 recorded")
  expect_no_match(msg, "new column")
})

test_that("a fingerprint count that cannot match is reported, not skipped", {
  # Silently skipping let a replay certify a sample it never checked.
  # samplyr can no longer write a file whose fingerprint count contradicts
  # its receipt, so the artifacts are doctored here. The replay check exists
  # for exactly that: files written by an earlier or foreign implementation.
  schools <- mf_schools()
  classes <- mf_classes()
  students <- mf_students()
  redo <- function(json, edit) {
    payload <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    jsonlite::toJSON(edit(payload), auto_unbox = TRUE, null = "null")
  }

  # Three frames recorded as one, replayed with the three real registers.
  # No digest: a jsonlite round-trip cannot preserve it exactly, and this
  # test is about fingerprint counts.
  multi <- execute(mf_design(), schools, classes, students, seed = 7,
                   frame_digest = "none")
  doctored <- redo(
    design_json(multi, frame = list(schools, classes, students)),
    function(p) {
      p$frame$fingerprint <- p$frame$fingerprints[[1]]
      p$frame$fingerprints <- NULL
      p
    }
  )
  err <- expect_error(
    replay_design(read_design(doctored), list(schools, classes, students)),
    class = "samplyr_error_replay_frame_mismatch"
  )
  expect_match(cli::ansi_strip(conditionMessage(err)),
               "3 frames supplied; 1 recorded")

  # And the mirror case: one frame recorded as several.
  one <- execute(mf_design(), mf_hierarchy(), seed = 7,
                 frame_digest = "none")
  doctored2 <- redo(
    design_json(one, frame = mf_hierarchy()),
    function(p) {
      p$frame$fingerprints <- list(p$frame$fingerprint, p$frame$fingerprint)
      p$frame$fingerprint <- NULL
      p
    }
  )
  err2 <- expect_error(
    replay_design(read_design(doctored2), mf_hierarchy()),
    class = "samplyr_error_replay_frame_mismatch"
  )
  expect_match(cli::ansi_strip(conditionMessage(err2)),
               "1 frame supplied; 2 recorded")

  # The write side now refuses to create either artifact.
  path <- withr::local_tempfile(fileext = ".json")
  expect_error(
    write_design(multi, path, frame = schools),
    class = "samplyr_error_serialization_frame_count"
  )
  expect_error(
    write_design(one, path, frame = list(schools, classes)),
    class = "samplyr_error_serialization_frame_count"
  )
})

test_that("recorded fingerprints normalize to one shape", {
  schools <- mf_schools()
  one <- list(fingerprint = list(nrow = 4L))
  many <- list(fingerprints = list(list(nrow = 4L), list(nrow = 8L)))

  expect_length(samplyr:::recorded_fingerprints(one), 1L)
  expect_length(samplyr:::recorded_fingerprints(many), 2L)
  expect_null(samplyr:::recorded_fingerprints(list()))
  expect_null(samplyr:::recorded_fingerprints(NULL))

  # A file with no fingerprint at all still compares clean rather than
  # inventing a difference.
  expect_identical(
    samplyr:::fingerprint_diffs(list(), list(schools)), character(0)
  )
})

test_that("saving without a frame warns only when none was ever recorded", {
  schools <- mf_schools()
  classes <- mf_classes()
  students <- mf_students()
  sample <- execute(mf_design(), schools, classes, students, seed = 7)

  # The warning reads the portable record, where the plural spelling lives.
  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = list(schools, classes, students))
  restored <- read_design(path)

  path2 <- withr::local_tempfile(fileext = ".json")
  expect_no_warning(write_design(restored, path2))

  bare <- withr::local_tempfile(fileext = ".json")
  expect_warning(write_design(sample, bare), "without a frame fingerprint")
})

test_that("an optional field is never shadowed by a longer sibling", {
  # `$` on a list is prefix matching: an absent name resolves to a sibling
  # when exactly one sibling starts with it. That silently returned the wrong
  # object once, when `frame_info$fingerprint` met a file recording
  # `fingerprints` and reported an unchanged frame as entirely new columns.
  #
  # A field that is always present is safe, because an exact name beats a
  # partial one. So is a field explicitly set to `NULL`, which keeps its name
  # and is therefore still matched exactly. The hazard is narrower: a name
  # genuinely absent from the list in some shape, with exactly one longer
  # sibling. Every pair recorded below is read with `[[` at every site. A new
  # pair failing this test is a decision to make, not a nuisance to silence:
  # rename the field, or read it exactly.
  schools <- mf_schools()
  classes <- mf_classes()
  students <- mf_students()

  multi <- execute(mf_design(), schools, classes, students, seed = 7,
                   frame_digest = "full")
  one <- execute(mf_design(), mf_hierarchy(), seed = 7)
  replicated <- execute(mf_design(), mf_hierarchy(), seed = 7, reps = 2)
  partial <- execute(mf_design(), schools, stages = 1, seed = 2)
  continued <- execute(partial, classes, students, stages = 2:3, seed = 3)

  path_multi <- withr::local_tempfile(fileext = ".json")
  write_design(multi, path_multi, frame = list(schools, classes, students))
  path_one <- withr::local_tempfile(fileext = ".json")
  write_design(one, path_one, frame = mf_hierarchy())
  restored_multi <- read_design(path_multi)
  restored_one <- read_design(path_one)

  # Several shapes of each structure, so a field absent from any one of them
  # counts as optional.
  variants <- list(
    metadata = list(
      attr(multi, "metadata"), attr(one, "metadata"),
      attr(replicated, "metadata"), attr(continued, "metadata")
    ),
    receipt = list(
      samplyr:::encode_execution(multi),
      samplyr:::encode_execution(replicated),
      samplyr:::encode_execution(continued)
    ),
    frame_info = list(
      attr(restored_multi, "frame_info"), attr(restored_one, "frame_info")
    ),
    portable_frame_info = list(
      attr(restored_multi, "portable_frame_info"),
      attr(restored_one, "portable_frame_info")
    ),
    digest = list(get_frame_digest(multi), get_frame_digest(one)),
    digest_frame = list(
      get_frame_digest(multi)$frames[[1]], get_frame_digest(one)$frames[[1]]
    ),
    digest_stage = list(
      get_frame_digest(multi)$stages[[1]], get_frame_digest(one)$stages[[1]]
    ),
    frame_schedule = list(
      attr(multi, "metadata")$frame_schedule,
      attr(one, "metadata")$frame_schedule
    )
  )

  shadowed <- character(0)
  for (nm in names(variants)) {
    shapes <- Filter(is.list, variants[[nm]])
    every <- unique(unlist(lapply(shapes, names)))
    always <- Reduce(intersect, lapply(shapes, names))
    for (field in setdiff(every, always)) {
      longer <- setdiff(every[startsWith(every, field)], field)
      if (length(longer) > 0) {
        shadowed <- c(shadowed, paste0(nm, ":", field))
      }
    }
  }

  expect_identical(
    sort(shadowed),
    c("frame_info:fingerprint", "portable_frame_info:fingerprint")
  )
})
