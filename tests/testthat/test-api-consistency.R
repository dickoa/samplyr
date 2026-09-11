## API consistency acceptance coverage.
## Every block states a contract that must hold at a public boundary; the
## reproductions in the plan are kept alongside the assertions they came from.

test_that("forwarded dots protect package-owned tail arguments lazily", {
  frame <- data.frame(id = 1:20, y = seq_len(20))
  sample <- sampling_design() |>
    draw(n = 5) |>
    execute(frame, seed = 1)

  for (call in list(
    function() as_svydesign(sample, nes = stop("forced")),
    function() as_svydesign(sample, methodd = stop("forced")),
    function() as_svrepdesign(sample, typ = stop("forced"))
  )) {
    err <- expect_error(call(), class = "samplyr_error_unknown_argument")
    expect_false(grepl("forced", conditionMessage(err), fixed = TRUE))
  }
})

test_that("selection and serialization modifiers are keyword-only", {
  design <- sampling_design()
  frame <- data.frame(id = 1:10)

  expect_error(
    draw(design, 2, NULL, "srswor"),
    class = "samplyr_error_unnamed_argument"
  )
  expect_error(
    design_json(draw(design, n = 2), frame, TRUE),
    class = "samplyr_error_unnamed_argument"
  )
  expect_error(
    write_design(draw(design, n = 2), withr::local_tempfile(), frame, FALSE),
    class = "samplyr_error_unnamed_argument"
  )
  expect_error(
    design_json(draw(design, n = 2), frame, prettyy = stop("forced")),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    write_design(
      draw(design, n = 2), withr::local_tempfile(), frame,
      prettyy = stop("forced")
    ),
    class = "samplyr_error_unknown_argument"
  )

  # The shape arguments stay positional, and write_design() keeps returning
  # its input invisibly after the file side effect.
  expect_s3_class(draw(design, 2), "sampling_design")
  path <- withr::local_tempfile(fileext = ".json")
  expect_invisible(write_design(draw(design, n = 2), path, frame))
  expect_true(file.exists(path))
})

test_that("frame-valued APIs share one typed grammar", {
  design <- sampling_design() |> draw(n = 2)
  frame <- data.frame(id = 1:10, z = 11:20)

  expect_error(
    design_json(design, frame = list()),
    class = "samplyr_error_frame_count"
  )
  expect_error(
    validate_frame(design, list(frame, bad = 1)),
    class = "samplyr_error_frame_not_data_frame"
  )
  expect_error(
    validate_frame(design, list(frame, bad = 1)),
    'frame 2 "bad"'
  )

  duplicated_frame <- frame
  names(duplicated_frame) <- c("id", "id")
  expect_error(
    validate_frame(design, duplicated_frame),
    class = "samplyr_error_frame_duplicate_names"
  )

  reserved_frame <- frame
  names(reserved_frame)[2] <- ".weight"
  expect_error(
    validate_frame(design, reserved_frame),
    class = "samplyr_error_frame_reserved_names"
  )

  sample <- execute(design, frame, seed = 1)
  stripped <- sample
  class(stripped) <- "data.frame"
  expect_error(
    validate_frame(design, stripped),
    class = "samplyr_error_stripped_sample_frame"
  )
})

test_that("validate_frame() cannot approve what execute() refuses", {
  # Every case below was accepted by the direct data-frame branch of
  # validate_frame() while execute() refused it before consuming any RNG.
  frame <- data.frame(id = 1:20, x = 1:20, z = 21:40)

  # Parent identity: a non-final stage with nothing to link the next one to.
  unlinkable <- sampling_design() |>
    add_stage() |>
    draw(n = 5) |>
    add_stage() |>
    draw(n = 2)
  for (spelling in list(frame, list(frame))) {
    expect_error(
      validate_frame(unlinkable, spelling),
      class = "samplyr_error_stage_parent_id"
    )
  }
  expect_error(
    execute(unlinkable, frame, seed = 1),
    class = "samplyr_error_stage_parent_id"
  )

  # Schema: both spellings raise the class execute() raises.
  cases <- list(
    samplyr_error_frame_duplicate_names = stats::setNames(
      frame, c("id", "id", "z")
    ),
    samplyr_error_frame_reserved_names = stats::setNames(
      frame, c("id", ".weight", "z")
    )
  )
  design <- sampling_design() |> draw(n = 2)
  for (class in names(cases)) {
    bad <- cases[[class]]
    expect_error(validate_frame(design, bad), class = class)
    expect_error(validate_frame(design, list(bad)), class = class)
    expect_error(execute(design, bad, seed = 1), class = class)
  }
})

test_that("both frame spellings give the same diagnostic, not just the same verdict", {
  frame <- data.frame(id = 1:20)
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(id) |>
    draw(n = 5) |>
    add_stage() |>
    draw(n = 2, method = "cube", aux = c(x))

  direct <- expect_error(
    validate_frame(design, frame),
    class = "samplyr_error_frame_missing_vars"
  )
  listed <- expect_error(
    validate_frame(design, list(frame)),
    class = "samplyr_error_frame_missing_vars"
  )
  expect_identical(
    cli::ansi_strip(conditionMessage(direct)),
    cli::ansi_strip(conditionMessage(listed))
  )
  # The column's role survives the shared path: knowing `x` is missing is
  # less useful than knowing it is the auxiliary variable.
  expect_match(cli::ansi_strip(conditionMessage(direct)), "auxiliary variable")
})

test_that("a singleton frame list is canonical one-frame input", {
  design <- sampling_design() |> draw(n = 2)
  frame <- data.frame(id = 1:10)

  expect_true(validate_frame(design, list(frame)))
  payload <- jsonlite::fromJSON(
    design_json(design, frame = list(frame)), simplifyVector = FALSE
  )
  expect_named(payload$frame, c("required_variables", "fingerprint"))
  expect_null(payload$frame$fingerprints)

  # Identical to the data-frame spelling, field for field and version for
  # version: the container the caller wrote must not change the schema.
  direct <- jsonlite::fromJSON(
    design_json(design, frame = frame), simplifyVector = FALSE
  )
  expect_identical(names(direct$frame), names(payload$frame))
  expect_identical(direct$format_version, payload$format_version)
  expect_identical(direct$frame$fingerprint, payload$frame$fingerprint)
  expect_identical(
    names(direct$tools$samplyr$frame), names(payload$tools$samplyr$frame)
  )

  # A name on the singleton stays a diagnostic label and nothing more.
  labelled <- jsonlite::fromJSON(
    design_json(design, frame = list(register = frame)),
    simplifyVector = FALSE
  )
  expect_identical(names(labelled$frame), names(direct$frame))
  expect_identical(labelled$tools$samplyr$frame$source$value, "register")
})

test_that("genuinely several frames keep the plural schema", {
  frames <- list(mf_schools(), mf_classes(), mf_students())
  payload <- jsonlite::fromJSON(
    design_json(mf_design(), frame = frames), simplifyVector = FALSE
  )
  expect_named(payload$frame, c("required_variables", "fingerprints"))
  expect_length(payload$frame$fingerprints, 3L)
  expect_identical(payload$format_version, 3L)
  expect_length(payload$tools$samplyr$frame$frames, 3L)
})

test_that("a one-element plural artifact is still readable", {
  # samplyr writes the singular form, but the reader still accepts a
  # one-element plural as the same one-frame call.
  frame <- data.frame(id = 1:20, x = 1:20)
  design <- sampling_design() |> draw(n = 5)
  sample <- execute(design, frame, seed = 3, frame_digest = "none")

  payload <- jsonlite::fromJSON(
    design_json(sample, frame = frame), simplifyVector = FALSE
  )
  payload$frame$fingerprints <- list(payload$frame$fingerprint)
  payload$frame$fingerprint <- NULL
  legacy <- read_design(
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  )

  expect_true(validate_frame(legacy, frame))
  expect_identical(replay_design(legacy, frame)$id, sample$id)
  expect_identical(replay_design(legacy, list(frame))$id, sample$id)
})

test_that("serialized frame counts agree with design and receipt shapes", {
  shared <- execute(mf_design(), mf_hierarchy(), seed = 7)
  separate <- execute(
    mf_design(), mf_schools(), mf_classes(), mf_students(), seed = 7
  )

  expect_error(
    design_json(shared, frame = list(mf_hierarchy(), mf_hierarchy())),
    class = "samplyr_error_serialization_frame_count"
  )
  expect_error(
    design_json(separate, frame = mf_hierarchy()),
    class = "samplyr_error_serialization_frame_count"
  )
  expect_error(
    design_json(mf_design(), frame = list(mf_schools(), mf_classes())),
    class = "samplyr_error_serialization_frame_count"
  )
  # The narrow class inherits the count class execute() already raises, so a
  # caller can catch either without learning two names.
  expect_error(
    design_json(separate, frame = mf_hierarchy()),
    class = "samplyr_error_frame_count"
  )

  # The counts a design and a receipt do allow still round-trip.
  expect_s3_class(design_json(shared, frame = mf_hierarchy()), "json")
  expect_s3_class(
    design_json(separate, frame = list(mf_schools(), mf_classes(),
                                       mf_students())),
    "json"
  )
  expect_s3_class(design_json(mf_design(), frame = mf_hierarchy()), "json")
  expect_s3_class(
    design_json(
      mf_design(),
      frame = list(mf_schools(), mf_classes(), mf_students())
    ),
    "json"
  )
})

test_that("a matching frame count round-trips through write, read and replay", {
  frames <- list(mf_schools(), mf_classes(), mf_students())
  sample <- execute(mf_design(), frames, seed = 7)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = frames)
  restored <- read_design(path)

  expect_true(validate_frame(restored, frames))
  replayed <- replay_design(restored, frames)
  expect_identical(replayed$.sample_id, sample$.sample_id)
  expect_identical(replayed$student_no, sample$student_no)

  # Frame order is significant and is retained.
  expect_error(
    replay_design(restored, frames[c(2, 1, 3)]),
    class = "samplyr_error_replay_frame_mismatch"
  )
})

test_that("fingerprints normalize singular and plural fields exactly", {
  frames <- list(mf_schools(), mf_classes(), mf_students())
  sample <- execute(mf_design(), frames, seed = 7)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = frames)
  restored <- read_design(path)

  expect_true(validate_frame(restored, frames))

  plural <- attr(restored, "frame_info")[["fingerprints"]]
  attr(restored, "frame_info") <- list(fingerprint = plural[[1]])
  expect_error(
    replay_design(restored, frames),
    class = "samplyr_error_replay_frame_mismatch"
  )
})

test_that("stage selectors share type and uniqueness rules", {
  sample <- execute(
    mf_design(), mf_hierarchy(), seed = 7, frame_digest = "full"
  )
  calls <- list(
    execute = function(stages) {
      execute(mf_design(), mf_hierarchy(), stages = stages, seed = 1)
    },
    validate_frame = function(stages) {
      validate_frame(mf_design(), mf_hierarchy(), stages = stages)
    },
    joint_expectation = function(stages) {
      joint_expectation(sample, stages = stages)
    },
    frame_summary = function(stages) frame_summary(sample, stages = stages)
  )
  invalid <- list(
    empty_character = character(),
    empty_numeric = numeric(),
    missing = NA_real_,
    fractional = 1.5,
    duplicated = c(1, 1),
    zero = 0,
    negative = -1,
    out_of_range = 99
  )

  for (name in names(calls)) {
    for (case in names(invalid)) {
      expect_error(
        calls[[name]](invalid[[case]]),
        class = "samplyr_error_stage_selector",
        info = paste(name, case)
      )
    }
  }

  # Valid selectors are unaffected, scalar and vector alike.
  expect_s3_class(calls$execute(1), "tbl_sample")
  expect_s3_class(calls$execute(c(1, 2)), "tbl_sample")
  expect_true(calls$validate_frame(1))
  expect_true(calls$validate_frame(c(1, 2)))
})

test_that("stored designs retain the draw validation contract", {
  frame <- data.frame(
    id = 1:20,
    stratum = rep(c("a", "b"), 10),
    mos = 1:20,
    prn = (1:20) / 21
  )

  cps <- sampling_design() |> draw(n = 4, method = "pps_cps", mos = mos)
  cps$stages[[1]]$draw_spec$n <- NULL
  cps$stages[[1]]$draw_spec$frac <- 0.2

  allocated <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 4)
  allocated$stages[[1]]$draw_spec$n <- NULL
  allocated$stages[[1]]$draw_spec$frac <- 0.2

  prn <- sampling_design() |> draw(n = 4)
  prn$stages[[1]]$draw_spec$prn <- "prn"

  certainty <- sampling_design() |> draw(n = 4)
  certainty$stages[[1]]$draw_spec$mos <- "mos"
  certainty$stages[[1]]$draw_spec$certainty_size <- 10

  spatial <- sampling_design() |> draw(n = 4)
  spatial$stages[[1]]$draw_spec$method <- "lpm2"

  cases <- list(
    list(cps, "requires.*n"),
    list(allocated, "cannot be combined"),
    list(prn, "only supported"),
    list(certainty, "only available"),
    list(spatial, "requires.*spread")
  )
  for (case in cases) {
    expect_error(execute(case[[1]], frame, seed = 1), case[[2]])
  }
})
