## Frames of other data frame classes

# Two made-up data.frame subclasses: one keeps a column in every `[` subset,
# one answers `[[` and `$` with the wrong column. Each must give the plain
# frame's result at every entry point.

`[.samplyr_test_sticky` <- function(x, i, j, ..., drop = FALSE) {
  keep <- attr(x, "sticky")
  cls <- class(x)
  y <- NextMethod()
  if (is.data.frame(y)) {
    if (!keep %in% names(y)) {
      col <- .subset2(x, keep)
      y[[keep]] <- if (missing(i)) col else col[i]
    }
    attr(y, "sticky") <- keep
    class(y) <- cls
  }
  y
}
registerS3method("[", "samplyr_test_sticky", `[.samplyr_test_sticky`)

`[[.samplyr_test_misnamed` <- function(x, i, ...) .subset2(x, 1L)
`$.samplyr_test_misnamed` <- function(x, name) .subset2(x, 1L)
registerS3method("[[", "samplyr_test_misnamed", `[[.samplyr_test_misnamed`)
registerS3method("$", "samplyr_test_misnamed", `$.samplyr_test_misnamed`)

# The sample's columns, without its attributes (which carry a timestamp).
sample_columns <- function(x) lapply(x, identity)

frame_class_fixture <- function() {
  frame <- data.frame(
    psu = rep(1:12, each = 5),
    region = rep(c("north", "south"), each = 30),
    size = rep(c(3, 8, 5, 12, 7, 4, 9, 6, 11, 2, 10, 5), each = 5)
  )
  frame$eid <- seq_len(nrow(frame))
  frame$y <- frame$size + frame$eid / 10
  frame
}

odd_frames <- function(frame) {
  tagged <- frame
  tagged$tag <- as.list(seq_len(nrow(frame)))
  list(
    sticky = structure(
      tagged,
      class = c("samplyr_test_sticky", "data.frame"),
      sticky = "tag"
    ),
    misnamed = structure(
      frame,
      class = c("samplyr_test_misnamed", "data.frame")
    )
  )
}

test_that("the made-up classes do change subsetting", {
  # Guards the premise: without it every test below would pass trivially.
  odd <- odd_frames(frame_class_fixture())
  expect_true("tag" %in% names(odd$sticky[, "region", drop = FALSE]))
  expect_identical(odd$misnamed[["region"]], odd$misnamed[["psu"]])
})

test_that("another data frame class gives the plain frame's sample", {
  frame <- frame_class_fixture()
  design <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(psu) |>
    draw(n = 2, method = "pps_brewer", mos = size)
  plain <- execute(design, frame, seed = 1)

  for (nm in c("sticky", "misnamed")) {
    odd <- odd_frames(frame)[[nm]]
    got <- execute(design, odd, seed = 1)
    expect_identical(got$eid, plain$eid, info = nm)
    expect_identical(got$.weight, plain$.weight, info = nm)
    expect_s3_class(got, "tbl_sample")
  }
})

test_that("every entry point reads another class by its columns", {
  frame <- frame_class_fixture()
  design <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(psu) |>
    draw(n = 2, method = "pps_brewer", mos = size)
  plain_sample <- execute(design, frame, seed = 1)

  for (nm in c("sticky", "misnamed")) {
    odd <- odd_frames(frame)[[nm]]
    expect_no_error(validate_frame(design, odd))
    expect_equal(
      frame_summary(design, odd),
      frame_summary(design, frame),
      info = nm
    )
    expect_equal(
      exante_probabilities(design, odd, key = eid),
      exante_probabilities(design, frame, key = eid),
      info = nm
    )
    expect_equal(
      joint_expectation(plain_sample, odd),
      joint_expectation(plain_sample, frame),
      info = nm
    )
  }

  # A continuation listing is a frame too.
  two_stage <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage() |>
    draw(n = 2)
  first <- execute(two_stage, frame, stages = 1, seed = 1)
  plain_second <- execute(first, frame, seed = 2)
  for (nm in c("sticky", "misnamed")) {
    got <- execute(first, odd_frames(frame)[[nm]], seed = 2)
    expect_identical(got$eid, plain_second$eid, info = nm)
  }
})

## Grouped and rowwise frames are ungrouped

test_that("a grouped or rowwise frame gives the ungrouped frame's sample", {
  frame <- tibble::as_tibble(frame_class_fixture())
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage() |>
    draw(n = 2)
  plain <- execute(design, frame, seed = 4)

  withr::local_options(rlib_message_verbosity = "quiet")
  inputs <- list(
    by_stratum = dplyr::group_by(frame, region),
    by_other = dplyr::group_by(frame, size),
    rowwise = dplyr::rowwise(frame)
  )
  for (nm in names(inputs)) {
    got <- execute(design, inputs[[nm]], seed = 4)
    expect_false(dplyr::is_grouped_df(got), info = nm)
    expect_false(inherits(got, "rowwise_df"), info = nm)
    # Attributes differ only by the execution timestamp.
    expect_identical(sample_columns(got), sample_columns(plain), info = nm)
  }

  # A grouped listing at a continuation is ungrouped too.
  first <- execute(design, frame, stages = 1, seed = 4)
  listing <- dplyr::group_by(frame, region)
  expect_identical(
    sample_columns(execute(first, listing, seed = 5)),
    sample_columns(execute(first, frame, seed = 5))
  )
})

test_that("a grouped frame exports like the ungrouped one", {
  skip_if_not_installed("survey")
  frame <- tibble::as_tibble(frame_class_fixture())
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage() |>
    draw(n = 2)
  withr::local_options(rlib_message_verbosity = "quiet")
  grouped <- execute(design, dplyr::group_by(frame, region), seed = 4)
  plain <- execute(design, frame, seed = 4)

  # The stage join must not split the grouping column into .x and .y.
  expect_true("region" %in% names(grouped))
  expect_false(any(c("region.x", "region.y") %in% names(grouped)))
  expect_equal(
    survey::svytotal(~y, as_svydesign(grouped)),
    survey::svytotal(~y, as_svydesign(plain))
  )
})

test_that("ungrouping is announced once per session", {
  frame <- dplyr::group_by(frame_class_fixture(), region)
  design <- sampling_design() |> draw(n = 4)
  rlang::reset_message_verbosity("samplyr_frame_ungrouped")
  withr::defer(rlang::reset_message_verbosity("samplyr_frame_ungrouped"))

  expect_message(
    execute(design, frame, seed = 1),
    class = "samplyr_message_frame_ungrouped"
  )
  expect_no_message(execute(design, frame, seed = 2))
})

test_that("a grouped sample used as a phase-2 frame is ungrouped", {
  # Ungrouped in place, so the class and metadata two-phase needs survive.
  frame <- frame_class_fixture()
  phase1 <- sampling_design() |>
    cluster_by(eid) |>
    draw(n = 40) |>
    execute(frame, seed = 1)
  phase2 <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(eid) |>
    draw(n = 5)
  withr::local_options(rlib_message_verbosity = "quiet")
  grouped <- execute(phase2, dplyr::group_by(phase1, region), seed = 2)
  plain <- execute(phase2, phase1, seed = 2)

  expect_false(dplyr::is_grouped_df(grouped))
  expect_identical(sample_columns(grouped), sample_columns(plain))
  expect_false(is.null(attr(grouped, "metadata")$prev_phase))
})
