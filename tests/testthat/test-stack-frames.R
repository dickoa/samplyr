## stack_frames(): overlapping frames over one target population

# The fixture takes every unit of each frame, so rows and domains are exact
# and no seed is recorded. Frame a holds units 1-4 and frame b units 3-6, so
# the domains are {1, 2} in a alone, {3, 4} in both, {5, 6} in b alone.

frame_population <- function() {
  data.frame(
    id = 1:6,
    in_a = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    in_b = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
}

frame_component <- function(col, population = frame_population(), seed = NULL) {
  rows <- population[population[[col]], , drop = FALSE]
  sampling_design() |>
    draw(n = nrow(rows)) |>
    execute(rows, seed = seed)
}

frame_fixture <- function(population = frame_population()) {
  stack_frames(
    a = frame_component("in_a", population),
    b = frame_component("in_b", population),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )
}

# Row order within a component is not a contract, so the view is sorted by
# key inside each component. The order of the component blocks is a contract.
frame_view <- function(frames) {
  view <- as.data.frame(frames)
  view[order(match(view$.frame, names(frames)), view$id), , drop = FALSE]
}

## The object

test_that("a stack is a collection, not a sample or a data frame", {
  frames <- frame_fixture()

  expect_s3_class(frames, "frame_stack")
  expect_false(inherits(frames, "tbl_sample"))
  expect_false(inherits(frames, "tbl_df"))
  expect_false(inherits(frames, "data.frame"))
  expect_identical(names(frames), c("a", "b"))
  expect_identical(
    attr(frames, "membership"),
    c(a = "in_a", b = "in_b")
  )
  expect_identical(attr(frames, "key"), "id")
})

test_that("the components come back out intact", {
  component_a <- frame_component("in_a")
  component_b <- frame_component("in_b")
  frames <- stack_frames(
    a = component_a,
    b = component_b,
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  expect_identical(frames[["a"]], component_a)
  expect_identical(frames[["b"]], component_b)
  expect_s3_class(frames[["a"]], "tbl_sample")
})

test_that("membership is stored as the mapping, in frame order", {
  # Given in the other order, so a stored copy of the argument would fail.
  frames <- stack_frames(
    a = frame_component("in_a"),
    b = frame_component("in_b"),
    membership = c(b = "in_b", a = "in_a"),
    key = id
  )
  expect_identical(attr(frames, "membership"), c(a = "in_a", b = "in_b"))
})

## The row-bound view

test_that("as.data.frame() labels the frame and the domain of every row", {
  view <- frame_view(frame_fixture())

  expect_identical(class(view), "data.frame")
  expect_false(is_tbl_sample(view))
  expect_identical(names(view)[1:2], c(".frame", ".domain"))
  expect_identical(view$id, c(1L, 2L, 3L, 4L, 3L, 4L, 5L, 6L))
  expect_identical(view$.frame, c(rep("a", 4), rep("b", 4)))
  expect_identical(
    view$.domain,
    c("a", "a", "a+b", "a+b", "a+b", "a+b", "b", "b")
  )
})

test_that("the view is a copy: editing it does not reach the components", {
  component_a <- frame_component("in_a")
  frames <- stack_frames(
    a = component_a,
    b = frame_component("in_b"),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  view <- as.data.frame(frames)
  view$.weight <- 0
  view$in_a <- FALSE

  expect_identical(frames[["a"]], component_a)
  expect_true(all(frames[["a"]]$.weight > 0))
})

test_that("components may carry different columns, and the view fills them", {
  # Only the membership columns and the key must be in every component.
  population <- frame_population()
  with_extra <- population
  with_extra$listed_at <- "2026"

  frames <- stack_frames(
    a = frame_component("in_a", with_extra),
    b = frame_component("in_b", population),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  view <- frame_view(frames)
  expect_identical(view$listed_at, c(rep("2026", 4), rep(NA_character_, 4)))
})

test_that("the view carries the membership columns statistical code reads", {
  view <- as.data.frame(frame_fixture())
  expect_true(all(c("in_a", "in_b") %in% names(view)))
  expect_identical(view$in_a & view$in_b, view$.domain == "a+b")
})

## Domain labels

test_that("domain labels do not depend on the order the frames were given", {
  forward <- frame_view(frame_fixture())
  backward <- frame_view(stack_frames(
    b = frame_component("in_b"),
    a = frame_component("in_a"),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  ))

  expect_identical(
    forward$.domain[forward$.frame == "a"],
    backward$.domain[backward$.frame == "a"]
  )
  expect_identical(
    sort(unique(forward$.domain)),
    sort(unique(backward$.domain))
  )
})

test_that("domain labels sort in byte order, not the session collation", {
  # Byte order gives "B+a", where most locales would give "a+B".
  population <- data.frame(
    id = 1:4,
    in_B = c(TRUE, TRUE, FALSE, FALSE),
    in_a = c(TRUE, FALSE, TRUE, TRUE)
  )
  frames <- stack_frames(
    B = frame_component("in_B", population),
    a = frame_component("in_a", population),
    membership = c(B = "in_B", a = "in_a"),
    key = id
  )

  view <- as.data.frame(frames)
  expect_identical(view$.domain[view$id == 1L], c("B+a", "B+a"))
  expect_identical(view$.domain[view$id == 2L], "B")
})

test_that("domains at K = 3 come from the membership columns", {
  population <- data.frame(
    id = 1:7,
    in_a = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
    in_b = c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
    in_c = c(FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  frames <- stack_frames(
    a = frame_component("in_a", population),
    b = frame_component("in_b", population),
    c = frame_component("in_c", population),
    membership = c(a = "in_a", b = "in_b", c = "in_c"),
    key = id
  )

  view <- as.data.frame(frames)
  by_unit <- vapply(1:7, function(u) view$.domain[view$id == u][[1]],
                    character(1))
  expect_identical(
    by_unit,
    c("a", "a+b", "a+c", "a+b+c", "b", "b+c", "c")
  )
  # Seven of them, which is 2^3 - 1: the population fills the lattice.
  expect_identical(sort(unique(view$.domain)), sort(by_unit))
})

test_that("domains at K = 4 are labelled by the whole frame set", {
  population <- data.frame(
    id = 1:3,
    in_a = c(TRUE, TRUE, FALSE),
    in_b = c(TRUE, TRUE, FALSE),
    in_c = c(TRUE, FALSE, TRUE),
    in_d = c(TRUE, FALSE, TRUE)
  )
  frames <- stack_frames(
    a = frame_component("in_a", population),
    b = frame_component("in_b", population),
    c = frame_component("in_c", population),
    d = frame_component("in_d", population),
    membership = c(a = "in_a", b = "in_b", c = "in_c", d = "in_d"),
    key = id
  )

  view <- as.data.frame(frames)
  expect_identical(unique(view$.domain[view$id == 1L]), "a+b+c+d")
  expect_identical(unique(view$.domain[view$id == 2L]), "a+b")
  expect_identical(unique(view$.domain[view$id == 3L]), "c+d")
})

## Components

test_that("fewer than two frames is refused", {
  expect_error(
    stack_frames(
      a = frame_component("in_a"),
      membership = c(a = "in_a"),
      key = id
    ),
    class = "samplyr_error_stack_frames_input"
  )
})

test_that("every frame must be named, and distinctly", {
  expect_error(
    stack_frames(
      frame_component("in_a"),
      b = frame_component("in_b"),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    ),
    class = "samplyr_error_stack_frames_names"
  )
  expect_error(
    stack_frames(
      a = frame_component("in_a"),
      a = frame_component("in_b"),
      membership = c(a = "in_a"),
      key = id
    ),
    class = "samplyr_error_stack_frames_names"
  )
})

test_that("a frame name carrying the domain separator is refused", {
  population <- frame_population()
  names(population)[names(population) == "in_a"] <- "in_x"
  expect_error(
    stack_frames(
      `a+x` = frame_component("in_b"),
      b = frame_component("in_b"),
      membership = c(`a+x` = "in_b", b = "in_b"),
      key = id
    ),
    class = "samplyr_error_stack_frames_separator"
  )
})

test_that("a component that is not an executed sample is refused", {
  expect_error(
    stack_frames(
      a = frame_component("in_a"),
      b = frame_population(),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    ),
    class = "samplyr_error_stack_frames_input"
  )
})

test_that("a misspelled reserved argument is named, not left in the dots", {
  # `membership` and `key` follow `...`, so `membershp` is matched as a frame.
  expect_error(
    stack_frames(
      a = frame_component("in_a"),
      b = frame_component("in_b"),
      membershp = c(a = "in_a", b = "in_b"),
      key = id
    ),
    regexp = "Did you mean",
    class = "samplyr_error_stack_frames_input"
  )
})

test_that("a replicated execution is refused as a component", {
  population <- frame_population()
  rows <- population[population$in_a, , drop = FALSE]
  replicated <- sampling_design() |>
    draw(n = 2) |>
    execute(rows, seed = 1, reps = 3)

  expect_error(
    stack_frames(
      a = replicated,
      b = frame_component("in_b"),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    ),
    class = "samplyr_error_replicated_sample_unsupported"
  )
})

test_that("a tampered component is refused", {
  tampered <- frame_component("in_a")
  tampered$.weight <- tampered$.weight * 2

  expect_error(
    stack_frames(
      a = tampered,
      b = frame_component("in_b"),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    ),
    class = "samplyr_error_modified_sample"
  )
})

## The membership mapping

test_that("membership must be given", {
  expect_error(
    stack_frames(
      a = frame_component("in_a"),
      b = frame_component("in_b"),
      key = id
    ),
    class = "samplyr_error_stack_frames_membership"
  )
})

test_that("membership must be a named character vector naming every frame", {
  component_a <- frame_component("in_a")
  component_b <- frame_component("in_b")

  expect_error(
    stack_frames(
      a = component_a, b = component_b,
      membership = c("in_a", "in_b"), key = id
    ),
    class = "samplyr_error_stack_frames_membership"
  )
  expect_error(
    stack_frames(
      a = component_a, b = component_b,
      membership = c(a = "in_a"), key = id
    ),
    class = "samplyr_error_stack_frames_membership"
  )
  expect_error(
    stack_frames(
      a = component_a, b = component_b,
      membership = c(a = "in_a", b = "in_b", c = "in_c"), key = id
    ),
    class = "samplyr_error_stack_frames_membership"
  )
})

test_that("two frames may not share one membership column", {
  expect_error(
    stack_frames(
      a = frame_component("in_a"),
      b = frame_component("in_b"),
      membership = c(a = "in_a", b = "in_a"),
      key = id
    ),
    class = "samplyr_error_stack_frames_membership"
  )
})

test_that("every component must carry every membership column", {
  partial <- frame_component("in_b")
  partial <- dplyr::select(partial, -"in_a")

  expect_error(
    stack_frames(
      a = frame_component("in_a"),
      b = partial,
      membership = c(a = "in_a", b = "in_b"),
      key = id
    ),
    class = "samplyr_error_stack_frames_membership"
  )
})

test_that("integer 0/1 membership is refused rather than read", {
  population <- frame_population()
  population$in_b <- as.integer(population$in_b)

  expect_error(
    frame_fixture(population),
    class = "samplyr_error_stack_frames_membership_type"
  )
})

test_that("a missing membership is an error, not a domain", {
  population <- frame_population()
  population$in_b[[1]] <- NA

  expect_error(
    frame_fixture(population),
    class = "samplyr_error_stack_frames_membership_missing"
  )
})

test_that("a component false on its own frame is refused", {
  # The diagonal ties each column to its frame's register.
  population <- frame_population()
  population$in_a[[1]] <- FALSE
  rows <- population[population$id %in% 1:4, , drop = FALSE]

  expect_error(
    stack_frames(
      a = sampling_design() |> draw(n = 4) |> execute(rows),
      b = frame_component("in_b", population),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    ),
    class = "samplyr_error_stack_frames_membership_diagonal"
  )
})

# Two censuses of overlapping registers, 100 units in both. A's register
# says the overlap is not in B, B's says it is.
conflict_population <- function() {
  data.frame(
    pid = 1:300,
    y = as.numeric(1:300),
    in_a = rep(c(TRUE, FALSE), c(200, 100)),
    in_b = rep(c(FALSE, TRUE), c(100, 200))
  )
}

census_component <- function(rows, seed) {
  sampling_design() |> draw(n = nrow(rows)) |> execute(rows, seed = seed)
}

test_that("a unit whose components disagree on its membership is refused", {
  population <- conflict_population()
  register_a <- population[population$in_a, ]
  register_b <- population[population$in_b, ]
  register_a$in_b[register_a$pid > 100] <- FALSE

  expect_error(
    stack_frames(
      a = census_component(register_a, 1),
      b = census_component(register_b, 2),
      membership = c(a = "in_a", b = "in_b"),
      key = pid
    ),
    class = "samplyr_error_stack_frames_membership_conflict"
  )

  # One unit is enough, and keys compare as labels across factor and text.
  register_a <- population[population$in_a, ]
  register_a$in_b[register_a$pid == 150] <- FALSE
  register_a$pid <- as.character(register_a$pid)
  register_b$pid <- factor(register_b$pid)
  expect_error(
    stack_frames(
      a = census_component(register_a, 1),
      b = census_component(register_b, 2),
      membership = c(a = "in_a", b = "in_b"),
      key = pid
    ),
    class = "samplyr_error_stack_frames_membership_conflict"
  )
})

test_that("agreeing memberships stack, whatever the component shapes", {
  population <- conflict_population()
  register_a <- population[population$in_a, ]
  register_b <- population[population$in_b, ]
  one_row <- census_component(register_a[150, ], 3)
  with_replacement <- sampling_design() |>
    draw(n = 50, method = "pps_multinomial", mos = y) |>
    execute(register_b, seed = 1)
  expect_true(anyDuplicated(with_replacement$pid) > 0)

  expect_s3_class(
    stack_frames(
      a = one_row, b = with_replacement,
      membership = c(a = "in_a", b = "in_b"),
      key = pid
    ),
    "frame_stack"
  )
})

test_that("a data column named like a generated one is refused", {
  population <- frame_population()
  population$.domain <- "x"

  expect_error(
    frame_fixture(population),
    class = "samplyr_error_stack_frames_columns"
  )
})

## The key

test_that("key must be given as a bare column", {
  component_a <- frame_component("in_a")
  component_b <- frame_component("in_b")

  expect_error(
    stack_frames(
      a = component_a, b = component_b,
      membership = c(a = "in_a", b = "in_b")
    ),
    class = "samplyr_error_stack_frames_key"
  )
  expect_error(
    stack_frames(
      a = component_a, b = component_b,
      membership = c(a = "in_a", b = "in_b"), key = "id"
    ),
    class = "samplyr_error_stack_frames_key"
  )
})

test_that("key must be a column of every component", {
  expect_error(
    stack_frames(
      a = frame_component("in_a"),
      b = frame_component("in_b"),
      membership = c(a = "in_a", b = "in_b"),
      key = person_id
    ),
    class = "samplyr_error_stack_frames_key"
  )
})

test_that("key must hold the same kind of value in every component", {
  population <- frame_population()
  text <- population
  text$id <- as.character(text$id)

  expect_error(
    stack_frames(
      a = frame_component("in_a", population),
      b = frame_component("in_b", text),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    ),
    class = "samplyr_error_stack_frames_key"
  )
})

test_that("a missing or repeated key is refused", {
  missing_key <- frame_population()
  missing_key$id[[1]] <- NA_integer_
  expect_error(
    frame_fixture(missing_key),
    class = "samplyr_error_stack_frames_key"
  )

  repeated <- frame_population()
  repeated$id[[2]] <- 1L
  expect_error(
    frame_fixture(repeated),
    class = "samplyr_error_stack_frames_key"
  )
})

test_that("a with-replacement component may repeat its key", {
  # The key repeats by construction, so the identity checked is the draw.
  population <- frame_population()
  rows <- population[population$in_a, , drop = FALSE]
  wr <- sampling_design() |>
    draw(n = 8, method = "srswr") |>
    execute(rows, seed = 4)

  expect_gt(sum(duplicated(wr$id)), 0)

  frames <- stack_frames(
    a = wr,
    b = frame_component("in_b", population),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )
  expect_s3_class(frames, "frame_stack")
  expect_identical(nrow(as.data.frame(frames)), 12L)
})

## Independence

test_that("one seed used by two frames warns", {
  expect_warning(
    stack_frames(
      a = frame_component("in_a", seed = 7),
      b = frame_component("in_b", seed = 7),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    ),
    class = "samplyr_warning_frame_seed_reused"
  )
})

test_that("distinct seeds and absent seeds do not warn", {
  population <- frame_population()

  expect_no_warning(
    stack_frames(
      a = frame_component("in_a", population, seed = 7),
      b = frame_component("in_b", population, seed = 8),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    )
  )
  # A take-all component records no seed, so it is never compared.
  expect_no_warning(frame_fixture(population))
  expect_no_warning(
    stack_frames(
      a = frame_component("in_a", population, seed = 7),
      b = frame_component("in_b", population),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    )
  )
})

test_that("the seed warning names the frames that share the seed", {
  expect_warning(
    stack_frames(
      a = frame_component("in_a", seed = 7),
      b = frame_component("in_b", seed = 7),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    ),
    regexp = "a and b"
  )
})

## Composition with share_weights()

test_that("a shared-weight component may be stacked", {
  # One target population, reached through links and listed directly.
  dwellings <- data.frame(dwelling_id = 1:12)
  source_sample <- sampling_design() |>
    draw(n = 6) |>
    execute(dwellings, seed = 11)

  people <- data.frame(
    person_id = 1:12,
    dwelling_id = 1:12,
    in_reached = TRUE,
    in_list = rep(c(TRUE, FALSE), times = 6)
  )
  links <- data.frame(dwelling_id = 1:12, person_id = 1:12)

  shared <- share_weights(
    source_sample,
    targets = people,
    links = links,
    by = c(dwelling_id = "dwelling_id"),
    to = c(person_id = "person_id"),
    within = NULL,
    multiplicity = complete_links()
  )

  listed <- sampling_design() |>
    draw(n = 6) |>
    execute(people[people$in_list, ], seed = 12)

  frames <- stack_frames(
    reached = shared,
    list = listed,
    membership = c(reached = "in_reached", list = "in_list"),
    key = person_id
  )

  expect_s3_class(frames, "frame_stack")
  expect_false(is_null(attr(frames[["reached"]], "metadata")$weight_share))
  expect_identical(frames[["reached"]], shared)
})

## Reporting

test_that("print reports the frames, the key and what is not combined", {
  frames <- frame_fixture()
  output <- capture.output(print(frames))

  expect_true(any(grepl("Frame Stack", output)))
  expect_true(any(grepl("2 frames over key id", output)))
  expect_true(any(grepl("a: 4 rows, in_a, no seed", output)))
  expect_true(any(grepl("b: 4 rows, in_b, no seed", output)))
  expect_true(any(grepl("not composited here", output)))
})

test_that("summary reports the domains in canonical order", {
  frames <- frame_fixture()
  output <- capture.output(summary(frames))

  expect_true(any(grepl("2 frames \\| 8 rows \\| key id", output)))
  # Domain lines end in a row count, frame lines in a seed.
  domains <- grep(" rows$", output, value = TRUE)
  expect_identical(
    sub("^. ", "", domains),
    c("a: 2 rows", "a+b: 4 rows", "b: 2 rows")
  )
})

test_that("print and summary return their input invisibly", {
  frames <- frame_fixture()
  expect_identical(capture.output(returned <- print(frames)), {
    capture.output(print(frames))
  })
  expect_identical(returned, frames)
  invisible(capture.output(summarized <- summary(frames)))
  expect_identical(summarized, frames)
})

## Compositions with the longitudinal feature

test_that("a materialized wave may be a component, and is refused at export", {
  skip_if_not_installed("survey")

  population <- data.frame(
    ea_id = sprintf("e%03d", 1:200),
    region = rep(c("N", "S"), each = 100),
    in_w = TRUE,
    in_o = TRUE,
    stringsAsFactors = FALSE
  )
  schedule <- data.frame(
    panel = rep(1:4, times = 4), wave = rep(1:4, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
               FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  master <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 30) |>
    execute(population, seed = 42, panels = schedule)
  wave <- execute(master, wave = 1)
  other <- sampling_design() |> draw(n = 30) |> execute(population, seed = 9)

  # A wave has membership and a key, which is all `stack_frames()` reads.
  stack <- stack_frames(
    w = wave, o = other,
    membership = c(w = "in_w", o = "in_o"), key = ea_id
  )
  expect_s3_class(stack, "frame_stack")
  expect_identical(nrow(stack[["w"]]), nrow(wave))
  expect_true(survey_phase_info(stack[["w"]])$is_twophase)

  # The inspection views take it, because they describe rather than estimate.
  expect_s3_class(as.data.frame(stack), "data.frame")
  expect_output(print(summary(stack)), "2 frames")

  # A wave's second phase is an activation, not a selection, so both refuse.
  expect_error(
    as_svydesign(stack),
    class = "samplyr_error_survey_multiframe_unsupported"
  )
  expect_error(as_svydesign(stack), regexp = "two-phase")
  expect_error(as_svydesign(stack), regexp = "\"w\"")
  expect_error(
    as_svrepdesign(stack, type = "bootstrap", replicates = 10),
    class = "samplyr_error_svrep_twophase_unsupported"
  )

  # The master is not two-phase, so the same stack built from it exports.
  from_master <- stack_frames(
    w = master, o = other,
    membership = c(w = "in_w", o = "in_o"), key = ea_id
  )
  expect_s3_class(as_svydesign(from_master), "multiframe")
})
