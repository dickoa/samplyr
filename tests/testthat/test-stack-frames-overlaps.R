## F4. Declared overlaps and the expected estimator

# The frames take equal-probability samples so every inclusion probability is
# a number the fixture can state exactly and the tests can assert against:
# 10 of 40 in frame a, 20 of 40 in frame b.
#
#   population   1 .. 20 .. 40 .. 60
#   frame a      x     x               pi_a = 0.25
#   frame b            x     x         pi_b = 0.5
#
# So the expected estimator's weight is 1 / (0.25 + 0.5) = 4/3 on the overlap,
# 4 on frame a alone and 2 on frame b alone. Those three numbers are the whole
# oracle and none of them comes from samplyr.

overlap_pi_a <- 10 / 40
overlap_pi_b <- 20 / 40

overlap_population <- function() {
  population <- data.frame(
    id = 1:60,
    y = as.numeric(1:60),
    in_a = rep(c(TRUE, FALSE), times = c(40, 20)),
    in_b = rep(c(FALSE, TRUE), times = c(20, 40))
  )
  population$pi_a <- ifelse(population$in_a, overlap_pi_a, 0)
  population$pi_b <- ifelse(population$in_b, overlap_pi_b, 0)
  population$w_a <- ifelse(population$in_a, 1 / overlap_pi_a, 0)
  population$w_b <- ifelse(population$in_b, 1 / overlap_pi_b, 0)
  population
}

overlap_component <- function(col, n, seed, population = overlap_population()) {
  sampling_design() |>
    draw(n = n) |>
    execute(population[population[[col]], , drop = FALSE], seed = seed)
}

overlap_fixture <- function(population = overlap_population(),
                            overlaps = declared_overlaps(
                              a = "pi_a", b = "pi_b",
    scale = "probabilities")) {
  stack_frames(
    a = overlap_component("in_a", 10, 1, population),
    b = overlap_component("in_b", 20, 2, population),
    membership = c(a = "in_a", b = "in_b"),
    key = id,
    overlaps = overlaps
  )
}

## The record

test_that("the declared scale is stored, and the columns in frame order", {
  frames <- overlap_fixture()
  record <- attr(frames, "overlaps")

  expect_identical(record$scale, "probabilities")
  expect_identical(record$cols, c(a = "pi_a", b = "pi_b"))
})

test_that("a stack without overlaps records none", {
  frames <- stack_frames(
    a = overlap_component("in_a", 10, 1),
    b = overlap_component("in_b", 20, 2),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )
  expect_null(attr(frames, "overlaps"))
})

test_that("both scales normalize to the same probabilities", {
  probabilities <- overlap_fixture()
  weights <- overlap_fixture(
    overlaps = declared_overlaps(a = "w_a", b = "w_b", scale = "weights")
  )

  for (nm in c("a", "b")) {
    expect_equal(
      frame_component_overlaps(probabilities, nm),
      frame_component_overlaps(weights, nm)
    )
  }
})

test_that("a frame a unit does not belong to reads as zero, not as a chance", {
  frames <- overlap_fixture()
  component <- frames[["a"]]
  matrix <- frame_component_overlaps(frames, "a")

  expect_identical(colnames(matrix), c("a", "b"))
  expect_true(all(matrix[, "a"] == overlap_pi_a))
  expect_identical(matrix[, "b"] == 0, !component$in_b)
})

test_that("print reports the declared scale", {
  output <- capture.output(print(overlap_fixture()))
  expect_true(any(grepl("Overlaps declared as probabilities", output)))
  expect_false(any(grepl(
    "Overlaps",
    capture.output(print(stack_frames(
      a = overlap_component("in_a", 10, 1),
      b = overlap_component("in_b", 20, 2),
      membership = c(a = "in_a", b = "in_b"),
      key = id
    )))
  )))
})

## The expected estimator

test_that("the expected weight is one over the sum of the chances", {
  skip_if_not_installed("survey")
  frames <- overlap_fixture()
  svy <- as_svydesign(frames, estimator = "expected")

  view <- as.data.frame(frames)
  weights <- stats::weights(svy)
  expect_equal(
    unname(weights[view$.domain == "a+b"]),
    rep(1 / (overlap_pi_a + overlap_pi_b), sum(view$.domain == "a+b"))
  )
  expect_equal(
    unname(weights[view$.domain == "a"]),
    rep(1 / overlap_pi_a, sum(view$.domain == "a"))
  )
  expect_equal(
    unname(weights[view$.domain == "b"]),
    rep(1 / overlap_pi_b, sum(view$.domain == "b"))
  )
})

test_that("the two declared scales give the same estimate", {
  skip_if_not_installed("survey")
  probabilities <- overlap_fixture()
  weights <- overlap_fixture(
    overlaps = declared_overlaps(a = "w_a", b = "w_b", scale = "weights")
  )

  expect_equal(
    coef(survey::svytotal(
      ~y, as_svydesign(probabilities, estimator = "expected")
    )),
    coef(survey::svytotal(~y, as_svydesign(weights, estimator = "expected")))
  )
})

test_that("the two routes agree on the expected point estimate", {
  skip_if_not_installed("survey")
  frames <- overlap_fixture()

  linearized <- coef(survey::svytotal(
    ~y, as_svydesign(frames, estimator = "expected")
  ))
  replicate <- coef(survey::svytotal(
    ~y,
    as_svrepdesign(
      frames, estimator = "expected", type = "bootstrap", replicates = 40
    )
  ))
  expect_equal(replicate, linearized)
})

test_that("the expected estimate differs from the constant one", {
  skip_if_not_installed("survey")
  # If they agreed, an export ignoring the overlaps entirely would pass.
  frames <- overlap_fixture()
  expect_false(isTRUE(all.equal(
    coef(survey::svytotal(~y, as_svydesign(frames, estimator = "expected"))),
    coef(survey::svytotal(~y, as_svydesign(frames)))
  )))
})

test_that("the exporter hands survey weights, which it cannot misread", {
  skip_if_not_installed("survey")
  # survey infers the scale, and reads a matrix as weights when no non-zero
  # entry in some frame falls below one. Weight form satisfies that rule by
  # construction; probability form is what a census or a certainty overlap
  # would push over the line.
  frames <- overlap_fixture()
  supplied <- multiframe_overlap_weights(frames)

  expect_identical(names(supplied), c("a", "b"))
  for (matrix in supplied) {
    non_zero <- matrix[matrix > 0]
    expect_true(all(non_zero >= 1))
  }
  expect_true(all(supplied[["a"]][, "a"] == 1 / overlap_pi_a))
})

test_that("the weight form survives the case survey's inference gets wrong", {
  skip_if_not_installed("survey")
  # survey reads a matrix as weights when no non-zero entry in SOME frame
  # falls below one. Here frame a is a census whose units are all certainties
  # in b, so frame a's matrix is all ones, while frame b holds a genuine 0.5.
  # Handed probabilities, survey would take the weights branch and invert the
  # wrong quantity. Handed weights, there is nothing to infer.
  population <- data.frame(
    id = 1:60,
    y = as.numeric(1:60),
    in_a = rep(c(TRUE, FALSE), times = c(20, 40)),
    in_b = TRUE,
    stratum = rep(c("shared", "other"), times = c(20, 40))
  )
  population$pi_a <- ifelse(population$in_a, 1, 0)
  population$pi_b <- ifelse(population$stratum == "shared", 1, 0.5)

  frames <- stack_frames(
    a = sampling_design() |>
      draw(n = 20) |>
      execute(population[population$in_a, , drop = FALSE], seed = 1),
    b = sampling_design() |>
      stratify_by(stratum) |>
      draw(n = c(shared = 20, other = 20)) |>
      execute(population, seed = 2),
    membership = c(a = "in_a", b = "in_b"),
    key = id,
    overlaps = declared_overlaps(
      a = "pi_a", b = "pi_b", scale = "probabilities"
    )
  )

  supplied <- multiframe_overlap_weights(frames)
  mins <- vapply(supplied, function(m) min(m[m > 0]), numeric(1))
  expect_true(all(mins >= 1))

  view <- as.data.frame(frames)
  weights <- stats::weights(as_svydesign(frames, estimator = "expected"))
  # 1 / (1 + 1) where both frames are certain, 1 / 0.5 where only b reaches.
  expect_equal(unname(weights[view$in_a]), rep(0.5, sum(view$in_a)))
  expect_equal(unname(weights[!view$in_a]), rep(2, sum(!view$in_a)))

  # What handing survey the probabilities instead would have produced.
  probabilities <- lapply(names(frames), function(nm) {
    frame_component_overlaps(frames, nm)
  })
  misread <- survey::multiframe(
    lapply(names(frames), function(nm) as_svydesign(frames[[nm]])),
    probabilities,
    estimator = "expected"
  )
  expect_false(isTRUE(all.equal(
    unname(stats::weights(misread)), unname(weights)
  )))
})

test_that("three frames take the expected estimator on the replicate route", {
  skip_if_not_installed("survey")
  population <- overlap_population()
  population$in_c <- rep(c(TRUE, FALSE, TRUE), times = c(15, 25, 20))
  population$pi_c <- ifelse(population$in_c, 12 / 35, 0)

  frames <- stack_frames(
    a = overlap_component("in_a", 10, 1, population),
    b = overlap_component("in_b", 20, 2, population),
    c = overlap_component("in_c", 12, 3, population),
    membership = c(a = "in_a", b = "in_b", c = "in_c"),
    key = id,
    overlaps = declared_overlaps(
      a = "pi_a", b = "pi_b", c = "pi_c", scale = "probabilities"
    )
  )

  combined <- as_svrepdesign(
    frames, estimator = "expected", type = "bootstrap", replicates = 30
  )
  view <- as.data.frame(frames)
  by_hand <- 1 / (view$in_a * overlap_pi_a +
                    view$in_b * overlap_pi_b +
                    view$in_c * (12 / 35))

  expect_equal(unname(stats::weights(combined, "sampling")), by_hand)
  expect_equal(
    unname(coef(survey::svytotal(~y, combined))),
    sum(by_hand * view$y)
  )
})

## Validators

test_that("an own-frame value must be the selection that happened", {
  skip_if_not_installed("survey")
  # The one overlap samplyr can check against reality, because it computed
  # that component's weights itself.
  population <- overlap_population()
  population$pi_a <- ifelse(population$in_a, 0.9, 0)

  expect_error(
    overlap_fixture(population),
    class = "samplyr_error_stack_frames_overlaps"
  )
  expect_error(overlap_fixture(population), regexp = "0\\.9")
})

test_that("a declared scale is not checked against the values", {
  # Probabilities passed off as weights fail the range check rather than being
  # quietly reinterpreted, which is the whole point of declaring the scale.
  expect_error(
    overlap_fixture(
      overlaps = declared_overlaps(a = "pi_a", b = "pi_b", scale = "weights")
    ),
    class = "samplyr_error_stack_frames_overlaps"
  )
  expect_error(
    overlap_fixture(
      overlaps = declared_overlaps(
        a = "w_a", b = "w_b", scale = "probabilities"
      )
    ),
    class = "samplyr_error_stack_frames_overlaps"
  )
})

test_that("the range rule is checked on cross-frame columns too", {
  # The integration cases below reach the own-frame column, where the
  # diagonal check fires first and would mask this one. Called directly, only
  # the range rule can pass or fail.
  member <- c(TRUE, TRUE, FALSE)
  ok <- function(value, scale) {
    check_overlap_column(value, member, scale, "a", "b", "col")
  }
  bad <- function(value, scale) {
    expect_error(
      check_overlap_column(value, member, scale, "a", "b", "col"),
      class = "samplyr_error_stack_frames_overlaps"
    )
  }

  expect_null(ok(c(0.25, 1, 0), "probabilities"))
  bad(c(0.25, 1.0001, 0), "probabilities")
  bad(c(0.25, 0, 0), "probabilities")
  bad(c(0.25, -0.1, 0), "probabilities")
  bad(c(0.25, NA, 0), "probabilities")

  expect_null(ok(c(4, 1, 0), "weights"))
  bad(c(4, 0.999, 0), "weights")
  bad(c(4, Inf, 0), "weights")
  bad(c(4, NA, 0), "weights")

  # Outside the frame, only zero or NA.
  expect_null(ok(c(0.25, 1, NA), "probabilities"))
  bad(c(0.25, 1, 0.3), "probabilities")

  # A character column compares against a number without complaint in R, so
  # the range rule would "catch" this one for the wrong reason. The message is
  # what separates the two.
  expect_error(
    check_overlap_column(c("a", "b", "c"), member, "probabilities",
                         "a", "b", "col"),
    regexp = "must be numeric"
  )
})

test_that("a chance outside (0, 1] for a member is refused", {
  population <- overlap_population()
  population$pi_b[population$in_b][1:3] <- 1.4
  expect_error(
    overlap_fixture(population),
    class = "samplyr_error_stack_frames_overlaps"
  )

  zeroed <- overlap_population()
  zeroed$pi_b[zeroed$in_b][1:3] <- 0
  expect_error(
    overlap_fixture(zeroed),
    class = "samplyr_error_stack_frames_overlaps"
  )
})

test_that("a non-member may not carry a chance", {
  population <- overlap_population()
  population$pi_b[!population$in_b] <- 0.3
  expect_error(
    overlap_fixture(population),
    class = "samplyr_error_stack_frames_overlaps"
  )

  # NA is the other way of saying the quantity does not exist.
  missing <- overlap_population()
  missing$pi_b[!missing$in_b] <- NA_real_
  expect_s3_class(overlap_fixture(missing), "frame_stack")
})

test_that("every component must carry every overlap column", {
  population <- overlap_population()
  partial <- overlap_component("in_b", 20, 2, population)
  partial <- dplyr::select(partial, -"pi_a")

  expect_error(
    stack_frames(
      a = overlap_component("in_a", 10, 1, population),
      b = partial,
      membership = c(a = "in_a", b = "in_b"),
      key = id,
      overlaps = declared_overlaps(
      a = "pi_a", b = "pi_b", scale = "probabilities"
    )
    ),
    class = "samplyr_error_stack_frames_overlaps"
  )
})

test_that("the marker, its names and its columns are all checked", {
  population <- overlap_population()
  components <- list(
    a = overlap_component("in_a", 10, 1, population),
    b = overlap_component("in_b", 20, 2, population)
  )
  stack <- function(overlaps) {
    stack_frames(
      a = components$a, b = components$b,
      membership = c(a = "in_a", b = "in_b"), key = id,
      overlaps = overlaps
    )
  }

  expect_error(stack(list(a = "pi_a", b = "pi_b")),
               class = "samplyr_error_stack_frames_overlaps")
  expect_error(stack(c(a = "pi_a", b = "pi_b")),
               class = "samplyr_error_stack_frames_overlaps")
  expect_error(stack(declared_overlaps(a = "pi_a", scale = "probabilities")),
               class = "samplyr_error_stack_frames_overlaps")
  expect_error(
    stack(declared_overlaps(a = "pi_a", c = "pi_b", scale = "probabilities")),
               class = "samplyr_error_stack_frames_overlaps")
  expect_error(
    stack(declared_overlaps(a = "pi_a", b = "pi_a", scale = "probabilities")),
               class = "samplyr_error_stack_frames_overlaps")
  expect_error(
    stack(declared_overlaps("pi_a", "pi_b", scale = "probabilities")),
               class = "samplyr_error_stack_frames_overlaps")
})

test_that("a spec is a value, so it can be built before the call", {
  # Unlike `complete_links()` on the other feature, which names a column and
  # has to be evaluated in a data mask, this one carries strings and a scale.
  # Making it a value keeps it composable, as `membership` beside it is.
  spec <- declared_overlaps(a = "pi_a", b = "pi_b", scale = "probabilities")
  expect_s3_class(spec, "samplyr_overlap_spec")
  expect_identical(spec$scale, "probabilities")
  expect_identical(spec$cols, c(a = "pi_a", b = "pi_b"))
  expect_identical(
    declared_overlaps(a = "w_a", b = "w_b", scale = "weights")$scale, "weights"
  )

  frames <- overlap_fixture(overlaps = spec)
  expect_identical(attr(frames, "overlaps")$cols, c(a = "pi_a", b = "pi_b"))

  expect_output(print(spec), "Overlap probabilities")
})

test_that("a spec is stored in frame order, not the order it was written", {
  frames <- overlap_fixture(
    overlaps = declared_overlaps(
      b = "pi_b", a = "pi_a", scale = "probabilities"
    )
  )
  expect_identical(attr(frames, "overlaps")$cols, c(a = "pi_a", b = "pi_b"))
})

test_that("a misspelled overlaps argument is named", {
  expect_error(
    stack_frames(
      a = overlap_component("in_a", 10, 1),
      b = overlap_component("in_b", 20, 2),
      membership = c(a = "in_a", b = "in_b"),
      key = id,
      overlap = declared_overlaps(
        a = "pi_a", b = "pi_b", scale = "probabilities"
      )
    ),
    regexp = "Did you mean"
  )
})

## Refusals at export

test_that("the expected estimator needs overlaps", {
  skip_if_not_installed("survey")
  frames <- stack_frames(
    a = overlap_component("in_a", 10, 1),
    b = overlap_component("in_b", 20, 2),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  expect_error(
    as_svydesign(frames, estimator = "expected"),
    class = "samplyr_error_survey_multiframe_overlaps"
  )
  expect_error(
    as_svrepdesign(frames, estimator = "expected"),
    class = "samplyr_error_survey_multiframe_overlaps"
  )
})

test_that("theta is refused for the expected estimator, not discarded", {
  skip_if_not_installed("survey")
  frames <- overlap_fixture()
  expect_error(
    as_svydesign(frames, estimator = "expected", theta = 0.4),
    class = "samplyr_error_survey_multiframe_theta"
  )
  expect_error(
    as_svrepdesign(frames, estimator = "expected", theta = 0.4),
    class = "samplyr_error_survey_multiframe_theta"
  )
})

test_that("overlaps supplied with the constant estimator are simply unused", {
  skip_if_not_installed("survey")
  frames <- overlap_fixture()
  bare <- stack_frames(
    a = frames[["a"]], b = frames[["b"]],
    membership = c(a = "in_a", b = "in_b"), key = id
  )

  expect_equal(
    coef(survey::svytotal(~y, as_svydesign(frames))),
    coef(survey::svytotal(~y, as_svydesign(bare)))
  )
})

## A shared-weight component

test_that("a shared weight cannot stand as an inclusion probability", {
  skip_if_not_installed("survey")
  dwellings <- data.frame(dwelling_id = 1:20)
  source_sample <- sampling_design() |>
    draw(n = 8) |>
    execute(dwellings, seed = 11)

  people <- data.frame(
    person_id = 1:20,
    dwelling_id = 1:20,
    y = as.numeric(1:20),
    in_reached = TRUE,
    in_list = rep(c(TRUE, FALSE), times = 10)
  )
  people$pi_reached <- 8 / 20
  people$pi_list <- ifelse(people$in_list, 0.5, 0)
  links <- data.frame(dwelling_id = 1:20, person_id = 1:20)

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
    draw(n = 5) |>
    execute(people[people$in_list, , drop = FALSE], seed = 12)

  # Refused where the values would be recorded ...
  expect_error(
    stack_frames(
      reached = shared, list = listed,
      membership = c(reached = "in_reached", list = "in_list"),
      key = person_id,
      overlaps = declared_overlaps(
        reached = "pi_reached", list = "pi_list",
    scale = "probabilities")
    ),
    class = "samplyr_error_stack_frames_overlaps"
  )

  # ... and again where the estimator would use them, so the message names
  # the estimator rather than the missing record.
  frames <- stack_frames(
    reached = shared, list = listed,
    membership = c(reached = "in_reached", list = "in_list"),
    key = person_id
  )
  expect_error(
    as_svrepdesign(frames, estimator = "expected"),
    class = "samplyr_error_survey_weight_contract"
  )
  expect_error(
    as_svrepdesign(frames, estimator = "expected"),
    regexp = "unbiasedness"
  )
  # The constant estimator still takes it, which F3 established.
  expect_s3_class(
    suppressWarnings(
      as_svrepdesign(frames, type = "bootstrap", replicates = 20)
    ),
    "svyrep.design"
  )
})

## One declaring constructor, and the scale it makes you state

test_that("declared_overlaps() requires the scale and matches it exactly", {
  # `overlap_probabilities()` and `overlap_weights()` said the scale in the
  # verb. One constructor says it in an argument, which has to be as hard to
  # omit as the verb was.
  expect_error(
    declared_overlaps(a = "pi_a", b = "pi_b"),
    class = "samplyr_error_stack_frames_overlaps"
  )
  expect_error(
    declared_overlaps(a = "pi_a", b = "pi_b"),
    regexp = "never inferred from the values"
  )
  expect_error(
    declared_overlaps(a = "pi_a", b = "pi_b", scale = "chances"),
    class = "samplyr_error_stack_frames_overlaps"
  )
  expect_error(
    declared_overlaps(a = "pi_a", b = "pi_b", scale = c("weights", "weights")),
    class = "samplyr_error_stack_frames_overlaps"
  )

  # `scale` follows the dots, so it is matched by exact name and never
  # mistaken for a frame called `scal`.
  expect_error(
    declared_overlaps(a = "pi_a", scal = "probabilities"),
    class = "samplyr_error_stack_frames_overlaps"
  )

  both <- lapply(c("probabilities", "weights"), function(s) {
    declared_overlaps(a = "x", b = "y", scale = s)
  })
  expect_identical(both[[1]]$scale, "probabilities")
  expect_identical(both[[2]]$scale, "weights")
  expect_identical(both[[1]]$cols, both[[2]]$cols)
  for (spec in both) expect_s3_class(spec, "samplyr_overlap_spec")
})

test_that("the two removed constructors are gone", {
  for (name in c("overlap_probabilities", "overlap_weights")) {
    expect_false(name %in% getNamespaceExports("samplyr"))
  }
  # And the message that lists the accepted forms names what exists.
  bad <- tryCatch(
    overlap_fixture(overlaps = list(scale = "probabilities", cols = c(a = "pi_a"))),
    error = identity
  )
  expect_match(conditionMessage(bad), "declared_overlaps()", fixed = TRUE)
  expect_match(conditionMessage(bad), "exante_overlaps()", fixed = TRUE)
  expect_no_match(conditionMessage(bad), "overlap_probabilities", fixed = TRUE)
})
