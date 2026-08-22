## F2. as_svydesign.frame_stack(): the linearized dual-frame export

# The frames take UNEQUAL sampling fractions on purpose. survey's own default
# for a missing `theta` is the ratio of the frames' mean sampling weights, and
# with equal fractions that ratio is exactly one half: the fixture would then
# agree with the default it is meant to distinguish samplyr's resolution from,
# and forwarding NULL would pass every test.
#
#   population   1 .. 20 .. 40 .. 60
#   frame a      x     x
#   frame b            x     x         overlap = units 21 to 40
#
# Frame a takes 10 of 40, frame b takes 20 of 40, so the design weights are 4
# and 2 and survey's default factor is 2/3 rather than 1/2.

multiframe_population <- function() {
  data.frame(
    id = 1:60,
    y = as.numeric(1:60),
    in_a = rep(c(TRUE, FALSE), times = c(40, 20)),
    in_b = rep(c(FALSE, TRUE), times = c(20, 40))
  )
}

multiframe_component <- function(col, n, seed,
                                 population = multiframe_population()) {
  sampling_design() |>
    draw(n = n) |>
    execute(population[population[[col]], , drop = FALSE], seed = seed)
}

multiframe_fixture <- function(n_a = 10, n_b = 20) {
  stack_frames(
    a = multiframe_component("in_a", n_a, 1),
    b = multiframe_component("in_b", n_b, 2),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )
}

# The same call written by hand against survey, which is the oracle: samplyr
# is doing the wiring, not the estimation.
hand_built_multiframe <- function(frames, theta) {
  designs <- lapply(names(frames), function(nm) as_svydesign(frames[[nm]]))
  membership <- attr(frames, "membership")
  overlaps <- lapply(names(frames), function(nm) {
    component <- frames[[nm]]
    do.call(cbind, lapply(unname(membership), function(col) {
      as.numeric(component[[col]])
    }))
  })
  survey::multiframe(designs, overlaps, estimator = "constant", theta = theta)
}

## Agreement with survey

test_that("the export equals the same multiframe() call written by hand", {
  skip_if_not_installed("survey")
  frames <- multiframe_fixture()

  ours <- survey::svytotal(~y, as_svydesign(frames, theta = 0.4))
  theirs <- survey::svytotal(~y, hand_built_multiframe(frames, theta = 0.4))

  expect_equal(coef(ours), coef(theirs))
  expect_equal(survey::SE(ours), survey::SE(theirs))
  expect_identical(
    survey::degf(as_svydesign(frames)),
    survey::degf(hand_built_multiframe(frames, theta = 0.5))
  )
})

test_that("the export returns survey's dual-frame object", {
  skip_if_not_installed("survey")
  svy <- as_svydesign(multiframe_fixture())

  expect_s3_class(svy, "dualframe")
  expect_s3_class(svy, "multiframe")
  expect_length(svy$designs, 2L)
})

test_that("the components keep their own strata and clusters", {
  skip_if_not_installed("survey")
  population <- multiframe_population()
  population$stratum <- rep(c("x", "y"), length.out = 60)

  stratified <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = c(x = 6, y = 4)) |>
    execute(population[population$in_a, , drop = FALSE], seed = 3)

  frames <- stack_frames(
    a = stratified,
    b = multiframe_component("in_b", 20, 2, population),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  svy <- as_svydesign(frames)
  expect_false(is.null(svy$designs[[1]]$strata))
  expect_equal(
    coef(survey::svytotal(~y, svy)),
    coef(survey::svytotal(~y, hand_built_multiframe(frames, theta = 0.5)))
  )
})

## theta

test_that("theta = NULL is the multiplicity estimator, not survey's default", {
  skip_if_not_installed("survey")
  frames <- multiframe_fixture()

  ours <- coef(survey::svytotal(~y, as_svydesign(frames)))
  half <- coef(survey::svytotal(~y, hand_built_multiframe(frames, 0.5)))
  # survey reads a missing theta as mean_weights / sum(mean_weights), which is
  # 4 / (4 + 2) here. Pin the number so a regression to forwarding NULL fails.
  survey_default <- coef(
    survey::svytotal(~y, hand_built_multiframe(frames, theta = NULL))
  )

  expect_equal(ours, half)
  expect_false(isTRUE(all.equal(ours, survey_default)))
})

test_that("theta belongs to the first frame of the stack", {
  skip_if_not_installed("survey")
  frames <- multiframe_fixture()
  reversed <- stack_frames(
    b = frames[["b"]],
    a = frames[["a"]],
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  forward <- survey::svytotal(~y, as_svydesign(frames, theta = 0.3))
  # Reversing the stack and asking for the same estimator means 1 - theta.
  backward <- survey::svytotal(~y, as_svydesign(reversed, theta = 0.7))
  expect_equal(coef(forward), coef(backward))
  expect_equal(survey::SE(forward), survey::SE(backward))

  # And 0.5 is the one value that does not care about the order.
  expect_equal(
    coef(survey::svytotal(~y, as_svydesign(frames))),
    coef(survey::svytotal(~y, as_svydesign(reversed)))
  )
  expect_false(isTRUE(all.equal(
    coef(forward),
    coef(survey::svytotal(~y, as_svydesign(reversed, theta = 0.3)))
  )))
})

test_that("the overlaps columns follow the stack order, not the mapping", {
  skip_if_not_installed("survey")
  # survey reads `overlaps[[f]][, 3 - f]`, the other frame's column by
  # position. A matrix built in the order the membership mapping was written
  # rather than in the order the frames are stacked produces a number, not an
  # error, so the encoding is asserted directly.
  frames <- stack_frames(
    b = multiframe_component("in_b", 20, 2),
    a = multiframe_component("in_a", 10, 1),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  overlaps <- multiframe_overlaps(frames)
  expect_identical(names(overlaps), c("b", "a"))
  expect_identical(colnames(overlaps[[1]]), c("b", "a"))
  # Every unit of a component is in its own frame, which is column 1 here.
  expect_true(all(overlaps[[1]][, 1] == 1))
  expect_true(all(overlaps[[2]][, 2] == 1))
  expect_identical(
    overlaps[[1]][, 2],
    as.numeric(frames[["b"]]$in_a)
  )
})

test_that("svydesign theta is scalar and in the unit interval", {
  skip_if_not_installed("survey")
  frames <- multiframe_fixture()

  for (bad in list(2, -0.1, c(0.3, 0.7), NA_real_, Inf, "0.5")) {
    expect_error(
      as_svydesign(frames, theta = bad),
      class = "samplyr_error_survey_multiframe_theta"
    )
  }
  expect_s3_class(as_svydesign(frames, theta = 0), "dualframe")
  expect_s3_class(as_svydesign(frames, theta = 1), "dualframe")
})

## Refusals

test_that("more than two frames is refused at the export, not the stack", {
  skip_if_not_installed("survey")
  population <- multiframe_population()
  population$in_c <- rep(c(TRUE, FALSE, TRUE), times = c(10, 30, 20))

  frames <- stack_frames(
    a = multiframe_component("in_a", 10, 1, population),
    b = multiframe_component("in_b", 20, 2, population),
    c = multiframe_component("in_c", 10, 3, population),
    membership = c(a = "in_a", b = "in_b", c = "in_c"),
    key = id
  )

  # The stack itself is fine: the ceiling belongs to survey's estimator.
  expect_s3_class(frames, "frame_stack")
  expect_error(
    as_svydesign(frames),
    class = "samplyr_error_survey_multiframe_unsupported"
  )
})

test_that("a two-phase component is refused before survey sees it", {
  skip_if_not_installed("survey")
  population <- multiframe_population()
  phase1 <- sampling_design() |>
    draw(n = 30) |>
    execute(population[population$in_a, , drop = FALSE], seed = 5)
  phase2 <- sampling_design() |>
    draw(n = 10) |>
    execute(phase1, seed = 6)

  frames <- stack_frames(
    a = phase2,
    b = multiframe_component("in_b", 20, 2, population),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  expect_error(
    as_svydesign(frames),
    class = "samplyr_error_survey_multiframe_unsupported"
  )
  expect_error(as_svydesign(frames), regexp = "two-phase")
})

test_that("a shared-weight component is refused on the linearized route", {
  skip_if_not_installed("survey")
  dwellings <- data.frame(dwelling_id = 1:12)
  source_sample <- sampling_design() |>
    draw(n = 6) |>
    execute(dwellings, seed = 11)

  people <- data.frame(
    person_id = 1:12,
    dwelling_id = 1:12,
    y = as.numeric(1:12),
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
    draw(n = 4) |>
    execute(people[people$in_list, , drop = FALSE], seed = 12)

  frames <- stack_frames(
    reached = shared,
    list = listed,
    membership = c(reached = "in_reached", list = "in_list"),
    key = person_id
  )

  expect_error(
    as_svydesign(frames),
    class = "samplyr_error_survey_weight_contract"
  )
  # The per-component export raises the same class, so the message is what
  # separates the two: this one names the frame, and it does not send the
  # caller to a stack method that does not exist.
  expect_error(as_svydesign(frames), regexp = "reached")
  expect_error(
    as_svydesign(frames),
    regexp = "frames\\[\\[\"<frame name>\"\\]\\]"
  )
})

## Forwarded arguments

test_that("pps is refused, because it describes one component", {
  skip_if_not_installed("survey")
  # Forwarding one joint-probability matrix to both components would compute
  # a variance from the wrong probabilities rather than raise an error.
  expect_error(
    as_svydesign(multiframe_fixture(), pps = "anything"),
    class = "samplyr_error_survey_multiframe_argument"
  )
})

test_that("method is refused, since no component may be two-phase", {
  skip_if_not_installed("survey")
  expect_error(
    as_svydesign(multiframe_fixture(), method = "full"),
    class = "samplyr_error_survey_multiframe_argument"
  )
})

test_that("an argument neither samplyr nor survey owns is refused by name", {
  skip_if_not_installed("survey")
  expect_error(
    as_svydesign(multiframe_fixture(), nonsense = 1),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    as_svydesign(multiframe_fixture(), data = data.frame()),
    class = "samplyr_error_derived_argument"
  )
})

test_that("an accepted survey argument reaches every component", {
  skip_if_not_installed("survey")
  frames <- multiframe_fixture()
  svy <- as_svydesign(frames, variables = ~y)

  expect_identical(names(svy$designs[[1]]$variables), "y")
  expect_identical(names(svy$designs[[2]]$variables), "y")
})

test_that("nest reaches every component", {
  skip_if_not_installed("survey")
  # `nest` cannot change a samplyr export's numbers: a cluster spanning two
  # strata is refused at `execute()`, so every design reaching survey is
  # already nested. What is checkable is that the value arrives, and each
  # component design records the call it was built with.
  frames <- multiframe_fixture()

  unnested <- as_svydesign(frames, nest = FALSE)
  expect_false(unnested$designs[[1]]$call$nest)
  expect_false(unnested$designs[[2]]$call$nest)

  nested <- as_svydesign(frames)
  expect_true(nested$designs[[1]]$call$nest)
  expect_true(nested$designs[[2]]$call$nest)
})

## Existing diagnostics survive the composition

test_that("a systematic component still reports its approximation", {
  skip_if_not_installed("survey")
  population <- multiframe_population()
  systematic <- sampling_design() |>
    draw(n = 10, method = "systematic") |>
    execute(population[population$in_a, , drop = FALSE], seed = 7)

  frames <- stack_frames(
    a = systematic,
    b = multiframe_component("in_b", 20, 2, population),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  expect_warning(
    as_svydesign(frames),
    class = "samplyr_warning_systematic_variance"
  )
  expect_error(
    as_svydesign(frames, systematic_variance = "error"),
    class = "samplyr_error_systematic_variance"
  )
  expect_s3_class(
    as_svydesign(frames, systematic_variance = "approximate"),
    "dualframe"
  )
})

test_that("a PPS component composites through the export", {
  skip_if_not_installed("survey")
  population <- multiframe_population()
  population$size <- rep(c(2, 5, 3, 8), length.out = 60)

  pps <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = size) |>
    execute(population[population$in_a, , drop = FALSE], seed = 8)

  frames <- stack_frames(
    a = pps,
    b = multiframe_component("in_b", 20, 2, population),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  expect_equal(
    coef(survey::svytotal(~y, as_svydesign(frames))),
    coef(survey::svytotal(~y, hand_built_multiframe(frames, theta = 0.5)))
  )
})
