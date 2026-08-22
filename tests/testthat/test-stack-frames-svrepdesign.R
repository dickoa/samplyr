## F3. as_svrepdesign.frame_stack(): the block replicate composition

# The components are chosen so that `type = "auto"` gives them DIFFERENT
# methods with DIFFERENT scales, and both deterministically: a stratified
# frame takes JKn, whose scale is 1 and whose factors live in `rscales`, and
# an unstratified frame takes JK1, whose scale is 0.475. A fixture where both
# scales were 1 could not tell a design that folds each component's scale into
# its rscales from one that drops it, which is the same trap the G3 fixture
# hit from the other side.
#
#   population   1 .. 20 .. 40 .. 60
#   frame a      x     x                stratified, 4 strata, n = 2 each
#   frame b            x     x          srswor, n = 20
#
# The compositing factors are written out from the membership columns rather
# than taken from samplyr, so the oracle does not read the code it checks.

svrep_population <- function() {
  data.frame(
    id = 1:60,
    y = as.numeric(1:60),
    st = rep(c("x", "y", "z", "w"), length.out = 60),
    size = rep(c(2, 5, 3, 8), length.out = 60),
    in_a = rep(c(TRUE, FALSE), times = c(40, 20)),
    in_b = rep(c(FALSE, TRUE), times = c(20, 40))
  )
}

svrep_fixture <- function(population = svrep_population()) {
  stack_frames(
    a = sampling_design() |>
      stratify_by(st) |>
      draw(n = 2) |>
      execute(population[population$in_a, , drop = FALSE], seed = 1),
    b = sampling_design() |>
      draw(n = 20) |>
      execute(population[population$in_b, , drop = FALSE], seed = 2),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )
}

# The multiplicity factor, written from the membership columns.
svrep_factor <- function(component, cols = c("in_a", "in_b")) {
  1 / rowSums(vapply(cols, function(col) {
    as.numeric(component[[col]])
  }, numeric(nrow(component))))
}

# What one frame's block would compute on its own, which is the quantity the
# combined design has to add up.
svrep_component_variance <- function(component, factor, type = "auto") {
  rep_design <- suppressWarnings(
    as_svrepdesign(component, type = type, mse = TRUE)
  )
  design <- survey::svrepdesign(
    data = as.data.frame(component),
    repweights = stats::weights(rep_design, "analysis") * factor,
    weights = stats::weights(rep_design, "sampling") * factor,
    combined.weights = TRUE,
    type = "other",
    scale = rep_design$scale,
    rscales = rep_design$rscales,
    mse = TRUE
  )
  survey::SE(survey::svytotal(~y, design))^2
}

## Agreement with the linearized route

test_that("the multiplicity point estimate matches the linearized route", {
  skip_if_not_installed("survey")
  frames <- svrep_fixture()

  replicate_total <- coef(survey::svytotal(~y, as_svrepdesign(frames)))
  linearized_total <- coef(survey::svytotal(~y, as_svydesign(frames)))

  expect_equal(replicate_total, linearized_total)
})

test_that("an explicit theta matches the linearized route as well", {
  skip_if_not_installed("survey")
  frames <- svrep_fixture()

  expect_equal(
    coef(survey::svytotal(~y, as_svrepdesign(frames, theta = 0.3))),
    coef(survey::svytotal(~y, as_svydesign(frames, theta = 0.3)))
  )
})

test_that("three frames composite by multiplicity against a hand total", {
  skip_if_not_installed("survey")
  population <- svrep_population()
  population$in_c <- rep(c(TRUE, FALSE, TRUE), times = c(15, 25, 20))

  frames <- stack_frames(
    a = sampling_design() |>
      draw(n = 10) |>
      execute(population[population$in_a, , drop = FALSE], seed = 1),
    b = sampling_design() |>
      draw(n = 20) |>
      execute(population[population$in_b, , drop = FALSE], seed = 2),
    c = sampling_design() |>
      draw(n = 12) |>
      execute(population[population$in_c, , drop = FALSE], seed = 3),
    membership = c(a = "in_a", b = "in_b", c = "in_c"),
    key = id
  )

  by_hand <- sum(vapply(names(frames), function(nm) {
    component <- frames[[nm]]
    factor <- svrep_factor(component, c("in_a", "in_b", "in_c"))
    sum(component$.weight * factor * component$y)
  }, numeric(1)))

  expect_equal(
    unname(coef(survey::svytotal(~y, as_svrepdesign(frames)))),
    by_hand
  )
})

## The block structure

test_that("the combined variance is the sum of the frames' own", {
  skip_if_not_installed("survey")
  # This is what independent selection from each frame buys, and it is the
  # reason the blocks are built the way they are.
  frames <- svrep_fixture()
  combined <- as_svrepdesign(frames)

  total_variance <- survey::SE(survey::svytotal(~y, combined))^2
  parts <- vapply(names(frames), function(nm) {
    svrep_component_variance(frames[[nm]], svrep_factor(frames[[nm]]))
  }, numeric(1))

  expect_equal(total_variance, sum(parts))
  expect_true(all(parts > 0))
})

test_that("each frame's block leaves every other frame at full sample", {
  skip_if_not_installed("survey")
  frames <- svrep_fixture()
  combined <- as_svrepdesign(frames)

  analysis <- stats::weights(combined, "analysis")
  base <- stats::weights(combined, "sampling")
  view <- as.data.frame(frames)
  rows_a <- which(view$.frame == "a")
  rows_b <- which(view$.frame == "b")

  widths <- attr(combined, "samplyr_frame_stack")$replicates
  cols_a <- seq_len(widths[["a"]])
  cols_b <- seq.int(widths[["a"]] + 1L, widths[["a"]] + widths[["b"]])

  # In frame a's columns, frame b's rows never move, and the other way round.
  expect_true(all(analysis[rows_b, cols_a] == base[rows_b]))
  expect_true(all(analysis[rows_a, cols_b] == base[rows_a]))
  # And each frame does move in its own block, or the design measures nothing.
  expect_false(all(analysis[rows_a, cols_a] == base[rows_a]))
  expect_false(all(analysis[rows_b, cols_b] == base[rows_b]))
})

test_that("each component's scale is folded into the combined rscales", {
  skip_if_not_installed("survey")
  # A jackknife leaves `scale` at 1 and carries its factors in `rscales`,
  # while JK1 here carries 0.475 in `scale`. Keeping only one of the two is
  # undetectable under the first and wrong under the second.
  frames <- svrep_fixture()
  combined <- as_svrepdesign(frames)

  components <- lapply(names(frames), function(nm) {
    suppressWarnings(as_svrepdesign(frames[[nm]], type = "auto", mse = TRUE))
  })
  expect_identical(components[[1]]$scale, 1)
  expect_false(identical(components[[2]]$scale, 1))

  expect_identical(combined$scale, 1)
  expect_equal(
    combined$rscales,
    unlist(lapply(components, function(r) r$scale * r$rscales),
           use.names = FALSE)
  )
  expect_true(combined$mse)
})

## Heterogeneous methods

test_that("frames may take different replicate methods", {
  skip_if_not_installed("survey")
  frames <- svrep_fixture()

  types <- vapply(names(frames), function(nm) {
    suppressWarnings(as_svrepdesign(frames[[nm]], type = "auto"))$type
  }, character(1))
  expect_identical(unname(types), c("JKn", "JK1"))

  # Mixing them costs nothing: the blocks do not interact.
  combined <- as_svrepdesign(frames)
  parts <- vapply(names(frames), function(nm) {
    svrep_component_variance(frames[[nm]], svrep_factor(frames[[nm]]))
  }, numeric(1))
  expect_equal(survey::SE(survey::svytotal(~y, combined))^2, sum(parts))
})

test_that("a PPS frame needs a type its own design supports", {
  skip_if_not_installed("survey")
  population <- svrep_population()
  frames <- stack_frames(
    a = sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size) |>
      execute(population[population$in_a, , drop = FALSE], seed = 4),
    b = sampling_design() |>
      draw(n = 20) |>
      execute(population[population$in_b, , drop = FALSE], seed = 2),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  # `type` is one choice for the whole stack, so a PPS frame moves every
  # frame onto a method that supports it. The per-component export says so
  # before it fails.
  expect_warning(
    expect_error(as_svrepdesign(frames), class = "samplyr_error"),
    regexp = "unequal-probability"
  )
  expect_s3_class(
    suppressWarnings(as_svrepdesign(frames, type = "subbootstrap")),
    "svyrep.design"
  )
})

## The returned object

test_that("the data carries the composited weight and the frame labels", {
  skip_if_not_installed("survey")
  frames <- svrep_fixture()
  combined <- as_svrepdesign(frames)

  expected <- unlist(lapply(names(frames), function(nm) {
    frames[[nm]]$.weight * svrep_factor(frames[[nm]])
  }), use.names = FALSE)

  expect_equal(unname(stats::weights(combined, "sampling")), expected)
  expect_equal(combined$variables$.weight, expected)
  expect_true(all(c(".frame", ".domain") %in% names(combined$variables)))
  expect_identical(
    combined$variables$.frame,
    c(rep("a", nrow(frames[["a"]])), rep("b", nrow(frames[["b"]])))
  )
})

test_that("the stack is recorded on the returned design", {
  skip_if_not_installed("survey")
  combined <- as_svrepdesign(svrep_fixture())
  record <- attr(combined, "samplyr_frame_stack")

  expect_identical(record$frames, c("a", "b"))
  expect_identical(
    record$replicates,
    c(a = 8L, b = 20L)
  )
  expect_identical(
    sum(record$replicates),
    ncol(stats::weights(combined, "analysis"))
  )
})

## A shared-weight component

test_that("a component whose weights were shared composites here", {
  skip_if_not_installed("survey")
  # The linearized route refuses this; the replicate route is where it works,
  # because the per-component export already applies the link operator inside
  # every replicate before the blocks are built.
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

  frames <- stack_frames(
    reached = shared,
    list = listed,
    membership = c(reached = "in_reached", list = "in_list"),
    key = person_id
  )

  combined <- suppressWarnings(
    as_svrepdesign(frames, type = "bootstrap", replicates = 30)
  )
  expect_s3_class(combined, "svyrep.design")

  # The point estimate is the composited total and owes nothing to the
  # replication, so it is checkable exactly.
  by_hand <- sum(vapply(names(frames), function(nm) {
    component <- frames[[nm]]
    factor <- svrep_factor(component, c("in_reached", "in_list"))
    sum(component$.weight * factor * component$y)
  }, numeric(1)))
  expect_equal(
    unname(coef(survey::svytotal(~y, combined))),
    by_hand
  )

  # The transformation each component carries is recorded, per frame.
  record <- attr(combined, "samplyr_weight_share")
  expect_identical(names(record), c("reached", "list"))
  expect_identical(record$reached$algorithm, "generalized_weight_share")
  expect_null(record$list)
})

## Refusals and forwarding

test_that("an explicit theta above two frames is refused", {
  skip_if_not_installed("survey")
  population <- svrep_population()
  population$in_c <- rep(c(TRUE, FALSE, TRUE), times = c(15, 25, 20))
  frames <- stack_frames(
    a = sampling_design() |>
      draw(n = 10) |>
      execute(population[population$in_a, , drop = FALSE], seed = 1),
    b = sampling_design() |>
      draw(n = 20) |>
      execute(population[population$in_b, , drop = FALSE], seed = 2),
    c = sampling_design() |>
      draw(n = 12) |>
      execute(population[population$in_c, , drop = FALSE], seed = 3),
    membership = c(a = "in_a", b = "in_b", c = "in_c"),
    key = id
  )

  # Three frames are fine by multiplicity: the refusal is about theta alone.
  expect_s3_class(as_svrepdesign(frames), "svyrep.design")
  expect_error(
    as_svrepdesign(frames, theta = 0.4),
    class = "samplyr_error_survey_multiframe_theta"
  )
  expect_error(as_svrepdesign(frames, theta = 0.4), regexp = "per domain")
})

test_that("svrepdesign theta is scalar and in the unit interval", {
  skip_if_not_installed("survey")
  frames <- svrep_fixture()
  for (bad in list(2, -0.1, c(0.3, 0.7), NA_real_, "0.5")) {
    expect_error(
      as_svrepdesign(frames, theta = bad),
      class = "samplyr_error_survey_multiframe_theta"
    )
  }
})

test_that("mse is refused, because the blocks share one center", {
  skip_if_not_installed("survey")
  expect_error(
    as_svrepdesign(svrep_fixture(), mse = FALSE),
    class = "samplyr_error_survey_multiframe_argument"
  )
  expect_error(
    as_svrepdesign(svrep_fixture(), mse = TRUE),
    class = "samplyr_error_survey_multiframe_argument"
  )
})

test_that("a two-phase component is refused, and the frame is named", {
  skip_if_not_installed("survey")
  population <- svrep_population()
  phase1 <- sampling_design() |>
    draw(n = 30) |>
    execute(population[population$in_a, , drop = FALSE], seed = 5)
  phase2 <- sampling_design() |>
    draw(n = 10) |>
    execute(phase1, seed = 6)

  frames <- stack_frames(
    a = phase2,
    b = sampling_design() |>
      draw(n = 20) |>
      execute(population[population$in_b, , drop = FALSE], seed = 2),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )

  expect_error(
    as_svrepdesign(frames),
    class = "samplyr_error_svrep_twophase_unsupported"
  )
  # The per-component export raises the same class, so what separates the two
  # is that this one says which frame.
  expect_error(as_svrepdesign(frames), regexp = "\"a\"")
})

test_that("forwarded arguments reach the components and strays are refused", {
  skip_if_not_installed("survey")
  frames <- svrep_fixture()

  combined <- suppressWarnings(
    as_svrepdesign(frames, type = "bootstrap", replicates = 25)
  )
  expect_identical(
    ncol(stats::weights(combined, "analysis")),
    50L
  )
  expect_identical(
    attr(combined, "samplyr_frame_stack")$replicates,
    c(a = 25L, b = 25L)
  )

  expect_error(
    as_svrepdesign(frames, nonsense = 1),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    as_svrepdesign(frames, design = 1),
    class = "samplyr_error_derived_argument"
  )
})
