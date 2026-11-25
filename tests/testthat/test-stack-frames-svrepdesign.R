## as_svrepdesign.frame_stack(): the block replicate composition

# Frame a (units 1 to 40, 4 strata of n = 2) takes JKn with scale 1, frame b
# (units 21 to 60, n = 20) takes JK1 with scale 0.475. The oracle writes the
# compositing factors from the membership columns, not from samplyr.

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
  # The frames are selected independently, so their variances add.
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
  # JKn carries its factors in `rscales`, and JK1 carries 0.475 in `scale`.
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

  # The default type takes the PPS first stage as drawn with replacement.
  expect_warning(
    expect_s3_class(as_svrepdesign(frames), "svyrep.design"),
    class = "samplyr_warning_replicate_wr_first_stage"
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
  # Each component applies the link operator inside every replicate.
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

  # The point estimate does not depend on the replicates, so it is exact.
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
  # The per-component export raises the same class, so match the frame name.
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

test_that("a frame with an empty primary unit keeps its zero row", {
  skip_if_not_installed("survey")
  # PSU 4 of frame a has no households, and seed 1 selects it. Its export
  # adds a zero row, which the blocks were sized without.
  psus <- data.frame(psu = 1:4)
  hh <- data.frame(
    psu = rep(1:3, each = 3), id = 1:9, in_a = TRUE,
    in_b = rep(c(TRUE, FALSE, FALSE), 3), pi_a = 0.5,
    pi_b = rep(c(4 / 9, 0, 0), 3), y = as.numeric(1:9)
  )
  b_frame <- data.frame(
    id = c(1, 4, 7, 10:15), in_a = c(rep(TRUE, 3), rep(FALSE, 6)),
    in_b = TRUE, pi_a = c(0.5, 0.5, 0.5, rep(0, 6)), pi_b = 4 / 9,
    y = as.numeric(11:19)
  )
  a <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> draw(n = 2, on_empty = "silent") |>
    execute(psus, hh, seed = 1)
  expect_identical(attr(a, "metadata")$empty_parents[[1]]$rows$psu, 4L)
  b <- sampling_design() |> draw(n = 4) |> execute(b_frame, seed = 2)
  stacks <- list(
    constant = stack_frames(a = a, b = b, membership = c(a = "in_a",
                            b = "in_b"), key = id),
    expected = stack_frames(a = a, b = b, membership = c(a = "in_a",
                            b = "in_b"), key = id,
                            overlaps = declared_overlaps(a = "pi_a",
                              b = "pi_b", scale = "probabilities"))
  )
  for (estimator in names(stacks)) {
    frames <- stacks[[estimator]]
    # JK1 warns that it applies the first stage's correction to both.
    combined <- suppressWarnings(
      as_svrepdesign(frames, type = "JK1", estimator = estimator)
    )
    # Each frame's own export, its rows composited by the oracle's factor:
    # an equal share on the overlap, or one over the summed chances.
    parts <- lapply(c("a", "b"), function(nm) {
      own <- suppressWarnings(
        as_svrepdesign(frames[[nm]], type = "JK1", mse = TRUE)
      )
      rows <- own$variables
      real <- !is.na(rows$.sample_id)
      factor <- rep(1, nrow(rows))
      factor[real] <- if (estimator == "constant") {
        ifelse(rows$in_a[real] & rows$in_b[real], 0.5, 1)
      } else {
        1 / (rows$pi_a[real] + rows$pi_b[real]) /
          stats::weights(own, "sampling")[real]
      }
      design <- survey::svrepdesign(
        data = rows,
        repweights = stats::weights(own, "analysis") * factor,
        weights = stats::weights(own, "sampling") * factor,
        combined.weights = TRUE, type = "other", scale = own$scale,
        rscales = own$rscales, mse = TRUE
      )
      survey::svytotal(~y, design, na.rm = TRUE)
    })
    est <- survey::svytotal(~y, combined, na.rm = TRUE)
    expect_equal(unname(coef(est)), sum(vapply(parts, coef, 1)),
                 info = estimator)
    expect_equal(unname(survey::SE(est))^2,
                 sum(vapply(parts, function(p) survey::SE(p)^2, 1)),
                 info = estimator)
    zero <- combined$variables[is.na(combined$variables$.sample_id), ]
    expect_identical(nrow(zero), 1L)
    expect_identical(zero$psu, 4L)
    expect_identical(zero$.frame, "a")
    expect_identical(zero$.weight, 0)
    expect_identical(c(zero$in_a, zero$in_b), c(TRUE, FALSE))
  }
  # The linearized route reads one probability per stage, which a
  # multistage export does not carry.
  expect_error(as_svydesign(stacks$constant),
               class = "samplyr_error_survey_multiframe_stages")
})

test_that("a settled reserve's dropped units composite with their membership", {
  skip_if_not_installed("survey")
  # Frame A lists units 1 to 8 with a reserve, frame B units 5 to 12, and
  # units 1, 2, 5, 6 and 9 are ineligible. The rows A's export adds for its
  # ineligible units held no membership and took factor 1 in A, which
  # counted each one of the overlap that A found ineligible twice: over
  # 2,000 samples 6.00 ineligible units for 5, against 5.00 with each unit's
  # own membership.
  ineligible <- c(1, 2, 5, 6, 9)
  frame_a <- data.frame(id = 1:8, st = "s", in_A = TRUE, in_B = 1:8 >= 5,
                        y = 1:8)
  frame_a$eligible <- !frame_a$id %in% ineligible
  frame_b <- data.frame(id = 5:12, in_A = 5:12 <= 8, in_B = TRUE, y = 5:12)
  frame_b$eligible <- !frame_b$id %in% ineligible
  listed <- execute(sampling_design() |> stratify_by(st) |>
                      draw(n = 4, reserve = 2), frame_a, seed = 1)
  # Released in rank order until four interviews.
  listed$disp <- ifelse(listed$eligible, "interview", "ineligible")
  got <- sum(listed$disp[listed$.reserve_1 == 0L] == "interview")
  for (r in 1:2) {
    at <- listed$.reserve_1 == r
    if (got >= 4) listed$disp[at] <- NA else
      got <- got + sum(listed$disp[at] == "interview")
  }
  dropped <- as.data.frame(listed)[listed$disp %in% "ineligible", ]
  expect_true(any(dropped$in_B) && any(!dropped$in_B))
  settled <- suppressWarnings(execute(listed, response_status = disp))
  b <- execute(sampling_design() |> draw(n = 4), frame_b, seed = 1001)
  b$disp <- ifelse(b$eligible, "interview", "ineligible")
  stack <- stack_frames(A = settled, B = b, key = id,
                        membership = c(A = "in_A", B = "in_B"))
  replicate <- suppressWarnings(as_svrepdesign(stack, type = "auto"))
  replicate <- update(replicate, counted = disp %in% "ineligible")
  # By hand, with the multiplicity factor 1/2 on the overlap.
  exported <- suppressWarnings(as_svydesign(settled))$variables
  weight <- unique(exported$.weight[is.na(exported$.sample_id)])
  expect_length(weight, 1L)
  share <- function(both) ifelse(both, 1 / 2, 1)
  by_hand <- weight * sum(share(dropped$in_B)) +
    sum((b$.weight * share(b$in_A))[!b$eligible])
  expect_equal(unname(coef(survey::svytotal(~counted, replicate)))[2],
               by_hand)
  y_total <- sum(settled$.weight * share(settled$in_B) * settled$y) +
    sum((b$.weight * share(b$in_A) * b$y)[b$eligible])
  replicate <- update(replicate, y_eligible = ifelse(eligible, y, 0))
  expect_equal(
    unname(coef(survey::svytotal(~y_eligible, replicate, na.rm = TRUE))),
    y_total
  )

  # A component replayed from its receipt rebuilds the same units, and its
  # added rows composite as before.
  added_a <- function(design) {
    design <- update(design, dropped = is.na(.sample_id) & .frame == "A")
    unname(coef(survey::svytotal(~dropped, design)))[2]
  }
  expect_equal(added_a(replicate), weight * sum(share(dropped$in_B)))
  path <- withr::local_tempfile(fileext = ".json")
  write_design(settled, path, frame = frame_a)
  replayed <- replay_design(read_design(path), frame_a)
  again <- stack_frames(A = replayed, B = b, key = id,
                        membership = c(A = "in_A", B = "in_B"))
  again <- suppressWarnings(as_svrepdesign(again, type = "auto"))
  expect_equal(added_a(again), added_a(replicate))

  # A membership joined after the settlement has no value for them.
  bare <- listed[setdiff(names(listed), "in_B")]
  late <- suppressWarnings(execute(bare, response_status = disp))
  late$in_B <- late$id >= 5
  stack <- stack_frames(A = late, B = b, key = id,
                        membership = c(A = "in_A", B = "in_B"))
  expect_error(suppressWarnings(as_svrepdesign(stack, type = "auto")),
               regexp = "in_B",
               class = "samplyr_error_stack_frames_membership_missing")
})

test_that("a census component composites with no replicate column", {
  skip_if_not_installed("survey")
  # A census exports no replicate column under the jackknife. Its block in
  # the stack's matrix then ran backwards and the export failed, with a
  # census beside a sampled frame as with two censuses.
  frame_a <- data.frame(id = 1:6, in_A = TRUE, in_B = 1:6 >= 4,
                        y = c(3, 1, 4, 1, 5, 9))
  frame_b <- data.frame(id = 4:12, in_A = 4:12 <= 6, in_B = TRUE,
                        y = c(2, 6, 5, 3, 5, 8, 9, 7, 9))
  census <- function(frame, seed) {
    execute(sampling_design() |> draw(frac = 1), frame, seed = seed)
  }
  sampled <- function(frame, seed) {
    execute(sampling_design() |> draw(n = 4), frame, seed = seed)
  }
  # One-to-one complete links leave each weight as it was. The source's
  # replicate system had no column either, and survey's constructor failed
  # on it with "missing value where TRUE/FALSE needed".
  shared <- function(frame, seed) {
    share_weights(census(frame, seed), targets = frame,
                  links = data.frame(id = frame$id), by = c(id = "id"),
                  to = c(id = "id"), within = NULL,
                  multiplicity = complete_links())
  }
  # By hand: each frame's total of y, halved on the overlap, and for a
  # simple random sample of n from N its variance N^2 (1 - n / N) s^2 / n.
  # A census adds its total and no variance.
  by_hand <- function(s, N, other) {
    z <- s$y * ifelse(s[[other]], 1 / 2, 1)
    n <- nrow(s)
    c(total = N * mean(z),
      variance = if (n == N) 0 else N^2 * (1 - n / N) * stats::var(z) / n)
  }
  cases <- list(
    census_sampled = list(census(frame_a, 1), sampled(frame_b, 2)),
    sampled_census = list(sampled(frame_a, 1), census(frame_b, 2)),
    census_census = list(census(frame_a, 1), census(frame_b, 2)),
    shared_sampled = list(shared(frame_a, 1), sampled(frame_b, 2)),
    sampled_shared = list(sampled(frame_a, 1), shared(frame_b, 2)),
    shared_census = list(shared(frame_a, 1), census(frame_b, 2))
  )
  checked <- character(0)
  for (case in names(cases)) {
    a <- cases[[case]][[1]]
    b <- cases[[case]][[2]]
    expected <- by_hand(a, 6, "in_B") + by_hand(b, 9, "in_A")
    stack <- stack_frames(A = a, B = b, key = id,
                          membership = c(A = "in_A", B = "in_B"))
    for (type in c("auto", "JK1")) {
      design <- as_svrepdesign(stack, type = type)
      total <- survey::svytotal(~y, design)
      expect_equal(unname(coef(total)), unname(expected[["total"]]),
                   info = paste(case, type))
      expect_equal(as.numeric(survey::SE(total)),
                   sqrt(unname(expected[["variance"]])),
                   info = paste(case, type))
      checked <- c(checked, paste(case, type))
    }
  }
  expect_setequal(checked, paste(rep(names(cases), each = 2),
                                 c("auto", "JK1")))
  # Two censuses are a census: variance zero, and a calibration runs.
  design <- update(as_svrepdesign(stack), one = 1)
  expect_identical(as.numeric(survey::SE(survey::svytotal(~y, design))), 0)
  calibrated <- survey::calibrate(design, ~one - 1,
                                  population = c(one = 30))
  expect_equal(sum(weights(calibrated, type = "sampling")), 30)

  # Disjoint frames give every row the weight 1. survey 4.5's calibrate()
  # then reads compress = NA as NA || FALSE on the uncompressed matrix and
  # fails, so it needs compress = FALSE.
  frame_c <- data.frame(id = 13:18, in_A = FALSE, in_B = TRUE,
                        y = c(1, 2, 3, 4, 5, 6))
  frame_a$in_B <- FALSE
  checked <- character(0)
  for (a in list(census(frame_a, 1), shared(frame_a, 1))) {
    stack <- stack_frames(A = a, B = census(frame_c, 2), key = id,
                          membership = c(A = "in_A", B = "in_B"))
    for (type in c("auto", "JK1")) {
      design <- update(as_svrepdesign(stack, type = type), one = 1)
      expect_identical(unique(weights(design, type = "sampling")), 1)
      expect_lt(as.numeric(stats::vcov(survey::svytotal(~y, design))),
                1e-20)
      calibrated <- survey::calibrate(design, ~one - 1,
                                      population = c(one = 24),
                                      compress = FALSE)
      expect_equal(sum(weights(calibrated, type = "sampling")), 24)
      expect_lt(as.numeric(stats::vcov(survey::svytotal(~y, calibrated))),
                1e-20)
      checked <- c(checked, paste(inherits(a, "tbl_sample") &&
                                    !is.null(attr(a, "metadata")$weight_share),
                                  type))
    }
  }
  expect_setequal(checked, c("FALSE auto", "FALSE JK1", "TRUE auto",
                             "TRUE JK1"))
})

test_that("a reordered shared component stacks as its recorded order does", {
  skip_if_not_installed("survey")
  dwellings <- data.frame(dwelling_id = 1:40, region = rep(1:2, each = 20))
  source <- sampling_design() |> stratify_by(region) |>
    draw(n = c(`1` = 6, `2` = 10)) |> execute(dwellings, seed = 11)
  people <- data.frame(
    person_id = 1:40, dwelling_id = 1:40, y = as.numeric((1:40)^1.5),
    in_reached = TRUE, in_list = rep(c(TRUE, FALSE), 20)
  )
  shared <- share_weights(
    source, targets = people,
    links = data.frame(dwelling_id = 1:40, person_id = 1:40),
    by = c(dwelling_id = "dwelling_id"), to = c(person_id = "person_id"),
    within = NULL, multiplicity = complete_links()
  )
  listed <- sampling_design() |> draw(n = 8) |>
    execute(people[people$in_list, ], seed = 12)
  stacked <- function(component) {
    frames <- stack_frames(
      reached = component, list = listed,
      membership = c(reached = "in_reached", list = "in_list"),
      key = person_id
    )
    set.seed(5)
    survey::svytotal(~y, suppressWarnings(
      as_svrepdesign(frames, type = "bootstrap", replicates = 50)
    ))
  }
  recorded <- stacked(shared)
  reversed <- stacked(shared[rev(seq_len(nrow(shared))), ])
  # Hartley's estimator with theta = 0.5, by hand.
  by_hand <- sum(shared$.weight * shared$y * ifelse(shared$in_list, 0.5, 1)) +
    sum(listed$.weight * listed$y * 0.5)
  expect_equal(unname(stats::coef(recorded)), by_hand, tolerance = 1e-10)
  expect_equal(unname(stats::coef(reversed)), by_hand, tolerance = 1e-10)
  expect_equal(unname(survey::SE(reversed)), unname(survey::SE(recorded)),
               tolerance = 1e-10)
})
