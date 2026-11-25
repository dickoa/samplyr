## as_svydesign.frame_stack(): the linearized dual-frame export

# Frame a (units 1 to 40) takes 10 and frame b (units 21 to 60) takes 20, so
# the weights are 4 and 2. The unequal fractions make survey's default theta
# 2/3 rather than 1/2, so forwarding NULL fails the tests.

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
  # survey reads a missing theta as 4 / (4 + 2) here.
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
  # survey reads `overlaps[[f]][, 3 - f]`, the other frame's column by index.
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
  # The per-component export raises the same class, so match the message.
  expect_error(as_svydesign(frames), regexp = "reached")
  expect_error(
    as_svydesign(frames),
    regexp = "frames\\[\\[\"<frame name>\"\\]\\]"
  )
})

## Forwarded arguments

test_that("pps is refused, because it describes one component", {
  skip_if_not_installed("survey")
  # One matrix for both components would give a wrong variance, not an error.
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
  # Every exported design is already nested, so only the value's arrival shows.
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

test_that("a frame drawn with unequal probabilities without replacement is refused", {
  skip_if_not_installed("survey")
  # Frame a lists every unit, so with theta = 1 the stack is frame a's own
  # design and its variance must be frame a's export (another route, but a
  # stack read as equal-probability sampling gave a negative variance).
  population <- data.frame(id = 1:40, size = (1:40 * 7) %% 19 + 1,
                           in_a = TRUE, in_b = rep(c(TRUE, FALSE), 20))
  population$y <- population$size * 5 + (1:40 %% 3)
  b <- sampling_design() |> draw(n = 8) |>
    execute(population[population$in_b, ], seed = 104)
  methods <- c("srswor", "systematic", "pps_poisson", "pps_brewer",
               "pps_systematic", "pps_sampford", "pps_cps")
  refused <- character(0)
  for (method in methods) {
    design <- if (method %in% c("srswor", "systematic")) {
      sampling_design() |> draw(n = 10, method = method)
    } else {
      sampling_design() |> draw(n = 10, method = method, mos = size)
    }
    a <- execute(design, population, seed = 4)
    frames <- stack_frames(a = a, b = b, membership = c(a = "in_a", b = "in_b"),
                           key = id)
    stacked <- tryCatch(
      suppressWarnings(as_svydesign(frames, theta = 1)),
      samplyr_error_survey_multiframe_unsupported = function(e) NULL
    )
    if (is.null(stacked)) {
      refused <- c(refused, method)
      expect_s3_class(
        suppressWarnings(as_svrepdesign(frames, replicates = 10)),
        "svyrep.design"
      )
      next
    }
    expect_equal(
      stats::vcov(survey::svytotal(~y, stacked))[1],
      stats::vcov(survey::svytotal(~y, suppressWarnings(as_svydesign(a))))[1],
      tolerance = 1e-8, info = method
    )
  }
  expect_setequal(refused,
                  c("pps_brewer", "pps_systematic", "pps_sampford", "pps_cps"))
})

test_that("two censuses of consistent registers estimate the total exactly", {
  skip_if_not_installed("survey")
  # Registers in agreement count the overlap once, so the estimate is exact.
  population <- data.frame(
    pid = 1:300,
    y = as.numeric(1:300),
    in_a = rep(c(TRUE, FALSE), c(200, 100)),
    in_b = rep(c(FALSE, TRUE), c(100, 200))
  )
  census <- function(col, seed) {
    rows <- population[population[[col]], , drop = FALSE]
    sampling_design() |> draw(n = nrow(rows)) |> execute(rows, seed = seed)
  }
  stack <- stack_frames(
    a = census("in_a", 1), b = census("in_b", 2),
    membership = c(a = "in_a", b = "in_b"),
    key = pid
  )
  total <- survey::svytotal(~y, as_svydesign(stack))
  expect_equal(unname(stats::coef(total)), sum(population$y))
})

test_that("a component's added rows line up with its overlap matrix", {
  skip_if_not_installed("survey")
  # A settled reserve exports its ineligible units as rows the sample does
  # not hold. The overlap matrices were built from the sample's rows, so
  # survey recycled them over the longer design: a total of 128.25 for
  # 131.625. An added row stands for a unit the settlement dropped, with
  # that unit's membership. The linearized stack refuses a settled
  # component, which it exports with no finite correction, so the matrices
  # are checked directly.
  units_b <- c(1, 4, 7, 10:15)
  units_a <- 1:12
  frame_b <- data.frame(
    unit_id = units_b, in_A = units_b %in% units_a, in_B = TRUE,
    pA = ifelse(units_b %in% units_a, 2 / 6, 0), pB = 4 / 9, y = units_b
  )
  frame_a <- data.frame(
    unit_id = units_a, st = rep(c("S1", "S2"), each = 6), in_A = TRUE,
    in_B = units_a %in% units_b, pA = 2 / 6,
    pB = ifelse(units_a %in% units_b, 4 / 9, 0), y = units_a
  )
  b <- execute(sampling_design() |> draw(n = 4), frame_b, seed = 2)
  b$disp <- "interview"
  a <- execute(sampling_design() |> stratify_by(st) |>
                 draw(n = 2, reserve = 1), frame_a, seed = 1)
  a$disp <- ifelse(a$st == "S1", "ineligible",
                   ifelse(a$.reserve_1 == 0, "interview", NA_character_))
  dropped <- as.data.frame(a)[a$disp %in% "ineligible", ]
  a <- suppressWarnings(execute(a, response_status = disp))
  stack <- stack_frames(
    A = a, B = b, membership = c(A = "in_A", B = "in_B"),
    overlaps = declared_overlaps(A = "pA", B = "pB", scale = "probabilities"),
    key = unit_id
  )
  exported <- suppressWarnings(as_svydesign(a))$variables
  added <- is.na(exported$.sample_id)
  expect_identical(sum(added), 3L)
  # By hand: each row's weight times its factor, theta = 1/2 on the overlap.
  factor_a <- ifelse(!added & exported$in_B %in% TRUE, 1 / 2, 1)
  factor_b <- ifelse(b$in_A, 1 / 2, 1)
  y_total <- sum((exported$.weight * factor_a * exported$y)[!added]) +
    sum(b$.weight * factor_b * b$y)
  # Each dropped unit composites with its own membership.
  weight <- unique(exported$.weight[added])
  expect_length(weight, 1L)
  expect_identical(nrow(dropped), 3L)
  ineligible <- weight * sum(ifelse(dropped$in_B, 1 / 2, 1))
  expect_true(any(dropped$in_B) && !all(dropped$in_B))
  expect_equal(y_total, 131.625)
  expect_error(as_svydesign(stack),
               class = "samplyr_error_survey_multiframe_unsupported")
  designs <- list(A = suppressWarnings(as_svydesign(a)),
                  B = as_svydesign(b))
  overlaps <- samplyr:::multiframe_overlaps(stack, designs)
  expect_identical(vapply(overlaps, nrow, integer(1)),
                   vapply(designs, nrow, integer(1)))
  # An added row has the membership of the unit it stands for.
  expect_setequal(overlaps$A[added, "B"], as.numeric(dropped$in_B))
  expect_identical(unname(overlaps$A[added, "A"]), rep(1, 3))
  composite <- survey::multiframe(unname(designs), unname(overlaps),
                                  theta = 1 / 2)
  expect_equal(unname(coef(survey::svytotal(~y, composite, na.rm = TRUE))),
               y_total)
  # The replicate export composites the added rows the same way.
  replicate <- suppressWarnings(as_svrepdesign(stack, type = "auto"))
  expect_equal(unname(coef(survey::svytotal(~y, replicate, na.rm = TRUE))),
               y_total)
  replicate <- update(replicate, counted = disp %in% "ineligible")
  expect_equal(unname(coef(survey::svytotal(~counted, replicate)))[2],
               ineligible)
  # The stack's rows for them carry their keys and membership.
  rows <- replicate$variables
  rows <- rows[is.na(rows$.sample_id) & rows$.frame == "A", ]
  expect_setequal(rows$unit_id, dropped$unit_id)
  expect_identical(rows$in_B[order(rows$unit_id)],
                   dropped$in_B[order(dropped$unit_id)])
  expect_true(all(is.na(rows$y)))

  # A two-stage settled component exports by ultimate clusters and adds a
  # row for a primary unit whose pool has no respondent.
  units <- data.frame(psu = rep(1:6, each = 6), id = 1:36, in_A = TRUE)
  units$in_B <- units$id > 18
  units$y <- units$id
  two <- sampling_design() |> add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> draw(n = 2, reserve = 2)
  s <- execute(two, units, seed = 4)
  s$disp <- ifelse(s$id %% 5 == 0, "ineligible",
                   ifelse(s$psu == 3, "noninterview", "interview"))
  s$disp[s$.reserve_2 > 0 & !(s$psu %in% s$psu[s$disp != "interview" &
                                                 s$.reserve_2 == 0])] <- NA
  s <- suppressWarnings(execute(s, response_status = disp))
  other <- data.frame(id = 19:40, in_A = 19:40 <= 36, in_B = TRUE, y = 19:40)
  o <- execute(sampling_design() |> draw(n = 5), other, seed = 2)
  stack <- stack_frames(A = s, B = o, membership = c(A = "in_A", B = "in_B"),
                        key = id)
  exported <- suppressWarnings(as_svydesign(s))$variables
  expect_gt(nrow(exported), nrow(s))
  added <- is.na(exported$.sample_id)
  factor_a <- ifelse(!added & exported$in_B %in% TRUE, 1 / 2, 1)
  y_total <- sum((exported$.weight * factor_a * exported$y)[!added]) +
    sum(o$.weight * ifelse(o$in_A, 1 / 2, 1) * o$y)
  expect_error(suppressWarnings(as_svydesign(stack)),
               class = "samplyr_error_survey_multiframe_unsupported")
  replicate <- suppressWarnings(as_svrepdesign(stack, type = "auto"))
  expect_equal(unname(coef(survey::svytotal(~y, replicate, na.rm = TRUE))),
               y_total)
})

test_that("a frame drawn with replacement is not composited by linearization", {
  skip_if_not_installed("survey")
  # survey's dual-frame variance reads a component's units as drawn without
  # replacement whatever its correction. For a draw with replacement it gave
  # an SE about 7% below the components' own variances, while the methods
  # with a finite correction match them to the last digit.
  frame_a <- data.frame(id = 1:30, in_A = TRUE, in_B = 1:30 > 15,
                        m = rep(1:5, 6) + 2, y = (1:30) %% 7 + 1)
  frame_b <- data.frame(id = 16:45, in_A = 16:45 <= 30, in_B = TRUE,
                        y = (16:45) %% 7 + 1)
  b <- execute(sampling_design() |> draw(n = 8), frame_b, seed = 2)
  own <- function(s, other) {
    d <- suppressWarnings(as_svydesign(s))
    d <- update(d, yf = y * ifelse(d$variables[[other]], 1 / 2, 1))
    unname(vcov(survey::svytotal(~yf, d)))[1]
  }
  designs <- list(
    srswr = sampling_design() |> draw(n = 8, method = "srswr"),
    pps_multinomial = sampling_design() |>
      draw(n = 8, method = "pps_multinomial", mos = m),
    pps_chromy = sampling_design() |>
      draw(n = 8, method = "pps_chromy", mos = m),
    srswor = sampling_design() |> draw(n = 8),
    census = sampling_design() |> draw(frac = 1)
  )
  checked <- character(0)
  for (method in names(designs)) {
    a <- suppressWarnings(execute(designs[[method]], frame_a, seed = 1))
    stack <- stack_frames(A = a, B = b, key = id,
                          membership = c(A = "in_A", B = "in_B"))
    reference <- sqrt(own(a, "in_B") + own(b, "in_A"))
    if (method %in% c("srswor", "census")) {
      linearized <- survey::svytotal(~y, as_svydesign(stack))
      expect_equal(as.numeric(survey::SE(linearized)), reference,
                   info = method)
    } else {
      expect_error(as_svydesign(stack), regexp = "\"A\"",
                   class = "samplyr_error_survey_multiframe_unsupported",
                   info = method)
      # The way out the message names keeps each frame's own variance.
      replicate <- suppressWarnings(as_svrepdesign(stack))
      expect_equal(as.numeric(survey::SE(survey::svytotal(~y, replicate))),
                   reference, info = method)
    }
    checked <- c(checked, method)
  }
  expect_setequal(checked, names(designs))
})

test_that("the expected estimator refuses a component settled from a reserve", {
  skip_if_not_installed("survey")
  # The expected estimator weights a unit by one over the sum of its
  # chances. A settled weight is a realized adjusted weight, so in a
  # two-frame simulation the overlap estimate was biased, by 13 Monte Carlo
  # standard errors with frame B's nominal chances and by 4.7 with the true
  # ones, while the constant estimator was not.
  units_b <- c(1, 4, 7, 10:15)
  units_a <- 1:12
  frame_b <- data.frame(
    unit_id = units_b, in_A = units_b %in% units_a, in_B = TRUE,
    pA = ifelse(units_b %in% units_a, 2 / 6, 0), pB = 4 / 9, y = units_b
  )
  frame_a <- data.frame(
    unit_id = units_a, st = rep(c("S1", "S2"), each = 6), in_A = TRUE,
    in_B = units_a %in% units_b, pA = 2 / 6,
    pB = ifelse(units_a %in% units_b, 4 / 9, 0), y = units_a
  )
  b <- execute(sampling_design() |> draw(n = 4), frame_b, seed = 2)
  a <- execute(sampling_design() |> stratify_by(st) |>
                 draw(n = 2, reserve = 1), frame_a, seed = 1)
  a$disp <- ifelse(a$st == "S1", "ineligible",
                   ifelse(a$.reserve_1 == 0, "interview", NA_character_))
  settled <- suppressWarnings(execute(a, response_status = disp))
  overlaps <- declared_overlaps(A = "pA", B = "pB", scale = "probabilities")
  stack <- stack_frames(A = settled, B = b, overlaps = overlaps,
                        membership = c(A = "in_A", B = "in_B"),
                        key = unit_id)
  # This refusal comes before the linearized one for a settled component.
  for (export in list(as_svydesign, as_svrepdesign)) {
    expect_error(suppressWarnings(export(stack, estimator = "expected")),
                 regexp = "settled from\\s+a reserve list",
                 class = "samplyr_error_survey_weight_contract")
  }
  expect_s3_class(suppressWarnings(as_svrepdesign(stack, type = "auto")),
                  "svyrep.design")
  # Unsettled, the same component keeps the expected estimator.
  unsettled <- execute(sampling_design() |> stratify_by(st) |> draw(n = 2),
                       frame_a, seed = 1)
  stack <- stack_frames(A = unsettled, B = b, overlaps = overlaps,
                        membership = c(A = "in_A", B = "in_B"),
                        key = unit_id)
  expect_s3_class(as_svydesign(stack, estimator = "expected"), "dualframe")
  expect_s3_class(as_svrepdesign(stack, estimator = "expected"),
                  "svyrep.design")
})
