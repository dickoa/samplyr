## X2. The linearized export of a weight-share transformation

# The generalized weight share total is the Horvitz-Thompson total of a
# variable derived on the source units:
#
#   sum_i w_i y_i  =  sum_j I(j in S) / pi_j * z_j,   z_j = sum_i L_ji/L_i y_i
#
# so the oracle is that identity, computed with machinery that already
# existed: attach z to the source sample, export the source sample the
# ordinary way, and take its total. Nothing about that reimplements what is
# under test, and it pins the standard error as well as the estimate.
#
# The fixture links 15 of the 90 targets to TWO dwellings each, which is the
# structure an earlier draft's cluster-level shortcut could not have
# expressed, and which is the reason this export exists.

linearized_targets <- function() {
  withr::with_seed(3, data.frame(
    person_id = 1:90,
    hh = rep(paste0("h", 1:45), each = 2),
    y = round(stats::rnorm(90, 100, 20), 3)
  ))
}

linearized_links <- function() {
  rbind(
    data.frame(dw_id = rep(1:45, each = 2), person_id = 1:90),
    # 15 people reached through a second dwelling as well.
    data.frame(dw_id = 46:60, person_id = 1:15)
  )
}

linearized_dwellings <- function() {
  data.frame(
    dw_id = 1:60,
    region = rep(c("n", "s"), each = 30),
    size = rep(c(2, 5, 3, 8, 4, 6), length.out = 60),
    blk = rep(paste0("b", 1:12), each = 5)
  )
}

linearized_shared <- function(source_sample,
                              targets = linearized_targets()) {
  share_weights(
    source_sample,
    targets = targets,
    links = linearized_links(),
    by = c(dw_id = "dw_id"),
    to = c(person_id = "person_id"),
    within = hh,
    multiplicity = complete_links()
  )
}

# The identity, computed from the recorded operator and the source design.
linearized_oracle <- function(shared, source_sample, variable = "y") {
  operator <- attr(shared, "metadata")$weight_share$operator
  contributions <- operator$share * shared[[variable]][operator$target_row]
  totals <- rowsum(contributions, group = operator$source_row, reorder = FALSE)

  derived <- rep(0, nrow(source_sample))
  derived[as.integer(rownames(totals))] <- totals[, 1]
  source_sample$.derived <- derived
  suppressWarnings(survey::svytotal(~.derived, as_svydesign(source_sample)))
}

expect_matches_oracle <- function(source_sample,
                                 targets = linearized_targets()) {
  shared <- linearized_shared(source_sample, targets)
  got <- suppressWarnings(survey::svytotal(~y, as_svydesign(shared)))
  want <- linearized_oracle(shared, source_sample)

  expect_equal(unname(coef(got)), unname(coef(want)))
  expect_equal(unname(survey::SE(got)), unname(survey::SE(want)))
  invisible(shared)
}

## The identity

test_that("the export is the HT total of the derived source variable", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6)) |>
    execute(linearized_dwellings(), seed = 7)

  shared <- expect_matches_oracle(source_sample)

  # And the point estimate is the transformation's own weighted total, which
  # is what makes this the same estimator the replicate route exports.
  expect_equal(
    unname(coef(survey::svytotal(~y, as_svydesign(shared)))),
    sum(shared$.weight * shared$y)
  )
})

test_that("it holds for every source design shape it accepts", {
  skip_if_not_installed("survey")
  dwellings <- linearized_dwellings()

  expect_matches_oracle(
    sampling_design() |> draw(n = 18) |> execute(dwellings, seed = 7)
  )
  expect_matches_oracle(
    suppressWarnings(
      sampling_design() |>
        draw(n = 18, method = "systematic") |>
        execute(dwellings, seed = 7)
    )
  )
  expect_matches_oracle(
    sampling_design() |>
      add_stage() |>
      cluster_by(blk) |>
      draw(n = 6) |>
      add_stage() |>
      draw(n = 3) |>
      execute(dwellings, seed = 7)
  )
})

test_that("a target reached twice is carried, with no condition on it", {
  skip_if_not_installed("survey")
  # The case the cluster-level shortcut could not express. There are more
  # contributions than target rows precisely because of it.
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)
  operator <- attr(shared, "metadata")$weight_share$operator

  expect_gt(length(operator$share), nrow(shared))
  expect_gt(max(table(operator$target_row)), 1L)
  expect_identical(
    nrow(as_svydesign(shared)$variables),
    length(operator$share)
  )
})

## The ordering the export depends on

test_that("a source unit is one sampling unit, not one per contribution", {
  skip_if_not_installed("survey")
  # `survey_id_info()` gives `~1` for an unclustered design, meaning every row
  # is a unit. Resolved after the expansion that would make each contribution
  # its own unit and take the variance apart into pieces that are not
  # independent, which understates it without failing.
  source_sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6)) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)
  svy <- as_svydesign(shared)

  expect_identical(deparse(svy$call$ids), "~.source_unit")
  # One identifier per source row, repeated across that row's contributions.
  operator <- attr(shared, "metadata")$weight_share$operator
  expect_identical(
    svy$variables$.source_unit,
    source_sample$.sample_id[operator$source_row]
  )

  # The value of getting it wrong, on this fixture.
  wrong <- svy
  wrong$variables$.each <- seq_len(nrow(wrong$variables))
  by_row <- survey::svydesign(
    ids = ~.each, strata = ~region, weights = ~.weight,
    fpc = ~.fpc_1, data = wrong$variables, nest = TRUE
  )
  expect_lt(
    survey::SE(survey::svytotal(~y, by_row)),
    survey::SE(survey::svytotal(~y, svy))
  )
})

test_that("linearization refuses a selected source with no contribution", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6)) |>
    execute(linearized_dwellings(), seed = 7)
  unlinked <- source_sample$dw_id[[1L]]
  links <- linearized_links()
  links <- links[links$dw_id != unlinked, , drop = FALSE]

  shared <- share_weights(
    source_sample,
    targets = linearized_targets(),
    links = links,
    by = c(dw_id = "dw_id"),
    to = c(person_id = "person_id"),
    within = hh,
    multiplicity = complete_links()
  )

  expect_error(
    as_svydesign(shared),
    class = "samplyr_error_share_weights_zero_contribution_source"
  )
  expect_no_error(suppressWarnings(as_svrepdesign(shared, type = "JKn")))
})

test_that("the contribution weight is the coefficient times the source's", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)
  operator <- attr(shared, "metadata")$weight_share$operator

  expect_equal(
    as_svydesign(shared)$variables$.weight,
    operator$share * source_sample$.weight[operator$source_row]
  )
  # Which sums back to the transformation's own weights.
  expect_equal(
    sum(as_svydesign(shared)$variables$.weight),
    sum(shared$.weight)
  )
})

test_that("a mean's denominator is the target population either way", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)

  expect_equal(
    unname(coef(survey::svymean(~y, as_svydesign(shared)))),
    sum(shared$.weight * shared$y) / sum(shared$.weight)
  )
})

## What it refuses

test_that("a source design whose variance is indexed by its rows is refused", {
  skip_if_not_installed("survey")
  # An unequal-probability design takes its variance from a pairwise
  # approximation over the sampled rows, and a random-size one from its own
  # probabilities. Neither survives one row becoming several. The first is
  # the dangerous one: it returns a number, 1.6% low on this fixture, which
  # reads as agreement rather than as a warning.
  dwellings <- linearized_dwellings()

  for (design in list(
    sampling_design() |> draw(n = 18, method = "pps_brewer", mos = size),
    sampling_design() |> draw(n = 18, method = "pps_poisson", mos = size),
    sampling_design() |> draw(frac = 0.3, method = "bernoulli")
  )) {
    shared <- linearized_shared(execute(design, dwellings, seed = 7))
    expect_error(
      as_svydesign(shared),
      class = "samplyr_error_share_weights_source_design"
    )
  }

  # The route the refusal names does take the unequal-probability case.
  unequal <- linearized_shared(
    sampling_design() |>
      draw(n = 18, method = "pps_brewer", mos = size) |>
      execute(dwellings, seed = 7)
  )
  expect_no_error(
    suppressWarnings(as_svrepdesign(unequal, type = "subbootstrap"))
  )
})

test_that("pps is refused, because it describes the unexpanded sample", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)

  expect_error(
    as_svydesign(shared, pps = "anything"),
    class = "samplyr_error_share_weights_pps"
  )
})

test_that("a two-phase source is refused without naming the route that refused it", {
  skip_if_not_installed("survey")

  # Both phases declare the dwelling as their unit, so the two-phase export
  # has the bridge it needs and the last assertion below is about the route
  # rather than about the fixture.
  phase1 <- sampling_design() |>
    cluster_by(dw_id) |>
    draw(n = 40) |>
    execute(linearized_dwellings(), seed = 11)
  phase2 <- sampling_design() |>
    cluster_by(dw_id) |>
    draw(n = 18) |>
    execute(phase1, seed = 12)
  shared <- linearized_shared(phase2)

  # Neither route takes it, so neither may advise the other. Before this the
  # linearized refusal read "as_svydesign() does not support two-phase
  # samples. Use as_svydesign() for two-phase linearization export."
  for (export in list(
    function() as_svydesign(shared),
    function() as_svrepdesign(shared, type = "bootstrap", replicates = 10)
  )) {
    expect_error(export(), class = "samplyr_error_share_weights_twophase_source")
    expect_error(export(), regexp = "weights were shared from is two-phase")
  }
  expect_no_match(
    conditionMessage(tryCatch(as_svydesign(shared), error = identity)),
    "Use `as_svydesign()`",
    fixed = TRUE
  )

  # The advice it gives instead has to work.
  from_phase1 <- linearized_shared(phase1)
  expect_s3_class(as_svydesign(from_phase1), "survey.design")

  # And the ordinary two-phase route keeps its own class and its own advice,
  # which is correct there because the linearized export does take it.
  expect_error(
    as_svrepdesign(phase2, type = "bootstrap", replicates = 10),
    class = "samplyr_error_svrep_twophase_unsupported"
  )
  expect_s3_class(as_svydesign(phase2), "twophase2")
})

test_that("a target column may not take a source design column's name", {
  skip_if_not_installed("survey")
  # The exported design describes the source selection, so these names carry
  # its strata, its units or its population counts.
  targets <- linearized_targets()
  targets$region <- "n"

  source_sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6)) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample, targets)

  expect_error(
    as_svydesign(shared),
    class = "samplyr_error_share_weights_columns"
  )
})

test_that("a stack still refuses a shared component on this route", {
  skip_if_not_installed("survey")
  # The export exists for a lone transformation, but `survey::multiframe()`
  # reads one selection probability per row and a contribution is not one, so
  # the composited variance would be wrong rather than refused.
  targets <- linearized_targets()
  targets$in_reached <- TRUE
  targets$in_list <- rep(c(TRUE, FALSE, TRUE), 30)

  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample, targets)
  listed <- sampling_design() |>
    draw(n = 20) |>
    execute(targets[targets$in_list, , drop = FALSE], seed = 9)

  frames <- stack_frames(
    reached = shared, list = listed,
    membership = c(reached = "in_reached", list = "in_list"),
    key = person_id
  )
  expect_error(
    as_svydesign(frames),
    class = "samplyr_error_survey_weight_contract"
  )
  expect_error(as_svydesign(frames), regexp = "one selection probability")
})

## What the export carries

test_that("the transformation and its coverage travel with the design", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)
  svy <- as_svydesign(shared)

  record <- attr(svy, "samplyr_weight_share")
  expect_identical(record$algorithm, "generalized_weight_share")
  expect_identical(record$n_source_rows, nrow(source_sample))
  expect_identical(
    record$n_contributions,
    length(attr(shared, "metadata")$weight_share$operator$share)
  )
  expect_false(is_null(attr(svy, "samplyr_weight_share_coverage")))
})

test_that("a tampered result is refused before anything is built", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)

  tampered <- shared
  tampered$.weight <- tampered$.weight * 2
  expect_error(as_svydesign(tampered), class = "samplyr_error")

  # An altered retained source is what the integrity record cannot see: the
  # target rows are untouched, so only the alignment check catches it, and
  # the contribution weights are built from exactly those source weights.
  altered <- shared
  metadata <- attr(altered, "metadata")
  metadata$weight_share$source_sample$.weight[[1]] <- 999
  attr(altered, "metadata") <- metadata

  expect_silent(check_sample_unmodified(altered, "test"))
  expect_error(
    as_svydesign(altered),
    class = "samplyr_error_weight_share_misaligned"
  )

  # Reordering is recoverable, and gives the same numbers.
  reordered <- shared[rev(seq_len(nrow(shared))), ]
  expect_equal(
    unname(coef(survey::svytotal(~y, as_svydesign(reordered)))),
    unname(coef(survey::svytotal(~y, as_svydesign(shared))))
  )
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, as_svydesign(reordered)))),
    unname(survey::SE(survey::svytotal(~y, as_svydesign(shared))))
  )
})
