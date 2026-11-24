## Random-group variance for a replicated execution
#
# Each replicate is an independent sample of the whole design, so the mean of
# the replicate estimates has variance estimated by their spread over R
# (Wolter 2007, ch. 2). Every check is against that formula by hand.

replicate_totals <- function(sample, y = "y") {
  counts <- attr(sample, "metadata")$replicate_rows
  vapply(names(counts), function(r) {
    rows <- as.character(sample$.replicate) == r
    sum(sample$.weight[rows] * sample[[y]][rows])
  }, numeric(1))
}

test_that("random groups give the mean of the replicates and their spread", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:500, y = stats::rnorm(500, 50, 10))
  sample <- sampling_design() |>
    draw(n = 40) |>
    execute(frame, seed = 1, reps = 8)
  rg <- as_svrepdesign(sample, type = "random_groups")
  totals <- replicate_totals(sample)

  total <- survey::svytotal(~y, rg)
  expect_equal(unname(coef(total)), mean(totals))
  expect_equal(unname(vcov(total))[1, 1], stats::var(totals) / 8)
  expect_identical(as.integer(survey::degf(rg)), 7L)
  expect_equal(
    unname(stats::weights(rg, type = "sampling")),
    as.data.frame(sample)$.weight / 8
  )
  expect_identical(
    attr(rg, "samplyr_replication"),
    list(method = "random_groups", replicates = 8L)
  )
})

test_that("an empty replicate counts as a zero estimate", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:30, y = 1:30)
  sample <- suppressWarnings(
    sampling_design() |>
      draw(frac = 0.1, method = "bernoulli", on_empty = "silent") |>
      execute(frame, seed = 5, reps = 5)
  )
  expect_identical(
    unname(attr(sample, "metadata")$replicate_rows),
    c(3L, 0L, 0L, 3L, 3L)
  )
  totals <- replicate_totals(sample)
  expect_identical(unname(totals[2:3]), c(0, 0))

  total <- survey::svytotal(~y, as_svrepdesign(sample, type = "random_groups"))
  expect_equal(unname(coef(total)), mean(totals))
  expect_equal(unname(vcov(total))[1, 1], stats::var(totals) / 5)
})

test_that("random groups refuse replicates that share an earlier selection", {
  skip_if_not_installed("survey")
  frame <- data.frame(psu = rep(1:20, each = 10), id = 1:200, y = 1)
  design <- sampling_design() |>
    cluster_by(psu) |> draw(n = 5) |>
    add_stage() |> draw(n = 3)

  # One realized stage 1, continued three times.
  first <- execute(design, frame, stages = 1, seed = 1)
  shared <- execute(first, frame, seed = 2, reps = 3)
  cnd <- expect_error(
    as_svrepdesign(shared, type = "random_groups"),
    class = "samplyr_error_random_groups_shared"
  )
  expect_identical(condition_header(cnd), "as_svrepdesign")

  # One realized phase 1, subsampled three times.
  phase1 <- sampling_design() |>
    cluster_by(id) |> draw(n = 100) |>
    execute(frame, seed = 1)
  phase2 <- sampling_design() |>
    cluster_by(id) |> draw(n = 20) |>
    execute(phase1, seed = 2, reps = 3)
  expect_error(
    as_svrepdesign(phase2, type = "random_groups"),
    class = "samplyr_error_random_groups_shared"
  )

  # Replicating the earlier stage or phase too makes every level vary.
  first_reps <- execute(design, frame, stages = 1, seed = 1, reps = 3)
  expect_s3_class(
    as_svrepdesign(execute(first_reps, frame, seed = 2), type = "random_groups"),
    "svyrep.design"
  )
  phase1_reps <- sampling_design() |>
    cluster_by(id) |> draw(n = 100) |>
    execute(frame, seed = 1, reps = 3)
  phase2_reps <- sampling_design() |>
    cluster_by(id) |> draw(n = 20) |>
    execute(phase1_reps, seed = 2)
  expect_s3_class(
    as_svrepdesign(phase2_reps, type = "random_groups"),
    "svyrep.design"
  )
})

test_that("random groups of a later phase follow what phase 1 shares", {
  skip_if_not_installed("survey")
  rg_status <- function(v) v$status[v$estimator == "Random groups"]
  # Phase 2 within the pools of one realized phase 1 composes with it, and
  # the preflight called random groups supported where the export refuses.
  frame <- data.frame(psu = rep(1:6, each = 5), mos = rep(10 * (1:6), each = 5),
                      y = (1:30 %% 7) * 2 + 1)
  design1 <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_cps", mos = mos) |>
    add_stage() |> draw(n = 4)
  within <- sampling_design() |> stratify_by(psu) |> draw(n = 2)
  phase1 <- execute(design1, frame, seed = 1)
  phase2 <- execute(within, phase1, reps = 4, seed = 2)
  expect_error(as_svrepdesign(phase2, type = "random_groups"),
               class = "samplyr_error_random_groups_shared")
  for (v in list(variance_estimators(within, phase1),
                 variance_estimators(phase2))) {
    expect_identical(rg_status(v), "refused")
    expect_identical(v$class[v$estimator == "Random groups"],
                     "samplyr_error_random_groups_shared")
  }
  stack1 <- execute(design1, frame, seed = 1, reps = 4)
  expect_identical(rg_status(variance_estimators(within, stack1)), "supported")
  expect_s3_class(
    as_svrepdesign(execute(within, stack1, seed = 2), type = "random_groups"),
    "svyrep.design"
  )

  # A phase 1 that could select nothing else shares no variance, so the
  # replicates of phase 2 are the whole design's.
  population <- data.frame(psu = 1L, hh = 1:6, y = c(3, 9, 4, 12, 6, 2))
  fixed <- sampling_design() |>
    cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> draw(frac = 1) |>
    execute(population, seed = 1)
  expect_true(samplyr:::phase1_is_fixed_one_unit(fixed))
  sub <- sampling_design() |> draw(n = 3)
  phase2 <- execute(sub, fixed, reps = 4, seed = 2)
  expect_true(attr(phase2, "metadata")$replicates_complete)
  totals <- replicate_totals(phase2)
  total <- survey::svytotal(~y, as_svrepdesign(phase2, type = "random_groups"))
  expect_equal(unname(coef(total)), mean(totals))
  expect_equal(unname(vcov(total))[1, 1], stats::var(totals) / 4)
  expect_identical(rg_status(variance_estimators(sub, fixed)), "supported")
  expect_identical(rg_status(variance_estimators(phase2)), "supported")
  # Its continuation carries the answer.
  two <- sampling_design() |>
    add_stage() |> cluster_by(hh) |> draw(n = 3) |>
    add_stage() |> draw(frac = 1)
  first <- execute(two, fixed, stages = 1, reps = 4, seed = 2)
  expect_true(attr(execute(first, fixed, seed = 3), "metadata")$replicates_complete)
})

test_that("random groups need every replicate and only their own arguments", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:100, y = 1)
  design <- sampling_design() |> draw(n = 10)

  single <- execute(design, frame, seed = 1)
  expect_error(
    as_svrepdesign(single, type = "random_groups"),
    class = "samplyr_error_random_groups_input"
  )
  reps <- execute(design, frame, seed = 1, reps = 4)
  expect_error(
    as_svrepdesign(dplyr::filter(reps, .replicate == 2), type = "random_groups"),
    class = "samplyr_error_random_groups_input"
  )
  expect_error(
    as_svrepdesign(reps, type = "random_groups", replicates = 50),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    as_svrepdesign(reps, type = "random_groups", mse = "yes"),
    class = "samplyr_error_survey_argument"
  )
})

test_that("the two-phase replicate shapes are the ones the help pages name", {
  skip_if_not_installed("survey")
  population <- data.frame(
    psu = rep(1:4, each = 6), hh = 1:24, mos = rep(c(2, 3, 4, 5), each = 6),
    y = (1:24) %% 7
  )
  sub <- sampling_design() |> draw(n = 4)

  # A census of two primary units could select nothing else, but is not one
  # primary unit, so it keeps the two-phase route and has no replicate
  # export, by random groups neither.
  census <- sampling_design() |>
    cluster_by(psu) |> draw(frac = 1) |> add_stage() |> draw(frac = 1) |>
    execute(population[population$psu <= 2, ], seed = 1)
  expect_false(samplyr:::phase1_is_fixed_one_unit(census))
  expect_warning(
    twophase <- as_svydesign(execute(sub, census, seed = 2)),
    class = "samplyr_warning_twophase_across_units"
  )
  expect_s3_class(twophase, "twophase2")
  expect_error(as_svrepdesign(execute(sub, census, seed = 2)),
               class = "samplyr_error_svrep_twophase_unsupported")
  expect_error(
    as_svrepdesign(execute(sub, census, seed = 2, reps = 4),
                   type = "random_groups"),
    class = "samplyr_error_random_groups_shared"
  )

  # A phase 1 drawn with unequal probabilities is refused by the exports,
  # and replicated by random groups when the design runs whole with reps.
  pps <- sampling_design() |>
    cluster_by(psu) |> draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(frac = 1)
  phase1 <- execute(pps, population, seed = 1)
  expect_error(as_svydesign(execute(sub, phase1, seed = 2)),
               class = "samplyr_error_twophase_phase1_pps")
  stacked <- execute(sub, execute(pps, population, seed = 1, reps = 4), seed = 2)
  expect_s3_class(as_svrepdesign(stacked, type = "random_groups"),
                  "svyrep.design")
})
