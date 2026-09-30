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
