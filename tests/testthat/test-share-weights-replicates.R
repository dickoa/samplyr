## G3. Replicate weights for a shared-weight sample


# The whole content of this phase is an ordering. Weight sharing is linear, so
# the recorded operator can be applied to a replicate weight system exactly as
# it is applied to the base weights. What it cannot do is act on replicates
# built from the target rows, because those rows were never sampled.

gwsm_rep_source <- function(seed = 1) {
  frame <- data.frame(unit = 1:40, st = rep(1:2, each = 20))
  sampling_design() |>
    stratify_by(st) |>
    # Unequal stratum fractions on purpose: with one weight for every unit,
    # pairing a row with the wrong row's weight would change nothing, and the
    # realignment tests would pass against an implementation that skipped it.
    draw(n = c("1" = 8, "2" = 4)) |>
    execute(frame, seed = seed)
}

gwsm_rep_targets <- function() {
  set.seed(9)
  data.frame(pid = 1:80, hh = rep(1:40, each = 2), y = stats::rnorm(80, 5))
}

gwsm_rep_links <- function() {
  data.frame(unit = rep(1:40, each = 2), pid = 1:80)
}

gwsm_rep_shared <- function(source = gwsm_rep_source()) {
  share_weights(
    source,
    targets = gwsm_rep_targets(),
    links = gwsm_rep_links(),
    by = c(unit = "unit"),
    to = c(pid = "pid"),
    within = hh,
    multiplicity = complete_links()
  )
}

## The operator inside the replication

test_that("the operator applied to a replicate matrix equals a hand-coded loop", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()
  record <- attr(shared, "metadata")$weight_share
  source_rep <- as_svrepdesign(record$source_sample, type = "JKn")
  analysis <- stats::weights(source_rep, type = "analysis")

  loop <- vapply(
    seq_len(ncol(analysis)),
    function(i) apply_share_operator(record$operator, analysis[, i]),
    numeric(record$operator$n_target)
  )

  expect_equal(
    unname(apply_share_operator(record$operator, analysis)),
    loop
  )
})

test_that("the shared total is the Horvitz-Thompson total of a derived variable", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()
  record <- attr(shared, "metadata")$weight_share
  op <- record$operator

  dense <- matrix(0, nrow = op$n_target, ncol = op$n_source)
  for (k in seq_along(op$share)) {
    dense[op$target_row[k], op$source_row[k]] <- op$share[k]
  }
  source_weights <- record$source_sample$.weight
  derived <- as.vector(t(dense) %*% shared$y)

  # This identity is why the method needs no new variance theory: the shared
  # total is an ordinary HT total of a variable defined on the source units.
  expect_equal(
    sum(apply_share_operator(op, source_weights) * shared$y),
    sum(source_weights * derived)
  )
})

test_that("sharing inside the replication differs from sharing after it", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()
  rep_design <- as_svrepdesign(shared, type = "JKn")
  inside <- survey::SE(survey::svytotal(~y, rep_design))

  # What a user gets by sharing once and then replicating the target rows as
  # though they had been sampled. Two members of a household carry the same
  # weight and are perfectly correlated, and a target-row jackknife cannot
  # see that.
  d <- as.data.frame(rep_design$variables)
  d$w <- stats::weights(rep_design, type = "sampling")
  after_design <- survey::as.svrepdesign(
    survey::svydesign(ids = ~1, weights = ~w, data = d),
    type = "JK1"
  )
  after <- survey::SE(survey::svytotal(~y, after_design))

  expect_false(isTRUE(all.equal(inside, after)))
  # Pin the correct one, so a regression to the other ordering fails rather
  # than merely producing a different number.
  expect_equal(inside, 25.45372, tolerance = 1e-5)
})

## What the exported object carries

test_that("the export is a target-row replicate design with the source's structure", {
  skip_if_not_installed("survey")
  source <- gwsm_rep_source()
  shared <- gwsm_rep_shared(source)
  source_rep <- as_svrepdesign(source, type = "JKn")
  rep_design <- as_svrepdesign(shared, type = "JKn")

  expect_s3_class(rep_design, "svyrep.design")
  expect_identical(nrow(rep_design$variables), nrow(shared))
  expect_identical(
    ncol(stats::weights(rep_design, type = "analysis")),
    ncol(stats::weights(source_rep, type = "analysis"))
  )
  # Replicate structure belongs to the source design and is carried across
  # rather than invented for the target rows.
  expect_identical(rep_design$scale, source_rep$scale)
  expect_equal(rep_design$rscales, source_rep$rscales)
  expect_identical(rep_design$mse, source_rep$mse)
})

test_that("the base weights of the export are the shared weights", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()
  rep_design <- as_svrepdesign(shared, type = "JKn")

  expect_equal(
    unname(stats::weights(rep_design, type = "sampling")),
    shared$.weight
  )
  # 40 households of two people each, and the shared weights estimate that
  # population total exactly under this design.
  expect_equal(sum(stats::weights(rep_design, type = "sampling")), 80)
})

test_that("the export records the transformation it came through", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()
  rep_design <- as_svrepdesign(shared, type = "JKn")
  note <- attr(rep_design, "samplyr_weight_share")

  expect_identical(note$algorithm, "generalized_weight_share")
  expect_identical(note$within_mode, "cluster")
  expect_identical(note$n_source_rows, 12L)
})

test_that("srvyr's replicate bridge reaches the same design", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  shared <- gwsm_rep_shared()

  tbl <- srvyr::as_survey_rep(shared, type = "JKn")
  expect_s3_class(tbl, "tbl_svy")
  expect_equal(
    survey::SE(survey::svytotal(~y, as_svrepdesign(shared, type = "JKn"))),
    survey::SE(survey::svytotal(~y, tbl))
  )
})

## Alignment

test_that("a reordered target sample exports through key realignment", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()
  reordered <- shared[rev(seq_len(nrow(shared))), ]

  straight <- as_svrepdesign(shared, type = "JKn")
  shuffled <- as_svrepdesign(reordered, type = "JKn")

  # Reordering a table is an ordinary thing to do, and the operator is
  # positional, so the export realigns rather than refusing or silently
  # pairing the wrong weights with the wrong rows.
  expect_equal(
    survey::SE(survey::svytotal(~y, straight)),
    survey::SE(survey::svytotal(~y, shuffled))
  )
  # Per unit, not merely as a multiset: a sorted comparison would pass even
  # if every weight had been attached to the wrong target.
  by_pid <- function(d) {
    stats::setNames(
      stats::weights(d, type = "sampling"), as.character(d$variables$pid)
    )
  }
  straight_w <- by_pid(straight)
  expect_equal(by_pid(shuffled)[names(straight_w)], straight_w)
  expect_gt(length(unique(straight_w)), 1L)
})

test_that("a replicate type whose scale is not one keeps that scale", {
  skip_if_not_installed("survey")
  source <- gwsm_rep_source()
  shared <- gwsm_rep_shared(source)

  # JKn carries its factors in rscales and leaves scale at 1, so it cannot
  # tell a preserved scale from a hardcoded one. bootstrap can.
  source_rep <- as_svrepdesign(source, type = "bootstrap")
  rep_design <- as_svrepdesign(shared, type = "bootstrap")

  expect_false(isTRUE(all.equal(source_rep$scale, 1)))
  expect_identical(rep_design$scale, source_rep$scale)
})

test_that("an altered shared sample is caught by the tampering gate", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()

  # The transformation minted a fresh integrity record covering the target
  # keys and the generated columns, so the ordinary gate sees these and owns
  # the finding. The alignment check is for what it cannot see.
  tampered <- shared
  tampered$.weight[1] <- tampered$.weight[1] * 2
  expect_error(
    as_svrepdesign(tampered, type = "JKn"),
    class = "samplyr_error_modified_sample"
  )

  dropped <- shared[-1, ]
  expect_error(
    as_svrepdesign(dropped, type = "JKn"),
    class = "samplyr_error_modified_sample"
  )
})

test_that("the tampering advice does not point a shared sample at a refusal", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()
  tampered <- shared
  tampered$.weight[1] <- tampered$.weight[1] * 2

  msg <- conditionMessage(tryCatch(
    as_svrepdesign(tampered, type = "JKn"), condition = function(e) e
  ))
  # Running a second phase and exporting with as_svydesign() are both refused
  # for a shared sample, so advice naming them would send the user from one
  # refusal to another.
  expect_match(msg, "share_weights")
  expect_no_match(msg, "run a second phase")
})

test_that("an altered retained source is caught by the alignment check", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()

  # Only the alignment check can see this: the target rows are untouched, so
  # the ordinary integrity record verifies.
  meta <- attr(shared, "metadata")
  meta$weight_share$source_sample$.weight[1] <- 999
  attr(shared, "metadata") <- meta

  expect_silent(check_sample_unmodified(shared, "test"))
  expect_error(
    as_svrepdesign(shared, type = "JKn"),
    class = "samplyr_error_weight_share_misaligned"
  )
})

## Restrictions propagate from the source export

test_that("sharing makes no replicate method more exact", {
  skip_if_not_installed("survey")
  frame <- data.frame(unit = 1:40, st = rep(1:2, each = 20))
  source <- sampling_design() |>
    stratify_by(st) |>
    draw(n = c("1" = 8, "2" = 8), method = "systematic") |>
    execute(frame, seed = 1)
  targets <- data.frame(pid = 1:40, hh = 1:40, y = 1)
  links <- data.frame(unit = 1:40, pid = 1:40)
  shared <- share_weights(
    source, targets, links,
    by = c(unit = "unit"), to = c(pid = "pid"),
    within = hh, multiplicity = complete_links()
  )

  # No replicate type reproduces a systematic stage. That restriction belongs
  # to the source design, and it reaches the user through the shared branch
  # unchanged, because the source export is what raises it.
  expect_error(
    as_svrepdesign(shared, type = "JKn", systematic_variance = "error"),
    class = "samplyr_error_systematic_variance"
  )
  expect_warning(
    as_svrepdesign(shared, type = "JKn", systematic_variance = "warn"),
    class = "samplyr_warning_systematic_variance"
  )
  # And the acknowledgement is recorded on the exported object, so a shared
  # export is no less traceable than an ordinary one.
  quiet <- as_svrepdesign(
    shared, type = "JKn", systematic_variance = "approximate"
  )
  expect_false(is.null(attr(quiet, "samplyr_systematic_variance")))
})

test_that("both routes take a shared sample, by different constructions", {
  skip_if_not_installed("survey")
  shared <- gwsm_rep_shared()

  expect_no_error(as_svrepdesign(shared, type = "JKn"))
  # The replicate route keeps the target rows and replicates the source; the
  # linearized route expands the source-target contributions instead.
  linearized <- as_svydesign(shared)
  expect_gte(nrow(linearized$variables), nrow(shared))
  expect_equal(
    unname(coef(survey::svytotal(~y, linearized))),
    sum(shared$.weight * shared$y)
  )
})

test_that("an ordinary sample's replicate export is unchanged", {
  skip_if_not_installed("survey")
  source <- gwsm_rep_source()

  expect_identical(sample_weight_contract(source), "design")
  expect_no_error(as_svrepdesign(source, type = "JKn"))
  expect_null(attr(as_svrepdesign(source, type = "JKn"), "samplyr_weight_share"))
})

test_that("RWYB Poisson factors propagate through the recorded sharing operator", {
  skip_if_not_installed("svrep", "0.9.1")
  source <- sampling_design() |> draw(frac = .5, method = "bernoulli") |>
    execute(data.frame(unit = 1:40), seed = 42)
  shared <- gwsm_rep_shared(source)
  record <- attr(shared, "metadata")$weight_share
  source_rep <- withr::with_seed(42, as_svrepdesign(source, type = "rwyb", replicates = 200))
  result <- withr::with_seed(42, as_svrepdesign(shared, type = "rwyb", replicates = 200))
  expect_equal(unname(stats::weights(result, type = "analysis")),
    unname(apply_share_operator(record$operator, stats::weights(source_rep, type = "analysis"))))
  expect_equal(unname(stats::weights(result, type = "sampling")), shared$.weight)
})
