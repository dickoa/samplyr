## G4. Quantitative links

# Lavallee section 4.5 replaces the 0/1 link indicator with a non-negative
# importance, with no loss of theory subject to Constraint 4.1: the total over
# each target cluster is strictly positive. Counting is the case where every
# link counts for one, so the two run the same code and the tests say so.

## The worked population
#
# Source U^A = {a, b, c, d}, srswor n = 2, every design weight 2.
# Target: H1 = {t1, t2}, H2 = {t3}.
#
# Importances B[j, ik]:  a->t1 = 3, a->t2 = 1, b->t1 = 2, c->t3 = 4, d->t3 = 6
# so B[t1] = 5, B[t2] = 1, B[t3] = 10, and B[H1] = 6, B[H2] = 10.

wl_source_frame <- function() {
  data.frame(unit = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
}

wl_targets <- function() {
  data.frame(
    tid = c("t1", "t2", "t3"),
    hh = c("H1", "H1", "H2"),
    y = c(1, 10, 100),
    stringsAsFactors = FALSE
  )
}

wl_links <- function() {
  data.frame(
    unit = c("a", "a", "b", "c", "d"),
    tid = c("t1", "t2", "t1", "t3", "t3"),
    B = c(3, 1, 2, 4, 6),
    stringsAsFactors = FALSE
  )
}

wl_all_samples <- function() {
  frame <- wl_source_frame()
  seen <- list()
  for (s in 1:200) {
    smp <- sampling_design() |> draw(n = 2) |> execute(frame, seed = s)
    key <- paste(sort(smp$unit), collapse = "")
    if (is.null(seen[[key]])) {
      seen[[key]] <- smp
    }
  }
  seen
}

wl_share <- function(sample, targets = wl_targets(), links = wl_links(), ...) {
  share_weights(
    sample,
    targets = targets, links = links,
    by = c(unit = "unit"), to = c(tid = "tid"),
    within = hh,
    multiplicity = weighted_links(B, total = complete_weighted_links()),
    ...
  )
}

## Expectations

test_that("quantitative links are exactly unbiased", {
  samples <- wl_all_samples()
  expect_length(samples, 6L)

  totals <- vapply(samples, function(s) {
    r <- wl_share(s)
    sum(r$.weight * r$y)
  }, numeric(1))

  # Every cluster has a positive link total, so Constraint 4.1 holds and the
  # whole target population is reachable.
  expect_equal(mean(totals), sum(wl_targets()$y))
  expect_equal(mean(totals), 111)
})

test_that("the weights agree with a hand-calculated B matrix", {
  r <- wl_share(wl_all_samples()[["ab"]])

  # Sample {a, b}: B[a, H1] = 3 + 1 = 4 and B[b, H1] = 2, over B[H1] = 6.
  expect_equal(unique(r$.weight[r$hh == "H1"]), 2 * (4 / 6) + 2 * (2 / 6))

  r2 <- wl_share(wl_all_samples()[["ac"]])
  # {a, c}: only a reaches H1, carrying 4 of the 6; only c reaches H2, with 4
  # of the 10.
  expect_equal(unique(r2$.weight[r2$hh == "H1"]), 2 * (4 / 6))
  expect_equal(unique(r2$.weight[r2$hh == "H2"]), 2 * (4 / 10))
})

test_that("the weight is constant within a target cluster", {
  r <- wl_share(wl_all_samples()[["ab"]])
  h1 <- r[r$hh == "H1", ]

  # t1 and t2 carry very different importances, and still one weight: the
  # denominator is the cluster's, which is what makes unit and cluster
  # estimates agree.
  expect_equal(length(unique(h1$.weight)), 1L)
  expect_false(isTRUE(all.equal(h1$.link_weight[1], h1$.link_weight[2])))
})

## Counting is the unit-importance case

test_that("unit importances reproduce complete_links() exactly", {
  s <- wl_all_samples()[["ab"]]
  links <- wl_links()
  links$B <- 1

  weighted <- wl_share(s, links = links)
  counted <- share_weights(
    s, wl_targets(), wl_links(),
    by = c(unit = "unit"), to = c(tid = "tid"),
    within = hh, multiplicity = complete_links()
  )

  # The generalization has to contain the case it generalizes. If these ever
  # diverge, one of the two paths has drifted.
  expect_equal(weighted$.weight, counted$.weight)
  expect_equal(weighted$.link_weight, counted$.unit_links)
  expect_equal(weighted$.cluster_link_weight, counted$.cluster_links)
})

## The result columns

test_that("only the pair belonging to the scale in use is emitted", {
  quantitative <- wl_share(wl_all_samples()[["ab"]])
  counted <- share_weights(
    wl_all_samples()[["ab"]], wl_targets(), wl_links(),
    by = c(unit = "unit"), to = c(tid = "tid"),
    within = hh, multiplicity = complete_links()
  )

  expect_true(all(
    c(".link_weight", ".cluster_link_weight") %in% names(quantitative)
  ))
  expect_false(any(c(".unit_links", ".cluster_links") %in% names(quantitative)))
  expect_true(all(c(".unit_links", ".cluster_links") %in% names(counted)))
  expect_false(any(
    c(".link_weight", ".cluster_link_weight") %in% names(counted)
  ))
})

test_that("the link columns hold the unit and cluster importance totals", {
  r <- wl_share(wl_all_samples()[["ab"]])

  expect_identical(
    stats::setNames(r$.link_weight, r$tid),
    c(t1 = 5, t2 = 1)
  )
  expect_true(all(r$.cluster_link_weight == 6))
})

test_that("a generated quantitative name the register already uses is refused", {
  targets <- wl_targets()
  targets$.link_weight <- 1

  expect_error(
    wl_share(wl_all_samples()[["ab"]], targets = targets),
    class = "samplyr_error_generated_column_collision"
  )
})

## The population total is a choice here too

test_that("a supplied total that differs from the links is used as given", {
  s <- wl_all_samples()[["ab"]]
  targets <- wl_targets()
  # The recorded link table is incomplete: t1 really carries 9 of importance
  # in the population, not the 5 that were observed.
  targets$Btotal <- c(9, 1, 10)

  supplied <- share_weights(
    s, targets, wl_links(),
    by = c(unit = "unit"), to = c(tid = "tid"),
    within = hh, multiplicity = weighted_links(B, total = Btotal)
  )
  counted <- wl_share(s)

  expect_identical(supplied$.link_weight[supplied$tid == "t1"], 9)
  expect_identical(counted$.link_weight[counted$tid == "t1"], 5)
  # Cluster total 10 rather than 6, with the same numerators.
  expect_equal(
    unique(supplied$.weight[supplied$hh == "H1"]),
    2 * (4 / 10) + 2 * (2 / 10)
  )
  expect_false(isTRUE(all.equal(
    unique(supplied$.weight[supplied$hh == "H1"]),
    unique(counted$.weight[counted$hh == "H1"])
  )))
})

## Constraint 4.1 and the other refusals

test_that("a reached cluster whose importance totals zero is refused", {
  s <- wl_all_samples()[["ab"]]
  targets <- wl_targets()
  targets$Btotal <- c(0, 0, 10)

  # Constraint 4.1. The sample reached H1, so a zero total for it is a
  # contradiction rather than an empty cluster, and dividing by it would
  # produce an infinite weight.
  expect_error(
    share_weights(
      s, targets, wl_links(),
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = hh, multiplicity = weighted_links(B, total = Btotal)
    ),
    class = "samplyr_error_share_weights_multiplicity"
  )
})

test_that("a zero importance on one link is allowed if its cluster is positive", {
  s <- wl_all_samples()[["ab"]]
  links <- wl_links()
  links$B[2] <- 0

  # Constraint 4.1 is about the cluster total, not about every link. A link
  # of no importance contributes nothing and is not an error.
  r <- share_weights(
    s, wl_targets(), links,
    by = c(unit = "unit"), to = c(tid = "tid"),
    within = hh, multiplicity = weighted_links(B, total = complete_weighted_links())
  )
  expect_equal(unique(r$.weight[r$hh == "H1"]), 2 * (3 / 5) + 2 * (2 / 5))
  expect_identical(r$.link_weight[r$tid == "t2"], 0)
})

test_that("negative and missing importances are refused", {
  s <- wl_all_samples()[["ab"]]

  negative <- wl_links()
  negative$B[1] <- -1
  expect_error(
    wl_share(s, links = negative),
    class = "samplyr_error_share_weights_multiplicity"
  )

  missing <- wl_links()
  missing$B[1] <- NA
  expect_error(
    wl_share(s, links = missing),
    class = "samplyr_error_share_weights_multiplicity"
  )
})

test_that("weighted_links refuses columns and totals that do not exist", {
  s <- wl_all_samples()[["ab"]]

  expect_error(
    share_weights(
      s, wl_targets(), wl_links(),
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = hh, multiplicity = weighted_links(nope, total = complete_weighted_links())
    ),
    class = "samplyr_error_share_weights_multiplicity"
  )
  expect_error(
    share_weights(
      s, wl_targets(), wl_links(),
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = hh, multiplicity = weighted_links(B, total = nope)
    ),
    class = "samplyr_error_share_weights_multiplicity"
  )
  expect_error(
    share_weights(
      s, wl_targets(), wl_links(),
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = hh, multiplicity = weighted_links(B)
    ),
    class = "samplyr_error_share_weights_multiplicity"
  )
})

test_that("quantitative links with cluster elimination are refused, not guessed", {
  s <- wl_all_samples()[["ab"]]

  # Section 4.4 defers this: an extended link has to say what importance it
  # carries, and that convention is not fixed. Any answer would be one no
  # published convention backs.
  expect_error(
    share_weights(
      s, wl_targets(), wl_links(),
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = extend_links(hh),
      multiplicity = weighted_links(B, total = complete_weighted_links())
    ),
    class = "samplyr_error_share_weights_multiplicity"
  )
})

test_that("the quantitative markers refuse to be called on their own", {
  expect_error(weighted_links(x, total = y))
  expect_error(complete_weighted_links())
})

## Record and export

test_that("the record names the quantitative scale and its total mode", {
  s <- wl_all_samples()[["ab"]]

  asserted <- attr(wl_share(s), "metadata")$weight_share
  expect_identical(asserted$denominator$scale, "quantitative")
  expect_identical(asserted$denominator$mode, "complete_weighted_links")
  expect_identical(asserted$generated_cols, weight_share_generated_cols$quantitative)
  expect_no_error(prepare_weight_share_record(asserted, "A test"))

  targets <- wl_targets()
  targets$Btotal <- c(5, 1, 10)
  supplied <- attr(
    share_weights(
      s, targets, wl_links(),
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = hh, multiplicity = weighted_links(B, total = Btotal)
    ),
    "metadata"
  )$weight_share
  expect_identical(supplied$denominator$mode, "weighted_links")
  expect_identical(supplied$denominator$scale, "quantitative")
})

test_that("a quantitative sample exports to replicate weights like any other", {
  skip_if_not_installed("survey")
  s <- wl_all_samples()[["ab"]]
  r <- wl_share(s)

  rep_design <- as_svrepdesign(r, type = "JK1")
  expect_s3_class(rep_design, "svyrep.design")
  # The operator is the whole transformation whatever built it, so the
  # replicate route needs nothing scale-specific.
  expect_equal(
    unname(stats::weights(rep_design, type = "sampling")),
    r$.weight
  )
  # And neither does the linearized route, which takes the same operator as
  # its contribution rows.
  expect_false(is_null(attr(as_svydesign(r), "samplyr_weight_share")))
})

test_that("the operator reproduces the quantitative weights it recorded", {
  s <- wl_all_samples()[["ab"]]
  r <- wl_share(s)
  record <- attr(r, "metadata")$weight_share

  expect_identical(
    apply_share_operator(record$operator, s$.weight),
    r$.weight
  )
})

test_that("the summary reports the quantitative mode", {
  r <- wl_share(wl_all_samples()[["ab"]])

  txt <- paste(capture.output(summary(r)), collapse = "\n")
  expect_match(txt, "clustered on hh")
  expect_match(txt, "not these rows")
})
