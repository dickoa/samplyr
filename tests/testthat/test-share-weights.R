## G2. share_weights(): the generalized weight share method

# The oracle is exact enumeration, not Monte Carlo. Both features are weight
# transformations with closed-form expectations, so the source population is
# small enough to enumerate every possible sample, and the expectation is an
# average over known probabilities rather than an approximation.

## The worked population
#
# Source U^A = {a, b, c, d}, srswor n = 2. Every one of the six samples has
# probability 1/6 and every unit has pi = 1/2, so every design weight is 2.
#
# Target U^B, three clusters:
#   H1 = {t1, t2, t5}   t1 <- a, b   t2 <- a   t5 has no link of its own
#   H2 = {t3}           t3 <- c, d
#   H3 = {t4}           no links at all: Constraint 2.1 is violated for it
#
# `a` links twice into H1, which is what makes the clustered and the extended
# methods disagree; H1 has three units, which is what makes the singleton
# method disagree with both.

gwsm_source_frame <- function() {
  data.frame(unit = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
}

gwsm_targets <- function() {
  data.frame(
    tid = c("t1", "t2", "t5", "t3", "t4"),
    hh = c("H1", "H1", "H1", "H2", "H3"),
    y = c(1, 10, 100, 1000, 10000),
    stringsAsFactors = FALSE
  )
}

gwsm_links <- function() {
  data.frame(
    unit = c("a", "a", "b", "c", "d"),
    tid = c("t1", "t2", "t1", "t3", "t3"),
    stringsAsFactors = FALSE
  )
}

# One executed tbl_sample per distinct subset of size 2. The seeds are only a
# way to obtain a valid sample object for each subset; the enumeration itself
# is exact, because srswor gives all six subsets probability 1/6.
gwsm_all_samples <- function() {
  frame <- gwsm_source_frame()
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

gwsm_share <- function(sample, within_mode, ...) {
  targets <- gwsm_targets()
  links <- gwsm_links()
  args <- list(
    sample,
    targets = targets, links = links,
    by = c(unit = "unit"), to = c(tid = "tid"),
    multiplicity = quote(complete_links()),
    ...
  )
  # `args$within <- NULL` would drop the element and leave `within` missing,
  # which is a different call. Single-bracket assignment of a list holding
  # NULL is what passes NULL through.
  args["within"] <- list(switch(
    within_mode,
    cluster = quote(hh),
    singleton = NULL,
    extended = quote(extend_links(hh))
  ))
  do.call(share_weights, args)
}

## Expectations, by exact enumeration

test_that("the clustered estimator is exactly unbiased for reachable clusters", {
  samples <- gwsm_all_samples()
  expect_length(samples, 6L)

  totals <- vapply(samples, function(s) {
    r <- gwsm_share(s, "cluster")
    sum(r$.weight * r$y)
  }, numeric(1))

  # H1 and H2 are reachable; H3 is not. Averaging over the six equally likely
  # samples must give their total exactly, not approximately.
  reachable <- sum(gwsm_targets()$y[gwsm_targets()$hh != "H3"])
  expect_equal(mean(totals), reachable)
  expect_equal(mean(totals), 1111)
})

test_that("the downward bias from an unreachable cluster is exactly its total", {
  samples <- gwsm_all_samples()
  totals <- vapply(samples, function(s) {
    r <- gwsm_share(s, "cluster")
    sum(r$.weight * r$y)
  }, numeric(1))

  # A test that only confirmed the good case would pass against an
  # implementation that ignored Constraint 2.1 entirely. Pin the size of the
  # failure, not just its presence.
  population <- sum(gwsm_targets()$y)
  expect_equal(population - mean(totals), 10000)
})

test_that("cluster elimination is unbiased too, and is not the clustered method", {
  samples <- gwsm_all_samples()

  extended <- vapply(samples, function(s) {
    r <- gwsm_share(s, "extended")
    sum(r$.weight * r$y)
  }, numeric(1))
  clustered <- vapply(samples, function(s) {
    r <- gwsm_share(s, "cluster")
    sum(r$.weight * r$y)
  }, numeric(1))

  # Both unbiased over the same reachable population...
  expect_equal(mean(extended), 1111)
  expect_equal(mean(clustered), 1111)
  # ...and different sample by sample. Asserting they agree would be
  # asserting the bug: section 5.3 yields a different weight, not a
  # simplification of the same one.
  expect_false(isTRUE(all.equal(extended, clustered)))
})

test_that("singleton clusters lose the units that have no link of their own", {
  samples <- gwsm_all_samples()
  totals <- vapply(samples, function(s) {
    r <- gwsm_share(s, "singleton")
    sum(r$.weight * r$y)
  }, numeric(1))

  # With every unit its own cluster, t5 has no link and is uncovered, so the
  # bias is H3 plus t5 rather than H3 alone.
  expect_equal(mean(totals), 1111 - 100)
})

test_that("the three within modes give three distinct weights", {
  s <- gwsm_all_samples()[["ac"]]

  w <- lapply(c("cluster", "singleton", "extended"), function(m) {
    r <- gwsm_share(s, m)
    stats::setNames(r$.weight, r$tid)
  })

  # Sample {a, c}: clustered gives H1 the weight 2 * 2/3; extended counts the
  # source units reaching H1 rather than the links into it, giving 2 * 1/2;
  # singleton splits t1 and t2 apart entirely.
  expect_equal(unname(w[[1]][["t1"]]), 4 / 3)
  expect_equal(unname(w[[3]][["t1"]]), 1)
  expect_equal(unname(w[[2]][["t1"]]), 1)
  expect_equal(unname(w[[2]][["t2"]]), 2)
  # t1 and t2 share a cluster, so the clustered and extended forms give them
  # one weight and the singleton form does not.
  expect_equal(unname(w[[1]][["t1"]]), unname(w[[1]][["t2"]]))
  expect_false(isTRUE(all.equal(
    unname(w[[2]][["t1"]]), unname(w[[2]][["t2"]])
  )))
})

## Constraint 2.1: the two findings that point in opposite directions

test_that("a unit with no link of its own carries its cluster's weight", {
  s <- gwsm_all_samples()[["ac"]]
  r <- gwsm_share(s, "cluster")

  t5 <- r[r$tid == "t5", ]
  t1 <- r[r$tid == "t1", ]

  # Lavallee Figure 2.1, unit 7. This is correct, not a defect, and producing
  # a weight for exactly these units is a reason to use the method.
  expect_equal(nrow(t5), 1L)
  expect_identical(t5$.unit_links, 0)
  expect_equal(t5$.weight, t1$.weight)
})

test_that("an unreached cluster contributes no rows rather than zero-weight rows", {
  s <- gwsm_all_samples()[["ac"]]
  r <- gwsm_share(s, "cluster")

  # H2 is reachable but was not reached by {a, c}... c links to t3, so it is.
  # H3 is unreachable and must simply be absent.
  expect_false("t4" %in% r$tid)
  expect_true(all(r$.weight > 0))
})

test_that("the link columns report the unit and the cluster separately", {
  s <- gwsm_all_samples()[["ab"]]
  r <- gwsm_share(s, "cluster")
  h1 <- r[r$hh == "H1", ]

  # L[t1] = 2 (a and b), L[t2] = 1 (a), L[t5] = 0; the cluster denominator is
  # their sum. A single column would conflate the quantity the weight uses
  # with the quantity the coverage diagnostics use.
  expect_identical(
    stats::setNames(h1$.unit_links, h1$tid),
    c(t1 = 2, t2 = 1, t5 = 0)
  )
  expect_true(all(h1$.cluster_links == 3))
})

## The denominator is a choice, and both choices are honored

test_that("a supplied multiplicity that differs from the links is used as given", {
  s <- gwsm_all_samples()[["ab"]]
  targets <- gwsm_targets()
  # The observed link table is incomplete: t1 really has four links in the
  # population, not the two that were recorded.
  targets$L <- c(4, 1, 0, 2, 0)

  supplied <- share_weights(
    s, targets = targets, links = gwsm_links(),
    by = c(unit = "unit"), to = c(tid = "tid"),
    within = hh, multiplicity = L
  )
  counted <- gwsm_share(s, "cluster")

  # Constructed rather than assumed: the two must disagree, or the test would
  # pass against an implementation that ignored the supplied column.
  expect_identical(supplied$.unit_links[supplied$tid == "t1"], 4)
  expect_identical(counted$.unit_links[counted$tid == "t1"], 2)
  expect_false(isTRUE(all.equal(
    supplied$.weight[supplied$tid == "t1"],
    counted$.weight[counted$tid == "t1"]
  )))
  # Cluster denominator 5 rather than 3. Sample {a, b}: `a` carries two links
  # into H1 and `b` one, each at weight 2, so w = 2*(2/5) + 2*(1/5).
  expect_equal(supplied$.weight[supplied$tid == "t1"], 2 * (2 / 5) + 2 * (1 / 5))
  expect_equal(counted$.weight[counted$tid == "t1"], 2 * (2 / 3) + 2 * (1 / 3))
})

test_that("multiplicity has no default and says why", {
  s <- gwsm_all_samples()[["ab"]]

  expect_error(
    share_weights(
      s, gwsm_targets(), gwsm_links(),
      by = c(unit = "unit"), to = c(tid = "tid"), within = hh
    ),
    class = "samplyr_error_share_weights_multiplicity"
  )
})

test_that("within has no default and is never inferred", {
  s <- gwsm_all_samples()[["ab"]]

  expect_error(
    share_weights(
      s, gwsm_targets(), gwsm_links(),
      by = c(unit = "unit"), to = c(tid = "tid"),
      multiplicity = complete_links()
    ),
    class = "samplyr_error_share_weights_within"
  )
})

test_that("the markers refuse to be called on their own", {
  expect_error(complete_links())
  expect_error(extend_links(x))
})

test_that("extend_links refuses a supplied multiplicity that varies in a cluster", {
  s <- gwsm_all_samples()[["ab"]]
  targets <- gwsm_targets()
  targets$L <- c(2, 1, 0, 2, 0)

  # After extension every unit of a cluster has the same links, so a value
  # varying inside a cluster is the un-extended multiplicity.
  expect_error(
    share_weights(
      s, targets, gwsm_links(),
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = extend_links(hh), multiplicity = L
    ),
    class = "samplyr_error_share_weights_multiplicity"
  )
})

## The operator and the record

test_that("the operator reproduces the weights it was recorded with", {
  s <- gwsm_all_samples()[["ab"]]
  r <- gwsm_share(s, "cluster")
  record <- attr(r, "metadata")$weight_share

  expect_identical(
    apply_share_operator(record$operator, s$.weight),
    r$.weight
  )
  expect_identical(record$operator$n_source, nrow(s))
  expect_identical(record$operator$n_target, nrow(r))
})

test_that("the record describes the transformation that was run", {
  s <- gwsm_all_samples()[["ab"]]
  r <- gwsm_share(s, "cluster")
  record <- attr(r, "metadata")$weight_share

  expect_identical(record$within_mode, "cluster")
  expect_identical(record$target_cluster, "hh")
  expect_identical(record$denominator$mode, "complete_links")
  expect_identical(record$denominator$scale, "binary")
  expect_identical(record$target_key_cols, "tid")
  expect_identical(record$source_key_cols, "unit")
  expect_identical(record$coverage$target_scope, "reached")
  # The reader accepts it, which is what the export path will do.
  expect_no_error(prepare_weight_share_record(record, "A test"))
})

test_that("the result keeps the source design and reports the shared contract", {
  s <- gwsm_all_samples()[["ab"]]
  r <- gwsm_share(s, "cluster")

  expect_s3_class(r, "tbl_sample")
  expect_identical(sample_weight_contract(r), "shared")
  # The recorded design still describes selection from the source population.
  expect_identical(get_design(r), get_design(s))
  expect_silent(check_sample_unmodified(r, "test"))
  expect_identical(
    verify_weight_share_alignment(r, attr(r, "metadata")$weight_share), "ok"
  )
})

test_that("coverage is recorded, and the unasked question is distinguishable", {
  s <- gwsm_all_samples()[["ab"]]

  reached <- gwsm_share(s, "cluster")$.weight
  cov_reached <- attr(gwsm_share(s, "cluster"), "metadata")$weight_share$coverage
  cov_pop <- attr(
    gwsm_share(s, "cluster", target_scope = "population"), "metadata"
  )$weight_share$coverage

  expect_identical(cov_reached$target_scope, "reached")
  expect_null(cov_reached$orphan_clusters)
  expect_identical(cov_pop$target_scope, "population")
  # H3 has no links at all, and only population scope can say so.
  expect_identical(cov_pop$orphan_clusters, "H3")
  expect_identical(cov_pop$n_target_clusters, 3L)
})

## Validation

test_that("duplicate link rows are refused rather than double counted", {
  s <- gwsm_all_samples()[["ab"]]
  links <- rbind(gwsm_links(), gwsm_links()[1, ])

  expect_error(
    share_weights(
      s, gwsm_targets(), links,
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = hh, multiplicity = complete_links()
    ),
    class = "samplyr_error_share_weights_links"
  )
})

test_that("a link from a selected unit to an unknown target is refused", {
  s <- gwsm_all_samples()[["ab"]]
  targets <- gwsm_targets()[gwsm_targets()$tid != "t2", ]

  # Dropping t2 from the register leaves a's link pointing nowhere. Silently
  # ignoring it would return H1 without all of its members and understate its
  # weight.
  expect_error(
    share_weights(
      s, targets, gwsm_links(),
      by = c(unit = "unit"), to = c(tid = "tid"),
      within = hh, multiplicity = complete_links()
    ),
    class = "samplyr_error_share_weights_coverage"
  )
})

test_that("keys must exist, match in type, and be complete", {
  s <- gwsm_all_samples()[["ab"]]
  targets <- gwsm_targets()
  links <- gwsm_links()

  expect_error(
    share_weights(s, targets, links, by = c(nope = "unit"),
                  to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_share_weights_keys"
  )
  expect_error(
    share_weights(s, targets, links, by = c(unit = "nope"),
                  to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_share_weights_keys"
  )
  expect_error(
    share_weights(s, targets, links, by = "unit",
                  to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_share_weights_keys"
  )

  numeric_links <- links
  numeric_links$unit <- seq_len(nrow(links))
  expect_error(
    share_weights(s, targets, numeric_links, by = c(unit = "unit"),
                  to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_share_weights_keys"
  )

  na_targets <- targets
  na_targets$tid[1] <- NA
  expect_error(
    share_weights(s, na_targets, links, by = c(unit = "unit"),
                  to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_share_weights_keys"
  )
})

test_that("a target key naming two rows is refused", {
  s <- gwsm_all_samples()[["ab"]]
  targets <- gwsm_targets()
  targets$tid[2] <- "t1"

  expect_error(
    share_weights(s, targets, gwsm_links(), by = c(unit = "unit"),
                  to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_share_weights_keys"
  )
})

test_that("a generated column the register already uses is refused", {
  s <- gwsm_all_samples()[["ab"]]
  targets <- gwsm_targets()
  targets$.unit_links <- 1

  expect_error(
    share_weights(s, targets, gwsm_links(), by = c(unit = "unit"),
                  to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_generated_column_collision"
  )
})

test_that("share_weights refuses a sample whose weights are already shared", {
  s <- gwsm_all_samples()[["ab"]]
  r <- gwsm_share(s, "cluster")

  expect_error(
    share_weights(r, gwsm_targets(), gwsm_links(), by = c(unit = "unit"),
                  to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_share_weights_weight_contract"
  )
})

test_that("share_weights refuses a modified sample and a non-sample", {
  s <- gwsm_all_samples()[["ab"]]

  expect_error(
    share_weights(gwsm_source_frame(), gwsm_targets(), gwsm_links(),
                  by = c(unit = "unit"), to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_share_weights_input"
  )
  expect_error(
    share_weights(s[1, ], gwsm_targets(), gwsm_links(),
                  by = c(unit = "unit"), to = c(tid = "tid"), within = hh,
                  multiplicity = complete_links()),
    class = "samplyr_error_modified_sample"
  )
})

test_that("dots must be empty and later arguments are matched by name", {
  s <- gwsm_all_samples()[["ab"]]

  expect_error(
    share_weights(s, gwsm_targets(), gwsm_links(), c(unit = "unit"),
                  c(tid = "tid"), hh, complete_links()),
    class = "samplyr_error_unnamed_argument"
  )
})

## What the object says about itself

test_that("print and summary say the weights are shared and the design is not", {
  s <- gwsm_all_samples()[["ab"]]
  r <- gwsm_share(s, "cluster", target_scope = "population")

  header <- paste(capture.output(print(r)), collapse = "\n")
  expect_match(header, "shared")
  expect_match(header, "Shared from")

  txt <- paste(capture.output(summary(r)), collapse = "\n")
  expect_match(txt, "not these rows")
  expect_match(txt, "clustered on hh")
  expect_match(txt, "no link of")
  expect_match(txt, "cannot be reached")
})

test_that("a reached-scope summary states that coverage is not established", {
  s <- gwsm_all_samples()[["ab"]]
  r <- gwsm_share(s, "cluster")

  txt <- paste(capture.output(summary(r)), collapse = "\n")
  expect_match(txt, "not established")
  expect_no_match(txt, "cannot be reached")
})

## The gates from G1 apply to what this verb produces

test_that("a real shared sample meets the gates from G1", {
  s <- gwsm_all_samples()[["ab"]]
  r <- gwsm_share(s, "cluster")

  # The linearized route takes it as its source-target contributions.
  expect_false(is_null(attr(as_svydesign(r), "samplyr_weight_share")))
  expect_no_error(as_svrepdesign(r, type = "subbootstrap"))
  expect_error(
    r |> execute(gwsm_source_frame(), seed = 1),
    class = "samplyr_error_execute_weight_contract"
  )
})
