# Selection traces: every internal wrapper returns list(sample, trace)
# where the trace records the full resolved chance vector, the selected
# positions in executed order, and how the frame was partitioned into
# pools. These tests drive the wrappers directly with specs pulled from
# designs and check the trace against the sample it came with.

stage_spec <- function(design) design$stages[[1]]

trace_of <- function(res) res$trace

test_that("unstratified srswor records a constant-chance pool", {
  d <- sampling_design() |> draw(n = 20)
  s <- stage_spec(d)
  res <- withr::with_seed(1, samplyr:::sample_units(test_frame, s$strata, s$draw_spec))
  tr <- trace_of(res)
  expect_identical(tr$type, "pool")
  expect_identical(tr$N, 120L)
  expect_equal(tr$n_target, 20)
  expect_equal(tr$chance, rep(20 / 120, 120))
  expect_identical(tr$chance_kind, "inclusion_probability")
  expect_identical(tr$order_kind, "input")
  expect_null(tr$perm)
  expect_identical(anyDuplicated(tr$selected), 0L)
  # The trace's selected positions are the sample rows, in order.
  expect_identical(res$sample$id, test_frame$id[tr$selected])
  expect_equal(res$sample$.weight, 1 / tr$chance[tr$selected])
})

test_that("requested size is recorded before capping to the pool", {
  d <- sampling_design() |> draw(n = 200)
  s <- stage_spec(d)
  res <- suppressWarnings(withr::with_seed(
    1, samplyr:::sample_units(test_frame, s$strata, s$draw_spec)
  ))
  tr <- trace_of(res)
  expect_equal(tr$n_target, 200)
  expect_identical(length(tr$selected), 120L)
})

test_that("the stratified srswor fast path records one pool per stratum", {
  d <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40)
  s <- stage_spec(d)
  res <- withr::with_seed(2, samplyr:::sample_units(test_frame, s$strata, s$draw_spec))
  tr <- trace_of(res)
  expect_identical(tr$type, "split")
  expect_identical(tr$by, "stratum")
  expect_length(tr$groups, 4)
  keys <- vapply(tr$groups, function(g) as.character(g$keys$stratum), character(1))
  expect_setequal(keys, c("A", "B", "C", "D"))
  # Group rows partition the frame.
  all_rows <- sort(unlist(lapply(tr$groups, function(g) g$rows)))
  expect_identical(all_rows, seq_len(120L))
  for (g in tr$groups) {
    leaf <- g$node
    expect_identical(leaf$type, "pool")
    expect_identical(leaf$N, 30L)
    expect_equal(leaf$chance, rep(10 / 30, 30))
    expect_identical(length(leaf$selected), 10L)
  }
})

test_that("stratified PPS pools carry the full resolved chance vector", {
  d <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 4, method = "pps_brewer", mos = mos)
  s <- stage_spec(d)
  res <- withr::with_seed(3, samplyr:::sample_units(test_frame, s$strata, s$draw_spec))
  tr <- trace_of(res)
  expect_identical(tr$type, "split")
  offset <- 0L
  for (g in tr$groups) {
    leaf <- g$node
    expect_identical(leaf$N, 30L)
    expect_length(leaf$chance, 30)
    expect_equal(sum(leaf$chance), 4, tolerance = 1e-8)
    expect_true(all(leaf$chance > 0 & leaf$chance <= 1))
    # 1/.weight of this stratum's sample rows equals the resolved
    # chance at the selected positions, in selection order.
    rows <- offset + seq_along(leaf$selected)
    expect_equal(
      1 / res$sample$.weight[rows],
      leaf$chance[leaf$selected]
    )
    # Selected frame rows line up through the split mapping.
    expect_identical(
      res$sample$id[rows],
      test_frame$id[g$rows[leaf$selected]]
    )
    offset <- offset + length(leaf$selected)
  }
})

test_that("control ordering is recorded as a pool-local permutation", {
  d <- sampling_design() |> draw(n = 10, control = desc(y))
  s <- stage_spec(d)
  res <- withr::with_seed(4, samplyr:::sample_units(test_frame, s$strata, s$draw_spec))
  tr <- trace_of(res)
  expect_identical(tr$order_kind, "control")
  expect_identical(sort(tr$perm), seq_len(120L))
  # perm maps executed order back to input order: position i of the
  # executed pool is input row perm[i].
  expect_identical(res$sample$id, test_frame$id[tr$perm[tr$selected]])
  expect_equal(test_frame$y[tr$perm], sort(test_frame$y, decreasing = TRUE))
})

test_that("certainty selection records chance one and the rule positions", {
  # test_frame has 5 rows with mos >= 180; n = 12 leaves room for a
  # probability draw among the rest.
  d <- sampling_design() |>
    draw(n = 12, method = "pps_brewer", mos = mos, certainty_size = 180)
  s <- stage_spec(d)
  res <- withr::with_seed(5, samplyr:::sample_units(test_frame, s$strata, s$draw_spec))
  tr <- trace_of(res)
  expect_identical(tr$type, "pool")
  expect_true(length(tr$cert_rule) > 0)
  expect_equal(tr$chance[tr$cert_rule], rep(1, length(tr$cert_rule)))
  expect_true(all(tr$cert_rule %in% tr$selected))
  expect_true(all(tr$chance[tr$selected] > 0))
  expect_equal(1 / res$sample$.weight, tr$chance[tr$selected])
  expect_identical(res$sample$id, test_frame$id[tr$selected])
  # The sample's certainty flags match the rule positions.
  expect_identical(
    res$sample$.certainty,
    tr$selected %in% tr$cert_rule
  )
})

test_that("with-replacement traces record expected hits and repeats", {
  d <- sampling_design() |> draw(n = 10, method = "pps_multinomial", mos = mos)
  s <- stage_spec(d)
  res <- withr::with_seed(6, samplyr:::sample_units(test_frame, s$strata, s$draw_spec))
  tr <- trace_of(res)
  expect_identical(tr$chance_kind, "expected_hits")
  expect_equal(sum(tr$chance), 10, tolerance = 1e-8)
  expect_identical(length(tr$selected), 10L)
  expect_identical(res$sample$id, test_frame$id[tr$selected])
  # Expected hits are the reciprocal per-draw weights.
  expect_equal(1 / res$sample$.weight, tr$chance[tr$selected])
})

test_that("random-size traces keep the capped per-unit probabilities", {
  d <- sampling_design() |>
    draw(frac = 0.1, method = "bernoulli", on_empty = "silent")
  s <- stage_spec(d)
  res <- withr::with_seed(7, samplyr:::sample_units(test_frame, s$strata, s$draw_spec))
  tr <- trace_of(res)
  expect_equal(tr$chance, rep(0.1, 120))
  expect_identical(res$sample$id, test_frame$id[tr$selected])
})

test_that("cluster selection wraps the units trace with cluster identity", {
  d <- sampling_design() |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos)
  s <- stage_spec(d)
  res <- withr::with_seed(8, samplyr:::sample_clusters(
    test_frame, s$strata, s$clusters, s$draw_spec
  ))
  tr <- trace_of(res)
  expect_identical(tr$type, "clusters")
  expect_identical(tr$by, "cluster")
  expect_length(tr$keys, 24)
  expect_identical(tr$sizes, rep(5L, 24))
  expect_identical(
    as.character(test_frame$cluster[tr$first_rows]),
    unlist(tr$keys, use.names = FALSE)
  )
  inner <- tr$node
  expect_identical(inner$type, "split")
  expect_identical(inner$by, "stratum")
  expect_length(inner$groups, 4)
  for (g in inner$groups) {
    leaf <- g$node
    expect_identical(leaf$N, 6L)
    expect_equal(sum(leaf$chance), 2, tolerance = 1e-8)
    expect_identical(length(leaf$selected), 2L)
    # rows index the one-row-per-cluster frame; compose down to the
    # full frame through first_rows.
    selected_clusters <- tr$first_rows[g$rows[leaf$selected]]
    expect_true(all(
      as.character(test_frame$cluster[selected_clusters]) %in%
        as.character(res$sample$cluster)
    ))
  }
})

test_that("within-cluster selection records one pool per parent", {
  d <- sampling_design() |> draw(n = 3)
  s <- stage_spec(d)
  res <- withr::with_seed(9, samplyr:::sample_within_clusters(
    test_frame, s$strata, s$draw_spec, "cluster"
  ))
  tr <- trace_of(res)
  expect_identical(tr$type, "split")
  expect_identical(tr$by, "cluster")
  expect_length(tr$groups, 24)
  for (g in tr$groups) {
    expect_identical(g$node$type, "pool")
    expect_identical(g$node$N, 5L)
    expect_equal(g$node$chance, rep(3 / 5, 5))
    expect_identical(length(g$node$selected), 3L)
  }
  # Non-fast path (frac): same shape, per-pool draw_sample leaves.
  d2 <- sampling_design() |> draw(frac = 0.4)
  s2 <- stage_spec(d2)
  res2 <- withr::with_seed(10, samplyr:::sample_within_clusters(
    test_frame, s2$strata, s2$draw_spec, "cluster"
  ))
  tr2 <- trace_of(res2)
  expect_length(tr2$groups, 24)
  expect_identical(tr2$groups[[1]]$node$type, "pool")
  expect_equal(tr2$groups[[1]]$node$chance, rep(2 / 5, 5))
})

test_that("stratified cube records one pool per stratum from the joint draw", {
  d <- sampling_design() |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 2, method = "cube", mos = mos, aux = mos)
  s <- stage_spec(d)
  res <- withr::with_seed(11, samplyr:::sample_clusters(
    test_frame, s$strata, s$clusters, s$draw_spec
  ))
  tr <- res$trace$node
  expect_identical(tr$type, "split")
  expect_length(tr$groups, 4)
  for (g in tr$groups) {
    leaf <- g$node
    expect_identical(leaf$N, 6L)
    expect_equal(leaf$n_target, 2)
    expect_equal(sum(leaf$chance), 2, tolerance = 1e-8)
    expect_identical(length(leaf$selected), 2L)
    expect_identical(leaf$order_kind, "input")
  }
})

test_that("empty selections leave an empty trace, not a missing one", {
  frame <- data.frame(id = 1:2, y = c(1, 1))
  d <- sampling_design() |>
    draw(frac = 1e-9, method = "bernoulli", on_empty = "silent")
  s <- stage_spec(d)
  res <- withr::with_seed(12, samplyr:::sample_units(frame, s$strata, s$draw_spec))
  tr <- trace_of(res)
  expect_identical(nrow(res$sample), 0L)
  expect_identical(tr$type, "pool")
  expect_identical(tr$N, 2L)
  expect_length(tr$selected, 0)
  expect_equal(tr$chance, rep(1e-9, 2))
})
