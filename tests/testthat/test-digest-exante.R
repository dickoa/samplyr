# The ex-ante digest: resolved from (design, frame) without drawing,
# mirroring what execution would resolve. The card built on it lives
# in the samplens package.

exante_two_stage <- function() {
  sampling_design("Two-stage") |>
    add_stage("Clusters") |> stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage("Units") |> draw(n = 3)
}

test_that("the ex-ante digest resolves what execution would resolve", {
  design <- exante_two_stage()
  d <- samplyr::build_exante_digest(design, test_frame)
  expect_identical(d$status, "complete")
  expect_identical(length(d$stages), 2L)
  expect_identical(d$frames[[1]]$n_rows, 120L)

  st1 <- d$stages[[1]]
  expect_identical(st1$scope, "universe")
  expect_identical(st1$storage, "units")
  expect_identical(nrow(st1$pools), 4L)
  expect_identical(nrow(st1$units), 24L)
  expect_identical(unique(st1$pools$chance_status), "design_resolved")
  expect_identical(sum(st1$pools$n_realized), 0L)
  expect_null(st1$selected)

  # Per-stratum brewer chances are the inclusion probabilities of the
  # six cluster sizes at n = 2, in frame order.
  cl_mos <- test_frame$mos[seq(1, 120, by = 5)]
  for (p in 1:4) {
    u <- st1$units[st1$units$pool_id == p, ]
    expect_equal(
      u$chance,
      sondage::inclusion_prob(cl_mos[(p - 1) * 6 + 1:6], 2),
      tolerance = 1e-12
    )
    expect_identical(u$n_descendants, rep(5L, 6))
  }

  # Stage 2: one pool per cluster over the whole universe, constant
  # 3/5, parents in stage-1 unit order.
  st2 <- d$stages[[2]]
  expect_identical(st2$storage, "constant")
  expect_identical(nrow(st2$pools), 24L)
  expect_equal(st2$pools$chance, rep(3 / 5, 24))
  expect_equal(st2$pools$n_target, rep(3, 24))
  expect_identical(st2$pools$parent_unit, st1$units$unit_id)

  # The executed digest (with its universe expansion) agrees pool for
  # pool; unit order may differ, values may not.
  s <- design |> execute(test_frame, seed = 8)
  ed <- samplyr::get_frame_digest(s)
  expect_equal(
    sort(ed$stages[[1]]$units$chance),
    sort(st1$units$chance),
    tolerance = 1e-12
  )
  est2 <- ed$stages[[2]]$pools
  expect_equal(
    sort(paste(est2$N, est2$n_target, round(est2$n_expected, 9))),
    sort(paste(
      st2$pools$N, st2$pools$n_target, round(st2$pools$n_expected, 9)
    ))
  )
})

test_that("stratum allocation and per-stratum sizes are replayed", {
  d_alloc <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40) |>
    samplyr::build_exante_digest(test_frame)
  st <- d_alloc$stages[[1]]
  expect_identical(st$storage, "constant")
  expect_equal(st$pools$n_target, rep(10, 4))
  expect_equal(st$pools$chance, rep(1 / 3, 4))
  expect_equal(sum(st$pools$n_expected), 40)
  ed <- samplyr::get_frame_digest(fix_strat_prop)
  expect_equal(st$pools$n_target, ed$stages[[1]]$pools$n_target)

  d_named <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = c(A = 5, B = 10, C = 15, D = 20)) |>
    samplyr::build_exante_digest(test_frame)
  pools <- d_named$stages[[1]]$pools
  expect_equal(
    pools$n_target[match(c("A", "B", "C", "D"),
                         as.character(pools$stratum))],
    c(5, 10, 15, 20)
  )

  d_df <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = data.frame(stratum = c("A", "B", "C", "D"),
                        n = c(4, 6, 8, 12))) |>
    samplyr::build_exante_digest(test_frame)
  expect_equal(sum(d_df$stages[[1]]$pools$n_target), 30)
})

test_that("varying element chances come back as a quantile profile", {
  design <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos)
  d <- samplyr::build_exante_digest(design, test_frame)
  st <- d$stages[[1]]
  expect_identical(st$storage, "quantiles")
  expect_identical(unique(st$pools$chance_status), "design_resolved")
  dist <- st$chance_distribution
  expect_identical(sum(dist$n_units), 120L)
  expect_equal(sum(dist$chance * dist$n_units), 10, tolerance = 1e-9)

  # Deterministic chances: the executed digest binned the same vector.
  ed <- samplyr::get_frame_digest(fix_pps_brewer)
  expect_equal(dist$chance, ed$stages[[1]]$chance_distribution$chance,
               tolerance = 1e-12)
  expect_identical(dist$n_units,
                   ed$stages[[1]]$chance_distribution$n_units)
})

test_that("ex-ante mixed compact and varying pools preserve pool sizes", {
  frame <- data.frame(
    id = seq_len(24),
    stratum = rep(c("equal", "varying"), each = 12),
    mos = c(rep(1, 12), seq_len(12))
  )
  digest <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 4, method = "pps_brewer", mos = mos) |>
    samplyr::build_exante_digest(frame)

  stage <- digest$stages[[1]]
  counts <- tapply(
    stage$chance_distribution$n_units,
    stage$chance_distribution$pool_id,
    sum
  )
  expect_identical(
    unname(as.integer(counts[as.character(stage$pools$pool_id)])),
    stage$pools$N
  )
  represented <- vapply(
    split(stage$chance_distribution, stage$chance_distribution$pool_id),
    function(pool) sum(pool$chance * pool$n_units),
    numeric(1)
  )
  expect_equal(
    unname(represented[as.character(stage$pools$pool_id)]),
    stage$pools$n_expected
  )
})

test_that("certainty and with-replacement designs resolve ex-ante", {
  frame_cert <- data.frame(
    cluster = rep(sprintf("c%d", 1:5), each = 2),
    mos = rep(c(1000, 10, 10, 10, 10), each = 2)
  )
  d <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_systematic", mos = mos) |>
    samplyr::build_exante_digest(frame_cert)
  u <- d$stages[[1]]$units
  expect_identical(u$is_certainty, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(sum(u$chance), 2, tolerance = 1e-9)

  d_wr <- sampling_design() |>
    draw(n = 15, method = "pps_multinomial", mos = mos) |>
    samplyr::build_exante_digest(test_frame)
  st <- d_wr$stages[[1]]
  expect_identical(st$chance_kind, "expected_hits")
  expect_equal(sum(st$pools$n_expected), 15, tolerance = 1e-9)
})

test_that("realization-dependent designs are refused", {
  wr_parent <- sampling_design() |>
    add_stage() |> cluster_by(cluster) |>
    draw(n = 4, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 2)
  expect_error(
    samplyr::build_exante_digest(wr_parent, test_frame),
    class = "samplyr_error_exante_unsupported"
  )

  element_parent <- sampling_design() |>
    add_stage() |> draw(n = 50) |>
    add_stage() |> draw(n = 10)
  expect_error(
    samplyr::build_exante_digest(element_parent, test_frame),
    class = "samplyr_error_exante_unsupported"
  )

  expect_error(
    samplyr::build_exante_digest(sampling_design(), test_frame),
    class = "samplyr_error_exante_unsupported"
  )

  missing_col <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = not_there)
  expect_error(
    samplyr::build_exante_digest(missing_col, test_frame),
    "not_there"
  )
})

test_that("a phase-2 design resolves ex-ante over a phase-1 sample", {
  phase1 <- sampling_design() |>
    draw(n = 60) |>
    execute(test_frame, seed = 1)
  d <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 20) |>
    samplyr::build_exante_digest(phase1)
  st <- d$stages[[1]]
  expect_identical(d$frames[[1]]$n_rows, 60L)
  expect_equal(sum(st$pools$N), 60L)
  expect_equal(sum(st$pools$n_target), 20)
})

test_that("the synthetic three-stage design resolves ex-ante", {
  frame <- synth_three_stage_frame()
  design <- synth_three_stage_design()
  d <- samplyr::build_exante_digest(design, frame)

  expect_identical(
    nrow(d$stages[[1]]$units), length(unique(frame$district))
  )
  expect_identical(
    nrow(d$stages[[2]]$units), length(unique(frame$village))
  )
  expect_identical(sum(d$stages[[1]]$units$n_descendants), nrow(frame))
  expect_equal(sum(d$stages[[1]]$pools$n_expected), 6)
  # Whole-take villages: every compound of a village with at most 3
  # compounds is a certainty selection.
  expect_identical(
    sum(d$stages[[3]]$pools$chance >= 1 - 1e-9),
    sum(table(frame$village) <= 3L)
  )

  # The executed digest's universe expansion resolves the same
  # chances the ex-ante digest resolves.
  s <- design |> execute(frame, seed = 7)
  ed <- samplyr::get_frame_digest(s)
  expect_equal(
    sort(d$stages[[2]]$units$chance),
    sort(ed$stages[[2]]$units$chance),
    tolerance = 1e-9
  )
  expect_equal(
    sort(round(d$stages[[3]]$pools$n_expected, 9)),
    sort(round(ed$stages[[3]]$pools$n_expected, 9))
  )
})

test_that("probability declarations gate the ex-ante digest", {
  on.exit(sondage::unregister_method("exante_exact"), add = TRUE)
  on.exit(sondage::unregister_method("exante_weight"), add = TRUE)
  sondage::register_method(
    "exante_exact", "wor",
    sample_fn = function(pik, n = NULL, prn = NULL, ...) {
      order(pik, decreasing = TRUE)[seq_len(n)]
    },
    probabilities = "exact"
  )
  sondage::register_method(
    "exante_weight", "wor",
    sample_fn = function(pik, n = NULL, prn = NULL, ...) {
      sample.int(length(pik), size = n, prob = pik)
    },
    probabilities = "unknown"
  )

  d <- sampling_design() |>
    draw(n = 10, method = "pps_exante_exact", mos = mos) |>
    samplyr::build_exante_digest(test_frame)
  dist <- d$stages[[1]]$chance_distribution
  expect_equal(sum(dist$chance * dist$n_units), 10, tolerance = 1e-9)

  # A FALSE method never reaches the ex-ante builder: draw() refuses
  # the design itself.
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_exante_weight", mos = mos),
    class = "samplyr_error_unknown_probabilities"
  )
})

test_that("the ex-ante digest records the probabilities tier", {
  design <- sampling_design() |>
    add_stage("Clusters") |> cluster_by(cluster) |>
    draw(n = 6, method = "pps_pareto", mos = mos) |>
    add_stage("Units") |> draw(n = 3)
  d <- samplyr::build_exante_digest(design, test_frame)
  expect_identical(d$stages[[1]]$probabilities, "approximate")
  expect_identical(d$stages[[2]]$probabilities, "exact")
})
