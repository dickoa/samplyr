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
  d <- samplyr::exante_digest(design, test_frame)
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
    samplyr::exante_digest(test_frame)
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
    samplyr::exante_digest(test_frame)
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
    samplyr::exante_digest(test_frame)
  expect_equal(sum(d_df$stages[[1]]$pools$n_target), 30)
})

test_that("varying element chances come back as a quantile profile", {
  design <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos)
  d <- samplyr::exante_digest(design, test_frame)
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
    samplyr::exante_digest(frame)

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
    samplyr::exante_digest(frame_cert)
  u <- d$stages[[1]]$units
  expect_identical(u$is_certainty, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(sum(u$chance), 2, tolerance = 1e-9)

  d_wr <- sampling_design() |>
    draw(n = 15, method = "pps_multinomial", mos = mos) |>
    samplyr::exante_digest(test_frame)
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
    samplyr::exante_digest(wr_parent, test_frame),
    class = "samplyr_error_exante_unsupported"
  )

  # An unclustered non-final stage has no identity to link stage 2 to, so
  # the design cannot be executed at all. The preview refuses it with the
  # class execution refuses it with, not a preview-specific reason.
  element_parent <- sampling_design() |>
    add_stage() |> draw(n = 50) |>
    add_stage() |> draw(n = 10)
  expect_error(
    samplyr::exante_digest(element_parent, test_frame),
    class = "samplyr_error_stage_parent_id"
  )
  expect_error(
    execute(element_parent, test_frame, seed = 1),
    class = "samplyr_error_stage_parent_id"
  )

  expect_error(
    samplyr::exante_digest(sampling_design(), test_frame),
    class = "samplyr_error_exante_unsupported"
  )

  missing_col <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = not_there)
  expect_error(
    samplyr::exante_digest(missing_col, test_frame),
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
    samplyr::exante_digest(phase1)
  st <- d$stages[[1]]
  expect_identical(d$frames[[1]]$n_rows, 60L)
  expect_equal(sum(st$pools$N), 60L)
  expect_equal(sum(st$pools$n_target), 20)
})

test_that("the synthetic three-stage design resolves ex-ante", {
  frame <- synth_three_stage_frame()
  design <- synth_three_stage_design()
  d <- samplyr::exante_digest(design, frame)

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
  # Same capped stage-3 pool as the executed-digest fixture.
  s <- suppressWarnings(design |> execute(frame, seed = 7))
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
    samplyr::exante_digest(test_frame)
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
  d <- samplyr::exante_digest(design, test_frame)
  expect_identical(d$stages[[1]]$probabilities, "approximate")
  expect_identical(d$stages[[2]]$probabilities, "exact")
})

## frame_summary(design, frame): the ex-ante preview

test_that("a preview reports what the design would do, without drawing", {
  design <- exante_two_stage()
  preview <- frame_summary(design, test_frame, detail = "stage")
  recorded <- frame_summary(
    execute(design, test_frame, seed = 8), detail = "stage"
  )

  expect_s3_class(preview, "tbl_df")
  expect_identical(names(preview), names(recorded))
  expect_identical(vapply(preview, class, character(1)),
                   vapply(recorded, class, character(1)))
  expect_equal(preview$N, recorded$N)
  expect_equal(preview$n_target, recorded$n_target)
  expect_equal(preview$n_expected, recorded$n_expected)

  # No realization exists, so these are NA rather than a measured zero.
  expect_true(all(is.na(preview$n_realized)))
  expect_true(all(is.na(preview$take_rate)))
  expect_false(anyNA(recorded$n_realized))

  # The same holds at the other two detail levels, and the shapes still
  # match the recorded report column for column.
  pool_preview <- frame_summary(design, test_frame, detail = "pool")
  pool_recorded <- frame_summary(
    execute(design, test_frame, seed = 8), detail = "pool"
  )
  expect_identical(names(pool_preview), names(pool_recorded))
  expect_true(all(is.na(pool_preview$n_realized)))
  expect_true(all(is.na(pool_preview$take_rate)))
  expect_false(anyNA(pool_recorded$n_realized))

  unit_preview <- frame_summary(design, test_frame, detail = "unit")
  unit_recorded <- frame_summary(
    execute(design, test_frame, seed = 8), detail = "unit"
  )
  expect_identical(names(unit_preview), names(unit_recorded))
  expect_true(nrow(unit_preview) > 0)
  expect_true(all(is.na(unit_preview$is_selected)))
  expect_true(all(is.na(unit_preview$n_hits)))
  expect_false(anyNA(unit_recorded$is_selected))
})

test_that("a preview draws nothing and leaves the RNG alone", {
  design <- exante_two_stage()

  # No withr::with_seed() here: it restores the stream and would hide an
  # advance. The seed is read directly before and after.
  set.seed(99)
  invisible(stats::runif(1))
  before <- .Random.seed
  invisible(frame_summary(design, test_frame, detail = "pool"))
  expect_identical(before, .Random.seed)

  # And when no seed existed at all, none is created.
  if (exists(".Random.seed", envir = globalenv())) {
    rm(".Random.seed", envir = globalenv())
  }
  invisible(frame_summary(design, test_frame, detail = "pool"))
  expect_false(exists(".Random.seed", envir = globalenv()))

  # The inputs are left as they were.
  frame_before <- test_frame
  design_before <- design
  invisible(frame_summary(design, test_frame))
  expect_identical(frame_before, test_frame)
  expect_identical(design_before, design)
})

test_that("a multistage stage row is the expected size, not the candidate total", {
  frame <- data.frame(
    cl = rep(sprintf("c%03d", seq_len(100)), each = 20),
    id = seq_len(2000)
  )
  design <- sampling_design() |>
    cluster_by(cl) |>
    draw(n = 10) |>
    add_stage() |>
    draw(n = 5)

  preview <- frame_summary(design, frame, detail = "stage")
  sample <- execute(design, frame, seed = 1)

  # 10 of 100 clusters, 5 each. Every candidate parent has a pool, so the
  # unweighted total would be 500; the design draws 50.
  expect_identical(preview$n_pools, c(1L, 100L))
  expect_equal(preview$n_target, c(10, 50))
  expect_equal(preview$N, c(100, 200))
  expect_identical(nrow(sample), 50L)
  expect_equal(preview$n_target, frame_summary(sample, detail = "stage")$n_target)

  # The per-pool table stays conditional on the parent: what a selected
  # cluster would give, which is what field planning needs.
  pools <- frame_summary(design, frame, detail = "pool")
  expect_identical(nrow(pools[pools$stage == 2, ]), 100L)
  expect_equal(unique(pools$n_target[pools$stage == 2]), 5)
})

test_that("the preview weights three stages through the ancestry", {
  frame <- data.frame(
    cl = rep(sprintf("c%03d", seq_len(100)), each = 20),
    hh = rep(sprintf("h%04d", seq_len(400)), each = 5),
    id = seq_len(2000)
  )
  design <- sampling_design() |>
    cluster_by(cl) |> draw(n = 10) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> draw(n = 3)

  preview <- frame_summary(design, frame, detail = "stage")
  # 0.1 * 0.5 * 1200 candidate takes = 60.
  expect_equal(preview$n_target, c(10, 20, 60))
  expect_identical(nrow(execute(design, frame, seed = 5)), 60L)
})

test_that("the preview cannot accept a frame execute() refuses", {
  # The preview is a third preflight; it must apply the same executable
  # layer as execute() and validate_frame(), or it does not describe what
  # execution would do.
  design <- sampling_design() |> draw(n = 1)

  duplicated_names <- data.frame(a = 1:3, b = 4:6)
  names(duplicated_names) <- c("a", "a")
  reserved <- data.frame(a = 1:3, .weight = 1)
  stripped <- execute(
    sampling_design() |> draw(n = 2), data.frame(a = 1:10), seed = 1
  )
  class(stripped) <- c("tbl_df", "tbl", "data.frame")

  cases <- list(
    samplyr_error_frame_duplicate_names = duplicated_names,
    samplyr_error_frame_reserved_names = reserved,
    samplyr_error_stripped_sample_frame = stripped
  )
  for (class in names(cases)) {
    expect_error(frame_summary(design, cases[[class]]), class = class)
    expect_error(
      samplyr::exante_digest(design, cases[[class]]), class = class
    )
    expect_error(execute(design, cases[[class]], seed = 1), class = class)
  }

  # An intact previous-phase sample is a legitimate frame, not a stripped
  # one, and still previews.
  phase1 <- execute(sampling_design() |> draw(n = 8), test_frame, seed = 1)
  expect_s3_class(
    frame_summary(sampling_design() |> draw(n = 2), phase1), "tbl_df"
  )
})

test_that("each stage records the frame it would select from", {
  # Hardcoding frame_ref = 1 made a three-register digest claim every stage
  # read the first register, while carrying three frame records.
  registers <- list(mf_schools(), mf_classes(), mf_students())
  digest <- samplyr::exante_digest(mf_design(), registers)

  expect_length(digest$frames, 3L)
  expect_identical(
    vapply(digest$stages, `[[`, integer(1), "frame_ref"), 1:3
  )
  expect_identical(
    vapply(digest$frames, `[[`, integer(1), "n_rows"),
    vapply(registers, nrow, integer(1))
  )
  # Each stage's referenced record is the register it was given.
  for (k in seq_along(digest$stages)) {
    ref <- digest$stages[[k]]$frame_ref
    expect_identical(digest$frames[[ref]]$n_rows, nrow(registers[[k]]))
  }

  # Row counts alone cannot tell the register apart from the linked frame the
  # stage selects from, because linking carries columns without changing the
  # count. The record is the register as supplied, so its fingerprint is the
  # register's.
  expect_identical(
    vapply(digest$frames, `[[`, character(1), "fingerprint_exact"),
    vapply(registers, samplyr:::frame_content_hash, character(1))
  )

  # The executed digest agrees, which is what makes the two comparable.
  executed <- get_frame_digest(
    execute(mf_design(), mf_schools(), mf_classes(), mf_students(), seed = 7)
  )
  expect_identical(
    vapply(executed$stages, `[[`, integer(1), "frame_ref"),
    vapply(digest$stages, `[[`, integer(1), "frame_ref")
  )
  # fingerprint_exact is the key a continuation merges registries on, so the
  # two digests describing one set of registers must agree on it, and on the
  # roles read off those registers.
  expect_identical(
    vapply(digest$frames, `[[`, character(1), "fingerprint_exact"),
    vapply(executed$frames, `[[`, character(1), "fingerprint_exact")
  )
  expect_identical(
    lapply(digest$frames, `[[`, "roles"),
    lapply(executed$frames, `[[`, "roles")
  )

  # One shared hierarchy is one record, referenced by every stage. The three
  # stages read the same supplied table, so deduplication must still collapse
  # them even though their linked frames differ.
  hierarchy <- mf_hierarchy()
  shared <- samplyr::exante_digest(mf_design(), hierarchy)
  expect_length(shared$frames, 1L)
  expect_identical(
    vapply(shared$stages, `[[`, integer(1), "frame_ref"), rep(1L, 3)
  )
  expect_identical(
    shared$frames[[1]]$fingerprint_exact,
    samplyr:::frame_content_hash(hierarchy)
  )
  expect_identical(
    shared$frames[[1]]$fingerprint_exact,
    get_frame_digest(
      execute(mf_design(), hierarchy, seed = 7)
    )$frames[[1]]$fingerprint_exact
  )
})

test_that("the preview takes registers as well as one hierarchy", {
  design <- exante_two_stage()
  hierarchy <- frame_summary(design, test_frame, detail = "stage")
  listed <- frame_summary(design, list(test_frame), detail = "stage")
  expect_identical(as.data.frame(hierarchy), as.data.frame(listed))

  # A genuine one-register-per-stage list, not the singleton spelling of a
  # shared hierarchy: the two must agree pool for pool.
  registers <- frame_summary(
    mf_design(), list(mf_schools(), mf_classes(), mf_students()),
    detail = "pool"
  )
  shared <- frame_summary(mf_design(), mf_hierarchy(), detail = "pool")
  expect_identical(as.data.frame(registers), as.data.frame(shared))

  # The frame grammar is the shared one.
  expect_error(
    frame_summary(design, list()),
    class = "samplyr_error_frame_count"
  )
  expect_error(
    frame_summary(design, list(test_frame, 42)),
    class = "samplyr_error_frame_not_data_frame"
  )
})

test_that("frame decides preview, whatever x already carries", {
  design <- exante_two_stage()
  sample <- execute(design, test_frame, seed = 8)

  # An executed sample previewed against a frame reports expectations.
  expect_true(all(is.na(frame_summary(sample, test_frame)$n_realized)))
  # Without a frame it reports what happened.
  expect_false(anyNA(frame_summary(sample)$n_realized))

  # An unexecuted design without a frame still says it has no digest, and
  # now says what to do about it.
  bare <- sampling_design() |> draw(n = 5)
  err <- expect_error(frame_summary(bare), class = "samplyr_error_no_digest")
  expect_match(cli::ansi_strip(conditionMessage(err)), "frame")
})

test_that("capped agrees between the preview and the execution", {
  # n_expected and n_target are computed by different paths and differ in
  # the last bits; an exact `<` reported one Neyman stratum as capped in
  # the preview and not in the execution.
  # This is the reproduction: Neyman over bfa_eas leaves n_expected and
  # n_target differing by ~7e-15 in the "Est" stratum, which an exact `<`
  # called capped in the preview and not capped in the execution.
  design <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = bfa_eas_variance) |>
    draw(n = 300)
  preview <- frame_summary(design, bfa_eas, detail = "pool")
  recorded <- frame_summary(
    execute(design, bfa_eas, seed = 3), detail = "pool"
  )
  expect_identical(preview$capped, recorded$capped)
  expect_false(any(preview$capped))

  # A pool that genuinely cannot supply its target is still reported.
  short <- data.frame(stratum = c(rep("A", 5), rep("B", 200)))
  capped <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = c(A = 20, B = 50))
  pools <- frame_summary(capped, short, detail = "pool")
  expect_identical(pools$capped[order(as.character(pools$stratum))],
                   c(TRUE, FALSE))
})
