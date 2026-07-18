# Digest capture in execute(): the assembled manifest must describe
# the executed selection exactly, and collecting it must not change
# the sample.

digest_of <- function(x) samplyr:::get_frame_digest(x)

# Pure data comparison: everything except execution-time metadata
# (executed_at differs between runs; the digest itself is the object
# under test).
strip_to_data <- function(x) {
  df <- as.data.frame(x)
  attributes(df) <- attributes(df)[c("names", "class", "row.names")]
  df
}

test_that("digest collection is observational", {
  designs <- list(
    srs = sampling_design() |> draw(n = 20),
    strat = sampling_design() |>
      stratify_by(stratum, alloc = "proportional") |>
      draw(n = 40),
    pps = sampling_design() |> draw(n = 10, method = "pps_brewer", mos = mos),
    twostage = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
      draw(n = 2, method = "pps_brewer", mos = mos) |>
      add_stage() |> draw(n = 3),
    bernoulli = sampling_design() |>
      draw(frac = 0.2, method = "bernoulli", on_empty = "silent")
  )
  for (nm in names(designs)) {
    none <- execute(designs[[nm]], test_frame, seed = 7,
                    frame_digest = "none")
    summ <- execute(designs[[nm]], test_frame, seed = 7,
                    frame_digest = "summary")
    full <- execute(designs[[nm]], test_frame, seed = 7,
                    frame_digest = "full")
    expect_identical(strip_to_data(none), strip_to_data(summ))
    expect_identical(strip_to_data(none), strip_to_data(full))
    # Same rows, weights, and RNG consumption regardless of mode.
    expect_identical(attr(none, "seed"), attr(summ, "seed"))
    expect_null(digest_of(none))
    expect_false(is.null(digest_of(summ)))
  }
})

test_that("every built digest passes the validator", {
  s <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 11)
  expect_no_error(samplyr:::validate_frame_digest(digest_of(s)))
  s2 <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 11, frame_digest = "full")
  expect_no_error(samplyr:::validate_frame_digest(digest_of(s2)))
})

## Fixture 1: srswor single stage (constant storage)

test_that("srswor records one constant pool and the frame fingerprint", {
  s <- sampling_design() |> draw(n = 20) |> execute(test_frame, seed = 1)
  d <- digest_of(s)
  expect_identical(d$status, "complete")
  expect_identical(d$privacy$mode, "summary")
  expect_length(d$frames, 1)
  expect_identical(
    d$frames[[1]]$fingerprint_exact,
    samplyr:::frame_content_hash(test_frame)
  )
  expect_identical(d$frames[[1]]$n_rows, 120L)
  expect_identical(d$frames[[1]]$scope, "universe")

  st <- d$stages[[1]]
  expect_identical(st$storage, "constant")
  expect_identical(st$unit_level, "element")
  expect_identical(st$chance_kind, "inclusion_probability")
  p <- st$pools
  expect_identical(nrow(p), 1L)
  expect_identical(p$N, 120L)
  expect_equal(p$n_target, 20)
  expect_equal(p$expected_n, 20)
  expect_identical(p$n_realized, 20L)
  expect_equal(p$chance, 20 / 120)
  expect_identical(nrow(st$selected), 20L)
  expect_identical(st$selected$occurrence, rep(1L, 20))
})

## Fixture 2: stratified srswor + allocation (quadruple per pool)

test_that("stratified allocation records the quadruple per stratum pool", {
  s <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40) |>
    execute(test_frame, seed = 2)
  d <- digest_of(s)
  st <- d$stages[[1]]
  expect_identical(st$strata, "stratum")
  p <- st$pools
  expect_identical(nrow(p), 4L)
  expect_s3_class(p$stratum, "factor")
  expect_setequal(as.character(p$stratum), c("A", "B", "C", "D"))
  expect_identical(p$N, rep(30L, 4))
  expect_equal(p$n_target, rep(10, 4))
  expect_equal(p$expected_n, rep(10, 4))
  expect_identical(p$n_realized, rep(10L, 4))
  # Frame roles cover the stratification column.
  roles <- d$frames[[1]]$roles
  expect_true("stratum" %in% roles$column)
  expect_identical(roles$role[roles$column == "stratum"], "strata")
})

## Fixture 3: unclustered PPS (quantiles, mean-faithful, certainty)

test_that("row-level PPS compresses to a mean-faithful distribution", {
  s <- sampling_design() |>
    draw(n = 100, method = "pps_brewer", mos = households) |>
    execute(bfa_eas, seed = 3)
  d <- digest_of(s)
  st <- d$stages[[1]]
  expect_identical(st$storage, "quantiles")
  expect_identical(st$pools$chance_status, "summarized")
  dist <- st$chance_distribution
  expect_identical(nrow(dist), 101L)
  expect_identical(sum(dist$n_units), st$pools$N)
  # The n_units-weighted sum reproduces expected_n exactly.
  expect_equal(sum(dist$chance * dist$n_units), st$pools$expected_n,
               tolerance = 1e-10)
  expect_equal(st$pools$expected_n, 100, tolerance = 1e-8)
  # Selected unit ids are executed-order positions within the pool.
  expect_identical(nrow(st$selected), 100L)
  expect_true(all(st$selected$unit_id >= 1 &
                    st$selected$unit_id <= st$pools$N))
})

test_that("certainty selections surface in the compressed distribution", {
  s <- sampling_design() |>
    draw(n = 12, method = "pps_brewer", mos = mos, certainty_size = 180) |>
    execute(test_frame, seed = 4)
  d <- digest_of(s)
  st <- d$stages[[1]]
  expect_identical(st$storage, "quantiles")
  dist <- st$chance_distribution
  # test_frame has 5 rows with mos >= 180: chance 1 mass is retained.
  expect_identical(sum(dist$n_units[dist$chance >= 1 - 1e-9]), 5L)
  expect_identical(st$pools$n_realized, 12L)
})

## Fixture 4: random-size methods (expected differs from realized)

test_that("random-size stages record no target and the realized size", {
  s <- sampling_design() |>
    draw(frac = 0.1, method = "bernoulli", on_empty = "silent") |>
    execute(test_frame, seed = 5)
  d <- digest_of(s)
  p <- d$stages[[1]]$pools
  expect_true(is.na(p$n_target))
  expect_equal(p$expected_n, 12)
  expect_identical(p$n_realized, nrow(s))
  expect_equal(d$stages[[1]]$pools$chance, 0.1)

  s2 <- sampling_design() |>
    draw(frac = 0.1, method = "pps_poisson", mos = mos,
         on_empty = "silent") |>
    execute(test_frame, seed = 5)
  p2 <- digest_of(s2)$stages[[1]]$pools
  expect_true(is.na(p2$n_target))
  expect_identical(p2$n_realized, nrow(s2))
})

## Fixture 5: with-replacement methods (expected hits, multiplicity)

test_that("with-replacement stages record expected hits and occurrences", {
  s <- sampling_design() |>
    draw(n = 15, method = "pps_multinomial", mos = mos) |>
    execute(test_frame, seed = 6)
  d <- digest_of(s)
  st <- d$stages[[1]]
  expect_identical(st$chance_kind, "expected_hits")
  expect_equal(st$pools$expected_n, 15, tolerance = 1e-8)
  # One trace row per draw; occurrences count repeat hits.
  expect_identical(nrow(st$selected), 15L)
  expect_identical(st$pools$n_realized, 15L)
  hit_counts <- table(st$selected$unit_id)
  for (u in names(hit_counts)) {
    occ <- st$selected$occurrence[st$selected$unit_id == as.integer(u)]
    expect_identical(sort(occ), seq_len(hit_counts[[u]]))
  }
})

## Fixture 6: two-stage cluster (units storage, descendants, linkage)

test_that("cluster stages store the anonymous unit registry", {
  s <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 8)
  d <- digest_of(s)

  st1 <- d$stages[[1]]
  expect_identical(st1$storage, "units")
  expect_identical(st1$unit_level, "cluster")
  expect_identical(st1$scope, "universe")
  expect_identical(nrow(st1$units), 24L)
  expect_identical(st1$units$n_descendants, rep(5L, 24))
  expect_identical(nrow(st1$selected), 8L)

  # The single universe frame lets stage 2 resolve the pools of
  # unselected clusters from the design: 8 executed + 16 resolved.
  st2 <- d$stages[[2]]
  expect_identical(st2$scope, "universe")
  expect_identical(nrow(st2$pools), 24L)
  executed <- st2$pools$chance_status != "design_resolved"
  expect_identical(sum(executed), 8L)
  # Executed pools hang off exactly the selected stage-1 units;
  # resolved pools cover the remaining clusters with chance and no
  # realization.
  expect_setequal(
    st2$pools$parent_unit[executed], st1$selected$unit_id
  )
  expect_setequal(st2$pools$parent_unit, st1$units$unit_id)
  expect_identical(st2$pools$N, rep(5L, 24))
  expect_identical(st2$pools$n_realized[executed], rep(3L, 8))
  expect_identical(st2$pools$n_realized[!executed], rep(0L, 16))
  expect_equal(st2$pools$chance, rep(3 / 5, 24))
  expect_identical(st2$pools$scope[!executed], rep("universe", 16))
})

test_that("the unit registry chances reconcile with sample weights", {
  s <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 9)
  d <- digest_of(s)
  st1 <- d$stages[[1]]
  sel_chance <- st1$units$chance[
    match(st1$selected$unit_id, st1$units$unit_id)
  ]
  weight_chance <- 1 / unique(
    as.data.frame(s)[, c("cluster", ".weight_1")]
  )$.weight_1
  expect_equal(sort(sel_chance), sort(weight_chance), tolerance = 1e-10)
  # Unselected units keep their resolved chances: the digest knows
  # what the sample cannot.
  expect_identical(nrow(st1$units), 24L)
  expect_equal(
    sum(st1$units$chance[st1$units$pool_id == 1]),
    st1$pools$expected_n[1]
  )
})

test_that("full mode keeps exact unit chances for element PPS stages", {
  s <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 10, frame_digest = "full")
  d <- digest_of(s)
  st <- d$stages[[1]]
  expect_identical(st$storage, "units")
  expect_identical(d$privacy$mode, "full")
  expect_identical(nrow(st$units), 120L)
  sel_chance <- st$units$chance[
    match(st$selected$unit_id, st$units$unit_id)
  ]
  expect_equal(sort(sel_chance), sort(1 / s$.weight), tolerance = 1e-10)
})

test_that("element-stage parents are absent by design", {
  # Stage 1 selects elements (no clusters); stage-2 pools cannot
  # reference an identifiable parent unit.
  frame <- data.frame(
    id = 1:200,
    region = rep(c("North", "South"), each = 100),
    value = 1
  )
  s <- sampling_design() |>
    add_stage() |> stratify_by(region, alloc = "proportional") |>
    draw(n = 40) |>
    add_stage() |> draw(n = 5) |>
    execute(frame, seed = 12)
  d <- digest_of(s)
  expect_false(is.null(d))
  expect_true(all(is.na(d$stages[[2]]$pools$parent_unit)))
})

## Fixture 7: Gambia three-stage reference (structural)

test_that("the Gambia reference design yields the expected scope chain", {
  gmb_path <- file.path("..", "..", "dev", "gmb_compounds.rda")
  skip_if_not(
    file.exists(gmb_path),
    "Gambia reference fixture is kept in the dev tree only"
  )
  load(gmb_path)

  s <- sampling_design() |>
    add_stage("Districts") |> stratify_by(region) |>
    cluster_by(district) |>
    draw(n = 5, method = "pps_systematic", mos = district_pop) |>
    add_stage("Villages") |> stratify_by(phc) |> cluster_by(village) |>
    draw(n = 2, method = "pps_systematic", mos = village_pop) |>
    add_stage("Compounds") |> draw(n = 6) |>
    execute(gmb_compounds, seed = 1991)
  expect_identical(nrow(s), 360L)

  # Eligible basis: denominators cover what this realization could
  # reach. The single universe frame also lets every stage resolve
  # its unreached pools from the design, so the universe basis has
  # complete denominators too (the proto20 promise).
  fs <- frame_summary(s)
  expect_equal(fs$N, c(36, 278, 2206))
  expect_equal(fs$n_realized, c(15, 60, 360))
  expect_equal(fs$n_target, c(15, 60, 360))
  expect_identical(fs$scope, rep("universe", 3))
  expect_identical(fs$unit_level, c("cluster", "cluster", "element"))

  fu <- frame_summary(s, scope = "universe")
  expect_equal(fu$N, c(36, 655, 19344))
  expect_equal(fu$n_realized, c(15, 60, 360))

  # Stage-2 pools are parent x phc, never merged across parents that
  # share a stratum label: 30 executed under the 15 selected
  # districts, the rest design-resolved.
  d <- digest_of(s)
  st2 <- d$stages[[2]]
  executed2 <- st2$pools$chance_status != "design_resolved"
  expect_identical(sum(executed2), 30L)
  expect_identical(
    as.integer(table(st2$pools$parent_unit[executed2])),
    rep(2L, 15)
  )
  # The full village registry, with the 27 whole-take villages the
  # bare sample cannot know about (stage-3 chance 1).
  expect_identical(nrow(st2$units), 655L)
  st3 <- d$stages[[3]]
  expect_identical(nrow(st3$pools), 655L)
  expect_identical(sum(st3$pools$chance >= 1 - 1e-9), 27L)
  # Descendant counts are ultimate frame rows (compounds).
  st1 <- d$stages[[1]]
  expect_identical(sum(st1$units$n_descendants), nrow(gmb_compounds))
  expect_identical(sum(st2$units$n_descendants), nrow(gmb_compounds))
  sel2_desc <- st2$units$n_descendants[
    match(st2$selected$unit_id, st2$units$unit_id)
  ]
  expect_identical(sum(sel2_desc), 2206L)
  # The verified sample_row locator maps the trace to the rows.
  expect_identical(st3$selected$sample_row, 1:360)
})

## Fixture 8: on_empty designs (empty pools stay present)

test_that("a zero-selection realization keeps its pool in the digest", {
  frame <- data.frame(id = 1:4, y = 1)
  s <- withr::with_seed(30, suppressWarnings(
    sampling_design() |>
      draw(frac = 1e-9, method = "bernoulli", on_empty = "silent") |>
      execute(frame)
  ))
  expect_identical(nrow(s), 0L)
  d <- digest_of(s)
  expect_no_error(samplyr:::validate_frame_digest(d))
  p <- d$stages[[1]]$pools
  expect_identical(p$N, 4L)
  expect_identical(p$n_realized, 0L)
  expect_null(d$stages[[1]]$selected)
  fs <- frame_summary(s)
  expect_equal(fs$n_realized, 0)
})

test_that("stages never reached are absent from the digest", {
  # Stage 1 selects zero clusters; stage 2 never executes and must be
  # absent, not present with invented zeroes.
  s <- suppressWarnings(
    sampling_design() |>
      add_stage() |> cluster_by(cluster) |>
      draw(frac = 1e-9, method = "bernoulli", on_empty = "silent") |>
      add_stage() |> draw(n = 2) |>
      execute(test_frame, seed = 31)
  )
  expect_identical(nrow(s), 0L)
  d <- digest_of(s)
  expect_length(d$stages, 1)
  expect_identical(d$stages[[1]]$pools$n_realized, 0L)
})

## Fixture 9: replicates, continuation, two-phase

test_that("replicated executions share structure and split traces", {
  r <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 20) |>
    execute(test_frame, seed = 40, reps = 3)
  d <- digest_of(r)
  expect_no_error(samplyr:::validate_frame_digest(d))
  st <- d$stages[[1]]
  # One shared pool registry; fixed-size allocation is kept because it
  # is identical across replicates.
  expect_identical(nrow(st$pools), 4L)
  expect_identical(st$pools$n_realized, rep(5L, 4))
  expect_identical(sort(unique(st$selected$replicate)), 1:3)
  expect_identical(nrow(st$selected), 60L)
  # Per-replicate traces match the stacked sample.
  for (rep in 1:3) {
    expect_identical(
      sum(st$selected$replicate == rep),
      sum(r$.replicate == rep)
    )
  }
})

test_that("replicate-varying realized sizes are NA, not a guess", {
  r <- sampling_design() |>
    draw(frac = 0.1, method = "bernoulli", on_empty = "silent") |>
    execute(test_frame, seed = 41, reps = 3)
  d <- digest_of(r)
  st <- d$stages[[1]]
  expect_true(is.na(st$pools$n_realized))
  expect_true(is.na(st$pools$n_target))
  # The traces still carry each replicate's realization.
  counts <- table(factor(st$selected$replicate, levels = 1:3))
  expect_identical(
    as.integer(counts),
    as.integer(table(factor(r$.replicate, levels = 1:3)))
  )
})

test_that("replicated multistage digests keep the shared stage prefix", {
  # Later-stage pools hang off each replicate's realized parents, so
  # they are replicate-specific: the merged manifest keeps stage 1 and
  # is explicitly marked partial, rather than stacking traces under one
  # replicate's pool registry.
  r <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 70, reps = 2)
  d <- digest_of(r)
  expect_no_error(samplyr:::validate_frame_digest(d))
  expect_identical(d$status, "partial")
  expect_length(d$stages, 1)
  st <- d$stages[[1]]
  expect_identical(st$storage, "units")
  expect_identical(sort(unique(st$selected$replicate)), 1:2)
  # Each replicate's selected clusters are traced against the shared
  # universe registry.
  expect_identical(nrow(st$selected), 16L)
})

test_that("a continuation extends the prior digest with linked stages", {
  part <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 42, stages = 1)
  d1 <- digest_of(part)
  expect_length(d1$stages, 1)
  # Selected cluster keys are recorded for continuation linkage; they
  # name only clusters already visible on the sample rows.
  expect_true("key" %in% names(d1$stages[[1]]$selected))

  cont <- part |> execute(test_frame, seed = 43)
  d <- digest_of(cont)
  expect_no_error(samplyr:::validate_frame_digest(d))
  expect_length(d$stages, 2)
  # The prior stage manifest is carried unchanged.
  expect_identical(d$stages[[1]]$pools, d1$stages[[1]]$pools)
  expect_identical(d$stages[[1]]$units, d1$stages[[1]]$units)
  # New pools hang off the previously selected units.
  expect_setequal(
    d$stages[[2]]$pools$parent_unit,
    d1$stages[[1]]$selected$unit_id
  )
  expect_identical(d$stages[[2]]$scope, "eligible")
  # One frame, supplied twice, recorded once.
  expect_length(d$frames, 1)
  fs <- frame_summary(cont)
  expect_equal(fs$n_realized, c(8, 24))
})

test_that("a replicated continuation keeps the prior manifest as partial", {
  part <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 80, stages = 1, reps = 2)
  cont <- part |> execute(test_frame, seed = 81)
  d <- digest_of(cont)
  expect_false(is.null(d))
  expect_identical(d$status, "partial")
  # The manifest describes the prior stage; the continued stages are
  # replicate-specific and are not manufactured.
  expect_length(d$stages, 1)
  expect_no_error(samplyr:::validate_frame_digest(d))
})

test_that("a continuation without a prior digest records none", {
  part <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 44, stages = 1, frame_digest = "none")
  cont <- part |> execute(test_frame, seed = 45)
  expect_null(digest_of(cont))
})

test_that("a modified input sample does not extend its stale digest", {
  part <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 46, stages = 1)
  part$.weight <- part$.weight * 2
  cont <- suppressWarnings(part |> execute(test_frame, seed = 47))
  expect_null(digest_of(cont))
})

test_that("a second phase records the phase-1 sample as its universe", {
  p1 <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40) |>
    execute(test_frame, seed = 48)
  p2 <- sampling_design() |>
    draw(n = 10) |>
    execute(p1, seed = 49)
  d <- digest_of(p2)
  expect_no_error(samplyr:::validate_frame_digest(d))
  # The phase-2 population is the realized phase-1 sample.
  expect_identical(d$frames[[1]]$n_rows, 40L)
  expect_identical(d$stages[[1]]$scope, "universe")
  expect_identical(d$stages[[1]]$pools$N, 40L)
  expect_identical(d$stages[[1]]$pools$n_realized, 10L)
})

## Fixture 10: registered custom methods

test_that("custom WOR methods pass their resolved chances through", {
  on.exit(sondage::unregister_method("digest_wor"), add = TRUE)
  # Deterministic: always takes the n largest-chance units.
  sondage::register_method(
    "digest_wor", "wor",
    sample_fn = function(pik, n = NULL, prn = NULL, ...) {
      order(pik, decreasing = TRUE)[seq_len(n)]
    }
  ,
    probabilities = "exact"
  )
  s <- sampling_design() |>
    draw(n = 10, method = "pps_digest_wor", mos = mos) |>
    execute(test_frame, seed = 50, frame_digest = "full")
  d <- digest_of(s)
  st <- d$stages[[1]]
  expect_identical(st$chance_kind, "inclusion_probability")
  expect_identical(st$storage, "units")
  # The recorded chances are exactly the vector samplyr resolved and
  # handed to the registered method.
  expect_equal(
    st$units$chance,
    sondage::inclusion_prob(test_frame$mos, 10),
    tolerance = 1e-12
  )
  # Deterministic method: selected units are the top-10 chances.
  expect_setequal(
    st$selected$unit_id,
    order(st$units$chance, decreasing = TRUE)[1:10]
  )
  expect_equal(st$pools$expected_n, 10, tolerance = 1e-8)
})

test_that("custom WR methods record expected hits", {
  on.exit(sondage::unregister_method("digest_wr"), add = TRUE)
  sondage::register_method(
    "digest_wr", "wr",
    sample_fn = function(hits, n = NULL, prn = NULL, ...) {
      sample.int(length(hits), size = n, replace = TRUE, prob = hits)
    }
  ,
    probabilities = "exact"
  )
  s <- sampling_design() |>
    draw(n = 12, method = "pps_digest_wr", mos = mos) |>
    execute(test_frame, seed = 51)
  d <- digest_of(s)
  st <- d$stages[[1]]
  expect_identical(st$chance_kind, "expected_hits")
  expect_equal(st$pools$expected_n, 12, tolerance = 1e-8)
  expect_identical(nrow(st$selected), 12L)
  expect_true(all(st$selected$occurrence >= 1))
})

test_that("custom random-size methods record no target", {
  on.exit(sondage::unregister_method("digest_rand"), add = TRUE)
  # Poisson-type: independent selection against the resolved chances.
  sondage::register_method(
    "digest_rand", "wor",
    sample_fn = function(pik, n = NULL, prn = NULL, ...) {
      which(runif(length(pik)) < pik)
    },
    fixed_size = FALSE
  ,
    probabilities = "exact"
  )
  s <- sampling_design() |>
    draw(n = 20, method = "pps_digest_rand", mos = mos,
         on_empty = "silent") |>
    execute(test_frame, seed = 52)
  d <- digest_of(s)
  p <- d$stages[[1]]$pools
  expect_true(is.na(p$n_target))
  expect_identical(p$n_realized, nrow(s))
})

test_that("custom balanced methods record chances and diagnostics", {
  on.exit(sondage::unregister_method("digest_bal"), add = TRUE)
  # Delegates to the cube landing so it behaves like a real method.
  sondage::register_method(
    "digest_bal", "balanced",
    sample_fn = function(pik, aux = NULL, spread = NULL, ...) {
      sondage::balanced_wor(pik, aux = aux)$sample
    }
  ,
    probabilities = "exact"
  )
  s <- sampling_design() |>
    draw(n = 12, method = "balanced_digest_bal", mos = mos, aux = y) |>
    execute(test_frame, seed = 53)
  d <- digest_of(s)
  st <- d$stages[[1]]
  expect_identical(st$chance_kind, "inclusion_probability")
  bal <- st$diagnostics$balance
  expect_identical(bal$term, "y")
  expect_equal(bal$target, sum(test_frame$y))
  expect_equal(bal$residual, bal$realized - bal$target)
})

test_that("custom cluster selection keeps registry and linkage", {
  on.exit(sondage::unregister_method("digest_wor"), add = TRUE)
  sondage::register_method(
    "digest_wor", "wor",
    sample_fn = function(pik, n = NULL, prn = NULL, ...) {
      order(pik, decreasing = TRUE)[seq_len(n)]
    }
  ,
    probabilities = "exact"
  )
  s <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_digest_wor", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 54)
  d <- digest_of(s)
  st1 <- d$stages[[1]]
  expect_identical(st1$storage, "units")
  expect_identical(nrow(st1$units), 24L)
  # Registered methods resolve unreached pools too (samplyr computes
  # their chances); executed pools hang off the selected units.
  p2 <- d$stages[[2]]$pools
  expect_setequal(
    p2$parent_unit[p2$chance_status != "design_resolved"],
    st1$selected$unit_id
  )
  # Deterministic per stratum: the two largest-mos clusters win.
  for (pool in 1:4) {
    in_pool <- st1$units$pool_id == pool
    top2 <- st1$units$unit_id[in_pool][
      order(st1$units$chance[in_pool], decreasing = TRUE)[1:2]
    ]
    sel <- st1$selected$unit_id[st1$selected$pool_id == pool]
    expect_setequal(sel, top2)
  }
})

## Design-resolved universe expansion: guards

test_that("expansion requires a resolvable design", {
  # Allocation methods need stratum statistics the resolution refuses
  # to replay: the stage stays eligible-only.
  s <- sampling_design() |>
    add_stage() |> cluster_by(cluster) |> draw(n = 6) |>
    add_stage() |> stratify_by(stratum, alloc = "proportional") |>
    draw(n = 3) |>
    execute(test_frame, seed = 20)
  st2 <- digest_of(s)$stages[[2]]
  expect_identical(st2$scope, "eligible")
  expect_true(all(st2$pools$chance_status != "design_resolved"))
})

test_that("expansion requires a single universe frame", {
  frame2 <- test_frame
  s <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, frame2[frame2$stratum != "Z", ], seed = 21)
  st2 <- digest_of(s)$stages[[2]]
  # Different frame objects with equal content share one registry
  # record, so this still expands; a genuinely different later frame
  # must not.
  frame3 <- test_frame
  frame3$extra <- 1
  s2 <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, frame3, seed = 21)
  st2b <- digest_of(s2)$stages[[2]]
  expect_identical(st2b$scope, "eligible")
  expect_identical(st2$scope, "universe")
})

test_that("expansion stops behind a with-replacement parent stage", {
  s <- sampling_design() |>
    add_stage() |> cluster_by(cluster) |>
    draw(n = 4, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 2) |>
    execute(test_frame, seed = 22)
  st2 <- digest_of(s)$stages[[2]]
  expect_true(all(st2$pools$chance_status != "design_resolved"))
})

test_that("resolved expansion is observational", {
  d <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3)
  none <- execute(d, test_frame, seed = 23, frame_digest = "none")
  summ <- execute(d, test_frame, seed = 23, frame_digest = "summary")
  expect_identical(strip_to_data(none), strip_to_data(summ))
})

## sample_row locator and parent_occurrence

test_that("the final-stage trace carries a verified sample_row", {
  s <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage() |> draw(n = 3) |>
    execute(test_frame, seed = 24)
  sel <- digest_of(s)$stages[[2]]$selected
  expect_identical(sel$sample_row, seq_len(nrow(s)))
  # The locator lands each trace row on its own sample row: the
  # sample's cluster at that row matches the pool's parent key.
  d <- digest_of(s)
  key_of_unit <- d$stages[[1]]$selected$key[
    match(
      d$stages[[2]]$pools$parent_unit[
        match(sel$pool_id, d$stages[[2]]$pools$pool_id)
      ],
      d$stages[[1]]$selected$unit_id
    )
  ]
  expect_identical(as.character(s$cluster[sel$sample_row]), key_of_unit)

  s1 <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40) |>
    execute(test_frame, seed = 25)
  sel1 <- digest_of(s1)$stages[[1]]$selected
  expect_identical(sel1$sample_row, seq_len(nrow(s1)))
})

test_that("cluster-final stages carry no sample_row", {
  s <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 6) |>
    execute(test_frame, seed = 26)
  expect_false(
    "sample_row" %in% names(digest_of(s)$stages[[1]]$selected)
  )
})

test_that("pools under a with-replacement parent record the occurrence", {
  s <- sampling_design() |>
    add_stage() |> cluster_by(cluster) |>
    draw(n = 6, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 2) |>
    execute(test_frame, seed = 27)
  d <- digest_of(s)
  st1 <- d$stages[[1]]
  st2 <- d$stages[[2]]
  # One stage-2 pool per selected draw, not per distinct parent.
  expect_identical(nrow(st2$pools), 6L)
  expect_true("parent_occurrence" %in% names(st2$pools))
  # parent_occurrence is the draw index (the sample's .draw_k), so
  # (parent, occurrence) pairs are the stage-1 trace in draw order.
  expect_setequal(
    paste(st2$pools$parent_unit, st2$pools$parent_occurrence),
    paste(st1$selected$unit_id, seq_len(nrow(st1$selected)))
  )
})

## Fixture 11: balanced and spatial diagnostics

test_that("cube stages record balance targets and residuals", {
  s <- sampling_design() |>
    draw(n = 20, method = "cube", mos = mos, aux = y) |>
    execute(test_frame, seed = 60)
  d <- digest_of(s)
  bal <- d$stages[[1]]$diagnostics$balance
  expect_identical(names(bal),
                   c("pool_id", "term", "target", "realized", "residual"))
  expect_equal(bal$target, sum(test_frame$y))
  # The realized value is the HT estimate from the recorded selection.
  expect_equal(bal$realized, sum(s$y * s$.weight), tolerance = 1e-8)
})

test_that("stratified cluster cube balances on cluster totals", {
  s <- sampling_design() |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 2, method = "cube", mos = mos, aux = y) |>
    execute(test_frame, seed = 61)
  d <- digest_of(s)
  bal <- d$stages[[1]]$diagnostics$balance
  expect_identical(nrow(bal), 4L)
  expect_equal(
    bal$target,
    vapply(c("A", "B", "C", "D"), function(h) {
      sum(test_frame$y[test_frame$stratum == h])
    }, numeric(1)),
    ignore_attr = TRUE
  )
})

test_that("bound() constraints record expected, bounds, and realized", {
  s <- sampling_design() |>
    draw(n = 20, method = "cube", mos = mos, aux = c(y, bound(stratum))) |>
    execute(test_frame, seed = 62)
  d <- digest_of(s)
  bounds <- d$stages[[1]]$diagnostics$bounds
  expect_identical(
    names(bounds),
    c("pool_id", "term", "level", "expected", "lower", "upper",
      "realized", "satisfied")
  )
  expect_identical(bounds$term, rep("stratum", 4))
  expect_setequal(bounds$level, c("A", "B", "C", "D"))
  expect_true(all(bounds$satisfied))
  expect_true(all(bounds$realized >= bounds$lower &
                    bounds$realized <= bounds$upper))
  # Realized counts match the sample composition.
  for (h in c("A", "B", "C", "D")) {
    expect_identical(
      bounds$realized[bounds$level == h],
      sum(s$stratum == h)
    )
  }
})

test_that("spatial stages record coordinate metadata, not coordinates", {
  frame <- test_frame
  set.seed(99)
  frame$lon <- runif(120)
  frame$lat <- runif(120)
  frame$lat[2] <- frame$lat[1]
  frame$lon[2] <- frame$lon[1]
  s <- sampling_design() |>
    draw(n = 15, method = "lpm2", mos = mos, spread = c(lon, lat)) |>
    execute(frame, seed = 63)
  d <- digest_of(s)
  sp <- d$stages[[1]]$diagnostics$spatial
  expect_identical(sp$variables, c("lon", "lat"))
  expect_identical(sp$dimensions, 2L)
  expect_equal(sp$ranges$min, c(min(frame$lon), min(frame$lat)))
  expect_equal(sp$ranges$max, c(max(frame$lon), max(frame$lat)))
  expect_identical(sp$n_duplicate_coordinates, 1L)
  # No population coordinate cloud is retained anywhere.
  expect_null(d$stages[[1]]$units)
  expect_false(any(c("lon", "lat") %in% names(d$stages[[1]]$pools)))
})

## Synthetic three-stage reference: same structural assertions as the
## Gambia fixture, from a frame reproducible in code (helper-fixtures.R)

test_that("the synthetic three-stage design yields the expected scope chain", {
  frame <- synth_three_stage_frame()
  s <- synth_three_stage_design() |> execute(frame, seed = 7)

  n_districts <- length(unique(frame$district))
  n_villages <- length(unique(frame$village))
  whole_takes <- sum(table(frame$village) <= 3L)

  fu <- frame_summary(s, scope = "universe")
  expect_equal(fu$N, c(n_districts, n_villages, nrow(frame)))
  expect_equal(fu$n_realized, c(6, 24, nrow(s)))
  expect_identical(fu$scope, rep("universe", 3))

  d <- digest_of(s)
  st2 <- d$stages[[2]]
  st3 <- d$stages[[3]]
  # Two executed phc pools under each of the 6 selected districts,
  # never merged across parents; the rest design-resolved.
  executed2 <- st2$pools$chance_status != "design_resolved"
  expect_identical(
    as.integer(table(st2$pools$parent_unit[executed2])),
    rep(2L, 6)
  )
  expect_identical(nrow(st2$units), n_villages)
  expect_identical(nrow(st3$pools), n_villages)
  expect_identical(sum(st3$pools$chance >= 1 - 1e-9), whole_takes)
  # Descendant counts are ultimate frame rows at every cluster stage.
  expect_identical(sum(d$stages[[1]]$units$n_descendants), nrow(frame))
  expect_identical(sum(st2$units$n_descendants), nrow(frame))
  expect_identical(st3$selected$sample_row, seq_len(nrow(s)))
})

## Fixture 10b: probabilities declarations (sondage >= 0.8.7)

test_that("an unknown-probabilities method is refused at draw", {
  on.exit(sondage::unregister_method("weight_only"), add = TRUE)
  # The registration-docs counterexample: successive sampling treats
  # pik as a selection weight, so 1/pik design weights would be
  # systematically biased. The declaration makes draw() refuse.
  sondage::register_method(
    "weight_only", "wor",
    sample_fn = function(pik, n = NULL, prn = NULL, ...) {
      sample.int(length(pik), size = n, prob = pik)
    },
    probabilities = "unknown"
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_weight_only", mos = mos),
    class = "samplyr_error_unknown_probabilities"
  )
})

test_that("a declared exact tier records pik", {
  on.exit(sondage::unregister_method("declared_exact"), add = TRUE)
  sondage::register_method(
    "declared_exact", "wor",
    sample_fn = function(pik, n = NULL, prn = NULL, ...) {
      order(pik, decreasing = TRUE)[seq_len(n)]
    },
    probabilities = "exact"
  )
  s <- sampling_design() |>
    draw(n = 10, method = "pps_declared_exact", mos = mos) |>
    execute(test_frame, seed = 55, frame_digest = "full")
  st <- digest_of(s)$stages[[1]]
  expect_false(identical(unique(st$pools$chance_status), "unavailable"))
  expect_equal(
    st$units$chance,
    sondage::inclusion_prob(test_frame$mos, 10),
    tolerance = 1e-12
  )
})

test_that("digest stages record the probabilities tier", {
  s <- sampling_design() |>
    add_stage("Clusters") |> cluster_by(cluster) |>
    draw(n = 6, method = "pps_sps", mos = mos) |>
    add_stage("Units") |> draw(n = 3) |>
    execute(test_frame, seed = 21)
  d <- samplyr:::get_frame_digest(s)
  expect_identical(d$stages[[1]]$probabilities, "approximate")
  expect_identical(d$stages[[2]]$probabilities, "exact")

  fs <- frame_summary(s)
  expect_identical(fs$probabilities, c("approximate", "exact"))
})
