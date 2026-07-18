# Hand-built frame digests for the schema tests. These fixtures pin
# the digest contract independently of digest capture in execute().
# Each builder returns a fresh, valid digest so tests can mutate one
# invariant at a time.

digest_frame_120 <- function() {
  samplyr:::new_digest_frame(
    frame_id = 1L,
    fingerprint_exact = "exact-hash-abc",
    fingerprint_roles = "roles-hash-def",
    n_rows = 120L,
    roles = data.frame(
      column = c("stratum", "cluster", "mos"),
      role = c("strata", "clusters", "mos")
    ),
    scope = "universe"
  )
}

# Single-stage stratified equal-probability selection: 4 strata of 30
# units, 10 selected in each, constant chance 1/3.
digest_fixture_constant <- function() {
  pools <- data.frame(
    pool_id = 1:4,
    parent_unit = NA_integer_,
    stratum = factor(c("A", "B", "C", "D")),
    N = 30L,
    n_target = 10,
    n_expected = 10,
    n_realized = 10L,
    scope = "universe",
    chance_status = "executed",
    chance = 10 / 30
  )
  selected <- data.frame(
    pool_id = rep(1:4, each = 10L),
    unit_id = rep(1:10, times = 4L),
    occurrence = 1L,
    sample_row = 1:40
  )
  stage <- samplyr:::new_digest_stage(
    stage_id = 1L,
    frame_ref = 1L,
    unit_level = "element",
    scope = "universe",
    chance_kind = "inclusion_probability",
    order_kind = "input",
    storage = "constant",
    pools = pools,
    selected = selected,
    strata = "stratum"
  )
  samplyr:::new_frame_digest(list(digest_frame_120()), list(stage))
}

# Two-stage cluster design: stage 1 selects 2 of 6 clusters per
# stratum by PPS (units storage, varying chances, descendants), stage
# 2 selects 3 of 5 elements in each selected cluster (constant).
digest_fixture_units <- function(stage2_scope = "eligible") {
  pools1 <- data.frame(
    pool_id = 1:4,
    parent_unit = NA_integer_,
    stratum = factor(c("A", "B", "C", "D")),
    N = 6L,
    n_target = 2,
    n_expected = 2,
    n_realized = 2L,
    scope = "universe",
    chance_status = "executed"
  )
  chance6 <- c(0.5, 0.5, 0.4, 0.3, 0.2, 0.1) # sums to 2 per pool
  units1 <- data.frame(
    unit_id = 1:24,
    pool_id = rep(1:4, each = 6L),
    unit_order = rep(1:6, times = 4L),
    chance = rep(chance6, times = 4L),
    is_certainty = FALSE,
    n_descendants = 5L
  )
  sel_units <- c(1L, 2L, 7L, 8L, 13L, 14L, 19L, 20L)
  selected1 <- data.frame(
    pool_id = rep(1:4, each = 2L),
    unit_id = sel_units,
    occurrence = 1L
  )
  stage1 <- samplyr:::new_digest_stage(
    stage_id = 1L,
    frame_ref = 1L,
    unit_level = "cluster",
    scope = "universe",
    chance_kind = "inclusion_probability",
    order_kind = "input",
    storage = "units",
    pools = pools1,
    units = units1,
    selected = selected1,
    strata = "stratum"
  )

  pools2 <- data.frame(
    pool_id = 1:8,
    parent_unit = sel_units,
    N = 5L,
    n_target = 3,
    n_expected = 3,
    n_realized = 3L,
    scope = stage2_scope,
    chance_status = "executed",
    chance = 3 / 5
  )
  selected2 <- data.frame(
    pool_id = rep(1:8, each = 3L),
    unit_id = rep(1:3, times = 8L),
    occurrence = 1L
  )
  stage2 <- samplyr:::new_digest_stage(
    stage_id = 2L,
    frame_ref = 1L,
    unit_level = "element",
    scope = stage2_scope,
    chance_kind = "inclusion_probability",
    order_kind = "input",
    storage = "constant",
    pools = pools2,
    selected = selected2
  )
  samplyr:::new_frame_digest(
    list(digest_frame_120()),
    list(stage1, stage2)
  )
}

# Single-stage row-level PPS on a large frame, compressed to a chance
# quantile function (101 points, one pool).
digest_fixture_quantiles <- function() {
  q <- seq(0, 1, length.out = 101)
  chance <- 0.02 + 0.13 * q
  pools <- data.frame(
    pool_id = 1L,
    parent_unit = NA_integer_,
    N = 120L,
    n_target = 10,
    n_expected = mean(chance) * 120,
    n_realized = 10L,
    scope = "universe",
    chance_status = "summarized"
  )
  dist <- data.frame(pool_id = 1L, quantile = q, chance = chance)
  selected <- data.frame(
    pool_id = 1L,
    unit_id = seq(5L, 50L, by = 5L),
    occurrence = 1L
  )
  stage <- samplyr:::new_digest_stage(
    stage_id = 1L,
    frame_ref = 1L,
    unit_level = "element",
    scope = "universe",
    chance_kind = "inclusion_probability",
    order_kind = "input",
    storage = "quantiles",
    pools = pools,
    chance_distribution = dist,
    selected = selected
  )
  samplyr:::new_frame_digest(list(digest_frame_120()), list(stage))
}

# Single-stage with-replacement selection: chances are expected hits,
# two units selected twice (occurrence 2 in the trace).
digest_fixture_wr <- function() {
  pools <- data.frame(
    pool_id = 1L,
    parent_unit = NA_integer_,
    N = 30L,
    n_target = 10,
    n_expected = 10,
    n_realized = 10L,
    scope = "universe",
    chance_status = "executed",
    chance = 10 / 30
  )
  selected <- data.frame(
    pool_id = 1L,
    unit_id = c(1L, 2L, 3L, 4L, 5L, 5L, 6L, 7L, 8L, 8L),
    occurrence = c(1L, 1L, 1L, 1L, 1L, 2L, 1L, 1L, 1L, 2L)
  )
  stage <- samplyr:::new_digest_stage(
    stage_id = 1L,
    frame_ref = 1L,
    unit_level = "element",
    scope = "universe",
    chance_kind = "expected_hits",
    order_kind = "input",
    storage = "constant",
    pools = pools,
    selected = selected
  )
  samplyr:::new_frame_digest(list(digest_frame_120()), list(stage))
}

# Replicated execution: shared pool registry, replicate-specific trace,
# n_realized left NA because it varies by replicate.
digest_fixture_replicated <- function() {
  d <- digest_fixture_constant()
  s <- d$stages[[1]]
  sel <- s$selected
  sel$sample_row <- NULL
  sel2 <- rbind(sel, sel)
  sel2$replicate <- rep(1:2, each = nrow(sel))
  s$selected <- sel2
  s$pools$n_realized <- NA_integer_
  d$stages[[1]] <- s
  d
}

# All valid fixtures, for validator sweeps.
digest_fixtures <- function() {
  list(
    constant = digest_fixture_constant(),
    units = digest_fixture_units(),
    conditional = digest_fixture_units(stage2_scope = "conditional"),
    quantiles = digest_fixture_quantiles(),
    wr = digest_fixture_wr(),
    replicated = digest_fixture_replicated()
  )
}
