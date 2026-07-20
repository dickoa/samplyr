# Digest schema: constructors, validator, accessors, and
# frame_summary() on hand-built digests (helper-digest.R).

expect_digest_error <- function(digest, what) {
  expect_error(
    samplyr:::validate_frame_digest(digest),
    class = paste0("samplyr_error_digest_", what)
  )
}

## Constructors

test_that("constructors produce the documented shape", {
  d <- digest_fixture_constant()
  expect_named(d, c("version", "status", "privacy", "frames", "stages"))
  expect_identical(d$version, 2L)
  expect_identical(d$status, "complete")
  expect_identical(d$privacy$mode, "summary")
  expect_false(d$privacy$stable_keys)
  expect_false(d$privacy$labels_retained)
  expect_length(d$frames, 1)
  expect_identical(d$frames[[1]]$frame_id, 1L)
  expect_identical(d$frames[[1]]$n_rows, 120L)
  expect_length(d$stages, 1)
  expect_identical(d$stages[[1]]$stage_id, 1L)
  expect_identical(d$stages[[1]]$storage, "constant")
})

test_that("all hand-built fixtures pass validation", {
  for (name in names(digest_fixtures())) {
    d <- digest_fixtures()[[name]]
    expect_no_error(samplyr:::validate_frame_digest(d))
  }
})

## Validator: top-level structure

test_that("validator rejects a non-list digest", {
  expect_digest_error("not a digest", "malformed")
})

test_that("validator rejects a missing or malformed version", {
  d <- digest_fixture_constant()
  d$version <- NULL
  expect_digest_error(d, "malformed")
  d$version <- "one"
  expect_digest_error(d, "malformed")
})

test_that("validator requires the exact supported schema version", {
  d <- digest_fixture_constant()
  d$version <- 1L
  expect_digest_error(d, "version")

  d <- digest_fixture_constant()
  d$version <- 99L
  expect_digest_error(d, "version")
})

test_that("validator rejects an invalid status", {
  d <- digest_fixture_constant()
  d$status <- "stale"
  expect_digest_error(d, "field")
})

test_that("validator rejects an invalid privacy record", {
  d <- digest_fixture_constant()
  d$privacy$mode <- "secret"
  expect_digest_error(d, "field")
  d <- digest_fixture_constant()
  d$privacy$stable_keys <- NA
  expect_digest_error(d, "field")
})

## Validator: frame registry

test_that("validator requires at least one frame record", {
  d <- digest_fixture_constant()
  d$frames <- list()
  expect_digest_error(d, "malformed")
})

test_that("validator rejects duplicate frame ids", {
  d <- digest_fixture_constant()
  d$frames <- list(digest_frame_120(), digest_frame_120())
  expect_digest_error(d, "duplicate_id")
})

test_that("validator rejects an invalid frame scope", {
  d <- digest_fixture_constant()
  d$frames[[1]]$scope <- "everything"
  expect_digest_error(d, "field")
})

test_that("validator rejects malformed frame roles", {
  d <- digest_fixture_constant()
  d$frames[[1]]$roles <- c("stratum", "cluster")
  expect_digest_error(d, "field")
})

test_that("validator rejects a non-string fingerprint", {
  d <- digest_fixture_constant()
  d$frames[[1]]$fingerprint_exact <- 42
  expect_digest_error(d, "field")
})

## Validator: stage registry

test_that("validator requires at least one stage record", {
  d <- digest_fixture_constant()
  d$stages <- list()
  expect_digest_error(d, "malformed")
})

test_that("validator rejects duplicate stage ids", {
  d <- digest_fixture_units()
  d$stages[[2]]$stage_id <- 1L
  expect_digest_error(d, "duplicate_id")
})

test_that("validator rejects a frame_ref to an unknown frame", {
  d <- digest_fixture_constant()
  d$stages[[1]]$frame_ref <- 5L
  expect_digest_error(d, "broken_link")
})

test_that("validator rejects invalid stage enums", {
  for (field in c("scope", "chance_kind", "order_kind", "storage")) {
    d <- digest_fixture_constant()
    d$stages[[1]][[field]] <- "bogus"
    expect_digest_error(d, "field")
  }
  d <- digest_fixture_constant()
  d$stages[[1]]$unit_level <- ""
  expect_digest_error(d, "field")
})

## Validator: pools

test_that("validator rejects pools with missing required columns", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$N <- NULL
  expect_digest_error(d, "malformed")
})

test_that("validator rejects duplicate pool ids", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$pool_id[2] <- 1L
  expect_digest_error(d, "duplicate_id")
})

test_that("validator rejects a stratum column absent from pools", {
  d <- digest_fixture_constant()
  d$stages[[1]]$strata <- "province"
  expect_digest_error(d, "broken_link")
})

test_that("validator rejects a stratum column with a reserved name", {
  d <- digest_fixture_constant()
  d$stages[[1]]$strata <- "N"
  expect_digest_error(d, "field")
})

test_that("validator rejects invalid pool scope and chance_status", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$scope[1] <- "bogus"
  expect_digest_error(d, "field")
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$chance_status[1] <- "guessed"
  expect_digest_error(d, "field")
})

test_that("validator rejects a negative pool size", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$N[1] <- -5L
  expect_digest_error(d, "field")
})

test_that("validator rejects inclusion probabilities above one", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$chance[1] <- 1.2
  expect_digest_error(d, "chance")
})

test_that("validator rejects negative expected hits", {
  d <- digest_fixture_wr()
  d$stages[[1]]$pools$chance[1] <- -0.2
  expect_digest_error(d, "chance")
})

test_that("pool chance values are only allowed under constant storage", {
  d <- digest_fixture_units()
  d$stages[[1]]$pools$chance <- 0.3
  expect_digest_error(d, "storage")
})

test_that("constant storage requires a pool chance", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$chance <- NULL
  expect_digest_error(d, "storage")
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$chance[1] <- NA_real_
  expect_digest_error(d, "storage")
})

test_that("n_realized cannot exceed N without replacement", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$n_realized[1] <- 31L
  expect_digest_error(d, "allocation")
})

test_that("first-stage pools must not declare a parent", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$parent_unit[1] <- 1L
  expect_digest_error(d, "broken_link")
})

test_that("later-stage pools must declare a valid parent", {
  d <- digest_fixture_units()
  d$stages[[2]]$pools$parent_unit[1] <- NA_integer_
  expect_digest_error(d, "broken_link")
  d <- digest_fixture_units()
  d$stages[[2]]$pools$parent_unit[1] <- 999L
  expect_digest_error(d, "broken_link")
})

## Validator: units table

test_that("units storage requires a units table and vice versa", {
  d <- digest_fixture_units()
  d$stages[[1]]$units <- NULL
  expect_digest_error(d, "storage")
  d <- digest_fixture_constant()
  d$stages[[1]]$units <- digest_fixture_units()$stages[[1]]$units
  expect_digest_error(d, "storage")
})

test_that("validator rejects units with missing required columns", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$is_certainty <- NULL
  expect_digest_error(d, "malformed")
})

test_that("validator rejects duplicate unit ids", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$unit_id[2] <- 1L
  expect_digest_error(d, "duplicate_id")
})

test_that("validator rejects units referencing unknown pools", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$pool_id[1] <- 99L
  expect_digest_error(d, "broken_link")
})

test_that("validator rejects duplicated unit_order within a pool", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$unit_order[2] <- 1L
  expect_digest_error(d, "field")
})

test_that("unit chance may be NA only when the pool chance is unavailable", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$chance[1] <- NA_real_
  expect_digest_error(d, "chance")
})

test_that("validator rejects unit inclusion probabilities above one", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$chance[1] <- 1.5
  expect_digest_error(d, "chance")
})

test_that("validator rejects non-logical is_certainty", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$is_certainty <- "no"
  expect_digest_error(d, "field")
})

test_that("validator rejects misdeclared certainty", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$chance[1] <- 1
  expect_digest_error(d, "certainty")
  d <- digest_fixture_units()
  d$stages[[1]]$units$is_certainty[1] <- TRUE
  expect_digest_error(d, "certainty")
})

test_that("a correctly declared certainty unit passes", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$chance[1] <- 1
  d$stages[[1]]$units$is_certainty[1] <- TRUE
  d$stages[[1]]$pools$n_expected[1] <- 2.5
  expect_no_error(samplyr:::validate_frame_digest(d))
})

test_that("validator rejects negative descendant counts", {
  d <- digest_fixture_units()
  d$stages[[1]]$units$n_descendants[1] <- -1L
  expect_digest_error(d, "field")
})

## Validator: chance distribution

test_that("quantile storage requires a distribution and vice versa", {
  d <- digest_fixture_quantiles()
  d$stages[[1]]$chance_distribution <- NULL
  expect_digest_error(d, "storage")
  d <- digest_fixture_constant()
  d$stages[[1]]$chance_distribution <-
    digest_fixture_quantiles()$stages[[1]]$chance_distribution
  expect_digest_error(d, "storage")
})

test_that("validator rejects quantiles outside [0, 1]", {
  d <- digest_fixture_quantiles()
  d$stages[[1]]$chance_distribution$quantile[1] <- -0.1
  expect_digest_error(d, "field")
})

test_that("validator rejects duplicated quantiles within a pool", {
  d <- digest_fixture_quantiles()
  d$stages[[1]]$chance_distribution$quantile[2] <-
    d$stages[[1]]$chance_distribution$quantile[3]
  expect_digest_error(d, "field")
})

test_that("validator rejects a non-monotone chance quantile function", {
  d <- digest_fixture_quantiles()
  d$stages[[1]]$chance_distribution$chance[5] <- 0.5
  expect_digest_error(d, "chance")
})

test_that("validator rejects distributions referencing unknown pools", {
  d <- digest_fixture_quantiles()
  d$stages[[1]]$chance_distribution$pool_id[1] <- 99L
  expect_digest_error(d, "broken_link")
})

## Validator: n_expected consistency

test_that("n_expected must match the constant representation", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$n_expected[1] <- 12
  expect_digest_error(d, "allocation")
})

test_that("n_expected must match the sum of unit chances", {
  d <- digest_fixture_units()
  d$stages[[1]]$pools$n_expected[1] <- 3
  expect_digest_error(d, "allocation")
})

test_that("n_expected must match the quantile representation loosely", {
  d <- digest_fixture_quantiles()
  d$stages[[1]]$pools$n_expected <- 15
  expect_digest_error(d, "allocation")
})

## Validator: selected trace

test_that("pools with selections require a trace", {
  d <- digest_fixture_constant()
  d$stages[[1]]$selected <- NULL
  expect_digest_error(d, "trace")
})

test_that("a stage with no selections may omit the trace", {
  d <- digest_fixture_constant()
  d$stages[[1]]$selected <- NULL
  d$stages[[1]]$pools$n_realized <- 0L
  d$stages[[1]]$pools$n_expected <- 10
  expect_no_error(samplyr:::validate_frame_digest(d))
})

test_that("validator rejects a trace referencing unknown pools", {
  d <- digest_fixture_constant()
  d$stages[[1]]$selected$pool_id[1] <- 99L
  expect_digest_error(d, "broken_link")
})

test_that("validator rejects a trace referencing unknown units", {
  d <- digest_fixture_units()
  d$stages[[1]]$selected$unit_id[1] <- 999L
  expect_digest_error(d, "broken_link")
})

test_that("validator rejects a trace that misplaces a unit's pool", {
  d <- digest_fixture_units()
  d$stages[[1]]$selected$pool_id[1] <- 2L
  expect_digest_error(d, "broken_link")
})

test_that("validator rejects non-positive occurrences", {
  d <- digest_fixture_constant()
  d$stages[[1]]$selected$occurrence[1] <- 0L
  expect_digest_error(d, "trace")
})

test_that("without-replacement stages cannot record repeat hits", {
  d <- digest_fixture_constant()
  d$stages[[1]]$selected$occurrence[1] <- 2L
  expect_digest_error(d, "trace")
})

test_that("validator rejects duplicated selected occurrences", {
  d <- digest_fixture_wr()
  d$stages[[1]]$selected <- rbind(
    d$stages[[1]]$selected,
    d$stages[[1]]$selected[6, ]
  )
  expect_digest_error(d, "duplicate_id")
})

test_that("n_realized must match the trace for a single replicate", {
  d <- digest_fixture_constant()
  d$stages[[1]]$pools$n_realized[1] <- 9L
  expect_digest_error(d, "allocation")
})

test_that("validator rejects non-positive sample_row locators", {
  d <- digest_fixture_constant()
  d$stages[[1]]$selected$sample_row[1] <- 0L
  expect_digest_error(d, "trace")
})

test_that("validator rejects non-positive replicate numbers", {
  d <- digest_fixture_replicated()
  d$stages[[1]]$selected$replicate[1] <- 0L
  expect_digest_error(d, "trace")
})

test_that("replicated traces skip the per-pool n_realized check", {
  # Two replicates: per-pool counts are replicate-specific, so an NA
  # n_realized passes and no count comparison is attempted.
  d <- digest_fixture_replicated()
  expect_no_error(samplyr:::validate_frame_digest(d))
})

## Accessors

test_that("get_frame_digest returns NULL without a digest", {
  nodigest <- sampling_design() |>
    draw(n = 5) |>
    execute(test_frame, seed = 1, frame_digest = "none")
  expect_null(samplyr:::get_frame_digest(nodigest))
  expect_null(samplyr:::get_frame_digest(data.frame(x = 1)))
})

test_that("set_frame_digest attaches a validated digest", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_constant())
  d <- samplyr:::get_frame_digest(s)
  expect_identical(d$version, 2L)
  expect_identical(d$status, "complete")

  bad <- digest_fixture_constant()
  bad$status <- "bogus"
  expect_error(
    samplyr:::set_frame_digest(fix_srs, bad),
    class = "samplyr_error_digest_invalid"
  )
})

test_that("get_frame_digest requires the exact supported schema version", {
  d <- digest_fixture_constant()
  d$version <- 1L
  s <- samplyr:::set_frame_digest(fix_srs, d, validate = FALSE)
  expect_error(
    samplyr:::get_frame_digest(s),
    class = "samplyr_error_digest_version"
  )

  d <- digest_fixture_constant()
  d$version <- 99L
  s <- samplyr:::set_frame_digest(fix_srs, d, validate = FALSE)
  expect_error(
    samplyr:::get_frame_digest(s),
    class = "samplyr_error_digest_version"
  )
})

test_that("a modified sample reports an invalidated digest lazily", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_constant())
  s$.weight <- s$.weight * 2
  expect_identical(samplyr:::get_frame_digest(s)$status, "invalidated")
  expect_error(
    frame_summary(s),
    class = "samplyr_error_digest_invalidated"
  )
})

test_that("value-identical overwrites do not invalidate the digest", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_constant())
  s$.weight <- s$.weight
  expect_identical(samplyr:::get_frame_digest(s)$status, "complete")
})

test_that("adding analysis columns does not invalidate the digest", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_constant())
  s$analysis_var <- seq_len(nrow(s))
  expect_identical(samplyr:::get_frame_digest(s)$status, "complete")
  expect_no_error(frame_summary(s))
})

## frame_summary: errors

test_that("frame_summary requires a tbl_sample with a digest", {
  expect_error(frame_summary(data.frame(x = 1)), class = "samplyr_error")
  nodigest <- sampling_design() |>
    draw(n = 5) |>
    execute(test_frame, seed = 1, frame_digest = "none")
  expect_error(frame_summary(nodigest), class = "samplyr_error_no_digest")
})

test_that("frame_summary validates the stage argument", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_constant())
  expect_error(frame_summary(s, stage = "one"), class = "samplyr_error")
  expect_error(frame_summary(s, stage = 3), class = "samplyr_error")
  expect_error(frame_summary(s, stage = Inf), class = "samplyr_error")
  expect_error(frame_summary(s, stage = NaN), class = "samplyr_error")
  expect_error(frame_summary(s, stage = NA_real_), class = "samplyr_error")
})

## frame_summary: stage detail

test_that("stage detail reports the selection chain", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_constant())
  fs <- frame_summary(s)
  expect_s3_class(fs, "tbl_df")
  expect_named(fs, c(
    "stage", "unit_level", "scope", "chance_kind", "probabilities",
    "storage", "n_pools", "N", "n_target", "n_expected", "n_realized",
    "take_rate"
  ))
  expect_identical(nrow(fs), 1L)
  expect_identical(fs$n_pools, 4L)
  expect_equal(fs$N, 120)
  expect_equal(fs$n_realized, 40)
  expect_equal(fs$take_rate, 1 / 3)
})

test_that("stage detail covers every stage of a multistage digest", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_units())
  fs <- frame_summary(s)
  expect_identical(nrow(fs), 2L)
  expect_identical(fs$storage, c("units", "constant"))
  expect_equal(fs$N, c(24, 40))
  expect_equal(fs$n_realized, c(8, 24))
  expect_equal(fs$take_rate, c(8 / 24, 24 / 40))
})

test_that("universe scope suppresses eligible-only denominators", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_units())
  fs <- frame_summary(s, scope = "universe")
  # Stage 1 covered the full universe; stage 2 only eligible units in
  # selected clusters, so its universe denominator is unknowable.
  expect_equal(fs$N, c(24, NA))
  expect_equal(fs$take_rate, c(8 / 24, NA))
  # Realized counts are facts and stay reported.
  expect_equal(fs$n_realized, c(8, 24))
})

test_that("conditional scope suppresses denominators on both bases", {
  s <- samplyr:::set_frame_digest(
    fix_srs, digest_fixture_units(stage2_scope = "conditional")
  )
  eligible <- frame_summary(s, scope = "eligible")
  universe <- frame_summary(s, scope = "universe")
  expect_true(is.na(eligible$N[2]))
  expect_true(is.na(eligible$take_rate[2]))
  expect_true(is.na(universe$N[2]))
  expect_identical(eligible$scope[2], "conditional")
})

test_that("stage detail keeps expected and realized sizes distinct", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_quantiles())
  fs <- frame_summary(s)
  expect_identical(fs$storage, "quantiles")
  expect_equal(fs$n_expected, 0.085 * 120)
  expect_equal(fs$n_realized, 10)
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_wr())
  fs <- frame_summary(s)
  expect_identical(fs$chance_kind, "expected_hits")
  expect_equal(fs$n_realized, 10)
})

test_that("replicate-varying realized allocation reports NA, not a guess", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_replicated())
  fs <- frame_summary(s)
  expect_true(is.na(fs$n_realized))
  expect_true(is.na(fs$take_rate))
})

test_that("the stage argument filters the report", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_units())
  fs <- frame_summary(s, stage = 2)
  expect_identical(nrow(fs), 1L)
  expect_identical(fs$stage, 2L)
})

## frame_summary: pool detail

test_that("pool detail reports every pool with its strata", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_units())
  fs <- frame_summary(s, detail = "pool")
  expect_identical(nrow(fs), 12L)
  expect_true("stratum" %in% names(fs))
  expect_identical(
    as.character(fs$stratum[fs$stage == 1]),
    c("A", "B", "C", "D")
  )
  # Stage 2 is unstratified: its stratum labels are NA, its parents
  # are the selected stage-1 units.
  expect_true(all(is.na(fs$stratum[fs$stage == 2])))
  expect_identical(
    fs$parent_unit[fs$stage == 2],
    c(1L, 2L, 7L, 8L, 13L, 14L, 19L, 20L)
  )
  expect_equal(fs$take_rate[fs$stage == 2], rep(3 / 5, 8))
})

test_that("pool detail reports the constant chance where one applies", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_constant())
  fs <- frame_summary(s, detail = "pool")
  expect_equal(fs$chance, rep(1 / 3, 4))
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_quantiles())
  fs <- frame_summary(s, detail = "pool")
  expect_true(is.na(fs$chance))
  expect_identical(fs$chance_status, "summarized")
})

test_that("pool-level take rates honor pool scope", {
  s <- samplyr:::set_frame_digest(
    fix_srs, digest_fixture_units(stage2_scope = "conditional")
  )
  fs <- frame_summary(s, detail = "pool")
  expect_true(all(is.na(fs$take_rate[fs$stage == 2])))
  expect_equal(fs$take_rate[fs$stage == 1], rep(2 / 6, 4))
})

## frame_summary: unit detail

test_that("unit detail reports the anonymous unit registry", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_units())
  fs <- frame_summary(s, detail = "unit")
  expect_named(fs, c(
    "stage", "pool_id", "unit_id", "unit_order", "chance",
    "is_certainty", "n_descendants", "is_selected", "n_hits"
  ))
  # Only stage 1 retained units; stage 2 is constant storage.
  expect_identical(unique(fs$stage), 1L)
  expect_identical(nrow(fs), 24L)
  expect_identical(sum(fs$is_selected), 8L)
  expect_identical(
    fs$unit_id[fs$is_selected],
    c(1L, 2L, 7L, 8L, 13L, 14L, 19L, 20L)
  )
  expect_identical(fs$n_hits[fs$is_selected], rep(1L, 8))
  expect_identical(fs$n_descendants, rep(5L, 24))
})

test_that("unit detail errors for a stage without unit storage", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_units())
  expect_error(
    frame_summary(s, stage = 2, detail = "unit"),
    class = "samplyr_error_digest_no_units"
  )
})

test_that("unit detail is empty when no stage retained units", {
  s <- samplyr:::set_frame_digest(fix_srs, digest_fixture_quantiles())
  fs <- frame_summary(s, detail = "unit")
  expect_identical(nrow(fs), 0L)
  expect_named(fs, c(
    "stage", "pool_id", "unit_id", "unit_order", "chance",
    "is_certainty", "n_descendants", "is_selected", "n_hits"
  ))
})
