# Tests for the tbl_sample row/column mutation contract (review issue 2)
#
# Removing, adding, or duplicating rows -- or overwriting internal
# design columns -- leaves a sample whose stored design no longer
# describes it. Such samples are marked as modified and rejected by
# design-based computations, with one exemption: a verified complete
# single replicate.

test_that("filter() marks the sample and as_svydesign() rejects it", {
  skip_if_not_installed("survey")

  filtered <- dplyr::filter(fix_srs, y > 0)
  expect_true(is_tbl_sample(filtered))
  expect_lt(nrow(filtered), nrow(fix_srs))
  expect_identical(sample_modifications(filtered), "rows")

  expect_error(
    as_svydesign(filtered),
    class = "samplyr_error_modified_sample"
  )
})

test_that("a filter that removes no rows does not invalidate", {
  skip_if_not_installed("survey")

  kept <- dplyr::filter(fix_srs, is.finite(y))
  expect_identical(nrow(kept), nrow(fix_srs))
  expect_length(sample_modifications(kept), 0)
  expect_s3_class(as_svydesign(kept), "survey.design")
})

test_that("[ row subsetting is consistent with filter()", {
  skip_if_not_installed("survey")

  subset_rows <- fix_srs[fix_srs$y > 0, ]
  expect_true(is_tbl_sample(subset_rows))
  expect_identical(sample_modifications(subset_rows), "rows")
  expect_error(
    as_svydesign(subset_rows),
    class = "samplyr_error_modified_sample"
  )

  # Column subsetting that keeps all internal columns does not
  # invalidate; dropping any of them does (see the dedicated tests).
  internal <- grep("^\\.", names(fix_srs), value = TRUE)
  subset_cols <- fix_srs[, c("id", "y", internal)]
  expect_length(sample_modifications(subset_cols), 0)
})

test_that("dropping internal columns via [ or select() invalidates", {
  skip_if_not_installed("survey")

  # Keeping only .weight silently changed the exported design before:
  # without .fpc_1 the FPC falls back to no correction and the SE
  # inflates. Now the drop is marked and the export refuses.
  slim <- fix_srs[, c("id", "y", ".weight")]
  expect_identical(sample_modifications(slim), "columns")
  expect_error(
    as_svydesign(slim),
    class = "samplyr_error_modified_sample"
  )

  slim2 <- dplyr::select(fix_srs, id, y, .weight)
  expect_identical(sample_modifications(slim2), "columns")

  # Keeping every internal column stays clean, and exports identically
  kept <- dplyr::select(fix_srs, id, y, dplyr::starts_with("."))
  expect_length(sample_modifications(kept), 0)
  se_full <- survey::SE(survey::svytotal(~y, as_svydesign(fix_srs)))
  se_kept <- survey::SE(survey::svytotal(~y, as_svydesign(kept)))
  expect_equal(as.numeric(se_kept), as.numeric(se_full))

  # Column reordering alone stays clean
  reordered <- dplyr::select(fix_srs, y, dplyr::everything())
  expect_length(sample_modifications(reordered), 0)
})

test_that("renaming internal columns invalidates", {
  renamed <- dplyr::rename(fix_srs, w1 = .weight_1)
  expect_identical(sample_modifications(renamed), "columns")
  expect_error(
    as_svydesign(renamed),
    class = "samplyr_error_modified_sample"
  )

  # Renaming ordinary data columns stays clean
  benign <- dplyr::rename(fix_srs, outcome = y)
  expect_length(sample_modifications(benign), 0)
})

test_that("two-phase export warns when phase 1 was modified", {
  skip_if_not_installed("survey")

  pop <- data.frame(
    id = seq_len(400),
    elig = rep(c(TRUE, FALSE), 200),
    z = seq_len(400)
  )
  p1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 100) |>
    execute(pop, seed = 2)
  p1_screened <- dplyr::filter(p1, elig)
  p2 <- suppressWarnings(
    sampling_design() |>
      cluster_by(id) |>
      draw(n = 20) |>
      execute(p1_screened, seed = 3)
  )

  expect_warning(
    as_svydesign(p2),
    "phase-1 sample was modified"
  )

  # A clean phase-1 sample exports without the warning
  p2_clean <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 20) |>
    execute(p1, seed = 3)
  expect_no_warning(as_svydesign(p2_clean))
})

test_that("reordering rows does not invalidate and preserves estimates", {
  skip_if_not_installed("survey")

  reordered <- dplyr::arrange(fix_strat_prop, dplyr::desc(y))
  expect_length(sample_modifications(reordered), 0)

  est_orig <- survey::svytotal(~y, as_svydesign(fix_strat_prop))
  est_reord <- survey::svytotal(~y, as_svydesign(reordered))
  expect_equal(as.numeric(coef(est_reord)), as.numeric(coef(est_orig)))
  expect_equal(as.numeric(survey::SE(est_reord)), as.numeric(survey::SE(est_orig)))
})

test_that("one-to-one joins remain supported", {
  skip_if_not_installed("survey")

  lookup <- data.frame(id = test_frame$id, extra = seq_len(nrow(test_frame)))
  joined <- dplyr::left_join(fix_srs, lookup, by = "id")
  expect_identical(nrow(joined), nrow(fix_srs))
  expect_length(sample_modifications(joined), 0)
  expect_s3_class(as_svydesign(joined), "survey.design")
})

test_that("row-duplicating joins invalidate the sample", {
  lookup <- data.frame(
    id = c(test_frame$id, test_frame$id[1:10]),
    extra = 1L
  )
  joined <- dplyr::left_join(fix_srs, lookup, by = "id",
                             relationship = "many-to-many")
  expect_identical(sample_modifications(joined), "rows")
})

test_that("same-length duplication via slice() is caught", {
  n <- nrow(fix_srs)
  sliced <- dplyr::slice(fix_srs, c(1L, 1L, 3:n))
  expect_identical(nrow(sliced), n)
  expect_identical(sample_modifications(sliced), "rows")
})

test_that("overwriting internal design columns invalidates", {
  skip_if_not_installed("survey")

  reweighted <- dplyr::mutate(fix_srs, .weight = .weight * 2)
  expect_identical(sample_modifications(reweighted), "columns")
  expect_error(
    as_svydesign(reweighted),
    class = "samplyr_error_modified_sample"
  )

  no_fpc <- dplyr::mutate(fix_srs, .fpc_1 = NULL)
  expect_identical(sample_modifications(no_fpc), "columns")

  # Ordinary data columns stay free
  extended <- dplyr::mutate(fix_srs, y2 = y * 2)
  expect_length(sample_modifications(extended), 0)
  expect_s3_class(as_svydesign(extended), "survey.design")
})

test_that("modification marks accumulate and survive round-trips", {
  both <- fix_srs |>
    dplyr::filter(y > 0) |>
    dplyr::mutate(.weight = 1)
  expect_setequal(sample_modifications(both), c("rows", "columns"))

  # as_tibble() + as_tbl_sample() keeps the marks
  restored <- as_tbl_sample(tibble::as_tibble(both))
  expect_setequal(sample_modifications(restored), c("rows", "columns"))
})

test_that("joint_expectation, design_effect, effective_n are guarded", {
  filtered <- dplyr::filter(fix_multistage, y > 0)
  expect_error(
    joint_expectation(filtered, test_frame),
    class = "samplyr_error_modified_sample"
  )
  expect_error(
    design_effect(filtered),
    class = "samplyr_error_modified_sample"
  )
  expect_error(
    effective_n(filtered),
    class = "samplyr_error_modified_sample"
  )
})

test_that("extracting one complete replicate is exempt", {
  skip_if_not_installed("survey")

  reps <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 7, reps = 4)
  expect_named(attr(reps, "metadata")$replicate_rows)

  r2 <- dplyr::filter(reps, .replicate == 2)
  expect_identical(sample_modifications(r2), "rows")
  expect_s3_class(as_svydesign(r2), "survey.design")

  # [ extraction works too
  r3 <- reps[reps$.replicate == 3, ]
  expect_s3_class(as_svydesign(r3), "survey.design")
})

test_that("a partial or forged replicate is rejected", {
  reps <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 7, reps = 4)

  # Complete replicate, then domain filter: no longer exempt
  partial <- reps |>
    dplyr::filter(.replicate == 1) |>
    dplyr::filter(y > 0)
  expect_error(
    as_svydesign(partial),
    class = "samplyr_error_modified_sample"
  )

  # Rewriting .replicate marks "columns", which the exemption refuses
  forged <- reps |>
    dplyr::filter(.replicate %in% c(1, 2)) |>
    dplyr::mutate(.replicate = 1)
  expect_error(
    as_svydesign(forged),
    class = "samplyr_error_modified_sample"
  )

  # Dropping .sample_id defeats verification
  no_id <- reps |>
    dplyr::filter(.replicate == 1) |>
    dplyr::mutate(.sample_id = NULL)
  expect_error(
    as_svydesign(no_id),
    class = "samplyr_error_modified_sample"
  )
})

test_that("continuation-produced replicated samples record replicate_rows", {
  skip_if_not_installed("survey")

  two_stage <- sampling_design() |>
    add_stage("Clusters") |>
    cluster_by(cluster) |>
    draw(n = 4) |>
    add_stage("Units") |>
    draw(n = 2)

  stage1 <- execute(two_stage, test_frame, stages = 1, seed = 11, reps = 3)
  full <- execute(stage1, test_frame, seed = 12)
  expect_named(attr(full, "metadata")$replicate_rows)

  r1 <- dplyr::filter(full, .replicate == 1)
  expect_s3_class(as_svydesign(r1), "survey.design")
})

test_that("execute() warns when given a modified sample", {
  filtered <- dplyr::filter(fix_srs, y > 0)
  second_phase <- sampling_design() |> draw(n = 5)
  expect_warning(
    execute(second_phase, filtered, seed = 3),
    "modified after execution"
  )
})

test_that("print header shows the modification marker", {
  filtered <- dplyr::filter(fix_srs, y > 0)
  header <- tbl_sum(filtered)
  expect_true("Modified" %in% names(header))
  expect_match(header[["Modified"]], "rows")

  clean_header <- tbl_sum(fix_srs)
  expect_false("Modified" %in% names(clean_header))
})

test_that("domain analysis via survey::subset matches, filtered export blocked", {
  skip_if_not_installed("survey")

  population <- data.frame(
    id = 1:1000,
    domain = rep(c(TRUE, FALSE), each = 500),
    y = seq_len(1000)
  )
  sample <- sampling_design() |>
    draw(n = 200) |>
    execute(population, seed = 9)

  full_design <- as_svydesign(sample)
  correct <- survey::svytotal(~y, subset(full_design, domain))
  expect_gt(as.numeric(survey::SE(correct)), 0)

  # The review's footgun now errors instead of understating the SE
  expect_error(
    as_svydesign(dplyr::filter(sample, domain)),
    class = "samplyr_error_modified_sample"
  )
})

# Integrity backstop and table-operation matrix (robustness review) ---------

test_that("integrity verification catches untracked modification routes", {
  skip_if_not_installed("survey")

  s <- fix_srs

  # vctrs and base row-binding produce doubled realizations
  doubled_v <- vctrs::vec_rbind(s, s)
  expect_error(as_svydesign(doubled_v), class = "samplyr_error_modified_sample")
  doubled_b <- rbind(s, s)
  expect_error(as_svydesign(doubled_b), class = "samplyr_error_modified_sample")

  # base assignment to a protected column
  tampered <- s
  tampered$.weight <- tampered$.weight * 2
  expect_length(sample_modifications(tampered), 0)
  expect_error(as_svydesign(tampered), class = "samplyr_error_modified_sample")

  # design-referenced user columns are protected by the record
  restratified <- dplyr::mutate(fix_strat_prop, stratum = factor("A"))
  expect_error(
    as_svydesign(restratified),
    class = "samplyr_error_modified_sample"
  )

  # stripping and restoring cannot launder modifications
  plain <- as.data.frame(s)
  plain$.fpc_1 <- NULL
  relaundered <- as_tbl_sample(plain)
  expect_identical(sample_modifications(relaundered), "columns")
  expect_error(as_svydesign(relaundered), class = "samplyr_error_modified_sample")
})

test_that("integrity is authoritative: value-identical overwrites pass", {
  skip_if_not_installed("survey")

  noop <- dplyr::mutate(fix_srs, .weight = .weight * 1)
  expect_identical(sample_modifications(noop), "columns")
  se_noop <- survey::SE(survey::svytotal(~y, as_svydesign(noop)))
  se_full <- survey::SE(survey::svytotal(~y, as_svydesign(fix_srs)))
  expect_equal(as.numeric(se_noop), as.numeric(se_full))
})

test_that("same-length base [ duplication is detected", {
  n <- nrow(fix_srs)
  swapped <- fix_srs[c(1L, 1L, 3:n), ]
  expect_identical(nrow(swapped), n)
  expect_identical(sample_modifications(swapped), "rows")
})

test_that("grouping preserves provenance and marks flow through", {
  skip_if_not_installed("survey")

  g <- dplyr::group_by(fix_strat_prop, stratum)
  expect_true(is_tbl_sample(g))
  expect_s3_class(g, "grouped_df")

  gm <- dplyr::mutate(g, dev = y - mean(y))
  expect_true(is_tbl_sample(gm))
  expect_identical(dplyr::group_vars(gm), "stratum")
  expect_length(sample_modifications(gm), 0)

  # grouped filter marks rows; export refuses after ungrouping too
  gf <- dplyr::filter(g, y > min(y))
  expect_true(is_tbl_sample(gf))
  expect_identical(sample_modifications(gf), "rows")
  expect_error(
    as_svydesign(dplyr::ungroup(gf)),
    class = "samplyr_error_modified_sample"
  )

  # grouped summarise leaves the sample world
  gs <- dplyr::summarise(g, mn = mean(y))
  expect_false(is_tbl_sample(gs))

  # roundtrip exports identically
  piped <- fix_strat_prop |>
    dplyr::group_by(stratum) |>
    dplyr::mutate(dev = y - mean(y)) |>
    dplyr::ungroup()
  expect_true(is_tbl_sample(piped))
  expect_length(sample_modifications(piped), 0)
  se_piped <- survey::SE(survey::svytotal(~y, as_svydesign(piped)))
  se_full <- survey::SE(survey::svytotal(~y, as_svydesign(fix_strat_prop)))
  expect_equal(as.numeric(se_piped), as.numeric(se_full))
})

test_that("a tampered extracted replicate is rejected", {
  reps <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 7, reps = 4)
  r1 <- dplyr::filter(reps, .replicate == 1)

  forged <- r1
  forged$.weight[1] <- 999
  expect_error(
    as_svydesign(forged),
    class = "samplyr_error_modified_sample"
  )
})

test_that("reordering and one-to-one joins verify against the integrity record", {
  skip_if_not_installed("survey")

  lookup <- data.frame(id = test_frame$id, extra = seq_len(nrow(test_frame)))
  reshaped <- fix_srs |>
    dplyr::arrange(dplyr::desc(y)) |>
    dplyr::left_join(lookup, by = "id")
  expect_length(sample_modifications(reshaped), 0)
  expect_s3_class(as_svydesign(reshaped), "survey.design")
})
