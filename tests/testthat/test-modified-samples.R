# Changing rows or internal design columns leaves a sample its stored design
# does not describe. Such samples are marked as modified and rejected by
# design-based computations, except a verified complete single replicate.

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

  # Column subsetting that keeps all internal columns does not invalidate.
  internal <- grep("^\\.", names(fix_srs), value = TRUE)
  subset_cols <- fix_srs[, c("id", "y", internal)]
  expect_length(sample_modifications(subset_cols), 0)
})

test_that("dropping internal columns via [ or select() invalidates", {
  skip_if_not_installed("survey")

  # Without .fpc_1 the export would drop the FPC and inflate the SE.
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
  skip_if_not_installed("survey")
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

test_that("two-phase export refuses a modified phase 1", {
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

  expect_error(as_svydesign(p2), class = "samplyr_error_modified_sample")
  expect_error(as_svrepdesign(p2, type = "JK1"),
               class = "samplyr_error_modified_sample")
  # The preflight refuses it the same way, before and after drawing.
  for (v in list(variance_estimators(p2),
                 variance_estimators(get_design(p2), p1_screened))) {
    expect_identical(unique(v$class[v$estimator != "Random groups"]),
                     "samplyr_error_modified_sample")
  }
  expect_warning(validate_frame(get_design(p2), p1_screened),
                 class = "samplyr_warning_modified_sample")

  # A clean phase-1 sample exports without the warning
  p2_clean <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 20) |>
    execute(p1, seed = 3)
  expect_no_warning(as_svydesign(p2_clean))
})

test_that("a screened phase 1 has no export, its stratified phase 2 has", {
  skip_if_not_installed("survey")
  # Twenty of forty units, half of them eligible. Phase 2 drawn from the
  # screened rows composed into one phase and gave SE 0 for a random
  # eligible count, and through survey::twophase() read the screened rows
  # as a sample of the whole frame, doubling the total.
  frame <- data.frame(
    id = 1:40, eligible = rep(c(TRUE, FALSE), each = 20),
    x = (1:40 %% 7) + 1
  )
  frame$y <- ifelse(frame$eligible, 100 + frame$x, NA)
  phase1 <- execute(sampling_design() |> draw(n = 20), frame, seed = 1)
  srs <- sampling_design() |> draw(n = 6)
  pps <- sampling_design() |> draw(n = 6, method = "pps_brewer", mos = x)
  edits <- list(
    filter = dplyr::filter(phase1, eligible),
    subset = phase1[phase1$eligible, ],
    weight = dplyr::mutate(phase1, .weight = .weight * 2)
  )
  for (edit in names(edits)) {
    for (d2 in list(srs, pps)) {
      info <- paste(edit, d2$stages[[1]]$draw_spec$method)
      phase2 <- suppressWarnings(execute(d2, edits[[edit]], seed = 2))
      expect_error(as_svydesign(phase2),
                   class = "samplyr_error_modified_sample", info = info)
      expect_error(as_svrepdesign(phase2, type = "JKn"),
                   class = "samplyr_error_modified_sample", info = info)
      for (v in list(variance_estimators(phase2),
                     variance_estimators(d2, edits[[edit]]))) {
        expect_identical(v$status[v$estimator != "Random groups"],
                         rep("refused", 10L), info = info)
        expect_identical(unique(v$class[v$estimator != "Random groups"]),
                         "samplyr_error_modified_sample", info = info)
      }
    }
  }

  # Edits that leave the realization intact keep the export.
  direct <- as_svydesign(execute(srs, phase1, seed = 2))
  kept <- list(
    column = dplyr::mutate(phase1, z = 1),
    value = dplyr::mutate(phase1, x = x + 1),
    order = dplyr::arrange(phase1, dplyr::desc(id)),
    dropped = dplyr::select(phase1, -x)
  )
  for (edit in names(kept)) {
    phase2 <- execute(srs, kept[[edit]], seed = 2)
    expect_no_error(svy <- as_svydesign(phase2))
    if (edit != "order") {
      expect_equal(survey::SE(survey::svytotal(~y, svy, na.rm = TRUE)),
                   survey::SE(survey::svytotal(~y, direct, na.rm = TRUE)),
                   info = edit)
    }
  }

  # Screening as a phase-2 stratum is an ordinary two-phase design: every
  # ineligible unit is kept, half of the eligible ones drawn.
  rates <- data.frame(eligible = c(TRUE, FALSE), frac = c(0.5, 1))
  screen <- sampling_design() |> stratify_by(eligible) |> draw(frac = rates)
  phase2 <- execute(screen, phase1, seed = 2)
  v <- variance_estimators(phase2)
  expect_identical(v$status[v$estimator == "Taylor linearization"],
                   "supported")
  est <- survey::svytotal(~y, subset(as_svydesign(phase2), eligible),
                          na.rm = TRUE)
  expect_equal(unname(coef(est)),
               sum((phase2$y * phase2$.weight)[phase2$eligible]))
  expect_gt(as.vector(survey::SE(est)), 0)

  # Random groups read each replicate's total alone, which a screening
  # applied in every replicate leaves estimable.
  reps <- execute(sampling_design() |> draw(n = 20), frame, seed = 1,
                  reps = 4)
  phase2 <- suppressWarnings(execute(srs, dplyr::filter(reps, eligible),
                                     seed = 2))
  v <- variance_estimators(phase2)
  expect_identical(v$status[v$estimator == "Random groups"], "supported")
  rg <- survey::svytotal(~y, as_svrepdesign(phase2, type = "random_groups"))
  totals <- tapply(phase2$y * phase2$.weight, phase2$.replicate, sum)
  expect_equal(unname(coef(rg)), mean(totals))
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
  skip_if_not_installed("survey")
  reps <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 7, reps = 4)

  # Complete replicate, then domain filter: not exempt
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

  # Filtering before export would understate the SE, so it is refused.
  expect_error(
    as_svydesign(dplyr::filter(sample, domain)),
    class = "samplyr_error_modified_sample"
  )
})

## Integrity backstop and table-operation matrix

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
  expect_identical(sample_modifications(tampered), "values")
  expect_identical(tbl_sum(tampered)[["Modified"]], "values")
  expect_error(as_svydesign(tampered), class = "samplyr_error_modified_sample")

  # design-referenced user columns are protected by the record
  restratified <- dplyr::mutate(fix_strat_prop, stratum = factor("A"))
  expect_identical(sample_modifications(restratified), "values")
  expect_identical(tbl_sum(restratified)[["Modified"]], "values")
  expect_error(
    as_svydesign(restratified),
    class = "samplyr_error_modified_sample"
  )

  # NA row index invalidates rows and reports in display
  na_rows <- s[c(NA_integer_, 2:nrow(s)), ]
  expect_identical(sample_modifications(na_rows), "rows")
  expect_identical(tbl_sum(na_rows)[["Modified"]], "rows")
  expect_error(as_svydesign(na_rows), class = "samplyr_error_modified_sample")

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

  # grouped filter marks rows, and export refuses after ungrouping too
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
  skip_if_not_installed("survey")
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

## What the integrity refusal names

test_that("a factor turned into its labels is the same realization", {
  # Levels out of label order, so ordering by codes and by labels differ.
  frame <- bfa_eas
  frame$region <- factor(frame$region, levels = rev(levels(frame$region)))
  s <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 2) |>
    execute(frame, seed = 1)
  expect_true(is.factor(s$region))
  as_text <- dplyr::mutate(s, region = as.character(region))
  expect_no_error(check_sample_unmodified(as_text, "as_svydesign"))

  relabeled <- dplyr::mutate(
    s, region = factor(region, labels = paste0("r", seq_along(levels(region))))
  )
  err <- tryCatch(
    check_sample_unmodified(relabeled, "as_svydesign"),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_modified_sample")
  expect_match(
    cli::ansi_strip(conditionMessage(err)),
    "The values of region no longer match", fixed = TRUE
  )
})

test_that("the integrity refusal names the column and not a route", {
  s <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = 2) |>
    execute(bfa_eas, seed = 1)
  message_of <- function(x) {
    err <- tryCatch(check_sample_unmodified(x, "as_svydesign"),
                    error = identity)
    expect_s3_class(err, "samplyr_error_modified_sample")
    cli::ansi_strip(conditionMessage(err))
  }
  dropped <- message_of(dplyr::select(s, -region))
  expect_match(dropped, "region was dropped or renamed", fixed = TRUE)
  expect_false(grepl("base assignment", dropped, fixed = TRUE))

  moved <- s
  moved$ea_id[1] <- moved$ea_id[2]
  expect_match(
    message_of(moved), "The values of ea_id no longer match", fixed = TRUE
  )
})

test_that("an edited weight is pointed to the exported design", {
  s <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = 2) |>
    execute(bfa_eas, seed = 1)
  adjusted <- dplyr::mutate(s, .weight = .weight * 1.1)
  for (fn in c("as_svydesign", "as_svrepdesign")) {
    err <- tryCatch(check_sample_unmodified(adjusted, fn), error = identity)
    expect_s3_class(err, "samplyr_error_modified_sample")
    text <- cli::ansi_strip(conditionMessage(err))
    expect_match(text, "The values of .weight no longer match", fixed = TRUE)
    expect_match(text, "survey::calibrate()", fixed = TRUE)
    expect_false(grepl("dropped or renamed", text, fixed = TRUE))
    expect_false(grepl("subpopulation", text, fixed = TRUE))
  }
  # Other verbs, and edits beyond the weight, keep the general advice.
  err <- tryCatch(check_sample_unmodified(adjusted, "joint_expectation"),
                  error = identity)
  expect_false(grepl("survey::calibrate", conditionMessage(err), fixed = TRUE))
  both <- dplyr::mutate(adjusted, ea_id = rev(ea_id))
  err <- tryCatch(check_sample_unmodified(both, "as_svydesign"),
                  error = identity)
  expect_false(grepl("survey::calibrate", conditionMessage(err), fixed = TRUE))
  expect_match(cli::ansi_strip(conditionMessage(err)), "subpopulation",
               fixed = TRUE)
})

test_that("a continuation of a modified sample stays modified", {
  skip_if_not_installed("survey")
  design <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(ea_id) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |> draw(n = 3) |>
    add_stage() |> draw(n = 2)
  selected <- execute(design, bfa_eas, stages = 1, seed = 1)
  listing <- function(eas) {
    eas <- as.data.frame(eas)[c("ea_id", "region")]
    out <- eas[rep(seq_len(nrow(eas)), each = 12), ]
    out$hh <- paste(out$ea_id, rep(1:4, each = 3))
    out$person <- seq_len(nrow(out))
    out
  }
  dropped <- selected[-1, ]
  expect_warning(
    households <- execute(dropped, listing(dropped), stages = 2, seed = 2),
    class = "samplyr_warning_modified_sample"
  )
  expect_identical(get_stages_executed(households), 1:2)
  err <- tryCatch(as_svydesign(households), error = identity)
  expect_s3_class(err, "samplyr_error_modified_sample")
  expect_match(cli::ansi_strip(conditionMessage(err)),
               "It continues a sample whose rows changed", fixed = TRUE)
  expect_error(as_svrepdesign(households),
               class = "samplyr_error_modified_sample")
  # The status survives a second continuation.
  expect_warning(
    persons <- execute(households, listing(dropped), seed = 3),
    class = "samplyr_warning_modified_sample"
  )
  expect_error(as_svydesign(persons), class = "samplyr_error_modified_sample")

  intact <- execute(selected, listing(selected), stages = 2, seed = 2) |>
    execute(listing(selected), seed = 3)
  expect_no_error(as_svydesign(intact))
})

test_that("a subround filtered out of a master cannot be continued into an export", {
  skip_if_not_installed("survey")
  design <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(ea_id) |> draw(n = 24) |>
    add_stage() |> draw(n = 12)
  selected <- execute(design, bfa_eas, stages = 1, seed = 1,
                      panels = subrounds(12))
  month <- dplyr::filter(selected, .panel == 3)
  listing <- as.data.frame(month)[c("ea_id", "region")]
  listing <- listing[rep(seq_len(nrow(listing)), each = 15), ]
  expect_warning(continued <- execute(month, listing, seed = 2),
                 class = "samplyr_warning_modified_sample")
  expect_error(execute(continued, period = 1, subround = 3),
               class = "samplyr_error_modified_sample")
  expect_error(as_svydesign(continued),
               class = "samplyr_error_modified_sample")
})

test_that("a written record carries the file fields only", {
  record <- attr(fix_srs, "metadata")$integrity
  expect_named(record, c("n_rows", "cols", "hash", "col_hashes",
                         "value_hashes"))
  expect_named(integrity_for_file(record), c("n_rows", "cols", "hash"))
})

## Generated columns are exact names

test_that("the generated names are matched exactly, not by prefix", {
  generated <- c(
    ".weight", ".sample_id", ".stage", ".panel", ".replicate", ".period",
    ".subround", ".visit", ".weight_1", ".weight_10", ".fpc_2", ".draw_1",
    ".certainty_3", ".zone_1", ".variance_group_1", ".reserve_2"
  )
  lookalike <- c(
    ".weight_adj", ".weight_", ".weights", ".fpc_inf_1", ".fpc_note",
    ".stage_name", ".panelist", ".replicate_id", ".period2", ".reserve_flag",
    "weight", "weight_1"
  )
  expect_identical(
    is_samplyr_column(c(generated, lookalike)),
    rep(c(TRUE, FALSE), c(length(generated), length(lookalike)))
  )
})

test_that("every column execution adds is generated and protected", {
  frame <- data.frame(id = 1:40, g = rep(1:2, 20), size = 1:40)
  schedule <- data.frame(
    panel = rep(1:2, times = 2), period = rep(1:2, each = 2),
    active = c(TRUE, FALSE, TRUE, TRUE)
  )
  master <- sampling_design() |>
    draw(n = 8) |>
    execute(frame, seed = 5, panels = schedule)
  bridge_frame <- certainty_element_frame(certainty_pair_register())
  samples <- list(
    list(sampling_design() |> draw(n = 6, method = "srswr") |>
           execute(frame, seed = 1), frame),
    list(sampling_design() |> draw(n = 6, reserve = 2) |>
           execute(frame, seed = 2), frame),
    list(sampling_design() |> stratify_by(g) |>
           draw(n = 4, method = "pps_brewer", mos = size) |>
           execute(frame, seed = 3, reps = 2), frame),
    list(master, frame),
    list(execute(master, period = 2), frame),
    list(execute(master, period = 1:2), frame),
    list(certainty_zone_design(certainty_pair_fixture()) |>
           execute(bridge_frame, seed = 1), bridge_frame)
  )
  added <- unique(unlist(lapply(samples, function(x) {
    setdiff(names(x[[1]]), names(x[[2]]))
  })))
  # Each family the package writes is exercised at least once.
  expect_setequal(
    added,
    c(".weight", ".sample_id", ".stage", ".weight_1", ".fpc_1", ".draw_1",
      ".reserve_1", ".replicate", ".certainty_1", ".panel", ".period",
      ".subround", ".visit", ".weight_2", ".fpc_2", ".zone_1", ".variance_group_1")
  )
  for (x in samples) {
    sample <- x[[1]]
    new_cols <- setdiff(names(sample), names(x[[2]]))
    expect_setequal(samplyr_internal_cols(sample), new_cols)
    expect_true(all(new_cols %in% attr(sample, "metadata")$integrity$cols))
  }
})

test_that("a frame column named like a generated one stays the user's", {
  skip_if_not_installed("survey")

  frame <- data.frame(
    id = 1:50, .weight_adj = 1, .stage_name = "a", .reserve_flag = FALSE
  )
  sample <- sampling_design() |> draw(n = 10) |> execute(frame, seed = 1)
  expect_identical(
    attr(sample, "metadata")$integrity$cols,
    c(".weight", ".sample_id", ".stage", ".weight_1", ".fpc_1")
  )

  assigned <- sample
  assigned$.weight_adj <- assigned$.weight * 1.2
  edited <- list(
    dplyr::mutate(sample, .weight_adj = .weight * 1.2),
    assigned,
    dplyr::select(sample, -.stage_name),
    dplyr::rename(sample, flag = .reserve_flag)
  )
  for (x in edited) {
    expect_length(sample_modifications(x), 0)
    expect_s3_class(as_svydesign(x), "survey.design")
  }
})

test_that("a column added to a sample carries into its next phase", {
  phase1 <- sampling_design() |>
    draw(n = 20) |>
    execute(data.frame(id = 1:100), seed = 1)
  phase1 <- dplyr::mutate(phase1, .weight_nr = .weight * 1.25)
  phase1 <- dplyr::mutate(phase1, .weight_nr = .weight_nr * 2)
  expect_length(sample_modifications(phase1), 0)

  phase2 <- sampling_design() |> draw(n = 5) |> execute(phase1, seed = 2)
  expect_identical(
    phase2$.weight_nr,
    phase1$.weight_nr[match(phase2$id, phase1$id)]
  )
  expect_equal(unique(phase2$.weight_nr), 12.5)
})

## Negative integer64 values

test_that("negative integer64 values are sorted and fingerprinted by value", {
  skip_if_not_installed("bit64")
  skip_if_not_installed("survey")
  # bit64 stores each value in the bits of a double, and negative values are
  # NaN payloads that a sort or a hash of doubles cannot tell apart.
  ids <- bit64::as.integer64(c(-5, 3, -100, 7, 0, -2147483648))
  frame <- data.frame(id = 1:6, k = ids, ea = rep(1:3, 2))
  expect_identical(control_order(frame, list(rlang::quo(k))),
                   c(6L, 3L, 1L, 5L, 2L, 4L))

  # An edited negative cluster id or stratum no longer passes as intact.
  frame <- data.frame(
    ea = bit64::as.integer64(rep(c(-100, -200, 300, -400, 500, -600), each = 2)),
    st = bit64::as.integer64(rep(c(-1, -2), 6)),
    hh = 1:12
  )
  sample <- sampling_design() |> cluster_by(ea) |> draw(n = 4) |>
    execute(frame, seed = 1)
  edited <- sample
  edited$ea[edited$ea == edited$ea[1]] <- bit64::as.integer64(-999999)
  integrity <- attr(sample, "metadata")$integrity
  expect_identical(verify_sample_integrity(edited, integrity), "values")
  expect_error(as_svydesign(edited), class = "samplyr_error_modified_sample")
  stratified <- sampling_design() |> stratify_by(st) |> draw(n = 2) |>
    execute(frame, seed = 1)
  edited <- stratified
  edited$st[1] <- bit64::as.integer64(-7)
  expect_identical(
    verify_sample_integrity(edited, attr(stratified, "metadata")$integrity),
    "values"
  )

  # A frame whose negative id was edited is another frame.
  changed <- frame
  changed$ea[1] <- bit64::as.integer64(-999999)
  expect_false(identical(frame_content_hash(frame), frame_content_hash(changed)))
})
