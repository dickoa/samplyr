## The linearized export of a weight-share transformation

# The oracle is sum_i w_i y_i = sum_j I(j in S) / pi_j * z_j, with
# z_j = sum_i L_ji / L_i * y_i, computed by exporting the source sample with z
# attached. The fixture links 15 of the 90 targets to two dwellings each.

linearized_targets <- function() {
  withr::with_seed(3, data.frame(
    person_id = 1:90,
    hh = rep(paste0("h", 1:45), each = 2),
    y = round(stats::rnorm(90, 100, 20), 3)
  ))
}

linearized_links <- function() {
  rbind(
    data.frame(dw_id = rep(1:45, each = 2), person_id = 1:90),
    # 15 people reached through a second dwelling as well.
    data.frame(dw_id = 46:60, person_id = 1:15)
  )
}

linearized_dwellings <- function() {
  data.frame(
    dw_id = 1:60,
    region = rep(c("n", "s"), each = 30),
    size = rep(c(2, 5, 3, 8, 4, 6), length.out = 60),
    blk = rep(paste0("b", 1:12), each = 5)
  )
}

linearized_shared <- function(source_sample,
                              targets = linearized_targets()) {
  share_weights(
    source_sample,
    targets = targets,
    links = linearized_links(),
    by = c(dw_id = "dw_id"),
    to = c(person_id = "person_id"),
    within = hh,
    multiplicity = complete_links()
  )
}

# The identity, computed from the recorded operator and the source design.
linearized_oracle <- function(shared, source_sample, variable = "y") {
  operator <- attr(shared, "metadata")$weight_share$operator
  contributions <- operator$share * shared[[variable]][operator$target_row]
  totals <- rowsum(contributions, group = operator$source_row, reorder = FALSE)

  derived <- rep(0, nrow(source_sample))
  derived[as.integer(rownames(totals))] <- totals[, 1]
  source_sample$.derived <- derived
  suppressWarnings(survey::svytotal(~.derived, as_svydesign(source_sample)))
}

expect_matches_oracle <- function(source_sample,
                                 targets = linearized_targets()) {
  shared <- linearized_shared(source_sample, targets)
  got <- suppressWarnings(survey::svytotal(~y, as_svydesign(shared)))
  want <- linearized_oracle(shared, source_sample)

  expect_equal(unname(coef(got)), unname(coef(want)))
  expect_equal(unname(survey::SE(got)), unname(survey::SE(want)))
  invisible(shared)
}

## The identity

test_that("the export is the HT total of the derived source variable", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6)) |>
    execute(linearized_dwellings(), seed = 7)

  shared <- expect_matches_oracle(source_sample)

  # The point estimate is the transformation's own weighted total.
  expect_equal(
    unname(coef(survey::svytotal(~y, as_svydesign(shared)))),
    sum(shared$.weight * shared$y)
  )
})

test_that("it holds for every source design shape it accepts", {
  skip_if_not_installed("survey")
  dwellings <- linearized_dwellings()

  expect_matches_oracle(
    sampling_design() |> draw(n = 18) |> execute(dwellings, seed = 7)
  )
  expect_matches_oracle(
    suppressWarnings(
      sampling_design() |>
        draw(n = 18, method = "systematic") |>
        execute(dwellings, seed = 7)
    )
  )
  expect_matches_oracle(
    sampling_design() |>
      add_stage() |>
      cluster_by(blk) |>
      draw(n = 6) |>
      add_stage() |>
      draw(n = 3) |>
      execute(dwellings, seed = 7)
  )
  # With replacement, a size measure included: the variance reads draw
  # identifiers, which the expansion carries, not a structure over rows.
  expect_matches_oracle(
    sampling_design() |>
      draw(n = 18, method = "pps_multinomial", mos = size) |>
      execute(dwellings, seed = 7)
  )
})

test_that("a target reached twice is carried, with no condition on it", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)
  operator <- attr(shared, "metadata")$weight_share$operator

  expect_gt(length(operator$share), nrow(shared))
  expect_gt(max(table(operator$target_row)), 1L)
  expect_identical(
    nrow(as_svydesign(shared)$variables),
    length(operator$share)
  )
})

## The ordering the export depends on

test_that("a source unit is one sampling unit, not one per contribution", {
  skip_if_not_installed("survey")
  # Per-contribution units would understate the variance without failing.
  source_sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6)) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)
  svy <- as_svydesign(shared)

  expect_identical(deparse(svy$call$ids), "~.source_unit")
  operator <- attr(shared, "metadata")$weight_share$operator
  expect_identical(
    svy$variables$.source_unit,
    source_sample$.sample_id[operator$source_row]
  )

  # Per-contribution units give a smaller SE on this fixture.
  wrong <- svy
  wrong$variables$.each <- seq_len(nrow(wrong$variables))
  by_row <- survey::svydesign(
    ids = ~.each, strata = ~region, weights = ~.weight,
    fpc = ~.fpc_1, data = wrong$variables, nest = TRUE
  )
  expect_lt(
    survey::SE(survey::svytotal(~y, by_row)),
    survey::SE(survey::svytotal(~y, svy))
  )
})

test_that("linearization refuses a selected source with no contribution", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6)) |>
    execute(linearized_dwellings(), seed = 7)
  unlinked <- source_sample$dw_id[[1L]]
  links <- linearized_links()
  links <- links[links$dw_id != unlinked, , drop = FALSE]

  shared <- share_weights(
    source_sample,
    targets = linearized_targets(),
    links = links,
    by = c(dw_id = "dw_id"),
    to = c(person_id = "person_id"),
    within = hh,
    multiplicity = complete_links()
  )

  expect_error(
    as_svydesign(shared),
    class = "samplyr_error_share_weights_zero_contribution_source"
  )
  expect_no_error(suppressWarnings(as_svrepdesign(shared, type = "JKn")))
})

test_that("the contribution weight is the coefficient times the source's", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)
  operator <- attr(shared, "metadata")$weight_share$operator

  expect_equal(
    as_svydesign(shared)$variables$.weight,
    operator$share * source_sample$.weight[operator$source_row]
  )
  expect_equal(
    sum(as_svydesign(shared)$variables$.weight),
    sum(shared$.weight)
  )
})

test_that("a mean's denominator is the target population either way", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)

  expect_equal(
    unname(coef(survey::svymean(~y, as_svydesign(shared)))),
    sum(shared$.weight * shared$y) / sum(shared$.weight)
  )
})

## What it refuses

test_that("a source design whose variance is indexed by its rows is refused", {
  skip_if_not_installed("survey")
  # Pairwise and random-size variances do not survive one row becoming several.
  dwellings <- linearized_dwellings()

  for (design in list(
    sampling_design() |> draw(n = 18, method = "pps_brewer", mos = size),
    sampling_design() |> draw(n = 18, method = "pps_poisson", mos = size),
    sampling_design() |> draw(frac = 0.3, method = "bernoulli")
  )) {
    shared <- linearized_shared(execute(design, dwellings, seed = 7))
    expect_error(
      as_svydesign(shared),
      class = "samplyr_error_share_weights_source_design"
    )
  }

  # The route the refusal names does take the unequal-probability case.
  unequal <- linearized_shared(
    sampling_design() |>
      draw(n = 18, method = "pps_brewer", mos = size) |>
      execute(dwellings, seed = 7)
  )
  expect_no_error(
    suppressWarnings(as_svrepdesign(unequal, type = "subbootstrap"))
  )
})

test_that("a source sample that lost a primary unit is refused", {
  skip_if_not_installed("survey")
  # Block b1 has no dwellings in the register, so it has no row.
  dwellings <- linearized_dwellings()
  blocks <- unique(dwellings[c("blk", "region")])
  source_sample <- sampling_design() |>
    cluster_by(blk) |> draw(n = 6) |>
    add_stage() |> draw(frac = 1, on_empty = "silent") |>
    execute(list(blocks, dwellings[dwellings$blk != "b1", ]), seed = 1)
  expect_false("b1" %in% source_sample$blk)
  expect_true(any(vapply(
    attr(source_sample, "metadata")$empty_parents,
    function(r) "b1" %in% r$keys$blk, NA
  )))
  expect_error(
    as_svydesign(linearized_shared(source_sample)),
    class = "samplyr_error_export_empty_psu"
  )
})

test_that("pps is refused, because it describes the unexpanded sample", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)

  expect_error(
    as_svydesign(shared, pps = "anything"),
    class = "samplyr_error_share_weights_pps"
  )
})

test_that("arguments forwarded to survey are checked as on every other path", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)

  expect_error(as_svydesign(shared, nesst = TRUE),
               class = "samplyr_error_unknown_argument")
  expect_error(as_svydesign(shared, ids = ~1),
               class = "samplyr_error_derived_argument")
  expect_error(as_svydesign(shared, 5),
               class = "samplyr_error_unnamed_argument")
  expect_s3_class(as_svydesign(shared, check.strata = FALSE), "survey.design2")
  skip_if_not_installed("srvyr")
  expect_error(srvyr::as_survey_design(shared, nesst = TRUE),
               class = "samplyr_error_unknown_argument")
})

test_that("a two-phase source is refused without naming the route that refused it", {
  skip_if_not_installed("survey")

  # Both phases declare the dwelling as their unit, so the bridge exists.
  phase1 <- sampling_design() |>
    cluster_by(dw_id) |>
    draw(n = 40) |>
    execute(linearized_dwellings(), seed = 11)
  phase2 <- sampling_design() |>
    cluster_by(dw_id) |>
    draw(n = 18) |>
    execute(phase1, seed = 12)
  shared <- linearized_shared(phase2)

  # Neither route takes it, so neither refusal may advise the other.
  for (export in list(
    function() as_svydesign(shared),
    function() as_svrepdesign(shared, type = "bootstrap", replicates = 10)
  )) {
    expect_error(export(), class = "samplyr_error_share_weights_twophase_source")
    expect_error(export(), regexp = "weights were shared from is two-phase")
  }
  expect_no_match(
    conditionMessage(tryCatch(as_svydesign(shared), error = identity)),
    "Use `as_svydesign()`",
    fixed = TRUE
  )

  # The advice it gives instead has to work.
  from_phase1 <- linearized_shared(phase1)
  expect_s3_class(as_svydesign(from_phase1), "survey.design")

  # The ordinary two-phase route keeps its own class and its own advice.
  expect_error(
    as_svrepdesign(phase2, type = "bootstrap", replicates = 10),
    class = "samplyr_error_svrep_twophase_unsupported"
  )
  expect_s3_class(as_svydesign(phase2), "twophase2")
})

test_that("a target column may not take a source design column's name", {
  skip_if_not_installed("survey")
  # These names carry the source selection's strata, units or counts.
  targets <- linearized_targets()
  targets$region <- "n"

  source_sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6)) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample, targets)

  expect_error(
    as_svydesign(shared),
    class = "samplyr_error_share_weights_columns"
  )
})

test_that("a target column named like samplyr's is carried unless reserved", {
  skip_if_not_installed("survey")
  # Only the names execute() writes are samplyr's. The replicate export
  # already carried the others.
  source_sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6)) |>
    execute(linearized_dwellings(), seed = 7)
  targets <- linearized_targets()
  targets$.weight_adj <- seq_len(nrow(targets)) + 0.5
  targets$.fpc_note <- rev(seq_len(nrow(targets))) + 0.25
  targets$.weight_1 <- -1
  shared <- linearized_shared(source_sample, targets)
  svy <- as_svydesign(shared)
  operator <- attr(shared, "metadata")$weight_share$operator
  for (nm in c(".weight_adj", ".fpc_note")) {
    expect_identical(
      svy$variables[[nm]],
      shared[[nm]][operator$target_row],
      info = nm
    )
  }
  # A name execute() writes stays the source design's own.
  expect_identical(
    svy$variables$.weight_1,
    source_sample$.weight_1[operator$source_row]
  )
})

test_that("generated source columns never overwrite a user column", {
  skip_if_not_installed("survey")
  # Each generated name is given to a source column, then to a target column.
  element <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(n = 12, s = 6))
  two_stage <- sampling_design() |>
    add_stage() |>
    cluster_by(blk) |>
    draw(n = 6) |>
    add_stage() |>
    stratify_by(region) |>
    draw(n = 2)
  with_replacement <- sampling_design() |>
    draw(n = 20, method = "srswr")
  cases <- list(
    list(design = element, generated = ".source_unit"),
    list(design = with_replacement, generated = ".fpc_inf_1"),
    list(design = two_stage, generated = c(".id_1", ".id_2", ".strata_all_1"))
  )

  for (case in cases) {
    source_sample <- execute(case$design, linearized_dwellings(), seed = 7)
    shared <- linearized_shared(source_sample)
    clean <- as_svydesign(shared)
    expect_setequal(
      setdiff(names(clean$variables), union(names(source_sample), names(shared))),
      case$generated
    )
    reference <- survey::svytotal(~y, clean)

    for (nm in case$generated) {
      user_source <- source_sample
      user_source[[nm]] <- seq_len(nrow(user_source)) + 0.5
      shared <- linearized_shared(user_source)
      svy <- as_svydesign(shared)
      operator <- attr(shared, "metadata")$weight_share$operator
      expect_identical(
        svy$variables[[nm]],
        user_source[[nm]][operator$source_row],
        info = nm
      )
      total <- survey::svytotal(~y, svy)
      expect_equal(coef(total), coef(reference), info = nm)
      expect_equal(vcov(total), vcov(reference), info = nm)

      targets <- linearized_targets()
      targets[[nm]] <- seq_len(nrow(targets)) + 0.5
      shared <- linearized_shared(source_sample, targets)
      svy <- as_svydesign(shared)
      expect_equal(
        unname(coef(survey::svytotal(stats::reformulate(nm), svy))),
        sum(shared$.weight * shared[[nm]]),
        info = nm
      )
      total <- survey::svytotal(~y, svy)
      expect_equal(coef(total), coef(reference), info = nm)
      expect_equal(vcov(total), vcov(reference), info = nm)
    }
  }
})

test_that("a stack still refuses a shared component on this route", {
  skip_if_not_installed("survey")
  # survey::multiframe() reads one selection probability per row.
  targets <- linearized_targets()
  targets$in_reached <- TRUE
  targets$in_list <- rep(c(TRUE, FALSE, TRUE), 30)

  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample, targets)
  listed <- sampling_design() |>
    draw(n = 20) |>
    execute(targets[targets$in_list, , drop = FALSE], seed = 9)

  frames <- stack_frames(
    reached = shared, list = listed,
    membership = c(reached = "in_reached", list = "in_list"),
    key = person_id
  )
  expect_error(
    as_svydesign(frames),
    class = "samplyr_error_survey_weight_contract"
  )
  expect_error(as_svydesign(frames), regexp = "one selection probability")
})

## What the export carries

test_that("the transformation and its coverage travel with the design", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)
  svy <- as_svydesign(shared)

  record <- attr(svy, "samplyr_weight_share")
  expect_identical(record$algorithm, "generalized_weight_share")
  expect_identical(record$n_source_rows, nrow(source_sample))
  expect_identical(
    record$n_contributions,
    length(attr(shared, "metadata")$weight_share$operator$share)
  )
  expect_false(is_null(attr(svy, "samplyr_weight_share_coverage")))
})

test_that("a tampered result is refused before anything is built", {
  skip_if_not_installed("survey")
  source_sample <- sampling_design() |>
    draw(n = 18) |>
    execute(linearized_dwellings(), seed = 7)
  shared <- linearized_shared(source_sample)

  tampered <- shared
  tampered$.weight <- tampered$.weight * 2
  expect_error(as_svydesign(tampered), class = "samplyr_error")

  # The integrity record cannot see an altered source, so alignment catches it.
  altered <- shared
  metadata <- attr(altered, "metadata")
  metadata$weight_share$source_sample$.weight[[1]] <- 999
  attr(altered, "metadata") <- metadata

  expect_silent(check_sample_unmodified(altered, "test"))
  expect_error(
    as_svydesign(altered),
    class = "samplyr_error_weight_share_misaligned"
  )

  # Reordering is recoverable, and gives the same numbers.
  reordered <- shared[rev(seq_len(nrow(shared))), ]
  expect_equal(
    unname(coef(survey::svytotal(~y, as_svydesign(reordered)))),
    unname(coef(survey::svytotal(~y, as_svydesign(shared))))
  )
  expect_equal(
    unname(survey::SE(survey::svytotal(~y, as_svydesign(reordered)))),
    unname(survey::SE(survey::svytotal(~y, as_svydesign(shared))))
  )
})
