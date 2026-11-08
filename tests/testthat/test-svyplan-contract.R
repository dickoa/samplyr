## The svyplan schedule contract
#
# samplyr reads these fields of an svyplan schedule by name. svyplan keeps no
# format version, so a change of meaning under an unchanged column name
# fails here first.

contract_plan <- function(retain_psus = FALSE) {
  svyplan::design_schedule(
    svyplan::n_panel(
      svyplan::n_prop(p = 0.5, moe = 0.1),
      retention = c(0.9, 0.9, 0.9),
      resp_rate = 0.8,
      design = "rotating",
      start = "immediate",
      rotation = "2-2-2"
    ),
    horizon = 8,
    horizon_policy = "continuing",
    refreshment = "entrant_register",
    rounding = "ceiling",
    take = 10,
    retain_psus = retain_psus
  )
}

# Within a panel, the rows where a cohort differs from the previous active
# row, and the rows whose visit is 1.
cohort_openings <- function(tab) {
  tab <- tab[tab$active, , drop = FALSE]
  tab <- tab[order(tab$panel, tab$period, tab$subround), , drop = FALSE]
  first <- !duplicated(tab$panel)
  changed <- first | c(TRUE, tab$cohort[-1L] != tab$cohort[-nrow(tab)])
  list(changed = changed, visit_one = tab$visit == 1L)
}

test_that("the svyplan schedule contract changed: fields samplyr reads", {
  plan <- contract_plan()

  expect_no_error(samplyr:::check_svyplan_schedule(plan))
  expect_true(all(
    c("period", "subround", "selection", "cohort", "panel", "active",
      "visit") %in% names(plan$schedule)
  ))
  expect_true(all(
    c("period", "subround", "selection", "cohort", "panel", "visit") %in%
      names(plan$tail_commitments)
  ))
  expect_true(all(
    c("selection", "entry_period", "panels", "panel_issue",
      "operational_issue") %in% names(plan$selections)
  ))
  expect_identical(plan$subrounds, 1L)
  expect_identical(plan$horizon, 8L)
  expect_false(plan$retain_psus)
  expect_named(plan$panel_parameters, c("k", "r_min", "block_width"))
})

test_that("the svyplan schedule contract changed: selection names", {
  plan <- contract_plan()

  expect_identical(
    plan$selections$selection,
    c("startup", paste0("period_", 2:8))
  )
  expect_identical(plan$selections$entry_period, 1:8)
  expect_setequal(unique(plan$schedule$selection), plan$selections$selection)
})

test_that("the svyplan schedule contract changed: panels run across the schedule", {
  plan <- contract_plan()
  tab <- plan$schedule
  k <- plan$panel_parameters$k
  startup <- plan$selections$panels[[1L]]

  expect_identical(k, startup)
  expect_identical(sort(unique(tab$panel[tab$selection == "startup"])), seq_len(k))
  later <- tab$panel[tab$selection != "startup"]
  expect_true(all(later > k))
  # One panel belongs to one selection.
  owners <- tapply(tab$selection, tab$panel, function(x) length(unique(x)))
  expect_true(all(owners == 1L))
  expect_identical(
    sort(unique(tab$panel)),
    seq_len(sum(plan$selections$panels))
  )
})

test_that("the svyplan schedule contract changed: cohort and visit per row", {
  for (retain in c(FALSE, TRUE)) {
    plan <- contract_plan(retain_psus = retain)
    tab <- plan$schedule
    live <- tab[tab$active, , drop = FALSE]

    expect_false(anyNA(live$cohort))
    expect_false(anyNA(live$visit))
    expect_true(all(is.na(tab$visit[!tab$active])))

    # A new cohort opens at visit 1, and only there.
    open <- cohort_openings(tab)
    expect_identical(open$changed, open$visit_one)
  }
})

test_that("the svyplan schedule contract changed: retained PSUs take new cohorts", {
  plan <- contract_plan(retain_psus = TRUE)
  tab <- plan$schedule
  live <- tab[tab$active, , drop = FALSE]

  expect_true(plan$retain_psus)
  expect_identical(plan$selections$selection, "startup")
  cohorts <- tapply(live$cohort, live$panel, function(x) length(unique(x)))
  expect_true(any(cohorts > 1L))
})
