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

test_that("a schedule missing a field names the table it is missing from", {
  plan <- contract_plan()
  # A schedule refuses changes in place, so the copy is edited as a list.
  without <- function(table, column) {
    x <- unclass(plan)
    x[[table]][[column]] <- NULL
    class(x) <- class(plan)
    x
  }
  cases <- c(schedule = "subround", tail_commitments = "visit",
             selections = "panel_issue")
  for (table in names(cases)) {
    err <- expect_error(
      sampling_design() |>
        draw(n = 20) |>
        execute(data.frame(id = 1:100), seed = 1,
                panels = without(table, cases[[table]])),
      class = "samplyr_error_svyplan_schedule"
    )
    msg <- cli::ansi_strip(conditionMessage(err))
    expect_match(msg, paste0("$", table, " has no ", cases[[table]]),
                 fixed = TRUE)
    expect_no_match(msg, "${table}", fixed = TRUE)
  }
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

## The svyplan rate contract
#
# The reserve note reads the rates a fit used from `$rates` of an allocation,
# from the indicator table or `params$resp_rate` of a cluster plan, and from
# the indicator table of an `n_multi()` fit on the rows of `$detail` that set
# its size.

test_that("the svyplan rate contract changed: allocations report $rates", {
  frame <- data.frame(stratum = c("A", "B"), N = c(4000, 3000),
                      sd = c(10, 15), mean = c(50, 60), resp_rate = 0.5)
  classic <- svyplan::n_alloc(frame, cv = 0.02, resp_rate = c(1, 0.8))
  expect_identical(classic$rates,
                   data.frame(stratum = c("A", "B"), resp_rate = c(1, 0.8),
                              eligibility_rate = c(1, 1)))
  clustered <- svyplan::n_alloc(
    transform(frame[c("stratum", "N", "sd", "mean")], icc_1 = 0.05,
              cost_1 = 400, cost_2 = 40, resp_rate_1 = c(1, 0.85)),
    cv = 0.02
  )
  expect_identical(names(clustered$rates),
                   c("stratum", "resp_rate_1", "resp_rate", "eligibility_rate"))
  expect_identical(clustered$rates$resp_rate_1, c(1, 0.85))

  measures <- data.frame(stratum = rep(c("A", "B"), 2),
                         name = rep(c("y", "z"), each = 2),
                         p = c(0.5, 0.4, 0.3, 0.2),
                         resp_rate = c(1, NA, 0.5, 0.5))
  joint <- svyplan::n_alloc_joint(
    data.frame(stratum = c("A", "B"), N = c(4000, 3000),
               resp_rate = c(0.7, 0.6)),
    measures, targets = data.frame(name = "y", cv = 0.1),
    eligibility_rate = 0.9
  )
  expect_identical(joint$rates,
                   data.frame(stratum = c("A", "B"), name = c("y", "y"),
                              resp_rate = c(1, 0.6),
                              eligibility_rate = c(0.9, 0.9)))
  register <- certainty_plan_fixture()
  expect_identical(names(register$rates),
                   c("stratum", "name", "resp_rate", "eligibility_rate"))
})

test_that("the svyplan rate contract changed: cluster plans name stage rates", {
  scalar <- svyplan::n_cluster(stage_cost = c(500, 50), icc = 0.05, cv = 0.05,
                               resp_rate = c(0.9, 0.8))
  expect_identical(scalar$params$resp_rate,
                   c(resp_rate_1 = 0.9, resp_rate = 0.8))
  tg <- data.frame(name = "stunting", domain = c("urban", "rural"),
                   p = c(0.25, 0.35), cv = 0.08, icc_1 = 0.05,
                   resp_rate_1 = c(0.9, NA))
  domains <- svyplan::n_cluster(tg, domains = "domain",
                                stage_cost = c(500, 50),
                                resp_rate = c(0.85, 0.8))
  expect_identical(domains$params$domain_cols, "domain")
  # A row without its own rate takes the stage default.
  expect_identical(domains$indicators$resp_rate_1, c(0.9, 0.85))
  expect_identical(domains$indicators$resp_rate, c(0.8, 0.8))
})

test_that("the svyplan rate contract changed: n_multi() rates per indicator", {
  indicators <- data.frame(name = c("a", "b"), p = c(0.3, 0.5), moe = 0.06,
                           resp_rate = c(1, NA))
  plain <- svyplan::n_multi(indicators, resp_rate = 0.5)
  expect_s3_class(plain, "svyplan_multi")
  expect_null(plain$params$resp_rate)
  expect_null(plain$params$eligibility_rate)
  # A missing row rate takes the argument, and a column of ones is left out.
  expect_identical(plain$indicators$resp_rate, c(1, 0.5))
  expect_null(plain$indicators$eligibility_rate)
  expect_identical(plain$detail$name, c("a", "b"))
  expect_identical(plain$detail$.n[plain$detail$.binding], plain$n)
  scoped <- svyplan::n_multi(indicators[c("name", "p", "moe")],
                             eligibility_rate = 0.9)
  expect_identical(scoped$indicators$eligibility_rate, c(0.9, 0.9))
  unnamed <- svyplan::n_multi(indicators[c("p", "moe")])
  expect_identical(unnamed$detail$name, 1:2)

  domains <- data.frame(name = "a", region = c("N", "S"), p = c(0.3, 0.5),
                        moe = 0.06)
  split <- svyplan::n_multi(domains, domains = "region")
  expect_identical(split$params$domain_cols, "region")
  expect_identical(split$detail$region, c("N", "S"))
  expect_identical(split$detail$.n, split$domains$.n)
  expect_identical(
    svyplan::n_multi(domains[-1], domains = "region")$detail$name, 1:2
  )
})

test_that("the svyplan varcomp contract changed: the estimator choices", {
  # varcomp() on a sample passes `estimator` to the default method.
  formals_default <- formals(
    getS3method("varcomp", "default", envir = asNamespace("svyplan"))
  )
  expect_true("estimator" %in% names(formals_default))
  expect_identical(eval(formals_default$estimator), c("expansion", "ratio"))
  expect_identical(eval(formals(varcomp.tbl_sample)$estimator),
                   eval(formals_default$estimator))
})
