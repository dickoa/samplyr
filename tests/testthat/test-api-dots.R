test_that("reserved dots reject unexpected arguments", {
  design <- sampling_design() |>
    draw(n = 3)
  sample <- execute(
    design,
    data.frame(id = 1:10),
    seed = 1,
    frame_digest = "none"
  )
  plain <- tibble::as_tibble(sample)

  expect_error(
    print(design, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    as.list(design, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    summary(sample, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    as_tbl_sample(sample, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
  expect_error(
    as_tbl_sample(plain, unexpected = TRUE),
    class = "rlib_error_dots_nonempty"
  )
  # write_design() reserves its dots too, through the package's own guard
  # rather than rlang's, so a near miss is named.
  expect_error(
    write_design(design, withr::local_tempfile(), unexpected = TRUE),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    write_design(design, withr::local_tempfile(), prettyy = TRUE),
    "Did you mean.*pretty"
  )
})

## Forwarded dots: the export verbs pass `...` to survey, so an argument
## that neither side owns must be refused by name rather than handed on.

dots_cluster_sample <- function(seed = 1) {
  frame <- withr::with_seed(seed, {
    data.frame(
      psu = rep(seq_len(40), each = 5),
      id = seq_len(200),
      y = stats::rnorm(200),
      stratum = rep(c("a", "b"), each = 100)
    )
  })
  sampling_design() |>
    stratify_by(stratum) |>
    cluster_by(psu) |>
    draw(n = 6) |>
    execute(frame, seed = seed)
}

test_that("as_svydesign() refuses a misspelled argument of its own", {
  skip_if_not_installed("survey")
  s <- dots_cluster_sample()

  expect_error(
    as_svydesign(s, nes = FALSE),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    as_svydesign(s, methodd = "full"),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(as_svydesign(s, nes = FALSE), "Did you mean.*nest")
})

test_that("as_svydesign() forwards what survey::svydesign() accepts", {
  skip_if_not_installed("survey")
  s <- dots_cluster_sample()

  expect_s3_class(as_svydesign(s), "survey.design")
  expect_s3_class(as_svydesign(s, nest = FALSE), "survey.design")
  expect_s3_class(as_svydesign(s, variables = ~y), "survey.design")
  expect_s3_class(as_svydesign(s, check.strata = FALSE), "survey.design")
})

test_that("as_svydesign() refuses a name neither it nor survey owns", {
  skip_if_not_installed("survey")
  s <- dots_cluster_sample()

  expect_error(
    as_svydesign(s, zzz = 1),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(as_svydesign(s, zzz = 1), "survey::svydesign")
  # A stray name is read without being forced, so an expression that would
  # fail is still diagnosed as the argument it was meant to be.
  expect_error(
    as_svydesign(s, nes = stop("must not be evaluated")),
    class = "samplyr_error_unknown_argument"
  )
})

test_that("the export verbs refuse an argument samplyr derives itself", {
  skip_if_not_installed("survey")
  s <- dots_cluster_sample()

  # Accepting these forwarded the value into a do.call() whose named
  # arguments were already fixed, so survey raised "formal argument matched
  # by multiple actual arguments" from a verb the user never called. The
  # name is survey's, so "unknown" would be the wrong report.
  for (arg in samplyr:::svydesign_derived_args) {
    call_args <- list(s)
    call_args[[arg]] <- ~y
    expect_error(
      do.call(as_svydesign, call_args),
      class = "samplyr_error_derived_argument"
    )
  }
  expect_error(
    as_svrepdesign(s, design = "x"),
    class = "samplyr_error_derived_argument"
  )

  # Diagnosed from the name alone: the value is never forced, so a stray
  # `data = frame` on a large frame does not evaluate before the message.
  expect_error(
    as_svydesign(s, ids = stop("must not be evaluated")),
    class = "samplyr_error_derived_argument"
  )
  expect_error(
    as_svrepdesign(s, design = stop("must not be evaluated")),
    class = "samplyr_error_derived_argument"
  )

  cnd <- tryCatch(as_svydesign(s, strata = ~y), error = function(e) e)
  expect_identical(cnd$argument, "strata")
  expect_match(conditionMessage(cnd), "samplyr derives")
  # One class for the whole category: handling it must not require knowing
  # which argument was refused.
  expect_identical(
    class(tryCatch(as_svrepdesign(s, design = 1), error = function(e) e))[1:2],
    c("samplyr_error_derived_argument", "samplyr_error")
  )

  # `pps` is extracted from the dots before the do.call() on both paths and
  # stays the documented route to an exact PPS variance.
  expect_s3_class(
    as_svydesign(s, pps = NULL), "survey.design"
  )

  # A near miss of a derived name is still unknown, since that is what was
  # typed, but the answer names what was meant and why it will not work
  # either. Dropping derived names from the suggestion candidates left
  # `strat` with nothing but the generic advice.
  expect_error(
    as_svydesign(s, strat = ~y),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(as_svydesign(s, strat = ~y), "Did you mean.*strata")
  expect_error(as_svydesign(s, strat = ~y), "samplyr derives it")
  expect_error(as_svydesign(s, dat = 1), "Did you mean.*data")

  # An accepted name wins a distance tie against a derived one, since it is
  # the suggestion the user can act on. `fps` is one edit from both the
  # accepted `pps` and the derived `fpc`.
  expect_error(as_svydesign(s, fps = 1), "Did you mean.*pps")
  expect_false(grepl(
    "samplyr derives it",
    conditionMessage(tryCatch(
      as_svydesign(s, fps = 1), error = function(e) e
    ))
  ))
})

test_that("a two-phase export refuses the arguments twophase() derives", {
  skip_if_not_installed("survey")
  skip_if_not_installed("tidyr")

  frame <- data.frame(site = seq_len(40))
  design <- sampling_design() |>
    add_stage("Sites") |>
    cluster_by(site) |>
    draw(frac = 0.5) |>
    add_stage("People") |>
    cluster_by(person) |>
    draw(n = 2)
  stage1 <- execute(design, frame, stages = 1, seed = 1)
  listing <- tidyr::expand_grid(site = stage1$site, person = seq_len(5))
  phase2 <- suppressWarnings(execute(design, stage1, listing, seed = 2))

  # survey::twophase() takes `id` where svydesign() takes `ids`, and `subset`
  # belongs to it alone. Both are supplied by samplyr, so both are derived.
  for (arg in samplyr:::twophase_derived_args) {
    call_args <- list(phase2, method = "simple")
    call_args[[arg]] <- ~site
    expect_error(
      do.call(as_svydesign, call_args),
      class = "samplyr_error_derived_argument"
    )
  }

  # `ids` is not a formal of twophase(), so on this path it is unknown
  # rather than derived: the two categories are read per path.
  expect_error(
    as_svydesign(phase2, method = "simple", ids = ~site),
    class = "samplyr_error_unknown_argument"
  )
  expect_s3_class(
    as_svydesign(phase2, method = "simple", pps = NULL), "twophase"
  )
})

test_that("the export verbs refuse a positional argument in the dots", {
  skip_if_not_installed("survey")
  s <- dots_cluster_sample()

  # survey matches it to whichever formal is still free: passed on, ~y
  # would have become as.svrepdesign()'s fay.rho.
  expect_error(
    as_svrepdesign(s, ~y),
    class = "samplyr_error_unnamed_argument"
  )
  expect_error(
    as_svydesign(s, ~y),
    class = "samplyr_error_unnamed_argument"
  )
})

test_that("as_svrepdesign() refuses a misspelled type but keeps survey's own", {
  skip_if_not_installed("survey")
  s <- dots_cluster_sample()

  expect_error(
    as_svrepdesign(s, typ = "JK1"),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(as_svrepdesign(s, typ = "JK1"), "Did you mean.*type")
  expect_error(
    as_svrepdesign(s, zzz = 1),
    class = "samplyr_error_unknown_argument"
  )

  # These reach survey through two forwarding layers and are bound without
  # being forced, so rlang::check_dots_used() reports them as unused. They
  # are valid, which is why the accepted names are listed explicitly.
  # BRR-family conversion drops the finite population correction and says so.
  expect_s3_class(
    suppressWarnings(as_svrepdesign(s, type = "Fay", fay.rho = 0.3)),
    "svyrep.design"
  )
  expect_s3_class(
    as_svrepdesign(s, type = "bootstrap", fpctype = "fraction"),
    "svyrep.design"
  )
  expect_s3_class(
    as_svrepdesign(s, type = "JKn", lonely.psu = "fail"),
    "svyrep.design"
  )
  expect_s3_class(
    as_svrepdesign(s, type = "subbootstrap", replicates = 10),
    "svyrep.design"
  )
  expect_s3_class(
    as_svrepdesign(s, type = "bootstrap", mse = TRUE),
    "svyrep.design"
  )
})

test_that("as_survey_design() inherits the as_svydesign() guard", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  s <- dots_cluster_sample()

  expect_error(
    srvyr::as_survey_design(s, nes = FALSE),
    class = "samplyr_error_unknown_argument"
  )
  expect_s3_class(srvyr::as_survey_design(s), "tbl_svy")
})

test_that("the accepted survey argument sets exist in the installed survey", {
  skip_if_not_installed("survey")

  svydesign_formals <- union(
    names(formals(survey::svydesign)),
    names(formals(utils::getS3method(
      "svydesign",
      "default",
      envir = asNamespace("survey")
    )))
  )
  expect_setequal(
    setdiff(samplyr:::svydesign_accepted_args, svydesign_formals),
    character()
  )
  expect_setequal(
    setdiff(samplyr:::twophase_accepted_args, names(formals(survey::twophase))),
    character()
  )

  # Pinned literally, not derived from survey's formals: the loops in the
  # tests above are vacuous on an empty set, so a name silently dropped from
  # a derived set would otherwise go unnoticed. Changing the contract has to
  # mean changing this list.
  expect_setequal(
    samplyr:::svydesign_derived_args,
    c("ids", "probs", "strata", "weights", "fpc", "data")
  )
  expect_setequal(
    samplyr:::twophase_derived_args,
    c("id", "strata", "probs", "weights", "fpc", "subset", "data")
  )
  expect_setequal(samplyr:::svrepdesign_derived_args, "design")

  # A derived name is one survey declares and samplyr fills in. If it were
  # not a formal it would be a typo, and if it were also accepted the guard
  # would let it through to collide in the do.call().
  expect_setequal(
    setdiff(samplyr:::svydesign_derived_args, svydesign_formals),
    character()
  )
  expect_setequal(
    setdiff(samplyr:::twophase_derived_args, names(formals(survey::twophase))),
    character()
  )
  expect_setequal(
    intersect(
      samplyr:::svydesign_accepted_args, samplyr:::svydesign_derived_args
    ),
    character()
  )
  expect_setequal(
    intersect(
      samplyr:::twophase_accepted_args, samplyr:::twophase_derived_args
    ),
    character()
  )
  expect_setequal(
    intersect(
      samplyr:::svrepdesign_accepted_args, samplyr:::svrepdesign_derived_args
    ),
    character()
  )
  # as.svrepdesign() reaches its replicate-weight generators through a
  # second `...`, so only the part it declares is checkable here. The rest
  # is covered by the calls above, which fail if a name is dropped.
  svrepdesign_formals <- c(
    names(formals(utils::getS3method(
      "as.svrepdesign",
      "default",
      envir = asNamespace("survey")
    ))),
    "match", "small", "large", "hadamard.matrix", "lonely.psu",
    "replicates", "multicore"
  )
  expect_setequal(
    setdiff(samplyr:::svrepdesign_accepted_args, svrepdesign_formals),
    character()
  )
  expect_setequal(
    setdiff(samplyr:::svrepdesign_derived_args, svrepdesign_formals),
    character()
  )
})

test_that("nest is reported as inert on a two-phase export", {
  skip_if_not_installed("survey")
  skip_if_not_installed("tidyr")

  frame <- data.frame(site = seq_len(40))
  design <- sampling_design() |>
    add_stage("Sites") |>
    cluster_by(site) |>
    draw(frac = 0.5) |>
    add_stage("People") |>
    cluster_by(person) |>
    draw(n = 2)

  stage1 <- execute(design, frame, stages = 1, seed = 1)
  listing <- tidyr::expand_grid(site = stage1$site, person = seq_len(5))
  phase2 <- suppressWarnings(execute(design, stage1, listing, seed = 2))

  expect_warning(
    as_svydesign(phase2, nest = FALSE, method = "simple"),
    class = "samplyr_warning_nest_ignored"
  )
  # The default must stay silent, and a single-phase export never warns.
  expect_no_warning(as_svydesign(phase2, method = "simple"))
  expect_no_warning(as_svydesign(dots_cluster_sample(), nest = FALSE))

  # `subset` belongs to twophase() alone; `variables` to svydesign() alone.
  expect_error(
    as_svydesign(phase2, variables = ~site),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    as_svydesign(dots_cluster_sample(), subset = ~y),
    class = "samplyr_error_unknown_argument"
  )
})

test_that("varcomp() refuses a misspelled strata instead of dropping it", {
  frame <- data.frame(
    cl = rep(sprintf("c%02d", seq_len(24)), each = 5),
    size = rep(rep(c(80, 120, 160, 200), 6), each = 5),
    dom = rep(c("a", "b"), each = 60),
    y = withr::with_seed(3, stats::rnorm(120))
  )
  s <- sampling_design() |>
    add_stage() |>
    cluster_by(cl) |>
    draw(n = 8, method = "pps_brewer", mos = size) |>
    add_stage() |>
    draw(n = 3) |>
    execute(frame, seed = 11)

  expect_error(
    varcomp(s, ~y, strat = ~dom),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(varcomp(s, ~y, strat = ~dom), "Did you mean.*strata")
  expect_error(
    varcomp(s, ~y, nonsense = 1),
    class = "samplyr_error_unknown_argument"
  )
  # A second positional formula was read as the outcome's neighbour and
  # dropped, so a per-stratum call came back unstratified.
  expect_error(
    varcomp(s, ~y, ~dom),
    class = "samplyr_error_unnamed_argument"
  )

  # The named spelling reaches the estimator: the guard is worth having
  # only because these two differ.
  plain <- as.data.frame(varcomp(s, ~y))
  by_dom <- as.data.frame(varcomp(s, ~y, strata = ~dom))
  expect_false(identical(plain, by_dom))
  expect_true(nrow(by_dom) > nrow(plain))
})

test_that("the forwarded pps argument is read exactly, not by prefix", {
  skip_if_not_installed("survey")
  s <- dots_cluster_sample()

  # `$` on a list matches by prefix, so a dot merely starting with "pps"
  # was read as the pps specification and changed the exported design.
  # The public guard now refuses such a name, so read the internal builder
  # directly to keep the exact match pinned.
  plain <- samplyr:::build_singlephase_svydesign(
    s,
    dots = list(),
    nest = TRUE
  )
  prefixed <- samplyr:::build_singlephase_svydesign(
    s,
    dots = list(ppsx = survey::ppsmat(diag(nrow(s)))),
    nest = TRUE
  )
  expect_identical(class(plain), class(prefixed))
  expect_identical(plain$pps, prefixed$pps)

  expect_error(
    as_svydesign(s, ppsx = 1),
    class = "samplyr_error_unknown_argument"
  )
})

test_that("check_forwarded_args() reads names without forcing values", {
  captured <- function(...) {
    samplyr:::check_forwarded_args(
      rlang::enquos(...),
      owned = "nest",
      accepted = c("strata", "fpc"),
      forwarded_to = "survey::svydesign"
    )
  }

  expect_null(captured())
  expect_null(captured(nest = FALSE, strata = ~a, fpc = ~b))
  expect_error(captured(zzz = 1), class = "samplyr_error_unknown_argument")
  expect_error(captured(1), class = "samplyr_error_unnamed_argument")
  expect_error(
    captured(zzz = stop("must not be evaluated")),
    class = "samplyr_error_unknown_argument"
  )
  # A suggestion is made against the forwarded names too, not only our own.
  expect_error(captured(strat = ~a), "Did you mean.*strata")
  expect_error(captured(zzz = 1), "arguments owned here are")
})

## Weighting-loss verbs: no outcome, but the planning arguments forward

test_that("design_effect() and effective_n() refuse a positional outcome", {
  frame <- data.frame(g = rep(1:10, each = 5), y = withr::with_seed(1, rnorm(50)))
  sample <- sampling_design() |>
    cluster_by(g) |>
    draw(n = 3) |>
    execute(frame, seed = 1)

  # `design_effect(x, y)` reads as "the design effect for y" and is the
  # natural first attempt. Unguarded it was forwarded to svyplan and forced
  # there, reporting `object 'y' not found`.
  for (call in list(
    function() design_effect(sample, y),
    function() effective_n(sample, y)
  )) {
    err <- expect_error(call(), class = "samplyr_error_unnamed_argument")
    msg <- cli::ansi_strip(conditionMessage(err))
    expect_match(msg, "takes no outcome variable")
    expect_match(msg, "svymean")
    expect_false(grepl("object 'y' not found", msg, fixed = TRUE))
  }

  # Named planning arguments still forward and still compute: the weighting
  # loss multiplied by the anticipated clustering component. Refusing the
  # whole of `...` would have broken this.
  expect_equal(as.double(design_effect(sample)), 1)
  expect_equal(
    as.double(design_effect(sample, icc = 0.1, n_per_psu = 5)), 1.4
  )
  expect_gt(as.double(effective_n(sample)), 0)

  # svyplan keeps rejecting names it does not know, so samplyr does not
  # duplicate that check.
  expect_error(design_effect(sample, zzz = 1), "zzz")
})

test_that("varcomp() names the contract when the outcome cannot be evaluated", {
  frame <- data.frame(
    cl = rep(sprintf("c%02d", 1:24), each = 5),
    size = rep(rep(c(80, 120, 160, 200), 6), each = 5),
    y = withr::with_seed(3, rnorm(120))
  )
  sample <- sampling_design() |>
    add_stage() |> cluster_by(cl) |>
    draw(n = 8, method = "pps_brewer", mos = size) |>
    add_stage() |> draw(n = 3) |>
    execute(frame, seed = 11)

  err <- expect_error(
    varcomp(sample, y),
    class = "samplyr_error_varcomp_formula"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "one-sided formula")
  expect_match(msg, "`y` could not be evaluated")

  # The expression alone cannot decide: a symbol holding a formula is a
  # legitimate outcome, which is why this is caught around the forcing.
  outcome <- ~y
  expect_s3_class(varcomp(sample, outcome), "svyplan_varcomp")
  expect_s3_class(varcomp(sample, ~y), "svyplan_varcomp")
})

test_that("stratify_by() suggests alloc for an expanded spelling", {
  # `allocation` is five edits from `alloc`, so the edit-distance rule says
  # nothing; the prefix fallback is enabled only where the value is already
  # known not to be a bare column name.
  err <- expect_error(
    sampling_design() |> stratify_by(region, allocation = "proportional")
  )
  expect_match(cli::ansi_strip(conditionMessage(err)), "Did you mean .?alloc")

  # A named bare column is a legitimate rename and is untouched.
  design <- sampling_design() |> stratify_by(region, cost_center = urban)
  expect_identical(design$stages[[1]]$strata$vars, c("region", "urban"))

  # No suggestion is invented when nothing matches.
  err2 <- expect_error(
    sampling_design() |> stratify_by(region, zzz = "x")
  )
  expect_false(grepl("Did you mean", cli::ansi_strip(conditionMessage(err2))))
})

test_that("the prefix fallback is opt-in and picks the longest match", {
  expect_null(samplyr:::suggest_reserved_arg("allocation", c("alloc", "cost")))
  expect_identical(
    samplyr:::suggest_reserved_arg(
      "allocation", c("alloc", "cost"), prefix = TRUE
    ),
    "alloc"
  )
  # Near misses still win over the prefix rule.
  expect_identical(
    samplyr:::suggest_reserved_arg("allocs", c("alloc", "cost"), prefix = TRUE),
    "alloc"
  )
  # Longest prefix, not the first.
  expect_identical(
    samplyr:::suggest_reserved_arg(
      "variance_data", c("var", "variance"), prefix = TRUE
    ),
    "variance"
  )
  expect_null(
    samplyr:::suggest_reserved_arg("zzz", c("alloc", "cost"), prefix = TRUE)
  )
})
