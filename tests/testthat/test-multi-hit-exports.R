## Exports and their preflight for stages that can hit a unit more than once
##
## variance_estimators() must agree with what as_svydesign() and
## as_svrepdesign() do. Each test checks the preflight against the export
## itself, never against another preflight route.

multi_hit_draw <- function(design, method, ...) {
  if (identical(method, "srswr")) {
    draw(design, method = method, ...)
  } else {
    draw(design, method = method, mos = mos, ...)
  }
}

multi_hit <- c("srswr", "pps_multinomial", "pps_chromy", "pps_systematic_mr")

warning_classes <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) e),
    warning = function(w) {
      seen <<- c(seen, class(w)[[1]])
      invokeRestart("muffleWarning")
    }
  )
  list(warnings = seen, error = if (inherits(value, "error")) class(value)[[1]])
}

# The class column joins a row's classes into one string.
estimator_classes <- function(rows, estimator) {
  strsplit(rows$class[rows$estimator == estimator], ", ", fixed = TRUE)[[1]]
}

## A take of N or more is no census with replacement

test_that("a multi-hit stage taking N or more is not read as a census", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:8,
    st = rep(c("a", "b"), each = 4),
    mos = c(1, 2, 3, 4, 1, 2, 3, 4),
    y = c(3, 1, 4, 1, 5, 9, 2, 6)
  )
  for (method in multi_hit) {
    for (size in list(list(n = 5), list(n = 4), list(frac = 1.5))) {
      design <- do.call(
        multi_hit_draw,
        c(list(sampling_design() |> stratify_by(st), method), size)
      )
      rows <- variance_estimators(design, frame)
      jkn <- rows$status[rows$estimator == "Stratified jackknife (JKn)"]
      expect_false(identical(jkn, "refused"), info = method)
      s <- execute(design, frame, seed = 1)
      out <- warning_classes(
        as_svrepdesign(s, type = "JKn", systematic_variance = "approximate")
      )
      expect_null(out$error)
    }
  }

  # Without replacement, a take of N is a census, and both refuse.
  census <- sampling_design() |> stratify_by(st) |> draw(n = 4)
  rows <- variance_estimators(census, frame)
  expect_identical(
    rows$status[rows$estimator == "Stratified jackknife (JKn)"], "refused"
  )
  expect_error(
    as_svrepdesign(execute(census, frame, seed = 1), type = "JKn"),
    class = "samplyr_error_svrep_conversion_failed"
  )
})

test_that("a later multi-hit stage taking N or more still adds variance", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  frame <- data.frame(
    psu = rep(1:6, each = 3),
    hh = 1:18,
    mos = rep(c(1, 2, 4), 6),
    y = c(2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9, 0, 4, 5, 2, 3)
  )
  for (method in multi_hit) {
    for (size in list(list(n = 4), list(frac = 1.5))) {
      design <- sampling_design() |>
        add_stage() |>
        cluster_by(psu) |>
        draw(n = 3) |>
        add_stage()
      design <- do.call(multi_hit_draw, c(list(design, method), size))
      rows <- variance_estimators(design, frame)
      s <- execute(design, frame, seed = 1)
      for (type in c("JK1", "bootstrap")) {
        estimator <- if (identical(type, "JK1")) {
          "Jackknife (JK1)"
        } else {
          "Bootstrap"
        }
        expected <- estimator_classes(rows, estimator)
        expect_true(
          "samplyr_warning_replicate_first_stage_fpc" %in% expected,
          info = paste(method, type)
        )
        out <- warning_classes(
          as_svrepdesign(s, type = type, systematic_variance = "approximate")
        )
        expect_null(out$error)
        expect_true("samplyr_warning_replicate_first_stage_fpc" %in% out$warnings)
      }
    }
  }
})

## Two phases linked on declared identifiers

test_that("a multi-hit phase 1 links to phase 2 on its declared units", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:20, mos = rep(1, 20), y = (1:20)^1.5)
  phase2 <- sampling_design() |> draw(n = 4)
  for (method in multi_hit) {
    design1 <- multi_hit_draw(
      sampling_design() |> cluster_by(id), method, n = 8
    )
    seed <- Find(function(seed) {
      !anyDuplicated(execute(design1, frame, seed = seed)$id)
    }, 1:50)
    phase1 <- execute(design1, frame, seed = seed)
    expect_false(anyDuplicated(phase1$id) > 0)

    expect_no_warning(validate_frame(phase2, phase1))
    rows <- variance_estimators(phase2, phase1)
    expect_false(identical(rows$status[[1]], "refused"), info = method)

    s <- execute(phase2, phase1, seed = 1)
    svy <- suppressWarnings(
      as_svydesign(s, systematic_variance = "approximate")
    )
    expect_s3_class(svy, "twophase2")
    expect_equal(as.vector(weights(svy)), s$.weight)
    expect_equal(
      as.numeric(coef(survey::svytotal(~y, svy))), sum(s$.weight * s$y)
    )
  }
})

# Each phase-2 row must sit on the phase-1 row it was drawn from: the
# exported phase-1 data keep phase 1's row order, so row k is phase-1 row k.
expect_rows_linked <- function(svy, s, phase1, info = NULL) {
  v <- svy$phase1$full$variables[seq_len(nrow(phase1)), ]
  rows <- s$.phase1_row
  expect_identical(v$.phase2, seq_len(nrow(phase1)) %in% rows, info = info)
  expect_equal(v$.weight_phase2[rows], s$.weight, info = info)
  expect_identical(v$.draw_1[rows], phase1$.draw_1[rows], info = info)
  expect_identical(v$id[rows], s$id, info = info)
}

test_that("a phase 2 taking every row of a repeated unit links each hit", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:20, mos = c(30, rep(1, 19)), y = 1:20)
  phase2 <- sampling_design() |> draw(n = 8)
  checked <- character(0)
  for (method in multi_hit) {
    for (clustered in c(TRUE, FALSE)) {
      design1 <- sampling_design()
      if (clustered) {
        design1 <- cluster_by(design1, id)
      }
      design1 <- multi_hit_draw(design1, method, n = 8)
      phase1 <- execute(design1, frame, seed = 3)
      # Phase 2 takes all of phase 1's rows, so it reaches a repeated unit.
      expect_true(anyDuplicated(phase1$id) > 0)
      expect_no_warning(validate_frame(phase2, phase1))
      rows <- variance_estimators(phase2, phase1)
      expect_false(
        "samplyr_error_twophase_bridge" %in%
          estimator_classes(rows, rows$estimator[[1]])
      )
      s <- execute(phase2, phase1, seed = 1)
      expect_setequal(s$.phase1_row, seq_len(nrow(phase1)))
      svy <- suppressWarnings(as_svydesign(
        s, method = "approx", systematic_variance = "approximate"
      ))
      expect_rows_linked(svy, s, phase1, info = method)
      checked <- c(checked, paste(method, clustered))
    }
  }
  expect_setequal(checked, paste(rep(multi_hit, each = 2), c(TRUE, FALSE)))
})

test_that("a phase 2 that reaches a repeated unit exports like one that misses it", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:20, mos = c(8, rep(1, 19)), y = 1:20)
  phase2 <- sampling_design() |> draw(n = 2)
  outcomes <- character(0)
  for (method in multi_hit) {
    design1 <- multi_hit_draw(cluster_by(sampling_design(), id), method, n = 8)
    phase1 <- execute(design1, frame, seed = 3)
    expect_true(anyDuplicated(phase1$id) > 0)
    rows <- variance_estimators(phase2, phase1)
    linearization <- rows[rows$estimator == "Taylor linearization", ]
    expect_false(linearization$status == "refused")
    expect_false(grepl(
      "samplyr_error_twophase_bridge", linearization$sample_risk, fixed = TRUE
    ))
    for (seed in 1:60) {
      s <- execute(phase2, phase1, seed = seed)
      reached <- any(s$id %in% phase1$id[duplicated(phase1$id)])
      svy <- suppressWarnings(as_svydesign(
        s, method = "approx", systematic_variance = "approximate"
      ))
      # One phase-1 cluster per hit, so every hit is its own draw.
      expect_identical(
        length(unique(svy$phase1$full$cluster[[1]])), nrow(phase1)
      )
      expect_rows_linked(svy, s, phase1, info = paste(method, seed))
      outcomes <- c(outcomes, paste(method, if (reached) "reached" else "missed"))
    }
  }
  expect_setequal(
    outcomes, paste(rep(multi_hit, each = 2), c("reached", "missed"))
  )
})

## A user pps argument needs a PPS stage 1 drawn without replacement

test_that("pps specifications are refused where stage 1 cannot take them", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    psu = rep(1:6, each = 4),
    id = 1:24,
    mos = rep(c(1, 2, 3, 9), 6),
    y = 1:24
  )
  specs <- function(s) {
    list(overton = "overton", hr = survey::HR(),
         ppsmat = survey::ppsmat(diag(length(unique(s$id)))))
  }
  single <- lapply(stats::setNames(nm = multi_hit), function(method) {
    multi_hit_draw(sampling_design(), method, n = 20)
  })
  single$srswor <- sampling_design() |> draw(n = 20)
  # At this take each unit of size 9 expects two hits.
  repeated <- character(0)
  for (name in names(single)) {
    s <- execute(single[[name]], frame, seed = 2)
    if (anyDuplicated(s$id) > 0) repeated <- c(repeated, name)
    for (spec in specs(s)) {
      expect_error(
        as_svydesign(s, pps = spec, systematic_variance = "approximate"),
        class = "samplyr_error_pps_argument"
      )
    }
  }
  expect_setequal(repeated, multi_hit)

  # A PPS stage below an equal-probability stage 1 does not justify them.
  two_stage <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage() |>
    draw(n = 2, method = "pps_brewer", mos = mos)
  s <- suppressMessages(execute(two_stage, frame, seed = 1))
  expect_error(as_svydesign(s, pps = "overton"), class = "samplyr_error_pps_argument")
  expect_error(as_svydesign(s, pps = survey::HR()), class = "samplyr_error_pps_argument")
})

test_that("the joint row names expected hits for a PPS stage with hits", {
  frame <- data.frame(id = 1:12, mos = c(9, rep(1, 11)), y = 1:12)
  for (method in c("pps_multinomial", "pps_chromy", "pps_systematic_mr")) {
    rows <- variance_estimators(
      sampling_design() |> draw(n = 4, method = method, mos = mos), frame
    )
    note <- rows$note[grepl("joint", rows$call)]
    expect_identical(rows$status[grepl("joint", rows$call)], "not applicable")
    expect_match(note, "joint expected hits")
    expect_no_match(note, "not drawn with unequal probabilities")
  }
  rows <- variance_estimators(sampling_design() |> draw(n = 4), frame)
  expect_match(
    rows$note[grepl("joint", rows$call)], "not drawn with unequal probabilities"
  )
})

## Refusals come before approximation advice

test_that("a refused export gives no approximation warning first", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:20, mos = 1:20, y = 1:20)

  # Two-phase export refuses a PPS phase 2 drawn with pps_systematic_mr.
  phase1 <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 12) |>
    execute(frame, seed = 1)
  s <- sampling_design() |>
    draw(n = 5, method = "pps_systematic_mr", mos = mos) |>
    execute(phase1, seed = 2)
  out <- warning_classes(as_svydesign(s))
  expect_identical(out$error, "samplyr_error_twophase_phase2_pps")
  expect_false("samplyr_warning_systematic_variance" %in% out$warnings)
  # With both a refused method and a refusing policy, the method wins.
  out <- warning_classes(as_svydesign(s, systematic_variance = "error"))
  expect_identical(out$error, "samplyr_error_twophase_phase2_pps")

  # A refusing policy comes before the with-replacement warning.
  s <- sampling_design() |>
    draw(n = 5, method = "pps_systematic_mr", mos = mos) |>
    execute(frame, seed = 2)
  out <- warning_classes(
    as_svrepdesign(s, type = "bootstrap", systematic_variance = "error")
  )
  expect_identical(out$error, "samplyr_error_systematic_variance")
  expect_length(out$warnings, 0)
  # Accepted, the same export still gives both warnings, once each.
  out <- warning_classes(as_svrepdesign(s, type = "bootstrap"))
  expect_null(out$error)
  expect_setequal(
    out$warnings,
    c("samplyr_warning_systematic_variance",
      "samplyr_warning_replicate_wr_first_stage")
  )
  expect_length(out$warnings, 2)
})

## Every replicate type reads minimum replacement as with replacement

test_that("the rescaled bootstraps flag a first stage read with replacement", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  frame <- data.frame(
    id = 1:30,
    st = rep(c("a", "b"), 15),
    mos = c(9, rep(1, 29)),
    y = 1:30
  )
  types <- c(
    subbootstrap = "Rescaled bootstrap",
    mrbbootstrap = "Multistage rescaled bootstrap"
  )
  for (method in c(multi_hit, "pps_brewer")) {
    design <- multi_hit_draw(
      sampling_design() |> stratify_by(st), method, n = 6
    )
    rows <- variance_estimators(design, frame)
    s <- suppressMessages(execute(design, frame, seed = 1))
    pmr <- method %in% c("pps_chromy", "pps_systematic_mr")
    for (type in names(types)) {
      out <- warning_classes(
        as_svrepdesign(s, type = type, systematic_variance = "approximate")
      )
      status <- rows$status[rows$estimator == types[[type]]]
      # mrbbootstrap reads one fraction per stratum, which PPS without
      # replacement does not have.
      if (method == "pps_brewer" && type == "mrbbootstrap") {
        expect_identical(out$error, "samplyr_error_svrep_conversion_failed")
        expect_identical(status, "refused")
        next
      }
      read_wr <- pmr || method == "pps_brewer"
      flagged <- "samplyr_warning_replicate_wr_first_stage" %in%
        estimator_classes(rows, types[[type]])
      expect_null(out$error)
      expect_identical(flagged, read_wr, info = paste(method, type))
      expect_identical(
        "samplyr_warning_replicate_wr_first_stage" %in% out$warnings, read_wr,
        info = paste(method, type)
      )
    }
  }
})

test_that("a replicate conversion that fails gives no advice first", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:30,
    st = rep(c("a", "b"), 15),
    mos = c(9, rep(1, 29)),
    y = 1:30
  )
  for (method in c("pps_chromy", "pps_brewer")) {
    s <- sampling_design() |>
      stratify_by(st) |>
      draw(n = 6, method = method, mos = mos) |>
      execute(frame, seed = 1) |>
      suppressMessages()
    out <- warning_classes(as_svrepdesign(s, type = "JK1"))
    expect_identical(out$error, "samplyr_error_svrep_conversion_failed")
    expect_length(out$warnings, 0)
  }
})

## A phase 2 drawn within each hit of phase 1

within_hit_frame <- function() {
  data.frame(
    village = rep(sprintf("v%02d", 1:10), each = 8),
    hh = rep(1:8, 10),
    mos = rep(c(40, 30, 6, 5, 5, 4, 4, 3, 2, 1), each = 8),
    y = (1:80) %% 9 + rep(1:10, each = 8)
  )
}

within_hit_phase1 <- function() {
  sampling_design() |>
    add_stage() |> cluster_by(village) |>
    draw(n = 5, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 4)
}

test_that("phase 2 carries the hit each row was drawn under", {
  phase1 <- execute(within_hit_phase1(), within_hit_frame(), seed = 1)
  expect_true(anyDuplicated(unique(phase1[c("village", ".draw_1")])$village) > 0)
  phase2 <- execute(sampling_design() |> draw(n = 10), phase1, seed = 2)
  expect_identical(phase2$.phase1_draw_1, phase1$.draw_1[phase2$.phase1_row])
  # A third phase drawn from phase 2 has no hits of phase 2 to carry.
  phase3 <- execute(sampling_design() |> draw(n = 4), phase2, seed = 3)
  expect_false(any(grepl("draw", names(phase3))))
  frame <- within_hit_frame()
  frame$.phase1_draw_1 <- 1L
  expect_error(execute(within_hit_phase1(), frame, seed = 1),
               class = "samplyr_error_frame_reserved_names")
})

test_that("a subsample within each hit exports as phase 1's design", {
  skip_if_not_installed("survey")
  phase1 <- execute(within_hit_phase1(), within_hit_frame(), seed = 1)
  within <- sampling_design() |>
    stratify_by(village, .phase1_draw_1) |> draw(n = 2)
  expect_no_warning(validate_frame(within, phase1))
  checked <- 0L
  for (seed in 1:4) {
    phase2 <- execute(within, phase1, seed = seed)
    svy <- as_svydesign(phase2)
    expect_false(inherits(svy, "twophase2"))
    # The same design built with survey: hits drawn with replacement, then
    # 2 of each village's 8 households.
    df <- as.data.frame(phase2)
    df$hit <- paste(df$village, df$.phase1_draw_1)
    df$row <- seq_len(nrow(df))
    df$f1 <- Inf
    df$f2 <- 8
    want <- survey::svydesign(ids = ~hit + row, fpc = ~f1 + f2,
                              weights = ~.weight, data = df)
    got_total <- survey::svytotal(~y, svy)
    want_total <- survey::svytotal(~y, want)
    expect_equal(as.numeric(coef(got_total)), as.numeric(coef(want_total)),
                 tolerance = 1e-12)
    expect_equal(as.numeric(survey::SE(got_total)),
                 as.numeric(survey::SE(want_total)), tolerance = 1e-10)
    checked <- checked + 1L
  }
  expect_identical(checked, 4L)
})

test_that("a subsample within each village, across its hits, stays two-phase", {
  skip_if_not_installed("survey")
  phase1 <- execute(within_hit_phase1(), within_hit_frame(), seed = 1)
  by_village <- sampling_design() |> stratify_by(village) |> draw(n = 2)
  phase2 <- execute(by_village, phase1, seed = 2)
  raised <- list()
  withCallingHandlers(
    as_svydesign(phase2, method = "approx"),
    warning = function(w) {
      raised[[class(w)[1]]] <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_setequal(names(raised), c("samplyr_warning_twophase_across_units",
                                   "samplyr_warning_twophase_approx"))
  # A village hit twice spans two units, so the advice names the hit.
  expect_match(raised$samplyr_warning_twophase_across_units,
               "stratify_by(village, .phase1_draw_1)", fixed = TRUE)
})

## A phase 1 drawn with replacement, then subsampled
#
# A subsample of independent draws is itself a sample of independent draws,
# so phase 2's rows form a with-replacement sample of phase 2's size and its
# textbook variance estimator is the exact target (closed form).

test_that("a with-replacement phase 1 keeps its variance in a two-phase export", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:30, y = (1:30 * 7) %% 19 + 1)
  phase1 <- sampling_design() |>
    draw(n = 30, method = "srswr") |>
    execute(frame, seed = 1)
  phase2 <- sampling_design() |>
    draw(n = 15) |>
    execute(phase1, seed = 2)
  svy <- as_svydesign(phase2)
  v <- stats::vcov(survey::svytotal(~y, svy))[1, 1]
  expect_equal(v, 30^2 * stats::var(phase2$y) / 15, tolerance = 1e-10)
})

test_that("a PPS phase 1 with hits gives the Hansen-Hurwitz variance", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:25,
    y = (1:25 * 7) %% 19 + 1,
    size = c(40, 40, 40, (4:25 * 3) %% 10 + 1)
  )
  # Expected hits above one, where reading them as a probability failed.
  expect_gt(max(12 * frame$size / sum(frame$size)), 1)
  phase1 <- sampling_design() |>
    draw(n = 12, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 3)
  expect_gt(anyDuplicated(phase1$id), 0)
  phase2 <- sampling_design() |>
    draw(n = 6) |>
    execute(phase1, seed = 4)
  svy <- as_svydesign(phase2)
  check <- as.matrix(svy$dcheck$phase1)
  expect_equal(check, t(check), tolerance = 0)
  z <- phase2$y / (frame$size[phase2$id] / sum(frame$size))
  v <- stats::vcov(survey::svytotal(~y, svy))[1, 1]
  expect_equal(v, sum((z - mean(z))^2) / (6 * 5), tolerance = 1e-10)
})

test_that("a stratified with-replacement phase 1 exports in two phases", {
  skip_if_not_installed("survey")
  frame <- data.frame(
    id = 1:40,
    y = (1:40 * 7) %% 19 + 1,
    region = rep(c("a", "b"), 20)
  )
  phase1 <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10, method = "srswr") |>
    execute(frame, seed = 1)
  expect_true(anyDuplicated(phase1$.draw_1) > 0)
  phase2 <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 5) |>
    execute(phase1, seed = 2)
  for (method in c("full", "approx", "simple")) {
    seen <- warning_classes(as_svydesign(phase2, method = method))
    expect_null(seen$error)
    expect_false("samplyr_warning_twophase_across_units" %in% seen$warnings)
  }
  v <- stats::vcov(survey::svytotal(~y, as_svydesign(phase2)))[1, 1]
  closed <- sum(tapply(phase2$y, phase2$region, function(y) {
    20^2 * stats::var(y) / 5
  }))
  expect_equal(v, closed, tolerance = 1e-10)

  across <- sampling_design() |>
    draw(n = 10) |>
    execute(phase1, seed = 2)
  seen <- warning_classes(as_svydesign(across))
  expect_null(seen$error)
  expect_identical(seen$warnings, character(0))
  rows <- variance_estimators(across)
  expect_identical(
    rows$status[rows$estimator == "Taylor linearization"], "supported"
  )
})

test_that("an occasion of a with-replacement master pairs its hits by stratum", {
  skip_if_not_installed("survey")
  rotation <- expand.grid(panel = 1:2, period = 1:3)
  rotation$active <- ((rotation$period - rotation$panel) %% 2) == 0
  frame <- data.frame(
    id = 1:40,
    y = (1:40 * 7) %% 19 + 1,
    region = rep(c("a", "b"), 20)
  )
  master <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 20, method = "srswr") |>
    execute(frame, seed = 11, panels = rotation)
  occasion <- execute(master, period = 2)
  svy <- as_svydesign(occasion, method = "full")
  # The check matrix of independent draws in each stratum (closed form).
  region <- svy$phase1$sample$variables$region
  expected <- outer(region, region, "==") * (-1 / (20 - 1))
  diag(expected) <- 1
  expect_equal(
    unname(as.matrix(svy$dcheck$phase1)), expected, tolerance = 1e-12
  )
})
