## Certainty units in stages that can hit a unit more than once
##
## A unit at or above `certainty_size`, or whose share reaches
## `certainty_prop`, is taken once with weight 1. The rest of the take is
## drawn by the method over the remaining sizes, so a remaining unit can
## still be hit more than once.

wr_cert_methods <- c("pps_multinomial", "pps_chromy", "pps_systematic_mr")

# Unit 1 is certain under `certainty_size = 60`. The other four hits fall on
# sizes summing to 100, so their expected hits are 1.6, 1.2, 0.6, 0.4, 0.2.
wr_cert_frame <- function() {
  data.frame(psu = 1:6, mos = c(100, 40, 30, 15, 10, 5), y = c(9, 4, 6, 2, 3, 1))
}

# Under `certainty_prop = 0.4`, units 1, 2 and 3 qualify in turn (100 / 240,
# 60 / 140, 35 / 80), and 15 / 45 stops the cascade.
cascade_frame <- function() {
  data.frame(psu = 1:8, mos = c(100, 60, 35, 15, 10, 8, 7, 5), y = 1:8)
}

wr_cert_design <- function(method, ...) {
  sampling_design() |>
    cluster_by(psu) |>
    draw(n = 5, method = method, mos = mos, ...)
}

# Exact first and second moments of the hits of a systematic draw with
# minimum replacement, from the definition: integrate over the random start.
# Hits change only where a selection point crosses a cumulated size.
systematic_mr_moments <- function(sizes, n) {
  cum <- c(0, cumsum(sizes))
  step <- sum(sizes) / n
  cuts <- unlist(lapply(0:(n - 1), function(k) cum - k * step))
  cuts <- sort(unique(c(0, step, cuts[cuts > 0 & cuts < step])))
  e <- numeric(length(sizes))
  j <- matrix(0, length(sizes), length(sizes))
  for (k in seq_len(length(cuts) - 1L)) {
    start <- (cuts[k] + cuts[k + 1L]) / 2
    share <- (cuts[k + 1L] - cuts[k]) / step
    hits <- tabulate(
      findInterval(start + (0:(n - 1)) * step, cum), length(sizes)
    )
    e <- e + share * hits
    j <- j + share * outer(hits, hits)
  }
  list(e = e, j = j)
}

# Moments of the whole pool, certainty units first in `cert`.
with_certainty_moments <- function(n_cert, rest) {
  e <- c(rep(1, n_cert), rest$e)
  j <- outer(e, e)
  rest_pos <- n_cert + seq_along(rest$e)
  j[rest_pos, rest_pos] <- rest$j
  list(e = e, j = j)
}

## Selection

test_that("certainty units are taken once, count in n, and weigh 1", {
  frame <- wr_cert_frame()
  # Unit 1 holds 100 / 200, unit 2 then 40 / 100.
  rules <- list(size = list(certainty_size = 60),
                prop = list(certainty_prop = 0.45))
  checked <- character(0)
  for (method in wr_cert_methods) {
    for (rule in names(rules)) {
      design <- do.call(wr_cert_design, c(list(method), rules[[rule]]))
      for (seed in 1:5) {
        s <- execute(design, frame, seed = seed)
        expect_identical(nrow(s), 5L)
        expect_identical(sort(s$.draw_1), 1:5)
        certain <- s[s$.certainty_1, ]
        expect_identical(certain$psu, 1L)
        expect_identical(certain$.weight, 1)
        rest <- s[!s$.certainty_1, ]
        expect_equal(rest$.weight, 100 / (4 * rest$mos))
      }
      checked <- c(checked, paste(method, rule))
    }
  }
  expect_setequal(
    checked, paste(rep(wr_cert_methods, each = 2), c("size", "prop"))
  )
})

test_that("a remaining unit expecting whole hits is not certain", {
  # Unit 2 expects exactly 2 of the 4 remaining hits, units 3 and 4 one each.
  frame <- data.frame(psu = 1:4, mos = c(100, 50, 25, 25))
  checked <- character(0)
  for (method in wr_cert_methods) {
    design <- wr_cert_design(method, certainty_size = 60)
    s <- execute(design, frame, seed = 1)
    # Four remaining hits fall on three units, before and after the draw.
    expect_identical(nrow(s), 5L)
    expect_equal(frame_summary(design, frame)$n_expected, 5)
    expect_equal(1 / s$.weight[s$psu != 1L],
                 c(2, 1, 1)[s$psu[s$psu != 1L] - 1L])
    expect_identical(s$psu[s$.certainty_1], 1L)
    expect_false(any(s$.certainty_1[s$psu != 1L]))
    if (method != "pps_multinomial") {
      # Minimum replacement fixes whole expected hits.
      expect_identical(sum(s$psu == 2L), 2L)
    }
    checked <- c(checked, method)
  }
  expect_setequal(checked, wr_cert_methods)
})

test_that("certainty_prop is applied again to the remaining shares", {
  for (method in wr_cert_methods) {
    s <- execute(wr_cert_design(method, certainty_prop = 0.4),
                 cascade_frame(), seed = 1)
    expect_setequal(s$psu[s$.certainty_1], 1:3)
    rest <- s[!s$.certainty_1, ]
    expect_equal(rest$.weight, 45 / (2 * rest$mos))
  }
})

test_that("certainty rules refuse what they refuse without replacement", {
  frame <- wr_cert_frame()
  frame$mos[2] <- 90
  every <- data.frame(psu = 1:2, mos = c(70, 90))
  draw_with <- function(method, n, f) {
    execute(
      sampling_design() |> cluster_by(psu) |>
        draw(n = n, method = method, mos = mos, certainty_size = 60),
      f, seed = 1
    )
  }
  for (method in wr_cert_methods) {
    # Two certainty units take the whole target, leaving nothing to draw.
    expect_error(draw_with(method, 2, frame),
                 class = "samplyr_error_certainty_zero_probability")
    # Every unit is certain, and there are more of them than the target.
    expect_error(draw_with(method, 1, every),
                 class = "samplyr_error_certainty_overflow")
  }
})

test_that("a threshold per stratum applies in each stratum", {
  frame <- rbind(
    transform(wr_cert_frame(), st = "a"),
    transform(wr_cert_frame(), st = "b", psu = 7:12)
  )
  thresholds <- data.frame(st = c("a", "b"), certainty_size = c(60, 200))
  s <- execute(
    sampling_design() |> stratify_by(st) |> cluster_by(psu) |>
      draw(n = 5, method = "pps_systematic_mr", mos = mos,
           certainty_size = thresholds),
    frame, seed = 1
  )
  expect_identical(s$psu[s$.certainty_1], 1L)
  # Without a certainty unit, stratum b's largest unit expects 2.5 hits.
  expect_equal(unique(s$.weight[s$psu == 7L]), 200 / (5 * 100))
})

test_that("a registered with-replacement method takes certainty units too", {
  toy_wr <- function(hits, n = NULL, prn = NULL, ...) {
    sample.int(length(hits), size = n, replace = TRUE, prob = hits)
  }
  on.exit(sondage::unregister_method("cert_wr"), add = TRUE)
  sondage::register_method("cert_wr", "wr", sample_fn = toy_wr,
                           probabilities = "exact")
  s <- execute(wr_cert_design("pps_cert_wr", certainty_size = 60),
               wr_cert_frame(), seed = 2)
  expect_identical(nrow(s), 5L)
  expect_identical(s$psu[s$.certainty_1], 1L)
  rest <- s[!s$.certainty_1, ]
  expect_equal(rest$.weight, 100 / (4 * rest$mos))
})

## Chances before and after the draw

test_that("frame_summary() counts certainty units before and after the draw", {
  frame <- wr_cert_frame()
  for (method in wr_cert_methods) {
    design <- wr_cert_design(method, certainty_size = 60)
    before <- frame_summary(design, frame)
    s <- execute(design, frame, seed = 1)
    after <- frame_summary(s)
    expect_identical(before$n_certainty, 1, info = method)
    expect_identical(after$n_certainty, 1, info = method)
    expect_equal(before$n_expected, 5, info = method)
    units <- get_frame_digest(s)$stages[[1]]$units
    expect_identical(units$is_certainty, c(TRUE, rep(FALSE, 5)))
    expect_equal(units$chance, c(1, 1.6, 1.2, 0.6, 0.4, 0.2))
  }
  # Without a rule, expected hits name no certainty unit.
  bare <- frame_summary(wr_cert_design("pps_systematic_mr"), frame)
  expect_true(is.na(bare$n_certainty))
})

test_that("the summary digest counts certainty units from the sample's flags", {
  # Pools of 150 keep their chances in quantile bins, without the units
  # that name the certainty ones. Each stratum has one unit of size 400.
  frame <- data.frame(
    id = 1:300, st = rep(c("a", "b"), each = 150),
    m = rep(c(400, rep(2, 149)), 2)
  )
  for (method in wr_cert_methods) {
    design <- sampling_design() |> stratify_by(st) |>
      draw(n = 4, method = method, mos = m, certainty_size = 100)
    before <- frame_summary(design, frame)$n_certainty
    expect_identical(before, 2, info = method)
    for (digest in c("summary", "full")) {
      s <- execute(design, frame, seed = 1, frame_digest = digest)
      if (digest == "summary") {
        expect_null(get_frame_digest(s)$stages[[1]]$units)
      }
      expect_identical(frame_summary(s)$n_certainty, before,
                       info = paste(method, digest))
    }
    reps <- execute(design, frame, seed = 1, reps = 2)
    expect_identical(frame_summary(reps)$n_certainty, before, info = method)
    # Without a rule a stage drawn with replacement names none.
    bare <- sampling_design() |> stratify_by(st) |>
      draw(n = 4, method = method, mos = m) |>
      execute(frame, seed = 1)
    expect_true(all(is.na(frame_summary(bare)$n_certainty)), info = method)
    # Equal sizes and one draw per unit store one expected hit, which no
    # unit's certainty.
    flat <- sampling_design() |>
      draw(n = 150, method = method, mos = m, certainty_size = 100) |>
      execute(data.frame(id = 1:150, m = 2), seed = 1)
    expect_identical(get_frame_digest(flat)$stages[[1]]$storage, "constant")
    expect_identical(frame_summary(flat)$n_certainty, 0, info = method)
  }
})

## Joint expected hits

test_that("the systematic oracle reproduces cases known in closed form", {
  # Two equal units and three hits: one unit gets 1, the other 2.
  two <- systematic_mr_moments(c(1, 1), 3)
  expect_equal(two$e, c(1.5, 1.5))
  expect_equal(two$j, matrix(c(2.5, 2, 2, 2.5), 2))
  # Whole expected hits are fixed, so the second moments are products.
  whole <- systematic_mr_moments(c(2, 1, 1), 4)
  expect_equal(whole$j, outer(c(2, 1, 1), c(2, 1, 1)))
})

test_that("joint expected hits match the definition for minimum replacement", {
  frame <- wr_cert_frame()
  cases <- list(
    size = list(rule = list(certainty_size = 60), n_cert = 1L,
                rest = systematic_mr_moments(frame$mos[-1], 4)),
    prop = list(rule = list(certainty_prop = 0.4), n_cert = 3L,
                frame = cascade_frame(),
                rest = systematic_mr_moments(cascade_frame()$mos[-(1:3)], 2))
  )
  checked <- character(0)
  for (label in names(cases)) {
    case <- cases[[label]]
    want <- with_certainty_moments(case$n_cert, case$rest)
    design <- do.call(wr_cert_design, c(list("pps_systematic_mr"), case$rule))
    pool <- case$frame %||% frame
    for (seed in 1:6) {
      s <- execute(design, pool, seed = seed)
      ids <- unique(s$psu)
      expect_equal(1 / s$.weight, want$e[s$psu], tolerance = 1e-12)
      for (route in c("frame", "digest")) {
        j <- if (route == "frame") {
          joint_expectation(s, pool)[[1]]
        } else {
          joint_expectation(s)[[1]]
        }
        expect_equal(unname(j), want$j[ids, ids], tolerance = 1e-12,
                     info = paste(label, seed, route))
        checked <- c(checked, paste(label, route))
      }
    }
  }
  expect_setequal(
    checked, paste(rep(c("size", "prop"), each = 2), c("frame", "digest"))
  )
})

test_that("joint expected hits match the multinomial closed form", {
  frame <- wr_cert_frame()
  # Four multinomial hits over the remaining sizes: E(n_i n_j) is
  # m (m - 1) p_i p_j off the diagonal, plus m p_i on it.
  p <- frame$mos[-1] / 100
  rest <- list(e = 4 * p, j = 4 * 3 * outer(p, p) + diag(4 * p))
  want <- with_certainty_moments(1L, rest)
  design <- wr_cert_design("pps_multinomial", certainty_size = 60)
  for (seed in 1:6) {
    s <- execute(design, frame, seed = seed)
    ids <- unique(s$psu)
    expect_equal(unname(joint_expectation(s, frame)[[1]]), want$j[ids, ids],
                 tolerance = 1e-12, info = seed)
    expect_equal(unname(joint_expectation(s)[[1]]), want$j[ids, ids],
                 tolerance = 1e-12, info = seed)
  }
})

test_that("joint matrices with certainty units are valid second moments", {
  # Two strata, so the blocks of different pools meet.
  frame <- rbind(
    transform(wr_cert_frame(), st = "a"),
    transform(wr_cert_frame(), st = "b", psu = 7:12)
  )
  checked <- character(0)
  for (method in wr_cert_methods) {
    design <- sampling_design() |> stratify_by(st) |> cluster_by(psu) |>
      draw(n = 5, method = method, mos = mos, certainty_size = 60)
    s <- execute(design, frame, seed = 3)
    j <- joint_expectation(s, frame)[[1]]
    ids <- unique(s$psu)
    e <- 1 / s$.weight[match(ids, s$psu)]
    certain <- ids %in% c(1L, 7L)
    stratum <- ifelse(ids <= 6L, "a", "b")
    expect_equal(j, t(j))
    expect_true(all(j^2 <= outer(diag(j), diag(j)) * (1 + 1e-12)))
    expect_true(all(diag(j) >= e^2 - 1e-12))
    expect_equal(e[certain], c(1, 1))
    expect_equal(unname(j[certain, certain]), matrix(1, 2, 2))
    expect_equal(unname(j[certain, !certain]),
                 outer(rep(1, 2), e[!certain]))
    across <- outer(stratum, stratum, "!=")
    expect_equal(unname(j[across]), outer(e, e)[across])
    checked <- c(checked, method)
  }
  expect_setequal(checked, wr_cert_methods)
})

## Export

test_that("the export takes certainty units whole and draws the rest with replacement", {
  skip_if_not_installed("survey")
  frame <- wr_cert_frame()
  design <- sampling_design() |> cluster_by(psu) |>
    draw(n = 5, method = "pps_multinomial", mos = mos, certainty_size = 60)
  checked <- 0L
  for (seed in 1:8) {
    s <- execute(design, frame, seed = seed)
    rest <- s[!s$.certainty_1, ]
    if (length(unique(rest$psu)) < 2L) next
    total <- survey::svytotal(~y, as_svydesign(s))
    # Hansen-Hurwitz on the four remaining hits, by hand: z = y / p.
    z <- rest$y / (rest$mos / 100)
    expect_equal(as.numeric(coef(total)), 9 + mean(z), tolerance = 1e-12)
    expect_equal(as.numeric(survey::SE(total))^2,
                 stats::var(z) / length(z), tolerance = 1e-10)
    checked <- checked + 1L
  }
  expect_gt(checked, 3L)
})

test_that("a design file keeps the certainty rule", {
  frame <- wr_cert_frame()
  design <- wr_cert_design("pps_chromy", certainty_size = 60)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(design, path)
  again <- read_design(path)
  expect_identical(
    as.data.frame(execute(again, frame, seed = 4))[c("psu", ".weight",
                                                     ".certainty_1")],
    as.data.frame(execute(design, frame, seed = 4))[c("psu", ".weight",
                                                      ".certainty_1")]
  )
})
