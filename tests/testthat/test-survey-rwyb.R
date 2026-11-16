skip_if_not_installed("svrep", "0.9.1")
skip_if_not_installed("survey")

rwyb_variance <- function(x, seed = 42, replicates = 20000) {
  r <- withr::with_seed(seed, as_svrepdesign(x, type = "rwyb", replicates = replicates))
  as.numeric(vcov(survey::svytotal(~y, r)))
}

test_that("RWYB recovers the Poisson variance lost by fixed-size replication", {
  frame <- data.frame(psu = rep(1:4, each = 40), id = 1:160, y = 1)
  sample <- sampling_design() |> cluster_by(psu) |> draw(n = 4) |> add_stage() |>
    draw(frac = .5, method = "bernoulli") |> execute(frame, seed = 41)
  expect_equal(nrow(sample), 87L)
  expect_error(as_svydesign(sample), class = "samplyr_error_multistage_poisson_later")
  for (type in c("auto", "bootstrap", "subbootstrap", "mrbbootstrap", "JK1")) {
    expect_error(as_svrepdesign(sample, type = type), class = "samplyr_error_poisson_replicates")
  }
  expect_equal(rwyb_variance(sample), 174, tolerance = .05)

  frame <- data.frame(psu = rep(1:20, each = 40), y = 1)
  sample <- sampling_design() |> cluster_by(psu) |> draw(frac = .5, method = "bernoulli") |>
    execute(frame, seed = 5)
  expect_equal(rwyb_variance(sample), 32000, tolerance = .05)
})

test_that("mixed SRS and Poisson stages reproduce the recursive variance estimator", {
  frame <- data.frame(psu = rep(1:8, each = 16), id = 1:128, y = (1:128)^.8)
  sample <- sampling_design() |> cluster_by(psu) |> draw(n = 5) |> add_stage() |>
    draw(frac = .5, method = "bernoulli") |> execute(frame, seed = 31)
  totals <- tapply(sample$y * sample$.weight_2, sample$psu, sum)
  conditional <- sum((1 - 1 / sample$.weight_2) * (sample$y * sample$.weight_2)^2)
  target <- 8^2 * (1 - 5/8) / 5 * var(totals) + 8/5 * conditional
  expect_equal(rwyb_variance(sample), target, tolerance = .05)
})

test_that("Poisson certainties have exactly factor one", {
  frame <- data.frame(id = 1:20, mos = c(20, rep(1, 19)), y = 1:20)
  sample <- suppressWarnings(sampling_design() |> draw(n = 8, method = "pps_poisson", mos = mos) |>
    execute(frame, seed = 1))
  r <- withr::with_seed(42, as_svrepdesign(sample, type = "rwyb", replicates = 20000))
  w <- stats::weights(r, type = "analysis")
  expect_true(all(w[sample$.weight == 1, ] == 1))
  target <- sum((1 - 1 / sample$.weight) * (sample$y * sample$.weight)^2)
  expect_equal(as.numeric(vcov(survey::svytotal(~y, r))), target, tolerance = .05)
  census <- sampling_design() |> draw(frac = 1, method = "bernoulli") |> execute(frame)
  expect_equal(rwyb_variance(census, replicates = 20), 0)
})

test_that("SRS and WR multistage designs retain their variance and draw occurrences", {
  frame <- data.frame(psu = rep(1:8, each = 12), id = 1:96,
    mos = rep(1:8, each = 12), y = (1:96)^.6)
  for (method in c("srswor", "srswr", "pps_multinomial")) {
    design <- sampling_design() |> cluster_by(psu)
    design <- if (method == "pps_multinomial") draw(design, n = 12, method = method, mos = mos) else draw(design, n = 5, method = method)
    sample <- design |> add_stage() |> draw(n = 6) |> execute(frame, seed = 3)
    target <- as.numeric(vcov(survey::svytotal(~y, as_svydesign(sample))))
    expect_equal(rwyb_variance(sample), target, tolerance = .05, info = method)
  }
})

test_that("WR certainty units keep factor one and their lower-stage variance", {
  # Oracle: Hansen-Hurwitz variance of the remaining draws, by hand.
  hh <- function(s, stage_mos, total_mos) {
    rest <- s[!s$.certainty_1, ]
    p <- rest[[stage_mos]] / total_mos
    stats::var(rest$y / p) / nrow(rest)
  }
  frame <- data.frame(id = 1:6, mos = c(100, 40, 30, 15, 10, 5),
    y = c(1000, 4, 6, 2, 3, 1))
  s <- sampling_design() |> cluster_by(id) |>
    draw(n = 5, method = "pps_multinomial", mos = mos, certainty_size = 60) |>
    execute(frame, seed = 1)
  expect_equal(sum(s$.certainty_1), 1L)
  r <- withr::with_seed(42, as_svrepdesign(s, type = "rwyb", replicates = 20000))
  expect_true(all(stats::weights(r, type = "analysis")[s$.certainty_1, ] == 1))
  expect_equal(as.numeric(vcov(survey::svytotal(~y, r))), hh(s, "mos", 100),
    tolerance = .05)

  strata <- rbind(transform(frame, st = 1), transform(frame, st = 2, id = id + 6))
  s <- sampling_design() |> stratify_by(st) |> cluster_by(id) |>
    draw(n = 5, method = "pps_multinomial", mos = mos, certainty_size = 60) |>
    execute(strata, seed = 1)
  target <- sum(vapply(split(s, s$st), hh, 0, stage_mos = "mos", total_mos = 100))
  expect_equal(rwyb_variance(s), target, tolerance = .05)

  # Every unit certain: the total is fixed.
  frame <- data.frame(id = 1:4, mos = c(10, 20, 30, 40), y = c(2, 10, 3, 9))
  s <- sampling_design() |> cluster_by(id) |>
    draw(n = 4, method = "pps_multinomial", mos = mos, certainty_size = 1) |>
    execute(frame, seed = 1)
  expect_true(all(s$.certainty_1))
  expect_equal(rwyb_variance(s, replicates = 50), 0)

  # A certain parent brings in its own SRS stage; the drawn parents give the
  # with-replacement variance of their estimated totals.
  frame <- expand.grid(hh = 1:10, psu = 1:4)
  frame$mos <- c(100, 10, 10, 10)[frame$psu]
  frame$y <- frame$hh + 10 * frame$psu
  s <- sampling_design() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = mos, certainty_size = 50) |>
    add_stage() |> draw(n = 4) |> execute(frame, seed = 3)
  certain <- s[s$.certainty_1, ]
  expect_equal(unique(certain$psu), 1)
  drawn <- s[!s$.certainty_1, ]
  z <- tapply(drawn$y * 10 / 4, drawn$.draw_1, sum) / (10 / 30)
  expect_length(z, 2L)
  target <- stats::var(z) / 2 + 10^2 * (1 - 4 / 10) * stats::var(certain$y) / 4
  expect_equal(rwyb_variance(s), target, tolerance = .05)

  # The sole parent, taken with certainty, is not a singleton.
  frame <- data.frame(psu = 1, mos = 100, hh = 1:10, y = 1:10)
  s <- sampling_design() |> cluster_by(psu) |>
    draw(n = 1, method = "pps_multinomial", mos = mos, certainty_size = 1) |>
    add_stage() |> draw(n = 4) |> execute(frame, seed = 1)
  expect_equal(rwyb_variance(s), 10^2 * (1 - 4 / 10) * stats::var(s$y) / 4,
    tolerance = .05)
})

test_that("RWYB respects local IDs and later-stage strata", {
  frame <- expand.grid(id = 1:8, h = 1:2, psu = 1:6)
  frame$y <- with(frame, id + h^2 + psu^2)
  sample <- sampling_design() |> cluster_by(psu) |> draw(n = 4) |> add_stage() |>
    stratify_by(h) |> cluster_by(h, id) |> draw(n = 4) |> execute(frame, seed = 3)
  target <- as.numeric(vcov(survey::svytotal(~y, as_svydesign(sample))))
  expect_equal(rwyb_variance(sample), target, tolerance = .05)
})

test_that("RWYB keeps a unit a later Poisson stage emptied", {
  # Rung 1: the unbiased estimator for clusters drawn by SRS and Poisson
  # households within them, where a cluster that drew nothing is a total of
  # zero, and its estimated variance within, a sum over the units drawn, is
  # zero too.
  N <- 6
  n <- 4
  p <- 0.3
  psu <- data.frame(psu = 1:N)
  hh <- data.frame(psu = rep(1:N, each = 3), id = 1:18,
                   y = c(2, 3, 4, 10, 12, 14, 1, 1, 2, 30, 35, 40, 5, 6, 7,
                         8, 8, 9))
  design <- sampling_design() |> cluster_by(psu) |> draw(n = n) |>
    add_stage() |> draw(frac = p, method = "bernoulli", on_empty = "silent")
  variance <- function(s, seed = 42) {
    r <- withr::with_seed(seed, as_svrepdesign(s, type = "rwyb",
                                               replicates = 20000))
    as.numeric(vcov(survey::svytotal(~y, r, na.rm = TRUE)))
  }
  emptied <- integer(0)
  for (seed in c(1, 3, 5, 12)) {
    s <- execute(design, psu, hh, seed = seed, frame_digest = "none")
    first <- execute(design, psu, seed = seed, stages = 1)
    emptied <- c(emptied, length(setdiff(first$psu, s$psu)))
    t <- vapply(first$psu, function(i) sum(s$y[s$psu == i]) / p, 0)
    want <- N^2 * (1 - n / N) * stats::var(t) / n +
      (N / n) * sum((1 - p) * s$y^2 / p^2)
    expect_equal(variance(s), want, tolerance = .05, info = paste(seed))
    # The frame digest only checks it, so it changes nothing.
    kept <- execute(design, psu, hh, seed = seed, frame_digest = "summary")
    expect_identical(variance(kept, seed = 7), variance(s, seed = 7),
                     info = paste(seed))
  }
  expect_true(all(emptied > 0L))
  # One cluster with rows, the others emptied, is no singleton.
  expect_true(any(emptied == n - 1L))
  # A zero row adds no degree of freedom, so dropping it for `na.rm` keeps
  # enough rows.
  s <- execute(design, psu, hh, seed = 1)
  r <- withr::with_seed(1, as_svrepdesign(s, type = "rwyb", replicates = 200))
  expect_no_warning(survey::svytotal(~y, r, na.rm = TRUE))
  expect_lt(survey::degf(r), sum(weights(r, "sampling") > 0))
  # The digest, when kept, checks the record: a record that misses an empty
  # pool leaves a selected cluster out, which the digest shows.
  attr(s, "metadata")$empty_pools <- list()
  expect_error(as_svrepdesign(s, type = "rwyb"),
               class = "samplyr_error_rwyb_missing_parents")
})

test_that("RWYB rejects an unrecorded lost parent, unsupported mechanisms and unidentified singleton variance", {
  frame <- data.frame(psu = rep(1:4, each = 4), id = 1:16, u = c(rep(.9, 4), rep(.1, 12)), y = 1)
  sample <- sampling_design() |> cluster_by(psu) |> draw(n = 3) |> add_stage() |>
    draw(frac = .5, method = "bernoulli", prn = u, on_empty = "silent") |> execute(frame, seed = 1)
  expect_s3_class(as_svrepdesign(sample, type = "rwyb"), "svyrep.design")
  # Without the record of the pools the Poisson stage left empty, a lost
  # parent cannot be told from a unit never selected, digest or none.
  for (digest in c("summary", "none")) {
    unrecorded <- sampling_design() |> cluster_by(psu) |> draw(n = 3) |>
      add_stage() |>
      draw(frac = .5, method = "bernoulli", prn = u, on_empty = "silent") |>
      execute(frame, seed = 1, frame_digest = digest)
    attr(unrecorded, "metadata")$empty_pools <- NULL
    expect_error(as_svrepdesign(unrecorded, type = "rwyb"),
                 class = "samplyr_error_rwyb_missing_parents", info = digest)
  }
  singleton <- sampling_design() |> draw(n = 1) |> execute(frame, seed = 1)
  expect_error(as_svrepdesign(singleton, type = "rwyb"), class = "samplyr_error_rwyb_singleton")
  poisson_one <- sampling_design() |> draw(frac = .1, method = "bernoulli", prn = u,
    on_empty = "silent") |> execute(data.frame(u = c(.01, rep(.9, 15)), y = 1))
  expect_gt(rwyb_variance(poisson_one, replicates = 1000), 0)
  pareto <- sampling_design() |> draw(n = 4, method = "pps_pareto", mos = id) |> execute(frame, seed = 1)
  cnd <- expect_error(as_svrepdesign(pareto, type = "rwyb"), class = "samplyr_error_rwyb_method")
  expect_identical(condition_header(cnd), "as_svrepdesign")
  # A built-in fixed-size method is told what RWYB resamples, not about
  # random sample sizes, which concern custom methods.
  expect_no_match(conditionMessage(cnd), "random sample size")
  expect_match(conditionMessage(cnd), "pps_multinomial", fixed = TRUE)
  expect_error(as_svrepdesign(singleton, type = "rwyb", replicates = 1), class = "samplyr_error_rwyb_input")
  # A single-stage design's only stage is final, so "certainty" applies there.
  expect_s3_class(
    as_svrepdesign(singleton, type = "rwyb", replicates = 10,
                   lonely.psu = "certainty"),
    "svyrep.design"
  )
  expect_error(as_svrepdesign(singleton, type = "rwyb", lonely.psu = "remove"),
    class = "samplyr_error_rwyb_input")
  expect_error(as_svrepdesign(singleton, type = "rwyb", lonely = "certainty"),
    class = "samplyr_error_unknown_argument")
})

test_that("RWYB singleton checks count stage units and allow certainties", {
  frame <- expand.grid(id = 1:4, psu = 1:4)
  first <- sampling_design() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> draw(n = 3) |> execute(frame, seed = 1)
  expect_equal(nrow(first), 3L)
  expect_error(as_svrepdesign(first, type = "rwyb"),
    class = "samplyr_error_rwyb_singleton")

  later <- sampling_design() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> draw(n = 1) |> execute(frame, seed = 1)
  expect_error(as_svrepdesign(later, type = "rwyb", replicates = 10),
    class = "samplyr_error_rwyb_singleton")

  census <- sampling_design() |> stratify_by(psu, id) |> draw(n = 1) |>
    execute(frame, seed = 1)
  expect_s3_class(as_svrepdesign(census, type = "rwyb", replicates = 10),
    "svyrep.design")
})

test_that("PPS approximation, seeds and srvyr export are explicit", {
  frame <- data.frame(id = 1:30, mos = 1:30, y = 1:30)
  sample <- sampling_design() |> draw(n = 10, method = "pps_brewer", mos = mos) |> execute(frame, seed = 3)
  expect_warning(r <- withr::with_seed(42, as_svrepdesign(sample, type = "rwyb")),
    class = "samplyr_warning_rwyb_pps_approximation")
  r2 <- suppressWarnings(withr::with_seed(42, as_svrepdesign(sample, type = "rwyb")))
  expect_equal(stats::weights(r, type = "analysis"), stats::weights(r2, type = "analysis"))
  expect_true(attr(r, "samplyr_replication")$pps_approximation)
  expect_equal(unname(stats::weights(r, type = "sampling")), sample$.weight)
  skip_if_not_installed("srvyr")
  expect_s3_class(suppressWarnings(srvyr::as_survey_rep(sample, type = "rwyb")), "tbl_svy")
})

test_that("independent frames combine RWYB variances by frame", {
  frame <- data.frame(id = 1:30, y = 1:30, in_a = TRUE, in_b = TRUE)
  a <- sampling_design() |> draw(frac = .5, method = "bernoulli") |> execute(frame, seed = 1)
  b <- sampling_design() |> draw(frac = .6, method = "bernoulli") |> execute(frame, seed = 2)
  frames <- stack_frames(a = a, b = b, membership = c(a = "in_a", b = "in_b"), key = id)
  result <- withr::with_seed(42, as_svrepdesign(frames, type = "rwyb", replicates = 200))
  separate <- withr::with_seed(42, lapply(list(a, b), function(s) {
    survey::svytotal(~y, as_svrepdesign(s, type = "rwyb", replicates = 200))
  }))
  estimate <- survey::svytotal(~y, result)
  expect_equal(as.numeric(coef(estimate)), sum(vapply(separate, function(z) as.numeric(coef(z)), 0)) / 2)
  expect_equal(as.numeric(vcov(estimate)), sum(vapply(separate, function(z) as.numeric(vcov(z)), 0)) / 4)
})

test_that("Poisson first-stage replication retains conditional SRS variance", {
  frame <- data.frame(psu = rep(1:12, each = 10), y = (1:120)^.7)
  sample <- sampling_design() |> cluster_by(psu) |> draw(frac = .5, method = "bernoulli") |>
    add_stage() |> draw(n = 5) |> execute(frame, seed = 1)
  totals <- tapply(sample$y * sample$.weight_2, sample$psu, sum)
  conditional <- sum(vapply(split(sample$y, sample$psu), function(y) 10^2 * (1 - 5/10) / 5 * var(y), 0))
  target <- sum((1 - .5) / .5^2 * totals^2) + conditional / .5
  expect_equal(rwyb_variance(sample), target, tolerance = .05)
})

## Zero rows are selected units only down to their record's stage

# A stage's selected units, keyed by the ancestry and hits above.
selection_keys <- function(design, k, df) {
  vars <- unique(c(
    samplyr:::collect_ancestor_strata_vars(design, k + 1L),
    samplyr:::collect_ancestor_cluster_vars(design, k + 1L),
    paste0(".draw_", seq_len(k))
  ))
  vars <- intersect(vars, names(df))
  keys <- unique(df[vars])
  sort(do.call(paste, c(lapply(keys, as.character), sep = "/")))
}

test_that("an empty middle stage keeps its parent without a selected child", {
  areas <- data.frame(a = 1:4)
  clusters <- data.frame(a = rep(1:4, each = 2), b = rep(1:2, 4),
                         st = rep(c("x", "y"), 4))
  persons <- data.frame(a = rep(1:4, each = 6),
                        b = rep(rep(1:2, each = 3), 4), id = 1:24, y = 1:24)
  design <- sampling_design() |> cluster_by(a) |> draw(n = 3) |>
    add_stage() |> stratify_by(st) |> cluster_by(b) |>
    draw(frac = .5, method = "bernoulli", on_empty = "silent") |>
    add_stage() |> draw(frac = 1)
  # Area 1 was selected and drew no cluster. Keys are stratum/area/cluster.
  prefix <- execute(design, areas, clusters, stages = 1:2, seed = 5)
  expect_identical(selection_keys(design, 2, as.data.frame(prefix)),
                   c("x/2/1", "x/3/1", "y/2/2", "y/3/2"))
  expect_identical(sort(unique(execute(design, areas, stages = 1,
                                       seed = 5)$a)), 1:3)
  results <- lapply(c("none", "summary", "full"), function(digest) {
    s <- execute(design, areas, clusters, persons, seed = 5,
                 frame_digest = digest)
    rows <- samplyr:::export_rows(s, every_level = TRUE, notify = FALSE)
    real <- is.na(attr(rows, "zero_from")) | attr(rows, "zero_from") > 2L
    expect_identical(selection_keys(design, 2, rows[real, ]),
                     c("x/2/1", "x/3/1", "y/2/2", "y/3/2"), info = digest)
    real <- is.na(attr(rows, "zero_from")) | attr(rows, "zero_from") > 1L
    expect_identical(sort(unique(rows$a[real])), 1:3, info = digest)
    expect_true(any(rows$a == 1 & rows$.weight == 0), info = digest)
    r <- withr::with_seed(1, as_svrepdesign(s, type = "rwyb",
                                            replicates = 200))
    e <- survey::svytotal(~y, r, na.rm = TRUE)
    list(estimate = c(coef(e), vcov(e)),
         weights = unname(as.matrix(weights(r, "analysis"))))
  })
  # The digest changes nothing, down to the replicate weights.
  expect_identical(results[[2]], results[[1]])
  expect_identical(results[[3]], results[[1]])
  expect_equal(unname(results[[1]]$estimate[1]), 400)
  # A record that misses the empty pool leaves area 1 out, which the digest
  # shows when it is kept.
  s <- execute(design, areas, clusters, persons, seed = 5)
  attr(s, "metadata")$empty_pools <- list()
  expect_error(as_svrepdesign(s, type = "rwyb"),
               class = "samplyr_error_rwyb_missing_parents")
})

test_that("every stage's selected units are those its prefix selected", {
  # The invariant over a grid, against executing the first k stages alone
  # with the same seed, which the zero rows must reproduce exactly at each
  # stage, under every digest setting, with the same RWYB result.
  areas <- data.frame(a = 1:5, mos = c(50, 1, 1, 2, 3))
  clusters <- data.frame(a = rep(1:5, each = 3), b = rep(1:3, 5),
                         st = rep(c("x", "y", "x"), 5))
  persons <- data.frame(a = rep(1:5, each = 9),
                        b = rep(rep(1:3, each = 3), 5), id = 1:45, y = 1:45)
  gaps <- list(areas, clusters[clusters$a != 2, ],
               persons[!(persons$a == 3 & persons$b == 2), ])
  first <- list(
    srs = function(d) d |> cluster_by(a) |> draw(n = 3),
    srswr = function(d) d |> cluster_by(a) |> draw(n = 4, method = "srswr"),
    census = function(d) d |> cluster_by(a) |> draw(frac = 1)
  )
  middle <- function(d) d |> add_stage() |> stratify_by(st) |>
    cluster_by(b) |> draw(frac = .4, method = "bernoulli", on_empty = "silent")
  last <- list(
    census = function(d) d |> add_stage() |> draw(frac = 1, on_empty = "silent"),
    bernoulli = function(d) d |> add_stage() |>
      draw(frac = .3, method = "bernoulli", on_empty = "silent")
  )
  reached <- character(0)
  for (f in names(first)) for (l in names(last)) for (g in c(FALSE, TRUE)) {
    design <- last[[l]](middle(first[[f]](sampling_design())))
    frames <- if (g) gaps else list(areas, clusters, persons)
    for (seed in 1:6) {
      run <- function(digest, stages = NULL) {
        suppressMessages(suppressWarnings(do.call(execute, c(
          list(design), if (is.null(stages)) frames else frames[stages],
          list(seed = seed, frame_digest = digest, stages = stages)
        ))))
      }
      out <- list()
      for (digest in c("none", "summary", "full")) {
        s <- run(digest)
        if (nrow(s) == 0L) next
        rows <- samplyr:::export_rows(s, every_level = TRUE, notify = FALSE)
        zero_from <- attr(rows, "zero_from")
        info <- paste(f, l, g, seed, digest)
        for (k in 1:2) {
          real <- is.na(zero_from) | zero_from > k
          expect_identical(selection_keys(design, k, rows[real, ]),
                           selection_keys(design, k, as.data.frame(run("none", 1:k))),
                           info = paste(info, k))
        }
        # A unit kept by a selected child has no placeholder of its own.
        placeholder <- rows$.weight == 0 & zero_from %in% 2L
        child <- !placeholder & !is.na(rows$b)
        expect_false(any(placeholder & paste(rows$a, rows$.draw_1) %in%
                           paste(rows$a, rows$.draw_1)[child]),
                     info = info)
        if (any(zero_from %in% 2L)) reached <- c(reached, "draw emptied")
        if (any(zero_from %in% 3L) && g) reached <- c(reached, "register or below")
        kept <- rows$.weight == 0 & zero_from %in% 3L
        if (any(kept & !rows$a %in% s$a)) reached <- c(reached, "kept by a deeper row")
        if (f == "srswr" && any(zero_from %in% 2L)) reached <- c(reached, "hit")
        out[[digest]] <- withr::with_seed(1, {
          r <- as_svrepdesign(s, type = "rwyb", replicates = 30)
          e <- survey::svytotal(~y, r, na.rm = TRUE)
          c(coef(e), vcov(e))
        })
      }
      if (length(out) == 3L) {
        expect_identical(out$summary, out$none, info = paste(f, l, g, seed))
        expect_identical(out$full, out$none, info = paste(f, l, g, seed))
      }
    }
  }
  expect_setequal(unique(reached), c("draw emptied", "register or below",
                                     "kept by a deeper row", "hit"))
})

test_that("RWYB refuses a singleton before it warns of an approximation", {
  skip_if_not_installed("svrep")
  frame <- data.frame(
    st = rep(c("a", "b"), c(5, 4)), id = 1:9,
    m = c(3, 5, 2, 6, 4, 2, 7, 3, 5), y = 1:9
  )
  n <- data.frame(st = c("a", "b"), n = c(2, 1))
  s <- sampling_design() |> stratify_by(st) |>
    draw(n = n, method = "pps_brewer", mos = m) |>
    execute(frame, seed = 1)
  expect_no_warning(expect_error(
    as_svrepdesign(s, type = "rwyb", replicates = 20),
    class = "samplyr_error_rwyb_singleton"
  ))
  # A sample it exports still says it approximates.
  n$n <- 2
  s <- sampling_design() |> stratify_by(st) |>
    draw(n = n, method = "pps_brewer", mos = m) |>
    execute(frame, seed = 1)
  expect_warning(as_svrepdesign(s, type = "rwyb", replicates = 20),
                 class = "samplyr_warning_rwyb_pps_approximation")
})
