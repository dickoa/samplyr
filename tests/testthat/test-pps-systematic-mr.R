## Systematic PPS with minimum replacement

# A frame of 19 units where unit 1 spans more than one sampling interval:
# with n = 4 its expected hits are 4 * 70 / 160 = 1.75.
mr_frame <- function() {
  data.frame(id = 1:19, size = c(70, rep(5, 18)), y = c(100, 1:18),
             st = rep(c("a", "b"), length.out = 19))
}

mr_design <- function(n = 4, ...) {
  sampling_design() |> draw(n = n, method = "pps_systematic_mr", mos = size, ...)
}

# Four equal units in the order 1, 2, 3, 4: a period that matches the
# interval of two draws.
periodic_frame <- function() {
  data.frame(id = 1:4, size = 1, y = c(0, 1, 0, 1))
}

## Selection

test_that("each unit gets the floor or the ceiling of its expected hits", {
  frame <- mr_frame()
  hits <- 4 * frame$size / sum(frame$size)
  expect_identical(hits[1], 1.75)
  seen <- integer(0)
  for (seed in 1:40) {
    s <- execute(mr_design(), frame, seed = seed)
    expect_identical(nrow(s), 4L)
    expect_identical(s$.draw_1, 1:4)
    counts <- tabulate(s$id, nbins = 19)
    expect_true(all(counts >= floor(hits) & counts <= ceiling(hits)))
    expect_equal(s$.weight, sum(frame$size) / (4 * s$size))
    seen <- union(seen, counts[1])
  }
  expect_setequal(seen, 1:2)
})

test_that("a whole number of expected hits is taken exactly", {
  frame <- data.frame(id = 1:5, size = c(4, 1, 1, 1, 1))
  for (seed in 1:10) {
    s <- execute(mr_design(n = 4), frame, seed = seed)
    expect_identical(sum(s$id == 1), 2L)
    expect_identical(nrow(s), 4L)
  }
})

test_that("n above N, frac above 1, zero sizes and residue in the total", {
  frame <- data.frame(id = 1:5, size = c(0, 1, 2, 3, 4))
  expect_warning(s <- execute(mr_design(n = 12), frame, seed = 1),
                 class = "samplyr_warning_mos_zero")
  expect_identical(nrow(s), 12L)
  expect_false(1L %in% s$id)
  expect_warning(
    s <- execute(
      sampling_design() |>
        draw(frac = 1.6, method = "pps_systematic_mr", mos = size),
      frame, seed = 1
    ),
    class = "samplyr_warning_mos_zero"
  )
  expect_identical(nrow(s), 8L)
  # Sizes whose expected hits sum to 3 only up to floating-point residue.
  frame <- data.frame(id = 1:7,
                      size = c(0.22, 0.66, 0.13, 0.27, 0.39, 0.02, 0.39))

  # The residue depends on the platform's long double: x86 leaves one,
  # Apple Silicon may not, so only the draw itself is checked everywhere.
  residue <- sum(sondage::expected_hits(frame$size, 3)) != 3
  for (seed in 1:10) {
    expect_identical(nrow(execute(mr_design(n = 3), frame, seed = seed)), 3L)
  }
  if (!residue) {
    skip("no floating-point residue on this platform")
  }
})

test_that("below one expected hit it draws what pps_systematic draws", {
  frame <- data.frame(id = 1:30, size = rep(c(3, 5, 8), 10))
  for (seed in 1:10) {
    mr <- execute(mr_design(n = 6), frame, seed = seed)
    wor <- execute(
      sampling_design() |>
        draw(n = 6, method = "pps_systematic", mos = size),
      frame, seed = seed
    )
    expect_identical(sort(mr$id), sort(wor$id))
  }
})

test_that("a stratified draw takes each stratum's number of hits", {
  frame <- mr_frame()
  s <- execute(
    sampling_design() |> stratify_by(st) |>
      draw(n = c(a = 3, b = 5), method = "pps_systematic_mr", mos = size),
    frame, seed = 2
  )
  expect_identical(as.vector(table(s$st)[c("a", "b")]), c(3L, 5L))
})

test_that("a PSU hit twice gets one second-stage draw per hit", {
  frame <- data.frame(
    psu = rep(1:6, each = 4),
    hh = 1:24,
    psize = rep(c(30, 4, 4, 4, 4, 4), each = 4)
  )
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_systematic_mr", mos = psize) |>
    add_stage() |> draw(n = 2)
  for (seed in 1:5) {
    s <- execute(design, frame, seed = seed)
    big <- s[s$psu == 1, ]
    # PSU 1 has 3 * 30 / 50 = 1.8 expected hits.
    expect_true(length(unique(big$.draw_1)) %in% 1:2)
    expect_identical(as.vector(table(big$.draw_1)),
                     rep(2L, length(unique(big$.draw_1))))
    expect_equal(big$.weight, rep(50 / (3 * 30) * 4 / 2, nrow(big)))
  }
  hit_twice <- Filter(function(seed) {
    s <- execute(design, frame, seed = seed)
    length(unique(s$.draw_1[s$psu == 1])) == 2L
  }, 1:10)
  expect_gt(length(hit_twice), 0L)
})

test_that("the method is refused where minimum replacement does not apply", {
  skip_if_not_installed("survey")
  frame <- mr_frame()
  expect_error(sampling_design() |>
                 draw(n = 4, method = "pps_systematic_mr"),
               class = "samplyr_error_draw_method_argument")
  expect_error(mr_design(reserve = 2), class = "samplyr_error_reserve_method")
  expect_error(
    sampling_design() |> draw(n = 4, method = "systematic_mr", mos = size),
    "pps_systematic_mr",
    class = "samplyr_error_unknown_method"
  )
  s <- execute(mr_design(), frame, seed = 1)
  expect_error(as_svrepdesign(s, type = "rwyb"),
               class = "samplyr_error_rwyb_method")
})

test_that("summary and the unit digest show hits", {
  frame <- mr_frame()
  s <- execute(mr_design(), frame, seed = 1, frame_digest = "full")
  out <- cli::ansi_strip(capture.output(print(summary(s))))
  expect_true(any(grepl("pps_systematic_mr (minimum replacement)", out,
                        fixed = TRUE)))
  units <- frame_summary(s, detail = "unit")
  expect_equal(units$chance, 4 * frame$size / sum(frame$size))
  expect_identical(
    as.integer(units$n_hits[units$is_selected]),
    as.vector(table(s$id))[match(units$unit_id[units$is_selected],
                                 names(table(s$id)))]
  )
})

test_that("a design and its sample survive JSON and replay", {
  frame <- mr_frame()
  path <- withr::local_tempfile(fileext = ".json")
  write_design(mr_design(), path)
  restored <- read_design(path)
  expect_identical(restored$stages[[1]]$draw_spec$method, "pps_systematic_mr")

  for (seed in 1:5) {
    s <- execute(mr_design(), frame, seed = seed)
    if (any(duplicated(s$id))) break
  }
  expect_true(any(duplicated(s$id)))
  again <- replay_design(s, frame)
  expect_identical(again$id, s$id)
  expect_identical(again$.draw_1, s$.draw_1)
  expect_identical(again$.weight, s$.weight)
})

## Joint expected hits

test_that("structural zeros sit in the population matrix, not the sample's", {
  full <- sondage::joint_expected_hits(
    sondage::unequal_prob_wr(rep(0.5, 4), method = "systematic_mr")
  )
  expect_identical(full[1, c(2, 4)], c(0, 0))
  expect_identical(full[1, 3], 0.5)

  frame <- periodic_frame()
  design <- mr_design(n = 2)
  for (seed in 1:6) {
    s <- execute(design, frame, seed = seed)
    expect_true(identical(sort(s$id), c(1L, 3L)) ||
                  identical(sort(s$id), c(2L, 4L)))
    je <- joint_expectation(s, frame)$stage_1
    expect_identical(je, matrix(0.5, 2, 2))
  }
})

test_that("the matrix is sondage's, exact and free of nsim and seed", {
  frame <- mr_frame()
  s <- execute(mr_design(), frame, seed = 3)
  ids <- unique(s$id)
  hits <- sondage::expected_hits(frame$size, 4)
  full <- sondage::joint_expected_hits(
    sondage::unequal_prob_wr(hits, method = "systematic_mr")
  )
  je <- joint_expectation(s, frame)$stage_1
  expect_equal(je, unname(full[ids, ids]), tolerance = 0)

  # A unit with whole part c of its expected hits f has E(n^2) = f(2c + 1) -
  # c(c + 1).
  f <- hits[ids]
  c0 <- floor(f)
  expect_equal(diag(je), f * (2 * c0 + 1) - c0 * (c0 + 1))

  set.seed(11)
  before <- .Random.seed
  expect_identical(joint_expectation(s, frame, nsim = 5L, seed = 9L)$stage_1,
                   je)
  expect_identical(.Random.seed, before)
})

test_that("the joint matrix needs the order the sample was drawn in", {
  frame <- mr_frame()
  s <- execute(mr_design(), frame, seed = 1)
  expect_error(joint_expectation(s, frame[19:1, ]),
               class = "samplyr_error_joint_frame_mismatch")
  full <- execute(mr_design(), frame, seed = 1, frame_digest = "full")
  expect_identical(joint_expectation(full), joint_expectation(full, frame))
})

## Variance

test_that("the export warns that a systematic stage is approximated", {
  skip_if_not_installed("survey")
  s <- execute(mr_design(), mr_frame(), seed = 1)
  expect_warning(svy <- as_svydesign(s),
                 class = "samplyr_warning_systematic_variance")
  expect_identical(unname(svy$fpc$popsize[, 1]), rep(Inf, 4))
  expect_identical(
    attr(svy, "samplyr_systematic_variance")$approximation,
    "with_replacement"
  )
  expect_no_warning(as_svydesign(s, systematic_variance = "approximate"))
  expect_error(as_svydesign(s, systematic_variance = "error"),
               class = "samplyr_error_systematic_variance")
  for (type in c("bootstrap", "subbootstrap", "mrbbootstrap")) {
    expect_warning(
      as_svrepdesign(s, type = type, replicates = 10,
                     systematic_variance = "warn"),
      class = "samplyr_warning_systematic_variance"
    ) |> suppressWarnings()
  }
})

test_that("on a periodic frame the exported variance is zero", {
  skip_if_not_installed("survey")
  frame <- periodic_frame()
  design <- mr_design(n = 2)
  totals <- c()
  for (seed in 1:6) {
    s <- execute(design, frame, seed = seed)
    svy <- as_svydesign(s, systematic_variance = "approximate")
    est <- survey::svytotal(~y, svy)
    totals[paste(sort(s$id), collapse = "")] <- unname(coef(est))
    expect_identical(unname(survey::SE(est))[1], 0)
  }
  # The two samples are equally likely, so the true variance is 4.
  expect_identical(totals[c("13", "24")], c(`13` = 0, `24` = 4))
  expect_identical(mean((totals - mean(totals))^2), 4)
})

test_that("a stage that can take a unit twice is no census", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:4, size = 1, y = 1:4)
  s <- execute(mr_design(n = 6), frame, seed = 1)
  # Every expected hit is 1.5, so the hits vary between samples.
  expect_identical(sort(as.vector(table(s$id))), c(1L, 1L, 2L, 2L))
  expect_warning(as_svydesign(s), class = "samplyr_warning_systematic_variance")
  report <- variance_estimators(mr_design(n = 6), frame)
  expect_identical(report$class[1], "samplyr_warning_systematic_variance")
})

test_that("random groups repeat the selection and need no warning", {
  skip_if_not_installed("survey")
  repeated <- execute(mr_design(), mr_frame(), seed = 1, reps = 6)
  rg <- expect_no_warning(as_svrepdesign(repeated, type = "random_groups"))
  expect_identical(ncol(rg$repweights), 6L)
  report <- variance_estimators(execute(mr_design(), mr_frame(), seed = 1))
  expect_identical(
    report$status[report$call == "as_svrepdesign(x, type = \"random_groups\")"],
    "supported"
  )
})

test_that("the report says the frame order sets the direction of the error", {
  report <- variance_estimators(mr_design(), mr_frame())
  expect_match(report$note[1], "overstates or understates", fixed = TRUE)
  expect_no_match(report$note[1], "conservative")
})

## Help page example

test_that("the draw() example repeats the largest EA as its comment says", {
  poura <- subset(bfa_eas, commune == "Poura")
  largest <- poura$ea_id[which.max(poura$households)]
  expected_hits <- 8 * max(poura$households) / sum(poura$households)
  expect_equal(round(expected_hits, 2), 1.74)
  s <- sampling_design() |>
    draw(n = 8, method = "pps_systematic_mr", mos = households) |>
    execute(poura, seed = 1)
  expect_equal(sum(s$ea_id == largest), 2L)
  expect_equal(s$.weight[s$ea_id == largest], rep(1 / expected_hits, 2))
  expect_identical(s$.draw_1, 1:8)
})

test_that("fixed hit counts still get a with-replacement variance", {
  skip_if_not_installed("survey")
  # Sizes 2, 1 and 1 with a take of 4 give every unit a whole number of
  # expected hits, so the counts never vary and the total has no variance.
  frame <- data.frame(id = 1:3, size = c(2, 1, 1), y = c(1, 3, 7))
  design <- sampling_design() |>
    draw(n = 4, method = "pps_systematic_mr", mos = size)
  for (seed in 1:5) {
    expect_identical(tabulate(execute(design, frame, seed = seed)$id, 3), c(2L, 1L, 1L))
  }
  # Hansen-Hurwitz by hand: each draw estimates the total by y / p, with
  # p = E(n) / n, giving 2, 2, 12 and 28.
  z <- c(2, 2, 12, 28)
  s <- execute(design, frame, seed = 1)
  svy <- as_svydesign(s, systematic_variance = "approximate")
  total <- survey::svytotal(~y, svy)
  expect_equal(as.numeric(coef(total)), mean(z))
  expect_equal(as.numeric(survey::SE(total))^2, var(z) / 4)
  expect_equal(round(var(z) / 4, 1), 37.7)
})
