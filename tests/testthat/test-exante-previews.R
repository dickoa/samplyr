test_that("Poisson n and frac share a certainty-adjusted expected target", {
  frame <- data.frame(id = 1:10, size = c(100, rep(1, 9)))
  for (target in c(5, 4)) {
    by_n <- sampling_design() |>
      draw(n = target, method = "pps_poisson", mos = size, certainty_size = 100)
    by_frac <- sampling_design() |>
      draw(frac = target / 10, method = "pps_poisson", mos = size,
           certainty_size = 100)
    expected <- c(1, rep((target - 1) / 9, 9))
    for (design in list(by_n, by_frac)) {
      probabilities <- exante_probabilities(design, frame, key = id)
      expect_equal(probabilities$probability, expected)
      preview <- frame_summary(design, frame)
      expect_equal(preview$n_target, target)
      expect_equal(preview$n_expected, target)
      sample <- execute(design, frame, seed = 2)
      expect_equal(1 / sample$.weight, expected[sample$id])
    }
    expect_equal(execute(by_n, frame, seed = 2)$id,
                 execute(by_frac, frame, seed = 2)$id)
  }
  fractional <- sampling_design() |>
    draw(frac = 0.45, method = "pps_poisson", mos = size, certainty_size = 100)
  expect_equal(exante_probabilities(fractional, frame, key = id)$probability,
               c(1, rep(3.5 / 9, 9)))
  expect_equal(frame_summary(fractional, frame)$n_expected, 4.5)
})

test_that("Poisson certainty previews preserve clipping and stratum targets", {
  frame <- data.frame(id = 1:10, size = c(100, 80, rep(1, 8)))
  design <- sampling_design() |>
    draw(frac = 0.5, method = "pps_poisson", mos = size, certainty_size = 100)
  # Clipping the second unit at one does not redistribute its excess chance.
  expected <- c(1, 1, rep(4 / 88, 8))
  expect_equal(exante_probabilities(design, frame, key = id)$probability, expected)
  expect_equal(frame_summary(design, frame)$n_expected, sum(expected))
  expect_warning(sample <- execute(design, frame, seed = 2),
                 class = "samplyr_warning_poisson_shortfall")
  expect_equal(1 / sample$.weight, expected[sample$id])

  frame <- data.frame(id = 1:20, st = rep(c("a", "b"), each = 10),
                      size = rep(c(100, rep(1, 9)), 2))
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(frac = 0.45, method = "pps_poisson", mos = size, certainty_size = 100)
  expect_equal(exante_probabilities(design, frame, key = id)$probability,
               rep(c(1, rep(3.5 / 9, 9)), 2))
  expect_equal(frame_summary(design, frame, detail = "pool")$n_expected, c(4.5, 4.5))
})

test_that("certainty-plan previews use stored PSU classifications and takes", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  frame$id <- seq_len(nrow(frame))
  first <- sampling_design() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N)
  design <- first |> add_stage() |> draw(n = plan)

  psu <- plan$psu
  pi1 <- rep(1, nrow(psu))
  for (h in plan$detail$stratum) {
    at <- psu$stratum == h & !psu$certainty
    n_draw <- plan$detail$n_psu_draw[plan$detail$stratum == h]
    pi1[at] <- n_draw * psu$N[at] / sum(psu$N[at])
  }
  at <- match(frame$psu_id, psu$psu_id)
  expect_equal(exante_probabilities(first, frame, key = id)$probability, pi1[at])
  expected <- (pi1 * psu$n_take / psu$N)[at]
  expect_equal(exante_probabilities(design, frame, key = id)$probability, expected)
  preview <- frame_summary(design, frame, detail = "pool")
  expect_equal(sort(preview$n_target[preview$stage == 2]), sort(psu$n_take))
  expect_equal(sort(preview$n_expected[preview$stage == 2]), sort(psu$n_take))
  separate <- frame_summary(design, list(certainty_plan_register(), frame),
                             detail = "pool")
  expect_equal(separate$n_expected, preview$n_expected)
  sample <- execute(design, frame, seed = 11)
  expect_equal(1 / sample$.weight, expected[sample$id])

  # Allocation within a parent uses that parent's take, including unselected PSUs.
  stratified <- first |> add_stage() |>
    stratify_by(sex, alloc = "proportional") |> draw(n = plan)
  preview <- frame_summary(stratified, frame, detail = "pool")
  take <- preview[preview$stage == 2, ]
  expect_equal(sort(as.numeric(tapply(take$n_target, take$parent_unit, sum))),
               sort(psu$n_take))
  resolved <- exante_probabilities(stratified, frame, key = id)
  sample <- execute(stratified, frame, seed = 11)
  expect_equal(1 / sample$.weight, resolved$probability[sample$id])

  altered <- frame
  altered$N[altered$psu_id == "A01"] <- 599
  expect_error(frame_summary(design, altered),
               class = "samplyr_error_certainty_register_mismatch")
  expect_error(exante_probabilities(design, altered, key = id),
               class = "samplyr_error_certainty_register_mismatch")
})

test_that("ex-ante approximate probabilities need opt-in and retain their quality", {
  frame <- data.frame(id = 1:60, size = rep(c(1, 2, 3), 20),
                      psu = rep(1:10, each = 6))
  exact <- sampling_design() |> draw(n = 12)
  expect_identical(unique(exante_probabilities(exact, frame, key = id)$probability_quality),
                   "exact")
  for (method in c("pps_sps", "pps_pareto")) {
    design <- sampling_design() |> draw(n = 12, method = method, mos = size)
    expect_error(exante_probabilities(design, frame, key = id),
                 class = "samplyr_error_exante_approximate")
    result <- exante_probabilities(design, frame, key = id, allow_approximate = TRUE)
    expect_identical(unique(result$probability_quality), "approximate")
    expect_equal(result$probability, 12 * frame$size / sum(frame$size))

    multistage <- sampling_design() |> cluster_by(psu) |> draw(n = 5) |>
      add_stage() |> draw(n = 2, method = method, mos = size)
    expect_error(exante_probabilities(multistage, frame, key = id),
                 class = "samplyr_error_exante_approximate")
    result <- exante_probabilities(multistage, frame, key = id,
                                   allow_approximate = TRUE)
    expect_identical(unique(result$probability_quality), "approximate")
    expect_equal(result$probability, 0.5 * 2 * frame$size / 12)
  }
  for (bad in list(NA, 1, "yes", logical(), c(TRUE, FALSE))) {
    expect_error(exante_probabilities(exact, frame, key = id, allow_approximate = bad),
                 class = "samplyr_error_exante_unsupported")
  }
  frame$probability_quality <- frame$id
  expect_error(exante_probabilities(exact, frame, key = probability_quality),
               class = "samplyr_error_exante_key")
})

test_that("approximate overlap quality survives stacking and survey exports", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:60, size = rep(c(1, 2, 3), 20),
                      in_a = TRUE, in_b = TRUE, y = 1:60)
  a <- sampling_design() |> draw(n = 12, method = "pps_pareto", mos = size) |>
    execute(frame, seed = 1)
  b <- sampling_design() |> draw(n = 15) |> execute(frame, seed = 2)
  stack <- function(overlaps) {
    stack_frames(a = a, b = b, membership = c(a = "in_a", b = "in_b"),
                 key = id, overlaps = overlaps)
  }
  expect_error(stack(exante_overlaps(list(a = frame, b = frame), by = c(id = "id"))),
               class = "samplyr_error_exante_approximate")
  result <- stack(exante_overlaps(list(a = frame, b = frame), by = c(id = "id"),
                                  allow_approximate = TRUE))
  quality <- c(a = "approximate", b = "exact")
  expect_identical(attr(result, "overlaps")$probability_quality, quality)
  expect_output(print(result), "approximate probabilities accepted")
  linear <- as_svydesign(result, estimator = "expected")
  expect_identical(attr(linear, "samplyr_overlap_probability_quality"), quality)
  reps <- suppressWarnings(as_svrepdesign(result, estimator = "expected",
                                          type = "subbootstrap", replicates = 20))
  expect_identical(attr(reps, "samplyr_overlap_probability_quality"), quality)
  expect_equal(coef(survey::svytotal(~y, linear)), coef(survey::svytotal(~y, reps)))
})

test_that("custom ex-ante quality uses the registered probability contract", {
  on.exit(sondage::unregister_method("review_approx"), add = TRUE)
  on.exit(sondage::unregister_method("review_unknown"), add = TRUE)
  sample_fn <- function(pik, n = NULL, ...) seq_len(n)
  frame <- data.frame(id = 1:20, size = rep(1:4, 5))
  for (quality in c("approximate", "unknown")) {
    name <- if (quality == "approximate") "review_approx" else "review_unknown"
    sondage::register_method(name, "wor", sample_fn = sample_fn,
                             probabilities = quality)
    if (quality == "unknown") {
      expect_error(sampling_design() |>
                     draw(n = 5, method = paste0("pps_", name), mos = size),
                   class = "samplyr_error_unknown_probabilities")
      # Protect the resolver even if an older object bypasses draw's guard.
      design$stages[[1]]$draw_spec$method_probabilities <- "unknown"
      expect_error(exante_probabilities(design, frame, key = id,
                                        allow_approximate = TRUE),
                   class = "samplyr_error_exante_unsupported")
    } else {
      design <- sampling_design() |>
        draw(n = 5, method = paste0("pps_", name), mos = size)
      expect_error(exante_probabilities(design, frame, key = id),
                   class = "samplyr_error_exante_approximate")
      result <- exante_probabilities(design, frame, key = id,
                                     allow_approximate = TRUE)
      expect_identical(unique(result$probability_quality), "approximate")
    }
  }
})
