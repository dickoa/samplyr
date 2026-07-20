# Tests for custom registered sondage methods flowing through samplyr

# Deterministic WOR: always picks the n units with largest pik
toy_wor_fn <- function(pik, n = NULL, prn = NULL, ...) {
  order(pik, decreasing = TRUE)[seq_len(n)]
}

# Deterministic WOR with PRN support
toy_prn_fn <- function(pik, n = NULL, prn = NULL, ...) {
  if (is.null(prn)) {
    prn <- runif(length(pik))
  }
  which(prn < pik)
}

# Simple joint: outer product (independent approximation)
toy_joint_fn <- function(pik, sample_idx = NULL, ...) {
  if (!is.null(sample_idx)) {
    pik <- pik[sample_idx]
  }
  J <- outer(pik, pik)
  diag(J) <- pik
  J
}

# Deterministic WR: multinomial-like, always pick top-prob units
toy_wr_fn <- function(hits, n = NULL, prn = NULL, ...) {
  sample.int(length(hits), size = n, replace = TRUE, prob = hits)
}

# Deterministic WR test method when PRNs are supplied. The registry owns the
# interpretation of PRN for custom methods; this simple implementation makes
# forwarding observable without depending on the ambient RNG stream.
toy_wr_prn_fn <- function(hits, n = NULL, prn = NULL, ...) {
  if (is.null(prn)) {
    prn <- runif(length(hits))
  }
  selected <- order(prn)[seq_len(min(n, length(hits)))]
  rep(selected, length.out = n)
}

set.seed(20260328)
custom_frame <- data.frame(
  id = seq_len(100),
  region = factor(rep(c("North", "South"), each = 50)),
  size = c(runif(50, 10, 100), runif(50, 5, 50)),
  prn_col = runif(100)
)

test_that("draw() accepts registered custom methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  design <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size)

  expect_s3_class(design, "sampling_design")
  expect_equal(design$stages[[1]]$draw_spec$method, "pps_test_wor")
  expect_equal(design$stages[[1]]$draw_spec$method_type, "wor")
  expect_true(design$stages[[1]]$draw_spec$method_fixed)
})

test_that("draw() rejects unregistered custom methods", {
  expect_error(
    sampling_design() |> draw(n = 10, method = "pps_nonexistent", mos = size),
    "Unknown sampling method"
  )
})

test_that("draw() requires mos for custom methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  expect_error(
    sampling_design() |> draw(n = 10, method = "pps_test_wor"),
    "mos"
  )
})

test_that("draw() validates frac <= 1 for custom WOR methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  expect_error(
    sampling_design() |> draw(frac = 1.5, method = "pps_test_wor", mos = size),
    "cannot exceed 1"
  )
})

test_that("draw() validates PRN support for custom methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method(
    "test_wor",
    "wor",
    sample_fn = toy_wor_fn,
    supports_prn = FALSE,
    probabilities = "exact"
  )

  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_test_wor", mos = size, prn = prn_col),
    "permanent random numbers"
  )
})

test_that("draw() accepts PRN for custom methods that support it", {
  on.exit(sondage::unregister_method("test_prn"), add = TRUE)
  sondage::register_method(
    "test_prn",
    "wor",
    sample_fn = toy_prn_fn,
    fixed_size = FALSE,
    supports_prn = TRUE,
    probabilities = "exact"
  )

  design <- sampling_design() |>
    draw(n = 20, method = "pps_test_prn", mos = size, prn = prn_col)
  expect_equal(design$stages[[1]]$draw_spec$prn, "prn_col")
})

test_that("execute() works with custom WOR method", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_equal(nrow(result), 10)
  expect_true(".weight_1" %in% names(result))
  expect_true(".fpc_1" %in% names(result))
  # WOR: FPC should be finite
  expect_true(all(is.finite(result$.fpc_1)))
})

test_that("execute() works with stratified custom WOR method", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  # n = 10 per stratum x 2 strata = 20
  expect_equal(nrow(result), 20)
  expect_equal(length(unique(result$region)), 2)
})

test_that("execute() works with custom WR method", {
  on.exit(sondage::unregister_method("test_wr"), add = TRUE)
  sondage::register_method("test_wr", "wr", sample_fn = toy_wr_fn, probabilities = "exact")

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wr", mos = size) |>
    execute(custom_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  # WR: FPC should be Inf
  expect_true(all(is.infinite(result$.fpc_1)))
  # WR: .draw column
  expect_true(".draw_1" %in% names(result))
})

test_that("custom WR PRN controls non-certainty selection", {
  on.exit(sondage::unregister_method("test_wr_prn"), add = TRUE)
  sondage::register_method(
    "test_wr_prn",
    "wr",
    sample_fn = toy_wr_prn_fn,
    supports_prn = TRUE,
    probabilities = "exact"
  )
  frame <- data.frame(
    id = seq_len(20),
    size = rep(1, 20),
    u = (seq_len(20) - 0.5) / 20
  )
  design <- sampling_design() |>
    draw(n = 8, method = "pps_test_wr_prn", mos = size, prn = u)

  first <- execute(design, frame, seed = 1)
  second <- execute(design, frame, seed = 999)
  changed <- frame
  changed$u <- rev(changed$u)
  third <- execute(design, changed, seed = 1)

  expect_identical(first$id, second$id)
  expect_false(identical(first$id, third$id))
  expect_false(any(first$.certainty_1))
})

test_that("custom WR methods must declare PRN support", {
  on.exit(sondage::unregister_method("test_wr_no_prn"), add = TRUE)
  sondage::register_method(
    "test_wr_no_prn",
    "wr",
    sample_fn = toy_wr_fn,
    supports_prn = FALSE,
    probabilities = "exact"
  )

  expect_error(
    sampling_design() |>
      draw(n = 8, method = "pps_test_wr_no_prn", mos = size, prn = prn_col),
    "permanent random numbers"
  )
})

test_that("execute() works with custom random-size PRN method", {
  on.exit(sondage::unregister_method("test_prn"), add = TRUE)
  sondage::register_method(
    "test_prn",
    "wor",
    sample_fn = toy_prn_fn,
    fixed_size = FALSE,
    supports_prn = TRUE,
    probabilities = "exact"
  )

  result <- sampling_design() |>
    draw(frac = 0.3, method = "pps_test_prn", mos = size, prn = prn_col) |>
    execute(custom_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_true(nrow(result) > 0)
})

test_that("certainty selection works with custom WOR method", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  # Extreme skew: one unit dominates
  cert_frame <- data.frame(
    id = 1:20,
    size = c(1000, rep(1, 19))
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_test_wor", mos = size, certainty_prop = 0.5) |>
    execute(cert_frame, seed = 1)

  expect_s3_class(result, "tbl_sample")
  expect_true(".certainty_1" %in% names(result))
  expect_true(any(result$.certainty_1))
})

test_that("joint_expectation() works with custom WOR method + joint_fn", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method(
    "test_wor",
    "wor",
    sample_fn = toy_wor_fn,
    joint_fn = toy_joint_fn,
    probabilities = "exact"
  )

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  jip <- joint_expectation(result, custom_frame)
  expect_type(jip, "list")
  stage1 <- jip$stage_1
  expect_true(is.matrix(stage1))
  expect_equal(nrow(stage1), 10)
  expect_true(isSymmetric(stage1))
})

test_that("joint_expectation() errors gracefully without joint_fn", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  expect_error(joint_expectation(result, custom_frame), "not implemented")
})

test_that("joint_expectation() forwards nsim to registered WR joint_fn", {
  on.exit(sondage::unregister_method("test_wr_nsim"), add = TRUE)
  seen <- new.env(parent = emptyenv())
  joint_fn <- function(hits, sample_idx = NULL, nsim = 10000L, ...) {
    seen$nsim <- nsim
    if (!is.null(sample_idx)) hits <- hits[sample_idx]
    out <- outer(hits, hits)
    diag(out) <- hits
    out
  }
  sondage::register_method(
    "test_wr_nsim",
    "wr",
    sample_fn = toy_wr_fn,
    joint_fn = joint_fn,
    probabilities = "exact"
  )

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wr_nsim", mos = size) |>
    execute(custom_frame, seed = 1, frame_digest = "full")

  expect_no_error(joint_expectation(result, custom_frame, nsim = 37L))
  expect_identical(seen$nsim, 37L)
  expect_no_error(joint_expectation(result, nsim = 41L))
  expect_identical(seen$nsim, 41L)
})

test_that("as_svydesign() works with custom WOR method", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  svyd <- as_svydesign(result)
  expect_s3_class(svyd, "survey.design2")
})

test_that("as_svydesign() works with custom WR method", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_wr"), add = TRUE)
  sondage::register_method("test_wr", "wr", sample_fn = toy_wr_fn, probabilities = "exact")

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wr", mos = size) |>
    execute(custom_frame, seed = 1)

  svyd <- as_svydesign(result)
  expect_s3_class(svyd, "survey.design2")
})

test_that("summary() works with custom methods", {
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size) |>
    execute(custom_frame, seed = 1)

  out <- capture.output(summary(result))
  expect_true(any(grepl("pps_test_wor", out)))
})

test_that("summary() shows WR label for custom WR method", {
  on.exit(sondage::unregister_method("test_wr"), add = TRUE)
  sondage::register_method("test_wr", "wr", sample_fn = toy_wr_fn, probabilities = "exact")

  result <- sampling_design() |>
    draw(n = 10, method = "pps_test_wr", mos = size) |>
    execute(custom_frame, seed = 1)

  out <- capture.output(summary(result))
  expect_true(any(grepl("with replacement", out)))
})

# --- Random-size custom WOR methods (fixed_size = FALSE) ---

# Poisson-type selection: independent Bernoulli trials against pik
toy_random_wor_fn <- function(pik, n = NULL, prn = NULL, ...) {
  which(runif(length(pik)) < pik)
}

test_that("custom random-size WOR export errors instead of using Brewer", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_rswor"), add = TRUE)
  sondage::register_method(
    "test_rswor", "wor",
    sample_fn = toy_random_wor_fn, fixed_size = FALSE,
    probabilities = "exact"
  )

  result <- sampling_design() |>
    draw(n = 20, method = "pps_test_rswor", mos = size) |>
    execute(custom_frame, seed = 5)

  # Brewer would silently report near-zero variance for this design
  expect_error(
    as_svydesign(result),
    class = "samplyr_error_custom_random_wor_export"
  )
})

test_that("custom random-size WOR exports with explicit poisson_sampling", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_rswor"), add = TRUE)
  sondage::register_method(
    "test_rswor", "wor",
    sample_fn = toy_random_wor_fn, fixed_size = FALSE,
    probabilities = "exact"
  )

  result <- sampling_design() |>
    draw(n = 20, method = "pps_test_rswor", mos = size) |>
    execute(custom_frame, seed = 5)

  svyd <- as_svydesign(
    result,
    pps = survey::poisson_sampling(1 / result$.weight)
  )
  expect_s3_class(svyd, "survey.design")

  # Matches the built-in pps_poisson export on the same realization
  builtin <- sampling_design() |>
    draw(n = 20, method = "pps_poisson", mos = size) |>
    execute(custom_frame, seed = 5)
  svyd_builtin <- as_svydesign(builtin)
  se_custom <- as.numeric(survey::SE(survey::svytotal(~id, svyd)))
  expect_gt(se_custom, 0)
  if (identical(sort(result$id), sort(builtin$id))) {
    se_builtin <- as.numeric(survey::SE(survey::svytotal(~id, svyd_builtin)))
    expect_equal(se_custom, se_builtin, tolerance = 1e-8)
  }
})

test_that("custom random-size WOR supports the subbootstrap escape hatch", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_rswor"), add = TRUE)
  sondage::register_method(
    "test_rswor", "wor",
    sample_fn = toy_random_wor_fn, fixed_size = FALSE,
    probabilities = "exact"
  )

  result <- sampling_design() |>
    draw(n = 20, method = "pps_test_rswor", mos = size) |>
    execute(custom_frame, seed = 5)

  rep_design <- as_svrepdesign(result, type = "subbootstrap", replicates = 20)
  expect_s3_class(rep_design, "svyrep.design")
})

# --- Custom balanced (cube-like) methods, type = "balanced" ---

# Delegates to the default cube algorithm; same RNG path as built-in
toy_balanced_fn <- function(pik, aux = NULL, ...) {
  sondage::balanced_wor(pik, aux = aux)$sample
}

toy_spatial_balanced_fn <- function(pik, spread = NULL, ...) {
  if (is.null(spread) || !is.matrix(spread)) {
    stop("spread was not forwarded as a matrix")
  }
  order(spread[, 1]) |> head(round(sum(pik)))
}

test_that("custom balanced methods execute and match the built-in cube", {
  on.exit(sondage::unregister_method("test_cube"), add = TRUE)
  sondage::register_method(
    "test_cube", "balanced",
    sample_fn = toy_balanced_fn,
    probabilities = "exact"
  )

  s_custom <- sampling_design() |>
    draw(n = 10, method = "balanced_test_cube", mos = size) |>
    execute(custom_frame, seed = 42)
  s_builtin <- sampling_design() |>
    draw(n = 10, method = "cube", mos = size) |>
    execute(custom_frame, seed = 42)

  expect_identical(sort(s_custom$id), sort(s_builtin$id))
  expect_equal(s_custom$.weight, s_builtin$.weight)

  # aux balancing variables flow through
  s_aux <- sampling_design() |>
    draw(n = 10, method = "balanced_test_cube", mos = size, aux = prn_col) |>
    execute(custom_frame, seed = 7)
  s_aux_builtin <- sampling_design() |>
    draw(n = 10, method = "cube", mos = size, aux = prn_col) |>
    execute(custom_frame, seed = 7)
  expect_identical(sort(s_aux$id), sort(s_aux_builtin$id))
})

test_that("custom balanced methods support equal inclusion probabilities", {
  on.exit(sondage::unregister_method("test_cube"), add = TRUE)
  sondage::register_method(
    "test_cube", "balanced",
    sample_fn = toy_balanced_fn,
    probabilities = "exact"
  )

  s_custom <- sampling_design() |>
    draw(n = 10, method = "balanced_test_cube") |>
    execute(custom_frame, seed = 42)
  s_builtin <- sampling_design() |>
    draw(n = 10, method = "cube") |>
    execute(custom_frame, seed = 42)

  expect_identical(sort(s_custom$id), sort(s_builtin$id))
  expect_equal(s_custom$.weight, rep(10, 10))
})

test_that("clustered custom balanced methods receive aggregated aux", {
  on.exit(sondage::unregister_method("test_cluster_aux"), add = TRUE)
  received <- new.env(parent = emptyenv())
  capture_balanced_fn <- function(pik, aux = NULL, ...) {
    received$aux <- aux
    seq_len(as.integer(round(sum(pik))))
  }
  sondage::register_method(
    "test_cluster_aux", "balanced",
    sample_fn = capture_balanced_fn,
    supports_aux = TRUE,
    probabilities = "exact"
  )

  frame <- data.frame(
    id = seq_len(8),
    cluster = rep(letters[1:4], c(2, 3, 1, 2)),
    aux_value = c(1, 2, 10, 20, 30, 5, 7, 8)
  )

  sampling_design() |>
    cluster_by(cluster) |>
    draw(
      n = 2,
      method = "balanced_test_cluster_aux",
      aux = aux_value
    ) |>
    execute(frame, seed = 42)

  expect_equal(as.numeric(received$aux[, 1]), c(3, 60, 5, 15))
})

test_that("registered method prefixes agree with their declared family", {
  on.exit(sondage::unregister_method("test_cube"), add = TRUE)
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method(
    "test_cube", "balanced",
    sample_fn = toy_balanced_fn,
    probabilities = "exact"
  )
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  expect_error(
    sampling_design() |>
      draw(n = 10, method = "pps_test_cube"),
    "balanced_test_cube"
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "balanced_test_wor", mos = size),
    "pps_test_wor"
  )
})

test_that("custom spatial balanced methods receive spread coordinates", {
  on.exit(sondage::unregister_method("test_spread"), add = TRUE)
  sondage::register_method(
    "test_spread", "balanced",
    sample_fn = toy_spatial_balanced_fn,
    supports_aux = FALSE,
    supports_spread = TRUE,
    variance_family = "unsupported",
    probabilities = "exact"
  )

  result <- sampling_design() |>
    draw(
      n = 10,
      method = "balanced_test_spread",
      mos = size,
      spread = c(size, prn_col)
    ) |>
    execute(custom_frame, seed = 42)

  expect_equal(nrow(result), 10)
  expected_ids <- custom_frame$id[order(custom_frame$size)[seq_len(10)]]
  expect_equal(sort(result$id), sort(expected_ids))
})

test_that("custom balanced methods export like the built-in cube", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_cube"), add = TRUE)
  sondage::register_method(
    "test_cube", "balanced",
    sample_fn = toy_balanced_fn,
    probabilities = "exact"
  )

  s_custom <- sampling_design() |>
    draw(n = 10, method = "balanced_test_cube", mos = size) |>
    execute(custom_frame, seed = 42)

  # Classified as PPS WOR (Brewer), not equal-probability SRS: the
  # method_type "balanced" previously fell through to "equal_wor" and
  # exported an unequal-probability design with SRS variance.
  spec <- get_design(s_custom)$stages[[1]]$draw_spec
  expect_identical(samplyr:::survey_stage_kind(spec), "pps_wor")

  s_builtin <- sampling_design() |>
    draw(n = 10, method = "cube", mos = size) |>
    execute(custom_frame, seed = 42)
  se_custom <- survey::SE(survey::svytotal(~id, as_svydesign(s_custom)))
  se_builtin <- survey::SE(survey::svytotal(~id, as_svydesign(s_builtin)))
  expect_equal(as.numeric(se_custom), as.numeric(se_builtin))
})

test_that("custom balanced methods respect the two-stage limit", {
  on.exit(sondage::unregister_method("test_cube"), add = TRUE)
  sondage::register_method(
    "test_cube", "balanced",
    sample_fn = toy_balanced_fn,
    probabilities = "exact"
  )

  three_stage <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    draw(n = 5, method = "balanced_test_cube", mos = size) |>
    add_stage() |>
    draw(n = 3, method = "balanced_test_cube", mos = size) |>
    add_stage() |>
    draw(n = 2, method = "balanced_test_cube", mos = size)

  expect_error(
    execute(three_stage, custom_frame, seed = 1),
    "at most 2 stages"
  )
})

test_that("custom balanced methods count as WOR for frac validation", {
  on.exit(sondage::unregister_method("test_cube"), add = TRUE)
  sondage::register_method(
    "test_cube", "balanced",
    sample_fn = toy_balanced_fn,
    probabilities = "exact"
  )

  # draw-time scalar frac check
  expect_error(
    sampling_design() |> draw(frac = 1.5, method = "balanced_test_cube", mos = size),
    "cannot exceed 1"
  )

  # is_wor_method() classifies method_type "balanced" as WOR
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(
      frac = data.frame(region = c("North", "South"), frac = c(0.2, 0.2)),
      method = "balanced_test_cube",
      mos = size
    )
  expect_true(samplyr:::is_wor_method(design$stages[[1]]$draw_spec))

  # allocation-time check goes through is_wor_method(); mutate the
  # draw_spec to bypass draw-time validation and exercise it
  design$stages[[1]]$draw_spec$frac <- data.frame(
    region = c("North", "South"),
    frac = c(0.2, 1.1)
  )
  expect_error(
    execute(design, custom_frame, seed = 1),
    class = "samplyr_error_alloc_frac_wor_bounds"
  )
})

test_that("frac > 1 follows the declared type, not the method name", {
  on.exit(sondage::unregister_method("test_wr"), add = TRUE)
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method("test_wr", "wr", sample_fn = toy_wr_fn, probabilities = "exact")
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  # Custom WR methods allow frac > 1, like the built-in WR methods
  # (the name test alone misclassified every custom method as WOR)
  s <- sampling_design() |>
    draw(frac = 1.5, method = "pps_test_wr", mos = size) |>
    execute(custom_frame, seed = 4)
  expect_gt(nrow(s), nrow(custom_frame))
  expect_true(all(s$.fpc_1 == Inf))

  # Custom WOR methods still reject frac > 1
  expect_error(
    sampling_design() |> draw(frac = 1.5, method = "pps_test_wor", mos = size),
    "cannot exceed 1"
  )

  # Built-in behavior unchanged on both sides
  expect_error(
    sampling_design() |> draw(frac = 1.5, method = "srswor"),
    "cannot exceed 1"
  )
  s_builtin <- sampling_design() |>
    draw(frac = 1.5, method = "srswr") |>
    execute(custom_frame, seed = 4)
  expect_gt(nrow(s_builtin), nrow(custom_frame))
})

# --- Declared variance_family (sondage register_method) ---

test_that("draw() carries a declared variance_family into the draw_spec", {
  on.exit(sondage::unregister_method("test_pois"), add = TRUE)
  on.exit(sondage::unregister_method("test_wor"), add = TRUE)
  sondage::register_method(
    "test_pois", "wor",
    sample_fn = toy_prn_fn, fixed_size = FALSE,
    supports_prn = TRUE, variance_family = "poisson",
    probabilities = "exact"
  )
  sondage::register_method("test_wor", "wor", sample_fn = toy_wor_fn, probabilities = "exact")

  design <- sampling_design() |>
    draw(n = 20, method = "pps_test_pois", mos = size, prn = prn_col)
  expect_identical(design$stages[[1]]$draw_spec$method_variance, "poisson")

  # Undeclared methods carry NULL: consumers fall back to inference
  design_undeclared <- sampling_design() |>
    draw(n = 10, method = "pps_test_wor", mos = size)
  expect_null(design_undeclared$stages[[1]]$draw_spec$method_variance)
})

test_that("method_variance round-trips through design serialization", {
  on.exit(sondage::unregister_method("test_pois"), add = TRUE)
  sondage::register_method(
    "test_pois", "wor",
    sample_fn = toy_prn_fn, fixed_size = FALSE,
    supports_prn = TRUE, variance_family = "poisson",
    probabilities = "exact"
  )

  design <- sampling_design() |>
    draw(n = 20, method = "pps_test_pois", mos = size, prn = prn_col)

  restored <- read_design(design_json(design))
  expect_identical(
    restored$stages[[1]]$draw_spec$method_variance,
    "poisson"
  )
})

test_that("a declared variance_family overrides stage-kind inference", {
  kind_for <- function(...) {
    samplyr:::survey_stage_kind(samplyr:::new_draw_spec(...))
  }

  expect_identical(
    kind_for(
      method = "pps_x", method_type = "wor", method_fixed = TRUE,
      method_variance = "srs"
    ),
    "equal_wor"
  )
  expect_identical(
    kind_for(
      method = "pps_x", method_type = "wor", method_fixed = TRUE,
      method_variance = "pps_brewer"
    ),
    "pps_wor"
  )
  expect_identical(
    kind_for(
      method = "pps_x", method_type = "wor", method_fixed = FALSE,
      method_variance = "poisson"
    ),
    "rs_poisson"
  )
  expect_identical(
    kind_for(method = "pps_x", method_type = "wr", method_variance = "wr"),
    "wr"
  )
  expect_identical(
    kind_for(
      method = "pps_x", method_type = "wor", method_fixed = TRUE,
      method_variance = "unsupported"
    ),
    "unsupported"
  )

  # NULL and unknown values fall back to type/fixed inference
  expect_identical(
    kind_for(method = "pps_x", method_type = "wor", method_fixed = TRUE),
    "pps_wor"
  )
  expect_identical(
    kind_for(
      method = "pps_x", method_type = "wor", method_fixed = TRUE,
      method_variance = "not_a_family"
    ),
    "pps_wor"
  )
})

test_that("declared poisson family exports like the built-in pps_poisson", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_pois"), add = TRUE)
  sondage::register_method(
    "test_pois", "wor",
    sample_fn = toy_prn_fn, fixed_size = FALSE,
    supports_prn = TRUE, variance_family = "poisson",
    probabilities = "exact"
  )

  # Same PRN column -> identical Poisson samples for clone and built-in
  s_custom <- sampling_design() |>
    draw(n = 20, method = "pps_test_pois", mos = size, prn = prn_col) |>
    execute(custom_frame, seed = 7)
  s_builtin <- sampling_design() |>
    draw(n = 20, method = "pps_poisson", mos = size, prn = prn_col) |>
    execute(custom_frame, seed = 7)
  expect_identical(sort(s_custom$id), sort(s_builtin$id))

  # The declaration replaces the refusal with exact Poisson
  # linearization: the motivating bug (Brewer misclassification with a
  # near-zero SE) now ends in the right estimator instead of an error
  se_custom <- as.numeric(
    survey::SE(survey::svytotal(~id, as_svydesign(s_custom)))
  )
  se_builtin <- as.numeric(
    survey::SE(survey::svytotal(~id, as_svydesign(s_builtin)))
  )
  expect_gt(se_custom, 0)
  expect_equal(se_custom, se_builtin, tolerance = 1e-10)
})

test_that("declared unsupported family refuses linearization, keeps bootstrap", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_unsup"), add = TRUE)
  # Fixed-size WOR would be inferred as pps_wor (Brewer); the declared
  # family overrides even a safe-looking inference
  sondage::register_method(
    "test_unsup", "wor",
    sample_fn = toy_wor_fn, fixed_size = TRUE,
    variance_family = "unsupported",
    probabilities = "exact"
  )

  result <- sampling_design() |>
    draw(n = 20, method = "pps_test_unsup", mos = size) |>
    execute(custom_frame, seed = 8)

  expect_error(
    as_svydesign(result),
    class = "samplyr_error_custom_random_wor_export"
  )

  # Replicate escape hatch stays open
  rep_design <- as_svrepdesign(result, type = "subbootstrap", replicates = 20)
  expect_s3_class(rep_design, "svyrep.design")
})

test_that("declared srs family gets the equal-probability treatment", {
  skip_if_not_installed("survey")
  on.exit(sondage::unregister_method("test_srs"), add = TRUE)
  # mos is required for custom methods, but a constant mos gives equal
  # pik and the declared family says the SRS treatment applies
  toy_srs_fn <- function(pik, n = NULL, prn = NULL, ...) {
    sample.int(length(pik), size = n)
  }
  sondage::register_method(
    "test_srs", "wor",
    sample_fn = toy_srs_fn, fixed_size = TRUE,
    variance_family = "srs",
    probabilities = "exact"
  )

  frame_const <- transform(custom_frame, one = 1)
  result <- sampling_design() |>
    draw(n = 20, method = "pps_test_srs", mos = one) |>
    execute(frame_const, seed = 9)

  spec <- get_design(result)$stages[[1]]$draw_spec
  expect_identical(samplyr:::survey_stage_kind(spec), "equal_wor")

  # Count-scale FPC and no pps argument, like the built-in srswor
  svyd <- as_svydesign(result)
  expect_s3_class(svyd, "survey.design2")
  df <- as.data.frame(result)
  ref <- survey::svydesign(
    ids = ~1, weights = ~.weight_1, fpc = ~.fpc_1, data = df
  )
  expect_equal(
    as.numeric(survey::SE(survey::svytotal(~id, svyd))),
    as.numeric(survey::SE(survey::svytotal(~id, ref)))
  )
})
