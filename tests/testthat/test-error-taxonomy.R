test_that("selector helpers are rejected at design specification time", {
  expect_error(
    sampling_design() |>
      stratify_by(starts_with("zzz")),
    "bare column names"
  )

  expect_error(
    sampling_design() |>
      stratify_by(all_of(missing_selector_vec)),
    "bare column names"
  )
})

test_that("auxiliary validation errors use standardized classes", {
  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "neyman",
        variance = data.frame(region = c("A", "A"), var = c(1, 2))
      ),
    class = "samplyr_error_aux_duplicate_keys"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "neyman",
        variance = data.frame(region = c("A", NA), var = c(1, 2))
      ),
    class = "samplyr_error_aux_missing_key_values"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "neyman",
        variance = data.frame(region = c("A", "B"), var = c(1, Inf))
      ),
    class = "samplyr_error_aux_non_finite_values"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = data.frame(region = c("A", "B"), cv = c(0.2, -0.1)),
        importance = data.frame(region = c("A", "B"), importance = c(10, 20))
      ),
    class = "samplyr_error_aux_cv_bounds"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = data.frame(region = c("A", "B"), cv = c(0.2, 0.1)),
        importance = data.frame(region = c("A", "B"), importance = c(10, 0))
      ),
    class = "samplyr_error_aux_importance_bounds"
  )
})

test_that("execution allocation/coverage errors use standardized classes", {
  frame <- data.frame(
    id = 1:60,
    region = rep(c("A", "B", "C"), each = 20),
    stringsAsFactors = FALSE
  )

  design_missing_aux <- sampling_design() |>
    stratify_by(
      region,
      alloc = "neyman",
      variance = data.frame(region = c("A", "B"), var = c(1, 2))
    ) |>
    draw(n = 15)

  expect_error(
    execute(design_missing_aux, frame, seed = 1),
    class = "samplyr_error_aux_missing_coverage"
  )

  design_bad_n <- sampling_design() |>
    stratify_by(region) |>
    draw(n = data.frame(region = c("A", "B", "C"), n = c(2, 2, 2)))

  design_bad_n$stages[[1]]$draw_spec$n <- data.frame(
    region = c("A", "A", "B", "C"),
    n = c(3, 4, 5, 6)
  )

  expect_error(
    execute(design_bad_n, frame, seed = 1),
    class = "samplyr_error_alloc_duplicate_keys"
  )

  design_bad_frac <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = data.frame(region = c("A", "B", "C"), frac = c(0.1, 0.2, 0.3)))

  design_bad_frac$stages[[1]]$draw_spec$frac <- data.frame(
    region = c("A", "B", "C"),
    frac = c(0.5, 1.1, 0.4)
  )

  expect_error(
    execute(design_bad_frac, frame, seed = 1),
    class = "samplyr_error_alloc_frac_wor_bounds"
  )
})

test_that("non-finite allocation target errors use standardized class", {
  frame <- data.frame(
    id = 1:60,
    region = rep(c("A", "B"), each = 30),
    stringsAsFactors = FALSE
  )

  design_zero_factor <- sampling_design() |>
    stratify_by(
      region,
      alloc = "neyman",
      variance = data.frame(region = c("A", "B"), var = c(0, 0))
    ) |>
    draw(n = 20)

  expect_error(
    execute(design_zero_factor, frame, seed = 1),
    class = "samplyr_error_alloc_target_non_finite"
  )
})

test_that("alloc + frac validation uses standardized class", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(frac = 0.1),
    class = "samplyr_error_alloc_frac_with_alloc"
  )
})

test_that("replicate export errors use standardized classes", {
  skip_if_not_installed("survey")

  pps_sample <- sampling_design() |>
    draw(n = 20, method = "pps_brewer", mos = households) |>
    execute(bfa_eas, seed = 1)

  # PPS + non-safe type emits warning (no longer a hard error)
  expect_warning(
    tryCatch(
      as_svrepdesign(pps_sample, type = "bootstrap"),
      samplyr_error_svrep_conversion_failed = function(e) NULL
    ),
    "may not work for PPS"
  )

  frame <- data.frame(id = 1:120, x = rnorm(120))
  design <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 60)
  phase1 <- execute(design, frame, seed = 1)
  phase2 <- execute(design, phase1, seed = 2)

  expect_error(
    as_svrepdesign(phase2),
    class = "samplyr_error_svrep_twophase_unsupported"
  )
})
