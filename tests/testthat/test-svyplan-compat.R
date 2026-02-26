test_that("draw accepts svyplan_n from n_prop", {
  n_obj <- svyplan::n_prop(p = 0.5, moe = 0.1)
  design <- sampling_design() |> draw(n = n_obj)
  frame <- data.frame(id = seq_len(500))
  result <- execute(design, frame, seed = 1)
  expect_equal(nrow(result), as.integer(n_obj))
})

test_that("draw accepts svyplan_n from n_mean", {
  n_obj <- svyplan::n_mean(var = 100, moe = 5)
  design <- sampling_design() |> draw(n = n_obj)
  frame <- data.frame(id = seq_len(500))
  result <- execute(design, frame, seed = 1)
  expect_equal(nrow(result), as.integer(n_obj))
})

test_that("draw accepts svyplan_n from n_multi", {
  targets <- data.frame(
    name = c("a", "b"),
    p = c(0.3, 0.1),
    moe = c(0.05, 0.03)
  )
  n_obj <- svyplan::n_multi(targets)
  design <- sampling_design() |> draw(n = n_obj)
  frame <- data.frame(id = seq_len(500))
  result <- execute(design, frame, seed = 1)
  expect_equal(nrow(result), as.integer(n_obj))
})

test_that("draw accepts svyplan_power from power_prop", {
  n_obj <- svyplan::power_prop(p1 = 0.5, p2 = 0.6, power = 0.8)
  n_int <- as.integer(n_obj)
  design <- sampling_design() |> draw(n = n_obj)
  frame <- data.frame(id = seq_len(n_int + 100))
  result <- execute(design, frame, seed = 1)
  expect_equal(nrow(result), n_int)
})

test_that("draw accepts svyplan_power from power_mean", {
  n_obj <- svyplan::power_mean(delta = 5, var = 200)
  design <- sampling_design() |> draw(n = n_obj)
  frame <- data.frame(id = seq_len(500))
  result <- execute(design, frame, seed = 1)
  expect_equal(nrow(result), as.integer(n_obj))
})

test_that("draw accepts svyplan_cluster from n_cluster", {
  cl_obj <- svyplan::n_cluster(cost = c(500, 50), delta = 0.05, budget = 100000)
  n_int <- as.integer(cl_obj)
  design <- sampling_design() |> draw(n = cl_obj)
  frame <- data.frame(id = seq_len(n_int + 100))
  result <- execute(design, frame, seed = 1)
  expect_equal(nrow(result), n_int)
})

test_that("non-svyplan n values pass through unchanged", {
  expect_equal(samplyr:::coerce_svyplan_n(50L), 50L)
  expect_equal(samplyr:::coerce_svyplan_n(50), 50)
  df <- data.frame(stratum = "A", n = 10)
  expect_identical(samplyr:::coerce_svyplan_n(df), df)
})

test_that("svyplan prec_prop round-trip preserves n", {
  s <- svyplan::n_prop(p = 0.3, moe = 0.05)
  p <- svyplan::prec_prop(s)
  s2 <- svyplan::n_prop(p)
  expect_equal(as.integer(s), as.integer(s2))
})

test_that("svyplan n_prop with resp_rate inflates sample size", {
  n_base <- svyplan::n_prop(p = 0.3, moe = 0.05)
  n_resp <- svyplan::n_prop(p = 0.3, moe = 0.05, resp_rate = 0.8)
  expect_true(as.integer(n_resp) > as.integer(n_base))
})

test_that("svyplan predict returns sensitivity data frame", {
  x <- svyplan::n_prop(p = 0.3, moe = 0.05, deff = 1.5)
  result <- predict(x, data.frame(deff = c(1.0, 2.0)))
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 2)
  expect_true("n" %in% names(result))
})

test_that("strata_bound + predict + n_h workflow", {
  set.seed(1)
  x <- rlnorm(500, 5, 1.2)
  bounds <- svyplan::strata_bound(
    x,
    n_strata = 3,
    method = "cumrootf",
    cv = 0.05
  )
  labels <- paste0("S", 1:3)

  frame <- data.frame(id = seq_len(500), revenue = x)
  frame$stratum <- predict(bounds, frame$revenue, labels = labels)

  n_alloc <- setNames(bounds$strata$n_h, labels)

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = n_alloc) |>
    execute(frame, seed = 1)

  expect_equal(nrow(result), sum(n_alloc))
  expect_true(all(labels %in% result$stratum))
  for (lbl in labels) {
    expect_equal(sum(result$stratum == lbl), n_alloc[[lbl]])
  }
})

set.seed(42)
deff_frame <- data.frame(
  id = 1:200,
  stratum = rep(c("A", "B"), each = 100),
  cluster = rep(1:40, each = 5),
  income = rnorm(200, 50, 10),
  census_pop = rnorm(200, 100, 20)
)

# Disproportionate stratified: w_A = 100/10 = 10, w_B = 100/40 = 2.5
fix_deff_disprop <- sampling_design() |>
  stratify_by(stratum) |>
  draw(n = c(A = 10, B = 40)) |>
  execute(deff_frame, seed = 1)

# Proportional stratified: equal weights
fix_deff_prop <- sampling_design() |>
  stratify_by(stratum, alloc = "proportional") |>
  draw(n = 50) |>
  execute(deff_frame, seed = 1)

# SRSWOR: equal weights, no strata
fix_deff_srs <- sampling_design() |>
  draw(n = 50) |>
  execute(deff_frame, seed = 1)

# Two-stage: stratified + clustered
fix_deff_strat_clust <- sampling_design() |>
  add_stage("psu") |>
  stratify_by(stratum) |>
  cluster_by(cluster) |>
  draw(n = c(A = 10, B = 10)) |>
  add_stage("ssu") |>
  draw(n = 3) |>
  execute(deff_frame, seed = 1)

# Two-stage: clustered only (no strata)
fix_deff_clust <- sampling_design() |>
  add_stage("psu") |>
  cluster_by(cluster) |>
  draw(n = 20) |>
  add_stage("ssu") |>
  draw(n = 3) |>
  execute(deff_frame, seed = 1)

test_that("design_effect dispatches on tbl_sample", {
  deff <- design_effect(fix_deff_disprop)
  expect_type(deff, "double")
  expect_true(deff >= 1)
})

test_that("effective_n dispatches on tbl_sample", {
  eff <- effective_n(fix_deff_disprop)
  expect_type(eff, "double")
  expect_true(eff > 0)
  expect_true(eff <= nrow(fix_deff_disprop))
})

test_that("design_effect.tbl_sample matches Kish formula", {
  w <- fix_deff_disprop$.weight
  kish_deff <- length(w) / (sum(w)^2 / sum(w^2))
  expect_equal(design_effect(fix_deff_disprop), kish_deff)
})

test_that("effective_n.tbl_sample matches Kish formula", {
  w <- fix_deff_disprop$.weight
  kish_eff <- sum(w)^2 / sum(w^2)
  expect_equal(effective_n(fix_deff_disprop), kish_eff)
})

test_that("design_effect.tbl_sample errors without .weight", {
  no_weight <- fix_deff_srs
  no_weight[[".weight"]] <- NULL
  expect_error(design_effect(no_weight), "\\.weight")
})

test_that("Henry accepts y and x_cal as bare column names", {
  deff_tidy <- design_effect(
    fix_deff_disprop,
    y = income,
    x_cal = census_pop,
    method = "henry"
  )
  deff_vec <- design_effect(
    fix_deff_disprop$.weight,
    y = fix_deff_disprop$income,
    x_cal = fix_deff_disprop$census_pop,
    method = "henry"
  )
  expect_equal(deff_tidy, deff_vec)
})

test_that("Spencer accepts y as bare column name", {
  deff <- design_effect(fix_deff_disprop, y = income, method = "spencer")
  expect_true(is.finite(deff))
})

test_that("Spencer auto-extracts p from .weight_1", {
  deff_auto <- design_effect(fix_deff_disprop, y = income, method = "spencer")
  deff_manual <- design_effect(
    fix_deff_disprop$.weight,
    y = fix_deff_disprop$income,
    p = 1 / fix_deff_disprop$.weight_1,
    method = "spencer"
  )
  expect_equal(deff_auto, deff_manual)
})

test_that("effective_n with Henry uses bare column names", {
  eff <- effective_n(
    fix_deff_disprop,
    y = income,
    x_cal = census_pop,
    method = "henry"
  )
  expect_true(is.finite(eff))
  expect_true(eff > 0)
})

test_that("CR auto-extracts strvar and stages from stratified design", {
  cr_auto <- design_effect(fix_deff_disprop, y = income, method = "cr")
  cr_manual <- design_effect(
    fix_deff_disprop$.weight,
    y = fix_deff_disprop$income,
    strvar = fix_deff_disprop$stratum,
    stages = c(1L, 1L),
    method = "cr"
  )
  expect_equal(cr_auto$overall, cr_manual$overall)
})

test_that("CR auto-extracts strvar, clvar, stages from stratified clustered design", {
  cr_auto <- design_effect(fix_deff_strat_clust, y = income, method = "cr")
  expect_type(cr_auto, "list")
  expect_true("deff_c" %in% names(cr_auto$strata))
  expect_true("deff_s" %in% names(cr_auto$strata))
  expect_equal(nrow(cr_auto$strata), 2)
})

test_that("CR auto-extracts clvar from unstratified clustered design", {
  cr_auto <- design_effect(fix_deff_clust, y = income, method = "cr")
  expect_type(cr_auto, "list")
  expect_true("deff_c" %in% names(cr_auto$strata))
})

test_that("effective_n with CR uses auto-extraction", {
  eff <- effective_n(fix_deff_strat_clust, y = income, method = "cr")
  expect_type(eff, "double")
  expect_true(eff > 0)
})

test_that("SRSWOR gives deff = 1 and effective_n = n", {
  expect_equal(design_effect(fix_deff_srs), 1)
  expect_equal(effective_n(fix_deff_srs), 50)
})

test_that("stratified proportional gives deff = 1", {
  expect_equal(design_effect(fix_deff_prop), 1)
  expect_equal(effective_n(fix_deff_prop), nrow(fix_deff_prop))
})

test_that("stratified disproportionate Kish deff matches formula", {
  # w_A = 100/10 = 10, w_B = 100/40 = 2.5
  # sum(w) = 10*10 + 40*2.5 = 200
  # sum(w^2) = 10*100 + 40*6.25 = 1250
  # eff_n = 200^2 / 1250 = 32
  # deff = 50 / 32 = 1.5625
  expect_equal(design_effect(fix_deff_disprop), 1.5625)
  expect_equal(effective_n(fix_deff_disprop), 32)
})

test_that("CR on stratified without clustering has no deff_c", {
  cr <- design_effect(fix_deff_disprop, y = income, method = "cr")
  expect_true("deff_w" %in% names(cr$strata))
  expect_true("deff_s" %in% names(cr$strata))
  expect_false("deff_c" %in% names(cr$strata))
})

test_that("CR on proportional stratified has deff_w = 1 per stratum", {
  cr <- design_effect(fix_deff_prop, y = income, method = "cr")
  expect_true(all(cr$strata$deff_w == 1))
})

test_that("CR on disproportionate stratified has deff_w = 1 per stratum but overall > 1", {
  cr <- design_effect(fix_deff_disprop, y = income, method = "cr")
  # Within each stratum weights are equal, so deff_w = 1
  expect_true(all(cr$strata$deff_w == 1))
  # But deff_s captures the between-stratum weighting effect
  expect_true(cr$overall > 1)
})

test_that("CR on clustered with high ICC gives large deff_c", {
  set.seed(42)
  cluster_means <- rnorm(40, 50, 20)
  hi_frame <- data.frame(
    id = 1:200,
    cluster = rep(1:40, each = 5),
    income = rep(cluster_means, each = 5) + rnorm(200, 0, 2)
  )
  samp <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(cluster) |>
    draw(n = 12) |>
    add_stage("ssu") |>
    draw(n = 5) |>
    execute(hi_frame, seed = 1)
  cr <- design_effect(samp, y = income, method = "cr")
  expect_true(cr$strata$rho > 0.5)
  expect_true(cr$strata$deff_c > 1.5)
  expect_equal(cr$strata$deff_w, 1)
})

test_that("CR on clustered with low ICC gives deff_c near 1", {
  set.seed(42)
  lo_frame <- data.frame(
    id = 1:200,
    cluster = rep(1:40, each = 5),
    income = rnorm(200, 50, 20)
  )
  samp <- sampling_design() |>
    add_stage("psu") |>
    cluster_by(cluster) |>
    draw(n = 12) |>
    add_stage("ssu") |>
    draw(n = 5) |>
    execute(lo_frame, seed = 1)
  cr <- design_effect(samp, y = income, method = "cr")
  expect_true(abs(cr$strata$rho) < 0.3)
  expect_true(cr$strata$deff_c < 1.5)
})

test_that("CR on stratified + clustered returns full decomposition", {
  set.seed(42)
  cluster_means <- rnorm(40, 50, 15)
  sc_frame <- data.frame(
    id = 1:200,
    stratum = rep(c("A", "B"), each = 100),
    cluster = rep(1:40, each = 5),
    income = rep(cluster_means, each = 5) + rnorm(200, 0, 3)
  )
  samp <- sampling_design() |>
    add_stage("psu") |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = c(A = 10, B = 10)) |>
    add_stage("ssu") |>
    draw(n = 3) |>
    execute(sc_frame, seed = 1)
  cr <- design_effect(samp, y = income, method = "cr")
  expect_true(all(c("deff_w", "deff_c", "deff_s") %in% names(cr$strata)))
  expect_equal(nrow(cr$strata), 2)
  expect_true(all(cr$strata$deff_w > 0))
  expect_true(all(cr$strata$deff_c > 0))
  expect_true(all(cr$strata$deff_s > 0))
  expect_true(all(cr$strata$rho_h > 0))
  eff <- effective_n(samp, y = income, method = "cr")
  expect_equal(eff, nrow(samp) / cr$overall)
})

test_that("Henry returns finite deff on unequal-weight sample", {
  deff <- design_effect(
    fix_deff_disprop,
    y = income,
    x_cal = census_pop,
    method = "henry"
  )
  expect_true(is.finite(deff))
})

test_that("Spencer returns finite deff on unequal-weight sample", {
  deff <- design_effect(fix_deff_disprop, y = income, method = "spencer")
  expect_true(is.finite(deff))
})

test_that("CR on unstratified unclustered design gives informative error", {
  expect_error(
    design_effect(fix_deff_srs, y = income, method = "cr"),
    "CR method requires stratification or clustering"
  )
})

test_that("summary.tbl_sample reports deff correctly", {
  out <- capture.output(summary(fix_deff_disprop))
  expect_true(any(grepl("DEFF", out)))
})
