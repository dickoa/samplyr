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
  n_obj <- svyplan::power_mean(effect = 5, var = 200)
  design <- sampling_design() |> draw(n = n_obj)
  frame <- data.frame(id = seq_len(500))
  result <- execute(design, frame, seed = 1)
  expect_equal(nrow(result), as.integer(n_obj))
})

test_that("draw accepts svyplan_cluster from n_cluster", {
  cl_obj <- svyplan::n_cluster(stage_cost = c(500, 50), icc = 0.05, budget = 100000)
  n_int <- prod(as.integer(cl_obj))
  design <- sampling_design() |> draw(n = cl_obj)
  frame <- data.frame(id = seq_len(n_int + 100))
  result <- execute(design, frame, seed = 1)
  expect_equal(nrow(result), n_int)
})

test_that("draw accepts svyplan n_alloc with per-stratum allocation", {
  alloc_frame <- data.frame(
    stratum = c("A", "B", "C"),
    N = c(200, 300, 500),
    sd = c(10, 15, 8)
  )
  alloc_obj <- svyplan::n_alloc(alloc_frame, n = 100, alloc = "neyman")
  n_expected <- alloc_obj$detail$n_int

  frame <- data.frame(
    id = seq_len(1000),
    stratum = rep(c("A", "B", "C"), times = c(200, 300, 500))
  )
  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = alloc_obj) |>
    execute(frame, seed = 1)

  expect_equal(nrow(result), sum(n_expected))
  for (i in seq_along(alloc_frame$stratum)) {
    lbl <- alloc_frame$stratum[i]
    expect_equal(sum(result$stratum == lbl), n_expected[i])
  }
})

test_that("coerce_svyplan_n returns named vector for n_alloc", {
  alloc_frame <- data.frame(
    stratum = c("X", "Y"),
    N = c(100, 200),
    sd = c(5, 10)
  )
  alloc_obj <- svyplan::n_alloc(alloc_frame, n = 50, alloc = "neyman")
  result <- samplyr:::coerce_svyplan_n(alloc_obj)

  expect_type(result, "integer")
  expect_named(result, c("X", "Y"))
  expect_equal(sum(result), 50L)
})

test_that("n_alloc with alloc in stratify_by errors", {
  alloc_frame <- data.frame(
    stratum = c("A", "B"),
    N = c(100, 200),
    sd = c(5, 10)
  )
  alloc_obj <- svyplan::n_alloc(alloc_frame, n = 50, alloc = "neyman")

  expect_error(
    sampling_design() |>
      stratify_by(stratum, alloc = "proportional") |>
      draw(n = alloc_obj),
    class = "samplyr_error_alloc_named_n_with_alloc"
  )
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

  n_alloc <- setNames(as.data.frame(bounds)$n, labels)

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

set.seed(1193)
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
  expect_s3_class(deff, "svyplan_deff")
  expect_true(deff >= 1)
})

test_that("samplyr re-exports the svyplan planning generics", {
  expect_identical(samplyr::design_effect, svyplan::design_effect)
  expect_identical(samplyr::effective_n, svyplan::effective_n)
  expect_identical(samplyr::varcomp, svyplan::varcomp)
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
  expect_equal(as.double(design_effect(fix_deff_disprop)), kish_deff)
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

test_that("SRSWOR gives deff = 1 and effective_n = n", {
  expect_equal(as.double(design_effect(fix_deff_srs)), 1)
  expect_equal(effective_n(fix_deff_srs), 50)
})

test_that("stratified proportional gives deff = 1", {
  expect_equal(as.double(design_effect(fix_deff_prop)), 1)
  expect_equal(effective_n(fix_deff_prop), nrow(fix_deff_prop))
})

test_that("stratified disproportionate Kish deff matches formula", {
  # w_A = 100/10 = 10, w_B = 100/40 = 2.5
  # sum(w) = 10*10 + 40*2.5 = 200
  # sum(w^2) = 10*100 + 40*6.25 = 1250
  # eff_n = 200^2 / 1250 = 32
  # deff = 50 / 32 = 1.5625
  expect_equal(as.double(design_effect(fix_deff_disprop)), 1.5625)
  expect_equal(effective_n(fix_deff_disprop), 32)
})

test_that("summary.tbl_sample reports deff correctly", {
  out <- capture.output(summary(fix_deff_disprop))
  expect_true(any(grepl("DEFF", out)))
})

test_that("svyplan_cluster feeds both stages of a clustered design", {
  cl <- svyplan::n_cluster(cv = 0.05, icc = 0.05, unit_relvar = 1,
                           stage_cost = c(500, 50))
  frame <- data.frame(
    ea = rep(sprintf("EA%03d", 1:200), each = 20),
    hh = 1:4000
  )
  res <- sampling_design() |>
    cluster_by(ea) |>
    draw(n = cl) |>
    add_stage() |>
    draw(n = cl) |>
    execute(frame, seed = 1)
  stages <- as.integer(cl)
  expect_equal(length(unique(res$ea)), stages[[1]])
  expect_equal(nrow(res), prod(stages))
})

test_that("svyplan_cluster keeps the operational total in a flat design", {
  cl <- svyplan::n_cluster(cv = 0.05, icc = 0.05, unit_relvar = 1,
                           stage_cost = c(500, 50))
  total <- prod(as.integer(cl))
  frame <- data.frame(id = seq_len(total + 100))
  res <- sampling_design() |> draw(n = cl) |> execute(frame, seed = 1)
  expect_equal(nrow(res), total)
})

test_that("svyplan_cluster errors past its planned stages", {
  cl <- svyplan::n_cluster(cv = 0.05, icc = 0.05, unit_relvar = 1,
                           stage_cost = c(500, 50))
  expect_error(
    sampling_design() |>
      cluster_by(ea) |> draw(n = 10) |>
      add_stage() |> draw(n = 5) |>
      add_stage() |> draw(n = cl),
    class = "samplyr_error_svyplan_stage"
  )
})

test_that("stratified two-stage n_alloc plan feeds both stages", {
  fr <- data.frame(
    stratum = c("North", "South"),
    N = c(2000, 2000),
    sd = c(14, 16),
    mean = c(58, 62),
    icc_psu = c(0.04, 0.06),
    cost_psu = c(400, 550),
    cost_ssu = c(45, 60)
  )
  plan <- svyplan::n_alloc(fr, cv = 0.02)
  d <- as.data.frame(plan)

  frame <- data.frame(
    region = rep(c("North", "South"), each = 2000),
    ea = rep(sprintf("EA%03d", 1:200), each = 20),
    hh = 1:4000
  )
  res <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(ea) |>
    draw(n = plan) |>
    add_stage() |>
    stratify_by(region) |>
    draw(n = plan) |>
    execute(frame, seed = 2)

  eas <- unique(res[, c("ea", "region")])
  expect_equal(
    as.integer(table(eas$region)[d$stratum]),
    d$n_psu_int
  )
  take <- as.integer(d$n_per_psu_int)
  expect_equal(
    as.integer(table(res$region)[d$stratum]),
    d$n_psu_int * take
  )
})

test_that("n_multi domain plans become per-domain data frames", {
  tg <- data.frame(
    name = "stunting",
    domain = c("urban", "rural"),
    p = c(0.25, 0.35),
    cv = 0.08
  )
  nm <- svyplan::n_multi(tg, domains = "domain")
  frame <- data.frame(
    domain = rep(c("urban", "rural"), each = 1000),
    id = 1:2000
  )
  res <- sampling_design() |>
    stratify_by(domain) |>
    draw(n = nm) |>
    execute(frame, seed = 1)
  expected <- setNames(as.integer(ceiling(nm$domains$.n)), nm$domains$domain)
  expect_equal(as.integer(table(res$domain)[names(expected)]),
               unname(expected))
})

test_that("n_multi_cluster domain plans feed both stages", {
  tg <- data.frame(
    name = "stunting",
    domain = c("urban", "rural"),
    p = c(0.25, 0.35),
    cv = 0.08,
    icc_psu = 0.05
  )
  nm <- svyplan::n_multi_cluster(
    tg,
    domains = "domain",
    stage_cost = c(500, 50)
  )
  frame <- data.frame(
    domain = rep(c("urban", "rural"), each = 3000),
    ea = rep(sprintf("EA%03d", 1:300), each = 20),
    hh = 1:6000
  )
  res <- sampling_design() |>
    stratify_by(domain) |> cluster_by(ea) |> draw(n = nm) |>
    add_stage() |> stratify_by(domain) |> draw(n = nm) |>
    execute(frame, seed = 2)
  eas <- unique(res[, c("ea", "domain")])
  expected <- setNames(as.integer(ceiling(nm$domains$n_psu)),
                       nm$domains$domain)
  expect_equal(as.integer(table(eas$domain)[names(expected)]),
               unname(expected))
})

test_that("named n without stage strata gets a targeted error", {
  expect_error(
    sampling_design() |>
      cluster_by(ea) |> draw(n = 10) |>
      add_stage() |> draw(n = c(North = 6, South = 7)),
    "requires stratification at this stage"
  )
})

test_that("named n with crossed strata errors at draw time", {
  expect_error(
    sampling_design() |>
      stratify_by(region, phc) |>
      draw(n = c(yes = 1, no = 2)),
    "single stratification variable"
  )
})

test_that("cluster-mode alloc plan requires cluster_by at stage 1", {
  fr <- data.frame(
    stratum = c("A", "B"), N = c(2000, 2000), sd = c(14, 16),
    mean = c(58, 62), icc_psu = c(0.04, 0.06),
    cost_psu = c(400, 550), cost_ssu = c(45, 60)
  )
  plan <- svyplan::n_alloc(fr, cv = 0.02)
  expect_error(
    sampling_design() |> stratify_by(region) |> draw(n = plan),
    class = "samplyr_error_svyplan_clustered_plan"
  )
})

test_that("cluster-mode alloc plan draws PSUs from an EA-level frame", {
  fr <- data.frame(
    stratum = c("North", "South"), N = c(2000, 2000), sd = c(14, 16),
    mean = c(58, 62), icc_psu = c(0.04, 0.06),
    cost_psu = c(400, 550), cost_ssu = c(45, 60)
  )
  plan <- svyplan::n_alloc(fr, cv = 0.02)
  d <- as.data.frame(plan)

  ea_frame <- data.frame(
    region = rep(c("North", "South"), each = 100),
    ea_id = 1:200,
    households = 20
  )
  res <- sampling_design() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = plan) |>
    execute(ea_frame, seed = 3)
  expect_equal(
    as.integer(table(res$region)[d$stratum]),
    d$n_psu_int
  )
})

test_that("an n_twophase() plan drives a two-phase design by subsampling fraction", {
  frame <- data.frame(
    stratum = c("A", "B", "C"),
    N = c(4000, 3000, 3000),
    sd = c(12, 25, 8),
    mean = c(40, 70, 35),
    unit_cost = c(2, 5, 1)
  )
  plan <- svyplan::n_twophase(frame, phase1_cost = 1, budget = 20000,
                              N = sum(frame$N))
  pop <- data.frame(
    unit_id = seq_len(sum(frame$N)),
    stratum = rep(frame$stratum, frame$N)
  )

  phase1 <- sampling_design() |>
    cluster_by(unit_id) |>
    draw(n = plan$operational$n[["n_phase1"]]) |>
    execute(pop, seed = 11)
  expect_equal(nrow(phase1), plan$operational$n[["n_phase1"]])

  # nu is a share of what phase 1 found, so phase 2 draws by fraction
  nu <- setNames(plan$detail$nu, plan$detail$stratum)
  phase2 <- sampling_design() |>
    stratify_by(stratum) |>
    cluster_by(unit_id) |>
    draw(frac = nu) |>
    execute(phase1, seed = 12)

  screened <- table(phase1$stratum)[names(nu)]
  taken <- table(phase2$stratum)[names(nu)]
  expect_equal(as.integer(taken), as.integer(ceiling(nu * screened)))

  # weights compound across the phases back to the frame
  expect_equal(sum(phase2$.weight), nrow(pop))

  # and the pair exports as a two-phase design, not a single-phase one
  des <- as_svydesign(phase2)
  expect_s3_class(des, "twophase2")
})

test_that("a two-phase design needs a shared identifier to export", {
  pop <- data.frame(unit_id = 1:2000, stratum = rep(c("A", "B"), each = 1000))
  phase1 <- sampling_design() |>
    draw(n = 400) |>
    execute(pop, seed = 3)
  phase2 <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = c(A = 0.5, B = 0.25)) |>
    execute(phase1, seed = 4)
  expect_error(as_svydesign(phase2), "shared phase identifiers")
})
