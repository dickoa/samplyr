## Declaring a reserve in draw()

two_stage_reserve <- function(reserve = 2, n = 4, method = "srswor") {
  sampling_design() |>
    add_stage() |>
    cluster_by(ea_id) |>
    draw(n = 3) |>
    add_stage() |>
    draw(n = n, reserve = reserve, method = method)
}

test_that("draw() stores a reserve as an integer and prints it", {
  design <- two_stage_reserve(reserve = 2)
  expect_identical(design$stages[[2]]$draw_spec$reserve, 2L)
  expect_null(design$stages[[1]]$draw_spec$reserve)
  expect_output(print(design), "n = 4 (per ea_id), method = srswor, reserve = 2",
                fixed = TRUE)
  expect_identical(
    two_stage_reserve(method = "systematic")$stages[[2]]$draw_spec$reserve,
    2L
  )
})

test_that("draw() refuses a reserve that is not a whole number of at least 1", {
  for (bad in list(0, 1.5, -1, NA_real_, c(1, 2), "2", -Inf, NaN, 3e9)) {
    expect_error(
      sampling_design() |> draw(n = 4, reserve = bad),
      class = "samplyr_error_draw_argument"
    )
  }
})

test_that("draw() refuses a reserve under a method other than srswor or systematic", {
  expect_error(
    sampling_design() |> draw(n = 4, reserve = 2, method = "bernoulli"),
    class = "samplyr_error_reserve_method"
  )
  expect_error(
    sampling_design() |>
      draw(n = 4, reserve = 2, method = "pps_brewer", mos = households),
    class = "samplyr_error_reserve_method"
  )
})

test_that("draw() refuses a reserve with a take below 2", {
  expect_error(
    sampling_design() |> draw(n = 1, reserve = 2),
    "at least 2",
    class = "samplyr_error_draw_argument"
  )
  expect_error(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = c(Centre = 3, Cascades = 1), reserve = 2),
    "at least 2",
    class = "samplyr_error_draw_argument"
  )
  # A rate gives the take only at execution.
  expect_no_error(sampling_design() |> draw(frac = 0.1, reserve = 2))
})

test_that("a take below 2 from a rate or an allocation is refused before a draw", {
  frame <- data.frame(st = rep(c("A", "B"), c(5, 20)), id = 1:25)
  by_rate <- sampling_design() |> stratify_by(st) |> draw(frac = 0.2, reserve = 2)
  allocated <- sampling_design() |>
    stratify_by(st, alloc = "proportional") |>
    draw(n = 5, reserve = 2)
  for (design in list(by_rate, allocated)) {
    for (run in list(validate_frame, execute, frame_summary)) {
      expect_error(
        run(design, frame),
        "the take is below 2 in 1 of 2 candidate pools",
        class = "samplyr_error_draw_argument"
      )
    }
  }
  expect_true(validate_frame(
    sampling_design() |> stratify_by(st) |> draw(frac = 0.4, reserve = 2),
    frame
  ))

  # A later stage is judged on every parent before stage 1 is drawn.
  hierarchy <- data.frame(ea = rep(1:4, c(5, 20, 20, 20)), hh = 1:65)
  two_stage <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 2) |>
    add_stage() |> draw(frac = 0.2, reserve = 2)
  for (run in list(validate_frame, execute)) {
    expect_error(
      run(two_stage, hierarchy),
      "Stage 2: .* below 2 in 1 of 4 candidate pools",
      class = "samplyr_error_draw_argument"
    )
  }
})

test_that("only one stage of a design can carry a reserve", {
  expect_error(
    sampling_design() |>
      add_stage(label = "EA") |>
      cluster_by(ea_id) |>
      draw(n = 3, reserve = 1) |>
      add_stage(label = "HH") |>
      draw(n = 4, reserve = 2),
    "EA.*HH",
    class = "samplyr_error_draw_argument"
  )
})

test_that("a plan inflated for losses the reserve covers warns", {
  plan_message <- function(design) {
    w <- expect_warning(design, class = "samplyr_warning_reserve_plan_rates")
    gsub("\\s+", " ", cli::ansi_strip(conditionMessage(w)))
  }
  net <- svyplan::n_prop(p = 0.3, moe = 0.1)
  expect_no_warning(sampling_design() |> draw(n = net, reserve = 3))

  gross <- svyplan::n_prop(p = 0.3, moe = 0.1, resp_rate = 0.8)
  expect_no_warning(sampling_design() |> draw(n = gross))
  msg <- plan_message(sampling_design() |> draw(n = gross, reserve = 3))
  expect_match(msg, "resp_rate = 0.8", fixed = TRUE)
  expect_match(msg, "with a rate below 1", fixed = TRUE)
  expect_match(msg, "The reserve also covers nonresponse at this stage, so",
               fixed = TRUE)
  expect_no_match(msg, "ineligible", fixed = TRUE)
  expect_match(
    msg,
    "Keep `resp_rate` if it covers nonresponse within responding units.$"
  )
  expect_no_match(msg, "eligibility_rate", fixed = TRUE)

  # svyplan stores eligibility_rate in params only when it is below 1.
  net$params$eligibility_rate <- 0.9
  msg <- plan_message(sampling_design() |> draw(n = net, reserve = 3))
  expect_match(msg, "eligibility_rate = 0.9", fixed = TRUE)
  expect_match(msg, "The reserve also replaces ineligible units, so",
               fixed = TRUE)
  expect_no_match(msg, "resp_rate|persons|nonresponse")

  both <- svyplan::n_prop(p = 0.3, moe = 0.1, resp_rate = 0.8,
                          eligibility_rate = 0.9)
  msg <- plan_message(sampling_design() |> draw(n = both, reserve = 3))
  expect_match(msg, "with rates below 1", fixed = TRUE)
  expect_match(
    msg,
    "eligibility_rate = 0.9` and `resp_rate = 0.8`. The reserve also replaces ineligible units and covers nonresponse at this stage, so",
    fixed = TRUE
  )
  expect_match(msg, "these rates at 1", fixed = TRUE)
  expect_match(
    msg,
    "within responding units. The advice still holds for `eligibility_rate`.$"
  )

  # A take of PSUs is inflated by the PSU rate only. n_cluster() keeps one
  # rate per stage.
  cluster <- svyplan::n_cluster(
    stage_cost = c(500, 50), icc = 0.05, cv = 0.05,
    resp_rate = c(0.9, 0.8)
  )
  msg <- plan_message(
    sampling_design() |> add_stage() |> cluster_by(ea_id) |>
      draw(n = cluster, reserve = 3)
  )
  expect_match(msg, "resp_rate_1 = 0.9", fixed = TRUE)
  expect_no_match(msg, "resp_rate = 0.8", fixed = TRUE)
  # A rate above the last stage covers units, not persons within them.
  expect_no_match(msg, "Keep", fixed = TRUE)
  msg <- plan_message(
    sampling_design() |> add_stage() |> cluster_by(ea_id) |> draw(n = 3) |>
      add_stage() |> draw(n = cluster, reserve = 3)
  )
  expect_match(msg, "resp_rate = 0.8", fixed = TRUE)
  expect_match(msg, "covers nonresponse within responding units.$")
  expect_no_match(msg, "resp_rate_1", fixed = TRUE)

  # Custom stage names do not change the rate names, as for an allocation.
  named <- svyplan::n_cluster(
    stage_cost = c(ea = 500, hh = 50), icc = 0.05, cv = 0.05,
    resp_rate = c(0.9, 0.8)
  )
  msg <- plan_message(
    sampling_design() |> add_stage() |> cluster_by(ea_id) |>
      draw(n = named, reserve = 3)
  )
  expect_match(msg, "resp_rate_1 = 0.9", fixed = TRUE)

  # A rate of 1 at the stage at hand is no inflation, whatever the others.
  early <- svyplan::n_cluster(
    stage_cost = c(500, 50), icc = 0.05, cv = 0.05,
    resp_rate = c(1, 0.8)
  )
  expect_no_warning(
    sampling_design() |> add_stage() |> cluster_by(ea_id) |>
      draw(n = early, reserve = 3)
  )
  ones <- svyplan::n_cluster(stage_cost = c(500, 50), icc = 0.05, cv = 0.05)
  expect_no_warning(
    sampling_design() |> add_stage() |> cluster_by(ea_id) |> draw(n = 3) |>
      add_stage() |> draw(n = ones, reserve = 3)
  )
})

test_that("an allocation's rates are read per stratum and per stage", {
  plan_message <- function(design) {
    w <- expect_warning(design, class = "samplyr_warning_reserve_plan_rates")
    cli::ansi_strip(conditionMessage(w))
  }
  frame <- data.frame(
    stratum = c("A", "B"), N = c(4000, 3000), sd = c(10, 15), mean = c(50, 60)
  )

  # n_alloc() keeps one rate per stratum. The lowest is named, so a first
  # stratum at a high rate cannot hide it.
  plan <- svyplan::n_alloc(frame, cv = 0.02, resp_rate = c(0.9, 0.6))
  msg <- plan_message(
    sampling_design() |> stratify_by(stratum) |> draw(n = plan, reserve = 3)
  )
  expect_match(msg, "resp_rate = 0.6` in stratum B", fixed = TRUE)
  expect_no_match(msg, "0.9", fixed = TRUE)
  plan <- svyplan::n_alloc(frame, cv = 0.02, resp_rate = c(1, 0.7))
  msg <- plan_message(
    sampling_design() |> stratify_by(stratum) |> draw(n = plan, reserve = 3)
  )
  expect_match(msg, "resp_rate = 0.7` in stratum B", fixed = TRUE)
  plan <- svyplan::n_alloc(frame, cv = 0.02)
  expect_no_warning(
    sampling_design() |> stratify_by(stratum) |> draw(n = plan, reserve = 3)
  )

  # A two-stage allocation sets its PSU rate as a frame column.
  clustered <- transform(
    frame, icc_1 = 0.05, cost_1 = 400, cost_2 = 40,
    resp_rate_1 = c(1, 0.85)
  )
  plan <- svyplan::n_alloc(clustered, cv = 0.02)
  expect_identical(plan$stages, 2L)
  msg <- plan_message(
    sampling_design() |> stratify_by(stratum) |> cluster_by(ea) |>
      draw(n = plan, reserve = 3)
  )
  expect_match(msg, "resp_rate_1 = 0.85` in stratum B", fixed = TRUE)
  expect_no_warning(
    sampling_design() |> stratify_by(stratum) |> cluster_by(ea) |>
      draw(n = 3) |>
      add_stage() |> stratify_by(stratum) |> draw(n = plan, reserve = 3)
  )

  # A joint allocation can override the rate per stratum and indicator.
  joint_frame <- data.frame(
    stratum = c("A", "B"), N = c(20000, 8000), N_1 = c(400, 160),
    cost_1 = 400, cost_2 = 40, take_2 = c(12, 8)
  )
  measures <- data.frame(
    stratum = c("A", "B", "A", "B"),
    name = rep(c("coverage", "anc"), each = 2),
    p = c(0.5, 0.4, 0.6, 0.7), icc_1 = 0.05,
    resp_rate = c(1, 1, 1, 0.65)
  )
  plan <- svyplan::n_alloc_joint(
    joint_frame, measures,
    targets = data.frame(name = c("coverage", "anc"), cv = 0.10)
  )
  msg <- plan_message(
    sampling_design() |> stratify_by(stratum) |> cluster_by(ea) |>
      draw(n = 3) |>
      add_stage() |> stratify_by(stratum) |> draw(n = plan, reserve = 3)
  )
  expect_match(
    msg, "resp_rate = 0.65` in stratum B for indicator anc", fixed = TRUE
  )
  expect_no_warning(
    sampling_design() |> stratify_by(stratum) |> cluster_by(ea) |>
      draw(n = plan, reserve = 3)
  )
})

test_that("a register plan's take stage reads the plan's final rate", {
  plan <- svyplan::n_alloc_joint(
    data.frame(stratum = c("A", "B"), N = c(2000, 1600), take_2 = 10),
    data.frame(stratum = c("A", "B"), name = "y", p = c(0.5, 0.4),
               icc_1 = 0.05),
    targets = data.frame(name = "y", cv = 0.10),
    psu = certainty_plan_register(), resp_rate = 0.8
  )
  stage1 <- sampling_design() |>
    stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N)
  w <- expect_warning(
    stage1 |> add_stage() |> draw(n = plan, reserve = 2),
    class = "samplyr_warning_reserve_plan_rates"
  )
  expect_match(conditionMessage(w), "resp_rate = 0.8", fixed = TRUE)
  # Stage 1 of a register plan still takes no reserve.
  expect_error(
    sampling_design() |>
      stratify_by(stratum) |> cluster_by(psu_id) |>
      draw(n = plan, method = "pps_brewer", mos = N, reserve = 2),
    class = "samplyr_error_reserve_method"
  )
  # The same register at a response rate of 1 needs no warning.
  unit_rate <- certainty_plan_fixture()
  expect_no_warning(
    sampling_design() |>
      stratify_by(stratum) |> cluster_by(psu_id) |>
      draw(n = unit_rate, method = "pps_brewer", mos = N) |>
      add_stage() |> draw(n = unit_rate, reserve = 2)
  )
})

test_that("a design without a reserve writes no reserve field", {
  design <- sampling_design() |> draw(n = 4)
  expect_null(as.list(design)$stages[[1]]$draw$reserve)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(design, path)
  expect_false(any(grepl('"reserve"', readLines(path), fixed = TRUE)))
})

test_that("a reserve survives as.list(), a write and a read", {
  design <- two_stage_reserve(reserve = 5)
  expect_identical(as.list(design)$stages[[2]]$draw$reserve, 5L)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(design, path)
  restored <- read_design(path)
  expect_identical(restored$stages[[2]]$draw_spec$reserve, 5L)
  expect_null(restored$stages[[1]]$draw_spec$reserve)

  # JSON has no infinity, so the file holds the token R prints.
  whole <- two_stage_reserve(reserve = Inf)
  expect_identical(whole$stages[[2]]$draw_spec$reserve, Inf)
  expect_output(print(whole), "reserve = Inf", fixed = TRUE)
  write_design(whole, path)
  expect_match(readLines(path), '"reserve": "Inf"', fixed = TRUE, all = FALSE)
  expect_identical(read_design(path)$stages[[2]]$draw_spec$reserve, Inf)
})

test_that("execute() checks the reserve of a design read from file", {
  # read_design() does not rerun draw(), so execute() is the gate.
  path <- withr::local_tempfile(fileext = ".json")
  write_design(sampling_design() |> draw(n = 4, reserve = 2), path)
  text <- sub('"n": 4', '"n": 1', paste(readLines(path), collapse = "\n"),
              fixed = TRUE)
  writeLines(text, path)
  restored <- read_design(path)
  expect_error(
    execute(restored, bfa_eas, seed = 1),
    "at least 2",
    class = "samplyr_error_draw_argument"
  )

  restored <- read_design(path)
  restored$stages[[1]]$draw_spec$n <- 4
  restored$stages[[1]]$draw_spec$method <- "bernoulli"
  expect_error(
    execute(restored, bfa_eas, seed = 1),
    class = "samplyr_error_reserve_method"
  )
})

test_that("execute() refuses a design whose reserve sits on two stages", {
  design <- two_stage_reserve()
  design$stages[[1]]$draw_spec$reserve <- 1L
  expect_error(
    execute(design, bfa_eas, seed = 1),
    class = "samplyr_error_draw_argument"
  )
})

test_that("compare_designs() lists the reserve only when a stage has one", {
  plain <- sampling_design() |> draw(n = 4)
  with_reserve <- sampling_design() |> draw(n = 4, reserve = 3)
  spec <- compare_specifications(with_reserve, plain, bfa_eas)
  expect_identical(spec$aspect, c("method", "mos", "reserve"))
  expect_identical(spec$x[[3]], "3")
  expect_identical(spec$y[[3]], NA_character_)
  expect_identical(spec$status[[3]], "differ")
  expect_identical(
    compare_specifications(plain, plain, bfa_eas)$aspect,
    c("method", "mos")
  )
})

## Executing a reserve

test_that("each pool lists its take and reserve, with list weights", {
  sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4, reserve = 3) |>
    execute(bfa_eas, seed = 11)
  df <- as.data.frame(sample)
  expect_type(df$.reserve_1, "integer")
  N_h <- table(bfa_eas$region)
  for (r in names(N_h)) {
    rows <- df[df$region == r, ]
    expect_identical(sort(rows$.reserve_1), c(0L, 0L, 0L, 0L, 1L, 2L, 3L))
    expect_equal(unique(rows$.weight_1), as.numeric(N_h[[r]]) / 7)
  }
})

test_that("an infinite reserve lists every unit of each pool", {
  whole <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4, reserve = Inf) |>
    execute(bfa_eas, seed = 11)
  # Any reserve that covers the largest pool draws the same list and order.
  covering <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4, reserve = max(table(bfa_eas$region))) |>
    execute(bfa_eas, seed = 11)
  expect_identical(whole$ea_id, covering$ea_id)
  expect_identical(whole$.reserve_1, covering$.reserve_1)
  expect_setequal(whole$ea_id, bfa_eas$ea_id)
  expect_identical(unique(whole$.weight_1), 1)
  expect_equal(
    frame_summary(whole, detail = "pool")$n_reserve,
    as.vector(table(bfa_eas$region)) - 4
  )
})

test_that("a systematic list carries the same reserve ranks and weights", {
  sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 3, reserve = 2, method = "systematic") |>
    execute(bfa_eas, seed = 12)
  df <- as.data.frame(sample)
  N_h <- table(bfa_eas$region)
  for (r in names(N_h)) {
    rows <- df[df$region == r, ]
    expect_identical(sort(rows$.reserve_1), c(0L, 0L, 0L, 1L, 2L))
    expect_equal(unique(rows$.weight_1), as.numeric(N_h[[r]]) / 5)
  }
})

test_that("a pool too small for the whole list lists what it has", {
  frame <- data.frame(
    id = 1:18,
    g = rep(c("a", "b", "c"), times = c(10, 5, 3))
  )
  expect_warning(
    sample <- sampling_design() |>
      stratify_by(g) |>
      draw(n = 4, reserve = 3) |>
      execute(frame, seed = 13),
    class = "samplyr_warning_size_capped"
  )
  df <- as.data.frame(sample)
  expect_identical(
    sort(df$.reserve_1[df$g == "a"]),
    c(0L, 0L, 0L, 0L, 1L, 2L, 3L)
  )
  expect_identical(sort(df$.reserve_1[df$g == "b"]), c(0L, 0L, 0L, 0L, 1L))
  # A pool its take covers is listed whole, with no reserve.
  expect_identical(df$.reserve_1[df$g == "c"], c(0L, 0L, 0L))
  expect_equal(unique(df$.weight_1[df$g == "a"]), 10 / 7)
  expect_equal(unique(df$.weight_1[df$g == "b"]), 1)
  expect_equal(unique(df$.weight_1[df$g == "c"]), 1)
})

test_that("a reserve at the second stage lists within each selected cluster", {
  sample <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(province) |>
    draw(n = 2) |>
    add_stage() |>
    draw(n = 3, reserve = 2) |>
    execute(bfa_eas, seed = 14) |>
    suppressWarnings()
  df <- as.data.frame(sample)
  expect_false(".reserve_1" %in% names(df))
  N_prov <- table(bfa_eas$province)
  for (p in unique(df$province)) {
    rows <- df[df$province == p, ]
    expect_identical(sort(rows$.reserve_2), c(0L, 0L, 0L, 1L, 2L))
    expect_equal(unique(rows$.weight_2), as.numeric(N_prov[[p]]) / 5)
  }
  expect_equal(df$.weight, df$.weight_1 * df$.weight_2)
})

test_that("a first-stage reserve lists clusters and carries to their rows", {
  sample <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(province) |>
    draw(n = 2, reserve = 1) |>
    add_stage() |>
    draw(n = 3) |>
    execute(bfa_eas, stages = 1, seed = 15) |>
    suppressWarnings()
  df <- as.data.frame(sample)
  expect_false(".reserve_2" %in% names(df))
  provinces <- df[!duplicated(df$province), ]
  n_prov <- tapply(bfa_eas$province, bfa_eas$region, function(x) {
    length(unique(x))
  })
  for (r in names(n_prov)) {
    listed <- provinces[provinces$region == r, ]
    L <- min(3, n_prov[[r]])
    expect_identical(nrow(listed), as.integer(L))
    expect_identical(sum(listed$.reserve_1 == 0L), as.integer(min(2, L)))
    expect_equal(unique(listed$.weight_1), n_prov[[r]] / L)
  }
  # Every row of a cluster carries its cluster's rank.
  ranks <- lengths(lapply(
    split(df$.reserve_1, as.character(df$province)),
    unique
  ))
  expect_true(all(ranks == 1L))
})

test_that("execute() refuses a take below 2 that a rate gives a pool", {
  expect_error(
    sampling_design() |>
      stratify_by(region) |>
      draw(frac = 0.0003, reserve = 2) |>
      execute(bfa_eas, seed = 1),
    "at least 2",
    class = "samplyr_error_draw_argument"
  )
})

test_that("panels cannot be assigned at the stage that holds the reserve", {
  reserve_first <- sampling_design() |>
    add_stage() |>
    cluster_by(province) |>
    draw(n = 4, reserve = 1) |>
    add_stage() |>
    draw(n = 2)
  expect_error(
    execute(reserve_first, bfa_eas, stages = 1, seed = 1, panels = 2),
    class = "samplyr_error_panel_stage_reserve"
  )
  reserve_second <- sampling_design() |>
    add_stage() |>
    cluster_by(province) |>
    draw(n = 4) |>
    add_stage() |>
    draw(n = 3, reserve = 1)
  expect_error(
    execute(reserve_second, bfa_eas, seed = 1, panels = 2, panel_stage = 2),
    class = "samplyr_error_panel_stage_reserve"
  )
  sample <- execute(reserve_second, bfa_eas, seed = 1, panels = 2)
  df <- as.data.frame(sample)
  expect_identical(sort(unique(df$.panel)), 1:2)
  expect_identical(as.vector(table(df$.reserve_2)), c(12L, 4L))
})

test_that("a continuation and replicates draw the reserve stage", {
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(province) |>
    draw(n = 3) |>
    add_stage() |>
    draw(n = 3, reserve = 2)
  first <- execute(design, bfa_eas, stages = 1, seed = 16)
  sample <- execute(first, bfa_eas, seed = 17)
  expect_identical(as.vector(table(sample$.reserve_2)), c(9L, 3L, 3L))
  expect_identical(get_frame_digest(sample)$version, 4L)

  reps <- execute(design, bfa_eas, seed = 18, reps = 2)
  expect_identical(
    as.vector(table(reps$.replicate, reps$.reserve_2)),
    c(9L, 9L, 3L, 3L, 3L, 3L)
  )
})

test_that("every survey export refuses a list whose reserve is not settled", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  sample <- sampling_design() |>
    draw(n = 10, reserve = 4) |>
    execute(bfa_eas, seed = 19)
  expect_error(as_svydesign(sample), class = "samplyr_error_reserve_unsettled")
  expect_error(
    as_svrepdesign(sample),
    class = "samplyr_error_reserve_unsettled"
  )
  expect_error(
    srvyr::as_survey_design(sample),
    class = "samplyr_error_reserve_unsettled"
  )
  expect_error(
    srvyr::as_survey_rep(sample),
    class = "samplyr_error_reserve_unsettled"
  )
})

test_that("exante_probabilities() gives the chance of the whole list", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4, reserve = 3)
  chances <- exante_probabilities(design, bfa_eas, key = ea_id)
  N_h <- table(bfa_eas$region)
  expected <- 7 / as.numeric(N_h[as.character(bfa_eas$region)])
  expect_equal(
    chances$probability[match(bfa_eas$ea_id, chances$ea_id)],
    expected
  )
})

test_that("summary() and the frame digest report the reserve per pool", {
  sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4, reserve = 3) |>
    execute(bfa_eas, seed = 20)
  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "n_h 4, n_reserve 3", fixed = TRUE)
  digest <- get_frame_digest(sample)
  expect_identical(digest$version, 4L)
  pools <- frame_summary(sample, detail = "pool")
  expect_identical(unique(pools$n_reserve), 3)
  expect_identical(unique(pools$n_target), 4)
  expect_identical(unique(pools$n_realized), 7)

  plain <- sampling_design() |> draw(n = 4) |> execute(bfa_eas, seed = 20)
  expect_identical(get_frame_digest(plain)$version, 2L)
  expect_false("n_reserve" %in% names(frame_summary(plain, detail = "pool")))
})

test_that("a design without a reserve draws the same units as before", {
  # Hashes recorded before execution learned to list a reserve. The ids,
  # their order and the panel or replicate columns must not move.
  q <- function(expr) suppressWarnings(suppressMessages(expr))
  ids <- function(s, extra = character(0)) {
    df <- as.data.frame(s)
    rlang::hash(df[c("ea_id", ".sample_id", extra)])
  }
  expect_identical(
    ids(q(sampling_design() |> draw(n = 50) |> execute(bfa_eas, seed = 1))),
    "d0586b795af17d68a7504a6981bcdbcf"
  )
  expect_identical(
    ids(q(sampling_design() |> stratify_by(region) |> draw(n = 5) |>
            execute(bfa_eas, seed = 2))),
    "74eed1a7089c4763d68b507a5699d773"
  )
  expect_identical(
    ids(q(sampling_design() |> add_stage() |> stratify_by(region) |>
            cluster_by(province) |> draw(n = 2) |> add_stage() |>
            draw(n = 4) |> execute(bfa_eas, seed = 3))),
    "fb5af7c5d09b5cd90ae39bbe7979cec7"
  )
  # Re-recorded when sondage 0.11.0 began returning brewer samples in
  # ascending unit order. The selected units are unchanged.
  expect_identical(
    ids(q(sampling_design() |> stratify_by(region) |>
            draw(n = 4, method = "pps_brewer", mos = households) |>
            execute(bfa_eas, seed = 4))),
    "c07e714214959367cad729e983874da3"
  )
  expect_identical(
    ids(q(sampling_design() |> stratify_by(region) |>
            draw(n = 4, method = "systematic") |>
            execute(bfa_eas, seed = 5))),
    "477d53e2174c2419b03882ea9ddfd252"
  )
  expect_identical(
    ids(q(sampling_design() |> stratify_by(region) |> draw(n = 8) |>
            execute(bfa_eas, seed = 6, panels = 4)), ".panel"),
    "ad572b4d6ec2525eae365952dc42a0c0"
  )
  expect_identical(
    ids(q(sampling_design() |> add_stage() |> cluster_by(province) |>
            draw(n = 3) |> add_stage() |> draw(n = 2) |>
            execute(bfa_eas, seed = 7, reps = 3)), ".replicate"),
    "05396ce603ee518cf9f31cc24c03ff7b"
  )
})

## Settling a reserve

# Every ordered list of b + r units from a pool of the given categories.
ordered_lists <- function(B, m) {
  if (m == 0) {
    return(matrix(integer(), 1, 0))
  }
  do.call(rbind, lapply(seq_len(B), function(i) {
    rest <- setdiff(seq_len(B), i)
    sub <- ordered_lists(B - 1, m - 1)
    cbind(i, matrix(rest[sub], nrow(sub)))
  }))
}

# Releases one reserve unit per failure, then `extra` more, at most `cap`.
field_release <- function(main, reserve, b, extra = 0L, cap = Inf) {
  need <- b - sum(main == 1L)
  m <- 0L
  if (need > 0L) {
    hit <- which(cumsum(reserve == 1L) >= need)
    m <- if (length(hit) > 0L) hit[1] else length(reserve)
  }
  m <- min(m + extra, length(reserve), cap)
  reserve[seq_len(m)]
}

expected_eligible <- function(type, b, r, ...) {
  B <- length(type)
  lists <- ordered_lists(B, b + r)
  est <- apply(lists, 1, function(o) {
    t <- type[o]
    main <- t[seq_len(b)]
    released <- field_release(main, t[b + seq_len(r)], b, ...)
    x <- settle_reserve_pool(main, released, B, r)
    B * (x$n_eligible - x$stopped) /
      (x$n_attempted - x$stopped - x$n_unknown)
  })
  mean(est)
}

test_that("the settled eligible count is exact over every release order", {
  # Categories: 1 interview, 2 eligible non-interview, 4 not eligible.
  type <- c(4L, 1L, 1L, 1L, 2L, 2L, 1L)
  expect_equal(expected_eligible(type, b = 2, r = 3), 6)
  # Coded 3, unknowns are eligible at the resolved rate of the pool.
  type <- c(4L, 3L, 1L, 1L, 2L, 1L, 2L, 4L)
  expect_equal(expected_eligible(type, b = 2, r = 3), 8 * 5 / 7)
})

test_that("truncation keeps a pool worked past its stop exact", {
  type <- c(4L, 1L, 1L, 1L, 2L, 2L, 1L)
  expect_equal(expected_eligible(type, b = 2, r = 3, extra = 1L), 6)
  # A cap that does not depend on the outcomes leaves it exact too.
  expect_equal(expected_eligible(type, b = 2, r = 3, cap = 1L), 6)
})

settle_frame <- function() {
  sizes <- c(10, 10, 10, 10, 3)
  data.frame(
    region = rep(c("north", "north", "north", "south", "south"), sizes),
    ea_id = rep(1:5, sizes),
    hh_id = seq_len(sum(sizes))
  )
}

settle_design <- function() {
  sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = c(north = 3, south = 2)) |>
    add_stage() |>
    draw(n = 3, reserve = 3)
}

# Codes the take in .sample_id order and the reserve in release order.
set_codes <- function(sample, codes) {
  rank <- sample$.reserve_2
  out <- rep(NA, nrow(sample))
  for (ea in names(codes)) {
    rows <- which(sample$ea_id == as.integer(ea))
    main <- rows[rank[rows] == 0L]
    reserve <- rows[rank[rows] > 0L]
    reserve <- reserve[order(rank[reserve])]
    out[main] <- codes[[ea]]$main
    out[reserve[seq_along(codes[[ea]]$reserve)]] <- codes[[ea]]$reserve
  }
  out
}

listed_sample <- function() {
  execute(settle_design(), settle_frame(), seed = 4) |> suppressWarnings()
}

settle_codes <- function(unknown) {
  list(
    # Under-released: the field stopped with two of three respondents.
    "1" = list(main = c(1, 2.1, 1), reserve = 4.6),
    # Over-released: rank 2 came after the stop and is dropped.
    "2" = list(main = c(1, 1, 3.2), reserve = c(1, 1)),
    # No respondent: the list runs out and the pool has no rows.
    "3" = list(main = c(2, 4, 3), reserve = c(2, 4, 3)),
    "4" = list(main = c(1, unknown, 4.6), reserve = c(1, 2.1, 1)),
    # Covered: the take lists every unit of the pool.
    "5" = list(main = c(1, 1, 3))
  )
}

test_that("a settled sample keeps the respondents at their exact weights", {
  listed <- listed_sample()
  expect_identical(as.vector(table(listed$ea_id)), c(6L, 6L, 6L, 6L, 3L))
  listed$code <- set_codes(listed, settle_codes(3))
  settled <- execute(listed, response_status = code)
  weights <- tapply(settled$.weight_2, settled$ea_id, unique)
  expect_equal(
    as.vector(weights),
    c(10 * 3 / 4 / 2, 10 * 2 / 2 / 3, 10 * 3 / 4 / 3, 3 * 2 / 2 / 2)
  )
  expect_identical(names(weights), c("1", "2", "4", "5"))
  expect_identical(as.vector(table(settled$ea_id)), c(2L, 3L, 3L, 2L))
  expect_equal(settled$.weight, settled$.weight_1 * settled$.weight_2)
  expect_true(all(settled$code == 1))

  record <- attr(settled, "metadata")$reserve_settlement
  expect_identical(names(record$keys), c("region", "ea_id"))
  expect_identical(record$keys$ea_id, 1:5)
  pools <- record$pools
  expect_identical(pools$n_attempted, c(4L, 4L, 6L, 6L, 3L))
  expect_identical(pools$dropped, c(0L, 1L, 0L, 0L, 0L))
  expect_identical(pools$under_released, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(pools$over_released, c(FALSE, TRUE, FALSE, FALSE, FALSE))
  # The covered pool worked every unit without reaching its take.
  expect_identical(pools$ran_out, c(FALSE, FALSE, TRUE, FALSE, TRUE))
  # Pool 3 has no respondent: 2 eligible among 4 resolved units of 10.
  expect_identical(pools$no_respondent, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(pools$eligible[3], 10 * 2 / 4)
  expect_identical(pools$eligible_rule, rep("resolved", 5))
  expect_output(
    print(summary(settled)),
    paste("ran_out 2, under_released 1, over_released 1, no_respondent 1",
          "of 5 pools, dropped 1"),
    fixed = TRUE
  )
  # 14 eligible, 4 ineligible and 5 unknown among 23 units, 10 respondents.
  expect_output(
    print(summary(settled)),
    paste("Realized eligibility_rate 0.778, resp_rate 0.559,",
          "n_respondents / n_attempted 0.435, sd between pools 0.047"),
    fixed = TRUE
  )
  rates <- reserve_realized_rates(pools)
  expect_equal(
    rates[["eligibility_rate"]] * rates[["resp_rate"]],
    rates[["completes"]]
  )
  # Each pool's (r - s) / (k - s), less its own noise p (1 - p) / (k - s - 1).
  share <- c(2 / 4, 2 / 3, 0, 2 / 5, 2 / 3)
  noise <- share * (1 - share) / c(3, 2, 5, 4, 2)
  expect_equal(rates[["completes_sd"]], sqrt(var(share) - mean(noise)))
  # With no unit resolved, unknowns count as eligible, as in the weights.
  unresolved <- tibble::tibble(
    n_attempted = 4L, n_eligible = 0L, n_ineligible = 0L, n_respondents = 0L
  )
  unresolved$stopped <- 0L
  expect_identical(
    reserve_realized_rates(unresolved),
    c(eligibility_rate = 1, resp_rate = 0, completes = 0,
      completes_sd = NA_real_)
  )
  unresolved$n_ineligible <- 4L
  expect_identical(
    reserve_realized_rates(unresolved),
    c(eligibility_rate = 0, resp_rate = NA_real_, completes = 0,
      completes_sd = NA_real_)
  )
  # Pools as alike as their noise allows have no spread beyond it.
  alike <- tibble::tibble(
    n_attempted = c(10L, 10L), n_eligible = 10L, n_ineligible = 0L,
    n_respondents = c(5L, 6L), stopped = 0L
  )
  expect_identical(reserve_realized_rates(alike)[["completes_sd"]], 0)
})

test_that("the code of an unknown sets its eligibility", {
  weight_4 <- function(unknown, as_factor = FALSE) {
    listed <- listed_sample()
    listed$code <- set_codes(listed, settle_codes(unknown))
    if (as_factor) {
      listed$code <- factor(listed$code)
    }
    settled <- execute(listed, response_status = code)
    unique(settled$.weight_2[settled$ea_id == 4L])
  }
  expect_equal(weight_4(3), 10 * 3 / 4 / 3)
  expect_equal(weight_4("noninterview"), 10 * 4 / 5 / 3)
  expect_equal(weight_4("4.60"), 10 * 3 / 5 / 3)
  expect_equal(weight_4("unknown", as_factor = TRUE), 10 * 3 / 4 / 3)
})

test_that("outcomes that break the release protocol are refused by pool", {
  codes <- settle_codes(3)
  listed <- listed_sample()
  listed$code <- set_codes(listed, codes)
  bad <- listed
  bad$code[which(bad$code == 4.6)[1]] <- 9
  expect_error(execute(bad, response_status = code),
               class = "samplyr_error_reserve_protocol")
  bad <- listed
  bad$code[bad$ea_id == 1L & bad$.reserve_2 == 0L][1] <- NA
  expect_error(execute(bad, response_status = code), "ea_id = 1",
               class = "samplyr_error_reserve_protocol")
  bad <- listed
  bad$code[bad$ea_id == 2L & bad$.reserve_2 == 1L] <- NA
  expect_error(execute(bad, response_status = code), "ea_id = 2",
               class = "samplyr_error_reserve_protocol")
})

test_that("settling is refused once settled, without a reserve, or with other input", {
  listed <- listed_sample()
  listed$code <- set_codes(listed, settle_codes(3))
  settled <- execute(listed, response_status = code)
  expect_error(execute(settled, response_status = code),
               class = "samplyr_error_reserve_already_settled")
  expect_error(execute(listed, response_status = code, seed = 1),
               class = "samplyr_error_execute_argument")
  expect_error(execute(listed, response_status = nothing_here),
               class = "samplyr_error_execute_argument")
  plain <- sampling_design() |> draw(n = 3) |> execute(settle_frame(), seed = 1)
  plain$code <- 1
  expect_error(execute(plain, response_status = code),
               class = "samplyr_error_execute_argument")
})

test_that("a stage below a first-stage reserve waits for its settlement", {
  skip_if_not_installed("survey")
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = c(north = 2, south = 2), reserve = 1) |>
    add_stage() |>
    draw(n = 2)
  frame <- settle_frame()
  expect_error(execute(design, frame, seed = 1),
               class = "samplyr_error_reserve_unsettled")
  listed <- execute(design, frame, stages = 1, seed = 1) |> suppressWarnings()
  expect_error(execute(listed, frame, seed = 2),
               class = "samplyr_error_reserve_unsettled")

  # The first EA of the north take is inaccessible, its reserve responds.
  north_take <- listed$ea_id[listed$region == "north" & listed$.reserve_1 == 0L]
  listed$access <- ifelse(listed$.reserve_1 == 0L, 1, NA)
  listed$access[listed$ea_id == min(north_take)] <- 2.3
  listed$access[listed$region == "north" & listed$.reserve_1 == 1L] <- 1
  # Every household row of an EA carries the EA's code.
  mixed <- listed
  mixed$access[which(mixed$ea_id == min(north_take))[1]] <- 1
  expect_error(execute(mixed, response_status = access),
               class = "samplyr_error_reserve_protocol")

  settled <- execute(listed, response_status = access)
  north <- settled[settled$region == "north", ]
  expect_false(min(north_take) %in% north$ea_id)
  # Three EAs listed, the stop at the reserve: 3 * (3 - 1) / (3 - 1) / 2.
  expect_equal(unique(north$.weight_1), 1.5)
  full <- execute(settled, frame, seed = 2)
  expect_identical(sort(unique(full$ea_id)), sort(unique(settled$ea_id)))
  expect_equal(full$.weight, full$.weight_1 * full$.weight_2)
  # The settlement carries over, so the continued sample is settled.
  expect_identical(
    attr(full, "metadata")$reserve_settlement$codes,
    attr(settled, "metadata")$reserve_settlement$codes
  )

  path <- withr::local_tempfile(fileext = ".json")
  write_design(full, path, frame = frame)
  replayed <- replay_design(read_design(path), list(frame, frame))
  expect_identical(replayed$hh_id, full$hh_id)
  expect_equal(replayed$.weight, full$.weight)
})

test_that("a settlement replays from its receipt", {
  listed <- listed_sample()
  listed$code <- set_codes(listed, settle_codes(3))
  settled <- execute(listed, response_status = code)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(settled, path, frame = settle_frame())
  replayed <- replay_design(read_design(path), settle_frame())
  expect_identical(replayed$.sample_id, settled$.sample_id)
  expect_equal(replayed$.weight, settled$.weight)
  expect_identical(
    attr(replayed, "metadata")$reserve_settlement$codes,
    attr(settled, "metadata")$reserve_settlement$codes
  )

  # A receipt whose outcomes do not fit the list is refused.
  text <- paste(readLines(path), collapse = "\n")
  writeLines(sub('"codes": \\[', '"codes": [9, ', text), path)
  expect_error(
    replay_design(read_design(path), settle_frame()),
    class = "samplyr_error_replay_settlement"
  )
})


test_that("a settlement records the attempted units its sample drops", {
  listed <- listed_sample()
  # EA 2's reserve released after its stop is ineligible here, so the walk
  # decides whether it is kept.
  codes <- settle_codes(3)
  codes[["2"]]$reserve <- c(1, 4)
  listed$code <- set_codes(listed, codes)
  settled <- execute(listed, response_status = code)
  units <- attr(settled, "metadata")$reserve_settlement$units
  # By hand: each pool's take, then its reserve in rank order while it has
  # fewer than three respondents. The kept units coded 2 to 4 are the ones
  # the sample drops.
  digit <- floor(listed$code)
  expected <- integer(0)
  for (ea in unique(listed$ea_id)) {
    rows <- which(listed$ea_id == ea)
    kept <- rows[listed$.reserve_2[rows] == 0L]
    reserve <- rows[listed$.reserve_2[rows] > 0L & !is.na(listed$code[rows])]
    reserve <- reserve[order(listed$.reserve_2[reserve])]
    for (r in reserve) {
      if (sum(digit[kept] == 1) >= 3) break
      kept <- c(kept, r)
    }
    expected <- c(expected, kept[digit[kept] %in% 2:4])
  }
  expect_setequal(units$rows$hh_id, listed$hh_id[expected])
  expect_identical(units$code[order(units$rows$hh_id)],
                   as.integer(digit[expected][order(listed$hh_id[expected])]))
  # EA 2 released a reserve after its stop, which is not kept.
  late <- listed$ea_id == 2 & listed$.reserve_2 == 2L
  expect_identical(digit[late], 4)
  expect_false(any(units$rows$hh_id %in% listed$hh_id[late]))
  expect_length(intersect(units$rows$hh_id, settled$hh_id), 0L)
  pools <- attr(settled, "metadata")$reserve_settlement$pools
  count <- function(code) tabulate(units$pool[units$code == code], nrow(pools))
  expect_identical(count(4L), pools$n_ineligible)
  expect_identical(count(3L), pools$n_unknown)
  expect_identical(count(2L), pools$n_eligible - pools$n_respondents)
  expect_true(samplyr:::sample_realization_status(settled)$ok)

  expect_null(attributes(units$rows)$metadata)
  # A replay from the receipt rebuilds the same units, with the columns of
  # the frame and the draw, and the response status rebuilt from their
  # categories.
  path <- withr::local_tempfile(fileext = ".json")
  write_design(settled, path, frame = settle_frame())
  replayed <- replay_design(read_design(path), settle_frame())
  again <- attr(replayed, "metadata")$reserve_settlement$units
  expect_identical(again[c("pool", "unit", "code")],
                   units[c("pool", "unit", "code")])
  expect_setequal(names(again$rows), names(units$rows))
  others <- setdiff(names(units$rows), "code")
  expect_identical(again$rows[others], units$rows[others])
  expect_identical(again$rows$code, floor(units$rows$code))

  # A stage-1 settlement keeps them through the continuation.
  design <- sampling_design() |> add_stage() |> cluster_by(ea_id) |>
    draw(n = 2, reserve = 2) |> add_stage() |> draw(n = 2)
  first <- suppressWarnings(execute(design, settle_frame(), seed = 1,
                                    stages = 1))
  first$code <- ifelse(first$.reserve_1 == 0L,
                       ifelse(first$ea_id == min(first$ea_id), 4, 1), NA)
  rank_one <- first$.reserve_1 == 1L
  first$code[rank_one] <- 1
  first <- suppressWarnings(execute(first, response_status = code))
  held <- attr(first, "metadata")$reserve_settlement$units
  expect_gt(nrow(held$rows), 0L)
  continued <- suppressWarnings(execute(first, settle_frame(), seed = 2))
  record <- attr(continued, "metadata")$reserve_settlement
  expect_true(record$carried)
  expect_identical(record$units, held)
})

## Exporting a settled reserve

export_codes <- function(pool_3) {
  codes <- settle_codes(3)
  codes[["3"]] <- pool_3
  codes
}

# The settlement frame with one EA more in each region, so that stage 1 is
# a sample and its EAs are sampled parents. Seed 15 draws EAs 1 to 5 again,
# at stage-1 weights of 4 / 3 in the north and 3 / 2 in the south. A parent
# taken whole is tested on its own fixtures.
export_frame <- function() {
  frame <- settle_frame()
  extra <- data.frame(
    region = rep(c("north", "south"), c(10, 3)),
    ea_id = rep(6:7, c(10, 3)),
    hh_id = max(frame$hh_id) + seq_len(13)
  )
  rbind(frame, extra)
}

export_listed <- function(frame = export_frame(), design = settle_design()) {
  execute(design, frame, seed = 15) |> suppressWarnings()
}

settled_sample <- function(pool_3 = list(main = c(1, 4, 1), reserve = 1)) {
  listed <- export_listed()
  listed$code <- set_codes(listed, export_codes(pool_3))
  execute(listed, response_status = code)
}

element_frame <- function() {
  data.frame(region = rep(c("a", "b"), c(40, 30)), id = 1:70, y = 1:70)
}

# Codes the take, then the reserve in release order, in each stratum.
element_settled <- function(as = identity) {
  listed <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4, reserve = 4) |>
    execute(element_frame(), seed = 3)
  code <- rep(NA, nrow(listed))
  for (r in c("a", "b")) {
    rows <- which(listed$region == r)
    main <- rows[listed$.reserve_1[rows] == 0L]
    reserve <- rows[listed$.reserve_1[rows] > 0L]
    reserve <- reserve[order(listed$.reserve_1[reserve])]
    code[main] <- c(1, 4, 1, 2)
    code[reserve[1:3]] <- c(4, 1, 1)
  }
  listed$code <- as(code)
  execute(listed, response_status = code)
}

test_that("a settled reserve below stage 1 exports by ultimate clusters", {
  skip_if_not_installed("survey")
  settled <- settled_sample()
  expect_identical(sort(unique(settled$ea_id)), 1:5)
  expect_identical(sort(unique(settled$.weight_1)), c(4 / 3, 1.5))
  svy <- as_svydesign(settled)
  data <- svy$variables
  respondent <- !is.na(data$.sample_id)
  expect_equal(unname(weights(svy))[respondent], settled$.weight)
  # One row per ineligible household, at the stage-1 weight times N over the
  # units the estimate counts: EA 1 resolved 4, EA 3 and EA 4 resolved 3
  # and 4 once the unit that stopped the list and an unknown are left out.
  expect_identical(data$ea_id[!respondent], c(1L, 3L, 4L))
  expect_identical(data$code[!respondent], c(4, 4, 4))
  expect_equal(data$.weight[!respondent],
               c(4 / 3 * 10 / 4, 4 / 3 * 10 / 3, 3 / 2 * 10 / 4))
  # Every pool's weights then sum to its size times the stage-1 weight.
  expect_equal(as.vector(tapply(data$.weight, data$ea_id, sum)),
               c(4 / 3 * c(10, 10, 10), 3 / 2 * c(10, 3)))
  # The added rows sit in their EAs and hold NA, so an analysis total and
  # its variance are the ultimate-cluster ones of the respondents: with
  # replacement, over each region's EA totals.
  z <- tapply(settled$.weight * settled$hh_id, settled$ea_id, sum)
  region <- tapply(settled$region, settled$ea_id, unique)
  by_hand <- sum(tapply(z, region, function(t) {
    length(t) / (length(t) - 1) * sum((t - mean(t))^2)
  }))
  total <- survey::svytotal(~hh_id, svy, na.rm = TRUE)
  expect_equal(unname(coef(total)), sum(z))
  expect_equal(as.numeric(survey::SE(total)), sqrt(by_hand))
  # Stage-1 clusters only, drawn with replacement.
  expect_identical(deparse(svy$call$ids), "~.id_1")
  expect_null(svy$fpc$popsize)
  rep <- as_svrepdesign(settled, type = "JKn")
  expect_s3_class(rep, "svyrep.design")
  expect_equal(unname(weights(rep, type = "sampling")), unname(weights(svy)))
})

test_that("the weighting-loss verbs and varcomp() wait for the settlement", {
  listed <- export_listed()
  expect_error(design_effect(listed), class = "samplyr_error_reserve_unsettled")
  expect_error(effective_n(listed), class = "samplyr_error_reserve_unsettled")
  expect_error(varcomp(listed, ~hh_id),
               class = "samplyr_error_reserve_unsettled")

  # A stage 1 whose reserve stage is still to be drawn holds no list.
  stage1 <- execute(settle_design(), export_frame(), stages = 1, seed = 15) |>
    suppressWarnings()
  expect_identical(unsettled_reserve_stages(stage1), integer(0))
  w <- stage1$.weight
  expect_equal(effective_n(stage1), sum(w)^2 / sum(w^2))

  # Settled, Kish's effective size is read from the respondents' weights.
  settled <- settled_sample()
  w <- settled$.weight
  expect_equal(effective_n(settled), sum(w)^2 / sum(w^2))
  expect_equal(as.double(design_effect(settled)), length(w) * sum(w^2) / sum(w)^2)
  expect_s3_class(varcomp(settled, ~hh_id), "svyplan_varcomp")
})

test_that("the full and two-phase routes refuse a settled reserve", {
  skip_if_not_installed("survey")
  settled <- settled_sample()
  for (type in c("mrbbootstrap", "rwyb")) {
    expect_error(
      as_svrepdesign(settled, type = type),
      class = "samplyr_error_reserve_full_export"
    )
  }
  phase2 <- sampling_design() |> draw(n = 4) |> execute(settled, seed = 1)
  expect_error(as_svydesign(phase2),
               class = "samplyr_error_reserve_full_export")
  expect_error(as_svrepdesign(phase2),
               class = "samplyr_error_reserve_full_export")
})

no_respondent_codes <- list(main = c(2, 4, 3), reserve = c(2, 4, 3))

# The outcome codes as words, or as a factor of those words.
code_words <- function(code) {
  words <- c("interview", "noninterview", "unknown", "ineligible")
  words[floor(code)]
}

## A reserve in panels

# 12 EAs of 10 households, and two panels active in turn over 4 periods.
panel_eas <- function() data.frame(ea = 1:12)
panel_listing <- function() expand.grid(hh = 1:10, ea = 1:12)
alternating <- function(cohorts) {
  schedule <- expand.grid(panel = 1:2, period = 1:4)
  schedule$active <- schedule$panel == (schedule$period - 1) %% 2 + 1
  if (cohorts) {
    schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  }
  schedule
}

test_that("a schedule with several cohorts per panel takes no reserve", {
  below <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 8) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2, reserve = 2)
  expect_error(
    execute(below, panel_eas(), stages = 1, panels = alternating(TRUE),
            seed = 1),
    "Stage 2 draws a `reserve`",
    class = "samplyr_error_cohort_stages"
  )
  # Settled above the panels, the master is still never complete.
  regions <- data.frame(region = 1:6)
  eas <- data.frame(region = rep(1:6, each = 2), ea = 1:12)
  above <- sampling_design() |>
    add_stage() |> cluster_by(region) |> draw(n = 2, reserve = 2) |>
    add_stage() |> cluster_by(ea) |> draw(n = 2) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2)
  listed <- execute(above, regions, stages = 1, seed = 1)
  listed$code <- ifelse(listed$.reserve_1 == 0L, 1, NA)
  settled <- execute(listed, response_status = code)
  expect_error(
    execute(settled, eas, stages = 2, panels = alternating(TRUE),
            panel_stage = 2, seed = 2),
    class = "samplyr_error_cohort_stages"
  )
  # One cohort per panel completes, so the master settles.
  master <- execute(below, panel_eas(), stages = 1,
                    panels = alternating(FALSE), seed = 1)
  for (t in 1:2) {
    master <- execute(master, panel_listing(), period = t, seed = t + 1)
  }
  expect_identical(get_stages_executed(master), 1:2)
})

test_that("an occasion of a master with a reserve is refused, settled or not", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 8) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2, reserve = 2)
  master <- execute(design, list(panel_eas(), panel_listing()),
                    panels = alternating(FALSE), seed = 1)
  master$code <- ifelse(master$.reserve_2 == 0L, 1, NA)
  settled <- execute(master, response_status = code)
  expect_s3_class(as_svydesign(settled), "survey.design")

  for (source in list(master, settled)) {
    occasion <- execute(source, period = 1)
    expect_error(
      as_svydesign(occasion),
      "Stage 2 lists a take and a reserve",
      class = "samplyr_error_reserve_full_export"
    )
    expect_error(as_svrepdesign(occasion),
                 class = "samplyr_error_reserve_full_export")
    expect_error(srvyr::as_survey_design(occasion),
                 class = "samplyr_error_reserve_full_export")
    expect_error(srvyr::as_survey_rep(occasion),
                 class = "samplyr_error_reserve_full_export")
  }
})

test_that("a pool with no respondent is exported as its eligible nonrespondents", {
  skip_if_not_installed("survey")
  settled <- settled_sample(no_respondent_codes)
  expect_false(3L %in% settled$ea_id)
  expect_warning(
    svy <- as_svydesign(settled),
    "ea_id = 3",
    class = "samplyr_warning_reserve_no_respondent"
  )
  data <- svy$variables
  held <- which(is.na(data$.sample_id) & data$code == 2)
  # One row for each of its 2 eligible non-interviews, at the stage-1
  # weight of 4 / 3 times 10 over its 4 resolved units, which together
  # carry its estimated eligible count.
  expect_identical(data$ea_id[held], c(3L, 3L))
  expect_identical(data$region[held], c("north", "north"))
  expect_equal(data$.weight[held], rep(4 / 3 * 10 / 4, 2))
  expect_true(all(is.na(data$hh_id[held])))
  # Its 2 ineligible households add rows at the same weight, and EAs 1 and
  # 4 add one ineligible row each.
  pool_3 <- data$ea_id == 3L
  expect_identical(sort(data$code[pool_3]), c(2, 2, 4, 4))
  expect_equal(sum(data$.weight[pool_3]), 4 / 3 * 10)
  expect_identical(nrow(data), nrow(settled) + 6L)
  # The rows form EA 3's primary unit, in its stratum.
  expect_identical(sum(svy$cluster[[1]] == svy$cluster[[1]][held[1]]), 4L)

  expect_warning(
    rep <- as_svrepdesign(settled, type = "JKn"),
    class = "samplyr_warning_reserve_no_respondent"
  )
  expect_identical(nrow(rep$variables), nrow(data))
})

test_that("the added rows carry their codes in the column's type", {
  skip_if_not_installed("survey")
  placeholder_code <- function(as) {
    listed <- export_listed()
    listed$code <- as(set_codes(listed, export_codes(no_respondent_codes)))
    settled <- execute(listed, response_status = code)
    data <- suppressWarnings(as_svydesign(settled))$variables
    data$code[is.na(data$.sample_id) & data$ea_id == 3L]
  }
  expect_identical(placeholder_code(identity), c(4, 4, 2, 2))
  expect_identical(placeholder_code(code_words),
                   rep(c("ineligible", "noninterview"), each = 2))
  held <- placeholder_code(function(code) factor(code_words(code)))
  expect_identical(as.character(held),
                   rep(c("ineligible", "noninterview"), each = 2))
  # A factor without the level gains it.
  without <- factor("interview", levels = c("interview", "ineligible"))
  added <- reserve_category_value(without, 2L, 2, "noninterview")
  expect_identical(as.character(added), rep("noninterview", 2))
  expect_identical(levels(added), c("interview", "ineligible", "noninterview"))
})

test_that("a pool of unknowns takes the resolved rate of its strata", {
  settled <- settled_sample(list(main = c(3, 3, 3), reserve = c(3, 3, 3)))
  pools <- attr(settled, "metadata")$reserve_settlement$pools
  expect_identical(pools$eligible_rule,
                   c("resolved", "resolved", "strata", "resolved", "resolved"))
  # North resolved units: 3 of 4 eligible in pool 1, and 2 of 2 in pool 2
  # once the unit that stopped it is left out.
  expect_equal(pools$eligible[3], 10 * (3 + 2) / (4 + 2))
  expect_output(
    print(summary(settled)),
    "eligible_rule strata 1, all 0 of 5 pools, an assumed eligible count",
    fixed = TRUE
  )
  clean <- capture.output(print(summary(settled_sample())))
  expect_false(any(grepl("eligible_rule", clean, fixed = TRUE)))
})

test_that("a pool of unknowns counts every unit when its strata resolved none", {
  pools <- tibble::tibble(
    region = c("a", "a", "b"),
    N = c(10, 8, 6),
    n_attempted = c(4L, 3L, 5L),
    n_eligible = c(0L, 0L, 3L),
    n_unknown = c(4L, 3L, 1L),
    n_respondents = c(0L, 0L, 2L),
    stopped = c(0L, 0L, 1L)
  )
  out <- reserve_pool_eligible(pools, c(NA, NA, 4.5), pools["region"])
  expect_identical(out$rule, c("all", "all", "resolved"))
  expect_equal(out$eligible, c(10, 8, 4.5))
})

test_that("a pool of unknowns shares its estimate among its units", {
  skip_if_not_installed("survey")
  pool_3 <- function(svy) {
    data <- svy$variables
    data[is.na(data$.sample_id) & data$ea_id == 3L, ]
  }
  # EA 3 worked six households of unknown eligibility, and the north
  # resolved 5 eligible of 6, so it holds 10 * 5 / 6 eligible units, shared
  # among the six as eligible non-interviews, and 10 / 6 ineligible ones,
  # which one row with no recorded household carries.
  settled <- settled_sample(list(main = c(3, 3, 3), reserve = c(3, 3, 3)))
  svy <- suppressWarnings(as_svydesign(settled))
  rows <- pool_3(svy)
  expect_identical(rows$code, c(rep(2, 6), 4))
  expect_equal(rows$.weight,
               c(rep(4 / 3 * 10 * 5 / 6 / 6, 6), 4 / 3 * 10 / 6))
  # It names no recorded unit, which the export marks with 0.
  expect_identical(attr(svy, "samplyr_reserve_units")[
    which(is.na(svy$variables$.sample_id) & svy$variables$ea_id == 3L)[7]
  ], 0L)
  # Every pool's weights sum to its stage-1 weight times its N (closed form).
  pools <- attr(settled, "metadata")$reserve_settlement$pools
  sums <- tapply(svy$variables$.weight, svy$variables$ea_id, sum)
  expect_equal(as.vector(sums), pools$upper_weight * pools$N)
  # With the other north EAs wholly ineligible, the resolved rate is 0 and
  # the six stand for EA 3's 10 households as ineligible units.
  codes <- export_codes(list(main = c(3, 3, 3), reserve = c(3, 3, 3)))
  codes[["1"]] <- list(main = c(4, 4, 4), reserve = c(4, 4, 4))
  codes[["2"]] <- list(main = c(4, 4, 4), reserve = c(4, 4, 4))
  listed <- export_listed()
  listed$code <- set_codes(listed, codes)
  settled <- execute(listed, response_status = code)
  pools <- attr(settled, "metadata")$reserve_settlement$pools
  expect_identical(pools$eligible_rule[3], "strata")
  expect_equal(pools$eligible[3], 0)
  rows <- pool_3(suppressWarnings(as_svydesign(settled)))
  expect_identical(rows$code, rep(4, 6))
  expect_equal(rows$.weight, rep(4 / 3 * 10 / 6, 6))
})

test_that("a pool with no respondent is refused once a later stage is drawn", {
  skip_if_not_installed("survey")
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = c(north = 2, south = 2), reserve = 1) |>
    add_stage() |>
    draw(n = 2)
  frame <- settle_frame()
  listed <- execute(design, frame, stages = 1, seed = 1) |> suppressWarnings()
  listed$access <- ifelse(listed$.reserve_1 == 0L, 1, NA)
  listed$access[listed$region == "north"] <- 2
  settled <- execute(listed, response_status = access)

  # Before the next stage, the stratum is exported as its three EAs, each
  # worked and found eligible, so it is no stratum of one unit.
  expect_warning(
    svy <- as_svydesign(settled),
    "region = north",
    class = "samplyr_warning_reserve_no_respondent"
  )
  held <- is.na(svy$variables$.sample_id)
  expect_identical(svy$variables$region[held], rep("north", 3))
  expect_equal(svy$variables$.weight[held], rep(1, 3))

  full <- execute(settled, frame, seed = 2)
  expect_error(
    as_svydesign(full),
    "region = north",
    class = "samplyr_error_reserve_no_respondent"
  )
})

test_that("svrep moves a pool's nonrespondent weight to the respondents of its stratum", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  settled <- settled_sample(no_respondent_codes)
  rep <- suppressWarnings(as_svrepdesign(settled, type = "JKn"))
  adjusted <- svrep::redistribute_weights(
    rep,
    reduce_if = code == 2,
    increase_if = code == 1,
    by = "region"
  )
  before <- weights(rep, type = "sampling")
  after <- weights(adjusted, type = "sampling")
  data <- rep$variables
  held <- is.na(data$.sample_id) & data$code == 2
  expect_identical(sum(held), 2L)
  expect_equal(unname(after[held]), c(0, 0))
  north <- data$region == "north"
  expect_equal(sum(after[north]), sum(before[north]))
  respondent <- north & data$code == 1
  expect_equal(
    sum(after[respondent]),
    sum(before[respondent]) + sum(before[held])
  )

  # The rows stay, with NA in the analysis variables.
  expect_true(all(is.na(data$hh_id[held])))
  expect_error(survey::svytotal(~hh_id, adjusted),
               "All replicates contained NAs")
  expect_false(anyNA(coef(survey::svytotal(~hh_id, adjusted, na.rm = TRUE))))
  svy <- suppressWarnings(as_svydesign(settled))
  expect_true(is.na(coef(survey::svytotal(~hh_id, svy))))
  expect_warning(
    as_svydesign(settled),
    "na.rm = TRUE",
    fixed = TRUE,
    class = "samplyr_warning_reserve_no_respondent"
  )
})

test_that("respondents calibrate through indicators that are 0 on the added rows", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # Two indicators from the parity of a respondent's sample id.
  with_parity <- function(svy) {
    x <- svy$variables$.sample_id
    update(
      svy,
      even = as.numeric(x %% 2 %in% 0),
      odd = as.numeric(x %% 2 %in% 1),
      ineligible = as.numeric(code %in% 4)
    )
  }
  calibrates <- function(svy, totals, calfun = "linear") {
    added <- is.na(svy$variables$.sample_id)
    expect_true(any(added))
    cal <- survey::calibrate(svy, ~ 0 + even + odd, totals, calfun = calfun)
    expect_equal(coef(survey::svytotal(~ even + odd, cal)), totals)
    expect_equal(
      unname(weights(cal, type = "sampling")[added]),
      unname(weights(svy, type = "sampling")[added])
    )
    expect_equal(
      coef(survey::svytotal(~ineligible, cal)),
      coef(survey::svytotal(~ineligible, svy))
    )
    cal
  }

  # Four ineligible rows.
  settled <- element_settled()
  totals <- c(even = 30, odd = 32)
  for (calfun in c("linear", "raking")) {
    calibrates(with_parity(as_svydesign(settled)), totals, calfun)
  }
  rep <- as_svrepdesign(settled, type = "JKn")
  calibrates(with_parity(rep), totals)

  # A placeholder keeps its weight, or the 0 svrep leaves it.
  settled <- settled_sample(no_respondent_codes)
  totals <- c(even = 30, odd = 25)
  svy <- suppressWarnings(as_svydesign(settled))
  calibrates(with_parity(svy), totals)
  rep <- suppressWarnings(as_svrepdesign(settled, type = "JKn"))
  adjusted <- svrep::redistribute_weights(
    rep,
    reduce_if = code == 2,
    increase_if = code == 1,
    by = "region"
  )
  cal <- calibrates(with_parity(adjusted), totals)
  expect_false(anyNA(coef(survey::svytotal(~hh_id, cal, na.rm = TRUE))))
})

test_that("a primary unit found wholly ineligible stays as a zero total", {
  skip_if_not_installed("survey")
  settled <- settled_sample(list(main = c(4, 4, 4), reserve = c(4, 4, 4)))
  expect_false(3L %in% settled$ea_id)
  svy <- as_svydesign(settled)
  data <- svy$variables
  zero <- data[is.na(data$.sample_id) & data$ea_id == 3L, ]
  # One row per household worked, which together stand for its 10
  # households at the stage-1 weight of 4 / 3.
  expect_identical(zero$code, rep(4, 6))
  expect_equal(zero$.weight, rep(4 / 3 * 10 / 6, 6))
  expect_identical(length(unique(svy$cluster[[1]][data$ea_id == 3L])), 1L)
  expect_identical(nrow(data), nrow(settled) + 8L)
})

test_that("a pool estimated to hold no eligible unit is exported as ineligible", {
  skip_if_not_installed("survey")
  # EA 3 resolved only ineligible households, and the rest are unknown.
  settled <- settled_sample(list(main = c(3, 4, 4), reserve = c(3, 4, 3)))
  pools <- attr(settled, "metadata")$reserve_settlement$pools
  expect_equal(pools$eligible[3], 0)
  expect_identical(pools$no_respondent, rep(FALSE, 5))
  expect_no_warning(svy <- as_svydesign(settled))
  data <- svy$variables
  zero <- data[is.na(data$.sample_id) & data$ea_id == 3L, ]
  # Its 3 resolved households, all ineligible, carry its 10 households.
  expect_identical(zero$code, rep(4, 3))
  expect_equal(zero$.weight, rep(4 / 3 * 10 / 3, 3))

  # At stage 1, the ineligible units carry the pool, and a stage drawn after
  # the settlement exports.
  design <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(ea) |>
    draw(n = 2, reserve = 2) |>
    add_stage() |> draw(n = 2)
  frame <- data.frame(region = rep(c("a", "b"), each = 10), ea = 1:20)
  listed <- execute(design, frame, stages = 1, seed = 1)
  b <- listed$region == "b"
  listed$code <- ifelse(listed$.reserve_1 == 0L, 1, NA)
  # In b, the take is of unknown eligibility and both reserves ineligible.
  listed$code[b] <- ifelse(listed$.reserve_1[b] == 0L, 3, 4)
  settled <- execute(listed, response_status = code)
  expect_no_warning(svy <- as_svydesign(settled))
  data <- svy$variables
  expect_identical(data$code[data$region == "b"], c(4, 4))
  expect_equal(data$.weight[data$region == "b"], c(5, 5))
  listing <- data.frame(ea = rep(settled$ea, each = 5),
                        hh = seq_len(5 * nrow(settled)))
  full <- execute(settled, listing, seed = 2)
  expect_s3_class(as_svydesign(full), "survey.design")
})

test_that("a first-stage reserve exports its ineligible units as zero rows", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  settled <- element_settled()
  pools <- attr(settled, "metadata")$reserve_settlement$pools
  # Seven attempted, the stop at the seventh, two found ineligible.
  expect_identical(pools$n_attempted, c(7L, 7L))
  expect_identical(pools$n_ineligible, c(2L, 2L))
  expect_equal(pools$eligible, c(40 * 4 / 6, 30 * 4 / 6))

  svy <- as_svydesign(settled)
  data <- svy$variables
  zero <- is.na(data$.sample_id)
  expect_identical(as.vector(table(data$region[zero])), c(2L, 2L))
  expect_equal(data$.weight[zero], rep(c(40 / 6, 30 / 6), each = 2))
  expect_identical(data$code[zero], rep(4, 4))
  expect_true(all(is.na(data$y[zero])))
  # The weights of each pool add up to its size.
  expect_equal(as.vector(tapply(data$.weight, data$region, sum)), c(40, 30))

  data$elig <- as.numeric(data$code != 4)
  svy <- update(svy, elig = as.numeric(code != 4))
  expect_equal(
    unname(coef(survey::svytotal(~elig, svy))),
    sum(pools$eligible)
  )

  # na.rm keeps the zero rows in the variance, as y = 0 would.
  as_zero <- update(svy, y = ifelse(is.na(y), 0, y))
  na_rm <- survey::svytotal(~y, svy, na.rm = TRUE)
  filled <- survey::svytotal(~y, as_zero)
  expect_equal(coef(na_rm), coef(filled))
  expect_equal(survey::SE(na_rm), survey::SE(filled))
  rep <- as_svrepdesign(settled, type = "JKn")
  expect_identical(nrow(rep$variables), nrow(data))
  rep_na <- survey::svytotal(~y, rep, na.rm = TRUE)
  rep_zero <- survey::svytotal(
    ~y, update(rep, y = ifelse(is.na(y), 0, y))
  )
  expect_equal(coef(rep_na), coef(rep_zero))
  expect_equal(survey::SE(rep_na), survey::SE(rep_zero))

  expect_s3_class(srvyr::as_survey_design(settled), "tbl_svy")
  expect_s3_class(srvyr::as_survey_rep(settled), "tbl_svy")
})

test_that("the zero rows carry the ineligible code in the column's type", {
  skip_if_not_installed("survey")
  words <- c("1" = "interview", "2" = "noninterview", "4" = "ineligible")
  as_words <- function(code) unname(words[as.character(code)])
  data <- as_svydesign(element_settled(as_words))$variables
  expect_identical(data$code[is.na(data$.sample_id)], rep("ineligible", 4))
  data <- as_svydesign(element_settled(function(code) factor(code)))$variables
  zero <- data$code[is.na(data$.sample_id)]
  expect_identical(as.character(zero), rep("ineligible", 4))
  expect_true("ineligible" %in% levels(data$code))
})

test_that("a primary unit taken with certainty settles as a stratum of its own", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # EA 1 is taken with certainty, and its households are the reserve stage.
  frame <- expand.grid(hh = 1:8, psu = 1:9)
  frame$h <- "a"
  frame$mos <- ifelse(frame$psu == 1L, 1000, 10)
  frame$y <- (frame$hh * 3 + frame$psu) %% 7
  listed <- suppressMessages(
    sampling_design() |>
      add_stage() |> stratify_by(h) |> cluster_by(psu) |>
      draw(n = 4, method = "pps_brewer", mos = mos) |>
      add_stage() |> draw(n = 3, reserve = 3) |>
      execute(frame, seed = 1)
  )
  expect_identical(unique(listed$psu[listed$.certainty_1]), 1L)
  settle <- function(sure_codes) {
    code <- rep(NA, nrow(listed))
    for (p in unique(listed$psu)) {
      rows <- which(listed$psu == p)
      main <- rows[listed$.reserve_2[rows] == 0L]
      reserve <- rows[listed$.reserve_2[rows] > 0L]
      reserve <- reserve[order(listed$.reserve_2[reserve])]
      codes <- if (p == 1L) sure_codes else list(main = c(1, 4, 1), reserve = 1)
      code[main] <- codes$main
      code[reserve[seq_along(codes$reserve)]] <- codes$reserve
    }
    listed$code <- code
    execute(listed, response_status = code)
  }
  # The households are its clusters, the ones found ineligible included, as
  # at a first-stage reserve. Every other EA is a cluster of the stratum,
  # its ineligible households included.
  reference <- function(svy) {
    df <- svy$variables
    added <- is.na(df$.sample_id)
    sure <- df$psu == 1L
    df$unit <- ifelse(sure, paste0("hh", df$hh), paste0("ea", df$psu))
    df$unit[added & sure] <- paste0("row", which(added & sure))
    df$stratum <- interaction(df$h, sure)
    survey::svydesign(
      ids = ~unit, strata = ~stratum, weights = ~.weight, data = df
    )
  }
  expect_same_variance <- function(svy) {
    ref <- reference(svy)
    expect_identical(survey::degf(svy), survey::degf(ref))
    expect_equal(
      unname(survey::SE(survey::svytotal(~y, svy, na.rm = TRUE))),
      unname(survey::SE(survey::svytotal(~y, ref, na.rm = TRUE)))
    )
  }

  # Five households worked, the fifth stopping the list, two ineligible.
  settled <- settle(list(main = c(1, 4, 1), reserve = c(4, 1)))
  svy <- as_svydesign(settled)
  data <- svy$variables
  sure <- data$psu == 1L
  expect_identical(data$code[sure & is.na(data$.sample_id)], c(4, 4))
  expect_equal(data$.weight[sure & is.na(data$.sample_id)], c(2, 2))
  expect_equal(sum(data$.weight[sure]), 8)
  expect_true(all(data$.certainty_1[sure]))
  # Each other EA adds its one ineligible household inside its cluster.
  expect_identical(nrow(data), nrow(settled) + 2L + 3L)
  expect_same_variance(svy)

  # A pool with no respondent keeps its eligible households as clusters.
  settled <- settle(list(main = c(2, 4, 2), reserve = c(2, 4, 2)))
  expect_warning(
    svy <- as_svydesign(settled),
    class = "samplyr_warning_reserve_no_respondent"
  )
  data <- svy$variables
  sure <- data$psu == 1L
  expect_identical(sort(data$code[sure]), c(2, 2, 2, 2, 4, 4))
  expect_equal(sum(data$.weight[sure]), 8)
  expect_same_variance(svy)
})

## A settled reserve under a zoned certainty plan. svyplan cuts each
## stratum's remainder into zones, and with one PSU per zone pairs the zones
## into variance groups. Those are the first stage's variance strata.

# Every PSU works its take with one ineligible unit and releases one reserve
# unit. The first PSU drawn at random refuses throughout, so it is left with
# added rows only.
zoned_settled <- function(m) {
  plan <- certainty_zone_fixture(m = m)
  frame <- certainty_element_frame(certainty_zone_register())
  frame$y <- (frame$person * 3 + frame$N) %% 11
  design <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N) |>
    add_stage() |> draw(n = plan, reserve = 3)
  listed <- suppressWarnings(execute(design, frame, seed = 4))
  refused <- sort(unique(listed$psu_id[!listed$.certainty_1]))[1]
  code <- rep(NA, nrow(listed))
  for (p in unique(listed$psu_id)) {
    rows <- which(listed$psu_id == p)
    main <- rows[listed$.reserve_2[rows] == 0L]
    reserve <- rows[listed$.reserve_2[rows] > 0L]
    reserve <- reserve[order(listed$.reserve_2[reserve])]
    if (p == refused) {
      code[c(main, reserve)] <- 2
    } else {
      code[main] <- c(1, 4, rep(1, length(main) - 2L))
      code[reserve[1]] <- 1
    }
  }
  listed$code <- code
  list(
    plan = plan,
    frame = frame,
    refused = refused,
    settled = execute(listed, response_status = code)
  )
}

# The expected variance stratum of each PSU, read from the plan's register:
# its zone within its stratum, or its variance group, and a certainty PSU
# alone.
zoned_strata <- function(plan, m) {
  psu <- plan$psu
  key <- if (m == 1) {
    paste("group", psu$.variance_group)
  } else {
    paste(psu$stratum, "zone", psu$.zone)
  }
  key[psu$certainty] <- paste("sure", psu$psu_id[psu$certainty])
  stats::setNames(key, psu$psu_id)
}

test_that("a settled reserve keeps the zones and variance groups of stage 1", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  reached <- character(0)
  for (m in 1:2) {
    case <- zoned_settled(m)
    expect_warning(
      svy <- as_svydesign(case$settled),
      class = "samplyr_warning_reserve_no_respondent"
    )
    data <- svy$variables
    added <- is.na(data$.sample_id)
    expect_true(any(added & data$psu_id == case$refused))
    expect_false(any(!added & data$psu_id == case$refused))
    expect_true(any(added & data$.certainty_1))
    expect_true(all(is.na(data$y[added])))

    # The export's strata are the plan's, as a partition of the PSUs, with
    # the added rows in the stratum of their own PSU.
    expected <- zoned_strata(case$plan, m)[data$psu_id]
    got <- as.character(svy$strata[[1]])
    expect_identical(nrow(unique(data.frame(expected, got))),
                     length(unique(expected)))
    expect_identical(nrow(unique(data.frame(expected, got))),
                     length(unique(got)))

    # Oracle: the ultimate-cluster variance computed by hand. A PSU drawn
    # at random is a cluster of its zone or group. A certainty PSU is a
    # stratum whose clusters are its rows, each added row one of them.
    sure <- data$.certainty_1
    cluster <- ifelse(sure, paste("row", seq_len(nrow(data))), data$psu_id)
    z <- tapply(data$.weight * ifelse(added, 0, data$y), cluster, sum)
    stratum_of <- tapply(expected, cluster, unique)
    by_hand <- sum(tapply(z, stratum_of[names(z)], function(t) {
      length(t) / (length(t) - 1) * sum((t - mean(t))^2)
    }))
    total <- survey::svytotal(~y, svy, na.rm = TRUE)
    expect_equal(unname(coef(total)), sum(z), tolerance = 1e-12)
    expect_equal(as.numeric(survey::SE(total)), sqrt(by_hand),
                 tolerance = 1e-10)

    # survey on the same rows, with strata and clusters built from the
    # register, agrees for the linearized and the jackknife designs.
    data$ref_stratum <- expected
    data$ref_cluster <- cluster
    ref <- survey::svydesign(
      ids = ~ref_cluster, strata = ~ref_stratum, weights = ~.weight,
      data = data
    )
    expect_equal(
      as.numeric(survey::SE(survey::svytotal(~y, ref, na.rm = TRUE))),
      as.numeric(survey::SE(total)), tolerance = 1e-10
    )
    expect_warning(
      rep <- as_svrepdesign(case$settled, type = "JKn"),
      class = "samplyr_warning_reserve_no_respondent"
    )
    expect_equal(
      as.numeric(survey::SE(survey::svytotal(~y, rep, na.rm = TRUE))),
      as.numeric(survey::SE(survey::svytotal(
        ~y, survey::as.svrepdesign(ref, type = "JKn"), na.rm = TRUE
      ))),
      tolerance = 1e-10
    )

    # The same export from a saved sample and from a replayed receipt.
    path <- withr::local_tempfile(fileext = ".rds")
    saveRDS(case$settled, path)
    expect_warning(
      again <- as_svydesign(readRDS(path)),
      class = "samplyr_warning_reserve_no_respondent"
    )
    expect_identical(as.character(again$strata[[1]]), got)
    json <- withr::local_tempfile(fileext = ".json")
    write_design(case$settled, json, frame = case$frame)
    replayed <- replay_design(read_design(json), case$frame)
    expect_warning(
      again <- as_svydesign(replayed),
      class = "samplyr_warning_reserve_no_respondent"
    )
    expect_identical(as.character(again$strata[[1]]), got)
    reached <- c(reached, paste0("m", m))
  }
  expect_setequal(reached, c("m1", "m2"))
})

test_that("a first-stage reserve exports after its later stage is drawn", {
  skip_if_not_installed("survey")
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(ea_id) |>
    draw(n = c(north = 2, south = 2), reserve = 1) |>
    add_stage() |>
    draw(n = 2)
  frame <- settle_frame()
  listed <- execute(design, frame, stages = 1, seed = 1) |> suppressWarnings()
  north_take <- listed$ea_id[listed$region == "north" & listed$.reserve_1 == 0L]
  listed$access <- ifelse(listed$.reserve_1 == 0L, 1, NA)
  listed$access[listed$ea_id == min(north_take)] <- 4
  listed$access[listed$region == "north" & listed$.reserve_1 == 1L] <- 1
  full <- execute(execute(listed, response_status = access), frame, seed = 2)
  data <- as_svydesign(full)$variables
  zero <- is.na(data$.sample_id)
  # Three EAs listed in the north, stopped at the third, one out of scope:
  # its row carries the list weight 3 / (3 - 1).
  expect_identical(data$region[zero], "north")
  expect_equal(data$.weight[zero], 1.5)
  stage_1 <- tapply(full$.weight_1, full$ea_id, unique)
  north <- unique(full$ea_id[full$region == "north"])
  expect_equal(sum(stage_1[as.character(north)]) + 1.5, 3)
})

test_that("frame columns named like pool statistics keep their values", {
  skip_if_not_installed("survey")
  # A two-stage export with the stratum and EA renamed after statistics.
  renamed <- function(codes) {
    frame <- export_frame()
    names(frame)[1:2] <- c("eligible", "n_respondents")
    design <- sampling_design() |>
      add_stage() |>
      stratify_by(eligible) |>
      cluster_by(n_respondents) |>
      draw(n = c(north = 3, south = 2)) |>
      add_stage() |>
      draw(n = 3, reserve = 3)
    listed <- export_listed(frame, design)
    view <- data.frame(ea_id = listed$n_respondents,
                       .reserve_2 = listed$.reserve_2)
    listed$code <- set_codes(view, export_codes(codes))
    execute(listed, response_status = code)
  }
  same_export <- function(codes) {
    a <- suppressWarnings(as_svydesign(settled_sample(codes)))
    b <- suppressWarnings(as_svydesign(renamed(codes)))
    expect_identical(b$variables$eligible, a$variables$region)
    expect_identical(b$variables$n_respondents, a$variables$ea_id)
    expect_equal(b$variables$.weight, a$variables$.weight)
    expect_equal(survey::SE(survey::svytotal(~code, b, na.rm = TRUE)),
                 survey::SE(survey::svytotal(~code, a, na.rm = TRUE)))
  }
  # A placeholder row, then a primary unit kept as a zero row.
  same_export(no_respondent_codes)
  same_export(list(main = c(4, 4, 4), reserve = c(4, 4, 4)))
  settled <- renamed(no_respondent_codes)
  keys <- attr(settled, "metadata")$reserve_settlement$keys
  expect_identical(keys$n_respondents, c(1L, 2L, 3L, 4L, 5L))
  expect_warning(
    as_svydesign(settled),
    "n_respondents = 3",
    class = "samplyr_warning_reserve_no_respondent"
  )

  # A first-stage reserve, whose zero rows carry the stratum.
  frame <- element_frame()
  names(frame)[1] <- "N"
  listed <- sampling_design() |>
    stratify_by(N) |>
    draw(n = 4, reserve = 4) |>
    execute(frame, seed = 3, frame_digest = "none")
  original <- element_settled()
  listed$code <- NA
  listed$code[order(listed$.sample_id)] <-
    attr(original, "metadata")$reserve_settlement$codes |>
    (\(x) c(NA, 1, 2, 3, 4)[x + 1])()
  settled <- execute(listed, response_status = code)
  a <- as_svydesign(original)$variables
  b <- as_svydesign(settled)$variables
  expect_identical(b$N, a$region)
  expect_equal(b$.weight, a$.weight)
})

## A parent drawn more than once

# Codes each draw of the parent: its first unit of the take fails, the rest
# respond, and its first reserve unit replaces the failed one.
code_each_draw <- function(listed, pool_vars) {
  code <- ifelse(listed$.reserve_2 == 0L, 1, NA)
  pools <- split(seq_len(nrow(listed)), listed[pool_vars], drop = TRUE)
  for (rows in pools) {
    units <- rows[!duplicated(listed$hh[rows])]
    main <- units[listed$.reserve_2[units] == 0L]
    first <- listed$hh[rows] == listed$hh[main[1]]
    code[rows[first]] <- 2
    code[rows[listed$.reserve_2[rows] == 1L]] <- 1
  }
  code
}

test_that("each draw of a parent drawn with replacement settles its own list", {
  skip_if_not_installed("survey")
  frame <- data.frame(ea = rep(1:2, each = 10), hh = 1:20, y = 1)
  listed <- sampling_design() |>
    add_stage() |>
    cluster_by(ea) |>
    draw(n = 4, method = "srswr") |>
    add_stage() |>
    draw(n = 2, reserve = 2) |>
    execute(frame, seed = 1)
  expect_identical(as.vector(table(listed$ea[!duplicated(listed$.draw_1)])),
                   c(3L, 1L))
  listed$code <- code_each_draw(listed, ".draw_1")
  settled <- execute(listed, response_status = code)
  record <- attr(settled, "metadata")$reserve_settlement
  expect_identical(names(record$keys), c("ea", ".draw_1"))
  expect_identical(nrow(record$keys), 4L)
  expect_identical(record$pools$n_respondents, rep(2L, 4))
  svy <- as_svydesign(settled)
  expect_identical(length(unique(svy$cluster[[1]])), 4L)
  expect_equal(unname(coef(survey::svytotal(~y, svy))), 20)
})

test_that("a household listed under two draws of its EA takes two codes", {
  frame <- data.frame(
    ea = rep(1:2, each = 8),
    hh = rep(1:8, each = 2),
    person = 1:16
  )
  listed <- sampling_design() |>
    add_stage() |>
    cluster_by(ea) |>
    draw(n = 4, method = "srswr") |>
    add_stage() |>
    cluster_by(hh) |>
    draw(n = 2, reserve = 2) |>
    execute(frame, seed = 1)
  # Each draw lists all four households of its EA, in its own order.
  expect_true(any(duplicated(listed$ea[!duplicated(listed$.draw_1)])))
  listed$code <- code_each_draw(listed, ".draw_1")
  twice <- tapply(listed$code, listed$hh, function(x) length(unique(x)))
  expect_true(any(twice > 1L))
  settled <- execute(listed, response_status = code)
  pools <- attr(settled, "metadata")$reserve_settlement$pools
  expect_identical(pools$n_respondents, rep(2L, 4))
  expect_true(all(table(settled$.draw_1, settled$hh)[
    table(settled$.draw_1, settled$hh) > 0] == 2L))
})

test_that("a draw of a parent found wholly ineligible stays as a zero total", {
  skip_if_not_installed("survey")
  frame <- data.frame(ea = rep(1:2, each = 10), hh = 1:20, y = 1)
  listed <- sampling_design() |>
    add_stage() |>
    cluster_by(ea) |>
    draw(n = 4, method = "srswr") |>
    add_stage() |>
    draw(n = 2, reserve = 2) |>
    execute(frame, seed = 1)
  listed$code <- code_each_draw(listed, ".draw_1")
  # Draw 3 repeats EA 1, whose draws 1 and 4 keep respondents.
  listed$code[listed$.draw_1 == 3L] <- 4
  settled <- execute(listed, response_status = code)
  expect_false(3L %in% settled$.draw_1)
  expect_true(1L %in% settled$ea)
  svy <- as_svydesign(settled)
  data <- svy$variables
  zero <- data[is.na(data$.sample_id) & data$.draw_1 == 3L, ]
  # Its 4 households worked, all ineligible, stand for its 10.
  expect_identical(zero$ea, rep(1L, 4))
  expect_equal(zero$.weight, rep(0.5 * 10 / 4, 4))
  expect_identical(length(unique(svy$cluster[[1]])), 4L)
})

test_that("a draw with no respondent is a unit apart from the other draws", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # One EA, so both draws take it.
  frame <- data.frame(ea = 1L, hh = 1:10, y = 1)
  listed <- sampling_design() |>
    add_stage() |>
    cluster_by(ea) |>
    draw(n = 2, method = "srswr") |>
    add_stage() |>
    draw(n = 2, reserve = 2) |>
    execute(frame, seed = 1)
  expect_identical(unique(listed$.draw_1), 1:2)
  # Draw 2 reaches no one: every unit it works is an eligible noncontact.
  listed$code <- ifelse(listed$.reserve_2 == 0L, 1, NA)
  second <- listed$.draw_1 == 2L
  listed$code[second] <- 2
  settled <- execute(listed, response_status = code)
  # The pool warns, and the stratum is not lonely: it holds two draws.
  warned <- character(0)
  svy <- withCallingHandlers(
    as_svydesign(settled),
    warning = function(w) {
      warned <<- c(warned, class(w)[1])
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(warned, "samplyr_warning_reserve_no_respondent")
  held <- is.na(svy$variables$.sample_id)
  expect_identical(svy$variables$.draw_1[held], rep(2L, 4))
  expect_equal(sum(svy$variables$.weight[held]), 0.5 * 10)
  expect_identical(length(unique(svy$cluster[[1]])), 2L)
  expect_no_error(survey::svytotal(~y, svy, na.rm = TRUE))
})

## Release ranks belong to the design

# Ten units, a take of 3 and a reserve of 2. The take holds two interviews
# and an eligible non-interview, rank 1 is ineligible and rank 2 an
# interview, so the pool stops at rank 2. Swapping ranks 1 and 2 would stop
# it at rank 1 instead, with weight 10 / 3.
ranked_list <- function() {
  listed <- sampling_design() |>
    draw(n = 3, reserve = 2) |>
    execute(data.frame(id = 1:10), seed = 1)
  code <- rep(NA_real_, nrow(listed))
  code[listed$.reserve_1 == 0L] <- c(1, 1, 2.1)
  code[listed$.reserve_1 == 1L] <- 4.7
  code[listed$.reserve_1 == 2L] <- 1
  listed$code <- code
  listed
}

swap_ranks <- function(rank) {
  ifelse(rank %in% 1:2, 3L - rank, rank)
}

test_that("a clean list settles at the weight of its stop", {
  settled <- execute(ranked_list(), response_status = code)
  expect_identical(nrow(settled), 3L)
  expect_equal(settled$.weight, rep(2.5, 3))
})

test_that("an edited release rank is refused at settlement", {
  listed <- ranked_list()
  swapped <- dplyr::mutate(listed, .reserve_1 = swap_ranks(.reserve_1))
  expect_error(
    execute(swapped, response_status = code),
    class = "samplyr_error_modified_sample"
  )
  assigned <- listed
  assigned$.reserve_1 <- swap_ranks(assigned$.reserve_1)
  expect_error(
    execute(assigned, response_status = code),
    class = "samplyr_error_modified_sample"
  )
})

test_that("a dropped or renamed release rank is refused at settlement", {
  listed <- ranked_list()
  dropped <- listed
  dropped$.reserve_1 <- NULL
  changed <- list(
    dplyr::select(listed, -.reserve_1),
    dropped,
    dplyr::rename(listed, rank = .reserve_1)
  )
  for (x in changed) {
    expect_error(
      execute(x, response_status = code),
      class = "samplyr_error_modified_sample"
    )
  }
})

test_that("codes and row order stay free to change before settlement", {
  listed <- ranked_list()
  recoded <- dplyr::mutate(listed, code = ifelse(code == 4.7, 4.1, code))
  reordered <- recoded[rev(seq_len(nrow(recoded))), ]
  expected <- execute(listed, response_status = code)
  for (x in list(recoded, reordered)) {
    settled <- execute(x, response_status = code)
    expect_identical(sort(settled$id), sort(expected$id))
    expect_equal(settled$.weight, rep(2.5, 3))
  }
})

test_that("a release rank edited after settlement blocks the export", {
  skip_if_not_installed("survey")
  settled <- execute(ranked_list(), response_status = code)
  expect_no_error(as_svydesign(settled))
  settled$.reserve_1 <- 0L
  expect_error(as_svydesign(settled), class = "samplyr_error_modified_sample")
})

## A list is used only once settled

test_that("a new phase, shared weights and frame stacks wait for settlement", {
  listed <- ranked_list()
  expect_error(
    sampling_design() |> draw(n = 2) |> execute(listed, seed = 2),
    "cannot draw a new phase from a sample whose reserve",
    class = "samplyr_error_reserve_unsettled"
  )
  people <- data.frame(person_id = 1:20, home = rep(1:10, each = 2))
  links <- data.frame(id = rep(1:10, each = 2), person_id = 1:20)
  share <- function(x) {
    share_weights(
      x,
      targets = people,
      links = links,
      by = c(id = "id"),
      to = c(person_id = "person_id"),
      within = home,
      multiplicity = complete_links()
    )
  }
  expect_error(share(listed), class = "samplyr_error_reserve_unsettled")

  pop <- data.frame(
    id = 1:30,
    in_a = rep(c(TRUE, FALSE, TRUE), 10),
    in_b = rep(c(FALSE, TRUE, TRUE), 10)
  )
  stack <- function(a) {
    b <- sampling_design() |> draw(n = 6) |> execute(pop[pop$in_b, ], seed = 2)
    stack_frames(a = a, b = b, membership = c(a = "in_a", b = "in_b"), key = id)
  }
  frame_a <- pop[pop$in_a, ]
  list_a <- sampling_design() |>
    draw(n = 6, reserve = 2) |>
    execute(frame_a, seed = 1)
  expect_error(stack(list_a), class = "samplyr_error_reserve_unsettled")

  # Once settled, each route takes the sample.
  settled <- execute(listed, response_status = code)
  phase <- sampling_design() |> draw(n = 2) |> execute(settled, seed = 2)
  expect_identical(nrow(phase), 2L)
  expect_true(all(phase$id %in% settled$id))
  expect_s3_class(share(settled), "tbl_sample")
  list_a$code <- ifelse(list_a$.reserve_1 == 0L, 1, NA)
  expect_s3_class(stack(execute(list_a, response_status = code)), "frame_stack")
})

test_that("a replicated list settles one replicate at a time", {
  listed <- sampling_design() |>
    draw(n = 3, reserve = 2) |>
    execute(data.frame(id = 1:10), seed = 1, reps = 2)
  listed$code <- ifelse(listed$.reserve_1 == 0L, 1, NA)
  expect_error(
    execute(listed, response_status = code),
    class = "samplyr_error_replicated_sample_unsupported"
  )
  for (r in 1:2) {
    one <- dplyr::filter(listed, .replicate == r)
    settled <- execute(one, response_status = code)
    expect_identical(
      sort(settled$id),
      sort(one$id[one$.reserve_1 == 0L])
    )
    expect_equal(settled$.weight, rep(10 / 3, 3))
  }
})

## Parents taken whole by a census

# Every main unit responds, so the settled list adds no rows.
settle_take <- function(sample, k = 2L) {
  sample$code <- ifelse(sample[[paste0(".reserve_", k)]] == 0L, 1, NA)
  execute(sample, response_status = code)
}

census_parents <- function(method = "srswor", n_ea = 3L, y = NULL) {
  frame <- expand.grid(hh = 1:10, ea = seq_len(n_ea))
  frame$mos <- 10
  frame$y <- y %||% ((frame$hh * 7 + frame$ea * 3) %% 11)
  first <- sampling_design() |> add_stage() |> cluster_by(ea)
  first <- if (identical(method, "srswor")) {
    draw(first, n = n_ea)
  } else {
    draw(first, n = n_ea, method = method, mos = mos)
  }
  listed <- suppressMessages(
    execute(first |> add_stage() |> draw(n = 3, reserve = 2), frame, seed = 4)
  )
  settle_take(listed)
}

svy_total <- function(svy) {
  est <- survey::svytotal(~y, svy, na.rm = TRUE)
  c(total = unname(stats::coef(est)), se = unname(survey::SE(est)))
}

test_that("a parent taken by an SRS census represents itself in a settled reserve", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # Each EA is the same in every sample, so a constant outcome has no variance.
  for (method in c("srswor", "pps_brewer")) {
    for (n_ea in c(1L, 3L)) {
      settled <- census_parents(method, n_ea, y = rep(seq_len(n_ea), each = 10))
      expect_equal(
        svy_total(as_svydesign(settled)),
        c(total = 10 * sum(seq_len(n_ea)), se = 0)
      )
    }
    settled <- census_parents(method, 3L, y = rep(1:3, each = 10))
    expect_equal(
      svy_total(as_svrepdesign(settled, type = "JKn")),
      c(total = 60, se = 0)
    )
  }

  # Its households are the clusters of a stratum of its own.
  for (method in c("srswor", "pps_brewer")) {
    settled <- census_parents(method, 3L)
    reference <- survey::svydesign(
      ids = ~hh, strata = ~ea, weights = ~.weight, nest = TRUE,
      data = as.data.frame(settled)
    )
    expect_equal(svy_total(as_svydesign(settled)), svy_total(reference))
    expect_equal(
      svy_total(as_svrepdesign(settled, type = "JKn")),
      svy_total(reference)
    )
  }
})

test_that("only the census stratum of a settled reserve represents itself", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  frame <- expand.grid(hh = 1:10, ea = 1:6)
  frame$region <- ifelse(frame$ea <= 2L, "A", "B")
  frame$y <- (frame$hh * 7 + frame$ea * 3) %% 11
  listed <- sampling_design() |>
    add_stage() |> stratify_by(region) |> cluster_by(ea) |>
    draw(n = c(A = 2, B = 2)) |>
    add_stage() |> draw(n = 3, reserve = 2) |>
    execute(frame, seed = 5)
  settled <- settle_take(listed)

  df <- as.data.frame(settled)
  census <- df$region == "A"
  df$stratum <- ifelse(census, paste0("A", df$ea), "B")
  df$cluster <- ifelse(census, paste0("hh", df$ea, ".", df$hh), paste0("ea", df$ea))
  reference <- survey::svydesign(
    ids = ~cluster, strata = ~stratum, weights = ~.weight, data = df
  )
  expect_equal(svy_total(as_svydesign(settled)), svy_total(reference))
})

test_that("added rows under a census parent join its stratum", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  for (method in c("srswor", "pps_brewer")) {
    frame <- expand.grid(hh = 1:10, ea = 1:3)
    frame$mos <- 10
    frame$y <- frame$hh
    first <- sampling_design() |> add_stage() |> cluster_by(ea)
    first <- if (identical(method, "srswor")) {
      draw(first, n = 3)
    } else {
      draw(first, n = 3, method = method, mos = mos)
    }
    listed <- suppressMessages(
      execute(first |> add_stage() |> draw(n = 3, reserve = 2), frame, seed = 4)
    )
    # In EA 1 one main unit is ineligible, and in EA 2 nobody responds.
    rank <- listed$.reserve_2
    listed$code <- ifelse(rank == 0L, 1, NA)
    in_1 <- which(listed$ea == 1L & rank == 0L)
    listed$code[in_1[1]] <- 4
    in_2 <- listed$ea == 2L
    listed$code[in_2] <- 2
    settled <- suppressWarnings(execute(listed, response_status = code))
    svy <- suppressWarnings(as_svydesign(settled))
    # One stratum per EA, and every row of an EA, added or not, in it.
    by_ea <- unique(data.frame(ea = svy$variables$ea, stratum = svy$strata[, 1]))
    expect_identical(nrow(by_ea), 3L, label = method)
    expect_false(anyDuplicated(by_ea$stratum) > 0L)
  }
})

test_that("expected hits and settled weights of 1 are no census", {
  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "fail")
  # A with-replacement stage 1 whose every EA expects one hit.
  frame <- expand.grid(hh = 1:10, ea = 1:3)
  frame$y <- (frame$hh * 7 + frame$ea * 3) %% 11
  listed <- suppressMessages(
    sampling_design() |>
      add_stage() |> cluster_by(ea) |> draw(n = 3, method = "srswr") |>
      add_stage() |> draw(n = 3, reserve = 2) |>
      execute(frame, seed = 6)
  )
  settled <- settle_take(listed)
  expect_identical(unique(settled$.weight_1), 1)
  expect_equal(
    svy_total(as_svydesign(settled)),
    c(total = 156.666666666667, se = 29.0593262903)
  )

  # A stage-1 reserve whose list covers the frame and whose settled weight
  # is 1. One EA of the take is ineligible, and every other EA responds.
  frame <- expand.grid(hh = 1:6, ea = 1:4)
  frame$y <- (frame$hh * 7 + frame$ea * 3) %% 11
  first <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 2, reserve = Inf) |>
    add_stage() |> draw(n = 2) |>
    execute(frame, stages = 1, seed = 7)
  take <- first$ea[first$.reserve_1 == 0L]
  first$code <- ifelse(first$ea == min(take), 4, 1)
  settled <- execute(first, response_status = code)
  expect_identical(unique(settled$.weight_1), 1)
  second <- execute(settled, frame, seed = 8)
  expect_equal(
    svy_total(as_svydesign(second)),
    c(total = 45, se = 25.980762113533)
  )
})

test_that("a replay rebuilds the response status its export codes", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # The receipt held the categories alone. The response status column is
  # not in the frame, so a replay settled without it and its export left
  # the added rows uncoded: an ineligible unit could not be told from an
  # eligible nonrespondent, and the adjustment below could not be run.
  as_kind <- list(
    double = identity,
    integer = function(code) as.integer(floor(code)),
    words = code_words,
    digits = function(code) ifelse(is.na(code), NA, as.character(code)),
    factor_words = function(code) factor(code_words(code)),
    factor_digits = function(code) factor(ifelse(is.na(code), NA,
                                                 as.character(code)))
  )
  category <- function(x) samplyr:::reserve_outcome_category(x)$category
  adjusted <- function(svy) {
    rep <- survey::as.svrepdesign(svy, type = "JKn")
    rep <- update(rep, status = category(code))
    weights(svrep::redistribute_weights(
      rep, reduce_if = status == 2, increase_if = status == 1, by = "region"
    ), type = "sampling")
  }
  checked <- character(0)
  for (kind in names(as_kind)) {
    listed <- export_listed()
    listed$code <- as_kind[[kind]](set_codes(listed,
                                             export_codes(no_respondent_codes)))
    settled <- execute(listed, response_status = code)
    path <- withr::local_tempfile(fileext = ".json")
    write_design(settled, path, frame = export_frame())
    receipt <- jsonlite::fromJSON(path)$execution$settlement
    expect_identical(receipt$column, "code", info = kind)
    replayed <- replay_design(read_design(path), export_frame())
    original <- suppressWarnings(as_svydesign(settled))
    again <- suppressWarnings(as_svydesign(replayed))
    a <- original$variables
    b <- again$variables
    expect_identical(nrow(b), nrow(a), info = kind)
    expect_equal(b$.weight, a$.weight, info = kind)
    expect_identical(class(b$code), class(a$code), info = kind)
    added <- is.na(a$.sample_id)
    expect_true(any(category(a$code[added]) == 2L), info = kind)
    expect_true(any(category(a$code[added]) == 4L), info = kind)
    expect_identical(b$code[added], a$code[added], info = kind)
    expect_identical(category(b$code), category(a$code), info = kind)
    # A respondent's code is rebuilt as its category, in the column's form.
    respondent <- if (kind %in% c("words", "factor_words")) "interview" else "1"
    expect_identical(unique(as.character(b$code[!added])), respondent,
                     info = kind)
    expect_equal(adjusted(again), adjusted(original), info = kind)
    checked <- c(checked, kind)
  }
  expect_setequal(checked, names(as_kind))
})

test_that("a user column named after the added rows' unit column is kept", {
  skip_if_not_installed("survey")
  # The export numbered each added row's recorded unit in `.reserve_unit`
  # and then removed that column. A user column of that name was lost, or
  # failed to bind when it was not an integer.
  settle <- function(extra, drop) {
    frame <- data.frame(id = 1:12, y = 1:12)
    for (nm in names(extra)) frame[[nm]] <- extra[[nm]]
    s <- execute(sampling_design() |> draw(n = 4, reserve = 2), frame,
                 seed = 1)
    s$code <- ifelse(s$.reserve_1 == 0, "interview", NA_character_)
    if (drop) {
      s$code[which(s$.reserve_1 == 0)[1]] <- "ineligible"
      s$code[s$.reserve_1 == 1] <- "interview"
    }
    suppressWarnings(execute(s, response_status = code))
  }
  export <- function(x) suppressWarnings(as_svydesign(x))
  checked <- character(0)
  for (drop in c(FALSE, TRUE)) {
    base <- export(settle(list(), drop))
    expect_identical(any(is.na(base$variables$.sample_id)), drop)
    extras <- list(
      numeric = list(.reserve_unit = 101:112),
      character = list(.reserve_unit = letters[1:12]),
      two = list(.reserve_unit = letters[1:12], .reserve_unit1 = 201:212)
    )
    for (kind in names(extras)) {
      x <- settle(extras[[kind]], drop)
      svy <- export(x)
      v <- svy$variables
      sampled <- !is.na(v$.sample_id)
      for (nm in names(extras[[kind]])) {
        expect_identical(v[[nm]][sampled],
                         extras[[kind]][[nm]][v$id[sampled]])
        expect_true(all(is.na(v[[nm]][!sampled])))
      }
      expect_identical(stats::weights(svy), stats::weights(base))
      expect_identical(attr(svy, "samplyr_reserve_units"),
                       attr(base, "samplyr_reserve_units"))
      checked <- c(checked, paste(kind, drop))
    }
  }
  expect_setequal(checked, c("numeric FALSE", "character FALSE",
                             "two FALSE", "numeric TRUE", "character TRUE",
                             "two TRUE"))

  # The response status may itself be called `.reserve_unit`.
  frame <- data.frame(id = 1:12, y = 1:12)
  s <- execute(sampling_design() |> draw(n = 4, reserve = 2), frame,
               seed = 1)
  s$.reserve_unit <- ifelse(s$.reserve_1 == 0, "interview", NA_character_)
  s$.reserve_unit[which(s$.reserve_1 == 0)[1]] <- "ineligible"
  s$.reserve_unit[s$.reserve_1 == 1] <- "interview"
  svy <- export(suppressWarnings(execute(s, response_status = .reserve_unit)))
  base <- export(settle(list(), TRUE))
  added <- is.na(svy$variables$.sample_id)
  expect_identical(svy$variables$.reserve_unit[added], "ineligible")
  expect_identical(stats::weights(svy), stats::weights(base))
  expect_identical(attr(svy, "samplyr_reserve_units"),
                   attr(base, "samplyr_reserve_units"))
})

test_that("the reserve note reads the rates each plan used", {
  plan_message <- function(design) {
    w <- expect_warning(design, class = "samplyr_warning_reserve_plan_rates")
    cli::ansi_strip(conditionMessage(w))
  }

  # n_cluster() with indicator rows: each row carries its own stage rates.
  indicators <- data.frame(
    name = c("early_nonresponse", "final_nonresponse"),
    p = c(0.35, 0.45), cv = 0.08, icc_1 = 0.05,
    resp_rate_1 = c(0.75, 1), resp_rate = c(1, 0.60)
  )
  cluster <- svyplan::n_cluster(indicators, stage_cost = c(500, 50))
  first <- sampling_design() |> cluster_by(psu)
  msg <- plan_message(first |> draw(n = cluster, reserve = 2))
  expect_match(msg, "resp_rate_1 = 0.75` for indicator early_nonresponse",
               fixed = TRUE)
  msg <- plan_message(
    first |> draw(n = 5) |> add_stage() |> draw(n = cluster, reserve = 2)
  )
  expect_match(msg, "resp_rate = 0.6` for indicator final_nonresponse",
               fixed = TRUE)
  # A flat total is inflated by every stage's rate.
  msg <- plan_message(sampling_design() |> draw(n = cluster, reserve = 2))
  expect_match(msg, "resp_rate = 0.6`", fixed = TRUE)
  expect_no_warning(first |> draw(n = cluster))

  # A classic rate vector takes precedence over the frame column.
  frame <- data.frame(stratum = c("A", "B"), N = c(4000, 3000),
                      sd = c(10, 15), mean = c(50, 60), resp_rate = 0.5)
  ones <- svyplan::n_alloc(frame, cv = 0.02, resp_rate = c(1, 1))
  expect_identical(ones$rates$resp_rate, c(1, 1))
  expect_no_warning(
    sampling_design() |> stratify_by(stratum) |> draw(n = ones, reserve = 2)
  )
  lower <- svyplan::n_alloc(frame, cv = 0.02, resp_rate = c(1, 0.8))
  msg <- plan_message(
    sampling_design() |> stratify_by(stratum) |> draw(n = lower, reserve = 2)
  )
  expect_match(msg, "resp_rate = 0.8` in stratum B", fixed = TRUE)

  # A joint measure value of 1 wins over a lower frame rate, a missing one
  # falls back to the frame, and an indicator no target reads is ignored.
  joint_frame <- data.frame(stratum = c("A", "B"), N = c(4000, 3000),
                            resp_rate = c(0.7, 0.6))
  measures <- data.frame(
    stratum = rep(c("A", "B"), 2), name = rep(c("y", "z"), each = 2),
    p = c(0.5, 0.4, 0.3, 0.2), resp_rate = c(1, 1, 0.5, 0.5)
  )
  targets <- data.frame(name = "y", cv = 0.1)
  held <- svyplan::n_alloc_joint(joint_frame, measures, targets = targets)
  expect_identical(held$rates$name, c("y", "y"))
  expect_no_warning(
    sampling_design() |> stratify_by(stratum) |> draw(n = held, reserve = 2)
  )
  measures$resp_rate[2] <- NA
  fallback <- svyplan::n_alloc_joint(joint_frame, measures, targets = targets)
  msg <- plan_message(
    sampling_design() |> stratify_by(stratum) |>
      draw(n = fallback, reserve = 2)
  )
  expect_match(msg, "resp_rate = 0.6` in stratum B for indicator y",
               fixed = TRUE)

  # Final eligibility, as well as response.
  scoped <- svyplan::n_alloc_joint(
    joint_frame[c("stratum", "N")], measures[1:2, c("stratum", "name", "p")],
    targets = targets, eligibility_rate = 0.9
  )
  msg <- plan_message(
    sampling_design() |> stratify_by(stratum) |> draw(n = scoped, reserve = 2)
  )
  expect_match(msg, "eligibility_rate = 0.9`", fixed = TRUE)

  # A free take reports its PSU rate per stratum and indicator.
  free <- svyplan::n_alloc_joint(
    data.frame(stratum = c("A", "B"), N = c(20000, 8000), N_1 = c(400, 160),
               cost_1 = 400, cost_2 = 40),
    data.frame(stratum = c("A", "B"), name = "coverage", p = c(0.5, 0.4),
               icc_1 = 0.05, resp_rate_1 = c(0.9, 1)),
    targets = data.frame(name = "coverage", cv = 0.1),
    take_2_min = 2, take_2_max = 20
  )
  with_reserve <- function(reserve) {
    suppressWarnings(
      sampling_design() |> stratify_by(stratum) |> cluster_by(ea) |>
        draw(n = free, reserve = reserve)
    )
  }
  msg <- plan_message(
    sampling_design() |> stratify_by(stratum) |> cluster_by(ea) |>
      draw(n = free, reserve = 2)
  )
  expect_match(msg, "resp_rate_1 = 0.9` in stratum A for indicator coverage",
               fixed = TRUE)
  # The note leaves the fielded counts as the plan gives them.
  expect_identical(with_reserve(2)$stages[[1]]$draw_spec$n,
                   with_reserve(NULL)$stages[[1]]$draw_spec$n)
})

test_that("a slack indicator with a low rate warns", {
  plan_message <- function(design) {
    w <- expect_warning(design, class = "samplyr_warning_reserve_plan_rates")
    cli::ansi_strip(conditionMessage(w))
  }
  rates_warn <- function(design) {
    w <- tryCatch(
      {
        design
        NULL
      },
      samplyr_warning_reserve_plan_rates = function(w) w
    )
    !is.null(w)
  }
  fielded <- function(design) {
    suppressWarnings(design)$stages[[1]]$draw_spec$n
  }

  # n_alloc_joint(): the target of z has slack, but its rate is read.
  frame <- data.frame(stratum = c("A", "B"), N = c(4000, 3000))
  measures <- data.frame(
    stratum = rep(c("A", "B"), 2), name = rep(c("y", "z"), each = 2),
    p = c(0.5, 0.4, 0.3, 0.2), resp_rate = c(1, 1, 0.3, 0.3)
  )
  targets <- data.frame(name = c("y", "z"), cv = c(0.05, 0.3))
  strata <- sampling_design() |> stratify_by(stratum)
  slack <- svyplan::n_alloc_joint(frame, measures, targets = targets)
  expect_identical(slack$constraints$.binding, c(TRUE, FALSE))
  msg <- plan_message(strata |> draw(n = slack, reserve = 2))
  expect_match(msg, "resp_rate = 0.3` in stratum A for indicator z",
               fixed = TRUE)
  expect_false(rates_warn(strata |> draw(n = slack)))
  expect_identical(fielded(strata |> draw(n = slack, reserve = 2)),
                   fielded(strata |> draw(n = slack)))
  ones <- svyplan::n_alloc_joint(frame, transform(measures, resp_rate = 1),
                                 targets = targets)
  expect_false(rates_warn(strata |> draw(n = ones, reserve = 2)))
  # An indicator no target reads has no rate in the fit.
  untargeted <- svyplan::n_alloc_joint(frame, measures, targets = targets[1, ])
  expect_identical(unique(untargeted$rates$name), "y")
  expect_false(rates_warn(strata |> draw(n = untargeted, reserve = 2)))

  # A target of y binds in N and has slack in S, and the floor sets B.
  # The South rate leaves the counts as they are and is still read.
  zoned <- data.frame(stratum = c("A", "B"), region = c("N", "S"),
                      N = c(4000, 3000))
  by_region <- data.frame(stratum = c("A", "B"), name = "y", p = c(0.5, 0.4),
                          resp_rate = c(1, 0.2))
  domain_targets <- data.frame(name = "y", domain = "region",
                               level = c("N", "S"), cv = c(0.05, 0.9))
  split <- svyplan::n_alloc_joint(zoned, by_region, targets = domain_targets,
                                  min_n_stratum = 30)
  neutral <- svyplan::n_alloc_joint(
    zoned, transform(by_region, resp_rate = 1), targets = domain_targets,
    min_n_stratum = 30
  )
  expect_identical(split$constraints$.binding, c(TRUE, FALSE))
  expect_identical(split$detail$n_int, neutral$detail$n_int)
  msg <- plan_message(strata |> draw(n = split, reserve = 2))
  expect_match(msg, "resp_rate = 0.2` in stratum B for indicator y",
               fixed = TRUE)
  expect_identical(fielded(strata |> draw(n = split, reserve = 2)),
                   fielded(strata |> draw(n = neutral, reserve = 2)))
  expect_false(rates_warn(strata |> draw(n = neutral, reserve = 2)))

  # Under a budget the objective's rates are read.
  spent <- svyplan::n_alloc_joint(
    frame, transform(measures, resp_rate = c(0.7, 0.7, 1, 1)),
    objective = data.frame(name = "y"), budget = 500
  )
  expect_identical(nrow(spent$constraints), 0L)
  msg <- plan_message(strata |> draw(n = spent, reserve = 2))
  expect_match(msg, "resp_rate = 0.7` in stratum A for indicator y",
               fixed = TRUE)
  expect_identical(fielded(strata |> draw(n = spent, reserve = 2)),
                   fielded(strata |> draw(n = spent)))

  # n_cluster(): a sets the size, and the rate of b is read.
  indicators <- data.frame(name = c("a", "b"), p = c(0.3, 0.5),
                           cv = c(0.08, 0.3), icc_1 = 0.05,
                           resp_rate = c(1, 0.3))
  stage_two <- sampling_design() |> cluster_by(psu) |> draw(n = 5) |>
    add_stage()
  cluster <- svyplan::n_cluster(indicators, stage_cost = c(500, 50))
  expect_identical(cluster$detail$.binding, c(TRUE, FALSE))
  msg <- plan_message(stage_two |> draw(n = cluster, reserve = 2))
  expect_match(msg, "resp_rate = 0.3` for indicator b", fixed = TRUE)
  msg <- plan_message(sampling_design() |> draw(n = cluster, reserve = 2))
  expect_match(msg, "resp_rate = 0.3` for indicator b", fixed = TRUE)
  expect_false(rates_warn(stage_two |> draw(n = cluster)))
  expect_identical(
    fielded(sampling_design() |> draw(n = cluster, reserve = 2)),
    fielded(sampling_design() |> draw(n = cluster))
  )
  ones <- svyplan::n_cluster(transform(indicators, resp_rate = 1),
                             stage_cost = c(500, 50))
  expect_false(rates_warn(stage_two |> draw(n = ones, reserve = 2)))
  # svyplan labels an unnamed indicator by its row.
  unnamed <- svyplan::n_cluster(indicators[-1], stage_cost = c(500, 50))
  msg <- plan_message(stage_two |> draw(n = unnamed, reserve = 2))
  expect_match(msg, "resp_rate = 0.3` for indicator 2.", fixed = TRUE)

  # n_multi(): a row below the largest requirement cannot change the take.
  multi <- svyplan::n_multi(data.frame(p = c(0.1, 0.5), moe = 0.06,
                                       resp_rate = c(0.5, 1)))
  expect_identical(multi$detail$.binding, c(FALSE, TRUE))
  expect_false(rates_warn(sampling_design() |> draw(n = multi, reserve = 2)))
})

test_that("the reserve note reads the indicators that set an n_multi() take", {
  plan_message <- function(design) {
    w <- expect_warning(design, class = "samplyr_warning_reserve_plan_rates")
    cli::ansi_strip(conditionMessage(w))
  }
  rates_warn <- function(plan, design = sampling_design()) {
    w <- tryCatch(
      {
        design |> draw(n = plan, reserve = 2)
        NULL
      },
      samplyr_warning_reserve_plan_rates = function(w) w
    )
    !is.null(w)
  }

  # The plan keeps its rates per indicator and none in params.
  indicators <- data.frame(name = c("a", "b"), p = c(0.3, 0.5), moe = 0.06)
  net <- svyplan::n_multi(indicators)
  expect_identical(as.integer(net), 267L)
  expect_false(rates_warn(net))

  gross <- svyplan::n_multi(indicators, resp_rate = 0.5)
  expect_identical(as.integer(gross), 534L)
  msg <- plan_message(sampling_design() |> draw(n = gross, reserve = 2))
  expect_match(msg, "resp_rate = 0.5` for indicator b", fixed = TRUE)
  expect_no_warning(sampling_design() |> draw(n = gross))

  column <- svyplan::n_multi(transform(indicators, resp_rate = c(1, 0.5)))
  msg <- plan_message(sampling_design() |> draw(n = column, reserve = 2))
  expect_match(msg, "resp_rate = 0.5` for indicator b", fixed = TRUE)

  scoped <- svyplan::n_multi(indicators, eligibility_rate = 0.5)
  msg <- plan_message(sampling_design() |> draw(n = scoped, reserve = 2))
  expect_match(msg, "eligibility_rate = 0.5` for indicator b", fixed = TRUE)
  expect_no_match(msg, "resp_rate")

  # Indicator a needs 192 at its rate and b sets the take of 267 alone.
  loose <- data.frame(name = c("a", "b"), p = c(0.1, 0.5), moe = 0.06,
                      resp_rate = c(0.5, 1))
  expect_identical(as.integer(svyplan::n_multi(loose)), 267L)
  expect_false(rates_warn(svyplan::n_multi(loose)))

  # An unnamed indicator is named by its row.
  unnamed <- loose[c("p", "moe")]
  unnamed$resp_rate <- c(1, 0.5)
  msg <- plan_message(
    sampling_design() |> draw(n = svyplan::n_multi(unnamed), reserve = 2)
  )
  expect_match(msg, "resp_rate = 0.5` for indicator 2", fixed = TRUE)

  # Tied to 13 digits, svyplan flags b as binding, but a's rate of 0.5
  # sets the same take.
  tied <- svyplan::n_multi(data.frame(
    name = c("b", "a"), p = 0.5, moe = c(0.06 / sqrt(2), 0.06),
    resp_rate = c(1, 0.5)
  ))
  expect_identical(tied$detail$.binding, c(TRUE, FALSE))
  msg <- plan_message(sampling_design() |> draw(n = tied, reserve = 2))
  expect_match(msg, "resp_rate = 0.5` for indicator a", fixed = TRUE)

  # Per domain, the indicator that sets each domain's size.
  domains <- data.frame(
    name = rep(c("a", "b"), 2), p = c(0.1, 0.5, 0.1, 0.5), moe = 0.06,
    region = rep(c("N", "S"), each = 2), resp_rate = c(0.5, 1, 0.2, 1)
  )
  by_region <- sampling_design() |> stratify_by(region)
  split <- svyplan::n_multi(domains, domains = "region")
  expect_identical(split$domains$.binding, c("b", "a"))
  msg <- plan_message(by_region |> draw(n = split, reserve = 2))
  expect_match(msg, "resp_rate = 0.2` for indicator a in region S",
               fixed = TRUE)
  expect_no_match(msg, "lowest value")
  unnamed_domains <- domains[setdiff(names(domains), "name")]
  msg <- plan_message(
    by_region |>
      draw(n = svyplan::n_multi(unnamed_domains, domains = "region"),
           reserve = 2)
  )
  expect_match(msg, "resp_rate = 0.2` for indicator 3 in region S",
               fixed = TRUE)
  natural <- svyplan::n_multi(
    transform(domains, share = rep(c(0.7, 0.3), each = 2)),
    domains = "region", domain_sampling = "natural"
  )
  msg <- plan_message(by_region |> draw(n = natural, reserve = 2))
  expect_match(msg, "resp_rate = 0.2` for indicator a in region S",
               fixed = TRUE)

  # In N indicator a does not bind, and S has no rate below 1.
  domains$resp_rate <- c(0.5, 1, 1, 1)
  expect_false(
    rates_warn(svyplan::n_multi(domains, domains = "region"), by_region)
  )
  # A domain raised to min_n_domain is not sized by any rate.
  domains$resp_rate <- c(0.5, 1, 0.2, 1)
  floored <- svyplan::n_multi(domains, domains = "region", min_n_domain = 2000)
  expect_identical(floored$domains$.binding, rep("(min_n_domain)", 2))
  expect_false(rates_warn(floored, by_region))
})

test_that("a status factor with an empty level is written and read back", {
  listed <- listed_sample()
  codes <- set_codes(listed, settle_codes(3))
  words <- c("interview", "noninterview", "unknown", "ineligible")
  # read.csv() gives an empty field its own level.
  listed$status <- factor(words[floor(codes)], levels = c("", words))
  settled <- execute(listed, response_status = status)
  json <- suppressWarnings(design_json(settled))
  expect_true(grepl("\"levels\":[\"\",", json, fixed = TRUE))
  expect_s3_class(read_design(json), "sampling_design")
})

test_that("a stack refuses the ineligible row of a pool of unknowns", {
  skip_if_not_installed("survey")
  # EA 3's estimated ineligible households have no record, so their
  # membership in frame B is unknown.
  frame <- export_frame()
  frame$uid <- paste(frame$ea_id, frame$hh_id)
  frame$in_A <- TRUE
  frame$in_B <- frame$hh_id %% 2 == 0
  listed <- export_listed(frame)
  listed$code <- set_codes(
    listed, export_codes(list(main = c(3, 3, 3), reserve = c(3, 3, 3)))
  )
  settled <- execute(listed, response_status = code)
  other <- execute(sampling_design() |> draw(n = 10), frame[frame$in_B, ],
                   seed = 3)
  stack <- stack_frames(A = settled, B = other, key = uid,
                        membership = c(A = "in_A", B = "in_B"))
  expect_error(suppressWarnings(as_svrepdesign(stack, type = "auto")),
               class = "samplyr_error_stack_frames_membership")
})
