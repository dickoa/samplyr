## Certainty-plan bridge. draw(n = plan) at a clustered, stratified stage 1
## fields the plan's certainty PSUs with probability one, draws the rest at
## exactly n_psu_draw, and refuses what it cannot field before any RNG.

test_that("a certainty plan is fielded exactly at a clustered stage 1", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()

  s <- sampling_design("bridge") |>
    add_stage("psu") |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    execute(frame, seed = 7)

  psu <- unique(s[, c("psu_id", "stratum", "N", ".weight_1", ".certainty_1")])

  got <- table(psu$stratum)
  want <- plan$detail$n_psu_certain + plan$detail$n_psu_draw
  expect_equal(as.integer(got[plan$detail$stratum]), as.integer(want))

  expect_setequal(
    psu$psu_id[psu$.certainty_1],
    plan$psu$psu_id[plan$psu$certainty]
  )
  expect_equal(unique(psu$.weight_1[psu$.certainty_1]), 1)

  # Drawn PSUs carry pik = n_psu_draw * N_i / sum(N_rest).
  register <- certainty_plan_register()
  for (h in c("A", "B")) {
    cert_ids <- plan$psu$psu_id[plan$psu$certainty & plan$psu$stratum == h]
    rest <- register[register$stratum == h & !register$psu_id %in% cert_ids, ]
    drawn <- psu[psu$stratum == h & !psu$.certainty_1, ]
    n_draw <- plan$detail$n_psu_draw[plan$detail$stratum == h]
    expect_equal(drawn$.weight_1, sum(rest$N) / (n_draw * drawn$N))
  }

  # A cluster stage keeps every element of a selected PSU.
  expect_equal(nrow(s), sum(psu$N))
})

test_that("the bridge fields a PSU-level frame the same way", {
  plan <- certainty_plan_fixture()
  register <- certainty_plan_register()

  s <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N) |>
    execute(register, seed = 11)

  expect_equal(
    as.integer(table(s$stratum)[plan$detail$stratum]),
    as.integer(plan$detail$n_psu_certain + plan$detail$n_psu_draw)
  )
  expect_true(all(plan$psu$psu_id[plan$psu$certainty] %in% s$psu_id))
})

test_that("every replicate holds the certainty PSUs", {
  plan <- certainty_plan_fixture()
  register <- certainty_plan_register()

  s <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    execute(register, seed = 3, reps = 2)

  by_rep <- split(s$psu_id, s$.replicate)
  expect_length(by_rep, 2L)
  for (ids in by_rep) {
    expect_true(all(plan$psu$psu_id[plan$psu$certainty] %in% ids))
  }
})

test_that("a manual scalar stage 2 still runs under a bridge stage 1", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()

  s <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    draw(n = 10) |>
    execute(frame, seed = 7)

  n_psu <- sum(plan$detail$n_psu_certain + plan$detail$n_psu_draw)
  expect_equal(nrow(s), n_psu * 10L)
})

## Draw-time contract

test_that("the bridge requires an exact-pik fixed-size PPS method", {
  plan <- certainty_plan_fixture()
  for (m in c("pps_poisson", "srswor", "pps_pareto", "systematic")) {
    expect_error(
      sampling_design() |>
        add_stage() |>
        stratify_by(stratum) |>
        cluster_by(psu_id) |>
        draw(n = plan, method = m, mos = N),
      class = "samplyr_error_svyplan_certainty_plan"
    )
  }
})

test_that("arguments the plan already owns are refused alongside it", {
  plan <- certainty_plan_fixture()
  base <- function() {
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id)
  }
  expect_error(
    base() |> draw(n = plan, method = "pps_systematic", mos = N,
                   certainty_size = 500),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    base() |> draw(n = plan, method = "pps_systematic", mos = N,
                   certainty_prop = 0.2),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    base() |> draw(n = plan, method = "pps_systematic", mos = N, min_n = 2),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    base() |> draw(n = plan, method = "pps_systematic", mos = N, max_n = 10),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum, alloc = "equal") |>
      cluster_by(psu_id) |>
      draw(n = plan, method = "pps_systematic", mos = N),
    class = "samplyr_error_svyplan_certainty_plan"
  )
})

test_that("the bridge requires one stratum variable and one cluster variable", {
  plan <- certainty_plan_fixture()
  expect_error(
    sampling_design() |>
      add_stage() |>
      cluster_by(psu_id) |>
      draw(n = plan, method = "pps_systematic", mos = N),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum, person) |>
      cluster_by(psu_id) |>
      draw(n = plan, method = "pps_systematic", mos = N),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id, stratum) |>
      draw(n = plan, method = "pps_systematic", mos = N),
    class = "samplyr_error_svyplan_certainty_plan"
  )
})

test_that("a plan without psu_id or without n_take is refused", {
  register <- certainty_plan_register()
  register$psu_id <- NULL
  anonymous <- certainty_plan_fixture(psu = register)
  expect_error(
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id) |>
      draw(n = anonymous, method = "pps_systematic", mos = N),
    class = "samplyr_error_svyplan_certainty_plan"
  )

  old <- certainty_plan_fixture()
  old$psu$n_take <- NULL
  expect_error(
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id) |>
      draw(n = old, method = "pps_systematic", mos = N),
    class = "samplyr_error_svyplan_certainty_plan"
  )
})

## Disagreement between the plan and the executable rule

test_that("a plan whose remainder would cap a PSU refuses before any RNG", {
  bad <- certainty_disagreement_fixture()
  set.seed(101)
  state <- .Random.seed
  expect_error(
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id) |>
      draw(n = bad, method = "pps_systematic", mos = N),
    class = "samplyr_error_certainty_plan_disagreement"
  )
  expect_identical(state, .Random.seed)
})

test_that("the execute gate re-runs the disagreement check on the stored spec", {
  # A file skips draw(), so the gate cannot rely on the draw-time check.
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  d <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N)
  # Totals stay consistent, but 6 draws in B cap a PSU: 6 * 220 / 1150 > 1.
  d$stages[[1]]$draw_spec$certainty_plan$n_psu_draw[["B"]] <- 6
  d$stages[[1]]$draw_spec$n[["B"]] <- 7

  set.seed(101)
  state <- .Random.seed
  expect_error(
    execute(d, frame, seed = 1),
    class = "samplyr_error_certainty_plan_disagreement"
  )
  expect_identical(state, .Random.seed)
})

## Frame and register reconciliation at execute

test_that("a frame that disagrees with the register is refused before RNG", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  d <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N)

  refused <- function(f) {
    set.seed(101)
    state <- .Random.seed
    expect_error(
      execute(d, f, seed = 1),
      class = "samplyr_error_certainty_register_mismatch"
    )
    expect_identical(state, .Random.seed)
  }

  refused(frame[frame$psu_id != "A05", ])
  refused(rbind(
    frame,
    transform(frame[frame$psu_id == "A05", ][1:3, ], psu_id = "A99")
  ))
  refused(transform(frame, N = ifelse(psu_id == "B04", N + 1, N)))
  refused(transform(frame, N = N * 2))
  refused(transform(
    frame,
    stratum = ifelse(psu_id == "B04", "A", stratum)
  ))
})

## Serialization: design format 3

certainty_bridge_design <- function(plan = certainty_plan_fixture()) {
  sampling_design("bridge") |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    draw(n = plan)
}

certainty_tampered_file <- function(path, f) {
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  doc <- f(doc)
  out <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  jsonlite::write_json(
    doc, out,
    auto_unbox = TRUE, digits = NA, na = "null", null = "null"
  )
  out
}

test_that("bridge and plain designs both write format 3", {
  d <- certainty_bridge_design()
  path <- withr::local_tempfile(fileext = ".json")
  write_design(d, path)
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(doc$format_version, 3L)
  expect_identical(doc$design$stages[[1]]$draw$certainty_plan$role, "select")
  expect_identical(doc$design$stages[[2]]$draw$certainty_plan$role, "take")

  plain <- withr::local_tempfile(fileext = ".json")
  write_design(sampling_design() |> draw(n = 10), plain)
  expect_identical(
    jsonlite::fromJSON(plain, simplifyVector = FALSE)$format_version,
    3L
  )
})

test_that("a bridge design round-trips and executes identically", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  d <- certainty_bridge_design(plan)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(d, path, frame = frame)

  rd <- read_design(path)
  s0 <- execute(d, frame, seed = 7)
  s1 <- execute(rd, frame, seed = 7)
  expect_identical(nrow(s0), nrow(s1))
  a <- as.data.frame(s0)
  b <- as.data.frame(s1)
  expect_identical(names(a), names(b))
  for (cn in names(a)) {
    expect_identical(a[[cn]], b[[cn]])
  }
})

test_that("an executed bridge sample writes, reads, and replays", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  s <- execute(certainty_bridge_design(plan), frame, seed = 7)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(s, path, frame = frame)

  rd <- read_design(path)
  rp <- replay_design(rd, frame)
  a <- as.data.frame(s)
  b <- as.data.frame(rp)
  expect_identical(nrow(a), nrow(b))
  for (cn in names(a)) {
    expect_identical(a[[cn]], b[[cn]])
  }
})

test_that("malformed certainty blocks are refused by the reader", {
  d <- certainty_bridge_design()
  path <- withr::local_tempfile(fileext = ".json")
  write_design(d, path)

  drop_col <- certainty_tampered_file(path, function(doc) {
    doc$design$stages[[1]]$draw$certainty_plan$register <- lapply(
      doc$design$stages[[1]]$draw$certainty_plan$register,
      function(r) {
        r$n_take <- NULL
        r
      }
    )
    doc
  })
  expect_error(
    read_design(drop_col),
    class = "samplyr_error_design_file_malformed"
  )

  frac_take <- certainty_tampered_file(path, function(doc) {
    doc$design$stages[[1]]$draw$certainty_plan$register[[1]]$n_take <- 2.5
    doc
  })
  expect_error(
    read_design(frac_take),
    class = "samplyr_error_design_file_malformed"
  )

  string_cert <- certainty_tampered_file(path, function(doc) {
    doc$design$stages[[1]]$draw$certainty_plan$register <- lapply(
      doc$design$stages[[1]]$draw$certainty_plan$register,
      function(r) {
        r$certainty <- "yes"
        r
      }
    )
    doc
  })
  expect_error(
    read_design(string_cert),
    class = "samplyr_error_design_file_malformed"
  )

  bad_role <- certainty_tampered_file(path, function(doc) {
    doc$design$stages[[1]]$draw$certainty_plan$role <- "other"
    doc
  })
  expect_error(
    read_design(bad_role),
    class = "samplyr_error_design_file_malformed"
  )

  future <- certainty_tampered_file(path, function(doc) {
    doc$format_version <- 4
    doc
  })
  expect_error(
    read_design(future),
    class = "samplyr_error_design_file_unsupported"
  )
})

test_that("a shape-valid but inconsistent file is caught at the execute gate", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  path <- withr::local_tempfile(fileext = ".json")
  write_design(certainty_bridge_design(plan), path, frame = frame)

  # A changed register size reads fine and is refused against the frame.
  resized <- certainty_tampered_file(path, function(doc) {
    doc$design$stages[[1]]$draw$certainty_plan$register[[2]]$N <- 999
    doc$design$stages[[2]]$draw$certainty_plan$register[[2]]$N <- 999
    doc
  })
  expect_error(
    execute(read_design(resized), frame, seed = 1),
    class = "samplyr_error_certainty_register_mismatch"
  )

  # Only draw() builds the stage-size identity, so the gate re-derives it.
  desynced <- certainty_tampered_file(path, function(doc) {
    doc$design$stages[[1]]$draw$n$A <- 12
    doc
  })
  expect_error(
    execute(read_design(desynced), frame, seed = 1),
    class = "samplyr_error_svyplan_certainty_plan"
  )

  # A take stage carrying a different register than stage 1 selects under.
  forked <- certainty_tampered_file(path, function(doc) {
    doc$design$stages[[2]]$draw$certainty_plan$register[[3]]$N <- 777
    doc
  })
  expect_error(
    execute(read_design(forked), frame, seed = 1),
    class = "samplyr_error_svyplan_certainty_plan"
  )
})

## Stage 2: the plan's per-PSU takes

test_that("the take stage reproduces the operational design exactly", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()

  s <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    draw(n = plan) |>
    execute(frame, seed = 7)

  # Each PSU contributes exactly its take, so stratum totals equal n_int.
  expect_equal(nrow(s), sum(plan$detail$n_int))
  expect_equal(
    as.integer(table(s$stratum)[plan$detail$stratum]),
    as.integer(plan$detail$n_int)
  )
  takes <- stats::setNames(plan$psu$n_take, plan$psu$psu_id)
  per_psu <- table(s$psu_id)
  expect_equal(
    as.integer(per_psu),
    as.integer(takes[names(per_psu)])
  )

  w <- unique(s[, c("psu_id", "N", ".weight_1", ".weight_2", ".weight")])
  expect_equal(w$.weight_2, unname(w$N / takes[w$psu_id]))
  expect_equal(w$.weight, w$.weight_1 * w$.weight_2)
})

test_that("a stratified take stage splits each take with alloc, preserving it", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()

  s <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    stratify_by(sex, alloc = "proportional") |>
    draw(n = plan) |>
    execute(frame, seed = 7)

  # The allocation runs within each pool and must hand back the take whole.
  expect_equal(nrow(s), sum(plan$detail$n_int))
  takes <- stats::setNames(plan$psu$n_take, plan$psu$psu_id)
  per_psu <- table(s$psu_id)
  expect_equal(
    as.integer(per_psu),
    as.integer(takes[names(per_psu)])
  )
  # Sexes alternate, so splits are even up to one unit (B01's take is 19).
  cells <- table(s$psu_id, s$sex)
  expect_true(all(abs(cells[, "f"] - cells[, "m"]) <= 1))
  expect_gt(sum(cells[, "f"] != cells[, "m"]), 0)
})

test_that("a continuation applies the takes to the stage-1 result", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  d <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    draw(n = plan)

  s1 <- execute(d, frame, stages = 1, seed = 7)
  s2 <- execute(s1, frame, seed = 8)
  expect_equal(nrow(s2), sum(plan$detail$n_int))
  takes <- stats::setNames(plan$psu$n_take, plan$psu$psu_id)
  per_psu <- table(s2$psu_id)
  expect_equal(
    as.integer(per_psu),
    as.integer(takes[names(per_psu)])
  )
})

test_that("the two-stage bridge exports with certainty intact", {
  skip_if_not_installed("survey")
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  s <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    draw(n = plan) |>
    execute(frame, seed = 7)

  cert_rows <- s$.certainty_1
  expect_equal(unique(s$.weight_1[cert_rows]), 1)

  e <- as_svydesign(s, systematic_variance = "approximate")
  expect_s3_class(e, "survey.design")
  expect_equal(unname(stats::weights(e)), s$.weight)
})

test_that("take-stage contexts the bridge does not serve are refused", {
  plan <- certainty_plan_fixture()
  stage2 <- function() {
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id) |>
      draw(n = plan, method = "pps_systematic", mos = N) |>
      add_stage()
  }

  # Bare stratification would draw the take per cell, so alloc states the split.
  expect_error(
    stage2() |> stratify_by(sex) |> draw(n = plan),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    stage2() |> draw(n = plan, method = "pps_brewer", mos = N),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    stage2() |> draw(n = plan, frac = 0.1),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    stage2() |> draw(n = plan, min_n = 2),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_error(
    stage2() |> cluster_by(person) |> draw(n = plan),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  # A different fitted plan than stage 1's is a different register.
  expect_error(
    stage2() |> draw(n = certainty_plan_fixture(certainty_altered_register())),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  # Without a bridged stage 1 the takes have no selection to sit inside.
  expect_error(
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id) |>
      draw(n = 5, method = "pps_systematic", mos = N) |>
      add_stage() |>
      draw(n = plan),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  # The plan covers two stages.
  expect_error(
    stage2() |> draw(n = plan) |> add_stage() |> draw(n = plan),
    class = "samplyr_error_svyplan_certainty_plan"
  )
})

## Joint expectations decompose the forced set

test_that("joint expectations give certainty PSUs probability one", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  s <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    execute(frame, seed = 7)

  j <- joint_expectation(s)
  psu <- unique(s[, c("psu_id", ".certainty_1", ".weight_1")])
  d <- diag(j$stage_1)
  expect_equal(d[psu$.certainty_1], rep(1, sum(psu$.certainty_1)))
  expect_equal(d[!psu$.certainty_1], 1 / psu$.weight_1[!psu$.certainty_1])
})

test_that("the disagreement gate uses the tolerance selection uses", {
  # The sizes sum exactly on every platform, so 3 / sum falls 1e-15 short of one.
  register <- data.frame(
    psu_id = c("a", "b", "c", "d"), stratum = "A",
    N = c(0.75, 1, 0.625, 0.625 + 14 * 2^-52), certainty = FALSE
  )
  pik <- 3 * register$N / sum(register$N)
  expect_lt(pik[2], 1)
  expect_true(is_certainty_probability(pik[2]))
  err <- tryCatch(
    check_certainty_plan_disagreement(register, c(A = 3), "pps_systematic"),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_certainty_plan_disagreement")
  expect_match(conditionMessage(err), "\"b\"", fixed = TRUE)
})

test_that("an unzoned plan selects the same sample as before zones existed", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  pins <- c(
    pps_brewer = "c671d4147356ac0ca0829445a072b736",
    pps_systematic = "2e2821816a3155e82c8ff3f753ab507b"
  )
  for (method in names(pins)) {
    s <- sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id) |>
      draw(n = plan, method = method, mos = N) |>
      add_stage() |>
      draw(n = plan) |>
      execute(frame, seed = 7)
    expect_identical(
      rlang::hash(sort(paste(s$psu_id, s$person))),
      pins[[method]],
      label = method
    )
    expect_false(any(grepl("^\\.zone", names(s))))
  }
})

## Zoned plans. svyplan's n_psu_per_zone = 2 cuts each stratum's remainder
## into zones, and stage 1 draws two PSUs from every zone.

test_that("a zoned plan draws two PSUs from every zone at the planned chances", {
  plan <- certainty_zone_fixture()
  frame <- certainty_element_frame(certainty_zone_register())
  pik <- certainty_zone_pik(plan)
  zoned <- !plan$psu$certainty
  for (method in certainty_bridge_methods) {
    s <- execute(certainty_zone_design(plan, method), frame, seed = 5)
    psu <- unique(as.data.frame(s)[
      , c("psu_id", "stratum", ".weight_1", ".certainty_1", ".zone_1")
    ])
    at <- match(psu$psu_id, plan$psu$psu_id)

    expect_setequal(
      psu$psu_id[psu$.certainty_1],
      plan$psu$psu_id[plan$psu$certainty]
    )
    expect_identical(psu$.zone_1, plan$psu$.zone[at], label = method)
    drawn <- psu[!psu$.certainty_1, ]
    expect_identical(
      as.vector(table(paste(drawn$stratum, drawn$.zone_1))),
      rep(2L, 6),
      label = method
    )
    expect_setequal(
      unique(paste(drawn$stratum, drawn$.zone_1)),
      unique(paste(plan$psu$stratum, plan$psu$.zone)[zoned])
    )
    expect_equal(1 / psu$.weight_1, pik[at], tolerance = 1e-12)
    expect_identical(
      as.vector(table(s$stratum)[plan$detail$stratum]),
      as.integer(plan$detail$n_int)
    )
  }
})

test_that("zones are drawn in a fixed order, so a seed fixes the sample", {
  plan <- certainty_zone_fixture()
  frame <- certainty_element_frame(certainty_zone_register())
  pins <- c(
    pps_brewer = "7750c50a69339767fc76191a7cba5500",
    pps_systematic = "3731210b3b7f9c40f9c06239279b8b46"
  )
  for (method in names(pins)) {
    s <- execute(certainty_zone_design(plan, method), frame, seed = 7)
    expect_identical(
      rlang::hash(sort(paste(s$psu_id, s$person))),
      pins[[method]],
      label = method
    )
  }
})

test_that("unzoned and two-per-zone samples keep their rows and weights", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  zplan <- certainty_zone_fixture()
  zframe <- certainty_element_frame(certainty_zone_register())
  unzoned <- c(
    pps_systematic = "5a3481bbade6fab364b674340e354fb6",
    pps_brewer = "58701f1b2729524a2a007af660908908",
    pps_cps = "289408485df844fd4b831e4e85d1215c",
    pps_sampford = "0eefbf6de1736610e33cae9056cb24f9"
  )
  zoned <- c(
    pps_systematic = "5205df5217e8f38ae106b23c848cf81f",
    pps_brewer = "186e41de1c9a305755daa56deeeae0f7",
    pps_cps = "926bdc79f36815d1963d6f7ee4aa10bc",
    pps_sampford = "366111637ef02b4715c76945b61a8bfa"
  )
  for (method in certainty_bridge_methods) {
    s <- sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id) |>
      draw(n = plan, method = method, mos = N) |>
      add_stage() |>
      draw(n = plan) |>
      execute(frame, seed = 11)
    expect_identical(certainty_sample_key(s), unzoned[[method]], label = method)
    s <- execute(certainty_zone_design(zplan, method), zframe, seed = 11)
    expect_identical(certainty_sample_key(s), zoned[[method]], label = method)
  }
})

test_that("a zoned sample does not depend on the frame digest mode", {
  plan <- certainty_zone_fixture()
  frame <- certainty_element_frame(certainty_zone_register())
  d <- certainty_zone_design(plan)
  key <- function(s) sort(paste(s$psu_id, s$person, s$.zone_1))
  full <- execute(d, frame, seed = 9)
  none <- execute(d, frame, seed = 9, frame_digest = "none")
  expect_identical(key(none), key(full))
})

test_that("the digest gives a zoned pool the chances selection uses", {
  plan <- certainty_zone_fixture()
  register <- certainty_zone_register()
  frame <- certainty_element_frame(register)
  d <- certainty_zone_design(plan)
  pik <- certainty_zone_pik(plan)

  # Stage-1 units come in register order, which n_descendants confirms.
  ex <- exante_digest(d, frame)$stages[[1]]$units
  expect_identical(as.numeric(ex$n_descendants), register$N)
  expect_equal(ex$chance, pik, tolerance = 1e-12)

  ed <- get_frame_digest(execute(d, frame, seed = 2))$stages[[1]]$units
  expect_identical(as.numeric(ed$n_descendants), register$N)
  expect_equal(ed$chance, pik, tolerance = 1e-12)
})

test_that("a zoned sample exports one variance stratum per zone", {
  skip_if_not_installed("survey")
  plan <- certainty_zone_fixture()
  frame <- certainty_element_frame(certainty_zone_register())
  frame$y <- (frame$person %% 7) + (frame$N %% 5)
  s <- execute(certainty_zone_design(plan), frame, seed = 4)

  spec <- export_stage_spec(as.data.frame(s), get_design(s), get_stages_executed(s))
  strata <- spec$stage[["1"]]$strata
  expect_identical(strata$user, c("stratum", ".zone_1"))
  units <- unique(data.frame(id = strata$id, psu = s$psu_id, cert = s$.certainty_1))
  per_stratum <- table(units$id[!units$cert])
  expect_identical(as.vector(per_stratum), rep(2L, 6))
  expect_true(all(table(units$id[units$cert]) == 1L))
  expect_length(intersect(units$id[units$cert], units$id[!units$cert]), 0L)

  expect_no_warning(e <- as_svydesign(s))
  expect_identical(length(unique(e$strata[, 1])), 8L)
  expect_no_warning(survey::svymean(~y, e))
})

test_that("a zoned stage's joint expectations need the frame", {
  plan <- certainty_zone_fixture()
  frame <- certainty_element_frame(certainty_zone_register())
  s <- execute(certainty_zone_design(plan), frame, seed = 4)
  # The digest identifies the selected PSUs only, not their zone mates.
  expect_error(
    joint_expectation(s, stages = 1),
    class = "samplyr_error_digest_unavailable"
  )
  expect_no_error(joint_expectation(s, frame = frame, stages = 1))
})

## The exact joint inclusion probabilities of systematic PPS: a unit is
## taken when one of the points u, u + 1, ... falls in its stretch of the
## cumulated chances, so pi_ij is the measure of the starts u that take both.
systematic_jip <- function(pik) {
  ends <- cumsum(pik)
  starts <- ends - pik
  cuts <- sort(unique(c(0, 1, (c(starts, ends) %% 1))))
  out <- matrix(0, length(pik), length(pik))
  for (k in seq_len(length(cuts) - 1L)) {
    u <- (cuts[k] + cuts[k + 1L]) / 2 + 0:(ceiling(sum(pik)) - 1L)
    taken <- vapply(seq_along(pik), function(i) {
      any(u > starts[i] & u <= ends[i])
    }, logical(1))
    out[taken, taken] <- out[taken, taken] + (cuts[k + 1L] - cuts[k])
  }
  out
}

test_that("a two-per-zone systematic stage has the exact joint probabilities", {
  plan <- certainty_zone_fixture()
  frame <- certainty_element_frame(certainty_zone_register())
  s <- execute(certainty_zone_design(plan, "pps_systematic"), frame, seed = 2)
  jip <- joint_expectation(s, frame = frame, stages = 1)$stage_1

  ids <- unique(as.data.frame(s)$psu_id)
  reg <- plan$psu
  pik <- certainty_zone_pik(plan)
  at <- match(ids, reg$psu_id)
  expected <- outer(pik[at], pik[at])
  diag(expected) <- pik[at]
  # Each zone in register order, which is the frame's order.
  for (key in unique(paste(reg$stratum, reg$.zone)[!reg$certainty])) {
    members <- which(paste(reg$stratum, reg$.zone) == key)
    zone_jip <- systematic_jip(pik[members])
    pos <- which(at %in% members)
    expected[pos, pos] <- zone_jip[match(at[pos], members), match(at[pos], members)]
  }
  expect_equal(diag(jip), pik[at], tolerance = 1e-12)
  expect_equal(jip, expected, tolerance = 1e-12)
})

test_that("a zoned pool's joint matrix keeps the fixed-size identity", {
  plan <- certainty_zone_fixture()
  a <- plan$psu$stratum == "A"
  pik <- certainty_zone_pik(plan)[a]
  zone <- plan$psu$.zone[a]
  n <- sum(pik)
  # A consistency check only: any fixed-size design satisfies it. Brewer's
  # joint probabilities are an approximation that does not.
  for (method in c("pps_systematic", "pps_sampford")) {
    jip <- assemble_jip_zoned(pik, zone, method, seq_along(pik))
    expect_equal(rowSums(jip) - diag(jip), (n - 1) * pik, tolerance = 1e-9,
                 label = method)
  }
})

test_that("zoned joint probabilities agree with repeated zoned draws", {
  plan <- certainty_zone_fixture()
  rest <- plan$psu$stratum == "A" & !plan$psu$certainty
  mos <- plan$psu$N[rest]
  zone <- plan$psu$.zone[rest]
  pik <- 2 * mos / ave(mos, zone, FUN = sum)
  data <- data.frame(id = seq_along(mos))
  R <- 6000L
  for (method in c("pps_brewer", "pps_sampford")) {
    jip <- assemble_jip_zoned(pik, zone, method, seq_along(pik))
    hits <- matrix(0, length(pik), length(pik))
    withr::with_seed(17, for (r in seq_len(R)) {
      taken <- draw_pps_zones(
        data, 2L * max(zone), method, mos, list(method = method), zone, 2L
      )$selected
      hits[taken, taken] <- hits[taken, taken] + 1
    })
    freq <- hits / R
    se <- sqrt(jip * (1 - jip) / R)
    expect_true(all(abs(freq - jip) <= 4.5 * se + 1e-12), label = method)
  }
})

test_that("a plan whose zones break the design is refused at draw()", {
  plan <- certainty_zone_fixture()
  refused <- function(p) {
    expect_error(
      sampling_design() |>
        add_stage() |>
        stratify_by(stratum) |>
        cluster_by(psu_id) |>
        draw(n = p, method = "pps_brewer", mos = N),
      class = "samplyr_error_svyplan_certainty_plan"
    )
  }
  a <- plan$psu$stratum == "A"

  # Zones without the per-zone count, and the count without zones.
  p <- plan
  p$params$n_psu_per_zone <- NULL
  refused(p)
  p <- plan
  p$psu$.zone <- NULL
  refused(p)
  # A count other than two.
  p <- plan
  p$params$n_psu_per_zone <- 3
  refused(p)
  # A certainty PSU with a zone.
  p <- plan
  p$psu$.zone[p$psu$psu_id == "A01"] <- 1L
  refused(p)
  # A zone missing from the numbering, and a remainder PSU with no zone.
  p <- plan
  p$psu$.zone[a & p$psu$.zone %in% 4] <- 5L
  refused(p)
  p <- plan
  p$psu$.zone[p$psu$psu_id == "A24"] <- NA
  refused(p)
  # A zone of two PSUs is a census, not a draw of two.
  p <- plan
  moved <- which(a & p$psu$.zone %in% 1)[3:4]
  p$psu$.zone[moved] <- 2L
  refused(p)
  # The detail's zone count disagrees with the draw.
  p <- plan
  p$detail$n_zone[p$detail$stratum == "A"] <- 3
  refused(p)
})

test_that("the zone checks run again at the execute gate, before RNG", {
  plan <- certainty_zone_fixture()
  frame <- certainty_element_frame(certainty_zone_register())
  d <- certainty_zone_design(plan)
  gate <- function(d, class) {
    set.seed(101)
    state <- .Random.seed
    expect_error(execute(d, frame, seed = 1), class = class)
    expect_identical(state, .Random.seed)
  }

  # Both stages carry the register, and the take stage checks it is the
  # select stage's, so each edit goes to both to reach the zone checks.
  tamper <- function(d, edit) {
    for (k in 1:2) {
      d$stages[[k]]$draw_spec$certainty_plan <-
        edit(d$stages[[k]]$draw_spec$certainty_plan)
    }
    d
  }
  gate(
    tamper(d, function(spec) {
      reg <- spec$register
      reg$zone[reg$stratum == "A" & reg$zone %in% 4] <- 5L
      spec$register <- reg
      spec
    }),
    "samplyr_error_svyplan_certainty_plan"
  )
  gate(
    tamper(d, function(spec) {
      spec$n_psu_per_zone <- NULL
      spec
    }),
    "samplyr_error_svyplan_certainty_plan"
  )
})

test_that("the disagreement check applies the within-zone rule", {
  # Stratum-wide, 4 * 2 / 13 is below one. Within its zone, 2 * 2 / 4 is one.
  register <- data.frame(
    psu_id = c("a", "b", "c", "d", "e", "f"), stratum = "A",
    N = c(2, 1, 1, 3, 3, 3), certainty = FALSE,
    zone = c(1L, 1L, 1L, 2L, 2L, 2L)
  )
  expect_null(check_certainty_plan_disagreement(register, c(A = 4), "pps_brewer"))
  err <- tryCatch(
    check_certainty_plan_disagreement(
      register, c(A = 4), "pps_brewer", zone_m = 2L
    ),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_certainty_plan_disagreement")
  expect_match(conditionMessage(err), "\"a\"", fixed = TRUE)
  expect_match(conditionMessage(err), "2 PSUs per zone", fixed = TRUE)
})

test_that("a zoned design writes, reads, executes and replays identically", {
  plan <- certainty_zone_fixture()
  frame <- certainty_element_frame(certainty_zone_register())
  d <- certainty_zone_design(plan)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(d, path, frame = frame)

  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(doc$format_version, 3L)
  block <- doc$design$stages[[1]]$draw$certainty_plan
  expect_identical(block$n_psu_per_zone, 2L)
  zones <- vapply(
    block$register,
    function(r) if (is.null(r$zone)) NA_integer_ else as.integer(r$zone),
    integer(1)
  )
  expect_identical(zones, as.integer(plan$psu$.zone))

  rd <- read_design(path)
  expect_identical(
    rd$stages[[1]]$draw_spec$certainty_plan$register$zone,
    as.integer(plan$psu$.zone)
  )
  s0 <- execute(d, frame, seed = 7)
  s1 <- execute(rd, frame, seed = 7)
  a <- as.data.frame(s0)
  b <- as.data.frame(s1)
  expect_identical(names(a), names(b))
  for (cn in names(a)) {
    expect_identical(a[[cn]], b[[cn]], label = cn)
  }

  spath <- withr::local_tempfile(fileext = ".json")
  write_design(s0, spath, frame = frame)
  rp <- replay_design(read_design(spath), frame)
  b <- as.data.frame(rp)
  for (cn in names(a)) {
    expect_identical(a[[cn]], b[[cn]], label = cn)
  }
})

test_that("the reader refuses zones and their count recorded apart", {
  d <- certainty_zone_design()
  path <- withr::local_tempfile(fileext = ".json")
  write_design(d, path)
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  rewrite <- function(edit) {
    bad <- doc
    bad$design$stages[[1]]$draw$certainty_plan <-
      edit(bad$design$stages[[1]]$draw$certainty_plan)
    out <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame(2))
    jsonlite::write_json(bad, out, auto_unbox = TRUE, null = "null",
                         na = "null", digits = NA)
    out
  }
  no_count <- rewrite(function(b) {
    b$n_psu_per_zone <- NULL
    b
  })
  expect_error(read_design(no_count), class = "samplyr_error_design_file_malformed")
  no_zones <- rewrite(function(b) {
    b$register <- lapply(b$register, function(r) {
      r$zone <- NULL
      r
    })
    b
  })
  expect_error(read_design(no_zones), class = "samplyr_error_design_file_malformed")
  bad_zone <- rewrite(function(b) {
    b$register[[2]]$zone <- 0
    b
  })
  expect_error(read_design(bad_zone), class = "samplyr_error_design_file_malformed")
})

## One PSU per zone. svyplan's n_psu_per_zone = 1 draws one PSU from every
## zone and fixes, before selection, the groups of zones the variance is
## collapsed in. The pair fixture's groups are A zones (1, 2), A zones
## (3, 4, 5), B zones (1, 2), and B zone 3 with C's only zone.

pair_design_frame <- function() {
  certainty_element_frame(certainty_pair_register())
}

test_that("a one-per-zone plan draws one PSU from every zone at the planned chances", {
  plan <- certainty_pair_fixture()
  frame <- pair_design_frame()
  pik <- certainty_zone_pik(plan)
  cells <- unique(paste(plan$psu$stratum, plan$psu$.zone)[!plan$psu$certainty])
  for (method in certainty_bridge_methods) {
    expect_no_warning(
      s <- execute(certainty_zone_design(plan, method), frame, seed = 5)
    )
    psu <- unique(as.data.frame(s)[
      , c("psu_id", "stratum", ".weight_1", ".certainty_1", ".zone_1", ".pair_1")
    ])
    at <- match(psu$psu_id, plan$psu$psu_id)

    expect_setequal(
      psu$psu_id[psu$.certainty_1],
      plan$psu$psu_id[plan$psu$certainty]
    )
    expect_identical(psu$.zone_1, plan$psu$.zone[at], label = method)
    expect_identical(psu$.pair_1, plan$psu$.pair[at], label = method)
    drawn <- psu[!psu$.certainty_1, ]
    expect_setequal(paste(drawn$stratum, drawn$.zone_1), cells)
    expect_false(anyDuplicated(paste(drawn$stratum, drawn$.zone_1)) > 0)
    expect_equal(1 / psu$.weight_1, pik[at], tolerance = 1e-12)
    expect_identical(
      as.vector(table(s$stratum)[plan$detail$stratum]),
      as.integer(plan$detail$n_int)
    )
  }
})

test_that("a one-per-zone sample is fixed by its seed, whatever the digest mode", {
  plan <- certainty_pair_fixture()
  frame <- pair_design_frame()
  # At one draw, sondage's systematic, Brewer and Sampford draws read the
  # same uniform the same way, so they select the same PSUs.
  pins <- c(
    pps_systematic = "5de029ad09f19da53d5be4d4f073e7ae",
    pps_brewer = "5de029ad09f19da53d5be4d4f073e7ae",
    pps_cps = "103bd92ad8838565d63b2d5cfd024a1c",
    pps_sampford = "5de029ad09f19da53d5be4d4f073e7ae"
  )
  for (method in certainty_bridge_methods) {
    d <- certainty_zone_design(plan, method)
    full <- execute(d, frame, seed = 11)
    none <- execute(d, frame, seed = 11, frame_digest = "none")
    expect_identical(certainty_sample_key(full), pins[[method]], label = method)
    expect_identical(certainty_sample_key(none), pins[[method]], label = method)
  }
})

test_that("the digest gives a one-per-zone pool the chances selection uses", {
  plan <- certainty_pair_fixture()
  register <- certainty_pair_register()
  frame <- certainty_element_frame(register)
  d <- certainty_zone_design(plan)
  pik <- certainty_zone_pik(plan)

  ex <- exante_digest(d, frame)$stages[[1]]$units
  expect_identical(as.numeric(ex$n_descendants), register$N)
  expect_equal(ex$chance, pik, tolerance = 1e-12)

  ed <- get_frame_digest(execute(d, frame, seed = 2))$stages[[1]]$units
  expect_identical(as.numeric(ed$n_descendants), register$N)
  expect_equal(ed$chance, pik, tolerance = 1e-12)
})

test_that("a one-per-zone plan counts single selections per variance group", {
  plan <- certainty_pair_fixture()
  frame <- pair_design_frame()
  s <- expect_no_message(
    execute(certainty_zone_design(plan), frame, seed = 5),
    class = "samplyr_message_singleton_pool"
  )
  # Stratum C draws one PSU, which a count per stratum would report.
  expect_length(unique(s$psu_id[s$stratum == "C"]), 1L)

  single <- certainty_single_zone_fixture()
  sframe <- certainty_element_frame(single$psu[, c("psu_id", "stratum", "N")])
  expect_message(
    execute(certainty_zone_design(single), sframe, seed = 5),
    class = "samplyr_message_singleton_pool"
  )
})

test_that("a plan whose variance groups break the design is refused at draw()", {
  plan <- certainty_pair_fixture()
  refused <- function(p, pattern) {
    expect_error(
      sampling_design() |>
        add_stage() |>
        stratify_by(stratum) |>
        cluster_by(psu_id) |>
        draw(n = p, method = "pps_brewer", mos = N),
      pattern,
      fixed = TRUE,
      class = "samplyr_error_svyplan_certainty_plan"
    )
  }
  in_zone <- function(p, h, z) p$psu$stratum == h & p$psu$.zone %in% z

  p <- plan
  p$psu$.pair <- NULL
  refused(p, "records no variance groups")

  p <- certainty_zone_fixture()
  p$psu$.pair <- ifelse(is.na(p$psu$.zone), NA_integer_, 1L)
  refused(p, "only a plan drawing one PSU per zone")

  p <- plan
  p$psu$.pair[p$psu$psu_id == "A01"] <- 1L
  refused(p, "a group without a zone")

  p <- plan
  p$psu$.pair[which(in_zone(p, "A", 1))[1]] <- 2L
  refused(p, "is split across variance groups")

  p <- plan
  p$psu$.pair[p$psu$.pair %in% 4] <- 5L
  refused(p, "numbered from 1 without gaps")

  # Zone A2 moves to the triple: group 1 is left with one zone.
  p <- plan
  p$psu$.pair[in_zone(p, "A", 2)] <- 2L
  refused(p, "group 1 holds 1 zone")

  # Groups 3 and 4 merged: B's three zones and C's one.
  p <- plan
  p$psu$.pair[p$psu$.pair %in% 4] <- 3L
  refused(p, "group 3 holds 4 zones")

  p <- plan
  p$psu$.pair[in_zone(p, "C", 1)] <- 4.5
  refused(p, "not positive whole numbers")
})

test_that("the variance-group checks run again at the execute gate, before RNG", {
  plan <- certainty_pair_fixture()
  frame <- pair_design_frame()
  d <- certainty_zone_design(plan)
  gate <- function(d) {
    set.seed(101)
    state <- .Random.seed
    expect_error(
      execute(d, frame, seed = 1),
      "ariance group",
      class = "samplyr_error_svyplan_certainty_plan"
    )
    expect_identical(state, .Random.seed)
  }
  # The take stage checks its register is the select stage's, so each edit
  # goes to both stages to reach the group checks.
  tamper <- function(d, edit) {
    for (k in 1:2) {
      spec <- d$stages[[k]]$draw_spec$certainty_plan
      spec$register <- edit(spec$register)
      d$stages[[k]]$draw_spec$certainty_plan <- spec
    }
    d
  }
  gate(tamper(d, function(reg) {
    reg$pair[reg$pair %in% 4] <- 3L
    reg
  }))
  gate(tamper(d, function(reg) {
    reg$pair <- NULL
    reg
  }))
})

test_that("the variance-group columns are reserved in a frame", {
  for (col in c(".pair", ".pair_1")) {
    frame <- data.frame(id = seq_len(20))
    frame[[col]] <- 1L
    expect_error(
      sampling_design() |>
        draw(n = 5) |>
        execute(frame, seed = 1),
      class = "samplyr_error_frame_reserved_names",
      label = col
    )
  }
  expect_identical(
    samplyr_reserved_names(c(".pair", ".pair_2", ".pairs", ".pair_x")),
    c(".pair", ".pair_2")
  )
})

pair_sample <- function(method = "pps_brewer", seed = 4) {
  frame <- pair_design_frame()
  frame$y <- (frame$person %% 7) + (frame$N %% 5)
  execute(certainty_zone_design(certainty_pair_fixture(), method), frame, seed = seed)
}

test_that("a one-per-zone sample exports its variance groups as strata", {
  skip_if_not_installed("survey")
  s <- pair_sample()
  spec <- export_stage_spec(as.data.frame(s), get_design(s), get_stages_executed(s))
  strata <- spec$stage[["1"]]$strata
  expect_identical(strata$user, ".pair_1")
  units <- unique(data.frame(
    id = strata$id, psu = s$psu_id, cert = s$.certainty_1, pair = s$.pair_1
  ))
  drawn <- units[!units$cert, ]
  # One stratum per group, holding its two or three zones' PSUs.
  expect_identical(
    as.vector(table(drawn$pair)[as.character(1:4)]),
    c(2L, 3L, 2L, 2L)
  )
  expect_identical(nrow(unique(drawn[c("id", "pair")])), 4L)
  expect_length(unique(drawn$id), 4L)
  expect_length(intersect(units$id[units$cert], drawn$id), 0L)
  # The group of B's third zone and C's only zone crosses strata.
  expect_setequal(unique(s$stratum[s$.pair_1 %in% 4]), c("B", "C"))

  expect_no_warning(e <- as_svydesign(s))
  expect_identical(length(unique(e$strata[, 1])), 5L)
})

test_that("a one-per-zone stage is exported with the with-replacement variance", {
  skip_if_not_installed("survey")
  s <- pair_sample()
  d <- as.data.frame(s)
  e <- as_svydesign(s)
  expect_identical(e$variables$.fpc_pi_1, ifelse(d$.certainty_1, 1, 0))

  # Remainder: the with-replacement variance of the estimated PSU totals
  # within each group. Certainty PSUs: their own stage-2 variance.
  rem <- d[!d$.certainty_1, ]
  t_psu <- tapply(rem$y * rem$.weight, rem$psu_id, sum)
  grp <- tapply(rem$.pair_1, rem$psu_id, unique)
  v_rem <- sum(vapply(split(t_psu, grp), function(t) {
    length(t) / (length(t) - 1) * sum((t - mean(t))^2)
  }, numeric(1)))
  cer <- d[d$.certainty_1, ]
  v_cer <- sum(vapply(split(cer, cer$psu_id), function(p) {
    n <- nrow(p)
    N <- p$.weight_2[1] * n
    N^2 * (1 - n / N) * stats::var(p$y) / n
  }, numeric(1)))
  expect_equal(
    stats::vcov(survey::svytotal(~y, e))[1],
    v_rem + v_cer,
    tolerance = 1e-10
  )
})

test_that("replicate routes treat a one-per-zone remainder as drawn with replacement", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  s <- pair_sample()
  v_lin <- stats::vcov(survey::svytotal(~y, as_svydesign(s)))[1]
  # Resampling the first stage loses no correction it has.
  expect_no_warning(as_svrepdesign(s, type = "JKn"))
  v <- variance_estimators(s)
  expect_identical(
    v$status[v$estimator == "Stratified jackknife (JKn)"],
    "supported"
  )
  # With 2000 replicates a bootstrap variance is within a few percent of its
  # expectation. A correction kept on the remainder lowers it by a fifth.
  for (type in c("mrbbootstrap", "rwyb")) {
    set.seed(3)
    r <- suppressWarnings(as_svrepdesign(s, type = type, replicates = 2000))
    ratio <- stats::vcov(survey::svytotal(~y, r))[1] / v_lin
    expect_gt(ratio, 0.9, label = type)
    expect_lt(ratio, 1.1, label = type)
  }
})

test_that("variance_estimators() refuses half-samples when a group is a triple", {
  skip_if_not_installed("survey")
  s <- pair_sample()
  v <- variance_estimators(s)
  for (est in c("Balanced repeated replication", "Fay's balanced repeated replication")) {
    row <- v[v$estimator == est, ]
    expect_identical(row$status, "refused", label = est)
    expect_identical(row$class, "samplyr_error_svrep_conversion_failed", label = est)
  }
  expect_error(
    as_svrepdesign(s, type = "BRR"),
    class = "samplyr_error_svrep_conversion_failed"
  )
  # Every group of the zone fixture is a pair, so the groups do not refuse.
  plan <- certainty_zone_fixture(m = 1)
  frame <- certainty_element_frame(certainty_zone_register())
  v <- variance_estimators(execute(certainty_zone_design(plan), frame, seed = 4))
  expect_false(identical(v$status[v$estimator == "Balanced repeated replication"], "refused"))
})

test_that("a one-per-zone systematic stage is not reported as systematic", {
  skip_if_not_installed("survey")
  s <- pair_sample("pps_systematic")
  expect_no_warning(as_svydesign(s))
  v <- variance_estimators(s)
  expect_false(any(v$class %in% "samplyr_warning_systematic_variance"))
  taylor <- v$note[v$estimator == "Taylor linearization"]
  expect_match(taylor, "collapsed in variance groups", fixed = TRUE)
  expect_no_match(taylor, "stage 2", fixed = TRUE)
})

test_that("a one-per-zone stage refuses joint expectations, with the reason", {
  s <- pair_sample()
  expect_error(
    joint_expectation(s, frame = pair_design_frame(), stages = 1),
    "Sen-Yates-Grundy",
    class = "samplyr_error_joint_method_unsupported"
  )
})

test_that("a one-per-zone design writes, reads, executes and replays identically", {
  skip_if_not_installed("survey")
  plan <- certainty_pair_fixture()
  frame <- pair_design_frame()
  frame$y <- (frame$person %% 7) + (frame$N %% 5)
  d <- certainty_zone_design(plan)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(d, path, frame = frame)

  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  block <- doc$design$stages[[1]]$draw$certainty_plan
  expect_identical(block$n_psu_per_zone, 1L)
  pairs <- vapply(
    block$register,
    function(r) if (is.null(r$pair)) NA_integer_ else as.integer(r$pair),
    integer(1)
  )
  expect_identical(pairs, as.integer(plan$psu$.pair))

  rd <- read_design(path)
  spec <- rd$stages[[1]]$draw_spec$certainty_plan
  expect_identical(spec$n_psu_per_zone, 1L)
  expect_identical(spec$register$pair, as.integer(plan$psu$.pair))

  s0 <- execute(d, frame, seed = 7)
  s1 <- execute(rd, frame, seed = 7)
  a <- as.data.frame(s0)
  b <- as.data.frame(s1)
  expect_identical(names(a), names(b))
  for (cn in names(a)) {
    expect_identical(a[[cn]], b[[cn]], label = cn)
  }
  # The read design still exports the groups, not only the column.
  e0 <- as_svydesign(s0)
  e1 <- as_svydesign(s1)
  expect_identical(e1$strata, e0$strata)
  expect_equal(
    stats::vcov(survey::svytotal(~y, e1)),
    stats::vcov(survey::svytotal(~y, e0))
  )

  spath <- withr::local_tempfile(fileext = ".json")
  write_design(s0, spath, frame = frame)
  rp <- replay_design(read_design(spath), frame)
  b <- as.data.frame(rp)
  for (cn in names(a)) {
    expect_identical(a[[cn]], b[[cn]], label = cn)
  }
})

test_that("the reader refuses variance groups recorded apart from one PSU per zone", {
  path <- withr::local_tempfile(fileext = ".json")
  write_design(certainty_zone_design(certainty_pair_fixture()), path)
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  refused <- function(edit) {
    bad <- doc
    bad$design$stages[[1]]$draw$certainty_plan <-
      edit(bad$design$stages[[1]]$draw$certainty_plan)
    out <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(bad, out, auto_unbox = TRUE, null = "null",
                         na = "null", digits = NA)
    expect_error(read_design(out), class = "samplyr_error_design_file_malformed")
  }
  drop_pairs <- function(b) {
    b$register <- lapply(b$register, function(r) {
      r$pair <- NULL
      r
    })
    b
  }
  # Groups with two PSUs per zone, and one PSU per zone without groups.
  refused(function(b) {
    b$n_psu_per_zone <- 2L
    b
  })
  refused(drop_pairs)
  refused(function(b) {
    b$n_psu_per_zone <- 3L
    b
  })
  refused(function(b) {
    b$register[[2]]$pair <- 0
    b
  })
})

## PSUs smaller than the take. svyplan refuses them in a register, and
## svyplan::merge_psus() merges them before the register is built. The
## bridge must then field the merged frame, and only that one.

test_that("a frame merged by svyplan::merge_psus() fields its plan", {
  # Ten PSUs per stratum, three of them smaller than the take of 8.
  sizes <- c(60, 5, 4, 70, 55, 3, 65, 80, 50, 45)
  register <- data.frame(
    psu_id = c(sprintf("A%02d", 1:10), sprintf("B%02d", 1:10)),
    stratum = rep(c("A", "B"), each = 10),
    N = c(sizes, rev(sizes))
  )
  frame <- certainty_element_frame(register)
  plan_for <- function(psu) {
    svyplan::n_alloc(
      transform(stats::aggregate(N ~ stratum, psu, sum), n_per_psu = 8),
      measures = data.frame(stratum = c("A", "B"), name = "y", p = 0.5,
                            icc_psu = 0.05),
      targets = data.frame(name = "y", cv = 0.15),
      psu = psu
    )
  }
  expect_error(plan_for(register), "fewer units than the take")

  merged <- frame |>
    dplyr::group_by(stratum) |>
    dplyr::mutate(psu_id = svyplan::merge_psus(psu_id, min_size = 8)) |>
    dplyr::ungroup() |>
    as.data.frame()
  merged_register <- dplyr::count(merged, psu_id, stratum, name = "N")
  # A02 + A03 and B08 + B09 merge in pairs, A06 and B05 join the PSU before.
  expect_setequal(
    merged_register$psu_id,
    setdiff(register$psu_id, c("A03", "A06", "B05", "B09"))
  )
  expect_true(all(merged_register$N >= 8))

  plan <- plan_for(merged_register)
  merged$N <- merged_register$N[match(merged$psu_id, merged_register$psu_id)]
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N) |>
    add_stage() |>
    draw(n = plan)
  s <- execute(design, merged, seed = 1)
  expect_identical(
    as.vector(table(s$stratum)[plan$detail$stratum]),
    as.integer(plan$detail$n_int)
  )
  expect_true(all(s$psu_id %in% merged_register$psu_id))

  # The unmerged frame is not the population the plan was solved for, and
  # is refused before any random number is drawn.
  set.seed(101)
  state <- .Random.seed
  expect_error(
    execute(design, frame, seed = 1),
    class = "samplyr_error_certainty_register_mismatch"
  )
  expect_identical(state, .Random.seed)
})
