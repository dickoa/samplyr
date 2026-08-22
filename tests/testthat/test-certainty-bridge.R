## Certainty-plan bridge, stage 1. draw(n = plan) at a clustered, stratified
## stage 1 fields the plan's stored classification exactly: every certainty
## PSU enters with probability one, the remainder is drawn at exactly
## n_psu_draw by an exact-pik PPS method, and a frame or plan the selection
## cannot honor is refused before any RNG is consumed.

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

  # Exactly the plan's PSU counts, stratum by stratum.
  got <- table(psu$stratum)
  want <- plan$detail$n_psu_certain + plan$detail$n_psu_draw
  expect_equal(as.integer(got[plan$detail$stratum]), as.integer(want))

  # Exactly the plan's certainty set, with first-stage weight one.
  expect_setequal(
    psu$psu_id[psu$.certainty_1],
    plan$psu$psu_id[plan$psu$certainty]
  )
  expect_equal(unique(psu$.weight_1[psu$.certainty_1]), 1)

  # Drawn PSUs carry the plan's own probabilities exactly:
  # pik = n_psu_draw * N_i / sum(N_rest).
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
  # Until the per-PSU take handoff ships, stage 2 is the caller's own; a
  # constant take must keep working under a bridged stage 1.
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
  # Deserialization skips draw(), so the gate cannot rely on the draw-time
  # check; a tampered spec stands in for a file until the format carries one.
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  d <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N)
  # Raise the remainder draw while keeping the stage total consistent with
  # it, so the totals gate passes and the disagreement gate is what fires:
  # 6 of B's noncertainty PSUs caps the 220-size PSU (6 * 220 / 1150 > 1).
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

test_that("a bridge design writes format 3 and plain designs stay lower", {
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
    1L
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

  # A stage size de-synced from the plan's own totals: only draw() built the
  # identity, and a file never ran draw(), so the gate re-derives it.
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

  # The whole-unit design, stratum by stratum and PSU by PSU: each
  # certainty PSU contributes exactly its own take, each drawn PSU exactly
  # n_per_psu, so the element totals equal n_int.
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

  # Element weights are the pool over its take, exactly, and the compound
  # weight is the product of the stages.
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
  # The fixture's sexes alternate, so a proportional split is even up to
  # the whole unit an odd take cannot halve (B01's take is 19).
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

  e <- as_svydesign(s)
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

  # Bare stratification would draw the take per cell; alloc states the split.
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
    stage2() |> draw(n = certainty_disagreement_fixture()),
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
  # The probability part's diagonal is its first-order inclusion.
  expect_equal(d[!psu$.certainty_1], 1 / psu$.weight_1[!psu$.certainty_1])
})