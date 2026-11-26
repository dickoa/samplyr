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
  err <- tryCatch(
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum, person) |>
      cluster_by(psu_id) |>
      draw(n = plan, method = "pps_systematic", mos = N),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_svyplan_certainty_plan")
  expect_match(conditionMessage(err), "names its strata in one column",
               fixed = TRUE)
  expect_error(
    sampling_design() |>
      add_stage() |>
      stratify_by(stratum) |>
      cluster_by(psu_id, stratum) |>
      draw(n = plan, method = "pps_systematic", mos = N),
    class = "samplyr_error_svyplan_certainty_plan"
  )
})

test_that("a plan without psu_id or without take_2 is refused", {
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
  old$psu$take_2 <- NULL
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
  # One PSU carrying two sizes across its rows.
  two_sizes <- frame
  first_b04 <- which(two_sizes$psu_id == "B04")[1]
  two_sizes$N[first_b04] <- two_sizes$N[first_b04] + 1
  refused(two_sizes)
})

test_that("validation refuses a register the plan was not solved on", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  frame$id <- seq_len(nrow(frame))
  d <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |> draw(n = plan)
  quietly <- function(expr) suppressWarnings(suppressMessages(expr))
  routes <- list(
    validate_frame = function(f) validate_frame(d, f),
    execute = function(f) execute(d, f, seed = 1),
    exante_digest = function(f) exante_digest(d, f),
    exante_probabilities = function(f) {
      exante_probabilities(d, f, key = id)
    },
    frame_summary = function(f) frame_summary(d, f)
  )
  for (route in names(routes)) {
    expect_no_error(quietly(routes[[route]](frame)))
  }
  edits <- list(
    "changed size" = transform(frame, N = ifelse(psu_id == "B04", N + 1, N)),
    "changed stratum" = transform(
      frame, stratum = ifelse(psu_id == "B04", "A", stratum)
    ),
    "dropped PSU" = frame[frame$psu_id != "A05", ]
  )
  checked <- character(0)
  for (edit in names(edits)) {
    for (route in names(routes)) {
      info <- paste(edit, route)
      err <- expect_error(
        quietly(routes[[route]](edits[[edit]])),
        class = "samplyr_error_certainty_register_mismatch", info = info
      )
      expect_match(cli::ansi_strip(conditionMessage(err)), "at\\s+stage 1",
                   info = info)
      checked <- c(checked, info)
    }
  }
  expect_length(unique(checked), 15L)
  err <- expect_error(
    validate_frame(d, edits[["changed size"]]),
    class = "samplyr_error_certainty_register_mismatch"
  )
  expect_match(cli::ansi_strip(conditionMessage(err)), "PSU \"B04\"")

  # Separate registers are read the same way.
  psus <- unique(frame[c("psu_id", "stratum", "N")])
  expect_true(quietly(validate_frame(d, list(psus, frame))))
  resized <- transform(psus, N = ifelse(psu_id == "B04", N + 1, N))
  expect_error(
    validate_frame(d, list(resized, frame)),
    class = "samplyr_error_certainty_register_mismatch"
  )

  # A continued take stage reads the selected PSUs only, and refuses one the
  # register does not list.
  s1 <- quietly(execute(d, frame, stages = 1, seed = 7))
  selected <- frame[frame$psu_id %in% s1$psu_id, ]
  expect_true(quietly(validate_frame(s1, selected, stages = 2)))
  unknown <- rbind(
    selected,
    transform(selected[selected$psu_id == selected$psu_id[1], ][1:3, ],
              psu_id = "A99")
  )
  for (route in c("validate_frame", "execute")) {
    expect_error(
      quietly(if (route == "execute") {
        execute(s1, unknown, seed = 8)
      } else {
        validate_frame(s1, unknown, stages = 2)
      }),
      class = "samplyr_error_certainty_register_mismatch", info = route
    )
  }
})

test_that("the register check does not depend on id type or row order", {
  plan <- certainty_plan_fixture()
  frame <- certainty_element_frame()
  d <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N)
  base <- execute(d, frame, seed = 1)

  as_factor <- transform(frame, psu_id = factor(psu_id))
  expect_identical(
    as.character(execute(d, as_factor, seed = 1)$psu_id),
    as.character(base$psu_id)
  )
  # Systematic selection follows frame order, so only the check is compared.
  shuffled <- frame[rev(seq_len(nrow(frame))), ]
  expect_identical(
    length(unique(execute(d, shuffled, seed = 1)$psu_id)),
    length(unique(base$psu_id))
  )
})

test_that("the register rows deduplicate the values the register compares", {
  # Two stratum codes that differ as numbers but print alike, in the key
  # text that getOption("scipen") does not change.
  frame <- data.frame(
    psu_id = c("A01", "A01", "A02", "A02"),
    stratum = c(0.3, 0.1 + 0.2, 0.3, 0.3),
    N = c(2L, 2L, 3L, 3L)
  )
  expect_identical(
    first_register_rows(frame, "psu_id", "stratum", "N"),
    data.frame(psu_id = c("A01", "A02"), stratum = "0.3", N = c(2, 3))
  )
  expect_identical(
    first_register_rows(frame, "psu_id", "stratum"),
    data.frame(psu_id = c("A01", "A02"), stratum = "0.3")
  )
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

test_that("bridge and plain designs both write format 5", {
  d <- certainty_bridge_design()
  path <- withr::local_tempfile(fileext = ".json")
  write_design(d, path)
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(doc$format_version, 5L)
  expect_identical(doc$design$stages[[1]]$draw$certainty_plan$role, "select")
  expect_identical(doc$design$stages[[2]]$draw$certainty_plan$role, "take")
  # The plan's nominal take below the PSUs, under svyplan's stage name.
  block <- doc$design$stages[[1]]$draw$certainty_plan
  expect_identical(block$take_2, list(A = 10L, B = 10L))
  expect_null(block$n_per_psu)

  plain <- withr::local_tempfile(fileext = ".json")
  write_design(sampling_design() |> draw(n = 10), plain)
  expect_identical(
    jsonlite::fromJSON(plain, simplifyVector = FALSE)$format_version,
    5L
  )

  # A format 3 file named the take `n_per_psu`. Only the current format reads.
  old <- certainty_tampered_file(path, function(doc) {
    doc$format_version <- 3L
    for (k in 1:2) {
      cp <- doc$design$stages[[k]]$draw$certainty_plan
      names(cp)[names(cp) == "take_2"] <- "n_per_psu"
      doc$design$stages[[k]]$draw$certainty_plan <- cp
    }
    doc
  })
  expect_error(
    read_design(old),
    class = "samplyr_error_design_file_unsupported"
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
        r$take_2 <- NULL
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
    doc$design$stages[[1]]$draw$certainty_plan$register[[1]]$take_2 <- 2.5
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
    doc$format_version <- 6
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
  takes <- stats::setNames(plan$psu$take_2, plan$psu$psu_id)
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
  takes <- stats::setNames(plan$psu$take_2, plan$psu$psu_id)
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
  takes <- stats::setNames(plan$psu$take_2, plan$psu$psu_id)
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

segment_frame <- function() {
  frame <- certainty_element_frame()
  frame$segment <- ceiling(frame$person / 100)
  frame$segment_size <- ave(
    frame$person, frame$psu_id, frame$segment, FUN = length
  )
  frame
}

segment_design <- function(plan, per_psu = 1) {
  sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    cluster_by(segment) |>
    draw(n = per_psu, method = "pps_systematic", mos = segment_size) |>
    add_stage()
}

test_that("one segment per PSU takes the plan's take inside it", {
  plan <- certainty_plan_fixture()
  s <- segment_design(plan) |>
    draw(n = plan) |>
    execute(segment_frame(), seed = 1)
  per_psu <- table(s$psu_id)
  takes <- stats::setNames(plan$psu$take_2, plan$psu$psu_id)
  expect_identical(as.integer(per_psu), as.integer(takes[names(per_psu)]))
  # One segment of each PSU, and its take drawn with equal chances in it.
  expect_true(all(tapply(s$segment, s$psu_id, function(x) {
    length(unique(x))
  }) == 1L))
  expect_equal(s$.weight_3,
               unname(s$segment_size / takes[as.character(s$psu_id)]))
})

test_that("below a stage that keeps more than one unit the refusal names the take table", {
  plan <- certainty_plan_fixture()
  segments <- segment_design(plan, per_psu = 2)
  err <- expect_error(
    segments |> draw(n = plan),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, "stage 3", fixed = TRUE)
  expect_match(msg, "stratify_by(psu_id)", fixed = TRUE)
  expect_no_match(msg, "stage 1 with", fixed = TRUE)

  expect_match(msg, "take_2 / k", fixed = TRUE)

  # The route it names fields each PSU's take, split over its two segments.
  takes <- data.frame(psu_id = plan$psu$psu_id, n = ceiling(plan$psu$take_2 / 2))
  s <- segments |>
    stratify_by(psu_id) |>
    draw(n = takes) |>
    execute(segment_frame(), seed = 1)
  per_psu <- table(s$psu_id)
  expect_identical(
    as.integer(per_psu),
    2L * as.integer(stats::setNames(takes$n, takes$psu_id)[names(per_psu)])
  )

  # At stage 1 without clusters the advice is the stage 1 bridge only.
  err <- expect_error(
    sampling_design() |> stratify_by(stratum) |> draw(n = plan),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_no_match(msg, "stratify_by(psu_id)", fixed = TRUE)
  expect_match(msg, "stage 1 with", fixed = TRUE)
})

## The printed design shows the plan's sizes

test_that("the print shows a certainty plan's classification and takes", {
  plan <- certainty_plan_fixture()
  d <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    draw(n = plan)
  printed <- cli::ansi_strip(utils::capture.output(print(d)))

  n_certain <- sum(plan$psu$certainty)
  n_drawn <- sum(plan$detail$n_psu_draw)
  expect_equal(c(n_certain, n_drawn), c(9, 4))
  expect_true(any(grepl(
    "n = <svyplan plan, 9 certainty, 4 drawn, per stratum>", printed,
    fixed = TRUE
  )))
  takes <- range(plan$psu$take_2)
  expect_equal(takes, c(9, 26))
  expect_true(any(grepl(
    "n = <svyplan plan, 9 to 26 per psu_id>", printed, fixed = TRUE
  )))

  # A file round trip prints the same design.
  path <- withr::local_tempfile(fileext = ".json")
  write_design(d, path)
  expect_identical(
    cli::ansi_strip(utils::capture.output(print(read_design(path)))),
    printed
  )
})

test_that("the print shows the zone size of a zoned plan", {
  plan <- certainty_zone_fixture()
  d <- sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N)
  printed <- cli::ansi_strip(utils::capture.output(print(d)))

  expected <- sprintf(
    "n = <svyplan plan, %d certainty, %d drawn, 2 per zone, per stratum>",
    sum(plan$psu$certainty), sum(plan$detail$n_psu_draw)
  )
  expect_true(any(grepl(expected, printed, fixed = TRUE)))
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
    pps_brewer = "c4669a265bb10cd720cac895fbe79070",
    pps_systematic = "2cb810449490a4b86dd9fe11097ccc07"
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
    pps_brewer = "9bdbd83715a73602dce7c0cbea10aa63",
    pps_systematic = "c66f93c970453dd57864039213932976"
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
    pps_systematic = "fb9d289b5c88d3007cc517404ed1b576",
    pps_brewer = "091c376010e8f543028cec82395f0a75",
    pps_cps = "b0adca7558370accb4ce33d9213208bf",
    pps_sampford = "44027a6aee085e497b959ae785bb12a8"
  )
  zoned <- c(
    pps_systematic = "ff50191188e46b778f0f82352a661d83",
    pps_brewer = "ec926bdac65a45ca78ddff3997c103c2",
    pps_cps = "2df90c13fbb2c8b95becf5b8ceff8e47",
    pps_sampford = "c08f3c1eb0a1c5ef2cbb77331a7139a3"
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
  expect_identical(length(unique(e$strata[, 1])), 7L)
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
        data, 2L * max(zone), method, mos,
        list(method = method, mos = "mos"), zone, 2L
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
  expect_identical(doc$format_version, 5L)
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
      , c("psu_id", "stratum", ".weight_1", ".certainty_1", ".zone_1", ".variance_group_1")
    ])
    at <- match(psu$psu_id, plan$psu$psu_id)

    expect_setequal(
      psu$psu_id[psu$.certainty_1],
      plan$psu$psu_id[plan$psu$certainty]
    )
    expect_identical(psu$.zone_1, plan$psu$.zone[at], label = method)
    expect_identical(psu$.variance_group_1, plan$psu$.variance_group[at], label = method)
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
    pps_systematic = "fb0c200636c30b34d588bcd74360a3bf",
    pps_brewer = "fb0c200636c30b34d588bcd74360a3bf",
    pps_cps = "972a8ad0b5ef30f7f850fbc5968f0fd1",
    pps_sampford = "fb0c200636c30b34d588bcd74360a3bf"
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
  p$psu$.variance_group <- NULL
  refused(p, "records no variance groups")

  p <- certainty_zone_fixture()
  p$psu$.variance_group <- ifelse(is.na(p$psu$.zone), NA_integer_, 1L)
  refused(p, "only a plan drawing one PSU per zone")

  p <- plan
  p$psu$.variance_group[p$psu$psu_id == "A01"] <- 1L
  refused(p, "a group without a zone")

  p <- plan
  p$psu$.variance_group[which(in_zone(p, "A", 1))[1]] <- 2L
  refused(p, "is split across variance groups")

  p <- plan
  p$psu$.variance_group[p$psu$.variance_group %in% 4] <- 5L
  refused(p, "numbered from 1 without gaps")

  # Zone A2 joins group 2, so group 1 is left with one zone.
  p <- plan
  p$psu$.variance_group[in_zone(p, "A", 2)] <- 2L
  refused(p, "group 1 holds 1 zone")

  # Groups 3 and 4 merged, A's last two zones with the triple of B and C.
  p <- plan
  p$psu$.variance_group[p$psu$.variance_group %in% 4] <- 3L
  refused(p, "group 3 holds 5 zones")

  p <- plan
  p$psu$.variance_group[in_zone(p, "C", 1)] <- 4.5
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
    reg$variance_group[reg$variance_group %in% 4] <- 3L
    reg
  }))
  gate(tamper(d, function(reg) {
    reg$variance_group <- NULL
    reg
  }))
})

test_that("the variance-group columns are reserved in a frame", {
  for (col in c(".variance_group", ".variance_group_1")) {
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
    samplyr_reserved_names(c(".variance_group", ".variance_group_2", ".variance_groups", ".variance_group_x")),
    c(".variance_group", ".variance_group_2")
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
  expect_identical(strata$user, ".variance_group_1")
  units <- unique(data.frame(
    id = strata$id, psu = s$psu_id, cert = s$.certainty_1, variance_group = s$.variance_group_1
  ))
  drawn <- units[!units$cert, ]
  # One stratum per group, holding its two or three zones' PSUs.
  expect_identical(
    as.vector(table(drawn$variance_group)[as.character(1:4)]),
    c(2L, 2L, 2L, 3L)
  )
  expect_identical(nrow(unique(drawn[c("id", "variance_group")])), 4L)
  expect_length(unique(drawn$id), 4L)
  expect_length(intersect(units$id[units$cert], drawn$id), 0L)
  # The group of B's third zone and C's only zone crosses strata.
  expect_setequal(unique(s$stratum[s$.variance_group_1 %in% 4]), c("B", "C"))

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
  grp <- tapply(rem$.variance_group_1, rem$psu_id, unique)
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
  set.seed(3)
  r <- suppressWarnings(as_svrepdesign(s, type = "rwyb", replicates = 2000))
  ratio <- stats::vcov(survey::svytotal(~y, r))[1] / v_lin
  expect_gt(ratio, 0.9)
  expect_lt(ratio, 1.1)
  # PSU A01 is certain and alone in its stratum, and survey's multistage
  # bootstrap gave its households no variance.
  expect_error(
    as_svrepdesign(s, type = "mrbbootstrap", replicates = 20),
    class = "samplyr_error_svrep_conversion_failed"
  )
  # RWYB resamples the zones as drawn with replacement, says so, and does
  # not warn of an approximation it does not make.
  expect_no_warning(r <- as_svrepdesign(s, type = "rwyb", replicates = 20))
  record <- attr(r, "samplyr_replication")
  expect_identical(record$methods, c("PPSWR", "SRSWOR"))
  expect_false(record$pps_approximation)
  expect_identical(
    v$status[v$estimator == "Rao-Wu-Yue-Beaumont bootstrap"], "supported"
  )
})

test_that("a plan of a single zone refuses replicates before the frame is read", {
  skip_if_not_installed("survey")
  skip_if_not_installed("svrep")
  # One certain PSU and one zone of five: the zone's one draw is a variance
  # group alone, which the plan fixes whatever the frame.
  register <- data.frame(
    psu_id = sprintf("A%02d", 1:6), stratum = "A",
    N = c(400, 60, 70, 80, 90, 100), stringsAsFactors = FALSE
  )
  plan <- svyplan::n_alloc_joint(
    data.frame(stratum = "A", N = sum(register$N), take_2 = 8),
    measures = data.frame(stratum = "A", name = "y", p = 0.5,
                          icc_1 = 0.05),
    targets = data.frame(name = "y", cv = 0.3),
    psu = register, n_psu_per_zone = 1
  )
  expect_identical(unique(stats::na.omit(plan$psu$.variance_group)), 1L)
  expect_length(unique(stats::na.omit(plan$psu$.zone)), 1L)
  design <- certainty_zone_design(plan)
  frame <- certainty_element_frame(register)
  frame$y <- frame$person %% 5
  replicates <- c("Stratified jackknife (JKn)",
                  "Rao-Wu-Yue-Beaumont bootstrap")
  ahead <- variance_estimators(design)
  framed <- suppressMessages(variance_estimators(design, frame))
  for (est in replicates) {
    expect_identical(ahead$status[ahead$estimator == est], "refused",
                     label = est)
    expect_identical(ahead$class[ahead$estimator == est],
                     framed$class[framed$estimator == est], label = est)
  }
  s <- suppressMessages(execute(design, frame, seed = 1))
  expect_no_warning(expect_error(
    as_svrepdesign(s, type = "rwyb", replicates = 20),
    class = "samplyr_error_rwyb_singleton"
  ))
  expect_error(as_svrepdesign(s, type = "JKn"),
               class = "samplyr_error_svrep_conversion_failed")
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
  # At a cv of 0.13 every group of the zone fixture is a pair, so the
  # groups do not refuse.
  plan <- certainty_zone_fixture(m = 1, cv = 0.13)
  groups <- unique(plan$psu[!is.na(plan$psu$.variance_group), c("stratum", ".zone", ".variance_group")])
  expect_true(all(table(groups$.variance_group) == 2L))
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
    function(r) if (is.null(r$variance_group)) NA_integer_ else as.integer(r$variance_group),
    integer(1)
  )
  expect_identical(pairs, as.integer(plan$psu$.variance_group))

  rd <- read_design(path)
  spec <- rd$stages[[1]]$draw_spec$certainty_plan
  expect_identical(spec$n_psu_per_zone, 1L)
  expect_identical(spec$register$variance_group, as.integer(plan$psu$.variance_group))

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
      r$variance_group <- NULL
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
    b$register[[2]]$variance_group <- 0
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
    svyplan::n_alloc_joint(
      transform(stats::aggregate(N ~ stratum, psu, sum), take_2 = 8),
      measures = data.frame(stratum = c("A", "B"), name = "y", p = 0.5,
                            icc_1 = 0.05),
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

## Register fits of svyplan 0.26.0

# A take solved by svyplan, between 8 and 30 per PSU. Stratum A holds all
# its PSUs certain, B holds B01 and draws 3 of the rest.
certainty_free_take_fixture <- function() {
  svyplan::n_alloc_joint(
    data.frame(stratum = c("A", "B"), N = c(2000, 1600), cost_1 = 300,
               cost_2 = 30),
    data.frame(stratum = c("A", "B"), name = "y", p = c(0.5, 0.4),
               icc_1 = 0.05),
    targets = data.frame(name = "y", cv = 0.10),
    psu = certainty_plan_register(), take_2_min = 8, take_2_max = 30
  )
}

# Each sampled row's stage-1 chance under the register's split, and its
# stage-2 chance within its PSU.
certainty_planned_chances <- function(plan, s) {
  psu <- plan$psu
  draw <- stats::setNames(plan$detail$n_psu_draw, plan$detail$stratum)
  rest <- ave(ifelse(psu$certainty, 0, psu$N), psu$stratum, FUN = sum)
  pik <- ifelse(psu$certainty, 1, draw[psu$stratum] * psu$N / rest)
  at <- match(s$psu_id, psu$psu_id)
  list(stage_1 = unname(pik[at]), stage_2 = psu$take_2[at] / psu$N[at])
}

test_that("a register fit reports two stages and the bridge fields them", {
  for (plan in list(certainty_plan_fixture(), certainty_free_take_fixture())) {
    expect_identical(plan$stages, 2L)
    expect_identical(plan$params$stages, 1L)
    s <- execute(certainty_bridge_design(plan), certainty_element_frame(),
                 seed = 41)
    d <- as.data.frame(s)
    psus <- unique(d[c("psu_id", "stratum")])
    certain <- plan$psu$psu_id[plan$psu$certainty]
    expect_true(all(certain %in% psus$psu_id))
    expect_identical(
      c(table(factor(psus$stratum, c("A", "B")))),
      stats::setNames(
        as.integer(plan$detail$n_psu_certain + plan$detail$n_psu_draw),
        plan$detail$stratum
      )
    )
    # Each PSU gets its own take, not the stratum's nominal one.
    per_psu <- c(table(d$psu_id))
    expect_identical(
      unname(per_psu),
      as.integer(plan$psu$take_2[match(names(per_psu), plan$psu$psu_id)])
    )
    chance <- certainty_planned_chances(plan, d)
    expect_equal(d$.weight_1, 1 / chance$stage_1, tolerance = 1e-12)
    expect_equal(d$.weight_2, 1 / chance$stage_2, tolerance = 1e-12)
    expect_equal(d$.weight, d$.weight_1 * d$.weight_2, tolerance = 1e-12)
  }
})

test_that("the spec records the nominal take a register fit was given or solved", {
  fixed <- certainty_bridge_design()$stages[[1]]$draw_spec$certainty_plan
  expect_identical(fixed$take_2, c(A = 10, B = 10))
  plan <- certainty_free_take_fixture()
  solved <- certainty_bridge_design(plan)$stages[[1]]$draw_spec$certainty_plan
  expect_identical(
    solved$take_2,
    stats::setNames(as.numeric(plan$detail$take_2_int), plan$detail$stratum)
  )
  # A plan that records no take keeps NA, and the take stage still fields
  # each PSU's take_2.
  untaken <- plan
  untaken$detail$take_2_int <- NULL
  spec <- certainty_bridge_design(untaken)$stages[[1]]$draw_spec$certainty_plan
  expect_identical(spec$take_2, c(A = NA_real_, B = NA_real_))
})

test_that("a free-take register design writes, reads and replays", {
  plan <- certainty_free_take_fixture()
  frame <- certainty_element_frame()
  s <- execute(certainty_bridge_design(plan), frame, seed = 43)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(s, path, frame = frame)
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  expect_identical(doc$format_version, 5L)
  restored <- read_design(path)
  expect_equal(
    restored$stages[[1]]$draw_spec$certainty_plan,
    certainty_bridge_design(plan)$stages[[1]]$draw_spec$certainty_plan
  )
  # The file alone replays the sample, with no svyplan object.
  expect_identical(
    certainty_sample_key(execute(restored, frame, seed = 43)),
    certainty_sample_key(s)
  )
})

test_that("a register fit rebuilt from its assessment fields the same sample", {
  plan <- certainty_plan_fixture()
  rebuilt <- svyplan::n_alloc_joint(svyplan::prec_alloc_joint(plan))
  expect_identical(rebuilt$psu, plan$psu)
  expect_identical(
    rebuilt$detail[c("n_psu_certain", "n_psu_draw")],
    plan$detail[c("n_psu_certain", "n_psu_draw")]
  )
  frame <- certainty_element_frame()
  expect_identical(
    certainty_sample_key(execute(certainty_bridge_design(rebuilt), frame,
                                 seed = 47)),
    certainty_sample_key(execute(certainty_bridge_design(plan), frame,
                                 seed = 47))
  )
})

test_that("a one-per-zone plan with every PSU certain writes, reads and replays", {
  psus <- data.frame(stratum = "A", psu_id = 1:6, N = 100L, certainty = TRUE)
  plan <- svyplan::n_alloc_joint(
    data.frame(stratum = "A", N = sum(psus$N), take_2 = 8),
    data.frame(stratum = "A", name = "y", p = 0.5, icc_1 = 0.05),
    targets = data.frame(name = "y", cv = 0.15),
    psu = psus, n_psu_per_zone = 1
  )
  # No remainder is drawn, so the plan has no zones and no variance groups.
  expect_identical(plan$detail$n_psu_draw, 0)
  expect_true(all(is.na(plan$psu$.variance_group)))
  frame <- psus[rep(seq_len(nrow(psus)), times = psus$N), ]
  frame$id <- seq_len(nrow(frame))
  design <- sampling_design() |>
    stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N) |>
    add_stage() |> draw(n = plan)
  sample <- execute(design, frame, seed = 7)

  rows <- function(s) {
    d <- as.data.frame(s)
    d <- d[order(d$id), c("id", "psu_id", ".weight_1", ".weight_2", ".weight")]
    rownames(d) <- NULL
    d
  }
  d <- rows(sample)
  take <- plan$psu$take_2[match(d$psu_id, plan$psu$psu_id)]
  expect_equal(d$.weight_1, rep(1, nrow(d)))
  expect_equal(d$.weight_2, 100 / take)

  for (x in list(design, sample)) {
    path <- withr::local_tempfile(fileext = ".json")
    write_design(x, path, frame = frame)
    restored <- read_design(path)
    for (k in 1:2) {
      spec <- restored$stages[[k]]$draw_spec$certainty_plan
      expect_identical(spec$n_psu_per_zone, 1L)
      expect_identical(spec$register$variance_group, rep(NA_integer_, 6L))
    }
    expect_identical(rows(execute(restored, frame, seed = 7)), d)
  }
})

test_that("an all-null variance group column keeps the populated checks", {
  plan <- certainty_pair_fixture()
  path <- withr::local_tempfile(fileext = ".json")
  write_design(certainty_zone_design(plan), path)
  # A fractional group is refused, while a null one is read.
  for (bad in list(1.5, 0)) {
    tampered <- certainty_tampered_file(path, function(doc) {
      reg <- doc$design$stages[[1]]$draw$certainty_plan$register
      at <- which(!vapply(reg, function(r) is.null(r$variance_group), TRUE))[1]
      doc$design$stages[[1]]$draw$certainty_plan$register[[at]]$variance_group <- bad
      doc
    })
    expect_error(read_design(tampered),
                 class = "samplyr_error_design_file_malformed")
  }
})

## Refusals that list PSU ids. cli reads a number beside a plural marker as
## the marker's quantity and fails on a numeric vector, so a message lists
## the ids as text and states the quantity.

# A register whose PSU ids are whole numbers from 100000, so a number
# formatted by default would read 1e+05.
numeric_id_register <- function() {
  register <- certainty_plan_register()
  register$psu_id <- 100000L + seq_len(nrow(register)) - 1L
  register
}

flat_message <- function(err) {
  gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
}

test_that("register problems list numeric PSU ids with the verb of their count", {
  base <- data.frame(
    psu_id = 100000L + 0:5, stratum = "A", N = 10, certainty = FALSE,
    zone = rep(1:3, each = 2L), variance_group = c(1L, 1L, 1L, 1L, 2L, 2L)
  )
  two <- base
  two$variance_group[3:4] <- NA
  expect_identical(
    certainty_pair_problem(two, 1L),
    "PSU \"100002\" and \"100003\" have a zone without a variance group, or a group without a zone."
  )
  one <- base
  one$variance_group[3] <- NA
  one$zone[4] <- NA
  expect_match(certainty_pair_problem(one, 1L), "\"100002\" and \"100003\" have",
               fixed = TRUE)
  one$zone[4] <- 2L
  expect_match(certainty_pair_problem(one, 1L), "PSU \"100002\" has a zone",
               fixed = TRUE)

  stray <- base[1:4, ]
  stray$certainty <- c(TRUE, TRUE, FALSE, FALSE)
  expect_identical(
    certainty_zone_problem(stray, c(A = 2), 2L),
    "Certainty PSU \"100000\" and \"100001\" have a zone."
  )
  stray$certainty[2] <- FALSE
  expect_identical(
    certainty_zone_problem(stray, c(A = 2), 2L),
    "Certainty PSU \"100000\" has a zone."
  )

  # Zones of two PSUs, drawing two per zone, are censuses.
  small <- base[1:4, ]
  small$zone <- c(1L, 1L, 2L, 2L)
  expect_match(certainty_zone_problem(small, c(A = 4), 2L),
               "2 zones (1 and 2) hold 2 or fewer PSUs", fixed = TRUE)
  small <- base[1:5, ]
  small$zone <- c(1L, 1L, 1L, 2L, 2L)
  expect_match(certainty_zone_problem(small, c(A = 4), 2L),
               "1 zone (2) holds 2 or fewer PSUs", fixed = TRUE)
})

test_that("the disagreement check lists numeric PSU ids", {
  register <- data.frame(
    psu_id = 100000L + 0:5, stratum = "A",
    N = c(100, 100, 1, 1, 1, 1), certainty = FALSE
  )
  err <- tryCatch(
    check_certainty_plan_disagreement(register, c(A = 3), "pps_brewer"),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_certainty_plan_disagreement")
  msg <- flat_message(err)
  expect_match(msg, "PSU \"100000\" and \"100001\" an inclusion", fixed = TRUE)
  expect_match(msg, "holds them noncertainty", fixed = TRUE)
  register$N[2] <- 2
  msg <- flat_message(tryCatch(
    check_certainty_plan_disagreement(register, c(A = 2), "pps_brewer"),
    error = identity
  ))
  expect_match(msg, "PSU \"100000\" an inclusion", fixed = TRUE)
  expect_match(msg, "holds it noncertainty", fixed = TRUE)
})

test_that("a frame that disagrees with a numeric register lists its PSU ids", {
  register <- numeric_id_register()
  plan <- certainty_plan_fixture(psu = register)
  frame <- certainty_element_frame(register)
  design <- certainty_bridge_design(plan)
  ids <- register$psu_id
  refusal <- function(expr) {
    err <- tryCatch(expr, error = identity)
    expect_s3_class(err, "samplyr_error_certainty_register_mismatch")
    flat_message(err)
  }
  reached <- character(0)

  # Stage 1, against the whole register.
  msg <- refusal(execute(design, frame[!frame$psu_id %in% ids[5:6], ], seed = 1))
  expect_match(msg, "Register PSU \"100004\" and \"100005\" are absent",
               fixed = TRUE)
  msg <- refusal(execute(design, frame[frame$psu_id != ids[5], ], seed = 1))
  expect_match(msg, "Register PSU \"100004\" is absent", fixed = TRUE)
  reached <- c(reached, "absent")

  extra <- frame[frame$psu_id %in% ids[1:2], ]
  extra$psu_id <- extra$psu_id + 100000L
  msg <- refusal(execute(design, rbind(frame, extra), seed = 1))
  expect_match(msg, "Frame PSU \"200000\" and \"200001\" are not in the register",
               fixed = TRUE)
  reached <- c(reached, "extra")

  moved <- frame
  moved$stratum[moved$psu_id %in% ids[2:3]] <- "B"
  msg <- refusal(execute(design, moved, seed = 1))
  expect_match(msg, "PSU \"100001\" and \"100002\" are in a different stratum",
               fixed = TRUE)
  reached <- c(reached, "moved")

  doubled <- frame
  first <- !duplicated(doubled$psu_id) & doubled$psu_id %in% ids[2:3]
  doubled$N[first] <- doubled$N[first] + 1
  msg <- refusal(execute(design, doubled, seed = 1))
  expect_match(msg, "PSU \"100001\" and \"100002\" carry more than one", fixed = TRUE)
  reached <- c(reached, "doubled")

  # The take stage, against the PSUs of a listing frame.
  stage1 <- execute(design, frame, stages = 1, seed = 1)
  msg <- refusal(execute(stage1, rbind(frame, extra), seed = 2))
  expect_match(msg, "Frame PSU \"200000\" and \"200001\" are not in the register",
               fixed = TRUE)
  msg <- refusal(execute(stage1, moved, seed = 2))
  expect_match(msg, "PSU \"100001\" and \"100002\" are in a different stratum",
               fixed = TRUE)
  reached <- c(reached, "take")

  expect_false(grepl("e+05", msg, fixed = TRUE))
  expect_setequal(reached, c("absent", "extra", "moved", "doubled", "take"))
})

## A stage-1 design read back from file, then given the live plan at its
## take stage. The file reads whole numbers back as integers and factors as
## labels, which must not make the two registers differ.

test_that("a design read back takes the live plan at its take stage", {
  id_types <- list(
    character = function(x) x,
    integer = function(x) 100000L + seq_along(x),
    double = function(x) as.double(100000L + seq_along(x)),
    factor = function(x) factor(x)
  )
  reached <- character(0)
  for (zoning in c("none", "one", "two")) {
    for (type in names(id_types)) {
      register <- if (zoning == "none") {
        certainty_plan_register()
      } else {
        certainty_zone_register()
      }
      register$psu_id <- id_types[[type]](register$psu_id)
      plan <- switch(zoning,
        none = certainty_plan_fixture(psu = register),
        one = certainty_zone_fixture(psu = register, m = 1),
        two = certainty_zone_fixture(psu = register, m = 2)
      )
      frame <- certainty_element_frame(register)
      stage1 <- sampling_design() |>
        add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
        draw(n = plan, method = "pps_brewer", mos = N)
      path <- withr::local_tempfile(fileext = ".json")
      write_design(stage1, path)
      read <- read_design(path)
      stored <- read$stages[[1]]$draw_spec$certainty_plan$register
      expect_type(stored$N, "double")
      expect_type(stored$take_2, "double")

      label <- paste(zoning, type)
      resumed <- read |> add_stage() |> draw(n = plan)
      live <- stage1 |> add_stage() |> draw(n = plan)
      a <- suppressWarnings(execute(resumed, frame, seed = 3))
      b <- suppressWarnings(execute(live, frame, seed = 3))
      expect_identical(a$.sample_id, b$.sample_id, label = label)
      expect_equal(a$.weight, b$.weight, label = label)
      reached <- c(reached, label)
    }
  }
  expect_setequal(
    reached,
    paste(rep(c("none", "one", "two"), each = 4), names(id_types))
  )
})

test_that("the register comparison still tells two plans apart", {
  plan <- certainty_plan_fixture()
  stage1 <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(stage1, path)
  other <- plan
  other$psu$take_2[2] <- other$psu$take_2[2] + 1
  for (design in list(stage1, read_design(path))) {
    expect_error(
      design |> add_stage() |> draw(n = other),
      "not the plan stage 1 was built from",
      class = "samplyr_error_svyplan_certainty_plan"
    )
  }
  register <- stage1$stages[[1]]$draw_spec$certainty_plan$register
  expect_true(same_certainty_register(register, register))
  # A label never equals a number, and the row order counts.
  as_text <- register
  as_text$psu_id <- as.character(seq_len(nrow(register)))
  as_number <- register
  as_number$psu_id <- seq_len(nrow(register))
  expect_false(same_certainty_register(as_text, as_number))
  expect_false(same_certainty_register(register, register[nrow(register):1, ]))
  changed <- register
  changed$stratum[1] <- "B"
  expect_false(same_certainty_register(register, changed))
})

test_that("a different plan below a segment stage names the take's stage", {
  plan <- certainty_plan_fixture()
  other <- plan
  other$psu$take_2[2] <- other$psu$take_2[2] + 1
  err <- expect_error(
    segment_design(plan) |> draw(n = other),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_match(conditionMessage(err), "The plan at stage 3 is not", fixed = TRUE)
  stage1 <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N)
  err <- expect_error(
    stage1 |> add_stage() |> draw(n = other),
    class = "samplyr_error_svyplan_certainty_plan"
  )
  expect_match(conditionMessage(err), "The plan at stage 2 is not", fixed = TRUE)
  expect_no_error(segment_design(plan) |> draw(n = plan))
})

## PSU ids are matched to the register by value. Base R compares a number
## with a label as text, and writes 100000 as "1e+05", so a match used to
## depend on the size of the ids.

test_that("PSU ids match the register by value at every entry point", {
  withr::local_options(scipen = 0)
  ids <- c(100000:100007, 200000:200007)
  as_type <- list(I = as.integer, D = as.double, C = as.character)
  setup <- function(types) {
    register <- certainty_plan_register()
    register$psu_id <- as_type[[types[1]]](ids)
    plan <- certainty_plan_fixture(psu = register)
    psu <- register
    psu$psu_id <- as_type[[types[2]]](ids)
    listing <- certainty_element_frame(register)
    listing$psu_id <- as_type[[types[3]]](listing$psu_id)
    list(
      register = register,
      plan = plan,
      design = certainty_bridge_design(plan),
      frames = list(psu, listing)
    )
  }
  reached <- character(0)

  # Numbers against numbers, whatever their storage, as with labels.
  for (types in list(c("I", "I", "D"), c("D", "D", "I"), c("I", "D", "D"),
                     c("C", "C", "C"))) {
    label <- paste(types, collapse = "/")
    case <- setup(types)
    reference <- setup(rep(if (types[1] == "C") "C" else "I", 3))
    s <- suppressWarnings(execute(case$design, case$frames, seed = 1))
    ref <- suppressWarnings(execute(reference$design, reference$frames, seed = 1))
    expect_identical(s$.sample_id, ref$.sample_id, label = label)
    # Each PSU's take is the plan's, read from the plan's own register,
    # whose rows are in the order of `ids`.
    position <- match(format(s$psu_id, scientific = FALSE, trim = TRUE),
                      format(ids, scientific = FALSE, trim = TRUE))
    expect_false(anyNA(position))
    expect_equal(
      as.vector(table(position)),
      case$plan$psu$take_2[sort(unique(position))],
      label = label
    )
    # Continuing from stage 1.
    stage1 <- execute(case$design, case$frames[[1]], stages = 1, seed = 1)
    expect_no_error(suppressWarnings(execute(stage1, case$frames[[2]], seed = 1)))
    # The frame digest resolves each take pool again.
    full <- suppressWarnings(
      execute(case$design, case$frames, seed = 1, frame_digest = "full")
    )
    expect_identical(full$.sample_id, s$.sample_id, label = label)
    reached <- c(reached, label)
  }

  # Numbers on one side and labels on the other are refused, small or
  # large, before any draw.
  for (types in list(c("C", "D", "D"), c("D", "C", "C"), c("C", "C", "D"),
                     c("I", "C", "C"))) {
    label <- paste(types, collapse = "/")
    case <- setup(types)
    err <- expect_error(
      suppressWarnings(execute(case$design, case$frames, seed = 1)),
      class = "samplyr_error_certainty_register_mismatch"
    )
    expect_match(conditionMessage(err), "of different types", fixed = TRUE)
    reached <- c(reached, label)
  }
  small <- setup(c("C", "D", "D"))
  small$frames[[1]]$psu_id <- seq_along(ids)
  small$frames[[2]]$psu_id <- as.double(match(small$frames[[2]]$psu_id, ids))
  small$register$psu_id <- as.character(seq_along(ids))
  expect_error(
    suppressWarnings(execute(
      certainty_bridge_design(certainty_plan_fixture(psu = small$register)),
      small$frames, seed = 1
    )),
    class = "samplyr_error_certainty_register_mismatch"
  )
  expect_setequal(reached, c("I/I/D", "D/D/I", "I/D/D", "C/C/C",
                             "C/D/D", "D/C/C", "C/C/D", "I/C/C"))
})

## 64-bit integer PSU ids (bit64). A double holds every integer only up to
## 2^53, so narrowing such ids to doubles merges neighbors. The ids are
## built from decimal strings or by arithmetic on integer64, since a double
## literal would round the difference away before the code saw it.

i64 <- function(x) bit64::as.integer64(x)

large_id_register <- function(register = certainty_plan_register(),
                              from = "9007199254740992", step = 1L) {
  register$psu_id <- i64(from) + step * (seq_len(nrow(register)) - 1L)
  register
}

test_that("64-bit PSU ids above 2^53 draw what their text labels draw", {
  skip_if_not_installed("bit64")
  register <- large_id_register()
  expect_lt(length(unique(suppressWarnings(as.numeric(register$psu_id)))),
            length(unique(register$psu_id)))
  as_text <- register
  as_text$psu_id <- as.character(register$psu_id)
  draws <- function(reg) {
    plan <- suppressWarnings(certainty_plan_fixture(psu = reg))
    design <- certainty_bridge_design(plan)
    frame <- certainty_element_frame(reg)
    list(
      one_call = suppressWarnings(execute(design, frame, seed = 7)),
      staged = suppressWarnings(execute(
        execute(design, frame, stages = 1, seed = 7), frame, seed = 7
      )),
      digest = suppressWarnings(execute(design, frame, seed = 7,
                                        frame_digest = "full"))
    )
  }
  wide <- draws(register)
  text <- draws(as_text)
  for (route in names(wide)) {
    expect_identical(wide[[route]]$.sample_id, text[[route]]$.sample_id,
                     label = route)
    expect_equal(wide[[route]]$.weight, text[[route]]$.weight, label = route)
    expect_identical(as.character(wide[[route]]$psu_id),
                     text[[route]]$psu_id, label = route)
    expect_s3_class(wide[[route]]$psu_id, "integer64")
  }
  # Each PSU's take is the plan's, read from the plan's register by its id.
  plan <- suppressWarnings(certainty_plan_fixture(psu = register))
  takes <- table(as.character(wide$one_call$psu_id))
  expect_equal(
    as.vector(takes),
    plan$psu$take_2[match(names(takes), as.character(plan$psu$psu_id))]
  )
  # A design saved as RDS keeps the type and takes the live plan again.
  path <- withr::local_tempfile(fileext = ".rds")
  stage1 <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N)
  saveRDS(stage1, path)
  expect_no_error(readRDS(path) |> add_stage() |> draw(n = plan))
})

test_that("zoned 64-bit PSU ids keep their certainty, zone and group", {
  skip_if_not_installed("bit64")
  register <- large_id_register(certainty_zone_register())
  plan <- suppressWarnings(certainty_zone_fixture(psu = register, m = 1))
  frame <- certainty_element_frame(register)
  s <- suppressWarnings(execute(certainty_zone_design(plan), frame, seed = 4))
  units <- as.data.frame(s)[!duplicated(as.character(s$psu_id)), ]
  at <- match(as.character(units$psu_id), as.character(plan$psu$psu_id))
  expect_false(anyNA(at))
  expect_identical(units$.certainty_1, plan$psu$certainty[at])
  expect_identical(units$.zone_1, as.integer(plan$psu$.zone[at]))
  expect_identical(units$.variance_group_1,
                   as.integer(plan$psu$.variance_group[at]))
})

test_that("a take plan differing in one 64-bit id is refused", {
  skip_if_not_installed("bit64")
  # Odd ids from 2^53 + 1, so 2^53 itself is free to be swapped in. As
  # doubles, 2^53 + 1 and 2^53 are the same number.
  register <- large_id_register(from = "9007199254740993", step = 2L)
  plan <- suppressWarnings(certainty_plan_fixture(psu = register))
  swapped <- register
  swapped$psu_id[1] <- i64("9007199254740992")
  other <- suppressWarnings(certainty_plan_fixture(psu = swapped))
  stage1 <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N)
  expect_error(stage1 |> add_stage() |> draw(n = other),
               "not the plan stage 1 was built from",
               class = "samplyr_error_svyplan_certainty_plan")
  expect_no_error(stage1 |> add_stage() |> draw(n = plan))
  # The execute gate compares the stored registers the same way.
  assembled <- stage1 |> add_stage() |> draw(n = plan)
  assembled$stages[[2]]$draw_spec$certainty_plan$register$psu_id[1] <-
    i64("9007199254740992")
  expect_error(
    suppressWarnings(execute(assembled, certainty_element_frame(register),
                             seed = 1)),
    class = "samplyr_error_svyplan_certainty_plan"
  )

  # The two largest ids read as NaN to a double comparison, so the
  # comparison is of the stored bits.
  top <- data.frame(
    psu_id = i64("9223372036854775807") - 1:0, stratum = "A", N = 1,
    certainty = FALSE, take_2 = 1
  )
  flipped <- top
  flipped$psu_id <- rev(top$psu_id)
  expect_true(same_certainty_register(top, top))
  expect_false(same_certainty_register(top, flipped))
})

test_that("64-bit ids are matched by value against every numeric type", {
  skip_if_not_installed("bit64")
  withr::local_options(scipen = 0)
  position <- function(ids, table) {
    register_id_positions(ids, table, "psu_id")
  }
  # The two largest and the two smallest valid ids.
  ends <- c(i64("9223372036854775807") - 1:0, -i64("9223372036854775807") + 0:1)
  expect_identical(position(ends[c(3, 1, 4, 2)], ends), c(3L, 1L, 4L, 2L))

  # Near 100000, both directions, integer and double.
  wide <- i64(99998L) + 0:4
  expect_identical(position(c(100000L, 100002L), wide), c(3L, 5L))
  expect_identical(position(c(100000, 100002), wide), c(3L, 5L))
  expect_identical(position(wide[c(5, 1)], 99998:100002), c(5L, 1L))
  expect_identical(position(wide[c(5, 1)], c(99998, 99999, 1e5, 100001, 100002)),
                   c(5L, 1L))
  # Above 2^53, a double equal to an id matches it, and none of its
  # neighbors, which no double can hold.
  big <- i64("9007199254740992") + 0:2
  expect_identical(position(2^53, big), 1L)
  expect_identical(position(2^53 + 2, big), 3L)
  expect_identical(position(big, c(2^53, 2^53 + 2)), c(1L, NA, 2L))

  # A double that is no whole number in range matches nothing, and keeps the
  # positions of the others. 100000.5 must not be truncated onto 100000.
  invalid <- c(100000, 100000.5, 2.5, Inf, -Inf, NA, NaN, 2^63, -2^63, 100002)
  expect_identical(position(invalid, wide), c(3L, rep(NA_integer_, 8), 5L))
  expect_identical(position(wide, c(100000.5, 100000, NA, 100002)),
                   c(NA, NA, 2L, NA, 4L))
  expect_identical(position(i64(integer(0)), wide), integer(0))

  # Numbers against labels are refused, as for every numeric type.
  expect_error(position(wide, as.character(99998:100002)),
               class = "samplyr_error_certainty_register_mismatch")
})

test_that("classed numeric ids match a 64-bit register by their values", {
  skip_if_not_installed("bit64")
  skip_if_not_installed("haven")
  register <- large_id_register(from = "100000")
  plan <- suppressWarnings(certainty_plan_fixture(psu = register))
  design <- certainty_bridge_design(plan)
  frame <- certainty_element_frame(register)
  values <- as.numeric(frame$psu_id)
  draws <- function(ids) {
    f <- frame
    f$psu_id <- ids
    list(
      one_call = suppressWarnings(execute(design, f, seed = 7)),
      staged = suppressWarnings(execute(
        execute(design, f, stages = 1, seed = 7), f, seed = 7
      )),
      digest = suppressWarnings(execute(design, f, seed = 7,
                                        frame_digest = "full"))
    )
  }
  control <- draws(values)
  classed <- list(
    labelled_double = haven::labelled(values, c(first = values[1])),
    labelled_integer = haven::labelled(as.integer(values))
  )
  for (kind in names(classed)) {
    got <- draws(classed[[kind]])
    for (route in names(control)) {
      label <- paste(kind, route)
      expect_identical(got[[route]]$.sample_id, control[[route]]$.sample_id,
                       label = label)
      expect_equal(got[[route]]$.weight, control[[route]]$.weight,
                   label = label)
      expect_identical(as.numeric(got[[route]]$psu_id),
                       as.numeric(control[[route]]$psu_id), label = label)
    }
  }
  takes <- table(as.numeric(control$one_call$psu_id))
  expect_equal(
    as.vector(takes),
    plan$psu$take_2[match(as.numeric(names(takes)),
                          as.numeric(plan$psu$psu_id))]
  )

  # The lookup in both directions, and a fraction left unmatched.
  wide <- i64(99998L) + 0:4
  position <- function(ids, table) {
    register_id_positions(ids, table, "psu_id")
  }
  expect_identical(position(haven::labelled(c(100000, 100002, 100000.5)),
                            wide), c(3L, 5L, NA))
  expect_identical(position(wide[c(5, 1)],
                            haven::labelled(c(99998, 100002))), c(2L, 1L))
  # Any class vctrs cannot cast to a 64-bit integer, as units is one.
  expect_identical(position(structure(c(100002, 99998), class = "psu_code"),
                            wide), c(5L, 1L))
  expect_error(position(haven::labelled(c(99998, 99999)),
                        as.character(99998:99999)),
               class = "samplyr_error_certainty_register_mismatch")

  # The plan comparison reads the stored values, and compares missing
  # positions one by one.
  reg <- function(id) {
    data.frame(psu_id = id, stratum = "A", N = 1, certainty = FALSE,
               take_2 = 1)
  }
  expect_true(same_certainty_register(reg(haven::labelled(c(99998, 99999))),
                                      reg(wide[1:2])))
  expect_false(same_certainty_register(reg(haven::labelled(c(99998, 1))),
                                       reg(wide[1:2])))
  expect_false(same_certainty_register(reg(c(NA, 2.5)), reg(i64(c(NA, NA)))))
  expect_true(same_certainty_register(reg(c(NA, 2)), reg(i64(c(NA, 2L)))))
})

test_that("a stage 2 listing whose PSU id is declared missing is refused", {
  skip_if_not_installed("haven")
  # PSU 100000 is certain, so stage 1 selects it, and the listing declares
  # its id missing. haven's labelled_spss keeps the value it declares
  # missing, so a match on stored values alone would draw its rows.
  ids <- 100000 + 0:15
  types <- list(
    double = function(x) x,
    integer = as.integer,
    label = function(x) format(x, scientific = FALSE, trim = TRUE)
  )
  if (requireNamespace("bit64", quietly = TRUE)) {
    types$integer64 <- function(x) i64(as.integer(x))
  }
  declare <- list(
    values = function(x) haven::labelled_spss(x, na_values = x[1]),
    range = function(x) {
      haven::labelled_spss(x, na_range = c(99999.5, 100000.5))
    }
  )
  reached <- character(0)
  for (type in names(types)) {
    register <- certainty_plan_register()
    register$psu_id <- types[[type]](ids)
    plan <- suppressWarnings(certainty_plan_fixture(psu = register))
    expect_true(plan$psu$certainty[1], label = type)
    design <- certainty_bridge_design(plan)
    psu <- register
    psu$psu_id <- if (type == "label") register$psu_id else as.numeric(ids)
    listing <- certainty_element_frame(register)
    stored <- if (type == "label") listing$psu_id else
      as.numeric(as.character(listing$psu_id))
    routes <- function(l) {
      list(
        combined = function() execute(design, list(psu, l), seed = 1),
        continuation = function() {
          execute(execute(design, psu, stages = 1, seed = 1), l, seed = 1)
        }
      )
    }
    # Without a declared code, the labelled listing draws the PSU's rows.
    control <- listing
    control$psu_id <- haven::labelled_spss(stored)
    for (route in routes(control)) {
      s <- suppressWarnings(route())
      expect_equal(sum(vctrs::vec_data(s$psu_id) == stored[1]), 26,
                   label = type)
    }
    kinds <- if (type == "label") "values" else names(declare)
    for (kind in kinds) {
      lost <- listing
      lost$psu_id <- declare[[kind]](stored)
      for (route in names(routes(lost))) {
        label <- paste(type, kind, route)
        err <- expect_error(suppressWarnings(routes(lost)[[route]]()),
                            class = "samplyr_error_certainty_register_mismatch",
                            label = label)
        expect_match(cli::ansi_strip(conditionMessage(err)),
                     "is missing in 600 rows of the frame", fixed = TRUE,
                     label = label)
        reached <- c(reached, label)
      }
    }
  }
  expect_length(reached, 2 * (2 * (length(types) - 1) + 1))

  # The lookup: a declared code names no PSU, on either side.
  position <- function(ids, table) {
    register_id_positions(ids, table, "psu_id")
  }
  spss <- haven::labelled_spss(c(1, 2, 3), na_values = 2)
  expect_identical(position(spss, c(3, 2, 1)), c(3L, NA, 1L))
  expect_identical(position(c(1, 2, 3), spss), c(1L, NA, 3L))
  expect_identical(position(c(NA, 1), c(NA, 1)), c(NA, 2L))

  # A labelled id the register lacks is named by its value, never as
  # 1e+06.
  register <- certainty_plan_register()
  register$psu_id <- ids
  plan <- suppressWarnings(certainty_plan_fixture(psu = register))
  listing <- certainty_element_frame(register)
  listing$psu_id[1] <- 1e6
  listing$psu_id <- haven::labelled(listing$psu_id)
  err <- expect_error(
    suppressWarnings(execute(certainty_bridge_design(plan),
                             list(register, listing), seed = 1)),
    class = "samplyr_error_certainty_register_mismatch"
  )
  expect_match(cli::ansi_strip(conditionMessage(err)),
               "Frame PSU \"1000000\" is not in the register", fixed = TRUE)
})

test_that("a design file refuses 64-bit ids it could not read back", {
  skip_if_not_installed("bit64")
  stage1_of <- function(register) {
    plan <- suppressWarnings(certainty_plan_fixture(psu = register))
    list(plan = plan, design = sampling_design() |>
      add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
      draw(n = plan, method = "pps_brewer", mos = N))
  }
  path <- withr::local_tempfile(fileext = ".json")
  # Beyond the range on either side, including ids a double holds exactly,
  # such as 2^53 + 2, the register is refused and no file is written.
  beyond <- list(
    rounding = large_id_register(),
    exact = large_id_register(from = "9007199254740994", step = 2L),
    negative = large_id_register(from = "-9007199254740993", step = -1L)
  )
  for (kind in names(beyond)) {
    err <- expect_error(write_design(stage1_of(beyond[[kind]])$design, path),
                        class = "samplyr_error_serialize_unsupported",
                        label = kind)
    msg <- cli::ansi_strip(conditionMessage(err))
    expect_match(msg, "only from -2^53 to 2^53", fixed = TRUE, label = kind)
    expect_match(msg, "character labels", fixed = TRUE, label = kind)
    expect_match(msg, "saveRDS()", fixed = TRUE, label = kind)
    expect_false(file.exists(path), label = kind)
  }
  # Both ends of the range are written and read back.
  ends <- list(
    top = large_id_register(from = "9007199254740992", step = -1L),
    bottom = large_id_register(from = "-9007199254740992", step = 1L)
  )
  for (kind in names(ends)) {
    written <- withr::local_tempfile(fileext = ".json")
    end <- stage1_of(ends[[kind]])
    write_design(end$design, written)
    read <- read_design(written)
    expect_no_error(read |> add_stage() |> draw(n = end$plan),
                    message = kind)
  }
  # The same ids as character labels are written, read and replayed.
  labels <- large_id_register()
  labels$psu_id <- as.character(labels$psu_id)
  text <- stage1_of(labels)
  write_design(text$design, path)
  frame <- certainty_element_frame(labels)
  replayed <- suppressWarnings(execute(
    read_design(path) |> add_stage() |> draw(n = text$plan), frame, seed = 2
  ))
  live <- suppressWarnings(execute(
    text$design |> add_stage() |> draw(n = text$plan), frame, seed = 2
  ))
  expect_identical(replayed$.sample_id, live$.sample_id)
  expect_identical(replayed$psu_id, live$psu_id)
  expect_equal(replayed$.weight, live$.weight)
  unlink(path)
  # Below 2^53 the ids read back as integers, which the live plan matches.
  small <- stage1_of(large_id_register(from = "100000"))
  write_design(small$design, path)
  read <- read_design(path)
  expect_no_error(read |> add_stage() |> draw(n = small$plan))
  frame <- certainty_element_frame(large_id_register(from = "100000"))
  expect_identical(
    suppressWarnings(execute(read |> add_stage() |> draw(n = small$plan),
                             frame, seed = 2))$.sample_id,
    suppressWarnings(execute(small$design |> add_stage() |>
                               draw(n = small$plan), frame, seed = 2))$.sample_id
  )
})

test_that("a numeric stratum matches the register under any scipen", {
  # svyplan writes the register's numeric strata as text under the session's
  # scipen, so a design written under one setting reads "1e+05" where the
  # frame holds 100000.
  register <- certainty_plan_register()
  register$stratum <- ifelse(register$stratum == "A", 1e5, 2e5)
  plan <- withr::with_options(list(scipen = 0), svyplan::n_alloc_joint(
    data.frame(stratum = c(1e5, 2e5), N = c(2000, 1600), take_2 = 10),
    measures = data.frame(stratum = c(1e5, 2e5), name = "y",
                          p = c(0.5, 0.4), icc_1 = 0.05),
    targets = data.frame(name = "y", cv = 0.10), psu = register
  ))
  design <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |> draw(n = plan)
  stored <- design$stages[[1]]$draw_spec$certainty_plan$register$stratum
  expect_true(all(grepl("e+05", stored, fixed = TRUE)))
  path <- withr::local_tempfile(fileext = ".json")
  withr::with_options(list(scipen = 0), write_design(design, path))
  elements <- certainty_element_frame(register)
  drawn <- lapply(c(0, 100), function(scipen) {
    withr::with_options(list(scipen = scipen), suppressWarnings(
      execute(read_design(path), list(register, elements), seed = 1)
    ))
  })
  expect_identical(drawn[[2]]$psu_id, drawn[[1]]$psu_id)
  planned <- design$stages[[1]]$draw_spec$certainty_plan$register
  certain <- planned$psu_id[planned$certainty]
  expect_gt(length(certain), 0)
  for (s in drawn) {
    expect_setequal(unique(s$psu_id[s$.certainty_1]), certain)
  }
})
