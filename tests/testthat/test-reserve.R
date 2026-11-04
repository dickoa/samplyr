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
    conditionMessage(w)
  }
  net <- svyplan::n_prop(p = 0.3, moe = 0.1)
  expect_no_warning(sampling_design() |> draw(n = net, reserve = 3))

  gross <- svyplan::n_prop(p = 0.3, moe = 0.1, resp_rate = 0.8)
  expect_no_warning(sampling_design() |> draw(n = gross))
  msg <- plan_message(sampling_design() |> draw(n = gross, reserve = 3))
  expect_match(msg, "resp_rate = 0.8", fixed = TRUE)
  expect_match(msg, "response rate of persons", fixed = TRUE)

  # svyplan stores eligibility_rate in params only when it is below 1.
  net$params$eligibility_rate <- 0.9
  msg <- plan_message(sampling_design() |> draw(n = net, reserve = 3))
  expect_match(msg, "eligibility_rate = 0.9", fixed = TRUE)
  expect_no_match(msg, "resp_rate|persons")

  # A take of PSUs is inflated by the PSU rate only.
  cluster <- svyplan::n_cluster(
    stage_cost = c(500, 50), icc = 0.05, cv = 0.05,
    resp_rate = 0.8, resp_rate_psu = 0.9
  )
  msg <- plan_message(
    sampling_design() |> add_stage() |> cluster_by(ea_id) |>
      draw(n = cluster, reserve = 3)
  )
  expect_match(msg, "resp_rate_psu = 0.9", fixed = TRUE)
  expect_no_match(msg, "resp_rate = 0.8", fixed = TRUE)
  msg <- plan_message(
    sampling_design() |> add_stage() |> cluster_by(ea_id) |> draw(n = 3) |>
      add_stage() |> draw(n = cluster, reserve = 3)
  )
  expect_match(msg, "resp_rate = 0.8", fixed = TRUE)
  expect_no_match(msg, "resp_rate_psu", fixed = TRUE)
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

## Exporting a settled reserve

export_codes <- function(pool_3) {
  codes <- settle_codes(3)
  codes[["3"]] <- pool_3
  codes
}

settled_sample <- function(pool_3 = list(main = c(1, 4, 1), reserve = 1)) {
  listed <- listed_sample()
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
  svy <- as_svydesign(settled)
  expect_identical(nrow(svy$variables), nrow(settled))
  expect_equal(unname(weights(svy)), settled$.weight)
  # Stage-1 clusters only, drawn with replacement.
  expect_identical(deparse(svy$call$ids), "~.id_1")
  expect_null(svy$fpc$popsize)
  rep <- as_svrepdesign(settled, type = "JKn")
  expect_s3_class(rep, "svyrep.design")
  expect_equal(unname(weights(rep, type = "sampling")), settled$.weight)
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

# 12 EAs of 10 households, and two panels active in turn over 4 waves.
panel_eas <- function() data.frame(ea = 1:12)
panel_listing <- function() expand.grid(hh = 1:10, ea = 1:12)
alternating <- function(tenures) {
  schedule <- expand.grid(panel = 1:2, wave = 1:4)
  schedule$active <- schedule$panel == (schedule$wave - 1) %% 2 + 1
  if (tenures) {
    schedule$visit <- ifelse(schedule$active, 1L, NA_integer_)
  }
  schedule
}

test_that("a schedule with several tenures per panel takes no reserve", {
  below <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 8) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2, reserve = 2)
  expect_error(
    execute(below, panel_eas(), stages = 1, panels = alternating(TRUE),
            seed = 1),
    "Stage 2 draws a `reserve`",
    class = "samplyr_error_tenure_stages"
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
    class = "samplyr_error_tenure_stages"
  )
  # One tenure per panel completes, so the master settles.
  master <- execute(below, panel_eas(), stages = 1,
                    panels = alternating(FALSE), seed = 1)
  for (t in 1:2) {
    master <- execute(master, panel_listing(), wave = t, seed = t + 1)
  }
  expect_identical(get_stages_executed(master), 1:2)
})

test_that("a wave of a master with a reserve is refused, settled or not", {
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
    wave <- execute(source, wave = 1)
    expect_error(
      as_svydesign(wave),
      "Stage 2 lists a take and a reserve",
      class = "samplyr_error_reserve_full_export"
    )
    expect_error(as_svrepdesign(wave),
                 class = "samplyr_error_reserve_full_export")
    expect_error(srvyr::as_survey_design(wave),
                 class = "samplyr_error_reserve_full_export")
    expect_error(srvyr::as_survey_rep(wave),
                 class = "samplyr_error_reserve_full_export")
  }
})

test_that("a pool with no respondent is exported as a placeholder row", {
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
  expect_identical(data$ea_id[held], 3L)
  expect_identical(data$region[held], "north")
  # The stage-1 weight of 1 times 2 eligible among 4 resolved units of 10.
  expect_equal(data$.weight[held], 10 * 2 / 4)
  expect_true(is.na(data$hh_id[held]))
  expect_identical(nrow(data), nrow(settled) + 1L)
  # The row is a primary unit of its own, in its stratum.
  expect_identical(sum(svy$cluster[[1]] == svy$cluster[[1]][held]), 1L)

  expect_warning(
    rep <- as_svrepdesign(settled, type = "JKn"),
    class = "samplyr_warning_reserve_no_respondent"
  )
  expect_identical(nrow(rep$variables), nrow(data))
})

test_that("the placeholder carries the non-interview code in the column's type", {
  skip_if_not_installed("survey")
  placeholder_code <- function(as) {
    listed <- listed_sample()
    listed$code <- as(set_codes(listed, export_codes(no_respondent_codes)))
    settled <- execute(listed, response_status = code)
    data <- suppressWarnings(as_svydesign(settled))$variables
    data$code[is.na(data$.sample_id) & data$ea_id == 3L]
  }
  expect_identical(placeholder_code(identity), 2)
  expect_identical(placeholder_code(code_words), "noninterview")
  held <- placeholder_code(function(code) factor(code_words(code)))
  expect_identical(as.character(held), "noninterview")
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

  # Before the next stage, the stratum is exported as a placeholder, its
  # only primary unit.
  expect_warning(
    expect_warning(
      svy <- as_svydesign(settled),
      class = "samplyr_warning_lonely_psu"
    ),
    "region = north",
    class = "samplyr_warning_reserve_no_respondent"
  )
  held <- is.na(svy$variables$.sample_id)
  expect_identical(svy$variables$region[held], "north")
  # All three north EAs were worked and found eligible.
  expect_equal(svy$variables$.weight[held], 3)

  full <- execute(settled, frame, seed = 2)
  expect_error(
    as_svydesign(full),
    "region = north",
    class = "samplyr_error_reserve_no_respondent"
  )
})

test_that("svrep moves a placeholder's weight to the respondents of its stratum", {
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
  expect_equal(unname(after[held]), 0)
  north <- data$region == "north"
  expect_equal(sum(after[north]), sum(before[north]))
  respondent <- north & data$code == 1
  expect_equal(
    sum(after[respondent]),
    sum(before[respondent]) + unname(before[held])
  )

  # The placeholder keeps its row, with NA in the analysis variables.
  expect_true(is.na(data$hh_id[held]))
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

test_that("a primary unit found wholly ineligible stays as a zero row", {
  skip_if_not_installed("survey")
  settled <- settled_sample(list(main = c(4, 4, 4), reserve = c(4, 4, 4)))
  expect_false(3L %in% settled$ea_id)
  svy <- as_svydesign(settled)
  zero <- svy$variables[is.na(svy$variables$.sample_id), ]
  expect_identical(zero$ea_id, 3L)
  expect_identical(zero$code, 4)
  # Its 10 households, at the stage-1 weight of 1.
  expect_equal(zero$.weight, 10)
  expect_identical(nrow(svy$variables), nrow(settled) + 1L)
})

test_that("a pool estimated to hold no eligible unit is exported as ineligible", {
  skip_if_not_installed("survey")
  # EA 3 resolved only ineligible households, and the rest are unknown.
  settled <- settled_sample(list(main = c(3, 4, 4), reserve = c(3, 4, 3)))
  pools <- attr(settled, "metadata")$reserve_settlement$pools
  expect_equal(pools$eligible[3], 0)
  expect_identical(pools$no_respondent, rep(FALSE, 5))
  expect_no_warning(svy <- as_svydesign(settled))
  zero <- svy$variables[is.na(svy$variables$.sample_id), ]
  expect_identical(zero$ea_id, 3L)
  expect_identical(zero$code, 4)
  expect_equal(zero$.weight, 10)

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
  # at a first-stage reserve. Every other EA is a cluster of the stratum.
  reference <- function(svy) {
    df <- svy$variables
    added <- is.na(df$.sample_id)
    sure <- df$psu == 1L
    df$unit <- ifelse(sure, paste0("hh", df$hh), paste0("ea", df$psu))
    df$unit[added] <- paste0("row", which(added))
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
  # The other EAs keep their own rows only.
  expect_identical(nrow(data), nrow(settled) + 2L)
  expect_same_variance(svy)

  # A pool with no respondent is a placeholder, a cluster of its own.
  settled <- settle(list(main = c(2, 4, 2), reserve = c(2, 4, 2)))
  expect_warning(
    svy <- as_svydesign(settled),
    class = "samplyr_warning_reserve_no_respondent"
  )
  data <- svy$variables
  sure <- data$psu == 1L
  expect_identical(sort(data$code[sure]), c(2, 4, 4))
  expect_equal(sum(data$.weight[sure]), 8)
  expect_same_variance(svy)
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
    frame <- settle_frame()
    names(frame)[1:2] <- c("eligible", "n_respondents")
    design <- sampling_design() |>
      add_stage() |>
      stratify_by(eligible) |>
      cluster_by(n_respondents) |>
      draw(n = c(north = 3, south = 2)) |>
      add_stage() |>
      draw(n = 3, reserve = 3)
    listed <- execute(design, frame, seed = 4) |> suppressWarnings()
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

test_that("a draw of a parent found wholly ineligible stays as a zero row", {
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
  zero <- svy$variables[is.na(svy$variables$.sample_id), ]
  expect_identical(zero$.draw_1, 3L)
  expect_identical(zero$ea, 1L)
  expect_equal(zero$.weight, 0.5 * 10)
  expect_identical(length(unique(svy$cluster[[1]])), 4L)
})

test_that("a placeholder of one draw is a unit apart from the other draws", {
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
  # The placeholder warns, and the stratum is not lonely: it holds two draws.
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
  expect_identical(svy$variables$.draw_1[held], 2L)
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
