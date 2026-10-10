# Coverage for cluster_by() input guards.

test_that("cluster_by requires a sampling_design", {
  expect_error(cluster_by(list(), ea), "sampling_design")
})

test_that("cluster_by requires at least one variable", {
  design <- sampling_design()
  expect_error(cluster_by(design), "At least one clustering variable")
})

test_that("cluster_by rejects expressions and tidy-select helpers", {
  design <- sampling_design()
  expect_error(cluster_by(design, starts_with("ea")), "bare column names")
})

test_that("cluster_by rejects a second clustering on the same stage", {
  design <- sampling_design() |>
    cluster_by(cluster)
  expect_error(
    cluster_by(design, stratum),
    "Clustering already defined"
  )
})

## Cluster ids numbered within strata

nested_ids_frame <- function() {
  frame <- expand.grid(
    hh = 1:5, town = 1:4, county = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  frame$block <- (frame$hh - 1L) %/% 3L + 1L
  frame$nbr <- frame$block
  frame
}

test_that("cluster ids numbered within strata are read within them", {
  frame <- nested_ids_frame()
  nested <- sampling_design() |>
    add_stage("Town") |>
    stratify_by(county) |>
    cluster_by(town) |>
    draw(n = 2) |>
    add_stage("Household") |>
    draw(n = 2)
  explicit <- sampling_design() |>
    add_stage("Town") |>
    stratify_by(county) |>
    cluster_by(county, town) |>
    draw(n = 2) |>
    add_stage("Household") |>
    draw(n = 2)

  s <- execute(nested, frame, seed = 1)
  expect_identical(
    as.vector(table(unique(s[c("county", "town")])$county)),
    c(2L, 2L, 2L)
  )
  expect_identical(nrow(s), 12L)
  ref <- execute(explicit, frame, seed = 1)
  expect_identical(
    as.data.frame(s)[c("county", "town", "hh", ".weight")],
    as.data.frame(ref)[c("county", "town", "hh", ".weight")]
  )
  expect_identical(get_design(s)$stages[[1]]$clusters$vars, c("county", "town"))
  expect_identical(get_design(s)$stages[[1]]$clusters$within, "county")
  expect_true(validate_frame(nested, frame))
})

test_that("nest = FALSE refuses ids repeated across strata", {
  frame <- nested_ids_frame()
  design <- sampling_design() |>
    add_stage("Town") |>
    stratify_by(county) |>
    cluster_by(town, nest = FALSE) |>
    draw(n = 2) |>
    add_stage("Household") |>
    draw(n = 2)

  calls <- list(
    function() execute(design, frame, seed = 1),
    function() execute(design, frame, seed = 1, stages = 1),
    function() validate_frame(design, frame)
  )
  for (f in calls) {
    cnd <- expect_error(f(), class = "samplyr_error_frame_cluster_invariant")
    msg <- cli::ansi_strip(conditionMessage(cnd))
    expect_match(msg, "Cluster ids in town repeat across strata of county", fixed = TRUE)
    expect_match(msg, "declares them unique with `nest = FALSE`", fixed = TRUE)
    expect_match(msg, "cluster \"1\" would join rows from 3 strata", fixed = TRUE)
    expect_match(msg, "drop `nest = FALSE`", fixed = TRUE)
  }
})

test_that("later-stage cluster ids are judged within their parent", {
  frame <- nested_ids_frame()
  later <- function(nest) {
    sampling_design() |>
      add_stage("Town") |>
      stratify_by(county) |>
      cluster_by(town) |>
      draw(n = 2) |>
      add_stage("Block") |>
      stratify_by(nbr) |>
      cluster_by(block, nest = nest) |>
      draw(n = 1)
  }
  # Block ids repeat across towns, and each block lies in one stratum.
  expect_identical(nrow(execute(later(FALSE), frame, seed = 1)), 30L)

  # Block ids restart in every stratum of the stage.
  frame$block <- (frame$hh - 1L) %% 2L + 1L
  frame$nbr <- (frame$hh - 1L) %/% 2L + 1L
  s <- execute(later(TRUE), frame, seed = 1)
  expect_identical(
    unique(as.data.frame(s)[c("county", "town", "nbr")]) |> nrow(),
    18L
  )
  expect_identical(
    get_design(s)$stages[[2]]$clusters$vars, c("nbr", "block")
  )
  cnd <- expect_error(
    execute(later(FALSE), frame, seed = 1),
    class = "samplyr_error_frame_cluster_invariant"
  )
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_match(msg, "Cluster ids in block repeat across strata of nbr", fixed = TRUE)
  expect_match(msg, "cluster \"A/1/1\" would join rows from 3 strata", fixed = TRUE)
})

## Cluster checks judge the rows a stage can reach

reach_design <- function(stage2_mos = FALSE) {
  d <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage()
  if (stage2_mos) {
    d |> cluster_by(blk) |> draw(n = 1, method = "pps_brewer", mos = m)
  } else {
    d |> stratify_by(nbr) |> cluster_by(blk, nest = FALSE) |> draw(n = 1)
  }
}

reach_listing <- function() {
  data.frame(
    psu = rep(1:6, each = 6),
    nbr = rep(c(1, 1, 1, 2, 2, 2), 6),
    blk = rep(1:6, 6),
    m = 5
  )
}

test_that("rows under a parent missing from the stage above are not judged", {
  psus <- data.frame(psu = 1:6)
  # Block 1 of PSU 99 sits in two strata, and has two sizes.
  stray <- data.frame(psu = 99L, nbr = c(1, 2), blk = 1L, m = c(1, 2))
  register <- rbind(reach_listing(), stray)

  for (design in list(reach_design(), reach_design(stage2_mos = TRUE))) {
    expect_true(validate_frame(design, list(psus, register)))
    s <- suppressMessages(execute(design, list(psus, register), seed = 1))
    clean <- suppressMessages(
      execute(design, list(psus, reach_listing()), seed = 1)
    )
    expect_identical(s[c("psu", "blk")], clean[c("psu", "blk")])
  }
})

test_that("a continuation judges only the parents it selected", {
  listing <- reach_listing()
  design <- reach_design()
  s1 <- execute(design, listing, seed = 1, stages = 1)
  picked <- sort(unique(s1$psu))
  other <- setdiff(1:6, picked)[1]

  # Under a PSU the sample did not select, the rows cannot be reached.
  unreached <- rbind(listing, transform(listing[listing$psu == other &
                                                  listing$blk == 1, ], nbr = 2))
  expect_true(suppressMessages(validate_frame(s1, unreached)))
  expect_identical(
    as.data.frame(suppressMessages(execute(s1, unreached, seed = 2)))[c("psu", "blk")],
    as.data.frame(suppressMessages(execute(s1, listing, seed = 2)))[c("psu", "blk")]
  )

  # Under a selected PSU they can.
  reached <- rbind(listing, transform(listing[listing$psu == picked[1] &
                                                listing$blk == 1, ], nbr = 2))
  cnd <- expect_error(execute(s1, reached, seed = 2),
                      class = "samplyr_error_frame_cluster_invariant")
  expect_match(cli::ansi_strip(conditionMessage(cnd)),
               "drop `nest = FALSE`", fixed = TRUE)
  expect_error(suppressMessages(validate_frame(s1, reached)),
               class = "samplyr_error_frame_cluster_invariant")
})

test_that("a later-stage cluster defect is refused whatever the seed", {
  listing <- reach_listing()
  spanning <- rbind(listing, transform(listing[listing$psu == 4 &
                                                 listing$blk == 1, ], nbr = 2))
  varying <- rbind(listing, transform(listing[listing$psu == 4 &
                                                listing$blk == 1, ], m = 9))
  cases <- list(
    list(design = reach_design(), frame = spanning),
    list(design = reach_design(stage2_mos = TRUE), frame = varying)
  )
  for (case in cases) {
    refused <- vapply(1:12, function(seed) {
      inherits(
        tryCatch(suppressMessages(execute(case$design, case$frame, seed = seed)),
                 error = identity),
        "samplyr_error_frame_cluster_invariant"
      )
    }, logical(1))
    expect_true(all(refused))
  }
})

## Nesting cluster ids within strata

# Towns 1 and 2 in strata A and B, of sizes 4, 6, 8 and 10.
two_by_two_frame <- function() {
  frame <- data.frame(
    st = rep(c("A", "A", "B", "B"), c(4, 6, 8, 10)),
    town = rep(c(1, 2, 1, 2), c(4, 6, 8, 10))
  )
  frame$size <- stats::ave(frame$town, frame$st, frame$town, FUN = length)
  frame$el <- stats::ave(frame$town, frame$st, frame$town, FUN = seq_along)
  frame$gid <- paste(frame$st, frame$town)
  frame
}

test_that("nested towns get the probabilities of a hand computation", {
  frame <- two_by_two_frame()
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(st) |>
    cluster_by(town) |>
    draw(n = 1) |>
    add_stage() |>
    draw(n = 2)
  for (seed in 1:40) {
    s <- as.data.frame(suppressMessages(execute(design, frame, seed = seed)))
    expect_identical(nrow(s), 4L)
    # One town per stratum, two of its own elements, weight 2 * size / 2.
    expect_identical(as.vector(table(unique(s[c("st", "town")])$st)), c(1L, 1L))
    expect_true(all(table(paste(s$st, s$town)) == 2L))
    expect_true(all(s$el <= s$size))
    expect_identical(s$.weight, s$size)
  }

  pps <- sampling_design() |>
    stratify_by(st) |>
    cluster_by(town) |>
    draw(n = 1, method = "pps_brewer", mos = size)
  s <- suppressMessages(execute(pps, frame, seed = 3))
  picked <- unique(as.data.frame(s)[c("st", "town", "size")])
  pik <- picked$size / c(A = 10, B = 18)[picked$st]
  joint <- joint_expectation(s, frame)$stage_1
  expect_equal(diag(joint), unname(pik))
  expect_equal(joint[1, 2], prod(pik))
})

test_that("nesting selects what a global id selects, method by method", {
  frame <- two_by_two_frame()
  frame$prn <- c(0.2, 0.7, 0.4, 0.9)[match(frame$gid, unique(frame$gid))]
  run <- function(clusters, method, ...) {
    d <- sampling_design() |> stratify_by(st)
    d <- do.call(cluster_by, c(list(d), lapply(clusters, as.name)))
    d <- draw(d, method = method, ...)
    set.seed(99)
    s <- suppressWarnings(suppressMessages(execute(d, frame, seed = 5)))
    list(
      rows = as.data.frame(s)[c("st", "town", "el", ".weight")],
      rng = .Random.seed
    )
  }
  cases <- list(
    list(method = "srswor", n = 1),
    list(method = "systematic", n = 1),
    list(method = "pps_systematic", n = 1, mos = "size"),
    list(method = "pps_brewer", n = 1, mos = "size"),
    list(method = "pps_sampford", n = 1, mos = "size"),
    list(method = "pps_cps", n = 1, mos = "size"),
    list(method = "bernoulli", frac = 0.5, on_empty = "silent"),
    list(method = "pps_poisson", n = 1, mos = "size", on_empty = "silent"),
    list(method = "pps_pareto", n = 1, mos = "size", prn = "prn"),
    list(method = "srswr", n = 2),
    list(method = "pps_multinomial", n = 2, mos = "size"),
    list(method = "pps_chromy", n = 2, mos = "size")
  )
  for (case in cases) {
    args <- case[setdiff(names(case), "method")]
    if (!is.null(args$mos)) args$mos <- as.name(args$mos)
    if (!is.null(args$prn)) args$prn <- as.name(args$prn)
    nested <- do.call(run, c(list("town", case$method), args))
    global <- do.call(run, c(list("gid", case$method), args))
    expect_identical(nested$rows, global$rows, label = case$method)
    expect_identical(nested$rng, global$rng, label = case$method)
  }
})

test_that("every entry point reads nested ids like the explicit key", {
  frame <- nested_ids_frame()
  frame$m <- frame$town + 2
  build <- function(clusters) {
    d <- sampling_design() |>
      add_stage("Town") |>
      stratify_by(county)
    d <- do.call(cluster_by, c(list(d), lapply(clusters, as.name)))
    d |>
      draw(n = 2, method = "pps_brewer", mos = m) |>
      add_stage("Household") |>
      draw(n = 2)
  }
  nested <- build("town")
  explicit <- build(c("county", "town"))
  sn <- execute(nested, frame, seed = 4)
  se <- execute(explicit, frame, seed = 4)
  cols <- c("county", "town", "hh", ".weight", ".weight_1", ".fpc_2")
  expect_identical(as.data.frame(sn)[cols], as.data.frame(se)[cols])

  strip <- function(x) {
    x <- as.data.frame(x)
    attributes(x) <- attributes(x)[c("names", "row.names", "class")]
    x
  }
  expect_identical(strip(frame_summary(nested, frame)),
                   strip(frame_summary(explicit, frame)))
  expect_identical(variance_estimators(nested, frame),
                   variance_estimators(explicit, frame))
  expect_identical(joint_expectation(sn, frame), joint_expectation(se, frame))
  expect_true(validate_frame(nested, frame))
  frame$key <- seq_len(nrow(frame))
  expect_identical(
    strip(exante_probabilities(nested, frame, key = key)),
    strip(exante_probabilities(explicit, frame, key = key))
  )

  partial <- execute(nested, frame, seed = 4, stages = 1)
  expect_identical(as.data.frame(execute(partial, frame, seed = 4))[cols],
                   as.data.frame(execute(
                     execute(explicit, frame, seed = 4, stages = 1),
                     frame, seed = 4
                   ))[cols])

  skip_if_not_installed("survey")
  withr::local_options(survey.lonely.psu = "adjust")
  vn <- survey::svytotal(~m, as_svydesign(sn))
  ve <- survey::svytotal(~m, as_svydesign(se))
  expect_equal(coef(vn), coef(ve))
  expect_equal(survey::SE(vn), survey::SE(ve))
})

test_that("a register for a later stage needs the strata only when ids repeat", {
  frame <- nested_ids_frame()
  towns <- unique(frame[c("county", "town")])
  design <- sampling_design() |>
    add_stage("Town") |>
    stratify_by(county) |>
    cluster_by(town) |>
    draw(n = 2) |>
    add_stage("Household") |>
    draw(n = 2)

  s <- execute(design, list(towns, frame), seed = 1)
  expect_identical(nrow(s), 12L)
  expect_true(all(paste(s$county, s$town) %in% paste(towns$county, towns$town)))

  # Town ids repeat across counties, so households cannot link by town alone.
  cnd <- expect_error(
    execute(design, list(towns, frame[setdiff(names(frame), "county")]), seed = 1),
    class = "samplyr_error_frame_missing_ancestry"
  )
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "is missing county",
               fixed = TRUE)

  # Unique town ids keep linking by the town id alone.
  frame$town <- paste0(frame$county, frame$town)
  towns <- unique(frame[c("county", "town")])
  s <- execute(design, list(towns, frame[setdiff(names(frame), "county")]),
               seed = 1)
  expect_identical(get_design(s)$stages[[1]]$clusters$vars, "town")
  expect_identical(nrow(s), 12L)
})

test_that("delimiters in values do not merge nested units", {
  frame <- data.frame(
    st = rep(c("A/1", "A", "A/1", "A"), each = 3),
    town = rep(c("2", "1/2", "9", "8"), each = 3)
  )
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(st) |>
    cluster_by(town) |>
    draw(n = 2) |>
    add_stage() |>
    draw(n = 1)
  s <- suppressMessages(execute(design, frame, seed = 1))
  expect_identical(nrow(unique(as.data.frame(s)[c("st", "town")])), 4L)
  expect_identical(get_design(s), design)
})

test_that("nest is a single TRUE or FALSE and has no effect without strata", {
  base <- sampling_design()
  for (bad in list(NA, c(TRUE, TRUE), "yes", 1L, NULL)) {
    expect_error(cluster_by(base, ea_id, nest = bad),
                 class = "samplyr_error_cluster_argument")
  }
  cnd <- expect_error(cluster_by(base, ea_id, Nest = TRUE),
                      class = "samplyr_error_unknown_argument")
  expect_match(cli::ansi_strip(conditionMessage(cnd)), "Did you mean `nest`?",
               fixed = TRUE)
  expect_error(cluster_by(base, nets = ea_id),
               class = "samplyr_error_unknown_argument")

  # A column called nest is still a clustering variable.
  frame <- data.frame(nest = rep(1:4, each = 2), y = 1:8)
  s <- execute(sampling_design() |> cluster_by(nest) |> draw(n = 2), frame,
               seed = 1)
  expect_identical(nrow(s), 4L)

  unstratified <- function(nest) {
    sampling_design() |> cluster_by(ea_id, nest = nest) |> draw(n = 3)
  }
  expect_identical(
    as.data.frame(execute(unstratified(TRUE), zwe_eas, seed = 2))$ea_id,
    as.data.frame(execute(unstratified(FALSE), zwe_eas, seed = 2))$ea_id
  )
})

test_that("nesting is resolved at execution and written as declared", {
  frame <- nested_ids_frame()
  design <- sampling_design() |>
    stratify_by(county) |>
    cluster_by(town) |>
    draw(n = 2)
  expect_identical(design$stages[[1]]$clusters$vars, "town")
  expect_null(design$stages[[1]]$clusters$within)

  s <- execute(design, frame, seed = 1)
  printed <- cli::ansi_strip(capture.output(print(get_design(s))))
  expect_true(any(grepl("Cluster: town (within county)", printed, fixed = TRUE)))
  printed <- cli::ansi_strip(capture.output(print(design)))
  expect_true(any(grepl("Cluster: town$", printed)))

  # The file carries the declaration, not a resolution of one frame.
  path <- withr::local_tempfile(fileext = ".json")
  write_design(get_design(s), path)
  restored <- read_design(path)
  expect_identical(restored$stages[[1]]$clusters$vars, "town")
  expect_true(restored$stages[[1]]$clusters$nest)
  expect_false(grepl("\"nest\"", paste(readLines(path), collapse = "")))
  expect_identical(
    as.data.frame(replay_design(s, frame))[c("county", "town", "hh")],
    as.data.frame(s)[c("county", "town", "hh")]
  )

  opted_out <- sampling_design() |>
    stratify_by(county) |>
    cluster_by(town, nest = FALSE) |>
    draw(n = 2)
  write_design(opted_out, path)
  expect_true(grepl("\"nest\": false", paste(readLines(path), collapse = "")))
  expect_false(read_design(path)$stages[[1]]$clusters$nest)
})

test_that("only rows a stage can reach decide whether its key gains strata", {
  psus <- data.frame(psu = 1:4)
  blocks <- data.frame(
    psu = rep(1:4, each = 4),
    st = rep(c("a", "a", "b", "b"), 4),
    blk = rep(1:4, 4)
  )
  homes <- merge(blocks, data.frame(hh = 1:3))[c("psu", "blk", "hh")]
  design <- function(nest) {
    sampling_design() |>
      add_stage() |>
      cluster_by(psu) |>
      draw(n = 2) |>
      add_stage() |>
      stratify_by(st) |>
      cluster_by(blk, nest = nest) |>
      draw(n = 1) |>
      add_stage() |>
      draw(n = 2)
  }
  # Block 1 of a PSU the first register does not hold sits in both strata.
  stray <- data.frame(psu = 99L, st = c("a", "b"), blk = 1L)
  frames <- list(psus, rbind(blocks, stray), homes)
  clean <- list(psus, blocks, homes)

  s <- suppressMessages(execute(design(TRUE), frames, seed = 1))
  expect_identical(get_design(s), design(TRUE))
  expect_identical(
    as.data.frame(s)[c("psu", "blk", "hh")],
    as.data.frame(suppressMessages(execute(design(TRUE), clean, seed = 1)))[
      c("psu", "blk", "hh")
    ]
  )
  expect_true(validate_frame(design(TRUE), frames))
  expect_no_error(frame_summary(design(TRUE), frames))
  partial <- execute(design(TRUE), psus, seed = 1, stages = 1)
  expect_no_error(suppressMessages(execute(partial, frames[2:3], seed = 1)))

  # The same rows decide the refusal under nest = FALSE.
  expect_no_error(suppressMessages(execute(design(FALSE), frames, seed = 1)))

  # Under a PSU the first register holds, the repeat is reached: the key
  # gains st, the household register must carry it, and nest = FALSE refuses.
  reached <- rbind(blocks, data.frame(psu = 2L, st = "b", blk = 1L))
  homes_st <- merge(reached, data.frame(hh = 1:3))
  s <- suppressMessages(
    execute(design(TRUE), list(psus, reached, homes_st), seed = 1)
  )
  expect_identical(get_design(s)$stages[[2]]$clusters$vars, c("st", "blk"))
  expect_error(
    execute(design(TRUE), list(psus, reached, homes), seed = 1),
    class = "samplyr_error_frame_missing_ancestry"
  )
  expect_error(
    execute(design(FALSE), list(psus, reached, homes_st), seed = 1),
    class = "samplyr_error_frame_cluster_invariant"
  )
})
