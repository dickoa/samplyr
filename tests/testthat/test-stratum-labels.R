## Labels that name a numeric stratum

# as.character() writes the stratum 100000 as "1e+05" under the default scipen
# and as "100000" under a large one. svyplan names its sizes that way, and so
# do table() and setNames(x, unique(col)). Either spelling names the stratum,
# under any scipen, through every argument that takes stratum labels. The
# expected takes are written by hand.

sl_frame <- function(values = c(1e5, 2e5)) {
  data.frame(st = rep(values, each = 10L), id = 1:20)
}

sl_take <- function(design, frame, seed = 1) {
  s <- execute(design, frame, seed = seed)
  as.vector(table(factor(
    as.character(vctrs::vec_data(s$st)),
    levels = as.character(vctrs::vec_data(unique(frame$st)))
  )))
}

sl_routes <- function(labels) {
  list(
    "named n" = sampling_design() |> stratify_by(st) |>
      draw(n = stats::setNames(c(2L, 3L), labels)),
    "named frac" = sampling_design() |> stratify_by(st) |>
      draw(frac = stats::setNames(c(0.2, 0.3), labels)),
    "table n" = sampling_design() |> stratify_by(st) |>
      draw(n = data.frame(st = labels, n = c(2L, 3L))),
    "named variance" = sampling_design() |>
      stratify_by(st, alloc = "neyman",
                  variance = stats::setNames(c(1, 4), labels)) |>
      draw(n = 6)
  )
}

sl_expected <- list(
  "named n" = c(2L, 3L), "named frac" = c(2L, 3L), "table n" = c(2L, 3L),
  # Neyman takes 6 in proportion to N_h S_h, here 10 x 1 and 10 x 2.
  "named variance" = c(2L, 4L)
)

test_that("either spelling of a numeric stratum names it, under any scipen", {
  checked <- character(0)
  spellings <- list(scientific = c("1e+05", "2e+05"),
                    fixed = c("100000", "200000"))
  for (scipen in c(0, 100)) {
    withr::local_options(scipen = scipen)
    for (spelling in names(spellings)) {
      routes <- sl_routes(spellings[[spelling]])
      for (route in names(routes)) {
        info <- paste(route, spelling, "scipen", scipen)
        for (values in list(c(1e5, 2e5), c(100000L, 200000L))) {
          frame <- sl_frame(values)
          expect_identical(
            sl_take(routes[[route]], frame), sl_expected[[route]],
            info = info
          )
          expect_true(validate_frame(routes[[route]], frame), info = info)
        }
        checked <- c(checked, paste(route, spelling))
      }
    }
  }
  expect_setequal(unique(checked), c(
    "named n scientific", "named n fixed", "named frac scientific",
    "named frac fixed", "table n scientific", "table n fixed",
    "named variance scientific", "named variance fixed"
  ))
})

test_that("a svyplan allocation on numeric strata is drawn as planned", {
  frame <- data.frame(st = rep(c(1e5, 2e5), each = 50L), id = 1:100)
  strata <- data.frame(
    stratum = c(1e5, 2e5), N = c(50, 50), mean = c(10, 10), var = c(1, 4)
  )
  for (scipen in c(0, 100)) {
    withr::local_options(scipen = scipen)
    plan <- svyplan::n_alloc(strata, cv = 0.05)
    design <- sampling_design() |> stratify_by(st) |> draw(n = plan)
    expect_identical(
      sl_take(design, frame), as.integer(plan$detail$n_int),
      info = paste("scipen", scipen)
    )
  }
})

test_that("character and factor strata compare their labels literally", {
  for (as_factor in c(FALSE, TRUE)) {
    values <- c("1e+05", "2e+05")
    frame <- sl_frame(if (as_factor) factor(values) else values)
    ok <- sampling_design() |> stratify_by(st) |>
      draw(n = c("1e+05" = 2L, "2e+05" = 3L))
    expect_identical(sl_take(ok, frame), c(2L, 3L))
    plain <- sampling_design() |> stratify_by(st) |>
      draw(n = c("100000" = 2L, "200000" = 3L))
    expect_error(
      execute(plain, frame, seed = 1),
      class = "samplyr_error_alloc_unknown_strata"
    )
  }
})

test_that("a label that names no stratum keeps its own refusal", {
  frame <- sl_frame()
  # R never writes 1e+5, so it is no spelling of the stratum.
  misspelled <- sampling_design() |> stratify_by(st) |>
    draw(n = c("1e+5" = 2L, "2e+05" = 3L))
  expect_error(
    execute(misspelled, frame, seed = 1),
    class = "samplyr_error_alloc_unknown_strata"
  )
  short <- sampling_design() |> stratify_by(st) |> draw(n = c("1e+05" = 2L))
  expect_error(
    execute(short, frame, seed = 1),
    class = "samplyr_error_alloc_missing_coverage"
  )
  short_aux <- sampling_design() |>
    stratify_by(st, alloc = "neyman", variance = c("1e+05" = 1)) |>
    draw(n = 6)
  expect_error(
    execute(short_aux, frame, seed = 1),
    class = "samplyr_error_aux_missing_coverage"
  )
})

test_that("two spellings of one stratum are refused as one stratum twice", {
  frame <- sl_frame()
  twice <- list(
    n = sampling_design() |> stratify_by(st) |>
      draw(n = c("1e+05" = 2L, "100000" = 3L, "2e+05" = 3L)),
    frac = sampling_design() |> stratify_by(st) |>
      draw(frac = c("1e+05" = 0.2, "100000" = 0.3, "2e+05" = 0.3)),
    table = sampling_design() |> stratify_by(st) |>
      draw(n = data.frame(st = c("1e+05", "100000", "2e+05"), n = 2L))
  )
  for (arg in names(twice)) {
    expect_error(
      execute(twice[[arg]], frame, seed = 1),
      class = "samplyr_error_alloc_duplicate_keys",
      info = arg
    )
  }
  aux <- sampling_design() |>
    stratify_by(st, alloc = "neyman",
                variance = c("1e+05" = 1, "100000" = 2, "2e+05" = 4)) |>
    draw(n = 6)
  expect_error(
    execute(aux, frame, seed = 1),
    class = "samplyr_error_aux_duplicate_keys"
  )
})

test_that("a secondary stage reads labels within each selected parent", {
  frame <- data.frame(
    psu = rep(1:2, each = 20L),
    st = rep(rep(c(1e5, 2e5), each = 10L), 2),
    id = 1:40
  )
  # PSU 2 has no unit of stratum 200000, the one stratum truly absent.
  frame <- frame[!(frame$psu == 2 & frame$st == 2e5), ]
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(st) |> draw(n = c("1e+05" = 1L, "2e+05" = 2L))
  expect_true(suppressMessages(validate_frame(design, frame)))
  absent <- list()
  s <- withCallingHandlers(
    execute(design, frame, seed = 1),
    samplyr_message_absent_strata = function(cnd) {
      absent[[length(absent) + 1L]] <<- cnd
      invokeRestart("muffleMessage")
    },
    samplyr_message_singleton_pool = function(cnd) {
      invokeRestart("muffleMessage")
    }
  )
  expect_length(absent, 1L)
  expect_identical(absent[[1]]$payload$n_absent, 1L)
  counts <- table(s$psu, s$st == 1e5)
  expect_identical(as.vector(counts[, "TRUE"]), c(1L, 1L))
  expect_identical(as.vector(counts[, "FALSE"]), c(2L, 0L))
})

test_that("labels resolve to the key text of the stratum they spell", {
  values <- c(1e5, 0.1, 123456.7, 1e-5, NA)
  expect_identical(
    resolve_stratum_labels(
      c("1e+05", "100000", "0.1", "1e-01", "1.234567e+05", "123456.7",
        "1e-05", "0.00001", "x", NA),
      values
    ),
    c("100000", "100000", "0.1", "0.1", "123456.7", "123456.7",
      "1e-05", "1e-05", "x", NA)
  )
  # A non-numeric column compares literally.
  expect_identical(resolve_stratum_labels("1e+05", "100000"), "1e+05")
  expect_identical(resolve_stratum_labels(factor("a"), 1), "a")
  expect_identical(resolve_stratum_labels(factor("a"), "a"), factor("a"))
  expect_no_warning(resolve_stratum_labels("1e+05", 1e5))
})

## Per-stratum draw settings

# A certainty threshold or a sampling fraction given by stratum names its
# strata by label too, and every route that reads it must find the same
# stratum. Unit 1 of each stratum holds 40 of the stratum's 100, so a
# threshold of 39 (or 0.39) takes it with certainty and weight 1 in stratum
# 100000, and 50 (or 0.50) takes no unit of stratum 200000.

sl_cert_frame <- function(mos = rep(c(40, 20, 20, 20), 2)) {
  data.frame(id = 1:8, st = rep(c(1e5, 2e5), each = 4L), mos = mos)
}

sl_cert_design <- function(labels, threshold, values = c(39, 50)) {
  table <- data.frame(st = labels)
  table[[threshold]] <- if (threshold == "certainty_size") {
    values
  } else {
    values / 100
  }
  args <- list(n = 2, method = "pps_brewer", mos = quote(mos))
  args[[threshold]] <- table
  do.call(draw, c(list(sampling_design() |> stratify_by(st)), args))
}

sl_quiet <- function(expr) suppressWarnings(suppressMessages(expr))

sl_cert_spellings <- list(
  typed = c(1e5, 2e5), scientific = c("1e+05", "2e+05"),
  fixed = c("100000", "200000")
)

test_that("a certainty threshold finds its stratum in either spelling", {
  frame <- sl_cert_frame()
  checked <- character(0)
  for (scipen in c(0, 100)) {
    withr::local_options(scipen = scipen)
    for (threshold in c("certainty_size", "certainty_prop")) {
      control <- sl_cert_design(sl_cert_spellings$typed, threshold)
      expected <- as.data.frame(sl_quiet(execute(control, frame, seed = 1)))
      expect_true(expected$.certainty_1[expected$id == 1])
      expect_identical(expected$.weight[expected$id == 1], 1)
      expected_reps <- as.data.frame(
        sl_quiet(execute(control, frame, seed = 1, reps = 2))
      )
      expected_digest <- sl_quiet(exante_digest(control, frame))
      for (spelling in names(sl_cert_spellings)) {
        info <- paste(threshold, spelling, "scipen", scipen)
        design <- sl_cert_design(sl_cert_spellings[[spelling]], threshold)
        got <- as.data.frame(sl_quiet(execute(design, frame, seed = 1)))
        expect_identical(got$id, expected$id, info = info)
        expect_identical(got$.certainty_1, expected$.certainty_1, info = info)
        expect_identical(got$.weight, expected$.weight, info = info)
        reps <- as.data.frame(
          sl_quiet(execute(design, frame, seed = 1, reps = 2))
        )
        expect_identical(reps$.weight, expected_reps$.weight, info = info)
        expect_identical(
          unclass(sl_quiet(exante_digest(design, frame))),
          unclass(expected_digest), info = info
        )
        checked <- c(checked, paste(threshold, spelling))
      }
    }
  }
  expect_setequal(unique(checked), c(
    "certainty_size typed", "certainty_size scientific",
    "certainty_size fixed", "certainty_prop typed",
    "certainty_prop scientific", "certainty_prop fixed"
  ))
})

test_that("validation and joint expectations read the draw's threshold", {
  # Stratum 100000 holds one unit of positive size. With it taken with
  # certainty, the remaining take of 1 meets only units of size 0. Without
  # the threshold, the take of 2 exceeds the one positive unit.
  short <- sl_cert_frame(mos = c(40, 0, 0, 0, 40, 20, 20, 20))
  for (spelling in names(sl_cert_spellings)) {
    design <- sl_cert_design(sl_cert_spellings[[spelling]], "certainty_size")
    expect_error(
      sl_quiet(validate_frame(design, short)),
      class = "samplyr_error_mos_zero_sum", info = spelling
    )
  }
  # The joint matrix is read from the stored digest, or computed again from
  # a frame, and each route resolves the table itself.
  frame <- sl_cert_frame()
  for (route in c("digest", "frame")) {
    joint <- lapply(sl_cert_spellings, function(labels) {
      design <- sl_cert_design(labels, "certainty_size")
      s <- sl_quiet(execute(design, frame, seed = 1, frame_digest = "full"))
      if (route == "digest") {
        joint_expectation(s)
      } else {
        joint_expectation(s, frame = frame)
      }
    })
    expect_identical(joint$scientific, joint$typed, info = route)
    expect_identical(joint$fixed, joint$typed, info = route)
  }
  # Unit 1 is taken with certainty, so its own entry is 1.
  design <- sl_cert_design(sl_cert_spellings$scientific, "certainty_size")
  s <- sl_quiet(execute(design, frame, seed = 1))
  in_1 <- which(as.data.frame(s)$id == 1L)
  expect_length(in_1, 1L)
  expect_identical(joint_expectation(s, frame = frame)$stage_1[in_1, in_1], 1)
})

test_that("a stratum a threshold table leaves out keeps no threshold", {
  frame <- sl_cert_frame()
  design <- sampling_design() |> stratify_by(st) |>
    draw(n = 2, method = "pps_brewer", mos = mos,
         certainty_size = data.frame(st = "2e+05", certainty_size = 39))
  s <- as.data.frame(sl_quiet(execute(design, frame, seed = 1)))
  expect_identical(s$.certainty_1[s$st == 2e5 & s$id == 5], TRUE)
  expect_false(any(s$.certainty_1[s$st == 1e5]))
  # Unit 1 then has probability 2 x 40 / 100.
  if (1L %in% s$id) {
    expect_equal(s$.weight[s$id == 1L], 1 / 0.8)
  }
})

test_that("a compound threshold table is read within its parent", {
  frame <- data.frame(
    psu = rep(1:2, each = 8L),
    region = rep(c(1e5, 2e5), each = 8L),
    st = rep(rep(c(1e5, 2e5), each = 4L), 2),
    id = 1:16,
    mos = rep(c(40, 20, 20, 20), 4)
  )
  thresholds <- data.frame(
    region = c("1e+05", "2e+05", "100000"),
    st = c("1e+05", "100000", "2e+05"),
    certainty_size = 39
  )
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> stratify_by(region, st) |>
    draw(n = 2, method = "pps_brewer", mos = mos,
         certainty_size = thresholds)
  for (seed in 1:4) {
    s <- as.data.frame(sl_quiet(execute(design, frame, seed = seed)))
    region <- unique(s$region)
    expect_length(region, 1L)
    # Region 100000 names both of its strata, region 200000 only stratum
    # 100000. The row for the other region is not this parent's.
    certain <- s$id[s$.certainty_2]
    expected <- if (region == 1e5) c(1L, 5L) else 9L
    expect_identical(sort(certain), expected, info = paste("seed", seed))
  }
  regions <- vapply(1:4, function(seed) {
    s <- sl_quiet(execute(design, frame, seed = seed))
    unique(as.data.frame(s)$region)
  }, 1)
  expect_setequal(regions, c(1e5, 2e5))
})

test_that("two spellings of one stratum in a draw table are refused", {
  frame <- sl_cert_frame()
  for (threshold in c("certainty_size", "certainty_prop")) {
    design <- sl_cert_design(c("1e+05", "100000", "2e+05"), threshold,
                             values = c(39, 30, 50))
    expect_error(
      sl_quiet(execute(design, frame, seed = 1)),
      class = "samplyr_error_draw_argument", info = threshold
    )
  }
})

test_that("a character stratum compares threshold labels literally", {
  frame <- sl_cert_frame()
  frame$st <- ifelse(frame$st == 1e5, "1e+05", "2e+05")
  literal <- sl_cert_design(c("1e+05", "2e+05"), "certainty_size")
  s <- as.data.frame(sl_quiet(execute(literal, frame, seed = 1)))
  expect_true(s$.certainty_1[s$id == 1])
  plain <- sl_cert_design(c("100000", "200000"), "certainty_size")
  s <- as.data.frame(sl_quiet(execute(plain, frame, seed = 1)))
  expect_false(any(s$.certainty_1))
})

test_that("a random-size draw keeps the fraction its stratum is named with", {
  frame <- data.frame(id = 1:2000, st = rep(c(1e5, 2e5), each = 1000L))
  checked <- character(0)
  for (scipen in c(0, 100)) {
    withr::local_options(scipen = scipen)
    for (spelling in c("scientific", "fixed")) {
      labels <- sl_cert_spellings[[spelling]]
      routes <- list(
        named = stats::setNames(c(0.5, 0.1), labels),
        table = data.frame(st = labels, frac = c(0.5, 0.1))
      )
      for (route in names(routes)) {
        info <- paste(route, spelling, "scipen", scipen)
        s <- as.data.frame(
          sampling_design() |> stratify_by(st) |>
            draw(frac = routes[[route]], method = "bernoulli") |>
            execute(frame, seed = 1)
        )
        expect_equal(1 / s$.weight[s$st == 1e5], rep(0.5, sum(s$st == 1e5)),
                     info = info)
        expect_equal(1 / s$.weight[s$st == 2e5], rep(0.1, sum(s$st == 2e5)),
                     info = info)
        checked <- c(checked, paste(route, spelling))
      }
    }
  }
  expect_setequal(unique(checked), c(
    "named scientific", "named fixed", "table scientific", "table fixed"
  ))
})

## A spelling R writes for several strata

# as.character() keeps 15 significant digits in scientific form, so it writes
# 1000000000000001 and 1000000000000002 both as "1e+15". Such a label names
# no single stratum and is refused, in whatever order the strata come.

sl_wide <- 1e15 + c(1, 2)
sl_wide_text <- sprintf("%.0f", sl_wide)

test_that("a label R writes for two strata is refused, not resolved to one", {
  for (values in list(sl_wide, rev(sl_wide))) {
    expect_error(
      resolve_stratum_labels("1e+15", values),
      class = "samplyr_error_stratum_label_ambiguous"
    )
  }
  # Exact key texts resolve, whichever spelling the other strata share.
  expect_identical(resolve_stratum_labels(sl_wide_text, sl_wide), sl_wide_text)
  expect_identical(
    resolve_stratum_labels(rev(sl_wide_text), rev(sl_wide)), rev(sl_wide_text)
  )
  # A spelling of one stratum listed twice in the column still names one.
  expect_identical(
    resolve_stratum_labels("1e+05", c(1e5, 1e5, 2e5)), "100000"
  )
  err <- expect_error(
    resolve_stratum_labels("1e+15", sl_wide, var = "st"),
    class = "samplyr_error_stratum_label_ambiguous"
  )
  msg <- cli::ansi_strip(conditionMessage(err))
  expect_match(msg, sl_wide_text[1], fixed = TRUE)
  expect_match(msg, sl_wide_text[2], fixed = TRUE)
})

test_that("every argument that names strata refuses the shared spelling", {
  checked <- character(0)
  for (reversed in c(FALSE, TRUE)) {
    values <- if (reversed) rev(sl_wide) else sl_wide
    frame <- data.frame(st = rep(values, each = 6L), id = 1:12,
                        mos = rep(c(40, 20, 20, 10, 5, 5), 2))
    order <- if (reversed) 2:1 else 1:2
    shared <- c("1e+15", sl_wide_text[2])[order]
    canonical <- sl_wide_text[order]
    routes <- function(labels) {
      list(
        "named n" = sampling_design() |> stratify_by(st) |>
          draw(n = stats::setNames(c(2L, 3L)[order], labels)),
        "named frac" = sampling_design() |> stratify_by(st) |>
          draw(frac = stats::setNames(c(2, 3)[order] / 6, labels)),
        "table n" = sampling_design() |> stratify_by(st) |>
          draw(n = data.frame(st = labels, n = c(2L, 3L)[order])),
        "named variance" = sampling_design() |>
          stratify_by(st, alloc = "neyman",
                      variance = stats::setNames(c(1, 4)[order], labels)) |>
          draw(n = 6),
        "certainty_size" = sampling_design() |> stratify_by(st) |>
          draw(n = 2, method = "pps_brewer", mos = mos,
               certainty_size = data.frame(st = labels,
                                           certainty_size = 39))
      )
    }
    bad <- routes(shared)
    good <- routes(canonical)
    for (route in names(bad)) {
      info <- paste(route, if (reversed) "reversed")
      expect_error(
        sl_quiet(execute(bad[[route]], frame, seed = 1)),
        class = "samplyr_error_stratum_label_ambiguous", info = info
      )
      s <- sl_quiet(execute(good[[route]], frame, seed = 1))
      checked <- c(checked, route)
      if (route %in% c("named n", "named frac", "table n")) {
        expect_identical(
          as.vector(table(factor(sprintf("%.0f", s$st), sl_wide_text))),
          c(2L, 3L), info = info
        )
      }
    }
  }
  expect_setequal(unique(checked), c(
    "named n", "named frac", "table n", "named variance", "certainty_size"
  ))
})

test_that("the other columns of a row can tell a shared spelling apart", {
  frame <- data.frame(
    region = rep(c(1, 2), each = 6L),
    st = rep(sl_wide, each = 6L),
    id = 1:12
  )
  # Region 1 holds only 1000000000000001 and region 2 only
  # 1000000000000002, so each row names one stratum.
  table <- data.frame(region = c(1, 2), st = "1e+15", n = c(2L, 3L))
  design <- sampling_design() |> stratify_by(region, st) |> draw(n = table)
  s <- execute(design, frame, seed = 1)
  expect_identical(as.vector(table(s$region)), c(2L, 3L))
  expect_true(all(sprintf("%.0f", s$st[s$region == 1]) == sl_wide_text[1]))

  # A region holding both strata leaves the row ambiguous.
  both <- frame
  both$st[both$region == 1][1:3] <- sl_wide[2]
  err <- expect_error(
    execute(design, both, seed = 1),
    class = "samplyr_error_stratum_label_ambiguous"
  )
  msg <- gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
  expect_match(msg, 'Stratum key "1/1e+15" names more than one stratum.',
               fixed = TRUE)
  for (text in sl_wide_text) {
    expect_match(msg, paste0('"1/', text, '"'), fixed = TRUE)
  }
  expect_match(msg, "Write each value of the key in full", fixed = TRUE)
})

test_that("a later stage reads a shared spelling within its parent", {
  frame <- data.frame(
    psu = rep(1:2, each = 6L),
    region = rep(c(1, 2), each = 6L),
    st = rep(sl_wide, each = 6L),
    id = 1:12
  )
  # One region is selected. Its row names one stratum, and the row for the
  # other region belongs to a parent the stage did not select.
  table <- data.frame(region = c(1, 2), st = "1e+15", n = c(2L, 3L))
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> stratify_by(region, st) |> draw(n = table)
  regions <- numeric(0)
  for (seed in 1:6) {
    s <- sl_quiet(execute(design, frame, seed = seed))
    region <- unique(s$region)
    expect_identical(nrow(s), if (region == 1) 2L else 3L)
    regions <- c(regions, region)
  }
  expect_setequal(unique(regions), c(1, 2))
})

## The scope a label is read in

# A later stage draws within each parent, and reads a stratum label among
# that parent's strata. "1e+15" names 1000000000000001 in PSU 1 and
# 1000000000000002 in PSU 2, so it names one stratum wherever it is read.
# The same label is refused within one parent that holds both, and at the
# first stage, whose one scope holds both.

sl_parent_frame <- function() {
  data.frame(
    psu = rep(1:2, each = 4L), id = 1:8, st = rep(sl_wide, each = 4L),
    mos = rep(c(40, 20, 20, 20), 2)
  )
}

sl_two_stage <- function(..., stratify = "st") {
  sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(!!!rlang::syms(stratify)) |> draw(...)
}

sl_probabilities <- function(design, frame) {
  p <- sl_quiet(exante_probabilities(design, frame, key = id))
  p$probability[order(p$id)]
}

test_that("a later stage reads a threshold label within each parent", {
  frame <- sl_parent_frame()
  # Brewer takes 2 of mos 40, 20, 20, 20 in each parent. The threshold
  # makes the 40 certain and spreads the other draw over the three 20s.
  # Without it the chances are 2 x mos / 100.
  certain <- rep(c(1, 1, 1, 1) / c(1, 3, 3, 3), 2)
  free <- rep(c(0.8, 0.4, 0.4, 0.4), 2)
  tables <- list(
    certainty_size = function(st) data.frame(st = st, certainty_size = 39),
    certainty_prop = function(st) data.frame(st = st, certainty_prop = 0.39)
  )
  checked <- character(0)
  for (threshold in names(tables)) {
    spellings <- list(shared = "1e+15", exact = sl_wide_text, typed = sl_wide)
    for (spelling in names(spellings)) {
      info <- paste(threshold, spelling)
      args <- list(n = 2, method = "pps_brewer", mos = quote(mos))
      args[[threshold]] <- tables[[threshold]](spellings[[spelling]])
      design <- do.call(sl_two_stage, args)
      expect_equal(sl_probabilities(design, frame), certain, info = info)
      expect_no_error(sl_quiet(exante_digest(design, frame)))
      expect_true(sl_quiet(validate_frame(design, frame)), info = info)

      s <- sl_quiet(execute(design, frame, seed = 1, frame_digest = "full"))
      expect_equal(1 / s$.weight, certain[s$id], info = info)
      expect_identical(s$.certainty_2, s$id %in% c(1L, 5L), info = info)
      joint <- joint_expectation(s, frame = frame)$stage_2
      expect_equal(unname(diag(joint)), certain[s$id], info = info)
      checked <- c(checked, info)
    }
    # A table for one parent's stratum leaves the other without a threshold.
    args <- list(n = 2, method = "pps_brewer", mos = quote(mos))
    args[[threshold]] <- tables[[threshold]](sl_wide_text[1])
    expect_equal(
      sl_probabilities(do.call(sl_two_stage, args), frame),
      c(certain[1:4], free[5:8]), info = threshold
    )
  }
  expect_length(unique(checked), 6L)
})

test_that("a threshold label naming two strata of one parent is refused", {
  # PSU 1 holds both strata, so the label names two of its strata.
  frame <- sl_parent_frame()
  frame$st[2] <- sl_wide[2]
  design <- sl_two_stage(
    n = 2, method = "pps_brewer", mos = mos,
    certainty_size = data.frame(st = "1e+15", certainty_size = 39)
  )
  for (route in c("execute", "exante_digest", "exante_probabilities")) {
    expect_error(
      sl_quiet(switch(route,
        execute = execute(design, frame, seed = 1),
        exante_digest = exante_digest(design, frame),
        exante_probabilities = exante_probabilities(design, frame, key = id)
      )),
      class = "samplyr_error_stratum_label_ambiguous", info = route
    )
  }
  # The first stage has one scope, and both strata are in it.
  first <- sampling_design() |> stratify_by(st) |>
    draw(n = 2, method = "pps_brewer", mos = mos,
         certainty_size = data.frame(st = "1e+15", certainty_size = 39))
  expect_error(
    sl_quiet(exante_digest(first, sl_parent_frame())),
    class = "samplyr_error_stratum_label_ambiguous"
  )
})

test_that("a compound threshold table is read within each parent", {
  frame <- sl_parent_frame()
  frame$region <- 7
  design <- sl_two_stage(
    n = 2, method = "pps_brewer", mos = mos, stratify = c("region", "st"),
    certainty_size = data.frame(region = 7, st = "1e+15", certainty_size = 39)
  )
  expect_equal(sl_probabilities(design, frame),
               rep(c(1, 1, 1, 1) / c(1, 3, 3, 3), 2))
})

test_that("a later stage reads a size label within each parent", {
  frame <- sl_parent_frame()
  routes <- list(
    "named n" = sl_two_stage(n = c("1e+15" = 2L)),
    "table n" = sl_two_stage(n = data.frame(st = "1e+15", n = 2L)),
    "named frac" = sl_two_stage(frac = c("1e+15" = 0.5)),
    "table frac" = sl_two_stage(frac = data.frame(st = "1e+15", frac = 0.5)),
    "random-size frac" = sl_two_stage(
      frac = data.frame(st = "1e+15", frac = 0.5), method = "bernoulli"
    )
  )
  checked <- character(0)
  for (route in names(routes)) {
    design <- routes[[route]]
    # Both PSUs are taken, and half of each parent's 4 units.
    expect_equal(sl_probabilities(design, frame), rep(0.5, 8), info = route)
    expect_true(sl_quiet(validate_frame(design, frame)), info = route)
    s <- sl_quiet(execute(design, frame, seed = 1))
    expect_equal(1 / s$.weight, rep(0.5, nrow(s)), info = route)
    if (route != "random-size frac") {
      expect_identical(as.vector(table(s$psu)), c(2L, 2L), info = route)
    }
    checked <- c(checked, route)
  }
  expect_setequal(checked, names(routes))

  # Within one parent holding both strata the label is refused.
  both <- frame
  both$st[2] <- sl_wide[2]
  for (route in names(routes)) {
    expect_error(
      sl_quiet(execute(routes[[route]], both, seed = 1)),
      class = "samplyr_error_stratum_label_ambiguous", info = route
    )
    expect_error(
      sl_quiet(validate_frame(routes[[route]], both)),
      class = "samplyr_error_stratum_label_ambiguous", info = route
    )
  }
})

## A continuation reads labels among the units it can reach

# Only PSU 1 is selected. PSU 2 holds both strata "1e+15" can name, and the
# take table's row for it names a parent the sample did not select.

sl_listing <- function() {
  data.frame(
    psu = rep(1:2, each = 8L),
    st = c(rep(sl_wide[1L], 8L), rep(sl_wide, each = 4L)),
    id = 1:16
  )
}

sl_continued <- function(..., listing = sl_listing(), seed = 1) {
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> stratify_by(psu, st) |> draw(...)
  parent <- sl_quiet(execute(design, listing, stages = 1, seed = seed))
  expect_identical(unique(as.integer(parent$psu)), 1L)
  parent
}

test_that("a continuation gives the same sample from a full or a selected listing", {
  listing <- sl_listing()
  routes <- list(
    n = list(n = data.frame(psu = 1:2, st = c(sl_wide_text[1], "1e+15"),
                            n = c(2L, 2L))),
    frac = list(frac = data.frame(psu = 1:2, st = c(sl_wide_text[1], "1e+15"),
                                  frac = c(0.25, 0.5)))
  )
  checked <- character(0)
  for (route in names(routes)) {
    parent <- do.call(sl_continued, routes[[route]])
    full <- sl_quiet(execute(parent, listing, stages = 2, seed = 1))
    selected <- sl_quiet(
      execute(parent, listing[listing$psu == 1, ], stages = 2, seed = 1)
    )
    expect_identical(full$id, selected$id, info = route)
    expect_identical(full$.weight, selected$.weight, info = route)
    expect_identical(nrow(full), 2L, info = route)
    # PSU 1 of 2, then 2 of its 8 units.
    expect_equal(1 / full$.weight, rep(1 / 8, 2), info = route)
    for (frame in list(listing, listing[listing$psu == 1, ])) {
      expect_true(sl_quiet(validate_frame(parent, frame, stages = 2)),
                  info = route)
    }
    checked <- c(checked, route)
  }
  expect_setequal(checked, c("n", "frac"))
})

test_that("a continuation still refuses what its selected parent cannot read", {
  listing <- sl_listing()
  continue <- function(table, frame = listing) {
    parent <- sl_continued(n = table, listing = frame)
    sl_quiet(execute(parent, frame, stages = 2, seed = 1))
  }
  # Both strata under the selected parent.
  both <- listing
  both$st[1:4] <- sl_wide[2]
  expect_error(
    continue(data.frame(psu = 1:2, st = "1e+15", n = 2L), both),
    class = "samplyr_error_stratum_label_ambiguous"
  )
  # A label of the selected parent that names no stratum.
  expect_error(
    continue(data.frame(psu = c(1, 1, 2), st = c(sl_wide_text[1], "999",
                                                 "1e+15"), n = 2L)),
    class = "samplyr_error_alloc_unknown_strata"
  )
  # A selected parent's stratum the table does not size.
  expect_error(
    continue(data.frame(psu = 2, st = "1e+15", n = 2L)),
    class = "samplyr_error_alloc_missing_coverage"
  )
  # Two spellings of the selected parent's stratum.
  expect_error(
    continue(data.frame(psu = 1, st = c(sl_wide_text[1], "1e+15"), n = 2L)),
    class = "samplyr_error_alloc_duplicate_keys"
  )
  # At the design start PSU 2 is still a candidate, and its row is judged.
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> stratify_by(psu, st) |>
    draw(n = data.frame(psu = 1:2, st = c(sl_wide_text[1], "1e+15"), n = 2L))
  expect_error(sl_quiet(execute(design, listing, seed = 1)),
               class = "samplyr_error_stratum_label_ambiguous")
  expect_error(sl_quiet(validate_frame(design, listing)),
               class = "samplyr_error_stratum_label_ambiguous")
})

test_that("a continuation reads separate registers and numeric parent labels", {
  wide <- 1e15 + c(1, 2)
  psus <- data.frame(psu = c(1e5, 2e5), region = c(1, 2))
  units <- data.frame(
    psu = rep(c(1e5, 2e5), each = 8L),
    st = c(rep(wide[1L], 8L), rep(wide, each = 4L)),
    id = 1:16
  )
  checked <- character(0)
  for (spelling in c("scientific", "fixed")) {
    labels <- if (spelling == "scientific") c("1e+05", "2e+05") else
      c("100000", "200000")
    table <- data.frame(psu = labels, st = c(sprintf("%.0f", wide[1]), "1e+15"),
                        n = 2L)
    design <- sampling_design() |>
      add_stage("psu") |> cluster_by(psu) |> draw(n = 1) |>
      add_stage("unit") |> stratify_by(psu, st) |> draw(n = table)
    seed <- 1
    repeat {
      parent <- sl_quiet(execute(design, psus, stages = 1, seed = seed))
      if (parent$psu == 1e5) break
      seed <- seed + 1
    }
    full <- sl_quiet(execute(parent, units, stages = 2, seed = 1))
    selected <- sl_quiet(
      execute(parent, units[units$psu == 1e5, ], stages = 2, seed = 1)
    )
    expect_identical(full$id, selected$id, info = spelling)
    expect_identical(nrow(full), 2L, info = spelling)
    # One call over both registers still judges PSU 2's row.
    expect_error(
      sl_quiet(execute(design, list(psus, units), seed = seed)),
      class = "samplyr_error_stratum_label_ambiguous", info = spelling
    )
    checked <- c(checked, spelling)
  }
  expect_setequal(checked, c("scientific", "fixed"))
})

test_that("a third stage continued under two completed ancestors reads its parent", {
  wide <- 1e15 + c(1, 2)
  frame <- data.frame(
    psu = rep(1:2, each = 8L),
    ssu = rep(1:4, each = 4L),
    st = c(rep(wide[1L], 8L), rep(wide[1L], 4L), rep(wide, each = 2L)),
    id = 1:16
  )
  # Each SSU's row of the table names the strata under it.
  table <- data.frame(
    psu = c(1, 1, 2, 2), ssu = 1:4,
    st = c(sprintf("%.0f", wide[1]), sprintf("%.0f", wide[1]),
           sprintf("%.0f", wide[1]), "1e+15"),
    n = 2L
  )
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> cluster_by(ssu) |> draw(n = 2) |>
    add_stage() |> stratify_by(psu, ssu, st) |> draw(n = table)
  seed <- 1
  repeat {
    parent <- sl_quiet(execute(design, frame, stages = 1:2, seed = seed))
    if (all(parent$psu == 1)) break
    seed <- seed + 1
  }
  full <- sl_quiet(execute(parent, frame, stages = 3, seed = 1))
  selected <- sl_quiet(
    execute(parent, frame[frame$psu == 1, ], stages = 3, seed = 1)
  )
  expect_identical(full$id, selected$id)
  expect_identical(as.vector(table(full$ssu)), c(2L, 2L))
})

test_that("an unkeyed size may name a stratum only an unselected parent holds", {
  listing <- data.frame(psu = rep(1:2, each = 4L), id = 1:8,
                        st = rep(c(1e5, 2e5), each = 4L))
  continue <- function(n) {
    design <- sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 1) |>
      add_stage() |> stratify_by(st) |> draw(n = n)
    seed <- 1
    repeat {
      parent <- sl_quiet(execute(design, listing, stages = 1, seed = seed))
      if (unique(parent$psu) == 1L) break
      seed <- seed + 1
    }
    sl_quiet(execute(parent, listing, stages = 2, seed = 1))
  }
  # 2e+05 is in PSU 2 alone, which the sample did not select.
  expect_identical(nrow(continue(c("1e+05" = 2L, "2e+05" = 3L))), 2L)
  expect_error(
    continue(c("1e+05" = 2L, "2e+05" = 3L, "3e+05" = 1L)),
    class = "samplyr_error_alloc_unknown_strata"
  )
})

## Previews refuse what execution refuses

test_that("the previews refuse a size for a stratum no unit can reach", {
  frame <- data.frame(psu = rep(1:2, each = 4L), id = 1:8,
                      st = rep(c(1, 2), each = 4L))
  sizes <- c("1" = 2L, "2" = 1L)
  designs <- list(
    "first stage" = function(n) {
      sampling_design() |> stratify_by(st) |> draw(n = n)
    },
    "later stage" = function(n) {
      sampling_design() |> add_stage() |> cluster_by(psu) |> draw(n = 2) |>
        add_stage() |> stratify_by(st) |> draw(n = n)
    }
  )
  previews <- list(
    execute = function(d) execute(d, frame, seed = 1),
    exante_digest = function(d) exante_digest(d, frame),
    exante_probabilities = function(d) {
      exante_probabilities(d, frame, key = id)
    },
    frame_summary = function(d) frame_summary(d, frame)
  )
  checked <- character(0)
  for (stage in names(designs)) {
    good <- designs[[stage]](sizes)
    bad <- designs[[stage]](c(sizes, "9" = 1L))
    for (route in names(previews)) {
      info <- paste(stage, route)
      expect_no_error(sl_quiet(previews[[route]](good)))
      expect_error(
        sl_quiet(previews[[route]](bad)),
        class = "samplyr_error_alloc_unknown_strata", info = info
      )
      checked <- c(checked, info)
    }
  }
  expect_length(unique(checked), 8L)
})

## A key text that is also another stratum's spelling

# "9.00719925474099e+15" is the key text of 2^53 + 2, which is not written
# in plain digits, and the scientific form of 2^53 - 1. It names its own
# stratum wherever that stratum is, and the stratum it spells elsewhere.

sl_above <- 2^53 + 2
sl_below <- 2^53 - 1
sl_both <- sprintf("%.15g", sl_above)

sl_split_frame <- function(reversed = FALSE) {
  values <- c(sl_above, sl_below)
  if (reversed) values <- rev(values)
  data.frame(psu = rep(1:2, each = 4L), st = rep(values, each = 4L),
             id = 1:8)
}

test_that("the fixture's label is one stratum's key and another's spelling", {
  expect_identical(key_value_text(sl_above), sl_both)
  expect_identical(
    withr::with_options(list(scipen = -9), as.character(sl_below)), sl_both
  )
  expect_false(identical(key_value_text(sl_below), sl_both))
})

test_that("a parent reads a key text as the stratum it spells there", {
  checked <- character(0)
  for (reversed in c(FALSE, TRUE)) {
    frame <- sl_split_frame(reversed)
    st <- vapply(1:2, function(p) key_value_text(frame$st[frame$psu == p][1]),
                 "")
    tables <- list(
      "shared n" = data.frame(psu = 1:2, st = sl_both, n = 2L),
      "shared frac" = data.frame(psu = 1:2, st = sl_both, frac = 0.5),
      "exact n" = data.frame(psu = 1:2, st = st, n = 2L),
      "typed n" = data.frame(psu = 1:2, st = frame$st[c(1, 5)], n = 2L)
    )
    for (case in names(tables)) {
      info <- paste(case, if (reversed) "reversed")
      table <- tables[[case]]
      design <- sampling_design() |>
        add_stage() |> cluster_by(psu) |> draw(n = 2) |>
        add_stage() |> stratify_by(psu, st)
      design <- if ("frac" %in% names(table)) {
        draw(design, frac = table)
      } else {
        draw(design, n = table)
      }
      expect_true(sl_quiet(validate_frame(design, frame)), info = info)
      s <- sl_quiet(execute(design, frame, seed = 1))
      expect_identical(as.vector(table(s$psu)), c(2L, 2L), info = info)
      # Both PSUs, then 2 of each one's 4 units.
      expect_equal(sl_probabilities(design, frame), rep(0.5, 8), info = info)
      expect_no_error(sl_quiet(exante_digest(design, frame)))
      expect_no_error(sl_quiet(frame_summary(design, frame)))
      checked <- c(checked, info)
    }
  }
  expect_length(unique(checked), 8L)
})

test_that("a continuation under the spelled stratum's parent draws its take", {
  frame <- sl_split_frame()
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> stratify_by(psu, st) |>
    draw(n = data.frame(psu = 1:2, st = sl_both, n = 2L))
  for (wanted in 1:2) {
    seed <- 1
    repeat {
      parent <- sl_quiet(execute(design, frame, stages = 1, seed = seed))
      if (all(parent$psu == wanted)) break
      seed <- seed + 1
    }
    s <- sl_quiet(execute(parent, frame, stages = 2, seed = 1))
    expect_identical(nrow(s), 2L, info = wanted)
    expect_true(all(s$psu == wanted), info = wanted)
  }
})

test_that("a key text keeps its own stratum where both strata are", {
  # One scope holds both, so the label names its own stratum.
  frame <- data.frame(st = rep(c(sl_above, sl_below), each = 4L), id = 1:8)
  design <- sampling_design() |> stratify_by(st) |>
    draw(n = stats::setNames(c(1L, 3L), c(sl_both, key_value_text(sl_below))))
  s <- sl_quiet(execute(design, frame, seed = 1))
  expect_identical(as.vector(table(s$st == sl_above)), c(3L, 1L))

  # So does a parent holding both, while the other parent reads the label
  # as the one stratum it spells there.
  frame <- sl_split_frame()
  frame$st[2] <- sl_below
  table <- data.frame(
    psu = c(1, 1, 2), st = c(sl_both, key_value_text(sl_below), sl_both),
    n = c(1L, 1L, 2L)
  )
  keyed <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(psu, st) |> draw(n = table)
  expect_true(sl_quiet(validate_frame(keyed, frame)))
  s <- sl_quiet(execute(keyed, frame, seed = 1))
  expect_identical(as.vector(table(s$psu)), c(2L, 2L))
  expect_identical(sum(s$psu == 1 & s$st == sl_above), 1L)

  # Unkeyed, the two names are one stratum in the parent that has only it.
  unkeyed <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(st) |>
    draw(n = stats::setNames(c(1L, 1L), c(sl_both, key_value_text(sl_below))))
  expect_error(
    sl_quiet(execute(unkeyed, frame, seed = 1)),
    class = "samplyr_error_alloc_duplicate_keys"
  )
})

test_that("each parent reads only the table rows that can name it", {
  frame <- data.frame(psu = c(1e5, 1e5, 2e5, 3e5), st = c(1, 2, 1, 1))
  table <- data.frame(
    psu = c("1e+05", "100000", "2e+05", NA, "9e+05"),
    st = c(1, 2, 1, 1, 1), n = 1:5
  )
  read <- parent_table_reader(list(n = table), list(vars = "st"), frame,
                              "psu")
  # Either spelling of the parent, and the missing label its reader refuses.
  expect_identical(read(frame[frame$psu == 1e5, ])$draw$n$n, c(1L, 2L, 4L))
  expect_identical(read(frame[frame$psu == 2e5, ])$draw$n$n, c(3L, 4L))
  expect_identical(read(frame[frame$psu == 3e5, ])$draw$n$n, 4L)
  # A table without the parent's column is read whole.
  whole <- parent_table_reader(list(n = table[c("st", "n")]),
                               list(vars = "st"), frame, "psu")
  expect_identical(whole(frame[1, ])$draw$n$n, 1:5)
})

test_that("a selected parent written in either spelling keeps its unknown-stratum refusal", {
  listing <- data.frame(psu = rep(c(1e5, 2e5), each = 4L),
                        st = rep(c(1, 2), 4L), id = 1:8)
  for (label in c("1e+05", "100000")) {
    table <- data.frame(psu = label, st = c("1", "2", "9"), n = 1L)
    design <- sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 1) |>
      add_stage() |> stratify_by(psu, st) |> draw(n = table)
    seed <- 1
    repeat {
      parent <- sl_quiet(execute(design, listing, stages = 1, seed = seed))
      if (all(parent$psu == 1e5)) break
      seed <- seed + 1
    }
    expect_error(
      sl_quiet(execute(parent, listing, stages = 2, seed = 1)),
      class = "samplyr_error_alloc_unknown_strata", info = label
    )
  }
})

test_that("a row of another parent stays as written, not resolved column by column", {
  # Parent 2 holds 2^53 - 1 only. Resolved alone, each st label of parent
  # 1's rows names it, and the two rows would read as one stratum twice.
  info <- data.frame(psu = 2, st = sl_below)
  table <- data.frame(psu = c(1, 1, 2),
                      st = c(sl_both, key_value_text(sl_below), sl_both))
  out <- resolve_stratum_columns(table, info, c("psu", "st"))
  expect_identical(out$st, c(sl_both, key_value_text(sl_below),
                             key_value_text(sl_below)))
  by_column <- resolve_stratum_columns(table, info, c("psu", "st"),
                                       by_column = TRUE)
  expect_identical(by_column$st, rep(key_value_text(sl_below), 3L))
})

## A row read by all its columns, whatever their order

# The row ("9.00719925474099e+15", "9.00719925474099e+15") is exact in x for
# the stratum (2^53 + 2, 2^53 - 1) and exact in y for (2^53 - 1, 2^53 + 2).
# Neither stratum is named exactly in more columns than the other, so the
# row is ambiguous in either column order. The probabilities are written by
# hand: Brewer takes 2 of mos 40, 20, 20, 20 in each stratum, 0.8 and 0.4,
# and a certain 40 leaves one draw over the three 20s.

sl_cross_frame <- function(full = FALSE, reversed = FALSE) {
  x <- c(sl_above, sl_below)
  y <- c(sl_below, sl_above)
  if (full) {
    x <- c(x, sl_above)
    y <- c(y, sl_above)
  }
  frame <- data.frame(
    x = rep(x, each = 4L), y = rep(y, each = 4L),
    id = seq_len(4L * length(x)), mos = rep(c(40, 20, 20, 20), length(x))
  )
  if (reversed) frame[rev(seq_len(nrow(frame))), ] else frame
}

sl_cross_designs <- function(vars, tables) {
  strata <- sampling_design() |> stratify_by(!!!rlang::syms(vars))
  lapply(tables, function(table) {
    field <- setdiff(names(table), c("x", "y"))
    args <- stats::setNames(list(strata, table), c(".data", field))
    if (!field %in% c("n", "frac")) {
      args <- c(args, list(n = 2, method = "pps_brewer", mos = quote(mos)))
    }
    do.call(draw, args)
  })
}

test_that("a row exact in different columns for two strata is refused in either order", {
  fixed_below <- key_value_text(sl_below)
  tables <- list(
    n = data.frame(x = c(sl_both, fixed_below), y = sl_both, n = c(1L, 2L)),
    frac = data.frame(x = c(sl_both, fixed_below), y = sl_both,
                      frac = c(0.25, 0.5)),
    certainty_size = data.frame(x = sl_both, y = sl_both,
                                certainty_size = 39),
    certainty_prop = data.frame(x = sl_both, y = sl_both,
                                certainty_prop = 0.39)
  )
  checked <- character(0)
  for (vars in list(c("x", "y"), c("y", "x"))) {
    for (reversed in c(FALSE, TRUE)) {
      frame <- sl_cross_frame(reversed = reversed)
      for (rows in list(1:2, 2:1)) {
        designs <- sl_cross_designs(vars, lapply(tables, function(t) {
          t[rows[rows <= nrow(t)], , drop = FALSE]
        }))
        for (field in names(designs)) {
          info <- paste(field, vars[1], reversed, rows[1])
          calls <- list(
            validate = function(d) validate_frame(d, frame),
            execute = function(d) execute(d, frame, seed = 1),
            probabilities = function(d) exante_probabilities(d, frame, key = id),
            digest = function(d) exante_digest(d, frame)
          )
          for (route in names(calls)) {
            err <- expect_error(
              sl_quiet(calls[[route]](designs[[field]])),
              class = "samplyr_error_stratum_label_ambiguous",
              info = paste(info, route)
            )
            msg <- cli::ansi_strip(conditionMessage(err))
            expect_match(msg, paste0(sl_both, "/", fixed_below), fixed = TRUE,
                         info = paste(info, route))
            expect_match(msg, paste0(fixed_below, "/", sl_both), fixed = TRUE,
                         info = paste(info, route))
            expect_match(gsub("\\s+", " ", msg),
                         "Give the stratum columns as numbers", fixed = TRUE,
                         info = paste(info, route))
            checked <- c(checked, paste(field, route))
          }
        }
      }
    }
  }
  expect_setequal(unique(checked), as.vector(outer(
    names(tables), c("validate", "execute", "probabilities", "digest"), paste
  )))
})

test_that("a row exact in every column names its stratum in either order", {
  fixed_below <- key_value_text(sl_below)
  for (vars in list(c("x", "y"), c("y", "x"))) {
    for (reversed in c(FALSE, TRUE)) {
      info <- paste(vars[1], reversed)
      frame <- sl_cross_frame(full = TRUE, reversed = reversed)
      designs <- sl_cross_designs(vars, list(
        certainty_size = data.frame(x = sl_both, y = sl_both,
                                    certainty_size = 39),
        n = data.frame(
          x = c(sl_both, fixed_below, sl_both),
          y = c(sl_both, sl_both, fixed_below),
          n = 1:3
        )
      ))
      expect_true(sl_quiet(validate_frame(designs$certainty_size, frame)),
                  info = info)
      s <- sl_quiet(execute(designs$certainty_size, frame, seed = 1,
                            frame_digest = "full"))
      expect_identical(sort(s$id[s$.certainty_1]), 9L, info = info)
      expected <- c(0.8, 0.4, 0.4, 0.4, 0.8, 0.4, 0.4, 0.4, 1, 1 / 3, 1 / 3,
                    1 / 3)
      expect_equal(sl_probabilities(designs$certainty_size, frame), expected,
                   info = info)
      joint <- joint_expectation(s, frame = frame)$stage_1
      expect_equal(diag(joint), expected[s$id], info = info)

      s <- sl_quiet(execute(designs$n, frame, seed = 1))
      take <- table(paste(key_value_text(s$x), key_value_text(s$y)))
      expect_identical(
        as.vector(take[c(paste(sl_both, sl_both),
                         paste(fixed_below, sl_both),
                         paste(sl_both, fixed_below))]),
        1:3, info = info
      )
    }
  }
})

test_that("typed declarations name each crossed stratum in either order", {
  for (vars in list(c("x", "y"), c("y", "x"))) {
    info <- vars[1]
    frame <- sl_cross_frame()
    designs <- sl_cross_designs(vars, list(
      certainty_size = data.frame(x = c(sl_above, sl_below),
                                  y = c(sl_below, sl_above),
                                  certainty_size = c(39, 1000)),
      n = data.frame(x = c(sl_above, sl_below), y = c(sl_below, sl_above),
                     n = c(1L, 3L))
    ))
    s <- sl_quiet(execute(designs$certainty_size, frame, seed = 1))
    expect_identical(sort(s$id[s$.certainty_1]), 1L, info = info)
    expect_equal(sl_probabilities(designs$certainty_size, frame),
                 c(1, 1 / 3, 1 / 3, 1 / 3, 0.8, 0.4, 0.4, 0.4), info = info)
    s <- sl_quiet(execute(designs$n, frame, seed = 1))
    expect_identical(sort(s$id[s$x == sl_above]), sort(s$id[s$id <= 4L]),
                     info = info)
    expect_identical(c(sum(s$id <= 4L), sum(s$id > 4L)), c(1L, 3L),
                     info = info)
  }
})

test_that("each parent reads a crossed row as the one stratum it holds", {
  # Parent 1 holds only (2^53 + 2, 2^53 - 1) and parent 2 only the other,
  # so the row names one stratum in each. A parent holding both refuses it.
  frame <- sl_cross_frame()
  frame$psu <- rep(1:2, each = 4L)
  for (vars in list(c("psu", "x", "y"), c("psu", "y", "x"))) {
    design <- sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 2) |>
      add_stage() |> stratify_by(!!!rlang::syms(vars)) |>
      draw(n = data.frame(psu = 1:2, x = sl_both, y = sl_both, n = 2L))
    s <- sl_quiet(execute(design, frame, seed = 1))
    expect_identical(as.vector(table(s$psu)), c(2L, 2L), info = vars[2])
    expect_equal(sl_probabilities(design, frame), rep(0.5, 8), info = vars[2])

    both <- frame
    both$psu <- rep(1L, 8L)
    one <- sampling_design() |>
      add_stage() |> cluster_by(psu) |> draw(n = 1) |>
      add_stage() |> stratify_by(!!!rlang::syms(vars)) |>
      draw(n = data.frame(psu = 1, x = sl_both, y = sl_both, n = 2L))
    expect_error(sl_quiet(execute(one, both, seed = 1)),
                 class = "samplyr_error_stratum_label_ambiguous",
                 info = vars[2])
  }
})
