# Digest serialization (execution receipts) and the frame drift
# comparison surfaced through validate_frame().

serialize_fixture <- function(seed = 8) {
  sampling_design() |>
    add_stage("Clusters") |> stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage("Units") |> draw(n = 3) |>
    execute(test_frame, seed = seed)
}

test_that("the digest round-trips through design JSON", {
  s <- serialize_fixture()
  d0 <- samplyr:::get_frame_digest(s)
  restored <- read_design(design_json(s, frame = test_frame))
  d1 <- attr(restored, "execution")$frame_digest

  expect_no_error(samplyr:::validate_frame_digest(d1))
  expect_identical(d1$version, d0$version)
  expect_identical(d1$status, d0$status)
  expect_identical(d1$privacy, d0$privacy)
  expect_length(d1$stages, 2)
  expect_identical(
    d1$frames[[1]]$fingerprint_exact,
    d0$frames[[1]]$fingerprint_exact
  )
  expect_identical(
    d1$frames[[1]]$fingerprint_roles,
    d0$frames[[1]]$fingerprint_roles
  )

  for (k in 1:2) {
    p0 <- d0$stages[[k]]$pools
    p1 <- d1$stages[[k]]$pools
    expect_identical(p1$pool_id, p0$pool_id)
    expect_identical(p1$N, p0$N)
    expect_identical(p1$n_realized, p0$n_realized)
    expect_equal(p1$n_expected, p0$n_expected, tolerance = 1e-12)
    expect_identical(p1$scope, p0$scope)
    expect_identical(
      d1$stages[[k]]$selected$unit_id,
      d0$stages[[k]]$selected$unit_id
    )
  }
  # Chances keep full precision; stratum labels come back as character.
  expect_equal(
    d1$stages[[1]]$units$chance,
    d0$stages[[1]]$units$chance,
    tolerance = 1e-12
  )
  expect_identical(
    d1$stages[[1]]$pools$stratum,
    as.character(d0$stages[[1]]$pools$stratum)
  )
  # Continuation linkage keys survive.
  expect_identical(
    d1$stages[[1]]$selected$key,
    d0$stages[[1]]$selected$key
  )
  expect_identical(d1$version, 2L)
})

test_that("a v2 digest round-trips through a design file", {
  s <- serialize_fixture()
  path <- withr::local_tempfile(fileext = ".json")
  expect_invisible(write_design(s, path, frame = test_frame))
  restored <- read_design(path)
  digest <- attr(restored, "execution")$frame_digest

  expect_identical(digest$version, 2L)
  expect_no_error(samplyr:::validate_frame_digest(digest))
  original <- samplyr:::get_frame_digest(s)
  expect_identical(digest$status, original$status)
  expect_identical(length(digest$stages), length(original$stages))
  expect_equal(
    lapply(digest$stages, function(stage) stage$pools$n_expected),
    lapply(original$stages, function(stage) stage$pools$n_expected)
  )
})

test_that("WR parent occurrences survive digest serialization", {
  frame <- data.frame(
    parent = rep(1:2, each = 5),
    unit = rep(1:5, 2),
    parent_mos = rep(c(1, 2), each = 5)
  )
  s <- sampling_design() |>
    add_stage() |> cluster_by(parent) |>
    draw(n = 3, method = "pps_multinomial", mos = parent_mos) |>
    add_stage() |> draw(n = 2) |>
    execute(frame, seed = 7)
  original <- samplyr:::get_frame_digest(s)
  path <- withr::local_tempfile(fileext = ".json")

  expect_invisible(write_design(s, path, frame = frame))
  restored <- read_design(path)
  digest <- attr(restored, "execution")$frame_digest

  expect_true("parent_occurrence" %in% names(digest$stages[[2]]$pools))
  expect_identical(
    digest$stages[[2]]$pools$parent_unit,
    original$stages[[2]]$pools$parent_unit
  )
  expect_identical(
    digest$stages[[2]]$pools$parent_occurrence,
    original$stages[[2]]$pools$parent_occurrence
  )
})

test_that("quantile distributions and diagnostics round-trip", {
  s <- sampling_design() |>
    draw(n = 20, method = "cube", mos = mos, aux = c(y, bound(stratum))) |>
    execute(test_frame, seed = 62)
  d0 <- samplyr:::get_frame_digest(s)
  d1 <- attr(
    read_design(suppressWarnings(design_json(s))), "execution"
  )$frame_digest

  expect_equal(
    d1$stages[[1]]$chance_distribution$chance,
    d0$stages[[1]]$chance_distribution$chance,
    tolerance = 1e-12
  )
  expect_identical(
    d1$stages[[1]]$chance_distribution$n_units,
    d0$stages[[1]]$chance_distribution$n_units
  )
  expect_equal(
    d1$stages[[1]]$diagnostics$balance$realized,
    d0$stages[[1]]$diagnostics$balance$realized,
    tolerance = 1e-12
  )
  b1 <- d1$stages[[1]]$diagnostics$bounds
  expect_identical(sum(b1$satisfied), 4L)
})

test_that("a sample without a digest writes a receipt without one", {
  s <- sampling_design() |>
    draw(n = 10) |>
    execute(test_frame, seed = 1, frame_digest = "none")
  restored <- read_design(design_json(s, frame = test_frame))
  expect_null(attr(restored, "execution")$frame_digest)
  # The receipt itself is intact.
  expect_identical(attr(restored, "execution")$seed, 1L)
})

test_that("schema v1 is rejected before either pool spelling is decoded", {
  s <- serialize_fixture()
  json <- design_json(s, frame = test_frame)
  payload <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  payload$execution$frame_digest$version <- 1L
  new_key <- samplyr:::make_group_key(
    data.frame(region = "A", cluster = "cl01"),
    c("region", "cluster")
  )
  expect_match(new_key, "^2\\|V")
  payload$execution$frame_digest$stages[[1]]$selected[[1]]$key <- new_key
  v1_new_names <- jsonlite::toJSON(
    payload, auto_unbox = TRUE, na = "null"
  )
  err <- expect_error(
    read_design(v1_new_names),
    class = "samplyr_error_digest_version"
  )
  expect_match(conditionMessage(err), "schema version 1", fixed = TRUE)
  expect_match(conditionMessage(err), "re-execute", ignore.case = TRUE)

  for (stage in seq_along(payload$execution$frame_digest$stages)) {
    rows <- payload$execution$frame_digest$stages[[stage]]$pools
    payload$execution$frame_digest$stages[[stage]]$pools <- lapply(
      rows,
      function(row) {
        row$expected_n <- row$n_expected
        row$n_expected <- NULL
        row
      }
    )
  }
  v1_old_names <- jsonlite::toJSON(
    payload, auto_unbox = TRUE, na = "null"
  )
  expect_error(
    read_design(v1_old_names),
    class = "samplyr_error_digest_version"
  )
})

test_that("future digest versions fail without returning a partial design", {
  s <- serialize_fixture()
  payload <- jsonlite::fromJSON(
    design_json(s, frame = test_frame), simplifyVector = FALSE
  )
  payload$execution$frame_digest$version <- 99L
  tampered <- jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null")
  completed <- FALSE
  err <- expect_error(
    {
      read_design(tampered)
      completed <- TRUE
    },
    class = "samplyr_error_digest_version"
  )
  expect_false(completed)
  expect_match(conditionMessage(err), "newer version", ignore.case = TRUE)
})

test_that("a malformed stored digest version keeps its malformed class", {
  s <- serialize_fixture()
  payload <- jsonlite::fromJSON(
    design_json(s, frame = test_frame), simplifyVector = FALSE
  )
  stored <- payload$execution$frame_digest
  stored$version <- "two"
  expect_error(
    samplyr:::decode_frame_digest(stored),
    class = "samplyr_error_digest_malformed"
  )
})

test_that("validate_frame is silent when the frame matches the digest", {
  s <- serialize_fixture()
  expect_no_message(validate_frame(s, test_frame))
  expect_true(validate_frame(s, test_frame))
})

test_that("added analysis columns do not report drift", {
  s <- serialize_fixture()
  frame2 <- test_frame
  frame2$outcome <- seq_len(nrow(frame2))
  expect_no_message(
    validate_frame(s, frame2),
    class = "samplyr_message_digest_drift"
  )
})

test_that("validate_frame reports where the frame drifted", {
  withr::local_options(cli.width = 500)
  s <- serialize_fixture()
  frame3 <- test_frame[test_frame$cluster != "cl01", ]
  expect_message(
    validate_frame(s, frame3),
    class = "samplyr_message_digest_drift"
  )
  txt <- paste(
    testthat::capture_messages(validate_frame(s, frame3)),
    collapse = ""
  )
  expect_match(txt, "115 rows instead of the 120 recorded", fixed = TRUE)
  expect_match(txt, "stratum = A", fixed = TRUE)
  expect_match(txt, "5 units instead of the 6 recorded", fixed = TRUE)
  # The emptied stage-2 pool is named through its parent's key.
  expect_match(txt, "under cl01", fixed = TRUE)
  expect_match(txt, "0 units instead of the 5 recorded", fixed = TRUE)
})

test_that("drift severity follows the fingerprint argument", {
  s <- serialize_fixture()
  frame3 <- test_frame[test_frame$cluster != "cl01", ]
  expect_warning(
    validate_frame(s, frame3, fingerprint = "warn"),
    class = "samplyr_warning_digest_drift"
  )
  expect_no_message(
    validate_frame(s, frame3, fingerprint = "ignore"),
    class = "samplyr_message_digest_drift"
  )
})

test_that("a restored design compares through its stored digest", {
  s <- serialize_fixture()
  restored <- read_design(design_json(s, frame = test_frame))
  expect_no_message(
    validate_frame(restored, test_frame),
    class = "samplyr_message_digest_drift"
  )
  # Both the file fingerprint and the digest report the drift.
  frame3 <- test_frame[test_frame$cluster != "cl01", ]
  msgs <- testthat::capture_messages(validate_frame(restored, frame3))
  txt <- paste(msgs, collapse = "")
  expect_match(txt, "recorded when the design was saved", fixed = TRUE)
  expect_match(txt, "digest recorded at execution", fixed = TRUE)
})

test_that("an invalidated digest is not written and not compared", {
  s <- serialize_fixture()
  s$.weight <- s$.weight * 2
  restored <- suppressWarnings(
    read_design(design_json(s, frame = test_frame))
  )
  expect_null(attr(restored, "execution")$frame_digest)
  expect_no_message(
    validate_frame(s, test_frame),
    class = "samplyr_message_digest_drift"
  )
})

test_that("modified strata values surface as pool drift", {
  withr::local_options(cli.width = 500)
  s <- serialize_fixture()
  frame4 <- test_frame
  frame4$stratum[frame4$stratum == "A"][1:5] <- "B"
  txt <- paste(
    testthat::capture_messages(validate_frame(s, frame4)),
    collapse = ""
  )
  expect_match(txt, "stratum = A", fixed = TRUE)
  expect_match(txt, "stratum = B", fixed = TRUE)
})

test_that("a shifted size measure surfaces as chance drift", {
  withr::local_options(cli.width = 500)
  s <- serialize_fixture()
  frame5 <- test_frame
  # Double one cluster's size measure: same pool counts, different
  # brewer inclusion probabilities within stratum A only.
  frame5$mos[frame5$cluster == "cl01"] <-
    frame5$mos[frame5$cluster == "cl01"] * 2
  txt <- paste(
    testthat::capture_messages(validate_frame(s, frame5)),
    collapse = ""
  )
  expect_match(txt, "role-scoped fingerprint mismatch", fixed = TRUE)
  expect_match(txt, "Clusters: selection chances differ in 1 of 4",
               fixed = TRUE)
  # No pool counts moved, so no recount lines.
  expect_false(grepl("instead of the", txt, fixed = TRUE))
})

test_that("a rescaled size measure reports unchanged chances", {
  withr::local_options(cli.width = 500)
  s <- serialize_fixture()
  frame6 <- test_frame
  frame6$mos <- frame6$mos * 10
  txt <- paste(
    testthat::capture_messages(validate_frame(s, frame6)),
    collapse = ""
  )
  # The bytes changed but no inclusion probability did: the chance
  # comparison is the sharper instrument.
  expect_match(txt, "role-scoped fingerprint mismatch", fixed = TRUE)
  expect_match(txt, "selection chances are unchanged", fixed = TRUE)
  expect_false(grepl("selection chances differ", txt, fixed = TRUE))
})

test_that("chance drift covers quantile element pools", {
  withr::local_options(cli.width = 500)
  s <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 42)
  frame7 <- test_frame
  frame7$mos[1:5] <- frame7$mos[1:5] * 3
  txt <- paste(
    testthat::capture_messages(validate_frame(s, frame7)),
    collapse = ""
  )
  expect_match(txt, "selection chances differ in 1 of 1", fixed = TRUE)
})

test_that("designs the ex-ante builder refuses skip chance drift", {
  withr::local_options(cli.width = 500)
  s <- sampling_design() |>
    add_stage() |> cluster_by(cluster) |>
    draw(n = 4, method = "pps_multinomial", mos = mos) |>
    add_stage() |> draw(n = 2) |>
    execute(test_frame, seed = 3)
  frame8 <- test_frame[-1, ]
  # Structural drift still reports; no chance lines, no error.
  txt <- paste(
    testthat::capture_messages(validate_frame(s, frame8)),
    collapse = ""
  )
  expect_match(txt, "119 rows instead of the 120 recorded",
               fixed = TRUE)
  expect_false(grepl("selection chances", txt, fixed = TRUE))
})

test_that("chance drift reports through a restored design too", {
  withr::local_options(cli.width = 500)
  s <- serialize_fixture()
  restored <- read_design(design_json(s, frame = test_frame))
  frame9 <- test_frame
  frame9$mos[frame9$cluster == "cl01"] <-
    frame9$mos[frame9$cluster == "cl01"] * 2
  txt <- paste(
    testthat::capture_messages(validate_frame(restored, frame9)),
    collapse = ""
  )
  expect_match(txt, "Clusters: selection chances differ in 1 of 4",
               fixed = TRUE)
})

test_that("probabilities round-trips and is enforced past draw", {
  on.exit(sondage::unregister_method("rt_exact"), add = TRUE)
  sampler <- function(pik, n = NULL, prn = NULL, ...) {
    order(pik, decreasing = TRUE)[seq_len(n)]
  }
  sondage::register_method(
    "rt_exact", "wor", sample_fn = sampler, probabilities = "exact"
  )
  s <- sampling_design() |>
    draw(n = 10, method = "pps_rt_exact", mos = mos) |>
    execute(test_frame, seed = 57)
  bare_json <- design_json(
    sampling_design() |> draw(n = 10, method = "pps_rt_exact", mos = mos)
  )
  json <- design_json(s, frame = test_frame)
  restored <- read_design(json)
  expect_identical(restored$stages[[1]]$draw_spec$method_probabilities, "exact")

  # Replay refuses when the re-registered method contradicts the
  # recorded declaration.
  sondage::unregister_method("rt_exact")
  sondage::register_method(
    "rt_exact", "wor", sample_fn = sampler, probabilities = "unknown"
  )
  expect_error(
    replay_design(restored, test_frame),
    class = "samplyr_error"
  )

  # A design file carrying probabilities = "unknown" bypasses draw(), so
  # execution itself re-checks and refuses. Flip only the flag in a
  # legitimately written file.
  payload <- jsonlite::fromJSON(bare_json, simplifyVector = FALSE)
  payload$tools$samplyr$design$stages[[1]]$method$probabilities <- "unknown"
  tampered <- read_design(
    jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null")
  )
  expect_identical(tampered$stages[[1]]$draw_spec$method_probabilities, "unknown")
  expect_error(
    execute(tampered, test_frame, seed = 57),
    class = "samplyr_error_unknown_probabilities"
  )
})

test_that("frame_summary reads a restored design's receipt digest", {
  s <- serialize_fixture()
  restored <- read_design(design_json(s, frame = test_frame))

  expect_equal(frame_summary(restored), frame_summary(s))
  fs_restored <- frame_summary(restored, detail = "pool")
  fs_sample <- frame_summary(s, detail = "pool")
  expect_equal(fs_restored$N, fs_sample$N)
  expect_equal(fs_restored$n_realized, fs_sample$n_realized)
  expect_equal(
    as.character(fs_restored$stratum),
    as.character(fs_sample$stratum)
  )

  bare <- sampling_design() |> draw(n = 5)
  expect_error(frame_summary(bare), class = "samplyr_error_no_digest")
  expect_error(frame_summary(42), class = "samplyr_error")
})

test_that("the probabilities tier round-trips with the digest", {
  s <- sampling_design() |>
    draw(n = 10, method = "pps_sps", mos = mos) |>
    execute(test_frame, seed = 33)
  json <- design_json(s, frame = test_frame)
  restored <- read_design(json)
  d <- attr(restored, "execution")$frame_digest
  expect_identical(d$stages[[1]]$probabilities, "approximate")
  expect_identical(frame_summary(restored)$probabilities, "approximate")

  # Digests written before the field existed carry no tier: they must
  # still validate and report NA rather than invent one.
  payload <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  stored <- payload$execution$frame_digest
  stored$stages[[1]]$probabilities <- NULL
  d_old <- samplyr:::decode_frame_digest(stored)
  expect_no_error(samplyr:::validate_frame_digest(d_old))
  expect_null(d_old$stages[[1]]$probabilities)
  fs_old <- samplyr:::frame_summary_stage(d_old$stages, "eligible")
  expect_identical(fs_old$probabilities, NA_character_)
})
