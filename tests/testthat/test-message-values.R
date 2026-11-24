## Identifiers in messages are written in full

# A message writes a numeric id as its key text, so the double 100000 reads
# "100000" under the default scipen, never "1e+05".

mv_text <- function(cnd) cli::ansi_strip(conditionMessage(cnd))

mv_two_stage <- function() {
  sampling_design() |> add_stage("psu") |> cluster_by(psu_id) |>
    draw(n = 3) |> add_stage("person") |> draw(n = 2)
}

test_that("values are written in full whatever their storage", {
  withr::local_options(scipen = 0)
  expect_identical(shown_values(c(1e5, 0.123456789)), c("100000", "0.123456789"))
  expect_identical(shown_values(100000L), "100000")
  expect_identical(shown_values(c("P1", "P2")), c("P1", "P2"))
  expect_identical(shown_values(1:9), as.character(1:5))
  # A code haven declares missing shows as the value it stores.
  skip_if_not_installed("haven")
  x <- haven::labelled_spss(c(1e5, 2e5), na_values = 1e5)
  expect_identical(shown_values(x), c("100000", "200000"))
  expect_identical(
    key_labels(data.frame(a = 1e5, b = "x"), c("a", "b")), "100000/x"
  )
  expect_identical(format_key_preview(data.frame(a = c(1e5, NA))), c("100000", "NA"))
})

test_that("a 64-bit id keeps all its digits", {
  skip_if_not_installed("bit64")
  x <- bit64::as.integer64("9007199254740993")
  expect_identical(shown_values(x), "9007199254740993")
})

test_that("linkage messages name a numeric parent in full", {
  withr::local_options(scipen = 0)
  psu <- data.frame(psu_id = c(100000, 200000, 300000))
  listing <- data.frame(
    psu_id = rep(c(100000, 300000), each = 3), person_id = 1:6
  )
  warnings <- character(0)
  err <- withCallingHandlers(
    tryCatch(execute(mv_two_stage(), list(psu, listing), seed = 1),
             error = identity),
    warning = function(w) {
      warnings <<- c(warnings, cli::ansi_strip(conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  expect_s3_class(err, "samplyr_error_frame_missing_parent")
  expect_match(mv_text(err), "cannot sample within \"200000\"", fixed = TRUE)
  expect_length(warnings, 1L)
  expect_match(warnings, "candidate unit of stage 2 \"person\": 200000.",
               fixed = TRUE)
  expect_false(any(grepl("e+05", c(mv_text(err), warnings), fixed = TRUE)))
})

test_that("an allocation message names a numeric stratum in full", {
  withr::local_options(scipen = 0)
  frame <- data.frame(st = rep(c(1e5, 2e5), each = 10L), id = 1:20)
  design <- sampling_design() |> stratify_by(st) |>
    draw(n = data.frame(st = 1e5, n = 2L))
  err <- expect_error(
    execute(design, frame, seed = 1),
    class = "samplyr_error_alloc_missing_coverage"
  )
  expect_match(mv_text(err), "\"200000\"", fixed = TRUE)
  expect_false(grepl("e+05", mv_text(err), fixed = TRUE))
})

test_that("the certainty register names a PSU whose MOS differs in full", {
  withr::local_options(scipen = 0)
  psu <- data.frame(
    psu_id = as.double(c(100000, 200000, 100002, 100003, 100004, 100005)),
    stratum = rep("A", 6),
    N = c(500, 100, 100, 100, 100, 100)
  )
  plan <- suppressWarnings(svyplan::n_alloc_joint(
    frame = data.frame(stratum = "A", N = sum(psu$N), take_2 = 10),
    measures = data.frame(stratum = "A", name = "y", sd = 10, mean = 50,
                          icc_1 = 0.05),
    targets = data.frame(name = "y", cv = 0.15),
    psu = psu
  ))
  frame <- psu
  frame$N[1:2] <- frame$N[1:2] + 1
  design <- sampling_design() |>
    add_stage("psu") |> stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_brewer", mos = N)
  err <- expect_error(
    execute(design, frame, seed = 1),
    class = "samplyr_error_certainty_register_mismatch"
  )
  expect_match(mv_text(err), "\"100000\" and \"200000\"", fixed = TRUE)
  expect_false(grepl("e+05", mv_text(err), fixed = TRUE))
})

test_that("the drift check counts a pool alike in integer and double", {
  withr::local_options(scipen = 0)
  frame <- data.frame(st = rep(c(1e5, 2e5), each = 10L), id = 1:20)
  design <- sampling_design() |> stratify_by(st) |> draw(n = 2)
  s <- execute(design, frame, seed = 1, frame_digest = "full")
  integer_frame <- frame
  integer_frame$st <- as.integer(integer_frame$st)
  msg <- expect_message(
    validate_frame(s, integer_frame),
    class = "samplyr_message_digest_drift"
  )
  # The column's storage changed, and each pool still holds its 10 units.
  expect_match(mv_text(msg), "fingerprint mismatch", fixed = TRUE)
  expect_false(grepl("units instead of", mv_text(msg), fixed = TRUE))
  expect_no_message(validate_frame(s, frame))
})
