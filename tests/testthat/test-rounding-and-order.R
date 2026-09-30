## Sizes computed from `frac` are the ones the arithmetic means

# `100 * 0.07` is 7.000000000000001 in floating point, and the size is still 7.
# The oracle is integer arithmetic on N * k / 100, which has no rounding error.

test_that("frac sizes equal the integer arithmetic for every N and k", {
  grid <- expand.grid(N = 1:1000, k = 1:99)
  frac <- grid$k / 100
  up <- (grid$N * grid$k + 99L) %/% 100L
  down <- (grid$N * grid$k) %/% 100L
  # Half away from zero: add 50 hundredths, then truncate.
  nearest <- (grid$N * grid$k + 50L) %/% 100L

  expect_identical(round_sample_size(grid$N * frac, "up"), pmax(up, 1L))
  expect_identical(round_sample_size(grid$N * frac, "down"), pmax(down, 1L))
  expect_identical(
    round_sample_size(grid$N * frac, "nearest"),
    pmax(nearest, 1L)
  )
})

test_that("execute() draws 7 of 100 at frac = 0.07, per PSU as well", {
  frame <- data.frame(id = 1:100)
  for (rule in c("up", "down", "nearest")) {
    s <- sampling_design() |>
      draw(frac = 0.07, round = rule) |>
      execute(frame, seed = 1)
    expect_identical(nrow(s), 7L, label = rule)
  }

  # The same holds in every PSU of a later stage.
  frame2 <- data.frame(psu = rep(1:5, each = 100), id = 1:500)
  s2 <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage() |>
    draw(frac = 0.07) |>
    execute(frame2, seed = 1)
  expect_identical(as.vector(table(s2$psu)), c(7L, 7L, 7L))
})

test_that("round = 'nearest' takes halves up, as documented", {
  expect_identical(round_sample_size(c(0.5, 1.5, 2.5, 3.5), "nearest"),
                   c(1L, 2L, 3L, 4L))
  frame <- data.frame(id = 1:50)
  s <- sampling_design() |>
    draw(frac = 0.05, round = "nearest") |>
    execute(frame, seed = 1)
  expect_identical(nrow(s), 3L)
})

## An allocation does not depend on the order strata arrive in

# Four strata of ten and n = 6 leave two units tied across all four.
tied_frame <- function() {
  data.frame(
    id = 1:40,
    st = rep(c("c", "a", "d", "b"), each = 10),
    size = rep(1:10, 4)
  )
}

tied_counts <- function(frame, design) {
  s <- execute(design, frame, seed = 1)
  counts <- table(factor(as.character(s$st), levels = c("a", "b", "c", "d")))
  stats::setNames(as.vector(counts), names(counts))
}

test_that("tied strata are served in label order, whatever the frame order", {
  design <- sampling_design() |>
    stratify_by(st, alloc = "proportional") |>
    draw(n = 6)
  frame <- tied_frame()
  expected <- c(a = 2L, b = 2L, c = 1L, d = 1L)

  expect_identical(tied_counts(frame, design), expected)
  expect_identical(tied_counts(frame[40:1, ], design), expected)
  set.seed(11)
  expect_identical(tied_counts(frame[sample(40), ], design), expected)

  # Factor levels in any order sort by label, not by level.
  relevelled <- frame
  relevelled$st <- factor(relevelled$st, levels = c("d", "c", "b", "a"))
  expect_identical(tied_counts(relevelled, design), expected)
})

test_that("the joint frame path allocates tied strata as selection did", {
  # First appearance c, a, d, b differs from sorted order.
  frame <- tied_frame()
  design <- sampling_design() |>
    stratify_by(st, alloc = "proportional") |>
    draw(n = 6, method = "pps_brewer", mos = size)
  s <- execute(design, frame, seed = 1, frame_digest = "full")

  from_frame <- joint_expectation(s, frame)$stage_1
  from_digest <- joint_expectation(s)$stage_1
  expect_lt(max(abs(from_frame - from_digest)), 1e-12)
  expect_lt(max(abs(diag(from_frame) - 1 / s$.weight)), 1e-12)
})

test_that("tied strata inside each PSU agree between joint paths", {
  frame <- data.frame(
    psu = rep(1:6, each = 20),
    st = rep(rep(c("d", "c", "b", "a"), each = 5), 6),
    size = 1:120
  )
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage() |>
    stratify_by(st, alloc = "proportional") |>
    draw(n = 6, method = "pps_brewer", mos = size)
  s <- execute(design, frame, seed = 2, frame_digest = "full")

  counts <- table(s$psu, s$st)
  expect_identical(
    unname(unclass(counts[1, ])),
    c(2L, 2L, 1L, 1L)
  )
  from_frame <- joint_expectation(s, frame)$stage_2
  expect_lt(max(abs(diag(from_frame) - 1 / s$.weight_2)), 1e-12)
})

## Control order is the byte order of UTF-8 labels, in every locale

# The oracle writes each label's UTF-8 bytes as hex. Hex pairs compare as
# the bytes do, so this is byte order with no sort of text involved.
utf8_hex_order <- function(x) {
  hex <- vapply(x, function(s) {
    paste(sprintf("%02x", as.integer(charToRaw(enc2utf8(s)))), collapse = "")
  }, character(1), USE.NAMES = FALSE)
  order(hex, method = "radix")
}

mixed_labels <- function() {
  e_utf8 <- "\u00e9t\u00e9"
  e_latin1 <- iconv("\u00e8re", "UTF-8", "latin1")
  # What read.csv() returns in a UTF-8 locale: UTF-8 bytes marked unknown.
  unknown <- rawToChar(as.raw(c(0xc3, 0xa0, 0x62)))
  x <- c("b", e_utf8, "B", e_latin1, "_x", unknown, "a", "Z", "10", "9")
  stopifnot(
    identical(Encoding(e_latin1), "latin1"),
    identical(Encoding(unknown), "unknown")
  )
  x
}

test_that("control sorts mixed encodings by their UTF-8 bytes", {
  labels <- mixed_labels()
  frame <- data.frame(lab = labels, id = seq_along(labels))
  expect_identical(
    control_order(frame, list(rlang::quo(lab))),
    utf8_hex_order(labels)
  )
  expect_identical(
    control_order(frame, list(rlang::quo(desc(lab)))),
    rev(utf8_hex_order(labels))
  )
  expect_identical(order(serp(labels)), utf8_hex_order(labels))
  expect_identical(
    order(serp(labels, rep(1, length(labels)))),
    utf8_hex_order(labels)
  )
})

test_that("missing control values sort last, also under desc()", {
  frame <- data.frame(g = c("b", NA, "a", "c"))
  expect_identical(
    control_order(frame, list(rlang::quo(g))),
    c(3L, 1L, 4L, 2L)
  )
  expect_identical(
    control_order(frame, list(rlang::quo(desc(g)))),
    c(4L, 1L, 3L, 2L)
  )
  # Namespaced desc() too, under the session locale since testthat uses C.
  cased <- data.frame(g = c("b", NA, "B", "a"))
  utf8 <- Sys.getlocale("LC_CTYPE")
  expect_identical(
    withr::with_locale(
      c(LC_COLLATE = utf8),
      control_order(cased, list(rlang::quo(dplyr::desc(g))))
    ),
    c(1L, 4L, 3L, 2L)
  )
})

test_that("a controlled sample is the same under a C and a UTF-8 locale", {
  skip_on_cran()
  labels <- mixed_labels()
  # Unequal groups, so that moving a group moves the systematic hits.
  lab <- rep(labels, times = 11:20)
  frame <- data.frame(
    id = seq_along(lab),
    lab = lab,
    size = seq_along(lab) %% 17 + 1
  )
  designs <- list(
    sys = sampling_design() |>
      draw(n = 20, method = "systematic", control = c(lab, desc(size))),
    serp = sampling_design() |>
      draw(n = 20, method = "systematic", control = serp(lab)),
    pps = sampling_design() |>
      draw(n = 20, method = "pps_systematic", mos = size, control = lab)
  )
  run <- function() {
    ids <- vapply(designs, function(d) {
      rlang::hash(sort(execute(d, frame, seed = 4)$id))
    }, character(1))
    panelled <- sampling_design() |>
      draw(n = 40, control = lab) |>
      execute(frame, seed = 4, panels = 4)
    c(ids, panels = rlang::hash(panelled$.panel[order(panelled$id)]))
  }

  # testthat sorts in C, so the UTF-8 run names the session locale.
  utf8 <- Sys.getlocale("LC_CTYPE")
  in_c <- withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), run())
  in_utf8 <- withr::with_locale(c(LC_CTYPE = utf8, LC_COLLATE = utf8), run())
  expect_identical(in_c, in_utf8)
})
