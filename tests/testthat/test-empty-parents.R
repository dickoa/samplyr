## A selected parent with nothing to sample
#
# Some selected households have no eligible member. Under `on_empty` they are
# empty pools, and each contributes zero to every total.

# Six EAs of four households. The roster lists 0 to 3 eligible persons per
# household, 29 in all, and seven households have none.
ep_eas <- function() data.frame(ea = 1:6)
ep_households <- function() data.frame(ea = rep(1:6, each = 4), hh = 1:24)
ep_persons <- function() {
  n_persons <- c(2, 0, 1, 3, 0, 2, 1, 1, 0, 0, 2, 3,
                 1, 2, 0, 1, 3, 0, 1, 1, 2, 0, 1, 2)
  hh <- ep_households()
  out <- data.frame(
    ea = rep(hh$ea, n_persons),
    hh = rep(hh$hh, n_persons)
  )
  out$pid <- seq_len(nrow(out))
  out
}

ep_design <- function(on_empty = "silent", ...) {
  sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 3) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> draw(..., on_empty = on_empty)
}

test_that("the default still refuses, and names on_empty", {
  # The candidate-gap warning comes first under "error".
  err <- tryCatch(
    suppressWarnings(execute(
      ep_design("error", frac = 1),
      list(ep_eas(), ep_households(), ep_persons()), seed = 4
    )),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_frame_missing_parent")
  expect_match(conditionMessage(err), "on_empty", fixed = TRUE)
})

test_that("empty households contribute zero and are recorded exactly", {
  persons <- ep_persons()
  two <- execute(
    ep_design(frac = 1), list(ep_eas(), ep_households()),
    stages = 1:2, seed = 4
  )
  three <- execute(two, persons, seed = 5)

  selected <- unique(as.data.frame(two)[, c("ea", "hh")])
  empty <- dplyr::anti_join(selected, persons, by = c("ea", "hh"))
  expect_gt(nrow(empty), 0L)

  records <- attr(three, "metadata")$empty_parents
  expect_length(records, 1L)
  expect_identical(records[[1]]$stage, 3L)
  expect_identical(records[[1]]$n, nrow(empty))
  expect_identical(
    sort(paste(records[[1]]$keys$ea, records[[1]]$keys$hh)),
    sort(paste(empty$ea, empty$hh))
  )

  # Every eligible person of a selected household, at its household weight.
  hh_weight <- as.data.frame(two)[, c("ea", "hh", ".weight")]
  per_hh <- stats::aggregate(pid ~ ea + hh, data = persons, FUN = length)
  hand <- merge(hh_weight, per_hh, by = c("ea", "hh"))
  expect_equal(sum(three$.weight), sum(hand$.weight * hand$pid))
  expect_identical(nrow(three), sum(hand$pid))
})

test_that("warn reports once per stage, silent says nothing", {
  frames <- list(ep_eas(), ep_households(), ep_persons())
  caught <- list()
  s <- withCallingHandlers(
    execute(ep_design("warn", n = 1), frames, seed = 4),
    samplyr_warning_empty_parent = function(w) {
      caught[[length(caught) + 1L]] <<- w
      invokeRestart("muffleWarning")
    },
    samplyr_warning_lonely_psu = function(w) invokeRestart("muffleWarning")
  )
  expect_length(caught, 1L)
  expect_identical(caught[[1]]$stage, 3L)
  expect_identical(
    caught[[1]]$payload$n_empty,
    attr(s, "metadata")$empty_parents[[1]]$n
  )
  expect_no_warning(
    execute(ep_design("silent", n = 1), frames, seed = 4)
  )
})

test_that("registers with empty parents are not incomplete at that stage", {
  frames <- list(ep_eas(), ep_households(), ep_persons())
  expect_no_error(validate_frame(ep_design("silent", n = 1), frames))
  expect_error(
    validate_frame(ep_design("error", n = 1), frames),
    class = "samplyr_error_frame_incomplete_register"
  )
  expect_no_warning(execute(ep_design("silent", n = 1), frames, seed = 4))
})

test_that("the linearized export says what the empty parents change", {
  skip_if_not_installed("survey")
  s <- execute(
    ep_design(frac = 1), list(ep_eas(), ep_households(), ep_persons()),
    seed = 4
  )
  caught <- NULL
  withCallingHandlers(
    as_svydesign(s),
    samplyr_warning_export_empty_parent = function(w) {
      caught <<- w
      invokeRestart("muffleWarning")
    },
    samplyr_warning_lonely_psu = function(w) invokeRestart("muffleWarning")
  )
  expect_false(is.null(caught))
  expect_identical(caught$stage, 3L)
  expect_identical(caught$n_empty, attr(s, "metadata")$empty_parents[[1]]$n)

  # Replicates resample whole units, whose totals already hold the zeros.
  classes <- character()
  withCallingHandlers(
    as_svrepdesign(s, type = "bootstrap", replicates = 5),
    warning = function(w) {
      classes <<- c(classes, class(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_false("samplyr_warning_export_empty_parent" %in% classes)
})

test_that("summary() counts the parents a stage found empty", {
  s <- execute(
    ep_design(frac = 1), list(ep_eas(), ep_households(), ep_persons()),
    seed = 4
  )
  n_empty <- attr(s, "metadata")$empty_parents[[1]]$n
  out <- cli::ansi_strip(utils::capture.output(summary(s)))
  expect_true(any(grepl(
    paste0(n_empty, " selected parents with nothing to sample"), out,
    fixed = TRUE
  )))
})

test_that("replicates and continuations keep their own records", {
  frames <- list(ep_eas(), ep_households(), ep_persons())
  reps <- execute(ep_design(frac = 1), frames, seed = 4, reps = 3)
  records <- attr(reps, "metadata")$empty_parents
  expect_identical(
    sort(unique(vapply(records, function(r) r$replicate, integer(1)))),
    sort(unique(reps$.replicate[reps$.replicate %in% 1:3]))
  )

  two <- execute(
    ep_design(frac = 1), list(ep_eas(), ep_households()),
    stages = 1:2, seed = 4
  )
  expect_length(attr(two, "metadata")$empty_parents, 0L)
  three <- execute(two, ep_persons(), seed = 5)
  expect_length(attr(three, "metadata")$empty_parents, 1L)
})

test_that("the person total stays unbiased with empty households", {
  # Truth is 29 persons. Band of three SEs of the mean, fixed before the run.
  frames <- list(ep_eas(), ep_households(), ep_persons())
  reps <- execute(ep_design(frac = 1), frames, seed = 11, reps = 1000)
  totals <- tapply(reps$.weight, factor(reps$.replicate, levels = 1:1000), sum)
  totals[is.na(totals)] <- 0
  se <- stats::sd(totals) / sqrt(length(totals))
  expect_lt(abs(mean(totals) - nrow(ep_persons())), 3 * se)
})

test_that("records from earlier calls are carried once, not per replicate", {
  # EA 6 lists no households, so stage 2 finds it empty whenever it is drawn.
  households <- ep_households()
  households <- households[households$ea != 6, ]
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea) |> draw(n = 6) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2, on_empty = "silent") |>
    add_stage() |> draw(frac = 1, on_empty = "silent")
  two <- execute(design, list(ep_eas(), households), stages = 1:2, seed = 4)
  expect_identical(
    vapply(attr(two, "metadata")$empty_parents, function(r) r$stage, 1L),
    2L
  )

  three <- execute(two, ep_persons(), seed = 5)
  stages <- vapply(attr(three, "metadata")$empty_parents,
                   function(r) r$stage, 1L)
  expect_identical(sum(stages == 2L), 1L)
  expect_gt(sum(stages == 3L), 0L)

  replicated <- execute(two, ep_persons(), seed = 1, reps = 3)
  records <- attr(replicated, "metadata")$empty_parents
  stages <- vapply(records, function(r) r$stage, 1L)
  expect_identical(sum(stages == 2L), 1L)
  expect_setequal(
    vapply(records[stages == 3L], function(r) r$replicate, 1L),
    1:3
  )
  # A replicated input keeps its own records once and adds each replicate's.
  two_reps <- execute(
    design, list(ep_eas(), households), stages = 1:2, seed = 1, reps = 2
  )
  n_input <- length(attr(two_reps, "metadata")$empty_parents)
  expect_identical(n_input, 2L)
  continued <- execute(two_reps, ep_persons(), seed = 1)
  records <- attr(continued, "metadata")$empty_parents
  stages <- vapply(records, function(r) r$stage, 1L)
  expect_identical(sum(stages == 2L), n_input)
  expect_setequal(
    vapply(records[stages == 3L], function(r) r$replicate, 1L),
    1:2
  )
})
