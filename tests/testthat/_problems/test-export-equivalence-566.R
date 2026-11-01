# Extracted from test-export-equivalence.R:566

# prequel ----------------------------------------------------------------------
late_certainty_design <- function(balanced) {
  set.seed(if (balanced) 5 else 4)
  sizes <- if (balanced) rep(12L, 40) else sample(8:14, 40, replace = TRUE)
  frame <- data.frame(psu = rep(1:40, sizes))
  frame$eid <- seq_len(nrow(frame))
  if (balanced) {
    frame$x <- stats::ave(frame$eid, frame$psu, FUN = function(v) {
      c(20, 20, stats::runif(length(v) - 2, 3, 5))
    })
    frame$y <- frame$x + stats::rnorm(nrow(frame), 0, 2)
  } else {
    frame$x <- stats::ave(frame$eid, frame$psu, FUN = function(v) {
      c(60, 40, rep(3, length(v) - 2))
    })
    frame$y <- stats::ave(frame$eid, frame$psu, FUN = function(v) {
      c(500, 300, rep(0, length(v) - 2))
    }) + 0.5 * frame$x + stats::rnorm(nrow(frame), 0, 6)
  }
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = if (balanced) 40 else 10) |>
    add_stage() |>
    draw(n = 4, method = "pps_brewer", mos = x)
  list(frame = frame, design = design)
}
certainty_rep_fixture <- function(stages = 2L) {
  set.seed(20260925)
  psu <- data.frame(
    str = rep(1:3, each = 15),
    psu = 1:45,
    M = sample(20:60, 45, TRUE)
  )
  frame <- psu[rep(seq_len(nrow(psu)), psu$M), ]
  frame$eid <- seq_len(nrow(frame))
  frame$y <- 20 + 0.3 * frame$M + stats::rnorm(nrow(frame), 0, 15)
  design <- if (stages == 2L) {
    sampling_design() |>
      add_stage() |> stratify_by(str) |> cluster_by(psu) |>
      draw(n = 5, method = "pps_brewer", mos = M, certainty_size = 56) |>
      add_stage() |> draw(n = 4)
  } else {
    sampling_design() |>
      stratify_by(str) |>
      draw(n = 8, method = "pps_brewer", mos = M, certainty_size = 58)
  }
  frame <- if (stages == 2L) frame else psu_frame_one_stage(psu)
  list(frame = frame, design = design)
}
psu_frame_one_stage <- function(psu) {
  set.seed(3)
  psu$y <- 5 * psu$M + stats::rnorm(nrow(psu), 0, 20)
  psu
}
quiet_rep <- function(expr) {
  withCallingHandlers(
    expr,
    samplyr_warning_replicate_wr_first_stage = function(w) {
      invokeRestart("muffleWarning")
    }
  )
}
jkn_se <- function(design) {
  as.numeric(survey::SE(survey::svytotal(
    ~y, survey::as.svrepdesign(design, type = "JKn")
  )))
}
empty_psu_areas <- function() data.frame(psu = 1:4)
empty_psu_roster <- function() data.frame(psu = 2:4, id = 2:4, y = 10)
zero_rows <- function(svy) {
  data <- svy$variables
  data[is.na(data$.sample_id), , drop = FALSE]
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
psus <- data.frame(psu = 1:4)
dwellings <- data.frame(psu = rep(1:4, each = 2), dw = 1:8)
design <- sampling_design() |>
    cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> cluster_by(dw) |> draw(frac = 1) |>
    add_stage() |> draw(frac = 1, on_empty = "silent")
roster <- function(dw) {
    people <- data.frame(dw = dw, pid = dw, y = 10)
    people$psu <- dwellings$psu[people$dw]
    people
  }
variance <- function(totals) 4^2 * (1 - 3 / 4) * stats::var(totals) / 3
gone <- execute(design, list(psus, dwellings, roster(3:8)), seed = 1)
expect_false(1L %in% gone$psu)
svy <- as_svydesign(gone)
expect_identical(zero_rows(svy)$dw, 1:2)
expect_identical(zero_rows(svy)$psu, c(1L, 1L))
totals <- c(0, 20, 20)
expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy, na.rm = TRUE)))^2,
    variance(totals)
  )
thinned <- execute(design, list(psus, dwellings, roster(2:8)), seed = 1)
svy <- as_svydesign(thinned)
expect_identical(zero_rows(svy)$dw, 1L)
expect_equal(
    unname(survey::SE(survey::svytotal(~y, svy, na.rm = TRUE)))^2,
    variance(c(10, 20, 20))
  )
