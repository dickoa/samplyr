# Extracted from test-occasion-windows.R:371

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "samplyr", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
window_frame <- function() {
  data.frame(
    id = 1:600,
    h = rep(c("a", "b", "c"), each = 200),
    y = rep(c(10, 20, 30), each = 200) + (1:600) %% 7
  )
}
monthly_schedule <- function() {
  schedule <- expand.grid(panel = 1:6, period = 1:3)
  schedule$active <- (schedule$period - 1) %% 3 == (schedule$panel - 1) %% 3
  schedule
}
overlap_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    period = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
}
window_master <- function(n = 15, schedule = monthly_schedule(), seed = 1) {
  sampling_design() |>
    stratify_by(h) |>
    draw(n = n) |>
    execute(window_frame(), seed = seed, panels = schedule)
}
with_occasions <- function(window, unit) {
  df <- as.data.frame(window)
  at <- paste(df$.period, df$.subround)
  occasions <- tapply(at, df[[unit]], function(w) {
    paste(sort(unique(w)), collapse = ", ")
  })
  df$occasions <- as.vector(occasions[as.character(df[[unit]])])
  df
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
withr::local_options(survey.lonely.psu = "fail")
eas <- expand.grid(hh = 1:4, psu = 1:9)
eas$mos <- ifelse(eas$psu == 1L, 1000, 10)
eas$y <- (eas$hh * 3 + eas$psu) %% 7
elements <- data.frame(id = 1:30, mos = c(1000, rep(10, 29)))
elements$y <- (elements$id * 7) %% 11
schedule <- data.frame(
    period = c(1, 1, 2, 2), panel = c(1, 2, 1, 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
masters <- suppressMessages(list(
    psu = sampling_design() |> cluster_by(psu) |>
      draw(n = 5, method = "pps_brewer", mos = mos) |>
      execute(eas, seed = 1, panels = schedule),
    .sample_id = sampling_design() |>
      draw(n = 7, method = "pps_brewer", mos = mos) |>
      execute(elements, seed = 1, panels = schedule)
  ))
for (unit in names(masters)) {
    master <- masters[[unit]]
    expect_identical(unique(master[[unit]][master$.certainty_1]), 1L)
    for (occasions in list(1, 1:2)) {
      window <- execute(master, period = occasions)
      df <- if (length(occasions) > 1L) {
        with_occasions(window, unit)
      } else {
        transform(as.data.frame(window), occasions = "1")
      }
      # The unit is the same in every sample, so the others give the whole
      # variance. In a window it would stand alone among its occasions.
      reference <- survey::svydesign(
        ids = stats::as.formula(paste0("~", unit)),
        strata = ~occasions,
        weights = ~.weight,
        data = df[!df$.certainty_1, ]
      )
      se <- as.vector(survey::SE(survey::svytotal(~y, reference)))
      info <- paste(unit, length(occasions))
      expect_no_warning(svy <- as_svydesign(window))
      expect_equal(as.vector(survey::SE(survey::svytotal(~y, svy))), se,
                   info = info)
      jkn <- as_svrepdesign(window, type = "JKn")
      expect_equal(as.vector(survey::SE(survey::svytotal(~y, jkn))), se,
                   info = info)
      boot <- as_svrepdesign(window, type = "subbootstrap", replicates = 20)
      sure <- boot$variables$.certainty_1
      expect_equal(
        unname(weights(boot, type = "analysis")[sure, , drop = FALSE]),
        matrix(boot$variables$.weight[sure], sum(sure), 20),
        info = info
      )
    }
  }
