# Extracted from test-empty-parents.R:145

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
frames <- list(ep_eas(), ep_households(), ep_persons())
caught <- function(export) {
    out <- NULL
    withCallingHandlers(
      export(),
      samplyr_warning_export_empty_parent = function(w) {
        out <<- w
        invokeRestart("muffleWarning")
      },
      warning = function(w) invokeRestart("muffleWarning")
    )
    out
  }
loud <- suppressWarnings(
    execute(ep_design("warn", frac = 1), frames, seed = 4)
  )
n_empty <- nrow(attr(loud, "metadata")$empty_parents[[1]]$rows)
expect_gt(n_empty, 0L)
for (export in list(
    function() as_svydesign(loud),
    function() as_svrepdesign(loud, type = "bootstrap", replicates = 5)
  )) {
    w <- caught(export)
    expect_identical(w$n_empty, n_empty)
    expect_match(conditionMessage(w), "na.rm = TRUE", fixed = TRUE)
  }
