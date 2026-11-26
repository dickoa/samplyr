# Extracted from test-survey-export.R:393

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
frame <- expand.grid(person = 1:4, hh = 1:4, ea = 1:6)
three_stage <- function(n) {
    sampling_design() |>
      stratify_by(st) |> cluster_by(ea) |> draw(n = n) |>
      add_stage() |> cluster_by(hh) |> draw(n = 2) |>
      add_stage() |> draw(n = 2)
  }
cases <- list(
    alone = list(a = 1, n = c(A = 1, B = 2), replicates = FALSE),
    pair = list(a = 1:2, n = c(A = 2, B = 2), replicates = TRUE),
    drawn = list(a = 1:2, n = c(A = 1, B = 2), replicates = TRUE)
  )
for (nm in names(cases)) {
    case <- cases[[nm]]
    frame$st <- ifelse(frame$ea %in% case$a, "A", "B")
    sample <- execute(three_stage(case$n), frame, seed = 3)
    mrb <- tryCatch(
      as_svrepdesign(sample, type = "mrbbootstrap", replicates = 20),
      samplyr_error_svrep_conversion_failed = function(e) NULL
    )
    expect_identical(!is_null(mrb), case$replicates, info = nm)

    messages <- character()
    for (type in c("JKn", "bootstrap", "subbootstrap", "auto")) {
      withCallingHandlers(
        as_svrepdesign(sample, type = type, replicates = 20),
        warning = function(w) {
          messages <<- c(messages, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    }
    advice <- grepl("rwyb", messages, fixed = TRUE)
    expect_true(any(advice), info = nm)
    expect_identical(
      unique(grepl("mrbbootstrap", messages[advice], fixed = TRUE)),
      case$replicates,
      info = nm
    )
  }
