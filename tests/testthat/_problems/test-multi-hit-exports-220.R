# Extracted from test-multi-hit-exports.R:220

# prequel ----------------------------------------------------------------------
multi_hit_draw <- function(design, method, ...) {
  if (identical(method, "srswr")) {
    draw(design, method = method, ...)
  } else {
    draw(design, method = method, mos = mos, ...)
  }
}
multi_hit <- c("srswr", "pps_multinomial", "pps_chromy", "pps_systematic_mr")
warning_classes <- function(expr) {
  seen <- character(0)
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) e),
    warning = function(w) {
      seen <<- c(seen, class(w)[[1]])
      invokeRestart("muffleWarning")
    }
  )
  list(warnings = seen, error = if (inherits(value, "error")) class(value)[[1]])
}
estimator_classes <- function(rows, estimator) {
  strsplit(rows$class[rows$estimator == estimator], ", ", fixed = TRUE)[[1]]
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
frame <- data.frame(id = 1:20, mos = c(8, rep(1, 19)), y = 1:20)
phase2 <- sampling_design() |> draw(n = 2)
outcomes <- character(0)
for (method in multi_hit) {
    design1 <- multi_hit_draw(cluster_by(sampling_design(), id), method, n = 8)
    phase1 <- execute(design1, frame, seed = 3)
    expect_true(anyDuplicated(phase1$id) > 0)
    expect_true(length(unique(phase1$id)) >= 4)
    rows <- variance_estimators(phase2, phase1)
    linearization <- rows[rows$estimator == "Taylor linearization", ]
    expect_false(linearization$status == "refused")
    expect_match(
      linearization$sample_risk, "samplyr_error_twophase_bridge", fixed = TRUE
    )
    # The draw decides: phase 2 refuses exactly when it reaches a repeated
    # unit, and otherwise links one phase-1 row per phase-2 row.
    for (seed in 1:20) {
      s <- execute(phase2, phase1, seed = seed)
      reached <- any(s$id %in% phase1$id[duplicated(phase1$id)])
      svy <- tryCatch(
        suppressWarnings(
          as_svydesign(s, systematic_variance = "approximate")
        ),
        samplyr_error_twophase_bridge = function(e) NULL
      )
      expect_identical(is_null(svy), reached)
      if (!is_null(svy)) {
        # One phase-1 cluster per hit, so every hit is its own draw.
        expect_identical(
          length(unique(svy$phase1$full$cluster[[1]])), nrow(phase1)
        )
      }
      outcomes <- c(outcomes, paste(method, if (reached) "refused" else "ok"))
    }
  }
expect_setequal(
    outcomes, paste(rep(multi_hit, each = 2), c("refused", "ok"))
  )
