# Shared checks for test-export-equivalence.R: exported weights equal `.weight`,
# the total equals sum(.weight * y) and the reference, the variance equals the
# reference variance, and the design has the expected number of stages.

# survey keeps the per-stage cluster identifiers as the columns of `$cluster`.
# A twophase2 object holds one design per phase, so it reports two counts.
export_stage_count <- function(svy) {
  if (inherits(svy, "twophase2")) {
    return(c(
      NCOL(svy$phase1$sample$cluster),
      NCOL(svy$phase2$cluster)
    ))
  }
  NCOL(svy$cluster)
}

expect_export_invariants <- function(svy, sample, y, reference = NULL,
                                     stages = NULL, tolerance = 1e-8) {
  f <- stats::reformulate(y)
  total <- survey::svytotal(f, svy)
  df <- as.data.frame(sample)

  # A replicate design returns replicate weights unless asked for sampling ones.
  w <- if (inherits(svy, "svyrep.design")) {
    stats::weights(svy, type = "sampling")
  } else {
    stats::weights(svy)
  }
  expect_equal(unname(w), unname(df$.weight))
  expect_equal(unname(coef(total)), sum(df$.weight * df[[y]]))

  if (!is.null(reference)) {
    # A survey design is also a list, so the reference is recognised by names.
    ref <- if (setequal(names(reference), c("total", "variance"))) {
      reference
    } else {
      ref_total <- survey::svytotal(f, reference)
      list(
        total = unname(coef(ref_total)),
        variance = unname(vcov(ref_total))[1, 1]
      )
    }
    expect_equal(unname(coef(total)), ref$total, tolerance = tolerance)
    expect_equal(
      unname(vcov(total))[1, 1],
      ref$variance,
      tolerance = tolerance
    )
  }
  if (!is.null(stages)) {
    expect_identical(as.integer(export_stage_count(svy)), as.integer(stages))
  }
  invisible(total)
}

# Monte Carlo ratio of the mean estimated variance of a total to its
# empirical variance, over fixed seeds, so the result is deterministic.
# `export` turns a sample into an exported design.
mc_variance_ratio <- function(design, frame, y, reps, export, seed0 = 1000L) {
  f <- stats::reformulate(y)
  draws <- vapply(seq_len(reps), function(r) {
    s <- suppressWarnings(execute(design, frame, seed = seed0 + r))
    t <- survey::svytotal(f, export(s))
    c(unname(coef(t)), unname(vcov(t))[1, 1])
  }, numeric(2))
  est <- draws[1, ]
  list(
    ratio = mean(draws[2, ]) / stats::var(est),
    bias_z = (mean(est) - sum(frame[[y]])) / (stats::sd(est) / sqrt(reps))
  )
}

# A phase 2 drawn across the phase-1 units warns at export. The warning is
# asserted in test-export-equivalence.R, so tests about something else mute it.
quiet_across <- function(expr) {
  withCallingHandlers(
    expr,
    samplyr_warning_twophase_across_units = function(w) {
      invokeRestart("muffleWarning")
    }
  )
}
