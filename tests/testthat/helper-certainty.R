## Certainty-aware svyplan fixtures for the plan bridge. Deterministic, no
## RNG: the solver is iterative but seed-free, and the sizes are chosen so
## exactly one PSU per stratum is certainty with a wide margin around the
## threshold.

certainty_plan_register <- function() {
  data.frame(
    psu_id = c(sprintf("A%02d", 1:8), sprintf("B%02d", 1:8)),
    stratum = rep(c("A", "B"), each = 8),
    N = c(600, rep(200, 7), 450, 220, 165, rep(153, 5)),
    stringsAsFactors = FALSE
  )
}

certainty_plan_fixture <- function(psu = certainty_plan_register()) {
  frame <- data.frame(
    stratum = c("A", "B"), N = c(2000, 1600), n_per_psu = 10,
    stringsAsFactors = FALSE
  )
  measures <- data.frame(
    stratum = c("A", "B"), name = "y", p = c(0.5, 0.4), icc_psu = 0.05,
    stringsAsFactors = FALSE
  )
  targets <- data.frame(name = "y", cv = 0.10)
  svyplan::n_alloc(frame, measures = measures, targets = targets, psu = psu)
}

## The remainder of stratum B holds a 230-size PSU whose fielded inclusion
## probability is exactly one (5 * 230 / 1150), while svyplan's threshold
## leaves it noncertainty: the executable rule and the plan disagree.
certainty_disagreement_fixture <- function() {
  psu <- certainty_plan_register()
  psu$N[psu$psu_id %in% c("B02", "B03")] <- c(230, 155)
  certainty_plan_fixture(psu)
}

## One element row per ultimate unit, carrying the register's stratum, id,
## and size, so the register is the frame's own PSU structure. `sex`
## alternates within each PSU, for take stages that stratify within PSUs.
certainty_element_frame <- function(psu = certainty_plan_register()) {
  frame <- psu[rep(seq_len(nrow(psu)), psu$N), ]
  frame$person <- ave(frame$N, frame$psu_id, FUN = seq_along)
  frame$sex <- c("f", "m")[frame$person %% 2 + 1]
  rownames(frame) <- NULL
  frame
}
