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

## The default register with two stratum B sizes changed. Its fitted plan
## is a different plan from the default one.
certainty_altered_register <- function() {
  psu <- certainty_plan_register()
  psu$N[psu$psu_id %in% c("B02", "B03")] <- c(230, 155)
  psu
}

## The plan an older svyplan fitted on the altered register, which held B02
## noncertainty and drew 5 of stratum B's remaining 7 PSUs. B02's fielded
## inclusion probability is then exactly one (5 * 230 / 1150), so the
## executable rule and the plan disagree at the boundary. svyplan now
## classifies certainty under the draw it fields and flags B02, but a
## stored, edited or older plan can still carry this split.
certainty_disagreement_fixture <- function() {
  plan <- certainty_plan_fixture(certainty_altered_register())
  b <- plan$detail$stratum == "B"
  plan$psu$certainty[plan$psu$psu_id == "B02"] <- FALSE
  plan$detail$n_psu_certain[b] <- 1
  plan$detail$n_psu_draw[b] <- 5
  plan
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
