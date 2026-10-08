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

## A register large enough for zones. With two PSUs drawn per zone, the
## plan holds A01 and B01 certain and cuts A's remainder into 4 zones and
## B's into 2, each of 4 to 11 PSUs. Seed-free, like the unzoned fixture.
certainty_zone_register <- function() {
  data.frame(
    psu_id = c(sprintf("A%02d", 1:24), sprintf("B%02d", 1:18)),
    stratum = rep(c("A", "B"), c(24, 18)),
    N = c(400, 60 + 5 * (1:23), 300, 50 + 4 * (1:17)),
    stringsAsFactors = FALSE
  )
}

certainty_zone_fixture <- function(psu = certainty_zone_register(), m = 2) {
  frame <- data.frame(
    stratum = c("A", "B"), N = c(3160, 1762), n_per_psu = 8,
    stringsAsFactors = FALSE
  )
  measures <- data.frame(
    stratum = c("A", "B"), name = "y", p = c(0.5, 0.4), icc_psu = 0.05,
    stringsAsFactors = FALSE
  )
  targets <- data.frame(name = "y", cv = 0.12)
  svyplan::n_alloc(
    frame, measures = measures, targets = targets, psu = psu,
    n_psu_per_zone = m
  )
}

## The zone register with a small third stratum. Drawing one PSU per zone,
## A's remainder is cut into 5 zones grouped (1, 2) and (3, 4, 5), B's into
## 3, and C's into one, which has no partner inside C. So C's zone joins B's
## zones, and one variance group crosses strata. Groups hold 2, 3, 2 and 2
## zones.
certainty_pair_register <- function() {
  rbind(
    certainty_zone_register(),
    data.frame(
      psu_id = sprintf("C%02d", 1:6), stratum = "C", N = 40 + 3 * (1:6),
      stringsAsFactors = FALSE
    )
  )
}

certainty_pair_fixture <- function(psu = certainty_pair_register()) {
  frame <- stats::aggregate(N ~ stratum, psu, sum)
  frame$n_per_psu <- 8
  measures <- data.frame(
    stratum = c("A", "B", "C"), name = "y", p = c(0.5, 0.4, 0.3),
    icc_psu = 0.05, stringsAsFactors = FALSE
  )
  targets <- data.frame(name = "y", cv = 0.15)
  svyplan::n_alloc(
    frame, measures = measures, targets = targets, psu = psu,
    n_psu_per_zone = 1
  )
}

## A hash of a bridge sample's rows, weights, zones and certainty flags,
## in a fixed row order. Weights are rounded so the pin does not depend on
## the last bit of a division.
certainty_sample_key <- function(s) {
  d <- as.data.frame(s)
  cols <- grep(
    "^(psu_id|person|\\.weight(_[0-9]+)?|\\.(zone|pair|certainty)_[0-9]+)$",
    names(d),
    value = TRUE
  )
  d <- d[order(d$psu_id, d$person), cols]
  for (cn in grep("^\\.weight", cols, value = TRUE)) {
    d[[cn]] <- signif(d[[cn]], 12)
  }
  rownames(d) <- NULL
  rlang::hash(d)
}

## Each register PSU's planned stage-1 inclusion probability: one for
## certainty, the PSUs per zone times its share of its zone otherwise.
certainty_zone_pik <- function(plan) {
  psu <- plan$psu
  zone_key <- paste(psu$stratum, psu$.zone)
  ifelse(
    psu$certainty, 1,
    plan$params$n_psu_per_zone * psu$N / ave(psu$N, zone_key, FUN = sum)
  )
}

certainty_zone_design <- function(plan = certainty_zone_fixture(),
                                  method = "pps_brewer") {
  sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = method, mos = N) |>
    add_stage() |>
    draw(n = plan)
}

## One stratum whose plan draws a single PSU, so the whole design has one
## zone and its one variance group has no partner.
certainty_single_zone_fixture <- function() {
  psu <- data.frame(
    psu_id = sprintf("A%02d", 1:10), stratum = "A",
    N = c(400, 60 + 5 * (1:9)), stringsAsFactors = FALSE
  )
  frame <- data.frame(stratum = "A", N = sum(psu$N), n_per_psu = 8)
  measures <- data.frame(stratum = "A", name = "y", p = 0.5, icc_psu = 0.05)
  targets <- data.frame(name = "y", cv = 0.5)
  svyplan::n_alloc(
    frame, measures = measures, targets = targets, psu = psu,
    n_psu_per_zone = 1
  )
}
