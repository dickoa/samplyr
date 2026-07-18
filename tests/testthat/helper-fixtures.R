# Shared test fixtures for test-survey-export.R
#
# A small synthetic frame (120 rows, 4 strata, 24 clusters) used
# instead of bfa_eas (14,900 rows) or zwe_eas (22,600 rows) for
# structural tests that do not depend on frame size.

set.seed(20260217)
n_clusters <- 24L
cluster_mos <- round(runif(n_clusters, 5, 200))
test_frame <- data.frame(
  id = sprintf("u%03d", 1:120),
  stratum = factor(rep(c("A", "B", "C", "D"), each = 30)),
  cluster = factor(rep(sprintf("cl%02d", 1:n_clusters), each = 5)),
  mos = rep(cluster_mos, each = 5),
  y = rnorm(120)
)

# --- Pre-computed fixtures (executed once at test load) ---

# Stratified proportional, n = 40 (10 per stratum)
fix_strat_prop <- sampling_design() |>
  stratify_by(stratum, alloc = "proportional") |>
  draw(n = 40) |>
  execute(test_frame, seed = 42)

# Unstratified SRS, n = 20
fix_srs <- sampling_design() |>
  draw(n = 20) |>
  execute(test_frame, seed = 1)

# Two-stage: stratified PPS brewer clusters, then SRS units
fix_multistage <- sampling_design() |>
  add_stage(label = "Clusters") |>
  stratify_by(stratum) |>
  cluster_by(cluster) |>
  draw(n = 2, method = "pps_brewer", mos = mos) |>
  add_stage(label = "Units") |>
  draw(n = 3) |>
  execute(test_frame, seed = 2025)

# Stratified PPS brewer with cluster_by (single-stage)
fix_strat_pps <- sampling_design() |>
  stratify_by(stratum, alloc = "proportional") |>
  cluster_by(cluster) |>
  draw(n = 12, method = "pps_brewer", mos = mos) |>
  execute(test_frame, seed = 2025)

# PPS brewer unstratified, n = 10
fix_pps_brewer <- sampling_design() |>
  draw(n = 10, method = "pps_brewer", mos = mos) |>
  execute(test_frame, seed = 42)

# SRSWR, n = 10
fix_srswr <- sampling_design() |>
  draw(n = 10, method = "srswr") |>
  execute(test_frame, seed = 1)

# PPS SPS, n = 10
fix_pps_sps <- sampling_design() |>
  draw(n = 10, method = "pps_sps", mos = mos) |>
  execute(test_frame, seed = 42)

# PPS Pareto, n = 10
fix_pps_pareto <- sampling_design() |>
  draw(n = 10, method = "pps_pareto", mos = mos) |>
  execute(test_frame, seed = 42)

# PPS multinomial (WR), n = 8
fix_pps_multinomial <- sampling_design() |>
  draw(n = 8, method = "pps_multinomial", mos = mos) |>
  execute(test_frame, seed = 42)

# PPS Chromy (WR), n = 8
fix_pps_chromy <- sampling_design() |>
  draw(n = 8, method = "pps_chromy", mos = mos) |>
  execute(test_frame, seed = 42)

# Synthetic three-stage frame: 3 regions x 4 districts, villages per
# district cycling 4-7 with alternating phc strata, compounds per
# village cycling 2-12. Purely arithmetic (no RNG, no stored data):
# the frame, and every count derived from it, is reproducible from
# this code alone. Villages with at most 3 compounds become stage-3
# whole takes under the reference design (n = 3).
synth_three_stage_frame <- function() {
  vil_counts <- c(4L, 5L, 6L, 7L)
  comp_cycle <- c(2L, 4L, 6L, 9L, 12L, 3L, 8L)
  rows <- list()
  v_global <- 0L
  for (r in 1:3) {
    for (d in 1:4) {
      district <- sprintf("d%02d", (r - 1L) * 4L + d)
      for (v in seq_len(vil_counts[d])) {
        v_global <- v_global + 1L
        n_comp <- comp_cycle[(v_global - 1L) %% 7L + 1L]
        rows[[v_global]] <- data.frame(
          region = c("East", "Central", "West")[r],
          district = district,
          phc = if (v %% 2L == 0L) "yes" else "no",
          village = sprintf("%s-v%02d", district, v),
          compound = seq_len(n_comp)
        )
      }
    }
  }
  frame <- do.call(rbind, rows)
  frame$village_pop <- as.integer(
    ave(frame$compound, frame$village, FUN = length)
  )
  frame$district_pop <- as.integer(
    ave(frame$compound, frame$district, FUN = length)
  )
  frame
}

# The reference three-stage design over synth_three_stage_frame().
synth_three_stage_design <- function() {
  sampling_design("Synthetic three-stage") |>
    add_stage("Districts") |> stratify_by(region) |>
    cluster_by(district) |>
    draw(n = 2, method = "pps_systematic", mos = district_pop) |>
    add_stage("Villages") |> stratify_by(phc) |> cluster_by(village) |>
    draw(n = 2, method = "pps_systematic", mos = village_pop) |>
    add_stage("Compounds") |> draw(n = 3)
}
