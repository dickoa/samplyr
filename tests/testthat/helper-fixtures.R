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
