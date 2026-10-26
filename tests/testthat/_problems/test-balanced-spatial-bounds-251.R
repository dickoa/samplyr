# Extracted from test-balanced-spatial-bounds.R:251

# prequel ----------------------------------------------------------------------
balanced_feature_frame <- data.frame(
  id = seq_len(120),
  mos = rep(seq_len(30), 4),
  income = sin(seq_len(120) / 7),
  region = factor(rep(letters[1:4], each = 30)),
  urban = rep(c(FALSE, TRUE), 60),
  lon = rep(seq_len(12), 10),
  lat = rep(seq_len(10), each = 12)
)

# test -------------------------------------------------------------------------
design <- sampling_design() |>
    draw(n = 20, method = "pps_sampford", mos = mos)
spec <- design$stages[[1]]$draw_spec
expect_null(spec$method_type)
expect_true(spec$method %in% pps_wor_methods)
result <- execute(design, balanced_feature_frame, seed = 31, reps = 2)
expect_equal(as.integer(table(result$.replicate)), c(20L, 20L))
one <- dplyr::filter(result, .replicate == 1)
jip <- joint_expectation(one, balanced_feature_frame)$stage_1
