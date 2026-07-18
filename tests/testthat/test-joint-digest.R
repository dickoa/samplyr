# Frame-free joint_expectation: the digest path must reproduce the
# frame path exactly wherever the digest keeps exact chances, and fail
# explicitly when exact chances are unavailable.

expect_jip_paths_equal <- function(s, frame, stage = NULL) {
  from_digest <- joint_expectation(s, stage = stage)
  from_frame <- joint_expectation(s, frame, stage = stage)
  expect_equal(from_digest, from_frame, tolerance = 1e-10)
  invisible(from_digest)
}

test_that("digest and frame paths agree for constant-chance stages", {
  s <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 5, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 11, frame_digest = "full")
  expect_jip_paths_equal(s, test_frame)

  # srswor is not a jip method; both paths return the same NULL shape.
  s2 <- sampling_design() |>
    draw(n = 10) |>
    execute(test_frame, seed = 12)
  expect_identical(joint_expectation(s2), joint_expectation(s2, test_frame))
})

test_that("digest and frame paths agree for element PPS (full mode)", {
  s <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 13, frame_digest = "full")
  expect_jip_paths_equal(s, test_frame)

  s_sampford <- sampling_design() |>
    draw(n = 8, method = "pps_sampford", mos = mos) |>
    execute(test_frame, seed = 14, frame_digest = "full")
  expect_jip_paths_equal(s_sampford, test_frame)
})

test_that("digest and frame paths agree with certainty selections", {
  frame_cert <- data.frame(
    id = 1:20,
    mos = c(5000, rep(c(20, 35, 50, 65), length.out = 19))
  )
  s <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = mos, certainty_size = 1000) |>
    execute(frame_cert, seed = 15, frame_digest = "full")
  jip <- expect_jip_paths_equal(s, frame_cert)
  expect_true(any(diag(jip[[1]]) >= 1 - 1e-9))
})

test_that("digest and frame paths agree for with-replacement hits", {
  s <- sampling_design() |>
    draw(n = 12, method = "pps_multinomial", mos = mos) |>
    execute(test_frame, seed = 16, frame_digest = "full")
  expect_jip_paths_equal(s, test_frame)
})

test_that("both paths give per-parent conditional stage-2 chances", {
  frame_jip <- expand.grid(
    district = c("A", "B"),
    ea = 1:5,
    KEEP.OUT.ATTRS = FALSE
  )
  frame_jip$pop <- rep(c(100, 200, 150, 80, 120), 2)
  s <- sampling_design() |>
    add_stage() |>
    cluster_by(district) |>
    draw(n = 2) |>
    add_stage() |>
    cluster_by(ea) |>
    draw(n = 3, method = "pps_brewer", mos = pop) |>
    execute(frame_jip, seed = 42)

  jip <- expect_jip_paths_equal(s, frame_jip, stage = 2)
  # The stage-2 chances are conditional per parent district: the
  # diagonal must reproduce the executed weights, not the pooled
  # approximation that merged both districts into one draw.
  sel <- dplyr::distinct(as.data.frame(s), district, ea, .weight_2)
  expect_equal(sort(diag(jip[[2]])), sort(1 / sel$.weight_2), tolerance = 1e-10)
  # Cross-district pairs are independent: pi_ij = pi_i * pi_j.
  expect_equal(
    jip[[2]][1, 4],
    jip[[2]][1, 1] * jip[[2]][4, 4],
    tolerance = 1e-10
  )
})

test_that("a stratified two-stage design agrees on both stages", {
  s <- sampling_design() |>
    add_stage("Clusters") |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    add_stage("Units") |>
    draw(n = 3, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 17, frame_digest = "full")
  expect_jip_paths_equal(s, test_frame)
})

test_that("summarized chances refuse instead of approximating", {
  s <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 18)
  expect_error(
    joint_expectation(s),
    class = "samplyr_error_digest_summarized"
  )
  # The frame path still works for the same sample.
  expect_silent(jip <- joint_expectation(s, test_frame))
  expect_true(is.matrix(jip[[1]]))
})

test_that("missing or invalidated digests refuse the frame-free path", {
  s_none <- sampling_design() |>
    draw(n = 10, method = "pps_brewer", mos = mos) |>
    execute(test_frame, seed = 19, frame_digest = "none")
  expect_error(joint_expectation(s_none), class = "samplyr_error_no_digest")
})
