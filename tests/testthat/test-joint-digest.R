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

test_that("multi-hit WR matrices use distinct units in first appearance order", {
  frame <- data.frame(
    id = 1:5,
    mos = c(1, 2, 3, 4, 5)
  )
  n <- 10L
  s <- sampling_design() |>
    draw(n = n, method = "pps_multinomial", mos = mos) |>
    execute(frame, seed = 1, frame_digest = "full")

  sampled_ids <- unique(s$id)
  expect_lt(length(sampled_ids), nrow(s))
  from_frame <- joint_expectation(s, frame)[[1]]
  from_digest <- joint_expectation(s)[[1]]
  expected_dim <- rep(length(sampled_ids), 2)
  expect_identical(dim(from_frame), expected_dim)
  expect_identical(dim(from_digest), expected_dim)

  p <- frame$mos / sum(frame$mos)
  expected_diag <- n * (n - 1) * p^2 + n * p
  expect_equal(
    diag(from_frame),
    expected_diag[sampled_ids],
    tolerance = 1e-10
  )
  expect_equal(
    diag(from_digest),
    expected_diag[sampled_ids],
    tolerance = 1e-10
  )
  expect_equal(from_digest, from_frame, tolerance = 1e-10)
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

test_that("WR parent occurrences define separate child-stage blocks", {
  frame <- data.frame(
    psu = rep(1:2, each = 5),
    unit = rep(1:5, 2),
    psu_mos = rep(c(1, 2), each = 5),
    unit_mos = rep(c(1, 2, 3, 4, 5), 2)
  )
  s <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = psu_mos) |>
    add_stage() |>
    draw(n = 2, method = "pps_brewer", mos = unit_mos) |>
    execute(frame, seed = 7, frame_digest = "full")

  sample_df <- as.data.frame(s)
  expect_lt(dplyr::n_distinct(sample_df$psu), 3L)
  expect_identical(dplyr::n_distinct(sample_df$.draw_1), 3L)
  from_frame <- joint_expectation(s, frame, stage = 2)[[2]]
  from_digest <- joint_expectation(s, stage = 2)[[2]]
  expect_identical(dim(from_frame), c(nrow(s), nrow(s)))
  expect_identical(dim(from_digest), c(nrow(s), nrow(s)))
  expect_equal(from_frame, from_digest, tolerance = 1e-9)
  expect_equal(diag(from_frame), 1 / sample_df$.weight_2, tolerance = 1e-9)

  parent_draws <- dplyr::distinct(sample_df, psu, .draw_1)
  repeated_psu <- parent_draws$psu[duplicated(parent_draws$psu)][[1]]
  repeated_rows <- which(sample_df$psu == repeated_psu)
  occurrence <- sample_df$.draw_1[repeated_rows]
  first_by_occurrence <- repeated_rows[!duplicated(occurrence)]
  i <- first_by_occurrence[[1]]
  j <- first_by_occurrence[[2]]
  expect_equal(
    from_frame[i, j],
    from_frame[i, i] * from_frame[j, j],
    tolerance = 1e-9
  )
})

test_that("a single-hit WR parent preserves its child-stage matrix", {
  frame <- data.frame(
    psu = rep(1:2, each = 5),
    unit = rep(1:5, 2),
    psu_mos = rep(c(1, 2), each = 5),
    unit_mos = rep(c(1, 2, 3, 4, 5), 2)
  )
  s <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 1, method = "pps_multinomial", mos = psu_mos) |>
    add_stage() |>
    draw(n = 2, method = "pps_brewer", mos = unit_mos) |>
    execute(frame, seed = 31, frame_digest = "full")

  from_frame <- joint_expectation(s, frame, stage = 2)[[2]]
  from_digest <- joint_expectation(s, stage = 2)[[2]]
  expect_identical(dim(from_frame), c(2L, 2L))
  expect_equal(from_frame, from_digest, tolerance = 1e-9)
  expect_equal(diag(from_frame), 1 / s$.weight_2, tolerance = 1e-9)
})

test_that("nested WR ancestry uses every prior draw column", {
  frame <- expand.grid(
    unit = 1:4,
    psu = 1:2,
    region = 1:2,
    KEEP.OUT.ATTRS = FALSE
  )
  frame <- frame[order(frame$region, frame$psu, frame$unit), ]
  frame$region_mos <- ifelse(frame$region == 1, 1, 2)
  frame$psu_mos <- ifelse(frame$psu == 1, 1, 2)
  frame$unit_mos <- frame$unit
  s <- sampling_design() |>
    add_stage() |>
    cluster_by(region) |>
    draw(n = 3, method = "pps_multinomial", mos = region_mos) |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = psu_mos) |>
    add_stage() |>
    draw(n = 2, method = "pps_brewer", mos = unit_mos) |>
    execute(frame, seed = 8, frame_digest = "full")

  sample_df <- as.data.frame(s)
  occurrences <- dplyr::distinct(
    sample_df, region, psu, .draw_1, .draw_2
  )
  expect_identical(nrow(occurrences), 9L)
  from_frame <- joint_expectation(s, frame, stage = 3)[[3]]
  from_digest <- joint_expectation(s, stage = 3)[[3]]
  expect_identical(dim(from_frame), c(nrow(s), nrow(s)))
  expect_identical(dim(from_digest), c(nrow(s), nrow(s)))
  expect_equal(from_frame, from_digest, tolerance = 1e-9)
  expect_equal(diag(from_frame), 1 / sample_df$.weight_3, tolerance = 1e-9)
})

test_that("clustered children deduplicate within each WR parent occurrence", {
  frame <- expand.grid(
    element = 1:2,
    child = 1:4,
    parent = 1:2,
    KEEP.OUT.ATTRS = FALSE
  )
  frame <- frame[order(frame$parent, frame$child, frame$element), ]
  frame$parent_mos <- ifelse(frame$parent == 1, 1, 2)
  frame$child_mos <- frame$child
  s <- sampling_design() |>
    add_stage() |>
    cluster_by(parent) |>
    draw(n = 3, method = "pps_multinomial", mos = parent_mos) |>
    add_stage() |>
    cluster_by(child) |>
    draw(n = 2, method = "pps_brewer", mos = child_mos) |>
    execute(frame, seed = 24, frame_digest = "full")

  sample_df <- as.data.frame(s)
  selected_children <- dplyr::distinct(
    sample_df, parent, .draw_1, child, .weight_2
  )
  child_blocks <- samplyr:::split_row_indices(
    selected_children, c("parent", ".draw_1")
  )
  selected_children <- selected_children[
    unlist(child_blocks$indices, use.names = FALSE), , drop = FALSE
  ]
  expect_identical(nrow(selected_children), 6L)
  expect_gt(nrow(sample_df), nrow(selected_children))

  from_frame <- joint_expectation(s, frame, stage = 2)[[2]]
  from_digest <- joint_expectation(s, stage = 2)[[2]]
  expect_identical(dim(from_frame), c(6L, 6L))
  expect_equal(from_frame, from_digest, tolerance = 1e-9)
  expect_equal(
    diag(from_frame), 1 / selected_children$.weight_2, tolerance = 1e-9
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

test_that("stratum blocks follow first sample appearance", {
  frame <- data.frame(
    id = 1:12,
    stratum = rep(c("zeta", "alpha"), each = 6),
    mos = rep(c(1, 2, 3, 4, 5, 6), 2)
  )
  s <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    execute(frame, seed = 21, frame_digest = "full")

  from_frame <- joint_expectation(s, frame)[[1]]
  from_digest <- joint_expectation(s)[[1]]
  expect_equal(from_frame, from_digest, tolerance = 1e-9)
  expect_equal(diag(from_frame), 1 / s$.weight_1, tolerance = 1e-9)
  expect_identical(as.character(s$stratum[1]), "zeta")
})

test_that("compound strata below parents follow first sample appearance", {
  frame <- expand.grid(
    unit = 1:5,
    stratum_b = c("9", "10"),
    parent = c("P2", "P1"),
    KEEP.OUT.ATTRS = FALSE
  )
  frame <- frame[order(
    match(frame$parent, c("P2", "P1")),
    match(frame$stratum_b, c("9", "10")),
    frame$unit
  ), ]
  frame$stratum_a <- ifelse(frame$stratum_b == "9", "zeta", "alpha")
  frame$mos <- ifelse(frame$stratum_b == "9", frame$unit, frame$unit^2)
  s <- sampling_design() |>
    add_stage() |>
    cluster_by(parent) |>
    draw(n = 2) |>
    add_stage() |>
    stratify_by(stratum_a, stratum_b) |>
    draw(n = 2, method = "pps_brewer", mos = mos) |>
    execute(frame, seed = 22, frame_digest = "full")

  from_frame <- joint_expectation(s, frame, stage = 2)[[2]]
  from_digest <- joint_expectation(s, stage = 2)[[2]]
  expect_equal(from_frame, from_digest, tolerance = 1e-9)
  expect_equal(diag(from_frame), 1 / s$.weight_2, tolerance = 1e-9)
  first_stratum_by_parent <- dplyr::distinct(
    as.data.frame(s), parent, stratum_a, stratum_b
  ) |>
    dplyr::group_by(parent) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()
  expect_true(all(first_stratum_by_parent$stratum_a == "zeta"))
})

test_that("stratified WR blocks and units use first appearance order", {
  frame <- data.frame(
    id = 1:8,
    stratum = rep(c("zeta", "alpha"), each = 4),
    mos = rep(c(1, 2, 3, 4), 2)
  )
  n <- 6L
  s <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = n, method = "pps_multinomial", mos = mos) |>
    execute(frame, seed = 23, frame_digest = "full")

  sampled_ids <- unique(s$id)
  from_frame <- joint_expectation(s, frame)[[1]]
  from_digest <- joint_expectation(s)[[1]]
  expect_identical(dim(from_frame), rep(length(sampled_ids), 2))
  expect_equal(from_frame, from_digest, tolerance = 1e-9)

  p <- frame$mos / ave(frame$mos, frame$stratum, FUN = sum)
  expected_diag <- n * (n - 1) * p^2 + n * p
  expect_equal(diag(from_frame), expected_diag[sampled_ids], tolerance = 1e-9)
  expect_identical(as.character(s$stratum[1]), "zeta")
})

test_that("twelve non-WR design shapes retain frame-digest parity", {
  element_frame <- data.frame(
    id = 1:40,
    stratum = rep(c("zeta", "alpha"), each = 20),
    mos = seq_len(40)
  )
  cluster_frame <- data.frame(
    cluster = rep(1:10, each = 4),
    unit = rep(1:4, 10),
    cluster_mos = rep(seq_len(10), each = 4),
    unit_mos = rep(c(1, 2, 3, 4), 10)
  )
  certainty_frame <- data.frame(
    id = 1:20,
    mos = c(5000, rep(c(20, 35, 50, 65), length.out = 19))
  )

  cases <- list(
    brewer = list(
      sampling_design() |>
        draw(n = 8, method = "pps_brewer", mos = mos),
      element_frame,
      NULL
    ),
    systematic = list(
      sampling_design() |>
        draw(n = 8, method = "pps_systematic", mos = mos),
      element_frame,
      NULL
    ),
    cps = list(
      sampling_design() |>
        draw(n = 8, method = "pps_cps", mos = mos),
      element_frame,
      NULL
    ),
    sampford = list(
      sampling_design() |>
        draw(n = 8, method = "pps_sampford", mos = mos),
      element_frame,
      NULL
    ),
    poisson = list(
      sampling_design() |>
        draw(n = 8, method = "pps_poisson", mos = mos),
      element_frame,
      NULL
    ),
    sps = list(
      sampling_design() |>
        draw(n = 8, method = "pps_sps", mos = mos),
      element_frame,
      NULL
    ),
    pareto = list(
      sampling_design() |>
        draw(n = 8, method = "pps_pareto", mos = mos),
      element_frame,
      NULL
    ),
    cube = list(
      sampling_design() |>
        draw(n = 8, method = "cube", mos = mos),
      element_frame,
      NULL
    ),
    stratified_brewer = list(
      sampling_design() |>
        stratify_by(stratum) |>
        draw(n = 4, method = "pps_brewer", mos = mos),
      element_frame,
      NULL
    ),
    clustered_brewer = list(
      sampling_design() |>
        cluster_by(cluster) |>
        draw(n = 4, method = "pps_brewer", mos = cluster_mos),
      cluster_frame,
      NULL
    ),
    stage_2_brewer = list(
      sampling_design() |>
        add_stage() |>
        cluster_by(cluster) |>
        draw(n = 4) |>
        add_stage() |>
        draw(n = 2, method = "pps_brewer", mos = unit_mos),
      cluster_frame,
      2L
    ),
    certainty_brewer = list(
      sampling_design() |>
        draw(
          n = 5,
          method = "pps_brewer",
          mos = mos,
          certainty_size = 1000
        ),
      certainty_frame,
      NULL
    )
  )

  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    sample <- execute(
      case[[1]], case[[2]], seed = 100, frame_digest = "full"
    )
    from_frame <- joint_expectation(
      sample, case[[2]], stage = case[[3]]
    )
    from_digest <- joint_expectation(sample, stage = case[[3]])
    expect_equal(
      from_digest,
      from_frame,
      tolerance = 1e-9,
      info = case_name
    )
  }
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
