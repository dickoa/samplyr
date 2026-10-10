## joint_expectation() from the frame reproduces the executed selection

# The frame path recomputes each pool from the frame, under `control` and the
# executed certainty remainder. The digest path reads what execute() recorded,
# so under frame_digest = "full" the two agree exactly.

joint_frame_fixture <- function() {
  set.seed(7)
  frame <- data.frame(
    psu = rep(1:12, each = 2),
    st = rep(c("a", "b"), each = 12)
  )
  frame$id <- seq_len(nrow(frame))
  frame$size <- round(stats::runif(nrow(frame), 1, 10), 1)
  frame$key <- sample(nrow(frame))
  frame$csize <- stats::ave(frame$size, frame$psu, FUN = function(x) x[1])
  frame$ckey <- stats::ave(frame$key, frame$psu, FUN = function(x) x[1])
  frame
}

# `control` is injected as one expression (NULL for none), so every spelling
# is tested, including the desc() and serp() markers.
joint_designs <- function(method, control, structure) {
  ctl <- control
  switch(
    structure,
    plain = sampling_design() |>
      draw(n = 6, method = method, mos = size, control = !!ctl),
    stratified = sampling_design() |>
      stratify_by(st) |>
      draw(n = 3, method = method, mos = size, control = !!ctl),
    cluster = sampling_design() |>
      cluster_by(psu) |>
      draw(n = 4, method = method, mos = csize, control = !!ctl),
    stage_two = sampling_design() |>
      add_stage() |>
      cluster_by(psu) |>
      draw(n = 6) |>
      add_stage() |>
      draw(n = 1, method = method, mos = size, control = !!ctl),
    certainty = sampling_design() |>
      draw(
        n = 6, method = method, mos = size, control = !!ctl,
        certainty_size = 9
      )
  )
}

joint_gap <- function(design, frame) {
  sample <- suppressWarnings(
    execute(design, frame, seed = 3, frame_digest = "full")
  )
  from_frame <- joint_expectation(sample, frame)
  from_digest <- joint_expectation(sample)
  stages <- which(!vapply(from_frame, is.null, logical(1)))
  expect_gt(length(stages), 0L)
  max(vapply(stages, function(k) {
    max(abs(from_frame[[k]] - from_digest[[k]]))
  }, numeric(1)))
}

test_that("the frame path equals the digest path under every control", {
  frame <- joint_frame_fixture()
  controls <- list(
    none = NULL,
    key = quote(key),
    desc = quote(desc(size)),
    serp = quote(serp(st, key))
  )
  structures <- c("plain", "stratified", "cluster", "stage_two", "certainty")
  # Order-dependent methods, where the order is the design.
  for (method in c("pps_systematic", "pps_chromy")) {
    for (ctl in names(controls)) {
      for (structure in structures) {
        if (structure == "certainty" && method == "pps_chromy") next
        ctl_use <- controls[[ctl]]
        if (structure == "cluster" && ctl != "none") {
          ctl_use <- quote(ckey)
        }
        gap <- joint_gap(joint_designs(method, ctl_use, structure), frame)
        expect_lt(gap, 1e-12, label = paste(method, ctl, structure))
      }
    }
  }
  # Order-free methods, whose matrix must not change with `control`.
  for (method in c("pps_brewer", "pps_sampford", "pps_cps")) {
    for (structure in structures) {
      ctl_use <- if (structure == "cluster") quote(ckey) else controls$key
      gap <- joint_gap(joint_designs(method, ctl_use, structure), frame)
      expect_lt(gap, 1e-12, label = paste(method, structure))
    }
  }
})

test_that("a controlled pps_systematic matrix is the sorted order's", {
  skip_if_not_installed("sampling")
  frame <- joint_frame_fixture()
  design <- sampling_design() |>
    draw(n = 6, method = "pps_systematic", mos = size, control = key)
  sample <- execute(design, frame, seed = 3)
  jip <- joint_expectation(sample, frame)[[1]]

  # Systematic PPS on the frame sorted by `key`, computed by `sampling`.
  sorted <- frame[order(frame$key), ]
  pik <- sampling::inclusionprobabilities(sorted$size, 6)
  oracle <- sampling::UPsystematicpi2(pik)
  pos <- match(sample$id, sorted$id)
  expect_equal(jip, oracle[pos, pos], ignore_attr = TRUE, tolerance = 1e-12)

  # The unsorted frame gives a different matrix.
  unsorted <- sampling::UPsystematicpi2(
    sampling::inclusionprobabilities(frame$size, 6)
  )
  at <- match(sample$id, frame$id)
  expect_gt(max(abs(jip - unsorted[at, at])), 0.1)
})

test_that("a random-size certainty remainder has the executed chances", {
  set.seed(2)
  frame <- data.frame(
    id = 1:100,
    x = c(80, 60, stats::rgamma(98, 2, scale = 3) + 1),
    st = rep(c("a", "b"), 50)
  )
  designs <- list(
    frac_size = sampling_design() |>
      draw(frac = 0.1, method = "pps_poisson", mos = x, certainty_size = 50),
    frac_prop = sampling_design() |>
      draw(frac = 0.137, method = "pps_poisson", mos = x, certainty_prop = 0.1),
    n_size = sampling_design() |>
      draw(n = 10, method = "pps_poisson", mos = x, certainty_size = 50),
    strat_named_frac = sampling_design() |>
      stratify_by(st) |>
      draw(
        frac = c(a = 0.1, b = 0.15), method = "pps_poisson", mos = x,
        certainty_size = 50
      ),
    natural = sampling_design() |>
      draw(frac = 0.1, method = "pps_poisson", mos = x)
  )
  for (nm in names(designs)) {
    sample <- execute(designs[[nm]], frame, seed = 3)
    jip <- joint_expectation(sample, frame)[[1]]
    expect_equal(diag(jip), 1 / sample$.weight, info = nm)
  }
})

test_that("a pool whose certainty units were taken is never empty", {
  frame <- data.frame(id = 1:3, x = c(100, 0.01, 0.01))
  design <- sampling_design() |>
    draw(
      frac = 0.4, method = "pps_poisson", mos = x, certainty_size = 50
    )
  sizes <- vapply(1:20, function(seed) {
    nrow(execute(design, frame, seed = seed))
  }, integer(1))
  expect_true(all(sizes >= 1L))
  # The certainty unit is in every sample, at weight one.
  expect_true(all(vapply(1:20, function(seed) {
    s <- execute(design, frame, seed = seed)
    1L %in% s$id && s$.weight[s$id == 1L] == 1
  }, logical(1))))

  # Without a certainty unit an empty realization is still reported.
  no_certainty <- sampling_design() |>
    draw(frac = 0.01, method = "pps_poisson", mos = x)
  expect_error(
    for (seed in 1:50) execute(no_certainty, frame, seed = seed),
    "zero selections"
  )
})

## The frame must be the one the sample was drawn from

# One case per reason a frame can differ. The fingerprint recorded at
# execution covers the design's columns in row order, so a reordered or
# retyped frame differs from it as well as a changed one does.

joint_check_fixture <- function() {
  set.seed(7)
  frame <- data.frame(
    psu = rep(1:12, each = 2),
    st = rep(c("a", "b"), each = 12)
  )
  frame$id <- seq_len(nrow(frame))
  frame$size <- round(stats::runif(nrow(frame), 1, 10), 1)
  frame$key <- sample(nrow(frame))
  frame
}

test_that("a frame equal in content to the executed one is accepted", {
  frame <- joint_check_fixture()
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(n = 3, method = "pps_brewer", mos = size)
  sample <- execute(design, frame, seed = 3)
  reference <- joint_expectation(sample, frame)

  set.seed(1)
  variants <- list(
    extra_column = transform(frame, note = "x"),
    reordered = frame[sample(nrow(frame)), ],
    retyped = transform(frame, id = as.double(id), st = factor(st))
  )
  for (nm in names(variants)) {
    expect_silent(got <- joint_expectation(sample, variants[[nm]]))
    expect_equal(got, reference, info = nm)
  }
})

test_that("a changed or resized frame is refused", {
  frame <- joint_check_fixture()
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(n = 3, method = "pps_brewer", mos = size)
  sample <- execute(design, frame, seed = 3)

  corrected <- frame
  corrected$size[5] <- corrected$size[5] + 3
  err <- expect_error(
    joint_expectation(sample, corrected),
    class = "samplyr_error_joint_frame_mismatch"
  )
  expect_match(conditionMessage(err), "selection chances differ")

  larger <- rbind(frame, transform(frame[1, ], id = 99L))
  err <- expect_error(
    joint_expectation(sample, larger),
    class = "samplyr_error_joint_frame_mismatch"
  )
  expect_match(conditionMessage(err), "25 rows")

  expect_error(
    joint_expectation(sample, frame[, setdiff(names(frame), "size")]),
    class = "samplyr_error_joint_frame_mismatch"
  )
})

test_that("an order-dependent method refuses any reordering", {
  frame <- joint_check_fixture()
  set.seed(1)
  reordered <- frame[sample(nrow(frame)), ]
  designs <- list(
    no_control = sampling_design() |>
      draw(n = 6, method = "pps_systematic", mos = size),
    with_control = sampling_design() |>
      draw(n = 6, method = "pps_systematic", mos = size, control = key)
  )
  for (nm in names(designs)) {
    sample <- execute(designs[[nm]], frame, seed = 3)
    expect_silent(joint_expectation(sample, frame))
    err <- expect_error(
      joint_expectation(sample, reordered),
      class = "samplyr_error_joint_frame_mismatch"
    )
    expect_match(conditionMessage(err), "order-dependent", info = nm)
  }
})

test_that("a sample without a digest is computed but not checked", {
  frame <- joint_check_fixture()
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(n = 3, method = "pps_brewer", mos = size)
  sample <- execute(design, frame, seed = 3, frame_digest = "none")
  expect_warning(
    got <- joint_expectation(sample, frame),
    class = "samplyr_warning_joint_frame_unverified"
  )
  expect_equal(
    got,
    joint_expectation(execute(design, frame, seed = 3), frame)
  )
})

test_that("both routes order units by first appearance when pools interleave", {
  # Two strata inside each PSU, and rows shuffled, so the sampled blocks of
  # one (PSU, stratum) pool are not contiguous in the sample.
  frame <- expand.grid(
    hh = 1:3, blk = 1:6, psu = 1:3, st = c("a", "b"),
    stringsAsFactors = FALSE
  )
  frame$m1 <- frame$psu + 2 + (frame$st == "b")
  frame$m2 <- (frame$blk * 3 + frame$psu) %% 5 + 1
  frame$s2 <- ifelse(frame$blk <= 3, "x", "y")
  frame <- frame[c(37L, 5L, 90L, 61L, 12L, 100L, 1L, 74L, 49L, 28L,
                   setdiff(seq_len(nrow(frame)),
                           c(37L, 5L, 90L, 61L, 12L, 100L, 1L, 74L, 49L, 28L))), ]
  frame <- frame[order(frame$blk %% 2, decreasing = TRUE), ]
  design <- sampling_design() |>
    add_stage() |>
    stratify_by(st) |>
    cluster_by(psu) |>
    draw(n = 2, method = "pps_brewer", mos = m1) |>
    add_stage() |>
    stratify_by(s2) |>
    cluster_by(blk) |>
    draw(n = 2, method = "pps_cps", mos = m2)
  s <- suppressMessages(execute(design, frame, seed = 3, frame_digest = "full"))

  units <- as.data.frame(s)
  units <- units[!duplicated(units[c("st", "psu", "blk")]), ]
  pool <- paste(units$st, units$psu, units$s2)
  expect_true(anyDuplicated(rle(pool)$values) > 0L)

  from_frame <- suppressMessages(joint_expectation(s, frame))$stage_2
  from_digest <- suppressMessages(joint_expectation(s))$stage_2
  expect_equal(from_frame, from_digest)

  # An independent reference built with sondage, pool by pool.
  blocks <- unique(frame[c("st", "psu", "s2", "blk", "m2")])
  expected <- outer(1 / units$.weight_2, 1 / units$.weight_2)
  for (key in unique(pool)) {
    pool_units <- blocks[paste(blocks$st, blocks$psu, blocks$s2) == key, ]
    pik <- sondage::inclusion_prob(pool_units$m2, 2)
    joint <- sondage::joint_inclusion_prob(
      sondage::unequal_prob_wor(pik, method = "cps")
    )
    rows <- which(pool == key)
    at <- match(units$blk[rows], pool_units$blk)
    expected[rows, rows] <- joint[at, at]
  }
  expect_equal(unname(diag(from_frame)), 1 / units$.weight_2)
  expect_equal(unname(from_frame), expected)
})

test_that("survey pairs a stage-1 matrix with the right rows", {
  skip_if_not_installed("survey")
  # One row per PSU, strata interleaved in the frame and so in the sample.
  psus <- data.frame(
    st = rep(c("a", "b", "c"), times = 6),
    psu = 1:18,
    mos = c(4, 9, 2, 7, 3, 8, 6, 1, 5, 9, 4, 7, 2, 6, 8, 3, 5, 1)
  )
  psus$y <- psus$mos * 10 + psus$psu
  s <- sampling_design() |>
    stratify_by(st) |>
    cluster_by(psu) |>
    draw(n = 2, method = "pps_cps", mos = mos) |>
    execute(psus, seed = 4)
  rows <- as.data.frame(s)
  expect_true(anyDuplicated(rle(rows$st)$values) > 0L)

  jip <- joint_expectation(s, psus)[[1]]
  pik <- 1 / rows$.weight
  expect_equal(unname(diag(jip)), pik)

  # Horvitz-Thompson with each row paired with its own matrix row.
  y_pik <- rows$y / pik
  v <- as.numeric(t(y_pik) %*% ((jip - outer(pik, pik)) / jip) %*% y_pik)
  svy <- as_svydesign(s, pps = survey::ppsmat(jip))
  expect_equal(as.numeric(stats::vcov(survey::svytotal(~y, svy))), v)
})

test_that("key columns named like internal counters are matched as keys", {
  frame <- data.frame(.joint_rank = 1:10, mos = 1:10)
  s <- sampling_design() |>
    cluster_by(.joint_rank) |>
    draw(n = 3, method = "pps_brewer", mos = mos) |>
    execute(frame, seed = 2, frame_digest = "full")
  expect_identical(s$.joint_rank, c(5L, 6L, 8L))
  from_frame <- joint_expectation(s, frame)[[1]]
  expect_equal(unname(diag(from_frame)), 1 / s$.weight)
  expect_equal(from_frame, joint_expectation(s)[[1]])

  # Two key columns take the joined path, which numbers rows internally.
  for (name in c(".sample_row", ".frame_row", ".joint_rank")) {
    frame <- expand.grid(k = 1:6, psu = 1:4)
    frame[[name]] <- c(5L, 2L, 6L, 1L, 4L, 3L)[frame$k]
    frame$m1 <- frame$psu + 1
    frame$m2 <- frame$k
    design <- sampling_design() |>
      add_stage() |>
      cluster_by(psu) |>
      draw(n = 2, method = "pps_brewer", mos = m1) |>
      add_stage()
    design <- do.call(cluster_by, list(design, as.name(name))) |>
      draw(n = 3, method = "pps_brewer", mos = m2)
    s <- suppressMessages(
      execute(design, frame, seed = 2, frame_digest = "full")
    )
    units <- as.data.frame(s)
    units <- units[!duplicated(units[c("psu", name)]), ]
    from_frame <- joint_expectation(s, frame)[[2]]
    expect_equal(unname(diag(from_frame)), 1 / units$.weight_2, label = name)
    expect_equal(from_frame, joint_expectation(s)[[2]], label = name)
  }
})
