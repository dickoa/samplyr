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
