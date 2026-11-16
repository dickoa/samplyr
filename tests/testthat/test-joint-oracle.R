## Joint expected hits against values computed by hand
##
## Every expected value here comes from the frame alone, without samplyr or
## sondage, so an error shared by the frame and digest paths, or by samplyr
## and its backend, cannot cancel out.

## Oracles

# Multinomial: the n draws are independent, each picking unit i with
# probability p_i.
oracle_multinomial <- function(size, n) {
  p <- size / sum(size)
  e <- n * p
  joint <- n * (n - 1) * outer(p, p)
  diag(joint) <- n * p * (1 - p) + e^2
  list(e = e, joint = joint)
}

# Systematic with minimum replacement: one uniform start u in (0, 1) and
# points u, u + 1, ..., u + n - 1 on the cumulated expected hits. The hit
# counts are constant between the start values where a point crosses a
# boundary, so the moments are exact sums over those intervals.
oracle_systematic_mr <- function(size, n) {
  e <- n * size / sum(size)
  cumulative <- c(0, cumsum(e))
  breaks <- sort(unique(c(0, 1, cumulative %% 1)))
  width <- diff(breaks)
  mid <- utils::head(breaks, -1) + width / 2
  counts <- vapply(mid, function(u) {
    tabulate(
      findInterval(u + seq_len(n) - 1, cumulative, rightmost.closed = TRUE),
      length(size)
    )
  }, integer(length(size)))
  list(e = e, joint = (counts * rep(width, each = length(size))) %*% t(counts))
}

# The full matrix over the sampled rows. Rows in different blocks were drawn
# independently, so their entry is the product of first moments. `blocks`
# names each row's block and `position` its place in that block's oracle.
oracle_assemble <- function(blocks, position, oracles) {
  e <- mapply(function(b, i) oracles[[b]]$e[[i]], blocks, position)
  joint <- outer(e, e)
  for (b in unique(blocks)) {
    rows <- which(blocks == b)
    joint[rows, rows] <- oracles[[b]]$joint[position[rows], position[rows]]
  }
  unname(joint)
}

oracle_for <- function(method, size, n) {
  switch(
    method,
    pps_multinomial = oracle_multinomial(size, n),
    pps_systematic_mr = oracle_systematic_mr(size, n),
    # Chromy's pair moments are simulated, so only its first moments, and
    # hence its entries across blocks, have a closed form.
    pps_chromy = list(e = n * size / sum(size), joint = NULL)
  )
}

## The oracles themselves

test_that("the systematic oracle reproduces moments known in closed form", {
  # Two equal units and three hits: each unit gets one or two hits, and
  # the two counts always sum to three.
  o <- oracle_systematic_mr(c(1, 1), 3)
  expect_equal(o$e, c(1.5, 1.5))
  expect_equal(o$joint, matrix(c(2.5, 2, 2, 2.5), 2))
  # A whole number of expected hits is drawn exactly that many times.
  o <- oracle_systematic_mr(c(2, 1, 1), 4)
  expect_equal(diag(o$joint)[[1]], 4)
  expect_equal(o$joint[1, 2:3], c(2, 2))
})

## One stage, several strata

test_that("joint expected hits across strata are products of first moments", {
  # Unequal sizes, strata interleaved in the frame, and two hits possible
  # for the large units.
  frame <- data.frame(
    id = 1:9,
    stratum = c("a", "b", "a", "c", "b", "a", "c", "b", "a"),
    size = c(6, 1, 1, 2, 3, 2, 5, 1, 1)
  )
  takes <- c(a = 4, b = 3, c = 3)
  for (method in pps_wr_methods) {
    design <- sampling_design() |>
      stratify_by(stratum) |>
      draw(n = takes, method = method, mos = size)
    for (seed in 1:5) {
      s <- execute(design, frame, seed = seed, frame_digest = "full")
      ids <- unique(s$id)
      stratum <- frame$stratum[ids]
      position <- vapply(seq_along(ids), function(r) {
        which(frame$id[frame$stratum == stratum[[r]]] == ids[[r]])
      }, integer(1))
      oracles <- lapply(stats::setNames(nm = names(takes)), function(h) {
        oracle_for(method, frame$size[frame$stratum == h], takes[[h]])
      })
      e <- mapply(function(b, i) oracles[[b]]$e[[i]], stratum, position)
      across <- outer(stratum, stratum, "!=")
      for (joint in list(
        joint_expectation(s, frame)$stage_1,
        joint_expectation(s)$stage_1
      )) {
        expect_named(attributes(joint), "dim")
        expect_equal(joint[across], outer(e, e)[across], tolerance = 1e-12)
        # Any matrix of second moments obeys Cauchy-Schwarz.
        expect_true(all(joint^2 <= outer(diag(joint), diag(joint)) + 1e-9))
        if (!identical(method, "pps_chromy")) {
          expect_equal(
            joint, oracle_assemble(stratum, position, oracles),
            tolerance = 1e-12
          )
        }
      }
    }
  }
})

## A later stage under repeated parents

test_that("a later stage's joint expected hits multiply across parent hits", {
  # Stage 1 can hit a PSU more than once, and each hit draws its own
  # households, stratified within the PSU. The matrix nests the strata
  # inside the hits of each PSU.
  psus <- data.frame(psu = 1:3, size = c(5, 2, 1))
  households <- data.frame(
    psu = rep(1:3, each = 4),
    hh = 1:12,
    zone = rep(c("u", "r"), 6),
    size = c(3, 1, 2, 2, 1, 1, 4, 2, 2, 3, 1, 1)
  )
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3, method = "pps_multinomial", mos = size) |>
    add_stage() |>
    stratify_by(zone) |>
    draw(n = 3, method = "pps_systematic_mr", mos = size)
  repeated_parent <- FALSE
  for (seed in 1:8) {
    s <- execute(
      design, list(psus, households), seed = seed, frame_digest = "full"
    )
    repeated_parent <- repeated_parent ||
      any(duplicated(unique(s[c("psu", ".draw_1")])$psu))
    rows <- as.data.frame(s)[!duplicated(paste(s$psu, s$.draw_1, s$hh)), ]
    blocks <- paste(rows$psu, rows$.draw_1, rows$zone)
    oracles <- list()
    position <- integer(nrow(rows))
    for (r in seq_len(nrow(rows))) {
      pool <- households[
        households$psu == rows$psu[[r]] & households$zone == rows$zone[[r]],
      ]
      oracles[[blocks[[r]]]] <- oracle_systematic_mr(pool$size, 3)
      position[[r]] <- which(pool$hh == rows$hh[[r]])
    }
    expected <- oracle_assemble(blocks, position, oracles)
    expect_equal(
      joint_expectation(s, list(psus, households))$stage_2, expected,
      tolerance = 1e-12
    )
    expect_equal(joint_expectation(s)$stage_2, expected, tolerance = 1e-12)

    psu_rows <- unique(s$psu)
    stage_1 <- oracle_multinomial(psus$size, 3)
    expect_equal(
      joint_expectation(s, list(psus, households))$stage_1,
      stage_1$joint[psu_rows, psu_rows, drop = FALSE],
      tolerance = 1e-12
    )
  }
  # The seeds must reach the case the nesting exists for.
  expect_true(repeated_parent)
})

## Properties every joint matrix has

# These need no oracle, so they cover every method at once. A matrix of
# second moments is symmetric, nonnegative and obeys Cauchy-Schwarz. Without
# replacement its diagonal is the inclusion probability, 1 / weight, and no
# pair exceeds the smaller one. With hits, E(n^2) >= E(n)^2.
check_joint_properties <- function(joint, weight, multi_hit) {
  e <- 1 / weight
  expect_equal(nrow(joint), length(e))
  expect_true(isSymmetric(unname(joint), tol = 1e-9))
  expect_true(all(joint >= -1e-12))
  expect_true(all(joint^2 <= outer(diag(joint), diag(joint)) * (1 + 1e-9)))
  if (multi_hit) {
    expect_true(all(diag(joint) >= e^2 - 1e-9))
  } else {
    expect_equal(diag(joint), e, tolerance = 1e-8)
    expect_true(all(joint <= outer(e, e, pmin) + 1e-9))
  }
}

joint_property_frame <- function() {
  psus <- data.frame(
    psu = 1:12,
    region = rep(c("n", "s"), each = 6),
    size_psu = c(9, 1, 3, 2, 6, 1, 4, 4, 1, 8, 2, 2)
  )
  households <- data.frame(
    psu = rep(1:12, each = 5),
    hh = seq_len(60),
    zone = rep(c("u", "r", "u", "r", "u"), 12),
    size = rep(c(3, 1, 2, 5, 1), 12)
  )
  merge(households, psus, by = "psu")
}

draw_for_properties <- function(design, method, n, mos) {
  switch(
    method,
    srswor = ,
    srswr = ,
    systematic = draw(design, n = n, method = method),
    bernoulli = draw(design, frac = 0.4, method = method),
    draw(design, n = n, method = method, mos = !!rlang::sym(mos))
  )
}

test_that("joint matrices keep their properties across strata and stages", {
  frame <- joint_property_frame()
  stage_1_methods <- c(
    "srswor", "systematic", "bernoulli", "pps_brewer", "pps_cps",
    "pps_sampford", "pps_systematic", "pps_poisson", "pps_sps",
    "pps_pareto", "srswr", "pps_multinomial", "pps_systematic_mr"
  )
  stage_2_methods <- c("pps_brewer", "pps_multinomial", "pps_systematic_mr")
  multi_hit <- function(method) method %in% c("srswr", pps_wr_methods)
  checked <- character(0)
  for (m1 in stage_1_methods) {
    for (m2 in stage_2_methods) {
      design <- sampling_design() |>
        add_stage() |>
        stratify_by(region) |>
        cluster_by(psu) |>
        draw_for_properties(m1, c(n = 2, s = 2), "size_psu") |>
        add_stage() |>
        stratify_by(zone) |>
        draw_for_properties(m2, 2, "size")
      s <- suppressMessages(suppressWarnings(
        execute(design, frame, seed = 2, frame_digest = "full")
      ))
      df <- as.data.frame(s)
      for (joint in list(
        suppressWarnings(joint_expectation(s, frame)),
        suppressWarnings(joint_expectation(s))
      )) {
        if (!is.null(joint$stage_1)) {
          check_joint_properties(
            joint$stage_1, df$.weight_1[!duplicated(df$psu)], multi_hit(m1)
          )
          checked <- c(checked, paste("stage 1", m1))
        }
        key <- intersect(c("psu", ".draw_1", "hh"), names(df))
        check_joint_properties(
          joint$stage_2, df$.weight_2[!duplicated(df[key])], multi_hit(m2)
        )
        checked <- c(checked, paste("stage 2", m1, m2))
      }
    }
  }
  # The equal-probability methods return no matrix. Every other method must
  # have been checked.
  no_matrix <- c("srswor", "systematic", "bernoulli", "srswr")
  expect_setequal(
    unique(checked),
    c(
      paste("stage 1", setdiff(stage_1_methods, no_matrix)),
      paste("stage 2", rep(stage_1_methods, each = 3), stage_2_methods)
    )
  )
})

test_that("a second phase drawn within phase 1's units has phase 1's joint", {
  skip_if_not_installed("survey")
  # One element per phase-1 PSU exports as phase 1's two-stage design, so
  # the preflight gives its joint-probability recipe. joint_expectation()
  # read phase 2 alone, returned no stage-1 matrix, and the recipe failed
  # with "jointprob must be a square matrix".
  # Exact oracle: conditional Poisson sampling of 3 PSUs of 6 enumerated,
  # with working odds solved so each PSU's chance is its target, i / 7.
  cps_joint <- function(target, n) {
    sets <- utils::combn(length(target), n)
    odds <- target / (1 - target)
    for (i in 1:5000) {
      p <- apply(sets, 2, function(s) prod(odds[s]))
      p <- p / sum(p)
      chance <- vapply(seq_along(target), function(u) {
        sum(p[colSums(sets == u) > 0])
      }, numeric(1))
      odds <- odds * target / chance
    }
    expect_lt(max(abs(chance - target)), 1e-10)
    joint <- matrix(0, length(target), length(target))
    for (k in seq_len(ncol(sets))) {
      s <- sets[, k]
      joint[s, s] <- joint[s, s] + p[k]
    }
    joint
  }
  exact <- cps_joint((1:6) / 7, 3)
  composed_psus <- function(s) {
    unique(suppressWarnings(as_svydesign(s))$variables$psu)
  }
  within <- sampling_design() |> stratify_by(psu) |> draw(n = 1)
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_cps", mos = mos) |>
    add_stage() |> draw(n = 4, on_empty = "silent")
  frame <- data.frame(psu = rep(1:6, each = 5), mos = rep(10 * (1:6), each = 5),
                      y = (1:30) %% 7)
  checked <- character(0)
  for (seed in c(1, 7)) {
    phase1 <- execute(design, frame, seed = seed, frame_digest = "full")
    phase2 <- suppressMessages(execute(within, phase1, seed = 2))
    v <- variance_estimators(phase2)
    row <- v[v$estimator == "Linearization, joint probabilities", ]
    expect_identical(row$status, "approximate")
    expect_identical(row$class, "samplyr_warning_pps_single_stage")
    joint <- joint_expectation(phase2)
    psus <- composed_psus(phase2)
    expect_equal(unname(joint$stage_1), exact[psus, psus], tolerance = 1e-8)
    expect_null(joint$stage_2)
    # Phase 1's frame gives the same matrix as its digest.
    expect_equal(joint_expectation(phase2, frame = frame)$stage_1,
                 joint$stage_1)
    expect_null(joint_expectation(phase2, stages = 2)$stage_1)
    # The recipe now runs, with the stage-1 approximation still warned.
    expect_warning(
      svy <- as_svydesign(phase2, pps = survey::ppsmat(joint$stage_1),
                          variance = "YG"),
      class = "samplyr_warning_pps_single_stage"
    )
    expect_false(is.na(survey::SE(survey::svytotal(~y, svy))))
    checked <- c(checked, paste("seed", seed))
  }

  expect_setequal(checked, c("seed 1", "seed 7"))

  # Two elements per PSU stay refused by the preflight, and a phase 2 that
  # is not drawn within phase 1's units keeps its own matrices.
  two <- suppressMessages(execute(
    sampling_design() |> stratify_by(psu) |> draw(n = 2),
    execute(design, frame, seed = 1, frame_digest = "full"), seed = 2
  ))
  v <- variance_estimators(two)
  expect_identical(
    v$class[v$estimator == "Linearization, joint probabilities"],
    "samplyr_error_pps_rows_per_psu"
  )
  across <- suppressMessages(execute(
    sampling_design() |> draw(n = 5),
    execute(design, frame, seed = 1, frame_digest = "full"), seed = 2
  ))
  expect_null(samplyr:::composed_within_pool(across))
  expect_identical(names(joint_expectation(across)), "stage_1")
})

test_that("joint matrices follow the exported rows when a PSU is empty", {
  skip_if_not_installed("survey")
  # A selected PSU with nothing below it is exported as a zero row after
  # the sample's rows. Both routes read the sample's rows alone, so the
  # digest kept the drawn order and the frame route dropped the PSU. Seed 1
  # draws PSUs 4, 5 and 6, and emptying 6 alone kept the drawn order by
  # chance. This covers a plain sample with one row per PSU and a second
  # phase composed with phase 1.
  # Exact oracle: conditional Poisson sampling of 3 PSUs of 6 enumerated,
  # with working odds solved so each PSU's chance is its target, i / 7.
  target <- (1:6) / 7
  sets <- utils::combn(6, 3)
  odds <- target / (1 - target)
  for (i in 1:5000) {
    p <- apply(sets, 2, function(s) prod(odds[s]))
    p <- p / sum(p)
    chance <- vapply(1:6, function(u) sum(p[colSums(sets == u) > 0]),
                     numeric(1))
    odds <- odds * target / chance
  }
  expect_lt(max(abs(chance - target)), 1e-10)
  exact <- matrix(0, 6, 6)
  for (k in seq_len(ncol(sets))) {
    exact[sets[, k], sets[, k]] <- exact[sets[, k], sets[, k]] + p[k]
  }

  psu_frame <- data.frame(psu = 1:6, mos = 10 * (1:6))
  first <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_cps", mos = mos)
  one_each <- first |>
    add_stage() |>
    draw(n = 1, method = "pps_brewer", mos = size, on_empty = "silent")
  four_each <- first |> add_stage() |> draw(n = 4, on_empty = "silent")
  within <- sampling_design() |> stratify_by(psu) |> draw(n = 1)
  checked <- character(0)
  for (empty in c(0L, 4L, 5L, 6L)) {
    units <- data.frame(psu = rep(setdiff(1:6, empty), each = 5))
    units$size <- rep(1:5, length.out = nrow(units))
    units$y <- seq_len(nrow(units)) %% 7
    frames <- list(psu_frame, units)
    plain <- suppressWarnings(execute(one_each, psu_frame, units, seed = 1,
                                      frame_digest = "full"))
    phase1 <- suppressWarnings(execute(four_each, psu_frame, units,
                                       seed = 1, frame_digest = "full"))
    composed <- suppressWarnings(suppressMessages(
      execute(within, phase1, seed = 2)
    ))
    samples <- list(plain = plain, composed = composed)
    for (kind in names(samples)) for (reversed in c(FALSE, TRUE)) {
      s <- samples[[kind]]
      if (reversed) {
        s <- dplyr::arrange(s, dplyr::desc(psu))
      }
      psus <- suppressWarnings(as_svydesign(s))$variables$psu
      expect_setequal(psus, 4:6)
      if (empty > 0L) {
        expect_identical(psus[[3]], empty)
      }
      digest <- joint_expectation(s)
      expect_equal(unname(digest$stage_1), exact[psus, psus],
                   tolerance = 1e-8)
      expect_equal(joint_expectation(s, frame = frames)$stage_1,
                   digest$stage_1)
      expect_equal(joint_expectation(s, stages = 1)$stage_1, digest$stage_1)
      svy <- suppressWarnings(as_svydesign(
        s, pps = survey::ppsmat(digest$stage_1), variance = "YG"
      ))
      expect_identical(nrow(svy$variables), 3L)
      expect_false(is.na(survey::SE(survey::svytotal(~y, svy, na.rm = TRUE))))
      if (kind == "plain") {
        # Stage 2 holds the sampled units, never the zero row's placeholder.
        # One unit of size s is drawn from each PSU's sizes 1 to 5.
        chance <- as.data.frame(s)$size / 15
        exact2 <- outer(chance, chance)
        diag(exact2) <- chance
        expect_equal(unname(digest$stage_2), exact2, tolerance = 1e-12)
        expect_equal(joint_expectation(s, frame = frames)$stage_2,
                     digest$stage_2)
      }
      checked <- c(checked, paste(kind, empty, reversed,
                                  is.unsorted(psus)))
    }
  }
  # Every case ran, and five of them export out of PSU order.
  expect_setequal(checked, c(
    "plain 0 FALSE FALSE", "plain 0 TRUE TRUE",
    "plain 4 FALSE TRUE", "plain 4 TRUE TRUE",
    "plain 5 FALSE TRUE", "plain 5 TRUE TRUE",
    "plain 6 FALSE FALSE", "plain 6 TRUE TRUE",
    "composed 0 FALSE FALSE", "composed 0 TRUE TRUE",
    "composed 4 FALSE TRUE", "composed 4 TRUE TRUE",
    "composed 5 FALSE TRUE", "composed 5 TRUE TRUE",
    "composed 6 FALSE FALSE", "composed 6 TRUE TRUE"
  ))
})

test_that("joint_expectation() refuses an empty PSU it cannot place", {
  # A record that keeps no row gives no zero row, so the export refuses the
  # sample and no matrix could line up with it.
  psu_frame <- data.frame(psu = 1:6, mos = 10 * (1:6))
  units <- data.frame(psu = rep(c(4L, 6L), each = 5), y = 1:10)
  sample <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 3, method = "pps_cps", mos = mos) |>
    add_stage() |> draw(n = 1, on_empty = "silent") |>
    execute(psu_frame, units, seed = 1, frame_digest = "full")
  expect_setequal(sample$psu, c(4L, 6L))
  attr(sample, "metadata")$empty_parents[[1]]$rows <- NULL
  expect_error(joint_expectation(sample),
               class = "samplyr_error_export_empty_psu")
  expect_error(joint_expectation(sample, frame = list(psu_frame, units)),
               class = "samplyr_error_export_empty_psu")
})
