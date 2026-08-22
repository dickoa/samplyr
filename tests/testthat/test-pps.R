test_that("fixed PPS methods store inverse resolved targets", {
  frame <- data.frame(
    id = 1:8,
    size = seq(10, 80, by = 10)
  )
  methods <- c(
    "pps_brewer", "pps_systematic", "pps_cps", "pps_sampford",
    "pps_sps", "pps_pareto"
  )
  n <- 2

  for (method in methods) {
    result <- sampling_design() |>
      draw(n = n, method = method, mos = size) |>
      execute(frame, seed = 42)
    expected <- sum(frame$size) / (n * result$size)

    expect_equal(nrow(result), n, label = paste(method, "cardinality"))
    expect_equal(
      result$.weight,
      expected,
      tolerance = 1e-10,
      label = paste(method, "weight")
    )
  }
})

test_that("Stratified PPS gives correct within-stratum weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 4),
    id = 1:8,
    size = c(10, 20, 30, 40, 100, 200, 300, 400)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 2, method = "pps_systematic", mos = size) |>
    execute(frame, seed = 123)

  # Check within each stratum
  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]

  # Stratum A: total = 100, n = 2, w_i = 100 / (2 * size_i)
  for (i in seq_len(nrow(result_A))) {
    expected_weight <- 100 / (2 * result_A$size[i])
    expect_equal(result_A$.weight[i], expected_weight, tolerance = 1e-10)
  }

  # Stratum B: total = 1000, n = 2, w_i = 1000 / (2 * size_i)
  for (i in seq_len(nrow(result_B))) {
    expected_weight <- 1000 / (2 * result_B$size[i])
    expect_equal(result_B$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("Cluster PPS sampling gives correct cluster-level weights", {
  # Frame with clusters of different sizes
  frame <- data.frame(
    cluster = rep(1:4, times = c(5, 10, 15, 20)),
    cluster_size = rep(c(5, 10, 15, 20), times = c(5, 10, 15, 20)),
    id = 1:50
  )

  result <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 2, method = "pps_systematic", mos = cluster_size) |>
    execute(frame, seed = 42)

  # Get unique cluster info from result
  cluster_info <- unique(result[, c(
    "cluster",
    "cluster_size",
    ".weight"
  )])

  # Total cluster size sum (at cluster level) = 5+10+15+20 = 50
  # w_i = 50 / (2 * cluster_size)
  for (i in seq_len(nrow(cluster_info))) {
    expected_weight <- 50 / (2 * cluster_info$cluster_size[i])
    expect_equal(cluster_info$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("stratified PPS Poisson stores inverse inclusion probabilities", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 5),
    id = 1:10,
    size = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500),
    u = 0.01
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = 0.4, method = "pps_poisson", mos = size, prn = u) |>
    execute(frame)

  stratum_total <- ave(frame$size, frame$stratum, FUN = sum)
  expected_pik <- 0.4 * 5 * frame$size / stratum_total
  expect_equal(result$id, frame$id)
  expect_equal(result$.weight, 1 / expected_pik, tolerance = 1e-10)
})

test_that("PPS Poisson has random realized cardinality", {
  frame <- data.frame(
    id = 1:20,
    size = 1,
    u4 = c(rep(0.1, 4), rep(0.9, 16)),
    u6 = c(rep(0.1, 6), rep(0.9, 14))
  )

  draw_with <- function(prn) {
    sampling_design() |>
      draw(frac = 0.25, method = "pps_poisson", mos = size, prn = {{ prn }}) |>
      execute(frame)
  }

  four <- draw_with(u4)
  six <- draw_with(u6)

  expect_equal(nrow(four), 4L)
  expect_equal(nrow(six), 6L)
  expect_equal(unique(four$.weight), 4)
  expect_equal(unique(six$.weight), 4)
})

test_that("PPS multinomial method produces replicated rows with draw index", {
  frame <- data.frame(
    id = 1:8,
    size = c(10, 20, 30, 40, 50, 60, 70, 80) # Total = 360
  )

  result <- sampling_design() |>
    draw(n = 4, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 123)

  # WR method: one row per draw (n rows total)
  expect_equal(nrow(result), 4L)

  # Should have .draw_1 column for stage 1
  expect_true(".draw_1" %in% names(result))

  # Draw IDs should be sequential 1:n
  expect_equal(result$.draw_1, 1:4)

  # Weights should be positive
  expect_true(all(result$.weight > 0))

  # Weight = 1/pik = total_size / (n * size_i) for each draw
  total_size <- sum(frame$size)
  n <- 4
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (n * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("PPS Chromy method produces replicated rows with draw index", {
  frame <- data.frame(
    id = 1:8,
    size = c(10, 20, 30, 40, 50, 60, 70, 80) # Total = 360
  )

  result <- sampling_design() |>
    draw(n = 4, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 123)

  # PMR: one row per draw (n rows total)
  expect_equal(nrow(result), 4L)

  # Should have .draw_1 column
  expect_true(".draw_1" %in% names(result))

  # Draw IDs should be sequential
  expect_equal(result$.draw_1, 1:4)

  # Weights should be positive
  expect_true(all(result$.weight > 0))

  # Weight = 1/pik = total_size / (n * size_i)
  total_size <- sum(frame$size)
  n <- 4
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (n * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("PPS Chromy with minimum replacement replicates large-hit units", {
  # When expected hits > 1, Chromy uses minimum replacement
  frame <- data.frame(
    id = 1:4,
    size = c(10, 20, 30, 140) # Total = 200
  )

  # n = 10 means expected hits for id=4 is 10*140/200 = 7
  result <- sampling_design() |>
    draw(n = 10, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 42)

  # PMR: one row per draw (n=10 rows total)
  expect_equal(nrow(result), 10L)
  expect_equal(result$.draw_1, 1:10)

  expect_equal(sum(result$id == 4), 7L)
})

test_that("Stratified PPS Chromy works correctly", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 5),
    id = 1:10,
    size = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 3, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 42)

  # Total rows should be 6 (3 per stratum)
  expect_equal(nrow(result), 6L)

  # 3 draws from each stratum
  expect_equal(sum(result$stratum == "A"), 3L)
  expect_equal(sum(result$stratum == "B"), 3L)
})

test_that("Stratified PPS SPS gives correct within-stratum weights", {
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 5),
    id = 1:10,
    size = c(10, 20, 30, 40, 50, 100, 200, 300, 400, 500)
  )

  result <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 2, method = "pps_sps", mos = size) |>
    execute(frame, seed = 123)

  expect_equal(nrow(result), 4)

  result_A <- result[result$stratum == "A", ]
  result_B <- result[result$stratum == "B", ]

  for (i in seq_len(nrow(result_A))) {
    expected_weight <- 150 / (2 * result_A$size[i])
    expect_equal(result_A$.weight[i], expected_weight, tolerance = 1e-10)
  }

  for (i in seq_len(nrow(result_B))) {
    expected_weight <- 1500 / (2 * result_B$size[i])
    expect_equal(result_B$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

## PPS Poisson shortfall
##
## The check measures against what the pool could reach, not against what was
## asked for. A target above the population has already been reduced once by
## the population, and that reduction belongs to `nominal_cap`; charging the
## same units to saturation as well would double count. Both conditions fire
## when there are genuinely two reductions.

count_conditions_pps <- function(expr, class) {
  n <- 0L
  withCallingHandlers(
    force(expr),
    condition = function(cnd) {
      if (inherits(cnd, class)) {
        n <<- n + 1L
      }
      if (inherits(cnd, "warning")) {
        invokeRestart("muffleWarning")
      }
      if (inherits(cnd, "message")) {
        invokeRestart("muffleMessage")
      }
    }
  )
  n
}

capture_shortfall <- function(expr) {
  found <- NULL
  suppressWarnings(withCallingHandlers(
    force(expr),
    samplyr_warning_poisson_shortfall = function(w) found <<- w
  ))
  found
}

# Three units 300x the rest: their computed chances clip at 1 and absorb the
# target while 97 units share what is left.
skewed_pps_frame <- function(n_big = 3, n_small = 97, big = 300) {
  data.frame(
    id = seq_len(n_big + n_small),
    m = c(rep(big, n_big), rep(1, n_small))
  )
}

uniform_pps_frame <- function(n = 100) {
  data.frame(id = seq_len(n), m = rep(1, n))
}

poisson_design <- function(...) {
  sampling_design() |> draw(method = "pps_poisson", mos = m, ...)
}

test_that("a saturating pool below its reachable target warns", {
  w <- capture_shortfall(
    execute(poisson_design(n = 40), skewed_pps_frame(), seed = 1)
  )

  expect_false(is.null(w))
  expect_equal(w$payload$n_requested, 40)
  expect_equal(w$payload$n_reachable, 40)
  expect_lt(w$payload$n_expected, 40 * 0.95)
  expect_equal(w$payload$n_clipped, 3L)
})

test_that("a pool that meets its target is silent", {
  expect_equal(
    count_conditions_pps(
      execute(poisson_design(n = 40), uniform_pps_frame(), seed = 1),
      "samplyr_warning_poisson_shortfall"
    ),
    0L
  )
})

test_that("mild clipping under the tolerance is silent", {
  # One unit at three times the rest clips, and the shortfall is far under 5%.
  frame <- data.frame(id = 1:100, m = c(3, rep(1, 99)))

  expect_equal(
    count_conditions_pps(
      execute(poisson_design(n = 40), frame, seed = 1),
      "samplyr_warning_poisson_shortfall"
    ),
    0L
  )
})

test_that("a target above the population is not charged to saturation", {
  # Uniform sizes, n > N: the population reduced the target and nothing else
  # did. Measuring against the request rather than against what was reachable
  # would report a shortfall that saturation did not cause.
  frame <- uniform_pps_frame(10)

  expect_equal(
    count_conditions_pps(
      execute(poisson_design(n = 20), frame, seed = 1),
      "samplyr_warning_poisson_shortfall"
    ),
    0L
  )
  expect_equal(
    count_conditions_pps(
      execute(poisson_design(n = 20), frame, seed = 1),
      "samplyr_warning_nominal_cap"
    ),
    1L
  )
})

test_that("two genuine reductions report both, and stay auditable", {
  # 20 requested -> 10 reachable (population) -> 3.08 expected (saturation).
  frame <- skewed_pps_frame(n_big = 3, n_small = 7)

  expect_equal(
    count_conditions_pps(
      execute(poisson_design(n = 20), frame, seed = 1),
      "samplyr_warning_nominal_cap"
    ),
    1L
  )

  w <- capture_shortfall(execute(poisson_design(n = 20), frame, seed = 1))
  expect_equal(w$payload$n_requested, 20)
  expect_equal(w$payload$n_reachable, 10)
  expect_gt(w$payload$n_requested, w$payload$n_reachable)
  expect_gt(w$payload$n_reachable, w$payload$n_expected)
})

test_that("explicit certainty that covers the deficit silences the check", {
  # The documented remedy. Checked on the combined pool: the certainty units
  # sit at chance one and raise the expectation, so the check goes quiet.
  expect_equal(
    count_conditions_pps(
      execute(
        poisson_design(n = 40, certainty_size = 200),
        skewed_pps_frame(),
        seed = 1
      ),
      "samplyr_warning_poisson_shortfall"
    ),
    0L
  )
})

test_that("explicit certainty that does not bind leaves the check firing", {
  # A threshold no unit reaches changes nothing, so the deficit stands.
  w <- capture_shortfall(
    execute(
      poisson_design(n = 40, certainty_size = 100000),
      skewed_pps_frame(),
      seed = 1
    )
  )

  expect_false(is.null(w))
  expect_lt(w$payload$n_expected, 40 * 0.95)
})

test_that("deliberate certainty units are not counted as clipped", {
  # One unit taken by the certainty rule, and two more whose computed chances
  # exceed one on the remainder: 9 residual units spread over a MOS total of
  # 217, so 9 * 100 / 217 = 4.1 for each of the two large ones. Counting
  # `sum(pik == 1)` on the combined pool would report three, charging the
  # explicitly selected unit to saturation.
  frame <- data.frame(id = 1:20, m = c(1000, 100, 100, rep(1, 17)))

  w <- capture_shortfall(
    execute(poisson_design(n = 10, certainty_size = 500), frame, seed = 2)
  )

  expect_false(is.null(w))
  expect_equal(w$payload$n_clipped, 2L)
  # The certainty unit still counts toward the expectation, at chance one.
  expect_equal(w$payload$n_expected, 1 + 2 + 17 * (9 / 217), tolerance = 1e-6)
})

test_that("a healthy pool cannot mask a collapsed one", {
  # Stratum A saturates, stratum B meets its target. A stage-level ratio
  # would average the two and report nothing.
  frame <- data.frame(
    stratum = rep(c("A", "B"), each = 50),
    m = c(c(rep(300, 3), rep(1, 47)), rep(1, 50)),
    id = 1:100
  )

  w <- capture_shortfall(
    execute(
      sampling_design() |>
        stratify_by(stratum) |>
        draw(n = 20, method = "pps_poisson", mos = m),
      frame,
      seed = 1
    )
  )

  expect_false(is.null(w))
  expect_equal(w$payload$n_pools, 1L)
  expect_identical(w$payload$pool_keys, "A")
  # Only the affected pool is aggregated, so the totals describe it alone.
  expect_equal(w$payload$n_reachable, 20)
})

test_that("a shortfall inside a cluster is named by its ancestry", {
  frame <- data.frame(
    psu = rep(c("p1", "p2"), each = 50),
    stratum = rep(rep(c("A", "B"), each = 25), 2),
    m = c(c(rep(300, 3), rep(1, 22)), rep(1, 25), rep(1, 50)),
    id = 1:100
  )

  w <- capture_shortfall(
    execute(
      sampling_design() |>
        add_stage("psu") |>
        cluster_by(psu) |>
        draw(n = 2) |>
        add_stage("unit") |>
        stratify_by(stratum) |>
        draw(n = 15, method = "pps_poisson", mos = m),
      frame,
      seed = 1
    )
  )

  expect_false(is.null(w))
  expect_identical(w$payload$pool_keys, "p1 / A")
})

test_that("replicates report one shortfall with one replicate's totals", {
  frame <- skewed_pps_frame()

  expect_equal(
    count_conditions_pps(
      execute(poisson_design(n = 40), frame, seed = 1, reps = 4),
      "samplyr_warning_poisson_shortfall"
    ),
    1L
  )

  w <- capture_shortfall(
    execute(poisson_design(n = 40), frame, seed = 1, reps = 4)
  )
  expect_equal(w$payload$n_reachable, 40)
  expect_equal(w$payload$n_pools, 1L)
})

test_that("the shortfall does not bias the Horvitz-Thompson total", {
  # The warning is about design fidelity, not about bias: a saturated Poisson
  # design still estimates the total without systematic error.
  frame <- skewed_pps_frame()
  frame$y <- frame$m * 2 + 1

  truth <- sum(frame$y)
  estimates <- vapply(
    seq_len(200),
    function(s) {
      sample <- suppressWarnings(
        execute(
          poisson_design(n = 40, on_empty = "silent"),
          frame,
          seed = s
        )
      )
      if (nrow(sample) == 0) return(0)
      sum(sample$y * sample$.weight)
    },
    numeric(1)
  )

  expect_equal(mean(estimates), truth, tolerance = 0.05)
})

test_that("the shortfall message reads as intended", {
  expect_snapshot(
    invisible(execute(poisson_design(n = 40), skewed_pps_frame(), seed = 1))
  )
})
