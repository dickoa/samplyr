make_unequal_frame <- function() {
  set.seed(42)
  data.frame(
    id = 1:1000,
    region = c(
      rep("Large", 800),
      rep("Medium", 150),
      rep("Small", 50)
    ),
    income = rlnorm(1000, meanlog = 10, sdlog = 0.5)
  )
}

make_variance_df <- function() {
  data.frame(
    region = c("Large", "Medium", "Small"),
    var = c(100, 400, 900)
  )
}

test_that("min_n must be a positive integer", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = -1),
    "positive integer"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = 2.5),
    "positive integer"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = c(2, 3)),
    "single positive integer"
  )
})

test_that("max_n must be a positive integer", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, max_n = -1),
    "positive integer"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, max_n = 2.5),
    "positive integer"
  )
})

test_that("min_n and max_n reject non-finite values", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = NA_real_),
    "single positive integer"
  )

  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, max_n = Inf),
    "single positive integer"
  )
})

test_that("min_n cannot exceed max_n", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = 50, max_n = 20),
    "cannot be greater than"
  )
})

test_that("min_n and max_n warn when no allocation method", {
  expect_warning(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = 100, min_n = 2),
    "applies only with `frac` or with an allocation method",
    class = "samplyr_warning_draw_argument_ignored"
  )

  expect_warning(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = 100, max_n = 50),
    "applies only with `frac` or with an allocation method",
    class = "samplyr_warning_draw_argument_ignored"
  )

  expect_no_warning(
    sampling_design() |>
      stratify_by(region) |>
      draw(frac = 0.1, min_n = 2, max_n = 50)
  )
})

test_that("min_n errors when constraint is infeasible", {
  frame <- make_unequal_frame()

  # 3 strata * 50 = 150 required, above n = 100.
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, min_n = 50) |>
      execute(frame, seed = 42),
    "Cannot satisfy minimum"
  )
})

test_that("max_n errors when constraint is infeasible", {
  frame <- make_unequal_frame()

  # 3 strata * 10 = 30 allowed, below n = 100.
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(n = 100, max_n = 10) |>
      execute(frame, seed = 42),
    "Cannot satisfy maximum"
  )
})

test_that("min_n ensures minimum per stratum with proportional allocation", {
  frame <- make_unequal_frame()

  # Proportional gives Small 5% of 100 = 5 units.
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 10) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  expect_true(all(counts >= 10))

  expect_equal(sum(counts), 100)

  expect_equal(as.numeric(counts["Small"]), 10)
})

test_that("min_n ensures minimum per stratum with Neyman allocation", {
  frame <- make_unequal_frame()
  var_df <- make_variance_df()

  result <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = var_df) |>
    draw(n = 100, min_n = 5) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  expect_true(all(counts >= 5))

  expect_equal(sum(counts), 100)
})

test_that("min_n ensures minimum with equal allocation", {
  frame <- make_unequal_frame()

  # Equal allocation gives 33-34 per stratum, so min_n = 35 needs n = 120.
  result <- sampling_design() |>
    stratify_by(region, alloc = "equal") |>
    draw(n = 120, min_n = 35) |>
    execute(frame, seed = 42)

  counts <- table(result$region)
  expect_true(all(counts >= 35))
  expect_equal(sum(counts), 120)
})

test_that("max_n caps large strata with proportional allocation", {
  frame <- make_unequal_frame()

  # Proportional gives Large 80 of 100.
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, max_n = 50) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  expect_true(all(counts <= 50))

  expect_equal(sum(counts), 100)

  expect_equal(as.numeric(counts["Large"]), 50)
})

test_that("max_n caps with Neyman allocation", {
  frame <- make_unequal_frame()
  var_df <- make_variance_df()

  result <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = var_df) |>
    draw(n = 100, max_n = 40) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  expect_true(all(counts <= 40))

  expect_equal(sum(counts), 100)
})

test_that("min_n and max_n work together", {
  frame <- make_unequal_frame()

  # Proportional gives Large ~80, Small ~5.
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 10, max_n = 50) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  expect_true(all(counts >= 10))
  expect_true(all(counts <= 50))

  expect_equal(sum(counts), 100)

  expect_equal(as.numeric(counts["Large"]), 50)
  expect_gte(as.numeric(counts["Small"]), 10)
})

test_that("tight bounds still work when feasible", {
  frame <- make_unequal_frame()

  # Each stratum in [30, 35] with total 99 is feasible as 33 + 33 + 33.
  result <- sampling_design() |>
    stratify_by(region, alloc = "equal") |>
    draw(n = 99, min_n = 30, max_n = 35) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  expect_true(all(counts >= 30))
  expect_true(all(counts <= 35))
  expect_equal(sum(counts), 99)
})

test_that("bounds work when stratum size < min_n", {
  small_frame <- data.frame(
    id = 1:100,
    region = c(rep("A", 80), rep("B", 15), rep("C", 5))
  )

  # C is capped at its 5 units, so the minimum total is 25 and n = 50 fits.
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 50, min_n = 10) |>
    execute(small_frame, seed = 42)

  counts <- table(result$region)

  expect_gte(as.numeric(counts["A"]), 10)
  expect_gte(as.numeric(counts["B"]), 10)

  expect_equal(as.numeric(counts["C"]), 5)

  expect_equal(sum(counts), 50)
})

test_that("bounds work with optimal allocation", {
  frame <- make_unequal_frame()
  var_df <- make_variance_df()
  cost_df <- data.frame(
    region = c("Large", "Medium", "Small"),
    cost = c(1, 2, 3)
  )

  result <- sampling_design() |>
    stratify_by(region, alloc = "optimal", variance = var_df, cost = cost_df) |>
    draw(n = 100, min_n = 5, max_n = 60) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  expect_true(all(counts >= 5))
  expect_true(all(counts <= 60))
  expect_equal(sum(counts), 100)
})

test_that("weights are correct when bounds applied", {
  frame <- make_unequal_frame()

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 10, max_n = 50) |>
    execute(frame, seed = 42)

  for (r in c("Large", "Medium", "Small")) {
    stratum_data <- result[result$region == r, ]
    n_h <- nrow(stratum_data)
    N_h <- sum(frame$region == r)
    expected_weight <- N_h / n_h

    expect_equal(length(unique(stratum_data$.weight)), 1)
    expect_equal(stratum_data$.weight[1], expected_weight, tolerance = 0.001)
  }
})

test_that("min_n and max_n are stored in design", {
  design <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 5, max_n = 50)

  draw_spec <- design$stages[[1]]$draw_spec

  expect_equal(draw_spec$min_n, 5)
  expect_equal(draw_spec$max_n, 50)
})

test_that("NULL bounds are stored correctly", {
  design <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100)

  draw_spec <- design$stages[[1]]$draw_spec

  expect_null(draw_spec$min_n)
  expect_null(draw_spec$max_n)
})

test_that("bounds converge with many strata within the iteration cap", {
  # The scale is found by bisection, so iterations do not grow with H > 50.
  n_strata <- 80
  sizes <- rep(c(500, 10), length.out = n_strata)
  frame <- data.frame(
    id = seq_len(sum(sizes)),
    stratum = rep(paste0("S", sprintf("%03d", seq_len(n_strata))), times = sizes)
  )

  # Small strata (N = 10) get proportional targets below min_n = 3.
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 400, min_n = 3, max_n = 20) |>
    execute(frame, seed = 42)

  counts <- table(result$stratum)
  expect_true(all(counts >= 3))
  expect_true(all(counts <= 20))
  expect_equal(sum(counts), 400)
})

test_that("bounds work with highly skewed population and tight bounds", {
  frame <- data.frame(
    id = 1:1000,
    stratum = c(rep("Huge", 900), rep("B", 50), rep("C", 30), rep("D", 20))
  )

  # Huge's target of 90 is capped at 30, so 60 units move to small strata.
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 100, min_n = 5, max_n = 30) |>
    execute(frame, seed = 42)

  counts <- table(result$stratum)
  expect_true(all(counts >= 5))
  expect_true(all(counts <= 30))
  expect_equal(sum(counts), 100)
})

test_that("bounds saturate correctly (all strata at min or max)", {
  frame <- data.frame(
    id = 1:1100,
    stratum = c(rep("A", 500), rep("B", 300), rep("C", 200),
                rep("D", 50), rep("E", 50))
  )

  # Proportional A=22.7, B=13.6, C=9.1, D=2.3, E=2.3 under min 8, max 12.
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 50, min_n = 8, max_n = 12) |>
    execute(frame, seed = 42)

  counts <- table(result$stratum)
  expect_true(all(counts >= 8))
  expect_true(all(counts <= 12))
  expect_equal(sum(counts), 50)
})

test_that("bounds work with equal allocation and many strata", {
  n_strata <- 100
  frame <- data.frame(
    id = seq_len(n_strata * 50),
    stratum = rep(paste0("S", seq_len(n_strata)), each = 50)
  )

  result <- sampling_design() |>
    stratify_by(stratum, alloc = "equal") |>
    draw(n = 500, min_n = 3, max_n = 10) |>
    execute(frame, seed = 42)

  counts <- table(result$stratum)
  expect_true(all(counts >= 3))
  expect_true(all(counts <= 10))
  expect_equal(sum(counts), 500)
})

test_that("Neyman allocation with extreme variance spread and bounds", {
  frame <- data.frame(
    id = 1:600,
    stratum = rep(c("Low", "Medium", "High"), each = 200)
  )
  var_df <- data.frame(
    stratum = c("High", "Low", "Medium"),
    var = c(10000, 1, 1)
  )

  result <- sampling_design() |>
    stratify_by(stratum, alloc = "neyman", variance = var_df) |>
    draw(n = 60, min_n = 10, max_n = 40) |>
    execute(frame, seed = 42)

  counts <- table(result$stratum)
  expect_true(all(counts >= 10))
  expect_true(all(counts <= 40))
  expect_equal(sum(counts), 60)
})


test_that("allocate_bounded uses ORIC tie-breaking without active bounds", {
  factors <- c(0.5, 2.5, 1)
  result <- samplyr:::allocate_bounded(
    factors,
    total = 4L,
    lower = rep(0, 3),
    upper = rep(4, 3)
  )

  expect_equal(result, c(0L, 3L, 1L))
  expect_equal(sum(result), 4L)
})

# Constrained allocation keeps its total and its own criterion once a stratum
# saturates. N_h is an upper bound with or without `max_n`, but not for
# with-replacement draws.

alloc_frame <- function(sizes, labels = LETTERS[seq_along(sizes)]) {
  data.frame(
    id = seq_len(sum(sizes)),
    h = rep(labels, times = sizes)
  )
}

# Capping reports itself with a message, tested at the end of this file, so
# the allocation-number tests silence it.
alloc_exec <- function(design, frame, ...) {
  suppressMessages(execute(design, frame, ...))
}

test_that("saturating allocation still delivers the requested total", {
  frame <- alloc_frame(c(10, 490, 500))
  variance <- data.frame(h = c("A", "B", "C"), var = c(100, 1, 1))

  design <- sampling_design() |>
    stratify_by(h, variance = variance, alloc = "neyman") |>
    draw(n = 300)
  result <- alloc_exec(design, frame, seed = 1)

  expect_equal(nrow(result), 300)
  expect_lte(max(table(result$h) - c(10, 490, 500)), 0)
})

test_that("min_n = 1 is a no-op on an allocation method", {
  frame <- alloc_frame(c(10, 490, 500))
  variance <- data.frame(h = c("A", "B", "C"), var = c(100, 1, 1))
  mk <- function(...) {
    sampling_design() |>
      stratify_by(h, variance = variance, alloc = "neyman") |>
      draw(n = 300, ...) |>
      alloc_exec(frame, seed = 1)
  }

  expect_identical(table(mk()$h), table(mk(min_n = 1)$h))
})

test_that("equal allocation redistributes past a small stratum", {
  result <- sampling_design() |>
    stratify_by(h, alloc = "equal") |>
    draw(n = 20) |>
    alloc_exec(alloc_frame(c(2, 98)), seed = 1)

  expect_equal(nrow(result), 20)
  expect_equal(as.vector(table(result$h)), c(2L, 18L))
})

test_that("redistribution follows the allocation factors, not spare capacity", {
  # Factors (1000, 1000, 500) with A saturated at 10 split 240 as 160/80.
  frame <- alloc_frame(c(10, 500, 500))
  variance <- data.frame(h = c("A", "B", "C"), var = c(10000, 4, 1))

  result <- sampling_design() |>
    stratify_by(h, variance = variance, alloc = "neyman") |>
    draw(n = 250) |>
    alloc_exec(frame, seed = 1)

  expect_equal(as.vector(table(result$h)), c(10L, 160L, 80L))
})

test_that("every allocation method preserves its total and respects N_h", {
  frame <- alloc_frame(c(10, 490, 500))
  aux <- data.frame(
    h = c("A", "B", "C"),
    var = c(100, 1, 1),
    cost = c(1, 2, 4),
    cv = c(0.5, 0.2, 0.3),
    importance = c(1, 2, 3)
  )
  N_h <- c(10, 490, 500)

  specs <- list(
    equal = function(d) stratify_by(d, h, alloc = "equal"),
    proportional = function(d) stratify_by(d, h, alloc = "proportional"),
    neyman = function(d) {
      stratify_by(d, h, variance = aux[c("h", "var")], alloc = "neyman")
    },
    optimal = function(d) {
      stratify_by(
        d, h,
        variance = aux[c("h", "var")],
        cost = aux[c("h", "cost")],
        alloc = "optimal"
      )
    },
    power = function(d) {
      stratify_by(
        d, h,
        cv = aux[c("h", "cv")],
        importance = aux[c("h", "importance")],
        alloc = "power"
      )
    }
  )

  for (name in names(specs)) {
    result <- sampling_design() |>
      specs[[name]]() |>
      draw(n = 300) |>
      alloc_exec(frame, seed = 2)

    expect_equal(nrow(result), 300, info = name)
    expect_true(all(as.vector(table(result$h)) <= N_h), info = name)
  }
})

test_that("explicit bounds preserve the total when feasible", {
  frame <- alloc_frame(c(5, 95))

  feasible <- sampling_design() |>
    stratify_by(h, alloc = "proportional") |>
    draw(n = 50, min_n = 10) |>
    execute(frame, seed = 1)

  expect_equal(nrow(feasible), 50)
  expect_equal(as.vector(table(feasible$h)), c(5L, 45L))
})

test_that("infeasible bounds raise typed errors", {
  frame <- alloc_frame(c(5, 95))
  mk <- function(...) {
    sampling_design() |>
      stratify_by(h, alloc = "proportional") |>
      draw(n = 50, ...) |>
      execute(frame, seed = 1)
  }

  expect_error(mk(min_n = 60), class = "samplyr_error_alloc_min_infeasible")
  # max_n = 10 allows at most 5 + 10 = 15 units.
  expect_error(mk(max_n = 10), class = "samplyr_error_alloc_max_infeasible")
})

test_that("a request above the population becomes a census", {
  frame <- alloc_frame(c(5, 95))

  expect_warning(
    result <- sampling_design() |>
      stratify_by(h, alloc = "proportional") |>
      draw(n = 150) |>
      execute(frame, seed = 1),
    class = "samplyr_warning_census"
  )

  expect_equal(nrow(result), 100)
  expect_equal(as.vector(table(result$h)), c(5L, 95L))
})

test_that("with-replacement allocation is not bounded by distinct units", {
  frame <- alloc_frame(c(10, 90))

  for (method in c("srswr", "pps_multinomial", "pps_chromy")) {
    mk <- function(...) {
      design <- if (method == "srswr") {
        sampling_design() |>
          stratify_by(h, alloc = "equal") |>
          draw(n = 150, method = method, ...)
      } else {
        frame$mos <- rep(c(1, 2), length.out = nrow(frame))
        sampling_design() |>
          stratify_by(h, alloc = "equal") |>
          draw(n = 150, method = method, mos = mos, ...)
      }
      execute(design, frame, seed = 1)
    }

    expect_equal(nrow(mk()), 150, info = method)
    expect_equal(nrow(mk(min_n = 1)), 150, info = method)
  }
})

test_that("zero-factor strata split what saturation leaves behind", {
  # Only A has a positive factor, so B and C split what it leaves equally.
  frame <- alloc_frame(c(5, 150, 150))
  variance <- data.frame(h = c("A", "B", "C"), var = c(1, 0, 0))

  result <- sampling_design() |>
    stratify_by(h, variance = variance, alloc = "neyman") |>
    draw(n = 45) |>
    alloc_exec(frame, seed = 1)

  expect_equal(as.vector(table(result$h)), c(5L, 20L, 20L))
})

test_that("bounded allocation holds its invariants under random inputs", {
  withr::with_seed(415, {
    for (i in seq_len(200)) {
      H <- sample(2:8, 1)
      N_h <- sample(5:200, H, replace = TRUE)
      factors <- runif(H, 0, 10)
      total <- sample(seq_len(sum(N_h)), 1)

      out <- samplyr:::allocate_bounded(factors, total, rep(0, H), N_h)

      expect_equal(sum(out), total)
      expect_true(all(out >= 0))
      expect_true(all(out <= N_h))
    }
  })
})

test_that("a binding lower bound releases an upper bound that looked binding", {
  # Freezing both apparent violations would leave 60 - 70 units for A.
  out <- samplyr:::allocate_bounded(
    factors = c(1, 10, 1),
    total = 60,
    lower = c(0, 0, 40),
    upper = c(100, 30, 100)
  )

  expect_equal(sum(out), 60)
  expect_equal(out, c(2L, 18L, 40L))
  expect_lt(out[2], 30)
})

test_that("a binding lower bound pushes a free stratum below its share", {
  # A is pinned by lower == upper, and B's lower bound beats the equal split.
  out <- samplyr:::allocate_bounded(
    factors = c(1, 40, 40),
    total = 27,
    lower = c(20, 5, 0),
    upper = c(20, 10, 10)
  )

  expect_equal(out, c(20L, 5L, 2L))
})

test_that("interior strata share one scale", {
  # Integerization moves an interior stratum by less than 1 / factor_h.
  withr::with_seed(416, {
    checked <- 0L
    for (i in seq_len(300)) {
      H <- sample(2:8, 1)
      factors <- runif(H, 0.5, 10)
      N_h <- sample(3:200, H, replace = TRUE)
      lower <- pmin(sample(0:15, H, replace = TRUE), N_h)
      upper <- pmax(pmin(N_h, sample(c(5, 20, 60, 200), H, replace = TRUE)), lower)
      total <- sum(lower) + floor(runif(1) * (sum(upper) - sum(lower) + 1))

      out <- samplyr:::allocate_bounded(factors, total, lower, upper)

      expect_equal(sum(out), total)
      expect_true(all(out >= ceiling(lower)))
      expect_true(all(out <= floor(upper)))

      interior <- out > lower & out < upper
      if (sum(interior) >= 2L) {
        checked <- checked + 1L
        ratio <- out[interior] / factors[interior]
        expect_lte(max(ratio) - min(ratio), max(1 / factors[interior]))
      }
    }
    expect_gt(checked, 50L)
  })
})

test_that("the solution depends on the factors only through their ratios", {
  # Tiny factors can overflow the search bracket to Inf.
  ref <- samplyr:::allocate_bounded(c(1, 2), 10, c(0, 0), c(10, 10))
  expect_identical(ref, c(3L, 7L))

  for (scale in c(1e-320, 1e-300, 1e-8, 1e8, 1e300)) {
    expect_identical(
      samplyr:::allocate_bounded(c(1, 2) * scale, 10, c(0, 0), c(10, 10)),
      ref
    )
  }
})

test_that("a factor far below its bound takes the share its ratio implies", {
  # The quotient overflows for the small factor alone.
  expect_identical(
    samplyr:::allocate_bounded(c(1e-320, 2), 10, c(0, 0), c(10, 10)),
    c(0L, 10L)
  )
  expect_identical(
    samplyr:::allocate_bounded(c(2, 1e-320), 10, c(0, 0), c(10, 10)),
    c(10L, 0L)
  )
})

test_that("min_n and N_h binding together still deliver the total", {
  # Factors (100, 1020, 100): B fills at 30 and A and C take 35 each.
  frame <- alloc_frame(c(100, 30, 100))
  variance <- data.frame(h = c("A", "B", "C"), var = c(1, 1156, 1))

  result <- sampling_design() |>
    stratify_by(h, variance = variance, alloc = "neyman") |>
    draw(n = 100, min_n = 25) |>
    alloc_exec(frame, seed = 1)

  expect_equal(nrow(result), 100)
  expect_equal(as.vector(table(result$h)), c(35L, 30L, 35L))
})

test_that("a saturating allocation replays identically everywhere", {
  frame <- alloc_frame(c(10, 490, 500))
  variance <- data.frame(h = c("A", "B", "C"), var = c(100, 1, 1))
  design <- sampling_design() |>
    stratify_by(h, variance = variance, alloc = "neyman") |>
    draw(n = 300)

  sample <- alloc_exec(design, frame, seed = 1, frame_digest = "full")
  realized <- as.vector(table(sample$h))

  expect_equal(frame_summary(sample, detail = "pool")$n_target, realized)
  expect_equal(
    samplyr::exante_digest(design, frame)$stages[[1]]$pools$n_target,
    realized
  )
})

test_that("capping the allocation reports once per execution", {
  frame <- alloc_frame(c(10, 490, 500))
  variance <- data.frame(h = c("A", "B", "C"), var = c(100, 1, 1))
  design <- sampling_design() |>
    stratify_by(h, variance = variance, alloc = "neyman") |>
    draw(n = 300)

  expect_message(
    execute(design, frame, seed = 1),
    class = "samplyr_message_allocation_capped"
  )

  count_messages <- function(expr) {
    n <- 0L
    withCallingHandlers(
      expr,
      samplyr_message_allocation_capped = function(m) {
        n <<- n + 1L
        invokeRestart("muffleMessage")
      }
    )
    n
  }

  expect_equal(count_messages(execute(design, frame, seed = 1)), 1L)

  # Recomputing the allocation for joint probabilities is not a new one.
  sample <- suppressMessages(execute(design, frame, seed = 1))
  expect_equal(count_messages(joint_expectation(sample, frame, stages = 1)), 0L)
})

test_that("an allocation that fits reports nothing", {
  frame <- alloc_frame(c(10, 490, 500))

  expect_no_message(
    sampling_design() |>
      stratify_by(h, alloc = "proportional") |>
      draw(n = 100) |>
      execute(frame, seed = 1),
    class = "samplyr_message_allocation_capped"
  )

  # A census warns on its own and does not also report redistribution.
  expect_no_message(
    suppressWarnings(
      sampling_design() |>
        stratify_by(h, alloc = "proportional") |>
        draw(n = 2000) |>
        execute(frame, seed = 1)
    ),
    class = "samplyr_message_allocation_capped"
  )
})

test_that("the reported redistribution counts whole units", {
  # Ideal 10.5/10.5 integerizes to 11/10, and A's realized 10 moves one unit.
  expect_message(
    sampling_design() |>
      stratify_by(h, alloc = "equal") |>
      draw(n = 21) |>
      execute(alloc_frame(c(10, 100)), seed = 1) |>
      invisible(),
    "1 unit redistributed",
    class = "samplyr_message_allocation_capped"
  )
})

test_that("a bound min_n releases does not report as capping", {
  # B's unconstrained share of 50 exceeds N = 30, but min_n = 20 gives 20/20/20.
  frame <- alloc_frame(c(100, 30, 100))
  variance <- data.frame(h = c("A", "B", "C"), var = c(1, (10 / 30 * 100)^2, 1))

  result <- expect_no_message(
    sampling_design() |>
      stratify_by(h, alloc = "neyman", variance = variance) |>
      draw(n = 60, min_n = 20) |>
      execute(frame, seed = 1),
    class = "samplyr_message_allocation_capped"
  )

  expect_equal(as.vector(table(result$h)), c(20L, 20L, 20L))
})

test_that("a bound that still binds under min_n reports", {
  # Without A's population bound the split would be 20/20/20.
  frame <- alloc_frame(c(10, 100, 100))

  expect_message(
    result <- sampling_design() |>
      stratify_by(h, alloc = "equal") |>
      draw(n = 60, min_n = 5) |>
      execute(frame, seed = 1),
    "10 units redistributed",
    class = "samplyr_message_allocation_capped"
  )

  expect_equal(as.vector(table(result$h)), c(10L, 25L, 25L))
})

test_that("the capping message names the strata and the units moved", {
  frame <- alloc_frame(c(10, 490, 500))
  variance <- data.frame(h = c("A", "B", "C"), var = c(100, 1, 1))

  expect_snapshot(
    sampling_design() |>
      stratify_by(h, variance = variance, alloc = "neyman") |>
      draw(n = 300) |>
      execute(frame, seed = 1) |>
      invisible()
  )
})

## Bounds on a sampling fraction

# Five counties of 13, 13, 15, 6 and 6 towns, town ids restarting in each
# county, and neighbourhoods numbered within each town.
spss_poll_frame <- function() {
  towns <- data.frame(
    county = rep(c("Central", "Eastern", "Northern", "Southern", "Western"),
                 c(13, 13, 15, 6, 6))
  )
  towns$town <- stats::ave(seq_len(nrow(towns)), towns$county, FUN = seq_along)
  towns$n_nbr <- (seq_len(nrow(towns)) %% 4L) + 2L
  frame <- towns[rep(seq_len(nrow(towns)), towns$n_nbr), c("county", "town")]
  frame$nbrhood <- stats::ave(seq_len(nrow(frame)),
                              frame$county, frame$town, FUN = seq_along)
  sizes <- 20L + (seq_len(nrow(frame)) * 7L) %% 31L
  frame <- frame[rep(seq_len(nrow(frame)), sizes), ]
  frame$voteid <- stats::ave(seq_len(nrow(frame)),
                             frame$county, frame$town, frame$nbrhood,
                             FUN = seq_along)
  frame$town_size <- stats::ave(frame$voteid, frame$county, frame$town,
                                FUN = length)
  rownames(frame) <- NULL
  frame
}

test_that("frac with min_n and max_n reproduces the SPSS rate rule", {
  frame <- spss_poll_frame()
  design <- sampling_design() |>
    add_stage("Town") |>
    stratify_by(county) |>
    cluster_by(county, town) |>
    draw(frac = 0.3, min_n = 3, max_n = 5, round = "nearest",
         method = "pps_sampford", mos = town_size) |>
    add_stage("Voters") |>
    stratify_by(nbrhood) |>
    draw(frac = 0.2, round = "nearest")

  s1 <- execute(design, frame, seed = 1, stages = 1)
  towns <- unique(as.data.frame(s1)[c("county", "town", "town_size", ".weight")])
  expect_identical(
    c(table(towns$county)),
    c(Central = 4L, Eastern = 4L, Northern = 5L, Southern = 3L, Western = 3L)
  )
  M_h <- c(table(frame$county))
  n_h <- c(table(towns$county))
  expect_equal(
    1 / towns$.weight,
    unname(n_h[towns$county] * towns$town_size / M_h[towns$county])
  )

  s <- as.data.frame(execute(design, frame, seed = 1))
  picked <- unique(s[c("county", "town")])
  pop <- merge(frame, picked)
  N <- stats::aggregate(voteid ~ county + town + nbrhood, pop, length)
  n <- stats::aggregate(voteid ~ county + town + nbrhood, s, length)
  both <- merge(N, n, by = c("county", "town", "nbrhood"))
  expect_identical(nrow(both), nrow(N))
  expect_identical(both$voteid.y, as.integer(floor(0.2 * both$voteid.x + 0.5)))

  pools <- frame_summary(design, frame, detail = "pool")
  stage1 <- pools[pools$stage == 1L, ]
  expect_identical(
    stats::setNames(stage1$n_target, stage1$county)[names(n_h)],
    stats::setNames(as.double(n_h), names(n_h))
  )
})

test_that("frac bounds apply per stratum without moving units", {
  frame <- data.frame(st = rep(c("a", "b", "c"), c(4, 20, 60)), id = 1:84)
  take <- function(...) {
    s <- execute(sampling_design() |> stratify_by(st) |> draw(...), frame,
                 seed = 1)
    c(table(s$st))
  }
  expect_identical(take(frac = 0.1, min_n = 5, max_n = 8),
                   c(a = 4L, b = 5L, c = 6L))
  expect_identical(take(frac = 0.5, max_n = 10), c(a = 2L, b = 10L, c = 10L))
  expect_identical(take(frac = c(a = 0.5, b = 0.1, c = 0.5), max_n = 10),
                   c(a = 2L, b = 2L, c = 10L))
  expect_identical(
    take(frac = data.frame(st = c("a", "b", "c"), frac = c(0.5, 0.1, 0.5)),
         min_n = 3, max_n = 10),
    c(a = 3L, b = 3L, c = 10L)
  )
  # Without replacement a stratum below min_n is a census, silently.
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(frac = 0.1, min_n = 5)
  expect_silent(s <- execute(design, frame, seed = 1))
  expect_identical(unique(s$.weight[s$st == "a"]), 1)
  # With replacement min_n is not capped by the stratum.
  expect_identical(take(frac = 0.1, min_n = 6, method = "srswr"),
                   c(a = 6L, b = 6L, c = 6L))
})

test_that("frac bounds apply to unstratified and later-stage pools", {
  frame <- data.frame(id = 1:84, x = seq(1, 10, length.out = 84))
  design <- sampling_design() |>
    draw(frac = 0.01, min_n = 5, method = "pps_brewer", mos = x)
  s <- execute(design, frame, seed = 1)
  expect_identical(nrow(s), 5L)
  expect_equal(1 / s$.weight, sondage::inclusion_prob(frame$x, 5)[s$id])
  expect_equal(diag(joint_expectation(s, frame)$stage_1), 1 / s$.weight)

  psus <- data.frame(psu = rep(1:6, each = 10), id = 1:60)
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage() |>
    draw(frac = 0.1, min_n = 2)
  s <- execute(design, psus, seed = 1)
  expect_identical(unname(c(table(s$psu))), c(2L, 2L, 2L))
})

test_that("frac bounds on a random-size method bound the expected size", {
  frame <- data.frame(st = rep(c("a", "b", "c"), c(4, 20, 60)), id = 1:84)
  design <- sampling_design() |>
    stratify_by(st) |>
    draw(frac = 0.1, min_n = 3, max_n = 4, method = "bernoulli")
  rates <- c(a = 3 / 4, b = 3 / 20, c = 4 / 60)
  s <- execute(design, frame, seed = 1)
  expect_equal(c(tapply(1 / s$.weight, s$st, unique))[names(rates)], rates)
  pools <- frame_summary(design, frame, detail = "pool")
  expect_equal(stats::setNames(pools$chance, pools$st)[names(rates)], rates)
  expect_equal(stats::setNames(pools$n_target, pools$st)[names(rates)],
               c(a = 3, b = 3, c = 4))
})

test_that("printed designs show min_n and max_n", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = 0.1, min_n = 3, max_n = 5)
  expect_match(
    paste(cli::ansi_strip(capture.output(print(design))), collapse = "\n"),
    "frac = 0.1 (per stratum), min_n = 3, max_n = 5, method = srswor",
    fixed = TRUE
  )
})

## Per-stratum n and an allocation method

test_that("per-stratum n is refused alongside alloc, as a table or a vector", {
  frame <- data.frame(id = 1:30, st = rep(c("a", "b"), c(10, 20)))
  table_n <- data.frame(st = c("a", "b"), n = c(5, 1))

  cnd <- expect_error(
    sampling_design() |>
      stratify_by(st, alloc = "proportional") |>
      draw(n = table_n),
    class = "samplyr_error_alloc_named_n_with_alloc"
  )
  msg <- cli::ansi_strip(conditionMessage(cnd))
  expect_match(msg, "`n` is a table of stratum sizes", fixed = TRUE)
  expect_match(msg, "alloc = \"proportional\"", fixed = TRUE)
  expect_match(msg, "remove `alloc`", fixed = TRUE)
  expect_match(msg, "such as their total `n = 6`", fixed = TRUE)

  cnd <- expect_error(
    sampling_design() |>
      stratify_by(st, alloc = "equal") |>
      draw(n = c(a = 5, b = 1)),
    class = "samplyr_error_alloc_named_n_with_alloc"
  )
  expect_match(cli::ansi_strip(conditionMessage(cnd)),
               "`n` is a named vector of stratum sizes", fixed = TRUE)

  # A design file that combines them is refused at execute().
  design <- sampling_design() |> stratify_by(st) |> draw(n = table_n)
  design$stages[[1]]$strata$alloc <- "proportional"
  expect_error(execute(design, frame, seed = 1),
               class = "samplyr_error_alloc_named_n_with_alloc")

  # Without alloc the table is drawn as given.
  s <- execute(sampling_design() |> stratify_by(st) |> draw(n = table_n),
               frame, seed = 1)
  expect_identical(c(table(s$st)), c(a = 5L, b = 1L))
})

test_that("a single named value is a stratum size, not a total", {
  frame <- data.frame(id = 1:30, st = rep(c("a", "b"), c(10, 20)),
                      g = rep(1:2, 15))
  # The name was dropped and 6 split as 2 and 4.
  cnd <- expect_error(
    sampling_design() |>
      stratify_by(st, alloc = "proportional") |>
      draw(n = c(a = 6)),
    class = "samplyr_error_alloc_named_n_with_alloc"
  )
  expect_match(cli::ansi_strip(conditionMessage(cnd)),
               "`n` is a named vector of stratum sizes", fixed = TRUE)
  # Unstratified, 6 was drawn from the whole frame.
  expect_error(sampling_design() |> draw(n = c(a = 6)),
               "Named `n` requires stratification",
               class = "samplyr_error_alloc_invalid_input_type")
  expect_error(sampling_design() |> draw(frac = c(a = 0.2)),
               "Named `frac` requires stratification",
               class = "samplyr_error_alloc_invalid_input_type")
  expect_error(sampling_design() |> stratify_by(st, g) |> draw(n = c(a = 6)),
               class = "samplyr_error_alloc_invalid_input_type")
  expect_error(sampling_design() |> stratify_by(st, g) |>
                 draw(frac = c(a = 0.2)),
               class = "samplyr_error_alloc_invalid_input_type")

  # A name that matches the one stratum is a valid take.
  one <- frame[frame$st == "a", ]
  s <- execute(sampling_design() |> stratify_by(st) |> draw(n = c(a = 6)),
               one, seed = 1)
  expect_identical(nrow(s), 6L)
})
