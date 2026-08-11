make_unequal_frame <- function() {
  set.seed(42)
  data.frame(
    id = 1:1000,
    region = c(
      rep("Large", 800), # 80% of population
      rep("Medium", 150), # 15% of population
      rep("Small", 50) # 5% of population
    ),
    income = rlnorm(1000, meanlog = 10, sdlog = 0.5)
  )
}

make_variance_df <- function() {
  data.frame(
    region = c("Large", "Medium", "Small"),
    var = c(100, 400, 900) # Small stratum has highest variance
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
      stratify_by(region) |> # No alloc specified
      draw(n = 100, min_n = 2),
    "only applies when an allocation method"
  )

  expect_warning(
    sampling_design() |>
      stratify_by(region) |>
      draw(n = 100, max_n = 50),
    "only applies when an allocation method"
  )
})

test_that("min_n errors when constraint is infeasible", {
  frame <- make_unequal_frame()

  # 3 strata * 50 min = 150 required, but only asking for 100
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

  # 3 strata * 10 max = 30 allowed, but asking for 100
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

  # Without min_n, proportional would give Small stratum very few units
  # 5% of 100 = 5 units

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 10) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # All strata should have at least min_n
  expect_true(all(counts >= 10))

  # Total should still be exactly 100
  expect_equal(sum(counts), 100)

  # Small stratum should have exactly 10 (raised from ~5)
  expect_equal(as.numeric(counts["Small"]), 10)
})

test_that("min_n ensures minimum per stratum with Neyman allocation", {
  frame <- make_unequal_frame()
  var_df <- make_variance_df()

  # Neyman allocation with high variance in Small stratum
  # But Small has low N_h, so allocation might still be low

  result <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = var_df) |>
    draw(n = 100, min_n = 5) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # All strata should have at least min_n
  expect_true(all(counts >= 5))

  # Total should be exactly 100
  expect_equal(sum(counts), 100)
})

test_that("min_n ensures minimum with equal allocation", {
  frame <- make_unequal_frame()

  # Equal allocation gives 33-34 per stratum for n=100
  # min_n = 40 should force all to at least 40
  # But that requires 120 total, which is infeasible for n=100

  # Test with feasible min_n
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

  # Proportional allocation would give Large stratum 80 units (80% of 100)
  # max_n = 50 should cap it

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, max_n = 50) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # No stratum should exceed max_n
  expect_true(all(counts <= 50))

  # Total should still be exactly 100
  expect_equal(sum(counts), 100)

  # Large stratum should be capped at 50 (down from ~80)
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

  # No stratum should exceed max_n
  expect_true(all(counts <= 40))

  # Total should be exactly 100
  expect_equal(sum(counts), 100)
})

test_that("min_n and max_n work together", {
  frame <- make_unequal_frame()

  # Proportional would give: Large ~80, Medium ~15, Small ~5
  # With min_n=10, max_n=50: Large capped at 50, Small raised to 10

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100, min_n = 10, max_n = 50) |>
    execute(frame, seed = 42)

  counts <- table(result$region)

  # All within bounds
  expect_true(all(counts >= 10))
  expect_true(all(counts <= 50))

  # Total correct
  expect_equal(sum(counts), 100)

  # Large capped, Small raised
  expect_equal(as.numeric(counts["Large"]), 50)
  expect_gte(as.numeric(counts["Small"]), 10)
})

test_that("tight bounds still work when feasible", {
  frame <- make_unequal_frame()

  # 3 strata, n = 99, min_n = 30, max_n = 35
  # Forces each to be in [30, 35], total must be 99
  # Feasible: 33 + 33 + 33 = 99

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
  # Create frame where one stratum is smaller than desired min_n
  small_frame <- data.frame(
    id = 1:100,
    region = c(rep("A", 80), rep("B", 15), rep("C", 5)) # C has only 5 units
  )

  # Asking for min_n = 10 but C only has 5 units
  # Should cap at population size (take all 5 from C)
  # Effective minimums: A=10, B=10, C=5 (capped at pop)
  # Total minimum = 25, so n=50 is feasible
  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 50, min_n = 10) |>
    execute(small_frame, seed = 42)

  counts <- table(result$region)

  # A and B should have at least 10
  expect_gte(as.numeric(counts["A"]), 10)
  expect_gte(as.numeric(counts["B"]), 10)

  # C should have all 5 (capped by population)
  expect_equal(as.numeric(counts["C"]), 5)

  # Total should still be 50
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

  # Check weights are inverse of selection probabilities
  for (r in c("Large", "Medium", "Small")) {
    stratum_data <- result[result$region == r, ]
    n_h <- nrow(stratum_data)
    N_h <- sum(frame$region == r)
    expected_weight <- N_h / n_h

    # All units in stratum should have same weight
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

test_that("bounds work with many strata (iteration cap regression)", {
  # Verifies that the bounded solver converges for H > 50 strata. The scale
  # is found by bisection, so the iteration count does not grow with H; an
  # earlier implementation capped iterations at 50 and could fail for large H.
  n_strata <- 80
  sizes <- rep(c(500, 10), length.out = n_strata)
  frame <- data.frame(
    id = seq_len(sum(sizes)),
    stratum = rep(paste0("S", sprintf("%03d", seq_len(n_strata))), times = sizes)
  )

  # Small strata (N=10) get proportional targets < min_n=3,
  # forcing redistribution from large strata across many iterations
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
  # One dominant stratum with 90% of population, tight max forces redistribution
  frame <- data.frame(
    id = 1:1000,
    stratum = c(rep("Huge", 900), rep("B", 50), rep("C", 30), rep("D", 20))
  )

  # Proportional target for Huge: 900/1000 * 100 = 90, capped at 30
  # 60 excess units must redistribute to 3 small strata
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
  # 5 strata: proportional gives very unequal allocation
  # Tight bounds force all strata to either min or max
  frame <- data.frame(
    id = 1:1100,
    stratum = c(rep("A", 500), rep("B", 300), rep("C", 200),
                rep("D", 50), rep("E", 50))
  )

  # n=50, min=8, max=12: feasible (5*8=40 <= 50 <= 5*12=60)
  # Proportional: A=22.7, B=13.6, C=9.1, D=2.3, E=2.3
  # D and E forced to 8, A capped at 12, remainder to B and C
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
  # Equal allocation with 100 strata: each gets n/100
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
  # One stratum has much higher variance, pulling allocation strongly
  # Bounds prevent over/under-allocation
  frame <- data.frame(
    id = 1:600,
    stratum = rep(c("Low", "Medium", "High"), each = 200)
  )
  var_df <- data.frame(
    stratum = c("High", "Low", "Medium"),
    var = c(10000, 1, 1) # extreme spread
  )

  # Neyman gives almost everything to High, bounds prevent this
  result <- sampling_design() |>
    stratify_by(stratum, alloc = "neyman", variance = var_df) |>
    draw(n = 60, min_n = 10, max_n = 40) |>
    execute(frame, seed = 42)

  counts <- table(result$stratum)
  expect_true(all(counts >= 10))
  expect_true(all(counts <= 40))
  expect_equal(sum(counts), 60)
})


test_that("round_preserve_total uses ORIC tie-breaking: larger stratum wins", {
  # Three strata with equal fractional remainders, scrambled position order.
  # x = c(0.5, 2.5, 1.5), n = 4: shortfall = 1, all frac = 0.5.
  # Position-based tie-break: index 1 (floor = 0) gets +1 -> c(1, 2, 1).
  # ORIC tie-break: index 2 (floor = 2, largest) gets +1 -> c(0, 3, 1).
  x <- c(0.5, 2.5, 1.5)
  result <- samplyr:::round_preserve_total(x, 4L)
  expect_equal(result, c(0L, 3L, 1L))
  expect_equal(sum(result), 4L)
})

test_that("round_preserve_total total is always preserved", {
  set.seed(99)
  for (i in 1:20) {
    x <- runif(10, 0, 5)
    n <- as.integer(round(sum(x)))
    result <- samplyr:::round_preserve_total(x, n)
    expect_equal(sum(result), n)
    expect_true(all(result >= 0L))
  }
})

# Constrained allocation. An allocation method must preserve its requested
# total and its own criterion once a stratum saturates. The population size
# is an upper bound whether or not `max_n` was supplied, and it is not a
# bound at all for with-replacement draws.

alloc_frame <- function(sizes, labels = LETTERS[seq_along(sizes)]) {
  data.frame(
    id = seq_len(sum(sizes)),
    h = rep(labels, times = sizes)
  )
}

# These tests are about the allocation numbers. Capping reports itself with a
# message, which has its own tests at the end of this block; silence it here
# so a saturating fixture does not fill the console.
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
  # Neyman factors N_h * sqrt(var) are (1000, 1000, 500). Once A saturates
  # at 10, the free strata must split 240 as 2:1, giving 160/80.
  # Redistributing in proportion to unused capacity would give 142/98 and
  # quietly stop being a Neyman allocation.
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

  # min_n exceeds stratum A, which is structurally capped at its population.
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
  # N_h is not a bound on the number of draws. Adding a nominal min_n used
  # to make this design abort with a maximum-bound error.
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
  # A carries the only positive Neyman factor and saturates at 5. The
  # criterion is then indifferent between B and C, so they split equally.
  frame <- alloc_frame(c(5, 150, 150))
  variance <- data.frame(h = c("A", "B", "C"), var = c(1, 0, 0))

  result <- sampling_design() |>
    stratify_by(h, variance = variance, alloc = "neyman") |>
    draw(n = 45) |>
    alloc_exec(frame, seed = 1)

  expect_equal(as.vector(table(result$h)), c(5L, 20L, 20L))
})

test_that("allocate_bounded reduces to ORIC rounding when no bound binds", {
  # The load-bearing assumption of routing every allocation through the
  # bounded path: with slack bounds the two rounders must agree exactly.
  withr::with_seed(414, {
    for (i in seq_len(200)) {
      H <- sample(2:8, 1)
      factors <- runif(H, 0.1, 10)
      total <- sample(20:200, 1)
      upper <- rep(total, H)
      target <- total * factors / sum(factors)

      expect_identical(
        samplyr:::allocate_bounded(factors, total, rep(0, H), upper),
        samplyr:::round_preserve_total(target, total)
      )
    }
  })
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
  # Unconstrained shares of 60 over factors (1, 10, 1) are 4.5/54.5/4.5, so C
  # looks short of its lower bound of 40 and B looks over its upper bound of
  # 30. Freezing both leaves 60 - 70 units to place in A. Solving the monotone
  # equation instead pins C at 40 and shares the remaining 20 at the factor
  # ratio, where B's upper bound never binds at all.
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
  # A is pinned by lower == upper, leaving 7 units for B and C at equal
  # factors. B's lower bound of 5 takes precedence over the equal split.
  out <- samplyr:::allocate_bounded(
    factors = c(1, 40, 40),
    total = 27,
    lower = c(20, 5, 0),
    upper = c(20, 10, 10)
  )

  expect_equal(out, c(20L, 5L, 2L))
})

test_that("interior strata share one scale", {
  # Characterization of the bounded solution: every stratum that no bound
  # touches sits at a common lambda = n_h / factor_h. Integerization moves a
  # stratum by less than one unit, so the spread in that ratio stays within
  # 1 / factor_h. A scheme that redistributes by spare capacity, or that
  # freezes violated bounds pass by pass, breaks this.
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
  # The bracket for the scale search is a bound divided by a factor, so a
  # uniformly tiny factor vector can overflow it to Inf and collapse the
  # search onto a degenerate interval. Scaling the whole vector must not
  # change the allocation at any magnitude, subnormal included.
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
  # The quotient overflows for the small factor alone, so the bracket cannot
  # come from it. The ratio still says the second stratum takes everything.
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
  # The released-upper-bound shape through the public API. Neyman factors are
  # N_h * sqrt(var) = (100, 1020, 100); min_n lifts every lower bound to 25
  # and B's population caps it at 30. The scale settles at 0.35, so B fills
  # its stratum and A and C take 35 each.
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

  # Recomputing the same allocation to recover joint probabilities is not a
  # second allocation, so it must stay quiet.
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

  # A census warns on its own; it must not also report redistribution.
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
  # Rounding the continuous overshoot reported "0 units redistributed" here:
  # the ideal split of 21 is 10.5/10.5, A overshoots its population by half a
  # unit, and one whole unit really does move. The realized 10/11 against an
  # integerized ideal of 11/10 is what the message must describe.
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
  # The same released-upper-bound shape the solver had to learn, one level up
  # in the diagnostic. Neyman factors (100, 1000, 100) on populations
  # (100, 30, 100) with n = 60: B's unconstrained share is 50, above its
  # population of 30, so a test against the unconstrained targets calls it
  # capped. But min_n = 20 lifts every stratum to 20 and consumes the whole
  # sample, so the solution is 20/20/20 and B never reaches 30.
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
  # The other half: min_n present and a population bound genuinely active.
  # Equal allocation of 60 over (10, 100, 100) with min_n = 5 gives 10/25/25,
  # against 20/20/20 without the population bound, so A gave up 10 units.
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
