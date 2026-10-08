## Spatially correlated Poisson sampling with permanent random numbers.
## With `prn`, scps visits each pool in row order and selects a unit when its
## random number is below its current probability, so a sample is fixed by
## the probabilities, the spread variables, the PRNs and the row order.

scps_frame <- function(seed = 1, N = 60) {
  withr::with_seed(seed, {
    data.frame(
      id = sample(N),
      stratum = rep(c("a", "b"), length.out = N),
      size = sample(5:40, N, replace = TRUE),
      x = stats::runif(N),
      y = stats::runif(N),
      burden = rep(c(1, 0, 0, 0), length.out = N),
      prn = stats::runif(N)
    )
  })
}

## The ids sondage selects on each stratum's pool, in frame order.
scps_direct <- function(frame, n, spread, prn, strata = NULL) {
  pools <- if (is.null(strata)) list(seq_len(nrow(frame))) else
    split(seq_len(nrow(frame)), frame[[strata]])
  unlist(lapply(pools, function(rows) {
    pool <- frame[rows, ]
    pik <- sondage::inclusion_prob(pool$size, n)
    pool$id[sondage::balanced_wor(
      pik,
      spread = as.matrix(pool[, spread, drop = FALSE]),
      method = "scps",
      prn = pool[[prn]]
    )$sample]
  }), use.names = FALSE)
}

test_that("scps with prn selects what sondage selects on the same pool", {
  frame <- scps_frame()
  s <- sampling_design() |>
    draw(n = 12, method = "scps", mos = size, spread = c(x, y), prn = prn) |>
    execute(frame, seed = 1)
  expect_setequal(s$id, scps_direct(frame, 12, c("x", "y"), "prn"))

  s <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 6, method = "scps", mos = size, spread = c(x, y), prn = prn) |>
    execute(frame, seed = 1)
  expect_setequal(s$id, scps_direct(frame, 6, c("x", "y"), "prn", "stratum"))
  # The weights are those of the planned probabilities, whatever the PRNs.
  pik <- stats::ave(frame$size, frame$stratum,
                    FUN = function(m) sondage::inclusion_prob(m, 6))
  expect_equal(1 / s$.weight, pik[match(s$id, frame$id)], tolerance = 1e-12)
})

test_that("scps with prn does not depend on the seed", {
  frame <- scps_frame()
  d <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 6, method = "scps", mos = size, spread = c(x, y), prn = prn)
  expect_identical(
    sort(execute(d, frame, seed = 1)$id),
    sort(execute(d, frame, seed = 99)$id)
  )
})

test_that("prn and 1 - prn coordinate two scps samples negatively", {
  frame <- scps_frame(N = 40)
  overlap <- function(u, v, spread) {
    frame$u <- u
    frame$v <- v
    draw_with <- function(prn) {
      sampling_design() |>
        draw(n = 10, method = "scps", mos = size, spread = !!spread,
             prn = !!rlang::sym(prn)) |>
        execute(frame, seed = 1)
    }
    s1 <- draw_with("u")
    s2 <- draw_with("v")
    both <- intersect(s1$id, s2$id)
    c(all = length(both), burdened = sum(frame$burden[frame$id %in% both]))
  }
  R <- 150L
  runs <- lapply(seq_len(R), function(r) {
    u <- withr::with_seed(r, stats::runif(nrow(frame)))
    w <- withr::with_seed(r + 10000L, stats::runif(nrow(frame)))
    # Spread on location for the overlap, on burden for burdened units.
    rbind(
      negative = overlap(u, 1 - u, rlang::expr(c(x, y))),
      independent = overlap(u, w, rlang::expr(c(x, y))),
      ascp = overlap(u, 1 - u, rlang::expr(burden)),
      uncoordinated = overlap(u, w, rlang::expr(burden))
    )
  })
  stat <- function(a, b, k) {
    d <- vapply(runs, function(m) m[a, k] - m[b, k], numeric(1))
    c(mean = mean(d), se = stats::sd(d) / sqrt(R))
  }
  # Paired differences, each below zero by more than three standard errors.
  all <- stat("negative", "independent", "all")
  expect_lt(all[["mean"]], -3 * all[["se"]])
  burdened <- stat("ascp", "uncoordinated", "burdened")
  expect_lt(burdened[["mean"]], -3 * burdened[["se"]])
})

test_that("scps executes on probabilities that do not sum exactly", {
  # Within regions, inclusion_prob(employees, 20) sums to 20 plus residues
  # up to 1.3e-12, which once made every scps draw fail.
  frame <- ken_enterprises
  frame$prn <- withr::with_seed(5, stats::runif(nrow(frame)))
  spread <- rlang::expr(c(year_established, revenue_millions))
  plain <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 20, method = "scps", mos = employees, spread = !!spread)
  with_prn <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 20, method = "scps", mos = employees, spread = !!spread,
         prn = prn)
  for (seed in 1:5) {
    s <- execute(plain, frame, seed = seed)
    expect_identical(nrow(s), 20L * length(unique(frame$region)))
  }
  expect_identical(nrow(execute(with_prn, frame, seed = 1)),
                   20L * length(unique(frame$region)))
})

test_that("prn is refused with balanced methods that cannot use it", {
  frame <- scps_frame()
  for (method in c("lpm2", "cube")) {
    args <- list(n = 10, method = method, mos = quote(size), prn = quote(prn))
    if (method == "lpm2") args$spread <- quote(c(x, y))
    err <- tryCatch(
      do.call(draw, c(list(sampling_design()), args)),
      error = identity
    )
    expect_s3_class(err, "samplyr_error_draw_method_argument")
    expect_match(conditionMessage(err), "scps", fixed = TRUE)
  }
})

test_that("a registered balanced method declaring prn support receives it", {
  frame <- scps_frame()
  received <- NULL
  on.exit(sondage::unregister_method("test_prn_balanced"), add = TRUE)
  sondage::register_method(
    "test_prn_balanced", "balanced",
    sample_fn = function(pik, aux = NULL, spread = NULL, prn = NULL, ...) {
      received <<- prn
      order(prn)[seq_len(round(sum(pik)))]
    },
    supports_prn = TRUE,
    probabilities = "exact"
  )
  s <- sampling_design() |>
    draw(n = 10, method = "balanced_test_prn_balanced", prn = prn) |>
    execute(frame, seed = 1)
  expect_identical(received, frame$prn)
  expect_setequal(s$id, frame$id[order(frame$prn)[1:10]])

  on.exit(sondage::unregister_method("test_no_prn_balanced"), add = TRUE)
  sondage::register_method(
    "test_no_prn_balanced", "balanced",
    sample_fn = function(pik, ...) seq_len(round(sum(pik))),
    probabilities = "exact"
  )
  expect_error(
    sampling_design() |>
      draw(n = 10, method = "balanced_test_no_prn_balanced", prn = prn),
    class = "samplyr_error_draw_method_argument"
  )
})

test_that("a clustered scps stage takes a prn constant within clusters", {
  frame <- scps_frame()
  frame$cl <- (seq_len(nrow(frame)) - 1L) %/% 4L + 1L
  frame$cx <- frame$cl / 15
  frame$cprn <- withr::with_seed(2, stats::runif(max(frame$cl)))[frame$cl]
  d <- sampling_design() |>
    cluster_by(cl) |>
    draw(n = 4, method = "scps", spread = cx, prn = cprn)
  s <- execute(d, frame, seed = 1)
  expect_length(unique(s$cl), 4L)
  expect_identical(sort(unique(s$cl)), sort(unique(execute(d, frame, seed = 7)$cl)))

  frame$cprn[1] <- 0.999
  expect_error(execute(d, frame, seed = 1),
               class = "samplyr_error_frame_cluster_invariant")
})

test_that("an scps design with prn writes, reads and replays to the same sample", {
  frame <- scps_frame()
  d <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 6, method = "scps", mos = size, spread = c(x, y), prn = prn)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(d, path, frame = frame)
  rd <- read_design(path)
  s0 <- execute(d, frame, seed = 1)
  expect_identical(sort(execute(rd, frame, seed = 3)$id), sort(s0$id))

  spath <- withr::local_tempfile(fileext = ".json")
  write_design(s0, spath, frame = frame)
  expect_identical(sort(replay_design(read_design(spath), frame)$id), sort(s0$id))
})

test_that("with prn, scps follows the row order, or control when given", {
  frame <- scps_frame()
  reversed <- frame[rev(seq_len(nrow(frame))), ]
  ids <- function(d, f) sort(execute(d, f, seed = 1)$id)
  by_rows <- sampling_design() |>
    draw(n = 12, method = "scps", mos = size, spread = c(x, y), prn = prn)
  expect_false(identical(ids(by_rows, frame), ids(by_rows, reversed)))
  by_id <- sampling_design() |>
    draw(n = 12, method = "scps", mos = size, spread = c(x, y), prn = prn,
         control = id)
  expect_identical(ids(by_id, frame), ids(by_id, reversed))
})
