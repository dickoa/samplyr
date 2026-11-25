## Largest-remainder rounding of allocated targets

test_that("equal remainders go to the larger target whatever the rounding noise", {
  # 10, 250 and 40 of 300 at n = 100: every target has remainder 1/3 in exact
  # arithmetic, so the one unit left goes to the largest stratum.
  N <- c(10, 250, 40)
  frame <- data.frame(h = rep(c("a", "b", "c"), N), id = 1:300)
  s <- execute(
    sampling_design() |> stratify_by(h, alloc = "proportional") |>
      draw(n = 100),
    frame, seed = 1
  )
  expect_identical(as.vector(table(s$h)), c(3L, 84L, 13L))

  # Exact rational oracle over random proportional allocations, on the
  # cases whose cut falls inside a tie.
  withr::local_seed(1)
  checked <- 0L
  for (i in 1:5000) {
    H <- sample(2:6, 1)
    sizes <- sample(1:300, H, TRUE)
    n <- sample(1:sum(sizes), 1)
    whole <- (sizes * n) %/% sum(sizes)
    remainder <- (sizes * n) %% sum(sizes)
    short <- n - sum(whole)
    if (short == 0 || short >= H) next
    o <- order(-remainder, -whole, seq_len(H))
    if (remainder[o[short]] != remainder[o[short + 1]]) next
    checked <- checked + 1L
    expected <- whole
    expected[o[seq_len(short)]] <- expected[o[seq_len(short)]] + 1L
    got <- round_preserve_total_bounded(sizes / sum(sizes) * n, n,
                                        rep(0, H), sizes)
    expect_identical(as.integer(got), as.integer(expected), info = i)
  }
  expect_identical(checked, 49L)
})
