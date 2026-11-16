# Extracted from test-wr-certainty.R:206

# prequel ----------------------------------------------------------------------
wr_cert_methods <- c("pps_multinomial", "pps_chromy", "pps_systematic_mr")
wr_cert_frame <- function() {
  data.frame(psu = 1:6, mos = c(100, 40, 30, 15, 10, 5), y = c(9, 4, 6, 2, 3, 1))
}
cascade_frame <- function() {
  data.frame(psu = 1:8, mos = c(100, 60, 35, 15, 10, 8, 7, 5), y = 1:8)
}
wr_cert_design <- function(method, ...) {
  sampling_design() |>
    cluster_by(psu) |>
    draw(n = 5, method = method, mos = mos, ...)
}
systematic_mr_moments <- function(sizes, n) {
  cum <- c(0, cumsum(sizes))
  step <- sum(sizes) / n
  cuts <- unlist(lapply(0:(n - 1), function(k) cum - k * step))
  cuts <- sort(unique(c(0, step, cuts[cuts > 0 & cuts < step])))
  e <- numeric(length(sizes))
  j <- matrix(0, length(sizes), length(sizes))
  for (k in seq_len(length(cuts) - 1L)) {
    start <- (cuts[k] + cuts[k + 1L]) / 2
    share <- (cuts[k + 1L] - cuts[k]) / step
    hits <- tabulate(
      findInterval(start + (0:(n - 1)) * step, cum), length(sizes)
    )
    e <- e + share * hits
    j <- j + share * outer(hits, hits)
  }
  list(e = e, j = j)
}
with_certainty_moments <- function(n_cert, rest) {
  e <- c(rep(1, n_cert), rest$e)
  j <- outer(e, e)
  rest_pos <- n_cert + seq_along(rest$e)
  j[rest_pos, rest_pos] <- rest$j
  list(e = e, j = j)
}

# test -------------------------------------------------------------------------
frame <- data.frame(
    id = 1:300, st = rep(c("a", "b"), each = 150),
    m = rep(c(400, rep(2, 149)), 2)
  )
for (method in wr_cert_methods) {
    design <- sampling_design() |> stratify_by(st) |>
      draw(n = 4, method = method, mos = m, certainty_size = 100)
    before <- frame_summary(design, frame)$n_certainty
    expect_identical(before, c(1, 1), info = method)
    for (digest in c("summary", "full")) {
      s <- execute(design, frame, seed = 1, frame_digest = digest)
      if (digest == "summary") {
        expect_null(get_frame_digest(s)$stages[[1]]$units)
      }
      expect_identical(frame_summary(s)$n_certainty, before,
                       info = paste(method, digest))
    }
    reps <- execute(design, frame, seed = 1, reps = 2)
    expect_identical(frame_summary(reps)$n_certainty, before, info = method)
    # Without a rule a stage drawn with replacement names none.
    bare <- sampling_design() |> stratify_by(st) |>
      draw(n = 4, method = method, mos = m) |>
      execute(frame, seed = 1)
    expect_true(all(is.na(frame_summary(bare)$n_certainty)), info = method)
  }
