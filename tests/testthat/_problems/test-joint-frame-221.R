# Extracted from test-joint-frame.R:221

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
frame <- data.frame(id = 1:30, mos = 1:30, y = 1:30)
sample <- sampling_design() |>
    draw(n = 10, method = "pps_sampford", mos = mos) |>
    execute(frame, seed = 3)
reference <- joint_expectation(sample, frame)[[1]]
edited <- sample
edited$y <- -edited$y
expect_error(joint_expectation(edited, frame),
               class = "samplyr_error_joint_frame_key")
one <- sample
one$y[1] <- 0
expect_error(joint_expectation(one, frame), "1 sampled unit matches no row")
added <- sample
added$z <- 1
frame_z <- frame
frame_z$z <- 2
expect_error(joint_expectation(added, frame_z),
               class = "samplyr_error_joint_frame_key")
