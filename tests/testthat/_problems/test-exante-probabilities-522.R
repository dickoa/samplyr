# Extracted from test-exante-probabilities.R:522

# prequel ----------------------------------------------------------------------
exante_register <- function(n = 60) {
  data.frame(
    uid = seq_len(n),
    size = rep(c(2, 5, 3, 8, 4, 6), length.out = n),
    st = rep(c("x", "y"), length.out = n),
    ea = rep(paste0("e", seq_len(n / 5)), each = 5),
    eapop = rep(c(10, 20, 30, 40, 15, 25, 35, 45, 12, 22, 32, 42),
                length.out = n / 5) |> rep(each = 5)
  )
}
expect_matches_execution <- function(design, register = exante_register(),
                                     seed = 1) {
  resolved <- exante_probabilities(design, register, key = uid)
  sample <- execute(design, register, seed = seed)

  expect_identical(nrow(resolved), nrow(register))
  expect_identical(resolved$uid, register$uid)
  expect_equal(
    resolved$probability[match(sample$uid, resolved$uid)],
    1 / sample$.weight
  )
  resolved
}
exante_stack_population <- function() {
  data.frame(
    person_id = 1:60,
    y = as.numeric(1:60),
    size = rep(c(2, 5, 3, 8, 4, 6), length.out = 60),
    in_a = rep(c(TRUE, FALSE), times = c(40, 20)),
    in_b = rep(c(FALSE, TRUE), times = c(20, 40))
  )
}
exante_stack <- function(population = exante_stack_population()) {
  register_a <- population[population$in_a, , drop = FALSE]
  register_b <- population[population$in_b, , drop = FALSE]
  stack_frames(
    a = sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size) |>
      execute(register_a, seed = 1),
    b = sampling_design() |>
      draw(n = 20) |>
      execute(register_b, seed = 2),
    membership = c(a = "in_a", b = "in_b"),
    key = person_id,
    overlaps = exante_overlaps(
      frames = list(a = register_a, b = register_b),
      by = c(person_id = "person_id")
    )
  )
}

# test -------------------------------------------------------------------------
frame <- data.frame(
    id = 1:4,
    g = c("a", NA, "b", "a"),
    mos = c(1, 2, NA, 4),
    prn = c(0.1, 0.2, 1.5, 0.4)
  )
designs <- list(
    missing_stratum = sampling_design() |> stratify_by(g) |> draw(n = 1),
    missing_mos = sampling_design() |>
      draw(n = 1, method = "pps_brewer", mos = mos),
    prn_out_of_range = sampling_design() |>
      draw(n = 1, method = "pps_poisson", mos = id, prn = prn)
  )
for (nm in names(designs)) {
    d <- designs[[nm]]
    expect_error(execute(d, frame, seed = 1),
                 class = "samplyr_error_frame_invalid", info = nm)
    expect_error(exante_probabilities(d, frame, key = id),
                 class = "samplyr_error_frame_invalid", info = nm)
    expect_error(exante_digest(d, frame),
                 class = "samplyr_error_frame_invalid", info = nm)
    expect_error(frame_summary(d, frame),
                 class = "samplyr_error_frame_invalid", info = nm)
  }
