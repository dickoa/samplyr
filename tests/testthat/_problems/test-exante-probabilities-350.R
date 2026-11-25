# Extracted from test-exante-probabilities.R:350

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
skip_if_not_installed("survey")
population <- exante_stack_population()
register_a <- population[population$in_a, , drop = FALSE]
register_b <- population[population$in_b, , drop = FALSE]
pi_a <- exante_probabilities(
    sampling_design() |> draw(n = 10, method = "pps_brewer", mos = size),
    register_a, key = person_id
  )
pi_b <- exante_probabilities(
    sampling_design() |> draw(n = 20), register_b, key = person_id
  )
stated <- population
stated$pi_a <- ifelse(
    stated$in_a,
    pi_a$probability[match(stated$person_id, pi_a$person_id)], 0
  )
stated$pi_b <- ifelse(
    stated$in_b,
    pi_b$probability[match(stated$person_id, pi_b$person_id)], 0
  )
by_column <- stack_frames(
    a = sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size) |>
      execute(stated[stated$in_a, , drop = FALSE], seed = 1),
    b = sampling_design() |>
      draw(n = 20) |>
      execute(stated[stated$in_b, , drop = FALSE], seed = 2),
    membership = c(a = "in_a", b = "in_b"),
    key = person_id,
    overlaps = declared_overlaps(
      a = "pi_a", b = "pi_b", scale = "probabilities"
    )
  )
expect_equal(
    frame_component_overlaps(exante_stack(population), "a"),
    frame_component_overlaps(by_column, "a")
  )
expect_equal(
    coef(survey::svytotal(
      ~y, as_svydesign(exante_stack(population), estimator = "expected")
    )),
    coef(survey::svytotal(
      ~y, as_svydesign(by_column, estimator = "expected")
    ))
  )
