## F5. Probabilities resolved from a design, without drawing

# The oracle is the execution engine, not a second calculation: `.weight` is
# exactly 1 / pi for the units a design selected, so a resolver that agrees
# with it unit by unit agrees with the allocation, the chance method, the
# certainty handling and the multistage compounding at once. Every design
# shape below is checked that way, and a reimplementation of any one of those
# would be a worse test than the engine itself.

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

# Every selected unit's resolved probability against the weight execution gave
# it, plus the promise that the resolver returns the whole register.
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

## Agreement with execution

test_that("equal-probability designs resolve exactly", {
  expect_matches_execution(sampling_design() |> draw(n = 15))
  expect_matches_execution(
    sampling_design() |> draw(n = 10, method = "systematic")
  )
  expect_matches_execution(
    sampling_design() |> draw(frac = 0.3, method = "bernoulli")
  )
})

test_that("stratified designs resolve through their allocation", {
  proportional <- expect_matches_execution(
    sampling_design() |>
      stratify_by(st, alloc = "proportional") |>
      draw(n = 20)
  )
  expect_equal(unique(proportional$probability), 20 / 60)

  named <- expect_matches_execution(
    sampling_design() |> stratify_by(st) |> draw(n = c(x = 8, y = 5))
  )
  register <- exante_register()
  expect_equal(unique(named$probability[register$st == "x"]), 8 / 30)
  expect_equal(unique(named$probability[register$st == "y"]), 5 / 30)
})

test_that("unequal-probability designs resolve exactly", {
  # The case the frame digest cannot answer: a varying element-level chance is
  # stored there as quantile bins, so `frame_summary(detail = "unit")` returns
  # no rows at all. This resolver keeps the vector.
  design <- sampling_design() |>
    draw(n = 12, method = "pps_brewer", mos = size)
  register <- exante_register()

  expect_identical(
    frame_summary(design, register)$storage, "quantiles"
  )
  expect_identical(nrow(frame_summary(design, register, detail = "unit")), 0L)

  resolved <- expect_matches_execution(design)
  expect_gt(length(unique(resolved$probability)), 1)
  # Brewer honours the target size exactly, so the chances sum to n.
  expect_equal(sum(resolved$probability), 12)

  expect_matches_execution(
    sampling_design() |> draw(n = 12, method = "pps_poisson", mos = size)
  )
})

test_that("certainty units resolve to one", {
  resolved <- expect_matches_execution(
    sampling_design() |>
      draw(n = 12, method = "pps_brewer", mos = size, certainty_size = 7)
  )
  register <- exante_register()
  expect_true(all(resolved$probability[register$size >= 7] == 1))
  expect_true(all(resolved$probability[register$size < 7] < 1))
})

test_that("a multistage design resolves as the product of its stages", {
  register <- exante_register()
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(ea) |>
    draw(n = 4, method = "pps_brewer", mos = eapop) |>
    add_stage() |>
    draw(n = 2)

  resolved <- expect_matches_execution(design, register)

  # Stage 2 takes 2 of the 5 rows of any cluster, so the compound is the
  # cluster's own chance times 0.4 and every row of a cluster shares it.
  by_cluster <- split(resolved$probability, register$ea)
  expect_true(all(vapply(by_cluster, function(p) {
    length(unique(p)) == 1L
  }, logical(1))))

  first_stage <- sampling_design() |>
    cluster_by(ea) |>
    draw(n = 4, method = "pps_brewer", mos = eapop)
  clusters <- exante_probabilities(
    first_stage,
    register[!duplicated(register$ea), , drop = FALSE],
    key = ea
  )
  expect_equal(
    vapply(by_cluster, function(p) p[[1]], numeric(1))[clusters$ea],
    stats::setNames(clusters$probability * 0.4, clusters$ea)
  )
})

test_that("a stratified second stage resolves too", {
  register <- exante_register()
  register$block <- rep(c("a", "b"), length.out = nrow(register))
  expect_matches_execution(
    sampling_design() |>
      add_stage() |>
      cluster_by(ea) |>
      draw(n = 5, method = "pps_brewer", mos = eapop) |>
      add_stage() |>
      stratify_by(block) |>
      draw(n = 1),
    register
  )
})

test_that("no random numbers are drawn", {
  register <- exante_register()
  design <- sampling_design() |>
    draw(n = 12, method = "pps_brewer", mos = size)

  set.seed(99)
  before <- .Random.seed
  resolved <- exante_probabilities(design, register, key = uid)
  expect_identical(.Random.seed, before)

  # And so the answer does not depend on the seed at all.
  set.seed(1)
  expect_identical(exante_probabilities(design, register, key = uid), resolved)
})

## Refusals

test_that("a with-replacement stage has no probability to resolve", {
  register <- exante_register()
  for (method in c("srswr", "pps_multinomial", "pps_chromy")) {
    # `mos` means nothing to srswr, which warns; the refusal is the subject.
    design <- suppressWarnings(
      sampling_design() |> draw(n = 10, method = method, mos = size)
    )
    expect_error(
      exante_probabilities(design, register, key = uid),
      class = "samplyr_error_exante_unsupported"
    )
  }
  expect_error(
    exante_probabilities(
      sampling_design() |> draw(n = 10, method = "srswr"),
      register, key = uid
    ),
    regexp = "expected number of hits"
  )
})

test_that("an incomplete design is refused", {
  expect_error(
    exante_probabilities(
      sampling_design() |> stratify_by(st),
      exante_register(), key = uid
    ),
    class = "samplyr_error_exante_unsupported"
  )
  expect_error(
    exante_probabilities("not a design", exante_register(), key = uid),
    class = "samplyr_error_exante_unsupported"
  )
})

test_that("one register per stage is refused", {
  # The probability is compounded along the rows of one register, and the rows
  # of two do not correspond.
  register <- exante_register()
  design <- sampling_design() |>
    add_stage() |>
    cluster_by(ea) |>
    draw(n = 4) |>
    add_stage() |>
    draw(n = 2)

  expect_error(
    exante_probabilities(
      design,
      list(register[!duplicated(register$ea), ], register),
      key = uid
    ),
    class = "samplyr_error_exante_unsupported"
  )
})

test_that("the key must be a column, unique, and complete", {
  register <- exante_register()
  design <- sampling_design() |> draw(n = 10)

  expect_error(
    exante_probabilities(design, register, key = person_id),
    class = "samplyr_error_exante_key"
  )
  expect_error(
    exante_probabilities(design, register, key = "uid"),
    class = "samplyr_error_exante_key"
  )
  expect_error(
    exante_probabilities(design, register),
    class = "samplyr_error_exante_key"
  )

  repeated <- register
  repeated$uid[[2]] <- repeated$uid[[1]]
  expect_error(
    exante_probabilities(design, repeated, key = uid),
    class = "samplyr_error_exante_key"
  )

  missing <- register
  missing$uid[[3]] <- NA_integer_
  expect_error(
    exante_probabilities(design, missing, key = uid),
    class = "samplyr_error_exante_key"
  )
})

## exante_overlaps()

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

test_that("resolving a stack gives what stating the same numbers gives", {
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
})

test_that("the resolved chances are stored, and read by both routes", {
  skip_if_not_installed("survey")
  frames <- exante_stack()
  record <- attr(frames, "overlaps")

  expect_identical(record$scale, "probabilities")
  expect_null(record$cols)
  expect_identical(names(record$resolved), c("a", "b"))

  matrix <- frame_component_overlaps(frames, "a")
  expect_identical(colnames(matrix), c("a", "b"))
  expect_true(all(matrix[, "a"] > 0))
  expect_identical(matrix[, "b"] == 0, !frames[["a"]]$in_b)

  # A unit's own-frame chance is what its design weight says it was.
  expect_equal(matrix[, "a"], 1 / frames[["a"]]$.weight)

  # A PPS frame moves the whole stack onto a type its own design supports,
  # which is the F3 rule about `type` being one choice for the stack.
  expect_equal(
    coef(survey::svytotal(~y, as_svydesign(frames, estimator = "expected"))),
    coef(survey::svytotal(
      ~y,
      suppressWarnings(as_svrepdesign(
        frames, estimator = "expected", type = "subbootstrap",
        replicates = 40
      ))
    ))
  )
})

test_that("a register that is not what the design drew from is caught", {
  # The resolved own-frame chance has to be the selection that happened, so a
  # register missing units, or holding different sizes, cannot pass.
  population <- exante_stack_population()
  register_a <- population[population$in_a, , drop = FALSE]
  register_b <- population[population$in_b, , drop = FALSE]

  # Same units, different sizes: every key resolves, so the only thing that
  # can catch it is the diagonal.
  altered <- register_a
  altered$size <- rev(altered$size)

  expect_error(
    stack_frames(
      a = sampling_design() |>
        draw(n = 10, method = "pps_brewer", mos = size) |>
        execute(register_a, seed = 1),
      b = sampling_design() |> draw(n = 20) |> execute(register_b, seed = 2),
      membership = c(a = "in_a", b = "in_b"),
      key = person_id,
      overlaps = exante_overlaps(
        frames = list(a = altered, b = register_b),
        by = c(person_id = "person_id")
      )
    ),
    class = "samplyr_error_stack_frames_overlaps"
  )
  expect_error(
    stack_frames(
      a = sampling_design() |>
        draw(n = 10, method = "pps_brewer", mos = size) |>
        execute(register_a, seed = 1),
      b = sampling_design() |> draw(n = 20) |> execute(register_b, seed = 2),
      membership = c(a = "in_a", b = "in_b"),
      key = person_id,
      overlaps = exante_overlaps(
        frames = list(a = altered, b = register_b),
        by = c(person_id = "person_id")
      )
    ),
    regexp = "selection that happened"
  )
})

test_that("a member missing from another frame's register is refused", {
  population <- exante_stack_population()
  register_a <- population[population$in_a, , drop = FALSE]
  register_b <- population[population$in_b, , drop = FALSE]

  expect_error(
    stack_frames(
      a = sampling_design() |>
        draw(n = 10, method = "pps_brewer", mos = size) |>
        execute(register_a, seed = 1),
      b = sampling_design() |> draw(n = 20) |> execute(register_b, seed = 2),
      membership = c(a = "in_a", b = "in_b"),
      key = person_id,
      overlaps = exante_overlaps(
        # Drops the overlap, so a sampled frame-a unit that is also in b has
        # no resolvable chance there.
        frames = list(
          a = register_a,
          b = register_b[register_b$person_id > 40, ]
        ),
        by = c(person_id = "person_id")
      )
    ),
    regexp = "not in its register"
  )
})

test_that("the marker checks its own arguments", {
  register <- data.frame(person_id = 1:10)

  bad <- function(...) {
    expect_error(
      exante_overlaps(...),
      class = "samplyr_error_stack_frames_overlaps"
    )
  }
  bad(frames = register, by = c(person_id = "person_id"))
  bad(frames = list(register), by = c(person_id = "person_id"))
  bad(frames = list(a = register), by = "person_id")
  bad(frames = list(a = register), by = c(a = "x", b = "y"))

  spec <- exante_overlaps(
    frames = list(a = register, b = register),
    by = c(person_id = "person_id")
  )
  expect_s3_class(spec, "samplyr_exante_overlap_spec")
  expect_output(print(spec), "2 registers")
})

test_that("the registers must match the frames, and carry the key", {
  population <- exante_stack_population()
  register_a <- population[population$in_a, , drop = FALSE]
  register_b <- population[population$in_b, , drop = FALSE]
  components <- list(
    a = sampling_design() |>
      draw(n = 10, method = "pps_brewer", mos = size) |>
      execute(register_a, seed = 1),
    b = sampling_design() |> draw(n = 20) |> execute(register_b, seed = 2)
  )
  stack <- function(overlaps) {
    stack_frames(
      a = components$a, b = components$b,
      membership = c(a = "in_a", b = "in_b"), key = person_id,
      overlaps = overlaps
    )
  }

  expect_error(
    stack(exante_overlaps(
      list(a = register_a), by = c(person_id = "person_id")
    )),
    class = "samplyr_error_stack_frames_overlaps"
  )
  expect_error(
    stack(exante_overlaps(
      list(a = register_a, c = register_b), by = c(person_id = "person_id")
    )),
    class = "samplyr_error_stack_frames_overlaps"
  )
  expect_error(
    stack(exante_overlaps(
      list(a = register_a, b = register_b), by = c(missing_id = "person_id")
    )),
    class = "samplyr_error_stack_frames_overlaps"
  )
})

test_that("print says the chances were resolved rather than declared", {
  output <- capture.output(print(exante_stack()))
  expect_true(any(grepl("resolved from the registers", output)))
})
