## The per-stage description of an executed sample
#
# Every field is checked against a value computed from the sample's own
# columns or from the design, not read back through the export.

spec_frame <- function() {
  set.seed(4)
  psu <- data.frame(
    psu = 1:40,
    a = rep(c("u", "v"), 20),
    x = c(400, 350, round(stats::runif(38, 5, 40)))
  )
  frame <- psu[rep(1:40, each = 6), ]
  frame$eid <- seq_len(nrow(frame))
  frame$h <- rep(c("m", "f"), length.out = nrow(frame))
  frame$y <- stats::rnorm(nrow(frame), 10)
  frame
}

sample_spec <- function(sample) {
  export_stage_spec(
    as.data.frame(sample),
    get_design(sample),
    get_stages_executed(sample)
  )
}

test_that("the spec describes a stratified PPS stage and an element stage", {
  s <- sampling_design() |>
    add_stage() |>
    stratify_by(a) |>
    cluster_by(psu) |>
    draw(n = 4, method = "pps_brewer", mos = x) |>
    add_stage() |>
    stratify_by(h) |>
    draw(n = 2) |>
    # Reversed, so first appearance is not the sorted order of the ids.
    execute(spec_frame()[240:1, ], seed = 1)
  df <- as.data.frame(s)
  spec <- sample_spec(s)

  expect_s3_class(spec, "samplyr_export_spec")
  expect_identical(spec$stages, 1:2)
  expect_identical(spec$n_rows, nrow(df))

  psus <- spec$stage[[1]]
  expect_identical(psus$kind, "pps_wor")
  expect_true(psus$unequal)
  expect_identical(psus$systematic, NA_character_)
  expect_identical(psus$unit$kind, "cluster")
  expect_identical(psus$unit$id, match(df$psu, unique(df$psu)))
  expect_false(psus$midstage_element)
  expect_identical(psus$strata$user, "a")
  # The two largest units are certainties, one in each stratum.
  expect_identical(sort(unique(df$psu[psus$strata$certainty])), c(1L, 2L))
  key <- paste(df$a, psus$strata$certainty)
  expect_identical(psus$strata$id, match(key, unique(key)))
  expect_equal(psus$prob, 1 / df$.weight_1)
  expect_false(psus$census)

  elements <- spec$stage[[2]]
  expect_identical(elements$kind, "equal_wor")
  expect_false(elements$unequal)
  expect_identical(elements$unit$kind, "element")
  expect_identical(elements$unit$id, seq_len(nrow(df)))
  expect_false(elements$midstage_element)
  expect_null(elements$strata$certainty)
  expect_identical(elements$strata$id, match(df$h, unique(df$h)))
  # Each PSU holds three rows of each sex, two of them drawn.
  expect_equal(elements$prob, rep(2 / 3, nrow(df)))
  expect_equal(elements$pop_count, rep(3, nrow(df)))
})

test_that("draws, Poisson, a size measure and a census are told apart", {
  frame <- spec_frame()

  wr <- sampling_design() |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 6, method = "pps_multinomial", mos = x) |>
    add_stage() |>
    draw(frac = 1) |>
    execute(frame, seed = 2)
  spec <- sample_spec(wr)
  expect_identical(spec$stage[[1]]$kind, "wr")
  expect_true(spec$stage[[1]]$unequal)
  expect_identical(spec$stage[[1]]$unit$kind, "draw")
  expect_identical(spec$stage[[1]]$unit$id, as.data.frame(wr)$.draw_1)
  expect_false(spec$stage[[1]]$census)
  expect_true(spec$stage[[2]]$census)

  unweighted <- sampling_design() |>
    draw(n = 6, method = "srswr") |>
    execute(frame, seed = 2)
  expect_false(sample_spec(unweighted)$stage[[1]]$unequal)

  bernoulli <- sampling_design() |>
    draw(frac = 0.2, method = "bernoulli") |>
    execute(frame, seed = 3)
  expect_identical(sample_spec(bernoulli)$stage[[1]]$kind, "rs_poisson")
  expect_false(sample_spec(bernoulli)$stage[[1]]$unequal)

  poisson <- sampling_design() |>
    draw(n = 30, method = "pps_poisson", mos = x) |>
    execute(frame, seed = 3)
  expect_identical(sample_spec(poisson)$stage[[1]]$kind, "rs_poisson")
  expect_true(sample_spec(poisson)$stage[[1]]$unequal)
  # Its certainty units are recorded but form no stratum: selections are
  # independent, so they add no variance whatever stratum they sit in.
  expect_true(any(as.data.frame(poisson)$.certainty_1))
  expect_null(sample_spec(poisson)$stage[[1]]$strata$certainty)

  systematic <- sampling_design() |>
    draw(n = 30, method = "pps_systematic", mos = x) |>
    execute(frame, seed = 3)
  expect_identical(
    sample_spec(systematic)$stage[[1]]$systematic,
    "pps_systematic"
  )
})

test_that("the spec describes designs every export route refuses", {
  frame <- spec_frame()
  # An element stage with a stage below it cannot be executed any more. A
  # saved sample of that shape is described all the same.
  midstage <- sampling_design() |>
    add_stage() |>
    draw(n = 100) |>
    add_stage() |>
    cluster_by(psu) |>
    draw(n = 4)
  data <- frame[seq_len(40), ]
  data$.weight_1 <- 2
  data$.weight_2 <- 2.5
  spec <- expect_silent(export_stage_spec(data, midstage, 1:2))
  expect_true(spec$stage[[1]]$midstage_element)
  expect_false(spec$stage[[2]]$midstage_element)
  expect_identical(spec$stage[[1]]$unit$kind, "element")
  expect_identical(spec$stage[[2]]$unit$kind, "cluster")

  bounded <- sampling_design() |>
    draw(n = 24, method = "cube", aux = c(bound(a))) |>
    execute(frame, seed = 6)
  spec <- expect_silent(sample_spec(bounded))
  expect_identical(spec$stage[[1]]$kind, "unsupported")
  expect_true(spec$stage[[1]]$unequal)
})

test_that("one census test serves every route", {
  # Probabilities just short of one sit between the two tolerances the
  # routes used to apply, so each must now give the same answer.
  design <- sampling_design() |> draw(n = 4, method = "systematic")
  near <- data.frame(id = 1:4, .weight_1 = rep(1 + 1e-10, 4))
  exact <- data.frame(id = 1:4, .weight_1 = rep(1, 4))

  expect_false(export_stage_spec(near, design, 1L)$stage[[1]]$census)
  expect_length(systematic_approximated_stages(design, 1L, near), 1L)

  expect_true(export_stage_spec(exact, design, 1L)$stage[[1]]$census)
  expect_length(systematic_approximated_stages(design, 1L, exact), 0L)
})
