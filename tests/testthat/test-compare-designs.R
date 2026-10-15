## compare_designs(): allocations and probabilities of two designs on one frame

# Three regions of four districts. Each district has two PHC and two non-PHC
# villages of six compounds, except one village of five.
compare_frame <- function() {
  villages <- expand.grid(
    v = 1:4,
    district = sprintf("d%02d", 1:12),
    stringsAsFactors = FALSE
  )
  villages$region <- c("East", "Centre", "West")[
    (match(villages$district, sprintf("d%02d", 1:12)) - 1L) %/% 4L + 1L
  ]
  villages$village <- paste0(villages$district, "-v", villages$v)
  villages$phc <- ifelse(villages$v <= 2, "PHC", "non-PHC")
  villages$size <- 6L
  villages$size[villages$village == "d07-v3"] <- 5L
  frame <- villages[rep(seq_len(nrow(villages)), villages$size), ]
  frame$compound <- seq_len(nrow(frame))
  frame$district_pop <- 100 + 7 * match(frame$district, sprintf("d%02d", 1:12))
  frame$village_pop <- 20 + 3 * frame$v + nchar(frame$district)
  rownames(frame) <- NULL
  frame[c("region", "district", "phc", "village", "compound",
          "district_pop", "village_pop")]
}

# Two districts per region, one PHC and one non-PHC village per district,
# four compounds per village.
compare_reference <- function(district_method = "pps_brewer") {
  mos <- if (district_method == "srswor") NULL else quote(district_pop)
  sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(district) |>
    draw(n = 2, method = district_method, mos = !!mos) |>
    add_stage() |>
    stratify_by(phc) |>
    cluster_by(village) |>
    draw(n = 1, method = "pps_brewer", mos = village_pop) |>
    add_stage() |>
    draw(n = 4)
}

compare_requirement <- function() {
  sampling_design() |>
    add_stage() |>
    stratify_by(region) |>
    cluster_by(district) |>
    draw(n = 2) |>
    add_stage() |>
    stratify_by(phc) |>
    cluster_by(village) |>
    draw(n = 1) |>
    add_stage() |>
    draw(n = 4)
}

test_that("a sketch requirement agrees with the design it describes", {
  result <- compare_designs(
    compare_requirement(), compare_reference(), compare_frame(),
    key = compound
  )
  expect_s3_class(result, "samplyr_design_comparison")
  expect_identical(result$verdict, "agree")
  expect_identical(result$level, "allocation")
  expect_identical(nrow(result$checks), 9L)
  expect_true(all(result$checks$status == "agree"))
  expect_identical(
    result$pools$n_target_x[result$pools$stage == 1L],
    c(2, 2, 2)
  )
})

test_that("a total read as per stratum is caught at the first stage", {
  shared <- compare_requirement()
  shared$stages[[1]]$strata$alloc <- "equal"
  shared$stages[[1]]$draw_spec$n <- 4
  result <- compare_designs(
    compare_requirement(), shared, compare_frame(),
    key = compound
  )
  expect_identical(result$verdict, "differ")
  stage1 <- result$checks[result$checks$stage == 1L, ]
  expect_identical(stage1$status, c("agree", "agree", "differ"))
  differing <- result$pools[result$pools$stage == 1L & !result$pools$agree, ]
  expect_setequal(differing$pool_key, c("East", "West"))
  expect_identical(sum(result$pools$n_target_y[result$pools$stage == 1L]), 4)
})

test_that("an intended total agrees with a requirement stated as a total", {
  shared <- function() {
    sampling_design() |>
      stratify_by(region, alloc = "equal") |>
      cluster_by(district) |>
      draw(n = 5)
  }
  pps <- sampling_design() |>
    stratify_by(region, alloc = "equal") |>
    cluster_by(district) |>
    draw(n = 5, method = "pps_brewer", mos = district_pop)
  result <- compare_designs(shared(), pps, compare_frame(), key = compound)
  expect_identical(result$verdict, "agree")
})

test_that("pools are matched by their rows, not by variable names", {
  by_pair <- compare_requirement()
  by_pair$stages[[2]]$strata$vars <- c("district", "phc")
  result <- compare_designs(
    compare_requirement(), by_pair, compare_frame(),
    key = compound
  )
  expect_identical(result$verdict, "agree")

  nested <- compare_requirement()
  nested$stages[[1]]$clusters$vars <- c("region", "district")
  result <- compare_designs(
    compare_requirement(), nested, compare_frame(),
    key = compound
  )
  expect_identical(result$verdict, "agree")
})

test_that("a different stratification names the pools it splits", {
  unstratified <- compare_requirement()
  unstratified$stages[[2]]$strata <- NULL
  unstratified$stages[[2]]$draw_spec$n <- 2
  result <- compare_designs(
    compare_requirement(), unstratified, compare_frame(),
    key = compound
  )
  expect_identical(result$verdict, "differ")
  stage2 <- result$checks[result$checks$stage == 2L, ]
  expect_identical(stage2$status, c("agree", "differ", "not_run"))
  expect_match(stage2$detail[[2]], "x splits", fixed = TRUE)
})

test_that("probabilities agree with the design's own file", {
  design <- compare_reference()
  path <- withr::local_tempfile(fileext = ".json")
  write_design(design, path)
  result <- compare_designs(
    design, read_design(path), compare_frame(),
    key = compound, level = "probabilities"
  )
  expect_identical(result$verdict, "agree")
  expect_identical(nrow(result$units), nrow(compare_frame()))
  expect_named(result$units, c(
    "compound", "probability_x", "probability_y", "abs_diff", "rel_diff",
    "agree"
  ))
  expect_identical(result$units$compound, compare_frame()$compound)
})

test_that("probabilities find a measure of size that changed", {
  frame <- compare_frame()
  changed <- frame
  changed$village_pop[changed$village == "d01-v1"] <- 999
  design <- compare_reference()
  other <- design
  other$stages[[2]]$draw_spec$mos <- "other_pop"
  frame$other_pop <- changed$village_pop
  result <- compare_designs(
    design, other, frame,
    key = compound, level = "probabilities"
  )
  expect_identical(result$verdict, "differ")
  differing <- unique(frame$district[!result$units$agree])
  expect_identical(differing, "d01")

  # Allocations ignore the measure of size.
  expect_identical(
    compare_designs(design, other, frame, key = compound)$verdict,
    "agree"
  )
})

test_that("a permuted frame gives the same agreement", {
  frame <- compare_frame()
  set.seed(3)
  permuted <- frame[sample(nrow(frame)), ]
  result <- compare_designs(
    compare_requirement(), compare_reference(), permuted,
    key = compound
  )
  expect_identical(result$verdict, "agree")
  result <- compare_designs(
    compare_reference(), compare_reference(), permuted,
    key = compound, level = "probabilities"
  )
  expect_identical(result$verdict, "agree")
})

test_that("checks that cannot run are unresolved, not agreement", {
  frame <- compare_frame()
  two <- sampling_design() |>
    add_stage() |>
    cluster_by(district) |>
    draw(n = 2) |>
    add_stage() |>
    draw(n = 4)
  result <- compare_designs(compare_requirement(), two, frame, key = compound)
  expect_identical(result$verdict, "unresolved")
  expect_identical(result$checks$check, "stages")

  with_replacement <- compare_requirement()
  with_replacement$stages[[1]]$draw_spec$method <- "srswr"
  result <- compare_designs(
    compare_requirement(), with_replacement, frame,
    key = compound
  )
  expect_identical(result$verdict, "unresolved")
  expect_match(result$checks$detail, "^y: ")

  flat <- data.frame(id = 1:40, size = rep(1:4, 10))
  exact <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = size)
  approximate <- sampling_design() |>
    draw(n = 5, method = "pps_sps", mos = size)
  result <- compare_designs(
    exact, approximate, flat,
    key = id, level = "probabilities", allow_approximate = TRUE
  )
  expect_identical(result$verdict, "unresolved")
  expect_match(result$checks$detail, "exact probabilities and y approximate")
})

test_that("a strict stage on a short frame is unresolved", {
  strict <- compare_requirement()
  strict$stages[[3]]$draw_spec$n <- 6
  strict$stages[[3]]$draw_spec$on_capped <- "error"
  result <- compare_designs(
    strict, strict, compare_frame(),
    key = compound
  )
  expect_identical(result$verdict, "unresolved")
  expect_match(result$checks$detail, "x: Stage 3", fixed = TRUE)
})

test_that("probability differences treat zeros and tolerances explicitly", {
  diff <- probability_differences(
    c(0, 0, 0.5, 0.2), c(0, 0.1, 0.5, 0.2 + 5e-7),
    atol = 1e-6, rtol = 0
  )
  expect_identical(diff$agree, c(TRUE, FALSE, TRUE, TRUE))
  expect_identical(diff$n_coverage, 1L)
  expect_identical(diff$rel_diff[1:2], c(NA_real_, NA_real_))
  expect_match(diff$detail, "1 units have probability zero", fixed = TRUE)

  outside <- probability_differences(0.2, 0.2 + 2e-6, atol = 1e-6, rtol = 0)
  expect_false(outside$agree)
  relative <- probability_differences(0.2, 0.2 + 2e-6, atol = 0, rtol = 1e-5)
  expect_true(relative$agree)
})

test_that("compare_designs() refuses invalid inputs", {
  frame <- compare_frame()
  design <- compare_requirement()
  sample <- execute(design, frame, seed = 1)
  err <- expect_error(
    compare_designs(design, sample, frame, key = compound),
    class = "samplyr_error_compare_argument"
  )
  expect_identical(condition_header(err), "compare_designs")
  expect_error(
    compare_designs(design, design, frame, key = compound, atol = -1),
    class = "samplyr_error_compare_argument"
  )
  expect_error(
    compare_designs(design, design, frame, key = compound, level = "units"),
    class = "samplyr_error_compare_argument"
  )
  expect_error(
    compare_designs(design, design, frame),
    class = "samplyr_error_exante_key"
  )
  duplicated <- frame
  duplicated$compound[2] <- duplicated$compound[1]
  expect_error(
    compare_designs(design, design, duplicated, key = compound),
    class = "samplyr_error_exante_key"
  )
  missing <- frame
  missing$compound[3] <- NA
  expect_error(
    compare_designs(design, design, missing, key = compound),
    class = "samplyr_error_exante_key"
  )
})

test_that("compare_designs() draws nothing", {
  frame <- compare_frame()
  set.seed(42)
  before <- .Random.seed
  compare_designs(compare_requirement(), compare_reference(), frame,
                  key = compound)
  compare_designs(compare_reference(), compare_reference(), frame,
                  key = compound, level = "probabilities")
  expect_identical(.Random.seed, before)

  withr::local_seed(1)
  rm(".Random.seed", envir = globalenv())
  compare_designs(compare_requirement(), compare_reference(), frame,
                  key = compound)
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
})

test_that("the comparison prints its verdict and each check", {
  result <- compare_designs(
    compare_requirement(), compare_reference(), compare_frame(),
    key = compound
  )
  text <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(text, "Allocations agree on this frame.", fixed = TRUE)
  expect_match(text, "Stage 3 targets: agree.", fixed = TRUE)
  expect_match(
    text, "Stage 1 method: srswor in x, pps_brewer in y.", fixed = TRUE
  )
  expect_match(
    text, "Stage 1 measure of size: none in x, district_pop in y.",
    fixed = TRUE
  )
  # Stage 3 has no measure of size in either design.
  expect_no_match(text, "Stage 3 measure of size", fixed = TRUE)
})

test_that("methods and measures of size are listed outside the verdict", {
  frame <- compare_frame()
  frame$district_hh <- frame$district_pop * 4
  brewer <- compare_reference()
  systematic <- compare_reference("pps_systematic")
  systematic$stages[[1]]$draw_spec$mos <- "district_hh"

  # Systematic PPS on a proportional size gives Brewer's probabilities.
  result <- compare_designs(
    brewer, systematic, frame,
    key = compound, level = "probabilities"
  )
  expect_identical(result$verdict, "agree")
  spec <- result$specification
  expect_identical(spec$stage, rep(1:3, each = 2L))
  expect_identical(spec$aspect, rep(c("method", "mos"), 3L))
  expect_identical(
    spec$status,
    c("differ", "proportional", "same", "same", "same", "same")
  )
  expect_identical(spec$x[1:2], c("pps_brewer", "district_pop"))
  expect_identical(spec$y[1:2], c("pps_systematic", "district_hh"))
  expect_identical(spec$x[6], NA_character_)
  text <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(
    text, "Stage 1 method: pps_brewer in x, pps_systematic in y.",
    fixed = TRUE
  )
  expect_match(
    text,
    "district_pop in x, district_hh in y, proportional on this frame.",
    fixed = TRUE
  )

  # The same listing comes with the allocation level.
  allocation <- compare_designs(brewer, systematic, frame, key = compound)
  expect_identical(allocation$specification, spec)

  # A size that is not a multiple of the other differs.
  frame$district_hh[frame$district == "d01"] <- 1
  spec <- compare_designs(
    brewer, systematic, frame, key = compound
  )$specification
  expect_identical(spec$status[2], "differ")

  # Designs of different depths are listed on the stages they share.
  one <- sampling_design() |> draw(n = 2)
  spec <- compare_designs(one, brewer, frame, key = compound)$specification
  expect_identical(spec$stage, c(1L, 1L))
  expect_identical(spec$status, c("differ", "differ"))
})

test_that("approximate probabilities are compared as targets", {
  flat <- data.frame(id = 1:20, size = rep(1:4, 5))
  sps <- sampling_design() |> draw(n = 4, method = "pps_sps", mos = size)
  result <- compare_designs(
    sps, sps, flat,
    key = id, level = "probabilities", allow_approximate = TRUE
  )
  expect_identical(result$verdict, "agree")
  expect_identical(result$probability_quality, "approximate")
  text <- paste(capture.output(print(result)), collapse = "\n")
  expect_match(
    text, "First-order inclusion probability targets agree on this frame.",
    fixed = TRUE
  )
  expect_match(text, "only approximately", fixed = TRUE)

  brewer <- sampling_design() |> draw(n = 4, method = "pps_brewer", mos = size)
  exact <- compare_designs(
    brewer, brewer, flat,
    key = id, level = "probabilities"
  )
  expect_identical(exact$probability_quality, "exact")
  expect_no_match(
    paste(capture.output(print(exact)), collapse = "\n"),
    "targets", fixed = TRUE
  )
  allocation <- compare_designs(brewer, brewer, flat, key = id)
  expect_identical(allocation$probability_quality, NA_character_)
})
