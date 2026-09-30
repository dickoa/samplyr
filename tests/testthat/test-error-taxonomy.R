test_that("selector helpers are rejected at design specification time", {
  expect_error(
    sampling_design() |>
      stratify_by(starts_with("zzz")),
    "bare column names"
  )

  expect_error(
    sampling_design() |>
      stratify_by(all_of(missing_selector_vec)),
    "bare column names"
  )
})

test_that("tbl_sample restoration errors use a standardized class", {
  expect_error(
    as_tbl_sample(data.frame(x = 1)),
    class = "samplyr_error_tbl_sample_missing_attributes"
  )
})

test_that("auxiliary validation errors use standardized classes", {
  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "neyman",
        variance = data.frame(region = c("A", "A"), var = c(1, 2))
      ),
    class = "samplyr_error_aux_duplicate_keys"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "neyman",
        variance = data.frame(region = c("A", NA), var = c(1, 2))
      ),
    class = "samplyr_error_aux_missing_key_values"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "neyman",
        variance = data.frame(region = c("A", "B"), var = c(1, Inf))
      ),
    class = "samplyr_error_aux_non_finite_values"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = data.frame(region = c("A", "B"), cv = c(0.2, -0.1)),
        importance = data.frame(region = c("A", "B"), importance = c(10, 20))
      ),
    class = "samplyr_error_aux_cv_bounds"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = data.frame(region = c("A", "B"), cv = c(0.2, 0.1)),
        importance = data.frame(region = c("A", "B"), importance = c(10, 0))
      ),
    class = "samplyr_error_aux_importance_bounds"
  )

  # The message names which of the key and value columns is absent.
  missing_key <- expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "neyman",
        variance = data.frame(zone = c("A", "B"), var = c(1, 2))
      ),
    class = "samplyr_error_aux_missing_columns"
  )
  expect_match(conditionMessage(missing_key), "region", fixed = TRUE)

  missing_value <- expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "neyman",
        variance = data.frame(region = c("A", "B"), sigma2 = c(1, 2))
      ),
    class = "samplyr_error_aux_missing_value_column"
  )
  expect_match(conditionMessage(missing_value), "var", fixed = TRUE)

  # Cost is checked here and in the allocator. This site is the reachable one.
  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "optimal",
        variance = data.frame(region = c("A", "B"), var = c(1, 2)),
        cost = data.frame(region = c("A", "B"), cost = c(4, -2))
      ),
    class = "samplyr_error_aux_cost_bounds"
  )
})

test_that("execution allocation/coverage errors use standardized classes", {
  frame <- data.frame(
    id = 1:60,
    region = rep(c("A", "B", "C"), each = 20),
    stringsAsFactors = FALSE
  )

  design_missing_aux <- sampling_design() |>
    stratify_by(
      region,
      alloc = "neyman",
      variance = data.frame(region = c("A", "B"), var = c(1, 2))
    ) |>
    draw(n = 15)

  expect_error(
    execute(design_missing_aux, frame, seed = 1),
    class = "samplyr_error_aux_missing_coverage"
  )

  design_bad_n <- sampling_design() |>
    stratify_by(region) |>
    draw(n = data.frame(region = c("A", "B", "C"), n = c(2, 2, 2)))

  design_bad_n$stages[[1]]$draw_spec$n <- data.frame(
    region = c("A", "A", "B", "C"),
    n = c(3, 4, 5, 6)
  )

  expect_error(
    execute(design_bad_n, frame, seed = 1),
    class = "samplyr_error_alloc_duplicate_keys"
  )

  design_bad_frac <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = data.frame(region = c("A", "B", "C"), frac = c(0.1, 0.2, 0.3)))

  design_bad_frac$stages[[1]]$draw_spec$frac <- data.frame(
    region = c("A", "B", "C"),
    frac = c(0.5, 1.1, 0.4)
  )

  expect_error(
    execute(design_bad_frac, frame, seed = 1),
    class = "samplyr_error_alloc_frac_wor_bounds"
  )
})

## Malformed n, frac, power and auxiliary inputs, on both routes that reach them

# `draw()` and `stratify_by()` validate what the caller typed, and
# `read_design()` restores a design without re-running either, so the allocator
# is the only gate on that route. Every class below is asserted on both routes.

corrupt_design_file <- function(design, pattern, replacement, fixed = TRUE,
                                frame = NULL) {
  path <- tempfile(fileext = ".json")
  # Without its frame, a written sample warns that replay cannot be verified.
  write_design(design, path, frame = frame)
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  edited <- sub(pattern, replacement, text, fixed = fixed)
  # A pattern that stopped matching would leave a valid design behind.
  expect_false(identical(edited, text))
  out <- tempfile(fileext = ".json")
  writeLines(edited, out)
  out
}

taxonomy_frame <- function() {
  data.frame(
    id = 1:60,
    region = rep(c("A", "B", "C"), each = 20),
    stringsAsFactors = FALSE
  )
}

test_that("a malformed `n` is catchable at draw() and after read_design()", {
  frame <- taxonomy_frame()
  strata <- function() sampling_design() |> stratify_by(region)
  n_df <- function(values) {
    data.frame(region = c("A", "B", "C"), n = values)
  }

  expect_error(
    strata() |> draw(n = n_df(c(Inf, 2, 2))),
    class = "samplyr_error_alloc_n_non_finite"
  )
  expect_error(
    strata() |> draw(n = c(A = Inf, B = 2, C = 2)),
    class = "samplyr_error_alloc_n_non_finite"
  )
  expect_error(
    strata() |> draw(n = Inf),
    class = "samplyr_error_alloc_n_non_finite"
  )

  expect_error(
    strata() |> draw(n = n_df(c(-1, 2, 2))),
    class = "samplyr_error_alloc_n_bounds"
  )
  expect_error(
    strata() |> draw(n = -1),
    class = "samplyr_error_alloc_n_bounds"
  )

  expect_error(
    strata() |> draw(n = n_df(c(2.5, 2, 2))),
    class = "samplyr_error_alloc_n_integer"
  )
  expect_error(
    strata() |> draw(n = 2.5),
    class = "samplyr_error_alloc_n_integer"
  )

  design <- strata() |> draw(n = n_df(c(11, 12, 13)))

  expect_error(
    execute(read_design(corrupt_design_file(design, '"n": 11', '"n": null')),
            frame, seed = 1),
    class = "samplyr_error_alloc_n_non_finite"
  )
  expect_error(
    execute(read_design(corrupt_design_file(design, '"n": 11', '"n": -3')),
            frame, seed = 1),
    class = "samplyr_error_alloc_n_bounds"
  )
  expect_error(
    execute(read_design(corrupt_design_file(design, '"n": 11', '"n": 10.5')),
            frame, seed = 1),
    class = "samplyr_error_alloc_n_integer"
  )
})

test_that("a stratum table with broken keys is catchable on both routes", {
  frame <- taxonomy_frame()
  strata <- function() sampling_design() |> stratify_by(region)

  # A missing key at draw() is refused before the design exists.
  expect_error(
    strata() |> draw(n = data.frame(region = c("A", NA), n = c(5, 5))),
    class = "samplyr_error_alloc_missing_key_values"
  )

  # A key nulled in the file restores as NA and is refused the same way.
  design <- strata() |>
    draw(n = data.frame(region = c("A", "B", "C"), n = c(11, 12, 13)))
  expect_error(
    execute(read_design(corrupt_design_file(
      design, '"region": "B"', '"region": null'
    )), frame, seed = 1),
    class = "samplyr_error_alloc_missing_key_values"
  )

  # Coverage needs a frame, so a short table is refused at execute().
  expect_error(
    strata() |>
      draw(n = data.frame(region = c("A", "B"), n = c(5, 5))) |>
      execute(frame, seed = 1),
    class = "samplyr_error_alloc_missing_coverage"
  )
})

test_that("a size of the wrong type or shape is catchable on both routes", {
  frame <- taxonomy_frame()
  strata <- function() sampling_design() |> stratify_by(region)

  expect_error(
    sampling_design() |> draw(n = "ten"),
    class = "samplyr_error_alloc_invalid_input_type"
  )
  expect_error(
    sampling_design() |> draw(frac = "half"),
    class = "samplyr_error_alloc_invalid_input_type"
  )
  expect_error(
    sampling_design() |> draw(n = c(1, 2)),
    class = "samplyr_error_alloc_invalid_input_type"
  )
  expect_error(
    sampling_design() |> draw(n = c(A = 5, B = 5)),
    class = "samplyr_error_alloc_invalid_input_type"
  )
  expect_error(
    sampling_design() |> draw(frac = c(0.1, 0.2)),
    class = "samplyr_error_alloc_invalid_input_type"
  )

  # On this route a scalar size bypasses the table checks and the allocator.
  scalar_n <- sampling_design() |> draw(n = 17)
  for (value in c('"n": "ten"', '"n": true', '"n": [1, 2]')) {
    expect_error(
      execute(read_design(corrupt_design_file(scalar_n, '"n": 17', value)),
              frame, seed = 1),
      class = "samplyr_error_alloc_invalid_input_type"
    )
  }

  scalar_frac <- sampling_design() |> draw(frac = 0.17)
  for (value in c('"frac": "half"', '"frac": true', '"frac": [0.1, 0.2]')) {
    expect_error(
      execute(read_design(corrupt_design_file(scalar_frac, '"frac": 0.17', value)),
              frame, seed = 1),
      class = "samplyr_error_alloc_invalid_input_type"
    )
  }

  # A named size with no stage to apply it to.
  expect_error(
    execute(
      read_design(corrupt_design_file(
        scalar_n, '"n": 17', '"n": {"A": 5, "B": 5}'
      )),
      frame, seed = 1
    ),
    class = "samplyr_error_alloc_invalid_input_type"
  )

  # The unstratified scalar path applies the stratum table's rules.
  for (value in c('"n": -4', '"n": 2.5')) {
    expect_error(
      execute(read_design(corrupt_design_file(scalar_n, '"n": 17', value)),
              frame, seed = 1),
      class = "samplyr_error"
    )
  }
  expect_error(
    execute(read_design(corrupt_design_file(scalar_frac, '"frac": 0.17', '"frac": -0.2')),
            frame, seed = 1),
    class = "samplyr_error_alloc_frac_bounds"
  )
  expect_error(
    execute(read_design(corrupt_design_file(scalar_frac, '"frac": 0.17', '"frac": 2')),
            frame, seed = 1),
    class = "samplyr_error_alloc_frac_wor_bounds"
  )

  # A named size that does have its stage still executes.
  stratified <- strata() |> draw(n = 17)
  expect_no_error(
    execute(
      read_design(corrupt_design_file(
        stratified, '"n": 17', '"n": {"A": 5, "B": 5, "C": 5}'
      )),
      frame, seed = 1
    )
  )
})

test_that("a stage must state exactly one of `n` and `frac`, on both routes", {
  frame <- taxonomy_frame()

  expect_error(
    sampling_design() |> draw(),
    class = "samplyr_error_alloc_size_absent"
  )
  expect_error(
    sampling_design() |> draw(n = 10, frac = 0.5),
    class = "samplyr_error_alloc_size_conflict"
  )
  expect_error(
    sampling_design() |> draw(n = 10, frac = 0.5, method = "bernoulli"),
    class = "samplyr_error_alloc_size_conflict"
  )
  expect_error(
    sampling_design() |>
      draw(frac = 0.5, method = "pps_cps", mos = region),
    class = "samplyr_error_alloc_size_conflict"
  )

  # The allocator reads `n` and ignores `frac`, so both are checked before it.
  scalar_n <- sampling_design() |> draw(n = 17)
  expect_error(
    execute(read_design(corrupt_design_file(scalar_n, '"n": 17', '"n": null')),
            frame, seed = 1),
    class = "samplyr_error_alloc_size_absent"
  )
  expect_error(
    execute(
      read_design(corrupt_design_file(
        scalar_n, '"n": 17', '"n": 17, "frac": 0.5'
      )),
      frame, seed = 1
    ),
    class = "samplyr_error_alloc_size_conflict"
  )

  stratified <- sampling_design() |> stratify_by(region) |> draw(n = 17)
  expect_error(
    execute(read_design(corrupt_design_file(stratified, '"n": 17', '"n": null')),
            frame, seed = 1),
    class = "samplyr_error_alloc_size_absent"
  )
})

test_that("a design file that cannot be read is refused by class", {
  # Malformed is a wrong file, unsupported is one this build is too old to read.
  control <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 10, method = "systematic", control = c(id))
  plain <- sampling_design() |> draw(n = 10, method = "systematic")
  table_n <- sampling_design() |>
    stratify_by(region) |>
    draw(n = data.frame(region = c("A", "B", "C"), n = c(11, 12, 13)))

  malformed <- list(
    list(control, '\\{[^{}]*"type": "ascending"[^{}]*\\}', "7", FALSE),
    list(control, '"ascending"', '"sideways"', TRUE),
    list(control, '["id"]', "[]", TRUE),
    list(plain, '"stages"', '"stagez"', TRUE),
    list(plain, '"method": \\{[^{}]*\\}', '"method": 7', FALSE),
    list(plain, '"name": "systematic"', '"name": []', TRUE),
    list(table_n, '"region": "A",', "", TRUE)
  )
  for (case in malformed) {
    expect_error(
      read_design(corrupt_design_file(case[[1]], case[[2]], case[[3]],
                                      fixed = case[[4]])),
      class = "samplyr_error_design_file_malformed"
    )
  }

  expect_error(
    read_design(corrupt_design_file(
      plain, '"format_version": 3', '"format_version": 99'
    )),
    class = "samplyr_error_design_file_unsupported"
  )
  expect_error(
    read_design(corrupt_design_file(plain, '"version": 2', '"version": 99')),
    class = "samplyr_error_design_file_unsupported"
  )
})

test_that("a malformed `frac` is catchable at draw() and after read_design()", {
  frame <- taxonomy_frame()
  strata <- function() sampling_design() |> stratify_by(region)
  frac_df <- function(values) {
    data.frame(region = c("A", "B", "C"), frac = values)
  }

  expect_error(
    strata() |> draw(frac = frac_df(c(Inf, 0.1, 0.1))),
    class = "samplyr_error_alloc_frac_non_finite"
  )
  expect_error(
    strata() |> draw(frac = Inf),
    class = "samplyr_error_alloc_frac_non_finite"
  )

  expect_error(
    strata() |> draw(frac = frac_df(c(-0.1, 0.1, 0.1))),
    class = "samplyr_error_alloc_frac_bounds"
  )
  expect_error(
    strata() |> draw(frac = -0.1),
    class = "samplyr_error_alloc_frac_bounds"
  )

  expect_error(
    strata() |> draw(frac = frac_df(c(1.1, 0.1, 0.1))),
    class = "samplyr_error_alloc_frac_wor_bounds"
  )
  expect_error(
    strata() |> draw(frac = 1.1),
    class = "samplyr_error_alloc_frac_wor_bounds"
  )

  design <- strata() |> draw(frac = frac_df(c(0.11, 0.12, 0.13)))

  expect_error(
    execute(read_design(corrupt_design_file(design, '"frac": 0.11', '"frac": null')),
            frame, seed = 1),
    class = "samplyr_error_alloc_frac_non_finite"
  )
  expect_error(
    execute(read_design(corrupt_design_file(design, '"frac": 0.11', '"frac": -0.2')),
            frame, seed = 1),
    class = "samplyr_error_alloc_frac_bounds"
  )
  expect_error(
    execute(read_design(corrupt_design_file(design, '"frac": 0.11', '"frac": 2')),
            frame, seed = 1),
    class = "samplyr_error_alloc_frac_wor_bounds"
  )
})

test_that("a malformed `power` is catchable at stratify_by() and after read_design()", {
  frame <- taxonomy_frame()
  power_design <- function(power) {
    sampling_design() |>
      stratify_by(
        region,
        alloc = "power",
        cv = c(A = 0.1, B = 0.2, C = 0.3),
        importance = c(A = 1, B = 1, C = 1),
        power = power
      )
  }

  # stratify_by() splits the check, so type and range refusals share the class.
  expect_error(power_design(2), class = "samplyr_error_alloc_power_bounds")
  expect_error(power_design(-1), class = "samplyr_error_alloc_power_bounds")
  expect_error(power_design("x"), class = "samplyr_error_alloc_power_bounds")
  expect_error(power_design(NA_real_), class = "samplyr_error_alloc_power_bounds")

  design <- power_design(0.37) |> draw(n = 30)

  expect_error(
    execute(read_design(corrupt_design_file(design, '"power": 0.37', '"power": 2')),
            frame, seed = 1),
    class = "samplyr_error_alloc_power_bounds"
  )
  expect_error(
    execute(read_design(corrupt_design_file(design, '"power": 0.37', '"power": -1')),
            frame, seed = 1),
    class = "samplyr_error_alloc_power_bounds"
  )
})

test_that("an allocation input the allocation does not read is refused", {
  aux <- c(A = 1, B = 2, C = 9)
  # Each allocation with every input it ignores, and no allocation at all.
  unused <- list(
    list(alloc = "equal", variance = aux),
    list(alloc = "proportional", variance = aux),
    list(alloc = "proportional", power = 0.3),
    list(alloc = "neyman", variance = aux, cost = aux),
    list(alloc = "neyman", variance = aux, cv = aux),
    list(alloc = "optimal", variance = aux, cost = aux, importance = aux),
    list(alloc = "power", cv = aux, importance = aux, variance = aux),
    list(variance = aux),
    list(power = 0.5)
  )
  expect_length(unused, 9L)
  for (args in unused) {
    expect_error(
      do.call(stratify_by, c(list(sampling_design(), quote(region)), args)),
      class = "samplyr_error_alloc_unused_aux",
      label = paste(names(args), collapse = " + ")
    )
  }

  # Each allocation with exactly the inputs it reads is accepted.
  used <- list(
    list(alloc = "equal"),
    list(alloc = "proportional"),
    list(alloc = "proportional", importance = aux),
    list(alloc = "neyman", variance = aux),
    list(alloc = "optimal", variance = aux, cost = aux),
    list(alloc = "power", cv = aux, importance = aux, power = 0.3)
  )
  for (args in used) {
    expect_s3_class(
      do.call(stratify_by, c(list(sampling_design(), quote(region)), args)),
      "sampling_design"
    )
  }
})

test_that("a non-positive proportional size restored from a file is refused", {
  # A design file skips stratify_by(), which refuses the value when typed.
  design <- sampling_design() |>
    stratify_by(region, alloc = "proportional",
                importance = c(A = 10, B = 30, C = 60)) |>
    draw(n = 12)
  expect_error(
    execute(
      read_design(corrupt_design_file(design, '"importance": 10', '"importance": 0')),
      taxonomy_frame(),
      seed = 1
    ),
    class = "samplyr_error_aux_importance_bounds"
  )
})

test_that("an unused allocation input restored from a file is refused", {
  frame <- taxonomy_frame()
  design <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = c(A = 1, B = 2, C = 9)) |>
    draw(n = 30)
  expect_error(
    execute(
      read_design(corrupt_design_file(
        design, '"alloc": "neyman"', '"alloc": "proportional"'
      )),
      frame,
      seed = 1
    ),
    class = "samplyr_error_alloc_unused_aux"
  )
})

test_that("an auxiliary input of the wrong type is catchable on both routes", {
  frame <- taxonomy_frame()
  neyman <- function(variance) {
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = variance)
  }

  expect_error(
    neyman(list(A = 1, B = 2, C = 3)),
    class = "samplyr_error_aux_invalid_input_type"
  )
  expect_error(
    neyman(matrix(1:6, nrow = 3)),
    class = "samplyr_error_aux_invalid_input_type"
  )
  expect_error(
    neyman(c(A = "1", B = "2", C = "3")),
    class = "samplyr_error_aux_invalid_input_type"
  )
  expect_error(
    neyman(c(1, 2, 3)),
    class = "samplyr_error_aux_invalid_input_type"
  )

  design <- neyman(c(A = 1, B = 2, C = 3)) |> draw(n = 30)

  expect_error(
    execute(
      read_design(corrupt_design_file(
        design, '"variance": \\[[^]]*\\]', '"variance": 7', fixed = FALSE
      )),
      frame, seed = 1
    ),
    class = "samplyr_error_aux_invalid_input_type"
  )
})

test_that("a design file whose tables lost a column is refused by class", {
  frame <- taxonomy_frame()

  n_design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = data.frame(region = c("A", "B", "C"), n = c(11, 12, 13)))

  expect_error(
    execute(read_design(corrupt_design_file(
      n_design, '"n": \\[[^]]*\\]', '"n": [{"zone": "A", "n": 11}]', fixed = FALSE
    )), frame, seed = 1),
    class = "samplyr_error_alloc_missing_columns"
  )
  expect_error(
    execute(read_design(corrupt_design_file(
      n_design, '"n": \\[[^]]*\\]', '"n": [{"region": "A", "count": 11}]',
      fixed = FALSE
    )), frame, seed = 1),
    class = "samplyr_error_alloc_missing_value_column"
  )

  aux_design <- sampling_design() |>
    stratify_by(region, alloc = "neyman", variance = c(A = 1, B = 2, C = 3)) |>
    draw(n = 30)

  expect_error(
    execute(read_design(corrupt_design_file(
      aux_design, '"variance": \\[[^]]*\\]',
      '"variance": [{"zone": "A", "var": 1}]', fixed = FALSE
    )), frame, seed = 1),
    class = "samplyr_error_aux_missing_columns"
  )
  expect_error(
    execute(read_design(corrupt_design_file(
      aux_design, '"variance": \\[[^]]*\\]',
      '"variance": [{"region": "A", "sigma2": 1}]', fixed = FALSE
    )), frame, seed = 1),
    class = "samplyr_error_aux_missing_value_column"
  )
})

test_that("duplicate and out-of-range stratum rows survive a design file round trip", {
  frame <- taxonomy_frame()

  # The classes the mutation-based test pins, reached through an edited file.
  n_design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = data.frame(region = c("A", "B", "C"), n = c(11, 12, 13)))

  expect_error(
    execute(read_design(corrupt_design_file(
      n_design, '"n": [', '"n": [{"region": "A", "n": 3},'
    )), frame, seed = 1),
    class = "samplyr_error_alloc_duplicate_keys"
  )

  frac_design <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = data.frame(region = c("A", "B", "C"), frac = c(0.11, 0.12, 0.13)))

  expect_error(
    execute(read_design(corrupt_design_file(
      frac_design, '"frac": 0.12', '"frac": 1.1'
    )), frame, seed = 1),
    class = "samplyr_error_alloc_frac_wor_bounds"
  )
})

## Refusals that need a fixture rather than a malformed argument

test_that("cube landing that cannot meet its bounds is refused by class", {
  # Three bound() margins over-constrain landing at n = 13, one alone would not.
  id <- 1:300
  frame <- data.frame(
    id = id,
    g1 = rep(letters[1:10], times = c(12, 18, 23, 27, 31, 34, 37, 39, 39, 40)),
    g2 = LETTERS[(id %% 7) + 1],
    g3 = paste0("z", ((id * 3) %% 5) + 1),
    stringsAsFactors = FALSE
  )

  expect_error(
    sampling_design() |>
      draw(n = 13, method = "cube", aux = c(bound(g1), bound(g2), bound(g3))) |>
      execute(frame, seed = 1),
    class = "samplyr_error_relaxed_bounds"
  )
})

test_that("a receipt with an unusable RNG record is refused by class", {
  frame <- taxonomy_frame()
  sample <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 5) |>
    execute(frame, seed = 7)

  # Absent, empty, and two kinds: every shape that fails the length-1 test.
  for (kind in c('"kind": null', '"kind": []',
                 '"kind": ["Mersenne-Twister", "Knuth-TAOCP"]')) {
    expect_error(
      replay_design(
        read_design(corrupt_design_file(
          sample, '"kind": "Mersenne-Twister"', kind, frame = frame
        )),
        frame
      ),
      class = "samplyr_error_replay_rng"
    )
  }
})

test_that("a two-phase export with no per-stage probabilities is refused", {
  skip_if_not_installed("survey")

  # One row per village, so phase-1 identifiers are unique and clear the bridge.
  frame <- data.frame(
    village = paste0("v", 1:60),
    region = rep(c("A", "B", "C"), each = 20),
    stringsAsFactors = FALSE
  )

  # A WR stage 1 leaves the fpc no probability for `method = "full"` to use.
  phase1 <- sampling_design() |>
    cluster_by(region) |>
    draw(n = 2, method = "srswr") |>
    add_stage() |>
    cluster_by(village) |>
    draw(n = 12)
  s1 <- execute(phase1, frame, seed = 1)
  s2 <- sampling_design() |> draw(n = 8) |> execute(s1, seed = 2)

  expect_error(
    as_svydesign(s2, method = "full"),
    class = "samplyr_error_twophase_stage_probs"
  )

  # The refusal names the two exports that do work, so they have to.
  expect_no_error(quiet_across(as_svydesign(s2, method = "simple")))
  expect_no_error(quiet_across(as_svydesign(s2, method = "approx")))
})

test_that("a per-domain per-stage svyplan plan is refused outside a cluster stage", {
  indicators <- data.frame(
    region = c("A", "B", "C"),
    p = c(0.3, 0.4, 0.5),
    cv = c(0.1, 0.1, 0.1),
    icc_psu = c(0.05, 0.05, 0.05),
    stringsAsFactors = FALSE
  )
  plan <- svyplan::n_cluster(
    indicators = indicators,
    domains = "region",
    stage_cost = c(100, 10),
    budget = 50000
  )

  # The plan sizes PSUs and elements, so stage 1 must declare `cluster_by()`.
  expect_error(
    sampling_design() |> draw(n = plan),
    class = "samplyr_error_svyplan_domains"
  )
  expect_error(
    sampling_design() |> stratify_by(region) |> draw(n = plan),
    class = "samplyr_error_svyplan_domains"
  )

  # The message pluralizes on the domain columns, so it must name them.
  expect_error(
    sampling_design() |> draw(n = plan),
    regexp = "region"
  )
})

test_that("a negative variance is refused by both allocators that read it", {
  frame <- data.frame(
    id = 1:60,
    region = rep(c("A", "B", "C"), each = 20),
    stringsAsFactors = FALSE
  )
  negative <- data.frame(region = c("A", "B", "C"), var = c(1, -2, 3))

  # The sign is checked after the join, in separate Neyman and optimal copies.
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "neyman", variance = negative) |>
      draw(n = 15) |>
      execute(frame, seed = 1),
    class = "samplyr_error_aux_variance_bounds"
  )

  expect_error(
    sampling_design() |>
      stratify_by(
        region,
        alloc = "optimal",
        variance = negative,
        cost = data.frame(region = c("A", "B", "C"), cost = c(4, 5, 6))
      ) |>
      draw(n = 15) |>
      execute(frame, seed = 1),
    class = "samplyr_error_aux_variance_bounds"
  )
})

test_that("non-finite allocation target errors use standardized class", {
  frame <- data.frame(
    id = 1:60,
    region = rep(c("A", "B"), each = 30),
    stringsAsFactors = FALSE
  )

  design_zero_factor <- sampling_design() |>
    stratify_by(
      region,
      alloc = "neyman",
      variance = data.frame(region = c("A", "B"), var = c(0, 0))
    ) |>
    draw(n = 20)

  expect_error(
    execute(design_zero_factor, frame, seed = 1),
    class = "samplyr_error_alloc_target_non_finite"
  )
})

test_that("alloc + frac validation uses standardized class", {
  expect_error(
    sampling_design() |>
      stratify_by(region, alloc = "proportional") |>
      draw(frac = 0.1),
    class = "samplyr_error_alloc_frac_with_alloc"
  )
})

test_that("replicate export errors use standardized classes", {
  skip_if_not_installed("survey")

  pps_sample <- sampling_design() |>
    draw(n = 20, method = "pps_brewer", mos = households) |>
    execute(bfa_eas, seed = 1)

  # A first-stage-only type on PPS is a classed WR approximation that succeeds.
  expect_warning(
    rep <- as_svrepdesign(pps_sample, type = "bootstrap"),
    class = "samplyr_warning_replicate_wr_first_stage"
  )
  expect_s3_class(rep, "svyrep.design")

  frame <- data.frame(id = 1:120, x = rnorm(120))
  design <- sampling_design() |>
    cluster_by(id) |>
    draw(n = 60)
  phase1 <- execute(design, frame, seed = 1)
  phase2 <- execute(design, phase1, seed = 2)

  expect_error(
    as_svrepdesign(phase2),
    class = "samplyr_error_svrep_twophase_unsupported"
  )
})

test_that("a misspelled reserved argument to execute() is named, not misdiagnosed", {
  frame <- data.frame(id = 1:40, region = rep(c("n", "s"), 20))
  design <- sampling_design() |> draw(n = 8)

  # Arguments after `...` match exactly, so near misses land in `...`.
  expect_error(
    execute(design, frame, seedd = 1),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(execute(design, frame, seedd = 1), "Did you mean.*seed")

  # The singular forms are the natural guess given add_stage().
  expect_error(execute(design, frame, seed = 1, stage = 1), "Did you mean.*stages")
  expect_error(execute(design, frame, seed = 1, rep = 3), "Did you mean.*reps")
  expect_error(execute(design, frame, seed = 1, panel = 2), "Did you mean.*panels")

  # A near miss holding a data frame would otherwise pass as an extra frame.
  expect_error(
    execute(design, frame, seed = 1, stagess = frame),
    class = "samplyr_error_unknown_argument"
  )

  # No close candidate: list them instead of guessing.
  expect_error(execute(design, frame, junk = 42), "must be one of")

  # An unnamed non-frame keeps its positional diagnosis.
  expect_error(
    execute(design, frame, 42),
    class = "samplyr_error_frame_not_data_frame"
  )
})

test_that("execute() still accepts labeled frames and exact arguments", {
  frame <- data.frame(id = 1:40, ea = rep(1:8, each = 5))
  hh <- data.frame(ea = rep(1:8, each = 5), hh = 1:40)
  design <- sampling_design() |> draw(n = 8)

  expect_s3_class(execute(design, listing = frame, seed = 1), "tbl_sample")
  expect_s3_class(
    execute(design, frame, seed = 1, frame_digest = "none"),
    "tbl_sample"
  )

  multistage <- sampling_design() |>
    add_stage("a") |> cluster_by(ea) |> draw(n = 3) |>
    add_stage("b") |> draw(n = 2)
  expect_s3_class(
    execute(multistage, ea_frame = frame, hh_frame = hh, seed = 1),
    "tbl_sample"
  )
})

test_that("a misspelled reserved argument to stratify_by() is named", {
  expect_error(
    sampling_design() |> stratify_by(region, allocc = "neyman"),
    class = "samplyr_error_unknown_argument"
  )
  expect_error(
    sampling_design() |> stratify_by(region, allocc = "neyman"),
    "Did you mean.*alloc"
  )
  expect_error(
    sampling_design() |> stratify_by(region, varianc = 1),
    "Did you mean.*variance"
  )

  # A label that resembles no reserved argument is ignored.
  frame <- data.frame(id = 1:40, region = rep(c("n", "s"), 20))
  labeled <- sampling_design() |>
    stratify_by(reg = region) |>
    draw(n = 8) |>
    execute(frame, seed = 1)
  expect_equal(as.list(get_design(labeled))$stages[[1]]$strata$vars, "region")
})

## The inventory: every class this package can raise is asserted somewhere

# A class added without a test arrives as a name this scan does not find. Read
# from the namespace rather than `R/`, because `R CMD check` installs the
# package without its sources.

samplyr_condition_classes <- function() {
  ns <- asNamespace("samplyr")
  objects <- mget(ls(ns, all.names = TRUE), envir = ns, ifnotfound = list(NULL))
  text <- unlist(lapply(objects, function(object) {
    if (is.function(object)) deparse(object) else if (is.character(object)) object
  }), use.names = FALSE)
  drop_paste_fragments(unique(unlist(
    regmatches(text, gregexpr(samplyr_class_pattern, text))
  )))
}

samplyr_class_pattern <- "samplyr_(error|warning|message)_[a-z0-9_]+"

# `paste0("samplyr_error_digest_", what)` leaves a prefix behind. A class name
# never ends in an underscore, so the trailing one is what tells them apart.
drop_paste_fragments <- function(x) sort(x[!grepl("_$", x)])

test_that("an exported verb called directly names itself", {
  # exante_digest() takes a call for functions that wrap it. Called by a
  # user, its refusal must still name it rather than the user's frame.
  frame <- data.frame(id = 1:10)
  cnd <- expect_error(
    exante_digest(sampling_design(), frame),
    class = "samplyr_error_exante_unsupported"
  )
  expect_identical(condition_header(cnd), "exante_digest")
  cnd <- expect_error(
    frame_summary(sampling_design() |> draw(n = 2), "not a frame"),
    class = "samplyr_error"
  )
  expect_identical(condition_header(cnd), "frame_summary")
})

test_that("every condition class the package can raise is asserted by a test", {
  raised <- samplyr_condition_classes()
  expect_gt(length(raised), 200L)

  files <- list.files(".", pattern = "\\.R$")
  text <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)
  asserted <- drop_paste_fragments(unique(unlist(
    regmatches(text, gregexpr(samplyr_class_pattern, text))
  )))

  # Unreachable guards, spelled as suffixes so this list asserts nothing.
  untested <- paste0("samplyr_error_", c(
    "digest_no_stage",
    "digest_unavailable",
    "alloc_ambiguous_matches",
    "aux_ambiguous_matches"
  ))

  expect_identical(setdiff(raised, c(asserted, untested)), character(0))

  # Testing an exempt class fails here until it leaves the list.
  expect_identical(intersect(untested, asserted), character(0))
})

## The debt: refusals that carry no class at all

# The inventory cannot see a refusal raised with no class. This holds their
# count as a ceiling so it can only shrink. `abort_samplyr()` without a class
# is not counted, since it appends "samplyr_error" itself.

samplyr_bare_refusals <- function(ns = asNamespace("samplyr")) {
  signalers <- c("cli_abort", "cli_warn", "cli_inform", "abort", "warn",
                 "inform")
  found <- character(0)
  callee <- function(fn) {
    if (is.name(fn)) {
      return(as.character(fn))
    }
    # pkg::fn and pkg:::fn
    if (is.call(fn) && as.character(fn[[1]]) %in% c("::", ":::")) {
      return(as.character(fn[[3]]))
    }
    ""
  }
  bare_in <- function(expr, where, parent = "") {
    fn <- ""
    if (is.call(expr)) {
      fn <- callee(expr[[1]])
      args <- as.list(expr)[-1]
      bare <- if (fn %in% signalers) {
        !"class" %in% names(args)
      } else if (fn %in% c("stop", "warning", "message")) {
        # stop(e) re-raises a condition object, which keeps its class.
        length(args) > 0L && !is.name(args[[1]])
      } else if (fn == "arg_match") {
        parent != "with_error_class"
      } else {
        fn == "match.arg"
      }
      if (bare) {
        found <<- c(found, paste0(where, ": ", fn, "()"))
      }
    }
    if (is.call(expr) || is.pairlist(expr) || is.list(expr)) {
      for (i in seq_along(expr)) {
        if (!is.null(expr[[i]])) {
          tryCatch(bare_in(expr[[i]], where, fn), error = function(...) NULL)
        }
      }
    }
    invisible(NULL)
  }
  for (nm in ls(ns, all.names = TRUE)) {
    object <- get(nm, envir = ns)
    if (!is.function(object)) next
    tryCatch(bare_in(body(object), nm), error = function(...) NULL)
  }
  found
}

test_that("every condition the package signals carries a class", {
  expect_identical(samplyr_bare_refusals(), character(0))
})

test_that("the class scan sees qualified, base and message calls", {
  probe <- new.env()
  probe$f <- function(e) {
    cli::cli_inform("a")
    stop("b")
    inform("c")
    cli_abort("d", class = "samplyr_error_internal")
    stop(e)
    match.arg(e)
    rlang::arg_match(e)
    with_error_class(rlang::arg_match(e), "samplyr_error_internal")
  }
  expect_identical(
    samplyr_bare_refusals(probe),
    c("f: cli_inform()", "f: stop()", "f: inform()", "f: match.arg()",
      "f: arg_match()")
  )
})

test_that("an unknown selection method is refused with a class", {
  # `draw()` checks the name at build time, `execute()` after `read_design()`.
  expect_error(
    sampling_design() |> draw(n = 5, method = "not_a_method"),
    class = "samplyr_error_unknown_method"
  )

  on.exit(try(sondage::unregister_method("vanishing"), silent = TRUE), add = TRUE)
  sondage::register_method(
    "vanishing", "wor",
    sample_fn = function(pik, n = NULL, ...) {
      utils::head(order(pik, decreasing = TRUE), n)
    },
    fixed_size = TRUE, variance_family = "pps_brewer", probabilities = "exact"
  )
  frame <- data.frame(id = seq_len(50), size = seq_len(50))
  path <- withr::local_tempfile(fileext = ".json")
  design <- sampling_design() |> draw(n = 10, method = "pps_vanishing", mos = size)
  write_design(execute(design, frame, seed = 1), path, frame = frame)

  sondage::unregister_method("vanishing")
  expect_error(
    execute(read_design(path), frame, seed = 1),
    class = "samplyr_error_unknown_method"
  )
})

## Design verbs, draw arguments and selection

test_that("the design verbs class their refusals", {
  expect_error(stratify_by(list(), region), class = "samplyr_error_design_expected")
  expect_error(cluster_by(list(), region), class = "samplyr_error_design_expected")
  expect_error(draw(list(), n = 2), class = "samplyr_error_design_expected")
  expect_error(add_stage(list()), class = "samplyr_error_design_expected")

  expect_error(
    sampling_design() |> stratify_by(),
    class = "samplyr_error_grouping_variables"
  )
  expect_error(
    sampling_design() |> cluster_by(dplyr::starts_with("i")),
    class = "samplyr_error_grouping_variables"
  )
  expect_error(
    sampling_design() |> stratify_by(region) |> stratify_by(id),
    class = "samplyr_error_stage_duplicate"
  )
  expect_error(
    sampling_design() |> cluster_by(region) |> cluster_by(id),
    class = "samplyr_error_stage_duplicate"
  )
  expect_error(
    sampling_design() |> stratify_by(region) |> add_stage(),
    class = "samplyr_error_stage_incomplete"
  )
  expect_error(
    execute(sampling_design() |> stratify_by(region), taxonomy_frame()),
    class = "samplyr_error_stage_incomplete"
  )
})

test_that("an allocation name is checked in stratify_by() and in a file", {
  err <- tryCatch(
    sampling_design() |> stratify_by(region, alloc = "prop"),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_alloc_unknown_method")
  expect_match(conditionMessage(err), "is not one of them", fixed = TRUE)
  expect_error(
    sampling_design() |> stratify_by(region, alloc = 3),
    class = "samplyr_error_alloc_unknown_method"
  )
  expect_error(
    sampling_design() |> stratify_by(region, alloc = "neyman"),
    class = "samplyr_error_aux_required"
  )

  design <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 6)
  path <- corrupt_design_file(
    design, '"alloc": "proportional"', '"alloc": "zzz"'
  )
  expect_error(
    execute(read_design(path), taxonomy_frame(), seed = 1),
    class = "samplyr_error_alloc_unknown_method"
  )
})

test_that("draw() arguments are classed by what is wrong with them", {
  base <- sampling_design()
  expect_error(base |> draw(n = 2, on_empty = "zzz"),
               class = "samplyr_error_draw_argument")
  expect_error(base |> draw(n = 2, round = 1),
               class = "samplyr_error_draw_argument")
  expect_error(
    base |> stratify_by(region, alloc = "equal") |> draw(n = 4, min_n = 1:2),
    class = "samplyr_error_draw_argument"
  )

  expect_error(base |> draw(n = 2, method = "pps_brewer"),
               class = "samplyr_error_draw_method_argument")
  expect_error(base |> draw(n = 2, method = "srswor", prn = u),
               class = "samplyr_error_draw_method_argument")
  expect_error(
    base |>
      draw(n = 2, method = "pps_multinomial", mos = x, certainty_size = 5),
    class = "samplyr_error_draw_method_argument"
  )
  expect_warning(base |> draw(n = 2, mos = x),
                 class = "samplyr_warning_draw_argument_ignored")

  # A design file reaches the same refusals at execute().
  design <- base |> draw(n = 2, on_empty = "warn")
  path <- corrupt_design_file(design, '"on_empty": "warn"', '"on_empty": "zzz"')
  expect_error(
    execute(read_design(path), taxonomy_frame(), seed = 1),
    class = "samplyr_error_draw_argument"
  )
})

test_that("selection refusals carry a class", {
  frame <- data.frame(id = 1:20, x = c(0, 1:19), z = 0)
  bernoulli <- function(on_empty) {
    sampling_design() |>
      draw(frac = 0.0001, method = "bernoulli", on_empty = on_empty)
  }
  expect_error(execute(bernoulli("error"), frame, seed = 1),
               class = "samplyr_error_empty_selection")
  expect_warning(execute(bernoulli("warn"), frame, seed = 1),
                 class = "samplyr_warning_empty_selection")

  expect_warning(
    execute(sampling_design() |> draw(n = 2, method = "pps_brewer", mos = x),
            frame, seed = 1),
    class = "samplyr_warning_mos_zero"
  )
  expect_error(
    suppressWarnings(execute(
      sampling_design() |> draw(n = 2, method = "pps_brewer", mos = z),
      frame, seed = 1
    )),
    class = "samplyr_error_mos_zero_sum"
  )
  expect_error(
    execute(
      sampling_design() |>
        draw(n = 2, method = "pps_brewer", mos = s, certainty_size = 50),
      data.frame(s = rep(100, 4)), seed = 1
    ),
    class = "samplyr_error_certainty_overflow"
  )
})

## Arguments, files, replay and diagnostics

test_that("argument refusals carry the class of the function they reach", {
  frame <- data.frame(id = 1:40)
  design <- sampling_design() |> draw(n = 5)
  sample <- execute(design, frame, seed = 1)

  expect_error(sampling_design(title = 1),
               class = "samplyr_error_design_argument")
  expect_error(sampling_design() |> add_stage(label = 1),
               class = "samplyr_error_design_argument")
  expect_error(get_design(frame), class = "samplyr_error_sample_expected")
  expect_error(frame_summary(sample, scope = "zzz"),
               class = "samplyr_error_summary_argument")
  expect_error(execute(design, frame, reps = 1),
               class = "samplyr_error_execute_argument")
  expect_error(execute(design, frame, frame_digest = "zzz"),
               class = "samplyr_error_execute_argument")
  expect_error(validate_frame(design, frame, fingerprint = "zzz"),
               class = "samplyr_error_validate_argument")
  expect_error(joint_expectation(sample, nsim = 0),
               class = "samplyr_error_joint_argument")
  expect_error(write_design(design, 1),
               class = "samplyr_error_serialize_argument")
  expect_error(read_design("https://example.org/design.json"),
               class = "samplyr_error_serialize_argument")
})

test_that("joint expectations refuse what they cannot compute", {
  cube <- sampling_design() |>
    draw(n = 4, method = "cube", aux = bound(x)) |>
    execute(data.frame(x = rep(1, 20)), seed = 1)
  expect_error(joint_expectation(cube),
               class = "samplyr_error_joint_method_unsupported")

  twins <- data.frame(m = rep(1:5, 2))
  sample <- sampling_design() |>
    draw(n = 3, method = "pps_brewer", mos = m) |>
    execute(twins, seed = 1, frame_digest = "none")
  expect_error(
    suppressWarnings(joint_expectation(sample, twins)),
    class = "samplyr_error_joint_frame_key"
  )
})

test_that("writing and replaying warn with a class", {
  frame <- data.frame(id = 1:40)
  design <- sampling_design() |> draw(frac = 0.5)
  sample <- execute(design, frame, seed = 1)
  other <- frame
  other$id[1] <- 99L

  expect_warning(write_design(sample, tempfile()),
                 class = "samplyr_warning_no_fingerprint")
  unseeded <- execute(design, frame)
  expect_warning(write_design(unseeded, tempfile(), frame = frame),
                 class = "samplyr_warning_receipt_no_seed")
  expect_warning(
    write_design(dplyr::filter(sample, id > 3), tempfile(), frame = frame),
    class = "samplyr_warning_modified_sample"
  )
  expect_warning(
    execute(sampling_design() |> draw(n = 2), dplyr::filter(sample, id > 3),
            seed = 2),
    class = "samplyr_warning_modified_sample"
  )

  path <- tempfile(fileext = ".json")
  write_design(sample, path, frame = frame)
  restored <- read_design(path)
  expect_warning(replay_design(restored, other, fingerprint = "warn"),
                 class = "samplyr_warning_replay_frame_mismatch")
  expect_message(replay_design(restored, other, fingerprint = "inform"),
                 class = "samplyr_message_replay_frame_mismatch")
  expect_warning(
    replay_design(restored, frame[1:20, , drop = FALSE],
                  fingerprint = "ignore"),
    class = "samplyr_warning_replay_rows"
  )
  expect_warning(
    check_replay_environment(list(
      language = list(version = "0.0.0"), packages = list(samplyr = "0.0.1")
    )),
    class = "samplyr_warning_replay_environment"
  )

  design_path <- tempfile(fileext = ".json")
  write_design(design, design_path, frame = frame)
  expect_warning(
    validate_frame(read_design(design_path), other, fingerprint = "warn"),
    class = "samplyr_warning_frame_fingerprint"
  )
  expect_message(validate_frame(read_design(design_path), other),
                 class = "samplyr_message_frame_fingerprint")
})

test_that("digest diagnostics carry a class", {
  frame <- data.frame(id = 1:40, psu = rep(1:10, each = 4))
  two_stage <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 4) |>
    add_stage() |> draw(n = 2)
  replicated <- execute(two_stage, frame, seed = 1, reps = 2)
  expect_message(frame_summary(replicated),
                 class = "samplyr_message_digest_partial")

  local_mocked_bindings(build_frame_digest = function(...) stop("unbuilt"))
  expect_warning(
    execute(sampling_design() |> draw(n = 2), frame, seed = 1),
    class = "samplyr_warning_digest_unavailable"
  )
})

test_that("a single-stage pps object on a multistage sample warns", {
  skip_if_not_installed("survey")
  frame <- data.frame(id = 1:40, psu = rep(1:10, each = 4))
  frame$m <- frame$psu
  sample <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 4, method = "pps_brewer", mos = m) |>
    add_stage() |> draw(n = 2) |>
    execute(frame, seed = 1)
  expect_warning(as_svydesign(sample, pps = "overton"),
                 class = "samplyr_warning_pps_single_stage")
})
