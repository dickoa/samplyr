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

  # The key column and the value column are separate refusals, and the
  # message has to name which one is absent: both inputs are a data frame
  # with two columns and one wrong name.
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

  # Cost is checked here as well as in the allocator, and both sites carry
  # this class. This is the reachable one.
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

# Two routes carry these refusals and they are not the same code. `draw()` and
# `stratify_by()` validate what the caller typed; `read_design()` restores a
# design file without re-running either, so the allocator is the only gate on
# that route. Both are asserted for every class below: each one was raised
# unclassed on the first route and classed on the second, which made it
# uncatchable through the API and testable only past `draw()`.
#
# The mutation-based assertions above stay as they are. They pin the allocator
# site itself, so they hold whether or not the read path keeps reaching it.

corrupt_design_file <- function(design, pattern, replacement, fixed = TRUE,
                                frame = NULL) {
  path <- tempfile(fileext = ".json")
  # A sample written without its frame warns that replay cannot be verified,
  # which is not what these tests are about.
  write_design(design, path, frame = frame)
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  edited <- sub(pattern, replacement, text, fixed = fixed)
  # A pattern that stopped matching would leave a valid design behind and
  # report the absence of an error, which reads as the wrong failure.
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

  # The same refusal guards the read path: a key nulled in the file restores
  # as NA and execute() refuses before any allocation arithmetic.
  design <- strata() |>
    draw(n = data.frame(region = c("A", "B", "C"), n = c(11, 12, 13)))
  expect_error(
    execute(read_design(corrupt_design_file(
      design, '"region": "B"', '"region": null'
    )), frame, seed = 1),
    class = "samplyr_error_alloc_missing_key_values"
  )

  # Coverage is only checkable against a frame, so a table that names fewer
  # strata than the frame holds is refused at execute() on the normal route.
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

  # On the read route these reached selection instead of being refused:
  # `n = true` drew one row, `frac = -0.2` drew one row, and `frac = 2` drew
  # the whole frame. A scalar size touches neither the stratum-table checks
  # nor the allocator, so nothing re-applied draw()'s rules.
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

  # The same rules on the unstratified scalar path that the stratum table
  # already had, so a restored size is refused rather than drawn from.
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

  # Neither reached selection as "Cannot determine sample size" from three
  # files down. Both reached it as a sample of 17 rows, because the allocator
  # reads `n` first and never looks at `frac`.
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
  # Every refusal on the read path was unclassed, so a caller could not tell
  # a broken file from any other error. Split by what the reader can do about
  # it: malformed is a file that is wrong, unsupported is a file this build is
  # too old to read.
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

  # Two checks at stratify_by() split what the allocator covers in one, so
  # both the type refusal and the range refusal carry the class.
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

  # Before the allocator guarded these, a consistently renamed column reached
  # the join and came back as a vctrs out-of-bounds column or, for the value
  # column, as "non-numeric argument to mathematical function" from the
  # allocation arithmetic well below it.
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

  # The same two classes the mutation-based test above pins, reached instead
  # through a file a caller can edit.
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
  # Three bound() margins over-constrain landing: each compiles to the
  # tightest integer interval around its expected count, so with enough
  # margins relative to n the cube has to relax one. A single margin is
  # satisfiable and does not reach this.
  #
  # The frame carries no RNG: group sizes are fixed by rep() and modular
  # arithmetic, so only execute()'s seed drives the result.
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

  # Every shape that fails the length-1 test: absent, empty, and two kinds
  # recorded where one belongs.
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

  # One row per village, so the phase-1 identifiers name phase-1 rows
  # uniquely and clear the bridge. A cluster-then-element phase 1 never
  # does, which is what shadowed this refusal.
  frame <- data.frame(
    village = paste0("v", 1:60),
    region = rep(c("A", "B", "C"), each = 20),
    stringsAsFactors = FALSE
  )

  # With-replacement stage 1 has an infinite correction, so the phase-1 fpc
  # states no probability for it and `method = "full"` has nothing to derive
  # the per-stage probabilities from.
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
  expect_no_error(as_svydesign(s2, method = "simple"))
  expect_no_error(as_svydesign(s2, method = "approx"))
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

  # The plan sizes PSUs and elements within them, so stage 1 has to declare
  # the cluster structure. Without `cluster_by()` the stage is not stage
  # aware and the plan cannot be read.
  expect_error(
    sampling_design() |> draw(n = plan),
    class = "samplyr_error_svyplan_domains"
  )
  expect_error(
    sampling_design() |> stratify_by(region) |> draw(n = plan),
    class = "samplyr_error_svyplan_domains"
  )

  # The message has to name the domain columns: it pluralizes on them, and
  # a marker with no quantity fails to format and loses the class with it.
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

  # `stratify_by()` accepts it: the sign is not checked until the allocator
  # has joined the values to the strata. Neyman and optimal reach separate
  # copies of the check, so a fix that touches one has to touch both.
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

  # PPS + non-safe type emits warning (no longer a hard error)
  expect_warning(
    tryCatch(
      as_svrepdesign(pps_sample, type = "bootstrap"),
      samplyr_error_svrep_conversion_failed = function(e) NULL
    ),
    "may not work for unequal-probability"
  )

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

  # Arguments after `...` are matched exactly, so near misses land in `...`.
  # Reporting them by position would describe the wrong problem.
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

  # A label that resembles no reserved argument is ignored, as before.
  frame <- data.frame(id = 1:40, region = rep(c("n", "s"), 20))
  labeled <- sampling_design() |>
    stratify_by(reg = region) |>
    draw(n = 8) |>
    execute(frame, seed = 1)
  expect_equal(as.list(get_design(labeled))$stages[[1]]$strata$vars, "region")
})

## The inventory: every class this package can raise is asserted somewhere

# Not a count, and not fifty-three more assertions duplicating the suites that
# already make them. A class added without a test would arrive as a name this
# scan does not find, which is the same shape as the method-verdict inventory
# in test-weight-contract-gates.R.
#
# Read from the namespace rather than from `R/`: under `R CMD check` the
# package is installed and its sources are not there, and a conditional skip
# would be a skip.

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

test_that("every condition class the package can raise is asserted by a test", {
  raised <- samplyr_condition_classes()
  expect_gt(length(raised), 200L)

  files <- list.files(".", pattern = "\\.R$")
  text <- unlist(lapply(files, readLines, warn = FALSE), use.names = FALSE)
  asserted <- drop_paste_fragments(unique(unlist(
    regmatches(text, gregexpr(samplyr_class_pattern, text))
  )))

  # Present in the namespace scan and named in no test. All four predate the
  # indirect-sampling and the longitudinal work: every class either feature
  # introduced is asserted by its own suite, which is what this pins.
  #
  # The digest pair are guards on a digest that only samplyr builds, and it is
  # built to cover exactly the executed stages with a chance representation
  # for every pool. Probing found no route: partial execution, continuation,
  # every `frame_digest` setting on either call, two-phase, and replicates
  # all produce a digest covering all executed stages or none at all, and a
  # sample carries its digest as an attribute rather than through any file
  # a caller can edit. They stay because they prevent a wrong number: a
  # digest missing a stage, or holding no chances, would otherwise yield
  # joint expectations that look exact.
  #
  # The two ambiguous-matches guards protect join_aux_to_strata() against a
  # stratum table with duplicate keys, which would make match() silently take
  # first matches and misallocate. Probing found no route: stratum_info rows
  # come from split_row_indices() groups, one per distinct combination, and
  # make_group_key() is a length-prefixed collision-free encoding, so its
  # keys cannot collide. They stay because nothing else defends that
  # invariant, and a wrong number is worse than a dead branch.
  #
  # Built from suffixes so the list is not itself an assertion: spelled out,
  # every name here would count as tested by the scan above.
  untested <- paste0("samplyr_error_", c(
    "digest_no_stage",
    "digest_unavailable",
    "alloc_ambiguous_matches",
    "aux_ambiguous_matches"
  ))

  expect_identical(setdiff(raised, c(asserted, untested)), character(0))

  # And the exemptions have to stay real. Writing a test for one of them
  # fails here until it is taken off the list, so the debt cannot quietly
  # stop being debt.
  expect_identical(intersect(untested, asserted), character(0))
})

## The debt: refusals that carry no class at all

# The inventory above pins every class that exists. It says nothing about
# refusals raised with no class, because an unclassed `cli_abort()` leaves no
# string for the scan to find. It is invisible in both directions, the same
# blind spot the file already records for `paste0()`-built names, and it means
# the inventory's guarantee is "every class that exists is tested" rather than
# "every refusal has a class".
#
# Those two are different, and the gap is not small. A refusal with no class
# reaches the caller as a bare `rlang_error`, so `tryCatch()` on a samplyr
# class cannot see it and neither can a test asserting one. This holds the
# count as a ceiling so the debt can only shrink. It is deliberately not zero:
# many of these are internal assertions where a class would be noise, and
# renaming in bulk would churn the message-matching tests for no caller's
# benefit. What it stops is the number growing unnoticed.
#
# `abort_samplyr()` without a class is NOT counted. It appends "samplyr_error"
# itself, so it is catchable at the family level even when it names nothing
# more specific.

samplyr_bare_refusals <- function() {
  ns <- asNamespace("samplyr")
  names <- ls(ns, all.names = TRUE)
  count <- 0L
  bare_in <- function(expr) {
    if (is.call(expr)) {
      fn <- expr[[1]]
      if (is.name(fn) &&
            as.character(fn) %in%
              c("cli_abort", "cli_warn", "cli_inform", "abort", "warn") &&
            !("class" %in% names(as.list(expr)))) {
        count <<- count + 1L
      }
    }
    if (is.call(expr) || is.pairlist(expr) || is.list(expr)) {
      for (i in seq_along(expr)) {
        if (!is.null(expr[[i]])) {
          tryCatch(bare_in(expr[[i]]), error = function(...) NULL)
        }
      }
    }
    invisible(NULL)
  }
  for (nm in names) {
    object <- get(nm, envir = ns)
    if (!is.function(object)) next
    tryCatch(bare_in(body(object)), error = function(...) NULL)
  }
  count
}

test_that("the number of refusals carrying no class does not grow", {
  # Lower this when you classify some. Never raise it: a new refusal gets a
  # class, or it gets an entry in a suite that says why it does not need one.
  expect_lte(samplyr_bare_refusals(), 195L)
})

test_that("an unknown selection method is refused with a class", {
  # Reached two ways, and both used to arrive as a bare rlang_error: `draw()`
  # validates the name at build time, and `execute()` re-resolves it for a
  # design that came back from `read_design()` without re-running `draw()`.
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
