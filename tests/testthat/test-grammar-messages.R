## Messages that teach the grammar

## The design print says what a later stage's size is per

test_that("a later stage's n and frac are printed per parent unit", {
  draw_lines <- function(design) {
    lines <- cli::ansi_strip(utils::capture.output(print(design)))
    # The bullet glyph depends on the terminal, the text does not.
    sub("^.*Draw: ", "Draw: ", grep("Draw:", lines, value = TRUE))
  }
  design <- sampling_design() |>
    add_stage() |> cluster_by(ea_id) |> draw(n = 10) |>
    add_stage() |> stratify_by(urban_rural) |> draw(n = 3) |>
    add_stage() |> stratify_by(region, alloc = "proportional") |> draw(n = 6)
  expect_identical(draw_lines(design), c(
    "Draw: n = 10, method = srswor",
    "Draw: n = 3 (per stratum, per ea_id), method = srswor",
    "Draw: n = 6 (total, per ea_id), method = srswor"
  ))

  two_keys <- sampling_design() |>
    add_stage() |> cluster_by(region, province) |> draw(n = 2) |>
    add_stage() |> draw(frac = 0.1)
  expect_identical(
    draw_lines(two_keys)[2],
    "Draw: frac = 0.1 (per region/province), method = srswor"
  )
})

## A take per parent unit

test_that("a size table keyed on the parent's units teaches stratify_by()", {
  take <- data.frame(ea_id = c(1, 2, 3), n = c(2, 3, 4))
  stage_two <- sampling_design() |>
    add_stage() |> cluster_by(ea_id) |> draw(n = 10) |>
    add_stage()
  err <- tryCatch(stage_two |> draw(n = take), error = identity)
  expect_s3_class(err, "samplyr_error_draw_parent_keyed_take")
  expect_match(conditionMessage(err), "stratify_by(ea_id)", fixed = TRUE)

  # The pattern it names is accepted.
  expect_s3_class(
    stage_two |> stratify_by(ea_id) |> draw(n = take),
    "sampling_design"
  )

  # A table keyed on anything else keeps the general refusal.
  expect_error(
    stage_two |> draw(n = data.frame(region = "x", n = 1)),
    class = "samplyr_error_alloc_invalid_input_type"
  )
})

## A plan on several stratification variables

test_that("a svyplan allocation at a stage with two strata says what to do", {
  plan <- svyplan::n_alloc(
    data.frame(stratum = c("a f", "b m"), N = c(100, 200), sd = 1),
    n = 20
  )
  err <- tryCatch(
    sampling_design() |> stratify_by(stratum, sex) |> draw(n = plan),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_svyplan_multivariable_strata")
  expect_match(conditionMessage(err), "one column", fixed = TRUE)
})

## The frame goes to execute()

test_that("a frame where the design goes is named as such", {
  frame <- data.frame(id = 1:10)
  for (attempt in list(
    function() sampling_design(frame),
    function() frame |> draw(n = 2),
    function() frame |> stratify_by(id),
    function() frame |> cluster_by(id),
    function() frame |> add_stage()
  )) {
    expect_error(attempt(), class = "samplyr_error_frame_misplaced")
  }
  err <- tryCatch(
    execute(frame, sampling_design() |> draw(n = 2)),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_frame_misplaced")
  expect_match(conditionMessage(err), "execute(design, frame)", fixed = TRUE)
})

## Method names

test_that("a misspelled method name is answered with the likely one", {
  suggested <- function(method) {
    err <- tryCatch(
      sampling_design() |> draw(n = 2, method = method, mos = x),
      error = identity
    )
    expect_s3_class(err, "samplyr_error_unknown_method")
    conditionMessage(err)
  }
  expect_match(suggested("brewer"), "Did you mean \"pps_brewer\"", fixed = TRUE)
  expect_match(suggested("sampford"), "Did you mean \"pps_sampford\"", fixed = TRUE)
  # One edit from two methods: both are named, neither is guessed.
  tie <- suggested("srswo")
  expect_match(tie, "\"srswor\"", fixed = TRUE)
  expect_match(tie, "\"srswr\"", fixed = TRUE)
  expect_identical(
    suggest_method_names("srswo", valid_builtin_methods),
    c("srswor", "srswr")
  )
  expect_false(grepl("Did you mean", suggested("zzzzzz"), fixed = TRUE))
})

## A column name held in a variable

test_that("a column name in a variable is read through .data[[v]]", {
  column <- "households"
  from_pronoun <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = .data[[column]])
  expect_identical(from_pronoun$stages[[1]]$draw_spec$mos, "households")
  cube <- sampling_design() |>
    draw(n = 5, method = "cube", aux = c(.data[[column]], bound(.data[[column]])))
  expect_identical(cube$stages[[1]]$draw_spec$aux, "households")
  expect_identical(cube$stages[[1]]$draw_spec$bounds, "households")

  for (attempt in list(
    function() sampling_design() |>
      draw(n = 5, method = "pps_brewer", mos = "households"),
    function() sampling_design() |>
      draw(n = 5, method = "pps_brewer", mos = !!column),
    function() sampling_design() |>
      draw(n = 5, method = "cube", aux = "households")
  )) {
    err <- tryCatch(attempt(), error = identity)
    expect_s3_class(err, "samplyr_error_draw_string_column")
    expect_match(conditionMessage(err), ".data[[v]]", fixed = TRUE)
  }

  # A bare name that is not a column is reported at execution, with the idiom.
  err <- tryCatch(
    sampling_design() |>
      draw(n = 5, method = "pps_brewer", mos = column) |>
      execute(bfa_eas, seed = 1),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_frame_missing_vars")
  expect_match(conditionMessage(err), ".data[[v]]", fixed = TRUE)
})

## Listing rows under units the sample did not select

test_that("validate_frame() counts the listing rows execute() sets aside", {
  frame <- data.frame(psu = rep(1:20, each = 6), id = 1:120)
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 5) |>
    add_stage() |> draw(n = 2)
  first <- execute(design, frame, stages = 1, seed = 1)
  msg <- tryCatch(validate_frame(first, frame), message = identity)
  expect_s3_class(msg, "samplyr_message_frame_unselected_rows")
  expect_match(conditionMessage(msg), "90 of 120 rows", fixed = TRUE)
  expect_no_message(
    validate_frame(first, frame[frame$psu %in% first$psu, ])
  )
  # execute() stays quiet about it.
  expect_no_message(execute(first, frame, seed = 2))
})

## Several panel counts

test_that("a vector of panel sizes is read as what it is", {
  err <- tryCatch(
    execute(sampling_design() |> draw(n = 20), data.frame(id = 1:100),
            seed = 1, panels = c(main = 20, reserve = 5)),
    error = identity
  )
  expect_s3_class(err, "samplyr_error_panel_count")
  expect_match(conditionMessage(err), "one count", fixed = TRUE)
  expect_false(grepl("no partition", conditionMessage(err), fixed = TRUE))
})
