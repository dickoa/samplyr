## The weight contract at every consumer

# Every statistical consumer decides explicitly what a shared estimation
# weight means to it, and no ordinary sample notices that the decision exists.

## The inventory

# Every S3 method on tbl_sample, with what it does about a shared weight.
# "preserve" is a decision, not an omission: a data-manipulation method must
# carry the transformation record through rather than refuse it or drop it.
tbl_sample_verdicts <- c(
  "[" = "preserve",
  "names<-" = "preserve",
  "as_tbl_sample" = "preserve",
  "dplyr_col_modify" = "preserve",
  "dplyr_reconstruct" = "preserve",
  "dplyr_row_slice" = "preserve",
  "group_by" = "preserve",
  "ungroup" = "preserve",
  "vec_restore" = "preserve",
  "summary" = "support",
  "tbl_sum" = "support",
  "design_effect" = "support",
  "effective_n" = "support",
  "as_svydesign" = "support",
  "as_survey_design" = "support",
  "as_svrepdesign" = "support",
  "as_survey_rep" = "support",
  "varcomp" = "refuse"
)

test_that("every registered tbl_sample method has a recorded verdict", {
  registered <- getNamespaceInfo(asNamespace("samplyr"), "S3methods")
  methods <- registered[registered[, 2] == "tbl_sample", 1]

  # A method added without a verdict shows up as a name this vector lacks.
  expect_setequal(as.character(methods), names(tbl_sample_verdicts))
})

## Refusals: each names its own operation and is catchable on its own

test_that("the linearized survey exports take a shared-weight sample", {
  skip_if_not_installed("survey")
  # Rows are source-target contributions, so the design is the source's.
  shared <- shared_weight_sample()

  svy <- as_svydesign(shared)
  expect_s3_class(svy, "survey.design")
  expect_false(is_null(attr(svy, "samplyr_weight_share")))
  expect_gte(nrow(svy$variables), nrow(shared))

  skip_if_not_installed("srvyr")
  expect_s3_class(srvyr::as_survey_design(shared), "tbl_svy")
})

test_that("the replicate exports accept a shared-weight sample", {
  skip_if_not_installed("survey")
  shared <- shared_weight_sample()

  # survey refuses JKn for this unstratified source, so JK1 is used.
  expect_no_error(as_svrepdesign(shared, type = "JK1"))
  expect_s3_class(as_svrepdesign(shared, type = "JK1"), "svyrep.design")
})

test_that("joint_expectation and varcomp refuse with their own classes", {
  shared <- shared_weight_sample()

  expect_error(
    joint_expectation(shared, bfa_eas),
    class = "samplyr_error_joint_weight_contract"
  )
  expect_error(
    varcomp(shared, ~.weight),
    class = "samplyr_error_varcomp_weight_contract"
  )
})

test_that("further selection refuses a shared-weight sample, by either route", {
  shared <- shared_weight_sample()

  # Continuing the stored design.
  expect_error(
    shared |> execute(bfa_eas, seed = 2),
    class = "samplyr_error_execute_weight_contract"
  )
  # Using it as the frame for a further phase.
  expect_error(
    sampling_design() |> draw(n = 5) |> execute(shared, seed = 2),
    class = "samplyr_error_execute_weight_contract"
  )
})

test_that("panel and wave operations refuse a shared-weight sample", {
  shared <- shared_weight_sample()

  expect_error(
    rotation_program(list(a = shared)),
    class = "samplyr_error_panel_weight_contract"
  )
  expect_error(
    execute(shared, wave = 1),
    class = "samplyr_error_panel_weight_contract"
  )

  # The message must not point to the operation that refuses the other way.
  expect_error(
    execute(shared, wave = 1),
    regexp = "do not compose in either direction"
  )
})

test_that("share_weights() refuses a materialized wave", {
  master <- wave_share_master()
  wave <- execute(master, wave = 1)

  targets <- data.frame(
    person_id = paste0("p", seq_len(nrow(master))),
    hh_id = paste0("h", seq_len(nrow(master))),
    stringsAsFactors = FALSE
  )
  links <- data.frame(
    ea_id = master$ea_id,
    person_id = paste0("p", seq_len(nrow(master))),
    stringsAsFactors = FALSE
  )
  share <- function(x) {
    share_weights(
      x,
      targets = targets, links = links,
      by = c(ea_id = "ea_id"), to = c(person_id = "person_id"),
      within = hh_id, multiplicity = complete_links()
    )
  }

  expect_error(share(wave), class = "samplyr_error_share_weights_wave")
  expect_error(share(wave), regexp = "realizes wave 1")

  # The master is accepted, and its panel assignment is not carried over.
  from_master <- share(master)
  expect_s3_class(from_master, "tbl_sample")
  expect_identical(nrow(from_master), nrow(master))
  expect_null(attr(from_master, "metadata")$panel_assignment)
  expect_null(attr(from_master, "metadata")$wave)
  expect_false(".panel" %in% names(from_master))

  # The result is an ordinary shared sample, which the wave route refuses.
  expect_error(
    execute(from_master, wave = 1),
    class = "samplyr_error_panel_weight_contract"
  )
})

test_that("stack_waves reports non-wave arguments instead of failing to format", {
  ordinary <- shared_weight_source()

  # Two positions give the plural message, not a cli formatting error.
  expect_error(
    stack_waves(ordinary, ordinary),
    class = "samplyr_error_stack_waves_input"
  )
  expect_error(stack_waves(ordinary, ordinary), regexp = "Arguments 1 and 2")
  # The second argument is a real wave, so the first is reported alone.
  expect_error(
    stack_waves(ordinary, execute(wave_share_master(), wave = 1)),
    regexp = "Argument 1 is not one"
  )
})

test_that("every refusal carries the family class as well as its own", {
  shared <- shared_weight_sample()
  calls <- list(
    function() joint_expectation(shared, bfa_eas),
    function() varcomp(shared, ~.weight),
    function() shared |> execute(bfa_eas, seed = 2),
    function() rotation_program(list(a = shared))
  )

  for (i in seq_along(calls)) {
    cond <- tryCatch(calls[[i]](), condition = function(e) e)
    expect_s3_class(cond, "samplyr_error_weight_contract")
    expect_s3_class(cond, "samplyr_error")
  }
})

test_that("a refusal is not reported as tampering", {
  shared <- shared_weight_sample()

  # The transformation minted its own integrity record.
  expect_silent(check_sample_unmodified(shared, "test"))
  cond <- tryCatch(varcomp(shared, ~.weight), condition = function(e) e)
  expect_false(inherits(cond, "samplyr_error_modified_sample"))
})

test_that("the srvyr bridges take a shared-weight sample", {
  skip_if_not_installed("srvyr")
  shared <- shared_weight_sample()

  expect_s3_class(srvyr::as_survey_design(shared), "tbl_svy")
  expect_s3_class(
    srvyr::as_survey_rep(shared, type = "subbootstrap"), "tbl_svy"
  )
})

## Supported operations

test_that("the weighting-loss diagnostics accept shared weights", {
  shared <- shared_weight_sample()

  # Computed from .weight alone, and the links here are one-to-one.
  expect_identical(design_effect(shared), design_effect(shared_weight_source()))
  expect_identical(effective_n(shared), effective_n(shared_weight_source()))
})

test_that("print and summary work on a shared-weight sample", {
  shared <- shared_weight_sample()

  expect_no_error(capture.output(print(shared)))
  expect_no_error(capture.output(summary(shared)))
})

## Preservation: no route launders an estimation weight back to a design weight

test_that("dplyr verbs carry the transformation record through", {
  shared <- shared_weight_sample()

  expect_identical(
    sample_weight_contract(dplyr::arrange(shared, person_id)), "shared"
  )
  expect_identical(
    sample_weight_contract(dplyr::mutate(shared, extra = 1)), "shared"
  )
  expect_identical(
    sample_weight_contract(dplyr::filter(shared, .weight > 0)), "shared"
  )
  expect_identical(
    sample_weight_contract(dplyr::group_by(shared, hh_id)), "shared"
  )
  expect_identical(
    sample_weight_contract(dplyr::ungroup(dplyr::group_by(shared, hh_id))),
    "shared"
  )
  expect_identical(sample_weight_contract(shared[1:3, ]), "shared")
})

test_that("stripping and restoring the class does not launder the contract", {
  shared <- shared_weight_sample()

  stripped <- shared
  class(stripped) <- setdiff(class(stripped), "tbl_sample")
  restored <- as_tbl_sample(stripped)

  expect_identical(sample_weight_contract(restored), "shared")
  # It exports as a transformation, not as an ordinary design-weight sample.
  skip_if_not_installed("survey")
  expect_false(
    is_null(attr(as_svydesign(restored), "samplyr_weight_share"))
  )
})

test_that("vctrs restoration carries the record or drops the class entirely", {
  shared <- shared_weight_sample()

  doubled <- vctrs::vec_rbind(shared, shared)
  # Either still a shared sample or no sample at all.
  if (is_tbl_sample(doubled)) {
    expect_identical(sample_weight_contract(doubled), "shared")
  } else {
    expect_null(attr(doubled, "metadata"))
  }
})

test_that("dropping .weight demotes to a plain tibble rather than a design sample", {
  shared <- shared_weight_sample()
  demoted <- dplyr::select(shared, person_id, hh_id)

  expect_false(is_tbl_sample(demoted))
  expect_null(attr(demoted, "metadata"))
})

## Ordinary samples notice nothing

test_that("an ordinary sample passes every gate the shared one is refused by", {
  skip_if_not_installed("survey")
  ordinary <- shared_weight_source()

  expect_identical(sample_weight_contract(ordinary), "design")
  expect_no_error(as_svydesign(ordinary))
  # survey warns about the tiny sampling fraction.
  expect_no_error(suppressWarnings(as_svrepdesign(ordinary, type = "bootstrap")))
  expect_no_error(joint_expectation(ordinary, bfa_eas))
  expect_no_error(design_effect(ordinary))
  expect_no_error(effective_n(ordinary))
})

test_that("an ordinary clustered varcomp is unaffected", {
  frame <- data.frame(
    psu = rep(1:8, each = 5),
    y = as.numeric(seq_len(40))
  )
  clustered <- sampling_design() |>
    cluster_by(psu) |>
    draw(n = 4) |>
    execute(frame, seed = 1)

  expect_identical(sample_weight_contract(clustered), "design")
  expect_no_error(varcomp(clustered, ~y))
})

test_that("an ordinary two-phase execution is unaffected", {
  phase1 <- shared_weight_source()

  expect_no_error(
    sampling_design() |> draw(n = 5) |> execute(phase1, seed = 2)
  )
})

test_that("an ordinary stage continuation is unaffected", {
  frame <- data.frame(psu = rep(1:4, each = 3), ssu = 1:12)
  design <- sampling_design() |>
    add_stage("PSU") |>
    cluster_by(psu) |>
    draw(n = 2) |>
    add_stage("SSU") |>
    draw(n = 1)

  stage1 <- execute(design, frame, stages = 1, seed = 1)

  expect_identical(sample_weight_contract(stage1), "design")
  expect_no_error(execute(stage1, frame, seed = 2))
})

test_that("the gates add no condition to an ordinary sample's failures", {
  skip_if_not_installed("survey")
  ordinary <- shared_weight_source()
  modified <- ordinary[1:3, ]

  # The tampering gate owns this finding.
  cond <- tryCatch(as_svydesign(modified), condition = function(e) e)
  expect_s3_class(cond, "samplyr_error_modified_sample")
  expect_false(inherits(cond, "samplyr_error_weight_contract"))
})
