## Multi-wave interchange

#' Stack materialized waves into a long unit-wave table
#'
#' @description
#' Verifies that two or more materialized waves come from one executed master
#' and stacks them into a plain long table, one row per active master row per
#' wave. This is the structural handoff to an inference layer. Which estimator
#' of change is appropriate, and under what conditions, is a separate question
#' that depends on the design and the overlap. See
#' `vignette("rotating-panels")`.
#'
#' Row-binding waves by hand does not reproduce the checks this performs. Two
#' executions of one design produce identical `.sample_id` values, so waves of
#' *different* masters stack without complaint. A wave edited after
#' execution keeps a correct provenance record while its rows no longer match
#' it. Both are refused here.
#'
#' @details
#' ## What the columns mean
#'
#' Four columns are generated, and their names are chosen to prevent the two
#' substitutions that would quietly invalidate a downstream estimate:
#'
#' \describe{
#'   \item{`wave`}{The wave each row was observed at.}
#'   \item{`master_id`}{A **master-local** unit key, taken from `.sample_id`.
#'     It matches units across waves of one master and means nothing outside
#'     it. It is not a population identity, and it is not generally a primary
#'     sampling unit: a clustered design's own cluster variable is carried
#'     through unchanged and is what a consumer's `PSU` argument wants.}
#'   \item{`panel`}{The rotation panel the row's **assignment unit** was given.
#'     Always present: a master drawn without panels has no schedule to
#'     materialize, so it has no waves to stack. That unit is the one the
#'     master's `panel_stage` names, so several rows can share one panel: all
#'     the members of a rotating household under a retained primary unit carry
#'     its label, not one of their own.}
#'   \item{`design_weight`}{samplyr's exact design weight for that wave,
#'     including the activation factor. It is **not** a final weight: nothing
#'     here is adjusted for nonresponse or calibrated, so a consumer asking
#'     for `w_final` is asking for something the analysis must supply.}
#' }
#'
#' Every other column of each wave is carried through unchanged, including the
#' strata and cluster variables a consumer needs. samplyr's internal columns
#' are dropped, stage-specific quantities such as `.fpc_1` among them.
#'
#' ## What it does not do
#'
#' It does not compute a covariance, a correlation, or a variance of change.
#' Those belong to the inference layer, and which estimator is appropriate
#' depends on the overlap and the design. It does not reshape outcomes: attach
#' each wave's measurement under a common name before stacking. It does not
#' span cohorts of a [rotation_program()], because identity across frame
#' vintages is not established.
#'
#' @param ... Two or more materialized waves, each from
#'   `execute(master, wave = t)`, all of one master realization.
#'
#' @return A tibble with `wave`, `master_id`, `panel` and `design_weight`
#'   first, then the columns of the waves themselves. One row per active
#'   master row per wave: under with-replacement selection a population unit
#'   may occupy several master rows, so a row is a selection occurrence rather
#'   than necessarily a distinct population unit. `master_id` is unique within
#'   a wave either way. It is an ordinary tibble, not a `tbl_sample`: it holds
#'   several realizations and repeats the unit key on purpose.
#'
#' @examples
#' rotation <- data.frame(
#'   panel = rep(1:4, times = 4),
#'   wave = rep(1:4, each = 4),
#'   active = c(
#'     TRUE, TRUE, FALSE, FALSE,
#'     FALSE, TRUE, TRUE, FALSE,
#'     FALSE, FALSE, TRUE, TRUE,
#'     TRUE, FALSE, FALSE, TRUE
#'   )
#' )
#'
#' master <- sampling_design() |>
#'   draw(n = 40) |>
#'   execute(bfa_eas, seed = 2025, panels = rotation)
#'
#' stack_waves(execute(master, wave = 1), execute(master, wave = 2))
#'
#' @seealso [execute()] for materializing a wave, [as_svydesign()] for the
#'   per-wave export that supplies each wave's own variance,
#'   [joint_expectation()] with `waves` for how far two waves overlap
#'
#' @family survey export
#' @export
stack_waves <- function(...) {
  waves <- list(...)
  check_stack_waves_inputs(waves)
  records <- lapply(waves, stack_waves_provenance)
  check_stack_waves_agreement(records)

  rows <- lapply(seq_along(waves), function(i) {
    stack_waves_rows(waves[[i]], records[[i]]$wave)
  })
  vctrs::vec_rbind(!!!rows)
}

#' The provenance one wave carries, and the guards it must pass alone
#' @noRd
stack_waves_provenance <- function(wave, call = caller_env()) {
  metadata <- attr(wave, "metadata")
  check_single_replicate(wave, "stack_waves", call = call)
  check_sample_unmodified(wave, "stack_waves", call = call)
  # Shared samples cannot also be waves.

  # Refuse wave stacks spanning cohorts without cross-vintage identity.
  if (!is_null(metadata$wave$cohort)) {
    abort_samplyr(
      c(
        "{.fn stack_waves} does not span the cohorts of a rotation program.",
        "x" = "This wave is cohort {.val {metadata$wave$cohort}} of a
               program.",
        "i" = "Identity across cohorts is not established, so units of
               different cohorts cannot be matched by {.field master_id}."
      ),
      class = "samplyr_error_stack_waves_input",
      call = call
    )
  }

  # Require provenance before comparing waves with each other.
  missing <- c(
    if (!identical(metadata$prev_phase$transition, "panel_activation")) {
      "an activation link"
    },
    if (length(metadata$wave$wave) != 1L ||
          !is_integerish_numeric(metadata$wave$wave)) {
      "a single wave number"
    },
    if (is_null(metadata$wave$master_digest)) "a master fingerprint",
    if (is_null(metadata$wave$schedule_digest)) "a schedule fingerprint",
    if (is_null(metadata$panel_assignment)) "a panel assignment record"
  )
  if (length(missing) > 0) {
    abort_samplyr(
      c(
        "A wave is missing the provenance {.fn stack_waves} verifies.",
        "x" = "Missing: {missing}.",
        "i" = "Materialize it from the master with
               {.code execute(master, wave = t)} rather than assembling it."
      ),
      class = "samplyr_error_stack_waves_provenance",
      call = call
    )
  }

  master <- metadata$prev_phase$sample
  if (is_null(master)) {
    abort_samplyr(
      c(
        "{.fn stack_waves} needs the master each wave was activated from.",
        "x" = "The wave realizing wave {metadata$wave$wave} carries no link
               to it.",
        "i" = "Materialize it again from the master:
               {.code execute(master, wave = {metadata$wave$wave})}."
      ),
      class = "samplyr_error_wave_no_master",
      call = call
    )
  }
  check_wave_master_identity(metadata, master, call = call)
  prepare_panel_record(
    metadata$panel_assignment,
    "A stacked table",
    call = call
  )

  list(
    wave = metadata$wave$wave,
    master_digest = metadata$wave$master_digest,
    schedule_digest = metadata$wave$schedule_digest,
    algorithm = metadata$panel_assignment$algorithm,
    version = metadata$panel_assignment$version,
    unit = metadata$panel_assignment$unit
  )
}

#' @noRd
check_stack_waves_inputs <- function(waves, call = caller_env()) {
  if (length(waves) < 2L) {
    abort_samplyr(
      c(
        "{.fn stack_waves} stacks two or more waves.",
        "x" = "{length(waves)} {?was/were} given.",
        "i" = "A single wave is already a sample: use it directly, or export
               it with {.fn as_svydesign}."
      ),
      class = "samplyr_error_stack_waves_input",
      call = call
    )
  }

  bad <- which(!vapply(waves, function(w) {
    is_tbl_sample(w) && !is_null(attr(w, "metadata")$wave)
  }, logical(1)))
  if (length(bad) > 0) {
    abort_samplyr(
      c(
        "{.fn stack_waves} takes materialized waves.",
        # Set each cli plural quantity explicitly.
        "x" = "{cli::qty(length(bad))}Argument{?s} {bad}{cli::qty(length(bad))} {?is/are} not one.",
        "i" = "A wave comes from {.code execute(master, wave = t)}."
      ),
      class = "samplyr_error_stack_waves_input",
      call = call
    )
  }
  invisible(NULL)
}

#' Every wave must describe the same realization of the same master
#' @noRd
check_stack_waves_agreement <- function(records, call = caller_env()) {
  # `toString()` safely describes malformed non-scalar legacy fields.
  disagree <- function(field) {
    values <- unique(vapply(records, function(r) {
      value <- r[[field]]
      if (is_null(value) || length(value) == 0L) NA_character_ else
        toString(as.character(value))
    }, character(1)))
    length(values) > 1L
  }
  differing <- c(
    if (disagree("master_digest")) "the master they were activated from",
    if (disagree("schedule_digest")) "their rotation schedule",
    if (disagree("algorithm")) "their assignment algorithm",
    if (disagree("version")) "their assignment record version",
    if (disagree("unit")) "their assignment unit"
  )
  if (length(differing) > 0) {
    abort_samplyr(
      c(
        "Every wave must come from one execution of one master.",
        "x" = "These waves differ on {differing}.",
        "i" = "Two executions of one design carry the same
               {.field .sample_id} values, so stacking them would match units
               that were never the same unit."
      ),
      class = "samplyr_error_wave_master_mismatch",
      call = call
    )
  }

  labels <- vapply(records, function(r) as.integer(r$wave), integer(1))
  if (anyDuplicated(labels) > 0) {
    repeated <- unique(labels[duplicated(labels)])
    abort_samplyr(
      c(
        "Each wave may be stacked once.",
        "x" = "Wave {repeated} {?was/were} given more than once."
      ),
      class = "samplyr_error_stack_waves_input",
      call = call
    )
  }
  invisible(NULL)
}

#' @noRd
stack_waves_columns <- c("wave", "master_id", "panel", "design_weight")

#' One wave's rows, with the generated columns in front
#' @noRd
stack_waves_rows <- function(wave, label, call = caller_env()) {
  data <- as.data.frame(wave)
  carried <- setdiff(names(data), samplyr_internal_cols(data))

  # Refuse collisions with the four interchange columns.
  clash <- intersect(carried, stack_waves_columns)
  if (length(clash) > 0) {
    abort_samplyr(
      c(
        "{.fn stack_waves} generates columns that a wave already carries as
         data.",
        "x" = "Conflicting: {.field {clash}}.",
        "i" = "The generated names are {.field {stack_waves_columns}}. Rename
               the data columns before stacking; {.fn execute} cannot refuse
               them, because none is a reserved samplyr name."
      ),
      class = "samplyr_error_stack_waves_columns",
      call = call
    )
  }

  generated <- tibble::tibble(
    wave = rep(as.integer(label), nrow(data)),
    master_id = data$.sample_id,
    # Keep an explicit missing-panel guard for future cohort support.
    panel = if (is_null(data$.panel)) {
      rep(NA_integer_, nrow(data))
    } else {
      as.integer(data$.panel)
    },
    design_weight = data$.weight
  )
  vctrs::vec_cbind(generated, tibble::as_tibble(data[, carried, drop = FALSE]))
}
