#' Define a New Stage in Multi-Stage Designs
#'
#' `stage()` opens a new stage context in multi-stage sampling designs.
#' It acts as a delimiter between stages, not a wrapper—each stage's
#' specification follows `stage()` using the same verbs.
#'
#' @param .data A `sampling_design` object.
#' @param label Optional character string labeling the stage (e.g., "Schools",
#'   "Classrooms", "Students"). Used for documentation and printing.
#'
#' @return A modified `sampling_design` object with a new stage context.
#'
#' @details
#' ## Multi-Stage Design Structure
#'
#' In multi-stage designs, sampling proceeds hierarchically:
#' 1. **Stage 1**: Select primary sampling units (PSUs), e.g., schools
#' 2. **Stage 2**: Within selected PSUs, select secondary units, e.g., classrooms
#' 3. **Stage 3+**: Continue nesting as needed
#'
#' Each stage can have its own:
#' - Stratification ([stratify_by()])
#' - Clustering ([cluster_by()])
#' - Selection method and sample size ([draw()])
#'
#' ## Design Patterns
#'
#' **Pattern 1: Single-stage (no explicit `stage()`):**
#' \preformatted{
#' sampling_design() |>
#'   stratify_by(...) |>
#'   draw(...)
#' }
#'
#' **Pattern 2: Multi-stage (explicit stages):**
#' \preformatted{
#' sampling_design() |>
#'   stage(label = "Stage 1") |>
#'     cluster_by(...) |>
#'     draw(...) |>
#'   stage(label = "Stage 2") |>
#'     cluster_by(...) |>
#'     draw(...) |>
#'   stage(label = "Stage 3") |>
#'     draw(...)
#' }
#'
#' ## Validation Rules
#'
#' - Each stage must end with [draw()] before the next `stage()` or [execute()]
#' - Empty stages (stage followed immediately by stage) are not allowed
#' - The final stage doesn't need `cluster_by()` (samples individuals)
#'
#' @section Execution:
#' Multi-stage designs can be executed:
#' - All at once with a single frame (hierarchical data)
#' - All at once with multiple frames (one per stage)
#' - Stage by stage using `stages =` parameter in [execute()]
#'
#' See [execute()] for details on execution patterns.
#'
#' @examples
#' \dontrun{
#' # Two-stage design: schools then students
#' sampling_design() |>
#'   stage(label = "Schools") |>
#'     cluster_by(school_id) |>
#'     draw(n = 50, method = "pps_brewer", mos = enrollment) |>
#'   stage(label = "Students") |>
#'     draw(n = 20) |>
#'   execute(frame, seed = 42)
#'
#' # Three-stage with stratification at stage 1
#' sampling_design() |>
#'   stage(label = "Districts") |>
#'     stratify_by(region) |>
#'     cluster_by(district_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = population) |>
#'   stage(label = "Schools") |>
#'     cluster_by(school_id) |>
#'     draw(n = 4) |>
#'   stage(label = "Students") |>
#'     draw(n = 15) |>
#'   execute(frame, seed = 42)
#'
#' # Gambia bednet survey pattern
#' sampling_design() |>
#'   stage(label = "Districts") |>
#'     stratify_by(region) |>
#'     cluster_by(district) |>
#'     draw(n = 5, method = "pps_brewer", mos = census_pop) |>
#'   stage(label = "Villages") |>
#'     stratify_by(phc_status) |>
#'     cluster_by(village) |>
#'     draw(n = 2, method = "pps_brewer", mos = census_pop) |>
#'   stage(label = "Compounds") |>
#'     draw(n = 6) |>
#'   execute(frame, seed = 42)
#' }
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [draw()] for completing stages,
#' [execute()] for running multi-stage designs
#'
#' @export
stage <- function(.data, label = NULL) {
  if (!is_sampling_design(.data)) {
    cli_abort("{.arg .data} must be a {.cls sampling_design} object")
  }

  if (!is_null(label)) {
    if (!is_character(label) || length(label) != 1) {
      cli_abort("{.arg label} must be a single character string")
    }
  }

  current <- .data$current_stage

  # Handle implicit stage 0 if this is the first explicit stage call
  if (current >= 1) {
    current_stage <- .data$stages[[current]]

    stage_is_empty <- is_null(current_stage$strata) &&
                      is_null(current_stage$clusters) &&
                      is_null(current_stage$draw_spec) &&
                      is_null(current_stage$label)

    if (stage_is_empty) {
      .data$stages[[current]] <- new_sampling_stage(label = label)
      return(.data)
    }

    if (is_null(current_stage$draw_spec)) {
      stage_label <- current_stage$label %||% paste("Stage", current)
      cli_abort(c(
        "Cannot start new stage: {.val {stage_label}} has no {.fn draw}",
        "i" = "Each stage must end with {.fn draw} before starting a new stage"
      ))
    }
  }

  new_stage <- new_sampling_stage(label = label)
  .data$stages <- c(.data$stages, list(new_stage))
  .data$current_stage <- length(.data$stages)
  .data$validated <- FALSE

  .data
}
