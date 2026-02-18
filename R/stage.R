#' Define a New Stage in Multi-Stage Designs
#'
#' `add_stage()` opens a new stage context in multi-stage sampling designs.
#' It acts as a delimiter between stages, not a wrapper -- each stage's
#' specification follows `add_stage()` using the same verbs.
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
#' **Pattern 1: Single-stage (no explicit `add_stage()`):**
#' \preformatted{
#' sampling_design() |>
#'   stratify_by(...) |>
#'   draw(...)
#' }
#'
#' **Pattern 2: Multi-stage (explicit stages):**
#' \preformatted{
#' sampling_design() |>
#'   add_stage(label = "Stage 1") |>
#'     cluster_by(...) |>
#'     draw(...) |>
#'   add_stage(label = "Stage 2") |>
#'     cluster_by(...) |>
#'     draw(...) |>
#'   add_stage(label = "Stage 3") |>
#'     draw(...)
#' }
#'
#' ## Validation Rules
#'
#' - Each stage must end with [draw()] before the next `add_stage()` or [execute()]
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
#' # Two-stage design: districts then EAs
#' zwe_frame <- zwe_eas |>
#'   dplyr::mutate(district_hh = sum(households), .by = district)
#'
#' sampling_design() |>
#'   add_stage(label = "Districts") |>
#'     cluster_by(district) |>
#'     draw(n = 20, method = "pps_brewer", mos = district_hh) |>
#'   add_stage(label = "EAs") |>
#'     draw(n = 10) |>
#'   execute(zwe_frame, seed = 123)
#'
#' # Two-stage with stratification at stage 1
#' sampling_design() |>
#'   add_stage(label = "Districts") |>
#'     stratify_by(province) |>
#'     cluster_by(district) |>
#'     draw(n = 2, method = "pps_brewer", mos = district_hh) |>
#'   add_stage(label = "EAs") |>
#'     draw(n = 5) |>
#'   execute(zwe_frame, seed = 1234)
#'
#' # DHS-style two-stage stratified cluster sample
#' sampling_design(title = "DHS-style Household Survey") |>
#'   add_stage(label = "Enumeration Areas") |>
#'     stratify_by(region, urban_rural) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 3, method = "pps_brewer", mos = households) |>
#'   add_stage(label = "Households") |>
#'     draw(n = 20) |>
#'   execute(bfa_eas, seed = 2026)
#'
#' # Partial execution: select only stage 1
#' design <- sampling_design() |>
#'   add_stage(label = "EAs") |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 10, method = "pps_brewer", mos = households) |>
#'   add_stage(label = "Households") |>
#'     draw(n = 12)
#'
#' # Execute stage 1 only
#' selected_eas <- execute(design, bfa_eas, stages = 1, seed = 1)
#' nrow(selected_eas)
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [draw()] for completing stages,
#' [execute()] for running multi-stage designs
#'
#' @export
add_stage <- function(.data, label = NULL) {
  if (!is_sampling_design(.data)) {
    cli_abort("{.arg .data} must be a {.cls sampling_design} object")
  }

  if (!is_null(label)) {
    if (!is_character(label) || length(label) != 1) {
      cli_abort("{.arg label} must be a single character string")
    }
  }

  current <- .data$current_stage

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
