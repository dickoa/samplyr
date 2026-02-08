#' Create a Sampling Design
#'
#' `sampling_design()` is the entry point for creating survey sampling
#' specifications. It creates an empty design object that can be built
#' up using pipe-able verbs like [stratify_by()], [cluster_by()],
#' [draw()], and [add_stage()].
#'
#' @param title Optional character string providing a title for the design.
#'   Useful for documentation and printing purposes.
#'
#' @return A `sampling_design` object that can be piped to other design
#'   functions.
#'
#' @details
#' The sampling design paradigm separates the specification of a sampling
#' plan from its execution. This allows designs to be:
#'
#' - Reused across different data frames
#' - Partially executed (e.g., stage by stage)
#' - Inspected and validated before execution
#' - Documented and shared
#'
#' The design specification is frame-independent: it describes *how* to sample,
#' not *what* to sample from.
#'
#' @section Design Flow:
#' A typical design workflow follows this pattern:
#' \preformatted{
#' sampling_design() |>
#'   stratify_by(...) |>
#'   cluster_by(...) |>
#'   draw(...) |>
#'   execute(frame)
#' }
#'
#' @examples
#' # Simple random sample of 100 health facilities
#' sampling_design() |>
#'   draw(n = 100) |>
#'   execute(kenya_health, seed = 1)
#'
#' # Stratified sample with proportional allocation
#' sampling_design(title = "Kenya Health Facility Survey") |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 400) |>
#'   execute(kenya_health, seed = 2)
#'
#' # Two-stage cluster sample of schools and students
#' sampling_design(title = "Tanzania Education Survey") |>
#'   add_stage(label = "Schools") |>
#'     cluster_by(school_id) |>
#'     draw(n = 50, method = "pps_brewer", mos = enrollment) |>
#'   add_stage(label = "Students") |>
#'     draw(n = 20) |>
#'   execute(tanzania_schools, seed = 3)
#'
#' @seealso
#' [stratify_by()] for defining strata,
#' [cluster_by()] for defining clusters,
#' [draw()] for specifying selection parameters,
#' [add_stage()] for multi-stage designs,
#' [execute()] for running designs
#'
#' @export
sampling_design <- function(title = NULL) {
  if (!is_null(title) && !is_character(title)) {
    cli_abort("{.arg title} must be a character string or NULL")
  }

  if (!is_null(title) && length(title) != 1) {
    cli_abort(
      "{.arg title} must be a single string, not a vector of length {length(title)}"
    )
  }

  design <- new_sampling_design(
    title = title,
    stages = list(),
    current_stage = 0L,
    validated = FALSE
  )

  design$stages <- list(new_sampling_stage())
  design$current_stage <- 1L

  validate_sampling_design(design)
}
