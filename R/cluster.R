#' Define Sampling Units (Clusters)
#'
#' `cluster_by()` specifies the sampling units (PSUs/clusters) for cluster
#' or multi-stage sampling designs. Unlike [stratify_by()], which defines
#' subgroups to sample *within*, `cluster_by()` defines units to sample
#' *as a whole*.
#'
#' @param .data A `sampling_design` object (piped from [sampling_design()],
#'   [stratify_by()], or [add_stage()]).
#' @param ... Clustering variable(s) specified as bare column names that
#'   identify the sampling units. In most cases this is a single variable
#'   (e.g., school_id, household_id).
#'
#' @return A modified `sampling_design` object with clustering specified.
#'
#' @details
#' `cluster_by()` is purely structural -- it defines *what* to sample, not *how*.
#' The selection method and sample size are specified in [draw()].
#'
#' ## Cluster vs. Stratification
#'
#' - **Stratification** ([stratify_by()]): Sample *within* each group; all
#'   groups represented in the sample
#' - **Clustering** (`cluster_by()`): Sample *groups as units*; only selected
#'   groups appear in sample
#'
#' ## Multi-Stage Designs
#'
#' In multi-stage designs, each stage typically has its own clustering variable:
#' - Stage 1: Select schools (`cluster_by(school_id)`)
#' - Stage 2: Select classrooms within schools (`cluster_by(classroom_id)`)
#' - Stage 3: Select students within classrooms (no clustering, sample individuals)
#'
#' The nesting structure (classrooms within schools) is validated at execution time.
#'
#' @section Order of Operations:
#' In a single stage, the typical order is:
#' 1. `stratify_by()` (optional) - define strata
#' 2. `cluster_by()` (optional) - define sampling units
#' 3. `draw()` (required) - specify selection parameters
#'
#' Both `stratify_by()` and `cluster_by()` are optional but `draw()` is required.
#'
#' @examples
#' # Simple cluster sample: select 30 schools
#' sampling_design() |>
#'   cluster_by(school_id) |>
#'   draw(n = 30) |>
#'   execute(tanzania_schools, seed = 123)
#'
#' # Stratified cluster sample: 10 schools per education level
#' sampling_design() |>
#'   stratify_by(school_level) |>
#'   cluster_by(school_id) |>
#'   draw(n = 10) |>
#'   execute(tanzania_schools, seed = 1)
#'
#' # PPS cluster sample using enrollment as measure of size
#' sampling_design() |>
#'   cluster_by(school_id) |>
#'   draw(n = 50, method = "pps_brewer", mos = enrollment) |>
#'   execute(tanzania_schools, seed = 2026)
#'
#' # Two-stage cluster sample
#' sampling_design() |>
#'   add_stage(label = "Schools") |>
#'     cluster_by(school_id) |>
#'     draw(n = 30, method = "pps_brewer", mos = enrollment) |>
#'   add_stage(label = "Students") |>
#'     draw(n = 15) |>
#'   execute(tanzania_schools, seed = 1234)
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [stratify_by()] for stratification,
#' [draw()] for specifying selection,
#' [add_stage()] for multi-stage designs
#'
#' @export
cluster_by <- function(.data, ...) {
  if (!is_sampling_design(.data)) {
    cli_abort("{.arg .data} must be a {.cls sampling_design} object")
  }

  vars_quo <- enquos(...)
  if (length(vars_quo) == 0) {
    cli_abort("At least one clustering variable must be specified")
  }

  is_bare_name <- vapply(
    vars_quo,
    function(q) is.symbol(quo_get_expr(q)),
    logical(1)
  )
  if (any(!is_bare_name)) {
    cli_abort(c(
      "{.fn cluster_by} variables must be bare column names.",
      "x" = "Tidy-select helpers and expressions are not supported.",
      "i" = "Example: {.code cluster_by(ea_id)}"
    ))
  }

  vars <- unname(vapply(vars_quo, as_label, character(1)))
  cluster_spec <- new_cluster_spec(vars = vars)

  current <- .data$current_stage
  if (current < 1 || current > length(.data$stages)) {
    cli_abort("Invalid design state: no current stage")
  }

  if (!is_null(.data$stages[[current]]$clusters)) {
    cli_abort(
      "Clustering already defined for this stage. Use {.fn add_stage} to start a new stage."
    )
  }

  .data$stages[[current]]$clusters <- cluster_spec
  .data$validated <- FALSE
  .data
}
