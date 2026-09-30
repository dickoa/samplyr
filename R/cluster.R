#' Define sampling units (clusters)
#'
#' `cluster_by()` specifies the sampling units (PSUs/clusters) for cluster
#' or multi-stage sampling designs. Unlike [stratify_by()], which defines
#' subgroups to sample *within*, `cluster_by()` defines units to sample
#' *as a whole*.
#'
#' @param .data A `sampling_design` object (piped from [sampling_design()],
#'   [add_stage()], or [stratify_by()]), before the stage's [draw()].
#' @param ... Clustering variable(s) specified as bare column names that
#'   identify the sampling units. In most cases this is a single variable
#'   (e.g., school_id, household_id).
#'
#' @return A modified `sampling_design` object with clustering specified.
#'
#' @details
#' `cluster_by()` is purely structural. It defines *what* to sample, not *how*.
#' The selection method and sample size are specified in [draw()].
#'
#' ## Cluster vs. stratification
#'
#' - **Stratification** ([stratify_by()]): Sample *within* each group. All
#'   groups represented in the sample
#' - **Clustering** (`cluster_by()`): Sample *groups as units*. Only selected
#'   groups appear in sample
#'
#' ## Multi-stage designs
#'
#' In multi-stage designs, each stage typically has its own clustering variable:
#' - Stage 1: Select schools (`cluster_by(school_id)`)
#' - Stage 2: Select classrooms within schools (`cluster_by(classroom_id)`)
#' - Stage 3: Select students within classrooms (no clustering, sample individuals)
#'
#' Child-stage IDs do not need to be globally unique across the entire frame.
#' For example, `classroom_id = 1` can appear in more than one school. At
#' execution time, `samplyr` resolves lower-stage clusters using the full
#' ancestry from earlier stages, so IDs can be unique within parent clusters
#' rather than globally unique.
#'
#' In practice, this means the frame must represent a valid hierarchy through
#' the combination of parent-stage and current-stage IDs. If a lower-stage ID is
#' only meaningful within a parent, that is supported. If users want a stage's
#' cluster variable to be globally unique on its own, they should provide a
#' globally unique ID or include multiple columns in `cluster_by()`.
#'
#' @section Order of operations:
#' Within a stage, `cluster_by()` and [stratify_by()] are optional and come
#' before [draw()], in either order. `draw()` is required and closes the
#' stage, so `cluster_by()` after it is refused. [sampling_design()]
#' describes the stage grammar and why the order matters.
#'
#' @examples
#' # Simple cluster sample: select 30 EAs
#' sampling_design() |>
#'   cluster_by(ea_id) |>
#'   draw(n = 30) |>
#'   execute(zwe_eas, seed = 123)
#'
#' # Stratified cluster sample: 10 EAs per urban/rural
#' sampling_design() |>
#'   stratify_by(urban_rural) |>
#'   cluster_by(ea_id) |>
#'   draw(n = 10) |>
#'   execute(zwe_eas, seed = 1)
#'
#' # PPS cluster sample using households as measure of size
#' sampling_design() |>
#'   cluster_by(ea_id) |>
#'   draw(n = 50, method = "pps_brewer", mos = households) |>
#'   execute(zwe_eas, seed = 2026)
#'
#' # Two-stage cluster sample
#' zwe_frame <- zwe_eas |>
#'   dplyr::mutate(district_hh = sum(households), .by = district)
#'
#' sampling_design() |>
#'   add_stage(label = "Districts") |>
#'     cluster_by(district) |>
#'     draw(n = 20, method = "pps_brewer", mos = district_hh) |>
#'   add_stage(label = "EAs") |>
#'     draw(n = 10) |>
#'   execute(zwe_frame, seed = 1234)
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [stratify_by()] for stratification,
#' [draw()] for specifying selection,
#' [add_stage()] for multi-stage designs
#'
#' @family design specification
#' @export
cluster_by <- function(.data, ...) {
  if (is.data.frame(.data)) {
    abort_frame_misplaced("cluster_by")
  }
  if (!is_sampling_design(.data)) {
    cli_abort(
      "{.arg .data} must be a {.cls sampling_design} object",
      class = "samplyr_error_design_expected"
    )
  }
  check_stage_open(.data, "cluster_by")

  vars_quo <- enquos(...)
  if (length(vars_quo) == 0) {
    cli_abort(
      "At least one clustering variable must be specified",
      class = "samplyr_error_grouping_variables"
    )
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
    ), class = "samplyr_error_grouping_variables")
  }

  vars <- unname(vapply(vars_quo, as_label, character(1)))
  cluster_spec <- new_cluster_spec(vars = vars)

  current <- .data$current_stage
  if (current < 1 || current > length(.data$stages)) {
    cli_abort(
      "Invalid design state: no current stage",
      class = "samplyr_error_internal"
    )
  }

  if (!is_null(.data$stages[[current]]$clusters)) {
    cli_abort(
      "Clustering already defined for this stage. Use {.fn add_stage} to start a new stage.",
      class = "samplyr_error_stage_duplicate"
    )
  }

  .data$stages[[current]]$clusters <- cluster_spec
  .data$validated <- FALSE
  .data
}
