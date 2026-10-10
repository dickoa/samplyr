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
#' @param nest Whether cluster ids are read within the stage's strata. `TRUE`
#'   (the default) makes town 1 of county A and town 1 of county B two
#'   different units. `FALSE` declares the ids unique across the strata and
#'   refuses a frame where they repeat. Without [stratify_by()] at the stage,
#'   `nest` has no effect.
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
#' ## Ids numbered within strata and parents
#'
#' Many frames number units within the level above them: towns 1, 2, 3 in
#' every county, households 1, 2, 3 in every enumeration area. A unit is
#' identified by its own id together with the strata of its stage and the
#' units that earlier stages selected.
#'
#' - **Within earlier stages.** A stage's units are always identified within
#'   the units of earlier stages, so `classroom_id = 1` can appear in every
#'   school.
#' - **Within the stage's strata.** With `nest = TRUE`, a stage's ids are
#'   read within its own strata, so `stratify_by(county) |>
#'   cluster_by(town)` selects towns as county and town pairs. When the
#'   ids already differ across strata the key stays as declared, and frames
#'   for later stages can link by it alone. When they repeat, the strata join
#'   the key, and a separate register for a later stage must then carry them
#'   too. The executed design prints the result as `town (within county)`.
#'
#' Nesting only uses the variables the design names. If town ids restart in
#' every district but the stage is stratified by region alone, towns of
#' different districts with the same id are merged into one unit. List each
#' level the ids restart under, either as strata or in `cluster_by()`, for
#' example `cluster_by(district, town)`.
#'
#' Set `nest = FALSE` when the ids are meant to be unique across strata.
#' It then works as a check. A cluster coded into two strata, such as an
#' enumeration area whose households were given different urban and rural
#' codes, is refused instead of being split into two units with part of the
#' rows each.
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
#' # Town ids restart in every county: each county's town 1 is its own unit
#' towns <- data.frame(
#'   county = rep(c("A", "B"), each = 6),
#'   town = rep(1:3, times = 4),
#'   household = 1:12
#' )
#' sampling_design() |>
#'   stratify_by(county) |>
#'   cluster_by(town) |>
#'   draw(n = 2) |>
#'   execute(towns, seed = 1)
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
cluster_by <- function(.data, ..., nest = TRUE) {
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

  check_grouping_dots(
    vars_quo, "cluster_by", "nest",
    "Clustering variables are passed as bare column names."
  )
  if (!is.logical(nest) || length(nest) != 1L || is.na(nest)) {
    abort_samplyr(
      "{.arg nest} must be TRUE or FALSE.",
      class = "samplyr_error_cluster_argument"
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
  cluster_spec <- new_cluster_spec(vars = vars, nest = nest)

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

#' Read each scheduled stage's cluster ids within its strata where needed
#'
#' Units of a stratified clustered stage are always nested in their stratum.
#' When the declared ids repeat across strata of the rows the stage can
#' reach, the strata join the cluster key and are recorded in `within`. When
#' they do not, the declared key already names the same units, so the stage
#' is left as declared and frames that lack the strata still link by it.
#'
#' The rows a stage can reach are its frame linked to the stage above, as
#' `effective_register_frames()` builds them, with the keys resolved so far.
#' Rows under a parent the stage above cannot select never change a key, and
#' `nest = FALSE` is refused on exactly the frames that would gain strata.
#' A linkage failure stops resolution and is reported by validation. Only
#' scheduled stages are resolved, so a continuation keeps the key its earlier
#' stages selected by.
#' @noRd
resolve_cluster_nesting <- function(design, entries, previous_sample = NULL) {
  parent <- previous_sample
  for (entry in entries) {
    frame <- entry$frame
    if (!is_null(parent)) {
      frame <- tryCatch(
        link_stage_frame(
          frame, parent, design, entry$stage,
          frame_index = entry$frame_index, frame_label = entry$frame_label,
          check_coverage = FALSE
        )$frame,
        error = function(e) NULL
      )
      if (is_null(frame)) {
        break
      }
    }
    design <- nest_stage_clusters(design, entry$stage, frame)
    parent <- frame
  }
  design
}

#' Add a stage's strata to its cluster key when its ids repeat across them
#' @noRd
nest_stage_clusters <- function(design, stage_idx, frame) {
  spec <- design$stages[[stage_idx]]$clusters
  if (is_null(spec) || !isTRUE(spec$nest) || !is_null(spec$within)) {
    return(design)
  }
  added <- setdiff(design$stages[[stage_idx]]$strata$vars, spec$vars)
  if (length(added) == 0L || !all(c(spec$vars, added) %in% names(frame))) {
    return(design)
  }
  unit_vars <- intersect(
    unique(c(collect_ancestor_cluster_vars(design, stage_idx), spec$vars)),
    names(frame)
  )
  pairs <- vctrs::vec_unique(vctrs::new_data_frame(
    .subset(frame, c(unit_vars, added)),
    n = nrow(frame)
  ))
  pairs <- pairs[stats::complete.cases(pairs), , drop = FALSE]
  if (anyDuplicated(pairs[, unit_vars, drop = FALSE]) > 0L) {
    design$stages[[stage_idx]]$clusters <- new_cluster_spec(
      vars = c(added, spec$vars), nest = TRUE, within = added
    )
  }
  design
}

#' The cluster variables a stage declared, before frame resolution
#' @noRd
declared_cluster_vars <- function(spec) {
  if (is_null(spec)) {
    return(character(0))
  }
  setdiff(spec$vars, spec$within)
}

#' A design as the user declared it, with frame resolution undone
#' @noRd
declared_design <- function(design) {
  for (i in seq_along(design$stages)) {
    spec <- design$stages[[i]]$clusters
    if (!is_null(spec)) {
      design$stages[[i]]$clusters <- new_cluster_spec(
        vars = declared_cluster_vars(spec), nest = spec$nest
      )
    }
  }
  design
}
