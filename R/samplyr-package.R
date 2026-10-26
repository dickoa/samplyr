#' @description
#' samplyr states a survey sampling design as a pipeline of verbs and runs
#' it on a frame with [execute()]. A design is built with
#' [sampling_design()], [add_stage()], [stratify_by()], [cluster_by()] and
#' [draw()], independently of any frame. The same design can then be
#' previewed against a frame with [frame_summary()], executed on several
#' frames, and saved with [write_design()]. The sample [execute()] returns
#' carries its weights and its design, and [as_svydesign()] and
#' [as_svrepdesign()] export it for analysis with the survey package.
#'
#' @section Where to start:
#' `vignette("introduction", package = "samplyr")` builds designs from a
#' single draw to a three-stage household sample.
#' `vignette("survey-analysis", package = "samplyr")` exports a sample and
#' estimates from it.
#'
#' The reference topics answer narrower questions. [selection_methods]
#' compares the selection methods, [frame_input_grammar] says how frames
#' and stage registers are passed, [sample_columns] lists the columns a
#' sample carries and how its weights are computed, and
#' [variance_estimation] gives how each method's variance is estimated.
#'
#' @examples
#' # 300 EAs allocated proportionally across the 13 regions
#' design <- sampling_design(title = "EA survey") |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300)
#' design
#'
#' sample <- execute(design, bfa_eas, seed = 1)
#' sample
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data !! !!! enquo enquos %||%
#' @importFrom rlang quo_get_expr quo_get_env is_call new_quosure
#' @importFrom rlang caller_env current_env
#' @importFrom rlang is_null is_character
#' @importFrom rlang as_label
#' @importFrom cli cli_abort cli_warn cli_inform qty
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select group_by arrange
#' @importFrom dplyr left_join inner_join semi_join across all_of
#' @importFrom dplyr n summarise row_number
#' @importFrom dplyr bind_rows distinct
#' @importFrom dplyr dplyr_reconstruct dplyr_row_slice dplyr_col_modify
#' @importFrom dplyr ungroup
#' @importFrom vctrs vec_group_loc vec_as_location vec_restore
#' @importFrom withr with_seed
#' @importFrom pillar tbl_sum
## usethis namespace: end
NULL
