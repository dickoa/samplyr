#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang .data .env := !! !!! enquo enquos quo quos
#' @importFrom rlang sym syms expr exprs parse_expr
#' @importFrom rlang caller_env current_env global_env
#' @importFrom rlang is_null is_character is_list is_formula
#' @importFrom rlang is_named is_empty abort warn inform
#' @importFrom rlang as_label as_name as_string
#' @importFrom glue glue glue_collapse
#' @importFrom cli cli_abort cli_warn cli_inform
#' @importFrom cli cli_h1 cli_h2 cli_bullets cli_text
#' @importFrom cli cli_alert_success cli_alert_danger cli_alert_warning
#' @importFrom tibble tibble as_tibble is_tibble
#' @importFrom dplyr mutate filter select group_by ungroup group_modify
#' @importFrom dplyr left_join inner_join anti_join across all_of
#' @importFrom dplyr n n_distinct summarise slice_sample slice
#' @importFrom dplyr bind_rows bind_cols row_number pull distinct
#' @importFrom dplyr dplyr_reconstruct
#' @importFrom vctrs vec_size vec_slice new_data_frame
#' @importFrom pillar pillar_shaft type_sum
#' @importFrom stats rlnorm rnorm sd setNames
## usethis namespace: end
NULL

# Global variables to avoid R CMD check notes
utils::globalVariables(c(
  ".sample_id", ".stratum_id", ".cluster_id", ".stage",
  ".weight", ".prob", ".mos_value", ".prev_weight", ".pik",
  ".n_h", ".n_strata", "n", "frac", "var", "cost",
  "district", "hh_count"
))
