## The per-stage description every export route reads

#' What an executed sample is, stage by stage
#'
#' One entry per executed stage, in execution order. It describes and never
#' refuses: each export route decides what it can carry and refuses the rest,
#' in its own order, and a preflight can read the same description for a
#' design some route would refuse. It writes no columns, so reading it
#' changes nothing a route exports.
#'
#' Per stage:
#' - `kind`: the variance family, from `survey_stage_kind()`.
#' - `unequal`: a variance family the equal-probability estimators do not
#'   cover. That is a PPS without-replacement, balanced, spatial or bounded
#'   stage, whatever its probabilities, or a with-replacement or Poisson
#'   stage with a measure of size.
#' - `unit`: what the stage selects. `"draw"` for a with-replacement stage
#'   with draw identifiers (its id is the `.draw_k` column), `"cluster"`
#'   (ids in order of first appearance), or `"element"` (the row).
#'   `midstage_element` flags an element stage with stages below it.
#' - `strata`: the user's stratification variables (with the `.zone_k`
#'   column when the stage drew a certainty plan's zones, each zone being a
#'   stratum of the design), the take-all flag when the stage's certainty
#'   units form a stratum of their own, and the partition of rows they give
#'   together, numbered in order of first appearance. A stage drawing one
#'   PSU per zone is stratified by its `.pair_k` variance groups alone,
#'   since a group may cross the user's strata.
#' - `prob`: the conditional inclusion probability, or for a
#'   with-replacement stage the reciprocal of the stage weight. `census` is
#'   TRUE when every row was taken with certainty.
#' - `pop_count`: the recorded stratum population count, when there is one.
#'
#' @param df The sample's rows as a data frame.
#' @param design,stages The design and the stages it executed.
#' @param phase 1 or 2 for a phase of a two-phase sample, NULL otherwise.
#' @noRd
export_stage_spec <- function(df, design, stages, phase = NULL) {
  n_stages <- length(stages)
  entries <- lapply(seq_len(n_stages), function(pos) {
    stage_idx <- stages[pos]
    stage_spec <- design$stages[[stage_idx]]
    draw_spec <- stage_spec$draw_spec
    kind <- survey_stage_kind(draw_spec)
    list(
      stage = stage_idx,
      name = stage_token(design, stage_idx),
      method = draw_spec$method,
      kind = kind,
      unequal = stage_is_unequal(draw_spec, kind),
      systematic = if (draw_spec$method %in% c("systematic", "pps_systematic")) {
        draw_spec$method
      } else {
        NA_character_
      },
      unit = spec_stage_unit(df, stage_spec, stage_idx),
      midstage_element = is_null(stage_spec$clusters) &&
        !spec_has_draw_ids(df, stage_spec, stage_idx) &&
        pos < n_stages,
      strata = spec_stage_strata(df, stage_spec, stage_idx, kind),
      certainty = df[[paste0(".certainty_", stage_idx)]],
      prob = spec_stage_prob(df, stage_idx),
      census = spec_stage_census(df, stage_idx),
      pop_count = df[[paste0(".fpc_", stage_idx)]]
    )
  })
  names(entries) <- as.character(stages)
  structure(
    list(phase = phase, stages = stages, n_rows = nrow(df), stage = entries),
    class = "samplyr_export_spec"
  )
}

#' A variance family the equal-probability estimators do not cover
#'
#' Read from the design alone, so the export and [variance_estimators()]
#' classify a stage the same way.
#' @noRd
stage_is_unequal <- function(draw_spec, kind = survey_stage_kind(draw_spec)) {
  kind %in% c("pps_wor", "unsupported") ||
    (kind %in% c("wr", "rs_poisson") && !is_null(draw_spec$mos))
}

#' A first stage the generic replicate types read as drawn with replacement
#'
#' They resample first-stage units, so a PPS first stage drawn without
#' replacement loses its finite population correction, and Chromy's method
#' is read as with replacement. Unequal probabilities at later stages, and a
#' first stage drawn with replacement, reach the variance through the
#' weights: in a simulation a PPS second stage gave the same replicate
#' variance as its SRS counterpart. A stage drawing one PSU per zone has the
#' with-replacement variance within its groups, so it loses nothing.
#' @noRd
stage_replicated_as_wr <- function(draw_spec,
                                   kind = survey_stage_kind(draw_spec)) {
  (identical(kind, "pps_wor") && !draws_one_per_zone(draw_spec)) ||
    identical(draw_spec$method, "pps_chromy")
}

#' @noRd
spec_has_draw_ids <- function(df, stage_spec, stage_idx) {
  is_multi_hit_method(stage_spec$draw_spec) &&
    paste0(".draw_", stage_idx) %in% names(df)
}

#' @noRd
spec_stage_unit <- function(df, stage_spec, stage_idx) {
  if (spec_has_draw_ids(df, stage_spec, stage_idx)) {
    col <- paste0(".draw_", stage_idx)
    return(list(kind = "draw", source = col, id = df[[col]]))
  }
  if (!is_null(stage_spec$clusters)) {
    # Ids in order of first appearance, even for one column.
    vars <- stage_spec$clusters$vars
    return(list(kind = "cluster", source = vars, id = group_ids(df, vars)))
  }
  list(kind = "element", source = character(0), id = seq_len(nrow(df)))
}

#' Certainty units are a take-all stratum of the stage when its variance
#' family is PPS without replacement and it has any
#' @noRd
spec_stage_strata <- function(df, stage_spec, stage_idx, kind) {
  user <- stage_spec$strata$vars %||% character(0)
  zone_col <- paste0(".zone_", stage_idx)
  pair_col <- paste0(".pair_", stage_idx)
  if (pair_col %in% names(df)) {
    user <- pair_col
  } else if (zone_col %in% names(df)) {
    user <- c(user, zone_col)
  }
  cert_col <- paste0(".certainty_", stage_idx)
  # Certainty units are a take-all stratum at every stage.
  certainty <- if (
    identical(kind, "pps_wor") &&
      cert_col %in% names(df) &&
      any(df[[cert_col]])
  ) {
    df[[cert_col]]
  }
  id <- if (length(user) > 0L || !is_null(certainty)) {
    parts <- df[user]
    if (!is_null(certainty)) {
      parts[[free_name(user, ".certainty")]] <- certainty
    }
    group_ids(parts, names(parts))
  }
  list(user = user, certainty = certainty, id = id)
}

#' @noRd
spec_stage_prob <- function(df, stage_idx) {
  weight <- df[[paste0(".weight_", stage_idx)]]
  if (is_null(weight)) NULL else 1 / weight
}

#' @noRd
spec_stage_census <- function(df, stage_idx) {
  prob <- spec_stage_prob(df, stage_idx)
  !is_null(prob) && all(is_certainty_probability(prob))
}
