#' Validate a Frame Against a Design
#'
#' Checks if a data frame contains all required variables for a sampling
#' design and reports any issues.
#'
#' @param design A `sampling_design` object
#' @param frame A data frame to validate
#' @param stage Which stage(s) to validate against. Default validates all stages.
#' @param fingerprint How to report differences between `frame` and the
#'   frame fingerprint stored in a design file. Applies to designs restored
#'   with [read_design()] when the design was saved with `frame =`. One of
#'   `"inform"` (default, emits a message), `"warn"`, or `"ignore"`. The
#'   comparison is informational and never fails validation, because a
#'   design remains executable on any frame that passes the variable checks.
#'
#' @return Invisibly returns `TRUE` if validation passes.
#'   Throws an informative error if validation fails.
#'
#' @details
#' Validation checks include:
#' - Presence of required stratification variables
#' - Presence of required clustering variables
#' - Presence of measure of size (MOS) variables for PPS sampling
#' - Non-empty frame
#' - Positive values for MOS variables
#'
#' For designs restored with [read_design()], if the design file carries a
#' frame fingerprint, `validate_frame()` also compares it against `frame`
#' and reports what changed (rows, columns, column types, or content).
#'
#' When `frame` is itself a `tbl_sample` (that is, the design is being
#' prepared as phase 2 of a two-phase sample), `validate_frame()` also
#' pre-flights the two-phase export requirements: the design and the
#' phase-1 sample must share unit identifiers declared with
#' [cluster_by()], and those identifiers must uniquely identify the
#' phase-1 rows. Problems are reported as warnings rather than errors,
#' because selection and weighting work without linkage; only
#' [as_svydesign()] needs it.
#'
#' @examples
#' # Create a design requiring region stratification and PPS by household count
#' design <- sampling_design() |>
#'   stratify_by(region) |>
#'   cluster_by(ea_id) |>
#'   draw(n = 10, method = "pps_brewer", mos = households)
#'
#' # Validate against bfa_eas (should pass)
#' validate_frame(design, bfa_eas)
#'
#' # Create a frame missing required variables (will fail)
#' bad_frame <- data.frame(id = 1:100, value = rnorm(100))
#' try(validate_frame(design, bad_frame))
#'
#' # Validate only specific stages of a multi-stage design
#' zwe_frame <- zwe_eas |>
#'   dplyr::mutate(district_hh = sum(households), .by = district)
#'
#' multi_design <- sampling_design() |>
#'   add_stage(label = "Districts") |>
#'     cluster_by(district) |>
#'     draw(n = 20, method = "pps_brewer", mos = district_hh) |>
#'   add_stage(label = "EAs") |>
#'     draw(n = 10)
#'
#' # Validate stage 1 only
#' validate_frame(multi_design, zwe_frame, stage = 1)
#'
#' # Designs restored from a file also check the stored frame fingerprint
#' path <- tempfile(fileext = ".json")
#' write_design(design, path, frame = bfa_eas)
#' restored <- read_design(path)
#'
#' # Same frame, no message
#' validate_frame(restored, bfa_eas)
#'
#' # A modified frame passes validation with an informational message
#' validate_frame(restored, bfa_eas[-1, ])
#' unlink(path)
#'
#' @export
validate_frame <- function(
  design,
  frame,
  stage = NULL,
  fingerprint = c("inform", "warn", "ignore")
) {
  if (!is_sampling_design(design)) {
    cli_abort("{.arg design} must be a {.cls sampling_design}")
  }

  if (!is.data.frame(frame)) {
    cli_abort("{.arg frame} must be a data frame")
  }

  if (nrow(frame) == 0) {
    cli_abort("Frame has 0 rows")
  }

  fingerprint <- match.arg(fingerprint)
  check_frame_fingerprint(design, frame, fingerprint)

  n_stages <- length(design$stages)
  if (is_null(stage)) {
    stage <- seq_len(n_stages)
  } else {
    if (
      !is_integerish_numeric(stage) ||
        any(stage < 1) ||
        any(stage > n_stages)
    ) {
      cli_abort("{.arg stage} must be between 1 and {n_stages}")
    }
    stage <- as.integer(stage)
  }

  issues <- list()

  for (s in stage) {
    stage_spec <- design$stages[[s]]
    label <- stage_spec$label %||% paste("Stage", s)

    if (!is_null(stage_spec$strata)) {
      strata_vars <- stage_spec$strata$vars
      missing_strata <- setdiff(strata_vars, names(frame))
      if (length(missing_strata) > 0) {
        issues <- c(
          issues,
          list(list(
            stage = label,
            type = "stratification",
            vars = missing_strata
          ))
        )
      } else {
        na_strata <- Filter(function(v) anyNA(frame[[v]]), strata_vars)
        if (length(na_strata) > 0) {
          issues <- c(
            issues,
            list(list(
              stage = label,
              type = "strata_na",
              vars = na_strata
            ))
          )
        }
      }
    }

    if (!is_null(stage_spec$clusters)) {
      cluster_vars <- stage_spec$clusters$vars
      missing_clusters <- setdiff(cluster_vars, names(frame))
      if (length(missing_clusters) > 0) {
        issues <- c(
          issues,
          list(list(
            stage = label,
            type = "cluster",
            vars = missing_clusters
          ))
        )
      } else {
        na_clusters <- Filter(function(v) anyNA(frame[[v]]), cluster_vars)
        if (length(na_clusters) > 0) {
          issues <- c(
            issues,
            list(list(
              stage = label,
              type = "cluster_na",
              vars = na_clusters
            ))
          )
        }
      }
    }

    if (!is_null(stage_spec$draw_spec) && !is_null(stage_spec$draw_spec$mos)) {
      mos_var <- stage_spec$draw_spec$mos
      if (!mos_var %in% names(frame)) {
        issues <- c(
          issues,
          list(list(
            stage = label,
            type = "mos",
            vars = mos_var
          ))
        )
      } else {
        mos_vals <- frame[[mos_var]]
        if (!is.numeric(mos_vals)) {
          issues <- c(
            issues,
            list(list(
              stage = label,
              type = "mos_type",
              vars = mos_var,
              actual_class = class(mos_vals)[[1]]
            ))
          )
        } else {
          if (any(is.na(mos_vals))) {
            issues <- c(
              issues,
              list(list(
                stage = label,
                type = "mos_na",
                vars = mos_var
              ))
            )
          }
          if (any(mos_vals < 0, na.rm = TRUE)) {
            issues <- c(
              issues,
              list(list(
                stage = label,
                type = "mos_negative",
                vars = mos_var
              ))
            )
          }
        }
      }
    }

    if (!is_null(stage_spec$draw_spec) && !is_null(stage_spec$draw_spec$aux)) {
      aux_vars <- stage_spec$draw_spec$aux
      for (av in aux_vars) {
        if (!av %in% names(frame)) {
          issues <- c(
            issues,
            list(list(
              stage = label,
              type = "aux",
              vars = av
            ))
          )
        } else {
          aux_vals <- frame[[av]]
          if (!is.numeric(aux_vals)) {
            issues <- c(
              issues,
              list(list(
                stage = label,
                type = "aux_type",
                vars = av,
                actual_class = class(aux_vals)[[1]]
              ))
            )
          } else if (any(is.na(aux_vals))) {
            issues <- c(
              issues,
              list(list(
                stage = label,
                type = "aux_na",
                vars = av
              ))
            )
          }
        }
      }
    }

    if (!is_null(stage_spec$draw_spec) && !is_null(stage_spec$draw_spec$bounds)) {
      for (var in stage_spec$draw_spec$bounds) {
        if (!var %in% names(frame)) {
          issues <- c(issues, list(list(
            stage = label,
            type = "bounds",
            vars = var
          )))
        } else if (anyNA(frame[[var]])) {
          issues <- c(issues, list(list(
            stage = label,
            type = "bounds_na",
            vars = var
          )))
        }
      }
    }

    if (!is_null(stage_spec$draw_spec) && !is_null(stage_spec$draw_spec$spread)) {
      for (var in stage_spec$draw_spec$spread) {
        if (!var %in% names(frame)) {
          issues <- c(issues, list(list(
            stage = label,
            type = "spread",
            vars = var
          )))
        } else {
          values <- frame[[var]]
          if (!is.numeric(values)) {
            issues <- c(issues, list(list(
              stage = label,
              type = "spread_type",
              vars = var,
              actual_class = class(values)[[1]]
            )))
          } else if (anyNA(values) || any(!is.finite(values))) {
            issues <- c(issues, list(list(
              stage = label,
              type = "spread_na",
              vars = var
            )))
          }
        }
      }
    }

    if (!is_null(stage_spec$draw_spec) && !is_null(stage_spec$draw_spec$control)) {
      control_vars <- extract_control_vars(stage_spec$draw_spec$control)
      missing_control <- setdiff(control_vars, names(frame))
      if (length(missing_control) > 0) {
        issues <- c(
          issues,
          list(list(
            stage = label,
            type = "control",
            vars = missing_control
          ))
        )
      }
    }

    if (!is_null(stage_spec$draw_spec) && !is_null(stage_spec$draw_spec$prn)) {
      prn_var <- stage_spec$draw_spec$prn
      if (!prn_var %in% names(frame)) {
        issues <- c(
          issues,
          list(list(
            stage = label,
            type = "prn",
            vars = prn_var
          ))
        )
      } else {
        prn_vals <- frame[[prn_var]]
        if (!is.numeric(prn_vals)) {
          issues <- c(
            issues,
            list(list(
              stage = label,
              type = "prn_type",
              vars = prn_var,
              actual_class = class(prn_vals)[[1]]
            ))
          )
        } else {
          if (any(is.na(prn_vals))) {
            issues <- c(
              issues,
              list(list(
                stage = label,
                type = "prn_na",
                vars = prn_var
              ))
            )
          }
          if (any(prn_vals <= 0, na.rm = TRUE) || any(prn_vals >= 1, na.rm = TRUE)) {
            issues <- c(
              issues,
              list(list(
                stage = label,
                type = "prn_range",
                vars = prn_var
              ))
            )
          }
        }
      }
    }
  }

  if (length(issues) > 0) {
    report_validation_issues(issues)
  }

  check_phase_linkage(design, frame)

  invisible(TRUE)
}

#' Pre-flight two-phase export linkage when the frame is a tbl_sample.
#'
#' Executing a design on a tbl_sample creates a phase-2 sample.
#' Selection and weighting need nothing extra, but as_svydesign() must
#' join phase-2 rows back to phase-1 rows via shared unit identifiers
#' declared with cluster_by() in both phase designs, and the phase-1
#' keys must uniquely identify its rows. Both requirements otherwise
#' surface only at export time; warn here so the user learns before
#' executing. Warnings, not errors: validation still passes because the
#' phase-2 sample itself is valid without linkage.
#' @noRd
check_phase_linkage <- function(design, frame) {
  if (!is_tbl_sample(frame)) {
    return(invisible(NULL))
  }

  key1 <- survey_key_vars(
    get_design(frame),
    get_stages_executed(frame),
    frame
  )

  # The design is not executed yet, so .draw_k columns cannot
  # participate; its user-declared cluster variables are the linkable
  # keys.
  key2 <- unlist(lapply(design$stages, function(s) s$clusters$vars))
  shared <- intersect(key1, key2)

  if (length(shared) == 0) {
    cli_warn(
      c(
        "No shared phase identifiers between this design and the
         phase-1 sample.",
        "i" = "Executing a design on a {.cls tbl_sample} creates a
               phase-2 sample. Selection and weights work without
               identifiers, but {.fn as_svydesign} needs them to link
               the phases for two-phase variance estimation.",
        "i" = "Declare a shared unit identifier with {.fn cluster_by}
               in both phase designs (e.g. {.code cluster_by(id)})."
      ),
      class = "samplyr_warning_phase_linkage"
    )
    return(invisible(NULL))
  }

  if (anyDuplicated(frame[, shared, drop = FALSE]) > 0) {
    cli_warn(
      c(
        "Shared phase identifier{?s} {.val {shared}} {?does/do} not
         uniquely identify rows of the phase-1 sample.",
        "i" = "{.fn as_svydesign} joins phase-2 rows back to phase-1
               rows by {cli::qty(shared)}{?this/these}
               identifier{?s} and requires phase-1 keys to be unique.",
        "i" = "Add a row-level identifier via {.fn cluster_by} at the
               final stage of the phase-1 design."
      ),
      class = "samplyr_warning_phase_linkage"
    )
  }

  invisible(NULL)
}

#' Report validation issues
#' @noRd
report_validation_issues <- function(issues) {
  bullets <- vapply(issues, function(issue) {
    stage <- issue$stage
    vars <- issue$vars
    switch(
      issue$type,
      "stratification" = cli::format_inline(
        "{stage}: missing stratification variable{?s}: {.val {vars}}"
      ),
      "cluster" = cli::format_inline(
        "{stage}: missing cluster variable{?s}: {.val {vars}}"
      ),
      "strata_na" = cli::format_inline(
        "{stage}: stratification variable{?s} {.var {vars}} contain{?s/} NA values"
      ),
      "cluster_na" = cli::format_inline(
        "{stage}: cluster variable{?s} {.var {vars}} contain{?s/} NA values"
      ),
      "mos" = cli::format_inline(
        "{stage}: missing MOS variable: {.var {vars}}"
      ),
      "mos_type" = cli::format_inline(
        "{stage}: MOS variable {.var {vars}} must be numeric, not {.cls {issue$actual_class}}"
      ),
      "mos_na" = cli::format_inline(
        "{stage}: MOS variable {.var {vars}} contains NA values"
      ),
      "mos_negative" = cli::format_inline(
        "{stage}: MOS variable {.var {vars}} contains negative values"
      ),
      "aux" = cli::format_inline(
        "{stage}: missing auxiliary variable: {.var {vars}}"
      ),
      "aux_type" = cli::format_inline(
        "{stage}: auxiliary variable {.var {vars}} must be numeric, not {.cls {issue$actual_class}}"
      ),
      "aux_na" = cli::format_inline(
        "{stage}: auxiliary variable {.var {vars}} contains NA values"
      ),
      "bounds" = cli::format_inline(
        "{stage}: missing count-bound variable: {.var {vars}}"
      ),
      "bounds_na" = cli::format_inline(
        "{stage}: count-bound variable {.var {vars}} contains NA values"
      ),
      "spread" = cli::format_inline(
        "{stage}: missing spatial coordinate variable: {.var {vars}}"
      ),
      "spread_type" = cli::format_inline(
        "{stage}: spatial coordinate variable {.var {vars}} must be numeric, not {.cls {issue$actual_class}}"
      ),
      "spread_na" = cli::format_inline(
        "{stage}: spatial coordinate variable {.var {vars}} must be finite with no missing values"
      ),
      "control" = cli::format_inline(
        "{stage}: missing control variable{?s}: {.val {vars}}"
      ),
      "prn" = cli::format_inline(
        "{stage}: missing PRN variable: {.var {vars}}"
      ),
      "prn_type" = cli::format_inline(
        "{stage}: PRN variable {.var {vars}} must be numeric, not {.cls {issue$actual_class}}"
      ),
      "prn_na" = cli::format_inline(
        "{stage}: PRN variable {.var {vars}} contains NA values"
      ),
      "prn_range" = cli::format_inline(
        "{stage}: PRN variable {.var {vars}} must have values in (0, 1)"
      )
    )
  }, character(1))
  names(bullets) <- rep("x", length(bullets))
  cli_abort(c("Frame validation failed:", bullets), call = NULL)
}

#' Compare a frame against a stored fingerprint and report differences
#'
#' Designs restored with read_design() carry the fingerprint written by
#' write_design(..., frame =) in the "frame_info" attribute. The
#' comparison is informational only. Executing a design on a different
#' frame is a supported workflow, so a mismatch must never fail
#' validation.
#' @noRd
check_frame_fingerprint <- function(design, frame, fingerprint) {
  if (identical(fingerprint, "ignore")) {
    return(invisible(NULL))
  }
  fp <- attr(design, "frame_info")$fingerprint
  if (is_null(fp)) {
    return(invisible(NULL))
  }

  diffs <- fingerprint_differences(fp, frame)
  if (length(diffs) == 0) {
    return(invisible(NULL))
  }

  msg <- c(
    "Frame differs from the one recorded when the design was saved:",
    setNames(diffs, rep("*", length(diffs))),
    "i" = "This is informational. The design remains executable on any
           frame that passes the variable checks."
  )
  if (identical(fingerprint, "warn")) {
    cli_warn(msg)
  } else {
    cli::cli_inform(msg)
  }
  invisible(NULL)
}

#' Describe how a frame differs from a stored fingerprint
#' @noRd
fingerprint_differences <- function(fp, frame) {
  if (identical(fp$hash, frame_content_hash(frame))) {
    return(character(0))
  }

  diffs <- character(0)

  fp_nrow <- fp$nrow
  if (!is_null(fp_nrow) && fp_nrow != nrow(frame)) {
    diffs <- c(
      diffs,
      cli::format_inline(
        "{nrow(frame)} row{?s} instead of the {fp_nrow} recorded"
      )
    )
  }

  recorded_names <- vapply(
    fp$columns,
    function(col) as.character(col$name),
    character(1)
  )
  recorded_types <- vapply(
    fp$columns,
    function(col) as.character(col$type),
    character(1)
  )

  removed <- setdiff(recorded_names, names(frame))
  if (length(removed) > 0) {
    diffs <- c(
      diffs,
      cli::format_inline(
        "recorded column{?s} {.val {removed}} no longer present"
      )
    )
  }

  added <- setdiff(names(frame), recorded_names)
  if (length(added) > 0) {
    diffs <- c(
      diffs,
      cli::format_inline("new column{?s} {.val {added}}")
    )
  }

  common <- intersect(recorded_names, names(frame))
  current_types <- vapply(
    frame[common],
    function(x) class(x)[[1]],
    character(1)
  )
  previous_types <- recorded_types[match(common, recorded_names)]
  changed <- common[current_types != previous_types]
  if (length(changed) > 0) {
    details <- paste0(
      changed,
      " (",
      current_types[match(changed, common)],
      " instead of ",
      previous_types[match(changed, common)],
      ")"
    )
    diffs <- c(
      diffs,
      cli::format_inline("column type{?s} changed: {details}")
    )
  }

  if (length(diffs) == 0) {
    diffs <- "same structure but different content (hash mismatch)"
  }
  diffs
}
