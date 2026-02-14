#' Frame Validation and Diagnostics
#'
#' Functions for validating sampling frames and checking design compatibility.
#'
#' @name frame_validation
NULL

#' Validate a Frame Against a Design
#'
#' Checks if a data frame contains all required variables for a sampling
#' design and reports any issues.
#'
#' @param design A `sampling_design` object
#' @param frame A data frame to validate
#' @param stage Which stage(s) to validate against. Default validates all stages.
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
#' @examples
#' # Create a design requiring region stratification and PPS by household count
#' design <- sampling_design() |>
#'   stratify_by(region) |>
#'   cluster_by(ea_id) |>
#'   draw(n = 10, method = "pps_brewer", mos = hh_count)
#'
#' # Validate against niger_eas (should pass)
#' validate_frame(design, niger_eas)
#'
#' # Create a frame missing required variables (will fail)
#' bad_frame <- data.frame(id = 1:100, value = rnorm(100))
#' try(validate_frame(design, bad_frame))
#'
#' # Validate only specific stages of a multi-stage design
#' multi_design <- sampling_design() |>
#'   add_stage(label = "Schools") |>
#'     cluster_by(school_id) |>
#'     draw(n = 30, method = "pps_brewer", mos = enrollment) |>
#'   add_stage(label = "Students") |>
#'     draw(n = 15)
#'
#' # Validate stage 1 only
#' validate_frame(multi_design, tanzania_schools, stage = 1)
#'
#' @export
validate_frame <- function(design, frame, stage = NULL) {
  if (!is_sampling_design(design)) {
    cli_abort("{.arg design} must be a {.cls sampling_design}")
  }

  if (!is.data.frame(frame)) {
    cli_abort("{.arg frame} must be a data frame")
  }

  if (nrow(frame) == 0) {
    cli_abort("Frame has 0 rows")
  }

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
  invisible(TRUE)
}

#' Report validation issues
#' @noRd
report_validation_issues <- function(issues) {
  msgs <- "Frame validation failed:"

  for (issue in issues) {
    stage <- issue$stage
    vars <- issue$vars
    msg <- switch(
      issue$type,
      "stratification" = c(
        "x" = "{stage}: missing stratification variable{?s}: {.val {vars}}"
      ),
      "cluster" = c(
        "x" = "{stage}: missing cluster variable{?s}: {.val {vars}}"
      ),
      "mos" = c(
        "x" = "{stage}: missing MOS variable: {.var {vars}}"
      ),
      "mos_type" = c(
        "x" = "{stage}: MOS variable {.var {vars}} must be numeric, not {.cls {issue$actual_class}}"
      ),
      "mos_na" = c(
        "x" = "{stage}: MOS variable {.var {vars}} contains NA values"
      ),
      "mos_negative" = c(
        "x" = "{stage}: MOS variable {.var {vars}} contains negative values"
      ),
      "prn" = c(
        "x" = "{stage}: missing PRN variable: {.var {vars}}"
      ),
      "prn_type" = c(
        "x" = "{stage}: PRN variable {.var {vars}} must be numeric, not {.cls {issue$actual_class}}"
      ),
      "prn_na" = c(
        "x" = "{stage}: PRN variable {.var {vars}} contains NA values"
      ),
      "prn_range" = c(
        "x" = "{stage}: PRN variable {.var {vars}} must have values in (0, 1)"
      )
    )
    msgs <- c(msgs, msg)
  }
  cli_abort(msgs, call = NULL)
}
