#' Validate a Frame Against a Design
#'
#' Checks if a data frame contains all required variables for a sampling
#' design and reports any issues.
#'
#' @param design A `sampling_design` object, or an executed
#'   `tbl_sample`: its stored design is validated and its frame digest
#'   (when present) is compared against `frame`.
#' @param frame A data frame to validate
#' @param stage Which stage(s) to validate against. Default validates all stages.
#' @param fingerprint How to report differences between `frame` and
#'   what was recorded earlier: the frame fingerprint stored in a
#'   design file (designs restored with [read_design()] when saved
#'   with `frame =`) and the frame digest recorded at execution (when
#'   `design` is a `tbl_sample`, or a restored design whose receipt
#'   carries one). One of `"inform"` (default, emits a message),
#'   `"warn"`, or `"ignore"`. Both comparisons are informational and
#'   never fail validation, because a design remains executable on any
#'   frame that passes the variable checks.
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
#' When a frame digest is available (an executed `tbl_sample`, or a
#' design file written from one), the structural comparison goes
#' further: the role-scoped fingerprint (analysis columns added later
#' do not trigger it), the frame size, and per-pool population sizes
#' recomputed from `frame` at every stage the digest can anchor
#' (stage 1 over the universe; later stages under the recorded
#' parents). The report says where the frame drifted, not merely that
#' it did.
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
  digest <- NULL
  if (is_tbl_sample(design)) {
    digest <- get_frame_digest(design)
    if (!is_null(digest) && identical(digest$status, "invalidated")) {
      digest <- NULL
    }
    design <- get_design(design)
  } else if (is_sampling_design(design)) {
    digest <- attr(design, "execution")$frame_digest
  }
  if (!is_sampling_design(design)) {
    cli_abort(
      "{.arg design} must be a {.cls sampling_design} or a
       {.cls tbl_sample}"
    )
  }

  if (!is.data.frame(frame)) {
    cli_abort("{.arg frame} must be a data frame")
  }

  if (nrow(frame) == 0) {
    cli_abort("Frame has 0 rows")
  }

  fingerprint <- match.arg(fingerprint)
  check_frame_fingerprint(design, frame, fingerprint)
  check_digest_drift(digest, design, frame, fingerprint)

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

#' Report structural drift between a frame and a recorded digest
#'
#' Compares the supplied frame against the population structure the
#' frame digest recorded at execution: the role-scoped fingerprint,
#' the frame size, and per-pool population sizes recomputed from the
#' frame at every stage the digest can anchor (stage 1 over the
#' universe; later stages under the recorded parents). Informational,
#' like the fingerprint check: a drifted frame remains executable.
#' @noRd
check_digest_drift <- function(digest, design, frame, fingerprint) {
  if (is_null(digest) || identical(fingerprint, "ignore")) {
    return(invisible(NULL))
  }
  diffs <- tryCatch(
    digest_frame_drift(digest, design, frame),
    error = function(e) {
      cli::format_inline(
        "the drift comparison itself failed ({conditionMessage(e)})"
      )
    }
  )
  if (length(diffs) == 0) {
    return(invisible(NULL))
  }
  msg <- c(
    "Frame structure differs from the digest recorded at execution:",
    setNames(diffs, rep("*", length(diffs))),
    "i" = "This is informational. The design remains executable, but
           replaying or extending the recorded sample on this frame
           would not reproduce it."
  )
  if (identical(fingerprint, "warn")) {
    cli_warn(msg, class = "samplyr_warning_digest_drift")
  } else {
    cli::cli_inform(msg, class = "samplyr_message_digest_drift")
  }
  invisible(NULL)
}

#' @return Character vector of drift descriptions; empty = no drift.
#' @noRd
digest_frame_drift <- function(digest, design, frame) {
  rec <- digest$frames[[1]]

  # Exact content match: nothing can have drifted.
  if (
    !is_null(rec$fingerprint_exact) &&
      identical(rec$fingerprint_exact, frame_content_hash(frame))
  ) {
    return(character(0))
  }

  diffs <- character(0)

  role_cols <- unique(rec$roles$column)
  missing_roles <- setdiff(role_cols, names(frame))
  if (length(missing_roles) > 0) {
    diffs <- c(
      diffs,
      cli::format_inline(
        "design-relevant column{?s} {.val {missing_roles}} no longer
         present"
      )
    )
  }
  roles_match <- FALSE
  if (
    length(missing_roles) == 0 && !is_null(rec$fingerprint_roles)
  ) {
    roles_match <- identical(
      rec$fingerprint_roles,
      frame_content_hash(frame, columns = role_cols)
    )
    if (!roles_match) {
      diffs <- c(
        diffs,
        "design-relevant columns changed (role-scoped fingerprint
         mismatch)"
      )
    }
  }

  if (!is_null(rec$n_rows) && rec$n_rows != nrow(frame)) {
    diffs <- c(
      diffs,
      cli::format_inline(
        "{nrow(frame)} row{?s} instead of the {rec$n_rows} recorded"
      )
    )
  }

  # Identical role content at identical size: the pool structure is
  # unchanged by construction; skip the per-stage recount.
  if (roles_match && rec$n_rows == nrow(frame)) {
    return(diffs)
  }
  if (length(missing_roles) > 0) {
    return(diffs)
  }

  pool_diffs <- character(0)
  parent_keys <- NULL
  for (pos in seq_along(digest$stages)) {
    st <- digest$stages[[pos]]
    spec <- design$stages[[st$stage_id]]
    label <- spec$label %||% paste("Stage", st$stage_id)
    ancestor_vars <- collect_ancestor_cluster_vars(design, st$stage_id)
    if (!all(c(ancestor_vars, st$strata) %in% names(frame))) {
      break
    }

    if (pos > 1L) {
      if (is_null(parent_keys)) {
        break
      }
      scope_keys <- digest_path_keys(
        frame, seq_len(nrow(frame)), ancestor_vars
      )
      key_of_pool <- parent_keys[
        match(st$pools$parent_unit, parent_keys$unit_id), "key"
      ]
    }

    for (p in seq_len(nrow(st$pools))) {
      pool <- st$pools[p, , drop = FALSE]
      # Design-resolved pools hang under unselected parents, whose
      # ancestry keys the digest deliberately does not retain.
      if (pos > 1L && is.na(key_of_pool[p])) {
        next
      }
      rows <- if (pos == 1L) {
        seq_len(nrow(frame))
      } else {
        which(scope_keys == key_of_pool[p])
      }
      for (v in st$strata %||% character(0)) {
        rows <- rows[
          as.character(frame[[v]][rows]) == as.character(pool[[v]])
        ]
      }
      n_now <- if (identical(st$unit_level, "cluster")) {
        cluster_vars <- spec$clusters$vars
        length(unique(digest_path_keys(
          frame, rows, c(ancestor_vars, cluster_vars)
        )))
      } else {
        length(rows)
      }
      if (n_now != pool$N) {
        pool_label <- paste(
          c(
            if (pos > 1L) {
              paste0(
                "under ", gsub("\x1f", "/", key_of_pool[p], fixed = TRUE)
              )
            },
            vapply(
              st$strata %||% character(0),
              function(v) paste0(v, " = ", as.character(pool[[v]])),
              character(1)
            )
          ),
          collapse = ", "
        )
        pool_diffs <- c(
          pool_diffs,
          cli::format_inline(
            "{label}{if (nzchar(pool_label)) paste0(' (', pool_label, ')')}:
             {n_now} units instead of the {pool$N} recorded"
          )
        )
      }
    }

    sel <- st$selected
    if (
      identical(st$unit_level, "cluster") &&
        !is_null(sel) && "key" %in% names(sel)
    ) {
      parent_keys <- sel[!duplicated(sel$unit_id), c("unit_id", "key")]
    } else {
      parent_keys <- NULL
    }
  }

  if (length(pool_diffs) > 5) {
    pool_diffs <- c(
      pool_diffs[1:5],
      cli::format_inline(
        "and {length(pool_diffs) - 5L} more pool difference{?s}"
      )
    )
  }

  # Chance drift: resolve the chances the design would use on this
  # frame (the ex-ante digest) and compare them to the recorded ones.
  # This is sharper than the role-scoped fingerprint: a size measure
  # rescaled by a constant factor changes the bytes but not one
  # selection chance.
  chance <- digest_chance_drift(digest, design, frame)
  all_diffs <- c(diffs, pool_diffs, chance$diffs)
  if (
    length(all_diffs) > 0 && length(chance$diffs) == 0 &&
      chance$n_compared > 0
  ) {
    all_diffs <- c(
      all_diffs,
      cli::format_inline(
        "resolved selection chances are unchanged in the
         {chance$n_compared} comparable pool{?s}"
      )
    )
  }
  all_diffs
}

#' Compare recorded selection chances against the design's ex-ante
#' resolution over a frame
#'
#' Pools are lined up by parent ancestry key (recorded side: the
#' selected-trace keys; ex-ante side: the keys the builder retains)
#' plus stratum labels, so only pools the recorded digest can anchor
#' are compared: stage-1 pools always, later pools under selected
#' parents. Each side's retained representation is expanded to a
#' sorted chance vector; pools whose sizes differ are left to the
#' recount. Designs the ex-ante builder refuses (with-replacement or
#' element-level parents) skip the comparison silently: the
#' structural checks have already run.
#'
#' @return list(diffs = character per-stage drift lines,
#'   n_compared = number of pools compared).
#' @noRd
digest_chance_drift <- function(digest, design, frame) {
  none <- list(diffs = character(0), n_compared = 0L)
  exante <- tryCatch(
    build_exante_digest(design, frame),
    error = function(e) NULL
  )
  if (is_null(exante)) {
    return(none)
  }
  ex_keys <- attr(exante, "exante_pool_keys")
  ex_ids <- vapply(exante$stages, function(s) s$stage_id, integer(1))

  strata_label_key <- function(pools, strata) {
    if (is_null(strata)) {
      rep("", nrow(pools))
    } else {
      do.call(paste, c(
        lapply(strata, function(v) as.character(pools[[v]])),
        sep = "\x1f"
      ))
    }
  }
  pool_chances <- function(stg, p) {
    pid <- stg$pools$pool_id[p]
    switch(
      stg$storage,
      constant = rep(stg$pools$chance[p], stg$pools$N[p]),
      units = sort(stg$units$chance[stg$units$pool_id == pid]),
      quantiles = {
        b <- stg$chance_distribution[
          stg$chance_distribution$pool_id == pid, , drop = FALSE
        ]
        if (!"n_units" %in% names(b)) {
          return(NULL)
        }
        b <- b[order(b$quantile), , drop = FALSE]
        rep(b$chance, b$n_units)
      }
    )
  }

  diffs <- character(0)
  n_compared <- 0L
  parent_keys <- NULL
  for (pos in seq_along(digest$stages)) {
    st <- digest$stages[[pos]]
    ex_pos <- match(st$stage_id, ex_ids)
    rec_parent <- if (pos == 1L) {
      rep("", nrow(st$pools))
    } else if (is_null(parent_keys)) {
      rep(NA_character_, nrow(st$pools))
    } else {
      parent_keys$key[match(st$pools$parent_unit, parent_keys$unit_id)]
    }

    ex <- if (is.na(ex_pos)) NULL else exante$stages[[ex_pos]]
    ex_match <- rep(NA_integer_, nrow(st$pools))
    comparable <- logical(nrow(st$pools))
    if (
      !is_null(ex) && st$frame_ref == 1L &&
        identical(st$chance_kind, ex$chance_kind)
    ) {
      rec_key <- paste(
        rec_parent, strata_label_key(st$pools, st$strata),
        sep = "\x1f\x1f"
      )
      ex_match <- match(
        rec_key,
        paste(
          ex_keys[[ex_pos]], strata_label_key(ex$pools, ex$strata),
          sep = "\x1f\x1f"
        )
      )
      comparable <- !is.na(rec_parent) & !is.na(ex_match) &
        st$pools$chance_status != "unavailable"
    }

    n_stage <- 0L
    n_drifted <- 0L
    max_diff <- 0
    for (p in which(comparable)) {
      rec_ch <- pool_chances(st, p)
      ex_ch <- pool_chances(ex, ex_match[p])
      if (
        is_null(rec_ch) || is_null(ex_ch) || anyNA(rec_ch) ||
          length(rec_ch) != length(ex_ch)
      ) {
        next
      }
      n_stage <- n_stage + 1L
      d <- max(abs(sort(rec_ch) - ex_ch))
      if (d > 1e-9) {
        n_drifted <- n_drifted + 1L
        max_diff <- max(max_diff, d)
      }
    }
    n_compared <- n_compared + n_stage
    if (n_drifted > 0) {
      spec <- design$stages[[st$stage_id]]
      label <- spec$label %||% paste("Stage", st$stage_id)
      diffs <- c(
        diffs,
        cli::format_inline(
          "{label}: selection chances differ in {n_drifted} of
           {n_stage} comparable pool{?s} (largest difference
           {signif(max_diff, 2)})"
        )
      )
    }

    sel <- st$selected
    if (
      identical(st$unit_level, "cluster") &&
        !is_null(sel) && "key" %in% names(sel)
    ) {
      parent_keys <- sel[!duplicated(sel$unit_id), c("unit_id", "key")]
    } else {
      parent_keys <- NULL
    }
  }
  list(diffs = diffs, n_compared = n_compared)
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
  has_content_hash <- !is_null(fp$hash)
  if (has_content_hash && identical(fp$hash, frame_content_hash(frame))) {
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

  if (length(diffs) == 0 && has_content_hash) {
    diffs <- "same structure but different content (hash mismatch)"
  }
  diffs
}
