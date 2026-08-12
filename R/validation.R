#' Validate a frame against a design
#'
#' Checks if a data frame contains all required variables for a sampling
#' design and reports any issues.
#'
#' @param design A `sampling_design` object, or a `tbl_sample`. A fully
#'   executed sample validates its stored design and compares its frame
#'   digest (when present) against `frame`. A partial sample validates
#'   the frame that would continue it, against the units it selected.
#' @param frame A data frame to validate, or an ordered list of stage
#'   frames: one per stage, in the order [execute()] would receive them.
#'   Each entry may itself be hierarchical. A data frame and a
#'   one-element list are the same input, as they are in [execute()],
#'   and every check here is one [execute()] runs before it samples: a
#'   frame this accepts is a frame execution accepts.
#' @param ... These dots are for future extensions and must be empty.
#'   `stages` and the arguments after it follow `...`, so each must be
#'   named exactly: the singular `stage` is reported rather than
#'   prefix-matched.
#' @param stages Which stage(s) to validate against. Defaults to all
#'   stages, and for a partial `tbl_sample` to every remaining stage,
#'   matching [execute()]. Where one frame cannot say whether it is the
#'   next stage's register or a hierarchy covering the rest, `stages` is
#'   required, again as in [execute()].
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
#' (stage 1 over the universe and later stages under the recorded
#' parents). The report says where the frame drifted, not merely that
#' it did.
#'
#' ## Ordered stage frames
#'
#' A data frame means one hierarchy. A list means separately supplied
#' stage frames, and `validate_frame()` then checks everything
#' [execute()] checks before it draws: the frame count, the columns
#' each stage selects on, cluster-level variables that must be constant
#' within a unit, ancestry types that must be joinable, and ancestry
#' values that name no parent.
#'
#' It also checks candidate coverage, and is stricter there than
#' execution: a unit reachable at one stage with no rows in the
#' register the next stage samples from is
#' `samplyr_error_frame_incomplete_register`. [execute()] only warns,
#' because the sample it happens to draw may never need that unit. The
#' two agree on realized parents, which fail in both.
#'
#' ## Continuing a partial sample
#'
#' When `design` is a `tbl_sample` with stages left to run,
#' `validate_frame()` checks the supplied frames against the units that
#' sample actually selected, through the same transition the
#' continuation would use: complete ancestry, joinable key types, no
#' missing parent identifiers, and a row for every selected parent. It
#' accepts the same shapes [execute()] does, so one register per
#' remaining stage works here as it does there. The recorded fingerprint
#' and frame digest are not compared, because they describe the frame
#' the executed stages drew from rather than the register the next stage
#' needs.
#'
#' ## Preparing a second phase
#'
#' When `frame` is itself a `tbl_sample` (that is, the design is being
#' prepared as phase 2 of a two-phase sample), `validate_frame()` also
#' pre-flights the two-phase export requirements. The phases declare
#' their sampling units independently, so the link is the compound of
#' every identifier either phase declares with [cluster_by()] that both
#' samples carry, which is the bridge [as_svydesign()] builds. It warns
#' when neither phase declares an identifier the phase-1 sample carries,
#' and when the identifiers together do not uniquely identify phase-1
#' rows. Problems are reported as warnings rather than errors, because
#' selection and weighting work without linkage. Only [as_svydesign()]
#' needs it.
#'
#' A previous-phase sample also carries identifiers that later stages
#' must keep. One that varies inside a sampling unit cannot be carried,
#' and is refused here for the same reason [execute()] refuses it: a
#' clustered stage keeps one representative row, so by the time anything
#' downstream looks, the disagreement is gone.
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
#' validate_frame(multi_design, zwe_frame, stages = 1)
#'
#' # One register per stage: a list, in the order execute() receives them
#' districts <- zwe_frame |>
#'   dplyr::distinct(district, district_hh)
#' validate_frame(multi_design, list(districts, zwe_eas))
#'
#' # A partial sample validates the register that would continue it
#' stage1 <- execute(multi_design, districts, stages = 1, seed = 5)
#' validate_frame(stage1, zwe_eas)
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
#' @family execution
#' @export
validate_frame <- function(
  design,
  frame,
  ...,
  stages = NULL,
  fingerprint = c("inform", "warn", "ignore")
) {
  check_keyword_args(enquos(...), c("stages", "fingerprint"))
  fingerprint <- match.arg(fingerprint)

  digest <- NULL
  partial_sample <- NULL
  # Read before `design` is replaced by the design it carries: what the caller
  # passed decides whether generated columns and a dropped sample class are
  # legitimate, exactly as `.data` does in execute().
  continuing <- is_tbl_sample(design)
  if (is_tbl_sample(design)) {
    digest <- get_frame_digest(design)
    if (!is_null(digest) && identical(digest$status, "invalidated")) {
      digest <- NULL
    }
    # Kept only long enough to validate a next-stage frame against the units
    # this sample actually selected.
    if (length(remaining_stages(design)) > 0) {
      partial_sample <- design
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

  # One frame and a one-element list are the same input, as they are in
  # execute(). Both spellings then run the same checks in the same order:
  # a preflight that approves what execution refuses is worse than none.
  supplied <- normalize_frame_input(frame)
  check_frames_executable(
    supplied$frames,
    labels = supplied$labels,
    allow_generated = continuing,
    allow_stripped = continuing
  )

  validate_frame_registers(
    design,
    supplied$frames,
    stages,
    fingerprint,
    partial_sample,
    digest = digest
  )
}

#' Stages of a sample's design that it has not executed
#' @noRd
remaining_stages <- function(sample) {
  setdiff(
    seq_along(get_design(sample)$stages), get_stages_executed(sample)
  )
}

#' Walk the candidate population through every stage transition
#'
#' The deterministic counterpart of an execution. Every candidate is treated as
#' selected, so each register is linked and carried exactly as it would be, and
#' what a stage will actually see is available to validate. Without this,
#' validation judges raw registers and contradicts execution in both
#' directions: it rejects a register that legitimately omits a carried stratum,
#' and accepts one whose copy of that stratum disagrees.
#'
#' @param previous_sample The realized sample a continuation extends, or `NULL`
#'   for a design start, where the first register is the population.
#' @return One effective frame per scheduled entry.
#' @noRd
effective_register_frames <- function(schedule, design, previous_sample = NULL,
                                      phase_link_vars = character(0),
                                      call = caller_env()) {
  entries <- schedule$entries
  effective <- vector("list", length(entries))
  parent <- previous_sample

  for (i in seq_along(entries)) {
    entry <- entries[[i]]
    effective[[i]] <- if (is_null(parent)) {
      entry$frame
    } else {
      link_stage_frame(
        entry$frame, parent, design, entry$stage,
        frame_index = entry$frame_index, frame_label = entry$frame_label,
        phase_link_vars = phase_link_vars, call = call
      )$frame
    }
    parent <- effective[[i]]
  }
  effective
}

#' Validate an ordered list of stage frames without drawing a sample
#'
#' Explicit validation is strict where execution is permissive: a candidate
#' parent no later register can serve is a defect here, while `execute()` only
#' warns because the realization it draws may never need that parent. Every
#' other check is the one execution runs, called on the same primitives so the
#' two cannot drift.
#' @noRd
validate_frame_registers <- function(design, frames, stages, fingerprint,
                                     partial_sample = NULL,
                                     digest = NULL,
                                     call = caller_env()) {
  executed <- if (is_null(partial_sample)) {
    NULL
  } else {
    get_stages_executed(partial_sample)
  }
  previous_sample <- if (is_null(partial_sample)) {
    NULL
  } else {
    as.data.frame(partial_sample)
  }

  # Frame count, phase-frame position, parent identity, and every column each
  # stage selects on: all static, all already defined for execution.
  schedule <- stage_frame_schedule(
    design, frames, stages, executed = executed, call = call
  )

  # Both comparisons describe the frames the executed stages sampled from. A
  # partial sample is being handed the register for a stage that has not run,
  # which is a different table by design, so comparing them would report drift
  # on every correct call.
  if (is_null(partial_sample)) {
    check_frame_fingerprint(design, frames, fingerprint)
    check_digest_drift(digest, design, frames[[1]], fingerprint)
  }

  # A previous-phase sample in the first position, exactly as execution reads
  # it. Only a design start can have one: a continuation stays in its phase.
  prev_phase <- NULL
  if (is_null(partial_sample)) {
    prepared <- prepare_multiphase_frame(schedule$entries[[1]]$frame)
    prev_phase <- prepared$prev_phase
    if (!is_null(prev_phase)) {
      for (i in seq_along(schedule$entries)) {
        if (schedule$entries[[i]]$frame_index == 1L) {
          schedule$entries[[i]]$frame <- prepared$frame
        }
      }
    }
  } else {
    prev_phase <- attr(partial_sample, "metadata")$prev_phase
  }
  phase_link_vars <- phase_link_vars_of(prev_phase)
  check_phase_key_invariance(schedule, design, phase_link_vars, prev_phase)
  # Judged on the frame as supplied, not the prepared one: preparation strips
  # the generated columns, and the identifiers this reports on are exactly
  # what a phase-1 sample carries.
  check_phase_linkage(design, frames[[1]])

  # Rules about the registers as supplied. Ancestry NAs stay here rather than
  # moving to the effective frame: linking filters those rows out, so by then
  # the defect the caller asked about is gone.
  for (entry in schedule$entries) {
    check_register_cluster_invariants(design, entry, call = call)
    parent_vars <- collect_ancestor_cluster_vars(design, entry$stage)
    check_parent_key_na(
      entry$frame, intersect(parent_vars, names(entry$frame)), design,
      entry$stage, entry$frame_index, entry$frame_label, call = call
    )
  }
  check_register_key_types(design, schedule, call = call)
  abort_incomplete_registers(
    schedule, design, previous_sample = previous_sample, call = call
  )

  # Everything below judges what each stage will actually select from, after
  # its register has been linked to its parents and carried their variables.
  effective <- effective_register_frames(
    schedule, design, previous_sample, phase_link_vars, call = call
  )

  issues <- list()
  for (i in seq_along(schedule$entries)) {
    stage_idx <- schedule$entries[[i]]$stage
    issues <- c(issues, stage_frame_issues(
      design$stages[[stage_idx]], effective[[i]], stage_idx
    ))
  }
  if (length(issues) > 0) {
    report_validation_issues(issues)
  }

  invisible(TRUE)
}

#' Require cluster-level design variables to be constant within each unit
#'
#' A register supplied on its own is not yet restricted to one parent, so the
#' unit is the full ancestry plus this stage's cluster: `C1` under one school
#' and `C1` under another are different classes, and grouping on the local
#' identifier alone would compare their values.
#' @noRd
check_register_cluster_invariants <- function(design, entry,
                                              call = caller_env()) {
  spec <- design$stages[[entry$stage]]
  if (is_null(spec$clusters)) {
    return(invisible(NULL))
  }
  frame <- entry$frame
  unit_vars <- intersect(
    unique(c(
      collect_ancestor_cluster_vars(design, entry$stage), spec$clusters$vars
    )),
    names(frame)
  )
  if (length(unit_vars) == 0) {
    return(invisible(NULL))
  }
  invariant <- setdiff(
    cluster_invariant_vars(frame, spec$draw_spec, spec$strata$vars), unit_vars
  )
  if (length(invariant) == 0) {
    return(invisible(NULL))
  }

  n_units <- nrow(unique(frame[, unit_vars, drop = FALSE]))
  varying <- invariant[vapply(
    invariant,
    function(v) {
      nrow(unique(frame[, c(unit_vars, v), drop = FALSE])) != n_units
    },
    logical(1)
  )]
  if (length(varying) == 0) {
    return(invisible(NULL))
  }

  abort_samplyr(
    c(
      "{.field {varying}} {?is/are} not constant within {.field {unit_vars}}
       in {frame_token(entry$frame_index, entry$frame_label)}.",
      "x" = "{stage_token(design, entry$stage)} selects on one row per unit,
             so a value that varies inside a unit makes the selection depend
             on which row comes first.",
      "i" = "Give each unit one value, or drop the column from this frame."
    ),
    class = "samplyr_error_frame_cluster_invariant",
    call = call
  )
}

#' Every value check one stage makes against one frame
#'
#' Split out so the shared-frame form and the ordered-register form apply the
#' same checks. A register form differs only in which frame each stage is
#' judged against.
#' @noRd
stage_frame_issues <- function(stage_spec, frame, stage_idx) {
  issues <- list()
  label <- stage_spec$label %||% paste("Stage", stage_idx)

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


  issues
}

#' Pre-flight two-phase export linkage when the frame is a tbl_sample.
#'
#' Executing a design on a tbl_sample creates a phase-2 sample. Selection
#' and weighting need nothing extra, but as_svydesign() must join phase-2
#' rows back to phase-1 rows, and this reports before execution what that
#' join will find.
#'
#' The phases declare their sampling units independently: phase 1 by PSU,
#' phase 2 by household and person, with neither obliged to redeclare the
#' other's. The bridge is therefore every identifier either phase declares
#' that both samples carry, taken together as one compound key, which is
#' what resolve_phase_bridge() builds at export. Requiring the two
#' declarations to intersect would reject the ordinary case where they
#' name different levels of the same hierarchy.
#'
#' Warnings, not errors: validation still passes because the phase-2
#' sample itself is valid without linkage.
#' @noRd
check_phase_linkage <- function(design, frame) {
  if (!is_tbl_sample(frame)) {
    return(invisible(NULL))
  }

  phase1_ids <- survey_key_vars(
    get_design(frame),
    get_stages_executed(frame),
    frame
  )
  # The design is not executed yet, so .draw_k columns cannot participate.
  # its user-declared cluster variables are what it will contribute.
  phase2_ids <- unlist(lapply(design$stages, function(s) s$clusters$vars))

  # Only the phase-1 side is knowable now. A phase-2 identifier the phase-1
  # sample also carries is already part of the bridge. One it does not is
  # carried onto phase-2 rows at execution and cannot be judged here.
  bridge <- intersect(unique(c(phase1_ids, phase2_ids)), names(frame))

  if (length(bridge) == 0) {
    cli_warn(
      c(
        "Neither phase declares a unit identifier the phase-1 sample
         carries.",
        "i" = "Executing a design on a {.cls tbl_sample} creates a
               phase-2 sample. Selection and weights work without
               identifiers, but {.fn as_svydesign} needs them to link
               the phases for two-phase variance estimation.",
        "i" = "Declare the sampling units of either phase with
               {.fn cluster_by}. The phases need not declare the same
               units."
      ),
      class = "samplyr_warning_phase_linkage"
    )
    return(invisible(NULL))
  }

  if (anyDuplicated(frame[, bridge, drop = FALSE]) > 0) {
    cli_warn(
      c(
        "The phase identifier{?s} {.val {bridge}} {?does/do} not uniquely
         identify rows of the phase-1 sample.",
        "i" = "{.fn as_svydesign} joins phase-2 rows back to phase-1 rows
               on every identifier either phase declares, and requires
               the phase-1 rows it reaches to be unique.",
        "i" = "Declare a finer identifier with {.fn cluster_by} in either
               phase, for example the row-level unit at the final stage."
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
#' universe and later stages under the recorded parents). Informational,
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

#' @return Character vector of drift descriptions. It is empty when there is
#'   no drift.
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
  # unchanged by construction, so skip the per-stage recount.
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
        parent_label <- if (pos > 1L) {
          if (length(ancestor_vars) > 1L) {
            display_path_key(key_of_pool[p])
          } else {
            key_of_pool[p]
          }
        }
        pool_label <- paste(
          c(
            if (pos > 1L) {
              paste0("under ", parent_label)
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

  # Compare resolved chances as well as role-scoped frame fingerprints.
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
#' selected-trace keys, while the ex-ante side uses the keys the builder retains)
#' plus stratum labels, so only pools the recorded digest can anchor
#' are compared: stage-1 pools always, later pools under selected
#' parents. Each side's retained representation is expanded to a
#' sorted chance vector. Pools whose sizes differ are left to the
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
    exante_digest(design, frame),
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
      make_group_key(pools, strata)
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
      rec_key_df <- data.frame(
        .parent = rec_parent,
        .stratum = strata_label_key(st$pools, st$strata)
      )
      rec_key <- make_group_key(rec_key_df, names(rec_key_df))
      ex_key_df <- data.frame(
        .parent = ex_keys[[ex_pos]],
        .stratum = strata_label_key(ex$pools, ex$strata)
      )
      ex_match <- match(
        rec_key,
        make_group_key(ex_key_df, names(ex_key_df))
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
check_frame_fingerprint <- function(design, frames, fingerprint) {
  if (identical(fingerprint, "ignore")) {
    return(invisible(NULL))
  }
  diffs <- fingerprint_diffs(attr(design, "frame_info"), frames)
  if (length(diffs) == 0) {
    return(invisible(NULL))
  }

  # The header commits to no count on the recorded side: the difference may be
  # that there is a different number of them, which the bullet states.
  msg <- c(
    "{cli::qty(length(frames))}Frame{?s} differ{?s/} from what was recorded
     when the design was saved:",
    setNames(diffs, rep("*", length(diffs))),
    "i" = "This is informational. The design remains executable on any
           {cli::qty(length(frames))}frame{?s} that pass{?es/} the variable
           checks."
  )
  if (identical(fingerprint, "warn")) {
    cli_warn(msg)
  } else {
    cli::cli_inform(msg)
  }
  invisible(NULL)
}

#' The fingerprints a design file recorded, always as a list
#'
#' One frame is stored under `fingerprint`, several under `fingerprints`.
#' Reading either with `$` is a trap: the singular name is a prefix of the
#' plural one, so a file that recorded several answers a request for one with
#' all of them, and the comparison then reports every column of the first
#' frame as new. Extraction is exact, and callers see one shape.
#'
#' @return A list of fingerprints, or `NULL` when the file recorded none.
#' @noRd
recorded_fingerprints <- function(frame_info) {
  if (is_null(frame_info)) {
    return(NULL)
  }
  plural <- frame_info[["fingerprints"]]
  if (!is_null(plural)) {
    return(plural)
  }
  singular <- frame_info[["fingerprint"]]
  if (!is_null(singular)) {
    return(list(singular))
  }
  NULL
}

#' Compare supplied frames against what a design file recorded
#'
#' The single comparator behind validation and replay. A count that does not
#' match is itself the difference: a file recording three registers cannot say
#' anything about one frame, and silently skipping the comparison would let a
#' replay certify a sample it never checked.
#'
#' @return Character vector of differences. It is empty when everything
#'   matches.
#' @noRd
fingerprint_diffs <- function(frame_info, frames) {
  recorded <- recorded_fingerprints(frame_info)
  if (is_null(recorded)) {
    return(character(0))
  }
  if (length(recorded) != length(frames)) {
    return(cli::format_inline(
      "{length(frames)} frame{?s} supplied; {length(recorded)} recorded"
    ))
  }

  diffs <- character(0)
  for (i in seq_along(frames)) {
    one <- fingerprint_differences(recorded[[i]], frames[[i]])
    if (length(one) == 0) {
      next
    }
    # A single frame needs no position: there is only one thing it can be.
    diffs <- c(diffs, if (length(recorded) == 1L) {
      one
    } else {
      paste0(frame_token(i, recorded[[i]][["name"]]), ": ", one)
    })
  }
  diffs
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
