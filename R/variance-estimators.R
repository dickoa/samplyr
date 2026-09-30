#' Which variance estimators a design supports
#'
#' Reports, before anything is drawn, how each variance estimator the export
#' functions offer would treat a sample of this design: supported, supported
#' only approximately, or refused, with the condition class the export would
#' raise. Most of the answer is fixed by the design, some by the frame, and a
#' few conditions can only be settled by the sample itself. The report keeps
#' those apart, so a refusal can be designed around before fieldwork rather
#' than discovered at export.
#'
#' @details
#' Each row is one estimator and the call that requests it:
#'
#' - Taylor linearization, [as_svydesign()].
#' - Linearization with joint inclusion probabilities, [as_svydesign()] with
#'   a matrix from [joint_expectation()] passed as `pps`.
#' - The replicate types of [as_svrepdesign()]: the jackknife (`"JK1"`,
#'   `"JKn"`), balanced repeated replication (`"BRR"`, `"Fay"`), the bootstrap
#'   (`"bootstrap"`), the rescaled bootstrap (`"subbootstrap"`), the
#'   multistage rescaled bootstrap (`"mrbbootstrap"`), the
#'   Rao-Wu-Yue-Beaumont bootstrap (`"rwyb"`) and random groups
#'   (`"random_groups"`).
#'
#' `status` is `"supported"` when the export carries the design with no
#' approximation of its own, `"approximate"` when it knowingly approximates
#' part of it (Brewer's approximation for a PPS stage, a systematic stage,
#' a first stage treated as drawn with replacement), `"refused"` when the
#' export stops, and `"not applicable"` for the joint-probability route when
#' stage 1 has no unequal probabilities to supply. `decided_by` says what
#' settles it: `"design"` for the methods and the stage and phase structure,
#' `"frame"` for what the frame fixes before any draw, such as a stratum
#' that takes one unit or a stage that takes every unit. `class` and `note`
#' give the condition the export raises and why: for a refusal, the one the
#' export meets first, and for an approximation, every one that applies.
#' `phase` and `stage` locate the first of them.
#'
#' `sample_risk` lists what only the realized sample decides and what it
#' would do to the estimator: a primary unit left with no rows, a stratum left
#' with one sampled unit at a random-size or later stage. What the frame shows
#' about the risk is part of the text, such as how many candidate units have
#' nothing below them.
#'
#' Without `frame`, the frame checks are not run. A stratum that may take one
#' unit, the pairing BRR needs, several rows per cluster and candidate units
#' with nothing below them are then listed in `sample_risk`, and a stage that
#' takes every unit is not recognized. With a frame the design is resolved
#' against it as [frame_summary()] does, without drawing and without using
#' random numbers.
#'
#' To assess a second phase, give the phase-1 sample as `frame`: the report
#' then covers the two-phase export. A `tbl_sample` passed as `x` stands for
#' its design, and when it is itself a later phase, its earlier phase is used.
#' Materialized panel waves are not assessed.
#'
#' The report reads the same rules the exports apply, and a test compares it
#' with the conditions the exports actually raise on executed samples. It
#' does not certify an estimator's accuracy beyond that:
#' `?variance-estimation` gives the measured error of each approximation.
#'
#' @param x A `sampling_design`, or a `tbl_sample` standing for its design.
#' @param frame Optional. The frame the design will be executed on, as
#'   [execute()] takes it: a data frame, or a list of stage registers. A
#'   `tbl_sample` is the phase-1 sample of a second phase.
#'
#' @return A tibble with one row per estimator and the columns `estimator`,
#'   `call`, `status`, `decided_by`, `phase`, `stage`, `class`, `note` and
#'   `sample_risk`.
#'
#' @examples
#' design <- sampling_design() |>
#'   add_stage(label = "EAs") |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 4, method = "pps_brewer", mos = households) |>
#'   add_stage(label = "Households") |>
#'     draw(n = 12)
#'
#' # From the design alone
#' variance_estimators(design)[c("estimator", "status", "class")]
#'
#' # Against the frame, which settles the checks the frame decides
#' estimators <- variance_estimators(design, bfa_eas)
#' estimators[estimators$status == "refused", c("estimator", "class")]
#'
#' # A second phase, assessed against its phase-1 sample
#' phase1 <- sampling_design() |>
#'   stratify_by(region) |>
#'   cluster_by(ea_id) |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 7)
#' phase2 <- sampling_design() |> draw(n = 60, method = "pps_sampford",
#'                                      mos = households)
#' variance_estimators(phase2, phase1)[c("estimator", "status", "note")]
#'
#' @seealso `?variance-estimation` for how each selection method's variance
#'   is estimated, [frame_summary()] and [validate_frame()] for the checks
#'   before execution.
#' @family survey export
#' @export
variance_estimators <- function(x, frame = NULL) {
  if (is_tbl_sample(x)) {
    design <- get_design(x)
    phase1 <- if (is_null(frame)) attr(x, "metadata")$prev_phase$sample
  } else if (is_sampling_design(x)) {
    design <- x
    phase1 <- NULL
  } else {
    abort_samplyr(
      "{.arg x} must be a {.cls sampling_design} or a {.cls tbl_sample}.",
      class = "samplyr_error_design_expected"
    )
  }
  validate_design_complete(design)
  if (is_tbl_sample(frame)) {
    phase1 <- frame
    frame <- NULL
  }
  stages <- seq_along(design$stages)
  traits <- stage_variance_traits(design, stages)
  facts <- frame_variance_facts(design, frame, traits, call = current_env())

  rows <- if (is_null(phase1)) {
    singlephase_estimator_rows(design, traits, facts)
  } else {
    twophase_estimator_rows(design, traits, phase1)
  }
  tibble::tibble(
    estimator = vapply(rows, `[[`, "", "estimator"),
    call = vapply(rows, `[[`, "", "call"),
    status = vapply(rows, `[[`, "", "status"),
    decided_by = vapply(rows, `[[`, "", "decided_by"),
    phase = vapply(rows, `[[`, 1L, "phase"),
    stage = vapply(rows, `[[`, 1L, "stage"),
    class = vapply(rows, `[[`, "", "class"),
    note = vapply(rows, `[[`, "", "note"),
    sample_risk = vapply(rows, `[[`, "", "sample_risk")
  )
}

## What each stage is, from the design

#' The design-level variance description of each stage
#'
#' The same kind and unequal-probability rule the export reads through
#' `export_stage_spec()`, without the rows.
#' @noRd
stage_variance_traits <- function(design, stages) {
  n <- length(stages)
  lapply(seq_len(n), function(pos) {
    idx <- stages[pos]
    stage <- design$stages[[idx]]
    draw <- stage$draw_spec
    kind <- survey_stage_kind(draw)
    list(
      stage = idx,
      name = stage_token(design, idx),
      method = draw$method,
      kind = kind,
      unequal = stage_is_unequal(draw, kind),
      systematic = draw$method %in% c("systematic", "pps_systematic"),
      clustered = !is_null(stage$clusters),
      stratified = !is_null(stage$strata),
      midstage_element = is_null(stage$clusters) &&
        !is_multi_hit_method(draw) && pos < n,
      random_size = is_random_size_method(draw),
      draw = draw
    )
  })
}

## What the frame fixes before any draw

#' Pool sizes and takes of each stage, resolved against the frame
#'
#' `NULL` without a frame, or when the design cannot be resolved ex ante (a
#' stage below a with-replacement stage), in which case every frame check is
#' left to the sample.
#' @noRd
frame_variance_facts <- function(design, frame, traits, call = caller_env()) {
  if (is_null(frame)) {
    return(NULL)
  }
  digest <- tryCatch(
    exante_digest(design, frame, call = call),
    samplyr_error_exante_unsupported = function(e) NULL,
    samplyr_exante_unresolvable = function(e) NULL
  )
  if (is_null(digest)) {
    return(NULL)
  }
  facts <- lapply(seq_along(digest$stages), function(pos) {
    record <- digest$stages[[pos]]
    trait <- traits[[pos]]
    pools <- record$pools
    units <- record$units
    n_cert <- rep(0, nrow(pools))
    if (!is_null(units) && "is_certainty" %in% names(units) &&
        identical(trait$kind, "pps_wor")) {
      n_cert <- as.numeric(tapply(
        units$is_certainty,
        factor(units$pool_id, levels = pools$pool_id),
        sum
      ))
      n_cert[is.na(n_cert)] <- 0
    }
    fixed <- !trait$random_size
    take <- pools$n_target - n_cert
    list(
      stage = trait$stage,
      n_pools = nrow(pools),
      n_units = sum(pools$N),
      n_certainty = sum(n_cert),
      fixed = fixed,
      singleton = if (fixed) take == 1 else rep(FALSE, nrow(pools)),
      takes = take,
      census = fixed && nrow(pools) > 0 && all(pools$n_target >= pools$N),
      parents = unique(pools$parent_unit),
      rows_per_unit = units$n_descendants
    )
  })
  for (pos in seq_along(facts)[-1]) {
    facts[[pos]]$empty_parents <- facts[[pos - 1L]]$n_units -
      length(facts[[pos]]$parents)
  }
  facts
}

## Rows

#' @noRd
estimator_catalogue <- function() {
  list(
    list(key = "linearization", estimator = "Taylor linearization",
         call = "as_svydesign(x)"),
    list(key = "joint", estimator = "Linearization, joint probabilities",
         call = paste0("as_svydesign(x, pps = survey::ppsmat(",
                       "joint_expectation(x)[[1]]))")),
    list(key = "JK1", estimator = "Jackknife (JK1)",
         call = "as_svrepdesign(x, type = \"JK1\")"),
    list(key = "JKn", estimator = "Stratified jackknife (JKn)",
         call = "as_svrepdesign(x, type = \"JKn\")"),
    list(key = "BRR", estimator = "Balanced repeated replication",
         call = "as_svrepdesign(x, type = \"BRR\")"),
    list(key = "Fay", estimator = "Fay's balanced repeated replication",
         call = "as_svrepdesign(x, type = \"Fay\")"),
    list(key = "bootstrap", estimator = "Bootstrap",
         call = "as_svrepdesign(x, type = \"bootstrap\")"),
    list(key = "subbootstrap", estimator = "Rescaled bootstrap",
         call = "as_svrepdesign(x, type = \"subbootstrap\")"),
    list(key = "mrbbootstrap", estimator = "Multistage rescaled bootstrap",
         call = "as_svrepdesign(x, type = \"mrbbootstrap\")"),
    list(key = "rwyb", estimator = "Rao-Wu-Yue-Beaumont bootstrap",
         call = "as_svrepdesign(x, type = \"rwyb\")"),
    list(key = "random_groups", estimator = "Random groups",
         call = "as_svrepdesign(x, type = \"random_groups\")")
  )
}

#' One finding about an estimator
#'
#' `effect` is "refused", "approximate" or "risk". A refusal is listed in the
#' order the export would meet it, so the first one is the one it raises.
#' @noRd
finding <- function(effect, class = NA_character_, note, stage = NA_integer_,
                    phase = NA_integer_, decided_by = "design") {
  note <- gsub("[[:space:]]+", " ", note)
  list(effect = effect, class = class, note = note, stage = as.integer(stage),
       phase = as.integer(phase), decided_by = decided_by)
}

#' Collapse the findings about one estimator into its row
#' @noRd
estimator_row <- function(entry, findings, not_applicable = NULL) {
  row <- list(
    estimator = entry$estimator, call = entry$call, status = "supported",
    decided_by = "design", phase = NA_integer_, stage = NA_integer_,
    class = NA_character_, note = NA_character_, sample_risk = NA_character_
  )
  risks <- Filter(function(f) identical(f$effect, "risk"), findings)
  if (length(risks) > 0) {
    row$sample_risk <- paste(vapply(risks, function(f) {
      if (is.na(f$class)) f$note else paste0(f$note, " (", f$class, ")")
    }, ""), collapse = ". ")
  }
  if (!is_null(not_applicable)) {
    row$status <- "not applicable"
    row$note <- not_applicable
    return(row)
  }
  refused <- Filter(function(f) identical(f$effect, "refused"), findings)
  approx <- Filter(function(f) identical(f$effect, "approximate"), findings)
  deciding <- if (length(refused) > 0) refused[1] else approx
  if (length(deciding) == 0) {
    return(row)
  }
  row$status <- if (length(refused) > 0) "refused" else "approximate"
  first <- deciding[[1]]
  row$decided_by <- if (any(vapply(deciding, function(f) {
    identical(f$decided_by, "frame")
  }, logical(1)))) "frame" else "design"
  row$phase <- first$phase
  row$stage <- first$stage
  classes <- unique(stats::na.omit(vapply(deciding, `[[`, "", "class")))
  row$class <- if (length(classes) > 0) paste(classes, collapse = ", ") else
    NA_character_
  row$note <- paste(vapply(deciding, `[[`, "", "note"), collapse = ". ")
  row
}

## One phase

#' @noRd
singlephase_estimator_rows <- function(design, traits, facts) {
  common <- common_stage_findings(traits, facts)
  lapply(estimator_catalogue(), function(entry) {
    switch(
      entry$key,
      linearization = estimator_row(
        entry,
        c(common$refusals, linearization_findings(traits, facts),
          common$systematic, brewer_findings(traits), common$risks,
          lonely_findings(traits, facts))
      ),
      joint = joint_estimator_row(entry, traits, facts, common),
      rwyb = estimator_row(entry, rwyb_findings(traits, facts, common)),
      random_groups = estimator_row(entry, list(finding(
        "risk",
        note = "Needs the whole design executed with `reps = R`, R >= 2, and
          estimates the variance of the estimate pooled over the replicates"
      ))),
      estimator_row(
        entry, generic_replicate_findings(entry$key, traits, facts, common)
      )
    )
  })
}

#' Findings every single-phase estimator shares
#' @noRd
common_stage_findings <- function(traits, facts) {
  refusals <- list()
  midstage <- Filter(function(t) t$midstage_element, traits)
  if (length(midstage) > 0) {
    refusals <- list(finding(
      "refused", "samplyr_error_survey_midstage_element",
      paste0("Stage ", midstage[[1]]$stage, " selects elements and has stages",
             " below it"),
      stage = midstage[[1]]$stage
    ))
  }
  systematic <- list()
  for (t in Filter(function(t) t$systematic, traits)) {
    fact <- stage_fact(facts, t$stage)
    if (isTRUE(fact$census)) {
      next
    }
    systematic <- c(systematic, list(finding(
      "approximate", "samplyr_warning_systematic_variance",
      paste0("A systematic ", t$name, " has no design-unbiased variance",
             " estimator. Its variance is approximated",
             if (is_null(fact)) {
               ", unless the stage takes every unit, which the frame shows"
             }),
      stage = t$stage,
      decided_by = if (is_null(fact)) "design" else "frame"
    )))
  }
  list(
    refusals = refusals,
    systematic = systematic,
    risks = empty_unit_findings(traits, facts)
  )
}

#' @noRd
stage_fact <- function(facts, stage) {
  if (is_null(facts)) {
    return(NULL)
  }
  Find(function(f) identical(f$stage, stage), facts)
}

#' Primary units, and deeper parents, the frame leaves with nothing below
#' @noRd
empty_unit_findings <- function(traits, facts) {
  if (length(traits) < 2L) {
    return(list())
  }
  if (is_null(facts)) {
    return(list(finding(
      "risk", "samplyr_error_export_empty_psu",
      "A selected primary unit with no rows below it is refused. Pass `frame`
       to see whether any candidate has none"
    )))
  }
  out <- list()
  for (fact in facts[-1]) {
    if (fact$empty_parents <= 0) {
      next
    }
    first_level <- identical(fact$stage, 2L)
    out <- c(out, list(finding(
      "risk",
      if (first_level) "samplyr_error_export_empty_psu" else
        "samplyr_warning_export_empty_parent",
      paste0(fact$empty_parents, " candidate unit",
             if (fact$empty_parents > 1) "s have" else " has",
             " nothing to sample at stage ", fact$stage,
             if (first_level) ", and a selected one is refused" else
               ", and a selected one warns")
    )))
  }
  out
}

#' Linearization refusals, in the order the export meets them
#' @noRd
linearization_findings <- function(traits, facts) {
  out <- list()
  first <- traits[[1]]
  later_poisson <- Filter(function(t) identical(t$kind, "rs_poisson"),
                          traits[-1])
  if (length(later_poisson) > 0) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_multistage_poisson_later",
      paste0("Poisson sampling at ", later_poisson[[1]]$name, " has no",
             " linearization export. RWYB keeps its random-size variance"),
      stage = later_poisson[[1]]$stage
    )))
  }
  unsupported <- Filter(function(t) identical(t$kind, "unsupported"), traits)
  if (length(unsupported) > 0) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_custom_random_wor_export",
      paste0(unsupported[[1]]$method, " at ", unsupported[[1]]$name,
             " has no linearization variance estimator"),
      stage = unsupported[[1]]$stage
    )))
  }
  if (identical(first$kind, "rs_poisson")) {
    if (length(traits) > 1L) {
      out <- c(out, list(finding(
        "refused", "samplyr_error_multistage_poisson_stage1",
        "survey has no multistage design with a Poisson first stage",
        stage = 1L
      )))
    } else {
      out <- c(out, poisson_cluster_findings(first, facts))
      declared <- identical(first$draw$method_variance, "poisson")
      if (!first$method %in% rs_poisson_methods && !declared) {
        out <- c(out, list(finding(
          "refused", "samplyr_error_custom_random_wor_export",
          paste0("The custom random-size method ", first$method, " is not",
                 " declared as Poisson sampling"),
          stage = 1L
        )))
      }
    }
  }
  out
}

#' A single-stage clustered Poisson sample with several rows per cluster
#' @noRd
poisson_cluster_findings <- function(first, facts) {
  if (!first$clustered) {
    return(list())
  }
  rows <- stage_fact(facts, first$stage)$rows_per_unit
  note <- "A sampled cluster with more than one row is refused, since the
    Poisson estimator treats rows as independent"
  if (is_null(rows)) {
    return(list(finding("risk", "samplyr_error_cluster_poisson_export",
                        note)))
  }
  if (all(rows > 1)) {
    return(list(finding("refused", "samplyr_error_cluster_poisson_export",
                        "Every cluster has more than one row, and the Poisson
                         estimator treats rows as independent",
                        stage = 1L, decided_by = "frame")))
  }
  if (any(rows > 1)) {
    return(list(finding("risk", "samplyr_error_cluster_poisson_export",
                        paste0(sum(rows > 1), " clusters have more than one",
                               " row, and selecting one is refused"))))
  }
  list()
}

#' The approximations linearization makes without a warning
#' @noRd
brewer_findings <- function(traits) {
  out <- list()
  for (t in traits) {
    note <- if (identical(t$method, "cube")) {
      paste0("The balanced ", t$name, " is linearized with the high-entropy",
             " approximation, which ignores the balancing")
    } else if (identical(t$method, "pps_chromy")) {
      paste0("Chromy's ", t$name, " is treated as drawn with replacement,",
             " which can be strongly conservative")
    } else if (identical(t$kind, "pps_wor") && !t$systematic) {
      paste0("Brewer's approximation at ", t$name)
    }
    if (!is_null(note)) {
      out <- c(out, list(finding("approximate", note = note, stage = t$stage)))
    }
  }
  out
}

#' Strata left with one sampled unit, which survey stops on by default
#' @noRd
lonely_findings <- function(traits, facts) {
  if (!identical(getOption("survey.lonely.psu", "fail"), "fail")) {
    return(list())
  }
  singleton_findings(
    traits, facts,
    class = "samplyr_warning_lonely_psu",
    effect = "approximate",
    advice = "Set `options(survey.lonely.psu = \"adjust\")` or collapse the
      strata"
  )
}

#' Pools the frame fixes at one non-certainty unit, stage by stage
#'
#' Certain at stage 1, where every pool is drawn from, and at a later stage
#' whose every pool takes one. Otherwise a risk: whether it happens depends
#' on which parents are selected, or on a random sample size.
#' @noRd
singleton_findings <- function(traits, facts, class, effect, advice,
                               skip = function(t) FALSE) {
  if (is_null(facts)) {
    if (!any(!vapply(traits, skip, logical(1)))) {
      return(list())
    }
    return(list(finding(
      "risk", class,
      "A stratum left with one sampled unit outside certainty. Pass `frame`
       to see whether the allocation fixes one"
    )))
  }
  out <- list()
  for (t in traits) {
    if (skip(t)) {
      next
    }
    fact <- stage_fact(facts, t$stage)
    if (is_null(fact)) {
      next
    }
    n_single <- sum(fact$singleton)
    if (n_single > 0 &&
        (identical(t$stage, 1L) || n_single == fact$n_pools)) {
      out <- c(out, list(finding(
        effect, class,
        paste0(n_single, " strat", if (n_single > 1) "a" else "um",
               " at ", t$name, " take", if (n_single > 1) "" else "s",
               " a single unit outside certainty. ", advice),
        stage = t$stage, decided_by = "frame"
      )))
    } else if (n_single > 0 || (!fact$fixed && fact$n_pools > 0)) {
      out <- c(out, list(finding(
        "risk", class,
        paste0("A stratum at ", t$name, " may be left with one sampled unit")
      )))
    }
  }
  out
}

#' The joint-probability route, single phase
#' @noRd
joint_estimator_row <- function(entry, traits, facts, common) {
  first <- traits[[1]]
  pps_first <- identical(first$kind, "pps_wor") ||
    (identical(first$kind, "rs_poisson") && !is_null(first$draw$mos))
  if (!pps_first) {
    return(estimator_row(entry, list(), not_applicable = paste0(
      "Stage 1 is not drawn with unequal probabilities, so there is no joint",
      " matrix to supply"
    )))
  }
  out <- list()
  if (!is_null(first$draw$bounds) || !is_null(first$draw$spread)) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_joint_method_unsupported",
      paste0("No joint inclusion probabilities for ", first$method,
             " with its declared constraints"),
      stage = 1L
    )))
  }
  out <- c(out, joint_rows_per_unit_findings(traits, facts))
  if (length(traits) > 1L) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_pps_single_stage",
      "The matrix applies to stage 1, so the export represents stage 1 only",
      stage = 1L
    )))
  }
  quality <- if (first$method %in% c("pps_cps", "pps_sampford",
                                      "pps_poisson")) {
    NULL
  } else if (identical(first$method, "pps_systematic")) {
    finding("approximate", "samplyr_warning_systematic_ppsmat",
            "Systematic PPS leaves pairs with zero joint probability",
            stage = 1L)
  } else {
    finding("approximate",
            note = paste0("The joint probabilities of ", first$method,
                          " are the high-entropy approximation"),
            stage = 1L)
  }
  # A matrix replaces stage 1, and the stages below it are not exported.
  out <- c(out, if (!is_null(quality)) list(quality),
           Filter(function(f) !identical(f$stage, 1L), common$systematic),
           common$risks, lonely_findings(traits[1], facts))
  estimator_row(entry, out)
}

#' survey indexes a stage matrix by row, so it needs one row per stage-1 unit
#' @noRd
joint_rows_per_unit_findings <- function(traits, facts) {
  if (!traits[[1]]$clustered) {
    return(list())
  }
  note <- "survey reads the matrix by row, and a stage-1 unit with more than
    one row in the sample is refused"
  if (length(traits) == 1L) {
    rows <- stage_fact(facts, traits[[1]]$stage)$rows_per_unit
    if (is_null(rows) || any(rows > 1) && !all(rows > 1)) {
      return(list(finding("risk", "samplyr_error_pps_rows_per_psu", note)))
    }
    if (all(rows > 1)) {
      return(list(finding("refused", "samplyr_error_pps_rows_per_psu", note,
                          stage = 1L, decided_by = "frame")))
    }
    return(list())
  }
  if (is_null(facts)) {
    below_one <- all(vapply(traits[-1], function(t) {
      n <- t$draw$n
      is.numeric(n) && length(n) == 1L && n == 1 && is_null(t$draw$frac)
    }, logical(1)))
    if (below_one) {
      return(list())
    }
    return(list(finding("refused", "samplyr_error_pps_rows_per_psu", note,
                        stage = 1L)))
  }
  takes <- unlist(lapply(facts[-1], `[[`, "takes"))
  if (length(takes) > 0 && any(takes > 1)) {
    return(list(finding("refused", "samplyr_error_pps_rows_per_psu", note,
                        stage = 1L, decided_by = "frame")))
  }
  list()
}

#' The generic replicate types
#' @noRd
generic_replicate_findings <- function(type, traits, facts, common) {
  out <- list()
  poisson <- Filter(function(t) identical(t$kind, "rs_poisson"), traits)
  if (length(poisson) > 0) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_poisson_replicates",
      "Generic replicates do not represent a Poisson sample-size variance",
      stage = poisson[[1]]$stage
    )))
  }
  pps_safe <- type %in% c("subbootstrap", "mrbbootstrap")
  first <- traits[[1]]
  if (stage_replicated_as_wr(first$draw, first$kind) && !pps_safe) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_replicate_wr_first_stage",
      paste0("The first stage, drawn with ", first$method, ", is treated as",
             " drawn with replacement, which errs toward too large a",
             " variance"),
      stage = first$stage
    )))
  }
  out <- c(out, common$systematic, common$refusals)
  unsupported <- Filter(function(t) identical(t$kind, "unsupported"), traits)
  if (length(unsupported) > 0) {
    out <- c(out, list(if (pps_safe) {
      finding("approximate",
              note = paste0("The bootstrap does not recreate the constraints",
                            " of ", unsupported[[1]]$method),
              stage = unsupported[[1]]$stage)
    } else {
      finding("refused", "samplyr_error_custom_random_wor_export",
              paste0(unsupported[[1]]$method, " at ", unsupported[[1]]$name,
                     " has no variance estimator of this type"),
              stage = unsupported[[1]]$stage)
    }))
  }
  stratified <- replicate_first_stage_stratified(traits)
  if (identical(type, "JK1") && stratified) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_svrep_conversion_failed",
      "survey's JK1 takes an unstratified first stage, and this one is
       stratified (a PPS first stage is exported with its own strata)",
      stage = traits[[1]]$stage
    )))
  }
  if (type %in% c("JKn", "BRR", "Fay") && !stratified) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_svrep_conversion_failed",
      "survey needs a stratified first stage for this type. JK1 or the
       bootstrap take an unstratified one",
      stage = traits[[1]]$stage
    )))
  }
  if (identical(type, "JKn") && isTRUE(stage_fact(facts, 1L)$census)) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_svrep_conversion_failed",
      "survey's JKn stops when every first-stage stratum is taken whole, since
       the first stage then has no sampling variance",
      stage = 1L, decided_by = "frame"
    )))
  }
  if (type %in% c("JKn", "BRR", "Fay", "subbootstrap")) {
    out <- c(out, singleton_findings(
      traits[1], facts,
      class = "samplyr_error_svrep_conversion_failed",
      effect = "refused",
      advice = "survey cannot form replicates from it"
    ))
  }
  if (type %in% c("BRR", "Fay")) {
    out <- c(out, brr_findings(traits, facts))
  }
  c(out, common$risks)
}

#' Is the first stage the replicate types resample stratified?
#'
#' A PPS first stage is converted on its own, and that conversion always
#' passes survey a strata term.
#' @noRd
replicate_first_stage_stratified <- function(traits) {
  first <- traits[[1]]
  identical(first$kind, "pps_wor") || first$stratified
}

#' Balanced half-samples pair the units of each first-stage stratum
#' @noRd
brr_findings <- function(traits, facts) {
  fact <- stage_fact(facts, traits[[1]]$stage)
  note <- "Balanced half-samples need an even number of sampled units, at
    least two, in every first-stage stratum"
  if (is_null(fact) || !fact$fixed ||
      (fact$n_certainty > 0 && length(traits) > 1L)) {
    return(list(finding("risk", "samplyr_error_svrep_conversion_failed",
                        note)))
  }
  paired <- fact$takes == 0 | (fact$takes >= 2 & fact$takes %% 2 == 0)
  if (!all(paired)) {
    return(list(finding("refused", "samplyr_error_svrep_conversion_failed",
                        note, stage = traits[[1]]$stage,
                        decided_by = "frame")))
  }
  list()
}

#' The Rao-Wu-Yue-Beaumont bootstrap
#' @noRd
rwyb_findings <- function(traits, facts, common) {
  out <- list()
  methods <- character(0)
  for (t in traits) {
    refusal <- tryCatch(
      {
        methods <- c(methods, rwyb_stage_method(t$draw))
        NULL
      },
      samplyr_error_rwyb_method = function(e) e
    )
    if (!is_null(refusal)) {
      out <- c(out, list(finding(
        "refused", "samplyr_error_rwyb_method",
        paste0("RWYB has no mapping for ", t$method, " at ", t$name),
        stage = t$stage
      )))
      return(out)
    }
  }
  out <- c(out, common$systematic)
  pps <- Filter(function(t) identical(t$kind, "pps_wor"), traits)
  if (length(pps) > 0) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_rwyb_pps_approximation",
      "RWYB approximates the joint probabilities of unequal-probability
       sampling without replacement",
      stage = pps[[1]]$stage
    )))
  }
  out <- c(out, common$refusals)
  missing <- lapply(common$risks, function(f) {
    f$class <- "samplyr_error_rwyb_missing_parents"
    f$note <- sub(", and a selected one (is refused|warns)$",
                  ", and a selected one is refused", f$note)
    f
  })
  c(out, missing, singleton_findings(
    traits, facts,
    class = "samplyr_error_rwyb_singleton",
    effect = "refused",
    advice = "At the final stage, `lonely.psu = \"certainty\"` treats it as
      taken with certainty",
    skip = function(t) identical(t$kind, "rs_poisson")
  ))
}

## Two phases

#' @noRd
twophase_estimator_rows <- function(design, traits, phase1) {
  meta1 <- attr(phase1, "metadata")
  design1 <- get_design(phase1)
  stages1 <- get_stages_executed(phase1)
  df1 <- as.data.frame(phase1)
  lapply(estimator_catalogue(), function(entry) {
    if (!is_null(meta1$prev_phase)) {
      return(estimator_row(entry, list(finding(
        "refused", "samplyr_error_survey_multiphase_unsupported",
        "The phase-1 sample is itself a later phase, and survey exports two
         phases at most",
        decided_by = "frame"
      ))))
    }
    switch(
      entry$key,
      linearization = estimator_row(entry, twophase_linearization_findings(
        design, traits, phase1, design1, stages1, df1
      )),
      joint = estimator_row(entry, twophase_linearization_findings(
        design, traits, phase1, design1, stages1, df1, user_pps = TRUE
      )),
      random_groups = estimator_row(entry, if (
        has_multiple_replicates(phase1) && isTRUE(meta1$replicates_complete)
      ) {
        list()
      } else {
        list(finding(
          "refused", "samplyr_error_random_groups_shared",
          "Replicates drawn from one realized phase 1 share its selection, so
           random groups would leave its variance out",
          decided_by = "frame"
        ))
      }),
      estimator_row(entry, list(finding(
        "refused", "samplyr_error_svrep_twophase_unsupported",
        "No replicate export exists for a two-phase sample",
        decided_by = "design"
      )))
    )
  })
}

#' The two-phase linearization, in the order the export meets each condition
#' @noRd
twophase_linearization_findings <- function(design, traits, phase1, design1,
                                            stages1, df1, user_pps = FALSE) {
  prev <- list(design = design1, stages = stages1, sample = phase1)
  traits1 <- stage_variance_traits(design1, stages1)
  out <- list()
  systematic1 <- systematic_approximated_stages(design1, stages1, df1,
                                                phase = 1L)
  for (s in systematic1) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_systematic_variance",
      paste0("A systematic ", s$name, " of phase 1 has its variance",
             " approximated"),
      stage = s$stage, phase = 1L, decided_by = "frame"
    )))
  }
  for (t in Filter(function(t) t$systematic, traits)) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_systematic_variance",
      paste0("A systematic ", t$name, " of phase 2 has its variance",
             " approximated"),
      stage = t$stage, phase = 2L
    )))
  }
  if (!sample_realization_status(phase1)$ok) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_modified_sample",
      "The phase-1 sample was modified, and its current rows are read as the
       whole phase 1",
      phase = 1L, decided_by = "frame"
    )))
  }
  refuse <- function(class, note, phase = NA_integer_, stage = NA_integer_,
                     decided_by = "design") {
    list(finding("refused", class, note, stage = stage, phase = phase,
                 decided_by = decided_by))
  }
  unsupported1 <- phase1_pps_methods(prev, kinds = "unsupported")
  if (length(unsupported1) > 0) {
    return(c(out, refuse(
      "samplyr_error_custom_random_wor_export",
      paste0("Phase 1 was drawn with ", unsupported1[1], ", which has no",
             " linearization variance estimator"),
      phase = 1L, decided_by = "frame"
    )))
  }
  pps1 <- phase1_pps_methods(prev)
  if (length(pps1) > 0) {
    return(c(out, refuse(
      "samplyr_error_twophase_phase1_pps",
      paste0("survey::twophase() takes no pps specification at phase 1,",
             " which was drawn with ", pps1[1], ". The ultimate-cluster",
             " approximation in ?as_svydesign is conservative"),
      phase = 1L, decided_by = "frame"
    )))
  }
  if (!is_null(vanished_primary_units(phase1, design1, stages1))) {
    return(c(out, refuse(
      "samplyr_error_export_empty_psu",
      "A selected phase-1 primary unit has no rows",
      phase = 1L, decided_by = "frame"
    )))
  }
  poisson <- Filter(function(t) identical(t$kind, "rs_poisson"),
                    c(traits1, traits))
  if (length(poisson) > 0) {
    return(c(out, refuse(
      "samplyr_error_twophase_poisson",
      "Two-phase export does not carry Poisson sampling in either phase",
      decided_by = if (any(vapply(traits1, function(t) {
        identical(t$kind, "rs_poisson")
      }, logical(1)))) "frame" else "design"
    )))
  }
  bridge <- twophase_bridge_finding(design, design1, stages1, phase1)
  if (!is_null(bridge)) {
    return(c(out, list(bridge)))
  }
  midstage <- Filter(function(t) t$midstage_element, c(traits1, traits))
  if (length(midstage) > 0) {
    return(c(out, refuse(
      "samplyr_error_survey_midstage_element",
      paste0("Stage ", midstage[[1]]$stage, " selects elements and has",
             " stages below it"),
      stage = midstage[[1]]$stage
    )))
  }
  if (user_pps) {
    return(c(out, refuse(
      "samplyr_error_twophase_phase2_pps",
      "A two-phase export takes no `pps`. It supplies the phase-2 joint
       probabilities itself where it can, see Taylor linearization"
    )))
  }
  spec2 <- list(stage = traits)
  joint <- tryCatch(
    check_twophase_phase2_family(spec2, NULL, NULL),
    samplyr_error_twophase_phase2_pps = function(e) e
  )
  if (inherits(joint, "condition")) {
    methods <- unique(vapply(Filter(function(t) t$unequal, traits),
                             function(t) t$method, ""))
    return(c(out, refuse(
      "samplyr_error_twophase_phase2_pps",
      paste0("No two-phase variance for ", paste(methods, collapse = ", "),
             " at phase 2. A single-stage phase 2 drawn with ",
             paste(twophase_joint_methods, collapse = ", "),
             " is exported with its joint probabilities"),
      phase = 2L
    )))
  }
  probs <- twophase_stage_probs_finding(traits, design1, stages1, df1,
                                         joint)
  if (!is_null(probs)) {
    return(c(out, list(probs)))
  }
  spec1 <- export_stage_spec(df1, design1, stages1, phase = 1L)
  if (twophase_across_units(df1, spec1, design, traits[[1]]$stage)) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_twophase_across_units",
      "Phase 2 is drawn across the units phase 1 selected, so its variance is
       unstable. Draw it within each phase-1 unit",
      phase = 2L, decided_by = "frame"
    )))
  }
  if (isTRUE(joint) &&
      !traits[[1]]$method %in% c("pps_sampford", "pps_cps")) {
    out <- c(out, list(finding(
      "approximate",
      note = paste0("The phase-2 joint probabilities of ", traits[[1]]$method,
                    " are the high-entropy approximation"),
      stage = traits[[1]]$stage, phase = 2L
    )))
  }
  out
}

#' Can phase 2 be joined back to phase 1?
#'
#' The same compound identifier [validate_frame()] checks: every identifier
#' either phase declares that the phase-1 sample carries.
#' @noRd
twophase_bridge_finding <- function(design, design1, stages1, phase1) {
  phase1_ids <- survey_key_vars(design1, stages1, phase1)
  phase2_ids <- unlist(lapply(design$stages, function(s) s$clusters$vars))
  bridge <- intersect(unique(c(phase1_ids, phase2_ids)), names(phase1))
  note <- if (length(bridge) == 0) {
    "Neither phase declares a unit identifier the phase-1 sample carries, so
     the phases cannot be joined"
  } else if (anyDuplicated(as.data.frame(phase1)[, bridge, drop = FALSE]) > 0) {
    "The phase identifiers do not identify phase-1 rows uniquely"
  }
  if (is_null(note)) {
    return(NULL)
  }
  finding("refused", "samplyr_error_twophase_bridge", note,
          decided_by = "frame")
}

#' The full two-phase covariance needs a probability for each stage
#' @noRd
twophase_stage_probs_finding <- function(traits, design1, stages1, df1,
                                         joint) {
  spec1 <- export_stage_spec(df1, design1, stages1, phase = 1L)
  ids1 <- spec_survey_ids(spec1, df1, synthesize_unclustered = TRUE,
                          prefix = "p1_")
  fpc1 <- survey_fpc_info(ids1$df, design1, stages1, ids1$stage_indices)
  covered1 <- spec_fpc_states_probabilities(spec1, ids1$stage_indices,
                                            fpc1$scale)
  ids2 <- Filter(function(t) t$clustered || is_multi_hit_method(t$draw),
                 traits)
  n_ids <- max(length(ids1$id_vars), length(ids2))
  covered2 <- length(ids2) == length(traits) || length(traits) == 1L
  if (covered2) {
    count_scale <- !any(vapply(traits, function(t) {
      t$kind %in% c("pps_wor") ||
        (identical(t$kind, "rs_poisson") && identical(t$stage, 1L))
    }, logical(1)))
    covered2 <- all(vapply(traits, function(t) {
      !(identical(t$kind, "wr") && count_scale)
    }, logical(1)))
  }
  failed <- if (isTRUE(joint)) {
    length(ids1$id_vars) > 1 && !covered1
  } else {
    n_ids > 1 && !(covered1 && covered2)
  }
  if (!failed) {
    return(NULL)
  }
  finding(
    "refused", "samplyr_error_twophase_stage_probs",
    "The full two-phase covariance needs a probability for each identifier
     stage, and no finite population correction states them. `method =
     \"simple\"` or `\"approx\"` use the weights instead",
    decided_by = "frame"
  )
}
