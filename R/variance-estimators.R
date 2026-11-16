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
#' a first stage treated as drawn with replacement, a first-stage correction
#' that also scales the later stages' variance, stage 2 treated as drawn
#' with replacement inside a PSU taken with certainty), `"refused"` when the
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
#' would do to the estimator: a stratum left with one sampled unit at a
#' random-size or later stage, a selected unit with nothing below it,
#' which is exported as a zero row, or, where a later stage of phase 2
#' drew from a register of its own, a phase-2 row whose phase-1 identifier
#' repeats, such as a unit hit twice, which refuses the join. What the frame
#' shows about the risk is part of the text, such as how many candidate
#' units have nothing below them.
#'
#' Without `frame`, the frame checks are not run. A stratum that may take one
#' unit, the pairing BRR needs, several rows per cluster and candidate units
#' with nothing below them are then listed in `sample_risk`, and a stage that
#' takes every unit is recognized only when its draw says so with
#' `frac = 1`, since a size `n` needs the frame to show it. With a frame the
#' design is resolved
#' against it as [frame_summary()] does, without drawing and without using
#' random numbers.
#'
#' To assess a second phase, give the phase-1 sample as `frame`: the report
#' then covers the two-phase export. A `tbl_sample` passed as `x` stands for
#' its design, and when it is itself a later phase, its earlier phase is used.
#' Materialized occasions and shared-weight samples are refused, as `x` or as
#' `frame`, and so is a design with a reserve, or its sample. Their exports
#' follow rules of their own, so the report of the design they come from
#' does not describe them.
#'
#' The report reads the same rules the exports apply, and a test compares it
#' with the conditions the exports actually raise on executed samples. It
#' does not certify an estimator's accuracy beyond that, and
#' `?variance_estimation` describes each approximation.
#'
#' @param x A `sampling_design`, or a `tbl_sample` standing for its design.
#' @param frame Optional. The frame the design will be executed on, as
#'   [execute()] takes it: a data frame, or a list of stage registers. A
#'   single data frame may cover only the first stages, and the report then
#'   reads the frame for those. A list of registers needs every column
#'   [execute()] would read. A `tbl_sample` is the phase-1 sample of a
#'   second phase.
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
#' @seealso `?variance_estimation` for how each selection method's variance
#'   is estimated, [frame_summary()] and [validate_frame()] for the checks
#'   before execution.
#' @family survey export
#' @export
variance_estimators <- function(x, frame = NULL) {
  if (is_tbl_sample(x)) {
    design <- get_design(x)
    phase1 <- if (is_null(frame)) get_prev_phase(x)$sample
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
  for (s in list(if (is_tbl_sample(x)) x, phase1)) {
    if (!is_null(s)) refuse_unassessed_sample(s)
  }
  for (d in list(design, if (is_tbl_sample(phase1)) get_design(phase1))) {
    if (!is_null(d)) refuse_reserve_design(d)
  }
  stages <- seq_along(design$stages)
  traits <- stage_variance_traits(design, stages)
  facts <- frame_variance_facts(design, frame, traits, call = current_env())

  rows <- if (is_null(phase1)) {
    singlephase_estimator_rows(design, traits, facts)
  } else if (!sample_realization_status(phase1)$ok) {
    modified_phase1_estimator_rows(phase1)
  } else if (!is_null(within_pool_subsample_vars(design, stages, phase1))) {
    # Exported as phase 1's multistage design with a smaller final take.
    # Phase 1's rows are phase 2's frame, so they show a census.
    # The ex-ante walk reads a sample as execute() does, with the row
    # positions and hit numbers the new phase carries.
    facts2 <- frame_variance_facts(design, phase1, traits,
                                   call = current_env())
    design1 <- composed_design(
      get_design(phase1), design,
      census = stage_fact(facts2, 1L)$census
    )
    traits1 <- stage_variance_traits(design1, seq_along(design1$stages))
    # Phase 1 is realized, so its own draw says whether stage 1 holds one
    # unit drawn from several.
    rows <- singlephase_estimator_rows(
      design1, traits1,
      composed_variance_facts(phase1, design1, facts2),
      one_unit = phase1_one_unit_findings(
        get_design(phase1), get_stages_executed(phase1),
        as.data.frame(phase1), twophase = FALSE
      )
    )
    # Random groups vary with phase 1 only when it was replicated too, or
    # could select nothing else.
    fixed <- phase1_is_fixed_one_unit(phase1)
    lapply(rows, function(row) {
      if (fixed || !identical(row$estimator, "Random groups")) {
        return(row)
      }
      entry <- Filter(function(e) identical(e$key, "random_groups"),
                      estimator_catalog())[[1]]
      estimator_row(entry, random_groups_phase1_findings(phase1))
    })
  } else if (phase1_is_fixed_one_unit(phase1)) {
    # Exported as a one-phase sample of the units a fixed phase 1 holds.
    singlephase_estimator_rows(
      design, traits,
      frame_variance_facts(design, phase1, traits, call = current_env())
    )
  } else {
    twophase_estimator_rows(design, traits, phase1,
                            sample2 = if (is_tbl_sample(x)) x)
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

#' Refuse a sample whose export the report does not describe
#'
#' An occasion exports by ultimate clusters and refuses `"rwyb"`, and a
#' shared-weight sample exports by replicating its source design. The
#' report reads the design's rules, which neither follows.
#' @noRd
refuse_unassessed_sample <- function(s, call = caller_env()) {
  if (!is_null(attr(s, "metadata")[["occasion"]])) {
    abort_samplyr(
      c(
        "{.fn variance_estimators} does not assess a materialized occasion.",
        "i" = "An occasion's export follows rules of its own: ultimate clusters
               by default, and no {.val rwyb} replicates.",
        "i" = "Call {.fn as_svydesign} or {.fn as_svrepdesign} on the occasion
               to see whether an estimator is supported."
      ),
      class = "samplyr_error_variance_estimators_input",
      call = call
    )
  }
  if (identical(sample_weight_contract(s), "shared")) {
    abort_samplyr(
      c(
        "{.fn variance_estimators} does not assess a shared-weight sample.",
        "i" = "Its export replicates the source design and shares the
               weights inside every replicate, so the source design's report
               does not describe it.",
        "i" = "Use {.fn as_svrepdesign} on the shared sample."
      ),
      class = "samplyr_error_variance_estimators_input",
      call = call
    )
  }
  invisible(NULL)
}

#' Refuse a design that holds a reserve
#'
#' A settled reserve exports by ultimate clusters, whatever the stages
#' below, so the design's own rules do not describe its export.
#' @noRd
refuse_reserve_design <- function(design, call = caller_env()) {
  reserve <- vapply(
    design$stages,
    function(stage) !is_null(stage$draw_spec$reserve),
    logical(1)
  )
  if (!any(reserve)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn variance_estimators} does not assess a design with a reserve.",
      "i" = "A list is exported only once settled, by ultimate clusters, and
             {.val mrbbootstrap} and {.val rwyb} are refused.",
      "i" = "Settle it with {.code execute(sample, response_status = col)},
             then call {.fn as_svydesign} or {.fn as_svrepdesign} to see
             whether an estimator is supported."
    ),
    class = "samplyr_error_variance_estimators_input",
    call = call
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
      # A systematic draw of one PSU per zone is a draw proportional to size.
      systematic = draw$method %in% systematic_methods &&
        !draws_one_per_zone(draw),
      clustered = !is_null(stage$clusters),
      stratified = !is_null(stage$strata),
      midstage_element = is_null(stage$clusters) &&
        !is_multi_hit_method(draw) && pos < n,
      random_size = is_random_size_method(draw),
      census = design_takes_every_unit(draw),
      draw = draw
    )
  })
}

#' Does the design alone prove that a stage takes every unit?
#'
#' A fraction of one, as one value or as a table of ones, with a fixed-size
#' draw without replacement and no cap on the take. A size `n` needs the
#' frame to show it.
#' @noRd
design_takes_every_unit <- function(draw) {
  frac <- draw$frac
  if (is.data.frame(frac)) {
    frac <- frac$frac
  }
  is.numeric(frac) && length(frac) > 0L && all(frac == 1) &&
    is_null(draw$max_n) && is_wor_method(draw) &&
    !is_random_size_method(draw)
}

#' Does a stage take every unit, by the frame or by the design?
#' @noRd
report_takes_every_unit <- function(trait, facts) {
  isTRUE(stage_fact(facts, trait$stage)$census) || isTRUE(trait$census)
}

## What the frame fixes before any draw

#' Pool sizes and takes of each stage, resolved against the frame
#'
#' `NULL` without a frame, or when the design cannot be resolved ex ante (a
#' stage below a with-replacement stage), in which case every frame check is
#' left to the sample. A frame of the first stages only, lacking the
#' variables of a later stage, gives facts for the stages above that one.
#' A list of several registers gets the error [execute()] would raise.
#' @noRd
frame_variance_facts <- function(design, frame, traits, call = caller_env()) {
  if (is_null(frame)) {
    return(NULL)
  }
  resolve <- function(depth, frame) {
    tryCatch(
      resolve_exante_digest(design, frame, depth = depth, call = call),
      samplyr_error_exante_unsupported = function(e) NULL,
      samplyr_exante_unresolvable = function(e) NULL
    )
  }
  digest <- tryCatch(
    resolve(NULL, frame),
    samplyr_error_frame_missing_vars = function(e) e,
    samplyr_error_frame_missing_ancestry = function(e) e
  )
  # A stage drawn with replacement leaves its children's pools uncounted,
  # since each hit is one, but the stages down to it are resolved.
  wr <- which(vapply(traits, function(t) identical(t$kind, "wr"), NA))
  if (is_null(digest) && length(wr) > 0L && wr[[1]] < length(traits)) {
    several <- !is.data.frame(frame) && length(frame) > 1L
    digest <- resolve(
      wr[[1]], if (several) frame[seq_len(wr[[1]])] else frame
    )
  }
  if (inherits(digest, "condition")) {
    # A list of stage registers covers every stage, so it falls back to no
    # shallower depth.
    several <- !is.data.frame(frame) && length(frame) > 1L
    if (several || is_null(digest$stage) || digest$stage < 2L) {
      rlang::cnd_signal(digest)
    }
    digest <- resolve(digest$stage - 1L, frame)
  }
  if (is_null(digest)) {
    return(NULL)
  }
  facts <- lapply(seq_along(digest$stages), function(pos) {
    record <- digest$stages[[pos]]
    trait <- traits[[pos]]
    pools <- record$pools
    units <- record$units
    # The digest summarizes an element stage's chances into quantile bins,
    # which can average a unit of chance one away. The resolution counts
    # those units before it summarizes.
    n_cert <- rep(0, nrow(pools))
    if (trait$kind %in% c("pps_wor", "wr")) {
      n_cert <- as.numeric(pool_certainty_counts(
        record, attr(digest, "exante_pool_certain")
      ))
      n_cert[is.na(n_cert)] <- 0
    }
    fixed <- !trait$random_size
    if (draws_one_per_zone(trait$draw)) {
      # Exports collapse one-per-zone draws into the plan's variance groups.
      reg <- trait$draw$certainty_plan$register
      zoned <- !is.na(reg$pair)
      cells <- unique(
        data.frame(zone = paste(reg$stratum, reg$zone), pair = reg$pair)[zoned, ]
      )
      take <- as.numeric(tabulate(cells$pair))
      whole_certain <- rep(FALSE, length(take))
      whole_pools <- integer(0)
      n_pools <- length(take)
    } else {
      take <- pools$n_target - n_cert
      # As in as_svrepdesign(), a stage-1 pool taken whole above a later stage
      # is a set of certainty units.
      whole_certain <- pos == 1L && length(traits) > 1L && fixed &&
        !identical(trait$kind, "wr") & pools$n_target >= pools$N
      whole_pools <- pools$pool_id[whole_certain]
      n_pools <- nrow(pools)
    }
    # Pools taken whole, which an export reads as a take-all stratum.
    pool_census <- if (identical(trait$kind, "wr") || !fixed) {
      rep(FALSE, n_pools)
    } else if (draws_one_per_zone(trait$draw)) {
      whole_certain
    } else {
      pools$n_target >= pools$N
    }
    list(
      stage = trait$stage,
      n_pools = n_pools,
      n_units = sum(pools$N),
      n_certainty = sum(n_cert),
      whole_certain = whole_certain,
      whole_pools = whole_pools,
      fixed = fixed,
      # A pool of one unit taken whole is a census, as the exports read it.
      singleton = if (fixed) take == 1 & !pool_census else rep(FALSE, n_pools),
      takes = take,
      pool_census = pool_census,
      # A take of N or more is a census only without replacement. With
      # replacement it counts draws, and the stage still adds variance.
      census = !identical(trait$kind, "wr") && fixed && nrow(pools) > 0 &&
        all(pools$n_target >= pools$N),
      parents = unique(pools$parent_unit),
      rows_per_unit = units$n_descendants
    )
  })
  for (pos in seq_along(facts)[-1]) {
    facts[[pos]]$empty_parents <- facts[[pos - 1L]]$n_units -
      length(facts[[pos]]$parents)
    parents <- digest$stages[[pos - 1L]]$units
    certain <- if (!is_null(parents) && "is_certainty" %in% names(parents)) {
      parents$unit_id[parents$is_certainty]
    }
    if (!is_null(parents) && length(facts[[pos - 1L]]$whole_pools) > 0L) {
      certain <- union(certain, parents$unit_id[
        parents$pool_id %in% facts[[pos - 1L]]$whole_pools
      ])
    }
    pools <- digest$stages[[pos]]$pools
    under <- pools$parent_unit %in% certain
    facts[[pos]]$certain_parent_takes <- facts[[pos]]$takes[under]
    facts[[pos]]$certain_parent_census <- facts[[pos]]$pool_census[under]
    # Whether a later stage samples inside the selected units of each of
    # those pools, NA when the frame or the sample decides it.
    facts[[pos]]$certain_parent_below <- pool_samples_below(
      digest, traits, from = pos, pool_ids = pools$pool_id[under],
      census = facts[[pos]]$pool_census[under]
    )
    # A certain PSU is a replicate stratum of its pool's selected units. One
    # unit outside certainty from a larger pool is a singleton. One selected
    # unit otherwise, taken whole or with certainty, is harmless only when
    # nothing is drawn inside it. The selected count is the resolved one,
    # which a take capped at the pool's size or a certainty unit leaves at
    # one where the take outside certainty reads two or zero.
    genuine <- (facts[[pos]]$takes == 1 & !facts[[pos]]$pool_census)[under]
    one <- (pools$n_expected == 1)[under] & !genuine
    below <- if (facts[[pos]]$fixed && any(one)) {
      draws_below_pools(digest, pools$pool_id[under][one], traits,
                        from = pos)
    } else {
      FALSE
    }
    facts[[pos]]$certain_parent_one <- one
    facts[[pos]]$certain_parent_single <- genuine | (one & isTRUE(below))
    facts[[pos]]$certain_parent_unknown <- one & is.na(below)
  }
  if (length(facts) == 1L && wr_certain_single_shape(traits) &&
      facts[[1]]$n_certainty > 0) {
    facts[[1]]$certain_child_pools <- wr_certain_child_pools(
      design, frame, digest, traits, call = call
    )
  }
  facts
}

#' The stage-2 pools under the PSUs a stage-1 draw with replacement takes
#' for sure
#'
#' A PSU taken with certainty is taken once, so its stage-2 pools are fixed,
#' although the resolution stops below a draw with replacement because the
#' other PSUs' hits are random. They are resolved with stage 1 read as a
#' census, which builds the same units, and kept for the certain PSUs. The
#' stages below are resolved the same way when the frame reaches them.
#' @return `NULL` when the frame does not resolve those pools. Otherwise a
#'   list of `pools`, a data frame with the population `N`, target
#'   `n_target`, take outside certainty `take` and `census` (taken whole
#'   without replacement) of each pool, and
#'   `samples_below`, whether a later stage draws inside a pool of one
#'   selected unit there (`NA` when the frame does not settle it).
#' @noRd
wr_certain_child_pools <- function(design, frame, digest, traits,
                                   call = caller_env()) {
  units <- digest$stages[[1]]$units
  if (is_null(units) || !"is_certainty" %in% names(units)) {
    return(NULL)
  }
  certain <- units$unit_id[units$is_certainty %in% TRUE]
  # A census of stage 1 keeps its strata and empty-pool policy. An
  # allocation cannot be paired with a fraction, so it is dropped with its
  # inputs.
  census <- design
  census$stages[[1]]$draw_spec <-
    (sampling_design() |> draw(frac = 1))$stages[[1]]$draw_spec
  census$stages[[1]]$draw_spec$on_empty <- design$stages[[1]]$draw_spec$on_empty
  if (!is_null(census$stages[[1]]$strata)) {
    allocation <- c("alloc", "variance", "cost", "cv", "importance", "power")
    census$stages[[1]]$strata[allocation] <- list(NULL)
  }
  several <- !is.data.frame(frame) && length(frame) > 1L
  view_at <- function(depth) {
    tryCatch(
      resolve_exante_digest(
        census, if (several && identical(depth, 2L)) frame[1:2] else frame,
        depth = depth, call = call
      ),
      samplyr_error = function(e) NULL,
      samplyr_exante_unresolvable = function(e) NULL
    )
  }
  view <- if (length(traits) > 2L) view_at(NULL)
  if (is_null(view)) {
    view <- view_at(2L)
  }
  if (is_null(view) || length(view$stages) < 2L) {
    return(NULL)
  }
  same <- c("unit_id", "pool_id", "n_descendants")
  if (!all(same %in% names(view$stages[[1]]$units)) ||
      !identical(view$stages[[1]]$units[same], units[same])) {
    return(NULL)
  }
  record <- view$stages[[2]]
  pools <- record$pools
  n_cert <- rep(0, nrow(pools))
  if (traits[[2]]$kind %in% c("pps_wor", "wr")) {
    n_cert <- as.numeric(pool_certainty_counts(
      record, attr(view, "exante_pool_certain")
    ))
    n_cert[is.na(n_cert)] <- 0
  }
  under <- pools$parent_unit %in% certain
  take <- pools$n_target - n_cert
  # Only a pool of one selected unit makes its PSU a stratum of one unit,
  # so only its descendants are read: sampling inside a pool of two units
  # leaves that stratum two.
  one <- under & pools$n_expected == 1
  below <- if (any(under & take == 0 & pools$N > 1)) {
    # A stage-2 unit taken with certainty is not read here.
    NA
  } else if (any(one)) {
    draws_below_pools(view, pools$pool_id[one], traits)
  } else {
    FALSE
  }
  # A take of N with replacement counts draws, as the exports read it.
  census <- !identical(traits[[2]]$kind, "wr") & pools$n_target >= pools$N
  list(
    pools = data.frame(
      N = pools$N[under],
      n_target = pools$n_target[under],
      take = take[under],
      census = census[under],
      below = pool_samples_below(view, traits, from = 2L,
                                 pool_ids = pools$pool_id[under],
                                 census = census[under])
    ),
    samples_below = below
  )
}

#' The stage-2 pools under the PSUs stage 1 takes with certainty
#'
#' Read from the stage-2 facts, or, below a stage 1 drawn with replacement,
#' whose children the resolution leaves uncounted, from the pools
#' `wr_certain_child_pools()` resolves for its certain PSUs.
#' @return `NULL` when neither is there. Otherwise a list of the pools'
#'   `takes` outside certainty, `census`, `below` (whether a later stage
#'   samples inside their units), `one` (a single selected unit that is no
#'   singleton), `unknown` (a pool of one whose descendants are unsettled)
#'   and `fixed`.
#' @noRd
certain_parent_pools <- function(traits, facts) {
  if (length(traits) < 2L) {
    return(NULL)
  }
  second <- stage_fact(facts, traits[[2]]$stage)
  if (!is_null(second)) {
    n <- length(second$certain_parent_takes)
    return(list(
      takes = second$certain_parent_takes,
      census = second$certain_parent_census %||% rep(FALSE, n),
      below = second$certain_parent_below %||% rep(NA, n),
      one = second$certain_parent_one %||% rep(FALSE, n),
      unknown = second$certain_parent_unknown %||% rep(FALSE, n),
      fixed = second$fixed
    ))
  }
  child <- stage_fact(facts, traits[[1]]$stage)$certain_child_pools$pools
  if (is_null(child)) {
    return(NULL)
  }
  selected <- ifelse(child$census, child$N, child$n_target)
  singleton <- child$take == 1 & !child$census
  list(
    takes = child$take,
    census = child$census,
    below = child$below,
    one = selected == 1 & !singleton,
    unknown = rep(FALSE, nrow(child)),
    fixed = TRUE
  )
}

#' Does a later stage draw inside the units of some pools of stage `from`?
#'
#' `FALSE` when every pool below them is taken whole, `TRUE` when one of
#' fixed size without replacement is not, and `NA` when the resolution does
#' not reach those stages or one of them is drawn with replacement or at a
#' random size.
#' The below-state of units, FALSE for a unit with no child pool
#' @noRd
lookup_below <- function(below, unit_ids) {
  out <- unname(below[as.character(unit_ids)])
  out[is.na(match(as.character(unit_ids), names(below)))] <- FALSE
  out
}

#' Does a later stage sample inside the units a pool selects?
#'
#' Read bottom up: a stage-k pool samples when its take is below its size,
#' and a pool taken whole samples when one of its units does. A unit samples
#' below when one of its child pools does. A pool of stage `from` is then
#' `FALSE` when none of its units samples below, `TRUE` when every unit
#' does or when it is taken whole and one does, and `NA` when the sample
#' decides it, or the frame does not reach the last stage, or a stage below
#' is drawn with replacement or at random size.
#' @return A logical vector along `pool_ids`.
#' @noRd
pool_samples_below <- function(view, traits, from, pool_ids, census) {
  if (length(pool_ids) == 0L) {
    return(logical(0))
  }
  last <- length(traits)
  if (last == from) {
    return(rep(FALSE, length(pool_ids)))
  }
  units <- view$stages[[from]]$units
  if (length(view$stages) < last || is_null(units)) {
    return(rep(NA, length(pool_ids)))
  }
  any3 <- function(v) {
    if (any(v %in% TRUE)) TRUE else if (anyNA(v)) NA else FALSE
  }
  below <- NULL
  for (k in last:(from + 1L)) {
    pools <- view$stages[[k]]$pools
    flag <- if (identical(traits[[k]]$kind, "wr") || traits[[k]]$random_size) {
      rep(NA, nrow(pools))
    } else {
      pools$n_target < pools$N
    }
    if (!is_null(below)) {
      kunits <- view$stages[[k]]$units
      inner <- if (is_null(kunits)) {
        rep(NA, nrow(pools))
      } else {
        by_pool <- tapply(lookup_below(below, kunits$unit_id), kunits$pool_id,
                          any3)
        out <- unname(by_pool[as.character(pools$pool_id)])
        # A pool with no unit listed holds nothing below.
        out[is.na(match(pools$pool_id, kunits$pool_id))] <- FALSE
        out
      }
      flag <- ifelse(flag %in% TRUE | inner %in% TRUE, TRUE,
                     ifelse(is.na(flag) | is.na(inner), NA, FALSE))
    }
    below <- tapply(flag, pools$parent_unit, any3)
  }
  unit_below <- lookup_below(below, units$unit_id)
  vapply(seq_along(pool_ids), function(i) {
    v <- unit_below[units$pool_id == pool_ids[[i]]]
    if (length(v) == 0L || all(v %in% FALSE)) {
      FALSE
    } else if (all(v %in% TRUE) || (isTRUE(census[[i]]) && any(v %in% TRUE))) {
      TRUE
    } else {
      NA
    }
  }, NA)
}

#' @noRd
draws_below_pools <- function(view, pool_ids, traits, from = 2L) {
  last <- length(traits)
  if (last == from) {
    return(FALSE)
  }
  if (length(view$stages) < last) {
    return(NA)
  }
  for (k in (from + 1L):last) {
    parents <- view$stages[[k - 1L]]$units
    if (is_null(parents)) {
      return(NA)
    }
    ids <- parents$unit_id[parents$pool_id %in% pool_ids]
    pools <- view$stages[[k]]$pools
    sel <- pools$parent_unit %in% ids
    if (any(sel)) {
      if (identical(traits[[k]]$kind, "wr") || traits[[k]]$random_size) {
        return(NA)
      }
      if (any(pools$n_target[sel] < pools$N[sel])) {
        return(TRUE)
      }
    }
    pool_ids <- pools$pool_id[sel]
  }
  FALSE
}

## Rows

#' @noRd
estimator_catalog <- function() {
  list(
    list(key = "linearization", estimator = "Taylor linearization",
         call = "as_svydesign(x)"),
    list(key = "joint", estimator = "Linearization, joint probabilities",
         call = paste0("as_svydesign(x, pps = survey::ppsmat(",
                       "joint_expectation(x)[[1]]), variance = \"YG\")")),
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
singlephase_estimator_rows <- function(design, traits, facts,
                                       one_unit = NULL) {
  common <- common_stage_findings(traits, facts)
  if (!is_null(one_unit)) {
    common$one_unit <- one_unit
  }
  lapply(estimator_catalog(), function(entry) {
    switch(
      entry$key,
      linearization = estimator_row(
        entry,
        c(common$refusals, common$one_unit,
          linearization_findings(traits, facts),
          common$systematic, brewer_findings(traits, facts), common$risks,
          lonely_findings(traits, facts))
      ),
      joint = joint_estimator_row(entry, traits, facts, common),
      rwyb = estimator_row(entry, c(
        rwyb_findings(traits, facts, common),
        # RWYB refuses the stage of one unit with a class of its own.
        lapply(Filter(function(f) identical(f$effect, "refused"), one_unit),
               function(f) {
                 finding("refused", "samplyr_error_rwyb_singleton",
                         "Stage 1 holds a single unit not recorded as
                          taken with certainty, which RWYB cannot resample",
                         stage = f$stage, decided_by = "frame")
               })
      )),
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
    multi_hit <- is_multi_hit_method(t$draw)
    if ((!multi_hit && report_takes_every_unit(t, facts)) ||
        fact_takes_one_per_pool(fact)) {
      next
    }
    systematic <- c(systematic, list(finding(
      "approximate", "samplyr_warning_systematic_variance",
      paste0("A systematic ", t$name, " has no design-unbiased variance",
             " estimator. Its variance is approximated",
             if (is_null(fact)) {
               if (multi_hit) {
                 ", unless every pool takes one unit, which the frame shows"
               } else {
                 paste0(", unless the stage takes every unit or one unit per",
                        " pool, which the frame shows")
               }
             }),
      stage = t$stage,
      decided_by = "frame"
    )))
  }
  list(
    refusals = refusals,
    # The exports built on survey's design meet it; RWYB refuses the
    # stratum of one unit itself, first.
    one_unit = one_unit_findings(traits, facts),
    systematic = systematic,
    risks = empty_unit_findings(traits, facts)
  )
}

#' A first stage of one unit not taken with certainty
#'
#' survey refuses a design with a single primary unit outside a stratum,
#' and the exports carry the refusal (`samplyr_error_export_one_unit`).
#' A unit taken with certainty, or a pool of one taken whole, is a census
#' and exports. Without the frame, a draw of one without strata is a risk,
#' since the frame shows whether the pool holds more.
#' @noRd
one_unit_findings <- function(traits, facts) {
  first <- traits[[1]]
  if (first$stratified) {
    return(list())
  }
  note <- "Stage 1 holds a single primary unit not recorded as taken with
    certainty, which has no variance estimate"
  fact <- stage_fact(facts, first$stage)
  if (!is_null(fact)) {
    drawn <- sum(fact$takes[!fact$pool_census])
    if (fact$fixed && !draws_one_per_zone(first$draw) &&
        fact$n_certainty == 0 && drawn == 1 &&
        sum(fact$takes[fact$pool_census]) == 0) {
      return(list(finding("refused", "samplyr_error_export_one_unit", note,
                          stage = first$stage, decided_by = "frame")))
    }
    return(list())
  }
  n <- first$draw$n
  if (is.numeric(n) && length(n) == 1L && n == 1 && is_null(first$draw$frac)) {
    return(list(finding("risk", "samplyr_error_export_one_unit",
                        paste0(note, ", unless the frame holds that one unit"))))
  }
  list()
}

#' Does the frame fix one drawn unit in every pool of a stage?
#'
#' A systematic draw of one is a draw proportional to size, whatever the
#' frame order. Pools taken whole and pools taking nothing are left out, as
#' the export leaves them out. The facts cover every pool of the frame, so
#' at a later stage a pool that is never reached can still keep the
#' approximation.
#' @noRd
fact_takes_one_per_pool <- function(fact) {
  if (is_null(fact) || !isTRUE(fact$fixed)) {
    return(FALSE)
  }
  drawn <- fact$takes[!fact$pool_census & fact$takes > 0]
  length(drawn) > 0L && all(drawn == 1)
}

#' @noRd
stage_fact <- function(facts, stage) {
  if (is_null(facts)) {
    return(NULL)
  }
  Find(function(f) identical(f$stage, stage), facts)
}

#' Carry phase-2 pool facts onto the composed design's final stage
#' @noRd
composed_variance_facts <- function(phase1, design1, facts2) {
  fact2 <- stage_fact(facts2, 1L)
  if (is_null(fact2)) {
    return(NULL)
  }
  last <- length(design1$stages)
  df1 <- as.data.frame(phase1)
  fact2$stage <- last
  fact2$census <- isTRUE(fact2$census) && spec_stage_census(df1, last)
  fact2$whole_certain <- rep(FALSE, fact2$n_pools)
  fact2$whole_pools <- integer(0)
  if (last > 1L) {
    fact2$empty_parents <- if (last == 2L) {
      length(export_empty_records(phase1, design1, seq_len(last), vanished = TRUE))
    } else {
      0L
    }
  }
  list(fact2)
}

#' Primary units, and deeper parents, the frame leaves with nothing below
#' @noRd
empty_unit_findings <- function(traits, facts) {
  if (length(traits) < 2L) {
    return(list())
  }
  # A primary unit with nothing below it is exported as a zero row, which
  # the export names under `on_empty = "warn"`. A deeper unit keeps no row,
  # whatever `on_empty` allows it, and the linearized export warns of its
  # within-unit term under "warn".
  named <- Filter(function(t) {
    on_empty <- t$draw$on_empty %||% "error"
    if (identical(t$stage, traits[[2]]$stage)) {
      identical(on_empty, "warn")
    } else {
      t$stage > traits[[2]]$stage && !identical(on_empty, "error")
    }
  }, traits)
  if (length(named) == 0L) {
    return(list())
  }
  effect <- function(t) {
    if (identical(t$stage, traits[[2]]$stage)) {
      "is exported as a zero row, so estimates need `na.rm = TRUE`"
    } else {
      paste("has no row, so survey computes its parent's within-unit",
            "variance without it")
    }
  }
  out <- list()
  for (t in named) {
    class <- if (identical(t$draw$on_empty, "warn")) {
      "samplyr_warning_export_empty_parent"
    } else {
      NA_character_
    }
    fact <- stage_fact(facts, t$stage)
    if (is_null(fact)) {
      out <- c(out, list(finding(
        "risk", class,
        paste0("A selected unit with nothing at ", t$name, " ", effect(t),
               if (is_null(facts)) {
                 ". Pass `frame` to see whether any candidate has none"
               } else {
                 paste0(". A frame that reaches ", t$name, " shows whether",
                        " any candidate has none")
               })
      )))
      next
    }
    if (fact$empty_parents <= 0) {
      next
    }
    out <- c(out, list(finding(
      "risk", class,
      paste0(fact$empty_parents, " candidate unit",
             if (fact$empty_parents > 1) "s have" else " has",
             " nothing to sample at ", t$name, ", and a selected one ",
             effect(t))
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
brewer_findings <- function(traits, facts = NULL) {
  out <- list()
  for (t in traits) {
    # A systematic draw of one per pool is a draw of one proportional to size.
    one <- t$systematic && fact_takes_one_per_pool(stage_fact(facts, t$stage))
    note <- if (identical(t$method, "cube")) {
      paste0("The balanced ", t$name, " is linearized with the high-entropy",
             " approximation, which ignores the balancing")
    } else if (identical(t$method, "pps_chromy")) {
      paste0("Chromy's ", t$name, " is treated as drawn with replacement,",
             " which can be strongly conservative")
    } else if (identical(t$method, "pps_systematic_mr") && !one) {
      paste0("The systematic ", t$name, " with minimum replacement is",
             " treated as drawn with replacement, which ignores the",
             " dependence of systematic selection. The frame order decides",
             " whether that overstates or understates the variance")
    } else if (draws_one_per_zone(t$draw)) {
      paste0("One PSU per zone at ", t$name, ", collapsed in variance",
             " groups, which errs toward too large a variance")
    } else if (identical(t$kind, "pps_wor") && (!t$systematic || one)) {
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
  advice <- "Set `options(survey.lonely.psu = \"adjust\")` or collapse the
      strata"
  c(singleton_findings(
    traits, facts,
    class = "samplyr_warning_lonely_psu",
    effect = "approximate",
    advice = advice
  ), wr_certain_single_findings(traits, facts, "samplyr_warning_lonely_psu",
                                "approximate", advice))
}

#' The stratum of one unit below a PSU a WR draw took for sure
#' @noRd
wr_certain_single_findings <- function(traits, facts, class, effect, advice) {
  hit <- wr_certain_parent_single(traits, facts)
  # Unresolved pools are a risk singleton_findings() already reports.
  if (is_null(hit) || !identical(hit$state, "single")) {
    return(list())
  }
  list(finding(
    effect, class,
    paste0("Stage ", hit$stage, " takes a single unit from a pool of a PSU",
           " taken with certainty, which is a stratum of its own. ", advice),
    stage = hit$stage, decided_by = "frame"
  ))
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
    return(c(lone_zone_findings(traits, class, effect, advice, skip),
             list(finding(
      "risk", class,
      "A stratum left with one sampled unit outside certainty. Pass `frame`
       to see whether the allocation fixes one"
    ))))
  }
  out <- list()
  uncovered <- FALSE
  for (t in traits) {
    if (skip(t)) {
      next
    }
    fact <- stage_fact(facts, t$stage)
    if (is_null(fact)) {
      uncovered <- TRUE
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
  if (uncovered) {
    out <- c(out, list(finding(
      "risk", class,
      "A stratum left with one sampled unit outside certainty. A frame that
       reaches every stage shows whether the allocation fixes one"
    )))
  }
  out
}

#' A variance group of one zone, which a certainty plan fixes without a frame
#'
#' One PSU is drawn per zone, so a group holding a single zone holds a
#' single unit outside certainty whatever the frame.
#' @noRd
lone_zone_findings <- function(traits, class, effect, advice,
                               skip = function(t) FALSE) {
  out <- list()
  for (t in traits) {
    if (skip(t) || !draws_one_per_zone(t$draw)) {
      next
    }
    reg <- t$draw$certainty_plan$register
    zoned <- !is.na(reg$pair)
    cells <- unique(data.frame(zone = paste(reg$stratum, reg$zone),
                               pair = reg$pair)[zoned, ])
    lone <- sum(tabulate(cells$pair) == 1L)
    if (lone > 0L) {
      out <- c(out, list(finding(
        effect, class,
        paste0(lone, " variance group", if (lone > 1) "s" else "",
               " at ", t$name, " hold", if (lone > 1) "" else "s",
               " a single zone, so a single unit outside certainty. ",
               advice),
        stage = t$stage
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
  if (identical(first$kind, "wr") && !is_null(first$draw$mos)) {
    return(estimator_row(entry, list(), not_applicable = paste0(
      "Stage 1 is drawn with replacement or minimum replacement.",
      " joint_expectation() gives its joint expected hits, but survey's pps",
      " argument needs inclusion probabilities, so the stage is exported",
      " as drawn with replacement"
    )))
  }
  if (!pps_first) {
    return(estimator_row(entry, list(), not_applicable = paste0(
      "Stage 1 is not drawn with unequal probabilities, so there is no joint",
      " matrix to supply"
    )))
  }
  if (identical(first$kind, "rs_poisson")) {
    entry$call <- paste0(
      "as_svydesign(x, pps = survey::ppsmat(joint_expectation(x)[[1]]))"
    )
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
  registered <- !is_null(first$draw$method_type)
  if (registered && is_null(custom_method_spec(first$method)$joint_fn)) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_joint_method_unsupported",
      paste0(first$method, " was registered without joint_fn, so it has no",
             " joint inclusion probabilities"),
      stage = 1L
    )))
  }
  if (draws_one_per_zone(first$draw)) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_joint_method_unsupported",
      "Joint inclusion probabilities are not computed for a stage that draws
       one PSU per zone",
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
  # A registered matrix is the joint_fn's own, not one samplyr approximates.
  quality <- if (registered || first$method %in% c("pps_cps", "pps_sampford",
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
  out <- c(out, common$one_unit, if (!is_null(quality)) list(quality),
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
  # A stage the frame does not reach is judged by its design take.
  takes_more <- vapply(traits[-1], function(t) {
    fact <- stage_fact(facts, t$stage)
    if (is_null(fact)) {
      n <- t$draw$n
      return(!(is.numeric(n) && length(n) == 1L && n == 1 &&
                 is_null(t$draw$frac)))
    }
    length(fact$takes) > 0 && any(fact$takes > 1)
  }, logical(1))
  if (!any(takes_more)) {
    return(list())
  }
  by_frame <- vapply(traits[-1][takes_more], function(t) {
    !is_null(stage_fact(facts, t$stage))
  }, logical(1))
  if (any(by_frame)) {
    return(list(finding("refused", "samplyr_error_pps_rows_per_psu", note,
                        stage = 1L, decided_by = "frame")))
  }
  list(finding("refused", "samplyr_error_pps_rows_per_psu", note,
               stage = 1L))
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
  # Strata taken whole above a later stage are converted as certainty units,
  # which drops the other strata's correction and stratifies the first stage.
  whole <- stage_fact(facts, first$stage)$whole_certain %||%
    (isTRUE(first$census) && length(traits) > 1L)
  spliced <- any(whole) &&
    type %in% c("JK1", "JKn", "bootstrap", "subbootstrap", "BRR", "Fay")
  if (spliced) {
    facts <- whole_pools_not_single(facts, first$stage)
  }
  # A single stage taken whole keeps its full weight in every replicate.
  census <- length(traits) == 1L && report_takes_every_unit(first, facts)
  fpc_dropped <- identical(first$kind, "equal_wor") && !all(whole) &&
    !census && (type %in% c("BRR", "Fay", "subbootstrap") || spliced)
  # Minimum replacement has no correction for the probability-based types
  # to keep, so they read it as with replacement too. A first stage taken
  # whole holds no unit to resample.
  all_whole <- census || (length(whole) > 0L && all(whole))
  if ((stage_replicated_as_wr(first$draw, first$kind) && !all_whole &&
         (!pps_safe || first$method %in% pmr_methods)) ||
      fpc_dropped) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_replicate_wr_first_stage",
      paste0("The first stage, drawn with ", first$method, ", is treated as",
             " drawn with replacement, ",
             if (first$method %in% pmr_methods) {
               "as in every export, which ignores the dependence between hits"
             } else {
               "which errs toward too large a variance"
             }),
      stage = first$stage
    )))
  }
  if (type %in% c("JK1", "JKn", "bootstrap") && !spliced &&
      identical(first$kind, "equal_wor") &&
      later_stage_may_sample(traits, facts)) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_replicate_first_stage_fpc",
      "The first stage's finite population correction also scales the
       variance later stages add, which errs toward too small a variance",
      stage = first$stage
    )))
  }
  if (type %in% c("JK1", "JKn", "BRR", "Fay", "bootstrap", "subbootstrap")) {
    out <- c(out, certainty_stage_wr_findings(traits, facts))
  }
  out <- c(out, common$systematic, common$refusals, common$one_unit)
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
  stratified <- replicate_first_stage_stratified(traits, facts) || spliced
  # Without the frame, a certainty rule drawn with replacement may or may
  # not take a unit, so whether stage 1 is stratified is unknown.
  unknown <- !stratified && identical(first$kind, "wr") &&
    (has_certainty_rule(first$draw) || !is_null(first$draw$certainty_plan)) &&
    is_null(stage_fact(facts, first$stage))
  if (unknown && type %in% c("JK1", "JKn", "BRR", "Fay")) {
    out <- c(out, list(finding(
      "risk", "samplyr_error_svrep_conversion_failed",
      "A unit stage 1 takes with certainty is a stratum of its own, so
       whether survey reads stage 1 as stratified depends on the frame"
    )))
  }
  if (identical(type, "JK1") && stratified) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_svrep_conversion_failed",
      "survey's JK1 takes an unstratified first stage, and this one is
       stratified (a PPS first stage, or units taken with probability 1
       above a later stage, are exported with strata of their own)",
      stage = traits[[1]]$stage
    )))
  }
  if (type %in% c("JKn", "BRR", "Fay") && !stratified && !unknown) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_svrep_conversion_failed",
      "survey needs a stratified first stage for this type. JK1 or the
       bootstrap take an unstratified one",
      stage = traits[[1]]$stage
    )))
  }
  # survey converts an equal-probability first stage itself. samplyr
  # converts a PPS one and gives its census a zero variance.
  if (identical(type, "JKn") && !spliced &&
      identical(first$kind, "equal_wor") &&
      report_takes_every_unit(first, facts)) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_svrep_conversion_failed",
      "survey's JKn stops when every first-stage stratum is taken whole, since
       the first stage then has no sampling variance",
      stage = 1L,
      decided_by = if (isTRUE(first$census)) "design" else "frame"
    )))
  }
  one_whole <- stage_fact(facts, first$stage)
  if (identical(type, "subbootstrap") && length(traits) == 1L &&
      identical(first$kind, "equal_wor") && !is_null(one_whole) &&
      any(one_whole$pool_census & one_whole$takes == 1)) {
    out <- c(out, list(finding(
      "refused", "samplyr_error_svrep_conversion_failed",
      "A stratum taken whole holds a single unit, which the rescaled
       bootstrap cannot resample",
      stage = first$stage, decided_by = "frame"
    )))
  }
  if (type %in% c("JKn", "BRR", "Fay", "subbootstrap")) {
    out <- c(out, singleton_findings(
      traits[1], facts,
      class = "samplyr_error_svrep_conversion_failed",
      effect = "refused",
      advice = "survey cannot form replicates from it"
    ), certainty_singleton_findings(traits, facts))
  }
  if (type %in% c("BRR", "Fay")) {
    out <- c(out, brr_findings(traits, facts))
  }
  c(out, common$risks)
}

#' Stage 2 read as drawn with replacement inside a PSU taken with certainty
#'
#' The generic replicate types resample a certainty PSU's stage-2 units. The
#' export restores stage 2's correction when it drew with equal
#' probabilities without replacement and nothing below it samples. A PPS
#' stage 2 that does not take its pool whole, or a stage below that samples,
#' keeps the with-replacement reading
#' (`samplyr_warning_replicate_certainty_stage_wr`). The frame settles it
#' through the stage-2 pools under the certain PSUs. Without them, a design
#' census of stage 1 is read from the design, and a first stage that may
#' take a PSU with certainty is a risk.
#' @noRd
certainty_stage_wr_findings <- function(traits, facts) {
  if (length(traits) < 2L) {
    return(list())
  }
  second <- traits[[2]]
  pps <- identical(second$kind, "pps_wor")
  if (!pps && !identical(second$kind, "equal_wor")) {
    return(list())
  }
  first <- traits[[1]]
  note <- paste0("Stage ", second$stage, " inside a PSU taken with",
                 " certainty is treated as drawn with replacement, which",
                 " errs toward too large a variance")
  approximate <- function(decided_by) {
    list(finding("approximate", "samplyr_warning_replicate_certainty_stage_wr",
                 note, stage = second$stage, decided_by = decided_by))
  }
  risk <- function(why) {
    list(finding("risk", "samplyr_warning_replicate_certainty_stage_wr",
                 paste0(note, ". ", why)))
  }
  pools <- certain_parent_pools(traits, facts)
  if (!is_null(pools)) {
    if (length(pools$takes) == 0L) {
      return(list())
    }
    below <- pools$below
    if (any(below %in% TRUE) || (pps && !all(pools$census))) {
      return(approximate("frame"))
    }
    return(if (anyNA(below)) {
      risk("Whether a stage below samples inside the units such a PSU
            selects depends on the frame or on the sample")
    } else {
      list()
    })
  }
  if (!pps && !later_stage_may_sample(traits[-1], facts)) {
    return(list())
  }
  fact <- stage_fact(facts, first$stage)
  if (!is_null(fact)) {
    if (fact$n_certainty > 0 || any(fact$whole_certain)) {
      return(risk("The frame does not settle the stage-2 pools of such a
                   PSU"))
    }
    return(list())
  }
  if (isTRUE(first$census)) {
    return(approximate("design"))
  }
  if (identical(first$kind, "pps_wor") || has_certainty_rule(first$draw) ||
      !is_null(first$draw$certainty_plan)) {
    return(risk(paste0("Pass `frame` to see whether stage ", first$stage,
                       " takes a PSU with certainty")))
  }
  list()
}

#' A PSU taken with certainty that keeps a single unit below it
#'
#' The generic replicate types make each certainty PSU a stratum of its
#' stage-2 units, so a take of one there leaves a stratum with one unit.
#' Without a frame, `singleton_findings()` already reports the risk.
#' @noRd
certainty_singleton_findings <- function(traits, facts) {
  if (length(traits) < 2L) {
    return(list())
  }
  first <- stage_fact(facts, traits[[1]]$stage)
  if (is_null(first) || (first$n_certainty == 0 && !any(first$whole_certain))) {
    return(list())
  }
  note <- "A PSU taken with certainty is a stratum of its stage-2 units in
    the replicate design, and one that keeps a single unit cannot be
    resampled"
  second <- stage_fact(facts, traits[[2]]$stage)
  hit <- wr_certain_parent_single(traits, facts)
  if (identical(hit$state, "single")) {
    return(list(finding(
      "refused", "samplyr_error_svrep_conversion_failed",
      paste0("Stage ", hit$stage, " takes a single unit from a pool of a",
             " PSU taken with certainty. Each such PSU is a stratum of its",
             " stage-2 units in the replicate design, so survey cannot form",
             " replicates from it"),
      stage = hit$stage, decided_by = "frame"
    )))
  }
  if (identical(hit$state, "none")) {
    # A pool of one unit is taken whole, so the PSU is a stratum of that one
    # unit, which adds no variance unless a later stage draws inside it.
    if (isFALSE(hit$samples_below)) {
      return(list())
    }
    if (isTRUE(hit$samples_below)) {
      return(list(finding(
        "refused", "samplyr_error_svrep_conversion_failed",
        paste0("Stage ", hit$stage, " keeps a single unit in a PSU taken",
               " with certainty, and a later stage draws inside it. Each",
               " such PSU is a stratum of its stage-2 units in the replicate",
               " design, so survey cannot form replicates from it"),
        stage = hit$stage, decided_by = "frame"
      )))
    }
  }
  if (is_null(second)) {
    return(list(finding("risk", "samplyr_error_svrep_conversion_failed",
                        note)))
  }
  single <- sum(second$certain_parent_single)
  if (second$fixed && single > 0) {
    return(list(finding(
      "refused", "samplyr_error_svrep_conversion_failed",
      paste0(single, " stage-2 pool", if (single > 1) "s" else "",
             " under a PSU taken with certainty take",
             if (single > 1) "" else "s", " a single unit. Each such PSU",
             " is a stratum of its stage-2 units in the replicate design,",
             " so survey cannot form replicates from it"),
      stage = traits[[2]]$stage, decided_by = "frame"
    )))
  }
  if ((!second$fixed && length(second$certain_parent_takes) > 0) ||
      any(second$certain_parent_unknown)) {
    return(list(finding("risk", "samplyr_error_svrep_conversion_failed",
                        note)))
  }
  list()
}

#' A stage-1 pool taken whole above a later stage leaves no single unit
#'
#' Its units are certainty units in the replicate conversion, so a pool of
#' one is no stratum with a single unit outside certainty.
#' @noRd
whole_pools_not_single <- function(facts, stage) {
  for (pos in seq_along(facts)) {
    if (identical(facts[[pos]]$stage, stage)) {
      facts[[pos]]$singleton[facts[[pos]]$whole_certain] <- FALSE
    }
  }
  facts
}

#' Does a stage below the first add variance?
#'
#' Without a frame, a later stage is taken to sample unless its design
#' takes every unit.
#' @noRd
later_stage_may_sample <- function(traits, facts) {
  any(vapply(traits[-1], function(t) {
    !report_takes_every_unit(t, facts)
  }, logical(1)))
}

#' Is the first stage the replicate types resample stratified?
#'
#' A PPS first stage is converted on its own, and that conversion always
#' passes survey a strata term.
#' @noRd
replicate_first_stage_stratified <- function(traits, facts = NULL) {
  first <- traits[[1]]
  # A unit taken with certainty by a stage drawn with replacement is a
  # stratum of its own, when the frame has one.
  wr_certain <- identical(first$kind, "wr") &&
    isTRUE(stage_fact(facts, first$stage)$n_certainty > 0)
  identical(first$kind, "pps_wor") || first$stratified || wr_certain
}

#' A stage-1 draw with replacement above a stage of fixed size
#'
#' The design shape `wr_certain_parent_single()` reads: stage 1 drawn with
#' replacement and a fixed take at stage 2, whatever its form, a number, a
#' vector or a table by stratum.
#' @noRd
wr_certain_single_shape <- function(traits) {
  if (length(traits) < 2L || !identical(traits[[1]]$kind, "wr")) {
    return(FALSE)
  }
  !traits[[2]]$random_size
}

#' A stage taking one unit inside a PSU a stage-1 WR draw took for sure
#'
#' A PSU taken with certainty is taken once, so the next stage's take in
#' each of its pools is that pool's own, whatever the hits of the other
#' PSUs. The frame says
#' whether there is such a PSU, and the facts of the stage below a draw with
#' replacement are not resolved. A take of one is a single unit outside
#' certainty unless the pool is a census: a pool of one unit taken whole
#' without replacement adds no variance.
#' @return `NULL` when the shape does not apply. Otherwise a list of the
#'   `stage`, its `state` and `samples_below`. The state is `"single"` when
#'   a pool under a certain PSU keeps one unit outside certainty, `"none"`
#'   when none does, and `"unknown"` when the frame does not resolve those
#'   pools. `samples_below` says whether a later stage draws inside the
#'   units taken there, as `wr_certain_child_pools()` returns it.
#' @noRd
wr_certain_parent_single <- function(traits, facts) {
  if (!wr_certain_single_shape(traits)) {
    return(NULL)
  }
  first <- stage_fact(facts, traits[[1]]$stage)
  if (!isTRUE(first$n_certainty > 0) ||
      !is_null(stage_fact(facts, traits[[2]]$stage))) {
    return(NULL)
  }
  child <- first$certain_child_pools
  state <- if (is_null(child)) {
    "unknown"
  } else if (any(child$pools$take == 1 & !child$pools$census)) {
    "single"
  } else {
    "none"
  }
  list(stage = traits[[2]]$stage, state = state,
       samples_below = if (is_null(child)) NA else child$samples_below)
}

#' Balanced half-samples pair the units of each first-stage stratum
#' @noRd
brr_findings <- function(traits, facts) {
  fact <- stage_fact(facts, traits[[1]]$stage)
  note <- "Balanced half-samples need an even number of sampled units, at
    least two, in every first-stage stratum"
  draw <- traits[[1]]$draw
  if (draws_one_per_zone(draw)) {
    # The plan fixes the variance groups, one sampled PSU per zone.
    reg <- draw$certainty_plan$register
    zoned <- !is.na(reg$pair)
    cells <- unique(data.frame(zone = paste(reg$stratum, reg$zone), pair = reg$pair)[zoned, ])
    if (any(tabulate(cells$pair) %% 2L != 0L)) {
      return(list(finding("refused", "samplyr_error_svrep_conversion_failed",
                          note, stage = traits[[1]]$stage)))
    }
  }
  if (is_null(fact) || !fact$fixed) {
    return(list(finding("risk", "samplyr_error_svrep_conversion_failed",
                        note)))
  }
  pairs_up <- function(takes) all(takes == 0 | (takes >= 2 & takes %% 2 == 0))
  # A certainty PSU is a stratum of its stage-2 units.
  if ((fact$n_certainty > 0 || any(fact$whole_certain)) &&
        length(traits) > 1L) {
    second <- certain_parent_pools(traits, facts)
    if (is_null(second) || !second$fixed) {
      return(list(finding("risk", "samplyr_error_svrep_conversion_failed",
                          note)))
    }
    # A pool of one selected unit, taken whole or with certainty, is no
    # half-sample stratum. certainty_singleton_findings() refuses one with a
    # stage drawn inside it. A pool taken whole with nothing drawn below is
    # no half-sample stratum either.
    # The pool's own census exempts it only when nothing below samples
    # inside its units, and the frame may leave that to the sample.
    census <- second$census
    below <- second$below
    takes <- second$takes
    surely <- !second$one & !(census & !(below %in% TRUE))
    maybe <- !second$one & !(census & below %in% FALSE)
    if (!pairs_up(takes[surely])) {
      return(list(finding(
        "refused", "samplyr_error_svrep_conversion_failed",
        paste0(note, ", and a PSU taken with certainty is a stratum of its",
               " stage-2 units"),
        stage = traits[[2]]$stage, decided_by = "frame"
      )))
    }
    if (!pairs_up(takes[maybe])) {
      return(list(finding("risk", "samplyr_error_svrep_conversion_failed",
                          note)))
    }
    if (any(second$unknown)) {
      return(list(finding("risk", "samplyr_error_svrep_conversion_failed",
                          note)))
    }
  }
  if (!pairs_up(fact$takes[!fact$whole_certain])) {
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
  pps <- Filter(function(t) {
    identical(t$kind, "pps_wor") && !draws_one_per_zone(t$draw)
  }, traits)
  if (length(pps) > 0) {
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_rwyb_pps_approximation",
      "RWYB approximates the joint probabilities of unequal-probability
       sampling without replacement",
      stage = pps[[1]]$stage
    )))
  }
  out <- c(out, common$refusals, common$risks)
  # A Poisson stage that selects nothing leaves its parent a zero row.
  poisson <- Filter(function(t) {
    identical(t$kind, "rs_poisson") && t$stage > traits[[1]]$stage
  }, traits)
  if (length(poisson) > 0) {
    out <- c(out, list(finding(
      "risk",
      note = paste0("A selected unit in which ", poisson[[1]]$name,
                    " selects nothing is a zero row, so estimates need",
                    " `na.rm = TRUE`")
    )))
  }
  advice <- "At the final stage, `lonely.psu = \"certainty\"` treats it as
      taken with certainty"
  c(out, singleton_findings(
    traits, facts,
    class = "samplyr_error_rwyb_singleton",
    effect = "refused",
    advice = advice,
    skip = function(t) identical(t$kind, "rs_poisson")
  ), wr_certain_single_findings(traits, facts, "samplyr_error_rwyb_singleton",
                                "refused", advice))
}

## Two phases

#' A phase 1 of one first-stage unit
#'
#' survey's two-phase design refuses a phase 1 with a single primary unit
#' outside a stratum, whether that unit was drawn or taken
#' with certainty. A phase 1 that could select nothing else is exported as
#' one phase and never reaches it, and a certainty rule makes the unit a
#' stratum of its own. A second phase drawn within phase 1's final-stage
#' pools is exported as one phase (`twophase = FALSE`), which passes
#' survey a census of that unit as a certainty stratum, so only a unit
#' not taken with certainty is refused there, by the export's own rule of
#' certainty (`stage1_sure_rows()`), for which a single hit with
#' replacement is no certainty.
#' @noRd
phase1_one_unit_findings <- function(design1, stages1, df1, twophase = TRUE) {
  first <- design1$stages[[stages1[1]]]
  certain <- df1[[paste0(".certainty_", stages1[1])]]
  if (!is_null(first$strata) || any(certain %in% TRUE)) {
    return(list())
  }
  units <- spec_stage_unit(df1, first, stages1[1])$id
  if (vctrs::vec_unique_count(units) != 1L) {
    return(list())
  }
  # The export's rule: a draw with replacement is certain only when its
  # certainty is recorded, since a weight of one is an expected hit.
  whole <- nrow(df1) > 0L && all(stage1_sure_rows(df1, design1, stages1))
  if (whole && !twophase) {
    return(list())
  }
  note <- if (whole) {
    "Phase 1 holds a single primary unit, taken with certainty, and
     survey's two-phase design needs at least two"
  } else {
    "Phase 1 holds a single primary unit not recorded as taken with
     certainty, which has no variance estimate"
  }
  list(finding(
    "refused", "samplyr_error_export_one_unit", note,
    stage = stages1[1], phase = 1L, decided_by = "frame"
  ))
}

#' A phase 2 that survey's two-phase design cannot build
#'
#' survey builds the phase-2 design, and phase 1's design on phase 2's
#' rows, and refuses either with a single primary unit outside a stratum.
#' A realized phase 2 settles both. Before it is drawn, a phase 2 of one
#' unit without strata is refused, and a draw small enough to fall in one
#' phase-1 unit, when phase 1 has no strata, is a risk.
#' @noRd
phase2_one_unit_findings <- function(design, design1, stages1, df1,
                                     sample2 = NULL) {
  first1 <- design1$stages[[stages1[1]]]
  first2 <- design$stages[[1]]
  certain1 <- df1[[paste0(".certainty_", stages1[1])]]
  plain1 <- is_null(first1$strata) && !any(certain1 %in% TRUE)
  plain2 <- is_null(first2$strata)
  note2 <- "Phase 2 holds a single primary unit, which survey's two-phase
    design refuses"
  note_rows <- "Phase 2's rows all fall in a single phase-1 primary unit,
    which survey's two-phase design refuses"
  if (!is_null(sample2)) {
    df2 <- as.data.frame(sample2)
    if (plain2 && vctrs::vec_unique_count(
      spec_stage_unit(df2, first2, 1L)$id
    ) == 1L) {
      return(list(finding("refused", "samplyr_error_export_one_unit", note2,
                          stage = 1L, phase = 2L, decided_by = "frame")))
    }
    units1 <- spec_stage_unit(df2, first1, stages1[1])$id
    if (plain1 && vctrs::vec_unique_count(units1) == 1L) {
      return(list(finding("refused", "samplyr_error_export_one_unit",
                          note_rows, stage = 1L, phase = 2L,
                          decided_by = "frame")))
    }
    return(list())
  }
  n2 <- first2$draw_spec$n
  if (!plain2 || !is.numeric(n2) || length(n2) != 1L ||
      !is_null(first2$draw_spec$frac)) {
    return(list())
  }
  if (n2 == 1) {
    return(list(finding("refused", "samplyr_error_export_one_unit", note2,
                        stage = 1L, phase = 2L)))
  }
  rows1 <- spec_stage_unit(df1, first1, stages1[1])$id
  if (plain1 && vctrs::vec_unique_count(rows1) > 1L &&
      n2 <= max(tabulate(match(rows1, unique(rows1))))) {
    return(list(finding(
      "risk", "samplyr_error_export_one_unit",
      paste0(note_rows, " when the draw falls in one of them")
    )))
  }
  list()
}

#' @noRd
twophase_estimator_rows <- function(design, traits, phase1, sample2 = NULL) {
  meta1 <- attr(phase1, "metadata")
  design1 <- get_design(phase1)
  stages1 <- get_stages_executed(phase1)
  df1 <- as.data.frame(phase1)
  lapply(estimator_catalog(), function(entry) {
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
      linearization = estimator_row(entry, c(twophase_linearization_findings(
        design, traits, phase1, design1, stages1, df1, sample2 = sample2
      ), phase1_one_unit_findings(design1, stages1, df1),
      phase2_one_unit_findings(design, design1, stages1, df1, sample2))),
      joint = estimator_row(entry, c(twophase_linearization_findings(
        design, traits, phase1, design1, stages1, df1, user_pps = TRUE,
        sample2 = sample2
      ), phase1_one_unit_findings(design1, stages1, df1),
      phase2_one_unit_findings(design, design1, stages1, df1, sample2))),
      random_groups = estimator_row(entry, random_groups_phase1_findings(
        phase1
      )),
      estimator_row(entry, list(finding(
        "refused", "samplyr_error_svrep_twophase_unsupported",
        "No replicate export exists for a two-phase sample",
        decided_by = "design"
      )))
    )
  })
}

#' Random groups of a later phase need phase 1 replicated with it
#' @noRd
random_groups_phase1_findings <- function(phase1) {
  if (has_multiple_replicates(phase1) &&
      isTRUE(attr(phase1, "metadata")$replicates_complete)) {
    return(list())
  }
  list(finding(
    "refused", "samplyr_error_random_groups_shared",
    "Replicates drawn from one realized phase 1 share its selection, so
     random groups would leave its variance out",
    decided_by = "frame"
  ))
}

#' Every export reading phase 1's rows refuses a modified phase 1
#'
#' Random groups read only the replicate totals, which a modification
#' applied to each replicate leaves estimable.
#' @noRd
modified_phase1_estimator_rows <- function(phase1) {
  lapply(estimator_catalog(), function(entry) {
    estimator_row(entry, if (identical(entry$key, "random_groups")) {
      random_groups_phase1_findings(phase1)
    } else {
      list(finding(
        "refused", "samplyr_error_modified_sample",
        "The phase-1 sample was modified after execution, and every export
         reading its rows as the whole first phase refuses it",
        phase = 1L, decided_by = "frame"
      ))
    })
  })
}

#' The two-phase linearization, in the order the export meets each condition
#' @noRd
twophase_linearization_findings <- function(design, traits, phase1, design1,
                                            stages1, df1, user_pps = FALSE,
                                            sample2 = NULL) {
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
  facts2 <- twophase_phase2_facts(design, traits, phase1)
  for (t in Filter(function(t) t$systematic, traits)) {
    if (fact_takes_one_per_pool(stage_fact(facts2, t$stage))) {
      next
    }
    out <- c(out, list(finding(
      "approximate", "samplyr_warning_systematic_variance",
      paste0("A systematic ", t$name, " of phase 2 has its variance",
             " approximated"),
      stage = t$stage, phase = 2L
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
  # A phase-1 primary unit with no rows is a zero row of phase 1, which
  # the export names under that stage's `on_empty = "warn"`.
  vanished <- export_empty_records(phase1, design1, stages1, vanished = TRUE)
  if (any(vapply(vanished, function(r) {
    identical(design1$stages[[r$stage]]$draw_spec$on_empty, "warn")
  }, logical(1)))) {
    out <- c(out, list(finding(
      "risk", "samplyr_warning_export_empty_parent",
      "A selected phase-1 primary unit has no rows and is exported as a
       zero row, so estimates need `na.rm = TRUE`",
      phase = 1L
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
  bridge <- twophase_bridge_finding(design, traits, design1, stages1, phase1,
                                    sample2 = sample2)
  if (identical(bridge$effect, "refused")) {
    return(c(out, list(bridge)))
  }
  if (!is_null(bridge)) {
    out <- c(out, list(bridge))
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
    samplyr_error_twophase_phase2_pps = function(e) e,
    samplyr_error_twophase_phase2_wr = function(e) e
  )
  if (inherits(joint, "samplyr_error_twophase_phase2_wr")) {
    return(c(out, refuse(
      "samplyr_error_twophase_phase2_wr",
      "Phase 2 is drawn with replacement, which survey's two-phase estimator
       cannot represent. Draw it without replacement",
      phase = 2L
    )))
  }
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
  # In the export's order: this refusal before the stage probabilities.
  spec1 <- export_stage_spec(df1, design1, stages1, phase = 1L)
  df1_carried <- with_phase_draw_columns(df1, phase1)
  if (twophase_subsamples_wr_units(df1_carried, spec1, design1, stages1,
                                   design, traits[[1]]$stage,
                                   complete = sample_realization_status(
                                     phase1
                                   )$ok)) {
    return(c(out, list(finding(
      "refused", "samplyr_error_twophase_wr_subsample",
      "Phase 2 subsamples inside the units phase 1's last stage drew with
       replacement, and the full two-phase covariance leaves out much of the
       variance between their hits. Draw those units as a stage of phase 1
       and phase 2 within each hit, which exports as one phase",
      decided_by = "frame"
    ))))
  }
  probs <- twophase_stage_probs_finding(traits, design1, stages1, df1,
                                         joint, phase1)
  if (!is_null(probs)) {
    return(c(out, list(probs)))
  }
  if (twophase_across_units(df1_carried, spec1, design, traits[[1]]$stage)) {
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
  if (length(vanished) > 0L && !user_pps) {
    out <- c(out, twophase_empty_unit_findings(sample2))
  }
  out
}

#' survey's two-phase design and a phase-1 zero row
#'
#' What survey cannot build around the zero row of an empty unit depends on
#' where phase 2's rows fall, so before phase 2 is drawn it is a risk. Once
#' drawn, the arguments survey would receive are built as the export builds
#' them, without calling survey, and the export's own check decides, so the
#' two agree.
#' @noRd
twophase_empty_unit_findings <- function(sample2) {
  class <- "samplyr_error_twophase_empty_unit"
  if (is_null(sample2)) {
    return(list(finding(
      "risk", class,
      "A selected phase-1 primary unit has no rows, and survey's two-phase
       design cannot represent its zero row when phase 2 lies in one other
       primary unit, takes every unit it can draw beside joint
       probabilities, or leaves the zero row a phase-2 stratum of its own",
      phase = 1L
    )))
  }
  design2 <- get_design(sample2)
  stages2 <- get_stages_executed(sample2)
  # A refusal of another class is reported by its own finding.
  prepared <- tryCatch(
    suppressMessages(suppressWarnings(twophase_svydesign_args(
      export_rows(sample2, design2, stages2, notify = FALSE),
      design2, stages2, attr(sample2, "metadata")$prev_phase,
      method = NULL, dots = list()
    ))),
    samplyr_error = function(e) NULL
  )
  if (is_null(prepared)) {
    return(list())
  }
  restated <- survey_twophase_args(prepared$args)
  refused <- tryCatch(
    check_twophase_zero_rows(restated$args, restated$subset),
    samplyr_error_twophase_empty_unit = function(e) e
  )
  if (!inherits(refused, class)) {
    return(list())
  }
  list(finding(
    "refused", class,
    "survey's two-phase design cannot represent the zero row of the empty
     phase-1 unit with the rows phase 2 drew",
    phase = 1L, decided_by = "frame"
  ))
}

#' Can phase 2 be joined back to phase 1?
#'
#' A phase drawn from the phase-1 sample alone carries each phase-1 row's
#' position and always joins. A design is assessed as drawn that way. A
#' phase-2 sample whose rows lack the positions, because a later stage drew
#' from a register of its own, joins on the compound identifier
#' [validate_frame()] checks: every identifier either phase declares that
#' the phase-1 sample carries. The export requires unique keys only among
#' the phase-1 rows phase 2 reaches, so a repeated key, such as a unit hit
#' twice by a multi-hit phase 1, refuses the export only when phase 2 draws
#' it. That is a risk of the phase-2 draw, and a refusal only when phase 2
#' takes every phase-1 row.
#' @noRd
twophase_bridge_finding <- function(design, traits, design1, stages1,
                                    phase1, sample2 = NULL) {
  if (is_null(sample2) || phase_rows_linked(
    as.data.frame(sample2), phase_row_column(phase1), nrow(phase1)
  )) {
    return(NULL)
  }
  phase1_ids <- survey_key_vars(design1, stages1, phase1)
  phase2_ids <- unlist(lapply(design$stages, function(s) s$clusters$vars))
  bridge <- intersect(unique(c(phase1_ids, phase2_ids)), names(phase1))
  if (length(bridge) == 0) {
    return(finding(
      "refused", "samplyr_error_twophase_bridge",
      "Neither phase declares a unit identifier the phase-1 sample carries,
       so the phases cannot be joined",
      decided_by = "frame"
    ))
  }
  keys1 <- as.data.frame(phase1)[, bridge, drop = FALSE]
  keys1 <- keys1[stats::complete.cases(keys1), , drop = FALSE]
  if (anyDuplicated(keys1) == 0) {
    return(NULL)
  }
  if (twophase_takes_all_rows(design, traits, phase1)) {
    return(finding(
      "refused", "samplyr_error_twophase_bridge",
      "The phase identifiers do not identify phase-1 rows uniquely, and
       phase 2 takes every phase-1 row",
      decided_by = "frame"
    ))
  }
  finding(
    "risk", "samplyr_error_twophase_bridge",
    "Phase 2 may draw a phase-1 row whose identifiers repeat, such as a unit
     hit more than once, and the export refuses that join"
  )
}

#' Does phase 2 take every phase-1 row?
#'
#' Read from the phase-2 design resolved against the phase-1 rows, as
#' [frame_summary()] does. A design that cannot be resolved there counts as
#' not taking every row.
#' @noRd
twophase_takes_all_rows <- function(design, traits, phase1) {
  facts <- twophase_phase2_facts(design, traits, phase1)
  !is_null(facts) && all(vapply(seq_along(design$stages), function(i) {
    isTRUE(stage_fact(facts, i)$census)
  }, logical(1)))
}

#' The phase-2 frame facts, read from the phase-1 rows
#' @noRd
twophase_phase2_facts <- function(design, traits, phase1) {
  tryCatch(
    frame_variance_facts(design, phase1, traits),
    samplyr_error = function(e) NULL
  )
}

#' The full two-phase covariance needs a probability for each stage
#' @noRd
twophase_stage_probs_finding <- function(traits, design1, stages1, df1,
                                         joint, phase1) {
  spec1 <- export_stage_spec(df1, design1, stages1, phase = 1L)
  ids1 <- spec_survey_ids(spec1, df1, synthesize_unclustered = TRUE,
                          prefix = "p1_")
  fpc1 <- survey_fpc_info(ids1$df, design1, stages1, ids1$stage_indices)
  covered1 <- spec_fpc_states_probabilities(
    spec1, ids1$stage_indices, fpc1$scale,
    phase_wr_stages_certain(phase1, design1, stages1)
  )
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
     \"simple\"` or `\"approx\"` use the weights instead, and overstate the
     variance when phase 2 subsamples inside the units phase 1 selected",
    decided_by = "frame"
  )
}
