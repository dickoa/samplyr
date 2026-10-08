#' Convert a tbl_sample to a survey design object
#'
#' Creates a [survey::svydesign()] object from a `tbl_sample`, using
#' the sampling design metadata (strata, clusters, weights, and
#' finite population corrections) captured during [execute()].
#'
#' @param x A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to [survey::svydesign()], or to
#'   [survey::twophase()] for a two-phase sample. In particular, you can
#'   pass `pps = survey::ppsmat(joint_matrix)` to supply exact joint
#'   inclusion probabilities instead of the default Brewer approximation
#'   (see Details). Every argument must be named, and its name must be one
#'   the receiving function accepts: `nest` and `method` follow the `...`
#'   and so are matched exactly, and a near miss such as `nes` is reported
#'   rather than forwarded. The design arguments themselves (`ids`, `strata`,
#'   `weights`, `probs`, `fpc`, `data`, and `subset` for a two-phase sample)
#'   are derived from the sample and cannot be given here.
#' @param nest If `TRUE` (the default), relabel cluster ids to enforce
#'   nesting within strata, which suits most complex survey designs. Passed
#'   to [survey::svydesign()]. It has no effect on a two-phase sample, which
#'   is exported with [survey::twophase()], and giving it there raises a
#'   warning.
#' @param systematic_variance What to do about the variance approximation
#'   used for systematic stages: simple random sampling for `systematic`,
#'   Brewer's for `pps_systematic`. `"warn"` (default) applies it and warns
#'   once per call, naming every affected stage and its approximation.
#'   `"approximate"` applies it silently, for a caller who has acknowledged
#'   it, and `"error"` refuses. Census stages are exempt, since a stage that
#'   took everything within reach contributes no variance, and so is a
#'   `pps_systematic` first stage exported with a caller-supplied `pps`
#'   object. The choice, the affected stages and each stage's approximation
#'   are recorded in the `"samplyr_systematic_variance"` attribute of the
#'   result. [as_svrepdesign()] takes the same argument for its own
#'   approximation of these stages.
#' @param method For two-phase samples, the variance method passed to
#'   [survey::twophase()]. One of `"full"`, `"approx"`, or `"simple"`.
#'   This argument is only accepted for two-phase samples.
#'
#' @return A `survey.design2` object for single-phase and multi-stage samples,
#'   or a `twophase`/`twophase2` object for two-phase samples.
#'
#' @details
#' Every executed stage contributes one `ids` term, one `fpc` term and, when
#' stratified, one `strata` term, so [survey::svydesign()] represents the
#' multi-stage structure in its linearization (Sarndal et al. 1992, ch.
#' 4.3):
#'
#' - **Cluster ids** (`ids`): the `cluster_by()` variable of a clustered
#'   stage. A stage clustered by several variables, which execution treats
#'   as one cluster id, gets one synthesized interaction column, because
#'   [survey::svydesign()] reads each formula term as a separate stage. A
#'   final unclustered stage gets a synthesized row-identity column so that
#'   its sampling variance is represented. A WR or PMR stage uses its
#'   `.draw_k` column, and survey treats each occurrence as independent for
#'   Hansen-Hurwitz variance estimation. This is exact for WR and the
#'   documented approximation for PMR.
#' - **Strata** (`strata`): aligned with `ids`. A stage stratified by
#'   several variables exports their cross-classification as one
#'   synthesized column, since survey silently ignores extra variables
#'   within a stage's term. Trailing unstratified stages are omitted, and
#'   unstratified stages before a stratified stage get a constant
#'   placeholder column.
#' - **Weights** (`weights`): the `.weight` column, the product of the
#'   per-stage weights described in [sample-columns].
#' - **FPC** (`fpc`): aligned with `ids`. [survey::svydesign()] requires
#'   every FPC term on the same scale, so one of two encodings is used:
#'   - **Count scale** (designs without unequal-probability WOR
#'     stages): `.fpc_k`, the stratum population count \eqn{N_h}, for
#'     equal-probability WOR stages, and a synthetic `Inf` column (no
#'     correction, Hansen-Hurwitz variance) for WR/PMR stages.
#'   - **Fraction scale** (multi-stage designs with a PPS WOR,
#'     balanced, or custom WOR stage): every WOR stage passes its
#'     per-unit stage sampling fraction
#'     \eqn{1 / w_k = \pi_k}{1/w_k = pi_k}, and WR/PMR stages pass 0 (no
#'     correction). A multi-stage design with a random-size Poisson stage is
#'     refused by linearization export.
#'
#'   A single-stage PPS WOR design passes \eqn{\pi_i}{pi_i} directly,
#'   which survey interprets as inclusion probabilities.
#'
#' For a two-stage stratified-cluster design with a final element stage,
#' the exported call is equivalent to:
#' ```r
#' survey::svydesign(
#'   ids     = ~ ea_id + .id_2,       # stage-1 clusters, stage-2 elements
#'   strata  = ~ region,              # stage-1 strata
#'   weights = ~ .weight,             # product of per-stage weights
#'   fpc     = ~ .fpc_pi_1 + .fpc_f_2,  # per-stage sampling fractions
#'   data    = sample,
#'   nest    = TRUE
#' )
#' ```
#'
#' ## Multi-stage designs
#'
#' Exactness depends on the variance treatment available for each method
#' ([variance-estimation]). A design whose first stage is a census of PSUs
#' correctly attributes all variance to the later stages. Executing stages
#' separately, as `stage1 <- execute(design, psu_frame, stages = 1)` then
#' `sample <- execute(stage1, listing_frame)`, still gives one multi-stage
#' design, exported with [survey::svydesign()].
#'
#' An unclustered element-sampling stage *followed by further stages* is
#' not nested cluster sampling, because the later selections are
#' conditional on the realized element sample, which is phase sampling.
#' [execute()] refuses it (`samplyr_error_stage_parent_id`). Express it as a
#' two-phase sample instead: execute the element stage under its first-phase design, then a
#' new second-phase design with that sample as its frame.
#'
#' ## Two-phase samples and waves
#'
#' A *new* phase-2 `sampling_design` executed with the phase-1 `tbl_sample`
#' as its frame, as in `phase2 <- execute(design2, phase1)`, records a
#' previous-phase link, and `as_svydesign()` calls [survey::twophase()].
#'
#' A materialized wave, `execute(master, wave = t)`, is the other two-phase
#' case. Its second phase is the panel activation: within a frozen block the
#' master took a simple random sample without replacement of the realized
#' quota, so the blocks are the phase-2 strata and their sizes the phase-2
#' population counts. The master is retained as the first phase and supplies
#' the rows the wave did not keep. Columns added to the wave for analysis
#' are carried into the exported design. Wave or phase-2 analysis columns
#' replace same-named first-phase columns and are missing on unsampled
#' rows. Columns absent from the wave remain available from the master, so
#' dropping a wave column does not erase the master's measurements. Design
#' identifiers, strata and internal sampling columns keep their recorded
#' meanings. Keep earlier measurements under separate names if both are
#' needed.
#'
#' On a stratified master whose stratum means differ strongly relative to
#' the variation within them, the two-phase variance of a *total* can be
#' negative, and [survey::svytotal()] returns `NaN` with base R's
#' `sqrt(v): NaNs produced`. That is [survey::twophase()]'s exact estimator,
#' computed only when an estimator is called, so samplyr cannot intercept
#' it. `method = "approx"` gives a finite standard error. A mean is
#' unaffected.
#'
#' A clustered first phase needs its second phase drawn inside each of its
#' units, with `stratify_by(<unit>)`, or by taking whole phase-1 units
#' before sampling inside them. In a simulation both gave 0.93 to 1.03 of
#' the true variance and never a negative one. When phase 2 draws smaller
#' units across the phase-1 clusters, the estimator is right over repeated
#' samples but not in one ([variance-estimation] has the measured error),
#' and the export warns (`samplyr_warning_twophase_across_units`). The
#' weights are exact either way.
#'
#' [survey::twophase()] takes no `pps` for the first phase, so a two-phase
#' sample or wave whose phase 1 was drawn with unequal probabilities without
#' replacement is refused (`samplyr_error_twophase_phase1_pps`,
#' `samplyr_error_wave_phase1_pps`). So are `cube`, first-stage
#' `pps_poisson` and the spatial methods, which are unsupported at either
#' phase. Equal-probability first phases export, stratified, clustered,
#' multistage and with-replacement alike. No two-phase sample, a wave
#' included, has a replicate export. The weights of a refused sample are
#' exact, so totals and means are right. For a variance, an ultimate-cluster
#' approximation treats phase 1's units as drawn with replacement and
#' carries everything below in them. It is conservative, by the margins
#' [variance-estimation] reports, and much more so when phase 2 was drawn
#' across the phase-1 units:
#'
#' ```r
#' survey::svydesign(ids = ~ea_id, strata = ~region, weights = ~.weight,
#'                   data = as.data.frame(phase2))
#' ```
#'
#' ## Modified samples and domain analysis
#'
#' `as_svydesign()` raises an error on a `tbl_sample` whose row set changed
#' after [execute()] (rows removed by [dplyr::filter()] or `[`, added, or
#' duplicated by a join) or whose internal design columns (`.weight`,
#' `.weight_k`, `.fpc_k`, ...) were overwritten, dropped, or renamed. The
#' sample is verified against an integrity record stored at execution (row
#' count and a hash of the weights, design metadata, and strata and cluster
#' columns), so changes the dplyr hooks cannot see (base assignment,
#' `rbind()`, vctrs operations, third-party verbs) are also caught, and an
#' overwrite that left every value identical passes. Row reordering,
#' one-to-one joins, and adding ordinary data columns do not mark the
#' sample. One complete replicate extracted from a replicated execution
#' (`filter(.replicate == r)`) is verified against the execution metadata
#' and remains supported.
#'
#' Dropping out-of-domain rows before conversion is not domain estimation.
#' The point estimate can agree, but its variance estimate is generally
#' wrong, often too small, because the domain sample size is random under
#' the design. Convert the full sample first and then subset the design:
#' ```r
#' svy <- as_svydesign(sample)
#' survey::svymean(~y, subset(svy, domain))
#' # or with srvyr:
#' as_survey_design(sample) |> filter(domain) |> summarise(...)
#' ```
#'
#' ## Variance by selection method
#'
#' Which estimator each selection method reaches, how exact it is, and which
#' designs are refused are in [variance-estimation].
#'
#' `pps` is read as follows. `"brewer"` is the default treatment and keeps
#' every stage. `FALSE` states that no stage is PPS, so it is refused on a
#' design with one. `"overton"`, [survey::HR()] and the matrix objects
#' ([survey::ppsmat()], [survey::poisson_sampling()]) are single-stage in
#' survey, so a multi-stage sample is exported at stage 1 with a warning. A
#' matrix object is indexed by row, so it also needs one row per stage-1
#' unit, and a sample with several is refused
#' (`samplyr_error_pps_rows_per_psu`). Any other value is refused
#' (`samplyr_error_pps_argument`). A two-phase or wave export takes no `pps`,
#' because [survey::twophase()] does not apply it to the phase-2 variance
#' (`samplyr_error_twophase_phase2_pps`). A phase 2 of one stage drawn with
#' `pps_sampford`, `pps_cps`, `pps_brewer`, `pps_sps` or `pps_pareto` is
#' exported with its joint inclusion probabilities, computed on the phase-1
#' sample, and needs `method = "full"`. Any other unequal-probability,
#' balanced or spatial phase 2 is refused with the same class.
#'
#' ## A shared estimation weight
#'
#' A sample from [share_weights()] carries weights for a population other
#' than the one that was selected, so it is exported as its **source-target
#' contributions**: one row per link, weighted by the recorded coefficient
#' times the source unit's design weight. This gives the generalized weight
#' share total exactly for any link structure, provided every selected
#' source unit has at least one contribution. A selected source unit with
#' no link must still count in the variance, which contribution rows cannot
#' do without inventing a target row, so this route refuses it and names
#' [as_svrepdesign()] instead. The result has more rows than the
#' transformation returned, but no estimate changes: a total sums the same
#' terms, and a mean's denominator is the estimated target population size
#' either way.
#'
#' An unequal-probability or random-size source design is also refused
#' here, because its variance comes from a structure indexed by the
#' source sample's rows, which the contribution rows no longer are. Use
#' `as_svrepdesign()`, which replicates the source design and applies the
#' sharing inside every replicate.
#'
#' The `survey` package is required but not imported. It must be
#' installed to use this function.
#'
#' @references
#' Sarndal, C.-E., Swensson, B. and Wretman, J. (1992). *Model
#' Assisted Survey Sampling*. Springer.
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
#' # Stratified sample -> survey design
#' sample <- sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 42)
#'
#' svy <- as_svydesign(sample)
#' survey::svymean(~households, svy)
#'
#' # Two-stage sample: PPS selection of EAs, then households from a listing
#' selected <- sampling_design() |>
#'   add_stage() |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = households) |>
#'   add_stage() |>
#'     draw(n = 8) |>
#'   execute(bfa_eas, stages = 1, seed = 2025)
#' listing <- selected |>
#'   as.data.frame() |>
#'   dplyr::reframe(hh_id = seq_len(households), .by = ea_id)
#' sample <- execute(selected, listing, seed = 2026)
#'
#' # Brewer's variance approximation at the PPS stage
#' svy <- as_svydesign(sample)
#'
#' # A joint-probability matrix instead, for a single-stage Sampford sample,
#' # whose matrix is exact. A matrix needs one row per stage-1 unit.
#' sampford <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 5, method = "pps_sampford", mos = households) |>
#'   execute(bfa_eas, seed = 2025)
#' jip <- joint_expectation(sampford, bfa_eas)
#' svy_joint <- as_svydesign(sampford, pps = survey::ppsmat(jip[[1]]))
#' survey::svytotal(~population, svy_joint)
#'
#' @seealso [execute()] for producing tbl_sample objects,
#'   [survey::svydesign()] for the underlying function,
#'   [as_survey_design.tbl_sample] for converting directly to a srvyr `tbl_svy`,
#'   `as_svrepdesign()` for replicate-weight export,
#'   [as_svydesign.frame_stack()] for overlapping frames
#'
#' @family survey export
#' @export
as_svydesign <- function(x, ...) {
  UseMethod("as_svydesign")
}

#' @noRd
survey_phase_info <- function(sample) {
  metadata <- attr(sample, "metadata")
  prev_phase <- metadata$prev_phase
  has_prev_sample <- is.list(prev_phase) && is_tbl_sample(prev_phase$sample)
  prev_prev <- if (has_prev_sample) {
    attr(prev_phase$sample, "metadata")
  } else {
    NULL
  }
  has_three_phase <- has_prev_sample &&
    is.list(prev_prev) &&
    !is_null(prev_prev$prev_phase)
  is_twophase <- has_prev_sample && !has_three_phase

  list(
    prev_phase = prev_phase,
    has_prev_sample = has_prev_sample,
    has_three_phase = has_three_phase,
    is_twophase = is_twophase
  )
}

#' What to tell someone whose shared weights came from a two-phase sample
#'
#' Neither export route takes it, so neither may name the other. The one
#' thing that does work is sharing from the first phase, whose own export is
#' single-phase.
#' @noRd
shared_twophase_source_advice <- function() {
  c(
    "x" = "The sample its weights were shared from is two-phase.",
    "i" = "A shared weight linearizes as the contributions of the source
           units it came from, and those rows carry one selection's strata
           and units, which a two-phase sample does not have.",
    "i" = "Share weights from the first-phase sample instead, or from the
           master if the second phase is a wave activation."
  )
}

#' Refuse a sample with more phases than the route can carry
#'
#' @param advice Bullets naming what to do instead, and `class` the caller's
#'   own condition class. Both are the caller's because what to do instead
#'   differs by route: the default names the linearized export, which is the
#'   answer for a sample being converted to replicate weights and is not the
#'   answer for a sample whose weights were shared from a two-phase source,
#'   where the linearized export is what raised the refusal.
#' @noRd
survey_validate_phase_support <- function(
  sample,
  allow_twophase = TRUE,
  fn_name = "as_svydesign",
  advice = NULL,
  class = "samplyr_error_svrep_twophase_unsupported",
  call = rlang::caller_env()
) {
  phase_info <- survey_phase_info(sample)

  if (phase_info$has_three_phase) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} only supports up to two-phase samples.",
        "i" = "This sample has more than two phases.",
        "i" = "Convert phases separately or collapse phases before exporting."
      ),
      class = "samplyr_error_survey_multiphase_unsupported",
      call = call
    )
  }

  if (!allow_twophase && phase_info$is_twophase) {
    # Pointing to the linearized export is advice only where it exports.
    pps1 <- phase1_pps_methods(
      phase_info$prev_phase,
      kinds = c("pps_wor", "unsupported")
    )
    default_advice <- if (length(pps1) > 0L) {
      c(
        "i" = "{.fn as_svydesign} refuses it too: phase 1 was drawn with
               {.val {pps1}}, which {.fn survey::twophase} has no variance
               treatment for at phase 1.",
        "i" = "The weights in {.field .weight} are exact for totals and
               means. For a variance, see the ultimate-cluster approximation
               in {.help as_svydesign}."
      )
    } else {
      c("i" = "Use {.fn as_svydesign} for two-phase linearization export.")
    }
    abort_samplyr(
      c(
        "{.fn {fn_name}} does not support two-phase samples.",
        advice %||% default_advice
      ),
      class = class,
      call = call
    )
  }

  check_wave_carries_master(sample, phase_info, fn_name, call = call)
  phase_info
}

#' Unequal-probability methods drawn without replacement at phase 1
#'
#' [survey::twophase()] takes no `pps` specification for its first phase,
#' so these have no linearization route through it. `kinds` widens the
#' question to other variance families, such as `"unsupported"` for the
#' spatial and bounded methods, which have none at any phase. Read from the phase-1
#' design before anything else about the export, so that a linkage problem
#' downstream of it is not reported in its place.
#' @noRd
phase1_pps_methods <- function(prev_phase, kinds = "pps_wor") {
  if (is_null(prev_phase)) {
    return(character(0))
  }
  design1 <- prev_phase$design %||% get_design(prev_phase$sample)
  stages1 <- prev_phase$stages %||% get_stages_executed(prev_phase$sample)
  pps <- vapply(stages1, function(i) {
    survey_stage_kind(design1$stages[[i]]$draw_spec) %in% kinds
  }, logical(1))
  unique(vapply(stages1[pps], function(i) {
    design1$stages[[i]]$draw_spec$method
  }, character(1)))
}

## The systematic variance approximation

# One systematic sample does not identify its design variance. The SRSWOR
# approximation can fail under frame periodicity. Generic replicates also miss
# the original order and random start. Both export routes warn once.

#' Systematic stages whose variance is being approximated
#'
#' Both systematic methods: one systematic sample does not identify its
#' design variance, whatever formula stands in for it. `pps_systematic` is
#' exported with Brewer's approximation, which is as blind to frame order as
#' the simple random sampling one: on a frame whose period matches the
#' interval it gave 0.003 of the true variance, with intervals covering 9 %
#' of the time. A census stage is excluded, since a stage that took
#' everything within reach contributes no variance for the approximation to
#' get wrong.
#' @noRd
systematic_approximated_stages <- function(design, stages_executed, df,
                                           phase = NULL) {
  affected <- vapply(stages_executed, function(stage_idx) {
    method <- design$stages[[stage_idx]]$draw_spec$method
    if (!method %in% c("systematic", "pps_systematic")) {
      return(FALSE)
    }
    # A systematic draw of one PSU is a draw proportional to size.
    if (draws_one_per_zone(design$stages[[stage_idx]]$draw_spec)) {
      return(FALSE)
    }
    # A census has no variance to approximate.
    !spec_stage_census(df, stage_idx)
  }, logical(1))

  lapply(stages_executed[affected], function(stage_idx) {
    label <- design$stages[[stage_idx]]$label
    list(
      stage = stage_idx,
      phase = phase,
      method = design$stages[[stage_idx]]$draw_spec$method,
      name = if (is_null(label)) {
        paste("stage", stage_idx)
      } else {
        paste0("stage ", stage_idx, " (", label, ")")
      }
    )
  })
}

#' Tell the caller once, unless they have said they know
#'
#' The two exports approximate a systematic stage by different means, so the
#' condition names the estimator actually in use. What they share is the
#' cause, the argument that settles it, and the condition class.
#' @noRd
check_systematic_variance <- function(
  stages,
  choice,
  approximation = c("srswor", "generic_replicates"),
  fn_name = "as_svydesign",
  call = caller_env()
) {
  if (length(stages) == 0 || identical(choice, "approximate")) {
    return(invisible(NULL))
  }
  approximation <- with_error_class(
    rlang::arg_match(approximation),
    "samplyr_error_internal"
  )
  named <- vapply(stages, function(s) {
    if (is_null(s$phase)) s$name else paste0(s$name, ", phase ", s$phase)
  }, character(1))

  estimator <- if (identical(approximation, "srswor")) {
    per_stage <- vapply(stages, function(s) {
      what <- if (identical(s$method, "pps_systematic")) {
        "Brewer's approximation"
      } else {
        "a simple random sampling approximation"
      }
      paste(what, "for", s$name)
    }, character(1))
    c(
      "x" = "{.fn {fn_name}} is using {per_stage}.",
      "i" = "Frame ordering or periodicity can make standard errors far too
             small or too large. A frame whose period matches the sampling
             interval has been measured at 0.0006 of the true variance for
             {.val systematic} and 0.003 for {.val pps_systematic}, with 95%
             intervals covering about 10% of the time. See
             {.topic variance-estimation}."
    )
  } else {
    c(
      "x" = "{.fn {fn_name}} is building generic replicate weights for
             {cli::qty(length(named))}{?it/them}. They resample the realized
             sample and do not reproduce the systematic selection mechanism
             or its dependence on frame order.",
      "i" = "Frame ordering or periodicity can make the resulting standard
             errors too small or too large. The size of that gap has been
             measured for the linearization export only: see
             {.topic variance-estimation}."
    )
  }

  bullets <- c(
    "{cli::qty(length(named))}{?A stage/Stages} of this design used
     systematic selection: {named}.",
    estimator,
    "i" = "Use {.code systematic_variance = \"approximate\"} to accept the
           approximation, or {.code \"error\"} to refuse it."
  )

  if (identical(choice, "error")) {
    abort_samplyr(
      bullets,
      class = "samplyr_error_systematic_variance",
      call = call
    )
  }
  cli_warn(bullets, class = "samplyr_warning_systematic_variance")
  invisible(NULL)
}

#' Leave the decision on the exported object
#'
#' A survey design object carries no sign of which variance model produced it,
#' so what was approximated and whether the caller had said they knew is
#' recorded where a later reader can find it.
#' @noRd
record_systematic_variance <- function(result, stages, choice, approximation) {
  # Each systematic method is approximated differently.
  per_stage <- vapply(stages, function(s) {
    if (identical(approximation, "srswor") &&
        identical(s$method, "pps_systematic")) {
      "brewer"
    } else {
      approximation
    }
  }, character(1))
  attr(result, "samplyr_systematic_variance") <- list(
    approximation = if (length(stages) > 0) per_stage else NULL,
    stages = vapply(stages, function(s) s$name, character(1)),
    acknowledged = choice
  )
  result
}

## Export of a materialized wave

# A wave is a two-phase sample. Its activation identifiers, strata, and counts
# come from the frozen assignment. Each block activates its quota by SRSWOR.

#' @noRd
survey_is_activation <- function(phase_info) {
  identical(phase_info$prev_phase$transition, "panel_activation")
}

#' A wave must carry the master it was activated from
#'
#' [survey::twophase()] builds the first-phase design from every phase-1 row
#' and marks the active ones with `subset`, so the master's rows are needed
#' and nothing can reconstruct the units the wave did not keep. Every export
#' route checks it through `survey_validate_phase_support()`: without the
#' link, a replicate route would read the wave as a single-phase sample and
#' leave the activation out of the variance.
#' @noRd
check_wave_carries_master <- function(
  x,
  phase_info,
  fn_name,
  call = caller_env()
) {
  metadata <- attr(x, "metadata")
  if (is_null(metadata$wave) || survey_is_activation(phase_info)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn {fn_name}} needs the master this wave was activated from.",
      "x" = "This sample realizes wave {metadata$wave$wave} but carries no
             link to it.",
      "i" = "The master is the first phase of the export, so materialize the
             wave again from it:
             {.code execute(master, wave = {metadata$wave$wave})}."
    ),
    class = "samplyr_error_wave_no_master",
    call = call
  )
}

#' Which master designs an activation can be exported on top of
#'
#' [survey::twophase()] takes no `pps` specification at phase 1, so a master
#' whose units carry unequal inclusion probabilities has no exact
#' linearization there. Dropping the specification would return the
#' with-replacement approximation without saying so.
#' @noRd
check_activation_phase1_supported <- function(
  design,
  stages_executed,
  fpc,
  call = caller_env()
) {
  kinds <- vapply(
    stages_executed,
    function(i) survey_stage_kind(design$stages[[i]]$draw_spec),
    character(1)
  )

  if (any(kinds == "unsupported")) {
    methods <- vapply(
      stages_executed[kinds == "unsupported"],
      function(i) design$stages[[i]]$draw_spec$method,
      character(1)
    )
    abort_samplyr(
      c(
        "Cannot export a wave of a master drawn with
         method{?s} {.val {methods}}.",
        "i" = "No linearization variance estimator is available for this
               method and its declared constraints, at either phase.",
        "i" = "Export the master itself with
               {.code as_svrepdesign(type = \"subbootstrap\")} for a
               bootstrap approximation of its own variance."
      ),
      class = "samplyr_error_custom_random_wor_export",
      call = call
    )
  }

  if (identical(fpc$scale, "count")) {
    return(invisible(NULL))
  }

  methods <- vapply(
    stages_executed,
    function(i) design$stages[[i]]$draw_spec$method,
    character(1)
  )
  abort_samplyr(
    c(
      "Cannot export a wave of a master drawn with
       method{?s} {.val {unique(methods)}}.",
      "x" = "{.fn survey::twophase} takes no {.arg pps} specification at
             phase 1, so the master's unequal inclusion probabilities have no
             exact treatment there.",
      "i" = "The activation weights in {.field .weight} are exact and
             estimate totals correctly; it is the variance that has no exact
             form.",
      "i" = "Export the master itself for its own exact variance:
             {.code as_svydesign(master)}."
    ),
    class = "samplyr_error_wave_phase1_pps",
    call = call
  )
}

#' The phase-1 sample must be the realization the activation was computed on
#'
#' Two executions of one design produce the same `.sample_id` values, because
#' those are row positions, so a substituted master of the same shape would
#' pass every structural check while supplying different non-active rows.
#' @noRd
check_wave_master_identity <- function(metadata, master, call = caller_env()) {
  recorded <- metadata$wave$master_digest
  if (
    is_null(recorded) || identical(recorded, wave_source_digest(master))
  ) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "This wave was activated from a different realization.",
      "x" = "The sample held as its first phase is not the one wave
             {metadata$wave$wave} was computed against.",
      "i" = "Materialize the wave again from the master it belongs to."
    ),
    class = "samplyr_error_wave_master_mismatch",
    call = call
  )
}

#' Phase-2 columns of an activation, from the frozen assignment record
#'
#' The block a unit sits in, the block's size in assignment units, and the
#' conditional probability the recorded quotas give it. `active` comes from
#' the wave's own rows rather than from recomputing the panel test, so the
#' exported subset is the sample the user holds.
#' @noRd
activation_phase2_columns <- function(df1, x, metadata, call = caller_env()) {
  # Validate the recorded assignment law before deriving design columns.
  record <- prepare_panel_record(
    metadata$panel_assignment,
    "A survey export",
    call = call
  )
  wave_pools <- metadata$wave$pools

  cols <- list(
    unit = free_column_name(df1, ".activation_unit"),
    block = free_column_name(df1, ".activation_block"),
    block_n = free_column_name(df1, ".activation_block_N"),
    prob = free_column_name(df1, ".activation_prob"),
    weight = free_column_name(df1, ".activation_weight"),
    active = free_column_name(df1, ".active"),
    prob1 = free_column_name(df1, ".prob_1")
  )

  keys <- make_group_key(df1, record$key_vars)
  block <- rep(NA_character_, nrow(df1))
  block_n <- rep(NA_real_, nrow(df1))
  prob <- rep(NA_real_, nrow(df1))

  for (p in seq_along(record$pools)) {
    pool <- record$pools[[p]]
    at <- match(keys, pool$keys)
    rows <- which(!is.na(at))
    b <- rep(seq_along(pool$blocks), pool$blocks)[at[rows]]
    block[rows] <- paste(p, b, sep = ".")
    block_n[rows] <- pool$blocks[b]
    prob[rows] <- wave_pools[[p]]$probability[b]
  }

  if (anyNA(block)) {
    abort_samplyr(
      c(
        "The assignment record does not cover every row of the master.",
        "x" = "{sum(is.na(block))} row{?s} belong{?s/} to no assignment
               pool.",
        "i" = "Materialize the wave again from the master."
      ),
      class = "samplyr_error_wave_master_mismatch",
      call = call
    )
  }

  df1[[cols$unit]] <- group_ids(df1, record$key_vars)
  df1[[cols$block]] <- block
  df1[[cols$block_n]] <- block_n
  df1[[cols$active]] <- df1$.sample_id %in% x$.sample_id
  df1[[cols$prob]] <- ifelse(df1[[cols$active]], prob, NA_real_)
  df1[[cols$weight]] <- ifelse(df1[[cols$active]], 1 / prob, NA_real_)
  df1[[cols$prob1]] <- 1 / df1$.weight

  check_activation_take(df1, cols, call = call)

  list(df = df1, cols = cols)
}

#' The realized take must be the take the record froze
#'
#' Counting the active assignment units of each block reproduces `a_bt`, so a
#' wave whose rows do not belong to the master it names is caught here rather
#' than silently exported against the wrong first phase.
#' @noRd
check_activation_take <- function(df1, cols, call = caller_env()) {
  units <- !duplicated(df1[[cols$unit]])
  by_block <- split(
    data.frame(
      active = df1[[cols$active]][units],
      n = df1[[cols$block_n]][units],
      p = df1[[cols$prob]][units]
    ),
    df1[[cols$block]][units]
  )
  bad <- vapply(
    by_block,
    function(b) {
      take <- sum(b$active)
      expected <- unique(b$p[b$active])
      length(expected) > 1L ||
        (take > 0L && !isTRUE(all.equal(take / b$n[1], expected)))
    },
    logical(1)
  )
  if (any(bad)) {
    abort_samplyr(
      c(
        "The active rows do not match the activation the master recorded.",
        "x" = "{sum(bad)} block{?s} {?has/have} a realized take that is not
               the frozen quota.",
        "i" = "This wave and the master it carries describe different
               executions. Materialize it again from the master."
      ),
      class = "samplyr_error_wave_master_mismatch",
      call = call
    )
  }
  invisible(NULL)
}

#' Can a phase's inclusion probabilities be read off its correction?
#'
#' [survey::twophase()] derives each phase's weights from its finite
#' population correction when no probability is given, so the correction has
#' to state every stage's probability. It does not when a stage contributed no
#' term, which is what an unclustered stage does at a phase (it carries no
#' identifier there), and it does not when a term is infinite: a
#' with-replacement stage, or a later unsupported one, on the count scale,
#' and an equal-probability stage with no recorded population count. In both
#' cases the phase's own weight is the exact probability and is passed
#' instead. On the fraction scale "no correction" is written as a zero, which
#' survey reads as a probability, so it counts as stated.
#' @param scale The phase's FPC scale from `survey_fpc_info()`.
#' @noRd
spec_fpc_states_probabilities <- function(spec, id_stage_indices, scale) {
  # A lone element stage contributes one probability term.
  covered <- if (length(id_stage_indices) == 0) {
    spec$stages[1]
  } else {
    id_stage_indices
  }
  if (length(covered) != length(spec$stages)) {
    return(FALSE)
  }
  on_count <- identical(scale, "count")
  all(vapply(covered, function(stage_idx) {
    entry <- spec$stage[[as.character(stage_idx)]]
    switch(
      entry$kind,
      wr = !on_count,
      unsupported = stage_idx == spec$stages[1] || !on_count,
      equal_wor = !on_count || !is_null(entry$pop_count),
      TRUE
    )
  }, logical(1)))
}

#' The `method = "full"` covariance needs one probability per stage
#'
#' Shared with the generic two-phase path: a single overall probability makes
#' survey fail inside covariance construction rather than report anything.
#' @noRd
check_twophase_stage_probs <- function(
  n_id_stages,
  use_weights,
  fpc_covers_stages,
  call = caller_env()
) {
  if (use_weights || fpc_covers_stages || n_id_stages <= 1) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "This two-phase design cannot be exported with
       {.code method = \"full\"}.",
      "x" = "It has {n_id_stages} identifier stages but no finite
             population correction to derive their per-stage
             probabilities from.",
      "i" = "Use {.code method = \"simple\"} or {.code \"approx\"}, which
             use the design weights directly."
    ),
    class = "samplyr_error_twophase_stage_probs",
    call = call
  )
}

#' Does the activation retain every unit with certainty?
#'
#' Read from the realized per-block activation probabilities rather than from
#' the pool classes, so it covers every route to an identity: all pools
#' permanent, and a wave that activates every panel. A record with no pools
#' states nothing and is not treated as an identity.
#' @noRd
activation_is_identity <- function(metadata) {
  pools <- metadata$wave$pools
  if (is_null(pools) || length(pools) == 0L) {
    return(FALSE)
  }
  probability <- unlist(lapply(pools, function(pool) pool$probability))
  length(probability) > 0L && all(is_certainty_probability(probability))
}

#' Export a materialized wave through survey::twophase()
#' @noRd
build_activation_twophase <- function(
  x,
  prev_phase,
  method,
  dots,
  call = caller_env()
) {
  rlang::local_error_call(call)
  master <- prev_phase$sample
  design1 <- prev_phase$design %||% get_design(master)
  stages1 <- prev_phase$stages %||% get_stages_executed(master)
  metadata <- attr(x, "metadata")

  # Identity activation has no second-phase variance.
  if (is_null(metadata$panel_assignment) || activation_is_identity(metadata)) {
    return(build_singlephase_svydesign(
      x,
      dots = dots,
      nest = TRUE,
      relax_pps_for_bootstrap = FALSE
    ))
  }

  check_wave_master_identity(metadata, master, call = call)
  check_export_primary_units(master, design1, stages1, call = call)

  df1 <- as.data.frame(master)

  # Copy measurements first so their names join collision avoidance.
  carried <- setdiff(names(x), protected_sample_cols(df1, design1, stages1))
  if (length(carried) > 0) {
    at <- match(df1$.sample_id, x$.sample_id)
    for (nm in carried) {
      df1[[nm]] <- as.data.frame(x)[[nm]][at]
    }
  }

  spec1 <- export_stage_spec(df1, design1, stages1, phase = 1L)
  id_info <- spec_survey_ids(
    spec1,
    df1,
    synthesize_unclustered = TRUE,
    prefix = "p1_",
    call = call
  )
  df1 <- id_info$df
  strata1 <- spec_survey_strata(
    spec1,
    df1,
    id_stage_indices = id_info$stage_indices,
    prefix = "p1_"
  )
  df1 <- strata1$df
  fpc1 <- survey_fpc_info(df1, design1, stages1, id_info$stage_indices)
  df1 <- fpc1$df

  check_activation_phase1_supported(design1, stages1, fpc1, call = call)

  phase2 <- activation_phase2_columns(df1, x, metadata, call = call)
  df1 <- phase2$df
  cols <- phase2$cols

  check_twophase_phase2_family(NULL, dots[["pps"]], call = call)
  pps_arg <- NULL
  dots[["pps"]] <- NULL

  use_weights <- !is_null(method) && method %in% c("approx", "simple")
  n_id_stages <- max(length(id_info$id_vars), 1L)
  # Phase 2 always states its correction, so only the master can fail to.
  fpc_covers_stages <- spec_fpc_states_probabilities(
    spec1, id_info$stage_indices, fpc1$scale
  )
  check_twophase_stage_probs(
    n_id_stages,
    use_weights,
    fpc_covers_stages,
    call = call
  )

  probs_arg <- if (use_weights || fpc_covers_stages) {
    NULL
  } else {
    list(
      survey_formula_from_vars(cols$prob1),
      survey_formula_from_vars(cols$prob)
    )
  }
  weights_arg <- if (use_weights) {
    list(
      stats::as.formula("~.weight"),
      survey_formula_from_vars(cols$weight)
    )
  } else {
    NULL
  }

  args <- c(
    list(
      id = list(
        survey_ids_formula(id_info$id_vars),
        survey_formula_from_vars(cols$unit)
      ),
      strata = list(
        strata1$formula,
        survey_formula_from_vars(cols$block)
      ),
      probs = probs_arg,
      weights = weights_arg,
      fpc = list(fpc1$formula, survey_formula_from_vars(cols$block_n)),
      subset = survey_formula_from_vars(cols$active),
      data = df1,
      method = method,
      pps = pps_arg
    ),
    dots
  )
  result <- do.call(survey::twophase, args)
  result$call <- survey_export_call("twophase", args)
  result
}

#' Refuse a phase 2 that survey's two-phase estimator cannot represent
#'
#' `survey::twophase()` builds the phase-2 variance of an unequal-probability
#' selection from a joint probability matrix only. A Brewer specification is
#' ignored there (a census phase 1 then gave a phase-2 variance 1.9 times the
#' direct one), `method = "approx"` and `"simple"` drop `pps` altogether, and
#' a user's `pps` value (the spelling that would name Brewer) gave a wrong
#' total and a zero variance.
#'
#' Phase 2's frame is the phase-1 sample, so samplyr computes that matrix
#' itself for one PPS stage without replacement. The methods are those
#' whose joint probabilities are positive for every pair: exact for Sampford
#' and CPS, the high-entropy approximation for Brewer, SPS and Pareto. Systematic PPS is left out, since its pairs of zero joint
#' probability leave the variance estimator biased whatever the matrix.
#' Anything else unequal, and a `pps` argument, is refused.
#' @param spec2 The phase-2 description, or NULL for an activation phase,
#'   which is equal-probability by construction.
#' @return TRUE when phase 2 takes the joint-matrix route, FALSE when it
#'   needs no `pps`.
#' @noRd
check_twophase_phase2_family <- function(spec2, user_pps, method = NULL,
                                         call = caller_env()) {
  if (!is_null(user_pps)) {
    abort_samplyr(
      c(
        "{.arg pps} is not available for a two-phase export.",
        "x" = "{.fn survey::twophase} does not apply it to the phase-2
               variance, so the export would state a variance it does not
               compute."
      ),
      class = "samplyr_error_twophase_phase2_pps",
      call = call
    )
  }
  if (is_null(spec2)) {
    return(FALSE)
  }
  unequal <- vapply(spec2$stage, function(e) e$unequal, logical(1))
  if (!any(unequal)) {
    return(FALSE)
  }
  methods <- unique(vapply(
    spec2$stage[unequal],
    function(e) e$method,
    character(1)
  ))
  entry <- spec2$stage[[1]]
  joint_route <- length(spec2$stage) == 1L &&
    identical(entry$kind, "pps_wor") &&
    entry$method %in% twophase_joint_methods
  if (joint_route && !is_null(method) && !identical(method, "full")) {
    abort_samplyr(
      c(
        "{.code method = \"{method}\"} cannot export {.val {methods}} at
         phase 2.",
        "x" = "Only {.code method = \"full\"} uses the phase-2 joint
               probabilities. The others would drop them."
      ),
      class = "samplyr_error_twophase_phase2_pps",
      call = call
    )
  }
  if (!joint_route) {
    abort_samplyr(
      c(
        "Two-phase export does not support {.val {methods}} at phase 2.",
        "x" = "{.fn survey::twophase} computes a phase-2 variance for
               unequal-probability, balanced or spatial selection only from
               a joint probability matrix, which this export supplies for a
               single-stage phase 2 drawn with {.or {.val
               {twophase_joint_methods}}}.",
        "i" = "The weights are valid for point estimates. Draw phase 2 with
               one of those methods, or with equal probabilities, for a
               two-phase variance."
      ),
      class = "samplyr_error_twophase_phase2_pps",
      call = call
    )
  }
  TRUE
}

#' Phase-2 methods whose joint probabilities the two-phase export supplies
#' @noRd
twophase_joint_methods <- c(
  "pps_sampford", "pps_cps", "pps_brewer", "pps_sps", "pps_pareto"
)

#' Phase 2's joint inclusion probabilities, over its rows in order
#'
#' Computed on the phase-1 sample, which is the frame phase 2 was drawn
#' from. A phase-2 unit lies within one phase-1 unit (execution refuses one
#' that spans several) and the phase bridge is unique on phase-1 rows, so
#' each phase-2 unit is one row, and the matrix, in order of first
#' appearance, is in row order.
#' @noRd
twophase_phase2_joint <- function(df, prev_phase, design, stages_executed,
                                  entry) {
  compute_stage_jip(
    df, as.data.frame(prev_phase$sample), design, entry$stage,
    stages_executed
  )
}

#' Is phase 2 drawn across the units phase 1 selected?
#'
#' When phase 1 selects clusters and phase 2 selects smaller units without
#' staying inside each of them, [survey::twophase()]'s variance is unbiased
#' over repeated samples but unstable in any one: in a simulation it was
#' negative in 26 % to 51 % of samples and 1.4 to 5 times the true variance
#' otherwise. Phase 2 drawn within each phase-1 unit, or taking whole
#' phase-1 units, was right. Read on the phase-1 rows, which carry the
#' phase-2 design's variables because they were its frame.
#' @noRd
twophase_across_units <- function(df1, spec1, design2, stages2) {
  psu1 <- spec1$stage[[1]]$unit$id
  first2 <- export_stage_spec(df1, design2, stages2[1])$stage[[1]]
  per_unit <- function(ids, by) {
    tapply(ids, by, function(v) length(unique(v)))
  }
  finer <- any(per_unit(first2$unit$id, psu1) > 1L)
  stratum2 <- if (length(first2$strata$user)) {
    group_ids(df1, first2$strata$user)
  } else {
    rep(1L, nrow(df1))
  }
  finer && any(per_unit(psu1, stratum2) > 1L)
}

#' @noRd
warn_twophase_across_units <- function(design1, stages1,
                                       call = caller_env()) {
  units <- design1$stages[[stages1[1]]]$clusters$vars
  cli_warn(
    c(
      "Phase 2 was drawn across the units phase 1 selected, so its variance
       is unstable.",
      "x" = "{.fn survey::twophase} gives a variance that is right over
             repeated samples but not in one: in a simulation it was
             negative (a {.code NaN} standard error) in 26% to 51% of
             samples and 1.4 to 5 times the true variance otherwise.",
      "i" = "The weights in {.field .weight} are exact, so totals and means
             are unaffected.",
      "i" = "Phase 2 drawn within each phase-1 unit, with
             {.code stratify_by({paste(units, collapse = ', ')})}, has a
             stable variance. See {.help as_svydesign}."
    ),
    class = "samplyr_warning_twophase_across_units",
    call = call
  )
}

#' Give the phase-2 strata terms a value on every phase-1 row.
#'
#' `spec_survey_strata()` builds the phase-2 terms on the phase-2 rows, so
#' after the join the generated ones are NA on phase-1 rows outside phase 2.
#' `survey::twophase()` reads phase-2 strata on every phase-1 row, because it
#' relates each phase-2 stratum to its phase-1 count, and refuses the NA.
#' A placeholder term is constant, and a combined term is rebuilt from its
#' source variables when phase 1 carries them, which it does for the design's
#' own stratification variables because the phase-2 frame is the phase-1
#' sample. A term naming a user variable already has phase-1 values.
#' @noRd
complete_phase2_strata <- function(df, strata) {
  for (i in seq_along(strata$vars)) {
    v <- strata$vars[i]
    if (identical(strata$kinds[i], "placeholder")) {
      df[[v]] <- "all"
    } else if (identical(strata$kinds[i], "combined")) {
      src <- strata$sources[[i]]
      if (
        all(src %in% names(df)) &&
          !anyNA(unlist(lapply(src, function(s) df[[s]])))
      ) {
        df[[v]] <- group_ids(df, src)
      }
    }
  }
  df
}

#' Classify a stage's selection method for variance export.
#'
#' Centralizes the `wr`, `rs_poisson`, `pps_wor`, `equal_wor`, and
#' `unsupported` families used by FPC, PPS, and replicate decisions.
#' @noRd
survey_stage_kind <- function(draw_spec) {
  # Controlled and spatially balanced methods lack a supported linearization.
  if (
    !is_null(draw_spec$bounds) ||
      draw_spec$method %in% spatial_balanced_methods
  ) {
    return("unsupported")
  }

  # A registered variance family overrides inference from method metadata.
  if (!is_null(draw_spec$method_variance)) {
    kind <- switch(
      draw_spec$method_variance,
      srs = "equal_wor",
      pps_brewer = "pps_wor",
      poisson = "rs_poisson",
      wr = "wr",
      unsupported = "unsupported",
      NULL
    )
    if (!is_null(kind)) {
      return(kind)
    }
  }
  method <- draw_spec$method
  if (
    method %in%
      c(wr_methods, pmr_methods) ||
      identical(draw_spec$method_type, "wr")
  ) {
    return("wr")
  }
  if (method %in% rs_poisson_methods) {
    return("rs_poisson")
  }
  if (identical(draw_spec$method_type, "wor")) {
    # Route custom WOR by fixed or random sample size.
    if (identical(draw_spec$method_fixed, FALSE)) {
      return("rs_poisson")
    }
    return("pps_wor")
  }
  if (identical(draw_spec$method_type, "balanced")) {
    # Route custom balanced methods with cube rather than SRS variance.
    return("pps_wor")
  }
  if (method %in% pps_wor_methods || method %in% balanced_methods) {
    return("pps_wor")
  }
  "equal_wor"
}

#' Resolve the variables that match phase-2 rows into the phase-1 data
#'
#' Uses identifiers shared by both phases and requires a unique phase-1 match.
#' Phase-2 descendants may repeat that bridge.
#' @noRd
resolve_phase_bridge <- function(phase1_ids, phase2_ids, df1, df2,
                                 call = caller_env()) {
  candidates <- unique(c(phase1_ids, phase2_ids))
  bridge_vars <- candidates[
    candidates %in% names(df1) & candidates %in% names(df2)
  ]

  if (length(bridge_vars) == 0) {
    abort_samplyr(
      c(
        "Two-phase conversion needs a variable linking the phases.",
        "i" = "Neither phase declares a unit identifier that both samples
               carry.",
        "i" = "Declare the sampling units with {.fn cluster_by}, and keep the
               phase-1 identifier on the phase-2 frames."
      ),
      class = "samplyr_error_twophase_bridge",
      call = call
    )
  }

  for (var in bridge_vars) {
    compatible <- tryCatch(
      {
        vctrs::vec_ptype2(df1[[var]], df2[[var]])
        TRUE
      },
      error = function(e) FALSE
    )
    if (!compatible) {
      abort_samplyr(
        c(
          "{.field {var}} cannot link the phases.",
          "x" = "Phase 1 holds {.cls {vctrs::vec_ptype_full(df1[[var]])}} and
                 phase 2 holds {.cls {vctrs::vec_ptype_full(df2[[var]])}}.",
          "i" = "Give the identifier the same type in both phases."
        ),
        class = "samplyr_error_twophase_bridge",
        call = call
      )
    }
  }

  keys1 <- df1[, bridge_vars, drop = FALSE]
  keys2 <- df2[, bridge_vars, drop = FALSE]

  # Missing phase keys never link.
  incomplete2 <- !stats::complete.cases(keys2)
  if (any(incomplete2)) {
    abort_samplyr(
      c(
        "{sum(incomplete2)} phase-2 row{?s} {?has/have} a missing
         {.field {bridge_vars}}.",
        "x" = "A row with no identifier cannot be matched to the phase-1
               sample.",
        "i" = "Missing values never link two phases."
      ),
      class = "samplyr_error_twophase_bridge",
      call = call
    )
  }
  keys1 <- keys1[stats::complete.cases(keys1), , drop = FALSE]

  # Only reachable phase-1 keys must be unique.
  distinct2 <- unique(keys2)
  matches <- vctrs::vec_count(
    vctrs::vec_slice(
      keys1,
      !is.na(vctrs::vec_match(keys1, distinct2))
    ),
    sort = "none"
  )

  reachable <- distinct2[
    !is.na(vctrs::vec_match(distinct2, matches$key)), , drop = FALSE
  ]
  if (nrow(reachable) < nrow(distinct2)) {
    orphans <- distinct2[
      is.na(vctrs::vec_match(distinct2, matches$key)), , drop = FALSE
    ]
    abort_samplyr(
      c(
        "{nrow(orphans)} phase-2 {.field {bridge_vars}} value{?s} {?is/are}
         absent from the phase-1 sample.",
        "x" = "{format_pool_sample(format_key_preview(orphans))}",
        "i" = "Every phase-2 observation must belong to a phase-1 unit."
      ),
      class = "samplyr_error_twophase_bridge",
      call = call
    )
  }

  if (any(matches$count > 1)) {
    ambiguous <- matches$key[matches$count > 1, , drop = FALSE]
    abort_samplyr(
      c(
        "{.field {bridge_vars}} does not identify phase-1 rows uniquely.",
        "x" = "{format_pool_sample(format_key_preview(ambiguous))} match more than one
               phase-1 row, so the join would be many-to-many and would
               duplicate observations.",
        "i" = "A finer identifier shared by both phases resolves this."
      ),
      class = "samplyr_error_twophase_bridge",
      call = call
    )
  }

  bridge_vars
}

#' @noRd
survey_key_vars <- function(design, stages_executed, df) {
  vars <- character(0)
  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    draw_col <- paste0(".draw_", stage_idx)

    if (is_multi_hit_method(stage_spec$draw_spec) && draw_col %in% names(df)) {
      vars <- c(vars, draw_col)
    } else if (!is_null(stage_spec$clusters)) {
      vars <- c(vars, stage_spec$clusters$vars)
    }
  }
  vars
}

#' @noRd
survey_ids_formula <- function(id_vars) {
  if (length(id_vars) == 0) {
    rlang::new_formula(NULL, 1)
  } else {
    survey_formula_from_vars(id_vars)
  }
}

#' Build a one-sided formula without parsing column names as code
#' @noRd
survey_formula_from_vars <- function(vars) {
  if (length(vars) == 0L) {
    cli_abort(
      "A survey formula requires at least one variable.",
      call = NULL,
      class = "samplyr_error_survey_argument"
    )
  }
  terms <- lapply(vars, rlang::sym)
  rhs <- Reduce(function(x, y) call("+", x, y), terms)
  rlang::new_formula(NULL, rhs)
}

#' Does the first stage's FPC term state that there is no correction?
#'
#' A with-replacement first stage (or one demoted to that approximation) has
#' no finite population correction, written as an infinite population size
#' on the count scale or a zero sampling fraction on the fraction scale.
#' Linearization reads both. survey's jackknife and Rao-Wu bootstrap
#' conversions do not: the jackknife stops with a missing-value error and the
#' bootstrap returns a standard error of zero. Those conversions use only the
#' first stage's correction, so leaving the term out is exact for them.
#' @noRd
first_stage_uncorrected <- function(df, fpc) {
  if (is_null(fpc$formula) || length(fpc$fpc_vars) == 0) {
    return(FALSE)
  }
  first <- df[[fpc$fpc_vars[1]]]
  all(is.infinite(first)) ||
    (identical(fpc$scale, "fraction") && all(first == 0))
}

#' Warn about strata that hold a single sampled unit.
#'
#' `survey` computes each stratum's variance from the spread of its sampled
#' units, which one unit cannot give. Under its default
#' `survey.lonely.psu = "fail"` it stops when a variance is requested,
#' naming a stratum by internal codes. Warning at export names the stage and
#' the design's own strata instead. A stratum taken whole is a census, which
#' survey accepts, and so is not reported.
#' @noRd
warn_lonely_strata <- function(spec, design, id_stage_indices,
                               call = caller_env()) {
  if (!identical(getOption("survey.lonely.psu", "fail"), "fail")) {
    return(invisible(NULL))
  }
  lonely <- spec_singletons(spec, id_stage_indices)
  for (stage_idx in as.integer(names(lonely)[lonely > 0L])) {
    n_lonely <- lonely[[as.character(stage_idx)]]
    token <- stage_token(design, stage_idx)
    cli_warn(
      c(
        "In {token}, {n_lonely} strat{?um/a} hold{?s/} a single
         sampled unit.",
        "i" = "{.pkg survey} cannot estimate a variance from one unit and,
               under its default {.code survey.lonely.psu = \"fail\"},
               stops when a standard error is requested.",
        "i" = "Set {.code options(survey.lonely.psu = \"adjust\")} for the
               conservative treatment, or collapse the strata before
               export."
      ),
      class = "samplyr_warning_lonely_psu",
      stage = stage_idx,
      n_strata = n_lonely,
      call = call
    )
  }
  invisible(NULL)
}

#' Selected primary units left with no row in the sample
#'
#' A primary unit whose descendants were all empty has no row, so survey's
#' variance between primary units runs over the others: the unit's zero total
#' is lost, and with it both the spread and the count of units. Each
#' empty-parent record carries its parent's full ancestry, which names the
#' primary unit.
#' @return The vanished units' keys as a data frame, or NULL.
#' @noRd
vanished_primary_units <- function(x, design = get_design(x),
                                   stages = get_stages_executed(x)) {
  records <- sample_empty_parents(x)
  vars <- design$stages[[stages[1]]]$clusters$vars
  if (length(records) == 0L || is_null(vars)) {
    return(NULL)
  }
  df <- as.data.frame(x)
  keys <- Filter(Negate(is_null), lapply(records, function(r) {
    if (all(vars %in% names(r$keys))) r$keys[vars]
  }))
  if (length(keys) == 0L) {
    return(NULL)
  }
  keys <- vctrs::vec_unique(vctrs::vec_rbind(!!!keys))
  gone <- keys[!vctrs::vec_in(keys, df[vars]), , drop = FALSE]
  if (nrow(gone) == 0L) NULL else gone
}

#' The empty-parent records of the realization this sample holds
#'
#' Recorded at execution whatever the frame digest, one per stage and
#' replicate. A sample holding one replicate keeps that replicate's.
#' @noRd
sample_empty_parents <- function(x) {
  records <- attr(x, "metadata")$empty_parents
  if (length(records) == 0L || !".replicate" %in% names(x)) {
    return(records)
  }
  reps <- unique(x$.replicate)
  Filter(function(r) is.na(r$replicate) || r$replicate %in% reps, records)
}

#' Refuse an export that would drop an empty primary unit
#' @noRd
check_export_primary_units <- function(x, design = get_design(x),
                                       stages = get_stages_executed(x),
                                       call = caller_env()) {
  gone <- vanished_primary_units(x, design, stages)
  if (is_null(gone)) {
    return(invisible(NULL))
  }
  n <- nrow(gone)
  abort_samplyr(
    c(
      "{n} selected primary unit{?s} {?has/have} no row in this sample.",
      "x" = "{cli::qty(n)}{?Its/Their} descendants were all empty, so
             {.pkg survey} would compute the variance between primary units
             without {?it/them}. A unit with nothing eligible still counts,
             as a zero total, and leaving it out makes the variance too
             small, often zero.",
      "i" = "{cli::qty(n)}Unit{?s}: {format_pool_sample(format_key_preview(gone))}.",
      "i" = "Totals and means from {.field .weight} are right. No export
             represents an empty primary unit yet, and
             {.code type = \"rwyb\"} refuses it too."
    ),
    class = "samplyr_error_export_empty_psu",
    n_units = n,
    call = call
  )
}

#' Warn when a stage ran with selected parents that had nothing to sample
#'
#' Accepted with `on_empty`, such a parent contributes zero, so totals are
#' unbiased and so is every primary unit's total. It has no rows, though, so
#' survey's recursion computes the parent stage's within-unit variance
#' without it. That term matters only when first-stage sampling fractions
#' are large. The ultimate-cluster route avoids it. A primary unit left with
#' no row at all changes the variance between primary units instead, and is
#' refused before this by `check_export_primary_units()`.
#' @noRd
warn_export_empty_parents <- function(x, design, id_stage_indices,
                                      call = caller_env()) {
  records <- attr(x, "metadata")$empty_parents
  if (length(records) == 0L || length(id_stage_indices) < 2L) {
    return(invisible(NULL))
  }
  stages <- sort(unique(vapply(records, function(r) r$stage, integer(1))))
  stages <- stages[(stages - 1L) %in% id_stage_indices[-1L]]
  for (stage_idx in stages) {
    n_empty <- sum(vapply(
      Filter(function(r) identical(r$stage, stage_idx), records),
      function(r) r$n, integer(1)
    ))
    token <- stage_token(design, stage_idx)
    parent <- stage_token(design, stage_idx - 1L)
    cli_warn(
      c(
        "In {token}, {n_empty} selected unit{?s} had nothing to sample, so
         {?it has/they have} no rows here.",
        "i" = "{cli::qty(n_empty)}Totals, and the total of every primary
               unit, are unaffected, since {?such a unit contributes/these
               units contribute} zero.",
        "i" = "{.pkg survey} computes the variance of {parent} without
               {cli::qty(n_empty)}{?it/them}, and a parent left with one
               unit reads to it as a lonely stratum at that stage.",
        "i" = "Both are within-unit terms, which matter only when
               first-stage sampling fractions are large. {.code
               options(survey.lonely.psu = \"adjust\")} treats a lonely
               parent conservatively."
      ),
      class = "samplyr_warning_export_empty_parent",
      stage = stage_idx,
      n_empty = n_empty,
      call = call
    )
  }
  invisible(NULL)
}

#' Survey sampling-unit terms from the per-stage description
#'
#' One identifier term per represented stage: a with-replacement stage's draw
#' identifier, a cluster stage's first-appearance ids, and a synthesized row
#' id for a final element stage below another stage. A single element stage
#' needs no term. `prefix` separates two-phase columns. Every column added
#' takes a name free in `df` and in `taken`, because the sample already holds
#' the user's columns and a fixed name would overwrite one.
#' @noRd
spec_survey_ids <- function(
  spec,
  df,
  synthesize_unclustered = TRUE,
  prefix = "",
  taken = character(0),
  call = rlang::caller_env()
) {
  id_vars <- character(0)
  stage_indices <- integer(0)
  n_exec <- length(spec$stages)

  for (entry in spec$stage) {
    stage_idx <- entry$stage
    if (identical(entry$unit$kind, "draw")) {
      id_vars <- c(id_vars, entry$unit$source)
      stage_indices <- c(stage_indices, stage_idx)
    } else if (identical(entry$unit$kind, "cluster") ||
               (synthesize_unclustered && n_exec > 1L)) {
      if (entry$midstage_element) {
        abort_survey_midstage_element(stage_idx, call = call)
      }
      id_var <- free_name(
        c(names(df), taken),
        paste0(".", prefix, "id_", stage_idx)
      )
      df[[id_var]] <- entry$unit$id
      id_vars <- c(id_vars, id_var)
      stage_indices <- c(stage_indices, stage_idx)
    }
  }

  list(df = df, id_vars = id_vars, stage_indices = stage_indices)
}

#' @noRd
abort_survey_midstage_element <- function(stage_idx, call = caller_env()) {
  abort_samplyr(
    c(
      "Cannot export stage {stage_idx} to {.fn survey::svydesign}:
       an unclustered element-sampling stage followed by later
       stages cannot be expressed as nested cluster sampling.",
      "i" = "If stage {stage_idx} selects whole clusters, declare
             them with {.fn cluster_by}.",
      "i" = "If it selects elements, this is phase sampling:
             execute stages 1-{stage_idx} as phase 1, then run the
             remaining stages as a {.emph separate design} on that
             result (not a continuation of this one).
             {.fn as_svydesign} then exports via
             {.fn survey::twophase}.",
      "i" = "Each phase declares its own units with {.fn cluster_by};
             they need not be the same. Export links the phases on the
             phase-1 identifier, so keep it on the phase-2 frames."
    ),
    class = "samplyr_error_survey_midstage_element",
    call = call
  )
}

#' Survey strata terms from the per-stage description
#'
#' One term per represented stage, aligned with the identifier terms. A
#' stage's term is its single stratification variable, its take-all column,
#' or a synthesized column for their combination. Interior unstratified
#' stages get a constant placeholder and trailing ones are dropped. Columns
#' are added in a fixed order (take-all, then combination, per stage, then
#' the placeholders), because each name is resolved against those already
#' present. `kinds`, `stages` and `sources` record what each term is, so later
#' steps never read that back from a column name.
#' @noRd
spec_survey_strata <- function(
  spec,
  df,
  id_stage_indices = integer(0),
  prefix = "",
  taken = character(0)
) {
  first_stage_idx <- spec$stages[1]
  term_stages <- if (length(id_stage_indices) == 0) {
    first_stage_idx
  } else {
    id_stage_indices
  }

  n_terms <- length(term_stages)
  terms <- rep(NA_character_, n_terms)
  kinds <- rep(NA_character_, n_terms)
  sources <- vector("list", n_terms)
  for (i in seq_along(term_stages)) {
    stage_idx <- term_stages[i]
    strata <- spec$stage[[as.character(stage_idx)]]$strata
    vars <- strata$user
    has_certainty <- !is_null(strata$certainty)
    if (has_certainty) {
      base <- if (stage_idx == first_stage_idx) {
        paste0(".", prefix, "cert_stratum")
      } else {
        paste0(".", prefix, "cert_stratum_", stage_idx)
      }
      cert_var <- free_name(c(names(df), taken), base)
      df[[cert_var]] <- certainty_labels(strata$certainty)
      vars <- c(vars, cert_var)
    }
    if (length(vars) == 0) {
      next
    }
    sources[[i]] <- vars
    if (length(vars) == 1) {
      terms[i] <- vars
      kinds[i] <- if (has_certainty) "certainty" else "user"
    } else {
      combined <- free_name(
        c(names(df), taken),
        paste0(".", prefix, "strata_", stage_idx)
      )
      df[[combined]] <- strata$id
      terms[i] <- combined
      kinds[i] <- "combined"
    }
  }

  last_stratified <- max(c(0L, which(!is.na(terms))))
  keep <- seq_len(last_stratified)
  terms <- terms[keep]
  kinds <- kinds[keep]
  sources <- sources[keep]
  for (i in seq_along(terms)) {
    if (is.na(terms[i])) {
      placeholder <- free_name(
        c(names(df), taken),
        paste0(".", prefix, "strata_all_", term_stages[i])
      )
      df[[placeholder]] <- "all"
      terms[i] <- placeholder
      kinds[i] <- "placeholder"
    }
  }

  list(
    df = df,
    formula = if (length(terms) == 0) NULL else survey_formula_from_vars(terms),
    vars = terms,
    kinds = kinds,
    stages = term_stages[keep],
    sources = sources
  )
}

#' @noRd
certainty_labels <- function(certainty) {
  ifelse(certainty, "certainty", "probability")
}

#' A stage's stratum as its linearized term reads, as character
#'
#' The single stratification variable, the take-all labels, or the ids of
#' their combination, which is what `spec_survey_strata()` writes. NULL for
#' an unstratified stage.
#' @noRd
spec_strata_labels <- function(entry, df) {
  strata <- entry$strata
  n_vars <- length(strata$user) + !is_null(strata$certainty)
  if (n_vars == 0L) {
    return(NULL)
  }
  if (n_vars > 1L) {
    return(as.character(strata$id))
  }
  if (!is_null(strata$certainty)) {
    return(certainty_labels(strata$certainty))
  }
  as.character(df[[strata$user]])
}

#' Strata holding one sampled unit, per represented stage
#'
#' Pools are strata within the units selected at the stages above, and a
#' pool is lonely when it holds one sampling unit and was not taken whole.
#' A census pool is exempt, since survey accepts it. With no identifier
#' stage, the single element stage is scanned.
#' @return Named integer, the number of lonely pools per stage.
#' @noRd
spec_singletons <- function(spec, stages) {
  # A single unclustered stage needs no id column: its units are the rows.
  if (length(stages) == 0) {
    stages <- spec$stages[1]
  }
  n <- spec$n_rows
  parent <- rep(1L, n)
  lonely <- integer(0)
  for (stage_idx in stages) {
    entry <- spec$stage[[as.character(stage_idx)]]
    stratum <- entry$strata$id %||% rep(1L, n)
    pool <- group_ids(
      data.frame(parent = parent, stratum = stratum),
      c("parent", "stratum")
    )
    # Execution keeps a cluster inside one stratum, so parent and unit
    # identify it within its pool.
    unit <- group_ids(
      data.frame(parent = parent, unit = entry$unit$id),
      c("parent", "unit")
    )
    first <- !duplicated(unit)
    n_units <- tabulate(pool[first], nbins = max(c(0L, pool)))
    census <- as.vector(tapply(
      is_certainty_probability(entry$prob %||% rep(0, n)),
      factor(pool, levels = seq_along(n_units)),
      all
    ))
    lonely[as.character(stage_idx)] <- sum(n_units == 1L & !census)
    parent <- unit
  }
  lonely
}

#' Per-stage FPC terms.
#'
#' Aligns one term per ID stage. Uses counts by default, a common fraction
#' scale in multi-stage PPS designs, and the legacy probability scale for one
#' represented PPS stage.
#' @noRd
survey_fpc_info <- function(df, design, stages_executed, id_stage_indices,
                            taken = character(0)) {
  rlang::local_error_call(caller_env())
  later_poisson <- stages_executed[-1L][vapply(stages_executed[-1L], function(i) {
    identical(survey_stage_kind(design$stages[[i]]$draw_spec), "rs_poisson")
  }, logical(1))]
  if (length(later_poisson)) {
    abort_samplyr(c(
      "Linearization export cannot represent Poisson sampling at later stages.",
      "i" = "Use {.code as_svrepdesign(x, type = \"rwyb\")} to retain the random sample-size variance."
    ), class = "samplyr_error_multistage_poisson_later")
  }
  fpc_stage_indices <- if (length(id_stage_indices) == 0) {
    stages_executed[1]
  } else {
    id_stage_indices
  }

  first_executed <- stages_executed[1]

  stage_kind <- vapply(
    fpc_stage_indices,
    function(stage_idx) {
      kind <- survey_stage_kind(design$stages[[stage_idx]]$draw_spec)
      # Unsupported stages are refused or demoted before FPC use.
      if (kind %in% c("rs_poisson", "unsupported")) {
        if (identical(stage_idx, first_executed)) {
          paste0(kind, "_first")
        } else {
          paste0(kind, "_later")
        }
      } else {
        kind
      }
    },
    character(1)
  )

  has_pps_wor <- any(stage_kind == "pps_wor")
  has_rs_poisson_stage1 <- any(stage_kind == "rs_poisson_first")

  needs_pi <- has_pps_wor ||
    has_rs_poisson_stage1 ||
    any(stage_kind == "unsupported_first")
  scale <- if (needs_pi && length(fpc_stage_indices) > 1L) {
    "fraction"
  } else if (needs_pi) {
    "pi"
  } else {
    "count"
  }

  # Generated terms take names free in `df` and `taken`.
  add_term <- function(base, value) {
    name <- free_name(c(names(df), taken), base)
    df[[name]] <<- value
    name
  }
  fpc_vars <- character(0)
  for (i in seq_along(fpc_stage_indices)) {
    stage_idx <- fpc_stage_indices[i]
    kind <- stage_kind[i]
    weight_col <- paste0(".weight_", stage_idx)
    fpc_col <- paste0(".fpc_", stage_idx)

    if (kind %in% c("wr", "unsupported_later")) {
      fpc_vars <- c(fpc_vars, if (scale == "fraction") {
        add_term(paste0(".fpc_f0_", stage_idx), 0)
      } else {
        add_term(paste0(".fpc_inf_", stage_idx), Inf)
      })
      next
    }

    if (kind %in% c("pps_wor", "rs_poisson_first", "unsupported_first")) {
      prob <- 1 / df[[weight_col]]
      if (draws_one_per_zone(design$stages[[stage_idx]]$draw_spec)) {
        # One draw per zone has the with-replacement variance, so the
        # remainder carries no correction. Certainty PSUs keep theirs, which
        # brings their later stages in.
        prob[!is_certainty_probability(prob)] <- 0
      }
      fpc_vars <- c(
        fpc_vars,
        add_term(paste0(".fpc_pi_", stage_idx), prob)
      )
      next
    }

    # Equal-probability WOR.
    if (scale == "fraction") {
      fpc_vars <- c(
        fpc_vars,
        add_term(paste0(".fpc_f_", stage_idx), 1 / df[[weight_col]])
      )
    } else if (fpc_col %in% names(df)) {
      fpc_vars <- c(fpc_vars, fpc_col)
    } else {
      # Keep an infinite FPC term when its population count is absent.
      fpc_vars <- c(fpc_vars, add_term(paste0(".fpc_inf_", stage_idx), Inf))
    }
  }

  fpc_formula <- if (length(fpc_vars) == 0) {
    NULL
  } else {
    survey_formula_from_vars(fpc_vars)
  }

  list(
    df = df,
    formula = fpc_formula,
    fpc_vars = fpc_vars,
    stage_indices = fpc_stage_indices,
    scale = scale,
    has_pps_wor = has_pps_wor,
    has_rs_poisson_stage1 = has_rs_poisson_stage1
  )
}

#' Demote an unsupported stage-1 variance specification to a WR approximation
#'
#' Used only by the existing generic bootstrap route for unsupported
#' balanced or spatial variance families. Poisson designs use RWYB directly.
#' @noRd
survey_demote_rs_poisson_stage1 <- function(df, fpc, first_idx) {
  pi_col <- fpc$fpc_vars[match(first_idx, fpc$stage_indices)]
  # No correction is zero fraction or infinite population.
  if (identical(fpc$scale, "fraction")) {
    inf_col <- free_name(names(df), paste0(".fpc_f0_", first_idx))
    df[[inf_col]] <- 0
  } else {
    inf_col <- free_name(names(df), paste0(".fpc_inf_", first_idx))
    df[[inf_col]] <- Inf
  }
  df[[pi_col]] <- NULL
  fpc$fpc_vars <- ifelse(fpc$fpc_vars == pi_col, inf_col, fpc$fpc_vars)
  fpc$formula <- if (length(fpc$fpc_vars) == 0) {
    NULL
  } else {
    survey_formula_from_vars(fpc$fpc_vars)
  }
  fpc$has_rs_poisson_stage1 <- FALSE
  list(df = df, fpc = fpc)
}

#' Classify a user's `pps` argument for the single-phase export
#'
#' survey reads `pps` in several ways, and a single rule (truncate to stage 1
#' whenever it is given) treated them alike. `"brewer"` is what samplyr
#' already applies at every PPS stage, so it keeps the multi-stage design.
#' `FALSE` says there is no PPS stage, which is true or a contradiction.
#' `"overton"`, `HR()` and the matrix objects are survey's single-stage
#' specifications. The matrix objects (`ppsmat()`, `poisson_sampling()`) are
#' indexed by row, so they need one row per stage-1 unit: with several, survey
#' fails ("incorrect length for 'group'") or, row-expanded, reads the matrix
#' diagonal as the weights.
#'
#' @return list(kind = "default" | "single_stage", pps = the value to forward
#'   to the resolver, NULL for the default).
#' @noRd
survey_user_pps <- function(pps, design, stages_executed, df,
                            call = caller_env()) {
  if (is_null(pps)) {
    return(list(kind = "default", pps = NULL))
  }
  has_pps_stage <- any(vapply(
    stages_executed,
    function(i) {
      identical(survey_stage_kind(design$stages[[i]]$draw_spec), "pps_wor")
    },
    logical(1)
  ))
  refuse <- function(why) {
    abort_samplyr(
      c("{.arg pps} does not fit this design.", "x" = why),
      class = "samplyr_error_pps_argument",
      call = call
    )
  }
  if (identical(pps, "brewer")) {
    if (!has_pps_stage) {
      refuse("{.code pps = \"brewer\"} applies to PPS stages, and this
              design has none. Omit {.arg pps}.")
    }
    return(list(kind = "default", pps = NULL))
  }
  if (isFALSE(pps)) {
    if (has_pps_stage) {
      refuse("{.code pps = FALSE} would read the PPS stages' inclusion
              probabilities as equal-probability sampling fractions. Omit
              {.arg pps} for Brewer's approximation.")
    }
    return(list(kind = "default", pps = NULL))
  }
  if (!identical(pps, "overton") && !inherits(pps, c("pps_spec", "HR"))) {
    refuse("{.arg pps} takes {.code \"brewer\"}, {.code \"overton\"},
            {.code FALSE}, or an object from {.fn survey::ppsmat},
            {.fn survey::HR} or {.fn survey::poisson_sampling}.")
  }
  if (inherits(pps, "pps_spec")) {
    first <- design$stages[[stages_executed[1]]]
    if (!is_null(first$clusters)) {
      n_units <- nrow(unique(df[, first$clusters$vars, drop = FALSE]))
      if (nrow(df) > n_units) {
        abort_samplyr(
          c(
            "A {.arg pps} matrix needs one row per stage-1 unit.",
            "x" = "This sample has {nrow(df)} rows for {n_units} stage-1
                   unit{?s}, and {.pkg survey} indexes the matrix by row.",
            "i" = "Omit {.arg pps} for Brewer's approximation at every
                   stage, or use {.code as_svrepdesign(x, type = \"rwyb\")}."
          ),
          class = "samplyr_error_pps_rows_per_psu",
          call = call
        )
      }
    }
  }
  list(kind = "single_stage", pps = pps)
}

#' Resolve the pps argument for as_svydesign.
#'
#' Single-phase only: exact Poisson variance for single-stage element
#' sampling, refusal for other Poisson designs, Brewer for fixed-size PPS
#' WOR, and FALSE otherwise. Generic bootstraps may relax unsupported
#' balanced or spatial families, but never independent Poisson sampling.
#' @noRd
survey_resolve_pps <- function(
  df,
  design,
  stages_executed,
  fpc,
  user_pps = NULL,
  relax_pps_for_bootstrap = FALSE
) {
  rlang::local_error_call(caller_env())
  if (!is_null(user_pps)) {
    # A user pps object describes stage 1 only.
    if (inherits(user_pps, "ppsmat") &&
        identical(
          design$stages[[stages_executed[1]]]$draw_spec$method,
          "pps_systematic"
        )) {
      cli_warn(
        c(
          "A sampled joint matrix cannot establish full pair positivity for systematic PPS.",
          "i" = "Zero or very small population pair probabilities can make the {.pkg survey} variance unavailable or unstable.",
          "i" = "Inspect the full population joint matrix before relying on this variance estimate."
        ),
        class = "samplyr_warning_systematic_ppsmat"
      )
    }
    return(list(pps = user_pps, df = df, fpc = fpc))
  }

  # Unsupported variance families cannot use linearization.
  unsupported_idx <- stages_executed[vapply(
    stages_executed,
    function(i) {
      identical(survey_stage_kind(design$stages[[i]]$draw_spec), "unsupported")
    },
    logical(1)
  )]
  if (length(unsupported_idx) > 0) {
    if (!relax_pps_for_bootstrap) {
      unsupported_methods <- vapply(
        unsupported_idx,
        function(i) design$stages[[i]]$draw_spec$method,
        character(1)
      )
      abort_samplyr(
        c(
          "Cannot export method{?s} {.val {unsupported_methods}} via {.fn as_svydesign}.",
          "i" = "No linearization variance estimator is available for this method and its declared constraints.",
          "i" = "Use {.code as_svrepdesign(type = \"subbootstrap\")} for a bootstrap approximation."
        ),
        class = "samplyr_error_custom_random_wor_export"
      )
    }
    if (stages_executed[1] %in% unsupported_idx) {
      relaxed <- survey_demote_rs_poisson_stage1(df, fpc, stages_executed[1])
      df <- relaxed$df
      fpc <- relaxed$fpc
    }
  }

  if (!fpc$has_rs_poisson_stage1) {
    pps <- if (fpc$has_pps_wor) "brewer" else FALSE
    return(list(pps = pps, df = df, fpc = fpc))
  }

  first_idx <- stages_executed[1]

  if (length(stages_executed) > 1L) {
    abort_samplyr(
      c(
        "{.pkg survey} does not support multi-stage designs with a random-size Poisson method at stage 1.",
        "i" = "{.pkg survey} rejects multi-stage designs when the {.code pps} argument is set.",
        "i" = "Use {.code as_svrepdesign(type = \"rwyb\")} for independent Poisson replication.",
        "i" = "Or convert each stage separately."
      ),
      class = "samplyr_error_multistage_poisson_stage1"
    )
  }

  stage_spec <- design$stages[[first_idx]]
  if (!is_null(stage_spec$clusters)) {
    cluster_vars <- stage_spec$clusters$vars
    n_clusters <- nrow(unique(df[, cluster_vars, drop = FALSE]))
    if (nrow(df) > n_clusters) {
      abort_samplyr(
        c(
          "Cannot export a clustered random-size Poisson design with multiple rows per sampled cluster via {.fn as_svydesign}.",
          "i" = "{.pkg survey}'s {.fn poisson_sampling} estimator treats rows as independent and does not honor within-cluster correlation.",
          "i" = "Use {.code as_svrepdesign(type = \"rwyb\")} to replicate the independent Poisson cluster selections."
        ),
        class = "samplyr_error_cluster_poisson_export"
      )
    }
  }

  # Only methods declaring independent Poisson selections use this estimator.
  is_declared_poisson <- identical(
    stage_spec$draw_spec$method_variance,
    "poisson"
  )
  if (
    !stage_spec$draw_spec$method %in% rs_poisson_methods &&
      !is_declared_poisson
  ) {
    abort_samplyr(
      c(
        "Cannot export the custom random-size method {.val {stage_spec$draw_spec$method}} via {.fn as_svydesign}.",
        "i" = "The method is registered with {.code fixed_size = FALSE}, so the sample size is random. {.pkg survey}'s Poisson variance estimator assumes selections are independent across units, which samplyr cannot verify for a custom method.",
        "i" = "If selections are independent (Poisson-type), pass the inclusion probabilities explicitly: {.code as_svydesign(x, pps = survey::poisson_sampling(1 / x$.weight))}.",
        "i" = "To use RWYB replication, register the method with {.code variance_family = \"poisson\"} only if selections are independent."
      ),
      class = "samplyr_error_custom_random_wor_export"
    )
  }

  pi_vec <- df[[fpc$fpc_vars[match(first_idx, fpc$stage_indices)]]]
  list(pps = survey::poisson_sampling(pi_vec), df = df, fpc = fpc)
}

#' Argument names the survey package accepts on the export paths
#'
#' The export verbs forward `...` to survey, so a name survey accepts must
#' pass and everything else must be refused by name rather than silently
#' forwarded. Each set is the formals of the function named, plus the formals
#' of the helpers that function forwards its own `...` to, minus the ones
#' samplyr supplies itself (the `derived_args` sets below). Taken from survey
#' 4.5. `test-survey-arguments.R` checks them against the installed version.
#' @noRd
svydesign_accepted_args <- c(
  # survey::svydesign() and its default method
  "variables", "nest", "check.strata", "pps", "calibrate.formula",
  "variance", "na_weights"
)

#' @noRd
twophase_accepted_args <- c(
  # These are the only forwardable `survey::twophase()` arguments.
  "method", "pps"
)

#' @noRd
svrepdesign_accepted_args <- c(
  # survey::as.svrepdesign() and its default method
  "type", "fay.rho", "fpc", "fpctype", "compress", "mse",
  # Include arguments used by the replicate-weight generators.
  "match", "small", "large", "hadamard.matrix", "lonely.psu", "replicates",
  "multicore"
)

#' Argument names samplyr computes and supplies to survey itself
#'
#' Accepting these forwarded the user's value into a `do.call()` whose named
#' arguments were already fixed, so survey raised "formal argument matched by
#' multiple actual arguments" from inside a verb the user never called. They
#' are refused by name instead, before their values are forced.
#'
#' `probs` is derived on both paths for the same reason with a different
#' symptom: `weights = ~.weight` is always supplied, and survey refuses a
#' design that carries both.
#'
#' `pps` is not here. It is extracted from `...` before the `do.call()` on
#' both paths and is the documented route to an exact PPS variance.
#' @noRd
svydesign_derived_args <- c(
  "ids", "probs", "strata", "weights", "fpc", "data"
)

#' @noRd
twophase_derived_args <- c(
  "id", "strata", "probs", "weights", "fpc", "subset", "data"
)

#' @noRd
svrepdesign_derived_args <- "design"

#' @rdname as_svydesign
#' @export
as_svydesign.tbl_sample <- function(x, ..., nest = TRUE, method = NULL,
                                   systematic_variance = c("warn",
                                                           "approximate",
                                                           "error")) {
  systematic_variance <- with_error_class(
    rlang::arg_match(systematic_variance),
    "samplyr_error_survey_argument"
  )
  check_single_replicate(x, "as_svydesign")
  check_sample_unmodified(x, "as_svydesign")
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a survey design object."
  )

  # Export shared weights through source-target contributions.
  if (identical(sample_weight_contract(x), "shared")) {
    check_forwarded_args(
      enquos(...),
      owned = c("nest", "method", "systematic_variance"),
      accepted = svydesign_accepted_args,
      derived = svydesign_derived_args,
      forwarded_to = "survey::svydesign"
    )
    return(svydesign_from_shared_weights(
      x,
      nest = nest,
      method = method,
      systematic_variance = systematic_variance,
      dots = list(...)
    ))
  }

  phase_info <- survey_validate_phase_support(
    x,
    allow_twophase = TRUE,
    fn_name = "as_svydesign"
  )
  prev_phase <- phase_info$prev_phase
  is_twophase <- phase_info$is_twophase
  is_activation <- survey_is_activation(phase_info)

  # `subset` belongs only to the two-phase branch.
  check_forwarded_args(
    enquos(...),
    owned = c("nest", "method", "systematic_variance"),
    accepted = if (is_twophase) {
      twophase_accepted_args
    } else {
      svydesign_accepted_args
    },
    derived = if (is_twophase) {
      twophase_derived_args
    } else {
      svydesign_derived_args
    },
    forwarded_to = if (is_twophase) "survey::twophase" else "survey::svydesign"
  )

  # `nest` has no effect on a two-phase export.
  if (is_twophase && !missing(nest)) {
    cli_warn(
      c(
        "{.arg nest} has no effect when exporting a two-phase sample.",
        "i" = "It is an argument of {.fn survey::svydesign}; a two-phase
               sample is exported with {.fn survey::twophase}, which nests
               strata within clusters unconditionally."
      ),
      class = "samplyr_warning_nest_ignored"
    )
  }

  if (is_twophase) {
    method <- if (is_null(method)) {
      NULL
    } else {
      with_error_class(
        rlang::arg_match(method, c("full", "approx", "simple")),
        "samplyr_error_survey_argument"
      )
    }
  } else if (!is_null(method)) {
    cli_abort(
      "{.arg method} is only valid when converting a two-phase sample.",
      class = "samplyr_error_survey_argument"
    )
  }

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)

  df <- as.data.frame(x)

  # Inspect every contributing phase for systematic approximation.
  systematic_stages <- systematic_approximated_stages(
    design, stages_executed, df,
    phase = if (is_twophase) 2L else NULL
  )
  # A user pps object replaces Brewer's approximation at stage 1.
  user_pps <- list(...)[["pps"]]
  if (!is_null(user_pps) && !is.character(user_pps) && !isFALSE(user_pps)) {
    systematic_stages <- Filter(function(s) {
      !(identical(s$method, "pps_systematic") &&
          identical(s$stage, stages_executed[1]))
    }, systematic_stages)
  }
  if (is_twophase) {
    previous <- prev_phase$sample
    systematic_stages <- c(
      systematic_approximated_stages(
        prev_phase$design %||% get_design(previous),
        prev_phase$stages %||% get_stages_executed(previous),
        as.data.frame(previous),
        phase = 1L
      ),
      # A wave's stages are its master's, and activation is not systematic.
      if (!is_activation) systematic_stages
    )
  }
  check_systematic_variance(
    systematic_stages,
    systematic_variance,
    approximation = "srswor"
  )

  record_systematic <- function(result) {
    record_systematic_variance(
      result, systematic_stages, systematic_variance, "srswor"
    )
  }

  if (is_activation) {
    return(record_systematic(build_activation_twophase(
      x,
      prev_phase = prev_phase,
      method = method,
      dots = list(...)
    )))
  }

  if (is_twophase) {
    check_export_primary_units(x, design, stages_executed)
    record_systematic(build_twophase_svydesign(
      df = df,
      design = design,
      stages_executed = stages_executed,
      prev_phase = prev_phase,
      method = method,
      dots = list(...)
    ))
  } else {
    record_systematic(build_singlephase_svydesign(
      x,
      dots = list(...),
      nest = nest,
      relax_pps_for_bootstrap = FALSE
    ))
  }
}

#' Export a two-phase sample through survey::twophase()
#'
#' Phase 1 is the sample the phase-2 design was executed on. Each phase's
#' terms are built against the other phase's names, the phase-2 columns are
#' joined onto every phase-1 row, and a free-named indicator marks the rows
#' phase 2 reached.
#' @noRd
build_twophase_svydesign <- function(
  df,
  design,
  stages_executed,
  prev_phase,
  method,
  dots,
  call = caller_env()
) {
  phase1 <- prev_phase$sample
  # Also reject a modified phase-1 parent.
  phase1_status <- sample_realization_status(phase1)
  phase1_mods <- phase1_status$mods
  if (!phase1_status$ok) {
    cli_warn(c(
      "The phase-1 sample was modified after its execution
       ({.field {phase1_mods}} changed).",
      "i" = "{.fn survey::twophase} treats the current phase-1 rows
             as the complete phase-1 sample.",
      "i" = "If rows were removed to screen eligibility, estimates
             describe the screened population. For domain analysis,
             subset the exported design instead."
    ), class = "samplyr_warning_modified_sample")
  }
  design1 <- prev_phase$design %||% get_design(phase1)
  stages1 <- prev_phase$stages %||% get_stages_executed(phase1)
  df1 <- as.data.frame(phase1)
  df2 <- df
  design2 <- design

  unsupported1 <- phase1_pps_methods(prev_phase, kinds = "unsupported")
  if (length(unsupported1) > 0L) {
    abort_samplyr(
      c(
        "Cannot export a two-phase sample whose phase 1 was drawn with
         {.val {unsupported1}}.",
        "x" = "No linearization variance estimator is available for this
               method and its declared constraints, at either phase.",
        "i" = "The weights in {.field .weight} are exact and estimate
               totals and means correctly. It is the variance that has no
               exact route.",
        "i" = "An ultimate-cluster approximation treats phase 1's units
               as drawn with replacement. See {.help as_svydesign}."
      ),
      class = "samplyr_error_custom_random_wor_export",
      call = call
    )
  }

  pps1 <- phase1_pps_methods(prev_phase)
  if (length(pps1) > 0L) {
    abort_samplyr(
      c(
        "Cannot export a two-phase sample whose phase 1 was drawn with
         {.val {pps1}}.",
        "x" = "{.fn survey::twophase} takes no {.arg pps} specification at
               phase 1, so phase 1's unequal inclusion probabilities have
               no linearization variance there.",
        "i" = "The weights in {.field .weight} are exact and estimate
               totals and means correctly. It is the variance that has no
               exact route.",
        "i" = "An ultimate-cluster approximation treats phase 1's units
               as drawn with replacement. See {.help as_svydesign}."
      ),
      class = "samplyr_error_twophase_phase1_pps",
      call = call
    )
  }

  spec1 <- export_stage_spec(df1, design1, stages1, phase = 1L)
  spec2 <- export_stage_spec(df2, design2, stages_executed, phase = 2L)

  check_export_primary_units(phase1, design1, stages1, call = call)

  # Refused before the bridge, so a linkage problem is not reported instead.
  is_poisson <- function(e) identical(e$kind, "rs_poisson")
  if (any(vapply(c(spec1$stage, spec2$stage), is_poisson, logical(1)))) {
    abort_samplyr(c(
      "Two-phase export does not support Poisson sampling in either phase.",
      "i" = "The current two-phase bridge cannot represent the random sample-size variance."
    ), class = "samplyr_error_twophase_poisson", call = call)
  }

  bridge_vars <- resolve_phase_bridge(
    survey_key_vars(design1, stages1, df1),
    survey_key_vars(design2, stages_executed, df2),
    df1,
    df2,
    call = call
  )

  # Generated names avoid every column of both phases.
  id_info1 <- spec_survey_ids(
    spec1,
    df1,
    synthesize_unclustered = TRUE,
    prefix = "p1_",
    taken = names(df2),
    call = call
  )
  df1 <- id_info1$df
  id_info2 <- spec_survey_ids(
    spec2,
    df2,
    synthesize_unclustered = FALSE,
    prefix = "p2_",
    taken = names(df1),
    call = call
  )
  df2 <- id_info2$df
  id_vars2 <- id_info2$id_vars

  ids_formula1 <- survey_ids_formula(id_info1$id_vars)
  ids_formula2 <- survey_ids_formula(id_vars2)

  # Each phase keeps the strata of every stage it represents.
  strata1 <- spec_survey_strata(
    spec1,
    df1,
    id_stage_indices = id_info1$stage_indices,
    prefix = "p1_",
    taken = names(df2)
  )
  df1 <- strata1$df
  strata2 <- spec_survey_strata(
    spec2,
    df2,
    id_stage_indices = id_info2$stage_indices,
    prefix = "p2_",
    taken = names(df1)
  )
  df2 <- strata2$df

  fpc1 <- survey_fpc_info(
    df1,
    design1,
    stages1,
    id_info1$stage_indices,
    taken = names(df2)
  )
  df1 <- fpc1$df
  fpc2 <- survey_fpc_info(
    df2,
    design2,
    stages_executed,
    id_info2$stage_indices,
    taken = names(df1)
  )
  df2 <- fpc2$df

  joint_route <- check_twophase_phase2_family(
    spec2, dots[["pps"]], method, call = call
  )

  strata2_extra <- setdiff(strata2$vars, names(df1))
  id_vars2_extra <- setdiff(id_vars2, names(df1))

  # Phase 1 also holds .fpc_k, so phase 2's terms are renamed.
  taken_all <- union(names(df1), names(df2))
  fpc2_vars <- fpc2$fpc_vars
  fpc2_vars_renamed <- character(0)
  for (v in fpc2_vars) {
    fpc2_vars_renamed <- c(
      fpc2_vars_renamed,
      free_name(
        c(taken_all, fpc2_vars_renamed),
        sub("^\\.fpc_", ".fpc_phase2_", v)
      )
    )
  }
  fpc2_rename_map <- setNames(fpc2_vars_renamed, fpc2_vars)
  weight2_col <- free_name(
    c(taken_all, fpc2_vars_renamed),
    ".weight_phase2"
  )

  analysis_cols <- setdiff(
    names(df),
    unique(c(
      protected_sample_cols(df1, design1, stages1),
      protected_sample_cols(df2, design2, stages_executed),
      bridge_vars
    ))
  )
  # Drop stale phase-1 measurements before joining current phase-2 values.
  df1[intersect(analysis_cols, names(df1))] <- NULL
  phase2_cols_needed <- unique(
    c(
      bridge_vars,
      id_vars2_extra,
      strata2_extra,
      fpc2_vars,
      setdiff(names(df2), names(df1)),
      analysis_cols,
      ".weight"
    )
  )
  phase2_cols_needed <- intersect(phase2_cols_needed, names(df2))

  df2_join <- df2[, phase2_cols_needed, drop = FALSE]
  row2_col <- free_name(c(taken_all, fpc2_vars_renamed), ".row_phase2")
  df2_join[[row2_col]] <- seq_len(nrow(df2_join))
  if (".weight" %in% names(df2_join)) {
    names(df2_join)[names(df2_join) == ".weight"] <- weight2_col
  }
  if (length(fpc2_rename_map) > 0) {
    idx <- match(names(fpc2_rename_map), names(df2_join))
    names(df2_join)[idx] <- fpc2_rename_map
  }

  df_combined <- df1 |>
    left_join(
      df2_join,
      by = bridge_vars,
      # Backstop the validated phase bridge.
      na_matches = "never",
      relationship = "one-to-many"
    )

  phase2_col <- free_name(names(df_combined), ".phase2")
  df_combined[[phase2_col]] <- !is.na(df_combined[[weight2_col]])
  in_phase2 <- df_combined[[phase2_col]]
  if (!any(in_phase2)) {
    cli_abort(
      c(
        "Phase 2 rows could not be matched to phase 1 identifiers.",
        "i" = "Ensure a shared unique identifier is present in both phases."
      ),
      class = "samplyr_error_twophase_bridge",
      call = call
    )
  }
  row2 <- df_combined[[row2_col]][in_phase2]
  df_combined[[row2_col]] <- NULL
  df_combined <- complete_phase2_strata(df_combined, strata2)
  cond_col <- free_name(names(df_combined), ".weight_phase2_cond")
  df_combined[[cond_col]] <- ifelse(
    in_phase2,
    df_combined[[weight2_col]] / df_combined$.weight,
    NA_real_
  )
  if (any(!is.finite(df_combined[[cond_col]][in_phase2]))) {
    cli_abort(
      "Invalid phase 2 conditional weights detected after matching phases.",
      class = "samplyr_error_twophase_bridge",
      call = call
    )
  }
  prob1_col <- free_name(names(df_combined), ".prob_1")
  df_combined[[prob1_col]] <- 1 / df_combined$.weight
  prob2_col <- free_name(names(df_combined), ".prob_2")
  df_combined[[prob2_col]] <- ifelse(
    in_phase2,
    1 / df_combined[[cond_col]],
    NA_real_
  )

  # Use exact list lookup to avoid partial argument matching.
  pps_arg <- dots[["pps"]]
  dots[["pps"]] <- NULL

  fpc2_formula <- if (length(fpc2_vars_renamed) == 0) {
    NULL
  } else {
    survey_formula_from_vars(fpc2_vars_renamed)
  }

  use_weights <- !is_null(method) && method %in% c("approx", "simple")

  # One probability term per ID stage unless the FPCs carry it.
  n_id_stages <- max(
    length(id_info1$id_vars), length(id_vars2)
  )
  fpc_covers_stages <- spec_fpc_states_probabilities(
    spec1, id_info1$stage_indices, fpc1$scale
  ) &&
    spec_fpc_states_probabilities(
      spec2, id_info2$stage_indices, fpc2$scale
    )

  if (joint_route) {
    # survey reads phase 2's probabilities off `probs` next to the matrix.
    phase1_covered <- spec_fpc_states_probabilities(
      spec1, id_info1$stage_indices, fpc1$scale
    )
    check_twophase_stage_probs(
      length(id_info1$id_vars), FALSE, phase1_covered, call = call
    )
    joint <- twophase_phase2_joint(
      df, prev_phase, design2, stages_executed, spec2$stage[[1]]
    )
    probs_arg <- list(
      if (!phase1_covered) survey_formula_from_vars(prob1_col),
      survey_formula_from_vars(prob2_col)
    )
    # The matrix carries phase 2's strata and certainty units.
    strata2$formula <- NULL
    pps_arg <- list(NULL, survey::ppsmat(joint[row2, row2, drop = FALSE]))
  } else {
    check_twophase_stage_probs(
      n_id_stages, use_weights, fpc_covers_stages, call = call
    )
  }

  probs_arg <- if (joint_route) {
    probs_arg
  } else if (use_weights || fpc_covers_stages) {
    NULL
  } else {
    list(
      survey_formula_from_vars(prob1_col),
      survey_formula_from_vars(prob2_col)
    )
  }
  weights_arg <- if (use_weights) {
    list(
      stats::as.formula("~.weight"),
      survey_formula_from_vars(cond_col)
    )
  } else {
    NULL
  }

  args <- c(
    list(
      id = list(ids_formula1, ids_formula2),
      strata = list(strata1$formula, strata2$formula),
      probs = probs_arg,
      weights = weights_arg,
      fpc = list(fpc1$formula, fpc2_formula),
      subset = survey_formula_from_vars(phase2_col),
      data = df_combined,
      method = method,
      pps = pps_arg
    ),
    dots
  )
  if (twophase_across_units(df1, spec1, design2, stages_executed)) {
    warn_twophase_across_units(design1, stages1, call = call)
  }
  result <- do.call(survey::twophase, args)
  if (joint_route) {
    result <- twophase_phase2_syg(result, sum(in_phase2), call = call)
  }
  result$call <- survey_export_call("twophase", args)

  result
}

#' Make the phase-2 variance term the Sen-Yates-Grundy form
#'
#' `survey::twophase()` reads the phase-2 joint matrix in the
#' Horvitz-Thompson form only. For a PPS phase 2 that form is unbiased but
#' unstable: in a simulation with y proportional to size it was negative in
#' 3.6 % of samples and its intervals covered 86 %. Sen-Yates-Grundy is the
#' same quadratic form with each diagonal entry lowered by its row sum, as
#' survey's `ygvar.matrix()` computes it. Lowering the phase-2 and full
#' matrices by the same diagonal leaves the phase-1 term, their difference,
#' unchanged. The simulation then covered 93 % with no negative value.
#' survey's subset method zeroes only entries whose rows carry zero values,
#' so domains keep the form. An object laid out otherwise is refused rather
#' than left in the Horvitz-Thompson form.
#' @noRd
twophase_phase2_syg <- function(result, n2, call = caller_env()) {
  dcheck <- result$dcheck
  is_square <- function(m) {
    inherits(m, "Matrix") && identical(dim(m), c(n2, n2))
  }
  if (
    !inherits(result, "twophase2") ||
      !is_square(dcheck$phase2) ||
      !is_square(dcheck$full)
  ) {
    abort_samplyr(
      c(
        "Cannot export a PPS phase 2 with this version of {.pkg survey}.",
        "x" = "Its two-phase object does not hold the phase-2 joint
               probabilities where the export expects them, so the
               Sen-Yates-Grundy form cannot be applied."
      ),
      class = "samplyr_error_twophase_phase2_pps",
      call = call
    )
  }
  lower <- Matrix::Diagonal(x = Matrix::rowSums(dcheck$phase2))
  result$dcheck$phase2 <- Matrix::forceSymmetric(dcheck$phase2 - lower)
  result$dcheck$full <- Matrix::forceSymmetric(dcheck$full - lower)
  result
}

#' Build a single-phase survey.design from a tbl_sample.
#'
#' Shared by [as_svydesign.tbl_sample()] and generic replicate exports.
#' `relax_pps_for_bootstrap` only permits the existing generic approximation
#' for unsupported balanced or spatial variance families. RWYB bypasses this
#' function and preserves the original stage mechanisms.
#' @noRd
build_singlephase_svydesign <- function(
  x,
  dots,
  nest,
  relax_pps_for_bootstrap = FALSE,
  check_lonely = TRUE,
  omit_uncorrected_fpc = FALSE
) {
  rlang::local_error_call(caller_env())
  design <- get_design(x)
  stages_executed <- get_stages_executed(x)
  df <- as.data.frame(x)
  check_export_primary_units(x, design, stages_executed)

  # Only a single-stage specification reaches the resolver.
  user_pps <- survey_user_pps(dots[["pps"]], design, stages_executed, df)
  dots[["pps"]] <- user_pps$pps
  if (identical(user_pps$kind, "single_stage") && length(stages_executed) > 1L) {
    cli_warn(c(
      "{.pkg survey} applies this {.arg pps} specification to one stage.",
      "i" = "Exporting the stage-1 design only, so later-stage sampling
             variance is not represented.",
      "i" = "Omit {.arg pps}, or use {.code pps = \"brewer\"}, for the
             multi-stage design with Brewer's approximation at each PPS
             stage."
    ), class = "samplyr_warning_pps_single_stage")
    stages_executed <- stages_executed[1]
  }

  spec <- export_stage_spec(df, design, stages_executed)
  id_info <- spec_survey_ids(spec, df)
  df <- id_info$df
  ids_formula <- survey_ids_formula(id_info$id_vars)

  strata <- spec_survey_strata(
    spec,
    df,
    id_stage_indices = id_info$stage_indices
  )
  df <- strata$df

  fpc <- survey_fpc_info(df, design, stages_executed, id_info$stage_indices)
  df <- fpc$df

  if (check_lonely) {
    warn_lonely_strata(spec, design, id_info$stage_indices)
    warn_export_empty_parents(x, design, id_info$stage_indices)
  }

  resolved <- survey_resolve_pps(
    df = df,
    design = design,
    stages_executed = stages_executed,
    fpc = fpc,
    user_pps = dots[["pps"]],
    relax_pps_for_bootstrap = relax_pps_for_bootstrap
  )
  df <- resolved$df
  fpc <- resolved$fpc
  pps_arg <- resolved$pps

  if (omit_uncorrected_fpc && first_stage_uncorrected(df, fpc)) {
    fpc$formula <- NULL
  }

  dots[["pps"]] <- NULL

  args <- c(
    list(
      ids = ids_formula,
      strata = strata$formula,
      weights = stats::as.formula("~.weight"),
      fpc = fpc$formula,
      data = df,
      nest = nest,
      pps = pps_arg
    ),
    dots
  )
  result <- do.call(survey::svydesign, args)
  result$call <- survey_export_call("svydesign", args)
  result
}

#' The call survey keeps, as a caller would have typed it
#'
#' survey stores the call on the design and prints it. Through `do.call()`
#' every argument arrives as its value, so a two-phase export printed its
#' data frame in full, over a thousand lines. The data, and any other object
#' passed as a value (a `ppsmat`, say), become a name. Formulas, lists of
#' formulas and single settings stay as given, so the call still states the
#' design it built, and evaluating it with those names bound rebuilds it.
#' @noRd
survey_export_call <- function(fn, args) {
  args <- args[!vapply(args, is_null, logical(1))]
  is_formula_list <- function(a) {
    is.list(a) && !is.data.frame(a) &&
      all(vapply(a, function(e) is_null(e) || inherits(e, "formula"), NA))
  }
  for (nm in names(args)) {
    a <- args[[nm]]
    readable <- inherits(a, "formula") || is_formula_list(a) ||
      (is.atomic(a) && length(a) <= 1L)
    if (!readable) {
      args[[nm]] <- as.name(nm)
    }
  }
  as.call(c(as.name(fn), args))
}

## Overlapping frames

#' Export overlapping frames to survey's dual-frame estimator
#'
#' @description
#' Exports a [stack_frames()] collection to `survey::multiframe()`, which
#' composites the frames into one estimator. Every component is exported on
#' its own with [as_svydesign()], so each keeps its own strata, clusters and
#' variance treatment, and the compositing is applied on top of them.
#'
#' @details
#' ## The compositing factor
#'
#' `theta` is Hartley's constant factor: a unit belonging to both frames
#' contributes `theta` of its weight through the first frame and `1 - theta`
#' through the second, so the two contributions sum to one whichever frame
#' selected it. A unit belonging to one frame alone keeps its full weight.
#'
#' **`theta` belongs to the first frame of the stack.** Reversing the
#' components and asking for the same estimator means asking for
#' `1 - theta`. Only `theta = 0.5` is invariant to the order they were
#' stacked in.
#'
#' `theta = NULL` is the multiplicity estimator, which gives every frame
#' reaching a unit an equal share and is `theta = 0.5` for two frames.
#' samplyr resolves it and passes the value on. It never forwards `NULL`,
#' which survey reads as the ratio of the frames' mean sampling weights: a
#' data-dependent heuristic standing in for frame size over sample size,
#' rather than a neutral default.
#'
#' ## What is not exported this way
#'
#' `survey::multiframe()` takes two frames. A stack of more is refused here
#' rather than at the design layer, the same split [as_svydesign()] already
#' runs for phases, which chain without a bound while the export stops at
#' two.
#'
#' A two-phase component is refused as well: `as_svydesign()` exports one
#' with `survey::twophase()`, and `multiframe()` accepts only the objects
#' `survey::svydesign()` builds.
#'
#' ## The expected estimator
#'
#' `estimator = "expected"` needs the chance a unit had in every frame,
#' including the frames it was not selected from, which is what
#' [stack_frames()]'s `overlaps` argument declares. The composited weight is
#' then one over the sum of those chances, and it owes nothing to the design
#' weight, which cancels.
#'
#' samplyr hands survey the **weight** form of those chances, never the
#' probabilities, whichever way they were declared. `survey::multiframe()`
#' infers the scale from the values, reading a matrix as weights when no
#' non-zero entry in some frame falls below one, and that is reachable
#' whenever a frame is a census or its overlapping units are certainties.
#' Values at or above one are unambiguous under the same rule, so there is
#' nothing left to infer.
#'
#' It is refused for a component whose weights were shared from another
#' population: the combination is harmonic, and a realized weight-share
#' weight is a random variable rather than an inverse inclusion probability.
#'
#' @param x A `frame_stack` from [stack_frames()].
#' @param ... Forwarded to `survey::svydesign()` for every component. `pps`
#'   is refused, because a joint-probability matrix describes one component
#'   and there is no way to say which.
#' @param estimator `"constant"`, the default, is Hartley's: a fixed share of
#'   an overlapping unit's weight through each frame. `"expected"` is the
#'   Bankier and Kalton-Anderson single-frame estimator, which weights a unit
#'   by one over the sum of its chances across the frames reaching it and so
#'   needs the `overlaps` declared on the stack.
#' @param theta The compositing factor applied to the first frame's
#'   overlapping units, a single number in `[0, 1]`. `NULL`, the default, is
#'   the multiplicity estimator. It belongs to `estimator = "constant"` and is
#'   refused for the other, which would otherwise discard it.
#' @param nest,systematic_variance Passed to [as_svydesign()] for every
#'   component.
#'
#' @return A `dualframe` object from the survey package, which
#'   `survey::svytotal()`, `svymean()`, `svyglm()` and their relatives accept.
#'
#' @references
#' Hartley, H. O. (1962). Multiple frame surveys. *Proceedings of the Social
#' Statistics Section, American Statistical Association*, 203-206.
#'
#' Lohr, S. L. (2021). Multiple-frame surveys for a multiple-data-source
#' world. *Survey Methodology*, 47(2), 229-263.
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
#' population <- data.frame(
#'   person_id = 1:200,
#'   spend = stats::rnorm(200, 100, 10),
#'   in_landline = rep(c(TRUE, FALSE), times = c(140, 60)),
#'   in_cell = rep(c(FALSE, TRUE), times = c(60, 140))
#' )
#'
#' frames <- stack_frames(
#'   landline = sampling_design() |>
#'     draw(n = 40) |>
#'     execute(population[population$in_landline, ], seed = 1),
#'   cell = sampling_design() |>
#'     draw(n = 50) |>
#'     execute(population[population$in_cell, ], seed = 2),
#'   membership = c(landline = "in_landline", cell = "in_cell"),
#'   key = person_id
#' )
#'
#' # The multiplicity estimator: an overlap unit counts half in each frame.
#' svy <- as_svydesign(frames)
#' survey::svytotal(~spend, svy)
#'
#' # Hartley's estimator with a stated factor on the landline frame.
#' survey::svytotal(~spend, as_svydesign(frames, theta = 0.74))
#'
#' @seealso [stack_frames()] for building the collection,
#'   [as_svydesign()] for the per-component export
#'
#' @family multiple frames
#' @export
as_svydesign.frame_stack <- function(
  x,
  ...,
  estimator = c("constant", "expected"),
  theta = NULL,
  nest = TRUE,
  systematic_variance = c("warn", "approximate", "error")
) {
  estimator <- with_error_class(
    rlang::arg_match(estimator),
    "samplyr_error_survey_argument"
  )
  systematic_variance <- with_error_class(
    rlang::arg_match(systematic_variance),
    "samplyr_error_survey_argument"
  )
  rlang::check_installed(
    "survey",
    reason = "to convert a frame stack to a survey design object."
  )
  check_multiframe_dots(enquos(...))
  survey_validate_multiframe_support(x, "as_svydesign")
  check_multiframe_estimator(x, estimator, theta, "as_svydesign")
  theta <- if (identical(estimator, "constant")) {
    resolve_multiframe_theta(theta)
  }

  designs <- lapply(names(x), function(nm) {
    do.call(
      as_svydesign,
      c(
        list(x[[nm]]),
        list(...),
        list(nest = nest, systematic_variance = systematic_variance)
      )
    )
  })

  result <- survey::multiframe(
    designs,
    if (identical(estimator, "constant")) {
      multiframe_overlaps(x)
    } else {
      # Supply unambiguous overlap weights rather than probabilities.
      multiframe_overlap_weights(x)
    },
    estimator = estimator,
    theta = theta
  )
  attr(result, "samplyr_overlap_probability_quality") <- attr(x, "overlaps")$probability_quality
  result
}

#' Two arguments of the per-component export that a stack cannot carry
#'
#' Both would otherwise be forwarded to every component. `pps` describes one
#' design's joint probabilities, so sending it to both is a wrong variance
#' rather than an error survey would catch, and `method` belongs to the
#' two-phase path a component may not take at all.
#' @noRd
check_multiframe_dots <- function(dots, call = caller_env()) {
  nms <- names(dots) %||% rep("", length(dots))

  if ("pps" %in% nms) {
    abort_samplyr(
      c(
        "{.arg pps} describes one component, not a stack.",
        "i" = "A joint-probability matrix belongs to the design that
               produced it, and forwarding one to every component would
               compute a variance from the wrong probabilities.",
        "i" = "Export the component on its own with
               {.code as_svydesign(frames[[\"<frame name>\"]], pps = ...)} to
               inspect it."
      ),
      class = "samplyr_error_survey_multiframe_argument",
      call = call
    )
  }
  if ("method" %in% nms) {
    abort_samplyr(
      c(
        "{.arg method} is an argument of the two-phase export.",
        "i" = "A component of a stack is exported with
               {.fn survey::svydesign}, and a two-phase component is refused
               outright."
      ),
      class = "samplyr_error_survey_multiframe_argument",
      call = call
    )
  }

  check_forwarded_args(
    dots,
    owned = c("estimator", "theta", "nest", "systematic_variance"),
    # `nest` is consumed here and `pps` was refused above.
    accepted = setdiff(svydesign_accepted_args, "pps"),
    derived = svydesign_derived_args,
    forwarded_to = "survey::svydesign",
    call = call
  )
}

#' What survey's dual-frame estimator will take
#'
#' Mirrors `survey_validate_phase_support()`: the ceiling belongs to the
#' export, not to the design layer, so `stack_frames()` stays K-general and
#' the refusal is stated here.
#' @noRd
survey_validate_multiframe_support <- function(x, fn_name,
                                               call = caller_env()) {
  if (length(x) != 2L) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} only supports stacks of two frames.",
        "i" = "This stack has {length(x)}.",
        "i" = "{.fn survey::multiframe} composites two frames. The
               multiplicity estimator is defined for any number, and
               {.fn stack_frames} records any number."
      ),
      class = "samplyr_error_survey_multiframe_unsupported",
      call = call
    )
  }

  shared <- names(x)[vapply(x, function(component) {
    identical(sample_weight_contract(component), "shared")
  }, logical(1))]
  if (length(shared) > 0) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} does not composite a shared estimation weight.",
        "x" = "{cli::qty(length(shared))}Frame{?s} {.val {shared}}
               carr{?ies/y} weights shared from another population.",
        "i" = "A shared weight linearizes as its source-target contributions,
               and {.fn survey::multiframe} reads one selection probability
               per row, which a contribution is not.",
        "i" = "Export that component on its own with
               {.code as_svydesign(frames[[\"<frame name>\"]])}, which does
               take the contributions, or the whole stack with
               {.fn as_svrepdesign}."
      ),
      class = "samplyr_error_survey_weight_contract",
      call = call
    )
  }

  twophase <- names(x)[vapply(x, function(component) {
    survey_phase_info(component)$is_twophase
  }, logical(1))]
  if (length(twophase) > 0) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} cannot take a two-phase component.",
        "x" = "{cli::qty(length(twophase))}Frame{?s} {.val {twophase}}
               {?is/are} two-phase.",
        "i" = "A two-phase sample is exported with {.fn survey::twophase},
               and {.fn survey::multiframe} accepts only the designs
               {.fn survey::svydesign} builds."
      ),
      class = "samplyr_error_survey_multiframe_unsupported",
      call = call
    )
  }
  invisible(NULL)
}

#' The compositing factor, resolved rather than forwarded
#' @noRd
resolve_multiframe_theta <- function(theta, call = caller_env()) {
  # Multiplicity uses equal frame shares rather than survey's theta default.
  if (is_null(theta)) {
    return(0.5)
  }
  validate_multiframe_theta(theta, call = call)
}

#' @noRd
validate_multiframe_theta <- function(theta, call = caller_env()) {
  if (
    !is.numeric(theta) || length(theta) != 1L ||
      is.na(theta) || !is.finite(theta) || theta < 0 || theta > 1
  ) {
    abort_samplyr(
      c(
        "{.arg theta} must be a single number between 0 and 1.",
        "x" = "Got {.code {as_label(theta)}}.",
        "i" = "It is the share of an overlapping unit's weight carried by the
               first frame, and the second carries {.code 1 - theta}.",
        "i" = "Use {.code theta = NULL} for the multiplicity estimator."
      ),
      class = "samplyr_error_survey_multiframe_theta",
      call = call
    )
  }
  theta
}

#' What the expected estimator needs, and what it cannot be given
#'
#' Every refusal here prevents a number rather than standing in for something
#' unbuilt: the estimator has no meaning without overlaps, `theta` is silently
#' discarded by survey under it, and a realized weight-share weight is not an
#' inclusion probability.
#' @noRd
check_multiframe_estimator <- function(x, estimator, theta, fn_name,
                                       call = caller_env()) {
  if (identical(estimator, "constant")) {
    return(invisible(NULL))
  }

  shared <- names(x)[vapply(x, function(component) {
    identical(sample_weight_contract(component), "shared")
  }, logical(1))]
  if (length(shared) > 0) {
    abort_samplyr(
      c(
        "{.code estimator = \"expected\"} cannot take a component whose
         weights were shared from another population.",
        "x" = "{cli::qty(length(shared))}Frame{?s} {.val {shared}}
               carr{?ies/y} shared weights.",
        "i" = "The estimator combines inverse inclusion probabilities
               harmonically. A shared weight is a realized random quantity
               whose expectation carries the unbiasedness of a total, and
               putting realized values through that combination has no
               unbiasedness result behind it.",
        "i" = "Use {.code estimator = \"constant\"}, whose factors apply to
               components that are each unbiased for their own domain."
      ),
      class = "samplyr_error_survey_weight_contract",
      call = call
    )
  }

  if (is_null(attr(x, "overlaps"))) {
    abort_samplyr(
      c(
        "{.code estimator = \"expected\"} needs the chance each unit had in
         every frame.",
        "i" = "It weights a unit by {.code 1 / sum(pi)} over the frames
               reaching it, which membership alone does not give.",
        "i" = "Declare them on the stack:
               {.code stack_frames(..., overlaps = declared_overlaps(frame =
               \"column\", ..., scale = \"probabilities\"))}."
      ),
      class = "samplyr_error_survey_multiframe_overlaps",
      call = call
    )
  }

  if (!is_null(theta)) {
    abort_samplyr(
      c(
        "{.arg theta} has no meaning for {.code estimator = \"expected\"}.",
        "i" = "That estimator takes a unit's whole weight from its chances in
               the frames reaching it, so there is no share left to split.",
        "i" = "{.arg theta} belongs to {.code estimator = \"constant\"}.
               survey would discard it here without saying so."
      ),
      class = "samplyr_error_survey_multiframe_theta",
      call = call
    )
  }
  invisible(NULL)
}

#' The declared overlaps as weights, one matrix per component
#'
#' Zero still marks a frame the unit does not belong to, which is the absence
#' survey's own formula reads and removes.
#' @noRd
multiframe_overlap_weights <- function(x) {
  stats::setNames(lapply(names(x), function(nm) {
    probabilities <- frame_component_overlaps(x, nm)
    ifelse(probabilities > 0, 1 / probabilities, 0)
  }), names(x))
}

#' One membership matrix per component, in the order the frames are stacked
#'
#' survey reads the column belonging to the *other* frame by position in the
#' designs list, so these columns follow the stack's order and not the order
#' the membership mapping happened to be written in. Getting that wrong
#' produces a number rather than an error.
#' @noRd
multiframe_overlaps <- function(x) {
  membership <- attr(x, "membership")
  lapply(x, function(component) {
    matrix(
      as.numeric(frame_component_membership(component, membership)),
      nrow = nrow(component),
      ncol = length(membership),
      dimnames = list(NULL, names(membership))
    )
  })
}

#' Convert a tbl_sample to a replicate-weight survey design
#'
#' Creates a `svyrep.design` object from a `tbl_sample`. The `"rwyb"` method
#' generates Rao-Wu-Yue-Beaumont factors with the optional svrep package
#' directly from recorded stage mechanisms. Other methods first build a
#' [survey::svydesign()] object, then call [survey::as.svrepdesign()].
#'
#' @inheritParams as_svydesign
#' @param type Replicate method passed to [survey::as.svrepdesign()].
#'   One of `"auto"`, `"JK1"`, `"JKn"`, `"BRR"`, `"bootstrap"`,
#'   `"subbootstrap"`, `"mrbbootstrap"`, `"Fay"`, `"rwyb"`, or
#'   `"random_groups"`. `"rwyb"` uses svrep rather than
#'   [survey::as.svrepdesign()], and `"random_groups"` builds the weights
#'   from the replicates of `execute(reps = R)` (see Details).
#'
#'   The jackknife, BRR and Fay types are deterministic: one sample gives one
#'   set of replicate weights. The bootstrap types draw from the session's
#'   random stream, so two calls on one sample give two different standard
#'   errors unless a seed is set beforehand.
#'
#'   The spread is not small at survey's default of 50 replicates. On a
#'   90-of-600 stratified sample, twelve `"bootstrap"` calls on one sample
#'   ranged over 37% of their mean, falling to 12% at `replicates = 200` and
#'   4% at `replicates = 4000`. Raise `replicates` through `...` when the
#'   second decimal of a bootstrap standard error is going to be read.
#' @param ... Additional arguments passed to [survey::as.svrepdesign()] and
#'   on to the replicate-weight generator it selects, such as `replicates`,
#'   `fay.rho`, `fpctype`, or `mse`. Every argument must be named, and its
#'   name must be one those functions accept: `type` follows the `...` and so
#'   is matched exactly, and a near miss such as `typ` is reported rather
#'   than forwarded. `design` cannot be given, since this verb builds it
#'   from the sample. For `type = "rwyb"`, only `replicates` (default 500,
#'   integer at least 2), `mse` (default TRUE), `compress` (default TRUE) and
#'   `lonely.psu` are accepted. `lonely.psu = "certainty"` treats a
#'   final-stage stratum holding one noncertainty unit as taken with
#'   certainty, so it adds no variance of its own, whereas the default
#'   `"fail"` refuses such a stratum. For `type = "random_groups"`, only
#'   `mse` (default `FALSE`) is accepted.
#' @param systematic_variance What to do about the generic replicate weights
#'   built for `systematic` and `pps_systematic` stages. `"warn"` (default)
#'   builds them and warns once per call, naming every affected stage.
#'   `"approximate"` builds them silently, for a caller who has acknowledged
#'   the approximation, and `"error"` refuses. Naming a `type` is not an
#'   acknowledgement, since no type reproduces systematic selection. Census
#'   stages are exempt, as in [as_svydesign()]. The choice and the affected
#'   stages are recorded in the `"samplyr_systematic_variance"` attribute of
#'   the result.
#'
#' @return A `svyrep.design` object from the survey package.
#'
#' @details
#' Replicate conversion supports single-phase designs, including multistage
#' samples, shared weights and independent frame stacks. Two-phase replicate
#' export is unsupported. `"auto"` keeps survey's method choice, whether or
#' not svrep is installed.
#'
#' ## The first stage under the generic types
#'
#' `"JK1"`, `"JKn"`, `"BRR"`, `"Fay"`, `"bootstrap"` and `"subbootstrap"`
#' resample first-stage units within first-stage strata and read nothing
#' below, whereas `"mrbbootstrap"` and `"rwyb"` read every stage. A PPS
#' first stage drawn without replacement is read as drawn with replacement,
#' which errs toward too large a variance at large sampling fractions
#' (`samplyr_warning_replicate_wr_first_stage`, see [variance-estimation]).
#' Chromy's method is read as with replacement too. Unequal probabilities
#' at later stages, and a first stage drawn with replacement, reach the
#' variance through the weights and draw no warning. In a simulation with
#' `"JKn"`, a PPS second stage gave 0.98 of the true variance, as its SRS
#' counterpart did, while a Brewer first stage with a sampling fraction of
#' 0.4 gave 1.43. A certainty PSU is a stratum of its own whose stage-two
#' units are resampled, and a certainty unit with no stage below keeps its
#' full weight in every replicate. With certainty PSUs these types gave 1.1
#' to 1.2 times the true variance in a Monte Carlo, and linearization and
#' `"rwyb"` about 1.0.
#'
#' ## Rao-Wu-Yue-Beaumont bootstrap
#'
#' `type = "rwyb"` supports SRS without replacement, independent draws with
#' replacement (`srswr`, `pps_multinomial`), independent Poisson selection
#' (`bernoulli`, `pps_poisson`), and combinations of these across stages.
#' Fixed-size PPS WOR (`pps_brewer`, `pps_cps`, `pps_sampford`,
#' `pps_systematic`) uses approximate joint probabilities, with a warning.
#' Equal-probability systematic stages use the SRS approximation governed by
#' `systematic_variance`. Custom methods must declare a supported variance
#' family. Balanced, spatial, Pareto, SPS and Chromy methods have no
#' built-in RWYB mapping.
#'
#' The adapter keeps stage-specific sampling units, strata and
#' probabilities. With-replacement stages resample draw occurrences, not
#' distinct population units. Certainty units have conditional replicate
#' factor one. Noncertainty singleton strata raise
#' `samplyr_error_rwyb_singleton` whenever their variance contribution is
#' needed, except under Poisson sampling, whose variance is estimable from
#' one unit, and at the final stage under `lonely.psu = "certainty"`.
#'
#' Export refuses a selected parent with no descendant in the final sample,
#' because silently dropping it changes the earlier-stage resampling
#' distribution. When a later stage is Poisson, this check needs a complete
#' frame digest (`"summary"` or `"full"`). Empty samples cannot be
#' exported. These are export limits, and empty Poisson realizations remain
#' valid sampling outcomes.
#'
#' Replicate variances carry simulation error, so set a seed and increase
#' `replicates` for stable estimates. svrep's `estimate_boot_sim_cv()`
#' assesses that error for chosen estimates. With `mse = TRUE` the factors use
#' scale `1 / replicates`, and with `mse = FALSE` they use
#' `1 / (replicates - 1)`. The backend and stage methods are recorded in the
#' `"samplyr_replication"` attribute.
#'
#' ## Random groups
#'
#' `type = "random_groups"` takes a sample from `execute(reps = R)` as R
#' independent samples of the same design (Wolter 2007, ch. 2). The estimate
#' is the mean of the R replicate estimates, carried by the full-sample
#' weight `.weight / R`, and its variance is their spread divided by R, with
#' R - 1 degrees of freedom. A replicate that selected nothing counts as a
#' zero estimate. Nothing is assumed about the selection method, so the
#' route is right for systematic, balanced, spatial or custom stages, which
#' no other type redraws. On a frame whose period matches a systematic
#' interval it gave 0.997 of the true variance, where one sample's
#' linearization gave 0.0001. The cost is R samples.
#'
#' The replicates must vary at every stage and phase. A sample that
#' continues one realized stage with `reps`, or replicates a later phase
#' over one realized first phase, is refused
#' (`samplyr_error_random_groups_shared`), because the shared selection's
#' variance would be left out. Replicate that stage or phase too, and
#' continue each replicate. The sample must hold every replicate of its
#' execution (`samplyr_error_random_groups_input`).
#'
#' ## Variance by selection method
#'
#' The generic types are refused for Poisson sampling, which needs
#' `"rwyb"`. Bounded cube, LPM2 and SCPS have only the generic PPS bootstrap
#' of `"subbootstrap"` and `"mrbbootstrap"`, which does not recreate their
#' constraints or spatial algorithm. Only `"random_groups"` redraws a
#' systematic start. [variance-estimation] has each case and the measured
#' error.
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
#' sample <- sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 42)
#'
#' rep_svy <- as_svrepdesign(sample, type = "auto")
#' survey::svymean(~households, rep_svy)
#'
#' @references
#' Rao, J.N.K., Wu, C.F.J. and Yue, K. (1992). Some recent work on
#' resampling methods for complex surveys. *Survey Methodology*, 18(2),
#' 209-217.
#'
#' Wolter, K. M. (2007). *Introduction to Variance Estimation*, 2nd ed.
#' Springer.
#'
#' Beaumont, J.-F. and \enc{Émond}{Emond}, N. (2022). A bootstrap variance
#' estimation method for multistage sampling and two-phase sampling when
#' Poisson sampling is used at the second phase. *Stats*, 5(2), 339-357.
#' \doi{10.3390/stats5020019}
#'
#' @seealso [as_svydesign()] for linearization export,
#'   [survey::as.svrepdesign()] for the underlying conversion
#'
#' @family survey export
#' @export
as_svrepdesign <- function(x, ...) {
  UseMethod("as_svrepdesign")
}

#' @rdname as_svrepdesign
#' @export
as_svrepdesign.tbl_sample <- function(
  x,
  ...,
  type = c(
    "auto",
    "JK1",
    "JKn",
    "BRR",
    "bootstrap",
    "subbootstrap",
    "mrbbootstrap",
    "rwyb",
    "Fay",
    "random_groups"
  ),
  systematic_variance = c("warn", "approximate", "error")
) {
  systematic_variance <- with_error_class(
    rlang::arg_match(systematic_variance),
    "samplyr_error_survey_argument"
  )
  type <- with_error_class(
    rlang::arg_match(type),
    "samplyr_error_survey_argument"
  )
  # Random groups are built from the replicates themselves.
  if (!identical(type, "random_groups")) {
    check_single_replicate(x, "as_svrepdesign")
  }
  check_sample_unmodified(x, "as_svrepdesign")
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a replicate-weight survey design."
  )

  check_forwarded_args(
    enquos(...),
    owned = c("type", "systematic_variance"),
    accepted = switch(
      type,
      rwyb = c("replicates", "mse", "compress", "lonely.psu"),
      random_groups = "mse",
      svrepdesign_accepted_args
    ),
    derived = svrepdesign_derived_args,
    forwarded_to = "survey::as.svrepdesign"
  )

  if (identical(type, "random_groups")) {
    return(build_random_groups_svrepdesign(x, ...))
  }

  # Replicate the source design before applying shared weights.
  if (identical(sample_weight_contract(x), "shared")) {
    return(svrep_from_shared_weights(
      x,
      type = type,
      systematic_variance = systematic_variance,
      dots = list(...)
    ))
  }

  survey_validate_phase_support(
    x,
    allow_twophase = FALSE,
    fn_name = "as_svrepdesign"
  )

  design <- get_design(x)
  if (type == "rwyb") {
    return(build_rwyb_svrepdesign(x, systematic_variance, ...))
  }
  spec <- export_stage_spec(as.data.frame(x), design, get_stages_executed(x))
  if (any(vapply(spec$stage, function(e) e$kind, "") == "rs_poisson")) {
    abort_samplyr(c(
      "Generic replicate methods do not represent Poisson sample-size variance.",
      "i" = "Use {.code as_svrepdesign(x, type = \"rwyb\")} for independent Poisson sampling."
    ), class = "samplyr_error_poisson_replicates")
  }
  first <- spec$stage[[1]]
  pps_safe_types <- c("subbootstrap", "mrbbootstrap")
  if (
    stage_replicated_as_wr(design$stages[[first$stage]]$draw_spec,
                           first$kind) &&
      !type %in% pps_safe_types
  ) {
    cli_warn(
      c(
        "{.fn as_svrepdesign} with {.val {type}} treats the first stage,
         drawn with {.val {first$method}}, as drawn with replacement.",
        "i" = "The variance leaves out the first stage's finite population
               correction, so it errs toward too large when first-stage
               sampling fractions are large.",
        "i" = "{.val mrbbootstrap} and {.val rwyb} use every stage's
               probabilities, and {.fn as_svydesign} gives the
               linearization variance."
      ),
      class = "samplyr_warning_replicate_wr_first_stage"
    )
  }

  # Replicates do not reproduce systematic selection.
  systematic_stages <- systematic_approximated_stages(
    design, get_stages_executed(x), as.data.frame(x)
  )
  check_systematic_variance(
    systematic_stages,
    systematic_variance,
    approximation = "generic_replicates",
    fn_name = "as_svrepdesign"
  )

  svydesign_obj <- build_singlephase_svydesign(
    x,
    dots = list(),
    nest = TRUE,
    relax_pps_for_bootstrap = type %in% pps_safe_types,
    check_lonely = FALSE,
    # These types misread a first-stage no-correction term.
    omit_uncorrected_fpc = type %in% c("JK1", "JKn", "bootstrap")
  )

  call <- current_env()
  result <- tryCatch(
    if (needs_first_stage_rebuild(spec, type)) {
      svrep_from_first_stage(svydesign_obj, spec, type, ...)
    } else {
      survey::as.svrepdesign(design = svydesign_obj, type = type, ...)
    },
    error = function(e) {
      abort_samplyr(
        c(
          "{.fn as_svrepdesign} failed to convert this design to replicate weights.",
          "x" = "{conditionMessage(e)}"
        ),
        class = "samplyr_error_svrep_conversion_failed",
        call = call
      )
    }
  )

  record_systematic_variance(
    result, systematic_stages, systematic_variance, "generic_replicates"
  )
}


#' Does the first stage need its own conversion?
#'
#' These types resample first-stage units within first-stage strata and read
#' nothing below. Two first stages defeat survey's conversion. A PPS stage
#' without replacement carries a correction on the probability scale, which
#' JKn and the bootstrap refuse ("More distinct fpc values than strata"). And
#' certainty units form strata that may hold one unit, which the
#' subbootstrap rescales by n / (n - 1) into NaN weights.
#' @noRd
needs_first_stage_rebuild <- function(spec, type) {
  # "auto" is survey's choice between JK1 and JKn.
  first_stage_types <- c(
    "auto", "JK1", "JKn", "bootstrap", "subbootstrap", "BRR", "Fay"
  )
  if (!type %in% first_stage_types) {
    return(FALSE)
  }
  first <- spec$stage[[1]]
  identical(first$kind, "pps_wor") || any(first$certainty %in% TRUE)
}

#' Replicate weights from the first stage alone
#'
#' Certainty units are no random draw. A certainty PSU is a stratum of its
#' own, whose stage-two units are the ones drawn, so they become its
#' sampling units. A certainty unit with no stage below it contributes no
#' variance, so it is left out of the resampling and keeps its full weight in
#' every replicate. The remaining first-stage units are treated as drawn with
#' replacement: the probability-scale correction is dropped, which errs
#' toward a larger variance.
#' @noRd
svrep_from_first_stage <- function(svydesign_obj, spec, type, ...) {
  df <- svydesign_obj$variables
  weight <- 1 / svydesign_obj$prob
  n <- spec$n_rows
  first <- spec$stage[[1]]
  cert <- if (is_null(first$certainty)) rep(FALSE, n) else
    first$certainty %in% TRUE

  # Strata carry the linearized term's labels, because BRR orders strata by
  # sorting them. Certainty strata use survey's nested labels for the same
  # reason. Units only need their partition.
  stratum <- spec_strata_labels(first, df) %||% rep("1", n)
  psu_id <- group_ids(
    data.frame(stratum = stratum, unit = first$unit$id),
    c("stratum", "unit")
  )
  psu <- as.character(psu_id)
  below <- length(spec$stages) >= 2L
  if (below && any(cert)) {
    second <- spec$stage[[2]]
    psu_label <- paste(stratum, first$unit$id, sep = ".")
    stratum2 <- paste(
      spec_strata_labels(second, df) %||% stratum, psu_label, sep = "."
    )
    unit2 <- group_ids(
      data.frame(psu = psu_id, unit = second$unit$id),
      c("psu", "unit")
    )
    stratum[cert] <- paste(
      "certainty", psu_label[cert], stratum2[cert], sep = "\r"
    )
    psu[cert] <- paste("certainty", unit2[cert], sep = "\r")
  }
  drawn <- !(cert & !below)

  taken <- names(df)
  psu_col <- free_name(taken, ".rep_psu")
  stratum_col <- free_name(c(taken, psu_col), ".rep_stratum")
  weight_col <- free_name(c(taken, psu_col, stratum_col), ".rep_weight")
  conv <- df[drawn, , drop = FALSE]
  conv[[psu_col]] <- psu[drawn]
  conv[[stratum_col]] <- stratum[drawn]
  conv[[weight_col]] <- weight[drawn]
  first_stage <- survey::svydesign(
    ids = stats::as.formula(paste0("~", psu_col)),
    strata = stats::as.formula(paste0("~", stratum_col)),
    weights = stats::as.formula(paste0("~", weight_col)),
    data = conv,
    nest = TRUE
  )
  replicated <- survey::as.svrepdesign(first_stage, type = type, ...)
  replicated$variables[c(psu_col, stratum_col, weight_col)] <- NULL
  if (all(drawn)) {
    return(replicated)
  }

  analysis <- stats::weights(replicated, type = "analysis")
  full <- matrix(weight, nrow(df), ncol(analysis))
  full[drawn, ] <- analysis
  spliced <- survey::svrepdesign(
    variables = df,
    repweights = full,
    weights = weight,
    type = switch(replicated$type, subbootstrap = "bootstrap", replicated$type),
    rho = if (identical(replicated$type, "Fay")) replicated$rho,
    scale = replicated$scale,
    rscales = replicated$rscales,
    combined.weights = TRUE,
    mse = replicated$mse
  )
  spliced$type <- replicated$type
  spliced
}

#' Replicate weights for a sample whose weights were shared
#'
#' The ordering is the whole content of this function. Weight sharing is
#' linear in the source weights, so the recorded operator can be applied to a
#' replicate weight system exactly as it is applied to the base weights. What
#' it cannot do is act on replicate weights that were built from the target
#' rows: those rows were never sampled, and resampling them would describe a
#' selection that did not happen.
#'
#' So the source sample is replicated first, by its own design and through the
#' ordinary path, and the transformation is applied inside every replicate.
#' Every restriction the source export carries (unequal probability, Poisson,
#' systematic, balanced, spatial) reaches the user from that call, because it
#' is that call: sharing weights makes no replicate method more exact.
#'
#' @return A `svyrep.design` over the target rows.
#' @noRd
svrep_from_shared_weights <- function(x, type, systematic_variance, dots,
                                      call = caller_env()) {
  record <- prepare_weight_share_record(
    attr(x, "metadata")$weight_share,
    "A replicate export",
    call = call
  )
  check_weight_share_alignment(x, "as_svrepdesign", call = call)
  # Restore target rows to the operator's recorded order.
  pos <- align_share_rows(x, record, "as_svrepdesign", call = call)

  # Refuse unsupported shared-weight replication here.
  survey_validate_phase_support(
    record$source_sample,
    allow_twophase = FALSE,
    fn_name = "as_svrepdesign",
    advice = shared_twophase_source_advice(),
    class = "samplyr_error_share_weights_twophase_source",
    call = call
  )

  source_rep <- rlang::exec(
    as_svrepdesign,
    record$source_sample,
    type = type,
    systematic_variance = systematic_variance,
    !!!dots
  )

  analysis <- stats::weights(source_rep, type = "analysis")
  sampling <- stats::weights(source_rep, type = "sampling")

  shared_analysis <- apply_share_operator(record$operator, analysis)
  shared_base <- apply_share_operator(record$operator, sampling)

  variables <- as.data.frame(x)[pos, , drop = FALSE]
  rownames(variables) <- NULL
  variables[[".weight"]] <- shared_base

  result <- survey::svrepdesign(
    data = variables,
    repweights = shared_analysis,
    weights = shared_base,
    # Replicate columns already contain full analysis weights.
    combined.weights = TRUE,
    type = "other",
    scale = source_rep$scale,
    rscales = source_rep$rscales,
    mse = source_rep$mse
  )

  # Carry the source approximation flag to target rows.
  attr(result, "samplyr_systematic_variance") <-
    attr(source_rep, "samplyr_systematic_variance")
  attr(result, "samplyr_weight_share") <- list(
    algorithm = record$algorithm,
    version = record$version,
    within_mode = record$within_mode,
    n_source_rows = nrow(record$source_sample)
  )
  report_share_coverage(
    result,
    union_share_coverage(list(record)),
    where = " from this frame",
    call = call
  )
}

## Overlapping frames, replicate route

#' Export overlapping frames to a combined replicate-weight design
#'
#' @description
#' Exports a [stack_frames()] collection to one `svyrep.design` whose
#' replicate columns are grouped in blocks, one block per frame. In a column
#' belonging to frame `q` only frame `q` varies and every other frame stays at
#' its full-sample weight, so the combined variance is the sum of the frames'
#' own contributions, which is what independent selection from each frame
#' gives.
#'
#' Unlike the linearized route this takes any number of frames, exports a
#' component whose weights were shared from another population, and lets each
#' frame use the replicate method that suits its own design.
#'
#' @details
#' ## The compositing factor
#'
#' `theta = NULL` is the multiplicity estimator: a unit reached by `m` frames
#' contributes `1/m` of its weight through each of them. It is defined for any
#' number of frames and needs nothing stated.
#'
#' An explicit `theta` is Hartley's constant factor and applies to **two**
#' frames only, the first frame of the stack carrying `theta` of an
#' overlapping unit's weight. Above two frames the factors are per domain
#' rather than per frame, up to `2^K - 1` of them each summing to one over the
#' frames in that domain, so a single number is not a partial answer but a
#' wrong one. It is refused rather than recycled.
#'
#' ## Replicate methods may differ between frames
#'
#' `type = "auto"` picks a method per component, so a PPS frame can take
#' `"subbootstrap"` beside a stratified frame taking `"JKn"`. Each block keeps
#' its own component's `scale` and `rscales`, folded together so the combined
#' design carries `scale = 1`, and mixing methods costs nothing: the blocks do
#' not interact.
#'
#' ## Centering
#'
#' Every block is centered at the full combined estimate, which is what makes
#' the block contributions add up. `mse` is therefore not accepted: with
#' mean-centering survey would center at the mean over *all* columns and mix
#' the blocks together.
#'
#' ## The returned data
#'
#' The rows are the components' rows in stack order, with `.frame` and
#' `.domain` from [as.data.frame.frame_stack()] in front. `.weight` holds the
#' **composited** weight, so it agrees with the design's own. Every other
#' generated column is the component's and describes its selection alone.
#'
#' @param x A `frame_stack` from [stack_frames()].
#' @param ... Passed to [as_svrepdesign()] for every component and on to the
#'   replicate-weight generator, such as `replicates` or `fay.rho`. `mse` is
#'   refused.
#' @param estimator `"constant"`, the default, or `"expected"`, which needs
#'   the `overlaps` declared on the stack. See [as_svydesign.frame_stack()],
#'   which documents both. Here the expected estimator takes any number of
#'   frames, since nothing is delegated to `survey::multiframe()`.
#' @param theta The compositing factor for the first frame's overlapping
#'   units, a single number in `[0, 1]`, for two frames only. `NULL`, the
#'   default, is the multiplicity estimator and works for any number.
#' @param type,systematic_variance Passed to [as_svrepdesign()] for every
#'   component.
#'
#' @return A `svyrep.design` object from the survey package.
#'
#' @references
#' Lohr, S. L. (2021). Multiple-frame surveys for a multiple-data-source
#' world. *Survey Methodology*, 47(2), 229-263.
#'
#' Mecatti, F. (2007). A single frame multiplicity estimator for multiple
#' frame surveys. *Survey Methodology*, 33(2), 151-157.
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
#' population <- data.frame(
#'   person_id = 1:200,
#'   spend = stats::rnorm(200, 100, 10),
#'   in_landline = rep(c(TRUE, FALSE), times = c(140, 60)),
#'   in_cell = rep(c(FALSE, TRUE), times = c(60, 140))
#' )
#'
#' frames <- stack_frames(
#'   landline = sampling_design() |>
#'     draw(n = 40) |>
#'     execute(population[population$in_landline, ], seed = 1),
#'   cell = sampling_design() |>
#'     draw(n = 50) |>
#'     execute(population[population$in_cell, ], seed = 2),
#'   membership = c(landline = "in_landline", cell = "in_cell"),
#'   key = person_id
#' )
#'
#' rep_svy <- as_svrepdesign(frames, type = "bootstrap", replicates = 50)
#' survey::svytotal(~spend, rep_svy)
#'
#' @seealso [as_svydesign.frame_stack()] for the linearized route,
#'   [stack_frames()] for building the collection
#'
#' @family multiple frames
#' @export
as_svrepdesign.frame_stack <- function(
  x,
  ...,
  estimator = c("constant", "expected"),
  theta = NULL,
  type = c(
    "auto",
    "JK1",
    "JKn",
    "BRR",
    "bootstrap",
    "subbootstrap",
    "mrbbootstrap",
    "rwyb",
    "Fay"
  ),
  systematic_variance = c("warn", "approximate", "error")
) {
  estimator <- with_error_class(
    rlang::arg_match(estimator),
    "samplyr_error_survey_argument"
  )
  systematic_variance <- with_error_class(
    rlang::arg_match(systematic_variance),
    "samplyr_error_survey_argument"
  )
  type <- with_error_class(
    rlang::arg_match(type),
    "samplyr_error_survey_argument"
  )
  rlang::check_installed(
    "survey",
    reason = "to convert a frame stack to a replicate-weight survey design."
  )
  check_multiframe_rep_dots(enquos(...))
  check_multiframe_rep_support(x, "as_svrepdesign")
  check_multiframe_estimator(x, estimator, theta, "as_svrepdesign")
  factors <- multiframe_compositing_factors(x, theta, estimator)

  components <- lapply(names(x), function(nm) {
    # Report coverage once over the frame union.
    withCallingHandlers(
      do.call(
        as_svrepdesign,
        c(
          list(x[[nm]]),
          list(...),
          list(
            type = type,
            systematic_variance = systematic_variance,
            mse = TRUE
          )
        )
      ),
      samplyr_warning_unlinked_cluster = function(w) {
        rlang::cnd_muffle(w)
      }
    )
  })
  names(components) <- names(x)

  report_share_coverage(
    combine_frame_replicates(x, components, factors),
    stack_share_coverage(x),
    where = " from any frame of this stack"
  )
}

#' Coverage over the union of the components that can speak about it
#'
#' A stack with no shared component has no link structure and so nothing to
#' report. Where every component is one, the union is the clusters all of them
#' name, and the digest decides whether their silences are comparable.
#' @noRd
stack_share_coverage <- function(x) {
  records <- lapply(x, function(component) {
    attr(component, "metadata")$weight_share
  })
  if (all(vapply(records, is_null, logical(1)))) {
    return(list(status = "not_applicable", clusters = NULL))
  }
  # A direct component has no link-based orphan information.
  union_share_coverage(records)
}

#' The one argument of the per-component export a stack cannot carry
#' @noRd
check_multiframe_rep_dots <- function(dots, call = caller_env()) {
  nms <- names(dots) %||% rep("", length(dots))

  if ("mse" %in% nms) {
    abort_samplyr(
      c(
        "{.arg mse} is not accepted for a stack of frames.",
        "i" = "Each block of replicate columns is centered at the full
               combined estimate, which is what makes the frames'
               contributions add up.",
        "i" = "Mean-centering would center at the mean over every column and
               mix the blocks together."
      ),
      class = "samplyr_error_survey_multiframe_argument",
      call = call
    )
  }

  check_forwarded_args(
    dots,
    owned = c("estimator", "theta", "type", "systematic_variance"),
    accepted = setdiff(svrepdesign_accepted_args, "mse"),
    derived = svrepdesign_derived_args,
    forwarded_to = "survey::as.svrepdesign",
    call = call
  )
}

#' @noRd
check_multiframe_rep_support <- function(x, fn_name, call = caller_env()) {
  # Identify a two-phase component before its own export refuses it.
  twophase <- names(x)[vapply(x, function(component) {
    survey_phase_info(component)$is_twophase
  }, logical(1))]
  if (length(twophase) > 0) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} does not support two-phase samples.",
        "x" = "{cli::qty(length(twophase))}Frame{?s} {.val {twophase}}
               {?is/are} two-phase.",
        "i" = "Use {.fn as_svydesign} for two-phase linearization export."
      ),
      class = "samplyr_error_svrep_twophase_unsupported",
      call = call
    )
  }
  invisible(NULL)
}

#' The share of each unit's weight its own frame carries
#'
#' One vector per component, in stack order. The multiplicity form reads the
#' number of frames reaching a unit straight off the membership matrix, so it
#' needs no argument and is defined whatever the number of frames is.
#' @noRd
multiframe_compositing_factors <- function(x, theta,
                                           estimator = "constant",
                                           call = caller_env()) {
  membership <- attr(x, "membership")

  if (identical(estimator, "expected")) {
    # Replicate the fixed factor left after removing the design weight.
    return(stats::setNames(lapply(names(x), function(nm) {
      probabilities <- frame_component_overlaps(x, nm)
      1 / rowSums(probabilities) / x[[nm]][[".weight"]]
    }), names(x)))
  }

  if (is_null(theta)) {
    return(lapply(x, function(component) {
      1 / rowSums(frame_component_membership(component, membership))
    }))
  }

  validate_multiframe_theta(theta, call = call)
  if (length(x) != 2L) {
    abort_samplyr(
      c(
        "A single {.arg theta} composites two frames.",
        "x" = "This stack has {length(x)}.",
        "i" = "Above two frames the factors are per domain rather than per
               frame, up to {.code 2^K - 1} of them, each summing to one over
               the frames in that domain. One number is not a partial
               statement of that.",
        "i" = "Use {.code theta = NULL} for the multiplicity estimator, which
               is defined for any number of frames."
      ),
      class = "samplyr_error_survey_multiframe_theta",
      call = call
    )
  }

  shares <- c(theta, 1 - theta)
  out <- lapply(seq_along(x), function(q) {
    m <- frame_component_membership(x[[q]], membership)
    ifelse(rowSums(m) > 1, shares[[q]], 1)
  })
  names(out) <- names(x)
  out
}

#' One replicate system per frame, combined in blocks
#'
#' In a column belonging to frame `q` only frame `q` varies, and every other
#' frame sits at its full-sample composited weight. So the squared deviation a
#' column contributes is frame `q`'s alone, and the combined variance is the
#' sum over frames of what each would have computed by itself. That is the
#' variance independent selection from each frame gives.
#'
#' Each component's `scale` is folded into its `rscales` and the combined
#' design carries `scale = 1`. Both have to travel: a jackknife leaves `scale`
#' at one and puts its factors in `rscales`, while a bootstrap does the
#' opposite, so keeping only one of them is undetectable under one method and
#' wrong under the other.
#' @noRd
combine_frame_replicates <- function(x, components, factors) {
  frames <- names(x)
  sizes <- vapply(x, nrow, integer(1))
  analysis <- lapply(frames, function(nm) {
    stats::weights(components[[nm]], type = "analysis") * factors[[nm]]
  })
  base <- lapply(frames, function(nm) {
    stats::weights(components[[nm]], type = "sampling") * factors[[nm]]
  })
  widths <- vapply(analysis, ncol, integer(1))

  weights_vec <- unlist(base, use.names = FALSE)
  row_start <- cumsum(c(0L, sizes))
  col_start <- cumsum(c(0L, widths))
  repweights <- matrix(0, nrow = sum(sizes), ncol = sum(widths))
  for (q in seq_along(frames)) {
    cols <- seq.int(col_start[[q]] + 1L, col_start[[q + 1L]])
    for (p in seq_along(frames)) {
      rows <- seq.int(row_start[[p]] + 1L, row_start[[p + 1L]])
      repweights[rows, cols] <- if (identical(p, q)) {
        analysis[[q]]
      } else {
        matrix(base[[p]], nrow = length(rows), ncol = length(cols))
      }
    }
  }

  variables <- as.data.frame(x)
  variables[[".weight"]] <- weights_vec

  result <- survey::svrepdesign(
    data = variables,
    repweights = repweights,
    weights = weights_vec,
    # Blocks already contain full analysis weights.
    combined.weights = TRUE,
    type = "other",
    scale = 1,
    rscales = unlist(lapply(frames, function(nm) {
      components[[nm]]$scale * components[[nm]]$rscales
    }), use.names = FALSE),
    mse = TRUE
  )

  systematic <- lapply(components, attr, which = "samplyr_systematic_variance")
  if (any(!vapply(systematic, is_null, logical(1)))) {
    attr(result, "samplyr_systematic_variance") <- systematic
  }
  shared <- lapply(components, attr, which = "samplyr_weight_share")
  if (any(!vapply(shared, is_null, logical(1)))) {
    attr(result, "samplyr_weight_share") <- shared
  }
  attr(result, "samplyr_frame_stack") <- list(
    frames = frames,
    replicates = stats::setNames(widths, frames)
  )
  attr(result, "samplyr_overlap_probability_quality") <- attr(x, "overlaps")$probability_quality
  result
}

#' Convert a tbl_sample to a srvyr tbl_svy object
#'
#' Creates a [srvyr::tbl_svy] object from a `tbl_sample` by first
#' converting to a [survey::svydesign()] object via [as_svydesign()],
#' then wrapping with [srvyr::as_survey_design()].
#'
#' This method is registered on the [srvyr::as_survey_design()] generic,
#' so it is available when srvyr is loaded.
#'
#' Random-size Poisson designs (`bernoulli`, `pps_poisson`) export to a
#' `pps` survey design. These are summarized, grouped, and subset like any
#' other srvyr design, with Horvitz-Thompson Poisson variances.
#'
#' @param .data A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to [as_svydesign()].
#'
#' @return A `tbl_svy` object from the srvyr package.
#'
#' @examplesIf requireNamespace("srvyr", quietly = TRUE)
#' library(srvyr)
#'
#' sample <- sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 12345)
#'
#' # Returns a tbl_svy for use with srvyr verbs
#' svy <- as_survey_design(sample)
#' svy |>
#'   group_by(region) |>
#'   summarise(mean_hh = survey_mean(households))
#'
#' @seealso [as_svydesign()] for converting to a survey.design2 object
#'
#' @family survey export
#' @exportS3Method srvyr::as_survey_design
as_survey_design.tbl_sample <- function(.data, ...) {
  rlang::check_installed(
    "srvyr",
    reason = "to convert a tbl_sample to a srvyr tbl_svy object."
  )

  survey_validate_phase_support(
    .data,
    allow_twophase = TRUE,
    fn_name = "as_survey_design"
  )

  svydesign_obj <- as_svydesign(.data, ...)
  if (inherits(svydesign_obj, c("twophase", "twophase2"))) {
    srvyr::as_survey_twophase(svydesign_obj)
  } else {
    srvyr::as_survey_design(srvyr_dispatchable_design(svydesign_obj))
  }
}

#' Make a survey design object dispatchable by srvyr's as_survey_design
#'
#' Adds `survey.design2` behind `pps` so srvyr finds its converter and subset
#' method while survey retains PPS dispatch.
#' @noRd
srvyr_dispatchable_design <- function(x) {
  if (inherits(x, "pps") && !inherits(x, "survey.design2")) {
    cls <- class(x)
    pos <- match("survey.design", cls)
    class(x) <- append(cls, "survey.design2", after = pos - 1L)
  }
  x
}

#' Convert a tbl_sample to a srvyr replicate-weight tbl_svy object
#'
#' Creates a [srvyr::tbl_svy] replicate design from a `tbl_sample` by first
#' converting to a `svyrep.design` object via `as_svrepdesign()`,
#' then wrapping with [srvyr::as_survey_rep()].
#'
#' @param .data A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to `as_svrepdesign()`.
#'
#' @return A replicate-weight `tbl_svy` object from the srvyr package.
#'
#' @examplesIf requireNamespace("srvyr", quietly = TRUE)
#' library(srvyr)
#'
#' sample <- sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 42)
#'
#' rep_tbl <- as_survey_rep(sample, type = "auto")
#' rep_tbl |>
#'   summarise(mean_hh = survey_mean(households, vartype = "se"))
#'
#' @seealso `as_svrepdesign()` for survey replicate-weight export
#'
#' @family survey export
#' @exportS3Method srvyr::as_survey_rep
as_survey_rep.tbl_sample <- function(.data, ...) {
  rlang::check_installed(
    "srvyr",
    reason = "to convert a tbl_sample to a srvyr replicate-weight tbl_svy object."
  )

  rep_design <- as_svrepdesign(.data, ...)
  srvyr::as_survey_rep(rep_design)
}

## Linearized export of a shared-weight sample

#' Export a weight-share transformation as its source-target contributions
#'
#' The generalized weight share total is the Horvitz-Thompson total of a
#' variable derived on the *source* units:
#' `sum_i w_i y_i = sum_j I(j in S) / pi_j * z_j`, with
#' `z_j = sum_i (L_ji / L_i) y_i`.
#'
#' `z` depends on the variable being analyzed, so it cannot be formed in
#' advance. Expanding one row per contribution instead, weighted by the
#' recorded coefficient times the source weight, makes survey form `z` inside
#' each primary sampling unit for whatever variable it is given. That is exact
#' for any link structure, and needs no condition on how many source units
#' reach a target, which is what an earlier draft's cluster-level shortcut
#' would have required.
#'
#' The rows of the result are contributions, not target units, so there are
#' more of them than the transformation returned. No estimate is affected: a
#' total sums the same terms, and a mean's denominator is the estimated size
#' of the target population either way.
#' @noRd
svydesign_from_shared_weights <- function(x, nest, method,
                                          systematic_variance, dots,
                                          call = caller_env()) {
  if (!is_null(method)) {
    cli_abort(
      "{.arg method} is only valid when converting a two-phase sample.",
      call = call,
      class = "samplyr_error_survey_argument"
    )
  }
  if ("pps" %in% names(dots)) {
    abort_samplyr(
      c(
        "{.arg pps} describes the source sample, not its contributions.",
        "i" = "A joint-probability matrix is indexed by the rows of the
               sample it was computed for, and this export expands one row
               per source-target contribution.",
        "i" = "Use {.fn as_svrepdesign}, which replicates the source design
               and applies the sharing inside every replicate."
      ),
      class = "samplyr_error_share_weights_pps",
      call = call
    )
  }

  record <- prepare_weight_share_record(
    attr(x, "metadata")$weight_share,
    "A linearized export",
    call = call
  )
  check_weight_share_alignment(x, "as_svydesign", call = call)
  pos <- align_share_rows(x, record, "as_svydesign", call = call)

  source_sample <- record$source_sample
  survey_validate_phase_support(
    source_sample,
    allow_twophase = FALSE,
    fn_name = "as_svydesign",
    advice = shared_twophase_source_advice(),
    class = "samplyr_error_share_weights_twophase_source",
    call = call
  )

  target <- as.data.frame(x)[pos, , drop = FALSE]
  # Only the names execution writes are samplyr's, as in execute().
  carried <- setdiff(names(target), samplyr_reserved_names(names(target)))

  # Generated source columns avoid the target columns carried alongside.
  parts <- share_contribution_frame(
    source_sample, systematic_variance, taken = carried, call = call
  )
  operator <- record$operator
  represented_source <- unique(operator$source_row)
  missing_source <- setdiff(seq_len(nrow(source_sample)), represented_source)
  if (length(missing_source) > 0L) {
    abort_samplyr(
      c(
        "Cannot linearize a shared-weight sample when a selected source unit
         has no target contribution.",
        "x" = "{length(missing_source)} of {nrow(source_sample)} selected
               source unit{?s} {?is/are} absent from the link operator.",
        "i" = "A zero-contribution source unit still defines a sampling unit
               for variance estimation, but a contribution-row design cannot
               retain it without inventing a target row.",
        "i" = "Use {.fn as_svrepdesign}, which replicates the complete source
               design before applying the sharing operator."
      ),
      class = c(
        "samplyr_error_share_weights_zero_contribution_source",
        "samplyr_error_share_weights_source_design"
      ),
      call = call
    )
  }
  clash <- intersect(carried, parts$design_vars)
  if (length(clash) > 0) {
    abort_samplyr(
      c(
        "A target column has the name of a source design column.",
        "x" = "Conflicting: {.field {clash}}.",
        "i" = "The exported design describes the source selection, and these
               names carry its strata, its units or its population counts.
               Rename the target columns before sharing."
      ),
      class = "samplyr_error_share_weights_columns",
      call = call
    )
  }

  expanded <- parts$df[operator$source_row, , drop = FALSE]
  expanded[carried] <- target[operator$target_row, carried, drop = FALSE]
  expanded[[".weight"]] <- operator$share *
    source_sample[[".weight"]][operator$source_row]
  rownames(expanded) <- NULL

  result <- do.call(
    survey::svydesign,
    c(
      list(
        ids = parts$ids,
        strata = parts$strata,
        weights = stats::as.formula("~.weight"),
        fpc = parts$fpc,
        data = expanded,
        nest = nest
      ),
      dots
    )
  )
  result$call <- survey_export_call("svydesign", c(
    list(
      ids = parts$ids,
      strata = parts$strata,
      weights = stats::as.formula("~.weight"),
      fpc = parts$fpc,
      data = expanded,
      nest = nest
    ),
    dots
  ))

  attr(result, "samplyr_systematic_variance") <- parts$systematic
  attr(result, "samplyr_weight_share") <- list(
    algorithm = record$algorithm,
    version = record$version,
    within_mode = record$within_mode,
    n_source_rows = nrow(source_sample),
    n_contributions = length(operator$share)
  )
  report_share_coverage(
    result,
    union_share_coverage(list(record)),
    where = " from this frame",
    call = call
  )
}

#' The source design, resolved before the rows are expanded
#'
#' The order is the whole of it. `spec_survey_ids()` returns `~1` for an
#' unclustered design, meaning every row is a primary sampling unit, and
#' synthesizes an element identifier as a row counter. Either one computed
#' *after* expansion would make each contribution its own unit and split the
#' variance into pieces that are not independent. So the identifiers are
#' resolved on the source sample and carried through the expansion, and an
#' unclustered design is given an explicit source-unit identifier rather than
#' left at `~1`. Every column added takes a name free in the source sample
#' and in `taken`, the target columns the expansion carries.
#' @noRd
share_contribution_frame <- function(source_sample, systematic_variance,
                                     taken = character(0),
                                     call = caller_env()) {
  design <- get_design(source_sample)
  stages <- get_stages_executed(source_sample)
  df <- as.data.frame(source_sample)
  check_export_primary_units(source_sample, design, stages, call = call)

  systematic_stages <- systematic_approximated_stages(design, stages, df)
  check_systematic_variance(
    systematic_stages, systematic_variance,
    approximation = "srswor"
  )

  spec <- export_stage_spec(df, design, stages)
  id_info <- spec_survey_ids(spec, df, taken = taken, call = call)
  df <- id_info$df
  ids <- survey_ids_formula(id_info$id_vars)
  strata <- spec_survey_strata(
    spec, df, id_info$stage_indices, taken = taken
  )
  df <- strata$df
  fpc <- survey_fpc_info(
    df, design, stages, id_info$stage_indices, taken = taken
  )
  df <- fpc$df

  # Row expansion invalidates source-indexed Poisson and PPS variance terms.
  kinds <- vapply(spec$stage, function(e) e$kind, character(1))
  poisson_first <- identical(kinds[[1]], "rs_poisson")
  if (poisson_first || any(kinds == "pps_wor")) {
    kind <- if (poisson_first) {
      "random-size"
    } else {
      "unequal-probability"
    }
    abort_samplyr(
      c(
        "A {kind} source design cannot be linearized through its
         contributions.",
        "i" = "Its variance comes from a structure indexed by the rows of the
               source sample, and this export expands one row per
               source-target contribution, so those rows are no longer the
               units that were selected.",
        "i" = "Use {.fn as_svrepdesign}, which replicates the source design
               and applies the sharing inside every replicate."
      ),
      class = "samplyr_error_share_weights_source_design",
      call = call
    )
  }

  resolved <- survey_resolve_pps(
    df = df, design = design, stages_executed = stages,
    fpc = fpc, user_pps = NULL
  )
  df <- resolved$df
  fpc <- resolved$fpc

  # Expanded contributions cannot use `ids = ~1`.
  if (length(id_info$id_vars) == 0L) {
    unit_col <- free_name(c(names(df), taken), ".source_unit")
    df[[unit_col]] <- df[[".sample_id"]]
    ids <- survey_formula_from_vars(unit_col)
  }

  list(
    df = df,
    ids = ids,
    strata = strata$formula,
    fpc = fpc$formula,
    design_vars = unique(c(
      all.vars(ids), all.vars(strata$formula), all.vars(fpc$formula)
    )),
    systematic = list(
      approximation = if (length(systematic_stages) > 0) "srswor",
      stages = vapply(systematic_stages, function(s) s$name, character(1)),
      acknowledged = systematic_variance
    )
  )
}
