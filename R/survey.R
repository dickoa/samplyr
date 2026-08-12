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
#' @param nest If `TRUE`, relabel cluster ids to enforce nesting within
#'   strata. Passed to [survey::svydesign()]. Default is `TRUE`, which
#'   is appropriate for most complex survey designs. It has no effect on a
#'   two-phase sample, which is exported with [survey::twophase()]. Giving
#'   it there raises a warning.
#' @param systematic_variance What to do about the simple random sampling
#'   variance approximation used for equal-probability `systematic` stages.
#'   `"warn"` (default) applies it and warns once per call, naming every
#'   affected stage. `"approximate"` applies it silently for a caller who has
#'   acknowledged it, while `"error"` refuses. Census stages are exempt, since a
#'   stage that took everything within reach contributes no variance, and
#'   `pps_systematic` is unaffected, having its own treatment. The choice and
#'   the affected stages are recorded on the returned object in the
#'   `"samplyr_systematic_variance"` attribute. [as_svrepdesign()] takes the
#'   same argument for its own approximation of these stages.
#' @param method For two-phase samples, the variance method passed to
#'   [survey::twophase()]. One of `"full"`, `"approx"`, or `"simple"`.
#'   This argument is only accepted for two-phase samples.
#'
#' @return A `survey.design2` object for single-phase and multi-stage samples,
#'   or a `twophase`/`twophase2` object for two-phase samples.
#'
#' @details
#' The conversion maps samplyr's design specification to the arguments
#' expected by [survey::svydesign()]:
#'
#' - **Cluster ids** (`ids`): one formula term per executed stage.
#'   Clustered stages use the `cluster_by()` variable and when a stage
#'   clusters by several variables, their combination (which execution
#'   treats as a single cluster id) is collapsed into one synthesized
#'   interaction column, because [survey::svydesign()] reads each
#'   formula term as a separate sampling stage. A final unclustered
#'   stage (elements sampled within the previous stage's clusters) gets
#'   a synthesized row-identity column so that its sampling variance is
#'   represented. For WR/PMR stages, the `.draw_k` column is used as
#'   the sampling unit identifier instead (each draw is treated as an
#'   independent unit for Hansen-Hurwitz variance estimation).
#' - **Strata** (`strata`): one term per stage, aligned with `ids`.
#'   A stage stratified by several variables exports their
#'   cross-classification as a single synthesized interaction column
#'   (survey silently ignores extra variables within a stage's term).
#'   Trailing unstratified stages are omitted and unstratified stages
#'   before a stratified stage get a constant placeholder column.
#' - **Weights** (`weights`): the `.weight` column i.e. the compound weight
#'   across all stages (i.e., the product of per-stage weights
#'   \eqn{w = \prod w_k = \prod 1/\pi_k}{w = prod(1/pi_k)}).
#'   This is the inverse of the overall inclusion probability and is the
#'   correct weight for design-based point estimation
#'   (\eqn{\hat{Y} = \sum w_i y_i}{Y-hat = sum(w_i * y_i)}).
#' - **FPC** (`fpc`): one term per stage, aligned with `ids`. Because
#'   [survey::svydesign()] requires every FPC term on the same scale,
#'   two encodings are used:
#'   - **Count scale** (designs without unequal-probability WOR
#'     stages): `.fpc_k` (the stratum population count \eqn{N_h}) is
#'     passed for equal-probability WOR stages. A synthetic `Inf`
#'     column (no correction, Hansen-Hurwitz variance) is used for
#'     WR/PMR stages and for random-size Poisson stages after the first.
#'   - **Fraction scale** (multi-stage designs with a PPS WOR,
#'     balanced, or custom WOR stage): every WOR stage passes its
#'     per-unit stage sampling fraction
#'     \eqn{1 / w_k = \pi_k}{1/w_k = pi_k}. WR/PMR and later Poisson
#'     stages pass 0 (no correction).
#'
#'   A single-stage PPS WOR design passes \eqn{\pi_i}{pi_i} directly,
#'   which survey interprets as inclusion probabilities.
#'
#' ## Multi-stage designs
#'
#' Every executed sampling stage is represented in the exported design:
#' one `ids` term, one `fpc` term, and (when stratified) one `strata`
#' term per stage, so [survey::svydesign()] represents the multi-stage
#' structure in its linearization (Sarndal et al. 1992, ch. 4.3). Exactness
#' still depends on the variance treatment available for each method. A
#' design whose first stage is a census of PSUs correctly attributes
#' all variance to the later stages.
#'
#' Operational execution does not change this classification. For example,
#' `stage1 <- execute(design, psu_frame, stages = 1)` followed by
#' `sample <- execute(stage1, listing_frame)` remains one multi-stage design.
#' The partial `tbl_sample` stores the same design plus the realized PSU
#' selection and the final sample records all executed stages and
#' `as_svydesign()` calls [survey::svydesign()], not [survey::twophase()].
#'
#' A two-phase sample has a different provenance. A *new* phase-2
#' `sampling_design` is executed with the phase-1 `tbl_sample` as its frame,
#' for example `phase2 <- execute(design2, phase1)`. That execution records a
#' previous-phase link, and `as_svydesign()` calls [survey::twophase()].
#'
#' A materialized wave, `execute(master, wave = t)`, is the other two-phase
#' provenance. Its second phase is the panel activation rather than an
#' executed design: within a frozen block the master took a simple random
#' sample without replacement of the realized quota, so the blocks are the
#' phase-2 strata and their sizes the phase-2 population counts. The master
#' is retained as the first phase and supplies the rows the wave did not
#' keep, which [survey::twophase()] needs to build that phase. Columns added
#' to the wave for analysis are carried into the exported design. Columns the
#' master already has keep the master's values.
#'
#' The activation is exact, but the first phase is only as expressible as
#' [survey::twophase()] allows, which takes no `pps` specification there. A
#' wave of a master with unequal inclusion probabilities is therefore refused
#' rather than exported with a with-replacement approximation: `pps_*`
#' methods, `cube`, first-stage `pps_poisson`, and the spatial methods, whose
#' variance family is unsupported at either phase. Waves of equal-probability
#' masters export, stratified, clustered, multistage and with-replacement
#' alike. `as_svrepdesign()` does not build replicate weights for any
#' two-phase sample, a wave included.
#'
#' An unclustered element-sampling stage *followed by further stages* is not
#' nested cluster sampling (the later selections are conditional on the realized
#' element sample, i.e. phase sampling). It can't be represented currently and
#' `as_svydesign()` raises an error. Express such designs as two-phase samples instead.
#' Execute the element stage under its first-phase design, then execute a new
#' second-phase design with that sample as its frame. This exports via [survey::twophase()].
#'
#' Concretely, for a two-stage stratified-cluster design with a final
#' element stage, the exported call is equivalent to:
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
#' ## Modified samples and domain analysis
#'
#' The conversion requires a sample whose rows still match the executed
#' design. A `tbl_sample` whose row set was changed after [execute()]
#' (rows removed by [dplyr::filter()] or `[`, added, or duplicated by a
#' join) or whose internal design columns (`.weight`, `.weight_k`,
#' `.fpc_k`, ...) were overwritten, dropped, or renamed is marked as
#' modified, and `as_svydesign()` raises an error. The check is
#' authoritative, not just mark-based: the sample is verified against
#' an integrity record (row count and a hash of the weights, design
#' metadata, and strata/cluster columns) stored at execution, so
#' modifications through routes the dplyr hooks cannot see (base
#' assignment, `rbind()`, vctrs operations, third-party verbs) are
#' also caught, and an overwrite that left every value identical
#' passes. Physically dropping out-of-domain
#' rows before conversion is not equivalent to domain estimation, in that
#' situation the point estimate can agree, but its variance estimate is
#' generally wrong and is often too small because the domain sample size is
#' random under the design.
#'
#' For subpopulation estimates, convert the full sample first and then
#' subset the design, which applies the proper domain estimator:
#' ```r
#' svy <- as_svydesign(sample)
#' survey::svymean(~y, subset(svy, domain))
#' # or with srvyr:
#' as_survey_design(sample) |> filter(domain) |> summarise(...)
#' ```
#'
#' Row reordering, one-to-one joins, and adding ordinary data columns
#' do not mark the sample. Extracting one complete replicate from a
#' replicated execution (`filter(.replicate == r)`) is verified against
#' the execution metadata and remains supported.
#'
#' ## Equal-probability systematic sampling
#'
#' `systematic` stages are exported with the SRSWOR variance estimator,
#' the standard approximation for systematic sampling. Depending on the
#' frame ordering (see the `control` argument of [draw()]), the true
#' variance can be smaller (favorable ordering) or larger (periodic
#' ordering) than this estimate.
#'
#' The size of that gap is worth stating, because it is not always small. A
#' systematic design with interval \eqn{k}{k} has only \eqn{k}{k} distinct
#' samples per stratum, so its variance for one particular frame is a fixed
#' quantity that need not sit near the SRSWOR value. Measured over repeated
#' draws, 90 per stratum from 1800 with an interval of 20:
#'
#' | frame order | reported / true variance | coverage of a 95% interval |
#' |---|---|---|
#' | random | 0.73 | 90.2% |
#' | ordered by a trend | 1.76 | 100.0% |
#' | period equal to the interval | 0.0006 | 9.8% |
#'
#' A favourable ordering is therefore conservative, which is the usual reason
#' for choosing one, and a frame whose structure resonates with the sampling
#' interval is not merely imprecise but reports intervals that almost never
#' cover. Even an unstructured ordering is not guaranteed close. Where the
#' frame may carry periodicity, prefer a randomized order or a design whose
#' variance is estimable, and treat the exported standard error for a
#' systematic stage as an approximation whose direction depends on the frame.
#'
#' Because a returned `survey.design` carries no sign that its variance model
#' is approximate, the export says so once: see `systematic_variance`. The
#' approximation is still supplied, since one systematic sample generally does
#' not identify its own design variance, and refusing would leave the caller
#' with nothing.
#'
#' Replicate weights are no way around this. [as_svrepdesign()] takes the same
#' `systematic_variance` argument, because no replicate type on offer
#' reconstructs a systematic sample's random start or its dependence on frame
#' order. Each resamples the realized sample as though its units had been
#' drawn independently within strata. Requesting a particular type is
#' therefore not an acknowledgement, and does not silence the condition. The
#' figures in the table above were measured for the linearization export and
#' are not a measurement of the replicate one.
#'
#' ## Variance estimation for PPS designs
#'
#' For fixed-size PPS without-replacement stages (`pps_brewer`,
#' `pps_systematic`, `pps_cps`, `pps_sampford`, `pps_sps`, `pps_pareto`),
#' variance is estimated by default using Brewer's approximation (`pps =
#' "brewer"` in survey's terminology), which approximates the joint inclusion
#' probabilities from the marginal inclusion probabilities. Here Brewer names
#' the variance estimator, not the selection algorithm e.g Sampford selection
#' receives this default treatment. This is the approximation
#' described by Berger (2004). Its accuracy depends on the sampling design and
#' population. It is not an exact substitute for joint inclusion probabilities.
#'
#' For supported methods, you can instead compute joint inclusion
#' probabilities using [joint_expectation()] and pass them via `pps =
#' survey::ppsmat(joint_matrix)`. The matrix is exact for CPS, Sampford,
#' systematic PPS, and Poisson selection. Generalized Brewer, SPS, Pareto, and
#' unconstrained cube use the documented high-entropy approximation.
#'
#' ## Spatial and constrained balanced methods
#'
#' Bounded cube, LPM2, and SCPS alter pairwise selection behavior beyond the
#' available linearization approximation. [as_svydesign()] therefore refuses
#' these designs, and [joint_expectation()] does not provide a matrix for them.
#' Use `as_svrepdesign(type = "subbootstrap")` or `"mrbbootstrap"` for a
#' generic PPS bootstrap approximation. These replicates do not recreate the
#' count constraints or spatial algorithm and are not an exact,
#' design-specific variance estimator.
#'
#' ## Random-size Poisson methods
#'
#' Methods `bernoulli` and `pps_poisson` select units independently
#' with known marginal inclusion probabilities, so the realized
#' sample size is random. The standard SRSWOR variance estimator
#' is not appropriate, and Brewer's approximation (designed for
#' fixed-size PPS) understates the variance. Instead, these
#' methods are exported with `pps = survey::poisson_sampling(pi)`,
#' which produces the Horvitz-Thompson Poisson variance estimator
#' \eqn{\hat V = \sum_{i \in S} (1 - \pi_i) / \pi_i^2 \cdot y_i^2}
#' described in Sarndal, Swensson and Wretman (1992), section 2.8.
#'
#' This applies under the following conditions.
#'
#' - Single-stage designs (no `cluster_by()`, or `cluster_by()` with
#'   one row per sampled cluster) are exported with `poisson_sampling()`
#'   and produce the exact Horvitz-Thompson Poisson variance.
#' - Multi-stage designs with a random-size Poisson method at stage k > 1
#'   omit the finite-population correction at the Poisson stage (the same
#'   handling used for with-replacement methods). The Poisson stage is
#'   treated as sampled with replacement, which is mildly conservative.
#' - Multi-stage designs with a random-size Poisson method at stage 1
#'   are not supported by `survey::svydesign()`, which rejects multi-stage
#'   designs when the `pps` argument is set. Such designs raise an error
#'   suggesting `as_svrepdesign(type = "subbootstrap")`.
#' - Single-stage designs that use `cluster_by()` with multiple rows per
#'   sampled cluster (for example a household listing within sampled EAs)
#'   raise an error. `survey::poisson_sampling()` treats rows as
#'   independent and does not honor within-cluster correlation. Use
#'   `as_svrepdesign(type = "subbootstrap")` for these designs.
#' - Custom methods registered with `fixed_size = FALSE`
#'   (`sondage::register_method()`) are also random-size, but samplyr
#'   cannot verify that their selections are independent across units,
#'   which the Poisson estimator requires. The method author can settle
#'   this at registration: a method registered with
#'   `variance_family = "poisson"` asserts independent selections and is
#'   exported through `poisson_sampling()` exactly like the built-ins
#'   above. Undeclared methods raise an error. If you know the method is
#'   Poisson-type, pass the probabilities explicitly:
#'   `as_svydesign(x, pps = survey::poisson_sampling(1 / x$.weight))`,
#'   or use `as_svrepdesign(type = "subbootstrap")`.
#'
#' ## Declared variance families for custom methods
#'
#' `sondage::register_method()` accepts a `variance_family` declaration
#' (`"srs"`, `"pps_brewer"`, `"poisson"`, `"wr"`, `"unsupported"`).
#' When present it overrides the classification samplyr would otherwise
#' infer from the method's `type` and `fixed_size`: `"srs"` receives the
#' equal-probability treatment (count-scale FPC), `"pps_brewer"` the
#' fixed-size PPS treatment (Brewer approximation), `"poisson"` exact
#' Poisson linearization, and `"wr"` the with-replacement treatment.
#' A method declared `"unsupported"` cannot be linearized at all:
#' `as_svydesign()` refuses with an error and
#' `as_svrepdesign(type = "subbootstrap")` remains the escape hatch.
#'
#' ## Chromy's sequential PPS method (PMR)
#'
#' `pps_chromy` is classified as a *Probability Minimum Replacement*
#' (PMR) method which is neither with-replacement nor without-replacement.
#' Each unit receives exactly \eqn{\lfloor E(n_i) \rfloor} or
#' \eqn{\lfloor E(n_i) \rfloor + 1} hits, where
#' \eqn{E(n_i) = n \cdot \textrm{mos}_i / \sum \textrm{mos}}.
#' When all expected hit counts are below 1, this reduces to WOR,
#' otherwise large units receive multiple hits.
#'
#' For variance estimation, Chromy (2009) recommends the
#' Hansen-Hurwitz (with-replacement) approximation rather than
#' exact pairwise expectations, which he found "quite variable."
#' Chauvet (2019) confirmed this in simulation. Accordingly,
#' `as_svydesign()` treats `pps_chromy` stages like
#' with-replacement stages (no FPC, no pps argument).
#'
#' Note that `survey::ppsmat()` is **not** valid for the general
#' PMR case. The survey package reads \eqn{\pi_i} from the diagonal
#' of the joint matrix, but for PMR the diagonal contains
#' \eqn{E(n_i^2)}, which differs from \eqn{E(n_i)} when units
#' receive multiple hits. The generalized Sen-Yates-Grundy variance
#' requires \eqn{E(n_i) E(n_j) - E(n_i n_j)} as the pairwise
#' weight (Chromy 2009, eq. 5), not \eqn{E(n_i^2) E(n_j^2) - E(n_i n_j)}.
#'
#' ## Certainty stratum (take-all units)
#'
#' For stages exported under the PPS without-replacement (Brewer)
#' treatment, units with inclusion probability \eqn{\pi_i = 1}{pi_i = 1}
#' are placed in a separate take-all stratum. This covers every route to
#' probability one, whether a `certainty_size` or `certainty_prop` rule
#' named the unit or the probability calculation capped it, and it covers
#' balanced (cube) designs alongside the PPS methods. Random-size designs
#' (`bernoulli`, `pps_poisson`) keep the Poisson treatment and form no
#' take-all stratum, even when some probabilities equal one. This follows
#' the standard practice from Cochran (1977, ch. 11) and Sarndal et al.
#' (1992, ch. 3.5): the take-all stratum contributes zero variance (it is
#' a census) and does not inflate the degrees of freedom for the
#' probability stratum.
#'
#' Splitting certainty units out of a user stratum can leave a single
#' probability unit behind. Such a stratum has no estimable
#' within-stratum variance, and `survey` signals a lonely PSU rather than
#' returning a number. This reflects the design: set
#' `options(survey.lonely.psu = "adjust")` for the conservative
#' population-mean centering, or collapse the affected strata before
#' export.
#'
#' For stages using with-replacement methods (`srswr`,
#' `pps_multinomial`), the finite population correction is omitted
#' and the `.draw_k` column (sequential draw index) is used as the
#' sampling unit identifier for Hansen-Hurwitz variance estimation.
#'
#' The `survey` package is required but not imported. It must be
#' installed to use this function.
#'
#' @references
#' Berger, Y.G. (2004). A Simple Variance Estimator for Unequal
#' Probability Sampling Without Replacement. *Journal of Applied
#' Statistics*, 31, 305-315.
#'
#' Brewer, K.R.W. (2002). *Combined Survey Sampling Inference
#' (Weighing Basu's Elephants)*. Chapter 9.
#'
#' Chauvet, G. (2019). Properties of Chromy's sampling procedure.
#' *arXiv:1912.10896*.
#'
#' Chromy, J.R. (2009). Some Generalizations of the Horvitz-Thompson
#' Estimator. *JSM Proceedings, Survey Research Methods Section*.
#'
#' Cochran, W.G. (1977). *Sampling Techniques*. 3rd edition. Wiley.
#'
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
#' # Two-stage cluster sample with PPS first stage
#' sample <- sampling_design() |>
#'   add_stage() |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = households) |>
#'   add_stage() |>
#'     draw(n = 12) |>
#'   execute(bfa_eas, seed = 2025)
#'
#' # Default: Brewer variance approximation
#' svy <- as_svydesign(sample)
#'
#' # Exact: compute joint probabilities from frame
#' jip <- joint_expectation(sample, bfa_eas, stages = 1)
#' svy_exact <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
#'
#' @seealso [execute()] for producing tbl_sample objects,
#'   [survey::svydesign()] for the underlying function,
#'   [as_survey_design.tbl_sample] for converting directly to a srvyr `tbl_svy`,
#'   `as_svrepdesign()` for replicate-weight export
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

#' @noRd
survey_validate_phase_support <- function(
  sample,
  allow_twophase = TRUE,
  fn_name = "as_svydesign",
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
    abort_samplyr(
      c(
        "{.fn {fn_name}} does not support two-phase samples.",
        "i" = "Use {.fn as_svydesign} for two-phase linearization export."
      ),
      class = "samplyr_error_svrep_twophase_unsupported",
      call = call
    )
  }

  phase_info
}

## The systematic variance approximation

# `systematic` selection generally does not identify its own design variance
# from one sample, which is why exporting it with the SRSWOR estimator is
# standard practice. What is not defensible is returning an ordinary
# survey.design silently: nothing downstream then records that the variance
# model is an approximation, and the approximation can fail badly rather than
# merely conservatively. On a frame whose period matches the sampling interval
# the reported variance was measured at 0.0006 of the truth, with 95%
# intervals covering 9.8% of the time.
#
# Replicate weights are in the same position for a different reason: a generic
# resampler perturbs the realized sample without reproducing the random start
# or the frame order that produced it, so it estimates the variance of a
# design that was not run.
#
# So the approximation is still supplied by either export, and the caller is
# told once.

#' Equal-probability systematic stages whose variance is being approximated
#'
#' `pps_systematic` is excluded: its treatment is Brewer's, with its own
#' semantics. A census stage is excluded too, since a stage that took
#' everything within reach contributes no variance for the approximation to
#' get wrong.
#' @noRd
systematic_approximated_stages <- function(design, stages_executed, df,
                                           phase = NULL) {
  affected <- vapply(stages_executed, function(stage_idx) {
    method <- design$stages[[stage_idx]]$draw_spec$method
    if (!identical(method, "systematic")) {
      return(FALSE)
    }
    weight_col <- paste0(".weight_", stage_idx)
    if (!weight_col %in% names(df)) {
      return(TRUE)
    }
    # A sampling fraction of one at every unit: no variance to approximate.
    !isTRUE(all.equal(unname(df[[weight_col]]), rep(1, nrow(df))))
  }, logical(1))

  lapply(stages_executed[affected], function(stage_idx) {
    label <- design$stages[[stage_idx]]$label
    list(
      stage = stage_idx,
      phase = phase,
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
  approximation <- match.arg(approximation)
  named <- vapply(stages, function(s) {
    if (is_null(s$phase)) s$name else paste0(s$name, ", phase ", s$phase)
  }, character(1))

  estimator <- if (identical(approximation, "srswor")) {
    c(
      "x" = "{.fn {fn_name}} is using a simple random sampling variance
             approximation for {cli::qty(length(named))}{?it/them}.",
      "i" = "Frame ordering or periodicity can make standard errors far too
             small or too large. A frame whose period matches the sampling
             interval has been measured at 0.0006 of the true variance, with
             95% intervals covering 9.8% of the time. See
             {.help as_svydesign}."
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
             {.help as_svydesign}."
    )
  }

  bullets <- c(
    "{cli::qty(length(named))}{?A stage/Stages} of this design used
     equal-probability systematic sampling: {named}.",
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
  attr(result, "samplyr_systematic_variance") <- list(
    approximation = if (length(stages) > 0) approximation else NULL,
    stages = vapply(stages, function(s) s$name, character(1)),
    acknowledged = choice
  )
  result
}

## Export of a materialized wave

# A wave is a two-phase sample whose second phase is the panel activation.
# Phase 1 is the master, exported the way any first phase is. Phase 2 is not
# an executed design: the units were assigned when the master was drawn, so
# its identifiers, strata and population counts come from the frozen
# assignment record rather than from a sampling_design. Within a block the
# activation is a simple random sample without replacement of the realized
# quota, which is what makes the phase exact.

#' @noRd
survey_is_activation <- function(phase_info) {
  identical(phase_info$prev_phase$transition, "panel_activation")
}

#' A wave must carry the master it was activated from
#'
#' [survey::twophase()] builds the first-phase design from every phase-1 row
#' and marks the active ones with `subset`, so the master's rows are needed
#' and nothing can reconstruct the units the wave did not keep.
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
#' identifier there), and it does not when a term is infinite, which is what a
#' with-replacement stage has instead of a population count. In both cases the
#' phase's own weight is the exact probability and is passed instead.
#' @noRd
fpc_states_all_probabilities <- function(
  df,
  formula,
  fpc_vars,
  stage_indices,
  stages_executed
) {
  # survey_fpc_info() falls back to the first executed stage when no stage
  # contributed an identifier, which is what a lone unclustered stage does:
  # there the one term does state the one probability.
  covered <- if (length(stage_indices) == 0) {
    stages_executed[1]
  } else {
    stage_indices
  }
  !is_null(formula) &&
    length(covered) == length(stages_executed) &&
    all(vapply(
      fpc_vars,
      function(v) all(is.finite(df[[v]])),
      logical(1)
    ))
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
  length(probability) > 0L && all(probability == 1)
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
  master <- prev_phase$sample
  design1 <- prev_phase$design %||% get_design(master)
  stages1 <- prev_phase$stages %||% get_stages_executed(master)
  metadata <- attr(x, "metadata")

  # An identity activation has no second-phase variance. Exporting it through
  # twophase() also fails when phase-1 strata are singletons.
  if (is_null(metadata$panel_assignment) || activation_is_identity(metadata)) {
    return(build_singlephase_svydesign(
      x,
      dots = dots,
      nest = TRUE,
      relax_pps_for_bootstrap = FALSE
    ))
  }

  check_wave_master_identity(metadata, master, call = call)

  df1 <- as.data.frame(master)

  id_info <- survey_id_info(
    design1,
    stages1,
    df1,
    synthesize_unclustered = TRUE,
    prefix = "p1_"
  )
  df1 <- id_info$df
  strata1 <- survey_strata_info(
    df1,
    design1,
    stages1,
    mode = "first_stage",
    prefix = "p1_"
  )
  df1 <- strata1$df
  fpc1 <- survey_fpc_info(df1, design1, stages1, id_info$stage_indices)
  df1 <- fpc1$df

  check_activation_phase1_supported(design1, stages1, fpc1, call = call)

  phase2 <- activation_phase2_columns(df1, x, metadata, call = call)
  df1 <- phase2$df
  cols <- phase2$cols

  # Variables the analysis added to the wave. Columns the master already
  # carries keep the master's values: they are the phase-1 sample's own.
  carried <- setdiff(names(as.data.frame(x)), names(df1))
  if (length(carried) > 0) {
    at <- match(df1$.sample_id, x$.sample_id)
    for (nm in carried) {
      df1[[nm]] <- as.data.frame(x)[[nm]][at]
    }
  }

  pps_arg <- dots[["pps"]]
  dots[["pps"]] <- NULL

  use_weights <- !is_null(method) && method %in% c("approx", "simple")
  n_id_stages <- max(length(id_info$id_vars), 1L)
  # Phase 2 always states its correction, so only the master can fail to.
  fpc_covers_stages <- fpc_states_all_probabilities(
    df1,
    fpc1$formula,
    fpc1$fpc_vars,
    id_info$stage_indices,
    stages1
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

  do.call(
    survey::twophase,
    c(
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
  )
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
    # Custom WOR methods: fixed-size follows the Brewer (PPS-WOR)
    # strategy. Random-size follows the Poisson strategy.
    if (identical(draw_spec$method_fixed, FALSE)) {
      return("rs_poisson")
    }
    return("pps_wor")
  }
  if (identical(draw_spec$method_type, "balanced")) {
    # Custom balanced methods follow the built-in balanced (cube)
    # strategy. Without this branch they would fall through to
    # "equal_wor" (the method field holds the custom name, so the
    # name test below never matches) and export with SRS variance.
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
#' phase-2 descendants may repeat that bridge.
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

  # A missing value is not an identity. Phase-2 rows must all name the phase-1
  # unit they descend from, and a missing key on either side never links.
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

  # Uniqueness is only required of the phase-1 rows a phase-2 row can reach.
  # A phase-1 key repeated among rows no phase-2 row matches is irrelevant,
  # and repeated phase-2 keys are expected: one phase-1 unit may have many
  # descendants.
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
        "x" = "{.val {format_key_preview(orphans)}}",
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
        "x" = "{.val {format_key_preview(ambiguous)}} match more than one
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

#' Per-stage survey sampling-unit identifiers.
#'
#' Builds one ID term per represented stage. Multi-hit stages use draw IDs,
#' clustered stages use cluster keys, and terminal element stages may receive
#' synthesized row IDs. `prefix` separates two-phase columns.
#' @noRd
survey_id_info <- function(
  design,
  stages_executed,
  df,
  synthesize_unclustered = TRUE,
  prefix = "",
  call = rlang::caller_env()
) {
  id_vars <- character(0)
  stage_indices <- integer(0)
  n_exec <- length(stages_executed)

  for (pos in seq_len(n_exec)) {
    stage_idx <- stages_executed[pos]
    stage_spec <- design$stages[[stage_idx]]
    draw_col <- paste0(".draw_", stage_idx)

    if (is_multi_hit_method(stage_spec$draw_spec) && draw_col %in% names(df)) {
      id_vars <- c(id_vars, draw_col)
      stage_indices <- c(stage_indices, stage_idx)
    } else if (!is_null(stage_spec$clusters)) {
      cluster_vars <- stage_spec$clusters$vars
      if (length(cluster_vars) == 1L) {
        id_var <- cluster_vars
      } else {
        id_var <- paste0(".", prefix, "id_", stage_idx)
        df[[id_var]] <- group_ids(df, cluster_vars)
      }
      id_vars <- c(id_vars, id_var)
      stage_indices <- c(stage_indices, stage_idx)
    } else if (synthesize_unclustered && n_exec > 1L) {
      if (pos < n_exec) {
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
      id_var <- paste0(".", prefix, "id_", stage_idx)
      df[[id_var]] <- seq_len(nrow(df))
      id_vars <- c(id_vars, id_var)
      stage_indices <- c(stage_indices, stage_idx)
    }
    # Single executed unclustered stage: ids = ~1 (element sampling).
  }

  list(df = df, id_vars = id_vars, stage_indices = stage_indices)
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
    cli_abort("A survey formula requires at least one variable.", call = NULL)
  }
  terms <- lapply(vars, rlang::sym)
  rhs <- Reduce(function(x, y) call("+", x, y), terms)
  rlang::new_formula(NULL, rhs)
}

#' Per-stage survey strata terms.
#'
#' Builds one term per represented stage, collapsing multi-column strata to an
#' interaction and inserting placeholders for positional alignment. Two-phase
#' mode keeps only the first stage.
#' @noRd
survey_strata_info <- function(
  df,
  design,
  stages_executed,
  id_stage_indices = integer(0),
  mode = c("multistage", "first_stage"),
  prefix = ""
) {
  mode <- match.arg(mode)
  first_stage_idx <- stages_executed[1]

  # Certainty handling follows the resolved variance family, not method names.
  cert_var <- NULL
  first_draw_spec <- design$stages[[first_stage_idx]]$draw_spec
  cert_col <- paste0(".certainty_", first_stage_idx)
  if (
    identical(survey_stage_kind(first_draw_spec), "pps_wor") &&
      cert_col %in% names(df) &&
      any(df[[cert_col]])
  ) {
    cert_var <- paste0(".", prefix, "cert_stratum")
    df[[cert_var]] <- ifelse(
      df[[cert_col]],
      "certainty",
      "probability"
    )
  }

  term_stages <- if (mode == "first_stage" || length(id_stage_indices) == 0) {
    first_stage_idx
  } else {
    id_stage_indices
  }

  terms <- rep(NA_character_, length(term_stages))
  for (i in seq_along(term_stages)) {
    stage_idx <- term_stages[i]
    stage_spec <- design$stages[[stage_idx]]
    vars <- if (!is_null(stage_spec$strata)) {
      stage_spec$strata$vars
    } else {
      character(0)
    }
    if (identical(stage_idx, first_stage_idx)) {
      vars <- c(vars, cert_var)
    }
    if (length(vars) == 0) {
      next
    }
    if (length(vars) == 1) {
      terms[i] <- vars
    } else {
      combined <- paste0(".", prefix, "strata_", stage_idx)
      df[[combined]] <- group_ids(df, vars)
      terms[i] <- combined
    }
  }

  # Trailing unstratified stages are dropped from the formula. Interior
  # gaps get a single-stratum placeholder to keep terms aligned with
  # the ids formula.
  last_stratified <- max(c(0L, which(!is.na(terms))))
  terms <- terms[seq_len(last_stratified)]
  for (i in seq_along(terms)) {
    if (is.na(terms[i])) {
      placeholder <- paste0(".", prefix, "strata_all_", term_stages[i])
      df[[placeholder]] <- "all"
      terms[i] <- placeholder
    }
  }

  strata_formula <- if (length(terms) == 0) {
    NULL
  } else {
    survey_formula_from_vars(terms)
  }

  list(
    df = df,
    formula = strata_formula,
    vars = terms
  )
}

#' Per-stage FPC terms.
#'
#' Aligns one term per ID stage. Uses counts by default, a common fraction
#' scale in multi-stage PPS designs, and the legacy probability scale for one
#' represented PPS stage.
#' @noRd
survey_fpc_info <- function(df, design, stages_executed, id_stage_indices) {
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
      # "unsupported" gets the same FPC encoding as rs_poisson (pi at
      # stage 1, no correction later): survey_resolve_pps() errors on
      # it before the FPC is used, or demotes it for the bootstrap.
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

  fpc_vars <- character(0)
  for (i in seq_along(fpc_stage_indices)) {
    stage_idx <- fpc_stage_indices[i]
    kind <- stage_kind[i]
    weight_col <- paste0(".weight_", stage_idx)
    fpc_col <- paste0(".fpc_", stage_idx)

    if (kind %in% c("wr", "rs_poisson_later", "unsupported_later")) {
      if (scale == "fraction") {
        f0_col <- paste0(".fpc_f0_", stage_idx)
        df[[f0_col]] <- 0
        fpc_vars <- c(fpc_vars, f0_col)
      } else {
        inf_col <- paste0(".fpc_inf_", stage_idx)
        df[[inf_col]] <- Inf
        fpc_vars <- c(fpc_vars, inf_col)
      }
      next
    }

    if (kind %in% c("pps_wor", "rs_poisson_first", "unsupported_first")) {
      fpc_pi_col <- paste0(".fpc_pi_", stage_idx)
      df[[fpc_pi_col]] <- 1 / df[[weight_col]]
      fpc_vars <- c(fpc_vars, fpc_pi_col)
      next
    }

    # Equal-probability WOR.
    if (scale == "fraction") {
      f_col <- paste0(".fpc_f_", stage_idx)
      df[[f_col]] <- 1 / df[[weight_col]]
      fpc_vars <- c(fpc_vars, f_col)
    } else if (fpc_col %in% names(df)) {
      fpc_vars <- c(fpc_vars, fpc_col)
    } else {
      # No population count available for this stage. Fall back to no
      # correction rather than dropping the term, which would misalign
      # the remaining fpc terms with the ids formula.
      inf_col <- paste0(".fpc_inf_", stage_idx)
      df[[inf_col]] <- Inf
      fpc_vars <- c(fpc_vars, inf_col)
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
    scale = scale,
    has_pps_wor = has_pps_wor,
    has_rs_poisson_stage1 = has_rs_poisson_stage1
  )
}

#' Demote a stage-1 random-size Poisson FPC to Inf.
#'
#' Used by the bootstrap escape hatch when survey::svydesign() cannot
#' represent a stage-1 Poisson PPS specification in a multi-stage object.
#' The demoted design carries no finite-population correction at that stage.
#' bootstrap resampler supplies the variance instead.
#' @noRd
survey_demote_rs_poisson_stage1 <- function(df, fpc, first_idx) {
  pi_col <- paste0(".fpc_pi_", first_idx)
  # On the fraction scale, "no correction" is a sampling fraction of 0.
  # on the count/pi scales it is an infinite population.
  if (identical(fpc$scale, "fraction")) {
    inf_col <- paste0(".fpc_f0_", first_idx)
    df[[inf_col]] <- 0
  } else {
    inf_col <- paste0(".fpc_inf_", first_idx)
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

#' Resolve the pps argument for as_svydesign.
#'
#' Single-phase only. Encapsulates the case split documented in
#' as_svydesign(): exact poisson_sampling() at single-stage, error or
#' bootstrap relaxation for multi-stage stage-1 Poisson, error or
#' bootstrap relaxation for clustered single-stage Poisson with multiple
#' rows per cluster, Brewer for fixed-size PPS WOR, FALSE otherwise.
#'
#' Returns a list with the resolved `pps` argument plus possibly modified
#' `df` and `fpc` (when the bootstrap relaxation rewrites a stage-1
#' Poisson FPC to Inf).
#' @noRd
survey_resolve_pps <- function(
  df,
  design,
  stages_executed,
  fpc,
  user_pps = NULL,
  relax_pps_for_bootstrap = FALSE
) {
  if (!is_null(user_pps)) {
    return(list(pps = user_pps, df = df, fpc = fpc))
  }

  # Stages whose method declares variance_family = "unsupported": no
  # linearization treatment is valid, whatever the selection metadata
  # looks like. as_svydesign() refuses. The bootstrap escape demotes a
  # stage-1 pi FPC (later-stage unsupported FPCs are already Inf/0).
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
    if (relax_pps_for_bootstrap) {
      relaxed <- survey_demote_rs_poisson_stage1(df, fpc, first_idx)
      pps <- if (relaxed$fpc$has_pps_wor) "brewer" else FALSE
      return(list(pps = pps, df = relaxed$df, fpc = relaxed$fpc))
    }
    abort_samplyr(
      c(
        "{.pkg survey} does not support multi-stage designs with a random-size Poisson method at stage 1.",
        "i" = "{.pkg survey} rejects multi-stage designs when the {.code pps} argument is set.",
        "i" = "Use {.code as_svrepdesign(type = \"subbootstrap\")} for a bootstrap approximation.",
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
      if (relax_pps_for_bootstrap) {
        relaxed <- survey_demote_rs_poisson_stage1(df, fpc, first_idx)
        return(list(pps = FALSE, df = relaxed$df, fpc = relaxed$fpc))
      }
      abort_samplyr(
        c(
          "Cannot export a clustered random-size Poisson design with multiple rows per sampled cluster via {.fn as_svydesign}.",
          "i" = "{.pkg survey}'s {.fn poisson_sampling} estimator treats rows as independent and does not honor within-cluster correlation.",
          "i" = "Use {.code as_svrepdesign(type = \"subbootstrap\")} for a bootstrap approximation that resamples clusters."
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
    if (relax_pps_for_bootstrap) {
      relaxed <- survey_demote_rs_poisson_stage1(df, fpc, first_idx)
      return(list(pps = FALSE, df = relaxed$df, fpc = relaxed$fpc))
    }
    abort_samplyr(
      c(
        "Cannot export the custom random-size method {.val {stage_spec$draw_spec$method}} via {.fn as_svydesign}.",
        "i" = "The method is registered with {.code fixed_size = FALSE}, so the sample size is random. {.pkg survey}'s Poisson variance estimator assumes selections are independent across units, which samplyr cannot verify for a custom method.",
        "i" = "If selections are independent (Poisson-type), pass the inclusion probabilities explicitly: {.code as_svydesign(x, pps = survey::poisson_sampling(1 / x$.weight))}.",
        "i" = "Otherwise use {.code as_svrepdesign(type = \"subbootstrap\")} for a bootstrap approximation."
      ),
      class = "samplyr_error_custom_random_wor_export"
    )
  }

  pi_vec <- df[[paste0(".fpc_pi_", first_idx)]]
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
  # survey::twophase(), which has no dots of its own. Everything else it
  # declares is derived, so this is the whole forwardable surface.
  "method", "pps"
)

#' @noRd
svrepdesign_accepted_args <- c(
  # survey::as.svrepdesign() and its default method
  "type", "fay.rho", "fpc", "fpctype", "compress", "mse",
  # the replicate-weight generators its dots reach: brrweights(),
  # jknweights(), bootweights(), subbootweights(), mrbweights()
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
  systematic_variance <- match.arg(systematic_variance)
  check_single_replicate(x, "as_svydesign")
  check_sample_unmodified(x, "as_svydesign")
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a survey design object."
  )

  phase_info <- survey_validate_phase_support(
    x,
    allow_twophase = TRUE,
    fn_name = "as_svydesign"
  )
  check_wave_carries_master(x, phase_info, "as_svydesign")
  prev_phase <- phase_info$prev_phase
  is_twophase <- phase_info$is_twophase
  is_activation <- survey_is_activation(phase_info)

  # The two branches call different survey functions, so what `...` may
  # legitimately carry differs: `subset` belongs to twophase() alone.
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

  # nest reaches survey::svydesign() only, so on a two-phase export it is
  # accepted and has no effect. Say so rather than appear to honour it.
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
      match.arg(method, c("full", "approx", "simple"))
    }
  } else if (!is_null(method)) {
    cli_abort(
      "{.arg method} is only valid when converting a two-phase sample."
    )
  }

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)

  df <- as.data.frame(x)

  # Every design contributing to the export is examined, not only this
  # sample's: a two-phase or activation export can carry a systematic first
  # phase whose approximation the caller never sees named.
  systematic_stages <- systematic_approximated_stages(
    design, stages_executed, df,
    phase = if (is_twophase) 2L else NULL
  )
  if (is_twophase) {
    previous <- prev_phase$sample
    systematic_stages <- c(
      systematic_approximated_stages(
        prev_phase$design %||% get_design(previous),
        prev_phase$stages %||% get_stages_executed(previous),
        as.data.frame(previous),
        phase = 1L
      ),
      systematic_stages
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
    phase1 <- prev_phase$sample
    # The phase-2 sample itself is clean (checked above), but the
    # phase-1 sample it was drawn from may have been filtered or
    # otherwise modified before phase-2 execution. twophase() then
    # treats the modified rows as the complete phase-1 sample.
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
      ))
    }
    design1 <- prev_phase$design %||% get_design(phase1)
    stages1 <- prev_phase$stages %||% get_stages_executed(phase1)
    df1 <- as.data.frame(phase1)
    df2 <- df
    design2 <- design

    bridge_vars <- resolve_phase_bridge(
      survey_key_vars(design1, stages1, df1),
      survey_key_vars(design2, stages_executed, df2),
      df1,
      df2
    )

    # Between-phase subsampling variance is handled by
    # survey::twophase() itself, so unclustered element stages are not
    # synthesized into the per-phase ids formulas.
    id_info1 <- survey_id_info(
      design1,
      stages1,
      df1,
      synthesize_unclustered = TRUE,
      prefix = "p1_"
    )
    df1 <- id_info1$df
    id_info2 <- survey_id_info(
      design2,
      stages_executed,
      df2,
      synthesize_unclustered = FALSE,
      prefix = "p2_"
    )
    df2 <- id_info2$df
    id_vars2 <- id_info2$id_vars

    ids_formula1 <- survey_ids_formula(id_info1$id_vars)
    ids_formula2 <- survey_ids_formula(id_vars2)

    strata1 <- survey_strata_info(
      df1,
      design1,
      stages1,
      mode = "first_stage",
      prefix = "p1_"
    )
    df1 <- strata1$df
    strata2 <- survey_strata_info(
      df2,
      design2,
      stages_executed,
      mode = "first_stage",
      prefix = "p2_"
    )
    df2 <- strata2$df

    fpc1 <- survey_fpc_info(df1, design1, stages1, id_info1$stage_indices)
    df1 <- fpc1$df
    fpc2 <- survey_fpc_info(
      df2,
      design2,
      stages_executed,
      id_info2$stage_indices
    )
    df2 <- fpc2$df

    if (fpc1$has_pps_wor) {
      cli_abort(c(
        "Two-phase export does not support PPS at phase 1.",
        "i" = "{.fn survey::twophase} requires the phase 1 PPS specification to be {.code NULL}.",
        "i" = "Export each phase separately with {.fn as_svydesign} instead."
      ))
    }

    strata2_extra <- setdiff(strata2$vars, names(df1))
    id_vars2_extra <- setdiff(id_vars2, names(df1))

    fpc2_vars <- fpc2$fpc_vars
    fpc2_vars_renamed <- if (length(fpc2_vars) > 0) {
      sub("^\\.fpc_", ".fpc_phase2_", fpc2_vars)
    } else {
      character(0)
    }
    fpc2_rename_map <- setNames(fpc2_vars_renamed, fpc2_vars)

    phase2_cols_needed <- unique(
      c(
        bridge_vars,
        id_vars2_extra,
        strata2_extra,
        fpc2_vars,
        setdiff(names(df2), names(df1)),
        ".weight"
      )
    )
    phase2_cols_needed <- intersect(phase2_cols_needed, names(df2))

    df2_join <- df2[, phase2_cols_needed, drop = FALSE]
    if (".weight" %in% names(df2_join)) {
      names(df2_join)[names(df2_join) == ".weight"] <- ".weight_phase2"
    }
    if (length(fpc2_rename_map) > 0) {
      idx <- match(names(fpc2_rename_map), names(df2_join))
      names(df2_join)[idx] <- fpc2_rename_map
    }

    df_combined <- df1 |>
      left_join(
        df2_join,
        by = bridge_vars,
        # Backstops for what resolve_phase_bridge() already established: a
        # missing key never links, and no phase-2 row may reach two phase-1
        # rows.
        na_matches = "never",
        relationship = "one-to-many"
      )

    df_combined$.phase2 <- !is.na(df_combined$.weight_phase2)
    if (!any(df_combined$.phase2)) {
      cli_abort(
        c(
          "Phase 2 rows could not be matched to phase 1 identifiers.",
          "i" = "Ensure a shared unique identifier is present in both phases."
        )
      )
    }
    df_combined$.weight_phase2_cond <- ifelse(
      df_combined$.phase2,
      df_combined$.weight_phase2 / df_combined$.weight,
      NA_real_
    )
    if (any(!is.finite(df_combined$.weight_phase2_cond[df_combined$.phase2]))) {
      cli_abort(
        "Invalid phase 2 conditional weights detected after matching phases."
      )
    }
    df_combined$.prob_1 <- 1 / df_combined$.weight
    df_combined$.prob_2 <- ifelse(
      df_combined$.phase2,
      1 / df_combined$.weight_phase2_cond,
      NA_real_
    )

    dots <- list(...)
    # [[ not $: `$` on a list matches by prefix, so a forwarded argument
    # whose name merely starts with "pps" would be read as `pps` here.
    pps_arg <- dots[["pps"]]
    dots[["pps"]] <- NULL

    fpc2_formula <- if (length(fpc2_vars_renamed) == 0) {
      NULL
    } else {
      survey_formula_from_vars(fpc2_vars_renamed)
    }

    use_weights <- !is_null(method) && method %in% c("approx", "simple")

    # The full method needs one probability term per ID stage unless the FPCs
    # supply the same information.
    n_id_stages <- max(
      length(id_info1$id_vars), length(id_vars2)
    )
    fpc_covers_stages <- fpc_states_all_probabilities(
      df_combined,
      fpc1$formula,
      fpc1$fpc_vars,
      id_info1$stage_indices,
      stages1
    ) &&
      fpc_states_all_probabilities(
        # Phase-2 columns are absent on the rows phase 2 did not reach, which
        # is what `subset` already says. Only the phase-2 rows are read.
        df_combined[df_combined$.phase2, , drop = FALSE],
        fpc2_formula,
        fpc2_vars_renamed,
        id_info2$stage_indices,
        stages_executed
      )

    check_twophase_stage_probs(n_id_stages, use_weights, fpc_covers_stages)

    probs_arg <- if (use_weights || fpc_covers_stages) {
      NULL
    } else {
      list(
        stats::as.formula("~.prob_1"),
        stats::as.formula("~.prob_2")
      )
    }
    weights_arg <- if (use_weights) {
      list(
        stats::as.formula("~.weight"),
        stats::as.formula("~.weight_phase2_cond")
      )
    } else {
      NULL
    }

    result <- do.call(
      survey::twophase,
      c(
        list(
          id = list(ids_formula1, ids_formula2),
          strata = list(strata1$formula, strata2$formula),
          probs = probs_arg,
          weights = weights_arg,
          fpc = list(fpc1$formula, fpc2_formula),
          subset = stats::as.formula("~.phase2"),
          data = df_combined,
          method = method,
          pps = pps_arg
        ),
        dots
      )
    )

    record_systematic(result)
  } else {
    record_systematic(build_singlephase_svydesign(
      x,
      dots = list(...),
      nest = nest,
      relax_pps_for_bootstrap = FALSE
    ))
  }
}

#' Build a single-phase survey.design from a tbl_sample.
#'
#' Shared by [as_svydesign.tbl_sample()] and [as_svrepdesign.tbl_sample()].
#' When `relax_pps_for_bootstrap = TRUE`, multi-stage stage-1 random-size
#' Poisson designs and clustered single-stage random-size Poisson designs
#' with multi-row clusters are exported with a permissive specification
#' (Inf at the Poisson stage, no `pps` argument), so that the bootstrap
#' resampler can produce a variance estimate. The relaxed design is not a
#' valid linearization design.
#' @noRd
build_singlephase_svydesign <- function(
  x,
  dots,
  nest,
  relax_pps_for_bootstrap = FALSE
) {
  design <- get_design(x)
  stages_executed <- get_stages_executed(x)
  df <- as.data.frame(x)

  # User-supplied PPS objects are single-stage in survey.
  # [[ not $: `$` on a list matches by prefix.
  if (!is_null(dots[["pps"]]) && length(stages_executed) > 1L) {
    cli_warn(c(
      "Exact PPS variance ({.arg pps}) is single-stage in {.pkg survey}.",
      "i" = "Exporting the stage-1 design only; later-stage sampling
             variance is not represented.",
      "i" = "Omit {.arg pps} for multi-stage linearization with
             Brewer's approximation at the PPS stage."
    ))
    stages_executed <- stages_executed[1]
  }

  id_info <- survey_id_info(design, stages_executed, df)
  df <- id_info$df
  ids_formula <- survey_ids_formula(id_info$id_vars)

  strata <- survey_strata_info(
    df,
    design,
    stages_executed,
    id_stage_indices = id_info$stage_indices
  )
  df <- strata$df

  fpc <- survey_fpc_info(df, design, stages_executed, id_info$stage_indices)
  df <- fpc$df

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

  dots[["pps"]] <- NULL

  result <- do.call(
    survey::svydesign,
    c(
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
  )

  # Replace the stored call to avoid inlining the entire data frame,
  # which causes massive output when printing the survey.design object
  result$call <- call(
    "svydesign",
    ids = ids_formula,
    strata = strata$formula,
    weights = stats::as.formula("~.weight"),
    fpc = fpc$formula,
    data = quote(data),
    nest = nest
  )

  result
}

#' Convert a tbl_sample to a replicate-weight survey design
#'
#' Creates a `svyrep.design` object from a `tbl_sample` by first
#' converting to a [survey::svydesign()] object via [as_svydesign()],
#' then converting with [survey::as.svrepdesign()].
#'
#' @inheritParams as_svydesign
#' @param type Replicate method passed to [survey::as.svrepdesign()].
#'   One of `"auto"`, `"JK1"`, `"JKn"`, `"BRR"`, `"bootstrap"`,
#'   `"subbootstrap"`, `"mrbbootstrap"`, or `"Fay"`.
#' @param ... Additional arguments passed to [survey::as.svrepdesign()] and
#'   on to the replicate-weight generator it selects, such as `replicates`,
#'   `fay.rho`, `fpctype`, or `mse`. Every argument must be named, and its
#'   name must be one those functions accept: `type` follows the `...` and so
#'   is matched exactly, and a near miss such as `typ` is reported rather
#'   than forwarded. `design` cannot be given here: it is the
#'   [survey::svydesign()] object this verb builds from the sample.
#' @param systematic_variance What to do about the generic replicate weights
#'   built for equal-probability `systematic` stages. `"warn"` (default) builds
#'   them and warns once per call, naming every affected stage.
#'   `"approximate"` builds them silently, for a caller who has acknowledged
#'   the approximation, while `"error"` refuses. Naming a `type` is not an
#'   acknowledgement, since no type reproduces systematic selection. Census
#'   stages are exempt and `pps_systematic` is unaffected, as in
#'   [as_svydesign()]. The choice and the affected stages are recorded on the
#'   returned object in the `"samplyr_systematic_variance"` attribute.
#'
#' @return A `svyrep.design` object from the survey package.
#'
#' @details
#' Replicate conversion supports single-phase designs. For unequal-probability
#' designs (PPS or random-size Poisson), `"subbootstrap"` and `"mrbbootstrap"`
#' are the supported replicate types. Other types emit a warning and may fail
#' because inclusion probabilities vary within strata. For fixed-size PPS
#' variance estimation, linearization via [as_svydesign()] is generally
#' preferred. Two-phase designs should be exported with [as_svydesign()].
#'
#' ## Bootstrap escape hatch for random-size Poisson at stage 1
#'
#' Some designs cannot be expressed as a linearization-based
#' [survey::svydesign()] object. Specifically, multi-stage designs with
#' a random-size Poisson method (`bernoulli` or `pps_poisson`) at stage 1,
#' and single-stage designs with `cluster_by()` and multiple rows per
#' sampled cluster, are rejected by [as_svydesign()] for those methods.
#'
#' For these cases `as_svrepdesign(type = "subbootstrap")` (or
#' `"mrbbootstrap"`) is the recommended path. The design is exported with
#' a permissive specification (no finite-population correction at the
#' Poisson stage, no `pps` argument), and the bootstrap resampler supplies
#' the variance through replicate weights.
#'
#' This is the package's bootstrap approximation for designs that exact
#' Horvitz-Thompson linearization cannot express in
#' [survey::svydesign()]. The subbootstrap and mrbbootstrap methods were
#' developed for fixed-size PPS sampling (Antal and Tille 2011). Their
#' behavior on random-size Poisson designs, especially at multiple
#' stages, has weaker theoretical backing and should be treated as an
#' approximation. In particular, the resampling is fixed-size, so it
#' does not capture the variance contribution of the random sample
#' size and can materially understate the total variance of a
#' Poisson-type design. When the exact Poisson linearization is
#' available (single-stage designs), prefer [as_svydesign()].
#'
#' Bounded cube, LPM2, and SCPS designs likewise have no native,
#' design-specific replicate variance estimator in `samplyr`.
#' `as_svrepdesign(type = "subbootstrap")` and `"mrbbootstrap"` export a
#' generic PPS bootstrap approximation for them. They do not reproduce the
#' original cube constraints or spatial selection algorithm within each
#' replicate. Treat the resulting variance estimates as approximations, not
#' as exact variance estimators for those designs.
#'
#' ## Equal-probability systematic sampling
#'
#' Equal-probability `systematic` stages are in the same position, for every
#' replicate type rather than for a subset of them. A jackknife or bootstrap
#' replicate perturbs the realized sample. It does not redraw a random start
#' against the frame in the order the frame was in, which is what generates a
#' systematic sample's variance. Frame ordering or periodicity can therefore
#' make the resulting standard errors too small or too large, in the same
#' direction that ordering moves the true variance. See `systematic_variance`,
#' and [as_svydesign()] for how large the analogous gap was measured to be
#' under linearization.
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
    "Fay"
  ),
  systematic_variance = c("warn", "approximate", "error")
) {
  systematic_variance <- match.arg(systematic_variance)
  check_single_replicate(x, "as_svrepdesign")
  check_sample_unmodified(x, "as_svrepdesign")
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a replicate-weight survey design."
  )

  check_forwarded_args(
    enquos(...),
    owned = c("type", "systematic_variance"),
    accepted = svrepdesign_accepted_args,
    derived = svrepdesign_derived_args,
    forwarded_to = "survey::as.svrepdesign"
  )

  survey_validate_phase_support(
    x,
    allow_twophase = FALSE,
    fn_name = "as_svrepdesign"
  )

  type <- match.arg(type)

  design <- get_design(x)
  unequal_used <- unique(unlist(lapply(
    get_stages_executed(x),
    function(stage_idx) {
      draw_spec <- design$stages[[stage_idx]]$draw_spec
      kind <- survey_stage_kind(draw_spec)
      unequal <- kind %in%
        c("pps_wor", "rs_poisson", "unsupported") ||
        (kind == "wr" && !is_null(draw_spec$mos))
      if (unequal) draw_spec$method else NULL
    }
  )))
  pps_safe_types <- c("subbootstrap", "mrbbootstrap")
  if (length(unequal_used) > 0 && !type %in% pps_safe_types) {
    cli_warn(c(
      "{.fn as_svrepdesign} with {.val {type}} may not work for unequal-probability designs.",
      "i" = "Found method{?s}: {.val {unequal_used}}.",
      "i" = "Use {.val subbootstrap} or {.val mrbbootstrap} for unequal-probability designs,
             or use {.fn as_svydesign} for linearization-based variance."
    ))
  }

  # No replicate type reproduces a systematic stage, so asking for one is not
  # itself an acknowledgement. Checked before the conversion, so `"error"`
  # refuses rather than building weights it then throws away.
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
    relax_pps_for_bootstrap = type %in% pps_safe_types
  )

  result <- tryCatch(
    survey::as.svrepdesign(design = svydesign_obj, type = type, ...),
    error = function(e) {
      abort_samplyr(
        c(
          "{.fn as_svrepdesign} failed to convert this design to replicate weights.",
          "x" = "{conditionMessage(e)}"
        ),
        class = "samplyr_error_svrep_conversion_failed"
      )
    }
  )

  record_systematic_variance(
    result, systematic_stages, systematic_variance, "generic_replicates"
  )
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
