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
#'   the sampling unit identifier instead. The survey package treats each occurrence as
#'   independent for Hansen-Hurwitz variance estimation. This is exact for WR
#'   and the documented approximation for PMR.
#' - **Strata** (`strata`): one term per stage, aligned with `ids`.
#'   A stage stratified by several variables exports their
#'   cross-classification as a single synthesized interaction column
#'   (survey silently ignores extra variables within a stage's term).
#'   Trailing unstratified stages are omitted and unstratified stages
#'   before a stratified stage get a constant placeholder column.
#' - **Weights** (`weights`): the `.weight` column, equal to the product of
#'   per-stage weights \eqn{w = \prod w_k = \prod 1/q_k}{w = prod(1/q_k)}.
#'   For WOR, \eqn{q_k = \pi_k}{q_k = pi_k}. For WR or PMR,
#'   \eqn{q_k = E(K_k)}{q_k = E(K_k)}. Thus an all-WOR exact design uses the
#'   inverse overall inclusion probability, while WR and PMR rows use the
#'   occurrence-level Hansen-Hurwitz form.
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
#' to the wave for analysis are carried into the exported design. Current
#' wave or phase-2 analysis columns replace same-named first-phase columns.
#' Unsampled rows have missing current measurements in those columns. Columns
#' absent from the wave remain available from the master, so dropping a wave
#' column does not erase the corresponding master measurements. Design identifiers,
#' strata and internal sampling columns retain their recorded meanings.
#' Keep earlier measurements under separate names if both are needed.
#'
#' The two-phase variance of a *total* can come out negative on a stratified
#' master whose stratum means differ strongly relative to the variation within
#' them, in which case [survey::svytotal()] returns `NaN` with base R's
#' `sqrt(v): NaNs produced`. That is [survey::twophase()]'s exact estimator
#' rather than anything samplyr adds, and samplyr cannot intercept it: the
#' variance is not computed until an estimator is called on the returned
#' object, and samplyr is not in that call. `method = "approx"` gives a finite
#' standard error for the same design. A mean is unaffected, and so is a
#' clustered master.
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
#' the variance estimator, not the selection algorithm e.g. Sampford selection
#' receives this default treatment. This is the approximation
#' described by Berger (2004). Its accuracy depends on the sampling design and
#' population. It is not an exact substitute for joint inclusion probabilities.
#'
#' For supported methods, you can instead compute joint inclusion
#' probabilities using [joint_expectation()] and pass them via `pps =
#' survey::ppsmat(joint_matrix)`. The matrix is exact for CPS, Sampford,
#' systematic PPS, and Poisson selection. Generalized Brewer, SPS, Pareto, and
#' unconstrained cube use the documented high-entropy approximation.
#' Systematic PPS matrices can contain zero pair probabilities. A zero pair
#' probability rules out a design-unbiased variance estimator.
#' A sampled matrix contains only pairs observed together and cannot establish
#' positivity for all population pairs. Supplying a `survey::ppsmat()` object
#' for systematic PPS therefore warns. The route remains available because
#' full pair positivity can hold, especially at high sampling fractions.
#'
#' An accurate variance estimate does not by itself give accurate Wald
#' interval coverage, because finite-sample PPS total estimators can be
#' skewed. Validate coverage for the population and sample sizes an
#' operational design will use. `vignette("survey-analysis")` shows a
#' log-scale interval for strictly positive domain totals.
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
#' \eqn{\hat V = \sum_{i \in S} (1 - \pi_i) / \pi_i^2 \cdot y_i^2}{Vhat = sum_{i in S} (1 - pi_i) / pi_i^2 * y_i^2}
#' described in Sarndal, Swensson and Wretman (1992), section 2.8.
#'
#' This applies under the following conditions.
#'
#' - Single-stage designs (no `cluster_by()`, or `cluster_by()` with
#'   one row per sampled cluster) are exported with `poisson_sampling()`
#'   and produce the exact Horvitz-Thompson Poisson variance.
#' - Multi-stage designs with a Poisson method at any stage are refused by
#'   linearization export. Treating Poisson sampling as fixed-size sampling
#'   with replacement can understate variance, even to zero. Use
#'   `as_svrepdesign(type = "rwyb")`, which requires the optional svrep package.
#' - Single-stage designs that use `cluster_by()` with multiple rows per
#'   sampled cluster are also refused by linearization export. Use
#'   `as_svrepdesign(type = "rwyb")` to replicate the sampled clusters.
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
#'   or declare `variance_family = "poisson"` and use `type = "rwyb"`.
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
#' Each unit receives exactly \eqn{\lfloor E(n_i) \rfloor}{floor(E(n_i))} or
#' \eqn{\lfloor E(n_i) \rfloor + 1}{floor(E(n_i)) + 1} hits, where
#' \eqn{E(n_i) = n \cdot \textrm{mos}_i / \sum \textrm{mos}}{E(n_i) = n * mos_i / sum(mos)}.
#' When all expected hit counts are below 1, this reduces to WOR,
#' otherwise large units receive multiple hits.
#'
#' For variance estimation, Chromy (2009) recommends the
#' Hansen-Hurwitz (with-replacement) approximation rather than
#' exact pairwise expectations, which he found "quite variable."
#' Accordingly,
#' `as_svydesign()` treats `pps_chromy` stages like
#' with-replacement stages (no FPC, no pps argument).
#' Chauvet (2019) studied the related randomized without-replacement design.
#'
#' Note that `survey::ppsmat()` is **not** valid for the general
#' PMR case. The survey package reads \eqn{\pi_i} from the diagonal
#' of the joint matrix, but for PMR the diagonal contains
#' \eqn{E(n_i^2)}, which differs from \eqn{E(n_i)} when units
#' receive multiple hits. The generalized Sen-Yates-Grundy variance
#' requires \eqn{E(n_i) E(n_j) - E(n_i n_j)} as the pairwise
#' weight (Chromy 2009, eq. 5), not \eqn{E(n_i^2) E(n_j^2) - E(n_i n_j)}.
#' A direct generalized estimator using Monte Carlo expected hits can be
#' negative and unstable, so samplyr does not expose it.
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
#' ## A shared estimation weight
#'
#' A sample from [share_weights()] carries weights for a population other
#' than the one that was selected, so it is exported as its **source-target
#' contributions**: one row per link, weighted by the recorded coefficient
#' times the source unit's design weight. The generalized weight share total
#' is the Horvitz-Thompson total of a variable derived on the source units,
#' and expanding the contributions is what lets `survey` form that variable
#' inside each sampling unit for whatever is being analyzed. It is exact for
#' any link structure, with no condition on how many source units reach a
#' target, provided every selected source unit has at least one contribution.
#' A selected source unit with no link has derived value zero but must still
#' remain in the variance calculation. A contribution-row design cannot retain
#' that sampling unit without inventing a target row, so this route refuses
#' the case and names [as_svrepdesign()] as the supported alternative.
#'
#' The rows of the result are contributions rather than target units, so
#' there are more of them than the transformation returned. No estimate is
#' affected: a total sums the same terms, and a mean's denominator is the
#' estimated size of the target population either way.
#'
#' An unequal-probability or random-size source design is refused on this
#' route. Both take their variance from a structure indexed by the rows of
#' the source sample, and those rows are no longer the sampled units once
#' each appears per contribution. Use `as_svrepdesign()` there, which
#' replicates the source design and applies the sharing inside every
#' replicate.
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
    abort_samplyr(
      c(
        "{.fn {fn_name}} does not support two-phase samples.",
        advice %||%
          c("i" = "Use {.fn as_svydesign} for two-phase linearization
                   export.")
      ),
      class = class,
      call = call
    )
  }

  phase_info
}

## The systematic variance approximation

# One systematic sample does not identify its design variance. The SRSWOR
# approximation can fail under frame periodicity. Generic replicates also miss
# the original order and random start. Both export routes warn once.

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
    # A census has no variance to approximate.
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
  # A lone element stage contributes one probability term.
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

  df1 <- as.data.frame(master)

  # Copy current measurements before generating export columns so their
  # names participate in collision avoidance (for example a user's .active).
  carried <- setdiff(names(x), protected_sample_cols(df1, design1, stages1))
  if (length(carried) > 0) {
    at <- match(df1$.sample_id, x$.sample_id)
    for (nm in carried) {
      df1[[nm]] <- as.data.frame(x)[[nm]][at]
    }
  }

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
    # A single unclustered stage uses element sampling.
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

  # Fill interior stratum gaps to align with ID terms.
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

  fpc_vars <- character(0)
  for (i in seq_along(fpc_stage_indices)) {
    stage_idx <- fpc_stage_indices[i]
    kind <- stage_kind[i]
    weight_col <- paste0(".weight_", stage_idx)
    fpc_col <- paste0(".fpc_", stage_idx)

    if (kind %in% c("wr", "unsupported_later")) {
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
      # Keep an infinite FPC term when its population count is absent.
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

#' Demote an unsupported stage-1 variance specification to a WR approximation
#'
#' Used only by the existing generic bootstrap route for unsupported
#' balanced or spatial variance families. Poisson designs use RWYB directly.
#' @noRd
survey_demote_rs_poisson_stage1 <- function(df, fpc, first_idx) {
  pi_col <- paste0(".fpc_pi_", first_idx)
  # No correction is zero fraction or infinite population.
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
  if (!is_null(user_pps)) {
    # A user pps object describes stage 1 only, so a later systematic stage
    # must not trigger this warning even if a caller ever passes untruncated
    # stages alongside a user pps.
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
  systematic_variance <- match.arg(systematic_variance)
  check_single_replicate(x, "as_svydesign")
  check_sample_unmodified(x, "as_svydesign")
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a survey design object."
  )

  # Export shared weights through source-target contributions.
  if (identical(sample_weight_contract(x), "shared")) {
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
  check_wave_carries_master(x, phase_info, "as_svydesign")
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

  # Inspect every contributing phase for systematic approximation.
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

    # Let `survey::twophase()` handle between-phase variance.
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

    if (fpc1$has_rs_poisson_stage1 || fpc2$has_rs_poisson_stage1) {
      abort_samplyr(c(
        "Two-phase export does not support Poisson sampling in either phase.",
        "i" = "The current two-phase bridge cannot represent the random sample-size variance."
      ), class = "samplyr_error_twophase_poisson")
    }

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
        # Backstop the validated phase bridge.
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
    # Use exact list lookup to avoid partial argument matching.
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
        # Read phase-2 columns only on reached rows.
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
#' Shared by [as_svydesign.tbl_sample()] and generic replicate exports.
#' `relax_pps_for_bootstrap` only permits the existing generic approximation
#' for unsupported balanced or spatial variance families. RWYB bypasses this
#' function and preserves the original stage mechanisms.
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

  # User PPS objects are single-stage. Use exact list lookup.
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

  # Keep the full data frame out of the stored call.
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
  estimator <- match.arg(estimator)
  systematic_variance <- match.arg(systematic_variance)
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
#'   `"subbootstrap"`, `"mrbbootstrap"`, `"Fay"`, or `"rwyb"`. The last
#'   uses svrep rather than [survey::as.svrepdesign()].
#'
#'   The jackknife, BRR and Fay types are deterministic: one sample gives one
#'   set of replicate weights. The bootstrap types resample, so they draw from
#'   the session's random stream and two calls on one sample give two
#'   different standard errors. Set a seed beforehand to make a result
#'   reproducible, as with any resampling in R.
#'
#'   The spread is not small at survey's default of 50 replicates. On a
#'   90-of-600 stratified sample, twelve `"bootstrap"` calls on one sample
#'   ranged over 37% of their mean, falling to 12% at `replicates = 200` and
#'   4% at `replicates = 4000`. A reported bootstrap standard error carries
#'   that simulation noise on top of the sampling variance it is estimating,
#'   so raise `replicates` through `...` when the second decimal is going to
#'   be read.
#' @param ... Additional arguments passed to [survey::as.svrepdesign()] and
#'   on to the replicate-weight generator it selects, such as `replicates`,
#'   `fay.rho`, `fpctype`, or `mse`. Every argument must be named, and its
#'   name must be one those functions accept: `type` follows the `...` and so
#'   is matched exactly, and a near miss such as `typ` is reported rather
#'   than forwarded. `design` cannot be given here: it is the
#'   [survey::svydesign()] object this verb builds from the sample. For
#'   `type = "rwyb"`, only `replicates` (default 500, integer at least 2),
#'   `mse` (default TRUE) and `compress` (default TRUE) are accepted.
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
#' Replicate conversion supports single-phase designs, including multistage
#' samples, shared weights and independent frame stacks. `"auto"` retains
#' survey's method choice. It does not depend on whether svrep is installed.
#' Two-phase replicate export remains unsupported.
#'
#' ## Rao-Wu-Yue-Beaumont bootstrap
#'
#' `as_svrepdesign(x, type = "rwyb")` supports SRS without replacement,
#' independent draws with replacement (`srswr`, `pps_multinomial`), independent
#' Poisson selection (`bernoulli`, `pps_poisson`), and combinations of these
#' across stages. It also supports fixed-size PPS WOR (`pps_brewer`, `pps_cps`,
#' `pps_sampford`, `pps_systematic`) using approximate joint probabilities
#' and warns about this approximation. Equal-probability systematic stages
#' use the SRS approximation governed by `systematic_variance`.
#' Custom methods must declare a supported variance family. Balanced, spatial,
#' Pareto, SPS and Chromy methods have no built-in RWYB mapping.
#'
#' The adapter retains stage-specific sampling units, strata and probabilities.
#' With-replacement stages resample draw occurrences, not distinct population
#' units. Certainty units have conditional replicate factor one. Noncertainty
#' singleton strata raise `samplyr_error_rwyb_singleton` whenever their variance
#' contribution is needed, except under Poisson sampling, whose variance is
#' estimable from one unit.
#'
#' Every selected parent must have a descendant in the final sample. When a
#' later stage is Poisson, a complete frame digest (`"summary"` or `"full"`)
#' is required to check this. Export refuses missing selected parents because
#' silently dropping them changes the earlier-stage resampling distribution.
#' Empty samples cannot be exported. These are export limits. Empty Poisson
#' realizations remain valid sampling outcomes.
#'
#' Replication adds simulation error, so finite replicate variances need not
#' equal analytic variances exactly. Set a seed and increase `replicates` for
#' stable estimates. With `mse = TRUE`, factors use scale `1 / replicates`,
#' while with `mse = FALSE`, they use `1 / (replicates - 1)`. svrep's
#' `estimate_boot_sim_cv()` can assess simulation error for chosen estimates.
#' The direct export records backend and stage methods in the
#' `"samplyr_replication"` attribute.
#'
#' ## Poisson variance
#'
#' Generic survey bootstrap and jackknife methods are refused for Poisson
#' sampling: they can lose the variance of its random sample size. Use
#' `type = "rwyb"`. For single-stage element Poisson sampling, [as_svydesign()]
#' remains available with the analytic Horvitz-Thompson Poisson variance.
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
    "rwyb",
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

  type <- match.arg(type)
  check_forwarded_args(
    enquos(...),
    owned = c("type", "systematic_variance"),
    accepted = if (type == "rwyb") c("replicates", "mse", "compress") else svrepdesign_accepted_args,
    derived = svrepdesign_derived_args,
    forwarded_to = "survey::as.svrepdesign"
  )

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
  poisson_stages <- get_stages_executed(x)[vapply(get_stages_executed(x), function(i) {
    identical(survey_stage_kind(design$stages[[i]]$draw_spec), "rs_poisson")
  }, logical(1))]
  if (length(poisson_stages)) {
    abort_samplyr(c(
      "Generic replicate methods do not represent Poisson sample-size variance.",
      "i" = "Use {.code as_svrepdesign(x, type = \"rwyb\")} for independent Poisson sampling."
    ), class = "samplyr_error_poisson_replicates")
  }
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
  estimator <- match.arg(estimator)
  systematic_variance <- match.arg(systematic_variance)
  type <- match.arg(type)
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
      call = call
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

  parts <- share_contribution_frame(
    source_sample, systematic_variance, call = call
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
  target <- as.data.frame(x)[pos, , drop = FALSE]
  carried <- setdiff(names(target), samplyr_internal_cols(target))

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
  result$call <- call(
    "svydesign",
    ids = parts$ids,
    strata = parts$strata,
    weights = stats::as.formula("~.weight"),
    fpc = parts$fpc,
    data = quote(data),
    nest = nest
  )

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
#' The order is the whole of it. `survey_id_info()` returns `~1` for an
#' unclustered design, meaning every row is a primary sampling unit, and
#' synthesizes an element identifier as a row counter. Either one computed
#' *after* expansion would make each contribution its own unit and split the
#' variance into pieces that are not independent: measured at 631.74 against
#' the exact 1100.08 on the fixture the tests use. So the identifiers are
#' resolved on the source sample and carried through the expansion, and an
#' unclustered design is given an explicit source-unit identifier rather than
#' left at `~1`.
#' @noRd
share_contribution_frame <- function(source_sample, systematic_variance,
                                     call = caller_env()) {
  design <- get_design(source_sample)
  stages <- get_stages_executed(source_sample)
  df <- as.data.frame(source_sample)

  systematic_stages <- systematic_approximated_stages(design, stages, df)
  check_systematic_variance(
    systematic_stages, systematic_variance,
    approximation = "srswor"
  )

  id_info <- survey_id_info(design, stages, df, call = call)
  df <- id_info$df
  ids <- survey_ids_formula(id_info$id_vars)
  strata <- survey_strata_info(df, design, stages, id_info$stage_indices)
  df <- strata$df
  fpc <- survey_fpc_info(df, design, stages, id_info$stage_indices)
  df <- fpc$df

  # Row expansion invalidates source-indexed Poisson and PPS variance terms.
  if (isTRUE(fpc$has_rs_poisson_stage1) || isTRUE(fpc$has_pps_wor)) {
    kind <- if (isTRUE(fpc$has_rs_poisson_stage1)) {
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
    df[[".source_unit"]] <- df[[".sample_id"]]
    ids <- survey_formula_from_vars(".source_unit")
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
