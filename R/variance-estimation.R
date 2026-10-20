#' Variance estimation by selection method
#'
#' Which variance estimator each selection method reaches at export, and how
#' exact it is. Selection support and variance support are separate
#' contracts. Every method in `?selection_methods` draws a sample, but not
#' every one has an exact variance estimator in the survey framework.
#'
#' A sample reaches `survey` in three ways. [as_svydesign()] linearizes,
#' with Brewer's approximation for PPS stages unless `pps` is given. For a
#' single-stage sample, a joint-probability matrix from [joint_expectation()]
#' passed as `pps = survey::ppsmat()` replaces that approximation.
#' [as_svrepdesign()] builds replicate weights through `survey`'s generic
#' types or svrep's Rao-Wu-Yue-Beaumont (RWYB) bootstrap.
#' [variance_estimators()] reports, before anything is drawn, which
#' estimators a design supports, which only approximately, and which the
#' export would refuse.
#'
#' ## Method families and their variance estimators
#'
#' Each entry gives the first-order chances of a method or family, what
#' [joint_expectation()] gives for pairs of units, and how the export
#' estimates the variance.
#'
#' \describe{
#'   \item{`srswor`}{Exact inclusion probabilities. Pair probabilities follow
#'     from SRS formulas, which [joint_expectation()] does not expose. The
#'     variance uses SRS with an FPC, standard replicates or RWYB.}
#'   \item{`srswr`, `pps_multinomial`}{Exact expected hit counts, and exact
#'     joint hits for PPS. The variance treats each draw as an occurrence with
#'     replacement, with standard replicates or RWYB.}
#'   \item{`systematic`}{Exact inclusion probabilities. Pair probabilities are
#'     not exposed. The variance uses the SRS approximation under
#'     `systematic_variance`, or generic replicates.}
#'   \item{`pps_systematic`}{Exact inclusion probabilities, and an exact
#'     order-specific joint matrix, in which zero pairs may occur. The
#'     variance uses Brewer's approximation under `systematic_variance`, a
#'     joint matrix, or generic replicates.}
#'   \item{`bernoulli`, `pps_poisson`}{Exact independent probabilities, and an
#'     exact Poisson joint matrix for PPS. The variance is the analytic
#'     Poisson variance of a single stage, or RWYB for the supported clustered
#'     and multistage designs.}
#'   \item{`pps_brewer`, `pps_cps`, `pps_sampford`}{Exact inclusion
#'     probabilities. Pair probabilities are approximate for Brewer and exact
#'     for CPS and Sampford. The variance uses Brewer's approximation by
#'     default, an explicit joint matrix, or PPS-compatible replicates,
#'     including an approximate RWYB.}
#'   \item{`pps_sps`, `pps_pareto`}{Approximate targets, and pair
#'     probabilities from the high-entropy approximation using those targets.
#'     The variance uses Brewer's approximation or generic PPS-compatible
#'     replicates, with no built-in RWYB mapping.}
#'   \item{`pps_chromy`}{Exact expected hit counts, and joint hits by Monte
#'     Carlo. The variance uses a with-replacement or generic replicate
#'     approximation to minimum replacement, with no built-in RWYB mapping.}
#'   \item{Unconstrained `cube`}{Exact inclusion probabilities under the
#'     method contract, and pair probabilities from the high-entropy
#'     approximation. The variance uses approximate linearization or generic
#'     replicates, with no built-in RWYB mapping.}
#'   \item{Bounded `cube`, `lpm2`, `scps`}{Exact inclusion probabilities under
#'     the method contract. Pair probabilities are refused, and so is
#'     linearization. The variance uses the generic `subbootstrap` or
#'     `mrbbootstrap` only.}
#'   \item{Custom methods}{First-order quality as declared, exact or
#'     approximate, and joint support as registered. The variance depends on
#'     the variance-family declaration and the adapter checks.}
#' }
#'
#' Joint information means [joint_expectation()], which exposes PPS and
#' balanced-family quantities. A stage's joint matrix does not cover the
#' final units of a multistage design. Exact first-order probabilities do not
#' imply exact variance or confidence-interval coverage, since finite-sample
#' PPS total estimators can be skewed. `vignette("survey-analysis")` shows a
#' log-scale interval for strictly positive domain totals.
#'
#' ## Linearization with Brewer's approximation
#'
#' Fixed-size PPS stages without replacement (`pps_brewer`, `pps_systematic`,
#' `pps_cps`, `pps_sampford`, `pps_sps`, `pps_pareto`) are linearized by
#' default with Brewer's approximation (`pps = "brewer"` in `survey`), which
#' derives joint inclusion probabilities from the marginal ones (Berger
#' 2004). Brewer names the variance estimator here, not the selection
#' algorithm, so a Sampford sample gets the same default. It is not an exact
#' substitute for the joint probabilities, and its accuracy depends on the
#' design and population. [as_svydesign()] documents each value of `pps`.
#'
#' ## Linearization with joint probabilities
#'
#' A matrix from [joint_expectation()] is exact for CPS, Sampford, systematic
#' PPS and Poisson selection. Generalized Brewer, SPS, Pareto and
#' unconstrained cube use the high-entropy approximation (Hajek 1964; Brewer
#' and Donadio 2003). Exact recursive formulas exist for Brewer's method
#' (Brewer 2002, ch. 9) but cost \eqn{O(N^3)}{O(N^3)}.
#'
#' `survey` applies a matrix to one stage and reads it by row, so the sample
#' needs one row per stage-1 unit. A multistage sample is exported at stage 1
#' with a warning, and one with several rows per stage-1 unit is refused
#' (`samplyr_error_pps_rows_per_psu`). Systematic PPS matrices often hold
#' zero pair probabilities, which rule out a design-unbiased variance
#' estimator. A sampled matrix cannot show that every population pair is
#' positive, so supplying one for systematic PPS warns.
#'
#' ## Replicate weights
#'
#' The generic types (`"JK1"`, `"JKn"`, `"BRR"`, `"Fay"`, `"bootstrap"`,
#' `"subbootstrap"`) resample first-stage units within first-stage strata,
#' treating a PPS first stage drawn without replacement as drawn with
#' replacement. That overstates the variance when its sampling fractions are
#' large (`samplyr_warning_replicate_wr_first_stage`). Under an
#' equal-probability first stage, `"JK1"`, `"JKn"` and `"bootstrap"` apply
#' its finite population correction to the variance of the later stages too,
#' which understates it when the first-stage fraction is large
#' (`samplyr_warning_replicate_first_stage_fpc`). `"mrbbootstrap"` and
#' `"rwyb"` read every stage. RWYB supports SRS, draws with replacement,
#' Poisson selection, and Brewer, CPS, Sampford and systematic PPS, the last
#' four with approximate joint probabilities and a warning. It is used only for
#' `type = "rwyb"`, never by `"auto"`, and needs the svrep package.
#' Replicates do not recreate ordering, balancing, spatial spreading or hard
#' constraints, and [as_svrepdesign()] states the adapter's limits.
#'
#' `"random_groups"` is the exception. Its replicates are independent samples
#' of the whole design from `execute(reps = R)`, so their spread is a
#' variance for any selection method, ordering, balancing and systematic
#' starts included, at a cost of R samples and R - 1 degrees of freedom.
#'
#' ## Systematic stages
#'
#' `systematic` stages are linearized with the SRSWOR estimator and
#' `pps_systematic` stages with Brewer's approximation. A systematic design
#' with interval \eqn{k}{k} has only \eqn{k}{k} distinct samples per stratum,
#' so its true variance for one frame need not be near either value. The
#' table below comes from repeated draws of 90 per stratum from 1800, with
#' an interval of 20. The ratio is the reported variance over the true
#' variance, and the coverage is that of a 95% interval.
#'
#' | Frame order                  | Ratio  | Coverage |
#' |------------------------------|--------|----------|
#' | Random                       | 0.73   | 90.2%    |
#' | Ordered by a trend           | 1.76   | 100.0%   |
#' | Period equal to the interval | 0.0006 | 9.8%     |
#'
#' `pps_systematic` behaves the same way. Brewer's approximation gave 0.003
#' of the true variance with 9% coverage on a frame whose period matched the
#' interval, and 1.46 on a frame sorted by size. A favorable order is thus
#' conservative, and a frame resonating with the interval gives intervals
#' that almost never cover. No replicate type redraws the random start
#' against the ordered frame, so replicates are also too large when the
#' order lowers the true variance and too small when it raises it.
#' Both export functions warn once about these stages, and
#' `systematic_variance = "approximate"` records that the approximation was
#' accepted. Census stages are exempt.
#'
#' ## Certainty units
#'
#' Under linearization, units with inclusion probability one at a stage
#' exported with Brewer's treatment form a take-all stratum within the
#' stage's own strata and parents, however they reached probability one,
#' balanced designs included. As a census, the stratum contributes no
#' variance and no degrees of freedom (Cochran 1977, sec. 5.8; Sarndal
#' et al. 1992, sec. 3.7). Random-size designs keep the Poisson treatment.
#' The generic replicate types make a certainty PSU its own stratum and
#' resample its stage-two units, and RWYB gives a certainty unit the factor
#' one.
#'
#' Splitting certainty units out can leave a stratum with one probability
#' unit, whose variance is not estimable. [as_svydesign()] then warns
#' (`samplyr_warning_lonely_psu`), and `options(survey.lonely.psu = "adjust")`
#' or collapsing strata before export resolves it. RWYB refuses such a
#' stratum (`samplyr_error_rwyb_singleton`) unless the stage is Poisson or
#' `lonely.psu = "certainty"` is given at the final stage.
#'
#' ## One PSU per zone
#'
#' A certainty plan solved with `svyplan::n_alloc(n_psu_per_zone = 1)`
#' draws one PSU from each zone, so no zone has a variance of its own. The
#' export collapses the zones in the variance groups the plan fixed before
#' selection, recorded in `.pair_1`, which may cross strata (Valliant,
#' Dever and Kreuter 2018, sec. 15.5.3). One draw per zone has the
#' with-replacement variance, so the remainder PSUs carry no finite
#' population correction under linearization, RWYB or the generic replicate
#' types, while certainty PSUs keep theirs and contribute their later
#' stages. The collapsed estimator overestimates the variance by a term in
#' the squared differences between the totals of the zones grouped together
#' (Wolter 2007, sec. 2.5), most where a group joins zones of different
#' sizes.
#' A group of three zones rules out balanced half-samples.
#'
#' ## Selected units with nothing below them
#'
#' A unit accepted by `on_empty` with nothing selected under it keeps its
#' zero in every total. When it is a stage-two or deeper unit inside a
#' primary unit that still has rows, [as_svydesign()] warns
#' (`samplyr_warning_export_empty_parent`), because survey computes that
#' unit's within-parent term without it. This matters only at large
#' first-stage sampling fractions. When a whole primary unit has no row, the
#' variance between primary units would lose its zero total and could come
#' out as zero, so every route refuses it (`samplyr_error_export_empty_psu`,
#' and `samplyr_error_rwyb_missing_parents` for RWYB).
#'
#' ## Random-size Poisson methods
#'
#' `bernoulli` and `pps_poisson` select units independently, so the sample
#' size is random and Brewer's approximation would understate the variance. A
#' single-stage design, or one with one row per sampled cluster, is exported
#' with `survey::poisson_sampling()`, the Horvitz-Thompson Poisson variance
#' (Sarndal et al. 1992, sec. 2.8). A multistage design with a Poisson stage,
#' or a clustered one with several rows per cluster, needs `type = "rwyb"`.
#' Linearization and the generic replicate types refuse it, since they can
#' lose the variance of the random size.
#'
#' ## Chromy's sequential method
#'
#' `pps_chromy` gives each unit \eqn{\lfloor E(n_i) \rfloor}{floor(E(n_i))} or
#' \eqn{\lceil E(n_i) \rceil}{ceiling(E(n_i))} hits, which is neither
#' with nor without replacement. Following Chromy (2009), it is exported like a
#' with-replacement stage, with no FPC and no `pps` argument. That
#' Hansen-Hurwitz treatment can be strongly conservative when the frame order
#' acts as implicit stratification, but a generalized estimator from Monte
#' Carlo expected hits was often negative, so it is not offered.
#' `survey::ppsmat()` does not apply, because a PMR matrix's diagonal holds
#' \eqn{E(n_i^2)}{E(n_i^2)} rather than \eqn{E(n_i)}{E(n_i)}. Chauvet (2019)
#' gives exact results for the related without-replacement design.
#'
#' ## Balanced, spatial and custom methods
#'
#' Unconstrained `cube` is linearized with the high-entropy approximation,
#' which ignores the balancing. The more closely an outcome follows the
#' balancing variables, the more the reported variance overstates. In a
#' simulation the standard error was about five times the true spread for an
#' outcome correlated 0.98 with the balancing variable, and close to right
#' for a weakly related one.
#' Bounded `cube`, `lpm2` and `scps` change pairwise selection beyond that
#' approximation, so linearization and [joint_expectation()] refuse them, and
#' `type = "subbootstrap"` or `"mrbbootstrap"` gives a generic PPS bootstrap
#' that does not recreate the constraints or the spatial algorithm.
#'
#' A method registered with `sondage::register_method()` can declare a
#' `variance_family`: `"srs"` receives the equal-probability treatment,
#' `"pps_brewer"` Brewer's approximation, `"poisson"` the Poisson estimator
#' (and RWYB), `"wr"` the with-replacement treatment, and `"unsupported"` is
#' refused by linearization, leaving `type = "subbootstrap"`. A random-size
#' method with no declaration is refused, since its selections are not known
#' to be independent.
#'
#' ## Two-phase samples
#'
#' [survey::twophase()] takes no `pps` specification, so a two-phase sample
#' or a wave whose first phase was drawn with unequal probabilities without
#' replacement, or by `cube`, `pps_poisson` or a spatial method, is refused,
#' although its weights are exact. For a variance, an ultimate-cluster design
#' treating phase-1 units as drawn with replacement is conservative, at 1.3,
#' 1.8 and 2.1 times the true variance for phase-1 sampling fractions of
#' 1/6, 1/3 and 1/2 with phase 2 drawn inside each phase-1 unit, and 2.4 to
#' 3.6 times with phase 2 drawn across the phase-1 sample.
#' On other populations the across case reached 4 to 15 times, higher when
#' little of the outcome's variation lies between phase-1 units.
#' [as_svydesign()] gives the call. No two-phase sample has a replicate
#' export, except the subsample within pools below.
#'
#' Phase 2 is drawn from the phase-1 sample, so its joint inclusion
#' probabilities are computed there. A single-stage phase 2 drawn with
#' `pps_sampford`, `pps_cps`, `pps_brewer`, `pps_sps` or `pps_pareto` exports
#' with them under `method = "full"`, exact for Sampford and CPS and the
#' high-entropy approximation for the other three. survey reads such a
#' matrix in the Horvitz-Thompson form, which is unbiased but unstable when
#' the outcome follows the size measure. In a simulation it was negative in
#' 3.6% of samples, and 95% intervals covered 86%. The export uses the
#' Sen-Yates-Grundy form for the phase-2 term instead and leaves the phase-1
#' term as survey computes it. In the same simulation that form was never
#' negative and covered 93%. Systematic PPS stays refused, since its zero pair
#' probabilities bias the variance estimator under any matrix. So are a
#' multistage phase 2 with an unequal-probability stage and
#' `method = "approx"` or `"simple"`, which drop the matrix
#' (`samplyr_error_twophase_phase2_pps`).
#'
#' A clustered phase 1 with an equal-probability first stage does export,
#' but its variance is stable only when phase 2 is drawn inside each phase-1
#' unit. Drawn across them, survey's variance was negative in 26% to 51% of
#' samples and 1.4 to 5 times the true variance otherwise, and the export
#' warns (`samplyr_warning_twophase_across_units`).
#'
#' A second phase drawn by equal probability within each final-stage pool of
#' phase 1, whose final stage draws elements by equal probability, is
#' exported as phase 1's multistage design with the smaller final take
#' ([as_svydesign()] states the shape). In a simulation with 8 EAs drawn by
#' `pps_brewer` in each of 2 strata, 20 households in each and half of them
#' kept, the estimated variance of a total was 0.99 of the true variance,
#' against 1.22 for the ultimate-cluster call. With `srswor` EAs it was 0.98,
#' as [survey::twophase()] gave on the same design (0.98), against 1.21 for
#' the ultimate cluster.
#'
#' @references
#' Berger, Y.G. (2004). A simple variance estimator for unequal probability
#' sampling without replacement. *Journal of Applied Statistics*, 31,
#' 305-315.
#'
#' Brewer, K.R.W. (2002). *Combined Survey Sampling Inference: Weighing
#' Basu's Elephants*. Arnold, ch. 9.
#'
#' Brewer, K.R.W. and Donadio, M.E. (2003). The high entropy variance of the
#' Horvitz-Thompson estimator. *Survey Methodology*, 29(2), 189-196.
#'
#' Chauvet, G. (2019). Properties of Chromy's sampling procedure.
#' *arXiv:1912.10896*.
#'
#' Chromy, J.R. (2009). Some generalizations of the Horvitz-Thompson
#' estimator. *JSM Proceedings, Survey Research Methods Section*.
#'
#' Cochran, W.G. (1977). *Sampling Techniques*. 3rd edition. Wiley.
#'
#' \enc{Hájek}{Hajek}, J. (1964). Asymptotic theory of rejective sampling with
#' varying probabilities from a finite population. *Annals of Mathematical
#' Statistics*, 35(4), 1491-1523.
#'
#' Sarndal, C.-E., Swensson, B. and Wretman, J. (1992). *Model Assisted
#' Survey Sampling*. Springer.
#'
#' Valliant, R., Dever, J.A. and Kreuter, F. (2018). *Practical Tools for
#' Designing and Weighting Survey Samples*. 2nd edition. Springer.
#'
#' Wolter, K.M. (2007). *Introduction to Variance Estimation*. 2nd edition.
#' Springer.
#'
#' @seealso [as_svydesign()], [as_svrepdesign()], [joint_expectation()],
#'   `?selection_methods`
#'
#' @family survey export
#' @name variance_estimation
NULL
