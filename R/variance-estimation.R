#' Variance estimation by selection method
#'
#' Which variance estimator each selection method reaches at export, and how
#' exact it is. Selection support and variance support are separate
#' contracts. Every method in [selection_methods] draws a sample, but not
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
#'     and multi-stage designs.}
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
#'   \item{`pps_systematic_mr`}{Exact expected hit counts, and exact joint
#'     hits, in which zero pairs may occur. The variance uses a
#'     with-replacement approximation under `systematic_variance`, or generic
#'     replicates, with no built-in RWYB mapping.}
#'   \item{Unconstrained `cube`}{Exact inclusion probabilities under the
#'     method contract, and pair probabilities from the high-entropy
#'     approximation. The variance uses approximate linearization or generic
#'     replicates, with no built-in RWYB mapping.}
#'   \item{Bounded `cube`, `lpm2`, `scps`}{Exact inclusion probabilities under
#'     the method contract. Pair probabilities are refused, and so is
#'     linearization. The variance uses the generic `subbootstrap` only, or
#'     `mrbbootstrap` when no size is given.}
#'   \item{Custom methods}{First-order quality as declared, exact or
#'     approximate, and joint support as registered. The variance depends on
#'     the variance-family declaration and the adapter checks.}
#' }
#'
#' Joint information means [joint_expectation()], which exposes PPS and
#' balanced-family quantities. A stage's joint matrix does not cover the
#' final units of a multi-stage design. Exact first-order probabilities do not
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
#' needs one row per stage-1 unit. On a fixed-size stage drawn with unequal
#' probabilities without replacement, [as_svydesign()] defaults to
#' `variance = "YG"` unless `variance` is supplied, because the
#' Horvitz-Thompson quadratic form can be negative even with an exact
#' matrix. A multi-stage sample is exported at stage 1 with a warning, and one
#' with several rows per stage-1 unit is refused
#' (`samplyr_error_pps_rows_per_psu`). Systematic PPS matrices often hold
#' zero pair probabilities, which rule out a design-unbiased variance
#' estimator. A sampled matrix cannot show that every population pair is
#' positive, so supplying one for systematic PPS warns.
#'
#' ## Replicate weights
#'
#' The generic types resample first-stage units within first-stage strata
#' and read the later stages through them. `"mrbbootstrap"` reads the
#' sampling fraction of every stage drawn with equal probabilities, and
#' refuses a stage drawn with unequal probabilities without replacement.
#' RWYB reads the probabilities of every
#' stage for SRS, draws with replacement, Poisson selection and PPS without
#' replacement, the last with approximate joint probabilities.
#' [as_svrepdesign()] documents each type, the designs it suits, and the
#' first-stage readings that make it approximate. Replicates do not recreate
#' ordering, balancing, spatial spreading or hard constraints.
#'
#' `"random_groups"` is the exception. Its replicates are independent samples
#' of the whole design from `execute(reps = R)`, so their spread is a
#' variance for any selection method, ordering, balancing and systematic
#' starts included, at a cost of R samples and R - 1 degrees of freedom
#' (Wolter 2007, ch. 2).
#'
#' ## Systematic stages
#'
#' `systematic` stages are linearized with the SRSWOR estimator and
#' `pps_systematic` stages with Brewer's approximation. A systematic design
#' with interval \eqn{k}{k} has only \eqn{k}{k} distinct samples per stratum,
#' and one sample cannot estimate its variance without assumptions about
#' the frame order (Wolter 2007, ch. 8). The approximations are close when
#' the order is unrelated to the outcome. They overstate the variance when
#' the order follows a trend in the outcome, which acts as implicit
#' stratification, and they can understate it severely when the frame has
#' a periodicity that matches the interval. No replicate type redraws the
#' random start against the ordered frame, so replicates share these
#' errors.
#' Both export functions warn once about these stages, and
#' `systematic_variance = "approximate"` records that the approximation was
#' accepted. Census stages are exempt, and so are stages that drew one unit
#' in each pool, such as one unit per stratum. One random start then takes
#' each unit with its chance, whatever the order.
#'
#' `pps_systematic_mr` stages are exported with replacement, with no FPC,
#' and warn the same way. The direction of the error again depends on the
#' frame order. Take four units of equal size in the order 1, 2, 3, 4 with
#' \eqn{y = (0, 1, 0, 1)}{y = (0, 1, 0, 1)} and a take of 2. The draw selects
#' units 1 and 3 or units 2 and 4, so the estimated total is 0 or 4 and the
#' true variance is 4. The exported variance is 0 for both samples, since
#' the two hits of a sample carry the same value. The error can also go
#' the other way. Units of sizes 2, 1 and 1 with a take of 4 always get 2, 1
#' and 1 hits, so the estimated total has no variance, yet for
#' \eqn{y = (1, 3, 7)}{y = (1, 3, 7)} the exported variance is 37.7. A frame
#' sorted by a variable related to the outcome also pushes the variance up.
#' A take of \eqn{N}{N} or more, or expected hits of one or more, count
#' draws, not units taken with certainty, so they do not make a
#' `pps_systematic_mr` stage a census. A stage whose every unit was taken with
#' certainty, by a certainty rule, is one: the export gives it no variance and
#' raises no warning.
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
#' resample its stage-two units, with the stage-2 correction where it is
#' exact ([as_svrepdesign()] gives the rule), and RWYB gives a certainty unit
#' the factor one.
#'
#' Splitting certainty units out can leave a stratum with one probability
#' unit, whose variance is not estimable. [as_svydesign()] then warns
#' (`samplyr_warning_lonely_psu`), and `options(survey.lonely.psu = "adjust")`
#' or collapsing strata before export resolves it. RWYB refuses such a
#' stratum (`samplyr_error_rwyb_singleton`) unless the stage is Poisson or
#' `lonely.psu = "certainty"` is given at the final stage.
#'
#' A certainty PSU that keeps one unit at stage 2 is such a stratum.
#' `"JKn"`, `"BRR"`, `"Fay"` and `"subbootstrap"` then refuse the sample
#' (`samplyr_error_svrep_conversion_failed`), and so does RWYB.
#' `"bootstrap"` accepts it, and
#' `options(survey.lonely.psu = "adjust")` lets `"JKn"` through. Drawing at
#' least two stage-2 units in every PSU that can be taken with certainty
#' avoids the refusal under every type. `frame_summary(design, frame,
#' stages = 1)` shows those PSUs before selection, in `n_certainty`, and
#' [variance_estimators()] reports the refusal when its frame reaches
#' stage 2.
#'
#' ## One PSU per zone
#'
#' A certainty plan solved with `svyplan::n_alloc_joint(n_psu_per_zone = 1)`
#' draws one PSU from each zone, so no zone has a variance of its own. The
#' export collapses the zones in the variance groups the plan fixed before
#' selection, recorded in `.variance_group_1`, which may cross strata
#' (Valliant, Dever and Kreuter 2018, sec. 15.5.3). One draw per zone has the
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
#' zero in every total, but it has no row, and survey learns its units from
#' rows. A primary unit left with no row at all would take its zero total
#' out of the variance between primary units, which then understates it. The export adds zero rows for it instead, one per
#' empty unit it holds and per draw. A zero row carries its unit's values
#' for the stages above, its frame variables included, and below it a
#' single unit taken whole, so it adds no variance of its own. Its weight is
#' 0, so totals and counts are unchanged, its `.sample_id` is `NA`, and its
#' analysis variables are `NA`, so estimates need `na.rm = TRUE`, which
#' keeps the row in the variance:
#'
#' ```r
#' svy <- as_svydesign(sample)
#' survey::svymean(~income, svy, na.rm = TRUE)
#' ```
#'
#' Every route of [as_svydesign()] and [as_svrepdesign()] carries the zero
#' rows, RWYB, shared weights, two phases and occasions included. A two-phase
#' export holds a phase-1 zero row as a unit phase 2 did not reach, and an
#' occasion holds the zero rows of the panels it activates. Brewer's
#' approximation still errs when units with a measure of size have a total
#' of zero, as it does for any such unit. Under `on_empty = "warn"` the
#' export warns (`samplyr_warning_export_empty_parent`) and counts the rows
#' it added.
#'
#' A design that holds zero rows has three limits from survey.
#' `survey::degf()` leaves out rows of weight 0, so it counts one primary
#' unit fewer per empty one, which widens a confidence interval. And
#' `survey::calibrate()` cannot take a row of weight 0 in a linearized
#' design, and the estimates after it fail. `survey::postStratify()` and
#' `survey::rake()` on either design, and `survey::calibrate()` on a
#' replicate design from [as_svrepdesign()], work once the calibration
#' variables have a value on the zero rows. Any value serves, since the
#' weight is 0.
#'
#' The third limit is in `survey::twophase()`, which reads every phase-1
#' row as a unit phase 2 might have drawn, though phase 2 could never draw
#' a zero row. A two-phase export is refused where that stops survey
#' (`samplyr_error_twophase_empty_unit`). That happens when phase 2 lies in
#' one primary unit and every other one is empty, when phase 2 takes every
#' unit it can draw beside joint probabilities, and when the zero row has a
#' phase-2 stratum value that no phase-2 row shares. The sample is valid in
#' each case. Totals and means from `.weight` are right.
#'
#' An empty unit below the first stage, inside a primary unit that still
#' has rows, keeps no row, since a household with no eligible member is the
#' common case and its sample has to calibrate. The total of its primary
#' unit is right, and the variance within that primary unit leaves the
#' empty unit out, which matters only at large first-stage sampling
#' fractions. Under
#' `on_empty = "warn"`, [as_svydesign()] says so
#' (`samplyr_warning_export_empty_parent`). RWYB resamples every stage, so
#' it gives such a unit a zero row too, and also a unit in which a later
#' Poisson stage selected nothing, which execution records. Its estimated
#' total is zero, and so is the usual estimate of the variance within it,
#' which sums over the units drawn. A sample that lacks the record, such as
#' one rebuilt from another, is refused
#' (`samplyr_error_rwyb_missing_parents`).
#'
#' ## Random-size Poisson methods
#'
#' `bernoulli` and `pps_poisson` select units independently, so the sample
#' size is random and Brewer's approximation would understate the variance. A
#' single-stage design, or one with one row per sampled cluster, is exported
#' with `survey::poisson_sampling()`, the Horvitz-Thompson Poisson variance
#' (Sarndal et al. 1992, sec. 2.8). A multi-stage design with a Poisson stage,
#' or a clustered one with several rows per cluster, needs `type = "rwyb"`.
#' Linearization and the generic replicate types refuse it, since they can
#' lose the variance of the random size.
#'
#' ## Minimum replacement
#'
#' `pps_chromy` and `pps_systematic_mr` give each unit
#' \eqn{\lfloor E(n_i) \rfloor}{floor(E(n_i))} or
#' \eqn{\lceil E(n_i) \rceil}{ceiling(E(n_i))} hits, which is neither
#' with nor without replacement. Following Chromy (2009), both are exported
#' like a with-replacement stage, with no FPC and no `pps` argument, and
#' every replicate type reads them that way. RWYB refuses them. For
#' `pps_chromy`, that Hansen-Hurwitz treatment (Hansen and Hurwitz 1943) can
#' be strongly conservative when the frame order acts as implicit
#' stratification. In a simulated population, Chromy (2010) found it
#' consistently above the true variance, and with-replacement forms with an
#' approximate finite population correction moderately below it. A
#' generalized estimator from Monte Carlo expected joint hits is not
#' offered, since its quadratic form is not guaranteed to be positive. For
#' `pps_systematic_mr` the joint expected hits are exact. A pair
#' can have zero joint expected hits, and then no design-unbiased variance
#' estimator exists from one sample. The export offers no estimator built
#' on the joint expected hits even when every pair is positive, so the same
#' treatment applies, with the frame-order caveat of the systematic stages
#' above. The weighted estimator of a total stays design-unbiased under both
#' methods. `survey::ppsmat()` does not apply, because a PMR matrix's
#' diagonal holds \eqn{E(n_i^2)}{E(n_i^2)} rather than
#' \eqn{E(n_i)}{E(n_i)}. Chauvet (2021) shows that, when no unit expects a
#' hit or more, Chromy's method draws the same design as pivotal sampling,
#' which gives exact results for that without-replacement design. Where
#' independent samples are feasible, `execute(reps = R)` with
#' `type = "random_groups"` repeats the selection itself, random start
#' included.
#'
#' ## Balanced, spatial and custom methods
#'
#' Unconstrained `cube` is linearized with the high-entropy approximation,
#' which ignores the balancing. Balancing removes the variance an outcome
#' shares with the balancing variables, so the reported variance overstates,
#' the more so as the outcome follows them, and is close for an outcome
#' unrelated to them.
#' Bounded `cube`, `lpm2` and `scps` change pairwise selection beyond that
#' approximation, so linearization and [joint_expectation()] refuse them, and
#' `type = "subbootstrap"` gives a generic bootstrap, as does
#' `"mrbbootstrap"` when no size is given, and neither recreates the
#' constraints or the spatial algorithm.
#'
#' A method registered with `sondage::register_method()` can declare a
#' `variance_family`: `"srs"` receives the equal-probability treatment,
#' `"pps_brewer"` Brewer's approximation, `"poisson"` the Poisson estimator
#' (and RWYB), `"wr"` the with-replacement treatment, and `"unsupported"` is
#' refused by linearization, leaving `type = "subbootstrap"`. A random-size
#' method with no declaration is refused, since its selections are not known
#' to be independent.
#' Its matrix for the joint-probability route is the one its `joint_fn`
#' returns, and a method registered without `joint_fn` has none.
#'
#' ## Two-phase samples and occasions
#'
#' A materialized occasion is exported by ultimate clusters, whatever method
#' drew its master, and replicated the same way. [as_svydesign()] gives the
#' details, for windows of several occasions too.
#'
#' [survey::twophase()] takes no `pps` specification, so a two-phase sample,
#' or an occasion exported with `method`, whose first phase was drawn with
#' unequal probabilities without replacement, or by `cube`, `pps_poisson` or
#' a spatial method, is refused, although its weights are exact. For a
#' variance, an ultimate-cluster design treating phase-1 units as drawn with
#' replacement leaves out phase 1's correction and overstates the variance,
#' the more so as phase 1's sampling fraction grows, and more again when
#' phase 2 was drawn across the phase-1 units, where little of the outcome's
#' variation lies between them. [as_svydesign()] gives the call. A
#' two-phase sample has a replicate export in three shapes, the subsample
#' within pools below, a second phase of a phase 1 fixed in one primary unit
#' (every stage taken for sure, which can be a census of its elements), and
#' random groups of a design executed whole with `reps`, a refused one
#' included ([as_svrepdesign()]).
#'
#' Phase 2 is drawn from the phase-1 sample, so its joint inclusion
#' probabilities are computed there. A single-stage phase 2 drawn with
#' `pps_sampford`, `pps_cps`, `pps_brewer`, `pps_sps` or `pps_pareto` exports
#' with them under `method = "full"`, exact for Sampford and CPS and the
#' high-entropy approximation for the other three. The Horvitz-Thompson form
#' of that variance is unbiased but can be negative, and is unstable when
#' the outcome follows the size measure, so the export uses the
#' Sen-Yates-Grundy form for the phase-2 term, which is non-negative when
#' \eqn{\pi_i \pi_j \ge \pi_{ij}}{pi_i pi_j >= pi_ij} for every pair.
#' Systematic PPS stays refused, since its pair probabilities can
#' be zero, which biases the variance estimator under any matrix, and the
#' sampled matrix cannot show whether they are. So are a multi-stage phase 2
#' with an unequal-probability stage and `method = "approx"` or `"simple"`,
#' which drop the matrix (`samplyr_error_twophase_phase2_pps`).
#'
#' A clustered phase 1 with an equal-probability first stage does export,
#' but its variance is stable only when phase 2 is drawn inside each phase-1
#' unit. Drawn across them, the two-phase variance is unbiased over repeated
#' samples but unstable in one, and can be negative, and the export warns
#' (`samplyr_warning_twophase_across_units`).
#'
#' A second phase drawn by equal probability within each final-stage pool of
#' phase 1, whose final stage draws elements by equal probability, is
#' exported as phase 1's multi-stage design with the smaller final take
#' ([as_svydesign()] states the shape). That is its exact design, so the
#' linearized variance keeps phase 1's corrections, where the
#' ultimate-cluster call would leave them out.
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
#' Chauvet, G. (2021). A note on Chromy's sampling procedure. *Journal of
#' Survey Statistics and Methodology*, 9(5), 1050-1061.
#' \doi{10.1093/jssam/smaa025}
#'
#' Chromy, J.R. (2009). Some generalizations of the Horvitz-Thompson
#' estimator. *Proceedings of the Survey Research Methods Section, American
#' Statistical Association*, 217-227.
#'
#' Chromy, J.R. (2010). Horvitz-Thompson variance weights: exact vs.
#' approximate. *Proceedings of the Survey Research Methods Section,
#' American Statistical Association*, 4808-4816.
#'
#' Cochran, W.G. (1977). *Sampling Techniques*. 3rd edition. Wiley.
#'
#' Hansen, M.H. and Hurwitz, W.N. (1943). On the theory of sampling from
#' finite populations. *Annals of Mathematical Statistics*, 14(4), 333-362.
#' \doi{10.1214/aoms/1177731356}
#'
#' \enc{Hájek}{Hajek}, J. (1964). Asymptotic theory of rejective sampling with
#' varying probabilities from a finite population. *Annals of Mathematical
#' Statistics*, 35(4), 1491-1523.
#'
#' \enc{Särndal}{Sarndal}, C.-E., Swensson, B. and Wretman, J. (1992). *Model Assisted
#' Survey Sampling*. Springer.
#'
#' Valliant, R., Dever, J.A. and Kreuter, F. (2018). *Practical Tools for
#' Designing and Weighting Survey Samples*. 2nd edition. Springer.
#'
#' Wolter, K.M. (2007). *Introduction to Variance Estimation*. 2nd edition.
#' Springer.
#'
#' @seealso [as_svydesign()], [as_svrepdesign()], [joint_expectation()],
#'   [selection_methods]
#'
#' @family reference topics
#' @name variance_estimation
NULL
