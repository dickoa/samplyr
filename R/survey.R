#' Convert a tbl_sample to a survey design object
#'
#' Creates a [survey::svydesign()] object from a `tbl_sample`, using
#' the sampling design metadata (strata, clusters, weights, and
#' finite population corrections) captured during [execute()].
#'
#' @param x A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to [survey::svydesign()], or to
#'   [survey::twophase()] for a sample exported as two phases. In
#'   particular, you can pass `pps = survey::ppsmat(joint_matrix)` to
#'   supply exact joint inclusion probabilities instead of the default
#'   Brewer approximation (see Details). Every argument must be named, and
#'   its name must be one the receiving function accepts: `nest` and
#'   `method` follow the `...` and so are matched exactly, and a near miss
#'   such as `nes` is reported rather than forwarded. The design arguments
#'   themselves (`ids`, `strata`, `weights`, `probs`, `fpc`, `data`, and
#'   `subset` for a two-phase sample) are derived from the sample and cannot
#'   be given here.
#' @param nest If `TRUE` (the default), relabel cluster ids to enforce
#'   nesting within strata, which suits most complex survey designs. Passed
#'   to [survey::svydesign()]. It has no effect on a sample exported with
#'   [survey::twophase()], and giving it there raises a warning.
#' @param systematic_variance What to do about the variance approximation
#'   used for systematic stages: simple random sampling for `systematic`,
#'   Brewer's for `pps_systematic`, and with replacement for
#'   `pps_systematic_mr`. `"warn"` (default) applies it and warns
#'   once per call, naming every affected stage and its approximation.
#'   `"approximate"` applies it silently, for a caller who has acknowledged
#'   it, and `"error"` refuses. Census stages are exempt, since a stage that
#'   took everything within reach contributes no variance, and so is a
#'   `pps_systematic` first stage exported with a caller-supplied `pps`
#'   object. A stage that drew one unit in each pool is exempt too, since a
#'   single random start takes each unit with its chance whatever the frame
#'   order. A `pps_systematic_mr` stage is never exempt as a census, since a
#'   take of \eqn{N}{N} or more counts draws, and the with-replacement export
#'   gives it a variance even when every unit's hits are fixed. The choice, the
#'   affected stages and each stage's approximation are recorded in the
#'   `"samplyr_systematic_variance"` attribute of the result.
#'   [as_svrepdesign()] takes the same argument for its own approximation of
#'   these stages.
#' @param method For two-phase samples, the variance method passed to
#'   [survey::twophase()]. One of `"full"`, `"approx"`, or `"simple"`.
#'   This argument is only accepted for two-phase samples. A materialized
#'   occasion is exported by ultimate clusters unless `method` is given, which
#'   asks for its two-phase export instead.
#' @param stages `NULL`, the default, exports every executed stage. A prefix
#'   of the executed stages, such as `1` or `1:2`, keeps those stages and
#'   reads the last as drawn with replacement within its parents, without a
#'   finite population correction, so it carries the variance of every stage
#'   below. `stages = 1` is the ultimate-cluster variance. A kept stage of
#'   random size (`bernoulli`, `pps_poisson`), or one below a certainty
#'   unit, is refused, because this reading drops the variance of its
#'   sample size. `as_svrepdesign(x, type = "rwyb")` estimates it. A
#'   materialized occasion and a settled reserve take `stages = 1` only,
#'   which is how they are exported anyway, and a two-phase sample or one
#'   with shared weights takes none (`samplyr_error_survey_argument`).
#'
#' @return A `survey.design2` object for single-phase and multi-stage samples
#'   and for materialized occasions, or a `twophase`/`twophase2` object for
#'   two-phase samples and for occasions exported with `method`.
#'
#' @details
#' Every executed stage contributes one `ids` term, one `fpc` term and, when
#' stratified, one `strata` term, so [survey::svydesign()] represents the
#' multi-stage structure in its linearization (Sarndal et al. 1992, ch.
#' 4.3):
#'
#' - **Cluster ids** (`ids`): the `cluster_by()` variable of a clustered
#'   stage. A stage clustered by several variables, which execution treats
#'   as one cluster id, gets one synthesized interaction column. A final
#'   unclustered stage gets a synthesized row-identity column so that its
#'   sampling variance is represented. A WR or PMR stage uses its `.draw_k`
#'   column, so each draw is an independent unit of the Hansen-Hurwitz
#'   variance. This is exact for WR and an approximation for PMR, whose
#'   draws are dependent.
#' - **Strata** (`strata`): aligned with `ids`. A stage stratified by
#'   several variables exports their cross-classification as one
#'   synthesized column. Trailing unstratified stages are omitted, and
#'   unstratified stages before a stratified stage get a constant
#'   placeholder column.
#' - **Weights** (`weights`): the `.weight` column, the product of the
#'   per-stage weights described in [sample_columns]. They are design
#'   weights. A sample whose `.weight` was edited is refused, and weights
#'   adjusted for nonresponse or calibrated are computed on the exported
#'   design, with [survey::calibrate()] or [survey::postStratify()].
#' - **FPC** (`fpc`): aligned with `ids`, on one scale for every stage.
#'   Without an unequal-probability stage drawn without replacement, an
#'   equal-probability WOR stage passes its stratum population count
#'   \eqn{N_h}, and a WR or PMR stage passes no correction. With one, every
#'   WOR stage passes its stage inclusion probability
#'   \eqn{1 / w_k = \pi_k}{1/w_k = pi_k}, and WR and PMR stages pass 0. A
#'   multi-stage design with a random-size Poisson stage is refused.
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
#' ([variance_estimation]). A design whose first stage is a census of PSUs
#' attributes all variance to the later stages. Executing stages
#' separately, as `stage1 <- execute(design, psu_frame, stages = 1)` then
#' `sample <- execute(stage1, listing_frame)`, still gives one multi-stage
#' design.
#'
#' An unclustered element-sampling stage *followed by further stages* is
#' not nested cluster sampling, because the later selections are
#' conditional on the realized element sample, which is phase sampling.
#' [execute()] refuses it (`samplyr_error_stage_parent_id`). Express it as a
#' two-phase sample instead: execute the element stage under its first-phase
#' design, then a new second-phase design with that sample as its frame.
#'
#' A variance compares primary units, so a design whose first stage holds a
#' single unit not taken with certainty, outside any stratum, has no
#' variance estimate and is refused (`samplyr_error_export_one_unit`). That
#' covers one unit drawn from several and a single draw with replacement,
#' whose weight of one is an expected hit rather than a certainty. A census
#' of that unit, or a recorded certainty, is exported, with no first-stage
#' variance.
#'
#' ## Two-phase samples
#'
#' A *new* phase-2 `sampling_design` executed with the phase-1 `tbl_sample`
#' as its frame, as in `phase2 <- execute(design2, phase1)`, records a
#' previous-phase link, and `as_svydesign()` calls [survey::twophase()].
#' Each phase-2 row carries `.phase1_row`, the position of the phase-1 row it
#' was drawn from, and the export joins the phases on it. Neither phase needs
#' to declare an identifier, and a unit phase 1 drew more than once, such as
#' a PSU hit twice by `pps_multinomial`, links to the hit phase 2 reached.
#' Rows that a later phase-2 stage drew from a register of its own carry no
#' position, so the phases are then joined on the identifiers either phase
#' declares with [cluster_by()], which must identify the phase-1 rows phase 2
#' reaches (`samplyr_error_twophase_bridge`) and be constant within each
#' phase-2 unit (`samplyr_error_phase_key_ambiguous`).
#'
#' Phase 2 may take whole phase-1 EAs, or EAs and then some of their
#' households. Each phase-2 unit must lie within one phase-1 primary unit,
#' each hit counting as one (`samplyr_error_phase_cluster_spans_psu`). For
#' `method = "full"`, declare the households phase 2 draws, as in
#' `cluster_by(hh_id)`. [survey::twophase()] reads phase 2 as a subset of
#' the phase-1 rows, which cannot hold a unit drawn twice, so a phase 2 drawn
#' with replacement is refused (`samplyr_error_twophase_phase2_wr`). So is a
#' phase 1, or a phase 2, whose rows hold a single primary unit outside a
#' stratum (`samplyr_error_export_one_unit`).
#'
#' The three values of `method` read the phase-1 part of the variance
#' differently. `"full"` reads the probabilities stage by stage, from phase
#' 1's finite population corrections. `"approx"` and `"simple"` estimate it
#' from the phase-2 rows, weighting each phase-1 primary unit by its chance
#' of entering phase 2. That is exact when phase 2 takes whole phase-1
#' units or phase 1 selects elements. When phase 2 subsamples inside the
#' units phase 1 selected, the row's chance stands in for the unit's, which
#' is one, and the phase-1 part is overstated
#' (`samplyr_warning_twophase_approx`). `"full"` cannot state the
#' probabilities of a phase 1 drawn with replacement above a later stage,
#' unless that stage took only certainty units, and is refused there
#' (`samplyr_error_twophase_stage_probs`). It is refused as well when phase
#' 2 subsamples inside clusters that phase 1's last stage drew with
#' replacement, where it understates the variance
#' (`samplyr_error_twophase_wr_subsample`). A phase 2 drawn within each hit
#' is exported as one phase instead (see below). In the second case, draw
#' the units inside those clusters as a stage of phase 1 first, with
#' `add_stage() |> draw(frac = 1)`.
#'
#' The export computes the phase-1 part of the variance with each phase-1
#' stratum's own sample size, also when phase 2 keeps unequal shares of the
#' phase-1 strata. A phase 2 stratified across the phase-1 strata can give a
#' negative variance in some samples, since the Horvitz-Thompson form is not
#' guaranteed positive, and `method = "approx"` gives a finite standard
#' error there.
#'
#' A clustered first phase needs its second phase drawn inside each of its
#' units, with `stratify_by(<unit>)`, or by taking whole phase-1 units
#' before sampling inside them. When phase 2 draws smaller units across the
#' phase-1 clusters, the estimator is unbiased over repeated samples but
#' unstable in one, and can be negative
#' (`samplyr_warning_twophase_across_units`). The weights are exact either
#' way.
#'
#' ## A subsample within pools is one phase
#'
#' One shape is not exported as two phases. Phase 1's final stage draws
#' elements by `srswor` or `systematic`, and phase 2 draws the same way,
#' without clusters, with one stratum per final-stage pool of phase 1, as in
#' `stratify_by(ea_id)` when the final stage draws households within EAs. A
#' simple random subsample of a simple random sample is a simple random
#' sample, so the two phases are phase 1's multistage design with a smaller
#' final take. `as_svydesign()` and `as_svrepdesign()` export it that way,
#' with phase 1's strata, clusters and finite population corrections and
#' phase 2's weights, whatever phase 1's earlier stages drew. Half of each
#' EA's households kept for a second questionnaire is this shape. `method`
#' does not apply to it, and no row identifier is needed to link the
#' phases. Below a stage that can take a unit more than once, each hit is a
#' pool of its own. Phase 2 names it by the hit number it carries, as in
#' `stratify_by(ea_id, .phase1_draw_1)`, and is then exported this way.
#' `stratify_by(ea_id)` alone pools the hits of an EA drawn twice, and keeps
#' the two-phase route. When phase 2 is `systematic`, the final take is a
#' systematic one, and `systematic_variance` applies to it, unless phase 2
#' took every unit of every pool or one unit of each. A phase 2 stratified
#' more coarsely or more finely than the pools, clustered, or drawn with
#' unequal probabilities keeps the two-phase route.
#'
#' ## Two-phase samples that are refused
#'
#' [survey::twophase()] takes no `pps` for the first phase, so any other
#' two-phase sample, or occasion exported with `method`, whose phase 1 was
#' drawn with unequal probabilities without replacement is refused
#' (`samplyr_error_twophase_phase1_pps`, `samplyr_error_occasion_phase1_pps`).
#' So are `cube`, first-stage `pps_poisson` and the spatial methods, which
#' are unsupported at either phase. Equal-probability first phases export,
#' stratified, clustered, multistage and with-replacement alike. No
#' two-phase sample has a replicate export, except the subsample within
#' pools above. The weights of a refused sample are exact, so totals and
#' means are right. For a variance, the ultimate-cluster approximation
#' treats phase 1's units as drawn with replacement and carries everything
#' below in them. It leaves out phase 1's correction and overstates the
#' variance, and more so when phase 2 was drawn across the phase-1 units:
#'
#' ```r
#' survey::svydesign(ids = ~ea_id, strata = ~region, weights = ~.weight,
#'                   data = as.data.frame(phase2))
#' ```
#'
#' ## Occasions
#'
#' A materialized occasion, `execute(master, period = p)`, is exported as one
#' phase by ultimate clusters. The master's stage-1 units are the clusters,
#' its stage-1 stratification variables the strata, and the occasion's weights
#' the weights, with no finite population correction. Every later stage and
#' the activation are carried in the unit totals, so the master may have
#' been drawn by any method, with probabilities proportional to size
#' included. It is the variance rotating household surveys publish for each
#' quarter or month. Zones and variance pairs are not used, since activation
#' leaves most zones with one unit or none.
#'
#' A stage-1 unit taken with certainty, with a later stage drawn inside it,
#' represents itself. It is a stratum of its own, whose clusters are its
#' units of the next stage, since only they vary between samples. A unit of
#' a stratum taken as a census counts as taken with certainty, unless the
#' panels are assigned at stage 1, so that each occasion holds only some of
#' its units. A unit taken with certainty and nothing drawn inside it, a
#' stage-1 cluster with no later stage or an element, is the same in every
#' sample. It is a stratum of its own taken as a census, the one stratum
#' with a finite population correction, so it adds no variance. Leaving out
#' the correction elsewhere overstates the variance where a stratum's
#' sampling fraction is large.
#'
#' A window of several occasions, `execute(master, period = 1:3)`, is exported
#' the same way. A unit's rows over the window are one cluster, and the
#' strata are crossed with the quota block and the occasions of the window
#' a unit is active at, which its panel fixes. The design is calibrated on
#' each stratum's total of the units' activation multipliers, which leaves
#' the weights unchanged and gives merged strata ratio residuals.
#' [panel_assignment] gives the rule and the smaller strata. `method` is
#' refused for a window.
#'
#' An occasion of a master whose later stages are still drawn one period or
#' cohort at a time is exported the same way, from its own rows. The
#' two-phase export reads the whole master and refuses it
#' (`samplyr_error_occasion_provisional`).
#'
#' [as_svrepdesign()] replicates the same design, and [as_survey_design()]
#' follows `as_svydesign()`. `pps` is refused (`samplyr_error_survey_argument`),
#' and so is an occasion holding a single primary unit
#' (`samplyr_error_occasion_one_unit`). An occasion that activates every unit
#' is the master's own design and is exported as such.
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
#' and remains supported. A later stage drawn from a modified sample is
#' refused too, since its weights carry the change, and so is every stage
#' drawn after it.
#'
#' A second phase drawn from a modified sample is refused by both exports
#' as well (`samplyr_error_modified_sample`). They read the phase-1 rows as
#' the whole first phase, and removed rows would give it other
#' probabilities and another variance. Random groups of replicates read
#' only the replicate totals and remain. To subsample the eligible units of
#' phase 1, draw phase 2 from phase 1 as executed with the eligibility
#' column as a phase-2 stratum, then subset the exported design.
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
#' designs are refused are in [variance_estimation].
#'
#' `pps` is read as follows. `"brewer"` is the default treatment and keeps
#' every stage. `FALSE` states that no stage is PPS, so it is refused on a
#' design with one. `"overton"`, [survey::HR()] and the matrix objects
#' ([survey::ppsmat()], [survey::poisson_sampling()]) are single-stage in
#' survey, so a multi-stage sample is exported at stage 1 with a warning.
#' They need stage 1 drawn with unequal probabilities without replacement,
#' or by Poisson sampling with a size measure. `"overton"` and
#' [survey::HR()] assume a fixed sample size, so they are refused on a
#' Poisson stage 1, whose exact variance is the default export.
#' [survey::poisson_sampling()] assumes independent selections, so it is
#' refused on a fixed-size stage 1. A matrix object is indexed by row, so it
#' also needs one row per stage-1 unit, and a sample with several is refused
#' (`samplyr_error_pps_rows_per_psu`). On a fixed-size stage drawn with
#' unequal probabilities without replacement, a [survey::ppsmat()] export
#' defaults to `variance = "YG"` unless `variance` is supplied in `...`,
#' because the Horvitz-Thompson quadratic form can be negative even with an
#' exact joint matrix. Any other `pps` value is refused
#' (`samplyr_error_pps_argument`). A two-phase export takes no `pps`,
#' because [survey::twophase()] does not apply it to the phase-2 variance
#' (`samplyr_error_twophase_phase2_pps`). A phase 2 of one stage drawn with
#' `pps_sampford`, `pps_cps`, `pps_brewer`, `pps_sps` or `pps_pareto` is
#' exported with its joint inclusion probabilities, computed on the phase-1
#' sample, and needs `method = "full"`. Any other unequal-probability,
#' balanced or spatial phase 2 is refused with the same class.
#'
#' ## Ultimate-cluster variance
#'
#' Many survey programs publish sampling errors computed as if the primary
#' units were drawn with replacement within their strata, without a finite
#' population correction. The variance of every later stage is then carried
#' by the primary-unit totals, which is why it is called the ultimate-cluster
#' variance (Hansen, Hurwitz and Madow 1953; Wolter 2007, sec. 2.4). It is
#' the variance to reproduce when a new design is compared with what an
#' earlier round published, and `as_svydesign(sample, stages = 1)` builds
#' it. A materialized occasion and a settled reserve are always exported
#' this way.
#'
#' The export keeps the first stage's strata and units, the zero rows of
#' empty primary units included. A primary unit taken with probability 1, by
#' a certainty rule or because its stratum was taken whole, is no draw. When
#' the next stage drew two units or more inside it, it is a stratum of its
#' own whose next-stage units are the clusters. When nothing below it is
#' random, with no stage below or every unit below taken, it is a census
#' stratum of its own and adds no variance. When the next stage drew a single
#' unit inside it, as one household in a certainty EA, a stratum of its own
#' could not estimate the variance below. It then stays a draw of its
#' stratum, and the spread between that stratum's units stands in for the
#' variance below it (`samplyr_message_stage_certainty_drawn`). A stage drawn
#' with replacement has no such units, even where a unit expects one hit or
#' more. A design built by hand from `as.data.frame(sample)` loses the empty
#' units and reads certainty units as drawn.
#'
#' `stages = 1:k` keeps stages 1 to k - 1 with their corrections and reads
#' stage k the same way. It suits a stage whose variance cannot be
#' estimated, such as one respondent per household: `stages = 1:2` on a
#' design of EAs, households and one person carries the person stage in the
#' household totals, and keeps the first stage's correction, which matters
#' where that stage takes a large share. Exported in full, such a stage
#' leaves each household a stratum of one unit, which the export names
#' (`samplyr_warning_lonely_psu`). The reading leaves out the correction at
#' stage k, so it overstates the variance most where stage k takes a large
#' share of its units. A stratum with a single unit stops survey, as for the
#' full export, unless `options(survey.lonely.psu = )` says how to treat it.
#' So does one below a unit drawn with replacement when survey computes the
#' later stages, under `options(survey.use_rcpp = FALSE)` or a calibration by
#' stage.
#'
#' ## A settled reserve
#'
#' A sample whose reserve is not settled is refused
#' (`samplyr_error_reserve_unsettled`), since its weights describe the whole
#' list. A settled sample is exported by ultimate clusters, with rows added
#' for the units found ineligible and for the eligible nonrespondents of a
#' pool with no respondent. [reserve_lists] gives those rows, the estimates
#' and calibrations they need, and the exports that refuse a settled
#' reserve.
#'
#' ## A shared estimation weight
#'
#' A sample from [share_weights()] carries weights for a population other
#' than the one that was selected, so it is exported as its **source-target
#' contributions**: one row per link, weighted by the recorded coefficient
#' times the source unit's design weight. This gives the generalized weight
#' share total exactly for any link structure (Lavallee 2007). The result has
#' more rows than the transformation returned, but no estimate changes: a
#' total sums the same terms, and a mean's denominator is the estimated
#' target population size either way.
#'
#' A selected source unit linked to no target, such as a dwelling with
#' nobody in the target population, has no contribution. The export keeps
#' it as a zero row, just as it keeps a unit found empty, with weight 0,
#' its source values, and `NA` in the target variables. That keeps every
#' sampled unit in the between-unit and within-unit variance as a zero
#' total. Estimates need `na.rm = TRUE`, and the export warns
#' (`samplyr_warning_export_empty_parent`) when zero rows are added.
#'
#' An unequal-probability or random-size source design is refused here,
#' because its variance comes from a structure indexed by the source
#' sample's rows, which the contribution rows no longer are. Use
#' `as_svrepdesign()`, which replicates the source design and applies the
#' sharing inside every replicate.
#'
#' The `survey` package is required but not imported. It must be
#' installed to use this function.
#'
#' @references
#' Hansen, M. H., Hurwitz, W. N. and Madow, W. G. (1953). *Sample Survey
#' Methods and Theory*. Wiley.
#'
#' Lavallee, P. (2007). *Indirect Sampling*. Springer.
#'
#' Sarndal, C.-E., Swensson, B. and Wretman, J. (1992). *Model
#' Assisted Survey Sampling*. Springer.
#'
#' Wolter, K. M. (2007). *Introduction to Variance Estimation*, 2nd ed.
#' Springer.
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
#' # Half of each EA's households, kept as a second phase. It is exported as
#' # the two-stage design with a take of 4.
#' half <- sampling_design() |>
#'   stratify_by(ea_id) |>
#'   draw(n = 4) |>
#'   execute(sample, seed = 2027)
#' svy_half <- as_svydesign(half)
#'
#' # A joint-probability matrix instead, for a single-stage Sampford sample,
#' # whose matrix is exact. A matrix needs one row per stage-1 unit, and YG is
#' # the Sen-Yates-Grundy form for fixed-size sampling without replacement.
#' sampford <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 5, method = "pps_sampford", mos = households) |>
#'   execute(bfa_eas, seed = 2025)
#' jip <- joint_expectation(sampford, bfa_eas)
#' svy_joint <- as_svydesign(sampford, pps = survey::ppsmat(jip[[1]]), variance = "YG")
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

#' The stages of a sample, and of its earlier phases, that list a reserve
#' the sample has not settled
#' @noRd
unsettled_reserve_stages <- function(x) {
  stages <- get_stages_executed(x)
  design <- get_design(x)
  meta <- attr(x, "metadata")
  listed <- stages[vapply(
    stages,
    function(k) !is_null(design$stages[[k]]$draw_spec$reserve),
    logical(1)
  )]
  if (!is_null(meta$reserve_settlement)) {
    listed <- integer(0)
  }
  prev <- meta$prev_phase$sample
  if (is_tbl_sample(prev)) {
    listed <- c(listed, unsettled_reserve_stages(prev))
  }
  unique(listed)
}

#' Does a sample, or one of its earlier phases, hold a settled reserve?
#' @noRd
holds_settled_reserve <- function(x) {
  meta <- attr(x, "metadata")
  if (!is_null(meta$reserve_settlement)) {
    return(TRUE)
  }
  prev <- meta$prev_phase$sample
  is_tbl_sample(prev) && holds_settled_reserve(prev)
}

#' Refuse to use a list that still holds its reserve
#'
#' `.weight` at a reserve stage is the chance of the whole list, take and
#' reserve together. The weight of an analysis comes from the units the
#' field worked and their outcomes, which an executed list does not record.
#' An export also refuses an occasion of a reserve master, settled or not.
#' @param action What `fn_name` would do with the sample, completing
#'   "cannot ... a sample".
#' @noRd
check_reserve_settled <- function(x, fn_name, action = "export",
                                  call = caller_env()) {
  if (identical(action, "export")) {
    check_reserve_occasion(x, fn_name, call = call)
  }
  stages <- unsettled_reserve_stages(x)
  if (length(stages) == 0L) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn {fn_name}} cannot {action} a sample whose reserve is not
       settled.",
      "x" = "{cli::qty(length(stages))}Stage{?s} {stages}
             {cli::qty(length(stages))}list{?s/} a take and a reserve, and
             {.field .weight} there is the chance of the whole list.",
      "i" = "Settle it with {.code execute(sample, response_status = col)},
             which weights the units the field worked from their codes."
    ),
    class = "samplyr_error_reserve_unsettled",
    call = call
  )
}

#' Refuse an occasion of a master that draws a reserve, settled or not
#'
#' An occasion holds no settlement of its own, and a settled reserve exports only
#' from the sample that settled it.
#' @noRd
check_reserve_occasion <- function(x, fn_name, call = caller_env()) {
  if (is_null(attr(x, "metadata")[["occasion"]])) {
    return(invisible(NULL))
  }
  design <- get_design(x)
  stages <- Filter(function(k) {
    !is_null(design$stages[[k]]$draw_spec$reserve)
  }, get_stages_executed(x))
  if (length(stages) == 0L) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn {fn_name}} cannot export an occasion of a master that draws a
       reserve.",
      "x" = "{cli::qty(length(stages))}Stage{?s} {stages}
             {cli::qty(length(stages))}list{?s/} a take and a reserve.",
      "i" = "A reserve is exported by ultimate clusters from the sample that
             settled it. Settle the master with
             {.code execute(master, response_status = col)} and export it
             as a whole."
    ),
    class = "samplyr_error_reserve_full_export",
    call = call
  )
}

#' Refuse an export route that would write the settled stage's correction
#'
#' A settled reserve exports by ultimate clusters from the single phase
#' that settled it. Two phases, an occasion, shared weights and a subsample
#' composed with its parent each build the stages one by one.
#' @noRd
check_reserve_export_route <- function(x, fn_name, call = caller_env()) {
  meta <- attr(x, "metadata")
  reason <- if (is_null(meta$reserve_settlement)) {
    "Its reserve was settled in an earlier phase."
  } else if (is_tbl_sample(meta$prev_phase$sample)) {
    "It is the second phase of a two-phase sample."
  } else if (!is_null(meta[["occasion"]])) {
    "It is an occasion of a master."
  } else if (identical(sample_weight_contract(x), "shared")) {
    "Its weights are shared from another sample."
  }
  if (is_null(reason)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn {fn_name}} exports a settled reserve by ultimate clusters, from
       the sample that settled it.",
      "x" = reason,
      "i" = "The settled stage's finite population correction is unknown,
             since its respondents are a random subset of a respondent count
             the field never observed. An export that builds the stages one
             by one would need it."
    ),
    class = "samplyr_error_reserve_full_export",
    call = call
  )
}

#' Labels for the pools of a settlement, as cli-safe text
#' @noRd
reserve_pool_labels <- function(keys, which) {
  labels <- if (ncol(keys) > 0L) {
    do.call(paste, c(
      lapply(names(keys), function(v) paste0(v, " = ", keys[[v]][which])),
      sep = ", "
    ))
  } else {
    rep("the whole frame", sum(which))
  }
  escape <- function(x) gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE),
                             fixed = TRUE)
  shown <- escape(utils::head(labels, 5L))
  more <- length(labels) - length(shown)
  c(
    stats::setNames(shown, rep(" ", length(shown))),
    if (more > 0L) {
      c(" " = paste0("and ", more, " more pool", if (more > 1L) "s"))
    }
  )
}

#' Pools with eligible units and no respondent, warned or refused
#'
#' Their weight goes to rows for their eligible nonrespondents, which a
#' nonresponse adjustment can move to respondents. Below the reserve stage those rows would need the
#' weights of units the field never reached, so a sample that drew a stage
#' after settling is refused instead.
#' @noRd
check_reserve_respondents <- function(pools, record, stages, fn_name,
                                      call = caller_env()) {
  empty <- pools$no_respondent
  if (!any(empty)) {
    return(invisible(NULL))
  }
  shown <- reserve_pool_labels(record$keys, empty)
  n <- sum(empty)
  if (!identical(as.integer(record$stage), as.integer(max(stages)))) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} cannot export a pool with no respondent once a
         stage below its reserve is drawn.",
        "x" = "{cli::qty(n)}{?This pool holds/These pools hold} eligible
               units or units of unknown eligibility, and no interview.",
        shown,
        "i" = "Its nonrespondents' rows would carry a count of stage
               {record$stage} units into a design weighted in units of
               stage {max(stages)}, which the field never reached there.",
        "i" = "Export the settled sample before drawing the next stage, or
               combine that stratum with a neighbor in the design."
      ),
      class = "samplyr_error_reserve_no_respondent",
      call = call
    )
  }
  cli::cli_warn(
    c(
      "{n} pool{?s} ha{?s/ve} eligible units and no respondent.",
      shown,
      "i" = "Each is exported as one row per eligible unit that did not
             respond, coded as an eligible non-interview, which together
             carry the pool's estimated eligible weight. Their analysis
             variables hold {.code NA}, so estimates need
             {.code na.rm = TRUE}.",
      "i" = "Estimates leave that weight out until a nonresponse adjustment
             or a calibration moves it to respondents, such as
             {.fn svrep::redistribute_weights} on a replicate design or
             {.fn survey::calibrate}."
    ),
    class = "samplyr_warning_reserve_no_respondent",
    call = call
  )
  invisible(NULL)
}

#' The columns that name a primary unit, each of its draws apart
#' @noRd
reserve_primary_vars <- function(design, stage, data) {
  vars <- design$stages[[stage]]$clusters$vars %||% character(0)
  if (length(vars) == 0L) {
    return(vars)
  }
  intersect(c(vars, paste0(".draw_", stage)), names(data))
}

#' Rows that stand for the attempted units a settlement drops
#'
#' Each pool adds one row per dropped unit its estimate counts, from the
#' units the settlement records, at the weight of the stages above times
#' `N / (k - s - u)`, so its weights sum to `N` at every stage. A pool with
#' respondents adds its ineligible units, since its respondents carry its
#' eligible nonrespondents. A pool with no respondent adds its ineligible
#' units and its eligible nonrespondents, which then carry its estimated
#' eligible count, coded as non-interviews. A pool whose attempted units are
#' all of unknown eligibility shares its estimate among them: as eligible
#' nonrespondents when the estimate is positive, as ineligible units when it
#' is zero. `unit_col` names the column that gives each row's recorded unit,
#' a name free in `df`.
#' @noRd
reserve_unit_rows <- function(df, record, unit_col) {
  pools <- record$pools
  units <- record$units
  # One row per unit, the first of a unit drawn as a cluster.
  first <- !duplicated(units$unit)
  pool <- units$pool[first]
  code <- units$code[first]
  resolved <- (pools$n_attempted - pools$stopped - pools$n_unknown)[pool]
  none <- pools$n_respondents[pool] == 0L
  unknown_only <- resolved == 0L
  positive <- pools$eligible[pool] > 0
  eligible <- none & ifelse(unknown_only, code == 3L & positive, code == 2L)
  ineligible <- ifelse(unknown_only, none & code == 3L & !positive,
                       code == 4L)
  upper <- pools$upper_weight[pool]
  weight <- ifelse(
    unknown_only,
    upper * ifelse(positive, pools$eligible[pool], pools$N[pool]) /
      pools$n_attempted[pool],
    upper * pools$N[pool] / resolved
  )
  column <- record$column
  unit_row <- which(first)
  block <- function(keep, code_value, word) {
    out <- reserve_key_rows(df, record, pool[keep])
    out$.weight <- weight[keep]
    # The recorded unit, read by a frame stack and dropped from the design.
    out[[unit_col]] <- unit_row[keep]
    if (!is_null(column) && column %in% names(df)) {
      out[[column]] <- reserve_category_value(
        df[[column]], sum(keep), code_value, word
      )
    }
    out
  }
  vctrs::vec_rbind(
    block(ineligible, 4, "ineligible"),
    block(eligible, 2, "noninterview")
  )
}

#' Rows that carry the keys of the given pools, with their certainty
#' @noRd
reserve_key_rows <- function(df, record, rows) {
  out <- vctrs::vec_init(df, length(rows))
  keys <- record$keys
  for (v in intersect(names(keys), names(df))) {
    out[[v]] <- vctrs::vec_cast(keys[[v]][rows], df[[v]])
  }
  certain <- record$pools$certainty
  if (!is_null(certain)) {
    if (".certainty_1" %in% names(df)) {
      out$.certainty_1 <- certain[rows]
    }
    # A census parent records its certainty as a probability of 1.
    if (".weight_1" %in% names(df)) {
      out$.weight_1[certain[rows]] <- 1
    }
  }
  out
}

#' A category in the type of the user's response status column
#' @noRd
reserve_category_value <- function(x, n, code, word) {
  if (is.factor(x)) {
    levels <- union(levels(x), word)
    return(factor(rep(word, n), levels = levels))
  }
  if (is.numeric(x)) {
    return(vctrs::vec_cast(rep(code, n), x))
  }
  rep(word, n)
}

#' Export a settled reserve by ultimate clusters
#'
#' The first stage's units are the clusters, its stratification variables
#' the strata, with no finite population correction. Every later stage,
#' the settled one included, is carried in the unit totals. A first-stage
#' reserve adds its ineligible units as rows, so the eligible count it
#' estimates enters the variance. A unit taken with certainty is a stratum
#' of its own, as in an occasion, and a reserve at the stage below it is
#' exported as a first-stage reserve of that stratum.
#' @noRd
build_reserve_svydesign <- function(x, nest, dots, fn_name,
                                    check_lonely = TRUE,
                                    call = caller_env()) {
  rlang::local_error_call(call)
  design <- get_design(x)
  stages <- get_stages_executed(x)
  record <- attr(x, "metadata")$reserve_settlement
  df <- export_rows(x, design, stages, call = call)
  spec <- export_stage_spec(df, design, stages[1])
  entry <- spec$stage[[1]]
  if (length(stages) > 1L && identical(entry$unit$kind, "element")) {
    abort_survey_midstage_element(stages[1], call = call)
  }
  check_reserve_respondents(record$pools, record, stages, fn_name,
                            call = call)
  # A free name, since a user column may be called `.reserve_unit`.
  unit_col <- free_column_name(df, ".reserve_unit")
  added <- reserve_unit_rows(df, record, unit_col)
  if (nrow(added) > 0L) {
    # At the first stage each added row is a unit of its own.
    fresh <- max(c(entry$unit$id, 0L)) + seq_len(nrow(added))
    # Each draw of a primary unit drawn with replacement is a unit apart.
    unit_vars <- reserve_primary_vars(design, stages[1], df)
    # Below the first stage an added row joins its primary unit, if exported.
    if (record$stage > stages[1] && length(unit_vars) > 0L) {
      at <- vctrs::vec_match(added[unit_vars], df[unit_vars])
      fresh[!is.na(at)] <- entry$unit$id[at[!is.na(at)]]
      absent <- is.na(at)
      fresh[absent] <- max(c(entry$unit$id, 0L)) +
        vctrs::vec_group_id(added[absent, unit_vars, drop = FALSE])
    }
    entry$unit$id <- c(entry$unit$id, fresh)
    df <- vctrs::vec_rbind(df, added)
  }
  spec$n_rows <- nrow(df)
  vars <- design$stages[[stages[1]]]$strata$vars %||% character(0)
  sure <- self_represent(df, entry, vars, design, stages,
                         flag_only_stage = record$stage)
  df <- sure$df
  entry <- sure$entry
  vars <- sure$vars
  entry$strata <- list(
    user = vars,
    certainty = NULL,
    id = if (length(vars) > 0L) group_ids(df, vars)
  )
  entry$prob <- NULL
  spec$stage[[1]] <- entry

  id_info <- spec_survey_ids(spec, df, call = call)
  df <- id_info$df
  strata <- spec_survey_strata(
    spec,
    df,
    id_stage_indices = id_info$stage_indices
  )
  df <- strata$df
  if (check_lonely) {
    warn_lonely_strata(spec, design, id_info$stage_indices, call = call)
  }
  reserve_units <- df[[unit_col]]
  df[[unit_col]] <- NULL

  args <- c(
    list(
      ids = survey_ids_formula(id_info$id_vars),
      strata = strata$formula,
      weights = stats::as.formula("~.weight"),
      data = df,
      nest = nest
    ),
    dots
  )
  result <- survey_svydesign(args)
  # Which recorded unit each added row stands for, `NA` on the sample's.
  attr(result, "samplyr_reserve_units") <- reserve_units
  result
}

#' @noRd
survey_phase_info <- function(sample) {
  prev_phase <- get_prev_phase(sample)
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
           master if the second phase is an occasion activation."
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

  check_occasion_carries_master(sample, phase_info, fn_name, call = call)
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
#' The three systematic methods: one systematic sample does not identify its
#' design variance, whatever formula stands in for it. `pps_systematic` is
#' exported with Brewer's approximation, which is as blind to frame order as
#' the simple random sampling one: on a frame whose period matches the
#' interval it gave 0.003 of the true variance, with intervals covering 9 %
#' of the time. `pps_systematic_mr` is exported with replacement, and on
#' such a frame every hit of a sample can carry the same value per unit of
#' size, which gives an estimated variance of zero. A census stage is
#' excluded, since a stage that took everything within reach contributes no
#' variance for the approximation to get wrong. So is a stage whose every
#' pool drew one unit, since that draw does not depend on the frame order.
#' @noRd
systematic_approximated_stages <- function(design, stages_executed, df,
                                           phase = NULL) {
  affected <- vapply(stages_executed, function(stage_idx) {
    method <- design$stages[[stage_idx]]$draw_spec$method
    if (!method %in% systematic_methods) {
      return(FALSE)
    }
    # A systematic draw of one PSU is a draw proportional to size.
    if (draws_one_per_zone(design$stages[[stage_idx]]$draw_spec)) {
      return(FALSE)
    }
    # A census has no variance to approximate. Minimum replacement can take
    # a unit more than once, so a probability of 1 or more is no census.
    if (!is_multi_hit_method(design$stages[[stage_idx]]$draw_spec) &&
        spec_stage_census(df, stage_idx)) {
      return(FALSE)
    }
    !systematic_takes_one_per_pool(design, stage_idx, df)
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

#' Does every pool of a systematic stage hold one drawn unit?
#'
#' A single random start takes unit i with chance x_i / X whatever the
#' order, which is a draw of one proportional to size, so the frame order
#' plays no part. Pools taken whole, such as the stratum of certainty units,
#' are left out.
#' @noRd
systematic_takes_one_per_pool <- function(design, stage_idx, df) {
  spec <- export_stage_spec(df, design, seq_len(stage_idx))
  pools <- spec_pool_units(spec, seq_len(stage_idx))[[as.character(stage_idx)]]
  drawn <- pools$n_units[!pools$census]
  length(drawn) > 0L && all(drawn == 1L)
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
  approximation = c("srswor", "generic_replicates", "ultimate_cluster"),
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

  mr_note <- if (any(vapply(stages, function(s) {
    identical(s$method, "pps_systematic_mr")
  }, logical(1)))) {
    c("i" = "On such a frame the with-replacement variance of
             {.val pps_systematic_mr} can be exactly zero.")
  }
  estimator <- if (approximation %in% c("srswor", "ultimate_cluster")) {
    per_stage <- vapply(stages, function(s) {
      what <- if (identical(approximation, "ultimate_cluster") ||
                    identical(s$method, "pps_systematic_mr")) {
        "a with-replacement approximation"
      } else if (identical(s$method, "pps_systematic")) {
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
             {.topic variance_estimation}.",
      mr_note
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
             {.topic variance_estimation}."
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
    } else if (identical(approximation, "srswor") &&
               identical(s$method, "pps_systematic_mr")) {
      "with_replacement"
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

## Export of a materialized occasion

# An occasion is a two-phase sample. Its activation identifiers, strata, and counts
# come from the frozen assignment. Each block activates its quota by SRSWOR.

#' @noRd
survey_is_activation <- function(phase_info) {
  identical(phase_info$prev_phase$transition, "panel_activation")
}

#' An occasion must carry the master it was activated from
#'
#' [survey::twophase()] builds the first-phase design from every phase-1 row
#' and marks the active ones with `subset`, so the master's rows are needed
#' and nothing can reconstruct the units the occasion did not keep. Every export
#' route checks it through `survey_validate_phase_support()`: without the
#' link, a replicate route would read the occasion as a single-phase sample and
#' leave the activation out of the variance.
#' @noRd
check_occasion_carries_master <- function(
  x,
  phase_info,
  fn_name,
  call = caller_env()
) {
  metadata <- attr(x, "metadata")
  if (is_null(metadata$occasion) || survey_is_activation(phase_info)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn {fn_name}} needs the master this occasion was activated from.",
      "x" = "This sample realizes {sample_occasion_phrase(metadata)} but
             carries no link to it.",
      "i" = "The master is the first phase of the export, so materialize the
             occasion again from it:
             {.code execute(master, {occasion_code(
               metadata$panel_assignment$schedule, metadata$occasion$occasion)})}."
    ),
    class = "samplyr_error_occasion_no_master",
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
        "Cannot export an occasion of a master drawn with
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
      "Cannot export an occasion of a master drawn with
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
    class = "samplyr_error_occasion_phase1_pps",
    call = call
  )
}

#' The phase-1 sample must be the realization the activation was computed on
#'
#' Two executions of one design produce the same `.sample_id` values, because
#' those are row positions, so a substituted master of the same shape would
#' pass every structural check while supplying different non-active rows.
#' @noRd
check_occasion_master_identity <- function(metadata, master, call = caller_env()) {
  recorded <- metadata$occasion$master_digest
  if (
    is_null(recorded) || identical(recorded, occasion_source_digest(master))
  ) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "This occasion was activated from a different realization.",
      "x" = "The sample held as its first phase is not the one occasion
             {metadata$occasion$occasion} was computed against.",
      "i" = "Materialize the occasion again from the master it belongs to."
    ),
    class = "samplyr_error_occasion_master_mismatch",
    call = call
  )
}

#' Phase-2 columns of an activation, from the frozen assignment record
#'
#' The block a unit sits in, the block's size in assignment units, and the
#' conditional probability the recorded quotas give it. `active` comes from
#' the occasion's own rows rather than from recomputing the panel test, so the
#' exported subset is the sample the user holds.
#' @noRd
activation_phase2_columns <- function(df1, x, metadata, call = caller_env()) {
  # Validate the recorded assignment law before deriving design columns.
  record <- prepare_panel_record(
    metadata$panel_assignment,
    "A survey export",
    call = call
  )
  occasion_pools <- metadata$occasion$pools

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
  located <- record_pool_rows(keys, record$pools)

  for (p in seq_along(record$pools)) {
    pool <- record$pools[[p]]
    rows <- located[[p]]$rows
    b <- rep(seq_along(pool$blocks), pool$blocks)[located[[p]]$at]
    block[rows] <- paste(p, b, sep = ".")
    block_n[rows] <- pool$blocks[b]
    prob[rows] <- occasion_pools[[p]]$probability[b]
  }

  if (anyNA(block)) {
    abort_samplyr(
      c(
        "The assignment record does not cover every row of the master.",
        "x" = "{sum(is.na(block))} row{?s} belong{?s/} to no assignment
               pool.",
        "i" = "Materialize the occasion again from the master."
      ),
      class = "samplyr_error_occasion_master_mismatch",
      call = call
    )
  }

  df1[[cols$unit]] <- group_ids(df1, record$key_vars)
  df1[[cols$block]] <- block
  df1[[cols$block_n]] <- block_n
  # A zero row is active when its panel is, or its pool is permanent.
  zero <- is.na(df1$.sample_id)
  permanent <- rep(FALSE, nrow(df1))
  for (p in seq_along(record$pools)) {
    if (identical(record$pools[[p]]$activation, "permanent")) {
      permanent[located[[p]]$rows] <- TRUE
    }
  }
  df1[[cols$active]] <- ifelse(
    zero,
    df1$.panel %in% metadata$occasion$active_panels | permanent,
    df1$.sample_id %in% x$.sample_id
  )
  df1[[cols$prob]] <- ifelse(df1[[cols$active]], prob, NA_real_)
  df1[[cols$weight]] <- ifelse(df1[[cols$active]], 1 / prob, NA_real_)
  df1[[cols$prob1]] <- 1 / df1$.weight

  check_activation_take(df1, cols, call = call)

  list(df = df1, cols = cols)
}

#' The realized take must be the take the record froze
#'
#' Counting the active assignment units of each block reproduces `a_bt`, so an
#' occasion whose rows do not belong to the master it names is caught here rather
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
        "i" = "This occasion and the master it carries describe different
               executions. Materialize it again from the master."
      ),
      class = "samplyr_error_occasion_master_mismatch",
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
#' identifier there), and it does not when a term is infinite: a later
#' unsupported stage on the count scale, and an equal-probability stage with
#' no recorded population count. In both cases the phase's own weight is the
#' exact probability and is passed instead. A with-replacement stage states
#' it only when it took nothing but certainty units, which
#' `survey_fpc_info()` writes as a sampling fraction of one. Otherwise its
#' term is infinite on the count scale, and on the fraction scale a zero
#' for each random hit, which survey reads as a probability of zero and
#' cannot derive a weight from.
#' @param scale The phase's FPC scale from `survey_fpc_info()`.
#' @param certain_wr The with-replacement stages at which the phase took only
#'   certainty units, from `phase_wr_stages_certain()`.
#' @noRd
spec_fpc_states_probabilities <- function(spec, id_stage_indices, scale,
                                          certain_wr = integer(0)) {
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
      wr = stage_idx %in% certain_wr,
      unsupported = stage_idx == spec$stages[1] || !on_count,
      equal_wor = !on_count || !is_null(entry$pop_count),
      TRUE
    )
  }, logical(1)))
}

#' The stages drawn with replacement at which a phase took only certainty units
#'
#' Such a stage took each of its units once and for sure, so it is a census.
#' Only a recorded certainty counts: expected hits of one, a missing flag or
#' a take of every unit leave room for a random draw. The rows alone do not
#' show it, since a unit selected at random can lose every row to an empty
#' stage below. So every unit the phase selected is read, from its
#' execution record when it has no row, and a record that keeps no row
#' proves nothing. A sample changed after execution proves nothing either,
#' since its rows need not be every unit the phase selected.
#' @param x The phase's sample.
#' @noRd
phase_wr_stages_certain <- function(x, design, stages) {
  if (!sample_realization_status(x)$ok) {
    return(integer(0))
  }
  all_certain <- function(df, k) {
    flag <- df[[paste0(".certainty_", k)]]
    nrow(df) > 0L && !is_null(flag) && all(flag %in% TRUE)
  }
  rows <- as.data.frame(x)
  wr <- Filter(function(k) {
    identical(survey_stage_kind(design$stages[[k]]$draw_spec), "wr") &&
      all_certain(rows, k)
  }, stages)
  if (length(wr) == 0L) {
    return(integer(0))
  }
  every <- sample_every_unit_rows(x, design, stages)
  if (is_null(every)) {
    return(integer(0))
  }
  as.integer(Filter(function(k) all_certain(every, k), wr))
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
      "i" = "{.code method = \"simple\"} and {.code \"approx\"} use the
             design weights directly. They suit a phase 2 that takes whole
             phase-1 units or rows, and overstate the variance when phase 2
             subsamples inside the units phase 1 selected
             ({.code samplyr_warning_twophase_approx})."
    ),
    class = "samplyr_error_twophase_stage_probs",
    call = call
  )
}

#' Does the activation retain every unit with certainty?
#'
#' Read from the realized per-block activation probabilities rather than from
#' the pool classes, so it covers every route to an identity: all pools
#' permanent, and an occasion that activates every panel. A record with no pools
#' states nothing and is not treated as an identity.
#' @noRd
activation_is_identity <- function(metadata) {
  pools <- metadata$occasion$pools
  if (is_null(pools) || length(pools) == 0L) {
    return(FALSE)
  }
  probability <- unlist(lapply(pools, function(pool) pool$probability))
  length(probability) > 0L && all(is_certainty_probability(probability))
}

#' Export a materialized occasion through survey::twophase()
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

  check_occasion_master_identity(metadata, master, call = call)
  # A unit the master lost is a zero row of phase 1.
  df1 <- export_rows(master, design1, stages1, call = call)

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
  survey_twophase(args, call = call)
}

#' Rows of the stage-1 units taken whole
#'
#' A recorded certainty unit, or a unit of a stage drawn without replacement
#' whose probability is 1, as a census stratum gives. A with-replacement
#' stage's expected hits never count. Neither does the weight of a stage
#' that does not give its own probability (`flag_only_stage`). A reserve
#' settlement rewrites that stage's weights, and an occasion's panels rotate
#' the units of the stage they are assigned at, so a unit of a census is in
#' some occasions only. There the recorded flag alone counts. Rows added for
#' a reserve carry their parent's certainty in the same two columns.
#' @noRd
stage1_sure_rows <- function(df, design, stages, flag_only_stage = NULL) {
  first <- stages[1]
  flag <- df[[paste0(".certainty_", first)]]
  if (!is_null(flag_only_stage) &&
        as.integer(first) %in% as.integer(flag_only_stage)) {
    return((flag %||% rep(FALSE, nrow(df))) %in% TRUE)
  }
  entry <- list(prob = spec_stage_prob(df, first), certainty = flag)
  export_stage_sure(entry, design$stages[[first]]$draw_spec, nrow(df)) %in%
    TRUE
}

#' Rows of the stage-1 units taken whole with nothing drawn inside
#'
#' Clusters with no later stage, and elements. Each is the same in every
#' sample, so an export by ultimate clusters makes it a census stratum.
#' @noRd
census_rows <- function(df, design, stages, entry, flag_only_stage = NULL) {
  stage1_sure_rows(df, design, stages, flag_only_stage) &
    !self_representing_rows(df, design, stages, entry, flag_only_stage)
}

#' Rows of the stage-1 clusters an export treats as self-representing
#'
#' Clusters taken whole, when a later stage was drawn inside them.
#' @noRd
self_representing_rows <- function(df, design, stages, entry,
                                   flag_only_stage = NULL) {
  if (length(stages) < 2L || !identical(entry$unit$kind, "cluster")) {
    return(rep(FALSE, nrow(df)))
  }
  stage1_sure_rows(df, design, stages, flag_only_stage)
}

#' Make the self-representing stage-1 clusters strata of their own
#'
#' Their units of the next stage become the clusters. A row that names no
#' such unit, as a row added for a reserve pool, is a unit apart.
#' @return list(df, entry, vars, sure), with the stratum column added to
#'   `vars` and `sure` flagging the self-representing rows.
#' @noRd
self_represent <- function(df, entry, vars, design, stages,
                           flag_only_stage = NULL) {
  sure <- self_representing_rows(df, design, stages, entry, flag_only_stage)
  if (!any(sure)) {
    return(list(df = df, entry = entry, vars = vars, sure = sure))
  }
  later <- reserve_primary_vars(design, stages[2], df)
  below <- if (length(later) > 0L) df[later] else df[".sample_id"]
  below <- vctrs::vec_slice(below, sure)
  lone <- vctrs::vec_detect_missing(below)
  below$.lone <- ifelse(lone, seq_along(lone), 0L)
  below$.unit <- entry$unit$id[sure]
  sure_col <- free_name(names(df), ".self_representing")
  df[[sure_col]] <- ifelse(sure, entry$unit$id, 0L)
  entry$unit$id[sure] <- max(entry$unit$id) + vctrs::vec_group_id(below)
  list(df = df, entry = entry, vars = c(vars, sure_col), sure = sure)
}

#' The occasions of a window each primary unit is active at
#'
#' A unit's occasions are fixed by its panel, and a stratum's quotas fix how
#' many of its units share each set, so the sets are second-phase strata. A
#' pool whose occasions a period reads together counts that period as one
#' occasion, since its weights do not fix each occasion's total.
#' @param unit The primary unit of each row.
#' @return A character vector, one per row.
#' @noRd
window_occasion_patterns <- function(df, unit, metadata) {
  label <- paste(df$.period, df$.subround)
  record <- metadata$panel_assignment
  if (length(metadata$occasion$collapsed) > 0L) {
    pairs <- grid_pairs(record$schedule)
    located <- record_pool_rows(
      make_group_key(df, record$key_vars),
      record$pools
    )
    for (part in metadata$occasion$collapsed) {
      at <- paste(pairs$period[part$occasions], pairs$subround[part$occasions])
      pooled <- unlist(lapply(located[part$pools], function(l) l$rows))
      rows <- intersect(which(label %in% at), pooled)
      label[rows] <- paste("period", pairs$period[part$occasions[[1L]]])
    }
  }
  pattern <- tapply(label, unit, function(l) {
    paste(sort(unique(l)), collapse = ", ")
  })
  unname(pattern[as.character(unit)])
}

#' The variance strata of a window's primary units
#'
#' The activation is a second phase whose strata are the quota blocks, and
#' within a block the quotas fix how many units share each set of occasions.
#' A primary unit's cell is its pool, block and set of occasions. A cell with
#' one unit is merged with the cell of the same occasions in the next block
#' of its pool, the adjacent strata of the collapsed-strata estimator. A cell
#' still holding one unit falls back to the number of its occasions, which
#' keeps its weight multiplier. Where a primary unit spans several blocks, as
#' a retained primary unit does, the cell is its set of occasions only.
#' @param unit The primary unit of each row.
#' @return A character vector, one per row, crossed with the design's strata
#'   by the caller, with attribute `fallback` flagging the rows of units that
#'   fell back.
#' @noRd
window_variance_cells <- function(df, unit, metadata) {
  pattern <- window_occasion_patterns(df, unit, metadata)
  occasion <- paste(df$.period, df$.subround)
  visits <- stats::ave(occasion, unit, FUN = function(o) length(unique(o)))
  record <- metadata$panel_assignment
  located <- record_pool_rows(make_group_key(df, record$key_vars), record$pools)
  pool <- block <- rep(NA_integer_, nrow(df))
  for (p in seq_along(record$pools)) {
    rows <- located[[p]]$rows
    of_unit <- rep(seq_along(record$pools[[p]]$blocks), record$pools[[p]]$blocks)
    pool[rows] <- p
    block[rows] <- of_unit[located[[p]]$at]
  }
  spans <- tapply(paste(pool, block), unit, function(b) length(unique(b)))
  cell <- if (any(spans > 1L)) {
    pattern
  } else {
    unit_rows <- !duplicated(unit)
    group <- block
    for (key in unique(paste(pool, pattern)[unit_rows])) {
      at <- which(unit_rows & paste(pool, pattern) == key)
      sizes <- table(block[at])
      merged <- adjacent_groups(as.vector(sizes))
      group[at] <- merged[match(block[at], as.integer(names(sizes)))]
    }
    group <- group[which(unit_rows)][match(unit, unit[unit_rows])]
    paste(pool, group, pattern, sep = "|")
  }
  size <- stats::ave(seq_along(unit), cell, FUN = function(i) {
    length(unique(unit[i]))
  })
  lone <- size == 1L
  cell[lone] <- paste("visits", visits[lone])
  attr(cell, "fallback") <- lone
  cell
}

#' Join consecutive blocks until each group holds two units
#'
#' A short last group joins the one before it.
#' @param sizes The units of each block, in block order.
#' @return The group of each block.
#' @noRd
adjacent_groups <- function(sizes) {
  group <- integer(length(sizes))
  g <- 1L
  count <- 0L
  for (i in seq_along(sizes)) {
    group[[i]] <- g
    count <- count + sizes[[i]]
    if (count >= 2L && i < length(sizes)) {
      g <- g + 1L
      count <- 0L
    }
  }
  if (count < 2L && g > 1L) {
    group[group == g] <- g - 1L
  }
  group
}

#' Pool the primary units a window's split leaves alone in their stratum
#'
#' A unit alone in its stratum, in a design stratum that holds others, is
#' there because of the window's split. Such units share one stratum, across
#' the design's strata, which with their shared ratio column is the
#' ratio-adjusted collapsed-strata estimator for them. A single such unit
#' joins the stratum of its design stratum whose units are active at the
#' nearest number of occasions, the one with more units on a tie. Units with
#' certainty or a census stratum are not pooled, nor joined.
#' @param drawn Whether each row's unit was drawn at random.
#' @return The new stratum label of each row, or `NULL` when nothing is
#'   pooled.
#' @noRd
pool_window_lone <- function(df, vars, design_vars, unit, drawn) {
  stratum <- group_ids(df, vars)
  design_stratum <- if (length(design_vars) > 0L) {
    group_ids(df, design_vars)
  } else {
    rep(1L, nrow(df))
  }
  count_units <- function(by) {
    n <- tapply(unit, by, function(u) length(unique(u)))
    as.vector(n[as.character(by)])
  }
  lone <- drawn & count_units(stratum) == 1L & count_units(design_stratum) >= 2L
  label <- paste("stratum", stratum)
  if (length(unique(unit[lone])) >= 2L) {
    return(ifelse(lone, "lone", label))
  }
  if (!any(lone)) {
    return(NULL)
  }
  occasion <- paste(df$.period, df$.subround)
  visits <- stats::ave(seq_along(unit), unit, FUN = function(i) {
    length(unique(occasion[i]))
  })
  other <- drawn & !lone & design_stratum == design_stratum[lone][[1L]]
  if (!any(other)) {
    return(NULL)
  }
  near <- tapply(abs(visits[other] - visits[lone][[1L]]), stratum[other], min)
  size <- tapply(unit[other], stratum[other], function(u) length(unique(u)))
  # tapply orders by stratum id, which breaks the remaining ties.
  join <- names(near)[order(near, -size)][[1L]]
  label[lone] <- paste("stratum", join)
  label
}

#' The calibration that gives a window's merged strata ratio residuals
#'
#' A primary unit's window total has expectation m_i beta in its stratum,
#' where m_i is its activation multiplier, the sum of its occasion factors
#' over the number of occasions. Calibrating on each stratum's total of an
#' auxiliary that sums to m_i over the unit's rows, with variance
#' proportional to it, leaves the weights unchanged and makes the variance
#' use u_i - m_i ubar / mbar, the ratio-adjusted collapsed-strata estimator
#' (Hansen, Hurwitz and Madow 1953, Wolter 2007, section 2.5). For a row the
#' auxiliary is one over its master weight times the unit's rows at that
#' occasion. A stratum of one unit takes no column of its own, which would
#' fit it exactly. Such strata share one, so `survey.lonely.psu` reads them
#' as residuals like the others.
#' @param weights `.sample_id` and `.weight` of the rows before activation:
#'   the master's, or on a master drawn occasion by occasion, the window's
#'   sources.
#' @param cells The window cell of each row (`empty_unit_stand_in()`).
#' @return `NULL` when no stratum needs it, otherwise the columns, formula,
#'   population totals and variances for [survey::calibrate()], and
#'   `stand_in`, the weights of empty units' rows during the calibration.
#' @noRd
window_ratio_spec <- function(svy, weights, cells = NULL) {
  df <- svy$variables
  master_weight <- weights$.weight[match(df$.sample_id, weights$.sample_id)]
  unit <- as.character(svy$cluster[, 1])
  stratum <- as.character(svy$strata[, 1])
  occasion <- paste(df$.period, df$.subround)
  rows_at <- stats::ave(rep(1, nrow(df)), unit, occasion, FUN = sum)
  known <- !is.na(master_weight) & master_weight > 0
  aux <- ifelse(known, 1 / (master_weight * rows_at), 0)
  units_in <- tapply(unit, stratum, function(u) length(unique(u)))
  alone <- as.vector(units_in[stratum]) < 2L
  group <- ifelse(alone, "\ralone", stratum)
  if (length(unique(unit[alone])) < 2L) {
    group[alone] <- NA_character_
  }
  stand_in <- empty_unit_stand_in(
    aux, stats::weights(svy), unit, known, group, cells %||% group
  )
  aux <- stand_in$aux
  groups <- unique(stats::na.omit(group))
  columns <- vapply(groups, function(g) {
    ifelse(!is.na(group) & group == g, aux, 0)
  }, numeric(nrow(df)))
  columns <- columns[, colSums(abs(columns)) > 0, drop = FALSE]
  # Every unit in its own stratum or no stratum to adjust: nothing to do.
  if (ncol(columns) == 0L) {
    return(NULL)
  }
  prefix <- ".ratio"
  repeat {
    names <- paste0(prefix, "_", seq_len(ncol(columns)))
    if (!any(names %in% names(df))) break
    prefix <- paste0(prefix, "_")
  }
  colnames(columns) <- names
  list(
    columns = columns,
    formula = stats::as.formula(paste("~ 0 +", paste(names, collapse = " + "))),
    population = colSums(columns * (stats::weights(svy) + stand_in$weight)),
    variance = ifelse(aux > 0, aux, 1),
    stand_in = if (any(stand_in$weight > 0)) stand_in$weight
  )
}

#' Weights that let the ratio fit count an empty primary unit
#'
#' The zero row of an empty primary unit has no weight, so the calibration
#' would fit its stratum without it and give it no residual. Its window
#' total is zero whatever its activation, and its multiplier is that of its
#' cell, whose units share its block and occasions. During the calibration
#' only, its rows take a weight that gives them that multiplier at the
#' cell's mean auxiliary, so the unit enters the ratio residuals with a zero
#' total.
#' @param cell The window cell of each row, `NA` where the unit had no cell
#'   of its own. Without another unit there, the row's ratio group is used.
#' @return `aux`, the auxiliary of every row, and `weight`, the weight of the
#'   empty units' rows during the calibration, 0 elsewhere.
#' @noRd
empty_unit_stand_in <- function(aux, weight, unit, known, group, cell) {
  stand_in <- numeric(length(aux))
  empty <- !as.logical(stats::ave(known, unit, FUN = any))
  if (!any(empty)) {
    return(list(aux = aux, weight = stand_in))
  }
  multiplier <- stats::ave(weight * aux, unit, FUN = sum)
  first <- !duplicated(unit) & !empty
  mean_by <- function(x, by, keep) {
    keep <- keep & !is.na(by)
    means <- tapply(x[keep], by[keep], mean)
    as.vector(means[by])
  }
  m <- mean_by(multiplier, cell, first)
  a <- mean_by(aux, cell, known)
  fall <- is.na(m)
  m[fall] <- mean_by(multiplier, group, first)[fall]
  a[fall] <- mean_by(aux, group, known)[fall]
  rows_of <- stats::ave(rep(1, length(aux)), unit, FUN = sum)
  fill <- empty & !is.na(m) & !is.na(group)
  aux[fill] <- a[fill]
  stand_in[fill] <- m[fill] / (a[fill] * rows_of[fill])
  list(aux = aux, weight = stand_in)
}

#' Calibrate a window's design, or its replicates, on the ratio columns
#'
#' The weights do not move, and the columns are dropped once the calibration
#' holds what the variance needs. Empty units' rows weigh their stand-in
#' during the calibration and nothing after it, which in a replicate design
#' of factors zeroes them in every replicate.
#' @noRd
apply_window_ratio <- function(design, ratio) {
  if (is_null(ratio)) {
    return(design)
  }
  call <- design$call
  rows <- which(ratio$stand_in %||% 0 > 0)
  # A replicate design took these weights before its replicates were made.
  if (!inherits(design, "svyrep.design")) {
    design <- with_stand_in(design, ratio)
  }
  # Stand-ins mark empty primary units, which a stage below found empty.
  # Replicates hold combined weights only where census units, with no stage
  # below, were spliced out, so the two never meet, and the replicates
  # here hold factors that a zero weight cancels.
  if (length(rows) > 0L && isTRUE(design$combined.weights)) {
    cli_abort(
      "A window's replicate weights are combined around an empty primary
       unit, which the calibration does not handle.",
      call = NULL,
      class = "samplyr_error_internal"
    )
  }
  missing <- setdiff(colnames(ratio$columns), names(design$variables))
  design$variables <- cbind(design$variables, ratio$columns[, missing, drop = FALSE])
  args <- list(
    design,
    ratio$formula,
    population = ratio$population,
    variance = ratio$variance
  )
  # survey's default compress = NA fails on uncompressed replicate weights,
  # as a design spliced around certainty units has.
  if (inherits(design, "svyrep.design")) {
    args$compress <- inherits(design$repweights, "repweights_compressed")
  }
  calibrated <- do.call(survey::calibrate, args)
  if (inherits(calibrated, "svyrep.design")) {
    calibrated$pweights[rows] <- 0
  } else {
    calibrated$prob[rows] <- Inf
    # The linearized variance divides each row by g times the square root
    # of its weight, 0 / 0 on a zero row left without a stand-in. That row
    # is a zero row of the stored QR factor, so any nonzero divisor gives
    # the same residuals, and 1 gives zero for it.
    last <- length(calibrated$postStrata)
    w <- calibrated$postStrata[[last]]$w
    calibrated$postStrata[[last]]$w[w == 0] <- 1
  }
  calibrated$variables[colnames(ratio$columns)] <- NULL
  calibrated$call <- call
  calibrated
}

#' Give empty units' rows their stand-in weights in a linearized design
#' @noRd
with_stand_in <- function(design, ratio) {
  rows <- which(ratio$stand_in %||% 0 > 0)
  design$prob[rows] <- 1 / ratio$stand_in[rows]
  design
}

#' Export a materialized occasion as one phase, by ultimate clusters
#'
#' The occasion's stage-1 units are the clusters and the master's stage-1
#' stratification variables the strata, with the activation weights and no
#' finite population correction. Later stages and the activation are carried
#' in the unit totals, so the master's stage-1 method does not enter. A
#' window of occasions also stratifies by quota block and by the occasions a
#' unit is active at, the second-phase strata (`window_variance_cells()`),
#' pools the units left alone (`pool_window_lone()`), and is calibrated for
#' ratio residuals (`window_ratio_spec()`).
#' Leaving the number of occasions in the spread between units overstated
#' the variance by half under probabilities proportional to size, leaving
#' which occasions they are overstated it whenever the outcome changes
#' between occasions, and leaving the block overstated it by up to four
#' times when a stratum's units fill its blocks unevenly. Zones,
#' variance pairs and take-all strata are left out, since activation leaves
#' most of them with one unit or none. Without a correction no other stratum
#' is excused as a census.
#'
#' A stage-1 cluster taken with certainty is self-representing: a stratum of
#' its own whose clusters are its units of the next stage. Folded into its
#' stratum, its total was read as a draw among the others, and a window gave
#' it a stratum of one. In a simulation of one certainty EA among twelve,
#' the estimated variance of a two-occasion window fell from 5.7 times the true
#' variance (under `survey.lonely.psu = "adjust"`) to 1.6, and of a single
#' occasion from 1.7 to 1.1.
#'
#' A unit taken with certainty and nothing drawn inside it, a cluster with
#' no later stage or an element, is the same in every sample. It is a
#' census stratum of its own, with a correction of one unit out of one, and
#' the other strata take none. Left in its stratum, its total was read as a
#' draw among the others, which nearly doubled the standard error of a test
#' occasion, and a window gave it a stratum of one, on which survey stopped.
#' @noRd
build_activation_ultimate_cluster <- function(
  x,
  prev_phase,
  nest,
  dots,
  check_lonely = TRUE,
  with_spec = FALSE,
  call = caller_env()
) {
  rlang::local_error_call(call)
  # A record this samplyr cannot read may not have made these weights.
  prepare_panel_record(
    attr(x, "metadata")$panel_assignment,
    "A survey export",
    call = call
  )
  design <- prev_phase$design %||% get_design(prev_phase$sample)
  stages <- prev_phase$stages %||% get_stages_executed(prev_phase$sample)
  # A unit the master lost has a zero total in the occasions that activate it.
  df <- occasion_export_rows(x, prev_phase$sample, design, stages, call = call)
  spec <- export_stage_spec(df, design, stages[1])
  entry <- spec$stage[[1]]
  # A window repeats a unit over its occasions, and the unit is the cluster.
  if (identical(entry$unit$kind, "element") && is_occasion_window(x)) {
    entry$unit <- list(
      kind = "cluster",
      source = ".sample_id",
      id = vctrs::vec_group_id(df$.sample_id)
    )
  }
  vars <- design$stages[[stages[1]]]$strata$vars %||% character(0)
  # A unit taken with certainty does not vary between samples, only its
  # next-stage units do. It is a stratum of its own, and they its clusters.
  rotated <- attr(x, "metadata")$panel_assignment$assignment_stage %||% 1L
  census <- census_rows(df, design, stages, entry, flag_only_stage = rotated)
  sure <- self_represent(df, entry, vars, design, stages,
                         flag_only_stage = rotated)
  df <- sure$df
  entry <- sure$entry
  vars <- sure$vars
  # One with nothing drawn inside it is the same in every sample, a census
  # stratum of its own.
  if (any(census)) {
    census_col <- free_name(names(df), ".census")
    df[[census_col]] <- ifelse(census, entry$unit$id, 0L)
    vars <- c(vars, census_col)
  }
  design_vars <- vars
  # The quota blocks are second-phase strata, and their quotas fix how many
  # units share each set of occasions.
  if (is_occasion_window(x)) {
    cell_col <- free_name(names(df), ".window_cell")
    df[[cell_col]] <- window_variance_cells(
      df, entry$unit$id, attr(x, "metadata")
    )
    own_cell <- df[[cell_col]]
    own_cell[attr(own_cell, "fallback")] <- NA
    attr(df[[cell_col]], "fallback") <- NULL
    vars <- c(vars, cell_col)
    # Units the split leaves alone share one stratum, the collapsed strata
    # whose ratio residuals the calibration below gives.
    pooled <- pool_window_lone(
      df, vars, design_vars, entry$unit$id, drawn = !census & !sure$sure
    )
    if (!is_null(pooled)) {
      df[[cell_col]] <- pooled
      vars <- cell_col
    }
  }
  entry$strata <- list(
    user = vars,
    certainty = NULL,
    id = if (length(vars) > 0L) group_ids(df, vars)
  )
  entry$prob <- NULL
  spec$stage[[1]] <- entry

  id_info <- spec_survey_ids(spec, df, call = call)
  df <- id_info$df
  n_units <- if (length(id_info$id_vars) > 0L) {
    vctrs::vec_unique_count(df[[id_info$id_vars[1]]])
  } else {
    nrow(df)
  }
  if (n_units < 2L) {
    occasions <- sample_occasion_phrase(attr(x, "metadata"))
    abort_samplyr(
      c(
        "The sample of {occasions} holds a single primary unit.",
        "x" = "A variance compares primary units, and {.pkg survey} refuses
               a design with only one.",
        "i" = "The weights in {.field .weight} are exact. For a variance,
               an occasion needs at least two primary units."
      ),
      class = "samplyr_error_occasion_one_unit",
      call = call
    )
  }
  strata <- spec_survey_strata(
    spec,
    df,
    id_stage_indices = id_info$stage_indices
  )
  df <- strata$df
  if (check_lonely) {
    # A census stratum of one unit is no lonely stratum.
    checked <- spec
    checked$stage[[1]]$census_rows <- census
    # The strata the window's split by occasions leaves with one unit.
    split <- 0L
    if (is_occasion_window(x)) {
      unsplit <- checked
      unsplit$stage[[1]]$strata$id <- if (length(design_vars) > 0L) {
        group_ids(df, design_vars)
      }
      split <- sum(spec_singletons(checked, id_info$stage_indices)) -
        sum(spec_singletons(unsplit, id_info$stage_indices))
    }
    warn_lonely_strata(checked, design, id_info$stage_indices,
                       window_split = split, call = call)
  }

  args <- c(
    list(
      ids = survey_ids_formula(id_info$id_vars),
      strata = strata$formula,
      weights = stats::as.formula("~.weight"),
      data = df,
      nest = nest
    ),
    dots
  )
  # The other strata take no correction.
  if (any(census)) {
    fpc_col <- free_name(names(df), ".fpc_census")
    args$data[[fpc_col]] <- ifelse(census, 1, Inf)
    args$fpc <- stats::as.formula(paste0("~", fpc_col))
  }
  result <- survey_svydesign(args)
  ratio <- if (is_occasion_window(x)) {
    window_ratio_spec(
      result,
      attr(x, "metadata")$occasion$source_weights %||%
        as.data.frame(prev_phase$sample)[c(".sample_id", ".weight")],
      cells = own_cell
    )
  }
  if (with_spec) {
    spec$stage[[1]]$certainty <- census
    return(list(design = result, spec = spec, ratio = ratio))
  }
  apply_window_ratio(result, ratio)
}

#' Replicate weights for an occasion, from its ultimate-cluster design
#'
#' Every type resamples the occasion's primary units within the master's
#' stage-1 strata. The systematic stages named are the master's, as for any
#' generic replicate weights. A census stratum keeps its weight in every
#' replicate, as it does in the master's.
#' @noRd
build_activation_ultimate_svrep <- function(
  x,
  prev_phase,
  type,
  systematic_variance,
  ...,
  call = caller_env()
) {
  rlang::local_error_call(call)
  design <- prev_phase$design %||% get_design(prev_phase$sample)
  stages <- prev_phase$stages %||% get_stages_executed(prev_phase$sample)
  systematic_stages <- systematic_approximated_stages(
    design, stages, as.data.frame(x)
  )
  check_systematic_variance(
    systematic_stages,
    systematic_variance,
    approximation = "generic_replicates",
    fn_name = "as_svrepdesign",
    call = call
  )
  built <- build_activation_ultimate_cluster(
    x,
    prev_phase = prev_phase,
    nest = TRUE,
    dots = list(),
    check_lonely = FALSE,
    with_spec = TRUE,
    call = call
  )
  svy <- built$design
  # A window's replicates are calibrated like its linearized design, and
  # empty units weigh their stand-in in the replicates the calibration reads.
  if (!is_null(built$ratio)) {
    svy$variables <- cbind(svy$variables, built$ratio$columns)
    svy <- with_stand_in(svy, built$ratio)
  }
  result <- tryCatch(
    apply_window_ratio(
      if (any(built$spec$stage[[1]]$certainty) &&
            needs_first_stage_rebuild(built$spec, type)) {
        warn_certainty_lower_wr(
          svrep_from_first_stage(svy, built$spec, type, ...), type
        )
      } else {
        survey::as.svrepdesign(design = svy, type = type, ...)
      },
      built$ratio
    ),
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

#' Which export a materialized occasion takes
#'
#' An activation that keeps every unit is the master's own design. Any
#' other occasion is exported by ultimate clusters, unless `method` asks for
#' [survey::twophase()].
#' @return `NULL` for a sample that is not an occasion, otherwise `"identity"`,
#'   `"ultimate_cluster"` or `"twophase"`.
#' @noRd
occasion_export_route <- function(x, phase_info, method = NULL) {
  if (!survey_is_activation(phase_info)) {
    return(NULL)
  }
  metadata <- attr(x, "metadata")
  if (is_occasion_window(x)) {
    return("ultimate_cluster")
  }
  if (is_null(metadata$panel_assignment) || activation_is_identity(metadata)) {
    return("identity")
  }
  if (is_null(method)) "ultimate_cluster" else "twophase"
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
#' and CPS, the high-entropy approximation for Brewer, SPS and Pareto.
#' Systematic PPS is left out, since its pairs can have zero joint
#' probability, which leaves the variance estimator biased whatever the
#' matrix.
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
  # Unequal ones are refused below, for want of a joint matrix.
  equal_wr <- vapply(spec2$stage, function(e) {
    identical(e$kind, "wr") && !isTRUE(e$unequal)
  }, logical(1))
  if (any(equal_wr)) {
    abort_samplyr(
      c(
        "Two-phase export does not support a phase 2 drawn with replacement.",
        "x" = "{.fn survey::twophase} reads phase 2 as a subset of the
               phase-1 rows, which cannot hold a unit drawn more than once.",
        "i" = "Draw phase 2 without replacement, for instance with
               {.val srswor}."
      ),
      class = "samplyr_error_twophase_phase2_wr",
      call = call
    )
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
  stratum2 <- if (length(first2$strata$user)) {
    group_ids(df1, first2$strata$user)
  } else {
    rep(1L, nrow(df1))
  }
  twophase_finer_than_psu(df1, spec1, design2, stages2) &&
    any(units_per_group(psu1, stratum2) > 1L)
}

#' @noRd
units_per_group <- function(ids, by) {
  tapply(ids, by, function(v) length(unique(v)))
}

#' Does phase 2 select units smaller than phase 1's primary units?
#'
#' Read on the phase-1 rows, which carry the phase-2 design's variables.
#' An unclustered phase 1 has rows for primary units, so never.
#' @noRd
twophase_finer_than_psu <- function(df1, spec1, design2, stages2) {
  psu1 <- spec1$stage[[1]]$unit$id
  first2 <- export_stage_spec(df1, design2, stages2[1])$stage[[1]]
  any(units_per_group(first2$unit$id, psu1) > 1L)
}

#' Does phase 2 subsample inside units phase 1's final stage drew with
#' replacement?
#'
#' Each hit is a unit of its own, and only random hits count: a unit a
#' certainty rule took once adds no with-replacement variance. `survey::twophase()` with
#' `method = "full"` leaves out much of the variance between those hits.
#' In simulations of clusters drawn by `srswr` and households subsampled
#' within each hit, within each cluster or across clusters, it gave 0.49 to
#' 0.82 of the true variance. Read on the phase-1 rows, which carry phase
#' 2's variables and phase 1's hit numbers. Rows of a phase 1 changed after
#' execution need not hold every hit it drew, so there no hit counts as
#' certain.
#' @param complete Whether phase 1 is as executed.
#' @noRd
twophase_subsamples_wr_units <- function(df1, spec1, design1, stages1,
                                         design2, stages2, complete = TRUE) {
  last <- stages1[length(stages1)]
  if (!identical(survey_stage_kind(design1$stages[[last]]$draw_spec), "wr")) {
    return(FALSE)
  }
  # Units a certainty rule took once add no with-replacement variance.
  entry <- list(
    prob = spec_stage_prob(df1, last),
    certainty = df1[[paste0(".certainty_", last)]]
  )
  random <- !complete |
    !export_stage_sure(entry, design1$stages[[last]]$draw_spec, nrow(df1))
  if (!any(random)) {
    return(FALSE)
  }
  first2 <- export_stage_spec(df1, design2, stages2[1])$stage[[1]]
  any(units_per_group(first2$unit$id[random],
                      stage_path_ids(spec1)[random]) > 1L)
}

#' Refuse `method = "full"` for a phase 2 inside with-replacement units
#' @noRd
abort_twophase_wr_subsample <- function(design1, stages1, complete = TRUE,
                                        call = caller_env()) {
  last <- stages1[length(stages1)]
  units <- c(design1$stages[[last]]$clusters$vars,
             paste0(".phase1_draw_", last))
  abort_samplyr(
    c(
      "This two-phase design cannot be exported with
       {.code method = \"full\"}.",
      "x" = "Phase 2 subsamples inside the units phase 1's last stage drew
             with replacement, and {.fn survey::twophase} leaves out much
             of the variance between their hits. In simulations it gave
             0.49 to 0.82 of the true variance.",
      "x" = if (!complete) {
        "Phase 1 was changed after execution, so its rows need not hold
         every hit it drew, and none of them is taken as certain."
      },
      "i" = "Draw the units inside them as a stage of phase 1, with
             {.code add_stage() |> draw(frac = 1)}, and phase 2 within each
             hit, with {.code stratify_by({paste(units, collapse = ', ')})}.
             The sample then exports as one phase. See {.help as_svydesign}."
    ),
    class = "samplyr_error_twophase_wr_subsample",
    call = call
  )
}

#' Do `method = "approx"` and `"simple"` overstate this sample's variance?
#'
#' survey estimates the phase-1 part of their variance from the phase-2
#' rows, weighting each phase-1 primary unit by its chance of entering
#' phase 2, which it reads off the unit's first row. When phase 2 subsamples
#' inside the primary units, that is a row's chance, while the unit itself
#' is in phase 2 for certain, so the part is inflated. In a simulation it was
#' 2.9 times the true phase-1 variance with two `srswor` stages in phase 1
#' and 43 times with a `pps_multinomial` first stage. A phase 2 that takes
#' whole primary units, or every row, reads the right chance.
#' @noRd
twophase_approx_overstates <- function(df1, spec1, design2, stages2,
                                       all_rows) {
  !all_rows && twophase_finer_than_psu(df1, spec1, design2, stages2)
}

#' @noRd
warn_twophase_approx <- function(method, design1, stages1,
                                 call = caller_env()) {
  units <- design1$stages[[stages1[1]]]$clusters$vars
  cli_warn(
    c(
      "{.code method = \"{method}\"} overstates the phase-1 variance of this
       sample.",
      "x" = "Phase 2 subsamples inside the units phase 1 selected
             ({.field {units}}), and {.fn survey::twophase} weights each of
             those units by a row's chance of entering phase 2, while the
             unit itself is in phase 2 for certain. In a simulation the
             phase-1 part was 2.9 to 43 times its true value.",
      "i" = "The weights in {.field .weight} are exact, so totals and means
             are unaffected.",
      "i" = "{.code method = \"full\"} reads the probabilities stage by
             stage where phase 1's corrections state them. See
             {.help as_svydesign}."
    ),
    class = "samplyr_warning_twophase_approx",
    call = call
  )
}

#' @noRd
warn_twophase_across_units <- function(design1, stages1,
                                       call = caller_env()) {
  units <- design1$stages[[stages1[1]]]$clusters$vars
  if (is_multi_hit_method(design1$stages[[stages1[1]]]$draw_spec)) {
    # Each hit is a unit of its own, named by the hit number phase 2 carries.
    units <- c(units, paste0(".phase1_draw_", stages1[1]))
  }
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

#' Phase 1's final-stage pool variables, when phase 2 subsamples within them
#'
#' A simple random subsample of a simple random sample is a simple random
#' sample. So a phase 2 that draws by equal probability, without clusters,
#' within each final-stage pool of a phase 1 whose final stage draws elements
#' the same way, one phase-2 stratum per pool, is phase 1's multistage design
#' with a smaller final take. Read on the phase-1 rows, which carry the
#' phase-2 design's variables because they were its frame. `NULL` for any
#' other shape.
#' @noRd
within_pool_subsample_vars <- function(design2, stages2, phase1) {
  if (!is_null(attr(phase1, "metadata")$prev_phase)) {
    return(NULL)
  }
  equal_wor_elements <- function(spec) {
    is_null(spec$clusters) &&
      spec$draw_spec$method %in% c("srswor", "systematic") &&
      is_null(spec$draw_spec$prn) &&
      is_null(spec$draw_spec$method_type)
  }
  if (
    length(design2$stages) != 1L ||
      !identical(as.integer(stages2), 1L) ||
      !equal_wor_elements(design2$stages[[1]])
  ) {
    return(NULL)
  }
  design1 <- get_design(phase1)
  stages1 <- get_stages_executed(phase1)
  last <- length(design1$stages)
  if (
    !identical(as.integer(stages1), seq_len(last)) ||
      !equal_wor_elements(design1$stages[[last]])
  ) {
    return(NULL)
  }

  # Above a stage that can repeat a unit, each hit is a pool of its own,
  # which phase 2 names by the hit number it carries.
  hits <- phase_draw_columns(phase1)
  pool_vars <- unique(c(
    collect_ancestor_cluster_vars(design1, last),
    unname(hits),
    design1$stages[[last]]$strata$vars
  ))
  strata2 <- design2$stages[[1]]$strata$vars %||% character(0)
  df1 <- with_phase_draw_columns(as.data.frame(phase1), phase1)
  if (!all(c(pool_vars, strata2) %in% names(df1))) {
    return(NULL)
  }
  n_groups <- function(vars) {
    if (length(vars) == 0L) 1L else nrow(vctrs::vec_unique(df1[vars]))
  }
  n_pools <- n_groups(pool_vars)
  if (
    n_groups(strata2) != n_pools ||
      n_groups(unique(c(pool_vars, strata2))) != n_pools
  ) {
    return(NULL)
  }
  pool_vars
}

#' Phase 1's design as the composed export reads it
#'
#' A systematic phase 2 makes the composed final take systematic. The final
#' stage carries that method, so every export and `variance_estimators()`
#' treat its variance as the approximation it is. A phase 2 that took every
#' unit of every pool (`census`) selected nothing and leaves phase 1's method.
#' @noRd
composed_design <- function(design1, design2, census) {
  if (
    identical(design2$stages[[1]]$draw_spec$method, "systematic") &&
      !isTRUE(census)
  ) {
    last <- length(design1$stages)
    design1$stages[[last]]$draw_spec$method <- "systematic"
  }
  design1
}

#' The single-phase sample a two-phase export stands for, if there is one
#' @noRd
composed_subsample <- function(x) {
  phase_info <- survey_phase_info(x)
  if (!phase_info$is_twophase || survey_is_activation(phase_info)) {
    return(NULL)
  }
  compose_within_pool_subsample(x) %||% fixed_phase1_subsample(x)
}

#' Is phase 1 fixed, in a single primary unit?
#'
#' `survey::twophase()` builds phase 1 without `nest`, so it refuses one
#' primary unit whatever its strata. When phase 1 could only have selected
#' what it did, phase 2 is a sample of that unit's population itself. A
#' final weight of one does not show it, since stage weights can cancel and
#' a unit drawn with replacement can be hit more than once. So every stage
#' must have taken each of its units for sure, by its own mechanism (only a
#' recorded certainty counts at a stage drawn with replacement). A
#' fixed-size stage whose units are all sure gave the others no chance, and
#' a stage drawn with replacement whose hits are all recorded certainties
#' had no draw left. A random-size stage can miss units by chance, and a
#' pool in which it selected nothing leaves no row. It has to be proved to
#' take every unit of every pool. Execution records every such empty pool,
#' so where the record names none at the stage, its pools are those with
#' rows, and each must hold as many units, all sure, as its population
#' count. A sample without the record leaves its empty pools unknown, and
#' only the design can prove the stage, see `bernoulli_takes_every_unit()`.
#' A fixed-size stage without a
#' variance formula of its own takes at least one unit in every pool it
#' draws from, so its pools must hold as many units as their recorded
#' population counts. The proof reads the sample and the design alone, so it
#' is the same with or without a frame digest, and a sample changed after
#' execution never qualifies. It reads every unit selected, including those
#' an empty stage below left with no row, from their execution records.
#' @noRd
phase1_is_fixed_one_unit <- function(phase1) {
  if (!is_tbl_sample(phase1) ||
      !is_null(attr(phase1, "metadata")$prev_phase)) {
    return(FALSE)
  }
  design1 <- get_design(phase1)
  stages1 <- get_stages_executed(phase1)
  if (nrow(phase1) == 0L ||
      !identical(as.integer(stages1), seq_along(design1$stages)) ||
      !sample_realization_status(phase1)$ok) {
    return(FALSE)
  }
  df1 <- sample_every_unit_rows(phase1, design1, stages1)
  if (is_null(df1)) {
    return(FALSE)
  }
  spec1 <- export_stage_spec(df1, design1, stages1)
  for (pos in seq_along(stages1)) {
    k <- stages1[pos]
    entry <- list(
      prob = spec_stage_prob(df1, k),
      certainty = df1[[paste0(".certainty_", k)]]
    )
    draw <- design1$stages[[k]]$draw_spec
    if (!all(export_stage_sure(entry, draw, nrow(df1)))) {
      return(FALSE)
    }
    if (is_random_size_method(draw)) {
      recorded <- identical(stage_pool_evidence(phase1, k), "none_empty") &&
        stage_takes_every_unit(df1, spec1, pos, k)
      if (!recorded &&
            !bernoulli_takes_every_unit(design1$stages[[k]], df1, pos, k)) {
        return(FALSE)
      }
    } else if (
      !survey_stage_kind(draw) %in% c("equal_wor", "pps_wor", "wr") &&
        !stage_takes_every_unit(df1, spec1, pos, k)
    ) {
      return(FALSE)
    }
  }
  # survey sees the sample's rows, so its primary units are counted there.
  rows1 <- as.data.frame(phase1)
  first <- design1$stages[[stages1[1]]]
  if (is_null(first$clusters)) {
    return(nrow(rows1) == 1L)
  }
  nrow(unique(rows1[c(first$strata$vars, first$clusters$vars)])) == 1L
}

#' A row for every unit a sample selected, or `NULL` when that is unknown
#'
#' A unit selected at random can lose every row to an empty stage below.
#' Its record keeps it, as a zero row, and a record without a row proves
#' nothing, so it gives `NULL`.
#' @noRd
sample_every_unit_rows <- function(x, design, stages) {
  records <- sample_empty_parents(x)
  if (any(vapply(records, function(r) is_null(r$rows), logical(1)))) {
    return(NULL)
  }
  export_rows(x, design, stages, every_level = TRUE, notify = FALSE)
}

#' What execution recorded about the empty pools of a stage
#'
#' `"none_empty"` when the record holds no pool of the stage, `"some_empty"`
#' when it holds one, and `"unknown"` when the sample has no record, as a
#' sample built anew from another, such as an occasion, has none. A
#' fixed-size stage takes a unit in every pool, so only a random-size
#' stage's pools can be empty.
#' @noRd
stage_pool_evidence <- function(x, stage) {
  records <- attr(x, "metadata")$empty_pools
  if (is_null(records)) {
    return("unknown")
  }
  stages <- vapply(records, function(r) as.integer(r$stage), integer(1))
  if (any(stages == as.integer(stage))) "some_empty" else "none_empty"
}

#' Is a random-size stage proved to take every unit of every pool?
#'
#' Only a Bernoulli stage can be, and only where the design and what
#' execution always records prove it for every pool, the pools without a row
#' included. Each sufficient proof is listed here. A fraction of one gives
#' every unit chance one, as one value or as a table of ones. A size `n`
#' gives chance `min(1, n / N)`, so it proves the stage at stage 1 when
#' every pool is known and shows `n >= N`. A table of sizes is checked at
#' execution to name every stratum of the frame and no other, and a zero
#' size is refused, so a stratum of the table without a row drew nothing by
#' chance. Every stratum of the table must then have rows whose recorded
#' population count `.fpc_k` its size reaches. One size without strata is
#' one pool, which the rows show. A population count only speaks for a pool
#' with rows, so it never proves the stage alone, and the frame digest,
#' which is optional, never enters. One size across strata, or allocated
#' between them, and a stage below the first, whose pools lie inside each
#' parent, leave pools that nothing recorded at execution enumerates, so
#' they decline.
#' @param stage The stage as designed.
#' @param df1 A row for every unit the sample selected.
#' @noRd
bernoulli_takes_every_unit <- function(stage, df1, pos, k) {
  draw <- stage$draw_spec
  if (!identical(draw$method, "bernoulli")) {
    return(FALSE)
  }
  frac <- draw$frac
  if (!is_null(frac)) {
    if (is.data.frame(frac)) {
      frac <- frac$frac
    }
    return(is.numeric(frac) && length(frac) > 0L && all(frac == 1))
  }
  size <- draw$n
  count <- df1[[paste0(".fpc_", k)]]
  strata <- stage$strata$vars %||% character(0)
  if (pos != 1L || is_null(size) || is_null(count) || nrow(df1) == 0L) {
    return(FALSE)
  }
  if (!is.data.frame(size)) {
    return(length(strata) == 0L && length(size) == 1L &&
             all(size >= count))
  }
  if (length(strata) == 0L || !all(c(strata, "n") %in% names(size)) ||
        !all(strata %in% names(df1))) {
    return(FALSE)
  }
  keys <- size[strata]
  # Every stratum of the table holds rows, and its size reaches its count.
  all(vctrs::vec_in(keys, df1[strata])) &&
    isTRUE(all(size$n[vctrs::vec_match(df1[strata], keys)] >= count))
}

#' Does every pool of a stage hold as many units as its population count?
#'
#' A pool is a stratum of the stage within one unit of the stage above.
#' `.fpc_<k>` records its population count, infinite for a stage drawn with
#' replacement, which then never qualifies.
#' @noRd
stage_takes_every_unit <- function(df1, spec1, pos, k) {
  count <- df1[[paste0(".fpc_", k)]]
  if (is_null(count) || !all(is.finite(count))) {
    return(FALSE)
  }
  n <- nrow(df1)
  parent <- if (pos == 1L) {
    rep(1L, n)
  } else {
    stage_path_ids(list(stage = spec1$stage[seq_len(pos - 1L)], n_rows = n))
  }
  entry <- spec1$stage[[pos]]
  pool <- group_ids(
    data.frame(parent = parent, stratum = entry$strata$id %||% rep(1L, n)),
    c("parent", "stratum")
  )
  taken <- tapply(entry$unit$id, pool, function(u) length(unique(u)))
  recorded <- tapply(count, pool, function(v) v[1])
  all(taken == recorded)
}

#' Phase 2 as a one-phase sample, when phase 1 could select nothing else
#'
#' Exact for every phase-2 method, so its own design exports it. Any phase 1
#' survey can represent keeps the two-phase export.
#' @noRd
fixed_phase1_subsample <- function(x) {
  if (!phase1_is_fixed_one_unit(get_prev_phase(x)$sample)) {
    return(NULL)
  }
  metadata <- attr(x, "metadata")
  metadata$prev_phase <- NULL
  new_tbl_sample(as.data.frame(x), get_design(x), get_stages_executed(x),
                 seed = attr(x, "seed"), metadata = metadata)
}

#' A subsample within phase 1's final-stage pools, as phase 1's sample
#'
#' Every phase-1 quantity the export reads (each stage's weight, fpc,
#' certainty, zone, pair and hit number, and the ids and strata) is constant
#' within a final-stage pool, so phase 2's rows take them from their pool
#' and no row identifier is needed. Below a stage that can repeat a unit the
#' pool is one hit, which phase 2 names by its carried hit number. The final stage's weight absorbs phase 2's, so its
#' take, probability and weight agree. `NULL` when the shape does not apply.
#' @noRd
compose_within_pool_subsample <- function(x) {
  phase1 <- get_prev_phase(x)$sample
  design2 <- get_design(x)
  stages2 <- get_stages_executed(x)
  pool_vars <- within_pool_subsample_vars(design2, stages2, phase1)
  if (is_null(pool_vars)) {
    return(NULL)
  }
  design1 <- get_design(phase1)
  stages1 <- get_stages_executed(phase1)
  df1 <- with_phase_draw_columns(as.data.frame(phase1), phase1)
  df2 <- as.data.frame(x)

  # A hit number is constant within its pool, which it names.
  stage_cols <- intersect(
    paste0(
      c(".weight_", ".fpc_", ".certainty_", ".zone_", ".pair_", ".draw_"),
      rep(stages1, each = 6L)
    ),
    names(df1)
  )
  lookup <- vctrs::vec_unique(df1[c(pool_vars, stage_cols)])
  if (vctrs::vec_duplicate_any(lookup[pool_vars])) {
    return(NULL)
  }
  row <- if (length(pool_vars) == 0L) {
    rep(1L, nrow(df2))
  } else {
    vctrs::vec_match(df2[pool_vars], lookup[pool_vars])
  }
  if (anyNA(row)) {
    return(NULL)
  }

  weight <- df2$.weight
  factor2 <- df2[[paste0(".weight_", stages2)]]
  df2 <- df2[setdiff(names(df2), samplyr_reserved_names(names(df2)))]
  for (col in stage_cols) {
    df2[[col]] <- lookup[[col]][row]
  }
  last <- paste0(".weight_", max(stages1))
  df2[[last]] <- df2[[last]] * factor2
  df2$.weight <- weight
  df2$.sample_id <- seq_len(nrow(df2))
  df2$.stage <- max(stages1)

  product <- Reduce(`*`, df2[paste0(".weight_", stages1)])
  if (!isTRUE(all.equal(product, weight))) {
    cli_abort(
      "Internal error: the composed stage weights do not multiply to
       {.field .weight}.",
      call = NULL,
      class = "samplyr_error_internal"
    )
  }
  new_tbl_sample(
    df2,
    composed_design(
      design1, design2,
      census = spec_stage_census(as.data.frame(x), stages2)
    ),
    stages1,
    seed = attr(phase1, "seed"),
    metadata = attr(phase1, "metadata")
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
  # The export's last kept stage, read as drawn with replacement.
  if (isTRUE(draw_spec$export_wr)) {
    return("wr")
  }
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

#' Does every phase-2 row carry the position of its phase-1 row?
#'
#' A phase drawn from the phase-1 sample alone keeps each phase-1 row's
#' position, which links the phases exactly. Rows a later stage drew from
#' a register of its own carry none, and link through the identifiers the
#' phases declare.
#' @noRd
phase_rows_linked <- function(df2, row_col, n1) {
  rows <- df2[[row_col]]
  !is_null(rows) && length(rows) > 0L && !anyNA(rows) &&
    all(rows >= 1L & rows <= n1)
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

#' The unit identifiers a phase declares, which can link it to the other
#'
#' Only declared columns qualify. They link the phases when a phase-2 stage
#' drew from a register of its own, whose rows carry no phase-1 row
#' position. A `.draw_k` column numbers the hits of a multi-hit stage, but
#' execution drops it from the frame of the next phase, so it can never link
#' the phases. A unit hit twice then repeats its identifier, and
#' `resolve_phase_bridge()` refuses the ambiguous join if phase 2 reaches
#' it.
#' @noRd
survey_key_vars <- function(design, stages_executed, df) {
  vars <- unlist(lapply(stages_executed, function(stage_idx) {
    design$stages[[stage_idx]]$clusters$vars
  }))
  unique(vars[!is_samplyr_column(vars)])
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

#' The stages of each row that the variance of a survey design never reads
#'
#' survey's compiled variance, which it uses for a design without `pps`,
#' adds the stages below a unit scaled by the unit's sampling fraction and
#' skips them when that fraction is zero. A unit drawn with replacement has
#' none, since the spread between its hits carries the stages below it.
#' Under `options(survey.ultimate.cluster = TRUE)` survey reads stage 1
#' alone. Its R variance, used for a `pps` design, after a calibration by
#' stage or under `options(survey.use_rcpp = FALSE)`, computes every stage
#' before scaling it, so a stratum of one unit stops it at any stage.
#' @param n_rows The rows of the spec the design was built from.
#' @return A logical matrix with one row per data row and one column per
#'   identifier stage, `TRUE` where survey reads none of that stage, or `NULL`
#'   when it reads every stage, has no correction term to read the fractions
#'   from, or the design's rows do not match the spec.
#' @noRd
survey_unread_stages <- function(svy, n_rows) {
  n_stages <- NCOL(svy$cluster)
  if (n_stages < 2L || !identical(nrow(svy$variables), as.integer(n_rows))) {
    return(NULL)
  }
  out <- matrix(FALSE, n_rows, n_stages)
  if (isTRUE(getOption("survey.ultimate.cluster"))) {
    out[, -1L] <- TRUE
    return(out)
  }
  popsize <- svy$fpc$popsize
  if (is_null(popsize) || !isFALSE(svy$fpc$pps) ||
      !isTRUE(getOption("survey.use_rcpp"))) {
    return(NULL)
  }
  zero <- svy$fpc$sampsize / popsize == 0
  for (p in 2:n_stages) {
    out[, p] <- rowSums(zero[, seq_len(p - 1L), drop = FALSE]) > 0
  }
  out
}

#' Warn about strata that hold a single sampled unit.
#'
#' `survey` computes each stratum's variance from the spread of its sampled
#' units, which one unit cannot give. Under its default
#' `survey.lonely.psu = "fail"` it stops when a variance is requested,
#' naming a stratum by internal codes. Warning at export names the stage and
#' the design's own strata instead. A stratum taken whole is a census, which
#' survey accepts, and so is not reported.
#' @param window_split How many of the lonely strata a window's export made
#'   by splitting the design's strata by quota block and the occasions a unit is
#'   active. They cannot be collapsed before export.
#' @param unread From `survey_unread_stages()`: the rows whose strata at each
#'   stage survey's variance never reads, which no singleton there can stop.
#'   `NULL` reads every stage.
#' @noRd
warn_lonely_strata <- function(spec, design, id_stage_indices,
                               window_split = 0L, unread = NULL,
                               call = caller_env()) {
  if (!identical(getOption("survey.lonely.psu", "fail"), "fail")) {
    return(invisible(NULL))
  }
  lonely <- spec_singletons(spec, id_stage_indices, unread)
  first_stage <- c(id_stage_indices, spec$stages[1])[1L]
  for (stage_idx in as.integer(names(lonely)[lonely > 0L])) {
    n_lonely <- lonely[[as.character(stage_idx)]]
    token <- stage_token(design, stage_idx)
    hint <- if (window_split > 0L && stage_idx == first_stage) {
      c(
        "i" = "{window_split} of them come from the window's export, which
               splits each stratum by quota block and by the occasions a unit
               is active at, and not from the design.",
        "i" = "Set {.code options(survey.lonely.psu = \"average\")} to give
               such a stratum the mean variance of the others, or
               {.code \"adjust\"} for a conservative variance that can be
               far larger. See {.topic panel_assignment}."
      )
    } else if (stage_idx != first_stage) {
      above <- stage_idx - 1L
      kept <- if (above == 1L) "1" else paste0("1:", above)
      c(
        "i" = "At a later stage a stratum is the units drawn in one unit of
               the stage above, so a stage that takes one unit per parent,
               such as a person per household, leaves each parent a stratum
               of one.",
        "i" = "Export with {.code stages = {kept}} to carry {token} in the
               totals of the stage above, or set
               {.code options(survey.lonely.psu = \"adjust\")} for the
               conservative treatment."
      )
    } else {
      c(
        "i" = "Set {.code options(survey.lonely.psu = \"adjust\")} for the
               conservative treatment, or collapse the strata before
               export."
      )
    }
    cli_warn(
      c(
        "In {token}, {n_lonely} strat{?um/a} hold{?s/} a single
         sampled unit.",
        "i" = "{.pkg survey} cannot estimate a variance from one unit and,
               under its default {.code survey.lonely.psu = \"fail\"},
               stops when a standard error is requested.",
        hint
      ),
      class = "samplyr_warning_lonely_psu",
      stage = stage_idx,
      n_strata = n_lonely,
      call = call
    )
  }
  invisible(NULL)
}

#' Report units taken with probability 1 that the stage export reads as drawn
#' @noRd
inform_stage_certainty_drawn <- function(token, n_units, call = caller_env()) {
  cli_inform(
    c(
      "In {token}, {n_units} unit{?s} taken with probability 1
       {qty(n_units)}{?has/have} a single unit drawn below, and {?is/are}
       read as drawn.",
      "i" = "A stratum of one unit cannot estimate the variance below it, so
             the spread between the units of {qty(n_units)}{?its/their}
             pool stands in for it."
    ),
    class = "samplyr_message_stage_certainty_drawn",
    n_units = n_units,
    call = call
  )
}

#' Selected primary units left with no row in the export's rows
#'
#' A primary unit whose descendants were all empty has no row, so survey's
#' variance between primary units runs over the others: the unit's zero total
#' is lost, and with it both the spread and the count of units. Each
#' empty-parent record carries its parent's full ancestry, which names the
#' primary unit. `df` is the rows the export uses, its zero rows included.
#' @return The vanished units' keys as a data frame, or NULL.
#' @noRd
vanished_primary_units <- function(x, design = get_design(x),
                                   stages = get_stages_executed(x),
                                   df = as.data.frame(x)) {
  records <- sample_empty_parents(x)
  vars <- design$stages[[stages[1]]]$clusters$vars
  if (length(records) == 0L || is_null(vars)) {
    return(NULL)
  }
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
  # A replicate whose rows a settlement removed still names itself.
  reps <- union(unique(x$.replicate), extracted_replicate_id(x))
  Filter(function(r) is.na(r$replicate) || r$replicate %in% reps, records)
}

#' Refuse an export that would drop an empty primary unit
#'
#' Reached by a route that does not add zero rows, and by a unit whose
#' record keeps no row to build one from.
#' @noRd
check_export_primary_units <- function(x, design = get_design(x),
                                       stages = get_stages_executed(x),
                                       df = as.data.frame(x),
                                       call = caller_env()) {
  gone <- vanished_primary_units(x, design, stages, df)
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
      "i" = "Totals and means from {.field .weight} are right. This export
             does not represent an empty primary unit yet."
    ),
    class = "samplyr_error_export_empty_psu",
    n_units = n,
    call = call
  )
}

#' A sample's rows for export, with a zero row per unit found empty
#'
#' A selected unit whose later stage had nothing to sample has no row, and
#' survey learns its units from rows. A primary unit left with no row would
#' take its zero total out of the variance, so each of its empty units gets
#' a zero row instead, one per draw. `every_level` does the same for empty
#' units inside a primary unit that keeps rows, and for every unit whose
#' pools a random-size draw below left all empty, for RWYB. The row
#' carries the unit's values for the stages above, its frame variables
#' included, and below it a single unit taken whole, so it adds no variance
#' of its own. Its weight is 0, so it adds nothing to a total or a count,
#' and its analysis variables are `NA`. In a simulation of two strata of
#' five schools, two of them empty, the estimated variance went from 0.47
#' of the true one to 0.97 under simple random sampling.
#'
#' A zero row is a selected unit only down to its record's stage. From that
#' stage on it is a placeholder, no unit any stage selected, and the
#' `zero_from` attribute gives that stage for each row, `NA` for a row of
#' the sample, so a count of a stage's selections can leave them out.
#' @noRd
export_rows <- function(x, design = get_design(x),
                        stages = get_stages_executed(x),
                        every_level = FALSE, notify = TRUE,
                        call = caller_env()) {
  df <- as.data.frame(x)
  zero_from <- rep(NA_integer_, nrow(df))
  records <- if (every_level) {
    Filter(
      function(r) !is_null(r$rows) && r$stage <= max(stages),
      sample_empty_parents(x)
    )
  } else {
    export_empty_records(x, design, stages, vanished = TRUE)
  }
  if (length(records) > 0L) {
    zero <- lapply(records, function(r) empty_unit_rows(df, r, stages))
    df <- vctrs::vec_rbind(df, !!!zero)
    zero_from <- c(zero_from, unlist(lapply(records, function(r) {
      rep(as.integer(r$stage), nrow(r$rows))
    })))
    if (notify) {
      warn_export_empty_units(records, design, call = call)
    }
  }
  if (every_level) {
    # Execution already said that the draw selected nothing.
    drawn <- add_draw_emptied_units(x, design, stages, df)
    zero_from <- c(zero_from, drawn$zero_from)
    df <- drawn$df
  }
  df <- mark_placeholder_wr_stages(df, zero_from, design, stages)
  check_export_primary_units(x, design, stages, df = df, call = call)
  attr(df, "zero_from") <- zero_from
  df
}

#' Mark a zero row's stages drawn with replacement as no draw
#'
#' A zero row takes the stages from its record's on as one unit taken
#' whole. A stage drawn with replacement reads that from `.certainty_k`
#' alone, which a stage without a certainty rule, as `srswr`, never wrote.
#' Its rows are then flagged `FALSE`, which they would read without the
#' column, and its placeholders `TRUE`.
#' @noRd
mark_placeholder_wr_stages <- function(df, zero_from, design, stages) {
  if (!any(!is.na(zero_from))) {
    return(df)
  }
  for (k in stages) {
    col <- paste0(".certainty_", k)
    if (col %in% names(df) ||
        !identical(survey_stage_kind(design$stages[[k]]$draw_spec), "wr")) {
      next
    }
    df[[col]] <- !is.na(zero_from) & zero_from <= k
  }
  df
}

#' Add a zero row for each unit a random-size draw below left with no row
#'
#' A unit whose pools at the next stage all selected nothing has no row,
#' and no row below it either. `empty_pools` keeps its rows in the sample of
#' the stage above, one per hit. Its estimated total is zero, as is the
#' usual estimate of the variance within it, which sums over sampled units,
#' so its zero row takes the stages below as a single unit taken whole, as
#' for a unit whose register was empty. A unit is lost only if no row
#' descends from it, a zero row of a deeper record included, so the records
#' are read from the deepest stage up and each against the rows added
#' before it. A unit whose later pools drew a child that a deeper stage
#' emptied in turn is kept by that child's row, with no placeholder of its
#' own.
#' @param df The rows the export holds so far, zero rows included.
#' @return A list of `df` with the zero rows added and `zero_from`, their
#'   records' stages.
#' @noRd
add_draw_emptied_units <- function(x, design, stages, df) {
  records <- attr(x, "metadata")$empty_pools
  if (".replicate" %in% names(x)) {
    reps <- union(unique(x$.replicate), extracted_replicate_id(x))
    records <- Filter(function(r) {
      is.na(r$replicate) || r$replicate %in% reps
    }, records)
  }
  records <- Filter(function(r) {
    !is_null(r$rows) && r$stage >= 2L && r$stage <= max(stages)
  }, records)
  order <- order(-vapply(records, function(r) as.integer(r$stage), 1L))
  zero_from <- integer(0)
  for (r in records[order]) {
    k <- r$stage
    parent <- intersect(
      c(collect_ancestor_strata_vars(design, k),
        collect_ancestor_cluster_vars(design, k),
        paste0(".draw_", seq_len(k - 1L))),
      intersect(names(r$rows), names(df))
    )
    if (length(parent) == 0L) {
      next
    }
    lost <- !vctrs::vec_in(r$rows[parent], df[parent])
    if (!any(lost)) {
      next
    }
    record <- list(stage = k, replicate = r$replicate,
                   rows = vctrs::vec_slice(r$rows, lost), n = sum(lost))
    df <- vctrs::vec_rbind(df, empty_unit_rows(df, record, stages))
    zero_from <- c(zero_from, rep(as.integer(k), sum(lost)))
  }
  list(df = df, zero_from = zero_from)
}

#' The records of empty units, cut to those whose primary unit has no row
#'
#' `vanished = TRUE` keeps the rows of units whose primary unit lost every
#' row, which the export turns into zero rows. `FALSE` keeps the others,
#' empty units inside a primary unit that still has rows. Those stay
#' without a row, since survey's calibration cannot take a row of weight 0
#' and a household with no eligible member is the common case.
#' @noRd
export_empty_records <- function(x, design, stages, vanished) {
  df <- as.data.frame(x)
  records <- Filter(
    function(r) r$stage <= max(stages),
    sample_empty_parents(x)
  )
  vars <- reserve_primary_vars(design, stages[1], df)
  if (length(vars) == 0L) {
    return(if (vanished) list() else records)
  }
  records <- lapply(records, function(r) {
    if (is_null(r$rows) || !all(vars %in% names(r$rows))) {
      return(if (vanished) NULL else r)
    }
    lost <- !vctrs::vec_in(r$rows[vars], df[vars])
    keep <- if (vanished) lost else !lost
    r$rows <- vctrs::vec_slice(r$rows, keep)
    r$n <- nrow(vctrs::vec_unique(r$rows[intersect(names(r$keys), names(r$rows))]))
    if (nrow(r$rows) > 0L) r
  })
  Filter(Negate(is_null), records)
}

#' An occasion's rows for export, with the zero rows its activation keeps
#'
#' A primary unit the master found empty is a zero row of every occasion that
#' activates it, once for each occasion of a window at which its panel is
#' active, as a unit with rows would be.
#' @noRd
occasion_export_rows <- function(x, master, design, stages,
                             call = caller_env()) {
  df <- as.data.frame(x)
  # Refuses a unit of the master whose record keeps no row.
  export_rows(master, design, stages, notify = FALSE, call = call)
  records <- export_empty_records(master, design, stages, vanished = TRUE)
  if (length(records) == 0L) {
    return(df)
  }
  record <- prepare_panel_record(
    attr(x, "metadata")$panel_assignment,
    "A survey export",
    call = call
  )
  occasions <- attr(x, "metadata")[["occasion"]]$occasion
  kept <- lapply(records, function(r) {
    zero <- empty_unit_rows(df, r, stages)
    parts <- lapply(occasions, function(w) {
      active <- record$schedule$panel[
        record$schedule$occasion == w & record$schedule$active
      ]
      on <- activate_panels(zero, record, active, allow_empty = TRUE,
                            call = call)
      rows <- vctrs::vec_slice(zero, on$keep)
      if (".period" %in% names(rows)) {
        pairs <- grid_pairs(record$schedule)
        rows$.period <- vctrs::vec_cast(rep(pairs$period[w], nrow(rows)),
                                        df$.period)
        rows$.subround <- vctrs::vec_cast(
          rep(pairs$subround[w], nrow(rows)), df$.subround
        )
      }
      if (".visit" %in% names(rows)) {
        rows$.visit <- vctrs::vec_cast(
          occasion_row_visits(record, rows$.panel, w, on$permanent[on$keep]),
          df$.visit
        )
      }
      rows
    })
    r$rows <- vctrs::vec_rbind(!!!parts)
    r
  })
  kept <- Filter(function(r) nrow(r$rows) > 0L, kept)
  if (length(kept) == 0L) {
    return(df)
  }
  warn_export_empty_units(kept, design, call = call)
  vctrs::vec_rbind(df, !!!lapply(kept, function(r) r$rows))
}

#' The zero rows of one record of empty units
#' @noRd
empty_unit_rows <- function(df, record, stages) {
  rows <- record$rows
  out <- vctrs::vec_init(df, nrow(rows))
  for (v in setdiff(intersect(names(rows), names(df)), ".sample_id")) {
    out[[v]] <- vctrs::vec_cast(rows[[v]], df[[v]])
  }
  set <- function(col, value) {
    if (col %in% names(out)) {
      out[[col]] <<- vctrs::vec_cast(rep(value, nrow(out)), df[[col]])
    }
  }
  for (j in stages[stages >= record$stage]) {
    set(paste0(".weight_", j), 1)
    set(paste0(".fpc_", j), 1)
    set(paste0(".certainty_", j), TRUE)
    set(paste0(".draw_", j), 1)
    set(paste0(".reserve_", j), 0)
  }
  if (!is.na(record$replicate)) {
    set(".replicate", record$replicate)
  }
  set(".weight", 0)
  out
}

#' Say that an export added zero rows, unless every stage asked for silence
#' @noRd
warn_export_empty_units <- function(records, design, call = caller_env()) {
  loud <- Filter(function(r) {
    !identical(design$stages[[r$stage]]$draw_spec$on_empty, "silent")
  }, records)
  if (length(loud) == 0L) {
    return(invisible(NULL))
  }
  stages <- vapply(loud, function(r) r$stage, integer(1))
  counts <- vapply(loud, function(r) nrow(r$rows), integer(1))
  by_stage <- tapply(counts, stages, sum)
  bullets <- vapply(names(by_stage), function(k) {
    k <- as.integer(k)
    n_k <- by_stage[[as.character(k)]]
    paste0(
      n_k, " unit", if (n_k > 1L) "s", " of ", stage_token(design, k - 1L),
      " with nothing at ", stage_token(design, k)
    )
  }, character(1))
  n <- sum(counts)
  cli_warn(
    c(
      "{n} selected unit{?s} had nothing to sample below {?it/them}, so the
       export adds a zero row for {?it/each}.",
      stats::setNames(bullets, rep("*", length(bullets))),
      "i" = "A zero row has weight 0, so totals and counts are unchanged,
             and it keeps its unit in the variance as a zero total.",
      "i" = "Its analysis variables hold {.code NA}, so estimates need
             {.code na.rm = TRUE}.",
      "i" = "{.code on_empty = \"silent\"} in {.fn draw} drops this
             warning."
    ),
    class = "samplyr_warning_export_empty_parent",
    n_empty = n,
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
#' exported as zero rows, which this leaves out. A stage drawn with
#' `on_empty = "silent"` is not named.
#' @noRd
warn_export_empty_parents <- function(x, design, id_stage_indices,
                                      call = caller_env()) {
  records <- Filter(function(r) {
    !identical(design$stages[[r$stage]]$draw_spec$on_empty, "silent")
  }, export_empty_records(x, design, get_stages_executed(x),
                          vanished = FALSE))
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
#' A pool is lonely when it holds one sampling unit and was not taken whole.
#' A census pool is exempt, since survey accepts it, and so is a pool whose
#' rows survey's variance never reads (`unread`, as in
#' `spec_pool_units()`).
#' @return Named integer, the number of lonely pools per stage.
#' @noRd
spec_singletons <- function(spec, stages, unread = NULL) {
  vapply(spec_pool_units(spec, stages, unread), function(pools) {
    sum(pools$n_units == 1L & !pools$census & !pools$unread)
  }, integer(1))
}

#' Sampled units in each pool, per represented stage
#'
#' Pools are strata within the units selected at the stages above. With no
#' identifier stage, the single element stage is scanned.
#' @param unread `NULL`, or a logical matrix with one row per row of the
#'   spec and one column per stage of `stages`, `TRUE` where survey's
#'   variance never reads that stage of the row.
#' @return A list named by stage, each a data frame with one row per pool
#'   giving `n_units`, whether the pool was taken whole (`census`) and
#'   whether survey reads none of its rows (`unread`).
#' @noRd
spec_pool_units <- function(spec, stages, unread = NULL) {
  # A single unclustered stage needs no id column: its units are the rows.
  if (length(stages) == 0) {
    stages <- spec$stages[1]
  }
  n <- spec$n_rows
  parent <- rep(1L, n)
  out <- list()
  for (stage_idx in stages) {
    entry <- spec$stage[[as.character(stage_idx)]]
    stratum <- entry$strata$id %||% rep(1L, n)
    pool <- group_ids(
      data.frame(parent = parent, stratum = stratum),
      c("parent", "stratum")
    )
    # A cluster lies inside one stratum, but the draw number that names a
    # unit drawn with replacement restarts in each stratum.
    unit <- group_ids(
      data.frame(parent = parent, stratum = stratum, unit = entry$unit$id),
      c("parent", "stratum", "unit")
    )
    first <- !duplicated(unit)
    n_units <- tabulate(pool[first], nbins = max(c(0L, pool)))
    row_census <- entry$census_rows %||% if (identical(entry$kind, "wr")) {
      # Expected hits of one are no census, a declared certainty unit is.
      !is.na(entry$certainty %||% rep(FALSE, n)) &
        (entry$certainty %||% rep(FALSE, n))
    } else {
      is_certainty_probability(entry$prob %||% rep(0, n))
    }
    census <- as.vector(tapply(
      row_census,
      factor(pool, levels = seq_along(n_units)),
      all
    ))
    pos <- match(stage_idx, stages)
    skipped <- if (is_null(unread)) {
      rep(FALSE, length(n_units))
    } else {
      as.vector(tapply(unread[, pos], factor(pool, levels = seq_along(n_units)),
                       all))
    }
    out[[as.character(stage_idx)]] <- data.frame(
      n_units = n_units,
      census = as.logical(census),
      unread = as.logical(skipped)
    )
    parent <- unit
  }
  out
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
  # Certainty units of a stage drawn with replacement are a census.
  wr_certain <- lapply(fpc_stage_indices, function(stage_idx) {
    flag <- df[[paste0(".certainty_", stage_idx)]]
    if (!is_null(flag) && any(flag %in% TRUE)) flag %in% TRUE
  })

  needs_pi <- has_pps_wor || any(!vapply(wr_certain, is_null, logical(1))) ||
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

    if (identical(kind, "wr") && !is_null(wr_certain[[i]])) {
      # Sampling fraction one for the take-all stratum, none for the draws.
      fpc_vars <- c(fpc_vars, add_term(
        paste0(".fpc_pi_", stage_idx), as.numeric(wr_certain[[i]])
      ))
      next
    }
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
  # These specifications describe stage 1 alone, from its inclusion
  # probabilities, so a PPS stage further down does not justify them.
  first_kind <- survey_stage_kind(design$stages[[stages_executed[1]]]$draw_spec)
  if (identical(first_kind, "wr")) {
    refuse("Stage 1 is drawn with replacement or minimum replacement, so its
            rows carry expected hits, not the inclusion probabilities
            {.arg pps} needs. Omit {.arg pps} for the with-replacement
            export.")
  }
  first_mos <- design$stages[[stages_executed[1]]]$draw_spec$mos
  pps_first <- identical(first_kind, "pps_wor") ||
    (identical(first_kind, "rs_poisson") && !is_null(first_mos))
  if (!pps_first) {
    refuse("{.arg pps} describes an unequal-probability stage 1, and
            stage 1 of this design is not one. Omit {.arg pps}.")
  }
  # Overton and HR assume a fixed sample size, which a Poisson stage lacks.
  if (identical(first_kind, "rs_poisson") && !inherits(pps, "pps_spec")) {
    refuse("Stage 1 is drawn by Poisson sampling, whose sample size is
            random. {.code \"overton\"} and {.fn survey::HR} assume a
            fixed size and would leave that part out of the variance.
            Omit {.arg pps} for the exact Poisson variance, or pass
            {.fn survey::ppsmat} or {.fn survey::poisson_sampling}.")
  }
  # poisson_sampling() is ppscov() on a diagonal matrix: independent
  # selections, which a fixed-size stage does not have.
  poisson_spec <- inherits(pps, "ppsdcheck") &&
    inherits(pps$dcheck, "ddiMatrix")
  if (identical(first_kind, "pps_wor") && poisson_spec) {
    refuse("Stage 1 selects a fixed number of units, so its selections are
            not independent. {.fn survey::poisson_sampling} treats them as
            independent and would add the variance of a random sample
            size. Omit {.arg pps} for Brewer's approximation, or pass
            {.code \"overton\"}, {.fn survey::HR} or
            {.fn survey::ppsmat}.")
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
                                                           "error"),
                                   stages = NULL) {
  systematic_variance <- with_error_class(
    rlang::arg_match(systematic_variance),
    "samplyr_error_survey_argument"
  )
  check_single_replicate(x, "as_svydesign")
  check_sample_unmodified(x, "as_svydesign", provisional_ok = TRUE)
  check_reserve_settled(x, "as_svydesign")
  rlang::check_installed(
    "survey",
    version = "4.5",
    reason = "to convert a tbl_sample to a survey design object."
  )

  if (holds_settled_reserve(x)) {
    check_reserve_export_route(x, "as_svydesign")
    check_stages_ultimate_only(stages, "a settled reserve")
    check_forwarded_args(
      enquos(...),
      owned = c("nest", "method", "systematic_variance", "stages"),
      accepted = svydesign_accepted_args,
      derived = svydesign_derived_args,
      forwarded_to = "survey::svydesign"
    )
    if (!is_null(method)) {
      cli_abort(
        "{.arg method} is only valid when converting a two-phase sample.",
        class = "samplyr_error_survey_argument"
      )
    }
    if (!is_null(list(...)[["pps"]])) {
      abort_samplyr(
        c(
          "{.arg pps} is not available for a settled reserve.",
          "i" = "A settled reserve is exported by ultimate clusters, which
                 treats the primary units as drawn with replacement, so no
                 inclusion probability enters its variance."
        ),
        class = "samplyr_error_survey_argument"
      )
    }
    systematic_stages <- systematic_approximated_stages(
      get_design(x), get_stages_executed(x)[1], as.data.frame(x)
    )
    check_systematic_variance(
      systematic_stages,
      systematic_variance,
      approximation = "ultimate_cluster"
    )
    return(record_systematic_variance(
      build_reserve_svydesign(
        x, nest = nest, dots = list(...), fn_name = "as_svydesign"
      ),
      systematic_stages, systematic_variance, "ultimate_cluster"
    ))
  }

  # Export shared weights through source-target contributions.
  if (identical(sample_weight_contract(x), "shared")) {
    check_no_export_stages(stages, "a sample with shared weights")
    check_forwarded_args(
      enquos(...),
      owned = c("nest", "method", "systematic_variance", "stages"),
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

  check_prev_phase_unmodified(x, "as_svydesign")
  composed <- composed_subsample(x)
  if (!is_null(composed)) {
    if (!is_null(method)) {
      abort_samplyr(
        c(
          "{.arg method} is only valid when converting a two-phase sample.",
          "i" = if (phase1_is_fixed_one_unit(
            get_prev_phase(x)$sample
          )) {
            "Phase 1 could select nothing else, so this second phase
             exports as a sample of its units, not through
             {.fn survey::twophase}."
          } else {
            "This second phase draws within each final-stage pool of
             phase 1, so it exports as phase 1's multistage design with a
             smaller final take, not through {.fn survey::twophase}."
          }
        ),
        class = "samplyr_error_survey_argument"
      )
    }
    x <- composed
  }

  phase_info <- survey_validate_phase_support(
    x,
    allow_twophase = TRUE,
    fn_name = "as_svydesign"
  )
  prev_phase <- phase_info$prev_phase
  is_twophase <- phase_info$is_twophase
  is_activation <- survey_is_activation(phase_info)
  occasion_route <- occasion_export_route(x, phase_info, method)
  if (is_occasion_window(x) && !is_null(method)) {
    abort_samplyr(
      c(
        "{.arg method} gives the two-phase export of a single occasion.",
        "x" = "This sample pools
               {sample_occasion_phrase(attr(x, \"metadata\"))}, and a
               window of occasions is exported by ultimate clusters only."
      ),
      class = "samplyr_error_survey_argument"
    )
  }
  if (identical(occasion_route, "twophase") &&
      isTRUE(attr(x, "metadata")[["occasion"]]$provisional)) {
    abort_samplyr(
      c(
        "{.arg method} asks for the two-phase export, which reads the whole
         master as the first phase.",
        "x" = "This master's later stages are drawn one occasion at a time, and
               some are not drawn yet.",
        "i" = "Without {.arg method}, the occasion is exported by ultimate
               clusters from its own rows."
      ),
      class = "samplyr_error_occasion_provisional"
    )
  }
  # An occasion exported as one phase goes to survey::svydesign().
  to_twophase <- is_twophase &&
    !identical(occasion_route, "ultimate_cluster")

  if (identical(occasion_route, "ultimate_cluster")) {
    check_stages_ultimate_only(stages, "an occasion")
  } else if (is_twophase) {
    check_no_export_stages(stages, "a two-phase sample")
  } else {
    stages <- check_export_stages(stages, x)
    if (!is_null(stages) && !is_null(list(...)[["pps"]])) {
      abort_samplyr(
        c(
          "{.arg pps} is not available with {.arg stages}.",
          "i" = "The last kept stage is read as drawn with replacement, so
                 its inclusion probabilities do not enter the variance."
        ),
        class = "samplyr_error_survey_argument"
      )
    }
  }

  # `subset` belongs only to the two-phase branch.
  check_forwarded_args(
    enquos(...),
    owned = c("nest", "method", "systematic_variance", "stages"),
    accepted = if (to_twophase) {
      twophase_accepted_args
    } else {
      svydesign_accepted_args
    },
    derived = if (to_twophase) {
      twophase_derived_args
    } else {
      svydesign_derived_args
    },
    forwarded_to = if (to_twophase) "survey::twophase" else "survey::svydesign"
  )

  if (identical(occasion_route, "ultimate_cluster") &&
      !is_null(list(...)[["pps"]])) {
    abort_samplyr(
      c(
        "{.arg pps} is not available for an occasion's ultimate-cluster export.",
        "i" = "The export treats the occasion's primary units as drawn with
               replacement, so no inclusion probability enters its
               variance.",
        "i" = "Give {.arg method} for the two-phase export of a master drawn
               with equal probabilities."
      ),
      class = "samplyr_error_survey_argument"
    )
  }

  # `nest` has no effect on a two-phase export.
  if (to_twophase && !missing(nest)) {
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
  # Single-stage pps specifications drop stages below stage 1, and a matrix or
  # HR object also replaces Brewer's approximation at stage 1.
  user_pps <- list(...)[["pps"]]
  if (!is_twophase && is_null(stages) &&
      (identical(user_pps, "overton") ||
         inherits(user_pps, c("pps_spec", "HR")))) {
    systematic_stages <- Filter(function(s) {
      identical(s$stage, stages_executed[1]) &&
        !(inherits(user_pps, c("pps_spec", "HR")) &&
            identical(s$method, "pps_systematic"))
    }, systematic_stages)
  }
  if (identical(occasion_route, "ultimate_cluster")) {
    # Only the master's first stage enters the ultimate-cluster variance.
    systematic_stages <- systematic_approximated_stages(
      prev_phase$design %||% get_design(prev_phase$sample),
      (prev_phase$stages %||% get_stages_executed(prev_phase$sample))[1],
      df
    )
  } else if (is_twophase) {
    previous <- prev_phase$sample
    systematic_stages <- c(
      systematic_approximated_stages(
        prev_phase$design %||% get_design(previous),
        prev_phase$stages %||% get_stages_executed(previous),
        as.data.frame(previous),
        phase = 1L
      ),
      # An occasion's stages are its master's, and activation is not systematic.
      if (!is_activation) systematic_stages
    )
  }
  if (!is_null(stages) && !is_twophase) {
    systematic_stages <- stage_export_systematic_stages(
      x, design, stages_executed, stages
    )
  }
  approximation <- if (identical(occasion_route, "ultimate_cluster") ||
                         identical(stages, 1L)) {
    "ultimate_cluster"
  } else {
    "srswor"
  }
  # A two-phase build refuses whole method families, so the policy is
  # checked after it, and a refused export carries no approximation advice.
  check_systematic <- function() {
    check_systematic_variance(
      systematic_stages,
      systematic_variance,
      approximation = approximation
    )
  }
  twophase_build <- is_twophase && !is_activation &&
    !identical(occasion_route, "ultimate_cluster")
  if (!twophase_build) {
    check_systematic()
  }

  record_systematic <- function(result) {
    record_systematic_variance(
      result, systematic_stages, systematic_variance, approximation
    )
  }

  if (identical(occasion_route, "ultimate_cluster")) {
    return(record_systematic(build_activation_ultimate_cluster(
      x,
      prev_phase = prev_phase,
      nest = nest,
      dots = list(...)
    )))
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
    result <- build_twophase_svydesign(
      df = export_rows(x, design, stages_executed),
      design = design,
      stages_executed = stages_executed,
      prev_phase = prev_phase,
      method = method,
      dots = list(...)
    )
    check_systematic()
    record_systematic(result)
  } else {
    record_systematic(build_singlephase_svydesign(
      x,
      dots = list(...),
      nest = nest,
      relax_pps_for_bootstrap = FALSE,
      stages = stages
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
  prepared <- twophase_svydesign_args(
    df, design, stages_executed, prev_phase, method, dots, call = call
  )
  result <- survey_twophase(prepared$args, call = call)
  if (prepared$joint_route) {
    lonely2 <- twophase_phase2_lonely(
      prepared$spec2, prepared$design2, prepared$row2
    )
    result <- twophase_phase2_syg(
      result,
      prepared$n2,
      lonely = lonely2,
      call = call
    )
  }
  result
}

#' The arguments of survey::twophase() for a two-phase sample
#'
#' Everything `build_twophase_svydesign()` decides before calling survey,
#' its refusals and warnings included, so `variance_estimators()` can read
#' the same arguments without calling it.
#' @return A list of `args`, and of what the joint-probability route needs
#'   afterwards: `joint_route`, `spec2`, `design2`, `row2` and `n2`.
#' @noRd
twophase_svydesign_args <- function(
  df,
  design,
  stages_executed,
  prev_phase,
  method,
  dots,
  call = caller_env()
) {
  phase1 <- prev_phase$sample
  phase1_status <- sample_realization_status(phase1)
  design1 <- prev_phase$design %||% get_design(phase1)
  stages1 <- prev_phase$stages %||% get_stages_executed(phase1)
  # A phase-1 unit found empty is a zero row that phase 2 did not reach.
  # Phase 2's design names phase 1's hits by their carried names.
  df1 <- with_phase_draw_columns(
    export_rows(phase1, design1, stages1, call = call), phase1
  )
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

  # Refused before the bridge, so a linkage problem is not reported instead.
  is_poisson <- function(e) identical(e$kind, "rs_poisson")
  if (any(vapply(c(spec1$stage, spec2$stage), is_poisson, logical(1)))) {
    abort_samplyr(c(
      "Two-phase export does not support Poisson sampling in either phase.",
      "i" = "The current two-phase bridge cannot represent the random sample-size variance."
    ), class = "samplyr_error_twophase_poisson", call = call)
  }

  row_col <- phase_row_column(phase1)
  bridge_vars <- if (phase_rows_linked(df2, row_col, nrow(phase1))) {
    # Zero rows for empty units follow the phase-1 rows and link nothing.
    df1[[row_col]] <- c(
      seq_len(nrow(phase1)),
      rep(NA_integer_, nrow(df1) - nrow(phase1))
    )
    row_col
  } else {
    resolve_phase_bridge(
      survey_key_vars(design1, stages1, df1),
      survey_key_vars(design2, stages_executed, df2),
      df1,
      df2,
      call = call
    )
  }

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
  if (!use_weights && twophase_subsamples_wr_units(
    df1, spec1, design1, stages1, design2, stages_executed,
    complete = phase1_status$ok
  )) {
    abort_twophase_wr_subsample(design1, stages1,
                                complete = phase1_status$ok, call = call)
  }

  # One probability term per ID stage unless the FPCs carry it.
  n_id_stages <- max(
    length(id_info1$id_vars), length(id_vars2)
  )
  certain_wr1 <- phase_wr_stages_certain(phase1, design1, stages1)
  fpc_covers_stages <- spec_fpc_states_probabilities(
    spec1, id_info1$stage_indices, fpc1$scale, certain_wr1
  ) &&
    spec_fpc_states_probabilities(
      spec2, id_info2$stage_indices, fpc2$scale
    )

  # A phase 2 taking every phase-1 row adds no variance, and survey cannot
  # read its all-one probabilities next to a matrix.
  if (joint_route && all(in_phase2) &&
      all(is_certainty_probability(1 / df_combined[[cond_col]]))) {
    joint_route <- FALSE
  }

  if (joint_route) {
    # survey reads phase 2's probabilities off `probs` next to the matrix.
    phase1_covered <- spec_fpc_states_probabilities(
      spec1, id_info1$stage_indices, fpc1$scale, certain_wr1
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
  if (use_weights && twophase_approx_overstates(
    df1, spec1, design2, stages_executed,
    all_rows = !anyNA(vctrs::vec_match(
      df1[seq_len(nrow(phase1)), bridge_vars, drop = FALSE],
      df2[, bridge_vars, drop = FALSE]
    ))
  )) {
    warn_twophase_approx(method, design1, stages1, call = call)
  }
  list(
    args = args,
    joint_route = joint_route,
    spec2 = spec2,
    design2 = design2,
    row2 = row2,
    n2 = sum(in_phase2)
  )
}

#' Non-certainty phase-2 rows left alone in their phase-2 stratum
#' @noRd
twophase_phase2_lonely <- function(spec2, design2, row2) {
  entry <- spec2$stage[[1]]
  n2 <- length(row2)
  if (n2 == 0L) {
    return(integer(0))
  }
  stratum <- (entry$strata$id %||% rep(1L, spec2$n_rows))[row2]
  unit <- group_ids(
    data.frame(stratum = stratum, unit = entry$unit$id[row2]),
    c("stratum", "unit")
  )
  sure <- export_stage_sure(
    list(
      prob = if (!is_null(entry$prob)) entry$prob[row2],
      certainty = if (!is_null(entry$certainty)) entry$certainty[row2]
    ),
    design2$stages[[entry$stage]]$draw_spec,
    n2
  )
  non_cert <- !sure
  if (!any(non_cert)) {
    return(integer(0))
  }
  first <- non_cert & !duplicated(unit)
  n_units <- tabulate(stratum[first], nbins = max(c(0L, stratum)))
  which(non_cert & n_units[stratum] == 1L)
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
twophase_phase2_syg <- function(result, n2, lonely = integer(0),
                                call = caller_env()) {
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
  diag_orig <- Matrix::diag(dcheck$phase2)
  lower <- Matrix::Diagonal(x = Matrix::rowSums(dcheck$phase2))
  phase2_syg <- Matrix::forceSymmetric(dcheck$phase2 - lower)
  full_syg <- Matrix::forceSymmetric(dcheck$full - lower)
  if (length(lonely) > 0L) {
    n_lonely <- length(lonely)
    opt <- getOption("survey.lonely.psu", "fail")
    if (identical(opt, "fail")) {
      abort_samplyr(
        c(
          "In phase 2, {n_lonely} strat{?um/a} hold{?s/} a single
           non-certainty sampled unit.",
          "x" = "The Sen-Yates-Grundy variance has no pair to compare in a
                 one-unit stratum, and {.code survey.lonely.psu = \"fail\"}
                 refuses one.",
          "i" = "Set {.code options(survey.lonely.psu = \"adjust\")} for
                 grand-mean centering, {.code \"certainty\"} or
                 {.code \"remove\"} to drop that stratum's phase-2
                 contribution, or allocate at least two phase-2 units per
                 stratum."
        ),
        class = "samplyr_error_lonely_psu",
        call = call
      )
    } else if (opt %in% c("certainty", "remove")) {
      # Zero row and column already drop the singleton's phase-2 term.
    } else if (identical(opt, "adjust")) {
      if (n2 < 2L) {
        abort_samplyr(
          c(
            "Cannot apply {.code survey.lonely.psu = \"adjust\"} to a
             phase-2 sample of a single unit.",
            "i" = "Grand-mean centering needs at least two phase-2 units."
          ),
          class = "samplyr_error_lonely_psu",
          call = call
        )
      }
      B <- Matrix::Matrix(-1 / n2, nrow = n2, ncol = n_lonely)
      B[cbind(lonely, seq_len(n_lonely))] <- 1 - 1 / n2
      adj <- B %*% Matrix::Diagonal(x = diag_orig[lonely]) %*% Matrix::t(B)
      phase2_syg <- Matrix::forceSymmetric(phase2_syg + adj)
      full_syg <- Matrix::forceSymmetric(full_syg + adj)
    } else {
      abort_samplyr(
        c(
          "{.code survey.lonely.psu = {.val {opt}}} is not supported for a
           PPS phase-2 export.",
          "x" = "{.fn survey::twophase} evaluates a data-independent check
                 matrix, which cannot average realized variances from other
                 strata.",
          "i" = "Use {.val adjust}, {.val certainty}, {.val remove}, or
                 {.val fail}."
        ),
        class = "samplyr_error_lonely_psu",
        call = call
      )
    }
  }
  result$dcheck$phase2 <- phase2_syg
  result$dcheck$full <- full_syg
  result
}

#' Restate a census correction that survey cannot read
#'
#' survey 4.5 stops on a correction whose entries are all one, which is what
#' a census at every stage gives as sampling fractions, or as population
#' counts when each stratum holds one unit. Such a design has no sampling
#' variance. Its correction is restated as counts equal to the units
#' sampled in each stratum and stage, which survey reads as a census. When
#' every stratum holds one unit, as when each certainty unit is its own
#' stratum, the strata are dropped and the units keep survey's nested
#' labels, since strata change neither the estimates of a census nor their
#' zero variance. A single primary unit keeps its stratum, which survey
#' needs to accept it. When its counts are all one too, as for a census of
#' one element, no correction can state it, and `certain` asks the caller
#' to mark the unit as taken with certainty. Any other correction is left
#' as it is, so the export stays right once survey reads all-one values.
#' @param rows The rows survey builds this design on.
#' @return `NULL` when `fpc` needs no change, else a list of `data`, `fpc`
#'   (`NULL` when `certain`), `ids` (`NULL` when unchanged), `strata` and
#'   `certain`.
#' @noRd
census_fpc_restated <- function(ids, strata, fpc, data, rows = NULL,
                                call = caller_env()) {
  if (!inherits(fpc, "formula")) {
    return(NULL)
  }
  rows <- rows %||% seq_len(nrow(data))
  if (length(rows) == 0L) {
    return(NULL)
  }
  used <- data[rows, , drop = FALSE]
  values <- as.matrix(stats::model.frame(fpc, data = used,
                                         na.action = stats::na.pass))
  if (!isTRUE(all(values == 1))) {
    return(NULL)
  }
  # survey refuses one unstratified primary unit before reading its FPC.
  one_psu <- count_survey_psus(ids, strata, used) < 2L
  if (one_psu && is_null(strata)) {
    return(NULL)
  }
  probe <- function(ids, strata) {
    survey::svydesign(ids = ids, strata = strata,
                      weights = rep(1, length(rows)), data = used,
                      nest = TRUE)
  }
  design <- probe(ids, strata)
  counts <- design$fpc$sampsize[, seq_len(ncol(values)), drop = FALSE]
  new_ids <- NULL
  if (all(counts == 1) && !is_null(strata) && !one_psu) {
    units <- character(0)
    for (k in seq_len(ncol(design$cluster))) {
      unit <- free_name(c(names(data), units), paste0(".census_unit_", k))
      data[[unit]] <- NA_integer_
      data[[unit]][rows] <- as.integer(factor(design$cluster[[k]]))
      units <- c(units, unit)
    }
    used <- data[rows, , drop = FALSE]
    new_ids <- survey_formula_from_vars(units)
    counts <- probe(new_ids, NULL)$fpc$sampsize[, seq_len(ncol(values)),
                                                 drop = FALSE]
  }
  if (all(counts == 1)) {
    return(list(data = data, fpc = NULL, ids = NULL, strata = strata,
                certain = TRUE))
  }
  terms <- character(0)
  for (k in seq_len(ncol(counts))) {
    term <- free_name(names(data), paste0(".fpc_census_", k))
    data[[term]] <- 1
    data[[term]][rows] <- counts[, k]
    terms <- c(terms, term)
  }
  list(data = data, fpc = survey_formula_from_vars(terms), ids = new_ids,
       strata = if (!is_null(new_ids)) NULL else strata, certain = FALSE)
}

#' Number of primary units survey sees, nested in first-stage strata
#'
#' Only the first terms count: a later stage's strata divide a primary unit,
#' they do not make several.
#' @noRd
count_survey_psus <- function(ids, strata, data) {
  if (is_null(ids) || length(all.vars(ids)) == 0L) {
    return(nrow(data))
  }
  # The variable, not the term label, which keeps a non-syntactic name's
  # backticks.
  first_strata <- if (is_null(strata)) {
    character(0)
  } else {
    all.vars(attr(stats::terms(strata), "variables")[[2]])
  }
  nrow(unique(data[c(first_strata, all.vars(ids)[1])]))
}

#' Let survey accept a single primary unit taken with certainty
#'
#' survey refuses a design with one primary unit unless that unit is nested
#' in a stratum. When the unit is the whole first stage, its correction is
#' one, and survey then reads its stratum as taken with certainty: no
#' first-stage variance, and the later stages' variance within it. One
#' unit drawn from several stays refused by survey.
#' @noRd
single_certain_psu <- function(args) {
  if (!inherits(args$fpc, "formula")) {
    return(args)
  }
  data <- args$data
  first <- stats::model.frame(args$fpc, data = data,
                              na.action = stats::na.pass)[[1]]
  if (count_survey_psus(args$ids, args$strata, data) != 1L ||
      !isTRUE(all(first == 1))) {
    return(args)
  }
  if (is_null(args$strata)) {
    stratum <- free_name(names(data), ".census_stratum")
    args$data[[stratum]] <- 1L
    args$strata <- survey_formula_from_vars(stratum)
  }
  args$nest <- TRUE
  args
}

#' Call survey::svydesign(), restating a census correction first
#'
#' A census has no variance under any formula, so `pps` is dropped with it.
#' A unit no correction can state is marked as taken with certainty, its
#' population size set to its sample size as survey reads a certainty
#' stratum.
#' @noRd
survey_svydesign <- function(args, call = caller_env()) {
  args <- single_certain_psu(args)
  restated <- census_fpc_restated(args$ids, args$strata, args$fpc,
                                  args$data, call = call)
  if (!is_null(restated)) {
    args$data <- restated$data
    args["fpc"] <- list(restated$fpc)
    args$ids <- restated$ids %||% args$ids
    args["strata"] <- list(restated$strata)
    args$pps <- FALSE
    args$variance <- NULL
  }
  # survey refuses one primary unit outside a stratum, so the refusal
  # carries a class and its alternative.
  if (is_null(args$strata) &&
      count_survey_psus(args$ids, NULL, args$data) == 1L) {
    abort_one_primary_unit("The sample", call = call)
  }
  result <- do.call(survey::svydesign, args)
  if (isTRUE(restated$certain)) {
    result$fpc$popsize <- result$fpc$sampsize
  }
  result$call <- survey_export_call("svydesign", args)
  result
}

#' Refuse a design with a single primary unit
#'
#' survey refuses a design with one primary unit outside a stratum, with an
#' unclassed error. One phase passes it a census of that unit as a
#' certainty stratum, so only a unit not taken with certainty is refused
#' there, a single hit of a draw with replacement included.
#' survey's two-phase design refuses a phase 1 of one unit even when that
#' unit is taken with certainty (`certain`).
#' @noRd
abort_one_primary_unit <- function(what, certain = FALSE,
                                   call = caller_env()) {
  message <- if (certain) {
    c(
      "{what} holds a single primary unit, taken with certainty.",
      "x" = "{.pkg survey}'s two-phase design needs at least two phase-1
             primary units, and refuses one even when it is taken with
             certainty.",
      "i" = "The weights in {.field .weight} are exact. For a variance,
             draw phase 2 within each final-stage pool of phase 1, which is
             exported as one phase, or replicate the whole design with
             {.code execute(reps = )} and use
             {.code as_svrepdesign(type = \"random_groups\")}."
    )
  } else {
    c(
      "{what} holds a single primary unit, not taken with certainty.",
      "x" = "A variance compares primary units, and {.pkg survey} refuses a
             design with only one primary sampling unit.",
      "i" = "The weights in {.field .weight} are exact. For a variance, draw
             at least two first-stage units, or replicate the whole design
             with {.code execute(reps = )} and use
             {.code as_svrepdesign(type = \"random_groups\")}."
    )
  }
  abort_samplyr(message, class = "samplyr_error_export_one_unit",
                call = call)
}

#' Refuse a phase 2 that survey's two-phase design cannot build
#'
#' A phase 2 of a single primary unit (`within_one = FALSE`), or one whose
#' rows all fall in a single phase-1 primary unit (`within_one = TRUE`),
#' as can happen when phase 2 is drawn across the phase-1 units.
#' @noRd
abort_twophase_phase2_one_unit <- function(within_one, call = caller_env()) {
  message <- if (within_one) {
    c(
      "Phase 2's rows all fall in a single phase-1 primary unit.",
      "x" = "{.pkg survey}'s two-phase design reads phase 1's design on
             phase 2's rows, and refuses it with only one primary sampling
             unit.",
      "i" = "The weights in {.field .weight} are exact. For a variance,
             draw phase 2 within each phase-1 unit, with {.fn stratify_by}
             on its identifier, which is exported as one phase, or
             replicate the whole design with {.code execute(reps = )} and
             use {.code as_svrepdesign(type = \"random_groups\")}."
    )
  } else {
    c(
      "Phase 2 holds a single primary unit.",
      "x" = "{.pkg survey}'s two-phase design refuses a phase 2 with only one
             primary sampling unit outside a stratum.",
      "i" = "The weights in {.field .weight} are exact. For a variance,
             draw at least two phase-2 units, or replicate the whole design
             with {.code execute(reps = )} and use
             {.code as_svrepdesign(type = \"random_groups\")}."
    )
  }
  abort_samplyr(message, class = "samplyr_error_export_one_unit",
                call = call)
}

#' Call survey::twophase(), restating each phase's census correction first
#'
#' What survey cannot build around a phase-1 zero row is refused first.
#' @noRd
survey_twophase <- function(args, call = caller_env()) {
  restated <- survey_twophase_args(args, call = call)
  args <- restated$args
  check_twophase_zero_rows(args, restated$subset, call = call)
  if (is_null(args$strata[[1]]) &&
      count_survey_psus(args$id[[1]], NULL, args$data) == 1L) {
    # A first-stage correction of one is a census of that unit.
    first <- if (inherits(args$fpc[[1]], "formula")) {
      stats::model.frame(args$fpc[[1]], data = args$data,
                         na.action = stats::na.pass)[[1]]
    }
    abort_one_primary_unit("Phase 1", certain = isTRUE(all(first == 1)),
                           call = call)
  }
  # survey builds the phase-2 design, and phase 1's design on phase 2's
  # rows, and refuses either with one primary unit outside a stratum.
  rows2 <- args$data[restated$subset, , drop = FALSE]
  if (is_null(args$strata[[2]]) &&
      count_survey_psus(args$id[[2]], NULL, rows2) == 1L) {
    abort_twophase_phase2_one_unit(within_one = FALSE, call = call)
  }
  if (is_null(args$strata[[1]]) &&
      count_survey_psus(args$id[[1]], NULL, rows2) == 1L) {
    abort_twophase_phase2_one_unit(within_one = TRUE, call = call)
  }
  result <- twophase_phase1_aligned(do.call(survey::twophase, args))
  result$call <- survey_export_call("twophase", args)
  result
}

#' The arguments survey::twophase() receives, each phase's census restated
#'
#' Phase 2's correction is read on the phase-2 rows only, as survey reads it.
#' @return A list of `args` and `subset`, which rows are in phase 2.
#' @noRd
survey_twophase_args <- function(args, call = caller_env()) {
  subset <- stats::model.frame(args$subset, data = args$data)[[1]]
  rows <- list(NULL, which(subset %in% TRUE))
  for (k in 1:2) {
    restated <- census_fpc_restated(args$id[[k]], args$strata[[k]],
                                    args$fpc[[k]], args$data,
                                    rows = rows[[k]], call = call)
    # A phase of one unit no correction states is left to survey.
    if (is_null(restated) || restated$certain) {
      next
    }
    args$data <- restated$data
    args$fpc[k] <- list(restated$fpc)
    if (!is_null(restated$ids)) args$id[[k]] <- restated$ids
    args$strata[k] <- list(restated$strata)
  }
  list(args = args, subset = subset %in% TRUE)
}

#' Refuse what survey::twophase() cannot build around a phase-1 zero row
#'
#' The zero row of an empty unit keeps its zero total in the phase-1
#' variance, and phase 2 could never draw it. survey reads every phase-1 row
#' as a unit phase 2 might have drawn, and stops in three places, which are
#' checked here in its order and only where the zero rows cause them. It
#' builds phase 1 again on the phase-2 rows, so a phase 2 inside one primary
#' unit whose only other unit is empty has a single one. It reads a phase-2
#' census beside the joint probabilities as an all-one correction it cannot
#' parse. And it needs every phase-2 stratum value of a phase-1 row to have a
#' phase-2 row, where the zero row has a value of its own or none. The
#' selection is valid in each case, and the export does not represent it
#' yet.
#' @param subset Which rows of `args$data` are in phase 2.
#' @noRd
check_twophase_zero_rows <- function(args, subset, call = caller_env()) {
  data <- args$data
  zero <- data$.weight %in% 0 & !subset
  if (!any(zero)) {
    return(invisible(NULL))
  }
  # The first variable of a formula, as survey reads it.
  first_var <- function(f) {
    vars <- if (inherits(f, "formula")) attr(stats::terms(f), "variables")
    if (length(vars) < 2L) NULL else eval(vars[[2]], data)
  }
  refuse <- function(why) {
    abort_samplyr(
      c(
        "survey's two-phase design cannot represent the empty unit this
         phase 1 recorded.",
        "x" = why,
        "i" = "The empty unit is a zero row of phase 1, which keeps its
               zero total in the phase-1 variance. The sample is valid.",
        "i" = "Totals and means from {.field .weight} are right. This
               export does not represent this configuration yet."
      ),
      class = "samplyr_error_twophase_empty_unit",
      call = call
    )
  }
  psu <- first_var(args$id[[1]])
  if (!is_null(psu) && length(unique(psu[subset])) == 1L &&
        length(unique(psu[!zero])) == 1L) {
    refuse("Phase 2 lies in one primary unit, and every other one phase 1
            selected is empty. survey builds phase 1 again on the phase-2
            rows and stops on a single primary unit.")
  }
  prob2 <- first_var(args$probs[[2]])
  if (!is_null(args$pps[[2]]) && !is_null(prob2) &&
        all(is_certainty_probability(prob2[subset]))) {
    refuse("Phase 2 took every unit it could draw, but not the zero row,
            and survey cannot read that census beside the phase-2 joint
            probabilities.")
  }
  stratum <- first_var(args$strata[[2]])
  if (!is_null(stratum)) {
    lost <- setdiff(unique(stratum), unique(stratum[subset]))
    if (length(lost) > 0L && all(zero[stratum %in% lost])) {
      refuse("The zero row's phase-2 stratum holds no unit phase 2 could
              draw, and survey stops on a phase-2 stratum with no phase-2
              row.")
    }
  }
  invisible(NULL)
}

#' Rebuild the phase-1 term of a two-phase variance
#'
#' survey 4.5 counts the units of each phase-1 stratum over every phase-1
#' row but reads those counts by position among the phase-2 rows. Once
#' phase 2 drops units unevenly, later strata take a neighbor's count, the
#' matrix stops cancelling a constant, and the variance of a total depends
#' on the outcome's level. It can then be negative in every sample. The
#' phase-1 matrix is rebuilt here by survey's own formula with the counts
#' aligned, and the full matrix recombined from it as survey combines them.
#' Where the counts were already aligned the result is survey's own, so the
#' export stays right once survey corrects this.
#' @noRd
twophase_phase1_aligned <- function(result) {
  design1 <- result$phase1$full
  if (
    !inherits(result, "twophase2") ||
      !is.null(design1$dcheck) ||
      isTRUE(design1$phase1_is_iid)
  ) {
    return(result)
  }
  subset <- result$subset
  n <- sum(subset)
  with_replacement <- is.null(design1$fpc$popsize)
  phase1 <- Matrix::sparseMatrix(
    i = integer(), j = integer(), x = numeric(), dims = c(n, n)
  )
  for (stage in seq_len(NCOL(design1$cluster))) {
    id <- design1$cluster[, stage]
    strata <- design1$strata[, stage]
    first <- rep(FALSE, length(id))
    first[subset] <- !duplicated(id[subset])
    count <- stats::ave(seq_along(id), strata, FUN = function(k) {
      rep(length(unique(id[k])), length(k))
    })
    stage_check <- stratified_dcheck(
      strata[first],
      design1$allprob[first, stage],
      count[first],
      with_replacement
    )
    expand <- Matrix::sparseMatrix(
      i = seq_len(n),
      j = match(id[subset], id[first]),
      x = 1,
      dims = c(n, sum(first))
    )
    this_stage <- expand %*% stage_check %*% Matrix::t(expand)
    phase1 <- phase1 + this_stage - phase1 * this_stage
  }
  phase2 <- result$dcheck$phase2
  result$dcheck$phase1 <- phase1
  result$dcheck$full <- phase1 + phase2 - phase1 * phase2
  result
}

#' One stage's phase-1 check matrix, by stratum
#'
#' Units of a stratum pair at `-(1 - p) / (n - 1)` under sampling without
#' replacement, and the diagonal is `1 - p`, as survey writes it.
#' @noRd
stratified_dcheck <- function(strata, prob, count, with_replacement) {
  n <- length(strata)
  pairs <- if (with_replacement) {
    list(i = integer(), j = integer())
  } else {
    members <- split(seq_len(n), strata)
    list(
      i = unlist(lapply(members, function(k) rep(k, times = length(k)))),
      j = unlist(lapply(members, function(k) rep(k, each = length(k))))
    )
  }
  off <- pairs$i != pairs$j
  i <- pairs$i[off]
  Matrix::sparseMatrix(
    i = c(i, seq_len(n)),
    j = c(pairs$j[off], seq_len(n)),
    x = c(-(1 - prob[i]) / (count[i] - 1), 1 - prob),
    dims = c(n, n)
  )
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
  omit_uncorrected_fpc = FALSE,
  stages = NULL
) {
  rlang::local_error_call(caller_env())
  design <- get_design(x)
  stages_executed <- get_stages_executed(x)
  df <- export_rows(x, design, stages_executed)
  if (!is_null(stages)) {
    return(build_stages_svydesign(
      x, df, design, stages_executed, stages,
      dots = dots, nest = nest,
      relax_pps_for_bootstrap = relax_pps_for_bootstrap,
      check_lonely = check_lonely
    ))
  }

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
  if (inherits(pps_arg, "ppsmat") &&
      identical(
        survey_stage_kind(design$stages[[stages_executed[1]]]$draw_spec),
        "pps_wor"
      ) &&
      !"variance" %in% names(dots)) {
    dots[["variance"]] <- "YG"
  }

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
  result <- survey_svydesign(args)
  if (check_lonely) {
    warn_lonely_strata(spec, design, id_info$stage_indices,
                       unread = survey_unread_stages(result, spec$n_rows))
  }
  result
}

#' Export the first stages of a sample, the last read as drawn with
#' replacement
#'
#' Stages 1 to k - 1 keep their corrections. Stage k is drawn with
#' replacement within its parents and carries every stage below it, the
#' ultimate-cluster variance when k is 1. Its units taken with probability 1
#' are read by `stage_export_units()`.
#' @param stages The kept stages, from `check_export_stages()`.
#' @noRd
build_stages_svydesign <- function(x, df, design, executed, stages, dots,
                                   nest, relax_pps_for_bootstrap = FALSE,
                                   check_lonely = TRUE) {
  k <- length(stages)
  last <- stages[[k]]
  units <- stage_export_units(df, design, executed, stages,
                              notify = check_lonely)
  check_stage_export_support(
    design, executed, stages, units,
    relax_pps_for_bootstrap = relax_pps_for_bootstrap
  )
  df <- units$df
  spec <- units$spec
  kept <- units$kept
  census <- units$census

  id_info <- spec_survey_ids(spec, df)
  df <- id_info$df
  strata <- spec_survey_strata(spec, df, id_stage_indices = id_info$stage_indices)
  df <- strata$df
  fpc <- survey_fpc_info(df, kept, stages, id_info$stage_indices)
  df <- fpc$df
  if (any(census)) {
    # The stage's term is free-named for it, so it can be edited.
    term <- fpc$fpc_vars[match(last, fpc$stage_indices)]
    df[[term]][census] <- 1
  }
  if (check_lonely) {
    warn_export_empty_parents(x, kept, id_info$stage_indices)
  }
  resolved <- survey_resolve_pps(
    df = df, design = kept, stages_executed = stages, fpc = fpc,
    relax_pps_for_bootstrap = relax_pps_for_bootstrap
  )
  args <- c(
    list(
      ids = survey_ids_formula(id_info$id_vars),
      strata = strata$formula,
      weights = stats::as.formula("~.weight"),
      fpc = resolved$fpc$formula,
      data = resolved$df,
      nest = nest,
      pps = resolved$pps
    ),
    dots[setdiff(names(dots), "pps")]
  )
  # A design read as drawn with replacement needs no correction term.
  if (!any(census) && k == 1L) {
    args$fpc <- NULL
  }
  result <- survey_svydesign(args)
  if (check_lonely) {
    # Only census units are taken whole in the export. A unit moved down a
    # stage or kept as a draw still varies.
    checked <- spec
    checked$stage[[k]]$census_rows <- census
    warn_lonely_strata(checked, kept, id_info$stage_indices,
                       unread = survey_unread_stages(result, spec$n_rows))
  }
  result
}

#' Check support for the last kept stage and any stage promoted below certainty
#' @noRd
check_stage_export_support <- function(design, executed, stages, units,
                                       relax_pps_for_bootstrap = FALSE,
                                       call = caller_env()) {
  rlang::local_error_call(call)
  k <- length(stages)
  last <- stages[[k]]
  checked_stages <- unique(c(stages, units$active_below_stage))
  for (stage_idx in checked_stages) {
    kind <- survey_stage_kind(design$stages[[stage_idx]]$draw_spec)
    if (identical(kind, "rs_poisson")) {
      if (!identical(stage_idx, executed[1])) {
        abort_samplyr(c(
          "Linearization export cannot represent Poisson sampling at later stages.",
          "i" = "Use {.code as_svrepdesign(x, type = \"rwyb\")} to retain the random sample-size variance."
        ), class = "samplyr_error_multistage_poisson_later")
      }
      if (length(executed) > 1L) {
        abort_samplyr(c(
          "{.pkg survey} does not support multi-stage designs with a random-size Poisson method at stage 1.",
          "i" = "{.pkg survey} rejects multi-stage designs when the {.code pps} argument is set.",
          "i" = "Use {.code as_svrepdesign(type = \"rwyb\")} for independent Poisson replication.",
          "i" = "Or convert each stage separately."
        ), class = "samplyr_error_multistage_poisson_stage1")
      }
      abort_samplyr(c(
        "{.code stages = 1} reads the stage as drawn with replacement, which drops Poisson sample-size variance.",
        "i" = "Omit {.arg stages} to export {.fn survey::poisson_sampling} variance."
      ), class = "samplyr_error_survey_argument")
    }
  }

  promoted_check <- unique(c(last, units$promoted_stage))
  unsupported_idx <- promoted_check[vapply(
    promoted_check,
    function(i) {
      identical(survey_stage_kind(design$stages[[i]]$draw_spec), "unsupported")
    },
    logical(1)
  )]
  if (length(unsupported_idx) > 0 && !relax_pps_for_bootstrap) {
    unsupported_methods <- vapply(
      unsupported_idx,
      function(i) design$stages[[i]]$draw_spec$method,
      character(1)
    )
    abort_samplyr(c(
      "Cannot export method{?s} {.val {unsupported_methods}} via {.fn as_svydesign}.",
      "i" = "No linearization variance estimator is available for this method and its declared constraints.",
      "i" = "Use {.code as_svrepdesign(type = \"subbootstrap\")} for a bootstrap approximation."
    ), class = "samplyr_error_custom_random_wor_export")
  }
  invisible(NULL)
}

#' Systematic stages approximated by a stage-truncated export
#' @noRd
stage_export_systematic_stages <- function(x, design, executed, stages) {
  df <- export_rows(x, design, executed, notify = FALSE)
  out <- systematic_approximated_stages(design, stages, df)
  if (length(stages) < length(executed)) {
    units <- stage_export_units(df, design, executed, stages, notify = FALSE)
    if (length(units$promoted_stage) > 0L && any(units$split)) {
      out <- c(out, systematic_approximated_stages(
        design, units$promoted_stage, units$df[units$split, , drop = FALSE]
      ))
    }
  }
  out
}

#' Split certainty parent units by the next stage's strata and units
#'
#' Shared by `stage_export_units()` and `svrep_from_first_stage()`. Combines
#' the lower-stage census check across every stage below the parent with the
#' crossing of certainty parents by the next stage's strata.
#' @noRd
certainty_substage_split <- function(df, parent_label, parent_id, sure,
                                     lower_entries, default_stratum = "1",
                                     allow_drawn = FALSE, pool_id = NULL) {
  n <- nrow(df)
  census <- drawn <- split <- single_below <- cell_census <- rep(FALSE, n)
  if (!any(sure)) {
    return(list(
      census = census, drawn = drawn, split = split, apart = census,
      single_below = single_below, cell_census = cell_census,
      stratum_label = NULL, cell_id = NULL, unit_id = NULL
    ))
  }
  if (length(lower_entries) == 0L) {
    census <- sure
    return(list(
      census = census, drawn = drawn, split = split, apart = census,
      single_below = single_below, cell_census = cell_census,
      stratum_label = paste("certainty", parent_label, sep = "\r"),
      cell_id = parent_id, unit_id = parent_id
    ))
  }

  whole <- rep(TRUE, n)
  for (e in lower_entries) {
    certain <- (e$certainty %||% rep(FALSE, n)) %in% TRUE
    whole <- whole & (if (identical(e$kind, "wr")) certain else
      certain | is_certainty_probability(e$prob %||% rep(0, n)))
  }
  all_whole <- sure &
    as.vector(tapply(whole, parent_id, all)[as.character(parent_id)])

  next_entry <- lower_entries[[1]]
  under <- next_entry$unit$id
  n_under <- tapply(under, parent_id, function(u) length(unique(u[!is.na(u)])))
  single_below <- sure & !all_whole &
    as.vector(n_under[as.character(parent_id)]) < 2L

  if (allow_drawn) {
    drawn <- single_below
    # When a pool holds a certainty unit with one draw below and moving its
    # census neighbors apart would leave that unit alone, keep them in the pool.
    if (any(drawn) && !is_null(pool_id)) {
      drawn_pools <- unique(pool_id[drawn])
      keep_rows <- !sure | drawn
      rescued_pools <- Filter(function(p) {
        in_p <- pool_id == p
        length(unique(parent_id[in_p & keep_rows])) < 2L &&
          any(in_p & all_whole)
      }, drawn_pools)
      if (length(rescued_pools) > 0L) {
        rescue <- pool_id %in% rescued_pools & all_whole
        all_whole[rescue] <- FALSE
        drawn[rescue] <- TRUE
      }
    }
  }

  next_labels <- spec_strata_labels(next_entry, df) %||%
    rep_len(default_stratum, n)
  stratum2 <- paste(next_labels, parent_label, sep = ".")
  stratum_label <- paste("certainty", parent_label, stratum2, sep = "\r")
  cell_id <- group_ids(
    data.frame(parent = parent_id, stratum = next_labels),
    c("parent", "stratum")
  )
  unit_id <- group_ids(
    data.frame(parent = parent_id, unit = under),
    c("parent", "unit")
  )

  candidate <- sure & !all_whole & !drawn
  cell_whole <- rep(FALSE, n)
  if (any(candidate)) {
    by_cell <- tapply(whole[candidate], cell_id[candidate], all)
    cell_whole[candidate] <- as.vector(by_cell[as.character(cell_id[candidate])])
  }
  cell_census <- candidate & cell_whole
  census <- all_whole | cell_census
  split <- candidate & !cell_whole
  apart <- census | split

  list(
    census = census,
    drawn = drawn,
    split = split,
    apart = apart,
    single_below = single_below,
    cell_census = cell_census,
    stratum_label = stratum_label,
    cell_id = cell_id,
    unit_id = unit_id
  )
}

#' The units of the last kept stage, as the stage export reads them
#'
#' A stage-k unit taken with probability 1 is no draw of stage k. With no
#' stage below, or when every stage below took all of its units, it is a
#' census stratum of its own (`census`). When the next stage drew two units
#' or more inside it, it is crossed with the next stage's strata and its
#' next-stage units are the clusters. When the next stage drew one unit
#' inside it, that stratum could not estimate the variance left below it, so
#' the unit stays a draw of its parent's pool (`drawn`). The spread between
#' the units of that pool then stands in for the variance below it, an
#' approximation `notify` reports. Weights and certainty flags are left as
#' recorded.
#' @return A list with the data, the spec, the design with stage k read as
#'   drawn with replacement, and the rows of the census units, the drawn
#'   units, and the units made strata of their own (`apart`).
#' @noRd
stage_export_units <- function(df, design, executed, stages, notify = TRUE) {
  k <- length(stages)
  last <- stages[[k]]
  kept <- with_replacement_stage(design, last)
  spec <- export_stage_spec(df, kept, stages)
  entry <- spec$stage[[k]]
  n <- nrow(df)
  sure <- export_stage_sure(entry, design$stages[[last]]$draw_spec, n)
  below <- k < length(executed)
  # Without clusters, an element stage above another has no unit to read.
  if (below && identical(entry$unit$kind, "element")) {
    abort_survey_midstage_element(last)
  }
  if (!any(sure)) {
    return(list(
      df = df, spec = spec, kept = kept,
      census = rep(FALSE, n), drawn = rep(FALSE, n),
      split = rep(FALSE, n), apart = rep(FALSE, n),
      active_below_stage = integer(0), promoted_stage = integer(0)
    ))
  }
  unit <- stage_path_ids(spec)
  pool_id <- stage_pool_ids(spec)
  lower_stages <- if (below) executed[-seq_len(k)] else integer(0)
  lower_entries <- if (below) {
    export_stage_spec(df, design, lower_stages)$stage
  } else {
    list()
  }
  sub <- certainty_substage_split(
    df = df,
    parent_label = as.character(unit),
    parent_id = unit,
    sure = sure,
    lower_entries = lower_entries,
    allow_drawn = TRUE,
    pool_id = pool_id
  )
  census <- sub$census
  drawn <- sub$drawn
  split <- sub$split
  apart <- sub$apart

  if (any(split)) {
    entry$unit$id[split] <- max(entry$unit$id) + sub$unit_id[split]
    entry$unit$kind <- "cluster"
  }
  if (any(sub$cell_census)) {
    cc <- sub$cell_census
    entry$unit$id[cc] <- max(entry$unit$id) +
      match(sub$cell_id[cc], unique(sub$cell_id[cc]))
    entry$unit$kind <- "cluster"
  }
  if (any(apart)) {
    stratum_col <- free_name(names(df), ".taken_whole")
    apart_key <- character(n)
    whole_unit <- census & !sub$cell_census
    apart_key[whole_unit] <- paste0("c:", unit[whole_unit])
    apart_key[sub$cell_census] <- paste0("cc:", sub$cell_id[sub$cell_census])
    apart_key[split] <- paste0("s:", sub$cell_id[split])
    apart_id <- integer(n)
    apart_id[apart] <- match(apart_key[apart], unique(apart_key[apart]))
    df[[stratum_col]] <- apart_id
    user <- c(entry$strata$user, stratum_col)
    entry$strata <- list(user = user, certainty = NULL,
                         id = group_ids(df, user))
  }
  spec$stage[[k]] <- entry
  if (notify && any(sub$single_below)) {
    inform_stage_certainty_drawn(
      spec$stage[[k]]$name,
      length(unique(unit[sub$single_below]))
    )
  }
  next_stage <- if (below) executed[[k + 1L]] else integer(0)
  list(
    df = df,
    spec = spec,
    kept = kept,
    census = census,
    drawn = drawn,
    split = split,
    apart = apart,
    active_below_stage = if (below && any(sure & !census)) next_stage else integer(0),
    promoted_stage = if (below && any(split)) next_stage else integer(0)
  )
}

#' Ids of the last kept stage's selection pools across parent stages
#' @noRd
stage_pool_ids <- function(spec) {
  k <- length(spec$stage)
  prior <- if (k > 1L) {
    unlist(lapply(spec$stage[seq_len(k - 1L)], function(e) {
      list(e$strata$id %||% rep(1L, spec$n_rows), e$unit$id)
    }), recursive = FALSE)
  } else {
    list()
  }
  path <- c(prior, list(spec$stage[[k]]$strata$id %||% rep(1L, spec$n_rows)))
  names(path) <- paste0("v", seq_along(path))
  as.integer(vctrs::vec_group_id(vctrs::new_data_frame(path)))
}

#' Ids of the last kept stage's units, unique across their parents
#'
#' Cluster ids may repeat across strata and parents, so a unit is named by
#' its strata and units at every kept stage.
#' @noRd
stage_path_ids <- function(spec) {
  path <- unlist(lapply(spec$stage, function(e) {
    list(e$strata$id %||% rep(1L, spec$n_rows), e$unit$id)
  }), recursive = FALSE)
  names(path) <- paste0("v", seq_along(path))
  as.integer(vctrs::vec_group_id(vctrs::new_data_frame(path)))
}

#' The units of an export's last kept stage that are no random draw
#'
#' A recorded certainty unit, or a unit drawn without replacement with
#' probability 1. A stage drawn with replacement records expected hits, which
#' can reach 1 in a random draw, so only its recorded certainty units count.
#' @param draw_spec The stage as drawn, before `with_replacement_stage()`.
#' @noRd
export_stage_sure <- function(entry, draw_spec, n) {
  certain <- (entry$certainty %||% rep(FALSE, n)) %in% TRUE
  if (identical(survey_stage_kind(draw_spec), "wr")) {
    return(certain)
  }
  certain | is_certainty_probability(entry$prob %||% rep(0, n))
}

#' Read a stage of the export as drawn with replacement
#' @noRd
with_replacement_stage <- function(design, stage) {
  design$stages[[stage]]$draw_spec$export_wr <- TRUE
  design
}

#' The stages an export keeps, a prefix of those the sample executed
#' @return `NULL` for the full export, otherwise the stage numbers.
#' @noRd
check_export_stages <- function(stages, x, call = caller_env()) {
  if (is_null(stages)) {
    return(NULL)
  }
  executed <- get_stages_executed(x)
  ok <- is.numeric(stages) && length(stages) > 0L && !anyNA(stages) &&
    is_integerish_numeric(stages) && length(stages) <= length(executed) &&
    identical(as.integer(stages), as.integer(executed[seq_along(stages)]))
  if (!ok) {
    abort_samplyr(
      c(
        "{.arg stages} must name the first stages of the sample, such as
         {.code 1} or {.code 1:2}.",
        "i" = "This sample executed {cli::qty(length(executed))}stage{?s}
               {executed}."
      ),
      class = "samplyr_error_survey_argument",
      call = call
    )
  }
  as.integer(stages)
}

#' Refuse `stages` beyond 1 where the export is by ultimate clusters already
#' @noRd
check_stages_ultimate_only <- function(stages, what, call = caller_env()) {
  if (is_null(stages) ||
        (is.numeric(stages) && length(stages) == 1L && isTRUE(stages == 1))) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.arg stages} can only be {.code 1} for {what}.",
      "i" = "It is exported by ultimate clusters, its stage-1 units drawn
             with replacement, which is what {.code stages = 1} asks."
    ),
    class = "samplyr_error_survey_argument",
    call = call
  )
}

#' Refuse `stages` on an export that has no such form
#' @noRd
check_no_export_stages <- function(stages, what, call = caller_env()) {
  if (is_null(stages)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.arg stages} is not available for {what}.",
      "i" = "It truncates the stages of a single-phase design, and this
             export has no such form."
    ),
    class = "samplyr_error_survey_argument",
    call = call
  )
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
#' `theta` belongs to the first frame of the stack. Reversing the
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
#' `survey::svydesign()` builds. So is a component drawn in several stages
#' (`samplyr_error_survey_multiframe_stages`), since `multiframe()` reads
#' one selection probability per stage and the export carries one for the
#' whole design. [as_svrepdesign()] replicates such a stack.
#'
#' So is a frame exported as drawn with replacement, such as a draw by
#' `srswr`, a PPS draw with replacement or minimum replacement, or a settled
#' reserve list (`samplyr_error_survey_multiframe_unsupported`).
#' `multiframe()` computes each frame's variance as for a draw without
#' replacement, which understates it there. [as_svrepdesign()] keeps each
#' frame's own variance.
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
#' population, or adjusted when a reserve list was settled: the combination
#' is harmonic, and such a realized weight is a random variable rather than
#' an inverse inclusion probability.
#'
#' ## Means of a variable with missing values
#'
#' In survey 4.5, `svymean(..., na.rm = TRUE)` on the returned design keeps
#' the rows whose value is missing in its denominator, so the mean is wrong.
#' `svytotal(..., na.rm = TRUE)` is not affected, nor is the design
#' [as_svrepdesign()] returns.
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
    version = "4.5",
    reason = "to convert a frame stack to a survey design object."
  )
  check_multiframe_dots(enquos(...))
  survey_validate_multiframe_support(x, "as_svydesign")
  check_multiframe_estimator(x, estimator, theta, "as_svydesign")
  theta <- if (identical(estimator, "constant")) {
    resolve_multiframe_theta(theta)
  }

  designs <- stats::setNames(lapply(names(x), function(nm) {
    do.call(
      as_svydesign,
      c(
        list(x[[nm]]),
        list(...),
        list(nest = nest, systematic_variance = systematic_variance)
      )
    )
  }), names(x))

  check_multiframe_component_stages(designs, names(x))
  check_multiframe_component_replacement(designs, names(x))
  result <- survey::multiframe(
    unname(designs),
    if (identical(estimator, "constant")) {
      unname(multiframe_overlaps(x, designs))
    } else {
      # Supply unambiguous overlap weights rather than probabilities.
      unname(multiframe_overlap_weights(x, designs))
    },
    estimator = estimator,
    theta = theta
  )
  attr(result, "samplyr_overlap_probability_quality") <- attr(x, "overlaps")$probability_quality
  result
}

#' Refuse a component exported as drawn with replacement
#'
#' survey's dual-frame variance builds each component's pairwise terms from
#' its units' selection probabilities, as for a draw without replacement,
#' whatever correction the component carries. A component exported with no
#' finite correction, such as a draw by `srswr`, a PPS draw with replacement
#' or minimum replacement, or a settled reserve read by ultimate clusters,
#' then has its variance understated, by about 7% for eight draws of thirty.
#' The replicate export keeps each frame's own variance.
#' @noRd
check_multiframe_component_replacement <- function(designs, frames,
                                                   call = caller_env()) {
  drawn_wr <- vapply(designs, function(d) {
    if (inherits(d, "pps")) {
      return(FALSE)
    }
    popsize <- d$fpc$popsize
    is_null(popsize) || any(is.infinite(popsize[, 1]))
  }, logical(1))
  if (!any(drawn_wr)) {
    return(invisible(NULL))
  }
  drawn_wr <- frames[drawn_wr]
  abort_samplyr(
    c(
      "{.fn as_svydesign} cannot composite a frame exported as drawn with
       replacement.",
      "x" = "{cli::qty(length(drawn_wr))}Frame{?s} {.val {drawn_wr}}
             {?has/have} no finite population correction.",
      "i" = "{.fn survey::multiframe} computes each frame's variance from its
             units' selection probabilities as for a draw without
             replacement, which understates the variance of a draw with
             replacement.",
      "i" = "Use {.fn as_svrepdesign} on the stack, whose replicates keep each
             frame's own variance."
    ),
    class = "samplyr_error_survey_multiframe_unsupported",
    call = call
  )
}

#' Refuse a component whose stages survey's dual-frame estimator cannot read
#'
#' `survey::multiframe()` checks each component with one selection
#' probability per stage, and a multistage export carries one for the whole
#' design.
#' @noRd
check_multiframe_component_stages <- function(designs, frames,
                                              call = caller_env()) {
  for (q in seq_along(designs)) {
    d <- designs[[q]]
    if (inherits(d, "pps") || NCOL(d$cluster) <= NCOL(d$allprob)) {
      next
    }
    abort_samplyr(
      c(
        "{.fn as_svydesign} cannot composite frame {.val {frames[[q]]}},
         drawn in {NCOL(d$cluster)} stages.",
        "x" = "{.fn survey::multiframe} reads one selection probability per
               stage, and the export of a multistage sample carries one for
               the whole design.",
        "i" = "{.fn as_svrepdesign} replicates each frame's stages and
               composites the replicate weights."
      ),
      class = "samplyr_error_survey_multiframe_stages",
      call = call
    )
  }
  invisible(NULL)
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
#' discarded by survey under it, and neither a realized weight-share weight
#' nor a weight adjusted at a reserve settlement is an inclusion probability.
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

  settled <- names(x)[vapply(x, function(component) {
    !is_null(attr(component, "metadata")$reserve_settlement)
  }, logical(1))]
  if (length(settled) > 0) {
    abort_samplyr(
      c(
        "{.code estimator = \"expected\"} cannot take a component settled from
         a reserve list.",
        "x" = "{cli::qty(length(settled))}Frame{?s} {.val {settled}}
               carr{?ies/y} weights adjusted at settlement.",
        "i" = "The estimator combines inverse inclusion probabilities
               harmonically, and a settled weight is a realized random
               quantity: in a two-frame simulation the estimate of the
               overlap was biased even with each unit's true chance of
               responding in the settled frame.",
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

#' The row of its stack component behind each row a component exports
#'
#' A component's export can add rows, such as the ineligible units of a
#' settled reserve, which have no `.sample_id` and map to `NA`. A shared
#' component exports its target rows, one for each, without it.
#' @param designs The exported components, named by frame, or `NULL` for
#'   the components' own rows.
#' @noRd
frame_stack_rows_of <- function(x, designs = NULL) {
  stats::setNames(lapply(names(x), function(nm) {
    ids <- designs[[nm]]$variables$.sample_id
    if (is_null(ids)) {
      return(seq_len(nrow(x[[nm]])))
    }
    match(ids, x[[nm]]$.sample_id)
  }), names(x))
}

#' The declared overlaps as weights, one matrix per exported component
#'
#' Zero still marks a frame the unit does not belong to, which is the absence
#' survey's own formula reads and removes. No exported row is added here: the
#' only components that add rows, settled reserves, are refused under this
#' estimator.
#' @noRd
multiframe_overlap_weights <- function(x, designs = NULL) {
  rows_of <- frame_stack_rows_of(x, designs)
  stats::setNames(lapply(names(x), function(nm) {
    probabilities <- frame_component_overlaps(x, nm)[rows_of[[nm]], ,
                                                     drop = FALSE]
    ifelse(probabilities > 0, 1 / probabilities, 0)
  }), names(x))
}

#' One membership matrix per exported component, in the order the frames are
#' stacked
#'
#' survey reads the column belonging to the *other* frame by position in the
#' designs list, so these columns follow the stack's order and not the order
#' the membership mapping happened to be written in. Getting that wrong
#' produces a number rather than an error. A row the export added stands in
#' its own frame alone, unless it stands for a unit a settled reserve
#' dropped, which keeps that unit's membership.
#' @noRd
multiframe_overlaps <- function(x, designs = NULL) {
  membership <- attr(x, "membership")
  rows_of <- frame_stack_rows_of(x, designs)
  stats::setNames(lapply(names(x), function(nm) {
    member <- frame_component_membership(x[[nm]], membership)[
      rows_of[[nm]], , drop = FALSE
    ]
    added <- is.na(rows_of[[nm]])
    member[added, ] <- FALSE
    member[added, nm] <- TRUE
    units <- if (!is_null(designs)) {
      reserve_added_membership(x, nm, designs[[nm]])
    }
    if (!is_null(units)) {
      held <- !is.na(units[, 1L])
      member[held, ] <- units[held, ]
    }
    matrix(
      as.numeric(member),
      nrow = nrow(member),
      ncol = length(membership),
      dimnames = list(NULL, names(membership))
    )
  }), names(x))
}

#' Convert a tbl_sample to a replicate-weight survey design
#'
#' Creates a `svyrep.design` object from a `tbl_sample`. `"rwyb"` builds
#' Rao-Wu-Yue-Beaumont factors with the svrep package from the recorded
#' stage mechanisms, and `"random_groups"` reads the replicates of
#' `execute(reps = R)`. The other types build a [survey::svydesign()]
#' object and convert it with [survey::as.svrepdesign()].
#'
#' @inheritParams as_svydesign
#' @param type The replicate method. The generic types, `"JK1"` to
#'   `"subbootstrap"` below, resample first-stage units within first-stage
#'   strata and read nothing below them. The last three read more of the
#'   design.
#'   - `"auto"` (default): survey's choice, `"JKn"` for a stratified first
#'     stage and `"JK1"` otherwise.
#'   - `"JK1"` and `"JKn"`: the delete-one jackknife of an unstratified
#'     first stage, and its stratified form (Wolter 2007, ch. 4). They keep
#'     the finite population correction of an equal-probability first stage.
#'   - `"BRR"` and `"Fay"`: balanced half-samples, with Fay's damping set by
#'     `fay.rho` (Wolter 2007, ch. 3). They need an even number of
#'     first-stage units, at least two, in every first-stage stratum, and
#'     carry no finite population correction.
#'   - `"bootstrap"`: the Canty-Davison bootstrap of first-stage units, which
#'     keeps an equal-probability first stage's correction (Canty and
#'     Davison 1999).
#'   - `"subbootstrap"`: the Rao-Wu rescaled bootstrap, with no finite
#'     population correction (Rao, Wu and Yue 1992). It accepts a PPS first
#'     stage.
#'   - `"mrbbootstrap"`: Preston's multistage rescaled bootstrap, which reads
#'     the sampling fraction of every stage (Preston 2009).
#'   - `"rwyb"`: the Rao-Wu-Yue-Beaumont bootstrap of every stage (Beaumont
#'     and \enc{Émond}{Emond} 2022). It needs the svrep package and is never chosen by
#'     `"auto"`.
#'   - `"random_groups"`: the spread between the independent replicates of
#'     `execute(reps = R)`, for any selection method (Wolter 2007, ch. 2).
#'
#'   The jackknife and balanced types give one set of replicate weights per
#'   sample. The bootstrap types draw from the session's random stream, so
#'   set a seed before the call, and raise `replicates` through `...` until
#'   the standard errors that will be read are stable. svrep's
#'   `estimate_boot_sim_cv()` estimates that simulation error.
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
#' @param stages `NULL`, the default, or `1`, which replicates the
#'   ultimate-cluster design of `as_svydesign(x, stages = 1)`: the stage-1
#'   units are resampled within their strata, the correction is left out by
#'   choice, and no first-stage warning is raised. It is taken by the types
#'   that resample stage-1 units only, and refused with `"mrbbootstrap"`,
#'   `"rwyb"` and `"random_groups"` (`samplyr_error_survey_argument`).
#' @param systematic_variance What to do about the generic replicate weights
#'   built for `systematic`, `pps_systematic` and `pps_systematic_mr` stages. `"warn"` (default)
#'   builds them and warns once per call, naming every affected stage.
#'   `"approximate"` builds them silently, for a caller who has acknowledged
#'   the approximation, and `"error"` refuses. Naming a `type` is not an
#'   acknowledgment, since no type reproduces systematic selection. Census
#'   stages and stages that drew one unit in each pool are exempt, as in
#'   [as_svydesign()]. The choice and the affected
#'   stages are recorded in the `"samplyr_systematic_variance"` attribute of
#'   the result.
#'
#' @return A `svyrep.design` object from the survey package. `"rwyb"` and
#'   `"random_groups"` record their method in its `"samplyr_replication"`
#'   attribute, and the treatment of systematic stages is recorded in
#'   `"samplyr_systematic_variance"`.
#'
#' @details
#' ## What is replicated
#'
#' A single-phase sample is replicated from the design [as_svydesign()]
#' exports, multistage samples, shared weights, independent frame stacks
#' and settled reserves included. A materialized occasion is replicated by
#' ultimate clusters, and a window's replicates are calibrated on the same
#' totals as its linearized design. `"rwyb"` is refused for an occasion
#' (`samplyr_error_occasion_replicate_type`). Of the two-phase samples,
#' only two shapes are replicated: a second phase drawn within each
#' final-stage pool of phase 1, which is phase 1's design with a smaller
#' final take, and a second phase of a phase 1 that could select nothing
#' else. The others are refused (`samplyr_error_svrep_twophase_unsupported`).
#'
#' ## The first stage under the generic types
#'
#' The generic types read the spread of first-stage totals within
#' first-stage strata, which carries the variance of the later stages as
#' well. That reading needs no correction for a first stage drawn with
#' replacement, and unequal probabilities at later stages reach it through
#' the weights. It is approximate in three cases, each with a warning:
#'
#' - A PPS first stage drawn without replacement is read as drawn with
#'   replacement, and so is an equal-probability first stage under `"BRR"`,
#'   `"Fay"` and `"subbootstrap"`, which carry no correction. The variance
#'   leaves out the first-stage correction and tends to overstate, most at
#'   large first-stage sampling fractions
#'   (`samplyr_warning_replicate_wr_first_stage`).
#' - `"JK1"`, `"JKn"` and `"bootstrap"` keep an equal-probability first
#'   stage's correction and apply it to the later stages' variance too,
#'   which understates the variance as the first-stage fraction grows
#'   (`samplyr_warning_replicate_first_stage_fpc`).
#' - The minimum-replacement methods `pps_chromy` and `pps_systematic_mr`
#'   are read as drawn with replacement, as every export reads them, which
#'   ignores the dependence between hits. `"mrbbootstrap"` reads them that
#'   way too, with the same warning, and `"rwyb"` refuses them.
#'
#' A first-stage unit taken with probability 1 above a later stage, by a
#' certainty rule or because its stratum is taken whole, is a stratum of
#' its own whose stage-2 units are resampled (see Statistical details). The
#' first stage is then stratified, so `"JK1"` is refused and `"auto"` takes
#' `"JKn"`. `"mrbbootstrap"` and `"rwyb"` read every stage's probabilities
#' and need none of these readings, and [as_svydesign()] gives the
#' linearization variance.
#'
#' ## Censuses
#'
#' A stratum of a single stage taken whole keeps its full weight in every
#' replicate, so it adds no variance under any type. An unstratified census
#' gets two replicate columns at the full-sample weights under `"JK1"` and
#' `"auto"`, so its variance is zero and it can be shared, stacked and
#' calibrated. When every row has the same weight the replicate matrix
#' stays uncompressed, and [survey::calibrate()] then needs
#' `compress = FALSE` (survey 4.5). A stratified census is refused under
#' `"JKn"` and `"auto"` (`samplyr_error_svrep_conversion_failed`), and
#' `"bootstrap"`, `"rwyb"` and [as_svydesign()] give it a variance of zero.
#'
#' ## Statistical details
#'
#' Inside a PSU taken with certainty, the generic types resample the
#' stage-2 units of each stratum `h` of that PSU as drawn with replacement,
#' which leaves out stage 2's correction `1 - f_h`. When stage 2 drew the
#' stratum with equal probabilities without replacement and no later stage
#' samples inside its units, the export replaces each replicate weight of
#' the stratum's rows by
#' \deqn{w_k^{(r)*} = w_k + \sqrt{1 - f_h}\,\left(w_k^{(r)} - w_k\right),}{w*_k(r) = w_k + sqrt(1 - f_h) (w_k(r) - w_k),}
#' where \eqn{w_k}{w_k} is the full-sample weight and \eqn{w_k^{(r)}}{w_k(r)}
#' the weight in replicate `r`. This multiplies the stratum's contribution
#' to the variance of a linear estimator by `1 - f_h`. For a total, the
#' jackknife and balanced types then give the without-replacement variance
#' of the stratum exactly, and the bootstrap types in expectation. One
#' factor serves every unit of the stratum, since the replicates contrast
#' those units with each other. A stratum with a unit sampled below it, or
#' a PPS stage 2, keeps the with-replacement reading, which tends to
#' overstate its contribution (`samplyr_warning_replicate_certainty_stage_wr`).
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
#' family. Balanced, spatial, Pareto, SPS and minimum-replacement methods
#' have no built-in RWYB mapping.
#'
#' The adapter keeps stage-specific sampling units, strata and
#' probabilities. With-replacement stages resample draw occurrences, not
#' distinct population units. Certainty units have conditional replicate
#' factor one. Noncertainty singleton strata raise
#' `samplyr_error_rwyb_singleton` whenever their variance contribution is
#' needed, except under Poisson sampling, whose variance is estimable from
#' one unit, and at the final stage under `lonely.psu = "certainty"`.
#'
#' A selected unit with no descendant in the final sample is kept as a zero
#' row, because dropping it would change the earlier-stage resampling
#' distribution. This holds for a unit whose next register was empty and
#' for one in which a later Poisson stage selected nothing, which execution
#' records whatever `frame_digest` is, so estimates need `na.rm = TRUE`. A
#' sample that lacks that record is refused
#' (`samplyr_error_rwyb_missing_parents`). Empty samples cannot be
#' exported. With `mse = TRUE` the factors use scale `1 / replicates`, and
#' with `mse = FALSE` they use `1 / (replicates - 1)`. The backend and stage
#' methods are recorded in the `"samplyr_replication"` attribute.
#'
#' ## Random groups
#'
#' `type = "random_groups"` takes a sample from `execute(reps = R)` as R
#' independent samples of the same design (Wolter 2007, ch. 2). The estimate
#' is the mean of the R replicate estimates, carried by the full-sample
#' weight `.weight / R`, and its variance is their spread divided by R, with
#' R - 1 degrees of freedom. A replicate that selected nothing counts as a
#' zero estimate. Nothing is assumed about the selection method, so the
#' route suits systematic, balanced, spatial or custom stages, which no
#' other type redraws. The cost is R samples.
#'
#' The replicates must vary at every stage and phase. A sample that
#' continues one realized stage with `reps`, or replicates a later phase
#' over one realized first phase, is refused
#' (`samplyr_error_random_groups_shared`), because the shared selection's
#' variance would be left out. Replicate that stage or phase too, and
#' continue each replicate. A first phase that could select nothing else,
#' such as the one unit of a one-unit population, has no variance to leave
#' out, and a later phase replicated over it is accepted. The sample must
#' hold every replicate of its
#' execution (`samplyr_error_random_groups_input`).
#'
#' ## Variance by selection method
#'
#' The generic types are refused for Poisson sampling, which needs
#' `"rwyb"`. Bounded cube, LPM2 and SCPS have only the generic PPS bootstrap
#' of `"subbootstrap"` and `"mrbbootstrap"`, which does not recreate their
#' constraints or spatial algorithm. Only `"random_groups"` redraws a
#' systematic start. [variance_estimation] compares the estimators by
#' selection method.
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
#' # The deterministic jackknife against linearization, and a seeded
#' # bootstrap with enough replicates for a stable second decimal.
#' survey::SE(survey::svytotal(~households, rep_svy))
#' survey::SE(survey::svytotal(~households, as_svydesign(sample)))
#' set.seed(1)
#' boot <- as_svrepdesign(sample, type = "bootstrap", replicates = 500)
#' survey::SE(survey::svytotal(~households, boot))
#'
#' @references
#' Beaumont, J.-F. and \enc{Émond}{Emond}, N. (2022). A bootstrap variance
#' estimation method for multistage sampling and two-phase sampling when
#' Poisson sampling is used at the second phase. *Stats*, 5(2), 339-357.
#' \doi{10.3390/stats5020019}
#'
#' Canty, A. J. and Davison, A. C. (1999). Resampling-based variance
#' estimation for labour force surveys. *The Statistician*, 48, 379-391.
#'
#' Preston, J. (2009). Rescaled bootstrap for stratified multistage
#' sampling. *Survey Methodology*, 35(2), 227-234.
#'
#' Rao, J.N.K., Wu, C.F.J. and Yue, K. (1992). Some recent work on
#' resampling methods for complex surveys. *Survey Methodology*, 18(2),
#' 209-217.
#'
#' Wolter, K. M. (2007). *Introduction to Variance Estimation*, 2nd ed.
#' Springer.
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
  systematic_variance = c("warn", "approximate", "error"),
  stages = NULL
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
  check_sample_unmodified(x, "as_svrepdesign", provisional_ok = TRUE)
  check_reserve_settled(x, "as_svrepdesign")
  rlang::check_installed(
    "survey",
    version = "4.5",
    reason = "to convert a tbl_sample to a replicate-weight survey design."
  )

  check_forwarded_args(
    enquos(...),
    owned = c("type", "systematic_variance", "stages"),
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
    check_no_export_stages(stages, "random-group replicates")
    return(build_random_groups_svrepdesign(x, ...))
  }

  if (holds_settled_reserve(x)) {
    check_reserve_export_route(x, "as_svrepdesign")
    check_stages_ultimate_only(stages, "a settled reserve")
    if (type %in% c("mrbbootstrap", "rwyb")) {
      abort_samplyr(
        c(
          "{.fn as_svrepdesign} cannot build {.val {type}} replicates for a
           settled reserve.",
          "x" = "{.val {type}} uses the probabilities of every stage, and the
                 settled stage's is unknown, since its respondents are a
                 random subset of a respondent count the field never
                 observed.",
          "i" = "The other types replicate the primary units, which carry
                 the settled stage in their totals."
        ),
        class = "samplyr_error_reserve_full_export"
      )
    }
    systematic_stages <- systematic_approximated_stages(
      get_design(x), get_stages_executed(x)[1], as.data.frame(x)
    )
    check_systematic_variance(
      systematic_stages,
      systematic_variance,
      approximation = "generic_replicates",
      fn_name = "as_svrepdesign"
    )
    svy <- build_reserve_svydesign(
      x, nest = TRUE, dots = list(), fn_name = "as_svrepdesign",
      check_lonely = FALSE
    )
    call <- current_env()
    result <- tryCatch(
      survey::as.svrepdesign(design = svy, type = type, ...),
      error = function(e) {
        abort_samplyr(
          c(
            "{.fn as_svrepdesign} failed to convert this design to
             replicate weights.",
            "x" = "{conditionMessage(e)}"
          ),
          class = "samplyr_error_svrep_conversion_failed",
          call = call
        )
      }
    )
    attr(result, "samplyr_reserve_units") <- attr(svy, "samplyr_reserve_units")
    return(record_systematic_variance(
      result, systematic_stages, systematic_variance, "generic_replicates"
    ))
  }

  # Replicate the source design before applying shared weights.
  if (identical(sample_weight_contract(x), "shared")) {
    check_no_export_stages(stages, "a sample with shared weights")
    return(svrep_from_shared_weights(
      x,
      type = type,
      systematic_variance = systematic_variance,
      dots = list(...)
    ))
  }

  check_prev_phase_unmodified(x, "as_svrepdesign")
  x <- composed_subsample(x) %||% x
  occasion_route <- occasion_export_route(x, survey_phase_info(x))
  phase_info <- survey_validate_phase_support(
    x,
    allow_twophase = !is_null(occasion_route),
    fn_name = "as_svrepdesign"
  )
  if (!is_null(occasion_route) && type == "rwyb") {
    abort_samplyr(
      c(
        "{.fn as_svrepdesign} cannot build {.val rwyb} replicates for an
         occasion.",
        "i" = "{.val rwyb} replicates the master's selection, and an occasion
               adds its activation on top of it.",
        "i" = "The other types replicate the occasion's primary units."
      ),
      class = "samplyr_error_occasion_replicate_type"
    )
  }
  if (identical(occasion_route, "ultimate_cluster")) {
    check_stages_ultimate_only(stages, "an occasion")
    return(build_activation_ultimate_svrep(
      x,
      prev_phase = phase_info$prev_phase,
      type = type,
      systematic_variance = systematic_variance,
      ...
    ))
  }

  design <- get_design(x)
  stages <- check_export_stages(stages, x)
  if (!is_null(stages)) {
    return(build_stage1_svrepdesign(x, stages, type, systematic_variance, ...))
  }
  if (type == "rwyb") {
    return(build_rwyb_svrepdesign(x, systematic_variance, ...))
  }
  # The rows the design below is built from, its zero rows included.
  spec <- export_stage_spec(
    export_rows(x, design, notify = FALSE),
    design,
    get_stages_executed(x)
  )
  if (any(vapply(spec$stage, function(e) e$kind, "") == "rs_poisson")) {
    abort_samplyr(c(
      "Generic replicate methods do not represent Poisson sample-size variance.",
      "i" = "Use {.code as_svrepdesign(x, type = \"rwyb\")} for independent Poisson sampling."
    ), class = "samplyr_error_poisson_replicates")
  }
  spec <- flag_probability_one(spec)
  first <- spec$stage[[1]]
  pps_safe_types <- c("subbootstrap", "mrbbootstrap")

  # Replicates do not reproduce systematic selection. A refusing policy
  # stops here. The approximation advice waits until the conversion has
  # succeeded, so a refused export carries none.
  systematic_stages <- systematic_approximated_stages(
    design, get_stages_executed(x), as.data.frame(x)
  )
  check_systematic <- function() {
    check_systematic_variance(
      systematic_stages,
      systematic_variance,
      approximation = "generic_replicates",
      fn_name = "as_svrepdesign"
    )
  }
  if (identical(systematic_variance, "error")) {
    check_systematic()
  }
  # BRR, Fay and the rescaled bootstrap carry no first-stage correction, and
  # the conversion around certainty units drops it, so they read an
  # equal-probability first stage as drawn with replacement.
  # A single stage taken whole keeps its full weight in every replicate.
  whole_rows <- single_stage_whole_rows(spec)
  taken_whole <- length(whole_rows) > 0L && all(whole_rows)
  fpc_dropped <- identical(first$kind, "equal_wor") &&
    !(length(first$certainty) > 0L && all(first$certainty %in% TRUE)) &&
    !taken_whole &&
    (type %in% c("BRR", "Fay", "subbootstrap") ||
       needs_first_stage_rebuild(spec, type))
  # The types that use every stage's probabilities keep a first stage's
  # correction, but minimum replacement has none to keep.
  # A first stage whose units are all certain holds none to resample.
  all_certain <- (length(first$certainty) > 0L &&
                    all(first$certainty %in% TRUE)) || taken_whole
  read_as_wr <- (stage_replicated_as_wr(
    design$stages[[first$stage]]$draw_spec, first$kind
  ) && !all_certain &&
    (!type %in% pps_safe_types || first$method %in% pmr_methods)) ||
    fpc_dropped
  warn_wr_first_stage <- if (read_as_wr) function() {
    # Every export reads minimum replacement as with replacement, and RWYB
    # refuses it, so only independent samples reflect its selection.
    why <- if (first$method %in% pmr_methods) {
      c(
        "i" = "Every export reads minimum replacement this way,
               {.fn as_svydesign} included, so the variance ignores the
               dependence between hits. {.val rwyb} refuses it.",
        "i" = "Independent samples from {.code execute(reps = )} with
               {.code type = \"random_groups\"} repeat the selection itself."
      )
    } else {
      c(
        "i" = "The variance leaves out the first stage's finite population
               correction, so it errs toward too large when first-stage
               sampling fractions are large.",
        "i" = "{.val mrbbootstrap} and {.val rwyb} use every stage's
               probabilities, and {.fn as_svydesign} gives the
               linearization variance."
      )
    }
    cli_warn(
      c(
        "{.fn as_svrepdesign} with {.val {type}} treats the first stage,
         drawn with {.val {first$method}}, as drawn with replacement.",
        why
      ),
      class = "samplyr_warning_replicate_wr_first_stage"
    )
  }

  svydesign_obj <- build_singlephase_svydesign(
    x,
    dots = list(),
    nest = TRUE,
    relax_pps_for_bootstrap = type %in% pps_safe_types,
    check_lonely = FALSE,
    # These types misread a first-stage no-correction term.
    omit_uncorrected_fpc = type %in% c("auto", "JK1", "JKn", "bootstrap")
  )
  warn_fpc_scales <- if (
    first_stage_fpc_scales_later_stages(svydesign_obj, spec, type)
  ) {
    function() cli_warn(
      c(
        "{.fn as_svrepdesign} with {.val {type}} applies the first stage's
         finite population correction to the whole variance, including the
         part that later stages add.",
        "i" = "That part shrinks with the first-stage sampling fraction, so
               the variance errs toward too small when the fraction is
               large.",
        "i" = "{.val mrbbootstrap} and {.val rwyb} use every stage's
               probabilities, and {.fn as_svydesign} gives the
               linearization variance."
      ),
      class = "samplyr_warning_replicate_first_stage_fpc"
    )
  }

  if (identical(type, "JK1") && needs_first_stage_rebuild(spec, type)) {
    abort_samplyr(
      c(
        "{.code type = \"JK1\"} resamples an unstratified first stage, and
         this sample's replicate design stratifies it.",
        "i" = "A first stage drawn with probabilities proportional to size
               without replacement, and units taken with probability 1 above
               a later stage, are strata of their own there.",
        "i" = "Use {.code type = \"JKn\"}, which {.code type = \"auto\"}
               picks for this sample."
      ),
      class = "samplyr_error_svrep_conversion_failed"
    )
  }
  # survey's own conversion stops there. A PPS first stage is converted by
  # samplyr, which gives the census its zero variance.
  if (type %in% c("auto", "JKn") && taken_whole &&
      length(spec$stage[[1]]$strata$user) > 0L &&
      !needs_first_stage_rebuild(spec, type)) {
    abort_samplyr(
      c(
        "{.fn as_svrepdesign} cannot give {.code type = \"{type}\"}
         replicate weights for a census.",
        "x" = "Every first-stage stratum is taken whole, so the first stage
               has no sampling variance, and survey's stratified jackknife
               stops on it.",
        "i" = "{.code type = \"bootstrap\"} or {.code type = \"rwyb\"}, or
               {.fn as_svydesign}, give this census a variance of zero."
      ),
      class = "samplyr_error_svrep_conversion_failed"
    )
  }
  # survey's jackknife divides by the number of first-stage units less one,
  # so an unstratified census of one unit takes its zero variance here.
  one_unit_census <- type %in% c("auto", "JK1") && taken_whole &&
    length(spec$stage[[1]]$strata$user) == 0L &&
    vctrs::vec_unique_count(spec$stage[[1]]$unit$id) == 1L
  call <- current_env()
  result <- tryCatch(
    withCallingHandlers(
      if (one_unit_census) {
        zero_variance_svrep(svydesign_obj$variables,
                            1 / svydesign_obj$prob, "JK1",
                            replicate_mse(list(...)))
      } else if (needs_first_stage_rebuild(spec, type)) {
        svrep_from_first_stage(svydesign_obj, spec, type, ...)
      } else {
        survey::as.svrepdesign(design = svydesign_obj, type = type, ...)
      },
      # The classed warning above already says the correction is dropped,
      # and a census takes its full weight back afterwards.
      warning = function(w) {
        if ((fpc_dropped || taken_whole) && identical(
          conditionMessage(w),
          "Finite population correction dropped in conversion"
        )) {
          invokeRestart("muffleWarning")
        }
      }
    ),
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

  if (!identical(systematic_variance, "error")) {
    check_systematic()
  }
  if (!is_null(warn_wr_first_stage)) warn_wr_first_stage()
  if (!is_null(warn_fpc_scales)) warn_fpc_scales()
  result <- warn_certainty_lower_wr(result, type)

  result <- hold_self_representing(result, whole_rows)
  record_systematic_variance(
    compress_replicate_weights(pad_zero_width_replicates(result)),
    systematic_stages, systematic_variance, "generic_replicates"
  )
}

#' The rows of a single stage taken with probability 1
#'
#' Such a row adds no variance, since nothing is drawn below it. Below a
#' later stage, `flag_probability_one()` converts it as a certainty unit
#' instead. A first stage drawn with replacement has no such rows.
#' @noRd
single_stage_whole_rows <- function(spec) {
  first <- spec$stage[[1]]
  if (length(spec$stages) > 1L || identical(first$kind, "wr") ||
        is_null(first$prob)) {
    return(logical(0))
  }
  is_certainty_probability(first$prob)
}

#' Keep the self-representing rows at their full weight in every replicate
#'
#' survey marks a row self-representing when its first-stage stratum is
#' taken whole, and leaves it out of the replicate estimates. Under
#' `mse = TRUE` they are then centered on a full-sample estimate that still
#' holds the row, and with one stratum of three taken whole the jackknife
#' variance was 165 times the linearization variance. The mark reads the
#' first stage only, so a census first stage with a stage drawn below lost
#' that stage's variance under `"mrbbootstrap"`, and survey's ratio fails
#' on a design whose rows are all marked. The mark is removed. The rows of
#' `sure` take their full weight in every replicate instead, which BRR, Fay
#' and the rescaled bootstrap, carrying no first-stage correction, did not
#' give them. Each type resamples its strata apart, so this removes the
#' variance of those strata and nothing else.
#' @noRd
hold_self_representing <- function(design, sure) {
  design$selfrep <- NULL
  if (!any(sure)) {
    return(design)
  }
  # A census, whose replicate scale survey can leave infinite for a
  # stratum of one unit.
  if (all(sure)) {
    return(zero_variance_svrep(design$variables, design$pweights,
                               design$type, design$mse))
  }
  analysis <- as.matrix(stats::weights(design, type = "analysis"))
  analysis[sure, ] <- design$pweights[sure]
  design$repweights <- analysis
  design$combined.weights <- TRUE
  design
}

#' Two replicate columns at the full-sample weights for a design with none
#'
#' survey's jackknife gives a census no replicate column, and
#' `survey::svrepdesign()` cannot take a matrix without one, so the census
#' could be neither shared nor calibrated. Columns at the full-sample
#' weights deviate by zero, so the variance stays zero. Two of them, because
#' survey's compressor drops the matrix shape of a single column.
#' @noRd
pad_zero_width_replicates <- function(design) {
  if (ncol(design$repweights) > 0L) {
    return(design)
  }
  zero_variance_svrep(design$variables, stats::weights(design, "sampling"),
                      design$type, design$mse)
}

#' The `mse` survey's conversion would use
#' @noRd
replicate_mse <- function(dots) {
  dots$mse %||% getOption("survey.replicates.mse", FALSE)
}

#' A replicate design of zero variance, labeled with its type
#'
#' Built as type `"other"`, which takes the scale and replicate scales given
#' here, since survey ignores them for BRR and warns. Only the label then
#' names the type.
#' @noRd
zero_variance_svrep <- function(variables, weights, type, mse) {
  design <- survey::svrepdesign(
    variables = variables,
    repweights = cbind(weights, weights, deparse.level = 0),
    weights = weights,
    combined.weights = TRUE,
    type = "other",
    scale = 1,
    rscales = c(1, 1),
    mse = isTRUE(mse)
  )
  design$type <- type
  design
}

#' Store replicate weights compressed, which survey's calibration needs
#'
#' `survey::calibrate()`, `survey::postStratify()` and `survey::rake()` read
#' how a replicate design stores its weights, and calibration fails on a
#' plain matrix ("missing value where TRUE/FALSE needed" in survey 4.5):
#' its check of the `compress` argument comes out `NA`. A design built with
#' `survey::svrepdesign()`, or the multistage rescaled bootstrap of
#' `survey::as.svrepdesign()`, holds a plain matrix. Compressing keeps
#' every weight. survey 4.5's compressor drops the matrix dimension when
#' every row is the same, so such a design keeps its matrix.
#' @noRd
compress_replicate_weights <- function(design) {
  weights <- design$repweights
  if (!is.matrix(weights) || inherits(weights, "repweights_compressed") ||
      nrow(unique(weights)) < 2L) {
    return(design)
  }
  survey::compressWeights(design)
}

#' Does a first-stage correction reach the later stages' variance?
#'
#' JK1, JKn and the bootstrap multiply the spread of the first-stage units
#' by one minus the first-stage sampling fraction. That spread also carries
#' the variance the later stages add, which no first-stage correction
#' applies to. A later stage that takes every unit adds none.
#' @noRd
first_stage_fpc_scales_later_stages <- function(svydesign_obj, spec, type) {
  if (!type %in% c("auto", "JK1", "JKn", "bootstrap") ||
      needs_first_stage_rebuild(spec, type) || length(spec$stage) < 2L) {
    return(FALSE)
  }
  popsize <- svydesign_obj$fpc$popsize
  !is_null(popsize) && any(is.finite(popsize[, 1])) &&
    !all(vapply(spec$stage[-1], `[[`, logical(1), "census"))
}


#' Replicates of the ultimate-cluster design
#'
#' The first-stage types resample the stage-1 units of
#' `as_svydesign(x, stages = 1)`, so the correction is left out by choice and
#' no warning says so. A unit taken with probability 1 and nothing below it
#' is left out of the resampling and keeps its weight, since survey's
#' jackknife and bootstrap misread the infinite correction the other units
#' would take.
#' @noRd
build_stage1_svrepdesign <- function(x, stages, type, systematic_variance,
                                     ..., call = caller_env()) {
  if (length(stages) > 1L || type %in% c("rwyb", "mrbbootstrap")) {
    abort_samplyr(
      c(
        "{.fn as_svrepdesign} takes {.code stages = 1} with the types that
         resample stage-1 units only.",
        "i" = "{.val mrbbootstrap} and {.val rwyb} read every stage, and a
               replicate form for keeping later stages is not written.",
        "i" = "{.fn as_svydesign} takes {.code stages = 1:k}."
      ),
      class = "samplyr_error_survey_argument",
      call = call
    )
  }
  design <- get_design(x)
  executed <- get_stages_executed(x)
  preview_units <- stage_export_units(
    export_rows(x, design, executed, notify = FALSE),
    design, executed, stages, notify = FALSE
  )
  checked_stages <- unique(c(stages, preview_units$active_below_stage))
  if (any(vapply(checked_stages, function(i) {
    identical(survey_stage_kind(design$stages[[i]]$draw_spec), "rs_poisson")
  }, logical(1)))) {
    abort_samplyr(c(
      "Generic replicate methods do not represent Poisson sample-size variance.",
      "i" = "Use {.code as_svrepdesign(x, type = \"rwyb\")} for independent Poisson sampling."
    ), class = "samplyr_error_poisson_replicates", call = call)
  }
  systematic_stages <- stage_export_systematic_stages(
    x, design, executed, stages
  )
  check_systematic_variance(
    systematic_stages,
    systematic_variance,
    approximation = "generic_replicates",
    fn_name = "as_svrepdesign"
  )
  pps_safe_types <- c("subbootstrap", "mrbbootstrap")
  svy <- build_singlephase_svydesign(
    x, dots = list(), nest = TRUE,
    relax_pps_for_bootstrap = type %in% pps_safe_types,
    check_lonely = FALSE, stages = stages
  )
  units <- stage_export_units(
    export_rows(x, design, executed, notify = FALSE), design, executed, stages
  )
  spec <- units$spec
  if (identical(type, "JK1") && any(units$apart)) {
    abort_samplyr(
      c(
        "{.code type = \"JK1\"} resamples an unstratified first stage, and
         this sample's replicate design stratifies it.",
        "i" = "Units taken with probability 1 are strata of their own there.",
        "i" = "Use {.code type = \"JKn\"}, which {.code type = \"auto\"}
               picks for this sample."
      ),
      class = "samplyr_error_svrep_conversion_failed",
      call = call
    )
  }
  result <- tryCatch(
    if (any(units$census)) {
      spec$stage[[1]]$certainty <- units$census
      warn_certainty_lower_wr(svrep_from_first_stage(svy, spec, type, ...),
                              type)
    } else {
      survey::as.svrepdesign(design = svy, type = type, ...)
    },
    error = function(e) {
      abort_samplyr(
        c(
          "{.fn as_svrepdesign} failed to convert this design to replicate
           weights.",
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

#' Read the stage-1 units taken with probability 1 as certainty units
#'
#' A stratum taken whole, or a unit whose probability reaches 1 with no
#' certainty rule, is no random draw. With a stage below, a correction of one
#' would remove that stage's variance from the replicates, so the replicate
#' conversion takes its next-stage units as the sampling units, as for a
#' certainty unit. A single stage keeps its correction, which gives such a
#' unit no variance and is exact. A first stage drawn with replacement has
#' no such units.
#' @noRd
flag_probability_one <- function(spec) {
  first <- spec$stage[[1]]
  if (length(spec$stages) < 2L || identical(first$kind, "wr") ||
        is_null(first$prob)) {
    return(spec)
  }
  sure <- is_certainty_probability(first$prob)
  if (any(sure)) {
    flagged <- first$certainty %||% rep(FALSE, length(sure))
    spec$stage[[1]]$certainty <- flagged %in% TRUE | sure
  }
  spec
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
#'
#' survey resamples the stage-two units of a certainty PSU as drawn with
#' replacement. `certainty_lower_correction()` gives the factors that put
#' stage two's correction back where it is exact, and the result carries
#' the stage left without it as the attribute
#' `samplyr_certainty_lower_wr`, for the caller to report.
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
  psu_label <- paste(stratum, first$unit$id, sep = ".")
  lower_entries <- if (length(spec$stages) >= 2L) spec$stage[-1] else list()
  sub <- certainty_substage_split(
    df = df,
    parent_label = psu_label,
    parent_id = psu_id,
    sure = cert,
    lower_entries = lower_entries,
    default_stratum = stratum,
    allow_drawn = FALSE
  )
  if (any(sub$split)) {
    stratum[sub$split] <- sub$stratum_label[sub$split]
    psu[sub$split] <- paste("certainty", sub$unit_id[sub$split], sep = "\r")
  }
  drawn <- !sub$census
  if (!any(drawn)) {
    return(zero_variance_svrep(df, weight, switch(type, auto = "JKn", type),
                               replicate_mse(list(...))))
  }
  lower <- certainty_lower_correction(sub, lower_entries, n)
  corrected <- any(lower$factor != 1)
  uncorrected <- if (any(lower$uncorrected)) {
    list(stage = lower_entries[[1]]$stage, method = lower_entries[[1]]$method,
         below = lower$below)
  }

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
  if (all(drawn) && !corrected) {
    attr(replicated, "samplyr_certainty_lower_wr") <- uncorrected
    return(replicated)
  }

  analysis <- stats::weights(replicated, type = "analysis")
  if (corrected) {
    base <- weight[drawn]
    analysis <- base + lower$factor[drawn] * (analysis - base)
  }
  full <- matrix(weight, nrow(df), ncol(analysis))
  full[drawn, ] <- analysis
  spliced <- survey::svrepdesign(
    variables = df,
    repweights = full,
    weights = weight,
    type = switch(replicated$type, subbootstrap = "bootstrap", replicated$type),
    rho = if (identical(replicated$type, "Fay")) replicated$rho,
    # survey sets the scale of BRR and Fay itself, and warns when given one.
    scale = if (!replicated$type %in% c("BRR", "Fay")) replicated$scale,
    rscales = replicated$rscales,
    combined.weights = TRUE,
    mse = replicated$mse
  )
  spliced$type <- replicated$type
  attr(spliced, "samplyr_certainty_lower_wr") <- uncorrected
  spliced
}

#' Stage two's correction inside the PSUs taken with certainty
#'
#' The replicates resample a certainty PSU's stage-two units as drawn with
#' replacement, which leaves out the factor `1 - f` of their variance, with
#' `f` the stage-two sampling fraction. Where stage two drew with equal
#' probabilities without replacement and every stage below it took all of
#' its units, scaling each replicate weight's deviation from the full-sample
#' weight by `sqrt(1 - f)` restores it, exactly for a total. The decision is
#' taken for each resampling cell (the PSU crossed with stage two's strata),
#' since replicate contrasts are formed across the units of a cell: one
#' factor for every unit of the cell, or none. A cell with a unit that a
#' later stage samples inside, whose variance the factor would shrink, a
#' PPS stage two, which has no single factor, and a cell whose fraction is
#' not one value keep the with-replacement reading and are returned as
#' `uncorrected`. A stage two drawn with replacement has no correction to
#' restore.
#' @return A list of the row `factor`s (1 where nothing changes), the
#'   `uncorrected` rows, and `below`, TRUE when a later stage sampling inside
#'   them is the reason.
#' @noRd
certainty_lower_correction <- function(sub, lower_entries, n) {
  out <- list(factor = rep(1, n), uncorrected = rep(FALSE, n), below = FALSE)
  split <- sub$split
  if (!any(split) || identical(lower_entries[[1]]$kind, "wr")) {
    return(out)
  }
  next_entry <- lower_entries[[1]]
  whole_below <- rep(TRUE, n)
  for (e in lower_entries[-1]) {
    certain <- (e$certainty %||% rep(FALSE, n)) %in% TRUE
    whole_below <- whole_below & (if (identical(e$kind, "wr")) certain else
      certain | is_certainty_probability(e$prob %||% rep(0, n)))
  }
  cell <- as.character(sub$cell_id)
  whole_below <- as.vector(tapply(whole_below, cell, all)[cell])
  prob <- next_entry$prob %||% rep(NA_real_, n)
  one_fraction <- as.vector(tapply(prob, cell, function(p) {
    all(is.finite(p)) && max(p) - min(p) <= 1e-12 * max(1, abs(p))
  })[cell])
  exact <- split & whole_below & one_fraction &
    identical(next_entry$kind, "equal_wor")
  out$factor[exact] <- sqrt(pmax(0, 1 - prob[exact]))
  out$uncorrected <- split & !exact
  out$below <- any(out$uncorrected & !whole_below)
  out
}

#' Warn that stage two kept no correction inside a certainty PSU
#'
#' Reads and removes the attribute `svrep_from_first_stage()` leaves.
#' @noRd
warn_certainty_lower_wr <- function(design, type) {
  info <- attr(design, "samplyr_certainty_lower_wr")
  attr(design, "samplyr_certainty_lower_wr") <- NULL
  if (is_null(info)) {
    return(design)
  }
  why <- if (info$below) {
    "A later stage samples inside its units, and stage {info$stage}'s
     correction would shrink that stage's variance too."
  } else {
    "Stage {info$stage}, drawn with {.val {info$method}}, has no single
     correction to restore there."
  }
  cli_warn(
    c(
      "{.fn as_svrepdesign} with {.val {type}} treats stage {info$stage}
       inside a PSU taken with certainty as drawn with replacement.",
      "i" = why,
      "i" = "The variance leaves out stage {info$stage}'s finite population
             correction there, so it errs toward too large when those
             sampling fractions are large.",
      "i" = "{.val mrbbootstrap} and {.val rwyb} use every stage's
             probabilities, and {.fn as_svydesign} gives the linearization
             variance."
    ),
    class = "samplyr_warning_replicate_certainty_stage_wr"
  )
  design
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

  # The zero rows of empty units follow the source rows and reach no target.
  source_rows <- seq_len(record$operator$n_source)
  analysis <- as.matrix(
    stats::weights(source_rep, type = "analysis")
  )[source_rows, , drop = FALSE]
  sampling <- stats::weights(source_rep, type = "sampling")[source_rows]

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
  result <- compress_replicate_weights(result)

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
#' It also exports a component settled from a reserve list, whose export adds
#' rows for the ineligible units and nonrespondents the settlement dropped
#' ([reserve_lists]). Each such row carries the key and membership of the
#' unit it stands for and composites with that unit's factor. Those values
#' are recorded when the list is settled, so the membership columns must be
#' on the list by then (`samplyr_error_stack_frames_membership_missing`).
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
    version = "4.5",
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
  # The rows a settled reserve adds composite with their units' membership.
  added <- lapply(stats::setNames(seq_along(x), names(x)), function(q) {
    m <- reserve_added_membership(x, names(x)[[q]], components[[q]])
    if (!is_null(m)) compositing_factors_of(m, q, theta)
  })

  report_share_coverage(
    combine_frame_replicates(x, components, factors, added),
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
      compositing_factors_of(
        frame_component_membership(component, membership), 1L, NULL
      )
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

  out <- lapply(seq_along(x), function(q) {
    compositing_factors_of(
      frame_component_membership(x[[q]], membership), q, theta
    )
  })
  names(out) <- names(x)
  out
}

#' The constant estimator's factors for the rows of a membership matrix
#'
#' The multiplicity form when `theta` is `NULL`, otherwise frame `q`'s share
#' of an overlapping unit.
#' @noRd
compositing_factors_of <- function(m, q, theta) {
  if (is_null(theta)) {
    return(1 / rowSums(m))
  }
  ifelse(rowSums(m) > 1, c(theta, 1 - theta)[[q]], 1)
}

#' The membership of the units a settled component's added rows stand for
#'
#' A settled reserve's export adds rows for the attempted units the sample
#' drops, and its settlement records those units with their columns. `NULL`
#' for a component whose export added no such row. Otherwise a logical
#' matrix with one row per exported row, `NA` on the sample's rows. A
#' recorded unit without a membership column, which was joined to the
#' sample after it was settled, is refused: its compositing factor would
#' be a guess.
#' @noRd
reserve_added_membership <- function(x, nm, component, call = caller_env()) {
  index <- attr(component, "samplyr_reserve_units")
  if (is_null(index) || all(is.na(index))) {
    return(NULL)
  }
  membership <- attr(x, "membership")
  units <- attr(x[[nm]], "metadata")$reserve_settlement$units$rows
  held <- !is.na(index)
  absent <- setdiff(unname(membership), names(units))
  m <- if (length(absent) == 0L) {
    frame_component_membership(units[index[held], , drop = FALSE], membership)
  }
  if (length(absent) > 0L || anyNA(m)) {
    missing <- c(absent, if (!is_null(m)) colnames(m)[apply(m, 2L, anyNA)])
    abort_samplyr(
      c(
        "Frame {.val {nm}}'s settled reserve drops units whose membership is
         not recorded.",
        "x" = "Its settlement kept no value of {.val {missing}} for the
               ineligible units and nonrespondents it drops.",
        "i" = "The export composites the row standing for each such unit
               with that unit's membership. Add the membership columns to
               the list before settling it."
      ),
      class = "samplyr_error_stack_frames_membership_missing",
      call = call
    )
  }
  out <- matrix(NA, length(index), length(membership),
                dimnames = list(NULL, names(membership)))
  out[held, ] <- m
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
combine_frame_replicates <- function(x, components, factors,
                                     added = NULL) {
  frames <- names(x)
  rows_of <- frame_stack_rows_of(x, components)
  sizes <- vapply(rows_of, length, integer(1))
  factors <- stats::setNames(lapply(frames, function(nm) {
    f <- factors[[nm]][rows_of[[nm]]]
    new <- is.na(rows_of[[nm]])
    # A row for a settled reserve's dropped unit takes that unit's factor,
    # and any other added row its own frame's.
    own <- added[[nm]][new] %||% rep(NA_real_, sum(new))
    f[new] <- ifelse(is.na(own), 1, own)
    f
  }), frames)
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
    cols <- col_start[[q]] + seq_len(widths[[q]])
    for (p in seq_along(frames)) {
      rows <- row_start[[p]] + seq_len(sizes[[p]])
      # Off the diagonal the base weights recycle down every column, which
      # fills the block without building it as a matrix first.
      repweights[rows, cols] <- if (identical(p, q)) {
        analysis[[q]]
      } else {
        base[[p]]
      }
    }
  }

  variables <- frame_stack_export_variables(x, components, rows_of)
  variables[[".weight"]] <- weights_vec
  rscales <- unlist(lapply(frames, function(nm) {
    components[[nm]]$scale * components[[nm]]$rscales
  }), use.names = FALSE)

  result <- survey::svrepdesign(
    data = variables,
    repweights = repweights,
    weights = weights_vec,
    # Blocks already contain full analysis weights.
    combined.weights = TRUE,
    type = "other",
    scale = 1,
    rscales = rscales,
    mse = TRUE
  )
  result <- compress_replicate_weights(result)

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

#' The stack's rows in the order its components exported them
#'
#' A zero row a component added belongs to its own frame alone, and keeps
#' the columns its export gave it. A row standing for a unit a settled
#' reserve dropped takes that unit's key, membership and declared overlaps
#' instead.
#' @noRd
frame_stack_export_variables <- function(x, components, rows_of) {
  whole <- as.data.frame(x)
  identity <- vapply(names(x), function(nm) {
    identical(rows_of[[nm]], seq_len(nrow(x[[nm]])))
  }, logical(1))
  if (all(identity)) {
    return(whole)
  }
  membership <- attr(x, "membership")
  start <- cumsum(c(0L, vapply(x, nrow, integer(1))))
  blocks <- lapply(seq_along(x), function(q) {
    nm <- names(x)[[q]]
    idx <- rows_of[[nm]]
    block <- whole[start[[q]] + idx, , drop = FALSE]
    zero <- is.na(idx)
    if (!any(zero)) {
      return(block)
    }
    exported <- components[[nm]]$variables[zero, , drop = FALSE]
    for (col in intersect(names(exported), names(block))) {
      value <- tryCatch(vctrs::vec_cast(exported[[col]], block[[col]]),
                        error = function(e) NULL)
      if (!is_null(value)) {
        block[[col]] <- vctrs::vec_assign(block[[col]], zero, value)
      }
    }
    own <- matrix(FALSE, sum(zero), length(membership),
                  dimnames = list(NULL, names(membership)))
    own[, nm] <- TRUE
    # A row for a settled reserve's dropped unit carries that unit's key,
    # membership and declared overlaps, and no other column of it.
    index <- attr(components[[nm]], "samplyr_reserve_units")[zero]
    held <- !is.na(index %||% rep(NA, sum(zero)))
    if (any(held)) {
      units <- attr(x[[nm]], "metadata")$reserve_settlement$units$rows
      cols <- intersect(
        c(attr(x, "key"), unname(membership),
          unlist(attr(x, "overlaps")$cols, use.names = FALSE)),
        intersect(names(units), names(block))
      )
      at <- which(zero)[held]
      for (col in cols) {
        block[[col]][at] <- vctrs::vec_cast(units[[col]][index[held]],
                                            block[[col]])
      }
      own[held, ] <- frame_component_membership(
        units[index[held], , drop = FALSE], membership
      )
    }
    for (f in names(membership)) {
      block[[membership[[f]]]][zero] <- own[, f]
    }
    block$.frame[zero] <- nm
    block$.domain[zero] <- frame_domain_labels(own)
    block
  })
  out <- vctrs::vec_rbind(!!!blocks)
  rownames(out) <- NULL
  out
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
  check_reserve_settled(.data, "as_survey_design")

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
  check_reserve_settled(.data, "as_survey_rep")

  rep_design <- as_svrepdesign(.data, ...)
  srvyr::as_survey_rep(rep_design)
}

## Linearized export of a shared-weight sample

#' Say what the export does with source units linked to no target
#'
#' A zero row keeps an unlinked source unit in the variance as a zero total
#' and needs `na.rm = TRUE`.
#' @noRd
warn_share_unlinked <- function(n_zero, call = caller_env()) {
  if (n_zero > 0L) {
    cli_warn(
      c(
        "{n_zero} selected source unit{?s} {?is/are} linked to no target, so
         the export adds a zero row for {?it/each}.",
        "i" = "A zero row has weight 0, so totals and counts are unchanged,
               and it keeps the unit in the variance as a zero total.",
        "i" = "Its target variables hold {.code NA}, so estimates need
               {.code na.rm = TRUE}."
      ),
      class = "samplyr_warning_export_empty_parent",
      n_empty = n_zero,
      call = call
    )
  }
  invisible(NULL)
}

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
  warn_share_unlinked(length(missing_source), call = call)
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
  # Source units found empty or linked to no target keep a zero row.
  zero <- seq_len(nrow(parts$df)) > nrow(source_sample)
  zero[missing_source] <- TRUE
  if (any(zero)) {
    rows <- parts$df[zero, , drop = FALSE]
    rows[carried] <- vctrs::vec_init(target[carried], sum(zero))
    rows$.weight <- 0
    expanded <- vctrs::vec_rbind(expanded, rows)
  }
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
  df <- export_rows(source_sample, design, stages, call = call)

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
