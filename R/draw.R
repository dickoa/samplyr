#' Selection methods
#'
#' The sixteen selection methods samplyr ships, what each one requires, and
#' how registered methods extend the set. [draw()] chooses among them with
#' its `method` argument.
#'
#' Sixteen methods are built in, in three families: equal probability, PPS
#' (probability proportional to size, all requiring `mos`), and balanced.
#'
#' | Method | Replacement | Size | `mos` | Other input | Notes |
#' |--------|-------------|------|-------|-------------|-------|
#' | `srswor` | Without | Fixed | - | - | The default. Standard SRS |
#' | `srswr` | With | Fixed | - | - | Allows duplicates |
#' | `systematic` | Without | Fixed | - | - | Periodic selection |
#' | `bernoulli` | Without | Random | - | `prn` | Independent trial per unit |
#' | `pps_systematic` | Without | Fixed | Required | - | Order-sensitive, exact first-order probabilities |
#' | `pps_brewer` | Without | Fixed | Required | - | Exact first-order, approximate joint probabilities |
#' | `pps_cps` | Without | Fixed | Required | - | Highest entropy, exact joint probabilities |
#' | `pps_sampford` | Without | Fixed | Required | - | Exact Sampford joint probabilities |
#' | `pps_poisson` | Without | Random | Required | `prn` | PPS analog of Bernoulli |
#' | `pps_sps` | Without | Fixed | Required | `prn` | Sequential Poisson with approximate probability targets |
#' | `pps_pareto` | Without | Fixed | Required | `prn` | Pareto with approximate probability targets |
#' | `pps_multinomial` | With | Fixed | Required | - | Any hit count, Hansen-Hurwitz |
#' | `pps_chromy` | Min. repl. | Fixed | Required | - | As SAS `PPS_SEQ` |
#' | `cube` | Without | Fixed | Optional | `aux` optional | Deville & \enc{Tillé}{Tille} 2004 |
#' | `lpm2` | Without | Fixed | Optional | `spread` required | Spatial spread |
#' | `scps` | Without | Fixed | Optional | `spread` required, `prn` | Spatial spread |
#'
#' Every method takes either `n` or `frac`, except `pps_cps`, which requires
#' `n`. For fixed-size methods, `frac` follows the `round` parameter (ceiling
#' by default). Bernoulli and PPS Poisson use an unrounded expected target.
#' The `prn` column marks the methods that accept permanent random
#' numbers for coordination. It is always optional.
#'
#' "Min. repl." is probability minimum replacement: `pps_chromy` draws a unit
#' either \eqn{\lfloor E \rfloor}{floor(E)} or \eqn{\lceil E \rceil}{ceiling(E)}
#' times, where \eqn{E} is its expected number of hits, so a unit is never hit
#' more often than its size warrants.
#'
#' ## Selection and inference
#'
#' Every method above draws a sample when its inputs are met, but selection
#' support and variance support are separate contracts. [variance-estimation]
#' gives how each method's variance is estimated and how exact it is.
#'
#' SPS, Pareto and custom approximate targets require
#' `allow_approximate = TRUE` in [exante_probabilities()] and
#' [exante_overlaps()]. Estimates using these targets need not be
#' design-unbiased. Unknown probabilities are refused. Probability and
#' variance declarations by a custom-method author are contracts, not proofs.
#'
#' The two order-sampling methods part ways in small pools. Drawing 3 of 12
#' units with sizes from 1 to 20, over 20,000 draws, `pps_sps` realized
#' inclusion probabilities up to 0.047 away from its targets, and a
#' Horvitz-Thompson count built on the targets came out 6.6% low. `pps_pareto`
#' stayed within 0.009 of its targets, with a bias under 1%. For coordinated
#' samples from small pools, prefer `pps_pareto`.
#'
#' ## Fixed vs random sample size
#'
#' Where the table says **Fixed**, `n` is the realized sample size. Where it
#' says **Random**, `n` is the *expected* size: it is converted to
#' `frac = n / N` (with `N` the stratum or frame size) and the realized count
#' varies around it.
#'
#' For `pps_poisson`, the raw inclusion probabilities are computed as
#' \eqn{\pi_i = f \cdot x_i / \bar{x}}{pi_i = f * x_i / mean(x)} where
#' \eqn{f} is `frac` and \eqn{x_i} is the MOS value. Any \eqn{\pi_i > 1}
#' is clipped to 1, so the expected sample size
#' \eqn{E[n] = \sum \min(\pi_i, 1)}{E[n] = sum(min(pi_i, 1))} can be less
#' than \eqn{f \cdot N}{f * N} when large units dominate the MOS
#' distribution. Use `certainty_size` or `certainty_prop` to handle these
#' dominant units explicitly.
#'
#' Declaring certainty units does more than remove them. The remainder is
#' re-resolved over the reduced target and the reduced MOS total, so the
#' surviving chances can rise. The remaining expected take equals the reduced target
#' only if no remaining probability needs clipping. See [draw()] for the
#' certainty-adjusted `n` and `frac` contract.
#'
#' This is not silent. `execute()` warns with class
#' `samplyr_warning_poisson_shortfall` once per stage when a pool's resolved
#' expectation falls more than 5% below what that pool could reach, naming the
#' pools affected and how many chances were clipped. The comparison is against
#' the reachable target rather than the request: a pool asked for more units
#' than it holds has already had its target reduced by the population, which
#' `samplyr_warning_nominal_cap` reports, and only the further reduction that
#' saturation caused is charged here. A design reduced both ways gets both
#' warnings.
#'
#' The shortfall is a gap between the nominal and realized design, not a bias.
#' Horvitz-Thompson estimates from a saturated Poisson design remain unbiased,
#' because the weights are the reciprocals of the resolved probabilities.
#'
#' When an allocation method is set in [stratify_by()] (`equal`,
#' `proportional`, `neyman`, `optimal`, `power`), specify total sample size via `n`.
#' Combining `alloc` with `frac` is not supported.
#'
#'
#' @references
#' `srswor`, `srswr`, `systematic`, `bernoulli`, `pps_systematic`,
#' `pps_multinomial`:
#' Cochran, W.G. (1977). *Sampling Techniques*, 3rd ed. Wiley.
#'
#' `pps_brewer`:
#' Brewer, K.R.W. (1975). A simple procedure for sampling PPS WOR.
#' *Australian Journal of Statistics*, 17(3), 166-172.
#'
#' `pps_cps`:
#' \enc{Hájek}{Hajek}, J. (1964). Asymptotic theory of rejective sampling with varying
#' probabilities from a finite population.
#' *Annals of Mathematical Statistics*, 35(4), 1491-1523.
#'
#' Chen, X.-H., Dempster, A.P. and Liu, J.S. (1994). Weighted finite
#' population sampling to maximize entropy. *Biometrika*, 81(3), 457-469.
#'
#' `pps_sampford`:
#' Sampford, M.R. (1967). On sampling without replacement with unequal
#' probabilities of selection. *Biometrika*, 54(3/4), 499-513.
#'
#' `pps_poisson`:
#' \enc{Tillé}{Tille}, Y. (2006). *Sampling Algorithms*. Springer.
#'
#' `pps_sps`:
#' Ohlsson, E. (1998). Sequential Poisson sampling.
#' *Journal of Official Statistics*, 14(2), 149-162.
#'
#' `pps_pareto`:
#' \enc{Rosén}{Rosen}, B. (1997). Asymptotic theory for order sampling.
#' *Journal of Statistical Planning and Inference*, 62(2), 135-158.
#'
#' `pps_chromy`:
#' Chromy, J.R. (1979). Sequential sample selection methods.
#' *Proceedings of the Survey Research Methods Section, ASA*, 401-406.
#'
#' `balanced`:
#' Deville, J.-C. and \enc{Tillé}{Tille}, Y. (2004). Efficient balanced
#' sampling: the cube method. *Biometrika*, 91(4), 893-912.
#'
#' Chauvet, G. (2009). Stratified balanced sampling.
#' *Survey Methodology*, 35(1), 115-119.
#'
#' `lpm2`:
#' \enc{Grafström}{Grafstrom}, A., \enc{Lundström}{Lundstrom}, N.L.P. and
#' Schelin, L. (2012). Spatially balanced sampling through the pivotal method.
#' *Biometrics*, 68(2), 514-520. \doi{10.1111/j.1541-0420.2011.01699.x}
#'
#' `scps`:
#' \enc{Grafström}{Grafstrom}, A. (2012). Spatially correlated Poisson
#' sampling. *Journal of Statistical Planning and Inference*, 142(1),
#' 139-147. \doi{10.1016/j.jspi.2011.07.003}
#'
#' \enc{Grafström}{Grafstrom}, A. and Matei, A. (2018). Coordination of
#' spatially balanced samples. *Survey Methodology*, 44(2), 215-238.
#'
#' Matei, A., Smith, P.A., Smeets, M.J.E. and Klingwort, J. (2023).
#' Targetted double control of burden in multiple surveys. *Survey
#' Methodology*, 49(2), 363-384.
#'
#'
#' @name selection-methods
#' @family design specification
#' @seealso [draw()] to set a method on a stage,
#'   [variance-estimation] for how each method's variance is estimated,
#'   [joint_expectation()] for which methods yield exact second-order
#'   quantities
NULL

#' Specify how units are selected
#'
#' `draw()` sets the sample size or sampling fraction, the selection method,
#' and whatever that method needs: a measure of size for PPS, auxiliary
#' variables for balanced sampling, coordinates for spatial spread. Every
#' stage in a sampling design ends with `draw()`, which closes it. A second
#' `draw()`, or a [stratify_by()] or [cluster_by()] after it, is refused, and
#' [sampling_design()] describes the stage grammar.
#'
#' @param .data A `sampling_design` object (piped from [sampling_design()],
#'   [add_stage()], [stratify_by()], or [cluster_by()]).
#' @param n Sample size. For random-size methods (`bernoulli`, `pps_poisson`),
#'   `n` is the **expected** sample size, converted to `frac = n / N`.
#'   [selection-methods] explains the fixed versus random distinction and the
#'   warning raised when `pps_poisson` falls short of `n`. Can be:
#'   - A scalar: applies per stratum (if no `alloc`) or as total (if `alloc` specified)
#'   - A named vector: stratum-specific sizes (for single stratification
#'     variable). A name, or a data frame row, for a stratum the stage cannot
#'     reach is refused at execution (`samplyr_error_alloc_unknown_strata`).
#'   - A data frame: stratum-specific sizes, with every stratification column
#'     of [stratify_by()] and an `n` column. For a take per parent unit,
#'     stratify the stage by the parent's cluster id:
#'     `add_stage() |> stratify_by(ea_id) |> draw(n = take)`. The table may
#'     cover every parent of the design, and a continuation on a listing of
#'     the selected parents uses only their rows. Entries for parents the
#'     sample did not select are not checked, so a mistyped identifier that
#'     matches no selected parent passes unnoticed.
#'   - A svyplan size or plan object, read through svyplan's documented
#'     coercions and stage-aware for cluster and stratified two-stage plans.
#'     A plan giving one size for a stage stratified without `alloc` is
#'     refused (`samplyr_error_svyplan_total_per_stratum`) rather than taken
#'     in every stratum.
#'   - A certainty-aware `svyplan::n_alloc()` plan (solved with a `psu`
#'     register carrying `psu_id`), at a clustered, stratified stage 1 and
#'     again at stage 2 for the per-PSU takes. Stage 1 takes every certainty
#'     PSU and draws exactly the plan's remainder per stratum. A plan solved
#'     with `n_psu_per_zone = 2` draws two PSUs from each of its zones
#'     instead, records the zone in `.zone_1`, and exports one variance
#'     stratum per zone. With `n_psu_per_zone = 1` it draws one PSU per zone,
#'     records the plan's variance group in `.pair_1`, and exports the
#'     groups as variance strata ([variance-estimation]). Its method
#'     must be `pps_systematic`, `pps_brewer`, `pps_cps`, or `pps_sampford`,
#'     with `mos` equal to the register's `N`. Stage 2 selects by `srswor` or
#'     `systematic`, and stratifying there requires an `alloc` method.
#'     Arguments the plan owns (`frac`, `certainty_size`, `certainty_prop`,
#'     `min_n`, `max_n`, and at stage 1 `alloc`) are refused alongside it.
#'     [svyplan::merge_psus()] merges PSUs smaller than the take.
#' @param frac Sampling fraction: a scalar for all strata, a named vector of
#'   stratum-specific fractions, or a data frame with every stratification
#'   column and a `frac` column. Give `n` or `frac`, not both.
#' @param ... These dots are for future extensions and must be empty.
#'   Arguments after `...` must be named in full, so a positional fourth
#'   argument is refused rather than taken for `min_n`.
#' @param min_n Minimum sample size per stratum, `NULL` (no minimum) by
#'   default. When an allocation method would assign fewer units to a
#'   stratum, the stratum is raised to `min_n` and the other strata give up
#'   the difference. For without-replacement designs a `min_n` above a
#'   stratum's population makes that stratum a census. `min_n` counts the
#'   stratum's whole take, certainty units included, so a stratum can hold
#'   fewer than `min_n` units drawn outside certainty, and `execute()` names
#'   one left with a single such unit (`samplyr_message_singleton_pool`).
#'   Allocations that give a nonempty stratum zero units are refused.
#'   `min_n = 1` requests positive allocations explicitly. For minimums that
#'   differ by stratum, compute the sizes and pass them as a named `n` or a
#'   data frame.
#' @param max_n Maximum sample size per stratum, `NULL` (no maximum) by
#'   default. When an allocation method would assign more, the stratum is
#'   capped and the surplus goes to the other strata.
#'
#'   Both bounds apply only with an allocation method in [stratify_by()],
#'   and narrow a range the stratum population already caps. The
#'   "Population bounds and redistribution" section of [stratify_by()] gives
#'   how the surplus is shared and why with-replacement methods differ.
#'   `frame_summary(design, frame, detail = "pool")` reports the per-stratum
#'   target each bound produces, without selecting or consuming random
#'   numbers.
#' @param method Selection method, `"srswor"` by default.
#'   [selection-methods] lists the sixteen built-ins with what each one
#'   requires and the paper behind each.
#'
#'   `"cube"` balances the sample on `aux` so that Horvitz-Thompson
#'   estimates of auxiliary totals match population totals, with equal or
#'   unequal (`mos`) inclusion probabilities, and uses the stratified cube
#'   algorithm (Chauvet 2009) when stratified. At most two stages may use a
#'   balanced-family method. `"balanced"` is a compatibility alias for
#'   `"cube"`.
#'
#'   A method registered with [sondage::register_method()] is named
#'   `"pps_<name>"` for `type = "wor"` or `type = "wr"`, and
#'   `"balanced_<name>"` for `type = "balanced"`, where `mos` is optional.
#'   `supports_aux = TRUE` permits ordinary balancing variables in `aux`, and
#'   `supports_spread = TRUE` requires coordinates in `spread`.
#'
#'   Its weights are `1 / pik`, the inclusion expectation (target inclusion
#'   probabilities, or expected hits for `type = "wr"`) samplyr hands to the
#'   method. The `probabilities` tier declared at registration is `"exact"`
#'   (the design's first-order inclusion probabilities, or expected hits,
#'   equal `pik`), `"approximate"` (to a documented approximation, as in
#'   Pareto sampling), or `"unknown"`, the default. `draw()` refuses
#'   `"unknown"`, because `pik` is then a selection weight only and the
#'   weights would be systematically biased. The classic trap is
#'   `sample(prob = pik)`. With `replace = TRUE` its expected hits equal
#'   `pik`, a valid `type = "wr"` method, but its inclusion probabilities
#'   without replacement do not, so a `type = "wor"` wrapper's tier is
#'   unknown.
#'
#' @param mos Measure of size, a bare column name. Required for built-in PPS
#'   methods and registered `pps_` methods. Optional for `cube`, `lpm2`,
#'   `scps`, and registered `balanced_` methods, which use equal inclusion
#'   probabilities without it. A column name held in a variable is written
#'   `.data[[v]]`, here and in `prn`, `aux` and `spread`. A quoted string is
#'   refused with class `samplyr_error_draw_string_column`.
#' @param prn Permanent random numbers for sample coordination, a bare
#'   numeric column with values in the open interval (0, 1) and no missing
#'   values. Supported by `"bernoulli"`, `"pps_poisson"`, `"pps_sps"`,
#'   `"pps_pareto"`, and `"scps"`. The sample is then deterministic for a
#'   given set of PRN values, which coordinates samples across survey waves.
#'   With `"scps"` it is also fixed by the order the pool's units are
#'   visited in, the frame's row order or the `control` order, so
#'   coordinated draws need the same order.
#' @param aux Cube balancing declarations for `method = "cube"`, or ordinary
#'   balancing variables for a registered balanced method that declares
#'   `supports_aux = TRUE`. Bare numeric columns, such as
#'   `aux = c(income, pop_density)`, request approximate Horvitz-Thompson
#'   total balance. A [bound()] marker requests adjacent-integer count bounds
#'   for every observed category, as in
#'   `aux = c(income, bound(region), bound(urban_rural))`, with a separate
#'   `bound()` for each marginal constraint. With `cluster_by()`, ordinary
#'   auxiliary values are summed to cluster level, while bound variables
#'   must be constant within each cluster.
#' @param spread Spatial coordinates for `method = "lpm2"` or `"scps"`, or
#'   for a registered balanced method declaring `supports_spread = TRUE`,
#'   which requires them. Bare numeric columns such as
#'   `spread = c(longitude, latitude)`, finite, with no missing values, and
#'   placed on comparable scales. With `cluster_by()`, coordinates must be
#'   constant within each cluster.
#' @param round Rounding when `frac` is converted to sample sizes. One of:
#'   - `"up"` (default): ceiling, the SAS SURVEYSELECT default.
#'   - `"down"`: floor.
#'   - `"nearest"`: nearest integer, halves up.
#'
#'   An `n` given directly is not rounded. A product within floating-point
#'   error of an integer counts as that integer, so `frac = 0.07` of 100
#'   units draws 7 under every rule. After rounding, every stratum or group
#'   receives at least 1 unit.
#'
#' @param control <[`data-masking`][dplyr::dplyr_data_masking]> Variables for
#'   sorting the frame before selection. Can be:
#'   - A single variable: `control = region`
#'   - Multiple variables: `control = c(region, district)`
#'   - With [serp()] for serpentine sorting: `control = serp(region, district)`
#'   - With [dplyr::desc()] for descending: `control = c(region, desc(population))`
#'   - Mixed: `control = c(region, serp(district, commune), desc(size))`
#'
#'   Character values sort by their bytes in UTF-8, as in the C locale, so
#'   the order and the sample do not depend on the session locale. Missing
#'   values sort last, also under `desc()`. Ties keep the frame's order. See
#'   the "Control sorting" section.
#'
#' @param certainty_size For PPS without-replacement methods, units with
#'   MOS >= this value are selected with certainty (probability 1). A scalar
#'   for all strata, or a data frame with the stratification columns and a
#'   `certainty_size` column. Certainty units are removed from the frame
#'   before probability sampling, which draws the remaining sample size.
#'   Mutually exclusive with `certainty_prop`. Equivalent to SAS
#'   SURVEYSELECT `CERTSIZE=`.
#'
#' @param certainty_prop For PPS without-replacement methods, units whose
#'   MOS share (MOS_i / sum(MOS)) >= this value are selected with certainty.
#'   A scalar strictly between 0 and 1, or a data frame with the
#'   stratification columns and a `certainty_prop` column. Shares are
#'   recomputed after removing certainty units, until no new unit qualifies.
#'   Mutually exclusive with `certainty_size`.
#'
#' @param certainty_overflow What to do when certainty units exceed the
#'   target sample size of a sampling pool (a stratum within its parent,
#'   when applicable). One of:
#'   - `"error"` (default): stop with an error.
#'   - `"allow"`: permit a census above the target only when every unit in
#'     that pool is certain. All units in the pool get stage weight 1.
#'
#'   Under either setting, certainty units that exactly exhaust or exceed
#'   the target are refused if any noncertainty units remain in the pool,
#'   because those units would have zero inclusion probability.
#'
#' @param on_empty What to do when this stage selects nothing or has nothing
#'   to select from: a random-size method (`bernoulli`, `pps_poisson`, or a
#'   custom method registered with `fixed_size = FALSE`) that selects zero
#'   units in a stratum or the whole frame, or a unit selected at the
#'   previous stage with no rows in this stage's frame, such as a household
#'   with no eligible member. One of:
#'   - `"error"` (default): stop with an error. Zero selections usually
#'     signal a design problem, such as a sampling fraction or a stratum
#'     that is too small.
#'   - `"warn"`: warn and keep the empty selection.
#'   - `"silent"`: keep the empty selection without a message.
#'
#'   An empty selection is a valid realization of a random-size design. It
#'   contributes zero to Horvitz-Thompson totals, so estimates over repeated
#'   executions remain unbiased. Later stages of a multistage design then
#'   have nothing to select from, and the sample is empty. `"warn"` and
#'   `"silent"` are meant for simulation and replicated runs, so check
#'   `nrow()` before analyzing a single realization.
#'
#'   A selected unit with no rows in the next frame is refused under
#'   `"error"` (`samplyr_error_frame_missing_parent`). Under `"warn"`
#'   (`samplyr_warning_empty_parent`, once per stage) or `"silent"` it stays
#'   in the design with nothing selected under it and contributes zero to
#'   every total. It is recorded in the sample, counted by [summary()],
#'   exempt from the incomplete-register checks of [validate_frame()] and
#'   [execute()], and named at export by [as_svydesign()].
#'   [variance-estimation] gives how the export treats such units, and why
#'   a primary unit left with no row is refused.
#'
#'   A replicated execution with empty replicates cannot feed a later
#'   [execute()] call (a new phase or a stage continuation). It raises
#'   `samplyr_error_empty_phase_replicate` rather than skip them and
#'   condition downstream results on nonempty realizations. Handle them
#'   explicitly, for example by executing each nonempty replicate separately
#'   and accounting for the empty ones in the analysis.
#'
#' @return A modified `sampling_design` object with selection parameters specified.
#'
#' @details
#' ## Certainty selection
#'
#' In PPS without-replacement sampling, very large units can have
#' theoretical inclusion probabilities above 1. Certainty selection takes
#' them with probability 1 before sampling the remainder, and marks them in
#' the `.certainty_k` column described in [sample-columns]. It is available
#' for the WOR PPS methods (`pps_systematic`, `pps_brewer`, `pps_cps`,
#' `pps_sampford`, `pps_poisson`, `pps_sps`, `pps_pareto`). With-replacement
#' (`pps_multinomial`) and PMR (`pps_chromy`) methods handle large units
#' through their hit mechanism. A pool censused under
#' `certainty_overflow = "allow"` has stage weight 1, but in a multistage
#' design the final `.weight` can still exceed 1 because it compounds all
#' stage weights.
#'
#' **Certainty with `pps_poisson`.** Both `n` and `frac` give a target
#' expected total that includes certainty units, `frac` as the unrounded
#' `frac * N` over the original pool. After `n_cert` certainty units are
#' selected, the remaining probabilities are
#' `pmin(1, (target - n_cert) * mos / sum(mos))` over the remaining units'
#' sizes. Thus `n = 5` and `frac = 0.5` on ten units give the same
#' probabilities, with or without a certainty rule. Clipping at one does not
#' redistribute the excess to other units, so the expected size can fall
#' below the target. [frame_summary()] reports both `n_target` and
#' `n_expected`.
#'
#' ## Control sorting
#'
#' Control sorting orders the frame before selection, giving implicit
#' stratification. It is most effective with the systematic and sequential
#' methods (`systematic`, `pps_systematic`, `pps_chromy`), where the sample
#' spreads evenly across the sorted variables.
#'
#' Nested sorting (the default, `control = c(var1, var2, var3)`) sorts
#' ascending by each variable in turn. Serpentine sorting
#' (`control = serp(var1, var2, var3)`) alternates direction at each
#' hierarchy level, which minimizes the jumps between adjacent units. For a
#' geographic hierarchy, the last district of region 1 is then adjacent to
#' the last district of region 2.
#'
#' With [stratify_by()], sorting is applied within each stratum, combining
#' explicit stratification for variance control with implicit
#' stratification for sample spread.
#'
#' @examples
#' # Simple random sample of 100 EAs
#' sampling_design() |>
#'   draw(n = 100) |>
#'   execute(bfa_eas, seed = 1)
#'
#' # Systematic sample of 10%
#' sampling_design() |>
#'   draw(frac = 0.10, method = "systematic") |>
#'   execute(bfa_eas, seed = 123)
#'
#' # PPS sample of EAs using household count
#' sampling_design() |>
#'   cluster_by(ea_id) |>
#'   draw(n = 50, method = "pps_brewer", mos = households) |>
#'   execute(bfa_eas, seed = 42)
#'
#' # Bernoulli sampling with frac (random sample size, expected ~5%)
#' sampling_design() |>
#'   draw(frac = 0.05, method = "bernoulli") |>
#'   execute(ken_enterprises, seed = 12345)
#'
#' # Bernoulli sampling with expected n (converted to frac = 500/N)
#' sampling_design() |>
#'   draw(n = 500, method = "bernoulli") |>
#'   execute(bfa_eas, seed = 42)
#'
#' # Stratified with different sizes per stratum (data frame)
#' region_sizes <- data.frame(
#'   region = levels(bfa_eas$region),
#'   n = c(20, 12, 25, 18, 22, 16, 14, 15, 20, 18, 12, 10, 8)
#' )
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = region_sizes) |>
#'   execute(bfa_eas, seed = 123)
#'
#' # Stratified with different rates per stratum (named vector)
#' sampling_design() |>
#'   stratify_by(size_class) |>
#'   draw(frac = c(Small = 0.02, Medium = 0.10, Large = 0.50)) |>
#'   execute(ken_enterprises, seed = 42)
#'
#' # Neyman allocation with minimum 2 per stratum (for variance estimation)
#' sampling_design() |>
#'   stratify_by(region, alloc = "neyman", variance = bfa_eas_variance) |>
#'   draw(n = 150, min_n = 2) |>
#'   execute(bfa_eas, seed = 2026)
#'
#' # Proportional allocation with min and max bounds
#' sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 200, min_n = 10, max_n = 50) |>
#'   execute(bfa_eas, seed = 1)
#'
#' # Control sorting with serpentine ordering (implicit stratification)
#' sampling_design() |>
#'   draw(n = 100, method = "systematic",
#'        control = serp(region, province)) |>
#'   execute(bfa_eas, seed = 2)
#'
#' # Control sorting with nested (standard) ordering
#' sampling_design() |>
#'   draw(n = 100, method = "systematic",
#'        control = c(region, province)) |>
#'   execute(bfa_eas, seed = 3)
#'
#' # Combined explicit stratification with control sorting within strata
#' sampling_design() |>
#'   stratify_by(urban_rural) |>
#'   draw(n = 50, method = "systematic",
#'        control = serp(region, province)) |>
#'   execute(bfa_eas, seed = 25)
#'
#' # PPS with certainty selection (absolute threshold)
#' # Large EAs selected with certainty, rest sampled with PPS
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 100, method = "pps_brewer", mos = households,
#'        certainty_size = 800) |>
#'   execute(bfa_eas, seed = 3)
#'
#' # PPS with certainty selection (proportional threshold)
#' # EAs with >= 10% of stratum total selected with certainty
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 100, method = "pps_systematic", mos = households,
#'        certainty_prop = 0.10) |>
#'   execute(bfa_eas, seed = 321)
#'
#' # Stratum-specific certainty thresholds (data frame)
#' cert_thresholds <- data.frame(
#'   region = levels(bfa_eas$region),
#'   certainty_size = c(700, 450, 800, 850, 750, 800, 550,
#'                      450, 700, 950, 750, 600, 480)
#' )
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 100, method = "pps_brewer", mos = households,
#'        certainty_size = cert_thresholds) |>
#'   execute(bfa_eas, seed = 424)
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [stratify_by()] for stratification,
#' [cluster_by()] for clustering,
#' [execute()] for running designs,
#' [serp()] for serpentine sorting,
#' [variance-estimation] for how each method's variance is estimated
#'
#' @family design specification
#' @export
draw <- function(
  .data,
  n = NULL,
  frac = NULL,
  ...,
  min_n = NULL,
  max_n = NULL,
  method = "srswor",
  mos = NULL,
  prn = NULL,
  aux = NULL,
  spread = NULL,
  round = "up",
  control = NULL,
  certainty_size = NULL,
  certainty_prop = NULL,
  certainty_overflow = "error",
  on_empty = "error"
) {
  # Match all modifiers after `...` exactly.
  check_keyword_args(
    enquos(...),
    c(
      "min_n", "max_n", "method", "mos", "prn", "aux", "spread", "round",
      "control", "certainty_size", "certainty_prop", "certainty_overflow",
      "on_empty"
    )
  )
  if (is.data.frame(.data)) {
    abort_frame_misplaced("draw")
  }
  if (!is_sampling_design(.data)) {
    cli_abort(
      "{.arg .data} must be a {.cls sampling_design} object",
      class = "samplyr_error_design_expected"
    )
  }
  check_stage_open(.data, "draw")

  mos_quo <- enquo(mos)
  mos_name <- if (quo_is_null(mos_quo)) {
    NULL
  } else {
    draw_column_name(quo_get_expr(mos_quo), quo_get_env(mos_quo), "mos")
  }

  prn_quo <- enquo(prn)
  prn_name <- if (quo_is_null(prn_quo)) {
    NULL
  } else {
    draw_column_name(quo_get_expr(prn_quo), quo_get_env(prn_quo), "prn")
  }

  control_quo <- enquo(control)
  control_quos <- if (quo_is_null(control_quo)) {
    NULL
  } else {
    control_expr <- quo_get_expr(control_quo)
    control_env <- quo_get_env(control_quo)

    if (is_call(control_expr, "c")) {
      lapply(as.list(control_expr)[-1], function(expr) {
        new_quosure(expr, control_env)
      })
    } else {
      list(control_quo)
    }
  }

  aux_spec <- parse_balanced_aux(enquo(aux))
  aux_names <- aux_spec$aux
  bound_names <- aux_spec$bounds
  spread_names <- parse_draw_variables(enquo(spread), "spread")

  resolved_method <- resolve_draw_method(method)
  method <- resolved_method$method
  custom_spec <- resolved_method$custom_spec

  valid_round <- c("up", "down", "nearest")
  if (!is_character(round) || length(round) != 1) {
    cli_abort(
      "{.arg round} must be a single character string",
      class = "samplyr_error_draw_argument"
    )
  }
  round <- with_error_class(
    rlang::arg_match(round, valid_round),
    "samplyr_error_draw_argument"
  )

  current <- .data$current_stage
  if (current < 1 || current > length(.data$stages)) {
    cli_abort(
      "Invalid design state: no current stage",
      class = "samplyr_error_internal"
    )
  }

  current_stage <- .data$stages[[current]]
  has_alloc <- !is_null(current_stage$strata) &&
    !is_null(current_stage$strata$alloc)

  strata_vars <- current_stage$strata$vars

  # A certainty plan is fielded from its own classification, never coerced.
  certainty_plan <- NULL
  if (is_certainty_alloc_plan(n)) {
    if (current == 1L && !is_null(current_stage$clusters)) {
      bridge <- certainty_bridge_spec(
        plan = n,
        strata_vars = strata_vars,
        cluster_vars = current_stage$clusters$vars,
        method = method,
        certainty_size = certainty_size,
        certainty_prop = certainty_prop,
        min_n = min_n,
        max_n = max_n,
        has_alloc = has_alloc
      )
      certainty_plan <- bridge$spec
      n <- bridge$n_total
    } else if (current == 2L) {
      certainty_plan <- certainty_take_spec(
        plan = n,
        design = .data,
        method = method,
        mos = mos_name,
        frac = frac,
        certainty_size = certainty_size,
        certainty_prop = certainty_prop,
        min_n = min_n,
        max_n = max_n,
        has_alloc = has_alloc,
        strata_vars = strata_vars,
        clustered = !is_null(current_stage$clusters)
      )
      n <- NULL
    }
  }

  from_plan <- inherits(n, c("svyplan_n", "svyplan_power", "svyplan_cluster"))
  n <- coerce_svyplan_n(
    n,
    stage_index = current,
    clustered = !is_null(current_stage$clusters)
  )
  # Only draw() knows a scalar came from a plan.
  if (
    from_plan && !is.data.frame(n) && length(n) == 1L && is_null(names(n)) &&
      !is_null(strata_vars) && !has_alloc
  ) {
    abort_samplyr(
      c(
        "{.fn draw} got one sample size ({n}) from a svyplan plan at a stage
         stratified by {.val {strata_vars}} with no {.arg alloc}.",
        "x" = "Without {.arg alloc} a single {.arg n} is taken in every
               stratum, which multiplies the planned size by the number of
               strata.",
        "i" = "Add {.arg alloc} to {.fn stratify_by} to distribute it, or
               plan per stratum with {.fn svyplan::n_alloc}.",
        "i" = "To take {n} in every stratum on purpose, pass
               {.code n = {n}}."
      ),
      class = "samplyr_error_svyplan_total_per_stratum"
    )
  }
  # svyplan names each stratum in one column.
  if (
    from_plan && !is.data.frame(n) && length(n) > 1L && !is_null(names(n)) &&
      length(strata_vars) > 1L
  ) {
    abort_samplyr(
      c(
        "This svyplan plan names its strata in one column, and this stage
         is stratified by {.val {strata_vars}}.",
        "i" = "Stratify by the one column the plan was built on, for example
               a combined {.code paste(region, urban_rural)}, or give
               {.arg n} as a data frame built from the plan, with columns
               {.val {strata_vars}}, plus an {.val n} column."
      ),
      class = "samplyr_error_svyplan_multivariable_strata"
    )
  }
  valid_on_empty <- c("warn", "error", "silent")
  if (
    !is_character(on_empty) ||
      length(on_empty) != 1 ||
      !on_empty %in% valid_on_empty
  ) {
    cli_abort(
      "{.arg on_empty} must be one of {.val {valid_on_empty}}",
      class = "samplyr_error_draw_argument"
    )
  }

  certainty_overflow <- with_error_class(
    rlang::arg_match(certainty_overflow, c("error", "allow")),
    "samplyr_error_draw_argument"
  )

  validate_draw_configuration(
    n = n,
    frac = frac,
    method = method,
    mos = mos_name,
    prn = prn_name,
    min_n = min_n,
    max_n = max_n,
    certainty_size = certainty_size,
    certainty_prop = certainty_prop,
    round = round,
    certainty_overflow = certainty_overflow,
    on_empty = on_empty,
    has_alloc = has_alloc,
    strata_vars = strata_vars,
    aux = aux_names,
    bounds = bound_names,
    spread = spread_names,
    custom_spec = custom_spec,
    certainty_plan = certainty_plan,
    parent_vars = collect_ancestor_cluster_vars(.data, current)
  )

  draw_spec <- new_draw_spec(
    n = n,
    frac = frac,
    method = method,
    mos = mos_name,
    prn = prn_name,
    aux = aux_names,
    bounds = bound_names,
    spread = spread_names,
    min_n = min_n,
    max_n = max_n,
    round = round,
    control = control_quos,
    certainty_size = certainty_size,
    certainty_prop = certainty_prop,
    certainty_overflow = certainty_overflow,
    certainty_plan = certainty_plan,
    on_empty = on_empty,
    method_type = custom_spec$type,
    method_fixed = custom_spec$fixed_size,
    method_variance = custom_spec$variance_family,
    method_probabilities = custom_spec$probabilities,
    method_implementation = method_implementation_hash(custom_spec)
  )

  .data$stages[[current]]$draw_spec <- draw_spec
  .data$validated <- FALSE
  .data
}

#' @noRd
quo_is_null <- function(quo) {
  is_null(rlang::quo_get_expr(quo))
}

#' Parse bare variables from a data-masked draw argument
#' @noRd
parse_draw_variables <- function(quo, arg) {
  if (quo_is_null(quo)) {
    return(NULL)
  }
  expr <- quo_get_expr(quo)
  terms <- if (is_call(expr, "c", ns = "")) as.list(expr)[-1] else list(expr)
  if (length(terms) == 0) {
    cli_abort(
      "{.arg {arg}} must contain bare column names, for example {.code {arg} = c(x, y)}.",
      class = "samplyr_error_draw_argument"
    )
  }
  unique(vapply(terms, function(term) {
    draw_column_name(term, quo_get_env(quo), arg)
  }, character(1)))
}

#' The column a data-masked draw argument names
#'
#' A bare name, or `.data[[v]]` and `.data$x` for a name held in a variable.
#' A string is refused here, where the mistake is made, rather than failing
#' at execution as a missing column called `"households"`.
#' @noRd
draw_column_name <- function(term, env, arg) {
  rlang::local_error_call(caller_env())
  if (is.symbol(term)) {
    return(as.character(term))
  }
  if (is.character(term) && length(term) == 1L) {
    abort_samplyr(
      c(
        "{.arg {arg}} takes a column, not a string.",
        "i" = "Write {.code {arg} = {term}}, or {.code {arg} = .data[[v]]}
               when the column name is held in a variable {.var v}."
      ),
      class = "samplyr_error_draw_string_column"
    )
  }
  if (is_call(term, "[[") && identical(term[[2]], quote(.data))) {
    name <- rlang::eval_bare(term[[3]], env)
    if (is.character(name) && length(name) == 1L && !is.na(name)) {
      return(name)
    }
  }
  if (is_call(term, "$") && identical(term[[2]], quote(.data))) {
    return(as.character(term[[3]]))
  }
  cli_abort(
    c(
      "{.arg {arg}} must name columns: bare names, or {.code .data[[v]]}
       for a name held in a variable.",
      "x" = "Got {.code {as_label(term)}}."
    ),
    class = "samplyr_error_draw_argument"
  )
}

#' Parse ordinary cube auxiliaries and bound() markers
#' @noRd
parse_balanced_aux <- function(quo) {
  rlang::local_error_call(caller_env())
  if (quo_is_null(quo)) {
    return(list(aux = NULL, bounds = NULL))
  }
  expr <- quo_get_expr(quo)
  terms <- if (is_call(expr, "c", ns = "")) as.list(expr)[-1] else list(expr)
  if (length(terms) == 0) {
    cli_abort(
      "{.arg aux} must contain at least one column or {.fn bound} marker.",
      class = "samplyr_error_draw_argument"
    )
  }

  aux <- character(0)
  bounds <- character(0)
  env <- quo_get_env(quo)
  for (term in terms) {
    if (is_call(term, "bound", ns = marker_namespaces)) {
      args <- as.list(term)[-1]
      if (length(args) != 1L) {
        cli_abort(
          "{.fn bound} must contain exactly one column. Use separate calls for separate margins.",
          class = "samplyr_error_draw_argument"
        )
      }
      bounds <- c(bounds, draw_column_name(args[[1]], env, "aux"))
      next
    }
    if (is.symbol(term) || is.character(term) ||
        is_call(term, c("[[", "$"))) {
      aux <- c(aux, draw_column_name(term, env, "aux"))
      next
    }
    cli_abort(
      c(
        "Unsupported expression in {.arg aux}: {.code {as_label(term)}}.",
        "i" = "Use bare numeric columns and single-column {.fn bound} markers."
      ),
      class = "samplyr_error_draw_argument"
    )
  }
  aux <- unique(aux)
  bounds <- unique(bounds)
  list(
    aux = if (length(aux) > 0) aux else NULL,
    bounds = if (length(bounds) > 0) bounds else NULL
  )
}

#' The method names a misspelled one was most likely meant to be
#'
#' The PPS methods share a prefix, so `"brewer"` means `"pps_brewer"`. Otherwise
#' every name within two edits of the smallest distance found: `"srswo"` is
#' one edit from both `"srswor"` and `"srswr"`, and picking one of them would
#' be a guess.
#' @noRd
suggest_method_names <- function(name, candidates, max_dist = 2L) {
  prefixed <- paste0("pps_", name)
  if (prefixed %in% candidates) {
    return(prefixed)
  }
  distances <- as.integer(utils::adist(name, candidates, ignore.case = TRUE))
  best <- min(distances)
  if (best > max_dist) {
    return(character(0))
  }
  candidates[distances == best]
}

#' @noRd
resolve_draw_method <- function(method, call = rlang::caller_env()) {
  if (!is_character(method) || length(method) != 1) {
    cli_abort(
      "{.arg method} must be a single character string",
      call = call,
      class = "samplyr_error_draw_argument"
    )
  }

  if (method %in% valid_builtin_methods) {
    if (method %in% names(builtin_method_aliases)) {
      method <- unname(builtin_method_aliases[[method]])
    }
    return(list(method = method, custom_spec = NULL))
  }

  if (!is_custom_method(method)) {
    meant <- suggest_method_names(method, valid_builtin_methods)
    abort_samplyr(
      c(
        "Unknown sampling method: {.val {method}}.",
        if (length(meant) > 0L) {
          c("i" = cli::format_inline("Did you mean {.val {meant}}?"))
        },
        "i" = "Built-in methods: {.val {valid_builtin_methods}}",
        "i" = "Custom methods can be registered via {.fn sondage::register_method}."
      ),
      class = "samplyr_error_unknown_method",
      call = call
    )
  }

  custom_spec <- custom_method_spec(method)
  prefix <- custom_method_prefix(method)
  expected_prefix <- if (identical(custom_spec$type, "balanced")) {
    "balanced"
  } else {
    "pps"
  }
  if (!identical(prefix, expected_prefix)) {
    cli_abort(
      c(
        "Method {.val {method}} uses the wrong family prefix.",
        "i" = "Registered methods with {.code type = \"{custom_spec$type}\"} use the {.val {paste0(expected_prefix, '_')}} prefix.",
        "i" = "Use {.code method = \"{paste0(expected_prefix, '_', sondage_method_name(method))}\"}."
      ),
      call = call,
      class = "samplyr_error_unknown_method"
    )
  }
  if (identical(custom_spec$probabilities, "unknown")) {
    abort_unknown_probabilities(method, call = call)
  }

  list(method = method, custom_spec = custom_spec)
}

#' @noRd
validate_draw_df <- function(
  df,
  strata_vars = NULL,
  value_col,
  method = NULL,
  custom_spec = NULL,
  check_keys = TRUE,
  call = rlang::caller_env()
) {
  if (!is.data.frame(df)) {
    cli_abort(
      "{.arg {value_col}} must be a data frame when providing stratum-specific values",
      call = call,
      class = "samplyr_error_draw_argument"
    )
  }

  if (check_keys) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Internal error: {.arg strata_vars} must be provided",
        call = call,
        class = "samplyr_error_internal"
      )
    }

    missing_vars <- setdiff(strata_vars, names(df))
    if (length(missing_vars) > 0) {
      message <- c(
        "Data frame for {.arg {value_col}} is missing stratification variable{?s}:",
        "x" = "{.val {missing_vars}}"
      )
      if (value_col %in% c("n", "frac")) {
        abort_samplyr(
          message,
          class = "samplyr_error_alloc_missing_columns",
          call = call
        )
      }
      cli_abort(message, call = call, class = "samplyr_error_draw_argument")
    }
  }

  if (!value_col %in% names(df)) {
    message <- "Data frame for {.arg {value_col}} must contain a {.val {value_col}} column"
    if (value_col %in% c("n", "frac")) {
      abort_samplyr(
        message,
        class = "samplyr_error_alloc_missing_value_column",
        call = call
      )
    }
    cli_abort(message, call = call, class = "samplyr_error_draw_argument")
  }

  if (check_keys) {
    key_df <- df[, strata_vars, drop = FALSE]
    if (anyNA(key_df)) {
      missing_key_cols <- strata_vars[vapply(key_df, anyNA, logical(1))]
      message <- c(
        "Data frame for {.arg {value_col}} has missing values in stratification keys.",
        "x" = "Columns with missing values: {.val {missing_key_cols}}"
      )
      if (value_col %in% c("n", "frac")) {
        abort_samplyr(
          message,
          class = "samplyr_error_alloc_missing_key_values",
          call = call
        )
      }
      cli_abort(message, call = call, class = "samplyr_error_draw_argument")
    }
    if (anyDuplicated(key_df) > 0) {
      message <- "Data frame for {.arg {value_col}} has duplicate rows for the same stratum"
      if (value_col %in% c("n", "frac")) {
        abort_samplyr(
          message,
          class = "samplyr_error_alloc_duplicate_keys",
          call = call
        )
      }
      cli_abort(message, call = call, class = "samplyr_error_draw_argument")
    }
  }

  values <- df[[value_col]]
  if (value_col %in% c("n", "frac")) {
    if (!is_finite_numeric(values)) {
      abort_samplyr(
        "{.arg {value_col}} values must be finite numbers (no NA/NaN/Inf)",
        class = if (value_col == "n") {
          "samplyr_error_alloc_n_non_finite"
        } else {
          "samplyr_error_alloc_frac_non_finite"
        },
        call = call
      )
    }
  }

  if (value_col == "n") {
    if (any(values <= 0)) {
      abort_samplyr(
        "{.arg n} values must be positive",
        class = "samplyr_error_alloc_n_bounds",
        call = call
      )
    }
    if (!is_integerish_numeric(values)) {
      abort_samplyr(
        "{.arg n} values must be integer-valued",
        class = "samplyr_error_alloc_n_integer",
        call = call
      )
    }
  }

  if (value_col == "frac") {
    if (any(values <= 0)) {
      abort_samplyr(
        "{.arg frac} values must be positive",
        class = "samplyr_error_alloc_frac_bounds",
        call = call
      )
    }
    # Classify custom methods by their declared type.
    is_wor <- if (!is_null(custom_spec)) {
      custom_spec$type %in% c("wor", "balanced")
    } else {
      !is_null(method) && !(method %in% c(wr_methods, pmr_methods))
    }
    if (is_wor && any(values > 1)) {
      abort_samplyr(
        "{.arg frac} cannot exceed 1 for without-replacement methods",
        class = "samplyr_error_alloc_frac_wor_bounds",
        call = call
      )
    }
  }

  invisible(NULL)
}

#' @noRd
validate_draw_configuration <- function(
  n,
  frac,
  method,
  mos,
  prn,
  min_n,
  max_n,
  certainty_size,
  certainty_prop,
  round,
  certainty_overflow,
  on_empty,
  has_alloc,
  strata_vars = NULL,
  aux = NULL,
  bounds = NULL,
  spread = NULL,
  custom_spec = NULL,
  warn_ignored = TRUE,
  certainty_plan = NULL,
  parent_vars = character(0),
  call = rlang::caller_env()
) {
  # A take stage stores no size: the plan supplies each pool's take.
  provides_take <- identical(certainty_plan$role, "take")
  n_is_df <- is.data.frame(n)
  frac_is_df <- is.data.frame(frac)

  if (n_is_df || frac_is_df) {
    if (is_null(strata_vars)) {
      # A table keyed on the parent's units is a take per parent.
      table <- if (n_is_df) n else frac
      arg <- if (n_is_df) "n" else "frac"
      keyed <- intersect(names(table), parent_vars)
      if (length(keyed) > 0L) {
        abort_samplyr(
          c(
            "{.arg {arg}} is keyed on {.field {keyed}}, the units the
             previous stage selected, but this stage is not stratified by
             {cli::qty(length(keyed))}{?it/them}.",
            "i" = "A take per parent unit is a stratified stage: add
                   {.code stratify_by({paste(keyed, collapse = ', ')})}
                   before this {.fn draw}, and {.arg {arg}} then gives each
                   unit its own take."
          ),
          class = "samplyr_error_draw_parent_keyed_take",
          call = call
        )
      }
      abort_samplyr(
        "Data frame for {.arg n} or {.arg frac} requires stratification. Use {.fn stratify_by} first.",
        class = "samplyr_error_alloc_invalid_input_type",
        call = call
      )
    }
    if (n_is_df) {
      validate_draw_df(
        n,
        strata_vars = strata_vars,
        value_col = "n",
        method = method,
        custom_spec = custom_spec,
        check_keys = TRUE,
        call = call
      )
    }
    if (frac_is_df) {
      validate_draw_df(
        frac,
        strata_vars = strata_vars,
        value_col = "frac",
        method = method,
        custom_spec = custom_spec,
        check_keys = TRUE,
        call = call
      )
    }
  }

  validate_draw_args(
    n,
    frac,
    method,
    mos,
    has_alloc,
    n_is_df,
    frac_is_df,
    strata_vars = strata_vars,
    aux = aux,
    bounds = bounds,
    spread = spread,
    custom_spec = custom_spec,
    warn_ignored = warn_ignored,
    provides_take = provides_take,
    call = call
  )
  validate_bounds(
    min_n,
    max_n,
    has_alloc,
    warn_ignored = warn_ignored,
    call = call
  )
  validate_certainty(
    certainty_size,
    certainty_prop,
    mos,
    method,
    strata_vars,
    is.data.frame(certainty_size),
    is.data.frame(certainty_prop),
    custom_spec = custom_spec,
    call = call
  )
  validate_prn(prn, method, custom_spec = custom_spec, call = call)

  choices <- list(
    round = c("up", "down", "nearest"),
    certainty_overflow = c("error", "allow"),
    on_empty = c("warn", "error", "silent")
  )
  values <- list(
    round = round,
    certainty_overflow = certainty_overflow,
    on_empty = on_empty
  )
  for (name in names(choices)) {
    value <- values[[name]]
    if (!is.character(value) || length(value) != 1 || !value %in% choices[[name]]) {
      cli_abort(
        "{.arg {name}} must be one of {.val {choices[[name]]}}",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
  }

  invisible(NULL)
}

#' @noRd
validate_draw_args <- function(
  n,
  frac,
  method,
  mos,
  has_alloc,
  n_is_df,
  frac_is_df,
  strata_vars = NULL,
  aux = NULL,
  bounds = NULL,
  spread = NULL,
  custom_spec = NULL,
  warn_ignored = TRUE,
  provides_take = FALSE,
  call = rlang::caller_env()
) {
  if (has_alloc && is_null(n) && !is_null(frac)) {
    abort_samplyr(
      c(
        "{.arg frac} cannot be combined with {.arg alloc} in {.fn stratify_by}.",
        "i" = "Use {.arg n} with allocation methods, or remove {.arg alloc} and keep {.arg frac}."
      ),
      class = "samplyr_error_alloc_frac_with_alloc",
      call = call
    )
  }

  if (
    has_alloc && !is_null(n) && !n_is_df && length(n) > 1 && !is_null(names(n))
  ) {
    abort_samplyr(
      c(
        "Per-stratum {.arg n} cannot be combined with {.arg alloc} in {.fn stratify_by}.",
        "i" = "Remove {.arg alloc} when providing per-stratum allocations, or pass a scalar {.arg n} for samplyr to allocate."
      ),
      class = "samplyr_error_alloc_named_n_with_alloc",
      call = call
    )
  }

  is_balanced <- method %in%
    balanced_methods ||
    (!is_null(custom_spec) && custom_spec$type == "balanced")
  is_pps <- method %in%
    pps_methods ||
    (!is_null(custom_spec) && custom_spec$type != "balanced")

  if (is_pps && is_null(mos)) {
    cli_abort(
      "PPS methods require {.arg mos} (measure of size)",
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }

  if (warn_ignored && !is_pps && !is_balanced && !is_null(mos)) {
    cli_warn(
      "{.arg mos} is ignored for non-PPS methods",
      call = call,
      class = "samplyr_warning_draw_argument_ignored"
    )
  }

  if (!is_null(aux) && !is_balanced) {
    cli_abort(
      c(
        "{.arg aux} is only supported for balanced sampling
         ({.val cube} or a custom method registered with
         {.code type = \"balanced\"}).",
        "x" = "Current method: {.val {method}}"
      ),
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }

  if (!is_null(aux)) {
    if (!is.character(aux) || length(aux) < 1) {
      cli_abort(
        "{.arg aux} must specify at least one column name",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
  }

  if (
    !is_null(aux) &&
      !is_null(custom_spec) &&
      identical(custom_spec$type, "balanced") &&
      !isTRUE(custom_spec$supports_aux)
  ) {
    cli_abort(
      "Method {.val {method}} does not support ordinary auxiliary balancing variables.",
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }

  if (!is_null(bounds) && !identical(method, "cube")) {
    cli_abort(
      c(
        "{.fn bound} constraints are only supported by {.code method = \"cube\"}.",
        "x" = "Current method: {.val {method}}"
      ),
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }

  supports_spread <- method %in%
    spatial_balanced_methods ||
    (!is_null(custom_spec) &&
      identical(custom_spec$type, "balanced") &&
      isTRUE(custom_spec$supports_spread))
  if (!is_null(spread) && !supports_spread) {
    cli_abort(
      c(
        "{.arg spread} requires a spatially balanced method.",
        "i" = "Use {.code method = \"lpm2\"} or {.code method = \"scps\"}.",
        "i" = "Registered balanced methods may opt in with {.code supports_spread = TRUE}.",
        "x" = "Current method: {.val {method}}"
      ),
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }
  if (supports_spread && is_null(spread)) {
    cli_abort(
      "Method {.val {method}} requires {.arg spread} coordinates.",
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }
  if (supports_spread && (!is_null(aux) || !is_null(bounds))) {
    cli_abort(
      "Method {.val {method}} cannot combine {.arg spread} with cube auxiliary or count-bound constraints.",
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }

  is_random_size <- method %in%
    rs_poisson_methods ||
    (!is_null(custom_spec) && !custom_spec$fixed_size)
  if (is_random_size) {
    if (!is_null(n) && !is_null(frac)) {
      abort_samplyr(
        "Specify either {.arg n} (expected sample size) or {.arg frac}, not both",
        class = "samplyr_error_alloc_size_conflict",
        call = call
      )
    }
    if (is_null(n) && is_null(frac)) {
      abort_samplyr(
        "{.val {method}} sampling requires {.arg n} or {.arg frac}",
        class = "samplyr_error_alloc_size_absent",
        call = call
      )
    }
  } else if (method == "pps_cps") {
    if (!is_null(frac)) {
      abort_samplyr(
        "{.val pps_cps} sampling requires {.arg n}, not {.arg frac}",
        class = "samplyr_error_alloc_size_conflict",
        call = call
      )
    }
    if (is_null(n)) {
      abort_samplyr(
        "{.val pps_cps} sampling requires {.arg n}",
        class = "samplyr_error_alloc_size_absent",
        call = call
      )
    }
  } else {
    if (is_null(n) && is_null(frac) && !provides_take) {
      abort_samplyr(
        "Specify either {.arg n} or {.arg frac}",
        class = "samplyr_error_alloc_size_absent",
        call = call
      )
    }
    if (!is_null(n) && !is_null(frac)) {
      abort_samplyr(
        "Specify either {.arg n} or {.arg frac}, not both",
        class = "samplyr_error_alloc_size_conflict",
        call = call
      )
    }
  }

  if (!is_null(n) && !n_is_df) {
    if (!is.numeric(n)) {
      abort_samplyr(
        "{.arg n} must be numeric or a data frame",
        class = "samplyr_error_alloc_invalid_input_type",
        call = call
      )
    }
    if (!is_finite_numeric(n)) {
      abort_samplyr(
        "{.arg n} must not contain NA, NaN, or Inf",
        class = "samplyr_error_alloc_n_non_finite",
        call = call
      )
    }
    if (length(n) > 1 && !is_null(names(n)) && is_null(strata_vars)) {
      abort_samplyr(
        c(
          "Named {.arg n} requires stratification at this stage.",
          "i" = "Add {.fn stratify_by} before this {.fn draw} (per-stage; stage-1 strata do not carry over), or pass a scalar."
        ),
        class = "samplyr_error_alloc_invalid_input_type",
        call = call
      )
    }
    if (length(n) > 1 && !is_null(names(n)) && length(strata_vars) > 1) {
      abort_samplyr(
        c(
          "Named {.arg n} vectors are only supported for single stratification variables.",
          "i" = "Use a data frame with columns {.val {strata_vars}} and {.val n}."
        ),
        class = "samplyr_error_alloc_invalid_input_type",
        call = call
      )
    }
    if (length(n) > 1 && (is_null(names(n)) || is_null(strata_vars))) {
      abort_samplyr(
        "{.arg n} must be a scalar, a named vector, or a data frame",
        class = "samplyr_error_alloc_invalid_input_type",
        call = call
      )
    }
    if (any(n <= 0)) {
      abort_samplyr(
        "{.arg n} must be positive",
        class = "samplyr_error_alloc_n_bounds",
        call = call
      )
    }
    if (!is_integerish_numeric(n)) {
      abort_samplyr(
        "{.arg n} must be integer-valued",
        class = "samplyr_error_alloc_n_integer",
        call = call
      )
    }
  }

  if (!is_null(frac) && !frac_is_df) {
    if (!is.numeric(frac)) {
      abort_samplyr(
        "{.arg frac} must be numeric or a data frame",
        class = "samplyr_error_alloc_invalid_input_type",
        call = call
      )
    }
    if (!is_finite_numeric(frac)) {
      abort_samplyr(
        "{.arg frac} must not contain NA, NaN, or Inf",
        class = "samplyr_error_alloc_frac_non_finite",
        call = call
      )
    }
    if (length(frac) > 1 && (is_null(names(frac)) || is_null(strata_vars))) {
      abort_samplyr(
        "{.arg frac} must be a scalar, a named vector, or a data frame",
        class = "samplyr_error_alloc_invalid_input_type",
        call = call
      )
    }
    if (
      length(frac) > 1 &&
        !is_null(names(frac)) &&
        length(strata_vars) > 1
    ) {
      abort_samplyr(
        c(
          "Named {.arg frac} vectors are only supported for single stratification variables.",
          "i" = "Use a data frame with columns {.val {strata_vars}} and {.val frac}."
        ),
        class = "samplyr_error_alloc_invalid_input_type",
        call = call
      )
    }
    if (any(frac <= 0)) {
      abort_samplyr(
        "{.arg frac} must be positive",
        class = "samplyr_error_alloc_frac_bounds",
        call = call
      )
    }
    # Prefer custom type over built-in name classification.
    is_wor <- if (!is_null(custom_spec)) {
      custom_spec$type %in% c("wor", "balanced")
    } else {
      !(method %in% c(wr_methods, pmr_methods))
    }
    if (is_wor && any(frac > 1)) {
      abort_samplyr(
        "{.arg frac} cannot exceed 1 for without-replacement methods",
        class = "samplyr_error_alloc_frac_wor_bounds",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' @noRd
validate_bounds <- function(
  min_n,
  max_n,
  has_alloc,
  warn_ignored = TRUE,
  call = rlang::caller_env()
) {
  if (!is_null(min_n)) {
    if (!is.numeric(min_n) || length(min_n) != 1 || !is_finite_numeric(min_n)) {
      cli_abort(
        "{.arg min_n} must be a single positive integer",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
    if (min_n < 1 || !is_integerish_numeric(min_n)) {
      cli_abort(
        "{.arg min_n} must be a positive integer",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
    if (warn_ignored && !has_alloc) {
      cli_warn(
        "{.arg min_n} only applies when an allocation method is specified in {.fn stratify_by}",
        class = "samplyr_warning_draw_argument_ignored"
      )
    }
  }

  if (!is_null(max_n)) {
    if (!is.numeric(max_n) || length(max_n) != 1 || !is_finite_numeric(max_n)) {
      cli_abort(
        "{.arg max_n} must be a single positive integer",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
    if (max_n < 1 || !is_integerish_numeric(max_n)) {
      cli_abort(
        "{.arg max_n} must be a positive integer",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
    if (warn_ignored && !has_alloc) {
      cli_warn(
        "{.arg max_n} only applies when an allocation method is specified in {.fn stratify_by}",
        class = "samplyr_warning_draw_argument_ignored"
      )
    }
  }

  if (!is_null(min_n) && !is_null(max_n) && min_n > max_n) {
    cli_abort(
      "{.arg min_n} ({min_n}) cannot be greater than {.arg max_n} ({max_n})",
      call = call,
      class = "samplyr_error_draw_argument"
    )
  }
  invisible(NULL)
}

#' @noRd
validate_certainty <- function(
  certainty_size,
  certainty_prop,
  mos,
  method,
  strata_vars,
  certainty_size_is_df,
  certainty_prop_is_df,
  custom_spec = NULL,
  call = rlang::caller_env()
) {
  if (!is_null(certainty_size) && !is_null(certainty_prop)) {
    cli_abort(
      "Specify only one of {.arg certainty_size} or {.arg certainty_prop}, not both.",
      call = call,
      class = "samplyr_error_draw_argument"
    )
  }

  has_certainty <- !is_null(certainty_size) || !is_null(certainty_prop)
  if (!has_certainty) {
    return(invisible(NULL))
  }

  if (is_null(mos)) {
    cli_abort(
      "Certainty selection requires {.arg mos} to be specified.",
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }

  is_pps_wor <- method %in%
    pps_wor_methods ||
    (!is_null(custom_spec) && custom_spec$type == "wor")
  if (!is_pps_wor) {
    cli_abort(
      c(
        "Certainty selection is only available for PPS without-replacement methods.",
        "i" = "Valid methods: {.val {pps_wor_methods}}",
        "i" = "Custom WOR methods registered via {.fn sondage::register_method} are also supported.",
        "i" = "WR ({.val pps_multinomial}) and PMR ({.val pps_chromy}) methods handle large units natively.",
        "x" = "Current method: {.val {method}}"
      ),
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }

  if (certainty_size_is_df) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Data frame for {.arg certainty_size} requires stratification. Use {.fn stratify_by} first.",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
    validate_draw_df(certainty_size, strata_vars, "certainty_size", call = call)
    vals <- certainty_size$certainty_size
    if (!is_finite_numeric(vals) || any(vals <= 0)) {
      cli_abort(
        "{.arg certainty_size} values must be positive numbers.",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
  } else if (!is_null(certainty_size)) {
    if (
      !is.numeric(certainty_size) ||
        length(certainty_size) != 1 ||
        !is_finite_numeric(certainty_size) ||
        certainty_size <= 0
    ) {
      cli_abort(
        "{.arg certainty_size} must be a single positive number or a data frame.",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
  }

  if (certainty_prop_is_df) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Data frame for {.arg certainty_prop} requires stratification. Use {.fn stratify_by} first.",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
    validate_draw_df(certainty_prop, strata_vars, "certainty_prop", call = call)
    vals <- certainty_prop$certainty_prop
    if (!is_finite_numeric(vals) || any(vals <= 0) || any(vals >= 1)) {
      cli_abort(
        "{.arg certainty_prop} values must be between 0 and 1 (exclusive).",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
  } else if (!is_null(certainty_prop)) {
    if (
      !is.numeric(certainty_prop) ||
        length(certainty_prop) != 1 ||
        !is_finite_numeric(certainty_prop) ||
        certainty_prop <= 0 ||
        certainty_prop >= 1
    ) {
      cli_abort(
        "{.arg certainty_prop} must be a single number between 0 and 1 (exclusive) or a data frame.",
        call = call,
        class = "samplyr_error_draw_argument"
      )
    }
  }
  invisible(NULL)
}

#' @noRd
validate_prn <- function(
  prn,
  method,
  custom_spec = NULL,
  call = rlang::caller_env()
) {
  if (is_null(prn)) {
    return(invisible(NULL))
  }
  supports_prn <- method %in%
    prn_methods ||
    (!is_null(custom_spec) && custom_spec$supports_prn)
  if (!supports_prn) {
    cli_abort(
      c(
        "{.arg prn} is only supported for methods that use permanent random numbers.",
        "i" = "Valid methods: {.val {prn_methods}}",
        "i" = "Custom methods can declare PRN support via {.fn sondage::register_method}.",
        "x" = "Current method: {.val {method}}"
      ),
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }
  invisible(NULL)
}
