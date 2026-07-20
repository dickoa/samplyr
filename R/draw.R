#' Specify Selection Parameters
#'
#' `draw()` specifies how units are selected: sample size, sampling fraction,
#' selection method, and measure of size for PPS sampling. Every stage in a
#' sampling design must end with `draw()`.
#'
#' @param .data A `sampling_design` object (piped from [sampling_design()],
#'   [stratify_by()], or [cluster_by()]).
#' @param n Sample size. For random-size methods (`bernoulli`, `pps_poisson`),
#'   `n` is the **expected** sample size (converted internally to `frac = n / N`).
#'   Can be:
#'   - A scalar: applies per stratum (if no `alloc`) or as total (if `alloc` specified)
#'   - A named vector: stratum-specific sizes (for single stratification variable)
#'   - A data frame: stratum-specific sizes with stratification columns + `n` column
#' @param frac Sampling fraction. Can be:
#'   - A scalar: same fraction for all strata
#'   - A named vector: stratum-specific fractions
#'   - A data frame: stratum-specific fractions with stratification columns + `frac` column
#'   Only one of `n` or `frac` should be specified. When the rounded
#'   stratum sample size (\eqn{N_h \cdot \text{frac}}{N_h * frac}) would
#'   be zero, it is floored at 1 so every stratum receives at least one
#'   unit.
#' @param min_n Minimum sample size per stratum. When an allocation method
#'   (e.g., Neyman, proportional) would assign fewer than `min_n` units to a
#'   stratum, that stratum receives `min_n` units instead. The excess is
#'   redistributed proportionally among strata that were above `min_n`.
#'   Commonly set to 2 (minimum for variance estimation) or higher for
#'   reliable subgroup estimates. Only applies when stratification with an
#'   allocation method is used. Default is `NULL` (no minimum).
#' @param max_n Maximum sample size per stratum. When an allocation method
#'   would assign more than `max_n` units to a stratum, that stratum is
#'   capped at `max_n` units. The surplus is redistributed proportionally
#'   among strata that were below `max_n`. Useful for capping dominant strata
#'   or managing operational constraints. Only applies when stratification
#'   with an allocation method is used. Default is `NULL` (no maximum).
#' @param method Character string specifying the selection method. One of:
#'
#'   **Equal probability methods:**
#'   - `"srswor"` (default): Simple random sampling without replacement
#'   - `"srswr"`: Simple random sampling with replacement
#'   - `"systematic"`: Systematic (fixed interval) sampling
#'   - `"bernoulli"`: Independent Bernoulli trials (random sample size)
#'
#'
#'   **PPS methods (require `mos`):**
#'   - `"pps_systematic"`: PPS systematic sampling
#'   - `"pps_brewer"`: Generalized Brewer (\enc{Tillé}{Tille}) method
#'   - `"pps_cps"`: Conditional Poisson sampling (maximum entropy)
#'   - `"pps_sampford"`: Sampford fixed-size PPS sampling
#'   - `"pps_poisson"`: PPS Poisson sampling (random sample size)
#'   - `"pps_sps"`: Sequential Poisson sampling (fixed size, supports `prn`)
#'   - `"pps_pareto"`: Pareto sampling (fixed size, supports `prn`)
#'   - `"pps_multinomial"`: PPS multinomial (with replacement, any hit count)
#'   - `"pps_chromy"`: Chromy's sequential PPS (minimum replacement)
#'
#'   **Balanced sampling:**
#'   - `"cube"`: Balanced sampling via the cube method
#'     (Deville & \enc{Tillé}{Tille} 2004).
#'     Uses auxiliary variables (`aux`) to balance the sample so that
#'     Horvitz-Thompson estimates of auxiliary totals match population totals.
#'     Supports equal or unequal (`mos`) inclusion probabilities. When
#'     stratified, uses the stratified cube algorithm (Chauvet 2009). At most
#'     2 stages may use a balanced-family method. `"balanced"` is retained
#'     as a compatibility alias for `"cube"`.
#'   - `"lpm2"`: Local pivotal sampling. Requires spatial coordinates in
#'     `spread` and does not accept `aux` or `bound()` constraints.
#'   - `"scps"`: Spatially correlated Poisson sampling. Requires spatial
#'     coordinates in `spread` and does not accept `aux` or `bound()`.
#'
#'   Methods registered with [sondage::register_method()] use a prefix that
#'   identifies their sampling family. Registered `type = "wor"` and
#'   `type = "wr"` methods use `"pps_<name>"` (for example,
#'   `"pps_mymethod"`). Registered `type = "balanced"` methods use
#'   `"balanced_<name>"`. For the latter, `mos` is optional;
#'   `supports_aux = TRUE` permits ordinary balancing variables in `aux`, and
#'   `supports_spread = TRUE` requires coordinates in `spread`. These
#'   capabilities are declared when the method is registered in `sondage`.
#'
#'   Sample weights are `1 / pik`, where `pik` is the chance vector
#'   (target inclusion probabilities, or expected hits for `type = "wr"`
#'   methods) that samplyr resolves and hands to the registered method.
#'   Registered methods state where they sit in the taxonomy with
#'   `probabilities` at registration: `"exact"` (the design's true
#'   first-order inclusion probabilities, or expected hits, equal
#'   `pik`), `"approximate"`
#'   (honored to a documented approximation, as Pareto sampling does), or
#'   `"unknown"` (the default: `pik` is a selection weight only, so the
#'   weights would be systematically biased). `draw()` refuses `"unknown"`
#'   methods; declare the method's tier to use it. The classic trap is
#'   `sample(prob = pik)`: with `replace = TRUE` it yields expected hits
#'   exactly equal to `pik` (a valid `type = "wr"` method), but without
#'   replacement its inclusion probabilities differ from `pik`, so a
#'   `type = "wor"` wrapper's tier really is unknown.
#'
#' @param mos Measure of size variable, specified as a bare column name
#'   (unquoted). Required for built-in PPS methods and registered `type =
#'   "wor"` or `type = "wr"` methods named with the `pps_` prefix. Optional
#'   for `cube`, `lpm2`, `scps`, and registered `type = "balanced"` methods
#'   named with the `balanced_` prefix; when omitted, equal inclusion
#'   probabilities are used.
#' @param prn Permanent random number variable for sample coordination,
#'   specified as a bare column name (unquoted). Must be a numeric column
#'   with values in the open interval (0, 1) and no missing values.
#'   Supported methods: `"bernoulli"`, `"pps_poisson"`, `"pps_sps"`,
#'   `"pps_pareto"`. When supplied, the sample is deterministic for a given
#'   set of PRN values, enabling coordination across survey waves.
#' @param aux Cube balancing declarations for `method = "cube"`, or ordinary
#'   balancing variables for a registered balanced method that declares
#'   `supports_aux = TRUE`.
#'   Bare numeric columns, such as `aux = c(income, pop_density)`, request
#'   approximate Horvitz-Thompson total balance. A [bound()] marker requests
#'   adjacent-integer count bounds for every observed category, for example
#'   `aux = c(income, bound(region), bound(urban_rural))`. Use separate
#'   `bound()` calls for separate marginal constraints. With `cluster_by()`,
#'   ordinary auxiliary values are summed to cluster level, while bound
#'   variables must be constant within each cluster.
#' @param spread Spatial coordinates for `method = "lpm2"`, `"scps"`, or a
#'   registered balanced method that declares `supports_spread = TRUE`,
#'   specified as bare numeric columns, for example `spread = c(longitude,
#'   latitude)`. Coordinates must be finite and have no missing values. They
#'   should be placed on comparable scales before sampling. Methods declaring
#'   `supports_spread = TRUE` require this argument. With `cluster_by()`,
#'   coordinates must be constant within each cluster.
#' @param round Rounding method when converting `frac` to sample sizes.
#'   One of:
#'   - `"up"` (default): Round up (ceiling). Matches SAS SURVEYSELECT default.
#'   - `"down"`: Round down (floor).
#'   - `"nearest"`: Round to nearest integer (standard rounding).
#'
#'   This parameter only affects designs using `frac` to specify the sampling
#'   rate. When `n` is specified directly, no rounding occurs. After rounding,
#'   a minimum of 1 is enforced per stratum or group.
#'
#' @param control <[`data-masking`][dplyr::dplyr_data_masking]> Variables for
#'   sorting the frame before selection. Control sorting provides implicit
#'   stratification, which is particularly effective with systematic and
#'   sequential sampling methods. Can be:
#'   - A single variable: `control = region`
#'   - Multiple variables: `control = c(region, district)`
#'   - With [serp()] for serpentine sorting: `control = serp(region, district)`
#'   - With [dplyr::desc()] for descending: `control = c(region, desc(population))`
#'   - Mixed: `control = c(region, serp(district, commune), desc(size))`
#'
#'   When stratification is also specified, control sorting is applied within
#'   each stratum. See the section "Control Sorting" below for details.
#'
#' @param certainty_size For PPS without-replacement methods, units with MOS >= this value
#'   are selected with certainty (probability = 1). Can be:
#'   - A scalar: same threshold for all strata
#'   - A data frame: stratum-specific thresholds with stratification columns
#'     + `certainty_size` column
#'
#'   Certainty units are removed from the frame before probability sampling,
#'   and the remaining sample size is reduced accordingly.
#'   Mutually exclusive with `certainty_prop`.
#'   Equivalent to SAS SURVEYSELECT `CERTSIZE=` option.
#' @param certainty_prop For PPS without-replacement methods, units whose MOS proportion
#'   (MOS_i / sum(MOS)) >= this value are selected with certainty. Can be:
#'   - A scalar between 0 and 1 (exclusive): same threshold for all strata
#'   - A data frame: stratum-specific thresholds with stratification columns
#'     + `certainty_prop` column
#'
#'   Uses iterative selection: after removing certainty units, proportions are
#'   recomputed and the check is repeated until no new units qualify.
#'   Mutually exclusive with `certainty_size`.
#'   Equivalent to SAS SURVEYSELECT `CERTSIZE=P=` option.
#'
#' @param certainty_overflow Controls behavior when certainty units exceed the
#'   target sample size `n`. One of:
#'   - `"error"` (default): Stop with an informative error.
#'   - `"allow"`: Return all certainty units with stage weight 1, even if the
#'     resulting sample has more than `n` units.
#'
#'   Equivalent to SAS SURVEYSELECT allowing `CERTSIZE=` overflow.
#'
#' @param on_empty Behavior when a random-size method (`bernoulli`,
#'   `pps_poisson`, or a custom method registered with
#'   `fixed_size = FALSE`) selects zero units in a stratum or the whole
#'   frame. One of:
#'   - `"error"` (default): Stop with an informative error. Zero selections
#'     usually indicate a design problem (sampling fraction too small or
#'     stratum too small) that should be fixed rather than silently papered
#'     over.
#'   - `"warn"`: Issue a warning and keep the empty selection.
#'   - `"silent"`: Keep the empty selection without a message.
#'
#'   An empty selection is a valid realization of a random-size design:
#'   it contributes zero to Horvitz-Thompson totals, so estimates from
#'   repeated executions remain unbiased. (A fallback that draws a
#'   substitute unit would need weights from the combined "draw, then
#'   fall back" design; reusing the SRS or Poisson weights biases HT
#'   totals upward.) When an empty stage occurs in a multi-stage
#'   design, later stages have nothing to select from and the result is
#'   an empty sample. `"warn"` and `"silent"` are intended for
#'   simulation and replicated runs; check `nrow()` before analyzing a
#'   single realization.
#'
#'   A replicated execution that produced empty replicates cannot be
#'   passed to a later [execute()] call (a new phase or a stage
#'   continuation): this raises an error of class
#'   `samplyr_error_empty_phase_replicate` rather than silently
#'   skipping the empty replicates, which would condition all
#'   downstream results on nonempty realizations. Handle empty
#'   replicates explicitly, for example by executing each nonempty
#'   replicate separately while accounting for the empty ones in the
#'   analysis.
#'
#' @return A modified `sampling_design` object with selection parameters specified.
#'
#' @details
#' ## Selection Methods
#'
#' ### Equal Probability Methods
#'
#' | Method | Replacement | Sample Size | Notes |
#' |--------|-------------|-------------|-------|
#' | `srswor` | Without | Fixed | Standard SRS |
#' | `srswr` | With | Fixed | Allows duplicates |
#' | `systematic` | Without | Fixed | Periodic selection |
#' | `bernoulli` | Without | Random | Each unit selected independently |
#'
#' ### PPS Methods
#'
#' | Method | Replacement | Sample Size | Notes |
#' |--------|-------------|-------------|-------|
#' | `pps_systematic` | Without | Fixed | Simple, some bias |
#' | `pps_brewer` | Without | Fixed | Fast, joint prob > 0 |
#' | `pps_cps` | Without | Fixed | Highest entropy, joint prob available |
#' | `pps_sampford` | Without | Fixed | Exact Sampford joint probabilities |
#' | `pps_poisson` | Without | Random | PPS analog of Bernoulli |
#' | `pps_sps` | Without | Fixed | Sequential Poisson, supports `prn` |
#' | `pps_pareto` | Without | Fixed | Pareto sampling, supports `prn` |
#' | `pps_multinomial` | With | Fixed | Any hit count, Hansen-Hurwitz |
#' | `pps_chromy` | Min. repl. | Fixed | SAS default PPS_SEQ |
#'
#' ### Balanced Sampling
#'
#' | Method | Replacement | Sample Size | Notes |
#' |--------|-------------|-------------|-------|
#' | `cube` | Without | Fixed | Deville & \enc{Tillé}{Tille} 2004, uses `aux` |
#' | `lpm2` | Without | Fixed | Spatial spread; requires `spread` |
#' | `scps` | Without | Fixed | Spatial spread; requires `spread` |
#'
#' ## Parameter Requirements
#'
#' | Method | `n` | `frac` | `mos` | Extra input |
#' |--------|-----|--------|-------|-------------|
#' | `srswor` | Yes | or Yes | -- | -- |
#' | `srswr` | Yes | or Yes | -- | -- |
#' | `systematic` | Yes | or Yes | -- | -- |
#' | `bernoulli` | Expected | or Yes | -- | -- |
#' | `pps_systematic` | Yes | or Yes | Yes | -- |
#' | `pps_brewer` | Yes | or Yes | Yes | -- |
#' | `pps_cps` | Yes | -- | Yes | -- |
#' | `pps_sampford` | Yes | or Yes | Yes | -- |
#' | `pps_poisson` | Expected | or Yes | Yes | -- |
#' | `pps_sps` | Yes | or Yes | Yes | -- |
#' | `pps_pareto` | Yes | or Yes | Yes | -- |
#' | `pps_multinomial` | Yes | or Yes | Yes | -- |
#' | `pps_chromy` | Yes | or Yes | Yes | -- |
#' | `cube` | Yes | or Yes | Optional | `aux` optional |
#' | `lpm2` | Yes | or Yes | Optional | `spread` required |
#' | `scps` | Yes | or Yes | Optional | `spread` required |
#'
#' ## Fixed vs Random Sample Size Methods
#'
#' Methods with **fixed sample size** (`srswor`, `srswr`, `systematic`, `pps_systematic`,
#' `pps_brewer`, `pps_cps`, `pps_sampford`, `pps_sps`, `pps_pareto`,
#' `pps_multinomial`, `pps_chromy`, `cube`, `lpm2`, `scps`)
#' accept either `n` or `frac`. When `frac`
#' is provided, the sample size is computed based on the `round` parameter (default: ceiling).
#'
#' Methods with **random sample size** (`bernoulli`, `pps_poisson`) accept either
#' `n` or `frac`. When `n` is provided, it is converted to `frac = n / N` (where
#' `N` is the stratum or frame size). The resulting sample size is still random:
#' `n` specifies the **expected** sample size, not a fixed count.
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
#' When an allocation method is set in [stratify_by()] (`equal`,
#' `proportional`, `neyman`, `optimal`, `power`), specify total sample size via `n`.
#' Combining `alloc` with `frac` is not supported.
#'
#' ## Custom Allocation with Data Frames
#'
#' For stratum-specific sample sizes or rates, pass a data frame to `n` or `frac`.
#' The data frame must contain:
#' - All stratification variable columns (matching those in `stratify_by()`)
#' - An `n` column (for sizes) or `frac` column (for rates)
#'
#' ## Certainty Selection
#'
#' In PPS without-replacement sampling, very large units can have theoretical
#' inclusion probabilities exceeding 1. Certainty selection handles this by
#' selecting such units with probability 1 before sampling the remainder.
#' The output includes a `.certainty_k` column (where `k` is the stage number)
#' indicating which units were certainty selections.
#'
#' Certainty selection is only available for WOR PPS methods (`pps_systematic`,
#' `pps_brewer`, `pps_cps`, `pps_poisson`, `pps_sps`, `pps_pareto`). With-replacement methods
#' (`pps_multinomial`) and PMR methods (`pps_chromy`) handle large units
#' natively through their hit mechanism.
#'
#' When `certainty_overflow = "allow"`, if more units qualify for certainty
#' selection than the requested `n`, all certainty units are returned with
#' probability 1 (stage weight = 1). No probabilistic sampling is performed in
#' this case. The resulting sample size will be the number of certainty
#' units, which exceeds `n`. In multi-stage designs, the final `.weight` can
#' still exceed 1 because it compounds all stage weights.
#'
#' For stratum-specific thresholds, pass a data frame containing:
#' - All stratification variable columns
#' - A `certainty_size` or `certainty_prop` column
#'
#' **Certainty with `pps_poisson` and user-supplied `frac`.** For
#' `pps_poisson`, the probabilistic remainder reuses the user-supplied
#' `frac` against the *remaining* (non-certainty) units. That is,
#' \eqn{\pi_i = \text{frac} \cdot \text{mos}_i \cdot N_r / \sum_{r}\text{mos}_r}{pi_i = frac * mos_i * N_r / sum_r(mos_r)}
#' for the \eqn{N_r} remaining units, so the expected total sample size is
#' \eqn{n_{\mathrm{cert}} + \text{frac} \cdot N_r}{n_cert + frac * N_r}
#' rather than \eqn{\text{frac} \cdot N}{frac * N}. If you need the
#' expected total to track `frac * N`, pass an expected `n` instead and
#' let samplyr derive the remaining fraction as `(n - n_cert) / N_r`.
#'
#' ## Control Sorting
#'
#' Control sorting orders the sampling frame before selection, providing implicit
#' stratification. This is particularly effective with systematic and sequential
#' methods (`systematic`, `pps_systematic`, `pps_chromy`), where it ensures the
#' sample spreads evenly across the sorted variables.
#'
#' **Serpentine vs Nested Sorting:**
#' - **Nested** (default): Standard ascending sort by each variable in order.
#'   Use `control = c(var1, var2, var3)`.
#' - **Serpentine**: Alternating direction that minimizes "jumps" between
#'   adjacent units. Use `control = serp(var1, var2, var3)`.
#'
#' Serpentine sorting makes nearby observations more similar by reversing
#' direction at each hierarchy level. For geographic hierarchies, this means
#' the last district of region 1 is adjacent to the last district of region 2.
#'
#' **Combining with Explicit Stratification:**
#' When both `stratify_by()` and `control` are used, sorting is applied within
#' each stratum. This allows explicit stratification for variance control
#' combined with implicit stratification for sample spread.
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
#' Hájek, J. (1964). Asymptotic theory of rejective sampling with varying
#' probabilities from a finite population.
#' *Annals of Mathematical Statistics*, 35(4), 1491-1523.
#'
#' Chen, X.-H., Dempster, A.P. and Liu, J.S. (1994). Weighted finite
#' population sampling to maximize entropy. *Biometrika*, 81(3), 457-469.
#'
#' `pps_poisson`:
#' Tillé, Y. (2006). *Sampling Algorithms*. Springer.
#'
#' `pps_sps`:
#' Ohlsson, E. (1998). Sequential Poisson sampling.
#' *Journal of Official Statistics*, 14(2), 149-162.
#'
#' `pps_pareto`:
#' Rosén, B. (1997). Asymptotic theory for order sampling.
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
#' [serp()] for serpentine sorting
#'
#' @export
draw <- function(
  .data,
  n = NULL,
  frac = NULL,
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
  if (!is_sampling_design(.data)) {
    cli_abort("{.arg .data} must be a {.cls sampling_design} object")
  }

  mos_quo <- enquo(mos)
  mos_name <- if (quo_is_null(mos_quo)) NULL else as_label(mos_quo)

  prn_quo <- enquo(prn)
  prn_name <- if (quo_is_null(prn_quo)) NULL else as_label(prn_quo)

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

  if (!is_character(method) || length(method) != 1) {
    cli_abort("{.arg method} must be a single character string")
  }

  custom_spec <- NULL
  if (method %in% valid_builtin_methods) {
    method <- match.arg(method, valid_builtin_methods)
    if (method %in% names(builtin_method_aliases)) {
      method <- unname(builtin_method_aliases[[method]])
    }
  } else if (is_custom_method(method)) {
    custom_spec <- custom_method_spec(method)
    prefix <- custom_method_prefix(method)
    expected_prefix <- if (identical(custom_spec$type, "balanced")) {
      "balanced"
    } else {
      "pps"
    }
    if (!identical(prefix, expected_prefix)) {
      cli_abort(c(
        "Method {.val {method}} uses the wrong family prefix.",
        "i" = "Registered methods with {.code type = \"{custom_spec$type}\"} use the {.val {paste0(expected_prefix, '_')}} prefix.",
        "i" = "Use {.code method = \"{paste0(expected_prefix, '_', sondage_method_name(method))}\"}."
      ))
    }
    if (identical(custom_spec$probabilities, "unknown")) {
      abort_unknown_probabilities(method)
    }
  } else {
    cli_abort(
      c(
        "Unknown sampling method: {.val {method}}.",
        "i" = "Built-in methods: {.val {valid_builtin_methods}}",
        "i" = "Custom methods can be registered via {.fn sondage::register_method}."
      )
    )
  }

  valid_round <- c("up", "down", "nearest")
  if (!is_character(round) || length(round) != 1) {
    cli_abort("{.arg round} must be a single character string")
  }
  round <- match.arg(round, valid_round)

  current <- .data$current_stage
  if (current < 1 || current > length(.data$stages)) {
    cli_abort("Invalid design state: no current stage")
  }

  current_stage <- .data$stages[[current]]
  has_alloc <- !is_null(current_stage$strata) &&
    !is_null(current_stage$strata$alloc)

  strata_vars <- current_stage$strata$vars

  n <- coerce_svyplan_n(
    n,
    stage_index = current,
    clustered = !is_null(current_stage$clusters)
  )
  n_is_df <- is.data.frame(n)
  frac_is_df <- is.data.frame(frac)
  certainty_size_is_df <- is.data.frame(certainty_size)
  certainty_prop_is_df <- is.data.frame(certainty_prop)

  if (n_is_df || frac_is_df) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Data frame for {.arg n} or {.arg frac} requires stratification. Use {.fn stratify_by} first."
      )
    }
    if (n_is_df) {
      validate_draw_df(
        n,
        strata_vars = strata_vars,
        value_col = "n",
        method = method,
        custom_spec = custom_spec,
        check_keys = TRUE
      )
    }
    if (frac_is_df) {
      validate_draw_df(
        frac,
        strata_vars = strata_vars,
        value_col = "frac",
        method = method,
        custom_spec = custom_spec,
        check_keys = TRUE
      )
    }
  }

  valid_on_empty <- c("warn", "error", "silent")
  if (
    !is_character(on_empty) ||
      length(on_empty) != 1 ||
      !on_empty %in% valid_on_empty
  ) {
    cli_abort(
      "{.arg on_empty} must be one of {.val {valid_on_empty}}"
    )
  }

  validate_draw_args(
    n,
    frac,
    method,
    mos_name,
    has_alloc,
    n_is_df,
    frac_is_df,
    strata_vars = strata_vars,
    aux = aux_names,
    bounds = bound_names,
    spread = spread_names,
    custom_spec = custom_spec
  )
  validate_bounds(min_n, max_n, has_alloc)
  validate_certainty(
    certainty_size,
    certainty_prop,
    mos_name,
    method,
    strata_vars,
    certainty_size_is_df,
    certainty_prop_is_df,
    custom_spec = custom_spec
  )
  certainty_overflow <- match.arg(certainty_overflow, c("error", "allow"))

  validate_prn(prn_name, method, custom_spec = custom_spec)

  if (!is_null(current_stage$draw_spec)) {
    cli_abort(
      "{.fn draw} already called for this stage. Use {.fn add_stage} to start a new stage."
    )
  }

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
  if (length(terms) == 0 || !all(vapply(terms, is.symbol, logical(1)))) {
    cli_abort(
      "{.arg {arg}} must contain bare column names, for example {.code {arg} = c(x, y)}."
    )
  }
  unique(vapply(terms, as_label, character(1)))
}

#' Parse ordinary cube auxiliaries and bound() markers
#' @noRd
parse_balanced_aux <- function(quo) {
  if (quo_is_null(quo)) {
    return(list(aux = NULL, bounds = NULL))
  }
  expr <- quo_get_expr(quo)
  terms <- if (is_call(expr, "c", ns = "")) as.list(expr)[-1] else list(expr)
  if (length(terms) == 0) {
    cli_abort(
      "{.arg aux} must contain at least one column or {.fn bound} marker."
    )
  }

  aux <- character(0)
  bounds <- character(0)
  for (term in terms) {
    if (is.symbol(term)) {
      aux <- c(aux, as_label(term))
      next
    }
    if (is_call(term, "bound", ns = "")) {
      args <- as.list(term)[-1]
      if (length(args) != 1L || !is.symbol(args[[1]])) {
        cli_abort(
          "{.fn bound} must contain exactly one bare column name; use separate calls for separate margins."
        )
      }
      bounds <- c(bounds, as_label(args[[1]]))
      next
    }
    cli_abort(
      c(
        "Unsupported expression in {.arg aux}: {.code {as_label(term)}}.",
        "i" = "Use bare numeric columns and single-column {.fn bound} markers."
      )
    )
  }
  aux <- unique(aux)
  bounds <- unique(bounds)
  list(
    aux = if (length(aux) > 0) aux else NULL,
    bounds = if (length(bounds) > 0) bounds else NULL
  )
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
      call = call
    )
  }

  if (check_keys) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Internal error: {.arg strata_vars} must be provided",
        call = call
      )
    }

    missing_vars <- setdiff(strata_vars, names(df))
    if (length(missing_vars) > 0) {
      cli_abort(
        c(
          "Data frame for {.arg {value_col}} is missing stratification variable{?s}:",
          "x" = "{.val {missing_vars}}"
        ),
        call = call
      )
    }
  }

  if (!value_col %in% names(df)) {
    cli_abort(
      "Data frame for {.arg {value_col}} must contain a {.val {value_col}} column",
      call = call
    )
  }

  if (check_keys) {
    key_df <- df[, strata_vars, drop = FALSE]
    if (anyDuplicated(key_df) > 0) {
      cli_abort(
        "Data frame for {.arg {value_col}} has duplicate rows for the same stratum",
        call = call
      )
    }
  }

  values <- df[[value_col]]
  if (value_col %in% c("n", "frac")) {
    if (!is_finite_numeric(values)) {
      cli_abort(
        "{.arg {value_col}} values must be finite numbers (no NA/NaN/Inf)",
        call = call
      )
    }
  }

  if (value_col == "n") {
    if (any(values <= 0)) {
      cli_abort("{.arg n} values must be positive", call = call)
    }
    if (!is_integerish_numeric(values)) {
      cli_abort("{.arg n} values must be integer-valued", call = call)
    }
  }

  if (value_col == "frac") {
    if (any(values <= 0)) {
      cli_abort("{.arg frac} values must be positive", call = call)
    }
    # The declared type is the truth for custom methods; the name test
    # only classifies built-ins (a custom name is never in the method
    # vectors, so the name test alone would call every custom method
    # WOR and wrongly reject frac > 1 for custom WR methods).
    is_wor <- if (!is_null(custom_spec)) {
      custom_spec$type %in% c("wor", "balanced")
    } else {
      !is_null(method) && !(method %in% c(wr_methods, pmr_methods))
    }
    if (is_wor && any(values > 1)) {
      cli_abort(
        "{.arg frac} cannot exceed 1 for without-replacement methods",
        call = call
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
    cli_abort("PPS methods require {.arg mos} (measure of size)", call = call)
  }

  if (!is_pps && !is_balanced && !is_null(mos)) {
    cli_warn("{.arg mos} is ignored for non-PPS methods")
  }

  if (!is_null(aux) && !is_balanced) {
    cli_abort(
      c(
        "{.arg aux} is only supported for balanced sampling
         ({.val cube} or a custom method registered with
         {.code type = \"balanced\"}).",
        "x" = "Current method: {.val {method}}"
      ),
      call = call
    )
  }

  if (!is_null(aux)) {
    if (!is.character(aux) || length(aux) < 1) {
      cli_abort("{.arg aux} must specify at least one column name", call = call)
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
      call = call
    )
  }

  if (!is_null(bounds) && !identical(method, "cube")) {
    cli_abort(
      c(
        "{.fn bound} constraints are only supported by {.code method = \"cube\"}.",
        "x" = "Current method: {.val {method}}"
      ),
      call = call
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
      call = call
    )
  }
  if (supports_spread && is_null(spread)) {
    cli_abort(
      "Method {.val {method}} requires {.arg spread} coordinates.",
      call = call
    )
  }
  if (supports_spread && (!is_null(aux) || !is_null(bounds))) {
    cli_abort(
      "Method {.val {method}} cannot combine {.arg spread} with cube auxiliary or count-bound constraints.",
      call = call
    )
  }

  is_random_size <- method %in%
    rs_poisson_methods ||
    (!is_null(custom_spec) && !custom_spec$fixed_size)
  if (is_random_size) {
    if (!is_null(n) && !is_null(frac)) {
      cli_abort(
        "Specify either {.arg n} (expected sample size) or {.arg frac}, not both",
        call = call
      )
    }
    if (is_null(n) && is_null(frac)) {
      cli_abort(
        "{.val {method}} sampling requires {.arg n} or {.arg frac}",
        call = call
      )
    }
  } else if (method == "pps_cps") {
    if (!is_null(frac)) {
      cli_abort(
        "{.val pps_cps} sampling requires {.arg n}, not {.arg frac}",
        call = call
      )
    }
    if (is_null(n)) {
      cli_abort("{.val pps_cps} sampling requires {.arg n}", call = call)
    }
  } else {
    if (is_null(n) && is_null(frac)) {
      cli_abort("Specify either {.arg n} or {.arg frac}", call = call)
    }
    if (!is_null(n) && !is_null(frac)) {
      cli_abort("Specify either {.arg n} or {.arg frac}, not both", call = call)
    }
  }

  if (!is_null(n) && !n_is_df) {
    if (!is.numeric(n)) {
      cli_abort("{.arg n} must be numeric or a data frame", call = call)
    }
    if (!is_finite_numeric(n)) {
      cli_abort("{.arg n} must not contain NA, NaN, or Inf", call = call)
    }
    if (length(n) > 1 && !is_null(names(n)) && is_null(strata_vars)) {
      cli_abort(
        c(
          "Named {.arg n} requires stratification at this stage.",
          "i" = "Add {.fn stratify_by} before this {.fn draw} (per-stage; stage-1 strata do not carry over), or pass a scalar."
        ),
        call = call
      )
    }
    if (length(n) > 1 && !is_null(names(n)) && length(strata_vars) > 1) {
      cli_abort(
        c(
          "Named {.arg n} vectors are only supported for single stratification variables.",
          "i" = "Use a data frame with columns {.val {strata_vars}} and {.val n}."
        ),
        call = call
      )
    }
    if (length(n) > 1 && (is_null(names(n)) || is_null(strata_vars))) {
      cli_abort(
        "{.arg n} must be a scalar, a named vector, or a data frame",
        call = call
      )
    }
    if (any(n <= 0)) {
      cli_abort("{.arg n} must be positive", call = call)
    }
    if (!is_integerish_numeric(n)) {
      cli_abort("{.arg n} must be integer-valued", call = call)
    }
  }

  if (!is_null(frac) && !frac_is_df) {
    if (!is.numeric(frac)) {
      cli_abort("{.arg frac} must be numeric or a data frame", call = call)
    }
    if (!is_finite_numeric(frac)) {
      cli_abort("{.arg frac} must not contain NA, NaN, or Inf", call = call)
    }
    if (length(frac) > 1 && (is_null(names(frac)) || is_null(strata_vars))) {
      cli_abort(
        "{.arg frac} must be a scalar, a named vector, or a data frame",
        call = call
      )
    }
    if (any(frac <= 0)) {
      cli_abort("{.arg frac} must be positive", call = call)
    }
    # See validate_draw_df(): custom_spec$type first, name test only
    # for built-ins.
    is_wor <- if (!is_null(custom_spec)) {
      custom_spec$type %in% c("wor", "balanced")
    } else {
      !(method %in% c(wr_methods, pmr_methods))
    }
    if (is_wor && any(frac > 1)) {
      cli_abort(
        "{.arg frac} cannot exceed 1 for without-replacement methods",
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
  call = rlang::caller_env()
) {
  if (!is_null(min_n)) {
    if (!is.numeric(min_n) || length(min_n) != 1 || !is_finite_numeric(min_n)) {
      cli_abort("{.arg min_n} must be a single positive integer", call = call)
    }
    if (min_n < 1 || !is_integerish_numeric(min_n)) {
      cli_abort("{.arg min_n} must be a positive integer", call = call)
    }
    if (!has_alloc) {
      cli_warn(
        "{.arg min_n} only applies when an allocation method is specified in {.fn stratify_by}"
      )
    }
  }

  if (!is_null(max_n)) {
    if (!is.numeric(max_n) || length(max_n) != 1 || !is_finite_numeric(max_n)) {
      cli_abort("{.arg max_n} must be a single positive integer", call = call)
    }
    if (max_n < 1 || !is_integerish_numeric(max_n)) {
      cli_abort("{.arg max_n} must be a positive integer", call = call)
    }
    if (!has_alloc) {
      cli_warn(
        "{.arg max_n} only applies when an allocation method is specified in {.fn stratify_by}"
      )
    }
  }

  if (!is_null(min_n) && !is_null(max_n) && min_n > max_n) {
    cli_abort(
      "{.arg min_n} ({min_n}) cannot be greater than {.arg max_n} ({max_n})",
      call = call
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
      call = call
    )
  }

  has_certainty <- !is_null(certainty_size) || !is_null(certainty_prop)
  if (!has_certainty) {
    return(invisible(NULL))
  }

  if (is_null(mos)) {
    cli_abort(
      "Certainty selection requires {.arg mos} to be specified.",
      call = call
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
      call = call
    )
  }

  if (certainty_size_is_df) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Data frame for {.arg certainty_size} requires stratification. Use {.fn stratify_by} first.",
        call = call
      )
    }
    validate_draw_df(certainty_size, strata_vars, "certainty_size", call = call)
    vals <- certainty_size$certainty_size
    if (!is_finite_numeric(vals) || any(vals <= 0)) {
      cli_abort(
        "{.arg certainty_size} values must be positive numbers.",
        call = call
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
        call = call
      )
    }
  }

  if (certainty_prop_is_df) {
    if (is_null(strata_vars)) {
      cli_abort(
        "Data frame for {.arg certainty_prop} requires stratification. Use {.fn stratify_by} first.",
        call = call
      )
    }
    validate_draw_df(certainty_prop, strata_vars, "certainty_prop", call = call)
    vals <- certainty_prop$certainty_prop
    if (!is_finite_numeric(vals) || any(vals <= 0) || any(vals >= 1)) {
      cli_abort(
        "{.arg certainty_prop} values must be between 0 and 1 (exclusive).",
        call = call
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
        call = call
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
      call = call
    )
  }
  invisible(NULL)
}
