# samplyr 0.5.4999 (development)

## Core Grammar

* Five verbs and one modifier for composable survey sampling designs:
  `sampling_design()`, `stratify_by()`, `cluster_by()`, `draw()`,
  `execute()`, and `add_stage()`.
* Frame-independent design pattern: specification is separate from execution.
  Designs are reusable across different frames.

## Sampling Methods (13 total)

* Equal probability: SRS without replacement (`srswor`), SRS with
  replacement (`srswr`), systematic, and Bernoulli.
* PPS without replacement: systematic, Brewer, conditional Poisson (maximum
  entropy), Poisson, sequential Poisson (SPS), and Pareto.
* PPS with replacement / PMR: multinomial and Chromy (probability minimum
  replacement).
* Balanced sampling (`balanced`): the cube method (Deville & Tille 2004)
  with optional auxiliary variables (`aux`) and measure of size (`mos`).
  Stratified designs use the stratified cube algorithm (Chauvet 2009).
  Supported for up to 2 stages.
* Permanent random number (PRN) support for sample coordination:
  `bernoulli`, `pps_poisson`, `pps_sps`, `pps_pareto`.
* Random-size methods (`bernoulli`, `pps_poisson`) accept `n` as expected
  sample size or `frac` as sampling fraction.

## Stratification and Allocation

* Proportional, equal, Neyman, optimal, and power allocation via
  `stratify_by(..., alloc = )`.
* Custom allocation via named vectors or data frames.
* Minimum and maximum sample size constraints (`min_n`, `max_n`).

## Multi-Stage and Multi-Phase

* Multi-stage sampling with `add_stage()` and automatic weight compounding
  across stages.
* Partial execution via `execute(..., stages = 1)` for operational workflows.
* Two-phase sampling: pipe a `tbl_sample` into `execute()`.

## Panel Partitioning

* `execute(..., panels = k)` assigns units to `k` panels via systematic
  within-stratum interleaving.
* Multi-stage designs: panels assigned at PSU level and propagated.
* Full-sample weights preserved; analyst multiplies by `k` for per-panel
  estimation.

## Certainty Selection

* PPS WOR methods support certainty selection via `certainty_size`
  (absolute threshold) or `certainty_prop` (proportional threshold).
* Iterative identification for proportional thresholds.
* `certainty_overflow = "allow"` returns all certainty units when they
  exceed the target `n`.
* Stratum-specific thresholds via data frames.

## Control Sorting

* `control = c(var1, var2)` for nested (standard) sorting.
* `control = serp(var1, var2)` for serpentine (alternating direction)
  sorting.
* Applied within strata when combined with `stratify_by()`.

## Zero-Selection Handling

* `on_empty` parameter for random-size methods: `"error"` (default),
  `"warn"` (fall back to SRS of 1), or `"silent"`.

## Survey Export

* `as_svydesign()` converts `tbl_sample` to `survey::svydesign()` with
  correct strata, cluster ids, weights, and finite population corrections.
  Handles PPS WOR (Brewer approximation or exact ppsmat), WR/PMR (Inf FPC,
  Hansen-Hurwitz), certainty strata, balanced sampling, and two-phase
  designs.
* `as_svrepdesign()` converts to replicate-weight designs via
  `survey::as.svrepdesign()`. For PPS and balanced designs, `"subbootstrap"`
  and `"mrbbootstrap"` are supported; other types emit a warning.
* `as_survey_design()` and `as_survey_rep()` methods registered on srvyr
  generics for direct conversion to `tbl_svy` objects.
* `joint_expectation()` computes pairwise joint inclusion probabilities
  (WOR) or joint expected hits (WR/PMR) for exact variance estimation.
  Supports balanced sampling via high-entropy approximation.

## Survey Planning (svyplan >= 0.4.0 integration)

* `design_effect()` and `effective_n()` re-exported from svyplan with
  `tbl_sample` methods. Five methods: Kish, Henry, Spencer, Chen-Rust,
  and cluster planning.
* Auto-extraction of design metadata (strata, clusters, selection
  probabilities) from the stored sampling design.
* `draw()` accepts `svyplan_n`, `svyplan_power`, and `svyplan_cluster`
  objects directly from `n_prop()`, `n_mean()`, `n_multi()`,
  `n_cluster()`, `power_prop()`, `power_mean()`.
* Precision analysis: `prec_prop()`, `prec_mean()`, `prec_cluster()`,
  `prec_multi()` evaluate achieved precision at a given sample size.
  Bidirectional round-trip between `n_*()` and `prec_*()` via S3
  dispatch.
* Sensitivity analysis via `predict()` methods on all svyplan objects.
* Response rate adjustment via `resp_rate` parameter across all
  planning functions.
* Confidence intervals via `confint()` on sample size and precision
  objects.

## Diagnostics

* `summary.tbl_sample()` shows per-stage stratum allocation tables with
  N_h, n_h, f_h, and weight diagnostics (Kish DEFF, n_eff, CV).
* `validate_frame()` checks for missing variables, NA values in
  strata/cluster columns, and MOS/PRN/auxiliary variable issues before
  execution.

## Datasets

* `bfa_eas`: 14,900 enumeration areas from Burkina Faso (LSMS/HBS style).
  Companion tables `bfa_eas_variance` and `bfa_eas_cost` for Neyman and
  optimal allocation.
* `zwe_eas` and `zwe_households`: DHS-style two-stage cluster frame from
  Zimbabwe (22,600 EAs, 379,326 households).
* `ken_enterprises`: 6,823 establishments from Kenya for enterprise surveys,
  panel partitioning, and PRN coordination examples.
