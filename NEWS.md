# samplyr 0.3.9999 (development)

## Core Grammar

* Five verbs and one modifier for composable survey sampling designs:
  `sampling_design()`, `stratify_by()`, `cluster_by()`, `draw()`,
  `execute()`, and `add_stage()`.
* Frame-independent design pattern: specification is separate from execution.
  Designs are reusable across different frames.

## Sampling Methods (12 total)

* Equal probability: SRS without replacement (`srswor`), SRS with
  replacement (`srswr`), systematic, and Bernoulli.
* PPS without replacement: systematic, Brewer, conditional Poisson (maximum
  entropy), Poisson, sequential Poisson (SPS), and Pareto.
* PPS with replacement / PMR: multinomial and Chromy (probability minimum
  replacement).
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
  Hansen-Hurwitz), certainty strata, and two-phase designs.
* `as_svrepdesign()` converts to replicate-weight designs via
  `survey::as.svrepdesign()`. For PPS designs, `"subbootstrap"` and
  `"mrbbootstrap"` are supported; other types emit a warning.
* `as_survey_design()` and `as_survey_rep()` methods registered on srvyr
  generics for direct conversion to `tbl_svy` objects.
* `joint_expectation()` computes pairwise joint inclusion probabilities
  (WOR) or joint expected hits (WR/PMR) for exact variance estimation.

## Survey Planning (svyplan integration)

* `design_effect()` and `effective_n()` re-exported from svyplan with
  `tbl_sample` methods. Five methods: Kish, Henry, Spencer, Chen-Rust,
  and cluster planning.
* Auto-extraction of design metadata (strata, clusters, selection
  probabilities) from the stored sampling design.
* `draw()` accepts `svyplan_n` and `svyplan_power` objects directly
  from `n_prop()`, `n_mean()`, `n_multi()`, `power_prop()`, `power_mean()`.

## Diagnostics

* `summary.tbl_sample()` shows per-stage stratum allocation tables with
  N_h, n_h, f_h, and weight diagnostics (Kish DEFF, n_eff, CV).
* `validate_frame()` checks for missing variables, NA values in
  strata/cluster columns, and MOS/PRN issues before execution.

## Datasets

* `bfa_eas`: 14,900 enumeration areas from Burkina Faso (LSMS/HBS style).
  Companion tables `bfa_eas_variance` and `bfa_eas_cost` for Neyman and
  optimal allocation.
* `zwe_eas` and `zwe_households`: DHS-style two-stage cluster frame from
  Zimbabwe (22,600 EAs, 379,326 households).
* `ken_enterprises`: 6,823 establishments from Kenya for enterprise surveys,
  panel partitioning, and PRN coordination examples.
