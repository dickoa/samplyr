# samplyr 0.8.9999

Initial release.

## Core grammar

* Frame-independent design specification with five verbs and one modifier:
  `sampling_design()`, `add_stage()`, `stratify_by()`, `cluster_by()`,
  `draw()`, and `execute()`.
* Designs are reusable across different frames.

## Sampling methods

* 13 methods in three families:
  - Equal probability: `srswor`, `srswr`, `systematic`, `bernoulli`.
  - PPS without replacement: `pps_systematic`, `pps_brewer`, `pps_cps`
    (maximum entropy), `pps_poisson`, `pps_sps`, `pps_pareto`.
  - PPS with replacement / PMR: `pps_multinomial`, `pps_chromy`.
* Balanced sampling via the cube method (`method = "balanced"`) with optional
  auxiliary balancing variables and measure of size. Stratified designs use
  the stratified cube algorithm. Supported for up to 2 stages.
* Permanent random numbers (PRN) for sample coordination:
  `bernoulli`, `pps_poisson`, `pps_sps`, `pps_pareto`.
* Random-size methods (`bernoulli`, `pps_poisson`) accept `n` (expected size)
  or `frac` (sampling fraction).
* Custom PPS methods registered via `sondage::register_method()` are accepted
  using the `pps_<name>` convention. Method metadata (WOR/WR type, fixed size,
  PRN support) flows through validation, execution, joint probabilities, and
  survey export.

## Stratification and allocation

* Five allocation methods via `stratify_by(..., alloc =)`: proportional,
  equal, Neyman, optimal, and power.
* Custom allocation via named vectors or data frames.
* Minimum and maximum sample size constraints per stratum (`min_n`, `max_n`).

## Multi-stage and multi-phase

* Multi-stage sampling with `add_stage()`. Weights compound automatically
  across stages.
* Partial execution via `execute(..., stages = 1)` for operational workflows.
* Two-phase sampling by piping a `tbl_sample` into `execute()`.

## Certainty selection

* PPS WOR methods support certainty selection via absolute (`certainty_size`)
  or proportional (`certainty_prop`) thresholds, including iterative
  identification for proportional thresholds.
* `certainty_overflow = "allow"` returns all certainty units when they
  exceed `n`.
* Stratum-specific thresholds via data frames.

## Panel partitioning

* `execute(..., panels = k)` assigns units to `k` panels via systematic
  within-stratum interleaving.
* Multi-stage designs assign panels at PSU level and propagate to all units.

## Replicated sampling

* `execute(..., reps = R)` draws R independent samples from the same frame
  under the same design. Output is a single stacked `tbl_sample` with a
  `.replicate` column (integer 1 through R).
* Replicate r uses seed `seed + r - 1` (SAS convention).
* Continuation from a replicated partial sample auto-loops per replicate.
* Cannot be combined with `panels` or with stages that use permanent random
  numbers.
* Survey export functions (`as_svydesign()`, `as_svrepdesign()`,
  `joint_expectation()`, `design_effect()`, `effective_n()`) require
  filtering to a single replicate first.

## Control sorting

* `control = c(var1, var2)` for nested sorting.
* `control = serp(var1, var2)` for serpentine (alternating direction) sorting.

## Survey export

* `as_svydesign()` converts `tbl_sample` to `survey::svydesign()` with
  correct strata, cluster IDs, weights, and finite population corrections.
  Handles PPS WOR (Brewer approximation or exact `ppsmat`), WR/PMR
  (`Inf` FPC, Hansen-Hurwitz), certainty strata, balanced sampling, and
  two-phase designs.
* `as_svrepdesign()` converts to replicate-weight designs. For PPS and
  balanced designs, `"subbootstrap"` and `"mrbbootstrap"` are supported.
* `as_survey_design()` and `as_survey_rep()` for direct conversion to
  srvyr `tbl_svy` objects.
* `joint_expectation()` computes pairwise joint inclusion probabilities
  (WOR) or joint expected hits (WR/PMR) for exact variance estimation.

## Survey planning

* `design_effect()` and `effective_n()` with `tbl_sample` methods. Five
  methods: Kish, Henry, Spencer, Chen-Rust, and cluster planning.
  Auto-extraction of strata, clusters, and selection probabilities from
  the stored design.
* `draw()` accepts `svyplan` sample size objects (`svyplan_n`, `svyplan_power`,
  `svyplan_cluster`) directly.
* Precision analysis (`prec_prop()`, `prec_mean()`, `prec_cluster()`,
  `prec_multi()`), sensitivity analysis (`predict()`), response rate
  adjustment (`resp_rate`), and confidence intervals (`confint()`) on
  all planning objects.

## Diagnostics

* `summary()` shows per-stage stratum allocation tables with N_h, n_h,
  f_h, and weight diagnostics (Kish DEFF, n_eff, CV).
* `validate_frame()` checks for missing variables, NA values in key
  columns, and MOS/PRN/auxiliary variable issues before execution.

## Datasets

* `bfa_eas`: 44,570 enumeration areas from Burkina Faso for household survey
  sampling. Companion tables `bfa_eas_variance` and `bfa_eas_cost` for Neyman
  and optimal allocation. Food insecurity calibrated from Cadre Harmonise.
* `zwe_eas`: 107,250 enumeration areas from Zimbabwe for two-stage cluster
  survey sampling. Population and households calibrated to 2022 Census
  ward-level tallies. Demographic columns from WorldPop 100m age-sex grids.
* `ken_enterprises`: 17,004 establishments from Kenya for enterprise surveys,
  panel partitioning, and PRN coordination examples. Calibrated to the
  Republic of Kenya 2025 WBES universe (KRA register, 6 regions, 7 sectors).

## Vignettes

* Introduction: full tutorial covering SRS through multi-stage PPS designs.
* Design semantics: assumptions, weight formulas, and method properties.
* Survey analysis: export to survey/srvyr, joint probabilities, two-phase.
* Sampling coordination: PRN workflows, positive/negative coordination.
* Survey planning: svyplan integration, sample size, precision, design effects.
* Validation: deterministic invariants and Monte Carlo coverage checks on
  synthetic populations.
