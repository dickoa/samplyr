# samplyr 0.1.0

Initial CRAN release.

## Fixes

* `draw()` now rejects `frac` when `stratify_by()` uses an allocation method
  (`equal`, `proportional`, `neyman`, `optimal`, `power`), with a classed error
  (`samplyr_error_alloc_frac_with_alloc`).

## Core Grammar

* Five verbs + one modifier for composable survey sampling designs:
  `sampling_design()`, `stratify_by()`, `cluster_by()`, `draw()`,
  `execute()`, and `add_stage()`.
* Frame-independent design pattern separating specification from execution.

## Sampling Methods

* Equal probability: SRS (with/without replacement), systematic, Bernoulli.
* PPS methods: systematic, Brewer, conditional Poisson (maximum entropy),
  Poisson, sequential Poisson (SPS), Pareto, multinomial, Chromy
  (Probability Minimum Replacement).
* Permanent random number (PRN) support for sample coordination:
  `bernoulli`, `pps_poisson`, `pps_sps`, `pps_pareto`.

## Allocation Methods

* Proportional, equal, Neyman, optimal, and power allocation.
* Custom allocation via data frames.
* Minimum and maximum sample size constraints (`min_n`, `max_n`).

## Features

* Multi-stage and multi-phase sampling with automatic weight compounding.
* Panel partitioning via `execute(..., panels = k)` for rotating panel surveys.
* Certainty selection for PPS methods (absolute and proportional thresholds,
  with `certainty_overflow = "allow"` for take-all overflow).
* Zero-selection handling for random-size methods (`on_empty` parameter).
* Control sorting with `serp()` for hierarchic serpentine ordering.
* Rounding control for fractional allocations (`round` parameter).
* Partial execution for operational sampling workflows.
* Weight diagnostics (Kish's effective sample size and deff) in `summary()`.
* Synthetic survey datasets from African countries for learning and testing.

## Survey Integration

* `as_svydesign()` converts `tbl_sample` objects to
  `survey::svydesign()` for variance estimation and analysis.
* `as_svrepdesign()` converts `tbl_sample` objects to
  `survey::svyrep.design` via `survey::as.svrepdesign()` for
  replicate-weight variance estimation (single-phase non-PPS designs).
* `as_survey_design()` method registered on `srvyr::as_survey_design()`
  converts `tbl_sample` objects directly to srvyr `tbl_svy` objects.
* `as_survey_rep()` method registered on `srvyr::as_survey_rep()`
  converts `tbl_sample` objects directly to srvyr replicate `tbl_svy`.
* `joint_expectation()` computes pairwise joint inclusion probabilities
  (pi_kl) for PPS WOR stages and joint expected hits for WR/PMR stages
  via `sondage::joint_inclusion_prob()` and `sondage::joint_expected_hits()`.
* `summary.tbl_sample()` provides per-stage stratum allocation tables
  and weight diagnostics.
