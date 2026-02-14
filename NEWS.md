# samplyr 0.1.0

Initial CRAN release.

## Fixes

* `draw()` now rejects `frac` when `stratify_by()` uses an allocation method
  (`equal`, `proportional`, `neyman`, `optimal`), with a classed error
  (`samplyr_error_alloc_frac_with_alloc`).

## Core Grammar

* Five verbs + one modifier for composable survey sampling designs:
  `sampling_design()`, `stratify_by()`, `cluster_by()`, `draw()`,
  `execute()`, and `add_stage()`.
* Frame-independent design pattern separating specification from execution.

## Sampling Methods

* Equal probability: SRS (with/without replacement), systematic, Bernoulli.
* PPS methods: systematic, Brewer, maximum entropy, Poisson, multinomial,
  Chromy (Probability Minimum Replacement).

## Allocation Methods

* Proportional, equal, Neyman, and optimal allocation.
* Custom allocation via data frames.
* Minimum and maximum sample size constraints (`min_n`, `max_n`).

## Features

* Multi-stage and multi-phase sampling with automatic weight compounding.
* Certainty selection for PPS methods (absolute and proportional thresholds).
* Control sorting with `serp()` for hierarchic serpentine ordering.
* Partial execution for operational sampling workflows.
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
* `joint_inclusion_prob()` computes exact joint inclusion probabilities
  (π_kl) for PPS WOR stages via `sondage::up_*_jip()`.
* `summary.tbl_sample()` provides per-stage stratum allocation tables
  and weight diagnostics.
