# samplyr (development version)

## Core Grammar

* Five verbs + one modifier for composable survey sampling designs:
  `sampling_design()`, `stratify_by()`, `cluster_by()`, `draw()`,
  `execute()`, and `stage()`.
* Frame-independent design pattern separating specification from execution.

## Sampling Methods

* Equal probability: SRS (with/without replacement), systematic, Bernoulli.
* PPS methods: systematic, Brewer, maximum entropy, Poisson, multinomial,
  Chromy (PMR).

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

* `as_survey_design()` converts `tbl_sample` objects to
  `survey::svydesign()` for variance estimation and analysis.
* `joint_inclusion_prob()` computes exact joint inclusion probabilities
  (π_kl) for PPS WOR stages via `sondage::up_*_jip()`.
* `summary.tbl_sample()` provides per-stage stratum allocation tables
  and weight diagnostics.
