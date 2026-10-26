# samplyr 0.9.999

Initial release. samplyr specifies survey sampling designs as pipelines,
executes them against sampling frames, and carries the design metadata
needed for weighting, replay, and export to the survey and srvyr packages.

## Design grammar

* A design is built with `sampling_design()`, `add_stage()`,
  `stratify_by()`, `cluster_by()` and `draw()`, independently of any frame,
  and run with `execute()`. The same design can be reused across frames.
* Multistage designs run against one hierarchical frame or one register per
  stage. Stages can be executed one at a time, and `execute()` on a
  partial sample runs the remaining stages. A sample passed as the frame
  of a new design starts a second phase.
* Cluster ids may be numbered within the level above them, so town 1 of
  every county is its own unit. `cluster_by(nest = FALSE)` instead declares
  ids unique across strata and checks it.
* `validate_frame()` runs every check `execute()` would run, and
  `frame_summary()` previews pool sizes and selection chances without
  drawing. Both name the pools too small for their take, and
  `validate_frame()` also names the strata an explicit size table gives a
  take that a parent does not hold.
* `draw(on_capped = "error")` refuses a stage before anything is drawn when
  a fixed take exceeds a pool the stage could reach. By default such a pool
  is taken whole, with a warning.
* `compare_designs()` compares two designs on one frame, by allocation or
  by first-order inclusion probabilities. A protocol requirement written
  as a design can be checked this way without a reference implementation.
* Errors name the argument or frame column at fault and suggest the fix.
  Every condition carries a stable class for programmatic handling.

## Selection methods

* Sixteen built-in methods: simple random, systematic and Bernoulli
  sampling, seven PPS methods without replacement, two with or minimum
  replacement, and balanced sampling (cube, local pivotal, spatially
  correlated Poisson).
* Certainty selection for large units through absolute or proportional
  thresholds. Units a fixed-size PPS draw takes with certainty by their
  size are reported, and `frame_summary()` counts them before drawing.
* Permanent random numbers coordinate samples across surveys and waves.
* Custom methods registered with `sondage::register_method()` work
  throughout the package.

## Stratification and allocation

* Proportional, equal, Neyman, optimal and power allocation, custom sizes
  per stratum, and minimum and maximum sizes per stratum.
* Sampling fractions with bounds per stratum.
* Control sorting and serpentine ordering for implicit stratification.

## Panels and replication

* Panel assignment, rotation schedules, wave materialization with exact
  activation weights, windows of several waves pooled into one sample, and
  replenishing panels across frame vintages.
  `periods()` spreads a sample over fieldwork periods visited once, such as
  the months of a yearly survey, and the later stages can be drawn one
  period or tenure at a time as listing proceeds, a schedule's `visit`
  column opening new tenures in primary units that stay in the survey.
* `execute(reps = R)` draws independent replicate samples.

## Analysis and export

* `as_svydesign()`, `as_svrepdesign()`, `as_survey_design()` and
  `as_survey_rep()` export a sample with its strata, clusters, weights and
  finite population corrections for every stage.
* Linearization and replicate weights (bootstrap, jackknife, BRR,
  Rao-Wu-Yue-Beaumont, random groups) cover multistage, PPS, certainty,
  balanced and two-phase designs. `variance_estimators()` reports before
  sampling which estimators a design supports.
* A materialized wave exports by ultimate clusters, the variance rotating
  surveys publish, whatever method drew its master, and its replicate
  weights follow the same design. `method` gives its two-phase export.
* `joint_expectation()` gives joint inclusion probabilities.
* A sample edited after execution is detected and refused at export.

## Reproducibility

* `write_design()` and `read_design()` save designs and samples as
  versioned JSON with an execution receipt, and `replay_design()`
  reproduces a sample from its frame.
* Most executions record a digest of the frame, so `summary()` and the
  diagnostics describe a sample without its frame and `validate_frame()`
  detects a frame that changed. `frame_digest = "none"` turns it off, and
  `?execute` lists the executions that record none.

## Planning with svyplan

* `draw()` takes svyplan sample-size and allocation objects directly,
  including multistage, multi-domain and certainty-aware plans, and
  allocations over strata crossed from several variables.
* `design_effect()`, `effective_n()` and `varcomp()` measure an executed
  sample for the next round of planning.

## Indirect sampling and multiple frames

* `share_weights()` applies the generalized weight share method to reach a
  linked population.
* `stack_frames()` combines samples from overlapping frames for
  multiple-frame estimation.

## Datasets

* `bfa_eas`, 44,570 enumeration areas from Burkina Faso, with companion
  variance and cost tables.
* `zwe_eas`, 107,250 enumeration areas from Zimbabwe.
* `ken_enterprises`, 17,004 synthetic establishments from Kenya.

## Documentation

Four vignettes: get started, analysis with survey and srvyr, saving and
replaying designs, and design semantics. The book
[Survey Sampling Design with R](https://www.ahmadoudicko.com/sampling-design-r-book/)
covers planning, coordination, rotating panels and validation in depth.
