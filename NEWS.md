# samplyr 0.8.9999

Initial release. samplyr specifies survey sampling designs as pipelines,
executes them against sampling frames, and carries the design metadata
needed for weighting, replay, and export to the survey and srvyr packages.

## Design grammar

* Five verbs and one modifier build a design independently of any frame:
  `sampling_design()`, `add_stage()`, `stratify_by()`, `cluster_by()`,
  `draw()`, and `execute()`. A design is reusable across frames.
* `execute()` takes one hierarchy frame or one register per stage, mapped
  by position. Registers are supplied whole and are restricted to the units
  the parent stage selected. Partial execution with `stages` and stage
  continuation on a partial sample run the same stage transition and draw
  the same sample under one RNG stream.
* Two-phase sampling is a `tbl_sample` piped into `execute()`. Phase
  weights compound.
* `validate_frame()` runs every check `execute()` runs before sampling,
  against the frame each stage will select from. `frame_summary(design,
  frame)` previews every pool and chance without drawing a random number.
* Arguments after `...` are matched exactly. A misspelled argument is
  reported with the name it most likely meant, and every refusal carries a
  stable `samplyr_error_*` condition class.

## Sampling methods

* Sixteen built-in methods. Equal probability: `srswor`, `srswr`,
  `systematic`, `bernoulli`. PPS without replacement: `pps_systematic`,
  `pps_brewer`, `pps_cps`, `pps_sampford`, `pps_poisson`, `pps_sps`,
  `pps_pareto`. PPS with or minimum replacement: `pps_multinomial`,
  `pps_chromy`. Balanced: `cube`, `lpm2`, `scps`.
* Permanent random numbers for coordinated sampling with `bernoulli`,
  `pps_poisson`, `pps_sps`, and `pps_pareto`.
* Random-size methods accept `n` as an expected size or `frac` as a
  sampling fraction, and `on_empty` governs an empty realization.
* `bound()` markers inside `aux` add integer count constraints to cube
  sampling, and `spread = c(x, y)` gives spatially balanced draws.
* Custom methods registered with `sondage::register_method()` are available
  as `pps_<name>` or `balanced_<name>`. Their declared probability tier,
  size behaviour, PRN support, and variance family flow through validation,
  execution, joint probabilities, and export. A method declaring unknown
  first-order probabilities is refused, since its weights would be biased.
* Every method states whether its first-order probabilities are exact or
  approximate. `pps_sps` and `pps_pareto` are approximate, and the digest,
  the summary, and serialized designs record that.

## Stratification and allocation

* Proportional, equal, Neyman, optimal, and power allocation through
  `stratify_by(alloc = )`, custom allocations as named vectors or data
  frames, and `min_n` and `max_n` bounds per stratum.
* Allocations preserve their total and their criterion. A stratum too small
  for its share is capped at its population and the surplus is redistributed
  by the method's own factors, reported once with class
  `samplyr_message_allocation_capped`. Infeasible bounds are refused.
* An allocation that gives a nonempty stratum zero units is refused. Use
  `min_n = 1` to require positive allocations.
* Control sorting with `control = c(var1, var2)`, and serpentine order with
  `serp()`.

## Certainty selection

* PPS methods take units with probability one through `certainty_size` or
  `certainty_prop` thresholds, including stratum-specific thresholds from a
  data frame and iterative identification for proportional thresholds.
* `.certainty_k` records every inclusion probability that resolves to one,
  whether from a threshold, from probability capping, or from a balanced
  design. The digest, joint probabilities, and survey export treat such
  units identically, in a take-all stratum with no variance contribution at
  that stage.
* `certainty_overflow = "allow"` permits an all-certainty census larger than
  its target. A certainty rule that would leave noncertainty units with zero
  probability is refused.
* For `pps_poisson`, `n` and `frac` both specify an expected total that
  includes certainty units, so the two spellings give the same probabilities.

## Panels, rotation, and waves

* `execute(panels = k)` assigns units to `k` panels by randomized fixed
  quota in frozen ordered blocks, so each unit carries each panel with
  probability `1/k` and panel sizes differ by at most one. `panel_stage`
  moves the assignment below the first stage for address-panel designs.
* `panels` also accepts a rotation schedule, a panel-by-wave data frame with
  an `active` column, or an `svyplan_schedule`. `small_pool` governs pools
  too small to rotate.
* `execute(master, wave = t)` materializes one occasion with exact
  activation weights and its own receipt. `stack_waves()` verifies that
  several waves come from one master and stacks them into a long table for
  change estimation. `joint_expectation(master, waves = c(t, s))` gives the
  exact between-wave joint activation probabilities block by block.
* `rotation_program()` links the cohorts of a replenishing panel drawn
  against successive frame vintages, records entry occasions, and
  materializes each live cohort with its own weights.
* `execute(reps = R)` draws `R` independent replicates as one stacked sample
  with a `.replicate` column. Replicate `r` is seeded at `seed + r - 1`.

## Serialization

* `write_design()`, `read_design()`, and `design_json()` save and restore
  designs and executed samples as versioned JSON. `replay_design()` re-runs
  a recorded execution and reproduces the sample, including panel and
  replicate assignments.
* Files record the frame variables each stage needs, an optional frame
  fingerprint but never the data, an execution receipt with every argument
  that affects the result, the RNG configuration and package versions, and
  the frame digest. Custom methods are fingerprinted, so replay refuses a
  re-registered function whose code differs.
* Frame collections from `stack_frames()` and shared-weight samples from
  `share_weights()` have formats of their own, `samplyr/frame-stack` and
  `samplyr/shared-sample`, and replay exactly.
* Documents are validated in native R against bundled JSON Schemas, which
  are installed under `system.file("schema", package = "samplyr")` for use
  outside R. Unknown executable fields, duplicate keys, and declared
  executable extensions are refused. Named `annotations` and `tools`
  namespaces are preserved through round trips. `read_design()` accepts
  files and JSON strings only and never touches the network.
* The format is native to samplyr and experimental.

## Indirect sampling

* `share_weights()` applies the generalized weight share method, turning a
  sample of one population into a weighted sample of a linked target
  population. The link denominator is always stated, through a multiplicity
  column, `complete_links()`, or `weighted_links()`, and the target cluster
  is given explicitly as a column, as `NULL`, or through `extend_links()`.
* The result carries an estimation weight, and every consumer honours that
  contract. `as_svrepdesign()` applies the share operator inside each
  replicate, `as_svydesign()` exports source-target contributions, and
  consumers that cannot handle a shared weight refuse with a class
  inheriting `samplyr_error_weight_contract`.
* Target clusters that no source unit can reach are recorded and warned
  about at export with `samplyr_warning_unlinked_cluster`.

## Overlapping frames

* `stack_frames()` collects independent samples from frames that overlap on
  one population, keeping each component and its receipt apart. Membership
  is declared per frame as logical columns, and `as.data.frame()` gives an
  inspection view with `.frame` and `.domain`.
* Overlaps come from declared columns through `declared_overlaps()`, or from
  `exante_overlaps()`, which resolves each unit's chance in every frame from
  the component designs. `exante_probabilities()` gives the chance a design
  would assign each unit of a register without drawing. Approximate
  probabilities are refused unless `allow_approximate = TRUE`.
* `as_svydesign()` exports two frames through `survey::multiframe()` with
  the multiplicity or Hartley estimator. `as_svrepdesign()` builds one
  replicate system per frame and takes any number of frames.

## Survey export

* `as_svydesign()` produces a `survey::svydesign()` with the strata, cluster
  identifiers, weights, and finite population corrections of every executed
  stage. It covers PPS without replacement through Brewer or an exact joint
  matrix, with-replacement and minimum-replacement methods, certainty
  strata, balanced sampling, and two-phase designs through
  `survey::twophase()`. `as_survey_design()` and `as_survey_rep()` return
  srvyr objects.
* `as_svrepdesign()` produces replicate-weight designs. `subbootstrap` and
  `mrbbootstrap` serve PPS and balanced designs, and `type = "rwyb"` gives
  Rao-Wu-Yue-Beaumont replication through svrep, including multistage
  designs with Poisson stages.
* Poisson sampling is refused by the generic replicate types, and by
  linearization at any stage after the first or in a two-phase bridge,
  because those paths lose the random sample-size variance. `systematic_variance` controls how the approximate
  variance of an equal-probability systematic stage is reported.
* `joint_expectation()` returns pairwise joint inclusion probabilities or
  expected hits, from the frame or from the recorded digest alone.

## Sample integrity

* A `tbl_sample` whose rows or design columns changed after execution is
  marked as modified and refused by the export and diagnostic functions. An
  order-invariant hash of the protected columns is verified at the analysis
  boundary, so changes made through base R are caught too. Extracting one
  complete replicate stays supported.
* A plain data frame that still carries execution columns is refused as the
  frame of a fresh execution, which prevents rerunning stage 1 on an
  expanded listing.
* `tbl_sample` is a tibble subclass, and grouped dplyr verbs preserve its
  provenance.

## Planning with svyplan

* `draw()` accepts svyplan sample-size objects directly. Cluster plans
  contribute PSU counts at stage 1 and per-cluster takes below, stratified
  `n_alloc()` plans feed both stages by stratum, `n_multi()` plans become
  per-domain tables, and certainty-aware plans field their stored certainty
  classification exactly.
* `design_effect()`, `effective_n()`, and `varcomp()` have `tbl_sample`
  methods. The first two report Kish's weighting design effect from
  `.weight`. `varcomp()` estimates design-based variance components from an
  executed clustered sample for next-round planning.

## Frame digest and diagnostics

* Every execution records a frame digest of the selection pools,
  first-order chances, and selected units, with no unit identifiers.
  `frame_digest = "full"` keeps exact unit chances and `"none"` opts out.
  The digest lets `print()`, `summary()`, `frame_summary()`, and
  `joint_expectation()` describe a sample without its frame, and
  `validate_frame()` reports drift between a new frame and the recorded one.
* `summary()` prints one section per stage with the design, the
  realization, and weight diagnostics (mean, range, CV, Kish DEFF, and
  effective n).
* Capping events are reported once per stage with typed conditions:
  `samplyr_warning_size_capped`, `samplyr_warning_census`,
  `samplyr_warning_nominal_cap`, `samplyr_warning_poisson_shortfall`, and
  `samplyr_message_allocation_capped`. Each carries the stage and a payload
  of counts.

## Datasets

* `bfa_eas`, 44,570 enumeration areas from Burkina Faso, with companion
  tables `bfa_eas_variance` and `bfa_eas_cost`.
* `zwe_eas`, 107,250 enumeration areas from Zimbabwe for two-stage
  demographic and health surveys.
* `ken_enterprises`, 17,004 synthetic establishments from Kenya for
  enterprise surveys, panels, and PRN coordination.

## Vignettes

Nine vignettes: get started, a three-stage household sample, analysis with
`survey` and `srvyr`, planning with `svyplan`, coordination with permanent random
numbers, rotating panels, saving and replaying designs, design semantics,
and validation on synthetic populations.
