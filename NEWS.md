# samplyr 0.8.9999

Initial release. samplyr specifies survey sampling designs as pipelines,
executes them against sampling frames, and carries the design metadata
needed for weighting, replay, and export to the survey and srvyr packages.

## Design grammar

* Five verbs and one modifier build a design independently of any frame:
  `sampling_design()`, `add_stage()`, `stratify_by()`, `cluster_by()`,
  `draw()`, and `execute()`. A design is reusable across frames.
* Within a stage, `stratify_by()` and `cluster_by()` come in either order
  and `draw()` closes the stage. A `stratify_by()`, a `cluster_by()` or a
  second `draw()` on a closed stage is refused with
  `samplyr_error_stage_closed`, since `draw()` reads the stage's strata and
  clusters when it is called. A design is extended with `add_stage()`.
* `execute()` takes one hierarchy frame or one register per stage, mapped
  by position. Registers are supplied whole and are restricted to the units
  the parent stage selected. Partial execution with `stages` and stage
  continuation on a partial sample run the same stage transition and draw
  the same sample under one RNG stream. A frame of another data frame class
  is read by its columns, and a grouped frame is used ungrouped.
* Two-phase sampling is a `tbl_sample` piped into `execute()`. Phase
  weights compound.
* `validate_frame()` runs every check `execute()` runs before sampling,
  against the frame each stage will select from. Both judge a later stage
  on every unit it could reach, so whether a frame is accepted does not
  depend on the seed (`samplyr_error_frame_invalid`), and both refuse a
  size in `n` or `frac` for a stratum no reachable unit belongs to
  (`samplyr_error_alloc_unknown_strata`). `frame_summary(design,
  frame)` previews every pool and chance without drawing a random number.
* Arguments after `...` are matched exactly. A misspelled argument is
  reported with the name it most likely meant. A choice argument such as
  `alloc`, `round` or `fingerprint` takes one of its values in full.
* Every error, warning and message samplyr signals carries a stable class
  (`samplyr_error_*`, `samplyr_warning_*`, `samplyr_message_*`), and a
  refusal raised inside a stratum keeps its class. An internal
  inconsistency is `samplyr_error_internal`. A missing suggested package is
  reported by rlang as `rlib_error_package_not_found`.
* A design file is checked at `execute()` as the verbs check a design, so an
  unknown allocation name or `on_empty` value in a file is refused with the
  class the verb would give.
* First mistakes are named for what they are: a frame where the design goes
  (`samplyr_error_frame_misplaced`), a string where a column goes
  (`samplyr_error_draw_string_column`, with `.data[[v]]` for a name held in
  a variable), a size table keyed on the parent stage's units
  (`samplyr_error_draw_parent_keyed_take`), a svyplan allocation at a stage
  stratified on several variables
  (`samplyr_error_svyplan_multivariable_strata`), and a misspelled method,
  which suggests the likely one. The design print states the scope of a
  later stage's size ("n = 3 (per ea_id)"), and `validate_frame()` counts
  the listing rows under units a partial sample did not select.

## Sampling methods

* Sixteen built-in methods. Equal probability: `srswor`, `srswr`,
  `systematic`, `bernoulli`. PPS without replacement: `pps_systematic`,
  `pps_brewer`, `pps_cps`, `pps_sampford`, `pps_poisson`, `pps_sps`,
  `pps_pareto`. PPS with or minimum replacement: `pps_multinomial`,
  `pps_chromy`. Balanced: `cube`, `lpm2`, `scps`.
* Permanent random numbers for coordinated sampling with `bernoulli`,
  `pps_poisson`, `pps_sps`, `pps_pareto`, and `scps`. With `prn`, `scps`
  visits each pool in the frame's row order, or the `control` order, so
  coordinated draws need the same order.
* Spreading `scps` on a measure of response burden and drawing a second
  survey with `1 - prn` gives the adapted SCP sampling of Matei, Smith,
  Smeets and Klingwort (2023), which keeps the number of burdened units
  steady across samples and rarely takes them twice.
* Random-size methods accept `n` as an expected size or `frac` as a
  sampling fraction, and `on_empty` governs an empty realization.
* `bound()` markers inside `aux` add integer count constraints to cube
  sampling, and `spread = c(x, y)` gives spatially balanced draws.
* Custom methods registered with `sondage::register_method()` are available
  as `pps_<name>` or `balanced_<name>`. Their declared probability tier,
  size behavior, PRN support (balanced methods included), and variance
  family flow through validation, execution, joint probabilities, and
  export. A method declaring unknown
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
* An auxiliary input the allocation does not read, such as `cost` with
  `alloc = "neyman"`, is refused with `samplyr_error_alloc_unused_aux`
  instead of being ignored.
* `alloc = "proportional"` takes an optional `importance`: each stratum's
  share is then in proportion to that size, such as its household total,
  rather than to its number of units, so a clustered stage can allocate its
  PSUs by the households they hold.
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
  with a `.replicate` column. The replicate seeds are drawn from `seed` and
  recorded in the sample, since consecutive seeds do not give independent
  draws in R.

## Serialization

* `write_design()`, `read_design()`, and `design_json()` save and restore
  designs and executed samples as versioned JSON. `replay_design()` re-runs
  a recorded execution and reproduces the sample, including panel and
  replicate assignments.
* A sample built by several `execute()` calls (a stage continuation, a new
  phase, or a materialized panel wave) records every earlier call in its
  receipt, and `replay_design()` re-runs them in order from the frames of
  each call. A continuation of a sample modified after its execution cannot
  be recorded, and writing it warns with the recipe to follow instead.
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
  inspection view with `.frame` and `.domain`. A unit selected in two
  components must have the same membership in both, or the stack is refused
  with `samplyr_error_stack_frames_membership_conflict`.
* Overlaps come from declared columns through `declared_overlaps()`, or from
  `exante_overlaps()`, which resolves each unit's chance in every frame from
  the component designs. `exante_probabilities()` gives the chance a design
  would assign each unit of a register without drawing. Approximate
  probabilities are refused unless `allow_approximate = TRUE`.
* `as_svydesign()` exports two frames through `survey::multiframe()` with
  the multiplicity or Hartley estimator. `as_svrepdesign()` builds one
  replicate system per frame and takes any number of frames.

## Survey export

* `variance_estimators()` reports, before anything is drawn, how each
  variance estimator the export functions offer would treat a sample of a
  design: supported, approximate or refused, with the condition class the
  export would raise. What the design decides is kept apart from what the
  frame decides and from what only the realized sample can settle, such as a
  primary unit left empty. A phase-1 sample given as the frame assesses the
  two-phase export. The page `?variance-routes` is now
  `?variance-estimation`.
* `as_svydesign()` produces a `survey::svydesign()` with the strata, cluster
  identifiers, weights, and finite population corrections of every executed
  stage. It covers PPS without replacement through Brewer or an exact joint
  matrix, with-replacement and minimum-replacement methods, certainty
  strata, balanced sampling, and two-phase designs through
  `survey::twophase()`. `as_survey_design()` and `as_survey_rep()` return
  srvyr objects.
* A two-phase sample whose phase 1 was drawn with unequal probabilities
  without replacement is refused with `samplyr_error_twophase_phase1_pps`,
  since `survey::twophase()` has no unequal-probability treatment at
  phase 1. Its weights are exact, and `?as_svydesign` gives an
  ultimate-cluster approximation with its measured margins.
* A single-stage phase 2 drawn with `pps_sampford`, `pps_cps`,
  `pps_brewer`, `pps_sps` or `pps_pareto` exports through
  `survey::twophase(method = "full")` with joint inclusion probabilities
  computed on the phase-1 sample. The phase-2 term is read in the
  Sen-Yates-Grundy form. Any other unequal-probability phase 2, and a
  `pps` argument to a two-phase export, is refused with
  `samplyr_error_twophase_phase2_pps`.
* A two-phase export whose phase 1 selects clusters and whose phase 2 draws
  smaller units across them warns with
  `samplyr_warning_twophase_across_units`: its variance is negative in a
  quarter to a half of samples. Phase 2 drawn inside each phase-1 unit,
  with `stratify_by()`, is stable.
* `as_svrepdesign()` produces replicate-weight designs. `subbootstrap` and
  `mrbbootstrap` serve PPS and balanced designs, and `type = "rwyb"` gives
  Rao-Wu-Yue-Beaumont replication through svrep, including multistage
  designs with Poisson stages. `type = "random_groups"` takes the replicates
  of `execute(reps = R)` as independent samples and estimates the variance
  from their spread, for any selection method. Replicates that share an
  earlier stage or phase are refused with
  `samplyr_error_random_groups_shared`.
* The jackknife, BRR, Fay and bootstrap types take a PPS first stage as
  drawn with replacement, which errs toward a larger variance, and warn with
  `samplyr_warning_replicate_wr_first_stage`. Certainty PSUs are resampled
  through their stage-two units, and a certainty unit with no stage below
  keeps its weight in every replicate. `type = "rwyb"` accepts
  `lonely.psu = "certainty"` for final-stage singletons.
* Poisson sampling is refused by the generic replicate types, and by
  linearization at any stage after the first or in a two-phase bridge,
  because those paths lose the random sample-size variance. `systematic_variance` controls how the approximate
  variance of a `systematic` or `pps_systematic` stage is reported. Both are
  approximated whatever formula stands in, and a frame whose period matches
  the interval can make either report a small fraction of the true variance.
* `joint_expectation()` returns pairwise joint inclusion probabilities or
  expected hits, from the frame or from the recorded digest alone. A frame
  is checked against the recorded digest and refused when it is not the one
  the sample was drawn from.

## Sample integrity

* A `tbl_sample` whose rows or design columns changed after execution is
  marked as modified and refused by the export and diagnostic functions. An
  order-invariant hash of the protected columns is verified at the analysis
  boundary, so changes made through base R are caught too. The refusal
  names the column that changed. Extracting one complete replicate stays
  supported, and so does turning a factor into a character vector of the
  same labels.
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
  classification exactly. A plan's single size at a stratified stage needs
  `alloc` to be distributed. Without it `draw()` refuses with
  `samplyr_error_svyplan_total_per_stratum`, since the size would otherwise
  be taken in every stratum.
* A certainty-aware plan solved with `n_psu_per_zone = 2` is fielded zone
  by zone. Stage 1 draws two PSUs from every zone, each with probability
  twice its share of the zone's size, and records the zone in `.zone_1`.
  The survey export uses stratum by zone as the variance strata, so every
  stratum outside certainty holds two sampled PSUs.
* A plan solved with `n_psu_per_zone = 1` draws one PSU from every zone and
  records the variance group svyplan fixed for its zone in `.pair_1`. The
  export collapses the zones in those groups, which may cross strata, and
  gives the zones' PSUs no finite population correction, since one draw
  per zone has the with-replacement variance. Linearization, RWYB and the
  replicate types all read it that way, and balanced half-samples are
  refused when a group holds three zones. A single selection is reported
  per group, not per stratum.
* `joint_expectation()` computes a stage drawing two PSUs per zone from
  the frame, each zone a draw of its own. It refuses a stage drawing one
  PSU per zone, since no unbiased variance estimator can use that matrix.
  Design files carry the zones, the groups and the PSUs per zone.
* PSUs smaller than their take are merged before the register is built with
  `svyplan::merge_psus()`, and the merged frame is fielded as planned.
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
* `summary()` returns a `summary_tbl_sample` object that prints one section
  per stage with the design, the realization, the certainty selections, and
  weight diagnostics (mean, range, CV, Kish DEFF, and effective n). Its
  fields hold the same figures.
* Capping events are reported once per stage with typed conditions:
  `samplyr_warning_size_capped`, `samplyr_warning_census`,
  `samplyr_warning_nominal_cap`, `samplyr_warning_poisson_shortfall`, and
  `samplyr_message_allocation_capped`. Each carries the stage and a payload
  of counts.
* `on_empty` also covers a unit selected at the previous stage that has no
  rows in this stage's frame, such as a household with no eligible member.
  `"error"` refuses as before. `"warn"` (`samplyr_warning_empty_parent`) and
  `"silent"` keep the unit with nothing selected under it, recorded in the
  sample, and `as_svydesign()` warns with
  `samplyr_warning_export_empty_parent` that the parent stage's variance
  is computed without it. A selected primary unit left with no row at all
  is refused at export with `samplyr_error_export_empty_psu`, since the
  variance between primary units would lose its zero total.
* Strata that draw a single unit outside certainty, and so give no variance
  estimate of their own, are named once per stage with the message
  `samplyr_message_singleton_pool`, while the allocation can still change.

## Datasets

* `bfa_eas`, 44,570 enumeration areas from Burkina Faso, with companion
  tables `bfa_eas_variance` and `bfa_eas_cost`.
* `zwe_eas`, 107,250 enumeration areas from Zimbabwe for two-stage
  demographic and health surveys.
* `ken_enterprises`, 17,004 synthetic establishments from Kenya for
  enterprise surveys, panels, and PRN coordination.

## Vignettes

Eight vignettes: get started, analysis with `survey` and `srvyr`, planning
with `svyplan`, coordination with permanent random numbers, rotating panels,
saving and replaying designs, design semantics, and validation on synthetic
populations.
