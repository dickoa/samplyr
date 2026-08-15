# samplyr 0.8.9999

Initial release.

## Core grammar

* Frame-independent design specification with five verbs and one modifier:
  `sampling_design()`, `add_stage()`, `stratify_by()`, `cluster_by()`,
  `draw()`, and `execute()`.
* Designs are reusable across different frames.
* Methods whose `...` is reserved (`print()`, `summary()`, `as_tbl_sample()`,
  `write_design()`) reject unexpected arguments rather than ignoring them.
  In the verbs whose `...` carries data (`execute()`, `stratify_by()`), a
  misspelled reserved argument is reported by name together with the
  argument it most likely meant.
* Where `...` is forwarded to another package (`as_svydesign()`,
  `as_svrepdesign()`, `as_survey_design()`), an argument is accepted only
  if samplyr or the receiving function owns its name. The arguments that
  follow the `...` are matched exactly, so `nes`, `methodd`, and `typ`
  used to be forwarded and silently ignored. Each is now reported with
  the name it was meant to be. Positional values are refused there, since
  they would be matched to whichever argument downstream happened to be
  free. Every argument the receiving function accepts still forwards,
  including ones it binds without evaluating, such as `fay.rho` and
  `fpctype`.
* On those same paths, the arguments samplyr computes from the sample and
  supplies itself are refused by name with
  `samplyr_error_derived_argument`, which carries the argument in its
  `argument` field. These are `ids`, `strata`, `weights`, `probs`, `fpc`,
  and `data` for `as_svydesign()`, plus `subset` and `id` on the two-phase
  path, and `design` for `as_svrepdesign()`. Passing one used to reach
  survey and fail there with "formal argument matched by multiple actual
  arguments", naming a call the user never wrote. They are reported
  before their values are evaluated, and separately from an unknown name,
  since the name is one survey owns. `pps` is unaffected and remains the
  documented route to exact joint inclusion probabilities.
* Arguments that accept a vector of stage numbers are called `stages`
  everywhere: `validate_frame()`, `joint_expectation()`, and
  `frame_summary()` all use `stages`, matching `execute()`. In each of
  them `...` precedes the optional arguments, so `stages` and everything
  after it is matched exactly rather than by prefix: the singular
  `stage` is reported together with the name it was meant to be, and a
  positional value is refused. Stray arguments are read by name without
  being evaluated, so one whose expression would fail is still
  diagnosed as the argument it was meant to be.
* `stages` also means the same thing in all four: a non-empty vector of
  distinct whole stage numbers drawn from what the verb allows, refused
  with `samplyr_error_stage_selector` otherwise. Duplicates such as
  `c(1, 1)` are refused rather than quietly collapsed, since asking for
  one stage twice is a mistake in whatever computed the vector. Each verb
  adds its own rules on top: `execute()` a start stage and contiguity,
  `joint_expectation()` the executed stages, `frame_summary()` the stages
  its digest records.
* `draw()`, `design_json()`, and `write_design()` take `...` before their
  optional arguments, like the verbs above. `draw(design, n, frac)` and
  `write_design(x, path, frame)` stay positional, because that is what a
  draw and a save are written. The fourteen modifiers after them must
  be named. `draw(design, 2, NULL, "srswor")` used to match `"srswor"` to
  `min_n` and report an unrelated bounds error.
* `design_effect()`, `effective_n()` and `varcomp()` on a `tbl_sample`
  name their contract instead of failing inside the value they were
  handed. `design_effect(x, y)` reads as "the design effect for `y`" and
  was reported as `object 'y' not found`. It now says the method takes no
  outcome, computes the weighting (Kish) design effect from `.weight`
  alone, and points at `survey::svymean(deff = TRUE)` for an
  outcome-specific one. Named arguments still forward, so
  `design_effect(x, icc = , n_per_psu = )` returns the weighting loss
  multiplied by the anticipated clustering component. `varcomp(x, y)`
  names the one-sided formula contract and shows the argument it could
  not evaluate.
* `stratify_by(region, allocation = "proportional")` suggests `alloc`.
  An expanded spelling is not a near miss by edit distance, so the
  suggestion falls back to the longest reserved name the argument starts
  with. This applies only where the value is already refused for not
  being a bare column name, so `stratify_by(region, cost_center = urban)`
  keeps working as the rename it is.
* `serp()` input errors carry stable `samplyr_error` subclasses.
* `execute()` takes its frames one per argument, or as a single unnamed
  list of data frames when the caller already holds them as a value. The
  two spellings are the same call. Mixing them is refused. Names inside
  the list are frame labels, so a list member that is not a data frame is
  reported by frame position and label
  (`samplyr_error_frame_not_data_frame`). Only names on the call's own
  arguments are read as possible misspelled arguments.
* Every verb that takes a frame reads it the same way. A data frame and a
  one-element list of data frames are the same input in `execute()`,
  `validate_frame()`, `joint_expectation()`, `design_json()`,
  `write_design()`, and `replay_design()`, and an empty list or a member
  that is not a data frame is refused by all of them with the same class,
  naming the frame by position and label.
* `validate_frame()` runs every check `execute()` runs before it samples,
  so it can no longer approve a frame execution refuses. It applies
  duplicate and reserved column names, a dropped `tbl_sample` class,
  parent identity, ancestry, and the per-stage variable set to a data
  frame exactly as to a list, and reports each with the class `execute()`
  reports. Where one frame cannot say whether it is the next stage's
  register or a hierarchy covering the rest, it requires `stages`, as
  `execute()` does. A partial sample previously defaulted to the next
  stage alone.
* A missing frame column is reported with the role its stage gives it, so
  a design selecting on `u` says `u` is the PRN variable rather than only
  that it is absent.
* `execute()` refuses duplicate input names, and input names reserved for
  its generated output (`.weight`, `.sample_id`, `.stage`, `.draw`,
  `.certainty`, and their per-stage forms such as `.weight_1` and
  `.fpc_1`), rather than overwriting user columns. Seed and replicate-seed
  ranges are validated before sampling begins.

## Sampling methods

* Added Sampford fixed-size PPS sampling (`method = "pps_sampford"`) with
  exact joint inclusion probabilities through `joint_expectation()`.
* Balanced sampling is an explicit method family: `method = "cube"` is the
  canonical cube path (`"balanced"` is an alias), while `"lpm2"` and
  `"scps"` provide spatially balanced draws through `spread = c(x, y)`.
* `bound()` markers inside the cube `aux` specification add hard
  adjacent-integer count constraints, for example
  `aux = c(income, bound(region), bound(urban))`.
* 16 methods across equal-probability, PPS, and balanced families:
  - Equal probability: `srswor`, `srswr`, `systematic`, `bernoulli`.
  - PPS without replacement: `pps_systematic`, `pps_brewer`, `pps_cps`
    (maximum entropy), `pps_sampford`, `pps_poisson`, `pps_sps`,
    `pps_pareto`.
  - PPS with replacement / PMR: `pps_multinomial`, `pps_chromy`.
  - Balanced: cube (`cube`), local pivotal (`lpm2`), and spatially
    correlated Poisson (`scps`).
* Permanent random numbers (PRN) for sample coordination:
  `bernoulli`, `pps_poisson`, `pps_sps`, `pps_pareto`.
* Random-size methods (`bernoulli`, `pps_poisson`) accept `n` (expected size)
  or `frac` (sampling fraction).
* Zero selections from a random-size method error by default
  (`on_empty = "error"`). `"warn"` and `"silent"` accept the empty
  realization, which contributes zero to Horvitz-Thompson totals and keeps
  estimates from repeated executions unbiased. Custom methods registered
  with `fixed_size = FALSE` honor `on_empty` the same way. A replicated
  execution with empty replicates cannot enter a later `execute()` call
  (class `samplyr_error_empty_phase_replicate`): silently skipping them
  would condition downstream results on nonempty realizations. Extracted
  single nonempty replicates remain executable.
* Custom methods registered via `sondage::register_method()` use family-aware
  names: WOR/WR methods use `pps_<name>`, while balanced methods use
  `balanced_<name>`. Method metadata (type, fixed size, PRN support) flows
  through validation, execution, joint probabilities, and survey export.
  Custom WR methods that declare PRN support receive the supplied PRNs
  during ordinary execution.
* Custom balanced methods (`type = "balanced"`) execute through
  `sondage::balanced_wor()`, may omit `mos` for equal probabilities, and may
  opt into `aux` or `spread` through the registry's `supports_aux` and
  `supports_spread` capabilities. They count toward the two-stage balanced
  limit and export with the built-in cube's variance treatment (Brewer,
  fraction-scale FPC) rather than falling through to SRS.
* `frac` validation follows a custom method's declared type, so custom WR
  methods accept `frac > 1` like the built-in WR methods.
* A `variance_family` declared at registration
  (`sondage::register_method(variance_family = )`) drives the survey
  export directly instead of inference from type and fixed size:
  `"poisson"` methods get exact `survey::poisson_sampling()`
  linearization without the explicit `pps =` escape, `"srs"` methods the
  equal-probability treatment, and `"unsupported"` methods refuse
  `as_svydesign()` while keeping the `subbootstrap` escape hatch. The
  declaration is serialized with the design receipt.

## Stratification and allocation

* Five allocation methods via `stratify_by(..., alloc =)`: proportional,
  equal, Neyman, optimal, and power.
* Custom allocation via named vectors or data frames.
* Minimum and maximum sample size constraints per stratum (`min_n`, `max_n`).
* Allocation methods preserve their requested total and their own criterion.
  A stratum too small to hold its share is capped at its population and the
  surplus is redistributed over the remaining strata in proportion to the
  method's factors, so a saturated Neyman design stays Neyman. The stratum
  population bounds the allocation whether or not `max_n` is supplied, and a
  named rule can therefore be departed from: `"equal"` on populations
  `(10, 490, 500)` with `n = 300` gives 10/145/145. Reported once per run
  with class `samplyr_message_allocation_capped`.
* A request above the frame size allocates every unit rather than failing,
  and is reported through the shared capping diagnostic described under
  Diagnostics. Bounds that make a request impossible raise
  `samplyr_error_alloc_min_infeasible` or
  `samplyr_error_alloc_max_infeasible` rather than adjusting silently.
* With-replacement and Poisson-multinomial designs are not bounded by the
  number of distinct units, so `n_h > N_h` is allowed and only `min_n` and
  `max_n` apply.
* Compound strata and allocation-table keys use collision-free matching, even
  when values contain punctuation or control characters.
* Simple stratified SRS uses a preallocated grouped draw path. It preserves the
  same seeded `sample.int()` selections while avoiding per-stratum result
  objects and repeated key lookups.

## Multi-stage and multi-phase

* Multi-stage sampling with `add_stage()`. Weights compound automatically
  across stages.
* A design may be executed against one shared hierarchy or against one
  frame per stage, and the number of frames is what schedules the stages:
  `execute(design, hierarchy)` runs every stage against one table, and
  `execute(design, schools, classes, students)` gives each stage its own
  register, mapped by position. Any count other than one or one per stage
  is `samplyr_error_frame_count`. A register is supplied whole and is
  restricted to the units its parent stage selected, and the variables
  earlier stages introduced are carried onto it. Nothing has to be
  pre-filtered or have upper-stage columns duplicated into it.
* The three spellings of a multi-stage execution are one implementation:
  one hierarchy, registers in one call, and a stage continuation
  (`execute(design, schools, stages = 1)` then
  `execute(stage1, classes, stages = 2)`) run the same stage transition.
  Under one shared RNG stream and with `stages` given on every
  intermediate call they draw the same sample, and that equivalence is a
  tested invariant rather than an incidental property.
* **Breaking, for code outside the package.** The internal helpers
  `subset_frame_to_sample()` and `find_compound_join_vars()` are removed.
  Both were unexported, so no documented interface changes, but any code
  reaching them through `:::` will fail. `link_stage_frame()` in
  `R/stage-link.R` is the single stage transition that replaced them, and
  it fixes what the old path got wrong: ancestry is no longer weakened to
  the variables both tables happen to share, so a lower register whose
  local identifiers repeat under different parents is now linked by full
  ancestry instead of being silently over-matched.
* Partial execution via `execute(..., stages = 1)` for operational workflows.
* Two-phase sampling by piping a `tbl_sample` into `execute()`.
* Earlier-phase weights carry through every stage when a multistage new
  phase is executed against separate frames in one call. The final weight is
  the product of the previous-phase weight and every conditional stage weight.
* `validate_frame()` takes the same frames `execute()` does: one data
  frame for a shared hierarchy, or an ordered list of stage registers.
  It runs every check execution runs before it draws, and judges each
  stage against the frame that stage will actually select from, after
  its register has been linked to its parents and carried their
  variables. A register that legitimately omits a stratum carried from
  an earlier stage passes; one whose copy of that stratum disagrees is
  `samplyr_error_frame_parent_conflict`, as it is at execution.
  Ancestry values that name no parent are judged on the register as
  supplied, since linking filters those rows out.
* Explicit validation is stricter than execution about candidate
  coverage: a unit reachable at one stage with no rows in the register
  the next stage samples from is
  `samplyr_error_frame_incomplete_register`, while `execute()` warns and
  fails only on a unit it actually selects. Candidacy is bounded by what
  has been selected, so a continuation is judged only on units the
  earlier call could reach.
* A partial `tbl_sample` validates the frames that would continue it,
  against the units that sample selected, through the same transition
  the continuation uses. The default scope is every remaining stage, as
  it is in `execute()`, and where one frame cannot say whether it is the
  next stage's register or a hierarchy covering the rest, `stages` is
  required in both. Neither compares the recorded fingerprint or frame
  digest, which describe the frame the executed stages drew from rather
  than the register the next stage needs. An ambiguous previous-phase
  identifier is refused here as it is at execution
  (`samplyr_error_phase_key_ambiguous`).
* `joint_expectation(sample, list(...))` reconstructs joint quantities
  from the original stage registers, linking each through the declared
  ancestry. A sample drawn from several registers refuses a single
  frame (`samplyr_error_frame_count`) rather than computing from the
  wrong population. The frame-free digest path remains the default and
  is preferred.
* The two-phase preflight in `validate_frame()` models the compound
  bridge `as_svydesign()` builds. The phases declare their sampling
  units independently, so the link is every identifier either phase
  declares that both samples carry, taken together: a phase 1 by PSU
  followed by a phase 2 by household and person links on all three. It
  warns only when nothing can link the phases, or when the identifiers
  together do not uniquely identify phase-1 rows.

## Certainty selection

* `.certainty_k` records a resolved inclusion probability of one, whatever
  produced it: an explicit threshold, capping inside
  `sondage::inclusion_prob()`, or a balanced design landing on one. Sample,
  digest, joint-inclusion matrix and survey export all decide it with one
  predicate and one tolerance, so a unit capped at one is treated exactly
  like one named by a rule and contributes no variance at its stage. Expected
  hits from WR/PMR methods are never certainty, even when at least one.
* Certainty units are placed in a take-all stratum wherever a stage exports
  under the PPS-WOR (Brewer) treatment, which includes balanced (cube) and
  custom balanced designs. Random-size designs keep the Poisson treatment and
  form no take-all stratum. Splitting certainty units out of a user stratum
  can leave a single probability unit behind, whose within-stratum variance
  is not estimable; `survey` reports that as a lonely PSU and
  `options(survey.lonely.psu = "adjust")` is the conservative response.
* PPS WOR methods support certainty selection via absolute (`certainty_size`)
  or proportional (`certainty_prop`) thresholds, including iterative
  identification for proportional thresholds.
* `certainty_overflow = "allow"` returns all certainty units when they
  exceed `n`.
* Stratum-specific thresholds via data frames.

## Panel partitioning

* `execute(..., panels = k)` assigns units to `k` panels by randomized fixed
  quota inside frozen ordered blocks. Each first-stage selection stratum is an
  assignment pool cut into consecutive blocks of `2k` units; every block
  carries a fixed quota per panel and its labels are permuted within the
  block. Each unit therefore carries each panel with probability `1/k`, and
  panel sizes within a pool differ by at most one.
* Blocking preserves the `control` order of `draw()`: units adjacent in that
  order share a block, so every panel inherits the same spread over it.
* Multi-stage designs assign panels at the first stage by default and
  propagate to all units. Under a with-replacement stage the assignment unit
  is the realized draw, so one population cluster drawn twice may carry two
  panels.
* `execute(..., panel_stage = s)` moves the assignment to another stage.
  Every later stage still inherits its ancestor's panel, so assigning at
  stage 1 rotates whole primary units while assigning lower down rotates
  units inside parents that stay in the survey: the address-panel design, in
  which selected areas are retained and households rotate within them. The
  stage must be one the execution completes, and it is accepted only
  alongside `panels`; `samplyr_error_panel_stage_value`,
  `samplyr_error_panel_stage_not_applicable` and
  `samplyr_error_panel_stage_unexecuted` report the three ways it can be
  wrong, all of them before any random number is drawn.
* An assignment pool is then the stage's own strata inside each realized
  parent occurrence, never crossing a parent, so pools are smaller than
  first-stage pools and `small_pool` binds more often. Selection certainty
  counts only at the assignment stage: a certainty primary unit does not make
  its households permanent.
* Assigning below the first stage is not a default. Holding the parent fixed
  while its members rotate can bias cross-sectional estimates over time, and a
  unit that cannot move between parents is balanced for net change but not for
  gross change.
* An assignment unit is identified by its realized ancestor occurrence, its
  own selection strata, its cluster variables, and its own draw index when the
  stage selects with replacement. A sample that has lost any of those columns
  is refused with `samplyr_error_panel_missing_identity` rather than assigned
  over a coarser identity.
* Certainty units are labelled from their own pools and consume no rotating
  quota.
* The frozen block sizes and the realized block-by-panel quotas are recorded
  with the sample and written by `write_design()`, since a subset of panels is
  a simple random subsample without replacement of the block quota rather than
  of `1/k`. The record is version 3, and states the assignment stage, the
  columns its pools are built from, and whether its units are clusters, draw
  occurrences or elements.
* Panels are assigned once. `.panel` is carried forward by a stage
  continuation, and redeclaring `panels` on a sample that already carries an
  assignment raises `samplyr_error_panels_already_assigned`. An assignment is
  the record and the `.panel` column together, so either one alone is enough
  for a sample to count as assigned, and a sample carrying one without the
  other is refused with `samplyr_error_panel_assignment_incomplete` whether or
  not new panels are supplied.
* Panel labels are rotation or workload groups, not an additional
  probability-sampling phase. Full-sample weights remain valid for
  the combined sample; multiplying one panel's weights by the number of panels
  is not generally valid for population inference.

## Rotation schedules and waves

* `panels` also accepts a rotation schedule: a data frame with `panel` and
  `wave` columns and an optional logical `active` column, where a combination
  left out is inactive. It declares the panel count and is stored with the
  sample. A schedule naming only its active rows is completed to the full
  panel-by-wave grid before it is recorded.
* A schedule sets the assignment block size. Where the leanest wave activates
  `r` of the `k` panels, blocks are `k * ceiling(2 / r)` rather than the
  scalar worst case `2k`, which keeps more of the assignment order while still
  leaving two units per block in the take.
* `execute(..., small_pool = )` governs what happens when a rotation schedule
  would leave a pool with no unit to activate. A pool of `m` assignment units
  leaves `panels - m` panels empty, so a wave activating `r` of them selects
  nothing from that pool when `m <= panels - r`; those units then have
  conditional inclusion probability zero in that wave rather than a small
  weight, so the wave's estimator is biased. The default refuses the
  assignment with `samplyr_error_panel_small_pool`, before any panel label is
  drawn, naming every affected pool. `small_pool = "permanent"` instead
  activates those pools at every wave with probability one, which is exact,
  and warns with `samplyr_warning_panel_small_pool` because it changes the
  operational design: wave sizes, overlap and repeated interviewing all
  increase. The policy governs positivity only; a pool with a positive but
  single active unit is still assigned and still carries no within-block
  variance estimate.
* The assignment record separates how a pool was selected from whether its
  units rotate. `class` remains the master's selection status, and a new
  `activation` field says `"rotating"` or `"permanent"`, with
  `permanent_reason` naming `"selection_certainty"` or `"small_pool"`. A
  promoted pool is permanent without being selection-certain, and
  `joint_expectation(waves = )` gains an `activation` column for the same
  reason. This separation is what took the record to version 2, and the
  assignment stage later took it to version 3. Versions 1 and 2 are read as
  what they were, and a version-1 record whose schedule strands a pool is
  refused when a wave is materialized rather than silently dropping it.
* `execute(master, wave = t)` materializes one precommitted occasion of a
  scheduled master. It activates the panels declared active at `t` and
  multiplies `.weight` by the inverse of the activation probability, which is
  the block's frozen quota for those panels over the block size. Permanent
  certainty units are activated at every wave with probability one.
* A materialized wave is a sample in its own right with its own integrity
  record, not a filtered master. `execute(master, wave = t)` takes no other
  execution input: a frame, `seed`, `stages`, `panels`, `small_pool` or
  `reps` alongside `wave` is an error, as is a master that is modified, incomplete, unscheduled
  or already materialized.
* The receipt records the wave, the active panels, the per-block take and
  activation probability, and a schedule digest.
* A materialized wave retains the master as its first phase, so
  `as_svydesign()` and srvyr export it through `survey::twophase()` with the
  activation as the second phase: the frozen blocks are the phase-2 strata
  and their sizes the phase-2 population counts. `method` is the only choice
  the export takes, and no method is ever selected as a silent fallback for
  another. `joint_expectation()` still refuses a materialized wave
  (`samplyr_error_wave_export_unsupported`) and points at the master, which
  answers the same question for any pair of declared waves:
  `joint_expectation(master, waves = c(t, s))`.
* `survey::twophase()` takes no `pps` specification at phase 1, so a wave of
  a master with unequal inclusion probabilities is refused
  (`samplyr_error_wave_phase1_pps`) rather than exported with the
  with-replacement approximation. Waves of equal-probability masters export,
  stratified, clustered, multistage and with-replacement alike. A method
  whose variance family is unsupported refuses at either phase
  (`samplyr_error_custom_random_wor_export`).
* A wave whose stored first phase is not the realization it was activated
  from is refused (`samplyr_error_wave_master_mismatch`), and one carrying no
  first phase at all is refused with an instruction to materialize it again
  (`samplyr_error_wave_no_master`). `.sample_id` is a row position rather
  than an identity, so the check is against the master's integrity record.
* `stack_waves()` verifies that two or more materialized waves come from one
  executed master and stacks them into a plain long table, one row per
  observed unit-wave, for handing to an inference layer that estimates change
  across waves. Row-binding waves by hand does not reproduce the checks: two
  executions of one design carry identical `.sample_id` values, so waves of
  different masters stack silently, and a wave edited after execution keeps a
  correct provenance record while its rows no longer match it.
* Four generated columns carry the contract. `master_id` is a master-local
  key, not a population identity and not generally a primary sampling unit: a
  clustered design's own cluster variable is carried through and is what a
  consumer's `PSU` argument wants. `design_weight` is samplyr's exact design
  weight including the activation factor, and is not a final weight, since
  nothing here is adjusted for nonresponse or calibrated. A data column of any
  of those four names is refused rather than silently renamed
  (`samplyr_error_stack_waves_columns`); `execute()` cannot catch them,
  because none is a reserved samplyr name.
* The result is an ordinary tibble, never a `tbl_sample`: it holds several
  realizations and repeats the unit key on purpose. samplyr's internal
  columns are dropped, stage-specific quantities among them, and waves
  carrying different analysis columns stack by column union rather than
  refusing.
* `joint_expectation(master, waves = c(t, s))` states how two occasions of a
  rotation overlap, exactly, from the quotas the master froze at its draw.
  Conditional on those quotas a block's panels are an arrangement of its
  labels, so a unit is active at both waves with probability `a_both / m` and
  two units of one block with probability `(a_t a_s - a_both) / {m (m - 1)}`,
  where the takes are read from the record rather than assumed equal. Units
  of different blocks are independent. The within-wave case is the same
  expression at `t == s`.
* The result is one row per block. Within a block the joints take only two
  values and across blocks they factorize as products of the marginals, so
  the table is a complete statement of the full matrix. It is the conditional
  covariance kernel that is block-diagonal, not the joint-probability matrix,
  whose cross-block entries are generally non-zero. A block of one unit has
  no distinct pair, so `has_pair` is `FALSE` there and its pairwise
  expectation is `NA` rather than zero. A permanent pool is certain at every
  wave and needs no special case in the arithmetic.
* Activation mode takes a master and a pair of declared waves, including a
  pair neither of which has been materialized. It uses nothing else, so
  `frame`, `stages` and `nsim` alongside `waves` are an error
  (`samplyr_error_joint_activation_arguments`) rather than arguments without
  effect, and a materialized wave is still refused, now pointing at the
  master call. These are joint expectations of the activation indicators,
  conditional on the phase-1 units and the frozen quotas; they are not the
  unconditional joint inclusion probabilities of the two-phase design.

## Rotation programs

* `execute(..., panels = plan)` now accepts an `svyplan_schedule` and freezes
  its startup activity at the draw. Immediate launches use the planned panel
  partition. A gradual launch has one whole startup cohort and does not gain a
  synthetic panel record. The plan-aware route refuses permanent small pools
  and selection-certainty activation because the plan describes rotating
  membership.
* `rotation_program()` accepts the same planning object with `through`, derives
  cohort entry occasions, translates the fielded prefix, and checks planned
  panel and issue counts against the executed receipts. The data-frame route
  keeps its existing `entry_wave` contract, and redundant entry input with a
  planning object is refused.
* `rotation_program()` links the cohorts of a rotating panel that
  replenishes. A start-up master drawn against one frame vintage and a
  refreshment cohort drawn against each later vintage are separate
  executions; the program records which samples make up the panel, when each
  entered, and which components are live at each occasion.
* A cohort may be *partitioned*, drawn with `panels` so that it carries
  `.panel` and a frozen assignment, or *whole*, drawn without them. A whole
  cohort has one implicit panel covering all its rows, and activating it is
  not a subsample, so its weights are unchanged.
* `entry_wave` is declared, never inferred from the schedule. A cohort may be
  drawn early and held in reserve, so its first active wave need not be its
  entry, and entry is what anchors which population vintage a cohort
  represents.
* The program schedule adds a `cohort` column to the panel-by-wave grid; for
  a one-cohort program it may be omitted. It is completed to the full grid
  only once the registry is known, because a cohort's available panels come
  from its own receipt. Activity need not be contiguous, since a 4-8-4
  rotation deliberately leaves and re-enters.
* A schedule that activates fewer panels of a cohort than the `r_min` its
  block size was frozen for is refused at construction with
  `samplyr_error_program_block_size`. Such an activation still has computable
  weights but leaves under two units per block, so it carries no within-block
  variance estimate, and the schedule is where the mistake was made.
* `execute(program, wave = t)` returns a collection of samples with separate
  receipts, one per live cohort, each carrying its own exact activation
  factor. `as.data.frame()` on the collection row-binds it with a `.cohort`
  column for fieldwork, and returns a plain data frame, which is what stops it
  reaching `as_svydesign()` as though it were one sample.
* Weights are valid within a cohort and are not combined across cohorts. A
  program records when its cohorts are live, not how they relate: whether a
  unit could have been drawn into more than one depends on its population
  membership at each vintage, the chance it would have had in a draw it was
  not selected into, and how the draws depend on one another. The last is not
  recoverable from inclusion probabilities at all, since two executions with
  identical marginals may be independent or identical. Overlap is therefore
  unknown rather than assumed absent. A refreshment cohort drawn from an
  entrant register is the case where the analyst knows it is absent, and there
  the component weights are already the right ones.
* Schedule defects now carry one condition class per defect kind rather than
  one per entry point, so a gap, a duplicate row or a non-logical `active`
  column is reported as `samplyr_error_schedule_gap`,
  `samplyr_error_schedule_duplicates` or `samplyr_error_schedule_active`
  whether it reached samplyr through `panels` or through
  `rotation_program()`. The message names whichever argument carried it.

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

## Serialization

* `write_design()` and `read_design()` save and restore designs as
  versioned, human-readable, samplyr-native JSON files. A restored design
  executes identically to the original. `design_json()` renders the same
  format as an in-memory string for databases and APIs. This interface and
  file format are experimental and are not a finalized cross-tool standard.
* Design files record the frame variables each stage requires, and
  optionally a frame fingerprint (name, dimensions, column types,
  content hash) via `write_design(..., frame =)` -- the frame data
  itself is never written. The content hash covers column names, column
  values, and row order; a tibble and a plain data frame holding the
  same data fingerprint identically, and column order does not matter.
* Saving an executed `tbl_sample` records an execution receipt with
  every `execute()` argument that affects the result (seed, executed
  stages, `panels`, `reps`, replicate seeds), the execution-time RNG
  configuration and package versions, plus the number of selected units and
  the timestamp. `replay_design()` restores the recorded RNG configuration
  and re-runs the call, reproducing the full sample -- including `.panel` and
  `.replicate` assignments -- with only the timestamp differing when the
  frame and implementations match. Frame mismatches error by default;
  implementation-version mismatches warn.
  Samples built by several `execute()` calls (continuation,
  multi-phase) or modified after execution are flagged in the receipt
  and warned about at write time; `replay_design()` refuses chained
  receipts rather than replaying only the final call.
* Receipts record how frames were mapped to stages: the frame mode, how
  many frames were supplied, their optional labels, and the frame
  position each executed stage drew from. `write_design(..., frame =)`
  accepts the same ordered list `execute()` takes and fingerprints each
  frame separately, and `replay_design(x, list(...))` replays a
  one-call multi-register execution, reporting a mismatch by frame
  position and label. Supplying the wrong number of frames is
  `samplyr_error_replay_frame_count`. Chained and multi-phase receipts
  are refused: the mapping describes the recorded call only.
  Files carrying these fields declare format version 2, since a version
  1 reader would replay them against a single frame; every other file
  is still written at version 1, and a receipt without the fields is
  read as the one-frame call it can only have been. The plural fields
  and version 2 describe genuinely several supplied frames, not the
  container the caller wrote: one frame passed as `list(frame)` records
  the singular fingerprint and version 1, identically to `frame`.
* Serialization refuses a frame count the design could not have been
  executed with, and, for an executed sample, any count other than the
  one its receipt records
  (`samplyr_error_serialization_frame_count`, which inherits
  `samplyr_error_frame_count`). A file whose fingerprint manifest
  contradicts its own receipt can no longer be written. The independent
  check in `replay_design()` remains, for files written elsewhere.
* A panel assignment record is read under the version it states, and in that
  order: the algorithm and version first, then what that version leaves
  implicit, then whether the record carries what that version requires. A
  record naming an algorithm or a version this build does not know is refused
  with `samplyr_error_panel_record_unsupported` rather than taken apart under
  a law it was not written under.
* Every path that reads a record reads it that way, and so does every path
  that writes one. `replay_design()` reads it before decoding any panel
  argument; `execute(master, wave = )` and `joint_expectation(waves = )` read
  it before asking whether it declares any waves, so an unreadable record is
  no longer reported as one that declares none; `as_svydesign()` reads it
  before building the phase-2 units, blocks and probabilities out of its
  quotas; `stack_waves()` and `rotation_program()` read it before taking a
  panel count, an `r_min` or a block size from it, so a program is refused
  where it is declared rather than at its first wave. `write_design()`,
  `design_json()` and `replay_design()` read it before writing a receipt,
  because a writer that fills in a field the record does not carry produces a
  well-formed file describing an assignment nothing recorded. Each refusal
  names the call that was made.
* A version-3 record must state `assignment_stage` as one whole stage number
  of 1 or more, name `unit` as one of `cluster`, `occurrence` or `element`,
  and list `key_vars` and `pool_vars` as distinct column names. A record
  numbered 3 that does not is refused with
  `samplyr_error_panel_record_malformed` rather than filled in, since version
  3 is the version where those are written down and inventing one would state
  an assignment the file does not record. An empty `pool_vars` is not a
  missing one: it is the single pool of an unstratified first-stage
  assignment. Versions 1 and 2 mean the first stage because it is the only
  stage they could assign, so an `assignment_stage` on a record numbered 1 or
  2 is ignored rather than read as a lower-stage assignment.
* A file carrying a bare value where the assignment record belongs is refused
  with `samplyr_error_panel_record_malformed`, naming what it carries. A JSON
  scalar parses to a length-one vector rather than to a set of named fields,
  and reading a field off one previously surfaced base R's complaint about the
  `$` operator instead of a statement about the artifact.
* Receipts for designs using registered custom methods record an
  implementation fingerprint (formals and body of the registered
  `sample_fn` and `joint_fn`, via `sondage::method_spec()`). Replay
  refuses a re-registered function whose code differs from the
  recorded one: matching registry metadata alone does not imply the
  same selections. The fingerprint does not cover the function's
  enclosing environment.
* The design format separates declarative design metadata from native
  implementation metadata. Selection methods use samplyr's internal semantic
  descriptor, while exact method names, R classes, execution environment, and
  the R-native frame hash live under `tools.samplyr`. These descriptors are
  not presented as a finalized external method vocabulary.
* Control ordering uses a declarative JSON grammar (`ascending`,
  `descending`, and `serpentine`, with explicit variable arrays) rather than
  embedded R expressions. `read_design()` accepts local file paths and JSON
  strings only; URLs are refused, so reading a design never touches the
  network.
* `validate_frame()` compares a restored design's stored fingerprint
  against the supplied frame and reports what changed (rows, columns,
  column types, or content). The comparison is informational and never
  fails validation; control it with the `fingerprint` argument
  (`"inform"`, `"warn"`, or `"ignore"`). One frame and several are
  compared by the same code, and a count that cannot match is itself
  reported: a file recording three registers says nothing about one
  frame, and `replay_design()` reports that rather than silently
  skipping the comparison.
* A sample carrying shared weights from `share_weights()` is written as
  `samplyr/shared-sample`, a format of its own. It records the source
  selection and the transformation's arguments and nothing else: the links
  and the target register are supplied again to `replay_design(links =,
  targets =)`, the way a frame is, so no unit-level data and no linkage is
  ever written. This is possible because the transformation record already
  carries its own call declaratively. `read_design()` returns a
  `shared_sample_design`; `replay_design()` re-executes the source selection
  against the register, re-applies the transformation to the supplied tables,
  and reproduces the sample exactly. A live shared sample replays too.
* Replaying one is checked against two integrity records the file carries, so
  a mismatch says which input was wrong: a source that does not reproduce
  means `frame` is not the register selected from, and a result that does not
  means `links` or `targets` is not the table the transformation was built
  from. `links` and `targets` are refused where nothing uses them, since
  accepting them would return an untransformed sample to someone who believes
  a transformation had been re-applied.
* A `frame_stack()` collection is written as `samplyr/frame-stack`, a format
  of its own rather than an optional block in a design file, so a reader
  that does not know collections refuses one outright instead of taking its
  first component for the whole thing. Each component entry is a complete
  `samplyr/design` document plus its name and its membership column, which
  is what lets the component encoder and decoder be the design ones
  unchanged. The key and any overlaps declared with
  `overlap_probabilities()` or `overlap_weights()` travel with it. `frame`
  is a list named by component, matched by name rather than position.
  `read_design()` returns a `frame_stack_design` carrying the components'
  designs and receipts; `replay_design()` executes each against its register
  and stacks the results, reproducing the collection exactly. It also takes
  a live `frame_stack`.
* Two things a collection cannot carry are refused rather than dropped, since
  either loss would return a collection that looks complete and estimates
  differently: overlaps from `exante_overlaps()`, which resolve to one chance
  per selected unit and whose originating request is not kept, and a
  component carrying shared weights, which a collection has nowhere to put a
  link table for.

## Indirect sampling

* `share_weights()` applies the generalized weight share method (Lavallee
  2007): a sample of one population becomes a weighted sample of a second
  population linked to it, so children can be estimated through their
  parents, establishments through their enterprises, or persons through
  their dwellings. The rows of the result are units of the target
  population. Nothing about selection changes: the method needs inclusion
  probabilities only for the units actually selected, which is what
  `.weight` already holds, so this is a transformation of an executed
  sample rather than a sixth verb.
* The denominator has no default and is where the statistical content
  sits. It is the number of links summed over the whole source
  population, not over the sample, so counting a supplied link table
  asserts that the table is a complete register. That assertion is the
  classic source of bias in the method and has to be made at the call
  site: either name a column of the target register holding the
  population multiplicity, or state it with `complete_links()`.
* `weighted_links()` replaces the 0/1 link indicator with a non-negative
  importance, which Lavallee section 4.5 shows costs no theory as long as
  each target cluster totals more than zero. Counting is the case where
  every link counts for one, and the two paths are one implementation: a
  `weighted_links()` call with every importance set to one reproduces
  `complete_links()` exactly. `complete_weighted_links()` is the
  corresponding assertion about the importance total.
* The target cluster is never inferred. Give a bare column for the
  ordinary clustered method, `NULL` to make every target unit its own
  cluster, or `extend_links()` to eliminate clusters by extending the
  links across them. The three produce three different weights, which is
  why guessing is not an option.
* A shared weight is an estimation weight, not a design weight, and the
  result records which it carries. Every statistical consumer states what
  it does about that contract rather than treating the sample as
  ordinary: `design_effect()` and `effective_n()` read `.weight` and are
  supported; the data-manipulation methods preserve the record;
  `as_svydesign()`, `joint_expectation()`, `varcomp()`, a further
  `execute()` and the panel verbs each refuse with their own condition
  class, all of them also catchable as
  `samplyr_error_weight_contract`. Neither `as_tbl_sample()` nor an
  ordinary dplyr reconstruction can launder an estimation weight back
  into a design-weight sample.
* `as_svrepdesign()` is one variance route, and the ordering is what
  makes it correct: the recorded operator is applied **inside every
  replicate**, so the replication happens on the source design and the
  target rows are never resampled. Sharing after replication is a
  different and wrong number.
* `as_svydesign()` is the other, and it exports the **source-target
  contributions**: one row per link, weighted by the recorded coefficient
  times the source unit's design weight. The generalized weight share
  total is the Horvitz-Thompson total of a variable derived on the source
  units, and expanding the contributions is what lets survey form that
  variable inside each sampling unit for whatever is being analyzed. It is
  exact for any link structure, with no condition on how many source units
  reach a target. The rows of the result are contributions rather than
  target units, which changes no estimate: a total sums the same terms and
  a mean's denominator is the estimated size of the target population
  either way.
* An unequal-probability or random-size source design is refused on the
  linearized route, because both take their variance from a structure
  indexed by the rows of the source sample and those rows stop being the
  sampled units once each appears per contribution. A stack of frames is
  refused there too: `survey::multiframe()` reads one selection
  probability per row, and a contribution is not one.
* A target cluster with no link to the source population can never be
  reached, and the estimator understates totals by exactly its share.
  That is Lavallee's Constraint 2.1 and it is detectable only when the
  target register claims to enumerate the population. A target unit
  inside a reached cluster with no link of its own is a different thing
  and is correct: it receives its cluster's weight, and producing a
  weight for exactly those units is one of the reasons to use the method.
* The finding is recorded at the transformation and warned about where
  the estimate is formed, with `samplyr_warning_unlinked_cluster` at
  `as_svrepdesign()`. That is a deliberate departure from the convention
  that a condition fires where it arises, and the reason is the composed
  case: `share_weights()` cannot know whether its result will stand
  alone or become one component of a design over several frames, where a
  cluster one frame cannot reach is the entire reason the second frame
  exists. A warning there would be wrong by construction.
* For a `stack_frames()` collection the finding is evaluated over the
  union and fires once, naming only the clusters no frame reaches. The
  components' own findings are muffled. A component's silence about a
  cluster counts as coverage only when it was describing the same target
  clusters, which the record establishes with a fingerprint of the
  cluster set rather than assuming; where it cannot, the union is
  reported as not established rather than guessed. `summary()` states
  whichever of those the collection has.

## Overlapping frames

* `stack_frames()` collects samples selected independently from two or
  more frames that overlap on one target population, such as a landline
  frame and a cell frame, or an area frame and a list frame. The result
  is a `frame_stack`: a collection that keeps its components and their
  receipts apart, not a row-bound table. Row-binding them would
  double-count every unit listed in two frames.
* Membership is declared as a mapping from frame name to column, and
  every component must carry every frame's column, because a unit
  selected from frame A has to say whether it was also listed in frame B.
  Membership columns must be logical: integer `0` and `1` are refused
  rather than read, since numeric overlap information is a different
  input on a different scale.
* `as.data.frame()` gives the row-bound inspection view with `.frame`,
  the component a row came from, and `.domain`, the set of frames the
  unit belongs to. Domain labels join the frame names in C-locale byte
  order, so they do not depend on the order the frames were stacked or on
  the session collation. Statistical code reads the membership columns
  and never parses the label back.
* Two components carrying the same recorded seed warn. Distinct seeds are
  not evidence that the samples were selected independently; the warning
  detects one recorded common-random-number mistake and nothing more. No
  other package is positioned to catch it, because no other package
  records the seeds.
* `as_svydesign()` exports a stack through `survey::multiframe()`. Each
  component is exported on its own, so it keeps its strata, clusters and
  variance treatment, and the compositing sits on top. `theta = NULL` is
  the multiplicity estimator, resolved by samplyr and passed on as a
  number: survey reads a missing `theta` as the ratio of the frames' mean
  sampling weights, which is a data-dependent heuristic rather than a
  neutral default. An explicit `theta` is Hartley's constant factor and
  belongs to the first frame of the stack, so reversing the components
  and wanting the same estimator means `1 - theta`.
* `as_svrepdesign()` builds one replicate system per frame and combines
  them in blocks: in a replicate column belonging to one frame, only that
  frame varies and every other stays at its full-sample weight. So the
  combined variance is the sum of the frames' own contributions, which is
  what independent selection from each frame gives. Unlike the linearized
  route it takes any number of frames, takes a component whose weights
  were shared from another population, and lets each frame use the
  replicate method that suits its own design.
* The compositing factor is chosen at export rather than stored on the
  stack, because which one is appropriate depends on the estimand and on
  the design effects. `stack_frames()` also does not assert that the
  frames together cover the target population: that is an assumption
  about the registers, not something the call can establish, which is why
  the verb is not named after their union.
* `exante_probabilities()` gives the probability a design would assign each
  unit of a register, computed from the design rather than observed from a
  sample, drawing no random numbers. It compounds across stages, so a
  two-stage probability is the cluster's chance times the chance within it,
  and it uses the same allocation and chance resolvers execution does: a
  sample's own weights reproduce these numbers exactly. It refuses a
  with-replacement stage, where the quantity is an expected number of hits
  rather than an inclusion probability, and a design given one register per
  stage, since the compounding runs along the rows of one register.
* `exante_overlaps()` applies that to a whole stack: instead of naming
  columns that already hold each unit's chance in every frame, it names the
  registers and samplyr resolves the chances from each component's own
  design. This is what the expected estimator needs and what a sample alone
  cannot supply, the chance a unit would have had in a frame it was not
  selected from. The frame digest cannot serve: it is a reporting and privacy
  structure that may store a varying chance vector as quantile bins, keyed by
  an internal unit id, so `frame_summary(detail = "unit")` returns no rows at
  all for an unequal-probability element design.
* Whether the chances are stated or resolved, the value samplyr can check
  against reality is the one a frame gives its own units, and it does: a
  frame's own chance must equal the design weight the execution produced. A
  register that is not the population a design drew from is caught there.
* Limits stated rather than discovered downstream: `survey::multiframe()`
  composites two frames, so a larger stack is refused at the linearized
  export and not at the design layer, exactly as `execute()` chains any
  number of phases while the two-phase export stops at two. Above two
  frames an explicit `theta` is refused as well, because the compositing
  factors are then per domain rather than per frame and one number is not
  a partial statement of that.

## Survey export

* `as_svydesign()` converts `tbl_sample` to `survey::svydesign()` with
  correct strata, cluster IDs, weights, and finite population corrections.
  Handles PPS WOR (Brewer approximation or exact `ppsmat`), WR/PMR
  (`Inf` FPC, Hansen-Hurwitz), certainty strata, balanced sampling, and
  two-phase designs.
* Exact multi-stage linearization: every executed stage is exported with
  one `ids`, one `fpc`, and (when stratified) one `strata` term. A final
  stage without `cluster_by()` gets a synthesized element identifier.
  Multi-variable `cluster_by()` and `stratify_by()` export as a single
  collision-free integer term per stage, and certainty strata combine with
  user strata. Formula construction supports non-syntactic column names.
  Multi-stage PPS designs use fraction-scale FPCs throughout.
* A two-phase export now represents every stage of its first phase. A final
  unclustered stage gets a synthesized identifier there, as it already did in
  single-phase export, so its variance contribution is present rather than
  absent from a design still described as `method = "full"`. Phase 2 is
  unchanged: its stages describe the subsampling `survey::twophase()` links
  across the phases itself.
* Relatedly, a two-phase export derives each phase's inclusion probabilities
  from its finite population correction only where the correction states all
  of them. It does not when a stage contributed no identifier term, nor when
  a term is infinite, which is what a with-replacement stage has instead of a
  population count. Those phases now pass their own weights, which are exact.
  Previously a first phase of clusters and then elements exported with only
  its cluster stage's probability, halving the weights and any total
  estimated from them.
* `as_svydesign()` and `as_svrepdesign()` gain `systematic_variance`, and say
  once per call when they are approximating the variance of an
  equal-probability `systematic` stage. One systematic sample generally does
  not identify its own design variance, so the approximation is supplied as
  before and neither export is refused; what changes is that a returned
  design no longer carries an approximate variance model with nothing to show
  for it. `"warn"` is the default and names every affected stage in one
  condition of class `samplyr_warning_systematic_variance`; `"approximate"`
  accepts the approximation silently; `"error"` refuses it. Census stages are
  exempt, `pps_systematic` is unaffected, a systematic stage in a first phase
  is named as such, and the choice is recorded on the returned object in the
  `"samplyr_systematic_variance"` attribute.
* The two exports approximate such a stage by different means, and each
  condition names the one in use. `as_svydesign()` substitutes the simple
  random sampling variance estimator. `as_svrepdesign()` builds generic
  replicate weights, which resample the realized sample rather than redraw a
  random start against the frame in its original order, so no replicate type
  reproduces the selection mechanism. Requesting a particular `type` is
  therefore not an acknowledgement and does not silence the condition; only
  `systematic_variance` does. The measured margins below are the
  linearization export's and are not claimed for the replicate one.
* The documented approximation for equal-probability `systematic` stages now
  states its size. The SRSWOR variance estimator is the standard treatment and
  its direction was already recorded, smaller under a favourable ordering and
  larger under a periodic one, but not its magnitude: measured over repeated
  draws at an interval of 20, the reported variance is 0.73 of the truth on a
  randomly ordered frame with 90.2% interval coverage, 1.76 under a trend, and
  0.0006 where the frame's period matches the sampling interval, where 95%
  intervals cover 9.8% of the time. A systematic design with interval `k` has
  only `k` distinct samples per stratum, so its variance for one frame need
  not sit near the SRSWOR value even when the ordering is unstructured.
* `as_svrepdesign()` converts to replicate-weight designs. For PPS and
  balanced designs, `"subbootstrap"` and `"mrbbootstrap"` are supported.
* `nest` reaches `survey::svydesign()` only, so giving it when exporting a
  two-phase sample warns (`samplyr_warning_nest_ignored`) rather than
  appearing to take effect.
* `as_survey_design()` and `as_survey_rep()` for direct conversion to
  srvyr `tbl_svy` objects.
* `joint_expectation()` computes pairwise joint inclusion probabilities
  (WOR) or joint expected hits (WR/PMR) for exact variance estimation.
  Later stages are computed conditionally within each parent cluster,
  with cross-parent pairs at the product of marginals (independent
  selections). The frame argument is optional: without it, the
  computation runs off the frame digest recorded at execution, so a
  sample that traveled without its frame still yields exact joint
  expectations. This needs an exact chance representation (the default
  digest for cluster and constant-chance stages;
  `frame_digest = "full"` for element stages with varying chances);
  summarized chances refuse rather than approximate.
  WR matrices contain distinct population units in first-appearance
  order. Repeated WR parent occurrences define separate independent child
  blocks, and stratified blocks also follow first sample appearance. Frame
  and digest computations therefore share dimensions, order, and values for
  repeated-hit designs.
* Custom methods registered with `fixed_size = FALSE` are random-size;
  `as_svydesign()` errors rather than applying Brewer's fixed-size
  approximation (which could report near-zero variance). Pass
  `pps = survey::poisson_sampling(1 / x$.weight)` for Poisson-type
  methods, or use `as_svrepdesign(type = "subbootstrap")`.

## Sample integrity

* Modified-sample guard: a `tbl_sample` whose rows were removed, added,
  or duplicated after `execute()`, or whose internal design columns
  (`.weight`, `.fpc_k`, ...) were overwritten, dropped, or renamed
  (including via `select()` and column `[`), is marked as modified and
  rejected by `as_svydesign()`, `as_svrepdesign()`, `joint_expectation()`,
  `design_effect()`, and `effective_n()`. Physically filtering rows
  before export silently understated domain variance. For subpopulation
  estimates, convert first and use `survey::subset()` or srvyr's
  `filter()` on the design object. Extracting one complete replicate
  with `filter(.replicate == r)` is verified against execution metadata
  and remains supported. Two-phase export warns when the phase-1 sample
  was modified before phase-2 execution, since `survey::twophase()`
  treats the current phase-1 rows as the complete phase-1 sample.
* Class-stripped sample guard: a plain data frame that retains sampling
  provenance or the full bundle of generated execution columns is rejected as
  the frame of a fresh design execution. This prevents operations such as
  `tidyr::uncount()` from silently dropping `tbl_sample`, rerunning stage 1 on
  an expanded listing, and producing weights for the listing rather than the
  population. The same object remains valid as a later-stage listing frame
  when the clean partial sample is used as the continuation input.
* Passing an intact strict-prefix result back as the frame of its own design
  warns even when the sample is pristine. The diagnostic explains that
  this starts a new phase and restarts the design at stage 1, points to the
  stage-continuation
  form, and notes that an intentional new phase remains supported through
  `survey::twophase()`.
* Integrity record: `execute()` stores the row count and an
  order-invariant hash of the protected columns (weights, design
  metadata, and the executed stages' strata/cluster variables). The
  analysis boundary verifies it authoritatively, so modifications
  through routes the dplyr hooks cannot see (base assignment,
  `rbind()`, `vctrs::vec_rbind()`, stripping and restoring the class,
  changing strata or cluster values) are caught, and value-identical
  overwrites pass. `as_tbl_sample()` re-verifies on restore.
* `tbl_sample` is a fuller tibble subclass: `group_by()`/`ungroup()`
  preserve sample provenance (grouped verbs work and marks flow
  through), `vec_restore()` applies the same rules as the dplyr hooks,
  and base `[` detects same-length row duplication.

## Survey planning

* Samplyr re-exports the `design_effect()`, `effective_n()`, and `varcomp()`
  generics from svyplan and registers `tbl_sample` methods on those exact
  generics rather than defining competing ones.
* `design_effect()` and `effective_n()` on a `tbl_sample` report the
  weighting loss (Kish's design effect) from the `.weight` column: 1 for a
  self-weighting design, rising with weight variability. Being
  outcome-independent is what makes the number available from the sample
  alone, and it is one component of a full design effect rather than a
  substitute for one. They follow svyplan's numeric `svyplan_deff` contract,
  so use `as.double()` for the value, and `summary()` labels the token
  `Kish DEFF` for the same reason. The weighting loss is the only
  design-effect strategy samplyr offers: svyplan's `design_effect()` is
  planning-only, and samplyr does not carry estimators its upstream does not
  define. For a design effect that reflects clustering and stratification,
  hand the design to survey with `as_svydesign()` and use
  `svymean(deff = TRUE)`; to anticipate the clustering component before
  collection, use `svyplan::design_effect()` with `icc` and `n_per_psu`.
* `varcomp()` has a `tbl_sample` method: design-based variance
  components (B, W, icc, var_ratio) estimated from an executed clustered
  sample, feeding `svyplan::n_cluster()` for next-round planning. It
  applies the two conventions that are easy to get wrong by hand:
  within-PSU weights (the product of the per-stage weights below
  stage 1, never the compound `.weight`) and stage-1 selection shares
  derived from the stage-1 weights, normalized over the sampled PSUs
  (per stratum when stratified). Handles 2- and 3-stage designs with
  SRS, PPS (WOR and WR), and stratified first stages; refuses
  two-phase samples, certainty PSUs, and deeper designs with precise
  messages. The certainty guard reads `.certainty_k`, which covers implicit
  probability capping as well as explicit thresholds. `strata` follows the
  `...` that carries the outcome formula, so it is matched exactly: `strat
  = ~region`, or a second positional formula, used to be dropped and return
  an unstratified decomposition. Both are now refused by name.
* `draw()` accepts `svyplan` sample size objects (`svyplan_n`, `svyplan_power`,
  `svyplan_cluster`) directly, and the handoff is stage-aware: cluster
  plans contribute the PSU count at a clustered stage 1 and the
  per-cluster take at later stages; stratified two-stage `n_alloc()`
  plans feed both stages by stratum; `n_multi()` domain plans become
  per-domain tables for domain-stratified designs. Consumption goes
  through svyplan's documented coercions (`as.data.frame()`,
  `as.integer()`), so draws use the plan's integerized field design
  rather than per-stage rounding.
* A named `n` without stage-level stratification, or with crossed
  stratification variables, fails at design time with guidance rather than
  at execution.
* Precision analysis (`prec_prop()`, `prec_mean()`, `prec_cluster()`,
  `prec_multi()`), sensitivity analysis (`predict()`), response rate
  adjustment (`resp_rate`), and confidence intervals (`confint()`) on
  all planning objects.

## Frame digest

* Every `execute()` records a frame digest by default
  (`frame_digest = "summary"`; `"full"` keeps exact unit chances,
  `"none"` disables it): a versioned manifest of the selection pools,
  first-order chances, and selected units the execution resolved, with
  no unit identifiers. Recording is observational and never changes the
  selection; its size scales with pools, clusters, and quantile bins
  rather than frame rows (clusters that are single frame rows make the
  two coincide; `frame_digest = "none"` opts out).
* `frame_digest = "none"` skips selection-trace construction entirely.
  Summary traces store constant chances once per pool while retaining full
  vectors for cluster and balanced diagnostics that require unit-level values.
* The digest keeps a `tbl_sample` intelligible without its frame: the
  printed header shows population coverage (`360/19,344 units`),
  `summary()` reports one realization line per stage, and
  `frame_summary()` returns the full record as documented tibbles
  with stage, pool, or unit detail (pools at parent-by-strata
  resolution) and eligible or universe scope.
* When one universe frame feeds every stage, pools under unselected
  parents are resolved from the design alone (`design_resolved`), so
  coverage is reported against the full universe.
* Registered methods declare their first-order probability tier with
  `sondage::register_method(probabilities =)`:
  `"exact"` (the design's true first-order inclusion probabilities,
  or expected hits, equal the `pik` handed to the method),
  `"approximate"` (honored to
  a documented approximation, as Pareto and sequential Poisson order
  sampling are), or `"unknown"` (the default: `pik` is a selection
  weight only). `draw()` refuses `"unknown"` methods, because their
  `1 / pik` design weights would be systematically biased, and
  execution re-checks designs restored from files. The strict
  default makes the classic `sample(prob = pik)` trap (exact for
  with-replacement expected hits, biased without replacement)
  impossible to hit silently.
* The probability tier is recorded for built-in methods too:
  `"pps_sps"` and
  `"pps_pareto"` honor the target `pik` only to a documented
  approximation, so their stages carry `probabilities = "approximate"`
  in the frame digest, the `probabilities` column of
  `frame_summary()`, the serialized design metadata, and a
  `(approximate probabilities)` flag in `summary()`. For these
  methods `.weight` is the inverse target probability, not the
  inverse of the design's true first-order inclusion probability;
  the `execute()` weight documentation says so.
* `frame_summary(design, frame)` previews a design before it is run.
  Every selection pool is enumerated and every chance resolved from the
  design and the frame, but nothing is selected: no random numbers are
  drawn and `.Random.seed` is left as it was, whether or not it existed.
  The frame is read with the same grammar `execute()` uses, so one
  shared hierarchy or one register per stage both work. Supplying
  `frame` always means "preview", so a design restored from a file can
  be checked against next wave's frame, and a sample can be previewed
  against a frame other than the one it was drawn from; omitting it
  reports what the recorded digest says happened.
* A recorded frame that no stage selects from is refused when a digest
  claims to be complete (`samplyr_error_digest_frame_ref`). Checking that
  each stage's frame reference is in range cannot catch a digest whose
  stages all point at the first of several frames; requiring every frame to
  be claimed does. A partial digest is exempt: a replicated multi-stage
  execution keeps the stage prefix common to every replicate and still
  records the frames the dropped stages used.
* A preview applies the same checks `execute()` applies before it
  samples, so it cannot approve a frame execution would refuse:
  duplicate column names, reserved generated names, and a `tbl_sample`
  whose class was dropped are all reported with the class `execute()`
  reports. Each stage also records which supplied frame it would select
  from, as an executed digest does, and the record it points at describes
  that register as supplied: a stage selects from its register linked to
  its parents, which carries their columns, but the fingerprint and roles
  are the register's own. A preview and an execution of the same registers
  therefore produce the same frame records.
* A preview has no realization, so `n_realized` and `take_rate` come
  back `NA`, as do `is_selected` and `n_hits` at `detail = "unit"`.
  Every other column and the shape of the table are the same as for a
  recorded digest, so the two are directly comparable.
* In a preview, `detail = "pool"` reports one pool per candidate parent
  with the take that parent would give **if selected**, which is what
  field planning needs. `detail = "stage"` rolls those up weighted by
  the probability each parent is selected, so it reports the expected
  size of the stage: a design taking 10 of 100 clusters and 5 units in
  each reports 50 at stage 2, not the 500 summed over every candidate.
  Where per-parent takes vary, the realized size varies around it.
* A stage below a with-replacement stage cannot be previewed, because
  the number of times each parent is hit is random
  (`samplyr_error_exante_unsupported`).
* `capped` is derived with a tolerance rather than an exact comparison.
  `n_expected` and `n_target` are equal by construction when nothing
  capped, but they are computed by different paths at execution and in
  the preview, and a difference in the last bits made one Neyman
  stratum report as capped in one and not the other.
* The digest travels inside execution receipts, so a design restored
  with `read_design()` carries it, and `frame_summary()` accepts such
  designs directly: a shipped design file supports next-wave planning
  (population counts, realized allocations) without the frame or the
  sample. The survey-planning vignette shows the wave loop into
  `svyplan::n_alloc()`. `validate_frame()` compares a
  candidate frame against the recorded digest and reports structural
  drift (per-pool recounts of population sizes) and chance drift: the
  chances the design would resolve on the new frame are compared with
  the recorded ones, so a size measure rescaled by a constant factor
  reports unchanged chances while a real shift names the stage. The
  comparison is informational and never fails validation.
* The digest carries a schema version. A file written under a schema this
  version does not understand is rejected before decoding rather than
  silently restored without its digest.
* `frame_summary(detail = "pool")` reports one scalar row per selection
  pool and realization. Its `replicate` column is `1` for ordinary executions
  and identifies each realization for replicated executions. Fixed
  allocations are never collapsed and varying random-size allocations keep
  their values in pool detail. The compact stage detail retains its
  common-value-or-`NA` behavior.
* `n_target` records the nominal requested allocation for random-size
  designs as well as fixed-size designs. For fraction-based Bernoulli and
  Poisson sampling this is `N * frac`, before probability capping, and may be
  fractional. `n_expected` remains the sum of the resolved chances and
  `n_realized` the observed count.

## Diagnostics

* `summary()` prints one section per stage: a design line (method,
  MOS, clusters, strata, balancing declarations) and a realization
  line (population and sample sizes with fractions as N_h/n_h/f_h
  ranges; later stages report how many universe pools the
  realization reached), then one line of weight diagnostics (mean
  and range, CV, Kish DEFF, n_eff). The header states the universe
  size when the digest records a complete denominator and the executed path
  contains no WR stage. WR paths retain their realized draw or unit count
  without presenting the frame size as a sampling-coverage denominator.
  When no digest is available, WR draw counts use ancestry-qualified
  `.draw_k` occurrences; without that column, the output explicitly labels
  the fallback as selected clusters or selected units rather than draws.
  Unknown pool sizes omit the corresponding sampling fractions instead of
  interrupting the summary, and Chromy stages are labeled as minimum
  replacement rather than with replacement.
  Per-pool allocation tables live in `frame_summary(detail = "pool")`. The
  summary notation maps directly to its columns: N_h to `N`, n_h to
  `n_realized`, and f_h to `take_rate`.
* Capping diagnostics are classified by what happened, not by which code
  path detected it. A stage reports each distinct finding once, however many
  pools capped, however many parent pools it ran inside, and however many
  replicates ran.
  - `samplyr_warning_size_capped`: some pools held fewer units than the
    stage asked for. `frame_summary(detail = "pool")` marks the same pools
    in its `capped` column.
  - `samplyr_warning_census`: the stage selected every unit available in the
    pools it executed, so it contributes no sampling variance. This is a
    statement about the stage. Above the first stage those pools are the
    ones a sampled ancestor supplied, and the design as a whole is a census
    only if every stage is.
  - `samplyr_warning_nominal_cap`: a random-size method (`bernoulli`,
    `pps_poisson`, or a registered method declared as random-size) asked for
    more units than the pool holds. Clamping every chance at one caps the
    target the stage aims at; it does not select that many units, and the
    realized size usually lands below the cap.
  - `samplyr_warning_poisson_shortfall`: a `pps_poisson` pool resolved to an
    expectation more than 5% below what it could have reached, because
    dominant units saturated at probability 1. Measured against the reachable
    target, so a pool whose target the population already reduced is charged
    only for the further reduction saturation caused; a design reduced both
    ways gets both warnings. The payload carries `n_requested`,
    `n_reachable`, `n_expected` and `n_clipped`, and only affected pools are
    aggregated, so a healthy pool cannot mask a collapsed one.
  - `samplyr_message_allocation_capped`: a feasible allocation was
    redistributed past a saturated stratum.

  Every condition carries `stage`, an `operation` naming the detected event,
  and a `payload` whose fields are stable across detection sites: `pool_keys`,
  `n_capped`, `n_pools`, `n_requested`, `n_actual`, `n_available`,
  `n_reachable`, `n_expected`, `n_clipped`, `n_moved`, `n_replicates`, and
  `varied`. A field the event does not record is `NA` rather than zero. Pool
  identities are qualified by their parent, so the same stratum capping in
  three clusters reports as three pools rather than one. Replicates that
  reach different parents still report once: the pool lists are unioned and
  the message states that the counts describe one replicate. Replicates are
  classified before they are merged, so replicates that reach genuinely
  different outcomes report each one, and every condition names only the
  pools that produced it.
* `validate_frame()` checks for missing variables, NA values in key
  columns, and MOS/PRN/auxiliary variable issues before execution.
* When the frame is itself a `tbl_sample` (phase-2 preparation),
  `validate_frame()` pre-flights the two-phase export requirements:
  shared `cluster_by()` identifiers between the phases, unique on the
  phase-1 rows. Problems warn rather than error, because only
  `as_svydesign()` needs the linkage.

## Datasets

* `bfa_eas`: 44,570 enumeration areas from Burkina Faso for household budget
  and living-standards sampling. Companion tables `bfa_eas_variance` and
  `bfa_eas_cost` provide synthetic prior-consumption variances and relative
  fieldwork costs.
* `zwe_eas`: 107,250 enumeration areas from Zimbabwe for demographic, health,
  and child-indicator two-stage cluster survey sampling. Population and
  households calibrated to 2022 Census ward-level tallies. Demographic columns
  from WorldPop 100m age-sex grids.
* `ken_enterprises`: 17,004 establishments from Kenya for enterprise surveys,
  panel partitioning, and PRN coordination examples. Calibrated to the
  Republic of Kenya 2025 WBES universe (KRA register, 6 regions, 7 sectors).

## Vignettes

* Introduction: full tutorial covering SRS through multi-stage PPS designs.
* Design semantics: assumptions, weight formulas, and method properties.
* Survey analysis: export to survey/srvyr, joint probabilities, two-phase.
* Sampling coordination: PRN workflows, positive/negative coordination, and
  a longitudinal design taxonomy covering the vocabulary, the choice of
  longitudinal population, the five design types and how each is expressed,
  what `panels` builds and what it does not, and the limits that belong to
  the frame rather than the draw.
* Rotating panels: partitioning an executed master into panels, the rotation
  schedule, materializing the wave a panel is scheduled for, rotation
  programs across frame vintages, and stacking waves for an inference layer.
* Survey planning: svyplan integration, sample size, precision, design effects.
* Validation: deterministic invariants and Monte Carlo coverage checks on
  synthetic populations.
* Serialization: saving, sharing, and restoring designs as JSON files,
  frame fingerprints, reproducible execution receipts, and drift
  detection with the execution digest.
