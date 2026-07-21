# samplyr 0.8.9999

Initial release.

## Core grammar

* Frame-independent design specification with five verbs and one modifier:
  `sampling_design()`, `add_stage()`, `stratify_by()`, `cluster_by()`,
  `draw()`, and `execute()`.
* Designs are reusable across different frames.
* Public methods whose `...` arguments are reserved now reject unexpected
  arguments instead of silently ignoring likely misspellings.
* `serp()` now has one implementation and its input errors use stable
  `samplyr_error` subclasses.
* The README and introduction now state the panel-weight limitation,
  document frame-digest performance choices, and list all built-in methods
  consistently.
* Package prose now consistently uses US English.
* `execute()` now refuses duplicate input names and names reserved for its
  generated output, such as `.weight`, `.sample_id`, `.draw`, `.certainty`,
  and `.weight_k`, instead of silently overwriting user columns. Seed and
  replicate-seed ranges are validated before sampling begins.

## Sampling methods

* Added Sampford fixed-size PPS sampling (`method = "pps_sampford"`) with
  exact joint inclusion probabilities through `joint_expectation()`.
* Balanced sampling is now an explicit method family: the existing
  `method = "cube"` is now the canonical cube path (`"balanced"` remains a
  compatibility alias), while `"lpm2"` and
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
  (`on_empty = "error"`); `"warn"` and `"silent"` accept the empty
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
  Custom WR methods that declare PRN support now receive the supplied PRNs
  during ordinary execution; these PRNs were previously validated but ignored.
* Custom balanced methods (`type = "balanced"`) execute through
  `sondage::balanced_wor()`, may omit `mos` for equal probabilities, and may
  opt into `aux` or `spread` through the registry's `supports_aux` and
  `supports_spread` capabilities. They count toward the two-stage balanced
  limit and export with the built-in cube's variance treatment (Brewer,
  fraction-scale FPC) rather than falling through to SRS.
* `frac` validation follows a custom method's declared type: custom WR
  methods accept `frac > 1` like the built-in WR methods (the name-based
  test previously classified every custom method as WOR and rejected it).
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
* Compound strata and allocation-table keys use collision-free matching, even
  when values contain punctuation or control characters.
* Simple stratified SRS uses a preallocated grouped draw path. It preserves the
  same seeded `sample.int()` selections while avoiding per-stratum result
  objects and repeated key lookups.

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
* Panel labels are deterministic rotation or workload groups, not an
  additional probability-sampling phase. Full-sample weights remain valid for
  the combined sample; multiplying one panel's weights by the number of panels
  is not generally valid for population inference.

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
  (`"inform"`, `"warn"`, or `"ignore"`).

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
* `as_svrepdesign()` converts to replicate-weight designs. For PPS and
  balanced designs, `"subbootstrap"` and `"mrbbootstrap"` are supported.
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
  WR matrices now contain distinct population units in first-appearance
  order. Repeated WR parent occurrences define separate independent child
  blocks, and stratified blocks also follow first sample appearance. Frame
  and digest computations therefore share dimensions, order, and values for
  repeated-hit designs.
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
  now warns even when the sample is pristine. The diagnostic explains that
  this starts a new phase and restarts the design at stage 1, points to the
  stage-continuation
  form, and notes that an intentional new phase remains supported through
  `survey::twophase()`.
* Earlier-phase weights now carry through every stage when a multistage new
  phase is executed against separate frames in one call. The final weight is
  the product of the previous-phase weight and every conditional stage weight.
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
* Custom methods registered with `fixed_size = FALSE` are random-size;
  `as_svydesign()` now errors instead of applying Brewer's fixed-size
  approximation (which could report near-zero variance). Pass
  `pps = survey::poisson_sampling(1 / x$.weight)` for Poisson-type
  methods, or use `as_svrepdesign(type = "subbootstrap")`.

## Survey planning

* Samplyr now requires svyplan 0.8.9 or later so installations cannot
  silently use the earlier 0.8.8 API with the revised planning methods.
* Samplyr continues to re-export the `design_effect()`, `effective_n()`, and
  `varcomp()` generics from svyplan, with `tbl_sample` methods registered on
  those exact generics. `design_effect.tbl_sample()` now follows svyplan's
  numeric `svyplan_design_effect` contract. Use `as.double()` for the overall
  value and `as.data.frame()` for the Chen-Rust decomposition. Unstratified
  `varcomp.tbl_sample()` results now have the same one-row export schema as
  svyplan results.
* `design_effect()` and `effective_n()` with `tbl_sample` methods. Five
  methods: Kish, Henry, Spencer, Chen-Rust, and cluster planning.
  Auto-extraction of strata, clusters, and selection probabilities from
  the stored design.
* `varcomp()` gains a `tbl_sample` method: design-based variance
  components (B, W, delta, k) estimated from an executed clustered
  sample, feeding `svyplan::n_cluster()` for next-round planning. It
  applies the two conventions that are easy to get wrong by hand:
  within-PSU weights (the product of the per-stage weights below
  stage 1, never the compound `.weight`) and stage-1 selection shares
  derived from the stage-1 weights, normalized over the sampled PSUs
  (per stratum when stratified). Handles 2- and 3-stage designs with
  SRS, PPS (WOR and WR), and stratified first stages; refuses
  two-phase samples, certainty PSUs, and deeper designs with precise
  messages. The certainty guard now also recognizes realized PPS stage
  weights effectively equal to one, covering implicit probability capping
  even when no explicit certainty threshold was requested.
* `draw()` accepts `svyplan` sample size objects (`svyplan_n`, `svyplan_power`,
  `svyplan_cluster`) directly, and the handoff is stage-aware: cluster
  plans contribute the PSU count at a clustered stage 1 and the
  per-cluster take at later stages; stratified two-stage `n_alloc()`
  plans feed both stages by stratum; `n_multi()` domain plans become
  per-domain tables for domain-stratified designs. Consumption goes
  through svyplan's documented coercions (`as.data.frame()`,
  `as.integer()`), so draws use the plan's integerized field design
  (svyplan >= 0.8.8) rather than per-stage rounding.
* Clearer `draw()` errors: a named `n` without stage-level
  stratification, or with crossed stratification variables, now fails at
  design time with guidance instead of at execution.
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
* `frame_digest = "none"` now skips selection-trace construction entirely.
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
  `sondage::register_method(probabilities =)` (sondage >= 0.8.8):
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
  the `execute()` weight documentation now says so.
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
* Frame digest schema v2 is a clean compatibility break. It uses
  `n_expected` as the pool field and the length-prefixed compound-key
  encoding. Schema-v1 artifacts are rejected before decoding instead of
  being silently restored without their digest. Read a v1 artifact with the
  samplyr version that wrote it, or re-execute the original design and save
  it with the current version.
* Restored v2 digests retain the parent occurrence recorded for pools below
  a with-replacement parent stage.
* `frame_summary(detail = "pool")` now reports one scalar row per selection
  pool and realization. Its `replicate` column is `1` for ordinary executions
  and identifies each realization for replicated executions; fixed allocations
  are no longer conditionally collapsed and varying random-size allocations no
  longer become `NA` in pool detail. The compact stage detail retains its
  common-value-or-`NA` behavior.
* `n_target` now records the nominal requested allocation for random-size
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
  Realization lines apply thousands separators to pool counts, keep sampling
  fractions in fixed four-decimal notation, and place `draws` before the
  `across replicates` qualifier. Unknown pool sizes omit the corresponding
  sampling fractions instead of interrupting the summary, and Chromy stages
  are labeled as minimum replacement rather than with replacement.
  The missing-replicate warning now renders the `.replicate` field instead of
  exposing raw cli markup.
  Per-pool allocation tables live in `frame_summary(detail = "pool")`. The
  summary notation maps directly to its columns: N_h to `N`, n_h to
  `n_realized`, and f_h to `take_rate`.
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
* Sampling coordination: PRN workflows, positive/negative coordination.
* Survey planning: svyplan integration, sample size, precision, design effects.
* Validation: deterministic invariants and Monte Carlo coverage checks on
  synthetic populations.
* Serialization: saving, sharing, and restoring designs as JSON files,
  frame fingerprints, reproducible execution receipts, and drift
  detection with the execution digest.
