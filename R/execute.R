#' Conditions raised during execution
#'
#' [execute()] signals five conditions when a design cannot be realized as
#' written. Each is a classed condition carrying a `payload`, so it can be
#' caught and inspected rather than only read. They are reported once per
#' stage per distinct finding, not once per capped pool and not once per
#' replicate.
#'
#' A pool holding fewer units than the stage asks for is selected whole, which
#' makes the design non-self-weighting. `execute()` reports this once per
#' stage for each distinct finding, however many pools capped, however many
#' parent pools the stage ran inside, and however many replicates ran. Which
#' condition you get depends on what happened, not on which part of the
#' package noticed:
#'
#' \describe{
#'   \item{`samplyr_warning_size_capped`}{Some pools ran short. The stage left
#'     units behind in the pools it did not exhaust.}
#'   \item{`samplyr_warning_census`}{The stage selected every unit available
#'     in the pools it executed, so it contributes no sampling variance. This
#'     is a claim about the stage: above the first stage those pools are the
#'     ones a sampled ancestor supplied, and the design as a whole is a census
#'     only if every stage is.}
#'   \item{`samplyr_warning_nominal_cap`}{A random-size method asked for more
#'     units than the pool holds. Clamping every chance at one caps the target
#'     the stage aims at. It does not select that many units, and the realized
#'     size usually lands below the cap.}
#'   \item{`samplyr_warning_poisson_shortfall`}{A `pps_poisson` pool resolved
#'     to an expectation more than 5% below what it could have reached,
#'     because dominant units saturated at probability 1. Measured against the
#'     reachable target, so a pool whose target the population already reduced
#'     is charged only for the further reduction saturation caused. See
#'     [draw()].}
#'   \item{`samplyr_message_allocation_capped`}{A feasible allocation was
#'     redistributed past a saturated stratum. A message, not a warning:
#'     nothing went wrong, and simulation loops can silence it with
#'     `suppressMessages()`.}
#' }
#'
#' Every condition carries `stage`, an `operation` naming the detected event,
#' and a `payload` of aggregated detail. Payload fields mean the same thing
#' wherever the event was detected:
#'
#' \describe{
#'   \item{`pool_keys`}{The pools affected, qualified by their parent, so a
#'     stratum capping inside three clusters reports three pools rather than
#'     one.}
#'   \item{`n_capped`, `n_pools`}{Pools affected, out of pools executed.}
#'   \item{`n_requested`}{Units the stage asked for.}
#'   \item{`n_actual`}{Units selected.}
#'   \item{`n_available`}{Units the stage could have reached.}
#'   \item{`n_reachable`}{The target after any population bound, which is what
#'     a `pps_poisson` shortfall is measured against.}
#'   \item{`n_expected`}{The resolved expectation of a random-size stage.}
#'   \item{`n_clipped`}{Units whose computed chance exceeded one. Units taken
#'     by an explicit `certainty_size`/`certainty_prop` rule sit at one by
#'     instruction and are not counted here.}
#'   \item{`n_moved`}{Units redistributed by an allocation method.}
#'   \item{`n_replicates`, `varied`}{How many replicates reported this finding,
#'     and whether they reported it identically. When `varied` is `TRUE` the
#'     pool list is the union across those replicates while the counts describe
#'     one of them, and the printed message says so.}
#' }
#'
#' Replicates are classified before they are merged, so a replicated execution
#' whose replicates reach genuinely different outcomes reports each one. A
#' design that exhausts its clusters in some replicates and merely runs short
#' in others emits both `samplyr_warning_census` and
#' `samplyr_warning_size_capped`, each naming only the pools that produced it.
#' The count of conditions tracks distinct findings, not replicate count.
#'
#' A field the event does not record is `NA`, never zero.
#'
#' `frame_digest` defaults to `"summary"`, so the ordinary way to read capping
#' is the `capped` column of `frame_summary(sample, detail = "pool")`. It
#' compares the executable target with the pool population, so a random-size
#' method realizing below its target is never reported as capped. Reading a
#' digest against a design that does not record the stage leaves `capped` as
#' `NA` where a shortfall appears, because which of the two it is cannot be
#' told without the design.
#'
#' `capped` marks pools that could not supply the target they were given, so
#' it agrees with `samplyr_warning_size_capped` pool for pool. It does not
#' mark a stratum whose target an allocation method had already reduced to
#' the stratum population: the digest records the post-cap target, and the
#' two are equal by the time the pool is written. Read the conditions for
#' allocation capping and for a stage census. The column reports what
#' selection could not deliver.
#'
#' Under `frame_digest = "none"` there is no digest to read and the condition
#' is the only record. Capturing it needs a calling handler, because
#' `tryCatch()` unwinds and loses the sample, `suppressWarnings()` loses the
#' payload, and `rlang::catch_cnd()` loses the sample:
#'
#' ```r
#' capped <- NULL
#' sample <- withCallingHandlers(
#'   design |> execute(frame, seed = 1, frame_digest = "none"),
#'   samplyr_warning_size_capped = function(w) {
#'     capped <<- w$payload$pool_keys
#'     invokeRestart("muffleWarning")
#'   }
#' )
#' ```
#'
#'
#' @name execution-conditions
#' @family execution
#' @seealso [execute()] which raises them, [frame_summary()] whose `capped`
#'   column is the ordinary way to read capping, [validate_frame()] to catch
#'   frame problems before executing
NULL

#' Execute a sampling design
#'
#' `execute()` runs a sampling design against one or more data frames,
#' producing a sampled dataset with appropriate weights and metadata.
#'
#' @param .data A `sampling_design` object to start a new execution, or a
#'   partially executed `tbl_sample` to continue the remaining stages of its
#'   stored design.
#' @param ... Data frame(s) to sample from. For single-stage designs, provide
#'   one frame. For multi-stage designs with separate frames, provide frames
#'   in stage order. Passing a `tbl_sample` here while `.data` is a new
#'   `sampling_design` starts a new sampling phase. It does not continue the
#'   stages stored in that sample. Ordinary input frames must have unique
#'   names and must not use columns reserved for execution output, such as
#'   `.weight`, `.sample_id`, `.stage`, `.weight_k`, or `.fpc_k`, where `k`
#'   is the stage number (`.weight_1`, `.fpc_1`, and so on). Frames are
#'   matched positionally, so any name given here is a label. A label
#'   resembling one of the arguments below (`seedd`, or the singular
#'   `stage`, `rep`, `panel`) is refused rather than read as a frame,
#'   because those arguments follow `...` and are matched exactly.
#'
#'   A single unnamed list of data frames is read as those frames in that
#'   order, which is the same call written with a value rather than one
#'   argument per frame. Write them out when you are typing the call.
#'   The list is for code that already holds them, such as a loop over
#'   registers or [replay_design()]. Mixing the two is refused. Names
#'   inside the list are frame labels, so a misspelled argument must be
#'   written outside it to be reported as one.
#' @param stages Integer vector specifying which stage(s) to execute.
#'   From a `sampling_design`, the vector must start at stage 1 and this is how
#'   an operational workflow stops after its first contiguous batch of stages.
#'   From a partial `tbl_sample`, it must start at the next unexecuted stage.
#'   Default (`NULL`) executes all remaining stages.
#' @param seed Integer random seed for reproducibility, between
#'   `-.Machine$integer.max` and `.Machine$integer.max`.
#' @param panels Rotation groups (panels) to partition the sample into for
#'   rotation or workload management, as an integer count, a rotation
#'   schedule, or an `svyplan_schedule` from
#'   [svyplan::design_schedule()]. Assignment is a randomized fixed-quota partition
#'   within the assignment stage's selection strata: every unit carries each
#'   panel with probability `1 / panels`. It is not an additional
#'   probability-sampling phase. The output includes a `.panel` column with
#'   values 1 through the panel count. Default `NULL` means no panel
#'   partitioning. Cannot be used together with `reps`, and cannot be
#'   redeclared on a sample that already carries an assignment.
#'
#'   A schedule is a data frame with an integer `panel` column, an integer
#'   `wave` column and an optional logical `active` column. A combination
#'   left out is inactive. It declares the panel count and, through the
#'   fewest panels any wave activates, the assignment block size. Only a
#'   sample drawn with a schedule can be materialized by `wave`.
#'
#'   For an `svyplan_schedule`, `execute()` extracts the startup activity and
#'   checks its panel parameters before assignment. A gradual launch has one
#'   whole startup cohort and needs no partition. Permanent activation is not
#'   part of this automatic route.
#' @param panel_stage Stage whose selected units are assigned to panels, as a
#'   single stage number, or `NULL` (the default) for the first executed
#'   stage. Accepted only alongside `panels`, and the stage must be one the
#'   execution completes. Every later stage inherits its ancestor's panel, so
#'   assigning at stage 1 rotates whole primary units while assigning lower
#'   down rotates units inside parents that stay in the survey: the
#'   address-panel design, in which selected areas are retained and households
#'   rotate within them.
#'
#'   Pools are then the assignment stage's strata inside each realized parent,
#'   so they are smaller than first-stage pools and `small_pool` matters more.
#'   Selection certainty counts only at the assignment stage, in both
#'   directions: a certainty primary unit does not make its households
#'   permanent, and a certainty selection below the assignment stage does not
#'   keep its household in every wave. A stage that selects with replacement
#'   assigns realized draw occurrences, so one population unit selected twice
#'   may take two different panels. A parent selected twice likewise gives two
#'   separate populations of households, and a household reached under both
#'   hits is assigned once for each.
#'
#'   Assigning below the first stage is not a default and should not be
#'   treated as one. Holding the parent fixed while its members rotate can
#'   bias cross-sectional estimates over time, and a unit that cannot move
#'   between parents is balanced for net change but not for gross change.
#' @param small_pool What to do when a rotation schedule would leave a pool
#'   with no unit to activate, as `"error"` (the default, reached by `NULL`)
#'   or `"permanent"`. A pool of `m` units leaves `panels - m` panels empty,
#'   so a wave activating `r` of them selects nothing from that pool when
#'   `m <= panels - r`. Those units then have inclusion probability zero in
#'   that wave rather than a small weight, so the wave's estimator is biased.
#'   `"error"` refuses such an assignment, before any panel is drawn, and
#'   names the pools. `"permanent"` instead activates them at every wave with
#'   probability one, warning that it has done so, which is exact but changes
#'   the operational design: wave sizes, overlap and repeated interviewing all
#'   increase. Meaningful only with a schedule, since a panel count declares
#'   no wave to protect. This governs positivity only: a pool with a positive
#'   but single active unit is still assigned, and is still marked as carrying
#'   no within-block variance estimate. An `svyplan_schedule` refuses
#'   `"permanent"` because its overlap describes a fully rotating life.
#' @param wave Integer wave of a scheduled master to materialize, or `NULL`
#'   (default). `execute(master, wave = t)` activates the panels the stored
#'   schedule declares active at `t` and compounds the exact activation
#'   factor into `.weight`. It takes no other execution input: no frame, no
#'   `seed`, `stages`, `panels`, `small_pool` or `reps`, since a wave selects
#'   units the master already assigned, under the policy the master froze.
#' @param reps Integer number of independent replicate samples to draw (>= 2),
#'   or `NULL` (default) for a single sample. When specified, `execute()` draws
#'   `reps` independent samples from the same frame under the same design and
#'   returns a single stacked `tbl_sample` with a `.replicate` column (integer
#'   1 through `reps`). Replicate `r` uses seed `seed + r - 1`. The complete
#'   sequence must remain within `R` supported integer range. Cannot be
#'   combined with `panels` or with stages that use permanent random numbers.
#'
#'   This is **repeated sample realization** (drawing multiple independent
#'   samples), not replicate-weight variance estimation. For the latter, see
#'   [as_svrepdesign()].
#' @param frame_digest Controls the frame digest, a compact execution
#'   manifest recorded with the sample and read by [frame_summary()].
#'   `"summary"` (default) records anonymous population structure:
#'   selection pools, resolved chances (exact for cluster stages,
#'   constant or quantile-compressed for element stages), and the
#'   selected-unit trace. `"full"` keeps exact per-unit chances for
#'   element stages too. `"none"` records no digest and skips trace
#'   construction for minimum execution overhead. When one universe frame
#'   feeds every stage, later stages also record the pools their
#'   realization never reached, with chances resolved deterministically
#'   from the design (`chance_status = "design_resolved"`), this gives
#'   [frame_summary()] and downstream digest consumers complete universe
#'   denominators without the frame. The digest never
#'   affects selection, weights, or estimation. Design executions
#'   record a digest, replicated executions share the population
#'   structure across replicates with replicate-specific traces, and a
#'   stage continuation extends the digest carried by its input sample
#'   (an input without a valid digest yields no digest). In a
#'   replicated multi-stage execution, later-stage pools depend on each
#'   replicate's realized parents, so the digest keeps the stage
#'   prefix shared by all replicates and reports status `"partial"`.
#'   Replicated multi-phase and replicated-continuation executions do
#'   not record one yet.
#'
#' @return A `tbl_sample`: a data frame subclass carrying the selected rows,
#'   the design that produced them, and generated columns recording the
#'   selection. Those are `.sample_id`, `.weight`, the per-stage `.weight_k`,
#'   `.fpc_k`, `.draw_k` and `.certainty_k`, and `.replicate` or `.panel`
#'   when `reps` or `panels` is used. [sample-columns] documents what each
#'   one holds.
#'
#' @details
#' Every pattern below has a worked example under **Examples**.
#'
#' ## Multi-stage with a single frame
#' For hierarchical data where all stages are in one frame, pass that one
#' frame. It must contain all clustering variables and represent the stage
#' hierarchy correctly. Lower-stage IDs may repeat across different parents.
#' `samplyr` resolves them using the full ancestry from earlier stages.
#'
#' ## Multi-stage with one frame per stage
#' When each stage has its own register, pass one frame per stage.
#' Frames map to stages by position, and the number of frames is what
#' schedules the stages: one frame is a shared hierarchy covering all of
#' them, and one frame per stage gives each its own. Any other count is
#' `samplyr_error_frame_count`.
#'
#' Registers are supplied whole. Each is restricted to the units its parent
#' stage selected, and the variables earlier stages introduced are carried
#' onto it, so a lower register neither has to be pre-filtered nor to
#' duplicate the upper stages' stratification columns. A register that
#' legitimately omits a carried stratum is fine. One whose own copy of it
#' disagrees is an error.
#'
#' The frames may also be held as a list, which is the same call. Inside that
#' list a name is a diagnostic label for the frame, not an argument name.
#' Mixing the two spellings in one call is refused.
#'
#' This form, the single-hierarchy form, and the stage continuation below
#' run the same stage transition. Under one shared RNG stream, and with
#' `stages` given on every intermediate call, all three draw the same
#' sample.
#'
#' ## Partial execution (operational sampling)
#' `stages` executes only the stages named, returning a partial `tbl_sample`.
#' Fieldwork then produces the next stage's frame, and passing the partial
#' sample back as `.data` continues the same design.
#' Omitting `stages` on the continuation executes every remaining stage.
#' Where more than one stage remains and one frame is supplied, that frame
#' could be the next stage's register or a hierarchy covering the rest, and
#' the two draw different samples. `execute()` refuses to guess and asks for
#' `stages` (`samplyr_error_ambiguous_continuation`). [validate_frame()]
#' applies the same rule, so a frame it approves is one this call accepts.
#'
#' When the listing frame is derived from a `tbl_sample` (e.g. via
#' [tidyr::uncount()] or [dplyr::slice()]), it may carry internal
#' columns (`.weight`, `.fpc_1`, etc.) from the earlier stage. These
#' are automatically stripped before sampling so they do not collide
#' with the metadata carried by the stage-1 result.
#' Pass the unmodified stage-1 result as `.data` and the expanded listing
#' as the frame, as above. Passing the original design as `.data` instead
#' starts a new execution at stage 1 and treats a `tbl_sample` frame as a
#' previous sampling phase. It is not a stage continuation. When an intact
#' frame is a strict partial result of that same design, `execute()` warns
#' about this ambiguity but permits it because it is a valid new-phase
#' operation and will export through [survey::twophase()].
#' If a class-dropping operation such as [tidyr::uncount()] leaves a plain
#' listing with sampling attributes or generated columns, `execute()` refuses
#' to use it as an ordinary frame for a fresh design execution. It remains a
#' valid listing frame when the unmodified partial sample is `.data`. To use
#' such rows as a genuinely unrelated ordinary frame, remove both the sampling
#' attributes and the generated sample columns explicitly.
#'
#' ## Multi-phase sampling
#' To start a new phase, use the new phase's design as `.data` and pass the
#' previous phase's `tbl_sample` as its frame.
#' This is distinct from stage continuation: `phase1` is a frame for a new
#' design, rather than `.data` carrying unexecuted stages of the same design.
#' Weights compound automatically in multi-phase designs, and
#' [as_svydesign()] exports this path through [survey::twophase()].
#'
#' ## Weight calculation
#'
#' The `.weight` column is the inverse of the selection chance that
#' samplyr resolves for the unit based on the first-order inclusion probability
#' for without-replacement methods, or the expected number of
#' selections for with-replacement methods. The per-stage weight is
#' \eqn{w_i^{(k)} = 1 / \pi_i^{(k)}}{w_i(k) = 1 / pi_i(k)}:
#'
#' - **SRS**: \eqn{w_i = N / n}{w = N/n}, constant for all units.
#' - **Stratified SRS**: \eqn{w_i = N_h / n_h}{w = N_h/n_h} within stratum \eqn{h}.
#' - **PPS WOR**: \eqn{w_i = 1 / \pi_i}{w_i = 1/pi_i} where
#'   \eqn{\pi_i}{pi_i} is computed from the measure of size by
#'   `sondage::inclusion_prob()`. Varies across units.
#' - **WR / PMR**: \eqn{w_i = 1 / E(n_i)}{w_i = 1/E(n_i)} where
#'   \eqn{E(n_i) = n \cdot p_i}{E(n_i) = n * p_i} is the expected number
#'   of selections. Each draw is one row. A unit selected \eqn{k} times
#'   appears \eqn{k} times, each with the same weight.
#'
#' For every built-in method except `"pps_sps"` and `"pps_pareto"`, the
#' resolved chance equals the design's true first-order inclusion
#' probability (or expected hits), so `.weight` is the inverse of that
#' true probability. The order-sampling pair, and registered methods
#' declared `probabilities = "approximate"`, honor the resolved chance
#' only to a documented approximation: `.weight` is then the inverse of
#' the target probability, not of the design's true first-order
#' inclusion probability. Weighting by the inverse target is standard
#' practice for these methods, and the deviation is typically small,
#' but it is a bias of the method, not of samplyr. The tier is recorded
#' per stage in the frame digest, reported by [frame_summary()] as the
#' `probabilities` column, and flagged by `summary()`.
#'
#' ## Multi-stage weight compounding
#'
#' In a \eqn{K}-stage design, the overall weight for unit \eqn{i} is the
#' product of per-stage weights:
#' \deqn{w_i = \prod_{k=1}^{K} w_i^{(k)} = \prod_{k=1}^{K} \frac{1}{\pi_i^{(k \mid S^{(k-1)})}}}
#' where \eqn{\pi_i^{(k \mid S^{(k-1)})}}{pi_i(k | S(k-1))} is the
#' conditional inclusion probability at stage \eqn{k}, given the set of
#' clusters selected at all prior stages. For example, in a two-stage design
#' where 5 of 30 EAs are selected in a region (stage 1) and 12 of 50
#' households are listed within each selected EA (stage 2):
#' \deqn{w_i = \frac{30}{5} \times \frac{50}{12} = 6 \times 4.17 = 25}
#' The `.weight` column always equals the product of `.weight_1`, `.weight_2`,
#' etc. Per-stage weights are preserved for diagnostics and for survey
#' export.
#'
#' ## Multi-phase weight compounding
#'
#' When a new phase's design is executed with a previous-phase `tbl_sample`
#' as its frame, the phase-1 inclusion probability is already reflected in
#' the input weights.
#' The final `.weight` is the product of phase-1 and phase-2 weights:
#' \deqn{w_i = w_i^{(\text{phase 1})} \times w_i^{(\text{phase 2} \mid \text{phase 1})}}
#' This ensures the Horvitz-Thompson estimator
#' \eqn{\hat{Y} = \sum_S w_i \, y_i}{Y-hat = sum(w_i * y_i)} is unbiased
#' for the population total.
#'
#' ## Panel partitioning
#'
#' When `panels` is specified, the sample is partitioned into non-overlapping
#' groups for rotation or workload management.
#'
#' Assignment is randomized with fixed quotas. Within each selection stratum
#' of the assignment stage the assignment units are ordered, cut into
#' consecutive blocks of `2 * panels`, given a fixed quota per panel inside
#' each block, and permuted within their block. Every unit therefore carries
#' each panel with probability `1 / panels`, and panel sizes within a pool
#' differ by at most one.
#'
#' Blocking is what preserves order. Units adjacent in the `control` order of
#' the assignment stage's `draw()` fall in the same block, so every panel
#' inherits the same spread over that order. A pool holding fewer than
#' `2 * panels` units is a single block: still assigned, simply with no
#' block-level order structure left to preserve.
#'
#' For multi-stage designs, panels are assigned at stage 1 by default, and
#' every unit below inherits its ancestor's panel. `panel_stage` moves the
#' assignment to another stage, which is what expresses a design that retains
#' its primary units and rotates the units inside them. A pool is then the
#' assignment stage's strata inside each realized parent and never crosses a
#' parent. Under a with-replacement assignment stage the assignment unit is
#' the realized draw, the unit the estimator uses, so one population cluster
#' drawn twice may carry two different panels.
#'
#' Certainty units are labelled from their own pools and consume no rotating
#' quota. A certainty unit is in the sample at every occasion, so a schedule
#' that rotated it out would sample the very units the certainty stratum
#' exists to enumerate. Certainty counts only at the assignment stage, in both
#' directions: a certainty primary unit does not make the units below it
#' permanent when `panel_stage` names a lower stage, and a certainty selection
#' at a stage below the assignment stage does not keep its assignment unit in
#' every wave. A unit selected with probability one inside a rotating parent
#' is absent from the waves that parent sits out.
#'
#' Panels are assigned once. `.panel` is carried forward by a stage
#' continuation, and redeclaring `panels` on a sample that already carries an
#' assignment is an error.
#'
#' ## Rotation schedules and waves
#'
#' Passing a schedule to `panels` instead of a count declares which panels are
#' active at which occasion, and lets `execute(master, wave = t)` materialize
#' one of them:
#'
#' ```r
#' schedule <- data.frame(
#'   panel  = rep(1:4, times = 3),
#'   wave   = rep(1:3, each = 4),
#'   active = c(TRUE, TRUE, FALSE, FALSE,
#'              FALSE, TRUE, TRUE, FALSE,
#'              FALSE, FALSE, TRUE, TRUE)
#' )
#' master <- execute(design, frame, seed = 1, panels = schedule)
#' wave_2 <- execute(master, wave = 2)
#' ```
#'
#' The schedule is read at the master draw, not only at materialization,
#' because the block size follows from it. A schedule whose leanest wave
#' activates `r` of the `k` panels blocks at `k * ceiling(2 / r)` rather than
#' at the worst case `2k`, which keeps more of the assignment order while
#' still leaving two units per block in the take.
#'
#' Materializing wave `t` selects the panels declared active at `t` and
#' multiplies `.weight` by the inverse of the activation probability. That
#' probability is the block's frozen quota for the active panels over the
#' block size, so it is exact rather than nominal, and it is generally not
#' `k / r`. Permanent certainty units are activated at every wave with
#' probability one and their weights are untouched.
#'
#' A materialized wave is a sample in its own right, with its own integrity
#' record: it is not a filtered master, and `frame_summary()` and the weight
#' diagnostics work on it. It retains the master as its first phase, so
#' [as_svydesign()] exports it through [survey::twophase()] with the
#' activation as the second phase. What it cannot do is replay, because a wave
#' is derived from a recorded execution rather than being one. That refuses
#' rather than answering approximately.
#'
#' The schedule states which groups are live when. It does not replenish the
#' sample: every panel comes from the frame vintage the master was drawn
#' from. Steady-state replenishment is a fresh `execute()` against a later
#' frame. See `vignette("rotating-panels")`.
#'
#' Weights are not adjusted for panel membership. They reflect the full-sample
#' inclusion probability and are valid for the combined sample. Taking a
#' subset of the panels is a simple random subsample without replacement
#' within each block, but its conditional probability is the block's realized
#' quota over the block size, not `1 / panels`, so multiplying one panel's
#' weights by `panels` is not generally valid for population inference. The
#' block sizes and realized quotas are recorded with the sample and written
#' to the design file by [write_design()], because they, not `1 / panels`,
#' are what such a subset has to be computed against.
#'
#' ## When a design cannot be realized as written
#'
#' A pool holding fewer units than the stage asks for is selected whole,
#' which makes the design non-self-weighting. `execute()` reports this, and
#' four related outcomes, as classed conditions carrying a `payload`.
#'
#' `frame_digest` defaults to `"summary"`, so the ordinary way to read
#' capping after the fact is the `capped` column of
#' `frame_summary(sample, detail = "pool")`. See [execution-conditions] for
#' the five classes, their payload fields, and how to capture one when no
#' digest is kept.
#'
#' @examples
#' # Basic SRS execution
#' sample <- sampling_design() |>
#'   draw(n = 100) |>
#'   execute(bfa_eas, seed = 1234)
#' sample
#'
#' # Stratified execution with proportional allocation
#' sample <- sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 5789)
#' table(sample$region)
#'
#' # Two-stage cluster sample execution
#' zwe_frame <- zwe_eas |>
#'   dplyr::mutate(district_hh = sum(households), .by = district)
#'
#' sample <- sampling_design() |>
#'   add_stage(label = "Districts") |>
#'     cluster_by(district) |>
#'     draw(n = 20, method = "pps_brewer", mos = district_hh) |>
#'   add_stage(label = "EAs") |>
#'     draw(n = 10) |>
#'   execute(zwe_frame, seed = 3)
#' length(unique(sample$district))  # 20 districts selected
#'
#' # Partial execution: stage 1 only
#' design <- sampling_design() |>
#'   add_stage(label = "EAs") |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = households) |>
#'   add_stage(label = "Households") |>
#'     draw(n = 12)
#'
#' # Execute only stage 1 to get selected EAs
#' selected_eas <- execute(design, bfa_eas, stages = 1, seed = 2)
#' nrow(selected_eas)  # Number of selected EAs
#'
#' # Continuation: the listing produced by fieldwork becomes the next frame,
#' # and the partial sample is passed back as `.data`
#' listing <- selected_eas |>
#'   dplyr::slice(rep(seq_len(dplyr::n()), each = 20)) |>
#'   dplyr::mutate(hh_id = dplyr::row_number())
#' sample <- selected_eas |> execute(listing, seed = 43)
#' nrow(sample)  # 12 households in each selected EA
#'
#' # One frame per stage ----------------------------------------
#' # Frames map to stages by position: a district register, then an EA register
#' districts <- dplyr::distinct(zwe_eas, province, district)
#' two_stage <- sampling_design() |>
#'   add_stage(label = "Districts") |>
#'     cluster_by(district) |>
#'     draw(n = 8) |>
#'   add_stage(label = "EAs") |>
#'     draw(n = 3)
#' sample <- two_stage |> execute(districts, zwe_eas, seed = 424)
#' length(unique(sample$district))  # 8 districts, 3 EAs each
#'
#' # The same call with the frames held as a list
#' registers <- list(districts, zwe_eas)
#' same <- two_stage |> execute(registers, seed = 424)
#' identical(sample$.sample_id, same$.sample_id)
#'
#' # Multi-phase --------------------------------------------------
#' # The previous phase's sample is the new phase's frame
#' phase1 <- sampling_design() |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 42)
#' phase2 <- sampling_design() |>
#'   draw(n = 50) |>
#'   execute(phase1, seed = 123)
#' nrow(phase2)  # weights compound across both phases
#'
#' # Replicated sampling: 5 independent draws
#' sample <- sampling_design() |>
#'   draw(n = 100) |>
#'   execute(bfa_eas, seed = 42, reps = 5)
#' table(sample$.replicate)  # 100 per replicate
#'
#' # Rotating panel: 4 rotation groups
#' sample <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 1, panels = 4)
#' table(sample$.panel)  # ~50 per panel
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [frame-input-grammar] for the frame forms every verb accepts,
#' [sample-columns] for what the generated columns hold,
#' [execution-conditions] for what `execute()` signals when a design cannot
#' be realized as written,
#' [get_design()] for extracting metadata
#'
#' @family execution
#' @export
execute <- function(
  .data,
  ...,
  stages = NULL,
  seed = NULL,
  panels = NULL,
  panel_stage = NULL,
  small_pool = NULL,
  reps = NULL,
  wave = NULL,
  frame_digest = c("summary", "full", "none")
) {
  frame_digest <- match.arg(frame_digest)

  # Argument names belong to the call, so stray-argument reporting reads them
  # before the frames are normalized. Reading them must not evaluate them: a
  # misspelled argument is reported by its name, whatever its expression would
  # have done.
  check_execute_dot_names(enquos(...))
  dots <- list(...)

  execution_environment <- capture_execution_environment()

  # A program is a registry of executed cohorts, so the only thing it can
  # be executed for is a wave.
  if (is_rotation_program(.data)) {
    if (is_null(wave)) {
      abort_samplyr(
        c(
          "A rotation program is executed one wave at a time.",
          "i" = "{.code execute(program, wave = t)} materializes the
                 occasion {.arg t}."
        ),
        class = "samplyr_error_program_wave_required"
      )
    }
    return(materialize_program_wave(
      .data,
      wave,
      frames = dots,
      stages = stages,
      seed = seed,
      panels = panels,
      panel_stage = panel_stage,
      small_pool = small_pool,
      reps = reps,
      execution_environment = execution_environment
    ))
  }

  # A wave is materialized from a stored schedule rather than drawn from a
  # frame, so it leaves before frame normalization. Its guards refuse every
  # other execution input rather than quietly ignoring it.
  if (!is_null(wave)) {
    return(materialize_wave(
      .data,
      wave,
      frames = dots,
      stages = stages,
      seed = seed,
      panels = panels,
      panel_stage = panel_stage,
      small_pool = small_pool,
      reps = reps,
      execution_environment = execution_environment
    ))
  }

  supplied <- normalize_execute_frames(dots)
  frames <- supplied$frames

  if (length(frames) == 0) {
    cli_abort("At least one data frame must be provided")
  }

  check_execute_dots(frames, collected = supplied$collected)

  # The same layer validate_frame() runs, so a preflight cannot approve a
  # frame this call is about to refuse.
  check_frames_executable(
    frames,
    labels = if (supplied$collected) names(frames) else NULL,
    allow_generated = is_tbl_sample(.data),
    allow_stripped = !is_sampling_design(.data),
    require_rows = FALSE
  )

  if (!is_null(seed)) {
    if (
      length(seed) != 1L ||
        !is_integerish_numeric(seed) ||
        seed < -.Machine$integer.max ||
        seed > .Machine$integer.max
    ) {
      abort_samplyr(
        "{.arg seed} must be a single integer between
         { -.Machine$integer.max } and { .Machine$integer.max }.",
        class = "samplyr_error_seed_range"
      )
    }
    seed <- as.integer(seed)
  }

  panels <- normalize_panel_input(
    panels,
    panel_stage = panel_stage,
    small_pool = small_pool
  )

  if (!is_null(reps)) {
    if (
      !is.numeric(reps) ||
        length(reps) != 1 ||
        !is_integerish_numeric(reps) ||
        reps < 2
    ) {
      cli_abort("{.arg reps} must be a single integer >= 2")
    }
    reps <- as.integer(reps)
  }

  if (
    !is_null(seed) &&
      !is_null(reps) &&
      as.double(seed) + as.double(reps) - 1 > .Machine$integer.max
  ) {
    abort_samplyr(
      c(
        "Replicate seeds exceed the supported integer range.",
        "i" = "Use {.arg seed} <=
               { .Machine$integer.max - reps + 1L } for {reps} replicates."
      ),
      class = "samplyr_error_seed_overflow"
    )
  }

  if (!is_null(reps) && !is_null(panels)) {
    cli_abort("{.arg panels} and {.arg reps} cannot be used together.")
  }

  if (is_sampling_design(.data)) {
    design <- .data
    executed <- NULL
    validate_design_complete(design)
  } else if (is_tbl_sample(.data)) {
    check_weight_contract_execute(.data, "execute")
    design <- get_design(.data)
    executed <- get_stages_executed(.data)
  } else {
    cli_abort(
      "{.arg .data} must be a {.cls sampling_design} or {.cls tbl_sample}"
    )
  }

  # Resolve stages and match frames to them once, before any random number is
  # consumed, so every execution form runs the same linkage and a static
  # misuse cannot leave the RNG stream advanced.
  schedule <- stage_frame_schedule(design, frames, stages, executed)

  # The assignment stage is resolved against what this execution will have
  # completed, which is the executed stages plus the ones now scheduled. Here
  # rather than at assignment: a stage that is never drawn is a static misuse
  # and must not leave the RNG stream advanced.
  panels <- resolve_panel_stage(
    panels,
    executed = sort(unique(c(executed, schedule$stages))),
    design = design
  )

  # Only an all-at-once call can compare the candidate populations of
  # consecutive registers. Reported once here, outside any replicate loop.
  warn_incomplete_registers(
    schedule,
    design,
    previous_sample = if (is_tbl_sample(.data)) as.data.frame(.data) else NULL
  )

  # Keep panel diagnostics at the execute() call.
  user_call <- current_env()

  run_execution <- function() {
    warn_if_modified <- function(obj, role) {
      status <- sample_realization_status(obj)
      same_partial_design <-
        identical(role, "frame") &&
        is_sampling_design(.data) &&
        identical(get_design(obj), .data) &&
        length(get_stages_executed(obj)) < length(.data$stages)

      stage_hint <- if (same_partial_design) {
        c(
          "i" = "Passing a partial result as a frame starts a new sampling
                 phase and restarts the design at stage 1. It does not
                 continue with only the remaining stages.",
          "i" = "For operational multistage sampling, continue from the
                 unmodified partial sample and pass the listing as its frame:
                 {.code partial_sample |> execute(listing_frame)}."
        )
      } else {
        character(0)
      }

      if (same_partial_design && status$ok) {
        cli_warn(
          c(
            "The frame sample is a partial result of the same design.",
            stage_hint,
            "i" = "If a new phase is intended, this execution is valid and
                   will be exported through {.fn survey::twophase}."
          ),
          class = "samplyr_warning_same_design_frame"
        )
      }

      if (!status$ok) {
        mods <- status$mods
        cli_warn(c(
          "The {role} sample was modified after execution ({.field {mods}} changed).",
          "i" = "Its weights and design metadata are used as-is for the new selection.",
          stage_hint,
          "i" = "If rows were removed to define a subpopulation, prefer restricting the frame before executing."
        ))
      }
    }
    # Empty replicates otherwise disappear from the stacked sample.
    if (is_tbl_sample(.data)) {
      warn_if_modified(.data, "input")
      check_no_empty_replicates(.data, blocked = "stages")
    } else {
      # Prior-phase sample frames retain provenance. Listing frames do not.
      for (f in frames) {
        if (is_tbl_sample(f)) {
          warn_if_modified(f, "frame")
          check_no_empty_replicates(f, blocked = "phase")
        }
      }
    }

    # PRN conflict check: only on stages being executed in this call
    if (!is_null(reps)) {
      valid_stages <- schedule$stages
      if (length(valid_stages) > 0) {
        has_prn <- any(vapply(
          valid_stages,
          function(idx) {
            !is_null(design$stages[[idx]]$draw_spec$prn)
          },
          logical(1)
        ))
        if (has_prn) {
          cli_abort(c(
            "{.arg reps} cannot be used when an executed stage uses permanent random numbers.",
            "i" = "PRN produces identical samples across replicates.",
            "i" = "Use a loop with different PRN vectors for coordinated repeated sampling."
          ))
        }
      }
    }

    # Detect replicated tbl_sample passed as a frame (multi-phase)
    frame_has_reps <- any(vapply(
      frames,
      function(f) {
        is_tbl_sample(f) && has_multiple_replicates(f)
      },
      logical(1)
    ))

    if (is_sampling_design(.data)) {
      if (frame_has_reps) {
        if (!is_null(reps)) {
          cli_abort(c(
            "Cannot add new replicates when the frame is already replicated.",
            "i" = "The frame has {length(unique(frames[[1]]$.replicate))} replicates."
          ))
        }
        if (!is_null(panels)) {
          cli_abort(
            "{.arg panels} cannot be used with a replicated frame.",
          )
        }
        execute_replicated_multiphase(
          .data,
          schedule,
          seed,
          execution_environment
        )
      } else if (is_null(reps)) {
        execute_design(
          .data,
          schedule,
          seed,
          panels,
          execution_environment,
          frame_digest = frame_digest,
          call = user_call
        )
      } else {
        execute_replicated(
          .data,
          schedule,
          seed,
          reps,
          executor = "design",
          execution_environment = execution_environment,
          frame_digest = frame_digest
        )
      }
    } else {
      has_existing_reps <- has_multiple_replicates(.data)
      if (has_existing_reps && !is_null(reps)) {
        cli_abort(c(
          "Cannot add new replicates to an already-replicated sample.",
          "i" = "The input sample already has {length(unique(.data$.replicate))} replicates."
        ))
      }
      if (has_existing_reps) {
        execute_replicated_continuation(
          .data,
          schedule,
          seed,
          panels,
          execution_environment,
          frame_digest = frame_digest,
          call = user_call
        )
      } else if (!is_null(reps)) {
        execute_replicated(
          .data,
          schedule,
          seed,
          reps,
          executor = "continuation",
          execution_environment = execution_environment
        )
      } else {
        execute_continuation(
          .data,
          schedule,
          seed,
          panels,
          execution_environment,
          frame_digest = frame_digest,
          call = user_call
        )
      }
    }
  }

  # One report per execution, whatever shape it has: stages, cluster loops,
  # replicates, continuations and phases all funnel through here.
  report_selection_events(
    if (!is_null(seed) && is_null(reps)) {
      withr::with_seed(seed, run_execution())
    } else {
      run_execution()
    }
  )
}

#' Report a stray argument that landed in execute()'s dots as a frame
#'
#' `stages`, `seed`, `panels`, `reps` and `frame_digest` all sit after `...`,
#' so R matches them exactly and a near miss lands here as an extra frame.
#' Naming the stray argument beats reporting its position, which describes
#' the wrong problem.
#'
#' This reads the call's own argument names, so it runs before the frames are
#' normalized: inside a list, a name is a frame label and must never be read
#' as a misspelled argument.
#' @noRd
check_execute_dot_names <- function(dots, call = rlang::caller_env()) {
  reserved <- c(
    "stages", "seed", "panels", "panel_stage", "small_pool", "reps", "wave",
    "frame_digest"
  )
  nms <- names(dots) %||% rep("", length(dots))

  for (i in seq_along(dots)) {
    name <- nms[[i]]
    if (!nzchar(name) || is.null(suggest_reserved_arg(name, reserved))) {
      next
    }
    abort_samplyr(
      c(
        "{.fn execute} received an unexpected argument.",
        stray_arg_bullets(name, reserved),
        "i" = "Frames are passed positionally, in stage order."
      ),
      class = "samplyr_error_unknown_argument",
      call = call
    )
  }
  invisible(dots)
}

#' Resolve the two spellings of execute()'s frames into one ordered list
#'
#' Frames are normally written out one per argument. A single unnamed list
#' collects the same frames as a value, which is what a caller building them
#' programmatically has: `replay_design()` and any loop over registers would
#' otherwise have to splice. Data frames are lists too, so the data-frame test
#' comes first, and a named list stays an ordinary argument so a misspelled
#' one is still reported as such.
#' @noRd
normalize_execute_frames <- function(dots, call = rlang::caller_env()) {
  nms <- names(dots) %||% rep("", length(dots))
  is_bare_list <- function(x) !is.data.frame(x) && is.list(x)

  collections <- which(vapply(dots, is_bare_list, logical(1)) & !nzchar(nms))
  if (length(collections) == 0) {
    return(list(frames = dots, collected = FALSE))
  }
  if (length(collections) == 1L && length(dots) == 1L) {
    return(list(frames = dots[[1]], collected = TRUE))
  }

  # Built as plain text: the positions and the remaining count are two
  # separate quantities, which cli cannot pluralize in one string.
  where <- paste(collections, collapse = ", ")
  detail <- paste0(
    if (length(collections) == 1L) "Argument " else "Arguments ",
    where,
    if (length(collections) == 1L) " is a list" else " are lists",
    ", alongside ",
    length(dots) - length(collections),
    if (length(dots) - length(collections) == 1L) {
      " other argument."
    } else {
      " other arguments."
    }
  )
  abort_samplyr(
    c(
      "{.fn execute} received both a list of frames and separate frames.",
      "x" = detail,
      "i" = "Pass every frame as its own argument, or all of them as one
             list."
    ),
    class = "samplyr_error_frame_count",
    call = call
  )
}

#' Check the frames execute() will sample from
#'
#' `collected` says where the frames came from, which is what a name means. On
#' the call's own arguments a name can be a misspelled argument, so it is
#' diagnosed as one. Inside a list it is a frame label, as documented, and a
#' bad member is a bad frame rather than a stray argument.
#' @noRd
check_execute_dots <- function(
  frames,
  collected = FALSE,
  call = rlang::caller_env()
) {
  reserved <- c(
    "stages", "seed", "panels", "panel_stage", "small_pool", "reps", "wave",
    "frame_digest"
  )
  nms <- names(frames) %||% rep("", length(frames))

  for (i in seq_along(frames)) {
    name <- nms[[i]]
    if (is.data.frame(frames[[i]])) {
      next
    }
    stray <- nzchar(name) && !collected

    abort_samplyr(
      c(
        if (stray) {
          c(
            "{.fn execute} received an unexpected argument.",
            stray_arg_bullets(name, reserved)
          )
        } else if (collected) {
          c(
            "{frame_token(i, name)} must be a data frame.",
            "x" = "Got {.cls {class(frames[[i]])[[1]]}}."
          )
        } else {
          c(
            "Argument {i} to {.fn execute} must be a data frame.",
            "x" = "Got {.cls {class(frames[[i]])[[1]]}}."
          )
        },
        "i" = "Frames are passed positionally, in stage order."
      ),
      class = if (stray) {
        "samplyr_error_unknown_argument"
      } else {
        "samplyr_error_frame_not_data_frame"
      },
      call = call
    )
  }

  invisible(frames)
}

#' Capture the implementation state that affects a sample realization
#' @noRd
capture_execution_environment <- function() {
  rng <- RNGkind()
  list(
    language = list(
      name = "R",
      version = as.character(getRversion())
    ),
    packages = list(
      samplyr = as.character(utils::packageVersion("samplyr")),
      sondage = as.character(utils::packageVersion("sondage")),
      svyplan = as.character(utils::packageVersion("svyplan"))
    ),
    rng = list(
      kind = unname(rng[[1]]),
      normal_kind = unname(rng[[2]]),
      sample_kind = unname(rng[[3]])
    )
  )
}

#' @noRd
execute_design <- function(
  design,
  schedule,
  seed,
  panels,
  execution_environment,
  frame_digest = "summary",
  call = caller_env()
) {
  stages <- schedule$stages
  frames <- schedule_frames(schedule)

  # The digest records what the user supplied. Preparing a previous-phase
  # frame strips its generated columns and adds an internal weight, so the
  # prepared version must not stand in for the original when fingerprinting.
  supplied_frames <- frames

  prev_phase <- NULL
  for (i in seq_along(frames)) {
    prepared <- prepare_multiphase_frame(frames[[i]])
    frames[[i]] <- prepared$frame
    if (!is_null(prepared$prev_phase)) {
      prev_phase <- prepared$prev_phase
    }
  }

  # Derived once from the phase this execution descends from, then carried
  # through every stage transition below. Checked here, before any stage
  # draws, because a clustered stage keeps one representative row and would
  # otherwise resolve an ambiguous key silently.
  phase_link_vars <- phase_link_vars_of(prev_phase)
  check_phase_key_invariance(schedule, design, phase_link_vars, prev_phase)

  current_sample <- NULL
  previous_stage_idx <- NULL
  all_prior_cluster_vars <- character(0)
  collect_trace <- !identical(frame_digest, "none")
  stage_traces <- if (collect_trace) vector("list", length(stages)) else NULL
  stage_used_frames <- if (collect_trace) {
    vector("list", length(stages))
  } else {
    NULL
  }

  for (i in seq_along(stages)) {
    stage_idx <- stages[i]
    frame <- frames[[i]]
    stage_spec <- design$stages[[stage_idx]]
    prev_stage_for_frame <- if (is_null(previous_stage_idx)) {
      NULL
    } else {
      design$stages[[previous_stage_idx]]
    }

    is_final_stage_of_execution <- (i == length(stages))
    is_final_stage_of_design <- (stage_idx == length(design$stages))
    is_final_stage <- is_final_stage_of_execution || is_final_stage_of_design

    if (!is_null(current_sample)) {
      entry <- schedule$entries[[i]]
      frame <- link_stage_frame(
        frame,
        current_sample,
        design = design,
        stage_idx = stage_idx,
        frame_index = entry$frame_index,
        frame_label = entry$frame_label,
        phase_link_vars = phase_link_vars
      )$frame
    }

    step <- execute_single_stage(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_idx,
      previous_sample = current_sample,
      previous_stage_spec = prev_stage_for_frame,
      is_final_stage = is_final_stage,
      all_prior_cluster_vars = all_prior_cluster_vars,
      trace_mode = frame_digest
    )
    current_sample <- step$sample
    if (collect_trace) {
      stage_traces[[i]] <- step$trace
      stage_used_frames[[i]] <- step$frame
    }

    # A random-size stage with on_empty = "warn"/"silent" can select
    # zero units. Later stages then have nothing to select from: the
    # empty sample is the realization, so stop here.
    if (nrow(current_sample) == 0) {
      break
    }

    if (!is_null(stage_spec$clusters)) {
      all_prior_cluster_vars <- unique(c(
        all_prior_cluster_vars,
        stage_spec$clusters$vars
      ))
    }
    previous_stage_idx <- stage_idx
  }

  panel_assignment <- NULL
  if (!is_null(panels) && nrow(current_sample) > 0) {
    assigned <- assign_panels(
      current_sample,
      panels,
      panel_assignment_context(
        design, panels$stage %||% stages[1], current_sample, call = call
      ),
      call = call
    )
    current_sample <- assigned$sample
    panel_assignment <- assigned$record
  }

  if (
    !is_null(prev_phase) &&
      "._prev_phase_weight" %in% names(current_sample)
  ) {
    current_sample$.weight <- current_sample$.weight *
      current_sample$._prev_phase_weight
    current_sample$._prev_phase_weight <- NULL
  }

  digest <- NULL
  if (!identical(frame_digest, "none")) {
    # Digest failure must never fail the execution: the sample is the
    # product, the digest an annotation.
    digest <- tryCatch(
      build_frame_digest(
        design = design,
        stage_ids = stages,
        stage_traces = stage_traces,
        stage_frames = stage_used_frames,
        input_frames = supplied_frames,
        mode = frame_digest,
        sample = current_sample
      ),
      error = function(e) {
        cli_warn(c(
          "The frame digest could not be recorded for this execution.",
          "i" = conditionMessage(e)
        ))
        NULL
      }
    )
  }

  new_tbl_sample(
    data = current_sample,
    design = design,
    stages_executed = stages,
    seed = seed,
    metadata = list(
      n_selected = nrow(current_sample),
      executed_at = Sys.time(),
      panels = panels$k,
      panel_assignment = panel_assignment,
      prev_phase = prev_phase,
      execution_environment = execution_environment,
      integrity = sample_integrity_record(current_sample, design, stages),
      frame_digest = digest,
      frame_schedule = schedule_record(schedule)
    )
  )
}

#' @noRd
execute_replicated <- function(
  .data,
  schedule,
  seed,
  reps,
  executor = "design",
  execution_environment,
  frame_digest = "none",
  call = caller_env()
) {
  results <- vector("list", reps)
  rep_digests <- vector("list", reps)
  collect_digest <- executor == "design" &&
    !identical(frame_digest, "none")

  for (r in seq_len(reps)) {
    rep_seed <- if (!is_null(seed)) seed + r - 1L else NULL

    run_one <- function() {
      if (executor == "design") {
        execute_design(
          .data,
          schedule,
          rep_seed,
          panels = NULL,
          execution_environment = execution_environment,
          frame_digest = if (collect_digest) frame_digest else "none"
        )
      } else {
        execute_continuation(
          .data,
          schedule,
          rep_seed,
          panels = NULL,
          execution_environment = execution_environment
        )
      }
    }

    result <- tag_replicate_events(
      if (!is_null(rep_seed)) {
        withr::with_seed(rep_seed, run_one())
      } else {
        run_one()
      },
      replicate = r
    )

    if (collect_digest) {
      rep_digests[[r]] <- attr(result, "metadata")$frame_digest
    }
    df <- as.data.frame(result)
    df$.replicate <- rep.int(r, nrow(df))
    results[[r]] <- df
  }

  digest <- NULL
  if (collect_digest) {
    digest <- tryCatch(
      merge_replicated_digests(rep_digests),
      error = function(e) {
        cli_warn(c(
          "The frame digest could not be recorded for this replicated
           execution.",
          "i" = conditionMessage(e)
        ))
        NULL
      }
    )
  }

  combined <- do.call(rbind, results)
  combined$.sample_id <- seq_len(nrow(combined))

  # Derive metadata from inputs, not from last loop iteration.
  # Only prev_phase genuinely comes from inner pipeline.
  the_design <- if (is_sampling_design(.data)) .data else get_design(.data)
  the_stages <- if (executor == "design") {
    schedule$stages
  } else {
    c(get_stages_executed(.data), schedule$stages)
  }

  integrity <- sample_integrity_record(combined, the_design, the_stages)
  integrity$replicate_hashes <- replicate_integrity_hashes(
    combined,
    integrity$cols,
    seq_len(reps)
  )

  new_tbl_sample(
    data = combined,
    design = the_design,
    stages_executed = the_stages,
    seed = seed,
    metadata = list(
      n_selected = nrow(combined),
      executed_at = Sys.time(),
      reps = reps,
      replicate_seeds = if (!is_null(seed)) {
        seed + seq_len(reps) - 1L
      } else {
        NULL
      },
      replicate_rows = setNames(
        vapply(results, nrow, integer(1)),
        as.character(seq_len(reps))
      ),
      prev_phase = attr(result, "metadata")$prev_phase,
      execution_environment = execution_environment,
      integrity = integrity,
      frame_digest = digest,
      frame_schedule = schedule_record(schedule)
    )
  )
}

#' @noRd
execute_continuation <- function(
  sample,
  schedule,
  seed,
  panels,
  execution_environment,
  frame_digest = "none",
  call = caller_env()
) {
  design <- get_design(sample)
  executed <- get_stages_executed(sample)
  stages <- schedule$stages
  frames <- schedule_frames(schedule)

  parent_meta <- attr(sample, "metadata")
  phase_link_vars <- phase_link_vars_of(parent_meta$prev_phase)

  current_sample <- as.data.frame(sample)
  # An assignment is two things that only mean something together: the record,
  # which states the pools, blocks and quotas a later wave is computed from,
  # and the `.panel` column, which says which panel each unit is in. Either
  # one alone says a sample has been assigned, so both are tested.
  has_record <- !is_null(parent_meta$panel_assignment)
  has_column <- ".panel" %in% names(current_sample)

  # Panels are assigned once, at the draw that creates them. A continuation
  # carries the assignment forward, so redeclaring `panels` would overwrite
  # one whose pools and quotas are already frozen in the receipt.
  if (!is_null(panels) && (has_record || has_column)) {
    abort_samplyr(
      c(
        "{.arg panels} cannot be declared on a sample that already carries a
         panel assignment.",
        "x" = "Panels are assigned once and carried forward by later
               stages.",
        "i" = "Continue without {.arg panels} to keep the assignment
               recorded with the master draw."
      ),
      class = "samplyr_error_panels_already_assigned",
      call = call
    )
  }

  # Checked whether or not new panels were asked for: continuing a
  # half-assignment would carry it into the result and on into a wave.
  if (!identical(has_record, has_column)) {
    abort_samplyr(
      c(
        "This sample's panel assignment is incomplete.",
        "x" = if (has_record) {
          "It carries an assignment record but no {.field .panel} column."
        } else {
          "It carries a {.field .panel} column but no assignment record."
        },
        "i" = "The record holds the pools, blocks and quotas a wave is
               computed from, and {.field .panel} says which panel each unit
               is in. One without the other can be neither activated nor
               replayed.",
        "i" = "Design columns are removed by modifying a sample after it was
               executed. Continue the sample {.fn execute} returned."
      ),
      class = "samplyr_error_panel_assignment_incomplete",
      call = call
    )
  }
  last_executed_stage <- max(executed)
  previous_stage_idx <- last_executed_stage
  all_prior_cluster_vars <- collect_ancestor_cluster_vars(design, stages[1])

  collect_trace <- !identical(frame_digest, "none")
  stage_traces <- if (collect_trace) vector("list", length(stages)) else NULL
  stage_used_frames <- if (collect_trace) {
    vector("list", length(stages))
  } else {
    NULL
  }
  input_frames_used <- if (collect_trace) {
    vector("list", length(stages))
  } else {
    NULL
  }

  for (i in seq_along(stages)) {
    stage_idx <- stages[i]
    frame <- frames[[i]]
    stage_spec <- design$stages[[stage_idx]]
    prev_stage_for_frame <- design$stages[[previous_stage_idx]]

    # Strip internal columns from the frame so they do not collide with
    # stage-1 metadata carried in current_sample (e.g. .fpc_1, .weight).
    internal <- samplyr_internal_cols(frame)
    if (length(internal) > 0L) {
      frame <- frame[, setdiff(names(frame), internal), drop = FALSE]
    }
    if (collect_trace) {
      input_frames_used[[i]] <- frame
    }

    is_final_stage_of_execution <- (i == length(stages))
    is_final_stage_of_design <- (stage_idx == length(design$stages))
    is_final_stage <- is_final_stage_of_execution || is_final_stage_of_design

    entry <- schedule$entries[[i]]
    frame <- link_stage_frame(
      frame,
      current_sample,
      design = design,
      stage_idx = stage_idx,
      frame_index = entry$frame_index,
      frame_label = entry$frame_label,
      phase_link_vars = phase_link_vars
    )$frame

    step <- execute_single_stage(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_idx,
      previous_sample = current_sample,
      previous_stage_spec = prev_stage_for_frame,
      is_final_stage = is_final_stage,
      all_prior_cluster_vars = all_prior_cluster_vars,
      trace_mode = frame_digest
    )
    current_sample <- step$sample
    if (collect_trace) {
      stage_traces[[i]] <- step$trace
      stage_used_frames[[i]] <- step$frame
    }

    # See execute_design(): an empty stage ends the selection.
    if (nrow(current_sample) == 0) {
      break
    }

    if (!is_null(stage_spec$clusters)) {
      all_prior_cluster_vars <- unique(c(
        all_prior_cluster_vars,
        stage_spec$clusters$vars
      ))
    }
    previous_stage_idx <- stage_idx
  }

  panel_assignment <- parent_meta$panel_assignment
  if (!is_null(panels) && nrow(current_sample) > 0) {
    assignment_stage <- panels$stage %||% c(executed, stages)[1]
    assigned <- assign_panels(
      current_sample,
      panels,
      panel_assignment_context(
        design, assignment_stage, current_sample, call = call
      ),
      call = call
    )
    current_sample <- assigned$sample
    panel_assignment <- assigned$record
  }

  digest <- NULL
  if (!identical(frame_digest, "none")) {
    # Merge only onto a prior digest that still describes its sample.
    # a continuation cannot manufacture the manifest of stages it did
    # not observe.
    prior <- get_frame_digest(sample)
    if (!is_null(prior) && !identical(prior$status, "invalidated")) {
      digest <- tryCatch(
        merge_continuation_digest(
          prior,
          design,
          stages,
          stage_traces = stage_traces,
          stage_frames = stage_used_frames,
          input_frames = input_frames_used
        ),
        error = function(e) {
          cli_warn(c(
            "The frame digest could not be extended for this
             continuation.",
            "i" = conditionMessage(e)
          ))
          NULL
        }
      )
    }
  }

  new_tbl_sample(
    data = current_sample,
    design = design,
    stages_executed = c(executed, stages),
    seed = seed,
    metadata = list(
      n_selected = nrow(current_sample),
      executed_at = Sys.time(),
      panels = panels$k %||% parent_meta$panels,
      panel_assignment = panel_assignment,
      frame_digest = digest,
      continued_from = parent_meta,
      # A stage continuation remains in the same phase. If that phase was
      # itself sampled from an earlier phase, retain the phase link so survey
      # export does not mistake the completed multistage phase for a
      # single-phase design.
      prev_phase = parent_meta$prev_phase,
      execution_environment = execution_environment,
      integrity = sample_integrity_record(
        current_sample,
        design,
        c(executed, stages)
      ),
      frame_schedule = schedule_record(schedule)
    )
  )
}

#' Phase number of a sample (1 + length of its prev_phase chain)
#' @noRd
sample_phase_number <- function(x) {
  n <- 1L
  prev <- attr(x, "metadata")$prev_phase
  while (is.list(prev) && is_tbl_sample(prev$sample)) {
    n <- n + 1L
    prev <- attr(prev$sample, "metadata")$prev_phase
  }
  n
}

#' Replicate ids of a stacked sample that have zero rows
#'
#' Empty replicates leave no rows in the stacked data, so they are
#' invisible to the .replicate column. They are recovered from the
#' per-replicate row counts recorded at execution
#' (metadata$replicate_rows), with metadata$reps as a fallback for
#' samples that predate replicate_rows.
#' @noRd
find_empty_replicates <- function(sample) {
  meta <- attr(sample, "metadata")
  counts <- meta$replicate_rows
  if (!is_null(counts) && !is_null(names(counts))) {
    return(names(counts)[counts == 0L])
  }
  reps <- meta$reps
  if (is_null(reps) || !".replicate" %in% names(sample)) {
    return(character(0))
  }
  present <- unique(sample$.replicate)
  if (length(present) < reps) {
    return(as.character(setdiff(seq_len(reps), present)))
  }
  character(0)
}

#' Check that a tbl_sample entering execution has no empty replicates
#'
#' A verified single-replicate extraction (filter(.replicate == r) of a
#' complete nonempty replicate) is a standalone sample. Empty siblings
#' recorded in the parent metadata are irrelevant to it.
#' @noRd
check_no_empty_replicates <- function(x, blocked, call = caller_env()) {
  if (identical(sample_modifications(x), "rows") && is_complete_replicate(x)) {
    return(invisible(NULL))
  }
  empty_reps <- find_empty_replicates(x)
  if (length(empty_reps) > 0) {
    abort_empty_replicate(x, empty_reps, blocked = blocked, call = call)
  }
  invisible(NULL)
}

#' Abort when empty replicates block further execution
#'
#' A replicate with zero rows (an accepted empty realization under
#' on_empty = "warn"/"silent") cannot serve as the frame for a later
#' phase or as the basis for continuing later stages. Without this
#' check the replicate loop, which reads replicate ids from the data,
#' would silently skip empty replicates and condition all downstream
#' results on nonempty realizations. The error names the replicates,
#' phase, and random-size method(s) so the failure is traceable to the
#' design, and warns against dropping empty replicates for the same
#' conditioning reason.
#' @noRd
abort_empty_replicate <- function(
  sample,
  r,
  blocked = c("phase", "stages"),
  call = caller_env()
) {
  blocked <- match.arg(blocked)
  design <- get_design(sample)
  stages_exec <- get_stages_executed(sample)
  rs_methods <- unique(unlist(lapply(stages_exec, function(i) {
    spec <- design$stages[[i]]$draw_spec
    if (
      spec$method %in% rs_poisson_methods || identical(spec$method_fixed, FALSE)
    ) {
      spec$method
    } else {
      NULL
    }
  })))
  phase <- sample_phase_number(sample)
  title <- design$title

  if (blocked == "phase") {
    header <- "Replicate{cli::qty(r)}{?s} {r} of the phase-{phase} sample {cli::qty(r)}{?is/are} empty, so phase {phase + 1L} cannot be executed."
    blocked_txt <- paste0("phase-", phase + 1L, " execution")
  } else {
    header <- "Replicate{cli::qty(r)}{?s} {r} {cli::qty(r)}{?is/are} empty, so the remaining stages cannot be executed."
    blocked_txt <- "continuing the remaining stages"
  }

  method_bullet <- if (length(rs_methods) > 0) {
    c(
      "i" = "Empty realizations are possible under random-size method{?s} {.val {rs_methods}}."
    )
  } else {
    c(
      "i" = "Empty realizations are possible under Bernoulli and Poisson sampling."
    )
  }

  abort_samplyr(
    c(
      header,
      if (!is_null(title)) c("i" = "Design: {.val {title}}."),
      method_bullet,
      "i" = "Increase the expected sample size, use a fixed-size method, or handle empty replicates explicitly before {blocked_txt}.",
      "i" = "Dropping empty replicates conditions results on nonempty realizations, which can bias simulation summaries."
    ),
    class = "samplyr_error_empty_phase_replicate",
    call = call
  )
}

#' @noRd
execute_replicated_continuation <- function(
  sample,
  schedule,
  seed,
  panels,
  execution_environment,
  frame_digest = "none",
  call = caller_env()
) {
  if (!is_null(panels)) {
    cli_abort(
      "{.arg panels} cannot be used with a replicated sample.",
      call = call
    )
  }

  rep_ids <- sort(unique(sample$.replicate))
  results <- vector("list", length(rep_ids))

  for (i in seq_along(rep_ids)) {
    r <- rep_ids[i]
    rep_sample <- sample[sample$.replicate == r, ]
    rep_sample$.replicate <- NULL

    # Restore tbl_sample class for the subset
    rep_sample <- new_tbl_sample(
      data = rep_sample,
      design = get_design(sample),
      stages_executed = get_stages_executed(sample),
      seed = attr(sample, "seed"),
      metadata = attr(sample, "metadata")
    )

    rep_seed <- if (!is_null(seed)) seed + i - 1L else NULL

    run_one <- function() {
      execute_continuation(
        rep_sample,
        schedule,
        rep_seed,
        panels = NULL,
        execution_environment = execution_environment
      )
    }

    result <- tag_replicate_events(
      if (!is_null(rep_seed)) {
        withr::with_seed(rep_seed, run_one())
      } else {
        run_one()
      },
      replicate = r
    )

    df <- as.data.frame(result)
    df$.replicate <- rep.int(r, nrow(df))
    results[[i]] <- df
  }

  combined <- do.call(rbind, results)
  combined$.sample_id <- seq_len(nrow(combined))

  # Derive metadata from inputs, not from last loop iteration.
  the_design <- get_design(sample)
  already_executed <- get_stages_executed(sample)
  cont_stages <- schedule$stages

  # Continued pools hang off each replicate's realized parents, so the
  # continued stages are replicate-specific by construction: the
  # shareable manifest is the prior one, explicitly marked partial.
  digest <- NULL
  if (!identical(frame_digest, "none")) {
    prior <- get_frame_digest(sample)
    if (!is_null(prior) && !identical(prior$status, "invalidated")) {
      digest <- prior
      digest$status <- "partial"
    }
  }

  new_tbl_sample(
    data = combined,
    design = the_design,
    stages_executed = c(already_executed, cont_stages),
    seed = seed,
    metadata = list(
      n_selected = nrow(combined),
      executed_at = Sys.time(),
      frame_digest = digest,
      reps = length(rep_ids),
      replicate_seeds = if (!is_null(seed)) {
        seed + seq_along(rep_ids) - 1L
      } else {
        NULL
      },
      replicate_rows = setNames(
        vapply(results, nrow, integer(1)),
        as.character(rep_ids)
      ),
      continued_from = attr(sample, "metadata"),
      prev_phase = attr(sample, "metadata")$prev_phase,
      execution_environment = execution_environment,
      frame_schedule = schedule_record(schedule),
      integrity = {
        integrity <- sample_integrity_record(
          combined,
          the_design,
          c(already_executed, cont_stages)
        )
        integrity$replicate_hashes <- replicate_integrity_hashes(
          combined,
          integrity$cols,
          rep_ids
        )
        integrity
      }
    )
  )
}

#' One replicate's slice of a replicated previous-phase sample
#'
#' Restores the `tbl_sample` class so `prepare_multiphase_frame()` recognizes
#' it as a phase rather than an ordinary frame.
#' @noRd
replicate_phase_source <- function(source_sample, replicate) {
  sub <- source_sample[source_sample$.replicate == replicate, ]
  sub$.replicate <- NULL
  new_tbl_sample(
    data = sub,
    design = get_design(source_sample),
    stages_executed = get_stages_executed(source_sample),
    seed = attr(source_sample, "seed"),
    metadata = attr(source_sample, "metadata")
  )
}

#' Execute a design on replicated multi-phase frame(s)
#'
#' When one or more replicated tbl_samples are passed as frames to a new
#' sampling_design, each replicate must be sampled independently. Non-replicated
#' frames are left unchanged across replicates.
#' @noRd
execute_replicated_multiphase <- function(
  design,
  schedule,
  seed,
  execution_environment,
  call = caller_env()
) {
  # Only the first supplied frame may be a previous phase, so there is exactly
  # one replicated source. The schedule may point several stages at it. Those
  # entries are aliases of that one sample, not independent frames.
  supplied <- vector("list", schedule$n_supplied)
  for (entry in schedule$entries) {
    supplied[[entry$frame_index]] <- entry$frame
  }
  source_sample <- supplied[[1]]

  rep_ids <- sort(unique(source_sample$.replicate))

  # Each replicate carries its own population, so a phase key can be
  # unambiguous in one and ambiguous in another. Validating inside the loop
  # would let earlier replicates draw before a later one failed, so every
  # replicate-specific source is checked first.
  replicate_frames <- lapply(rep_ids, function(r) {
    replicate_phase_source(source_sample, r)
  })
  prev_design <- get_design(source_sample)
  phase_link_vars <- phase_link_vars_of(list(
    design = prev_design,
    stages = get_stages_executed(source_sample),
    sample = source_sample
  ))
  for (i in seq_along(rep_ids)) {
    rep_supplied <- supplied
    rep_supplied[[1]] <- replicate_frames[[i]]
    check_phase_key_invariance(
      schedule_swap_frames(schedule, rep_supplied),
      design,
      phase_link_vars,
      list(
        design = prev_design,
        stages = get_stages_executed(source_sample),
        sample = replicate_frames[[i]]
      ),
      call = call
    )
  }

  results <- vector("list", length(rep_ids))

  for (i in seq_along(rep_ids)) {
    r <- rep_ids[i]

    # Prevalidated above. The schedule remaps this one subset to every stage
    # that draws on it.
    rep_frames <- supplied
    rep_frames[[1]] <- replicate_frames[[i]]

    rep_seed <- if (!is_null(seed)) seed + i - 1L else NULL

    run_one <- function() {
      execute_design(
        design,
        schedule_swap_frames(schedule, rep_frames),
        rep_seed,
        panels = NULL,
        execution_environment = execution_environment,
        frame_digest = "none"
      )
    }

    result <- tag_replicate_events(
      if (!is_null(rep_seed)) {
        withr::with_seed(rep_seed, run_one())
      } else {
        run_one()
      },
      replicate = r
    )

    df <- as.data.frame(result)
    df$.replicate <- rep.int(r, nrow(df))
    results[[i]] <- df
  }

  combined <- do.call(rbind, results)
  combined$.sample_id <- seq_len(nrow(combined))

  the_stages <- schedule$stages

  new_tbl_sample(
    data = combined,
    design = design,
    stages_executed = the_stages,
    seed = seed,
    metadata = list(
      n_selected = nrow(combined),
      executed_at = Sys.time(),
      reps = length(rep_ids),
      replicate_seeds = if (!is_null(seed)) {
        seed + seq_along(rep_ids) - 1L
      } else {
        NULL
      },
      replicate_rows = setNames(
        vapply(results, nrow, integer(1)),
        as.character(rep_ids)
      ),
      # The phase this sample descends from is the whole replicated phase-1
      # sample, not the subset the final replicate happened to use. Taking it
      # from the last loop result recorded 1/r of the phase, stripped of
      # .replicate, while claiming r replicates.
      prev_phase = list(
        design = get_design(source_sample),
        stages = get_stages_executed(source_sample),
        sample = source_sample
      ),
      execution_environment = execution_environment,
      frame_schedule = schedule_record(schedule),
      integrity = {
        integrity <- sample_integrity_record(combined, design, the_stages)
        integrity$replicate_hashes <- replicate_integrity_hashes(
          combined,
          integrity$cols,
          rep_ids
        )
        integrity
      }
    )
  )
}

#' Run one stage and label anything it reports with the stage index
#'
#' A thin wrapper so the stage body stays one expression. Selection leaves
#' signal per-pool diagnostics that only make sense aggregated, and this is the
#' innermost frame that knows which stage they came from.
#' @noRd
execute_single_stage <- function(
  frame,
  stage_spec,
  stage_num,
  previous_sample,
  previous_stage_spec = NULL,
  is_final_stage = FALSE,
  all_prior_cluster_vars = character(0),
  trace_mode = "full"
) {
  collect_stage_events(
    execute_single_stage_impl(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_num,
      previous_sample = previous_sample,
      previous_stage_spec = previous_stage_spec,
      is_final_stage = is_final_stage,
      all_prior_cluster_vars = all_prior_cluster_vars,
      trace_mode = trace_mode
    ),
    stage = stage_num
  )
}

#' @noRd
execute_single_stage_impl <- function(
  frame,
  stage_spec,
  stage_num,
  previous_sample,
  previous_stage_spec = NULL,
  is_final_stage = FALSE,
  all_prior_cluster_vars = character(0),
  trace_mode = "full"
) {
  strata_spec <- stage_spec$strata
  cluster_spec <- stage_spec$clusters
  draw_spec <- stage_spec$draw_spec

  validate_frame_vars(frame, stage_spec)

  if (!is_null(cluster_spec)) {
    if (
      !is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)
    ) {
      full_parent_vars <- unique(c(
        all_prior_cluster_vars,
        previous_stage_spec$clusters$vars
      ))
      split_vars <- full_parent_vars

      if (!is_null(previous_sample)) {
        attach <- attach_draw_assignments(
          frame,
          previous_sample,
          full_parent_vars
        )
        frame <- attach$frame
        split_vars <- attach$split_vars
      }

      split <- split_row_indices(frame, split_vars)
      indices_list <- split$indices
      # The parent here is the previous stage's cluster, so events raised
      # under it need the same qualification the within-cluster loop applies.
      parent_labels <- key_labels(split$key_df, split_vars)

      results_list <- lapply(seq_along(indices_list), function(i) {
        data <- frame[indices_list[[i]], , drop = FALSE]
        qualify_pool_events(
          sample_clusters(
            data,
            strata_spec,
            cluster_spec,
            draw_spec,
            trace_mode = trace_mode
          ),
          parent_labels[[i]]
        )
      })
      result <- bind_rows(lapply(results_list, function(r) r$sample))
      if (nrow(result) > 0) {
        result$.sample_id <- seq_len(nrow(result))
      }
      stage_trace <- if (identical(trace_mode, "none")) {
        NULL
      } else {
        trace_split(
          by = split_vars,
          groups = lapply(seq_along(indices_list), function(i) {
            trace_group(
              key = split$keys[[i]],
              keys = NULL,
              rows = indices_list[[i]],
              node = results_list[[i]]$trace
            )
          })
        )
      }
    } else {
      res <- sample_clusters(
        frame,
        strata_spec,
        cluster_spec,
        draw_spec,
        trace_mode = trace_mode
      )
      result <- res$sample
      stage_trace <- res$trace
    }

    if (is_final_stage) {
      cluster_vars <- cluster_spec$vars
      draw_k_cols <- grep("^\\.draw_\\d+$", names(result), value = TRUE)
      draw_k_cols <- intersect(draw_k_cols, names(frame))
      ancestor_in_frame <- intersect(all_prior_cluster_vars, names(frame))
      by_vars <- unique(c(ancestor_in_frame, cluster_vars, draw_k_cols))
      join_cols <- c(by_vars, ".weight", ".fpc", ".sample_id")
      if (".draw" %in% names(result)) {
        join_cols <- c(join_cols, ".draw")
      }
      if (".certainty" %in% names(result)) {
        join_cols <- c(join_cols, ".certainty")
      }
      cluster_data <- result[, join_cols, drop = FALSE]
      result <- dplyr::inner_join(
        frame,
        cluster_data,
        by = by_vars,
        relationship = "many-to-many"
      )
    }
  } else if (
    !is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)
  ) {
    full_parent_vars <- unique(c(
      all_prior_cluster_vars,
      previous_stage_spec$clusters$vars
    ))
    split_vars <- full_parent_vars

    if (!is_null(previous_sample)) {
      attach <- attach_draw_assignments(
        frame,
        previous_sample,
        full_parent_vars
      )
      frame <- attach$frame
      split_vars <- attach$split_vars
    }

    res <- sample_within_clusters(
      frame,
      strata_spec,
      draw_spec,
      split_vars,
      trace_mode = trace_mode
    )
    result <- res$sample
    stage_trace <- res$trace
  } else {
    res <- sample_units(
      frame,
      strata_spec,
      draw_spec,
      trace_mode = trace_mode
    )
    result <- res$sample
    stage_trace <- res$trace
  }

  result$.stage <- rep.int(stage_num, nrow(result))

  stage_weight_col <- paste0(".weight_", stage_num)
  result[[stage_weight_col]] <- result$.weight

  stage_fpc_col <- paste0(".fpc_", stage_num)
  result[[stage_fpc_col]] <- result$.fpc
  result$.fpc <- NULL

  if (".draw" %in% names(result)) {
    stage_draw_col <- paste0(".draw_", stage_num)
    result[[stage_draw_col]] <- result$.draw
    result$.draw <- NULL
  }

  if (".certainty" %in% names(result)) {
    stage_cert_col <- paste0(".certainty_", stage_num)
    result[[stage_cert_col]] <- result$.certainty
    result$.certainty <- NULL
  }

  if (!is_null(previous_sample) && ".weight" %in% names(previous_sample)) {
    result <- compound_stage_weights(
      result,
      previous_sample,
      parent_vars = all_prior_cluster_vars
    )
  }

  # Materialized outputs should always expose a row-unique sample id,
  # including clustered stages that are expanded back to all rows.
  result$.sample_id <- seq_len(nrow(result))
  # `frame` is returned because draw-assignment attachment can rebuild
  # it, and the trace's row indices refer to the frame actually used.
  list(sample = result, trace = stage_trace, frame = frame)
}

#' Detect columns to carry forward from a previous stage
#' @noRd
find_carry_forward_cols <- function(previous_sample) {
  nms <- names(previous_sample)
  c(
    grep("^\\.weight_\\d+$", nms, value = TRUE),
    grep("^\\.draw_\\d+$", nms, value = TRUE),
    grep("^\\.fpc_\\d+$", nms, value = TRUE),
    grep("^\\.certainty_\\d+$", nms, value = TRUE),
    intersect(".panel", nms),
    intersect("._prev_phase_weight", nms)
  )
}

#' Compound weights by joining on shared variables
#' @noRd
compound_by_join <- function(result, previous_sample, join_vars, carry_cols) {
  carry_cols_to_select <- setdiff(carry_cols, join_vars)

  # `.prev_weight` is a name a frame may legitimately carry. Colliding with it
  # surfaced as an internal dplyr error.
  prev_weight <- free_column_name(result, ".prev_weight")

  prev_data <- previous_sample |>
    distinct(across(all_of(join_vars)), .keep_all = TRUE) |>
    select(
      all_of(join_vars),
      all_of(carry_cols_to_select),
      all_of(".weight")
    )
  names(prev_data)[names(prev_data) == ".weight"] <- prev_weight

  n_before <- nrow(result)
  out <- left_join(result, prev_data, by = join_vars)

  # Compounding reads one parent row per selected row. Losing that match would
  # silently drop the earlier stage's weight from the product.
  if (nrow(out) != n_before || anyNA(out[[prev_weight]])) {
    cli_abort(
      c(
        "Stage weights could not be compounded onto every selected row.",
        "i" = "Joined on {.field {join_vars}}."
      ),
      call = NULL
    )
  }

  out[[".weight"]] <- out[[".weight"]] * out[[prev_weight]]
  out[[prev_weight]] <- NULL
  out
}

#' Compound weights by broadcasting first-row values
#' @noRd
compound_broadcast <- function(result, previous_sample, carry_cols) {
  for (col in carry_cols) {
    result[[col]] <- previous_sample[[col]][1]
  }
  result$.weight <- result$.weight * previous_sample$.weight[1]
  result
}

#' Compound current-stage weights with previous-stage weights
#' @noRd
compound_stage_weights <- function(
  result,
  previous_sample,
  parent_vars = character(0)
) {
  carry_cols <- find_carry_forward_cols(previous_sample)
  # A repeated/shared phase frame may already put this transient column on
  # the current result. Do not join a second copy with .x/.y suffixes.
  if ("._prev_phase_weight" %in% names(result)) {
    carry_cols <- setdiff(carry_cols, "._prev_phase_weight")
  }

  # The ancestry resolved by the transition, not a key re-derived here from
  # whichever columns happen to be shared.
  join_vars <- parent_vars
  prev_draw_cols <- grep("^\\.draw_\\d+$", names(previous_sample), value = TRUE)
  if (
    length(join_vars) > 0 &&
      length(prev_draw_cols) > 0 &&
      all(prev_draw_cols %in% names(result))
  ) {
    join_vars <- c(join_vars, prev_draw_cols)
  }

  if (length(join_vars) > 0) {
    compound_by_join(result, previous_sample, join_vars, carry_cols)
  } else {
    compound_broadcast(result, previous_sample, carry_cols)
  }
}

#' @noRd
attach_draw_assignments <- function(frame, previous_sample, cluster_vars_prev) {
  prev_draw_cols <- grep(
    "^\\.draw_\\d+$",
    names(previous_sample),
    value = TRUE
  )
  split_vars <- cluster_vars_prev
  if (length(prev_draw_cols) == 0) {
    return(list(frame = frame, split_vars = split_vars))
  }

  draw_assignments <- unique(
    previous_sample[, c(cluster_vars_prev, prev_draw_cols), drop = FALSE]
  )

  # `.row_id` is a legitimate column name a user may already have, so the
  # position marker must not assume it is free.
  pos <- free_column_name(frame, ".row_id")
  frame[[pos]] <- seq_len(nrow(frame))
  frame <- dplyr::left_join(
    frame,
    draw_assignments,
    by = cluster_vars_prev,
    relationship = "many-to-many"
  )
  frame <- frame[order(frame[[pos]]), , drop = FALSE]
  frame[[pos]] <- NULL

  split_vars <- c(cluster_vars_prev, prev_draw_cols)
  list(frame = frame, split_vars = split_vars)
}

#' @noRd
prepare_multiphase_frame <- function(frame) {
  if (!is_tbl_sample(frame)) {
    return(list(frame = frame, prev_phase = NULL))
  }
  check_weight_contract_execute(frame, "execute")

  prev_phase_sample <- frame
  prev_phase <- list(
    design = get_design(frame),
    stages = get_stages_executed(frame),
    sample = prev_phase_sample
  )

  frame$._prev_phase_weight <- frame$.weight

  internal <- samplyr_internal_cols(frame)
  frame[internal] <- NULL
  frame <- as.data.frame(frame)

  list(frame = frame, prev_phase = prev_phase)
}

#' @noRd
samplyr_internal_cols <- function(x) {
  # Strip sample metadata before a tbl_sample is reused as a frame.
  grep(samplyr_internal_col_pattern, names(x), value = TRUE)
}


#' @noRd
validate_design_complete <- function(design, call = rlang::caller_env()) {
  if (length(design$stages) == 0) {
    cli_abort("Design has no stages defined", call = call)
  }

  for (i in seq_along(design$stages)) {
    stage <- design$stages[[i]]
    if (is_null(stage$draw_spec)) {
      label <- stage$label %||% paste("Stage", i)
      cli_abort("{.val {label}} is incomplete: missing {.fn draw}", call = call)
    }
    # draw() refuses these at design time. A design restored from a
    # file bypasses draw(), so execution re-checks.
    if (identical(stage$draw_spec$method_probabilities, "unknown")) {
      abort_unknown_probabilities(stage$draw_spec$method, call = call)
    }
  }

  balanced_stages <- which(vapply(
    design$stages,
    function(s) {
      is_balanced_method(s$draw_spec)
    },
    logical(1)
  ))
  if (length(balanced_stages) > 2) {
    cli_abort(
      c(
        "Balanced sampling ({.val balanced}) is supported for at most 2 stages.",
        "i" = "Found {.val balanced} at stages {balanced_stages}."
      ),
      call = call
    )
  }

  invisible(TRUE)
}

#' @noRd
validate_frame_vars <- function(frame, stage_spec, call = rlang::caller_env()) {
  if (nrow(frame) == 0) {
    cli_abort("Frame has 0 rows", call = call)
  }

  # One definition, shared with the pre-RNG preflight in
  # stage_frame_schedule(), so the two cannot disagree about what a stage
  # needs.
  required_vars <- stage_required_vars(stage_spec)
  strata_vars <- stage_spec$strata$vars
  cluster_vars <- stage_spec$clusters$vars
  mos_var <- stage_spec$draw_spec$mos
  prn_var <- stage_spec$draw_spec$prn

  missing <- setdiff(required_vars, names(frame))
  if (length(missing) > 0) {
    cli_abort(
      c(
        "Required {cli::qty(length(missing))} variable{?s} not found in frame:",
        "x" = "{.val {missing}}"
      ),
      call = call
    )
  }

  if (!is_null(strata_vars)) {
    na_strata <- Filter(function(v) anyNA(frame[[v]]), strata_vars)
    if (length(na_strata) > 0) {
      cli_abort(
        "Stratification variable{?s} {.var {na_strata}} contain{?s/} NA values",
        call = call
      )
    }
  }

  if (!is_null(cluster_vars)) {
    na_clusters <- Filter(function(v) anyNA(frame[[v]]), cluster_vars)
    if (length(na_clusters) > 0) {
      cli_abort(
        "Cluster variable{?s} {.var {na_clusters}} contain{?s/} NA values",
        call = call
      )
    }
  }

  if (!is_null(mos_var)) {
    mos_vals <- frame[[mos_var]]
    if (!is.numeric(mos_vals)) {
      cli_abort(
        "MOS variable {.var {mos_var}} must be numeric, not {.cls {class(mos_vals)[[1]]}}",
        call = call
      )
    }
    if (anyNA(mos_vals)) {
      cli_abort(
        "MOS variable {.var {mos_var}} contains NA values",
        call = call
      )
    }
    if (any(mos_vals < 0)) {
      cli_abort(
        "MOS variable {.var {mos_var}} contains negative values",
        call = call
      )
    }
    # Only warn about zero MOS when there is a non-zero remainder to
    # sample from. An all-zero MOS is handled by the harder PPS error
    # raised later in draw_sample() / draw_pps_method().
    if (any(mos_vals == 0) && sum(mos_vals) > 0) {
      n_zero <- sum(mos_vals == 0)
      cli_warn(c(
        "MOS variable {.var {mos_var}} contains {n_zero} zero value{?s}.",
        "i" = "Units with MOS = 0 have zero inclusion probability and will never be selected.",
        "i" = "Consider removing them from the frame or assigning a positive measure of size."
      ))
    }
  }

  if (!is_null(prn_var)) {
    prn_vals <- frame[[prn_var]]
    if (!is.numeric(prn_vals)) {
      cli_abort(
        "PRN variable {.var {prn_var}} must be numeric, not {.cls {class(prn_vals)[[1]]}}",
        call = call
      )
    }
    if (anyNA(prn_vals)) {
      cli_abort(
        "PRN variable {.var {prn_var}} contains NA values",
        call = call
      )
    }
    if (any(prn_vals <= 0) || any(prn_vals >= 1)) {
      cli_abort(
        "PRN variable {.var {prn_var}} must have values in the open interval (0, 1)",
        call = call
      )
    }
  }

  aux_vars <- stage_spec$draw_spec$aux
  if (!is_null(aux_vars)) {
    missing_aux <- setdiff(aux_vars, names(frame))
    if (length(missing_aux) > 0) {
      cli_abort(
        c(
          "Required {cli::qty(length(missing_aux))} auxiliary variable{?s} not found in frame:",
          "x" = "{.val {missing_aux}}"
        ),
        call = call
      )
    }
    for (av in aux_vars) {
      aux_vals <- frame[[av]]
      if (!is.numeric(aux_vals)) {
        cli_abort(
          "Auxiliary variable {.var {av}} must be numeric, not {.cls {class(aux_vals)[[1]]}}",
          call = call
        )
      }
      if (anyNA(aux_vals)) {
        cli_abort(
          "Auxiliary variable {.var {av}} contains NA values",
          call = call
        )
      }
    }
  }

  bound_vars <- stage_spec$draw_spec$bounds
  if (!is_null(bound_vars)) {
    missing_bounds <- setdiff(bound_vars, names(frame))
    if (length(missing_bounds) > 0) {
      cli_abort(
        c(
          "Required count-bound variable{?s} not found in frame:",
          "x" = "{.val {missing_bounds}}"
        ),
        call = call
      )
    }
    for (var in bound_vars) {
      if (anyNA(frame[[var]])) {
        cli_abort(
          "Count-bound variable {.var {var}} contains NA values",
          call = call
        )
      }
    }
  }

  spread_vars <- stage_spec$draw_spec$spread
  if (!is_null(spread_vars)) {
    missing_spread <- setdiff(spread_vars, names(frame))
    if (length(missing_spread) > 0) {
      cli_abort(
        c(
          "Required spatial coordinate variable{?s} not found in frame:",
          "x" = "{.val {missing_spread}}"
        ),
        call = call
      )
    }
    for (var in spread_vars) {
      values <- frame[[var]]
      if (!is.numeric(values) || anyNA(values) || any(!is.finite(values))) {
        cli_abort(
          "Spatial coordinate variable {.var {var}} must be finite numeric with no missing values",
          call = call
        )
      }
    }
  }

  control_vars <- extract_control_vars(stage_spec$draw_spec$control)
  if (length(control_vars) > 0) {
    missing_control <- setdiff(control_vars, names(frame))
    if (length(missing_control) > 0) {
      cli_abort(
        c(
          "Control {cli::qty(missing_control)}variable{?s} not found in frame:",
          "x" = "{.val {missing_control}}",
          "i" = "Control sorting is applied within strata or clusters, so control variables must exist in the frame."
        ),
        call = call
      )
    }
  }

  invisible(TRUE)
}

#' @noRd
extract_control_vars <- function(control_quos) {
  if (is_null(control_quos) || length(control_quos) == 0) {
    return(character(0))
  }

  known_fns <- c("c", "desc", "serp")
  vars <- unique(unlist(lapply(control_quos, function(q) {
    expr <- rlang::quo_get_expr(q)
    names <- all.vars(expr)
    setdiff(names, known_fns)
  })))

  vars[vars != "."]
}
