#' Panel assignment and rotation schedules
#'
#' How [execute()] partitions a sample into panels with `panels`, where
#' `panel_stage` puts the assignment, what a rotation schedule declares, and
#' how `execute(master, wave = t)` weights the wave it materializes.
#'
#' ## How units are assigned
#'
#' Assignment is randomized with fixed quotas. Within each selection stratum
#' of the assignment stage the assignment units are ordered, cut into
#' consecutive blocks of `2 * panels`, given a fixed quota per panel inside
#' each block, and permuted within their block. Every unit therefore carries
#' each panel with probability `1 / panels`, and panel sizes within a pool
#' differ by at most one. It is a partition of the selected sample, not an
#' additional probability-sampling phase. It changes no inclusion probability
#' and no `.weight`, so the combined sample is analyzed with the stored
#' weights.
#'
#' Blocking is what preserves order. Units adjacent in the `control` order of
#' the assignment stage's `draw()` fall in the same block, so every panel
#' inherits the same spread over that order. A pool holding fewer than
#' `2 * panels` units is a single block, still assigned, with no block-level
#' order left to preserve.
#'
#' Panels are assigned once. `.panel` is carried forward by a stage
#' continuation, and redeclaring `panels` on a sample that already carries an
#' assignment is an error.
#'
#' ## Assignment below the first stage
#'
#' Panels are assigned at stage 1 by default, and every unit below inherits
#' its ancestor's panel, so whole primary units rotate. `panel_stage` moves
#' the assignment to a lower stage, which retains the primary units and
#' rotates the units inside them: the address-panel design, in which selected
#' areas stay in the survey and households rotate within them. A pool is then
#' the assignment stage's strata inside each realized parent and never
#' crosses a parent, so pools are smaller and `small_pool` matters more.
#'
#' Under a with-replacement assignment stage the assignment unit is the
#' realized draw, the unit the estimator uses, so one population unit
#' selected twice may take two different panels. A parent selected twice
#' likewise gives two separate populations of households, and a household
#' reached under both hits is assigned once for each.
#'
#' Assigning below the first stage is not a default and should not be treated
#' as one. Holding the parent fixed while its members rotate can bias
#' cross-sectional estimates over time, and a unit that cannot move between
#' parents is balanced for net change but not for gross change.
#'
#' ## Certainty units
#'
#' Certainty units are labelled from their own pools and consume no rotating
#' quota. A certainty unit is in the sample at every occasion, since a
#' schedule that rotated it out would drop the very units the certainty
#' stratum exists to enumerate. Certainty counts only at the assignment
#' stage, in both directions: a certainty primary unit does not make the
#' units below it permanent when `panel_stage` names a lower stage, and a
#' certainty selection below the assignment stage does not keep its
#' assignment unit in every wave. A unit selected with probability one inside
#' a rotating parent is absent from the waves that parent sits out.
#'
#' ## Rotation schedules and waves
#'
#' Passing a schedule to `panels` instead of a count declares which panels are
#' active at which occasion, and lets `execute(master, wave = t)` materialize
#' one of them. A schedule is a data frame with an integer `panel` column, an
#' integer `wave` column and an optional logical `active` column, and a
#' combination left out is inactive:
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
#' The schedule is read at the master draw, because the block size follows
#' from it. A schedule whose leanest wave activates `r` of the `k` panels
#' blocks at `k * ceiling(2 / r)` rather than at the worst case `2k`, which
#' keeps more of the assignment order while still leaving two units per block
#' in the take.
#'
#' Materializing wave `t` is a probability subsample rather than a filter: a
#' two-phase sample whose second phase is the activation, with the blocks as
#' phase-2 strata and their frozen quotas as phase-2 population counts. It
#' multiplies `.weight` by the inverse of the activation probability. That
#' probability is the block's frozen quota for the active panels over the
#' block size, so it is exact rather than nominal, and it is generally not
#' `k / r`. Permanent certainty units are activated at every wave with
#' probability one and their weights are untouched.
#'
#' A materialized wave is a sample in its own right, with its own integrity
#' record, so [frame_summary()] and the weight diagnostics work on it. It
#' retains the master as its first phase, and [as_svydesign()] exports it
#' through [survey::twophase()] with the activation as the second phase. Its
#' receipt records the master's execution, so [replay_design()] rebuilds the
#' master and materializes the wave again.
#'
#' The schedule states which groups are live when. It does not replenish the
#' sample: every panel comes from the frame vintage the master was drawn from,
#' and steady-state replenishment is a fresh `execute()` against a later
#' frame. For an `svyplan_schedule`, `execute()` takes the startup activity
#' and checks its panel parameters before assignment. A gradual launch has
#' one whole startup cohort and needs no partition.
#'
#' ## Pools too small for a schedule
#'
#' A pool of `m` units leaves `panels - m` panels empty, so a wave activating
#' `r` of them selects nothing from that pool when `m <= panels - r`. Those
#' units would have inclusion probability zero in that wave rather than a
#' small weight, which biases the wave's estimator. `small_pool = "error"`,
#' the default, refuses such an assignment before any panel is drawn and names
#' the pools. `"permanent"` activates them at every wave with probability one
#' and warns with `samplyr_warning_panel_small_pool`, which is exact but
#' changes the operational design: wave sizes, overlap and repeated
#' interviewing all increase. It matters only with a schedule, since a panel
#' count declares no wave to protect, and it governs positivity only. A pool
#' with a single active unit is still assigned, and is marked as carrying no
#' within-block variance estimate. An `svyplan_schedule` refuses
#' `"permanent"`, and selection-certainty units
#' (`samplyr_error_plan_certainty`), because its overlap describes a fully
#' rotating life.
#'
#' ## Weights of a subset of panels
#'
#' Weights are not adjusted for panel membership. They reflect the full-sample
#' inclusion probability and are valid for the combined sample. A subset of
#' the panels is a simple random subsample without replacement within each
#' block, but its conditional probability is the block's realized quota over
#' the block size, not `1 / panels`, so multiplying one panel's weights by
#' `panels` is not generally valid for population inference. The block sizes
#' and realized quotas are recorded with the sample and written to the design
#' file by [write_design()], because they are what such a subset has to be
#' computed against. [joint_expectation()] with `waves` reads how two
#' occasions overlap from the same record.
#'
#' @seealso [execute()], [rotation_program()] for cohort programs,
#'   [stack_waves()] for stacking materialized waves.
#'
#' @name panel-assignment
NULL
