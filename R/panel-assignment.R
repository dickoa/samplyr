#' Panel assignment and rotation schedules
#'
#' How [execute()] partitions a sample into panels with `panels`, where
#' `panel_stage` puts the assignment, what a rotation schedule declares, and
#' how `execute(master, wave = t)` weights the wave it materializes.
#'
#' ## Periods and rotation groups
#'
#' `panels` takes three forms.
#'
#' | `panels =` | Declares | Waves | A certainty unit is in |
#' |---|---|---|---|
#' | `12` | rotation groups | declared later | every wave |
#' | a schedule | rotation groups | as scheduled | every wave |
#' | `periods(12)` | periods | one per period | one period |
#'
#' Rotation groups are for a sample interviewed again, where a unit selected
#' with certainty stays in every wave. [periods()] is for a sample spread
#' over fieldwork periods and visited once, such as the months of a yearly
#' survey, where a certainty unit belongs to one period like any other. A
#' count that keeps certainty units apart says so with
#' `samplyr_message_panel_certainty`.
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
#' Under a count or a schedule, certainty units are labeled from their own
#' pools and consume no rotating quota. Under [periods()] they are assigned
#' with the other units of their stratum, so each belongs to one period and
#' the periods stay even. A certainty unit is in the sample at every occasion, since a
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
#' retains the master as its first phase. [as_svydesign()] exports it by
#' ultimate clusters, or through [survey::twophase()] with the activation as
#' the second phase when `method` is given. Its
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
#' ## Windows of several waves
#'
#' `execute(master, wave = 1:3)` pools several waves into one sample, such
#' as a quarter written as three monthly waves, a moving window, or a year
#' of quarters. Each row carries the wave it was observed at in `.wave`, and
#' totals estimate the average over the window of each wave's total. The
#' schedule decides between two forms.
#'
#' When no panel is active at more than one wave of the window, as for the
#' months of a quarter where each unit keeps its own month, the window is
#' one activation over the union of its panels. Each unit has one row, at
#' its wave, and its weight uses the block's quota for the union over the
#' block size, so a stratum's weights add up to its size. When every block
#' fills the window's waves evenly, this equals the average of the waves.
#' When a partial block fills them unevenly, the two differ in a given
#' sample, and in a simulation both were unbiased for the average and
#' about as variable.
#'
#' When some panel is active at more than one wave, as in a year of quarters
#' under a rotation that keeps units for several quarters, each unit keeps
#' a row for every wave it is active, weighted by that wave's weight over
#' the number of waves. A unit seen at two waves then counts at both.
#' Permanent units are active at every wave and always take this form.
#'
#' A window is exported by ultimate clusters, with a unit's rows in one
#' cluster ([as_svydesign()]). It needs a master whose stages are all
#' drawn. It has no receipt of its own, so [write_design()] refuses it and
#' the master's receipt gives it again, and [stack_waves()] takes single
#' waves only.
#'
#' ## Tenures
#'
#' In most rotating household surveys the rotation group is a set of primary
#' units, and a unit's households are drawn when its group enters, from a
#' listing updated then, and kept for a run of visits, a tenure. When the
#' tenure ends the primary unit either leaves or gets new households from a
#' new listing. A schedule states this with an optional `visit` column, the
#' interview number within the tenure, `NA` where a panel is inactive. A
#' tenure opens where `visit` is 1. Without the column each panel has one
#' tenure, opened at its first active wave. A materialized wave carries
#' `.visit`, the number agencies publish to measure rotation group bias.
#'
#' The stages below the panels are then drawn where tenures open, with
#' `execute(master, listing, wave = t)` (see [execute()]), from the full
#' listing of the primary units. A new tenure in a primary unit leaves out
#' the units its earlier tenures drew, as rotating surveys do, and draws from
#' the rest. Each of the `M` units of the listing then has the chance `n /
#' M` of a single draw, so the stage's weight and population count are
#' those of the whole listing, and every tenure estimates the whole
#' population. That holds exactly for simple random sampling on a listing
#' that only loses the units drawn before, and for the first-order chances
#' of systematic sampling. Units built since the earlier tenures are
#' weighted the same way, which approximates their larger chance.
#'
#' A schedule with several tenures per panel therefore needs the stage
#' below the panels to name its units with [cluster_by()] and draw them by
#' `"srswor"` or `"systematic"`, and refuses drawing that stage up front,
#' since every tenure would then hold the same units
#' (`samplyr_error_tenure_stages`). A listing already cut of the units
#' drawn before is refused (`samplyr_error_tenure_listing`), since its
#' weights could not count them back. Permanent units are active at every
#' wave outside the rotation, keep the units drawn under them at the first
#' wave drawn, and have no visit.
#'
#' A wave reads each active panel's current tenure, and is exported by
#' ultimate clusters from its own rows ([as_svydesign()]) as soon as those
#' tenures are drawn. The master is never complete when a panel has several
#' tenures, as rotating surveys never complete. Its record is version 5,
#' which earlier builds refuse rather than read as one tenure per panel.
#' Its frame digest records every tenure drawn so far, keyed by the wave
#' each opened at, so [frame_summary()] reports the stages below the panels
#' by tenure and [replay_design()] checks every tenure's draw. That digest
#' is schema version 3, which earlier builds refuse for the same reason.
#'
#' The digest of a later tenure records the draw as it was made. A pool's
#' `N` counts the units left after the exclusion, and its chances are
#' those of a draw from them. Its `n_excluded` counts the units left out,
#' so `N + n_excluded` is the listing that the weights and `.fpc_k`
#' count, and `chance * N / (N + n_excluded)` is the chance a weight
#' inverts. [summary()] shows both, as in `N_h 8 of 10 listed`.
#'
#' ## Pools too small for a schedule
#'
#' Periods activate one period per wave, so a stratum with fewer units than
#' periods would leave a period without any of its units. It is refused,
#' and `small_pool = "permanent"` is refused with [periods()], since a unit
#' active in every period is not visited once.
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
#' @name panel_assignment
NULL
