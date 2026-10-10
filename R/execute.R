#' Conditions raised during execution
#'
#' [execute()] signals five conditions when a *selection* cannot be realized
#' as written, because a pool held fewer units than the stage asked for. Each
#' is a classed condition carrying a `payload`, so it can be caught and
#' inspected rather than only read. They are reported once per stage per
#' distinct finding, not once per capped pool and not once per replicate.

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
#' One more is reported the same way, once per stage with a `payload`,
#' although nothing was capped. `samplyr_message_singleton_pool` names the
#' strata of a fixed-size design without replacement that draw a single unit
#' outside certainty. Such a stratum gives no variance estimate of its own:
#' survey stops on it unless strata are collapsed, and the export warns
#' again. It is a message because one unit per stratum is sometimes the
#' design. Its payload carries `pool_keys` and `n_singleton`, the number of
#' such strata, each counted once in every parent it appears in.
#'
#' A selected unit that has no rows in the next stage's frame is reported the
#' same way under that stage's `on_empty = "warn"`:
#' `samplyr_warning_empty_parent`, with `pool_keys` and `n_empty`. See
#' `on_empty` in [draw()].
#'
#' These are not every condition `execute()` raises. Panel assignment has one
#' of its own, `samplyr_warning_panel_small_pool`, documented with
#' `small_pool`: it is about a pool too small to *rotate* rather than too
#' small to draw from, fires once for the assignment rather than per stage,
#' and carries no `payload`.
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
#' @param ... Data frame(s) to sample from: one frame for a single-stage
#'   design or a shared hierarchy, or one frame per stage in stage order. A
#'   `tbl_sample` passed here while `.data` is a new `sampling_design` starts
#'   a new sampling phase rather than continuing that sample's stages.
#'   Ordinary input frames must have unique column names and must not use
#'   columns reserved for execution output, such as `.weight`, `.sample_id`,
#'   `.stage`, `.weight_k`, or `.fpc_k` for stage `k`. Frames are matched by
#'   position, so a name given here is a label. A label resembling an
#'   argument below (`seedd`, or the singular `stage`, `rep`, `panel`) is
#'   refused, because those arguments follow `...` and are matched exactly.
#'
#'   A single unnamed list of data frames is the same call, for code that
#'   already holds the frames, such as a loop over registers or
#'   [replay_design()]. Mixing it with separate frames is refused. Names
#'   inside the list are frame labels, so a misspelled argument must be
#'   written outside it to be reported as one. See [frame-input-grammar].
#' @param stages Integer vector of the stages to execute, or `NULL`
#'   (default) for all remaining stages. From a `sampling_design` it must
#'   start at stage 1, so an operational workflow can stop after its first
#'   contiguous batch of stages. From a partial `tbl_sample` it must start at
#'   the next unexecuted stage.
#' @param seed Integer random seed, between `-.Machine$integer.max` and
#'   `.Machine$integer.max`. A given seed leaves the session's random number
#'   stream untouched. With `seed = NULL`, selection draws from the session
#'   stream and advances it, and [replay_design()] refuses the resulting
#'   sample because its receipt cannot reproduce it.
#' @param panels Rotation groups (panels) to partition the sample into, as an
#'   integer count, a rotation schedule (a data frame with integer `panel`
#'   and `wave` columns and an optional logical `active` column), or an
#'   `svyplan_schedule` from [svyplan::design_schedule()]. The output gains a
#'   `.panel` column, and only a sample drawn with a schedule can be
#'   materialized by `wave`. Default `NULL` means no panels. Cannot be
#'   combined with `reps`, or redeclared on a sample that already carries an
#'   assignment. [panel-assignment] describes the assignment and the
#'   schedules.
#' @param panel_stage Stage whose selected units are assigned to panels, as a
#'   single stage number, or `NULL` (the default) for the first executed
#'   stage. Accepted only alongside `panels`, and the stage must be one the
#'   execution completes. Later stages inherit their ancestor's panel, so
#'   assigning below stage 1 rotates units inside parents that stay in the
#'   survey. [panel-assignment] explains what that changes, including how it
#'   can bias estimates over time.
#' @param small_pool What to do when a rotation schedule would leave a pool
#'   with no unit to activate at some wave. `"error"` (the default, reached by
#'   `NULL`) refuses the assignment, and `"permanent"` activates those units
#'   at every wave and warns with `samplyr_warning_panel_small_pool`.
#'   Meaningful only with a schedule. See [panel-assignment].
#' @param wave Integer wave of a scheduled master to materialize, or `NULL`
#'   (default). `execute(master, wave = t)` activates the panels the stored
#'   schedule declares active at `t` and compounds the exact activation
#'   factor into `.weight`. A wave selects units the master already assigned,
#'   under the policy the master froze, so it takes no frame and none of
#'   `seed`, `stages`, `panels`, `panel_stage`, `small_pool`, `reps` or
#'   `frame_digest`. Each is refused by name rather than ignored.
#' @param reps Integer number of independent replicate samples to draw (>= 2),
#'   or `NULL` (default) for a single sample. The replicates are stacked in
#'   one `tbl_sample` with a `.replicate` column (integer 1 through `reps`).
#'   Their seeds are drawn from `seed`, since consecutive seeds do not give
#'   independent draws in R, and recorded in the `replicate_seeds` element
#'   of the sample's metadata, so replicate `r` alone is
#'   `execute(design, frame, seed = replicate_seeds[r])`. Cannot be combined
#'   with `panels` or with stages that use permanent random numbers.
#'   `as_svrepdesign(type = "random_groups")` estimates variance from the
#'   spread of the replicates, for any selection method.
#' @param frame_digest Controls the frame digest, a compact execution
#'   manifest recorded with the sample and read by [frame_summary()]. The
#'   digest never affects selection, weights, or estimation. `"summary"`
#'   (default) records selection pools, resolved chances (exact for cluster
#'   stages, constant or quantile-compressed for element stages), and the
#'   selected-unit trace. `"full"` keeps exact per-unit chances for element
#'   stages too. `"none"` records no digest and skips trace construction for
#'   minimum execution overhead.
#'
#'   When one universe frame feeds every stage, later stages also record the
#'   pools their realization never reached, with chances resolved from the
#'   design (`chance_status = "design_resolved"`), which gives complete
#'   universe denominators without the frame. A stage continuation extends
#'   the digest of its input sample, and an input without a valid digest
#'   yields none. Replicates share one population structure with their own
#'   traces. A replicated multi-stage execution keeps only the stage prefix
#'   shared by all replicates, with status `"partial"`, as [frame_summary()]
#'   describes. Replicated multi-phase and replicated-continuation executions
#'   do not record a digest yet.
#'
#'   A digest is not anonymized. Its selection trace keeps the identifiers
#'   of realized units (frame-derived cluster keys or row indices), even
#'   under `"summary"`, so treat the sample and any serialized execution
#'   receipt as potentially confidential.
#'
#' @return A `tbl_sample`: a data frame subclass carrying the selected rows,
#'   the design that produced them, and generated columns recording the
#'   selection. Those are `.sample_id`, `.weight`, the per-stage `.weight_k`,
#'   `.fpc_k`, `.draw_k` and `.certainty_k`, `.zone_1` and `.pair_1` for a
#'   zoned certainty plan, and `.replicate` or `.panel` when `reps` or
#'   `panels` is used.
#'   [sample-columns] documents what each one holds.
#'
#' @details
#' ## Multi-stage with a single frame
#' For hierarchical data where all stages are in one frame, pass that one
#' frame. It must contain all clustering variables and represent the stage
#' hierarchy correctly. Lower-stage IDs may repeat across different parents,
#' because `samplyr` resolves them using the full ancestry from earlier
#' stages.
#'
#' ## Multi-stage with one frame per stage
#' When each stage has its own register, pass one frame per stage. The
#' number of frames schedules the stages: one frame is a shared hierarchy
#' covering all of them, one frame per stage gives each its own, and any
#' other count is `samplyr_error_frame_count`.
#'
#' Registers are supplied whole. Each is restricted to the units its parent
#' stage selected, and the variables earlier stages introduced are carried
#' onto it, so a lower register need not be pre-filtered or duplicate the
#' upper stages' stratification columns. A register may legitimately omit a
#' carried stratum, but one whose own copy of it disagrees is an error.
#'
#' Before any stage draws, every later stage is checked on all the units it
#' could reach, as [validate_frame()] checks them, so a defect in a unit the
#' seed happens to skip is still refused.
#'
#' This form, the single-hierarchy form, and the stage continuation below
#' run the same stage transition. Under one shared RNG stream, and with
#' `stages` given on every intermediate call, all three draw the same
#' sample.
#'
#' ## Partial execution (operational sampling)
#' `stages` executes only the stages named, returning a partial `tbl_sample`.
#' Fieldwork then produces the next stage's frame, and passing the partial
#' sample back as `.data` continues the same design. [frame-input-grammar]
#' states how a continuation reads its frame, when it asks for `stages`, and
#' how it differs from starting a new phase on a sample.
#'
#' ## Multi-phase sampling
#' `execute(new_design, previous_sample)` starts a new phase, with the
#' earlier sample as the frame of a new design rather than `.data` carrying
#' unexecuted stages. Weights compound across phases, and [as_svydesign()]
#' exports this path through [survey::twophase()].
#'
#' ## Weights
#'
#' `.weight` is the product of the per-stage weights and of the phase weights
#' across phases. [sample-columns] gives the formulas.
#'
#' ## When a design cannot be realized as written
#'
#' A pool holding fewer units than the stage asks for is selected whole,
#' which makes the design non-self-weighting. `execute()` reports this and
#' four related outcomes as classed conditions carrying a `payload`. With the
#' default digest, the `capped` column of
#' `frame_summary(sample, detail = "pool")` shows capping after the fact.
#' [execution-conditions] gives the five classes, their payload fields, and
#' how to capture one when no digest is kept.
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
#'   as.data.frame() |>
#'   dplyr::reframe(hh_id = seq_len(20), .by = ea_id)
#' sample <- selected_eas |> execute(listing, seed = 43)
#' nrow(sample)  # 12 households in each selected EA
#'
#' ## One frame per stage
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
#' ## Multi-phase
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
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 1, panels = 4)
#' table(sample$.panel)  # 50 per panel
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
  # Read this before `match.arg()` fills its default.
  frame_digest_given <- !missing(frame_digest)
  frame_digest <- with_error_class(
    rlang::arg_match(frame_digest),
    "samplyr_error_execute_argument"
  )

  # Read call names without evaluating stray arguments.
  check_execute_dot_names(enquos(...))
  dots <- list(...)

  if (is.data.frame(.data) && !is_tbl_sample(.data)) {
    abort_frame_misplaced(
      "execute",
      design_given = any(vapply(dots, is_sampling_design, logical(1)))
    )
  }

  execution_environment <- capture_execution_environment()

  # A program can only materialize a wave.
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
      frame_digest_given = frame_digest_given,
      execution_environment = execution_environment
    ))
  }

  # Materialize waves before frame normalization.
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
      frame_digest_given = frame_digest_given,
      execution_environment = execution_environment
    ))
  }

  supplied <- normalize_execute_frames(dots)
  frames <- supplied$frames

  if (length(frames) == 0) {
    cli_abort(
      "At least one data frame must be provided",
      class = "samplyr_error_frame_count"
    )
  }

  check_execute_dots(frames, collected = supplied$collected)
  for (i in seq_along(frames)) {
    frames[[i]] <- prepare_frame(frames[[i]])
  }

  # Use the same preflight as `validate_frame()`.
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
      cli_abort(
        "{.arg reps} must be a single integer >= 2",
        class = "samplyr_error_execute_argument"
      )
    }
    reps <- as.integer(reps)
  }

  if (!is_null(reps) && !is_null(panels)) {
    cli_abort(
      "{.arg panels} and {.arg reps} cannot be used together.",
      class = "samplyr_error_execute_argument"
    )
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
      "{.arg .data} must be a {.cls sampling_design} or {.cls tbl_sample}",
      class = "samplyr_error_design_expected"
    )
  }

  # Resolve frames before consuming RNG state.
  schedule <- stage_frame_schedule(design, frames, stages, executed)
  design <- resolve_cluster_nesting(
    design, schedule$entries,
    previous_sample = if (is_tbl_sample(.data)) as.data.frame(.data)
  )
  if (is_sampling_design(.data)) {
    .data <- design
  } else {
    attr(.data, "design") <- design
  }

  # Before any RNG, so deserialized designs are covered too.
  validate_certainty_bridge(design, schedule)

  # A deserialized design never ran the draw() validators.
  check_custom_methods_match_record(design, strict = FALSE)

  # Resolve panel stages before consuming RNG state.
  panels <- resolve_panel_stage(
    panels,
    executed = sort(unique(c(executed, schedule$stages))),
    design = design
  )

  # Compare adjacent frame populations once.
  warn_incomplete_registers(
    schedule,
    design,
    previous_sample = if (is_tbl_sample(.data)) as.data.frame(.data) else NULL
  )

  # Every reachable unit, so acceptance does not depend on the seed.
  preflight_later_stages(
    schedule, design,
    sample = if (is_tbl_sample(.data)) .data else NULL
  )

  # Keep panel diagnostics at the execute() call.
  user_call <- current_env()

  run_execution <- function() {
    rlang::local_error_call(caller_env())
    warn_if_modified <- function(obj, role) {
      status <- sample_realization_status(obj)
      same_partial_design <-
        identical(role, "frame") &&
        is_sampling_design(.data) &&
        identical(declared_design(get_design(obj)), declared_design(.data)) &&
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
        ), class = "samplyr_warning_modified_sample")
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

    # Check PRN conflicts only for stages executed now.
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
          ), class = "samplyr_error_execute_argument")
        }
      }
    }

    # Detect replicated samples used as phase frames.
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
          ), class = "samplyr_error_replicated_sample_unsupported")
        }
        if (!is_null(panels)) {
          cli_abort(
            "{.arg panels} cannot be used with a replicated frame.",
            class = "samplyr_error_replicated_sample_unsupported"
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
        ), class = "samplyr_error_replicated_sample_unsupported")
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

  # Emit one report per execution.
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

  # Plain text keeps the two pluralized counts separate.
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
  rlang::local_error_call(call)
  stages <- schedule$stages
  frames <- schedule_frames(schedule)

  # Fingerprint the supplied frame before phase preparation.
  supplied_frames <- frames

  prev_phase <- NULL
  for (i in seq_along(frames)) {
    prepared <- prepare_multiphase_frame(frames[[i]])
    frames[[i]] <- prepared$frame
    if (!is_null(prepared$prev_phase)) {
      prev_phase <- prepared$prev_phase
    }
  }

  # Validate phase keys before clustering can collapse rows.
  phase_link_vars <- phase_link_vars_of(prev_phase)
  check_phase_key_invariance(schedule, design, phase_link_vars, prev_phase)

  current_sample <- NULL
  previous_stage_idx <- NULL
  all_prior_cluster_vars <- character(0)
  empty_parents <- list()
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
      linked <- link_stage_frame(
        frame,
        current_sample,
        design = design,
        stage_idx = stage_idx,
        frame_index = entry$frame_index,
        frame_label = entry$frame_label,
        phase_link_vars = phase_link_vars
      )
      frame <- linked$frame
      empty_parents <- add_empty_parents(
        empty_parents, stage_idx, linked$empty_parents
      )
    }

    step <- execute_single_stage(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_idx,
      previous_sample = current_sample,
      previous_stage_spec = prev_stage_for_frame,
      is_final_stage = is_final_stage,
      all_prior_cluster_vars = all_prior_cluster_vars,
      parent_levels = ancestor_cluster_levels(design, stage_idx),
      trace_mode = frame_digest
    )
    current_sample <- step$sample
    if (collect_trace) {
      stage_traces[[i]] <- step$trace
      stage_used_frames[[i]] <- step$frame
    }

    # An empty random-size draw ends the selection.
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
    # Digest failure must not discard a valid sample.
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
        ), class = "samplyr_warning_digest_unavailable")
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
      frame_schedule = schedule_record(schedule),
      empty_parents = empty_parents
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
  rlang::local_error_call(call)
  results <- vector("list", reps)
  rep_digests <- vector("list", reps)
  collect_digest <- executor == "design" &&
    !identical(frame_digest, "none")
  inherited_empty <- if (is_tbl_sample(.data)) {
    attr(.data, "metadata")$empty_parents
  } else {
    list()
  }
  empty_parents <- inherited_empty
  rep_seeds <- replicate_seed_values(seed, reps)

  for (r in seq_len(reps)) {
    rep_seed <- rep_seeds[r]

    run_one <- function() {
      rlang::local_error_call(caller_env())
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
    # Keep only what this replicate added to the carried record.
    added <- attr(result, "metadata")$empty_parents
    added <- added[setdiff(seq_along(added), seq_along(inherited_empty))]
    empty_parents <- c(empty_parents, tag_empty_parents(added, r))
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
        ), class = "samplyr_warning_digest_unavailable")
        NULL
      }
    )
  }

  combined <- do.call(rbind, results)
  combined$.sample_id <- seq_len(nrow(combined))

  # Derive metadata from inputs, not the last loop iteration.
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
      replicate_seeds = rep_seeds,
      replicate_rows = setNames(
        vapply(results, nrow, integer(1)),
        as.character(seq_len(reps))
      ),
      continued_from = if (identical(executor, "continuation")) {
        continuation_parent_record(.data)
      },
      prev_phase = attr(result, "metadata")$prev_phase,
      execution_environment = execution_environment,
      integrity = integrity,
      frame_digest = digest,
      frame_schedule = schedule_record(schedule),
      empty_parents = empty_parents,
      # Each replicate ran every stage and phase itself.
      replicates_complete = identical(executor, "design") &&
        is_null(attr(result, "metadata")$prev_phase)
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
  empty_parents <- list()
  # A valid assignment needs both its record and `.panel`.
  has_record <- !is_null(parent_meta$panel_assignment)
  has_column <- ".panel" %in% names(current_sample)

  # A continuation cannot replace a frozen panel assignment.
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

  # Refuse incomplete inherited assignments.
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

    # Avoid collisions with inherited sample metadata.
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
    linked <- link_stage_frame(
      frame,
      current_sample,
      design = design,
      stage_idx = stage_idx,
      frame_index = entry$frame_index,
      frame_label = entry$frame_label,
      phase_link_vars = phase_link_vars
    )
    frame <- linked$frame
    empty_parents <- add_empty_parents(
      empty_parents, stage_idx, linked$empty_parents
    )

    step <- execute_single_stage(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_idx,
      previous_sample = current_sample,
      previous_stage_spec = prev_stage_for_frame,
      is_final_stage = is_final_stage,
      all_prior_cluster_vars = all_prior_cluster_vars,
      parent_levels = ancestor_cluster_levels(design, stage_idx),
      trace_mode = frame_digest
    )
    current_sample <- step$sample
    if (collect_trace) {
      stage_traces[[i]] <- step$trace
      stage_used_frames[[i]] <- step$frame
    }

    # See `execute_design()`. An empty stage ends the selection.
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
    # Extend only a digest that still describes its sample.
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
          ), class = "samplyr_warning_digest_unavailable")
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
      continued_from = continuation_parent_record(sample),
      # Preserve the earlier phase link across stage continuation.
      prev_phase = parent_meta$prev_phase,
      execution_environment = execution_environment,
      integrity = sample_integrity_record(
        current_sample,
        design,
        c(executed, stages)
      ),
      frame_schedule = schedule_record(schedule),
      empty_parents = c(parent_meta$empty_parents, empty_parents)
    )
  )
}

#' The call a continuation continued, as its receipt needs it
#'
#' The parent's metadata, plus what a chained receipt needs to replay that call
#' and the metadata does not hold: the seed and stages are attributes of the
#' parent, which the continuation replaces, and whether the parent was modified
#' decides whether replaying it can rebuild what was continued.
#' @noRd
continuation_parent_record <- function(sample) {
  record <- attr(sample, "metadata") %||% list()
  record$seed <- attr(sample, "seed")
  record$stages_executed <- get_stages_executed(sample)
  record$realization_modified <- !sample_realization_status(sample)$ok
  record
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
  blocked <- with_error_class(
    rlang::arg_match(blocked),
    "samplyr_error_internal"
  )
  design <- get_design(sample)
  stages_exec <- get_stages_executed(sample)
  rs_methods <- unique(unlist(lapply(stages_exec, function(i) {
    spec <- design$stages[[i]]$draw_spec
    if (is_random_size_method(spec)) {
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

#' Seeds for the replicates of one execution
#'
#' Consecutive seeds do not give independent first draws in R: a start taken
#' from one uniform, as a systematic stage takes it, coincided across seeds
#' `s, s + 1, ...` measurably less often than chance, so replicates seeded
#' that way were negatively correlated. The seeds are drawn from `seed`
#' instead and recorded, so each replicate can still be rerun alone.
#' @return An integer vector of `reps` distinct seeds, or NULL without a seed.
#' @noRd
replicate_seed_values <- function(seed, reps) {
  if (is_null(seed)) {
    return(NULL)
  }
  withr::with_seed(seed, sample.int(.Machine$integer.max, reps))
}

#' Were the source's replicates independent at every stage and phase?
#'
#' A continuation or a later phase runs each source replicate on, so its
#' result is complete when the source was. An empty source replicate cannot
#' be dropped on the way: `check_no_empty_replicates()` refuses it first.
#' @noRd
replicates_carried <- function(source) {
  isTRUE(attr(source, "metadata")$replicates_complete)
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
      call = call,
      class = "samplyr_error_replicated_sample_unsupported"
    )
  }

  rep_ids <- sort(unique(sample$.replicate))
  results <- vector("list", length(rep_ids))
  inherited_empty <- attr(sample, "metadata")$empty_parents
  empty_parents <- inherited_empty
  rep_seeds <- replicate_seed_values(seed, length(rep_ids))

  for (i in seq_along(rep_ids)) {
    r <- rep_ids[i]
    rep_sample <- sample[sample$.replicate == r, ]
    rep_sample$.replicate <- NULL

    # Restore the sample class after subsetting.
    rep_sample <- new_tbl_sample(
      data = rep_sample,
      design = get_design(sample),
      stages_executed = get_stages_executed(sample),
      seed = attr(sample, "seed"),
      metadata = attr(sample, "metadata")
    )

    rep_seed <- rep_seeds[i]

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

    added <- attr(result, "metadata")$empty_parents
    added <- added[setdiff(seq_along(added), seq_along(inherited_empty))]
    empty_parents <- c(empty_parents, tag_empty_parents(added, r))
    df <- as.data.frame(result)
    df$.replicate <- rep.int(r, nrow(df))
    results[[i]] <- df
  }

  combined <- do.call(rbind, results)
  combined$.sample_id <- seq_len(nrow(combined))

  # Derive metadata from inputs.
  the_design <- get_design(sample)
  already_executed <- get_stages_executed(sample)
  cont_stages <- schedule$stages

  # Continued stages are replicate-specific.
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
      replicate_seeds = rep_seeds,
      replicate_rows = setNames(
        vapply(results, nrow, integer(1)),
        as.character(rep_ids)
      ),
      continued_from = continuation_parent_record(sample),
      prev_phase = attr(sample, "metadata")$prev_phase,
      replicates_complete = replicates_carried(sample),
      execution_environment = execution_environment,
      frame_schedule = schedule_record(schedule),
      empty_parents = empty_parents,
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
  # Repeated schedule entries alias one previous phase.
  supplied <- vector("list", schedule$n_supplied)
  for (entry in schedule$entries) {
    supplied[[entry$frame_index]] <- entry$frame
  }
  source_sample <- supplied[[1]]

  rep_ids <- sort(unique(source_sample$.replicate))

  # Validate all replicate phase keys before drawing.
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
  empty_parents <- list()
  rep_seeds <- replicate_seed_values(seed, length(rep_ids))

  for (i in seq_along(rep_ids)) {
    r <- rep_ids[i]

    # Remap this validated subset to each scheduled stage.
    rep_frames <- supplied
    rep_frames[[1]] <- replicate_frames[[i]]

    rep_seed <- rep_seeds[i]

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

    empty_parents <- c(
      empty_parents,
      tag_empty_parents(attr(result, "metadata")$empty_parents, r)
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
      replicate_seeds = rep_seeds,
      replicate_rows = setNames(
        vapply(results, nrow, integer(1)),
        as.character(rep_ids)
      ),
      # Retain the complete replicated parent phase.
      prev_phase = list(
        design = get_design(source_sample),
        stages = get_stages_executed(source_sample),
        sample = source_sample
      ),
      replicates_complete = replicates_carried(source_sample),
      execution_environment = execution_environment,
      frame_schedule = schedule_record(schedule),
      empty_parents = empty_parents,
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
  parent_levels = list(),
  trace_mode = "full"
) {
  rlang::local_error_call(caller_env())
  collect_stage_events(
    execute_single_stage_impl(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_num,
      previous_sample = previous_sample,
      previous_stage_spec = previous_stage_spec,
      is_final_stage = is_final_stage,
      all_prior_cluster_vars = all_prior_cluster_vars,
      parent_levels = parent_levels,
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
  parent_levels = list(),
  trace_mode = "full"
) {
  rlang::local_error_call(caller_env())
  strata_spec <- stage_spec$strata
  cluster_spec <- stage_spec$clusters
  draw_spec <- stage_spec$draw_spec
  # The parent pools a lower stage runs in, none at stage 1.
  split_vars <- character(0)

  validate_frame_vars(frame, stage_spec)

  stage_totals <- NULL
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
      # Qualify events by the parent cluster.
      parent_labels <- path_labels(split$key_df, split_vars, parent_levels)
      if (!is_null(strata_spec)) {
        strata_spec$label_vars <- setdiff(strata_spec$vars, split_vars)
      }

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

    stage_totals <- stage_pool_totals(
      frame, split_vars, strata_spec$vars, cluster_spec$vars, nrow(result)
    )
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
      if (".zone" %in% names(result)) {
        join_cols <- c(join_cols, ".zone")
      }
      if (".pair" %in% names(result)) {
        join_cols <- c(join_cols, ".pair")
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

    if (!is_null(strata_spec)) {
      strata_spec$label_vars <- setdiff(strata_spec$vars, split_vars)
    }
    res <- sample_within_clusters(
      frame,
      strata_spec,
      draw_spec,
      split_vars,
      trace_mode = trace_mode,
      levels = parent_levels
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
  if (is_null(stage_totals)) {
    stage_totals <- stage_pool_totals(
      frame, split_vars, strata_spec$vars, character(0), nrow(result)
    )
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

  if (".zone" %in% names(result)) {
    result[[paste0(".zone_", stage_num)]] <- result$.zone
    result$.zone <- NULL
  }

  if (".pair" %in% names(result)) {
    result[[paste0(".pair_", stage_num)]] <- result$.pair
    result$.pair <- NULL
  }

  if (!is_null(previous_sample) && ".weight" %in% names(previous_sample)) {
    result <- compound_stage_weights(
      result,
      previous_sample,
      parent_vars = all_prior_cluster_vars
    )
  }

  # Keep sample IDs unique after cluster expansion.
  result$.sample_id <- seq_len(nrow(result))
  # Return the frame used by trace row indices.
  list(
    sample = result,
    trace = stage_trace,
    frame = frame,
    stage_totals = stage_totals
  )
}

#' What one stage executed: its pools, the units in them, the units taken
#'
#' A lower stage runs once per selected parent, and each run reports only
#' the pools it capped. Summed over those runs, a stage where 2 of 40 EAs
#' ran short read as having exhausted everything it reached. These totals
#' come from the stage's own linked frame and result, so the census test
#' compares the whole stage. Pools are parent by stratum, units are rows or,
#' at a clustered stage, distinct clusters within their parent.
#' @noRd
stage_pool_totals <- function(frame, split_vars, strata_vars, cluster_vars,
                              n_selected) {
  distinct_rows <- function(vars) {
    if (length(vars) == 0L) {
      return(if (nrow(frame) > 0L) 1L else 0L)
    }
    nrow(vctrs::vec_unique(frame[vars]))
  }
  list(
    n_pools = distinct_rows(c(split_vars, strata_vars)),
    n_available = if (length(cluster_vars) > 0L) {
      distinct_rows(c(split_vars, cluster_vars))
    } else {
      nrow(frame)
    },
    n_actual = n_selected
  )
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
    grep("^\\.zone_\\d+$", nms, value = TRUE),
    grep("^\\.pair_\\d+$", nms, value = TRUE),
    intersect(".panel", nms),
    intersect("._prev_phase_weight", nms)
  )
}

#' Compound weights by joining on shared variables
#' @noRd
compound_by_join <- function(result, previous_sample, join_vars, carry_cols) {
  carry_cols_to_select <- setdiff(carry_cols, join_vars)

  # Do not collide with a user `.prev_weight` column.
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

  # Every selected row must match one parent weight.
  if (nrow(out) != n_before || anyNA(out[[prev_weight]])) {
    cli_abort(
      c(
        "Stage weights could not be compounded onto every selected row.",
        "i" = "Joined on {.field {join_vars}}."
      ),
      call = NULL,
      class = "samplyr_error_internal"
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
  # Avoid duplicate transient ancestry columns.
  if ("._prev_phase_weight" %in% names(result)) {
    carry_cols <- setdiff(carry_cols, "._prev_phase_weight")
  }

  # Use ancestry resolved by the stage transition.
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

  # Do not collide with a user `.row_id` column.
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
  rlang::local_error_call(caller_env())
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
    cli_abort(
      "Design has no stages defined",
      call = call,
      class = "samplyr_error_stage_incomplete"
    )
  }

  for (i in seq_along(design$stages)) {
    stage <- design$stages[[i]]
    if (is_null(stage$draw_spec)) {
      label <- stage$label %||% paste("Stage", i)
      cli_abort(
        "{.val {label}} is incomplete: missing {.fn draw}",
        call = call,
        class = "samplyr_error_stage_incomplete"
      )
    }
    if (identical(stage$draw_spec$method_probabilities, "unknown")) {
      abort_unknown_probabilities(stage$draw_spec$method, call = call)
    }
    validate_stored_draw_spec(stage, call = call)
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
      call = call,
      class = "samplyr_error_draw_method_argument"
    )
  }

  invisible(TRUE)
}

#' @noRd
validate_stored_draw_spec <- function(stage, call = rlang::caller_env()) {
  draw_spec <- stage$draw_spec
  strata <- stage$strata
  if (!is_null(strata)) {
    alloc <- check_alloc_method(strata$alloc, call = call)
    aux <- Map(
      function(value, col, arg) {
        if (is_null(value)) {
          return(NULL)
        }
        coerce_aux_input(value, strata$vars, col, arg, call = call)
      },
      strata[c("variance", "cost", "cv", "importance")],
      c("var", "cost", "cv", "importance"),
      c("variance", "cost", "cv", "importance")
    )
    validate_stratify_args(
      alloc = alloc, variance = aux$variance, cost = aux$cost,
      cv = aux$cv, importance = aux$importance, power = strata$power,
      vars = strata$vars, call = call
    )
  }
  resolved_method <- resolve_draw_method(draw_spec$method, call = call)
  method <- resolved_method$method
  custom_spec <- resolved_method$custom_spec

  validate_draw_configuration(
    n = draw_spec$n,
    frac = draw_spec$frac,
    method = method,
    mos = draw_spec$mos,
    prn = draw_spec$prn,
    min_n = draw_spec$min_n,
    max_n = draw_spec$max_n,
    certainty_size = draw_spec$certainty_size,
    certainty_prop = draw_spec$certainty_prop,
    round = draw_spec$round,
    certainty_overflow = draw_spec$certainty_overflow,
    on_empty = draw_spec$on_empty,
    has_alloc = !is_null(stage$strata) && !is_null(stage$strata$alloc),
    alloc = stage$strata$alloc,
    strata_vars = stage$strata$vars,
    aux = draw_spec$aux,
    bounds = draw_spec$bounds,
    spread = draw_spec$spread,
    custom_spec = custom_spec,
    warn_ignored = FALSE,
    certainty_plan = draw_spec$certainty_plan,
    call = call
  )
  invisible(NULL)
}

#' @noRd
validate_frame_vars <- function(frame, stage_spec, call = rlang::caller_env()) {
  if (nrow(frame) == 0) {
    cli_abort(
      "Frame has 0 rows",
      call = call,
      class = "samplyr_error_frame_empty"
    )
  }

  # Share required-variable rules with the pre-RNG preflight.
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
      call = call,
      class = "samplyr_error_frame_missing_vars"
    )
  }

  if (!is_null(strata_vars)) {
    na_strata <- Filter(function(v) anyNA(frame[[v]]), strata_vars)
    if (length(na_strata) > 0) {
      cli_abort(
        "Stratification variable{?s} {.var {na_strata}} contain{?s/} NA values",
        call = call,
        class = "samplyr_error_frame_invalid"
      )
    }
  }

  if (!is_null(cluster_vars)) {
    na_clusters <- Filter(function(v) anyNA(frame[[v]]), cluster_vars)
    if (length(na_clusters) > 0) {
      cli_abort(
        "Cluster variable{?s} {.var {na_clusters}} contain{?s/} NA values",
        call = call,
        class = "samplyr_error_frame_invalid"
      )
    }
  }

  if (!is_null(mos_var)) {
    mos_vals <- frame[[mos_var]]
    if (!is.numeric(mos_vals)) {
      cli_abort(
        "MOS variable {.var {mos_var}} must be numeric, not {.cls {class(mos_vals)[[1]]}}",
        call = call,
        class = "samplyr_error_frame_invalid"
      )
    }
    if (anyNA(mos_vals)) {
      cli_abort(
        "MOS variable {.var {mos_var}} contains NA values",
        call = call,
        class = "samplyr_error_frame_invalid"
      )
    }
    if (any(mos_vals < 0)) {
      cli_abort(
        "MOS variable {.var {mos_var}} contains negative values",
        call = call,
        class = "samplyr_error_frame_invalid"
      )
    }
    # Leave all-zero MOS to the PPS selection error.
    if (any(mos_vals == 0) && sum(mos_vals) > 0) {
      n_zero <- sum(mos_vals == 0)
      cli_warn(c(
        "MOS variable {.var {mos_var}} contains {n_zero} zero value{?s}.",
        "i" = "Units with MOS = 0 have zero inclusion probability and will never be selected.",
        "i" = "Consider removing them from the frame or assigning a positive measure of size."
      ), class = "samplyr_warning_mos_zero")
    }
  }

  if (!is_null(prn_var)) {
    prn_vals <- frame[[prn_var]]
    if (!is.numeric(prn_vals)) {
      cli_abort(
        "PRN variable {.var {prn_var}} must be numeric, not {.cls {class(prn_vals)[[1]]}}",
        call = call,
        class = "samplyr_error_frame_invalid"
      )
    }
    if (anyNA(prn_vals)) {
      cli_abort(
        "PRN variable {.var {prn_var}} contains NA values",
        call = call,
        class = "samplyr_error_frame_invalid"
      )
    }
    if (any(prn_vals <= 0) || any(prn_vals >= 1)) {
      cli_abort(
        "PRN variable {.var {prn_var}} must have values in the open interval (0, 1)",
        call = call,
        class = "samplyr_error_frame_invalid"
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
        call = call,
        class = "samplyr_error_frame_missing_vars"
      )
    }
    for (av in aux_vars) {
      aux_vals <- frame[[av]]
      if (!is.numeric(aux_vals)) {
        cli_abort(
          "Auxiliary variable {.var {av}} must be numeric, not {.cls {class(aux_vals)[[1]]}}",
          call = call,
          class = "samplyr_error_frame_invalid"
        )
      }
      if (anyNA(aux_vals)) {
        cli_abort(
          "Auxiliary variable {.var {av}} contains NA values",
          call = call,
          class = "samplyr_error_frame_invalid"
        )
      }
    }
  }

  # Required-variable checks already established presence.
  bound_vars <- stage_spec$draw_spec$bounds
  if (!is_null(bound_vars)) {
    for (var in bound_vars) {
      if (anyNA(frame[[var]])) {
        cli_abort(
          "Count-bound variable {.var {var}} contains NA values",
          call = call,
          class = "samplyr_error_frame_invalid"
        )
      }
    }
  }

  # Required-variable checks already established presence.
  spread_vars <- stage_spec$draw_spec$spread
  if (!is_null(spread_vars)) {
    for (var in spread_vars) {
      values <- frame[[var]]
      if (!is.numeric(values) || anyNA(values) || any(!is.finite(values))) {
        cli_abort(
          "Spatial coordinate variable {.var {var}} must be finite numeric with no missing values",
          call = call,
          class = "samplyr_error_frame_invalid"
        )
      }
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
