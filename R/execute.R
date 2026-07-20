#' Execute a Sampling Design
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
#'   `sampling_design` starts a new sampling phase; it does not continue the
#'   stages stored in that sample. Ordinary input frames must have unique
#'   names and must not use columns reserved for execution output, such as
#'   `.weight`, `.sample_id`, `.stage`, `.weight_k`, or `.fpc_k`.
#' @param stages Integer vector specifying which stage(s) to execute.
#'   From a `sampling_design`, the vector must start at stage 1; this is how
#'   an operational workflow stops after its first contiguous batch of stages.
#'   From a partial `tbl_sample`, it must start at the next unexecuted stage.
#'   Default (`NULL`) executes all remaining stages.
#' @param seed Integer random seed for reproducibility, between
#'   `-.Machine$integer.max` and `.Machine$integer.max`.
#' @param panels Integer number of rotation groups (panels) to partition the
#'   sample into for rotation or workload management. Assignment uses
#'   deterministic systematic interleaving within strata; it is not an
#'   additional probability-sampling phase. The output includes a `.panel`
#'   column with values 1 through `panels`. Default `NULL` means no panel
#'   partitioning. Cannot be used together with `reps`.
#' @param reps Integer number of independent replicate samples to draw (>= 2),
#'   or `NULL` (default) for a single sample. When specified, `execute()` draws
#'   `reps` independent samples from the same frame under the same design and
#'   returns a single stacked `tbl_sample` with a `.replicate` column (integer
#'   1 through `reps`). Replicate `r` uses seed `seed + r - 1`; the complete
#'   sequence must remain within R's supported integer range. Cannot be
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
#'   from the design (`chance_status = "design_resolved"`); this gives
#'   [frame_summary()] and downstream digest consumers complete universe
#'   denominators without the frame. The digest never
#'   affects selection, weights, or estimation. Design executions
#'   record a digest, replicated executions share the population
#'   structure across replicates with replicate-specific traces, and a
#'   stage continuation extends the digest carried by its input sample
#'   (an input without a valid digest yields no digest). In a
#'   replicated multistage execution, later-stage pools depend on each
#'   replicate's realized parents, so the digest keeps the stage
#'   prefix shared by all replicates and reports status `"partial"`.
#'   Replicated multi-phase and replicated-continuation executions do
#'   not record one yet.
#'
#' @return A `tbl_sample` object (a data frame subclass with sampling
#'   metadata). Contains the selected units plus:
#'   - `.sample_id`: Unique identifier for each sampled unit
#'   - `.weight`: Sampling weight (1/probability)
#'   - `.weight_1`, `.weight_2`, ...: Per-stage sampling weights
#'     (\eqn{1/\pi_i^{(k)}}{1/pi_i(k)}) for the stored design. In a
#'     single-phase multistage sample their product equals `.weight`; in a
#'     multiphase sample `.weight` additionally includes earlier-phase
#'     weights.
#'   - `.fpc_1`, `.fpc_2`, ...: Per-stage finite population correction
#'     values. The meaning depends on the method and context:
#'     - **Equal-probability WOR** (srswor, systematic): \eqn{N_h} (stratum
#'       population size), or \eqn{N} if unstratified. The sampling fraction
#'       \eqn{f = n / N} is derived from this at variance-estimation time.
#'     - **PPS WOR** (pps_brewer, pps_cps, etc.): \eqn{N_h} (stratum
#'       population size), converted to \eqn{\pi_i = 1/w_i}{pi_i = 1/w_i}
#'       at survey export, because `survey::svydesign()` expects inclusion
#'       probabilities for unequal-probability stages.
#'     - **Clustered stages**: the number of clusters in the
#'       stratum/group, not the number of ultimate units.
#'     - **WR / PMR** (srswr, pps_multinomial, pps_chromy): \eqn{\infty}{Inf}.
#'       With-replacement designs have no finite population correction;
#'       variance is estimated via the Hansen--Hurwitz formula.
#'     In a multi-stage design, each stage has its own `.fpc_k`. At survey
#'     export (`as_svydesign()`), these are assembled into a multi-level FPC
#'     formula (e.g., `~ .fpc_1 + .fpc_2`).
#'   - `.draw_1`, `.draw_2`, ...: Draw index per stage (WR/PMR methods only).
#'     Each row represents one independent draw; the draw index identifies
#'     which with-replacement selection the row came from.
#'   - `.certainty_1`, `.certainty_2`, ...: Whether each unit was a certainty
#'     selection (PPS methods with certainty thresholds only)
#'   - `.replicate`: Replicate identifier (only when `reps` is specified)
#'   - `.panel`: Panel assignment (only when `panels` is specified)
#'   - Stage and stratum identifiers as appropriate
#'
#' @details
#' ## Execution Patterns
#'
#' ### Single-Stage Execution
#' \preformatted{
#' design |> execute(frame, seed = 1)
#' }
#'
#' ### Multi-Stage with Single Frame
#' For hierarchical data where all stages are in one frame:
#' \preformatted{
#' design |> execute(frame, seed = 2025)
#' }
#' The frame must contain all clustering variables and represent the stage
#' hierarchy correctly. Lower-stage IDs may repeat across different parents;
#' `samplyr` resolves them using the full ancestry from earlier stages.
#'
#' ### Multi-Stage with Multiple Frames
#' When each stage has its own frame:
#' \preformatted{
#' design |> execute(frame1, frame2, frame3, seed = 424)
#' }
#' Frames are matched to stages by position.
#'
#' ### Partial Execution (Operational Sampling)
#' Execute only specific stages:
#' \preformatted{
#' selected_eas <- design |> execute(ea_frame, stages = 1, seed = 42)
#' # ... fieldwork: listing in selected EAs ...
#' sample <- selected_eas |> execute(listing_frame, seed = 43)
#' }
#' When the listing frame is derived from a `tbl_sample` (e.g. via
#' [tidyr::uncount()] or [dplyr::slice()]), it may carry internal
#' columns (`.weight`, `.fpc_1`, etc.) from the earlier stage. These
#' are automatically stripped before sampling so they do not collide
#' with the metadata carried by the stage-1 result.
#' Pass the unmodified stage-1 result as `.data` and the expanded listing
#' as the frame, as above. Passing the original design as `.data` instead
#' starts a new execution at stage 1 and treats a `tbl_sample` frame as a
#' previous sampling phase; it is not a stage continuation. When an intact
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
#' ### Multi-Phase Sampling
#' To start a new phase, use the new phase's design as `.data` and pass the
#' previous phase's `tbl_sample` as its frame:
#' \preformatted{
#' phase1 <- design1 |> execute(frame, seed = 42)
#' # ... add screening data to phase1 ...
#' phase2 <- design2 |> execute(phase1_updated, seed = 123)
#' }
#' This is distinct from stage continuation: `phase1` is a frame for a new
#' design, rather than `.data` carrying unexecuted stages of the same design.
#' Weights compound automatically in multi-phase designs, and
#' [as_svydesign()] exports this path through [survey::twophase()].
#'
#' ## Weight Calculation
#'
#' The `.weight` column is the inverse of the selection chance that
#' samplyr resolves for the unit: the first-order inclusion probability
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
#'   of selections. Each draw is one row; a unit selected \eqn{k} times
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
#' ### Multi-stage weight compounding
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
#' ### Multi-phase weight compounding
#'
#' When a new phase's design is executed with a previous-phase `tbl_sample`
#' as its frame, the phase-1 inclusion probability is already reflected in
#' the input weights.
#' The final `.weight` is the product of phase-1 and phase-2 weights:
#' \deqn{w_i = w_i^{(\text{phase 1})} \times w_i^{(\text{phase 2} \mid \text{phase 1})}}
#' This ensures the Horvitz--Thompson estimator
#' \eqn{\hat{Y} = \sum_S w_i \, y_i}{Y-hat = sum(w_i * y_i)} is unbiased
#' for the population total.
#'
#' ## Panel Partitioning
#'
#' When `panels` is specified, the sample is partitioned into non-overlapping
#' groups for rotation or workload management using systematic interleaving
#' within strata.
#'
#' Assignment is deterministic (not random): within each stratum, units are
#' assigned round-robin to panels 1, 2, ..., k. This ensures each panel has
#' approximately equal representation from every stratum. The quality of panel
#' balance benefits from `control` sorting in `draw()`, which determines the
#' order of units before interleaving.
#'
#' For multi-stage designs, panels are assigned at stage 1 (PSU level).
#' All units within a PSU inherit the PSU's panel assignment.
#'
#' Weights are not adjusted for panel membership. They reflect the full-sample
#' inclusion probability and are valid for the combined sample. A single panel
#' does not have a known probability-sampling interpretation merely from this
#' assignment, so multiplying its weights by `panels` is not generally valid
#' for population inference.
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
#' [is_tbl_sample()] for testing results,
#' [get_design()] for extracting metadata
#'
#' @export
execute <- function(.data, ..., stages = NULL, seed = NULL, panels = NULL,
                    reps = NULL,
                    frame_digest = c("summary", "full", "none")) {
  frame_digest <- match.arg(frame_digest)
  frames <- list(...)

  execution_environment <- capture_execution_environment()

  if (length(frames) == 0) {
    cli_abort("At least one data frame must be provided")
  }

  for (i in seq_along(frames)) {
    if (!is.data.frame(frames[[i]])) {
      cli_abort("Argument {i} to {.fn execute} must be a data frame")
    }
  }

  # A class-dropping operation such as tidyr::uncount() can leave both the
  # sample attributes and all generated design columns on an apparently plain
  # frame. Starting from a design would otherwise rerun stage 1 silently.
  if (is_sampling_design(.data)) {
    for (i in seq_along(frames)) {
      frame <- frames[[i]]
      if (looks_like_stripped_tbl_sample(frame)) {
        has_provenance <-
          is_sampling_design(attr(frame, "design")) &&
            !is_null(attr(frame, "stages_executed"))

        abort_samplyr(
          c(
            "Frame {i} looks like a {.cls tbl_sample} whose class was dropped.",
            "x" = "Executing it from a {.cls sampling_design} would start a
                   fresh execution at stage 1 and could silently produce
                   incorrect weights.",
            "i" = "For operational multistage sampling, continue from the
                   unmodified partial sample and pass this object only as
                   the listing frame:
                   {.code partial_sample |> execute(listing_frame)}.",
            if (has_provenance) c(
              "i" = "For a genuinely new sampling phase, restore the previous
                     sample explicitly with {.fn as_tbl_sample} before using
                     it as the new design's frame."
            ),
            "i" = "Generated columns such as {.field .weight},
                   {.field .weight_k}, {.field .fpc_k},
                   {.field .sample_id}, and {.field .stage} are evidence of
                   an earlier execution."
          ),
          class = "samplyr_error_stripped_sample_frame"
        )
      }
    }
  }

  for (i in seq_along(frames)) {
    validate_execute_frame_names(
      frames[[i]],
      index = i,
      allow_generated = is_tbl_sample(.data) || is_tbl_sample(frames[[i]])
    )
  }

  if (!is_null(seed)) {
    if (
      length(seed) != 1L || !is_integerish_numeric(seed) ||
        seed < -.Machine$integer.max || seed > .Machine$integer.max
    ) {
      abort_samplyr(
        "{.arg seed} must be a single integer between
         { -.Machine$integer.max } and { .Machine$integer.max }.",
        class = "samplyr_error_seed_range"
      )
    }
    seed <- as.integer(seed)
  }

  if (!is_null(panels)) {
    if (
      !is.numeric(panels) ||
        length(panels) != 1 ||
        !is_integerish_numeric(panels) ||
        panels < 2
    ) {
      cli_abort("{.arg panels} must be a single integer >= 2")
    }
    panels <- as.integer(panels)
  }

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
    !is_null(seed) && !is_null(reps) &&
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

  run_execution <- function() {
    # A modified tbl_sample (rows or design columns changed after its
    # own execute()) is accepted as input, but its weights and design
    # metadata are taken at face value for the new selection.
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
                 phase and restarts the design at stage 1; it does not
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
    # Empty replicates leave no rows in a stacked sample, so without
    # the check_no_empty_replicates() guard the replicate machinery
    # would silently skip them and condition every downstream result on
    # nonempty realizations.
    if (is_tbl_sample(.data)) {
      warn_if_modified(.data, "input")
      check_no_empty_replicates(.data, blocked = "stages")
    } else {
      # When .data is a design, a tbl_sample frame is the prior phase's
      # sample and its provenance matters. In a continuation
      # (.data is a tbl_sample) the frames are listing frames whose
      # sample provenance is stripped anyway (e.g. the documented
      # row-expansion household listing), so marks on them are
      # irrelevant.
      for (f in frames) {
        if (is_tbl_sample(f)) {
          warn_if_modified(f, "frame")
          check_no_empty_replicates(f, blocked = "phase")
        }
      }
    }

    # PRN conflict check: only on stages being executed in this call
    if (!is_null(reps)) {
      design <- if (is_sampling_design(.data)) .data else get_design(.data)
      n_design_stages <- length(design$stages)
      executed_stages <- if (is_null(stages)) {
        if (is_sampling_design(.data)) {
          seq_along(design$stages)
        } else {
          setdiff(seq_along(design$stages), get_stages_executed(.data))
        }
      } else {
        stages
      }
      # Only check PRN for valid stage indices (stage validation follows)
      valid_stages <- executed_stages[
        executed_stages >= 1L & executed_stages <= n_design_stages
      ]
      if (length(valid_stages) > 0) {
        has_prn <- any(vapply(valid_stages, function(idx) {
          !is_null(design$stages[[idx]]$draw_spec$prn)
        }, logical(1)))
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
    frame_has_reps <- any(vapply(frames, function(f) {
      is_tbl_sample(f) && has_multiple_replicates(f)
    }, logical(1)))

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
          .data, frames, stages, seed, execution_environment
        )
      } else if (is_null(reps)) {
        execute_design(
          .data, frames, stages, seed, panels, execution_environment,
          frame_digest = frame_digest
        )
      } else {
        execute_replicated(.data, frames, stages, seed, reps,
                           executor = "design",
                           execution_environment = execution_environment,
                           frame_digest = frame_digest)
      }
    } else if (is_tbl_sample(.data)) {
      has_existing_reps <- has_multiple_replicates(.data)
      if (has_existing_reps && !is_null(reps)) {
        cli_abort(c(
          "Cannot add new replicates to an already-replicated sample.",
          "i" = "The input sample already has {length(unique(.data$.replicate))} replicates."
        ))
      }
      if (has_existing_reps) {
        execute_replicated_continuation(
          .data, frames, stages, seed, panels, execution_environment,
          frame_digest = frame_digest
        )
      } else if (!is_null(reps)) {
        execute_replicated(.data, frames, stages, seed, reps,
                           executor = "continuation",
                           execution_environment = execution_environment)
      } else {
        execute_continuation(
          .data, frames, stages, seed, panels, execution_environment,
          frame_digest = frame_digest
        )
      }
    } else {
      cli_abort(
        "{.arg .data} must be a {.cls sampling_design} or {.cls tbl_sample}"
      )
    }
  }

  if (!is_null(seed) && is_null(reps)) {
    withr::with_seed(seed, run_execution())
  } else {
    run_execution()
  }
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
execute_design <- function(design, frames, stages, seed, panels,
                           execution_environment,
                           frame_digest = "summary",
                           call = caller_env()) {
  validate_design_complete(design)

  n_stages <- length(design$stages)

  if (is_null(stages)) {
    stages <- seq_len(n_stages)
  } else {
    if (
      !is_integerish_numeric(stages) ||
        any(stages < 1) ||
        any(stages > n_stages)
    ) {
      cli_abort("{.arg stages} must be integers between 1 and {n_stages}",
                call = call)
    }
    stages <- sort(as.integer(stages))
    if (stages[1] != 1L) {
      cli_abort(c(
        "{.arg stages} must start at stage 1 when executing from a design.",
        "i" = "To continue from a previous sample, pass the {.cls tbl_sample} instead of the design."
      ), call = call)
    }
    expected <- seq.int(stages[1], stages[length(stages)])
    if (!identical(stages, expected)) {
      cli_abort("{.arg stages} must be contiguous (no gaps)", call = call)
    }
  }

  if (length(frames) == 1) {
    frames <- rep(frames, length(stages))
  } else if (length(frames) != length(stages)) {
    cli_abort(
      "Number of frames ({length(frames)}) must be 1 or match number of stages to execute ({length(stages)})",
      call = call
    )
  }

  prev_phase <- NULL
  for (i in seq_along(frames)) {
    prepared <- prepare_multiphase_frame(frames[[i]])
    frames[[i]] <- prepared$frame
    if (!is_null(prepared$prev_phase)) {
      prev_phase <- prepared$prev_phase
    }
  }

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
      frame <- subset_frame_to_sample(
        frame,
        current_sample,
        stage_spec,
        prev_stage_for_frame,
        all_prior_cluster_vars = all_prior_cluster_vars
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
        all_prior_cluster_vars, stage_spec$clusters$vars
      ))
    }
    previous_stage_idx <- stage_idx
  }

  if (!is_null(panels) && nrow(current_sample) > 0) {
    current_sample <- assign_panels(
      current_sample, panels, design$stages[[stages[1]]]
    )
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
        input_frames = frames,
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
      panels = panels,
      prev_phase = prev_phase,
      execution_environment = execution_environment,
      integrity = sample_integrity_record(current_sample, design, stages),
      frame_digest = digest
    )
  )
}

#' @noRd
execute_replicated <- function(.data, frames, stages, seed, reps,
                               executor = "design",
                               execution_environment,
                               frame_digest = "none",
                               call = caller_env()) {
  results <- vector("list", reps)
  rep_digests <- vector("list", reps)
  collect_digest <- executor == "design" &&
    !identical(frame_digest, "none")

  for (r in seq_len(reps)) {
    rep_seed <- if (!is_null(seed)) seed + r - 1L else NULL

    run_one <- function() {
      if (executor == "design") {
        execute_design(
          .data, frames, stages, rep_seed, panels = NULL,
          execution_environment = execution_environment,
          frame_digest = if (collect_digest) frame_digest else "none"
        )
      } else {
        execute_continuation(
          .data, frames, stages, rep_seed, panels = NULL,
          execution_environment = execution_environment
        )
      }
    }

    result <- if (!is_null(rep_seed)) {
      withr::with_seed(rep_seed, run_one())
    } else {
      run_one()
    }

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
    if (!is_null(stages)) as.integer(stages) else seq_along(the_design$stages)
  } else {
    already <- get_stages_executed(.data)
    new_s <- if (!is_null(stages)) {
      as.integer(stages)
    } else {
      setdiff(seq_along(the_design$stages), already)
    }
    c(already, new_s)
  }

  integrity <- sample_integrity_record(combined, the_design, the_stages)
  integrity$replicate_hashes <- replicate_integrity_hashes(
    combined, integrity$cols, seq_len(reps)
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
      frame_digest = digest
    )
  )
}

#' @noRd
execute_continuation <- function(sample, frames, stages, seed, panels,
                                 execution_environment,
                                 frame_digest = "none",
                                 call = caller_env()) {
  design <- get_design(sample)
  executed <- get_stages_executed(sample)

  n_stages <- length(design$stages)

  if (is_null(stages)) {
    remaining <- setdiff(seq_len(n_stages), executed)
    if (length(remaining) == 0) {
      cli_abort("All stages have been executed", call = call)
    }
    stages <- remaining
  } else {
    if (
      !is_integerish_numeric(stages) ||
        any(stages < 1) ||
        any(stages > n_stages)
    ) {
      cli_abort("{.arg stages} must be integers between 1 and {n_stages}",
                call = call)
    }
    stages <- sort(as.integer(stages))
    already_done <- intersect(stages, executed)
    if (length(already_done) > 0) {
      cli_abort("Stage{?s} {already_done} already executed", call = call)
    }
    next_expected <- max(executed) + 1L
    if (stages[1] != next_expected) {
      executed_str <- paste(executed, collapse = ", ")
      cli_abort(c(
        "{.arg stages} must continue from stage {next_expected}.",
        "i" = "Stage(s) {executed_str} already executed; next stage must be {next_expected}."
      ), call = call)
    }
    expected <- seq.int(stages[1], stages[length(stages)])
    if (!identical(stages, expected)) {
      cli_abort("{.arg stages} must be contiguous (no gaps)", call = call)
    }
  }

  current_sample <- as.data.frame(sample)
  last_executed_stage <- max(executed)
  previous_stage_idx <- last_executed_stage
  all_prior_cluster_vars <- collect_ancestor_cluster_vars(design, stages[1])

  if (length(frames) == 1) {
    frames <- rep(frames, length(stages))
  } else if (length(frames) != length(stages)) {
    cli_abort(
      "Number of frames ({length(frames)}) must be 1 or match number of stages ({length(stages)})",
      call = call
    )
  }

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

    frame <- subset_frame_to_sample(
      frame,
      current_sample,
      stage_spec,
      prev_stage_for_frame,
      all_prior_cluster_vars = all_prior_cluster_vars
    )

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
        all_prior_cluster_vars, stage_spec$clusters$vars
      ))
    }
    previous_stage_idx <- stage_idx
  }

  if (!is_null(panels) && nrow(current_sample) > 0) {
    first_stage_idx <- c(executed, stages)[1]
    current_sample <- assign_panels(
      current_sample, panels, design$stages[[first_stage_idx]]
    )
  }

  digest <- NULL
  if (!identical(frame_digest, "none")) {
    # Merge only onto a prior digest that still describes its sample;
    # a continuation cannot manufacture the manifest of stages it did
    # not observe.
    prior <- get_frame_digest(sample)
    if (!is_null(prior) && !identical(prior$status, "invalidated")) {
      digest <- tryCatch(
        merge_continuation_digest(
          prior, design, stages,
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
      panels = panels,
      frame_digest = digest,
      continued_from = attr(sample, "metadata"),
      # A stage continuation remains in the same phase. If that phase was
      # itself sampled from an earlier phase, retain the phase link so survey
      # export does not mistake the completed multistage phase for a
      # single-phase design.
      prev_phase = attr(sample, "metadata")$prev_phase,
      execution_environment = execution_environment,
      integrity = sample_integrity_record(
        current_sample, design, c(executed, stages)
      )
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
#' complete nonempty replicate) is a standalone sample; empty siblings
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
      spec$method %in% rs_poisson_methods ||
        identical(spec$method_fixed, FALSE)
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
    c("i" = "Empty realizations are possible under random-size method{?s} {.val {rs_methods}}.")
  } else {
    c("i" = "Empty realizations are possible under Bernoulli and Poisson sampling.")
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
execute_replicated_continuation <- function(sample, frames, stages, seed,
                                            panels,
                                            execution_environment,
                                            frame_digest = "none",
                                            call = caller_env()) {
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
        rep_sample, frames, stages, rep_seed, panels = NULL,
        execution_environment = execution_environment
      )
    }

    result <- if (!is_null(rep_seed)) {
      withr::with_seed(rep_seed, run_one())
    } else {
      run_one()
    }

    df <- as.data.frame(result)
    df$.replicate <- rep.int(r, nrow(df))
    results[[i]] <- df
  }

  combined <- do.call(rbind, results)
  combined$.sample_id <- seq_len(nrow(combined))

  # Derive metadata from inputs, not from last loop iteration.
  the_design <- get_design(sample)
  already_executed <- get_stages_executed(sample)
  cont_stages <- if (!is_null(stages)) {
    as.integer(stages)
  } else {
    setdiff(seq_along(the_design$stages), already_executed)
  }

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
      integrity = {
        integrity <- sample_integrity_record(
          combined, the_design, c(already_executed, cont_stages)
        )
        integrity$replicate_hashes <- replicate_integrity_hashes(
          combined, integrity$cols, rep_ids
        )
        integrity
      }
    )
  )
}

#' Execute a design on replicated multi-phase frame(s)
#'
#' When one or more replicated tbl_samples are passed as frames to a new
#' sampling_design, each replicate must be sampled independently. Non-replicated
#' frames are left unchanged across replicates.
#' @noRd
execute_replicated_multiphase <- function(design, frames, stages, seed,
                                          execution_environment,
                                          call = caller_env()) {
  # Identify which frames are replicated tbl_samples
  is_rep <- vapply(frames, function(f) {
    is_tbl_sample(f) && has_multiple_replicates(f)
  }, logical(1))
  rep_indices <- which(is_rep)

  # Determine replicate IDs; validate consistency across frames
  rep_ids <- sort(unique(frames[[rep_indices[1]]]$.replicate))
  if (length(rep_indices) > 1) {
    for (idx in rep_indices[-1]) {
      other_ids <- sort(unique(frames[[idx]]$.replicate))
      if (!identical(rep_ids, other_ids)) {
        cli_abort(c(
          "Replicated frames have inconsistent replicate IDs.",
          "i" = "Frame {rep_indices[1]} has replicates: {paste(rep_ids, collapse = ', ')}.",
          "i" = "Frame {idx} has replicates: {paste(other_ids, collapse = ', ')}."
        ), call = call)
      }
    }
  }

  results <- vector("list", length(rep_ids))

  for (i in seq_along(rep_ids)) {
    r <- rep_ids[i]

    # Build per-replicate frame set: subset replicated frames, keep others
    rep_frames <- frames
    for (idx in rep_indices) {
      orig <- frames[[idx]]
      sub <- orig[orig$.replicate == r, ]
      sub$.replicate <- NULL
      # Restore tbl_sample class so prepare_multiphase_frame recognizes it
      sub <- new_tbl_sample(
        data = sub,
        design = get_design(orig),
        stages_executed = get_stages_executed(orig),
        seed = attr(orig, "seed"),
        metadata = attr(orig, "metadata")
      )
      rep_frames[[idx]] <- sub
    }

    rep_seed <- if (!is_null(seed)) seed + i - 1L else NULL

    run_one <- function() {
      execute_design(
        design, rep_frames, stages, rep_seed, panels = NULL,
        execution_environment = execution_environment,
        frame_digest = "none"
      )
    }

    result <- if (!is_null(rep_seed)) {
      withr::with_seed(rep_seed, run_one())
    } else {
      run_one()
    }

    df <- as.data.frame(result)
    df$.replicate <- rep.int(r, nrow(df))
    results[[i]] <- df
  }

  combined <- do.call(rbind, results)
  combined$.sample_id <- seq_len(nrow(combined))

  the_stages <- if (!is_null(stages)) {
    as.integer(stages)
  } else {
    seq_along(design$stages)
  }

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
      prev_phase = attr(result, "metadata")$prev_phase,
      execution_environment = execution_environment,
      integrity = {
        integrity <- sample_integrity_record(combined, design, the_stages)
        integrity$replicate_hashes <- replicate_integrity_hashes(
          combined, integrity$cols, rep_ids
        )
        integrity
      }
    )
  )
}

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
  strata_spec <- stage_spec$strata
  cluster_spec <- stage_spec$clusters
  draw_spec <- stage_spec$draw_spec

  validate_frame_vars(frame, stage_spec)

  if (!is_null(cluster_spec)) {
    if (
      !is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)
    ) {
      full_parent_vars <- unique(c(
        all_prior_cluster_vars, previous_stage_spec$clusters$vars
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

      results_list <- lapply(indices_list, function(idxs) {
        data <- frame[idxs, , drop = FALSE]
        sample_clusters(
          data,
          strata_spec,
          cluster_spec,
          draw_spec,
          trace_mode = trace_mode
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
        frame, cluster_data,
        by = by_vars, relationship = "many-to-many"
      )
    }
  } else if (
    !is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)
  ) {
    full_parent_vars <- unique(c(
      all_prior_cluster_vars, previous_stage_spec$clusters$vars
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
      result, previous_sample, previous_stage_spec, all_prior_cluster_vars
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

#' Assign panel labels by systematic interleaving within strata
#' @noRd
assign_panels <- function(result, k, first_stage_spec) {
  strata_spec <- first_stage_spec$strata
  cluster_spec <- first_stage_spec$clusters
  control_quos <- first_stage_spec$draw_spec$control

  if (!is_null(cluster_spec)) {
    # Multi-stage: assign at PSU level, propagate to all units
    cluster_vars <- cluster_spec$vars
    make_key <- function(df) {
      make_group_key(df, cluster_vars)
    }
    all_keys <- make_key(result)
    unique_mask <- !duplicated(all_keys)
    psu_data <- result[unique_mask, , drop = FALSE]
    psu_data <- apply_panel_control_order(
      psu_data,
      strata_spec = strata_spec,
      control_quos = control_quos
    )
    psu_keys <- make_key(psu_data)
    n_psu <- nrow(psu_data)

    if (!is_null(strata_spec)) {
      groups <- split_row_indices(psu_data, strata_spec$vars)
      psu_panel <- integer(n_psu)
      for (idxs in groups$indices) {
        psu_panel[idxs] <- rep_len(seq_len(k), length(idxs))
      }
    } else {
      psu_panel <- rep_len(seq_len(k), n_psu)
    }

    result$.panel <- psu_panel[match(all_keys, psu_keys)]
  } else if (!is_null(strata_spec)) {
    groups <- split_row_indices(result, strata_spec$vars)
    result$.panel <- integer(nrow(result))
    for (idxs in groups$indices) {
      result$.panel[idxs] <- rep_len(seq_len(k), length(idxs))
    }
  } else {
    result$.panel <- rep_len(seq_len(k), nrow(result))
  }
  result
}

#' Apply control sorting to panel assignment units
#' @noRd
apply_panel_control_order <- function(df, strata_spec = NULL, control_quos = NULL) {
  if (is_null(control_quos) || length(control_quos) == 0 || nrow(df) <= 1) {
    return(df)
  }

  if (is_null(strata_spec)) {
    return(arrange(df, !!!control_quos))
  }

  groups <- split_row_indices(df, strata_spec$vars)
  ordered <- lapply(groups$indices, function(idxs) {
    arrange(df[idxs, , drop = FALSE], !!!control_quos)
  })
  bind_rows(ordered)
}

#' Determine join variables for weight compounding
#' @noRd
find_compound_join_vars <- function(
  result,
  previous_sample,
  previous_stage_spec,
  all_prior_cluster_vars = character(0)
) {
  if (is_null(previous_stage_spec)) {
    return(character(0))
  }

  if (!is_null(previous_stage_spec$clusters)) {
    cluster_vars <- unique(c(
      all_prior_cluster_vars, previous_stage_spec$clusters$vars
    ))
    prev_draw_cols <- grep(
      "^\\.draw_\\d+$",
      names(previous_sample),
      value = TRUE
    )
    if (length(prev_draw_cols) > 0 && all(prev_draw_cols %in% names(result))) {
      return(c(cluster_vars, prev_draw_cols))
    }
    return(cluster_vars)
  }

  if (!is_null(previous_stage_spec$strata)) {
    strata_vars <- previous_stage_spec$strata$vars
    return(intersect(
      strata_vars,
      intersect(names(result), names(previous_sample))
    ))
  }

  character(0)
}

#' Compound weights by joining on shared variables
#' @noRd
compound_by_join <- function(result, previous_sample, join_vars, carry_cols) {
  carry_cols_to_select <- setdiff(carry_cols, join_vars)

  prev_data <- previous_sample |>
    distinct(across(all_of(join_vars)), .keep_all = TRUE) |>
    select(
      all_of(join_vars),
      all_of(carry_cols_to_select),
      ".prev_weight" = ".weight"
    )

  result |>
    left_join(prev_data, by = join_vars) |>
    mutate(".weight" = .data$`.weight` * .data$`.prev_weight`) |>
    select(-".prev_weight")
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
  previous_stage_spec,
  all_prior_cluster_vars = character(0)
) {
  carry_cols <- find_carry_forward_cols(previous_sample)
  # A repeated/shared phase frame may already put this transient column on
  # the current result. Do not join a second copy with .x/.y suffixes.
  if ("._prev_phase_weight" %in% names(result)) {
    carry_cols <- setdiff(carry_cols, "._prev_phase_weight")
  }
  join_vars <- find_compound_join_vars(
    result, previous_sample, previous_stage_spec, all_prior_cluster_vars
  )

  if (length(join_vars) > 0) {
    compound_by_join(result, previous_sample, join_vars, carry_cols)
  } else {
    compound_broadcast(result, previous_sample, carry_cols)
  }
}

#' @noRd
subset_frame_to_sample <- function(
  frame,
  sample,
  stage_spec,
  previous_stage_spec = NULL,
  all_prior_cluster_vars = character(0)
) {
  if (!is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)) {
    cluster_vars <- unique(c(
      all_prior_cluster_vars, previous_stage_spec$clusters$vars
    ))
    cluster_vars <- intersect(
      cluster_vars, intersect(names(frame), names(sample))
    )
    selected_clusters <- unique(sample[, cluster_vars, drop = FALSE])
    return(semi_join(frame, selected_clusters, by = cluster_vars))
  }

  join_cols <- character(0)
  if (!is_null(stage_spec$strata)) {
    join_cols <- c(join_cols, stage_spec$strata$vars)
  }
  if (!is_null(stage_spec$clusters)) {
    join_cols <- c(join_cols, stage_spec$clusters$vars)
  }
  if (length(join_cols) == 0 && !is_null(previous_stage_spec$strata)) {
    join_cols <- c(join_cols, previous_stage_spec$strata$vars)
  }
  join_cols <- intersect(join_cols, intersect(names(frame), names(sample)))

  if (length(join_cols) == 0) {
    cli_abort(c(
      "No design-driven columns for linking frames across stages.",
      "i" = "Neither strata nor cluster variables are available as join keys.",
      "i" = "Multi-stage designs require cluster or strata variables to link stages."
    ), call = NULL)
  }
  semi_join(frame, sample, by = join_cols)
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

  frame$.row_id <- seq_len(nrow(frame))
  frame <- dplyr::left_join(
    frame, draw_assignments,
    by = cluster_vars_prev, relationship = "many-to-many"
  )
  frame <- frame[order(frame$.row_id), , drop = FALSE]
  frame$.row_id <- NULL

  split_vars <- c(cluster_vars_prev, prev_draw_cols)
  list(frame = frame, split_vars = split_vars)
}

#' @noRd
prepare_multiphase_frame <- function(frame) {
  if (!is_tbl_sample(frame)) {
    return(list(frame = frame, prev_phase = NULL))
  }

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
  # Internal tbl_sample metadata columns. When a tbl_sample is reused as
  # a frame (continuation or new-phase execute), these are stripped so
  # they do not collide with new-stage metadata or leak into downstream
  # samples as ordinary data columns. `.panel` is included because panel
  # labels are post-hoc sample metadata, not frame attributes.
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
    # draw() refuses these at design time; a design restored from a
    # file bypasses draw(), so execution re-checks.
    if (identical(stage$draw_spec$method_probabilities, "unknown")) {
      abort_unknown_probabilities(stage$draw_spec$method, call = call)
    }
  }

  balanced_stages <- which(vapply(design$stages, function(s) {
    is_balanced_method(s$draw_spec)
  }, logical(1)))
  if (length(balanced_stages) > 2) {
    cli_abort(c(
      "Balanced sampling ({.val balanced}) is supported for at most 2 stages.",
      "i" = "Found {.val balanced} at stages {balanced_stages}."
    ), call = call)
  }

  invisible(TRUE)
}

#' @noRd
validate_frame_vars <- function(frame, stage_spec, call = rlang::caller_env()) {
  if (nrow(frame) == 0) {
    cli_abort("Frame has 0 rows", call = call)
  }

  required_vars <- c()

  strata_vars <- stage_spec$strata$vars
  if (!is_null(strata_vars)) {
    required_vars <- c(required_vars, strata_vars)
  }

  cluster_vars <- stage_spec$clusters$vars
  if (!is_null(cluster_vars)) {
    required_vars <- c(required_vars, cluster_vars)
  }

  mos_var <- stage_spec$draw_spec$mos
  if (!is_null(mos_var)) {
    required_vars <- c(required_vars, mos_var)
  }

  prn_var <- stage_spec$draw_spec$prn
  if (!is_null(prn_var)) {
    required_vars <- c(required_vars, prn_var)
  }

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
    # sample from; an all-zero MOS is handled by the harder PPS error
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
      cli_abort(c(
        "Required {cli::qty(length(missing_aux))} auxiliary variable{?s} not found in frame:",
        "x" = "{.val {missing_aux}}"
      ), call = call)
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
      cli_abort(c(
        "Required count-bound variable{?s} not found in frame:",
        "x" = "{.val {missing_bounds}}"
      ), call = call)
    }
    for (var in bound_vars) {
      if (anyNA(frame[[var]])) {
        cli_abort("Count-bound variable {.var {var}} contains NA values", call = call)
      }
    }
  }

  spread_vars <- stage_spec$draw_spec$spread
  if (!is_null(spread_vars)) {
    missing_spread <- setdiff(spread_vars, names(frame))
    if (length(missing_spread) > 0) {
      cli_abort(c(
        "Required spatial coordinate variable{?s} not found in frame:",
        "x" = "{.val {missing_spread}}"
      ), call = call)
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
