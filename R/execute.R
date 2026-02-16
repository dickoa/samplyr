#' Execute a Sampling Design
#'
#' `execute()` runs a sampling design against one or more data frames,
#' producing a sampled dataset with appropriate weights and metadata.
#'
#' @param .data A `sampling_design` object, or a `tbl_sample` object
#'   for continuation (multi-phase or multi-stage with separate frames).
#' @param ... Data frame(s) to sample from. For single-stage designs, provide
#'   one frame. For multi-stage designs with separate frames, provide frames
#'   in stage order.
#' @param stages Integer vector specifying which stage(s) to execute.
#'   Default (`NULL`) executes all remaining stages.
#' @param seed Integer random seed for reproducibility.
#' @param panels Integer number of rotation groups (panels) to partition the
#'   sample into. Each panel is a representative subsample created by systematic
#'   interleaving within strata. The output includes a `.panel` column with
#'   values 1 through `panels`. Default `NULL` means no panel partitioning.
#'
#' @return A `tbl_sample` object (a data frame subclass with sampling
#'   metadata). Contains the selected units plus:
#'   - `.sample_id`: Unique identifier for each sampled unit
#'   - `.weight`: Sampling weight (1/probability)
#'   - `.weight_1`, `.weight_2`, ...: Per-stage sampling weights
#'     (\eqn{1/\pi_i^{(k)}}{1/pi_i(k)}). The product of all per-stage
#'     weights equals `.weight`.
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
#' The frame must contain all clustering variables and respect nesting.
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
#'
#' ### Multi-Phase (Continuation)
#' When `.data` is a `tbl_sample`, sampling continues from that sample:
#' \preformatted{
#' phase1 <- design1 |> execute(frame, seed = 42)
#' # ... add screening data to phase1 ...
#' phase2 <- design2 |> execute(phase1_updated, seed = 123)
#' }
#' Weights compound automatically in multi-phase designs.
#'
#' ## Weight Calculation
#'
#' The `.weight` column is always the inverse of the inclusion probability.
#' For all methods the per-stage weight is \eqn{w_i^{(k)} = 1 / \pi_i^{(k)}}{w_i(k) = 1 / pi_i(k)}:
#'
#' - **SRS**: \eqn{w_i = N / n}{w = N/n}, constant for all units.
#' - **Stratified SRS**: \eqn{w_i = N_h / n_h}{w = N_h/n_h} within stratum \eqn{h}.
#' - **PPS WOR**: \eqn{w_i = 1 / \pi_i}{w_i = 1/pi_i} where
#'   \eqn{\pi_i}{pi_i} is computed from the measure of size by
#'   `sondage::inclusion_prob()`. Varies across units.
#' - **WR / PMR**: \eqn{w_i = 1 / p_i}{w_i = 1/p_i} where \eqn{p_i}{p_i} is
#'   the single-draw selection probability. Each draw is one row; a unit
#'   selected \eqn{k} times appears \eqn{k} times, each with the same weight.
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
#' When `.data` is itself a `tbl_sample` (two-phase sampling), the
#' phase-1 inclusion probability is already reflected in the input weights.
#' The final `.weight` is the product of phase-1 and phase-2 weights:
#' \deqn{w_i = w_i^{(\text{phase 1})} \times w_i^{(\text{phase 2} \mid \text{phase 1})}}
#' This ensures the Horvitz--Thompson estimator
#' \eqn{\hat{Y} = \sum_S w_i \, y_i}{Y-hat = sum(w_i * y_i)} is unbiased
#' for the population total.
#'
#' ## Panel Partitioning
#'
#' When `panels` is specified, the sample is partitioned into non-overlapping
#' rotation groups suitable for rotating panel surveys. Each panel is a
#' representative subsample created by systematic interleaving within strata.
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
#' inclusion probability. When analysing a single panel, multiply weights by
#' `panels` to obtain per-panel weights.
#'
#' @examples
#' # Basic SRS execution
#' sample <- sampling_design() |>
#'   draw(n = 100) |>
#'   execute(kenya_health, seed = 1234)
#' sample
#'
#' # Stratified execution with proportional allocation
#' sample <- sampling_design() |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(kenya_health, seed = 5789)
#' table(sample$facility_type)
#'
#' # Two-stage cluster sample execution
#' sample <- sampling_design() |>
#'   add_stage(label = "Schools") |>
#'     cluster_by(school_id) |>
#'     draw(n = 30, method = "pps_brewer", mos = enrollment) |>
#'   add_stage(label = "Students") |>
#'     draw(n = 15) |>
#'   execute(tanzania_schools, seed = 3)
#' length(unique(sample$school_id))  # 30 schools selected
#'
#' # Partial execution: stage 1 only
#' design <- sampling_design() |>
#'   add_stage(label = "EAs") |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = hh_count) |>
#'   add_stage(label = "Households") |>
#'     draw(n = 12)
#'
#' # Execute only stage 1 to get selected EAs
#' selected_eas <- execute(design, niger_eas, stages = 1, seed = 2)
#' nrow(selected_eas)  # Number of selected EAs
#'
#' # Rotating panel: 4 rotation groups
#' sample <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 200) |>
#'   execute(niger_eas, seed = 1, panels = 4)
#' table(sample$.panel)  # ~50 per panel
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [is_tbl_sample()] for testing results,
#' [get_design()] for extracting metadata
#'
#' @export
execute <- function(.data, ..., stages = NULL, seed = NULL, panels = NULL) {
  frames <- list(...)

  if (length(frames) == 0) {
    cli_abort("At least one data frame must be provided")
  }

  for (i in seq_along(frames)) {
    if (!is.data.frame(frames[[i]])) {
      cli_abort("Argument {i} to {.fn execute} must be a data frame")
    }
  }

  if (!is_null(seed)) {
    if (length(seed) != 1 || !is_integerish_numeric(seed)) {
      cli_abort("{.arg seed} must be a single integer")
    }
    seed <- as.integer(seed)
  }

  if (!is_null(panels)) {
    if (!is.numeric(panels) || length(panels) != 1 ||
        !is_integerish_numeric(panels) || panels < 2) {
      cli_abort("{.arg panels} must be a single integer >= 2")
    }
    panels <- as.integer(panels)
  }

  run_execution <- function() {
    if (is_sampling_design(.data)) {
      execute_design(.data, frames, stages, seed, panels)
    } else if (is_tbl_sample(.data)) {
      execute_continuation(.data, frames, stages, seed, panels)
    } else {
      cli_abort(
        "{.arg .data} must be a {.cls sampling_design} or {.cls tbl_sample}"
      )
    }
  }

  if (!is_null(seed)) {
    withr::with_seed(seed, run_execution())
  } else {
    run_execution()
  }
}

#' @noRd
execute_design <- function(design, frames, stages, seed, panels) {
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
      cli_abort("{.arg stages} must be integers between 1 and {n_stages}")
    }
    stages <- sort(as.integer(stages))
    if (stages[1] != 1L) {
      cli_abort(c(
        "{.arg stages} must start at stage 1 when executing from a design.",
        "i" = "To continue from a previous sample, pass the {.cls tbl_sample} instead of the design."
      ))
    }
    expected <- seq.int(stages[1], stages[length(stages)])
    if (!identical(stages, expected)) {
      cli_abort("{.arg stages} must be contiguous (no gaps)")
    }
  }

  if (length(frames) == 1) {
    frames <- rep(frames, length(stages))
  } else if (length(frames) != length(stages)) {
    cli_abort(
      "Number of frames ({length(frames)}) must be 1 or match number of stages to execute ({length(stages)})"
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
        prev_stage_for_frame
      )
    }

    current_sample <- execute_single_stage(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_idx,
      previous_sample = current_sample,
      previous_stage_spec = prev_stage_for_frame,
      is_final_stage = is_final_stage
    )

    previous_stage_idx <- stage_idx
  }

  if (!is_null(panels)) {
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

  new_tbl_sample(
    data = current_sample,
    design = design,
    stages_executed = stages,
    seed = seed,
    metadata = list(
      n_selected = nrow(current_sample),
      executed_at = Sys.time(),
      prev_phase = prev_phase
    )
  )
}

#' @noRd
execute_continuation <- function(sample, frames, stages, seed, panels) {
  design <- get_design(sample)
  executed <- get_stages_executed(sample)

  n_stages <- length(design$stages)

  if (is_null(stages)) {
    remaining <- setdiff(seq_len(n_stages), executed)
    if (length(remaining) == 0) {
      cli_abort("All stages have been executed")
    }
    stages <- remaining
  } else {
    if (
      !is_integerish_numeric(stages) ||
        any(stages < 1) ||
        any(stages > n_stages)
    ) {
      cli_abort("{.arg stages} must be integers between 1 and {n_stages}")
    }
    stages <- sort(as.integer(stages))
    already_done <- intersect(stages, executed)
    if (length(already_done) > 0) {
      cli_abort("Stage{?s} {already_done} already executed")
    }
    next_expected <- max(executed) + 1L
    if (stages[1] != next_expected) {
      executed_str <- paste(executed, collapse = ", ")
      cli_abort(c(
        "{.arg stages} must continue from stage {next_expected}.",
        "i" = "Stage(s) {executed_str} already executed; next stage must be {next_expected}."
      ))
    }
    expected <- seq.int(stages[1], stages[length(stages)])
    if (!identical(stages, expected)) {
      cli_abort("{.arg stages} must be contiguous (no gaps)")
    }
  }

  current_sample <- as.data.frame(sample)
  last_executed_stage <- max(executed)
  previous_stage_idx <- last_executed_stage

  if (length(frames) == 1) {
    frames <- rep(frames, length(stages))
  } else if (length(frames) != length(stages)) {
    cli_abort(
      "Number of frames ({length(frames)}) must be 1 or match number of stages ({length(stages)})"
    )
  }

  for (i in seq_along(stages)) {
    stage_idx <- stages[i]
    frame <- frames[[i]]
    stage_spec <- design$stages[[stage_idx]]
    prev_stage_for_frame <- design$stages[[previous_stage_idx]]

    is_final_stage_of_execution <- (i == length(stages))
    is_final_stage_of_design <- (stage_idx == length(design$stages))
    is_final_stage <- is_final_stage_of_execution || is_final_stage_of_design

    frame <- subset_frame_to_sample(
      frame,
      current_sample,
      stage_spec,
      prev_stage_for_frame
    )

    current_sample <- execute_single_stage(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_idx,
      previous_sample = current_sample,
      previous_stage_spec = prev_stage_for_frame,
      is_final_stage = is_final_stage
    )

    previous_stage_idx <- stage_idx
  }

  if (!is_null(panels)) {
    first_stage_idx <- c(executed, stages)[1]
    current_sample <- assign_panels(
      current_sample, panels, design$stages[[first_stage_idx]]
    )
  }

  new_tbl_sample(
    data = current_sample,
    design = design,
    stages_executed = c(executed, stages),
    seed = seed,
    metadata = list(
      n_selected = nrow(current_sample),
      executed_at = Sys.time(),
      continued_from = attr(sample, "metadata"),
      prev_phase = list(
        design = get_design(sample),
        stages = get_stages_executed(sample),
        sample = sample
      )
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
  is_final_stage = FALSE
) {
  strata_spec <- stage_spec$strata
  cluster_spec <- stage_spec$clusters
  draw_spec <- stage_spec$draw_spec

  validate_frame_vars(frame, stage_spec)

  if (!is_null(cluster_spec)) {
    if (
      !is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)
    ) {
      cluster_vars_prev <- previous_stage_spec$clusters$vars
      split_vars <- cluster_vars_prev

      if (!is_null(previous_sample)) {
        attach <- attach_draw_assignments(
          frame,
          previous_sample,
          cluster_vars_prev
        )
        frame <- attach$frame
        split_vars <- attach$split_vars
      }

      indices_list <- split_row_indices(frame, split_vars)$indices

      results_list <- lapply(indices_list, function(idxs) {
        data <- frame[idxs, , drop = FALSE]
        sample_clusters(data, strata_spec, cluster_spec, draw_spec)
      })
      result <- bind_rows(results_list)
      if (nrow(result) > 0) {
        result$.sample_id <- seq_len(nrow(result))
      }
    } else {
      result <- sample_clusters(frame, strata_spec, cluster_spec, draw_spec)
    }

    if (is_final_stage) {
      cluster_vars <- cluster_spec$vars
      join_cols <- c(cluster_vars, ".weight", ".fpc", ".sample_id")
      if (".draw" %in% names(result)) {
        join_cols <- c(join_cols, ".draw")
      }
      if (".certainty" %in% names(result)) {
        join_cols <- c(join_cols, ".certainty")
      }
      cluster_data <- result[, join_cols, drop = FALSE]
      result <- dplyr::inner_join(
        frame, cluster_data,
        by = cluster_vars, relationship = "many-to-many"
      )
    }
  } else if (
    !is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)
  ) {
    cluster_vars_prev <- previous_stage_spec$clusters$vars
    split_vars <- cluster_vars_prev

    if (!is_null(previous_sample)) {
      attach <- attach_draw_assignments(
        frame,
        previous_sample,
        cluster_vars_prev
      )
      frame <- attach$frame
      split_vars <- attach$split_vars
    }

    result <- sample_within_clusters(
      frame,
      strata_spec,
      draw_spec,
      split_vars
    )
  } else {
    result <- sample_units(frame, strata_spec, draw_spec)
  }

  result$.stage <- stage_num

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
    result <- compound_stage_weights(result, previous_sample, previous_stage_spec)
  }
  result
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
    intersect(".panel", nms)
  )
}

#' Assign panel labels by systematic interleaving within strata
#' @noRd
assign_panels <- function(result, k, first_stage_spec) {
  strata_spec <- first_stage_spec$strata
  cluster_spec <- first_stage_spec$clusters

  if (!is_null(cluster_spec)) {
    # Multi-stage: assign at PSU level, propagate to all units
    cluster_vars <- cluster_spec$vars
    make_key <- function(df) {
      if (length(cluster_vars) == 1) return(df[[cluster_vars]])
      do.call(paste, c(df[cluster_vars], list(sep = "\x1f")))
    }
    all_keys <- make_key(result)
    unique_mask <- !duplicated(all_keys)
    psu_keys <- all_keys[unique_mask]
    n_psu <- length(psu_keys)

    if (!is_null(strata_spec)) {
      psu_data <- result[unique_mask, , drop = FALSE]
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

#' Determine join variables for weight compounding
#' @noRd
find_compound_join_vars <- function(result, previous_sample, previous_stage_spec) {
  if (is_null(previous_stage_spec)) {
    return(character(0))
  }

  if (!is_null(previous_stage_spec$clusters)) {
    cluster_vars <- previous_stage_spec$clusters$vars
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
compound_stage_weights <- function(result, previous_sample, previous_stage_spec) {
  carry_cols <- find_carry_forward_cols(previous_sample)
  join_vars <- find_compound_join_vars(result, previous_sample, previous_stage_spec)

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
  previous_stage_spec = NULL
) {
  if (!is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)) {
    cluster_vars <- previous_stage_spec$clusters$vars
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
    cli_warn(c(
      "No design-driven columns for linking frames across stages.",
      "i" = "Neither strata nor cluster variables are available as join keys.",
      "i" = "Returning full frame; results may be incorrect if the frame should be subsetted."
    ))
    return(frame)
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
  nms <- names(x)
  internal_pattern <- "^\\.(weight|fpc|sample_id|stage|draw|certainty)"
  grep(internal_pattern, nms, value = TRUE)
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
  }
  invisible(TRUE)
}

#' @noRd
validate_frame_vars <- function(frame, stage_spec, call = rlang::caller_env()) {
  if (nrow(frame) == 0) {
    cli_abort("Frame has 0 rows", call = call)
  }

  required_vars <- c()

  if (!is_null(stage_spec$strata)) {
    required_vars <- c(required_vars, stage_spec$strata$vars)
  }

  if (!is_null(stage_spec$clusters)) {
    required_vars <- c(required_vars, stage_spec$clusters$vars)
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

  control_vars <- extract_control_vars(stage_spec$draw_spec$control)
  if (length(control_vars) > 0) {
    missing_control <- setdiff(control_vars, names(frame))
    if (length(missing_control) > 0) {
      cli_abort(
        c(
          "Control variable{?s} not found in frame:",
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
