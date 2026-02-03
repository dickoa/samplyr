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
#'
#' @return A `tbl_sample` object (a data frame subclass with sampling
#'   metadata). Contains the selected units plus:
#'   - `.sample_id`: Unique identifier for each sampled unit
#'   - `.weight`: Sampling weight (1/probability)
#'   - `.prob`: Inclusion probability
#'   - Stage and stratum identifiers as appropriate
#'
#' @details
#' ## Execution Patterns
#'
#' ### Single-Stage Execution
#' \preformatted{
#' design |> execute(frame, seed = 42)
#' }
#'
#' ### Multi-Stage with Single Frame
#' For hierarchical data where all stages are in one frame:
#' \preformatted{
#' design |> execute(frame, seed = 42)
#' }
#' The frame must contain all clustering variables and respect nesting.
#'
#' ### Multi-Stage with Multiple Frames
#' When each stage has its own frame:
#' \preformatted{
#' design |> execute(frame1, frame2, frame3, seed = 42)
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
#' phase2 <- design2 |> execute(phase1_updated, seed = 43)
#' }
#' Weights compound automatically in multi-phase designs.
#'
#' ## Weight Calculation
#'
#' Weights are calculated as the inverse of inclusion probabilities:
#' - **SRS**: w = N/n (population size / sample size)
#' - **Stratified**: w_h = N_h/n_h within each stratum
#' - **PPS**: w_i = 1/π_i where π_i is the inclusion probability
#' - **Multi-stage**: Weights compound across stages
#' - **Multi-phase**: Weights compound across phases
#'
#' @examples
#' # Basic SRS execution
#' sample <- sampling_design() |>
#'   draw(n = 100) |>
#'   execute(kenya_health, seed = 42)
#' sample
#'
#' # Stratified execution with proportional allocation
#' sample <- sampling_design() |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(kenya_health, seed = 42)
#' table(sample$facility_type)
#'
#' # Two-stage cluster sample execution
#' sample <- sampling_design() |>
#'   stage(label = "Schools") |>
#'     cluster_by(school_id) |>
#'     draw(n = 30, method = "pps_brewer", mos = enrollment) |>
#'   stage(label = "Students") |>
#'     draw(n = 15) |>
#'   execute(tanzania_schools, seed = 42)
#' length(unique(sample$school_id))  # 30 schools selected
#'
#' # Partial execution: stage 1 only
#' design <- sampling_design() |>
#'   stage(label = "EAs") |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = hh_count) |>
#'   stage(label = "Households") |>
#'     draw(n = 12)
#'
#' # Execute only stage 1 to get selected EAs
#' selected_eas <- execute(design, niger_eas, stages = 1, seed = 42)
#' nrow(selected_eas)  # Number of selected EAs
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [is_tbl_sample()] for testing results,
#' [get_design()] for extracting metadata
#'
#' @export
execute <- function(.data, ..., stages = NULL, seed = NULL) {
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
    if (!is.numeric(seed) || length(seed) != 1) {
      cli_abort("{.arg seed} must be a single integer")
    }
  }

  run_execution <- function() {
    if (is_sampling_design(.data)) {
      execute_design(.data, frames, stages, seed)
    } else if (is_tbl_sample(.data)) {
      execute_continuation(.data, frames, stages, seed)
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
execute_design <- function(design, frames, stages, seed) {
  validate_design_complete(design)

  n_stages <- length(design$stages)

  if (is_null(stages)) {
    stages <- seq_len(n_stages)
  } else {
    if (!is.numeric(stages) || any(stages < 1) || any(stages > n_stages)) {
      cli_abort("{.arg stages} must be integers between 1 and {n_stages}")
    }
    stages <- as.integer(stages)
  }

  if (length(frames) == 1) {
    frames <- rep(frames, length(stages))
  } else if (length(frames) != length(stages)) {
    cli_abort(
      "Number of frames ({length(frames)}) must be 1 or match number of stages to execute ({length(stages)})"
    )
  }

  current_sample <- NULL
  previous_stage_spec <- NULL

  for (i in seq_along(stages)) {
    stage_idx <- stages[i]
    frame <- frames[[i]]
    stage_spec <- design$stages[[stage_idx]]

    is_final_stage_of_execution <- (i == length(stages))
    is_final_stage_of_design <- (stage_idx == length(design$stages))
    is_final_stage <- is_final_stage_of_execution || is_final_stage_of_design

    if (!is_null(current_sample)) {
      frame <- subset_frame_to_sample(
        frame,
        current_sample,
        stage_spec,
        previous_stage_spec
      )
    }

    current_sample <- execute_single_stage(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_idx,
      previous_sample = current_sample,
      previous_stage_spec = previous_stage_spec,
      is_final_stage = is_final_stage
    )

    previous_stage_spec <- stage_spec
  }

  new_tbl_sample(
    data = current_sample,
    design = design,
    stages_executed = stages,
    seed = seed,
    metadata = list(
      n_selected = nrow(current_sample),
      executed_at = Sys.time()
    )
  )
}

#' @noRd
execute_continuation <- function(sample, frames, stages, seed) {
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
    already_done <- intersect(stages, executed)
    if (length(already_done) > 0) {
      cli_abort("Stage{?s} {already_done} already executed")
    }
  }

  current_sample <- as.data.frame(sample)
  last_executed_stage <- max(executed)
  previous_stage_spec <- design$stages[[last_executed_stage]]

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

    is_final_stage_of_execution <- (i == length(stages))
    is_final_stage_of_design <- (stage_idx == length(design$stages))
    is_final_stage <- is_final_stage_of_execution || is_final_stage_of_design

    frame <- subset_frame_to_sample(
      frame,
      current_sample,
      stage_spec,
      previous_stage_spec
    )

    current_sample <- execute_single_stage(
      frame = frame,
      stage_spec = stage_spec,
      stage_num = stage_idx,
      previous_sample = current_sample,
      previous_stage_spec = previous_stage_spec,
      is_final_stage = is_final_stage
    )

    previous_stage_spec <- stage_spec
  }

  new_tbl_sample(
    data = current_sample,
    design = design,
    stages_executed = c(executed, stages),
    seed = seed,
    metadata = list(
      n_selected = nrow(current_sample),
      executed_at = Sys.time(),
      continued_from = attr(sample, "metadata")
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
    result <- sample_clusters(frame, strata_spec, cluster_spec, draw_spec)

    if (is_final_stage) {
      cluster_vars <- cluster_spec$vars
      result <- frame |>
        inner_join(
          result |>
            select(all_of(cluster_vars), ".weight", ".prob", ".sample_id"),
          by = cluster_vars
        )
    }
  } else if (
    !is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)
  ) {
    result <- sample_within_clusters(
      frame,
      strata_spec,
      draw_spec,
      previous_stage_spec$clusters$vars
    )
  } else {
    result <- sample_units(frame, strata_spec, draw_spec)
  }

  result$.stage <- stage_num

  # Store stagewise probability before compounding

  stage_prob_col <- paste0(".prob_", stage_num)
  result[[stage_prob_col]] <- result$.prob

  # Compound weights AND probabilities from previous stages
  if (!is_null(previous_sample) && ".weight" %in% names(previous_sample)) {
    # Identify previous stagewise prob columns to carry forward
    prev_prob_cols <- grep(
      "^\\.prob_\\d+$",
      names(previous_sample),
      value = TRUE
    )

    if (
      !is_null(previous_stage_spec) && !is_null(previous_stage_spec$clusters)
    ) {
      cluster_vars <- previous_stage_spec$clusters$vars
      prev_data <- previous_sample |>
        distinct(across(all_of(cluster_vars)), .keep_all = TRUE) |>
        select(
          all_of(cluster_vars),
          all_of(prev_prob_cols),
          ".prev_weight" = ".weight",
          ".prev_prob" = ".prob"
        )

      result <- result |>
        left_join(prev_data, by = cluster_vars) |>
        mutate(
          ".weight" = .data$`.weight` * .data$`.prev_weight`,
          ".prob" = .data$`.prob` * .data$`.prev_prob`
        ) |>
        select(-".prev_weight", -".prev_prob")
    } else {
      # Non-clustered continuation
      for (col in prev_prob_cols) {
        result[[col]] <- previous_sample[[col]][1]
      }
      result$.weight <- result$.weight * previous_sample$.weight[1]
      result$.prob <- result$.prob * previous_sample$.prob[1]
    }
  }
  result
}

#' @noRd
sample_clusters <- function(frame, strata_spec, cluster_spec, draw_spec) {
  cluster_vars <- cluster_spec$vars
  cluster_frame <- frame |>
    distinct(across(all_of(cluster_vars)), .keep_all = TRUE)
  sample_units(cluster_frame, strata_spec, draw_spec)
}


#' @noRd
sample_within_clusters <- function(
  frame,
  strata_spec,
  draw_spec,
  cluster_vars
) {
  n <- draw_spec$n
  frac <- draw_spec$frac
  round_method <- draw_spec$round %||% "up"

  if (length(cluster_vars) == 1) {
    split_key <- frame[[cluster_vars]]
  } else {
    split_key <- interaction(frame[cluster_vars], drop = TRUE)
  }

  indices_list <- split(seq_len(nrow(frame)), split_key, drop = TRUE)

  results_list <- lapply(indices_list, function(idxs) {
    data <- frame[idxs, , drop = FALSE]
    N_cluster <- length(idxs)

    n_draw <- if (!is_null(n)) {
      min(n, N_cluster)
    } else if (!is_null(frac)) {
      round_sample_size(N_cluster * frac, round_method)
    } else {
      cli_abort("Cannot determine sample size within cluster")
    }

    selected <- draw_sample(data, n_draw, draw_spec)

    selected$.weight <- 1 / selected$.pik
    selected$.prob <- selected$.pik
    selected$.pik <- NULL
    selected
  })

  result <- bind_rows(results_list)
  if (nrow(result) > 0) {
    result$.sample_id <- seq_len(nrow(result))
  }
  result
}

#' @noRd
sample_units <- function(frame, strata_spec, draw_spec) {
  if (!is_null(strata_spec)) {
    sample_stratified(frame, strata_spec, draw_spec)
  } else {
    sample_unstratified(frame, draw_spec)
  }
}

#' @noRd
sample_stratified <- function(frame, strata_spec, draw_spec) {
  strata_vars <- strata_spec$vars
  alloc <- strata_spec$alloc
  stratum_info <- frame |>
    group_by(across(all_of(strata_vars))) |>
    summarise(.N_h = n(), .groups = "drop")

  stratum_info <- calculate_stratum_sizes(stratum_info, strata_spec, draw_spec)

  result <- frame |>
    group_by(across(all_of(strata_vars))) |>
    group_modify(function(data, keys) {
      n_h <- stratum_info |>
        inner_join(keys, by = strata_vars) |>
        pull(".n_h")

      if (length(n_h) == 0 || is.na(n_h)) {
        cli_abort("Could not determine sample size for stratum")
      }

      N_h <- nrow(data)

      stratum_draw_spec <- draw_spec
      if (!is.null(draw_spec$frac) && length(draw_spec$frac) > 1) {
        stratum_id <- as.character(keys[[1]])
        if (stratum_id %in% names(draw_spec$frac)) {
          stratum_draw_spec$frac <- draw_spec$frac[[stratum_id]]
        }
      }

      selected <- draw_sample(data, n_h, stratum_draw_spec)
      selected$.weight <- 1 / selected$.pik
      selected$.prob <- selected$.pik
      selected$.pik <- NULL

      selected
    }) |>
    ungroup()

  result$.sample_id <- seq_len(nrow(result))
  result
}

#' @noRd
sample_unstratified <- function(frame, draw_spec) {
  N <- nrow(frame)
  round_method <- draw_spec$round %||% "up"

  n <- if (!is_null(draw_spec$n)) {
    draw_spec$n
  } else if (!is_null(draw_spec$frac)) {
    round_sample_size(N * draw_spec$frac, round_method)
  } else {
    cli_abort("Cannot determine sample size")
  }

  result <- draw_sample(frame, n, draw_spec)
  result$.weight <- 1 / result$.pik
  result$.prob <- result$.pik
  result$.pik <- NULL
  result$.sample_id <- seq_len(nrow(result))
  result
}

#' @noRd
apply_bounds <- function(target, n_total, min_n, max_n, N_h) {
  H <- length(target)
  adjusted <- target

  effective_min <- if (!is_null(min_n)) pmin(rep(min_n, H), N_h) else rep(0, H)
  effective_max <- if (!is_null(max_n)) pmin(rep(max_n, H), N_h) else N_h

  if (sum(effective_min) > n_total) {
    cli_abort(c(
      "Cannot satisfy minimum sample size constraint",
      "x" = "Minimum allocation requires n >= {sum(effective_min)}",
      "i" = "Total sample size n = {n_total}"
    ))
  }

  if (sum(effective_max) < n_total) {
    cli_abort(c(
      "Cannot satisfy maximum sample size constraint",
      "x" = "Maximum allocation allows at most n = {sum(effective_max)}",
      "i" = "Total sample size n = {n_total}"
    ))
  }

  max_iter <- 50L
  converged <- FALSE

  for (iter in seq_len(max_iter)) {
    changed <- FALSE

    below <- adjusted < effective_min
    if (any(below)) {
      borrowed <- sum(effective_min[below] - adjusted[below])
      adjusted[below] <- effective_min[below]

      can_give <- adjusted > effective_min & !below
      if (any(can_give) && borrowed > 0) {
        excess_above <- adjusted[can_give] - effective_min[can_give]
        total_excess <- sum(excess_above)
        if (total_excess > 0) {
          reduction <- pmin(
            borrowed * (excess_above / total_excess),
            excess_above
          )
          adjusted[can_give] <- adjusted[can_give] - reduction
        }
      }
      changed <- TRUE
    }

    above <- adjusted > effective_max
    if (any(above)) {
      excess <- sum(adjusted[above] - effective_max[above])
      adjusted[above] <- effective_max[above]

      can_receive <- adjusted < effective_max & !above
      if (any(can_receive) && excess > 0) {
        room <- effective_max[can_receive] - adjusted[can_receive]
        total_room <- sum(room)
        if (total_room > 0) {
          addition <- pmin(excess * (room / total_room), room)
          adjusted[can_receive] <- adjusted[can_receive] + addition
        }
      }
      changed <- TRUE
    }

    if (!changed) {
      converged <- TRUE
      break
    }
  }

  if (!converged) {
    final_sum <- sum(round(adjusted))
    cli_warn(c(
      "Bounds adjustment did not converge in {max_iter} iterations",
      "!" = "Stratum allocations may not sum exactly to {n_total}",
      "i" = "Current rounded sum: {final_sum}",
      "i" = "Consider relaxing {.arg min_n} or {.arg max_n} constraints"
    ))
  }

  adjusted <- pmin(adjusted, N_h)
  round_preserve_total_bounded(adjusted, n_total, effective_min, effective_max)
}

#' @noRd
round_preserve_total_bounded <- function(x, n, min_vals, max_vals) {
  floored <- pmax(floor(x), ceiling(min_vals))
  floored <- pmin(floored, floor(max_vals))
  remainders <- x - floored
  shortfall <- n - sum(floored)

  if (shortfall > 0) {
    can_add <- floored < max_vals
    adj_remainders <- ifelse(can_add, remainders, -Inf)
    add_indices <- order(adj_remainders, decreasing = TRUE)[seq_len(min(
      shortfall,
      sum(can_add)
    ))]
    add_indices <- add_indices[
      !is.na(add_indices) & add_indices <= length(floored)
    ]

    if (length(add_indices) > 0) {
      floored[add_indices] <- floored[add_indices] + 1
    }
  } else if (shortfall < 0) {
    can_remove <- floored > min_vals
    adj_remainders <- ifelse(can_remove, remainders, Inf)
    remove_indices <- order(adj_remainders, decreasing = FALSE)[seq_len(min(
      -shortfall,
      sum(can_remove)
    ))]
    remove_indices <- remove_indices[
      !is.na(remove_indices) & remove_indices <= length(floored)
    ]

    if (length(remove_indices) > 0) {
      floored[remove_indices] <- floored[remove_indices] - 1
    }
  }
  as.integer(floored)
}

#' @noRd
round_sample_size <- function(x, round_method = "up") {
  result <- switch(
    round_method,
    up = ceiling(x),
    down = floor(x),
    nearest = round(x)
  )
  pmax(as.integer(result), 1L)
}

#' @noRd
round_preserve_total <- function(x, n) {
  floored <- floor(x)
  remainders <- x - floored
  shortfall <- n - sum(floored)

  if (shortfall > 0) {
    add_indices <- order(remainders, decreasing = TRUE)[seq_len(shortfall)]
    floored[add_indices] <- floored[add_indices] + 1
  }
  as.integer(floored)
}

#' @noRd
calculate_stratum_sizes <- function(stratum_info, strata_spec, draw_spec) {
  alloc <- strata_spec$alloc
  n_total <- draw_spec$n
  frac <- draw_spec$frac
  min_n <- draw_spec$min_n
  max_n <- draw_spec$max_n
  round_method <- draw_spec$round %||% "up"

  N <- sum(stratum_info$.N_h)
  H <- nrow(stratum_info)

  finalize_allocation <- function(target, n_total, N_h) {
    if (!is_null(min_n) || !is_null(max_n)) {
      apply_bounds(target, n_total, min_n, max_n, N_h)
    } else {
      round_preserve_total(target, n_total)
    }
  }

  strata_ids <- if (length(strata_spec$vars) == 1) {
    stratum_info[[strata_spec$vars]]
  } else {
    NULL
  }

  n_is_df <- is.data.frame(n_total)
  frac_is_df <- is.data.frame(frac)

  stratum_info$.n_h <- if (n_is_df) {
    stratum_info <- stratum_info |>
      left_join(n_total, by = strata_spec$vars)
    stratum_info$n
  } else if (frac_is_df) {
    stratum_info <- stratum_info |>
      left_join(frac, by = strata_spec$vars)
    round_sample_size(stratum_info$.N_h * stratum_info$frac, round_method)
  } else if (is_null(alloc)) {
    if (!is_null(n_total)) {
      if (!is_null(names(n_total)) && !is_null(strata_ids)) {
        as.integer(n_total[as.character(strata_ids)])
      } else {
        rep(n_total, H)
      }
    } else if (!is_null(frac)) {
      if (!is_null(names(frac)) && !is_null(strata_ids)) {
        frac_matched <- frac[as.character(strata_ids)]
        round_sample_size(stratum_info$.N_h * frac_matched, round_method)
      } else {
        round_sample_size(stratum_info$.N_h * frac, round_method)
      }
    } else {
      cli_abort("Cannot determine stratum sample sizes")
    }
  } else if (alloc == "equal") {
    target <- rep(n_total / H, H)
    finalize_allocation(target, n_total, stratum_info$.N_h)
  } else if (alloc == "proportional") {
    target <- n_total * stratum_info$.N_h / N
    finalize_allocation(target, n_total, stratum_info$.N_h)
  } else if (alloc == "neyman") {
    var_df <- strata_spec$variance
    stratum_info <- stratum_info |>
      left_join(var_df, by = strata_spec$vars)

    stratum_info$.factor <- stratum_info$.N_h * sqrt(stratum_info$var)
    total_factor <- sum(stratum_info$.factor)
    target <- n_total * stratum_info$.factor / total_factor
    finalize_allocation(target, n_total, stratum_info$.N_h)
  } else if (alloc == "optimal") {
    var_df <- strata_spec$variance
    cost_df <- strata_spec$cost
    stratum_info <- stratum_info |>
      left_join(var_df, by = strata_spec$vars) |>
      left_join(cost_df, by = strata_spec$vars)

    stratum_info$.factor <- stratum_info$.N_h *
      sqrt(stratum_info$var) /
      sqrt(stratum_info$cost)
    total_factor <- sum(stratum_info$.factor)
    target <- n_total * stratum_info$.factor / total_factor
    finalize_allocation(target, n_total, stratum_info$.N_h)
  }
  stratum_info
}

#' @noRd
draw_sample <- function(data, n, draw_spec) {
  method <- draw_spec$method
  mos <- draw_spec$mos
  N <- nrow(data)
  n <- min(n, N)

  pik <- NULL

  if (method == "srswor") {
    idx <- sondage::srs(n, N, replace = FALSE)
    pik <- rep(n / N, N)
  } else if (method == "srswr") {
    idx <- sondage::srs(n, N, replace = TRUE)
    pik <- rep(n / N, N)
  } else if (method == "systematic") {
    idx <- sondage::systematic(n, N)
    pik <- rep(n / N, N)
  } else if (method == "bernoulli") {
    frac <- draw_spec$frac
    if (is.null(frac) || length(frac) == 0) {
      cli_abort("Bernoulli sampling requires {.arg frac} but none was provided")
    }
    pik <- rep(frac, N)
    idx <- sondage::bernoulli(frac, N)
    if (length(idx) == 0) {
      idx <- sondage::srs(1, N)
      pik <- rep(1 / N, N)
    }
  } else if (method == "pps_systematic") {
    mos_vals <- data[[mos]]
    pik <- sondage::inclusion_prob(mos_vals, n)
    idx <- sondage::up_systematic(pik)
  } else if (method == "pps_brewer") {
    mos_vals <- data[[mos]]
    pik <- sondage::inclusion_prob(mos_vals, n)
    idx <- sondage::up_brewer(pik)
  } else if (method == "pps_maxent") {
    mos_vals <- data[[mos]]
    pik <- sondage::inclusion_prob(mos_vals, n)
    idx <- sondage::up_maxent(pik)
  } else if (method == "pps_poisson") {
    mos_vals <- data[[mos]]
    frac <- draw_spec$frac %||% (n / N)
    pik <- frac * mos_vals / sum(mos_vals) * N
    pik <- pmin(pik, 1)
    idx <- sondage::up_poisson(pik)
    if (length(idx) == 0) {
      idx <- sondage::srs(1, N)
      pik <- rep(1 / N, N)
    }
  } else if (method == "pps_multinomial") {
    mos_vals <- data[[mos]]
    pik <- sondage::inclusion_prob(mos_vals, n)
    idx <- sondage::up_multinomial(pik)
  } else {
    cli_abort("Unknown sampling method: {.val {method}}")
  }

  result <- data[idx, , drop = FALSE]
  result$.pik <- pik[idx]
  result
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

  common_cols <- intersect(names(frame), names(sample))
  common_cols <- setdiff(
    common_cols,
    c(".sample_id", ".weight", ".prob", ".stage")
  )

  if (length(common_cols) == 0) {
    cli_warn("No common columns for linking frames across stages")
    return(frame)
  }
  semi_join(frame, sample, by = common_cols)
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
  required_vars <- c()

  if (!is_null(stage_spec$strata)) {
    required_vars <- c(required_vars, stage_spec$strata$vars)
  }

  if (!is_null(stage_spec$clusters)) {
    required_vars <- c(required_vars, stage_spec$clusters$vars)
  }

  if (!is_null(stage_spec$draw_spec$mos)) {
    required_vars <- c(required_vars, stage_spec$draw_spec$mos)
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
  invisible(TRUE)
}

#' @importFrom dplyr semi_join
NULL
