#' Assemble the frame digest of a design execution
#'
#' Turns the selection traces captured during the stage loop into the
#' versioned execution manifest attached under metadata$frame_digest.
#' Assembly is observational: it reads traces, frames, and the design,
#' performs no selection, and consumes no random numbers.
#'
#' @param design The executed sampling design.
#' @param stage_ids Integer stage numbers, aligned with the traces.
#' @param stage_traces One trace tree per stage (NULL for stages that
#'   were never reached because an earlier stage selected nothing).
#' @param stage_frames The frame each stage actually selected from
#'   (post subsetting and draw-assignment attachment).
#' @param input_frames The frames as supplied per stage, before
#'   stage subsetting.
#' @param mode "summary" or "full".
#' @param sample The realized sample rows, used only to attach a
#'   verified sample_row locator to the final-stage trace.
#' @noRd
build_frame_digest <- function(design, stage_ids, stage_traces,
                               stage_frames, input_frames, mode,
                               sample = NULL) {
  reached <- which(!vapply(stage_traces, is_null, logical(1)))
  stage_ids <- stage_ids[reached]
  stage_traces <- stage_traces[reached]
  stage_frames <- stage_frames[reached]
  input_frames <- input_frames[reached]

  frames_reg <- build_digest_frames(design, input_frames)

  stages_out <- vector("list", length(stage_ids))
  parent_registry <- NULL
  for (pos in seq_along(stage_ids)) {
    built <- build_digest_stage(
      design = design,
      stage_idx = stage_ids[pos],
      pos = pos,
      trace = stage_traces[[pos]],
      frame = stage_frames[[pos]],
      frame_ref = frames_reg$ref[pos],
      parent_registry = parent_registry,
      mode = mode
    )
    stage_rec <- built$stage
    registry <- built$registry

    # When one universe frame fed every stage, the pools under
    # unselected parents are deterministically resolvable: the design
    # is unambiguous and first-order chances need no random numbers.
    # Failure to resolve leaves the stage eligible-only.
    if (
      pos > 1L &&
        frames_reg$ref[pos] == frames_reg$ref[1] &&
        identical(stages_out[[pos - 1L]]$scope, "universe") &&
        identical(stages_out[[pos - 1L]]$unit_level, "cluster") &&
        identical(
          stages_out[[pos - 1L]]$chance_kind, "inclusion_probability"
        )
    ) {
      expanded <- tryCatch(
        expand_stage_universe(
          design = design,
          stage_idx = stage_ids[pos],
          stage = stage_rec,
          frame = input_frames[[pos]],
          parent_registry = parent_registry,
          registry = registry
        ),
        error = function(e) NULL
      )
      if (!is_null(expanded)) {
        stage_rec <- expanded$stage
        registry <- expanded$registry
      }
    }
    stages_out[[pos]] <- stage_rec
    parent_registry <- registry
  }

  if (!is_null(sample) && length(stages_out) > 0) {
    stages_out[[length(stages_out)]] <- attach_sample_rows(
      stages_out, design, sample
    )
  }

  digest <- new_frame_digest(
    frames = frames_reg$records,
    stages = stages_out,
    privacy = digest_privacy(mode = mode),
    status = "complete"
  )
  validate_frame_digest(digest)
  digest
}

#' Frame registry: one record per distinct supplied frame
#' @noRd
build_digest_frames <- function(design, input_frames) {
  reqs <- design_requirements(design)
  req_df <- if (length(reqs) == 0) {
    data.frame(column = character(0), role = character(0),
               stage = integer(0))
  } else {
    data.frame(
      column = vapply(reqs, function(r) r$name, character(1)),
      role = vapply(reqs, function(r) r$role, character(1)),
      stage = vapply(reqs, function(r) as.integer(r$stage), integer(1))
    )
  }

  # A single frame recycled across stages is the common case;
  # identical() short-circuits on pointer equality, so the content
  # hash is computed once per distinct object.
  hashes <- character(length(input_frames))
  for (i in seq_along(input_frames)) {
    seen <- NA_integer_
    for (j in seq_len(i - 1L)) {
      if (identical(input_frames[[j]], input_frames[[i]])) {
        seen <- j
        break
      }
    }
    hashes[i] <- if (is.na(seen)) {
      frame_content_hash(input_frames[[i]])
    } else {
      hashes[seen]
    }
  }
  uniq_pos <- which(!duplicated(hashes))
  ref <- match(hashes, hashes[uniq_pos])

  records <- lapply(seq_along(uniq_pos), function(j) {
    pos <- uniq_pos[j]
    frame <- input_frames[[pos]]
    role_cols <- intersect(unique(req_df$column), names(frame))
    roles <- req_df[req_df$column %in% names(frame), , drop = FALSE]
    rownames(roles) <- NULL
    new_digest_frame(
      frame_id = j,
      fingerprint_exact = hashes[pos],
      fingerprint_roles = if (length(role_cols) > 0) {
        frame_content_hash(frame, columns = role_cols)
      } else {
        NULL
      },
      n_rows = nrow(frame),
      roles = roles,
      # The frame supplied to the first executed stage is taken as the
      # population; frames first supplied for a later stage cover the
      # realized parents only.
      scope = if (pos == 1L) "universe" else "eligible"
    )
  })
  list(records = records, ref = ref)
}

#' Flatten a stage trace tree into pool records with global row maps
#'
#' Composes the `rows` mappings down the tree so each pool leaf comes
#' back with the stage-frame rows of its members (input order) and,
#' below a clusters node, the descendant count per member.
#' @noRd
flatten_stage_trace <- function(node, row_map, sizes = NULL) {
  switch(
    node$type,
    pool = list(list(leaf = node, rows = row_map, sizes = sizes)),
    split = {
      out <- lapply(node$groups, function(g) {
        flatten_stage_trace(
          g$node,
          row_map[g$rows],
          if (is_null(sizes)) NULL else sizes[g$rows]
        )
      })
      if (length(out) == 0) list() else do.call(c, out)
    },
    clusters = flatten_stage_trace(
      node$node,
      row_map[node$first_rows],
      sizes = node$sizes
    )
  )
}

#' Canonical ancestry key strings for frame rows
#' @noRd
digest_path_keys <- function(frame, rows, vars) {
  if (length(vars) == 1L) {
    return(as.character(frame[[vars]][rows]))
  }
  make_group_key(frame[rows, vars, drop = FALSE], vars)
}

#' Occurrence counter for a selected-unit vector (1, then 2 for the
#' second hit of the same unit, ...)
#' @noRd
occurrence_index <- function(unit_ids) {
  if (anyDuplicated(unit_ids) == 0L) {
    return(rep(1L, length(unit_ids)))
  }
  as.integer(stats::ave(seq_along(unit_ids), unit_ids, FUN = seq_along))
}

#' @noRd
build_digest_stage <- function(design, stage_idx, pos, trace, frame,
                               frame_ref, parent_registry, mode) {
  stage_spec <- design$stages[[stage_idx]]
  draw_spec <- stage_spec$draw_spec
  strata_vars <- stage_spec$strata$vars
  cluster_vars <- stage_spec$clusters$vars
  is_cluster <- !is_null(cluster_vars)
  ancestor_vars <- intersect(
    collect_ancestor_cluster_vars(design, stage_idx), names(frame)
  )

  records <- flatten_stage_trace(trace, seq_len(nrow(frame)))
  if (length(records) == 0) {
    cli_abort("Stage {stage_idx} produced no selection pools.", call = NULL)
  }

  # Executed order per pool: the leaf's chance/selected vectors are in
  # executed order; perm maps them back to input-order rows.
  for (i in seq_along(records)) {
    r <- records[[i]]
    perm <- r$leaf$perm
    records[[i]]$exec_rows <- if (is_null(perm)) r$rows else r$rows[perm]
    records[[i]]$exec_sizes <- if (is_null(r$sizes)) {
      NULL
    } else if (is_null(perm)) {
      r$sizes
    } else {
      r$sizes[perm]
    }
  }

  first_leaf <- records[[1]]$leaf
  chance_kind <- first_leaf$chance_kind
  order_kind <- first_leaf$order_kind

  # A pool with NA chances is recorded as unavailable: sizes and
  # selections are kept, the chance representation is absent rather
  # than invented. No current producer emits NA chances (unknown-probability
  # methods are refused at draw); this is schema-level hardening.
  unavailable <- vapply(
    records, function(r) anyNA(r$leaf$chance), logical(1)
  )
  is_const <- vapply(records, function(r) {
    ch <- r$leaf$chance
    length(ch) <= 1L || anyNA(ch) || max(ch) - min(ch) < 1e-12
  }, logical(1))
  storage <- if (is_cluster) {
    "units"
  } else if (all(is_const)) {
    "constant"
  } else if (identical(mode, "full")) {
    "units"
  } else {
    "quantiles"
  }

  pool_first_rows <- vapply(
    records, function(r) r$rows[1], integer(1)
  )

  # Parent linkage exists only when the previous stage retained an
  # identifiable unit registry (a cluster stage). After an element
  # stage, later pools are conditioned on the realized selection as a
  # whole and carry no per-unit parent.
  parent_unit <- rep(NA_integer_, length(records))
  if (pos > 1L && length(ancestor_vars) > 0 && !is_null(parent_registry)) {
    keys <- digest_path_keys(frame, pool_first_rows, ancestor_vars)
    parent_unit <- unname(parent_registry[keys])
    if (anyNA(parent_unit)) {
      cli_abort(
        "Stage {stage_idx} pools reference parents that stage
         {stage_idx - 1L} did not record.",
        call = NULL
      )
    }
  }

  pools <- data.frame(
    pool_id = seq_along(records),
    parent_unit = as.integer(parent_unit)
  )
  # After a with-replacement parent stage, the frame carries draw
  # columns and pools exist per (parent, occurrence): record which hit
  # of the parent each pool hangs under.
  draw_cols <- grep("^\\.draw_\\d+$", names(frame), value = TRUE)
  if (pos > 1L && length(draw_cols) > 0) {
    draw_col <- draw_cols[
      order(as.integer(sub("^\\.draw_", "", draw_cols)))
    ]
    draw_col <- draw_col[length(draw_col)]
    occ <- frame[[draw_col]][pool_first_rows]
    if (!anyNA(occ)) {
      pools$parent_occurrence <- as.integer(occ)
    }
  }
  if (!is_null(strata_vars)) {
    pools <- cbind(
      pools,
      frame[pool_first_rows, strata_vars, drop = FALSE]
    )
    rownames(pools) <- NULL
  }
  pools$N <- vapply(records, function(r) as.integer(r$leaf$N), integer(1))
  pools$n_target <- vapply(
    records, function(r) as.double(r$leaf$n_target), numeric(1)
  )
  pools$n_expected <- vapply(
    records,
    function(r) {
      chance <- r$leaf$chance
      if (length(chance) == 1L && r$leaf$N > 1L) {
        chance * r$leaf$N
      } else {
        sum(chance)
      }
    },
    numeric(1)
  )
  pools$n_realized <- vapply(
    records, function(r) length(r$leaf$selected), integer(1)
  )
  pools$scope <- rep(if (pos == 1L) "universe" else "eligible",
                     length(records))
  pools$chance_status <- ifelse(
    unavailable,
    "unavailable",
    if (identical(storage, "quantiles")) "summarized" else "executed"
  )
  if (identical(storage, "constant")) {
    pools$chance <- vapply(records, function(r) {
      if (anyNA(r$leaf$chance)) {
        NA_real_
      } else if (r$leaf$N == 0) {
        0
      } else {
        r$leaf$chance[1]
      }
    }, numeric(1))
  }

  units <- NULL
  registry <- NULL
  unit_ids_by_pool <- NULL
  if (identical(storage, "units")) {
    counts <- vapply(records, function(r) as.integer(r$leaf$N), integer(1))
    ends <- cumsum(counts)
    starts <- ends - counts + 1L
    unit_ids_by_pool <- lapply(seq_along(records), function(i) {
      seq.int(starts[i], length.out = counts[i])
    })
    chance_all <- unlist(
      lapply(records, function(r) r$leaf$chance), use.names = FALSE
    )
    units <- data.frame(
      unit_id = seq_len(sum(counts)),
      pool_id = rep.int(seq_along(records), counts),
      unit_order = sequence(counts),
      chance = chance_all,
      is_certainty = if (identical(chance_kind, "inclusion_probability")) {
        chance_all >= 1 - 1e-9
      } else {
        rep(NA, sum(counts))
      }
    )
    if (is_cluster) {
      units$n_descendants <- as.integer(unlist(
        lapply(records, function(r) r$exec_sizes), use.names = FALSE
      ))
      # Registry for the next stage: full ancestry key per cluster.
      key_vars <- c(ancestor_vars, cluster_vars)
      all_exec_rows <- unlist(
        lapply(records, function(r) r$exec_rows), use.names = FALSE
      )
      registry <- setNames(
        units$unit_id,
        digest_path_keys(frame, all_exec_rows, key_vars)
      )
    }
  }

  chance_distribution <- NULL
  if (identical(storage, "quantiles")) {
    chance_distribution <- digest_quantile_bins(
      chance_by_pool = lapply(records, function(r) r$leaf$chance),
      pool_ids = seq_along(records),
      pool_sizes = pools$N
    )
  }

  sel_list <- lapply(seq_along(records), function(i) {
    sel <- records[[i]]$leaf$selected
    if (length(sel) == 0) {
      return(NULL)
    }
    unit_ids <- if (!is_null(unit_ids_by_pool)) {
      unit_ids_by_pool[[i]][sel]
    } else {
      as.integer(sel)
    }
    out <- data.frame(
      pool_id = i,
      unit_id = unit_ids,
      occurrence = if (identical(chance_kind, "inclusion_probability")) {
        rep(1L, length(sel))
      } else {
        occurrence_index(unit_ids)
      }
    )
    if (!is_null(registry)) {
      # Ancestry keys of SELECTED clusters only: derivable from the
      # sample rows, so no anonymity leak. A continuation uses them to
      # link its pools back to this stage's units.
      out$key <- names(registry)[unit_ids]
    }
    out
  })
  sel_list <- sel_list[!vapply(sel_list, is_null, logical(1))]
  selected <- if (length(sel_list) == 0) {
    NULL
  } else {
    out <- do.call(rbind, sel_list)
    rownames(out) <- NULL
    out
  }

  stage <- new_digest_stage(
    stage_id = stage_idx,
    frame_ref = frame_ref,
    unit_level = if (is_cluster) "cluster" else "element",
    scope = if (pos == 1L) "universe" else "eligible",
    chance_kind = chance_kind,
    probabilities = draw_spec$method_probabilities,
    order_kind = order_kind,
    storage = storage,
    pools = pools,
    units = units,
    chance_distribution = chance_distribution,
    selected = selected,
    strata = strata_vars,
    diagnostics = build_stage_diagnostics(
      draw_spec, frame, records,
      is_cluster = is_cluster,
      ancestor_vars = ancestor_vars,
      cluster_vars = cluster_vars
    )
  )
  list(stage = stage, registry = registry)
}

#' Attach a verified sample_row locator to the final-stage trace
#'
#' Sample rows of an element final stage are the concatenation of the
#' per-pool selections in trace order, so the locator is positional.
#' It is attached only after that correspondence is verified against
#' the sample's own pool-identity columns: per-row stratum values, and
#' the parent ancestry through the previous stage's selected keys. Any
#' mismatch leaves the trace without locators rather than attaching
#' wrong ones.
#' @noRd
attach_sample_rows <- function(stages, design, sample) {
  stage <- stages[[length(stages)]]
  sel <- stage$selected
  if (
    is_null(sel) ||
      !identical(stage$unit_level, "element") ||
      nrow(sel) != nrow(sample)
  ) {
    return(stage)
  }
  pools <- stage$pools
  block_pool <- match(sel$pool_id, pools$pool_id)

  for (v in stage$strata %||% character(0)) {
    if (!v %in% names(sample)) {
      return(stage)
    }
    if (
      !identical(
        as.character(sample[[v]]),
        as.character(pools[[v]][block_pool])
      )
    ) {
      return(stage)
    }
  }

  if (length(stages) > 1L && !all(is.na(pools$parent_unit))) {
    prev <- stages[[length(stages) - 1L]]
    prev_sel <- prev$selected
    ancestor_vars <- collect_ancestor_cluster_vars(
      design, stage$stage_id
    )
    if (
      is_null(prev_sel) || !"key" %in% names(prev_sel) ||
        !all(ancestor_vars %in% names(sample))
    ) {
      return(stage)
    }
    key_of_unit <- prev_sel$key[
      match(pools$parent_unit, prev_sel$unit_id)
    ]
    expected <- key_of_unit[block_pool]
    actual <- digest_path_keys(
      sample, seq_len(nrow(sample)), ancestor_vars
    )
    if (!identical(actual, expected)) {
      return(stage)
    }
  }

  sel$sample_row <- seq_len(nrow(sel))
  stage$selected <- sel
  stage
}

#' Resolve a stage's unreached pools from the universe frame
#'
#' Enumerates every parent x strata pool of the stage over the full
#' frame and, for pools the execution never reached (their parent was
#' not selected), resolves the first-order chances the design would
#' have used: allocation from the draw specification, chances from the
#' same formulas the selection engine applies. Resolution refuses,
#' by erroring, whenever the design's chance for a pool cannot be
#' known without running it (per-stratum allocation tables, control
#' ordering, non-constant element chances); the caller then keeps the
#' eligible-only stage.
#'
#' Resolved pools carry chance_status "design_resolved", scope
#' "universe", and n_realized 0; the stage scope becomes "universe".
#' @noRd
expand_stage_universe <- function(design, stage_idx, stage, frame,
                                  parent_registry, registry) {
  spec <- design$stages[[stage_idx]]
  draw_spec <- spec$draw_spec
  strata_vars <- spec$strata$vars
  cluster_vars <- spec$clusters$vars
  is_cluster <- !is_null(cluster_vars)
  ancestor_vars <- collect_ancestor_cluster_vars(design, stage_idx)

  resolvable <- length(ancestor_vars) > 0 &&
    all(c(ancestor_vars, strata_vars, cluster_vars) %in% names(frame)) &&
    identical(stage$order_kind, "input") &&
    is_null(spec$strata$alloc) &&
    is_null(draw_spec$min_n) && is_null(draw_spec$max_n) &&
    !is.data.frame(draw_spec$n) && !is.data.frame(draw_spec$frac) &&
    !is.data.frame(draw_spec$certainty_size) &&
    !is.data.frame(draw_spec$certainty_prop) &&
    length(draw_spec$n %||% 1) == 1 &&
    length(draw_spec$frac %||% 1) == 1 &&
    is_null(names(draw_spec$n)) && is_null(names(draw_spec$frac)) &&
    (identical(stage$storage, "units") ||
       identical(stage$storage, "constant"))
  if (!resolvable) {
    stop("stage is not deterministically resolvable")
  }
  mos <- draw_spec$mos
  if (!is_null(mos) && !mos %in% names(frame)) {
    stop("measure of size not present in the universe frame")
  }

  keys_all <- digest_path_keys(frame, seq_len(nrow(frame)), ancestor_vars)
  parent_all <- unname(parent_registry[keys_all])
  if (anyNA(parent_all)) {
    stop("universe frame contains parents the previous stage did not record")
  }

  frame_pool_keys <- data.frame(.parent = parent_all)
  executed_pool_keys <- data.frame(.parent = stage$pools$parent_unit)
  pool_vars <- ".parent"
  if (!is_null(strata_vars)) {
    frame_pool_keys[strata_vars] <- frame[strata_vars]
    executed_pool_keys[strata_vars] <- stage$pools[strata_vars]
    pool_vars <- c(pool_vars, strata_vars)
  }
  pool_key <- make_group_key(frame_pool_keys, pool_vars)
  executed_key <- make_group_key(executed_pool_keys, pool_vars)
  groups <- split(
    seq_len(nrow(frame)), factor(pool_key, levels = unique(pool_key))
  )
  new_keys <- setdiff(names(groups), executed_key)
  if (length(new_keys) == 0) {
    return(list(stage = stage, registry = registry))
  }

  pools <- stage$pools
  units <- stage$units
  next_pool <- max(pools$pool_id) + 1L
  first_rows <- vapply(groups[new_keys], function(r) r[1], integer(1))

  make_pools <- function(N, n_target, n_expected, chance = NULL) {
    out <- data.frame(
      pool_id = seq.int(next_pool, length.out = length(new_keys)),
      parent_unit = parent_all[first_rows]
    )
    if (!is_null(strata_vars)) {
      out <- cbind(out, frame[first_rows, strata_vars, drop = FALSE])
      rownames(out) <- NULL
    }
    out$N <- as.integer(N)
    out$n_target <- n_target
    out$n_expected <- n_expected
    out$n_realized <- 0L
    out$scope <- "universe"
    out$chance_status <- "design_resolved"
    if (!is_null(chance)) {
      out$chance <- chance
    }
    out
  }

  if (!is_cluster) {
    # An element pool without a size measure has one constant chance,
    # resolvable for every pool at once. With a size measure the
    # chances vary within the pool: not representable as constant.
    if (
      !is_null(mos) ||
        !is_null(draw_spec$certainty_size) ||
        !is_null(draw_spec$certainty_prop)
    ) {
      stop("element chances vary within an unreached pool")
    }
    N_vec <- lengths(groups[new_keys])
    value <- vapply(N_vec, function(N) {
      resolved <- resolve_pool_chance(draw_spec, NULL, N)
      resolved$chance[1]
    }, numeric(1))
    random_size <- draw_spec$method %in% rs_poisson_methods ||
      isFALSE(draw_spec$method_fixed)
    n_target <- if (
      random_size && !is.null(draw_spec$n) &&
        is.numeric(draw_spec$n) && length(draw_spec$n) == 1L
    ) {
      rep(as.double(draw_spec$n), length(N_vec))
    } else if (
      random_size && !is.null(draw_spec$frac) &&
        is.numeric(draw_spec$frac) && length(draw_spec$frac) == 1L
    ) {
      as.double(N_vec * draw_spec$frac)
    } else if (!is.null(draw_spec$n)) {
      rep(as.double(draw_spec$n), length(N_vec))
    } else {
      vapply(N_vec, function(N) {
        as.double(round_sample_size(
          N * draw_spec$frac, draw_spec$round %||% "up"
        ))
      }, numeric(1))
    }
    stage$pools <- rbind(
      pools, make_pools(N_vec, n_target, value * N_vec, chance = value)
    )
    rownames(stage$pools) <- NULL
    stage$scope <- "universe"
    return(list(stage = stage, registry = registry))
  }

  next_unit <- if (is_null(units)) 1L else max(units$unit_id) + 1L
  new_units <- list()
  new_reg <- integer(0)
  N_vec <- integer(length(new_keys))
  n_target_vec <- numeric(length(new_keys))
  expected_vec <- numeric(length(new_keys))

  for (i in seq_along(new_keys)) {
    rows <- groups[[new_keys[i]]]
    child_keys <- digest_path_keys(
      frame, rows, c(ancestor_vars, cluster_vars)
    )
    first_of <- !duplicated(child_keys)
    child_first_rows <- rows[first_of]
    N <- length(child_first_rows)
    mos_vals <- if (!is_null(mos)) frame[[mos]][child_first_rows]
    n_desc <- as.integer(
      table(factor(child_keys, levels = child_keys[first_of]))
    )

    resolved <- resolve_pool_chance(draw_spec, mos_vals, N)
    N_vec[i] <- N
    n_target_vec[i] <- resolved$n_target
    expected_vec[i] <- sum(resolved$chance)

    ids <- seq.int(next_unit, length.out = N)
    new_units[[i]] <- data.frame(
      unit_id = ids,
      pool_id = next_pool + i - 1L,
      unit_order = seq_len(N),
      chance = resolved$chance,
      is_certainty = resolved$chance >= 1 - 1e-9,
      n_descendants = n_desc
    )
    new_reg <- c(new_reg, setNames(ids, child_keys[first_of]))
    next_unit <- next_unit + N
  }

  stage$pools <- rbind(
    pools, make_pools(N_vec, n_target_vec, expected_vec)
  )
  rownames(stage$pools) <- NULL
  stage$units <- rbind(units, do.call(rbind, new_units))
  rownames(stage$units) <- NULL
  stage$scope <- "universe"
  list(stage = stage, registry = c(registry, new_reg))
}

#' First-order chances of one pool, from the design alone
#'
#' Mirrors the chance computations of draw_sample() without drawing:
#' the selection algorithm changes which units come out, never their
#' first-order chances. Errors when the chance is not deterministic
#' from (method, mos, n, frac).
#' @noRd
resolve_pool_chance <- function(draw_spec, mos_vals, N) {
  if (identical(draw_spec$method_probabilities, "unknown")) {
    stop(
      "the registered method declares its selection probabilities ",
      "unknown, so they cannot be resolved from the design"
    )
  }
  method <- draw_spec$method
  wr <- is_multi_hit_method(draw_spec)
  random_size <- method %in% rs_poisson_methods ||
    isFALSE(draw_spec$method_fixed)

  n <- draw_spec$n
  frac <- draw_spec$frac
  if (is_null(n) && !is_null(frac) &&
        !method %in% c("bernoulli", "pps_poisson")) {
    n <- round_sample_size(N * frac, draw_spec$round %||% "up")
  }
  n_target <- if (
    random_size && !is.null(frac) &&
      is.numeric(frac) && length(frac) == 1L
  ) {
    as.double(N * frac)
  } else if (!is.null(n)) {
    as.double(n)
  } else {
    NA_real_
  }
  if (!wr && !is_null(n)) {
    n <- min(n, N)
  }

  base_chance <- function(mos_vals, n, N) {
    if (!is_null(draw_spec$method_type)) {
      return(switch(
        draw_spec$method_type,
        wr = sondage::expected_hits(mos_vals, n),
        balanced = if (!is_null(draw_spec$mos)) {
          sondage::inclusion_prob(mos_vals, n)
        } else {
          rep(n / N, N)
        },
        sondage::inclusion_prob(mos_vals, n)
      ))
    }
    switch(
      method,
      srswor = ,
      systematic = rep(n / N, N),
      srswr = rep(n / N, N),
      bernoulli = rep(frac %||% (n / N), N),
      pps_poisson = pmin(
        (frac %||% (n / N)) * mos_vals / sum(mos_vals) * N, 1
      ),
      pps_multinomial = ,
      pps_chromy = sondage::expected_hits(mos_vals, n),
      lpm2 = ,
      scps = ,
      cube = if (!is_null(draw_spec$mos)) {
        sondage::inclusion_prob(mos_vals, n)
      } else {
        rep(n / N, N)
      },
      sondage::inclusion_prob(mos_vals, n)
    )
  }

  has_cert <- !is_null(draw_spec$certainty_size) ||
    !is_null(draw_spec$certainty_prop)
  if (!has_cert) {
    return(list(chance = base_chance(mos_vals, n, N), n_target = n_target))
  }

  cert <- identify_certainty(
    mos_vals = mos_vals,
    n = n,
    certainty_size = draw_spec$certainty_size,
    certainty_prop = draw_spec$certainty_prop
  )
  if (
    cert$n_remaining < 0 &&
      !identical(draw_spec$certainty_overflow, "allow")
  ) {
    stop("certainty overflow cannot be resolved without executing")
  }
  chance <- numeric(N)
  chance[cert$certainty_idx] <- 1
  if (cert$n_remaining > 0 && length(cert$remaining_idx) > 0) {
    n_prob <- min(cert$n_remaining, length(cert$remaining_idx))
    chance[cert$remaining_idx] <- base_chance(
      mos_vals[cert$remaining_idx], n_prob, length(cert$remaining_idx)
    )
  }
  list(chance = chance, n_target = n_target)
}

#' Equal-count quantile bins of per-pool chance vectors
#'
#' Equal-count bins over the sorted chance vector, storing each bin's
#' mean and unit count. The n_units-weighted sum reproduces n_expected
#' exactly, unlike an interpolated quantile grid, which is not
#' mean-faithful under heavy skew. Pools with at most 101 units come
#' back exact (one unit per bin).
#' @noRd
digest_quantile_bins <- function(chance_by_pool, pool_ids, pool_sizes) {
  n_pools <- length(chance_by_pool)
  if (
    !is.list(chance_by_pool) ||
      length(pool_ids) != n_pools ||
      length(pool_sizes) != n_pools
  ) {
    abort_samplyr(
      "Internal digest invariant failed: chance vectors, pool ids, and
       pool sizes must have equal lengths.",
      class = "samplyr_error_internal",
      call = NULL
    )
  }
  if (
    !is_integerish_numeric(pool_sizes) ||
      any(pool_sizes < 0) ||
      any(pool_sizes > .Machine$integer.max)
  ) {
    abort_samplyr(
      "Internal digest invariant failed: pool sizes must be
       nonnegative integers.",
      class = "samplyr_error_internal",
      call = NULL
    )
  }

  dist_list <- lapply(seq_along(chance_by_pool), function(i) {
    ch <- chance_by_pool[[i]]
    pool_size <- as.integer(pool_sizes[[i]])
    if (!length(ch) %in% c(1L, pool_size)) {
      abort_samplyr(
        "Internal digest invariant failed: pool {pool_ids[[i]]} has
         {length(ch)} stored chance values for declared size
         {pool_size}.",
        class = "samplyr_error_internal",
        call = NULL
      )
    }
    if (pool_size == 0L) {
      return(data.frame(
        pool_id = pool_ids[i][0],
        quantile = numeric(0),
        chance = numeric(0),
        n_units = integer(0)
      ))
    }
    if (length(ch) == 1L && pool_size > 1L) {
      return(data.frame(
        pool_id = pool_ids[i],
        quantile = 0.5,
        chance = as.double(ch),
        n_units = pool_size
      ))
    }

    ch <- sort(ch)
    n <- pool_size
    k <- min(101L, n)
    ends <- as.integer(floor(n * seq_len(k) / k))
    starts <- c(1L, ends[-k] + 1L)
    data.frame(
      pool_id = pool_ids[i],
      quantile = ((starts + ends) / 2 - 0.5) / n,
      chance = vapply(
        seq_len(k),
        function(j) mean(ch[starts[j]:ends[j]]),
        numeric(1)
      ),
      n_units = ends - starts + 1L
    )
  })
  out <- do.call(rbind, dist_list)
  rownames(out) <- NULL
  out
}

#' Resolve a design's full ex-ante digest from a frame, without drawing
#'
#' Enumerates every pool of every stage over the universe frame and
#' resolves the first-order chances the design would use, exactly as
#' execution resolves them: parents split before strata, stratum
#' allocation replayed, chances from the same formulas the selection
#' engine applies. Nothing is drawn and no random numbers are
#' consumed: every pool carries chance_status `"design_resolved"`,
#' scope `"universe"` and `n_realized` 0, with no selected trace.
#'
#' Refuses, with class `samplyr_error_exante_unsupported`, designs
#' whose pool structure depends on the realization: a stage under a
#' with-replacement parent (the occurrence count of each parent is
#' random) or under an element-level stage (no unit registry to nest
#' pools in).
#'
#' This is an extension API for packages that build on the digest
#' (such as samplens, which draws the ex-ante sampling card from it).
#' The list structure of the returned digest is internal and
#' versioned: it may change between samplyr releases, so extension
#' packages should pin a minimum samplyr version.
#'
#' @param design A complete `sampling_design` (a `draw()` at every
#'   stage).
#' @param frame The sampling frame the design targets.
#' @param call Environment reported as the error call.
#' @return An ex-ante frame digest list.
#' @seealso [get_frame_digest()], [frame_summary()]
#' @keywords internal
#' @export
build_exante_digest <- function(design, frame,
                                call = rlang::caller_env()) {
  stages_spec <- design$stages
  incomplete <- length(stages_spec) == 0 ||
    any(vapply(
      stages_spec, function(s) is_null(s$draw_spec), logical(1)
    ))
  if (incomplete) {
    abort_samplyr(
      c(
        "The design has no complete {.fn draw} specification.",
        "i" = "The ex-ante digest resolves selection chances from
               the design, so every stage needs a {.fn draw}."
      ),
      class = "samplyr_error_exante_unsupported",
      call = call
    )
  }
  if (!is.data.frame(frame)) {
    abort_samplyr(
      "{.arg frame} must be a data frame.",
      call = call
    )
  }
  for (spec in stages_spec) {
    validate_frame_vars(frame, spec, call = call)
  }

  stages_out <- vector("list", length(stages_spec))
  pool_keys <- vector("list", length(stages_spec))
  registry <- NULL
  for (stage_idx in seq_along(stages_spec)) {
    if (stage_idx > 1L) {
      prev <- stages_spec[[stage_idx - 1L]]
      if (is_null(prev$clusters)) {
        abort_samplyr(
          c(
            "Stage {stage_idx} cannot be resolved ex-ante.",
            "x" = "Stage {stage_idx - 1L} selects elements, so the
                   rows that feed stage {stage_idx} depend on the
                   realization.",
            "i" = "Execute the design and plot the sample instead."
          ),
          class = "samplyr_error_exante_unsupported",
          call = call
        )
      }
      if (is_multi_hit_method(prev$draw_spec)) {
        abort_samplyr(
          c(
            "Stage {stage_idx} cannot be resolved ex-ante.",
            "x" = "Stage {stage_idx - 1L} draws with replacement, so
                   the occurrence count of each parent cluster is
                   random.",
            "i" = "Execute the design and plot the sample instead."
          ),
          class = "samplyr_error_exante_unsupported",
          call = call
        )
      }
    }
    # samplyr conditions are the same informative errors execution
    # gives (allocation coverage, invariance) and pass through; plain
    # stops from the chance resolvers are wrapped with the stage.
    built <- tryCatch(
      build_exante_stage(design, stage_idx, frame, registry),
      error = function(e) {
        if (inherits(e, "samplyr_error")) {
          stop(e)
        }
        reason <- conditionMessage(e)
        abort_samplyr(
          c(
            "Stage {stage_idx} cannot be resolved ex-ante.",
            "x" = "{reason}"
          ),
          class = "samplyr_error_exante_unsupported",
          call = call
        )
      }
    )
    stages_out[[stage_idx]] <- built$stage
    registry <- built$registry
    pool_keys[[stage_idx]] <- built$pool_keys
  }

  frames_reg <- build_digest_frames(
    design, rep(list(frame), length(stages_spec))
  )
  digest <- new_frame_digest(
    frames = frames_reg$records,
    stages = stages_out,
    privacy = digest_privacy(mode = "summary"),
    status = "complete"
  )
  validate_frame_digest(digest)
  # Parent ancestry key per pool and stage, for callers that need to
  # line ex-ante pools up with another digest's pools (drift). An
  # attribute, not schema: ex-ante digests never leave the package.
  attr(digest, "exante_pool_keys") <- pool_keys
  digest
}

#' One ex-ante stage record: every pool enumerated, chances resolved
#' @noRd
build_exante_stage <- function(design, stage_idx, frame,
                               parent_registry) {
  spec <- design$stages[[stage_idx]]
  draw_spec <- spec$draw_spec
  strata_spec <- spec$strata
  strata_vars <- strata_spec$vars
  cluster_vars <- spec$clusters$vars
  is_cluster <- !is_null(cluster_vars)
  ancestor_vars <- collect_ancestor_cluster_vars(design, stage_idx)
  wr <- is_multi_hit_method(draw_spec)

  if (
    is_null(strata_vars) &&
      is_null(draw_spec$n) && is_null(draw_spec$frac)
  ) {
    cli_abort("Cannot determine sample size", call = NULL)
  }

  if (is_cluster) {
    check_exante_cluster_invariants(
      frame, draw_spec, strata_vars, ancestor_vars, cluster_vars
    )
  }

  if (stage_idx == 1L) {
    parent_groups <- list(seq_len(nrow(frame)))
    parent_ids <- NA_integer_
  } else {
    keys_all <- digest_path_keys(
      frame, seq_len(nrow(frame)), ancestor_vars
    )
    parent_all <- unname(parent_registry[keys_all])
    parent_groups <- split(seq_len(nrow(frame)), parent_all)
    parent_ids <- as.integer(names(parent_groups))
  }

  lookup <- if (!is_null(strata_vars)) {
    prepare_stratum_draw_lookup(draw_spec, strata_vars)
  }

  pools_acc <- list()
  add_pool <- function(parent, urows, pool_spec, n_desc, keys) {
    mos_vals <- if (!is_null(draw_spec$mos)) {
      frame[[draw_spec$mos]][urows]
    }
    resolved <- resolve_pool_chance(pool_spec, mos_vals, length(urows))
    pools_acc[[length(pools_acc) + 1L]] <<- list(
      parent = parent,
      first_row = urows[1],
      N = length(urows),
      n_target = resolved$n_target,
      chance = resolved$chance,
      n_desc = n_desc,
      keys = keys
    )
  }

  for (g in seq_along(parent_groups)) {
    rows <- parent_groups[[g]]
    if (is_cluster) {
      ckeys <- digest_path_keys(
        frame, rows, c(ancestor_vars, cluster_vars)
      )
      first_of <- !duplicated(ckeys)
      unit_rows <- rows[first_of]
      n_desc <- as.integer(
        table(factor(ckeys, levels = ckeys[first_of]))
      )
      unit_keys <- ckeys[first_of]
    } else {
      unit_rows <- rows
      n_desc <- NULL
      unit_keys <- NULL
    }

    if (is_null(strata_vars)) {
      add_pool(parent_ids[g], unit_rows, draw_spec, n_desc, unit_keys)
      next
    }

    skeys <- digest_path_keys(frame, unit_rows, strata_vars)
    sgroups <- split(
      seq_along(unit_rows), factor(skeys, levels = unique(skeys))
    )
    info <- stratum_info_from_groups(
      frame, strata_vars,
      lapply(sgroups, function(ix) unit_rows[ix])
    )
    info <- calculate_stratum_sizes(info, strata_spec, draw_spec)
    info_keys <- make_group_key(info, strata_vars)

    for (s in seq_along(sgroups)) {
      urows <- unit_rows[sgroups[[s]]]
      key_row <- frame[urows[1], strata_vars, drop = FALSE]
      skey <- make_group_key(key_row, strata_vars)
      pool_spec <- resolve_stratum_draw_spec(
        draw_spec,
        keys = key_row,
        strata_vars = strata_vars,
        stratum_key = skey,
        lookup = lookup
      )
      # The allocation-resolved n_h drives the chance; a per-stratum
      # frac (already resolved above) prevails for the fraction-driven
      # methods, exactly as at draw time.
      pool_spec$n <- as.double(info$.n_h[match(skey, info_keys)])
      add_pool(
        parent_ids[g], urows,
        pool_spec,
        if (is_null(n_desc)) NULL else n_desc[sgroups[[s]]],
        if (is_null(unit_keys)) NULL else unit_keys[sgroups[[s]]]
      )
    }
  }

  n_pools <- length(pools_acc)
  first_rows <- vapply(pools_acc, function(p) p$first_row, integer(1))
  chance_by_pool <- lapply(pools_acc, function(p) p$chance)

  pools <- data.frame(
    pool_id = seq_len(n_pools),
    parent_unit = vapply(
      pools_acc, function(p) as.integer(p$parent), integer(1)
    )
  )
  if (!is_null(strata_vars)) {
    pools <- cbind(pools, frame[first_rows, strata_vars, drop = FALSE])
    rownames(pools) <- NULL
  }
  pools$N <- vapply(pools_acc, function(p) as.integer(p$N), integer(1))
  pools$n_target <- vapply(
    pools_acc, function(p) as.double(p$n_target), numeric(1)
  )
  pools$n_expected <- vapply(chance_by_pool, sum, numeric(1))
  pools$n_realized <- 0L
  pools$scope <- "universe"
  pools$chance_status <- "design_resolved"

  chance_kind <- if (wr) "expected_hits" else "inclusion_probability"
  is_const <- vapply(chance_by_pool, function(ch) {
    length(ch) <= 1L || max(ch) - min(ch) < 1e-12
  }, logical(1))
  storage <- if (is_cluster) {
    "units"
  } else if (all(is_const)) {
    "constant"
  } else {
    "quantiles"
  }

  units <- NULL
  registry <- NULL
  chance_distribution <- NULL
  if (identical(storage, "constant")) {
    pools$chance <- vapply(chance_by_pool, function(ch) {
      if (length(ch) == 0) 0 else ch[1]
    }, numeric(1))
  } else if (identical(storage, "quantiles")) {
    chance_distribution <- digest_quantile_bins(
      chance_by_pool = chance_by_pool,
      pool_ids = pools$pool_id,
      pool_sizes = pools$N
    )
  } else {
    counts <- pools$N
    chance_all <- unlist(chance_by_pool, use.names = FALSE)
    units <- data.frame(
      unit_id = seq_len(sum(counts)),
      pool_id = rep.int(pools$pool_id, counts),
      unit_order = sequence(counts),
      chance = chance_all,
      is_certainty = if (
        identical(chance_kind, "inclusion_probability")
      ) {
        chance_all >= 1 - 1e-9
      } else {
        rep(NA, sum(counts))
      },
      n_descendants = as.integer(unlist(
        lapply(pools_acc, function(p) p$n_desc), use.names = FALSE
      ))
    )
    registry <- setNames(
      units$unit_id,
      unlist(lapply(pools_acc, function(p) p$keys), use.names = FALSE)
    )
  }

  stage <- new_digest_stage(
    stage_id = stage_idx,
    frame_ref = 1L,
    unit_level = if (is_cluster) "cluster" else "element",
    scope = "universe",
    chance_kind = chance_kind,
    probabilities = draw_spec$method_probabilities,
    order_kind = "input",
    storage = storage,
    pools = pools,
    units = units,
    chance_distribution = chance_distribution,
    selected = NULL,
    strata = strata_vars
  )
  pool_parent_key <- if (stage_idx == 1L) {
    rep("", n_pools)
  } else {
    digest_path_keys(frame, first_rows, ancestor_vars)
  }
  list(stage = stage, registry = registry, pool_keys = pool_parent_key)
}

#' Ex-ante mirror of the per-cluster invariance check of
#' sample_clusters(): the resolved chances read one value per cluster,
#' which is only valid when the frame carries one value per cluster.
#' @noRd
check_exante_cluster_invariants <- function(frame, draw_spec,
                                            strata_vars, ancestor_vars,
                                            cluster_vars) {
  invariant_vars <- cluster_invariant_vars(frame, draw_spec, strata_vars)
  if (length(invariant_vars) == 0) {
    return(invisible(NULL))
  }
  all_rows <- seq_len(nrow(frame))
  key_vars <- c(ancestor_vars, cluster_vars)
  n_clusters <- length(unique(digest_path_keys(frame, all_rows, key_vars)))
  varying <- invariant_vars[vapply(invariant_vars, function(v) {
    length(unique(digest_path_keys(frame, all_rows, c(key_vars, v)))) !=
      n_clusters
  }, logical(1))]
  if (length(varying) > 0) {
    abort_cluster_invariants(varying, cluster_vars)
  }
  invisible(NULL)
}

#' Merge per-replicate digests into one manifest
#'
#' Population structure is shared across replicates only where it is
#' actually the same object: stage-1 pools are deterministic given the
#' frame and design, but later-stage pools hang off the replicate's
#' realized parents and can differ between replicates. The merge keeps
#' the deepest stage prefix whose structure is identical across all
#' replicates that reached it, stacks per-replicate selected traces
#' with a replicate column, keeps n_realized when it is identical
#' across replicates, and marks the digest "partial" when
#' replicate-specific later stages had to be dropped.
#' @noRd
merge_replicated_digests <- function(digests) {
  if (
    length(digests) == 0 ||
      any(vapply(digests, is_null, logical(1)))
  ) {
    return(NULL)
  }
  n_stages <- vapply(digests, function(d) length(d$stages), integer(1))
  base <- digests[[which.max(n_stages)]]

  structural_cols <- function(pools) {
    pools[setdiff(names(pools), "n_realized")]
  }

  keep <- length(base$stages)
  for (k in seq_along(base$stages)) {
    if (k > keep) {
      break
    }
    sid <- base$stages[[k]]$stage_id
    sel_parts <- list()
    realized_list <- vector("list", length(digests))
    present_in_all <- TRUE
    shared <- TRUE
    for (r in seq_along(digests)) {
      ids_r <- vapply(
        digests[[r]]$stages, function(s) s$stage_id, integer(1)
      )
      pos <- match(sid, ids_r)
      if (is.na(pos)) {
        present_in_all <- FALSE
        next
      }
      st <- digests[[r]]$stages[[pos]]
      if (
        !identical(
          structural_cols(st$pools),
          structural_cols(base$stages[[k]]$pools)
        ) ||
          !identical(st$units, base$stages[[k]]$units)
      ) {
        shared <- FALSE
        break
      }
      realized_list[[r]] <- st$pools$n_realized
      sel <- st$selected
      if (!is_null(sel) && nrow(sel) > 0) {
        sel$replicate <- r
        sel_parts[[length(sel_parts) + 1L]] <- sel
      }
    }
    if (!shared) {
      keep <- k - 1L
      break
    }
    merged_sel <- NULL
    if (length(sel_parts) > 0) {
      merged_sel <- do.call(rbind, sel_parts)
      rownames(merged_sel) <- NULL
    }
    base$stages[[k]]$selected <- merged_sel

    seen <- realized_list[!vapply(realized_list, is_null, logical(1))]
    constant <- present_in_all &&
      length(seen) > 0 &&
      all(vapply(seen, identical, logical(1), y = seen[[1]]))
    if (!constant) {
      base$stages[[k]]$pools$n_realized <- NA_integer_
    }
  }

  if (keep == 0L) {
    return(NULL)
  }
  if (keep < length(base$stages)) {
    base$stages <- base$stages[seq_len(keep)]
    base$status <- "partial"
  }
  validate_frame_digest(base)
  base
}

#' Append continuation stages to a prior digest
#'
#' The prior manifest is immutable; the continuation appends new frame
#' records (content-deduplicated against the prior registry) and new
#' stage manifests. Pools of the first new stage link to the prior
#' last stage's units through the ancestry keys stored with its
#' selected trace.
#' @noRd
merge_continuation_digest <- function(prior, design, stage_ids,
                                      stage_traces, stage_frames,
                                      input_frames) {
  reached <- which(!vapply(stage_traces, is_null, logical(1)))
  stage_ids <- stage_ids[reached]
  stage_traces <- stage_traces[reached]
  stage_frames <- stage_frames[reached]
  input_frames <- input_frames[reached]
  if (length(stage_ids) == 0) {
    return(prior)
  }
  mode <- prior$privacy$mode

  new_reg <- build_digest_frames(design, input_frames)
  prior_hashes <- vapply(
    prior$frames,
    function(f) f$fingerprint_exact %||% NA_character_,
    character(1)
  )
  next_id <- max(vapply(
    prior$frames, function(f) f$frame_id, integer(1)
  ))
  kept <- list()
  id_map <- integer(length(new_reg$records))
  for (j in seq_along(new_reg$records)) {
    rec <- new_reg$records[[j]]
    existing <- match(rec$fingerprint_exact, prior_hashes)
    if (!is.na(existing)) {
      id_map[j] <- prior$frames[[existing]]$frame_id
    } else {
      next_id <- next_id + 1L
      rec$frame_id <- next_id
      # Continuation frames cover the realized parents, not the
      # universe.
      rec$scope <- "eligible"
      kept[[length(kept) + 1L]] <- rec
      id_map[j] <- next_id
    }
  }
  frame_refs <- id_map[new_reg$ref]

  last <- prior$stages[[length(prior$stages)]]
  registry <- NULL
  if (
    identical(last$storage, "units") &&
      !is_null(last$selected) &&
      "key" %in% names(last$selected)
  ) {
    registry <- setNames(
      as.integer(last$selected$unit_id),
      last$selected$key
    )
  }

  new_stages <- vector("list", length(stage_ids))
  for (i in seq_along(stage_ids)) {
    built <- build_digest_stage(
      design = design,
      stage_idx = stage_ids[i],
      pos = length(prior$stages) + i,
      trace = stage_traces[[i]],
      frame = stage_frames[[i]],
      frame_ref = frame_refs[i],
      parent_registry = registry,
      mode = mode
    )
    new_stages[[i]] <- built$stage
    registry <- built$registry
  }

  digest <- prior
  digest$frames <- c(prior$frames, kept)
  digest$stages <- c(prior$stages, new_stages)
  validate_frame_digest(digest)
  digest
}

#' Method diagnostics for balanced and spatial stages
#'
#' Balance: per pool and auxiliary term, the population total, its
#' Horvitz-Thompson estimate from the selected units, and the
#' residual. Bounds: per bound() level, the expected count, the
#' adjacent-integer bounds, and the realized count. Spatial:
#' coordinate variable names, ranges, and duplicate-coordinate count.
#' @noRd
build_stage_diagnostics <- function(draw_spec, frame, records,
                                    is_cluster, ancestor_vars,
                                    cluster_vars) {
  if (!is_balanced_method(draw_spec)) {
    return(NULL)
  }
  diagnostics <- list()

  aux_vars <- draw_spec$aux
  if (!is_null(aux_vars)) {
    # Cluster stages balance on cluster totals: reproduce the sums
    # sample_clusters() fed to the selection.
    lookup <- if (is_cluster) {
      key_vars <- c(ancestor_vars, cluster_vars)
      keys_all <- digest_path_keys(frame, seq_len(nrow(frame)), key_vars)
      function(v, rows) {
        sums <- tapply(frame[[v]], keys_all, sum)
        unname(sums[digest_path_keys(frame, rows, key_vars)])
      }
    } else {
      function(v, rows) frame[[v]][rows]
    }
    bal <- list()
    for (v in aux_vars) {
      for (i in seq_along(records)) {
        r <- records[[i]]
        x <- lookup(v, r$exec_rows)
        sel <- r$leaf$selected
        target <- sum(x)
        realized <- sum(x[sel] / r$leaf$chance[sel])
        bal[[length(bal) + 1L]] <- data.frame(
          pool_id = i,
          term = v,
          target = target,
          realized = realized,
          residual = realized - target
        )
      }
    }
    balance <- do.call(rbind, bal)
    rownames(balance) <- NULL
    diagnostics$balance <- balance
  }

  bound_vars <- draw_spec$bounds
  if (!is_null(bound_vars)) {
    bnd <- list()
    for (v in bound_vars) {
      for (i in seq_along(records)) {
        r <- records[[i]]
        values <- frame[[v]][r$exec_rows]
        chance <- r$leaf$chance
        sel <- r$leaf$selected
        for (level in unique(values)) {
          in_level <- values == level
          expected <- sum(chance[in_level])
          tol <- 1e-10 * (1 + abs(expected))
          lower <- floor(expected + tol)
          upper <- ceiling(expected - tol)
          realized <- sum(in_level[sel])
          bnd[[length(bnd) + 1L]] <- data.frame(
            pool_id = i,
            term = v,
            level = as.character(level),
            expected = expected,
            lower = lower,
            upper = upper,
            realized = realized,
            satisfied = realized >= lower && realized <= upper
          )
        }
      }
    }
    bounds <- do.call(rbind, bnd)
    rownames(bounds) <- NULL
    diagnostics$bounds <- bounds
  }

  spread_vars <- draw_spec$spread
  if (!is_null(spread_vars)) {
    all_rows <- unlist(
      lapply(records, function(r) r$rows), use.names = FALSE
    )
    coords <- frame[all_rows, spread_vars, drop = FALSE]
    ranges <- data.frame(
      variable = spread_vars,
      min = vapply(spread_vars, function(v) min(coords[[v]]), numeric(1)),
      max = vapply(spread_vars, function(v) max(coords[[v]]), numeric(1))
    )
    rownames(ranges) <- NULL
    diagnostics$spatial <- list(
      variables = spread_vars,
      dimensions = length(spread_vars),
      ranges = ranges,
      n_duplicate_coordinates = sum(duplicated(coords))
    )
  }

  if (length(diagnostics) == 0) NULL else diagnostics
}
