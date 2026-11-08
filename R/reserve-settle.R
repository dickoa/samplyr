## Settling a reserve list

#' The stage of a design that lists a reserve, or `NA`
#' @noRd
reserve_stage_of <- function(design) {
  has <- vapply(
    design$stages,
    function(stage) !is_null(stage$draw_spec$reserve),
    logical(1)
  )
  if (!any(has)) NA_integer_ else which(has)[1]
}

#' Refuse a call that draws below a reserve that is not settled
#'
#' A later stage drawn inside every listed unit would also draw inside
#' reserve units the field never released.
#' @noRd
check_reserve_order <- function(design, stages, sample = NULL,
                                call = caller_env()) {
  k <- reserve_stage_of(design)
  if (is.na(k) || !any(stages > k)) {
    return(invisible(NULL))
  }
  listed_now <- k %in% stages
  listed_before <- !is_null(sample) && k %in% get_stages_executed(sample) &&
    is_null(attr(sample, "metadata")$reserve_settlement)
  if (!listed_now && !listed_before) {
    return(invisible(NULL))
  }
  below <- stages[stages > k]
  abort_samplyr(
    c(
      "Stage {k} lists a reserve, so the stages below it wait until it is
       settled.",
      "x" = "This call would draw {cli::qty(length(below))}stage{?s}
             {below} inside every listed unit, including reserve units the
             field never released.",
      "i" = if (listed_now) {
        "Execute through stage {k} with {.code stages = {k}}, settle it
         with {.code execute(sample, response_status = col)}, then continue
         from the settled sample."
      } else {
        "Settle the reserve with
         {.code execute(sample, response_status = col)}, then continue from
         the settled sample."
      }
    ),
    class = "samplyr_error_reserve_unsettled",
    call = call
  )
}

#' Settle the reserve of an executed sample from its response status
#'
#' The route of `execute(sample, response_status = col)`. Like an occasion, it
#' takes no other execution input, since the list and its order are
#' already drawn.
#' @noRd
settle_reserve <- function(sample, response_status, frames, stages, seed,
                           panels, panel_stage, small_pool, reps, occasion,
                           frame_digest_given = FALSE, call = caller_env()) {
  supplied <- c(
    if (length(frames) > 0) "a frame",
    if (!is_null(stages)) "stages",
    if (!is_null(seed)) "seed",
    if (!is_null(panels)) "panels",
    if (!is_null(panel_stage)) "panel_stage",
    if (!is_null(small_pool)) "small_pool",
    if (!is_null(reps)) "reps",
    if (!is_null(occasion)) "period",
    if (isTRUE(frame_digest_given)) "frame_digest"
  )
  if (length(supplied) > 0L) {
    abort_samplyr(
      c(
        "{.arg response_status} settles a list that is already drawn, so it
         takes no further execution input.",
        "x" = "Also given: {supplied}."
      ),
      class = "samplyr_error_execute_argument",
      call = call
    )
  }
  if (!is_tbl_sample(sample)) {
    abort_samplyr(
      c(
        "{.arg response_status} settles the reserve of an executed sample.",
        "i" = "Execute the design first, then pass the sample with its
               response status column."
      ),
      class = "samplyr_error_execute_argument",
      call = call
    )
  }
  column <- draw_column_name(
    rlang::quo_get_expr(response_status), rlang::quo_get_env(response_status),
    "response_status"
  )
  if (!column %in% names(sample)) {
    abort_samplyr(
      "{.arg response_status} names {.field {column}}, which is not a
       column of the sample.",
      class = "samplyr_error_execute_argument",
      call = call
    )
  }
  check_single_replicate(sample, "execute", call = call)
  check_sample_unmodified(sample, "execute", call = call)

  design <- get_design(sample)
  meta <- attr(sample, "metadata")
  k <- reserve_stage_of(design)
  executed <- get_stages_executed(sample)
  if (is.na(k) || !k %in% executed || !is_null(meta[["occasion"]])) {
    abort_samplyr(
      c(
        "This sample holds no reserve to settle.",
        "i" = "A reserve is listed by a stage that sets {.arg reserve} in
               {.fn draw}."
      ),
      class = "samplyr_error_execute_argument",
      call = call
    )
  }
  if (!is_null(meta$reserve_settlement)) {
    abort_samplyr(
      c(
        "The reserve of stage {k} is already settled.",
        "i" = "A list is settled once. To settle it again, settle the sample
               the list was drawn in."
      ),
      class = "samplyr_error_reserve_already_settled",
      call = call
    )
  }
  if (any(executed > k)) {
    below <- executed[executed > k]
    abort_samplyr(
      c(
        "Stages below the reserve were drawn before it was settled.",
        "x" = "Stage {k} lists the reserve, and the sample also holds
               {cli::qty(length(below))}stage{?s} {below}."
      ),
      class = "samplyr_error_reserve_unsettled",
      call = call
    )
  }

  parsed <- reserve_outcome_category(sample[[column]])
  settle_reserve_codes(sample, design, k, parsed, column = column,
                       call = call)
}

#' Read outcome codes as AAPOR final disposition categories
#'
#' A code is read by its leading digit, so 2.24 and "2.24" are both an
#' eligible non-interview. `NA` is a unit the field did not release.
#' @return list(category, bad), `category` an integer 1 to 4 or `NA`.
#' @noRd
reserve_outcome_category <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  category <- rep(NA_integer_, length(x))
  bad <- rep(FALSE, length(x))
  if (is.logical(x)) {
    bad <- !is.na(x)
  } else if (is.numeric(x)) {
    seen <- !is.na(x)
    lead <- floor(x[seen])
    valid <- is.finite(x[seen]) & lead >= 1 & lead <= 4
    category[seen][valid] <- as.integer(lead[valid])
    bad[seen] <- !valid
  } else if (is.character(x)) {
    v <- tolower(trimws(x))
    seen <- !is.na(v)
    words <- c(interview = 1L, noninterview = 2L, unknown = 3L,
               ineligible = 4L)
    value <- unname(words[v[seen]])
    digit <- is.na(value) & grepl("^[1-4](\\.[0-9]+)?$", v[seen])
    value[digit] <- as.integer(substr(v[seen][digit], 1, 1))
    category[seen] <- value
    bad[seen] <- is.na(value)
  } else {
    bad <- rep(TRUE, length(x))
  }
  list(category = category, bad = bad)
}

#' Settle one pool from its outcomes in release order
#'
#' `main` holds the categories of the take, all released. `reserve` holds
#' the released reserve units in rank order. The stop rule is walked over the
#' final outcomes, so units released after the stop are dropped. The
#' eligible count drops the unit that stopped the list, which makes it
#' unbiased over the release order.
#' @noRd
settle_reserve_pool <- function(main, reserve, N, reserve_size) {
  b <- length(main)
  m <- length(reserve)
  r_main <- sum(main == 1L)
  if (reserve_size == 0L) {
    kept <- 0L
    s <- 0L
  } else if (r_main >= b) {
    kept <- 0L
    s <- 1L
  } else {
    hit <- which(r_main + cumsum(reserve == 1L) >= b)
    if (length(hit) > 0L) {
      kept <- hit[1]
      s <- 1L
    } else {
      kept <- m
      s <- 0L
    }
  }
  units <- c(main, reserve[seq_len(kept)])
  k <- length(units)
  e <- sum(units == 1L | units == 2L)
  u <- sum(units == 3L)
  b_r <- sum(units == 1L)
  list(
    n_listed = b + reserve_size,
    n_attempted = k,
    n_eligible = e,
    n_unknown = u,
    n_ineligible = sum(units == 4L),
    n_respondents = b_r,
    stopped = s,
    kept = kept,
    dropped = m - kept,
    ran_out = s == 0L && m == reserve_size && b_r < b,
    under_released = s == 0L && m < reserve_size,
    over_released = kept < m,
    eligible = if (b_r > 0L) N * (e - s) / (k - s - u) else NA_real_
  )
}

#' Each pool's estimated eligible count, with or without respondents
#'
#' A pool with respondents keeps the estimate of its walk. Without one,
#' nothing stopped the list, so s = 0 and the resolved units give the rate.
#' A pool whose attempted units are all of unknown eligibility takes the
#' resolved rate of the pools that share its strata, or counts every unit
#' as eligible when none of them resolved a unit.
#' @return list(eligible, rule), `rule` one of `"resolved"`, `"strata"` and
#'   `"all"`.
#' @noRd
reserve_pool_eligible <- function(pools, eligible, strata) {
  rule <- rep("resolved", nrow(pools))
  resolved <- pools$n_attempted - pools$stopped - pools$n_unknown
  missing <- pools$n_respondents == 0L
  direct <- missing & resolved > 0L
  eligible[direct] <- pools$N[direct] * pools$n_eligible[direct] /
    resolved[direct]
  unresolved <- which(missing & resolved == 0L)
  if (length(unresolved) == 0L) {
    return(list(eligible = eligible, rule = rule))
  }
  group <- if (ncol(strata) > 0L) {
    vctrs::vec_group_id(strata)
  } else {
    rep(1L, nrow(pools))
  }
  counted <- tapply(pools$n_eligible - pools$stopped, group, sum)
  base <- tapply(resolved, group, sum)
  for (p in unresolved) {
    g <- as.character(group[p])
    if (base[[g]] > 0) {
      eligible[p] <- pools$N[p] * counted[[g]] / base[[g]]
      rule[p] <- "strata"
    } else {
      eligible[p] <- pools$N[p]
      rule[p] <- "all"
    }
  }
  list(eligible = eligible, rule = rule)
}

#' Pools and units of the reserve stage, one entry per row
#'
#' A pool is a stratum of the stage inside its realized parent. A unit is a
#' cluster of the stage, or a row when the stage is not clustered.
#' @noRd
reserve_pool_index <- function(data, design, k) {
  # A parent drawn more than once lists a pool for each of its draws.
  draws <- paste0(".draw_", seq_len(k - 1L))
  pool_vars <- unique(c(
    collect_ancestor_strata_vars(design, k),
    collect_ancestor_cluster_vars(design, k),
    draws,
    design$stages[[k]]$strata$vars
  ))
  pool_vars <- intersect(pool_vars, names(data))
  unit_vars <- intersect(design$stages[[k]]$clusters$vars %||% character(0),
                         names(data))
  pool <- if (length(pool_vars) > 0L) {
    vctrs::vec_group_id(data[pool_vars])
  } else {
    rep(1L, nrow(data))
  }
  unit <- if (length(unit_vars) > 0L) {
    vctrs::vec_group_id(data[c(pool_vars, unit_vars)])
  } else {
    seq_len(nrow(data))
  }
  first <- match(seq_len(max(c(pool, 0L))), pool)
  labels <- if (length(pool_vars) > 0L) {
    keys <- data[first, pool_vars, drop = FALSE]
    do.call(paste, c(
      lapply(pool_vars, function(v) paste0(v, " = ", keys[[v]])),
      sep = ", "
    ))
  } else {
    rep("the whole frame", length(first))
  }
  list(
    pool = as.integer(pool),
    unit = as.integer(unit),
    vars = pool_vars,
    keys = data[first, pool_vars, drop = FALSE],
    labels = labels
  )
}

#' Settle a listed sample from outcome categories, one per row
#'
#' Shared by `execute(sample, response_status = col)` and by replay, which
#' reads the categories back from the receipt.
#' @noRd
settle_reserve_codes <- function(sample, design, k, parsed, column = NULL,
                                 call = caller_env()) {
  data <- as.data.frame(sample)
  category <- parsed$category
  index <- reserve_pool_index(data, design, k)
  rank <- data[[paste0(".reserve_", k)]]
  weight_k <- data[[paste0(".weight_", k)]]

  problems <- list()
  flag <- function(pools, reason) {
    if (length(pools) > 0L) {
      problems[[reason]] <<- unique(c(problems[[reason]], pools))
    }
  }
  flag(index$pool[parsed$bad], "codes")

  # Units are numbered in order of appearance. A unit mixes codes when it
  # holds more than one distinct pair of unit and code.
  n_units <- max(c(index$unit, 0L))
  unit_row <- match(seq_len(n_units), index$unit)
  pairs <- vctrs::vec_unique(data.frame(unit = index$unit, code = category))
  mixed <- tabulate(pairs$unit, n_units) > 1L
  flag(index$pool[unit_row[mixed]], "mixed")

  unit_pool <- index$pool[unit_row]
  unit_rank <- rank[unit_row]
  unit_cat <- category[unit_row]
  unit_weight <- weight_k[unit_row]
  units_by_pool <- split(
    seq_len(n_units),
    factor(unit_pool, levels = seq_along(index$labels))
  )
  results <- vector("list", length(index$labels))
  for (p in seq_along(index$labels)) {
    in_pool <- units_by_pool[[p]]
    is_main <- unit_rank[in_pool] == 0L
    main <- unit_cat[in_pool][is_main]
    if (anyNA(main)) {
      flag(p, "main")
      next
    }
    ranks <- unit_rank[in_pool][!is_main]
    cats <- unit_cat[in_pool][!is_main]
    released <- sort(ranks[!is.na(cats)])
    if (!identical(as.integer(released), seq_along(released))) {
      flag(p, "gap")
      next
    }
    L <- length(in_pool)
    N <- round(unit_weight[in_pool][1] * L)
    reserve <- cats[!is.na(cats)][order(ranks[!is.na(cats)])]
    results[[p]] <- c(
      list(pool = p, N = N),
      settle_reserve_pool(main, reserve, N, sum(!is_main))
    )
  }
  if (length(problems) > 0L) {
    abort_reserve_protocol(problems, index$labels, call = call)
  }

  # Units kept by the walk: the take, and reserve ranks up to the stop.
  kept_rank <- vapply(results, function(r) r$kept, integer(1))
  unit_kept <- unit_rank == 0L | unit_rank <= kept_rank[unit_pool]
  unit_respondent <- unit_kept & !is.na(unit_cat) & unit_cat == 1L
  factor <- vapply(results, function(r) {
    r$eligible / r$n_respondents
  }, numeric(1))
  keep <- unit_respondent[index$unit]

  out <- data[keep, , drop = FALSE]
  wk <- paste0(".weight_", k)
  new_weight <- factor[index$pool[keep]]
  out$.weight <- out$.weight / out[[wk]] * new_weight
  out[[wk]] <- new_weight
  rownames(out) <- NULL

  codes <- category[order(data$.sample_id)]
  codes[is.na(codes)] <- 0L
  # Keys apart from the statistics, so that no frame column can collide.
  keys <- tibble::as_tibble(index$keys)
  pools <- tibble::tibble(.rows = length(results))
  pool_stat <- function(name, type) {
    vapply(results, function(r) r[[name]], type)
  }
  pools$N <- pool_stat("N", numeric(1))
  for (name in c("n_listed", "n_attempted", "n_eligible", "n_unknown",
                 "n_ineligible", "n_respondents", "stopped", "dropped")) {
    pools[[name]] <- as.integer(pool_stat(name, numeric(1)))
  }
  for (name in c("ran_out", "under_released", "over_released")) {
    pools[[name]] <- pool_stat(name, logical(1))
  }
  strata_vars <- intersect(
    c(collect_ancestor_strata_vars(design, k),
      design$stages[[k]]$strata$vars),
    names(keys)
  )
  estimate <- reserve_pool_eligible(
    pools, pool_stat("eligible", numeric(1)), keys[strata_vars]
  )
  pools$eligible <- estimate$eligible
  pools$eligible_rule <- estimate$rule
  # A pool estimated to hold no eligible unit has no weight to place.
  pools$no_respondent <- pools$n_respondents == 0L & pools$eligible > 0
  # The stages above the reserve, which a unit with no respondent row keeps.
  first_row <- match(seq_along(results), index$pool)
  pools$upper_weight <- data$.weight[first_row] / weight_k[first_row]
  # Whether the pool's primary unit was taken with certainty.
  sure <- if (k > 1L) data$.certainty_1[first_row]
  pools$certainty <- if (is_null(sure)) rep(FALSE, nrow(pools)) else sure %in% TRUE

  meta <- attr(sample, "metadata")
  executed <- get_stages_executed(sample)
  meta$modified <- NULL
  meta$n_selected <- nrow(out)
  meta$integrity <- sample_integrity_record(out, design, executed)
  meta$reserve_settlement <- list(
    stage = as.integer(k),
    codes = as.integer(codes),
    keys = keys,
    pools = pools,
    column = column
  )
  new_tbl_sample(
    out,
    design = design,
    stages_executed = executed,
    seed = attr(sample, "seed"),
    metadata = meta
  )
}

#' Refuse outcomes that break the release protocol, naming the pools
#' @noRd
abort_reserve_protocol <- function(problems, labels, call = caller_env()) {
  reasons <- c(
    codes = "Some values are neither a code from 1 to 4, one of
             \"interview\", \"noninterview\", \"unknown\" or \"ineligible\",
             nor NA.",
    mixed = "The rows of one listed unit carry different codes.",
    main = "A unit of the take has no outcome, but every unit of the take is
            released.",
    gap = "The released reserve units are not ranks 1 to m in release order."
  )
  # Pool labels hold frame values, which cli must not interpolate.
  escape <- function(x) gsub("}", "}}", gsub("{", "{{", x, fixed = TRUE),
                             fixed = TRUE)
  bullets <- character(0)
  for (reason in names(problems)) {
    pools <- labels[problems[[reason]]]
    shown <- escape(utils::head(pools, 5L))
    more <- length(pools) - length(shown)
    bullets <- c(
      bullets,
      "x" = reasons[[reason]],
      stats::setNames(shown, rep(" ", length(shown))),
      if (more > 0L) {
        c(" " = paste0("and ", more, " more pool", if (more > 1L) "s"))
      }
    )
  }
  abort_samplyr(
    c(
      "The response status codes do not follow the release protocol of the
       reserve.",
      bullets,
      "i" = "The office releases reserve units in {.field .reserve_k} order,
             one per failed unit, and every unit of the take is worked."
    ),
    class = "samplyr_error_reserve_protocol",
    call = call
  )
}

#' Apply a recorded settlement to a replayed list
#' @noRd
replay_reserve_settlement <- function(sample, record, call = caller_env()) {
  if (is_null(record)) {
    return(sample)
  }
  k <- as.integer(unlist(record$stage))
  codes <- as.integer(unlist(record$codes))
  ids <- sample$.sample_id
  if (length(codes) != nrow(sample) || is_null(ids) || anyNA(codes) ||
      !all(codes %in% 0:4) || !identical(k, reserve_stage_of(get_design(sample)))) {
    abort_samplyr(
      c(
        "The recorded settlement does not fit the replayed list.",
        "x" = "It records {length(codes)} outcome{?s} at stage {k} for a list
               of {nrow(sample)} row{?s}, with categories 0 to 4."
      ),
      class = "samplyr_error_replay_settlement",
      call = call
    )
  }
  category <- rep(NA_integer_, nrow(sample))
  category[order(ids)] <- codes
  category[category == 0L] <- NA_integer_
  parsed <- list(category = category, bad = rep(FALSE, nrow(sample)))
  settle_reserve_codes(sample, get_design(sample), k, parsed, call = call)
}
