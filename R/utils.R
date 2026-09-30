#' @noRd
equal_prob_methods <- c("srswor", "srswr", "systematic", "bernoulli")
wr_methods <- c("srswr", "pps_multinomial")
pmr_methods <- c("pps_chromy")
multi_hit_methods <- c(wr_methods, pmr_methods)
pps_wor_methods <- c(
  "pps_brewer",
  "pps_systematic",
  "pps_cps",
  "pps_sampford",
  "pps_poisson",
  "pps_sps",
  "pps_pareto"
)
pps_wr_methods <- c("pps_multinomial", "pps_chromy")
pps_methods <- c(pps_wor_methods, pps_wr_methods)
prn_methods <- c("bernoulli", "pps_poisson", "pps_sps", "pps_pareto")
spatial_balanced_methods <- c("lpm2", "scps")
balanced_methods <- c("cube", spatial_balanced_methods)
builtin_methods <- c(equal_prob_methods, pps_methods, balanced_methods)
builtin_method_aliases <- c(balanced = "cube")
valid_builtin_methods <- c(builtin_methods, names(builtin_method_aliases))
jip_methods <- c(pps_methods, balanced_methods)

# Random-size Poisson methods require the Horvitz-Thompson Poisson variance.
rs_poisson_methods <- c("bernoulli", "pps_poisson")

# Report PPS Poisson pools below 95 percent of their reachable target.
poisson_shortfall_tolerance <- 0.95

#' Return the public family prefix for a registered method
#' @noRd
custom_method_prefix <- function(method) {
  if (grepl("^pps_.+", method)) return("pps")
  if (grepl("^balanced_.+", method)) return("balanced")
  NULL
}

#' Normalize compatibility names to their canonical public form
#' @noRd
canonical_method_name <- function(method, method_type = NULL) {
  if (identical(method, "balanced")) return("cube")
  if (
    identical(method_type, "balanced") &&
      grepl("^pps_.+", method)
  ) {
    return(sub("^pps_", "balanced_", method))
  }
  method
}

#' Map a samplyr method name to its sondage registry name
#' @noRd
sondage_method_name <- function(method) {
  if (method %in% c("balanced", "cube")) return("cube")
  sub("^(pps|balanced)_", "", method)
}

#' Check if a method is a custom registered method in sondage
#' @noRd
is_custom_method <- function(method) {
  if (is_null(custom_method_prefix(method))) return(FALSE)
  sondage_name <- sondage_method_name(method)
  sondage::is_registered_method(sondage_name)
}

#' Query metadata for a custom registered method
#' @return A list with type, fixed_size, supports_prn, or NULL.
#' @noRd
custom_method_spec <- function(method) {
  sondage::method_spec(sondage_method_name(method))
}

#' Probabilities tier of a built-in method, NULL for non-built-ins
#' @noRd
builtin_method_probabilities <- function(method) {
  if (!method %in% builtin_methods) return(NULL)
  sampling_method_dictionary()[[method]]$probability_quality
}

#' Fingerprint of a registered method's implementation
#'
#' Hashes the deparsed formals and body of `sample_fn` and `joint_fn`. The
#' enclosing environment is not covered. NULL for built-ins.
#' @noRd
method_implementation_hash <- function(spec) {
  if (is_null(spec$sample_fn)) return(NULL)
  fingerprint_fn <- function(f) {
    if (is_null(f)) return(NULL)
    list(deparse(formals(f)), deparse(body(f)))
  }
  rlang::hash(list(
    sample_fn = fingerprint_fn(spec$sample_fn),
    joint_fn = fingerprint_fn(spec$joint_fn)
  ))
}

#' Refuse a method whose selection probabilities are unknown
#' @noRd
abort_unknown_probabilities <- function(method,
                                        call = rlang::caller_env()) {
  abort_samplyr(
    c(
      "Method {.val {method}} declares its selection probabilities
       unknown, so design weights cannot be computed.",
      "i" = "samplyr weights samples by the inverse of their inclusion
             probability or expected hit count. A method whose true
             selection expectations are unknown cannot produce them.",
      "i" = "Declare {.code probabilities = \"exact\"} or
             {.code \"approximate\"} at registration if the method
             honors the {.arg pik} it receives, or draw with sondage
             directly for an unweighted selection."
    ),
    class = "samplyr_error_unknown_probabilities",
    call = call
  )
}

#' Check if method is WOR (built-in or registered)
#' @noRd
is_wor_method <- function(draw_spec) {
  method <- draw_spec$method
  if (!is_null(draw_spec$method_type)) {
    return(draw_spec$method_type %in% c("wor", "balanced"))
  }
  !(method %in% c(wr_methods, pmr_methods))
}

#' Check if method is a multi-hit method (built-in or registered)
#' @noRd
is_multi_hit_method <- function(draw_spec) {
  method <- draw_spec$method
  if (!is_null(draw_spec$method_type)) return(draw_spec$method_type == "wr")
  method %in% multi_hit_methods
}

#' Check if a method draws a random number of units
#' @noRd
is_random_size_method <- function(draw_spec) {
  draw_spec$method %in% rs_poisson_methods ||
    identical(draw_spec$method_fixed, FALSE)
}

#' Check if a method belongs to the balanced family
#' @noRd
is_balanced_method <- function(draw_spec) {
  draw_spec$method %in% balanced_methods ||
    identical(draw_spec$method_type, "balanced")
}

#' @noRd
is_finite_numeric <- function(x) {
  is.numeric(x) &&
    length(x) > 0 &&
    !anyNA(x) &&
    all(is.finite(x))
}

#' @noRd
is_integerish_numeric <- function(x, tol = sqrt(.Machine$double.eps)) {
  is_finite_numeric(x) &&
    all(abs(x - round(x)) <= tol)
}

#' Resolved certainty: an inclusion probability numerically equal to one
#'
#' Takes inclusion probabilities, never expected hits.
#' @noRd
is_certainty_probability <- function(p, tol = 100 * .Machine$double.eps) {
  is.finite(p) & p >= 1 - tol
}

#' Evaluate `expr`, giving an error it raises a samplyr class
#'
#' For `rlang::arg_match()`, whose refusal has no class of its own.
#' @noRd
with_error_class <- function(expr, class) {
  tryCatch(expr, error = function(e) {
    class(e) <- unique(c(class, "samplyr_error", class(e)))
    stop(e)
  })
}

#' Report a per-pool selection diagnostic, aggregate it later
#'
#' Leaves signal per pool. `execute()` emits one aggregated condition per stage.
#' @noRd
signal_selection_event <- function(operation, ..., stage = NA_integer_) {
  rlang::signal(
    message = "",
    class = "samplyr_condition_selection_event",
    operation = operation,
    stage = stage,
    payload = list(...)
  )
}

#' Report a fixed-size target the pool population could not supply
#'
#' `n_available` covers every pool the stage executed, not only capped ones.
#' @noRd
signal_population_cap <- function(
  pool_keys,
  n_capped,
  n_pools,
  n_requested,
  n_actual,
  n_available
) {
  signal_selection_event(
    "population_cap",
    pool_keys = pool_keys,
    n_capped = n_capped,
    n_pools = n_pools,
    n_requested = n_requested,
    n_actual = n_actual,
    n_available = n_available
  )
}

#' @noRd
signal_nominal_cap <- function(
  pool_keys,
  n_capped,
  n_pools,
  n_requested,
  n_available
) {
  signal_selection_event(
    "nominal_cap",
    pool_keys = pool_keys,
    n_capped = n_capped,
    n_pools = n_pools,
    n_requested = n_requested,
    n_available = n_available
  )
}

#' Tag selection events with the replicate that produced them
#' @noRd
tag_replicate_events <- function(expr, replicate) {
  events <- list()
  result <- withCallingHandlers(
    expr,
    samplyr_condition_selection_event = function(cnd) {
      events[[length(events) + 1L]] <<- cnd
      rlang::cnd_muffle(cnd)
    }
  )
  for (cnd in events) {
    rlang::signal(
      message = "",
      class = "samplyr_condition_selection_event",
      operation = cnd$operation,
      stage = cnd$stage,
      replicate = replicate,
      payload = cnd$payload
    )
  }
  result
}

#' Attach a stage to selection events raised by its leaves
#'
#' A population cap also carries the stage totals, which replace leaf sums.
#' @noRd
collect_stage_events <- function(expr, stage) {
  events <- list()
  result <- withCallingHandlers(
    expr,
    samplyr_condition_selection_event = function(cnd) {
      events[[length(events) + 1L]] <<- cnd
      rlang::cnd_muffle(cnd)
    }
  )
  for (cnd in events) {
    payload <- cnd$payload
    if (identical(cnd$operation, "population_cap")) {
      payload$stage_totals <- result$stage_totals
    }
    rlang::signal(
      message = "",
      class = "samplyr_condition_selection_event",
      operation = cnd$operation,
      stage = stage,
      payload = payload
    )
  }
  result
}

#' Report a PPS Poisson pool that cannot reach the size it was asked for
#'
#' Measured against the reachable take, not the nominal one. `pik` includes
#' certainty units and `n_clipped` does not.
#' @noRd
check_poisson_shortfall <- function(
  pik,
  n_requested,
  n_reachable,
  n_clipped,
  pool_keys = character(0)
) {
  # Direct `pik` may omit a nominal count.
  if (is_null(n_requested) || is.na(n_reachable) || n_reachable <= 0) {
    return(invisible(NULL))
  }
  n_expected <- sum(pik)
  if (n_expected >= n_reachable * poisson_shortfall_tolerance) {
    return(invisible(NULL))
  }
  signal_selection_event(
    "poisson_shortfall",
    pool_keys = pool_keys,
    n_pools = 1L,
    n_requested = as.double(n_requested),
    n_reachable = as.double(n_reachable),
    n_expected = as.double(n_expected),
    n_clipped = as.integer(n_clipped)
  )
  invisible(NULL)
}

#' Name the parent pool an event came from
#'
#' Qualifies otherwise identical leaf-pool labels by their realized parent.
#' @noRd
qualify_pool_events <- function(expr, parent_key) {
  events <- list()
  result <- withCallingHandlers(
    expr,
    samplyr_condition_selection_event = function(cnd) {
      events[[length(events) + 1L]] <<- cnd
      rlang::cnd_muffle(cnd)
    }
  )
  for (cnd in events) {
    payload <- cnd$payload
    keys <- payload$pool_keys
    # Parent identity defines stages without their own pools.
    payload$pool_keys <- if (length(keys) == 0L) {
      parent_key
    } else {
      ifelse(nzchar(keys), paste(parent_key, keys, sep = " > "), parent_key)
    }
    rlang::signal(
      message = "",
      class = "samplyr_condition_selection_event",
      operation = cnd$operation,
      stage = cnd$stage,
      replicate = cnd$replicate,
      payload = payload
    )
  }
  result
}

#' Collect every selection event of an execution and report each once
#' @noRd
report_selection_events <- function(expr) {
  events <- list()
  result <- withCallingHandlers(
    expr,
    samplyr_condition_selection_event = function(cnd) {
      events[[length(events) + 1L]] <<- cnd
      rlang::cnd_muffle(cnd)
    }
  )

  if (length(events) == 0L) {
    return(result)
  }

  stage_of <- vapply(
    events,
    function(cnd) {
      paste(cnd$operation, cnd$stage %||% NA_integer_, sep = "|")
    },
    character(1)
  )
  # Use a token because missing replicate IDs do not compare equal.
  replicate_of <- vapply(
    events,
    function(cnd) {
      if (is_null(cnd$replicate)) "none" else as.character(cnd$replicate)
    },
    character(1)
  )

  for (key in unique(stage_of)) {
    in_stage <- stage_of == key
    group <- events[in_stage]
    first <- group[[1]]

    per_replicate <- lapply(
      unique(replicate_of[in_stage]),
      function(rep_id) {
        summarize_selection_events(group[replicate_of[in_stage] == rep_id])
      }
    )

    # Classify before merging replicate-specific exhaustion.
    outcome_of <- vapply(
      per_replicate,
      function(x) selection_event_outcome(first$operation, x),
      character(1)
    )

    for (outcome in unique(outcome_of)) {
      report_selection_outcome(
        outcome,
        first$stage,
        merge_replicate_aggregates(per_replicate[outcome_of == outcome])
      )
    }
  }

  result
}

#' The public reading of one replicate's aggregate
#' @noRd
selection_event_outcome <- function(operation, x) {
  if (identical(operation, "population_cap") && is_stage_census(x)) {
    return("census")
  }
  if (identical(operation, "population_cap")) {
    return("size_capped")
  }
  operation
}

#' @noRd
report_selection_outcome <- function(outcome, stage, x) {
  switch(
    outcome,
    census = warn_census(stage, x),
    size_capped = warn_size_capped(stage, x),
    nominal_cap = warn_nominal_capped(stage, x),
    poisson_shortfall = warn_poisson_shortfall(stage, x),
    singleton_pool = inform_singleton_strata(stage, x),
    empty_parent = warn_empty_parents(stage, x),
    allocation_cap = inform_allocation_capped(stage, x),
    cli_abort(
      "Internal error: unhandled selection outcome {.val {outcome}}.",
      call = NULL,
      class = "samplyr_error_internal"
    )
  )
}

#' @noRd
summarize_selection_events <- function(group) {
  field <- function(name) {
    lapply(group, function(cnd) cnd$payload[[name]])
  }
  # Do not turn an absent realized count into zero.
  total <- function(name) {
    values <- unlist(field(name))
    if (length(values) == 0L) {
      return(NA_real_)
    }
    sum(values)
  }

  keys <- unique(unlist(field("pool_keys")))
  out <- list(
    pool_keys = keys,
    n_capped = total("n_capped"),
    n_pools = total("n_pools"),
    n_requested = total("n_requested"),
    n_actual = total("n_actual"),
    n_available = total("n_available"),
    n_reachable = total("n_reachable"),
    n_expected = total("n_expected"),
    n_clipped = total("n_clipped"),
    n_moved = total("n_moved"),
    n_singleton = total("n_singleton"),
    n_empty = total("n_empty")
  )

  # Only capped pools signal, so add their shortfall to the selection.
  stage_totals <- Filter(Negate(is_null), field("stage_totals"))
  if (length(stage_totals) > 0L) {
    whole <- stage_totals[[1]]
    shortfall <- out$n_requested - out$n_actual
    out$n_pools <- whole$n_pools
    out$n_available <- whole$n_available
    out$n_actual <- whole$n_actual
    out$n_requested <- whole$n_actual + shortfall
  }
  out
}

#' Collapse one stage's per-replicate aggregates into the single report
#'
#' Counts come from the first replicate and pool keys are unioned. `varied`
#' flags that the two came apart.
#' @noRd
merge_replicate_aggregates <- function(per_replicate) {
  out <- per_replicate[[1]]
  out$n_replicates <- length(per_replicate)
  if (length(per_replicate) == 1L) {
    out$varied <- FALSE
    return(out)
  }

  keys <- unique(unlist(lapply(per_replicate, function(x) x$pool_keys)))
  compared <- lapply(per_replicate, function(x) x[names(x) != "pool_keys"])
  out$varied <- !all(vapply(
    compared[-1],
    function(x) identical(x, compared[[1]]),
    logical(1)
  )) ||
    !all(vapply(
      per_replicate[-1],
      function(x) setequal(x$pool_keys, per_replicate[[1]]$pool_keys),
      logical(1)
    ))
  out$pool_keys <- keys
  out
}

#' Did this stage select every unit it could reach?
#'
#' Stage-local. Compares totals, not counts of saturated pools.
#' @noRd
is_stage_census <- function(x) {
  isTRUE(!is.na(x$n_actual) && !is.na(x$n_available) &&
    x$n_actual >= x$n_available)
}

#' Name a few pools and count the rest
#' @noRd
format_pool_sample <- function(keys, max_shown = 5L) {
  if (length(keys) == 0L) {
    return(NULL)
  }
  if (length(keys) <= max_shown) {
    return(cli::format_inline("{.val {keys}}"))
  }
  # Join truncated keys manually to retain the omitted count.
  shown <- cli::cli_vec(
    keys[seq_len(max_shown)],
    style = list("vec-last" = ", ")
  )
  hidden <- length(keys) - max_shown
  cli::format_inline("{.val {shown}}, and {hidden} more")
}

#' Render the pool list, and say so when replicates disagreed
#' @noRd
pool_lines <- function(x, label) {
  pools <- format_pool_sample(x$pool_keys)
  if (is_null(pools)) {
    return(NULL)
  }
  if (isTRUE(x$varied)) {
    return(c(
      "i" = cli::format_inline("{label} across replicates: {pools}."),
      "i" = cli::format_inline(
        "Counts describe one of {x$n_replicates} replicate{?s} reporting
         this; the pools reached varied."
      )
    ))
  }
  c("i" = cli::format_inline("{label}: {pools}."))
}

#' @noRd
warn_size_capped <- function(stage, x) {
  cli_warn(
    c(
      "Stage {stage}: sample size exceeded the pool population in
       {x$n_capped} of {x$n_pools} pool{?s}.",
      "x" = "Requested {x$n_requested} unit{?s}, selected {x$n_actual}.",
      pool_lines(x, "Capped pools"),
      "i" = "Inspect with {.code frame_summary(sample, detail = \"pool\")} and
             the {.field capped} column."
    ),
    class = "samplyr_warning_size_capped",
    stage = stage,
    operation = "population_cap",
    payload = x
  )
}

#' Report a stage that took every unit within reach
#' @noRd
warn_census <- function(stage, x) {
  cli_warn(
    c(
      "Stage {stage}: selected every unit available in the pools it
       executed.",
      "x" = "Requested {x$n_requested} unit{?s}, selected all
             {x$n_actual} available.",
      pool_lines(x, "Exhausted pools"),
      "i" = "This stage contributes no sampling variance. Earlier stages are
             unaffected: the design as a whole is a census only if every
             stage is."
    ),
    class = "samplyr_warning_census",
    stage = stage,
    operation = "population_cap",
    payload = x
  )
}

#' Report a nominal target above the pool population on a random-size stage
#'
#' Names no selected count, because the realized size is a draw that usually
#' lands below the capped target.
#' @noRd
warn_nominal_capped <- function(stage, x) {
  cli_warn(
    c(
      "Stage {stage}: target sample size exceeded the pool population in
       {x$n_capped} of {x$n_pools} pool{?s}.",
      "x" = "Requested {x$n_requested} unit{?s}, nominal target capped at
             {x$n_available}.",
      pool_lines(x, "Capped pools"),
      "i" = "This is a random-size design: the realized size can still fall
             below the capped target."
    ),
    class = "samplyr_warning_nominal_cap",
    stage = stage,
    operation = "nominal_cap",
    payload = x
  )
}

#' Report PPS Poisson pools that saturated below their reachable target
#'
#' Aggregates only affected pools, so a large pool cannot hide a collapsed one.
#' @noRd
warn_poisson_shortfall <- function(stage, x) {
  cli_warn(
    c(
      "Stage {stage}: PPS Poisson expected sample size fell short of the
       reachable target in {x$n_pools} pool{?s}.",
      "x" = "Reachable {round(x$n_reachable, 1)} unit{?s}, expected
             {round(x$n_expected, 1)}.",
      "x" = "{x$n_clipped} unit{?s} ha{?s/ve} an inclusion probability
             clipped at 1.",
      pool_lines(x, "Affected pools"),
      "i" = "Handle dominant units explicitly with {.arg certainty_size} or
             {.arg certainty_prop}.",
      "i" = "See {.topic selection-methods} for the {.val pps_poisson}
             contract."
    ),
    class = "samplyr_warning_poisson_shortfall",
    stage = stage,
    operation = "poisson_shortfall",
    payload = x
  )
}

#' Report strata whose draw outside certainty is a single unit
#'
#' A message, since one unit per stratum can be the design. Counted per
#' parent, which is the number of lonely strata survey will find.
#' @noRd
inform_singleton_strata <- function(stage, x) {
  cli_inform(
    c(
      "Stage {stage}: {x$n_singleton} strat{?um/a} {?takes/take} a single
       unit outside certainty.",
      pool_lines(x, "Strata with one unit"),
      "i" = "A stratum needs two selections for its variance to be
             estimated. survey stops on a stratum with one unless its
             strata are collapsed, and a replicate method has nothing to
             resample in it.",
      "i" = "To estimate within each stratum, allocate at least two units
             outside certainty to it. {.arg min_n} bounds a stratum's whole
             take, certainty units included."
    ),
    class = "samplyr_message_singleton_pool",
    stage = stage,
    operation = "singleton_pool",
    payload = x
  )
}

#' Report selected units a later stage found empty
#'
#' Raised only under `on_empty = "warn"`.
#' @noRd
warn_empty_parents <- function(stage, x) {
  cli_warn(
    c(
      "Stage {stage}: {x$n_empty} selected unit{?s} of the previous stage
       {?has/have} no rows to sample from.",
      pool_lines(x, "Empty units"),
      "i" = "Each contributes zero to every total, which keeps the estimates
             unbiased. Set {.code on_empty = \"silent\"} in this stage's
             {.fn draw} once that is expected."
    ),
    class = "samplyr_warning_empty_parent",
    stage = stage,
    operation = "empty_parent",
    payload = x
  )
}

#' @noRd
inform_allocation_capped <- function(stage, x) {
  cli_inform(
    c(
      "Allocation capped at the stratum population in {x$n_capped} of
       {x$n_pools} strata.",
      pool_lines(x, "Capped strata"),
      "i" = "{x$n_moved} unit{?s} redistributed across the remaining strata."
    ),
    class = "samplyr_message_allocation_capped",
    stage = stage,
    operation = "allocation_cap",
    payload = x
  )
}

#' @noRd
abort_samplyr <- function(
  message,
  class = NULL,
  call = rlang::caller_env(),
  envir = parent.frame(),
  ...
) {
  cli_abort(
    message,
    ...,
    class = c(class, "samplyr_error"),
    call = call,
    .envir = envir
  )
}

#' Find the reserved argument a stray name in `...` was most likely meant to be
#' @noRd
suggest_reserved_arg <- function(name, candidates, max_dist = 2L,
                                 prefix = FALSE) {
  if (!is_character(name) || length(name) != 1L || !nzchar(name)) {
    return(NULL)
  }
  if (length(candidates) == 0L) {
    return(NULL)
  }
  distances <- as.integer(utils::adist(name, candidates, ignore.case = TRUE))
  closest <- which.min(distances)
  if (distances[closest] <= max_dist) {
    return(candidates[[closest]])
  }
  if (!prefix) {
    return(NULL)
  }
  # Suggest expansions only after an argument is known to be invalid.
  starts <- vapply(
    candidates,
    function(cand) startsWith(tolower(name), tolower(cand)),
    logical(1)
  )
  if (!any(starts)) {
    return(NULL)
  }
  matches <- candidates[starts]
  matches[[which.max(nchar(matches))]]
}

#' Message bullets naming a stray argument and its likely intended spelling
#'
#' Formatted here, not returned as cli templates, because the caller raises
#' them from a frame where these locals no longer exist.
#' @noRd
stray_arg_bullets <- function(name, candidates) {
  suggestion <- suggest_reserved_arg(name, candidates)
  advice <- if (!is.null(suggestion)) {
    cli::format_inline("Did you mean {.arg {suggestion}}?")
  } else {
    cli::format_inline(
      "Named arguments here must be one of {.arg {candidates}}."
    )
  }
  c(
    "x" = cli::format_inline(
      "{.arg {name}} is not an argument of this function."
    ),
    "i" = advice
  )
}

#' Refuse anything that lands in a `...` reserved for nothing
#'
#' @param dots The caller's `...` as quosures from `enquos()`. Forcing them
#'   would let a stray expression fail first with its own message.
#' @param candidates The arguments that follow `...`, for suggestions.
#' @noRd
check_keyword_args <- function(dots, candidates, call = rlang::caller_env()) {
  if (length(dots) == 0L) {
    return(invisible(NULL))
  }
  nms <- names(dots) %||% rep("", length(dots))

  named <- which(nzchar(nms))
  if (length(named) > 0) {
    abort_samplyr(
      c(
        "This function received an unexpected argument.",
        stray_arg_bullets(nms[[named[[1]]]], candidates)
      ),
      class = "samplyr_error_unknown_argument",
      call = call
    )
  }

  abort_samplyr(
    c(
      "{length(dots)} argument{?s} {?was/were} passed positionally where only
       named arguments are accepted.",
      "i" = "{.arg {candidates}} follow{?s/} {.code ...}, so each must be
             given by name."
    ),
    class = "samplyr_error_unnamed_argument",
    call = call
  )
}

#' Refuse names that belong to nobody in a `...` that is forwarded onward
#'
#' Checks against explicit accepted names, so no downstream argument is forced
#' just to see whether it was used.
#' @noRd
check_forwarded_args <- function(
  dots,
  owned,
  accepted,
  derived = character(0),
  forwarded_to,
  call = rlang::caller_env()
) {
  if (length(dots) == 0L) {
    return(invisible(NULL))
  }
  nms <- names(dots) %||% rep("", length(dots))

  unnamed <- sum(!nzchar(nms))
  if (unnamed > 0) {
    abort_samplyr(
      c(
        "{unnamed} argument{?s} {?was/were} passed positionally into
         {.code ...}.",
        "i" = "{.code ...} is forwarded to {.fn {forwarded_to}}, where a
               positional value is matched to whichever argument is still
               free.",
        "i" = "Give every argument after the sample by name."
      ),
      class = "samplyr_error_unnamed_argument",
      call = call
    )
  }

  known <- c(owned, accepted)
  stray <- setdiff(nms, known)
  if (length(stray) == 0L) {
    return(invisible(NULL))
  }

  if (stray[[1]] %in% derived) {
    abort_samplyr(
      c(
        "{.arg {stray[[1]]}} cannot be supplied here.",
        "i" = "samplyr derives {.arg {stray[[1]]}} from the executed sample
               and its design.",
        "i" = "It cannot be overridden through {.code ...}."
      ),
      class = "samplyr_error_derived_argument",
      call = call,
      argument = stray[[1]]
    )
  }

  # Let known names win suggestion distance ties.
  suggestion <- suggest_reserved_arg(stray[[1]], c(known, derived))
  advice <- if (!is_null(suggestion) && suggestion %in% derived) {
    cli::format_inline(
      "Did you mean {.arg {suggestion}}? samplyr derives it from the executed
       sample and its design, so it cannot be given here either."
    )
  } else if (!is_null(suggestion)) {
    cli::format_inline("Did you mean {.arg {suggestion}}?")
  } else {
    cli::format_inline(
      "{.code ...} is forwarded to {.fn {forwarded_to}}; the arguments owned
       here are {.arg {owned}}."
    )
  }
  abort_samplyr(
    c(
      "This function received an unexpected argument.",
      "x" = cli::format_inline(
        "{.arg {stray[[1]]}} is not an argument of this function or of
         {.fn {forwarded_to}}."
      ),
      "i" = advice
    ),
    class = "samplyr_error_unknown_argument",
    call = call
  )
}

#' The column names execution writes, which an input may not hold
#'
#' Exact names, not prefixes: `.weight_adj` is the user's.
#' @return The members of `nms` that are reserved.
#' @noRd
samplyr_reserved_names <- function(nms) {
  exact <- c(
    ".weight", ".fpc", ".pik", ".sample_id", ".stage", ".panel",
    ".replicate", ".draw", ".certainty", "._prev_phase_weight"
  )
  generated <- grepl("^\\.(weight|fpc|draw|certainty)_[0-9]+$", nms)
  unique(c(intersect(nms, exact), nms[generated]))
}

#' Validate names before execute() adds sampling columns
#' @noRd
validate_execute_frame_names <- function(
  frame,
  index,
  label = "",
  allow_generated = FALSE,
  call = rlang::caller_env()
) {
  token <- sentence_frame_token(index, label)
  nms <- names(frame)
  if (anyDuplicated(nms) > 0L) {
    duplicated_names <- unique(nms[
      duplicated(nms) | duplicated(nms, fromLast = TRUE)
    ])
    abort_samplyr(
      c(
        "{token} must have unique column names.",
        "x" = "Duplicated names: {.field {duplicated_names}}"
      ),
      class = "samplyr_error_frame_duplicate_names",
      call = call
    )
  }

  if (allow_generated) {
    return(invisible(NULL))
  }

  reserved <- samplyr_reserved_names(nms)
  if (length(reserved) > 0L) {
    abort_samplyr(
      c(
        "{token} uses column names reserved by {.pkg samplyr}.",
        "x" = "Reserved names: {.field {reserved}}",
        "i" = "Rename these input columns before calling {.fn execute}."
      ),
      class = "samplyr_error_frame_reserved_names",
      call = call
    )
  }

  invisible(NULL)
}

#' Collect cluster variables from all stages before the given stage
#' @noRd
collect_ancestor_cluster_vars <- function(design, stage_idx) {
  if (stage_idx <= 1L) return(character(0))
  vars <- character(0)
  for (i in seq_len(stage_idx - 1L)) {
    spec <- design$stages[[i]]
    if (!is_null(spec$clusters)) vars <- c(vars, spec$clusters$vars)
  }
  unique(vars)
}

#' Identity of the realized ancestor occurrence a stage's units sit inside
#'
#' Adds pool-qualified draw indices for with-replacement ancestors.
#' @noRd
collect_ancestor_occurrence_vars <- function(design, stage_idx, sample,
                                             call = caller_env()) {
  if (stage_idx <= 1L) {
    return(character(0))
  }
  vars <- character(0)
  for (i in seq_len(stage_idx - 1L)) {
    spec <- design$stages[[i]]
    draw_col <- paste0(".draw_", i)
    if (is_multi_hit_method(spec$draw_spec)) {
      if (!draw_col %in% names(sample)) {
        abort_panel_missing_identity(i, draw_col, occurrence = TRUE,
                                     call = call)
      }
      vars <- c(vars, spec$strata$vars, spec$clusters$vars, draw_col)
    } else if (!is_null(spec$clusters)) {
      vars <- c(vars, spec$clusters$vars)
    }
  }
  unique(vars)
}

#' @noRd
find_duplicate_key_rows <- function(df, vars) {
  key_df <- df[, vars, drop = FALSE]
  dup <- duplicated(key_df) | duplicated(key_df, fromLast = TRUE)
  unique(key_df[dup, , drop = FALSE])
}

#' Test whether a tbl_sample contains multiple replicates
#'
#' TRUE for `c(1, NA)` by design. Guarded callers reject NA first with
#' `check_single_replicate()`.
#' @noRd
has_multiple_replicates <- function(x) {
  ".replicate" %in% names(x) && length(unique(x$.replicate)) > 1L
}

#' Check that a tbl_sample has at most one replicate
#' @noRd
check_single_replicate <- function(x, fn_name, call = caller_env()) {
  if (!".replicate" %in% names(x)) {
    return(invisible(NULL))
  }
  if (anyNA(x$.replicate)) {
    cli_abort(
      "{.field .replicate} column contains {.val NA} values.",
      call = call,
      class = "samplyr_error_replicated_sample_unsupported"
    )
  }
  if (has_multiple_replicates(x)) {
    n_reps <- length(unique(x$.replicate))
    abort_samplyr(
      c(
        "{.fn {fn_name}} requires a single replicate.",
        "i" = "This sample has {n_reps} replicates.",
        "i" = "Filter first: {.code x |> filter(.replicate == 1)}"
      ),
      class = "samplyr_error_replicated_sample_unsupported",
      call = call
    )
  }
  invisible(NULL)
}

#' Internal tbl_sample column pattern
#'
#' Stripped from reused frames, guarded by `dplyr_col_modify.tbl_sample()`.
#' @noRd
samplyr_internal_col_pattern <-
  "^\\.(weight|fpc|sample_id|stage|draw|certainty|replicate|panel)"

#' Detect a tbl_sample whose class was stripped
#'
#' Accepting one as a frame would rerun stage 1 on inherited weights. The
#' column fallback needs the full signature, not a lone `.weight` column.
#' @noRd
looks_like_stripped_tbl_sample <- function(x) {
  if (is_tbl_sample(x) || !is.data.frame(x)) {
    return(FALSE)
  }

  has_provenance <-
    is_sampling_design(attr(x, "design")) &&
      !is_null(attr(x, "stages_executed")) &&
      is.list(attr(x, "metadata"))

  nms <- names(x)
  has_internal_signature <-
    all(c(".weight", ".sample_id", ".stage") %in% nms) &&
      any(grepl("^\\.weight_[0-9]+$", nms)) &&
      any(grepl("^\\.fpc_[0-9]+$", nms))

  has_provenance || has_internal_signature
}

#' Columns whose values the stored design depends on
#' @noRd
protected_sample_cols <- function(data, design, stages_executed) {
  internal <- grep(samplyr_internal_col_pattern, names(data), value = TRUE)
  design_vars <- character(0)
  for (i in stages_executed) {
    stage_spec <- design$stages[[i]]
    if (!is_null(stage_spec$strata)) {
      design_vars <- c(design_vars, stage_spec$strata$vars)
    }
    if (!is_null(stage_spec$clusters)) {
      design_vars <- c(design_vars, stage_spec$clusters$vars)
    }
  }
  unique(c(internal, intersect(unique(design_vars), names(data))))
}

#' Order-invariant hash of the protected columns
#'
#' Rows are sorted on every protected column first. `.sample_id` alone is not
#' a key because expanded rows of cluster-final stages repeat it.
#' @noRd
protected_values_hash <- function(data, cols) {
  vals <- lapply(cols, function(col) data[[col]])
  if (nrow(data) > 1L) {
    ord <- do.call(order, c(vals, list(method = "radix")))
    vals <- lapply(vals, function(v) v[ord])
  }
  names(vals) <- cols
  rlang::hash(vals)
}

#' Integrity record for an executed sample
#'
#' Authoritative over the per-operation marks, which many table operations
#' bypass. `check_sample_unmodified()` recomputes it.
#' @noRd
sample_integrity_record <- function(data, design, stages_executed) {
  cols <- protected_sample_cols(data, design, stages_executed)
  list(
    n_rows = nrow(data),
    cols = cols,
    hash = protected_values_hash(data, cols),
    col_hashes = integrity_column_hashes(data, cols)
  )
}

#' One hash per protected column, reading a factor by its labels
#'
#' Kept in memory only, to name the changed column. Files carry `hash` alone.
#' @noRd
integrity_column_hashes <- function(data, cols) {
  vals <- lapply(cols, function(col) {
    v <- data[[col]]
    if (is.factor(v)) as.character(v) else v
  })
  if (nrow(data) > 1L) {
    keys <- lapply(vals, utf8_sort_key)
    ord <- do.call(order, c(unname(keys), list(method = "radix")))
    vals <- lapply(vals, function(v) v[ord])
  }
  stats::setNames(vapply(vals, rlang::hash, character(1)), cols)
}

#' The protected columns a sample no longer matches its record on
#' @noRd
integrity_changed_columns <- function(x, integrity) {
  missing <- setdiff(integrity$cols, names(x))
  if (length(missing) > 0L || nrow(x) != integrity$n_rows ||
        is_null(integrity$col_hashes)) {
    return(missing)
  }
  now <- integrity_column_hashes(x, integrity$cols)
  integrity$cols[now != integrity$col_hashes[integrity$cols]]
}

#' Keep only the fields a file records
#' @noRd
integrity_for_file <- function(integrity) {
  if (is_null(integrity)) {
    return(NULL)
  }
  integrity[c("n_rows", "cols", "hash")]
}

#' Per-replicate hashes for the complete-replicate exemption
#' @noRd
replicate_integrity_hashes <- function(data, cols, rep_ids) {
  hashes <- lapply(rep_ids, function(r) {
    protected_values_hash(
      data[data$.replicate == r, , drop = FALSE],
      cols
    )
  })
  setNames(hashes, as.character(rep_ids))
}

#' Compare a sample against its stored integrity record
#'
#' @return "ok", or the failure kind "columns", "rows" or "values".
#' @noRd
verify_sample_integrity <- function(x, integrity) {
  if (!all(integrity$cols %in% names(x))) {
    return("columns")
  }
  if (nrow(x) != integrity$n_rows) {
    return("rows")
  }
  if (!identical(protected_values_hash(x, integrity$cols), integrity$hash) &&
        (is_null(integrity$col_hashes) ||
           length(integrity_changed_columns(x, integrity)) > 0L)) {
    return("values")
  }
  "ok"
}

#' Apply integrity-derived modification marks to a tbl_sample
#'
#' Keeps as_tbl_sample() from laundering a stripped-and-restored object.
#' @noRd
apply_integrity_marks <- function(x) {
  integrity <- attr(x, "metadata")$integrity
  if (is_null(integrity)) {
    return(x)
  }
  status <- verify_sample_integrity(x, integrity)
  if (!identical(status, "ok") && !is_complete_replicate(x)) {
    x <- mark_sample_modified(x, status)
  }
  x
}

#' Record a post-execution modification on a tbl_sample
#'
#' `what` is "rows", "columns" or "values", accumulated in `metadata$modified`.
#' @noRd
mark_sample_modified <- function(x, what) {
  meta <- attr(x, "metadata") %||% list()
  meta$modified <- union(meta$modified, what)
  attr(x, "metadata") <- meta
  x
}

#' Modifications recorded on a tbl_sample
#' @return Character vector, subset of c("rows", "columns", "values").
#' @noRd
sample_modifications <- function(x) {
  attr(x, "metadata")$modified %||% character(0)
}

#' Test whether a row-modified sample is exactly one complete replicate
#'
#' Exempts `filter(.replicate == 1)` from the modified-rows check when the rows
#' match their `.sample_id` block and stored per-replicate hash.
#' @noRd
is_complete_replicate <- function(x) {
  meta <- attr(x, "metadata")
  counts <- meta$replicate_rows
  if (
    is_null(counts) ||
      is_null(names(counts)) ||
      !all(c(".replicate", ".sample_id") %in% names(x))
  ) {
    return(FALSE)
  }
  r <- unique(x$.replicate)
  if (length(r) != 1L || is.na(r)) {
    return(FALSE)
  }
  pos <- match(as.character(r), names(counts))
  if (is.na(pos)) {
    return(FALSE)
  }
  ids <- x$.sample_id
  if (length(ids) == 0L) {
    return(counts[pos] == 0L)
  }
  end <- sum(counts[seq_len(pos)])
  start <- end - counts[pos] + 1L
  structural_ok <- !anyNA(ids) &&
    length(ids) == counts[pos] &&
    anyDuplicated(ids) == 0L &&
    min(ids) == start &&
    max(ids) == end
  if (!structural_ok) {
    return(FALSE)
  }

  integrity <- meta$integrity
  rep_hash <- integrity$replicate_hashes[[as.character(r)]]
  if (!is_null(rep_hash)) {
    if (!all(integrity$cols %in% names(x))) {
      return(FALSE)
    }
    return(identical(protected_values_hash(x, integrity$cols), rep_hash))
  }
  TRUE
}

#' Integrity-aware realization status of a tbl_sample
#'
#' The integrity record decides when present, overriding the marks either way.
#' One complete replicate counts as intact. Without a record the marks decide.
#' @return list(ok, mods), mods joining the marks and the integrity failure.
#' @noRd
sample_realization_status <- function(x) {
  mods <- sample_modifications(x)
  integrity <- attr(x, "metadata")$integrity

  if (!is_null(integrity)) {
    status <- verify_sample_integrity(x, integrity)
    if (identical(status, "ok") || is_complete_replicate(x)) {
      return(list(ok = TRUE, mods = character(0)))
    }
    return(list(ok = FALSE, mods = union(mods, status)))
  }

  if (
    length(mods) == 0 ||
      (identical(mods, "rows") && is_complete_replicate(x))
  ) {
    return(list(ok = TRUE, mods = character(0)))
  }
  list(ok = FALSE, mods = mods)
}

#' Refuse a materialized wave where the joint probabilities are not yet built
#'
#' First-order probabilities are exact and [as_svydesign()] carries the
#' activation as a second phase. The second-order joint is not built.
#' @noRd
check_no_materialized_wave <- function(x, fn_name, call = caller_env()) {
  wave <- attr(x, "metadata")$wave
  if (is_null(wave)) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn {fn_name}} does not take a materialized wave.",
      "x" = "This sample realizes wave {wave$wave}, which is an activation
             of a master rather than a selection of its own.",
      "i" = "For how two waves overlap, ask the master, which can also be
             asked about waves it has not materialized:
             {.code joint_expectation(master, waves = c({wave$wave}, s))}.",
      "i" = "For the master's own joint probabilities:
             {.code joint_expectation(master, frame)}."
    ),
    class = "samplyr_error_wave_joint_unsupported",
    call = call
  )
}

#' Check that a tbl_sample still matches its executed realization
#' @noRd
check_sample_unmodified <- function(x, fn_name, call = caller_env()) {
  status <- sample_realization_status(x)
  if (status$ok) {
    return(invisible(NULL))
  }
  mods <- status$mods
  integrity <- attr(x, "metadata")$integrity
  changed <- if (is_null(integrity)) {
    character(0)
  } else {
    integrity_changed_columns(x, integrity)
  }

  bullets <- character(0)
  if ("rows" %in% mods) {
    bullets <- c(
      bullets,
      "x" = "Rows were removed, added, or duplicated after {.fn execute}."
    )
  }
  if ("columns" %in% mods) {
    gone <- intersect(changed, setdiff(integrity$cols, names(x)))
    bullets <- c(
      bullets,
      "x" = if (length(gone) > 0L) {
        cli::format_inline(
          "{.field {gone}} {?was/were} dropped or renamed after {.fn execute}."
        )
      } else {
        "Internal design columns (e.g. {.field .weight}, {.field .fpc_*}) or design-referenced strata/cluster columns were dropped or renamed after {.fn execute}."
      }
    )
  }
  if ("values" %in% mods) {
    bullets <- c(
      bullets,
      "x" = if (length(changed) > 0L) {
        cli::format_inline(
          "The values of {.field {changed}} no longer match the executed realization."
        )
      } else {
        "Protected values (weights, design metadata, or strata/cluster identifiers) no longer match the executed realization."
      }
    )
  }

  # Do not advise operations that also refuse transformed samples.
  advice <- if (identical(sample_weight_contract(x), "shared")) {
    c(
      "i" = "This sample's weights were shared with a linked target population, and the recorded transformation addresses its rows by position.",
      "i" = "Share weights again from the source sample rather than repairing this one: {.code share_weights(source, ...)}."
    )
  } else {
    c(
      "i" = "For domain (subpopulation) analysis, convert the full sample first, then subset the design: {.code subset(as_svydesign(full_sample), condition)}, or with srvyr: {.code as_survey_design(full_sample) |> filter(condition)}.",
      "i" = "To subsample an executed sample, run a second phase: {.code sampling_design() |> draw(...) |> execute(full_sample)}."
    )
  }

  abort_samplyr(
    c(
      "{.fn {fn_name}} requires a sample that still matches its executed design.",
      bullets,
      advice
    ),
    class = "samplyr_error_modified_sample",
    call = call
  )
}

#' Label each row of a key table
#'
#' Positional, one label per row. `format_key_labels()` deduplicates.
#' @noRd
key_labels <- function(df, vars) {
  if (nrow(df) == 0) {
    return(character(0))
  }
  if (length(vars) == 0L) {
    return(rep("", nrow(df)))
  }
  do.call(
    paste,
    c(df[, vars, drop = FALSE], list(sep = "/"))
  )
}

#' Every distinct key of a set of rows, as the user's values
#'
#' Does not truncate, so "and 12 more" stays out of the quoted keys.
#' @noRd
format_key_labels <- function(df, vars) {
  if (nrow(df) == 0) {
    return(character(0))
  }
  unique(key_labels(df, vars))
}

#' A parent path as the user's values, one level per stage
#'
#' Levels join with " > ", variables within a level with "/". A variable in
#' no `levels` entry, such as a draw index, is a level of its own.
#' @noRd
path_labels <- function(df, vars, levels = list()) {
  if (nrow(df) == 0) {
    return(character(0))
  }
  groups <- Filter(length, lapply(levels, function(lv) lv[lv %in% vars]))
  groups <- c(groups, as.list(setdiff(vars, unlist(groups))))
  groups <- groups[order(vapply(groups, function(g) min(match(g, vars)), 1))]
  parts <- lapply(groups, function(g) key_labels(df, g))
  do.call(paste, c(parts, list(sep = " > ")))
}

#' Each earlier stage's cluster variables, in stage order
#' @noRd
ancestor_cluster_levels <- function(design, stage_idx) {
  if (stage_idx <= 1L) {
    return(list())
  }
  levels <- lapply(design$stages[seq_len(stage_idx - 1L)], function(s) {
    s$clusters$vars
  })
  Filter(length, levels)
}

#' The stratification variables a pool's label shows
#'
#' Leaves out the variables that repeat the parent's own identifier.
#' @noRd
strata_label_vars <- function(strata_spec) {
  strata_spec$label_vars %||% strata_spec$vars
}
