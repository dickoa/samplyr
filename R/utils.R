#' Internal utility functions
#'
#' @name utils
#' @keywords internal
NULL

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

# Random-size Poisson methods: independent unit selection with random
# realized sample size. Variance estimation requires the Horvitz-Thompson
# Poisson formula via survey::poisson_sampling(), not the SRSWOR or
# Brewer estimators used for fixed-size designs.
rs_poisson_methods <- c("bernoulli", "pps_poisson")

# A PPS Poisson pool realizing below 95% of what it could have reached is
# reported. One rule, no special cases: a 5% shortfall on a target of 40
# delivers about 38 in expectation, which is a real gap between the nominal
# and realized design. Mild saturation stays well under it, since one unit
# clipped from 1.05 to 1 on a target of 40 is a 0.125% shortfall.
poisson_shortfall_tolerance <- 0.95

# Built-in methods whose true first-order inclusion probabilities equal
# the target pik only to a documented approximation (Rosen's order
# sampling). Every other built-in is exact; sondage::method_spec()
# reports the same tiers.
approx_probability_methods <- c("pps_sps", "pps_pareto")

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
  if (method %in% approx_probability_methods) "approximate" else "exact"
}

#' Fingerprint of a registered method's implementation
#'
#' Hash of the formals and body of the registered sample_fn and
#' joint_fn (sondage >= 0.8.8 exposes them in method_spec()).
#' Deparsing the language objects normalizes formatting and drops
#' comments, so re-registering the same code fingerprints identically;
#' the enclosing environment is not covered. NULL for built-ins and
#' for specs without functions.
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
#'
#' A registered method with probabilities = "unknown" (the strict
#' default) treats the pik it receives as a selection weight, not an
#' honored first-order target, so 1/pik design weights would be
#' systematically biased. samplyr samples are weighted by
#' construction; such a method cannot produce one.
#' @noRd
abort_unknown_probabilities <- function(method,
                                        call = rlang::caller_env()) {
  abort_samplyr(
    c(
      "Method {.val {method}} declares its selection probabilities
       unknown, so design weights cannot be computed.",
      "i" = "samplyr weights samples by 1/probability. A method whose
             true selection probabilities are not known cannot
             produce them.",
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
#'
#' Balanced (cube) selection is without replacement; custom balanced
#' methods carry method_type "balanced" and must count as WOR just
#' like the built-in `cube` method does through the name test.
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
#'
#' A random-size method realizes a count around its target rather than exactly
#' it, so a target above the population caps the *nominal* target while the
#' realized size can still land below it. Every diagnostic that reads a
#' shortfall as running out of units has to exclude these.
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
#' Certainty is a property of the resolved probability, not of how that
#' probability arose. An explicit `certainty_size`/`certainty_prop` rule,
#' capping inside `sondage::inclusion_prob()`, and a balanced design landing
#' on one are the same statistical object: the unit is self-representing and
#' contributes no variance at its stage.
#'
#' Vectorized and elementwise, so the sample, digest, joint matrix and survey
#' export all decide certainty the same way. Callers must pass inclusion
#' probabilities: expected hits from WR/PMR methods are never certainty, even
#' when at least one.
#'
#' The tolerance is deliberately tight, and much tighter than the
#' `sqrt(.Machine$double.eps)` used for approximate-equality tests elsewhere.
#' This is an exactness test, not an approximate one: every producer of a
#' probability-one unit assigns the value rather than converging on it
#' (`pmin(pik, 1)`, an explicit rule, `sondage::inclusion_prob()` capping), so
#' the deviation to absorb is a few eps at most. The error to avoid is the
#' other one: a design probability legitimately just below one, say
#' 1 - 1e-8, must not be read as certainty, because that would drop a real
#' variance contribution and understate the standard error.
#' @noRd
is_certainty_probability <- function(p, tol = 100 * .Machine$double.eps) {
  is.finite(p) & p >= 1 - tol
}

#' Report a per-pool selection diagnostic, aggregate it later
#'
#' Selection leaves know one pool at a time. A stratified stage inside a
#' cluster loop runs the same leaf once per parent, and a replicated execution
#' runs the whole design once per replicate, so a diagnostic emitted where it
#' is detected fires once per pool per replicate. Leaves signal instead, and
#' `execute()` emits one aggregated condition per stage.
#'
#' The envelope is deliberately generic. `operation` names what happened, the
#' payload fields belong to that operation, and the collector groups on
#' `operation` and `stage`. The Poisson shortfall diagnostic routes through the
#' same mechanism.
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
#' `n_available` is the population of every pool the stage executed, not only
#' the capped ones. The reporter needs it to tell a stage that exhausted
#' everything it could reach from one that merely ran short in places, and
#' that comparison has to hold after aggregation across pools, parents and
#' replicates.
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

#' Collect selection events raised inside one stage and re-signal with the
#' stage index attached
#'
#' `expr` is a promise and is forced under the handler. Leaves have no way to
#' know which stage they are running in, and threading the index through every
#' selection signature would touch six functions to carry one integer, so the
#' stage is attached here, at the one place that knows it.
#'
#' Re-signaling happens after the handler has been removed, so the events
#' reach the outer collector in `execute()` rather than this one.
#' Tag selection events with the replicate that produced them
#'
#' Replicates re-run the whole design, so the same pool caps in every one of
#' them. Aggregating without the tag would either report a replicated execution
#' once per replicate or merge genuinely distinct pools of the same stage into
#' one event. The tag lets the reporter aggregate within a replicate and
#' deduplicate across replicates.
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
    rlang::signal(
      message = "",
      class = "samplyr_condition_selection_event",
      operation = cnd$operation,
      stage = stage,
      payload = cnd$payload
    )
  }
  result
}

#' Report a PPS Poisson pool that cannot reach the size it was asked for
#'
#' Measured against `n_reachable`, not against the request. A pool asked for
#' more units than it holds has already had its target clamped by the
#' population, and that reduction is `nominal_cap`'s to report; charging the
#' same units to saturation as well would be double counting.
#'
#' What is left is the reduction saturation alone caused: dominant units whose
#' chances clip at one absorb the target while the pool still has room. Both
#' conditions can be right about one stage, and then both fire: 20 requested
#' from a pool of 10 that resolves to 8.54 has been reduced twice, 20 -> 10 by
#' the population and 10 -> 8.54 by saturation. With uniform sizes the same
#' pool resolves to exactly 10 and only `nominal_cap` fires.
#'
#' @param pik The whole pool's resolved chances, including any explicit
#'   certainty units. A check that sees only the probabilistic remainder
#'   cannot state the pool's totals.
#' @param n_clipped Units whose *computed* chance exceeded one. Explicit
#'   certainty units are selected deliberately at one and are not clipping.
#' @noRd
check_poisson_shortfall <- function(
  pik,
  n_requested,
  n_reachable,
  n_clipped,
  pool_keys = character(0)
) {
  # Direct `pik` leaves `n` optional: with no declared target there is
  # nothing to fall short of.
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
#' A stage running inside a cluster loop sees one parent's rows at a time and
#' labels its pools from the variables it can see, so the same stratum in three
#' parents produces the same label three times. Aggregation then deduplicates
#' three genuinely distinct pools into one, and the report names one pool while
#' counting three.
#'
#' Qualifying at the loop is what keeps the leaves data-agnostic: no selection
#' function has to carry an ancestry it never uses. Same capture-then-resignal
#' shape as `collect_stage_events()`, so the re-signaled event leaves this
#' handler rather than being caught by it.
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
    # A stage with no pools of its own is identified by its parent alone.
    payload$pool_keys <- if (length(keys) == 0L) {
      parent_key
    } else {
      paste(parent_key, keys, sep = " / ")
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
#'
#' The single point where per-pool events become user-facing conditions.
#'
#' Aggregation runs at two levels. Within a replicate the per-pool events of a
#' stage become one aggregate, whose totals are what the user should read: a
#' replicate is one realization of the design. Across replicates those
#' aggregates collapse to a single report, because ten replicates of one design
#' are one finding, not ten.
#'
#' Collapsing cannot require the aggregates to match. A replicated multi-stage
#' design reaches different parents in different replicates, so the same stage
#' legitimately names different pools each time. Hashing the whole aggregate
#' therefore emitted one warning per distinct pool set, which is the spam this
#' machinery exists to prevent. The pool lists are unioned instead, and the
#' report says the counts describe one replicate whenever they varied.
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
  # A literal token rather than NA: an unreplicated execution has no replicate
  # id, and `NA == NA` would match no event at all.
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

    # Classify each replicate before merging any of them. The reading is a
    # property of one realization: a replicate that drew only small clusters
    # exhausted them, and one that drew a large cluster did not. Merging first
    # would let whichever replicate reported earliest name the class for the
    # rest, and would file pools from a census under "capped".
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
#'
#' `population_cap` has two readings and the aggregate decides which: an
#' allocation site sees its own strata, a cluster leaf sees one parent's pools,
#' and neither can tell whether the stage as a whole came up empty-handed.
#' Every other operation reads one way.
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
    allocation_cap = inform_allocation_capped(stage, x),
    cli_abort(
      "Internal error: unhandled selection outcome {.val {outcome}}.",
      call = NULL
    )
  )
}

#' @noRd
summarize_selection_events <- function(group) {
  field <- function(name) {
    lapply(group, function(cnd) cnd$payload[[name]])
  }
  # Absent is not zero. An operation that never records a realized count would
  # otherwise ship `n_actual = 0` in its public payload, which reads as a
  # measurement rather than as a field this event does not carry.
  total <- function(name) {
    values <- unlist(field(name))
    if (length(values) == 0L) {
      return(NA_real_)
    }
    sum(values)
  }

  keys <- unique(unlist(field("pool_keys")))
  list(
    pool_keys = keys,
    n_capped = total("n_capped"),
    n_pools = total("n_pools"),
    n_requested = total("n_requested"),
    n_actual = total("n_actual"),
    n_available = total("n_available"),
    n_reachable = total("n_reachable"),
    n_expected = total("n_expected"),
    n_clipped = total("n_clipped"),
    n_moved = total("n_moved")
  )
}

#' Collapse one stage's per-replicate aggregates into the single report
#'
#' Counts come from the first replicate rather than from a sum, because the
#' user reads them as the size of one realization: ten replicates capping the
#' same three pools capped three pools, not thirty. Pool identities are unioned,
#' since a replicate that reached a different parent found a real pool that the
#' others did not.
#'
#' `varied` records that those two facts came apart, so the report can stop
#' implying that its counts describe the list it prints.
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
#' Stage-local by construction. A second-stage take that exhausts every
#' selected cluster satisfies this while the design as a whole still sampled,
#' so the claim is about the stage and its wording has to stay there.
#'
#' Counting saturated pools would not do: `equal` allocation on populations
#' (1, 100) with `n = 102` has one stratum over its share before
#' redistribution and both strata taken whole after it.
#' @noRd
is_stage_census <- function(x) {
  isTRUE(!is.na(x$n_actual) && !is.na(x$n_available) &&
    x$n_actual >= x$n_available)
}

#' Name a few pools and count the rest
#'
#' A capped design can have hundreds of pools, so the message names enough to
#' start an investigation and points at the digest for the full list.
#' @noRd
format_pool_sample <- function(keys, max_shown = 5L) {
  if (length(keys) == 0L) {
    return(NULL)
  }
  if (length(keys) <= max_shown) {
    return(cli::format_inline("{.val {keys}}"))
  }
  # The count takes the place of the final list item, so the shown keys are
  # joined without cli's trailing "and": ".., "c005", and 35 more", not
  # ".., and "c005", and 35 more". cli's own truncation is not used because it
  # renders a non-ASCII ellipsis and drops the count.
  shown <- cli::cli_vec(
    keys[seq_len(max_shown)],
    style = list("vec-last" = ", ")
  )
  hidden <- length(keys) - max_shown
  cli::format_inline("{.val {shown}}, and {hidden} more")
}

#' Render the pool list, and say so when replicates disagreed
#'
#' When the pools varied across replicates the printed list is a union while
#' the counts describe one realization. Labeling it as such is what keeps the
#' two from reading as the same measurement.
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
#'
#' Stage-local, and the wording carries that: the stage exhausted the pools it
#' executed, which above stage one are the pools a sampled ancestor handed it.
#' Such a stage contributes no variance of its own, and it does not make the
#' design a census.
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
#' Deliberately not the population-cap wording. A Poisson or Bernoulli stage
#' asked for more units than the pool holds has its per-unit chances clamped at
#' 1, which caps the target it aims at; it has not selected that many units,
#' and the realized count is a draw that usually lands below the cap. Naming a
#' selected count here would state a number the sample does not contain.
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
#' Only the affected pools are aggregated, because they are the only ones that
#' signaled. A stage total would let a large pool meeting its target hide a
#' small one that collapsed, which is the case most worth reporting.
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
      "i" = "See the {.val pps_poisson} section of {.fn draw}."
    ),
    class = "samplyr_warning_poisson_shortfall",
    stage = stage,
    operation = "poisson_shortfall",
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
#'
#' Arguments placed after `...` in a signature must be matched exactly, so a
#' near miss such as `seedd` or the singular `stage` falls into `...` instead
#' of raising R's own "unused argument" error. Returns the closest candidate
#' within `max_dist` edits, or `NULL` when nothing is close enough to name.
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
  # An expansion of the argument name is not a near miss by edit distance:
  # `allocation` is five edits from `alloc`. Enable this only where a name
  # is already known to be wrong, so a guess can add advice but never
  # reclassify a legitimate argument.
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
#' Used by the verbs whose `...` carries data, where a misspelled reserved
#' argument would otherwise be diagnosed as bad data. The bullets are
#' formatted here rather than returned as cli templates, because the caller
#' raises them from a frame where these locals no longer exist.
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
#' A function whose optional arguments follow `...` matches them exactly, so a
#' near miss such as the singular `stage` falls into `...` rather than raising
#' R's "unused argument" error. That is the point of the placement: partial
#' matching would otherwise accept `stage`, and `st`, without ever teaching the
#' name, and would break the day an argument sharing that prefix is added.
#'
#' @param dots The caller's `...`, captured with `enquos()`. Quosures, not
#'   values: a stray argument is diagnosed by its name, so forcing it would
#'   let its expression fail first and replace this message with its own.
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
#' A function whose optional arguments follow a `...` matches them exactly, so
#' a near miss such as `nes` for `nest` is forwarded to the downstream package
#' instead of raising R's "unused argument" error. Where the `...` is reserved
#' `check_keyword_args()` refuses everything; here it carries arguments that
#' legitimately belong to someone else, so only names neither side accepts are
#' refused.
#'
#' [rlang::check_dots_used()] is the usual tool and does not work on this path.
#' It reports an argument the downstream function binds but never forces as
#' unused, which rejects valid calls: `as_svrepdesign(x, type = "Fay",
#' fay.rho = 0.3)` returns a design, and `check_dots_used()` refuses it. The
#' accepted names are therefore listed explicitly.
#'
#' A positional value is refused outright. The forwarded arguments are spliced
#' into a call whose named arguments are already fixed, so an unnamed one is
#' matched to whichever formal happens to be free.
#'
#' `derived` names a third category between the two. The downstream function
#' declares the argument, but the caller computes it from the sample and its
#' design and supplies it itself, so a user value collides in the eventual
#' `do.call()` rather than reaching the estimator. Reporting it as unknown
#' would be wrong: the name is known, it is the ownership that is not the
#' user's. One class carries them all, with the name in the `argument` field,
#' so a caller can handle the category without a taxonomy per argument.
#'
#' @param dots The caller's `...`, captured with `enquos()`. Quosures, not
#'   values: a stray argument is diagnosed by its name, so forcing it would let
#'   its expression fail first and replace this message with its own.
#' @param owned The arguments the caller itself owns, for suggestions.
#' @param accepted The argument names the downstream function accepts.
#' @param derived The argument names the caller supplies itself, refused with
#'   their own class. Never listed in `accepted`, but still a candidate for a
#'   spelling suggestion, since `strat` means `strata` whether or not `strata`
#'   can be given; such a suggestion says the name is derived rather than
#'   offering it as a fix.
#' @param forwarded_to The downstream function, unquoted for `{.fn}`.
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

  # Reported on the first stray name in call order, whichever category it
  # falls into, so the two branches cannot disagree about which argument
  # the message is about.
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

  # Derived names are candidates for the suggestion even though they are not
  # accepted: `strat` means `strata` whether or not `strata` can be given, and
  # answering a near miss with the generic advice leaves the user to guess.
  # `known` is listed first so it wins a distance tie.
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

  exact <- c(
    ".weight", ".fpc", ".pik", ".sample_id", ".stage", ".panel",
    ".replicate", ".draw", ".certainty", "._prev_phase_weight"
  )
  generated <- grepl(
    "^\\.(weight|fpc|draw|certainty)_[0-9]+$",
    nms
  )
  reserved <- unique(c(intersect(nms, exact), nms[generated]))
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

#' @noRd
find_duplicate_key_rows <- function(df, vars) {
  key_df <- df[, vars, drop = FALSE]
  dup <- duplicated(key_df) | duplicated(key_df, fromLast = TRUE)
  unique(key_df[dup, , drop = FALSE])
}

#' Test whether a tbl_sample contains multiple replicates
#'
#' Note: if `.replicate` contains NA, unique() includes it, so this
#' returns TRUE for c(1, NA). This is intentionally conservative.
#' In guarded contexts (survey export, svyplan), check_single_replicate()
#' catches NA before calling this. In display contexts (print, summary),
#' treating corrupted data as multi-replicate is the safe default.
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
      call = call
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
#' Columns written by execute() that carry design metadata. Stripped
#' when a tbl_sample is reused as a frame (samplyr_internal_cols) and
#' protected against overwrites by dplyr_col_modify.tbl_sample.
#' @noRd
samplyr_internal_col_pattern <-
  "^\\.(weight|fpc|sample_id|stage|draw|certainty|replicate|panel)"

#' Detect a tbl_sample whose class was stripped
#'
#' Some tidyr and base operations preserve samplyr's attributes and generated
#' columns while dropping only the tbl_sample class. Such an object must not be
#' accepted as an ordinary population frame for a fresh design execution: that
#' would silently rerun stage 1 and treat inherited weights as frame variables.
#'
#' Attributes are definitive evidence. The column fallback deliberately
#' requires the full core/stage signature so an unrelated frame with a single
#' conventional `.weight` column is not rejected.
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
#'
#' Internal metadata columns plus the stratification and clustering
#' variables of the executed stages. These are the columns covered by
#' the integrity record: dropping, renaming, or changing their values
#' breaks the link between the data and the stored design.
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
#' Rows are put in a canonical order before hashing, so harmless
#' reordering (arrange, sorted joins) leaves the hash unchanged. Ties
#' in the ordering are rows with identical protected values, which are
#' interchangeable, so the hash is well defined. (.sample_id alone is
#' not a usable key: it is duplicated across expanded rows of
#' cluster-final stages.)
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
#' Stored in metadata$integrity by every execute path. This is the
#' authoritative description of the realization: per-operation
#' modification marks give immediate feedback and good messages, but
#' too many table operations can bypass an S3 hook (base assignment,
#' rbind(), vctrs operations, third-party verbs), so
#' check_sample_unmodified() recomputes and compares this record at
#' the analysis boundary.
#' @noRd
sample_integrity_record <- function(data, design, stages_executed) {
  cols <- protected_sample_cols(data, design, stages_executed)
  list(
    n_rows = nrow(data),
    cols = cols,
    hash = protected_values_hash(data, cols)
  )
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
#' @return "ok", or the failure kind: "columns" (protected columns
#'   missing), "rows" (row count changed), or "values" (protected
#'   values changed).
#' @noRd
verify_sample_integrity <- function(x, integrity) {
  if (!all(integrity$cols %in% names(x))) {
    return("columns")
  }
  if (nrow(x) != integrity$n_rows) {
    return("rows")
  }
  if (!identical(protected_values_hash(x, integrity$cols), integrity$hash)) {
    return("values")
  }
  "ok"
}

#' Apply integrity-derived modification marks to a tbl_sample
#'
#' Used by as_tbl_sample() so a stripped-and-restored object cannot
#' launder away its modification state.
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
#' `what` is "rows" (row set changed: removed, added, or duplicated),
#' "columns" (an internal design column was overwritten or dropped),
#' or "values" (protected values changed through an untracked route,
#' detected by integrity verification). The marks accumulate in
#' `metadata$modified` and give immediate feedback;
#' check_sample_unmodified() treats the integrity record as
#' authoritative at the analysis boundary.
#' @noRd
mark_sample_modified <- function(x, what) {
  meta <- attr(x, "metadata") %||% list()
  meta$modified <- union(meta$modified, what)
  attr(x, "metadata") <- meta
  x
}

#' Modifications recorded on a tbl_sample
#' @return Character vector, subset of c("rows", "columns").
#' @noRd
sample_modifications <- function(x) {
  attr(x, "metadata")$modified %||% character(0)
}

#' Test whether a row-modified sample is exactly one complete replicate
#'
#' Extracting a single replicate from a replicated execution (for
#' example `filter(.replicate == 1)`) is the documented way to analyze
#' one realization, so it is exempt from the modified-rows check. The
#' extraction is verified against the contiguous `.sample_id` block
#' recorded at execution time (`metadata$replicate_rows`), so a
#' replicate that was further filtered, duplicated, or had its
#' identifier columns rewritten does not pass.
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

  # When the execution stored per-replicate hashes, verify the
  # extracted values match the recorded realization exactly.
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
#' The integrity record is authoritative: when it verifies, the sample
#' IS the executed realization and any per-operation marks were false
#' alarms (e.g. an overwrite with identical values). When it fails, the
#' sample is not the realization even if no operation marked it, which
#' catches routes the S3 hooks cannot see (base assignment, rbind(),
#' vctrs operations, third-party verbs). A sample reduced to exactly
#' one complete replicate (hash-verified when available) counts as
#' intact. Samples without an integrity record (built by older
#' versions or by hand) fall back to the marks.
#' @return list(ok = logical, mods = character): mods combines the
#'   per-operation marks with the integrity failure kind.
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

#' Check that a tbl_sample still matches its executed realization
#' @noRd
check_sample_unmodified <- function(x, fn_name, call = caller_env()) {
  status <- sample_realization_status(x)
  if (status$ok) {
    return(invisible(NULL))
  }
  mods <- status$mods

  bullets <- character(0)
  if ("rows" %in% mods) {
    bullets <- c(
      bullets,
      "x" = "Rows were removed, added, or duplicated after {.fn execute}."
    )
  }
  if ("columns" %in% mods) {
    bullets <- c(
      bullets,
      "x" = "Internal design columns (e.g. {.field .weight}, {.field .fpc_*}) or design-referenced strata/cluster columns were dropped or renamed after {.fn execute}."
    )
  }
  if ("values" %in% mods) {
    bullets <- c(
      bullets,
      "x" = "Protected values (weights, design metadata, or strata/cluster identifiers) no longer match the executed realization."
    )
  }
  if (length(sample_modifications(x)) == 0) {
    bullets <- c(
      bullets,
      "i" = "The change came through a route samplyr does not track per operation (e.g. base assignment, {.fn rbind}, or a vctrs operation); integrity verification caught it at this boundary."
    )
  }

  abort_samplyr(
    c(
      "{.fn {fn_name}} requires a sample that still matches its executed design.",
      bullets,
      "i" = "For domain (subpopulation) analysis, convert the full sample first, then subset the design: {.code subset(as_svydesign(full_sample), condition)}, or with srvyr: {.code as_survey_design(full_sample) |> filter(condition)}.",
      "i" = "To subsample an executed sample, run a second phase: {.code sampling_design() |> draw(...) |> execute(full_sample)}."
    ),
    class = "samplyr_error_modified_sample",
    call = call
  )
}

#' Label each row of a key table
#'
#' One label per row, in row order. `format_key_labels()` deduplicates on top
#' of this; callers holding a table of already-distinct groups need the
#' positional correspondence instead.
#' @noRd
key_labels <- function(df, vars) {
  if (nrow(df) == 0) {
    return(character(0))
  }
  do.call(
    paste,
    c(df[, vars, drop = FALSE], list(sep = "/"))
  )
}

#' @noRd
format_key_labels <- function(df, vars, max_n = 8L) {
  if (nrow(df) == 0) {
    return(character(0))
  }

  labels <- unique(key_labels(df, vars))

  if (length(labels) <= max_n) {
    return(labels)
  }

  c(
    labels[seq_len(max_n)],
    paste0("... and ", length(labels) - max_n, " more")
  )
}
