#' Define stratification
#'
#' `stratify_by()` specifies stratification variables and an optional
#' allocation method for a sampling design, so that every subgroup the
#' variables define is represented. It applies to the current stage and comes
#' before that stage's [draw()], which closes it.
#'
#' @param .data A `sampling_design` object (piped from [sampling_design()],
#'   [add_stage()], or [cluster_by()]), before the stage's [draw()].
#' @param ... Stratification variables as bare column names. A name given
#'   here is a label and does not rename the variable. A label resembling one
#'   of the arguments below (`allocc`, `varianc`) is refused, because those
#'   arguments follow `...` and are matched exactly.
#' @param alloc Character string naming the allocation method. With a method,
#'   `n` in [draw()] is the *total* sample size to distribute. One of:
#'   - `NULL` (default): No allocation. `n` in [draw()] is *per stratum*
#'   - `"equal"`: Equal allocation across strata
#'   - `"proportional"`: Proportional to the number of the stage's sampling
#'     units in each stratum, or to `importance` when it is supplied
#'   - `"neyman"`: Neyman optimal allocation (requires `variance`)
#'   - `"optimal"`: Cost-variance optimal allocation (requires `variance` and `cost`)
#'   - `"power"`: Power allocation (requires `cv` and `importance`)
#'
#'   Every method caps a stratum at its population and redistributes the
#'   surplus, so `"equal"` on populations \eqn{(10, 490, 500)} with `n = 300`
#'   gives 10/145/145 rather than 100/100/100. `execute()` reports this with
#'   a message of class `samplyr_message_allocation_capped` (see
#'   [execution-conditions]).
#'
#'   An input the method does not read is refused with class
#'   `samplyr_error_alloc_unused_aux` rather than ignored: `variance`
#'   next to `"proportional"`, `cost` next to `"neyman"`, or any of
#'   `variance`, `cost`, `cv`, `importance`, `power` with no `alloc`.
#' @param variance Stratum variances for Neyman or optimal allocation. Either
#'   a data frame with every stratification column (the join keys) plus a
#'   `var` column, or, with a single stratification variable only, a named
#'   numeric vector whose names match the values of the stratification
#'   column (for example `c(A = 1.2, B = 0.8)`).
#' @param cost Stratum costs for optimal allocation, in the same two forms
#'   as `variance` with a `cost` column.
#' @param cv Stratum coefficients of variation (\eqn{C_h}) for power
#'   allocation, in the same two forms as `variance` with a `cv` column.
#' @param importance A positive size per stratum (\eqn{X_h}), in the same two
#'   forms as `variance` with an `importance` column. It is what
#'   `"proportional"` allocates in proportion to, such as each stratum's
#'   household total, or the importance measure of power allocation.
#' @param power Power exponent \eqn{q} for power allocation.
#'   Must satisfy \eqn{0 \le q \le 1}. Defaults to `0.5`.
#'
#' @return A modified `sampling_design` object with stratification specified.
#'
#' @details
#' ## Equal allocation
#' Each stratum receives n/H units, where H is the number of strata.
#'
#' ## Proportional allocation
#' Each stratum receives \eqn{n \times N_h/N}{n * N_h/N} units, where \eqn{N_h}
#' is the number of the stage's sampling units in stratum h and N their total.
#' At a clustered stage \eqn{N_h} counts PSUs, not the elements inside them,
#' and `mos` does not enter the allocation.
#'
#' With `importance`, each stratum receives
#' \eqn{n \times X_h / \sum X_h}{n * X_h / sum(X_h)} units instead, so a
#' clustered stage can allocate its PSUs in proportion to the households they
#' hold. This gives the same sizes as power allocation with every
#' \eqn{C_h = 1} and \eqn{q = 1}. A two-stage plan allocated on the ultimate
#' units, from [svyplan::n_alloc()] with a cluster model, is passed to
#' [draw()] directly.
#'
#' ## Neyman allocation
#' Minimizes variance for a fixed sample size. Each stratum receives
#' \eqn{n \times (N_h \times S_h) / \sum(N_h \times S_h)}{n * (N_h * S_h) / sum(N_h * S_h)}
#' units, where S_h is the stratum standard deviation. At a clustered stage
#' the units allocated are PSUs, so S_h is the standard deviation of PSU
#' totals.
#'
#' ## Optimal allocation
#' Minimizes variance for fixed cost (or cost for fixed variance). Each
#' stratum receives
#' \eqn{n \times (N_h \times S_h / \sqrt{C_h}) / \sum(N_h \times S_h / \sqrt{C_h})}{n * (N_h * S_h / sqrt(C_h)) / sum(N_h * S_h / sqrt(C_h))}
#' units, where C_h is the per-unit cost in stratum h.
#'
#' ## Power allocation
#' A compromise allocation (Bankier, 1988) with
#' \eqn{n_h \propto C_h \times X_h^q}{n_h proportional to C_h * X_h^q}, where
#' \eqn{C_h} is the stratum CV, \eqn{X_h} a stratum importance measure, and
#' \eqn{q \in [0, 1]}{0 <= q <= 1}.
#'
#' ## Population bounds and redistribution
#'
#' For an allocation method the realized sizes satisfy
#' \eqn{\sum_h n_h = n}{sum(n_h) = n} and
#' \eqn{0 \le n_h \le N_h}{0 <= n_h <= N_h} whenever the request is feasible.
#' The population cap is always present, and `min_n` and `max_n` add user
#' bounds on top of it.
#'
#' Saturated strata are fixed at their bound and removed, and the remaining
#' total is reallocated over the other strata with the method's own factors
#' (\eqn{N_h} for proportional,
#' \eqn{N_h \sqrt{S_h^2}}{N_h * sqrt(var_h)} for Neyman, and so on), which
#' keeps the stated criterion after saturation. When the free strata all
#' carry zero factors, the remainder is split equally in stratum order.
#'
#' Real-valued targets become integers by largest remainder, which keeps the
#' total. Strata tied for a unit are served in a fixed order of their labels,
#' the "stratum order" above. It depends on neither the order of the frame's
#' rows nor the session locale, so a permuted frame gets the same allocation.
#'
#' Requesting more than the frame holds allocates every unit rather than
#' failing, and `execute()` warns with class `samplyr_warning_census`, or
#' `samplyr_warning_nominal_cap` for a random-size method (see
#' [execution-conditions]). Bounds that make the request impossible are
#' errors of class `samplyr_error_alloc_min_infeasible` or
#' `samplyr_error_alloc_max_infeasible`.
#'
#' With-replacement (`srswr`, `pps_multinomial`) and minimum-replacement
#' (`pps_chromy`) methods can select a unit more than once, so the number of
#' distinct units does not bound the draws, and only `min_n` and `max_n` bind.
#'
#' Per-stratum sizes given directly, through a scalar or named `n`, a `frac`,
#' or a data frame, are never redistributed. Selection caps an impossible one
#' and reports it.
#'
#' ## Custom allocation
#' For sizes or rates chosen per stratum, pass [draw()] a data frame as `n`
#' or `frac`, with every stratification column plus an `n` or `frac` column.
#'
#' @examples
#' # Simple stratification: 20 EAs per region
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 20) |>
#'   execute(bfa_eas, seed = 1234)
#'
#' # Proportional allocation across regions
#' sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 123)
#'
#' # Neyman allocation using pre-computed variances
#' sampling_design() |>
#'   stratify_by(region, alloc = "neyman", variance = bfa_eas_variance) |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 12)
#'
#' # Optimal allocation considering both variance and cost
#' sampling_design() |>
#'   stratify_by(region, alloc = "optimal",
#'               variance = bfa_eas_variance,
#'               cost = bfa_eas_cost) |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 1)
#'
#' # Power allocation (Bankier, 1988)
#' sampling_design() |>
#'   stratify_by(
#'     region,
#'     alloc = "power",
#'     cv = data.frame(
#'       region = levels(bfa_eas$region),
#'       cv = c(0.40, 0.35, 0.12, 0.20, 0.30, 0.18,
#'              0.15, 0.38, 0.22, 0.32, 0.17, 0.45, 0.25)
#'     ),
#'     importance = data.frame(
#'       region = levels(bfa_eas$region),
#'       importance = c(60, 40, 120, 70, 80, 65,
#'                      50, 55, 90, 75, 45, 35, 30)
#'     ),
#'     power = 0.5
#'   ) |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 7)
#'
#' # EAs allocated in proportion to each region's households rather than to
#' # its number of EAs
#' households <- stats::aggregate(households ~ region, bfa_eas, sum)
#' names(households)[2] <- "importance"
#' sampling_design() |>
#'   stratify_by(region, alloc = "proportional", importance = households) |>
#'   cluster_by(ea_id) |>
#'   draw(n = 100) |>
#'   execute(bfa_eas, seed = 7)
#'
#' # Custom sample sizes per stratum using a data frame
#' custom_sizes <- data.frame(
#'   region = levels(bfa_eas$region),
#'   n = c(20, 12, 25, 18, 22, 16, 14, 15, 20, 18, 12, 10, 8)
#' )
#' sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = custom_sizes) |>
#'   execute(bfa_eas, seed = 2026)
#'
#' # Multiple stratification variables
#' sampling_design() |>
#'   stratify_by(region, urban_rural, alloc = "proportional") |>
#'   draw(n = 300, min_n = 2) |>
#'   execute(bfa_eas, seed = 2025)
#'
#' @references
#' Bankier, M.D. (1988). Power allocations: determining sample sizes for
#' subnational areas. *The American Statistician*, 42(3), 174-177.
#'
#' @seealso
#' [sampling_design()] for creating designs,
#' [draw()] for specifying sample sizes,
#' [cluster_by()] for cluster sampling
#'
#' @family design specification
#' @export
stratify_by <- function(
  .data,
  ...,
  alloc = NULL,
  variance = NULL,
  cost = NULL,
  cv = NULL,
  importance = NULL,
  power = NULL
) {
  if (is.data.frame(.data)) {
    abort_frame_misplaced("stratify_by")
  }
  if (!is_sampling_design(.data)) {
    cli_abort(
      "{.arg .data} must be a {.cls sampling_design} object",
      class = "samplyr_error_design_expected"
    )
  }
  check_stage_open(.data, "stratify_by")

  vars_quo <- enquos(...)
  if (length(vars_quo) == 0) {
    cli_abort(
      "At least one stratification variable must be specified",
      class = "samplyr_error_grouping_variables"
    )
  }

  check_grouping_dots(
    vars_quo, "stratify_by",
    c("alloc", "variance", "cost", "cv", "importance", "power"),
    "Stratification variables are passed as bare column names."
  )

  is_bare_name <- vapply(
    vars_quo,
    function(q) is.symbol(quo_get_expr(q)),
    logical(1)
  )
  if (any(!is_bare_name)) {
    # Suggest prefix matches only after the name is known to be invalid.
    offending <- (names(vars_quo) %||% rep("", length(vars_quo)))[!is_bare_name]
    meant <- NULL
    for (nm in offending) {
      meant <- suggest_reserved_arg(
        nm,
        c("alloc", "variance", "cost", "cv", "importance", "power"),
        prefix = TRUE
      )
      if (!is_null(meant)) {
        break
      }
    }
    cli_abort(c(
      "{.fn stratify_by} variables must be bare column names.",
      "x" = "Tidy-select helpers and expressions are not supported.",
      if (!is_null(meant)) {
        c("i" = cli::format_inline("Did you mean {.arg {meant}}?"))
      },
      "i" = "Example: {.code stratify_by(region, strata)}"
    ), class = "samplyr_error_grouping_variables")
  }

  vars <- unname(vapply(vars_quo, as_label, character(1)))

  alloc <- check_alloc_method(alloc)

  check_alloc_inputs_used(
    alloc,
    list(
      variance = variance, cost = cost, cv = cv,
      importance = importance, power = power
    )
  )

  if (!is_null(variance)) {
    variance <- coerce_aux_input(variance, vars, "var", "variance")
  }
  if (!is_null(cost)) {
    cost <- coerce_aux_input(cost, vars, "cost", "cost")
  }
  if (!is_null(cv)) {
    cv <- coerce_aux_input(cv, vars, "cv", "cv")
  }
  if (!is_null(importance)) {
    importance <- coerce_aux_input(importance, vars, "importance", "importance")
  }
  if (identical(alloc, "power") && is_null(power)) {
    power <- 0.5
  }

  validate_stratify_args(
    alloc = alloc,
    variance = variance,
    cost = cost,
    cv = cv,
    importance = importance,
    power = power,
    vars = vars
  )

  if (!is_null(variance)) {
    variance <- variance[, c(vars, "var"), drop = FALSE]
  }
  if (!is_null(cost)) {
    cost <- cost[, c(vars, "cost"), drop = FALSE]
  }
  if (!is_null(cv)) {
    cv <- cv[, c(vars, "cv"), drop = FALSE]
  }
  if (!is_null(importance)) {
    importance <- importance[, c(vars, "importance"), drop = FALSE]
  }

  strata_spec <- new_stratum_spec(
    vars = vars,
    alloc = alloc,
    variance = variance,
    cost = cost,
    cv = cv,
    importance = importance,
    power = power
  )

  current <- .data$current_stage
  if (current < 1 || current > length(.data$stages)) {
    cli_abort(
      "Invalid design state: no current stage",
      class = "samplyr_error_internal"
    )
  }

  if (!is_null(.data$stages[[current]]$strata)) {
    cli_abort(
      "Stratification already defined for this stage. Use {.fn add_stage} to start a new stage.",
      class = "samplyr_error_stage_duplicate"
    )
  }

  .data$stages[[current]]$strata <- strata_spec
  .data$validated <- FALSE
  .data
}

#' Catch a misspelled reserved argument before it is read as a variable
#'
#' The reserved arguments of `stratify_by()` and `cluster_by()` sit after
#' `...`, so a near miss such as `allocc` or `Nest` is captured as a grouping
#' variable and then reported as a bad variable expression. Naming the
#' argument is the useful diagnosis. Names that resemble no reserved argument
#' are left alone: they are ignored labels, as in `stratify_by(reg = region)`.
#' @noRd
check_grouping_dots <- function(vars_quo, fn, reserved, hint,
                                call = rlang::caller_env()) {
  nms <- names(vars_quo) %||% rep("", length(vars_quo))

  for (i in seq_along(vars_quo)) {
    name <- nms[[i]]
    if (!nzchar(name) || is.null(suggest_reserved_arg(name, reserved))) {
      next
    }
    abort_samplyr(
      c(
        "{.fn {fn}} received an unexpected argument.",
        stray_arg_bullets(name, reserved),
        "i" = hint
      ),
      class = "samplyr_error_unknown_argument",
      call = call
    )
  }

  invisible(vars_quo)
}

#' The auxiliary inputs each allocation reads
#' @noRd
alloc_inputs <- list(
  equal = character(0),
  proportional = "importance",
  neyman = "variance",
  optimal = c("variance", "cost"),
  power = c("cv", "importance", "power")
)

#' Refuse an allocation input the allocation does not read
#'
#' An input the rule ignores was dropped without a word, so `importance`
#' next to `alloc = "proportional"` looked like it shaped the allocation.
#' `stratify_by()` checks what the caller typed and the allocator checks the
#' stored spec, which is what a design file restores.
#' @noRd
check_alloc_inputs_used <- function(alloc, inputs, call = caller_env()) {
  supplied <- names(inputs)[!vapply(inputs, is_null, logical(1))]
  used <- if (is_null(alloc)) character(0) else alloc_inputs[[alloc]]
  unused <- setdiff(supplied, used)
  if (length(unused) == 0L) {
    return(invisible(NULL))
  }
  # Name the allocations that read every unused input, else any of them.
  reads_all <- vapply(alloc_inputs, function(x) all(unused %in% x), NA)
  reads_any <- vapply(alloc_inputs, function(x) any(unused %in% x), NA)
  readers <- names(alloc_inputs)[if (any(reads_all)) reads_all else reads_any]
  problem <- if (is_null(alloc)) {
    cli::format_inline(
      "{.arg {unused}} {?is/are} read only by an allocation method, and
       there is no {.arg alloc}."
    )
  } else {
    cli::format_inline(
      "{.val {alloc}} allocation does not use {.arg {unused}}."
    )
  }
  abort_samplyr(
    c(
      problem,
      "i" = cli::format_inline(
        "Remove {.arg {unused}}, or choose an allocation that reads
         {cli::qty(length(unused))}{?it/them}: {.val {readers}}."
      )
    ),
    class = "samplyr_error_alloc_unused_aux",
    call = call
  )
}

#' The allocation methods `stratify_by()` knows
#' @noRd
valid_alloc_methods <- c("equal", "proportional", "neyman", "optimal", "power")

#' Refuse an allocation name `stratify_by()` does not know
#' @noRd
check_alloc_method <- function(alloc, call = caller_env()) {
  if (is_null(alloc)) {
    return(NULL)
  }
  hit <- if (is_character(alloc) && length(alloc) == 1L && !is.na(alloc)) {
    match(alloc, valid_alloc_methods)
  } else {
    NA_integer_
  }
  if (is.na(hit)) {
    given <- if (rlang::is_string(alloc)) {
      cli::format_inline("{.val {alloc}} is not one of them.")
    } else {
      cli::format_inline("It is {.obj_type_friendly {alloc}}.")
    }
    abort_samplyr(
      c(
        "{.arg alloc} must be one of {.val {valid_alloc_methods}}.",
        "x" = given
      ),
      class = "samplyr_error_alloc_unknown_method",
      call = call
    )
  }
  valid_alloc_methods[[hit]]
}

#' @noRd
validate_stratify_args <- function(
  alloc,
  variance,
  cost,
  cv,
  importance,
  power,
  vars,
  call = rlang::caller_env()
) {
  if (!is_null(alloc)) {
    switch(alloc,
      neyman = {
        if (is_null(variance)) {
          cli_abort(
            "Neyman allocation requires {.arg variance} data frame",
            call = call,
            class = "samplyr_error_aux_required"
          )
        }
      },
      optimal = {
        if (is_null(variance)) {
          cli_abort(
            "Optimal allocation requires {.arg variance} data frame",
            call = call,
            class = "samplyr_error_aux_required"
          )
        }
        if (is_null(cost)) {
          cli_abort(
            "Optimal allocation requires {.arg cost} data frame",
            call = call,
            class = "samplyr_error_aux_required"
          )
        }
      },
      power = {
        if (is_null(cv)) {
          cli_abort(
            "Power allocation requires {.arg cv} data frame or named vector",
            call = call,
            class = "samplyr_error_aux_required"
          )
        }
        if (is_null(importance)) {
          cli_abort(
            "Power allocation requires {.arg importance} data frame or named vector",
            call = call,
            class = "samplyr_error_aux_required"
          )
        }
        if (!is.numeric(power) || length(power) != 1 || !is_finite_numeric(power)) {
          abort_samplyr(
            "{.arg power} must be a single finite number in [0, 1]",
            class = "samplyr_error_alloc_power_bounds",
            call = call
          )
        }
        if (power < 0 || power > 1) {
          abort_samplyr(
            "{.arg power} must be between 0 and 1",
            class = "samplyr_error_alloc_power_bounds",
            call = call
          )
        }
      }
    )
  }

  if (!is_null(variance)) {
    validate_aux_df(variance, vars, "var", "variance", call = call)
  }

  if (!is_null(cost)) {
    validate_aux_df(cost, vars, "cost", "cost", call = call)
  }
  if (!is_null(cv)) {
    validate_aux_df(cv, vars, "cv", "cv", call = call)
  }
  if (!is_null(importance)) {
    validate_aux_df(importance, vars, "importance", "importance", call = call)
  }
  invisible(NULL)
}

#' @noRd
validate_aux_df <- function(
  df,
  vars,
  value_col,
  arg_name,
  call = rlang::caller_env()
) {
  # Callers validate auxiliary input types before this helper.
  missing_vars <- setdiff(vars, names(df))
  if (length(missing_vars) > 0) {
    abort_samplyr(
      c(
        "{.arg {arg_name}} is missing stratification variable{?s}:",
        "x" = "{.val {missing_vars}}"
      ),
      class = "samplyr_error_aux_missing_columns",
      call = call
    )
  }

  if (!value_col %in% names(df)) {
    abort_samplyr(
      "{.arg {arg_name}} must contain a {.val {value_col}} column",
      class = "samplyr_error_aux_missing_value_column",
      call = call
    )
  }

  key_df <- df[, vars, drop = FALSE]
  if (anyNA(key_df)) {
    missing_key_cols <- vars[vapply(key_df, anyNA, logical(1))]
    abort_samplyr(
      c(
        "{.arg {arg_name}} has missing values in stratification keys.",
        "x" = "Columns with missing values: {.val {missing_key_cols}}"
      ),
      class = "samplyr_error_aux_missing_key_values",
      call = call
    )
  }

  dup_keys <- find_duplicate_key_rows(df, vars)
  if (nrow(dup_keys) > 0) {
    dup_labels <- format_key_labels(dup_keys, vars)
    abort_samplyr(
      c(
        "{.arg {arg_name}} has duplicate rows for the same stratum.",
        "x" = "Duplicate keys: {format_pool_sample(dup_labels)}"
      ),
      class = "samplyr_error_aux_duplicate_keys",
      call = call
    )
  }

  values <- df[[value_col]]
  if (!is_finite_numeric(values)) {
    abort_samplyr(
      "{.arg {arg_name}} column {.val {value_col}} must be finite numeric values (no NA/NaN/Inf).",
      class = "samplyr_error_aux_non_finite_values",
      call = call
    )
  }

  switch(arg_name,
    variance = {
      if (any(values < 0)) {
        abort_samplyr(
          "{.arg variance} values must be non-negative",
          class = "samplyr_error_aux_variance_bounds",
          call = call
        )
      }
    },
    cost = {
      if (any(values <= 0)) {
        abort_samplyr(
          "{.arg cost} values must be positive",
          class = "samplyr_error_aux_cost_bounds",
          call = call
        )
      }
    },
    cv = {
      if (any(values <= 0)) {
        abort_samplyr(
          "{.arg cv} values must be positive",
          class = "samplyr_error_aux_cv_bounds",
          call = call
        )
      }
    },
    importance = {
      if (any(values <= 0)) {
        abort_samplyr(
          "{.arg importance} values must be positive",
          class = "samplyr_error_aux_importance_bounds",
          call = call
        )
      }
    }
  )
  invisible(NULL)
}

#' Coerce auxiliary input to a data frame
#'
#' Accepts either a data frame (returned as-is) or a named numeric vector
#' which is converted to a two-column data frame. Named vectors are only
#' supported when there is a single stratification variable.
#' @noRd
coerce_aux_input <- function(
  x,
  vars,
  value_col,
  arg_name,
  call = rlang::caller_env()
) {
  if (is.data.frame(x)) {
    return(x)
  }

  if (is.numeric(x) && !is_null(names(x))) {
    if (length(vars) != 1) {
      cli_abort(
        c(
          "{.arg {arg_name}} as a named vector is only supported with a single stratification variable.",
          "i" = "Current stratification variables: {.val {vars}}.",
          "i" = "Use a data frame with columns {.val {c(vars, value_col)}}."
        ),
        call = call,
        class = "samplyr_error_aux_invalid_input_type"
      )
    }
    df <- data.frame(names(x), unname(x), stringsAsFactors = FALSE)
    names(df) <- c(vars, value_col)
    return(df)
  }

  if (is.numeric(x) && is_null(names(x))) {
    abort_samplyr(
      c(
        "{.arg {arg_name}} must be a data frame or a named numeric vector.",
        "i" = "For one stratification variable, use names as stratum levels (e.g., {.code c(A = 1, B = 2)}).",
        "i" = "For multiple stratification variables, use a data frame."
      ),
      class = "samplyr_error_aux_invalid_input_type",
      call = call
    )
  }

  abort_samplyr(
    c(
      "{.arg {arg_name}} must be a data frame or a named numeric vector.",
      "i" = "Named vectors are only supported with one stratification variable."
    ),
    class = "samplyr_error_aux_invalid_input_type",
    call = call
  )
}
