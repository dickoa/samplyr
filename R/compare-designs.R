#' Compare two sampling designs on one frame
#'
#' Resolves both designs on the same register, without drawing, and reports
#' where they agree and where they part. The allocation level asks whether
#' the two designs form the same units and pools at every stage and give
#' each pool the same take. The probability level asks whether they give
#' every unit the same first-order inclusion probability.
#'
#' The allocation level ignores the selection method and the measure of
#' size. A design written with the default `srswor` can therefore stand for a
#' requirement of a protocol, such as five districts in every region, and be
#' compared with a design that draws the districts with probability
#' proportional to size. The probability level compares the designs as they
#' would select.
#'
#' At either level, the method and the measure of size of each stage are
#' listed side by side, outside the verdict. Two designs can agree on their
#' probabilities with different methods, and a requirement written with the
#' default method differs from the design that meets it by construction, so
#' the list describes the two specifications without judging them.
#'
#' @param x,y Two `sampling_design` objects, each with a complete [draw()] on
#'   every stage. A design restored with [read_design()] is one. An executed
#'   sample is refused.
#' @param frame The register both designs are resolved on, a data frame.
#' @param ... Must be empty. Arguments after it are matched by exact name.
#' @param key A bare column of `frame` identifying the population unit. It
#'   must be unique and have no missing values.
#' @param level What to compare, `"allocation"` (default) or
#'   `"probabilities"`.
#' @param atol,rtol Absolute and relative tolerances. Two values `a` and `b`
#'   agree when `abs(a - b) <= atol + rtol * max(abs(a), abs(b))`. They
#'   apply to expected pool sizes and to probabilities. Requested takes are
#'   compared exactly.
#' @param allow_approximate Passed to [exante_probabilities()] at the
#'   probability level. Both designs must then have the same probability
#'   tier.
#'
#' @return An object of class `samplyr_design_comparison`, a list with
#'   - `verdict`: `"agree"`, `"differ"` or `"unresolved"`.
#'   - `level`: the level compared.
#'   - `probability_quality`: at the probability level, the tier both
#'     designs share, `"exact"` or `"approximate"`, and `NA` otherwise. With
#'     `"approximate"`, what agrees is the targets of the methods, which they
#'     meet only to an approximation, and the print says so.
#'   - `checks`: a tibble with one row per check, with `stage` (`NA` for a
#'     check of the whole design), `check`, `status` (`"agree"`, `"differ"`,
#'     `"unresolved"` or `"not_run"`) and `detail`.
#'   - `pools`: at the allocation level, one row per pool the two designs
#'     share, with `stage`, `pool_key`, `N`, `n_target_x`, `n_target_y`,
#'     `n_expected_x`, `n_expected_y` and `agree`.
#'   - `units`: at the probability level, one row per register row, with the
#'     key column, `probability_x`, `probability_y`, `abs_diff`, `rel_diff`
#'     and `agree`.
#'   - `specification`: two rows per stage the designs both have, with
#'     `stage`, `aspect` (`"method"` or `"mos"`), `x`, `y` and `status`.
#'     The status is `"same"`, `"differ"`, or for two measures of size in
#'     different columns, `"proportional"` when one is a constant multiple
#'     of the other on every row of `frame`. It does not enter the verdict.
#'
#'   The verdict is `"differ"` when any check differs, `"unresolved"` when
#'   none differs but one could not run, and `"agree"` otherwise.
#'
#' @details
#' ## Allocation
#'
#' Pools are matched by the frame rows they hold, never by the names of the
#' stratification variables. `stratify_by(province, urban_rural)` and
#' `stratify_by(urban_rural)` under a province parent form the same pools,
#' and `cluster_by(region, province)` and `cluster_by(province)` form the
#' same units when province ids are nested in regions. When the units of a
#' stage differ, its pools and takes are not compared. When the pools
#' differ, the check names the pools of `x` that `y` splits or merges.
#'
#' Two designs on the same frame see the same pools, so a comparison cannot
#' find a category the frame lacks. A province without urban EAs forms one
#' pool in both designs, and they agree. [execute()] and [validate_frame()]
#' report such a parent with `samplyr_message_absent_strata` when the
#' requirement names its strata in an explicit `n` table.
#'
#' ## Probabilities
#'
#' Units with probability zero in one design and positive in the other are a
#' coverage difference and are counted on their own. A relative difference is
#' computed only where both probabilities are positive. A verdict of
#' agreement says that the first-order inclusion probabilities agree on this
#' frame. It says nothing about joint inclusion probabilities, variances or
#' the selection algorithm, which can differ between designs that agree.
#'
#' ## What is not compared
#'
#' A check that cannot run is reported as `"unresolved"` with the reason,
#' never as agreement: designs with different numbers of stages, a stage below
#' a with-replacement stage, methods whose probabilities are unknown, mixed
#' probability tiers, and a stage that sets `on_capped = "error"` on a frame
#' with short pools.
#'
#' @examples
#' frame <- data.frame(
#'   region = rep(c("North", "South", "East"), each = 40),
#'   district = rep(sprintf("d%02d", 1:12), each = 10),
#'   size = rep(c(5, 9, 4, 7, 6, 8, 3, 9, 5, 6, 7, 4), each = 10),
#'   household = 1:120
#' )
#'
#' # A protocol asks for two districts in every region. A sketch design
#' # states that requirement in the grammar, with the default method.
#' requirement <- sampling_design() |>
#'   stratify_by(region) |>
#'   cluster_by(district) |>
#'   draw(n = 2)
#'
#' # The design as written draws districts with PPS.
#' written <- sampling_design() |>
#'   stratify_by(region) |>
#'   cluster_by(district) |>
#'   draw(n = 2, method = "pps_brewer", mos = size)
#' compare_designs(requirement, written, frame, key = household)
#'
#' # A design that shares four districts across the regions departs from it.
#' shared <- sampling_design() |>
#'   stratify_by(region, alloc = "equal") |>
#'   cluster_by(district) |>
#'   draw(n = 4, method = "pps_brewer", mos = size)
#' compare_designs(requirement, shared, frame, key = household)
#'
#' # A design and its own file give the same probabilities.
#' path <- tempfile(fileext = ".json")
#' write_design(written, path)
#' compare_designs(
#'   written, read_design(path), frame,
#'   key = household, level = "probabilities"
#' )
#'
#' @seealso [exante_probabilities()] for the probabilities of one design,
#'   [frame_summary()] for the pools of one design
#' @family diagnostics
#' @export
compare_designs <- function(x, y, frame, ..., key,
                            level = c("allocation", "probabilities"),
                            atol = sqrt(.Machine$double.eps), rtol = 0,
                            allow_approximate = FALSE) {
  check_keyword_args(
    enquos(...),
    c("key", "level", "atol", "rtol", "allow_approximate")
  )
  call <- current_env()
  for (arg in c("x", "y")) {
    if (!is_sampling_design(get(arg))) {
      abort_samplyr(
        c(
          "{.arg {arg}} must be a {.cls sampling_design}.",
          if (is_tbl_sample(get(arg))) {
            c("i" = "Compare designs before execution. An executed sample
                     carries one realization, not the design alone.")
          }
        ),
        class = "samplyr_error_compare_argument"
      )
    }
  }
  level <- with_error_class(
    rlang::arg_match(level),
    "samplyr_error_compare_argument"
  )
  for (arg in c("atol", "rtol")) {
    value <- get(arg)
    if (!is.numeric(value) || length(value) != 1L || !is.finite(value) ||
        value < 0) {
      abort_samplyr(
        "{.arg {arg}} must be one finite number, zero or more.",
        class = "samplyr_error_compare_argument"
      )
    }
  }
  check_exante_approximate_arg(allow_approximate, call = call)
  check_exante_design_complete(x, call = call)
  check_exante_design_complete(y, call = call)
  key_col <- parse_frame_key(
    rlang::enquo(key),
    class = "samplyr_error_exante_key"
  )

  result <- if (identical(level, "allocation")) {
    compare_allocations(x, y, frame, key_col, atol, rtol, call = call)
  } else {
    compare_probabilities(
      x, y, frame, key_col, atol, rtol, allow_approximate,
      call = call
    )
  }
  new_design_comparison(
    level, result$checks, result$pools, result$units,
    probability_quality = result$probability_quality %||% NA_character_,
    specification = compare_specifications(x, y, frame)
  )
}

#' The method and measure of size of each stage, side by side
#'
#' A description outside the verdict. Measures of size in different columns
#' are proportional when one is a constant multiple of the other on every
#' row of the frame, which gives the same probabilities.
#' @noRd
compare_specifications <- function(x, y, frame) {
  rows <- lapply(
    seq_len(min(length(x$stages), length(y$stages))),
    function(k) {
      dx <- x$stages[[k]]$draw_spec
      dy <- y$stages[[k]]$draw_spec
      mos_x <- dx$mos %||% NA_character_
      mos_y <- dy$mos %||% NA_character_
      mos_status <- if (identical(mos_x, mos_y)) {
        "same"
      } else if (!anyNA(c(mos_x, mos_y)) &&
                   proportional_columns(frame, mos_x, mos_y)) {
        "proportional"
      } else {
        "differ"
      }
      tibble::tibble(
        stage = k,
        aspect = c("method", "mos"),
        x = c(dx$method, mos_x),
        y = c(dy$method, mos_y),
        status = c(
          if (identical(dx$method, dy$method)) "same" else "differ",
          mos_status
        )
      )
    }
  )
  if (length(rows) == 0L) {
    return(tibble::tibble(
      stage = integer(), aspect = character(), x = character(),
      y = character(), status = character()
    ))
  }
  do.call(rbind, rows)
}

#' Is one numeric column a constant multiple of another on every row?
#' @noRd
proportional_columns <- function(frame, a, b) {
  if (!all(c(a, b) %in% names(frame))) {
    return(FALSE)
  }
  u <- frame[[a]]
  v <- frame[[b]]
  if (!is.numeric(u) || !is.numeric(v) || anyNA(u) || anyNA(v) ||
      sum(v) <= 0) {
    return(FALSE)
  }
  scale <- sum(u) / sum(v)
  tol <- sqrt(.Machine$double.eps)
  all(within_tolerance(u, scale * v, tol * max(abs(u)), tol))
}

#' Refusals that make a comparison unresolved rather than fail
#'
#' Each says the design cannot be resolved on this frame. Anything else, an
#' invalid key or frame for instance, stops the comparison.
#' @noRd
comparison_unresolved_classes <- c(
  "samplyr_error_exante_unsupported",
  "samplyr_error_exante_approximate",
  "samplyr_error_size_capped",
  "samplyr_error_stage_parent_id"
)

#' Run a resolution, turning a documented refusal into its reason
#' @noRd
try_resolve <- function(expr) {
  tryCatch(
    list(value = expr, reason = NULL),
    error = function(e) {
      if (!rlang::inherits_any(e, comparison_unresolved_classes)) {
        stop(e)
      }
      list(value = NULL, reason = cli::ansi_strip(rlang::cnd_header(e)))
    }
  )
}

#' @noRd
comparison_check <- function(stage, check, status, detail) {
  tibble::tibble(
    stage = as.integer(stage),
    check = check,
    status = status,
    detail = detail
  )
}

#' @noRd
within_tolerance <- function(a, b, atol, rtol) {
  abs(a - b) <= atol + rtol * pmax(abs(a), abs(b))
}

#' The pool and unit of every register row at one stage
#' @noRd
stage_row_partition <- function(resolved, n_rows) {
  pool <- rep(NA_integer_, n_rows)
  unit <- rep(NA_integer_, n_rows)
  offset <- 0L
  for (i in seq_along(resolved$pools)) {
    p <- resolved$pools[[i]]
    pool[p$rows] <- i
    unit[p$rows] <- offset + p$row_units
    offset <- offset + as.integer(p$N)
  }
  list(pool = pool, unit = unit)
}

#' Do two labelings of the same rows group them identically?
#' @noRd
same_partition <- function(a, b) {
  if (!identical(is.na(a), is.na(b))) {
    return(FALSE)
  }
  keep <- !is.na(a)
  a <- a[keep]
  b <- b[keep]
  pairs <- unique(data.frame(a = a, b = b))
  nrow(pairs) == length(unique(a)) && nrow(pairs) == length(unique(b))
}

#' Labels of the pools of one resolved stage
#' @noRd
resolved_pool_labels <- function(walked, stage_idx) {
  resolved <- walked$stages[[stage_idx]]
  first_rows <- vapply(resolved$pools, function(p) p$first_row, integer(1))
  exante_pool_labels(
    walked$design, stage_idx, walked$frames[[stage_idx]], first_rows,
    resolved$strata_vars, resolved$ancestor_vars
  )
}

#' Pools of `a` whose rows fall in more than one pool of `b`
#' @noRd
split_pools <- function(a, b, labels) {
  keep <- !is.na(a) & !is.na(b)
  spans <- tapply(b[keep], a[keep], function(v) length(unique(v)))
  ids <- as.integer(names(spans)[spans > 1L])
  labels[ids]
}

#' @noRd
compare_allocations <- function(x, y, frame, key_col, atol, rtol, call) {
  no_pools <- tibble::tibble(
    stage = integer(), pool_key = character(), N = double(),
    n_target_x = double(), n_target_y = double(),
    n_expected_x = double(), n_expected_y = double(), agree = logical()
  )
  if (length(x$stages) != length(y$stages)) {
    return(list(
      checks = comparison_check(
        NA, "stages", "unresolved",
        sprintf(
          "x has %d stages and y has %d, so no stage can be matched.",
          length(x$stages), length(y$stages)
        )
      ),
      pools = no_pools
    ))
  }
  walked_x <- try_resolve(resolve_exante_register(x, frame, key_col, call = call))
  walked_y <- try_resolve(resolve_exante_register(y, frame, key_col, call = call))
  if (!is_null(walked_x$reason) || !is_null(walked_y$reason)) {
    reasons <- c(
      if (!is_null(walked_x$reason)) paste0("x: ", walked_x$reason),
      if (!is_null(walked_y$reason)) paste0("y: ", walked_y$reason)
    )
    return(list(
      checks = comparison_check(
        NA, "stages", "unresolved", paste(reasons, collapse = " ")
      ),
      pools = no_pools
    ))
  }
  wx <- walked_x$value
  wy <- walked_y$value
  n_rows <- nrow(wx$register)

  checks <- list()
  pools <- list()
  for (k in seq_along(wx$stages)) {
    rx <- wx$stages[[k]]
    ry <- wy$stages[[k]]
    px <- stage_row_partition(rx, n_rows)
    py <- stage_row_partition(ry, n_rows)
    labels_x <- resolved_pool_labels(wx, k)
    labels_y <- resolved_pool_labels(wy, k)

    if (!same_partition(px$unit, py$unit)) {
      checks <- c(checks, list(
        comparison_check(
          k, "units", "differ",
          sprintf(
            "x forms %d units and y forms %d, from different rows.",
            length(unique(stats::na.omit(px$unit))),
            length(unique(stats::na.omit(py$unit)))
          )
        ),
        comparison_check(k, "pools", "not_run", "The units differ."),
        comparison_check(k, "targets", "not_run", "The units differ.")
      ))
      next
    }
    checks <- c(checks, list(comparison_check(
      k, "units", "agree",
      count_text(length(unique(stats::na.omit(px$unit))), "unit")
    )))

    if (!same_partition(px$pool, py$pool)) {
      split_by_y <- split_pools(px$pool, py$pool, labels_x)
      split_by_x <- split_pools(py$pool, px$pool, labels_y)
      detail <- paste(c(
        sprintf("x forms %d pools and y forms %d.",
                length(rx$pools), length(ry$pools)),
        if (length(split_by_y)) {
          sprintf("y splits %s.", format_label_sample(split_by_y))
        },
        if (length(split_by_x)) {
          sprintf("x splits %s.", format_label_sample(split_by_x))
        }
      ), collapse = " ")
      checks <- c(checks, list(
        comparison_check(k, "pools", "differ", detail),
        comparison_check(k, "targets", "not_run", "The pools differ.")
      ))
      next
    }
    checks <- c(checks, list(comparison_check(
      k, "pools", "agree", count_text(length(rx$pools), "pool")
    )))

    first_x <- vapply(rx$pools, function(p) p$rows[[1]], integer(1))
    match_y <- py$pool[first_x]
    target_x <- vapply(rx$pools, function(p) as.double(p$n_target), double(1))
    target_y <- vapply(ry$pools, function(p) as.double(p$n_target), double(1))[match_y]
    expected_x <- vapply(rx$pools, function(p) sum(p$chance), double(1))
    expected_y <- vapply(ry$pools, function(p) sum(p$chance), double(1))[match_y]
    agree <- (target_x == target_y) %in% TRUE &
      within_tolerance(expected_x, expected_y, atol, rtol)
    pools[[k]] <- tibble::tibble(
      stage = k,
      pool_key = labels_x,
      N = vapply(rx$pools, function(p) as.double(p$N), double(1)),
      n_target_x = target_x,
      n_target_y = target_y,
      n_expected_x = expected_x,
      n_expected_y = expected_y,
      agree = agree
    )
    # Takes at a later stage are per candidate parent, so no total is exact.
    totals <- if (k == 1L) {
      sprintf(
        " x takes %s units in all, y %s.",
        format(sum(target_x), big.mark = ","),
        format(sum(target_y), big.mark = ",")
      )
    } else {
      ""
    }
    checks <- c(checks, list(comparison_check(
      k, "targets",
      if (all(agree)) "agree" else "differ",
      if (all(agree)) {
        paste0("Every pool takes the same.", totals)
      } else {
        paste0(
          sprintf(
            "%d of %d pools differ (%s).",
            sum(!agree), length(agree),
            format_label_sample(labels_x[!agree])
          ),
          totals
        )
      }
    )))
  }
  list(
    checks = do.call(rbind, checks),
    pools = if (length(pools)) do.call(rbind, pools) else no_pools
  )
}

#' @noRd
compare_probabilities <- function(x, y, frame, key_col, atol, rtol,
                                  allow_approximate, call) {
  resolve <- function(design) {
    try_resolve(resolve_exante_probabilities(
      design, frame, key_col,
      allow_approximate = allow_approximate, call = call
    ))
  }
  rx <- resolve(x)
  ry <- resolve(y)
  no_units <- tibble::tibble(
    key = character(), probability_x = double(), probability_y = double(),
    abs_diff = double(), rel_diff = double(), agree = logical()
  )
  names(no_units)[[1]] <- key_col
  if (!is_null(rx$reason) || !is_null(ry$reason)) {
    reasons <- c(
      if (!is_null(rx$reason)) paste0("x: ", rx$reason),
      if (!is_null(ry$reason)) paste0("y: ", ry$reason)
    )
    return(list(
      checks = comparison_check(
        NA, "probabilities", "unresolved", paste(reasons, collapse = " ")
      ),
      units = no_units
    ))
  }
  rx <- rx$value
  ry <- ry$value
  if (!identical(rx$probability_quality, ry$probability_quality)) {
    return(list(
      checks = comparison_check(
        NA, "probabilities", "unresolved",
        sprintf(
          "x has %s probabilities and y %s, which are not compared.",
          rx$probability_quality, ry$probability_quality
        )
      ),
      units = no_units
    ))
  }
  # Both resolved the same register, so its rows align.
  if (!identical(rx$keys, ry$keys)) {
    cli_abort(
      "Internal error: the two designs resolved different registers.",
      call = NULL,
      class = "samplyr_error_internal"
    )
  }

  diff <- probability_differences(rx$probability, ry$probability, atol, rtol)
  quality <- rx$probability_quality
  units <- tibble::tibble(
    key = rx$keys,
    probability_x = rx$probability,
    probability_y = ry$probability,
    abs_diff = diff$abs_diff,
    rel_diff = diff$rel_diff,
    agree = diff$agree
  )
  names(units)[[1]] <- key_col
  list(
    checks = comparison_check(
      NA, "probabilities", if (all(diff$agree)) "agree" else "differ",
      diff$detail
    ),
    units = units,
    probability_quality = quality
  )
}

#' Unit by unit differences between two probability vectors
#'
#' A relative difference needs both probabilities positive, and a unit
#' positive in one design only is a coverage difference, counted apart.
#' @noRd
probability_differences <- function(p_x, p_y, atol, rtol) {
  abs_diff <- abs(p_x - p_y)
  positive <- p_x > 0 & p_y > 0
  rel_diff <- ifelse(positive, abs_diff / pmax(p_x, p_y), NA_real_)
  agree <- within_tolerance(p_x, p_y, atol, rtol)
  coverage <- sum((p_x > 0) != (p_y > 0))
  detail <- paste0(
    sprintf(
      "%s units, %s differ. Largest absolute difference %s",
      format(length(agree), big.mark = ","),
      format(sum(!agree), big.mark = ","),
      format(max(c(0, abs_diff)), digits = 3)
    ),
    if (any(positive)) {
      sprintf(", largest relative difference %s",
              format(max(rel_diff, na.rm = TRUE), digits = 3))
    },
    "."
  )
  if (coverage > 0L) {
    detail <- paste(
      detail,
      sprintf(
        "%s units have probability zero in one design only.",
        format(coverage, big.mark = ",")
      )
    )
  }
  list(
    abs_diff = abs_diff,
    rel_diff = rel_diff,
    agree = agree,
    n_coverage = coverage,
    detail = detail
  )
}

#' A count and its noun, singular for one
#' @noRd
count_text <- function(n, noun) {
  paste0(format(n, big.mark = ","), " ", plural_suffix(n, noun), ".")
}

#' Name a few labels and count the rest, as plain text
#' @noRd
format_label_sample <- function(labels, max_shown = 3L) {
  shown <- utils::head(labels, max_shown)
  text <- paste0("\"", shown, "\"", collapse = ", ")
  if (length(labels) > max_shown) {
    text <- paste0(text, " and ", length(labels) - max_shown, " more")
  }
  text
}

#' @noRd
new_design_comparison <- function(level, checks, pools = NULL, units = NULL,
                                  probability_quality = NA_character_,
                                  specification = NULL) {
  verdict <- if (any(checks$status == "differ")) {
    "differ"
  } else if (any(checks$status %in% c("unresolved", "not_run"))) {
    "unresolved"
  } else {
    "agree"
  }
  structure(
    list(
      verdict = verdict,
      level = level,
      probability_quality = probability_quality,
      checks = checks,
      pools = pools,
      units = units,
      specification = specification
    ),
    class = "samplyr_design_comparison"
  )
}
