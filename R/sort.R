#' Serpentine sorting for implicit stratification
#'
#' @description
#' `serp()` implements hierarchic serpentine sorting (also called "snake" sorting),
#' transforming a multi-dimensional hierarchy into a one-dimensional path that
#' preserves spatial contiguity. This is the algorithm used by SAS PROC SURVEYSELECT
#' with `SORT=SERP`.
#'
#' Serpentine sorting alternates direction at each hierarchy level:
#' - First variable: ascending
#' - Second variable: ascending in odd groups of first, descending in even groups
#' - Third variable: alternates based on combined grouping of first two
#' - And so on...
#'
#' This provides implicit stratification when combined with systematic or sequential
#' sampling, ensuring samples spread evenly across geographic/administrative hierarchies.
#'
#' @param ... Columns to sort by, in hierarchical order (e.g., region, district,
#'   commune). Used inside [dplyr::arrange()], similar to [dplyr::desc()].
#'
#' @return A numeric vector (sort key) for use by [dplyr::arrange()].
#'
#' @details
#' ## Algorithm
#'
#' The algorithm builds a composite sort key by:
#'
#' 1. Converting each variable to integer ranks, with character values in
#'    byte order (the C locale) whatever the session locale, and missing
#'    values last
#' 2. For variable i, numbering the cells of variables 1..(i-1) in the order
#'    the traversal visits them
#' 3. Flipping variable i's ranks (descending) in every odd-numbered cell
#' 4. Using multi-column ordering to produce final sort positions
#'
#' Step 2 is the cell's position along the snake, not a count of the ranks
#' above it. The two agree only when every variable above has an odd number
#' of values, and where they disagree the direction fails to reverse at a
#' cell boundary, which is the contiguity the sort exists to provide.
#'
#' ## Use with systematic sampling
#'
#' Serpentine sorting is particularly effective with systematic sampling.
#' By ordering the frame in a snake-like pattern, a systematic sample
#' automatically spreads across all regions and sub-regions.
#'
#' ## Comparison with nested sorting
#'
#' Standard sorting creates large "jumps" at hierarchy boundaries. Serpentine
#' sorting minimizes these by reversing direction, so the last district of
#' region 1 is adjacent to the last district of region 2.
#'
#' @references
#' Chromy, J. R. (1979). Sequential sample selection methods.
#' \emph{Proceedings of the Survey Research Methods Section, ASA}, 401-406.
#'
#' Williams, R. L. and Chromy, J. R. (1980). SAS sample selection MACROS.
#' \emph{Proceedings of the Fifth Annual SAS Users Group International
#' Conference}, 392-396.
#'
#' @seealso [dplyr::arrange()], [dplyr::desc()]
#'
#' @examples
#' library(dplyr)
#'
#' # Basic serpentine sorting with mtcars
#' mtcars |>
#'   arrange(serp(cyl, gear, carb)) |>
#'   select(cyl, gear, carb) |>
#'   head(15)
#'
#' # Compare nested vs serpentine sorting
#' # Nested: gear always ascending within cyl
#' mtcars |>
#'   arrange(cyl, gear) |>
#'   select(cyl, gear) |>
#'   head(12)
#'
#' # Serpentine: gear direction alternates by cyl group
#' mtcars |>
#'   arrange(serp(cyl, gear)) |>
#'   select(cyl, gear) |>
#'   head(12)
#'
#' # Implicit stratification with systematic sampling
#' # Sort BFA EAs in serpentine order, then draw systematic sample
#' sampling_design() |>
#'   draw(n = 100, method = "systematic") |>
#'   execute(arrange(bfa_eas, serp(region, province)),
#'                   seed = 1)
#'
#' # Combine explicit stratification with serpentine sorting
#' # Stratify by urban/rural, use serpentine within strata
#' sampling_design() |>
#'   stratify_by(urban_rural) |>
#'   draw(n = 100, method = "systematic") |>
#'   execute(arrange(bfa_eas, urban_rural, serp(region, province)),
#'                   seed = 1234)
#'
#' @family helpers
#' @export
serp <- function(...) {
  var_vals <- list(...)
  nvars <- length(var_vals)

  if (nvars == 0) {
    abort_samplyr(
      "At least one variable must be specified for serpentine sorting.",
      class = "samplyr_error_serp_no_variables"
    )
  }

  n <- length(var_vals[[1]])
  if (n == 0) {
    return(numeric(0))
  }
  if (n == 1) {
    return(1)
  }

  lengths <- vapply(var_vals, length, integer(1))
  if (length(unique(lengths)) > 1) {
    abort_samplyr(
      "All variables must have the same length.",
      class = "samplyr_error_serp_incompatible_lengths"
    )
  }

  # Radix ranks, so the order is the same in every locale. NAs rank last.
  ranks <- lapply(var_vals, function(v) {
    v <- utf8_sort_key(v)
    lvls <- sort(unique(v[!is.na(v)]), method = "radix")
    r <- match(v, lvls)
    r[is.na(r)] <- length(lvls) + 1L
    r
  })

  if (nvars == 1) {
    return(ranks[[1]])
  }

  sort_keys <- vector("list", nvars)
  sort_keys[[1]] <- ranks[[1]]

  # Cell position on the snake, not a rank sum, whose parity breaks.
  cell <- ranks[[1]]

  for (i in 2:nvars) {
    r <- ranks[[i]]
    max_r <- max(r)

    adjusted_r <- ifelse(cell %% 2L == 1L, r, max_r + 1L - r)
    sort_keys[[i]] <- adjusted_r

    if (i < nvars) {
      # A dense rank: ragged hierarchies have no fixed radix.
      cell <- vctrs::vec_rank(
        data.frame(cell = cell, adjusted = adjusted_r),
        ties = "dense"
      )
    }
  }

  ord <- do.call(order, sort_keys)

  rank_vec <- integer(n)
  rank_vec[ord] <- seq_len(n)
  rank_vec
}

#' The row order a `control` specification sorts a data frame into
#'
#' The same terms `arrange()` accepts: columns, expressions, top-level
#' `desc()` and `serp()`. Keys are ordered by a stable radix sort with NAs
#' last, so the order and the sample drawn from it do not depend on the
#' locale or on how a label is encoded. `arrange()` translates labels of
#' unknown encoding under a C locale and sorts the translation.
#' @noRd
control_order <- function(data, control_quos) {
  keys <- list()
  decreasing <- logical()
  for (quo in control_quos) {
    expr <- quo_get_expr(quo)
    descending <- is_call(expr, "desc", n = 1L, ns = c("", "dplyr"))
    if (descending) {
      quo <- new_quosure(expr[[2L]], quo_get_env(quo))
    }
    value <- rlang::eval_tidy(quo, data)
    columns <- if (is.data.frame(value)) as.list(value) else list(value)
    for (column in columns) {
      column <- utf8_sort_key(vctrs::vec_recycle(column, nrow(data)))
      keys[[length(keys) + 1L]] <- column
      decreasing[[length(decreasing) + 1L]] <- descending
    }
  }
  do.call(
    order,
    c(unname(keys), list(na.last = TRUE, decreasing = decreasing, method = "radix"))
  )
}

#' Character values as UTF-8, ready for a radix sort
#'
#' `order(method = "radix")` refuses non-ASCII strings marked "unknown",
#' which is what `read.csv()` returns in a UTF-8 locale. Those that are valid
#' UTF-8 are marked as UTF-8 and keep their bytes. The rest are translated
#' from the native encoding. A radix sort then orders the UTF-8 bytes, the
#' same in every locale. Anything other than a character vector is returned
#' unchanged.
#' @noRd
utf8_sort_key <- function(x) {
  if (!is.character(x)) {
    return(x)
  }
  native <- which(Encoding(x) == "unknown" & validUTF8(x))
  marked <- x[native]
  Encoding(marked) <- "UTF-8"
  x[native] <- marked
  enc2utf8(x)
}
