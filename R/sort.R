#' Serpentine Sorting for Implicit Stratification
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
#' 1. Converting each variable to integer ranks
#' 2. For variable i, determining group membership from variables 1..(i-1)
#' 3. If the cumulative group number is even, flipping ranks (descending)
#' 4. Combining all adjusted ranks into a single numeric key
#'
#' ## Use with Systematic Sampling
#'
#' Serpentine sorting is particularly effective with systematic sampling.
#' By ordering the frame in a snake-like pattern, a systematic sample
#' automatically spreads across all regions and sub-regions.
#'
#' ## Comparison with Nested Sorting
#'
#' Standard sorting creates large "jumps" at hierarchy boundaries. Serpentine
#' sorting minimizes these by reversing direction—the last district of region 1
#' is adjacent to the last district of region 2.
#'
#' @references
#' Chromy, J. R. (1979). Sequential sample selection methods.
#' \emph{Proceedings of the Survey Research Methods Section, ASA}, 401-406.
#'
#' Williams, R. L., & Chromy, J. R. (1980). SAS sample selection macros.
#' \emph{Proceedings of the Fifth Annual SAS Users Group International}, 392-396.
#'
#' @seealso [dplyr::arrange()], [dplyr::desc()]
#'
#' @examples
#' library(dplyr)
#'
#' # Use inside arrange() like desc()
#' mtcars |>
#'   arrange(serp(cyl, gear, carb)) |>
#'   select(cyl, gear, carb) |>
#'   head(15)
#'
#' # Compare nested vs serpentine sorting:
#' # Nested (gear always ascending within cyl)
#' mtcars |> arrange(cyl, gear) |> select(cyl, gear) |> head(12)
#'
#' # Serpentine (gear descends in even-numbered cyl groups)
#' mtcars |> arrange(serp(cyl, gear)) |> select(cyl, gear) |> head(12)
#'
#' # With systematic sampling for implicit stratification
#' \dontrun{
#' niger_eas |>
#'   arrange(serp(region, department, commune)) |>
#'   sampling_design() |>
#'   draw(n = 100, method = "systematic") |>
#'   execute()
#' }
#'
#' @export
serp <- function(...) {
  var_vals <- list(...)
  nvars <- length(var_vals)

  if (nvars == 0) {
    cli_abort("At least one variable must be specified for serpentine sorting.")
  }

  n <- length(var_vals[[1]])
  if (n == 0) return(numeric(0))
  if (n == 1) return(1)

  if (nvars == 1) {
    return(xtfrm(var_vals[[1]]))
  }

  lengths <- vapply(var_vals, length, integer(1))
  if (length(unique(lengths)) > 1) {
    cli_abort("All variables must have the same length.")
  }

  ranks <- lapply(var_vals, function(v) {
    lvls <- sort(unique(v[!is.na(v)]))
    r <- as.integer(factor(v, levels = lvls))
    r[is.na(r)] <- length(lvls) + 1L
    r
  })

  sort_keys <- vector("list", nvars)
  sort_keys[[1]] <- ranks[[1]]


  group_id <- ranks[[1]]

  for (i in 2:nvars) {
    # Parity: odd = ascending, even = descending
    parity <- group_id %% 2L

    r <- ranks[[i]]
    max_r <- max(r)

    # Flip ranks for even groups (descending)
    adjusted_r <- ifelse(parity == 1L, r, max_r + 1L - r)
    sort_keys[[i]] <- adjusted_r

    if (i < nvars) {
      group_id <- (group_id - 1L) * max_r + adjusted_r
    }
  }

  # Combine into single numeric key using base conversion
  # key = r1 * M^(n-1) + r2 * M^(n-2) + ... + rn
  max_ranks <- vapply(sort_keys, max, integer(1))
  M <- max(max_ranks) + 1

  key <- as.numeric(sort_keys[[1]])
  for (i in 2:nvars) {
    key <- key * M + sort_keys[[i]]
  }
  key
}
