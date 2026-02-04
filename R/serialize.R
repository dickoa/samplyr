#' Convert Sampling Design to List
#'
#' Converts a sampling design object to a plain list representation,
#' useful for inspection, serialization, or export.
#'
#' @param x A `sampling_design` object
#' @param ... Additional arguments (ignored)
#'
#' @return A list representation of the design containing:
#'   - `title`: The design title (if any)
#'   - `stages`: A list of stage specifications
#'
#' @examples
#' design <- sampling_design(title = "Household Survey") |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 500)
#'
#' as.list(design)
#'
#' @export
as.list.sampling_design <- function(x, ...) {
  result <- list(
    title = x$title
  )

  result$stages <- lapply(x$stages, function(stage) {
    stage_list <- list()

    if (!is_null(stage$label)) {
      stage_list$label <- stage$label
    }

    if (!is_null(stage$strata)) {
      stage_list$strata <- list(
        vars = stage$strata$vars,
        alloc = stage$strata$alloc
      )
      if (!is_null(stage$strata$variance)) {
        stage_list$strata$variance_columns <- names(stage$strata$variance)
      }
      if (!is_null(stage$strata$cost)) {
        stage_list$strata$cost_columns <- names(stage$strata$cost)
      }
    }

    if (!is_null(stage$clusters)) {
      stage_list$clusters <- list(
        vars = stage$clusters$vars
      )
    }

    if (!is_null(stage$draw_spec)) {
      stage_list$draw <- list()
      if (!is_null(stage$draw_spec$n)) {
        if (is.data.frame(stage$draw_spec$n)) {
          stage_list$draw$n <- "custom (data frame)"
          stage_list$draw$n_columns <- names(stage$draw_spec$n)
        } else {
          stage_list$draw$n <- stage$draw_spec$n
        }
      }
      if (!is_null(stage$draw_spec$frac)) {
        if (is.data.frame(stage$draw_spec$frac)) {
          stage_list$draw$frac <- "custom (data frame)"
          stage_list$draw$frac_columns <- names(stage$draw_spec$frac)
        } else {
          stage_list$draw$frac <- stage$draw_spec$frac
        }
      }
      stage_list$draw$method <- stage$draw_spec$method
      if (!is_null(stage$draw_spec$mos)) {
        stage_list$draw$mos <- stage$draw_spec$mos
      }
      if (!is_null(stage$draw_spec$control)) {
        stage_list$draw$control <- vapply(
          stage$draw_spec$control,
          rlang::as_label,
          character(1)
        )
      }
    }
    stage_list
  })
  result
}
