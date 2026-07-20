#' Controlled Count Bounds for Balanced Sampling
#'
#' `bound()` is a declarative marker used inside the `aux` argument of
#' [draw()] with `method = "cube"`. It requests adjacent-integer bounds on
#' the realized sample count for every observed category of `x`.
#'
#' @param x A single categorical frame variable. Use separate `bound()` calls
#'   for separate marginal constraints.
#'
#' @return `bound()` is only meaningful inside `draw(aux = ...)` and otherwise
#'   throws an informative error.
#'
#' @examples
#' sampling_design() |>
#'   draw(
#'     n = 100,
#'     method = "cube",
#'     aux = c(income, bound(region), bound(urban_rural))
#'   )
#'
#' @export
bound <- function(x) {
  cli_abort(
    paste0(
      "{.fn bound} is a declarative marker and must be used inside ",
      "{.code draw(aux = ...)}."
    )
  )
}
