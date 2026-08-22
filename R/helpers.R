#' Controlled count bounds for balanced sampling
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
#' @family helpers
#' @export
bound <- function(x) {
  abort_marker_misused("bound", "{.code draw(aux = ...)}")
}

#' Namespaces a declarative marker may be written under
#'
#' A marker is read as an expression and never evaluated, so the qualified
#' spelling has to be matched rather than merely working by accident. Only
#' samplyr's own namespace: a marker names an argument of a samplyr verb, so
#' no other package can have supplied it.
#'
#' Deliberately not the rule `control_expr_serializable()` uses, which stays
#' at `ns = ""`. That one reads expressions out of a design *file*, where
#' matching a qualified call would let a file name an arbitrary package.
#' These are expressions a user typed in a live session.
#' @noRd
marker_namespaces <- c("", "samplyr")

#' Refuse a declarative marker called outside the argument it belongs to
#'
#' One class for all five. They are never evaluated in the position that
#' makes them meaningful, so reaching the body always means the same mistake,
#' and it was uncatchable except by message.
#' @noRd
abort_marker_misused <- function(fn, inside, call = caller_env()) {
  abort_samplyr(
    c(
      "{.fn {fn}} is a declarative marker and must be used inside {inside}.",
      "i" = "It is read where it is written and never evaluated, so calling
             it on its own has nothing to describe."
    ),
    class = "samplyr_error_marker_misused",
    call = call
  )
}
