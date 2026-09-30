#' Create a sampling design
#'
#' `sampling_design()` is the entry point for creating survey sampling
#' specifications. It creates an empty design object that can be built
#' up using pipe-able verbs like [stratify_by()], [cluster_by()],
#' [draw()], and [add_stage()].
#'
#' @param title Optional character string providing a title for the design.
#'   Useful for documentation and printing purposes.
#'
#' @return A `sampling_design` object that can be piped to other design
#'   functions.
#'
#' @details
#' The sampling design paradigm separates the specification of a sampling
#' plan from its execution. This allows designs to be:
#'
#' - Reused across different data frames
#' - Partially executed (e.g., stage by stage)
#' - Inspected and validated before execution
#' - Documented and shared
#'
#' The design specification is frame-independent: it describes *how* to sample,
#' not *what* to sample from.
#'
#' @section Design flow:
#' A design is a sequence of stages, and each stage is written in this order:
#'
#' 1. [stratify_by()] and [cluster_by()], each optional and used at most once,
#'    in either order.
#' 2. [draw()], required, which closes the stage.
#'
#' [add_stage()] then opens the next stage, and [execute()] runs the design.
#' A single-stage design needs no `add_stage()`.
#' ```r
#' sampling_design() |>
#'   stratify_by(...) |>
#'   cluster_by(...) |>
#'   draw(...) |>
#'   execute(frame)
#' ```
#'
#' `draw()` reads the stage's strata and clusters when it is called. They
#' decide which forms of `n`, `frac` and the certainty thresholds are valid,
#' whether `min_n` and `max_n` apply, and how a svyplan plan is taken. Once a stage has its `draw()`, a `stratify_by()`, a `cluster_by()`
#' or a second `draw()` on it is refused with class
#' `samplyr_error_stage_closed`. The same holds for a design from
#' [read_design()] or [get_design()], whose last stage is closed. Extend it
#' with `add_stage()`.
#'
#' @examples
#' # Simple random sample of 100 EAs
#' sampling_design() |>
#'   draw(n = 100) |>
#'   execute(bfa_eas, seed = 1)
#'
#' # Stratified sample with proportional allocation
#' sampling_design(title = "Burkina Faso EA Survey") |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 400) |>
#'   execute(bfa_eas, seed = 2)
#'
#' # Two-stage cluster sample of districts and EAs
#' zwe_frame <- zwe_eas |>
#'   dplyr::mutate(district_hh = sum(households), .by = district)
#'
#' sampling_design(title = "Zimbabwe Household Health Survey") |>
#'   add_stage(label = "Districts") |>
#'     cluster_by(district) |>
#'     draw(n = 20, method = "pps_brewer", mos = district_hh) |>
#'   add_stage(label = "EAs") |>
#'     draw(n = 10) |>
#'   execute(zwe_frame, seed = 3)
#'
#' @seealso
#' [stratify_by()] for defining strata,
#' [cluster_by()] for defining clusters,
#' [draw()] for specifying selection parameters,
#' [add_stage()] for multi-stage designs,
#' [execute()] for running designs
#'
#' @family design specification
#' @export
sampling_design <- function(title = NULL) {
  if (is.data.frame(title)) {
    abort_frame_misplaced("sampling_design")
  }
  if (!is_null(title) && !is_character(title)) {
    cli_abort(
      "{.arg title} must be a character string or NULL",
      class = "samplyr_error_design_argument"
    )
  }

  if (!is_null(title) && length(title) != 1) {
    cli_abort(
      "{.arg title} must be a single string, not a vector of length {length(title)}",
      class = "samplyr_error_design_argument"
    )
  }

  design <- new_sampling_design(
    title = title,
    stages = list(),
    current_stage = 0L,
    validated = FALSE
  )

  design$stages <- list(new_sampling_stage())
  design$current_stage <- 1L

  validate_sampling_design(design)
}

#' Refuse a frame where the design goes
#'
#' A design is built without its frame and meets it in `execute()`. The
#' three first mistakes, `sampling_design(frame)`, `frame |> draw()` and
#' `execute(frame, design)`, are each refused with a message that names it.
#' @noRd
abort_frame_misplaced <- function(verb, design_given = FALSE,
                                  call = caller_env()) {
  what <- if (identical(verb, "sampling_design")) {
    "{.fn sampling_design} takes a title, not a frame."
  } else if (identical(verb, "execute")) {
    "{.fn execute} takes the design first and the frame after it."
  } else {
    "{.fn {verb}} takes a design, not a frame."
  }
  fix <- if (identical(verb, "execute") && design_given) {
    "Swap them: {.code execute(design, frame)}."
  } else {
    "Build the design with {.fn sampling_design} and the verbs, then pass
     the frame to {.fn execute}: {.code sampling_design() |> draw(n = 100)
     |> execute(frame)}."
  }
  abort_samplyr(
    c(what, "i" = fix),
    class = "samplyr_error_frame_misplaced",
    call = call
  )
}
