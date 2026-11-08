#' Partition a sample into the subrounds of one period
#'
#' `subrounds()` is used inside `execute(panels = )` to spread the selected
#' units over the `k` subrounds of one reporting period, such as the months
#' of a yearly survey, one round of twelve subrounds in the ILO's terms.
#' Each unit is assigned to exactly one subround, certainty units included,
#' and each stratum's units are spread as evenly as the count allows.
#' `execute(sample, period = 1, subround = s)` returns the sample of
#' subround `s` with its weights, and `execute(sample, period = 1)` the
#' whole period. For rotation groups, which can be active at several
#' occasions, pass a count or a schedule to `panels` instead (see
#' [panel_assignment]).
#'
#' @param k The number of subrounds, a whole number of at least 2.
#'
#' @return A `samplyr_subrounds` object, for the `panels` argument of
#'   [execute()].
#'
#' @details
#' ## Subrounds and rotation groups
#'
#' `panels` takes three forms. A count, `panels = 12`, makes 12 rotation
#' groups whose occasions a schedule or [rotation_program()] can declare
#' later. A schedule makes rotation groups and declares their occasions. In
#' both, a unit selected with certainty is kept apart from the rotating
#' units and is active at every occasion. `panels = subrounds(12)` makes 12
#' subrounds instead. A unit belongs to one subround only, whether or not it
#' was selected with certainty, so it is visited once.
#'
#' The assignment is the one [panel_assignment] describes, randomized with
#' fixed quotas within each stratum along the frame order, so every
#' subround spreads over the stratum like the whole sample. The subrounds
#' are recorded in the `.panel` column.
#'
#' ## Equal subrounds
#'
#' A stratum gets the same number of units in every subround when its
#' sample size is a multiple of `k`. Survey programs that publish estimates
#' by month allocate in such multiples, for example 24 EAs per district over
#' 12 months. Otherwise the counts differ by one between subrounds. A
#' stratum with fewer than `k` units is refused, since some subround would
#' hold none of its units and could not estimate it.
#'
#' ## Weights
#'
#' The full-sample `.weight` is valid for the combined sample, which is
#' what `execute(sample, period = 1)` returns. A subround's own weights come
#' from `execute(sample, period = 1, subround = s)`, which multiplies
#' `.weight` by the exact inverse of the subround's activation chance, the
#' block's quota for that subround over the block size. That factor is `k`
#' when a stratum's units fill whole blocks evenly, and it differs from `k`
#' otherwise, so multiplying by `k` by hand is not generally valid.
#' [summary()][summary.tbl_sample] reports how evenly each stratum fills the
#' subrounds.
#'
#' A subround exports to survey as one phase by ultimate clusters, whatever
#' method drew the sample, EAs drawn by size included. [as_svydesign()]
#' describes the design. A subround of a sample drawn with equal
#' probabilities also has a two-phase export, the activation as its second
#' phase, with `as_svydesign(method = "full")`.
#'
#' ## Subrounds in a multistage design
#'
#' Subrounds assigned at the first stage of a multistage design give the
#' field teams their work plan. `dplyr::filter(sample, .panel == s)` lists
#' the primary units of subround `s`, which is what the listing teams need.
#' That table is a work list, not a sample to continue. Its weights describe
#' the whole period without the subround's activation factor, and
#' [as_svydesign()] refuses a stage drawn from it
#' (`samplyr_error_modified_sample`).
#'
#' The later stages are drawn from the sample itself, either for one
#' subround with that subround's listing,
#' `sample <- execute(sample, listing, period = 1, subround = s)`, or for
#' every subround at once, `execute(sample, listing)`. The section "Drawing
#' the later stages occasion by occasion" of [execute()] states the rules of the
#' first form. `execute(sample, period = 1, subround = s)` gives a
#' subround's sample and weights once its later stages are drawn.
#'
#' @examples
#' # Spread 24 EAs per region over 12 months
#' monthly <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 24, method = "pps_systematic", mos = households,
#'        control = c(province, commune)) |>
#'   execute(bfa_eas, seed = 1, panels = subrounds(12))
#'
#' # Every region has two EAs in each month
#' table(monthly$region, monthly$.panel)[1:3, ]
#' summary(monthly)
#'
#' # The sample of the third month, with its own weights
#' march <- execute(monthly, period = 1, subround = 3)
#' nrow(march)
#'
#' # A unit taken with certainty is assigned to one subround like any other
#' poura <- subset(bfa_eas, commune == "Poura" & urban_rural == "Urban")
#' small <- sampling_design() |>
#'   draw(n = 12, method = "pps_systematic", mos = households) |>
#'   execute(poura, seed = 1, panels = subrounds(12))
#' tabulate(small$.panel, 12)
#'
#' @family helpers
#' @export
subrounds <- function(k) {
  if (
    !is.numeric(k) || length(k) != 1L || !is_integerish_numeric(k) ||
      k < 2 || k > .Machine$integer.max
  ) {
    abort_samplyr(
      c(
        "{.arg k} must be a single integer >= 2.",
        "i" = "One subround is no partition, so there is nothing to spread."
      ),
      class = "samplyr_error_panel_count",
      call = current_env()
    )
  }
  structure(list(k = as.integer(k)), class = "samplyr_subrounds")
}

#' @export
format.samplyr_subrounds <- function(x, ...) {
  paste0("<subrounds: ", x$k, ">")
}

#' @export
print.samplyr_subrounds <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' Does the panel specification declare subrounds rather than rotation?
#' @noRd
is_subrounds_spec <- function(spec) {
  identical(spec$certainty, "assigned")
}
