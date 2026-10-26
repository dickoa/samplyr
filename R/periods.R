#' Partition a sample into fieldwork periods
#'
#' `periods()` is used inside `execute(panels = )` to spread the selected
#' units over `k` fieldwork periods, such as the months of a yearly survey.
#' Each unit is assigned to exactly one period, certainty units included,
#' and each stratum's units are spread as evenly as the count allows.
#' `execute(sample, wave = t)` returns the sample of period `t` with its
#' weights. For rotation groups, which can be active at several waves, pass
#' a count or a schedule to `panels` instead (see [panel_assignment]).
#'
#' @param k The number of periods, a whole number of at least 2.
#'
#' @return A `samplyr_periods` object, for the `panels` argument of
#'   [execute()].
#'
#' @details
#' ## Periods and rotation groups
#'
#' `panels` takes three forms. A count, `panels = 12`, makes 12 rotation
#' groups whose waves a schedule or [rotation_program()] can declare later.
#' A schedule makes rotation groups and declares their waves. In both, a
#' unit selected with certainty is kept apart from the rotating units and
#' is active at every wave. `panels = periods(12)` makes 12 periods instead.
#' A unit belongs to one period only, whether or not it was selected with
#' certainty, so it is visited once.
#'
#' The assignment is the one [panel_assignment] describes, randomized with
#' fixed quotas within each stratum along the frame order, so every period
#' spreads over the stratum like the whole sample. The periods are recorded
#' in the `.panel` column.
#'
#' ## Equal periods
#'
#' A stratum gets the same number of units in every period when its sample
#' size is a multiple of `k`. Survey programs that publish estimates by
#' period allocate in such multiples, for example 24 EAs per district over
#' 12 months. Otherwise the counts differ by one between periods. A stratum
#' with fewer than `k` units is refused, since some period would hold none
#' of its units and could not estimate it.
#'
#' ## Weights
#'
#' The full-sample `.weight` is valid for the combined sample. A period's
#' own weights come from `execute(sample, wave = t)`, which multiplies
#' `.weight` by the exact inverse of the period's activation chance, the
#' block's quota for that period over the block size. That factor is `k`
#' when a stratum's units fill whole blocks evenly, and it differs from `k`
#' otherwise, so multiplying by `k` by hand is not generally valid.
#' [summary()][summary.tbl_sample] reports how evenly each stratum fills the
#' periods.
#'
#' A period exports to survey as one phase by ultimate clusters, whatever
#' method drew the sample, EAs drawn by size included. [as_svydesign()]
#' describes the design. A period of a sample drawn with equal probabilities
#' also has a two-phase export, the activation as its second phase, with
#' `as_svydesign(method = "full")`.
#'
#' ## Periods in a multistage design
#'
#' Periods assigned at the first stage of a multistage design give the field
#' teams their work plan. `dplyr::filter(sample, .panel == t)` lists the
#' primary units of period `t`, which is what the listing teams need. That
#' table is a work list, not a sample to continue. Its weights describe the
#' whole year without the period's activation factor, and [as_svydesign()]
#' refuses a stage drawn from it (`samplyr_error_modified_sample`).
#'
#' The later stages are drawn from the sample itself, either for one period
#' with that period's listing, `sample <- execute(sample, listing, wave =
#' t)`, or for every period at once, `execute(sample, listing)`. The section
#' "Drawing the later stages wave by wave" of [execute()] states the rules
#' of the first form. `execute(sample, wave = t)` gives a period's
#' sample and weights once its later stages are drawn.
#'
#' @examples
#' # Spread 24 EAs per region over 12 months
#' monthly <- sampling_design() |>
#'   stratify_by(region) |>
#'   draw(n = 24, method = "pps_systematic", mos = households,
#'        control = c(province, commune)) |>
#'   execute(bfa_eas, seed = 1, panels = periods(12))
#'
#' # Every region has two EAs in each month
#' table(monthly$region, monthly$.panel)[1:3, ]
#' summary(monthly)
#'
#' # The sample of the third month, with its own weights
#' march <- execute(monthly, wave = 3)
#' nrow(march)
#'
#' # A unit taken with certainty is assigned to one period like any other
#' poura <- subset(bfa_eas, commune == "Poura" & urban_rural == "Urban")
#' small <- sampling_design() |>
#'   draw(n = 12, method = "pps_systematic", mos = households) |>
#'   execute(poura, seed = 1, panels = periods(12))
#' tabulate(small$.panel, 12)
#'
#' @family helpers
#' @export
periods <- function(k) {
  if (
    !is.numeric(k) || length(k) != 1L || !is_integerish_numeric(k) ||
      k < 2 || k > .Machine$integer.max
  ) {
    abort_samplyr(
      c(
        "{.arg k} must be a single integer >= 2.",
        "i" = "One period is no partition, so there is nothing to spread."
      ),
      class = "samplyr_error_panel_count",
      call = current_env()
    )
  }
  structure(list(k = as.integer(k)), class = "samplyr_periods")
}

#' @export
format.samplyr_periods <- function(x, ...) {
  paste0("<periods: ", x$k, ">")
}

#' @export
print.samplyr_periods <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' Does the panel specification declare periods rather than rotation?
#' @noRd
is_periods_spec <- function(spec) {
  identical(spec$certainty, "assigned")
}
