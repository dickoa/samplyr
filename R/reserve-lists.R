#' Reserve lists for units that fail in the field
#'
#' A reserve list is the form of field substitution that samplyr weights.
#' Each pool, such as the listing of one enumeration area, draws its take
#' plus a reserve, and the list is put in a random order. The office
#' releases the reserve one unit at a time, one for each unit that fails,
#' until the pool has its take of respondents or the list runs out.
#' Because the list and its order are part of the design, the weights of
#' the respondents follow from the record of what the field did.
#'
#' Many national survey programs, the DHS and MICS among them, allow no
#' substitution and adjust for nonresponse in the weights. A reserve list
#' serves a protocol that substitutes, and is not a reason to adopt one.
#'
#' Other forms of substitution change the design in a way no weight
#' records, and samplyr does not support them. These include substitutes
#' matched to the failed unit, substitutes chosen by the interviewer, and
#' substitutes drawn at random only when a unit fails, since nothing then
#' records the pool or the order.
#'
#' Reserve batches are another protocol, which a settlement does not fit.
#' The sample holds random batches that are activated together in every
#' pool when response falls short, and every released unit is worked, as
#' the European Social Survey asked from its tenth round. The weight then
#' comes from the units released, so `response_status` would drop the
#' respondents released after a pool reached its take. Draw the main
#' sample and the batches as one take, and assign it at that stage to
#' panels the size of one batch, so that the main sample spans several
#' panels. A schedule whose occasion `b + 1` activates the main sample and the
#' first `b` batches then gives, through `execute(master, period = b + 1)`,
#' each unit the weight of the units released. [panel_assignment]
#' describes schedules.
#'
#' ## Workflow
#'
#' `draw(n = 8, reserve = 4)` asks each pool for a take of 8 and a list of
#' 12. [execute()] draws the list with the stage's method and adds
#' `.reserve_k`, which is 0 for the take and ranks the reserve units 1 to 4
#' in release order. The weight at that stage is the chance of the whole
#' list. The field works the take, releases reserve units in order, and
#' records a code for every unit it released. `execute(sample,
#' response_status = result)` reads those codes, settles each pool, and
#' returns the respondents with their weights. Only a settled sample can be
#' exported, take a new phase, or go to [share_weights()] or
#' [stack_frames()]. Lists drawn with `execute(reps = )` are settled one
#' replicate at a time, from `dplyr::filter(sample, .replicate == r)`.
#'
#' A parent drawn with replacement, such as an enumeration area selected
#' twice by `"srswr"` or `"pps_multinomial"`, draws a list for each of its
#' draws, told apart by `.draw_k`. Each list is a pool of its own, settled
#' and exported apart, so a household listed under two draws can take a
#' different code under each.
#'
#' A reserve at the first stage, such as reserve enumeration areas for
#' areas that cannot be reached, is settled before the next stage is drawn.
#' Execute it with `stages = 1`, settle it, then continue from the settled
#' sample with `execute(settled, listing)`.
#'
#' ## Field protocol
#'
#' The weights hold only if the field follows the protocol below.
#'
#' - The office holds the reserve and releases units in `.reserve_k` order.
#'   Interviewers see a released unit as an ordinary assignment.
#' - Every failure releases one unit, whether the unit refused, was never
#'   found, or proved ineligible.
#' - Work in a pool stops at its take of respondents or when the list runs
#'   out. An earlier stop is allowed only for a reason that does not depend
#'   on how the pool was going, such as insecurity or the end of the field
#'   period.
#' - Reserve units get the same contact rules as the take. When they get
#'   fewer visits, or interviewers try less because they know a reserve
#'   exists, the estimates carry a bias that no weight removes (Vehovar
#'   1999).
#'
#' ## Response status codes
#'
#' The column named by `response_status` holds the final disposition of
#' each released unit, in the terms of the AAPOR Standard Definitions. It
#' records whether the unit was eligible as well as whether it responded.
#' A code is read by its leading digit, so `2.24` and `"2.24"` are both an
#' eligible non-interview, and four words stand for the four categories.
#'
#' | Code | Word | Meaning |
#' |---|---|---|
#' | 1 | `"interview"` | complete or partial interview |
#' | 2 | `"noninterview"` | eligible, not interviewed |
#' | 3 | `"unknown"` | eligibility never established |
#' | 4 | `"ineligible"` | not eligible, such as a vacant dwelling |
#'
#' `NA` marks a reserve unit that was never released. A survey that treats
#' a partial interview as a break-off codes it 2. How a unit of unknown
#' eligibility is coded chooses its eligibility. Code 3 estimates it from
#' the resolved units of the pool, as AAPOR's proportional allocation does.
#' Code 2 counts it as eligible and code 4 as ineligible. Many field systems
#' record a household where no one was found as a non-contact, which AAPOR
#' keeps for dwellings known to be occupied. Coding such a unit 2 counts it
#' as eligible.
#'
#' ## Weights
#'
#' Settlement walks each pool in release order and stops at the unit that
#' brought the pool to its take. The pool's eligible units are then
#' estimated from the units worked, and the estimate is shared among the
#' respondents. For a pool of `N` units with `k` units worked, `e` of them
#' eligible, `u` of unknown eligibility and `n_r` respondents,
#'
#' ```
#' weight = N * (e - s) / (k - s - u) / n_r
#' ```
#'
#' where `s` is 1 when the pool stopped at its take and 0 otherwise.
#' Leaving out the unit that stopped the pool makes the estimate of
#' the eligible units unbiased over the release order. Giving each
#' respondent the pool's size over its respondents instead overstates the
#' total whenever ineligible units were replaced.
#'
#' A unit released after the stop is dropped, together with its interview,
#' so a pool worked past its take keeps the weight of the stop rule. A pool
#' stopped early with reserve left is settled with the units it worked,
#' which stays unbiased when the stop did not depend on the outcomes. A
#' pool that its take covers lists every unit and is settled from its known
#' count. Under `method = "srswor"` the weighted totals are unbiased. Under
#' `"systematic"` the eligible count stays unbiased, but a total need not be.
#' Its bias depends on the frame order and can be material under an
#' unfavorable one.
#'
#' These properties hold for the pools with a respondent. A pool without
#' one keeps its weight in a placeholder row (see the Export section).
#' When every unit it worked is of unknown eligibility, its eligible count
#' takes the resolved rate of the pools that share its strata, or the pool
#' size when none of them resolved a unit. Both are assumptions about the
#' pool, not consequences of the release order.
#'
#' [summary()] reports the counts per pool, and how many pools ran out,
#' stopped early, released too many or ended with no respondent. It also
#' reports the realized rates under the names of the svyplan arguments, so
#' that one round can plan the next. `eligibility_rate` is the share of
#' worked units found eligible, with the unknowns allocated at the rate of
#' the resolved units. `resp_rate` is the share of eligible units that
#' responded, AAPOR's RR4. Both are ratios of totals over all pools. The
#' line ends with how much the share of worked units that responded varies
#' between pools, as a standard deviation net of each pool's own sampling
#' noise. It is 0 when that noise explains all the spread.
#'
#' ## Export
#'
#' A settled sample is exported by ultimate clusters, since the settled
#' stage's finite population correction is unknown. With the reserve at the
#' first stage, or at the second inside a primary unit taken with
#' certainty, the export adds a row for each unit found ineligible, so the
#' variance treats the eligible count as estimated. A pool with eligible
#' units and no respondent is kept as one placeholder row that carries its
#' weight until a nonresponse adjustment, such as
#' [svrep::redistribute_weights()], or a calibration moves it to
#' respondents. The rows the export adds hold `NA` in the analysis
#' variables, so estimates need `na.rm = TRUE`, and a calibration needs
#' auxiliary variables that are 0 on those rows. The "A settled reserve"
#' section of [as_svydesign()] gives the details.
#'
#' A sample in panels exports as a whole once settled, and its occasions are
#' refused (`samplyr_error_reserve_full_export`). A schedule that opens
#' several cohorts per panel takes no reserve at any stage
#' (`samplyr_error_cohort_stages`), since its master is never complete.
#'
#' ## Planning a reserve
#'
#' Plan the take net of the units the reserve replaces. A svyplan plan
#' whose `eligibility_rate` or `resp_rate` is below 1 inflates the take for
#' the same losses, so the field works until it has more respondents than
#' the plan needed, and [draw()] warns when it gets such a plan with a
#' reserve. For a take of PSUs the rate is `resp_rate_psu`. When the
#' indicator is about persons, `resp_rate` can hold the response rate of
#' persons in responding units, which a reserve of units does not replace.
#' The take is then right and the warning can be ignored.
#'
#' Size the reserve so that a list rarely runs out. With `p` the share of
#' released units that end as respondents, the product of
#' `eligibility_rate` and `resp_rate`, the list length `L` should make the
#' chance of at least `b` respondents among `L` units high. For a take of
#' 12 and `p = 0.63`, a list of 25 reaches the take with a 95% chance, a
#' reserve of 13. A list of 18 runs out in about half the pools.
#' Interviewers still visit about `b / p` units on average, since
#' unreleased reserve units are listed but never visited.
#'
#' A list sized for the mean of `p` runs out more often when `p` varies
#' between pools. At a standard deviation of 0.10 between pools, the list
#' for a take of 12 at `p = 0.63` grows from 25 to 29 units. The spread
#' that [summary()] reports for a settled round gives that standard
#' deviation for the next one. With a few dozen pools it is a rough guide,
#' since it cannot tell a spread of 0.05 from none.
#'
#' When the office or a tablet holds the whole listing and releases units
#' one at a time, `reserve = Inf` lists every unit of each pool in random
#' order. No reserve needs sizing, and a list runs out only when the pool
#' does. The list weight of that stage is then 1.
#'
#' A reserve controls the number of respondents in each pool. It removes
#' no nonresponse bias, since the respondents still differ from the
#' nonrespondents as they would without it (Vehovar 1999, Lynn 2004).
#'
#' @references
#' AAPOR (2016). *Standard Definitions: Final Dispositions of Case Codes and
#' Outcome Rates for Surveys*. 9th edition. American Association for Public
#' Opinion Research.
#'
#' Lynn, P. (2004). The use of substitution in surveys. *The Survey
#' Statistician*, 49, 14-16.
#'
#' Vehovar, V. (1999). Field substitution and unit nonresponse. *Journal of
#' Official Statistics*, 15(2), 335-350.
#'
#' @seealso [draw()] for `reserve`, [execute()] for `response_status`,
#'   [as_svydesign()] for export.
#'
#' @examples
#' # Two enumeration areas per region, then 8 households and a reserve of 4
#' frame <- bfa_eas[bfa_eas$households >= 20, ]
#' design <- sampling_design() |>
#'   add_stage(label = "EA") |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 2, method = "pps_brewer", mos = households) |>
#'   add_stage(label = "Household") |>
#'     draw(n = 8, reserve = 4)
#' ea_sample <- execute(design, frame, stages = 1, seed = 2026)
#' # One row per selected EA, even one drawn more than once
#' eas <- unique(as.data.frame(ea_sample)[c("ea_id", "households")])
#' listing <- data.frame(ea_id = rep(eas$ea_id, eas$households))
#' listing$hh_id <- ave(listing$ea_id, listing$ea_id, FUN = seq_along)
#' set.seed(1)
#' listing$members <- 1 + rpois(nrow(listing), 4)
#' listed <- execute(ea_sample, listing, seed = 2027)
#' table(listed$.reserve_2)
#'
#' # Simulate the field: work the take, then the reserve in order,
#' # until 8 interviews or the end of the list
#' set.seed(3)
#' listed$latent <- sample(
#'   c("interview", "noninterview", "unknown", "ineligible"),
#'   nrow(listed), replace = TRUE, prob = c(0.7, 0.15, 0.05, 0.1)
#' )
#' work <- function(rank, latent) {
#'   o <- order(rank)
#'   hit <- latent[o] == "interview"
#'   out <- latent[o]
#'   out[cumsum(hit) - hit >= 8] <- NA
#'   out[order(o)]
#' }
#' listed <- dplyr::mutate(listed, result = work(.reserve_2, latent),
#'                         .by = ea_id)
#'
#' # Settle each pool from the codes and keep the respondents
#' settled <- execute(listed, response_status = result)
#' summary(settled)
#'
#' if (requireNamespace("survey", quietly = TRUE)) {
#'   svy <- as_svydesign(settled)
#'   survey::svytotal(~members, svy)
#' }
#'
#' @name reserve_lists
NULL
