#' Convert a tbl_sample to a survey design object
#'
#' Creates a [survey::svydesign()] object from a `tbl_sample`, using
#' the sampling design metadata (strata, clusters, weights, and
#' finite population corrections) captured during [execute()].
#'
#' @param x A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to [survey::svydesign()].
#'   In particular, you can pass `pps = survey::ppsmat(joint_matrix)`
#'   to supply exact joint inclusion probabilities instead of the
#'   default Brewer approximation (see Details).
#' @param nest If `TRUE`, relabel cluster ids to enforce nesting within
#'   strata. Passed to [survey::svydesign()]. Default is `TRUE`, which
#'   is appropriate for most complex survey designs.
#' @param method For two-phase samples, the variance method passed to
#'   [survey::twophase()]. One of `"full"`, `"approx"`, or `"simple"`.
#'   This argument is only accepted for two-phase samples.
#'
#' @return A `survey.design2` object for single-phase and multistage samples,
#'   or a `twophase`/`twophase2` object for two-phase samples.
#'
#' @details
#' The conversion maps samplyr's design specification to the arguments
#' expected by [survey::svydesign()]:
#'
#' - **Cluster ids** (`ids`): one formula term per executed stage.
#'   Clustered stages use the `cluster_by()` variable; when a stage
#'   clusters by several variables, their combination (which execution
#'   treats as a single cluster id) is collapsed into one synthesized
#'   interaction column, because [survey::svydesign()] reads each
#'   formula term as a separate sampling stage. A final unclustered
#'   stage (elements sampled within the previous stage's clusters) gets
#'   a synthesized row-identity column so that its sampling variance is
#'   represented. For WR/PMR stages, the `.draw_k` column is used as
#'   the sampling unit identifier instead (each draw is treated as an
#'   independent unit for Hansen--Hurwitz variance estimation).
#' - **Strata** (`strata`): one term per stage, aligned with `ids`.
#'   A stage stratified by several variables exports their
#'   cross-classification as a single synthesized interaction column
#'   (survey silently ignores extra variables within a stage's term).
#'   Trailing unstratified stages are omitted; unstratified stages
#'   before a stratified stage get a constant placeholder column.
#' - **Weights** (`weights`): the `.weight` column -- the compound weight
#'   across all stages (i.e., the product of per-stage weights
#'   \eqn{w = \prod w_k = \prod 1/\pi_k}{w = prod(1/pi_k)}).
#'   This is the inverse of the overall inclusion probability and is the
#'   correct weight for design-based point estimation
#'   (\eqn{\hat{Y} = \sum w_i y_i}{Y-hat = sum(w_i * y_i)}).
#' - **FPC** (`fpc`): one term per stage, aligned with `ids`. Because
#'   [survey::svydesign()] requires every FPC term on the same scale,
#'   two encodings are used:
#'   - **Count scale** (designs without unequal-probability WOR
#'     stages): `.fpc_k` (the stratum population count \eqn{N_h}) is
#'     passed for equal-probability WOR stages; a synthetic `Inf`
#'     column (no correction, Hansen--Hurwitz variance) for WR/PMR
#'     stages and for random-size Poisson stages after the first.
#'   - **Fraction scale** (multi-stage designs with a PPS WOR,
#'     balanced, or custom WOR stage): every WOR stage passes its
#'     per-unit stage sampling fraction
#'     \eqn{1 / w_k = \pi_k}{1/w_k = pi_k}; WR/PMR and later Poisson
#'     stages pass 0 (no correction).
#'   A single-stage PPS WOR design passes \eqn{\pi_i}{pi_i} directly,
#'   which survey interprets as inclusion probabilities.
#'
#' ## Multi-stage designs
#'
#' Every executed sampling stage is represented in the exported design:
#' one `ids` term, one `fpc` term, and (when stratified) one `strata`
#' term per stage, so [survey::svydesign()] performs exact multi-stage
#' linearization (Sarndal et al. 1992, ch. 4.3). In particular, a
#' design whose first stage is a census of PSUs correctly attributes
#' all variance to the later stages.
#'
#' Operational execution does not change this classification. For example,
#' `stage1 <- execute(design, psu_frame, stages = 1)` followed by
#' `sample <- execute(stage1, listing_frame)` remains one multistage design.
#' The partial `tbl_sample` stores the same design plus the realized PSU
#' selection; the final sample records all executed stages and
#' `as_svydesign()` calls [survey::svydesign()], not [survey::twophase()].
#'
#' A two-phase sample has a different provenance: a *new* phase-2
#' `sampling_design` is executed with the phase-1 `tbl_sample` as its frame,
#' for example `phase2 <- execute(design2, phase1)`. That execution records a
#' previous-phase link, and `as_svydesign()` calls [survey::twophase()].
#'
#' One shape cannot be represented: an unclustered element-sampling
#' stage *followed by further stages* is not nested cluster sampling
#' (the later selections are conditional on the realized element
#' sample, i.e. phase sampling), and `as_svydesign()` raises an error.
#' Express such designs as two-phase samples instead: execute the element
#' stage under its first-phase design, then execute a new second-phase design
#' with that sample as its frame. This exports via [survey::twophase()].
#'
#' Concretely, for a two-stage stratified-cluster design with a final
#' element stage, the exported call is equivalent to:
#' \preformatted{
#' survey::svydesign(
#'   ids     = ~ ea_id + .id_2,       # stage-1 clusters, stage-2 elements
#'   strata  = ~ region,              # stage-1 strata
#'   weights = ~ .weight,             # product of per-stage weights
#'   fpc     = ~ .fpc_pi_1 + .fpc_f_2,  # per-stage sampling fractions
#'   data    = sample,
#'   nest    = TRUE
#' )
#' }
#'
#' ## Modified samples and domain analysis
#'
#' The conversion requires a sample whose rows still match the executed
#' design. A `tbl_sample` whose row set was changed after [execute()]
#' (rows removed by [dplyr::filter()] or `[`, added, or duplicated by a
#' join) or whose internal design columns (`.weight`, `.weight_k`,
#' `.fpc_k`, ...) were overwritten, dropped, or renamed is marked as
#' modified, and `as_svydesign()` raises an error. The check is
#' authoritative, not just mark-based: the sample is verified against
#' an integrity record (row count and a hash of the weights, design
#' metadata, and strata/cluster columns) stored at execution, so
#' modifications through routes the dplyr hooks cannot see (base
#' assignment, `rbind()`, vctrs operations, third-party verbs) are
#' also caught, and an overwrite that left every value identical
#' passes. Physically dropping out-of-domain
#' rows before conversion is not equivalent to domain estimation: the
#' point estimate agrees, but the variance is understated because the
#' domain sample size is random under the design.
#'
#' For subpopulation estimates, convert the full sample first and then
#' subset the design, which applies the proper domain estimator:
#' \preformatted{
#' svy <- as_svydesign(sample)
#' survey::svymean(~y, subset(svy, domain))
#' # or with srvyr:
#' as_survey_design(sample) |> filter(domain) |> summarise(...)
#' }
#'
#' Row reordering, one-to-one joins, and adding ordinary data columns
#' do not mark the sample. Extracting one complete replicate from a
#' replicated execution (`filter(.replicate == r)`) is verified against
#' the execution metadata and remains supported.
#'
#' ## Equal-probability systematic sampling
#'
#' `systematic` stages are exported with the SRSWOR variance estimator,
#' the standard approximation for systematic sampling. Depending on the
#' frame ordering (see the `control` argument of [draw()]), the true
#' variance can be smaller (favorable ordering) or larger (periodic
#' ordering) than this estimate.
#'
#' ## Variance estimation for PPS designs
#'
#' For fixed-size PPS without-replacement stages (`pps_brewer`,
#' `pps_systematic`, `pps_cps`, `pps_sampford`, `pps_sps`, `pps_pareto`),
#' variance is estimated by default using Brewer's approximation (`pps =
#' "brewer"` in survey's terminology), which approximates the joint inclusion
#' probabilities from the marginal inclusion probabilities. Here Brewer names
#' the variance estimator, not the selection algorithm: Sampford selection,
#' for example, receives this default treatment. This is the approximation
#' described by Berger (2004) and works well for most PPS designs regardless
#' of the sampling algorithm used.
#'
#' For supported methods, you can instead compute joint inclusion
#' probabilities using [joint_expectation()] and pass them via `pps =
#' survey::ppsmat(joint_matrix)`. The matrix is exact for CPS, Sampford,
#' systematic PPS, and Poisson selection; generalized Brewer, SPS, Pareto, and
#' unconstrained cube use the documented high-entropy approximation.
#'
#' ## Spatial and constrained balanced methods
#'
#' Bounded cube, LPM2, and SCPS alter pairwise selection behavior beyond the
#' available linearization approximation. [as_svydesign()] therefore refuses
#' these designs, and [joint_expectation()] does not provide a matrix for them.
#' Use `as_svrepdesign(type = "subbootstrap")` or `"mrbbootstrap"` for a
#' generic PPS bootstrap approximation. These replicates do not recreate the
#' count constraints or spatial algorithm and are not an exact,
#' design-specific variance estimator.
#'
#' ## Random-size Poisson methods
#'
#' Methods `bernoulli` and `pps_poisson` select units independently
#' with known marginal inclusion probabilities, so the realized
#' sample size is random. The standard SRSWOR variance estimator
#' is not appropriate, and Brewer's approximation (designed for
#' fixed-size PPS) understates the variance. Instead, these
#' methods are exported with `pps = survey::poisson_sampling(pi)`,
#' which produces the Horvitz-Thompson Poisson variance estimator
#' \eqn{\hat V = \sum_{i \in S} (1 - \pi_i) / \pi_i^2 \cdot y_i^2}
#' described in Sarndal, Swensson and Wretman (1992), section 2.8.
#'
#' This applies under the following conditions.
#'
#' - Single-stage designs (no `cluster_by()`, or `cluster_by()` with
#'   one row per sampled cluster) are exported with `poisson_sampling()`
#'   and produce the exact Horvitz-Thompson Poisson variance.
#' - Multi-stage designs with a random-size Poisson method at stage k > 1
#'   omit the finite-population correction at the Poisson stage (the same
#'   handling used for with-replacement methods). The Poisson stage is
#'   treated as sampled with replacement, which is mildly conservative.
#' - Multi-stage designs with a random-size Poisson method at stage 1
#'   are not supported by `survey::svydesign()`, which rejects multi-stage
#'   designs when the `pps` argument is set. Such designs raise an error
#'   suggesting `as_svrepdesign(type = "subbootstrap")`.
#' - Single-stage designs that use `cluster_by()` with multiple rows per
#'   sampled cluster (for example a household listing within sampled EAs)
#'   raise an error. `survey::poisson_sampling()` treats rows as
#'   independent and does not honor within-cluster correlation. Use
#'   `as_svrepdesign(type = "subbootstrap")` for these designs.
#' - Custom methods registered with `fixed_size = FALSE`
#'   (`sondage::register_method()`) are also random-size, but samplyr
#'   cannot verify that their selections are independent across units,
#'   which the Poisson estimator requires. The method author can settle
#'   this at registration: a method registered with
#'   `variance_family = "poisson"` asserts independent selections and is
#'   exported through `poisson_sampling()` exactly like the built-ins
#'   above. Undeclared methods raise an error; if you know the method is
#'   Poisson-type, pass the probabilities explicitly:
#'   `as_svydesign(x, pps = survey::poisson_sampling(1 / x$.weight))`,
#'   or use `as_svrepdesign(type = "subbootstrap")`.
#'
#' ## Declared variance families for custom methods
#'
#' `sondage::register_method()` accepts a `variance_family` declaration
#' (`"srs"`, `"pps_brewer"`, `"poisson"`, `"wr"`, `"unsupported"`).
#' When present it overrides the classification samplyr would otherwise
#' infer from the method's `type` and `fixed_size`: `"srs"` receives the
#' equal-probability treatment (count-scale FPC), `"pps_brewer"` the
#' fixed-size PPS treatment (Brewer approximation), `"poisson"` exact
#' Poisson linearization, and `"wr"` the with-replacement treatment.
#' A method declared `"unsupported"` cannot be linearized at all:
#' `as_svydesign()` refuses with an error and
#' `as_svrepdesign(type = "subbootstrap")` remains the escape hatch.
#'
#' ## Chromy's sequential PPS method (PMR)
#'
#' `pps_chromy` is classified as a *Probability Minimum Replacement*
#' (PMR) method -- neither with-replacement nor without-replacement.
#' Each unit receives exactly \eqn{\lfloor E(n_i) \rfloor} or
#' \eqn{\lfloor E(n_i) \rfloor + 1} hits, where
#' \eqn{E(n_i) = n \cdot \textrm{mos}_i / \sum \textrm{mos}}.
#' When all expected hit counts are below 1, this reduces to WOR;
#' otherwise large units receive multiple hits.
#'
#' For variance estimation, Chromy (2009) recommends the
#' Hansen-Hurwitz (with-replacement) approximation rather than
#' exact pairwise expectations, which he found "quite variable."
#' Chauvet (2019) confirmed this in simulation. Accordingly,
#' `as_svydesign()` treats `pps_chromy` stages like
#' with-replacement stages (no FPC, no pps argument).
#'
#' Note that `survey::ppsmat()` is **not** valid for the general
#' PMR case. The survey package reads \eqn{\pi_i} from the diagonal
#' of the joint matrix, but for PMR the diagonal contains
#' \eqn{E(n_i^2)}, which differs from \eqn{E(n_i)} when units
#' receive multiple hits. The generalized Sen-Yates-Grundy variance
#' requires \eqn{E(n_i) E(n_j) - E(n_i n_j)} as the pairwise
#' weight (Chromy 2009, eq. 5), not \eqn{E(n_i^2) E(n_j^2) - E(n_i n_j)}.
#'
#' ## Certainty stratum (take-all units)
#'
#' For PPS without-replacement stages that use certainty selection
#' (`certainty_size` or `certainty_prop`), units with inclusion
#' probability \eqn{\pi_i = 1}{pi_i = 1} are placed in a separate
#' take-all stratum. This follows the standard practice from
#' Cochran (1977, ch. 11) and Sarndal et al. (1992, ch. 3.5):
#' the take-all stratum contributes zero variance (it is a census)
#' and does not inflate the degrees of freedom for the probability
#' stratum.
#'
#' For stages using with-replacement methods (`srswr`,
#' `pps_multinomial`), the finite population correction is omitted
#' and the `.draw_k` column (sequential draw index) is used as the
#' sampling unit identifier for Hansen-Hurwitz variance estimation.
#'
#' The `survey` package is required but not imported -- it must be
#' installed to use this function.
#'
#' @references
#' Berger, Y.G. (2004). A Simple Variance Estimator for Unequal
#' Probability Sampling Without Replacement. *Journal of Applied
#' Statistics*, 31, 305-315.
#'
#' Brewer, K.R.W. (2002). *Combined Survey Sampling Inference
#' (Weighing Basu's Elephants)*. Chapter 9.
#'
#' Chauvet, G. (2019). Properties of Chromy's sampling procedure.
#' *arXiv:1912.10896*.
#'
#' Chromy, J.R. (2009). Some Generalizations of the Horvitz-Thompson
#' Estimator. *JSM Proceedings, Survey Research Methods Section*.
#'
#' Cochran, W.G. (1977). *Sampling Techniques*. 3rd edition. Wiley.
#'
#' Sarndal, C.-E., Swensson, B. and Wretman, J. (1992). *Model
#' Assisted Survey Sampling*. Springer.
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
#' # Stratified sample -> survey design
#' sample <- sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 42)
#'
#' svy <- as_svydesign(sample)
#' survey::svymean(~households, svy)
#'
#' # Two-stage cluster sample with PPS first stage
#' sample <- sampling_design() |>
#'   add_stage() |>
#'     stratify_by(region) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 5, method = "pps_brewer", mos = households) |>
#'   add_stage() |>
#'     draw(n = 12) |>
#'   execute(bfa_eas, seed = 2025)
#'
#' # Default: Brewer variance approximation
#' svy <- as_svydesign(sample)
#'
#' # Exact: compute joint probabilities from frame
#' jip <- joint_expectation(sample, bfa_eas, stage = 1)
#' svy_exact <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
#'
#' @seealso [execute()] for producing tbl_sample objects,
#'   [survey::svydesign()] for the underlying function,
#'   [as_survey_design.tbl_sample] for converting directly to a srvyr `tbl_svy`,
#'   `as_svrepdesign()` for replicate-weight export
#'
#' @export
as_svydesign <- function(x, ...) {
  UseMethod("as_svydesign")
}

#' @noRd
survey_phase_info <- function(sample) {
  metadata <- attr(sample, "metadata")
  prev_phase <- metadata$prev_phase
  has_prev_sample <- is.list(prev_phase) && is_tbl_sample(prev_phase$sample)
  prev_prev <- if (has_prev_sample) {
    attr(prev_phase$sample, "metadata")
  } else {
    NULL
  }
  has_three_phase <- has_prev_sample &&
    is.list(prev_prev) &&
    !is_null(prev_prev$prev_phase)
  is_twophase <- has_prev_sample && !has_three_phase

  list(
    prev_phase = prev_phase,
    has_prev_sample = has_prev_sample,
    has_three_phase = has_three_phase,
    is_twophase = is_twophase
  )
}

#' @noRd
survey_validate_phase_support <- function(
  sample,
  allow_twophase = TRUE,
  fn_name = "as_svydesign",
  call = rlang::caller_env()
) {
  phase_info <- survey_phase_info(sample)

  if (phase_info$has_three_phase) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} only supports up to two-phase samples.",
        "i" = "This sample has more than two phases.",
        "i" = "Convert phases separately or collapse phases before exporting."
      ),
      class = "samplyr_error_survey_multiphase_unsupported",
      call = call
    )
  }

  if (!allow_twophase && phase_info$is_twophase) {
    abort_samplyr(
      c(
        "{.fn {fn_name}} does not support two-phase samples.",
        "i" = "Use {.fn as_svydesign} for two-phase linearization export."
      ),
      class = "samplyr_error_svrep_twophase_unsupported",
      call = call
    )
  }

  phase_info
}

#' Classify a stage's selection method for variance export.
#'
#' Single source of truth for the method-combination matrix: every
#' export decision (FPC encoding, pps argument, replicate-type warning)
#' derives from this kind rather than re-testing method constants.
#'
#' - "wr": with-replacement or PMR selection. Hansen-Hurwitz variance,
#'   no finite-population correction.
#' - "rs_poisson": random-size independent-selection WOR (bernoulli,
#'   pps_poisson, or a custom WOR method registered with
#'   fixed_size = FALSE). Poisson linearization at stage 1 (built-ins
#'   only; see survey_resolve_pps), WR treatment at later stages.
#' - "pps_wor": fixed-size unequal-probability WOR (PPS methods,
#'   balanced, custom fixed-size WOR). Brewer approximation.
#' - "equal_wor": equal-probability fixed-size WOR (srswor,
#'   systematic). Count-scale FPC, SRS-style variance.
#' - "unsupported": the method declares that no linearization
#'   treatment is valid (variance_family = "unsupported").
#'   survey_resolve_pps() refuses as_svydesign() and points to
#'   replicate methods; the bootstrap escape demotes its FPC.
#' @noRd
survey_stage_kind <- function(draw_spec) {
  # Built-in controlled and spatially balanced designs do not have a valid
  # linearization family in v1. Keep this classification here, alongside all
  # other built-in method rules; method_variance remains metadata supplied by
  # registered methods.
  if (
    !is_null(draw_spec$bounds) ||
      draw_spec$method %in% spatial_balanced_methods
  ) {
    return("unsupported")
  }

  # A variance family declared at registration (sondage
  # register_method(variance_family = )) overrides inference from
  # type/fixed: the method author knows the estimator, samplyr can
  # only guess. Unknown values (a newer sondage) fall through to the
  # inference below.
  if (!is_null(draw_spec$method_variance)) {
    kind <- switch(
      draw_spec$method_variance,
      srs = "equal_wor",
      pps_brewer = "pps_wor",
      poisson = "rs_poisson",
      wr = "wr",
      unsupported = "unsupported",
      NULL
    )
    if (!is_null(kind)) {
      return(kind)
    }
  }
  method <- draw_spec$method
  if (
    method %in%
      c(wr_methods, pmr_methods) ||
      identical(draw_spec$method_type, "wr")
  ) {
    return("wr")
  }
  if (method %in% rs_poisson_methods) {
    return("rs_poisson")
  }
  if (identical(draw_spec$method_type, "wor")) {
    # Custom WOR methods: fixed-size follows the Brewer (PPS-WOR)
    # strategy; random-size follows the Poisson strategy.
    if (identical(draw_spec$method_fixed, FALSE)) {
      return("rs_poisson")
    }
    return("pps_wor")
  }
  if (identical(draw_spec$method_type, "balanced")) {
    # Custom balanced methods follow the built-in balanced (cube)
    # strategy. Without this branch they would fall through to
    # "equal_wor" (the method field holds the custom name, so the
    # name test below never matches) and export with SRS variance.
    return("pps_wor")
  }
  if (method %in% pps_wor_methods || method %in% balanced_methods) {
    return("pps_wor")
  }
  "equal_wor"
}

#' Flattened unit-identifier variables across executed stages.
#'
#' Cluster variables and .draw_k columns, in stage order. Used only to
#' find shared unit identifiers between the phases of a two-phase
#' sample; formula construction uses survey_id_info().
#' @noRd
survey_key_vars <- function(design, stages_executed, df) {
  vars <- character(0)
  for (stage_idx in stages_executed) {
    stage_spec <- design$stages[[stage_idx]]
    draw_col <- paste0(".draw_", stage_idx)

    if (is_multi_hit_method(stage_spec$draw_spec) && draw_col %in% names(df)) {
      vars <- c(vars, draw_col)
    } else if (!is_null(stage_spec$clusters)) {
      vars <- c(vars, stage_spec$clusters$vars)
    }
  }
  vars
}

#' Per-stage survey sampling-unit identifiers.
#'
#' Builds exactly one id term per represented executed stage, in
#' execution order. survey::svydesign() reads each term of the `ids`
#' formula as one sampling stage, so the number of terms must match the
#' number of represented stages:
#'
#' - Multi-hit (WR/PMR) stages use the .draw_k column: one row per
#'   draw, each draw an independent unit.
#' - Clustered stages use the cluster variable directly, or a
#'   synthesized interaction column when cluster_by() has several
#'   variables. Execution treats the combination as a single-stage
#'   cluster id; listing the variables as separate formula terms would
#'   make survey read them as extra sampling stages.
#' - Unclustered WOR stages are element-sampling stages. The final
#'   executed stage gets a synthesized row-identity column so its
#'   sampling variance is represented (a single-stage design keeps
#'   ids = ~1, which survey treats identically). An unclustered
#'   element stage followed by further stages cannot be expressed as
#'   nested cluster sampling and aborts.
#'
#' `synthesize_unclustered = FALSE` keeps the legacy behavior of
#' skipping unclustered WOR stages. It is used by the two-phase path,
#' where survey::twophase() handles between-phase subsampling variance
#' itself.
#'
#' `prefix` disambiguates synthesized column names when two designs
#' share one data frame (two-phase export).
#' @noRd
survey_id_info <- function(
  design,
  stages_executed,
  df,
  synthesize_unclustered = TRUE,
  prefix = "",
  call = rlang::caller_env()
) {
  id_vars <- character(0)
  stage_indices <- integer(0)
  n_exec <- length(stages_executed)

  for (pos in seq_len(n_exec)) {
    stage_idx <- stages_executed[pos]
    stage_spec <- design$stages[[stage_idx]]
    draw_col <- paste0(".draw_", stage_idx)

    if (is_multi_hit_method(stage_spec$draw_spec) && draw_col %in% names(df)) {
      id_vars <- c(id_vars, draw_col)
      stage_indices <- c(stage_indices, stage_idx)
    } else if (!is_null(stage_spec$clusters)) {
      cluster_vars <- stage_spec$clusters$vars
      if (length(cluster_vars) == 1L) {
        id_var <- cluster_vars
      } else {
        id_var <- paste0(".", prefix, "id_", stage_idx)
        df[[id_var]] <- group_ids(df, cluster_vars)
      }
      id_vars <- c(id_vars, id_var)
      stage_indices <- c(stage_indices, stage_idx)
    } else if (synthesize_unclustered && n_exec > 1L) {
      if (pos < n_exec) {
        abort_samplyr(
          c(
            "Cannot export stage {stage_idx} to {.fn survey::svydesign}:
             an unclustered element-sampling stage followed by later
             stages cannot be expressed as nested cluster sampling.",
            "i" = "If stage {stage_idx} selects whole clusters, declare
                   them with {.fn cluster_by}.",
            "i" = "If it selects elements, this is phase sampling:
                   execute stages 1-{stage_idx} as phase 1, then run the
                   remaining stages as a {.emph separate design} on that
                   result (not a continuation of this one).
                   {.fn as_svydesign} then exports via
                   {.fn survey::twophase}.",
            "i" = "Declare a shared unit identifier with
                   {.fn cluster_by} in both phase designs so the phases
                   can be linked at export."
          ),
          class = "samplyr_error_survey_midstage_element",
          call = call
        )
      }
      id_var <- paste0(".", prefix, "id_", stage_idx)
      df[[id_var]] <- seq_len(nrow(df))
      id_vars <- c(id_vars, id_var)
      stage_indices <- c(stage_indices, stage_idx)
    }
    # Single executed unclustered stage: ids = ~1 (element sampling).
  }

  list(df = df, id_vars = id_vars, stage_indices = stage_indices)
}

#' @noRd
survey_ids_formula <- function(id_vars) {
  if (length(id_vars) == 0) {
    rlang::new_formula(NULL, 1)
  } else {
    survey_formula_from_vars(id_vars)
  }
}

#' Build a one-sided formula without parsing column names as code
#' @noRd
survey_formula_from_vars <- function(vars) {
  if (length(vars) == 0L) {
    cli_abort("A survey formula requires at least one variable.", call = NULL)
  }
  terms <- lapply(vars, rlang::sym)
  rhs <- Reduce(function(x, y) call("+", x, y), terms)
  rlang::new_formula(NULL, rhs)
}

#' Per-stage survey strata terms.
#'
#' survey::svydesign() reads each term of the `strata` formula as the
#' strata for the corresponding sampling stage; extra variables in one
#' stage's term are silently ignored. Each stage therefore contributes
#' at most ONE term: a single stratification variable is used directly,
#' while several variables (or a certainty stratum combined with user
#' strata) are collapsed into a synthesized interaction column.
#'
#' In "multistage" mode, terms are positionally aligned with the ids
#' formula (id_stage_indices). Unstratified stages between stratified
#' ones get a constant placeholder column (a single stratum);
#' trailing unstratified stages are dropped, which survey pads as
#' unstratified.
#'
#' In "first_stage" mode (two-phase export), only the first executed
#' stage's strata are used, matching survey::twophase() expectations.
#'
#' The certainty stratum applies to the first executed stage: WOR
#' take-all units form a separate stratum that contributes zero
#' variance.
#' @noRd
survey_strata_info <- function(
  df,
  design,
  stages_executed,
  id_stage_indices = integer(0),
  mode = c("multistage", "first_stage"),
  prefix = ""
) {
  mode <- match.arg(mode)
  first_stage_idx <- stages_executed[1]

  cert_var <- NULL
  first_draw_spec <- design$stages[[first_stage_idx]]$draw_spec
  first_method <- first_draw_spec$method
  cert_col <- paste0(".certainty_", first_stage_idx)
  if (
    (
      first_method %in% pps_wor_methods ||
        identical(first_draw_spec$method_type, "wor")
    ) &&
      cert_col %in% names(df) &&
      any(df[[cert_col]])
  ) {
    cert_var <- paste0(".", prefix, "cert_stratum")
    df[[cert_var]] <- ifelse(
      df[[cert_col]],
      "certainty",
      "probability"
    )
  }

  term_stages <- if (mode == "first_stage" || length(id_stage_indices) == 0) {
    first_stage_idx
  } else {
    id_stage_indices
  }

  terms <- rep(NA_character_, length(term_stages))
  for (i in seq_along(term_stages)) {
    stage_idx <- term_stages[i]
    stage_spec <- design$stages[[stage_idx]]
    vars <- if (!is_null(stage_spec$strata)) {
      stage_spec$strata$vars
    } else {
      character(0)
    }
    if (identical(stage_idx, first_stage_idx)) {
      vars <- c(vars, cert_var)
    }
    if (length(vars) == 0) {
      next
    }
    if (length(vars) == 1) {
      terms[i] <- vars
    } else {
      combined <- paste0(".", prefix, "strata_", stage_idx)
      df[[combined]] <- group_ids(df, vars)
      terms[i] <- combined
    }
  }

  # Trailing unstratified stages are dropped from the formula; interior
  # gaps get a single-stratum placeholder to keep terms aligned with
  # the ids formula.
  last_stratified <- max(c(0L, which(!is.na(terms))))
  terms <- terms[seq_len(last_stratified)]
  for (i in seq_along(terms)) {
    if (is.na(terms[i])) {
      placeholder <- paste0(".", prefix, "strata_all_", term_stages[i])
      df[[placeholder]] <- "all"
      terms[i] <- placeholder
    }
  }

  strata_formula <- if (length(terms) == 0) {
    NULL
  } else {
    survey_formula_from_vars(terms)
  }

  list(
    df = df,
    formula = strata_formula,
    vars = terms
  )
}

#' Per-stage FPC terms.
#'
#' Builds exactly one fpc term per represented stage, positionally
#' aligned with the ids formula. Two encodings are used, because
#' survey::svydesign() requires every fpc term on the same scale
#' (all population counts >= 1, or all sampling fractions <= 1):
#'
#' - Count scale (default): the .fpc_k population count for
#'   equal-probability WOR stages, Inf (no correction) for WR/PMR
#'   stages and for random-size Poisson stages after the first.
#' - Fraction scale: used whenever a stage passes per-unit inclusion
#'   probabilities (PPS WOR, balanced, custom WOR, first-stage
#'   random-size Poisson) in a design with more than one represented
#'   stage. Each WOR stage passes its per-unit stage sampling
#'   fraction 1/.weight_k (equal to pi_k); WR/PMR and later Poisson
#'   stages pass 0 (no correction).
#'
#' A single represented pi-scale stage keeps the legacy "pi" encoding
#' (one .fpc_pi_k term).
#' @noRd
survey_fpc_info <- function(df, design, stages_executed, id_stage_indices) {
  fpc_stage_indices <- if (length(id_stage_indices) == 0) {
    stages_executed[1]
  } else {
    id_stage_indices
  }

  first_executed <- stages_executed[1]

  stage_kind <- vapply(
    fpc_stage_indices,
    function(stage_idx) {
      kind <- survey_stage_kind(design$stages[[stage_idx]]$draw_spec)
      # "unsupported" gets the same FPC encoding as rs_poisson (pi at
      # stage 1, no correction later): survey_resolve_pps() errors on
      # it before the FPC is used, or demotes it for the bootstrap.
      if (kind %in% c("rs_poisson", "unsupported")) {
        if (identical(stage_idx, first_executed)) {
          paste0(kind, "_first")
        } else {
          paste0(kind, "_later")
        }
      } else {
        kind
      }
    },
    character(1)
  )

  has_pps_wor <- any(stage_kind == "pps_wor")
  has_rs_poisson_stage1 <- any(stage_kind == "rs_poisson_first")

  needs_pi <- has_pps_wor ||
    has_rs_poisson_stage1 ||
    any(stage_kind == "unsupported_first")
  scale <- if (needs_pi && length(fpc_stage_indices) > 1L) {
    "fraction"
  } else if (needs_pi) {
    "pi"
  } else {
    "count"
  }

  fpc_vars <- character(0)
  for (i in seq_along(fpc_stage_indices)) {
    stage_idx <- fpc_stage_indices[i]
    kind <- stage_kind[i]
    weight_col <- paste0(".weight_", stage_idx)
    fpc_col <- paste0(".fpc_", stage_idx)

    if (kind %in% c("wr", "rs_poisson_later", "unsupported_later")) {
      if (scale == "fraction") {
        f0_col <- paste0(".fpc_f0_", stage_idx)
        df[[f0_col]] <- 0
        fpc_vars <- c(fpc_vars, f0_col)
      } else {
        inf_col <- paste0(".fpc_inf_", stage_idx)
        df[[inf_col]] <- Inf
        fpc_vars <- c(fpc_vars, inf_col)
      }
      next
    }

    if (kind %in% c("pps_wor", "rs_poisson_first", "unsupported_first")) {
      fpc_pi_col <- paste0(".fpc_pi_", stage_idx)
      df[[fpc_pi_col]] <- 1 / df[[weight_col]]
      fpc_vars <- c(fpc_vars, fpc_pi_col)
      next
    }

    # Equal-probability WOR.
    if (scale == "fraction") {
      f_col <- paste0(".fpc_f_", stage_idx)
      df[[f_col]] <- 1 / df[[weight_col]]
      fpc_vars <- c(fpc_vars, f_col)
    } else if (fpc_col %in% names(df)) {
      fpc_vars <- c(fpc_vars, fpc_col)
    } else {
      # No population count available for this stage. Fall back to no
      # correction rather than dropping the term, which would misalign
      # the remaining fpc terms with the ids formula.
      inf_col <- paste0(".fpc_inf_", stage_idx)
      df[[inf_col]] <- Inf
      fpc_vars <- c(fpc_vars, inf_col)
    }
  }

  fpc_formula <- if (length(fpc_vars) == 0) {
    NULL
  } else {
    survey_formula_from_vars(fpc_vars)
  }

  list(
    df = df,
    formula = fpc_formula,
    fpc_vars = fpc_vars,
    scale = scale,
    has_pps_wor = has_pps_wor,
    has_rs_poisson_stage1 = has_rs_poisson_stage1
  )
}

#' Demote a stage-1 random-size Poisson FPC to Inf.
#'
#' Used by the bootstrap escape hatch when survey::svydesign() cannot
#' represent exact multi-stage Poisson linearization. The demoted design
#' carries no finite-population correction at the Poisson stage; the
#' bootstrap resampler supplies the variance instead.
#' @noRd
survey_demote_rs_poisson_stage1 <- function(df, fpc, first_idx) {
  pi_col <- paste0(".fpc_pi_", first_idx)
  # On the fraction scale, "no correction" is a sampling fraction of 0;
  # on the count/pi scales it is an infinite population.
  if (identical(fpc$scale, "fraction")) {
    inf_col <- paste0(".fpc_f0_", first_idx)
    df[[inf_col]] <- 0
  } else {
    inf_col <- paste0(".fpc_inf_", first_idx)
    df[[inf_col]] <- Inf
  }
  df[[pi_col]] <- NULL
  fpc$fpc_vars <- ifelse(fpc$fpc_vars == pi_col, inf_col, fpc$fpc_vars)
  fpc$formula <- if (length(fpc$fpc_vars) == 0) {
    NULL
  } else {
    survey_formula_from_vars(fpc$fpc_vars)
  }
  fpc$has_rs_poisson_stage1 <- FALSE
  list(df = df, fpc = fpc)
}

#' Resolve the pps argument for as_svydesign.
#'
#' Single-phase only. Encapsulates the case split documented in
#' as_svydesign(): exact poisson_sampling() at single-stage, error or
#' bootstrap relaxation for multi-stage stage-1 Poisson, error or
#' bootstrap relaxation for clustered single-stage Poisson with multiple
#' rows per cluster, Brewer for fixed-size PPS WOR, FALSE otherwise.
#'
#' Returns a list with the resolved `pps` argument plus possibly modified
#' `df` and `fpc` (when the bootstrap relaxation rewrites a stage-1
#' Poisson FPC to Inf).
#' @noRd
survey_resolve_pps <- function(
  df,
  design,
  stages_executed,
  fpc,
  user_pps = NULL,
  relax_pps_for_bootstrap = FALSE
) {
  if (!is_null(user_pps)) {
    return(list(pps = user_pps, df = df, fpc = fpc))
  }

  # Stages whose method declares variance_family = "unsupported": no
  # linearization treatment is valid, whatever the selection metadata
  # looks like. as_svydesign() refuses; the bootstrap escape demotes a
  # stage-1 pi FPC (later-stage unsupported FPCs are already Inf/0).
  unsupported_idx <- stages_executed[vapply(
    stages_executed,
    function(i) {
      identical(survey_stage_kind(design$stages[[i]]$draw_spec), "unsupported")
    },
    logical(1)
  )]
  if (length(unsupported_idx) > 0) {
    if (!relax_pps_for_bootstrap) {
      unsupported_methods <- vapply(
        unsupported_idx,
        function(i) design$stages[[i]]$draw_spec$method,
        character(1)
      )
      abort_samplyr(
        c(
          "Cannot export method{?s} {.val {unsupported_methods}} via {.fn as_svydesign}.",
          "i" = "No linearization variance estimator is available for this method and its declared constraints.",
          "i" = "Use {.code as_svrepdesign(type = \"subbootstrap\")} for a bootstrap approximation."
        ),
        class = "samplyr_error_custom_random_wor_export"
      )
    }
    if (stages_executed[1] %in% unsupported_idx) {
      relaxed <- survey_demote_rs_poisson_stage1(df, fpc, stages_executed[1])
      df <- relaxed$df
      fpc <- relaxed$fpc
    }
  }

  if (!fpc$has_rs_poisson_stage1) {
    pps <- if (fpc$has_pps_wor) "brewer" else FALSE
    return(list(pps = pps, df = df, fpc = fpc))
  }

  first_idx <- stages_executed[1]

  if (length(stages_executed) > 1L) {
    if (relax_pps_for_bootstrap) {
      relaxed <- survey_demote_rs_poisson_stage1(df, fpc, first_idx)
      pps <- if (relaxed$fpc$has_pps_wor) "brewer" else FALSE
      return(list(pps = pps, df = relaxed$df, fpc = relaxed$fpc))
    }
    abort_samplyr(
      c(
        "{.pkg survey} does not support multi-stage designs with a random-size Poisson method at stage 1.",
        "i" = "{.pkg survey} rejects multi-stage designs when the {.code pps} argument is set.",
        "i" = "Use {.code as_svrepdesign(type = \"subbootstrap\")} for a bootstrap approximation.",
        "i" = "Or convert each stage separately."
      ),
      class = "samplyr_error_multistage_poisson_stage1"
    )
  }

  stage_spec <- design$stages[[first_idx]]
  if (!is_null(stage_spec$clusters)) {
    cluster_vars <- stage_spec$clusters$vars
    n_clusters <- nrow(unique(df[, cluster_vars, drop = FALSE]))
    if (nrow(df) > n_clusters) {
      if (relax_pps_for_bootstrap) {
        relaxed <- survey_demote_rs_poisson_stage1(df, fpc, first_idx)
        return(list(pps = FALSE, df = relaxed$df, fpc = relaxed$fpc))
      }
      abort_samplyr(
        c(
          "Cannot export a clustered random-size Poisson design with multiple rows per sampled cluster via {.fn as_svydesign}.",
          "i" = "{.pkg survey}'s {.fn poisson_sampling} estimator treats rows as independent and does not honor within-cluster correlation.",
          "i" = "Use {.code as_svrepdesign(type = \"subbootstrap\")} for a bootstrap approximation that resamples clusters."
        ),
        class = "samplyr_error_cluster_poisson_export"
      )
    }
  }

  # Custom random-size WOR methods (registered with fixed_size = FALSE)
  # reach this point classified as rs_poisson, but
  # survey::poisson_sampling() is only valid when selections are
  # independent across units. Built-in Poisson methods qualify, and so
  # do custom methods whose author declared variance_family =
  # "poisson" (the declaration asserts independence). Undeclared
  # custom methods still error.
  is_declared_poisson <- identical(
    stage_spec$draw_spec$method_variance,
    "poisson"
  )
  if (
    !stage_spec$draw_spec$method %in% rs_poisson_methods &&
      !is_declared_poisson
  ) {
    if (relax_pps_for_bootstrap) {
      relaxed <- survey_demote_rs_poisson_stage1(df, fpc, first_idx)
      return(list(pps = FALSE, df = relaxed$df, fpc = relaxed$fpc))
    }
    abort_samplyr(
      c(
        "Cannot export the custom random-size method {.val {stage_spec$draw_spec$method}} via {.fn as_svydesign}.",
        "i" = "The method is registered with {.code fixed_size = FALSE}, so the sample size is random. {.pkg survey}'s Poisson variance estimator assumes selections are independent across units, which samplyr cannot verify for a custom method.",
        "i" = "If selections are independent (Poisson-type), pass the inclusion probabilities explicitly: {.code as_svydesign(x, pps = survey::poisson_sampling(1 / x$.weight))}.",
        "i" = "Otherwise use {.code as_svrepdesign(type = \"subbootstrap\")} for a bootstrap approximation."
      ),
      class = "samplyr_error_custom_random_wor_export"
    )
  }

  pi_vec <- df[[paste0(".fpc_pi_", first_idx)]]
  list(pps = survey::poisson_sampling(pi_vec), df = df, fpc = fpc)
}

#' @rdname as_svydesign
#' @export
as_svydesign.tbl_sample <- function(x, ..., nest = TRUE, method = NULL) {
  check_single_replicate(x, "as_svydesign")
  check_sample_unmodified(x, "as_svydesign")
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a survey design object."
  )

  phase_info <- survey_validate_phase_support(
    x,
    allow_twophase = TRUE,
    fn_name = "as_svydesign"
  )
  prev_phase <- phase_info$prev_phase
  is_twophase <- phase_info$is_twophase

  if (is_twophase) {
    method <- if (is_null(method)) {
      NULL
    } else {
      match.arg(method, c("full", "approx", "simple"))
    }
  } else if (!is_null(method)) {
    cli_abort(
      "{.arg method} is only valid when converting a two-phase sample."
    )
  }

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)

  df <- as.data.frame(x)

  if (is_twophase) {
    phase1 <- prev_phase$sample
    # The phase-2 sample itself is clean (checked above), but the
    # phase-1 sample it was drawn from may have been filtered or
    # otherwise modified before phase-2 execution. twophase() then
    # treats the modified rows as the complete phase-1 sample.
    phase1_status <- sample_realization_status(phase1)
    phase1_mods <- phase1_status$mods
    if (!phase1_status$ok) {
      cli_warn(c(
        "The phase-1 sample was modified after its execution
         ({.field {phase1_mods}} changed).",
        "i" = "{.fn survey::twophase} treats the current phase-1 rows
               as the complete phase-1 sample.",
        "i" = "If rows were removed to screen eligibility, estimates
               describe the screened population. For domain analysis,
               subset the exported design instead."
      ))
    }
    design1 <- prev_phase$design %||% get_design(phase1)
    stages1 <- prev_phase$stages %||% get_stages_executed(phase1)
    df1 <- as.data.frame(phase1)
    df2 <- df
    design2 <- design

    key_vars <- intersect(
      survey_key_vars(design1, stages1, df1),
      survey_key_vars(design2, stages_executed, df2)
    )

    if (length(key_vars) == 0) {
      cli_abort(
        c(
          "Two-phase conversion requires shared phase identifiers.",
          "i" = "Define a unique identifier available in both phases (e.g. via {.fn cluster_by})."
        )
      )
    }

    key_df <- df1[, key_vars, drop = FALSE]
    if (anyDuplicated(key_df) > 0) {
      cli_abort(
        c(
          "Phase 1 identifiers are not unique.",
          "i" = "Use a unique unit identifier for two-phase conversion."
        )
      )
    }

    # Between-phase subsampling variance is handled by
    # survey::twophase() itself, so unclustered element stages are not
    # synthesized into the per-phase ids formulas.
    id_info1 <- survey_id_info(
      design1, stages1, df1,
      synthesize_unclustered = FALSE, prefix = "p1_"
    )
    df1 <- id_info1$df
    id_info2 <- survey_id_info(
      design2, stages_executed, df2,
      synthesize_unclustered = FALSE, prefix = "p2_"
    )
    df2 <- id_info2$df
    id_vars2 <- id_info2$id_vars

    ids_formula1 <- survey_ids_formula(id_info1$id_vars)
    ids_formula2 <- survey_ids_formula(id_vars2)

    strata1 <- survey_strata_info(
      df1, design1, stages1,
      mode = "first_stage", prefix = "p1_"
    )
    df1 <- strata1$df
    strata2 <- survey_strata_info(
      df2, design2, stages_executed,
      mode = "first_stage", prefix = "p2_"
    )
    df2 <- strata2$df

    fpc1 <- survey_fpc_info(df1, design1, stages1, id_info1$stage_indices)
    df1 <- fpc1$df
    fpc2 <- survey_fpc_info(df2, design2, stages_executed, id_info2$stage_indices)
    df2 <- fpc2$df

    if (fpc1$has_pps_wor) {
      cli_abort(c(
        "Two-phase export does not support PPS at phase 1.",
        "i" = "{.fn survey::twophase} requires the phase 1 PPS specification to be {.code NULL}.",
        "i" = "Export each phase separately with {.fn as_svydesign} instead."
      ))
    }

    strata2_extra <- setdiff(strata2$vars, names(df1))
    id_vars2_extra <- setdiff(id_vars2, names(df1))

    fpc2_vars <- fpc2$fpc_vars
    fpc2_vars_renamed <- if (length(fpc2_vars) > 0) {
      sub("^\\.fpc_", ".fpc_phase2_", fpc2_vars)
    } else {
      character(0)
    }
    fpc2_rename_map <- setNames(fpc2_vars_renamed, fpc2_vars)

    phase2_cols_needed <- unique(
      c(
        key_vars,
        id_vars2_extra,
        strata2_extra,
        fpc2_vars,
        setdiff(names(df2), names(df1)),
        ".weight"
      )
    )
    phase2_cols_needed <- intersect(phase2_cols_needed, names(df2))

    df2_join <- df2[, phase2_cols_needed, drop = FALSE]
    if (".weight" %in% names(df2_join)) {
      names(df2_join)[names(df2_join) == ".weight"] <- ".weight_phase2"
    }
    if (length(fpc2_rename_map) > 0) {
      idx <- match(names(fpc2_rename_map), names(df2_join))
      names(df2_join)[idx] <- fpc2_rename_map
    }

    df_combined <- df1 |>
      left_join(df2_join, by = key_vars)

    df_combined$.phase2 <- !is.na(df_combined$.weight_phase2)
    if (!any(df_combined$.phase2)) {
      cli_abort(
        c(
          "Phase 2 rows could not be matched to phase 1 identifiers.",
          "i" = "Ensure a shared unique identifier is present in both phases."
        )
      )
    }
    df_combined$.weight_phase2_cond <- ifelse(
      df_combined$.phase2,
      df_combined$.weight_phase2 / df_combined$.weight,
      NA_real_
    )
    if (any(!is.finite(df_combined$.weight_phase2_cond[df_combined$.phase2]))) {
      cli_abort(
        "Invalid phase 2 conditional weights detected after matching phases."
      )
    }
    df_combined$.prob_1 <- 1 / df_combined$.weight
    df_combined$.prob_2 <- ifelse(
      df_combined$.phase2,
      1 / df_combined$.weight_phase2_cond,
      NA_real_
    )

    dots <- list(...)
    pps_arg <- if (!is_null(dots$pps)) dots$pps else NULL
    dots$pps <- NULL

    fpc2_formula <- if (length(fpc2_vars_renamed) == 0) {
      NULL
    } else {
      survey_formula_from_vars(fpc2_vars_renamed)
    }

    use_weights <- !is_null(method) && method %in% c("approx", "simple")
    probs_arg <- if (use_weights) {
      NULL
    } else {
      list(
        stats::as.formula("~.prob_1"),
        stats::as.formula("~.prob_2")
      )
    }
    weights_arg <- if (use_weights) {
      list(
        stats::as.formula("~.weight"),
        stats::as.formula("~.weight_phase2_cond")
      )
    } else {
      NULL
    }

    result <- do.call(
      survey::twophase,
      c(
        list(
          id = list(ids_formula1, ids_formula2),
          strata = list(strata1$formula, strata2$formula),
          probs = probs_arg,
          weights = weights_arg,
          fpc = list(fpc1$formula, fpc2_formula),
          subset = stats::as.formula("~.phase2"),
          data = df_combined,
          method = method,
          pps = pps_arg
        ),
        dots
      )
    )

    result
  } else {
    build_singlephase_svydesign(
      x,
      dots = list(...),
      nest = nest,
      relax_pps_for_bootstrap = FALSE
    )
  }
}

#' Build a single-phase survey.design from a tbl_sample.
#'
#' Shared by [as_svydesign.tbl_sample()] and [as_svrepdesign.tbl_sample()].
#' When `relax_pps_for_bootstrap = TRUE`, multi-stage stage-1 random-size
#' Poisson designs and clustered single-stage random-size Poisson designs
#' with multi-row clusters are exported with a permissive specification
#' (Inf at the Poisson stage, no `pps` argument), so that the bootstrap
#' resampler can produce a variance estimate. The relaxed design is not a
#' valid linearization design.
#' @noRd
build_singlephase_svydesign <- function(
  x,
  dots,
  nest,
  relax_pps_for_bootstrap = FALSE
) {
  design <- get_design(x)
  stages_executed <- get_stages_executed(x)
  df <- as.data.frame(x)

  # A user-supplied pps object (ppsmat, poisson_sampling, HR) is
  # single-stage in survey: multistage ids are rejected outright.
  # Export the first-stage design in that case, as documented for
  # exact PPS variance estimation.
  if (!is_null(dots$pps) && length(stages_executed) > 1L) {
    cli_warn(c(
      "Exact PPS variance ({.arg pps}) is single-stage in {.pkg survey}.",
      "i" = "Exporting the stage-1 design only; later-stage sampling
             variance is not represented.",
      "i" = "Omit {.arg pps} for exact multi-stage linearization with
             Brewer's approximation at the PPS stage."
    ))
    stages_executed <- stages_executed[1]
  }

  id_info <- survey_id_info(design, stages_executed, df)
  df <- id_info$df
  ids_formula <- survey_ids_formula(id_info$id_vars)

  strata <- survey_strata_info(
    df,
    design,
    stages_executed,
    id_stage_indices = id_info$stage_indices
  )
  df <- strata$df

  fpc <- survey_fpc_info(df, design, stages_executed, id_info$stage_indices)
  df <- fpc$df

  resolved <- survey_resolve_pps(
    df = df,
    design = design,
    stages_executed = stages_executed,
    fpc = fpc,
    user_pps = dots$pps,
    relax_pps_for_bootstrap = relax_pps_for_bootstrap
  )
  df <- resolved$df
  fpc <- resolved$fpc
  pps_arg <- resolved$pps

  dots$pps <- NULL

  result <- do.call(
    survey::svydesign,
    c(
      list(
        ids = ids_formula,
        strata = strata$formula,
        weights = stats::as.formula("~.weight"),
        fpc = fpc$formula,
        data = df,
        nest = nest,
        pps = pps_arg
      ),
      dots
    )
  )

  # Replace the stored call to avoid inlining the entire data frame,
  # which causes massive output when printing the survey.design object
  result$call <- call(
    "svydesign",
    ids = ids_formula,
    strata = strata$formula,
    weights = stats::as.formula("~.weight"),
    fpc = fpc$formula,
    data = quote(data),
    nest = nest
  )

  result
}

#' Convert a tbl_sample to a replicate-weight survey design
#'
#' Creates a `svyrep.design` object from a `tbl_sample` by first
#' converting to a [survey::svydesign()] object via [as_svydesign()],
#' then converting with [survey::as.svrepdesign()].
#'
#' @param x A `tbl_sample` object produced by [execute()].
#' @param type Replicate method passed to [survey::as.svrepdesign()].
#'   One of `"auto"`, `"JK1"`, `"JKn"`, `"BRR"`, `"bootstrap"`,
#'   `"subbootstrap"`, `"mrbbootstrap"`, or `"Fay"`.
#' @param ... Additional arguments passed to [survey::as.svrepdesign()].
#'
#' @return A `svyrep.design` object from the survey package.
#'
#' @details
#' Replicate conversion supports single-phase designs. For unequal-probability
#' designs (PPS or random-size Poisson), `"subbootstrap"` and `"mrbbootstrap"`
#' are the supported replicate types. Other types emit a warning and may fail
#' because inclusion probabilities vary within strata. For fixed-size PPS
#' variance estimation, linearization via [as_svydesign()] is generally
#' preferred. Two-phase designs should be exported with [as_svydesign()].
#'
#' ## Bootstrap escape hatch for random-size Poisson at stage 1
#'
#' Some designs cannot be expressed as a linearization-based
#' [survey::svydesign()] object. Specifically, multi-stage designs with
#' a random-size Poisson method (`bernoulli` or `pps_poisson`) at stage 1,
#' and single-stage designs with `cluster_by()` and multiple rows per
#' sampled cluster, are rejected by [as_svydesign()] for those methods.
#'
#' For these cases `as_svrepdesign(type = "subbootstrap")` (or
#' `"mrbbootstrap"`) is the recommended path. The design is exported with
#' a permissive specification (no finite-population correction at the
#' Poisson stage, no `pps` argument), and the bootstrap resampler supplies
#' the variance through replicate weights.
#'
#' This is the package's bootstrap approximation for designs that exact
#' Horvitz-Thompson linearization cannot express in
#' [survey::svydesign()]. The subbootstrap and mrbbootstrap methods were
#' developed for fixed-size PPS sampling (Antal and Tille 2011); their
#' behavior on random-size Poisson designs, especially at multiple
#' stages, has weaker theoretical backing and should be treated as an
#' approximation. In particular, the resampling is fixed-size, so it
#' does not capture the variance contribution of the random sample
#' size and can materially understate the total variance of a
#' Poisson-type design. When the exact Poisson linearization is
#' available (single-stage designs), prefer [as_svydesign()].
#'
#' Bounded cube, LPM2, and SCPS designs likewise have no native,
#' design-specific replicate variance estimator in `samplyr`.
#' `as_svrepdesign(type = "subbootstrap")` and `"mrbbootstrap"` export a
#' generic PPS bootstrap approximation for them; they do not reproduce the
#' original cube constraints or spatial selection algorithm within each
#' replicate. Treat the resulting variance estimates as approximations, not
#' as exact variance estimators for those designs.
#'
#' @examplesIf requireNamespace("survey", quietly = TRUE)
#' sample <- sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 42)
#'
#' rep_svy <- as_svrepdesign(sample, type = "auto")
#' survey::svymean(~households, rep_svy)
#'
#' @seealso [as_svydesign()] for linearization export,
#'   [survey::as.svrepdesign()] for the underlying conversion
#'
#' @export
as_svrepdesign <- function(x, ...) {
  UseMethod("as_svrepdesign")
}

#' @rdname as_svrepdesign
#' @export
as_svrepdesign.tbl_sample <- function(
  x,
  ...,
  type = c(
    "auto",
    "JK1",
    "JKn",
    "BRR",
    "bootstrap",
    "subbootstrap",
    "mrbbootstrap",
    "Fay"
  )
) {
  check_single_replicate(x, "as_svrepdesign")
  check_sample_unmodified(x, "as_svrepdesign")
  rlang::check_installed(
    "survey",
    reason = "to convert a tbl_sample to a replicate-weight survey design."
  )

  survey_validate_phase_support(
    x,
    allow_twophase = FALSE,
    fn_name = "as_svrepdesign"
  )

  type <- match.arg(type)

  design <- get_design(x)
  unequal_used <- unique(unlist(lapply(
    get_stages_executed(x),
    function(stage_idx) {
      draw_spec <- design$stages[[stage_idx]]$draw_spec
      kind <- survey_stage_kind(draw_spec)
      unequal <- kind %in%
        c("pps_wor", "rs_poisson", "unsupported") ||
        (kind == "wr" && !is_null(draw_spec$mos))
      if (unequal) draw_spec$method else NULL
    }
  )))
  pps_safe_types <- c("subbootstrap", "mrbbootstrap")
  if (length(unequal_used) > 0 && !type %in% pps_safe_types) {
    cli_warn(c(
      "{.fn as_svrepdesign} with {.val {type}} may not work for unequal-probability designs.",
      "i" = "Found method{?s}: {.val {unequal_used}}.",
      "i" = "Use {.val subbootstrap} or {.val mrbbootstrap} for unequal-probability designs,
             or use {.fn as_svydesign} for linearization-based variance."
    ))
  }

  svydesign_obj <- build_singlephase_svydesign(
    x,
    dots = list(),
    nest = TRUE,
    relax_pps_for_bootstrap = type %in% pps_safe_types
  )

  tryCatch(
    survey::as.svrepdesign(design = svydesign_obj, type = type, ...),
    error = function(e) {
      abort_samplyr(
        c(
          "{.fn as_svrepdesign} failed to convert this design to replicate weights.",
          "x" = "{conditionMessage(e)}"
        ),
        class = "samplyr_error_svrep_conversion_failed"
      )
    }
  )
}


#' Convert a tbl_sample to a srvyr tbl_svy object
#'
#' Creates a [srvyr::tbl_svy] object from a `tbl_sample` by first
#' converting to a [survey::svydesign()] object via [as_svydesign()],
#' then wrapping with [srvyr::as_survey_design()].
#'
#' This method is registered on the [srvyr::as_survey_design()] generic,
#' so it is available when srvyr is loaded.
#'
#' Random-size Poisson designs (`bernoulli`, `pps_poisson`) export to a
#' `pps` survey design. These are summarized, grouped, and subset like any
#' other srvyr design, with Horvitz-Thompson Poisson variances.
#'
#' @param .data A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to [as_svydesign()].
#'
#' @return A `tbl_svy` object from the srvyr package.
#'
#' @examplesIf requireNamespace("srvyr", quietly = TRUE)
#' library(srvyr)
#'
#' sample <- sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 12345)
#'
#' # Returns a tbl_svy for use with srvyr verbs
#' svy <- as_survey_design(sample)
#' svy |>
#'   group_by(region) |>
#'   summarise(mean_hh = survey_mean(households))
#'
#' @seealso [as_svydesign()] for converting to a survey.design2 object
#'
#' @exportS3Method srvyr::as_survey_design
as_survey_design.tbl_sample <- function(.data, ...) {
  rlang::check_installed(
    "srvyr",
    reason = "to convert a tbl_sample to a srvyr tbl_svy object."
  )

  survey_validate_phase_support(
    .data,
    allow_twophase = TRUE,
    fn_name = "as_survey_design"
  )

  svydesign_obj <- as_svydesign(.data, ...)
  if (inherits(svydesign_obj, c("twophase", "twophase2"))) {
    srvyr::as_survey_twophase(svydesign_obj)
  } else {
    srvyr::as_survey_design(srvyr_dispatchable_design(svydesign_obj))
  }
}

#' Make a survey design object dispatchable by srvyr's as_survey_design
#'
#' Random-size Poisson methods (`bernoulli`, `pps_poisson`) export through
#' [survey::poisson_sampling()], which yields an object of class
#' `c("pps", "survey.design")`. This trips srvyr in two ways: srvyr only
#' registers an `as_survey_design` method for `survey.design2` (so the
#' generic finds no method and errors), and its grouped path subsets the
#' design with `[`, which dispatches to the old `[.survey.design` method
#' that fails on `pps` objects.
#'
#' A `pps` design is built on the same internals as `survey.design2`, so
#' we insert that class tag just before `survey.design`. The generic then
#' finds srvyr's method, and `[` dispatches to the working
#' `[.survey.design2` method, which enables grouped srvyr verbs. `pps`
#' stays first, so survey's unequal-probability variance methods still
#' dispatch correctly.
#' @noRd
srvyr_dispatchable_design <- function(x) {
  if (inherits(x, "pps") && !inherits(x, "survey.design2")) {
    cls <- class(x)
    pos <- match("survey.design", cls)
    class(x) <- append(cls, "survey.design2", after = pos - 1L)
  }
  x
}

#' Convert a tbl_sample to a srvyr replicate-weight tbl_svy object
#'
#' Creates a [srvyr::tbl_svy] replicate design from a `tbl_sample` by first
#' converting to a `svyrep.design` object via `as_svrepdesign()`,
#' then wrapping with [srvyr::as_survey_rep()].
#'
#' @param .data A `tbl_sample` object produced by [execute()].
#' @param ... Additional arguments passed to `as_svrepdesign()`.
#'
#' @return A replicate-weight `tbl_svy` object from the srvyr package.
#'
#' @examplesIf requireNamespace("srvyr", quietly = TRUE)
#' library(srvyr)
#'
#' sample <- sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(bfa_eas, seed = 42)
#'
#' rep_tbl <- as_survey_rep(sample, type = "auto")
#' rep_tbl |>
#'   summarise(mean_hh = survey_mean(households, vartype = "se"))
#'
#' @seealso `as_svrepdesign()` for survey replicate-weight export
#'
#' @exportS3Method srvyr::as_survey_rep
as_survey_rep.tbl_sample <- function(.data, ...) {
  rlang::check_installed(
    "srvyr",
    reason = "to convert a tbl_sample to a srvyr replicate-weight tbl_svy object."
  )

  rep_design <- as_svrepdesign(.data, ...)
  srvyr::as_survey_rep(rep_design)
}
