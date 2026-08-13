#' Design effect and effective sample size
#'
#' These are the \pkg{svyplan} generics re-exported by samplyr. Samplyr adds
#' `tbl_sample` methods rather than defining competing generics.
#'
#' The `tbl_sample` methods report the **weighting loss**: how much precision
#' the realized weights cost relative to a self-weighting sample of the same
#' size. This is Kish's design effect,
#' \eqn{n \sum w_i^2 / (\sum w_i)^2}{n * sum(w^2) / sum(w)^2}, computed from
#' the `.weight` column, so it equals 1 for a self-weighting design and rises
#' with weight variability. It is outcome-independent, which is what makes it
#' available from the sample alone.
#'
#' It is one component of a full design effect and not a substitute for one.
#' Clustering and stratification also move precision, and neither is visible
#' in the weights. To estimate a design effect that reflects them, fit the
#' design and ask the estimator: `as_svydesign()` then
#' `survey::svymean(deff = TRUE)`, which is outcome-specific by necessity.
#' To *anticipate* the clustering component before collecting data, name the
#' planning arguments in the same call: `design_effect(x, icc = , n_per_psu =
#' )` forwards them to [svyplan::design_effect()] and returns the weighting
#' loss multiplied by the anticipated clustering component. Positional
#' arguments are refused, since these methods take no outcome variable.
#'
#' @param x A `tbl_sample`, or a numeric weight vector passed to the svyplan
#'   method.
#' @param ... Passed to the svyplan method: the planning arguments `icc`,
#'   `n_per_psu`, `n_per_ssu` and `var_ratio` are the useful ones here. Every
#'   argument must be named. svyplan reports any name it does not recognize.
#'
#' @return `design_effect()` returns a numeric `svyplan_deff` object. Use
#'   [as.double()] for the value. `effective_n()` returns a numeric scalar.
#'
#' @examples
#' set.seed(1207)
#' frame <- data.frame(
#'   id = 1:200,
#'   stratum = rep(c("A", "B"), each = 100),
#'   income = c(rnorm(100, 50, 10), rnorm(100, 80, 15))
#' )
#'
#' # A disproportionate allocation costs precision through its weights
#' samp <- sampling_design() |>
#'   stratify_by(stratum) |>
#'   draw(n = c(A = 10, B = 40)) |>
#'   execute(frame, seed = 1213)
#'
#' design_effect(samp)
#' effective_n(samp)
#'
#' # A proportional allocation is self-weighting, so the loss is 1
#' prop_samp <- sampling_design() |>
#'   stratify_by(stratum) |>
#'   draw(n = c(A = 25, B = 25)) |>
#'   execute(frame, seed = 1213)
#'
#' design_effect(prop_samp)
#'
#' # Anticipating the clustering component: the weighting loss above,
#' # multiplied by the clustering component the planning arguments imply
#' design_effect(samp, icc = 0.05, n_per_psu = 25)
#'
#' @seealso [svyplan::design_effect()], [svyplan::effective_n()],
#'   [svyplan::varcomp()], [svyplan::n_cluster()],
#'   [as_svydesign()] to hand the design to \pkg{survey} for an
#'   outcome-specific design effect
#'
#' @family diagnostics
#' @name design_effect
#' @importFrom svyplan design_effect effective_n
#' @export
svyplan::design_effect

#' @rdname design_effect
#' @export
svyplan::effective_n

#' @rdname design_effect
#' @export
design_effect.tbl_sample <- function(x, ...) {
  check_weighting_deff_dots(enquos(...), "design_effect")
  design_effect(weights = sample_weights(x, "design_effect"), ...)
}

#' @rdname design_effect
#' @export
effective_n.tbl_sample <- function(x, ...) {
  check_weighting_deff_dots(enquos(...), "effective_n")
  effective_n(weights = sample_weights(x, "effective_n"), ...)
}

#' Refuse a positional argument to the weighting-loss verbs
#'
#' `design_effect(x, y)` is the natural first thing to try, and it reads as
#' "the design effect for `y`". These methods take no outcome: the weighting
#' loss is computed from `.weight` alone, which is what makes it available
#' without one. Unguarded, the outcome is forwarded to svyplan and forced
#' there, so the user is told `object 'y' not found`.
#'
#' Only positional values are refused. Named arguments legitimately forward
#' to svyplan's planning components, and svyplan rejects the names it does
#' not know, so re-checking them here would duplicate a check that already
#' exists and would have to track svyplan's releases.
#'
#' @param dots The caller's `...`, captured with `enquos()`. The names are
#'   read without forcing the values.
#' @noRd
check_weighting_deff_dots <- function(dots, fn, call = rlang::caller_env()) {
  nms <- names(dots) %||% rep("", length(dots))
  unnamed <- sum(!nzchar(nms))
  if (unnamed == 0L) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "{.fn {fn}} takes no outcome variable.",
      "x" = "{unnamed} argument{?s} {?was/were} passed positionally.",
      "i" = "It reports the weighting (Kish) design effect, computed from
             the {.field .weight} column alone.",
      "i" = "For an outcome-specific design effect, export the sample with
             {.fn as_svydesign} and use
             {.code survey::svymean(deff = TRUE)}.",
      "i" = "To add an anticipated clustering component, name the planning
             arguments: {.code {fn}(x, icc = , n_per_psu = )}."
    ),
    class = "samplyr_error_unnamed_argument",
    call = call
  )
}

#' Extract the weight column a weighting-loss calculation needs
#' @noRd
sample_weights <- function(x, fn) {
  check_single_replicate(x, fn)
  check_sample_unmodified(x, fn)
  w <- x[[".weight"]]
  if (is.null(w)) {
    cli_abort("tbl_sample has no {.field .weight} column.")
  }
  w
}

#' Is this the schedule class samplyr consumes directly?
#' @noRd
is_svyplan_schedule <- function(x) {
  inherits(x, "svyplan_schedule")
}

#' Check the supported planning schema
#' @noRd
check_svyplan_schedule <- function(x, arg = "schedule",
                                   call = caller_env()) {
  if (!is_svyplan_schedule(x) || !identical(x$schema_version, 1L)) {
    abort_samplyr(
      "{.arg {arg}} must be an {.cls svyplan_schedule} with schema version 1.",
      class = "samplyr_error_svyplan_schedule",
      call = call
    )
  }
  invisible(x)
}

#' Coerce svyplan objects for draw()
#'
#' Uses svyplan's `as.data.frame()` contract for tabular plans and the
#' design context for multi-stage ones. A stage is "stage-aware" when it
#' is clustered or is not the first stage. There, cluster plans hand over
#' the value for that stage (PSU count, then per-cluster take) instead
#' of a grand total.
#'
#' - `n_alloc()` results: named per-stratum vector. For stratified
#'   two-stage plans (cluster mode), stage 1 gets `n_psu_int` and
#'   stage 2 gets `n_per_psu_int`, both named by stratum (the jointly
#'   integerized field design, svyplan >= 0.8.8).
#' - `n_multi()` results with domains: data frame keyed on the domain
#'   columns (requires a matching `stratify_by()`).
#' - `n_cluster()` results: `as.integer()` returns the integerized
#'   field design as a stage vector. Stage-aware contexts take the
#'   value for their stage, a flat single-stage design takes the
#'   product (operational element total). Per-domain plans expose only
#'   continuous stages, which are ceiled.
#' - Other svyplan objects: scalar total via `as.integer()`.
#' @noRd
coerce_svyplan_n <- function(n, stage_index = 1L, clustered = FALSE) {
  stage_aware <- clustered || stage_index > 1L

  if (inherits(n, "svyplan_n") && identical(n$type, "alloc")) {
    detail <- as.data.frame(n)
    if ("n_psu_int" %in% names(detail)) {
      if (stage_index == 1L && !clustered) {
        abort_samplyr(
          c(
            "This svyplan allocation plans PSUs, then elements within them.",
            "i" = "Declare the cluster structure with {.fn cluster_by} at stage 1 (on an EA-level frame, cluster by the EA id)."
          ),
          class = "samplyr_error_svyplan_clustered_plan"
        )
      }
      if (stage_index == 1L) {
        return(stats::setNames(detail$n_psu_int, detail$stratum))
      }
      if (stage_index == 2L) {
        return(stats::setNames(
          as.integer(detail$n_per_psu_int),
          detail$stratum
        ))
      }
      abort_samplyr(
        c(
          "This svyplan allocation covers 2 stages. The design is at stage {stage_index}.",
          "i" = "Pass an explicit {.arg n} for stages beyond the plan."
        ),
        class = "samplyr_error_svyplan_stage"
      )
    }
    return(stats::setNames(detail$n_int, detail$stratum))
  }

  if (inherits(n, "svyplan_n") && identical(n$type, "multi") &&
      !is_null(n$domains)) {
    tab <- as.data.frame(n)
    out <- tab[, setdiff(names(tab), grep("^\\.", names(tab), value = TRUE)),
               drop = FALSE]
    out$n <- as.integer(ceiling(tab$.n))
    return(out)
  }

  if (inherits(n, "svyplan_cluster")) {
    stage_cols <- c("n_psu", "n_per_psu", "n_per_ssu")
    if (!is_null(n$domains)) {
      dom <- as.data.frame(n)
      if (!stage_aware) {
        abort_samplyr(
          c(
            "This svyplan plan allocates per domain and per stage.",
            "i" = "Use it in a design with {.fn stratify_by} on the domain variable{?s} and {.fn cluster_by} at stage 1."
          ),
          class = "samplyr_error_svyplan_domains"
        )
      }
      if (stage_index > n$stages) {
        abort_samplyr(
          "This svyplan plan covers {n$stages} stages. The design is at stage {stage_index}.",
          class = "samplyr_error_svyplan_stage"
        )
      }
      keep <- setdiff(
        names(dom),
        c(stage_cols, grep("^\\.", names(dom), value = TRUE))
      )
      out <- dom[, keep, drop = FALSE]
      out$n <- as.integer(ceiling(dom[[stage_cols[stage_index]]]))
      return(out)
    }
    stages <- as.integer(n)
    if (stage_aware) {
      if (stage_index > length(stages)) {
        abort_samplyr(
          "This svyplan plan covers {length(stages)} stages. The design is at stage {stage_index}.",
          class = "samplyr_error_svyplan_stage"
        )
      }
      return(stages[[stage_index]])
    }
    return(as.integer(prod(stages)))
  }

  if (inherits(n, c("svyplan_n", "svyplan_power"))) {
    return(as.integer(n))
  }
  n
}

#' Variance components from an executed sample
#'
#' Estimate design-based variance components (B, W, icc, k) from a
#' `tbl_sample`, for planning the next round with
#' [svyplan::n_cluster()]. The method extracts everything the
#' estimation needs from the sample's design columns, applying two
#' conventions that are easy to get wrong by hand:
#'
#' - **Within-PSU weights.** The components are weighted by the
#'   product of the per-stage weights below stage 1 (`.weight_2`, or
#'   `.weight_2 * .weight_3`), never the compound `.weight`, whose
#'   stage-1 factor would overstate every cluster size.
#' - **Stage-1 selection shares.** For an unequal-probability first
#'   stage, per-PSU shares are derived from the stage-1 weights and
#'   normalized to sum to 1 over the sampled PSUs (per stratum when
#'   the first stage is stratified). For an equal-probability first
#'   stage the SRS path applies. See the Details of
#'   [svyplan::varcomp()].
#'
#' The decomposition covers the clustered stages plus the sample rows
#' as elements: one clustered stage gives 2-stage components, two give
#' 3-stage. Deeper designs are refused. Estimate the top stages and
#' fold the rest into a design effect. With-replacement (WR/PMR)
#' stages treat each draw as an independent unit, keyed by the
#' `.draw_k` column as in [as_svydesign()]: a cluster hit twice enters
#' the decomposition twice, with its share counted per draw. Two-phase samples are refused:
#' the nested decomposition does not model phase sampling. Certainty
#' PSUs are refused: self-representing PSUs contribute no between-PSU
#' variance and belong in their own stratum, so estimate components on
#' the probability part of the design.
#'
#' Variance-component estimation should use this method on the
#' `tbl_sample` directly, not [as_svydesign()]: the exported design
#' carries the compound weight and encodes stage probabilities in
#' fpc conventions that `varcomp` cannot see.
#'
#' @param x A `tbl_sample` with at least one executed clustered stage.
#' @param ... The outcome as a one-sided formula, e.g.
#'   `varcomp(x, ~y)`, mirroring the survey.design method. Nothing else
#'   belongs here: `strata` follows the `...` and so is matched exactly, so
#'   a near miss such as `strat` or a second positional formula is reported
#'   rather than silently dropped.
#' @param strata Optional one-sided formula naming a column to
#'   estimate per-stratum components by, when the first stage was not
#'   stratified. Stage-1 design strata are picked up automatically.
#'   Combining them with `strata` is an error, because per-stratum
#'   components crossed with design strata are ambiguous.
#'
#' @return A `svyplan_varcomp` object. Pass it as `icc` to
#'   [svyplan::n_cluster()]. Use [as.data.frame()] to export either the
#'   one-row unstratified components or the per-stratum component table.
#'
#' @examples
#' # Two-stage sample: 8 of 24 clusters by PPS, 3 persons per cluster
#' set.seed(7)
#' frame <- data.frame(
#'   cl = rep(sprintf("c%02d", 1:24), each = 5),
#'   size = rep(rep(c(80, 120, 160, 200), 6), each = 5),
#'   y = rnorm(120) + rep(rnorm(24, sd = 0.4), each = 5)
#' )
#' sam <- sampling_design() |>
#'   add_stage() |> cluster_by(cl) |>
#'   draw(n = 8, method = "pps_brewer", mos = size) |>
#'   add_stage() |> draw(n = 3) |>
#'   execute(frame, seed = 11)
#'
#' vc <- varcomp(sam, ~y)
#' vc
#' as.data.frame(vc)
#'
#' # Feed the components into next-round cluster planning
#' svyplan::n_cluster(stage_cost = c(500, 50), icc = vc,
#'                    budget = 100000)
#'
#' @seealso [svyplan::varcomp()] for the estimator and its
#'   conventions, [svyplan::n_cluster()] for planning with the result
#'
#' @name varcomp.tbl_sample
#' @aliases varcomp
#' @importFrom svyplan varcomp
#' @export
svyplan::varcomp

#' Refuse anything in varcomp()'s dots but the outcome formula
#'
#' `varcomp()` inherits its `...` from the generic, where the outcome is
#' positional, and forwards nothing. Its own `strata` follows the `...` and is
#' therefore matched exactly, so `strat = ~region` lands in `...`, is ignored,
#' and a stratified decomposition comes back unstratified with no sign that
#' anything was dropped.
#'
#' @param dots The caller's `...`, captured with `enquos()`. Names are read
#'   without forcing the values.
#' @noRd
check_varcomp_dots <- function(dots, call = rlang::caller_env()) {
  nms <- names(dots) %||% rep("", length(dots))

  named <- which(nzchar(nms))
  if (length(named) > 0) {
    suggestion <- suggest_reserved_arg(nms[[named[[1]]]], "strata")
    advice <- if (!is_null(suggestion)) {
      cli::format_inline("Did you mean {.arg {suggestion}}?")
    } else {
      cli::format_inline(
        "{.fn varcomp} takes the outcome formula, then {.arg strata} by name."
      )
    }
    abort_samplyr(
      c(
        "{.fn varcomp} received an unexpected argument.",
        "x" = cli::format_inline(
          "{.arg {nms[[named[[1]]]]}} is not an argument of {.fn varcomp}."
        ),
        "i" = advice
      ),
      class = "samplyr_error_unknown_argument",
      call = call
    )
  }

  if (length(dots) > 1L) {
    abort_samplyr(
      c(
        "{.fn varcomp} takes one outcome formula, not {length(dots)}.",
        "i" = "To estimate components by stratum, name the argument:
               {.code varcomp(x, ~y, strata = ~region)}."
      ),
      class = "samplyr_error_unnamed_argument",
      call = call
    )
  }

  invisible(NULL)
}

#' @rdname varcomp.tbl_sample
#' @family diagnostics
#' @export
varcomp.tbl_sample <- function(x, ..., strata = NULL) {
  check_single_replicate(x, "varcomp")
  check_sample_unmodified(x, "varcomp")

  metadata <- attr(x, "metadata") %||% list()
  if (!is_null(metadata$prev_phase)) {
    abort_samplyr(
      c(
        "{.fn varcomp} does not support two-phase samples.",
        "i" = "The decomposition assumes nested stages. Phase sampling
               is not a stage. Estimate components on each phase
               separately."
      ),
      class = "samplyr_error_varcomp_two_phase"
    )
  }

  quos <- enquos(...)
  check_varcomp_dots(quos)

  # Force dots here so a positional outcome gets a contract error, not lookup.
  outcome_label <- if (length(quos) >= 1) as_label(quos[[1]]) else ""
  # Captured here: inside the handler, caller_env() is the handler frame and
  # the error would be reported against `value[[3L]](cond)`.
  vc_frame <- environment()
  dots <- tryCatch(
    list(...),
    error = function(e) {
      abort_samplyr(
        c(
          "{.fn varcomp} takes the outcome as a one-sided formula:
           {.code varcomp(x, ~y)}.",
          "x" = "{.code {outcome_label}} could not be evaluated:
                 {conditionMessage(e)}",
          "i" = "A bare column name is not a formula. Write
                 {.code ~{outcome_label}}."
        ),
        class = "samplyr_error_varcomp_formula",
        call = vc_frame
      )
    }
  )

  fml <- if (length(dots) >= 1) dots[[1]] else NULL
  if (!inherits(fml, "formula") || length(fml) != 2L) {
    abort_samplyr(
      "Pass the outcome as a one-sided formula: {.code varcomp(x, ~y)}.",
      class = "samplyr_error_varcomp_formula"
    )
  }
  y_name <- all.vars(fml)
  if (length(y_name) != 1L) {
    abort_samplyr(
      "The outcome formula must reference exactly one variable."
    )
  }
  if (!y_name %in% names(x)) {
    abort_samplyr("Variable {.var {y_name}} not found in the sample.")
  }
  y <- x[[y_name]]

  design <- get_design(x)
  stages_executed <- get_stages_executed(x)
  k1 <- stages_executed[1]

  # Decomposition levels: every executed clustered stage contributes
  # its (ancestor-qualified) cluster key. The sample rows are the
  # elements below them.
  clustered <- stages_executed[vapply(
    stages_executed,
    function(k) !is_null(design$stages[[k]]$clusters),
    logical(1)
  )]
  if (length(clustered) == 0) {
    abort_samplyr(
      c(
        "{.fn varcomp} needs a clustered sample.",
        "i" = "The decomposition estimates between- and within-cluster
               components. This sample has no {.fn cluster_by} stage."
      ),
      class = "samplyr_error_varcomp_unclustered"
    )
  }
  if (length(clustered) > 2) {
    abort_samplyr(
      c(
        "{.fn varcomp} decomposes at most 3 stages (2 clustered stages
         above the elements). This sample has {length(clustered)}.",
        "i" = "Estimate components for the top stages and fold deeper
               stages into a design effect."
      ),
      class = "samplyr_error_varcomp_stages"
    )
  }
  stage_key <- function(k) {
    # WR/PMR occurrences are keyed by draw index, qualified by their pool.
    draw_col <- paste0(".draw_", k)
    if (is_multi_hit_method(design$stages[[k]]$draw_spec) &&
        draw_col %in% names(x)) {
      vars <- c(
        intersect(collect_ancestor_cluster_vars(design, k), names(x)),
        intersect(design$stages[[k]]$strata$vars, names(x)),
        draw_col
      )
      if (length(vars) == 1L) {
        return(x[[draw_col]])
      }
      return(make_group_key(x, vars))
    }
    vars <- unique(c(
      intersect(collect_ancestor_cluster_vars(design, k), names(x)),
      design$stages[[k]]$clusters$vars
    ))
    if (length(vars) == 1L) x[[vars]] else make_group_key(x, vars)
  }
  stage_ids <- lapply(clustered, stage_key)
  psu_key <- stage_ids[[1]]
  if (!any(duplicated(psu_key))) {
    abort_samplyr(
      c(
        "{.fn varcomp} needs elements below the first-stage clusters.",
        "i" = "Every row is its own cluster here, so within-cluster
               components are undefined."
      ),
      class = "samplyr_error_varcomp_unclustered"
    )
  }

  spec1 <- design$stages[[k1]]$draw_spec
  equal_prob <- spec1$method %in% equal_prob_methods ||
    identical(spec1$method_variance, "srs") ||
    (is_balanced_method(spec1) && is_null(spec1$mos))

  # Certainty PSUs contribute no between-PSU component and are not normalized.
  cert_col <- paste0(".certainty_", k1)
  if (cert_col %in% names(x) && any(x[[cert_col]])) {
    abort_samplyr(
      c(
        "The first stage holds certainty selections. {.fn varcomp}
         covers probability PSUs only.",
        "i" = "Self-representing PSUs contribute no between-PSU
               variance and act as their own strata. Estimate the
               components on a design without certainty selections,
               and handle the certainty stratum separately."
      ),
      class = "samplyr_error_varcomp_certainty"
    )
  }

  # Within-PSU weights: the product of the per-stage weights below
  # stage 1. Whole-take rows below a single executed clustered stage
  # are self-weighting within their cluster.
  later <- stages_executed[-1]
  w_within <- if (length(later) == 0) {
    rep(1, nrow(x))
  } else {
    Reduce(`*`, lapply(later, function(k) x[[paste0(".weight_", k)]]))
  }

  # Stage-1 strata (design) or the user's stratification, not both.
  strata_vec <- NULL
  design_strata <- design$stages[[k1]]$strata$vars
  if (!is_null(strata) && !is_null(design_strata)) {
    abort_samplyr(
      c(
        "The first stage is stratified by
         {.var {design_strata}}. A {.arg strata} argument would cross
         two stratifications.",
        "i" = "Design strata are picked up automatically. For other
               splits, derive per-domain samples and estimate each."
      ),
      class = "samplyr_error_varcomp_strata"
    )
  }
  if (!is_null(design_strata)) {
    strata_vec <- if (length(design_strata) == 1L) {
      x[[design_strata]]
    } else {
      make_group_key(x, design_strata)
    }
  } else if (!is_null(strata)) {
    if (!inherits(strata, "formula") || length(all.vars(strata)) != 1L) {
      abort_samplyr(
        "{.arg strata} must be a one-sided formula naming one column."
      )
    }
    s_name <- all.vars(strata)
    if (!s_name %in% names(x)) {
      abort_samplyr("Variable {.var {s_name}} not found in the sample.")
    }
    strata_vec <- x[[s_name]]
  }

  # Recover unequal-probability stage-1 shares from stage weights.
  prob <- NULL
  if (!equal_prob) {
    pi1 <- 1 / x[[paste0(".weight_", k1)]]
    spread_in_psu <- tapply(pi1, psu_key, function(v) diff(range(v)))
    if (any(spread_in_psu > 1e-9 * max(pi1))) {
      abort_samplyr(
        "Stage-1 weights vary within a first-stage cluster. The design
         columns are inconsistent."
      )
    }
    first_of_psu <- !duplicated(psu_key)
    psu_pi <- pi1[first_of_psu]
    psu_stratum <- if (is_null(strata_vec)) {
      rep("all", sum(first_of_psu))
    } else {
      as.character(strata_vec)[first_of_psu]
    }
    share <- psu_pi / stats::ave(psu_pi, psu_stratum, FUN = sum)
    prob <- share[match(psu_key, psu_key[first_of_psu])]
  }

  svyplan::varcomp(
    y,
    stage_id = stage_ids,
    prob = prob,
    weights = w_within,
    strata = strata_vec
  )
}
