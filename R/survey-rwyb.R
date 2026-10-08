#' Rao-Wu-Yue-Beaumont replicates from the recorded selection mechanisms
#'
#' Work with adjustment factors, keeping samplyr's original weights. WR
#' stages resample distinct draw occurrences, with zero variance FPC. Their
#' expected hit counts are not inclusion probabilities and must not be passed
#' to svrep as such.
#'
#' Generate independent conditional stage factors with svrep, then combine
#' them using Beaumont and Emond (2022), section 7, as in svrep's multistage
#' implementation. Generating stages separately lets certainty units have
#' exactly factor one: svrep 0.9.1 evaluates Gamma(Inf, 0) as zero for Poisson
#' certainties. It also lets us refuse noncertainty singletons rather than
#' silently treating them as certainties.
#' @noRd
build_rwyb_svrepdesign <- function(x, systematic_variance,
                                  replicates = 500, mse = TRUE,
                                  compress = TRUE, lonely.psu = "fail") {
  rlang::local_error_call(caller_env())
  rlang::check_installed("svrep", version = "0.9.1",
    reason = "to generate Rao-Wu-Yue-Beaumont replicate weights.")
  if (!is.numeric(replicates) || length(replicates) != 1L ||
      !is.finite(replicates) || replicates < 2 || replicates != floor(replicates)) {
    abort_samplyr("{.arg replicates} must be an integer of at least 2.",
      class = "samplyr_error_rwyb_input")
  }
  for (arg in c("mse", "compress")) {
    val <- get(arg)
    if (!is.logical(val) || length(val) != 1L || is.na(val)) {
      abort_samplyr("{.arg {arg}} must be TRUE or FALSE.",
      class = "samplyr_error_rwyb_input")
    }
  }
  # Only at the final stage, where no lower stage adds variance.
  if (!is.character(lonely.psu) || length(lonely.psu) != 1L ||
      !lonely.psu %in% c("fail", "certainty")) {
    abort_samplyr(
      c(
        "{.arg lonely.psu} must be {.val fail} or {.val certainty} for RWYB.",
        "i" = "{.val certainty} treats a final-stage stratum with one
               noncertainty unit as taken with certainty."
      ),
      class = "samplyr_error_rwyb_input"
    )
  }
  df <- as.data.frame(x)
  if (nrow(df) == 0L) {
    abort_samplyr("An empty sample cannot be exported as a replicate design.",
      class = "samplyr_error_rwyb_input")
  }
  design <- get_design(x)
  stages <- get_stages_executed(x)
  call <- current_env()
  methods <- vapply(stages, function(i) rwyb_stage_method(design$stages[[i]]$draw_spec, call = call), character(1))
  systematic <- systematic_approximated_stages(design, stages, df)
  check_systematic_variance(systematic, systematic_variance,
    approximation = "generic_replicates", fn_name = "as_svrepdesign")
  if (any(methods == "PPSWOR")) {
    cli_warn(c(
      "RWYB uses an approximation for unequal-probability sampling without replacement.",
      "i" = "It does not reproduce the sampler's exact joint inclusion probabilities."
    ), class = "samplyr_warning_rwyb_pps_approximation")
  }

  spec <- export_stage_spec(df, design, stages)
  # Refuse a stage the nested grammar cannot carry before creating factors.
  midstage <- Filter(function(e) e$midstage_element, spec$stage)
  if (length(midstage) > 0L) {
    abort_survey_midstage_element(midstage[[1]]$stage)
  }
  # Recorded whatever the digest, which may be absent.
  empty <- sample_empty_parents(x)
  if (length(empty) > 0L) {
    parent_stages <- sort(unique(vapply(empty, function(r) r$stage, 1L))) - 1L
    abort_samplyr(c(
      "Some selected stage-{parent_stages} units have no rows in the final sample.",
      "i" = "RWYB export currently requires every selected parent to be represented. The missing parents cannot be discarded from variance estimation."
    ), class = "samplyr_error_rwyb_missing_parents")
  }
  factors <- matrix(1, nrow(df), replicates)
  prior_prob <- rep(1, nrow(df))
  parent <- rep(1L, nrow(df))
  digest <- get_frame_digest(x)
  if (length(stages) > 1L && any(methods[-1L] == "Poisson") &&
      (is_null(digest) || !identical(digest$status, "complete"))) {
    abort_samplyr(c(
      "RWYB needs a complete frame digest for a multistage design with later Poisson sampling.",
      "i" = "Execute with {.code frame_digest = \"summary\"} or {.code \"full\"} so missing parent selections can be detected."
    ), class = "samplyr_error_rwyb_missing_parents")
  }
  for (pos in seq_along(stages)) {
    i <- stages[pos]
    entry <- spec$stage[[pos]]
    # User strata only: certainty units are left out through `active`.
    strata <- if (length(entry$strata$user)) group_ids(df, entry$strata$user) else rep(1L, nrow(df))
    pool <- group_ids(data.frame(parent = parent, stratum = strata), c("parent", "stratum"))
    unit <- group_ids(data.frame(pool = pool, unit = entry$unit$id), c("pool", "unit"))

    # A parent with no survivors still belongs to the first stage.
    stage_digest <- Filter(function(s) identical(s$stage_id, i), digest$stages)
    if (pos < length(stages) && length(stage_digest) &&
        length(unique(unit)) != sum(stage_digest[[1]]$pools$n_realized)) {
      abort_samplyr(c(
        "Some selected stage-{i} units have no rows in the final sample.",
        "i" = "RWYB export currently requires every selected parent to be represented. The missing parents cannot be discarded from variance estimation."
      ), class = "samplyr_error_rwyb_missing_parents")
    }
    prob <- entry$prob
    method <- methods[pos]
    wr <- method %in% c("SRSWR", "PPSWR")
    if (wr) prob[] <- 0
    if (draws_one_per_zone(design$stages[[i]]$draw_spec)) {
      # One draw per zone has the with-replacement variance within each
      # variance group. Certainty PSUs keep probability one.
      wr <- TRUE
      method <- "PPSWR"
      prob[!is_certainty_probability(prob)] <- 0
    }
    if (length(prob) != nrow(df) || any(!is.finite(prob)) ||
        any(prob < 0 | prob > 1) || (!wr && any(prob == 0))) {
      abort_samplyr("Invalid recorded stage-{i} probabilities for RWYB export.",
      class = "samplyr_error_rwyb_input")
    }
    first <- !duplicated(unit)
    unit_prob <- prob[first][match(unit, unit[first])]
    if (any(prob != unit_prob)) {
      abort_samplyr("Stage-{i} probabilities vary within a sampling unit.",
      class = "samplyr_error_rwyb_input")
    }
    active <- !is_certainty_probability(prob) & prior_prob > 0
    conditional <- matrix(1, nrow(df), replicates)
    if (any(active)) {
      # Count sampling units, not their rows of surviving descendants.
      final <- pos == length(stages)
      allow_singletons <- final && identical(lonely.psu, "certainty")
      if (method != "Poisson" && !allow_singletons &&
          any(tabulate(pool[active & !duplicated(unit)]) == 1L)) {
        abort_samplyr(c(
          "RWYB cannot estimate variance for a noncertainty singleton stratum at stage {i}.",
          "i" = "At least two sampled noncertainty units per stratum are needed for this sampling method.",
          "i" = if (final) {
            "Pass {.code lonely.psu = \"certainty\"} to treat each such unit
             as taken with certainty at this final stage, which gives it no
             variance of its own."
          } else {
            "Collapse the strata before sampling. Only a final-stage
             singleton can be treated as taken with certainty."
          }
        ), class = "samplyr_error_rwyb_singleton")
      }
      conditional[active, ] <- svrep::make_rwyb_bootstrap_weights(
        num_replicates = replicates,
        samp_unit_ids = matrix(unit[active], ncol = 1L),
        strata_ids = matrix(pool[active], ncol = 1L),
        samp_unit_sel_probs = matrix(prob[active], ncol = 1L),
        samp_method_by_stage = method,
        allow_final_stage_singletons = allow_singletons,
        output = "factors"
      )
    }
    # Damp so lower-stage variability is not counted twice.
    attenuation <- sqrt(prior_prob / (2 - prior_prob))
    factors <- factors * (1 + attenuation * (conditional - 1))
    prior_prob <- prior_prob * prob
    parent <- unit
  }
  # Rank on distinct rows: QR on the wide matrix is expensive.
  distinct <- factors[!duplicated(factors), , drop = FALSE]
  rank_matrix <- if (nrow(distinct) < ncol(distinct)) t(distinct) else distinct
  degrees <- qr(rank_matrix, tol = 1e-5)$rank - 1L
  result <- survey::svrepdesign(
    variables = df, weights = df$.weight, repweights = factors,
    combined.weights = FALSE, type = "bootstrap", mse = mse,
    degf = if (degrees > 1L) degrees else NULL,
    scale = if (mse) 1 / replicates else 1 / (replicates - 1),
    rscales = rep(1, replicates)
  )
  # survey 4.5's compressor drops the matrix dimension for one distinct row.
  if (compress && nrow(distinct) > 1L) result <- survey::compressWeights(result)
  attr(result, "samplyr_replication") <- list(
    method = "rwyb", backend = "svrep", stages = stages,
    methods = methods, pps_approximation = any(methods == "PPSWOR")
  )
  result$call <- call("as_svrepdesign", x = quote(x), type = "rwyb", replicates = replicates, mse = mse)
  record_systematic_variance(result, systematic, systematic_variance, "generic_replicates")
}

#' Only mechanisms whose variance interpretation is known may use RWYB
#' @noRd
rwyb_stage_method <- function(draw, call = caller_env()) {
  rlang::local_error_call(call)
  kind <- survey_stage_kind(draw)
  declared <- draw$method_variance
  supported <- is_null(draw$bounds) && (
    draw$method %in% c("srswor", "srswr", "systematic", "bernoulli",
      "pps_poisson", "pps_brewer", "pps_cps", "pps_sampford",
      "pps_systematic", "pps_multinomial") ||
    (!is_null(declared) && declared != "unsupported")
  )
  if (!supported) {
    abort_samplyr(c(
      "RWYB export does not support method {.val {draw$method}} and its constraints.",
      "i" = "A random sample size alone does not establish independent Poisson sampling. Custom methods must declare a supported variance family."
    ), class = "samplyr_error_rwyb_method")
  }
  switch(kind, equal_wor = "SRSWOR", pps_wor = "PPSWOR", rs_poisson = "Poisson",
    wr = if (is_null(draw$mos)) "SRSWR" else "PPSWR",
    abort_samplyr("This variance family is unsupported by RWYB.", class = "samplyr_error_rwyb_method"))
}
