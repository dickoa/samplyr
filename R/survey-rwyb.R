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
                                  compress = TRUE) {
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
  df <- as.data.frame(x)
  if (nrow(df) == 0L) {
    abort_samplyr("An empty sample cannot be exported as a replicate design.",
      class = "samplyr_error_rwyb_input")
  }
  design <- get_design(x)
  stages <- get_stages_executed(x)
  methods <- vapply(stages, function(i) rwyb_stage_method(design$stages[[i]]$draw_spec), character(1))
  systematic <- systematic_approximated_stages(design, stages, df)
  check_systematic_variance(systematic, systematic_variance,
    approximation = "generic_replicates", fn_name = "as_svrepdesign")
  if (any(methods == "PPSWOR")) {
    cli_warn(c(
      "RWYB uses an approximation for unequal-probability sampling without replacement.",
      "i" = "It does not reproduce the sampler's exact joint inclusion probabilities."
    ), class = "samplyr_warning_rwyb_pps_approximation")
  }

  # This also validates the nested-stage grammar before creating any factors.
  ids <- survey_id_info(design, stages, df)
  df_ids <- ids$df
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
    spec <- design$stages[[i]]
    strata <- if (length(spec$strata$vars)) group_ids(df, spec$strata$vars) else rep(1L, nrow(df))
    pool <- group_ids(data.frame(parent = parent, stratum = strata), c("parent", "stratum"))
    id_var <- ids$id_vars[match(i, ids$stage_indices)]
    unit <- if (is.na(id_var)) seq_len(nrow(df)) else df_ids[[id_var]]
    unit <- group_ids(data.frame(pool = pool, unit = unit), c("pool", "unit"))

    # A selected parent with no surviving descendants still belongs to the
    # first-stage empirical distribution. Never silently resample survivors.
    stage_digest <- Filter(function(s) identical(s$stage_id, i), digest$stages)
    if (pos < length(stages) && length(stage_digest) &&
        length(unique(unit)) != sum(stage_digest[[1]]$pools$n_realized)) {
      abort_samplyr(c(
        "Some selected stage-{i} units have no rows in the final sample.",
        "i" = "RWYB export currently requires every selected parent to be represented. The missing parents cannot be discarded from variance estimation."
      ), class = "samplyr_error_rwyb_missing_parents")
    }
    prob <- 1 / df[[paste0(".weight_", i)]]
    wr <- methods[pos] %in% c("SRSWR", "PPSWR")
    if (wr) prob[] <- 0
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
    active <- prob < 1 & prior_prob > 0
    conditional <- matrix(1, nrow(df), replicates)
    if (any(active)) {
      # Count sampling units, not their rows of surviving descendants.
      if (methods[pos] != "Poisson" &&
          any(tabulate(pool[active & !duplicated(unit)]) == 1L)) {
        abort_samplyr(c(
          "RWYB cannot estimate variance for a noncertainty singleton stratum at stage {i}.",
          "i" = "At least two sampled noncertainty units per stratum are needed for this sampling method."
        ), class = "samplyr_error_rwyb_singleton")
      }
      conditional[active, ] <- svrep::make_rwyb_bootstrap_weights(
        num_replicates = replicates,
        samp_unit_ids = matrix(unit[active], ncol = 1L),
        strata_ids = matrix(pool[active], ncol = 1L),
        samp_unit_sel_probs = matrix(prob[active], ncol = 1L),
        samp_method_by_stage = methods[pos],
        allow_final_stage_singletons = FALSE,
        output = "factors"
      )
    }
    # Damp conditional factors to avoid counting lower-stage
    # variability twice. WR ancestors have zero variance FPC.
    attenuation <- sqrt(prior_prob / (2 - prior_prob))
    factors <- factors * (1 + attenuation * (conditional - 1))
    prior_prob <- prior_prob * prob
    parent <- unit
  }
  # Compute rank on distinct factor rows with the tall matrix orientation.
  # survey's default QR on a very wide, expanded cluster matrix is expensive.
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
rwyb_stage_method <- function(draw) {
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
