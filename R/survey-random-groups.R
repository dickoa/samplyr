#' Random-group replicate weights for a replicated execution
#'
#' Each replicate of `execute(reps = R)` is an independent sample of the whole
#' design, so its estimate is unbiased and independent of the others. The
#' pooled estimate is their mean, and the spread of the R estimates divided
#' by R estimates its variance whatever the selection method (Wolter 2007,
#' ch. 2). The full-sample weight is `.weight / R`, and replicate r gives its
#' own rows their `.weight` and every other row zero, so it reproduces that
#' replicate's estimate, zero for a replicate with no rows.
#' @noRd
build_random_groups_svrepdesign <- function(x, mse = FALSE,
                                            call = caller_env()) {
  if (!is.logical(mse) || length(mse) != 1L || is.na(mse)) {
    abort_samplyr(
      "{.arg mse} must be TRUE or FALSE.",
      class = "samplyr_error_survey_argument",
      call = call
    )
  }
  meta <- attr(x, "metadata")
  counts <- meta$replicate_rows
  if (!".replicate" %in% names(x) || length(counts) < 2L) {
    abort_samplyr(
      c(
        "Random groups need a sample with at least two replicates.",
        "i" = "Execute the design with {.code execute(..., reps = R)}."
      ),
      class = "samplyr_error_random_groups_input",
      call = call
    )
  }
  present <- table(factor(as.character(x$.replicate), levels = names(counts)))
  if (!identical(as.integer(present), as.integer(counts))) {
    abort_samplyr(
      c(
        "Random groups need every replicate of the execution, complete.",
        "x" = "This sample holds {sum(present > 0)} of the {length(counts)}
               replicates, or not all of their rows."
      ),
      class = "samplyr_error_random_groups_input",
      call = call
    )
  }
  if (!isTRUE(meta$replicates_complete)) {
    abort_samplyr(
      c(
        "The replicates of this sample share an earlier selection.",
        "x" = "They continue one realized stage or phase, so they do not
               vary with it, and random groups would leave its variance
               out.",
        "i" = "Execute the whole design with {.arg reps}, or replicate the
               earlier stage or phase too and continue each replicate."
      ),
      class = "samplyr_error_random_groups_shared",
      call = call
    )
  }

  df <- as.data.frame(x)
  if (nrow(df) == 0L) {
    abort_samplyr(
      "Every replicate of this sample is empty.",
      class = "samplyr_error_random_groups_input",
      call = call
    )
  }
  n_groups <- length(counts)
  group <- match(as.character(df$.replicate), names(counts))
  repweights <- matrix(0, nrow(df), n_groups)
  repweights[cbind(seq_len(nrow(df)), group)] <- df$.weight
  result <- survey::svrepdesign(
    variables = df,
    repweights = repweights,
    weights = df$.weight / n_groups,
    combined.weights = TRUE,
    type = "other",
    scale = 1 / (n_groups * (n_groups - 1)),
    rscales = rep(1, n_groups),
    mse = mse,
    degf = n_groups - 1L
  )
  attr(result, "samplyr_replication") <- list(
    method = "random_groups",
    replicates = n_groups
  )
  result$call <- call("as_svrepdesign", x = quote(x), type = "random_groups")
  result
}
