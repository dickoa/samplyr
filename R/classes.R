#' Create a new sampling_design object
#'
#' Low-level constructor for sampling_design objects. Users should use
#' [sampling_design()] instead.
#'
#' @param title Optional title for the design
#' @param stages List of sampling_stage objects
#' @param current_stage Index of the current stage being built
#' @param validated Logical indicating if design has been validated
#'
#' @return A sampling_design object
#' @noRd
new_sampling_design <- function(
  title = NULL,
  stages = list(),
  current_stage = 0L,
  validated = FALSE
) {
  structure(
    list(
      title = title,
      stages = stages,
      current_stage = current_stage,
      validated = validated
    ),
    class = "sampling_design"
  )
}

#' Validate a sampling_design object
#' @noRd
validate_sampling_design <- function(x) {
  if (!inherits(x, "sampling_design")) {
    cli_abort("Object must be a {.cls sampling_design}")
  }

  if (!is_null(x$title) && !is_character(x$title)) {
    cli_abort("{.arg title} must be a character string")
  }

  if (!is.list(x$stages)) {
    cli_abort("{.arg stages} must be a list")
  }

  x
}

#' Test if object is a sampling_design
#'
#' @param x Object to test
#' @return Logical
#' @export
is_sampling_design <- function(x) {
  inherits(x, "sampling_design")
}

#' Create a new sampling_stage object
#'
#' Low-level constructor for sampling_stage objects.
#'
#' @param label Optional label for the stage
#' @param strata Stratification specification
#' @param clusters Cluster specification
#' @param draw_spec Draw specification
#'
#' @return A sampling_stage object
#' @noRd
new_sampling_stage <- function(
  label = NULL,
  strata = NULL,
  clusters = NULL,
  draw_spec = NULL
) {
  structure(
    list(
      label = label,
      strata = strata,
      clusters = clusters,
      draw_spec = draw_spec
    ),
    class = "sampling_stage"
  )
}

#' Test if object is a sampling_stage
#'
#' @param x Object to test
#' @return Logical
#' @noRd
is_sampling_stage <- function(x) {
  inherits(x, "sampling_stage")
}

#' Create a stratum specification
#'
#' @param vars Character vector of stratification variable names
#' @param alloc Allocation method
#' @param variance Variance data frame for Neyman/optimal allocation
#' @param cost Cost data frame for optimal allocation
#' @param cv Coefficient-of-variation data frame for power allocation
#' @param importance Importance/size data frame for power allocation
#' @param power Power exponent for power allocation
#'
#' @return A stratum_spec object
#' @noRd
new_stratum_spec <- function(
  vars,
  alloc = NULL,
  variance = NULL,
  cost = NULL,
  cv = NULL,
  importance = NULL,
  power = NULL
) {
  structure(
    list(
      vars = vars,
      alloc = alloc,
      variance = variance,
      cost = cost,
      cv = cv,
      importance = importance,
      power = power
    ),
    class = "stratum_spec"
  )
}

#' Create a cluster specification
#'
#' @param vars Character vector of clustering variable names
#'
#' @return A cluster_spec object
#' @noRd
new_cluster_spec <- function(vars) {
  structure(
    list(vars = vars),
    class = "cluster_spec"
  )
}

#' Create a draw specification
#'
#' @param n Sample size
#' @param frac Sampling fraction
#' @param method Selection method
#' @param mos Measure of size variable name
#' @param prn Permanent random number variable name
#' @param min_n Minimum sample size per stratum
#' @param max_n Maximum sample size per stratum
#' @param round Rounding method
#' @param control List of quosures for control sorting
#' @param certainty_size Absolute MOS threshold for certainty selection
#' @param certainty_prop Proportional MOS threshold for certainty selection
#'
#' @return A draw_spec object
#' @noRd
new_draw_spec <- function(
  n = NULL,
  frac = NULL,
  method = "srswor",
  mos = NULL,
  prn = NULL,
  min_n = NULL,
  max_n = NULL,
  round = "up",
  control = NULL,
  certainty_size = NULL,
  certainty_prop = NULL,
  on_empty = "warn"
) {
  structure(
    list(
      n = n,
      frac = frac,
      method = method,
      mos = mos,
      prn = prn,
      min_n = min_n,
      max_n = max_n,
      round = round,
      control = control,
      certainty_size = certainty_size,
      certainty_prop = certainty_prop,
      on_empty = on_empty
    ),
    class = "draw_spec"
  )
}

#' Create a new tbl_sample object
#'
#' Low-level constructor for tbl_sample objects. This is the result
#' of executing a sampling design.
#'
#' @param data The sampled data frame
#' @param design The design that was executed
#' @param stages_executed Integer vector of executed stages
#' @param seed Random seed used
#' @param metadata Additional metadata
#'
#' @return A tbl_sample object
#' @noRd
new_tbl_sample <- function(
  data,
  design,
  stages_executed,
  seed = NULL,
  metadata = list()
) {
  if (!inherits(data, "tbl_df")) {
    data <- tibble::as_tibble(data)
  }
  structure(
    data,
    design = design,
    stages_executed = stages_executed,
    seed = seed,
    metadata = metadata,
    class = unique(c("tbl_sample", class(data)))
  )
}

#' Test if object is a tbl_sample
#'
#' @param x Object to test
#' @return Logical
#' @export
is_tbl_sample <- function(x) {
  inherits(x, "tbl_sample")
}

#' Get design from a sample
#'
#' @param x A tbl_sample object
#' @return The sampling_design used to create the sample
#' @export
get_design <- function(x) {
  if (!is_tbl_sample(x)) {
    cli_abort("{.arg x} must be a {.cls tbl_sample}")
  }
  attr(x, "design")
}

#' Get executed stages from a sample
#'
#' @param x A tbl_sample object
#' @return Integer vector of executed stages
#' @export
get_stages_executed <- function(x) {
  if (!is_tbl_sample(x)) {
    cli_abort("{.arg x} must be a {.cls tbl_sample}")
  }
  attr(x, "stages_executed")
}

#' Reconstruct a tbl_sample after dplyr operations
#'
#' This method ensures that the tbl_sample class is only preserved
#' when the essential column (.weight) is still present.
#' Operations like summarise() or count() that remove this column
#' will return a regular tibble instead.
#'
#' @param data The modified data
#' @param template The original tbl_sample object
#' @return A tbl_sample if essential columns present, otherwise a tibble
#' @export
#' @keywords internal
dplyr_reconstruct.tbl_sample <- function(data, template) {
  essential_cols <- c(".weight")

  has_essential <- all(essential_cols %in% names(data))

  if (has_essential) {
    new_tbl_sample(
      data = data,
      design = attr(template, "design"),
      stages_executed = attr(template, "stages_executed"),
      seed = attr(template, "seed"),
      metadata = attr(template, "metadata")
    )
  } else {
    tibble::as_tibble(data)
  }
}

#' Subset a tbl_sample preserving class
#'
#' Subsetting a tbl_sample with `[` preserves the tbl_sample class
#' and its sampling metadata when the essential column (.weight)
#' remains.
#'
#' @param x A tbl_sample object
#' @param i Row index
#' @param j Column index
#' @param ... Additional arguments passed to the default method
#' @param drop Coerce to lowest possible dimension
#' @return A tbl_sample if essential columns remain, otherwise a data.frame
#' @method [ tbl_sample
#' @export
`[.tbl_sample` <- function(x, i, j, ..., drop = FALSE) {
  result <- NextMethod()
  if (is.data.frame(result)) {
    essential_cols <- c(".weight")
    if (all(essential_cols %in% names(result))) {
      return(new_tbl_sample(
        data = result,
        design = attr(x, "design"),
        stages_executed = attr(x, "stages_executed"),
        seed = attr(x, "seed"),
        metadata = attr(x, "metadata")
      ))
    }
    class(result) <- setdiff(class(result), "tbl_sample")
    attr(result, "design") <- NULL
    attr(result, "stages_executed") <- NULL
    attr(result, "seed") <- NULL
    attr(result, "metadata") <- NULL
  }
  result
}
