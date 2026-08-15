## Sharing design weights with a linked target population

# The generalized weight share method. A sample was selected from one
# population and the estimates are wanted for another, linked to it: children
# through their parents, establishments through their enterprises, persons
# through their dwellings.
#
# Nothing here touches selection. Lavallee section 2.2.2 is the reason the
# whole feature is a post-execute transformation: the method needs inclusion
# probabilities only for units actually selected, which is exactly what
# `.weight` already holds.

#' Share design weights with a linked target population
#'
#' `share_weights()` applies the generalized weight share method (Lavallee
#' 2007): it turns a sample of one population into a weighted sample of a
#' second population linked to it. The rows of the result are units of the
#' target population, not the units that were selected.
#'
#' @section The weight:
#' Every target unit in a target cluster reached by the sample receives the
#' same weight, which is the source weight carried across the links and
#' divided by the population number of links to that cluster:
#'
#' \deqn{w_i = \sum_j \frac{I(j \in S)}{\pi_j} \frac{L_{ji}}{L_i}}
#'
#' Assigning one weight per cluster is what makes unit-level and
#' cluster-level estimates of the same total agree.
#'
#' @section The denominator:
#' `multiplicity` is where the statistical content sits, and it has no
#' default. The denominator is defined over the *whole* source population,
#' not over the sample, so counting the rows of a link table asserts that the
#' table is a complete register. That assertion is the classic source of bias
#' in this method, so it has to be made at the call site rather than guessed:
#' either name a column of `targets` holding the population multiplicity, or
#' state the assertion with [complete_links()].
#'
#' Links need not be counted. [weighted_links()] replaces the 0/1 indicator
#' with a non-negative importance, which Lavallee section 4.5 shows costs no
#' theory as long as each target cluster totals more than zero. Counting is
#' the case where every link counts for one.
#'
#' @section Coverage:
#' A target cluster with no link to the source population can never be
#' reached, and the estimator understates totals by exactly its share. That is
#' Lavallee's Constraint 2.1, and it is detectable only when `targets`
#' enumerates the population, so it is recorded rather than warned about here
#' and reported at the analysis boundary.
#'
#' A target unit inside a reached cluster with no link of its own is a
#' different thing and is correct: it receives its cluster's weight. Producing
#' a weight for exactly those units is one of the reasons to use this method.
#'
#' @param x An executed `tbl_sample`, the sample of the source population.
#' @param targets The target register: one row per target unit, in the scope
#'   named by `target_scope`. Required, because a link table holds only linked
#'   units and so cannot describe a unit that has no link of its own.
#' @param links The link table: one row per link between a source unit and a
#'   target unit. Duplicate rows are refused, because a link counted twice
#'   changes both the numerator and the denominator.
#' @param by,to Named character vectors matching the source sample and the
#'   target register to `links`, in the same direction as a dplyr join:
#'   `c(local_column = "links_column")`.
#' @param within The target cluster, Lavallee's \eqn{i}. Never inferred. Give
#'   a bare column of `targets` for the ordinary clustered method, `NULL` to
#'   make every target unit its own cluster, or [extend_links()] to eliminate
#'   clusters by extending the links across them.
#' @param ... Must be empty. Arguments after it are matched by exact name.
#' @param multiplicity The population multiplicity \eqn{L_{ik}}: a bare column
#'   of `targets`, or [complete_links()] to assert that `links` is the
#'   complete population register and let samplyr count it. For links carrying
#'   an importance rather than a presence, [weighted_links()].
#' @param target_scope What `targets` is. `"reached"`, the default, claims
#'   only a full roster of the clusters the sample reached. `"population"`
#'   asserts that it enumerates the target population, and only that form
#'   supports checking for clusters that can never be reached.
#'
#' @return A `tbl_sample` whose rows are target units. `.weight` holds the
#'   shared estimation weight, `.unit_links` the unit's population
#'   multiplicity, and `.cluster_links` the cluster denominator used. Under
#'   [weighted_links()] those two are `.link_weight` and
#'   `.cluster_link_weight` instead, holding the same quantities on the
#'   importance scale; only the pair belonging to the scale in use is emitted.
#'   The recorded design still describes selection from the source population.
#'
#' @references
#' Lavallee, P. (2007). *Indirect Sampling*. Springer.
#'
#' @examples
#' dwellings <- data.frame(dwelling_id = 1:20, region = rep(1:2, each = 10))
#' sample <- sampling_design() |>
#'   draw(n = 8) |>
#'   execute(dwellings, seed = 1)
#'
#' # Two people per dwelling, each linked to the one dwelling they live in.
#' people <- data.frame(
#'   person_id = 1:40,
#'   household = rep(1:20, each = 2)
#' )
#' links <- data.frame(dwelling_id = rep(1:20, each = 2), person_id = 1:40)
#'
#' share_weights(
#'   sample,
#'   targets = people,
#'   links = links,
#'   by = c(dwelling_id = "dwelling_id"),
#'   to = c(person_id = "person_id"),
#'   within = household,
#'   multiplicity = complete_links()
#' )
#'
#' @family weight sharing
#' @export
share_weights <- function(
  x,
  targets,
  links,
  by,
  to,
  within,
  ...,
  multiplicity = NULL,
  target_scope = c("reached", "population")
) {
  check_keyword_args(enquos(...), c("multiplicity", "target_scope"))
  target_scope <- match.arg(target_scope)

  if (!is_tbl_sample(x)) {
    abort_samplyr(
      "{.arg x} must be an executed {.cls tbl_sample}.",
      class = "samplyr_error_share_weights_input"
    )
  }
  check_single_replicate(x, "share_weights")
  check_sample_unmodified(x, "share_weights")
  check_weight_contract(
    x, "share_weights",
    class = "samplyr_error_share_weights_weight_contract",
    advice = c(
      "i" = "Weights are shared once. Chaining a second transformation would
             compose two link structures into one weight vector that neither
             record describes."
    )
  )

  targets <- check_share_table(targets, "targets")
  links <- check_share_table(links, "links")

  within_spec <- parse_within(enquo(within), targets)
  mult_spec <- parse_multiplicity(enquo(multiplicity))

  by <- parse_join_map(by, "by", x, "the sample", links)
  to <- parse_join_map(to, "to", targets, "{.arg targets}", links)

  validate_share_inputs(x, targets, links, by, to, within_spec, mult_spec)

  generated <- weight_share_generated_cols[[mult_spec$scale]]
  check_generated_cols(targets, generated, "share_weights")

  parts <- gwsm_compute(x, targets, links, by, to, within_spec, mult_spec)

  result_targets <- targets[parts$kept, , drop = FALSE]
  result_targets[[generated[[1]]]] <- parts$unit_links[parts$kept]
  result_targets[[generated[[2]]]] <- parts$cluster_links[parts$kept]
  result_targets[[".weight"]] <- apply_share_operator(
    parts$operator, x[[".weight"]]
  )

  record <- new_weight_share_record(
    operator = parts$operator,
    source_sample = x,
    source_integrity = attr(x, "metadata")$integrity,
    source_key_cols = names(by),
    target_key_cols = names(to),
    target_cluster = within_spec$col,
    within_mode = within_spec$mode,
    denominator = list(mode = mult_spec$mode, scale = mult_spec$scale),
    coverage = new_weight_share_coverage(
      target_scope = target_scope,
      n_target_units = nrow(targets),
      n_target_clusters = parts$n_clusters,
      n_reached_clusters = parts$n_reached,
      n_unlinked_units = sum(parts$unit_links[parts$kept] == 0),
      # Identifiable only when `targets` claims to be the population. Under
      # reached scope the absence of an observed orphan is not evidence that
      # none exists, so the question is recorded as unasked.
      orphan_clusters = if (identical(target_scope, "population")) {
        parts$orphan_clusters
      },
      cluster_digest = if (identical(target_scope, "population")) {
        parts$cluster_digest
      }
    ),
    generated_cols = generated,
    call_info = list(
      fn = "share_weights",
      by = by,
      to = to,
      within = within_spec,
      multiplicity = mult_spec,
      target_scope = target_scope
    )
  )

  result <- new_tbl_sample(
    data = result_targets,
    design = get_design(x),
    stages_executed = get_stages_executed(x),
    seed = attr(x, "seed"),
    metadata = list()
  )
  attach_weight_share_record(result, record)
}

#' Assert that a link table is the complete population register
#'
#' A declarative marker for [share_weights()]'s `multiplicity` argument. It
#' states that `links` holds every link in the source population, not only the
#' links of the sampled units, and so authorizes samplyr to count it.
#'
#' Named for the assertion rather than for the counting it triggers, because
#' the assertion is what a reader has to check.
#'
#' @return `complete_links()` is only meaningful inside
#'   `share_weights(multiplicity = )` and otherwise throws an error.
#'
#' @examples
#' # Used inside share_weights():
#' # share_weights(sample, targets, links, by, to, within = hh,
#' #               multiplicity = complete_links())
#'
#' @family weight sharing
#' @export
complete_links <- function() {
  cli_abort(
    paste0(
      "{.fn complete_links} is a declarative marker and must be used inside ",
      "{.code share_weights(multiplicity = ...)}."
    )
  )
}

#' Weight links by their importance rather than counting them
#'
#' A declarative marker for [share_weights()]'s `multiplicity` argument. It
#' replaces the 0/1 link indicator with a non-negative quantity saying how much
#' of a target unit each link accounts for: a subsidiary's share of an
#' enterprise's assets, a child's share of time with each parent.
#'
#' Lavallee section 4.5 generalizes the method to any such quantity with no
#' loss of theory, subject to Constraint 4.1: the total over each target
#' cluster must be strictly positive.
#'
#' The argument is not called `theta`. Lavallee uses that name for normalized
#' link constants and `survey::multiframe()` uses it for an unrelated
#' frame-compositing factor, and a workflow can involve both.
#'
#' @param x A single non-negative column of `links` holding the importance of
#'   each link.
#' @param total The population total of that importance for each target unit:
#'   a column of `targets`, or [complete_weighted_links()] to assert that
#'   `links` is the complete population register.
#'
#' @return `weighted_links()` is only meaningful inside
#'   `share_weights(multiplicity = )` and otherwise throws an error.
#'
#' @examples
#' # Used inside share_weights():
#' # share_weights(sample, targets, links, by, to, within = enterprise,
#' #               multiplicity = weighted_links(asset_share,
#' #                                             total = population_assets))
#'
#' @family weight sharing
#' @export
weighted_links <- function(x, total) {
  cli_abort(
    paste0(
      "{.fn weighted_links} is a declarative marker and must be used inside ",
      "{.code share_weights(multiplicity = ...)}."
    )
  )
}

#' Assert that a weighted link table is the complete population register
#'
#' A declarative marker for [weighted_links()]'s `total` argument. It is
#' [complete_links()] for link importances rather than link counts: it states
#' that `links` holds every link in the source population, so their importances
#' can be totalled to give the population figure.
#'
#' @return `complete_weighted_links()` is only meaningful inside
#'   `weighted_links(total = )` and otherwise throws an error.
#'
#' @examples
#' # Used inside share_weights():
#' # share_weights(sample, targets, links, by, to, within = enterprise,
#' #               multiplicity = weighted_links(asset_share,
#' #                                             total = complete_weighted_links()))
#'
#' @family weight sharing
#' @export
complete_weighted_links <- function() {
  cli_abort(
    paste0(
      "{.fn complete_weighted_links} is a declarative marker and must be used ",
      "inside {.code weighted_links(total = ...)}."
    )
  )
}

#' Eliminate target clusters by extending links across them
#'
#' A declarative marker for [share_weights()]'s `within` argument. It requests
#' Lavallee section 5.3: a source unit linked to any member of a cluster is
#' treated as linked to all of them, and target units are then treated as
#' their own clusters.
#'
#' This is not a simplification of the clustered method. It produces a
#' different weight, because the extended structure counts source units
#' reaching a cluster where the clustered form counts links into it.
#'
#' @param x A single column of `targets` naming the cluster. The column is
#'   required: the extension is defined over the cluster, so it cannot be
#'   expressed without one.
#'
#' @return `extend_links()` is only meaningful inside
#'   `share_weights(within = )` and otherwise throws an error.
#'
#' @examples
#' # Used inside share_weights():
#' # share_weights(sample, targets, links, by, to,
#' #               within = extend_links(household),
#' #               multiplicity = complete_links())
#'
#' @family weight sharing
#' @export
extend_links <- function(x) {
  cli_abort(
    paste0(
      "{.fn extend_links} is a declarative marker and must be used inside ",
      "{.code share_weights(within = ...)}."
    )
  )
}

## Argument parsing

#' @noRd
parse_within <- function(quo, targets, call = caller_env()) {
  if (rlang::quo_is_missing(quo)) {
    abort_samplyr(
      c(
        "{.arg within} must be given.",
        "i" = "It names the target cluster, and the method assigns one weight
               to every unit of a cluster, which is what makes unit and
               cluster estimates agree.",
        "i" = "Use a bare column of {.arg targets}, {.code NULL} to make every
               target unit its own cluster, or {.fn extend_links}.",
        "i" = "It is never inferred: guessing wrong changes every weight."
      ),
      class = "samplyr_error_share_weights_within",
      call = call
    )
  }

  expr <- quo_get_expr(quo)
  if (is_null(expr)) {
    return(list(mode = "singleton", col = NULL))
  }
  if (is.symbol(expr)) {
    return(list(mode = "cluster", col = as_label(expr)))
  }
  if (is_call(expr, "extend_links", ns = "")) {
    args <- as.list(expr)[-1]
    if (length(args) != 1L || !is.symbol(args[[1]])) {
      abort_samplyr(
        c(
          "{.fn extend_links} takes exactly one bare column name.",
          "i" = "The extension runs over a cluster, so it needs the column
                 that says which cluster a target unit is in."
        ),
        class = "samplyr_error_share_weights_within",
        call = call
      )
    }
    return(list(mode = "extended", col = as_label(args[[1]])))
  }

  abort_samplyr(
    c(
      "{.arg within} must be a column name, {.code NULL}, or
       {.fn extend_links}.",
      "x" = "Got {.code {as_label(expr)}}."
    ),
    class = "samplyr_error_share_weights_within",
    call = call
  )
}

#' @noRd
parse_multiplicity <- function(quo, call = caller_env()) {
  expr <- quo_get_expr(quo)

  if (is_call(expr, "weighted_links", ns = "")) {
    return(parse_weighted_links(expr, call = call))
  }

  if (is_null(expr)) {
    abort_samplyr(
      c(
        "{.arg multiplicity} must be given.",
        "i" = "It is the population number of links to a target unit, summed
               over the whole source population and not over the sample.",
        "i" = "Name a column of {.arg targets} that holds it, or state
               {.code multiplicity = complete_links()} to assert that
               {.arg links} is the complete population register.",
        "i" = "Counting the supplied links without that assertion would make
               it silently, and understate every weight whose links are only
               partly recorded."
      ),
      class = "samplyr_error_share_weights_multiplicity",
      call = call
    )
  }
  if (is.symbol(expr)) {
    return(list(
      mode = "supplied", scale = "binary",
      col = as_label(expr), total_col = NULL
    ))
  }
  if (is_call(expr, "complete_links", ns = "")) {
    if (length(as.list(expr)) != 1L) {
      abort_samplyr(
        "{.fn complete_links} takes no arguments.",
        class = "samplyr_error_share_weights_multiplicity",
        call = call
      )
    }
    return(list(
      mode = "complete_links", scale = "binary",
      col = NULL, total_col = NULL
    ))
  }

  abort_samplyr(
    c(
      "{.arg multiplicity} must be a column of {.arg targets},
       {.fn complete_links}, or {.fn weighted_links}.",
      "x" = "Got {.code {as_label(expr)}}."
    ),
    class = "samplyr_error_share_weights_multiplicity",
    call = call
  )
}

#' Read `weighted_links(col, total = )`
#'
#' Two things have to be established and they are separate: which column of
#' `links` carries the link importance, and where the population total of that
#' importance comes from. The second is the same choice `complete_links()`
#' makes for counts, and it is made the same way, because the bias it guards
#' against is the same one.
#' @noRd
parse_weighted_links <- function(expr, call = caller_env()) {
  args <- as.list(expr)[-1]
  nms <- names(args) %||% rep("", length(args))

  named_total <- which(nms == "total")
  positional <- which(!nzchar(nms))
  value_expr <- if (length(positional) >= 1L) args[[positional[[1]]]] else NULL
  total_expr <- if (length(named_total) == 1L) {
    args[[named_total]]
  } else if (length(positional) >= 2L) {
    args[[positional[[2]]]]
  }

  if (is_null(value_expr) || !is.symbol(value_expr) || is_null(total_expr)) {
    abort_samplyr(
      c(
        "{.fn weighted_links} takes a column of {.arg links} and a
         {.arg total}.",
        "i" = "{.code weighted_links(importance, total = population_total)},
               or {.code total = complete_weighted_links()} to assert that
               {.arg links} is the complete population register."
      ),
      class = "samplyr_error_share_weights_multiplicity",
      call = call
    )
  }

  if (is_call(total_expr, "complete_weighted_links", ns = "")) {
    return(list(
      mode = "complete_weighted_links", scale = "quantitative",
      col = as_label(value_expr), total_col = NULL
    ))
  }
  if (is.symbol(total_expr)) {
    return(list(
      mode = "weighted_links", scale = "quantitative",
      col = as_label(value_expr), total_col = as_label(total_expr)
    ))
  }

  abort_samplyr(
    c(
      "{.arg total} must be a column of {.arg targets} or
       {.fn complete_weighted_links}.",
      "x" = "Got {.code {as_label(total_expr)}}."
    ),
    class = "samplyr_error_share_weights_multiplicity",
    call = call
  )
}

#' @noRd
check_share_table <- function(value, arg, call = caller_env()) {
  if (!is.data.frame(value)) {
    abort_samplyr(
      "{.arg {arg}} must be a data frame.",
      class = "samplyr_error_share_weights_input",
      call = call
    )
  }
  if (nrow(value) == 0L) {
    abort_samplyr(
      c(
        "{.arg {arg}} has no rows.",
        "i" = "There is nothing to share weights with."
      ),
      class = "samplyr_error_share_weights_input",
      call = call
    )
  }
  value
}

#' Read a `c(local = "links_column")` mapping
#'
#' dplyr's join direction, because it is a vocabulary users already have.
#' Inferring keys from shared names is the implicit behavior `frame_ref`
#' already refuses: two tables sharing a column name is not evidence that the
#' column means the same thing in both.
#' @noRd
parse_join_map <- function(map, arg, local, local_label, links,
                           call = caller_env()) {
  nms <- names(map)
  if (
    !is.character(map) || length(map) == 0L ||
      is_null(nms) || !all(nzchar(nms))
  ) {
    abort_samplyr(
      c(
        "{.arg {arg}} must be a named character vector.",
        "i" = "Name the columns as a join does:
               {.code {arg} = c(local_column = \"links_column\")}."
      ),
      class = "samplyr_error_share_weights_keys",
      call = call
    )
  }

  missing_local <- setdiff(nms, names(local))
  if (length(missing_local) > 0) {
    abort_samplyr(
      c(
        "{.arg {arg}} names {cli::qty(length(missing_local))}{?a column/columns}
         {local_label} does not have.",
        "x" = "Missing: {.field {missing_local}}."
      ),
      class = "samplyr_error_share_weights_keys",
      call = call
    )
  }
  missing_link <- setdiff(unname(map), names(links))
  if (length(missing_link) > 0) {
    abort_samplyr(
      c(
        "{.arg {arg}} names {cli::qty(length(missing_link))}{?a column/columns}
         {.arg links} does not have.",
        "x" = "Missing: {.field {missing_link}}."
      ),
      class = "samplyr_error_share_weights_keys",
      call = call
    )
  }
  map
}

## Input validation

#' @noRd
validate_share_inputs <- function(x, targets, links, by, to, within, mult,
                                  call = caller_env()) {
  check_key_types(x, links, by, "by", call = call)
  check_key_types(targets, links, to, "to", call = call)
  check_no_missing_keys(x, names(by), "the sample", call = call)
  check_no_missing_keys(targets, names(to), "targets", call = call)
  check_no_missing_keys(links, unname(c(by, to)), "links", call = call)

  if (anyDuplicated(as.data.frame(links)[unname(c(by, to))]) > 0L) {
    abort_samplyr(
      c(
        "{.arg links} carries duplicate links.",
        "x" = "Some source and target pair appears on more than one row.",
        "i" = "A link counted twice raises that target's numerator and the
               cluster's denominator, so it changes the weights without
               anything reporting it.",
        "i" = "Reduce {.arg links} to distinct pairs."
      ),
      class = "samplyr_error_share_weights_links",
      call = call
    )
  }

  tgt_keys <- make_group_key(as.data.frame(targets), names(to))
  if (anyDuplicated(tgt_keys) > 0L) {
    abort_samplyr(
      c(
        "{.arg to} must identify target rows uniquely.",
        "x" = "{sum(duplicated(tgt_keys))} row{?s} repeat{?s/} a key.",
        "i" = "The weight is attached to a target unit, so two rows with one
               key leave no way to say which unit was meant."
      ),
      class = "samplyr_error_share_weights_keys",
      call = call
    )
  }

  if (!is_null(within$col) && !within$col %in% names(targets)) {
    abort_samplyr(
      c(
        "{.arg within} names {.field {within$col}}, which {.arg targets} does
         not have."
      ),
      class = "samplyr_error_share_weights_within",
      call = call
    )
  }
  if (!is_null(within$col) && anyNA(targets[[within$col]])) {
    abort_samplyr(
      c(
        "{.field {within$col}} has missing values.",
        "i" = "A target unit with no cluster cannot be given the cluster's
               weight."
      ),
      class = "samplyr_error_share_weights_within",
      call = call
    )
  }

  if (identical(mult$mode, "supplied")) {
    check_supplied_multiplicity(targets, mult$col, within, call = call)
  }
  if (identical(mult$scale, "quantitative")) {
    check_weighted_links(targets, links, within, mult, call = call)
  }

  invisible(NULL)
}

#' @noRd
check_weighted_links <- function(targets, links, within, mult,
                                 call = caller_env()) {
  # Section 4.4 defers this combination rather than choosing for the user:
  # extending a link across a cluster has to say what importance the extended
  # link carries, and that normalization is not settled. Running it anyway
  # would produce a number no convention backs.
  if (identical(within$mode, "extended")) {
    abort_samplyr(
      c(
        "{.fn weighted_links} and {.fn extend_links} cannot be combined.",
        "x" = "Extending a link across a cluster has to state what importance
               the extended link carries, and that convention is not fixed.",
        "i" = "Use {.fn extend_links} with counted links, or keep the cluster
               and use {.fn weighted_links} with it."
      ),
      class = "samplyr_error_share_weights_multiplicity",
      call = call
    )
  }

  if (!mult$col %in% names(links)) {
    abort_samplyr(
      "{.fn weighted_links} names {.field {mult$col}}, which {.arg links} does
       not have.",
      class = "samplyr_error_share_weights_multiplicity",
      call = call
    )
  }
  value <- links[[mult$col]]
  if (!is.numeric(value) || anyNA(value) || any(value < 0)) {
    abort_samplyr(
      c(
        "{.field {mult$col}} must be non-negative numbers with no missing
         values.",
        "i" = "It is the importance of a link, and a negative importance would
               subtract one target unit's representation from another's."
      ),
      class = "samplyr_error_share_weights_multiplicity",
      call = call
    )
  }

  if (!is_null(mult$total_col)) {
    if (!mult$total_col %in% names(targets)) {
      abort_samplyr(
        "{.arg total} names {.field {mult$total_col}}, which {.arg targets}
         does not have.",
        class = "samplyr_error_share_weights_multiplicity",
        call = call
      )
    }
    total <- targets[[mult$total_col]]
    if (!is.numeric(total) || anyNA(total) || any(total < 0)) {
      abort_samplyr(
        c(
          "{.field {mult$total_col}} must be non-negative numbers with no
           missing values.",
          "i" = "It is the population total of the link importances."
        ),
        class = "samplyr_error_share_weights_multiplicity",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' @noRd
check_supplied_multiplicity <- function(targets, col, within,
                                        call = caller_env()) {
  if (!col %in% names(targets)) {
    abort_samplyr(
      "{.arg multiplicity} names {.field {col}}, which {.arg targets} does not
       have.",
      class = "samplyr_error_share_weights_multiplicity",
      call = call
    )
  }
  value <- targets[[col]]
  if (!is.numeric(value) || anyNA(value) || any(value < 0)) {
    abort_samplyr(
      c(
        "{.field {col}} must be non-negative numbers with no missing values.",
        "i" = "It is a count of links in the source population."
      ),
      class = "samplyr_error_share_weights_multiplicity",
      call = call
    )
  }

  # Under cluster elimination the supplied value is the multiplicity of the
  # extended structure, which is a property of the cluster. A value varying
  # inside a cluster is therefore the un-extended one, and using it would
  # silently compute neither method.
  if (identical(within$mode, "extended")) {
    cl <- make_group_key(as.data.frame(targets), within$col)
    varies <- tapply(value, cl, function(v) length(unique(v)) > 1L)
    if (any(varies)) {
      abort_samplyr(
        c(
          "{.field {col}} varies inside a target cluster.",
          "x" = "{sum(varies)} cluster{?s} carr{?ies/y} more than one value.",
          "i" = "With {.fn extend_links} the multiplicity is that of the
                 extended structure, in which every unit of a cluster has the
                 same links, so it is constant within the cluster.",
          "i" = "Supply the extended multiplicity, or use
                 {.fn complete_links} and let samplyr extend before counting."
        ),
        class = "samplyr_error_share_weights_multiplicity",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' @noRd
check_key_types <- function(local, links, map, arg, call = caller_env()) {
  for (i in seq_along(map)) {
    lhs <- local[[names(map)[[i]]]]
    rhs <- links[[unname(map)[[i]]]]
    if (!share_key_types_compatible(lhs, rhs)) {
      abort_samplyr(
        c(
          "{.arg {arg}} matches columns of different types.",
          "x" = "{.field {names(map)[[i]]}} is {.cls {class(lhs)[[1]]}} and
                 {.field {unname(map)[[i]]}} is {.cls {class(rhs)[[1]]}}.",
          "i" = "Keys are compared as values, so a number and the text of that
                 number are different units."
        ),
        class = "samplyr_error_share_weights_keys",
        call = call
      )
    }
  }
  invisible(NULL)
}

#' @noRd
share_key_types_compatible <- function(lhs, rhs) {
  numeric_like <- function(v) is.numeric(v) && !is.factor(v)
  if (numeric_like(lhs) && numeric_like(rhs)) {
    return(TRUE)
  }
  # A factor and its labels compare as the same values once keyed, so the
  # useful distinction is character-like against everything else.
  chr_like <- function(v) is.character(v) || is.factor(v)
  if (chr_like(lhs) && chr_like(rhs)) {
    return(TRUE)
  }
  identical(class(lhs), class(rhs))
}

#' @noRd
check_no_missing_keys <- function(data, cols, label, call = caller_env()) {
  bad <- cols[vapply(cols, function(cl) anyNA(data[[cl]]), logical(1))]
  if (length(bad) == 0) {
    return(invisible(NULL))
  }
  abort_samplyr(
    c(
      "Key columns of {label} have missing values.",
      "x" = "Missing in {.field {bad}}.",
      "i" = "A missing key matches nothing, so the rows carrying it would
             drop out of the transformation without being reported."
    ),
    class = "samplyr_error_share_weights_keys",
    call = call
  )
}

## The method itself

#' Steps 1 to 4 of the generalized weight share method
#'
#' Returns the operator, the two link columns over all target rows, which
#' target rows the result keeps, and the coverage counts. The weight itself is
#' left to `apply_share_operator()`, so the map and the arithmetic stay
#' separable and the same map serves every replicate.
#' @noRd
gwsm_compute <- function(x, targets, links, by, to, within, mult,
                         call = caller_env()) {
  n_src <- nrow(x)
  n_tgt <- nrow(targets)

  src_key_sample <- make_group_key(as.data.frame(x), names(by))
  src_key_links <- make_group_key(as.data.frame(links), unname(by))
  tgt_key_targets <- make_group_key(as.data.frame(targets), names(to))
  tgt_key_links <- make_group_key(as.data.frame(links), unname(to))

  cluster_key <- if (identical(within$mode, "singleton")) {
    tgt_key_targets
  } else {
    make_group_key(as.data.frame(targets), within$col)
  }
  cl_index <- vctrs::vec_group_loc(cluster_key)
  cluster_of_target <- match(cluster_key, cl_index$key)
  n_clusters <- nrow(cl_index)

  link_tgt <- match(tgt_key_links, tgt_key_targets)
  src_index <- vctrs::vec_group_loc(src_key_sample)
  link_src_grp <- match(src_key_links, src_index$key)

  # A selected source unit linked to a target the register does not contain
  # means the register is not the roster it claims to be. Silently dropping
  # the link would understate that cluster's weight.
  orphaned <- !is.na(link_src_grp) & is.na(link_tgt)
  if (any(orphaned)) {
    abort_samplyr(
      c(
        "{.arg links} reaches target units {.arg targets} does not contain.",
        "x" = "{sum(orphaned)} link{?s} from selected source units name{?s/} a
               target that is not in the register.",
        "i" = "Every target a selected unit reaches has to be in
               {.arg targets}, or its cluster is returned without all of its
               members."
      ),
      class = "samplyr_error_share_weights_coverage",
      call = call
    )
  }

  # The importance of each link. Counting is the case where every link counts
  # for one, so the binary and quantitative methods run the same code and the
  # binary one is not a separate path that could drift from it.
  link_value <- if (identical(mult$scale, "quantitative")) {
    as.numeric(links[[mult$col]])
  } else {
    rep(1, nrow(links))
  }

  population <- gwsm_population_links(
    links, link_tgt, link_value, src_key_links, cluster_of_target,
    n_tgt, n_clusters, targets, within, mult
  )

  contrib <- gwsm_contributions(
    link_tgt, link_src_grp, link_value, src_index,
    cluster_of_target, cl_index, within
  )

  reached <- unique(contrib$cluster)
  kept <- cluster_of_target %in% reached

  # Expand cluster-level contributions to every target unit of the cluster.
  # This is step 4, and it is what makes the weight constant within cluster.
  units_of <- cl_index$loc[contrib$cluster]
  reps <- lengths(units_of)
  target_row <- unlist(units_of, use.names = FALSE)
  source_row <- rep(contrib$source, reps)
  numer <- rep(contrib$count, reps)

  denom <- population$denominator[target_row]
  if (any(denom <= 0)) {
    abort_samplyr(
      c(
        "A reached target cluster has no links in the source population.",
        "x" = "{sum(denom <= 0)} contribution{?s} would divide by zero.",
        "i" = if (identical(mult$scale, "quantitative")) {
          "Constraint 4.1: the total link importance over a target cluster
           must be strictly positive."
        } else {
          "The sample reached this cluster, so its population link total
           cannot be zero."
        },
        "i" = "The supplied total disagrees with {.arg links}: the sample
               reached this cluster, so something links to it."
      ),
      class = "samplyr_error_share_weights_multiplicity",
      call = call
    )
  }

  # Row positions of the kept targets, so the operator spans the result
  # rather than the register.
  position <- cumsum(kept)
  operator <- new_share_operator(
    target_row = position[target_row],
    source_row = source_row,
    share = numer / denom,
    n_target = sum(kept),
    n_source = n_src
  )

  orphan_clusters <- cl_index$key[population$cluster_total <= 0]

  list(
    operator = operator,
    unit_links = population$unit,
    cluster_links = population$denominator,
    kept = kept,
    n_clusters = n_clusters,
    n_reached = length(reached),
    orphan_clusters = orphan_clusters,
    # One value whatever the register's size, which is what makes it storable
    # for a national register. Two transformations describe the same target
    # clusters when their digests agree, and one component's silence about a
    # cluster is evidence of coverage only then.
    cluster_digest = rlang::hash(sort(cl_index$key))
  )
}

#' Population link totals: step 2, and the one that is not about the sample
#'
#' The denominator sums over the whole source population. Every branch here is
#' either reading what the user supplied or counting a table they asserted is
#' complete.
#' @noRd
gwsm_population_links <- function(links, link_tgt, link_value, src_key_links,
                                  cluster_of_target, n_tgt, n_clusters,
                                  targets, within, mult) {
  known <- !is.na(link_tgt)

  if (identical(within$mode, "extended")) {
    # Cluster elimination counts the source units reaching a cluster, not the
    # links into it: after extension every unit of the cluster has exactly
    # those units as its links.
    unit <- if (identical(mult$mode, "supplied")) {
      as.numeric(targets[[mult$col]])
    } else {
      cl_of_link <- cluster_of_target[link_tgt[known]]
      pairs <- unique(data.frame(
        cluster = cl_of_link,
        source = src_key_links[known],
        stringsAsFactors = FALSE
      ))
      per_cluster <- tabulate(pairs$cluster, nbins = n_clusters)
      per_cluster[cluster_of_target]
    }
    # Every unit of a cluster carries the cluster's own total, so the unit
    # multiplicity and the denominator are the same number here.
    return(list(unit = unit, denominator = unit,
                cluster_total = cluster_totals(unit, cluster_of_target,
                                               n_clusters, mean_within = TRUE)))
  }

  supplied_col <- switch(
    mult$mode,
    supplied = mult$col,
    weighted_links = mult$total_col,
    NULL
  )
  unit <- if (!is_null(supplied_col)) {
    as.numeric(targets[[supplied_col]])
  } else {
    # Summing the link importances over the asserted-complete register. With
    # unit importances this is exactly the count.
    totals <- numeric(n_tgt)
    if (any(known)) {
      agg <- rowsum(link_value[known], group = link_tgt[known], reorder = TRUE)
      totals[as.integer(rownames(agg))] <- agg
    }
    totals
  }
  total <- cluster_totals(unit, cluster_of_target, n_clusters)
  list(unit = unit, denominator = total[cluster_of_target],
       cluster_total = total)
}

#' @noRd
cluster_totals <- function(unit, cluster_of_target, n_clusters,
                           mean_within = FALSE) {
  if (mean_within) {
    # Constant within cluster by construction, so any member states it.
    out <- numeric(n_clusters)
    out[cluster_of_target] <- unit
    return(out)
  }
  as.numeric(rowsum(unit, group = cluster_of_target, reorder = TRUE)[
    match(seq_len(n_clusters), sort(unique(cluster_of_target)))
  ])
}

#' Numerator counts: which sample rows reach which target cluster, how often
#' @noRd
gwsm_contributions <- function(link_tgt, link_src_grp, link_value, src_index,
                               cluster_of_target, cl_index, within) {
  usable <- !is.na(link_tgt) & !is.na(link_src_grp)
  if (!any(usable)) {
    return(list(cluster = integer(0), source = integer(0),
                count = numeric(0)))
  }

  cl <- cluster_of_target[link_tgt[usable]]
  grp <- link_src_grp[usable]
  value <- link_value[usable]

  if (identical(within$mode, "extended")) {
    # Presence, not multiplicity: a source unit linked anywhere in the cluster
    # is linked to all of it exactly once.
    keep <- !duplicated(data.frame(cl = cl, grp = grp))
    cl <- cl[keep]
    grp <- grp[keep]
    value <- value[keep]
  }

  # One sample row per selected occurrence of a source unit. A with-
  # replacement design selects a unit more than once and each selection
  # carries its own weight, so each is its own contribution.
  rows <- src_index$loc[grp]
  reps <- lengths(rows)
  long <- data.frame(
    cluster = rep(cl, reps),
    source = unlist(rows, use.names = FALSE)
  )
  long_value <- rep(value, reps)

  agg <- vctrs::vec_group_loc(long)
  order_in_groups <- unlist(agg$loc, use.names = FALSE)
  group_id <- rep(seq_len(nrow(agg)), lengths(agg$loc))
  list(
    cluster = agg$key$cluster,
    source = agg$key$source,
    count = as.numeric(rowsum(long_value[order_in_groups], group = group_id))
  )
}
