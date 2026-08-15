## C0. The recorded operator, the transformation record, and the weight contract

# Nothing here goes through share_weights(), which does not exist yet. These
# are the machinery's own guarantees: the arithmetic of the sparse map, the
# record's version discipline, integrity of a transformed sample, and the
# contract that tells consumers what .weight holds.

## Helpers

# A three-source, four-target map, and the target table it produces. Targets 3
# and 4 are one cluster and take the same weight, which is the case a weight
# assigned at unit level would get wrong: target 4 has no link of its own.
demo_operator <- function() {
  new_share_operator(
    target_row = c(1L, 1L, 2L, 3L, 4L),
    source_row = c(1L, 2L, 2L, 3L, 3L),
    share = c(0.5, 0.5, 1.0, 0.25, 0.25),
    n_target = 4L,
    n_source = 3L
  )
}

# The same map with the cluster's second unit dropped, so target 4 is reached
# by nothing. A dense-matrix implementation gets that row right by accident;
# a scatter can get it wrong.
sparse_operator <- function() {
  new_share_operator(
    target_row = c(1L, 1L, 2L, 3L),
    source_row = c(1L, 2L, 2L, 3L),
    share = c(0.5, 0.5, 1.0, 0.25),
    n_target = 4L,
    n_source = 3L
  )
}

demo_record <- function(result, source_sample, op = demo_operator()) {
  new_weight_share_record(
    operator = op,
    source_sample = source_sample,
    source_integrity = weight_share_integrity_record(
      source_sample, "src_id", character(0)
    ),
    source_key_cols = "src_id",
    target_key_cols = "tgt_id",
    target_cluster = "hh_id",
    within_mode = "cluster",
    denominator = list(mode = "supplied", scale = "binary"),
    coverage = new_weight_share_coverage(
      target_scope = "reached",
      n_target_units = nrow(result),
      n_target_clusters = 3L,
      n_reached_clusters = 3L,
      n_unlinked_units = 1L
    ),
    generated_cols = weight_share_generated_cols$binary,
    call_info = list(fn = "share_weights")
  )
}

# The record as an exporter sees it: attached to its result, so the two
# result-describing fields are filled.
demo_attached <- function() {
  attr(
    attach_weight_share_record(
      demo_target(), demo_record(demo_target(), demo_source())
    ),
    "metadata"
  )$weight_share
}

demo_source <- function() {
  tibble::tibble(
    src_id = c("a", "b", "c"),
    .weight = c(10, 20, 40),
    .sample_id = 1:3
  )
}

demo_target <- function() {
  tibble::tibble(
    tgt_id = c("t1", "t2", "t3", "t4"),
    hh_id = c("h1", "h2", "h3", "h3"),
    .weight = c(15, 20, 10, 10),
    .unit_links = c(2L, 1L, 1L, 0L),
    .cluster_links = c(2L, 1L, 1L, 1L)
  )
}

## Operator arithmetic

test_that("the operator reproduces a hand-computed weight vector", {
  op <- demo_operator()
  w <- c(10, 20, 40)

  # 1: 0.5*10 + 0.5*20 = 15; 2: 1*20 = 20; 3 and 4: 0.25*40 = 10 each, the
  # cluster weight assigned to both of its units.
  expect_identical(apply_share_operator(op, w), c(15, 20, 10, 10))
})

test_that("a target row no entry names receives zero, not a missing value", {
  op <- sparse_operator()
  out <- apply_share_operator(op, c(10, 20, 40))

  expect_false(anyNA(out))
  expect_identical(out[[4]], 0)
  expect_identical(share_operator_unreached(op), 4L)
})

test_that("triplet multiplication equals dense matrix multiplication", {
  op <- demo_operator()
  dense <- matrix(0, nrow = op$n_target, ncol = op$n_source)
  for (i in seq_along(op$share)) {
    dense[op$target_row[i], op$source_row[i]] <- op$share[i]
  }
  w <- c(3.5, -0.25, 11)

  expect_equal(apply_share_operator(op, w), as.numeric(dense %*% w))
})

test_that("applying to a replicate matrix equals applying column by column", {
  op <- demo_operator()
  reps <- matrix(
    c(10, 20, 40, 0, 20, 40, 10, 40, 40, 30, 20, 5),
    nrow = 3
  )
  colnames(reps) <- paste0("rep", 1:4)

  shared <- apply_share_operator(op, reps)
  by_column <- vapply(
    seq_len(ncol(reps)),
    function(j) apply_share_operator(op, reps[, j]),
    numeric(op$n_target)
  )

  expect_identical(dim(shared), c(4L, 4L))
  expect_identical(colnames(shared), colnames(reps))
  expect_equal(unname(shared), by_column)
})

test_that("a missing source weight propagates rather than aggregating to zero", {
  op <- demo_operator()
  out <- apply_share_operator(op, c(10, NA, 40))

  # Targets 1 and 2 draw on source 2; targets 3 and 4 do not.
  expect_identical(is.na(out), c(TRUE, TRUE, FALSE, FALSE))
})

test_that("the scatter lands by target index, not by aggregation order", {
  # Built by hand, deliberately not canonicalized: a record assembled outside
  # the constructor, or an aggregation that groups by first appearance rather
  # than by sorted order, must still put each total on the row it belongs to.
  # An implementation that scattered positionally would pass on canonical
  # input and be wrong here.
  op <- list(
    target_row = c(4L, 1L, 4L, 2L),
    source_row = c(1L, 2L, 3L, 1L),
    share = c(1, 1, 1, 1),
    n_target = 4L,
    n_source = 3L
  )

  # 1: 200; 2: 100; 3: unreached; 4: 100 + 400 = 500.
  expect_identical(
    apply_share_operator(op, c(100, 200, 400)),
    c(200, 100, 0, 500)
  )
})

test_that("an empty operator maps to zeros over its declared target rows", {
  op <- new_share_operator(
    target_row = integer(0),
    source_row = integer(0),
    share = numeric(0),
    n_target = 3L,
    n_source = 2L
  )

  expect_identical(apply_share_operator(op, c(5, 6)), c(0, 0, 0))
  expect_identical(share_operator_unreached(op), 1:3)
})

test_that("weights of the wrong length are refused rather than recycled", {
  op <- demo_operator()

  expect_error(
    apply_share_operator(op, c(10, 20)),
    class = "samplyr_error_share_operator_misaligned"
  )
  expect_error(
    apply_share_operator(op, matrix(1, nrow = 4, ncol = 2)),
    class = "samplyr_error_share_operator_misaligned"
  )
})

## Operator construction

test_that("triplets are canonicalized so build order does not change the record", {
  forward <- demo_operator()
  shuffled <- new_share_operator(
    target_row = c(4L, 1L, 3L, 2L, 1L),
    source_row = c(3L, 2L, 3L, 2L, 1L),
    share = c(0.25, 0.5, 0.25, 1.0, 0.5),
    n_target = 4L,
    n_source = 3L
  )

  expect_identical(forward, shuffled)
  expect_identical(rlang::hash(forward), rlang::hash(shuffled))
  expect_identical(forward$target_row, c(1L, 1L, 2L, 3L, 4L))
  expect_identical(forward$source_row, c(1L, 2L, 2L, 3L, 3L))
})

test_that("a repeated target-source pair is refused, not summed silently", {
  expect_error(
    new_share_operator(
      target_row = c(1L, 1L),
      source_row = c(2L, 2L),
      share = c(0.5, 0.5),
      n_target = 2L,
      n_source = 2L
    ),
    class = "samplyr_error_share_operator_duplicate"
  )
})

test_that("distinct sources for one target are not mistaken for duplicates", {
  op <- new_share_operator(
    target_row = c(1L, 1L),
    source_row = c(1L, 2L),
    share = c(0.5, 0.5),
    n_target = 1L,
    n_source = 2L
  )

  expect_identical(apply_share_operator(op, c(4, 6)), 5)
})

test_that("malformed coefficients and out-of-range indices are refused", {
  expect_error(
    new_share_operator(1L, 1L, NA_real_, 1L, 1L),
    class = "samplyr_error_share_operator_malformed"
  )
  expect_error(
    new_share_operator(1L, 1L, Inf, 1L, 1L),
    class = "samplyr_error_share_operator_malformed"
  )
  expect_error(
    new_share_operator(1L, 1L, -0.5, 1L, 1L),
    class = "samplyr_error_share_operator_malformed"
  )
  expect_error(
    new_share_operator(1.5, 1L, 1, 2L, 1L),
    class = "samplyr_error_share_operator_malformed"
  )
  expect_error(
    new_share_operator(3L, 1L, 1, 2L, 1L),
    class = "samplyr_error_share_operator_malformed"
  )
  expect_error(
    new_share_operator(1L, 4L, 1, 2L, 2L),
    class = "samplyr_error_share_operator_malformed"
  )
  expect_error(
    new_share_operator(c(1L, 2L), 1L, 1, 2L, 1L),
    class = "samplyr_error_share_operator_malformed"
  )
})

test_that("n_target is taken as declared, not inferred from the entries", {
  op <- new_share_operator(1L, 1L, 1, n_target = 5L, n_source = 1L)

  expect_identical(op$n_target, 5L)
  expect_length(apply_share_operator(op, 7), 5L)
})

## Record version discipline

test_that("a complete attached record round-trips through the reader", {
  attached <- attr(
    attach_weight_share_record(
      demo_target(), demo_record(demo_target(), demo_source())
    ),
    "metadata"
  )$weight_share

  expect_identical(attached$algorithm, "generalized_weight_share")
  expect_identical(attached$version, 1L)
  expect_identical(prepare_weight_share_record(attached, "A test"), attached)
  expect_null(prepare_weight_share_record(NULL, "A test"))
})

test_that("a record that never reached its result is refused by the reader", {
  unattached <- demo_record(demo_target(), demo_source())

  expect_null(unattached$result_integrity)
  expect_null(unattached$target_row_keys)
  expect_error(
    prepare_weight_share_record(unattached, "A test"),
    class = "samplyr_error_weight_share_record_malformed"
  )
})

test_that("a record naming another algorithm or version is refused", {
  record <- demo_attached()

  record$algorithm <- "cluster_elimination"
  expect_error(
    prepare_weight_share_record(record, "A test"),
    class = "samplyr_error_weight_share_record_malformed"
  )

  record <- demo_attached()
  record$version <- 2L
  expect_error(
    prepare_weight_share_record(record, "A test"),
    class = "samplyr_error_weight_share_record_unsupported"
  )

  # A version below any that existed is as unreadable as one from the future.
  record$version <- 0L
  expect_error(
    prepare_weight_share_record(record, "A test"),
    class = "samplyr_error_weight_share_record_unsupported"
  )
})

test_that("a bare value where the record belongs is refused before any field is read", {
  expect_error(
    prepare_weight_share_record("generalized_weight_share", "A test"),
    class = "samplyr_error_weight_share_record_malformed"
  )
})

test_that("every declared record field is required", {
  complete <- demo_attached()

  for (field in weight_share_record_fields) {
    truncated <- complete
    truncated[[field]] <- NULL
    # The algorithm and version are established before any other field is
    # read, so their absence is diagnosed by that step rather than by the
    # field check. A missing version is unreadable in the same way a future
    # one is, and reports itself that way.
    expected <- if (identical(field, "version")) {
      "samplyr_error_weight_share_record_unsupported"
    } else {
      "samplyr_error_weight_share_record_malformed"
    }
    expect_error(
      prepare_weight_share_record(truncated, "A test"),
      class = expected,
      info = field
    )
  }
})

test_that("target_cluster is required by name but may be NULL for singletons", {
  record <- demo_attached()
  record$within_mode <- "singleton"
  record$target_cluster <- NULL

  expect_error(
    prepare_weight_share_record(record, "A test"),
    class = "samplyr_error_weight_share_record_malformed"
  )

  record["target_cluster"] <- list(NULL)
  expect_identical(prepare_weight_share_record(record, "A test"), record)
})

test_that("record modes are checked against the sets this build reads", {
  base <- demo_attached()

  bad_within <- base
  bad_within$within_mode <- "eliminated"
  expect_error(
    prepare_weight_share_record(bad_within, "A test"),
    class = "samplyr_error_weight_share_record_malformed"
  )

  bad_mode <- base
  bad_mode$denominator$mode <- "count_links"
  expect_error(
    prepare_weight_share_record(bad_mode, "A test"),
    class = "samplyr_error_weight_share_record_malformed"
  )

  bad_scale <- base
  bad_scale$denominator$scale <- "probability"
  expect_error(
    prepare_weight_share_record(bad_scale, "A test"),
    class = "samplyr_error_weight_share_record_malformed"
  )

  bad_scope <- base
  bad_scope$coverage$target_scope <- "sample"
  expect_error(
    prepare_weight_share_record(bad_scope, "A test"),
    class = "samplyr_error_weight_share_record_malformed"
  )
})

test_that("a record carrying no usable operator is refused", {
  record <- demo_attached()
  record$operator <- list(target_row = 1L, source_row = 1L)

  expect_error(
    prepare_weight_share_record(record, "A test"),
    class = "samplyr_error_weight_share_record_malformed"
  )
})

test_that("the three modes and both scales are all readable", {
  base <- demo_attached()

  for (mode in weight_share_within_modes) {
    record <- base
    record$within_mode <- mode
    expect_no_error(prepare_weight_share_record(record, "A test"))
  }
  for (scale in weight_share_denominator_scales) {
    record <- base
    record$denominator$scale <- scale
    expect_no_error(prepare_weight_share_record(record, "A test"))
  }
  for (scope in weight_share_target_scopes) {
    record <- base
    record$coverage$target_scope <- scope
    expect_no_error(prepare_weight_share_record(record, "A test"))
  }
})

## Coverage record

test_that("an unasked coverage question is distinguishable from a negative answer", {
  reached <- new_weight_share_coverage(
    target_scope = "reached",
    n_target_units = 4L, n_target_clusters = 3L,
    n_reached_clusters = 3L, n_unlinked_units = 1L
  )
  population <- new_weight_share_coverage(
    target_scope = "population",
    n_target_units = 4L, n_target_clusters = 3L,
    n_reached_clusters = 3L, n_unlinked_units = 1L,
    orphan_clusters = character(0)
  )

  expect_null(reached$orphan_clusters)
  expect_identical(population$orphan_clusters, character(0))
})

## Integrity of a transformed sample

test_that("attaching a record mints a fresh integrity record and clears source marks", {
  result <- demo_target()
  attr(result, "metadata") <- list(
    integrity = list(n_rows = 3L, cols = ".weight", hash = "stale"),
    modified = "rows"
  )
  record <- demo_record(result, demo_source())
  out <- attach_weight_share_record(result, record)
  meta <- attr(out, "metadata")

  expect_identical(sample_modifications(out), character(0))
  expect_identical(meta$integrity$n_rows, 4L)
  expect_identical(meta$integrity, meta$weight_share$result_integrity)
  expect_identical(verify_weight_share_alignment(out, meta$weight_share), "ok")
})

test_that("the target key and generated link columns are protected", {
  cols <- weight_share_protected_cols(
    demo_target(), "tgt_id", weight_share_generated_cols$binary
  )

  expect_true(all(
    c(".weight", "tgt_id", ".unit_links", ".cluster_links") %in% cols
  ))
  # The cluster column is not protected: it is user data the transformation
  # read, not a quantity it produced or addresses rows by.
  expect_false("hh_id" %in% cols)
})

test_that("altering the result, the retained source, or the span is each detected", {
  result <- attach_weight_share_record(
    demo_target(), demo_record(demo_target(), demo_source())
  )
  record <- attr(result, "metadata")$weight_share

  altered_result <- result
  altered_result$.weight[2] <- 999
  expect_identical(
    verify_weight_share_alignment(altered_result, record), "result"
  )

  dropped_row <- result[-1, ]
  expect_identical(
    verify_weight_share_alignment(dropped_row, record), "dimensions"
  )

  altered_key <- result
  altered_key$tgt_id[1] <- "elsewhere"
  expect_identical(
    verify_weight_share_alignment(altered_key, record), "result"
  )

  altered_source <- record
  altered_source$source_sample$.weight[1] <- 999
  expect_identical(
    verify_weight_share_alignment(result, altered_source), "source"
  )

  wrong_span <- record
  wrong_span$operator$n_source <- 2L
  expect_identical(
    verify_weight_share_alignment(result, wrong_span), "dimensions"
  )
})

test_that("reordering is reported as its own finding, not as tampering", {
  result <- attach_weight_share_record(
    demo_target(), demo_record(demo_target(), demo_source())
  )
  record <- attr(result, "metadata")$weight_share
  reordered <- result[c(2, 1, 4, 3), ]

  # protected_values_hash() is order-invariant by design, so the integrity
  # record alone cannot see a permutation. The operator is positional, so the
  # permutation still matters and has to be found somewhere.
  expect_identical(
    verify_sample_integrity(reordered, record$result_integrity), "ok"
  )
  expect_identical(verify_weight_share_alignment(reordered, record), "reordered")

  # Recovered rather than refused: an arrange() on a transformed sample is an
  # ordinary thing to do.
  expect_silent(check_weight_share_alignment(reordered, "as_svrepdesign"))
})

test_that("key realignment restores the recorded row order", {
  result <- attach_weight_share_record(
    demo_target(), demo_record(demo_target(), demo_source())
  )
  record <- attr(result, "metadata")$weight_share
  reordered <- result[c(3, 1, 4, 2), ]

  pos <- align_share_rows(reordered, record, "as_svrepdesign")

  expect_identical(pos, c(2L, 4L, 1L, 3L))
  expect_identical(reordered$tgt_id[pos], result$tgt_id)
  # The recorded operator reproduces the weights once the rows are back in the
  # order it was recorded in, and does not before.
  recomputed <- apply_share_operator(
    record$operator, record$source_sample$.weight
  )
  expect_identical(recomputed, reordered$.weight[pos])
  expect_false(identical(recomputed, reordered$.weight))
})

test_that("realignment refuses a row set that is not the recorded one", {
  result <- attach_weight_share_record(
    demo_target(), demo_record(demo_target(), demo_source())
  )
  record <- attr(result, "metadata")$weight_share

  renamed <- result
  renamed$tgt_id[2] <- "elsewhere"
  expect_error(
    align_share_rows(renamed, record, "as_svrepdesign"),
    class = "samplyr_error_weight_share_misaligned"
  )

  duplicated_key <- result
  duplicated_key$tgt_id <- c("t1", "t1", "t3", "t4")
  expect_error(
    align_share_rows(duplicated_key, record, "as_svrepdesign"),
    class = "samplyr_error_weight_share_misaligned"
  )
})

test_that("a target key naming two rows is refused when the record is attached", {
  ambiguous <- demo_target()
  ambiguous$tgt_id <- c("t1", "t1", "t3", "t4")

  expect_error(
    attach_weight_share_record(
      ambiguous, demo_record(ambiguous, demo_source())
    ),
    class = "samplyr_error_weight_share_duplicate_key"
  )
})

test_that("row keys survive a value containing the key encoder's separator", {
  targets <- demo_target()
  targets$tgt_id <- c("a|b", "a", "b", "c")
  targets$part <- c("c", "b|c", "x", "y")
  record <- demo_record(targets, demo_source())
  record$target_key_cols <- c("tgt_id", "part")

  keys <- share_row_keys(targets, c("tgt_id", "part"))

  # "a|b" + "c" and "a" + "b|c" must not encode to the same key.
  expect_identical(anyDuplicated(keys), 0L)
})

test_that("check_weight_share_alignment passes an ordinary sample and refuses a broken one", {
  ordinary <- demo_target()
  expect_silent(check_weight_share_alignment(ordinary, "as_svrepdesign"))

  result <- attach_weight_share_record(
    demo_target(), demo_record(demo_target(), demo_source())
  )
  expect_silent(check_weight_share_alignment(result, "as_svrepdesign"))

  broken <- result
  broken$.weight[1] <- 0
  expect_error(
    check_weight_share_alignment(broken, "as_svrepdesign"),
    class = "samplyr_error_weight_share_misaligned"
  )
})

## The weight contract

test_that("the contract is read from the record and nothing else", {
  ordinary <- demo_target()
  expect_identical(sample_weight_contract(ordinary), "design")

  shared <- attach_weight_share_record(
    demo_target(), demo_record(demo_target(), demo_source())
  )
  expect_identical(sample_weight_contract(shared), "shared")
})

test_that("an ordinary sample passes the design-weight contract", {
  expect_silent(check_weight_contract(demo_target(), "joint_expectation"))
})

test_that("a shared-weight sample is refused with its own class and the family class", {
  shared <- attach_weight_share_record(
    demo_target(), demo_record(demo_target(), demo_source())
  )

  expect_error(
    check_weight_contract(
      shared, "joint_expectation",
      class = "samplyr_error_joint_weight_contract"
    ),
    class = "samplyr_error_joint_weight_contract"
  )
  expect_error(
    check_weight_contract(
      shared, "joint_expectation",
      class = "samplyr_error_joint_weight_contract"
    ),
    class = "samplyr_error_weight_contract"
  )
  # The tampering gate is a separate finding and must not be raised here.
  expect_error(
    check_weight_contract(shared, "joint_expectation"),
    class = "samplyr_error_weight_contract"
  )
  expect_false(inherits(
    tryCatch(
      check_weight_contract(shared, "joint_expectation"),
      condition = function(e) e
    ),
    "samplyr_error_modified_sample"
  ))
})

test_that("an operation defined for shared weights accepts both contracts", {
  shared <- attach_weight_share_record(
    demo_target(), demo_record(demo_target(), demo_source())
  )

  expect_silent(check_weight_contract(
    shared, "as_svrepdesign",
    allowed = c("design", "shared")
  ))
  expect_silent(check_weight_contract(
    demo_target(), "as_svrepdesign",
    allowed = c("design", "shared")
  ))
})

test_that("caller advice reaches the message", {
  shared <- attach_weight_share_record(
    demo_target(), demo_record(demo_target(), demo_source())
  )

  expect_error(
    check_weight_contract(
      shared, "as_svydesign",
      advice = c("i" = "Use as_svrepdesign() instead.")
    ),
    regexp = "as_svrepdesign"
  )
})

## Generated column collisions

test_that("a generated name the target data already uses is refused", {
  targets <- tibble::tibble(person_id = 1:3, .unit_links = 1:3)

  expect_error(
    check_generated_cols(
      targets, weight_share_generated_cols$binary, "share_weights"
    ),
    class = "samplyr_error_generated_column_collision"
  )
})

test_that("the refusal names every colliding column, not the first", {
  targets <- tibble::tibble(
    person_id = 1:3, .unit_links = 1:3, .cluster_links = 1:3
  )

  expect_error(
    check_generated_cols(
      targets, weight_share_generated_cols$binary, "share_weights"
    ),
    regexp = "\\.unit_links.*\\.cluster_links"
  )
})

test_that("only the pair belonging to the mode in use can collide", {
  targets <- tibble::tibble(person_id = 1:3, .link_weight = 1)

  expect_silent(check_generated_cols(
    targets, weight_share_generated_cols$binary, "share_weights"
  ))
  expect_error(
    check_generated_cols(
      targets, weight_share_generated_cols$quantitative, "share_weights"
    ),
    class = "samplyr_error_generated_column_collision"
  )
})
