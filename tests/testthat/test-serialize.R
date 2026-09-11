# Coverage for as.list.sampling_design(): exercises the optional-field
# branches (labels, strata variance/cost columns, clusters, data-frame n/frac,
# control, on_empty) that the round-trip serialization records.

test_that("as.list records labels, strata aux columns, and clusters", {
  strata <- c("A", "B", "C", "D")
  design <- sampling_design(title = "Multi") |>
    add_stage(label = "PSU") |>
    stratify_by(
      stratum,
      alloc = "optimal",
      variance = data.frame(stratum = strata, var = c(1.2, 0.8, 1, 1)),
      cost = data.frame(stratum = strata, cost = c(1, 2, 1, 2))
    ) |>
    cluster_by(cluster) |>
    draw(n = 6, method = "pps_brewer", mos = size)

  out <- as.list(design)
  expect_equal(out$title, "Multi")
  stage <- out$stages[[1]]
  expect_equal(stage$label, "PSU")
  expect_equal(stage$strata$alloc, "optimal")
  expect_true("var" %in% stage$strata$variance_columns)
  expect_true("cost" %in% stage$strata$cost_columns)
  expect_equal(stage$clusters$vars, "cluster")
  expect_equal(stage$draw$method, "pps_brewer")
})

test_that("as.list records a custom data-frame n", {
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = data.frame(stratum = c("A", "B"), n = c(5L, 5L)))

  out <- as.list(design)
  expect_equal(out$stages[[1]]$draw$n, "custom (data frame)")
  expect_true("n" %in% out$stages[[1]]$draw$n_columns)
})

test_that("as.list records a custom data-frame frac", {
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(frac = data.frame(stratum = c("A", "B"), frac = c(0.2, 0.2)))

  out <- as.list(design)
  expect_equal(out$stages[[1]]$draw$frac, "custom (data frame)")
  expect_true("frac" %in% out$stages[[1]]$draw$frac_columns)
})

test_that("as.list records scalar frac, control, and non-default on_empty", {
  design <- sampling_design() |>
    draw(
      frac = 0.3,
      method = "systematic",
      control = c(cluster, y),
      on_empty = "warn"
    )

  out <- as.list(design)
  draw <- out$stages[[1]]$draw
  expect_equal(draw$frac, 0.3)
  expect_equal(draw$control, c("cluster", "y"))
  expect_equal(draw$on_empty, "warn")
})

## write_design() / read_design() / design_json()

test_that("a simple design round-trips through a file", {
  design <- sampling_design(title = "Simple") |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 40)

  path <- withr::local_tempfile(fileext = ".json")
  expect_invisible(write_design(design, path))
  restored <- read_design(path)

  expect_s3_class(restored, "sampling_design")
  expect_equal(restored$title, "Simple")
  expect_equal(restored$stages[[1]]$strata$vars, "stratum")
  expect_equal(restored$stages[[1]]$strata$alloc, "proportional")
  expect_equal(restored$stages[[1]]$draw_spec$n, 40)

  s1 <- execute(design, test_frame, seed = 42)
  s2 <- execute(restored, test_frame, seed = 42)
  expect_equal(s2$id, s1$id)
  expect_equal(s2$.weight, s1$.weight)
})

test_that("balanced method alias serializes as canonical cube", {
  design <- sampling_design() |>
    draw(n = 10, method = "balanced", aux = y)
  payload <- jsonlite::fromJSON(
    design_json(design),
    simplifyVector = FALSE
  )
  restored <- read_design(design_json(design))

  expect_identical(restored$stages[[1]]$draw_spec$method, "cube")
  expect_identical(
    payload$design$stages[[1]]$draw$method$id,
    "cube_balanced"
  )
  expect_identical(
    payload$tools$samplyr$design$stages[[1]]$method$name,
    "cube"
  )
})

test_that("a complex multi-stage design round-trips faithfully", {
  strata <- c("A", "B", "C", "D")
  design <- sampling_design(title = "Complex") |>
    add_stage(label = "Clusters") |>
    stratify_by(
      stratum,
      alloc = "optimal",
      variance = data.frame(stratum = strata, var = c(1.2, 0.8, 1, 1)),
      cost = data.frame(stratum = strata, cost = c(1, 2, 1, 2))
    ) |>
    cluster_by(cluster) |>
    draw(
      n = 8,
      method = "pps_brewer",
      mos = mos,
      min_n = 1,
      control = c(desc(mos), stratum)
    ) |>
    add_stage(label = "Units") |>
    draw(n = 2, on_empty = "warn")

  path <- withr::local_tempfile(fileext = ".json")
  write_design(design, path)
  restored <- read_design(path)

  expect_length(restored$stages, 2)
  stage1 <- restored$stages[[1]]
  expect_equal(stage1$label, "Clusters")
  expect_equal(stage1$strata$alloc, "optimal")
  expect_equal(stage1$strata$variance$var, c(1.2, 0.8, 1, 1))
  expect_equal(stage1$strata$cost$cost, c(1, 2, 1, 2))
  expect_equal(stage1$clusters$vars, "cluster")
  expect_equal(stage1$draw_spec$mos, "mos")
  expect_equal(stage1$draw_spec$min_n, 1)
  expect_equal(restored$stages[[2]]$draw_spec$on_empty, "warn")

  s1 <- execute(design, test_frame, seed = 7)
  s2 <- execute(restored, test_frame, seed = 7)
  expect_equal(s2$id, s1$id)
  expect_equal(s2$.weight, s1$.weight)
})

test_that("named-vector and data-frame sample sizes round-trip", {
  design_named <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = c(A = 5, B = 10, C = 5, D = 10))

  restored <- read_design(design_json(design_named))
  expect_equal(
    restored$stages[[1]]$draw_spec$n,
    c(A = 5, B = 10, C = 5, D = 10)
  )

  design_df <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = data.frame(stratum = c("A", "B"), n = c(5L, 7L)))

  restored_df <- read_design(design_json(design_df))
  n_spec <- restored_df$stages[[1]]$draw_spec$n
  expect_s3_class(n_spec, "data.frame")
  expect_equal(n_spec$stratum, c("A", "B"))
  expect_equal(n_spec$n, c(5, 7))
})

test_that("control expressions round-trip and order identically", {
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(
      n = 12,
      method = "systematic",
      control = c(stratum, desc(mos), serp(cluster, y))
    )

  json <- design_json(design)
  payload <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  control_json <- payload$design$stages[[1]]$draw$control

  expect_equal(payload$format_version, 3)
  expect_equal(control_json, list(
    list(type = "ascending", variables = list("stratum")),
    list(type = "descending", variables = list("mos")),
    list(type = "serpentine", variables = list("cluster", "y"))
  ))
  expect_false(grepl("serp\\(", json))
  expect_false(grepl("desc\\(", json))

  restored <- read_design(json)
  control <- restored$stages[[1]]$draw_spec$control
  expect_length(control, 3)
  expect_true(all(vapply(control, rlang::is_quosure, logical(1))))
  expect_equal(
    vapply(control, rlang::as_label, character(1)),
    c("stratum", "desc(mos)", "serp(cluster, y)")
  )

  s1 <- execute(design, test_frame, seed = 11)
  s2 <- execute(restored, test_frame, seed = 11)
  expect_equal(s2$id, s1$id)
})

test_that("write_design() rejects control expressions outside the allowlist", {
  design <- sampling_design() |>
    draw(n = 5, control = c(mos * 2))

  expect_error(
    design_json(design),
    "Cannot serialize the control expression"
  )
})

test_that("read_design() refuses unknown declarative control types", {
  design <- sampling_design() |>
    draw(n = 5, method = "systematic", control = c(mos))
  payload <- jsonlite::fromJSON(
    design_json(design),
    simplifyVector = FALSE
  )
  payload$design$stages[[1]]$draw$control[[1]]$type <- "system"
  bad <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")

  expect_error(read_design(bad), "draw/control/0/type")
})

test_that("namespaced control calls are rejected when writing", {
  design <- sampling_design() |>
    draw(n = 5, method = "systematic", control = c(dplyr::desc(mos)))

  expect_error(
    design_json(design),
    "Cannot serialize the control expression"
  )

})

test_that("control variables are always treated as data", {
  design <- sampling_design() |>
    draw(n = 5, method = "systematic", control = c(mos))
  payload <- jsonlite::fromJSON(
    design_json(design),
    simplifyVector = FALSE
  )
  literal_name <- 'system("echo pwned")'
  payload$design$stages[[1]]$draw$control[[1]]$variables <- list(literal_name)
  json <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")

  restored <- read_design(json)
  expr <- rlang::quo_get_expr(
    restored$stages[[1]]$draw_spec$control[[1]]
  )
  expect_true(rlang::is_symbol(expr))
  expect_identical(as.character(expr), literal_name)
})

test_that("sampling methods have a portable descriptor and samplyr mapping", {
  design <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = mos)
  payload <- jsonlite::fromJSON(
    design_json(design),
    simplifyVector = FALSE
  )

  common <- payload$design$stages[[1]]$draw$method
  expect_equal(
    common$id,
    "generalized_brewer_probability_proportional_to_size"
  )
  expect_equal(common$family, "probability_proportional_to_size")
  expect_equal(common$algorithm, "generalized_brewer")
  expect_equal(common$replacement, "without_replacement")
  expect_equal(common$sample_size, "fixed")
  expect_equal(common$probabilities, "unequal")
  expect_equal(common$standards[[1]]$vocabulary, "DDI SamplingProcedure")
  expect_equal(common$standards[[1]]$code, "Probability")

  native <- payload$tools$samplyr$design$stages[[1]]$method
  expect_equal(native$name, "pps_brewer")
  expect_equal(payload$tools$samplyr$language$name, "R")
})

test_that("portable frame metadata is separated from R metadata", {
  design <- sampling_design() |>
    draw(n = 2)
  frame <- data.frame(
    id = 1:4,
    group = factor(c("a", "a", "b", "b")),
    date = as.Date("2026-01-01") + 0:3
  )
  payload <- jsonlite::fromJSON(
    design_json(design, frame = frame),
    simplifyVector = FALSE
  )

  portable <- payload$frame$fingerprint
  expect_equal(portable$row_count, 4)
  expect_equal(
    vapply(portable$columns, `[[`, character(1), "type"),
    c("integer", "categorical", "date")
  )
  expect_null(portable$name)
  expect_null(portable$hash)

  native <- payload$tools$samplyr$frame
  expect_equal(native$source$kind, "r_expression")
  expect_equal(native$source$value, "frame")
  expect_equal(native$hash$algorithm, "rlang::hash")
  expect_equal(native$columns[[2]]$class, list("factor"))
})

test_that("all built-in methods have unique common mappings", {
  dictionary <- sampling_method_dictionary()

  expect_setequal(names(dictionary), builtin_methods)
  ids <- vapply(dictionary, `[[`, character(1), "id")
  expect_identical(anyDuplicated(ids), 0L)
  expect_true(all(vapply(
    dictionary,
    function(x) {
      all(c(
        "id", "family", "algorithm", "replacement", "sample_size",
        "probabilities", "ddi"
      ) %in% names(x))
    },
    logical(1)
  )))

  published <- jsonlite::fromJSON(
    system.file(
      "schema", "sampling-methods-v2.json",
      package = "samplyr",
      mustWork = TRUE
    ),
    simplifyVector = FALSE
  )
  expect_equal(published$id, method_vocabulary_id)
  expect_equal(published$version, method_vocabulary_version)
  published_names <- vapply(
    published$methods,
    function(x) x$implementations$samplyr,
    character(1)
  )
  published_ids <- vapply(published$methods, `[[`, character(1), "id")
  expect_equal(
    setNames(published_ids, published_names)[names(dictionary)],
    ids
  )
})

test_that("common-only methods from another tool map into samplyr", {
  design <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = mos)
  payload <- jsonlite::fromJSON(
    design_json(design),
    simplifyVector = FALSE
  )
  payload$tools$samplyr <- NULL
  payload$tools$other_sampler <- list(
    version = "1.0",
    language = list(name = "Python", version = "3.14")
  )
  foreign_json <- jsonlite::toJSON(
    payload,
    auto_unbox = TRUE,
    null = "null"
  )

  restored <- read_design(foreign_json)
  expect_identical(
    restored$stages[[1]]$draw_spec$method,
    "pps_brewer"
  )

  reencoded <- jsonlite::fromJSON(
    design_json(restored),
    simplifyVector = FALSE
  )
  expect_equal(reencoded$tools$other_sampler, payload$tools$other_sampler)
  expect_equal(
    reencoded$tools$samplyr$design$stages[[1]]$method$name,
    "pps_brewer"
  )
})

test_that("common and samplyr method metadata cannot contradict", {
  design <- sampling_design() |>
    draw(n = 5, method = "pps_brewer", mos = mos)
  payload <- jsonlite::fromJSON(
    design_json(design),
    simplifyVector = FALSE
  )
  payload$tools$samplyr$design$stages[[1]]$method$name <- "srswor"
  bad <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")

  expect_error(read_design(bad), "methods disagree")
})

test_that("write_design() warns when the sample was executed without a seed", {
  design <- sampling_design() |>
    draw(n = 10)
  smpl <- execute(design, test_frame)

  path <- withr::local_tempfile(fileext = ".json")
  expect_warning(
    write_design(smpl, path, frame = test_frame),
    "without a seed"
  )

  receipt <- attr(read_design(path), "execution")
  expect_null(receipt$seed)
  expect_equal(receipt$n_selected, 10)
})

test_that("write_design() warns when a receipt has no frame fingerprint", {
  sample <- sampling_design() |>
    draw(n = 10) |>
    execute(test_frame, seed = 3)
  path <- withr::local_tempfile(fileext = ".json")

  expect_warning(
    write_design(sample, path),
    "without a frame fingerprint"
  )
})

test_that("design files record frame requirements and fingerprint", {
  design <- sampling_design() |>
    add_stage(label = "PSU") |>
    stratify_by(stratum) |>
    cluster_by(cluster) |>
    draw(n = 4, method = "pps_brewer", mos = mos, control = c(y)) |>
    add_stage() |>
    draw(n = 2)

  restored <- read_design(design_json(design, frame = test_frame))
  info <- attr(restored, "frame_info")

  reqs <- vapply(info$required_variables, `[[`, character(1), "name")
  roles <- vapply(info$required_variables, `[[`, character(1), "role")
  expect_setequal(reqs, c("stratum", "cluster", "mos", "y"))
  expect_setequal(roles, c("strata", "clusters", "mos", "control"))

  fp <- info$fingerprint
  expect_equal(fp$name, "test_frame")
  expect_equal(fp$nrow, nrow(test_frame))
  col_names <- vapply(fp$columns, `[[`, character(1), "name")
  expect_equal(col_names, names(test_frame))

  # the content hash sees columns by name, not the wrapper or its order
  expect_equal(fp$hash, frame_content_hash(test_frame))
  expect_equal(fp$hash, frame_content_hash(as.data.frame(test_frame)))
  expect_equal(
    fp$hash,
    frame_content_hash(test_frame[, rev(names(test_frame))])
  )
  edited <- test_frame
  edited$y[1] <- edited$y[1] + 1
  expect_false(identical(fp$hash, frame_content_hash(edited)))
})

test_that("saving a tbl_sample records a receipt that reproduces the sample", {
  design <- sampling_design() |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 20)
  sample <- execute(design, test_frame, seed = 99)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = test_frame)
  restored <- read_design(path)

  receipt <- attr(restored, "execution")
  expect_equal(receipt$seed, 99)
  expect_equal(unlist(receipt$stages_executed), 1L)
  expect_equal(receipt$n_selected, nrow(sample))

  recorded_environment <- attr(
    restored,
    "design_tools"
  )$samplyr$execution$environment
  expect_equal(
    recorded_environment,
    attr(sample, "metadata")$execution_environment
  )
  expect_named(
    recorded_environment$rng,
    c("kind", "normal_kind", "sample_kind")
  )

  replay <- execute(restored, test_frame, seed = receipt$seed)
  expect_equal(replay$id, sample$id)
  expect_equal(replay$.weight, sample$.weight)
})

test_that("read_design() validates format and version", {
  expect_error(
    read_design('{"format": "something/else", "format_version": 1}'),
    "not a samplyr design file"
  )
  expect_error(
    read_design('{"format": "samplyr/design", "format_version": 999}'),
    "not supported"
  )
  expect_error(
    read_design('{"format": "samplyr/design", "format_version": 3}'),
    "design.stages"
  )
  expect_error(read_design("not json at all {"), "not valid JSON")
})

test_that("read_design() refuses URLs and never fetches remote files", {
  expect_error(
    read_design("https://example.com/design.json"),
    "not a URL"
  )
  expect_error(
    read_design("http://example.com/design.json"),
    "not a URL"
  )
  expect_error(
    read_design("ftp://example.com/design.json"),
    "not a URL"
  )
  # Windows drive paths are not URL-shaped
  expect_error(
    read_design("C:/no/such/design.json"),
    "existing file"
  )
})

test_that("read_design() errors cleanly on a missing file", {
  expect_error(
    read_design("no/such/design.json"),
    "not valid JSON or a path to an existing file"
  )
})

test_that("write_design() validates its inputs", {
  design <- sampling_design() |>
    draw(n = 5)

  expect_error(write_design(list(), "x.json"), "must be a")
  expect_error(write_design(design, c("a", "b")), "single file path")
  path <- withr::local_tempfile(fileext = ".json")
  expect_error(
    write_design(design, path, frame = "not a frame"),
    "must be a data frame"
  )
})

## Complete receipts and replay_design() (review issue 4)

# The replay contract: full equality of the materialized sample except
# the execution timestamp.
sans_timestamp <- function(x) {
  meta <- attr(x, "metadata")
  meta$executed_at <- NULL
  attr(x, "metadata") <- meta
  x
}

sample_data <- function(x) {
  out <- as.data.frame(x)
  attributes(out) <- attributes(out)[c("names", "class", "row.names")]
  out
}

test_that("replay_design() reproduces a panelized sample including .panel", {
  sample <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 11, panels = 4)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = test_frame)
  restored <- read_design(path)

  receipt <- attr(restored, "execution")
  expect_equal(receipt$panels, 4L)

  replay <- replay_design(restored, test_frame)
  expect_true(".panel" %in% names(replay))
  expect_identical(sample_data(replay), sample_data(sample))
})

test_that("replay_design() reproduces a partial-stage execution", {
  stage1 <- sampling_design() |>
    add_stage("Clusters") |>
    cluster_by(cluster) |>
    draw(n = 4) |>
    add_stage("Units") |>
    draw(n = 2) |>
    execute(test_frame, stages = 1, seed = 5)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(stage1, path, frame = test_frame)

  replay <- replay_design(read_design(path), test_frame)
  expect_identical(get_stages_executed(replay), 1L)
  expect_identical(sample_data(replay), sample_data(stage1))
})

test_that("replay_design() reproduces a replicated execution", {
  reps <- sampling_design() |>
    draw(n = 5) |>
    execute(test_frame, seed = 3, reps = 4)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(reps, path, frame = test_frame)
  receipt <- attr(read_design(path), "execution")
  expect_equal(receipt$reps, 4L)
  expect_equal(unlist(receipt$replicate_seeds), 3:6)

  replay <- replay_design(read_design(path), test_frame)
  expect_identical(replay$.replicate, reps$.replicate)
  expect_identical(sample_data(replay), sample_data(reps))
})

test_that("replay_design() accepts a tbl_sample directly", {
  sample <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 12) |>
    execute(test_frame, seed = 21, panels = 3)

  replay <- replay_design(sample, test_frame)
  expect_identical(sans_timestamp(replay), sans_timestamp(sample))
})

test_that("replay_design() restores the execution-time RNG configuration", {
  old_kind <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)

  RNGkind(kind = "L'Ecuyer-CMRG")
  sample <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 31)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = test_frame)

  RNGkind(kind = "Mersenne-Twister")
  caller_kind <- RNGkind()
  replay <- replay_design(read_design(path), test_frame)

  expect_identical(sample_data(replay), sample_data(sample))
  expect_identical(RNGkind(), caller_kind)
})

test_that("replay_design() warns when execution versions differ", {
  sample <- sampling_design() |>
    draw(n = 10) |>
    execute(test_frame, seed = 4)
  payload <- jsonlite::fromJSON(
    design_json(sample, frame = test_frame),
    simplifyVector = FALSE
  )
  payload$tools$samplyr$execution$environment$packages$samplyr <- "0.0.0"
  # Match the writer's serialization settings: default digits would
  # truncate the digest's chance values and fail its round trip.
  json <- jsonlite::toJSON(
    payload, auto_unbox = TRUE, null = "null", na = "null", digits = NA
  )

  expect_warning(
    replay_design(read_design(json), test_frame),
    "samplyr: recorded 0.0.0"
  )
})

test_that("replay_design() requires recorded custom methods", {
  toy_method <- function(pik, n = NULL, prn = NULL, ...) {
    order(pik, decreasing = TRUE)[seq_len(n)]
  }
  sondage::register_method(
    "serialize_wor",
    "wor",
    sample_fn = toy_method,
    probabilities = "exact"
  )
  on.exit(
    if (sondage::is_registered_method("serialize_wor")) {
      sondage::unregister_method("serialize_wor")
    },
    add = TRUE
  )

  sample <- sampling_design() |>
    draw(n = 10, method = "pps_serialize_wor", mos = mos) |>
    execute(test_frame, seed = 8)
  json <- design_json(sample, frame = test_frame)
  sondage::unregister_method("serialize_wor")

  expect_error(
    replay_design(read_design(json), test_frame),
    class = "samplyr_error_replay_method_unregistered"
  )
})

test_that("chained receipts warn at write time and refuse replay", {
  stage1 <- sampling_design() |>
    add_stage("Clusters") |>
    cluster_by(cluster) |>
    draw(n = 4) |>
    add_stage("Units") |>
    draw(n = 2) |>
    execute(test_frame, stages = 1, seed = 5)
  full <- execute(stage1, test_frame, seed = 6)

  path <- withr::local_tempfile(fileext = ".json")
  expect_warning(
    write_design(full, path, frame = test_frame),
    "more than one"
  )
  expect_true(isTRUE(attr(read_design(path), "execution")$chained))
  expect_error(
    replay_design(read_design(path), test_frame),
    class = "samplyr_error_receipt_chained"
  )
})

test_that("two-phase receipts are flagged as chained", {
  phase1 <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 6) |>
    execute(test_frame, seed = 1)
  phase2 <- sampling_design() |>
    cluster_by(cluster) |>
    draw(n = 3) |>
    execute(phase1, seed = 2)

  path <- withr::local_tempfile(fileext = ".json")
  expect_warning(
    write_design(phase2, path, frame = test_frame),
    "more than one"
  )
  expect_true(isTRUE(attr(read_design(path), "execution")$chained))
})

test_that("modified samples are flagged in the receipt", {
  sample <- sampling_design() |>
    draw(n = 20) |>
    execute(test_frame, seed = 9)
  filtered <- dplyr::filter(sample, y > 0)

  path <- withr::local_tempfile(fileext = ".json")
  expect_warning(
    write_design(filtered, path, frame = test_frame),
    "modified after execution"
  )
  expect_true(isTRUE(attr(read_design(path), "execution")$modified))

  # Replay reproduces the original, full execution
  replay <- replay_design(read_design(path), test_frame)
  expect_identical(nrow(replay), nrow(sample))
})

test_that("replay_design() rejects missing receipts and seedless receipts", {
  design <- sampling_design() |> draw(n = 5)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(design, path)
  expect_error(
    replay_design(read_design(path), test_frame),
    class = "samplyr_error_no_receipt"
  )

  seedless <- suppressWarnings(execute(design, test_frame))
  suppressWarnings(write_design(seedless, path, frame = test_frame))
  expect_error(
    replay_design(read_design(path), test_frame),
    class = "samplyr_error_receipt_no_seed"
  )
})

test_that("replay_design() is strict by default when the frame differs", {
  sample <- sampling_design() |>
    draw(frac = 0.2) |>
    execute(test_frame, seed = 13)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = test_frame)
  restored <- read_design(path)

  other <- rbind(test_frame, test_frame)
  expect_error(
    replay_design(restored, other),
    class = "samplyr_error_replay_frame_mismatch"
  )

  warns <- character(0)
  withCallingHandlers(
    replay_design(restored, other, fingerprint = "warn"),
    warning = function(w) {
      warns <<- c(warns, cli::ansi_strip(conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("differs from the frame", warns)))
  expect_true(any(grepl("receipt recorded", warns)))

  # fingerprint = "ignore" skips the frame comparison
  warns2 <- character(0)
  withCallingHandlers(
    replay_design(restored, other, fingerprint = "ignore"),
    warning = function(w) {
      warns2 <<- c(warns2, cli::ansi_strip(conditionMessage(w)))
      invokeRestart("muffleWarning")
    }
  )
  expect_false(any(grepl("differs from the frame", warns2)))
})

test_that("built-in probability tiers are serialized and back-filled", {
  design <- sampling_design() |>
    draw(n = 10, method = "pps_sps", mos = mos)
  json <- design_json(design)
  payload <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_identical(
    payload$tools$samplyr$design$stages[[1]]$method$probabilities,
    "approximate"
  )
  expect_identical(
    read_design(json)$stages[[1]]$draw_spec$method_probabilities,
    "approximate"
  )

  # Files written before the field carry no tier for built-ins;
  # reconstruction fills it from the method name.
  payload$tools$samplyr$design$stages[[1]]$method$probabilities <- NULL
  restored <- read_design(
    jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null")
  )
  expect_identical(
    restored$stages[[1]]$draw_spec$method_probabilities,
    "approximate"
  )
})

test_that("replay refuses a different implementation under the same name", {
  on.exit(sondage::unregister_method("impl_swap"), add = TRUE)
  first_k <- function(pik, n = NULL, prn = NULL, ...) seq_len(n)
  sondage::register_method(
    "impl_swap", "wor", sample_fn = first_k, probabilities = "exact"
  )
  s <- sampling_design() |>
    draw(n = 3, method = "pps_impl_swap", mos = mos) |>
    execute(test_frame, seed = 5)
  expect_identical(s$id, sprintf("u%03d", 1:3))
  json <- design_json(s, frame = test_frame)
  restored <- read_design(json)

  # Re-registering the same code (formatting and comments may differ)
  # fingerprints identically and replays the recorded sample.
  sondage::unregister_method("impl_swap")
  same_code <- function(pik, n = NULL, prn = NULL, ...)   seq_len(n) # same tree
  sondage::register_method(
    "impl_swap", "wor", sample_fn = same_code, probabilities = "exact"
  )
  replayed <- replay_design(restored, test_frame)
  expect_identical(replayed$id, s$id)
  expect_identical(replayed$.weight, s$.weight)

  # A different function under identical registry metadata is refused:
  # without the fingerprint this silently replayed different rows.
  sondage::unregister_method("impl_swap")
  last_k <- function(pik, n = NULL, prn = NULL, ...) {
    rev(seq_along(pik))[seq_len(n)]
  }
  sondage::register_method(
    "impl_swap", "wor", sample_fn = last_k, probabilities = "exact"
  )
  expect_error(
    replay_design(restored, test_frame),
    class = "samplyr_error_replay_method_mismatch"
  )
})

## I1a. The shared-weight sample the format has no schema for

# The format carries one design and one execution receipt. A shared-weight
# sample is more than that, and writing one would produce a file describing
# the source selection alone: it reads back as an ordinary sample and replays
# to one, with nothing recording that the links, the target rows and the
# shared weights were dropped. Refused at every entry point rather than
# written partially.
#
# A frame collection was refused here too until it got a format of its own.
# What remains refused about one is a component the format cannot describe,
# which is I1b-1 below.

# A collection carrying overlaps resolved from registers, which is the
# collection-level thing the format still cannot write.
serialize_unwritable_stack <- function() {
  population <- data.frame(
    id = 1:6,
    in_a = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    in_b = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
  registers <- list(
    a = population[population$in_a, , drop = FALSE],
    b = population[population$in_b, , drop = FALSE]
  )
  # Take-all draws: deterministic, and they record no seed, so the
  # independence warning stays out of tests that are not about it.
  component <- function(rows) {
    sampling_design() |>
      draw(n = nrow(rows)) |>
      execute(rows)
  }
  stack_frames(
    a = component(registers$a),
    b = component(registers$b),
    membership = c(a = "in_a", b = "in_b"),
    key = id,
    overlaps = exante_overlaps(registers, by = c(id = "id"))
  )
}

# A collection whose first component carries shared weights. Standalone shared
# samples have a format of their own; a component still has none, because a
# component entry is a design document and the collection's replay has no way
# to take one link table per component.
serialize_shared_component_stack <- function() {
  households <- data.frame(hh = paste0("h", 1:20), stringsAsFactors = FALSE)
  people <- data.frame(
    person = paste0("p", 1:40),
    hh = rep(paste0("h", 1:20), each = 2),
    in_a = TRUE,
    in_b = rep(c(FALSE, TRUE), 20),
    stringsAsFactors = FALSE
  )
  shared <- share_weights(
    sampling_design() |> draw(n = 8, method = "srswor") |>
      execute(households, seed = 3),
    targets = people, links = people,
    by = c(hh = "hh"), to = c(person = "person"),
    within = hh, multiplicity = complete_links()
  )
  in_b <- people[people$in_b, , drop = FALSE]
  list(
    households = households,
    in_b = in_b,
    shared = shared,
    stack = stack_frames(
      a = shared,
      b = sampling_design() |> draw(n = 10, method = "srswor") |>
        execute(in_b, seed = 4),
      membership = c(a = "in_a", b = "in_b"),
      key = person
    )
  )
}

test_that("a shared-weight component is refused at every verb", {
  fixture <- serialize_shared_component_stack()
  frames <- fixture$stack
  registers <- list(a = fixture$households, b = fixture$in_b)
  path <- withr::local_tempfile(fileext = ".json")

  expect_error(
    design_json(frames),
    class = "samplyr_error_serialize_weight_contract"
  )
  expect_error(
    write_design(frames, path),
    class = "samplyr_error_serialize_weight_contract"
  )
  expect_error(
    replay_design(frames, registers),
    class = "samplyr_error_serialize_weight_contract"
  )

  # Catchable as the whole weight-contract family too, which is what lets a
  # caller handle every refusal of a transformed sample in one place.
  expect_error(design_json(frames), class = "samplyr_error_weight_contract")

  # Each verb names itself. They share one helper, so a message naming a verb
  # the user did not call is the failure mode, and a class-only assertion
  # cannot see it.
  expect_error(design_json(frames), "`design_json\\(\\)` is not defined")
  expect_error(write_design(frames, path), "`write_design\\(\\)` is not defined")
  expect_error(replay_design(frames, registers), "`replay_design\\(\\)` is not")
})

test_that("the shared-weight refusal precedes the receipt warnings", {
  frames <- serialize_shared_component_stack()$stack

  # Encoding the component would warn that its receipt carries no seed and no
  # frame fingerprint. Advice about a receipt inside a file that is not going
  # to be written is worse than none, so the gate sits ahead of the encoding.
  # Pins the placement: moving it below encode_execution() still errors, and
  # only this sees the difference.
  expect_no_warning(
    expect_error(
      design_json(frames),
      class = "samplyr_error_serialize_weight_contract"
    )
  )
})

test_that("a collection the format cannot describe is refused at every verb", {
  frames <- serialize_unwritable_stack()
  path <- withr::local_tempfile(fileext = ".json")

  expect_error(
    design_json(frames),
    class = "samplyr_error_serialize_unsupported"
  )
  expect_error(
    write_design(frames, path),
    class = "samplyr_error_serialize_unsupported"
  )
  expect_error(
    replay_design(frames, bfa_eas),
    class = "samplyr_error_serialize_unsupported"
  )
})

test_that("a refused write_design() leaves no file behind", {
  path <- withr::local_tempfile(fileext = ".json")
  expect_false(file.exists(path))

  # The refusal is raised while the JSON is built, before writeLines(), so
  # neither kind can leave a file that reads back as a complete design.
  expect_error(write_design(serialize_shared_component_stack()$stack, path))
  expect_false(file.exists(path))

  expect_error(write_design(serialize_unwritable_stack(), path))
  expect_false(file.exists(path))
})

test_that("the new refusals leave the ordinary paths alone", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 4)
  expect_s3_class(design_json(design), "json")

  sample <- sampling_design() |>
    draw(n = 3) |>
    execute(test_frame, seed = 11)
  json <- design_json(sample, frame = test_frame)
  expect_identical(replay_design(read_design(json), test_frame)$id, sample$id)

  # The frame_stack branch sits ahead of the design/sample dispatch, so a
  # genuinely wrong argument must still get the dispatch message rather than
  # a report about frame collections.
  expect_error(design_json(42), "must be a <sampling_design> or a <tbl_sample>")
  expect_error(replay_design(42, test_frame), "must be a <sampling_design>")
})

## I1b-1. The frame collection format

# A collection is one design and one receipt per component plus what makes
# them a collection, so it gets its own format identifier rather than an
# optional block in a design file. Each component entry is a complete
# samplyr/design document with two fields added, which is what lets the
# component encoder and decoder be the design ones unchanged.

stack_population <- function() {
  data.frame(
    id = 1:40,
    y = (1:40) * 1.5,
    in_a = rep(TRUE, 40),
    in_b = rep(c(FALSE, TRUE), each = 20),
    pa = 0.25,
    pb = rep(c(0, 0.4), each = 20)
  )
}

stack_registers <- function(population = stack_population()) {
  list(a = population, b = population[population$in_b, , drop = FALSE])
}

# Seeded rather than take-all: a receipt with no seed cannot be replayed, and
# replay is what these tests are about.
stack_fixture <- function(overlaps = NULL, registers = stack_registers()) {
  stack_frames(
    a = sampling_design() |>
      draw(n = 10, method = "srswor") |>
      execute(registers$a, seed = 11),
    b = sampling_design() |>
      draw(n = 8, method = "srswor") |>
      execute(registers$b, seed = 22),
    membership = c(a = "in_a", b = "in_b"),
    key = id,
    overlaps = overlaps
  )
}

test_that("a frame collection writes its own format, not a design file", {
  frames <- stack_fixture()
  registers <- stack_registers()
  payload <- jsonlite::fromJSON(
    design_json(frames, frame = registers),
    simplifyVector = FALSE
  )

  expect_identical(payload$format, "samplyr/frame-stack")
  expect_identical(payload$format_version, 1L)
  expect_identical(payload$key, "id")
  expect_length(payload$components, 2L)

  # Each component is a design document plus the two fields that make it a
  # component. Asserted directly, because it is what lets decode_design_payload()
  # read a component with no changes.
  expect_identical(
    vapply(payload$components, function(x) x$name, character(1)),
    c("a", "b")
  )
  expect_identical(
    vapply(payload$components, function(x) x$membership, character(1)),
    c("in_a", "in_b")
  )
  expect_identical(payload$components[[1]]$format, "samplyr/design")
  expect_identical(payload$components[[1]]$execution$seed, 11L)
  expect_identical(payload$components[[2]]$execution$seed, 22L)
})

test_that("a collection round trips through a file to an identical collection", {
  frames <- stack_fixture()
  registers <- stack_registers()
  path <- withr::local_tempfile(fileext = ".json")
  write_design(frames, path, frame = registers)

  restored <- read_design(path)
  expect_s3_class(restored, "frame_stack_design")
  expect_identical(names(restored), c("a", "b"))
  expect_identical(attr(restored, "key"), "id")
  expect_identical(attr(restored, "membership"), c(a = "in_a", b = "in_b"))
  expect_s3_class(restored[[1]], "sampling_design")

  replayed <- replay_design(restored, frame = registers)
  expect_s3_class(replayed, "frame_stack")
  expect_identical(as.data.frame(replayed), as.data.frame(frames))
  expect_identical(attr(replayed, "membership"), attr(frames, "membership"))
  expect_identical(attr(replayed, "key"), attr(frames, "key"))
})

test_that("a live collection replays without a file", {
  frames <- stack_fixture()
  replayed <- replay_design(frames, frame = stack_registers())
  expect_identical(as.data.frame(replayed), as.data.frame(frames))
})

test_that("declared overlaps travel with the collection", {
  overlaps <- declared_overlaps(a = "pa", b = "pb", scale = "probabilities")
  frames <- stack_fixture(overlaps = overlaps)
  registers <- stack_registers()

  restored <- read_design(design_json(frames, frame = registers))
  expect_identical(attr(restored, "overlaps"), overlaps)

  replayed <- replay_design(restored, frame = registers)
  expect_identical(attr(replayed, "overlaps"), overlaps)

  # Dropping the specification would export the collection under a different
  # estimator, so its absence has to be visible rather than inferred.
  bare <- read_design(design_json(stack_fixture(), frame = registers))
  expect_null(attr(bare, "overlaps"))
})

test_that("resolved overlaps are refused rather than written empty", {
  registers <- stack_registers()
  frames <- stack_fixture(
    overlaps = exante_overlaps(registers, by = c(id = "id"))
  )

  # The record left on the collection carries no class at all, so a class test
  # falls through to the declared branch and writes `cols: null`, losing the
  # matrices silently. The discriminator is `cols`, and this pins it.
  spec <- attr(frames, "overlaps")
  expect_false(inherits(spec, "samplyr_exante_overlap_spec"))
  expect_null(spec$cols)
  expect_false(is.null(spec$resolved))

  expect_error(
    design_json(frames, frame = registers),
    class = "samplyr_error_serialize_unsupported"
  )
  expect_error(
    replay_design(frames, frame = registers),
    class = "samplyr_error_serialize_unsupported"
  )
  expect_error(design_json(frames, frame = registers), "exante_overlaps")
})

test_that("the resolved-overlaps refusal precedes encoding the components", {
  frames <- serialize_unwritable_stack()

  # Same contract as the shared-weight gate, and the same reason: encoding a
  # component warns about its own receipt, and advice about a receipt inside a
  # file that is not going to be written is worse than none. Below the
  # component loop the refusal still fires and every class assertion still
  # passes, so only this sees the difference.
  expect_no_warning(
    expect_error(
      design_json(frames),
      class = "samplyr_error_serialize_unsupported"
    )
  )
})

test_that("a collection needs one register per component, named", {
  frames <- stack_fixture()
  registers <- stack_registers()

  # A single data frame, a list in the wrong shape, a short list, and one
  # naming a component that does not exist.
  expect_error(
    design_json(frames, frame = registers$a),
    class = "samplyr_error_serialize_frame_stack_frame"
  )
  expect_error(
    design_json(frames, frame = list(registers$a, registers$b)),
    class = "samplyr_error_serialize_frame_stack_frame"
  )
  expect_error(
    design_json(frames, frame = list(a = registers$a)),
    class = "samplyr_error_serialize_frame_stack_frame"
  )
  expect_error(
    design_json(frames, frame = c(registers, list(c = registers$a))),
    class = "samplyr_error_serialize_frame_stack_frame"
  )

  # Saving without registers stays legal: a frame only adds a fingerprint.
  # Replaying without them does not, since there is nothing to select from.
  expect_s3_class(suppressWarnings(design_json(frames)), "json")
  expect_error(
    replay_design(frames, frame = NULL),
    class = "samplyr_error_serialize_frame_stack_frame"
  )
})

test_that("registers are matched to components by name, not position", {
  frames <- stack_fixture()
  registers <- stack_registers()
  reversed <- registers[c("b", "a")]

  # The components have different registers, so matching by position would
  # fingerprint each against the other's and replay the wrong rows. It would
  # not necessarily error, which is why this asserts the result rather than
  # the absence of a condition.
  expect_identical(
    as.data.frame(replay_design(frames, frame = reversed)),
    as.data.frame(replay_design(frames, frame = registers))
  )
  expect_identical(
    design_json(frames, frame = reversed),
    design_json(frames, frame = registers)
  )
})

test_that("a collection with a shared-weight component is refused", {
  households <- data.frame(
    hh = paste0("h", 1:20),
    stringsAsFactors = FALSE
  )
  people <- data.frame(
    person = paste0("p", 1:40),
    hh = rep(paste0("h", 1:20), each = 2),
    in_a = TRUE,
    in_b = rep(c(FALSE, TRUE), 20),
    stringsAsFactors = FALSE
  )
  shared <- share_weights(
    sampling_design() |> draw(n = 8, method = "srswor") |>
      execute(households, seed = 3),
    targets = people, links = people,
    by = c(hh = "hh"), to = c(person = "person"),
    within = hh, multiplicity = complete_links()
  )
  in_b <- people[people$in_b, , drop = FALSE]
  frames <- stack_frames(
    a = shared,
    b = sampling_design() |> draw(n = 10, method = "srswor") |>
      execute(in_b, seed = 4),
    membership = c(a = "in_a", b = "in_b"),
    key = person
  )

  # The collection is writable the moment the component is, so the refusal is
  # the component's own rather than a second one about collections.
  expect_error(
    design_json(frames),
    class = "samplyr_error_serialize_weight_contract"
  )
  expect_error(
    replay_design(frames, frame = list(a = households, b = in_b)),
    class = "samplyr_error_serialize_weight_contract"
  )

  # Replay checks every component before running any. The shared one has to
  # come second for that to be visible: the first component is then given a
  # register too small for its receipt, which replays with warnings rather
  # than failing. Without the up-front pass the user hears those warnings
  # about a collection that was never going to be rebuilt, after a selection
  # has been re-executed for nothing.
  reversed <- stack_frames(
    b = sampling_design() |> draw(n = 10, method = "srswor") |>
      execute(in_b, seed = 4),
    a = shared,
    membership = c(b = "in_b", a = "in_a"),
    key = person
  )
  expect_identical(names(reversed), c("b", "a"))
  expect_no_warning(
    expect_error(
      replay_design(
        reversed,
        frame = list(b = in_b[1:3, , drop = FALSE], a = households)
      ),
      class = "samplyr_error_serialize_weight_contract"
    )
  )
})

test_that("the two formats do not read each other", {
  registers <- stack_registers()
  stack_file <- design_json(stack_fixture(), frame = registers)
  sample_file <- design_json(
    sampling_design() |> draw(n = 5, method = "srswor") |>
      execute(registers$a, seed = 9),
    frame = registers$a
  )

  # A design reader given a collection would take the first component for the
  # whole thing, which is one frame's estimate presented as the collection's.
  # The identifier is what stops that, so it is asserted from both sides.
  expect_s3_class(read_design(stack_file), "frame_stack_design")
  expect_s3_class(read_design(sample_file), "sampling_design")
  expect_false(inherits(read_design(sample_file), "frame_stack_design"))

  payload <- jsonlite::fromJSON(stack_file, simplifyVector = FALSE)
  payload$format_version <- 2L
  expect_error(
    read_design(jsonlite::toJSON(payload, auto_unbox = TRUE)),
    "format version"
  )
})

test_that("a collection file missing what makes it one is refused", {
  intact <- jsonlite::fromJSON(
    suppressWarnings(design_json(stack_fixture())),
    simplifyVector = FALSE
  )
  # The encoder's own options. A receipt carries a frame digest whose
  # length-one entries do not survive a plain re-encode, which would be this
  # test's round trip rather than anything the format does.
  reread <- function(payload) {
    read_design(jsonlite::toJSON(
      payload,
      auto_unbox = TRUE, dataframe = "rows", digits = NA,
      na = "null", null = "null"
    ))
  }
  expect_s3_class(reread(intact), "frame_stack_design")

  # Each of these could be filled in with a guess, and each guess would build
  # a collection that is not the one the file describes.
  without_key <- intact
  without_key$key <- NULL
  expect_error(reread(without_key), "key")

  without_name <- intact
  without_name$components[[2]]$name <- NULL
  expect_error(reread(without_name), "name")

  without_membership <- intact
  without_membership$components[[1]]$membership <- NULL
  expect_error(reread(without_membership), "membership")

  duplicated_name <- intact
  duplicated_name$components[[2]]$name <- "a"
  expect_error(reread(duplicated_name), "name")

  without_components <- intact
  without_components$components <- list()
  expect_error(reread(without_components), "components")

  # A partial overlap mapping would export the collection under a different
  # estimator, so it is refused rather than dropped to none.
  declared <- jsonlite::fromJSON(
    suppressWarnings(design_json(
      stack_fixture(
        overlaps = declared_overlaps(
          a = "pa", b = "pb", scale = "probabilities"
        )
      )
    )),
    simplifyVector = FALSE
  )
  expect_s3_class(reread(declared), "frame_stack_design")
  declared$overlaps$cols$b <- NULL
  expect_error(reread(declared), "overlaps")
})

## I1b-2. The shared-weight sample format

# The file records the source selection and the transformation's arguments.
# The links and the target register are supplied again at replay, the way a
# frame is, so no unit-level data and no linkage is ever written. What makes
# that possible is that the transformation record already carries its own call
# declaratively.

shared_source_register <- function() {
  data.frame(hh = paste0("h", 1:20), stringsAsFactors = FALSE)
}

shared_target_register <- function() {
  data.frame(
    person = paste0("p", 1:40),
    hh = rep(paste0("h", 1:20), each = 2),
    importance = rep(c(1, 2), 20),
    household_total = 3,
    stringsAsFactors = FALSE
  )
}

shared_sample_fixture <- function(...) {
  people <- shared_target_register()
  share_weights(
    sampling_design() |>
      draw(n = 8, method = "srswor") |>
      execute(shared_source_register(), seed = 3),
    targets = people,
    links = people,
    by = c(hh = "hh"),
    to = c(person = "person"),
    ...
  )
}

test_that("a shared-weight sample writes its source and its call, not its data", {
  shared <- shared_sample_fixture(
    within = hh, multiplicity = complete_links()
  )
  json <- design_json(shared, frame = shared_source_register())
  payload <- jsonlite::fromJSON(json, simplifyVector = FALSE)

  expect_identical(payload$format, "samplyr/shared-sample")
  expect_identical(payload$format_version, 1L)
  expect_identical(names(payload), c(
    "format", "format_version", "transformation", "source"
  ))

  # The source is an ordinary design document, so it goes through the encoder
  # every other sample goes through.
  expect_identical(payload$source$format, "samplyr/design")
  expect_identical(payload$source$execution$seed, 3L)

  spec <- payload$transformation
  expect_identical(spec$algorithm, "generalized_weight_share")
  expect_identical(spec$by, list(hh = "hh"))
  expect_identical(spec$to, list(person = "person"))
  expect_identical(spec$within$mode, "cluster")
  expect_identical(spec$within$col, "hh")
  expect_identical(spec$multiplicity$mode, "complete_links")
  expect_identical(spec$target_scope, "reached")

  # The point of the format, and the reason there is no privacy question to
  # settle: no target unit and no link appears anywhere in the file. Column
  # names do, because the call names them, and that is all a reader learns.
  people <- shared_target_register()
  expect_false(any(vapply(
    people$person, grepl, logical(1), x = json, fixed = TRUE
  )))
  expect_false(any(vapply(
    unique(people$hh), grepl, logical(1), x = json, fixed = TRUE
  )))
  expect_true(grepl("person", json, fixed = TRUE))
})

test_that("every within and multiplicity mode replays to the same sample", {
  register <- shared_source_register()
  people <- shared_target_register()
  modes <- list(
    cluster = list(within = quote(hh), multiplicity = quote(complete_links())),
    singleton = list(within = NULL, multiplicity = quote(complete_links())),
    extended = list(
      within = quote(extend_links(hh)),
      multiplicity = quote(complete_links())
    ),
    weighted = list(
      within = quote(hh),
      multiplicity = quote(weighted_links(importance, total = household_total))
    ),
    complete_weighted = list(
      within = quote(hh),
      multiplicity = quote(
        weighted_links(importance, total = complete_links())
      )
    ),
    population = list(
      within = quote(hh),
      multiplicity = quote(complete_links()),
      target_scope = "population"
    )
  )

  for (nm in names(modes)) {
    args <- modes[[nm]]
    shared <- rlang::inject(shared_sample_fixture(!!!args))
    replayed <- replay_design(
      read_design(design_json(shared, frame = register)),
      frame = register, links = people, targets = people
    )
    expect_identical(replayed$person, shared$person, info = nm)
    expect_identical(replayed$.weight, shared$.weight, info = nm)
    expect_identical(names(replayed), names(shared), info = nm)
    # The transformation record has to survive too, or the replayed object is
    # a different kind of thing from the one that was saved.
    expect_identical(
      attr(replayed, "metadata")$weight_share$call,
      attr(shared, "metadata")$weight_share$call,
      info = nm
    )
  }
})

test_that("a live shared-weight sample replays without a file", {
  register <- shared_source_register()
  people <- shared_target_register()
  shared <- shared_sample_fixture(within = hh, multiplicity = complete_links())

  replayed <- replay_design(
    shared, frame = register, links = people, targets = people
  )
  expect_identical(replayed$.weight, shared$.weight)
})

test_that("replay reports which of the three inputs was wrong", {
  register <- shared_source_register()
  people <- shared_target_register()
  shared <- shared_sample_fixture(within = hh, multiplicity = complete_links())
  restored <- read_design(design_json(shared, frame = register))

  # The links and the targets are not in the file, so they cannot be checked
  # before use. Two recorded integrity records are what catch them, and they
  # sit at different stages so the message can say which.
  expect_error(
    replay_design(restored, frame = register, links = people),
    class = "samplyr_error_replay_argument"
  )
  expect_error(
    replay_design(restored, frame = register, targets = people),
    class = "samplyr_error_replay_argument"
  )

  # A register that is not the one selected from: caught before the
  # transformation is re-applied at all.
  expect_error(
    replay_design(
      restored, frame = register[1:10, , drop = FALSE],
      links = people, targets = people
    ),
    class = "samplyr_error_replay_frame_mismatch"
  )

  # The source checkpoint carries the case the frame fingerprint cannot: a
  # sample saved without `frame` records no fingerprint, so nothing else
  # guards the register. It compares the design columns, so what it sees is a
  # selection whose weights differ, not a register with different labels.
  bigger <- data.frame(
    hh = paste0("h", 1:30), stringsAsFactors = FALSE
  )
  wider_people <- data.frame(
    person = paste0("p", 1:60),
    hh = rep(paste0("h", 1:30), each = 2),
    importance = rep(c(1, 2), 30),
    household_total = 3,
    stringsAsFactors = FALSE
  )
  unfingerprinted <- read_design(suppressWarnings(design_json(shared)))
  expect_error(
    replay_design(
      unfingerprinted, frame = bigger,
      links = wider_people, targets = wider_people
    ),
    class = "samplyr_error_replay_weight_share_mismatch"
  )
  expect_error(
    replay_design(
      unfingerprinted, frame = bigger,
      links = wider_people, targets = wider_people
    ),
    "replayed selection"
  )

  # A link table that transforms cleanly but is not the one used: only the
  # result integrity can see this, which is why it is recorded.
  relinked <- people
  relinked$hh <- rev(relinked$hh)
  expect_error(
    replay_design(
      restored, frame = register, links = relinked, targets = people
    ),
    class = "samplyr_error_replay_weight_share_mismatch"
  )
  expect_error(
    replay_design(
      restored, frame = register, links = relinked, targets = people
    ),
    "links.+or.+targets"
  )
})

test_that("links and targets are refused where they describe nothing", {
  register <- shared_source_register()
  people <- shared_target_register()
  sample <- sampling_design() |>
    draw(n = 3, method = "srswor") |>
    execute(register, seed = 1)

  # Accepting and ignoring them would let a user replay an ordinary sample
  # believing a transformation had been re-applied.
  expect_error(
    replay_design(sample, register, links = people),
    class = "samplyr_error_replay_argument"
  )
  expect_error(
    replay_design(sample, register, targets = people),
    class = "samplyr_error_replay_argument"
  )
  # Singular and plural both, because cli needs a quantity for each bullet and
  # loses it after interpolating a vector.
  expect_error(
    replay_design(sample, register, links = people, targets = people),
    "are not used"
  )
  expect_error(
    replay_design(sample, register, links = people),
    "is not used"
  )
})

test_that("a transformation this build cannot replay is refused", {
  register <- shared_source_register()
  shared <- shared_sample_fixture(within = hh, multiplicity = complete_links())
  payload <- jsonlite::fromJSON(
    design_json(shared, frame = register),
    simplifyVector = FALSE
  )
  reread <- function(p) {
    read_design(jsonlite::toJSON(
      p,
      auto_unbox = TRUE, dataframe = "rows", digits = NA,
      na = "null", null = "null"
    ))
  }
  expect_s3_class(reread(payload), "shared_sample_design")

  # Replaying under the rules of a different algorithm would reproduce a
  # sample nobody drew, so the name and the version are read before anything
  # is taken from the record.
  wrong_algorithm <- payload
  wrong_algorithm$transformation$algorithm <- "some_other_method"
  expect_error(
    reread(wrong_algorithm),
    class = "samplyr_error_weight_share_record_unsupported"
  )

  newer <- payload
  newer$transformation$version <- 2L
  expect_error(
    reread(newer),
    class = "samplyr_error_weight_share_record_unsupported"
  )

  newer_format <- payload
  newer_format$format_version <- 2L
  expect_error(reread(newer_format), "format version")

  # Each part of the call is load-bearing and none can be inferred from the
  # others, so a file missing one is refused rather than defaulted.
  for (field in c("by", "to", "within", "multiplicity", "target_scope")) {
    broken <- payload
    broken$transformation[[field]] <- NULL
    expect_error(reread(broken), "transformation",
                 class = "samplyr_error_design_file_malformed", info = field)
  }
})

test_that("the three formats stay distinguishable", {
  register <- shared_source_register()
  shared_file <- design_json(
    shared_sample_fixture(within = hh, multiplicity = complete_links()),
    frame = register
  )
  design_file <- design_json(
    sampling_design() |> draw(n = 3, method = "srswor") |>
      execute(register, seed = 1),
    frame = register
  )

  expect_s3_class(read_design(shared_file), "shared_sample_design")
  expect_s3_class(read_design(design_file), "sampling_design")
  expect_false(inherits(read_design(design_file), "shared_sample_design"))

  # A shared-sample design is a sampling_design, so the accessors keep
  # working on the source selection it carries.
  restored <- read_design(shared_file)
  expect_s3_class(restored, "sampling_design")
  expect_length(restored$stages, 1L)
})
