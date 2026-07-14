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

# write_design() / read_design() / design_json() -------------------------

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
      control = c(desc(mos), y)
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

  expect_equal(payload$format_version, 1)
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

  expect_error(read_design(bad), "invalid control term")
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
  expect_equal(common$id, "brewer_probability_proportional_to_size")
  expect_equal(common$family, "probability_proportional_to_size")
  expect_equal(common$algorithm, "brewer")
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
    function(x) all(c(
      "id", "family", "algorithm", "replacement", "sample_size",
      "probabilities", "ddi"
    ) %in% names(x)),
    logical(1)
  )))

  published <- jsonlite::fromJSON(
    system.file(
      "schema", "sampling-methods-v1.json",
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
  expect_warning(write_design(smpl, path), "without a seed")

  receipt <- attr(read_design(path), "execution")
  expect_null(receipt$seed)
  expect_equal(receipt$n_selected, 10)
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
    read_design('{"format": "samplyr/design", "format_version": 1}'),
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

# Complete receipts and replay_design() (review issue 4) --------------------

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

test_that("replay_design() warns when the frame differs", {
  sample <- sampling_design() |>
    draw(frac = 0.2) |>
    execute(test_frame, seed = 13)

  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = test_frame)
  restored <- read_design(path)

  other <- rbind(test_frame, test_frame)
  warns <- character(0)
  withCallingHandlers(
    replay_design(restored, other),
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
