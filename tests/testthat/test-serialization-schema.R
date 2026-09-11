schema_json <- function(x) {
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
}

schema_document <- function(method = "srswor") {
  design <- if (method %in% equal_prob_methods) {
    sampling_design() |> draw(n = 2, method = method)
  } else if (method %in% c("lpm2", "scps")) {
    sampling_design() |> draw(n = 2, method = method, spread = c(x, y))
  } else {
    sampling_design() |> draw(n = 2, method = method, mos = mos)
  }
  jsonlite::fromJSON(design_json(design), simplifyVector = FALSE)
}

schema_wrappers <- function() {
  source <- schema_document()
  list(
    design = source,
    stack = list(
      format = "samplyr/frame-stack", format_version = 1L, key = "id",
      components = list(
        c(list(name = "a", membership = "in_a"), source),
        c(list(name = "b", membership = "in_b"), source)
      )
    ),
    shared = list(
      format = "samplyr/shared-sample", format_version = 1L, source = source,
      transformation = list(
        algorithm = "generalized_weight_share", version = 1L,
        by = list(hh = "hh"), to = list(person = "person"),
        within = list(mode = "cluster", col = "hh"),
        multiplicity = list(mode = "complete_links", scale = NULL, col = NULL,
                            total_col = NULL), target_scope = "population"
      )
    )
  )
}

test_that("the frozen version 3 document remains executable", {
  path <- test_path("fixtures", "serialization", "design-v3.json")
  s <- execute(read_design(path), certainty_element_frame(), seed = 12)
  certain <- certainty_plan_fixture()$psu
  expect_setequal(unique(s$psu_id[s$.certainty_1]),
                  certain$psu_id[certain$certainty])
})

test_that("unreleased design format versions are refused", {
  for (version in c(1L, 2L)) {
    p <- schema_document()
    p$format_version <- version
    expect_error(read_design(schema_json(p)),
                 class = "samplyr_error_design_file_unsupported")
  }
})

test_that("structural failures are reported before native construction", {
  intact <- schema_document()
  local_mocked_bindings(new_sampling_design = function(...) stop("constructor reached"))
  cases <- list(
    function(p) { p$design$stages[[1]]$draw$minn_n <- 1; p },
    function(p) { p$design$stages[[1]]$draw$n <- "two"; p },
    function(p) { p$design$stages[[1]]$draw$method <- NULL; p },
    function(p) { p$design$stages[[1]]$draw$method$replacement <- "with_replacement"; p },
    function(p) { p$execution <- list(seed = "12"); p },
    function(p) { p$frame <- list(fingerprint = list(row_count = "ten")); p },
    function(p) { p$tools$samplyr$execution <- list(environment = list(rng = list(kinnd = "x"))); p }
  )
  for (change in cases) {
    expect_error(read_design(schema_json(change(intact))),
                 class = "samplyr_error_design_file_malformed")
  }
  p <- cases[[1]](intact)
  expect_error(read_design(schema_json(p)), "minn_n")
})

test_that("unknown core fields are refused in every document format", {
  for (p in schema_wrappers()) {
    p$typo <- TRUE
    expect_error(read_design(schema_json(p)), "typo",
                 class = "samplyr_error_design_file_malformed")
  }
  p <- schema_wrappers()$stack
  p$components[[2]]$design$stages[[1]]$draww <- list(n = 3)
  expect_error(read_design(schema_json(p)), "draww")
  p <- schema_wrappers()$shared
  p$transformation$within$coll <- "household"
  expect_error(read_design(schema_json(p)), "coll")
})

test_that("required extensions are refused at every execution boundary", {
  for (p in schema_wrappers()) {
    p$required_extensions <- list("example.org/adaptive-selection/1")
    expect_error(read_design(schema_json(p)), "required_extensions",
                 class = "samplyr_error_design_file_unsupported")
  }
  p <- schema_wrappers()$stack
  p$components[[2]]$required_extensions <- list("example.org/selection/1")
  expect_error(read_design(schema_json(p)), "components\\[1\\].required_extensions")
  p <- schema_wrappers()$shared
  p$source$required_extensions <- list("example.org/selection/1")
  expect_error(read_design(schema_json(p)), "source.required_extensions")
})

test_that("namespaced annotations survive all three document round trips", {
  annotations <- list(example = list(note = "reviewed", empty = setNames(list(), character()),
                                     values = list(1, 2), flag = TRUE))
  for (p in schema_wrappers()) {
    p$annotations <- annotations
    p$tools$other_sampler <- list(version = "1.0", label = "external")
    restored <- read_design(schema_json(p))
    result <- jsonlite::fromJSON(design_json(restored), simplifyVector = FALSE)
    expect_equal(result$annotations, annotations)
    expect_equal(result$tools$other_sampler, p$tools$other_sampler)
    expect_identical(result$format, p$format)
    expect_no_error(read_design(schema_json(result)))
  }
  p <- schema_wrappers()$shared
  p$annotations <- list(wrapper = list(note = "outer"))
  p$source$annotations <- list(source = list(note = "inner"))
  result <- jsonlite::fromJSON(design_json(read_design(schema_json(p))), simplifyVector = FALSE)
  expect_equal(result$annotations, p$annotations)
  expect_equal(result$source$annotations, p$source$annotations)
})

test_that("vocabulary 2 exposes first-order meaning without native metadata", {
  for (method in builtin_methods) {
    p <- schema_document(method)
    descriptor <- p$design$stages[[1]]$draw$method
    expect_equal(p$schema$method_vocabulary$version, 2)
    expect_identical(descriptor$probability_quality,
                     if (method %in% c("pps_sps", "pps_pareto")) "approximate" else "exact")
    expect_identical(descriptor$probability_quantity,
                     if (method %in% c("srswr", "pps_multinomial", "pps_chromy"))
                       "expected_hits" else "inclusion_probability")
    p$tools <- NULL
    restored <- read_design(schema_json(p))
    expect_identical(restored$stages[[1]]$draw_spec$method, method)
    expect_identical(restored$stages[[1]]$draw_spec$method_probabilities,
                     descriptor$probability_quality)
  }
})

test_that("contradictory or missing probability facets cannot be downgraded", {
  p <- schema_document("pps_sps")
  p$design$stages[[1]]$draw$method$probability_quality <- "exact"
  expect_error(read_design(schema_json(p)), "probability_quality")
  p <- schema_document("pps_sps")
  p$tools$samplyr$design$stages[[1]]$method$probabilities <- "exact"
  expect_error(read_design(schema_json(p)), "probability quality disagree")
  p <- schema_document("pps_sps")
  p$design$stages[[1]]$draw$method$probability_quantity <- NULL
  expect_error(read_design(schema_json(p)), "probability_quantity")
  p <- schema_document("pps_sps")
  p$schema$method_vocabulary$version <- 1L
  expect_error(read_design(schema_json(p)),
               class = "samplyr_error_design_file_unsupported")
})

test_that("duplicate JSON keys cannot create parser-dependent designs", {
  json <- as.character(design_json(sampling_design() |> draw(n = 2)))
  json <- sub('"n":2', '"n":2,"n":4', json, fixed = TRUE)
  expect_error(read_design(json), "Duplicate JSON key",
               class = "samplyr_error_design_file_malformed")
  expect_error(read_design("[]"), class = "samplyr_error_design_file_malformed")
})

test_that("only document frame digest contents bypass the duplicate-key walk", {
  duplicate <- list(value = 1, value = 2)
  source <- schema_document()
  source$execution <- list(frame_digest = list(stages = list(duplicate)))
  stack <- schema_wrappers()$stack
  stack$components[[1]] <- c(list(name = "a", membership = "in_a"), source)
  shared <- schema_wrappers()$shared
  shared$source <- stack
  for (p in list(source, stack, shared)) {
    expect_no_error(check_json_duplicate_keys(p))
  }

  # The exemption is about document roles, not names or dotted paths.
  fake <- list(execution = list(frame_digest = duplicate))
  for (metadata in list(fake, source, list("execution.frame_digest" = duplicate))) {
    p <- schema_document()
    p$annotations <- list("example.org" = metadata)
    expect_error(check_json_duplicate_keys(p),
      class = "samplyr_error_design_file_malformed")
  }
  source$execution <- list(frame_digest = list(), frame_digest = list())
  expect_error(check_json_duplicate_keys(source),
    class = "samplyr_error_design_file_malformed")
  source$execution <- list(frame_digest = list(), seed = 1, seed = 2)
  expect_error(check_json_duplicate_keys(source),
    class = "samplyr_error_design_file_malformed")
  shared$source$components[[2]]$design$stages[[1]]$draw$n <- duplicate
  expect_error(check_json_duplicate_keys(shared),
    class = "samplyr_error_design_file_malformed")
})

test_that("native contract validation does not need samplyr constructors", {
  for (p in schema_wrappers()) {
    expect_length(serialization_contract_errors(p), 0)
    p$unknown_selection_rule <- "ignored?"
    expect_gt(length(serialization_contract_errors(p)), 0)
  }
})
