## Coverage reported at the analysis boundary, over the union

# `share_weights()` records target clusters no link reaches but does not warn,
# since another frame may reach them. The warning fires where the estimate is
# formed. The targets are twelve households of two people each.

coverage_targets <- function() {
  data.frame(
    person_id = 1:24,
    hh = rep(paste0("h", sprintf("%02d", 1:12)), each = 2),
    y = as.numeric(1:24),
    in_a = TRUE,
    in_b = TRUE
  )
}

# `linked` names the households this frame's source population reaches.
coverage_component <- function(linked, seed,
                               targets = coverage_targets(),
                               scope = "population") {
  dwellings <- data.frame(dwelling_id = 1:12)
  source_sample <- sampling_design() |>
    draw(n = 8) |>
    execute(dwellings, seed = seed)

  links <- data.frame(
    dwelling_id = rep(linked, each = 2),
    person_id = unlist(lapply(linked, function(i) c(2 * i - 1, 2 * i)))
  )
  share_weights(
    source_sample,
    targets = targets,
    links = links,
    by = c(dwelling_id = "dwelling_id"),
    to = c(person_id = "person_id"),
    within = hh,
    multiplicity = complete_links(),
    target_scope = scope
  )
}

coverage_export <- function(x, ...) {
  suppressWarnings(as_svrepdesign(x, type = "bootstrap", replicates = 10, ...))
}

coverage_of <- function(x, ...) {
  attr(coverage_export(x, ...), "samplyr_weight_share_coverage")
}

count_coverage_warnings <- function(expr) {
  n <- 0L
  withCallingHandlers(
    force(expr),
    samplyr_warning_unlinked_cluster = function(w) {
      n <<- n + 1L
      rlang::cnd_muffle(w)
    }
  )
  n
}

## One transformation, standing alone

test_that("an unreachable cluster warns where the estimate is formed", {
  skip_if_not_installed("survey")
  shared <- coverage_component(1:9, seed = 11)

  # Recorded at the transformation, and silent there.
  expect_no_warning(coverage_component(1:9, seed = 11))
  expect_identical(
    attr(shared, "metadata")$weight_share$coverage$orphan_clusters,
    c("h10", "h11", "h12")
  )

  expect_warning(
    as_svrepdesign(shared, type = "bootstrap", replicates = 10),
    class = "samplyr_warning_unlinked_cluster"
  )
  expect_warning(
    as_svrepdesign(shared, type = "bootstrap", replicates = 10),
    regexp = "h10"
  )
})

test_that("what was warned about is recorded on the export", {
  skip_if_not_installed("survey")
  coverage <- coverage_of(coverage_component(1:9, seed = 11))

  expect_identical(coverage$status, "known")
  expect_identical(coverage$clusters, c("h10", "h11", "h12"))
})

test_that("a frame reaching every cluster does not warn", {
  skip_if_not_installed("survey")
  shared <- coverage_component(1:12, seed = 11)

  expect_no_warning(
    as_svrepdesign(shared, type = "bootstrap", replicates = 10)
  )
  coverage <- coverage_of(shared)
  expect_identical(coverage$status, "known")
  expect_length(coverage$clusters, 0L)
})

test_that("a reached-scope transformation states nothing and does not warn", {
  skip_if_not_installed("survey")
  # A roster of reached clusters cannot show that no orphan exists.
  shared <- coverage_component(1:9, seed = 11, scope = "reached")

  expect_no_warning(
    as_svrepdesign(shared, type = "bootstrap", replicates = 10)
  )
  expect_identical(coverage_of(shared)$status, "unknown")
})

test_that("an ordinary sample carries no coverage finding at all", {
  skip_if_not_installed("survey")
  ordinary <- sampling_design() |>
    draw(n = 10) |>
    execute(data.frame(id = 1:40, y = as.numeric(1:40)), seed = 3)

  expect_no_warning(
    as_svrepdesign(ordinary, type = "bootstrap", replicates = 10)
  )
  expect_null(
    attr(
      suppressWarnings(
        as_svrepdesign(ordinary, type = "bootstrap", replicates = 10)
      ),
      "samplyr_weight_share_coverage"
    )
  )
})

## A collection, over the union

coverage_stack <- function(a, b) {
  stack_frames(
    a = a, b = b,
    membership = c(a = "in_a", b = "in_b"),
    key = person_id
  )
}

test_that("frames that cover each other's gaps produce no finding", {
  skip_if_not_installed("survey")
  # a misses h10-h12 and b misses h01-h03, but together they reach all.
  frames <- coverage_stack(
    coverage_component(1:9, seed = 11),
    coverage_component(4:12, seed = 12)
  )

  expect_no_warning(
    as_svrepdesign(frames, type = "bootstrap", replicates = 10)
  )
  coverage <- coverage_of(frames)
  expect_identical(coverage$status, "known")
  expect_length(coverage$clusters, 0L)
})

test_that("only the clusters no frame reaches are reported", {
  skip_if_not_installed("survey")
  frames <- coverage_stack(
    coverage_component(1:10, seed = 11),
    coverage_component(c(1:9, 10), seed = 12)
  )
  # Both reach h01-h10, neither reaches h11 or h12.
  coverage <- coverage_of(frames)
  expect_identical(coverage$status, "known")
  expect_identical(coverage$clusters, c("h11", "h12"))

  expect_warning(
    as_svrepdesign(frames, type = "bootstrap", replicates = 10),
    class = "samplyr_warning_unlinked_cluster"
  )
  expect_warning(
    as_svrepdesign(frames, type = "bootstrap", replicates = 10),
    regexp = "any frame of this stack"
  )
})

test_that("the finding fires once, not once per component", {
  skip_if_not_installed("survey")
  frames <- coverage_stack(
    coverage_component(1:10, seed = 11),
    coverage_component(1:10, seed = 12)
  )

  expect_identical(
    count_coverage_warnings(
      as_svrepdesign(frames, type = "bootstrap", replicates = 10)
    ),
    1L
  )
})

test_that("a component's own warning is muffled inside a collection", {
  skip_if_not_installed("survey")
  component <- coverage_component(1:9, seed = 11)
  expect_warning(
    as_svrepdesign(component, type = "bootstrap", replicates = 10),
    class = "samplyr_warning_unlinked_cluster"
  )

  frames <- coverage_stack(component, coverage_component(4:12, seed = 12))
  expect_identical(
    count_coverage_warnings(
      as_svrepdesign(frames, type = "bootstrap", replicates = 10)
    ),
    0L
  )
})

## When the union cannot be established

test_that("one component that cannot answer leaves the union unknown", {
  skip_if_not_installed("survey")
  frames <- coverage_stack(
    coverage_component(1:9, seed = 11),
    coverage_component(1:9, seed = 12, scope = "reached")
  )

  expect_no_warning(
    as_svrepdesign(frames, type = "bootstrap", replicates = 10)
  )
  expect_identical(coverage_of(frames)$status, "unknown")
})

test_that("components describing different populations are incompatible", {
  skip_if_not_installed("survey")
  other <- coverage_targets()
  other$hh <- paste0(other$hh, "x")

  frames <- coverage_stack(
    coverage_component(1:9, seed = 11),
    coverage_component(1:9, seed = 12, targets = other)
  )

  expect_no_warning(
    as_svrepdesign(frames, type = "bootstrap", replicates = 10)
  )
  expect_identical(coverage_of(frames)$status, "incompatible")
})

test_that("a record written without the digest leaves the union unknown", {
  skip_if_not_installed("survey")
  # A missing digest reads as "not asked", never as coverage.
  a <- coverage_component(1:9, seed = 11)
  b <- coverage_component(4:12, seed = 12)
  metadata <- attr(b, "metadata")
  metadata$weight_share$coverage$cluster_digest <- NULL
  attr(b, "metadata") <- metadata

  expect_identical(
    union_share_coverage(list(
      attr(a, "metadata")$weight_share,
      attr(b, "metadata")$weight_share
    ))$status,
    "unknown"
  )
})

test_that("a frame sampling the target directly cannot answer for it", {
  skip_if_not_installed("survey")
  # No map joins its units to clusters of a population it never linked to.
  targets <- coverage_targets()
  direct <- sampling_design() |>
    draw(n = 8) |>
    execute(targets, seed = 4)

  frames <- coverage_stack(coverage_component(1:9, seed = 11), direct)
  expect_no_warning(
    as_svrepdesign(frames, type = "bootstrap", replicates = 10)
  )
  expect_identical(coverage_of(frames)$status, "unknown")
})

test_that("a collection with no link structure reports nothing to cover", {
  skip_if_not_installed("survey")
  targets <- coverage_targets()
  frames <- coverage_stack(
    sampling_design() |> draw(n = 8) |> execute(targets, seed = 4),
    sampling_design() |> draw(n = 10) |> execute(targets, seed = 5)
  )

  expect_no_warning(
    as_svrepdesign(frames, type = "bootstrap", replicates = 10)
  )
  expect_identical(coverage_of(frames)$status, "not_applicable")
})

## The digest itself

test_that("the digest does not depend on the session locale", {
  targets <- coverage_targets()
  # Mixed case collates one way in C and another in a UTF-8 locale.
  targets$hh <- rep(
    c("a", "B", "b", "A", "c", "C", "_d", "D", "e", "E", "f", "F"),
    each = 2
  )
  digest <- function() {
    component <- coverage_component(1:9, seed = 11, targets = targets)
    attr(component, "metadata")$weight_share$coverage$cluster_digest
  }
  # testthat sorts in C, so the other run names the session's own locale.
  in_c <- withr::with_locale(c(LC_COLLATE = "C"), digest())
  utf8 <- Sys.getlocale("LC_CTYPE")
  expect_identical(in_c, withr::with_locale(c(LC_COLLATE = utf8), digest()))
})

test_that("the digest is the target cluster set, not its size", {
  a <- coverage_component(1:9, seed = 11)
  same <- coverage_component(4:12, seed = 12)

  # Same register, different links: the population is the same.
  expect_identical(
    attr(a, "metadata")$weight_share$coverage$cluster_digest,
    attr(same, "metadata")$weight_share$coverage$cluster_digest
  )

  renamed <- coverage_targets()
  renamed$hh <- paste0(renamed$hh, "x")
  expect_false(identical(
    attr(a, "metadata")$weight_share$coverage$cluster_digest,
    attr(
      coverage_component(1:9, seed = 11, targets = renamed),
      "metadata"
    )$weight_share$coverage$cluster_digest
  ))

  # Not recorded when the register does not claim to be the population.
  expect_null(
    attr(
      coverage_component(1:9, seed = 11, scope = "reached"),
      "metadata"
    )$weight_share$coverage$cluster_digest
  )
})

## Reporting

test_that("summary states the union finding it has", {
  reachable <- coverage_stack(
    coverage_component(1:9, seed = 11),
    coverage_component(4:12, seed = 12)
  )
  output <- capture.output(summary(reachable))
  expect_true(any(grepl("^. 0 target clusters cannot be reached", output)))

  orphaned <- coverage_stack(
    coverage_component(1:10, seed = 11),
    coverage_component(1:10, seed = 12)
  )
  expect_true(any(grepl(
    "2 target clusters cannot be reached from any frame",
    capture.output(summary(orphaned))
  )))

  targets <- coverage_targets()
  ordinary <- coverage_stack(
    sampling_design() |> draw(n = 8) |> execute(targets, seed = 4),
    sampling_design() |> draw(n = 10) |> execute(targets, seed = 5)
  )
  expect_false(any(grepl(
    "Coverage", capture.output(summary(ordinary))
  )))
})
