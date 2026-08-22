## The multi-wave interchange

stack_frame <- function(n = 400) {
  data.frame(
    id = seq_len(n),
    region = rep(c("North", "South"), each = n / 2),
    psu = rep(seq_len(n / 10), each = 10),
    value = seq_len(n) / n
  )
}

stack_rotation <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    wave = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
}

stack_master <- function(seed = 11, design = NULL) {
  design <- design %||%
    (sampling_design() |> stratify_by(region) |> draw(n = 60))
  execute(design, stack_frame(), seed = seed, panels = stack_rotation())
}

test_that("waves stack into one row per observed unit-wave", {
  master <- stack_master()
  w1 <- execute(master, wave = 1)
  w2 <- execute(master, wave = 2)

  tab <- stack_waves(w1, w2)

  expect_s3_class(tab, "tbl_df")
  expect_false(inherits(tab, "tbl_sample"))
  expect_identical(nrow(tab), nrow(w1) + nrow(w2))
  expect_identical(
    names(tab)[1:4],
    c("wave", "master_id", "panel", "design_weight")
  )
  expect_setequal(unique(tab$wave), c(1L, 2L))

  # Each wave's rows are that wave's rows, and its weight is that wave's own.
  for (w in list(w1, w2)) {
    label <- attr(w, "metadata")$wave$wave
    part <- tab[tab$wave == label, ]
    expect_setequal(part$master_id, as.data.frame(w)$.sample_id)
    expect_equal(
      part$design_weight[order(part$master_id)],
      as.data.frame(w)$.weight[order(as.data.frame(w)$.sample_id)]
    )
  }

  # A unit in both waves appears twice, under one key. That is the point.
  expect_gt(sum(table(tab$master_id) == 2L), 0L)
})

test_that("internal columns are dropped and user columns are not", {
  master <- stack_master()
  w1 <- execute(master, wave = 1)
  w2 <- execute(master, wave = 2)
  w1$response <- seq_len(nrow(w1))
  w2$response <- seq_len(nrow(w2))

  tab <- stack_waves(w1, w2)

  expect_false(any(grepl("^\\.", names(tab))))
  # The strata and cluster variables a consumer needs survive, and so does an
  # analysis column attached after fieldwork.
  expect_true(all(c("region", "psu", "value", "response") %in% names(tab)))
})

test_that("a clustered master keeps the cluster variable a consumer needs", {
  # master_id is a master-local key, not a PSU. A consumer's PSU argument
  # wants the design's own cluster variable, which must survive the stack.
  master <- stack_master(
    design = sampling_design() |> cluster_by(psu) |> draw(n = 16)
  )
  tab <- stack_waves(execute(master, wave = 1), execute(master, wave = 2))

  expect_true("psu" %in% names(tab))
  expect_gt(length(unique(tab$psu)), 1L)
  # And the two are genuinely different columns.
  expect_false(identical(tab$psu, tab$master_id))
})

test_that("waves carrying different analysis columns still stack", {
  master <- stack_master()
  w1 <- execute(master, wave = 1)
  w2 <- execute(master, wave = 2)
  w1$asked_at_wave_1 <- 1
  w2$asked_at_wave_2 <- 2

  tab <- stack_waves(w1, w2)

  expect_true(all(c("asked_at_wave_1", "asked_at_wave_2") %in% names(tab)))
  expect_true(all(is.na(tab$asked_at_wave_2[tab$wave == 1L])))
  expect_true(all(is.na(tab$asked_at_wave_1[tab$wave == 2L])))
})

## The guards, in the order the contract states them

test_that("fewer than two waves, or something that is not a wave, refuses", {
  master <- stack_master()
  w1 <- execute(master, wave = 1)

  expect_error(stack_waves(w1), class = "samplyr_error_stack_waves_input")
  expect_error(
    stack_waves(w1, master),
    class = "samplyr_error_stack_waves_input"
  )
  expect_error(
    stack_waves(w1, stack_frame()),
    class = "samplyr_error_stack_waves_input"
  )
  expect_error(
    stack_waves(w1, w1),
    class = "samplyr_error_stack_waves_input"
  )
})

test_that("a wave edited after execution refuses", {
  master <- stack_master()
  w1 <- execute(master, wave = 1)
  w2 <- execute(master, wave = 2)
  w1$.sample_id[1] <- 999L

  expect_error(
    stack_waves(w1, w2),
    class = "samplyr_error_modified_sample"
  )
})

test_that("waves of different masters refuse, though nothing structural differs", {
  # The case a hand-written bind_rows cannot catch: two executions of one
  # design produce identical .sample_id values, so the stack would match units
  # that were never the same unit.
  a <- stack_master(seed = 11)
  b <- stack_master(seed = 99)

  # The two masters carry the SAME identifier values for DIFFERENT units,
  # which is what makes the hazard silent: .sample_id is a row position.
  expect_identical(
    sort(as.data.frame(a)$.sample_id),
    sort(as.data.frame(b)$.sample_id)
  )
  expect_false(identical(as.data.frame(a)$id, as.data.frame(b)$id))
  # And their waves share identifier values while describing different units,
  # so a hand-written bind would match units that were never the same unit.
  expect_gt(
    length(intersect(execute(a, wave = 1)$.sample_id,
                     execute(b, wave = 2)$.sample_id)),
    0L
  )
  expect_error(
    stack_waves(execute(a, wave = 1), execute(b, wave = 2)),
    class = "samplyr_error_wave_master_mismatch"
  )
})

test_that("a wave with no retained master refuses", {
  master <- stack_master()
  w1 <- execute(master, wave = 1)
  w2 <- execute(master, wave = 2)

  # The activation link is intact but the master it points at is gone, which
  # is what this condition describes. Removing the link entirely is a
  # provenance defect and is reported as one.
  metadata <- attr(w1, "metadata")
  metadata$prev_phase$sample <- NULL
  attr(w1, "metadata") <- metadata
  expect_error(
    stack_waves(w1, w2),
    class = "samplyr_error_wave_no_master"
  )

  gone <- execute(master, wave = 1)
  metadata <- attr(gone, "metadata")
  metadata$prev_phase <- NULL
  attr(gone, "metadata") <- metadata
  expect_error(
    stack_waves(gone, w2),
    class = "samplyr_error_stack_waves_provenance"
  )
})

test_that("an unreadable assignment record refuses", {
  master <- stack_master()
  w1 <- execute(master, wave = 1)
  w2 <- execute(master, wave = 2)
  metadata <- attr(w1, "metadata")
  metadata$panel_assignment$algorithm <- "some_future_algorithm"
  attr(w1, "metadata") <- metadata

  expect_error(
    stack_waves(w1, w2),
    class = "samplyr_error_panel_record_unsupported"
  )
})

test_that("a data column named for a generated one refuses", {
  # execute() cannot catch these: none is a reserved samplyr name, so a frame
  # may legitimately carry any of them. Without this check vctrs renames them
  # to panel...3 and panel...8 and the contract's column silently changes
  # meaning.
  master <- stack_master()
  for (column in c("wave", "master_id", "panel", "design_weight")) {
    w1 <- execute(master, wave = 1)
    w2 <- execute(master, wave = 2)
    w1[[column]] <- "user data"
    w2[[column]] <- "user data"
    expect_error(
      stack_waves(w1, w2),
      class = "samplyr_error_stack_waves_columns"
    )
  }
})

test_that("the stack is not a sample and does not export as one", {
  master <- stack_master()
  tab <- stack_waves(execute(master, wave = 1), execute(master, wave = 2))

  expect_false(inherits(tab, "tbl_sample"))
  expect_null(attr(tab, "metadata"))
  expect_null(attr(tab, "design"))
})

test_that("masters of different frames refuse, at the same seed and schedule", {
  # The decisive case, and the one an integrity record cannot see: identical
  # design, seed, schedule and size, with frames sharing NO population unit.
  # The protected design columns are identical, so a digest over the integrity
  # record alone matches and the waves stack, matching row 7 of one master to
  # a different population unit in the other.
  design <- sampling_design() |> stratify_by(region) |> draw(n = 60)
  frame_a <- stack_frame()
  frame_b <- stack_frame()
  frame_b$id <- frame_b$id + 100000L

  a <- execute(design, frame_a, seed = 7, panels = stack_rotation())
  b <- execute(design, frame_b, seed = 7, panels = stack_rotation())

  expect_length(intersect(as.data.frame(a)$id, as.data.frame(b)$id), 0L)
  expect_identical(
    attr(a, "metadata")$integrity,
    attr(b, "metadata")$integrity
  )
  expect_false(identical(
    attr(execute(a, wave = 1), "metadata")$wave$master_digest,
    attr(execute(b, wave = 2), "metadata")$wave$master_digest
  ))
  expect_error(
    stack_waves(execute(a, wave = 1), execute(b, wave = 2)),
    class = "samplyr_error_wave_master_mismatch"
  )
})

test_that("cohorts of a rotation program are refused", {
  master <- stack_master()
  schedule <- data.frame(
    cohort = rep(c("a", "b"), each = 8),
    panel = rep(rep(1:4, each = 2), 2),
    wave = rep(1:2, 8),
    active = TRUE
  )
  program <- rotation_program(
    list(a = master, b = master),
    entry_wave = c(a = 1L, b = 1L),
    schedule = schedule
  )
  live_1 <- execute(program, wave = 1)
  live_2 <- execute(program, wave = 2)

  # Program components are ordinary wave objects, so nothing structural
  # distinguishes them; only the recorded cohort does.
  expect_identical(attr(live_1$a, "metadata")$wave$cohort, "a")
  expect_error(
    stack_waves(live_1$a, live_2$b),
    class = "samplyr_error_stack_waves_input"
  )
  # Even within one cohort, which would need its own contract.
  expect_error(
    stack_waves(live_1$a, live_2$a),
    class = "samplyr_error_stack_waves_input"
  )
})

test_that("provenance damaged the same way in every wave is refused", {
  # Damage to one wave alone can trip the cross-wave agreement check by
  # accident. The contract has to hold when the damage is consistent, which is
  # when nothing compares the waves against each other usefully.
  master <- stack_master()
  damage <- list(
    `no activation link` = function(md) {
      md$prev_phase$transition <- "something_else"
      md
    },
    `no wave number` = function(md) {
      md$wave$wave <- NULL
      md
    },
    `no master fingerprint` = function(md) {
      md$wave$master_digest <- NULL
      md
    },
    `no schedule fingerprint` = function(md) {
      md$wave$schedule_digest <- NULL
      md
    },
    `no assignment record` = function(md) {
      md$panel_assignment <- NULL
      md
    }
  )

  for (nm in names(damage)) {
    waves <- lapply(1:2, function(t) {
      w <- execute(master, wave = t)
      attr(w, "metadata") <- damage[[nm]](attr(w, "metadata"))
      w
    })
    expect_error(
      stack_waves(waves[[1]], waves[[2]]),
      class = "samplyr_error_stack_waves_provenance",
      info = nm
    )
  }
})

test_that("operations samplyr calls harmless do not invent a new realization", {
  # The fingerprint must be frozen at the realization. Enriching a master with
  # an analysis column, or reordering its rows, leaves
  # sample_realization_status() satisfied by design, so neither may make the
  # waves of one master look like waves of two.
  master <- stack_master()
  w1 <- execute(master, wave = 1)

  enriched <- master
  enriched$analysis_aux <- seq_len(nrow(enriched))
  expect_true(sample_realization_status(enriched)$ok)
  expect_identical(nrow(stack_waves(w1, execute(enriched, wave = 2))), 120L)

  reordered <- master[rev(seq_len(nrow(master))), ]
  expect_true(sample_realization_status(reordered)$ok)
  expect_identical(nrow(stack_waves(w1, execute(reordered, wave = 2))), 120L)
})

test_that("materializing a wave does not depend on reading the frame digest", {
  # The fingerprint reads the stored digest, not get_frame_digest(), which
  # validates the schema version. Coupling activation to digest readability
  # would make an unreadable digest block materialization entirely.
  master <- stack_master()
  metadata <- attr(master, "metadata")
  metadata$frame_digest$version <- 999L
  attr(master, "metadata") <- metadata

  expect_error(get_frame_digest(master))
  expect_s3_class(execute(master, wave = 1), "tbl_sample")
  expect_identical(
    nrow(stack_waves(execute(master, wave = 1), execute(master, wave = 2))),
    120L
  )
})

## The agreement check reports rather than failing on a type

test_that("a field that is not a scalar is reported, not raised on", {
  record <- function(unit, wave) {
    list(
      wave = wave, master_digest = "d", schedule_digest = "s",
      algorithm = "x", version = 3L, unit = unit
    )
  }

  # Versions 1 and 2 are exempt from the record field checks, so nothing
  # guarantees `unit` is a scalar. `as.character()` inside a
  # `vapply(..., character(1))` ended this with R's "values must be length 1"
  # instead of the disagreement it exists to report.
  expect_error(
    check_stack_waves_agreement(list(record(c("a", "b"), 1), record("cluster", 2))),
    class = "samplyr_error_wave_master_mismatch"
  )
  expect_error(
    check_stack_waves_agreement(list(record(NULL, 1), record("cluster", 2))),
    class = "samplyr_error_wave_master_mismatch"
  )

  # And agreement is still agreement, whatever the shape.
  expect_no_error(
    check_stack_waves_agreement(list(record("cluster", 1), record("cluster", 2)))
  )
  expect_no_error(
    check_stack_waves_agreement(list(record(NULL, 1), record(NULL, 2)))
  )
  expect_no_error(
    check_stack_waves_agreement(list(record(c("a", "b"), 1), record(c("a", "b"), 2)))
  )
})
