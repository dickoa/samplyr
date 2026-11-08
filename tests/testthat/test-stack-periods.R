## The multi-occasion interchange

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
    period = rep(1:4, each = 4),
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

test_that("occasions stack into one row per observed unit-occasion", {
  master <- stack_master()
  w1 <- execute(master, period = 1)
  w2 <- execute(master, period = 2)

  tab <- stack_periods(w1, w2)

  expect_s3_class(tab, "tbl_df")
  expect_false(inherits(tab, "tbl_sample"))
  expect_identical(nrow(tab), nrow(w1) + nrow(w2))
  expect_identical(
    names(tab)[1:6],
    c("period", "subround", "master_id", "panel", "visit", "design_weight")
  )
  expect_setequal(unique(tab$period), c(1L, 2L))
  expect_identical(unique(tab$subround), 1L)
  expect_identical(attr(tab, "level"), "period")

  for (w in list(w1, w2)) {
    label <- attr(w, "metadata")$occasion$period
    part <- tab[tab$period == label, ]
    expect_setequal(part$master_id, as.data.frame(w)$.sample_id)
    expect_equal(
      part$design_weight[order(part$master_id)],
      as.data.frame(w)$.weight[order(as.data.frame(w)$.sample_id)]
    )
    expect_identical(
      part$visit[order(part$master_id)],
      as.data.frame(w)$.visit[order(as.data.frame(w)$.sample_id)]
    )
  }

  # A unit in both occasions appears twice, under one key.
  expect_gt(sum(table(tab$master_id) == 2L), 0L)
})

test_that("internal columns are dropped and user columns are not", {
  master <- stack_master()
  w1 <- execute(master, period = 1)
  w2 <- execute(master, period = 2)
  w1$response <- seq_len(nrow(w1))
  w2$response <- seq_len(nrow(w2))

  tab <- stack_periods(w1, w2)

  expect_false(any(grepl("^\\.", names(tab))))
  # Design variables survive, and so does a column attached after fieldwork.
  expect_true(all(c("region", "psu", "value", "response") %in% names(tab)))
})

test_that("a clustered master keeps the cluster variable a consumer needs", {
  # master_id is a master-local key, not a PSU.
  master <- stack_master(
    design = sampling_design() |> cluster_by(psu) |> draw(n = 16)
  )
  tab <- stack_periods(execute(master, period = 1), execute(master, period = 2))

  expect_true("psu" %in% names(tab))
  expect_gt(length(unique(tab$psu)), 1L)
  expect_false(identical(tab$psu, tab$master_id))
})

test_that("occasions carrying different analysis columns still stack", {
  master <- stack_master()
  w1 <- execute(master, period = 1)
  w2 <- execute(master, period = 2)
  w1$asked_at_occasion_1 <- 1
  w2$asked_at_occasion_2 <- 2

  tab <- stack_periods(w1, w2)

  expect_true(all(c("asked_at_occasion_1", "asked_at_occasion_2") %in% names(tab)))
  expect_true(all(is.na(tab$asked_at_occasion_2[tab$period == 1L])))
  expect_true(all(is.na(tab$asked_at_occasion_1[tab$period == 2L])))
})

## The guards, in the order the contract states them

test_that("fewer than two occasions, or something that is not an occasion, refuses", {
  master <- stack_master()
  w1 <- execute(master, period = 1)

  expect_error(stack_periods(w1), class = "samplyr_error_stack_periods_input")
  expect_error(
    stack_periods(w1, master),
    class = "samplyr_error_stack_periods_input"
  )
  expect_error(
    stack_periods(w1, stack_frame()),
    class = "samplyr_error_stack_periods_input"
  )
  expect_error(
    stack_periods(w1, w1),
    class = "samplyr_error_stack_periods_input"
  )
})

test_that("an occasion edited after execution refuses", {
  master <- stack_master()
  w1 <- execute(master, period = 1)
  w2 <- execute(master, period = 2)
  w1$.sample_id[1] <- 999L

  expect_error(
    stack_periods(w1, w2),
    class = "samplyr_error_modified_sample"
  )
})

test_that("occasions of different masters refuse, though nothing structural differs", {
  # Two executions of one design produce identical .sample_id values.
  a <- stack_master(seed = 11)
  b <- stack_master(seed = 99)

  # .sample_id is a row position, so equal values name different units.
  expect_identical(
    sort(as.data.frame(a)$.sample_id),
    sort(as.data.frame(b)$.sample_id)
  )
  expect_false(identical(as.data.frame(a)$id, as.data.frame(b)$id))
  expect_gt(
    length(intersect(execute(a, period = 1)$.sample_id,
                     execute(b, period = 2)$.sample_id)),
    0L
  )
  expect_error(
    stack_periods(execute(a, period = 1), execute(b, period = 2)),
    class = "samplyr_error_occasion_master_mismatch"
  )
})

test_that("an occasion with no retained master refuses", {
  master <- stack_master()
  w1 <- execute(master, period = 1)
  w2 <- execute(master, period = 2)

  # The activation link is intact but the master it points at is gone.
  metadata <- attr(w1, "metadata")
  metadata$prev_phase$sample <- NULL
  attr(w1, "metadata") <- metadata
  expect_error(
    stack_periods(w1, w2),
    class = "samplyr_error_occasion_no_master"
  )

  gone <- execute(master, period = 1)
  metadata <- attr(gone, "metadata")
  metadata$prev_phase <- NULL
  attr(gone, "metadata") <- metadata
  cnd <- expect_error(
    stack_periods(gone, w2),
    class = "samplyr_error_stack_periods_provenance"
  )
  expect_identical(condition_header(cnd), "stack_periods")
})

test_that("an unreadable assignment record refuses", {
  master <- stack_master()
  w1 <- execute(master, period = 1)
  w2 <- execute(master, period = 2)
  metadata <- attr(w1, "metadata")
  metadata$panel_assignment$algorithm <- "some_future_algorithm"
  attr(w1, "metadata") <- metadata

  expect_error(
    stack_periods(w1, w2),
    class = "samplyr_error_panel_record_unsupported"
  )
})

test_that("a data column named for a generated one refuses", {
  # None is reserved, and vctrs would rename them to panel...3 and panel...8.
  master <- stack_master()
  for (column in c("period", "subround", "master_id", "panel", "visit",
                   "design_weight")) {
    w1 <- execute(master, period = 1)
    w2 <- execute(master, period = 2)
    w1[[column]] <- "user data"
    w2[[column]] <- "user data"
    expect_error(
      stack_periods(w1, w2),
      class = "samplyr_error_stack_periods_columns"
    )
  }
})

test_that("the stack is not a sample and does not export as one", {
  master <- stack_master()
  tab <- stack_periods(execute(master, period = 1), execute(master, period = 2))

  expect_false(inherits(tab, "tbl_sample"))
  expect_null(attr(tab, "metadata"))
  expect_null(attr(tab, "design"))
})

test_that("masters of different frames refuse, at the same seed and schedule", {
  # Identical design, seed, schedule and protected integrity record over
  # disjoint frames. Only the in-memory hashes of the id column differ.
  design <- sampling_design() |> stratify_by(region) |> draw(n = 60)
  frame_a <- stack_frame()
  frame_b <- stack_frame()
  frame_b$id <- frame_b$id + 100000L

  a <- execute(design, frame_a, seed = 7, panels = stack_rotation())
  b <- execute(design, frame_b, seed = 7, panels = stack_rotation())

  expect_length(intersect(as.data.frame(a)$id, as.data.frame(b)$id), 0L)
  expect_identical(
    integrity_for_file(attr(a, "metadata")$integrity),
    integrity_for_file(attr(b, "metadata")$integrity)
  )
  expect_false(identical(
    attr(execute(a, period = 1), "metadata")$occasion$master_digest,
    attr(execute(b, period = 2), "metadata")$occasion$master_digest
  ))
  expect_error(
    stack_periods(execute(a, period = 1), execute(b, period = 2)),
    class = "samplyr_error_occasion_master_mismatch"
  )
})

test_that("selections of a rotation program are refused", {
  master <- stack_master()
  schedule <- data.frame(
    selection = rep(c("a", "b"), each = 8),
    panel = rep(rep(1:4, each = 2), 2),
    period = rep(1:2, 8),
    active = TRUE
  )
  program <- rotation_program(
    list(a = master, b = master),
    entry_period = c(a = 1L, b = 1L),
    schedule = schedule
  )
  live_1 <- execute(program, period = 1)
  live_2 <- execute(program, period = 2)

  # Only the recorded selection distinguishes program components.
  expect_identical(attr(live_1$a, "metadata")$occasion$selection, "a")
  expect_error(
    stack_periods(live_1$a, live_2$b),
    class = "samplyr_error_stack_periods_input"
  )
  # Even within one selection, which would need its own contract.
  expect_error(
    stack_periods(live_1$a, live_2$a),
    class = "samplyr_error_stack_periods_input"
  )
})

test_that("provenance damaged the same way in every occasion is refused", {
  # Consistent damage defeats the cross-occasion agreement check.
  master <- stack_master()
  damage <- list(
    `no activation link` = function(md) {
      md$prev_phase$transition <- "something_else"
      md
    },
    `no occasion number` = function(md) {
      md$occasion$occasion <- NULL
      md
    },
    `no master fingerprint` = function(md) {
      md$occasion$master_digest <- NULL
      md
    },
    `no schedule fingerprint` = function(md) {
      md$occasion$schedule_digest <- NULL
      md
    },
    `no assignment record` = function(md) {
      md$panel_assignment <- NULL
      md
    }
  )

  for (nm in names(damage)) {
    occasions <- lapply(1:2, function(t) {
      w <- execute(master, period = t)
      attr(w, "metadata") <- damage[[nm]](attr(w, "metadata"))
      w
    })
    expect_error(
      stack_periods(occasions[[1]], occasions[[2]]),
      class = "samplyr_error_stack_periods_provenance",
      info = nm
    )
  }
})

test_that("operations samplyr calls harmless do not invent a new realization", {
  # The fingerprint is frozen at the realization.
  master <- stack_master()
  w1 <- execute(master, period = 1)

  enriched <- master
  enriched$analysis_aux <- seq_len(nrow(enriched))
  expect_true(sample_realization_status(enriched)$ok)
  expect_identical(nrow(stack_periods(w1, execute(enriched, period = 2))), 120L)

  reordered <- master[rev(seq_len(nrow(master))), ]
  expect_true(sample_realization_status(reordered)$ok)
  expect_identical(nrow(stack_periods(w1, execute(reordered, period = 2))), 120L)
})

test_that("materializing an occasion does not depend on reading the frame digest", {
  # The fingerprint reads the stored digest, not get_frame_digest().
  master <- stack_master()
  metadata <- attr(master, "metadata")
  metadata$frame_digest$version <- 999L
  attr(master, "metadata") <- metadata

  expect_error(get_frame_digest(master))
  expect_s3_class(execute(master, period = 1), "tbl_sample")
  expect_identical(
    nrow(stack_periods(execute(master, period = 1), execute(master, period = 2))),
    120L
  )
})

## The agreement check reports rather than failing on a type

test_that("a field that is not a scalar is reported, not raised on", {
  record <- function(unit, period) {
    list(
      occasion = period, level = "period", period = period, subround = 1L,
      master_digest = "d", schedule_digest = "s",
      algorithm = "x", version = 7L, unit = unit
    )
  }

  # The comparison reads each field as given, so `unit` need not be a scalar.
  expect_error(
    check_stack_periods_agreement(list(record(c("a", "b"), 1), record("cluster", 2))),
    class = "samplyr_error_occasion_master_mismatch"
  )
  expect_error(
    check_stack_periods_agreement(list(record(NULL, 1), record("cluster", 2))),
    class = "samplyr_error_occasion_master_mismatch"
  )

  expect_no_error(
    check_stack_periods_agreement(list(record("cluster", 1), record("cluster", 2)))
  )
  expect_no_error(
    check_stack_periods_agreement(list(record(NULL, 1), record(NULL, 2)))
  )
  expect_no_error(
    check_stack_periods_agreement(list(record(c("a", "b"), 1), record(c("a", "b"), 2)))
  )
})
