## Reading an assignment record under the version it states

# A design file is an interchange format, so a record reaching a reader may
# have been written by an older samplyr, a newer one, or something else
# entirely. The version stamp is what tells the reader which law the fields
# were written under, and it is only worth stamping if it is read before the
# fields are.

prv_frame <- function() {
  frame <- expand.grid(
    hh = 1:4, psu = sprintf("P%d", 1:8),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  frame <- frame[order(frame$psu, frame$hh), ]
  frame$y <- seq_len(nrow(frame))
  rownames(frame) <- NULL
  frame
}

prv_design <- function() {
  sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 4) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2)
}

prv_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 2),
    wave = rep(1:2, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
}

prv_master <- function(panels = 4, ...) {
  execute(prv_design(), prv_frame(), seed = 31, panels = panels, ...)
}

prv_record <- function(x) attr(x, "metadata")$panel_assignment

prv_file <- function(master, frame = prv_frame()) {
  path <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  write_design(master, path, frame = frame)
  path
}

## Rewriting a stored record, without the package's help

# Everything below is test-only and literal. It names the fields each version
# carried and writes them out itself: a legacy artifact built by the current
# writer, or repaired by the current normalizer, would agree with the current
# reader by construction and could not detect a reader that ignores the
# version at all.

prv_stored_record <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)$execution$panel_assignment
}

# The record occupies whole lines of a pretty-printed file, so it is replaced
# as lines and the rest of the file stays byte for byte what the writer wrote.
prv_rewrite_record <- function(path, record) {
  lines <- readLines(path)
  opens <- grep('^    "panel_assignment": \\{$', lines)
  expect_length(opens, 1L)
  closes <- opens + grep("^    \\}", lines[-seq_len(opens)])[1]
  expect_false(is.na(closes))

  body <- jsonlite::toJSON(
    record,
    auto_unbox = TRUE, dataframe = "rows", digits = NA,
    na = "null", null = "null", pretty = TRUE
  )
  body <- paste0("    ", strsplit(as.character(body), "\n")[[1]])
  body[1] <- '    "panel_assignment": {'
  # Whatever separated the record from the next field is still needed.
  body[length(body)] <- paste0(
    body[length(body)], sub("^    \\}", "", lines[closes])
  )

  out <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  writeLines(c(lines[seq_len(opens - 1L)], body, lines[-seq_len(closes)]), out)
  out
}

# The record replaced by a bare JSON value rather than by another object,
# which is what a truncated or hand-edited file carries where the assignment
# belongs. The literal is written as text: a scalar built in R and serialized
# would be an array of one under `auto_unbox = FALSE` and a scalar under
# `auto_unbox = TRUE`, and which of those a file carries is the point.
prv_replace_record <- function(path, literal) {
  lines <- readLines(path)
  opens <- grep('^    "panel_assignment": \\{$', lines)
  expect_length(opens, 1L)
  closes <- opens + grep("^    \\}", lines[-seq_len(opens)])[1]
  expect_false(is.na(closes))

  replacement <- paste0(
    '    "panel_assignment": ', literal, sub("^    \\}", "", lines[closes])
  )
  out <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  writeLines(
    c(lines[seq_len(opens - 1L)], replacement, lines[-seq_len(closes)]), out
  )
  out
}

# Version 1 knew one assignment stage, one unit vocabulary, and no small-pool
# policy: positivity was unchecked rather than defaulted.
prv_as_version_1 <- function(record) {
  list(
    algorithm = record$algorithm,
    version = 1L,
    panels = record$panels,
    block_size = record$block_size,
    r_min = record$r_min,
    unit = prv_legacy_unit(record$unit),
    key_vars = record$key_vars,
    control_ordered = record$control_ordered,
    certainty = record$certainty,
    schedule = record$schedule,
    pools = lapply(record$pools, function(pool) {
      out <- list()
      if (!is.null(pool$stratum)) {
        out$stratum <- pool$stratum
      }
      out$class <- pool$class
      out$size <- pool$size
      out$keys <- pool$keys
      out$blocks <- pool$blocks
      out$quotas <- pool$quotas
      out
    })
  )
}

# Version 2 added the small-pool policy and separated a pool's activation from
# its selection class. It still had one assignment stage and no pool columns.
prv_as_version_2 <- function(record) {
  list(
    algorithm = record$algorithm,
    version = 2L,
    panels = record$panels,
    block_size = record$block_size,
    r_min = record$r_min,
    unit = prv_legacy_unit(record$unit),
    key_vars = record$key_vars,
    control_ordered = record$control_ordered,
    certainty = record$certainty,
    small_pool_policy = record$small_pool_policy,
    schedule = record$schedule,
    pools = lapply(record$pools, function(pool) {
      out <- list()
      if (!is.null(pool$stratum)) {
        out$stratum <- pool$stratum
      }
      out$class <- pool$class
      out$activation <- pool$activation
      if (!is.null(pool$permanent_reason)) {
        out$permanent_reason <- pool$permanent_reason
      }
      out$size <- pool$size
      out$keys <- pool$keys
      out$blocks <- pool$blocks
      out$quotas <- pool$quotas
      out
    })
  )
}

# Setting a field to a value, including to nothing at all. Assignment rather
# than a merge: merging recurses into a list-valued field and leaves a column
# list looking exactly as it did.
within_record <- function(record, field, value) {
  record[field] <- list(value)
  record
}

# Both older versions called a clustered unit a PSU, because the assignment
# stage was always the first one and its clusters were the primary units.
prv_legacy_unit <- function(unit) {
  if (identical(unit, "element")) "element" else "psu"
}

## Genuine legacy artifacts replay as what they are

test_that("a version-1 receipt reproduces its first-stage assignment", {
  master <- prv_master()
  path <- prv_file(master)
  legacy <- prv_as_version_1(prv_stored_record(path))

  # The downgrade removed what version 1 never had, rather than renumbering a
  # version-3 record.
  expect_identical(legacy$version, 1L)
  expect_null(legacy$assignment_stage)
  expect_null(legacy$pool_vars)
  expect_null(legacy$small_pool_policy)
  expect_null(legacy$pools[[1]]$activation)
  expect_null(legacy$pools[[1]]$permanent_reason)
  expect_identical(legacy$unit, "psu")

  path1 <- prv_rewrite_record(path, legacy)
  stored <- prv_stored_record(path1)
  expect_identical(stored$version, 1L)
  expect_null(stored$assignment_stage)
  expect_null(stored$pool_vars)
  expect_null(stored$small_pool_policy)

  replayed <- replay_design(read_design(path1), prv_frame())
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(replayed$psu, master$psu)
  expect_identical(replayed$hh, master$hh)
})

test_that("a version-2 receipt reproduces its assignment and its policy", {
  # One PSU in stratum C, so its pool holds a single unit against four panels
  # with two active: it rotates to nothing and is promoted on request.
  frame <- prv_frame()
  frame$reg <- ifelse(frame$psu == "P8", "C", frame$psu)
  design <- sampling_design() |>
    add_stage() |> stratify_by(reg) |> cluster_by(psu) |> draw(n = 1) |>
    add_stage() |> cluster_by(hh) |> draw(n = 2)
  master <- suppressWarnings(execute(
    design, frame,
    seed = 31, panels = prv_schedule(), small_pool = "permanent"
  ))
  path <- prv_file(master, frame)
  legacy <- prv_as_version_2(prv_stored_record(path))

  expect_identical(legacy$version, 2L)
  expect_null(legacy$assignment_stage)
  expect_null(legacy$pool_vars)
  expect_identical(legacy$small_pool_policy, "permanent")
  # Version 2 is where activation became a fact of its own, so it stays.
  expect_identical(legacy$pools[[1]]$activation, "permanent")
  expect_identical(legacy$pools[[1]]$permanent_reason, "small_pool")
  expect_identical(legacy$unit, "psu")

  path2 <- prv_rewrite_record(path, legacy)
  replayed <- suppressWarnings(replay_design(read_design(path2), frame))
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(replayed$psu, master$psu)

  # The policy is what carries it, and version 1 is what a record without one
  # is: replayed under today's default, this draw is refused rather than
  # reproduced. That is the intended direction, and it is why the version-2
  # field has to survive the round trip.
  as_v1 <- prv_as_version_1(prv_stored_record(path))
  expect_null(as_v1$small_pool_policy)
  expect_error(
    suppressWarnings(
      replay_design(read_design(prv_rewrite_record(path, as_v1)), frame)
    ),
    class = "samplyr_error_panel_small_pool"
  )
})

test_that("a version-3 first-stage receipt reproduces its assignment", {
  master <- prv_master()
  path <- prv_file(master)

  stored <- prv_stored_record(path)
  expect_identical(stored$version, 3L)
  expect_identical(stored$assignment_stage, 1L)
  expect_identical(stored$unit, "cluster")

  replayed <- replay_design(read_design(path), prv_frame())
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(prv_record(replayed), prv_record(master))
})

test_that("a lower-stage receipt reproduces the whole generated record", {
  master <- prv_master(panel_stage = 2)
  path <- prv_file(master)
  replayed <- replay_design(read_design(path), prv_frame())

  original <- prv_record(master)
  again <- prv_record(replayed)

  # Panels, and then everything the record states about them: a replay that
  # reproduced the labels while pooling or blocking them differently would
  # carry different denominators into every later activation.
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(again$assignment_stage, 2L)
  expect_identical(again$key_vars, c("psu", "hh"))
  expect_identical(again$pool_vars, "psu")
  expect_identical(
    lapply(again$pools, function(p) p$keys),
    lapply(original$pools, function(p) p$keys)
  )
  expect_identical(
    lapply(again$pools, function(p) p$blocks),
    lapply(original$pools, function(p) p$blocks)
  )
  expect_identical(
    lapply(again$pools, function(p) p$quotas),
    lapply(original$pools, function(p) p$quotas)
  )
  expect_identical(again, original)
})

test_that("with-replacement occurrence identities survive the file", {
  frame <- prv_frame()
  master <- execute(
    sampling_design() |> cluster_by(psu) |> draw(n = 6, method = "srswr"),
    frame,
    seed = 19, panels = 2
  )
  record <- prv_record(master)
  expect_identical(record$unit, "occurrence")
  expect_identical(record$key_vars, c("psu", ".draw_1"))

  # Six draws over four distinct PSUs: P2 and P5 are each hit twice, and the
  # two hits of P5 fall in different panels. A file that collapsed an
  # occurrence to its PSU could not carry that.
  occurrences <- unique(data.frame(
    psu = master$psu, draw = master$.draw_1, panel = master$.panel
  ))
  expect_identical(nrow(occurrences), 6L)
  expect_identical(sort(unique(master$psu)), c("P2", "P3", "P5", "P6"))
  expect_identical(occurrences$draw[occurrences$psu == "P5"], c(1L, 5L))
  expect_identical(occurrences$panel[occurrences$psu == "P5"], c(2L, 1L))

  path <- prv_file(master, frame)
  stored <- prv_stored_record(path)
  keys <- unlist(stored$pools[[1]]$keys)
  expect_identical(unlist(stored$key_vars), c("psu", ".draw_1"))
  expect_identical(length(keys), 6L)
  expect_identical(anyDuplicated(keys), 0L)

  replayed <- replay_design(read_design(path), frame)
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(replayed$.draw_1, master$.draw_1)
  expect_identical(prv_record(replayed)$pools[[1]]$keys, record$pools[[1]]$keys)
})

## A frame that no longer carries the ancestry

test_that("a replay frame missing the assignment ancestry is refused", {
  master <- prv_master(panel_stage = 2)
  path <- prv_file(master)
  design <- read_design(path)

  no_hh <- prv_frame()
  no_hh$hh <- NULL
  # The recorded fingerprint sees it first, before the frame reaches a stage.
  expect_error(
    replay_design(design, no_hh),
    class = "samplyr_error_replay_frame_mismatch"
  )
  # Told to ignore the fingerprint, the execution refuses at the stage that
  # needs the column. What is observable from here is which refusal it is and
  # that no sample comes back: a replay restores the stream it was given
  # whether or not it drew from it, so "before any random number" is not
  # something this test can assert.
  expect_error(
    replay_design(design, no_hh, fingerprint = "ignore"),
    class = "samplyr_error_frame_missing_vars"
  )
})

## An unreadable law is not read as this one

test_that("an unsupported version is refused before any field is decoded", {
  master <- prv_master(panel_stage = 2)
  path <- prv_file(master)
  record <- prv_stored_record(path)
  record$version <- 99L
  path99 <- prv_rewrite_record(path, record)

  expect_error(
    replay_design(read_design(path99), prv_frame()),
    class = "samplyr_error_panel_record_unsupported"
  )

  # Reached in the right order, not merely reached: a decoder that ran first
  # would take the receipt apart under a law nothing has established.
  local_mocked_bindings(
    decode_panel_argument = function(...) stop("a decoder ran"),
    decode_panel_stage_argument = function(...) stop("a decoder ran"),
    decode_small_pool_argument = function(...) stop("a decoder ran"),
    .package = "samplyr"
  )
  expect_error(
    replay_design(read_design(path99), prv_frame()),
    class = "samplyr_error_panel_record_unsupported"
  )
})

test_that("an unknown algorithm is refused before any field is decoded", {
  master <- prv_master()
  path <- prv_file(master)
  record <- prv_stored_record(path)
  record$algorithm <- "some_other_law"
  path_alg <- prv_rewrite_record(path, record)

  expect_error(
    replay_design(read_design(path_alg), prv_frame()),
    class = "samplyr_error_panel_record_unsupported"
  )

  local_mocked_bindings(
    decode_panel_argument = function(...) stop("a decoder ran"),
    decode_panel_stage_argument = function(...) stop("a decoder ran"),
    decode_small_pool_argument = function(...) stop("a decoder ran"),
    .package = "samplyr"
  )
  expect_error(
    replay_design(read_design(path_alg), prv_frame()),
    class = "samplyr_error_panel_record_unsupported"
  )
})

test_that("a record that is not a record is refused as malformed", {
  master <- prv_master(panel_stage = 2)
  path <- prv_file(master)

  # A design file states the assignment as an object of named fields. A bare
  # JSON value in its place parses to a length-1 atomic vector, which reading a
  # field off is base R's complaint about `$` rather than anything about the
  # artifact. `simplifyVector = FALSE` does not prevent this: it stops arrays
  # collapsing, and a scalar was never an array.
  bare <- list(
    "a number" = "3",
    "a string" = '"blocked_random_quota"',
    "a boolean" = "true"
  )

  for (case in names(bare)) {
    scalar_path <- prv_replace_record(path, bare[[case]])
    stored <- prv_stored_record(scalar_path)
    # Asserted, because a splice that produced a list again would leave every
    # expectation below passing without the case ever arising.
    expect_false(is.list(stored), info = case)
    expect_length(stored, 1L)

    expect_error(
      replay_design(read_design(scalar_path), prv_frame()),
      class = "samplyr_error_panel_record_malformed",
      info = case
    )
  }

  # An empty object is a record, and an unreadable one: it states no algorithm
  # rather than being no record. The two are separated by remedy.
  expect_error(
    replay_design(read_design(prv_replace_record(path, "{}")), prv_frame()),
    class = "samplyr_error_panel_record_unsupported"
  )

  # Refused where the version would be established, not after a decoder has
  # taken the receipt apart under a law nothing has read.
  number <- prv_replace_record(path, "3")
  local_mocked_bindings(
    decode_panel_argument = function(...) stop("a decoder ran"),
    decode_panel_stage_argument = function(...) stop("a decoder ran"),
    decode_small_pool_argument = function(...) stop("a decoder ran"),
    .package = "samplyr"
  )
  expect_error(
    replay_design(read_design(number), prv_frame()),
    class = "samplyr_error_panel_record_malformed"
  )
})

test_that("the refusal names the call, the value and not the `$` operator", {
  master <- prv_master(panel_stage = 2)
  path <- prv_file(master)
  number <- prv_replace_record(path, "3")

  err <- tryCatch(
    replay_design(read_design(number), prv_frame()),
    samplyr_error_panel_record_malformed = function(cnd) cnd
  )
  # cli wraps to the console width, so the message is matched as one line.
  message <- gsub("[[:space:]]+", " ", conditionMessage(err))
  expect_match(message, "is not a record", fixed = TRUE)
  expect_match(message, "carries 3 where", fixed = TRUE)
  expect_no_match(message, "$ operator", fixed = TRUE)
  expect_identical(as.character(conditionCall(err)[[1]]), "replay_design")
})

test_that("in-memory paths refuse a record that is not a record", {
  # Reading and writing both, on records that never went through a file.
  scheduled <- prv_master(prv_schedule())
  cls <- "samplyr_error_panel_record_malformed"

  for (value in list(3L, "blocked_random_quota", TRUE, numeric(0))) {
    broken <- scheduled
    attr(broken, "metadata")$panel_assignment <- value
    case <- typeof(value)

    expect_error(execute(broken, wave = 1), class = cls, info = case)
    expect_error(
      joint_expectation(broken, waves = c(1, 2)), class = cls, info = case
    )
    expect_error(design_json(broken), class = cls, info = case)
    expect_error(replay_design(broken, prv_frame()), class = cls, info = case)
    expect_error(
      samplyr:::prepare_panel_record(value, "An activation"), class = cls,
      info = case
    )
  }
})

## An older version does not carry a later version's meaning

test_that("a version-1 record's stage comes from its version, not its fields", {
  # The master is a first-stage assignment, and stage 2 of the same design
  # assigns differently, so interpreting the extraneous field is detectable.
  master <- prv_master()
  lower <- prv_master(panel_stage = 2)
  expect_false(identical(master$.panel, lower$.panel))

  path <- prv_file(master)
  legacy <- prv_as_version_1(prv_stored_record(path))
  legacy$assignment_stage <- 2L
  path1 <- prv_rewrite_record(path, legacy)

  expect_identical(prv_stored_record(path1)$assignment_stage, 2L)
  replayed <- replay_design(read_design(path1), prv_frame())
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(prv_record(replayed)$assignment_stage, 1L)

  prepared <- samplyr:::prepare_panel_record(
    prv_stored_record(path1), "A replay"
  )
  expect_identical(prepared$assignment_stage, 1L)
  expect_null(samplyr:::decode_panel_stage_argument(prepared))
})

test_that("a version-2 record's stage comes from its version too", {
  master <- prv_master()
  path <- prv_file(master)
  legacy <- prv_as_version_2(prv_stored_record(path))
  legacy$assignment_stage <- 2L
  path2 <- prv_rewrite_record(path, legacy)

  replayed <- replay_design(read_design(path2), prv_frame())
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(prv_record(replayed)$assignment_stage, 1L)
})

## What a record numbered 3 has to carry

test_that("a version-3 record missing or misstating a field is refused", {
  master <- prv_master(panel_stage = 2)
  path <- prv_file(master)
  record <- prv_stored_record(path)

  # Each case sets one field outright. A merge would recurse into the column
  # lists and leave them as they were, which is how three of these first
  # passed against a reader that had not read them.
  malformed <- list(
    "no assignment stage" = function(r) within_record(r, "assignment_stage", NULL),
    "missing assignment stage" = function(r) within_record(r, "assignment_stage", NA_integer_),
    "fractional stage" = function(r) within_record(r, "assignment_stage", 2.5),
    "stage below one" = function(r) within_record(r, "assignment_stage", 0L),
    "stage as a string" = function(r) within_record(r, "assignment_stage", "two"),
    "two stages" = function(r) within_record(r, "assignment_stage", c(1L, 2L)),
    "no unit" = function(r) within_record(r, "unit", NULL),
    "retired unit vocabulary" = function(r) within_record(r, "unit", "psu"),
    "unit as a number" = function(r) within_record(r, "unit", 2L),
    "no key columns" = function(r) within_record(r, "key_vars", NULL),
    "empty key columns" = function(r) within_record(r, "key_vars", list()),
    "repeated key column" = function(r) within_record(r, "key_vars", list("psu", "psu")),
    "unnamed key column" = function(r) within_record(r, "key_vars", list("psu", "")),
    "key column as a number" = function(r) within_record(r, "key_vars", list(1L)),
    "no pool columns" = function(r) within_record(r, "pool_vars", NULL),
    "repeated pool column" = function(r) within_record(r, "pool_vars", list("psu", "psu"))
  )

  for (case in names(malformed)) {
    edited <- malformed[[case]](record)
    expect_error(
      replay_design(read_design(prv_rewrite_record(path, edited)), prv_frame()),
      class = "samplyr_error_panel_record_malformed",
      info = case
    )
  }

  # The one absence that is not a defect: no pool columns at all is one pool
  # holding every unit, which is what an unstratified first stage has.
  single_pool <- prv_master()
  expect_identical(prv_record(single_pool)$pool_vars, character(0))
  replayed <- replay_design(read_design(prv_file(single_pool)), prv_frame())
  expect_identical(replayed$.panel, single_pool$.panel)
})

test_that("the refusal names the version and the field", {
  record <- list(
    algorithm = "blocked_random_quota", version = 3L, panels = 4L,
    unit = "cluster", key_vars = "psu", pool_vars = character(0)
  )
  err <- tryCatch(
    samplyr:::prepare_panel_record(record, "An activation"),
    samplyr_error_panel_record_malformed = function(cnd) cnd
  )
  expect_match(conditionMessage(err), "version-3", fixed = TRUE)
  expect_match(conditionMessage(err), "assignment_stage", fixed = TRUE)
  expect_match(conditionMessage(err), "1 or more", fixed = TRUE)
})

test_that("an in-memory record is read under the same rule", {
  # Activation, joint moments and stacking read the record the execution left
  # on the sample, so they answer to it as well.
  master <- prv_master(prv_schedule())
  broken <- master
  attr(broken, "metadata")$panel_assignment$unit <- "psu"

  expect_error(
    execute(broken, wave = 1),
    class = "samplyr_error_panel_record_malformed"
  )
})

## Every path that reads a record reads its version, and so does the writer

# In-memory rather than through a file, because these are the paths that read
# the record an execution left on the sample.
prv_mutated <- function(x, field, value) {
  attr(x, "metadata")$panel_assignment[field] <- list(value)
  x
}

prv_broken <- list(
  "no stage" = list("assignment_stage", NULL, "malformed"),
  "fractional stage" = list("assignment_stage", 2.5, "malformed"),
  "retired unit" = list("unit", "psu", "malformed"),
  "unknown version" = list("version", 99L, "unsupported"),
  "unknown algorithm" = list("algorithm", "some_other_law", "unsupported")
)

prv_class <- function(case) {
  paste0("samplyr_error_panel_record_", case[[3]])
}

test_that("the writer refuses a record it would have to repair to write", {
  # Writing is reading: a receipt is the record as a later reader will find
  # it, so a writer that fills in a field the record does not carry produces a
  # well-formed file describing an assignment nothing recorded.
  master <- prv_master(panel_stage = 2)
  path <- withr::local_tempfile(fileext = ".json")

  for (case in names(prv_broken)) {
    spec <- prv_broken[[case]]
    broken <- prv_mutated(master, spec[[1]], spec[[2]])
    expect_error(
      write_design(broken, path, frame = prv_frame()),
      class = prv_class(spec), info = case
    )
    expect_error(design_json(broken), class = prv_class(spec), info = case)
    # The same receipt, built in memory rather than from a file.
    expect_error(
      replay_design(broken, prv_frame()),
      class = prv_class(spec), info = case
    )
  }

  # Refused before the file exists, not after it is written and then regretted.
  expect_false(file.exists(path))

  # Each refusal names the call the user made. The encoder is shared, so
  # without the public call threaded through it every one of them would be
  # reported against an internal function nobody invoked.
  stageless <- prv_mutated(master, "assignment_stage", NULL)
  named_by <- function(expr) {
    err <- tryCatch(expr, samplyr_error_panel_record_malformed = function(e) e)
    as.character(conditionCall(err)[[1]])
  }
  expect_identical(
    named_by(write_design(stageless, path, frame = prv_frame())),
    "write_design"
  )
  expect_identical(named_by(design_json(stageless)), "design_json")
  expect_identical(
    named_by(replay_design(stageless, prv_frame())), "replay_design"
  )
})

test_that("materializing, pairing and programming read the version first", {
  master <- prv_master(prv_schedule())
  program_schedule <- transform(prv_schedule(), cohort = "A")

  for (case in names(prv_broken)) {
    spec <- prv_broken[[case]]
    broken <- prv_mutated(master, spec[[1]], spec[[2]])
    cls <- prv_class(spec)
    expect_error(execute(broken, wave = 1), class = cls, info = case)
    expect_error(
      joint_expectation(broken, waves = c(1, 2)), class = cls, info = case
    )
    # A program is built out of numbers taken from these records, so it is
    # refused where it is declared rather than at its first wave.
    expect_error(
      rotation_program(
        cohorts = list(A = broken),
        entry_wave = c(A = 1),
        schedule = program_schedule
      ),
      class = cls, info = case
    )
  }

  program <- rotation_program(
    cohorts = list(A = master),
    entry_wave = c(A = 1),
    schedule = program_schedule
  )
  expect_s3_class(execute(program, wave = 1), "rotation_wave")
})

test_that("a survey export reads the version before the quotas it exports", {
  skip_if_not_installed("survey")
  master <- prv_master(prv_schedule())
  wave <- execute(master, wave = 1)
  expect_s3_class(as_svydesign(wave), "twophase2")

  # The phase-2 units, blocks and probabilities are this algorithm's and this
  # schema's. An export is where a misread record becomes an object that looks
  # estimable.
  for (case in names(prv_broken)) {
    spec <- prv_broken[[case]]
    expect_error(
      as_svydesign(prv_mutated(wave, spec[[1]], spec[[2]])),
      class = prv_class(spec), info = case
    )
  }
})

test_that("stacking waves reads the version of the record it stacks", {
  master <- prv_master(prv_schedule())
  wave1 <- execute(master, wave = 1)
  wave2 <- execute(master, wave = 2)
  # A long table of both waves, not a sample: stacking is for analysis.
  expect_s3_class(stack_waves(wave1, wave2), "tbl_df")

  expect_error(
    stack_waves(prv_mutated(wave1, "version", 99L), wave2),
    class = "samplyr_error_panel_record_unsupported"
  )
})

test_that("an unreadable record is not reported as one that declares no waves", {
  # Whether a record declares waves is a fact about its fields, so a record
  # whose law is unknown must not be described by one of them. The two facts
  # are separated by removing the schedule as well: before the version was
  # read first, this was a missing schedule.
  master <- prv_master(prv_schedule())
  unreadable <- prv_mutated(
    prv_mutated(master, "version", 99L), "schedule", NULL
  )
  expect_error(
    execute(unreadable, wave = 1),
    class = "samplyr_error_panel_record_unsupported"
  )
  expect_error(
    joint_expectation(unreadable, waves = c(1, 2)),
    class = "samplyr_error_panel_record_unsupported"
  )

  # And a readable record that declares no waves still says exactly that.
  partitioned <- prv_master()
  expect_error(
    execute(partitioned, wave = 1),
    class = "samplyr_error_wave_no_schedule"
  )
  expect_error(
    joint_expectation(partitioned, waves = c(1, 2)),
    class = "samplyr_error_wave_no_schedule"
  )
})
