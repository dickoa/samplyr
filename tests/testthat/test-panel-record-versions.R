## Reading an assignment record under the version it states

# A record may come from an older samplyr, a newer one, or another tool. The
# version stamp says which law the fields follow, so it is read before them.

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
    period = rep(1:2, each = 4),
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

# These helpers write each version's fields literally. A legacy artifact built
# by the current writer would agree with the current reader by construction.

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

# The record replaced by a bare JSON value, as a truncated or hand-edited file
# carries. The literal is written as text, because serializing an R scalar
# would decide whether the file holds a scalar or an array of one.
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

# Setting a field to a value, including to nothing at all. Assignment rather
# than a merge: merging recurses into a list-valued field and leaves a column
# list looking exactly as it did.
within_record <- function(record, field, value) {
  record[field] <- list(value)
  record
}

## A receipt replays as the assignment it records

test_that("a first-stage receipt reproduces its assignment", {
  master <- prv_master()
  path <- prv_file(master)

  stored <- prv_stored_record(path)
  expect_identical(stored$version, 7L)
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

  # Pools and blocks set the denominators of every later activation.
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

  # P2 and P5 are each hit twice, and P5's hits fall in different panels.
  occurrences <- unique(data.frame(
    psu = master$psu, draw = master$.draw_1, panel = master$.panel
  ))
  expect_identical(nrow(occurrences), 6L)
  expect_identical(sort(unique(master$psu)), c("P2", "P3", "P5", "P6"))
  expect_identical(occurrences$draw[occurrences$psu == "P5"], c(1L, 5L))
  expect_identical(occurrences$panel[occurrences$psu == "P5"], c(2L, 1L))

  path <- prv_file(master, frame)
  stored <- prv_stored_record(path)
  # The file names the occurrence columns, and replay rebuilds the pools.
  expect_identical(unlist(stored$key_vars), c("psu", ".draw_1"))
  expect_identical(stored$unit, "occurrence")
  expect_null(stored$pools)

  keys <- record$pools[[1]]$keys
  expect_identical(length(keys), 6L)
  expect_identical(anyDuplicated(keys), 0L)

  replayed <- replay_design(read_design(path), frame)
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(replayed$.draw_1, master$.draw_1)
  expect_identical(prv_record(replayed)$pools[[1]]$keys, record$pools[[1]]$keys)
})

## A frame without the assignment ancestry

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
  # Ignoring the fingerprint, the stage that needs the column refuses.
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

  # A decoder that ran before the version check would fail here.
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

  # A bare JSON value parses to a length-1 atomic vector, not a list.
  bare <- list(
    "a number" = "3",
    "a string" = '"blocked_random_quota"',
    "a boolean" = "true"
  )

  for (case in names(bare)) {
    scalar_path <- prv_replace_record(path, bare[[case]])
    stored <- prv_stored_record(scalar_path)
    # A splice that produced a list again would make this case vacuous.
    expect_false(is.list(stored), info = case)
    expect_length(stored, 1L)

    expect_error(
      replay_design(read_design(scalar_path), prv_frame()),
      class = "samplyr_error_panel_record_malformed",
      info = case
    )
  }

  # An empty object states no algorithm, so it is unsupported, not malformed.
  expect_error(
    replay_design(read_design(prv_replace_record(path, "{}")), prv_frame()),
    class = "samplyr_error_panel_record_unsupported"
  )

  # Refused before any decoder runs.
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

test_that("bad file records name the reader and bad in-memory records name replay", {
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
  # read_design() refuses this inside replay_design()'s x argument.
  expect_identical(as.character(conditionCall(err)[[1]]), "read_design")
  direct <- tryCatch(read_design(number),
    samplyr_error_panel_record_malformed = function(cnd) cnd)
  expect_identical(as.character(conditionCall(direct)[[1]]), "read_design")

  attr(master, "metadata")$panel_assignment <- 3L
  in_memory <- tryCatch(replay_design(master, prv_frame()),
    samplyr_error_panel_record_malformed = function(cnd) cnd)
  expect_identical(as.character(conditionCall(in_memory)[[1]]), "replay_design")
})

test_that("in-memory paths refuse a record that is not a record", {
  # Reading and writing both, on records that never went through a file.
  scheduled <- prv_master(prv_schedule())
  cls <- "samplyr_error_panel_record_malformed"

  for (value in list(3L, "blocked_random_quota", TRUE, numeric(0))) {
    broken <- scheduled
    attr(broken, "metadata")$panel_assignment <- value
    case <- typeof(value)

    expect_error(execute(broken, period = 1), class = cls, info = case)
    expect_error(
      joint_expectation(broken, period = c(1, 2)), class = cls, info = case
    )
    expect_error(design_json(broken), class = cls, info = case)
    expect_error(replay_design(broken, prv_frame()), class = cls, info = case)
    expect_error(
      samplyr:::prepare_panel_record(value, "An activation"), class = cls,
      info = case
    )
  }
})

## A record of an earlier version is refused

test_that("every record version before 7 is refused with a hint", {
  master <- prv_master(prv_schedule())
  path <- prv_file(master)
  for (version in 1:6) {
    record <- prv_stored_record(path)
    record$version <- version
    err <- expect_error(
      read_design(prv_rewrite_record(path, record)),
      class = "samplyr_error_panel_record_unsupported"
    )
    text <- cli::ansi_strip(conditionMessage(err))
    expect_match(text, paste0("The record is version ", version), info = version)
    expect_match(text, "Execute the design again", info = version)
  }
})

## What a record has to carry

test_that("a record missing or misstating a field is refused", {
  master <- prv_master(panel_stage = 2)
  path <- prv_file(master)
  record <- prv_stored_record(path)

  # Each case assigns one field, since a merge leaves column lists unchanged.
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

  # No pool columns is one pool holding every unit, as when unstratified.
  single_pool <- prv_master()
  expect_identical(prv_record(single_pool)$pool_vars, character(0))
  replayed <- replay_design(read_design(prv_file(single_pool)), prv_frame())
  expect_identical(replayed$.panel, single_pool$.panel)
})

test_that("the refusal names the version and the field", {
  record <- list(
    algorithm = "blocked_random_quota", version = 7L, panels = 4L,
    certainty = "permanent",
    unit = "cluster", key_vars = "psu", pool_vars = character(0)
  )
  err <- tryCatch(
    samplyr:::prepare_panel_record(record, "An activation"),
    samplyr_error_panel_record_malformed = function(cnd) cnd
  )
  expect_match(conditionMessage(err), "version-7", fixed = TRUE)
  expect_match(conditionMessage(err), "assignment_stage", fixed = TRUE)
  expect_match(conditionMessage(err), "1 or more", fixed = TRUE)
})

test_that("an in-memory record is read under the same rule", {
  master <- prv_master(prv_schedule())
  broken <- master
  attr(broken, "metadata")$panel_assignment$unit <- "psu"

  expect_error(
    execute(broken, period = 1),
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
  # Filling in a missing field would write an assignment nothing recorded.
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

  # Refused before the file exists.
  expect_false(file.exists(path))

  # The shared encoder reports each refusal against the public call.
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
  program_schedule <- transform(prv_schedule(), selection = "A")

  for (case in names(prv_broken)) {
    spec <- prv_broken[[case]]
    broken <- prv_mutated(master, spec[[1]], spec[[2]])
    cls <- prv_class(spec)
    expect_error(execute(broken, period = 1), class = cls, info = case)
    expect_error(
      joint_expectation(broken, period = c(1, 2)), class = cls, info = case
    )
    # A program is refused where it is declared, not at its first occasion.
    expect_error(
      rotation_program(
        selections = list(A = broken),
        entry_period = c(A = 1),
        schedule = program_schedule
      ),
      class = cls, info = case
    )
  }

  program <- rotation_program(
    selections = list(A = master),
    entry_period = c(A = 1),
    schedule = program_schedule
  )
  expect_s3_class(execute(program, period = 1), "rotation_occasion")
})

test_that("a survey export reads the version before the quotas it exports", {
  skip_if_not_installed("survey")
  master <- prv_master(prv_schedule())
  occasion <- execute(master, period = 1)
  expect_s3_class(as_svydesign(occasion), "survey.design2")
  expect_s3_class(as_svydesign(occasion, method = "full"), "twophase2")

  # A misread record would export as an object that looks estimable.
  for (case in names(prv_broken)) {
    spec <- prv_broken[[case]]
    broken <- prv_mutated(occasion, spec[[1]], spec[[2]])
    expect_error(
      as_svydesign(broken),
      class = prv_class(spec), info = case
    )
    expect_error(
      as_svydesign(broken, method = "full"),
      class = prv_class(spec), info = case
    )
    expect_error(
      as_svrepdesign(broken),
      class = prv_class(spec), info = case
    )
  }
})

test_that("stacking occasions reads the version of the record it stacks", {
  master <- prv_master(prv_schedule())
  occasion1 <- execute(master, period = 1)
  occasion2 <- execute(master, period = 2)
  # A long table of both occasions, not a sample: stacking is for analysis.
  expect_s3_class(stack_periods(occasion1, occasion2), "tbl_df")

  expect_error(
    stack_periods(prv_mutated(occasion1, "version", 99L), occasion2),
    class = "samplyr_error_panel_record_unsupported"
  )
})

test_that("an unreadable record is not reported as one that declares no occasions", {
  # The schedule is removed too, so the version must be read first.
  master <- prv_master(prv_schedule())
  unreadable <- prv_mutated(
    prv_mutated(master, "version", 99L), "schedule", NULL
  )
  expect_error(
    execute(unreadable, period = 1),
    class = "samplyr_error_panel_record_unsupported"
  )
  expect_error(
    joint_expectation(unreadable, period = c(1, 2)),
    class = "samplyr_error_panel_record_unsupported"
  )

  # And a readable record that declares no occasions still says exactly that.
  partitioned <- prv_master()
  expect_error(
    execute(partitioned, period = 1),
    class = "samplyr_error_occasion_no_schedule"
  )
  expect_error(
    joint_expectation(partitioned, period = c(1, 2)),
    class = "samplyr_error_occasion_no_schedule"
  )
})
