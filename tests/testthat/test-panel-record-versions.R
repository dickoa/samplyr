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
  expect_identical(stored$version, 8L)
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

test_that("every record version before 8 is refused with a hint", {
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
    expect_match(text, "indexes its schedule by", info = version)
    expect_match(text, "Execute the design again", info = version)
  }

  # Version 7 wrote the keys of its units by the scipen-dependent rule. A
  # receipt and a saved sample are refused alike. The hint promises no
  # redraw of the old sample, since later fixes can change what one seed
  # draws.
  record <- prv_stored_record(path)
  record$version <- 7L
  saved <- master
  attr(saved, "metadata")$panel_assignment$version <- 7L
  routes <- list(
    receipt = function() read_design(prv_rewrite_record(path, record)),
    sample = function() execute(saved, period = 1)
  )
  for (route in names(routes)) {
    err <- expect_error(
      routes[[route]](),
      class = "samplyr_error_panel_record_unsupported", info = route
    )
    text <- cli::ansi_strip(conditionMessage(err))
    expect_match(text, "The record is version 7, which writes the keys",
                 info = route)
    expect_match(text, "Execute the design again to write a current record",
                 info = route)
    expect_no_match(text, "same seed|same sample", info = route)
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
    algorithm = "blocked_random_quota", version = 8L, panels = 4L,
    certainty = "permanent",
    unit = "cluster", key_vars = "psu", pool_vars = character(0)
  )
  err <- tryCatch(
    samplyr:::prepare_panel_record(record, "An activation"),
    samplyr_error_panel_record_malformed = function(cnd) cnd
  )
  expect_match(conditionMessage(err), "version-8", fixed = TRUE)
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
  # A saved sample's version 7 pools write their keys by the earlier rule.
  "retired version 7" = list("version", 7L, "unsupported"),
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

## Unit keys written by an earlier rule

# Version 7 wrote a whole double such as 400000 as "4e+05" under the default
# scipen. Read by the current rule, no row of the sample matched its record,
# and an occasion came back empty without a word.

prv_numeric_master <- function(stratified) {
  psu <- data.frame(
    psu_id = 1e5 * (1:12),
    size = rep(c(10, 12, 9), 4),
    region = rep(c(1e5, 2e5), 6)
  )
  el <- data.frame(
    psu_id = rep(psu$psu_id, psu$size),
    unit_id = seq_len(sum(psu$size))
  )
  rotation <- data.frame(
    panel = rep(1:2, 2), period = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  d <- sampling_design() |> add_stage("psu")
  if (stratified) d <- stratify_by(d, region)
  d <- d |> cluster_by(psu_id) |>
    draw(n = 3, method = "pps_brewer", mos = size) |>
    add_stage("el") |> draw(n = 4)
  suppressWarnings(execute(
    d, list(psu = psu, el = el), seed = 1, panels = rotation
  ))
}

test_that("a fresh record activates the same rows under any scipen", {
  for (stratified in c(FALSE, TRUE)) {
    master <- withr::with_options(
      list(scipen = 0), prv_numeric_master(stratified)
    )
    low <- withr::with_options(list(scipen = 0), execute(master, period = 1))
    high <- withr::with_options(list(scipen = 100), execute(master, period = 1))
    expect_gt(nrow(low), 0L)
    expect_identical(low$unit_id, high$unit_id)
    expect_identical(low$.weight, high$.weight)
  }
})

test_that("keys of the earlier spelling are refused, not read as no unit", {
  master <- withr::with_options(list(scipen = 0), prv_numeric_master(TRUE))
  meta <- attr(master, "metadata")
  record <- meta$panel_assignment
  # Each key rewritten from the typed columns, by the rule version 7 used.
  typed <- unique(as.data.frame(master)[record$key_vars])
  current <- samplyr:::make_group_key(typed, record$key_vars)
  parts <- withr::with_options(list(scipen = 0), lapply(typed, as.character))
  legacy <- if (length(parts) == 1L) {
    parts[[1L]]
  } else {
    encoded <- lapply(parts, function(x) {
      paste0("V", nchar(x, type = "bytes"), ":", x)
    })
    do.call(paste0, c(list(paste0(length(parts), "|")), unname(encoded)))
  }
  for (i in seq_along(record$pools)) {
    at <- match(record$pools[[i]]$keys, current)
    expect_false(anyNA(at))
    record$pools[[i]]$keys <- legacy[at]
  }
  expect_true(any(grepl("e+05", legacy, fixed = TRUE)))
  meta$panel_assignment <- record
  attr(master, "metadata") <- meta
  expect_error(
    execute(master, period = 1),
    class = "samplyr_error_panel_record_malformed"
  )
})

## A unit listed in two pools

# Counting matched rows let a unit listed in two pools hide a unit listed in
# none, and the occasion came back short without a word. The writer puts each
# unit in one pool, so a full record is read only when every row is found in
# exactly one.

prv_overlapping <- function(x, lost_pool, from_pool) {
  meta <- attr(x, "metadata")
  record <- meta$panel_assignment
  record$pools[[lost_pool]]$keys[1L] <- record$pools[[from_pool]]$keys[1L]
  meta$panel_assignment <- record
  attr(x, "metadata") <- meta
  x
}

test_that("a saved record with a unit in two pools is refused before any row", {
  master <- withr::with_options(list(scipen = 0), prv_numeric_master(TRUE))
  record <- prv_record(master)
  expect_identical(record$version, 8L)
  expect_gte(length(record$pools), 2L)
  keys <- samplyr:::make_group_key(as.data.frame(master), record$key_vars)
  # The two first units hold the same number of rows, so a count of matched
  # rows is unchanged by the damage.
  first <- vapply(record$pools[1:2], function(pool) pool$keys[1L], "")
  expect_identical(sum(keys == first[1L]), sum(keys == first[2L]))

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(prv_overlapping(master, lost_pool = 2L, from_pool = 1L), path)
  damaged <- readRDS(path)
  expect_error(
    execute(damaged, period = 1),
    class = "samplyr_error_panel_record_malformed"
  )
  expect_error(
    execute(damaged, period = 1:2),
    class = "samplyr_error_panel_record_malformed"
  )
})

test_that("a window's survey export refuses a unit in two pools", {
  skip_if_not_installed("survey")
  master <- withr::with_options(list(scipen = 0), prv_numeric_master(TRUE))
  # A window carries the record into its survey export, which reads the
  # same pools.
  window <- execute(master, period = 1:2)
  expect_error(
    suppressWarnings(
      as_svydesign(prv_overlapping(window, lost_pool = 2L, from_pool = 1L))
    ),
    class = "samplyr_error_panel_record_malformed"
  )
  expect_s3_class(suppressWarnings(as_svydesign(window)), "survey.design")
})

prv_without_pools <- function(x, pools) {
  meta <- attr(x, "metadata")
  # NULL drops the field, as a record written without it reads.
  meta$panel_assignment$pools <- pools
  attr(x, "metadata") <- meta
  x
}

test_that("a saved record with no pools names none of the sample's rows", {
  frame <- data.frame(id = seq_len(12L))
  rotation <- data.frame(
    panel = rep(1:2, 2), period = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
  master <- execute(sampling_design() |> draw(n = 8), frame, seed = 1,
                    panels = rotation)
  expect_identical(prv_record(master)$version, 8L)
  expect_identical(nrow(execute(master, period = 1)), 4L)
  checked <- character(0)
  for (case in c("empty", "omitted")) {
    path <- withr::local_tempfile(fileext = ".rds")
    saveRDS(prv_without_pools(master, if (case == "empty") list()), path)
    damaged <- readRDS(path)
    expect_identical("pools" %in% names(prv_record(damaged)), case == "empty",
                     info = case)
    for (period in list(1L, 1:2)) {
      err <- expect_error(
        execute(damaged, period = period),
        class = "samplyr_error_panel_record_malformed", info = case
      )
      expect_match(cli::ansi_strip(conditionMessage(err)),
                   "8 of 8 rows belong to no pool", info = case)
    }
    checked <- c(checked, case)
  }
  expect_setequal(checked, c("empty", "omitted"))
})

test_that("a window's survey export refuses a record with no pools", {
  skip_if_not_installed("survey")
  master <- withr::with_options(list(scipen = 0), prv_numeric_master(TRUE))
  window <- execute(master, period = 1:2)
  for (pools in list(list(), NULL)) {
    expect_error(
      suppressWarnings(as_svydesign(prv_without_pools(window, pools))),
      class = "samplyr_error_panel_record_malformed"
    )
  }
})

## The layout of a saved record's pools

# Readers index blocks and quotas by position, so a pool laid out another
# way reads as a different probability law. Each fixture below damages one
# field of a valid version 8 master before it is read.

prv_two_periods <- function() {
  data.frame(
    panel = rep(1:2, 2), period = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
}

prv_edit_pool <- function(x, edit) {
  meta <- attr(x, "metadata")
  meta$panel_assignment <- edit(meta$panel_assignment)
  attr(x, "metadata") <- meta
  x
}

test_that("a unit listed twice in one pool is refused, not read in its first block", {
  psus <- data.frame(psu = 1:9)
  listing <- data.frame(psu = rep(psus$psu, each = 2L), id = 1:18)
  design <- sampling_design() |>
    add_stage("psu") |> cluster_by(psu) |> draw(n = 9) |>
    add_stage("element") |> draw(n = 2, on_empty = "silent")
  first <- suppressMessages(suppressWarnings(execute(
    design, psus, stages = 1, seed = 1, panels = prv_two_periods()
  )))
  pool <- prv_record(first)$pools[[1L]]
  expect_identical(as.integer(pool$blocks), c(5L, 4L))
  absent <- as.integer(pool$keys[1L])
  affected <- as.integer(pool$keys[pool$blocks[1L] + 1L])
  period <- first$.panel[match(affected, first$psu)]

  # The first PSU keeps no element, and its key is still in the record.
  master <- execute(first, listing[listing$psu != absent, ], stages = 2,
                    seed = 1)
  expect_false(any(master$psu == absent))
  valid <- execute(master, period = period)
  # All 9 PSUs and both elements of each are taken. The second block holds
  # 4 PSUs, 2 per panel, so an occasion keeps half of them.
  expect_equal(valid$.weight[valid$psu == affected], c(2, 2))

  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(prv_edit_pool(master, function(r) {
    r$pools[[1L]]$keys[1L] <- as.character(affected)
    r
  }), path)
  damaged <- readRDS(path)
  for (periods in list(period, 1:2)) {
    err <- expect_error(
      execute(damaged, period = periods),
      class = "samplyr_error_panel_record_malformed"
    )
    expect_match(cli::ansi_strip(conditionMessage(err)),
                 "Pool 1 lists 1 unit twice")
  }
})

test_that("a pool whose fields disagree is refused before any weight", {
  frame <- data.frame(id = 1:12)
  master <- suppressMessages(execute(
    sampling_design() |> draw(n = 9), frame, seed = 1,
    panels = prv_two_periods()
  ))
  expect_identical(nrow(execute(master, period = 1)), 5L)
  window <- execute(master, period = 1:2)
  damage <- list(
    "pools not a list" = function(r) within_record(r, "pools", "P1"),
    "atomic pool" = function(r) { r$pools[[1L]] <- 1L; r },
    "keys as numbers" = function(r) {
      r$pools[[1L]]$keys <- seq_along(r$pools[[1L]]$keys)
      r
    },
    "duplicate key" = function(r) {
      r$pools[[1L]]$keys[2L] <- r$pools[[1L]]$keys[1L]
      r
    },
    "unknown activation" = function(r) {
      r$pools[[1L]]$activation <- "typo"
      r
    },
    "excess key" = function(r) {
      r$pools[[1L]]$keys <- c(r$pools[[1L]]$keys, "extra")
      r
    },
    "wrong size" = function(r) { r$pools[[1L]]$size <- 8L; r },
    "block sizes" = function(r) { r$pools[[1L]]$blocks <- c(2L, 2L); r },
    "zero block" = function(r) {
      r$pools[[1L]]$blocks <- c(r$pools[[1L]]$blocks, 0L)
      r
    },
    "missing quotas" = function(r) { r$pools[[1L]]$quotas <- NULL; r },
    "quotas as a list" = function(r) {
      r$pools[[1L]]$quotas <- as.list(r$pools[[1L]]$quotas)
      r
    },
    "a column too few" = function(r) {
      r$pools[[1L]]$quotas <- r$pools[[1L]]$quotas[, 1L, drop = FALSE]
      r
    },
    "negative quota" = function(r) {
      q <- r$pools[[1L]]$quotas
      q[1L, ] <- c(q[1L, 1L] + q[1L, 2L] + 1L, -1L)
      r$pools[[1L]]$quotas <- q
      r
    },
    "fractional quota" = function(r) {
      q <- r$pools[[1L]]$quotas * 1
      q[1L, ] <- q[1L, ] + c(0.5, -0.5)
      r$pools[[1L]]$quotas <- q
      r
    },
    "quotas above blocks" = function(r) {
      r$pools[[1L]]$quotas[, 1L] <- r$pools[[1L]]$blocks + 1L
      r
    },
    "unit in two pools" = function(r) {
      r$pools[[2L]] <- r$pools[[1L]]
      r
    }
  )
  checked <- character(0)
  for (case in names(damage)) {
    routes <- list(
      period = function() execute(prv_edit_pool(master, damage[[case]]),
                                  period = 1),
      window = function() execute(prv_edit_pool(master, damage[[case]]),
                                  period = 1:2)
    )
    for (route in names(routes)) {
      expect_error(
        routes[[route]](),
        class = "samplyr_error_panel_record_malformed",
        info = paste(case, route)
      )
      checked <- c(checked, paste(case, route))
    }
  }
  expect_length(unique(checked), 2L * length(damage))
})

test_that("a window's survey export refuses a pool whose fields disagree", {
  skip_if_not_installed("survey")
  master <- withr::with_options(list(scipen = 0), prv_numeric_master(TRUE))
  window <- execute(master, period = 1:2)
  expect_s3_class(suppressWarnings(as_svydesign(window)), "survey.design")
  expect_error(
    suppressWarnings(as_svydesign(prv_edit_pool(window, function(r) {
      r$pools[[1L]]$quotas <- r$pools[[1L]]$quotas[, 2:1, drop = FALSE] + 0L
      r$pools[[1L]]$quotas[1L, ] <- r$pools[[1L]]$quotas[1L, ] + c(1L, 0L)
      r
    }))),
    class = "samplyr_error_panel_record_malformed"
  )
})

test_that("quotas that do not match the units' panels are refused", {
  frame <- data.frame(id = 1:12)
  master <- suppressMessages(execute(
    sampling_design() |> draw(n = 9), frame, seed = 1,
    panels = prv_two_periods()
  ))
  quotas <- prv_record(master)$pools[[1L]]$quotas
  # Swapping the panels keeps every row sum, so the layout is valid.
  swapped <- quotas[, 2:1, drop = FALSE]
  expect_identical(rowSums(swapped), rowSums(quotas))
  expect_false(identical(swapped, quotas))
  damaged <- prv_edit_pool(master, function(r) {
    r$pools[[1L]]$quotas <- swapped
    r
  })
  for (periods in list(1L, 2L, 1:2)) {
    err <- expect_error(
      execute(damaged, period = periods),
      class = "samplyr_error_panel_record_malformed"
    )
    expect_match(cli::ansi_strip(conditionMessage(err)),
                 "do not match its block quotas")
  }
  # Two rows of one unit carrying two panels.
  expect_error(
    check_pool_panels(
      record_pool_rows(c(prv_record(master)$pools[[1L]]$keys[1L],
                         prv_record(master)$pools[[1L]]$keys[1L]),
                       prv_record(master)$pools, complete = FALSE),
      prv_record(master)$pools, c(1L, 2L)
    ),
    class = "samplyr_error_panel_record_malformed"
  )
})

test_that("a receipt is read without the pools it lists", {
  master <- prv_master(prv_schedule())
  path <- prv_file(master)
  record <- prv_stored_record(path)
  # A pool no reader of a saved sample would accept: one unit twice.
  record$pools <- list(list(keys = I(c("P1", "P1")), size = 2L,
                            blocks = I(1L)))
  edited <- prv_rewrite_record(path, record)
  replayed <- replay_design(read_design(edited), prv_frame())
  expect_identical(replayed$psu, master$psu)
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(replayed$.weight, master$.weight)
})

## Version 7 receipts

# Until the first release a build reads the current record version only,
# from a design file as from a saved sample. A version 7 receipt is the
# current receipt with the panel record's version set to 7, so these
# fixtures are built by setting it on the lines the writer printed.

prv_as_version <- function(path, version, which) {
  lines <- readLines(path)
  stamps <- grep('^ *"version": 8,$', lines)
  lines[stamps[which]] <- sub("8,", paste0(version, ","), lines[stamps[which]],
                              fixed = TRUE)
  out <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  writeLines(lines, out)
  out
}

prv_numeric_frames <- function() {
  psu <- data.frame(
    psu_id = 1e5 * (1:12),
    size = rep(c(10, 12, 9), 4),
    region = rep(c(1e5, 2e5), 6)
  )
  el <- data.frame(
    psu_id = rep(psu$psu_id, psu$size),
    unit_id = seq_len(sum(psu$size))
  )
  list(psu = psu, el = el)
}

prv_numeric_design <- function(stratified) {
  design <- sampling_design() |> add_stage("psu")
  if (stratified) design <- stratify_by(design, region)
  design |> cluster_by(psu_id) |>
    draw(n = 3, method = "pps_brewer", mos = size) |>
    add_stage("el") |> draw(n = 4)
}

prv_rotation <- function() {
  data.frame(
    panel = rep(1:2, 2), period = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  )
}

test_that("a version 7 receipt is refused, and a version 8 one replays", {
  withr::local_options(scipen = 0)
  frames <- prv_numeric_frames()
  checked <- character(0)
  for (case in c("plain", "stratified", "lower stage")) {
    master <- suppressMessages(suppressWarnings(execute(
      prv_numeric_design(case != "plain"), frames, seed = 1,
      panels = prv_rotation(), panel_stage = if (case == "lower stage") 2L
    )))
    path <- withr::local_tempfile(fileext = ".json")
    write_design(master, path, frame = frames)
    old <- prv_as_version(path, 7L, 1L)
    expect_identical(prv_stored_record(old)$version, 7L, info = case)
    expect_error(
      read_design(old),
      class = "samplyr_error_panel_record_unsupported", info = case
    )

    # The current receipt and the saved sample keep the master.
    replayed <- suppressMessages(
      replay_design(read_design(path), frames, fingerprint = "error")
    )
    saved <- withr::local_tempfile(fileext = ".rds")
    saveRDS(master, saved)
    restored <- readRDS(saved)
    for (copy in list(replayed, restored)) {
      expect_identical(copy$unit_id, master$unit_id, info = case)
      expect_identical(copy$.panel, master$.panel, info = case)
      expect_identical(copy$.weight, master$.weight, info = case)
      for (period in 1:2) {
        active <- execute(copy, period = period)
        expected <- execute(master, period = period)
        expect_identical(active$unit_id, expected$unit_id, info = case)
        expect_identical(active$.weight, expected$.weight, info = case)
      }
    }
    expect_identical(prv_record(replayed), prv_record(master), info = case)

    # A frame that changed is still refused.
    changed <- frames
    changed$psu$size[1] <- 11
    expect_error(
      suppressMessages(
        replay_design(read_design(path), changed, fingerprint = "error")
      ),
      class = "samplyr_error_replay_frame_mismatch", info = case
    )
    checked <- c(checked, case)
  }
  expect_setequal(checked, c("plain", "stratified", "lower stage"))
})

test_that("every call of a chained receipt is read under the current version", {
  withr::local_options(scipen = 0)
  frames <- prv_numeric_frames()
  master <- suppressMessages(suppressWarnings(execute(
    prv_numeric_design(TRUE), frames, seed = 1, panels = prv_rotation()
  )))
  occasion <- execute(master, period = 2)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(occasion, path, frame = list(frames))
  # The occasion's call writes its record first, the master's call second.
  stamps <- grep('^ *"version": 8,$', readLines(path))
  expect_length(stamps, 2L)
  expect_gt(stamps[2], grep('"earlier_calls"', readLines(path)))

  # The current chain replays the occasion.
  replayed <- suppressMessages(
    replay_design(read_design(path), list(frames), fingerprint = "error")
  )
  expect_identical(replayed$unit_id, occasion$unit_id)
  expect_identical(replayed$.panel, occasion$.panel)
  expect_identical(replayed$.weight, occasion$.weight)

  # A version 7 record is refused in either call, and before any replay.
  for (which in 1:2) {
    expect_error(
      read_design(prv_as_version(path, 7L, which)),
      class = "samplyr_error_panel_record_unsupported", info = which
    )
  }
  decoded <- read_design(path)
  calls <- attr(decoded, "execution")$earlier_calls
  expect_length(calls, 1L)
  for (field in c("version", "algorithm")) {
    stale <- decoded
    value <- if (field == "version") 7L else "some_other_law"
    attr(stale, "execution")$earlier_calls[[1]]$panel_assignment[[field]] <- value
    expect_error(
      suppressMessages(replay_design(stale, list(frames))),
      class = "samplyr_error_panel_record_unsupported", info = field
    )
    # The occasion's own call reads its master's record, and is read too.
    stale <- decoded
    attr(stale, "execution")$panel_assignment[[field]] <- value
    expect_error(
      suppressMessages(replay_design(stale, list(frames))),
      class = "samplyr_error_panel_record_unsupported", info = field
    )
  }
})

## A stored schedule laid out as the assignment wrote it

# A saved schedule is the complete grid of the record's panels by its
# occasions, each row with a whole panel, period, subround and occasion, a
# complete activation flag and a visit where the panel is active. A grid
# read with a value outside it failed inside base R or read other panels.

prv_edit_schedule <- function(master, edit) {
  meta <- attr(master, "metadata")
  meta$panel_assignment$schedule <- edit(meta$panel_assignment$schedule)
  attr(master, "metadata") <- meta
  master
}

prv_schedule_damages <- list(
  "panel outside the record" = function(s) {
    s$panel[which(s$active)[1L]] <- 5L
    s
  },
  "missing activation flag" = function(s) {
    s$active[1L] <- NA
    s
  },
  "no activation column" = function(s) {
    s$active <- NULL
    s
  },
  "not a table" = function(s) 1L,
  "fractional period" = function(s) {
    s$period[1L] <- 1.5
    s
  },
  "no occasion index" = function(s) {
    s$occasion <- NULL
    s
  },
  "occasions numbered out of order" = function(s) {
    s$occasion <- 3L - s$occasion
    s
  },
  "a row left out" = function(s) s[-nrow(s), ],
  "a row repeated" = function(s) rbind(s, s[1L, ]),
  "a visit on an inactive row" = function(s) {
    s$visit[which(!s$active)[1L]] <- 1L
    s
  },
  "an occasion with no active panel" = function(s) {
    s$active[s$occasion == 1L] <- FALSE
    s$visit[s$occasion == 1L] <- NA
    s
  }
)

test_that("a saved schedule laid out another way is refused before any panel is read", {
  master <- prv_master(prv_schedule())
  expect_identical(nrow(execute(master, period = 1)), 4L)
  checked <- character(0)
  for (damage in names(prv_schedule_damages)) {
    damaged <- prv_edit_schedule(master, prv_schedule_damages[[damage]])
    expect_error(execute(damaged, period = 1),
                 class = "samplyr_error_panel_record_malformed", info = damage)
    expect_error(joint_expectation(damaged, period = c(1L, 2L)),
                 class = "samplyr_error_panel_record_malformed", info = damage)
    checked <- c(checked, damage)
  }
  expect_setequal(checked, names(prv_schedule_damages))

  err <- expect_error(
    execute(prv_edit_schedule(master, prv_schedule_damages[[1L]]),
            period = 1),
    class = "samplyr_error_panel_record_malformed"
  )
  expect_match(gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err))),
               "panel of each schedule row from 1 to 4, and row 1",
               fixed = TRUE)
  err <- expect_error(
    execute(prv_edit_schedule(master, prv_schedule_damages[[2L]]),
            period = 1),
    class = "samplyr_error_panel_record_malformed"
  )
  expect_match(gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err))),
               "row 1 (panel 1, period 1) does not", fixed = TRUE)
})

test_that("a receipt's schedule laid out another way is refused when read", {
  master <- prv_master(prv_schedule())
  path <- prv_file(master)
  stored <- prv_stored_record(path)
  expect_null(stored$schedule[[1L]]$occasion)
  rows <- list(
    "panel outside the record" = function(s) {
      s[[1L]]$panel <- 5L
      s
    },
    "missing activation flag" = function(s) {
      s[[1L]]$active <- NULL
      s
    },
    "a row left out" = function(s) s[-1L],
    "not a table" = function(s) 1L
  )
  for (damage in names(rows)) {
    record <- stored
    record$schedule <- rows[[damage]](record$schedule)
    out <- prv_rewrite_record(path, record)
    expect_error(read_design(out),
                 class = "samplyr_error_panel_record_malformed", info = damage)
  }
  # The record as written reads and replays.
  replayed <- suppressMessages(replay_design(read_design(path), prv_frame()))
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(prv_record(replayed)$schedule, prv_record(master)$schedule)
})

test_that("every schedule the assignment writes is read as written", {
  frame <- prv_frame()
  frames <- list(prv_frame())
  masters <- list(
    "periods" = prv_master(prv_schedule()),
    "inactive panels and revisits" = prv_master(data.frame(
      panel = rep(1:4, times = 3), period = rep(1:3, each = 4),
      active = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE,
                 TRUE, FALSE, FALSE, TRUE)
    )),
    "subrounds of a period" = prv_master(data.frame(
      panel = rep(1:4, times = 2), period = 1L,
      subround = rep(1:2, each = 4),
      active = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
    )),
    "subrounds()" = suppressMessages(execute(
      prv_design(), frame, seed = 31, panels = subrounds(2)
    )),
    "lower stage" = prv_master(data.frame(
      panel = rep(1:2, 2), period = rep(1:2, each = 2),
      active = c(TRUE, FALSE, FALSE, TRUE)
    ), panel_stage = 2)
  )
  checked <- character(0)
  for (kind in names(masters)) {
    master <- masters[[kind]]
    record <- prv_record(master)
    expect_identical(prepare_panel_record(record, "A test"), record,
                     info = kind)
    path <- prv_file(master)
    replayed <- suppressMessages(replay_design(read_design(path), frame))
    expect_identical(replayed$.panel, master$.panel, info = kind)
    expect_identical(replayed$.weight, master$.weight, info = kind)
    expect_identical(prv_record(replayed)$schedule, record$schedule,
                     info = kind)
    saved <- withr::local_tempfile(fileext = ".rds")
    saveRDS(master, saved)
    restored <- readRDS(saved)
    first <- grid_pairs(record$schedule)[1L, ]
    expect_identical(
      execute(restored, period = first$period, subround = first$subround)$.weight,
      execute(master, period = first$period, subround = first$subround)$.weight,
      info = kind
    )
    checked <- c(checked, kind)
  }
  expect_setequal(checked, names(masters))
})

test_that("an active-only schedule is saved as the full grid and a deleted row is refused", {
  # The schedule given to execute() lists only the active rows. Every
  # combination left out is inactive, and the record writes them all.
  sparse <- prv_schedule()[prv_schedule()$active, c("panel", "period")]
  master <- prv_master(sparse)
  expect_identical(prv_record(master)$schedule,
                   prv_record(prv_master(prv_schedule()))$schedule)
  path <- prv_file(master)
  stored <- prv_stored_record(path)
  expect_length(stored$schedule, 8L)
  expect_true(all(vapply(stored$schedule, function(row) {
    setequal(names(row), c("panel", "period", "subround", "active", "visit"))
  }, logical(1))))

  replayed <- suppressMessages(
    replay_design(read_design(path), prv_frame(), fingerprint = "error")
  )
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(replayed$.weight, master$.weight)
  expect_identical(prv_record(replayed)$schedule, prv_record(master)$schedule)

  # Without the row, replay would read the panel as inactive there.
  active <- which(vapply(stored$schedule, function(row) isTRUE(row$active),
                         logical(1)))
  for (row in active) {
    damaged <- stored
    damaged$schedule <- stored$schedule[-row]
    expect_error(read_design(prv_rewrite_record(path, damaged)),
                 class = "samplyr_error_panel_record_malformed", info = row)
  }
})

test_that("a schedule cut to fewer occasions that changes the block size is refused", {
  # Period 2 activates one panel, so r_min is 1 and blocks hold 3 * 2 = 6
  # units. Without period 2 every occasion activates two panels, r_min is 2
  # and blocks would hold 3 units, another assignment of the same sample.
  frame <- data.frame(id = 1:40)
  schedule <- data.frame(panel = rep(1:3, 2), period = rep(1:2, each = 3),
                         active = c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE))
  master <- suppressMessages(execute(sampling_design() |> draw(n = 24), frame,
                                     seed = 1, panels = schedule))
  record <- prv_record(master)
  expect_identical(c(record$r_min, record$block_size), c(1L, 6L))

  path <- withr::local_tempfile(fileext = ".json")
  write_design(master, path, frame = frame)
  stored <- prv_stored_record(path)
  stored$schedule <- Filter(function(row) row$period == 1L, stored$schedule)
  err <- expect_error(read_design(prv_rewrite_record(path, stored)),
                      class = "samplyr_error_panel_record_malformed")
  expect_match(gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err))),
               "schedule gives, 2 and 3", fixed = TRUE)

  # The saved sample is read under the same rule.
  cut <- prv_edit_schedule(master, function(s) s[s$period == 1L, ])
  expect_error(execute(cut, period = 1),
               class = "samplyr_error_panel_record_malformed")
})

test_that("a schedule with cohorts keeps its visits in the design file", {
  # Visit 1 after the first opens a new cohort. Read without visits, the
  # schedule would hold one cohort per panel and replay another design.
  eas <- data.frame(ea = 1:60, h = rep(c("a", "b"), each = 30))
  schedule <- expand.grid(panel = 1:3, period = 1:12)
  schedule$active <- (schedule$period - 1) %% 3 == (schedule$panel - 1) %% 3
  schedule$visit <- ifelse(schedule$active,
                           ((schedule$period - 1) %/% 3) %% 2 + 1, NA)
  design <- sampling_design() |>
    add_stage() |> stratify_by(h) |> cluster_by(ea) |> draw(n = 9) |>
    add_stage() |> cluster_by(hh) |> draw(n = 4)
  master <- suppressMessages(execute(design, eas, stages = 1, seed = 1,
                                     panels = schedule))
  expect_true(schedule_has_cohorts(prv_record(master)$schedule))
  path <- withr::local_tempfile(fileext = ".json")
  write_design(master, path, frame = eas)
  stored <- prv_stored_record(path)
  stored$schedule <- lapply(stored$schedule, function(row) {
    row$visit <- NULL
    row
  })
  expect_error(read_design(prv_rewrite_record(path, stored)),
               class = "samplyr_error_panel_record_malformed")
})

## The schedule checksum of a design file

# A design file records the MD5 checksum of its schedule rows. An edit that
# leaves a complete grid with the same block size can still change the
# assignment or an occasion, and only the checksum shows it.

prv_subround_master <- function(schedule = NULL) {
  if (is_null(schedule)) {
    schedule <- expand.grid(panel = 1:4, subround = 1:2, period = 1:2)
    schedule$active <- seq_len(nrow(schedule)) %in%
      c(1, 2, 3, 4, 5, 9, 11, 12, 16)
  }
  suppressMessages(execute(sampling_design() |> draw(n = 12),
                           data.frame(id = 1:40), seed = 1,
                           panels = schedule))
}

test_that("a design file records the MD5 checksum of its schedule rows", {
  master <- prv_master(data.frame(
    panel = rep(1:2, 2), period = rep(1:2, each = 2),
    active = c(TRUE, FALSE, FALSE, TRUE)
  ))
  # The rows as text, in order of period, subround and panel.
  text <- "1,1,1,true,1\n2,1,1,false,\n1,2,1,false,\n2,2,1,true,1\n"
  file <- withr::local_tempfile()
  writeBin(charToRaw(text), file)
  stored <- prv_stored_record(prv_file(master))
  expect_identical(stored$schedule_checksum, unname(tools::md5sum(file)))

  # The rows are read in any order.
  path <- prv_file(master)
  shuffled <- prv_stored_record(path)
  shuffled$schedule <- rev(shuffled$schedule)
  replayed <- suppressMessages(replay_design(
    read_design(prv_rewrite_record(path, shuffled)), prv_frame(),
    fingerprint = "error"
  ))
  expect_identical(replayed$.panel, master$.panel)
})

test_that("a schedule edited after its execution is refused when read", {
  master <- prv_subround_master()
  frame <- data.frame(id = 1:40)
  path <- withr::local_tempfile(fileext = ".json")
  write_design(master, path, frame = frame)
  stored <- prv_stored_record(path)

  # Without period 2 the block size stays 6, but units change panel.
  first <- prv_record(master)$schedule
  first <- first[first$period == 1L, c("panel", "period", "subround",
                                       "active")]
  cut <- prv_subround_master(first)
  expect_identical(prv_record(cut)$block_size, prv_record(master)$block_size)
  expect_identical(prv_record(cut)$r_min, prv_record(master)$r_min)
  expect_true(any(cut$.panel != master$.panel))

  edits <- list(
    "final period removed" = function(r) {
      r$schedule <- Filter(function(row) row$period == 1L, r$schedule)
      r
    },
    # Panel 3's activity at period 2 moves to panel 2, so each occasion
    # keeps its count of active panels.
    "active flag moved" = function(r) {
      at <- function(p) {
        which(vapply(r$schedule, function(row) {
          row$panel == p && row$period == 2L && row$subround == 1L
        }, logical(1)))
      }
      r$schedule[[at(3L)]]$active <- FALSE
      r$schedule[[at(3L)]]["visit"] <- list(NULL)
      r$schedule[[at(2L)]]$active <- TRUE
      r$schedule[[at(2L)]]$visit <- 2L
      r
    },
    "checksum changed" = function(r) {
      r$schedule_checksum <- strrep("0", 32L)
      r
    }
  )
  for (edit in names(edits)) {
    err <- expect_error(
      read_design(prv_rewrite_record(path, edits[[edit]](stored))),
      class = "samplyr_error_panel_record_malformed", info = edit
    )
    expect_match(gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err))),
                 "is not the one its execution recorded", fixed = TRUE,
                 info = edit)
  }
  missing <- stored
  missing["schedule_checksum"] <- list(NULL)
  expect_error(read_design(prv_rewrite_record(path, missing)),
               class = "samplyr_error_panel_record_malformed")
})

## A design file's schedule as written in the JSON text

# These edits are made to the text, since writing a value back through R
# can change how it is spelled, as 1.0 becomes 1.

prv_two_panel_file <- function(panels) {
  frame <- data.frame(id = 1:24)
  master <- suppressMessages(execute(sampling_design() |> draw(n = 12), frame,
                                     seed = 1, panels = panels))
  path <- withr::local_tempfile(fileext = ".json",
                                .local_envir = parent.frame())
  write_design(master, path, frame = frame)
  list(master = master, path = path, frame = frame)
}

prv_edit_text <- function(path, edit) {
  out <- withr::local_tempfile(fileext = ".json",
                               .local_envir = parent.frame())
  writeLines(edit(readLines(path)), out)
  out
}

test_that("a checksum without its schedule is refused, not read as no schedule", {
  file <- prv_two_panel_file(prv_two_periods())
  lines <- readLines(file$path)
  opens <- grep('^      "schedule": \\[$', lines)
  expect_length(opens, 1L)
  closes <- opens + grep("^      \\],$", lines[-seq_len(opens)])[1L]
  expect_true(any(grepl('"schedule_checksum": "[0-9a-f]{32}"',
                        lines[-seq_len(closes)])))
  replaced <- function(by) {
    function(x) c(x[seq_len(opens - 1L)], by, x[-seq_len(closes)])
  }
  edits <- list(
    removed = replaced(character(0)),
    null = replaced('      "schedule": null,'),
    empty = replaced('      "schedule": [],')
  )
  for (edit in names(edits)) {
    expect_error(read_design(prv_edit_text(file$path, edits[[edit]])),
                 class = "samplyr_error_panel_record_malformed", info = edit)
  }

  # An assignment with no schedule records no checksum and replays.
  plain <- prv_two_panel_file(2L)
  stored <- prv_stored_record(plain$path)
  expect_null(stored$schedule)
  expect_null(stored$schedule_checksum)
  replayed <- suppressMessages(replay_design(read_design(plain$path),
                                             plain$frame,
                                             fingerprint = "error"))
  expect_identical(replayed$.panel, plain$master$.panel)
})

test_that("replay reads the schedule as checked, whatever the number spelling", {
  file <- prv_two_panel_file(prv_two_periods())
  first <- function(pattern, x) grep(pattern, x, fixed = TRUE)[1L]
  respelled <- prv_edit_text(file$path, function(x) {
    at <- first('"panel": 1,', x)
    x[at] <- sub('"panel": 1,', '"panel": 1.0,', x[at], fixed = TRUE)
    at <- grep('"visit": 1$', x)[1L]
    x[at] <- sub('"visit": 1', '"visit": 1.0', x[at], fixed = TRUE)
    x
  })
  expect_true(any(grepl('"panel": 1.0,', readLines(respelled), fixed = TRUE)))
  expect_identical(prv_stored_record(respelled)$schedule_checksum,
                   prv_stored_record(file$path)$schedule_checksum)

  record <- prepare_receipt_record(
    attr(read_design(respelled), "execution")$panel_assignment, "A replay"
  )
  expect_true(is.data.frame(record$schedule))
  expect_true(all(vapply(record$schedule[c("panel", "period", "subround",
                                           "visit")],
                         is.integer, logical(1))))
  replayed <- suppressMessages(replay_design(read_design(respelled),
                                             file$frame,
                                             fingerprint = "error"))
  expect_identical(replayed$id, file$master$id)
  expect_identical(replayed$.panel, file$master$.panel)
  expect_identical(replayed$.weight, file$master$.weight)

  bad <- list(
    "period as text" = c('"period": 1,', '"period": "1",'),
    "fractional panel" = c('"panel": 1,', '"panel": 1.5,'),
    "panel beyond integers" = c('"panel": 1,', '"panel": 3000000000,'),
    "fractional visit" = c('"visit": 1', '"visit": 1.5')
  )
  for (case in names(bad)) {
    edited <- prv_edit_text(file$path, function(x) {
      at <- first(bad[[case]][1L], x)
      x[at] <- sub(bad[[case]][1L], bad[[case]][2L], x[at], fixed = TRUE)
      x
    })
    expect_error(read_design(edited),
                 class = "samplyr_error_panel_record_malformed", info = case)
  }
})
