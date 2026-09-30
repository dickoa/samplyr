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

# The pools a version-1 or version-2 file carried. The current writer emits
# the assignment law and not the realized pools, so the block is written here.
prv_legacy_pools <- function(live) {
  lapply(live$pools, function(pool) {
    out <- list()
    if (!is.null(pool$stratum)) {
      out$stratum <- lapply(pool$stratum, function(v) as.character(v)[1])
    }
    out$class <- pool$class
    out$size <- as.integer(pool$size)
    out$keys <- as.list(as.character(pool$keys))
    out$blocks <- as.list(as.integer(pool$blocks))
    out$quotas <- lapply(
      seq_len(nrow(pool$quotas)),
      function(b) as.list(as.integer(pool$quotas[b, ]))
    )
    out
  })
}

prv_as_version_1 <- function(record, live) {
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
    pools = prv_legacy_pools(live)
  )
}

# Version 2 added the small-pool policy and separated a pool's activation from
# its selection class. It still had one assignment stage and no pool columns.
prv_as_version_2 <- function(record, live) {
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
    # Version 2 adds activation to the version-1 pool block.
    pools = lapply(seq_along(live$pools), function(i) {
      pool <- live$pools[[i]]
      out <- prv_legacy_pools(live)[[i]]
      out <- c(
        out[intersect(c("stratum", "class"), names(out))],
        list(activation = pool$activation),
        if (!is.na(pool$permanent_reason %||% NA_character_)) {
          list(permanent_reason = pool$permanent_reason)
        },
        out[intersect(c("size", "keys", "blocks", "quotas"), names(out))]
      )
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
  legacy <- prv_as_version_1(prv_stored_record(path), prv_record(master))

  # The downgrade removes what version 1 never had.
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
  # The single-PSU pool in stratum C rotates to nothing and is promoted.
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
  legacy <- prv_as_version_2(prv_stored_record(path), prv_record(master))

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

  # Without the policy, as in version 1, the default refuses this draw.
  as_v1 <- prv_as_version_1(prv_stored_record(path), prv_record(master))
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
  # Stage 2 assigns differently, so reading the extraneous field would show.
  master <- prv_master()
  lower <- prv_master(panel_stage = 2)
  expect_false(identical(master$.panel, lower$.panel))

  path <- prv_file(master)
  legacy <- prv_as_version_1(prv_stored_record(path), prv_record(master))
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
  legacy <- prv_as_version_2(prv_stored_record(path), prv_record(master))
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
  program_schedule <- transform(prv_schedule(), cohort = "A")

  for (case in names(prv_broken)) {
    spec <- prv_broken[[case]]
    broken <- prv_mutated(master, spec[[1]], spec[[2]])
    cls <- prv_class(spec)
    expect_error(execute(broken, wave = 1), class = cls, info = case)
    expect_error(
      joint_expectation(broken, waves = c(1, 2)), class = cls, info = case
    )
    # A program is refused where it is declared, not at its first wave.
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

  # A misread record would export as an object that looks estimable.
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
  # The schedule is removed too, so the version must be read first.
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
