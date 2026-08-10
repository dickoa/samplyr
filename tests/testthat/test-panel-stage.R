## Stage-aware panel assignment
#
# `panel_stage` names the stage whose selected units are assigned to panels.
# Stage 1 rotates whole primary units; a lower stage rotates units inside
# parents that stay in the survey, which is the address-panel design.

ps_frame <- function() {
  frame <- expand.grid(
    person = 1:2, hh = 1:6, psu = sprintf("P%d", 1:6),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  frame <- frame[order(frame$psu, frame$hh, frame$person), ]
  frame$hh_stratum <- ifelse(frame$hh <= 3, "small", "large")
  frame$y <- seq_len(nrow(frame))
  rownames(frame) <- NULL
  frame
}

ps_design <- function(hh_take = 4) {
  sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 3) |>
    add_stage() |> cluster_by(hh) |> draw(n = hh_take) |>
    add_stage() |> draw(n = 1)
}

ps_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 3),
    wave = rep(1:3, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE
    )
  )
}

ps_master <- function() {
  execute(
    ps_design(), ps_frame(),
    seed = 77, panels = ps_schedule(), panel_stage = 2
  )
}

ps_record <- function(x) attr(x, "metadata")$panel_assignment

## Assignment below the first stage

test_that("panel_stage = 2 rotates households inside retained parents", {
  master <- ps_master()

  # Every selected PSU carries more than one panel, which stage-1 assignment
  # cannot produce.
  by_psu <- tapply(master$.panel, master$psu, function(x) length(unique(x)))
  expect_length(by_psu, 3L)
  expect_true(all(by_psu == 4L))

  # A household is one assignment unit, and its people inherit its panel.
  by_hh <- tapply(
    master$.panel, paste(master$psu, master$hh),
    function(x) length(unique(x))
  )
  expect_true(all(by_hh == 1L))
})

test_that("a wave of a lower-stage master keeps every parent", {
  master <- ps_master()
  wave <- execute(master, wave = 2)

  # The PSUs stay in the survey and the households rotate within them: this
  # is the design the stage-aware assignment exists for.
  expect_identical(sort(unique(wave$psu)), sort(unique(master$psu)))
  expect_identical(nrow(wave), nrow(master) %/% 2L)
  # Four households per PSU, two panels of four active: the activation
  # probability is 1/2 and the master weight of 6 doubles.
  expect_identical(unique(wave$.weight), 12)
  expect_identical(sort(unique(wave$.panel)), c(2L, 3L))
})

test_that("the lower-stage record states its stage, unit and pools", {
  record <- ps_record(ps_master())

  expect_identical(record$version, 3L)
  expect_identical(record$assignment_stage, 2L)
  expect_identical(record$unit, "cluster")
  # The household is identified inside the PSU it sits in.
  expect_identical(record$key_vars, c("psu", "hh"))
  # One pool per realized PSU: a block that crossed a parent would stop
  # guaranteeing rotation within every parent.
  expect_identical(record$pool_vars, "psu")
  expect_length(record$pools, 3L)
  expect_identical(
    vapply(record$pools, function(p) p$stratum$psu, character(1)),
    sort(unique(as.data.frame(ps_master())$psu))
  )
  for (pool in record$pools) {
    expect_identical(pool$size, 4L)
    expect_identical(pool$quotas, matrix(1L, nrow = 1, ncol = 4))
  }
})

test_that("a lower-stage stratified stage pools inside each parent", {
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> stratify_by(hh_stratum) |> cluster_by(hh) |> draw(n = 2) |>
    add_stage() |> draw(n = 1)
  record <- ps_record(
    execute(design, ps_frame(), seed = 88, panels = 2, panel_stage = 2)
  )

  expect_identical(record$pool_vars, c("psu", "hh_stratum"))
  expect_identical(record$key_vars, c("psu", "hh_stratum", "hh"))
  # Two PSUs, two household strata in each.
  expect_length(record$pools, 4L)
})

test_that("a terminal stage assigns its own rows", {
  record <- ps_record(
    execute(ps_design(), ps_frame(), seed = 77, panels = 2, panel_stage = 3)
  )

  expect_identical(record$assignment_stage, 3L)
  expect_identical(record$unit, "element")
  expect_identical(record$key_vars, ".sample_id")
  expect_identical(record$pool_vars, c("psu", "hh"))
})

test_that("panel_stage = 1 is what omitting it already did", {
  design <- ps_design()
  frame <- ps_frame()

  explicit <- execute(
    design, frame, seed = 77, panels = ps_schedule(), panel_stage = 1
  )
  implicit <- execute(design, frame, seed = 77, panels = ps_schedule())

  expect_identical(explicit$.panel, implicit$.panel)
  expect_identical(explicit$.weight, implicit$.weight)
  expect_identical(ps_record(explicit), ps_record(implicit))
})

## Refusals

test_that("panel_stage without panels is refused", {
  expect_error(
    execute(ps_design(), ps_frame(), seed = 1, panel_stage = 2),
    class = "samplyr_error_panel_stage_not_applicable"
  )
})

test_that("panel_stage must be a single stage number", {
  for (bad in list(0, -1, 2.5, c(1, 2), "two", NA_integer_)) {
    expect_error(
      execute(ps_design(), ps_frame(), seed = 1, panels = 2,
              panel_stage = bad),
      class = "samplyr_error_panel_stage_value"
    )
  }
})

test_that("panel_stage must name a stage the execution completes", {
  # Beyond the design.
  expect_error(
    execute(ps_design(), ps_frame(), seed = 1, panels = 2, panel_stage = 4),
    class = "samplyr_error_panel_stage_unexecuted"
  )
  # Inside the design but not in this partial execution.
  expect_error(
    execute(ps_design(), ps_frame(), stages = 1, seed = 1, panels = 2,
            panel_stage = 2),
    class = "samplyr_error_panel_stage_unexecuted"
  )
})

test_that("an unexecuted panel_stage is refused before any draw is taken", {
  # A static misuse must not leave the stream advanced, or a later seeded
  # execution in the same session would differ.
  after <- withr::with_seed(4, {
    try(
      execute(ps_design(), ps_frame(), seed = 1, panels = 2, panel_stage = 4),
      silent = TRUE
    )
    runif(1)
  })
  expect_identical(after, withr::with_seed(4, runif(1)))
})

test_that("a misspelled panel_stage is named, not read as a frame", {
  expect_error(
    execute(ps_design(), ps_frame(), seed = 1, panels = 2, panel_stag = 2),
    class = "samplyr_error_unknown_argument"
  )
})

test_that("a wave takes no panel_stage", {
  expect_error(
    execute(ps_master(), wave = 2, panel_stage = 2),
    class = "samplyr_error_wave_extra_arguments"
  )
})

test_that("a within-parent pool too small to rotate is refused", {
  # Two households per PSU against four panels with two active: a wave
  # activating the other two would take nothing from any PSU. Within-parent
  # pools are small by nature, which is why this matters more here than at
  # stage 1.
  expect_error(
    execute(ps_design(hh_take = 2), ps_frame(), seed = 77,
            panels = ps_schedule(), panel_stage = 2),
    class = "samplyr_error_panel_small_pool"
  )
  # And the opt-in still promotes rather than refuses.
  expect_warning(
    promoted <- execute(
      ps_design(hh_take = 2), ps_frame(), seed = 77,
      panels = ps_schedule(), panel_stage = 2, small_pool = "permanent"
    ),
    class = "samplyr_warning_panel_small_pool"
  )
  expect_true(all(vapply(
    ps_record(promoted)$pools,
    function(p) identical(p$permanent_reason, "small_pool"),
    logical(1)
  )))
})

## Continuations

test_that("a continuation can assign from a stage it has just executed", {
  design <- ps_design()
  frame <- ps_frame()
  stage1 <- execute(design, frame, stages = 1, seed = 3)
  continued <- execute(
    stage1, frame, stages = 2:3, seed = 4, panels = 2, panel_stage = 2
  )

  record <- ps_record(continued)
  expect_identical(record$assignment_stage, 2L)
  expect_identical(record$key_vars, c("psu", "hh"))
})

test_that("a continuation can assign from a stage executed earlier", {
  design <- ps_design()
  frame <- ps_frame()
  stage1 <- execute(design, frame, stages = 1, seed = 3)
  continued <- execute(
    stage1, frame, stages = 2:3, seed = 4, panels = 2, panel_stage = 1
  )

  record <- ps_record(continued)
  expect_identical(record$assignment_stage, 1L)
  expect_identical(record$key_vars, "psu")
})

## Serialization and replay

test_that("the assignment stage survives a receipt round trip", {
  master <- ps_master()
  path <- withr::local_tempfile(fileext = ".json")
  suppressWarnings(write_design(master, path))
  encoded <- jsonlite::fromJSON(
    path, simplifyVector = FALSE
  )$execution$panel_assignment

  expect_identical(encoded$version, 3L)
  expect_identical(encoded$assignment_stage, 2L)
  expect_identical(encoded$unit, "cluster")
  expect_identical(unlist(encoded$key_vars), c("psu", "hh"))
  expect_identical(unlist(encoded$pool_vars), "psu")
})

test_that("replaying a lower-stage master reproduces its assignment", {
  master <- ps_master()
  path <- withr::local_tempfile(fileext = ".json")
  suppressWarnings(write_design(master, path))
  replayed <- replay_design(read_design(path), ps_frame())

  expect_identical(replayed$.panel, master$.panel)
  expect_identical(replayed$.weight, master$.weight)
  expect_identical(ps_record(replayed), ps_record(master))
})

test_that("replay passes the assignment stage back rather than defaulting", {
  # Without this the receipt replays through an `execute()` that assigns from
  # stage 1, which reproduces a different assignment of the same sample
  # instead of failing.
  master <- ps_master()
  path <- withr::local_tempfile(fileext = ".json")
  suppressWarnings(write_design(master, path))
  receipt <- jsonlite::fromJSON(path, simplifyVector = FALSE)$execution

  expect_identical(
    samplyr:::decode_panel_stage_argument(
      samplyr:::prepare_panel_record(receipt$panel_assignment, "A replay")
    ),
    2L
  )

  as_stage_1 <- execute(
    ps_design(), ps_frame(), seed = 77, panels = ps_schedule()
  )
  expect_false(identical(as_stage_1$.panel, master$.panel))
})

test_that("a first-stage receipt replays without the argument", {
  master <- execute(
    ps_design(), ps_frame(), seed = 77, panels = ps_schedule()
  )
  path <- withr::local_tempfile(fileext = ".json")
  suppressWarnings(write_design(master, path))
  receipt <- jsonlite::fromJSON(path, simplifyVector = FALSE)$execution

  # Stage 1 is what an omitted argument already means, so nothing is passed.
  expect_null(
    samplyr:::decode_panel_stage_argument(
      samplyr:::prepare_panel_record(receipt$panel_assignment, "A replay")
    )
  )
})

## Reading the earlier record versions

test_that("versions 1 and 2 normalize to a first-stage assignment", {
  v1 <- list(
    algorithm = "blocked_random_quota", version = 1L, panels = 2L,
    unit = "psu", key_vars = "psu",
    pools = list(list(class = "rotating", size = 2L))
  )
  v2 <- list(
    algorithm = "blocked_random_quota", version = 2L, panels = 2L,
    unit = "element", key_vars = ".sample_id",
    small_pool_policy = "permanent",
    pools = list(list(
      class = "rotating", activation = "permanent",
      permanent_reason = "small_pool", size = 1L
    ))
  )

  n1 <- samplyr:::normalize_panel_record(v1)
  expect_identical(n1$assignment_stage, 1L)
  expect_identical(n1$small_pool_policy, "error")
  expect_identical(n1$pools[[1]]$activation, "rotating")

  n2 <- samplyr:::normalize_panel_record(v2)
  expect_identical(n2$assignment_stage, 1L)
  expect_identical(n2$small_pool_policy, "permanent")
  expect_identical(n2$pools[[1]]$permanent_reason, "small_pool")
})

test_that("an older record keeps the unit vocabulary it was written with", {
  # An unclustered with-replacement assignment recorded "element" and keyed
  # on `.sample_id`, so nothing in the record says whether the stage was
  # multi-hit. Translating it to "occurrence" would be inventing a fact.
  v2 <- list(
    algorithm = "blocked_random_quota", version = 2L, panels = 2L,
    unit = "element", key_vars = ".sample_id", pools = list()
  )
  expect_identical(samplyr:::normalize_panel_record(v2)$unit, "element")

  v1 <- list(
    algorithm = "blocked_random_quota", version = 1L, panels = 2L,
    unit = "psu", key_vars = "psu", pools = list()
  )
  expect_identical(samplyr:::normalize_panel_record(v1)$unit, "psu")
})

test_that("all three versions are supported and a fourth is not", {
  expect_identical(samplyr:::supported_panel_record_versions, c(1L, 2L, 3L))
  expect_error(
    samplyr:::check_panel_record_supported(
      list(algorithm = "blocked_random_quota", version = 4L), "A wave"
    ),
    class = "samplyr_error_panel_record_unsupported"
  )
})

## One assignment per master

# An assignment is a record and a `.panel` column together. Removing either
# used to leave a sample that looked unassigned to the continuation guard,
# which tested only the column.

ps_stage1_assigned <- function() {
  execute(ps_design(), ps_frame(), stages = 1, seed = 3, panels = 2)
}

test_that("panels cannot be redeclared once a master carries an assignment", {
  stage1 <- ps_stage1_assigned()

  # Both representations present, which is what an unmodified master has.
  expect_error(
    suppressWarnings(execute(
      stage1, ps_frame(), stages = 2:3, seed = 4, panels = 2
    )),
    class = "samplyr_error_panels_already_assigned"
  )

  # The record alone. It is still frozen in the receipt, so this would
  # replace a stage-1 assignment with a stage-2 one.
  record_only <- stage1
  record_only$.panel <- NULL
  expect_error(
    suppressWarnings(execute(
      record_only, ps_frame(), stages = 2:3, seed = 4,
      panels = 2, panel_stage = 2
    )),
    class = "samplyr_error_panels_already_assigned"
  )

  # The column alone, with no record behind it. A guard reading only the
  # record would let this one through and assign a second time.
  column_only <- execute(ps_design(), ps_frame(), stages = 1, seed = 3)
  column_only$.panel <- rep(1:2, length.out = nrow(column_only))
  expect_null(attr(column_only, "metadata")$panel_assignment)
  expect_error(
    suppressWarnings(execute(
      column_only, ps_frame(), stages = 2:3, seed = 4, panels = 2
    )),
    class = "samplyr_error_panels_already_assigned"
  )
})

test_that("a half-assignment is refused even without new panels", {
  stage1 <- ps_stage1_assigned()

  without_column <- stage1
  without_column$.panel <- NULL
  expect_error(
    suppressWarnings(execute(without_column, ps_frame(), stages = 2:3, seed = 4)),
    class = "samplyr_error_panel_assignment_incomplete"
  )

  # The other direction: labels with no provenance.
  without_record <- execute(ps_design(), ps_frame(), stages = 1, seed = 3)
  without_record$.panel <- rep(1:2, length.out = nrow(without_record))
  expect_error(
    suppressWarnings(execute(without_record, ps_frame(), stages = 2:3, seed = 4)),
    class = "samplyr_error_panel_assignment_incomplete"
  )
})

test_that("a complete assignment still continues", {
  stage1 <- ps_stage1_assigned()
  continued <- execute(stage1, ps_frame(), stages = 2:3, seed = 4)

  expect_true(".panel" %in% names(continued))
  expect_identical(ps_record(continued)$assignment_stage, 1L)
  # A continuation with neither a record nor a column is not a
  # half-assignment: it is a sample with no panels.
  plain <- execute(ps_design(), ps_frame(), stages = 1, seed = 3)
  expect_false(
    ".panel" %in% names(execute(plain, ps_frame(), stages = 2:3, seed = 4))
  )
})
