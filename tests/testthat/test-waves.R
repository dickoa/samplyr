## Rotation schedules and wave materialization

wave_frame <- function(n = 400) {
  data.frame(
    id = seq_len(n),
    region = rep(c("North", "South"), each = n / 2),
    score = rep((seq_len(n / 2) * 7L) %% 97L, times = 2),
    value = seq_len(n) / n
  )
}

# 4 panels over 4 waves, two active at a time: the "2-2" rotation, so
# r_min = 2 and the block size collapses from 2K to K.
rotation_2_2 <- function() {
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

# One panel at a time, so r_min = 1 and the block size stays at 2K.
rotation_1_of_4 <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    wave = rep(1:4, each = 4),
    active = as.logical(diag(4))
  )
}

master_2_2 <- function(seed = 42) {
  sampling_design() |>
    stratify_by(region) |>
    draw(n = 60) |>
    execute(wave_frame(), seed = seed, panels = rotation_2_2())
}

## Schedule validation

test_that("a schedule declares the panel count and the block size", {
  spec <- normalize_panel_input(rotation_2_2())

  expect_identical(spec$k, 4L)
  expect_identical(spec$r_min, 2L)
  expect_identical(spec$block_size, 4L)
  expect_identical(nrow(spec$schedule), 16L)
  expect_identical(names(spec$schedule), c("wave", "panel", "active"))

  # A single active panel per wave is the worst case and keeps 2K.
  lean <- normalize_panel_input(rotation_1_of_4())
  expect_identical(lean$r_min, 1L)
  expect_identical(lean$block_size, 8L)
})

test_that("a schedule naming only its active rows is completed", {
  full <- rotation_2_2()
  sparse <- full[full$active, c("panel", "wave")]

  expect_identical(
    normalize_panel_input(sparse)$schedule,
    normalize_panel_input(full)$schedule
  )
})

test_that("schedule defects are refused by kind", {
  expect_error(
    normalize_panel_input(data.frame(panel = 1:4)),
    class = "samplyr_error_panel_schedule_columns"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 1), wave = c(1, 1))),
    class = "samplyr_error_panel_schedule_duplicates"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 3), wave = c(1, 1))),
    class = "samplyr_error_panel_schedule_gap"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 2), wave = c(1, 3))),
    class = "samplyr_error_panel_schedule_gap"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 2), wave = c(1, 1))) |>
      suppressWarnings(),
    NA
  )
  expect_error(
    normalize_panel_input(data.frame(panel = 1:2, wave = c(1, 1.5))),
    class = "samplyr_error_panel_schedule_values"
  )
  expect_error(
    normalize_panel_input(
      data.frame(panel = 1:2, wave = c(1, 1), active = c("y", "n"))
    ),
    class = "samplyr_error_panel_schedule_active"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 1), wave = c(1, 2))),
    class = "samplyr_error_panel_schedule_size"
  )
  expect_error(
    normalize_panel_input(
      data.frame(
        panel = rep(1:2, 2),
        wave = rep(1:2, each = 2),
        active = c(TRUE, TRUE, FALSE, FALSE)
      )
    ),
    class = "samplyr_error_panel_schedule_idle_wave"
  )
})

test_that("a schedule assigns panels exactly as a count would, but blocked
           to its own r_min", {
  master <- master_2_2()
  record <- attr(master, "metadata")$panel_assignment

  expect_identical(record$block_size, 4L)
  expect_identical(record$r_min, 2L)
  expect_identical(
    record$schedule,
    normalize_panel_input(rotation_2_2())$schedule
  )

  # Sizes are still exact and the marginal contract is unchanged.
  expect_identical(as.integer(table(master$.panel)), rep(30L, 4))
  for (pool in record$pools) {
    expect_true(all(pool$blocks == 4L))
    expect_identical(as.integer(rowSums(pool$quotas)), pool$blocks)
  }
})

## Materialization

test_that("a wave activates exactly the scheduled panels", {
  master <- master_2_2()

  for (t in 1:4) {
    active <- rotation_2_2()
    active <- active$panel[active$wave == t & active$active]

    materialized <- execute(master, wave = t)
    expect_setequal(unique(materialized$.panel), active)
    expect_identical(
      sort(materialized$.sample_id),
      sort(master$.sample_id[master$.panel %in% active])
    )
  }
})

test_that("the activation factor is the block take, and totals are preserved", {
  master <- master_2_2()
  materialized <- execute(master, wave = 1)

  # Two of four panels, blocks of four with a quota of one each: the
  # conditional probability is 2/4 and the factor its inverse.
  factor <- materialized$.weight /
    master$.weight[match(materialized$.sample_id, master$.sample_id)]
  expect_identical(unique(factor), 2)

  # The frame total is estimated without bias at either stage of the story.
  expect_equal(sum(materialized$.weight), sum(master$.weight))
  expect_equal(sum(master$.weight), 400)
})

test_that("a single-panel wave carries the wider factor", {
  master <- sampling_design() |>
    draw(n = 80) |>
    execute(wave_frame(), seed = 5, panels = rotation_1_of_4())

  materialized <- execute(master, wave = 3)
  factor <- materialized$.weight /
    master$.weight[match(materialized$.sample_id, master$.sample_id)]

  # Blocks of 2K = 8 with a quota of two per panel: 2/8, so the factor is 4.
  expect_identical(unique(factor), 4)
  expect_identical(nrow(materialized), 20L)
})

test_that("the recorded probability matches the realized take of each block", {
  master <- master_2_2()
  materialized <- execute(master, wave = 2)

  assignment <- attr(master, "metadata")$panel_assignment
  wave <- attr(materialized, "metadata")$wave
  active <- wave$active_panels

  expect_identical(wave$wave, 2L)
  expect_identical(active, c(2L, 3L))

  keys <- make_group_key(as.data.frame(master), assignment$key_vars)
  for (p in seq_along(assignment$pools)) {
    pool <- assignment$pools[[p]]
    recorded <- wave$pools[[p]]

    expect_identical(recorded$class, pool$class)
    expect_identical(recorded$blocks, pool$blocks)
    expect_identical(
      recorded$take,
      as.integer(rowSums(pool$quotas[, active, drop = FALSE]))
    )
    expect_equal(recorded$probability, recorded$take / recorded$blocks)

    # The take is what the wave actually selected from that block.
    block_of_unit <- rep(seq_along(pool$blocks), pool$blocks)
    at <- match(keys, pool$keys)
    rows <- which(!is.na(at))
    selected <- master$.panel[rows] %in% active
    expect_identical(
      as.integer(tabulate(block_of_unit[at[rows]][selected],
                          length(pool$blocks))),
      recorded$take
    )
  }
})

test_that("permanent certainty units are activated at every wave", {
  frame <- data.frame(
    id = 1:60,
    mos = c(rep(10, 54), 900, 950, 1000, 1050, 1100, 1150)
  )

  master <- sampling_design() |>
    draw(n = 30, method = "pps_systematic", mos = mos, certainty_size = 800) |>
    execute(frame, seed = 3, panels = rotation_2_2())

  certain <- master$.sample_id[master$.certainty_1]
  expect_length(certain, 6L)

  for (t in 1:4) {
    materialized <- execute(master, wave = t)
    expect_true(all(certain %in% materialized$.sample_id))

    # Probability one, so their weights are untouched.
    kept <- match(certain, materialized$.sample_id)
    expect_identical(
      materialized$.weight[kept],
      master$.weight[match(certain, master$.sample_id)]
    )
  }

  wave <- attr(execute(master, wave = 1), "metadata")$wave
  classes <- vapply(wave$pools, function(p) p$class, character(1))
  expect_identical(classes, c("rotating", "certainty"))
  expect_true(all(wave$pools[[2]]$probability == 1))
})

test_that("a materialized wave is authoritative, not a filtered sample", {
  master <- master_2_2()
  materialized <- execute(master, wave = 1)

  expect_true(sample_realization_status(materialized)$ok)
  expect_identical(get_design(materialized), get_design(master))
  expect_identical(
    get_stages_executed(materialized),
    get_stages_executed(master)
  )

  # A hand-filtered subset is a modified sample and stays one.
  filtered <- master[master$.panel %in% 1:2, ]
  expect_false(sample_realization_status(filtered)$ok)
})

## Guards

test_that("the wave route refuses every other execution input", {
  master <- master_2_2()

  expect_error(
    execute(master, wave_frame(), wave = 1),
    class = "samplyr_error_wave_extra_arguments"
  )
  expect_error(
    execute(master, wave = 1, seed = 3),
    class = "samplyr_error_wave_extra_arguments"
  )
  expect_error(
    execute(master, wave = 1, panels = 2),
    class = "samplyr_error_wave_extra_arguments"
  )
  expect_error(
    execute(master, wave = 1, reps = 2),
    class = "samplyr_error_wave_extra_arguments"
  )
  expect_error(
    execute(master, wave = 1, stages = 1),
    class = "samplyr_error_wave_extra_arguments"
  )
})

test_that("only a scheduled, complete, unmaterialized master carries a wave", {
  expect_error(
    execute(sampling_design() |> draw(n = 10), wave = 1),
    class = "samplyr_error_wave_not_a_sample"
  )

  counted <- sampling_design() |>
    draw(n = 40) |>
    execute(wave_frame(), seed = 1, panels = 4)
  expect_error(
    execute(counted, wave = 1),
    class = "samplyr_error_wave_no_schedule"
  )

  unpanelled <- sampling_design() |>
    draw(n = 40) |>
    execute(wave_frame(), seed = 1)
  expect_error(
    execute(unpanelled, wave = 1),
    class = "samplyr_error_wave_no_schedule"
  )

  master <- master_2_2()
  expect_error(
    execute(execute(master, wave = 1), wave = 2),
    class = "samplyr_error_wave_already_materialized"
  )

  design <- sampling_design() |>
    add_stage("Clusters") |>
      cluster_by(region) |>
      draw(n = 2) |>
    add_stage("Units") |>
      draw(n = 5)
  partial <- execute(
    design, wave_frame(), stages = 1, seed = 1, panels = rotation_2_2()
  )
  expect_error(
    execute(partial, wave = 1),
    class = "samplyr_error_wave_incomplete_master"
  )
})

test_that("wave must name a declared wave", {
  master <- master_2_2()

  for (bad in list(5, 0, "1", c(1, 2), 1.5, NA_integer_)) {
    expect_error(
      execute(master, wave = bad),
      class = "samplyr_error_wave_undeclared"
    )
  }
})

test_that("a modified master cannot be materialized", {
  master <- master_2_2()
  master$.weight <- master$.weight * 2

  expect_error(
    execute(master, wave = 1),
    class = "samplyr_error_modified_sample"
  )
})

## Receipt, replay and export

test_that("the schedule survives the design file and replay reproduces it", {
  frame <- wave_frame()
  master <- master_2_2()

  path <- withr::local_tempfile(fileext = ".json")
  write_design(master, path, frame = frame)
  restored <- read_design(path)
  receipt <- attr(restored, "execution")

  expect_equal(receipt$panels, 4L)
  expect_equal(receipt$panel_assignment$r_min, 2L)
  expect_equal(
    decode_panel_argument(receipt),
    normalize_panel_input(rotation_2_2())$schedule[c("wave", "panel", "active")]
  )

  # The block size follows from the schedule, so a replay given only the
  # panel count would produce different labels.
  replayed <- replay_design(restored, frame)
  expect_identical(replayed$.panel, master$.panel)
  expect_identical(replayed$id, master$id)
})

test_that("a materialized wave records its wave and refuses replay", {
  frame <- wave_frame()
  materialized <- execute(master_2_2(), wave = 3)

  path <- withr::local_tempfile(fileext = ".json")
  suppressWarnings(write_design(materialized, path, frame = frame))
  receipt <- attr(read_design(path), "execution")

  expect_true(isTRUE(receipt$chained))
  expect_equal(receipt$wave$wave, 3L)
  expect_equal(as.integer(unlist(receipt$wave$active_panels)), c(3L, 4L))
  expect_length(receipt$wave$pools, 2L)

  expect_error(
    replay_design(read_design(path), frame),
    class = "samplyr_error_receipt_chained"
  )
})

test_that("survey export refuses a wave until activation is carried", {
  master <- master_2_2()
  materialized <- execute(master, wave = 1)

  expect_error(
    as_svydesign(materialized),
    class = "samplyr_error_wave_export_unsupported"
  )
  expect_error(
    as_svrepdesign(materialized),
    class = "samplyr_error_wave_export_unsupported"
  )
  expect_error(
    joint_expectation(materialized),
    class = "samplyr_error_wave_export_unsupported"
  )

  # The master itself is unaffected.
  expect_s3_class(as_svydesign(master), "survey.design")
})
