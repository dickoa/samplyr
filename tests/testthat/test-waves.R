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
    class = "samplyr_error_schedule_columns"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 1), wave = c(1, 1))),
    class = "samplyr_error_schedule_duplicates"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 3), wave = c(1, 1))),
    class = "samplyr_error_schedule_gap"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 2), wave = c(1, 3))),
    class = "samplyr_error_schedule_gap"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 2), wave = c(1, 1))) |>
      suppressWarnings(),
    NA
  )
  expect_error(
    normalize_panel_input(data.frame(panel = 1:2, wave = c(1, 1.5))),
    class = "samplyr_error_schedule_values"
  )
  expect_error(
    normalize_panel_input(
      data.frame(panel = 1:2, wave = c(1, 1), active = c("y", "n"))
    ),
    class = "samplyr_error_schedule_active"
  )
  expect_error(
    normalize_panel_input(data.frame(panel = c(1, 1), wave = c(1, 2))),
    class = "samplyr_error_schedule_size"
  )
  expect_error(
    normalize_panel_input(
      data.frame(
        panel = rep(1:2, 2),
        wave = rep(1:2, each = 2),
        active = c(TRUE, TRUE, FALSE, FALSE)
      )
    ),
    class = "samplyr_error_schedule_idle_wave"
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
  # wave_frame() has two regions, so a first stage of two clusters cannot
  # rotate over four panels. The policy is incidental to what this test
  # asserts, but it has to be declared for the master to exist at all.
  expect_warning(
    partial <- execute(
      design, wave_frame(), stages = 1, seed = 1, panels = rotation_2_2(),
      small_pool = "permanent"
    ),
    class = "samplyr_warning_panel_small_pool"
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
    decode_panel_argument(receipt, receipt$panel_assignment),
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

## Survey export of a materialized wave

test_that("a wave retains the master as its first phase", {
  master <- master_2_2()
  materialized <- execute(master, wave = 1)
  prev <- attr(materialized, "metadata")$prev_phase

  expect_identical(prev$transition, "panel_activation")
  expect_identical(as.data.frame(prev$sample), as.data.frame(master))
  expect_identical(prev$stages, get_stages_executed(master))

  # The master's metadata is reachable through the phase link, so the
  # separate copy it used to be given is gone.
  expect_null(attr(materialized, "metadata")$materialized_from)
  expect_equal(
    attr(prev$sample, "metadata")$n_selected,
    nrow(master)
  )
})

test_that("a wave exports through twophase() with its exact weights", {
  skip_if_not_installed("survey")
  master <- master_2_2()
  materialized <- execute(master, wave = 1)

  svy <- as_svydesign(materialized)
  expect_s3_class(svy, "twophase2")

  # The exported design's weights are the activation weights, not the
  # master's: survey derives them from the two phases independently.
  expect_equal(
    unname(stats::weights(svy)),
    unname(as.data.frame(materialized)$.weight)
  )
  expect_equal(nrow(svy$phase1$full$variables), nrow(master))
  expect_equal(sum(svy$subset), nrow(materialized))

  # The point estimate is the design-weighted total either way; it is the
  # variance the second phase supplies.
  total <- survey::svytotal(~value, svy)
  expect_equal(
    unname(coef(total)),
    sum(as.data.frame(materialized)$.weight * as.data.frame(materialized)$value)
  )
  expect_gt(unname(survey::SE(total)), 0)
})

test_that("the activation phase carries the frozen blocks, not a filter", {
  skip_if_not_installed("survey")
  master <- master_2_2()
  materialized <- execute(master, wave = 1)
  svy <- as_svydesign(materialized)

  record <- attr(master, "metadata")$panel_assignment
  blocks <- unlist(lapply(record$pools, function(p) p$blocks))

  # One phase-2 stratum per frozen block, and its population count is the
  # block's size in assignment units.
  strata2 <- svy$phase2$strata[, 1]
  expect_equal(length(unique(strata2)), length(blocks))
  expect_setequal(unique(svy$phase2$fpc$popsize[, 1]), unique(blocks))

  # A hand-written filter of the same rows is a modified sample and is
  # refused, so the two routes cannot be confused.
  filtered <- dplyr::filter(master, .panel %in% c(1L, 2L))
  expect_error(
    as_svydesign(filtered),
    class = "samplyr_error_modified_sample"
  )
})

test_that("wave export supports the method choice and never falls back", {
  skip_if_not_installed("survey")
  materialized <- execute(master_2_2(), wave = 1)

  full <- as_svydesign(materialized)
  simple <- as_svydesign(materialized, method = "simple")
  approx <- as_svydesign(materialized, method = "approx")

  expect_s3_class(full, "twophase2")
  expect_s3_class(simple, "twophase")
  expect_s3_class(approx, "twophase")

  # Same weights on every method; they differ only in the variance.
  expect_equal(unname(stats::weights(simple)), unname(stats::weights(full)))
  expect_equal(unname(stats::weights(approx)), unname(stats::weights(full)))
})

test_that("wave export covers the master shapes twophase() can represent", {
  skip_if_not_installed("survey")
  frame <- wave_frame()

  shapes <- list(
    unstratified = sampling_design() |> draw(n = 60),
    stratified = sampling_design() |> stratify_by(region) |> draw(n = 60),
    clustered = sampling_design() |> cluster_by(score) |> draw(n = 12),
    with_replacement = sampling_design() |> draw(n = 60, method = "srswr"),
    two_stage = sampling_design() |>
      add_stage() |>
      cluster_by(score) |>
      draw(n = 12) |>
      add_stage() |>
      draw(n = 2)
  )

  for (nm in names(shapes)) {
    master <- execute(shapes[[nm]], frame, seed = 11, panels = rotation_2_2())
    materialized <- execute(master, wave = 2)
    svy <- as_svydesign(materialized)
    expect_s3_class(svy, "twophase2")
    expect_equal(
      unname(stats::weights(svy)),
      unname(as.data.frame(materialized)$.weight),
      info = nm
    )
    # The master is the first phase and every one of its stages is
    # represented there. Weights alone do not detect a dropped stage,
    # because a stage can carry variance without carrying probability.
    expect_equal(
      ncol(svy$phase1$full$cluster),
      length(get_stages_executed(master)),
      info = nm
    )
  }
})

test_that("a wave of a multistage master matches a hand-written twophase()", {
  skip_if_not_installed("survey")
  frame <- wave_frame()

  master <- sampling_design() |>
    add_stage() |>
    cluster_by(score) |>
    draw(n = 20) |>
    add_stage() |>
    draw(n = 3) |>
    execute(frame, seed = 11, panels = rotation_2_2())
  materialized <- execute(master, wave = 1)

  total <- survey::svytotal(~value, as_svydesign(materialized))

  # The same design written out by hand: both master stages at phase 1, the
  # frozen blocks as phase-2 strata. A dropped stage moves the standard error
  # while leaving the total exact, so both are compared.
  record <- attr(master, "metadata")$panel_assignment
  df <- as.data.frame(master)
  keys <- make_group_key(df, record$key_vars)
  df$.block <- NA_character_
  df$.block_n <- NA_real_
  for (p in seq_along(record$pools)) {
    pool <- record$pools[[p]]
    at <- match(keys, pool$keys)
    rows <- which(!is.na(at))
    b <- rep(seq_along(pool$blocks), pool$blocks)[at[rows]]
    df$.block[rows] <- paste(p, b, sep = ".")
    df$.block_n[rows] <- pool$blocks[b]
  }
  df$.unit <- match(keys, unique(keys))
  df$.elem <- seq_len(nrow(df))
  df$.active <- df$.sample_id %in% materialized$.sample_id

  reference <- survey::twophase(
    id = list(~ score + .elem, ~.unit),
    strata = list(NULL, ~.block),
    fpc = list(~ .fpc_1 + .fpc_2, ~.block_n),
    subset = ~.active,
    data = df,
    method = "full"
  )
  reference_total <- survey::svytotal(~value, reference)

  expect_equal(unname(coef(total)), unname(coef(reference_total)))
  expect_equal(unname(survey::SE(total)), unname(survey::SE(reference_total)))
})

test_that("wave export refuses a master with unequal inclusion probabilities", {
  skip_if_not_installed("survey")
  frame <- wave_frame()
  frame$mos <- rep(c(rep(1, 190), rep(60, 10)), 2)

  pps <- sampling_design() |>
    draw(n = 40, method = "pps_sps", mos = mos) |>
    execute(frame, seed = 5, panels = rotation_2_2())
  materialized <- execute(pps, wave = 1)

  expect_error(
    as_svydesign(materialized),
    class = "samplyr_error_wave_phase1_pps"
  )
  # The master's own export is exact and is what the message points at.
  expect_s3_class(as_svydesign(pps), "survey.design")

  # A method with no linearization treatment at all refuses on that ground
  # instead, at either phase.
  spatial <- sampling_design() |>
    draw(n = 40, method = "lpm2", spread = c(score, value)) |>
    execute(frame, seed = 5, panels = rotation_2_2())
  expect_error(
    as_svydesign(execute(spatial, wave = 1)),
    class = "samplyr_error_custom_random_wor_export"
  )
})

test_that("a wave inherits the restrictions of any second phase", {
  skip_if_not_installed("survey")
  materialized <- execute(master_2_2(), wave = 1)

  # Replicate weights are not built for two-phase samples, and the message
  # names the export that does work.
  expect_error(
    as_svrepdesign(materialized),
    class = "samplyr_error_svrep_twophase_unsupported"
  )
  # Activation joints are computed, but from the master: a materialized wave
  # is not the query surface for them.
  expect_error(
    joint_expectation(materialized),
    class = "samplyr_error_wave_export_unsupported"
  )
  # Variance components assume nested stages, and a phase is not a stage.
  expect_error(
    varcomp(materialized),
    class = "samplyr_error_varcomp_two_phase"
  )

  # A wave of a master that is itself a second phase would be a third.
  frame <- wave_frame()
  phase1 <- sampling_design() |>
    cluster_by(region) |>
    draw(n = 2) |>
    execute(frame, seed = 3)
  phase2 <- sampling_design() |>
    draw(n = 60) |>
    execute(phase1, seed = 4, panels = rotation_2_2())
  expect_error(
    as_svydesign(execute(phase2, wave = 1)),
    class = "samplyr_error_survey_multiphase_unsupported"
  )
})

test_that("a wave without its master refuses rather than exporting", {
  skip_if_not_installed("survey")
  materialized <- execute(master_2_2(), wave = 2)

  metadata <- attr(materialized, "metadata")
  metadata$prev_phase <- NULL
  attr(materialized, "metadata") <- metadata

  expect_error(
    as_svydesign(materialized),
    class = "samplyr_error_wave_no_master"
  )
})

test_that("a wave whose rows are not the master's is caught", {
  skip_if_not_installed("survey")
  master <- master_2_2()
  materialized <- execute(master, wave = 1)

  # A master from a different execution has the same shape and the same
  # assignment record fields, so only the realized take detects the swap.
  metadata <- attr(materialized, "metadata")
  metadata$prev_phase$sample <- master_2_2(seed = 99)
  attr(materialized, "metadata") <- metadata

  expect_error(
    as_svydesign(materialized),
    class = "samplyr_error_wave_master_mismatch"
  )
})

test_that("srvyr export of a wave goes through the two-phase route", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  materialized <- execute(master_2_2(), wave = 1)

  tbl <- srvyr::as_survey(materialized)
  expect_s3_class(tbl, "tbl_svy")
  expect_s3_class(tbl, "twophase2")
  expect_equal(
    unname(stats::weights(tbl)),
    unname(as.data.frame(materialized)$.weight)
  )
})

test_that("analysis columns added to a wave reach the exported design", {
  skip_if_not_installed("survey")
  materialized <- execute(master_2_2(), wave = 1)
  materialized$response <- seq_len(nrow(materialized)) / nrow(materialized)

  svy <- as_svydesign(materialized)
  expect_true("response" %in% names(svy$phase1$full$variables))
  expect_equal(
    unname(coef(survey::svytotal(~response, svy))),
    sum(as.data.frame(materialized)$.weight * materialized$response)
  )
})

test_that("a cohort drawn whole exports as the single-phase design it is", {
  skip_if_not_installed("survey")
  frame <- wave_frame()

  cohorts <- list(
    a = sampling_design() |>
      stratify_by(region) |>
      draw(n = 40) |>
      execute(frame, seed = 21, panels = rotation_2_2()),
    b = sampling_design() |>
      stratify_by(region) |>
      draw(n = 20) |>
      execute(frame, seed = 22)
  )
  program <- rotation_program(
    cohorts,
    entry_wave = c(a = 1L, b = 2L),
    schedule = data.frame(
      cohort = c("a", "a", "a", "a", "b"),
      panel = c(1L, 2L, 1L, 2L, 1L),
      wave = c(1L, 1L, 2L, 2L, 2L),
      active = TRUE
    )
  )
  live <- execute(program, wave = 2)

  # The partitioned cohort activates a subsample and is two-phase; the whole
  # cohort is not a subsample at all and stays single-phase.
  expect_s3_class(as_svydesign(live$a), "twophase2")
  expect_s3_class(as_svydesign(live$b), "survey.design")
  expect_equal(
    unname(stats::weights(as_svydesign(live$b))),
    unname(as.data.frame(live$b)$.weight)
  )
})
