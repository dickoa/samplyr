## Small-pool positivity: refusal by default, promotion on request

# A pool of `m` units leaves `k - m` panels empty, so a wave activating `r`
# panels selects nothing from it exactly when `m <= k - r`. Those units have
# conditional inclusion probability zero in that wave, so the estimator is
# biased rather than imprecise. The default refuses; `small_pool =
# "permanent"` activates the pool at every wave instead.

small_pool_frame <- function() {
  data.frame(
    reg = c(rep("A", 400), rep("B", 400), rep("C", 20)),
    psu = c(
      rep(sprintf("A%02d", 1:20), each = 20),
      rep(sprintf("B%02d", 1:20), each = 20),
      rep("C01", 20)
    ),
    y = rep(c(1, 2, 50), times = c(400, 400, 20))
  )
}

# 4 panels, 2 active per wave, so k - r_min = 2 and a pool of 1 or 2 fails.
small_pool_schedule <- function() {
  data.frame(
    panel = rep(1:4, times = 2),
    wave = rep(1:2, each = 4),
    active = c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )
}

# One PSU per stratum: every pool holds a single assignment unit.
small_pool_design <- function() {
  sampling_design() |>
    add_stage("psu") |>
    stratify_by(reg) |>
    cluster_by(psu) |>
    draw(n = 1) |>
    add_stage("hh") |>
    draw(n = 5)
}

## The default refuses, and refuses before any panel is drawn

test_that("a schedule that would strand a pool is refused", {
  expect_error(
    execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule()
    ),
    class = "samplyr_error_panel_small_pool"
  )
})

test_that("the refusal names every affected pool and the remedies", {
  err <- tryCatch(
    execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule()
    ),
    samplyr_error_panel_small_pool = function(cnd) cnd
  )
  message <- cli::ansi_strip(paste(
    conditionMessage(err),
    paste(err$body, collapse = " ")
  ))
  # All three pools, not just the first one found.
  expect_match(message, "reg = A")
  expect_match(message, "reg = B")
  expect_match(message, "reg = C")
  expect_match(message, "small_pool")
})

test_that("the refusal is anchored on execute(), not an internal helper", {
  err <- tryCatch(
    execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule()
    ),
    samplyr_error_panel_small_pool = function(cnd) cnd
  )
  expect_identical(deparse(conditionCall(err)[[1]]), "execute")
})

test_that("refusal does not depend on the assignment draw", {
  # Whatever the seed, the same schedule and the same pool sizes refuse:
  # the check reads realized pool sizes and runs before any panel label is
  # drawn, so it cannot be retried into success.
  for (seed in c(1L, 2L, 3L, 99L)) {
    expect_error(
      execute(
        small_pool_design(), small_pool_frame(),
        seed = seed, panels = small_pool_schedule()
      ),
      class = "samplyr_error_panel_small_pool"
    )
  }
})

test_that("a pool above the threshold is not refused", {
  # Three PSUs per stratum against k - r_min = 2 is the first safe size, and
  # it is below the block size B = 4, so positivity and variance support are
  # separate thresholds.
  design <- sampling_design() |>
    add_stage("psu") |>
    stratify_by(reg) |>
    cluster_by(psu) |>
    draw(n = 3) |>
    add_stage("hh") |>
    draw(n = 5)
  frame <- small_pool_frame()
  frame <- frame[frame$reg != "C", , drop = FALSE]

  master <- execute(design, frame, seed = 5, panels = small_pool_schedule())
  record <- attr(master, "metadata")$panel_assignment
  expect_true(all(vapply(
    record$pools, function(pool) pool$size >= 3L, logical(1)
  )))
  expect_identical(record$small_pool_policy, "error")
})

## Promotion, when it is asked for

test_that("small_pool = 'permanent' promotes the pool and says so", {
  expect_warning(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    ),
    class = "samplyr_warning_panel_small_pool"
  )
  record <- attr(master, "metadata")$panel_assignment
  expect_identical(record$small_pool_policy, "permanent")
  expect_identical(record$version, 3L)

  # Selection status and activation status are recorded separately: these
  # pools are permanent without being selection-certain.
  expect_identical(
    vapply(record$pools, function(pool) pool$class, character(1)),
    rep("rotating", 3L)
  )
  expect_identical(
    vapply(record$pools, function(pool) pool$activation, character(1)),
    rep("permanent", 3L)
  )
  expect_identical(
    vapply(record$pools, function(pool) pool$permanent_reason, character(1)),
    rep("small_pool", 3L)
  )
})

test_that("a promoted pool appears in every wave with its weight intact", {
  suppressWarnings(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    )
  )
  total <- sum(master$.weight * master$y)

  for (t in 1:2) {
    wave <- execute(master, wave = t)
    # Activation probability one, so the wave reproduces the master exactly
    # rather than estimating it.
    expect_setequal(unique(wave$reg), c("A", "B", "C"))
    expect_equal(sum(wave$.weight * wave$y), total)
  }
})

test_that("promotion does not make a pool selection-certain", {
  suppressWarnings(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    )
  )
  joint <- joint_expectation(master, waves = c(1L, 2L))
  expect_true(all(joint$class == "rotating"))
  expect_true(all(joint$activation == "permanent"))
  expect_true(all(joint$prob_1 == 1))
  expect_true(all(joint$prob_2 == 1))
})

## Where the policy does and does not apply

test_that("small_pool needs a schedule to be meaningful", {
  expect_error(
    execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = 4, small_pool = "permanent"
    ),
    class = "samplyr_error_small_pool_not_applicable"
  )
  expect_error(
    execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, small_pool = "permanent"
    ),
    class = "samplyr_error_small_pool_not_applicable"
  )
})

test_that("small_pool rejects a value it does not define", {
  for (value in list("nope", 1L, c("error", "permanent"), NA_character_)) {
    expect_error(
      execute(
        small_pool_design(), small_pool_frame(),
        seed = 5, panels = small_pool_schedule(), small_pool = value
      ),
      class = "samplyr_error_small_pool_value"
    )
  }
})

test_that("a wave cannot revisit the policy its master froze", {
  suppressWarnings(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    )
  )
  expect_error(
    execute(master, wave = 1, small_pool = "error"),
    class = "samplyr_error_wave_extra_arguments"
  )
})

test_that("a misspelled small_pool is named rather than read as a frame", {
  expect_error(
    execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pools = "permanent"
    ),
    class = "samplyr_error_unknown_argument"
  )
})

## The policy survives serialization and replay

test_that("replay reproduces a promoted master rather than refusing it", {
  suppressWarnings(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    )
  )
  record <- attr(master, "metadata")$panel_assignment
  expect_identical(record$small_pool_policy, "permanent")

  path <- withr::local_tempfile(fileext = ".json")
  write_design(master, path, frame = small_pool_frame())

  suppressWarnings(
    replayed <- replay_design(read_design(path), small_pool_frame())
  )
  expect_identical(replayed$.panel, master$.panel)

  replayed_record <- attr(replayed, "metadata")$panel_assignment
  expect_identical(replayed_record$small_pool_policy, "permanent")
  expect_identical(
    vapply(replayed_record$pools, function(p) p$activation, character(1)),
    vapply(record$pools, function(p) p$activation, character(1))
  )
})

## A record written before the check existed

test_that("a version-1 record with a stranded pool refuses at materialization", {
  suppressWarnings(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    )
  )

  # Rewrite the record as version 1 would have written it: no activation
  # field, no policy, and the promoted pools left rotating. This is the state
  # an older samplyr could produce and this build must not materialize.
  metadata <- attr(master, "metadata")
  metadata$panel_assignment$version <- 1L
  metadata$panel_assignment$small_pool_policy <- NULL
  metadata$panel_assignment$pools <- lapply(
    metadata$panel_assignment$pools,
    function(pool) {
      pool$activation <- NULL
      pool$permanent_reason <- NULL
      pool
    }
  )
  attr(master, "metadata") <- metadata

  expect_error(
    execute(master, wave = 1),
    class = "samplyr_error_panel_small_pool"
  )
})

test_that("a version-1 record with adequate pools still materializes", {
  design <- sampling_design() |>
    add_stage("psu") |>
    stratify_by(reg) |>
    cluster_by(psu) |>
    draw(n = 4) |>
    add_stage("hh") |>
    draw(n = 5)
  frame <- small_pool_frame()
  frame <- frame[frame$reg != "C", , drop = FALSE]

  master <- execute(design, frame, seed = 5, panels = small_pool_schedule())

  metadata <- attr(master, "metadata")
  metadata$panel_assignment$version <- 1L
  metadata$panel_assignment$small_pool_policy <- NULL
  metadata$panel_assignment$pools <- lapply(
    metadata$panel_assignment$pools,
    function(pool) {
      pool$activation <- NULL
      pool$permanent_reason <- NULL
      pool
    }
  )
  attr(master, "metadata") <- metadata

  wave <- execute(master, wave = 1)
  expect_gt(nrow(wave), 0L)
  expect_setequal(unique(wave$reg), c("A", "B"))
})

## Export of an activation that retains everything

test_that("an all-permanent wave exports as the single-phase design it is", {
  suppressWarnings(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    )
  )
  wave <- execute(master, wave = 1)

  # Every activation probability is one, so the conditional phase-2 variance
  # is exactly zero and the design is the master's. A two-phase object would
  # state a different design, and survey aborts on this one outright:
  # an identity phase 2 over singleton phase-1 strata is a subscript error
  # inside twophase().
  design <- as_svydesign(wave)
  expect_s3_class(design, "survey.design2")
  expect_false(inherits(design, "twophase2"))

  withr::local_options(survey.lonely.psu = "adjust")
  expect_equal(
    coef(survey::svytotal(~y, design)),
    coef(survey::svytotal(~y, as_svydesign(master)))
  )
})

test_that("a wave activating every panel also reduces to single phase", {
  # The same identity reached by a different route: no pool is permanent, but
  # the wave takes every panel, so nothing is subsampled.
  frame <- small_pool_frame()
  frame <- frame[frame$reg != "C", , drop = FALSE]
  schedule <- data.frame(
    panel = rep(1:4, times = 2),
    wave = rep(1:2, each = 4),
    active = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
  )
  design <- sampling_design() |>
    add_stage("psu") |>
    stratify_by(reg) |>
    cluster_by(psu) |>
    draw(n = 8) |>
    add_stage("hh") |>
    draw(n = 5)
  master <- execute(design, frame, seed = 5, panels = schedule)

  record <- attr(master, "metadata")$panel_assignment
  expect_true(all(vapply(
    record$pools, function(pool) pool$activation == "rotating", logical(1)
  )))

  full <- as_svydesign(execute(master, wave = 1))
  expect_s3_class(full, "survey.design2")
  expect_false(inherits(full, "twophase2"))

  # Wave 2 subsamples, so it keeps the two-phase representation.
  partial <- as_svydesign(execute(master, wave = 2))
  expect_s3_class(partial, "twophase2")
})

test_that("a mixed wave keeps its second phase", {
  frame <- rbind(small_pool_frame(), data.frame(
    reg = rep("D", 400),
    psu = rep(sprintf("D%02d", 1:20), each = 20),
    y = 3
  ))
  design <- sampling_design() |>
    add_stage("psu") |>
    stratify_by(reg) |>
    cluster_by(psu) |>
    draw(n = c(A = 1, B = 1, C = 1, D = 8)) |>
    add_stage("hh") |>
    draw(n = 5)
  suppressWarnings(
    master <- execute(
      design, frame, seed = 5, panels = small_pool_schedule(),
      small_pool = "permanent"
    )
  )
  activations <- vapply(
    attr(master, "metadata")$panel_assignment$pools,
    function(pool) pool$activation,
    character(1)
  )
  expect_setequal(activations, c("permanent", "rotating"))
  expect_s3_class(as_svydesign(execute(master, wave = 1)), "twophase2")
})

## Diagnostics name the public call

test_that("the promotion warning is anchored on execute()", {
  warning <- NULL
  withCallingHandlers(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    ),
    samplyr_warning_panel_small_pool = function(cnd) {
      warning <<- cnd
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(deparse(conditionCall(warning)[[1]]), "execute")
})

test_that("the version-1 backstop is anchored on execute()", {
  suppressWarnings(
    master <- execute(
      small_pool_design(), small_pool_frame(),
      seed = 5, panels = small_pool_schedule(), small_pool = "permanent"
    )
  )
  metadata <- attr(master, "metadata")
  metadata$panel_assignment$version <- 1L
  metadata$panel_assignment$small_pool_policy <- NULL
  metadata$panel_assignment$pools <- lapply(
    metadata$panel_assignment$pools,
    function(pool) {
      pool$activation <- NULL
      pool$permanent_reason <- NULL
      pool
    }
  )
  attr(master, "metadata") <- metadata

  err <- tryCatch(
    execute(master, wave = 1),
    samplyr_error_panel_small_pool = function(cnd) cnd
  )
  expect_identical(deparse(conditionCall(err)[[1]]), "execute")
  # The stored body keeps the source line breaks; cli collapses them only
  # when it formats, so the assertion normalizes whitespace first.
  expect_match(
    gsub("\\s+", " ", cli::ansi_strip(paste(err$body, collapse = " "))),
    "in this wave"
  )
})
