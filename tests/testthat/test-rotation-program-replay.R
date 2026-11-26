## Replay of a rotation program's components

# Six programs whose components the program activates on its own schedule.
# Each case names the occasion and, per component, the expected active panels
# and visits worked out by hand from the program's schedule below, so the
# oracle shares no code with the activation (rung 1, closed form).
rpr_frame <- function() data.frame(id = 1:40)
rpr_entrants <- function() data.frame(id = 101:130)

rpr_draw <- function(frame, n, seed, panels = NULL) {
  design <- sampling_design() |> draw(n = n)
  if (is_null(panels)) {
    return(suppressMessages(execute(design, frame, seed = seed)))
  }
  suppressMessages(execute(design, frame, seed = seed, panels = panels))
}

rpr_own <- function() {
  own <- expand.grid(panel = 1:2, period = 1:3)
  own$active <- c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE)
  own
}

# Panel 1 alone at period 2, panel 2 alone at period 3.
rpr_effective <- function() {
  effective <- rpr_own()
  effective$active <- c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE)
  effective
}

rpr_cases <- function() {
  frame <- rpr_frame()
  entrants <- rpr_entrants()
  own_source <- rpr_draw(frame, 20, 2, rpr_own())

  own_e <- expand.grid(panel = 1:2, period = 1:2)
  own_e$active <- c(TRUE, FALSE, TRUE, TRUE)
  own_f <- expand.grid(panel = 1:2, subround = 1:2, period = 1:2)
  own_f$active <- c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  effective_f <- own_f
  effective_f$active <- c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)

  list(
    own_schedule_overridden = list(
      program = rotation_program(list(a = own_source), c(a = 1),
                                 rpr_effective()),
      frames = list(a = frame), period = 2L, subround = NULL,
      expected = list(a = list(panels = 1L, visits = 2L))
    ),
    count_of_panels = list(
      program = rotation_program(list(a = rpr_draw(frame, 20, 2, 2L)),
                                 c(a = 1), rpr_effective()),
      frames = list(a = frame), period = 2L, subround = NULL,
      expected = list(a = list(panels = 1L, visits = 2L))
    ),
    program_equals_own = list(
      program = rotation_program(list(a = own_source), c(a = 1), rpr_own()),
      frames = list(a = frame), period = 2L, subround = NULL,
      expected = list(a = list(panels = 1:2, visits = c(1L, 2L)))
    ),
    whole_refresher = list(
      program = rotation_program(
        list(a = own_source, b = rpr_draw(entrants, 10, 3)),
        c(a = 1, b = 2),
        rbind(
          transform(rpr_effective(), selection = "a"),
          data.frame(panel = 1L, period = 1:3, active = c(FALSE, TRUE, TRUE),
                     selection = "b")
        )
      ),
      frames = list(a = frame, b = entrants), period = 3L, subround = NULL,
      expected = list(a = list(panels = 2L, visits = 2L),
                      b = list(panels = NULL, visits = NULL))
    ),
    refresher_own_schedule = list(
      program = rotation_program(
        list(a = own_source, b = rpr_draw(entrants, 10, 4, own_e)),
        c(a = 1, b = 2),
        rbind(
          transform(rpr_effective(), selection = "a"),
          data.frame(panel = rep(1:2, 3), period = rep(1:3, each = 2),
                     active = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
                     selection = "b")
        )
      ),
      frames = list(a = frame, b = entrants), period = 3L, subround = NULL,
      expected = list(a = list(panels = 2L, visits = 2L),
                      b = list(panels = 1:2, visits = c(1L, 2L)))
    ),
    subrounds_overridden = list(
      program = rotation_program(list(a = rpr_draw(frame, 20, 5, own_f)),
                                 c(a = 1), effective_f),
      frames = list(a = frame), period = 2L, subround = 2L,
      expected = list(a = list(panels = 2L, visits = 3L))
    )
  )
}

rpr_occasion <- function(case) {
  if (is_null(case$subround)) {
    return(suppressMessages(execute(case$program, period = case$period)))
  }
  suppressMessages(execute(case$program, period = case$period,
                           subround = case$subround))
}

rpr_json_replay <- function(x, frame) {
  suppressMessages(replay_design(read_design(design_json(x, frame = frame)),
                                 frame))
}

# What the component must hold, from the selection and the hand-worked
# expectation alone. A whole selection is activated whole, unweighted.
rpr_expect_component <- function(x, source, expected, label) {
  data <- as.data.frame(x)
  src <- as.data.frame(source)
  if (is_null(expected$panels)) {
    expect_identical(sort(data$id), sort(src$id), label = label)
    expect_equal(data$.weight, src$.weight[match(data$id, src$id)],
                 label = label)
    expect_false(".visit" %in% names(data), label = label)
    return(invisible())
  }
  # The premise of the weight oracle: equal panels, so r of k active panels
  # take each unit with chance r / k.
  sizes <- table(src$.panel)
  expect_length(unique(as.vector(sizes)), 1L)
  k <- length(sizes)
  r <- length(expected$panels)

  expect_identical(sort(data$id), sort(src$id[src$.panel %in% expected$panels]),
                   label = label)
  expect_equal(data$.weight, src$.weight[match(data$id, src$id)] * k / r,
               label = label)
  expect_identical(
    data$.visit,
    expected$visits[match(data$.panel, expected$panels)],
    label = label
  )
  expect_identical(
    sort(as.integer(attr(x, "metadata")$occasion$active_panels)),
    sort(as.integer(expected$panels)),
    label = label
  )
}

test_that("a program's components replay live and from their receipts", {
  cases <- rpr_cases()
  checked <- character()
  for (case_name in names(cases)) {
    case <- cases[[case_name]]
    occasion <- rpr_occasion(case)
    expect_setequal(names(occasion), names(case$expected))
    for (nm in names(occasion)) {
      label <- paste(case_name, nm)
      part <- occasion[[nm]]
      source <- case$program$selections[[nm]]
      frame <- case$frames[[nm]]
      rpr_expect_component(part, source, case$expected[[nm]], label)

      live <- replay_design(part, frame)
      from_json <- rpr_json_replay(part, frame)
      for (replayed in list(live, from_json)) {
        rpr_expect_component(replayed, source, case$expected[[nm]], label)
        expect_identical(as.data.frame(replayed)$id, as.data.frame(part)$id,
                         label = label)
        recorded <- attr(part, "metadata")$occasion
        again <- attr(replayed, "metadata")$occasion
        for (field in c("selection", "entry_period", "period", "subround")) {
          expect_identical(again[[field]], recorded[[field]],
                           label = paste(label, field))
        }
      }
      checked <- c(checked, label)
    }
  }
  expect_setequal(checked, c(
    "own_schedule_overridden a", "count_of_panels a", "program_equals_own a",
    "whole_refresher a", "whole_refresher b", "refresher_own_schedule a",
    "refresher_own_schedule b", "subrounds_overridden a"
  ))
})

test_that("a phase drawn from a program's component replays to its units", {
  case <- rpr_cases()$own_schedule_overridden
  frame <- rpr_frame()
  part <- rpr_occasion(case)$a
  phase <- suppressMessages(
    execute(sampling_design() |> draw(n = 4), part, seed = 7)
  )
  drawn <- rlang::hash(sort(as.data.frame(phase)$id))
  # The phase must come from the program's activation, not the selection's own.
  expect_true(all(as.data.frame(phase)$.panel == 1L))

  expect_identical(
    rlang::hash(sort(as.data.frame(suppressMessages(
      replay_design(phase, frame)
    ))$id)),
    drawn
  )
  expect_identical(
    rlang::hash(sort(as.data.frame(rpr_json_replay(phase, frame))$id)),
    drawn
  )
})

test_that("a program occasion's receipt records the program's part of it", {
  cases <- rpr_cases()
  part <- rpr_occasion(cases$subrounds_overridden)$a
  written <- jsonlite::fromJSON(design_json(part, frame = rpr_frame()),
                                simplifyVector = TRUE)$execution$occasion
  expect_identical(written$selection, "a")
  expect_identical(as.integer(written$entry_period), 1L)
  expect_identical(as.integer(written$period), 2L)
  expect_identical(as.integer(written$subround), 2L)
  expect_identical(as.integer(written$n_subrounds), 2L)
  expect_identical(as.integer(written$active_panels), 2L)
  expect_identical(as.integer(written$visits), 3L)

  # An ordinary occasion's receipt records none of them.
  source <- cases$own_schedule_overridden$program$selections$a
  ordinary <- suppressMessages(execute(source, period = 2))
  plain <- jsonlite::fromJSON(design_json(ordinary, frame = rpr_frame()),
                              simplifyVector = TRUE)$execution$occasion
  expect_setequal(names(plain),
                  c("occasion", "active_panels", "schedule_digest", "pools"))
})

test_that("a receipt whose occasion contradicts its record is refused", {
  frame <- rpr_frame()
  part <- rpr_occasion(rpr_cases()$own_schedule_overridden)$a
  json <- design_json(part, frame = frame)
  edit <- function(from, to) {
    expect_identical(lengths(regmatches(json, gregexpr(from, json,
                                                         fixed = TRUE))), 1L)
    sub(from, to, json, fixed = TRUE)
  }
  replay <- function(text) {
    suppressMessages(replay_design(read_design(text), frame))
  }

  # A panel the selection does not have.
  expect_error(replay(edit('"active_panels":[1]', '"active_panels":[3]')),
               class = "samplyr_error_replay_occasion")
  # One visit for no panel.
  expect_error(replay(edit('"visits":[2]', '"visits":[2,3]')),
               class = "samplyr_error_replay_occasion")
  # Pools the activation does not rebuild.
  expect_error(replay(edit('"take":[2,2,2,2,2]', '"take":[2,2,2,2,4]')),
               class = "samplyr_error_replay_occasion")
  # Without the program's part, the program's digest is not the selection's.
  bare <- edit(
    ',"selection":"a","entry_period":1,"period":2,"subround":1,"n_subrounds":1,"visits":[2]',
    ""
  )
  expect_false(grepl('"selection"', bare, fixed = TRUE))
  expect_error(replay(bare), class = "samplyr_error_replay_occasion")

  # An ordinary occasion whose digest is another schedule's.
  source <- rpr_cases()$own_schedule_overridden$program$selections$a
  ordinary <- design_json(suppressMessages(execute(source, period = 2)),
                          frame = frame)
  digest <- regmatches(ordinary,
                       regexpr('"schedule_digest":"[0-9a-f]+"', ordinary))
  expect_length(digest, 1L)
  expect_error(
    replay(sub(digest, '"schedule_digest":"00000000000000000000000000000000"',
               ordinary, fixed = TRUE)),
    class = "samplyr_error_replay_occasion"
  )
  # An ordinary occasion recording fewer panels than its schedule activates.
  expect_identical(lengths(regmatches(
    ordinary, gregexpr('"active_panels":[1,2]', ordinary, fixed = TRUE)
  )), 1L)
  expect_error(
    replay(sub('"active_panels":[1,2]', '"active_panels":[1]', ordinary,
               fixed = TRUE)),
    class = "samplyr_error_replay_occasion"
  )
})

test_that("a design file of format 4 is refused", {
  part <- rpr_occasion(rpr_cases()$own_schedule_overridden)$a
  json <- design_json(part, frame = rpr_frame())
  expect_true(grepl('"format_version":5,', json, fixed = TRUE))
  expect_error(
    read_design(sub('"format_version":5,', '"format_version":4,', json,
                    fixed = TRUE)),
    class = "samplyr_error_design_file_unsupported"
  )
})

test_that("a component prints the program's period before and after replay", {
  cases <- rpr_cases()
  refresher <- rpr_occasion(cases$refresher_own_schedule)$b
  subrounds <- rpr_occasion(cases$subrounds_overridden)$a
  header <- function(x) {
    out <- utils::capture.output(print(x))
    trimws(sub("^#\\s*Occasion:\\s*", "", grep("Occasion:", out, value = TRUE)))
  }
  expect_identical(header(refresher), "period 3")
  expect_identical(header(replay_design(refresher, rpr_entrants())), "period 3")
  whole <- rpr_occasion(cases$whole_refresher)$b
  expect_identical(header(whole), "period 3")
  expect_identical(header(rpr_json_replay(whole, rpr_entrants())), "period 3")
  expect_identical(header(subrounds), "period.subround 2.2")
  expect_identical(
    header(rpr_json_replay(subrounds, rpr_frame())),
    "period.subround 2.2"
  )
})

test_that("an ordinary occasion and a phase drawn from it still replay", {
  frame <- rpr_frame()
  master <- rpr_draw(frame, 20, 2, rpr_own())
  occasion <- suppressMessages(execute(master, period = 2))
  phase <- suppressMessages(
    execute(sampling_design() |> draw(n = 4), occasion, seed = 7)
  )
  for (x in list(occasion, phase)) {
    ids <- sort(as.data.frame(x)$id)
    expect_identical(sort(as.data.frame(replay_design(x, frame))$id), ids)
    expect_identical(sort(as.data.frame(rpr_json_replay(x, frame))$id), ids)
  }
})
