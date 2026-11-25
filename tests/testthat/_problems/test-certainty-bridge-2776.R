# Extracted from test-certainty-bridge.R:2776

# prequel ----------------------------------------------------------------------
certainty_bridge_design <- function(plan = certainty_plan_fixture()) {
  sampling_design("bridge") |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    draw(n = plan)
}
certainty_tampered_file <- function(path, f) {
  doc <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  doc <- f(doc)
  out <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  jsonlite::write_json(
    doc, out,
    auto_unbox = TRUE, digits = NA, na = "null", null = "null"
  )
  out
}
segment_frame <- function() {
  frame <- certainty_element_frame()
  frame$segment <- ceiling(frame$person / 100)
  frame$segment_size <- ave(
    frame$person, frame$psu_id, frame$segment, FUN = length
  )
  frame
}
segment_design <- function(plan, per_psu = 1) {
  sampling_design() |>
    add_stage() |>
    stratify_by(stratum) |>
    cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |>
    cluster_by(segment) |>
    draw(n = per_psu, method = "pps_systematic", mos = segment_size) |>
    add_stage()
}
systematic_jip <- function(pik) {
  ends <- cumsum(pik)
  starts <- ends - pik
  cuts <- sort(unique(c(0, 1, (c(starts, ends) %% 1))))
  out <- matrix(0, length(pik), length(pik))
  for (k in seq_len(length(cuts) - 1L)) {
    u <- (cuts[k] + cuts[k + 1L]) / 2 + 0:(ceiling(sum(pik)) - 1L)
    taken <- vapply(seq_along(pik), function(i) {
      any(u > starts[i] & u <= ends[i])
    }, logical(1))
    out[taken, taken] <- out[taken, taken] + (cuts[k + 1L] - cuts[k])
  }
  out
}
pair_design_frame <- function() {
  certainty_element_frame(certainty_pair_register())
}
pair_sample <- function(method = "pps_brewer", seed = 4) {
  frame <- pair_design_frame()
  frame$y <- (frame$person %% 7) + (frame$N %% 5)
  execute(certainty_zone_design(certainty_pair_fixture(), method), frame, seed = seed)
}
certainty_free_take_fixture <- function() {
  svyplan::n_alloc_joint(
    data.frame(stratum = c("A", "B"), N = c(2000, 1600), cost_1 = 300,
               cost_2 = 30),
    data.frame(stratum = c("A", "B"), name = "y", p = c(0.5, 0.4),
               icc_1 = 0.05),
    targets = data.frame(name = "y", cv = 0.10),
    psu = certainty_plan_register(), take_2_min = 8, take_2_max = 30
  )
}
certainty_planned_chances <- function(plan, s) {
  psu <- plan$psu
  draw <- stats::setNames(plan$detail$n_psu_draw, plan$detail$stratum)
  rest <- ave(ifelse(psu$certainty, 0, psu$N), psu$stratum, FUN = sum)
  pik <- ifelse(psu$certainty, 1, draw[psu$stratum] * psu$N / rest)
  at <- match(s$psu_id, psu$psu_id)
  list(stage_1 = unname(pik[at]), stage_2 = psu$take_2[at] / psu$N[at])
}
numeric_id_register <- function() {
  register <- certainty_plan_register()
  register$psu_id <- 100000L + seq_len(nrow(register)) - 1L
  register
}
flat_message <- function(err) {
  gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
}
i64 <- function(x) bit64::as.integer64(x)
large_id_register <- function(register = certainty_plan_register(),
                              from = "9007199254740992", step = 1L) {
  register$psu_id <- i64(from) + step * (seq_len(nrow(register)) - 1L)
  register
}

# test -------------------------------------------------------------------------
register <- certainty_plan_register()
register$stratum <- ifelse(register$stratum == "A", 1e5, 2e5)
plan <- withr::with_options(list(scipen = 0), svyplan::n_alloc_joint(
    data.frame(stratum = c(1e5, 2e5), N = c(2000, 1600), take_2 = 10),
    measures = data.frame(stratum = c(1e5, 2e5), name = "y",
                          p = c(0.5, 0.4), icc_1 = 0.05),
    targets = data.frame(name = "y", cv = 0.10), psu = register
  ))
design <- sampling_design() |>
    add_stage() |> stratify_by(stratum) |> cluster_by(psu_id) |>
    draw(n = plan, method = "pps_systematic", mos = N) |>
    add_stage() |> draw(n = plan)
stored <- design$stages[[1]]$draw_spec$certainty_plan$register$stratum
expect_true(all(grepl("e+05", stored, fixed = TRUE)))
path <- withr::local_tempfile(fileext = ".json")
withr::with_options(list(scipen = 0), write_design(design, path))
elements <- certainty_element_frame(register)
drawn <- lapply(c(0, 100), function(scipen) {
    withr::with_options(list(scipen = scipen), suppressWarnings(
      execute(read_design(path), list(register, elements), seed = 1)
    ))
  })
expect_identical(drawn[[2]]$psu_id, drawn[[1]]$psu_id)
certain <- register$psu_id[register$certainty]
for (s in drawn) {
    expect_setequal(unique(s$psu_id[s$.certainty_1]), certain)
  }
