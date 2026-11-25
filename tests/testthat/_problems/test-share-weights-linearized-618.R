# Extracted from test-share-weights-linearized.R:618

# prequel ----------------------------------------------------------------------
linearized_targets <- function() {
  withr::with_seed(3, data.frame(
    person_id = 1:90,
    hh = rep(paste0("h", 1:45), each = 2),
    y = round(stats::rnorm(90, 100, 20), 3)
  ))
}
linearized_links <- function() {
  rbind(
    data.frame(dw_id = rep(1:45, each = 2), person_id = 1:90),
    # 15 people reached through a second dwelling as well.
    data.frame(dw_id = 46:60, person_id = 1:15)
  )
}
linearized_dwellings <- function() {
  data.frame(
    dw_id = 1:60,
    region = rep(c("n", "s"), each = 30),
    size = rep(c(2, 5, 3, 8, 4, 6), length.out = 60),
    blk = rep(paste0("b", 1:12), each = 5)
  )
}
linearized_shared <- function(source_sample,
                              targets = linearized_targets()) {
  share_weights(
    source_sample,
    targets = targets,
    links = linearized_links(),
    by = c(dw_id = "dw_id"),
    to = c(person_id = "person_id"),
    within = hh,
    multiplicity = complete_links()
  )
}
linearized_oracle <- function(shared, source_sample, variable = "y") {
  operator <- attr(shared, "metadata")$weight_share$operator
  contributions <- operator$share * shared[[variable]][operator$target_row]
  totals <- rowsum(contributions, group = operator$source_row, reorder = FALSE)

  derived <- rep(0, nrow(source_sample))
  derived[as.integer(rownames(totals))] <- totals[, 1]
  source_sample$.derived <- derived
  suppressWarnings(
    survey::svytotal(~.derived, as_svydesign(source_sample), na.rm = TRUE)
  )
}
expect_matches_oracle <- function(source_sample,
                                 targets = linearized_targets()) {
  shared <- linearized_shared(source_sample, targets)
  got <- suppressWarnings(
    survey::svytotal(~y, as_svydesign(shared), na.rm = TRUE)
  )
  want <- linearized_oracle(shared, source_sample)

  expect_equal(unname(coef(got)), unname(coef(want)))
  expect_equal(unname(survey::SE(got)), unname(survey::SE(want)))
  invisible(shared)
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
dwellings <- linearized_dwellings()
dwellings$st <- paste0("s", seq_len(nrow(dwellings)))
source <- sampling_design() |> stratify_by(st) |> draw(n = 1) |>
    execute(dwellings, seed = 1)
shared <- share_weights(
    source, targets = linearized_targets(), links = linearized_links(),
    by = c(dw_id = "dw_id"), to = c(person_id = "person_id"), within = hh,
    multiplicity = complete_links()
  )
svy <- as_svydesign(shared)
total <- survey::svytotal(~y, svy)
expect_equal(unname(survey::SE(total)), 0)
