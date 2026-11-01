# Extracted from test-share-weights-linearized.R:281

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
blocks <- unique(dwellings[c("blk", "region")])
source_sample <- sampling_design() |>
    cluster_by(blk) |> draw(n = 6) |>
    add_stage() |> draw(frac = 1, on_empty = "silent") |>
    execute(list(blocks, dwellings[dwellings$blk != "b1", ]), seed = 1)
expect_false("b1" %in% source_sample$blk)
shared <- expect_matches_oracle(source_sample)
data <- as_svydesign(shared)$variables
expect_identical(data$blk[is.na(data$person_id)], "b1")
set.seed(5)
got <- as_svrepdesign(shared, type = "JK1")
set.seed(5)
source_rep <- as_svrepdesign(source_sample, type = "JK1")
derived <- linearized_oracle(shared, source_sample)
expect_equal(
    unname(survey::SE(survey::svytotal(~y, got, na.rm = TRUE))),
    unname(survey::SE(derived))
  )
