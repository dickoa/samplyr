# Extracted from test-replicate-extraction.R:563

# prequel ----------------------------------------------------------------------
extraction_frame <- function() data.frame(id = 1:20, y = 1:20)
listed_stack <- function() {
  sampling_design() |>
    draw(n = 3, reserve = 2) |>
    execute(extraction_frame(), seed = 1, reps = 2)
}
code_take <- function(sample, k = 1L) {
  sample$code <- ifelse(sample[[paste0(".reserve_", k)]] == 0L, 1, NA)
  sample
}
listed_alone <- function(stack, r) {
  seed <- attr(stack, "metadata")$replicate_seeds[[r]]
  sampling_design() |>
    draw(n = 3, reserve = 2) |>
    execute(extraction_frame(), seed = seed)
}
total_se <- function(svy) {
  est <- survey::svytotal(~y, svy, na.rm = TRUE)
  c(total = unname(stats::coef(est)), se = unname(survey::SE(est)))
}
replay_both_ways <- function(sample, frame, ...) {
  path <- withr::local_tempfile(fileext = ".json")
  write_design(sample, path, frame = frame)
  list(
    file = replay_design(read_design(path), frame, ...),
    memory = replay_design(sample, frame, ...)
  )
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
frame <- data.frame(
    id = 1:100, stratum = rep(c("A", "B"), each = 50),
    x = (1:100 %% 9) + 1, y = 400 + (1:100 %% 13) * 10
  )
big <- data.frame(id = 1:1000, x = (1:1000 %% 9) + 1,
                    y = 400 + (1:1000 %% 13) * 10)
fit <- function(s) {
    e <- survey::svytotal(~y, suppressWarnings(as_svydesign(s)))
    c(unname(coef(e)), unname(survey::SE(e)))
  }
cases <- list(
    shared = list(frame = frame,
                  d1 = sampling_design() |> stratify_by(stratum) |>
                    draw(n = 15)),
    apart = list(frame = big, d1 = sampling_design() |> draw(n = 10))
  )
d2 <- sampling_design() |> draw(n = 6, method = "pps_brewer", mos = x)
seeds <- samplyr:::replicate_seed_values(2, 3)
for (case in names(cases)) {
    p1 <- execute(cases[[case]]$d1, cases[[case]]$frame, reps = 3, seed = 1)
    p2 <- execute(d2, p1, seed = 2)
    for (r in c(1L, 3L)) {
      alone <- dplyr::filter(p2, .replicate == r)
      # The same draw from that replicate of phase 1, as a phase of its own.
      direct <- execute(d2, dplyr::filter(p1, .replicate == r),
                        seed = seeds[r])
      expect_identical(alone$id, direct$id)
      info <- paste(case, r)
      expect_equal(fit(alone), fit(direct), info = info)
      expect_equal(fit(alone)[1], sum(alone$y * alone$.weight), info = info)
      phase1 <- samplyr:::get_prev_phase(alone)$sample
      expect_identical(unique(phase1$.replicate), r)
      expect_true(samplyr:::sample_realization_status(phase1)$ok)
      expect_identical(variance_estimators(alone)$status,
                       variance_estimators(direct)$status, info = info)
    }
  }
alone <- dplyr::filter(p2, .replicate == 2L)
path <- withr::local_tempfile(fileext = ".json")
suppressWarnings(write_design(alone, path, frame = big))
replayed <- replay_design(read_design(path), big)
expect_identical(replayed$id, alone$id)
expect_equal(fit(replayed), fit(alone))
rds <- withr::local_tempfile(fileext = ".rds")
saveRDS(alone, rds)
expect_equal(fit(readRDS(rds)), fit(alone))
cut <- dplyr::filter(p1, .replicate == 2L, x > 3)
expect_error(
    as_svydesign(suppressWarnings(execute(d2, cut, seed = 2))),
    class = "samplyr_error_modified_sample"
  )
