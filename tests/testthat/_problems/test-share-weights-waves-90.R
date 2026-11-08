# Extracted from test-share-weights-waves.R:90

# prequel ----------------------------------------------------------------------
waves_source_frame <- function() {
  data.frame(hh = c("H1", "H2", "H3", "H4", "H5"))
}
waves_rosters <- function() {
  wave1 <- data.frame(
    person = 1:10,
    hh = c("H1", "H1", "H2", "H2", "H3", "H4", "H4", "H4", "H5", "H5"),
    y = c(3, 5, 2, 8, 13, 1, 4, 6, 9, 10)
  )
  wave2 <- data.frame(
    person = c(1, 3, 4, 11, 5, 6, 8, 10, 2, 9, 12, 13),
    hh = c("H1", "H2", "H2", "H2", "H3", "H4", "H4", "H5", "K1", "K1",
           "M1", "M1"),
    y = c(4, 2, 9, 7, 15, 2, 6, 11, 6, 12, 20, 30)
  )
  wave1$in_w1 <- 1L
  wave2$in_w1 <- as.integer(wave2$person <= 10)
  list(wave1, wave2)
}
waves_all_samples <- function() {
  seen <- list()
  for (s in 1:300) {
    smp <- sampling_design() |>
      draw(n = 2) |>
      execute(waves_source_frame(), seed = s)
    key <- paste(sort(smp$hh), collapse = "")
    if (is.null(seen[[key]])) {
      seen[[key]] <- smp
    }
    if (length(seen) == 10L) break
  }
  seen
}
waves_follow <- function(sample, waves) {
  rosters <- waves_rosters()
  members <- rosters[[1]][rosters[[1]]$hh %in% sample$hh, ]
  parts <- lapply(waves, function(t) {
    roster <- rosters[[t]]
    alive <- members[members$person %in% roster$person, ]
    reached <- roster[
      roster$hh %in% roster$hh[roster$person %in% alive$person],
    ]
    list(
      targets = data.frame(
        key = paste(t, reached$person), period = t,
        hh_t = paste(t, reached$hh), y = reached$y, in_w1 = reached$in_w1
      ),
      links = data.frame(hh = alive$hh, key = paste(t, alive$person))
    )
  })
  share_weights(
    sample,
    targets = do.call(rbind, lapply(parts, `[[`, "targets")),
    links = do.call(rbind, lapply(parts, `[[`, "links")),
    by = c(hh = "hh"), to = c(key = "key"),
    within = hh_t, multiplicity = in_w1
  )
}
waves_change <- function(sample) {
  stacked <- waves_follow(sample, 1:2)
  stacked$change <- ifelse(stacked$wave == 2, stacked$y, -stacked$y)
  stacked
}

# test -------------------------------------------------------------------------
for (smp in waves_all_samples()) {
    stacked <- as.data.frame(waves_follow(smp, 1:2))
    for (t in 1:2) {
      alone <- as.data.frame(waves_follow(smp, t))
      part <- stacked[stacked$wave == t, ]
      expect_identical(
        part$.weight[order(part$key)],
        alone$.weight[order(alone$key)]
      )
    }
  }
