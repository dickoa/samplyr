## Change between two waves of a followed panel, from one shared-weight call

# The source is five households, srswor n = 2, so the ten samples are listed
# and every expectation is exact. Everyone in a selected household at wave 1
# is followed. At wave 2, person 7 has died, persons 2 and 9 have left H1 and
# H5 to form K1, a baby (11) is born in H2, and M1 arrives with nobody from
# wave 1, so no sample reaches it.

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

# Targets are keyed by wave and person, and clusters by wave and household,
# so one call can hold several waves.
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
        key = paste(t, reached$person), wave = t,
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

## The stacked call

test_that("each wave of a stacked call has the weights of its own call", {
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
})

test_that("a household formed from two sources shares their weight", {
  smp <- waves_all_samples()[["H1H2"]]
  out <- as.data.frame(waves_follow(smp, 2))

  # K1 holds person 2 of H1 and person 9 of the unselected H5.
  expect_identical(out$.weight[out$hh_t == "2 K1"], c(1.25, 1.25))
  expect_identical(out$.cluster_links[out$hh_t == "2 K1"], c(2, 2))
  expect_identical(out$.weight[out$key == "2 11"], 2.5)
  expect_false(any(grepl("M1", out$hh_t)))
})

## Expectations, by exact enumeration

test_that("the stacked change is exactly unbiased for the reachable change", {
  rosters <- waves_rosters()
  reachable <- rosters[[2]][rosters[[2]]$hh != "M1", ]
  truth <- sum(reachable$y) - sum(rosters[[1]]$y)

  estimates <- vapply(waves_all_samples(), function(smp) {
    out <- waves_change(smp)
    sum(out$.weight * out$change)
  }, numeric(1))

  expect_identical(truth, 13)
  expect_equal(mean(estimates), truth)
})

test_that("both variance routes are exactly unbiased for the change", {
  skip_if_not_installed("survey")
  samples <- waves_all_samples()

  fits <- vapply(samples, function(smp) {
    out <- waves_change(smp)
    lin <- survey::svytotal(~change, as_svydesign(out))
    jk <- survey::svytotal(~change, as_svrepdesign(out, type = "JK1"))
    c(
      estimate = coef(lin)[[1]],
      linearized = survey::SE(lin)[[1]]^2,
      jackknife = survey::SE(jk)[[1]]^2
    )
  }, numeric(3))
  totals <- vapply(samples, function(smp) {
    vapply(1:2, function(t) {
      out <- waves_follow(smp, t)
      sum(out$.weight * out$y)
    }, numeric(1))
  }, numeric(2))

  design_var <- function(x) mean((x - mean(x))^2)
  true_var <- design_var(fits["estimate", ])
  covariance <- mean(
    (totals[1, ] - mean(totals[1, ])) * (totals[2, ] - mean(totals[2, ]))
  )

  expect_equal(true_var, 129.75)
  expect_equal(mean(fits["linearized", ]), true_var)
  expect_equal(mean(fits["jackknife", ]), true_var)

  # Treating the waves as independent would drop a covariance of 82.875.
  expect_equal(covariance, 82.875)
  expect_equal(
    design_var(totals[1, ]) + design_var(totals[2, ]) - 2 * covariance,
    true_var
  )
})
