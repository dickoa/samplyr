# Extracted from test-activation-joint.R:71

# prequel ----------------------------------------------------------------------
enumerate_block <- function(labels, first, second) {
  m <- length(labels)
  in_first <- labels %in% first
  in_second <- labels %in% second

  same <- mean(in_first & in_second)
  distinct <- if (m < 2L) {
    NA_real_
  } else {
    pairs <- 0
    for (a in seq_len(m)) {
      for (b in seq_len(m)) {
        if (a != b && in_first[a] && in_second[b]) {
          pairs <- pairs + 1
        }
      }
    }
    pairs / (m * (m - 1))
  }
  list(marginal = mean(in_first), same = same, distinct = distinct)
}
block_labels <- function(pool, block) {
  rep(seq_len(ncol(pool$quotas)), pool$quotas[block, ])
}
joint_frame <- function(n = 400) {
  data.frame(
    id = seq_len(n),
    region = rep(c("North", "South"), each = n / 2),
    mos = rep(c(rep(10, (n / 2) - 3), 900, 950, 1000), times = 2),
    value = seq_len(n) / n
  )
}
joint_rotation <- function() {
  data.frame(
    panel = rep(1:4, times = 4),
    period = rep(1:4, each = 4),
    active = c(
      TRUE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, TRUE, TRUE,
      TRUE, FALSE, FALSE, TRUE
    )
  )
}
joint_master <- function(seed = 42, n = 60) {
  sampling_design() |>
    stratify_by(region) |>
    draw(n = n) |>
    execute(joint_frame(), seed = seed, panels = joint_rotation())
}

# test -------------------------------------------------------------------------
master <- joint_master()
record <- attr(master, "metadata")$panel_assignment
schedule <- record$schedule
for (pair in list(c(1L, 2L), c(1L, 3L), c(2L, 2L), c(4L, 1L))) {
    result <- joint_expectation(master, waves = pair)
    first <- schedule$panel[schedule$wave == pair[1] & schedule$active]
    second <- schedule$panel[schedule$wave == pair[2] & schedule$active]

    expect_identical(unique(result$wave_1), pair[1])
    expect_identical(unique(result$wave_2), pair[2])

    for (i in seq_len(nrow(result))) {
      pool <- record$pools[[result$pool[i]]]
      reference <- enumerate_block(
        block_labels(pool, result$block[i]),
        first,
        second
      )
      expect_equal(result$prob_1[i], reference$marginal)
      expect_equal(result$joint_same[i], reference$same)
      expect_equal(result$joint_distinct[i], reference$distinct)
    }
  }
