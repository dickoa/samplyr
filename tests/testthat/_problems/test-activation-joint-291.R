# Extracted from test-activation-joint.R:291

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
with_version <- function(value) {
    metadata <- attr(master, "metadata")
    metadata$panel_assignment$version <- value
    attr(master, "metadata") <- metadata
    master
  }
for (bad in list(0L, -5L, 6L, 99L, c(7L, 7L), NULL, "7", NA_integer_, 6.5)) {
    expect_error(
      joint_expectation(with_version(bad), waves = c(1L, 2L)),
      class = "samplyr_error_panel_record_unsupported"
    )
  }
expect_s3_class(
    joint_expectation(with_version(7L), waves = c(1L, 2L)),
    "tbl_df"
  )
