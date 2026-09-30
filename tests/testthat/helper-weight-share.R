# A weight-share sample built from the internal primitives, with one-to-one
# links so the target weights equal the source weights. It carries everything
# a real share_weights() result has, so a gate that passes it passes a real one.

shared_weight_source <- function(seed = 1) {
  sampling_design() |>
    draw(n = 12) |>
    execute(bfa_eas, seed = seed)
}

shared_weight_sample <- function(source = shared_weight_source()) {
  n <- nrow(source)
  targets <- tibble::tibble(
    person_id = paste0("p", seq_len(n)),
    hh_id = paste0("h", seq_len(n)),
    .weight = source$.weight,
    .unit_links = rep(1L, n),
    .cluster_links = rep(1L, n)
  )

  record <- new_weight_share_record(
    operator = new_share_operator(
      target_row = seq_len(n),
      source_row = seq_len(n),
      share = rep(1, n),
      n_target = n,
      n_source = n
    ),
    source_sample = source,
    # The source's own integrity record, so the retained realization is checked.
    source_integrity = attr(source, "metadata")$integrity,
    source_key_cols = ".sample_id",
    target_key_cols = "person_id",
    target_cluster = "hh_id",
    within_mode = "cluster",
    denominator = list(mode = "supplied", scale = "binary"),
    coverage = new_weight_share_coverage(
      target_scope = "reached",
      n_target_units = n,
      n_target_clusters = n,
      n_reached_clusters = n,
      n_unlinked_units = 0L
    ),
    generated_cols = weight_share_generated_cols$binary,
    call_info = list(fn = "share_weights")
  )

  # The design and stages still describe selection from the source population.
  result <- new_tbl_sample(
    data = targets,
    design = get_design(source),
    stages_executed = get_stages_executed(source),
    seed = attr(source, "seed"),
    metadata = list()
  )
  attach_weight_share_record(result, record)
}

# A real master and one of its waves, for the tests that need an object the
# longitudinal gates accept. Two panels over two waves, one active at a time,
# which is the smallest schedule that materializes.
wave_share_master <- function(seed = 1) {
  sampling_design() |>
    draw(n = 12) |>
    execute(
      bfa_eas,
      seed = seed,
      panels = data.frame(
        panel = rep(1:2, times = 2),
        wave = rep(1:2, each = 2),
        active = c(TRUE, FALSE, FALSE, TRUE)
      )
    )
}

## Target rows a share operator never names
share_operator_unreached <- function(op) {
  setdiff(seq_len(op$n_target), unique(op$target_row))
}
