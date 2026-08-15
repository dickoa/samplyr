# A sample carrying a weight-share transformation, built from the C0
# primitives because share_weights() does not exist yet. The links are
# one-to-one so the target weights equal the source weights, which keeps the
# fixture's arithmetic out of the way of what the tests are about: the
# contract the object declares, not the transformation that produced it.
#
# Everything a real share_weights() result must have is here, so a gate that
# passes this fixture will pass a real one.

shared_weight_source <- function(seed = 1) {
  sampling_design() |>
    draw(n = 12) |>
    execute(bfa_eas, seed = seed)
}

# `extra_metadata` is merged in after the transformation is attached, so a
# fixture can reach gates that sit behind an earlier structural check. The
# stack_waves() gate is the case: it runs after that verb has established its
# arguments are materialized waves, so a shared sample with no wave record
# never gets that far.
shared_weight_sample <- function(source = shared_weight_source(),
                                 extra_metadata = list()) {
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
    # The source's own integrity record, not a fresh one: what has to be
    # verified later is that the retained sample is still the realization the
    # weights were shared from.
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

  # The recorded design and stages still describe selection from the source
  # population, which is what print and summary have to say.
  result <- new_tbl_sample(
    data = targets,
    design = get_design(source),
    stages_executed = get_stages_executed(source),
    seed = attr(source, "seed"),
    metadata = list()
  )
  out <- attach_weight_share_record(result, record)
  if (length(extra_metadata) > 0) {
    meta <- attr(out, "metadata")
    attr(out, "metadata") <- utils::modifyList(meta, extra_metadata)
  }
  out
}
