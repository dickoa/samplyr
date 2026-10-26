# Extracted from test-share-weights-quantitative.R:303

# prequel ----------------------------------------------------------------------
wl_source_frame <- function() {
  data.frame(unit = c("a", "b", "c", "d"), stringsAsFactors = FALSE)
}
wl_targets <- function() {
  data.frame(
    tid = c("t1", "t2", "t3"),
    hh = c("H1", "H1", "H2"),
    y = c(1, 10, 100),
    stringsAsFactors = FALSE
  )
}
wl_links <- function() {
  data.frame(
    unit = c("a", "a", "b", "c", "d"),
    tid = c("t1", "t2", "t1", "t3", "t3"),
    B = c(3, 1, 2, 4, 6),
    stringsAsFactors = FALSE
  )
}
wl_all_samples <- function() {
  frame <- wl_source_frame()
  seen <- list()
  for (s in 1:200) {
    smp <- sampling_design() |> draw(n = 2) |> execute(frame, seed = s)
    key <- paste(sort(smp$unit), collapse = "")
    if (is.null(seen[[key]])) {
      seen[[key]] <- smp
    }
  }
  seen
}
wl_share <- function(sample, targets = wl_targets(), links = wl_links(), ...) {
  share_weights(
    sample,
    targets = targets, links = links,
    by = c(unit = "unit"), to = c(tid = "tid"),
    within = hh,
    multiplicity = weighted_links(B, total = complete_links()),
    ...
  )
}

# test -------------------------------------------------------------------------
s <- wl_all_samples()[["ab"]]
infinite <- wl_links()
infinite$B[1] <- Inf
expect_error(
    wl_share(s, links = infinite),
    class = "samplyr_error_share_weights_multiplicity"
  )
