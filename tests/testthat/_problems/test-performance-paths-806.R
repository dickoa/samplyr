# Extracted from test-performance-paths.R:806

# test -------------------------------------------------------------------------
design <- sampling_design() |>
    add_stage() |> stratify_by(h) |> cluster_by(psu) |> draw(n = 2) |>
    add_stage() |> draw(n = 1)
base <- data.frame(
    psu = rep(1:6, each = 3),
    h = rep(c("a", "b"), each = 9),
    id = 1:18,
    when = as.Date("2020-01-01") + 0:17
  )
frames <- list(
    automatic = base,
    integer_names = base[c(2, 1, 3:18), ],
    character_names = `rownames<-`(base, paste0("r", 1:18)),
    tibble = tibble::as_tibble(base),
    list_column = within(base, extra <- as.list(id))
  )
for (nm in names(frames)) {
    frame <- frames[[nm]]
    # An equal copy is a different object, so it takes the general path.
    copy <- vctrs::vec_slice(frame, seq_len(nrow(frame)))
    attributes(copy) <- attributes(frame)
    fast <- link_stage_frame(frame, frame, design, 2L, check_coverage = FALSE)
    general <- link_stage_frame(frame, copy, design, 2L,
                                check_coverage = FALSE)
    # Base identical(): waldo reads explicit 1:n row names as automatic.
    expect_true(identical(fast, general), info = nm)
  }
