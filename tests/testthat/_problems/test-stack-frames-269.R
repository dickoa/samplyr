# Extracted from test-stack-frames.R:269

# prequel ----------------------------------------------------------------------
frame_population <- function() {
  data.frame(
    id = 1:6,
    in_a = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE),
    in_b = c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
  )
}
frame_component <- function(col, population = frame_population(), seed = NULL) {
  rows <- population[population[[col]], , drop = FALSE]
  sampling_design() |>
    draw(n = nrow(rows)) |>
    execute(rows, seed = seed)
}
frame_fixture <- function(population = frame_population()) {
  stack_frames(
    a = frame_component("in_a", population),
    b = frame_component("in_b", population),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )
}
frame_view <- function(frames) {
  view <- as.data.frame(frames)
  view[order(match(view$.frame, names(frames)), view$id), , drop = FALSE]
}

# test -------------------------------------------------------------------------
population <- data.frame(
    region = rep(c("N", "S"), each = 6),
    id = rep(1:6, 2),
    in_a = rep(c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE), 2),
    in_b = rep(c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE), 2)
  )
a <- frame_component("in_a", population)
b <- frame_component("in_b", population)
frames <- stack_frames(a = a, b = b, membership = c(a = "in_a", b = "in_b"),
                         key = c(region, id))
expect_identical(attr(frames, "key"), c("region", "id"))
expect_output(print(frames), "key region and id")
expect_error(
    stack_frames(a = a, b = b, membership = c(a = "in_a", b = "in_b"),
                 key = id),
    "repeats",
    class = "samplyr_error_stack_frames_key"
  )
conflicted <- b
conflicted$in_a[conflicted$region == "N" & conflicted$id == 3] <- FALSE
expect_error(
    stack_frames(a = a, b = conflicted,
                 membership = c(a = "in_a", b = "in_b"),
                 key = c(region, id)),
    "region = N, id = 3",
    class = "samplyr_error_stack_frames_membership_conflict"
  )
path <- withr::local_tempfile(fileext = ".json")
rows <- function(col) population[population[[col]], , drop = FALSE]
write_design(frames, path, frame = list(a = rows("in_a"), b = rows("in_b")))
expect_true(any(grepl('"key": ["region", "id"]', readLines(path),
                        fixed = TRUE)))
replayed <- replay_design(
    read_design(path),
    list(a = rows("in_a"), b = rows("in_b"))
  )
