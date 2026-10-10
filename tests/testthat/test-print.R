# Coverage for print formatting helpers: stage labels, the scope qualifier
# on scalar/named n, control variables, and the incomplete-stage notice.

test_that("print shows stage labels, allocation scope, and control variables", {
  design <- sampling_design(title = "Demo") |>
    add_stage(label = "Districts") |>
    stratify_by(stratum, alloc = "proportional") |>
    draw(n = 20, method = "systematic", control = c(cluster, y))

  out <- capture.output(print(design))
  txt <- paste(out, collapse = "\n")
  expect_match(txt, "Districts")
  expect_match(txt, "total")        # alloc set -> n is the total
  expect_match(txt, "control = ")
})

test_that("print qualifies a scalar n as per-stratum when no alloc is given", {
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = 10)

  txt <- paste(capture.output(print(design)), collapse = "\n")
  expect_match(txt, "per stratum")
})

test_that("print describes a named per-stratum n vector", {
  design <- sampling_design() |>
    stratify_by(stratum) |>
    draw(n = c(A = 5, B = 5, C = 5, D = 5))

  txt <- paste(capture.output(print(design)), collapse = "\n")
  expect_match(txt, "per stratum")
})

test_that("print flags a stage with no draw specification", {
  design <- sampling_design() |>
    add_stage(label = "Incomplete")

  txt <- paste(capture.output(print(design)), collapse = "\n")
  expect_match(txt, "Incomplete: no draw specification", fixed = TRUE)
  expect_false(grepl("\u2014", txt, fixed = TRUE))
})

test_that("print shows a single control variable without c() wrapping", {
  design <- sampling_design() |>
    draw(n = 5, method = "systematic", control = cluster)

  txt <- paste(capture.output(print(design)), collapse = "\n")
  expect_match(txt, "control = cluster")
})

test_that("compact coverage omits the universe after a WR ancestor", {
  frame <- data.frame(
    psu = rep(1:2, each = 5),
    unit = rep(1:5, 2)
  )
  sample <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 3, method = "srswr") |>
    add_stage() |> draw(n = 2) |>
    execute(frame, seed = 3)

  header <- tbl_sum(sample)
  expect_identical(unname(header["Sampling"]), "2 stages | 6 units")
  expect_false(grepl("/10 units", unname(header["Sampling"]), fixed = TRUE))

  txt <- paste(capture.output(summary(sample)), collapse = "\n")
  expect_match(txt, "n = 6 | stages = 2/2", fixed = TRUE)
  expect_false(grepl("n = 6 of 10", txt, fixed = TRUE))
})

test_that("print shows balanced and spatial declarations compactly", {
  cube <- sampling_design() |>
    draw(
      n = 10,
      method = "cube",
      mos = size,
      aux = c(x, bound(group))
    )
  cube_txt <- paste(capture.output(print(cube)), collapse = "\n")
  expect_match(cube_txt, "method = cube", fixed = TRUE)
  expect_match(cube_txt, "mos = size", fixed = TRUE)
  expect_match(cube_txt, "aux = x", fixed = TRUE)
  expect_match(cube_txt, "count bounds = bound(group)", fixed = TRUE)

  spatial <- sampling_design() |>
    draw(n = 10, method = "lpm2", spread = c(longitude, latitude))
  spatial_txt <- paste(capture.output(print(spatial)), collapse = "\n")
  expect_match(spatial_txt, "method = lpm2", fixed = TRUE)
  expect_match(spatial_txt, "spread = longitude, latitude", fixed = TRUE)
})

## The coverage line counts units of one frame

coverage_fixture <- function() {
  frame <- data.frame(psu = rep(1:20, each = 6), id = 1:120)
  design <- sampling_design() |>
    add_stage() |> cluster_by(psu) |> draw(n = 5) |>
    add_stage() |> draw(n = 2)
  list(frame = frame, design = design)
}

sampling_line <- function(s) {
  grep("Sampling:", cli::ansi_strip(utils::capture.output(print(s, n = 0))),
       value = TRUE)
}

test_that("one shared frame gives units selected out of its rows", {
  fx <- coverage_fixture()
  s <- execute(fx$design, fx$frame, seed = 1)
  expect_identical(
    sampling_line(s),
    paste0("# Sampling:     2 stages | 10/", nrow(fx$frame), " units")
  )
  expect_true(any(grepl(
    paste0("n = 10 of ", nrow(fx$frame)),
    cli::ansi_strip(utils::capture.output(summary(s))),
    fixed = TRUE
  )))
})

test_that("separate registers and a continuation give the count alone", {
  fx <- coverage_fixture()
  psus <- fx$frame[!duplicated(fx$frame$psu), "psu", drop = FALSE]
  registers <- execute(fx$design, list(psus, fx$frame), seed = 1)
  expect_identical(sampling_line(registers), "# Sampling:     2 stages | 10 units")
  summary_lines <- cli::ansi_strip(utils::capture.output(summary(registers)))
  expect_true(any(grepl("n = 10 | stages", summary_lines, fixed = TRUE)))

  first <- execute(fx$design, fx$frame, stages = 1, seed = 1)
  listing <- fx$frame[fx$frame$psu %in% first$psu, ]
  listing <- listing[rep(seq_len(nrow(listing)), 2), ]
  listing$id <- seq_len(nrow(listing))
  continued <- execute(first, listing, seed = 2)
  expect_identical(sampling_line(continued), "# Sampling:     2 stages | 10 units")
})

## summary() returns what it prints

test_that("the summary object's figures are the package's own", {
  cut <- stats::quantile(ken_enterprises$revenue_millions, 0.99)
  design <- sampling_design() |>
    stratify_by(sector) |>
    draw(n = 80, method = "pps_brewer", mos = revenue_millions,
         certainty_size = cut)
  for (digest in c("summary", "full", "none")) {
    s <- execute(design, ken_enterprises, seed = 1, frame_digest = digest)
    x <- summary(s)
    expect_s3_class(x, "summary_tbl_sample")
    expect_identical(x$n, nrow(s))
    expect_equal(x$design_effect, design_effect(s))
    expect_equal(x$effective_n, effective_n(s))
    expect_identical(x$certainty, c(stage_1 = sum(s$.certainty_1)))
    # The certainty count reaches the print under every digest mode.
    printed <- cli::ansi_strip(utils::capture.output(print(x)))
    expect_true(
      any(grepl(paste(sum(s$.certainty_1), "certainty selections"), printed,
                fixed = TRUE)),
      label = digest
    )
  }
})

test_that("a clustered stage counts certainty units, not their rows", {
  frame <- data.frame(psu = rep(1:30, each = 4), id = 1:120)
  frame$size <- rep(c(80, 60, rep(5, 28)), each = 4)
  s <- sampling_design() |>
    add_stage() |> cluster_by(psu) |>
    draw(n = 6, method = "pps_brewer", mos = size, certainty_size = 50) |>
    add_stage() |> draw(n = 2) |>
    execute(frame, seed = 1)
  expect_identical(
    summary(s)$certainty[["stage_1"]],
    length(unique(s$psu[s$.certainty_1]))
  )
  expect_identical(summary(s)$certainty[["stage_1"]], 2L)
})

## Long key lists keep their count outside the quotes

test_that("a truncated key list never quotes its remainder", {
  frame <- data.frame(st = paste0("s", 1:100), id = 1:100)
  messages <- c(
    conditionMessage(tryCatch(
      sampling_design() |> stratify_by(st) |>
        draw(n = c(s1 = 1, s2 = 1)) |> execute(frame, seed = 1),
      error = identity
    )),
    conditionMessage(tryCatch(
      sampling_design() |> stratify_by(st) |>
        draw(n = data.frame(st = c("s1", "s2"), n = 1)) |>
        execute(frame, seed = 1),
      error = identity
    )),
    conditionMessage(tryCatch(
      sampling_design() |> stratify_by(st) |>
        draw(n = stats::setNames(rep(1, 60), paste0("z", 1:60))) |>
        execute(frame, seed = 1),
      error = identity
    ))
  )
  plain <- cli::ansi_strip(messages)
  expect_false(any(grepl("\"... and", plain, fixed = TRUE)))
  expect_true(all(grepl("and [0-9]+ more", plain)))

  # The formatters return every key and leave the bound to the message.
  keys <- data.frame(st = paste0("s", 1:100), g = "x")
  expect_identical(format_key_labels(keys, "st"), keys$st)
  expect_identical(format_key_preview(keys), paste0(keys$st, "/x"))
  expect_identical(
    format_key_preview(data.frame(a = c(1, 10, 100), b = "x")),
    c("1/x", "10/x", "100/x")
  )
})
