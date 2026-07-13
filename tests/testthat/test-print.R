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
