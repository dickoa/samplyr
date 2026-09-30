test_frame <- function(n = 1000) {
  set.seed(123)
  data.frame(
    id = seq_len(n),
    region = rep(c("North", "South", "East", "West"), each = n / 4),
    urban_rural = rep(c("Urban", "Rural"), n / 2),
    school_id = rep(seq_len(n / 10), each = 10),
    enrollment = rep(sample(100:500, n / 10, replace = TRUE), each = 10),
    value = rnorm(n)
  )
}

test_that("control sorting is applied for systematic sampling", {
  frame <- test_frame()

  # Shuffled, so the frame is not already sorted by region.
  set.seed(999)
  frame <- frame[sample(nrow(frame)), ]
  rownames(frame) <- NULL

  result_no_control <- sampling_design() |>
    draw(n = 100, method = "systematic") |>
    execute(frame, seed = 42)

  result_with_control <- sampling_design() |>
    draw(n = 100, method = "systematic", control = region) |>
    execute(frame, seed = 42)

  expect_false(identical(
    sort(result_no_control$id),
    sort(result_with_control$id)
  ))

  control_counts <- table(result_with_control$region)
  expect_true(all(control_counts > 0))
})

test_that("control sorting with multiple variables works", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(n = 100, method = "systematic", control = c(region, urban_rural)) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100)

  combo_counts <- table(result$region, result$urban_rural)
  expect_true(all(combo_counts > 0))
})

test_that("control sorting with serp() works", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(n = 100, method = "systematic", control = serp(region, urban_rural)) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100)
  expect_true(all(table(result$region) > 0))
})

test_that("control sorting within stratified sampling works", {
  frame <- test_frame()

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 25, method = "systematic", control = urban_rural) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100) # 4 strata x 25

  for (r in unique(result$region)) {
    r_data <- result[result$region == r, ]
    expect_true(length(unique(r_data$urban_rural)) == 2)
  }
})

test_that("control sorting allows strata variables", {
  frame <- test_frame()

  result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 25, method = "systematic", control = region) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100) # 4 strata x 25
})

test_that("control sorting preserves correctness for srswor (order-insensitive)", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(n = 100, control = region) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100)
  expect_equal(unique(result$.weight), nrow(frame) / 100)
})

test_that("multi-stage stratified weights differ by stratum", {
  # Unequal strata, so equal allocation gives each a different probability.
  frame <- data.frame(
    id = 1:200,
    psu = rep(1:40, each = 5),
    region = c(rep("North", 50), rep("South", 150)),
    value = rnorm(200)
  )

  # Stage-1 weights are 2 (North) and 6 (South), stage-2 weights 5/2.
  result <- sampling_design() |>
    add_stage() |>
    stratify_by(region, alloc = "equal") |>
    cluster_by(psu) |>
    draw(n = 10) |>
    add_stage() |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  expect_equal(
    result$.weight,
    result$.weight_1 * result$.weight_2,
    tolerance = 1e-10
  )

  north_weights <- unique(result$.weight_1[result$region == "North"])
  south_weights <- unique(result$.weight_1[result$region == "South"])
  if (length(north_weights) > 0 && length(south_weights) > 0) {
    expect_false(identical(north_weights, south_weights))
  }
})

test_that("multi-stage path with equal strata still works", {
  frame <- data.frame(
    id = 1:200,
    psu = rep(1:40, each = 5),
    region = rep(c("North", "South"), each = 100),
    value = rnorm(200)
  )

  result <- sampling_design() |>
    add_stage() |>
    stratify_by(region, alloc = "proportional") |>
    cluster_by(psu) |>
    draw(n = 10) |>
    add_stage() |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  expect_true(all(result$.weight > 0))
  expect_true(".weight_1" %in% names(result))
  expect_true(".weight_2" %in% names(result))
})

test_that("pps_multinomial uses expected hits for probabilities", {
  skip_if_not_installed("sondage")
  frame <- data.frame(
    id = 1:20,
    size = c(
      100,
      200,
      300,
      50,
      150,
      80,
      120,
      250,
      90,
      170,
      60,
      110,
      140,
      180,
      220,
      70,
      130,
      160,
      190,
      210
    )
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 5L)
  expect_true(".draw_1" %in% names(result))
  expect_equal(result$.draw_1, 1:5)

  total_size <- sum(frame$size)
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (5 * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("pps_chromy uses correct weights and draws", {
  skip_if_not_installed("sondage")
  frame <- data.frame(
    id = 1:20,
    size = c(
      100,
      200,
      300,
      50,
      150,
      80,
      120,
      250,
      90,
      170,
      60,
      110,
      140,
      180,
      220,
      70,
      130,
      160,
      190,
      210
    )
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_chromy", mos = size) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 5L)
  expect_true(".draw_1" %in% names(result))
  expect_equal(result$.draw_1, 1:5)

  total_size <- sum(frame$size)
  for (i in seq_len(nrow(result))) {
    expected_weight <- total_size / (5 * result$size[i])
    expect_equal(result$.weight[i], expected_weight, tolerance = 1e-10)
  }
})

test_that("pps_multinomial dominant unit gets many draws", {
  skip_if_not_installed("sondage")
  frame <- data.frame(
    id = 1:10,
    size = c(1000, rep(10, 9)) # One dominant unit
  )

  result <- sampling_design() |>
    draw(n = 5, method = "pps_multinomial", mos = size) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 5L)
  expect_equal(result$.draw_1, 1:5)

  # Expected hits for id 1 are 5 * 1000 / 1090 = 4.59.
  n_dominant_draws <- sum(result$id == 1)
  expect_true(n_dominant_draws >= 4)

  expect_true(all(result$.weight > 0))
})

test_that("%||% operator works after removing custom definition", {
  frame <- test_frame()

  # round defaults to "up" via %||%
  result <- sampling_design() |>
    draw(frac = 0.1) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), ceiling(nrow(frame) * 0.1))
})

test_that("print works with data-frame n", {
  alloc_df <- data.frame(
    region = c("North", "South"),
    n = c(10, 20)
  )

  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = alloc_df)

  output <- capture.output(print(design))
  expect_true(any(grepl("custom data frame", output)))
})

test_that("print works with data-frame frac", {
  alloc_df <- data.frame(
    region = c("North", "South"),
    frac = c(0.1, 0.2)
  )

  design <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = alloc_df)

  output <- capture.output(print(design))
  expect_true(any(grepl("custom data frame", output)))
})

test_that("print works with scalar n", {
  design <- sampling_design() |> draw(n = 100)

  output <- capture.output(print(design))
  expect_true(any(grepl("n = 100", output)))
  # Unstratified n carries no scope qualifier.
  expect_false(any(grepl("n = 100 \\(", output)))
})

test_that("print qualifies scalar n as per-stratum when stratified w/o alloc", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 100)

  output <- capture.output(print(design))
  expect_true(any(grepl("n = 100 \\(per stratum\\)", output)))
})

test_that("print qualifies scalar n as total when stratified w/ alloc", {
  design <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 100)

  output <- capture.output(print(design))
  expect_true(any(grepl("n = 100 \\(total\\)", output)))
})

test_that("print shows per-stratum tag for named-vector n", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(n = c(a = 10, b = 20, c = 30))

  output <- capture.output(print(design))
  expect_true(any(grepl("<3 values, per stratum>", output)))
})

test_that("print qualifies scalar frac as per-stratum when stratified", {
  design <- sampling_design() |>
    stratify_by(region) |>
    draw(frac = 0.1)

  output <- capture.output(print(design))
  expect_true(any(grepl("frac = 0.1 \\(per stratum\\)", output)))
})


test_that("[.tbl_sample preserves class on row subsetting", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  subset <- sample[1:10, ]
  expect_s3_class(subset, "tbl_sample")
  expect_true(is_tbl_sample(subset))

  expect_equal(get_design(subset)$title, get_design(sample)$title)
})

test_that("[.tbl_sample strips class when essential columns removed", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  subset <- sample[, c("id", "region")]
  expect_false(is_tbl_sample(subset))
})

test_that("[.tbl_sample preserves class on column subsetting with essentials", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  subset <- sample[, c("id", "region", ".weight")]
  expect_s3_class(subset, "tbl_sample")
})

test_that("sample_stratified gives correct results (implicit group_modify test)", {
  frame <- test_frame()

  result <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 200) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 200)
  expect_equal(length(unique(result$region)), 4)

  expect_true(all(result$.weight > 0))

  expect_equal(sum(result$.weight), nrow(frame), tolerance = 1)
})

test_that("sample_within_clusters (split+lapply) gives correct results", {
  frame <- test_frame()

  result <- sampling_design() |>
    add_stage(label = "Schools") |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    add_stage(label = "Students") |>
    draw(n = 5) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 100)

  school_counts <- table(result$school_id)
  expect_true(all(school_counts == 5))

  expect_true(all(result$.weight > 0))
  expect_true(".weight_1" %in% names(result))
  expect_true(".weight_2" %in% names(result))
})

test_that("stratified and within-clusters both produce correct weights", {
  frame <- test_frame()

  strat_result <- sampling_design() |>
    stratify_by(region) |>
    draw(n = 25) |>
    execute(frame, seed = 42)

  expect_equal(sum(strat_result$.weight), nrow(frame), tolerance = 1)

  twostage_result <- sampling_design() |>
    add_stage() |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    add_stage() |>
    draw(n = 5) |>
    execute(frame, seed = 42)

  expect_equal(sum(twostage_result$.weight), nrow(frame), tolerance = 50)
})

test_that("stratified systematic selection with control reproduces by seed", {
  frame <- test_frame()

  design <- sampling_design() |>
    stratify_by(region, alloc = "proportional") |>
    draw(n = 200, method = "systematic", control = urban_rural)

  result1 <- execute(design, frame, seed = 42)
  result2 <- execute(design, frame, seed = 42)

  expect_equal(result1$id, result2$id)
  expect_equal(result1$.weight, result2$.weight)
})

test_that("control sorting with desc() works", {
  frame <- test_frame()

  result <- sampling_design() |>
    draw(
      n = 50,
      method = "systematic",
      control = c(region, dplyr::desc(value))
    ) |>
    execute(frame, seed = 42)

  expect_equal(nrow(result), 50)
})

test_that("tbl_sum.tbl_sample prints header exactly once", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  output <- capture.output(print(sample))
  header_lines <- grep("A tbl_sample", output)
  expect_length(header_lines, 1)
})

test_that("tbl_sum.tbl_sample shows dimensions", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  output <- capture.output(print(sample))
  expect_true(any(grepl("100", output)))
})

test_that("tbl_sum.tbl_sample shows weight summary", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  output <- capture.output(print(sample))
  expect_true(any(grepl("Weights", output)))
})

test_that("tbl_sum.tbl_sample returns correct named vector", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  s <- pillar::tbl_sum(sample)
  expect_named(s[1], "A tbl_sample")
  expect_true("Weights" %in% names(s))
  expect_true(grepl("100", s[["A tbl_sample"]]))
})

test_that("tbl_sum header appears once after select", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  result <- dplyr::select(sample, id, region, .weight)
  output <- capture.output(print(result))
  header_lines <- grep("A tbl_sample", output)
  expect_length(header_lines, 1)
})

test_that("tbl_sum header appears once after select and head", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  result <- sample |>
    dplyr::select(id, region, .weight) |>
    head()
  output <- capture.output(print(result))
  header_lines <- grep("A tbl_sample", output)
  expect_length(header_lines, 1)
})

test_that("tbl_sum header appears once after filter", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  result <- dplyr::filter(sample, region == "North")
  output <- capture.output(print(result))
  header_lines <- grep("A tbl_sample", output)
  expect_length(header_lines, 1)
})

test_that("tbl_sum header appears once after mutate", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  result <- dplyr::mutate(sample, double_w = .weight * 2)
  output <- capture.output(print(result))
  header_lines <- grep("A tbl_sample", output)
  expect_length(header_lines, 1)
})

test_that("tbl_sample class is never duplicated after dplyr operations", {
  frame <- test_frame()

  sample <- sampling_design() |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  after_select <- dplyr::select(sample, id, region, .weight)
  expect_equal(sum(class(after_select) == "tbl_sample"), 1)

  after_head <- head(sample, 10)
  expect_equal(sum(class(after_head) == "tbl_sample"), 1)

  after_chain <- sample |>
    dplyr::select(id, region, .weight) |>
    head()
  expect_equal(sum(class(after_chain) == "tbl_sample"), 1)

  after_filter <- dplyr::filter(sample, region == "North")
  expect_equal(sum(class(after_filter) == "tbl_sample"), 1)
})

test_that("tbl_sum.tbl_sample shows title when present", {
  frame <- test_frame()

  sample <- sampling_design(title = "My Survey") |>
    draw(n = 100) |>
    execute(frame, seed = 42)

  output <- capture.output(print(sample))
  expect_true(any(grepl("My Survey", output)))

  s <- pillar::tbl_sum(sample)
  expect_true(grepl("My Survey", s[["A tbl_sample"]]))
})

test_that("tbl_sum.tbl_sample shows partial stages", {
  frame <- test_frame()

  design <- sampling_design() |>
    add_stage() |>
    cluster_by(school_id) |>
    draw(n = 20) |>
    add_stage() |>
    draw(n = 5)

  sample <- execute(design, frame, stages = 1, seed = 42)

  s <- pillar::tbl_sum(sample)
  expect_true("Stages" %in% names(s))
  expect_true(grepl("1/2", s[["Stages"]]))
})

test_that("multi-stage stratified then unstratified compounding works", {
  frame <- data.frame(
    id = 1:300,
    psu = rep(1:60, each = 5),
    region = rep(c("A", "B", "C"), each = 100),
    value = rnorm(300)
  )

  result <- sampling_design() |>
    add_stage() |>
    stratify_by(region, alloc = "proportional") |>
    cluster_by(psu) |>
    draw(n = 15) |>
    add_stage() |>
    draw(n = 2) |>
    execute(frame, seed = 42)

  expect_equal(
    result$.weight,
    result$.weight_1 * result$.weight_2,
    tolerance = 1e-10
  )

  expect_true(all(result$.weight > 0))
})


test_that("as_tbl_sample is a no-op on a tbl_sample", {
  expect_identical(as_tbl_sample(fix_srs), fix_srs)
  expect_identical(as_tbl_sample(fix_multistage), fix_multistage)
})

test_that("as_tbl_sample restores class from as_tibble()", {
  stripped <- tibble::as_tibble(fix_srs)
  expect_false(is_tbl_sample(stripped))
  expect_true(!is.null(attr(stripped, "design")))

  restored <- as_tbl_sample(stripped)
  expect_s3_class(restored, "tbl_sample")
  expect_equal(get_design(restored), get_design(fix_srs))
  expect_equal(get_stages_executed(restored), get_stages_executed(fix_srs))
  expect_equal(restored$.weight, fix_srs$.weight)
})

test_that("as_tbl_sample restores class from as.data.frame()", {
  stripped <- as.data.frame(fix_strat_prop)
  expect_false(is_tbl_sample(stripped))

  restored <- as_tbl_sample(stripped)
  expect_s3_class(restored, "tbl_sample")
  expect_equal(
    get_stages_executed(restored),
    get_stages_executed(fix_strat_prop)
  )
})

test_that("as_tbl_sample errors on plain data frame without attributes", {
  expect_error(as_tbl_sample(data.frame(x = 1)), "sampling attributes")
  expect_error(as_tbl_sample(test_frame()), "sampling attributes")
})

test_that("as_tbl_sample preserves all metadata through roundtrip", {
  stripped <- tibble::as_tibble(fix_multistage)
  restored <- as_tbl_sample(stripped)

  expect_equal(attr(restored, "seed"), attr(fix_multistage, "seed"))
  expect_equal(attr(restored, "metadata"), attr(fix_multistage, "metadata"))
  expect_equal(ncol(restored), ncol(fix_multistage))
  expect_equal(nrow(restored), nrow(fix_multistage))
})

test_that("dplyr joins preserve tbl_sample class", {
  extra <- data.frame(
    stratum = c("A", "B", "C", "D"),
    pop = c(1000, 2000, 3000, 4000)
  )
  for (join_fn in list(
    dplyr::left_join,
    dplyr::inner_join,
    dplyr::semi_join,
    dplyr::anti_join
  )) {
    result <- join_fn(fix_strat_prop, extra, by = "stratum")
    expect_s3_class(result, "tbl_sample")
    expect_equal(get_design(result), get_design(fix_strat_prop))
  }
})

test_that("base merge strips class and attributes", {
  extra <- data.frame(
    stratum = c("A", "B", "C", "D"),
    pop = c(1000, 2000, 3000, 4000)
  )
  result <- merge(fix_strat_prop, extra, by = "stratum")
  expect_false(is_tbl_sample(result))
  expect_null(attr(result, "design"))
  expect_error(as_tbl_sample(result), "sampling attributes")
})

test_that("an expanded listing is accepted as a continuation frame", {
  skip_if_not_installed("tidyr")

  frame <- test_frame()
  design <- sampling_design() |>
    add_stage(label = "School") |>
    cluster_by(school_id) |>
    draw(n = 10, method = "pps_brewer", mos = enrollment) |>
    add_stage(label = "Student") |>
    draw(n = 3, method = "srswor")

  stage1 <- execute(design, frame, stages = 1, seed = 1)

  listing <- stage1 |>
    tidyr::uncount(weights = enrollment, .id = "hh_id", .remove = FALSE)

  expect_false(is_tbl_sample(listing))
  expect_true(!is.null(attr(listing, "design")))

  expect_error(
    design |> execute(listing, seed = 2),
    class = "samplyr_error_stripped_sample_frame"
  )

  signature_only <- listing
  attr(signature_only, "design") <- NULL
  attr(signature_only, "stages_executed") <- NULL
  attr(signature_only, "seed") <- NULL
  attr(signature_only, "metadata") <- NULL
  expect_error(
    design |> execute(signature_only, seed = 2),
    "For operational multistage sampling",
    class = "samplyr_error_stripped_sample_frame"
  )

  # The clean partial sample supplies the design and the stage-1 state.
  expect_no_warning(
    final <- stage1 |> execute(listing, seed = 2)
  )
  expect_s3_class(final, "tbl_sample")
  expect_equal(get_stages_executed(final), c(1L, 2L))
  expect_true(all(final$.weight > 0))

  restored <- as_tbl_sample(listing)
  expect_s3_class(restored, "tbl_sample")
  # Restored, the expanded listing does not match its executed realization.
  expect_identical(samplyr:::sample_modifications(restored), "rows")

  # Starting again from the design is a new phase, so the continuation is named.
  expect_warning(
    design |> execute(restored, seed = 2),
    "continue from the unmodified partial sample"
  )
})

test_that("a lone user weight column is reserved but not sample provenance", {
  frame <- data.frame(id = seq_len(20), .weight = rep(1, 20))

  expect_false(samplyr:::looks_like_stripped_tbl_sample(frame))
  expect_error(
    sampling_design() |>
      draw(n = 5) |>
      execute(frame, seed = 1),
    class = "samplyr_error_frame_reserved_names"
  )
})
