# Extracted from test-stack-frames-svrepdesign.R:293

# prequel ----------------------------------------------------------------------
svrep_population <- function() {
  data.frame(
    id = 1:60,
    y = as.numeric(1:60),
    st = rep(c("x", "y", "z", "w"), length.out = 60),
    size = rep(c(2, 5, 3, 8), length.out = 60),
    in_a = rep(c(TRUE, FALSE), times = c(40, 20)),
    in_b = rep(c(FALSE, TRUE), times = c(20, 40))
  )
}
svrep_fixture <- function(population = svrep_population()) {
  stack_frames(
    a = sampling_design() |>
      stratify_by(st) |>
      draw(n = 2) |>
      execute(population[population$in_a, , drop = FALSE], seed = 1),
    b = sampling_design() |>
      draw(n = 20) |>
      execute(population[population$in_b, , drop = FALSE], seed = 2),
    membership = c(a = "in_a", b = "in_b"),
    key = id
  )
}
svrep_factor <- function(component, cols = c("in_a", "in_b")) {
  1 / rowSums(vapply(cols, function(col) {
    as.numeric(component[[col]])
  }, numeric(nrow(component))))
}
svrep_component_variance <- function(component, factor, type = "auto") {
  rep_design <- suppressWarnings(
    as_svrepdesign(component, type = type, mse = TRUE)
  )
  design <- survey::svrepdesign(
    data = as.data.frame(component),
    repweights = stats::weights(rep_design, "analysis") * factor,
    weights = stats::weights(rep_design, "sampling") * factor,
    combined.weights = TRUE,
    type = "other",
    scale = rep_design$scale,
    rscales = rep_design$rscales,
    mse = TRUE
  )
  survey::SE(survey::svytotal(~y, design))^2
}

# test -------------------------------------------------------------------------
skip_if_not_installed("survey")
dwellings <- data.frame(dwelling_id = 1:20)
source_sample <- sampling_design() |>
    draw(n = 8) |>
    execute(dwellings, seed = 11)
people <- data.frame(
    person_id = 1:20,
    dwelling_id = 1:20,
    y = as.numeric(1:20),
    in_reached = TRUE,
    in_list = rep(c(TRUE, FALSE), times = 10)
  )
links <- data.frame(dwelling_id = 1:20, person_id = 1:20)
shared <- share_weights(
    source_sample,
    targets = people,
    links = links,
    by = c(dwelling_id = "dwelling_id"),
    to = c(person_id = "person_id"),
    within = NULL,
    multiplicity = complete_links()
  )
listed <- sampling_design() |>
    draw(n = 5) |>
    execute(people[people$in_list, , drop = FALSE], seed = 12)
frames <- stack_frames(
    reached = shared,
    list = listed,
    membership = c(reached = "in_reached", list = "in_list"),
    key = person_id
  )
combined <- suppressWarnings(
    as_svrepdesign(frames, type = "bootstrap", replicates = 30)
  )
