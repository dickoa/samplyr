# Extracted from test-svyplan-compat.R:417

# prequel ----------------------------------------------------------------------
set.seed(1193)
deff_frame <- data.frame(
  id = 1:200,
  stratum = rep(c("A", "B"), each = 100),
  cluster = rep(1:40, each = 5),
  income = rnorm(200, 50, 10),
  census_pop = rnorm(200, 100, 20)
)
fix_deff_disprop <- sampling_design() |>
  stratify_by(stratum) |>
  draw(n = c(A = 10, B = 40)) |>
  execute(deff_frame, seed = 1)
fix_deff_prop <- sampling_design() |>
  stratify_by(stratum, alloc = "proportional") |>
  draw(n = 50) |>
  execute(deff_frame, seed = 1)
fix_deff_srs <- sampling_design() |>
  draw(n = 50) |>
  execute(deff_frame, seed = 1)
fix_deff_strat_clust <- sampling_design() |>
  add_stage("psu") |>
  stratify_by(stratum) |>
  cluster_by(cluster) |>
  draw(n = c(A = 10, B = 10)) |>
  add_stage("ssu") |>
  draw(n = 3) |>
  execute(deff_frame, seed = 1)
fix_deff_clust <- sampling_design() |>
  add_stage("psu") |>
  cluster_by(cluster) |>
  draw(n = 20) |>
  add_stage("ssu") |>
  draw(n = 3) |>
  execute(deff_frame, seed = 1)

# test -------------------------------------------------------------------------
tg <- data.frame(
    name = "stunting",
    domain = c("urban", "rural"),
    p = c(0.25, 0.35),
    cv = 0.08,
    icc_psu = 0.05
  )
nm <- svyplan::n_cluster(
    indicators = tg,
    domains = "domain",
    stage_cost = c(500, 50)
  )
frame <- data.frame(
    domain = rep(c("urban", "rural"), each = 3000),
    ea = rep(sprintf("EA%03d", 1:300), each = 20),
    hh = 1:6000
  )
drawn <- function(plan) {
    res <- sampling_design() |>
      stratify_by(domain) |> cluster_by(ea) |> draw(n = plan) |>
      add_stage() |> stratify_by(domain) |> draw(n = plan) |>
      execute(frame, seed = 2)
    eas <- unique(as.data.frame(res)[c("ea", "domain")])
    list(
      psu = c(table(eas$domain)[c("urban", "rural")]),
      take = c(table(res$domain)[c("urban", "rural")]) /
        c(table(eas$domain)[c("urban", "rural")])
    )
  }
whole <- nm$domains[2:1, c("domain", "n_psu", "n_per_psu")]
whole$n_psu <- floor(whole$n_psu)
whole$n_per_psu <- floor(whole$n_per_psu)
nm$operational$domains <- whole
continuous <- nm$domains[match(c("urban", "rural"), nm$domains$domain), ]
expect_true(all(floor(continuous$n_psu) < continuous$n_psu))
expect_equal(
    drawn(nm),
    list(
      psu = c(urban = floor(continuous$n_psu[1]),
              rural = floor(continuous$n_psu[2])),
      take = c(urban = floor(continuous$n_per_psu[1]),
               rural = floor(continuous$n_per_psu[2]))
    )
  )
