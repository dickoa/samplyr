
<!-- README.md is generated from README.Rmd. Please edit that file -->

# samplyr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/samplyr)](https://CRAN.R-project.org/package=samplyr)
[![R-CMD-check](https://gitlab.com/dickoa/samplyr/badges/main/pipeline.svg)](https://gitlab.com/dickoa/samplyr/-/pipelines)
[![Codecov test
coverage](https://codecov.io/gl/dickoa/samplyr/branch/main/graph/badge.svg)](https://app.codecov.io/gl/dickoa/samplyr?branch=main)
<!-- badges: end -->

A tidy grammar for survey sampling in R. `samplyr` provides a minimal
set of composable verbs for stratified, clustered, multi-stage, and
multi-phase sampling designs with PPS methods, sample coordination, and
panel rotation.

## Ecosystem Positioning

`samplyr` sits in the middle of a three-layer workflow:

- Use `svyplan` for planning (`n`, precision, power, allocation,
  budget).
- Use `samplyr` to specify designs, draw samples, and carry design
  metadata through fieldwork.
- Use `survey`/`srvyr` after data collection for estimation and
  inference.

These packages are complementary, not competing. Handoffs are explicit
(for example, `svyplan` outputs can feed `draw(n = ...)`, and `samplyr`
outputs convert via `as_svydesign()`, `as_svrepdesign()`, and
`as_survey_design()`).

## Why samplyr?

`samplyr` is built around a simple idea: sampling code should read like
its English description.

``` r
library(samplyr)
data(bfa_eas)

# "Stratify by region, proportionally allocate 500 samples, execute"
sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 500) |>
  execute(bfa_eas, seed = 1)
#> # A tbl_sample: 500 × 17
#> # Weights:      29.87 [29.29, 30.32]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_10182 Boucle … Mouhoun  Ouarko… Rural             1347        185    33.0 
#>  2 EA_03982 Boucle … Kossi    Doumba… Rural             1558        191    16.6 
#>  3 EA_10352 Boucle … Bale     Ouri    Rural              767         98    17.6 
#>  4 EA_03209 Boucle … Mouhoun  Dedoug… Rural              446         79    18.1 
#>  5 EA_12908 Boucle … Banwa    Tansila Rural             1076        129     9.45
#>  6 EA_11640 Boucle … Banwa    Sami    Rural              912        137    43.2 
#>  7 EA_06884 Boucle … Banwa    Kouka   Rural             1642        226    12.5 
#>  8 EA_13757 Boucle … Nayala   Toma    Rural              973        141     0.88
#>  9 EA_05785 Boucle … Sourou   Kassoum Rural             1315        195     1.5 
#> 10 EA_03598 Boucle … Kossi    Djibas… Rural              725         93     1.01
#> # ℹ 490 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>
```

Consider a real survey design from Lohr (2022, Example 7.1), based on a
1991 study of bed net use in rural Gambia (D’Alessandro et al., 1994):

> Malaria morbidity can be reduced by using bed nets impregnated with
> insecticide, but this is only effective if the bed nets are in
> widespread use. In 1991, a nationwide survey was designed to estimate
> the prevalence of bed net use in rural areas of the Gambia
> (D’Alessandro et al., 1994).
>
> The sampling frame consisted of all rural villages of fewer than 3,000
> people. The villages were **stratified by three geographic regions**
> (eastern, central, and western) and by **whether the village had a
> public health clinic (PHC)** or not. In each region **five districts
> were chosen with probability proportional to the district
> population**. In each district **four villages were chosen, again with
> probability proportional to census population**: two PHC villages and
> two non-PHC villages. Finally, **six compounds were chosen** more or
> less randomly from each village.

In `samplyr`, this three-stage stratified cluster design translates
directly into code:

``` r
design <- sampling_design(title = "Gambia bed nets") |>
  add_stage() |>
    stratify_by(region) |>
    cluster_by(district) |>
    draw(n = 5, method = "pps_brewer", mos = population) |>
  add_stage() |>
    stratify_by(phc) |>
    cluster_by(village) |>
    draw(n = 2, method = "pps_brewer", mos = population) |>
  add_stage() |>
    draw(n = 6)
design
#> ── Sampling Design: Gambia bed nets ────────────────────────────────────────
#> 
#> ℹ 3 stages
#> 
#> ── Stage 1 ─────────────────────────────────────────────────────────────────
#> • Strata: region
#> • Cluster: district
#> • Draw: n = 5, method = pps_brewer, mos = population
#> 
#> ── Stage 2 ─────────────────────────────────────────────────────────────────
#> • Strata: phc
#> • Cluster: village
#> • Draw: n = 2, method = pps_brewer, mos = population
#> 
#> ── Stage 3 ─────────────────────────────────────────────────────────────────
#> • Draw: n = 6, method = srswor
```

The `samplyr` code mirrors the verbal description verb for verb.

*Lohr, S. L. (2022). Sampling: Design and Analysis (3rd ed.). CRC
Press.*

## Installation

``` r
# Install sondage first (sampling algorithms backend)
pak::pkg_install("gitlab::dickoa/sondage")

# Install svyplan (sample size, precision, power, and stratification)
pak::pkg_install("gitlab::dickoa/svyplan")

# Install samplyr
pak::pkg_install("gitlab::dickoa/samplyr")
```

## The Grammar

`samplyr` uses 5 verbs and 1 modifier:

| Function            | Purpose                               |
|---------------------|---------------------------------------|
| `sampling_design()` | Create a new sampling design          |
| `stratify_by()`     | Define stratification and allocation  |
| `cluster_by()`      | Define cluster/PSU variable           |
| `draw()`            | Specify sample size and method        |
| `execute()`         | Run the design on a frame             |
| `add_stage()`       | Delimit stages in multi-stage designs |

### Frame-Independent Design

`stratify_by()` and `cluster_by()` take bare column names. The design is
stored as a specification and resolved only when a frame is available
(`validate_frame()`, `execute()`, `as_svydesign()`), so design
specification stays separate from execution.

``` r
design <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  cluster_by(ea_id) |>
  draw(n = 300)

sample <- execute(design, bfa_eas, seed = 2)
sample
#> # A tbl_sample: 300 × 17
#> # Weights:      49.78 [47.64, 51.25]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_00466 Boucle … Bale     Bana    Rural             1272        166     1.13
#>  2 EA_10367 Boucle … Bale     Ouri    Rural             2128        272    11.1 
#>  3 EA_00373 Boucle … Banwa    Balave  Rural             1624        256    57.0 
#>  4 EA_00380 Boucle … Banwa    Balave  Rural             1005        158    26.5 
#>  5 EA_06887 Boucle … Banwa    Kouka   Rural             1748        241    18.1 
#>  6 EA_12399 Boucle … Banwa    Solenzo Rural             1012        136    27.3 
#>  7 EA_12420 Boucle … Banwa    Solenzo Rural               79         11     5.68
#>  8 EA_12444 Boucle … Banwa    Solenzo Rural             1316        177    15.5 
#>  9 EA_12901 Boucle … Banwa    Tansila Rural             1267        152    44.5 
#> 10 EA_12917 Boucle … Banwa    Tansila Rural              789         95    15.3 
#> # ℹ 290 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>
```

## Quick Start

``` r
library(samplyr)
data(bfa_eas)

# Simple random sample
srs_smpl <- sampling_design() |>
  draw(n = 100) |>
  execute(bfa_eas, seed = 321)

srs_smpl
#> # A tbl_sample: 100 × 17
#> # Weights:      149.34 [149.34, 149.34]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_00678 Sahel    Seno     Bani    Rural              981        210    42.8 
#>  2 EA_12032 Nord     Yatenga  Sengue… Rural              720         76     0.67
#>  3 EA_11736 Boucle … Banwa    Sanaba  Rural             1074        137    25.2 
#>  4 EA_04335 Est      Komandj… Foutou… Rural             1349        219   167.  
#>  5 EA_06603 Centre-… Sanmate… Korsim… Rural             1706        338    19.2 
#>  6 EA_14749 Plateau… Oubrite… Ziniare Rural             1497        214    31.2 
#>  7 EA_09105 Centre   Kadiogo  Ouagad… Urban             2755        415     0.9 
#>  8 EA_07612 Plateau… Oubrite… Loumbi… Urban             2993        539    14.0 
#>  9 EA_12806 Centre   Kadiogo  Tangue… Urban             2585        477     6.14
#> 10 EA_03463 Centre-… Sanguie  Didyr   Rural             1315        201     9.3 
#> # ℹ 90 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>

# Stratified proportional allocation
strata_smpl <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300) |>
  execute(bfa_eas, seed = 12)

strata_smpl
#> # A tbl_sample: 300 × 17
#> # Weights:      49.78 [47.64, 51.25]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_12478 Boucle … Banwa    Solenzo Rural             2360        317    27.0 
#>  2 EA_12374 Boucle … Banwa    Solenzo Rural             1929        259    30.2 
#>  3 EA_11739 Boucle … Banwa    Sanaba  Rural              816        104    34.3 
#>  4 EA_14322 Boucle … Nayala   Ye      Rural             1281        184    25.9 
#>  5 EA_03138 Boucle … Mouhoun  Dedoug… Rural             1461        259    18.2 
#>  6 EA_11077 Boucle … Bale     Pompoi  Rural              917        124    26.8 
#>  7 EA_13839 Boucle … Sourou   Tougan  Rural             1934        265    24.2 
#>  8 EA_12957 Boucle … Mouhoun  Tcheri… Rural             1243        199     1.15
#>  9 EA_13862 Boucle … Sourou   Tougan  Rural              902        124    59.4 
#> 10 EA_02077 Boucle … Kossi    Bombor… Rural             1036        136     7.59
#> # ℹ 290 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>

# PPS cluster sampling
cluster_smpl <- sampling_design() |>
  cluster_by(ea_id) |>
  draw(n = 50, method = "pps_brewer", mos = households) |>
  execute(bfa_eas, seed = 123)

cluster_smpl
#> # A tbl_sample: 50 × 18
#> # Weights:      294.54 [152.25, 789.27]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_12435 Boucle … Banwa    Solenzo Rural              843        113     7.84
#>  2 EA_08689 Boucle … Kossi    Nouna   Rural             1011        149    15.9 
#>  3 EA_02099 Boucle … Mouhoun  Bondok… Rural             1341        205    16.6 
#>  4 EA_10299 Cascades Comoe    Ouo     Rural             1224        144    97.8 
#>  5 EA_08808 Centre   Kadiogo  Ouagad… Urban             1793        270     0.41
#>  6 EA_08848 Centre   Kadiogo  Ouagad… Urban             1844        278     0.57
#>  7 EA_08891 Centre   Kadiogo  Ouagad… Urban             1989        300     0.63
#>  8 EA_08943 Centre   Kadiogo  Ouagad… Urban             1782        268     1.24
#>  9 EA_09579 Centre   Kadiogo  Ouagad… Urban             1882        283     0.24
#> 10 EA_09730 Centre   Kadiogo  Ouagad… Urban             2022        305     0.24
#> # ℹ 40 more rows
#> # ℹ 10 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>, .certainty_1 <lgl>
```

## Multi-Stage Sampling

Use `add_stage()` to define multi-stage designs. This example selects
districts with PPS, then samples EAs within each:

``` r
library(dplyr, warn.conflicts = FALSE)
data(zwe_eas)

# Add district-level measure of size
zwe_frame <- zwe_eas |>
  mutate(district_hh = sum(households), .by = district)

# Two-stage design: 10 districts, 5 EAs per district
sample <- sampling_design() |>
  add_stage(label = "Districts") |>
    cluster_by(district) |>
    draw(n = 10, method = "pps_brewer", mos = district_hh) |>
  add_stage(label = "EAs") |>
    draw(n = 5) |>
  execute(zwe_frame, seed = 12345)

sample
#> # A tbl_sample: 50 × 16
#> # Weights:      472.68 [196.93, 541.06]
#>    ea_id    province   district urban_rural population households area_km2
#>  * <chr>    <fct>      <fct>    <fct>            <int>      <int>    <dbl>
#>  1 EA_00955 Harare     Harare   Urban             1379        407     0.52
#>  2 EA_01509 Harare     Harare   Urban             1417        377     0.22
#>  3 EA_00825 Harare     Harare   Urban             1273        374     0.72
#>  4 EA_00814 Harare     Harare   Urban             1332        400     0.5 
#>  5 EA_00777 Harare     Harare   Urban             1471        443     0.16
#>  6 EA_02797 Manicaland Chipinge Rural              673        152    12.4 
#>  7 EA_03044 Manicaland Chipinge Rural              769        180     9.57
#>  8 EA_02960 Manicaland Chipinge Rural              681        156     9.52
#>  9 EA_03159 Manicaland Chipinge Rural              366         83     9.62
#> 10 EA_03110 Manicaland Chipinge Urban             1011        283     3.78
#> # ℹ 40 more rows
#> # ℹ 9 more variables: district_hh <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_2 <dbl>, .fpc_2 <int>, .weight_1 <dbl>, .fpc_1 <int>,
#> #   .certainty_1 <lgl>
```

### Operational Sampling

Execute stages separately when fieldwork happens between stages:

``` r
design <- sampling_design() |>
  add_stage(label = "Districts") |>
    cluster_by(district) |>
    draw(n = 10, method = "pps_brewer", mos = district_hh) |>
  add_stage(label = "EAs") |>
    draw(n = 5)

# Add district-level measure of size
zwe_frame_agg <- zwe_eas |>
  summarize(district_hh = sum(households),
            m = n(),
            .by = district)

# Execute stage 1 only
selected_districts <- execute(design, zwe_frame_agg, stages = 1, seed = 1)

# ... fieldwork ...

zwe_frame <- zwe_eas |>
  mutate(district_hh = sum(households),
         m = n(),
         .by = district)

# Execute stage 2
final_sample <- selected_districts |> execute(zwe_frame, seed = 2)
```

## Selection Methods

### Equal Probability

| Method | Sample Size | Description |
|----|----|----|
| `srswor` | Fixed | Simple random sampling without replacement (default) |
| `srswr` | Fixed | Simple random sampling with replacement |
| `systematic` | Fixed | Systematic sampling |
| `bernoulli` | Random | Bernoulli sampling |

### Probability Proportional to Size (PPS)

| Method            | Sample Size | Description                       |
|-------------------|-------------|-----------------------------------|
| `pps_brewer`      | Fixed       | Brewer’s method (recommended)     |
| `pps_systematic`  | Fixed       | PPS systematic                    |
| `pps_cps`         | Fixed       | Conditional Poisson sampling      |
| `pps_poisson`     | Random      | PPS Poisson (PRN)                 |
| `pps_sps`         | Fixed       | Sequential Poisson sampling (PRN) |
| `pps_pareto`      | Fixed       | Pareto piPS sampling (PRN)        |
| `pps_multinomial` | Fixed       | PPS with replacement              |
| `pps_chromy`      | Fixed       | PPS with minimum replacement      |

### Balanced Sampling

| Method     | Sample Size | Description                                    |
|------------|-------------|------------------------------------------------|
| `balanced` | Fixed       | Cube method (Deville & Tille 2004), uses `aux` |

## Allocation Methods

When stratifying, control how the total sample is distributed:

| Method         | Description                                             |
|----------------|---------------------------------------------------------|
| (none)         | `n` applies per stratum                                 |
| `equal`        | Same sample size in each stratum                        |
| `proportional` | Proportional to stratum size                            |
| `neyman`       | Minimize variance (requires `variance`)                 |
| `optimal`      | Minimize cost-variance (requires `variance` and `cost`) |
| `power`        | Compromise allocation (requires `cv` and `importance`)  |

### Sample Size Bounds

Use `min_n` and `max_n` in `draw()` to constrain stratum sample sizes
when using allocation methods:

``` r
data(bfa_eas_variance)

# Ensure at least 2 per stratum (minimum for variance estimation)
sampling_design() |>
  stratify_by(region, alloc = "neyman", variance = bfa_eas_variance) |>
  draw(n = 300, min_n = 2) |>
  execute(bfa_eas, seed = 321)
#> # A tbl_sample: 300 × 17
#> # Weights:      49.78 [34.6, 76.88]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_13752 Boucle … Nayala   Toma    Rural             1235        178    17.3 
#>  2 EA_14328 Boucle … Nayala   Ye      Rural             1231        177    29.9 
#>  3 EA_11736 Boucle … Banwa    Sanaba  Rural             1074        137    25.2 
#>  4 EA_10182 Boucle … Mouhoun  Ouarko… Rural             1347        185    33.0 
#>  5 EA_06471 Boucle … Mouhoun  Kona    Rural             1148        159    16.6 
#>  6 EA_02527 Boucle … Kossi    Bouras… Rural             1268        143    42.5 
#>  7 EA_03143 Boucle … Mouhoun  Dedoug… Rural             1286        228    12.4 
#>  8 EA_12541 Boucle … Kossi    Sono    Rural             1296        196    23.6 
#>  9 EA_14303 Boucle … Nayala   Ye      Rural             1314        189     2.27
#> 10 EA_12935 Boucle … Mouhoun  Tcheri… Rural             1170        187     9.7 
#> # ℹ 290 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>
```

### Custom Allocation

For custom stratum-specific sizes or rates, pass a data frame to `n` or
`frac` in `draw()`:

``` r
# Custom allocation with data frame
sizes_df <- data.frame(
  region = c("North", "South", "East", "West"),
  n = c(100, 200, 150, 100)
)

sample <- sampling_design() |>
  stratify_by(region) |>
  draw(n = sizes_df) |>
  execute(frame, seed = 42)
```

``` r
# Neyman allocation
data(bfa_eas_variance)

sample <- sampling_design() |>
  stratify_by(region, alloc = "neyman", variance = bfa_eas_variance) |>
  draw(n = 300) |>
  execute(bfa_eas, seed = 4321)

sample
#> # A tbl_sample: 300 × 17
#> # Weights:      49.78 [34.6, 76.88]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_13823 Boucle … Sourou   Tougan  Rural             1349        185    17.6 
#>  2 EA_11131 Boucle … Bale     Poura   Urban             1287        207     0.95
#>  3 EA_12375 Boucle … Banwa    Solenzo Rural              528         71     8.98
#>  4 EA_02124 Boucle … Mouhoun  Bondok… Rural             1372        209    29.8 
#>  5 EA_03164 Boucle … Mouhoun  Dedoug… Rural              650        115    14.6 
#>  6 EA_00273 Boucle … Bale     Bagassi Rural             1459        175    16.3 
#>  7 EA_14031 Boucle … Nayala   Yaba    Rural             2013        266    30.6 
#>  8 EA_03760 Boucle … Kossi    Dokui   Rural             1017        126    24.4 
#>  9 EA_08696 Boucle … Kossi    Nouna   Rural             1470        217    89.2 
#> 10 EA_14036 Boucle … Nayala   Yaba    Rural             1299        171    24.7 
#> # ℹ 290 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>
```

## Sample Coordination (PRN)

Permanent random numbers enable coordinated sampling across survey
waves. Assign a stable uniform random number to each frame unit, then
pass it via `prn`:

``` r
frame$prn <- runif(nrow(frame))

# Wave 1: sequential Poisson sampling with PRN
wave1 <- sampling_design() |>
  draw(n = 500, method = "pps_sps", mos = size, prn = prn) |>
  execute(frame, seed = 10)

# Wave 2: same PRN -> high overlap (positive coordination)
wave2 <- sampling_design() |>
  draw(n = 500, method = "pps_sps", mos = size, prn = prn) |>
  execute(frame, seed = 20)
```

PRN is supported for `bernoulli`, `pps_poisson`, `pps_sps`, and
`pps_pareto`.

## Balanced Sampling

The cube method (Deville & Tille 2004) produces samples whose
Horvitz-Thompson estimates of auxiliary totals match the population
totals, improving precision:

``` r
# Balanced sample using population and area as auxiliary variables
balanced_smpl <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300, method = "balanced", mos = households,
       aux = c(population, area_km2)) |>
  execute(bfa_eas, seed = 24)

balanced_smpl
#> # A tbl_sample: 300 × 17
#> # Weights:      48.33 [7.35, 142.62]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_00246 Boucle … Bale     Bagassi Rural             2184        262     2.57
#>  2 EA_04217 Boucle … Bale     Fara    Rural             1285        188    18.7 
#>  3 EA_04224 Boucle … Bale     Fara    Rural             1040        152     7.17
#>  4 EA_11085 Boucle … Bale     Pompoi  Rural             1132        153    18.1 
#>  5 EA_11129 Boucle … Bale     Poura   Urban             1942        312     5.66
#>  6 EA_11135 Boucle … Bale     Poura   Urban             2473        397     3.09
#>  7 EA_14058 Boucle … Bale     Yaho    Rural             1508        225     7.74
#>  8 EA_14062 Boucle … Bale     Yaho    Rural              926        138    49.3 
#>  9 EA_00369 Boucle … Banwa    Balave  Rural             2049        323    36.5 
#> 10 EA_06873 Boucle … Banwa    Kouka   Rural             3546        488    12.2 
#> # ℹ 290 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <dbl>
```

The `aux` parameter accepts bare column names. When combined with `mos`,
inclusion probabilities are proportional to size while the sample
remains balanced on the auxiliary variables. Stratified designs use the
stratified cube algorithm (Chauvet 2009) in a single call. Balanced
sampling is supported for up to 2 stages.

## Survey Export

Convert samples to `survey` or `srvyr` objects for analysis:

``` r
svy <- as_svydesign(sample)
survey::svymean(~y, svy)

# Exact or high-entropy joint inclusion probabilities
jip <- joint_expectation(sample, frame, stage = 1)
svy_exact <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
```

By default `as_svydesign()` uses Brewer variance approximation. For
tighter variance estimates, `joint_expectation()` computes pairwise
joint inclusion probabilities from the original frame. The result is
exact for CPS, systematic, and Poisson whereas high-entropy
approximation O(N^2) is by Brewer, SPS, Pareto and Balanced sampling
using the Cube algorithm (exact recursive formulas exist for some of
these methods but are usually O(N^3) and impractical for large frames).
See `?joint_expectation` for the full method-by-method breakdown.

## Panel Partitioning

Partition the sample into rotation groups for panel surveys:

``` r
panel_sample <- sampling_design() |>
  stratify_by(region) |>
  draw(n = 200) |>
  execute(bfa_eas, seed = 1, panels = 4)

table(panel_sample$.panel)
#> 
#>   1   2   3   4 
#> 650 650 650 650
```

Panels are assigned by systematic interleaving within strata. For
multi-stage designs, panels are assigned at the PSU level and propagated
to all units. Per-panel analysis is also possible by multiplying the
weights by the number of panels since the weights reflect the
full-sample inclusion probability.

## Replicated Sampling

Draw multiple independent samples from the same frame and design with
`reps`. This is useful for simulation studies, variance estimation via
repeated sampling, or quality control:

``` r
rep_sample <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300) |>
  execute(bfa_eas, seed = 42, reps = 5)

table(rep_sample$.replicate)
#> 
#>   1   2   3   4   5 
#> 300 300 300 300 300
```

Each replicate is an independent draw. Replicate `r` uses seed
`seed + r - 1`. To analyse a single replicate, filter first:

``` r
rep1 <- rep_sample |> filter(.replicate == 1)
as_svydesign(rep1)
```

## Two-Phase Sampling

Pipe a `tbl_sample` into `execute()` for multi-phase designs:

``` r
# Phase 1: large screening sample
phase1 <- sampling_design() |>
  draw(n = 500) |>
  execute(bfa_eas, seed = 1)

# Phase 2: subsample from phase 1
phase2 <- sampling_design() |>
  draw(n = 50) |>
  execute(phase1, seed = 2)

phase2
#> # A tbl_sample: 50 × 17
#> # Weights:      298.68 [298.68, 298.68]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_11635 Nord     Passore  Samba   Rural              410         43     9.25
#>  2 EA_04168 Est      Gourma   Fada-N… Rural             1857        225    22.1 
#>  3 EA_02116 Boucle … Mouhoun  Bondok… Rural             1218        186    17.3 
#>  4 EA_07819 Est      Gnagna   Mani    Rural             1372        199    19.6 
#>  5 EA_10669 Hauts-B… Houet    Peni    Rural             1385        192    34.1 
#>  6 EA_09371 Centre   Kadiogo  Ouagad… Urban             1626        245     0.47
#>  7 EA_10872 Centre-… Sanmate… Pissila Rural              992        120    12.2 
#>  8 EA_02227 Est      Tapoa    Botou   Rural              861         94    11.7 
#>  9 EA_04822 Sahel    Seno     Gorgad… Rural              948        152    27.6 
#> 10 EA_00741 Boucle … Kossi    Barani  Rural             1016        133     8.38
#> # ℹ 40 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>
```

Weights compound automatically across phases.

## Diagnostics

``` r
summary(strata_smpl)
#> ── Sample Summary ──────────────────────────────────────────────────────────
#> 
#> ℹ n = 300 | stages = 1/1 | seed = 12
#> 
#> ── Design: Stage 1 ─────────────────────────────────────────────────────────
#> • Strata: region (proportional)
#> • Method: srswor
#> 
#> ── Allocation: Stage 1 ─────────────────────────────────────────────────────
#>   region             N_h    n_h  f_h   
#>   Boucle du Mouhoun  1510   30   0.0199
#>   Cascades           667    14   0.0210
#>   Centre             1556   31   0.0199
#>   Centre-Est         1259   25   0.0199
#>   Centre-Nord        1375   28   0.0204
#>   Centre-Ouest       1287   26   0.0202
#>   Centre-Sud         615    12   0.0195
#>   Est                1590   32   0.0201
#>   Hauts-Bassins      1483   30   0.0202
#>   Nord               1211   24   0.0198
#>   Plateau-Central    757    15   0.0198
#>   Sahel              902    18   0.0200
#>   Sud-Ouest          722    15   0.0208
#>                      ─────  ───  ──────
#>   Total              14934  300  0.0201
#> 
#> ── Weights ─────────────────────────────────────────────────────────────────
#> • Range: [47.64, 51.25]
#> • Mean:  49.78 · CV: 0.02
#> • DEFF:  1 · n_eff: 300
```

## Validation

For statistical validation on synthetic populations with known truths,
see `vignette("validation")`. It combines deterministic invariants
(weights, FPC, certainty, stage compounding) with Monte Carlo checks of
bias, standard-error calibration, and coverage.

## Included Datasets

Synthetic sampling frames for learning and testing:

| Dataset | Description | Rows |
|----|----|----|
| `bfa_eas` | LSMS-style EA frame (Burkina Faso) | 14,934 |
| `zwe_eas` | DHS-style EA frame (Zimbabwe) | 22,600 |
| `zwe_households` | Household frame for two-phase sampling (Zimbabwe) | 379,326 |
| `ken_enterprises` | Enterprise survey frame (Kenya) | 6,823 |

Plus auxiliary data: `bfa_eas_variance`, `bfa_eas_cost`

## Comparison with SAS and SPSS

### SAS PROC SURVEYSELECT

``` sas
proc surveyselect data=frame method=pps n=50 seed=12345;
  strata region;
  cluster school;
  size enrollment;
run;
```

``` r
sampling_design() |>
  stratify_by(region) |>
  cluster_by(school) |>
  draw(n = 50, method = "pps_brewer", mos = enrollment) |>
  execute(frame, seed = 1)
```

### SAS Allocation with Bounds

``` sas
proc surveyselect data=frame method=srs n=500 seed=42;
  strata region / alloc=neyman var=variance_data allocmin=2 allocmax=100;
run;
```

``` r
sampling_design() |>
  stratify_by(region, alloc = "neyman", variance = variance_data) |>
  draw(n = 500, min_n = 2, max_n = 100) |>
  execute(frame, seed = 2)
```

### SAS Rounding Control

``` sas
proc surveyselect data=frame method=sys samprate=0.02 seed=2 round=nearest;
  strata State;
run;
```

``` r
sampling_design() |>
  stratify_by(State) |>
  draw(frac = 0.02, method = "systematic", round = "nearest") |>
  execute(frame, seed = 3)
```

### SPSS CSPLAN

``` spss
CSPLAN SAMPLE
  /PLAN FILE='myplan.csplan'
  /DESIGN STRATA=region CLUSTER=school
  /METHOD TYPE=PPS_WOR
  /SIZE VALUE=50
  /MOS VARIABLE=enrollment.
```

``` r
sampling_design() |>
  stratify_by(region) |>
  cluster_by(school) |>
  draw(n = 50, method = "pps_brewer", mos = enrollment) |>
  execute(frame, seed = 4)
```
