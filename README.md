
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
#> # Weights:      89.14 [87.47, 90.09]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1  9648 Boucle du Mo… Banwa    Sanaba  Rural              279         35     8.37 TRUE               14.5
#>  2 11547 Boucle du Mo… Sourou   Toeni   Rural               49          5    21.4  TRUE                8.3
#>  3 41824 Boucle du Mo… Kossi    Dokui   Rural               75          9    15.8  TRUE                4.1
#>  4 11012 Boucle du Mo… Banwa    Tansila Rural              592         71     0.95 TRUE               16.2
#>  5 32308 Boucle du Mo… Sourou   Lanfie… Rural               57          8     9.49 TRUE                9.4
#>  6  7017 Boucle du Mo… Kossi    Madouba Rural              599         74     0.74 TRUE                4  
#>  7 36700 Boucle du Mo… Bale     Fara    Rural              402         59     6.36 TRUE                4.6
#>  8 11611 Boucle du Mo… Nayala   Yaba    Rural              111         15     8.97 TRUE               10.2
#>  9  8342 Boucle du Mo… Mouhoun  Ouarko… Rural               94         13     8.1  TRUE               45  
#> 10 11626 Boucle du Mo… Nayala   Yaba    Rural               56          7     8.31 TRUE               52.1
#> # ℹ 490 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
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
#> ── Sampling Design: Gambia bed nets ─────────────────────────────────────────────────────────────────
#> 
#> ℹ 3 stages
#> 
#> ── Stage 1 ──────────────────────────────────────────────────────────────────────────────────────────
#> • Strata: region
#> • Cluster: district
#> • Draw: n = 5, method = pps_brewer, mos = population
#> 
#> ── Stage 2 ──────────────────────────────────────────────────────────────────────────────────────────
#> • Strata: phc
#> • Cluster: village
#> • Draw: n = 2, method = pps_brewer, mos = population
#> 
#> ── Stage 3 ──────────────────────────────────────────────────────────────────────────────────────────
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
#> # Weights:      148.57 [146.5, 151.22]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 29513 Boucle du Mo… Bale     Bana    Rural              181         24     4.89 TRUE                9  
#>  2 29527 Boucle du Mo… Bale     Bana    Rural              193         25     0.14 TRUE                5.8
#>  3 36703 Boucle du Mo… Bale     Fara    Rural              975        143     1.31 TRUE               13.8
#>  4  8455 Boucle du Mo… Bale     Ouri    Rural               57          7     5.95 TRUE                5  
#>  5  8580 Boucle du Mo… Bale     Pa      Rural              171         25     5.12 TRUE                6.1
#>  6 11739 Boucle du Mo… Bale     Yaho    Rural              509         76     8.8  TRUE               23.4
#>  7 11746 Boucle du Mo… Bale     Yaho    Rural               98         15     8.72 FALSE              63.4
#>  8  6291 Boucle du Mo… Banwa    Kouka   Rural              563         78     7.96 TRUE                9.4
#>  9 34031 Boucle du Mo… Banwa    Sami    Rural               41          6    20.2  TRUE                7.5
#> 10 34058 Boucle du Mo… Banwa    Sami    Rural              516         78     7.6  TRUE               36.1
#> # ℹ 290 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
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
#> # Weights:      445.7 [445.7, 445.7]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1  5168 Centre-Nord   Sanmate… Kaya    Urban              794        117     0.18 TRUE                0.2
#>  2 11281 Centre-Sud    Nahouri  Tiebele Rural              123         19     8.83 TRUE               25.7
#>  3 30149 Centre-Ouest  Sanguie  Dassa   Rural              813        127     7.52 TRUE                3.1
#>  4 13590 Est           Gnagna   Koala   Rural              353         44     6.04 TRUE               45  
#>  5 26045 Boucle du Mo… Mouhoun  Dedoug… Rural              378         67     8.36 FALSE              41  
#>  6 44140 Centre-Est    Koulpel… Soudou… Rural              258         39     8.95 TRUE                7.6
#>  7 30931 Est           Komandj… Gayeri  Rural              150         19     8.8  TRUE               23.5
#>  8 10745 Sahel         Yagha    Tankou… Rural              312         43    17.0  TRUE                3.4
#>  9 13990 Hauts-Bassins Kenedou… Kourou… Rural              406         64     8.98 TRUE               12  
#> 10 27757 Est           Tapoa    Partia… Rural              571         78     8.82 TRUE                6.5
#> # ℹ 90 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>

# Stratified proportional allocation
strata_smpl <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300) |>
  execute(bfa_eas, seed = 12)

strata_smpl
#> # A tbl_sample: 300 × 17
#> # Weights:      148.57 [146.5, 151.22]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 31987 Boucle du Mo… Sourou   Kiemba… Rural               24          3     7.24 TRUE                6.3
#>  2  4968 Boucle du Mo… Sourou   Kassoum Rural              303         45     4.13 TRUE               24.7
#>  3  4958 Boucle du Mo… Sourou   Kassoum Rural              142         21     8.24 TRUE                4.3
#>  4 23903 Boucle du Mo… Banwa    Solenzo Rural               94         13    13.5  FALSE              12  
#>  5 25996 Boucle du Mo… Mouhoun  Dedoug… Rural              538         95     8.63 TRUE               24.4
#>  6 21501 Boucle du Mo… Kossi    Doumba… Rural              247         30     7.93 TRUE                6.2
#>  7 43794 Boucle du Mo… Mouhoun  Safane  Rural               87         12     8.96 TRUE               14.6
#>  8  9724 Boucle du Mo… Banwa    Sanaba  Rural               51          6     8.02 FALSE               6.4
#>  9 10979 Boucle du Mo… Banwa    Tansila Rural               71          9     7.72 FALSE              24.1
#> 10  8893 Boucle du Mo… Bale     Pompoi  Rural              522         71     0.48 TRUE               21.9
#> # ℹ 290 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>

# PPS cluster sampling
cluster_smpl <- sampling_design() |>
  cluster_by(ea_id) |>
  draw(n = 50, method = "pps_brewer", mos = households) |>
  execute(bfa_eas, seed = 123)

cluster_smpl
#> # A tbl_sample: 50 × 18
#> # Weights:      812.6 [113.61, 3998.97]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 23926 Boucle du Mo… Banwa    Solenzo Rural              843        113     7.84 TRUE                9.7
#>  2 33198 Boucle du Mo… Kossi    Nouna   Rural             1011        149    15.9  TRUE                8.8
#>  3 20930 Boucle du Mo… Mouhoun  Bondok… Rural              792        121     9.76 TRUE               25.9
#>  4 23139 Cascades      Comoe    Ouo     Rural              275         32     7.57 TRUE               34  
#>  5 15175 Centre        Kadiogo  Ouagad… Urban             1287        194     0.2  TRUE                0.8
#>  6 15267 Centre        Kadiogo  Ouagad… Urban             3506        528     0.88 TRUE                0.8
#>  7 15367 Centre        Kadiogo  Ouagad… Urban              993        150     0.32 TRUE                1.3
#>  8 15491 Centre        Kadiogo  Ouagad… Urban              854        129     0.11 TRUE                0.2
#>  9 17031 Centre        Kadiogo  Ouagad… Urban              802        121     1.93 TRUE                0.3
#> 10 17414 Centre        Kadiogo  Ouagad… Urban              874        132     0.12 TRUE                0.1
#> # ℹ 40 more rows
#> # ℹ 8 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
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
#> # A tbl_sample: 50 × 21
#> # Weights:      2514.13 [784.4, 4121.21]
#>    ea_id province   district  ward_pcode urban_rural population households buildings women_15_49 men_15_49
#>  * <int> <fct>      <fct>     <chr>      <fct>            <int>      <int>     <int>       <int>     <int>
#>  1 88821 Harare     Harare    ZW192102   Urban              197         57       115          63        54
#>  2 87420 Harare     Harare    ZW192116   Urban             1378        353       563         441       380
#>  3 86941 Harare     Harare    ZW192108   Urban              103         38       129          33        28
#>  4 88444 Harare     Harare    ZW192129   Urban              538        142        96         173       149
#>  5 34696 Harare     Harare    ZW192120   Urban              452        124       109         145       125
#>  6 14864 Manicaland Chimanim… ZW110201   Rural              111         29        78          27        24
#>  7 59668 Manicaland Chimanim… ZW110204   Rural              142         36        86          35        30
#>  8 60166 Manicaland Chimanim… ZW110203   Rural              114         29        88          29        24
#>  9 60082 Manicaland Chimanim… ZW110216   Rural               73         19        50          19        16
#> 10 58991 Manicaland Chimanim… ZW110219   Rural               73         18        79          19        15
#> # ℹ 40 more rows
#> # ℹ 11 more variables: children_under5 <int>, area_km2 <dbl>, district_hh <int>, .weight <dbl>,
#> #   .sample_id <int>, .stage <int>, .weight_2 <dbl>, .fpc_2 <int>, .weight_1 <dbl>, .fpc_1 <int>,
#> #   .certainty_1 <lgl>
```

### Operational Sampling

Execute stages separately when fieldwork happens between stages:

``` r
design <- sampling_design() |>
  add_stage(label = "EA") |>
    stratify_by(urban_rural) |>
    cluster_by(ea_id) |>
    draw(n = 10, method = "pps_brewer", mos = households) |>
  add_stage(label = "HH") |>
    draw(n = 5)

# Execute stage 1 only
selected_eas <- execute(design, zwe_eas, stages = 1, seed = 1)
selected_eas
#> # A tbl_sample: 20 × 18
#> # Stages:       1/2
#> # Weights:      6492.06 [591.31, 14492.74]
#>    ea_id province    district ward_pcode urban_rural population households buildings women_15_49 men_15_49
#>  * <int> <fct>       <fct>    <chr>      <fct>            <int>      <int>     <int>       <int>     <int>
#>  1 47209 Bulawayo    Bulawayo ZW102127   Urban              462        121       122         147       113
#>  2 35161 Harare      Harare   ZW192109   Urban              140         38        91          45        39
#>  3 86782 Harare      Harare   ZW192130   Urban              974        263       170         312       269
#>  4 88462 Harare      Harare   ZW192103   Urban              993        302        98         318       275
#>  5 93947 Mashonalan… Bindura  ZW120105   Rural               95         23        91          23        22
#>  6 35770 Mashonalan… Guruve   ZW120307   Urban              121         32       122          29        26
#>  7 35831 Mashonalan… Guruve   ZW120322   Rural              322         80       210          79        71
#>  8 83626 Mashonalan… Goromon… ZW130225   Urban              277         72       105          74        68
#>  9 37885 Mashonalan… Mudzi    ZW130514   Urban              294         80       117          69        57
#> 10 83520 Mashonalan… Murehwa  ZW130627   Rural               55         15        71          14        11
#> 11 20895 Mashonalan… Mutoko   ZW130726   Rural               96         23        96          23        20
#> 12 36189 Mashonalan… Makonde  ZW140513   Rural               94         21        77          22        22
#> 13 42748 Masvingo    Mwenezi  ZW180610   Rural               73         15        86          18        12
#> 14 76867 Matabelela… Binga    ZW150107   Rural               74         20        64          19        13
#> 15  5602 Matabelela… Tsholot… ZW150603   Rural               62         14        85          14        10
#> 16 61444 Matabelela… Tsholot… ZW150605   Rural               75         18        97          17        12
#> 17 28253 Matabelela… Gwanda … ZW162103   Urban              106         35        63          36        30
#> 18 51722 Matabelela… Umzingw… ZW160701   Urban              357         81       291          84        80
#> 19 54466 Midlands    Gweru U… ZW172114   Urban              510        136       121         171       136
#> 20   866 Midlands    Zvishav… ZW170818   Rural               91         19       107          22        18
#> # ℹ 8 more variables: children_under5 <int>, area_km2 <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>, .certainty_1 <lgl>

# ... fieldwork ...
# list all households in selected EAs
selected_eas_list <- selected_eas |>
  slice(rep(seq_len(n()), households)) |>
  mutate(hh_id = row_number())

# Execute stage 2
final_sample <- design |>
  execute(selected_eas_list, seed = 2)
final_sample
#> # A tbl_sample: 100 × 21
#> # Weights:      38147.25 [35714.84, 40579.66]
#>    ea_id province district ward_pcode urban_rural population households buildings women_15_49 men_15_49
#>  * <int> <fct>    <fct>    <chr>      <fct>            <int>      <int>     <int>       <int>     <int>
#>  1 47209 Bulawayo Bulawayo ZW102127   Urban              462        121       122         147       113
#>  2 47209 Bulawayo Bulawayo ZW102127   Urban              462        121       122         147       113
#>  3 47209 Bulawayo Bulawayo ZW102127   Urban              462        121       122         147       113
#>  4 47209 Bulawayo Bulawayo ZW102127   Urban              462        121       122         147       113
#>  5 47209 Bulawayo Bulawayo ZW102127   Urban              462        121       122         147       113
#>  6 35161 Harare   Harare   ZW192109   Urban              140         38        91          45        39
#>  7 35161 Harare   Harare   ZW192109   Urban              140         38        91          45        39
#>  8 35161 Harare   Harare   ZW192109   Urban              140         38        91          45        39
#>  9 35161 Harare   Harare   ZW192109   Urban              140         38        91          45        39
#> 10 35161 Harare   Harare   ZW192109   Urban              140         38        91          45        39
#> # ℹ 90 more rows
#> # ℹ 11 more variables: children_under5 <int>, area_km2 <dbl>, hh_id <int>, .weight <dbl>,
#> #   .sample_id <int>, .stage <int>, .weight_2 <dbl>, .fpc_2 <int>, .weight_1 <dbl>, .fpc_1 <int>,
#> #   .certainty_1 <lgl>
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
#> # Weights:      148.57 [56.35, 1209.75]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 11643 Boucle du Mo… Nayala   Yaba    Rural              396         52     8.75 TRUE               10.3
#>  2 36763 Boucle du Mo… Bale     Fara    Rural               79         12     2.36 TRUE               23.5
#>  3  9648 Boucle du Mo… Banwa    Sanaba  Rural              279         35     8.37 TRUE               14.5
#>  4 10555 Boucle du Mo… Kossi    Sono    Rural              164         25     8.38 TRUE                4.3
#>  5 34926 Boucle du Mo… Sourou   Tougan  Rural             1239        170     1.15 TRUE                5.2
#>  6 44501 Boucle du Mo… Mouhoun  Tcheri… Rural              221         35     0.22 TRUE                9.1
#>  7  9702 Boucle du Mo… Banwa    Sanaba  Rural              260         33     6.92 FALSE               4.7
#>  8 26045 Boucle du Mo… Mouhoun  Dedoug… Rural              378         67     8.36 FALSE              41  
#>  9  8605 Boucle du Mo… Bale     Pa      Rural              283         41     8.78 FALSE              16.4
#> 10 21078 Boucle du Mo… Kossi    Bouras… Rural               93         11     7.08 TRUE               13.3
#> # ℹ 290 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
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
#> # Weights:      148.57 [56.35, 1209.75]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 21517 Boucle du Mo… Kossi    Doumba… Rural              354         43     8.72 TRUE                8.8
#>  2 33112 Boucle du Mo… Kossi    Nouna   Rural               28          4     5.47 TRUE                6.6
#>  3 25963 Boucle du Mo… Mouhoun  Dedoug… Rural              416         74     0.67 TRUE               19  
#>  4  6354 Boucle du Mo… Banwa    Kouka   Rural              296         41     8.95 FALSE              36.1
#>  5 11787 Boucle du Mo… Bale     Bagassi Rural              874        105     1.4  TRUE                5.4
#>  6  8233 Boucle du Mo… Mouhoun  Ouarko… Rural              183         25     9.54 TRUE                4  
#>  7  9925 Boucle du Mo… Bale     Siby    Rural              204         30     0.36 TRUE                8.2
#>  8 34812 Boucle du Mo… Sourou   Tougan  Rural             1072        147     0.3  TRUE                3.5
#>  9 11441 Boucle du Mo… Sourou   Toeni   Rural               64          7    23.9  TRUE               35.2
#> 10  8883 Boucle du Mo… Bale     Pompoi  Rural              192         26     9.42 TRUE                5.1
#> # ℹ 290 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
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
#> # Weights:      144.02 [13.68, 2034.38]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 11819 Boucle du Mo… Bale     Bagassi Rural              386         46     0.57 TRUE                3.4
#>  2  1136 Boucle du Mo… Bale     Boromo  Rural             3613        471     4.23 FALSE              10.6
#>  3  1205 Boucle du Mo… Bale     Boromo  Rural             4324        564     3.13 TRUE                6.4
#>  4  8453 Boucle du Mo… Bale     Ouri    Rural              809        103     0.62 TRUE               35.2
#>  5 39991 Boucle du Mo… Banwa    Balave  Rural              865        136     0.81 TRUE                7.4
#>  6 39992 Boucle du Mo… Banwa    Balave  Rural              947        149     1.05 FALSE               6.7
#>  7  6360 Boucle du Mo… Banwa    Kouka   Rural              592         82     0.54 TRUE               21.9
#>  8 23749 Boucle du Mo… Banwa    Solenzo Rural              235         32     8.79 TRUE                7.1
#>  9 23855 Boucle du Mo… Banwa    Solenzo Rural              831        112     6.92 TRUE               18.6
#> 10 24020 Boucle du Mo… Banwa    Solenzo Rural              466         63     8.71 TRUE               15.3
#> # ℹ 290 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <dbl>
```

The `aux` parameter accepts bare column names. When combined with `mos`,
inclusion probabilities are proportional to size while the sample
remains balanced on the auxiliary variables. Stratified designs use the
stratified cube algorithm (Chauvet 2009) in a single call. Balanced
sampling is supported for up to 2 stages.

## Custom Sampling Methods

Any unequal probability sampling method can be plugged into `samplyr` by
registering it with `sondage::register_method()`. The registered method
is then available in `draw()` using the `pps_<name>` convention.

For example, to use the elimination procedure of Tille (1996) from the
sampling package:

``` r
# Wrap sampling::UPtille to return selected indices
tille_fn <- function(pik, n = NULL, prn = NULL, ...) {
  which(as.logical(sampling::UPtille(pik)))
}

# Joint inclusion probabilities, restricted to the sampled units
tille_joint_fn <- function(pik, sample_idx = NULL, ...) {
  pi2 <- sampling::UPtillepi2(pik)
  if (!is.null(sample_idx))
    pi2 <- pi2[sample_idx, sample_idx, drop = FALSE]
  pi2
}

# Register with both sampling and joint probability functions
sondage::register_method(
  "tille", type = "wor",
  sample_fn = tille_fn,
  joint_fn = tille_joint_fn
)

# Use it like any built-in method
sample_tille <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300, method = "pps_tille", mos = households) |>
  execute(bfa_eas, seed = 1)

sample_tille
#> # A tbl_sample: 300 × 18
#> # Weights:      146.13 [13.68, 1719.7]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 29508 Boucle du Mo… Bale     Bana    Rural              998        130     2.14 TRUE               31  
#>  2  1205 Boucle du Mo… Bale     Boromo  Rural             4324        564     3.13 TRUE                6.4
#>  3 36751 Boucle du Mo… Bale     Fara    Rural              581         85     8.69 TRUE               22.4
#>  4  8609 Boucle du Mo… Bale     Pa      Rural              778        112     0.88 TRUE                3.6
#>  5  9895 Boucle du Mo… Bale     Siby    Rural             1813        268     1.7  TRUE               47.7
#>  6 11739 Boucle du Mo… Bale     Yaho    Rural              509         76     8.8  TRUE               23.4
#>  7 39999 Boucle du Mo… Banwa    Balave  Rural             1820        287     2.04 TRUE                4.6
#>  8  6256 Boucle du Mo… Banwa    Kouka   Rural              493         68     8.55 TRUE                6.8
#>  9  6304 Boucle du Mo… Banwa    Kouka   Rural              540         74     8.06 TRUE               13.5
#> 10  6366 Boucle du Mo… Banwa    Kouka   Rural              717         99     0.5  TRUE                9.7
#> # ℹ 290 more rows
#> # ℹ 8 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>, .certainty_1 <lgl>

# Clean up
sondage::unregister_method("tille")
```

Custom methods flow through the full pipeline: stratification,
multi-stage designs, certainty selection, survey export, and joint
probabilities. Providing a `joint_fn` enables exact variance estimation
via `joint_expectation()` and `survey::ppsmat()`. See
`?sondage::register_method` for the full contract.

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
#> # Weights:      891.4 [891.4, 891.4]
#>    ea_id region        province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <int> <fct>         <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 33282 Boucle du Mo… Kossi    Nouna   Rural              327         48     4.35 TRUE                3.8
#>  2 25236 Est           Gnagna   Bilanga Rural              581         88     8.72 FALSE               4.7
#>  3 33467 Est           Kompien… Pama    Rural               46          6    34.1  FALSE               3  
#>  4 19744 Centre-Est    Boulgou  Zabre   Rural              433         79     8.46 TRUE                6.9
#>  5 16081 Centre        Kadiogo  Ouagad… Urban              830        125     0.11 TRUE                0.4
#>  6 20688 Cascades      Comoe    Banfora Rural              823        101     0.18 TRUE               24.1
#>  7 36869 Boucle du Mo… Nayala   Gassan  Rural             1172        129     1.18 FALSE              43.8
#>  8 32034 Centre-Est    Koulpel… Komin-… Rural              444         62     7.97 TRUE                8.7
#>  9 15189 Centre        Kadiogo  Ouagad… Urban              471         71     0.08 TRUE                1.5
#> 10 11082 Sud-Ouest     Bougour… Tianko… Rural               43          6    10.1  TRUE               30.3
#> # ℹ 40 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>
```

Weights compound automatically across phases.

## Diagnostics

``` r
summary(strata_smpl)
#> ── Sample Summary ───────────────────────────────────────────────────────────────────────────────────
#> 
#> ℹ n = 300 | stages = 1/1 | seed = 12
#> 
#> ── Design: Stage 1 ──────────────────────────────────────────────────────────────────────────────────
#> • Strata: region (proportional)
#> • Method: srswor
#> 
#> ── Allocation: Stage 1 ──────────────────────────────────────────────────────────────────────────────
#>   region             N_h    n_h  f_h   
#>   Boucle du Mouhoun  5009   34   0.0068
#>   Cascades           2508   17   0.0068
#>   Centre             3888   26   0.0067
#>   Centre-Est         2941   20   0.0068
#>   Centre-Nord        3402   23   0.0068
#>   Centre-Ouest       3723   25   0.0067
#>   Centre-Sud         1612   11   0.0068
#>   Est                5505   37   0.0067
#>   Hauts-Bassins      4839   32   0.0066
#>   Nord               2930   20   0.0068
#>   Plateau-Central    1662   11   0.0066
#>   Sahel              4144   28   0.0068
#>   Sud-Ouest          2407   16   0.0066
#>                      ─────  ───  ──────
#>   Total              44570  300  0.0067
#> 
#> ── Weights ──────────────────────────────────────────────────────────────────────────────────────────
#> • Range: [146.5, 151.22]
#> • Mean:  148.57 · CV: 0.01
#> • DEFF:  1 · n_eff: 300
```

## Validation

For statistical validation on synthetic populations with known truths,
see `vignette("validation")`. It combines deterministic invariants
(weights, FPC, certainty, stage compounding) with Monte Carlo checks of
bias, standard-error calibration, and coverage.

## Included Datasets

Synthetic sampling frames for learning and testing:

| Dataset           | Description                                  | Rows    |
|-------------------|----------------------------------------------|---------|
| `bfa_eas`         | Household survey EA frame (Burkina Faso)     | 44,570  |
| `zwe_eas`         | Two-stage cluster survey EA frame (Zimbabwe) | 107,250 |
| `ken_enterprises` | Enterprise survey frame (Kenya)              | 6,823   |

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
