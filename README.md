
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
#> # A tbl_sample: 500 × 18
#> # Sampling:     1 stage | 500/44,570 units
#> # Weights:      89.14 [87.47, 90.09]
#>    ea_id region province commune urban_rural population households area_km2 pop_density longitude latitude
#>  * <int> <fct>  <fct>    <fct>   <fct>            <int>      <int>    <dbl>       <dbl>     <dbl>    <dbl>
#>  1  9648 Boucl… Banwa    Sanaba  Rural              279         35     8.37        33.3     -3.79     12.4
#>  2 11547 Boucl… Sourou   Toéni   Rural               49          7    21.4          2.3     -3.11     13.5
#>  3 41824 Boucl… Kossi    Dokui   Rural               75         10    15.8          4.8     -3.93     12.6
#>  4 11012 Boucl… Banwa    Tansila Rural              592         71     0.95       621       -4.33     12.4
#>  5 32308 Boucl… Sourou   Lanfiè… Rural               57          8     9.49         6       -3.35     12.9
#>  6  7017 Boucl… Kossi    Madouba Rural              599         74     0.74       807.      -4.27     13.1
#>  7 36700 Boucl… Bale     Fara    Rural              402         59     6.36        63.2     -2.79     11.4
#>  8 11611 Boucl… Nayala   Yaba    Rural              111         15     8.97        12.4     -2.83     12.8
#>  9  8342 Boucl… Mouhoun  Ouarko… Rural               94         13     8.1         11.6     -3.80     12.2
#> 10 11626 Boucl… Nayala   Yaba    Rural               56          7     8.31         6.7     -2.97     12.9
#> # ℹ 490 more rows
#> # ℹ 7 more variables: remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <dbl>
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
#> • Draw: n = 5 (per stratum), method = pps_brewer, mos = population
#> 
#> ── Stage 2 ──────────────────────────────────────────────────────────────────────────────────────────
#> • Strata: phc
#> • Cluster: village
#> • Draw: n = 2 (per stratum), method = pps_brewer, mos = population
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
#> # A tbl_sample: 300 × 18
#> # Weights:      148.57 [146.5, 151.22]
#>    ea_id region province commune urban_rural population households area_km2 pop_density longitude latitude
#>  * <int> <fct>  <fct>    <fct>   <fct>            <int>      <int>    <dbl>       <dbl>     <dbl>    <dbl>
#>  1 29513 Boucl… Bale     Bana    Rural              181         24     4.89        37       -3.44     12.0
#>  2 29527 Boucl… Bale     Bana    Rural              193         25     0.14      1397.      -3.39     11.9
#>  3 36703 Boucl… Bale     Fara    Rural              975        143     1.31       742       -2.73     11.5
#>  4  8455 Boucl… Bale     Ouri    Rural               57          8     5.95         9.6     -3.01     11.9
#>  5  8580 Boucl… Bale     Pâ      Rural              171         20     5.12        33.4     -3.16     11.5
#>  6 11739 Boucl… Bale     Yaho    Rural              509         76     8.8         57.8     -3.44     11.9
#>  7 11746 Boucl… Bale     Yaho    Rural               98         15     8.72        11.2     -3.48     11.9
#>  8  6291 Boucl… Banwa    Kouka   Rural              563         65     7.96        70.7     -4.41     11.8
#>  9 34031 Boucl… Banwa    Sami    Rural               41          6    20.2          2       -4.34     12.2
#> 10 34058 Boucl… Banwa    Sami    Rural              516         78     7.6         67.9     -4.62     12.1
#> # ℹ 290 more rows
#> # ℹ 7 more variables: remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <dbl>
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
#> # A tbl_sample: 100 × 18
#> # Sampling:     1 stage | 100/44,570 units
#> # Weights:      445.7 [445.7, 445.7]
#>    ea_id region province commune urban_rural population households area_km2 pop_density longitude latitude
#>  * <int> <fct>  <fct>    <fct>   <fct>            <int>      <int>    <dbl>       <dbl>     <dbl>    <dbl>
#>  1  5168 Centr… Sanmate… Kaya    Urban              794        129     0.18      4392.    -1.10       13.1
#>  2 11281 Centr… Nahouri  Tiébélé Rural              123         20     8.83        13.9   -0.899      11.1
#>  3 30149 Centr… Sanguie  Dassa   Rural              813        127     7.52       108.    -2.62       12.4
#>  4 13590 Est    Gnagna   Koala   Rural              353         44     6.04        58.4   -0.0646     13.5
#>  5 26045 Boucl… Mouhoun  Dédoug… Rural              378         54     8.36        45.2   -3.66       12.3
#>  6 44140 Centr… Koulpel… Soudou… Rural              258         43     8.95        28.8    0.290      11.4
#>  7 30931 Est    Komandj… Gayéri  Rural              150         16     8.8         17      0.341      12.6
#>  8 10745 Sahel  Yagha    Tankou… Rural              312         43    17.0         18.4    0.623      13.6
#>  9 13990 Hauts… Kenedou… Kourou… Rural              406         48     8.98        45.2   -4.56       11.5
#> 10 27757 Est    Tapoa    Partia… Rural              571         75     8.82        64.7    1.62       12.1
#> # ℹ 90 more rows
#> # ℹ 7 more variables: remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>

# Stratified proportional allocation
strata_smpl <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300) |>
  execute(bfa_eas, seed = 12)

strata_smpl
#> # A tbl_sample: 300 × 18
#> # Sampling:     1 stage | 300/44,570 units
#> # Weights:      148.57 [146.5, 151.22]
#>    ea_id region province commune urban_rural population households area_km2 pop_density longitude latitude
#>  * <int> <fct>  <fct>    <fct>   <fct>            <int>      <int>    <dbl>       <dbl>     <dbl>    <dbl>
#>  1 31987 Boucl… Sourou   Kiemba… Rural               24          3     7.24         3.3     -2.71     13.2
#>  2  4968 Boucl… Sourou   Kassoum Rural              303         41     4.13        73.3     -3.39     13.0
#>  3  4958 Boucl… Sourou   Kassoum Rural              142         19     8.24        17.2     -3.28     13.1
#>  4 23903 Boucl… Banwa    Solenzo Rural               94         13    13.5          7       -4.14     12.3
#>  5 25996 Boucl… Mouhoun  Dédoug… Rural              538         76     8.63        62.4     -3.64     12.4
#>  6 21501 Boucl… Kossi    Doumba… Rural              247         42     7.93        31.2     -4.10     12.9
#>  7 43794 Boucl… Mouhoun  Safané  Rural               87         12     8.96         9.7     -3.26     12.3
#>  8  9724 Boucl… Banwa    Sanaba  Rural               51          6     8.02         6.4     -3.76     12.5
#>  9 10979 Boucl… Banwa    Tansila Rural               71          9     7.72         9.2     -4.32     12.4
#> 10  8893 Boucl… Bale     Pompoï  Rural              522         77     0.48      1094.      -3.29     11.9
#> # ℹ 290 more rows
#> # ℹ 7 more variables: remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <dbl>

# PPS cluster sampling
cluster_smpl <- sampling_design() |>
  cluster_by(ea_id) |>
  draw(n = 50, method = "pps_brewer", mos = households) |>
  execute(bfa_eas, seed = 123)

cluster_smpl
#> # A tbl_sample: 50 × 19
#> # Weights:      779.3 [188.64, 2999.41]
#>    ea_id region province commune urban_rural population households area_km2 pop_density longitude latitude
#>  * <int> <fct>  <fct>    <fct>   <fct>            <int>      <int>    <dbl>       <dbl>     <dbl>    <dbl>
#>  1 23944 Boucl… Banwa    Solenzo Rural              758        102     0.77        985.     -4.11     12.3
#>  2 33134 Boucl… Kossi    Nouna   Rural             1116        145     1.35        826.     -4.00     12.8
#>  3 10578 Boucl… Kossi    Sônô    Rural              326         49     0.41        790.     -3.49     12.8
#>  4 23289 Casca… Comoe    Ouô     Rural              534         70     8.62         62      -3.74     10.6
#>  5 15240 Centre Kadiogo  Ouagad… Urban              614         92     0.14       4509.     -1.61     12.5
#>  6 15327 Centre Kadiogo  Ouagad… Urban             1061        160     0.7        1509.     -1.52     12.4
#>  7 15429 Centre Kadiogo  Ouagad… Urban              667        100     0.14       4772.     -1.67     12.4
#>  8 15550 Centre Kadiogo  Ouagad… Urban              967        146     0.24       4114.     -1.53     12.4
#>  9 17098 Centre Kadiogo  Ouagad… Urban              844        127     0.12       6970.     -1.51     12.4
#> 10 17487 Centre Kadiogo  Ouagad… Urban              938        141     0.11       8542.     -1.52     12.3
#> # ℹ 40 more rows
#> # ℹ 8 more variables: remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
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
#> # Sampling:     2 stages | 50/107,250 units
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
#> #   .sample_id <int>, .stage <int>, .weight_2 <dbl>, .fpc_2 <dbl>, .weight_1 <dbl>, .fpc_1 <int>,
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
final_sample <- selected_eas |>
  execute(selected_eas_list, seed = 2)
final_sample
#> # A tbl_sample: 100 × 21
#> # Sampling:     2 stages | 100/107,250 units
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
#> #   .sample_id <int>, .stage <int>, .weight_2 <dbl>, .fpc_2 <dbl>, .weight_1 <dbl>, .fpc_1 <int>,
#> #   .certainty_1 <lgl>
```

`selected_eas` still contains `design` plus the realized stage-1
selection. Using it as the first argument continues that same
multi-stage design; the expanded listing is only the candidate frame for
stage 2. Starting from a new design with a prior `tbl_sample` as its
frame instead denotes a new sampling phase.

If `tidyr::uncount()` or another operation drops the listing’s
`tbl_sample` class, keep using that plain object as the second argument
above. `execute()` recognizes its inherited sampling attributes and
generated columns and refuses to run it as a fresh frame from `design`,
which would silently rerun stage 1. Passing the intact `selected_eas`
back as a frame of `design` also warns: that call is supported as a new
sampling phase, but it is not stage continuation.

## Selection Methods

Sixteen methods ship in three families:

- **Equal probability**: `srswor` (the default), `srswr`, `systematic`,
  `bernoulli`
- **PPS**, all requiring a measure of size: `pps_brewer`,
  `pps_systematic`, `pps_cps`, `pps_sampford`, `pps_poisson`, `pps_sps`,
  `pps_pareto`, `pps_multinomial`, `pps_chromy`
- **Balanced**: `cube`, plus the spatial `lpm2` and `scps`

`?selection-methods` is the reference: which take `n` or `frac`, which
need `mos`, `aux` or `spread`, which have a random rather than fixed
sample size, which accept permanent random numbers, and the paper behind
each. Custom methods registered with `sondage::register_method()` are
used the same way.

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
#> # A tbl_sample: 300 × 18
#> # Sampling:     1 stage | 300/44,570 units
#> # Weights:      148.57 [142.32, 162]
#>    ea_id region province commune urban_rural population households area_km2 pop_density longitude latitude
#>  * <int> <fct>  <fct>    <fct>   <fct>            <int>      <int>    <dbl>       <dbl>     <dbl>    <dbl>
#>  1 11643 Boucl… Nayala   Yaba    Rural              396         52     8.75        45.2     -2.81     12.8
#>  2 36763 Boucl… Bale     Fara    Rural               79         12     2.36        33.5     -2.56     11.4
#>  3  9648 Boucl… Banwa    Sanaba  Rural              279         35     8.37        33.3     -3.79     12.4
#>  4 10555 Boucl… Kossi    Sônô    Rural              164         24     8.38        19.6     -3.50     12.8
#>  5 34926 Boucl… Sourou   Tougan  Rural             1239        189     1.15      1078.      -3.01     13.1
#>  6 44501 Boucl… Mouhoun  Tchéri… Rural              221         35     0.22       998.      -3.01     12.3
#>  7  9702 Boucl… Banwa    Sanaba  Rural              260         33     6.92        37.5     -3.87     12.4
#>  8 26045 Boucl… Mouhoun  Dédoug… Rural              378         54     8.36        45.2     -3.66     12.3
#>  9  8605 Boucl… Bale     Pâ      Rural              283         33     8.78        32.2     -3.26     11.5
#> 10 21078 Boucl… Kossi    Bouras… Rural               93         15     7.08        13.1     -3.65     12.6
#> # ℹ 290 more rows
#> # ℹ 7 more variables: remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <dbl>
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
#> # A tbl_sample: 300 × 18
#> # Sampling:     1 stage | 300/44,570 units
#> # Weights:      148.57 [142.32, 162]
#>    ea_id region province commune urban_rural population households area_km2 pop_density longitude latitude
#>  * <int> <fct>  <fct>    <fct>   <fct>            <int>      <int>    <dbl>       <dbl>     <dbl>    <dbl>
#>  1 21517 Boucl… Kossi    Doumba… Rural              354         61     8.72        40.6     -4.17     12.7
#>  2 33112 Boucl… Kossi    Nouna   Rural               28          4     5.47         5.1     -3.71     12.7
#>  3 25963 Boucl… Mouhoun  Dédoug… Rural              416         59     0.67       624.      -3.47     12.6
#>  4  6354 Boucl… Banwa    Kouka   Rural              296         34     8.95        33.1     -4.32     12.0
#>  5 11787 Boucl… Bale     Bagassi Rural              874        105     1.4        626.      -3.31     11.7
#>  6  8233 Boucl… Mouhoun  Ouarko… Rural              183         25     9.54        19.2     -3.67     12.1
#>  7  9925 Boucl… Bale     Siby    Rural              204         30     0.36       572.      -2.87     11.9
#>  8 34812 Boucl… Sourou   Tougan  Rural             1072        164     0.3       3575.      -3.06     13.1
#>  9 11441 Boucl… Sourou   Toéni   Rural               64          9    23.9          2.7     -3.03     13.4
#> 10  8883 Boucl… Bale     Pompoï  Rural              192         28     9.42        20.4     -3.16     11.8
#> # ℹ 290 more rows
#> # ℹ 7 more variables: remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <dbl>
```

## Beyond the Basics

The verbs above cover most designs. These capabilities work the same way
and are documented where they are taught in depth:

| Capability | How | Where |
|----|----|----|
| Balanced sampling | `draw(method = "cube", aux = ...)`, with `bound()` for hard count constraints | `vignette("introduction")` |
| Spatially balanced | `draw(method = "lpm2" or "scps", spread = c(lon, lat))` | `?selection-methods` |
| Sample coordination | `draw(prn = ...)` with permanent random numbers, for overlap across waves | `vignette("sampling-coordination")` |
| Custom methods | `sondage::register_method()`, then `pps_<name>` or `balanced_<name>` | `vignette("introduction")` |
| Panel rotation | `execute(panels = 4)` | `vignette("design-semantics")` |
| Replicated draws | `execute(reps = 5)` | `?execute` |
| Two-phase | pipe a `tbl_sample` into a new design’s `execute()` | `vignette("survey-analysis")` |

``` r
# Balanced on auxiliary totals, PPS on size
sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300, method = "cube", mos = households,
       aux = c(population, area_km2)) |>
  execute(bfa_eas, seed = 24)

# Four rotation groups, five independent replicates
execute(design, bfa_eas, seed = 1, panels = 4)
execute(design, bfa_eas, seed = 42, reps = 5)
```

## Survey Export

Convert to `survey` or `srvyr` for estimation:

``` r
svy <- as_svydesign(sample)
survey::svymean(~y, svy)

as_survey_design(sample) |> dplyr::summarise(mean_y = srvyr::survey_mean(y))
```

`as_svydesign()` uses the Brewer variance approximation by default.
`joint_expectation()` computes exact pairwise joint inclusion
probabilities where the method supports them, for tighter variance
estimates. See `vignette("survey-analysis")` for domain estimation,
replicate weights, and the method-by-method breakdown.

## Diagnostics

``` r
summary(strata_smpl)
#> ── Sample Summary ───────────────────────────────────────────────────────────────────────────────────
#> 
#> ℹ n = 300 of 44,570 | stages = 1/1 | seed = 12
#> 
#> ── Stage 1 ──────────────────────────────────────────────────────────────────────────────────────────
#> • srswor, by region (proportional)
#> • 13 strata: N_h 1,612-5,505, n_h 11-37, f_h 0.0066-0.0068
#> 
#> ── Weights ──────────────────────────────────────────────────────────────────────────────────────────
#> • Mean 148.57 [146.5, 151.22] | CV 0.01 | Kish DEFF 1 | n_eff 300
```

By default, each execution records a summary frame digest: a compact
record of the selection pools and chances the design resolved.
`frame_summary()` returns it as tibbles without needing the frame. Use
`frame_digest = "none"` for the lowest execution overhead, or
`frame_digest = "full"` when downstream work needs exact per-unit
chances for unequal-probability element stages. The companion package
samplens, in development, draws the digest as a visual sampling card.
See `vignette("introduction")` for examples.

`execute()` also reports when the frame could not deliver what the
design asked for. A pool holding fewer units than the stage requested is
selected whole, which makes the design non-self-weighting, so this is a
warning rather than a silent adjustment. Each finding is reported once
per stage, however many pools were affected and however many replicates
ran:

| Condition | Meaning |
|----|----|
| `samplyr_warning_size_capped` | Some pools held fewer units than requested |
| `samplyr_warning_census` | The stage selected every unit within reach, so it contributes no sampling variance |
| `samplyr_warning_nominal_cap` | A random-size method asked for more units than the pool holds |
| `samplyr_warning_poisson_shortfall` | Dominant units saturated a `pps_poisson` stage below its reachable target |
| `samplyr_message_allocation_capped` | A feasible allocation was redistributed past a saturated stratum |

`frame_digest` defaults to `"summary"`, so the usual way to read capping
is the `capped` column of `frame_summary(sample, detail = "pool")`. Each
condition also carries the affected pools and the counts behind them in
a `payload`, which is what remains under `frame_digest = "none"`. See
`?execution-conditions` for the payload contract and for how to capture
one.

## Validation

For statistical validation on synthetic populations with known truths,
see `vignette("validation")`. It combines deterministic invariants
(weights, FPC, certainty, stage compounding) with Monte Carlo checks of
bias, standard-error calibration, and coverage.

## Included Datasets

Derived and synthetic sampling frames for learning and testing:

| Dataset | Description | Rows |
|----|----|----|
| `bfa_eas` | Household budget and living-standards EA frame (Burkina Faso) | 44,570 |
| `zwe_eas` | Demographic, health, and child-indicator EA frame (Zimbabwe) | 107,250 |
| `ken_enterprises` | Establishment survey frame (Kenya) | 17,004 |

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
