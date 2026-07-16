
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
#> # Weights:      89.14 [87.47, 90.09]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1  9648 Boucle du … Banwa    Sanaba  Rural              279         35     8.37
#>  2 11547 Boucle du … Sourou   Toéni   Rural               49          7    21.4 
#>  3 41824 Boucle du … Kossi    Dokui   Rural               75         10    15.8 
#>  4 11012 Boucle du … Banwa    Tansila Rural              592         71     0.95
#>  5 32308 Boucle du … Sourou   Lanfiè… Rural               57          8     9.49
#>  6  7017 Boucle du … Kossi    Madouba Rural              599         74     0.74
#>  7 36700 Boucle du … Bale     Fara    Rural              402         59     6.36
#>  8 11611 Boucle du … Nayala   Yaba    Rural              111         15     8.97
#>  9  8342 Boucle du … Mouhoun  Ouarko… Rural               94         13     8.1 
#> 10 11626 Boucle du … Nayala   Yaba    Rural               56          7     8.31
#> # ℹ 490 more rows
#> # ℹ 10 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
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
#> • Draw: n = 5 (per stratum), method = pps_brewer, mos = population
#> 
#> ── Stage 2 ─────────────────────────────────────────────────────────────────
#> • Strata: phc
#> • Cluster: village
#> • Draw: n = 2 (per stratum), method = pps_brewer, mos = population
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
#> # A tbl_sample: 300 × 18
#> # Weights:      148.57 [146.5, 151.22]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1 29513 Boucle du … Bale     Bana    Rural              181         24     4.89
#>  2 29527 Boucle du … Bale     Bana    Rural              193         25     0.14
#>  3 36703 Boucle du … Bale     Fara    Rural              975        143     1.31
#>  4  8455 Boucle du … Bale     Ouri    Rural               57          8     5.95
#>  5  8580 Boucle du … Bale     Pâ      Rural              171         20     5.12
#>  6 11739 Boucle du … Bale     Yaho    Rural              509         76     8.8 
#>  7 11746 Boucle du … Bale     Yaho    Rural               98         15     8.72
#>  8  6291 Boucle du … Banwa    Kouka   Rural              563         65     7.96
#>  9 34031 Boucle du … Banwa    Sami    Rural               41          6    20.2 
#> 10 34058 Boucle du … Banwa    Sami    Rural              516         78     7.6 
#> # ℹ 290 more rows
#> # ℹ 10 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
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
#> # A tbl_sample: 100 × 18
#> # Weights:      445.7 [445.7, 445.7]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1  5168 Centre-Nord Sanmate… Kaya    Urban              794        129     0.18
#>  2 11281 Centre-Sud  Nahouri  Tiébélé Rural              123         20     8.83
#>  3 30149 Centre-Oue… Sanguie  Dassa   Rural              813        127     7.52
#>  4 13590 Est         Gnagna   Koala   Rural              353         44     6.04
#>  5 26045 Boucle du … Mouhoun  Dédoug… Rural              378         54     8.36
#>  6 44140 Centre-Est  Koulpel… Soudou… Rural              258         43     8.95
#>  7 30931 Est         Komandj… Gayéri  Rural              150         16     8.8 
#>  8 10745 Sahel       Yagha    Tankou… Rural              312         43    17.0 
#>  9 13990 Hauts-Bass… Kenedou… Kourou… Rural              406         48     8.98
#> 10 27757 Est         Tapoa    Partia… Rural              571         75     8.82
#> # ℹ 90 more rows
#> # ℹ 10 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>

# Stratified proportional allocation
strata_smpl <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300) |>
  execute(bfa_eas, seed = 12)

strata_smpl
#> # A tbl_sample: 300 × 18
#> # Weights:      148.57 [146.5, 151.22]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1 31987 Boucle du … Sourou   Kiemba… Rural               24          3     7.24
#>  2  4968 Boucle du … Sourou   Kassoum Rural              303         41     4.13
#>  3  4958 Boucle du … Sourou   Kassoum Rural              142         19     8.24
#>  4 23903 Boucle du … Banwa    Solenzo Rural               94         13    13.5 
#>  5 25996 Boucle du … Mouhoun  Dédoug… Rural              538         76     8.63
#>  6 21501 Boucle du … Kossi    Doumba… Rural              247         42     7.93
#>  7 43794 Boucle du … Mouhoun  Safané  Rural               87         12     8.96
#>  8  9724 Boucle du … Banwa    Sanaba  Rural               51          6     8.02
#>  9 10979 Boucle du … Banwa    Tansila Rural               71          9     7.72
#> 10  8893 Boucle du … Bale     Pompoï  Rural              522         77     0.48
#> # ℹ 290 more rows
#> # ℹ 10 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>

# PPS cluster sampling
cluster_smpl <- sampling_design() |>
  cluster_by(ea_id) |>
  draw(n = 50, method = "pps_brewer", mos = households) |>
  execute(bfa_eas, seed = 123)

cluster_smpl
#> # A tbl_sample: 50 × 19
#> # Weights:      779.3 [188.64, 2999.41]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1 23944 Boucle du … Banwa    Solenzo Rural              758        102     0.77
#>  2 33134 Boucle du … Kossi    Nouna   Rural             1116        145     1.35
#>  3 10578 Boucle du … Kossi    Sônô    Rural              326         49     0.41
#>  4 23289 Cascades    Comoe    Ouô     Rural              534         70     8.62
#>  5 15240 Centre      Kadiogo  Ouagad… Urban              614         92     0.14
#>  6 15327 Centre      Kadiogo  Ouagad… Urban             1061        160     0.7 
#>  7 15429 Centre      Kadiogo  Ouagad… Urban              667        100     0.14
#>  8 15550 Centre      Kadiogo  Ouagad… Urban              967        146     0.24
#>  9 17098 Centre      Kadiogo  Ouagad… Urban              844        127     0.12
#> 10 17487 Centre      Kadiogo  Ouagad… Urban              938        141     0.11
#> # ℹ 40 more rows
#> # ℹ 11 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
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
#>    ea_id province   district    ward_pcode urban_rural population households
#>  * <int> <fct>      <fct>       <chr>      <fct>            <int>      <int>
#>  1 88821 Harare     Harare      ZW192102   Urban              197         57
#>  2 87420 Harare     Harare      ZW192116   Urban             1378        353
#>  3 86941 Harare     Harare      ZW192108   Urban              103         38
#>  4 88444 Harare     Harare      ZW192129   Urban              538        142
#>  5 34696 Harare     Harare      ZW192120   Urban              452        124
#>  6 14864 Manicaland Chimanimani ZW110201   Rural              111         29
#>  7 59668 Manicaland Chimanimani ZW110204   Rural              142         36
#>  8 60166 Manicaland Chimanimani ZW110203   Rural              114         29
#>  9 60082 Manicaland Chimanimani ZW110216   Rural               73         19
#> 10 58991 Manicaland Chimanimani ZW110219   Rural               73         18
#> # ℹ 40 more rows
#> # ℹ 14 more variables: buildings <int>, women_15_49 <int>, men_15_49 <int>,
#> #   children_under5 <int>, area_km2 <dbl>, district_hh <int>, .weight <dbl>,
#> #   .sample_id <int>, .stage <int>, .weight_2 <dbl>, .fpc_2 <int>,
#> #   .weight_1 <dbl>, .fpc_1 <int>, .certainty_1 <lgl>
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
#>    ea_id province          district ward_pcode urban_rural population households
#>  * <int> <fct>             <fct>    <chr>      <fct>            <int>      <int>
#>  1 47209 Bulawayo          Bulawayo ZW102127   Urban              462        121
#>  2 35161 Harare            Harare   ZW192109   Urban              140         38
#>  3 86782 Harare            Harare   ZW192130   Urban              974        263
#>  4 88462 Harare            Harare   ZW192103   Urban              993        302
#>  5 93947 Mashonaland Cent… Bindura  ZW120105   Rural               95         23
#>  6 35770 Mashonaland Cent… Guruve   ZW120307   Urban              121         32
#>  7 35831 Mashonaland Cent… Guruve   ZW120322   Rural              322         80
#>  8 83626 Mashonaland East  Goromon… ZW130225   Urban              277         72
#>  9 37885 Mashonaland East  Mudzi    ZW130514   Urban              294         80
#> 10 83520 Mashonaland East  Murehwa  ZW130627   Rural               55         15
#> 11 20895 Mashonaland East  Mutoko   ZW130726   Rural               96         23
#> 12 36189 Mashonaland West  Makonde  ZW140513   Rural               94         21
#> 13 42748 Masvingo          Mwenezi  ZW180610   Rural               73         15
#> 14 76867 Matabeleland Nor… Binga    ZW150107   Rural               74         20
#> 15  5602 Matabeleland Nor… Tsholot… ZW150603   Rural               62         14
#> 16 61444 Matabeleland Nor… Tsholot… ZW150605   Rural               75         18
#> 17 28253 Matabeleland Sou… Gwanda … ZW162103   Urban              106         35
#> 18 51722 Matabeleland Sou… Umzingw… ZW160701   Urban              357         81
#> 19 54466 Midlands          Gweru U… ZW172114   Urban              510        136
#> 20   866 Midlands          Zvishav… ZW170818   Rural               91         19
#> # ℹ 11 more variables: buildings <int>, women_15_49 <int>, men_15_49 <int>,
#> #   children_under5 <int>, area_km2 <dbl>, .weight <dbl>, .sample_id <int>,
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
#> # Weights:      38147.25 [35714.84, 40579.66]
#>    ea_id province district ward_pcode urban_rural population households
#>  * <int> <fct>    <fct>    <chr>      <fct>            <int>      <int>
#>  1 47209 Bulawayo Bulawayo ZW102127   Urban              462        121
#>  2 47209 Bulawayo Bulawayo ZW102127   Urban              462        121
#>  3 47209 Bulawayo Bulawayo ZW102127   Urban              462        121
#>  4 47209 Bulawayo Bulawayo ZW102127   Urban              462        121
#>  5 47209 Bulawayo Bulawayo ZW102127   Urban              462        121
#>  6 35161 Harare   Harare   ZW192109   Urban              140         38
#>  7 35161 Harare   Harare   ZW192109   Urban              140         38
#>  8 35161 Harare   Harare   ZW192109   Urban              140         38
#>  9 35161 Harare   Harare   ZW192109   Urban              140         38
#> 10 35161 Harare   Harare   ZW192109   Urban              140         38
#> # ℹ 90 more rows
#> # ℹ 14 more variables: buildings <int>, women_15_49 <int>, men_15_49 <int>,
#> #   children_under5 <int>, area_km2 <dbl>, hh_id <int>, .weight <dbl>,
#> #   .sample_id <int>, .stage <int>, .weight_2 <dbl>, .fpc_2 <int>,
#> #   .weight_1 <dbl>, .fpc_1 <int>, .certainty_1 <lgl>
```

`selected_eas` still contains `design` plus the realized stage-1
selection. Using it as the first argument continues that same multistage
design; the expanded listing is only the candidate frame for stage 2.
Starting from a new design with a prior `tbl_sample` as its frame
instead denotes a new sampling phase.

If `tidyr::uncount()` or another operation drops the listing’s
`tbl_sample` class, keep using that plain object as the second argument
above. `execute()` recognizes its inherited sampling attributes and
generated columns and refuses to run it as a fresh frame from `design`,
which would silently rerun stage 1. Passing the intact `selected_eas`
back as a frame of `design` also warns: that call is supported as a new
sampling phase, but it is not stage continuation.

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
| `pps_sampford`    | Fixed       | Sampford sampling                 |
| `pps_poisson`     | Random      | PPS Poisson (PRN)                 |
| `pps_sps`         | Fixed       | Sequential Poisson sampling (PRN) |
| `pps_pareto`      | Fixed       | Pareto piPS sampling (PRN)        |
| `pps_multinomial` | Fixed       | PPS with replacement              |
| `pps_chromy`      | Fixed       | PPS with minimum replacement      |

### Balanced Sampling

| Method | Sample Size | Description                                     |
|--------|-------------|-------------------------------------------------|
| `cube` | Fixed       | Cube method (Deville & Tillé 2004), uses `aux`  |
| `lpm2` | Fixed       | Local pivotal sampling, requires `spread`       |
| `scps` | Fixed       | Spatially correlated Poisson, requires `spread` |

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
#> # Weights:      148.57 [142.32, 162]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1 11643 Boucle du … Nayala   Yaba    Rural              396         52     8.75
#>  2 36763 Boucle du … Bale     Fara    Rural               79         12     2.36
#>  3  9648 Boucle du … Banwa    Sanaba  Rural              279         35     8.37
#>  4 10555 Boucle du … Kossi    Sônô    Rural              164         24     8.38
#>  5 34926 Boucle du … Sourou   Tougan  Rural             1239        189     1.15
#>  6 44501 Boucle du … Mouhoun  Tchéri… Rural              221         35     0.22
#>  7  9702 Boucle du … Banwa    Sanaba  Rural              260         33     6.92
#>  8 26045 Boucle du … Mouhoun  Dédoug… Rural              378         54     8.36
#>  9  8605 Boucle du … Bale     Pâ      Rural              283         33     8.78
#> 10 21078 Boucle du … Kossi    Bouras… Rural               93         15     7.08
#> # ℹ 290 more rows
#> # ℹ 10 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
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
#> # A tbl_sample: 300 × 18
#> # Weights:      148.57 [142.32, 162]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1 21517 Boucle du … Kossi    Doumba… Rural              354         61     8.72
#>  2 33112 Boucle du … Kossi    Nouna   Rural               28          4     5.47
#>  3 25963 Boucle du … Mouhoun  Dédoug… Rural              416         59     0.67
#>  4  6354 Boucle du … Banwa    Kouka   Rural              296         34     8.95
#>  5 11787 Boucle du … Bale     Bagassi Rural              874        105     1.4 
#>  6  8233 Boucle du … Mouhoun  Ouarko… Rural              183         25     9.54
#>  7  9925 Boucle du … Bale     Siby    Rural              204         30     0.36
#>  8 34812 Boucle du … Sourou   Tougan  Rural             1072        164     0.3 
#>  9 11441 Boucle du … Sourou   Toéni   Rural               64          9    23.9 
#> 10  8883 Boucle du … Bale     Pompoï  Rural              192         28     9.42
#> # ℹ 290 more rows
#> # ℹ 10 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
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

The cube method (Deville & Tillé 2004) produces samples whose
Horvitz-Thompson estimates of auxiliary totals match the population
totals, improving precision:

``` r
# Balanced sample using population and area as auxiliary variables
balanced_smpl <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300, method = "cube", mos = households,
       aux = c(population, area_km2)) |>
  execute(bfa_eas, seed = 24)

balanced_smpl
#> # A tbl_sample: 300 × 18
#> # Weights:      153.12 [12.35, 2866.07]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1  1205 Boucle du … Bale     Boromo  Rural             4324        630     3.13
#>  2  1207 Boucle du … Bale     Boromo  Rural             1465        213     1.67
#>  3  8426 Boucle du … Bale     Ouri    Rural              394         56     1.29
#>  4  9923 Boucle du … Bale     Siby    Rural               54          8     8.7 
#>  5 39991 Boucle du … Banwa    Balavé  Rural              865        136     0.81
#>  6  6334 Boucle du … Banwa    Kouka   Rural              785         90     0.94
#>  7 23749 Boucle du … Banwa    Solenzo Rural              235         32     8.79
#>  8 23855 Boucle du … Banwa    Solenzo Rural              831        112     6.92
#>  9 24031 Boucle du … Banwa    Solenzo Rural              227         30     0.35
#> 10 10952 Boucle du … Banwa    Tansila Rural              434         52     8.51
#> # ℹ 290 more rows
#> # ℹ 10 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <dbl>
```

The `aux` parameter accepts bare column names. When combined with `mos`,
inclusion probabilities are proportional to size while the sample
remains balanced on the auxiliary variables. A `bound()` marker adds
hard adjacent-integer count constraints for each observed category:

``` r
controlled_smpl <- sampling_design() |>
  draw(
    n = 300,
    method = "cube",
    mos = households,
    aux = c(population, bound(region), bound(urban_rural))
  ) |>
  execute(bfa_eas, seed = 24)
```

For spatially balanced sampling, choose `lpm2` or `scps` explicitly and
provide coordinates through `spread`:

``` r
spatial_smpl <- sampling_design() |>
  draw(
    n = 300,
    method = "lpm2",
    mos = households,
    spread = c(longitude, latitude)
  ) |>
  execute(bfa_eas, seed = 24)
```

Spatial methods cannot be combined with cube auxiliaries or `bound()`
constraints. Stratified cube designs use the stratified cube algorithm
(Chauvet 2009) in a single call. Balanced-family methods are supported
for up to 2 stages.

## Custom Sampling Methods

Methods registered with `sondage::register_method()` use a family prefix
in `draw()`. Registered `type = "wor"` and `type = "wr"` methods use
`pps_<name>` and require `mos`. Registered `type = "balanced"` methods
use `balanced_<name>` and may omit `mos` for equal probabilities. A
balanced method can declare `supports_aux = TRUE` to receive `aux`, or
`supports_spread = TRUE` to require and receive `spread` coordinates.

For example, to use the elimination procedure of Tillé (1996) from the
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
#> # A tbl_sample: 300 × 19
#> # Weights:      130.21 [14.77, 1717.89]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1 29508 Boucle du … Bale     Bana    Rural              998        130     2.14
#>  2  1198 Boucle du … Bale     Boromo  Rural              924        135     0.42
#>  3 36745 Boucle du … Bale     Fara    Rural             1839        269     1.22
#>  4  8606 Boucle du … Bale     Pâ      Rural              256         30     0.34
#>  5  9052 Boucle du … Bale     Poura   Urban              727        138     0.26
#>  6 11709 Boucle du … Bale     Yaho    Rural              416         62     8.97
#>  7 39994 Boucle du … Banwa    Balavé  Rural              106         17     7.98
#>  8 40012 Boucle du … Banwa    Balavé  Rural             1312        207     0.82
#>  9  6311 Boucle du … Banwa    Kouka   Rural             3561        410     2.69
#> 10 34016 Boucle du … Banwa    Sami    Rural              593         89     1.06
#> # ℹ 290 more rows
#> # ℹ 11 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
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
exact for CPS, Sampford, systematic, and Poisson methods. Brewer, SPS,
Pareto, and cube sampling use an O(N^2) high-entropy approximation;
exact recursive formulas for some of these designs are usually O(N^3)
and impractical for large frames. Joint probabilities are unavailable
for bounded cube, LPM2, and SCPS designs. See `?joint_expectation` for
the full method-by-method breakdown.

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
#> # A tbl_sample: 50 × 18
#> # Weights:      891.4 [891.4, 891.4]
#>    ea_id region      province commune urban_rural population households area_km2
#>  * <int> <fct>       <fct>    <fct>   <fct>            <int>      <int>    <dbl>
#>  1 33282 Boucle du … Kossi    Nouna   Rural              327         43     4.35
#>  2 25236 Est         Gnagna   Bilanga Rural              581         89     8.72
#>  3 33467 Est         Kompien… Pama    Rural               46          6    34.1 
#>  4 19744 Centre-Est  Boulgou  Zabré   Rural              433         79     8.46
#>  5 16081 Centre      Kadiogo  Ouagad… Urban              830        125     0.11
#>  6 20688 Cascades    Comoe    Banfora Rural              823         94     0.18
#>  7 36869 Boucle du … Nayala   Gassan  Rural             1172        143     1.18
#>  8 32034 Centre-Est  Koulpel… Komin-… Rural              444         69     7.97
#>  9 15189 Centre      Kadiogo  Ouagad… Urban              471         71     0.08
#> 10 11082 Sud-Ouest   Bougour… Tianko… Rural               43          7    10.1 
#> # ℹ 40 more rows
#> # ℹ 10 more variables: pop_density <dbl>, longitude <dbl>, latitude <dbl>,
#> #   remoteness <fct>, fieldwork_cost <int>, .weight <dbl>, .sample_id <int>,
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
#> ── Weights ─────────────────────────────────────────────────────────────────
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
