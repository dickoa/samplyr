
<!-- README.md is generated from README.Rmd. Please edit that file -->

# samplyr

A tidy grammar for survey sampling in R. **samplyr** provides a minimal
set of composable verbs for stratified, clustered, multi-stage, and
multi-phase sampling designs with PPS methods, sample coordination, and
panel rotation.

## Installation

``` r
# Install sondage first (sampling algorithms backend)
pak::pkg_install("gitlab::dickoa/sondage")

# Install svyplan first (sample size, power and strata alloc)
pak::pkg_install("gitlab::dickoa/svyplan")

# Install samplyr
pak::pkg_install("gitlab::dickoa/samplyr")
```

## Overview

samplyr is built around a simple idea: sampling code should read like
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
#> # Weights:      29.8 [28.95, 30.32]
#>    ea_id    region     province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <chr>    <fct>      <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 EA_10155 Boucle du… Mouhoun  Ouarko… Rural             1347        187    33.0  TRUE                7.1
#>  2 EA_03955 Boucle du… Kossi    Doumba… Rural             1558        211    16.6  TRUE               19.6
#>  3 EA_10325 Boucle du… Bale     Ouri    Rural              767         92    17.6  TRUE                7.7
#>  4 EA_03209 Boucle du… Mouhoun  Dedoug… Rural              446         79    18.1  TRUE                5.7
#>  5 EA_12881 Boucle du… Banwa    Tansila Rural             1076        137     9.45 TRUE               13.1
#>  6 EA_11613 Boucle du… Banwa    Sami    Rural              912        118    43.2  TRUE                9.1
#>  7 EA_06857 Boucle du… Banwa    Kouka   Rural             1642        189    12.5  TRUE               17.6
#>  8 EA_13730 Boucle du… Nayala   Toma    Rural              973        101     0.88 TRUE                8  
#>  9 EA_05972 Boucle du… Sourou   Kiemba… Rural             1359        216    36.6  TRUE                8.6
#> 10 EA_03571 Boucle du… Kossi    Djibas… Rural              725         86     1.01 TRUE               10  
#> # ℹ 490 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>
```

The package uses 5 verbs and 1 modifier:

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

sample <- execute(design, bfa_eas, seed = 42)
sample
#> # A tbl_sample: 300 × 17
#> # Weights:      49.67 [47.64, 50.67]
#>    ea_id    region     province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <chr>    <fct>      <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 EA_00268 Boucle du… Bale     Bagassi Rural              927        111    41.7  TRUE               10.4
#>  2 EA_02157 Boucle du… Bale     Boromo  Rural              129         17     8.12 FALSE               7.9
#>  3 EA_10375 Boucle du… Bale     Pa      Rural              943        133     4.73 TRUE                8.3
#>  4 EA_06846 Boucle du… Banwa    Kouka   Rural             3546        408    12.2  TRUE                6.1
#>  5 EA_11697 Boucle du… Banwa    Sanaba  Rural             1359        197    31.2  TRUE               49.7
#>  6 EA_12357 Boucle du… Banwa    Solenzo Rural              908        108    17.5  TRUE               23.1
#>  7 EA_12411 Boucle du… Banwa    Solenzo Rural             1341        160     0.46 TRUE               20.9
#>  8 EA_12891 Boucle du… Banwa    Tansila Rural             1656        211    61.1  TRUE                6.7
#>  9 EA_02518 Boucle du… Kossi    Bouras… Rural              806         91    14.8  TRUE               14  
#> 10 EA_03575 Boucle du… Kossi    Djibas… Rural              773         92     0.39 TRUE               32.4
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
  execute(bfa_eas, seed = 42)

srs_smpl
#> # A tbl_sample: 100 × 17
#> # Weights:      149 [149, 149]
#>    ea_id    region     province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <chr>    <fct>      <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 EA_05883 Hauts-Bas… Kenedou… Kayan   Rural             1572        190    33.0  TRUE                8.6
#>  2 EA_12026 Nord       Yatenga  Sengue… Rural             1774        220    19.6  FALSE              12.4
#>  3 EA_08856 Centre     Kadiogo  Ouagad… Urban             2051        389     0.37 TRUE                0.5
#>  4 EA_14625 Centre-No… Bam      Zimtan… Rural             1531        272    11.6  TRUE               17  
#>  5 EA_10472 Est        Kompien… Pama    Rural             1382        179    66.4  FALSE               8.9
#>  6 EA_14276 Boucle du… Nayala   Ye      Rural             1314        232     2.27 FALSE              40.3
#>  7 EA_03283 Est        Gourma   Diabo   Rural             1505        187    33.1  FALSE               3.6
#>  8 EA_01810 Hauts-Bas… Houet    Bobo-D… Urban             2759        442     0.37 TRUE                0.2
#>  9 EA_00658 Sahel      Seno     Bani    Rural             1458        312    17.8  FALSE               5  
#> 10 EA_03897 Sahel      Seno     Dori    Rural             1054        137    10.1  TRUE                6.1
#> # ℹ 90 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>

# Stratified proportional allocation
strata_smpl <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300) |>
  execute(bfa_eas, seed = 42)

strata_smpl
#> # A tbl_sample: 300 × 17
#> # Weights:      49.67 [47.64, 50.67]
#>    ea_id    region     province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <chr>    <fct>      <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 EA_02518 Boucle du… Kossi    Bouras… Rural              806         91    14.8  TRUE               14  
#>  2 EA_11697 Boucle du… Banwa    Sanaba  Rural             1359        197    31.2  TRUE               49.7
#>  3 EA_06821 Boucle du… Nayala   Kougny  Rural             3371        449     3.12 TRUE                9.5
#>  4 EA_12935 Boucle du… Mouhoun  Tcheri… Rural             1279        153    10.4  TRUE               15.1
#>  5 EA_14276 Boucle du… Nayala   Ye      Rural             1314        232     2.27 FALSE              40.3
#>  6 EA_06814 Boucle du… Nayala   Kougny  Rural              849        113    52.8  TRUE               18.6
#>  7 EA_03727 Boucle du… Kossi    Dokui   Rural             2183        349    10.0  TRUE               29  
#>  8 EA_02157 Boucle du… Bale     Boromo  Rural              129         17     8.12 FALSE               7.9
#>  9 EA_04881 Boucle du… Nayala   Gossina Rural             1549        250    15.1  TRUE                8.4
#> 10 EA_05968 Boucle du… Sourou   Kiemba… Rural              976        155    20.4  TRUE                6.4
#> # ℹ 290 more rows
#> # ℹ 7 more variables: food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>

# PPS cluster sampling
cluster_smpl <- sampling_design() |>
  cluster_by(ea_id) |>
  draw(n = 50, method = "pps_brewer", mos = households) |>
  execute(bfa_eas, seed = 42)

cluster_smpl
#> # A tbl_sample: 50 × 18
#> # Weights:      289.64 [88.85, 616.64]
#>    ea_id    region     province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <chr>    <fct>      <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 EA_02180 Boucle du… Bale     Boromo  Rural             4324        564     3.13 TRUE               16.1
#>  2 EA_10322 Boucle du… Bale     Ouri    Rural             1605        192    52.5  TRUE               20.1
#>  3 EA_06254 Boucle du… Kossi    Kombori Rural             1549        194    41.2  FALSE              25.4
#>  4 EA_13829 Boucle du… Sourou   Tougan  Rural             1309        181    55.4  TRUE                3.5
#>  5 EA_06311 Centre     Kadiogo  Komki-… Rural             1243        188    15.7  TRUE                8.4
#>  6 EA_08822 Centre     Kadiogo  Ouagad… Urban             1804        342     0.31 TRUE                1.4
#>  7 EA_08853 Centre     Kadiogo  Ouagad… Urban             1452        275     0.26 TRUE                0.5
#>  8 EA_09411 Centre     Kadiogo  Ouagad… Urban             2084        395     0.27 TRUE                0.3
#>  9 EA_09799 Centre     Kadiogo  Ouagad… Urban             1889        358     1.23 TRUE                0.6
#> 10 EA_11351 Centre     Kadiogo  Saaba   Urban             2486        445     6.55 TRUE                0.1
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
  execute(zwe_frame, seed = 123)

sample
#> # A tbl_sample: 50 × 16
#> # Weights:      457.6 [196.93, 659.34]
#>    ea_id    province district urban_rural population households area_km2 district_hh .weight .sample_id
#>  * <chr>    <fct>    <fct>    <fct>            <int>      <int>    <dbl>       <int>   <dbl>      <int>
#>  1 EA_00244 Bulawayo Bulawayo Urban             1213        331     0.54      159761    216.          1
#>  2 EA_00014 Bulawayo Bulawayo Rural             1392        335    31.6       159761    216.          2
#>  3 EA_00374 Bulawayo Bulawayo Urban             1352        389     0.38      159761    216.          3
#>  4 EA_00153 Bulawayo Bulawayo Urban             1254        325     0.79      159761    216.          4
#>  5 EA_00090 Bulawayo Bulawayo Urban             1232        340     1.97      159761    216.          5
#>  6 EA_01342 Harare   Harare   Urban             1837        475     1.03      374741    197.          6
#>  7 EA_01507 Harare   Harare   Urban             1431        388     0.24      374741    197.          7
#>  8 EA_01448 Harare   Harare   Urban             1218        345     0.43      374741    197.          8
#>  9 EA_00830 Harare   Harare   Urban             1246        372     0.3       374741    197.          9
#> 10 EA_01692 Harare   Harare   Urban             1222        362     0.75      374741    197.         10
#> # ℹ 40 more rows
#> # ℹ 6 more variables: .stage <int>, .weight_2 <dbl>, .fpc_2 <int>, .weight_1 <dbl>, .fpc_1 <int>,
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
| `bernoulli` | Random | Bernoulli sampling (requires `frac`) |

### Probability Proportional to Size (PPS)

| Method            | Sample Size | Description                       |
|-------------------|-------------|-----------------------------------|
| `pps_brewer`      | Fixed       | Brewer’s method (recommended)     |
| `pps_systematic`  | Fixed       | PPS systematic                    |
| `pps_cps`         | Fixed       | Conditional Poisson sampling      |
| `pps_poisson`     | Random      | PPS Poisson (requires `frac`)     |
| `pps_sps`         | Fixed       | Sequential Poisson sampling (PRN) |
| `pps_pareto`      | Fixed       | Pareto piPS sampling (PRN)        |
| `pps_multinomial` | Fixed       | PPS with replacement              |
| `pps_chromy`      | Fixed       | PPS with minimum replacement      |

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
  execute(bfa_eas, seed = 42)
#> # A tbl_sample: 300 × 17
#> # Weights:      49.67 [36.08, 77.8]
#>    ea_id    region     province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <chr>    <fct>      <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 EA_02518 Boucle du… Kossi    Bouras… Rural              806         91    14.8  TRUE               14  
#>  2 EA_11697 Boucle du… Banwa    Sanaba  Rural             1359        197    31.2  TRUE               49.7
#>  3 EA_06821 Boucle du… Nayala   Kougny  Rural             3371        449     3.12 TRUE                9.5
#>  4 EA_12935 Boucle du… Mouhoun  Tcheri… Rural             1279        153    10.4  TRUE               15.1
#>  5 EA_14276 Boucle du… Nayala   Ye      Rural             1314        232     2.27 FALSE              40.3
#>  6 EA_06814 Boucle du… Nayala   Kougny  Rural              849        113    52.8  TRUE               18.6
#>  7 EA_03727 Boucle du… Kossi    Dokui   Rural             2183        349    10.0  TRUE               29  
#>  8 EA_02157 Boucle du… Bale     Boromo  Rural              129         17     8.12 FALSE               7.9
#>  9 EA_04881 Boucle du… Nayala   Gossina Rural             1549        250    15.1  TRUE                8.4
#> 10 EA_05968 Boucle du… Sourou   Kiemba… Rural              976        155    20.4  TRUE                6.4
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
  execute(bfa_eas, seed = 42)

sample
#> # A tbl_sample: 300 × 17
#> # Weights:      49.67 [36.08, 77.8]
#>    ea_id    region     province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <chr>    <fct>      <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 EA_02518 Boucle du… Kossi    Bouras… Rural              806         91    14.8  TRUE               14  
#>  2 EA_11697 Boucle du… Banwa    Sanaba  Rural             1359        197    31.2  TRUE               49.7
#>  3 EA_06821 Boucle du… Nayala   Kougny  Rural             3371        449     3.12 TRUE                9.5
#>  4 EA_12935 Boucle du… Mouhoun  Tcheri… Rural             1279        153    10.4  TRUE               15.1
#>  5 EA_14276 Boucle du… Nayala   Ye      Rural             1314        232     2.27 FALSE              40.3
#>  6 EA_06814 Boucle du… Nayala   Kougny  Rural              849        113    52.8  TRUE               18.6
#>  7 EA_03727 Boucle du… Kossi    Dokui   Rural             2183        349    10.0  TRUE               29  
#>  8 EA_02157 Boucle du… Bale     Boromo  Rural              129         17     8.12 FALSE               7.9
#>  9 EA_04881 Boucle du… Nayala   Gossina Rural             1549        250    15.1  TRUE                8.4
#> 10 EA_05968 Boucle du… Sourou   Kiemba… Rural              976        155    20.4  TRUE                6.4
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
  execute(frame, seed = 1)

# Wave 2: same PRN -> high overlap (positive coordination)
wave2 <- sampling_design() |>
  draw(n = 500, method = "pps_sps", mos = size, prn = prn) |>
  execute(frame, seed = 2)
```

PRN is supported for `bernoulli`, `pps_poisson`, `pps_sps`, and
`pps_pareto`.

## Survey Export

Convert samples to `survey` or `srvyr` objects for analysis:

``` r
svy <- as_svydesign(sample)
survey::svymean(~y, svy)

# Exact or high-entropy joint inclusion probabilities
jip <- joint_expectation(sample, frame, stage = 1)
svy_exact <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
```

By default `as_svydesign()` uses Brewer’s variance approximation. For
tighter variance estimates, `joint_expectation()` computes pairwise
joint inclusion probabilities from the original frame. The result is
exact for CPS, systematic, and Poisson; for Brewer, SPS, and Pareto it
uses the O(N^2) high-entropy approximation (exact recursive formulas
exist but are O(N^3) and impractical for large frames). See
`?joint_expectation` for the full method-by-method breakdown.

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
to all units. Weights reflect the full-sample inclusion probability; for
per-panel analysis, multiply by the number of panels.

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
#> # Weights:      298 [298, 298]
#>    ea_id    region     province commune urban_rural population households area_km2 accessible dist_road_km
#>  * <chr>    <fct>      <fct>    <fct>   <fct>            <dbl>      <int>    <dbl> <lgl>             <dbl>
#>  1 EA_13110 Centre-Est Boulgou  Tenkod… Rural             1761        348     3.96 TRUE               27.9
#>  2 EA_11301 Centre-No… Bam      Rouko   Urban             2578        374    19.5  TRUE                0  
#>  3 EA_07031 Hauts-Bas… Kenedou… Kourin… Rural             1149        152    35.0  TRUE                7  
#>  4 EA_10184 Sud-Ouest  Ioba     Ouessa  Rural             2061        306    33.6  TRUE               17.2
#>  5 EA_05021 Nord       Zondoma  Goursi  Rural              747         91     0.27 TRUE               30  
#>  6 EA_02164 Boucle du… Bale     Boromo  Rural              528         69    12.5  TRUE                6.8
#>  7 EA_03618 Sahel      Soum     Djibo   Rural             1237        163    32.8  FALSE               6.1
#>  8 EA_00184 Nord       Passore  Arbolle Rural             2026        234    20.1  FALSE               9.2
#>  9 EA_06727 Centre-Ou… Boulkie… Koudou… Urban             1916        368     3.39 TRUE                0  
#> 10 EA_09532 Centre     Kadiogo  Ouagad… Urban             2282        433     0.4  TRUE                0.5
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
#> ℹ n = 300 | stages = 1/1 | seed = 42
#> 
#> ── Design: Stage 1 ──────────────────────────────────────────────────────────────────────────────────
#> • Strata: region (proportional)
#> • Method: srswor
#> 
#> ── Allocation: Stage 1 ──────────────────────────────────────────────────────────────────────────────
#>   region             N_h    n_h  f_h   
#>   Boucle du Mouhoun  1483   30   0.0202
#>   Cascades           667    14   0.0210
#>   Centre             1556   31   0.0199
#>   Centre-Est         1259   25   0.0199
#>   Centre-Nord        1375   28   0.0204
#>   Centre-Ouest       1287   26   0.0202
#>   Centre-Sud         608    12   0.0197
#>   Est                1590   32   0.0201
#>   Hauts-Bassins      1483   30   0.0202
#>   Nord               1211   24   0.0198
#>   Plateau-Central    757    15   0.0198
#>   Sahel              902    18   0.0200
#>   Sud-Ouest          722    15   0.0208
#>                      ─────  ───  ──────
#>   Total              14900  300  0.0201
#> 
#> ── Weights ──────────────────────────────────────────────────────────────────────────────────────────
#> • Range: [47.64, 50.67]
#> • Mean:  49.67 · CV: 0.02
#> • DEFF:  1 · n_eff: 300
```

## Included Datasets

Synthetic sampling frames for learning and testing:

| Dataset | Description | Rows |
|----|----|----|
| `bfa_eas` | LSMS-style EA frame (Burkina Faso) | 14,900 |
| `zwe_eas` | DHS-style EA frame (Zimbabwe) | 22,600 |
| `zwe_households` | Household frame for two-phase sampling (Zimbabwe) | 379,326 |
| `ken_enterprises` | Enterprise survey frame (Kenya) | 6,823 |

Plus auxiliary data: `bfa_eas_variance`, `bfa_eas_cost`

## Comparison with SAS and SPSS

### SAS PROC SURVEYSELECT

``` sas
proc surveyselect data=frame method=pps n=50 seed=42;
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
proc surveyselect data=frame method=sys samprate=0.02 seed=42 round=nearest;
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
