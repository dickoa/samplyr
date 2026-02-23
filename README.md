
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
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_10155 Boucle … Mouhoun  Ouarko… Rural             1347        187    33.0 
#>  2 EA_03955 Boucle … Kossi    Doumba… Rural             1558        211    16.6 
#>  3 EA_10325 Boucle … Bale     Ouri    Rural              767         92    17.6 
#>  4 EA_03209 Boucle … Mouhoun  Dedoug… Rural              446         79    18.1 
#>  5 EA_12881 Boucle … Banwa    Tansila Rural             1076        137     9.45
#>  6 EA_11613 Boucle … Banwa    Sami    Rural              912        118    43.2 
#>  7 EA_06857 Boucle … Banwa    Kouka   Rural             1642        189    12.5 
#>  8 EA_13730 Boucle … Nayala   Toma    Rural              973        101     0.88
#>  9 EA_05972 Boucle … Sourou   Kiemba… Rural             1359        216    36.6 
#> 10 EA_03571 Boucle … Kossi    Djibas… Rural              725         86     1.01
#> # ℹ 490 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
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

sample <- execute(design, bfa_eas, seed = 2)
sample
#> # A tbl_sample: 300 × 17
#> # Weights:      49.67 [47.64, 50.67]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_00466 Boucle … Bale     Bana    Rural             1272        166     1.13
#>  2 EA_10340 Boucle … Bale     Ouri    Rural             2128        254    11.1 
#>  3 EA_00373 Boucle … Banwa    Balave  Rural             1624        256    57.0 
#>  4 EA_00380 Boucle … Banwa    Balave  Rural             1005        158    26.5 
#>  5 EA_06860 Boucle … Banwa    Kouka   Rural             1748        201    18.1 
#>  6 EA_12372 Boucle … Banwa    Solenzo Rural             1012        121    27.3 
#>  7 EA_12393 Boucle … Banwa    Solenzo Rural               79          9     5.68
#>  8 EA_12417 Boucle … Banwa    Solenzo Rural             1316        157    15.5 
#>  9 EA_12874 Boucle … Banwa    Tansila Rural             1267        162    44.5 
#> 10 EA_12890 Boucle … Banwa    Tansila Rural              789        101    15.3 
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
#> # Weights:      149 [149, 149]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_03776 Sahel    Seno     Dori    Rural             1813        235    18.7 
#>  2 EA_12039 Nord     Yatenga  Sengue… Rural             1017        126     3.23
#>  3 EA_11709 Boucle … Banwa    Sanaba  Rural             1074        156    25.2 
#>  4 EA_04563 Est      Komandj… Gayeri  Rural             1302        140    32.2 
#>  5 EA_06603 Centre-… Sanmate… Korsim… Rural              632         97    20.2 
#>  6 EA_14769 Plateau… Oubrite… Zitenga Rural             1628        232    10.3 
#>  7 EA_09105 Centre   Kadiogo  Ouagad… Urban             1900        360     0.34
#>  8 EA_10304 Plateau… Oubrite… Ourgou… Rural             1309        161    10.6 
#>  9 EA_00281 Centre-… Boulgou  Bagre   Rural             1609        255    30   
#> 10 EA_04646 Centre-… Sanguie  Godyr   Rural              720        109     7.1 
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
#> # Weights:      49.67 [47.64, 50.67]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_12451 Boucle … Banwa    Solenzo Rural             2360        282    27.0 
#>  2 EA_12347 Boucle … Banwa    Solenzo Rural             1929        230    30.2 
#>  3 EA_11712 Boucle … Banwa    Sanaba  Rural              816        118    34.3 
#>  4 EA_14295 Boucle … Nayala   Ye      Rural             1281        226    25.9 
#>  5 EA_03138 Boucle … Mouhoun  Dedoug… Rural             1461        259    18.2 
#>  6 EA_11050 Boucle … Bale     Pompoi  Rural              917        144    26.8 
#>  7 EA_13839 Boucle … Sourou   Tougan  Rural             1073        148    12.2 
#>  8 EA_12930 Boucle … Mouhoun  Tcheri… Rural             1243        149     1.15
#>  9 EA_02077 Boucle … Kossi    Bombor… Rural             1036        136     7.59
#> 10 EA_04889 Boucle … Nayala   Gossina Rural             1032        166    27.0 
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
#> # Weights:      319.78 [126.62, 1185.84]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_12440 Boucle … Banwa    Solenzo Rural             1481        177    11.5 
#>  2 EA_12513 Boucle … Kossi    Sono    Rural             1566        225    41.1 
#>  3 EA_03121 Boucle … Mouhoun  Dedoug… Rural             1061        188    11.9 
#>  4 EA_12173 Cascades Comoe    Sidera… Rural             2610        360     6.97
#>  5 EA_08851 Centre   Kadiogo  Ouagad… Urban             1635        310     0.21
#>  6 EA_08884 Centre   Kadiogo  Ouagad… Urban             1514        287     0.32
#>  7 EA_08920 Centre   Kadiogo  Ouagad… Urban             1647        312     0.48
#>  8 EA_08962 Centre   Kadiogo  Ouagad… Urban             1726        327     0.27
#>  9 EA_09481 Centre   Kadiogo  Ouagad… Urban             1556        295     0.18
#> 10 EA_09604 Centre   Kadiogo  Ouagad… Urban             2568        487     0.97
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
#> # Weights:      49.67 [36.08, 77.8]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_13725 Boucle … Nayala   Toma    Rural             1235        128    17.3 
#>  2 EA_14301 Boucle … Nayala   Ye      Rural             1231        217    29.9 
#>  3 EA_11709 Boucle … Banwa    Sanaba  Rural             1074        156    25.2 
#>  4 EA_10155 Boucle … Mouhoun  Ouarko… Rural             1347        187    33.0 
#>  5 EA_06444 Boucle … Mouhoun  Kona    Rural             1148        181    16.6 
#>  6 EA_02527 Boucle … Kossi    Bouras… Rural             1268        143    42.5 
#>  7 EA_03143 Boucle … Mouhoun  Dedoug… Rural             1286        228    12.4 
#>  8 EA_12514 Boucle … Kossi    Sono    Rural             1296        187    23.6 
#>  9 EA_14276 Boucle … Nayala   Ye      Rural             1314        232     2.27
#> 10 EA_12908 Boucle … Mouhoun  Tcheri… Rural             1170        140     9.7 
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
#> # Weights:      49.67 [36.08, 77.8]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_13823 Boucle … Sourou   Tougan  Rural             1153        159    21.0 
#>  2 EA_11104 Boucle … Bale     Poura   Urban             1287        174     0.95
#>  3 EA_12348 Boucle … Banwa    Solenzo Rural              528         63     8.98
#>  4 EA_02124 Boucle … Mouhoun  Bondok… Rural             1372        209    29.8 
#>  5 EA_03164 Boucle … Mouhoun  Dedoug… Rural              650        115    14.6 
#>  6 EA_00273 Boucle … Bale     Bagassi Rural             1459        175    16.3 
#>  7 EA_14004 Boucle … Nayala   Yaba    Rural             2013        271    30.6 
#>  8 EA_03733 Boucle … Kossi    Dokui   Rural             1017        163    24.4 
#>  9 EA_08669 Boucle … Kossi    Nouna   Rural             1470        191    89.2 
#> 10 EA_14009 Boucle … Nayala   Yaba    Rural             1299        175    24.7 
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
#> # Weights:      48.19 [16.47, 157.71]
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_00253 Boucle … Bale     Bagassi Rural             1053        127    15.1 
#>  2 EA_04189 Boucle … Bale     Fara    Rural             1093        175    26.1 
#>  3 EA_10340 Boucle … Bale     Ouri    Rural             2128        254    11.1 
#>  4 EA_10382 Boucle … Bale     Pa      Rural              825        116     0.66
#>  5 EA_14033 Boucle … Bale     Yaho    Rural              946        125    18.5 
#>  6 EA_06882 Boucle … Banwa    Kouka   Rural             1024        118     3.26
#>  7 EA_11693 Boucle … Banwa    Sanaba  Rural             1548        225    16.9 
#>  8 EA_00758 Boucle … Kossi    Barani  Rural             1786        234    49.7 
#>  9 EA_00769 Boucle … Kossi    Barani  Rural             1448        190    40.7 
#> 10 EA_03727 Boucle … Kossi    Dokui   Rural             2183        349    10.0 
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
#>    ea_id    region   province commune urban_rural population households area_km2
#>  * <chr>    <fct>    <fct>    <fct>   <fct>            <dbl>      <int>    <dbl>
#>  1 EA_13110 Centre-… Boulgou  Tenkod… Rural             1761        348     3.96
#>  2 EA_11301 Centre-… Bam      Rouko   Urban             2578        374    19.5 
#>  3 EA_07031 Hauts-B… Kenedou… Kourin… Rural             1149        152    35.0 
#>  4 EA_10184 Sud-Oue… Ioba     Ouessa  Rural             2061        306    33.6 
#>  5 EA_05021 Nord     Zondoma  Goursi  Rural              747         91     0.27
#>  6 EA_02164 Boucle … Bale     Boromo  Rural              528         69    12.5 
#>  7 EA_03618 Sahel    Soum     Djibo   Rural             1237        163    32.8 
#>  8 EA_00184 Nord     Passore  Arbolle Rural             2026        234    20.1 
#>  9 EA_06727 Centre-… Boulkie… Koudou… Urban             1916        368     3.39
#> 10 EA_09532 Centre   Kadiogo  Ouagad… Urban             2282        433     0.4 
#> # ℹ 40 more rows
#> # ℹ 9 more variables: accessible <lgl>, dist_road_km <dbl>,
#> #   food_insecurity_pct <dbl>, cost <dbl>, .weight <dbl>, .sample_id <int>,
#> #   .stage <int>, .weight_1 <dbl>, .fpc_1 <int>
```

Weights compound automatically across phases.

## Diagnostics

``` r
summary(strata_smpl)
#> ── Sample Summary ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> 
#> ℹ n = 300 | stages = 1/1 | seed = 12
#> 
#> ── Design: Stage 1 ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
#> • Strata: region (proportional)
#> • Method: srswor
#> 
#> ── Allocation: Stage 1 ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
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
#> ── Weights ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
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
