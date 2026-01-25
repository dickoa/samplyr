# samplyr

A tidy grammar for survey sampling in R. **samplyr** provides a minimal set of composable verbs for stratified, clustered, and multi-stage sampling designs.

## Installation

``` r
# Install sondage first (sampling algorithms backend)
remotes::install_gitlab("dickoa/sondage")

remotes::install_gitlab("dickoa/samplyr")
```

## Overview

samplyr is built around a simple idea: sampling code should read like its English description.

```r
library(samplyr)

# "Stratify by region, proportionally allocate 500 samples, execute"
sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 500) |>
  execute(frame, seed = 42)
```

The package uses 5 verbs and 1 modifier:

| Function            | Purpose                               |
|---------------------|---------------------------------------|
| `sampling_design()` | Create a new sampling design          |
| `stratify_by()`     | Define stratification and allocation  |
| `cluster_by()`      | Define cluster/PSU variable           |
| `draw()`            | Specify sample size and method        |
| `execute()`         | Run the design on a frame             |
| `stage()`           | Delimit stages in multi-stage designs |

## Quick Start

```r
library(samplyr)
data(niger_eas)

# Simple random sample
sample <- sampling_design() |>
  draw(n = 100) |>
  execute(niger_eas, seed = 42)

# Stratified proportional allocation
sample <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300) |>
  execute(niger_eas, seed = 42)

# PPS cluster sampling
sample <- sampling_design() |>
  cluster_by(ea_id) |>
  draw(n = 50, method = "pps_brewer", mos = hh_count) |>
  execute(niger_eas, seed = 42)
```

## Multi-Stage Sampling

Use `stage()` to define multi-stage designs. This example selects districts with PPS, then samples schools within each:

```r
data(tanzania_schools)

# Add district-level measure of size
schools_frame <- tanzania_schools |>
  dplyr::group_by(district) |>
  dplyr::mutate(district_enrollment = sum(enrollment)) |>
  dplyr::ungroup()

# Two-stage design: 10 districts, 5 schools per district
sample <- sampling_design() |>
  stage(label = "Districts") |>
    cluster_by(district) |>
    draw(n = 10, method = "pps_brewer", mos = district_enrollment) |>
  stage(label = "Schools") |>
    draw(n = 5) |>
  execute(schools_frame, seed = 42)
```

### Operational Sampling

Execute stages separately when fieldwork happens between stages:

```r
design <- sampling_design() |>
  stage(label = "Districts") |>
    cluster_by(district) |>
    draw(n = 10, method = "pps_brewer", mos = district_enrollment) |>
  stage(label = "Schools") |>
    draw(n = 5)

# Execute stage 1 only
selected_districts <- execute(design, schools_frame, stages = 1, seed = 42)

# ... fieldwork ...

# Execute stage 2
final_sample <- selected_districts |> execute(schools_frame, seed = 43)
```

## Selection Methods

### Equal Probability

| Method       | Sample Size | Description                                          |
|--------------|-------------|------------------------------------------------------|
| `srswor`     | Fixed       | Simple random sampling without replacement (default) |
| `srswr`      | Fixed       | Simple random sampling with replacement              |
| `systematic` | Fixed       | Systematic sampling                                  |
| `bernoulli`  | Random      | Bernoulli sampling (requires `frac`)                 |

### Probability Proportional to Size (PPS)

| Method            | Sample Size | Description                   |
|-------------------|-------------|-------------------------------|
| `pps_brewer`      | Fixed       | Brewer's method (recommended) |
| `pps_systematic`  | Fixed       | PPS systematic                |
| `pps_maxent`      | Fixed       | Maximum entropy               |
| `pps_poisson`     | Random      | PPS Poisson (requires `frac`) |
| `pps_multinomial` | Fixed       | PPS with replacement          |

## Allocation Methods

When stratifying, control how the total sample is distributed:

| Method         | Description                                             |
|----------------|---------------------------------------------------------|
| (none)         | `n` applies per stratum                                 |
| `equal`        | Same sample size in each stratum                        |
| `proportional` | Proportional to stratum size                            |
| `neyman`       | Minimize variance (requires `variance`)                 |
| `optimal`      | Minimize cost-variance (requires `variance` and `cost`) |

### Sample Size Bounds

Use `min_n` and `max_n` in `draw()` to constrain stratum sample sizes when using allocation methods:

```r
# Ensure at least 2 per stratum (minimum for variance estimation)
sampling_design() |>
  stratify_by(region, alloc = "neyman", variance = niger_eas_variance) |>
  draw(n = 300, min_n = 2) |>
  execute(niger_eas, seed = 42)

# Cap large strata, ensure minimum representation
sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 500, min_n = 10, max_n = 100) |>
  execute(frame, seed = 42)
```

### Custom Allocation

For custom stratum-specific sizes or rates, pass a data frame to `n` or `frac` in `draw()`:

```r
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

```r
# Neyman allocation
data(niger_eas_variance)

sample <- sampling_design() |>
  stratify_by(region, alloc = "neyman", variance = niger_eas_variance) |>
  draw(n = 300) |>
  execute(niger_eas, seed = 42)
```

## Included Datasets

Synthetic datasets for learning and testing:

| Dataset | Description | Rows |
|---------|-------------|------|
| `niger_eas` | DHS-style enumeration areas (Niger) | ~1,500 |
| `uganda_farms` | LSMS-style agricultural frame (Uganda) | ~800 |
| `kenya_health` | Health facility frame (Kenya) | ~3,000 |
| `tanzania_schools` | School survey frame (Tanzania) | ~2,500 |
| `nigeria_business` | Enterprise survey frame (Nigeria) | ~10,000 |

Plus auxiliary data: `niger_eas_variance`, `niger_eas_cost`

## Comparison with SAS and SPSS

### SAS PROC SURVEYSELECT

```sas
proc surveyselect data=frame method=pps n=50 seed=42;
  strata region;
  cluster school;
  size enrollment;
run;
```

```r
sampling_design() |>
  stratify_by(region) |>
  cluster_by(school) |>
  draw(n = 50, method = "pps_brewer", mos = enrollment) |>
  execute(frame, seed = 42)
```

### SAS Allocation with Bounds

```sas
proc surveyselect data=frame method=srs n=500 seed=42;
  strata region / alloc=neyman var=variance_data allocmin=2 allocmax=100;
run;
```

```r
sampling_design() |>
  stratify_by(region, alloc = "neyman", variance = variance_data) |>
  draw(n = 500, min_n = 2, max_n = 100) |>
  execute(frame, seed = 42)
```

### SPSS CSPLAN

```spss
CSPLAN SAMPLE
  /PLAN FILE='myplan.csplan'
  /DESIGN STRATA=region CLUSTER=school
  /METHOD TYPE=PPS_WOR
  /SIZE VALUE=50
  /MOS VARIABLE=enrollment.
```

```r
sampling_design() |>
  stratify_by(region) |>
  cluster_by(school) |>
  draw(n = 50, method = "pps_brewer", mos = enrollment) |>
  execute(frame, seed = 42)
```

## License

MIT © Ahmadou Dicko
