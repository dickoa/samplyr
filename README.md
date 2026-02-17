# samplyr

A tidy grammar for survey sampling in R. **samplyr** provides a minimal set of composable verbs for stratified, clustered, multi-stage, and multi-phase sampling designs with PPS methods, sample coordination, and panel rotation.

## Installation

``` r
# Install sondage first (sampling algorithms backend)
pak::pkg_install("gitlab::dickoa/sondage")

# Install samplyr
pak::pkg_install("gitlab::dickoa/samplyr")
```

## Overview

samplyr is built around a simple idea: sampling code should read like its English description.

```r
library(samplyr)
data(kenya_health)

# "Stratify by region, proportionally allocate 500 samples, execute"
sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 500) |>
  execute(kenya_health, seed = 1)
```

The package uses 5 verbs and 1 modifier:

| Function            | Purpose                               |
|---------------------|---------------------------------------|
| `sampling_design()` | Create a new sampling design          |
| `stratify_by()`     | Define stratification and allocation  |
| `cluster_by()`      | Define cluster/PSU variable           |
| `draw()`            | Specify sample size and method        |
| `execute()`         | Run the design on a frame             |
| `add_stage()`           | Delimit stages in multi-stage designs |

### Frame-Independent Design

`stratify_by()` and `cluster_by()` take bare column names. The design is
stored as a specification and resolved only when a frame is available
(`validate_frame()`, `execute()`, `as_svydesign()`), so design specification
stays separate from execution.

```r
design <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  cluster_by(ea_id) |>
  draw(n = 300)

sample <- execute(design, niger_eas, seed = 42)
```

## Quick Start

```r
library(samplyr)
data(niger_eas)

# Simple random sample
srs_smpl <- sampling_design() |>
  draw(n = 100) |>
  execute(niger_eas, seed = 42)

# Stratified proportional allocation
strata_smpl <- sampling_design() |>
  stratify_by(region, alloc = "proportional") |>
  draw(n = 300) |>
  execute(niger_eas, seed = 42)

# PPS cluster sampling
cluster_smpl <- sampling_design() |>
  cluster_by(ea_id) |>
  draw(n = 50, method = "pps_brewer", mos = hh_count) |>
  execute(niger_eas, seed = 42)
```

## Multi-Stage Sampling

Use `add_stage()` to define multi-stage designs. This example selects districts with PPS, then samples schools within each:

```r
library(dplyr)
data(tanzania_schools)

# Add district-level measure of size
schools_frame <- tanzania_schools |>
  mutate(district_enrollment = sum(enrollment),
         .by = district)

# Two-stage design: 10 districts, 5 schools per district
sample <- sampling_design() |>
  add_stage(label = "Districts") |>
    cluster_by(district) |>
    draw(n = 10, method = "pps_brewer", mos = district_enrollment) |>
  add_stage(label = "Schools") |>
    draw(n = 5) |>
  execute(schools_frame, seed = 123)
```

### Operational Sampling

Execute stages separately when fieldwork happens between stages:

```r
design <- sampling_design() |>
  add_stage(label = "Districts") |>
    cluster_by(district) |>
    draw(n = 10, method = "pps_brewer", mos = district_enrollment) |>
  add_stage(label = "Schools") |>
    draw(n = 5)

# Add district-level measure of size
schools_frame_agg <- tanzania_schools |>
  summarize(district_enrollment = sum(enrollment),
            m = n(),
            .by = district)

# Execute stage 1 only
selected_districts <- execute(design, schools_frame_agg, stages = 1, seed = 1)

# ... fieldwork ...

schools_frame <- tanzania_schools |>
  mutate(district_enrollment = sum(enrollment),
         m = n(),
         .by = district)

# Execute stage 2
final_sample <- selected_districts |> execute(schools_frame, seed = 2)
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
| `pps_cps`         | Fixed       | Conditional Poisson sampling  |
| `pps_poisson`     | Random      | PPS Poisson (requires `frac`) |
| `pps_sps`         | Fixed       | Sequential Poisson sampling (PRN) |
| `pps_pareto`      | Fixed       | Pareto πPS sampling (PRN)         |
| `pps_multinomial` | Fixed       | PPS with replacement          |
| `pps_chromy`      | Fixed       | PPS with minimum replacement  |

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

## Sample Coordination (PRN)

Permanent random numbers enable coordinated sampling across survey waves. Assign a stable uniform random number to each frame unit, then pass it via `prn`:

```r
frame$prn <- runif(nrow(frame))

# Wave 1: sequential Poisson sampling with PRN
wave1 <- sampling_design() |>
  draw(n = 500, method = "pps_sps", mos = size, prn = prn) |>
  execute(frame, seed = 1)

# Wave 2: same PRN → high overlap (positive coordination)
wave2 <- sampling_design() |>
  draw(n = 500, method = "pps_sps", mos = size, prn = prn) |>
  execute(frame, seed = 2)
```

PRN is supported for `bernoulli`, `pps_poisson`, `pps_sps`, and `pps_pareto`.

## Survey Export

Convert samples to `survey` or `srvyr` objects for analysis:

```r
svy <- as_svydesign(sample)
survey::svymean(~y, svy)

# Exact or high-entropy joint inclusion probabilities
jip <- joint_expectation(sample, frame, stage = 1)
svy_exact <- as_svydesign(sample, pps = survey::ppsmat(jip[[1]]))
```

By default `as_svydesign()` uses Brewer's variance approximation. For
tighter variance estimates, `joint_expectation()` computes pairwise
joint inclusion probabilities from the original frame. The result is
exact for CPS, systematic, and Poisson; for Brewer, SPS, and Pareto it
uses the O(N^2) high-entropy approximation (exact recursive formulas
exist but are O(N^3) and impractical for large frames). See
`?joint_expectation` for the full method-by-method breakdown.

## Panel Partitioning

Partition the sample into rotation groups for panel surveys:

```r
sample <- sampling_design() |>
  stratify_by(region) |>
  draw(n = 200) |>
  execute(niger_eas, seed = 1, panels = 4)

table(sample$.panel)  # ~50 per panel
```

Panels are assigned by systematic interleaving within strata. For
multi-stage designs, panels are assigned at the PSU level and propagated
to all units. Weights reflect the full-sample inclusion probability; for
per-panel analysis, multiply by the number of panels.

## Two-Phase Sampling

Pipe a `tbl_sample` into `execute()` for multi-phase designs:

```r
# Phase 1: large screening sample
phase1 <- sampling_design() |>
  draw(n = 500) |>
  execute(frame, seed = 1)

# Phase 2: subsample from phase 1
phase2 <- sampling_design() |>
  draw(n = 50) |>
  execute(phase1, seed = 2)
```

Weights compound automatically across phases.

## Diagnostics

```r
summary(sample)           # Per-stage allocation tables, weight diagnostics
```

## Included Datasets

Synthetic datasets for learning and testing:

| Dataset | Description | Rows |
|---------|-------------|------|
| `niger_eas` | DHS-style enumeration areas (Niger) | 1,536 |
| `niger_households` | Household data nested in niger_eas (Niger) | 150,789 |
| `uganda_farms` | LSMS-style agricultural frame (Uganda) | 790 |
| `kenya_health` | Health facility frame (Kenya) | 3,098 |
| `tanzania_schools` | School survey frame (Tanzania) | 2,434 |
| `nigeria_business` | Enterprise survey frame (Nigeria) | 11,617 |

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
  execute(frame, seed = 1)
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
  execute(frame, seed = 2)
```

### SAS Rounding Control

```sas
proc surveyselect data=frame method=sys samprate=0.02 seed=42 round=nearest;
  strata State;
run;
```

```r
sampling_design() |>
  stratify_by(State) |>
  draw(frac = 0.02, method = "systematic", round = "nearest") |>
  execute(frame, seed = 3)
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
  execute(frame, seed = 4)
```
