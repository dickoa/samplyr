#' Sampling Frames
#'
#' @description
#' The samplyr package includes synthetic sampling frames for demonstrating
#' survey sampling designs. These frames use real administrative divisions
#' from African countries. They are designed to illustrate common survey
#' sampling scenarios.
#'
#' @section Datasets:
#' \itemize{
#'   \item [bfa_eas]: LSMS-style EA frame (Burkina Faso, 14,900 EAs)
#'   \item [zwe_eas]: DHS-style EA frame (Zimbabwe, 22,600 EAs)
#'   \item [zwe_households]: Household frame for two-phase sampling (Zimbabwe, 379,326 HH)
#'   \item [ken_enterprises]: Enterprise survey frame (Kenya, 6,823 establishments)
#' }
#'
#' @section Auxiliary Data:
#' \itemize{
#'   \item [bfa_eas_variance]: Stratum variances for Neyman allocation
#'   \item [bfa_eas_cost]: Stratum costs for optimal allocation
#' }
#'
#' @name samplyr-datasets
#' @keywords datasets
NULL


#' Burkina Faso Enumeration Areas (LSMS-style)
#'
#' @description
#' An enumeration area (EA) frame for household surveys, built from
#' WorldPop/GRID3 preEA data, EHCVM 2021 household survey parameters, and
#' COD-AB administrative boundaries. The frame covers 13 regions, 45 provinces,
#' and 346 communes of Burkina Faso.
#'
#' @format A tibble with 14,900 rows and 12 columns:
#' \describe{
#'   \item{ea_id}{Character. Unique enumeration area identifier}
#'   \item{region}{Factor. Region name (13 regions)}
#'   \item{province}{Factor. Province name within region (45 provinces)}
#'   \item{commune}{Factor. Commune name within province (346 communes)}
#'   \item{urban_rural}{Factor. Urban/Rural classification}
#'   \item{population}{Numeric. EA population}
#'   \item{households}{Integer. Number of households in the EA (measure of size for PPS)}
#'   \item{area_km2}{Numeric. EA area in square kilometres}
#'   \item{accessible}{Logical. Whether the EA is in an accessible zone}
#'   \item{dist_road_km}{Numeric. Distance to paved road in km}
#'   \item{food_insecurity_pct}{Numeric. Cadre Harmonise Phase 3+ prevalence}
#'   \item{cost}{Numeric. Survey cost per EA (thousands FCFA)}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Stratified multi-stage cluster sampling
#'   \item PPS (probability proportional to size) sampling using household counts
#'   \item Urban/rural stratification
#'   \item Neyman and optimal allocation using auxiliary variables
#' }
#'
#' The data structure mirrors typical LSMS/household survey sampling frames
#' where enumeration areas are the primary sampling units, selected with
#' probability proportional to the number of households.
#'
#' @seealso
#' [bfa_eas_variance] for Neyman allocation,
#' [bfa_eas_cost] for optimal allocation
#'
#' @examples
#' # Explore the data
#' head(bfa_eas)
#' table(bfa_eas$region)
#' table(bfa_eas$urban_rural)
#'
#' # Stratified PPS cluster sample
#' sampling_design() |>
#'   add_stage(label = "EAs") |>
#'     stratify_by(region, urban_rural) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 3, method = "pps_brewer", mos = households) |>
#'   add_stage(label = "Households") |>
#'     draw(n = 20) |>
#'   execute(bfa_eas, seed = 42)
#'
"bfa_eas"


#' Stratum Variances for Burkina Faso EAs
#'
#' @description
#' Variance of food insecurity prevalence by region, calculated from [bfa_eas].
#' Used for demonstrating Neyman optimal allocation in stratified sampling.
#'
#' @format A tibble with 13 rows and 2 columns:
#' \describe{
#'   \item{region}{Factor. Region name}
#'   \item{var}{Numeric. Variance of food insecurity prevalence within region}
#' }
#'
#' @seealso [bfa_eas], [stratify_by()]
#'
#' @examples
#' # View the variance data
#' bfa_eas_variance
#'
#' # Neyman allocation minimizes variance for fixed sample size
#' sampling_design() |>
#'   stratify_by(region, alloc = "neyman", variance = bfa_eas_variance) |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 42)
#'
"bfa_eas_variance"


#' Stratum Costs for Burkina Faso EAs
#'
#' @description
#' Mean survey cost per EA by region for [bfa_eas]. Conflict-affected regions
#' (Sahel, Est) have higher costs. Used for demonstrating optimal
#' (cost-variance) allocation.
#'
#' @format A tibble with 13 rows and 2 columns:
#' \describe{
#'   \item{region}{Factor. Region name}
#'   \item{cost}{Numeric. Mean per-EA survey cost (thousands FCFA)}
#' }
#'
#' @seealso [bfa_eas], [bfa_eas_variance], [stratify_by()]
#'
#' @examples
#' # View the cost data
#' bfa_eas_cost
#'
#' # Optimal allocation minimizes variance for fixed total cost
#' sampling_design() |>
#'   stratify_by(region, alloc = "optimal",
#'               variance = bfa_eas_variance,
#'               cost = bfa_eas_cost) |>
#'   draw(n = 200) |>
#'   execute(bfa_eas, seed = 42)
#'
"bfa_eas_cost"


#' Zimbabwe Enumeration Areas (DHS Two-Stage Cluster)
#'
#' @description
#' An enumeration area (EA) frame for two-stage cluster surveys, built from
#' WorldPop/GRID3 preEA boundaries, GHS-DUC urban classification, and
#' Zimbabwe 2022 Census population figures. The frame covers 10 provinces
#' and 91 districts.
#'
#' @format A tibble with 22,600 rows and 7 columns:
#' \describe{
#'   \item{ea_id}{Character. Unique enumeration area identifier}
#'   \item{province}{Factor. Province name (10 provinces)}
#'   \item{district}{Factor. District name within province (91 districts)}
#'   \item{urban_rural}{Factor. Urban/Rural classification}
#'   \item{households}{Integer. Number of households in the EA (measure of size for PPS)}
#'   \item{population}{Integer. EA population}
#'   \item{area_km2}{Numeric. EA area in square kilometres}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Two-stage cluster sampling (districts then EAs, or EAs then households)
#'   \item PPS sampling using household counts
#'   \item Stratification by province and urban/rural
#'   \item Partial execution (operational multi-stage sampling)
#'   \item Two-phase sampling (with [zwe_households])
#' }
#'
#' The data structure mirrors typical DHS/MICS sampling frames where EAs
#' are nested within districts and provinces.
#'
#' @seealso [zwe_households] for household-level data within a subset of EAs
#'
#' @examples
#' # Explore the data
#' head(zwe_eas)
#' table(zwe_eas$province)
#' table(zwe_eas$urban_rural)
#'
#' # Two-stage cluster sample: districts then EAs
#' zwe_frame <- zwe_eas |>
#'   dplyr::mutate(district_hh = sum(households), .by = district)
#'
#' sampling_design() |>
#'   add_stage(label = "Districts") |>
#'     stratify_by(province) |>
#'     cluster_by(district) |>
#'     draw(n = 2, method = "pps_brewer", mos = district_hh) |>
#'   add_stage(label = "EAs") |>
#'     draw(n = 5) |>
#'   execute(zwe_frame, seed = 42)
#'
"zwe_eas"


#' Zimbabwe Households (Two-Phase Subframe)
#'
#' @description
#' A household-level frame for a subset of 2,000 enumeration areas from
#' [zwe_eas]. Designed for demonstrating two-phase sampling, where phase 1
#' selects EAs and phase 2 subsamples households within selected EAs.
#'
#' @format A tibble with 379,326 rows and 9 columns:
#' \describe{
#'   \item{hh_id}{Character. Unique household identifier}
#'   \item{ea_id}{Character. Enumeration area identifier (links to [zwe_eas])}
#'   \item{province}{Factor. Province name}
#'   \item{district}{Factor. District name}
#'   \item{urban_rural}{Factor. Urban/Rural classification}
#'   \item{hh_size}{Integer. Number of household members}
#'   \item{n_children}{Integer. Number of children under 5}
#'   \item{wealth_score}{Numeric. Household wealth index score}
#'   \item{has_improved_water}{Logical. Whether the household has improved water access}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Two-phase sampling (EA selection followed by household subsampling)
#'   \item Joint inclusion probability computation
#'   \item Survey export with multi-stage designs
#' }
#'
#' Only a subset of EAs from [zwe_eas] have household-level data, reflecting
#' the operational reality where household listing is done only in selected EAs.
#'
#' @seealso [zwe_eas] for the EA-level frame
#'
#' @examples
#' # Explore the data
#' head(zwe_households)
#' table(zwe_households$province)
#'
"zwe_households"


#' Kenya Enterprises (Enterprise Survey with Panels and PRN)
#'
#' @description
#' A synthetic business establishment frame inspired by the KNBS 2017 Census
#' of Establishments and the World Bank Enterprise Survey (WBES) 2018 Kenya
#' design. Covers 47 counties, 11 regions, 7 sectors, and 3 size classes.
#'
#' @format A tibble with 6,823 rows and 9 columns:
#' \describe{
#'   \item{enterprise_id}{Character. Unique establishment identifier}
#'   \item{county}{Factor. County name (47 counties)}
#'   \item{region}{Factor. Region (11 regions: 10 WBES regions + Rest of Kenya)}
#'   \item{sector}{Factor. Business sector (7 sectors)}
#'   \item{size_class}{Factor. Size classification (Small: 5-19, Medium: 20-99, Large: 100+)}
#'   \item{employees}{Integer. Number of employees (measure of size)}
#'   \item{revenue_millions}{Numeric. Annual revenue in millions KES}
#'   \item{year_established}{Integer. Year the enterprise was established}
#'   \item{exporter}{Logical. Whether the enterprise exports}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Enterprise/business surveys
#'   \item Stratification by sector and size class
#'   \item PPS sampling using employment or revenue
#'   \item Disproportionate sampling (oversampling large enterprises)
#'   \item PRN-based sample coordination across survey waves
#'   \item Panel partitioning with \code{execute(..., panels = k)}
#'   \item Bernoulli and Poisson sampling
#' }
#'
#' @examples
#' # Explore the data
#' head(ken_enterprises)
#' table(ken_enterprises$size_class)
#' table(ken_enterprises$sector)
#'
#' # Stratified sample by sector and size class
#' sampling_design() |>
#'   stratify_by(sector, size_class) |>
#'   draw(n = 3) |>
#'   execute(ken_enterprises, seed = 42)
#'
#' # Disproportionate sampling: oversample large enterprises
#' sampling_design() |>
#'   stratify_by(size_class) |>
#'   draw(frac = c(Small = 0.02, Medium = 0.10, Large = 0.50)) |>
#'   execute(ken_enterprises, seed = 42)
#'
"ken_enterprises"
