#' Sampling Frames
#'
#' @description
#' The samplyr package includes synthetic sampling frames for demonstrating
#' survey sampling designs. These frames use real administrative divisions
#' and population data from African countries. They are designed to illustrate
#' common survey sampling scenarios.
#'
#' @section Datasets:
#' \itemize{
#'   \item [bfa_eas]: Household survey EA frame (Burkina Faso, 44,570 EAs)
#'   \item [zwe_eas]: Two-stage cluster survey EA frame (Zimbabwe, 107,250 EAs)
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


#' Burkina Faso Enumeration Areas
#'
#' @description
#' An enumeration area (EA) frame for household surveys, built from
#' WorldPop/GRID3 preEA boundaries (CC-BY 4.0), EHCVM 2021 household survey
#' parameters, Cadre Harmonise food security analysis, and HDX COD-AB
#' administrative boundaries. Each row corresponds to one preEA polygon
#' from the WorldPop shapefile, providing a 1:1 mapping for spatial joins.
#' The frame covers 13 regions, 45 provinces, and 348 communes of Burkina Faso.
#'
#' @format A tibble with 44,570 rows and 12 columns:
#' \describe{
#'   \item{ea_id}{Integer. Unique enumeration area identifier (matches preEA_EAID in WorldPop shapefile)}
#'   \item{region}{Factor. Region name (13 regions)}
#'   \item{province}{Factor. Province name within region (45 provinces)}
#'   \item{commune}{Factor. Commune name within province (348 communes)}
#'   \item{urban_rural}{Factor. Urban/Rural classification based on commune density}
#'   \item{population}{Integer. EA population from RGPH 2019}
#'   \item{households}{Integer. Number of households, derived from EHCVM 2021 household size parameters}
#'   \item{area_km2}{Numeric. EA area in square kilometres}
#'   \item{accessible}{Logical. Whether the EA is in an accessible zone (conflict-affected regions have lower accessibility)}
#'   \item{dist_road_km}{Numeric. Distance to paved road in km (synthetic, calibrated by milieu)}
#'   \item{food_insecurity_pct}{Numeric. Cadre Harmonise Phase 3+ prevalence, calibrated from Jan-May 2024 province-level analysis}
#'   \item{cost}{Numeric. Survey cost per EA in thousands FCFA (driven by accessibility and distance)}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Stratified multi-stage cluster sampling
#'   \item PPS (probability proportional to size) sampling using household counts
#'   \item Urban/rural stratification
#'   \item Neyman and optimal allocation using auxiliary variables
#'   \item Sampling in conflict-affected contexts with accessibility constraints
#' }
#'
#' The data structure follows typical household survey sampling frames
#' where enumeration areas serve as primary sampling units, selected with
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
#'   execute(bfa_eas, seed = 3)
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
#'   execute(bfa_eas, seed = 2)
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
#'   execute(bfa_eas, seed = 1)
#'
"bfa_eas_cost"


#' Zimbabwe Enumeration Areas
#'
#' @description
#' An enumeration area (EA) frame for two-stage cluster surveys, built from
#' WorldPop/GRID3 preEA boundaries (CC-BY 4.0), GHS-DUC urban classification,
#' WorldPop 2022 constrained 100m age-sex grids, and Zimbabwe 2022 Population
#' Census ward-level tallies. Each row corresponds to one preEA polygon from
#' the WorldPop shapefile, providing a 1:1 mapping for spatial joins. The frame
#' covers 10 provinces and 91 districts.
#'
#' @format A tibble with 107,250 rows and 12 columns:
#' \describe{
#'   \item{ea_id}{Integer. Unique enumeration area identifier (matches preEA_EAID in WorldPop shapefile)}
#'   \item{province}{Factor. Province name (10 provinces)}
#'   \item{district}{Factor. District name within province (91 districts)}
#'   \item{ward_pcode}{Character. Ward P-code from OCHA COD-AB (e.g. "ZW150104")}
#'   \item{urban_rural}{Factor. Urban/Rural classification based on building density, calibrated to GHS-DUC provincial shares}
#'   \item{population}{Integer. EA population, calibrated to 2022 Census ward totals}
#'   \item{households}{Integer. Number of households, calibrated to 2022 Census ward totals}
#'   \item{buildings}{Integer. Building count from GRID3 building footprints}
#'   \item{women_15_49}{Integer. Estimated women aged 15-49, from WorldPop age-sex grids scaled to census population}
#'   \item{men_15_49}{Integer. Estimated men aged 15-49, from WorldPop age-sex grids scaled to census population}
#'   \item{children_under5}{Integer. Estimated children under 5, from WorldPop age-sex grids scaled to census population}
#'   \item{area_km2}{Numeric. EA area in square kilometres}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Two-stage cluster sampling (EAs then households)
#'   \item PPS sampling using household or population counts
#'   \item Stratification by province and urban/rural
#'   \item Partial execution (operational multi-stage sampling)
#'   \item Creating household listings from selected EAs for second-stage sampling
#' }
#'
#' The data structure follows typical two-stage cluster survey frames where
#' EAs are nested within districts and provinces. To create a household
#' listing for second-stage sampling after selecting EAs, expand each
#' selected EA into individual household rows:
#'
#' \preformatted{
#' # After stage 1 selection:
#' listing <- selected[rep(seq_len(nrow(selected)), selected$households), ]
#' listing$hh_id <- seq_len(nrow(listing))
#' }
#'
#' @examples
#' # Explore the data
#' head(zwe_eas)
#' table(zwe_eas$province)
#' table(zwe_eas$urban_rural)
#'
#' # Two-stage cluster sample: EAs then households
#' sampling_design() |>
#'   add_stage(label = "EAs") |>
#'     stratify_by(province, urban_rural) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 3, method = "pps_systematic", mos = households) |>
#'   add_stage(label = "Households") |>
#'     draw(n = 20) |>
#'   execute(zwe_eas, seed = 123)
#'
"zwe_eas"


#' Kenya Enterprises
#'
#' @description
#' A synthetic business establishment frame covering 47 counties, 11 regions,
#' 7 sectors, and 3 size classes. Population structure calibrated to published
#' census of establishments and enterprise survey design parameters.
#'
#' @format A tibble with 6,823 rows and 9 columns:
#' \describe{
#'   \item{enterprise_id}{Character. Unique establishment identifier}
#'   \item{county}{Factor. County name (47 counties)}
#'   \item{region}{Factor. Region (11 regions)}
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
#'   execute(ken_enterprises, seed = 1960)
#'
"ken_enterprises"
