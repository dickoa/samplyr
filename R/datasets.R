#' Sampling Frames
#'
#' @description
#' The samplyr package includes derived and synthetic sampling frames for
#' demonstrating survey sampling designs. The frames represent three common
#' survey settings: household budget and living-standards surveys, demographic,
#' health, and child-indicator household surveys, and establishment surveys.
#'
#' @section Datasets:
#' \itemize{
#'   \item [bfa_eas]: Household budget and living-standards EA frame
#'     (Burkina Faso, 44,570 EAs)
#'   \item [zwe_eas]: Demographic, health, and child-indicator EA frame
#'     (Zimbabwe, 107,250 EAs)
#'   \item [ken_enterprises]: Establishment survey frame (Kenya, 17,004 establishments)
#' }
#'
#' @section Auxiliary Data:
#' \itemize{
#'   \item [bfa_eas_variance]: Illustrative prior consumption variances for
#'     Neyman allocation
#'   \item [bfa_eas_cost]: Relative fieldwork costs for optimal allocation
#' }
#'
#' @name samplyr-datasets
#' @keywords datasets
NULL


#' Burkina Faso Enumeration Areas
#'
#' @description
#' A derived enumeration area (EA) frame for household budget and
#' living-standards surveys. Each row corresponds to one WorldPop/GRID3 preEA
#' polygon, and `ea_id` preserves the source identifier for spatial joins. The
#' frame covers 13 regions, 45 provinces, and 348 communes of Burkina Faso.
#'
#' @format A tibble with 44,570 rows and 13 columns:
#' \describe{
#'   \item{ea_id}{Integer. Unique preEA identifier from the WorldPop source}
#'   \item{region}{Factor. Region name (13 regions)}
#'   \item{province}{Factor. Province name within region (45 provinces)}
#'   \item{commune}{Factor. Commune name within province (348 communes)}
#'   \item{urban_rural}{Factor. Modeled urban/rural classification based on
#'     commune population density}
#'   \item{population}{Integer. WorldPop/GRID3 preEA population estimate,
#'     informed by the 2019 population census}
#'   \item{households}{Integer. Modeled household count, derived from
#'     population and aggregated EHCVM 2021-2022 household-size parameters}
#'   \item{area_km2}{Numeric. EA area in square kilometers}
#'   \item{pop_density}{Numeric. Population per square kilometer, derived from
#'     population and source polygon area}
#'   \item{longitude}{Numeric. Longitude of the preEA point-on-surface in WGS 84}
#'   \item{latitude}{Numeric. Latitude of the preEA point-on-surface in WGS 84}
#'   \item{remoteness}{Factor. Modeled operational class (Low, Medium, High)
#'     for sampling examples; not an official geographic classification}
#'   \item{fieldwork_cost}{Integer. Synthetic relative fieldwork cost index;
#'     not a monetary estimate}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Stratified multi-stage cluster sampling
#'   \item PPS (probability proportional to size) sampling using household counts
#'   \item Urban/rural stratification
#'   \item Neyman and optimal allocation using auxiliary variables
#'   \item Balanced and spatially balanced selection using frame auxiliaries
#'   \item Operational planning with modeled remoteness and relative costs
#' }
#'
#' The data structure follows typical household survey sampling frames
#' where enumeration areas serve as primary sampling units, selected with
#' probability proportional to the number of households. Survey outcomes such
#' as expenditure, poverty, and food security are deliberately not included in
#' the frame: they are observed after selection. `remoteness` and
#' `fieldwork_cost` are synthetic planning variables supplied only for examples.
#'
#' @source
#' \itemize{
#'   \item Qader et al. (2022), *National automatic pre-Enumeration Areas
#'     (preEAs) in Burkina Faso (2019), version 1.0*, WorldPop, University of
#'     Southampton, \doi{10.5258/SOTON/WP00731}. Data licensed CC BY 4.0.
#'   \item Institut National de la Statistique et de la Demographie,
#'     *Enquete Harmonisee sur les Conditions de Vie des Menages 2021-2022*,
#'     reference BFA_2021_EHCVM-2_v01_M,
#'     \url{https://microdata.worldbank.org/catalog/6277}. Used only to derive
#'     aggregated household-size parameters.
#'   \item OCHA Common Operational Dataset for Burkina Faso administrative
#'     boundaries, \url{https://data.humdata.org/dataset/cod-ab-bfa}.
#' }
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
#' # Stratified PPS sample
#' sampling_design() |>
#'   stratify_by(region, urban_rural) |>
#'   draw(n = 3, method = "pps_brewer", mos = households) |>
#'   execute(bfa_eas, seed = 3)
#'
"bfa_eas"


#' Stratum Variances for Burkina Faso EAs
#'
#' @description
#' Illustrative prior variance of natural-log mean household consumption at EA
#' level, modeled as if obtained from an earlier household survey. Used for
#' demonstrating Neyman allocation in stratified sampling; the values are
#' synthetic and are not official estimates for Burkina Faso.
#'
#' @format A tibble with 13 rows and 2 columns:
#' \describe{
#'   \item{region}{Factor. Region name}
#'   \item{var}{Numeric. Prior variance of natural-log mean household
#'     consumption at EA level within region}
#' }
#'
#' @source Generated by the synthetic prior-survey model in the internal
#'   `bfa_eas` build recipe.
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
#' Mean synthetic relative fieldwork cost per EA by region for [bfa_eas]. Used
#' for demonstrating optimal (cost-variance) allocation. The values are a
#' planning index, not monetary estimates or official costs.
#'
#' @format A tibble with 13 rows and 2 columns:
#' \describe{
#'   \item{region}{Factor. Region name}
#'   \item{cost}{Numeric. Mean relative fieldwork cost index per EA}
#' }
#'
#' @source Aggregated from the synthetic `fieldwork_cost` variable in
#'   [bfa_eas].
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


#' Zimbabwe Enumeration Areas for Demographic, Health, and Child-Indicator Surveys
#'
#' @description
#' A derived enumeration area (EA) frame for two-stage demographic, health, and
#' child-indicator household surveys. Each row corresponds to one modeled
#' WorldPop/GRID3 preEA polygon, and `ea_id` preserves the source identifier for
#' spatial joins. Population and household counts are disaggregated from
#' Zimbabwe's 2022 census ward totals. The frame covers 10 provinces and 91
#' districts.
#'
#' @format A tibble with 107,250 rows and 12 columns:
#' \describe{
#'   \item{ea_id}{Integer. Unique preEA identifier from the WorldPop source}
#'   \item{province}{Factor. Province name (10 provinces)}
#'   \item{district}{Factor. District name within province (91 districts)}
#'   \item{ward_pcode}{Character. Ward P-code carried by the source preEA
#'     product (e.g. "ZW150104")}
#'   \item{urban_rural}{Factor. Modeled urban/rural classification based on
#'     building density and calibrated to GHS-DUC provincial shares}
#'   \item{population}{Integer. EA population, calibrated to 2022 Census ward totals}
#'   \item{households}{Integer. Number of households, calibrated to 2022 Census ward totals}
#'   \item{buildings}{Integer. Modeled building count from the WorldPop preEA
#'     product}
#'   \item{women_15_49}{Integer. Estimated women aged 15-49, from WorldPop
#'     age-sex grids scaled to census population}
#'   \item{men_15_49}{Integer. Estimated men aged 15-49, from WorldPop age-sex
#'     grids scaled to census population}
#'   \item{children_under5}{Integer. Estimated children under 5, from WorldPop
#'     age-sex grids scaled to census population}
#'   \item{area_km2}{Numeric. EA area in square kilometers}
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
#' selected EA into individual household rows. The preEAs are modeled
#' building-delimited areas rather than official census EAs, and the resulting
#' counts should not be used as official small-area statistics.
#'
#' \preformatted{
#' # After stage 1 selection:
#' listing <- selected[rep(seq_len(nrow(selected)), selected$households), ]
#' listing$hh_id <- seq_len(nrow(listing))
#' }
#'
#' @source
#' \itemize{
#'   \item Qader, Kuepie, and Tatem (2024), *Automatic national census
#'     pre-Enumeration Areas for Zimbabwe in 2021, version 1.0*, WorldPop,
#'     University of Southampton, \doi{10.5258/SOTON/WP00797}. Data licensed
#'     CC BY 4.0.
#'   \item European Commission Joint Research Centre, GHS-WUP-DUC R2025A,
#'     \url{https://human-settlement.emergency.copernicus.eu/GHSWUPDownload.php?ds=WUPDUC}.
#'   \item Zimbabwe National Statistics Agency, *2022 Population Distribution
#'     by District and Ward*,
#'     \url{https://zimgeoportal.org.zw/datasets/2022-population-distribution-by-district-and-ward/}.
#'   \item WorldPop 2022 constrained 100 m age-sex grids,
#'     \url{https://www.worldpop.org/}.
#' }
#'
#' @seealso [add_stage()], [cluster_by()], [execute()]
#'
#' @examples
#' # Explore the data
#' head(zwe_eas)
#' table(zwe_eas$province)
#' table(zwe_eas$urban_rural)
#'
#' # Two-stage cluster sample: EAs then households
#' design <- sampling_design() |>
#'   add_stage(label = "EAs") |>
#'     stratify_by(province, urban_rural) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 3, method = "pps_systematic", mos = households) |>
#'   add_stage(label = "Households") |>
#'     draw(n = 20)
#'
#' selected <- execute(design, zwe_eas, stages = 1, seed = 123)
#'
#' # listing after fieldwork
#' library(dplyr)
#' listing <- selected |>
#'   slice(rep(seq_len(n()), households)) |>
#'   mutate(hh_id = row_number())
#'
#' # final sample
#' smpl <- execute(design, listing, seed = 1234)
#' smpl
#'
"zwe_eas"


#' Synthetic Kenya Establishment Frame
#'
#' @description
#' A synthetic establishment frame covering 47 counties, 6 regions, 7 sectors,
#' and 3 size classes. Region-by-sector-by-size counts reproduce the Republic
#' of Kenya 2025 World Bank Enterprise Survey universe table (17,004 eligible
#' KRA-registered establishments). County assignments use reproducible
#' synthetic weights within each survey region. Every row, identifier, county
#' allocation, and establishment attribute is synthetic.
#'
#' @format A tibble with 17,004 rows and 9 columns:
#' \describe{
#'   \item{enterprise_id}{Character. Synthetic unique establishment identifier}
#'   \item{county}{Factor. Synthetically allocated county name (47 counties)}
#'   \item{region}{Factor. Region (6 regions: Central, Coast,
#'     East and Northeastern, Nairobi, Nyanza and Western, Rift Valley)}
#'   \item{sector}{Factor. Business sector (7 sectors: Food,
#'     Chemicals & Chemical Products, Other Manufacturing, Construction,
#'     Retail, Hotels and Restaurants, Other Services)}
#'   \item{size_class}{Factor. Size classification (Small: 5-19, Medium: 20-99, Large: 100+)}
#'   \item{employees}{Integer. Simulated number of employees and measure of size}
#'   \item{revenue_millions}{Numeric. Simulated annual revenue in millions of
#'     Kenyan shillings (KES)}
#'   \item{year_established}{Integer. Simulated year of establishment}
#'   \item{exporter}{Logical. Simulated export status}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Establishment surveys
#'   \item Stratification by sector and size class
#'   \item PPS sampling using employment or revenue
#'   \item Disproportionate sampling (oversampling large enterprises)
#'   \item PRN-based sample coordination across survey waves
#'   \item Panel partitioning with \code{execute(..., panels = k)}
#'   \item Bernoulli and Poisson sampling
#' }
#'
#' These records do not represent actual Kenyan establishments and must not be
#' used to produce substantive business statistics. The dataset is intended
#' only for sampling, planning, and coordination examples. In particular,
#' county totals are illustrative and do not reproduce official establishment
#' counts.
#'
#' @source
#' \itemize{
#'   \item World Bank Group, *Republic of Kenya World Bank Enterprise Survey
#'     2025*, reference KEN_2025_WBES_v01_M,
#'     \url{https://microdata.worldbank.org/catalog/8150}.
#' }
#'
#' @seealso [stratify_by()], [draw()], [execute()]
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
