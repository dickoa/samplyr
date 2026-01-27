#' Synthetic Survey Sampling Datasets
#'
#' @description
#' The samplyr package includes synthetic survey datasets for demonstrating
#' survey sampling designs. These datasets use real administrative divisions
#' from African countries but contain entirely fictional data. They are
#' designed to illustrate common survey sampling scenarios.
#'
#' @section Datasets:
#' \itemize{
#'   \item [niger_eas]: DHS-style household survey frame (Niger)
#'   \item [uganda_farms]: LSMS-style agricultural survey frame (Uganda)
#'   \item [kenya_health]: SPA-style health facility frame (Kenya)
#'   \item [tanzania_schools]: Education survey frame (Tanzania)
#'   \item [nigeria_business]: Enterprise survey frame (Nigeria)
#' }
#'
#' @section Auxiliary Data:
#' \itemize{
#'   \item [niger_eas_variance]: Stratum variances for Neyman allocation
#'   \item [niger_eas_cost]: Stratum costs for optimal allocation
#' }
#'
#' @name samplyr-datasets
#' @keywords datasets
NULL


#' Niger Enumeration Areas (DHS-style)
#'
#' @description
#' A synthetic enumeration area (EA) frame for household surveys, inspired by
#' Demographic and Health Survey (DHS) sampling designs. Uses real Niger
#' administrative divisions but contains entirely fictional data.
#'
#' @format A tibble with approximately 1,500 rows and 6 columns:
#' \describe{
#'   \item{ea_id}{Character. Unique enumeration area identifier}
#'   \item{region}{Factor. Region name (8 regions: Agadez, Diffa, Dosso, Maradi, Niamey, Tahoua, Tillabéri, Zinder)}
#'   \item{department}{Factor. Department name within region}
#'   \item{strata}{Factor. Urban/Rural stratification}
#'   \item{hh_count}{Integer. Number of households in the EA (measure of size for PPS)}
#'   \item{pop_estimate}{Integer. Estimated population}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Stratified multi-stage cluster sampling
#'   \item PPS (probability proportional to size) sampling using household counts
#'   \item Urban/rural stratification
#'   \item Two-stage designs (EAs then households)
#' }
#'
#' The data structure mirrors typical DHS sampling frames where enumeration
#' areas are the primary sampling units, selected with probability proportional
#' to the number of households.
#'
#' @note
#' This is a synthetic dataset created for demonstration purposes. While it uses
#' real Niger administrative divisions, all data values are fictional.
#'
#' @seealso
#' [niger_eas_variance] for Neyman allocation,
#' [niger_eas_cost] for optimal allocation
#'
#' @examples
#' # Explore the data
#' head(niger_eas)
#' table(niger_eas$region)
#' table(niger_eas$strata)
#'
#' # DHS-style two-stage stratified cluster sample
#' sampling_design() |>
#'   stage(label = "EAs") |>
#'     stratify_by(region, strata) |>
#'     cluster_by(ea_id) |>
#'     draw(n = 3, method = "pps_brewer", mos = hh_count) |>
#'   stage(label = "Households") |>
#'     draw(n = 20) |>
#'   execute(niger_eas, seed = 42)
#'
"niger_eas"


#' Stratum Variances for Niger EAs
#'
#' @description
#' Variance of household counts by region, calculated from [niger_eas].
#' Used for demonstrating Neyman optimal allocation in stratified sampling.
#'
#' @format A tibble with 8 rows and 2 columns:
#' \describe{
#'   \item{region}{Factor. Region name}
#'   \item{var}{Numeric. Variance of household counts within region}
#' }
#'
#' @seealso [niger_eas], [stratify_by()]
#'
#' @examples
#' # View the variance data
#' niger_eas_variance
#'
#' # Neyman allocation minimizes variance for fixed sample size
#' sampling_design() |>
#'   stratify_by(region, alloc = "neyman", variance = niger_eas_variance) |>
#'   draw(n = 200) |>
#'   execute(niger_eas, seed = 42)
#'
"niger_eas_variance"


#' Stratum Costs for Niger EAs
#'
#' @description
#' Per-interview cost by region for [niger_eas]. Remote regions (Agadez, Diffa)
#' have higher costs. Used for demonstrating optimal (cost-variance) allocation.
#'
#' @format A tibble with 8 rows and 2 columns:
#' \describe{
#'   \item{region}{Character. Region name}
#'   \item{cost}{Numeric. Per-interview cost (fictional units)}
#' }
#'
#' @seealso [niger_eas], [niger_eas_variance], [stratify_by()]
#'
#' @examples
#' # View the cost data
#' niger_eas_cost
#'
#' # Optimal allocation minimizes variance for fixed total cost
#' sampling_design() |>
#'   stratify_by(region, alloc = "optimal",
#'               variance = niger_eas_variance,
#'               cost = niger_eas_cost) |>
#'   draw(n = 200) |>
#'   execute(niger_eas, seed = 42)
#'
"niger_eas_cost"


#' Uganda Agricultural Survey Frame (LSMS-style)
#'
#' @description
#' A synthetic agricultural survey frame inspired by Living Standards Measurement
#' Study - Integrated Surveys on Agriculture (LSMS-ISA). Uses real Uganda
#' administrative divisions but contains entirely fictional data.
#'
#' @format A tibble with approximately 800 rows and 7 columns:
#' \describe{
#'   \item{ea_id}{Character. Unique enumeration area identifier}
#'   \item{region}{Factor. Region (Central, Eastern, Northern, Western)}
#'   \item{district}{Factor. District name}
#'   \item{urban_rural}{Factor. Urban/Rural classification}
#'   \item{n_households}{Integer. Number of households in the EA}
#'   \item{avg_farm_size_ha}{Numeric. Average farm size in hectares}
#'   \item{main_crop}{Factor. Predominant crop in the EA}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Agricultural survey sampling
#'   \item Stratification by region and urban/rural
#'   \item Domain estimation (by crop type)
#'   \item Multi-stage sampling for household agricultural surveys
#' }
#'
#' Main crops vary by region reflecting actual Ugandan agriculture:
#' Central (coffee, maize, beans, banana), Eastern (maize, millet, rice, cotton),
#' Northern (millet, sorghum, groundnuts, sesame), Western (coffee, banana, tea, maize).
#'
#' @note
#' This is a synthetic dataset. Administrative divisions are real but all
#' data values are fictional.
#'
#' @examples
#' # Explore the data
#' head(uganda_farms)
#' table(uganda_farms$region, uganda_farms$main_crop)
#'
#' # Stratified cluster sample by region
#' sampling_design() |>
#'   stratify_by(region, alloc = "proportional") |>
#'   cluster_by(ea_id) |>
#'   draw(n = 15) |>
#'   execute(uganda_farms, seed = 42)
#'
"uganda_farms"


#' Kenya Health Facilities (SPA-style)
#'
#' @description
#' A synthetic health facility frame inspired by Service Provision Assessment
#' (SPA) and Service Availability and Readiness Assessment (SARA) surveys.
#' Uses real Kenya counties but contains entirely fictional data.
#'
#' @format A tibble with approximately 3,000 rows and 9 columns:
#' \describe{
#'   \item{facility_id}{Character. Unique facility identifier}
#'   \item{region}{Factor. Former province (8 regions)}
#'   \item{county}{Factor. County name (47 counties)}
#'   \item{urban_rural}{Factor. Urban/Rural classification}
#'   \item{facility_type}{Factor. Type of facility (Referral Hospital, County Hospital, Sub-County Hospital, Health Centre, Dispensary, Clinic, Maternity Home)}
#'   \item{ownership}{Factor. Ownership type (Public, Private, Faith-based, NGO)}
#'   \item{beds}{Integer. Number of inpatient beds}
#'   \item{staff_count}{Integer. Number of health workers}
#'   \item{outpatient_visits}{Integer. Monthly outpatient visits (measure of size)}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Health facility surveys
#'   \item Stratification by facility type and region
#'   \item PPS sampling using patient volume
#'   \item Sampling across different ownership types
#' }
#'
#' Facility types follow the Kenyan health system hierarchy from referral
#' hospitals down to dispensaries and clinics.
#'
#' @note
#' This is a synthetic dataset. Counties and regions are real but all
#' data values are fictional.
#'
#' @examples
#' # Explore the data
#' head(kenya_health)
#' table(kenya_health$facility_type)
#'
#' # Stratified sample by facility type with proportional allocation
#' sampling_design() |>
#'   stratify_by(facility_type, alloc = "proportional") |>
#'   draw(n = 300) |>
#'   execute(kenya_health, seed = 42)
#'
#' # PPS sample using outpatient visits as measure of size
#' sampling_design() |>
#'   draw(n = 100, method = "pps_brewer", mos = outpatient_visits) |>
#'   execute(kenya_health, seed = 42)
#'
"kenya_health"


#' Tanzania Schools Survey Frame
#'
#' @description
#' A synthetic school survey frame inspired by education census and survey data.
#' Uses real Tanzania regions and districts but contains entirely fictional data.
#'
#' @format A tibble with approximately 2,500 rows and 9 columns:
#' \describe{
#'   \item{school_id}{Character. Unique school identifier}
#'   \item{region}{Factor. Region name (7 regions)}
#'   \item{district}{Factor. District name}
#'   \item{school_level}{Factor. Primary or Secondary}
#'   \item{ownership}{Factor. Government or Private}
#'   \item{enrollment}{Integer. Total student enrollment (measure of size)}
#'   \item{n_teachers}{Integer. Number of teachers}
#'   \item{has_electricity}{Logical. Whether school has electricity}
#'   \item{has_water}{Logical. Whether school has water supply}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Education surveys
#'   \item Two-stage sampling (schools then students)
#'   \item PPS sampling using enrollment
#'   \item Stratification by school level and ownership
#' }
#'
#' The dataset reflects typical East African education system characteristics
#' with more primary than secondary schools, and infrastructure varying by
#' urban/rural location.
#'
#' @note
#' This is a synthetic dataset. Regions and districts are real but all
#' data values are fictional.
#'
#' @examples
#' # Explore the data
#' head(tanzania_schools)
#' table(tanzania_schools$school_level, tanzania_schools$ownership)
#'
#' # Two-stage cluster sample: schools then students
#' sampling_design() |>
#'   stage(label = "Schools") |>
#'     stratify_by(school_level) |>
#'     cluster_by(school_id) |>
#'     draw(n = 25, method = "pps_brewer", mos = enrollment) |>
#'   stage(label = "Students") |>
#'     draw(n = 20) |>
#'   execute(tanzania_schools, seed = 42)
#'
"tanzania_schools"


#' Nigeria Business Survey Frame
#'
#' @description
#' A synthetic business establishment frame inspired by World Bank Enterprise
#' Surveys. Uses real Nigeria states and geopolitical zones but contains
#' entirely fictional data.
#'
#' @format A tibble with approximately 10,000 rows and 7 columns:
#' \describe{
#'   \item{enterprise_id}{Character. Unique business identifier}
#'   \item{zone}{Factor. Geopolitical zone (North Central, North East, North West, South East, South South, South West)}
#'   \item{state}{Factor. State name (36 states + FCT)}
#'   \item{sector}{Factor. Business sector (Manufacturing, Retail Trade, Wholesale Trade, Services, Construction, Transport, Hospitality)}
#'   \item{size_class}{Factor. Size classification (Micro: 1-4, Small: 5-19, Medium: 20-99, Large: 100+)}
#'   \item{employees}{Integer. Number of employees (measure of size)}
#'   \item{annual_turnover}{Numeric. Annual turnover in Naira}
#' }
#'
#' @details
#' This dataset is designed for demonstrating:
#' \itemize{
#'   \item Business/enterprise surveys
#'   \item Stratification by sector and size class
#'   \item PPS sampling using employment
#'   \item Geographic stratification by zone/state
#' }
#'
#' The distribution reflects typical business demographics with majority
#' micro/small enterprises, concentrated in South West (especially Lagos).
#'
#' @note
#' This is a synthetic dataset. States and zones are real but all
#' data values are fictional.
#'
#' @examples
#' # Explore the data
#' head(nigeria_business)
#' table(nigeria_business$size_class)
#' table(nigeria_business$sector)
#'
#' # Stratified sample by sector and size class
#' sampling_design() |>
#'   stratify_by(sector, size_class) |>
#'   draw(n = 3) |>
#'   execute(nigeria_business, seed = 42)
#'
#' # Disproportionate sampling: oversample large enterprises
#' sampling_design() |>
#'   stratify_by(size_class) |>
#'   draw(frac = c(Micro = 0.005, Small = 0.02, Medium = 0.10, Large = 0.50)) |>
#'   execute(nigeria_business, seed = 42)
#'
"nigeria_business"
