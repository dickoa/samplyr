# ============================================================================
# Data Generation Script for samplyr Package
# ============================================================================
# This script generates synthetic survey datasets for use in examples, tests,
# and vignettes. All datasets use real administrative divisions but contain
# entirely fictional data.
#
# To regenerate datasets, run:
#   source("data-raw/generate_datasets.R")
#
# Required packages: tibble, dplyr, usethis
# ============================================================================

library(tibble)
library(dplyr)

set.seed(2026)

# ============================================================================
# 1. NIGER_EAS: DHS-style Household Survey Frame
# ============================================================================
# A synthetic enumeration area frame inspired by DHS sampling designs.
# Uses real Niger administrative divisions (regions and departments).

niger_eas <- local({
  # Real Niger regions and departments

  admin <- tribble(
    ~region      , ~department      ,
    "Agadez"     , "Agadez"         ,
    "Agadez"     , "Arlit"          ,
    "Agadez"     , "Bilma"          ,
    "Agadez"     , "Tchirozérine"  ,
    "Diffa"      , "Diffa"          ,
    "Diffa"      , "Mainé-Soroa"   ,
    "Diffa"      , "N'Guigmi"       ,
    "Diffa"      , "Bosso"          ,
    "Dosso"      , "Dosso"          ,
    "Dosso"      , "Boboye"         ,
    "Dosso"      , "Dogondoutchi"   ,
    "Dosso"      , "Gaya"           ,
    "Dosso"      , "Loga"           ,
    "Maradi"     , "Maradi"         ,
    "Maradi"     , "Aguié"         ,
    "Maradi"     , "Dakoro"         ,
    "Maradi"     , "Guidan-Roumdji" ,
    "Maradi"     , "Madarounfa"     ,
    "Maradi"     , "Mayahi"         ,
    "Maradi"     , "Tessaoua"       ,
    "Tahoua"     , "Tahoua"         ,
    "Tahoua"     , "Abalak"         ,
    "Tahoua"     , "Birni N'Konni"  ,
    "Tahoua"     , "Bouza"          ,
    "Tahoua"     , "Illéla"        ,
    "Tahoua"     , "Keita"          ,
    "Tahoua"     , "Madaoua"        ,
    "Tahoua"     , "Malbaza"        ,
    "Tahoua"     , "Tchintabaraden" ,
    "Tillabéri" , "Tillabéri"     ,
    "Tillabéri" , "Filingué"      ,
    "Tillabéri" , "Kollo"          ,
    "Tillabéri" , "Ouallam"        ,
    "Tillabéri" , "Say"            ,
    "Tillabéri" , "Téra"          ,
    "Zinder"     , "Zinder"         ,
    "Zinder"     , "Dungass"        ,
    "Zinder"     , "Gouré"         ,
    "Zinder"     , "Magaria"        ,
    "Zinder"     , "Matamèye"      ,
    "Zinder"     , "Mirriah"        ,
    "Zinder"     , "Tanout"         ,
    "Niamey"     , "Niamey I"       ,
    "Niamey"     , "Niamey II"      ,
    "Niamey"     , "Niamey III"     ,
    "Niamey"     , "Niamey IV"      ,
    "Niamey"     , "Niamey V"
  )

  # Population weights by region (approximate relative sizes)
  region_weights <- c(
    "Agadez" = 0.03,
    "Diffa" = 0.04,
    "Dosso" = 0.12,
    "Maradi" = 0.18,
    "Tahoua" = 0.17,
    "Tillabéri" = 0.15,
    "Zinder" = 0.20,
    "Niamey" = 0.11
  )

  # Urban percentage by region
  urban_pct <- c(
    "Agadez" = 0.45,
    "Diffa" = 0.18,
    "Dosso" = 0.12,
    "Maradi" = 0.15,
    "Tahoua" = 0.11,
    "Tillabéri" = 0.09,
    "Zinder" = 0.14,
    "Niamey" = 0.95
  )

  n_total_eas <- 1500

  # Generate EAs per department
  eas_list <- lapply(seq_len(nrow(admin)), function(i) {
    reg <- admin$region[i]
    dept <- admin$department[i]

    # Number of EAs proportional to region population, divided among departments
    n_depts_in_region <- sum(admin$region == reg)
    region_eas <- round(n_total_eas * region_weights[[reg]])
    n_eas <- max(10, round(region_eas / n_depts_in_region * runif(1, 0.7, 1.3)))

    tibble(
      ea_id = sprintf("%s_%02d_%04d", substr(reg, 1, 3), i, seq_len(n_eas)),
      region = reg,
      department = dept,
      strata = factor(
        ifelse(runif(n_eas) < urban_pct[[reg]], "Urban", "Rural"),
        levels = c("Urban", "Rural")
      ),
      hh_count = ifelse(
        strata == "Urban",
        round(rlnorm(n_eas, meanlog = log(150), sdlog = 0.4)),
        round(rlnorm(n_eas, meanlog = log(70), sdlog = 0.5))
      ),
      pop_estimate = hh_count * round(runif(n_eas, 5, 8))
    )
  })

  bind_rows(eas_list) %>%
    mutate(
      region = factor(region),
      department = factor(department)
    ) %>%
    arrange(region, department, ea_id)
})

usethis::use_data(niger_eas, overwrite = TRUE)

# Auxiliary: Variance data for Neyman allocation
niger_eas_variance <- niger_eas %>%
  group_by(region) %>%
  summarise(var = var(hh_count), .groups = "drop")

usethis::use_data(niger_eas_variance, overwrite = TRUE)

# Auxiliary: Cost data for optimal allocation
niger_eas_cost <- tibble(
  region = levels(niger_eas$region),
  cost = c(180, 150, 60, 55, 70, 50, 65, 40) # Remote regions cost more
)

usethis::use_data(niger_eas_cost, overwrite = TRUE)


# ============================================================================
# 1b. NIGER_HOUSEHOLDS: Household-level data for two-stage sampling
# ============================================================================
# Individual household records nested within niger_eas. Enables true two-stage
# cluster sampling demonstrations (select EAs, then select households).

niger_households <- local({
  # Generate households for each EA based on hh_count
  hh_list <- lapply(seq_len(nrow(niger_eas)), function(i) {
    ea <- niger_eas[i, ]
    n_hh <- ea$hh_count

    tibble(
      hh_id = sprintf("%s_HH%04d", ea$ea_id, seq_len(n_hh)),
      ea_id = ea$ea_id,
      region = ea$region,
      department = ea$department,
      strata = ea$strata,
      hh_size = pmax(1, round(rnorm(n_hh, mean = 6, sd = 2.5))),
      head_age = pmin(85, pmax(18, round(rnorm(n_hh, mean = 42, sd = 12)))),
      head_sex = factor(
        sample(c("Male", "Female"), n_hh, replace = TRUE, prob = c(0.75, 0.25)),
        levels = c("Male", "Female")
      ),
      n_children_u5 = rpois(n_hh, lambda = 1.2)
    )
  })

  bind_rows(hh_list) %>%
    mutate(
      region = factor(region, levels = levels(niger_eas$region)),
      department = factor(department, levels = levels(niger_eas$department)),
      strata = factor(strata, levels = levels(niger_eas$strata))
    ) %>%
    arrange(region, department, ea_id, hh_id)
})

usethis::use_data(niger_households, overwrite = TRUE)


# ============================================================================
# 2. UGANDA_FARMS: LSMS-style Agricultural Survey Frame
# ============================================================================
# A synthetic agricultural survey frame inspired by LSMS-ISA designs.
# Uses real Uganda regions and districts.

uganda_farms <- local({
  # Real Uganda regions and selected districts
  admin <- tribble(
    ~region    , ~district     ,
    "Central"  , "Kampala"     ,
    "Central"  , "Wakiso"      ,
    "Central"  , "Mukono"      ,
    "Central"  , "Luweero"     ,
    "Central"  , "Masaka"      ,
    "Central"  , "Mpigi"       ,
    "Central"  , "Mubende"     ,
    "Eastern"  , "Jinja"       ,
    "Eastern"  , "Mbale"       ,
    "Eastern"  , "Soroti"      ,
    "Eastern"  , "Tororo"      ,
    "Eastern"  , "Iganga"      ,
    "Eastern"  , "Kamuli"      ,
    "Eastern"  , "Busia"       ,
    "Northern" , "Gulu"        ,
    "Northern" , "Lira"        ,
    "Northern" , "Arua"        ,
    "Northern" , "Kitgum"      ,
    "Northern" , "Moyo"        ,
    "Northern" , "Nebbi"       ,
    "Western"  , "Mbarara"     ,
    "Western"  , "Kabale"      ,
    "Western"  , "Kasese"      ,
    "Western"  , "Fort Portal" ,
    "Western"  , "Hoima"       ,
    "Western"  , "Masindi"     ,
    "Western"  , "Bushenyi"
  )

  # Regional characteristics
  region_info <- tribble(
    ~region    , ~urban_pct , ~weight , ~main_crops                                    ,
    "Central"  , 0.35       , 0.28    , c("coffee", "maize", "beans", "banana")        ,
    "Eastern"  , 0.15       , 0.26    , c("maize", "millet", "rice", "cotton")         ,
    "Northern" , 0.12       , 0.18    , c("millet", "sorghum", "groundnuts", "sesame") ,
    "Western"  , 0.18       , 0.28    , c("coffee", "banana", "tea", "maize")
  )

  n_total_eas <- 800

  eas_list <- lapply(seq_len(nrow(admin)), function(i) {
    reg <- admin$region[i]
    dist <- admin$district[i]
    reg_info <- region_info %>% filter(region == reg)

    n_dists_in_region <- sum(admin$region == reg)
    region_eas <- round(n_total_eas * reg_info$weight)
    n_eas <- max(8, round(region_eas / n_dists_in_region * runif(1, 0.8, 1.2)))

    tibble(
      ea_id = sprintf("UG_%02d_%03d", i, seq_len(n_eas)),
      region = reg,
      district = dist,
      urban_rural = factor(
        ifelse(runif(n_eas) < reg_info$urban_pct, "Urban", "Rural"),
        levels = c("Urban", "Rural")
      ),
      n_households = round(rlnorm(n_eas, meanlog = log(80), sdlog = 0.4)),
      avg_farm_size_ha = ifelse(
        urban_rural == "Urban",
        round(rlnorm(n_eas, meanlog = log(0.5), sdlog = 0.5), 2),
        round(rlnorm(n_eas, meanlog = log(2.5), sdlog = 0.6), 2)
      ),
      main_crop = sample(reg_info$main_crops[[1]], n_eas, replace = TRUE)
    )
  })

  bind_rows(eas_list) %>%
    mutate(
      region = factor(
        region,
        levels = c("Central", "Eastern", "Northern", "Western")
      ),
      district = factor(district),
      main_crop = factor(main_crop)
    ) %>%
    arrange(region, district, ea_id)
})

usethis::use_data(uganda_farms, overwrite = TRUE)


# ============================================================================
# 3. KENYA_HEALTH: SPA-style Health Facility Survey Frame
# ============================================================================
# A synthetic health facility frame inspired by SPA/SARA surveys.
# Uses real Kenya counties grouped into regions.

kenya_health <- local({
  # Kenya counties grouped by former provinces (regions)
  admin <- tribble(
    ~region         , ~county         ,
    "Central"       , "Kiambu"        ,
    "Central"       , "Kirinyaga"     ,
    "Central"       , "Murang'a"      ,
    "Central"       , "Nyandarua"     ,
    "Central"       , "Nyeri"         ,
    "Coast"         , "Kilifi"        ,
    "Coast"         , "Kwale"         ,
    "Coast"         , "Mombasa"       ,
    "Coast"         , "Taita-Taveta"  ,
    "Coast"         , "Tana River"    ,
    "Coast"         , "Lamu"          ,
    "Eastern"       , "Embu"          ,
    "Eastern"       , "Kitui"         ,
    "Eastern"       , "Machakos"      ,
    "Eastern"       , "Makueni"       ,
    "Eastern"       , "Meru"          ,
    "Eastern"       , "Tharaka-Nithi" ,
    "Nairobi"       , "Nairobi"       ,
    "North Eastern" , "Garissa"       ,
    "North Eastern" , "Mandera"       ,
    "North Eastern" , "Wajir"         ,
    "Nyanza"        , "Homa Bay"      ,
    "Nyanza"        , "Kisii"         ,
    "Nyanza"        , "Kisumu"        ,
    "Nyanza"        , "Migori"        ,
    "Nyanza"        , "Nyamira"       ,
    "Nyanza"        , "Siaya"         ,
    "Rift Valley"   , "Baringo"       ,
    "Rift Valley"   , "Bomet"         ,
    "Rift Valley"   , "Kericho"       ,
    "Rift Valley"   , "Nakuru"        ,
    "Rift Valley"   , "Nandi"         ,
    "Rift Valley"   , "Narok"         ,
    "Rift Valley"   , "Turkana"       ,
    "Rift Valley"   , "Uasin Gishu"   ,
    "Western"       , "Bungoma"       ,
    "Western"       , "Busia"         ,
    "Western"       , "Kakamega"      ,
    "Western"       , "Vihiga"
  )

  # Facility type distribution and characteristics
  facility_types <- tribble(
    ~facility_type        , ~weight , ~avg_beds , ~avg_staff , ~avg_visits ,
    "Referral Hospital"   , 0.01    ,       250 ,        120 ,       15000 ,
    "County Hospital"     , 0.03    ,        80 ,         45 ,        8000 ,
    "Sub-County Hospital" , 0.06    ,        40 ,         25 ,        4000 ,
    "Health Centre"       , 0.15    ,        12 ,         10 ,        2000 ,
    "Dispensary"          , 0.45    ,         2 ,          4 ,         500 ,
    "Clinic"              , 0.25    ,         3 ,          5 ,         800 ,
    "Maternity Home"      , 0.05    ,         6 ,          6 ,         400
  )

  # Regional characteristics
  region_weights <- c(
    "Central" = 0.12,
    "Coast" = 0.10,
    "Eastern" = 0.15,
    "Nairobi" = 0.12,
    "North Eastern" = 0.05,
    "Nyanza" = 0.14,
    "Rift Valley" = 0.20,
    "Western" = 0.12
  )

  urban_pct <- c(
    "Central" = 0.30,
    "Coast" = 0.35,
    "Eastern" = 0.18,
    "Nairobi" = 0.95,
    "North Eastern" = 0.25,
    "Nyanza" = 0.20,
    "Rift Valley" = 0.22,
    "Western" = 0.18
  )

  n_total_facilities <- 3000

  facilities_list <- lapply(seq_len(nrow(admin)), function(i) {
    reg <- admin$region[i]
    county <- admin$county[i]

    n_counties_in_region <- sum(admin$region == reg)
    region_fac <- round(n_total_facilities * region_weights[[reg]])
    n_fac <- max(
      15,
      round(region_fac / n_counties_in_region * runif(1, 0.8, 1.2))
    )

    # Sample facility types
    fac_types <- sample(
      facility_types$facility_type,
      n_fac,
      replace = TRUE,
      prob = facility_types$weight
    )

    tibble(
      facility_id = sprintf("KE_%02d_%04d", i, seq_len(n_fac)),
      region = reg,
      county = county,
      urban_rural = factor(
        ifelse(runif(n_fac) < urban_pct[[reg]], "Urban", "Rural"),
        levels = c("Urban", "Rural")
      ),
      facility_type = fac_types
    ) %>%
      left_join(facility_types, by = "facility_type") %>%
      mutate(
        beds = pmax(0, round(avg_beds * rlnorm(n_fac, 0, 0.4))),
        staff_count = pmax(1, round(avg_staff * rlnorm(n_fac, 0, 0.3))),
        outpatient_visits = pmax(50, round(avg_visits * rlnorm(n_fac, 0, 0.5))),
        ownership = sample(
          c("Public", "Private", "Faith-based", "NGO"),
          n_fac,
          replace = TRUE,
          prob = c(0.50, 0.25, 0.18, 0.07)
        )
      ) %>%
      select(-weight, -avg_beds, -avg_staff, -avg_visits)
  })

  bind_rows(facilities_list) %>%
    mutate(
      region = factor(region),
      county = factor(county),
      facility_type = factor(
        facility_type,
        levels = facility_types$facility_type
      ),
      ownership = factor(ownership)
    ) %>%
    arrange(region, county, facility_id)
})

usethis::use_data(kenya_health, overwrite = TRUE)


# ============================================================================
# 4. TANZANIA_SCHOOLS: Education Survey Frame
# ============================================================================
# A synthetic school survey frame inspired by education census data.
# Uses real Tanzania regions and districts.

tanzania_schools <- local({
  # Real Tanzania regions and selected districts
  admin <- tribble(
    ~region         , ~district         ,
    "Dar es Salaam" , "Ilala"           ,
    "Dar es Salaam" , "Kinondoni"       ,
    "Dar es Salaam" , "Temeke"          ,
    "Arusha"        , "Arusha City"     ,
    "Arusha"        , "Arusha District" ,
    "Arusha"        , "Meru"            ,
    "Arusha"        , "Monduli"         ,
    "Dodoma"        , "Dodoma Urban"    ,
    "Dodoma"        , "Chamwino"        ,
    "Dodoma"        , "Kondoa"          ,
    "Dodoma"        , "Mpwapwa"         ,
    "Kilimanjaro"   , "Moshi Urban"     ,
    "Kilimanjaro"   , "Moshi Rural"     ,
    "Kilimanjaro"   , "Hai"             ,
    "Kilimanjaro"   , "Rombo"           ,
    "Mwanza"        , "Nyamagana"       ,
    "Mwanza"        , "Ilemela"         ,
    "Mwanza"        , "Magu"            ,
    "Mwanza"        , "Sengerema"       ,
    "Tanga"         , "Tanga City"      ,
    "Tanga"         , "Korogwe"         ,
    "Tanga"         , "Lushoto"         ,
    "Tanga"         , "Muheza"          ,
    "Morogoro"      , "Morogoro Urban"  ,
    "Morogoro"      , "Morogoro Rural"  ,
    "Morogoro"      , "Kilombero"       ,
    "Morogoro"      , "Ulanga"
  )

  region_weights <- c(
    "Dar es Salaam" = 0.18,
    "Arusha" = 0.14,
    "Dodoma" = 0.12,
    "Kilimanjaro" = 0.14,
    "Mwanza" = 0.16,
    "Tanga" = 0.13,
    "Morogoro" = 0.13
  )

  urban_pct <- c(
    "Dar es Salaam" = 0.95,
    "Arusha" = 0.40,
    "Dodoma" = 0.30,
    "Kilimanjaro" = 0.25,
    "Mwanza" = 0.35,
    "Tanga" = 0.28,
    "Morogoro" = 0.25
  )

  n_total_schools <- 2500

  schools_list <- lapply(seq_len(nrow(admin)), function(i) {
    reg <- admin$region[i]
    dist <- admin$district[i]

    n_dists_in_region <- sum(admin$region == reg)
    region_schools <- round(n_total_schools * region_weights[[reg]])
    n_schools <- max(
      20,
      round(region_schools / n_dists_in_region * runif(1, 0.8, 1.2))
    )

    # More primary than secondary schools
    school_levels <- sample(
      c("Primary", "Secondary"),
      n_schools,
      replace = TRUE,
      prob = c(0.75, 0.25)
    )

    tibble(
      school_id = sprintf("TZ_%02d_%04d", i, seq_len(n_schools)),
      region = reg,
      district = dist,
      school_level = school_levels,
      ownership = sample(
        c("Government", "Private"),
        n_schools,
        replace = TRUE,
        prob = c(0.80, 0.20)
      ),
      enrollment = ifelse(
        school_levels == "Primary",
        round(rlnorm(n_schools, meanlog = log(400), sdlog = 0.5)),
        round(rlnorm(n_schools, meanlog = log(250), sdlog = 0.6))
      ),
      n_teachers = pmax(3, round(enrollment / runif(n_schools, 35, 50))),
      has_electricity = rbinom(
        n_schools,
        1,
        prob = ifelse(
          grepl("Urban|City", dist),
          0.85,
          0.45
        )
      ),
      has_water = rbinom(
        n_schools,
        1,
        prob = ifelse(
          grepl("Urban|City", dist),
          0.90,
          0.60
        )
      )
    )
  })

  bind_rows(schools_list) %>%
    mutate(
      region = factor(region),
      district = factor(district),
      school_level = factor(school_level, levels = c("Primary", "Secondary")),
      ownership = factor(ownership),
      has_electricity = as.logical(has_electricity),
      has_water = as.logical(has_water)
    ) %>%
    arrange(region, district, school_id)
})

usethis::use_data(tanzania_schools, overwrite = TRUE)


# ============================================================================
# 5. NIGERIA_BUSINESS: Enterprise Survey Frame
# ============================================================================
# A synthetic business establishment frame inspired by World Bank enterprise surveys.
# Uses real Nigeria states and geopolitical zones.

nigeria_business <- local({
  # Nigeria states by geopolitical zone
  admin <- tribble(
    ~zone           , ~state        ,
    "North Central" , "Benue"       ,
    "North Central" , "Kogi"        ,
    "North Central" , "Kwara"       ,
    "North Central" , "Nasarawa"    ,
    "North Central" , "Niger"       ,
    "North Central" , "Plateau"     ,
    "North Central" , "FCT Abuja"   ,
    "North East"    , "Adamawa"     ,
    "North East"    , "Bauchi"      ,
    "North East"    , "Borno"       ,
    "North East"    , "Gombe"       ,
    "North East"    , "Taraba"      ,
    "North East"    , "Yobe"        ,
    "North West"    , "Jigawa"      ,
    "North West"    , "Kaduna"      ,
    "North West"    , "Kano"        ,
    "North West"    , "Katsina"     ,
    "North West"    , "Kebbi"       ,
    "North West"    , "Sokoto"      ,
    "North West"    , "Zamfara"     ,
    "South East"    , "Abia"        ,
    "South East"    , "Anambra"     ,
    "South East"    , "Ebonyi"      ,
    "South East"    , "Enugu"       ,
    "South East"    , "Imo"         ,
    "South South"   , "Akwa Ibom"   ,
    "South South"   , "Bayelsa"     ,
    "South South"   , "Cross River" ,
    "South South"   , "Delta"       ,
    "South South"   , "Edo"         ,
    "South South"   , "Rivers"      ,
    "South West"    , "Ekiti"       ,
    "South West"    , "Lagos"       ,
    "South West"    , "Ogun"        ,
    "South West"    , "Ondo"        ,
    "South West"    , "Osun"        ,
    "South West"    , "Oyo"
  )

  # Zone economic weights (South West/Lagos dominates)
  zone_weights <- c(
    "North Central" = 0.12,
    "North East" = 0.06,
    "North West" = 0.10,
    "South East" = 0.15,
    "South South" = 0.17,
    "South West" = 0.40
  )

  # Business sectors
  sectors <- c(
    "Manufacturing",
    "Retail Trade",
    "Wholesale Trade",
    "Services",
    "Construction",
    "Transport",
    "Hospitality"
  )

  sector_weights <- c(0.12, 0.30, 0.10, 0.22, 0.08, 0.08, 0.10)

  # Size classes
  size_classes <- tribble(
    ~size_class , ~weight , ~emp_min , ~emp_max ,
    "Micro"     , 0.55    ,        1 ,        4 ,
    "Small"     , 0.28    ,        5 ,       19 ,
    "Medium"    , 0.12    ,       20 ,       99 ,
    "Large"     , 0.05    ,      100 ,     2000
  )

  n_total_businesses <- 10000

  businesses_list <- lapply(seq_len(nrow(admin)), function(i) {
    zone <- admin$zone[i]
    state <- admin$state[i]

    # Lagos gets extra weight
    state_multiplier <- ifelse(state == "Lagos", 3, 1)
    n_states_in_zone <- sum(admin$zone == zone)
    zone_biz <- round(n_total_businesses * zone_weights[[zone]])
    n_biz <- max(
      50,
      round(zone_biz / n_states_in_zone * state_multiplier * runif(1, 0.8, 1.2))
    )

    # Sample sectors and size classes
    biz_sectors <- sample(sectors, n_biz, replace = TRUE, prob = sector_weights)
    biz_sizes <- sample(
      size_classes$size_class,
      n_biz,
      replace = TRUE,
      prob = size_classes$weight
    )

    tibble(
      enterprise_id = sprintf("NG_%02d_%05d", i, seq_len(n_biz)),
      zone = zone,
      state = state,
      sector = biz_sectors,
      size_class = biz_sizes
    ) %>%
      left_join(
        size_classes %>% select(size_class, emp_min, emp_max),
        by = "size_class"
      ) %>%
      mutate(
        employees = round(runif(n_biz, emp_min, emp_max)),
        annual_turnover = round(
          employees *
            runif(n_biz, 800000, 3000000) * # Naira per employee
            ifelse(
              sector == "Manufacturing",
              1.5,
              ifelse(sector %in% c("Wholesale Trade", "Retail Trade"), 2, 1)
            ),
          -3 # Round to thousands
        )
      ) %>%
      select(-emp_min, -emp_max)
  })

  bind_rows(businesses_list) %>%
    mutate(
      zone = factor(
        zone,
        levels = c(
          "North Central",
          "North East",
          "North West",
          "South East",
          "South South",
          "South West"
        )
      ),
      state = factor(state),
      sector = factor(sector),
      size_class = factor(
        size_class,
        levels = c("Micro", "Small", "Medium", "Large")
      )
    ) %>%
    arrange(zone, state, enterprise_id)
})

usethis::use_data(nigeria_business, overwrite = TRUE)
