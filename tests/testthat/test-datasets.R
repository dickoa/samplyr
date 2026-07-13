test_that("bfa_eas is a household budget and living-standards design frame", {
  expect_identical(
    names(bfa_eas),
    c(
      "ea_id", "region", "province", "commune", "urban_rural",
      "population", "households", "area_km2", "pop_density",
      "longitude", "latitude", "remoteness", "fieldwork_cost"
    )
  )
  expect_equal(nrow(bfa_eas), 44570L)
  expect_false(anyNA(bfa_eas))
  expect_type(bfa_eas$population, "integer")
  expect_type(bfa_eas$households, "integer")
  expect_type(bfa_eas$fieldwork_cost, "integer")
  expect_setequal(levels(bfa_eas$remoteness), c("Low", "Medium", "High"))

  expect_true(all(is.finite(bfa_eas$longitude)))
  expect_true(all(is.finite(bfa_eas$latitude)))
  expect_true(all(bfa_eas$longitude >= -6 & bfa_eas$longitude <= 3))
  expect_true(all(bfa_eas$latitude >= 9 & bfa_eas$latitude <= 16))
  expect_true(all(bfa_eas$pop_density > 0))
  expect_true(all(bfa_eas$fieldwork_cost > 0))

  expect_false(any(c(
    "accessible", "dist_road_km", "food_insecurity_pct"
  ) %in% names(bfa_eas)))
})

test_that("bfa_eas companion tables match the regional frame", {
  expect_identical(names(bfa_eas_variance), c("region", "var"))
  expect_identical(names(bfa_eas_cost), c("region", "cost"))
  expect_setequal(as.character(bfa_eas_variance$region), levels(bfa_eas$region))
  expect_setequal(as.character(bfa_eas_cost$region), levels(bfa_eas$region))
  expect_true(all(is.finite(bfa_eas_variance$var)))
  expect_true(all(bfa_eas_variance$var > 0))
  expect_true(all(is.finite(bfa_eas_cost$cost)))
  expect_true(all(bfa_eas_cost$cost > 0))
})

test_that("ken_enterprises is a synthetic establishment survey frame", {
  expect_identical(
    names(ken_enterprises),
    c(
      "enterprise_id", "county", "region", "sector", "size_class",
      "employees", "revenue_millions", "year_established", "exporter"
    )
  )
  expect_equal(nrow(ken_enterprises), 17004L)
  expect_false(anyNA(ken_enterprises))
  expect_equal(length(unique(ken_enterprises$enterprise_id)), 17004L)
  expect_equal(nlevels(ken_enterprises$county), 47L)
  expect_equal(nlevels(ken_enterprises$region), 6L)
  expect_equal(nlevels(ken_enterprises$sector), 7L)
  expect_identical(
    levels(ken_enterprises$size_class),
    c("Small", "Medium", "Large")
  )

  expect_true(all(ken_enterprises$employees > 0L))
  expect_true(all(ken_enterprises$revenue_millions > 0))
  expect_true(all(ken_enterprises$year_established >= 1960L))
  expect_true(all(ken_enterprises$year_established <= 2024L))
  expect_type(ken_enterprises$exporter, "logical")

  small <- ken_enterprises$size_class == "Small"
  medium <- ken_enterprises$size_class == "Medium"
  large <- ken_enterprises$size_class == "Large"
  expect_true(all(ken_enterprises$employees[small] %in% 5:19))
  expect_true(all(ken_enterprises$employees[medium] %in% 20:99))
  expect_true(all(ken_enterprises$employees[large] >= 100L))
})
