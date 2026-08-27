test_that("consecutive_stations works", {
  check <- consecutive_stations(demo_iqi, use_mean_bearing = FALSE)
  testthat::expect_equal(class(check$sample_point_checks), "data.frame")
  testthat::expect_equal(nrow(check$sample_point_checks), 4)
  testthat::expect_equal(ncol(check$sample_point_checks), 5)
  testthat::expect_equal(
    check$sample_point_checks[1, "stationNumber"],
    "Compliant: Min. number of stations have been taken (9)"
  )
  testthat::expect_equal(
    check$sample_point_checks[1, "twoConsecutiveStations"],
    "Compliant: 2 consecutive stations at compliant status are returned"
  )
})

test_that("consecutive_stations calculates columns", {
  test <- consecutive_stations(kraken::demo_iqi, use_mean_bearing = FALSE)
  test <- test$survey_data
  test <- dplyr::select(
    test,
    Survey_date,
    MCFF,
    Transect,
    Station,
    IQI,
    Easting,
    Northing,
    MCFF_Transect,
    Longitude,
    Latitude,
    Bearing,
    Distance,
    `Number of stations per transect`,
    `WFD status`,
    mean_bearing,
    quickBearing
  )
  expected <- read.csv(
    system.file(
      "extdat",
      "test-data/2021-05-26-bellister/survey-iqi-data-gis.csv",
      package = "kraken"
    ),
    check.names = FALSE
  )

  expected$Survey_date <- test$Survey_date

  test <- as.data.frame(test)
  expected <- as.data.frame(expected)

  # Rounding error? very minor rounding issue - only detected in CI(?!)
  test$Bearing <- round(test$Bearing, 2)
  expected$Bearing <- round(expected$Bearing, 2)
  expected <- expected[1:nrow(expected), names(test)]
  testthat::expect_equal(test, expected)
})
