test_that("test kraken works", {
  # Need think about Eileen Coltair - 5188. Transect 2 & 1 - have NULLS. What to do?
  # If web app - what would be the best approach?

  # One transect doesn't reach good status - and all values the same!
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1:6] <- 0.5
  demo_iqi <- demo_iqi[c(1:6, 10:30), ]
  test_all_the_same <- kraken(
    demo_iqi,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )
  warnings <- test_all_the_same$response[grepl(
    "_warning",
    test_all_the_same$question
  )]
  testthat::expect_equal(warnings[2], "Minimal footprint area")

  # Ignore all missing IQI values except pen edge
  # Ignore all missing IQI values except pen edge (in plot and calculation)
  # May have a grey out pen edge station
  # Missing IQI values after pen edge - test two consecutive station rule

  # One transect doesn't reach good status
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[7:9] <- 0.62
  test_minimal <- kraken(
    demo_iqi,
    n_try = 10,
    ,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )
  testthat::expect_true(any(test_minimal$response == ">"))
  testthat::expect_true(any(test_minimal$response == "Minimal footprint area"))

  # Test if one missing IQI scores at station 2
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[2] <- NA
  missing_station_2 <- kraken(
    demo_iqi,
    n_try = 10,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )
  # missing_station_2$object[missing_station_2$question == "map"]

  # Test if one missing IQI scores at pen edge (station 1)
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- NA
  missing_station_1 <- kraken(
    demo_iqi,
    n_try = 10,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )

  # Test all pen edge missing IQI scores
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- NA
  demo_iqi$IQI[10] <- NA
  demo_iqi$IQI[17] <- NA
  demo_iqi$IQI[24] <- NA
  all_pen_edge_missing2 <- kraken(
    demo_iqi,
    n_try = 10,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )
  median_distance <- all_pen_edge_missing2$object[
    all_pen_edge_missing2$question == "Distance to Good (m)"
  ]
  testthat::expect_true(median_distance[[1]][1, 2] == 209)
  all_pen_edge_missing2$object[all_pen_edge_missing2$question == "map"]
  #  Test all pen edge missing IQI scores and one transect with data to fit
  #  model
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- NA
  demo_iqi$IQI[2:6] <- c(0.5, 0.51, 0.53, 0.55, 0.60)
  demo_iqi$IQI[10] <- NA
  demo_iqi$IQI[17] <- NA
  demo_iqi$IQI[24] <- NA
  all_pen_edge_missing <- kraken(
    demo_iqi,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )
  median_distance <- all_pen_edge_missing$object[
    all_pen_edge_missing$question == "Distance to Good (m)"
  ]
  testthat::expect_true(median_distance[[1]][1, 2] == 191)

  # Test all pen edge missing IQI scores and reduced sampling
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- NA
  demo_iqi$IQI[10] <- NA
  demo_iqi$IQI[17] <- NA
  demo_iqi$IQI[24] <- NA
  demo_iqi$IQI[7] <- 0.5
  demo_iqi$IQI[14] <- 0.5
  demo_iqi$IQI[21] <- 0.5
  demo_iqi$IQI[28] <- 0.5

  demo_iqi <- demo_iqi[
    c(1, 7, 8, 9, 10, 14, 15, 16, 17, 21, 22, 23, 24, 28, 29, 30),
  ]
  missing_pen_edge_reduced <- kraken(
    demo_iqi,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )

  # remove other values

  # Test all pen edge stations failing
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- 0.5
  demo_iqi$IQI[10] <- 0.5
  demo_iqi$IQI[17] <- 0.5
  demo_iqi$IQI[24] <- 0.5
  all_pen_edge_failing <- kraken(
    demo_iqi[c(1, 10:30), ],
    n_try = 10,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )

  # Test only failing pen edge stations
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- 0.5
  demo_iqi$IQI[10] <- 0.5
  demo_iqi$IQI[17] <- 0.5
  demo_iqi$IQI[24] <- 0.5
  only_faling_pen_edge <- kraken(
    demo_iqi[c(1, 10, 17, 24), ],
    n_try = 10,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )
  # If less than 7 stations is a model fitted? No.

  # Test if two consecutive stations fails if NA value in station between them?
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- 0.5
  demo_iqi$IQI[2] <- 0.64
  demo_iqi$IQI[3] <- NA
  demo_iqi$IQI[4] <- 0.64
  demo_iqi$IQI[5] <- 0.64
  demo_iqi$IQI[c(6:9)] <- NA
  only_faling_pen_edge <- kraken(
    demo_iqi,
    n_try = 10,
    ellipse_representative = FALSE,
    use_mean_bearing = FALSE
  )
  model <- only_faling_pen_edge$object[
    only_faling_pen_edge$question == "model_info"
  ][[1]]
  testthat::expect_equal(model$reducedSamplingD2G[1], "60.8483289180838")
})

test_that("test kraken override works", {
  # check override works for proxy transects
  demo <- read.csv(
    system.file("extdat", "test-data/2022-reintraid.csv", package = "kraken"),
    check.names = FALSE
  )
  override_check <- kraken(
    demo,
    overrideTransect4 = 200,
    overrideBearing4 = 300,
    loess = TRUE,
    n_try = 20
  )
  area <- override_check$response[
    override_check$question == "area_95_confidence"
  ]
  testthat::expect_equal(round(as.numeric(area), 2), 52694.76)
  distance_to_good <- override_check$object[
    override_check$question == "Distance to Good (m)"
  ][[1]]
  testthat::expect_equal(as.integer(distance_to_good[4, 2]), 200)
})


test_that("test kraken works for chemistry data", {
  # Create chemistry data with 3 replicates per station
  test_data <- read.csv(system.file(
    "extdat/test-data/",
    "residue-test-data.csv",
    package = "kraken"
  ))

  # Filter data for my particular farm/date of interest
  test_data <- dplyr::filter(test_data, Site.ID == "BELL1")

  # Select only the columns needed for calculations
  test_data <- dplyr::select(
    test_data,
    "Survey_date" = Survey.Date,
    "MCFF" = Site.ID,
    Transect,
    "Station" = Station.Order..transect.,
    Easting,
    Northing,
    "Embz-1" = EmBz.residues..Rep.1...ng.kg.,
    "Embz-2" = EmBz.residues..Rep.2..ng.kg.,
    "Embz-3" = EmBz.residues..Rep.3..ng.kg.
  )

  # Pivot the data into structure require for calculations
  test_data <- tidyr::pivot_longer(
    test_data,
    cols = c("Embz-1", "Embz-2", "Embz-3"),
    names_to = "Station_id",
    values_to = "IQI"
  )

  test_chem <- kraken(
    test_data,
    pass_fail = 768,
    method = "residue",
    loess = TRUE
  )
  area <- test_chem$response[
    test_chem$question == "area_95_confidence"
  ]
  testthat::expect_equal(round(as.numeric(area), 2), 39782.25)
})
