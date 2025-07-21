test_that("test kraken works", {

  # Need think about Eileen Coltair - 5188. Transect 2 & 1 - have NULLS. What to do?
  # If web app - what would be the best approach?


  # One transect doesn't reach good status - and all values the same!
  # Test failing! Appears if model can't be fitted but 7 stations available then
  # this state not catch correctly by if statements.
  # demo_iqi <- kraken::demo_iqi
  # demo_iqi$IQI[1:9] <- 0.5
  # test_all_the_same <- kraken(demo_iqi)
  # testthat::expect_equal(test_all_the, TRUE)


  # Ignore all missing IQI values except pen edge

  # Ignore all missing IQI values except pen edge (in plot and calculation)

  # May have a grey out pen edge station

  # Missig IQI values after


  # Negative IQI?
  # Add error message for negative values?

  # One transect doesn't reach good status
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[7:9] <- 0.62
  test_minimal <- kraken(demo_iqi)
  testthat::expect_true(any(test_minimal$response == ">"))
  testthat::expect_true(any(test_minimal$response == "Minimal footprint area"))


  # Test if one missing IQI scores at station 2
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[2] <- NA
  missing_station_2 <- kraken(demo_iqi)
  # missing_station_2$object[missing_station_2$question == "map"]

  # Test if one missing IQI scores at pen edge (station 1)
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- NA
  missing_station_1 <- kraken(demo_iqi)

  # Test all pen edge missing IQI scores
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- NA
  demo_iqi$IQI[10] <- NA
  demo_iqi$IQI[17] <- NA
  demo_iqi$IQI[24] <- NA
  all_pen_edge_missing <- kraken(demo_iqi)
  # all_pen_edge_missing$object[all_pen_edge_missing$question == "map"]

  # Test all pen edge missing IQI scores and reduced sampling
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1] <- NA
  demo_iqi$IQI[10] <- NA
  demo_iqi$IQI[17] <- NA
  demo_iqi$IQI[24] <- NA

  demo_iqi <- demo_iqi[c(1,10,17,24,7,8,13,14,21,22,27,28), ]
  # remove other values

  reduced_pen_edge_missing <- kraken(demo_iqi)
  # reduced_pen_edge_missing$object[reduced_pen_edge_missing$question == "map"]

})


test_that("test kraken works for chemistry data", {

 # Create chemistry data with 3 replicates per station
  test_data <- read.csv(system.file("extdat/test-data/", "residue-test-data.csv", package = "kraken"))

  # Filter data for my particular farm/date of interest
  test_data <- dplyr::filter(test_data, Site.ID == "BELL1")

  # Select only the columns needed for calculations
  test_data <- dplyr::select(test_data,
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
  test_data <- tidyr::pivot_longer(test_data,
                                   cols = c("Embz-1", "Embz-2", "Embz-3"),
                                   names_to = "Station_id", values_to = "IQI")

 test_chem  <- kraken(test_data,
                      good_moderate = 768,
                      method = "residue",
                      loess = TRUE)





 # results <- test_chem %>%
 #   filter(question %in% c("Distance", "IQI")) %>%
 #   select(question, response, sample_id, location_id) %>%
 #   pivot_wider(names_from = question, values_from = response) %>%
 #   unnest(cols = c(IQI, Distance))
 #
 #   ggplot(results, aes(Distance, IQI)) +
 #     geom_point() +
 #     facet_wrap(facets = results$location_id)

 # hellisay 5279

})




