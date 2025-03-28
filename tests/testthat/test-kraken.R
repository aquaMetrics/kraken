test_that("test kraken works", {

  # Need think about Eileen Coltair - 5188. Transect 2 & 1 - have NULLS. What to do?
  # If web app - what would be the best approach?


  # One transect doesn't reach good status - and all values the same!
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[1:9] <- 0.5
  test_all_the_same <- kraken(demo_iqi)
  testthat::expect_equal(test_all_the, TRUE)

  # Negative IQI?
  # Add error message for negative values?

  # One transect doesn't reach good status
  demo_iqi <- kraken::demo_iqi
  demo_iqi$IQI[7:9] <- 0.62
  test_minimal <- kraken(demo_iqi)
  testthat::expect_true(any(test_minimal$response == ">"))
  testthat::expect_true(any(test_minimal$response == "minimal area"))


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
 chem_data <- dplyr::bind_rows(kraken::demo_iqi, kraken::demo_iqi, kraken::demo_iqi)
 # jitter values to be more realistic of replicated results
 chem_data$IQI <- chem_data$IQI * 400
 chem_data$IQI <- jitter(chem_data$IQI, amount = 50)

 test_chem  <- kraken(chem_data, good_moderate = 200)

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
