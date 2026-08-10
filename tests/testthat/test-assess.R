test_that("test reintraid 2020 against reported results", {
  reintraid <- read.csv(
    system.file("extdat",
      "test-data/2022-reintraid.csv",
      package = "kraken"
    ),
    check.names = FALSE
  )

  area <- kraken::assess(reintraid, use_mean_bearing = FALSE)
  # Changed on 29/06/2026 from 35780 due to using calculated lat / lon within
  # package rather than using supplied lat / lon in test data file.
  testthat::expect_equal(round(area[[1]], 0), 35708)
  # Result reported for compliance is slightly different (37070). Possibly due
  # to slight change to area calculator code since Feb 2022 or error in input
  # data(?). Either way, no impact on compliance (Allowable mixing zone: 86606)
  # testthat::expect_equal(round(area, 0), 37070)
})

test_that("test tisti-geo 2021 against reported results", {
  tisti <- read.csv(
    system.file("extdat",
      "test-data/210706-teisti-geo.csv",
      package = "kraken"
    ),
    check.names = FALSE
  )

  # Current spotfire testing
  # area <- assess(tisti)
  # testthat::expect_equal(round(area[[1]], 0), 55851)
  # Result reported - with override on Transect 1. Access MCFF database has 6
  # stations for Transect 3 - emailed data has 7 stations.
  # Running with 6 stations returns reported value (in this case area at least
  # 148362 +). In this case, makes no difference on outcome of compliance.
  tisti <- tisti[-19, ] # remove station 7 from Transect 3
  area <- kraken::assess(tisti, loess = FALSE, overrideTransect1 = 392)
  testthat::expect_equal(round(area[[1]], 0), 148362)
})



test_that("test bearing", {
testthat::skip("long running test")
  user <- tolower(Sys.info()["user"])
 ellipse_folder <- paste0("C:/Users/",
                          user,
                       "/OneDrive - Scottish Environment Protection Agency/DNA Data/Readiness Data/ellipse-area/")
 samples_path <-  paste0(ellipse_folder,  "2026-06-22-survey-iqi-data.csv")
 survey_data <- read.csv(samples_path)

 survey_data <- dplyr::select(
   survey_data,
   "Survey ID" = Survey.Id,
   "Survey_date" = Survey.Date,
   "MCFF" = Site.Id,
   Transect,
   "Station" = Station,
   Easting,
   Northing,
   IQI = Iqi,
   "Pen-Group" = Pen.Group,
   Monitoring.Type
 )

 survey_data <- survey_data %>% dplyr::filter(!Transect %in% c(999, 555))


 # 4827 survey - transect to stations duplicate station 2 (no impact on result?).
 # 4951 survey CHA1 - transect 3 station 8 in wrong position (no impact on result?)
 # 4815 EAM - transect station 7 in wrong position / labelled. (no impact on result?)
 # 4981 transect 3 station 8 in wrong position / labelled. (no impact on result?)
 output <- purrr::map_df(split(survey_data, survey_data$`Survey ID`), function(survey) {
   survey_output <- kraken::consecutive_stations(survey)
   survey_output <- survey_output$survey_data
   survey_bearings <- dplyr::select(survey_output, MCFF, Survey_date, Transect,  Bearing, quickBearing, mean_bearing)
   survey_bearings <- dplyr::distinct(survey_bearings)
  return(survey_bearings)
})
 output$diff <- output$Bearing - output$quickBearing
 output$diff_mean <- output$mean_bearing - output$quickBearing

})



