#' Consecutive Stations
#'
#' Check if two consecutive stations are at Good status and minimum number of
#' stations have been sampled.
#'
#' @param data Data frame with survey data
#' @param good_moderate The EQR ratio for Good - Moderate boundary.
#' @param method Type of method used to analyse samples, either "iqi" or
#'   "residue".
#' @return A named list of two data frames `sample_point_checks` and
#'   `survey_data`
#' @export
#' @importFrom argosfilter radian
#' @importFrom dplyr mutate group_by ungroup n select
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' stations <- consecutive_stations(demo_iqi)
#' }
consecutive_stations <- function(data, good_moderate = 0.64, method = "iqi") {

  # summaryOuput - Survey - Initial checks
  set.seed(123)
  stringsAsFactors <- FALSE
  if (length(unique(data$MCFF)) > 1) {
    testOutput <- data.frame(cbind(
      Survey_date = NA,
      MCFF = NA,
      Transect = NA,
      Station = NA,
      IQI = NA,
      Easting = NA,
      Northing = NA,
      Bearing = NA,
      Distance = NA
    ))
    summaryOutput <- data.frame(cbind(
      MCFF = NA,
      MCFF_Transect = NA,
      Transect = NA,
      stationNumber = NA,
      twoConsecutiveStations = NA,
      withinRangeMsg = NA,
      stationSpacingMsg = NA
    ))
  } else {
    # Create variable for MCFF-Transect
    data$MCFF_Transect <- (paste0(data$MCFF, " - ", data$Transect))
    combs <- unique(data$MCFF_Transect)

    for (i in combs) {
      innerTransect <- data[data$MCFF_Transect == i, ]
      innerTransect <- innerTransect[order(innerTransect$Station), ]

      # Check if 7 stations taken ----------------------------------------------
      numberOfStations <- length(innerTransect$IQI)
      if (numberOfStations < 7) {
        stationNumber <-
          paste0(
            "Non-compliant: Min. number of stations not taken (",
            numberOfStations, ")"
          )
      } else {
        stationNumber <-
          paste0(
            "Compliant: Min. number of stations have been taken (",
            numberOfStations, ")"
          )
      }

      # Convert E/N to Lat/Lon -------------------------------------------------
      LatLon <- convert_coordinates(
        innerTransect$Easting,
        innerTransect$Northing
      )
      innerTransect <- cbind(innerTransect, LatLon)

      # Diagnose transect bearing using principal component analysis -----------
      rlat <- argosfilter::radian(innerTransect$Latitude)
      rlon <- argosfilter::radian(innerTransect$Longitude)
      correctedLatLon <- data.frame(cbind(rlon, rlat))
      names(correctedLatLon) <- c("rLon", "rLat")
      r <- stats::prcomp(~ correctedLatLon$rLon + correctedLatLon$rLat)
      slope <- r$rotation[2, 1] / r$rotation[1, 1]
      intercept <- r$center[2] - slope * r$center[1]
      modelledLongitude <- correctedLatLon$rLon
      modelledLatitude <- slope * modelledLongitude + intercept
      modelledLineRad <- data.frame(cbind(
        Longitude = modelledLongitude,
        Latitude = modelledLatitude
      ))
      modelledLineRad
      modelledLineLongitudeDeg <- (180 / pi) * modelledLineRad$Longitude
      modelledLineLatitudeDeg <- (180 / pi) * modelledLineRad$Latitude
      modelledLineRad2Deg <- data.frame(
        cbind(
          Latitude = modelledLineLatitudeDeg,
          Longitude = modelledLineLongitudeDeg
        )
      )

      bestFitBearing <- argosfilter::bearing(
       modelledLineRad2Deg$Latitude[1],
       modelledLineRad2Deg$Latitude[length(modelledLineRad2Deg$Latitude)],
       modelledLineRad2Deg$Longitude[1],
       modelledLineRad2Deg$Longitude[length(modelledLineRad2Deg$Longitude)]
      )

      if ((bestFitBearing < 0) & (is.na(bestFitBearing) <- FALSE)) {
        bestFitBearing <- bestFitBearing + 360
      }

      # Calculate distance from beginning --------------------------------------
      sf_points <- as.data.frame(cbind(innerTransect$Longitude,
                                    innerTransect$Latitude))
      sf_points <- sf::st_as_sf(sf_points, coords = c(1, 2), crs = 4326)
      firstPoints <- sf::st_transform(sf_points, crs = 4326)
      firstPoints <- sf::as_Spatial(firstPoints)
      Distances <- 1000 * (sp::spDists(firstPoints, longlat = TRUE)[1, ])


      if (min(diff(Distances)) < 20) {
        stationSpacingMsg <-
          "Warning: Minimum station spacing of 20 m violated"
      } else {
        stationSpacingMsg <-
          "All stations are separated by required minimum spacing"
      }
      geoDf <- cbind(Bearing = bestFitBearing, Distance = Distances)

      # Find distance to Good based on 2 consecutive station rule --------------
      if(method == "residue") {
      r <- rle(innerTransect$IQI < good_moderate)
      } else {
      r <- rle(innerTransect$IQI >= good_moderate)
      }
      reducedSamplingD2G <- NA
      s <- NULL
      for (j in 1:length(r$values)) {
        s_j <- (rep(r$values[j], r$lengths[j]))
        s <- c(s, s_j)
      }
      s <- as.numeric(s)
      summed <- NULL
      for (j in 1:length(s)) {
        summed[j] <- s[j] + s[j + 1]
      }

      row_index <- which(summed == 2, arr.ind = TRUE)[1]
      if (is.na(row_index) == FALSE) {
        reducedSamplingD2G <- geoDf[row_index, 2]
      }

      # Have 2 consecutive Good stations been taken ----------------------------
      if (is.na(reducedSamplingD2G) == TRUE) {
        twoConsecutiveStations <-
          "Non-compliant: 2 consecutive stations at Good not returned"
      } else {
        twoConsecutiveStations <-
          "Compliant: 2 consecutive stations at Good are returned"
      }

      # Assemble summary table
      if (exists("summaryOutput") == FALSE) {
        summaryOutput <- data.frame(cbind(
          MCFF = unique(innerTransect$MCFF),
          MCFF_Transect = unique(innerTransect$MCFF_Transect),
          Transect = unique(innerTransect$Transect),
          stationNumber = stationNumber,
          twoConsecutiveStations = twoConsecutiveStations
        ))
      } else {
        summaryOutput <- rbind(
          summaryOutput,
          data.frame(cbind(
            MCFF = unique(innerTransect$MCFF),
            MCFF_Transect = unique(innerTransect$MCFF_Transect),
            Transect = unique(innerTransect$Transect),
            stationNumber = stationNumber,
            twoConsecutiveStations = twoConsecutiveStations
          ))
        )
      }

      # Assemble output table
      if (exists("testOutput") == FALSE) {
        testOutput <- data.frame(cbind(innerTransect, geoDf))
      } else {
        testOutput <- rbind(
          testOutput,
          data.frame(cbind(innerTransect, geoDf))
        )
      }
    } # End of outer loop combs
  }


  # Calculate number of stations per transect -------------------------------
  testOutput <- dplyr::group_by(testOutput, .data$MCFF_Transect)
  testOutput <- dplyr::mutate(testOutput, "Number of stations per transect" = dplyr::n())
  testOutput <- dplyr::ungroup(testOutput)

  # Calculate class ----------------------------------------------------------
  if(method == "residue") {
    testOutput$`WFD status` <- "unclassifiable"
    testOutput$`WFD status`[testOutput$IQI < good_moderate] <- "Pass"
    testOutput$`WFD status`[testOutput$IQI >= good_moderate] <- "Fail"
  } else {

  testOutput$`WFD status` <- "unclassifiable"
  testOutput$`WFD status`[testOutput$IQI >= 0.75] <- "High"
  testOutput$`WFD status`[testOutput$IQI < 0.75] <- "Good"
  testOutput$`WFD status`[testOutput$IQI < good_moderate] <- "Moderate"
  testOutput$`WFD status`[testOutput$IQI < 0.44] <- "Poor"
  testOutput$`WFD status`[testOutput$IQI < 0.24] <- "Bad"
  }
  # Filter columns to only required columns
  testOutput <- dplyr::select(
    testOutput,
    .data$Survey_date,
    .data$MCFF,
    .data$Transect,
    .data$Station,
    .data$IQI,
    .data$Easting,
    .data$Northing,
    .data$MCFF_Transect,
    .data$Longitude,
    .data$Latitude,
    .data$Bearing,
    .data$Distance,
    .data$`Number of stations per transect`,
    .data$`WFD status`
  )

  # Remove all IQI that are missing/NA.
  testOutput <- testOutput[!is.na(testOutput$IQI), ]

  # For each transect, if cage edge (0m) station is NA (not sampled), then
  # remove cage edge station
  # testOutput <- purrr::map_df(unique(testOutput$Transect), function(transect) {
  #   transect_data <- testOutput[testOutput$Transect == transect, ]
  #   transect_data <- dplyr::arrange(transect_data, Distance)
  #   if(is.na(transect_data$IQI[1])) {
  #     transect_data <- transect_data[2:nrow(transect_data), ]
  #   }
  #   return(transect_data)
  # })
  data <- list(summaryOutput, testOutput)
  names(data) <- c("sample_point_checks", "survey_data")
  return(data)
}
