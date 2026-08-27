#' Consecutive Stations
#'
#' Check if two consecutive stations reaching the correct standard and minimum
#' number of stations have been sampled.
#'
#' @param data Data frame with survey data
#' @param pass_fail Pass-fail boundary value.
#' @param method Type of method used to analyse samples, either "iqi" or
#'   "residue".
#' @param average If replicates sample from a single station, should they be
#'   average or consider separate samples.
#' @param use_mean_bearing Use bearing based on the mean of all the
#'   bearings measured from pen edge to each station.
#' @return A named list of two data frames `sample_point_checks` and
#'   `survey_data`
#' @export
#' @importFrom argosfilter radian
#' @importFrom dplyr mutate group_by ungroup n select
#' @examples
#' \dontrun{
#' stations <- consecutive_stations(demo_iqi)
#' }
consecutive_stations <- function(
  data,
  pass_fail = 0.64,
  method = "iqi",
  average = FALSE,
  use_mean_bearing = TRUE
) {
  # summaryOuput - Survey - Initial checks
  set.seed(123)
  stringsAsFactors <- FALSE
  # Only use column needed for analysis in case name clash with calculated
  # columns. For example if latitude column provide this column is also calculated
  # later in this function
  data <- select(
    data,
    Survey_date,
    MCFF,
    Transect,
    Station,
    Easting,
    Northing,
    IQI
  )
  # If replicate values per station then return average values
  # keep original value
  data$original_iqi <- data$IQI
  data <- data %>%
    group_by(Transect, Station) %>%
    mutate(IQI = mean(IQI))
  data <- ungroup(data)

  if (average == FALSE) {
    data$IQI <- data$original_iqi
  } else {
    data <- select(data, -Station_id, -original_iqi)
    data <- distinct(data)
  }

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
            numberOfStations,
            ")"
          )
      } else {
        stationNumber <-
          paste0(
            "Compliant: Min. number of stations have been taken (",
            numberOfStations,
            ")"
          )
      }

      # Convert E/N to Lat/Lon -------------------------------------------------
      LatLon <- convert_coordinates(
        innerTransect$Easting,
        innerTransect$Northing
      )
      innerTransect <- cbind(innerTransect, LatLon)

      # Diagnose transect bearing using principal component analysis -----------
      # PCA doesn't work if points vary a lot at the pen edge stations of the
      # transect and then become more aligned. Use mean_bearing instead.
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

      # Convert the bearing to positive - this is not needed, a negative value
      # will still work for area calculations but positive values are easier to
      # understand and consistent.
      if (is.na(bestFitBearing) == FALSE && bestFitBearing < 0) {
        bestFitBearing <- bestFitBearing + 360
      }

      # Calculate geometric mean / mean of transect points -----------------
      # Where the bearing line needs to be sensible where dog-legged transects
      # could be make the bearing appear to be wrong. Using the mean gives
      # the most reasonable bearing in most circumstances and makes the distance
      # to good point appear to be in the most correct position

      # Exclude pen edge station - we only want bearing from pen edge to
      # mean of other stations. i.e. if single station beyond the pen edge,
      # that station coord will be the mean.
      coords <- data.frame(
        longitude = innerTransect$Longitude[2:nrow(innerTransect)],
        latitude = innerTransect$Latitude[2:nrow(innerTransect)]
      )

      if (all(is.na(coords$longitude))) {
        mean <- c(innerTransect$Longitude[1], innerTransect$Latitude[1])
      }
      if (nrow(coords) > 1) {
        mean <- geosphere::geomean(coords)
        # Get a bearing on a sphere (great circle) f = 0
      } else {
        mean <- c(innerTransect$Longitude[2], innerTransect$Latitude[2])
      }
      mean_bearing <- geosphere::bearing(
        p1 = c(innerTransect$Longitude[1], innerTransect$Latitude[1]),
        p2 = c(mean[1], mean[2]),
        f = 0
      )
      # Bearing function can create negative bearing. Need to be flipped 360
      # degrees, so all consistent.
      if (is.na(mean_bearing) == FALSE && mean_bearing < 0) {
        mean_bearing <- mean_bearing + 360
      }
      # Calculate quickbearing ------------------------------------------------
      # Naive bearing from pen edge to last station for testing to check for
      # errors
      quickBearing <- geosphere::bearing(
        p1 = c(innerTransect$Longitude[1], innerTransect$Latitude[1]),
        p2 = c(
          innerTransect$Longitude[nrow(innerTransect)],
          innerTransect$Latitude[nrow(innerTransect)]
        ),
        f = 0
      )
      # Bearing function can create negative bearing. Need to be flipped 360
      # degrees, so all consistent.
      if (is.na(quickBearing) == FALSE && quickBearing < 0) {
        quickBearing <- quickBearing + 360
      }

      # use mean value instead of PCA value
      if (use_mean_bearing == TRUE) {
        bestFitBearing <- mean_bearing
      }

      # Calculate distance from beginning --------------------------------------
      sf_points <- as.data.frame(cbind(
        innerTransect$Longitude,
        innerTransect$Latitude
      ))
      sf_points <- sf::st_as_sf(sf_points, coords = c(1, 2), crs = 4326)
      firstPoints <- sf::st_transform(sf_points, crs = 4326)
      firstPoints <- sf::as_Spatial(firstPoints)
      Distances <- 1000 * (sp::spDists(firstPoints, longlat = TRUE)[1, ])

      # Some transects may not have more than one station
      if (length(Distances) > 1 && min(diff(Distances)) < 20) {
        # Only highlights first violation of rule (20m maybe quite common so too
        # many false positive) The variable is never assign to something that is
        # returned so perhaps this was a WIP.
        stationSpacingMsg <-
          "Warning: Minimum station spacing of 20 m violated"
      } else {
        stationSpacingMsg <-
          "All stations are separated by required minimum spacing"
      }
      geoDf <- cbind(
        Bearing = bestFitBearing,
        Distance = Distances,
        mean_bearing = mean_bearing,
        quickBearing = quickBearing
      )

      # Find distance to Good based on 2 consecutive station rule --------------
      # If replicate samples from a single station then use average value of
      # replicates instead
      innerTransectMean <- innerTransect %>%
        group_by(Transect, Station) %>%
        mutate(IQI = mean(IQI))
      innerTransectMean <- ungroup(innerTransectMean)
      innerTransectMean <- select(innerTransectMean, -original_iqi)
      innerTransectMean <- distinct(innerTransectMean)

      if (method == "residue") {
        r <- rle(innerTransectMean$IQI < pass_fail)
      } else {
        r <- rle(innerTransectMean$IQI >= pass_fail)
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
          "Non-compliant: 2 consecutive stations at compliant status not returned"
      } else {
        twoConsecutiveStations <-
          "Compliant: 2 consecutive stations at compliant status are returned"
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
  testOutput <- dplyr::group_by(testOutput, MCFF_Transect)
  testOutput <- dplyr::mutate(
    testOutput,
    "Number of stations per transect" = dplyr::n()
  )
  testOutput <- dplyr::ungroup(testOutput)

  # Calculate class ----------------------------------------------------------
  if (method == "residue") {
    testOutput$`WFD status` <- "unclassifiable"
    testOutput$`WFD status`[testOutput$IQI < pass_fail] <- "Pass"
    testOutput$`WFD status`[testOutput$IQI >= pass_fail] <- "Fail"
  } else {
    testOutput$`WFD status` <- "unclassifiable"
    testOutput$`WFD status`[testOutput$IQI >= 0.75] <- "High"
    testOutput$`WFD status`[testOutput$IQI < 0.75] <- "Good"
    testOutput$`WFD status`[testOutput$IQI < pass_fail] <- "Moderate"
    testOutput$`WFD status`[testOutput$IQI < 0.44] <- "Poor"
    testOutput$`WFD status`[testOutput$IQI < 0.24] <- "Bad"
  }

  # Filter columns to only required columns
  testOutput <- dplyr::select(
    testOutput,
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
  # Remove all IQI that are missing/NA except if at pen edge (station 1) because
  # required for modelled distance calculation.
  testOutput <- dplyr::filter(testOutput, !is.na(IQI) | Station == 1)

  data <- list(summaryOutput, testOutput)
  names(data) <- c("sample_point_checks", "survey_data")
  return(data)
}
