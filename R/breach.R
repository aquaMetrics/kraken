#' Breach
#'
#' Calculates the breach distance from centre of ellipse for each transect.
#'
#' @param data named list of 4 data frames created by the
#'   `probability_non_linear` function. These are:  `data` (survey), `geoDf`
#'   (distances to good),`geoDfBestFit` (best fit distance) to good and
#'   `hexdfOut` (hexagon heat map).
#' @importFrom sp CRS spTransform SpatialPoints
#' @importFrom stats median
#' @import rgdal
#' @importFrom rlang .data
#' @importFrom dplyr mutate group_by ungroup
#' @return Named list containing 3 dataframes: `surveyData`,
#'   `breachPositionEnsemble` and `breachPositionBestFit`
#' @export
#'
#' @examples
#' \dontrun{
#' probability <- probability_non_linear(demo_iqi)
#' breach <- breach(probability)
#' }
breach <- function(data) {
  inSurveyData <- data[["data"]]
  geoDf <- data[["geoDf"]]
  geoDfBestFit <- data[["geoDfBestFit"]]

  # Convert E/N to Lat/Lon
  wgs84 <- "+init=epsg:4326"
  bng <-
    "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"
  set.seed(123)

  ConvertCoordinates <- function(easting, northing) {
    out <- cbind(easting, northing)
    mask <- !is.na(easting)
    sp <- spTransform(
      SpatialPoints(
        data.frame(
          as.numeric(easting[mask]),
          as.numeric(northing[mask])
        ),
        proj4string = CRS(bng)
      ),
      CRS(wgs84)
    )
    out[mask, ] <- sp@coords
    out
  }

  breachCoordinatesOut <- data.frame(cbind(
    MCFF = NA,
    MCFF_Transect = NA,
    Transect = NA,
    breachDistance = NA,
    breachLongitude = NA,
    breachLatitude = NA
  ))
  breachCoordinatesBestFitOut <- data.frame(cbind(
    MCFF = NA,
    MCFF_Transect = NA,
    Transect = NA,
    breachDistance = NA,
    breachLongitude = NA,
    breachLatitude = NA
  ))
  outSurveyData <- data.frame(cbind(
    Survey_date = NA,
    MCFF = NA,
    Transect = NA,
    Station = NA,
    IQI = NA,
    Easting = NA,
    Northing = NA,
    MCFF_Transect = NA,
    Longitude = NA,
    Latitude = NA,
    Bearing = NA,
    Distance = NA,
    WFD..status = NA,
    Source = NA
  ))

  if (is.null(inSurveyData) || nrow(inSurveyData) == 0) {
    breachCoordinatesOut <- data.frame(cbind(
      MCFF = NA,
      MCFF_Transect = NA,
      Transect = NA,
      breachDistance = NA,
      breachLongitude = NA,
      breachLatitude = NA
    ))
    breachCoordinatesBestFitOut <- data.frame(cbind(
      MCFF = NA,
      MCFF_Transect = NA,
      Transect = NA,
      breachDistance = NA,
      breachLongitude = NA,
      breachLatitude = NA
    ))
    outSurveyData <- data.frame(cbind(
      Survey_date = NA,
      MCFF = NA,
      Transect = NA,
      Station = NA,
      IQI = NA,
      Easting = NA,
      Northing = NA,
      MCFF_Transect = NA,
      Longitude = NA,
      Latitude = NA,
      Bearing = NA,
      Distance = NA,
      Number..of..stations..per..transect = NA,
      WFD..status = NA,
      Source = NA
    ))
  } else {

    # Distance ----------------------------------------------------------------
    LatLon <- as.data.frame(ConvertCoordinates(geoDf$Easting, geoDf$Northing))
    names(LatLon) <- c("Longitude", "Latitude")
    geoDf <- cbind(geoDf, LatLon)

    # Data prep complete: onto the calc proper
    position <- cbind(as.numeric(geoDf$Longitude), as.numeric(geoDf$Latitude))
    bearing <- as.numeric(geoDf$Bearing)
    breachDistance <- as.numeric(geoDf$D2G)

    # Run calculation
    breachCoordinates <- as.data.frame(geosphere::destPoint(
      position,
      bearing,
      breachDistance
    ))
    colnames(breachCoordinates) <- c(
      "breachLongitude",
      "breachLatitude"
    )
    breachCoordinatesOut <- cbind(
      MCFF = geoDf$MCFF,
      MCFF_Transect = geoDf$MCFF_Transect,
      Transect = geoDf$Transect,
      breachDistance,
      breachCoordinates
    )

    # Best-fit distances ------------------------------------------------------
    LatLonBestFit <- as.data.frame(ConvertCoordinates(
      geoDfBestFit$Easting,
      geoDfBestFit$Northing
    ))
    names(LatLonBestFit) <- c("Longitude", "Latitude")
    geoDfBestFit <- cbind(geoDfBestFit, LatLonBestFit)

    # Data prep complete: onto the calc proper
    positionBestFit <- cbind(
      as.numeric(geoDfBestFit$Longitude),
      as.numeric(geoDfBestFit$Latitude)
    )
    bearingBestFit <- as.numeric(geoDfBestFit$Bearing)
    breachDistanceBestFit <- as.numeric(geoDfBestFit$D2G)

    # Run calculation
    breachCoordinatesBestFit <- as.data.frame(geosphere::destPoint(
      positionBestFit,
      bearingBestFit,
      breachDistanceBestFit
    ))
    colnames(breachCoordinatesBestFit) <- c("breachLongitude", "breachLatitude")
    breachCoordinatesBestFitOut <- cbind(
      MCFF = geoDfBestFit$MCFF,
      MCFF_Transect = geoDfBestFit$MCFF_Transect,
      Transect = geoDfBestFit$Transect,
      breachDistanceBestFit,
      breachCoordinatesBestFit
    )

    # Copy survey data
    outSurveyData <- inSurveyData
  }
  breachCoordinatesOut <- group_by(breachCoordinatesOut, .data$MCFF_Transect)
  breachCoordinatesOut <- mutate(breachCoordinatesOut,
    "Rank" = 1:n(),
    "breachLongitude_50thPercentile" = median(.data$breachLongitude),
    "breachLatitude_50thPercentile" = median(.data$breachLatitude),
    "breachDistance_50thPercentile" = median(.data$breachDistance)
  )
  breachCoordinatesOut <- ungroup(breachCoordinatesOut)

  # Return named list of outputs ----------------------------------------------
  data <- list(
    outSurveyData,
    breachCoordinatesOut,
    breachCoordinatesBestFitOut
  )
  names(data) <- c(
    "surveyData",
    "breachPositionEnsemble",
    "breachPositionBestFit"
  )
  return(data)
}
