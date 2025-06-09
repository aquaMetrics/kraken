#' Breach
#'
#' Calculates the breach distance from centre of ellipse for each transect.
#'
#' @param data named list of 4 data frames created by the
#'   `probability_non_linear` function. These are:  `data` (survey), `geoDf`
#'   (distances to good),`geoDfBestFit` (best fit distance) to good and
#'   `hexdfOut` (hexagon heat map).
#' @importFrom stats median
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
  set.seed(123)

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
    LatLon <- convert_coordinates(geoDf$Easting, geoDf$Northing)
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
    LatLonBestFit <- convert_coordinates(
      geoDfBestFit$Easting,
      geoDfBestFit$Northing
    )
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
