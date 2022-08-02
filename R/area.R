#' Ellipse Area Distribution
#'
#' Calculate the ellipse mixing area distribution and create a ellipse polygon.
#'
#' @param data named list of 3 data frames from `breach` function:
#'   `surveyData`, `breachPositionEnsemble` and `breachPositionBestFit`.
#' @return Named list of objects: `ellipse` (sf object of the area of ellipse),
#'   `fifthPercentileArea` named list containing numeric value in metres of 5%
#'   percentile area, package version and date and `spotfire_ellipse` (TIBCO
#'   Spotfire compatible geometry of ellipse).
#' @export
#' @importFrom purrr map_df
#' @importFrom utils packageDate packageVersion
#' @examples
#' \dontrun{
#' probability <- probability_non_linear(demo_iqi)
#' breach <- breach(probability)
#' area <- area(breach)
#' plot(area[["ellipse"]])
#' area[["fifthPercentileArea"]]
#' }
area <- function(data) {
  # breachPositionEnsemble - Breach positions
  # surveyData - Survey IQI (with GIS)
  # breachPositionBestFit - Breach positions - Best Fit

  surveyData <- data[["surveyData"]]
  breachPositionEnsemble <- data[["breachPositionEnsemble"]]
  breachPositionBestFit <- data[["breachPositionBestFit"]]

  breachPositionEnsemble <- map_df(
    split(
      breachPositionEnsemble,
      breachPositionEnsemble$MCFF_Transect
    ),
    function(x) {
      x$rank <- 1:nrow(x)
      return(x)
    }
  )


  transectCombinations <- unique(breachPositionEnsemble$rank)
  ellipseArea <- data.frame(Area = NULL)
  fifthPercentileAreaDynamic <- rep(NA, length(transectCombinations))
  fifthPercentileArea <- 0

  if (is.null(breachPositionEnsemble) ||
    nrow(breachPositionEnsemble) == 0 ||
    sum(!is.na(breachPositionBestFit$breachDistance)) < 3) {
    ellipseArea <- data.frame(Area = NULL)
    outDf <- data.frame(outDf = NULL)
    ellipseResult <- "Dummy ellipse"
    warning1 <- "Warning: Distance to Good not established for all transects."
    warning2 <- "Error: Insufficient transects reach Good to constrain ellipse."
    fifthPercentileArea <- 0
    fifthPercentileAreaDynamic <- rep(0, length(transectCombinations))
  } else {
    set.seed(123)

    totalNumberOfTransects <- length(unique(surveyData$Transect))
    numberOfBreachTransects <- length(unique(breachPositionEnsemble$Transect))

    warning1 <- "Default"
    if (numberOfBreachTransects < totalNumberOfTransects) {
      warning1 <-
        "Warning: Distance to Good not established for all transects."
    }
    warning2 <- "Default"
    if (numberOfBreachTransects < 3) {
      warning2 <-
        "Error: Insufficient transects reach Good to constrain ellipse."
    }

    for (i in transectCombinations) {
      set.seed(123)
      breachData_i <- breachPositionEnsemble[breachPositionEnsemble$rank == i, ]
      breachPositions_i <- (as.matrix(
        cbind(
          Longitude = breachData_i$breachLongitude,
          Latitude = breachData_i$breachLatitude
        )
      ))
      if (numberOfBreachTransects >= 3) {
        actualEllipse_i <-
          suppressWarnings(
            stats::predict(cluster::ellipsoidhull(breachPositions_i)))
        actualEllipse_i <- as.data.frame(cbind(i, actualEllipse_i))
        names(actualEllipse_i) <- c("Run", "Longitude", "Latitude")
        # You need first to close your polygon
        actualEllipse_i <- dplyr::distinct(actualEllipse_i[, 2:3])
        actualEllipse_i <- rbind(actualEllipse_i[, ], actualEllipse_i[1, ])
        ellipseAsPolygon_i <- sf::st_sf(
          sf::st_sfc(sf::st_polygon(list(as.matrix(actualEllipse_i)))),
          crs = 4326
        )
        # Calculate area
        ellipseArea_i <- data.frame(
          Area = as.numeric(sf::st_area(ellipseAsPolygon_i))
        )
        ellipseArea <- rbind(ellipseArea, ellipseArea_i)
        fifthPercentileAreaDynamic[i] <-
          stats::quantile(as.vector(ellipseArea$Area),
            probs = c(.05)
          )
      }
    }

    fifthPercentileArea <- stats::quantile(as.vector(ellipseArea$Area),
      probs = c(.05)
    )
    newOuterGeometry <- function(inputDf) {

      ## select number of points
      pts <- nrow(inputDf)
      ## setup a raw connection and connection object
      rc <- rawConnection(raw(0), "r+")
      on.exit(close(rc))
      writeBin(as.raw(c(1, 3, 0, 0, 0, 1, 0, 0, 0)), rc)
      writeBin(pts, rc, size = 4)

      for (i in 1:pts) {
        lattitudeCol <- "Latitude"
        longitudeCol <- "Longitude"
        ## long
        writeBin(inputDf[i, ][[longitudeCol]], rc)
        ## lat
        writeBin(inputDf[i, ][[lattitudeCol]], rc)
      }
      rawConnectionValue(rc)
    } ## end of new outer geometry

    if ((numberOfBreachTransects >= 3) &
      (numberOfBreachTransects <- totalNumberOfTransects)) {
      breachPositions_bestFit <- (as.matrix(cbind(
        Longitude = breachPositionBestFit$breachLongitude,
        Latitude = breachPositionBestFit$breachLatitude
      )))
      ellipseResult <- "Actual ellipse"
    } else {
      breachPositions_bestFit <- (as.matrix(cbind(
        Longitude = c(-2, -2.1, -2.2),
        Latitude = c(58, 58.1, 58.2)
      )))
      ellipseResult <- "Dummy ellipse"
    }

    actualEllipse_bestFit <- data.frame(stats::predict(
      suppressWarnings(cluster::ellipsoidhull(breachPositions_bestFit))
    ))
    names(actualEllipse_bestFit) <- c("Longitude", "Latitude")

    # You need first to close your polygon
    actualEllipse_bestFit <- dplyr::distinct(actualEllipse_bestFit)
    actualEllipse_bestFit <- rbind(
      actualEllipse_bestFit[, ],
      actualEllipse_bestFit[1, ]
    )

    outDf <- newOuterGeometry(actualEllipse_bestFit) #
    class(outDf) <- "AsIs"
    attr(outDf, "SpotfireColumnMetaData") <-
      list(
        Description = "",
        ContentType = "application/x-wkb",
        `MapChart.ColumnTypeId` = "Geometry"
      )

    ellipse <- sf::st_sf(
      sf::st_sfc(
        sf::st_polygon(list(as.matrix(actualEllipse_bestFit)))
      ),
      crs = 4326
    )
  }
  names(fifthPercentileArea) <- NULL
  fifthPercentileArea <- list(fifthPercentileArea)
  fifthPercentileArea[[2]] <- packageVersion("kraken")[1]
  fifthPercentileArea[[3]] <- packageDate("kraken")[1]
  names(fifthPercentileArea) <- c("5%", "package version", "package date")
  data <- list(ellipse, fifthPercentileArea, outDf)
  names(data) <- c("ellipse", "fifthPercentileArea", "spotfire_ellipse")
  return(data)
}
