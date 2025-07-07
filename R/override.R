#' Override Distance and Bearing for Transects
#'
#' Sometimes it is desirable to override values for testing or to use inferred
#' values according to guidance where data could not be collected.
#'
#' @param data named list of 4 data frames created by the
#'   `probability_non_linear` function. These are:  `data` (survey), `geoDf`
#'   (distances to good),`geoDfBestFit` (best fit distance) to good and
#'   `hexdfOut` (hexagon heat map).
#' @param overrideTransect1 Optional override distance to good for Transect 1
#' @param overrideTransect2 Optional override distance to good for Transect 2
#' @param overrideTransect3 Optional override distance to good for Transect 3
#' @param overrideTransect4 Optional override distance to good for Transect 4
#' @param overrideBearing1 Optional override bearing Transect 1
#' @param overrideBearing2 Optional override bearing Transect 2
#' @param overrideBearing3 Optional override bearing Transect 3
#' @param overrideBearing4 Optional override bearing Transect 4
#'
#' @return area in meters
#' @export
#' @importFrom purrr map_df
#' @importFrom dplyr bind_rows
#' @importFrom stats complete.cases
#' @examples
#' \dontrun{
#' probability <- probability_non_linear(demo_iqi)
#' probability <- override(probability)
#' }
override <- function(data,
                     overrideTransect1 = NA,
                     overrideTransect2 = NA,
                     overrideTransect3 = NA,
                     overrideTransect4 = NA,
                     overrideBearing1 = NA,
                     overrideBearing2 = NA,
                     overrideBearing3 = NA,
                     overrideBearing4 = NA) {

  # Split out the tables from the input `data` list ---------------------------
  inSurveyData <- data[["data"]]
  geoDf <- data[["geoDf"]]
  geoDfBestFit <- data[["geoDfBestFit"]]
  hexdfOut <- data[["hexdfOut"]]

  # Prepare the input in list to make looping / checking easier ---------------
  transect_overrides <- data.frame("override" = c(
    overrideTransect1,
    overrideTransect2,
    overrideTransect3,
    overrideTransect4
  ))
  transect_overrides$transect <- 1:nrow(transect_overrides)
  transect_overrides$name <- "distance"
  bearing_overrides <- data.frame("override" = c(
    overrideBearing1,
    overrideBearing2,
    overrideBearing3,
    overrideBearing4
  ))

  bearing_overrides$transect <- 1:nrow(bearing_overrides)
  bearing_overrides$name <- "bearing"
  overrides <- bind_rows(transect_overrides, bearing_overrides)
  overrides$id <- paste0(overrides$name, overrides$transect)
  overrides <- overrides[complete.cases(overrides), ]

  # Override values ---------------------------------------------------
  # make sure geoDf$D2G is character
  geoDf$D2G <- as.character(geoDf$D2G)
  geoDf <- map_df(split(geoDf, geoDf$Transect), function(transect) {
    override <- overrides[overrides$transect == unique(transect$Transect), ]
    if (nrow(override) > 0) {
      if (any(override$name %in% "distance")) {
        transect$D2Ghist <-
          as.character(override$override[override$name == "distance"])
        transect$D2G <-
          as.character(override$override[override$name == "distance"])
      }
      if (any(override$name %in% "bearing")) {
        transect$Bearing <-
          as.character(override$override[override$name == "bearing"])
      }
    }
    return(transect)
  })

  # Bestfit override values ---------------------------------------------------
  geoDfBestFit <- map_df(
    split(geoDfBestFit, geoDfBestFit$Transect),
    function(transect) {
      override <- overrides[overrides$transect == transect$Transect, ]
      if (nrow(override) > 0) {
        if (any(override$name %in% "distance")) {
          transect$D2G <-
            as.character(override$override[override$name == "distance"])
        }
        if (any(override$name %in% "bearing")) {
          transect$Bearing <-
            as.character(override$override[override$name == "bearing"])
        }
      }
      return(transect)
    }
  )

  # Return values -------------------------------------------------------------
  data <- list(
    inSurveyData,
    geoDf,
    geoDfBestFit,
    hexdfOut
  )
  names(data) <- c("data", "geoDf", "geoDfBestFit", "hexdfOut")
  return(data)
}
