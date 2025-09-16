#' Calculate Footprint
#'
#' @param data Input data frame see `demo_iqi`.
#' @param overrideTransect1 overrideTransect1
#' @param overrideTransect2 overrideTransect2
#' @param overrideTransect3 overrideTransect3
#' @param overrideTransect4 overrideTransect4
#' @param overrideBearing1 overrideBearing1
#' @param overrideBearing2 overrideBearing2
#' @param overrideBearing3 overrideBearing3
#' @param overrideBearing4 overrideBearing4
#' @param loess Use loess model instead of fitting multiple different models
#' @param hera_format Flag if import data in 'hera' formats.
#' @param pass_fail Pass-fail compliance boundary value.
#' @param method Type of method used to analyse samples either 'iqi' or
#'   'residue'.
#' @param n_try Number of attempts to fit a model to bootstrap resamples. The
#'   model is fitted to a maximum 50% of attempts otherwise the model is not
#'   included. By default, n_try = 1000 and therefore 500 bootstraps are
#'   returned if model is successfully fitted.
#' @return Data frame contain 8 variables.
#' \describe{
#'  \item{project_id}{Unique ID for survey –  MCFF + Date}
#'  \item{location_id}{Unique Station ID – Farm + Transect + Station}
#'  \item{sample_id}{Uniquely identify sample. Currently Farm + Transect + Station + Date}
#'  \item{date_taken}{Survey date}
#'  \item{question}{One of 30 unique questions that require responses to be calculated to assess
#'  mixing zone compliance. Some questions require responses per station or transect or survey/project:
#'  \itemize{
#'   \item{stationNumber - Character. Indicates if the minimum number of stations 28 have been taken.}
#'   \item{twoConsecutiveStations - Character. Indicates if 2 consecutive stations at Good status are returned.}
#'   \item{breachDistanceBestFit -Numeric. Best fit distance to breach of EQS or EQR in metres.}
#'   \item{breachLongitude - Numeric. Longitude coordinate of breach.}
#'   \item{breachLatitude - Numeric. Latitude coordinate of breach.}
#'   \item{area_95_confidence - Numeric. Mixing zone in metres squared with 95 percent confidence.}
#'   \item{package_version - Numeric. Version of the Kraken package used.}
#'   \item{package_date- DateTime. Compilation date of the Kraken package used.}
#'   \item{Value / IQI - Numeric. Input IQI or Emamectin values, reflecting LOD if `<`.}
#'   \item{Easting - Numeric. Easting coordinate of each station}
#'   \item{Northing - Numeric. Northing coordinate of each station}
#'   \item{MCFF_Transect - Character. Unique identifier for farm and transect.}
#'   \item{Longitude - Numeric. Longitude coordinate of each station}
#'   \item{Latitude - Numeric. Latitude coordinate of each station}
#'   \item{Bearing - Numeric. Bearing in degrees from pen edge to outermost pen.}
#'   \item{Distance - Numeric. Distance of station from pen edge in metres.}
#'   \item{Number of stations per transect}{Numeric. Count of stations per transect.}
#'   \item{WFD status - Character. Water Framework Directive status based on IQI or residue.}
#'   \item{ellipse - Object. Polygon of indicative ellipse, class `st_sfc`.}
#'   \item{polygon - Object. Fitted polygon for development purposes of class `st_sfc`.}
#'   \item{spotfire_ellipse - Object. Spotfire-compatible average ellipse polygon of class `st_sfc`.}
#'   \item{breachPositionEnsemble - Object. Modelled distances to breach e.g., 500 runs.}
#'   \item{hex_df - Object. Hexagon heatmap of bootstrap curves for display in Spotfire.}
#'   \item{map - Object. ggplot map of polygon area and stations.}
#'   \item{model_info - Object. Dataframe list of models used to predict breach position.}
#'   \item{ellipse_warnings - Character. Warning if ellipse couldn't be calculated.}
#'   \item{sign - Character. Greater than value sign if area is minimal area or NA if exact area.}
#'   \item{area_warning - Character. Warning if insufficient transects reach compliant status.}
#'   \item{geoDf - Object. Geographic dataframe of polygon results.}
#'   \item{Median distance to compliance in metres - Object. List of median distances to `Compliance` per transect.}
#'      }
#'    }
#'  \item{response}{The calculated response to the question}
#'  \item{object}{response to the question if the response is an object for
#'  example a list, image or model}
#'  \item{parameter}{The method or procedure used to gathering responses to the
#'  question}
#' }
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr mutate case_when select filter distinct
#' @importFrom magrittr `%>%`
#' @importFrom tidyr pivot_wider
#' @examples
#' \dontrun{
#' probability <- kraken(demo_iqi, loess = TRUE)
#' }
kraken <- function(data,
                   overrideTransect1 = NA,
                   overrideTransect2 = NA,
                   overrideTransect3 = NA,
                   overrideTransect4 = NA,
                   overrideBearing1 = NA,
                   overrideBearing2 = NA,
                   overrideBearing3 = NA,
                   overrideBearing4 = NA,
                   loess = FALSE,
                   hera_format = FALSE,
                   pass_fail = 0.64,
                   method = "iqi",
                   n_try = 1000) {

  if (hera_format == TRUE) {
    # If input data from kraken::survey_import() change back into kraken data
    # 'standard'
    data <- convert_kraken(data)
  }

  # Need unique survey_id
  data$survey_id <- paste0(data$MCFF, "-", as.numeric(data$Survey_date))

  # Average values for each station
  # if(method == "residue") {
  # data <- group_by(data, Transect, Station) %>%
  #   mutate(IQI = mean(IQI)) %>%
  #   ungroup() %>%
  #   select(-Station_id) %>%
  #   distinct()
  # }
  # Loop to run through multiple surveys
  all_output <- purrr::map_df(split(data, data$survey_id), function(data) {

    data <- consecutive_stations(data,
                                 pass_fail = pass_fail,
                                 method = method)

    probs <- probability_non_linear(data$survey_data,
      loess = loess,
      pass_fail = pass_fail,
      method = method,
      n_try = n_try
    )

    overrides <- override(
      probs,
      overrideTransect1,
      overrideTransect2,
      overrideTransect3,
      overrideTransect4,
      overrideBearing1,
      overrideBearing2,
      overrideBearing3,
      overrideBearing4
    )
    breachs <- breach(overrides)
    areas <- area(breachs)
    # Pivot output into long format --------------------------------------------
    output <- convert_hera(method, data, overrides, breachs, areas)

    return(output)
  })
  return(all_output)
}
