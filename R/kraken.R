#' Calculate Footprint
#'
#' @param data
#' @param overrideTransect1
#' @param overrideTransect2
#' @param overrideTransect3
#' @param overrideTransect4
#' @param overrideBearing1
#' @param overrideBearing2
#' @param overrideBearing3
#' @param overrideBearing4
#' @param loess
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' probability <- kraken(demo_iqi)
#' }
kraken <-  function(data,
                    overrideTransect1 = NA,
                    overrideTransect2 = NA,
                    overrideTransect3 = NA,
                    overrideTransect4 = NA,
                    overrideBearing1 = NA,
                    overrideBearing2 = NA,
                    overrideBearing3 = NA,
                    overrideBearing4 = NA,
                    loess = FALSE) {
  data <- consecutive_stations(data)
  probs <- probability_non_linear(data$survey_data, loess = loess)
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

  breach <- tidyr::pivot_longer(breachs$breachPositionBestFit, cols = c(
    "breachDistanceBestFit",
    "breachLongitude",
    "breachLatitude"
  ),  values_to = "response",
  names_to = "question")
  breach$response <- as.character(breach$response)

  polygons <- tibble(
  question = c("ellipse", "polygon"),
  response = c("object", "object"),
  object = c(areas$ellipse, areas$polygon)
  )
  area <- data.frame(areas$fifthPercentileArea)
  names(area) <- c("area_95_confidence", "package_version", "package_date")
  area <- mutate_all(area , as.character)
  area <- tidyr::pivot_longer(area , cols = everything(),
                              values_to = "response",
                              names_to = "question"
  )

  data$survey_data <- mutate_all(data$survey_data, as.character)
  survey_data <- tidyr::pivot_longer(data$survey_data,
                              cols = c(-Transect,
                                       -Station,
                                       -MCFF,
                                       -Survey_date),
                              values_to = "response",
                              names_to = "question"
                              )

  warning <- tidyr::pivot_longer(data$sample_point_checks ,
                                  cols = c("stationNumber",
                                           "twoConsecutiveStations"),
                                  values_to = "response",
                                  names_to = "question"
                                  )
  output <- bind_rows(warning, breach, area, survey_data, polygons)
  output$Survey_date <- as.Date(output$Survey_date)
  output <- mutate(output,
    sample_id = paste0(Transect, Station, MCFF, as.numeric(Survey_date)),
    project_id = paste0(MCFF, as.numeric(Survey_date)),
    location_id = paste0(MCFF, Transect, Station)
  )
  output <- select(output,
    project_id,
    location_id,
    sample_id,
    "date_taken" = Survey_date,
    "parameter" = "benthic survey",
    question,
    response,
    object
  )

  return(output)
}
