#' Convert to hera
#'
#' Convert kraken output into 'hera' format which is a long format tibble
#' dataframe
#' @param method Type of method used to analyse samples either 'iqi' or
#'   'residue'.
#' @param data dataframe returned from `consecutive_stations()`
#' @param overrides dataframe returned from  `override()`
#' @param breachs dataframe returned from  `breach()`
#' @param areas dataframe returned from `area()`
#'
#' @return dataframe in 'hera' format
#' @export
#'
#' @examples
convert_hera <- function(method, data, overrides, breachs, areas) {
  breach <- tidyr::pivot_longer(breachs$breachPositionBestFit,
    cols = c(
      "breachDistanceBestFit",
      "breachLongitude",
      "breachLatitude"
    ), values_to = "response",
    names_to = "question"
  )
  breach$response <- as.character(breach$response)

  breachPositionEnsemble <- tibble::tibble(
    object = list(breachs$breachPositionEnsemble),
    response = NA,
    question = "breachPositionEnsemble"
  )

  polygons <- tibble::tibble(
    question = c("ellipse", "polygon", "spotfire_polygon"),
    response = c("object", "object", "object"),
    object = list(c(areas$ellipse, areas$polygon, areas$spotfire_ellipse))
  )
  area <- data.frame(areas$fifthPercentileArea)
  names(area) <- c("area_95_confidence", "package_version", "package_date")
  area <- dplyr::mutate_all(area, as.character)
  area <- tidyr::pivot_longer(area,
    cols = dplyr::everything(),
    values_to = "response",
    names_to = "question"
  )

  data$survey_data <- dplyr::mutate_all(data$survey_data, as.character)
  survey_data <- tidyr::pivot_longer(data$survey_data,
    cols = c(
      -.data$Transect,
      -.data$Station,
      -.data$MCFF,
      -.data$Survey_date
    ),
    values_to = "response",
    names_to = "question"
  )

  warning <- tidyr::pivot_longer(breachs$surveyData,
    cols = c(
      "stationNumber",
      "twoConsecutiveStations"
    ),
    values_to = "response",
    names_to = "question"
  )

  map <- create_map(data = data, areas = areas, method = method)
  map <- tibble::tibble(
    "question" = "map",
    "response" = "object",
    "object" = list(map)
  )

  context_warnings <- breachs$surveyData
  context_warning <-
    tibble::tibble(
      "question" = c("sign", "area_warning"),
      "response" = c(
        unique(context_warnings$sign),
        unique(context_warnings$type)
      )
    )

  geo_df <- tibble::tibble(
    "question" = "geoDf",
    "response" = "object",
    "object" = list(overrides$geoDf)
  )
  probs <- tibble::tibble(
    "question" = "model_info",
    "response" = "object",
    "object" = list(overrides$data)
  )

  hex_df <- tibble::tibble(
    "question" = "hex_df",
    "response" = "object",
    "object" = list(overrides$hexdfOut)
  )


  distance_to_good <- dplyr::group_by(overrides$geoDf, .data$Transect)
  distance_to_good <- dplyr::summarise(distance_to_good,
    "Median distance to Good (m)" =
      as.integer(
        round(
          median(
            as.numeric(.data$`D2Ghist`)
          )
        )
      )
  )
  distance_to_good <- tibble::tibble(
    "question" = "Median distance to Good (m)",
    "response" = NA,
    "object" = list(distance_to_good)
  )
  warnings <- tibble::tibble(
    "question" = "ellipse_warnings",
    "response" = NA,
    "object" = list(area$warnings)
  )

  output <- dplyr::bind_rows(
    warning,
    breach,
    area,
    survey_data,
    polygons,
    breachPositionEnsemble,
    hex_df,
    map,
    probs,
    warnings,
    context_warning,
    geo_df,
    distance_to_good
  )

  output$Survey_date <- as.Date(output$Survey_date)

  project_id <- paste0(data$survey_data$MCFF[1], data$survey_data$Survey_date[1])
  Survey_date <- data$survey_data$Survey_date[1]
  output <- dplyr::mutate(output,
    sample_id = paste0(
      .data$Transect,
      .data$Station,
      .data$MCFF,
      as.numeric(.data$Survey_date)
    ),
    "project_id" = project_id,
    location_id = paste0(.data$MCFF, .data$Transect, .data$Station),
    "Survey_date" = Survey_date
  )

  output <- dplyr::select(output,
    .data$project_id,
    .data$location_id,
    .data$sample_id,
    "date_taken" = .data$Survey_date,
    .data$question,
    .data$response,
    .data$object
  )
  output$parameter <- "benthic survey"
  return(output)
}
