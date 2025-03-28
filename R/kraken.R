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
#' @param loess Use loess model instead of fitting mulitple different models
#' @param hera_format Flag if import data in 'hera' format
#' @param good_moderate  Good-Moderate boundary value
#' @param method Type of method used to analyse samples either 'iqi' or
#'   'residue'.
#'
#' @return Data frame contain 8 variables.
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr mutate case_when select filter
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
                   good_moderate = 0.64,
                   method = "iqi") {

  if (hera_format == TRUE) {
    data <- filter(data, .data$question %in% c(
      "Site Name",
      "Easting",
      "Northing",
      "IQI",
      "Date of survey (start date)"
    ))
    data$Survey_date <-
      data$response[data$question == "Date of survey (start date)"]
    data$MCFF <-
      data$response[data$question == "Site Name"]
    data <- data %>%
      unique() %>%
      pivot_wider(
        names_from = .data$question,
        values_from = .data$response
      )

    data <- select(data,
                   .data$MCFF,
                   .data$Survey_date,
                   .data$sample_id,
                   .data$Easting,
                   .data$Northing,
                   .data$IQI)
    data <- stats::na.omit(data)
    data <- data %>%
      mutate(Transect = case_when(
        sample_id %in% c(
          "PE-1", "T1-2",
          "T1-3", "T1-4",
          "T1-5", "T1-6",
          "T1-7", "T1-8",
          "T1-9"
        ) ~ 1,
        sample_id %in% c(
          "PE-2", "T2-2",
          "T2-3", "T2-4",
          "T2-5", "T2-6",
          "T2-7", "T2-8",
          "T2-9"
        ) ~ 2,
        sample_id %in% c(
          "PE-3", "T3-2",
          "T3-3", "T3-4",
          "T3-5", "T3-6",
          "T3-7", "T3-8",
          "T3-9"
        ) ~ 3,
        sample_id %in% c(
          "PE-4", "T4-2",
          "T4-3", "T4-4",
          "T4-5", "T4-6",
          "T4-7", "T4-8",
          "T4-9"
        ) ~ 4,
        sample_id %in% c(
          "PE-5", "T5-2",
          "T5-3", "T5-4",
          "T5-5", "T5-6",
          "T5-7", "T5-8",
          "T5-9"
        ) ~ 5,
        sample_id %in% c(
          "PE-6", "T6-2",
          "T6-3", "T6-4",
          "T6-5", "T6-6",
          "T6-7", "T6-8",
          "T6-9"
        ) ~ 6
      ))

    data <- data %>%
      mutate(Station = case_when(
        sample_id %in% c(
          "PE-1", "PE-2",
          "PE-3", "PE-4",
          "PE-5", "PE-6"
        ) ~ 1,
        sample_id %in% c(
          "T1-2", "T2-2",
          "T3-2", "T4-2",
          "T5-2", "T6-2"
        ) ~ 2,
        sample_id %in% c(
          "T1-3", "T2-3",
          "T3-3", "T4-3",
          "T5-3", "T6-3"
        ) ~ 3,
        sample_id %in% c(
          "T1-4", "T2-4",
          "T3-4", "T4-4",
          "T5-4", "T6-4"
        ) ~ 4,
        sample_id %in% c(
          "T1-5", "T2-5",
          "T3-5", "T4-5",
          "T5-5", "T6-5"
        ) ~ 5,
        sample_id %in% c(
          "T1-6", "T2-6",
          "T3-6", "T4-6",
          "T5-6", "T6-6"
        ) ~ 6,
        sample_id %in% c(
          "T1-7", "T2-7",
          "T3-7", "T4-7",
          "T5-7", "T6-7"
        ) ~ 7,
        sample_id %in% c(
          "T1-8", "T2-8",
          "T3-8", "T4-8",
          "T5-8", "T6-8"
        ) ~ 8,
        sample_id %in% c(
          "T1-9", "T2-9",
          "T3-9", "T4-9",
          "T5-9", "T6-9"
        ) ~ 9,
      ))

    data <- data %>% stats::na.omit()
    data$Survey_date <- as.Date(as.numeric(data$Survey_date),
      origin = "1899-12-30"
    )
    data$Transect <- as.numeric(data$Transect)
    data$Station <- as.numeric(data$Station)
    data$Easting <- as.numeric(data$Easting)
    data$Northing <- as.numeric(data$Northing)
    data$IQI <- as.numeric(data$IQI)
  }

  data$survey_id <- paste0(data$MCFF, "-", as.numeric(data$Survey_date))

  all_output <- purrr::map_df(split(data, data$survey_id), function(data) {
    data <- consecutive_stations(data,
                                 good_moderate = good_moderate,
                                 method = method)
    probs <- probability_non_linear(data$survey_data,
      loess = loess,
      good_moderate = good_moderate,
      method = method
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
        "response" = c(unique(context_warnings$sign),
                       unique(context_warnings$type))
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
    output <- dplyr::mutate(output,
      sample_id = paste0(
        .data$Transect,
        .data$Station,
        .data$MCFF,
        as.numeric(.data$Survey_date)
      ),
      project_id = paste0(.data$MCFF, as.numeric(.data$Survey_date)),
      location_id = paste0(.data$MCFF, .data$Transect, .data$Station)
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
  })
  return(all_output)
}
