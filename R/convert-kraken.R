#' Convert to Kraken
#'
#' Converts data input from `survey_import()` into 'standard' kraken data
#' structure.
#' @param data
#'
#' @return dataframe
#'
#' @examples
convert_kraken <- function(data) {
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

  data <- select(
    data,
    .data$MCFF,
    .data$Survey_date,
    .data$sample_id,
    .data$Easting,
    .data$Northing,
    .data$IQI
  )
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
  return(data)
}
