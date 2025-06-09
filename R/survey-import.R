#' Import Survey Template
#'
#' @param path character file path to survey template
#'
#' @return Data frame with 5 variables
#' \describe{
#' \item{project_id}{character project id}
#' \item{sample_id}{character sample id}
#' \item{question}{question - character question}
#' \item{response}{response - character response value}
#' \item{label}{label - Mainly used for labelling taxonomic observations}
#' }
#' @export
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' file <- system.file("extdat",
#'   "survey-template/220421-SelfMon-N4952-CAV1-Enhanced.xlsx",
#'   package =
#'     "kraken"
#' )
#' data <- survey_import(file)
#' }
#'
survey_import <- function(path = NULL) {
  # Survey metadata ------------------------------------------------------------
  # Top level info about the survey for example company, site, licence etc
  cover <- suppressMessages(readxl::read_xlsx(path, sheet = "1. Cover Sheet"))

  survey <- tibble::tibble(
    question = unlist(c(
      cover[10:28, 1],
      cover[30, 1],
      cover[9, 8],
      cover[c(23:26, 28:29, 31:32), 8]
    )),
    response = unlist(c(
      cover[10:28, 3],
      cover[30, 3],
      cover[10, 8],
      cover[c(23:26, 28:29, 31:32), 9]
    ), )
  )
  survey$sample_id <- uuid::UUIDgenerate()
  project_id <- paste(survey[1, 2], survey[11, 2])
  survey$project_id <- project_id
  # Sample metadata -----------------------------------------------------------
  # Info from stations along transects

  transects <- rep(12, 7)
  transects <- transects * c(1, 2, 3, 4, 5, 6, 7)
  samples <- purrr::map_df(transects, function(transect) {
    if (transect == 84) {
      sample_1 <- cover[(34 + transect - 12):(34 + transect), c(1, 3:8)]
    } else {
      sample_1 <- cover[(34 + transect - 12):(34 + transect - 2), c(1, 3:8)]
    }

    names(sample_1) <- as.character(sample_1[1, ])
    sample_1 <- sample_1[2:nrow(sample_1), ]
    sample_long <- tidyr::pivot_longer(sample_1,
                                       cols = -1,
                                       values_to = "response",
                                       names_to = "sample_id"
    )

    names(sample_long) <- c("question", "sample_id", "response")
    sample_long <- dplyr::select(
      sample_long,
      "sample_id",
      "question",
      "response"
    )
    return(sample_long)
  })

  samples$project_id <- project_id
  meta <- dplyr::bind_rows(survey, samples)
  meta <- dplyr::select(
    meta,
    "project_id",
    "sample_id",
    "question",
    "response"
  )

  # PSA, Chemistry, meta data sheets - 'data' sheets. --------------------------
  data_sheets_ref <- c(
    "2. T1-Data",
    "4. T2-Data",
    "6. T3-Data",
    "8. T4-Data",
    "10. T5-Data",
    "12. T6-Data",
    "14. Add-Data"
  )
  # Loop through sheets and format
  data_sheets <- purrr::map_df(data_sheets_ref, function(data_sheet) {
    t1_data <- suppressMessages(readxl::read_xlsx(path, sheet = data_sheet))
    names(t1_data) <- as.character(t1_data[2, ])
    t1_data <- t1_data[3:nrow(t1_data), ]
    t1_data <- dplyr::select(t1_data, -"NA")
    t1_data <- tidyr::pivot_longer(t1_data,
                                   cols = c(2:ncol(t1_data)),
                                   names_to = "sample_id",
                                   values_to = "response"
    )
    t1_data <- dplyr::select(t1_data, "sample_id", question = 1, "response")
    t1_data <- dplyr::filter(t1_data, !is.na(question))
    t1_data <- dplyr::filter(t1_data, question != "Notes:")
    t1_data$project_id <- project_id

    return(t1_data)
  })

  # Fauna datasheets -----------------------------------------------------------
  # List of sheets containing Fauna info
  fauna_sheets_ref <- c(
    "3. T1-Fauna",
    "5. T2-Fauna",
    "7. T3-Fauna",
    "9. T4-Fauna",
    "11. T5-Fauna",
    "13. T6-Fauna",
    "15. Add-Fauna"
  )
  # Loop through fauna worksheets and format data
  fauna_sheets <- purrr::map_df(fauna_sheets_ref, function(data_sheet) {
    fauna <- suppressMessages(readxl::read_xlsx(path, sheet = data_sheet))
    # Format 'number of replicates' info
    replicates <- fauna[2, ]
    replicates <- tidyr::pivot_longer(replicates,
                                      cols = c(4:ncol(fauna)),
                                      names_to = "sample_id",
                                      values_to = "response"
    )
    replicates$project_id <- project_id
    replicates <- dplyr::select(replicates,
                                "project_id",
                                "sample_id",
                                question = "...3",
                                "response"
    )

    # Format taxonomic info
    fauna <- fauna[4:nrow(fauna), ]
    fauna <- tidyr::pivot_longer(fauna,
                                 cols = c(4:ncol(fauna)),
                                 names_to = "sample_id",
                                 values_to = "response"
    )
    fauna <- tidyr::pivot_longer(fauna,
                                 cols = -c(2, 4)
    )

    fauna <- dplyr::select(fauna,
                           "sample_id",
                           "question" = "name",
                           "response" = "value",
                           "label" = 1,
    )

    # Define 'question' for comment, count and MCS responses
    fauna <- dplyr::mutate(fauna,
                           question = replace(
                             question,
                             question == "...3",
                             "comment"
                           )
    )
    fauna <- dplyr::mutate(fauna,
                           question =
                             replace(
                               question,
                               question == "Transect 1 Species Abundance Matrix",
                               "MCS Code"
                             )
    )
    fauna <- dplyr::mutate(fauna,
                           question = replace(
                             question, question == "response",
                             "Taxon abundance"
                           )
    )
    fauna <- dplyr::filter(fauna, !is.na(label))
    fauna$project_id <- project_id
    fauna <- dplyr::bind_rows(replicates, fauna)
    return(fauna)
  })

  # Bind all sheets into single dataframe --------------------------------------
  result <- dplyr::bind_rows(meta, data_sheets, fauna_sheets)
  result <- dplyr::filter(result, !grepl("Table", question))
  result$parameter <- "MPFF Compliance"
  return(result)
}
