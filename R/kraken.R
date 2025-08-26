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
#' @param hera_format Flag if import data in 'hera' format
#' @param good_moderate  Good-Moderate boundary value
#' @param method Type of method used to analyse samples either 'iqi' or
#'   'residue'.
#' @param niter Number of bootstrap resamples must be even number
#'  description
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
                   method = "iqi",
                   niter = 1000) {

  if (hera_format == TRUE) {
    # If input data from kraken::survey_import() change back into kraken data
    # 'standard'
    data <- convert_kraken(data)
  }

  # Need unique survey_id
  data$survey_id <- paste0(data$MCFF, "-", as.numeric(data$Survey_date))

  # Loop to run through multiple surveys
  all_output <- purrr::map_df(split(data, data$survey_id), function(data) {

    data <- consecutive_stations(data,
                                 good_moderate = good_moderate,
                                 method = method)

    probs <- probability_non_linear(data$survey_data,
      loess = loess,
      good_moderate = good_moderate,
      method = method,
      niter = niter
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
