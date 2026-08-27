#' Assess survey data
#'
#' Assess the size of the mixing zone based on modelling the distance to good
#' status. Where distance cannot be modeled, the distance to the second
#' consecutive good status (or high status) station is used.
#'
#' @inheritParams kraken
#' @return A named list containing the fifth percentile of the modeled area in
#'   meters, package version and package date.
#' @export
#'
#' @examples
#' \dontrun{
#' area <- assess(demo_iqi)
#' }
assess <- function(
  data,
  overrideTransect1 = NA,
  overrideTransect2 = NA,
  overrideTransect3 = NA,
  overrideTransect4 = NA,
  overrideBearing1 = NA,
  overrideBearing2 = NA,
  overrideBearing3 = NA,
  overrideBearing4 = NA,
  loess = FALSE,
  use_mean_bearing = TRUE,
  ellipse_representative = TRUE
) {
  message(
    "This assess() function has been deprecated, please use kraken() function"
  )
  data <- consecutive_stations(data, use_mean_bearing = use_mean_bearing)
  probs <- probability_non_linear(
    data$survey_data,
    loess = loess
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
  breachs <- breach(overrides, ellipse_representative = ellipse_representative)
  areas <- area(breachs, ellipse_representative = ellipse_representative)
  return(areas$fifthPercentileArea)
}
