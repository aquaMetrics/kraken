#' Assess survey data
#'
#' Assess the size of the mixing zone based on modelling the distance to good
#' status. Where distance cannot be modeled, the distance to the second
#' consecutive good status (or high status) station is used.
#'
#' @param data Data frame with 7 case-sensitive named variables as described
#'   below, see `demo_iqi` for example data. Rows and columns can be arranged in
#'   any order. Additional columns can be present but will be ignored.
#' \describe{
#' \item{Survey_date}{Survey_date character string}
#' \item{MCFF}{MCFF Site name - character string}
#' \item{Transect}{Transect integer}
#' \item{Station}{Station integer increasing from cage edge e.g. 1,2,3,4 etc}
#' \item{Easting}{Easting coordinate}
#' \item{Northing}{Northing coordinate}
#' \item{IQI}{IQI ratio - Environmental Quality Ratio EQR}
#' }
#' @param overrideTransect1 Optional override distance Transect 1
#' @param overrideTransect2 Optional override distance Transect 2
#' @param overrideTransect3 Optional override distance Transect 3
#' @param overrideTransect4 Optional override distance Transect 4
#' @param overrideBearing1 Optional override bearing Transect 1
#' @param overrideBearing2 Optional override bearing Transect 2
#' @param overrideBearing3 Optional override bearing Transect 3
#' @param overrideBearing4 Optional override bearing Transect 4
#' @return A named list containing the fifth percentile of the modeled area in
#'   meters, package version and package date.
#' @export
#'
#' @examples
#' \dontrun{
#' area <- assess(demo_iqi)
#' }
assess <- function(data,
                   overrideTransect1 = NA,
                   overrideTransect2 = NA,
                   overrideTransect3 = NA,
                   overrideTransect4 = NA,
                   overrideBearing1 = NA,
                   overrideBearing2 = NA,
                   overrideBearing3 = NA,
                   overrideBearing4 = NA) {
  data <- consecutive_stations(data)
  probs <- probability_non_linear(data$survey_data)
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
  return(areas$fifthPercentileArea)
}
