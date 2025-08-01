% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probability-non-linear.R
\name{probability_non_linear}
\alias{probability_non_linear}
\title{Probability Non-linear}
\usage{
probability_non_linear(
  data,
  loess = FALSE,
  good_moderate = 0.64,
  method = "iqi"
)
}
\arguments{
\item{data}{The named data frame `survey_data` from `consecutive_station()` function. See examples.}

\item{loess}{Use loess model (instead of best fit model).}

\item{good_moderate}{The EQR ratio for Good - Moderate boundary.}

\item{method}{Type of method used to analyse samples, either "iqi" or
"residue".}
}
\value{
list containing four named data frames: data, geoDf, geoDfBestFit and
  hexdfOut.
}
\description{
Fit the best model calculating the distance to good status. Where distance
cannot be modeled, the distance to the second consecutive good status (or
high status) station is used.
}
\examples{
\dontrun{
data <- consecutive_stations(data)
probability <- probability_non_linear(data$survey_data)
}
}
