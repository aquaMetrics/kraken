% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/consecutive-stations.R
\name{consecutive_stations}
\alias{consecutive_stations}
\title{Consecutive Stations}
\usage{
consecutive_stations(data, good_moderate = 0.64, method = "iqi")
}
\arguments{
\item{data}{Data frame with survey data}

\item{good_moderate}{The EQR ratio for Good - Moderate boundary.}

\item{method}{Type of method used to analyse samples, either "iqi" or
"residue".}
}
\value{
A named list of two data frames `sample_point_checks` and
  `survey_data`
}
\description{
Check if two consecutive stations are at Good status and minimum number of
stations have been sampled.
}
\examples{
\dontrun{
stations <- consecutive_stations(demo_iqi)
}
}
