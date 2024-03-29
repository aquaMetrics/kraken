
<!-- README.md is generated from README.Rmd. Please edit that file -->

# kraken

<!-- badges: start -->

[![R-CMD-check](https://github.com/aquaMetrics/kraken/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aquaMetrics/kraken/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/aquaMetrics/kraken/branch/main/graph/badge.svg)](https://app.codecov.io/gh/aquaMetrics/kraken?branch=main)
<!-- badges: end -->

The goal of `kraken` [R](https://www.r-project.org/) package is to
calculate the mixing zone area from IQI sampling results (DNA or
taxonomic).

This package is in an experimental phase. The core algorithm for
calculating the area is unlikely to change, but the function names as
well as the data formats and structures may change.

## Installation

You can install the development version of `kraken` like so:

``` r
install.packages("devtools")
devtools::install_github("aquaMetrics/kraken")
```

## Calculate Area

This example shows you how to calculate mixing zone from demo IQI input
data:

``` r
library(kraken)
```

``` r
## Run the demo IQI data and return the area of the mixing zone
area <- assess(demo_iqi)
area
#> $`5%`
#> [1] 96914.92
#>
#> $`package version`
#> [1] ‘0.2.0’
#> 
#> $`package date`
#> [1] "2022-07-27"
```

Alternatively, use lower-level functions to step through each part of
the process:

``` r
data <- consecutive_stations(demo_iqi)
probs <- probability_non_linear(data$survey_data)
overrides <- override(probs,
                      overrideTransect1 = 100, # transect1 overrides
                      overrideBearing1 = -47)
breachs <- breach(overrides)
areas <- area(breachs)
```

Note, you can add manual override values for distance to good status or
bearing for each transect using the `override` function.

## Plot

The ellipse area is one of the outputs from the `area()` function. An
example of plotting the ellipse and survey data:

``` r
library(sf)
library(ggplot2)

# Calculate area without overrides
data <- consecutive_stations(demo_iqi)
probs <- probability_non_linear(data$survey_data)
breachs <- breach(probs)
areas <- area(breachs)
ellipse <- areas$ellipse
# Convert survey data to spatial
data <- st_as_sf(data$survey_data, coords = c("Longitude", "Latitude"), crs = 4326)
g <- ggplot() + geom_sf(data = data, aes(color = `WFD status`)) + 
                geom_sf(data = ellipse, alpha = 0)
g
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

## Help

View documentation for each function in the usual way.

``` r
?assess
?consecutive_stations
# ...
```
