
<!-- README.md is generated from README.Rmd. Please edit that file -->

# aquaman

<!-- badges: start -->

[![R-CMD-check](https://github.com/aquaMetrics/aquaman/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aquaMetrics/aquaman/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Codecov test
coverage](https://codecov.io/gh/aquaMetrics/aquaman/branch/main/graph/badge.svg)](https://app.codecov.io/gh/aquaMetrics/aquaman?branch=main)
<!-- badges: end -->

The goal of `aquaman` R package is to calculate ecology metrics and
assess the mixing zone area, fulfilling these specific steps:

-   Assess the mixing zone area from IQI sampling results (DNA or
    taxonomic).

Possible future development:

-   Assign bacteria taxa from DNA reads using `dada2` package.
-   Model IQI metric from assigned bacteria families.

This package is in an experimental phase. The core algorithm for
calculating the area is unlikely to change, but the functions, and input
and output data structures may change.

## Installation

You can install the development version of `aquaman` like so:

``` r
install.packages("devtools")
devtools::install_github("aquaMetrics/aquaman")
```

## Calculate Area

This example shows you how to calculate mixing zone from demo IQI input
data:

``` r
library(aquaman)
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

Alternatively, it is possible to to see outputs from each step in the
process. As well as add manual overrides for distance to good status or
bearing for each transect:

``` r
data <- consecutive_stations(demo_iqi)
probs <- probability_non_linear(data$survey_data)
overrides <- override(probs,
                      overrideTransect1 = 100, # transect1 overrides
                      overrideBearing1 = -47)
breachs <- breach(overrides)
areas <- area(breachs)
```

## Model IQI

Using assigned family-level bacteria taxa as predictors, calculate
benthic invert IQI as an outcome.

**WORK IN PROGRESS - DO NOT USE IN PRODUCTION**

``` r
# Run the IQI model based on demo taxanomic data
iqi_scores <- iqi(demo_taxa)
#> Warning in iqi(demo_taxa): WORK IN PROGRESS - DO NOT USE IN PRODUCTION
head(iqi_scores, 5)
#>    site 1    site 2    site 3    site 4    site 5 
#> 0.7436820 0.6092373 0.6048445 0.7551401 0.7568501
```

IQI prediction model created by Tom Wilding (SAMS) based on training
data from SEPA And MOWI.

## Assign Taxa

**WORK IN PROGRESS - DO NOT USE IN PRODUCTION**

Assign bacteria families based on S-16 DNA reads. This may in time
provide the input to `iqi()` function. Currently, a Qiime 2 command line
script achieves this part of the process.

*Note*, you will be prompted to download a reference taxonomic file on
first use. This will be stored locally.

``` r
# Provide a path to the demo data within the package:
taxa <- assign_taxa(demo_path())
# ...this could take some time...
head(taxa, 5)
#>        sample_id                 Family reads
#> 1   MHS-ARD-0-E2           Mitochondria     8
#> 2   MHS-ARD-0-E2           Mitochondria    47
#> 3   MHS-ARD-0-E2           Mitochondria   384
#> 4   MHS-ARD-0-E2        Anaerolineaceae     7
#> 5   MHS-ARD-0-E2                   <NA>    10
```
