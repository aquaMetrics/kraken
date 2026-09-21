
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
calculate a predicted impact area from Fauna IQI, DNA predicted IQI or
emamectin benzoate sampling results.

This package is in an experimental phase. The core algorithm for
calculating the area is unlikely to change, but the function names as
well as the data formats and structures may change.

## Installation

You can install the most up to date version of `kraken` like so:

``` r
install.packages("devtools")
devtools::install_github("aquaMetrics/kraken")
```

Run this code regularly to keep up to date with the latest version.

## Calculate Area

``` r
library(kraken)
```

Illustrative demo data shows how to calculate impact zone area and all
relevant outputs.

``` r
output <- kraken(demo_iqi)
head(output)
```

    #> # A tibble: 6 × 8
    #>   project_id location_id sample_id date_taken question response object parameter
    #>   <chr>      <chr>       <chr>     <date>     <chr>    <chr>    <list> <chr>    
    #> 1 Bellister… Bellister1… 1NABelli… NA         station… Complia… <NULL> benthic …
    #> 2 Bellister… Bellister1… 1NABelli… NA         twoCons… Complia… <NULL> benthic …
    #> 3 Bellister… Bellister2… 2NABelli… NA         station… Complia… <NULL> benthic …
    #> 4 Bellister… Bellister2… 2NABelli… NA         twoCons… Complia… <NULL> benthic …
    #> 5 Bellister… Bellister3… 3NABelli… NA         station… Complia… <NULL> benthic …
    #> 6 Bellister… Bellister3… 3NABelli… NA         twoCons… Complia… <NULL> benthic …

The returned dataframe provides responses to 32 key questions required
to assess the mixing zone. These responses included station, transect
and survey-level outputs. The overall estimated mixing zone (with 95%
confidence) is provided, see `area_95_confidence`.

``` r
# area_95 provides the output 
output$response[output$question == "area_95_confidence"]
#> [1] "96914.9237670089"
```

Additionally, numerous other outputs are provided see `?kraken`
documentation for details. For example, check if any warnings returned.

``` r
output$response[output$question == "area_warning"]
#> [1] NA
output$response[output$question == "ellipse_warnings"]
#> [1] NA
```

The `output` dataframe includes a map of the stations and the predicted
impact area and breach points. This is provided in the `object` variable
of the dataframe.

``` r
output$object[output$question == "map"]
#> [[1]]
```

<img src="man/figures/README-map-1.png" alt="" width="100%" />

An ellipse is used to approximate the impacted area by spanning the
breach points predicted for each sampling transect such that all given
points lie just inside or on the boundary of the ellipsoid.

# Residue area

To calculate emamectin benzoate sampling results

``` r
output <- kraken(demo_residue, method="residue", pass_fail = 763)
head(output)
```

    #> # A tibble: 6 × 8
    #>   project_id location_id sample_id date_taken question response object parameter
    #>   <chr>      <chr>       <chr>     <date>     <chr>    <chr>    <list> <chr>    
    #> 1 BELL103/0… BELL11NA    1NABELL1… NA         station… Complia… <NULL> benthic …
    #> 2 BELL103/0… BELL11NA    1NABELL1… NA         twoCons… Complia… <NULL> benthic …
    #> 3 BELL103/0… BELL12NA    2NABELL1… NA         station… Complia… <NULL> benthic …
    #> 4 BELL103/0… BELL12NA    2NABELL1… NA         twoCons… Complia… <NULL> benthic …
    #> 5 BELL103/0… BELL13NA    3NABELL1… NA         station… Complia… <NULL> benthic …
    #> 6 BELL103/0… BELL13NA    3NABELL1… NA         twoCons… Complia… <NULL> benthic …

# Plots

Currently, a function to generate plots for each sampling transect is
undocumented but can be accessed for demonstration purposes. The
`method` parameter must be set to “iqi” or “residue” and the `pass_fail`
to match either IQI standard of “0.64” or the required residue standard.

``` r
plots <- kraken:::create_plot(output, method = "residue", pass_fail = 763)
```

For example, mocked data for the first transect will look similar to
this:

<img src="man/figures/README-plot-1.png" alt="" width="100%" /> The plot
above shows a dashed green line as standard pass/fail boundary, the
black dashed line shows the predicted breach point. Black points
represent each replicate sample at each sampling station. The hexagon
heatmap show the number of predictions in that area.

# Save Ellipse Shapefile

You save can indicative mixing zone ‘ellipse’ as a shapefile.

``` r
ellipse <- output$object[output$question == "ellipse"][[1]]
sf::write_sf(ellipse, "ellipse.shp")
```

Other lower-level functions are provided to run specific parts of the
validation and calculation process.

## Help

View documentation for each function in the usual way.

``` r
?kraken
?consecutive_stations   
# ...
```

A [issues](https://github.com/aquaMetrics/kraken/issues) to github to
discuss bugs or features.
