
<!-- README.md is generated from README.Rmd. Please edit that file -->

# onsr

<!-- badges: start -->

<!-- badges: end -->

This package collects common R functions used in the Ottawa
Neighbourhood Study’s (ONS) data-science work. We use it to speed up our
work, but anyone is welcome to use it or learn from it.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Ottawa-Neighbourhood-Study/onsr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
# Load the ONS Gen 2 shapefile and plot it.
library(onsr)
library(ggplot2)
library(magrittr)

ons_shp <- get_ons_shp()

ons_shp %>%
  ggplot2::ggplot() +
    ggplot2::geom_sf()
```

<img src="man/figures/README-example-1.png" width="100%" />
