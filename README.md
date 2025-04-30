
<!-- README.md is generated from README.Rmd. Please edit that file -->

# translator

<!-- badges: start -->

<!-- badges: end -->

R package for tidyverse style text translations with the DeepL API.

## Installation

You can install the development version of translator from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("admarhi/translator")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(translator)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(tibble)


tb <- tibble(
  text_de = c("Das ist ein Buch.", "Woher kommst du?")
)
```
