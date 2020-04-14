
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chex

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Installation

``` r
# install.packages("devtools")
devtools::install_github("shunsambongi/chex")
```

## Example

`check_that()` takes an object you want to check, and a number of checks
to run against the object. A check is usually a function that takes the
object as the first argument and returns a boolean value of whether the
check passed (`TRUE`) or failed (`FALSE`).

``` r
library(chex)
library(magrittr)
#> Warning: package 'magrittr' was built under R version 3.6.1

results <- mtcars %>%
  check_that(
    is.data.frame, # this should PASS
    is.character,  # this should FAIL
  )

print(results)
#> -- CHECKS ---------------------------------------------------------------------
#> v is.data.frame ... PASS
#> x is.character ... FAIL
```
