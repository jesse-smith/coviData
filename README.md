<!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test coverage](https://codecov.io/gh/jesse-smith/coviData/branch/master/graph/badge.svg)](https://codecov.io/gh/jesse-smith/coviData?branch=master)
[![R build status](https://github.com/jesse-smith/coviData/workflows/R-CMD-check/badge.svg)](https://github.com/jesse-smith/coviData/actions)
<!-- badges: end -->
 
# coviData <img src='man/figures/logo.png' align="right" height="139" />

coviData is designed to help with ETL, munging, and basic analysis of COVID-19
data within the Shelby County Health Department. It provides utilities for
loading and munging data from several sources and a grab-bag of tidy, generic
tools for handling that data. It serves as a backend for the other SCHD COVID-19
packages.

## Installation

You can install the development version of the package using devtools:

``` r
# install.packages("devtools")
devtools::install_github("jesse-smith/coviData")
```

## Code of Conduct

Please note that the coviData project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By 
contributing to this project, you agree to abide by its terms.
