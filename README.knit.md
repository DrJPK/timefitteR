---
output: github_document
---



# timefitteR - A library for exploring longitudinal data

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/timefitteR)](https://CRAN.R-project.org/package=timefitteR)
<!-- badges: end -->

Determining the most appropriate model to fit to longitudinal data, particularly when there are multiple indicator variables to be considered alongside time data, can be a tricky problem. timefitteR has been developed to help test and compare multiple linear models for the researcher so that they can make an informed decision about what to include and what to exclude. There are two fundamental principles underpinning the use of this package:

1. Occam's Razor - When applied to developing models this can be interpreted as the simplest model that can predict the observed data should be preferred.
1. Statistical Reasonableness - By this we mean that any changes to models developed by timefitteR should be statistically significant (they should not be the result of chance), statistically meaningful (they should have a noticeable effect on the quality of the predicted outcomes), and they should be theoretically plausible (there should be some mechanism that can be hypothesised to mediate the effect seen between the variables).

This package provides functions to generate semi-deterministic time series synthetic data-sets to test various approaches as well as the functions needed to explore that data.  This package is in active development and should be treated as alpha software.

## Installation

You can install the development version of timefitteR like so:

``` r
devtools::install_github("DrJPK/timefitteR")
```

## Examples
