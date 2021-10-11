
# kctools

<!-- badges: start -->
<!-- badges: end -->

The `kctools` package includes a suite of helper functions I frequently use in my data wrangling and analysis. 

This includes functions to quickly generate a data frame of summary statistics and tabulations, creating indicators for duplicate observations, functions for spatial analysis, among others. Many of the custom functions were created based on the `dplyr` and `tidyr` packages as well as their counterparts in `tidytable`, a [package](https://markfairbanks.github.io/tidytable/) created by Mark Fairbanks which provides a tidy interface to `data.table` operations. 

## Installation

You can install the released version of kctools from  [my Github site](https://github.com/kennchua/kctools) with:

``` r
devtools::install_github("kennchua/kctools")
```

## Example

Check the pertinent function for use cases and examples.

``` r
library(kctools)
# Data exploration tools:
# kc_sum(): generate summary statistics
# kc_tab(): tabulate variables
# kc_dup(): create indicators for duplicated observations
# kc_cor(): create correlation matrix with lower triangular and diagonal elements only
# See also kcf_* counterparts for tidytable implementation

# Spatial analysis tools:
# kc_spatial: create data frame with pairwise distances of two sf objects
```
