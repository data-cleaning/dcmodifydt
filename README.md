
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dcmodifydt

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dcmodifydt)](https://CRAN.R-project.org/package=dcmodifydt)
[![R-CMD-check](https://github.com/data-cleaning/dcmodifydt/workflows/R-CMD-check/badge.svg)](https://github.com/data-cleaning/dcmodifydt/actions)
[![Codecov test
coverage](https://codecov.io/gh/data-cleaning/dcmodifydt/branch/main/graph/badge.svg)](https://codecov.io/gh/data-cleaning/dcmodifydt?branch=main)
<!-- badges: end -->

`dcmodifydt` executes modification rules on a data.table.

## Installation

The development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("data-cleaning/dcmodifydt")
```

## Example

``` r
library(dcmodifydt)
#> Loading required package: dcmodify
```

``` r
library(data.table)

m <- modifier( if (age > 130) age = 130
             , income[age < 12] <- 0
             )

dat <- fread(text =
"age, income
 140,  300
  11, 2000
  25, 3000"
)

# modify a copy of the data
dat_m <- modify(dat, m, copy = TRUE)
print(dat_m)
#>    age income
#> 1: 130    300
#> 2:  11      0
#> 3:  25   3000

# the data it self
setmodify(dat, m)
print(dat)
#>    age income
#> 1: 130    300
#> 2:  11      0
#> 3:  25   3000
```
