
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mcradds <a href='https://github.com/kaigu1990/mcradds'><img src="man/figures/logo.png" align="right" height="139" style="max-width: 100%;"/></a>

<!-- start badges -->

![GitHub commit
activity](https://img.shields.io/github/commit-activity/m/kaigu1990/mcradds)
![GitHub
contributors](https://img.shields.io/github/contributors/kaigu1990/mcradds)
![GitHub last
commit](https://img.shields.io/github/last-commit/kaigu1990/mcradds)
![GitHub pull
requests](https://img.shields.io/github/issues-pr/kaigu1990/mcradds)
![GitHub repo
size](https://img.shields.io/github/repo-size/kaigu1990/mcradds)
![GitHub language
count](https://img.shields.io/github/languages/count/kaigu1990/mcradds)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current
Version](https://img.shields.io/github/r-package/v/kaigu1990/mcradds/main?color=purple&label=package%20version)](https://github.com/kaigu1990/mcradds/tree/main)
<!-- end badges -->

The `mcradds` R package is a complement to `mcr` package, contains
common and solid functions for designing, analyzing and visualization in
In Vitro Diagnostic trials. Most of the methods and algorithms refer to
CLSI recommendations and NMPA guidelines.

The package provides a series of typical functionality, as shown below:

- Regression methods in method comparison
- Bland-Altman analysis and plots for numeric data
- Outlier detection, including 4E method from EP09A2 and ESD method from
  EP09A3
- Correlation, adding hypothesis test and confidence interval
- Estimating bias in medical decision level
- Computing Reference Range/Interval from CLSI EP28-A3 or NMPA guideline
- ROC/AUC for non-inferiority trials
- Diagnostic accuracy criteria
- Precision analysis
- Descriptive statistics, proportion/ratios

## Installation

You can install the development version from GitHub with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("kaigu1990/mcradds")
```

See package vignettes `browseVignettes(package = "mcradds")` for usage
of this package.
