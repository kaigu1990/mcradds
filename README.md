
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

- Estimation of sample size, NMPA guideline.
- Regression methods in method comparison, CLSI EP09-A3.
- Bland-Altman analysis and plots for quantitative measurements.
- Outlier detection, including 4E method from CLSI EP09-A2 and ESD
  method from CLSI EP09-A3.
- Correlation, adding hypothesis test and confidence interval.
- Evaluation of bias in medical decision level, CLSI EP09-A3.
- Computing Reference Range/Interval, CLSI EP28-A3 and NMPA guideline.
- Paired ROC/AUC test for superiority and non-inferiority trials, CLSI
  EP05-A3/EP15-A3.
- Diagnostic accuracy criteria with/without reference, CLSI EP12-A2.
- Evaluation of precision of quantitative measurements, CLSI EP05-A3.
- Reproducibility analysis (reader precision) for immunohistochemical
  assays, CLSI I/LA28-A2 and NMPA guideline.

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
