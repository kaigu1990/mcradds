
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
[![CRAN
status](https://www.r-pkg.org/badges/version/mcradds)](https://cran.r-project.org/package=mcradds)
[![](https://cranlogs.r-pkg.org/badges/grand-total/mcradds)](https://cran.r-project.org/package=mcradds)
<!-- end badges -->

The `mcradds` R package is a complement to `mcr` package, contains
common and solid functions for designing, analyzing and visualization in
In Vitro Diagnostic trials. Most of the methods and algorithms refer to
CLSI recommendations and NMPA guidelines.

The package provides a series of typical functionality, as shown below:

- Estimation of sample size for trials, NMPA guideline.
- Diagnostic accuracy with/without standard/golden reference, CLSI
  EP12-A2.
- Regression analysis and plot in method comparison, CLSI EP09-A3.
- Bland-Altman analysis and plot in method comparison, CLSI EP09-A3.
- Outlier detection with 4E method from CLSI EP09-A2 and ESD from CLSI
  EP09-A3.
- Evaluation of bias in medical decision level, CLSI EP09-A3.
- Pearson and Spearman correlation adding hypothesis test and confidence
  interval.
- Establishing of Reference Range/Interval, CLSI EP28-A3 and NMPA
  guideline.
- Paired ROC/AUC test for superiority and non-inferiority trials, CLSI
  EP05-A3/EP15-A3.
- Reproducibility analysis (reader precision) for immunohistochemical
  assays, CLSI I/LA28-A2 and NMPA guideline.
- Evaluation of precision of quantitative measurements, CLSI EP05-A3.
- Descriptive statistics summary.

## Installation

`mcradds` is available on CRAN and you can install the latest released
version with:

``` r
install.packages("mcradds")
```

or you can install the development version directly from GitHub with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("kaigu1990/mcradds")
```

See package vignettes `browseVignettes(package = "mcradds")` for usage
of this package.
