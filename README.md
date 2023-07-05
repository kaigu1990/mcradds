
<!-- README.md is generated from README.Rmd. Please edit that file -->

> `mcradds` facilitates analyzing and reporting in IVD trials.

- quantitative data:
  - Descriptive statistics, frequencies.
  - Regression methods, rely on `mcr` package.
  - Bland-Altman for quantitative data, refer to CLSI EP09.
  - Outlier detection for quantitative data, refer to CLSI EP09-A2/A3.
  - Medical Decision Level, rely on `mcr` package, and summarize the
    results.
  - Reference Range/Interval, refer to CLSI EP28-A3, or NMPA guideline.
  - ROC/AUC, wraps the `pROC` package.
  - Correlation
- qualitative data:
  - Descriptive statistics, proportion/ratios.
  - Diagnostic Accuracy Criteria for quantitative data, refers to CLSI
    EP12-A2.
  - ROC/AUC, wraps the `pROC` package.

## Installation

You can install the development version from GitHub with:

``` r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("kaigu1990/mcradds")
```

## Getting Started

You can get started by reading the introduction vignette:

``` r
# library(mcradds)
# vignette("introduction", package = "mcradds")
```
