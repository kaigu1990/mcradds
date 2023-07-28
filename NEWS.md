# mcradds 0.1.0

* First release of the `mcradds` package, contains basic quantitative or qualitative performance methods and functions as shown below.

#### Sample Size

* `size_one_prop` and `size_ci_one_prop` for sample size of qualitative trials, `size_corr` and `size_ci_corr` for quantitative trails.

#### Classes and Datasets

* `SampleSize`, `MCTab` and `BAsummary` classes for `show` method.
* `platelet` and `qualData` data sets for example and testing use.

#### Analyzing Functions and Methods

* `diagTab` function to get 2x2 contingency table, and `getAccuracy` method to compute the qualitative diagnostic accuracy criteria.
* `blandAltman` function to calculate statistics for Bland-Altman, and `getOutlier` method to detect the potential outliers.
* `pearsonTest` and `spearmanTest`, more efficient functions to compute the confidence interval and hypothesis test.
* `mcreg` and `calcBias` methods of `mcr` package are wrapped for regression analysis.
