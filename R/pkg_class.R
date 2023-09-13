# SampleSize-class ----

#' SampleSize Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `SampleSize` class serves as the store for results and parameters in sample
#' size calculation.
#'
#' @slot call call
#' @slot method method
#' @slot n n
#' @slot param param
#'
#' @rdname SampleSize-class
#' @aliases SampleSize
setClass(
  "SampleSize",
  slots = c(
    call = "call",
    method = "character",
    n = "numeric",
    param = "list"
  )
)

# SampleSize-constructors ----

#' @rdname SampleSize-class
#'
#' @param call (`call`)\cr function call.
#' @param method (`character`)\cr method name.
#' @param n (`numeric`)\cr number of sample size.
#' @param param (`list`)\cr list of relevant parameters.
#'
#' @return An object of class `SampleSize`.
#'
SampleSize <- function(call, method, n, param) {
  new("SampleSize", call = call, method = method, n = n, param = param)
}

# SampleSize-validity ----

setValidity("SampleSize", function(object) {
  if (object@n <= 3) {
    "@n is too small."
  } else {
    TRUE
  }

  if (length(object@param) < 1) {
    "Something is missing in @param."
  } else {
    TRUE
  }
})


# MCTab-class ----

#' MCTab Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `MCTab` class serves as the store for 2x2 contingency table
#'
#' @slot data data
#' @slot tab candidate
#' @slot levels levels
#'
#' @rdname MCTab-class
#' @aliases MCTab
setClass(
  "MCTab",
  slots = c(
    data = "data.frame",
    tab = "table",
    levels = "character"
  )
)

# MCTab-constructors ----

#' @rdname MCTab-class
#'
#' @param data (`data.frame`)\cr original data set.
#' @param tab (`table`)\cr `table` class converted from [table()] to display 2x2 contingency table.
#' @param levels (`character`)\cr levels of measurements.
#'
#' @return An object of class `MCTab`.
#'
MCTab <- function(data, tab, levels) {
  new("MCTab", data = data, tab = tab, levels = levels)
}

# MCTab-validity ----

setValidity("MCTab", function(object) {
  if (any(dim(object@tab) != c(2, 2))) {
    "@tab should be 2x2 contingency table."
  } else {
    TRUE
  }
})

# BAsummary-class ----

#' BAsummary Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `BAsummary` class is used to display the BlandAltman analysis and outliers.
#'
#' @slot call call
#' @slot data data
#' @slot outlier outlier
#' @slot param param
#'
#' @rdname BAsummary-class
#' @aliases BAsummary
setClass(
  "BAsummary",
  slots = c(
    call = "call",
    data = "data.frame",
    stat = "list",
    param = "list"
  )
)

# BAsummary-constructors ----

#' @rdname BAsummary-class
#'
#' @param data (`data.frame`)\cr stores the raw data from input.
#' @param stat (`list`)\cr contains several statistics for numeric data.
#' @param param (`list`)\cr list of relevant parameters.
#'
#' @return An object of class `BAsummary`.
#'
BAsummary <- function(call, data, stat, param) {
  new("BAsummary", call = call, data = data, stat = stat, param = param)
}

# BAsummary-validity ----

setValidity("BAsummary", function(object) {
  if (ncol(object@data) != 3 | any(names(object@data) != c("sid", "x", "y"))) {
    "@data should contain 3 columns, sid, x and y."
  } else {
    TRUE
  }
})


# RefInt-class ----

#' Reference Interval Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `RefInt` class serves as the store for results in reference
#' Interval calculation.
#'
#' @slot call call
#' @slot method method
#' @slot n n
#' @slot data data
#' @slot outlier outlier
#' @slot refInt refInt
#' @slot confInt confInt
#'
#' @rdname RefInt-class
#' @aliases RefInt
setClass(
  "RefInt",
  slots = c(
    call = "call",
    method = "character",
    n = "numeric",
    data = "numeric",
    outlier = "list",
    refInt = "numeric",
    confInt = "list"
  )
)

# RefInt-constructors ----

#' @rdname RefInt-class
#'
#' @param call (`call`)\cr function call.
#' @param method (`character`)\cr method names of reference interval and
#' confidence interval.
#' @param n (`numeric`)\cr number of available samples.
#' @param data (`numeric`)\cr numeric raw measurements, no outlier removed.
#' @param outlier (`list`)\cr list of outliers that contains the index and number
#' of outliers, and the data without outliers.
#' @param refInt (`numeric`)\cr number of reference interval.
#' @param confInt (`list`)\cr list of the confidence interval of lower and upper
#' of reference limit.
#'
#' @return An object of class `RefInt`.
#'
RefInt <- function(call, method, n, data, outlier, refInt, confInt) {
  new("RefInt",
    call = call, method = method, n = n, data = data, outlier = outlier,
    refInt = refInt, confInt = confInt
  )
}

# RefInt-validity ----

setValidity("RefInt", function(object) {
  if (any(names(object@confInt) != c("refLower", "refUpper"))) {
    "@confInt should contain 'refLower' and 'refUpper' confidence interval."
  } else {
    TRUE
  }

  if (object@refInt[1] <= min(object@data) | object@refInt[2] >= max(object@data)) {
    "@object should within the range of @data."
  } else {
    TRUE
  }
})

# tpROC-class ----

#' Test for Paired ROC Class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `tpROC` class serves as the store for results in testing the AUC of paired
#' two-sample assays.
#'
#' @slot testROC testROC
#' @slot refROC refROC
#' @slot method method
#' @slot stat stat
#'
#' @rdname tpROC-class
#' @aliases tpROC
setClass(
  "tpROC",
  slots = c(
    testROC = "list",
    refROC = "list",
    method = "character",
    H0 = "numeric",
    stat = "list"
  )
)

# tpROC-constructors ----

#' @rdname tpROC-class
#'
#' @param testROC (`list`)\cr object from `pRPC::roc()` function for test assay.
#' @param refROC (`list`)\cr object from `pRPC::roc()` function for reference/standard assay.
#' @param method (`character`)\cr method of hypothesis test.
#' @param H0 (`numeric`)\cr margin of test.
#' @param stat (`list`)\cr list that contains the difference comparing results,
#' such as the difference of AUC, standard error, confidence interval, Z statistic
#' and P value.
#'
#' @return An object of class `tpROC`.
#'
tpROC <- function(testROC, refROC, method, H0, stat) {
  new("tpROC",
    testROC = testROC, refROC = refROC, method = method, H0 = H0, stat = stat
  )
}
