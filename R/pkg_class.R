# SampleSize-class ----

#' SampleSize class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `SampleSize` class serves as the store for results and parameters in sample
#' size calculation.
#'
#' @slot slots call method n param
#'
#' @rdname SampleSize-class
#' @aliases SampleSize
setClass(
  "SampleSize",
  # contains = "list",
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
#' @param param (`list`)\cr listing of relevant parameters.
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

#' MCTab class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `MCTab` class serves as the store for 2x2 contingency table, candidate and
#' comparative measurement's information.
#'
#' @slot slots tab candidate comparative
#'
#' @rdname MCTab-class
#' @aliases MCTab
setClass(
  "MCTab",
  slots = c(
    tab = "matrix",
    candidate = "list",
    comparative = "list"
  )
)

# MCTab-constructors ----

#' @rdname MCTab-class
#'
#' @param tab (`matrix`)\cr `matrix` class converted from [xtabs()] to display 2x2 contingency table.
#' @param candidate (`list`)\cr candidate information, like measurements,
#'  positive and negative levels.
#' @param comparative (`list`)\cr comparative information, like measurements,
#'  positive and negative levels.
#'
#' @return An object of class `MCTab`.
#'
MCTab <- function(tab, candidate, comparative) {
  new("MCTab", tab = tab, candidate = candidate, comparative = comparative)
}

# MCTab-validity ----

setValidity("MCTab", function(object) {
  if (any(dim(object@tab) != c(2, 2))) {
    "@tab should be 2x2 2x2 contingency table."
  } else {
    TRUE
  }

  if (!is.factor(object@candidate$data) | !is.factor(object@comparative$data)) {
    "@candidate and @comparative should be all factor class."
  } else {
    TRUE
  }
})

# BAsummary-class ----

#' BAsummary class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `BAsummary` class is used to display the BlandAltman analysis and outliers.
#'
#' @slot slots data data outlier
#'
#' @rdname BAsummary-class
#' @aliases BAsummary
setClass(
  "BAsummary",
  slots = c(
    data = "data.frame",
    stat = "matrix",
    outlier = "list"
  )
)

# BAsummary-constructors ----

#' @rdname BAsummary-class
#'
#' @param data (`data.frame`)\cr stores the raw data from input.
#' @param stat (`matrix`)\cr contains several statistics for numeric data.
#' @param outlier (`list`)\cr contains the outlier order and sample id with corresponding
#' calculated statistics.
#'
#' @return An object of class `BAsummary`.
#'
BAsummary <- function(tab, data, stat, outlier) {
  new("BAsummary", data = data, stat = stat, outlier = outlier)
}

