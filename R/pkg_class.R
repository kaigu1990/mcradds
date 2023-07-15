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

# size-constructors ----

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
