# samplesize-class ----

#' samplesize class
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' The `samplesize` class serves as the store for results and parameters in sample
#' size calculation.
#'
#' @slot slots call method n param
#'
#' @rdname samplesize-class
#' @aliases samplesize
setClass(
  "samplesize",
  # contains = "list",
  slots = c(
    call = "call",
    method = "character",
    n = "numeric",
    param = "list"
  )
)

# size-constructors ----

#' @rdname samplesize-class
#'
#' @param call (`call`)\cr function call.
#' @param method (`character`)\cr method name.
#' @param n (`numeric`)\cr number of sample size.
#' @param param (`list`)\cr listing of relevant parameters.
#'
#' @return An object of class `samplesize`.
#'
samplesize <- function(call, method, n, param) {
  new("samplesize", call = call, method = method, n = n, param = param)
}

# samplesize-validity ----

setValidity("samplesize", function(object) {
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
