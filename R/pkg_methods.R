# show ----

#' Show Method for Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A show method that displays essential information of objects.
#'
#' @rdname show
#' @aliases show
#'
#' @param object (`SampleSize`)\cr input.
#' @return None (invisible `NULL`), only used for the side effect of printing to
#'   the console.
#'
#' @importFrom purrr map_chr
#'
#' @examples
#' size_one_prop(p1 = 0.95, p0 = 0.9, alpha = 0.05, power = 0.8)
#' size_ci_corr(r = 0.9, lr = 0.85, alpha = 0.025, alternative = "greater")
setMethod(
  f = "show",
  signature = "SampleSize",
  definition = function(object) {
    cat_with_newline("\n", object@method, "\n")
    cat(" Call: ", append = FALSE)
    show(object@call)
    cat_with_newline("\n", "  optimal sample size: n =", ceiling(object@n), "\n")
    cat_with_newline(
      "  ",
      map_chr(seq_along(object@param), ~ paste(names(object@param[.x]),
        object@param[.x],
        sep = ":"
      ))
    )
  }
)

#' @rdname show
#' @aliases show
#'
#' @param object (`MCTab`)\cr input.
#'
#' @examples
#' qualData %>% diagTab(formula = ~ CandidateN + ComparativeN)
setMethod(
  f = "show",
  signature = "MCTab",
  definition = function(object) {
    cat(" Contingency Table: \n\n")
    cat_with_newline(
      "  candidate has", length(object@candidate$levels), "levels:",
      object@candidate$levels
    )
    cat_with_newline(
      "  comparative has", length(object@candidate$levels), "levels:",
      object@candidate$levels, "\n"
    )
    show(object@tab)
  }
)

# getAccuracy ----

#' @rdname getAccuracy
#'
setGeneric("getAccuracy", function(object, ...) standardGeneric("getAccuracy"))
