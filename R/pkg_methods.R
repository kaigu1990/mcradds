# show ----

#' Show Method for `SampleSize` Objects
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A show method that displays essential information of `SampleSize` objects.
#'
#' @rdname show
#' @aliases show
#'
#' @param object (`SampleSize`)\cr input.
#' @return None (invisible `NULL`), only used for the side effect of printing to
#'   the console.
#'
#' @export
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
