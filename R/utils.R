#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Concatenate and Print with Newline
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function concatenates inputs like [cat()] and prints them with newline.
#'
#' @seealso This is similar to [cli::cat_line()].
#'
#' @param ... inputs to concatenate.
#'
#' @return None, only used for the side effect of producing the concatenated output in the R console.
#'
#' @export
#'
#' @examples
#' cat_with_newline("hello", "world")
cat_with_newline <- function(...) {
  cat(...)
  cat("\n", append = TRUE)
}
