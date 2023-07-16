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


#' Factor Variable Per the Levels
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function factor inputs in order of appearance, or per the levels that you provide.
#'
#' @param df (`data.frame`)\cr input data.
#' @param var (`string`)\cr variable to factor.
#' @param levels (`vector`)\cr a character vector of known levels.
#' @param ... other arguments to be passed to [factor()].
#'
#' @return A factor variable
#' @export
#'
#' @examples
#' df <- data.frame(a = c("aa", "a", "aa"))
#' h_factor(df, var = "a")
#' h_factor(df, var = "a", levels = c("aa", "a"))
h_factor <- function(df, var, levels = NULL, ...) {
  assert_data_frame(df)
  assert_choice(var, names(df))

  if (!is.factor(df[[var]]) & is.null(levels)) {
    factor(df[[var]], ...)
  } else if (!is.factor(df[[var]]) & !is.null(levels)) {
    factor(df[[var]], levels = levels, ...)
  } else if (is.factor(df[[var]]) & !is.null(levels)) {
    factor(df[[var]], levels = levels, ...)
  } else {
    df[[var]]
  }
}
