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

#' Summarize the basic statistics
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Help function summarizes the statistics as needed.
#'
#' @param x (`numeric`)\cr input numeric vector.
#' @param confint (`numeric`)\cr significance level, default is 0.95.
#'
#' @return a verctor contains several statistics, such as n, mean, median, min,
#' max, q25, q75, sd, se, limit of agreement of limit and confidence interval .
#' @export
#'
#' @examples
#' h_summarize(1:50)
h_summarize <- function(x, confint = 0.95) {
  x <- na.omit(x)
  n <- length(x)
  mean <- mean(x)
  median <- median(x)
  min <- min(x)
  max <- max(x)
  q1 <- as.numeric(quantile(x)[2])
  q3 <- as.numeric(quantile(x)[4])
  sd <- sd(x)
  se <- sd(x) / sqrt(n)
  limit_up <- mean + qnorm(1 - (1 - confint) / 2) * sd
  limit_lr <- mean - qnorm(1 - (1 - confint) / 2) * sd
  ci_up <- mean + qnorm(1 - (1 - confint) / 2) * se
  ci_lr <- mean - qnorm(1 - (1 - confint) / 2) * se

  cbind(
    n = n, mean = mean, median = median, min = min, max = max, q1 = q1, q3 = q3,
    sd = sd, se = se, limit_lr = limit_lr, limit_up = limit_up, ci_lr = ci_lr, ci_up = ci_up
  )
}

#' Compute the difference for Bland-Altman
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Helper function computes the difference with specific type.
#'
#' @param x (`numeric`)\cr reference method
#' @param y (`numeric`)\cr test method
#' @param type (`integer`)\cr integer specifying a specific difference for Bland-Altman
#' (default is 3). Possible choices are:
#' 1 - difference with X vs. Y-X (absolute differences).
#' 2 - difference with X vs. (Y-X)/X (relative differences).
#' 3 - difference with 0.5*(X+Y) vs. Y-X (absolute differences).
#' 4 - difference with 0.5*(X+Y) vs. (Y-X)/X (relative differences).
#' 5 - difference with 0.5*(X+Y) vs. (Y-X)/(0.5*(X+Y)) (relative differences).
#'
#' @return a matrix contains the x and y measurement data and corresponding difference.
#' @export
#'
#' @examples
#' h_difference(x = c(1.1, 1.2, 1.5), y = c(1.2, 1.3, 1.4), type = 5)
h_difference <- function(x, y, type) {
  assert_choice(type, choices = 1:5)

  if (type == 1) {
    x_ba <- x
    y_ba <- y - x
  } else if (type == 2) {
    x_ba <- x
    y_ba <- (y - x) / x
  } else if (type == 3) {
    x_ba <- (x + y) / 2
    y_ba <- y - x
  } else if (type == 4) {
    x_ba <- (x + y) / 2
    y_ba <- (y - x) / x
  } else if (type == 5) {
    x_ba <- (x + y) / 2
    y_ba <- (y - x) / ((x + y) / 2)
  }

  cbind(x, y, x_ba, y_ba)
}
