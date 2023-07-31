#' Robust Method in Calculation of Reference Interval
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This robust method is used to calculate the reference internal on small sample
#' size (below to 120 observations).
#'
#' @param x (`numeric`)\cr numeric measurements from target population.
#' @param ind (`integer`)\cr integer vector for boot process, default is all elements
#' in `x`.
#' @param conf.level (`numeric`)\cr significance level for the internal t statistic.
#' @param tol (`numeric`)\cr tolerance for when the iterative process can be stopped.
#'
#' @return a vector of robust reference interval
#' @export
#'
#' @references This robust algorithm is referring to CLSI document EP28A3.
#'
#' @examples
#' # This example data is taken from EP28A3 Appendix B. to ensure the result is in accordance.
#' x <- c(8.9, 9.2, rep(9.4, 2), rep(9.5, 3), rep(9.6, 4), rep(9.7, 5), 9.8, rep(9.9, 2), 10.2)
#' robustRI(x)
robustRI <- function(x, ind = 1:length(x), conf.level = 0.95, tol = 1e-06) {
  assert_numeric(x)
  assert_vector(ind)
  assert_number(conf.level, lower = 0.7, upper = 1)

  x <- sort(x[ind])
  n <- length(x)
  median <- median(x)
  Tbi <- median
  c <- 3.7
  MAD <- median(abs(x - median))
  repeat {
    ui <- (x - Tbi) / (c * MAD / 0.6745)
    wi <- ifelse(ui > 1 | ui < -1, 0, (1 - ui^2)^2)
    TbiNew <- sum(x * wi) / sum(wi)
    if (!is.finite(TbiNew) | (abs(TbiNew - Tbi)) < tol) {
      break
    }
    Tbi <- TbiNew
  }

  fm <- function(n, ui) {
    ui <- ui[ui > -1 & ui < 1]
    sqrt((n * sum(ui^2 * (1 - ui^2)^4))
         / (sum((1 - ui^2) * (1 - 5 * ui^2)) *
              max(c(1, -1 + sum((1 - ui^2) * (1 - 5 * ui^2))))))
  }
  fm2 <- function(ui) {
    ui <- ui[ui > -1 & ui < 1]
    sqrt(sum(ui^2 * (1 - ui^2)^4)
         / (sum((1 - ui^2) * (1 - 5 * ui^2)) *
              max(c(1, -1 + sum((1 - ui^2) * (1 - 5 * ui^2))))))
  }

  ui <- (x - median) / (205.6 * MAD / 0.6745)
  sbi205_6 <- 205.6 * MAD / 0.6745 * fm(n, ui)

  ui <- (x - median) / (3.7 * MAD / 0.6745)
  sbi3_7 <- 3.7 * MAD / 0.6745 * fm(n, ui)

  ui <- (x - Tbi) / (3.7 * sbi3_7)
  St3_7 <- 3.7 * sbi3_7 * fm2(ui)

  t <- qt(1 - ((1 - conf.level) / 2), (n - 1))
  robust_lower <- Tbi - t * sqrt(sbi205_6^2 + St3_7^2)
  robust_upper <- Tbi + t * sqrt(sbi205_6^2 + St3_7^2)

  c(robust_lower, robust_upper)
}

#' Nonparametric Method in Calculation of Reference Interval
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This nonparametric method is used to calculate the reference internal when the
#' distribution is skewed and the sample size is above to 120 observations.
#'
#' @param x (`numeric`)\cr numeric measurements from target population.
#' @param ind (`integer`)\cr integer vector for boot process, default is all
#' elements in `x`.
#' @param conf.level (`numeric`)\cr the percentile of reference limit.
#'
#' @return a vector of nonparametric reference interval
#' @export
#'
#' @examples
#' data("calcium")
#' x <- calcium$Value
#' nonparRI(x)
nonparRI <- function(x, ind = 1:length(x), conf.level = 0.95) {
  assert_numeric(x)
  assert_vector(ind)
  assert_number(conf.level, lower = 0.7, upper = 1)

  x <- x[ind]
  nonpar_lower <- quantile(x, (1 - conf.level)/2, type = 3)
  nonpar_upper <- quantile(x, 1-((1 - conf.level)/2), type = 3)

  c(nonpar_lower, nonpar_upper)
}
