# Test for One Proportion ----

#' Sample size determination for testing one proportion
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function performs sample size computation for testing one proportion
#' in accordance with Chinese NMPA's IVD guideline.
#'
#' @param p1 (numeric)\cr expected criteria of the evaluated assay.
#' @param p0 (numeric)\cr acceptable criteria of the evaluated assay.
#' @param alpha (numeric)\cr type-I-risk, \eqn{\alpha}.
#' @param power (numeric)\cr Power of test, equal to 1 minus type-II-risk (\eqn{\beta}).
#' @param alternative (character)\cr string specifying the alternative hypothesis,
#'    must be one of "two.sided" (default), "greater" or "less".
#'
#' @return an object of `size` class that contains the sample size and relevant parameters.
#'
#' @export
#' @seealso [size_ci_one_prop()]
#' @references Chinese NMPA's IVD technical guideline.
#'
#' @examples
#' size_one_prop(p1 = 0.95, p0 = 0.9, alpha = 0.05, power = 0.8)
size_one_prop <- function(p1, p0, alpha = 0.05, power = 0.8,
                          alternative = c("two.sided", "less", "greater")) {
  assert_numeric(p1)
  assert_numeric(p0)
  assert_numeric(alpha, lower = 0, upper = 1)
  assert_numeric(power, lower = 0, upper = 1)
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"), several.ok = FALSE)
  args <- c(as.list(environment()))

  side <- switch(alternative,
    two.sided = 2,
    less = 1,
    greater = 1
  )
  z_alpha <- qnorm(1 - alpha / side)
  z_beta <- qnorm(power)
  n <- (z_alpha * sqrt(p0 * (1 - p0)) + z_beta * sqrt(p1 * (1 - p1)))^2 / (p1 - p0)^2

  object <- list(
    call = match.call(),
    method = "Sample size determination for one Proportion",
    n = n,
    param = args
  )
  object
}


# Confidence Interval for One Proportion ----

#' Sample size determination for testing a given lower confidence interval of one proportion
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function performs sample size computation for testing a given lower
#' confidence interval of one proportion with the using of the
#' Simple Asymptotic(Wald), Wilson score, clopper-pearson and other methods.
#'
#' @param p (numeric)\cr expected criteria of the evaluated assay.
#' @param lr (numeric)\cr acceptable criteria of the evaluated assay.
#' @param alpha (numeric)\cr type-I-risk, \eqn{\alpha}.
#' @param interval (numeric)\cr a numeric vector containing the end-points of the interval
#'  to be searched for the root(sample size). The defaults are set to c(1, 100000).
#' @param tol (numeric)\cr tolerance for searching the root(sample size).
#' @param alternative (character)\cr string specifying the alternative hypothesis,
#'  must be one of "two.sided" (default), "greater" or "less".
#' @param method (character)\cr string specifying the which method to use.
#'  Simple Asymptotic is default, equal to Wald. Options can be "wilson",
#'  "clopper-pearson" and other method, see [DescTools::BinomCIn]
#'
#' @return an object of `size` class that contains the sample size and relevant parameters.
#'
#' @export
#' @seealso [size_one_prop()]
#' @references Newcombe, R. G. 1998. 'Two-Sided Confidence Intervals for the
#'  Single Proportion: Comparison of Seven Methods.' Statistics in Medicine, 17, pp. 857-872.
#'
#' @importFrom DescTools BinomCI
#' @importFrom stats uniroot
#'
#' @examples
#' size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "wilson")
#' size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "simple-asymptotic")
#' size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "wald")
size_ci_one_prop <- function(p, lr, alpha = 0.05,
                             interval = c(1, 100000), tol = 1e-05,
                             alternative = c("two.sided", "less", "greater"),
                             method = c("simple-asymptotic", "wilson", "wald", "clopper-pearson")) {
  assert_numeric(p)
  assert_numeric(lr)
  assert_true(p > lr)
  assert_numeric(alpha, lower = 0, upper = 1)
  assert_vector(interval, len = 2)
  assert_numeric(tol)
  assert_choice(method, choices = c(
    "simple-asymptotic", "wald", "wilson", "wilsoncc", "agresti-coull",
    "jeffreys", "modified wilson", "modified jeffreys",
    "clopper-pearson", "arcsine", "logit", "witting", "pratt"
  ))
  method <- match.arg(method, c("simple-asymptotic", "wald", "wilson", "clopper-pearson"), several.ok = FALSE)
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"), several.ok = FALSE)
  args <- c(as.list(environment()))

  n <- if (method == "simple-asymptotic") {
    side <- switch(alternative,
      two.sided = 2,
      less = 1,
      greater = 1
    )
    qnorm(1 - alpha / side)^2 * p * (1 - p) / (p - lr)^2
  } else {
    uniroot(f = function(n) {
      lr0 <- DescTools::BinomCI(
        x = p * n, n = n, conf.level = 1 - alpha, sides = alternative, method = method
      )[2]
      return(lr0 - lr)
    }, tol = tol, interval = interval)$root
  }

  object <- list(
    call = match.call(),
    method = "Sample size determination for a Given Lower Confidence Interval",
    n = n,
    param = args
  )
  object
}
