#' Hypothesis test for Pearson correlation coefficient
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Adjust the `cor.test` function so that it can define the specific H0 as per
#' your request, that is based on Fisher's Z transformation of the correlation.
#'
#' @param x (`numeric`)\cr one measurement.
#' @param y (`numeric`)\cr another measurement.
#' @param h0 (`numeric`)\cr a specified hypothesized value of the difference between
#' the two correlations, default is 0.
#' @param conf.level (`numeric`)\cr significance level for the returned confidence
#' interval and hypothesis.
#' @param alternative (`string`)\cr string specifying the alternative hypothesis,
#'    must be one of "two.sided" (default), "greater" or "less".
#' @param ... other arguments to be passed to [cor.test()].
#'
#' @return a named vector contains correlation coefficient (`cor`), confidence
#' interval(`lowerci` and `upperci`), Z statistic (`Z`) and p-value (`pval`)
#' @export
#'
#' @seealso [cor.test()] to see the detailed arguments.
#' @references NCSS correlation document
#'
#' @examples
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
#' pearsonTest(x, y, h0 = 0.5, alternative = "greater")
pearsonTest <- function(x, y,
                        h0 = 0,
                        conf.level = 0.95,
                        alternative = c("two.sided", "less", "greater"),
                        ...) {
  assert_numeric(x)
  assert_numeric(y)
  assert_true(length(x) == length(y))
  assert_number(h0, lower = -1, upper = 1)
  assert_number(conf.level, lower = 0.7, upper = 1)
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"), several.ok = FALSE)

  corr <- suppressWarnings(cor.test(x, y,
    alternative = c("two.sided", "less", "greater"),
    method = "pearson", conf.level = conf.level, ...
  ))

  rho <- as.numeric(corr$estimate)
  ci <- corr$conf.int
  n <- length(x)
  z <- (1 / 2 * log((1 + rho) / (1 - rho)) - 1 / 2 * log((1 + h0) / (1 - h0))) / sqrt(1 / (n - 3))
  pval <- if (alternative == "two.sided") {
    2 * pnorm(abs(z), lower.tail = F)
  } else if (alternative == "greater") {
    pnorm(abs(z), lower.tail = F)
  } else if (alternative == "less") {
    pnorm(abs(z), lower.tail = T)
  }

  list(
    stat = setNames(c(rho, ci, z, pval), c("cor", "lowerci", "upperci", "Z", "pval")),
    method = "Pearson's correlation",
    conf.level = conf.level
  )
}

#' Hypothesis test for Spearman correlation coefficient
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Providing the confidence interval of Spearman's rank correlation by Bootstrap,
#' and define the specific H0 as per your request, that is based on Fisher's Z
#' transformation of the correlation but with the variance recommended by
#' Bonett and Wright (2000), not the same as Pearson's.
#'
#' @param x (`numeric`)\cr one measurement.
#' @param y (`numeric`)\cr another measurement.
#' @param h0 (`numeric`)\cr a specified hypothesized value of the difference between
#' the two correlations, default is 0.
#' @param conf.level (`numeric`)\cr significance level for the returned confidence
#' interval and hypothesis.
#' @param alternative (`string`)\cr string specifying the alternative hypothesis,
#'    must be one of "two.sided" (default), "greater" or "less".
#' @param nrep (`integer`)\cr number of replicates for bootstrapping, default is 1000.
#' @param rng.seed (`integer`)\cr number of the random number generator seed
#' for bootstrap sampling. If set to NULL currently in the R session used RNG
#' setting will be used.
#' @param ... other arguments to be passed to [cor.test()].
#'
#' @return a named vector contains correlation coefficient (`cor`), confidence
#' interval(`lowerci` and `upperci`), Z statistic (`Z`) and p-value (`pval`)
#' @export
#'
#' @seealso [cor.test()] [boot()] to see the detailed arguments.
#' @references NCSS correlation document
#'
#' @examples
#' x <- c(44.4, 45.9, 41.9, 53.3, 44.7, 44.1, 50.7, 45.2, 60.1)
#' y <- c(2.6, 3.1, 2.5, 5.0, 3.6, 4.0, 5.2, 2.8, 3.8)
#' spearmanTest(x, y, h0 = 0.5, alternative = "greater")
spearmanTest <- function(x, y,
                         h0 = 0,
                         conf.level = 0.95,
                         alternative = c("two.sided", "less", "greater"),
                         nrep = 1000,
                         rng.seed = NULL,
                         ...) {
  assert_numeric(x)
  assert_numeric(y)
  assert_true(length(x) == length(y))
  assert_number(h0, lower = -1, upper = 1)
  assert_number(conf.level, lower = 0.7, upper = 1)
  alternative <- match.arg(alternative, c("two.sided", "less", "greater"), several.ok = FALSE)
  assert_int(nrep, lower = 100)

  corr <- suppressWarnings(cor.test(x, y,
    alternative = c("two.sided", "less", "greater"),
    method = "spearman", conf.level = conf.level, ...
  ))
  rho <- as.numeric(corr$estimate)
  n <- length(x)

  cor.fun <- function(data, ind) {
    as.numeric(suppressWarnings(cor.test(data[ind, 1], data[ind, 2],
      method = "spearman", ...
    )$estimate))
  }
  if (!is.null(rng.seed)) {
    set.seed(rng.seed)
  }
  sim <- boot(data.frame(x, y), cor.fun, R = nrep)
  ci <- c(
    apply(sim$t, 2, quantile, probs = (1 - conf.level) / 2, type = 3),
    apply(sim$t, 2, quantile, probs = (1 + conf.level) / 2, type = 3)
  )

  z <- (1 / 2 * log((1 + rho) / (1 - rho)) - 1 / 2 * log((1 + h0) / (1 - h0))) /
    sqrt((1 + rho^2 / 2) / (n - 3))
  pval <- if (alternative == "two.sided") {
    2 * pnorm(abs(z), lower.tail = F)
  } else if (alternative == "greater") {
    pnorm(abs(z), lower.tail = F)
  } else if (alternative == "less") {
    pnorm(abs(z), lower.tail = T)
  }

  list(
    stat = setNames(c(rho, ci, z, pval), c("cor", "lowerci", "upperci", "Z", "pval")),
    method = "Spearman's correlation",
    conf.level = conf.level
  )
}
