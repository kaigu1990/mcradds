#' Robust Method in Calculation of Reference Interval
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This robust method is used to calculate the reference interval on small sample
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
#' This nonparametric method is used to calculate the reference interval when the
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
  nonpar_lower <- quantile(x, (1 - conf.level) / 2)
  nonpar_upper <- quantile(x, 1 - ((1 - conf.level) / 2))

  c(nonpar_lower, nonpar_upper)
}


#' Calculate Reference Interval and Corresponding Confidence Interval
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function is used to establish the reference interval for target population
#' with parametric, non-parametric and robust methods that follows the CLSI-EP28A3
#' and NMPA guideline. In additional, it also provides the corresponding confidence
#' interval for lower/upper reference limit if needed. Given that outliers should be
#' identified beforehand, Tukey and Dixon methods can be applied depending on
#' distribution of the data.
#'
#' @note There are some conditions of use to be aware of:
#' - If parametric method is used to calculate reference interval, confidence
#' interval should be the same method as well.
#' - If non-parametric method is used to calculate the reference interval and
#' the sample size is up to 120 observations, the non-parametric is suggested for
#' confidence interval. Otherwise if the sample size is below to 120, the bootstrap
#' method is the better choice. Beside the non-parametric method for confidence
#' interval only allows the `refLevel=0.95` and `confLevel=0.9` arguments,
#' if not the bootstrap methods will be used automatically.
#' - If robust method is used to calculate the reference interval, the method for
#' confidence interval must be bootstrap.
#'
#' @param x (`numeric`)\cr numeric measurements from target population.
#' @param out_method (`string`)\cr string specifying the which outlier detection to use.
#' @param out_rm (`logical`)\cr weather the outliers is removed or not.
#' @param RI_method (`string`)\cr string specifying the which method for computing
#' reference interval to use. Default is `parametric`, options can be `nonparametric`
#' and `robust`.
#' @param CI_method (`string`)\cr string specifying the which method for computing
#' confidence interval of reference limit(lower or upper) to use. Default is
#' `parametric`, options can be `nonparametric` and `boot`.
#' @param refLevel (`numeric`)\cr reference range/interval, usual is 0.95.
#' @param bootCI (`string`)\cr string specifying the which bootstrap confidence
#' interval from `boot.ci()` function in `boot` package. Default is
#' `perc`(bootstrap percentile), options can be `norm`(normal approximation),
#' `boot`(basic bootstrap), `stud`(studentized bootstrap) and `bca`(adjusted
#' bootstrap percentile).
#' @param confLevel (`numeric`)\cr significance level for the confidence interval
#' of reference limit.
#' @param rng.seed (`integer`)\cr number of the random number generator seed
#' for bootstrap sampling. If set to NULL currently in the R session used RNG
#' setting will be used.
#' @param tol (`numeric`)\cr tolerance for when the iterative process can be
#' stopped in robust method.
#' @param R (`integer`)\cr number of bootstrap replicates, is used in `boot()` function.
#'
#' @return A `RefInt` object contains relevant results in establishing of reference interval.
#' @export
#'
#' @examples
#' data("calcium")
#' x <- calcium$Value
#' refInterval(x, RI_method = "parametric", CI_method = "parametric")
#' refInterval(x, RI_method = "nonparametric", CI_method = "nonparametric")
#' refInterval(x, RI_method = "robust", CI_method = "boot")
refInterval <- function(x, out_method = c("doxin", "tukey"),
                        out_rm = FALSE,
                        RI_method = c("parametric", "nonparametric", "robust"),
                        CI_method = c("parametric", "nonparametric", "boot"),
                        refLevel = 0.95,
                        bootCI = c("perc", "norm", "basic", "stud", "bca"),
                        confLevel = 0.9,
                        rng.seed = NULL, tol = 1e-06, R = 1e+04) {
  assert_numeric(x, any.missing = FALSE)
  assert_logical(out_rm)
  assert_numeric(refLevel, lower = 0.7, upper = 1)
  out_method <- match.arg(out_method, c("doxin", "tukey"), several.ok = FALSE)
  RI_method <- match.arg(RI_method, c("parametric", "nonparametric", "robust"), several.ok = FALSE)
  CI_method <- match.arg(CI_method, c("parametric", "nonparametric", "boot"), several.ok = FALSE)
  bootCI <- match.arg(bootCI, c("perc", "norm", "basic", "stud", "bca"), several.ok = FALSE)

  rd <- x

  if (out_method == "tukey") {
    outres <- tukey_outlier(x)
  }
  if (out_method == "doxin") {
    outres <- dixon_outlier(x)
  }
  if (out_rm == TRUE) {
    x <- outres$subset
  }

  n <- length(x)

  if (RI_method == "parametric") {
    sd <- sd(x)
    z <- qnorm(1 - ((1 - refLevel) / 2))
    refLimit_lower <- mean(x) - z * sd
    refLimit_upper <- mean(x) + z * sd

    if (CI_method != "parametric") {
      warning("As the parametric RI is selected, the parametric of CI is used automatically.")
      CI_method <- "parametric"
    }

    if (CI_method == "parametric") {
      # the Var of reference limit can be calculated as
      se <- sd * sqrt(1 / n + z^2 / (2 * (n - 1)))
      zconf <- qnorm(1 - ((1 - confLevel) / 2))
      refLowerLimit_lower <- refLimit_lower - zconf * se
      refLowerLimit_upper <- refLimit_lower + zconf * se
      refUpperLimit_lower <- refLimit_upper - zconf * se
      refUpperLimit_upper <- refLimit_upper + zconf * se
    }
  }

  if (RI_method == "nonparametric") {
    x <- sort(x)
    npar <- nonparRI(x, conf.level = refLevel)
    refLimit_lower <- as.numeric(npar[1])
    refLimit_upper <- as.numeric(npar[2])

    if (!CI_method %in% c("nonparametric", "boot")) {
      stop("If RI is established by nonparametric, the CI should be nonparametric or boot")
    }
    if (n < 120 & CI_method != "boot") {
      warning("Sample size is too small (<120) for non-parametric CI, boot CI is used automatically.")
      CI_method <- "boot"
    }
    if (refLevel != 0.95 | confLevel != 0.9) {
      warning("The non-parametric RI only works when `refLevel=0.95` and `confLevel=0.9`, boot CI is used automatically.")
      CI_method <- "boot"
    }

    if (CI_method == "nonparametric") {
      ranks <- nonparRanks[which(nonparRanks$SampleSize == n), ]
      refLowerLimit_lower <- x[ranks$Lower]
      refLowerLimit_upper <- x[ranks$Upper]
      refUpperLimit_lower <- x[n + 1 - ranks$Upper]
      refUpperLimit_upper <- x[n + 1 - ranks$Lower]
    }
  }

  if (RI_method == "robust") {
    robust <- robustRI(x, conf.level = refLevel, tol = tol)
    refLimit_lower <- robust[1]
    refLimit_upper <- robust[2]

    if (!CI_method %in% c("boot")) {
      warning("Boot CI is used automatically.")
      CI_method <- "boot"
    }
  }

  if (CI_method == "boot" & RI_method %in% c("nonparametric", "robust")) {
    if (!is.null(rng.seed)) {
      set.seed(rng.seed)
    }
    if (RI_method == "nonparametric") {
      print("Bootstrape process could take a short while.")
      bootres <- boot(data = x, statistic = nonparRI, R = R, conf.level = refLevel)
    } else if (RI_method == "robust") {
      print("Bootstrape process could take a short while.")
      bootres <- boot(data = x, statistic = robustRI, R = R, conf.level = refLevel, tol = tol)
    }
    bootlowerci <- boot.ci(bootres, conf = confLevel, type = bootCI, index = c(1, 2))
    bootupperci <- boot.ci(bootres, conf = confLevel, type = bootCI, index = c(2, 2))
    bootlen <- length(bootlowerci[[4]])
    refLowerLimit_lower <- bootlowerci[[4]][bootlen - 1]
    refLowerLimit_upper <- bootlowerci[[4]][bootlen]
    refUpperLimit_lower <- bootupperci[[4]][bootlen - 1]
    refUpperLimit_upper <- bootupperci[[4]][bootlen]
  }

  methods <- paste0(
    "Reference Interval Method: ", RI_method,
    ", Confidence Interval Method: ", CI_method
  )

  RefInt(
    call = match.call(),
    method = methods,
    n = n,
    data = rd,
    outlier = outres,
    refInt = c(refLimit_lower, refLimit_upper),
    confInt = list(
      refLower = c(refLowerLimit_lower, refLowerLimit_upper),
      refUpper = c(refUpperLimit_lower, refUpperLimit_upper)
    )
  )
}
