#' Compute Critical Value for ESD Test
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A helper function to find the lambda for all potential outliers in each iteration.
#'
#' @param alpha (`numeric`)\cr type-I-risk, \eqn{\alpha}.
#' @param N (`integer`)\cr the total number of samples.
#' @param i (`integer`)\cr the iteration number, less than the number of  biggest
#' potential outliers.
#'
#' @return a lambda value calculated from the formula.
#' @export
#'
#' @examples
#' esd.critical(alpha = 0.05, N = 100, i = 1)
esd.critical <- function(alpha, N, i) {
  v <- N - i - 1
  p <- 1 - alpha / (2 * (N - i + 1))
  t <- qt(p, v)
  lambda <- t * (N - i) / sqrt((N - i + 1) * (v + t^2))

  lambda
}


#' EDS Test for Outliers
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Perform Rosner's generalized extreme Studentized deviate (ESD) test, which assumes
#' that the distribution is normal (Gaussian), can be used when the number of outliers
#' is unknown, and becomes more robust as the number of samples increases.
#'
#' @param x (`numeric`)\cr vector of observations that can be the difference from
#' Bland-Altman analysis. Normally the relative difference is preferred in IVD trials.
#' Missing(NA) is allowed but will be removed. There must be at least 10 available
#' observations in `x`.
#' @param alpha (`numeric`)\cr type-I-risk, \eqn{\alpha}.
#' @param h (`integer`)\cr the positive integer indicating the number of suspected
#' outliers. The argument `h` must be between 1 and `n-2` where n denotes the number of
#' available values in `x`. The default value is `h = 5`.
#'
#' @references CLSI EP09A3 Appendix B. Detecting Aberrant Results (Outliers).
#'
#' @return A list class containing the results of the ESD test.
#' - `stat` a data frame contains the several statistics about ESD test that includes
#' the index(`i`), Mean, SD, raw data(`x`), the location(`Obs`) in `x`, ESD statistics(ESDi),
#' Lambda and Outliers(`TRUE` or `FALSE`).
#' - `ord` a vector with the order index of outliers that is equal to `Obs` in
#' the `stat` data frame.
#' @export
#'
#' @note The algorithm for determining the number of outliers is as follows:
#' - Compare ESDi with Lambda. If ESDi > Lambda then the observations will be
#' regards as outliers.
#' - The order index corresponds to the available `x` data that has been removed the
#' missing (NA) value.
#' - As we should compare if the ESD(h) and ESD(h+1) are equal, the h+1 ESD values
#' will be shown. If they are identical, both of them can not be regarded as outliers.
#'
#' @examples
#' data("platelet")
#' res <- blandAltman(x = platelet$Comparative, y = platelet$Candidate)
#' ESD_test(x = res@stat$relative_diff)
ESD_test <- function(x, alpha = 0.05, h = 5) {
  assert_numeric(x)
  assert_int(h, lower = 1, upper = length(x) - 2)

  rd <- x
  x <- na.omit(x)
  n <- length(x)
  obsN <- 1:n

  if (h > floor(0.05 * n)) {
    warning("No more than 5% of sample results can be flagged as potential outliers.")
  }

  mat <- data.frame(stringsAsFactors = F)
  for (i in 1:(h + 1)) {
    mn <- mean(x)
    sd <- sd(x)
    if (sd(x) == 0) {
      break
    } else {
      temp <- abs(x - mn) / sd
    }
    esdi <- max(temp)
    index <- which(temp == esdi)[1]
    obs <- obsN[index]

    lambda <- esd.critical(alpha, n, i)
    mat <- rbind(
      mat,
      data.frame(i = i, Mean = mn, SD = sd, x = x[index], Obs = obs, ESDi = esdi, Lambda = lambda)
    )

    # iterated remove esdi
    x <- x[-index]
    obsN <- obsN[-index]
  }

  mat$Outlier <- ifelse(mat$ESDi > mat$Lambda, TRUE, FALSE)

  # If ESDh and ESDh+1 are equal(a tie) then neither one should be seen as an outlier
  # The number of outliers is determined by finding the largest i such that ESDi > λi
  if (mat$ESDi[h] == mat$ESDi[h + 1]) {
    mat$Outlier[h] <- mat$Outlier[h + 1] <- FALSE
  }

  list(stat = mat, ord = mat$Obs[mat$Outlier == TRUE])
}


#' Detect Tukey Outlier
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Help function detects the potential outlier with Tukey method where the number
#' is below `Q1-1.5*IQR` and above `Q3+1.5*IQR`.
#'
#' @param x (`numeric`)\cr numeric input
#'
#' @return A list contains outliers and vector without outliers.
#' @export
#'
#' @examples
#' x <- c(13.6, 44.4, 45.9, 14.9, 41.9, 53.3, 44.7, 95.2, 44.1, 50.7, 45.2, 60.1, 89.1)
#' tukey_outlier(x)
tukey_outlier <- function(x) {
  assert_numeric(x)

  qt <- quantile(x)
  q1 <- as.numeric(qt[2])
  q3 <- as.numeric(qt[4])
  iqr <- q3 - q1
  ord <- which(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr))
  list(ord = ord, out = x[ord], subset = x[-ord])
}

#' Detect Dixon Outlier
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' Help function detects the potential outlier with Dixon method, following the
#' rules of EP28A3 and NMPA guideline for establishment of reference range.
#'
#' @param x (`numeric`)\cr numeric input.
#'
#' @return A list contains outliers and vector without outliers.
#' @export
#'
#' @examples
#' x <- c(13.6, 44.4, 45.9, 11.9, 41.9, 53.3, 44.7, 95.2, 44.1, 50.7, 45.2, 60.1, 89.1)
#' dixon_outlier(x)
dixon_outlier <- function(x) {
  assert_numeric(x)

  or_ord <- order(x)
  d <- x[or_ord]
  R <- max(d) - min(d)

  upr <- which(d > median(d))
  lwr <- which(d <= median(d))

  outord <- c()
  while (length(upr) > 1) {
    D <- abs(d[upr[length(upr)]] - d[upr[(length(upr) - 1)]])
    if (D / R > 1 / 3) {
      outord <- c(outord, max(upr))
    }
    upr <- upr[-length(upr)]
  }
  if (is.null(outord)) {
    ord1 <- outord
  } else if (max(outord) != length(d)) {
    ord1 <- or_ord[c(outord, (max(outord) + 1):length(d))]
  }

  outord <- c()
  while (length(lwr) > 1) {
    D <- abs(d[lwr[1]] - d[lwr[2]])
    if (D / R > 1 / 3) {
      outord <- c(outord, lwr[1])
    }
    lwr <- lwr[-1]
  }
  if (is.null(outord)) {
    ord2 <- outord
  } else if (max(outord) != length(d)) {
    ord2 <- or_ord[c(outord, 1:(min(outord) - 1))]
  }

  ord <- sort(c(ord1, ord2))
  if (is.null(ord)) {
    list(ord = ord, out = x[ord], subset = x)
  } else {
    list(ord = ord, out = x[ord], subset = x[-ord])
  }
}
