#' Compute the Critical Value for ESD Test
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
#' x <- res@stat$relative_diff
#' ESD_test(x)
ESD_test <- function(x, alpha = 0.05, h = 5) {
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
