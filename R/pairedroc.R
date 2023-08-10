#' AUC Test for Paired Two-sample Measurements
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This function compares two AUC of paired two-sample diagnostic assays by
#' standardized difference method, which has a little difference in SE calculation
#' with unpaired design. In order to compare the two assays, this function provides
#' three assessments including 'difference', 'non-inferiority' and 'superiority'.
#' This method of comparing is referred from Liu(2006)'s article that can be
#' found in reference section below.
#'
#' @param x (`numeric`)\cr reference/standard diagnostic assay.
#' @param y (`numeric`)\cr test diagnostic assay.
#' @param response (`numeric` or `factor`)\cr a vector of responses to represent
#'  the type of classes, typically encoded with 0(controls) and 1(cases).
#' @param h0 (`numeric`)\cr a specified hypothesized value of the margin between
#'  the two assays, default is 0 for difference method. If you select the
#'  non-inferiority method, the `h0` should be negative value. And if select
#'  superiority method, then it's non-negative value.
#' @param conf.level (`numeric`)\cr significance level between 0 and 1 (non-inclusive)
#'  for the returned confidence interval.
#' @param method (`string`)\cr string specifying the type of hypothesis test,
#'  must be one of "difference" (default), "non-inferiority" or "superiority".
#'  @param ... other arguments to be passed to [pROC::roc()].
#'
#' @details
#' If the samples are not considered independent, such as in a paired design,
#' the SE can not be computed by the method of Delong provided in `pROC` package.
#' Here the `aucTest` function use the standardized difference approach from
#' Liu(2006) publication to compute the SE and corresponding hypothesis test
#' statistic for a paired design study.
#' - `difference` is to test the difference between two diagnostic tests, the
#' default h0 is zero.
#' - `non-inferiority` is to test the new diagnostic tests is no worse than the
#' standard diagnostic test in a specific margin, but the same time maybe it's
#' safer, easier to administer or cost less.
#' - `superiority` is to test the test the new diagnostic tests is better than the
#' standard diagnostic test in a specific margin(default is zero), having better efficacy.
#'
#' @note
#' The test of significance for the difference is not equal to the result of EP24A2
#' Appendix D. Table D2. Because the Table D2 uses the method of Hanley & McNeil
#' (1982), whereas this function here uses the method of DeLong et al. (1988), which
#' results in the difference of SE. Thus the corresponding Z statistic and P value
#' will be not equal as well.
#'
#' @references Jen-Pei Liu (2006) "Tests of equivalence and non-inferiority for
#' diagnostic accuracy based on the paired areas under ROC curves". *Statist. Med.*
#' , 25:1219–1238. DOI: 10.1002/sim.2358.
#'
#' @return A `RefInt` object contains relevant results in comparing the paired
#' ROC of two-sample assays.
#' @export
#'
#' @examples
#' data("ldlroc")
#' # H0 : Difference between areas = 0:
#' aucTest(x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis)
#'
#' # H0 : Superiority margin <= 0.1:
#' aucTest(
#'   x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis,
#'   method = "superiority", h0 = 0.1
#' )
#'
#' # H0 : Non-inferiority margin <= -0.1:
#' aucTest(
#'   x = ldlroc$LDL, y = ldlroc$OxLDL, response = ldlroc$Diagnosis,
#'   method = "non-inferiority", h0 = -0.1
#' )
aucTest <- function(x, y,
                    response,
                    h0 = 0,
                    conf.level = 0.95,
                    method = c("difference", "non-inferiority", "superiority"),
                    ...) {
  assert_numeric(x)
  assert_numeric(y)
  assert_true(length(x) == length(y))
  assert_number(h0)
  method <- match.arg(method, c("difference", "non-inferiority", "superiority"), several.ok = FALSE)

  refroc <- pROC::roc(response, x)
  testroc <- suppressMessages(pROC::roc(response, y))
  paircov <- pROC::cov(refroc, testroc)
  refvar <- pROC::var(refroc)
  testvar <- pROC::var(testroc)

  auc_d <- as.numeric(testroc$auc) - as.numeric(refroc$auc)
  se <- sqrt(testvar + refvar - 2 * paircov)
  sign_z <- qnorm(1 - ((1 - conf.level) / 2))
  ci <- c(auc_d - sign_z * se, auc_d + sign_z * se)

  zstat <- (auc_d - h0) / se
  pval <- if (method == "difference") {
    2 * pnorm(abs(zstat), lower.tail = F)
  } else if (method == "non-inferiority") {
    if (h0 > 0) {
      stop("h0 should be negative when non-inferiority method is selected.")
    }
    pnorm(abs(zstat), lower.tail = F)
  } else if (method == "superiority") {
    if (h0 < 0) {
      stop("h0 should be non-negative when superiority method is selected.")
    }
    pnorm(abs(zstat), lower.tail = F)
  }

  tpROC(
    testROC = c(unclass(testroc), list(
      ci = ci.auc(testroc)[c(1, 3)],
      se = sqrt(var(testroc))
    )),
    refROC = c(unclass(refroc), list(
      ci = ci.auc(refroc)[c(1, 3)],
      se = sqrt(var(refroc))
    )),
    method = method,
    H0 = h0,
    stat = list(
      diffauc = auc_d,
      se = se,
      ci = ci,
      zstat = zstat,
      pval = pval
    )
  )
}
