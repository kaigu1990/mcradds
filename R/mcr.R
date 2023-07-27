# printSummary ----

#' Print Summary of a Regression Analysis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A copy from [mcr::printSummary] in `mcr` package
#'
#' @inheritDotParams mcr::printSummary
#'
#' @export
#' @examples
#' data(platelet)
#' fit <- mcreg(
#'   x = platelet$Comparative, y = platelet$Candidate,
#'   method.reg = "Deming", method.ci = "jackknife"
#' )
#' printSummary(fit)
printSummary <- function(...) {
  mcr::printSummary(...)
}


# getCoefficients ----

#' Get Regression Coefficients
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A copy from [mcr::getCoefficients] in `mcr` package
#'
#' @inheritDotParams mcr::getCoefficients
#'
#' @export
#' @examples
#' data(platelet)
#' fit <- mcreg(
#'   x = platelet$Comparative, y = platelet$Candidate,
#'   method.reg = "Deming", method.ci = "jackknife"
#' )
#' getCoefficients(fit)
getCoefficients <- function(...) {
  mcr::getCoefficients(...)
}


# mcreg ----

#' Comparison of Two Measurement Methods Using Regression Analysis
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A copy from [mcr::mcreg] in `mcr` package
#'
#' @inheritDotParams mcr::mcreg
#'
#' @return A regression fit model.
#' @seealso [mcr::mcreg()]
#' @export
#' @examples
#' data(platelet)
#' fit <- mcreg(
#'   x = platelet$Comparative, y = platelet$Candidate,
#'   method.reg = "Deming", method.ci = "jackknife"
#' )
#' printSummary(fit)
#' getCoefficients(fit)
mcreg <- function(...) {
  mcr::mcreg(...)
}


# calcBias ----

#' Systematical Bias Between Reference Method and Test Method
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A copy from [mcr::calcBias] in `mcr` package
#'
#' @inheritDotParams mcr::calcBias
#'
#' @return Bis and corresponding confidence interval for the specific medical
#' decision levels (`x.levels`).
#'
#' @seealso [mcr::calcBias()]
#' @export
#' @examples
#' data(platelet)
#' fit <- mcreg(
#'   x = platelet$Comparative, y = platelet$Candidate,
#'   method.reg = "Deming", method.ci = "jackknife"
#' )
#' calcBias(fit, x.levels = c(30, 200))
#' calcBias(fit, x.levels = c(30, 200), type = "proportional")
#' calcBias(fit, x.levels = c(30, 200), type = "proportional", percent = FALSE)
calcBias <- function(...) {
  mcr::calcBias(...)
}
