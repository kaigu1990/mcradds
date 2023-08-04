# anovaVCA ----

#' ANOVA-Type Estimation of Variance Components for Random Models
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A copy from [VCA::anovaVCA] in `VCA` package
#'
#' @inheritDotParams VCA::anovaVCA
#'
#' @return a class of `VCA` for downstream analysis.
#' @seealso [VCA::anovaVCA()]
#' @export
#' @examples
#' data(glucose)
#' anovaVCA(value ~ day / run, glucose)
anovaVCA <- function(...) {
  VCA::anovaVCA(...)
}

# VCAinference ----

#' Inferential Statistics for VCA-Results
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' A copy from [VCA::VCAinference] in `VCA` package
#'
#' @inheritDotParams VCA::VCAinference
#'
#' @return object of `VCAinference` contains a series of statistics.
#' @seealso [VCA::VCAinference()]
#' @export
#' @examples
#' data(glucose)
#' fit <- anovaVCA(value ~ day / run, glucose)
#' VCAinference(fit)
VCAinference <- function(...) {
  VCA::VCAinference(...)
}
