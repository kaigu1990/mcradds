#' `mcradds` Package
#'
#' `mcradds` Processing and analyzing of In Vitro Diagnostic Data.
#'
"_PACKAGE"

#' @import checkmate
#' @import ggplot2
#' @import methods
#' @import mcr
#' @importFrom stats qt na.omit terms qnorm uniroot complete.cases
#' quantile setNames pnorm cor.test median sd as.formula
#' @importFrom purrr map_chr
#' @importFrom DescTools BinomCI CohenKappa
#' @importFrom lifecycle deprecated
#' @importFrom boot boot boot.ci
#' @importFrom VCA anovaVCA VCAinference
#' @importFrom pROC roc var cov ci.auc
#' @importFrom utils combn
#' @importFrom formatters format_value
#' @importFrom rlang sym := .data
NULL

# Resolve missing global definitions:
utils::globalVariables(c(
  "nonparRanks", "."
))

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.mcradds <- list(
    mcradds.stats.default = c("N", "MEAN", "SD", "MEDIAN", "MAX", "MIN"),
    mcradds.precision.default = tibble::tribble(
      ~stat,     ~extra,
      "N",       0,
      "MEAN",    1,
      "SD",      2,
      "MEDIAN",  1,
      "MAX",     0,
      "MIN",     0,
      "Q1",      1,
      "Q3",      1
    )
  )
  toset <- !(names(op.mcradds) %in% names(op))
  if (any(toset)) {
    options(op.mcradds[toset])
  }

  invisible()
}
