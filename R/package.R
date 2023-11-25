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
#' quantile setNames pnorm cor.test median as.formula
#' @importFrom purrr map_chr
#' @importFrom DescTools BinomCI CohenKappa
#' @importFrom lifecycle deprecated
#' @importFrom boot boot boot.ci
#' @importFrom VCA anovaVCA VCAinference
#' @importFrom pROC roc var cov ci.auc
#' @importFrom utils combn
#' @importFrom formatters format_value
NULL

# Resolve missing global definitions:
utils::globalVariables(c(
  "nonparRanks"
))
