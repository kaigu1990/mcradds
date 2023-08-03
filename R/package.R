#' `mcradds` Package
#'
#' `mcradds` Processing and analyzing of In Vitro Diagnostic Data.
#'
"_PACKAGE"

#' @import mcr
#' @import checkmate
#' @import ggplot2
#' @import methods
#' @importFrom stats qt na.omit terms qnorm uniroot complete.cases
#' quantile setNames pnorm cor.test median
#' @importFrom purrr map_chr
#' @importFrom DescTools BinomCI
#' @importFrom lifecycle deprecated
#' @importFrom boot boot boot.ci
NULL

# Resolve missing global definitions:
utils::globalVariables(c(
  "nonparRanks"
))
