#' Example `platelet` Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This example [`mcradds::platelet`] can be used to create a data set comparing
#' Platelet results from two analyzers in cells/μL.
#'
#' @format A [mcradds::platelet] contains 120 observations and 3 variables.
#' \describe{
#'    \item{sample}{Sample id}
#'    \item{Comparative}{Measurement from comparative analyzer}
#'    \item{Candidate}{Measurement from candidate analyzer}
#' }
#' @source CLSI-EP09 A3 Appendix H, Table H2 is cited in this data set.
#' @seealso [mcr::creatinine] which contains data with with serum and plasma
#' creatinin measurements in mg/dL for each sample.
"platelet"


#' Example `qualData` Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This simulated data [`mcradds::qualData`] can be used to calculate the
#' quantitative performance such as sensitivity and specificity.
#'
#' @format A [mcradds::qualData] contains 200 observations and 3 variables.
#' \describe{
#'    \item{sample}{Sample id}
#'    \item{Comparative}{Measurement from comparative analyzer with 1=positive and 0=negative}
#'    \item{Candidate}{Measurement from candidate analyzer with 1=positive and 0=negative}
#' }
#'
#' @seealso [mcradds::platelet] which contains quantitative data comparing
#' platelet results from two analyzers.
"qualData"
