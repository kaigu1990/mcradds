#' Example `platelet` Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This example [`mcradds::platelet`] can be used to create a data set comparing
#' Platelet results from two analyzers in cells/μL.
#'
#' @format A [mcradds::platelet] contains 120 observations and 3 variables.
#' \describe{
#'    \item{sample}{sample id}
#'    \item{Comparative}{Measurement from comparative analyzer}
#'    \item{Candidate}{Measurement from candidate analyzer}
#' }
#' @source CLSI-EP09 A3 Appendix H, Table H2 is cited in this data set.
#' @seealso [mcr::creatinine] which contains data with with serum and plasma
#' creatinin measurements in mg/dL for each sample.
"platelet"
