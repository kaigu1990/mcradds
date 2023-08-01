#' Example `platelet` Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This example [`mcradds::platelet`] can be used to create a data set comparing
#' Platelet results from two analyzers in cells/μL.
#'
#' @format A [mcradds::platelet] data set contains 120 observations and 3 variables.
#' \describe{
#'    \item{Sample}{Sample id}
#'    \item{Comparative}{Measurements from comparative analyzer}
#'    \item{Candidate}{Measurements from candidate analyzer}
#' }
#' @source CLSI-EP09 A3 Appendix H, Table H2 is cited in this data set.
#' @seealso [mcr::creatinine] that contains data with with serum and plasma
#' creatinin measurements in mg/dL for each sample.
"platelet"


#' Example `qualData` Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This simulated data [`mcradds::qualData`] can be used to calculate the
#' qualitative performance such as sensitivity and specificity.
#'
#' @format A [mcradds::qualData] data set contains 200 observations and 3 variables.
#' \describe{
#'    \item{Sample}{Sample id}
#'    \item{ComparativeN}{Measurements from comparative analyzer with `1=positive` and `0=negative`}
#'    \item{CandidateN}{Measurements from candidate analyzer with `1=positive` and `0=negative`}
#' }
#'
#' @seealso [mcradds::platelet] that contains quantitative data comparing
#' platelet results from two analyzers.
"qualData"

#' Example `calcium` Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This example [`mcradds::calcium`] can be used to compute the reference range of
#' Calcium in 240 medical students by sex.
#'
#' @format A [mcradds::calcium] data set contains 240 observations and 3 variables.
#' \describe{
#'    \item{Sample}{Sample id}
#'    \item{Value}{Measurements from target subjects}
#'    \item{Group}{Sex group of target subjects}
#' }
#' @source CLSI-EP28A3 Table 4. is cited in this data set.
#' @seealso [mcradds::platelet].
"calcium"

#' Nonparametric Rank Number of Reference Interval
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This data shows the rank number for computing the confidence interval of
#' nonparametric reference limit when the samples within 119-1000 values. But the
#' reference interval must be 95% and the confidence interval is 90%.
#'
#' @format A [mcradds::nonparRanks] data set contains 882 observations and 3 variables.
#' \describe{
#'    \item{SampleSize}{sample size}
#'    \item{Lower}{lower rank}
#'    \item{Upper}{upper rank}
#' }
#' @source CLSI-EP28A3 Table 8. is cited in this data set.
#'
#' @references EP28-A3c: Defining, Establishing, and Verifying Reference Intervals
#' in the Clinical Laboratory; Approved Guideline—Third Edition
"nonparRanks"
