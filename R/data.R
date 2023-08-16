#' Quantitative Measurement Data
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


#' Simulated Qualitative Data
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

#' Reference Interval Data
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
#' in the Clinical Laboratory.
"nonparRanks"

#' Inermediate Precision Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This data set consists of the Glucose intermediate precision data in the CLSI
#' EP05-A3 guideline.
#'
#' @format A [mcradds::glucose] data set contains 80 observations and 3 variables.
#' \describe{
#'    \item{day}{day number}
#'    \item{run}{run number}
#'    \item{value}{measurement value}
#' }
#' @source CLSI-EP05A3 Table A1. Glucose Precision Evaluation Measurements (mg/dL)
#' is cited in this data set.
#'
#' @references EP05A3: Evaluation of Precision of Quantitative Measurement Procedures.
"glucose"

#' Two-sampled Paired Test Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This data set consists the measurements of low-density lipoprotein (LDL),
#' oxidized low-density lipoprotein (OxLDL) and the corresponding diagnosis.
#' OxLDL is thought to be the active molecule in the process of atherosclerosis,
#' so its proponents believe that its serum concentration should provide more
#' accurate risk stratification than the traditional LDL assay.
#'
#' @format A [mcradds::ldlroc] data set contains 50 observations and 3 variables.
#' \describe{
#'    \item{Diagnosis}{the diagnosis, 1 represents a subject has the disease or
#'    condition of interest is present, 0 is absent}
#'    \item{OxLDL}{oxidized low-density lipoprotein(OxLDL) measurement value}
#'    \item{LDL}{low-density lipoprotein(LDL) measurement value}
#' }
#' @source CLSI-EP24A2 Table D1. OxLDL and LDL Assay Values (in U/L) for 50 Subjects.
#'
#' @references EP24A2 Assessment of the Diagnostic Accuracy of Laboratory Tests
#' Using Receiver Operating Characteristic Curves.
"ldlroc"

#' PD-L1 Reader Precision Data
#'
#' @description `r lifecycle::badge("experimental")`
#'
#' This dummy data set is from a PD-L1 HE stained study to estimate the reproducibility
#' of one assay in determining the PD-L1 status of NSCLC tissue specimens. It
#' contains three sub-data to compute the reproducibility between read (one pathologists,
#' also called reader here, scores one specimen three times), between reader (three
#' readers scores the same specimen) and between site (one reader in three sites
#' scores the same specimens). These data sets don't have the reference for the each
#' score so it can be only used in the pairwise comparison to calculate the `APA`,
#' `ANA` and `OPA` which don't reply on the reference.
#'
#' @format A [mcradds::PDL1RP] data set contains 3 sub set, each sub set includes
#' 150 specimens, 450 observations and 4 variables.
#' \describe{
#'    \item{Sample}{Sample id}
#'    \item{Site}{Site id}
#'    \item{Order}{Order of reader scoring}
#'    \item{Reader}{Reader id, the first character represents the site id, and the
#'    second character is the reader number}
#'    \item{Value}{Result of scoring, `Positive` or `Negative`}
#' }
#'
"PDL1RP"
