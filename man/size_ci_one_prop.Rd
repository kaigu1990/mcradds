% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samplesize.R
\name{size_ci_one_prop}
\alias{size_ci_one_prop}
\title{Sample Size for Testing Confidence Interval of One Proportion}
\usage{
size_ci_one_prop(
  p,
  lr,
  alpha = 0.05,
  interval = c(1, 1e+05),
  tol = 1e-05,
  alternative = c("two.sided", "less", "greater"),
  method = c("simple-asymptotic", "wilson", "wald", "clopper-pearson")
)
}
\arguments{
\item{p}{(\code{numeric})\cr expected criteria of the evaluated assay.}

\item{lr}{(\code{numeric})\cr acceptable criteria of the evaluated assay.}

\item{alpha}{(\code{numeric})\cr type-I-risk, \eqn{\alpha}.}

\item{interval}{(\code{numeric})\cr a numeric vector containing the end-points of the interval
to be searched for the root(sample size). The defaults are set to c(1, 100000).}

\item{tol}{(\code{numeric})\cr tolerance for searching the root(sample size).}

\item{alternative}{(\code{string})\cr string specifying the alternative hypothesis,
must be one of "two.sided" (default), "greater" or "less".}

\item{method}{(\code{string})\cr string specifying the which method to use.
Simple Asymptotic is default, equal to Wald. Options can be "wilson",
"clopper-pearson" and other method, see \link[DescTools:BinomCIn]{DescTools::BinomCIn}}
}
\value{
an object of \code{size} class that contains the sample size and relevant parameters.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function performs sample size computation for testing a given lower
confidence interval of one proportion with the using of the
Simple Asymptotic(Wald), Wilson score, clopper-pearson and other methods.
}
\examples{
size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "wilson")
size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "simple-asymptotic")
size_ci_one_prop(p = 0.85, lr = 0.8, alpha = 0.05, method = "wald")
}
\references{
Newcombe, R. G. 1998. 'Two-Sided Confidence Intervals for the
Single Proportion: Comparison of Seven Methods.' Statistics in Medicine, 17, pp. 857-872.
}
\seealso{
\code{\link[=size_one_prop]{size_one_prop()}} \code{\link[=size_corr]{size_corr()}} \code{\link[=size_ci_corr]{size_ci_corr()}}
}
