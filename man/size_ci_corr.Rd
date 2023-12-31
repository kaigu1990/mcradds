% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/samplesize.R
\name{size_ci_corr}
\alias{size_ci_corr}
\title{Sample Size for Testing Confidence Interval of Pearson's correlation}
\usage{
size_ci_corr(
  r,
  lr,
  alpha = 0.05,
  interval = c(10, 1e+05),
  tol = 1e-05,
  alternative = c("two.sided", "less", "greater")
)
}
\arguments{
\item{r}{(\code{numeric})\cr expected correlation coefficient of the evaluated assay.}

\item{lr}{(\code{numeric})\cr acceptable correlation coefficient of the evaluated assay.}

\item{alpha}{(\code{numeric})\cr type-I-risk, \eqn{\alpha}.}

\item{interval}{(\code{numeric})\cr a numeric vector containing the end-points of the interval
to be searched for the root(sample size). The defaults are set to c(1, 100000).}

\item{tol}{(\code{numeric})\cr tolerance for searching the root(sample size).}

\item{alternative}{(\code{string})\cr string specifying the alternative hypothesis,
must be one of "two.sided" (default), "greater" or "less".}
}
\value{
an object of \code{size} class that contains the sample size and relevant parameters.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

This function performs sample size computation for testing Pearson's
correlation when a lower confidence interval is provided.
}
\examples{
size_ci_corr(r = 0.9, lr = 0.85, alpha = 0.025, alternative = "greater")
}
\references{
Fisher (1973, p. 199).
}
\seealso{
\code{\link[=size_one_prop]{size_one_prop()}} \code{\link[=size_ci_one_prop]{size_ci_one_prop()}} \code{\link[=size_corr]{size_corr()}}
}
