% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/outlier.R
\name{tukey_outlier}
\alias{tukey_outlier}
\title{Detect Tukey Outlier}
\usage{
tukey_outlier(x)
}
\arguments{
\item{x}{(\code{numeric})\cr numeric input}
}
\value{
A list contains outliers and vector without outliers.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Help function detects the potential outlier with Tukey method where the number
is below \code{Q1-1.5*IQR} and above \code{Q3+1.5*IQR}.
}
\examples{
x <- c(13.6, 44.4, 45.9, 14.9, 41.9, 53.3, 44.7, 95.2, 44.1, 50.7, 45.2, 60.1, 89.1)
tukey_outlier(x)
}
