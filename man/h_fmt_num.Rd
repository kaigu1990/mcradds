% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{h_fmt_num}
\alias{h_fmt_num}
\title{Format Numeric Data}
\usage{
h_fmt_num(x, digits, width = digits + 4)
}
\arguments{
\item{x}{(\code{numeric})\cr numeric input.}

\item{digits}{(\code{integer})\cr the desired number of digits after the
decimal point (format = "f").}

\item{width}{(\code{integer})\cr the total field width.}
}
\value{
A character object with specific digits and width.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}

Help function to format numeric data with \code{formatC} function.
}
\examples{
h_fmt_num(pi * 10^(-2:2), digits = 2, width = 6)
}
\seealso{
\code{\link[=formatC]{formatC()}}
}
