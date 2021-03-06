% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/core.R
\name{prop2str}
\alias{prop2str}
\title{Wrapper for \code{num2str(..., isProportion = T)}}
\usage{
prop2str(x, precision = 3, minPrefix = "< ", ...)
}
\arguments{
\item{x}{number to stringify}

\item{precision}{number of decimal places to preserve}

\item{minPrefix}{prefix for numbers that would render as .0}

\item{...}{
  Arguments passed on to \code{\link[=num2str]{num2str}}
  \describe{
    \item{\code{num}}{number to convert to string}
    \item{\code{isProportion}}{whether to strip leading 0 for 0.x values}
    \item{\code{truncateZeros}}{whether to strip trailing 0s}
  }}
}
\description{
Wrapper for \code{num2str(..., isProportion = T)}
}
\examples{
data.frame(input = c(seq(-10,10),100),
  num = num2str(exp(c(seq(-10,10),100)), 4),
  prop = prop2str(exp(c(seq(-10,10),100)), 4)
)

}
