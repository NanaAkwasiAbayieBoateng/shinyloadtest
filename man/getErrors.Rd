% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze-log.R
\name{getErrors}
\alias{getErrors}
\title{Get errors from event log}
\usage{
getErrors(eventLog)
}
\arguments{
\item{eventLog}{Returned from \code{\link{loadTest}}. The \code{eventLog} is
a list composed of data frames (from successful tests) and potnetially errors.}
}
\description{
Get errors from event log
}
\details{
Given an \code{eventLog} containing successesful test results (data
  frames) and error messages, this function returns the error messages in a
  list. See \code{\link{getSuccesses}}
}
