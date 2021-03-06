% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analyze-log.R
\name{getConnectionsPerR}
\alias{getConnectionsPerR}
\title{Get connections per R process}
\usage{
getConnectionsPerR(eventLog)
}
\arguments{
\item{eventLog}{A data frame with event information as returned from
\code{getSuccesses}}
}
\value{
A data frame with 2 columns: \code{connection} and
  \code{other_connections}, the number of other connections sharing the R
  process along with \code{connection}.
}
\description{
Get connections per R process
}
\details{
This function is useful to help determine if connections per R
  process is contributing to latency which is an indicator the utilization
  scheduler should be updated.
}
