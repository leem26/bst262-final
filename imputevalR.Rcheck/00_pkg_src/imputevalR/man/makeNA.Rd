% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.R
\name{makeNA}
\alias{makeNA}
\title{Set random values in a data frame to missing}
\usage{
makeNA(df, proportionNA = 0.2)
}
\arguments{
\item{df}{A data.frame for which NA values should be generated}

\item{proportionNA}{The proportion of all cells that should be NA,
across variables and observations}
}
\value{
A data.frame (\code{df}) that is the same dimensions as the input
data frame, but now has values missing (set by \code{proportionNA})
}
\description{
Set random values in a data frame to missing
}
\examples{
data(nhanes, package = "imputevalR")
miss_data <- imputevalR::makeNA(nhanes, proportionNA = 0.2)

}
