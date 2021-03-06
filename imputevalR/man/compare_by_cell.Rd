% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.R
\name{compare_by_cell}
\alias{compare_by_cell}
\title{Squared error between means for all cells in a dataset}
\usage{
compare_by_cell(df1, df2)
}
\arguments{
\item{df1}{The first data.frame for which comparisons should be made}

\item{df2}{The second data.frame for which comparisons should be made}
}
\value{
\code{summed_diff}, a named vector of length equal to the
number of variables, where each value is the summed squared difference between
the imputed and true value of a cell, for each variable
}
\description{
Squared error between means for all cells in a dataset
}
