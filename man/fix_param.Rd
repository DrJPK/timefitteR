% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix_param.R
\name{fix_param}
\alias{fix_param}
\title{Fix Parameters Helper}
\usage{
fix_param(param, n)
}
\arguments{
\item{param}{a vector of parameter values or an object that can be coerced to an atomic vector}

\item{n}{the length of the vector to be returned}
}
\value{
an atomic vector of n-length
}
\description{
This helper function is used by \link{generate_data_set} to ensure that the length of the parameter vectors passed to the model are of n-length. Parameter vectors of the wrong length are trimmed or padded with the last value in order to work.
}
\examples{
fix_param(param=c(1,2,3,4),n=2)

}
