% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timefitteR_Params_List.R
\name{is.timefitteR_Param_List}
\alias{is.timefitteR_Param_List}
\title{TimefitteR_Param_List}
\usage{
is.timefitteR_Param_List(x)
}
\arguments{
\item{x}{a suspected timefitteR_Param_List object}
}
\value{
LOGICAL
}
\description{
The is.timefitteR_Param_List is used to check if the supplied argument is a correctly formed and likely valid parameter list for use with timefitteR
}
\examples{
x <- timefitteR_Param_List()
is.timefitteR_Param_List(x)
# TRUE
x$test <- "something"
is.timefitteR_Param_List(x)
# FALSE

}
