% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/timefitteR_Params_List.R
\name{new_timefitteR_Param_List}
\alias{new_timefitteR_Param_List}
\title{Create a New timefitteR_Param_List Object}
\usage{
new_timefitteR_Param_List(
  intercept_offset = double(),
  intercept_noise = double(),
  slope_offset = double(),
  slope_noise = double(),
  curvature_offset = double(),
  curvature_noise = double()
)
}
\arguments{
\item{intercept_offset}{Numeric to control the intercept of the series for this grouping}

\item{intercept_noise}{numeric multiplier to apply to the standard noise level for the intercept for this grouping.}

\item{slope_offset}{numeric to control the slope of the series for this grouping.}

\item{slope_noise}{numeric multiplier to apply to the standard noise level for the slope for this grouping.}

\item{curvature_offset}{numeric to control the curvature of the series for this grouping.}

\item{curvature_noise}{numeric multiplier to apply to the standard noise level for the curvature for this grouping.}
}
\value{
timefitteR_Param_List
}
\description{
This is used internally to ensure that parameter lists are complete.
}
\examples{
x <- new_timefitteR_Param_List(0,1,0,1,0,1)

}
