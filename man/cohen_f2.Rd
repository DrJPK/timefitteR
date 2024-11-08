% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cohen_f2.R
\name{cohen_f2}
\alias{cohen_f2}
\title{Compute Cohen's f-squared for One or Two Linear Models}
\usage{
cohen_f2(baseModel, comparisonModel)
}
\arguments{
\item{baseModel}{a model generated by a call to lm()}

\item{comparisonModel}{a more complex model generated by a call to lm()}
}
\value{
The value of f^2
}
\description{
This takes one or two linear models and returns the value of f^2 as outlined in Cohen (1988).
}
\details{
f^2 is a measure of the \emph{local} effect size of a model. When using one model with the function e.g., \code{cohen_f2(model0)}, the researcher is testing to see if the difference between the values predicted by using the model are different that the actual values.

With 1 model, the aim is to reduce f^2 as close to zero as possible or to at least below the moderate threshold. That is the effect size of using the model rather than the actual data is minimal.

When comparing two models using \code{cohen_f2(model0,model1)} the researcher can determine if the \emph{effect} of including the additional predictor variables in model1 is significant enough to justify the increased complexity of the model compared to model0. In this case, the hope is to maximise the value of f^2 and usually if f^2 is less than Moderate, it is wise \strong{not} to accept the more complex model.

Cohen suggested the following cut off values for f^2

f^2 >= 0.02 : Small
f^2 >= 0.15 : Moderate
f^2 >= 0.35 : Large
}
\examples{

library(tibble)

## Generate some test data where y = x^2 + noise
df <- tibble::tibble(x = seq(1, 10, length.out = 100), y = x^2 + rnorm(length(x)))

## Create two simple linear models
model0 <- lm(y ~ x, data = df)
model1 <- lm(y ~ x + I(x^2), data = df)

## Check the fit of each model to the data
cohen_f2(model0)
cohen_f2(model1)

## Check if model1 is a better fit for the data than model0
cohen_f2(model0, model1)

}
