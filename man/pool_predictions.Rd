% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pool.R
\name{pool_predictions}
\alias{pool_predictions}
\title{Pool Predictions and Estimated Marginal Means}
\usage{
pool_predictions(x, transform = NULL, ...)
}
\arguments{
\item{x}{A list of \code{estimate_means} objects, as returned by \code{\link[=estimate_means]{estimate_means()}},
or \code{estimate_predicted}, as returned by \code{\link[=estimate_relation]{estimate_relation()}} and related
functions.}

\item{transform}{A function applied to predictions and confidence intervals
to (back-) transform results, which can be useful in case the regression
model has a transformed response variable (e.g., \code{lm(log(y) ~ x)}). For
Bayesian models, this function is applied to individual draws from the
posterior distribution, before computing summaries. Can also be \code{TRUE}, in
which case \code{insight::get_transformation()} is called to determine the
appropriate transformation-function. Note that no standard errors are returned
when transformations are applied.}

\item{...}{Currently not used.}
}
\value{
A data frame with pooled predictions.
}
\description{
This function "pools" (i.e. combines) multiple \code{estimate_means} objects, in
a similar fashion as \code{\link[mice:pool]{mice::pool()}}.
}
\details{
Averaging of parameters follows Rubin's rules (\emph{Rubin, 1987, p. 76}).
Pooling is applied to the predicted values on the scale of the \emph{linear predictor},
not on the response scale, in order to have accurate pooled estimates and
standard errors. The final pooled predicted values are then transformed to
the response scale, using \code{\link[insight:link_inverse]{insight::link_inverse()}}.
}
\examples{
\dontshow{if (require("mice")) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# example for multiple imputed datasets
data("nhanes2", package = "mice")
imp <- mice::mice(nhanes2, printFlag = FALSE)
predictions <- lapply(1:5, function(i) {
  m <- lm(bmi ~ age + hyp + chl, data = mice::complete(imp, action = i))
  estimate_means(m, "age")
})
pool_predictions(predictions)
\dontshow{\}) # examplesIf}
}
\references{
Rubin, D.B. (1987). Multiple Imputation for Nonresponse in Surveys. New York:
John Wiley and Sons.
}
