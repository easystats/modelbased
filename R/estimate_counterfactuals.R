#' Estimate Average Counterfactual Predictions
#'
#' non-focal predictors are marginalized over the observations in your sample,
#' for counterfactual predictions. Technically, predicted values for each
#' observation in the data are calculated multiple times (the data is duplicated
#' once for all unique values of the focal terms) "empirical"is probably the
#' most “realistic” approach, insofar as the results can also be transferred to
#' other contexts. It answers the question, “What is the predicted (or:
#' expected) value of the response at meaningful values or levels of my focal
#' terms for the ‘average’ observation in the population?”. It does not only
#' refer to the actual data in your sample, but also “what would be if” we had
#' more data, or if we had data from a different population. This is where
#' “counterfactual” refers to.
#'
#'
#' @inheritParams estimate_means
#'
#' @return A data frame of estimated counterfactual predictions.
#'
#' @references
#' Dickerman BA, Hernan, MA. Counterfactual prediction is not only for causal
#' inference. Eur J Epidemiol 35, 615–617 (2020).
#'
#' Heiss, A. (2022). Marginal and conditional effects for GLMMs with
#' {marginaleffects}. Andrew Heiss. \doi{10.59350/xwnfm-x1827}
#'
#' @examplesIf all(insight::check_if_installed(c("marginaleffects", "glmmTMB"), quietly = TRUE))
#' library(modelbased)
#' data(Salamanders, package = "glmmTMB")
#' m <- glmmTMB::glmmTMB(
#'   count ~ spp + mined + (1 | site),
#'   ziformula = ~ spp + mined,
#'   family = poisson(),
#'   data = Salamanders
#' )
#'
#' # count component
#' estimate_means(m, "spp", predict = "conditional")
#'
#' # response scale ("full" model)
#' estimate_means(m, "spp")
#'
#' # counterfactual
#' estimate_counterfactuals(m, "spp")
#' @export
estimate_counterfactuals <- function(model,
                                     by = "auto",
                                     predict = NULL,
                                     ci = 0.95,
                                     verbose = TRUE,
                                     ...) {
  out <- estimate_means(
    model,
    by = by,
    predict = predict,
    ci = ci,
    backend = "marginaleffects",
    verbose = verbose,
    counterfactual = TRUE,
    ...
  )

  attr(out, "table_title") <- c("Estimated Average Effects", "blue")
  out
}
