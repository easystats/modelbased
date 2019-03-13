#' Estimate the slopes of a numeric predictor (over different factor levels)
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_slopes.stanreg]{Bayesian models (stanreg and brms)}}
#'  }
#'
#' @param model Object.
#' @param ... Arguments passed to or from other methods.
#'
#' @export
estimate_slopes <- function(model, ...) {
  UseMethod("estimate_slopes")
}














#' Estimate the slopes of a numeric predictor (over different factor levels)
#'
#'
#' @inheritParams estimate_contrasts.stanreg
#' @param trend A character indicating the name of the numeric variable for which to compute the slopes.
#' @param levels A character vectors indicating the variables over which the slope will be computed. If NULL (default), it will select all the remaining predictors.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length * Petal.Width, data = iris)
#' estimate_slopes(model)
#' }
#' @import dplyr
#' @import emmeans
#' @importFrom graphics pairs
#' @importFrom stats mad median sd setNames
#' @export
estimate_slopes.stanreg <- function(model, trend = NULL, levels = NULL, transform = "response", ci = .90, estimate = "median", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, ...) {
  predictors <- insight::find_predictors(model)$conditional
  data <- insight::get_data(model)

  if (is.null(trend)) {
    trend <- predictors[sapply(data[predictors], is.numeric)][1]
    message("No numeric variable was selected for slope estimation. Selecting ", trend, ".")
  }
  if (length(trend) > 1) {
    message("More than one numeric variable was selected for slope estimation. Keeping only ", trend[1], ".")
    trend <- trend[1]
  }

  if (is.null(levels)) {
    levels <- predictors[!predictors %in% trend]
  }

  if (length(levels) == 0) {
    stop("No suitable factor levels detected over which to estimate slopes.")
  }


  # Basis
  trends <- model %>%
    emmeans::emtrends(levels, var = trend, transform = transform, ...)

  params <- as.data.frame(trends)
  rownames(params) <- NULL

  # Remove the posterior summary
  params <- params[names(params) %in% names(data)]
  # params <- params[, 1:(ncol(params)-3)]


  # Posteriors
  posteriors <- trends %>%
    emmeans::as.mcmc.emmGrid() %>%
    as.matrix() %>%
    as.data.frame()

  # Summary
  slopes <- parameters::summarise_posteriors(posteriors, ci = ci, estimate = estimate, test = test, rope_range = rope_range, rope_full = rope_full)

  slopes$Parameter <- NULL
  slopes <- cbind(params, slopes)

  # Restore factor levels
  slopes <- .restore_factor_levels(slopes, insight::get_data(model))

  return(slopes)
}
