#' Estimate Marginal Effects
#'
#' Estimate the slopes (i.e., the coefficient) of a predictor over different factor levels. See also other
#' related functions such as \code{\link{estimate_contrasts}} and \code{\link{estimate_means}}.
#'
#'
#' @inheritParams estimate_contrasts
#' @param trend A character vector indicating the name of the numeric variable
#'   for which to compute the slopes.
#' @param levels A character vector indicating the variables over which the
#'   slope will be computed. If NULL (default), it will select all the remaining
#'   predictors.
#' @param component A character vector indicating the model component for which
#'   estimation is requested. Only applies to models from \pkg{glmmTMB}. Use
#'   \code{"conditional"} for the count-model or \code{"zero_inflate"} or
#'   \code{"zi"} for the zero-inflation model.
#'
#'
#' @examples
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' estimate_slopes(model)
#'
#' @return A data frame of slopes.
#' @export
estimate_slopes <- function(model,
                            trend = NULL,
                            levels = NULL,
                            ci = 0.95,
                            ...) {
  # check if available
  insight::check_if_installed("emmeans")

  # Guess specs arguments
  args <- .estimate_slopes_guess_args(model, trend, levels)

  # Run emtrends
  trends <-emmeans::emtrends(model, levels, var = trend, ...)


  if (insight::model_info(model)$is_bayesian) {
    params <- as.data.frame(trends)
    rownames(params) <- NULL

    # Remove the posterior summary
    params <- params[names(params) %in% names(data)]

    # Summary
    slopes <- .summarize_posteriors(trends,
                                    ci = ci, ci_method = ci_method,
                                    centrality = centrality,
                                    test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = model
    )
    slopes$Parameter <- NULL
    slopes <- cbind(params, slopes)
  } else {
    params <- as.data.frame(stats::confint(trends, levels = ci, ...))
    slopes <- .clean_names_frequentist(params)
    names(slopes)[grepl("*.trend", names(slopes))] <- "Coefficient"
  }


  # Standardized slopes
  if (standardize) {
    slopes <- cbind(slopes, .standardize_slopes(slopes, model, trend, robust = standardize_robust))
  }

  # Restore factor levels
  slopes <- insight::data_restoretype(slopes, insight::get_data(model))


  attributes(slopes) <- c(
    attributes(slopes),
    list(
      levels = levels,
      trend = trend,
      transform = transform,
      ci = ci,
      ci_method = ci_method,
      rope_range = rope_range,
      rope_ci = rope_ci,
      response = insight::find_response(model)
    )
  )

  class(slopes) <- c("estimate_slopes", class(slopes))

  slopes

}






# Utilities ---------------------------------------------------------------



.estimate_slopes_guess_args <- function(model, trend, levels){
  # Gather info
  predictors <- insight::find_predictors(model, ...)
  data <- insight::get_data(model)

  # Guess arguments
  if (is.null(trend)) {
    trend <- predictors[sapply(data[predictors], is.numeric)][1]
    if (!length(trend) || is.na(trend)) {
      stop("Model contains no numeric predictor. Cannot estimate trend.")
    }
    message('No numeric variable was specified for slope estimation. Selecting `trend = "', trend, '"`.')
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

  list(trend = trend, levels = levels)
}