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
#'
#' @examples
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' slopes <- estimate_slopes(model, trend = "Petal.Length")
#' slopes
#' effectsize::standardize(slopes)
#'
#' \dontrun{
#' if (require("rstanarm")) {
#'   model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris, refresh = 0)
#'   estimate_slopes(model)
#' }}
#' @return A data.frame.
#' @export
estimate_slopes <- function(model,
                            trend = NULL,
                            levels = NULL,
                            ci = 0.95,
                            ...) {
  # check if available
  insight::check_if_installed("emmeans")

  # Guess specs arguments
  args <- .estimate_slopes_guess_args(model, trend, levels, ...)

  # Run emtrends
  estimated <- emmeans::emtrends(model, args$levels, var = args$trend, ...)

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    trends <- bayestestR::describe_posterior(estimated, ci = ci, ...)
    trends <- cbind(estimated@grid, trends)
    trends$`.wgt.` <- NULL  # Drop the weight column
    trends <- .clean_names_bayesian(trends, model, transform = "none", type = "trend")
    trends <- insight::data_relocate(trends, c("CI_low", "CI_high"), after = "Coefficient")
  } else {
    trends <- parameters::parameters(estimated, ci = ci, ...)
  }

  # Restore factor levels
  trends <- insight::data_restoretype(trends, insight::get_data(model))

  # Table formatting
  attr(trends, "table_title") <- c("Estimated Marginal Effects", "blue")
  attr(trends, "table_footer") <- c(paste("Marginal effects estimated for", args$trend), "blue")

  # Add attributes
  attr(trends, "model") <- model
  attr(trends, "response") <- insight::find_response(model)
  attr(trends, "ci") <- ci
  attr(trends, "levels") <- args$levels
  attr(trends, "trend") <- args$trend


  # Output
  class(trends) <- c("estimate_slopes", class(trends))
  trends
}






# Utilities ---------------------------------------------------------------


#' @keywords internal
.estimate_slopes_guess_args <- function(model, trend, levels, ...){
  # Gather info
  predictors <- insight::find_predictors(model, flatten = TRUE, ...)
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