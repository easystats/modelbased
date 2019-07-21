#' Estimate the slopes of a numeric predictor (over different factor levels)
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_slopes.stanreg]{Bayesian models (stanreg and brms)}}
#'  }
#'
#' @inheritParams estimate_contrasts
#' @param trend A character indicating the name of the numeric variable for which to compute the slopes.
#' @param levels A character vectors indicating the variables over which the slope will be computed. If NULL (default), it will select all the remaining predictors.
#'
#' @export
estimate_slopes <- function(model, trend = NULL, levels = NULL, transform = "response", standardize = TRUE, standardize_robust = FALSE, ...) {
  UseMethod("estimate_slopes")
}














#' Estimate the slopes of a numeric predictor (over different factor levels)
#'
#' @inheritParams estimate_slopes
#' @inheritParams estimate_contrasts.stanreg
#'
#' @examples
#' library(estimate)
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' estimate_slopes(model)
#' }
#'
#' @import emmeans
#' @importFrom graphics pairs
#' @importFrom stats mad median sd setNames
#' @export
estimate_slopes.stanreg <- function(model, trend = NULL, levels = NULL, transform = "response", standardize = TRUE, standardize_robust = FALSE, centrality = "median", ci = 0.89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1, ...) {
  predictors <- insight::find_predictors(model)$conditional
  data <- insight::get_data(model)

  if (is.null(trend)) {
    trend <- predictors[sapply(data[predictors], is.numeric)][1]
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


  # Basis
  trends <- emmeans::emtrends(model, levels, var = trend, transform = transform, ...)

  params <- as.data.frame(trends)
  rownames(params) <- NULL

  # Remove the posterior summary
  params <- params[names(params) %in% names(data)]
  # params <- params[, 1:(ncol(params)-3)]

  # Posteriors
  posteriors <- emmeans::as.mcmc.emmGrid(trends)
  posteriors <- as.data.frame(as.matrix(posteriors))

  # Summary
  slopes <- bayestestR::describe_posterior(posteriors, ci = ci, ci_method = ci_method, centrality = centrality, test = test, rope_range = rope_range, rope_ci = rope_ci)
  if ("CI" %in% names(slopes) & length(unique(slopes$CI)) == 1) slopes$CI <- NULL
  if ("ROPE_CI" %in% names(slopes) & length(unique(slopes$ROPE_CI)) == 1) slopes$ROPE_CI <- NULL
  slopes$ROPE_low <- slopes$ROPE_high <- NULL

  slopes$Parameter <- NULL
  slopes <- cbind(params, slopes)

  # Standardized slopes
  if (standardize) {
    slopes <- cbind(slopes, .standardize_slopes(slopes, model, trend, robust = standardize_robust))
  }

  # Restore factor levels
  slopes <- .restore_factor_levels(slopes, insight::get_data(model))

  attributes(slopes) <- c(
    attributes(slopes),
    list(
      levels = levels,
      trend = trend,
      transform = transform,
      ci = ci,
      ci_method = ci_method,
      rope_range = rope_range,
      rope_ci = rope_ci
    )
  )

  class(slopes) <- c("estimate_slopes", class(slopes))

  slopes
}

#' @export
print.estimate_slopes <- .print_estimate



#' @keywords internal
.standardize_slopes <- function(slopes, model, trend, robust = FALSE) {
  vars <- names(slopes)[names(slopes) %in% c("Median", "Mean", "MAP", "Coefficient")]
  x <- insight::get_predictors(model)[[trend]]
  if (insight::model_info(model)$is_linear) {
    response <- insight::get_response(model)
    if (robust) {
      std <- slopes[vars] * mad(x, na.rm = TRUE) / mad(response, na.rm = TRUE)
    } else {
      std <- slopes[vars] * sd(x, na.rm = TRUE) / sd(response, na.rm = TRUE)
    }
  } else {
    if (robust) {
      std <- slopes[vars] * mad(x, na.rm = TRUE)
    } else {
      std <- slopes[vars] * sd(x, na.rm = TRUE)
    }
  }
  names(std) <- paste0("Std_", names(std))
  as.data.frame(std)
}
