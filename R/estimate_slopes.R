#' Estimate the slopes of a numeric predictor (over different factor levels)
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_slopes.lm]{Frequentist models}}
#'  \item{\link[=estimate_slopes.stanreg]{Bayesian models (stanreg and brms)}}
#'  }
#'
#' @inheritParams estimate_contrasts.lm
#' @param trend A character vector indicating the name of the numeric variable for which to compute the slopes.
#' @param levels A character vector indicating the variables over which the slope will be computed. If NULL (default), it will select all the remaining predictors.
#' @param component A character vector indicating the model component for which estimation is requested. Only applies to models from \pkg{glmmTMB}. Use \code{"conditional"} for the count-model or \code{"zero_inflate"} or \code{"zi"} for the zero-inflation model.
#'
#' @return A data frame of slopes.
#' @export
estimate_slopes <- function(model, trend = NULL, levels = NULL, transform = "response", standardize = TRUE, standardize_robust = FALSE, ci = 0.95, ...) {
  UseMethod("estimate_slopes")
}














#' Estimate the slopes of a numeric predictor (over different factor levels)
#'
#' @inheritParams estimate_slopes
#' @inheritParams estimate_contrasts.stanreg
#'
#' @examples
#' library(modelbased)
#' \donttest{
#' if (require("rstanarm")) {
#'   model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'   estimate_slopes(model)
#' }
#' }
#' @importFrom stats mad median sd setNames
#' @export
estimate_slopes.stanreg <- function(model, trend = NULL, levels = NULL, transform = "response", standardize = TRUE, standardize_robust = FALSE, ci = 0.95, centrality = "median", ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1, ...) {
  .estimate_slopes(model, trend = trend, levels = levels, transform = transform, standardize = standardize, standardize_robust = standardize_robust, centrality = centrality, ci = ci, ci_method = ci_method, test = test, rope_range = rope_range, rope_ci = rope_ci)
}

#' @export
estimate_slopes.brmsfit <- estimate_slopes.stanreg



#' Estimate the slopes of a numeric predictor (over different factor levels)
#'
#' @inheritParams estimate_slopes
#' @inheritParams estimate_contrasts.lm
#'
#' @examples
#' library(modelbased)
#'
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' estimate_slopes(model)
#' @export
estimate_slopes.lm <- function(model, trend = NULL, levels = NULL, transform = "response", standardize = TRUE, standardize_robust = FALSE, ci = 0.95, ...) {
  .estimate_slopes(model, trend = trend, levels = levels, transform = transform, standardize = standardize, standardize_robust = standardize_robust, ci=ci, ...)
}


#' @export
estimate_slopes.merMod <- estimate_slopes.lm


#' @rdname estimate_slopes
#' @export
estimate_slopes.glmmTMB <- function(model, trend = NULL, levels = NULL, transform = "response", standardize = TRUE, standardize_robust = FALSE, ci = 0.95, component = c("conditional", "zero_inflated", "zi"), ...) {
  component <- match.arg(component)
  if (component == "zi") component <- "zero_inflated"
  .estimate_slopes(model, trend = trend, levels = levels, transform = transform, standardize = standardize, standardize_robust = standardize_robust, ci = ci, component = component, ...)
}








#' @importFrom stats confint
#' @importFrom emmeans emtrends
#' @keywords internal
.estimate_slopes <- function(model, trend = NULL, levels = NULL, transform = "response", standardize = TRUE, standardize_robust = FALSE, ci = 0.95, centrality = "median", ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1, component = "conditional", ...) {
  predictors <- insight::find_predictors(model)[[component]]
  data <- insight::get_data(model)

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


  # Basis
  trends <- .emtrends_helper(model, levels, trend, transform, component, ...)


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
      rope_ci = rope_ci,
      response = insight::find_response(model)
    )
  )

  class(slopes) <- c("estimate_slopes", class(slopes))

  slopes
}









#' @importFrom insight get_response model_info get_predictors
#' @importFrom stats sd mad
#' @keywords internal
.standardize_slopes <- function(slopes, model, trend, robust = FALSE) {
  vars <- names(slopes)[names(slopes) %in% c("Median", "Mean", "MAP", "Coefficient")]
  x <- insight::get_predictors(model)[[trend]]
  if (insight::model_info(model)$is_linear) {
    response <- insight::get_response(model)
    if (robust) {
      std <- slopes[vars] * stats::mad(x, na.rm = TRUE) / stats::mad(response, na.rm = TRUE)
    } else {
      std <- slopes[vars] * stats::sd(x, na.rm = TRUE) / stats::sd(response, na.rm = TRUE)
    }
  } else {
    if (robust) {
      std <- slopes[vars] * stats::mad(x, na.rm = TRUE)
    } else {
      std <- slopes[vars] * stats::sd(x, na.rm = TRUE)
    }
  }
  names(std) <- paste0("Std_", names(std))
  as.data.frame(std)
}




.emtrends_helper <- function(model, levels, trend, transform, component, ...) {
  if (component != "conditional") {
    if (transform == "response") {
      emmeans::emtrends(model, levels, var = trend, transform = "response", component = "zi", ...)
    } else if (transform == "mu") {
      emmeans::emtrends(model, levels, var = trend, transform = "mu", component = "zi", ...)
    } else if (transform == "unlink") {
      emmeans::emtrends(model, levels, var = trend, transform = "unlink", component = "zi", ...)
    } else if (transform == "log") {
      emmeans::emtrends(model, levels, var = trend, transform = "log", component = "zi", ...)
    } else if (transform == "none") {
      emmeans::emtrends(model, levels, var = trend, transform = "none", component = "zi", ...)
    }
  } else {
    if (transform == "response") {
      emmeans::emtrends(model, levels, var = trend, transform = "response", ...)
    } else if (transform == "mu") {
      emmeans::emtrends(model, levels, var = trend, transform = "mu", ...)
    } else if (transform == "unlink") {
      emmeans::emtrends(model, levels, var = trend, transform = "unlink", ...)
    } else if (transform == "log") {
      emmeans::emtrends(model, levels, var = trend, transform = "log", ...)
    } else if (transform == "none") {
      emmeans::emtrends(model, levels, var = trend, transform = "none", ...)
    }
  }
}