#' Generates predictions
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_response.stanreg]{Bayesian models (stanreg and brms)}}
#'  }
#' \code{estimate_link} is a shortcut to \code{estimate_response} with \code{data = "grid"} and, for Bayesian models, \code{predict = "link"} and \code{smooth_strength = 0.2}. \code{estimate_response} would be used in the context of generating actual predictions for the existing or new data, whereas \code{estimate_link} is more relevant in the context of visualisation and plotting.
#'
#'
#' @inheritParams estimate_contrasts
#' @param data A data frame with model's predictors to estimate the response. If NULL, the model's data is used. If "grid", the model matrix is obtained (through \code{\link{data_grid}}).
#' @param random Should it take the random effects into account? Can be \code{TRUE}, \code{FALSE} or a formula indicating which group-level parameters to condition on when making predictions. The data argument may include new levels of the grouping factors that were specified when the model was estimated, in which case the resulting posterior predictions marginalize over the relevant variables (see \code{posterior_predict.stanreg}).
#' @param length Passed to \code{\link{data_grid}} if \code{data = "grid"}.
#' @param preserve_range Passed to \code{\link{data_grid}} if \code{data = "grid"}.
#'
#'
#' @export
estimate_response <- function(model, data = NULL, transform = "response", random = FALSE, length = 25, preserve_range = TRUE, ...) {
  UseMethod("estimate_response")
}



#' Generates predictions for Bayesian models
#'
#' @inheritParams estimate_response
#' @inheritParams estimate_contrasts.stanreg
#'
#' @param predict Can be "response" (default) or "link". The former predicts the the outcome per se, while the latter predicts the link function (i.e., the regression "line"), equivalent to estimating the \code{fit}. In other words, \code{estimate_response(model, predict="link")} is equivalent to \code{estimate_link(model)}.
#' @param smooth_method As Bayesian predictions might create jittery-looking lines, smoothing can be an option when predictions are used for visualisation purposes. See arguments for \code{\link{smoothing}}. \code{smooth_method = NULL} or \code{smooth_strength = 0} corresponds to no smoothing.
#' @param smooth_strength This argument only applies to \code{smooth_method = "loess"}. See above the description for \code{smooth_method} and see arguments for \code{\link{smoothing}}.
#' @param keep_draws If FALSE, will summarise the posterior the obtained distributions. If TRUE, will keep all prediction iterations (draws).
#' @param draws An integer indicating the number of draws to return. The default and maximum number of draws is the size of the posterior sample contained in the model.
#' @param seed An optional seed to use.
#'
#' @examples
#' library(estimate)
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' estimate_response(model)
#' estimate_link(model)
#'
#' model <- stan_glmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
#' estimate_response(model)
#' estimate_link(model)
#' }
#'
#' @export
estimate_response.stanreg <- function(model, data = NULL, transform = "response", random = FALSE, length = 25, preserve_range = TRUE, predict = "response", smooth_method = "smooth", smooth_strength = 0, keep_draws = FALSE, draws = NULL, seed = NULL, centrality = "median", ci = 0.89, ci_method = "hdi", ...) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("This function needs `rstanarm` to be installed.")
  }


  # Data
  if (is.null(data)) {
    data <- insight::get_data(model)
  } else if (!is.data.frame(data)) {
    if (data == "grid") {
      data <- data_grid(model, random = random, length = length, preserve_range = preserve_range, reference = insight::get_data(model), ...)
    } else {
      stop('The `data` argument must either NULL, "grid" or another data.frame.')
    }
  }

  data <- data[names(data) %in% insight::find_predictors(model, effects = "all", flatten = TRUE)]

  # Deal with random
  if (insight::model_info(model)$is_mixed & random) {
    if (!insight::find_random(model, flatten = TRUE) %in% names(data)) {
      warning("Could not find random effects in data. Will turn `random` to FALSE.")
      random <- FALSE
    }
  }
  if (random == TRUE) {
    re.form <- NULL
  } else if (random == FALSE) {
    re.form <- NA
  }

  # Generate draws
  if (predict == "link") {
    if (transform == "response") {
      transform <- TRUE
    } else {
      transform <- FALSE
    }
    posteriors <- rstanarm::posterior_linpred(model, newdata = data, re.form = re.form, seed = seed, draws = draws, transform = transform)
  } else {
    posteriors <- rstanarm::posterior_predict(model, newdata = data, re.form = re.form, seed = seed, draws = draws, transform = "response")
  }

  # Summary
  prediction <- .summarize_posteriors(as.data.frame(posteriors), ci = ci, centrality = centrality, ci_method = ci_method, test = NULL, rope_range = NULL)
  prediction$Parameter <- NULL

  # Draws
  if (keep_draws == TRUE) {
    posteriors <- as.data.frame(t(posteriors))
    names(posteriors) <- paste0("Draw_", seq_len(length(names(posteriors))))
    prediction <- cbind(prediction, posteriors)
  }

  # Add predictors
  out <- cbind(data, prediction)

  # Restore factor levels
  out <- .restore_factor_levels(out, insight::get_data(model))

  # Smoothing
  out[c(names(prediction), names(data[sapply(data, is.factor)]))] <- smoothing(out[c(names(prediction), names(data[sapply(data, is.factor)]))], method = smooth_method, strength = smooth_strength)


  attributes(out) <- c(
    attributes(out),
    list(
      predict = predict,
      ci = ci,
      ci_method = ci_method,
      transform = transform,
      keep_draws = keep_draws,
      draws = draws,
      seed = seed,
      random = random,
      response = insight::find_response(model)
    )
  )

  class(out) <- c("estimate_response", "see_estimate_response", class(out))
  out
}






#' @rdname estimate_response
#' @export
#' @export
estimate_link <- function(model, data = "grid", transform = "response", random = FALSE, length = 25, preserve_range = TRUE, ...) {
  UseMethod("estimate_link")
}




#' @rdname estimate_response.stanreg
#' @export
estimate_link.stanreg <- function(model, data = "grid", transform = "response", random = FALSE, length = 25, preserve_range = TRUE, predict = "link", smooth_method = "loess", smooth_strength = 0.25, keep_draws = FALSE, draws = NULL, seed = NULL, centrality = "median", ci = 0.89, ci_method = "hdi", ...) {
  estimate_response(model, data = data, transform = transform, random = random, length = length, preserve_range = preserve_range, predict = predict, smooth_method = smooth_method, smooth_strength = smooth_strength, keep_draws = keep_draws, draws = draws, seed = seed, centrality = centrality, ci = ci, ci_method = ci_method, ...)
}


#' @rdname estimate_response.stanreg
#' @export
estimate_response.data.frame <- function(model, data = NULL, transform = "response", random = FALSE, length = 25, preserve_range = TRUE, predict = "link", smooth_method = "loess", smooth_strength = 0, keep_draws = FALSE, draws = NULL, seed = NULL, centrality = "median", ci = 0.89, ci_method = "hdi", ...) {
  estimate_response(data, data = model, transform = transform, random = random, length = length, preserve_range = preserve_range, predict = predict, smooth_method = smooth_method, smooth_strength = smooth_strength, keep_draws = keep_draws, draws = draws, seed = seed, centrality = centrality, ci = ci, ci_method = ci_method, ...)
}


#' @rdname estimate_response.stanreg
#' @export
estimate_link.data.frame <- function(model, data = "grid", transform = "response", random = FALSE, length = 25, preserve_range = TRUE, predict = "link", smooth_method = "loess", smooth_strength = 0.25, keep_draws = FALSE, draws = NULL, seed = NULL, centrality = "median", ci = 0.89, ci_method = "hdi", ...) {
  estimate_response(data, data = model, transform = transform, random = random, length = length, preserve_range = preserve_range, predict = predict, smooth_method = smooth_method, smooth_strength = smooth_strength, keep_draws = keep_draws, draws = draws, seed = seed, centrality = centrality, ci = ci, ci_method = ci_method, ...)
}
