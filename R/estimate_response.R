#' Generates predictions
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_response.stanreg]{Bayesian models (stanreg and brms)}}
#'  }
#'
#' @param model Object.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' estimate_response(model)
#'
#' model <- stan_glmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
#' estimate_response(model)
#' }
#'
#' @export
estimate_response <- function(model, ...) {
  UseMethod("estimate_response")
}



#' Generates predictions for Bayesian models
#'
#' @inheritParams estimate_contrasts.stanreg
#' @param data A data frame with model's predictors to estimate the response. If NULL, the model matrix is used. If "grid", the model's data is re-used.
#' @param predict Can be "response" (default) or "link". The former predicts the the outcome per se, while the latter predicts the link function (i.e., the regression "line"), equivalent to estimating the \code{fit}. In other words, \code{estimate_response(model, predict="link")} is equivalent to \code{estimate_fit(model)}.
#' @param keep_draws If FALSE, will summarise the posterior the obtained distributions. If TRUE, will keep all prediction iterations (draws).
#' @param draws An integer indicating the number of draws to return. The default and maximum number of draws is the size of the posterior sample contained in the model.
#' @param seed An optional seed to use.
#' @param random Should it take the random effects into account? Can be \code{TRUE}, \code{FALSE} or a formula indicating which group-level parameters to condition on when making predictions. The data argument may include new levels of the grouping factors that were specified when the model was estimated, in which case the resulting posterior predictions marginalize over the relevant variables (see \code{posterior_predict.stanreg}).
#' @param length Length of numeric target variables that applies to \link{data_grid}.
#' @export
estimate_response.stanreg <- function(model, data = NULL, predict = "response", ci = 0.90, estimate = "median", transform = "response", keep_draws = FALSE, draws = NULL, seed = NULL, random = FALSE, length = 10, ...) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("This function needs `rstanarm` to be installed.")
  }


  # Data
  if (is.null(data)) {
    data <- insight::get_data(model)
  } else if (!is.data.frame(data)) {
    if (data == "grid") {
      data <- data_grid(model, random = random, length = length, ...)
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
  prediction <- as.data.frame(posteriors)
  prediction <- bayestestR::describe_posterior(prediction, ci = ci, estimate = estimate, test = NULL, rope_range = NULL, rope_full = NULL)
  prediction$Parameter <- NULL
  # names(prediction) <- paste0(insight::find_response(model), "_", names(prediction))

  # Draws
  if (keep_draws == TRUE) {
    posteriors <- as.data.frame(t(posteriors))
    names(posteriors) <- paste0("Draw_", seq_len(length(names(posteriors))))
    prediction <- cbind(prediction, posteriors)
  }

  # Add predictors
  prediction <- cbind(data, prediction)


  # Restore factor levels
  prediction <- .restore_factor_levels(prediction, insight::get_data(model))

  attributes(prediction) <- c(attributes(prediction),
                         list(predict = predict,
                              ci = ci,
                              transform = transform,
                              keep_draws = keep_draws,
                              draws = draws,
                              seed = seed,
                              random = random))

  class(prediction) <- c("estimateResponse", class(prediction))
  return(prediction)
}






#' @rdname estimate_response
#' @export
#' @export
estimate_fit <- function(model, ...) {
  UseMethod("estimate_fit")
}




#' @rdname estimate_response.stanreg
#' @export
estimate_fit.stanreg <- function(model, data = "grid", predict = "link", ci = 0.90, estimate = "median", transform = "response", keep_draws = FALSE, draws = NULL, seed = NULL, random = FALSE, ...) {
  estimate_response(model, data = data, predict = predict, ci = ci, estimate = estimate, transform = transform, keep_draws = keep_draws, draws = draws, seed = seed, random = random, ...)
}


#' @rdname estimate_response.stanreg
#' @export
estimate_response.data.frame <- function(model, data = NULL, predict = "response", ci = 0.90, estimate = "median", transform = "response", keep_draws = FALSE, draws = NULL, seed = NULL, random = FALSE, ...) {
  estimate_response(data, data = model, predict = predict, ci = ci, estimate = estimate, transform = transform, keep_draws = keep_draws, draws = draws, seed = seed, random = random, ...)
}


#' @rdname estimate_response.stanreg
#' @export
estimate_fit.data.frame <- function(model, data = "grid", predict = "link", ci = 0.90, estimate = "median", transform = "response", keep_draws = FALSE, draws = NULL, seed = NULL, random = FALSE, ...) {
  estimate_response(data, data = model, predict = predict, ci = ci, estimate = estimate, transform = transform, keep_draws = keep_draws, draws = draws, seed = seed, random = random, ...)
}








#' Reshape Estimations with Draws to long format
#'
#' @param estimates Estimates with posterior draws.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' estimates <- estimate_response(model, keep_draws = TRUE, draws = 200)
#' reshape_draws(estimates)
#' }
#' @importFrom stats reshape
#' @export
reshape_draws <- function(estimates) {
  estimates$Index <- 1:nrow(estimates)
  long <- reshape(estimates,
    varying = names(estimates)[grepl("Draw_", names(estimates))],
    idvar = "Index",
    v.names = "Draw",
    timevar = "Draw_Index",
    direction = "long"
  )
  row.names(long) <- NULL

  return(long)
}
