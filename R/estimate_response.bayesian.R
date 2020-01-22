#' Generates predictions
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_response.stanreg]{Bayesian models (stanreg and brms)}}
#'  \item{\link[=estimate_response.glm]{Frequentist models}}
#'  }
#' \code{estimate_link} is a shortcut to \code{estimate_response} with \code{data = "grid"}. \code{estimate_response} would be used in the context of generating actual predictions for the existing or new data, whereas \code{estimate_link} is more relevant in the context of visualisation and plotting.
#'
#'
#' @inheritParams estimate_contrasts
#' @param data A data frame with model's predictors to estimate the response. If NULL, the model's data is used. If "grid", the model matrix is obtained (through \code{\link{visualisation_matrix}}).
#' @param random Should it take the random effects into account? Can be \code{TRUE}, \code{FALSE} or a formula indicating which group-level parameters to condition on when making predictions. The data argument may include new levels of the grouping factors that were specified when the model was estimated, in which case the resulting posterior predictions marginalize over the relevant variables (see \code{posterior_predict.stanreg}).
#' @param length Passed to \code{\link{visualisation_matrix}} if \code{data = "grid"}.
#' @param preserve_range Passed to \code{\link{visualisation_matrix}} if \code{data = "grid"}.
#'
#' @return A dataframe of predicted values.
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
#' @param keep_draws If FALSE, will summarise the posterior the obtained distributions. If TRUE, will keep all prediction iterations (draws).
#' @param draws An integer indicating the number of draws to return. The default and maximum number of draws is the size of the posterior sample contained in the model.
#' @param seed An optional seed to use.
#'
#' @examples
#' library(modelbased)
#' \donttest{
#' if (require("rstanarm") && require("brms")) {
#'   model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'   estimate_response(model)
#'   estimate_link(model)
#'
#'   model <- stan_glmer(Sepal.Width ~ Petal.Length + (1 | Species), data = iris)
#'   estimate_response(model)
#'   estimate_link(model)
#'
#'   library(brms)
#'   model <- brms::brm(Sepal.Width ~ Petal.Length, data = iris)
#'   estimate_response(model)
#'   estimate_link(model)
#' }
#' }
#' @return A dataframe of predicted values.
#' @export
estimate_response.stanreg <- function(model, data = NULL, transform = "response", random = FALSE, length = 25, preserve_range = TRUE, predict = "response", keep_draws = FALSE, draws = NULL, seed = NULL, centrality = "median", ci = 0.89, ci_method = "hdi", ...) {

  # Checks
  if (any(class(model) == "stanreg") & !requireNamespace("rstanarm", quietly = TRUE)) {
    stop("This function needs `rstanarm` to be installed.")
  }
  if (any(class(model) == "brmsfit") & !requireNamespace("brms", quietly = TRUE)) {
    stop("This function needs `brms` to be installed.")
  }

  # Initialize
  args <- .estimate_response_init(model, data, transform, random, length, preserve_range, predict, ...)
  data <- args$data

  # Predict link or response
  if (predict == "link" && !insight::model_info(model)$is_ordinal) {
    if(any(class(model) == "brmsfit")){
      posteriors <- brms::posterior_linpred(model, newdata = data, re.form = args$re.form, seed = seed, draws = draws, scale = args$transform)
    } else{
      posteriors <- rstanarm::posterior_linpred(model, newdata = data, re.form = args$re.form, seed = seed, draws = draws, transform = args$transform)
    }
  } else {
    if (any(class(model) == "brmsfit")){
      posteriors <- brms::posterior_predict(model, newdata = data, re.form = args$re.form, seed = seed, draws = draws, transform = NULL)
    } else{
      posteriors <- rstanarm::posterior_predict(model, newdata = data, re.form = args$re.form, seed = seed, draws = draws, transform = "response")
    }

  }

  # Summary
  prediction <- .summarize_posteriors(as.data.frame(posteriors, stringsAsFactors = FALSE), ci = ci, centrality = centrality, ci_method = ci_method, test = NULL, rope_range = NULL)
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
  row.names(out) <- NULL
  out
}










# Other - rstanarm -------------------------------------------------------------------




#' @rdname estimate_response
#' @export
#' @export
estimate_link <- function(model, data = "grid", transform = "response", random = FALSE, length = 25, preserve_range = TRUE, ...) {
  UseMethod("estimate_link")
}




#' @rdname estimate_response.stanreg
#' @export
estimate_link.stanreg <- function(model, data = "grid", transform = "response", random = FALSE, length = 25, preserve_range = TRUE, predict = "link", keep_draws = FALSE, draws = NULL, seed = NULL, centrality = "median", ci = 0.89, ci_method = "hdi", ...) {
  estimate_response(model, data = data, transform = transform, random = random, length = length, preserve_range = preserve_range, predict = predict, keep_draws = keep_draws, draws = draws, seed = seed, centrality = centrality, ci = ci, ci_method = ci_method, ...)
}


#' @rdname estimate_response.stanreg
#' @export
estimate_response.data.frame <- function(model, data = NULL, transform = "response", random = FALSE, length = 25, preserve_range = TRUE, predict = "link", keep_draws = FALSE, draws = NULL, seed = NULL, centrality = "median", ci = 0.89, ci_method = "hdi", ...) {

  # Try retrieve model from data
  if ((is.null(data) | all(data == "grid")) & !is.null(attributes(model)$model)) {
    data <- attributes(model)$model
  }

  estimate_response(data, data = model, transform = transform, random = random, length = length, preserve_range = preserve_range, predict = predict, keep_draws = keep_draws, draws = draws, seed = seed, centrality = centrality, ci = ci, ci_method = ci_method, ...)
}


#' @rdname estimate_response.stanreg
#' @export
estimate_link.data.frame <- function(model, data = "grid", transform = "response", random = FALSE, length = 25, preserve_range = TRUE, predict = "link", keep_draws = FALSE, draws = NULL, seed = NULL, centrality = "median", ci = 0.89, ci_method = "hdi", ...) {

  # Try retrieve model from data
  if ((is.null(data) | all(data == "grid")) & !is.null(attributes(model)$model)) {
    data <- attributes(model)$model
  }

  estimate_response(data, data = model, transform = transform, random = random, length = length, preserve_range = preserve_range, predict = predict, keep_draws = keep_draws, draws = draws, seed = seed, centrality = centrality, ci = ci, ci_method = ci_method, ...)
}



# brms --------------------------------------------------------------------

#' @export
estimate_response.brmsfit <- estimate_response.stanreg

#' @export
estimate_link.brmsfit <- estimate_link.stanreg

