#' Generates predictions for models
#'
#' \code{estimate_link} is a shortcut to \code{estimate_response} with \code{data = "grid"}. \code{estimate_response} would be used in the context of generating actual predictions for the existing or new data, whereas \code{estimate_link} is more relevant in the context of visualisation and plotting. There are many control parameters that are not listed here but can be used, such as the arguments from  \code{\link{visualisation_matrix}} (used when \code{data = "grid"}) and from \code{\link[insight:get_predicted]{insight::get_predicted()}} (the function to compute predictions used internally).
#'
#' @inheritParams estimate_contrasts
#' @param data A data frame with model's predictors to estimate the response. If NULL, the model's data is used. If "grid", the model matrix is obtained (through \code{\link{visualisation_matrix}}).
#' @param predict Can be "response" (default) or "link". The former predicts the the outcome per se, while the latter predicts the link function (i.e., the regression "line"), equivalent to estimating the \code{fit}. In other words, \code{estimate_response(model, predict="link")} is equivalent to \code{estimate_link(model)}.
#' @param keep_iterations Only relevant for Bayesian models or simulated models. If \code{TRUE}, will keep all prediction iterations (draws). You can reshape them by running \code{\link[bayestestR:reshape_iterations]{bayestestR::reshape_iterations()}}.
#' @param ... You can add all the additional control arguments from \code{\link{visualisation_matrix}} (used when \code{data = "grid"}) and \code{\link[insight:get_predicted]{insight::get_predicted()}}.
#'
#' @examples
#' library(modelbased)
#'
#' # Linear Models
#' model <- lm(mpg ~ wt, data = mtcars)
#' estimate_response(model)
#' estimate_link(model)
#'
#' # Logistic Models
#' model <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' estimate_response(model)
#' estimate_link(model)
#'
#' # Mixed models
#' if (require("lme4")) {
#'   model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#'   estimate_response(model)
#'   estimate_link(model)
#' }
#'
#' # Bayesian models
#' if (require("rstanarm")) {
#'   model <- rstanarm::stan_glm(mpg ~ wt, data = mtcars, refresh=0)
#'   estimate_response(model)
#'   estimate_link(model)
#' }
#' @return A dataframe of predicted values.
#' @export
estimate_response <- function(model, data = NULL, predict = "response", keep_iterations = FALSE, ...) {

  # Get data ----------------
  if (is.null(data)) {
    data <- insight::get_data(model)
  } else if (!is.data.frame(data)) {
    if (data == "grid") {
      data <- visualisation_matrix(model, reference = insight::get_data(model), ...)
    } else {
      stop('The `data` argument must either NULL, "grid" or another data.frame.')
    }
  }
  data <- data[names(data) %in% insight::find_predictors(model, effects = "all", flatten = TRUE)]

  # Restore factor levels
  data <- .restore_factor_levels(data, insight::get_data(model))

  # Get predicted ----------------
  ci_type <- ifelse(predict == "link", "confidence", "prediction")
  predicted <- insight::get_predicted(model, newdata = data, ci_type = ci_type, ...)
  ci <- attributes(predicted)$ci

  # Format predicted ----------------
  if(insight::model_info(model)$is_bayesian) {
    out <- bayestestR::describe_posterior(predicted, ci = ci, ...)
    centrality <- c("Median", "Mean", "MAP")[c("Median", "Mean", "MAP") %in% names(out)][1]
    out$Predicted <- out[[centrality]]
  } else {
    out <- as.data.frame(predicted)
    centrality <- NULL
  }

  # Select columns
  out <- out[c("Predicted", "CI_low", "CI_high")[c("Predicted", "CI_low", "CI_high") %in% names(out)]]
  if(keep_iterations && "iter_1" %in% names(predicted)) out <- cbind(out, predicted)

  # Bind data and predicted ----------------
  out <- cbind(data, out)
  row.names(out) <- NULL

  # Prepare output
  attr(out, "ci") <- ci
  attr(out, "ci_method") <- attributes(predicted)$ci_method
  attr(out, "centrality") <- centrality
  attr(out, "response") <- insight::find_response(model)
  class(out) <- c("estimate_response", "see_estimate_response", class(out))
  out
}





#' @rdname estimate_response
#' @export
estimate_link <- function(model, data = "grid", predict = "link", keep_iterations = FALSE, ...) {
  estimate_response(model, data = data, predict = predict, keep_iterations = keep_iterations, ...)
}
