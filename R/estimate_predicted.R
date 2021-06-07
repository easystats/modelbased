#' Generates predictions from models
#'
#' \code{estimate_link} is a shortcut to \code{estimate_response} with \code{data = "grid"}. \code{estimate_response} would be used in the context of generating actual predictions for the existing or new data, whereas \code{estimate_link} is more relevant in the context of visualisation and plotting. There are many control parameters that are not listed here but can be used, such as the arguments from  \code{\link{visualisation_matrix}} (used when \code{data = "grid"}) and from \code{\link[insight:get_predicted]{insight::get_predicted()}} (the function to compute predictions used internally). For plotting, check the examples in \code{\link{visualisation_recipe}}.
#'
#' @inheritParams estimate_means
#' @inheritParams bayestestR::describe_posterior
#' @param data A data frame with model's predictors to estimate the response. If NULL, the model's data is used. If "grid", the model matrix is obtained (through \code{\link{visualisation_matrix}}).
#' @param ... You can add all the additional control arguments from \code{\link{visualisation_matrix}} (used when \code{data = "grid"}) and \code{\link[insight:get_predicted]{insight::get_predicted()}}.
#'
#' @examples
#' library(modelbased)
#'
#' # Linear Models
#' model <- lm(mpg ~ wt, data = mtcars)
#'
#' # Get predicted and prediction interval (see insight::get_predicted)
#' estimate_response(model)
#'
#' # Get expected values with confidence interval
#' pred <- estimate_relation(model)
#' pred
#'
#' # Visualisation (see visualisation_recipe())
#' plot(pred)
#'
#' # Standardize predictions
#' pred <- estimate_relation(lm(mpg ~ wt + am, data = mtcars))
#' z <- effectsize::standardize(pred, include_response = FALSE)
#' z
#' effectsize::unstandardize(z, include_response = FALSE)
#'
#' # Logistic Models
#' model <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' estimate_response(model)
#' estimate_relation(model)
#'
#' # Mixed models
#' if (require("lme4")) {
#'   model <- lmer(mpg ~ wt + (1 | gear), data = mtcars)
#'   estimate_response(model)
#'   estimate_relation(model)
#' }
#'
#' # Bayesian models
#' \donttest{
#' if (require("rstanarm")) {
#'   model <- rstanarm::stan_glm(mpg ~ wt, data = mtcars, refresh = 0, iter = 200)
#'   estimate_response(model)
#'   estimate_relation(model)
#' }
#' }
#' @return A dataframe of predicted values.
#' @export
estimate_expectation <- function(model, data = "grid", ci = 0.95, keep_iterations = FALSE, ...) {
  .estimate_predicted(model, data = data, ci = ci, keep_iterations = keep_iterations, predict = "expectation", ...)
}

#' @rdname estimate_expectation
#' @export
estimate_relation <- estimate_expectation



#' @rdname estimate_expectation
#' @export
estimate_link <- function(model, data = "grid", ci = 0.95, keep_iterations = FALSE, ...) {
  .estimate_predicted(model, data = data, ci = ci, keep_iterations = keep_iterations, predict = "link", ...)
}


#' @rdname estimate_expectation
#' @export
estimate_prediction <- function(model, data = NULL, ci = 0.95, keep_iterations = FALSE, ...) {
  .estimate_predicted(model, data = data, ci = ci, keep_iterations = keep_iterations, predict = "prediction", ...)
}

#' @rdname estimate_expectation
#' @export
estimate_response <- estimate_prediction




# Internal ----------------------------------------------------------------

#' @keywords internal
.estimate_predicted <- function(model, data = "grid", predict = "expectation", ci = 0.95, keep_iterations = FALSE, ...) {

  # If a visualisation_matrix is passed
  if (inherits(model, "visualisation_matrix")) {
    data <- model
    if ("model" %in% names(attributes(model))) {
      model <- attributes(model)$model
    } else {
      stop("A model must be passed to make predictions.")
    }
  }


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

  # save grid specs for table footer
  grid_specs <- attributes(data)

  # Get response for later residuals -------------
  if (insight::find_response(model) %in% names(data)) {
    resid <- data[[insight::find_response(model)]]
  } else {
    resid <- NULL
  }

  # Keep only predictors --------
  data <- data[names(data) %in% insight::find_predictors(model, effects = "all", flatten = TRUE)]

  # Restore factor levels
  data <- insight::data_restoretype(data, insight::get_data(model))

  # Get predicted ----------------
  predictions <- insight::get_predicted(model,
    data = data,
    predict = predict,
    ci = ci,
    dispersion_method = "mad",
    ci_method = "hdi",
    ...
  )
  out <- as.data.frame(predictions, keep_iterations = keep_iterations)
  out <- cbind(data, out)

  # Add residuals
  if (!is.null(resid)) {
    out$Residuals <- out$Predicted - resid
  }

  # Store relevant information
  attr(out, "ci") <- ci
  attr(out, "keep_iterations") <- keep_iterations
  attr(out, "response") <- insight::find_response(model)
  attr(out, "model") <- model
  attr(out, "table_title") <- c(paste0("Model-based ", tools::toTitleCase(predict)), "blue")
  attr(out, "table_footer") <- .estimate_predicted_footer(model, grid_specs)
  attributes(out) <- c(attributes(out), grid_specs[!names(grid_specs) %in% names(attributes(out))])

  # Class
  class(out) <- c(paste0("estimate_", predict), "estimate_predicted", "see_estimate_predicted", class(out))

  out
}




# Utils -------------------------------------------------------------------

#' @keywords internal
.estimate_predicted_footer <- function(model, grid_specs) {
  footer <- paste0("\nVariable predicted: ", insight::find_response(model))

  if ("target" %in% names(grid_specs)) {
    footer <- paste0(footer, "\nPredictors modulated: ", paste0(grid_specs$target, collapse = ", "))
  }

  if ("adjusted_for" %in% names(grid_specs)) {
    if (!is.na(grid_specs$adjusted_for)) {
      footer <- paste0(footer, "\nPredictors controlled: ", paste0(grid_specs$adjusted_for, collapse = ", "))
    }
  }

  c(footer, "blue")
}
