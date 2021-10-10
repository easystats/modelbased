#' Estimate predicted values and uncertainty from models
#'
#' Using models to generate predictions and their uncertainty is useful for many purposes, including assessing model performance, visualizing relationships, making forecasts, or informing decisions.
#' \cr\cr
#' There are several kinds of model "predictions" that are used for different goals. See belwo for details.
#'
#' @details
#'
#' @section Expected (average) values versus individual predictions (forecasts):
#'
#' The most important way that various types of predictions differ is in terms of what quantity is being estimated and the meaning of the uncertainty intervals.
#'
#' - **Expected** values refer the the fitted regression line---the estimated **average** response value (i.e., the "expectation") for individuals with a specific set of predictor values. For example, in a linear model *y_i* = 2 + 3*x_i* + *e_i*, the estimated average *y* for individuals with *x* = 2 is 8. For expected values, uncertainty intervals refer to uncertainty in the estimated **conditional average** (where might the true regression line actually fall)? Uncertainty intervals for expected values are also called "confidence intervals". Expected values and their uncertainty intervals are useful for describing the general relationship between variables and for describing how precisely a model has been estimated.
#'
#' - **Predicted** values refer to forecasts or predictions for **individual cases**. Predicted values are also called "posterior predictions". For predicted values, uncertainty intervals refer to uncertainty in the **individual response values for each case** (where might any single case actually fall)? Uncertainty intervals for predicted values are also called "prediction intervals" and "posterior predictive intervals". Predicted values and their uncertainty intervals are useful for forecasting the range of values that might be observed in new data or for making decisions about individual cases.
#'
#' @section Response metric:
#'
#' Various types of predictions also differ in the metric or scale the predictions are expressed in.
#'
#' - The **link scale** refers to scale of the fitted regression line. For linear models, this is the original response variable scale. However, for generalized linear models, it is the scale of the response after transformation by the link function (e.g., log-odds for logit binomial models, log-counts for log-linked Poisson models, log-probability for log-linked beta models).
#'
#' - The **response scale** refers to the original scale of the response variable (i.e., without any link function transformation). For linear models, this is the original response variable scale. However, for generalized linear models, expected values on the link scale are back-transformed to the original response variable metric (e.g., predicted probabilities for binomial models, counts for Poisson models, predicted probabilities for beta models).
#'
#' @section *modelbased* functions for estimating predicted values and uncertainty:
#'
#' *modelbased* provides 4 functions for generating different types of predictions and their uncertainty from models:
#'
#' - **`estimate_link()`**: Generates **expected values** (conditional average) on the **link scale**. The uncertainty interval is a *confidence interval*. By default, values are estimated using a reference grid spanning the observed range of predictor values (see [visualisation_matrix()]).
#'
#' - **`estimate_expectation()`**: Generates **expected values** (conditional average) on the **response scale**. The uncertainty interval is a *confidence interval*. By default, values are estimated using the data used to fit the model.
#'
#' - **`estimate_prediction()`**: Generates **predicted values** (for individual cases) on the **response scale**. The uncertainty interval is a *prediction interval*. By default, values are estimated using the data used to fit the model.
#'
#' - **`estimate_relation()`**: Like `estimate_expectation()`. Generates **expected values** (conditional average) on the **response scale**. The uncertainty interval is a *confidence interval*. By default, values are estimated using a reference grid spanning the observed range of predictor values (see [visualisation_matrix()]).
#'
#' `estimate_response()` is a deprecated alias for `estimate_expectation()`.
#'
#' @section Data for predictions:
#'
#' If the `data = NULL`, values are estimated using the data used to fit the model. If `data = "grid"`, values are estimated a reference grid spanning the observed range of predictor values with [visualisation_matrix()]. This can be useful for model visualization. The number of predictor values used for each variable can be controlled with the `length` argument. `data` can also be a data frame containing columns with names matching the model frame (see [insight::get_data()]). This can be used to generate model predictions for specific combinations of predictor values.
#'
#' @note
#'
#' These functions are built on top of [insight::get_predicted()] and correspond to different specifications of its parameters. It may be useful to read its [documentation](https://easystats.github.io/insight/reference/get_predicted.html), in particular the description of the `predict` argument for additional details on the difference between expected vs. predicted values and link vs. response scales.
#' \cr\cr
#'
#' Additional control parameters can be used to control results from [visualisation_matrix()] (when `data = "grid"`) and from [insight::get_predicted()] (the function used internally to comptue predictions).
#' \cr\cr
#'
#' For plotting, check the examples in [visualisation_recipe()]. Also check out the [Vignettes](https://easystats.github.io/modelbased/articles/) and [README examples](https://easystats.github.io/modelbased/index.html#features) for various examples, tutorials and usecases.
#'
#' @inheritParams estimate_means
#' @inheritParams bayestestR::describe_posterior
#' @param data A data frame with model's predictors to estimate the response. If
#'   `NULL`, the model's data is used. If "grid", the model matrix is obtained
#'   (through [visualisation_matrix()]).
#' @param ... You can add all the additional control arguments from [visualisation_matrix()] (used when `data = "grid"`) and [insight::get_predicted()].
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
#' @return A data frame of predicted values and uncertainty intervals, with class `"estimate_predicted"`. Methods for [`visualisation_recipe()`][visualisation_recipe.estimate_predicted] and [`plot()`][visualisation_recipe.estimate_predicted] are available.
#'
#' @export
estimate_link <- function(model,
                          data = "grid",
                          ci = 0.95,
                          keep_iterations = FALSE,
                          ...) {
  .estimate_predicted(
    model,
    data = data,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = "link",
    ...
  )
}

#' @rdname estimate_link
#' @export
estimate_relation <- function(model,
                              data = "grid",
                              ci = 0.95,
                              keep_iterations = FALSE,
                              ...) {
  .estimate_predicted(
    model,
    data = data,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = "expectation",
    ...
  )
}

#' @rdname estimate_link
#' @export
estimate_expectation <- function(model,
                                 data = NULL,
                                 ci = 0.95,
                                 keep_iterations = FALSE,
                                 ...) {
  .estimate_predicted(
    model,
    data = data,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = "expectation",
    ...
  )
}


#' @rdname estimate_link
#' @export
estimate_response <- function(...) {
  message(insight::format_message(
    "`estimate_response()` is deprecated.", "Please use `estimate_expectation()` (for conditional expected values) or `estimate_prediction()` (for individual case predictions) instead."
  ))

  estimate_expectation(...)
}


#' @rdname estimate_link
#' @export
estimate_prediction <- function(model,
                                data = NULL,
                                ci = 0.95,
                                keep_iterations = FALSE,
                                ...) {
  .estimate_predicted(
    model,
    data = data,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = "prediction",
    ...
  )
}






# Internal ----------------------------------------------------------------

#' @keywords internal
.estimate_predicted <- function(model,
                                data = "grid",
                                predict = "expectation",
                                ci = 0.95,
                                keep_iterations = FALSE,
                                ...) {


  # If a visualisation_matrix is passed
  if (inherits(model, "visualisation_matrix") || all(class(model) == "data.frame")) {
    data_original <- data
    data <- model
    if ("model" %in% names(attributes(model))) {
      model <- attributes(model)$model
    } else if (insight::is_model(data_original)) {
      model <- data_original
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
  data <- datawizard::data_restoretype(data, insight::get_data(model))

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

  if ("at" %in% names(grid_specs)) {
    footer <- paste0(footer, "\nPredictors modulated: ", paste0(grid_specs$at, collapse = ", "))
  }

  if ("adjusted_for" %in% names(grid_specs)) {
    if (!is.na(grid_specs$adjusted_for)) {
      footer <- paste0(footer, "\nPredictors controlled: ", paste0(grid_specs$adjusted_for, collapse = ", "))
    }
  }

  c(footer, "blue")
}
