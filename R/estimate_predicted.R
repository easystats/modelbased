#' Model-based response estimates and uncertainty
#'
#' @description
#' After fitting a model, it is useful generate model-based estimates of the
#' response variables for different combinations of predictor values. Such
#' estimates can be used to make inferences about relationships between
#' variables and to make predictions about individual cases.
#'
#' Model-based response estimates and uncertainty can be generated for both the
#' conditional average response values (the regression line or expectation) and
#' for predictions about individual cases. See below for details.
#'
#' @section Expected (average) values:
#'
#' The most important way that various types of response estimates differ is in
#' terms of what quantity is being estimated and the meaning of the uncertainty
#' intervals. The major choices are **expected values** for uncertainty in the
#' regression line and **predicted values** for uncertainty in the individual
#' case predictions.
#'
#' **Expected values** refer to the fitted regression line - the estimated
#' *average* response value (i.e., the "expectation") for individuals with
#' specific predictor values. For example, in a linear model *y* = 2 + 3*x* +
#' 4*z* + *e*, the estimated average *y* for individuals with *x* = 1 and *z* =
#' 2 is 11.
#'
#' For expected values, uncertainty intervals refer to uncertainty in the
#' estimated **conditional average** (where might the true regression line
#' actually fall)? Uncertainty intervals for expected values are also called
#' "confidence intervals".
#'
#' Expected values and their uncertainty intervals are useful for describing the
#' relationship between variables and for describing how precisely a model has
#' been estimated.
#'
#' For generalized linear models, expected values are reported on one of two scales:
#'
#'  - The **link scale** refers to scale of the fitted regression line, after
#'  transformation by the link function. For example, for a logistic regression
#'  (logit binomial) model, the link scale gives expected log-odds. For a
#'  log-link Poisson model, the link scale gives the expected log-count.
#'
#'  - The **response scale** refers to the original scale of the response
#'  variable (i.e., without any link function transformation). Expected values
#'  on the link scale are back-transformed to the original response variable
#'  metric (e.g., expected probabilities for binomial models, expected counts
#'  for Poisson models).
#'
#'
#' @section Individual case predictions:
#'
#' In contrast to expected values, **predicted values** refer to predictions for
#' **individual cases**. Predicted values are also called "posterior
#' predictions" or "posterior predictive draws".
#'
#' For predicted values, uncertainty intervals refer to uncertainty in the
#' **individual response values for each case** (where might any single case
#' actually fall)? Uncertainty intervals for predicted values are also called
#' "prediction intervals" or "posterior predictive intervals".
#'
#' Predicted values and their uncertainty intervals are useful for forecasting
#' the range of values that might be observed in new data, for making decisions
#' about individual cases, and for checking if model predictions are reasonable
#' ("posterior predictive checks").
#'
#' Predicted values and intervals are always on the scale of the original
#' response variable (not the link scale).
#'
#'
#' @section Functions for estimating predicted values and uncertainty:
#'
#' *modelbased* provides 4 functions for generating model-based response
#' estimates and their uncertainty:
#'
#' - **`estimate_expectation()`**:
#'   - Generates **expected values** (conditional average) on the **response scale**.
#'   - The uncertainty interval is a *confidence interval*.
#'   - By default, values are computed using the data used to fit the model.
#'
#' - **`estimate_link()`**:
#'   - Generates **expected values** (conditional average) on the **link scale**.
#'   - The uncertainty interval is a *confidence interval*.
#'   - By default, values are computed using a reference grid spanning the
#'     observed range of predictor values (see [visualisation_matrix()]).
#'
#' - **`estimate_prediction()`**:
#'   - Generates **predicted values** (for individual cases) on the **response scale**.
#'   - The uncertainty interval is a *prediction interval*.
#'   - By default, values are computed using the data used to fit the model.
#'
#' - **`estimate_relation()`**:
#'   - Like `estimate_expectation()`.
#'   - Useful for visualizing a model.
#'   - Generates **expected values** (conditional average) on the **response scale**.
#'   - The uncertainty interval is a *confidence interval*.
#'   - By default, values are computed using a reference grid spanning the
#'     observed range of predictor values (see [visualisation_matrix()]).
#'
#' @section Data for predictions:
#'
#' If the `data = NULL`, values are estimated using the data used to fit the
#' model. If `data = "grid"`, values are computed using a reference grid
#' spanning the observed range of predictor values with
#' [visualisation_matrix()]. This can be useful for model visualization. The
#' number of predictor values used for each variable can be controlled with the
#' `length` argument. `data` can also be a data frame containing columns with
#' names matching the model frame (see [insight::get_data()]). This can be used
#' to generate model predictions for specific combinations of predictor values.
#'
#' @note
#'
#' These functions are built on top of [insight::get_predicted()] and correspond
#' to different specifications of its parameters. It may be useful to read its
#' [documentation](https://easystats.github.io/insight/reference/get_predicted.html),
#' in particular the description of the `predict` argument for additional
#' details on the difference between expected vs. predicted values and link vs.
#' response scales.
#'
#' Additional control parameters can be used to control results from
#' [insight::get_datagrid()] (when `data = "grid"`) and from
#' [insight::get_predicted()] (the function used internally to compute
#' predictions).
#'
#' For plotting, check the examples in [visualisation_recipe()]. Also check out
#' the [Vignettes](https://easystats.github.io/modelbased/articles/) and [README
#' examples](https://easystats.github.io/modelbased/index.html#features) for
#' various examples, tutorials and usecases.
#'
#' @inheritParams estimate_means
#' @inheritParams bayestestR::describe_posterior
#' @param data A data frame with model's predictors to estimate the response. If
#' `NULL`, the model's data is used. If `"grid"`, the model matrix is obtained
#' (through [insight::get_datagrid()]).
#' @param by The predictor variable(s) at which to estimate the response. Other
#' predictors of the model that are not included here will be set to their mean
#' value (for numeric predictors), reference level (for factors) or mode (other
#' types). The `by` argument will be used to create a data grid via
#' `insight::get_datagrid()`, which will then be used as `data` argument. Thus,
#' you cannot specify both `data` and `by` but only of these two arguments.
#' @param ... You can add all the additional control arguments from
#' [insight::get_datagrid()] (used when `data = "grid"`) and
#' [insight::get_predicted()].
#'
#' @return A data frame of predicted values and uncertainty intervals, with
#' class `"estimate_predicted"`. Methods for [`visualisation_recipe()`][visualisation_recipe.estimate_predicted]
#' and [`plot()`][visualisation_recipe.estimate_predicted] are available.
#'
#' @examplesIf all(insight::check_if_installed(c("see", "lme4", "rstanarm"), quietly = TRUE))
#' library(modelbased)
#'
#' # Linear Models
#' model <- lm(mpg ~ wt, data = mtcars)
#'
#' # Get predicted and prediction interval (see insight::get_predicted)
#' estimate_expectation(model)
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
#' z <- standardize(pred, include_response = FALSE)
#' z
#' unstandardize(z, include_response = FALSE)
#'
#' # Logistic Models
#' model <- glm(vs ~ wt, data = mtcars, family = "binomial")
#' estimate_expectation(model)
#' estimate_relation(model)
#'
#' # Mixed models
#' model <- lme4::lmer(mpg ~ wt + (1 | gear), data = mtcars)
#' estimate_expectation(model)
#' estimate_relation(model)
#'
#' # Bayesian models
#' \donttest{
#' model <- suppressWarnings(rstanarm::stan_glm(
#'   mpg ~ wt,
#'   data = mtcars, refresh = 0, iter = 200
#' ))
#' estimate_expectation(model)
#' estimate_relation(model)
#' }
#' @export
estimate_expectation <- function(model,
                                 data = NULL,
                                 by = NULL,
                                 ci = 0.95,
                                 keep_iterations = FALSE,
                                 ...) {
  .estimate_predicted(
    model,
    data = data,
    by = by,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = "expectation",
    ...
  )
}


#' @rdname estimate_expectation
#' @export
estimate_link <- function(model,
                          data = "grid",
                          by = NULL,
                          ci = 0.95,
                          keep_iterations = FALSE,
                          ...) {
  # reset to NULL if only "by" was specified
  if (missing(data) && !missing(by)) {
    data <- NULL
  }

  .estimate_predicted(
    model,
    data = data,
    by = by,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = "link",
    ...
  )
}

#' @rdname estimate_expectation
#' @export
estimate_prediction <- function(model,
                                data = NULL,
                                by = NULL,
                                ci = 0.95,
                                keep_iterations = FALSE,
                                ...) {
  .estimate_predicted(
    model,
    data = data,
    by = by,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = "prediction",
    ...
  )
}

#' @rdname estimate_expectation
#' @export
estimate_relation <- function(model,
                              data = "grid",
                              by = NULL,
                              ci = 0.95,
                              keep_iterations = FALSE,
                              ...) {
  # reset to NULL if only "by" was specified
  if (missing(data) && !missing(by)) {
    data <- NULL
  }

  .estimate_predicted(
    model,
    data = data,
    by = by,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = "expectation",
    ...
  )
}



# Internal ----------------------------------------------------------------

#' @keywords internal
.estimate_predicted <- function(model,
                                data = "grid",
                                by = NULL,
                                predict = "expectation",
                                ci = 0.95,
                                keep_iterations = FALSE,
                                ...) {
  # only "by" or "data", but not both
  if (!is.null(by) && !is.null(data)) {
    insight::format_error("You can only specify one of `by` or `data`, but not both.")
  }

  # call "get_data()" only once...
  model_data <- insight::get_data(model, verbose = FALSE)
  is_model <- insight::is_model(model)

  # model and data properties
  if (is_model) {
    # for models, get predictors, response etc.
    variables <- insight::find_predictors(model, effects = "all", flatten = TRUE)
    model_response <- insight::find_response(model)
    is_nullmodel <- insight::is_nullmodel(model)
  } else {
    # for stuff like data frame, no response and no null model
    variables <- colnames(model_data)
    model_response <- NULL
    is_nullmodel <- FALSE
  }

  # if "by" is provided, get datagrid
  if (!is.null(by)) {
    data <- insight::get_datagrid(model, by = by, include_response = is_nullmodel, ...)
  }

  is_grid <- identical(data, "grid")

  # check for correct attributes - a data frame of class `datagrid` may
  # contain all variables of that data frame as "adjusted-for" attribute,
  # which does not necessarily match with actual model predictors. Make sure
  # "adjusted_for" attribute only contains valid variable names
  if (inherits(data, "datagrid") && is_model) {
    adjusted_for <- attr(data, "adjusted_for", exact = TRUE)
    if (!is.null(adjusted_for)) {
      attr(data, "adjusted_for") <- intersect(variables, adjusted_for)
    }
  }

  # If a visualisation_matrix is passed
  if (inherits(model, "visualisation_matrix") || all(inherits(model, "data.frame"))) {
    data_original <- data
    data <- model
    if ("model" %in% names(attributes(model))) {
      model <- attributes(model)$model
    } else if (insight::is_model(data_original)) {
      model <- data_original
    } else {
      insight::format_error("A model must be passed to make predictions.")
    }
  }

  # Get data ----------------
  if (is.null(data)) {
    data <- model_data
  } else if (!is.data.frame(data)) {
    if (is_grid) {
      data <- visualisation_matrix(model, reference = model_data, include_response = is_nullmodel, ...)
    } else {
      insight::format_error(
        "The `data` argument must either NULL, \"grid\" or another data frame."
      )
    }
  }

  # save grid specs for table footer
  grid_specs <- attributes(data)

  # Get response for later residuals -------------
  if (!is.null(model_response) && model_response %in% names(data)) {
    response <- data[[model_response]]
  } else {
    response <- NULL
  }

  # Keep only predictors (and response) --------
  if (!is_grid || is_nullmodel) {
    variables <- c(model_response, variables)
  }
  data <- data[names(data) %in% variables]

  # Restore factor levels
  data <- datawizard::data_restoretype(data, model_data)

  # Get predicted ----------------
  predictions <- insight::get_predicted(model,
    data = data,
    predict = predict,
    ci = ci,
    ...
  )
  out <- as.data.frame(predictions, keep_iterations = keep_iterations)
  out <- cbind(data, out)

  # remove response variable from data frame, as this variable is predicted
  if (!is.null(model_response) && model_response %in% colnames(out)) {
    out[[model_response]] <- NULL
  }

  # Add residuals
  if (!is.null(response)) {
    out$Residuals <- response - out$Predicted
  }

  # Store relevant information
  attr(out, "ci") <- ci
  attr(out, "keep_iterations") <- keep_iterations
  attr(out, "response") <- model_response
  attr(out, "model") <- model
  attr(out, "table_title") <- c(paste0("Model-based ", tools::toTitleCase(predict)), "blue")
  attr(out, "table_footer") <- .estimate_predicted_footer(model, grid_specs, out)
  attributes(out) <- c(attributes(out), grid_specs[!names(grid_specs) %in% names(attributes(out))])

  # Class
  class(out) <- c(paste0("estimate_", predict), "estimate_predicted", "see_estimate_predicted", class(out))

  out
}



# Utils -------------------------------------------------------------------

#' @keywords internal
.estimate_predicted_footer <- function(model, grid_specs, predictions) {
  footer <- paste0("\nVariable predicted: ", insight::find_response(model), "\n")

  if ("at" %in% names(grid_specs)) {
    footer <- paste0(footer, "Predictors modulated: ", toString(grid_specs$at), "\n")
  }

  if ("adjusted_for" %in% names(grid_specs) &&
    length(grid_specs$adjusted_for) >= 1 &&
    !(length(grid_specs$adjusted_for) == 1 && is.na(grid_specs$adjusted_for))) {
    # if we have values of adjusted terms, add these here
    if (all(grid_specs$adjusted_for %in% colnames(predictions))) {
      # get values at which non-focal terms are hold constant
      adjusted_values <- sapply(grid_specs$adjusted_for, function(i) {
        predictions[[i]][1]
      })
      # at values to names of non-focal terms (footer)
      if (is.numeric(adjusted_values)) {
        grid_specs$adjusted_for <- sprintf("%s (%.2g)", grid_specs$adjusted_for, adjusted_values)
      } else {
        grid_specs$adjusted_for <- sprintf("%s (%s)", grid_specs$adjusted_for, adjusted_values)
      }
    }
    footer <- paste0(footer, "Predictors controlled: ", toString(grid_specs$adjusted_for), "\n")
  }

  c(footer, "blue")
}
