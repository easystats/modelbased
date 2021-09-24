#' Generate predictions from models
#'
#' Using models to generate "predictions" is useful for many reasons, from assessing the model's performance to visualizing the relationships estimated by the model. It is, however, a term covering a range of different statistical procedures.
#' \cr\cr
#' Making different types of predictions (usually for different goals) using `modelbased` can be achieved through 4 functions:
#' \itemize{
#'   \item{**estimate_link**: Returns a [`reference_grid()`][visualisation_matrix] with predictions on the model's link-scale (with *confidence* intervals)}.
#'   \item{**estimate_relation**: Returns a [`reference grid()`][visualisation_matrix] with predictions on the response scale (with *confidence* intervals)}.
#'   \item{**estimate_expectation**: Makes predictions on the data used for model fitting on the response scale (with *confidence* intervals)}.
#'   \item{**estimate_response**: Makes predictions on the data used for model fitting on the response (transformed for binomial models) scale (with *prediction* intervals)}.
#' }
#' You can see these 4 functions as placed on a gradient ranging from predictions "close to the model" to "close to the actual response data". The first two are typically used for visualizing the effects and relationships estimated by the model, whereas the last two are more likely to be used to visualize the performance of your model.
#' \cr\cr
#' These functions are built on top of [insight::get_predicted()], and correspond to different specifications of its parameters. It is very important to read its [documentation](https://easystats.github.io/insight/reference/get_predicted.html), and in particular the description of its `predict` argument to get a better sense of concepts such as "expectation", "link" and "prediction".
#' \cr\cr
#' The 4 modelbased functions mentioned above differ first and foremost by their default parameters. `estimate_link` and `estimate_relation` have the `data` argument set to [`"grid"()`][visualisation_matrix] by default. Their expected usage is for visualisation of the model's effects. `estimate_expectation` and `estimate_response` have the `data` argument set to `NULL` by default (which retrieves the data used for model's fitting). These functions' are useful in the context of generating actual predictions for the existing or new data, to assess the model's performance or make actual future predictions.
#' \cr\cr
#' There are many control parameters that are not listed here but can
#' be used, such as the arguments from  [visualisation_matrix()] (used
#' when `data = "grid"`) and from
#' [insight::get_predicted()] (the function
#' to compute predictions used internally). For plotting, check the examples in
#' [visualisation_recipe()]. Don't forget to also check out the [Vignettes](https://easystats.github.io/modelbased/articles/) and [README examples](https://easystats.github.io/modelbased/index.html#features) for various examples, tutorials and usecases.
#'
#' @inheritParams estimate_means
#' @inheritParams bayestestR::describe_posterior
#' @param data A data frame with model's predictors to estimate the response. If
#'   NULL, the model's data is used. If "grid", the model matrix is obtained
#'   (through [visualisation_matrix()]).
#' @param ... You can add all the additional control arguments from [visualisation_matrix()] (used when `data = "grid"`) and [insight::get_predicted()].
#'
#' @details Aliases for the "estimate_" functions  with shorter norms are
#'   available for typing convenience:
#'
#'   * `est_link()` is an alias for `estimate_link()`
#'   * `est_relat()` is an alias for `estimate_relation()`
#'   * `est_expect()` is an alias for `estimate_expectation()`
#'   * `est_predict()` is an alias for `estimate_prediction()`
#'   * `est_response()` is an alias for `estimate_response()`
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
estimate_response <- function(model,
                              data = NULL,
                              ci = 0.95,
                              keep_iterations = FALSE,
                              ...) {
  .estimate_predicted(
    model,
    data = data,
    ci = ci,
    keep_iterations = keep_iterations,
    predict = "response",
    ...
  )
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


# Aliases -----------------------------------------------------------------

#' @export
#' @rdname estimate_link
est_link <- estimate_link

#' @export
#' @rdname estimate_link
est_relat <- estimate_relation

#' @export
#' @rdname estimate_link
est_expect <- estimate_expectation

#' @export
#' @rdname estimate_link
est_response <- estimate_response

#' @export
#' @rdname estimate_link
est_predict <- estimate_prediction


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
