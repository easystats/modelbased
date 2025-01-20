#' Estimate Marginal Means (Model-based average at each factor level)
#'
#' Estimate average value of response variable at each factor level or
#' representative value, respectively at values defined in a "data grid" or
#' "reference grid". For plotting, check the examples in
#' [visualisation_recipe()]. See also other related functions such as
#' [estimate_contrasts()] and [estimate_slopes()].
#'
#' @param model A statistical model.
#' @param by The (focal) predictor variable(s) at which to evaluate the desired
#' effect / mean / contrasts. Other predictors of the model that are not
#' included here will be collapsed and "averaged" over (the effect will be
#' estimated across them). The `by` argument is used to create a "reference grid"
#' or "data grid" with representative values for the focal predictors. `by`
#' can be a character (vector) naming the focal predictors (and optionally,
#' representative values or levels), or a list of named elements. See details
#' in [`insight::get_datagrid()`] to learn more about how to create data grids
#' for predictors of interest.
#' @param predict Is passed to the `type` argument in `emmeans::emmeans()` (when
#' `backend = "emmeans"`) or in `marginaleffects::avg_predictions()` (when
#' `backend = "marginaleffects"`). For emmeans, see also
#' [this vignette](https://CRAN.R-project.org/package=emmeans/vignettes/transformations.html).
#' Valid options for `predict`` are:
#'
#' * `backend = "emmeans"`: `predict` can be `"response"`, `"link"`, `"mu"`,
#'   `"unlink"`, or `"log"`. If `predict = NULL` (default), the most appropriate
#'   transformation is selected (which usually is `"response"`).
#' * `backend = "marginaleffects"`: `predict` can be `"response"`, `"link"` or
#'   any valid `type` option supported by model's class `predict()` method (e.g.,
#'   for zero-inflation models from package **glmmTMB**, you can choose
#'   `predict = "zprob"` or `predict = "conditional"` etc., see
#'   [glmmTMB::predict.glmmTMB]). By default, when `predict = NULL`, the most
#'   appropriate transformation is selected, which usually returns predictions
#'   or contrasts on the response-scale.
#'
#' `"link"` will leave the values on scale of the linear predictors.
#' `"response"` (or `NULL`) will transform them on scale of the response
#' variable. Thus for a logistic model, `"link"` will give estimations expressed
#' in log-odds (probabilities on logit scale) and `"response"` in terms of
#' probabilities. To predict distributional parameters (called "dpar" in other
#' packages), for instance when using complex formulae in `brms` models, the
#' `predict` argument can take the value of the parameter you want to estimate,
#' for instance `"sigma"`, `"kappa"`, etc.
#' @param marginalize Character string, indicating the type of marginalization.
#' This dictates how the predictions are "averaged" over the non-focal predictors,
#' i.e. those variables that are not specified in `by` or `contrast`.
#' - `"average"` (default): Takes the mean value for non-focal numeric
#'   predictors and marginalizes over the factor levels of non-focal terms,
#'   which computes a kind of "weighted average" for the values at which these
#'   terms are hold constant. These predictions are a good representation of the
#'   sample, because all possible values and levels of the non-focal predictors
#'   are considered. It answers the question, "What is the predicted value for
#'   an 'average' observation in *my data*?". It refers to randomly picking a
#'   subject of your sample and the result you get on average. This approach is
#'   the one taken by default in the `emmeans` package.
#' - `"population"`: Non-focal predictors are marginalized over the observations
#'   in the sample, where the sample is replicated multiple times to produce
#'   "counterfactuals" and then takes the average of these predicted values
#'   (aggregated/grouped by the focal terms). It can be considered as
#'   extrapolation to a hypothetical target population. Counterfactual
#'   predictions are useful, insofar as the results can also be transferred to
#'   other contexts (Dickerman and Hernan, 2020). It answers the question, "What
#'   is the predicted response value for the 'average' observation in *the
#'   broader target population*?". It does not only refer to the actual data in
#'   your observed sample, but also "what would be if" we had more data, or if
#'   we had data from a different sample.
#'
#' In other words, the distinction between marginalization types resides in whether
#' the prediction are made for:
#' - A specific "individual" from the sample (i.e., a specific combination of
#'   predictor values): this is what is obtained when using [`estimate_relation()`]
#'   and the other prediction functions.
#' - An average individual from the sample: obtained with
#'   `estimate_means(..., marginalize = "average")`
#' - The broader, hypothetical target population: obtained with
#'   `estimate_means(..., marginalize = "population")`
#' @param backend Whether to use `"emmeans"` or `"marginaleffects"` as a backend.
#' Results are usually very similar. The major difference will be found for mixed
#' models, where `backend = "marginaleffects"` will also average across random
#' effects levels, producing "marginal predictions" (instead of "conditional
#' predictions", see Heiss 2022).
#'
#' You can set a default backend via `options()`, e.g. use
#' `options(modelbased_backend = "emmeans")` to use the **emmeans** package or
#' `options(modelbased_backend = "marginaleffects")` to set **marginaleffects**
#' as default backend.
#' @param transform Deprecated, please use `predict` instead.
#' @param verbose Use `FALSE` to silence messages and warnings.
#' @param ... Other arguments passed, for instance, to [insight::get_datagrid()],
#' to functions from the **emmeans** or **marginaleffects** package, or to process
#' Bayesian models via [bayestestR::describe_posterior()]. Examples:
#' - `insight::get_datagrid()`: Argument such as `length` or `range` can be used
#'   to control the (number of) representative values.
#' - **marginaleffects**: Internally used functions are `avg_predictions()` for
#'   means and contrasts, and `avg_slope()` for slopes. Therefore, arguments
#'   for instance like `vcov`, `transform`, `equivalence` or `slope` can be
#'   passed to those functions.
#' - **emmeans**: Internally used functions are `emmeans()` and `emtrends()`.
#'   Additional arguments can be passed to these functions.
#' - Bayesian models: For Bayesian models, parameters are cleaned using
#'   `describe_posterior()`, thus, arguments like, for example, `centrality`,
#'   `rope_range`, or `test` are passed to that function.
#'
#' @inheritParams parameters::model_parameters.default
#' @inheritParams estimate_expectation
#' @inherit estimate_slopes details
#'
#' @return A data frame of estimated marginal means.
#'
#' @references
#' Dickerman, Barbra A., and Miguel A. Hernán. 2020. Counterfactual Prediction
#' Is Not Only for Causal Inference. European Journal of Epidemiology 35 (7):
#' 615–17. \doi{10.1007/s10654-020-00659-8}
#'
#' Heiss, A. (2022). Marginal and conditional effects for GLMMs with
#' {marginaleffects}. Andrew Heiss. \doi{10.59350/xwnfm-x1827}
#'
#' @examplesIf all(insight::check_if_installed(c("marginaleffects", "see", "lme4"), quietly = TRUE))
#' library(modelbased)
#'
#' # Frequentist models
#' # -------------------
#' model <- lm(Petal.Length ~ Sepal.Width * Species, data = iris)
#'
#' estimate_means(model)
#'
#' # the `length` argument is passed to `insight::get_datagrid()` and modulates
#' # the number of representative values to return for numeric predictors
#' estimate_means(model, by = c("Species", "Sepal.Width"), length = 2)
#'
#' # an alternative way to setup your data grid is specify the values directly
#' estimate_means(model, by = c("Species", "Sepal.Width = c(2, 4)"))
#'
#' # or use one of the many predefined "tokens" that help you creating a useful
#' # data grid - to learn more about creating data grids, see help in
#' # `?insight::get_datagrid`.
#' estimate_means(model, by = c("Species", "Sepal.Width = [fivenum]"))
#'
#' \dontrun{
#' # same for factors: filter by specific levels
#' estimate_means(model, by = "Species=c('versicolor', 'setosa')")
#' estimate_means(model, by = c("Species", "Sepal.Width=0"))
#'
#' # estimate marginal average of response at values for numeric predictor
#' estimate_means(model, by = "Sepal.Width", length = 5)
#' estimate_means(model, by = "Sepal.Width=c(2, 4)")
#'
#' # or provide the definition of the data grid as list
#' estimate_means(
#'   model,
#'   by = list(Sepal.Width = c(2, 4), Species = c("versicolor", "setosa"))
#' )
#'
#' # Methods that can be applied to it:
#' means <- estimate_means(model, by = c("Species", "Sepal.Width=0"))
#'
#' plot(means) # which runs visualisation_recipe()
#' standardize(means)
#'
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' model <- lme4::lmer(
#'   Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor),
#'   data = data
#' )
#' estimate_means(model)
#' estimate_means(model, by = "Sepal.Width", length = 3)
#' }
#' @export
estimate_means <- function(model,
                           by = "auto",
                           predict = NULL,
                           ci = 0.95,
                           marginalize = "average",
                           backend = getOption("modelbased_backend", "marginaleffects"),
                           transform = NULL,
                           verbose = TRUE,
                           ...) {
  ## TODO: remove deprecation warning later
  if (!is.null(transform)) {
    insight::format_warning("Argument `transform` is deprecated. Please use `predict` instead.")
    predict <- transform
  }

  # validate input
  marginalize <- insight::validate_argument(
    marginalize,
    c("average", "population", "specific")
  )

  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emmeans(model, by = by, predict = predict, verbose = verbose, ...)
    means <- .format_emmeans_means(estimated, model, ci = ci, verbose = verbose, ...)
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalmeans(model, by = by, predict = predict, ci = ci, marginalize = marginalize, verbose = verbose, ...) # nolint
    means <- format(estimated, model, ...)
  }

  # restore attributes later
  info <- attributes(estimated)

  # Table formatting
  attr(means, "table_title") <- c(ifelse(
    marginalize == "specific",
    "Model-based Predictions",
    "Estimated Marginal Means"
  ), "blue")
  attr(means, "table_footer") <- .table_footer(
    means,
    type = ifelse(marginalize == "specific", "predictions", "means"),
    by = info$by,
    model = model,
    info = info
  )

  # Add attributes
  attr(means, "model") <- model
  attr(means, "response") <- insight::find_response(model)
  attr(means, "ci") <- ci
  attr(means, "backend") <- backend
  attr(means, "coef_name") <- intersect(.valid_coefficient_names(), colnames(means))

  # add attributes from workhorse function
  attributes(means) <- utils::modifyList(attributes(means), info[.info_elements()])

  # Output
  class(means) <- unique(c("estimate_means", class(means)))
  means
}
