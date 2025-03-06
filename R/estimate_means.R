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
#' estimated across them). `by` can be a character (vector) naming the focal
#' predictors, optionally including representative values or levels at which
#' focal predictors are evaluated (e.g., `by="x=c(1,2)"`). When `estimate` is
#' *not* `"average"`, the `by` argument is used to create a "reference grid" or
#' "data grid" with representative values for the focal predictors. In this
#' case, `by` can also be list of named elements. See details in
#' [`insight::get_datagrid()`] to learn more about how to create data grids for
#' predictors of interest.
#' @param predict Is passed to the `type` argument in `emmeans::emmeans()` (when
#' `backend = "emmeans"`) or in `marginaleffects::avg_predictions()` (when
#' `backend = "marginaleffects"`). For emmeans, see also
#' [this vignette](https://CRAN.R-project.org/package=emmeans/vignettes/transformations.html).
#' Valid options for `predict` are:
#'
#' * `backend = "marginaleffects"`: `predict` can be `"response"`, `"link"`,
#'   `"inverse_link"` or any valid `type` option supported by model's class
#'   `predict()` method (e.g., for zero-inflation models from package
#'   **glmmTMB**, you can choose `predict = "zprob"` or `predict = "conditional"`
#'   etc., see [glmmTMB::predict.glmmTMB]). By default, when `predict = NULL`,
#'   the most appropriate transformation is selected, which usually returns
#'   predictions or contrasts on the response-scale. The `"inverse_link"` is a
#'   special option, comparable to *marginaleffects*' `invlink(link)` option. It
#'   will calculate predictions on the link scale and then back-transform to the
#'   response scale.
#' * `backend = "emmeans"`: `predict` can be `"response"`, `"link"`, `"mu"`,
#'   `"unlink"`, or `"log"`. If `predict = NULL` (default), the most appropriate
#'   transformation is selected (which usually is `"response"`).
#'
#' `"link"` will leave the values on scale of the linear predictors.
#' `"response"` (or `NULL`) will transform them on scale of the response
#' variable. Thus for a logistic model, `"link"` will give estimations expressed
#' in log-odds (probabilities on logit scale) and `"response"` in terms of
#' probabilities. To predict distributional parameters (called "dpar" in other
#' packages), for instance when using complex formulae in `brms` models, the
#' `predict` argument can take the value of the parameter you want to estimate,
#' for instance `"sigma"`, `"kappa"`, etc.
#'
#' `"response"` and `"inverse_link"` both return predictions on the response
#' scale, however, `"response"` first calculates predictions on the response
#' scale for each observation and *then* aggregates them by groups or levels
#' defined in `by`. `"inverse_link"` first calculates predictions on the link
#' scale for each observation, then aggregates them by groups or levels defined
#' in `by`, and finally back-transforms the predictions to the response scale.
#' Both approaches have advantages and disadvantages. `"response"` usually
#' produces less biased predictions, but confidence intervals might be outside
#' reasonable bounds (i.e., for instance can be negative for count data). The
#' `"inverse_link"` approach is more robust in terms of confidence intervals, but
#' might produce biased predictions. In particular for mixed models, using
#' `"response"` is recommended, because averaging across random effects groups
#' is more accurate.
#' @param estimate The `estimate` argument determines how predictions are
#' averaged ("marginalized") over variables not specified in `by` or `contrast`
#' (non-focal predictors). It controls whether predictions represent a "typical"
#' individual, an "average" individual from the sample, or an "average"
#' individual from a broader population.
#' - `"typical"` (Default): Calculates predictions for a balanced data grid
#'   representing all combinations of focal predictor levels (specified in `by`).
#'   For non-focal numeric predictors, it uses the mean; for non-focal
#'   categorical predictors, it marginalizes (averages) over the levels. This
#'   represents a "typical" observation based on the data grid and is useful for
#'   comparing groups. It answers: "What would the average outcome be for a
#'   'typical' observation?". This is the default approach when estimating
#'   marginal means using the *emmeans* package.
#' - `"average"`: Calculates predictions for each observation in the sample and
#'   then averages these predictions within each group defined by the focal
#'   predictors. This reflects the sample's actual distribution of non-focal
#'   predictors, not a balanced grid. It answers: "What is the predicted value
#'   for an average observation in my data?"
#' - `"population"`: "Clones" each observation, creating copies with all
#'   possible combinations of focal predictor levels. It then averages the
#'   predictions across these "counterfactual" observations (non-observed
#'   permutations) within each group. This extrapolates to a hypothetical
#'   broader population, considering "what if" scenarios. It answers: "What is
#'   the predicted response for the 'average' observation in a broader possible
#'   target population?" This approach entails more assumptions about the
#'   likelihood of different combinations, but can be more apt to generalize.
#'   This is also the option that should be used for **G-computation**
#'   (_Chatton and Rohrer 2024_).
#'
#' You can set a default option for the `estimate` argument via `options()`,
#' e.g. `options(modelbased_estimate = "average")`
#' @param backend Whether to use `"marginaleffects"` or `"emmeans"`as a backend.
#' Results are usually very similar. The major difference will be found for mixed
#' models, where `backend = "marginaleffects"` will also average across random
#' effects levels, producing "marginal predictions" (instead of "conditional
#' predictions", see Heiss 2022).
#'
#' You can set a default backend via `options()`, e.g. use
#' `options(modelbased_backend = "emmeans")` to use the **emmeans** package or
#' `options(modelbased_backend = "marginaleffects")` to set **marginaleffects**
#' as default backend.
#' @param transform A function applied to predictions and confidence intervals
#' to (back-) transform results, which can be useful in case the regression
#' model has a transformed response variable (e.g., `lm(log(y) ~ x)`). For
#' Bayesian models, this function is applied to individual draws from the
#' posterior distribution, before computing summaries. Can also be `TRUE`, in
#' which case `insight::get_transformation()` is called to determine the
#' appropriate transformation-function. Note that no standard errors are returned
#' when transformations are applied.
#' @param verbose Use `FALSE` to silence messages and warnings.
#' @param ... Other arguments passed, for instance, to [insight::get_datagrid()],
#' to functions from the **emmeans** or **marginaleffects** package, or to process
#' Bayesian models via [bayestestR::describe_posterior()]. Examples:
#' - `insight::get_datagrid()`: Argument such as `length`, `digits` or `range`
#'   can be used to control the (number of) representative values.
#' - **marginaleffects**: Internally used functions are `avg_predictions()` for
#'   means and contrasts, and `avg_slope()` for slopes. Therefore, arguments for
#'   instance like `vcov`, `equivalence`, `df`, `slope` or even `newdata` can be
#'   passed to those functions. A `weights` argument is passed to the `wts`
#'   argument in `avg_predictions()` or `avg_slopes()`, however, weights can
#'   only be applied when `estimate` is `"average"` or `"population"` (i.e. for
#'   those marginalization options that do not use data grids). Other arguments,
#'   such as `re.form` or `allow.new.levels`, may be passed to `predict()` (which
#'   is internally used by *marginaleffects*) if supported by that model class.
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
#' @section Global Options to Customize Estimation of Marginal Means:
#'
#' - `modelbased_backend`: `options(modelbased_backend = <string>)` will set a
#'   default value for the `backend` argument and can be used to set the package
#'   used by default to calculate marginal means. Can be `"marginalmeans"` or
#'   `"emmeans"`.
#'
#' - `modelbased_estimate`: `options(modelbased_estimate = <string>)` will
#'   set a default value for the `estimate` argument.
#'
#' @references
#' Chatton, A. and Rohrer, J.M. 2024. The Causal Cookbook: Recipes for
#' Propensity Scores, G-Computation, and Doubly Robust Standardization. Advances
#' in Methods and Practices in Psychological Science. 2024;7(1).
#' \doi{10.1177/25152459241236149}
#'
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
#' # grids for numeric predictors, combine range and length
#' model <- lm(Sepal.Length ~ Sepal.Width * Petal.Length, data = iris)
#' # range from minimum to maximum spread over four values,
#' # and mean +/- 1 SD (a total of three values)
#' estimate_means(
#'   model,
#'   by = c("Sepal.Width", "Petal.Length"),
#'   range = c("range", "sd"),
#'   length = c(4, 3)
#' )
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
                           estimate = getOption("modelbased_estimate", "typical"),
                           transform = NULL,
                           backend = getOption("modelbased_backend", "marginaleffects"),
                           verbose = TRUE,
                           ...) {
  # validate input
  estimate <- insight::validate_argument(
    estimate,
    c("typical", "population", "specific", "average")
  )

  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emmeans(
      model,
      by = by,
      predict = predict,
      verbose = verbose,
      ...
    )
    means <- .format_emmeans_means(estimated, model, ci = ci, verbose = verbose, ...)
  } else {
    # Marginalmeans ------------------------------------------------------------
    estimated <- get_marginalmeans(
      model,
      by = by,
      predict = predict,
      ci = ci,
      estimate = estimate,
      transform = transform,
      verbose = verbose,
      ...
    )
    means <- format(estimated, model, ci = ci, ...)
  }

  # restore attributes later
  info <- attributes(estimated)

  # Table formatting
  attr(means, "table_title") <- c(switch(estimate,
    specific = "Model-based Predictions",
    typical = "Estimated Marginal Means",
    average = "Average Predictions",
    population = "Average Counterfactual Predictions"
  ), "blue")

  attr(means, "table_footer") <- .table_footer(
    means,
    type = ifelse(estimate == "specific", "predictions", "means"),
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
