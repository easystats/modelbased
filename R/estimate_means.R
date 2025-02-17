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
#' @param estimate Character string, indicating the type of target population
#' predictions refer to. This dictates how the predictions are "averaged" over
#' the non-focal predictors, i.e. those variables that are not specified in
#' `by` or `contrast`. We can roughly distinguish between "modelbased" and
#' "empirical" predictions.
#' - `"typical"` (default): Predictions are made for observations that are
#'   represented by a data grid, which is built from all combinations of the
#'   predictor levels in `by` (the focal predictors). `"typical"` then takes the
#'   mean value for non-focal numeric predictors and marginalizes over the
#'   factor levels of non-focal predictors, which computes a kind of "weighted
#'   average" for the values at which these terms are hold constant. These
#'   predictions are useful for comparing defined "groups" and are still a good
#'   representation of the sample, because all possible values and levels of the
#'   non-focal predictors are considered (averaged over). It answers the
#'   question, "What would be the average outcome for a 'typical' observation?",
#'   where 'typical' refers to subjects represented by (i.e., that share the
#'   characteristics from) the data grid. This approach is the one taken by
#'   default in the `emmeans` package.
#' - `"average"`: Predictions are made for each observation in the sample. Then,
#'   the average of all predictions is calculated within all groups (or levels)
#'   of the focal predictors defined in `by`. These predictions are the closest
#'   representation of the sample, because `estimate = "average"` averages
#'   across the full sample, where groups (in `by`) are not represented by a
#'   balanced data grid, but rather the empirical distributions of the
#'   characteristics of the sample. It answers the question, "What is the
#'   predicted value for an average observation (from a certain group in `by`)
#'   in my data?".
#' - `"population"`: Each observation is "cloned" multiple times, where each
#'   duplicate gets one of the levels from the focal predictors in `by`. We then
#'   have one "original" and several copies of that original, each varying in
#'   the levels of the focal predictors. Hence, the sample is replicated
#'   multiple times to produce "counterfactuals" and then takes the average of
#'   these predicted values (aggregated/grouped by the focal predictors). It can
#'   be considered as extrapolation to a hypothetical target population.
#'   Counterfactual predictions are useful, insofar as the results can also be
#'   transferred to other contexts (Dickerman and Hernan, 2020). It answers the
#'   question, "What is the predicted response value for the 'average'
#'   observation in *the broader target population*?". It does not only refer to
#'   the actual data in your observed sample, but also "what would be if" we had
#'   more data, or if we had data from a different sample.
#'
#' In other words, the distinction between estimate types resides in whether
#' the prediction are made for:
#' - *modelbased predictions* (focus lies on _predictors_), which are useful to
#'   look at differences between typical groups, or for visualization
#'   - A specific individual from the sample (i.e., a specific combination of
#'     predictor values for focal and non-focal predictors): this is what is obtained
#'     when using [`estimate_relation()`] and the other prediction functions.
#'   - A typical individual from the sample: obtained with
#'     `estimate_means(..., estimate = "typical")`
#' - *empirical predictions* (focus lies on _predictions_ of the outcome), which
#'   are useful if you want realistic predictions of your outcome, assuming that
#'   the sample is representative for a special population (option `"average"`),
#'   or useful for "what-if" scenarios, especially if you want to make unbiased
#'   comparisons (G-computation, option `"population"`)
#'   - The average individual from the sample: obtained with
#'     `estimate_means(..., estimate = "average")`
#'   - The broader, hypothetical target population: obtained with
#'     `estimate_means(..., estimate = "population")`
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
#' appropriate transformation-function.
#' @param verbose Use `FALSE` to silence messages and warnings.
#' @param ... Other arguments passed, for instance, to [insight::get_datagrid()],
#' to functions from the **emmeans** or **marginaleffects** package, or to process
#' Bayesian models via [bayestestR::describe_posterior()]. Examples:
#' - `insight::get_datagrid()`: Argument such as `length`, `digits` or `range`
#'   can be used to control the (number of) representative values.
#' - **marginaleffects**: Internally used functions are `avg_predictions()` for
#'   means and contrasts, and `avg_slope()` for slopes. Therefore, arguments for
#'   instance like `vcov`, `equivalence`, `df`, `slope` or even `newdata` can be
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
    means <- format(estimated, model, ...)
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
