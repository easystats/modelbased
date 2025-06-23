#' Estimate Marginal Means (Model-based average at each factor level)
#'
#' Estimate average values of the response variable at each factor level or
#' at representative values, respectively at values defined in a "data grid" or
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
#' focal predictors are evaluated (e.g., `by = "x = c(1, 2)"`). When `estimate`
#' is *not* `"average"`, the `by` argument is used to create a "reference grid"
#' or "data grid" with representative values for the focal predictors. In this
#' case, `by` can also be list of named elements. See details in
#' [`insight::get_datagrid()`] to learn more about how to create data grids for
#' predictors of interest.
#' @param predict Is passed to the `type` argument in `emmeans::emmeans()` (when
#' `backend = "emmeans"`) or in `marginaleffects::avg_predictions()` (when
#' `backend = "marginaleffects"`). Valid options for `predict` are:
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
#'   transformation is selected (which usually is `"response"`). See also
#'   [this vignette](https://CRAN.R-project.org/package=emmeans/vignettes/transformations.html).
#'
#' See also section _Predictions on different scales_.
#'
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
#' @param backend Whether to use `"marginaleffects"` (default) or `"emmeans"` as
#' a backend. Results are usually very similar. The major difference will be
#' found for mixed models, where `backend = "marginaleffects"` will also average
#' across random effects levels, producing "marginal predictions" (instead of
#' "conditional predictions", see Heiss 2022).
#'
#' Another difference is that `backend = "marginaleffects"` will be slower than
#' `backend = "emmeans"`. For most models, this difference is negligible. However,
#' in particular complex models or large data sets fitted with *glmmTMB* can be
#' significantly slower.
#'
#' You can set a default backend via `options()`, e.g. use
#' `options(modelbased_backend = "emmeans")` to use the **emmeans** package or
#' `options(modelbased_backend = "marginaleffects")` to set **marginaleffects** as
#' default backend.
#' @param transform A function applied to predictions and confidence intervals
#' to (back-) transform results, which can be useful in case the regression
#' model has a transformed response variable (e.g., `lm(log(y) ~ x)`). For
#' Bayesian models, this function is applied to individual draws from the
#' posterior distribution, before computing summaries. Can also be `TRUE`, in
#' which case `insight::get_transformation()` is called to determine the
#' appropriate transformation-function. Note that no standard errors are returned
#' when transformations are applied.
#' @param keep_iterations If `TRUE`, will keep all iterations (draws) of
#' bootstrapped or Bayesian models. They will be added as additional columns
#' named `iter_1`, `iter_2`, and so on. If `keep_iterations` is a positive
#' number, only as many columns as indicated in `keep_iterations` will be added
#' to the output. You can reshape them to a long format by running
#' [`bayestestR::reshape_iterations()`].
#' @param verbose Use `FALSE` to silence messages and warnings.
#' @param ... Other arguments passed, for instance, to [insight::get_datagrid()],
#' to functions from the **emmeans** or **marginaleffects** package, or to process
#' Bayesian models via [bayestestR::describe_posterior()]. Examples:
#' - `insight::get_datagrid()`: Argument such as `length`, `digits` or `range`
#'   can be used to control the (number of) representative values. For integer
#'   variables, `protect_integers` modulates whether these should also be
#'   treated as numerics, i.e. values can have fractions or not.
#' - **marginaleffects**: Internally used functions are `avg_predictions()` for
#'   means and contrasts, and `avg_slope()` for slopes. Therefore, arguments for
#'   instance like `vcov`, `equivalence`, `df`, `slope`, `hypothesis` or even
#'   `newdata` can be passed to those functions. A `weights` argument is passed
#'   to the `wts` argument in `avg_predictions()` or `avg_slopes()`, however,
#'   weights can only be applied when `estimate` is `"average"` or
#'   `"population"` (i.e. for those marginalization options that do not use data
#'   grids). Other arguments, such as `re.form` or `allow.new.levels`, may be
#'   passed to `predict()` (which is internally used by *marginaleffects*) if
#'   supported by that model class.
#' - **emmeans**: Internally used functions are `emmeans()` and `emtrends()`.
#'   Additional arguments can be passed to these functions.
#' - Bayesian models: For Bayesian models, parameters are cleaned using
#'   `describe_posterior()`, thus, arguments like, for example, `centrality`,
#'   `rope_range`, or `test` are passed to that function.
#' - Especially for `estimate_contrasts()` with integer focal predictors, for
#'   which contrasts should be calculated, use argument `integer_as_numeric` to
#'   set the maximum number of unique values in an integer predictor to treat
#'   that predictor as "discrete integer" or as numeric. For the first case,
#'   contrasts are calculated between values of the predictor, for the latter,
#'   contrasts of slopes are calculated. If the integer has more than
#'   `integer_as_numeric` unique values, it is treated as numeric. Defaults to
#'   `5`.
#' - For count regression models that use an offset term, use `offset = <value>`
#'   to fix the offset at a specific value. Or use `estimate = "average"`, to
#'   average predictions over the distribution of the offset (if appropriate).
#'
#' @inheritParams parameters::model_parameters.default
#' @inheritParams estimate_expectation
#'
#' @details
#' The [estimate_slopes()], [estimate_means()] and [estimate_contrasts()]
#' functions are forming a group, as they are all based on *marginal*
#' estimations (estimations based on a model). All three are built on the
#' **emmeans** or **marginaleffects** package (depending on the `backend`
#' argument), so reading its documentation (for instance [emmeans::emmeans()],
#' [emmeans::emtrends()] or this [website](https://marginaleffects.com/)) is
#' recommended to understand the idea behind these types of procedures.
#'
#' - Model-based **predictions** is the basis for all that follows. Indeed,
#' the first thing to understand is how models can be used to make predictions
#' (see [estimate_link()]). This corresponds to the predicted response (or
#' "outcome variable") given specific predictor values of the predictors (i.e.,
#' given a specific data configuration). This is why the concept of [`reference
#' grid()`][insight::get_datagrid()] is so important for direct predictions.
#'
#' - **Marginal "means"**, obtained via [estimate_means()], are an extension
#' of such predictions, allowing to "average" (collapse) some of the predictors,
#' to obtain the average response value at a specific predictors configuration.
#' This is typically used when some of the predictors of interest are factors.
#' Indeed, the parameters of the model will usually give you the intercept value
#' and then the "effect" of each factor level (how different it is from the
#' intercept). Marginal means can be used to directly give you the mean value of
#' the response variable at all the levels of a factor. Moreover, it can also be
#' used to control, or average over predictors, which is useful in the case of
#' multiple predictors with or without interactions.
#'
#' - **Marginal contrasts**, obtained via [estimate_contrasts()], are
#' themselves at extension of marginal means, in that they allow to investigate
#' the difference (i.e., the contrast) between the marginal means. This is,
#' again, often used to get all pairwise differences between all levels of a
#' factor. It works also for continuous predictors, for instance one could also
#' be interested in whether the difference at two extremes of a continuous
#' predictor is significant.
#'
#' - Finally, **marginal effects**, obtained via [estimate_slopes()], are
#' different in that their focus is not values on the response variable, but the
#' model's parameters. The idea is to assess the effect of a predictor at a
#' specific configuration of the other predictors. This is relevant in the case
#' of interactions or non-linear relationships, when the effect of a predictor
#' variable changes depending on the other predictors. Moreover, these effects
#' can also be "averaged" over other predictors, to get for instance the
#' "general trend" of a predictor over different factor levels.
#'
#' **Example:** Let's imagine the following model `lm(y ~ condition * x)` where
#' `condition` is a factor with 3 levels A, B and C and `x` a continuous
#' variable (like age for example). One idea is to see how this model performs,
#' and compare the actual response y to the one predicted by the model (using
#' [estimate_expectation()]). Another idea is evaluate the average mean at each of
#' the condition's levels (using [estimate_means()]), which can be useful to
#' visualize them. Another possibility is to evaluate the difference between
#' these levels (using [estimate_contrasts()]). Finally, one could also estimate
#' the effect of x averaged over all conditions, or instead within each
#' condition (using [estimate_slopes()]).
#'
#' @section Predictions and contrasts at meaningful values (data grids):
#'
#' To define representative values for focal predictors (specified in `by`,
#' `contrast`, and `trend`), you can use several methods. These values are
#' internally generated by `insight::get_datagrid()`, so consult its
#' documentation for more details.
#'
#' * You can directly specify values as strings or lists for `by`, `contrast`,
#'   and `trend`.
#'   * For numeric focal predictors, use examples like `by = "gear = c(4, 8)"`,
#'     `by = list(gear = c(4, 8))` or `by = "gear = 5:10"`
#'   * For factor or character predictors, use `by = "Species = c('setosa', 'virginica')"`
#'     or `by = list(Species = c('setosa', 'virginica'))`
#' * You can use "shortcuts" within square brackets, such as `by = "Sepal.Width = [sd]"`
#'   or `by = "Sepal.Width = [fivenum]"`
#' * For numeric focal predictors, if no representative values are specified,
#'   `length` and `range` control the number and type of representative values:
#'   * `length` determines how many equally spaced values are generated.
#'   * `range` specifies the type of values, like `"range"` or `"sd"`.
#'   * `length` and `range` apply to all numeric focal predictors.
#'   * If you have multiple numeric predictors, `length` and `range` can accept
#'     multiple elements, one for each predictor.
#' * For integer variables, only values that appear in the data will be included
#'   in the data grid, independent from the `length` argument. This behaviour
#'   can be changed by setting `protect_integers = FALSE`, which will then treat
#'   integer variables as numerics (and possibly produce fractions).
#'
#' See also [this vignette](https://easystats.github.io/modelbased/articles/visualisation_matrix.html)
#' for some examples.
#'
#' @section Predictions on different scales:
#'
#' The `predict` argument allows to generate predictions on different scales of
#' the response variable. The `"link"` option does not apply to all models, and
#' usually not to Gaussian models. `"link"` will leave the values on scale of
#' the linear predictors. `"response"` (or `NULL`) will transform them on scale
#' of the response variable. Thus for a logistic model, `"link"` will give
#' estimations expressed in log-odds (probabilities on logit scale) and
#' `"response"` in terms of probabilities.
#'
#' To predict distributional parameters (called "dpar" in other packages), for
#' instance when using complex formulae in `brms` models, the `predict` argument
#' can take the value of the parameter you want to estimate, for instance
#' `"sigma"`, `"kappa"`, etc.
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
#' `"inverse_link"` approach is more robust in terms of confidence intervals,
#' but might produce biased predictions. However, you can try to set
#' `bias_correction = TRUE`, to adjust for this bias.
#'
#' In particular for mixed models, using `"response"` is recommended, because
#' averaging across random effects groups is then more accurate.
#'
#' @section Finite mixture models:
#'
#' For finite mixture models (currently, only the [`brms::mixture()`] family
#' from package *brms* is supported), use `predict = "link"` to return predicted
#' values stratified by class membership. To predict the class membership, use
#' [`estimate_link()`].
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
#' @return A data frame of estimated marginal means.
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
#' estimate_means(model, by = "Species = c('versicolor', 'setosa')")
#' estimate_means(model, by = c("Species", "Sepal.Width = 0"))
#'
#' # estimate marginal average of response at values for numeric predictor
#' estimate_means(model, by = "Sepal.Width", length = 5)
#' estimate_means(model, by = "Sepal.Width = c(2, 4)")
#'
#' # or provide the definition of the data grid as list
#' estimate_means(
#'   model,
#'   by = list(Sepal.Width = c(2, 4), Species = c("versicolor", "setosa"))
#' )
#'
#' # Methods that can be applied to it:
#' means <- estimate_means(model, by = c("Species", "Sepal.Width = 0"))
#'
#' plot(means) # which runs visualisation_recipe()
#' standardize(means)
#'
#' # grids for numeric predictors, combine range and length
#' model <- lm(Sepal.Length ~ Sepal.Width * Petal.Length, data = iris)
#'
#' # create a "grid": value range for first numeric predictor, mean +/-1 SD
#' # for remaining numeric predictors.
#' estimate_means(model, c("Sepal.Width", "Petal.Length"), range = "grid")
#'
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
                           estimate = NULL,
                           transform = NULL,
                           keep_iterations = FALSE,
                           backend = NULL,
                           verbose = TRUE,
                           ...) {
  # Process argument ---------------------------------------------------------
  # --------------------------------------------------------------------------

  # set defaults
  if (is.null(estimate)) {
    estimate <- getOption("modelbased_estimate", "typical")
  }
  if (is.null(backend)) {
    backend <- getOption("modelbased_backend", "marginaleffects")
  }

  # validate input
  estimate <- insight::validate_argument(
    estimate,
    c("typical", "population", "specific", "average")
  )

  if (backend == "emmeans") {
    # Emmeans ----------------------------------------------------------------
    estimated <- get_emmeans(
      model,
      by = by,
      predict = predict,
      keep_iterations = keep_iterations,
      verbose = verbose,
      ...
    )
    means <- .format_emmeans_means(estimated, model, ci = ci, verbose = verbose, ...)
  } else {
    # Marginalmeans ----------------------------------------------------------
    estimated <- get_marginalmeans(
      model,
      by = by,
      predict = predict,
      ci = ci,
      estimate = estimate,
      transform = transform,
      keep_iterations = keep_iterations,
      verbose = verbose,
      ...
    )
    means <- format(estimated, model, ci = ci, ...)
  }

  # sanity check - did method return standard errors?
  .check_standard_errors(out = means, model = model, verbose = verbose)

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
  attr(means, "coef_name") <- intersect(.valid_coefficient_names(model), colnames(means))

  # add attributes from workhorse function
  attributes(means) <- utils::modifyList(attributes(means), info[.info_elements()])

  # Output
  class(means) <- unique(c("estimate_means", class(means)))
  means
}
