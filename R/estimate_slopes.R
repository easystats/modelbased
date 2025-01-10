#' Estimate Marginal Effects
#'
#' @description
#' Estimate the slopes (i.e., the coefficient) of a predictor over or within
#' different factor levels, or alongside a numeric variable. In other words, to
#' assess the effect of a predictor *at* specific configurations data. It corresponds
#' to the derivative and can be useful to understand where a predictor has a
#' significant role when interactions or non-linear relationships are present.
#'
#' Other related functions based on marginal estimations includes
#' [estimate_contrasts()] and [estimate_means()].
#'
#' See the **Details** section below, and don't forget to also check out the
#' [Vignettes](https://easystats.github.io/modelbased/articles/estimate_slopes.html)
#' and [README examples](https://easystats.github.io/modelbased/index.html#features) for
#' various examples, tutorials and use cases.
#'
#' @param trend A character indicating the name of the variable for which to
#' compute the slopes.
#' @inheritParams estimate_means
#' @inheritParams parameters::model_parameters.default
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
#' grid()`][visualisation_matrix] is so important for direct predictions.
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
#' condition (`using [estimate_slopes]`).
#'
#' @return A data.frame of class `estimate_slopes`.
#'
#' @examplesIf all(insight::check_if_installed(c("emmeans", "mgcv", "ggplot2", "see"), quietly = TRUE))
#' library(ggplot2)
#' # Get an idea of the data
#' ggplot(iris, aes(x = Petal.Length, y = Sepal.Width)) +
#'   geom_point(aes(color = Species)) +
#'   geom_smooth(color = "black", se = FALSE) +
#'   geom_smooth(aes(color = Species), linetype = "dotted", se = FALSE) +
#'   geom_smooth(aes(color = Species), method = "lm", se = FALSE)
#'
#' # Model it
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' # Compute the marginal effect of Petal.Length at each level of Species
#' slopes <- estimate_slopes(model, trend = "Petal.Length", by = "Species")
#' slopes
#'
#' # Plot it
#' plot(slopes)
#' standardize(slopes)
#'
#' model <- mgcv::gam(Sepal.Width ~ s(Petal.Length), data = iris)
#' slopes <- estimate_slopes(model, by = "Petal.Length", length = 50)
#' summary(slopes)
#' plot(slopes)
#'
#' model <- mgcv::gam(Sepal.Width ~ s(Petal.Length, by = Species), data = iris)
#' slopes <- estimate_slopes(model,
#'   trend = "Petal.Length",
#'   by = c("Petal.Length", "Species"), length = 20
#' )
#' summary(slopes)
#' plot(slopes)
#' @export
estimate_slopes <- function(model,
                            trend = NULL,
                            by = NULL,
                            ci = 0.95,
                            backend = getOption("modelbased_backend", "emmeans"),
                            verbose = TRUE,
                            ...) {
  if (backend == "emmeans") {
    # Emmeans ------------------------------------------------------------------
    estimated <- get_emtrends(model, trend = trend, by = by, verbose = verbose, ...)
    trends <- .format_emmeans_slopes(model, estimated, ci, ...)
  } else {
    estimated <- get_marginaltrends(model, trend = trend, by = by, verbose = verbose, ...)
    trends <- format(estimated, model, ci, ...)
  }

  # restore attributes later
  info <- attributes(estimated)

  # Table formatting
  attr(trends, "table_title") <- c("Estimated Marginal Effects", "blue")
  attr(trends, "table_footer") <- c(paste("Marginal effects estimated for", info$trend), "blue")

  # Add attributes
  attr(trends, "model") <- model
  attr(trends, "response") <- insight::find_response(model)
  attr(trends, "ci") <- ci

  # add attributes from workhorse function
  attributes(trends) <- utils::modifyList(
    attributes(trends),
    info[c("at", "by", "datagrid", "focal_terms", "trend", "coef_name")]
  )

  # Output
  class(trends) <- c("estimate_slopes_summary", "estimate_slopes", class(trends))
  trends
}
