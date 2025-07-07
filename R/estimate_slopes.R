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
#' compute the slopes. To get marginal effects at specific values, use
#' `trend="<variable>"` along with the `by` argument, e.g.
#' `by="<variable>=c(1, 3, 5)"`, or a combination of `by` and `length`, for
#' instance, `by="<variable>", length=30`. To calculate average marginal
#' effects over a range of values, use `trend="<variable>=seq(1, 3, 0.1)"` (or
#' similar) and omit the variable provided in `trend` from the `by` argument.
#' @param p_adjust The p-values adjustment method for frequentist multiple
#' comparisons. For `estimate_slopes()`, multiple comparison only occurs for
#' Johnson-Neyman intervals, i.e. in case of interactions with two numeric
#' predictors (one specified in `trend`, one in `by`). In this case, the
#' `"esarey"` option is recommended, but `p_adjust` can also be one of `"none"`
#' (default), `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`,
#' `"tukey"`, `"sidak"`,`"sup-t"`, or `"holm"`. `"sup-t"` computes simultaneous
#' confidence bands, also called sup-t confidence band (Montiel Olea &
#' Plagborg-Møller, 2019).
#' @inheritParams estimate_means
#' @inheritParams parameters::model_parameters.default
#'
#' @inherit estimate_means details
#'
#' @inheritSection estimate_means Predictions and contrasts at meaningful values (data grids)
#'
#' @return A data.frame of class `estimate_slopes`.
#'
#' @references
#' Montiel Olea, J. L., and Plagborg-Møller, M. (2019). Simultaneous
#' confidence bands: Theory, implementation, and an application to SVARs.
#' Journal of Applied Econometrics, 34(1), 1–17. \doi{10.1002/jae.2656}
#'
#' @examplesIf all(insight::check_if_installed(c("marginaleffects", "effectsize", "mgcv", "ggplot2", "see"), quietly = TRUE))
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
#' \dontrun{
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
#'
#' # marginal effects, grouped by Species, at different values of Petal.Length
#' estimate_slopes(model,
#'   trend = "Petal.Length",
#'   by = c("Petal.Length", "Species"), length = 10
#' )
#'
#' # marginal effects at different values of Petal.Length
#' estimate_slopes(model, trend = "Petal.Length", by = "Petal.Length", length = 10)
#'
#' # marginal effects at very specific values of Petal.Length
#' estimate_slopes(model, trend = "Petal.Length", by = "Petal.Length=c(1, 3, 5)")
#'
#' # average marginal effects of Petal.Length,
#' # just for the trend within a certain range
#' estimate_slopes(model, trend = "Petal.Length=seq(2, 4, 0.01)")
#' }
#' @export
estimate_slopes <- function(model,
                            trend = NULL,
                            by = NULL,
                            predict = NULL,
                            ci = 0.95,
                            p_adjust = "none",
                            transform = NULL,
                            keep_iterations = FALSE,
                            backend = NULL,
                            verbose = TRUE,
                            ...) {
  # Process argument ---------------------------------------------------------
  if (is.null(backend)) {
    backend <- getOption("modelbased_backend", "marginaleffects")
  }

  if (backend == "emmeans") {
    # Emmeans ----------------------------------------------------------------
    estimated <- get_emtrends(
      model,
      trend = trend,
      predict = predict,
      by = by,
      keep_iterations = keep_iterations,
      verbose = verbose,
      ...
    )
    trends <- .format_emmeans_slopes(model, estimated, ci, ...)
  } else {
    # Marginaleffects --------------------------------------------------------
    estimated <- get_marginaltrends(
      model,
      trend = trend,
      by = by,
      predict = predict,
      ci = ci,
      p_adjust = p_adjust,
      transform = transform,
      keep_iterations = keep_iterations,
      verbose = verbose,
      ...
    )
    trends <- format(estimated, model, ci, ...)
  }

  # restore attributes later
  info <- attributes(estimated)

  # Table formatting
  table_footer <- .table_footer_slopes(trends, model = model, info = info)
  attr(trends, "table_title") <- c("Estimated Marginal Effects", "blue")
  attr(trends, "table_footer") <- c(table_footer, "yellow")

  # Add attributes
  attr(trends, "model") <- model
  attr(trends, "response") <- insight::find_response(model)
  attr(trends, "ci") <- ci

  # add attributes from workhorse function
  attributes(trends) <- utils::modifyList(attributes(trends), info[.info_elements()])

  # Output
  class(trends) <- c("estimate_slopes_summary", "estimate_slopes", class(trends))
  trends
}
