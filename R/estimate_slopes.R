#' Estimate Marginal Effects
#'
#' Estimate the slopes (i.e., the coefficient) of a predictor over different factor levels. See also other
#' related functions such as \code{\link{estimate_contrasts}} and \code{\link{estimate_means}}.
#'
#' @inheritParams model_emtrends
#'
#' @examples
#' model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#' slopes <- estimate_slopes(model, trend = "Petal.Length")
#' slopes
#' effectsize::standardize(slopes)
#' \dontrun{
#' if (require("rstanarm")) {
#'   model <- stan_glm(Sepal.Width ~ Species * Petal.Length, data = iris, refresh = 0)
#'   estimate_slopes(model)
#' }
#' }
#'
#' @return A data.frame.
#' @export
estimate_slopes <- function(model,
                            trend = NULL,
                            levels = NULL,
                            ci = 0.95,
                            ...) {

  # Sanitize arguments
  args <- .guess_emtrends_arguments(model, trend = trend, levels = levels)

  estimated <- model_emtrends(model, trend = args$trend, levels = args$levels, ci = ci, ...)

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    trends <- bayestestR::describe_posterior(estimated, ci = ci, ...)
    trends <- cbind(estimated@grid, trends)
    trends$`.wgt.` <- NULL # Drop the weight column
    trends <- .clean_names_bayesian(trends, model, transform = "none", type = "trend")
    trends <- insight::data_relocate(trends, c("CI_low", "CI_high"), after = "Coefficient")
  } else {
    trends <- parameters::parameters(estimated, ci = ci, ...)
  }

  # Restore factor levels
  trends <- insight::data_restoretype(trends, insight::get_data(model))

  # Table formatting
  attr(trends, "table_title") <- c("Estimated Marginal Effects", "blue")
  attr(trends, "table_footer") <- c(paste("Marginal effects estimated for", args$trend), "blue")

  # Add attributes
  attr(trends, "model") <- model
  attr(trends, "response") <- insight::find_response(model)
  attr(trends, "ci") <- ci
  attr(trends, "levels") <- args$levels
  attr(trends, "trend") <- args$trend


  # Output
  class(trends) <- c("estimate_slopes", class(trends))
  trends
}




# DERIVATIVES -------------------------------------------------------------


# library(ggplot2)
#
#
# model <- lm(Petal.Length ~ poly(Sepal.Width, 4), data = iris)
# model <- mgcv::gam(Petal.Length ~ s(Sepal.Width), data = iris)
#
# x <- modelbased::estimate_relation(model, length = 20)
# plot(x)
#
# newdata <- as.data.frame(x[attributes(x)$target])
#
# # gratia
# fd <- gratia::fderiv(model, newdata = newdata)
# fd <- cbind(confint(fd, type = "confidence"), x = as.vector(fd[['eval']]))
# fd <- cbind(confint(fd, type = "simultaneous"), x = as.vector(fd[['eval']]))
#
#
# ggplot(fd, aes(x = Sepal.Width, y = est, group = term)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
#   geom_line()  +
#   geom_hline(yintercept = 0, linetype = "dashed")







