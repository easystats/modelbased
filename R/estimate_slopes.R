#' Estimate Marginal Effects
#'
#' Estimate the slopes (i.e., the coefficient) of a predictor over different factor levels. See also other
#' related functions such as \code{\link{estimate_contrasts}} and \code{\link{estimate_means}}.
#'
#' @inheritParams model_emtrends
#' @inheritParams estimate_means
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
                            modulate = NULL,
                            ci = 0.95,
                            ...) {

  # Sanitize arguments
  estimated <- model_emtrends(model, trend, levels, modulate, ...)
  args <- attributes(estimated)$args

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
  attr(trends, "modulate") <- args$modulate


  # Output
  class(trends) <- c("estimate_slopes", class(trends))
  trends
}


# Add summary method that summarized by direction
