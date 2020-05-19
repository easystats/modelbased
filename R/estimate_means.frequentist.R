#' Estimate marginal means
#'
#' @inheritParams estimate_means.stanreg
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#'
#' @examples
#' library(modelbased)
#'
#' model <- lm(Petal.Length ~ Sepal.Width + Species, data = iris)
#' estimate_means(model)
#' estimate_means(model, modulate = "Sepal.Width")
#' \donttest{
#' if (require("lme4")) {
#'   data <- iris
#'   data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#'   model <- lmer(Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor), data = data)
#'   estimate_means(model)
#'   estimate_means(model, modulate = "Sepal.Width")
#' }
#' }
#' @return A data frame of estimated marginal means.
#' @importFrom stats confint
#' @export
estimate_means.lm <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, ci = 0.95, ...) {

  args <- .guess_arguments(model, levels = levels, fixed = fixed, modulate = modulate)

  estimated <- .emmeans_wrapper(model, levels = args$levels, fixed = args$fixed, modulate = args$modulate, transform, length = length, type = "mean", ...)

  # Clean and rename
  means <- as.data.frame(stats::confint(estimated, level = ci))
  if ("df" %in% names(means)) means$df <- NULL
  means <- .clean_emmeans_frequentist(means)

  # Restore factor levels
  means <- .restore_factor_levels(means, insight::get_data(model))

  # Add attributes
  attributes(means) <- c(
    attributes(means),
    list(
      ci = ci,
      levels = args$levels,
      fixed = args$fixed,
      modulate = args$modulate,
      transform = transform,
      response = insight::find_response(model)
    )
  )

  class(means) <- c("estimate_means", class(means))
  means
}





#' @export
estimate_means.merMod <- estimate_means.lm
