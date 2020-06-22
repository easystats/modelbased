#' Estimate marginal means
#'
#' @inheritParams estimate_means.stanreg
#' @param ci Confidence Interval (CI) level. Default to 0.95 (95\%).
#'
#' @examples
#' library(modelbased)
#'
#' model <- lm(Petal.Length ~ Sepal.Width * Species, data = iris)
#'
#' estimate_means(model)
#' estimate_means(model, fixed = "Sepal.Width")
#' estimate_means(model, levels = c("Species", "Sepal.Width"), length = 2)
#' estimate_means(model, levels = "Species=c('versicolor', 'setosa')")
#' estimate_means(model, levels = "Sepal.Width=c(2, 4)")
#' estimate_means(model, levels = c("Species", "Sepal.Width=0"))
#' estimate_means(model, modulate = "Sepal.Width", length = 5)
#' estimate_means(model, modulate = "Sepal.Width=c(2, 4)")
#' \donttest{
#' if (require("lme4")) {
#'   data <- iris
#'   data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#'   model <- lmer(Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor), data = data)
#'   estimate_means(model)
#'   estimate_means(model, modulate = "Sepal.Width", length = 3)
#' }
#' }
#' @return A data frame of estimated marginal means.
#' @importFrom stats confint
#' @export
estimate_means.lm <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, ci = 0.95, ...) {
  args <- .guess_arguments(model, levels = levels, fixed = fixed, modulate = modulate)
  estimated <- .emmeans_wrapper(model, levels = args$levels, fixed = args$fixed, modulate = args$modulate, transform = transform, length = length, ...)

  # Clean and rename
  means <- as.data.frame(stats::confint(estimated, level = ci))
  if ("df" %in% names(means)) means$df <- NULL
  means <- .clean_names_frequentist(means)

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


#' @export
estimate_means.glmmTMB <- estimate_means.lm
