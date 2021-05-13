#' Estimate average value of response variable at each factor levels
#'
#' @inheritParams model_emmeans
#' @param ci Uncertainty Interval (CI) level. Default to 95\% (\code{0.95}).
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
#' \dontrun{
#' if (require("lme4")) {
#'   data <- iris
#'   data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#'   model <- lmer(Petal.Length ~ Sepal.Width + Species + (1 | Petal.Length_factor), data = data)
#'   estimate_means(model)
#'   estimate_means(model, modulate = "Sepal.Width", length = 3)
#' }
#' }
#' \donttest{
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$am <- as.factor(data$am)
#'
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ cyl * am, data = data, refresh = 0)
#'   estimate_means(model)
#'
#'   model <- stan_glm(mpg ~ cyl * wt, data = data, refresh = 0)
#'   estimate_means(model)
#'   estimate_means(model, modulate = "wt")
#'   estimate_means(model, fixed = "wt")
#' }
#' }
#'
#' \dontrun{
#' if (require("brms")) {
#'   model <- brm(mpg ~ cyl * am, data = data, refresh = 0)
#'   estimate_means(model)
#' }
#' }
#'
#' @return A dataframe of estimated marginal means.
#' @export
estimate_means <- function(model,
                           levels = NULL,
                           fixed = NULL,
                           modulate = NULL,
                           transform = "response",
                           ci = 0.95,
                           ...) {

  # Sanitize arguments
  args <- .guess_arguments(model, levels = levels, fixed = fixed, modulate = modulate)

  # Run emmeans
  estimated <- model_emmeans(
    model,
    levels = args$levels,
    fixed = args$fixed,
    modulate = args$modulate,
    transform = transform,
    ...
  )

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {

    means <- bayestestR::describe_posterior(estimated, test = NULL, rope_range = NULL, ci = ci, ...)
    means <- cbind(.get_variables_emmeans(estimated), means)
    means <- .clean_names_bayesian(means, model, transform, type = "mean")

  } else {
    means <- as.data.frame(stats::confint(estimated, level = ci))
    means <- .clean_names_frequentist(means)
  }

  # Restore factor levels
  means <- insight::data_restoretype(means, insight::get_data(model))

  # Add attributes
  attributes(means) <- c(
    attributes(means),
    list(
      levels = args$levels,
      fixed = args$fixed,
      modulate = args$modulate,
      transform = transform,
      response = insight::find_response(model),
      ci = ci
    )
  )

  class(means) <- c("estimate_means", class(means))
  means
}
