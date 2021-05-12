#' Estimate average value of response variable at each factor levels
#'
#' @inheritParams estimate_contrasts
#' @param centrality,ci,ci_method Arguments for Bayesian models.
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
                           length = 10,
                           centrality = "median",
                           ci = 0.95,
                           ci_method = "hdi",
                           ...) {
  args <- .guess_arguments(model, levels = levels, fixed = fixed, modulate = modulate)

  estimated <- .emmeans_wrapper(
    model,
    levels = args$levels,
    fixed = args$fixed,
    modulate = args$modulate,
    transform = transform,
    length = length,
    ...
  )

  # Summarize and clean
  if (insight::model_info(model)$is_bayesian) {
    means <- .summarize_posteriors(
      estimated,
      ci = ci,
      centrality = centrality,
      ci_method = ci_method,
      test = NULL,
      rope_range = NULL
    )

    means <- .clean_names_bayesian(means, model, transform, type = "mean")

    temp <- as.data.frame(estimated)
    temp <- temp[1:(ncol(temp) - 3)]
    means <- cbind(temp, means)
    means$Parameter <- NULL
  } else {
    means <- as.data.frame(stats::confint(estimated, level = ci))
    if ("df" %in% names(means)) means$df <- NULL
    means <- .clean_names_frequentist(means)
  }

  # Restore factor levels
  means <- insight::data_restoretype(means, insight::get_data(model))

  # Add attributes
  attributes(means) <- c(
    attributes(means),
    list(
      ci = ci,
      ci_method = ci_method,
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












#' Estimate marginal means
#'
#' @inheritParams estimate_contrasts.stanreg
#'
#' @examples
#' library(modelbased)
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
#' @return A dataframe of estimated marginal means.
#'
#' @importFrom emmeans as.mcmc.emmGrid
#' @importFrom insight find_response get_data
#' @importFrom stats mad median sd setNames
#' @export
# estimate_means.stanreg <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, centrality = "median", ci = 0.95, ci_method = "hdi", ...) {
#   args <- .guess_arguments(model, levels = levels, fixed = fixed, modulate = modulate)
#   estimated <- .emmeans_wrapper(model, levels = args$levels, fixed = args$fixed, modulate = args$modulate, transform = transform, length = length, ...)
#
#   # Summary
#   means <- .summarize_posteriors(estimated, ci = ci, centrality = centrality, ci_method = ci_method, test = NULL, rope_range = NULL)
#   means <- .clean_names_bayesian(means, model, transform, type = "mean")
#
#   # Format means
#   temp <- as.data.frame(estimated)
#   temp <- temp[1:(ncol(temp) - 3)]
#   means <- cbind(temp, means)
#   means$Parameter <- NULL
#
#   # Restore factor levels
#   means <- .restore_factor_levels(means, insight::get_data(model))
#
#   # Restore type
#   means[c(args$fixed, args$modulate)] <- sapply(means[c(args$fixed, args$modulate)], insight::as.numeric_ifnumeric)
#
#   # Add attributes
#   attributes(means) <- c(
#     attributes(means),
#     list(
#       ci = ci,
#       ci_method = ci_method,
#       levels = args$levels,
#       fixed = args$fixed,
#       modulate = args$modulate,
#       transform = transform,
#       response = insight::find_response(model)
#     )
#   )
#
#   class(means) <- c("estimate_means", class(means))
#   means
# }


#' @export
# estimate_means.brmsfit <- estimate_means.stanreg









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
#'
#' @return A data frame of estimated marginal means.
#' @importFrom stats confint
#' @export
# estimate_means.lm <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, ci = 0.95, ...) {
#   args <- .guess_arguments(model, levels = levels, fixed = fixed, modulate = modulate)
#   estimated <- .emmeans_wrapper(model, levels = args$levels, fixed = args$fixed, modulate = args$modulate, transform = transform, length = length, ...)
#
#   # Clean and rename
#   means <- as.data.frame(stats::confint(estimated, level = ci))
#   if ("df" %in% names(means)) means$df <- NULL
#   means <- .clean_names_frequentist(means)
#
#   # Restore factor levels
#   means <- .restore_factor_levels(means, insight::get_data(model))
#
#   # Add attributes
#   attributes(means) <- c(
#     attributes(means),
#     list(
#       ci = ci,
#       levels = args$levels,
#       fixed = args$fixed,
#       modulate = args$modulate,
#       transform = transform,
#       response = insight::find_response(model)
#     )
#   )
#
#   class(means) <- c("estimate_means", class(means))
#   means
# }





#' @export
# estimate_means.merMod <- estimate_means.lm


#' @export
# estimate_means.glmmTMB <- estimate_means.lm
