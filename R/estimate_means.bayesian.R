#' Estimate average value of response variable at each factor levels
#'
#' @description See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_means.lm]{Frequentist models}}
#'  \item{\link[=estimate_means.stanreg]{Bayesian models}}
#' }
#'
#' @inheritParams estimate_contrasts
#'
#' @return A dataframe of estimated marginal means.
#' @export
estimate_means <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, ...) {
  UseMethod("estimate_means")
}












#' Estimate marginal means
#'
#' @inheritParams estimate_contrasts.stanreg
#'
#' @examples
#' library(modelbased)
#' \donttest{
#'
#' data <- mtcars
#' data$cyl <- as.factor(data$cyl)
#' data$am <- as.factor(data$am)
#'
#' if (require("rstanarm")) {
#'   model <- stan_glm(mpg ~ cyl * am, data = data, refresh=0)
#'   estimate_means(model)
#'
#'   model <- stan_glm(mpg ~ cyl * wt, data = data, refresh=0)
#'   estimate_means(model)
#'   estimate_means(model, modulate = "wt")
#'   estimate_means(model, fixed = "wt")
#' }
#'
#' if (require("brms")) {
#'   model <- brm(mpg ~ cyl * am, data = data, refresh=0)
#'   estimate_means(model)
#' }
#' }
#' @return A dataframe of estimated marginal means.
#'
#' @importFrom emmeans as.mcmc.emmGrid
#' @importFrom insight find_response
#' @importFrom stats mad median sd setNames
#' @export
estimate_means.stanreg <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, centrality = "median", ci = 0.95, ci_method = "hdi", ...) {
  args <- .guess_arguments(model, levels = levels, fixed = fixed, modulate = modulate)
  estimated <- .emmeans_wrapper(model, levels = args$levels, fixed = args$fixed, modulate = args$modulate, transform = transform, length = length, ...)

  # Summary
  means <- .summarize_posteriors(estimated, ci = ci, centrality = centrality, ci_method = ci_method, test = NULL, rope_range = NULL)
  means <- .clean_names_bayesian(means, model, transform, type = "mean")

  # Format means
  temp <- as.data.frame(estimated)
  temp <- temp[1:(ncol(temp)-3)]
  means <- cbind(temp, means)
  means$Parameter <- NULL

  # Restore factor levels
  means <- .restore_factor_levels(means, insight::get_data(model))

  # Restore type
  means[c(args$fixed, args$modulate)] <- sapply(means[c(args$fixed, args$modulate)], as.numeric_ifnumeric)

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


#' @export
estimate_means.brmsfit <- estimate_means.stanreg
