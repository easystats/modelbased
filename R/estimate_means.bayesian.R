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
#' @export
estimate_means <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, ...) {
  UseMethod("estimate_means")
}












#' Estimate marginal means
#'
#' @inheritParams estimate_contrasts.stanreg
#'
#' @examples
#' library(estimate)
#' \dontrun{
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length_factor, data = data)
#' estimate_means(model)
#'
#' model <- stan_glm(Petal.Length ~ Sepal.Width * Species, data = iris)
#' estimate_means(model)
#' estimate_means(model, modulate = "Sepal.Width")
#' estimate_means(model, fixed = "Sepal.Width")
#' }
#' @importFrom emmeans as.mcmc.emmGrid
#' @importFrom insight find_response
#' @importFrom stats mad median sd setNames
#' @export
estimate_means.stanreg <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "response", length = 10, centrality = "median", ci = 0.89, ci_method = "hdi", ...) {
  estimated <- .emmeans_wrapper(model, levels = levels, fixed = fixed, modulate = modulate, transform, length = length, type = "mean", ...)
  posteriors <- emmeans::as.mcmc.emmGrid(estimated$means)
  posteriors <- as.data.frame(as.matrix(posteriors))

  # Summary
  means <- .summarize_posteriors(posteriors, ci = ci, centrality = centrality, ci_method = ci_method, test = NULL, rope_range = NULL)

  # Format means
  levelcols <- strsplit(as.character(means$Parameter), ", ")
  levelcols <- data.frame(do.call(rbind, levelcols))
  names(levelcols) <- unlist(sapply(levelcols, .find_name_level))
  if (nrow(levelcols) > 1) {
    levelcols <- as.data.frame(sapply(levelcols, .remove_name_level), stringsAsFactors = FALSE)
    levelcols <- as.data.frame(sapply(levelcols, as.numeric_ifnumeric), stringsAsFactors = FALSE)
  } else {
    levelcols <- as.data.frame(t(sapply(levelcols, .remove_name_level)), stringsAsFactors = FALSE)
    levelcols <- as.data.frame(t(sapply(levelcols, as.numeric_ifnumeric)), stringsAsFactors = FALSE)
  }
  means$Parameter <- NULL
  means <- cbind(levelcols, means)


  # Restore factor levels
  means <- .restore_factor_levels(means, insight::get_data(model))

  # Restore type
  means[c(fixed, modulate)] <- sapply(means[c(fixed, modulate)], as.numeric_ifnumeric)

  # Add attributes
  attributes(means) <- c(
    attributes(means),
    list(
      ci = ci,
      ci_method = ci_method,
      levels = estimated$levels,
      fixed = estimated$fixed,
      modulate = estimated$modulate,
      transform = transform,
      response = insight::find_response(model)
    )
  )

  class(means) <- c("estimate_means", class(means))
  means
}
