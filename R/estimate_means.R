#' Estimate average value of response at each factor levels
#'
#' See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_means.stanreg]{Bayesian models (stanreg and brms)}}
#'  }
#'
#' @param model Object.
#' @param ... Arguments passed to or from other methods.
#'
#' @export
estimate_means <- function(model, ...) {
  UseMethod("estimate_means")
}














#' Estimate marginal means
#'
#'
#' @inheritParams estimate_contrasts.stanreg
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * fac2,
#'   data = mutate(iris, fac2 = ifelse(Petal.Length < 4.2, "A", "B"))
#' )
#' estimate_means(model)
#' model <- stan_glm(binary ~ Species,
#'   data = mutate(iris, binary = ifelse(Petal.Length < 4.2, 0, 1)), family = "binomial"
#' )
#' estimate_means(model)
#' }
#' @import emmeans
#' @importFrom graphics pairs
#' @importFrom stats mad median sd setNames
#' @export
estimate_means.stanreg <- function(model, levels = NULL, transform = "response", ci = .90, estimate = "median", ...) {
  if (is.null(levels)) {
    levels <- insight::find_predictors(model)$conditional
  }


  # Posteriors
  posteriors <- emmeans::emmeans(model, levels, transform = transform, ...)
  posteriors <- emmeans::as.mcmc.emmGrid(posteriors)
  posteriors <- as.data.frame(as.matrix(posteriors))

  # Summary
  means <- parameters::summarise_posteriors(posteriors, ci = ci, estimate = estimate, test = NULL, rope_range = NULL, rope_full = NULL)


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

  attributes(means) <- c(attributes(means),
                               list(ci = ci, levels = levels, transform = transform))

  class(means) <- c("estimateMeans", class(means))
  return(means)
}
