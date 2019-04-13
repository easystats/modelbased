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
#'
#' model <- stan_glm(Petal.Length ~ Sepal.Width + Species, data=iris)
#' estimate_means(model)
#' estimate_means(model, modulate="Sepal.Width")
#' }
#' @import emmeans
#' @importFrom graphics pairs
#' @importFrom stats mad median sd setNames
#' @export
estimate_means.stanreg <- function(model, levels = NULL, fixed=NULL, modulate = NULL, transform = "response", ci = .90, estimate = "median", length = 10, ...) {



  estimated <- .emmeans_wrapper(model, levels = levels, fixed=fixed, modulate = modulate, transform, length=length, type="mean", ...)
  posteriors <- emmeans::as.mcmc.emmGrid(estimated$means)
  posteriors <- as.data.frame(as.matrix(posteriors))




  # Summary
  means <- parameters::describe_posterior(posteriors, ci = ci, estimate = estimate, test = NULL, rope_range = NULL, rope_full = NULL)


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
                               list(ci = ci,
                                    levels = estimated$levels,
                                    fixed = estimated$fixed,
                                    modulate = estimated$modulate,
                                    transform = transform))

  class(means) <- c("estimateMeans", class(means))
  return(means)
}









#' @keywords internal
.emmeans_wrapper <- function(model, levels=NULL, fixed=NULL, modulate=NULL, transform = "response", length=10, type="mean", ...){
  if (is.null(levels)) {
    levels <- insight::find_predictors(model)$conditional
    numeric <- levels[sapply(insight::get_data(model)[levels], is.numeric)]
    levels <- levels[!levels %in% numeric]
  } else {
    numeric <- NULL
  }

  if (!is.null(fixed)) {
    fixed <- unique(c(fixed, numeric))
    levels <- levels[!levels %in% fixed]
  }

  if (length(levels) == 0) {
    stop("No suitable factor levels detected.")
  }


  # Posteriors
  if (is.null(modulate)) {
    means <- emmeans::emmeans(model, levels, by = fixed, transform = transform, ...)
  } else {
    at <- insight::get_data(model)[c(levels, modulate)]
    at <- sapply(at, data_grid, length = length, simplify=FALSE)
    means <- emmeans::ref_grid(model, at = at)
    if(type == "mean"){
      means <- emmeans::emmeans(means, c(levels, modulate), transform = transform)
    } else{
      means <- emmeans::emmeans(means, levels, by = modulate, transform = transform, ...)
    }
  }
  return(list("means"=means,
              "levels"=levels,
              "fixed"=fixed,
              "modulate"=modulate))
}