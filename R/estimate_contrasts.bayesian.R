#' Estimate contrasts between factor levels
#'
#' Contrast analysis. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_contrasts.lm]{Frequentist models}}
#'  \item{\link[=estimate_contrasts.stanreg]{Bayesian models}}
#' }
#'
#' @param model A statistical model.
#' @param levels A character vector or formula specifying the names of the predictors over which to estimate means or contrasts.
#' @param fixed A character vector indicating the names of the predictors to be "fixed" (i.e., maintained), so that the estimation is made at these values.
#' @param modulate A character vector indicating the names of a numeric variable along which the means or the contrasts will be estimated. Adjust its length using \code{length}.
#' @param transform Can be \code{"none"} (default for contrasts), \code{"response"} (default for means), \code{"mu"}, \code{"unlink"}, \code{"log"}. \code{"none"}  will leave the values on scale of the linear predictors. \code{"response"} will transform them on scale of the response variable. Thus for a logistic model, \code{"none"} will give estimations expressed in log-odds (probabilities on logit scale) and \code{"response"} in terms of probabilities.
#' @param length Length of the spread numeric variables.
#' @param standardize If \code{TRUE}, add standardized differences (Cohen's d) or coefficients.
#' @param standardize_robust Robust standardization through \code{MAD} (Median Absolute Deviation, a robust estimate of SD) instead of regular \code{SD}.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A dataframe of estimated contrasts.
#' @export
estimate_contrasts <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "none", length = 10, standardize = TRUE, standardize_robust = FALSE, ...) {
  UseMethod("estimate_contrasts")
}







#' Estimate contrasts
#'
#'
#' @param model A Bayesian model.
#' @inheritParams estimate_contrasts
#' @inheritParams parameters::model_parameters.stanreg
#'
#' @examples
#' library(modelbased)
#' \donttest{
#' library(rstanarm)
#'
#' data <- iris
#' data$Petal.Length_factor <- ifelse(data$Petal.Length < 4.2, "A", "B")
#'
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Length_factor, data = data)
#' estimate_contrasts(model)
#' estimate_contrasts(model, fixed = "Petal.Length_factor")
#'
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Width, data = iris)
#' estimate_contrasts(model)
#' estimate_contrasts(model, fixed = "Petal.Width")
#' estimate_contrasts(model, modulate = "Petal.Width", length = 4)
#'
#' model <- stan_glm(Sepal.Width ~ Species + Petal.Width + Petal.Length, data = iris)
#' estimate_contrasts(model, fixed = "Petal.Width", modulate = "Petal.Length", test = "bf")
#' }
#'
#' @return A dataframe of estimated contrasts.
#'
#' @importFrom emmeans contrast
#' @importFrom graphics pairs
#' @importFrom stats mad median sd setNames
#' @importFrom bayestestR describe_posterior
#' @importFrom insight find_response
#' @export
estimate_contrasts.stanreg <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "none", length = 10, standardize = TRUE, standardize_robust = FALSE, centrality = "median", ci = 0.89, ci_method = "hdi", test = c("pd", "rope"), rope_range = "default", rope_ci = 1, ...) {
  estimated <- .emmeans_wrapper(model, levels = levels, fixed = fixed, modulate = modulate, transform = transform, length = length, type = "contrasts", ...)
  posteriors <- emmeans::contrast(estimated$means, method = "pairwise")

  # Summary
  contrasts <- .summarize_posteriors(posteriors,
    ci = ci, ci_method = ci_method,
    centrality = centrality,
    test = test, rope_range = rope_range, rope_ci = rope_ci, bf_prior = model
  )
  # Standardized differences
  if (standardize) {
    contrasts <- cbind(contrasts, .standardize_contrasts(contrasts, model, robust = standardize_robust))
  }



  # Format contrasts
  names <- gsub("contrast ", "", contrasts$Parameter)

  # Separate Contrasts from Others
  if (!is.null(fixed) | !is.null(modulate)) {
    others <- strsplit(as.character(names), ", ")
    others <- data.frame(do.call(rbind, others))
    # names(others) <- unlist(sapply(others, .find_name_level))
    names(others) <- c("Contrast", fixed, modulate)
    # others <- as.data.frame(sapply(others, .remove_name_level), stringsAsFactors = FALSE)
    levelcols <- data.frame("Contrast" = others$Contrast)
    others$Contrast <- NULL
    others <- as.data.frame(sapply(others, as.numeric_ifnumeric), stringsAsFactors = FALSE)
  } else {
    others <- data.frame()
    levelcols <- data.frame("Contrast" = names)
  }


  # Format contrasts names
  levelcols <- strsplit(as.character(levelcols$Contrast), " - ")
  levelcols <- data.frame(do.call(rbind, levelcols))
  names(levelcols) <- c("Level1", "Level2")
  levelcols$Level1 <- gsub(",", " - ", levelcols$Level1)
  levelcols$Level2 <- gsub(",", " - ", levelcols$Level2)

  contrasts$Parameter <- NULL
  if (nrow(others) != nrow(levelcols)) {
    contrasts <- cbind(levelcols, contrasts)
  } else {
    contrasts <- cbind(levelcols, others, contrasts)
  }

  attributes(contrasts) <- c(
    attributes(contrasts),
    list(
      levels = estimated$levels,
      fixed = estimated$fixed,
      modulate = estimated$modulate,
      transform = transform,
      ci = ci,
      ci_method = ci_method,
      rope_range = rope_range,
      rope_ci = rope_ci,
      response = insight::find_response(model)
    )
  )

  class(contrasts) <- unique(c("estimate_contrasts", "see_estimate_contrasts", class(contrasts)))
  contrasts
}
