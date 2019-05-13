#' Estimate contrasts between factor levels
#'
#' Contrast analysis. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=estimate_contrasts.stanreg]{Bayesian models (stanreg and brms)}}
#'  }
#'
#' @param model Object.
#' @param ... Arguments passed to or from other methods.
#'
#' @export
estimate_contrasts <- function(model, ...) {
  UseMethod("estimate_contrasts")
}







#' Estimate contrasts
#'
#'
#' @param model Bayesian model.
#' @param levels A character vector or formula specifying the names of the predictors over which to average or to contrast.
#' @param fixed A character vector indicating the names of the predictors to be "fixed" (i.e., maintained), so that the estimation is made at these values.
#' @param modulate A character vector indicating the names of a numeric variable along which the contrasts will be tested. Adjust its length using \code{length}.
#' @param transform Can be "none", "response", "mu", "unlink", "log". "none" (default for contrasts) will leave the values on scale of the linear predictors. "response" (default for means) will transform them on scale of the response variable. Thus for a logistic model the default predictions are of log-odds (probabilities on logit scale) and type = "response" gives the predicted probabilities.
#' @param ci Credible Interval (CI) level. Default to 0.90 (90\%).
#' @param estimate The \href{https://easystats.github.io/bayestestR/articles/2_IndicesEstimationComparison.html}{point-estimate(s)} to compute. Can be a character or a list with "median", "mean" or "MAP".
#' @param test What \href{https://easystats.github.io/bayestestR/articles/3_IndicesExistenceComparison.html}{indices of effect existence} to compute. Can be a character or a list with "p_direction", "rope" or "p_map".
#' @param rope_range \href{https://easystats.github.io/bayestestR/articles/1_IndicesDescription.html#rope}{ROPE's} lower and higher bounds. Should be a list of two values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the bounds are set to \code{x +- 0.1*SD(response)}.
#' @param rope_full If TRUE, use the proportion of the entire posterior distribution for the equivalence test. Otherwise, use the proportion of HDI as indicated by the \code{ci} argument.
#' @param length Length of the spreaded numeric variables.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' library(dplyr)
#'
#' model <- stan_glm(Sepal.Width ~ Species * fac2,
#'   data = mutate(iris, fac2 = ifelse(Petal.Length < 4.2, "A", "B"))
#' )
#' estimate_contrasts(model)
#' estimate_contrasts(model, fixed = "fac2")
#'
#' model <- stan_glm(Sepal.Width ~ Species * Petal.Width, data = iris)
#' estimate_contrasts(model)
#' estimate_contrasts(model, fixed = "Petal.Width")
#' estimate_contrasts(model, modulate = "Petal.Width", length = 4)
#' }
#' @import emmeans
#' @importFrom graphics pairs
#' @importFrom stats mad median sd setNames
#' @importFrom bayestestR describe_posterior
#' @export
estimate_contrasts.stanreg <- function(model, levels = NULL, fixed = NULL, modulate = NULL, transform = "none", ci = .90, estimate = "median", test = c("pd", "rope"), rope_range = "default", rope_full = TRUE, length = 10, ...) {

  estimated <- .emmeans_wrapper(model, levels = levels, fixed=fixed, modulate = modulate, transform = transform, length=length, type="contrasts", ...)
  posteriors <- emmeans::contrast(estimated$means, method = "pairwise")
  posteriors <- emmeans::as.mcmc.emmGrid(posteriors)
  posteriors <- as.data.frame(as.matrix(posteriors))


  # Summary
  contrasts <- bayestestR::describe_posterior(posteriors, ci = ci, estimate = estimate, test = test, rope_range = rope_range, rope_full = rope_full)


  # Format contrasts
  names <- gsub("contrast ", "", contrasts$Parameter)

  # Separate Contrasts from Others
  if (!is.null(fixed) | !is.null(modulate)) {
    others <- strsplit(as.character(names), ", ")
    others <- data.frame(do.call(rbind, others))
    names(others) <- unlist(sapply(others, .find_name_level))
    others <- as.data.frame(sapply(others, .remove_name_level), stringsAsFactors = FALSE)
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

  attributes(contrasts) <- c(attributes(contrasts),
                       list(levels = estimated$levels,
                            fixed = estimated$fixed,
                            modulate = estimated$modulate,
                            transform = transform,
                            ci = ci,
                            rope_range = rope_range,
                            rope_full = rope_full))

  class(contrasts) <- c("estimateContrasts", class(contrasts))
  return(contrasts)
}
