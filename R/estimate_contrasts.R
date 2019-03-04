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
estimate_contrasts <- function(model, ...){
  UseMethod("estimate_contrasts")
}


#' Estimate contrasts
#'
#'
#' @param model Bayesian model.
#' @param levels A character vector or formula specifying the names of the predictors over which contrasts are desired.
#' @param transform Can be "none", "response", "mu", "unlink", "log". "none" (default for contrasts) will leave the values on scale of the linear predictors. "response" (default for means) will transform them on scale of the response variable. Thus for a logistic model the default predictions are of log-odds (probabilities on logit scale) and type = "response" gives the predicted probabilities.
#' @param ci Credible Interval (CI) level. Default to 0.90 (90\%).
#' @param estimate The \href{https://easystats.github.io/bayestestR/articles/2_IndicesEstimationComparison.html}{point-estimate(s)} to compute. Can be a character or a list with "median", "mean" or "MAP".
#' @param test What \href{https://easystats.github.io/bayestestR/articles/3_IndicesExistenceComparison.html}{indices of effect existence} to compute. Can be a character or a list with "p_direction", "rope" or "p_map".
#' @param rope_bounds \href{https://easystats.github.io/bayestestR/articles/1_IndicesDescription.html#rope}{ROPE's} lower and higher bounds. Should be a list of two values (e.g., \code{c(-0.1, 0.1)}) or \code{"default"}. If \code{"default"}, the bounds are set to \code{x +- 0.1*SD(response)}.
#' @param rope_full If TRUE, use the proportion of the entire posterior distribution for the equivalence test. Otherwise, use the proportion of HDI as indicated by the \code{ci} argument.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * fac2,
#'     data=mutate(iris, fac2 = ifelse(Petal.Length < 4.2, "A", "B")))
#' estimate_contrasts(model)
#' }
#' @import dplyr
#' @import emmeans
#' @importFrom graphics pairs
#' @importFrom stats mad median sd setNames
#' @export
estimate_contrasts.stanreg <- function(model, levels=NULL, transform="none", ci = .90, estimate = "median", test = c("pd", "rope"), rope_bounds = "default", rope_full = TRUE, ...){

  if(is.null(levels)){
    levels <- insight::find_predictors(model)$conditional
  }


  # Posteriors
  posteriors <- model %>%
    emmeans::emmeans(levels, transform=transform, ...) %>%
    emmeans::contrast(method = "pairwise") %>%
    emmeans::as.mcmc.emmGrid() %>%
    as.matrix() %>%
    as.data.frame()


  # Summary
  contrasts <- parameters::summarise_posteriors(posteriors, ci = ci, estimate = estimate, test = test, rope_bounds = rope_bounds, rope_full = rope_full)


  # Format contrasts
  contrasts$Parameter <- gsub("contrast ", "", contrasts$Parameter)
  contrasts$Parameter <- gsub(",", ", ", contrasts$Parameter)

  levelcols <- strsplit(as.character(contrasts$Parameter), " - ")
  levelcols <- data.frame(do.call(rbind, levelcols))
  names(levelcols) <- c("Level1", "Level2")

  contrasts$Parameter <- NULL
  contrasts <- cbind(levelcols, contrasts)



  return(contrasts)

}
