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
estimate_means <- function(model, ...){
  UseMethod("estimate_means")
}














#' Estimate marginal means
#'
#'
#' @inheritParams estimate_contrasts.stanreg
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#' model <- stan_glm(Sepal.Width ~ Species * fac2,
#'     data=mutate(iris, fac2 = ifelse(Petal.Length < 4.2, "A", "B")))
#' estimate_means(model)
#' model <- stan_glm(binary ~ Species,
#'     data=mutate(iris, binary = ifelse(Petal.Length < 4.2, 0, 1)), family="binomial")
#' estimate_means(model)
#' }
#' @import dplyr
#' @import emmeans
#' @importFrom graphics pairs
#' @importFrom stats mad median sd setNames
#' @export
estimate_means.stanreg <- function(model, levels=NULL, transform="response", ci = .90, estimate = "median", ...){

  if(is.null(levels)){
    levels <- insight::find_predictors(model)$conditional
  }


  # Posteriors
  posteriors <- model %>%
    emmeans::emmeans(levels, transform=transform, ...) %>%
    emmeans::as.mcmc.emmGrid() %>%
    as.matrix() %>%
    as.data.frame()

  # Transform
  # if(transform=="response" & insight::model_info(model)$is_logit){
  #   posteriors <- parameters::odds_to_probs(posteriors, log=TRUE)
  # }

  # Summary
  means <- parameters::summarise_posteriors(posteriors, ci = ci, estimate = estimate, test = NULL, rope_bounds = NULL, rope_full = NULL)


  # Format means
  levelcols <- strsplit(as.character(means$Parameter), ", ")
  levelcols <- data.frame(do.call(rbind, levelcols))
  names(levelcols) <- unlist(sapply(levelcols, .find_name_level))
  levelcols <- as.data.frame(sapply(levelcols, .remove_name_level), stringsAsFactors = FALSE)

  means$Parameter <- NULL
  means <- cbind(levelcols, means)

  return(means)

}




#' @keywords internal
.remove_name_level <- function(x){
  name <- .find_name_level(x)
  x <- gsub(name, "", x)
  x <- trimws(x)
  return(x)
}







#' @keywords internal
.find_name_level <- function(x){
  splitted <- strsplit(as.character(x), " ")
  splitted <- data.frame(do.call(rbind, splitted), stringsAsFactors = FALSE)
  uniques <- sapply(splitted, unique)

  lengths <-  sapply(uniques, length)
  if(lengths[1] == 1){
    return(as.character(uniques[[1]]))
  } else{
    warning("Couldn't find consistent level name.")
    if(is.null(names(x))){
      return("X")
    } else{
      return(names(x))
    }
  }
}
