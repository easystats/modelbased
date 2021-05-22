#' Visualization helper (TODO)
#'
#' @param x Something.
#' @param ... Other arguments passed to other functions.
#'
#' @examples
#' library(modelbased)
#'
#' # Linear Models
#' model <- lm(mpg ~ wt, data = mtcars)
#' x <- estimate_relation(model)
visualisation_recipe <- function(x, ...) {
  # Export once it's ready.
  UseMethod("visualisation_recipe")
}


visualisation_recipe.estimate_relation <- function(x, ...) {
  # Return list of aesthetics, ytitle, xtitle, ...
  print("TODO.")
}