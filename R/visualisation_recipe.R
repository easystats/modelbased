#' Visualization helper
#'
#' @examples
#' library(modelbased)
#'
#' # Linear Models
#' model <- lm(mpg ~ wt, data = mtcars)
#' x <- estimate_relation(model)
#'
visualisation_recipe <- function(x, ...) {
  UseMethod("visualisation_recipe")
}


visualisation_recipe.estimate_relation <- function(x, ...) {
  # Return list of aesthetics, ytitle, xtitle, ...

  print("TODO.")
}