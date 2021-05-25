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
#' # visualisation_recipe(x)
#'
#' x <- estimate_response(model)
#' # visualisation_recipe(x)
#' @export
visualisation_recipe <- function(x, ...) {
  stop("Not implemented yet.")
  # UseMethod("visualisation_recipe")
}

#' @export
# visualisation_recipe.estimate_expectation <- function(x, ...) {
#   info <- attributes(x)
#
#   # Layer 1 ---------------------------
#   l <- list(l1 = list())
#
#   l$l1$data <- as.data.frame(x)
#   l$l1$y <- "Predicted"
#   l$l1$ylab <- info$response
#
#   target <- info$target
#   if(length(target) == 1) {
#     l$l1$x <- target
#     l$l1$xlab <- target
#     if(is.numeric(data[target])) {
#       l$l1$geom1 <- "geom_ribbon"
#     }
#   } else {
#     stop("Multiple targets not supported yet.")
#   }
# }

# .visualisation_ribbon <- function(x, data, ...) {
#   list(geom = "geom_ribbon",
#        data = data)
# }