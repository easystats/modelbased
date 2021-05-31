#' @rdname visualisation_recipe
#'
#' @examples
#' if(require("see")) {
#' library(modelbased)
#'
#' # Default
#' x <- estimate_means(lm(Sepal.Width ~ Species, data = iris))
#' # layers <- visualisation_recipe(x)
#' # layers
#' # plot(layers)
#' }
#' @export
visualisation_recipe.estimate_means <- function(x,
                                                show_points = TRUE,
                                                ...) {
  info <- attributes(x)
  layers <- list()

}