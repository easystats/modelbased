#' @rdname visualisation_recipe.estimate_predicted
#'
#' @examples
#' # ==============================================
#' # estimate_slopes
#' # ==============================================
#' if (require("ggplot2")) {
#'   model <- lm(Sepal.Width ~ Species * Petal.Length, data = iris)
#'   x <- estimate_slopes(model, levels = "Species", trend = "Petal.Length")
#' }
#' @export
visualisation_recipe.estimate_slopes <- function(x, ...) {
  x
}