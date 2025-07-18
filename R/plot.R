#' @export
plot.estimate_contrasts <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}

#' @rdname visualisation_recipe.estimate_predicted
#' @export
plot.estimate_predicted <- function(x, ...) {
  layers <- visualisation_recipe(x, ...)
  graphics::plot(layers)
}

#' @rdname visualisation_recipe.estimate_predicted
#' @export
plot.estimate_means <- plot.estimate_predicted

#' @export
plot.estimate_grouplevel <- plot.estimate_predicted

#' @export
plot.estimate_slopes <- plot.estimate_predicted
