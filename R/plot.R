#' @export
plot.estimate_contrasts <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


#' @export
plot.estimate_predicted <- function(x, ...) {
  layers <- visualisation_recipe(x, ...)
  plot(layers)
}


#' @export
plot.estimate_means <- plot.estimate_predicted

#' @export
plot.estimate_random <- plot.estimate_predicted
