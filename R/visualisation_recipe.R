#' Visualization helper
#'
#' @param point,jitter,pointrange,line,ribbon,labs Additional aesthetics and parameters for the geoms (see customization example).
#'
#' @param x Something.
#' @param ... Other arguments passed to other functions.
#'
#' @export
visualisation_recipe <- function(x, ...) {
  UseMethod("visualisation_recipe")
}


#' @export
print.visualisation_recipe <- function(x, ...) {
  for(i in 1:length(x)) {
    l <- x[[paste0("l", i)]]
    insight::print_color(paste0("Layer ", i, "\n--------\n"), "blue")
    insight::print_color(paste0("Geom type: ", l$geom, "\n"), "yellow")

    elements <- names(l)[!sapply(l, is.null)]
    cat(paste0(elements[elements != "geom"], collapse = ", "))
    cat("\n\n")
  }
}


#' @export
plot.visualisation_recipe <- function(x, ...) {
  insight::check_if_installed("see")

  ggplot2::ggplot(data = attributes(x)$data) + see::geoms_from_list(x, ...)
}