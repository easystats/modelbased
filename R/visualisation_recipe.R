#' Prepare objects for visualisation
#'
#' THis function prepares some objects for avisualisation by returning a list of layers with data and geoms that can be easily plotted using \code{ggplot2}.
#'
#' @param x An easystats object.
#' @param show_data Display the "raw" data as a background to the model-based estimation. Can be set to \code{"none"} to remove it. When input is the result of \code{estimate_means}, \code{show_data} can be "points" (the jittered observation points), "boxplot", "violin" a combination of them (see examples). When input is the result of \code{estimate_response} or \code{estimate_expectation}, \code{show_data} can be "points" (the points of the original data corresponding to the x and y axes), "density_2d", "density_2d_filled", "density_2d_polygon" or "density_2d_raster".
#'
#' @param point,jitter,boxplot,violin,pointrange,density_2d,line,ribbon,labs Additional aesthetics and parameters for the geoms (see customization example).
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


# Utilities ---------------------------------------------------------------

.visualisation_recipe_getrawdata <- function(x, ...) {
  rawdata <- insight::get_data(attributes(x)$model)

  # Add response to data if not there
  y <- insight::find_response(attributes(x)$model)
  if(!y %in% names(rawdata)) rawdata[y] <- insight::get_response(attributes(x)$model)
  rawdata
}
