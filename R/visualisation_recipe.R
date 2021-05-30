#' Visualization helper
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
    insight::print_color(paste0("Type: ", l$geom, "\n"), "yellow")
    cat(paste0(names(l)[names(l) != "geom"], collapse = ", "))
    cat("\n\n")
  }
}